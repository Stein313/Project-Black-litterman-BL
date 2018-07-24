#update.packages(ask = FALSE)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_151')
##############
#Load Library#
##############

library(quantmod)
library(lpSolveAPI)
library(lpSolve)
library(xlsx)
library(xlsxjars)
library(dplyr)
library(rJava)
library(WindR)
library(tseries)
library(TTR)
library(quadprog)

library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)

library(MASS)


#===========================================#
#                                           #
#              Data Input                   #
#                                           #
#===========================================#

library(WindR)
w.start(showmenu=FALSE)
w.menu()
#流动性[货币基金]
#c("SBond","Monetary","Stock","Bond","Saving","Other")
w_wsd_data<-w.wsd("885007.WI,H11025.CSI,000985.CSI,0371.CS,H11006.CSI,801180.SI","close","2012-01-01","2017-11-10","PriceAdj=F")
fulldata<-w_wsd_data$Data


w_wsd_data2<-w.edb('M1005146,M1000536','2013-01-01','2017-11-10','Fill=Previous')#一年期同业存单/AAA企业5年期债券
Amend<-w_wsd_data2$Data

#以AAA企业5年期债券加点200BP测算非标收益率，求区间内median。
#230 trading days

target<-median(Amend[,3])+2.00
print(target)
daytar<-(1+target/100)^(1/230)

Amend[,2]<-100/(1+Amend[,2]*0.01)
Other<-rep(1,nrow(Amend));
for (i in 2:nrow(Amend)){
  Other[i]<-Other[i-1]*daytar
}
set.seed(10000)
Other<-Other+(rnorm(nrow(Amend),mean=0,sd=0.001))

Amend[,3]<-Other
#Amend[,3]<-120/(1+Amend[,3]*0.01)^5
fulldata<-merge(fulldata,Amend,by="DATETIME")
Saving<-Amend[,2]
Other<-Amend[,3]


fulldata<-fulldata[,-c(6,7)]
names(fulldata)<-c("DATETIME","SBond","Monetary","Stock","Bond","Saving","Other")

fulldata<-read.zoo(fulldata)

#=============================USE THE ANSWER FROM Markowitz-mainland.R=====================================================

# Use reverse optimization to compute the vector of equilibrium returns

prices.data<-fulldata
returns.data <- merge.zoo(CalculateReturns(prices.data[,1]),CalculateReturns(prices.data[,2]),
                          CalculateReturns(prices.data[,3]),CalculateReturns(prices.data[,4]),
                          CalculateReturns(prices.data[,5]),CalculateReturns(prices.data[,6]))
returns.data <- na.omit(returns.data)
#单一资产衡量日均收益和波动

# Set names
colnames(returns.data) <- c("SBond","Monetary","Stock","Bond","Saving","Other")

# Save mean return vector and sample covariance matrix################################################
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)

#-=============================B-L Model====================================
#Key Assumptions:
library(knitr)
#Asset returns are normally distributed
#Variance of the prior and the conditional distributions about the true mean are known
#Assume tscalar = 0.25


C<-covMat


wm<-c(20,25,10,10,25,10);names(wm)<-rownames(C)
#各1/6的配置Initial这个到底是啥！！！


#Assumptions
P <- matrix( c(0,0,0,1,0,0,0,0,
               1,0,-1,0,0,0,0,0,
               0,0),3,6) #一定要有正有负

rownames(P)<-c('Q1','Q2','Q3')
colnames(P)<-c("SBond","Monetary","Stock","Bond","Saving","Other")
P
Q <- c(1.01^(1/240)-1,1.03^(1/240)-1,1.1^(1/240)-1)#主观预期========P里边对应资产的收益率,0.05或5没有影响

CF<-0.5*(P%*%C%*%t(P)) #信心校准因子
LF<-diag(c(.2,.8,.5))#主观预期的置信度

omg_diag<-diag(CF)/diag(LF) 
Omega<-diag(omg_diag)#正定矩阵，置信度，相当于pior 先验概率。

#Omega <- matrix(c(0.000801,0,0,
#                 0,0.009546,0,
#                 0,0,0.00084),3,3)


#tau <- 0.025#这部分是设定的，也可以用【信心调整指数】来算###########################
var1 <- ginv(tau * C)#计算用第一矩阵 ，Generalized Inverse of a Matrix，

lambda<-3#市场风险回避系数(这里假设为3)

tau<-sum(diag(CF))/mean(diag(Omega))


#also could use diag()


var2 <- t(P) %*% ginv(Omega) %*% P#计算用第二矩阵
var12 <- ginv(var1+var2)
#var13中放入主观预期和隐含超额预期。

#隐含超额预期收益率%为 meanp=============[       ???       ]==========================================================

meanp<-lambda*C%*%wm 

var3 <- (t(P) %*% ginv(Omega) %*% Q) + (var1 %*% meanp) 
mhat <- var12 %*% var3 #预期收益率ERER,要确保非零！！！！！！！！！！！！！！！

ER<-mhat
colnames(ER)<-'预期收益率ER'
#资产的期望收益ER等于市场均衡收益（meanp）和投资者主观期望收益(Q)的加权平均。
#其中meanp可以通过历史数据获得，Q则源自各种基础分析或来自媒体信息得到的人为主观判断。
#当投资者对自己的主观判断信心(LF或Ω−1Ω−1)很大，则主观的期望收益就会被赋予较大的权重。
#这是一种典型的Bayes分析方法。

#再带回meanReturns计算就可以了。


row.names(ER)<-c("SBond","Monetary","Stock","Bond","Saving","Other")

ER



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



meanReturns <- as.numeric(ER[,1])
covMat <- var12

# Start with the names of the assets
port <- portfolio.spec(assets = c("SBond","Monetary","Stock","Bond","Saving","Other"))

##########Now for some constraints. Let’s use the following:#####
#
#Box constraints
#Leverage (weight sum)
#
#################################################################


# Box 单一资产的比例限制，可以控制单一资产的名称。可多资产
# pspec <- add.constraint(portfolio=pspec, type="box", min=c(0.05, 0, 0.08, 0.1), 
# max=c(0.4, 0.3, 0.7, 0.55))

# max_pos限制为最多可进入的资产，栗子：
# pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

# Add turnover constraint
# pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)

# Add target mean return constraint
# pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
# c("SBond","Monetary","Stock","Bond","Saving","Other")
#port <- portfolio.spec(assets = c("SBond","Monetary","Stock","Bond","Saving","Other"))

port <- add.constraint(port, type = "box", min = c(0,0,0,0,0,0), max = c(1,1,1,1,1,1)) ###4.76%~25%


# Leverage 仓位限制，full要求一直满仓。
port <- add.constraint(portfolio = port, type = "full_investment")

#Let’s use the built-in random solver. 
#This essentially creates a set of feasible portfolios that satisfy all the constraints we have specified. 
#For a full list of supported constraints WEB-PORTE ANA

# Generate random portfolios #此处为产生随机变量。
rportfolios <- random_portfolios(port, permutations = 50000, rp_method = "sample")#check num 500000



#Now let’s add some objectives and optimize. For simplicity’s sake let’s do some mean-variance optimization.
#-----------------------------------------------------------------------------------------------------------

# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")
# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")
# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)


# Generate vector of returns
minret <- 0.001/100 #并不是设定最低目标，而只是排除了一些不会出现在Frontier上的点,*****但是调整不好容易NA, 近3%
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)#######mind this 100
#vec is the FINAL RETURN.

#------------------------------------------------------------------------------------------------------------
#Now that we have the minimum variance as well as the maximum return portfolios, 
#we can build out the efficient frontier.##########################################全有效
#Let’s add a weight concentration objective as well to ensure we don’t get highly concentrated portfolios.

#NOTE: random_portfolios() ignores any diversification constraints. Hence, we didn’t add it previously.
#Using the random solver for each portfolio in the loop below would be very compute intensive. 
#We’ll use the ROI (R Optmization Infrastructure) solver instead.

eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

#下面的代码注意Port和eff.port的选取
for(i in 1:length(vec))
{
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])#找每一个收益的最佳选择
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  #eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
  #                         conc_aversion = 0.001, risk_aversion=0.25)#风险厌恶
  
  
  eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")####只会保留最后一个值-Return最高的
  
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  
  eff.frontier$Sharperatio[i] <- eff.port$Return[i] / eff.port$Risk[i]
  
  frontier.weights[i,] = eff.port$weights#这个很重要，这个是整个组合配置的最终解 
  
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}

eff.frontier#9
#============================================================================================#
#
#########Now lets plot !======================================================================
#
#============================================================================================#

#标准差Numeric POINT
feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

#收益率Numeric point
feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

#Coefficients of Variance **sharoe ratio
feasible.sr <- feasible.means / feasible.sd #夏普率(无逾期收益中位数)

p <-  plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
              mode = "markers", type = "scattergl", showlegend = FALSE,
              
              marker = list(size = 3.5, opacity = 0.5, 
                            colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  
  
  layout(title = "Random Portfolios with Plotly",
         yaxis = list(title = "Mean Returns", tickformat = ".2%"),
         xaxis = list(title = "Standard Deviation", tickformat = ".2%"),
         plot_bgcolor = "#434343",
         paper_bgcolor = "#F8F8F8")
#annotations = list(
# list(x = 0.4, y = 0.75, 
#       ax = -30, ay = -30, 
#       text = "Efficient frontier", 
#       font = list(color = "#F6E7C1", size = 15),
#       arrowcolor = "white")
#)

p

add_trace(p,data = eff.frontier, x =eff.frontier$Risk, y = eff.frontier$Return, mode = "markers", 
          type = "scattergl", showlegend = FALSE,inherit = FALSE, 
          marker = list(color = "#EE7942", size = 3)) 


#Also, you’ll notice that since the portfolios on the frontier(beige dots) have an added weight concentration objective, thefrontier seems sub optimal. Below is a comparison.


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Let’s also plot the weights to check how diversified our optimal portfolios are. We’ll use a barchart for this.

frontier.weights.melt <- reshape2::melt(frontier.weights)
data1<-frontier.weights[,1]
data2<-frontier.weights[,2]
data3<-frontier.weights[,3]
data4<-frontier.weights[,4]
data5<-frontier.weights[,5]
data6<-frontier.weights[,6]
Index<-1:nrow(frontier.weights)

datavisual<-data.frame(Index,data1,data2,data3,data4,data5,data6)
names(datavisual)<-c("Index","SBond","Monetary","Stock","Bond","Saving","Other")

q <- plot_ly(datavisual, x = ~Index,y = ~SBond,name="SBond",type = "bar") %>%
  add_trace(y = ~Monetary, name = "Monetary",marker = list(color = "green")) %>%
  add_trace(y = ~Stock, name = "Stock") %>%
  add_trace(y = ~Bond, name = "Bond") %>%
  add_trace(y = ~Saving, name = "Saving") %>%
  add_trace(y = ~Other, name = "Other") %>%
  
  layout(title = "Portfolio weights across frontier", barmode = "stack",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weights(%)", tickformat = ".0%"))
q














































































































#===============================备查代码。计算BL有效边际=====================================================
#Define the QP
Dmat1 <- 2*C
dvec1 <- rep(0,6)
Amat1 <- matrix(c(mhat,-mhat,rep(1,6),rep(-1,6),diag(length(mhat))),6,10)

# compute efficient frontier for 6 assets - BL

BvarP=vector()#means
BsigmaP=vector()#sd
Bw1=vector()
Bw2=vector()
Bw3=vector()
Bw4=vector()
Bw5=vector()
Bw6=vector()

#Expected Returns 100 values   mind this 100
BRvecs=seq(min(mhat)+0.1^10,max(mhat)-0.1^10,length.out=100);


for (i in 1:length(BRvecs)) {
  BR=BRvecs[i]
  bvec1 <- c(BR,-BR,1,-1,0,0,0,0,0,0)
  BqpSol=solve.QP(Dmat1,dvec1,Amat1,bvec1)
  BvarP[i]=BqpSol$value
  BsigmaP[i]=sqrt(BvarP[i])
  Bw1[i]=BqpSol$solution[1];
  Bw2[i]=BqpSol$solution[2];
  Bw3[i]=BqpSol$solution[3];
  Bw4[i]=BqpSol$solution[4];
  Bw5[i]=BqpSol$solution[5];
  Bw6[i]=BqpSol$solution[6];
}

#Portfolio weights
Bweightsoutput<-data.frame(Bw1,Bw2,Bw3,Bw4,Bw5,Bw6)
names(Bweightsoutput)<-c("SBond","Monetary","Stock","Bond","Saving","Other")
Bweightsoutput

#Checking whether summation is equal to one
weightsum<-apply(Bweightsoutput,1,sum)
weightsum#check 1


p#Markowitz-answered

#B-L model
mmm=add_trace(p, x =BsigmaP, y = BvarP, mode = "markers", 
          type = "scattergl", showlegend = FALSE,inherit = FALSE, 
          marker = list(color = "#FFFF00", size = 3)) 

add_trace(mmm,data = eff.frontier, x =eff.frontier$Risk, y = eff.frontier$Return, mode = "markers", 
          type = "scattergl", showlegend = FALSE,inherit = FALSE, 
          marker = list(color = "#EE7942", size = 3)) 

