update.packages(ask = FALSE)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_151')

#install.packages("ROI","ROI.plugin.quadprog","ROI.plugin.glpk")

#Load Library#
{
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

  library(PerformanceAnalytics)
  
  library(zoo)
  
  library(plotly)
  
  library(MASS)
  
  library(knitr)
  
}
# Data Input #
{
  w.start(showmenu=FALSE)
  
  w_wsd_data<-w.wsd("M1004900,000985.CSI,CBA00301.CS,801180.SI","close","2013-01-01","2017-12-31","PriceAdj=F")
  
  w_wsd_data7<-w.wsd("FR007.IR","close","2013-01-01","2013-12-12","PriceAdj=F")
  
  fulldata<-w_wsd_data$Data
  
  fulldata$M1004900[1:225]<-w_wsd_data7$Data$CLOSE
  
}
# Modify Monetary #
{
  w_wsd_data2<-w.edb('M1000536','2013-01-01','2017-12-31','Fill=Previous')

  Amend<-w_wsd_data2$Data
}
# Modify Others #
{
  target<-median(Amend[,2])+1.10 #110Bps
  daytar<-(1+target/100)^(1/240) #240Days
  Other<-rep(1,nrow(Amend));

  for (i in 2:nrow(Amend))
    {
        Other[i]<-Other[i-1]*daytar
    }
  set.seed(10000) #Pick any # you like

  Other<-Other+(rnorm(nrow(Amend),mean=0,sd=0.000))

  Amend[,2]<-Other
 
  fulldata<-merge(fulldata,Amend,by="DATETIME")

  Other<-Amend[,2]

}

  fulldata<-fulldata[,-c(5)];

  FR007<-rep(1,nrow(fulldata));

  for (i in 2:length(FR007))
    {
       FR007[i]<-FR007[i-1]*((100+fulldata$M1004900[i])/100)^(1/240)
    }

  fulldata[,2]<-FR007
  
  names(fulldata)<-c("DATETIME","Monetary","Stock","Bond","Other")

  fulldata<-read.zoo(fulldata)
  
  #save(fulldata,file="fulldata.Rda")#Optional

# Use reverse optimization to compute the vector of equilibrium returns

  prices.data<-fulldata

  returns.data <- merge.zoo(
    CalculateReturns(prices.data[,1]),CalculateReturns(prices.data[,2]),
    CalculateReturns(prices.data[,3]),CalculateReturns(prices.data[,4])
  )
  
  returns.data <- na.omit(returns.data)

  #Set names
  colnames(returns.data) <- c("Monetary","Stock","Bond","Other")

# Save mean return vector and sample covariance matrix #
  meanReturns <- colMeans(returns.data)
  
  covMat <- cov(returns.data) #*#

#-=============================B-L Model====================================
#Key Assumptions:

#Asset returns are normally distributed
#Variance of the prior and the conditional distributions about the true mean are known

  C<-covMat


  wm<-c(40,5,40,15)# 
# Initial weights.

# Parameters Assumptions
  P <- matrix(c( 1, 1,0 , 0, 0,
                 0, 0,-1, 0, 0,
                -1, 0,0 , 1, 0,
                 0, 0,0 , 0, 1)
                ,nrow=5,ncol=4) # Negative number is necessary

  rownames(P)<-c('Q1','Q2','Q3','Q4','Q5')

  Q<-c(1.01^(1/240)-1,   1.03^(1/240)-1,    1.1^(1/240)-1,  1.02^(1/240)-1, 1.01^(1/240)-1)#主观预期===1货币2债券3股票=====P里边对应资产的收益率,0.05???5有影???,和预测周期相???

  lambda<-2.5# default setting 
  meanp<-lambda*C%*%wm 

  CF<-0.5*(P%*%C%*%t(P)) #confidence parameter
  LF<-diag(c(.8,.8,.6,.8,.8))#confidence parameter

  omg_diag<-diag(CF)/diag(LF) 
  Omega<-diag(omg_diag)#pior distribution

    #tau <- 0.025# you could alse set tau directlly instead.
    tau<-sum(diag(CF))/mean(diag(Omega))

  var1 <- ginv(tau * C)#Generalized Inverse of a Matrix. also could use diag()

  var2 <- t(P) %*% ginv(Omega) %*% P#
  var12 <- ginv(var1+var2)

  var3 <- (t(P) %*% ginv(Omega) %*% Q) + (var1 %*% meanp) 
  mhat <- var12 %*% var3 #needs to be non-zero

  ER<-mhat #implied expected returns.
  colnames(ER)<-'预期收益率ER'
  
  wp<-solve(C)%*%meanp%*%(t(rep(1,nrow(C)))%*%solve(C)%*%meanp)^(-1)
  we<-solve(C)%*%ER[,1]%*%(t(rep(1,nrow(C)))%*%solve(C)%*%ER[,1])^(-1)
  colnames(wp)<-'Market' #implied weight 
  colnames(we)<-'Tangent(ER)'#implied weight 

    compare<-cbind(we,wp)
    compare

  row.names(ER)<-c("Monetary","Stock","Bond","Other")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  meanReturns <- as.numeric(ER[,1])# if B-L model

  #Here we use the experts method instead.
  #meanReturns[1]<-1.0375^(1/240)-1#moneytary
  #meanReturns[2]<-1.060^(1/240)-1#stock
  #meanReturns[3]<-1.041^(1/240)-1#bond
  #meanReturns[4]<-1.055^(1/240)-1#other

  covMat <- var12 # should be var12 if use B-L ;'covMat = C' If you want Markowitz

  # Start with the names of the assets
  port <- portfolio.spec(assets = c("Monetary","Stock","Bond","Other"))

##########Now for some constraints. Let’s use the following:#####
#
#Box constraints
#Leverage (weight sum)
#
#################################################################


# Box 
# pspec <- add.constraint(portfolio=pspec, type="box", min=c(0.05, 0, 0.08, 0.1), 
# max=c(0.4, 0.3, 0.7, 0.55))

# max_pos
# pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

# Add turnover constraint
# pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)

# Add target mean return constraint
# pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
# c("SBond","Monetary","Stock","Bond","Saving","Other")
# port <- portfolio.spec(assets = c("SBond","Monetary","Stock","Bond","Saving","Other"))

  port <- add.constraint(port, type = "box", min = c(0.07,0.05,0.50,0), max = c(1,1,1,0.15)) #just for my case only


# Leverage 
  port <- add.constraint(portfolio = port, type = "full_investment")

  port <- add.constraint(portfolio = port, type = "return", return_target=0.00000,enabled = FALSE)
  #Let’s use the built-in random solver. 
  #This essentially creates a set of feasible portfolios that satisfy all the constraints we have specified. 

# Generate random portfolios #
  rportfolios <- random_portfolios(port, permutations = 50000, rp_method = "sample")#check num 500000



#Now let’s add some objectives and optimize. For simplicity’s sake let’s do some mean-variance optimization.
#-----------------------------------------------------------------------------------------------------------

  # Get minimum variance portfolio
  minvar.port <- add.objective(port, type = "risk", name = "var")
  # Optimize
  minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                   rp = rportfolios)

  # Generate maximum return portfolio
  maxret.port <- add.objective(port, type = "return", name = "mean")#could be "mean"
  # Optimize
  maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                   rp = rportfolios)

  # Generate vector of returns
  minret <- 0.0001/100 #THIS IS NOT THE BOTTOM LINE
  maxret <- maxret.opt$weights %*% meanReturns

  vec <- seq(minret, maxret, length.out = 200)#######mind this 100\#vec is the FINAL RETURN. but just points.

#------------------------------------------------------------------------------------------------------------
#Now that we have the minimum variance as well as the maximum return portfolios, 
#we can build out the efficient frontier.#
#Let’s add a weight concentration objective as well to ensure we don’t get highly concentrated portfolios.

#NOTE: random_portfolios() ignores any diversification constraints. Hence, we didn’t add it previously.
#Using the random solver for each portfolio in the loop below would be very compute intensive. 
#We’ll use the ROI (R Optmization Infrastructure) solver instead.

  eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                             Return = rep(NA, length(vec)), 
                             SharpeRatio = rep(NA, length(vec)))

  frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
  colnames(frontier.weights) <- colnames(returns.data)
  

  for(i in 1:length(vec))
  {
    eff.port <- add.constraint(port, type = "return", name = "mean", return_target = max(vec[i]))#找每一个收益的最佳选择,
    # if you don't want a bottom line for Return rate，return_target = vec[i]
    eff.port <- add.objective(eff.port, type = "risk", name = "var")
    # eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
    #                    conc_aversion = 0.001, risk_aversion=lambda)# If you want risk aversion
  
  
    eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")##
  
    eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  
    eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  
    eff.frontier$Sharperatio[i] <- (eff.frontier$Return[i]-(1.015^(1/240)-1) )/ eff.frontier$Risk[i]#
  
    frontier.weights[i,] = eff.port$weights# This is the final answer actually 
  
    print(paste(round(i/length(vec) * 100, 0), "% done..."))
  }

eff.frontier

# Print solutions #

  eff.frontier$Risk[which.max(eff.frontier$Sharperatio)]*sqrt(240)

  (1+eff.frontier$Return[which.max(eff.frontier$Sharperatio)])^240

  frontier.weights[which.max(eff.frontier$Sharperatio),]*100

  eff.frontier$Sharperatio[which.max(eff.frontier$Sharperatio)]

#============================================================================================#
#
#########Now lets plot !======================================================================
#
#============================================================================================#

#Numeric POINT
  feasible.sd <- apply(rportfolios, 1, function(x){
    return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
  })

#Numeric point
  feasible.means <- apply(rportfolios, 1, function(x){
    return(x %*% meanReturns)
  })

#Coefficients of Variance **sharoe ratio
  feasible.sr <- (feasible.means / feasible.sd) #
  
  p <-  plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
                mode = "markers", type = "scattergl", showlegend = FALSE,
              
                marker = list(size = 3.5, opacity = 0.5, 
                              colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  
  
    layout(title = "",
           yaxis = list(title = "日均收益", tickformat = ".2%"),
           xaxis = list(title = "标准", tickformat = ".2%"),
           plot_bgcolor = "#434343",
           paper_bgcolor = "#F8F8F8")

p


add_trace(p,data = eff.frontier, x =eff.frontier$Risk, y = eff.frontier$Return, mode = "markers", 
          type = "scattergl", showlegend = FALSE,inherit = FALSE, 
          marker = list(color = "#EE7942", size = 5)) 


#Also, you’ll notice that since the portfolios on the frontier(beige dots) have an added weight concentration objective, thefrontier seems sub optimal. Below is a comparison.


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Let’s also plot the weights to check how diversified our optimal portfolios are. We’ll use a barchart for this.
{
  frontier.weights.melt <- reshape2::melt(frontier.weights)
  data1<-frontier.weights[,1]
  data2<-frontier.weights[,2]
  data3<-frontier.weights[,3]
  data4<-frontier.weights[,4]
  
  Index<-1:nrow(frontier.weights)
  
  datavisual<-data.frame(Index,data1,data2,data3,data4)
  names(datavisual)<-c("Index","Monetary","Stock","Bond","Other")
  
  
  q <- plot_ly(datavisual, x = ~Index,y = ~Monetary,name="Monetary",type = "bar",marker = list(color = "#00CD00")) %>%
    add_trace(y = ~Stock, name = "Stock",marker = list(color = "#FFC125")) %>%
    add_trace(y = ~Bond, name = "Bond",marker = list(color = "#1C86EE")) %>%
    add_trace(y = ~Other, name = "Other",marker = list(color = "#EE3B3B")) %>%
    
    layout(title = "Portfolio weights across frontier", barmode = "stack",
           xaxis = list(title = "不同有效前沿选择"),
           yaxis = list(title = "大类资产占比(%)", tickformat = ".0%"))
  q
  
}





