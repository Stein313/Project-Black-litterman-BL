# load Systematic Investor Toolbox

source(gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb')))

#--------------------------------------------------------------------------
# Visualize Market Capitalization History
#--------------------------------------------------------------------------

hist.caps = aa.test.hist.capitalization()   
hist.caps.weight = hist.caps/rowSums(hist.caps)

# Plot Transition of Market Cap Weights in time
plot.transition.map(hist.caps.weight, index(hist.caps.weight), xlab='', name='Market Capitalization Weight History')

# Plot History for each Country's Market Cap
layout( matrix(1:9, nrow = 3, byrow=T) )
col = plota.colors(ncol(hist.caps))
for(i in 1:ncol(hist.caps)) {
  plota(hist.caps[,i], type='l', lwd=5, col=col[i], main=colnames(hist.caps)[i])
}