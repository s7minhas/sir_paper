####
source('/code/setup.R')
source(paste0(rfuncs, 'perfHelpers.R'))

loadPkg('gbm') # generalized boosted poisson model

# number of cores to use
coresToUse = 1

# turn off warnings, 
# occurs in double loading of 
# objects to parallel workers
old_warn = getOption("warn")
options(warn = -1)  
####

####
# Load data
load(paste0(dpth, 'socRegData.rda'))

# Create binary joint democracy measure
iPol = ifelse(Z[,,'i_polity',]>17,1,0)
jPol = ifelse(Z[,,'j_polity',]>17,1,0)
jointDem = iPol * jPol
Z[,,'i_polity',] = jointDem

# spec covars
wVars = c(
	'int', 'exports', 'ally',
	'verbCoop','minDistLog'
	)
W = W[,,wVars,] ; W_avg = W_avg[,,wVars]
W[,,c('verbCoop'),] = log(W[,,c('verbCoop'),] + 1)
W_avg[,,c('verbCoop')] = log(W_avg[,,c('verbCoop')] + 1)
zVars = c(
	'int' , 'mConf', 'mConf_ji',
	'minDistLog', 'i_polity',
	'ally', 'exports',
	'verbCoop', 'verbCoop_ji'
	)
Z = Z[,,zVars,]
####

####
# Loop through out samp types
outSampType = c('random','time')
for(oSampT in outSampType){

# option 1 randomly exclude pds and predict
if(oSampT=='random'){
	set.seed(6886) ; seeds = sample(1:6889, 10, replace=FALSE)
	pdOut = lapply(seeds, function(x){ set.seed(x)
		sample(1:dim(Y)[3], 5, replace=FALSE) })
	fName = 'alsModPerf.rda' ; set.seed(6886)
}

# option 2 exclude up to last five years of data
if(oSampT=='time'){
	pdOut = lapply(1:4, function(w){ (dim(Y)[3]-w):dim(Y)[3] })
	fName = 'alsModPerfLastPd.rda' ; set.seed(6886)
}
####

####
  
  
# Running models in parallel
cl=makeCluster(coresToUse) ; registerDoParallel(cl)
oPerfSummCV <- foreach(ii=1:length(pdOut),
	.packages=c("speedglm", "reshape2", "gbm")) %dopar% {
  
	####
	# Create in/out sample period
	pdToDrop = pdOut[[ii]]
	yIn = Y[,,-pdToDrop] ; xIn = X[,,-pdToDrop]
	zIn = Z[,,,-pdToDrop] ; wIn = W[,,,-pdToDrop]

	yOut = Y[,,pdToDrop,drop=FALSE] ; xOut = X[,,pdToDrop,drop=FALSE]
	zOut = Z[,,,pdToDrop,drop=FALSE] ; wOut = W[,,,pdToDrop,drop=FALSE]
	####

	####
	# run als approach with varying W
	dynMod = list(coef=poisblr_dyn(yIn, wIn, xIn, zIn, calcSE=FALSE)$summ)
	dynMod$pred = exp(eta_tab_dyn(c(dynMod$coef), wOut, xOut, zOut))

	# Compare with simple dyadic model
	dyadMod=summary(glm(c(yIn)~-1+apply(zIn,3,c), family='poisson'))$'coefficients'
	rownames(dyadMod) = gsub('apply(zIn, 3, c)', '(Z) ', rownames(dyadMod), fixed=TRUE)
	dyadMod = list(coef=dyadMod[,1], pred=exp(t(dyadMod[,1] %*% t(apply(zOut,3,c)))))

	# run generalized linear boosted model
	df = data.frame(y=c(yIn), apply(zIn[,,-1,], 3, c))
	form = formula( paste0( 'y~', paste(dimnames(zIn[,,-1,])[[3]], collapse='+') ) )
  	gbmMod = gbm(formula=form, data=df, distribution='poisson')
  	dfOut = data.frame(y=c(yOut), apply(zOut[,,-1,],3,c))
  	gbmPred = predict(gbmMod, dfOut[,-1], n.trees=100)
	####

	####
	# calc out samp scores
	predMelt = cbind(melt(yOut),
		dynPred=melt(dynMod$pred)[,'value'],
		glmPred=dyadMod$pred,
		gbmPred=exp(gbmPred) )
	oPerfSumm = list(
		dyn=perfScore(predMelt$value, predMelt$dynPred),
		glm=perfScore(predMelt$value, predMelt$glmPred),
		gbm=perfScore(predMelt$value, predMelt$gbmPred) )
	return(oPerfSumm)
	####
}
stopCluster(cl)
options(warn = old_warn)
####

####
# save
save(oPerfSummCV, file=paste0(rpth, fName) )
####
}