####
source('/code/setup.R')
source(paste0(rfuncs, 'postHelpers.R'))
####

####
# Load output
load(paste0(dpth, 'socRegData.rda'))
rownames(Y) = colnames(Y) = countrycode(rownames(Y), 'country.name', 'iso3c')
load(paste0(rpth, 'alsModResults.rda'))
rownames(staticMod$A)=colnames(staticMod$A)=rownames(Y)
rownames(staticMod$B)=colnames(staticMod$B)=rownames(Y)
rownames(dynMod$A)=colnames(dynMod$A)=rownames(Y)
rownames(dynMod$B)=colnames(dynMod$B)=rownames(Y)
timeLabs = dimnames(Y)[[3]]
####

####
# Influence plots +

# Gen colors
fname=paste0(gpth, 'matlConf_map.pdf')
genCntryMap = FALSE ; source(paste0(rfuncs, "genColors.r"))
tcols = rep('white', length(cntries)) ; names(tcols) = cntries
tcols[ which( cntries %in% c(
	'RUS', 'NOR', 'FIN', 'SWE', 'CAN', 'DNK',
	'GBR', 'IRL', 'POL', 'BLR'
	) )] = 'black'

# plot infl
pds = round(quantile(1:dim(Y)[3], probs=seq(0,1,.1)))
fNamesInflA = paste0(gpth, 'aInfl_', gsub('-','_',timeLabs[pds]), '.pdf')
fNamesInflB = paste0(gpth, 'bInfl_', gsub('-','_',timeLabs[pds]), '.pdf')

rcols = list(
	c(253,219,199),
	c(244,165,130),
	c(178,24,43) )
bcols = list(
	c(246,232,195),
	c(223,194,125),
	c(191,129,45) )
rcolshex = lapply(rcols, function(x){
	hc=rgb(x[1],x[2],x[3], maxColorValue=255)
	hc=rep(hc, length(ccols))
	names(hc) = names(ccols)
	return(hc)
})
bcolshex = lapply(bcols, function(x){
	hc = rgb(x[1],x[2],x[3], maxColorValue=255)
	hc=rep(hc, length(ccols))
	names(hc) = names(ccols)
	return(hc)
})

# loop through pds
lapply(1:length(pds), function(i){

	# A
	pdf(file=fNamesInflA[i], width=13, height=8)
	abPlot(Infl=dynMod$A[,,pds[i]],
		pThresh=.05, plotType='Sender',
		edgeArrowSize=.5, dir=gpth, save=FALSE)
	dev.off() ; system(paste('pdfcrop', fNamesInflA[i], fNamesInflA[i], sep=' '))

	# B
	pdf(file=fNamesInflB[i], width=13, height=8)
	abPlot(Infl=dynMod$B[,,pds[i]],
		pThresh=.05, plotType='Receiver',
		edgeArrowSize=.5, dir=gpth, save=FALSE)
	dev.off() ; system(paste('pdfcrop', fNamesInflB[i], fNamesInflB[i], sep=' '))
})
# in the paper we show
# figure 6 (top): aInfl_2007_06_01.pdf
# figure 6 (bottom): bInfl_2007_06_01.pdf
# appendix shows
####
