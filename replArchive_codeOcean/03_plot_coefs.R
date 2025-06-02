####
source('/code/setup.R')
source(paste0(rfuncs, 'postHelpers.R'))
####

####
# Load output for #dynMod, staticMod, dyadMod
# add following to workspace:
# "coefData", "dyadMod", "dynMod", "staticMod", "varKey"
load(paste0(rpth, 'alsModResults.rda'))
####

####
# Build coefplot
z=buildCoef(
	coefData$socInflDyn[coefData$socInflDyn$type=='Z',],
	est='coef', se='ses', var='varClean') +
	theme(
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light") )
a=buildCoef(
	coefData$socInflDyn[coefData$socInflDyn$type=='alphaW',],
	est='coef', se='ses', var='varClean') +
	annotate('text', hjust=0, x=4, y=-2.3, label='Sender\nInfluence\nEstimates',
		family='Source Sans Pro Semibold', size=3) +
	theme(
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light") )
b=buildCoef(
	coefData$socInflDyn[coefData$socInflDyn$type=='betaW',],
	est='coef', se='ses', var='varClean') +
	annotate('text', hjust=0, x=4, y=-0.01, label='Receiver\nInfluence\nEstimates',
		family='Source Sans Pro Semibold', size=3) +
	theme(
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light") )

loadPkg('gridExtra')
ggsave(
	grid.arrange(z,arrangeGrob(a,b,ncol=1), ncol=2 ),
	width=6, height=5,
	file=paste0(gpth, 'figure5.pdf'), device=cairo_pdf)	
####