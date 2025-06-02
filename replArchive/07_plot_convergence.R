####
source('~/replArchive/setup.R')
source(paste0(rfuncs, 'postHelpers.R'))
####

####
# Load output for #dynMod, staticMod, dyadMod
# add following to workspace:
# "coefData", "dyadMod", "dynMod", "staticMod", "varKey"
load(paste0(rpth, 'alsModResults.rda'))

varKey$cleanTeX = gsub('\n', ' ', varKey$cleanTeX, fixed=TRUE)
####

####
# get data ready for plotting
params = cbind(dynMod$THETA, dynMod$ALPHA, dynMod$BETA)
rownames(params) = 1:nrow(params)
coefChain = reshape2::melt(params)
coefChain$Var2 = char(coefChain$Var2)
coefChain = coefChain[!grepl(' int', coefChain$Var2),]
coefChain$Var2 = gsub('apply(Z, 3, c)', '(Z) ', coefChain$Var2, fixed=TRUE)
coefChain$type = ifelse(
	grepl('(Z)',coefChain$Var2,fixed=TRUE),'Z',
	ifelse(grepl('(alphaW)',coefChain$Var2,fixed=TRUE),'alpha',
		ifelse(grepl('(betaW)',coefChain$Var2,fixed=TRUE),'beta',NA)
		)
	)
coefChain$varLab = varKey$cleanTeX[match(coefChain$Var2, varKey$dirty)]
coefChain$varLab = factor(coefChain$varLab,
	levels=unique(intersect(varKey$cleanTeX,coefChain$varLab)))
####

####
# plot
facet_labeller = function(string){ TeX(string) }
loadPkg('scales')
typeCols = c('#1f78b4', '#33a02c', '#e31a1c')

zCheck=ggplot(coefChain[coefChain$type=='Z',], aes(x=Var1, y=value)) +
	geom_line() + xlab('') + ylab('') +
	facet_wrap(~varLab,scales='free_y', ncol=2,
		labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.x=element_text(size=5,family="Source Sans Pro Light"),
		axis.text.y=element_text(size=5,family="Source Sans Pro Light"),
		strip.text.x = element_text(size = 9, color='white', family="Source Sans Pro Semibold"),
		strip.background = element_rect(fill = typeCols[1], color=typeCols[1])
		)
aCheck=ggplot(coefChain[coefChain$type=='alpha',], aes(x=Var1, y=value)) +
	geom_line() + xlab('') + ylab('') +
	facet_wrap(~varLab,scales='free_y', ncol=2,
		labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.x=element_text(size=5,family="Source Sans Pro Light"),
		axis.text.y=element_text(size=5,family="Source Sans Pro Light"),
		strip.text.x = element_text(size = 9, color='white', family="Source Sans Pro Semibold"),
		strip.background = element_rect(fill = typeCols[2], color=typeCols[2])
		)
bCheck=ggplot(coefChain[coefChain$type=='beta',], aes(x=Var1, y=value)) +
	geom_line() + xlab('') + ylab('') +
	facet_wrap(~varLab,scales='free_y', ncol=2,
		labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.x=element_text(size=5,family="Source Sans Pro Light"),
		axis.text.y=element_text(size=5,family="Source Sans Pro Light"),
		strip.text.x = element_text(size = 9, color='white', family="Source Sans Pro Semibold"),
		strip.background = element_rect(fill = typeCols[3], color=typeCols[3])
		)
loadPkg('gridExtra')
ggsave(
	grid.arrange(zCheck,arrangeGrob(aCheck,bCheck,ncol=1), ncol=2 ),
	file=paste0(gpth, 'figurea1.pdf'),	
	width=8, height=7, device=cairo_pdf
	)
####
