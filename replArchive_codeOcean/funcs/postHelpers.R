# colors for coefp
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insig" = rgb(150, 150, 150, maxColorValue=255))

buildCoef = function(coefData, est='est', se='se', var='var', plot=TRUE){
	# Add conf ints
	coefData$up95 = coefData[,est] + qnorm(.975)*coefData[,se]
	coefData$lo95 = coefData[,est] - qnorm(.975)*coefData[,se]
	coefData$up90 = coefData[,est] + qnorm(.95)*coefData[,se]
	coefData$lo90 = coefData[,est] - qnorm(.95)*coefData[,se]

	# Add in variable for colors
	coefData$sig = NULL
	coefData$sig[coefData$lo90 > 0 & coefData$lo95 < 0] = "Positive at 90"
	coefData$sig[coefData$lo95 > 0] = "Positive"
	coefData$sig[coefData$up90 < 0 & coefData$up95 > 0] = "Negative at 90"
	coefData$sig[coefData$up95 < 0] = "Negative"
	coefData$sig[coefData$lo90 < 0 & coefData$up90 > 0] = "Insig"

	# Create coefficient plot
	mathLabs = levels(coefData[,var])
	ggCoef = ggplot(coefData, aes_string(var, est, color = 'sig')) +
		geom_linerange(aes(ymin=lo95, ymax=up95), size = 0.5) + 
		geom_linerange(aes(ymin=lo90, ymax=up90), size = 1) + 
		geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
		geom_point(aes_string(var,est), size=4, shape=20) + 
		scale_colour_manual(values = coefp_colors) +
		scale_x_discrete(labels=unlist(lapply(mathLabs, TeX))) +
		coord_flip() + xlab("") + ylab("") +
		theme(
			axis.ticks=element_blank(), panel.border=element_blank(),
			legend.position='none'
			)
	if(plot){ ggCoef } else { coefData }
}

# Some descriptives
cntEdge = function(Infl){
	ABbin = Infl>quantile(c(Infl),1-.05/2)*1
	# ggData = rowSums(ABbin) + colSums(ABbin) %>%  
	# 	data.frame(., stringsAsFactors=FALSE) 
	ggData = rowSums(ABbin) %>%  
		data.frame(., stringsAsFactors=FALSE) 		
	ggData$Country = rownames(ggData) ; names(ggData)[1] = c('Edges')
	ggData$Type = deparse(substitute(Infl))
	return(ggData)
}

edgePlot=function(Infl, n, cntryCols){
	edges = cntEdge(Infl)
	edges$Country = factor(edges$Country, 
		levels=edges$Country[order(edges$Edges, decreasing=TRUE)])
	edges = edges[order(edges$Edges, decreasing=TRUE),]

	ggInfl = ggplot(edges[1:n,], aes(x=Country, xend=Country, y=0, yend=Edges, color=Country)) +
		geom_segment(size=2) + 
		geom_point(aes(x=Country,y=0,size=2.5)) + geom_point(aes(x=Country,y=Edges,size=3)) +
		ylab('') + xlab('') +
		scale_color_manual(values=cntryCols) +
		theme(
			axis.text.x = element_text(angle=45, hjust=1, 
				size=8,family="Source Sans Pro Light"),
			axis.text.y = element_text(family="Source Sans Pro Light"),
			axis.ticks=element_blank(), panel.border=element_blank(),
			legend.position='none'
			)
	return(ggInfl)
}

# influence
abPlot = function( Infl, pThresh=.01, plotType, pWidth=12, pHeight=8, 
	cAbb = NULL,
	edgeArrowSize=.45, cntryCols=ccols, textCols=tcols, dir=NULL, save=FALSE ){
	
	# conf int
	ABbin = Infl>quantile(c(Infl),1-pThresh/2)*1
	pThreshLab = (1-pThresh)*100

	# narrow to country network
	vShapes = 'circle'
	if(!is.null(cAbb)){
		cntryNetActors = c(which(ABbin[cAbb,] != 0), which(ABbin[,cAbb] != 0)) %>% 
			names() %>% 
			unique() %>% 
			append(cAbb,.)
		ABbin = ABbin[cntryNetActors,cntryNetActors]
		vShapes = rep('circle', nrow(ABbin))
		vShapes[match(cAbb, rownames(ABbin))] = 'square'
	}

	# throw out isolates
	activeNodes = names( which(rowSums(ABbin) + colSums(ABbin) > 0) )
	if( length(activeNodes)==0 ){ 
		return(paste0(
			'No countries with interactions at threshold of ', pThreshLab
			))  }
	ABbin = ABbin[activeNodes,activeNodes]
	cntryCols = cntryCols[activeNodes]
	textCols = textCols[activeNodes]

	# Other graph params
	g = graph.adjacency(ABbin, mode='directed', diag=FALSE)
	g$labSize = rescale(degree(g), c(.4, .8))
	g$vSize = rescale(degree(g), c(4, 7))

	# graph layout
	loadPkg('qgraph')
	xyPos = qgraph.layout.fruchtermanreingold(
		edgelist=get.edgelist(g,names=FALSE), vcount=vcount(g)
		,area=1*(vcount(g)^2)
		# ,repulse.rad=(vcount(g)^2.1)
		)
	rownames(xyPos) = activeNodes	

	fName = paste0(dir, 'matlConf_', plotType, 'Influence_pval', pThreshLab, '.pdf')
	if(save){ pdf(file=fName, width=pWidth, height=pHeight) }
	plot.igraph(g,
			layout=xyPos,
			vertex.label.color=textCols, 
			vertex.color=cntryCols, 
			vertex.frame.color=cntryCols, 
			vertex.label.cex=g$labSize,
			vertex.size=g$vSize,
			vertex.shape=vShapes,
			edge.arrow.size=edgeArrowSize,
			asp=FALSE	
		)
	if(save){ dev.off() }
	if(save){ system(paste('pdfcrop', fName, fName, sep=' ')) }
}