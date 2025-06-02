####
source('/code/setup.R')
load(paste0(dpth,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
####

####
# Load directed dyadic array, object called Y
load(paste0(dpth, 'dirDyad_array.rda'))
####

####
# Plot networks

# gen legend
fname=paste0(gpth, 'map.pdf')
genCntryMap = TRUE ; source(paste0(rfuncs, "genColors.r"))

# Choose pds to plot over
pds = round(quantile(1:dim(Y)[3], probs=seq(0,1,.1)))

# Create filenames
fNames = paste0(gpth, 'zzz_matlConfNet_', gsub('-','_',dimnames(Y)[[3]][pds]), '.pdf')

# Set static node positions
set.seed(6886)

# Subset to relevant pds
ySub = apply(Y[,,pds], c(1,2), sum)
activeNodes = names( which(rowSums(ySub) + colSums(ySub) > 0) )
ySub = ySub[activeNodes, activeNodes]

# Turn into graph
adjSum = graph.adjacency(ySub, mode='directed', weighted=TRUE)

# Choose layout
set.seed(7634) ; xyPos = layout_with_lgl(adjSum)
rownames(xyPos) = activeNodes
# xyPos[c('RUS'),] = c(68.03478, 154.3331)
loadPkg('qgraph')
xyPos = qgraph.layout.fruchtermanreingold(
	edgelist=get.edgelist(adjSum,names=FALSE), vcount=vcount(adjSum),
	area=100*(vcount(adjSum)^2),repulse.rad=(vcount(adjSum)^3.1) )
rownames(xyPos) = activeNodes

# write file names
fNames[c(1, length(pds))] = c(
  paste0(gpth, 'figure4_jan05.pdf'),
  paste0(gpth, 'figure4_dec12.pdf')
)

# Loop over pds
lapply(c(1,length(pds)), function(ii){

	# subset adjacency matrix by t
	adjT = Y[,,pds[ii]]
	activeNodes = names( which(rowSums(adjT) + colSums(adjT) > 0) )
	adjT = adjT[activeNodes,activeNodes]
	ccols = ccols[activeNodes]

	# Adjust layout
	xyPosT = xyPos[activeNodes,]

	# create graph
	g = graph.adjacency(adjT, mode='directed', diag=FALSE, weighted=TRUE)

	# Adjust label size of nodes
	V(g)$label.cex = degree(g)/50
	V(g)$label.cex[V(g)$label.cex>.7] = .7
	V(g)$label.cex[V(g)$label.cex<.7] = .4

	# Adjust size of nodes
	g$vSize = log(degree(g))^1.5
	# g$vSize = degree(g)

	# Adjust which nodes get labeled
	V(g)$label = NA
	labCap=20
	V(g)$label[which(degree(g)>labCap)]=names(which(degree(g)>labCap))

	# Color nodes
	V(g)$color = ccols

	# Color labels
	vLabCol = rep('white', length(V(g)$label))
	vLabCol[which(V(g)$label %in% c('RUS','GBR','DEU'))] = 'black'

	# Adjust weights of edges
	E(g)$weight = log(E(g)$weight + 1)/2
	# E(g)$weight = E(g)$weight
	# E(g)$weight[E(g)$weight<quantile(E(g)$weight,.9)] = .1

	# Color nodes by source
	edgeStart <- ends(g, es=E(g), names=F)[,1]
	edgeCol <- V(g)$color[edgeStart]

	# Color nodes lighter shade
	edgeCol = rep('#f0f0f0', length( E(g)$weight ) )
	edgeCol[
		which(E(g)$weight>quantile(E(g)$weight,.7) &
		E(g)$weight<quantile(E(g)$weight,.98)) ] = '#bdbdbd' # '#d9d9d9'
	edgeCol[ which(E(g)$weight>=quantile(E(g)$weight,.98) ) ] = '#969696' # '#bdbdbd'

	pdf(file=fNames[ii], width=8,height=5)
	plot.igraph(
		x=g,
		layout=xyPosT,
		vertex.frame.color=V(g)$color,
		vertex.label.color=vLabCol,
		vertex.size=g$vSize,
		edge.arrow.size=.3,
		edge.width=E(g)$weight,
		edge.color=edgeCol,
		# edge.curved=TRUE,
		# , rescale=FALSE,asp=FALSE
		asp=FALSE
		)
	dev.off()
	system(paste('pdfcrop', fNames[ii], fNames[ii], sep=' '))
} )
####
