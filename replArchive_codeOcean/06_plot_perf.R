####
source('/code/setup.R')
source(paste0(rfuncs, 'perfHelpers.R'))
####

####
perfRdas = paste0('alsModPerf',c('','LastPd'),'.rda')
lapply(perfRdas, function(perfFile){

# load perf results
load(paste0(rpth, perfFile)) # oPerfSummCV

# adjust labs
if( !grepl('LastPd', perfFile) ){
	names(oPerfSummCV) = paste0('fold',1:length(oPerfSummCV)) }

if( grepl('LastPd', perfFile) ){
	names(oPerfSummCV) = paste0('missT',2:(length(oPerfSummCV)+1)) }

# unpack into df
oDF = reshape2::melt( oPerfSummCV )

# add labels
metricKey = data.frame(dirty=unique(oDF$L3))
metricKey$clean = c( 
	'Logarithmic', 'Brier', 'Spherical',
	'Dawid-Sebastiani', 'RMSE' )
oDF$L3 = metricKey$clean[match(oDF$L3, metricKey$dirty)]
oDF$L3 = factor(oDF$L3, levels=metricKey$clean)

modKey = data.frame(dirty=unique(oDF$L2))
modKey$clean = c(
	'Social Influence\nModel\n',
	'Generalized Boosted\nModel\n',
	'Generalized Linear\nModel\n' )
oDF$L2 = modKey$clean[match(oDF$L2, modKey$dirty)]
oDF$L2 = factor(oDF$L2, levels=modKey$clean)
####

####
# plot temporal cross val results
if( grepl('LastPd', perfFile) ){
tKey = data.frame(dirty=unique(oDF$L1))
tKey$clean = paste0('$Y_{T-',2:(length(oPerfSummCV)+1),':T}$')
oDF$L1 = tKey$clean[match(oDF$L1, tKey$dirty)]
oDF$L1 = factor(oDF$L1, levels=tKey$clean)
perf = ggplot(oDF, aes(x=L1, y=value, color=L2, shape=L2)) +
  geom_point(size=1.5) + 
  facet_wrap(~L3, scales='free_y') + 
  scale_x_discrete(labels=unlist(lapply(levels(oDF$L1), TeX))) +
  xlab('') + ylab('') + 
  scale_color_brewer(palette='Set2') + 
  theme_light(base_family="Source Sans Pro") +
  theme(
    axis.ticks=element_blank(), 
    axis.text.y=element_text(size=5,family="Source Sans Pro Light"),
    legend.position = c(0.842, 0.22),
    legend.title=element_blank(),
    legend.key=element_blank(),
    panel.border=element_blank(),
    axis.text.x=element_text(size=7,family="Source Sans Pro Light", angle=45, hjust=1),
    strip.text.x = element_text(size = 9, color='white',family="Source Sans Pro Semibold"),
    strip.background = element_rect(fill = "#525252", color='#525252')		
  )
ggsave(perf, file=paste0(gpth, 'figure8.pdf'), width=5, height=4, device=cairo_pdf)
}
####

####
# plot random cross val results
# aggregate
if( !grepl('LastPd', perfFile) ){
loadPkg('dplyr')
  oSummDF = oDF %>% na.omit() %>% dplyr::group_by(L2, L3) %>% 
    dplyr::summarise(
      mu=median(value),
      max=max(value), min=min(value),
      qt5=quantile(value,probs=.5),
      qt95=quantile(value,probs=.95),
      qt25=quantile(value,probs=.25),
      qt75=quantile(value,probs=.75)
    )

perf=ggplot(oSummDF, aes(x=L2, color=L2)) +
  geom_point(aes(y=mu)) +
  geom_linerange(aes(ymin=qt25, ymax=qt75), size = .8) + 
  coord_flip() + 
  facet_wrap(~L3,scales='free_x') + 
  xlab('') + ylab('') + 
  scale_color_brewer(palette='Set2') + 
  theme_light(base_family="Source Sans Pro") +
  theme(
    axis.ticks=element_blank(), 
    axis.text.y=element_blank(),
    legend.position = c(0.842, 0.22),
    legend.title=element_blank(),
    legend.key=element_blank(),
    panel.border=element_blank(),
    axis.text.x=element_text(size=5,family="Source Sans Pro Light"),
    strip.text.x = element_text(size = 9, color='white',family="Source Sans Pro Semibold"),
    strip.background = element_rect(fill = "#525252", color='#525252')		
  )
ggsave(perf, file=paste0(gpth, 'figure7.pdf'), width=5, height=4, device=cairo_pdf)
}
####
})