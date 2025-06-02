rm(list=ls())

# set up paths
bpth = '/home/rstudio/replArchive/'
dpth = paste0(bpth, 'data/')
rpth = paste0(bpth, 'results/')
gpth = paste0(bpth, 'graphics/')
rfuncs = paste0(bpth, 'funcs/')

# General functions/libraries
## See info on package versions and other session info
### at bottom of script
options(repos = c(
  CRAN = "https://packagemanager.posit.co/cran/2017-10-10" 
))

loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){
	    install.packages(lib) }
		suppressWarnings(
			suppressMessages( library(lib, character.only=TRUE) ) )
	}
}

toLoad=c(
	'countrycode' , 
	'reshape2', 'magrittr', 'dplyr',
	'stringr', 'abind',
	'ggplot2', 'grid', 
	'latex2exp', 'Cairo', 'extrafont',
	'network', 'igraph',
	'speedglm',
	'doParallel', 'foreach'
	)
loadPkg(toLoad)

# Set a theme for gg
theme_set(theme_bw())

# Global params
seed=6886
set.seed(seed)

# File with helpful functions
source(paste0(rfuncs, 'genHelpers.R'))
source(paste0(rfuncs, 'mltrHelpers.R'))

# model functions
source(paste0(rfuncs, 'tfunctions.r'))
source(paste0(rfuncs, "poisblr_functions_dyn.R"))
source(paste0(rfuncs, 'poisblr_functions.R'))
