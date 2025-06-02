# run this first to install all libs
# necessary for analysis

######################
# relev cran repo for this version of R
options(repos = c(
  CRAN = "https://packagemanager.posit.co/cran/2017-10-10" 
))

# fn to load pkgs
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){
      install.packages(lib) }
    suppressWarnings(
      suppressMessages( library(lib, character.only=TRUE) ) )
  }
}

# pkgs to load
toLoad=c(
  'countrycode' , 
  'reshape2', 'magrittr', 
  'dplyr', 'scales',
  'stringr', 'abind',
  'ggplot2', 'grid', 'gridExtra',
  'latex2exp', 'Cairo', 'extrafont',
  'network', 'igraph',
  'speedglm', 'gbm',
  'doParallel', 'foreach',
  'qgraph', 'cshapes' )

# load
loadPkg(toLoad)
######################

######################
# Sys and package info
# > sessionInfo()
# R version 3.4.4 (2018-03-15)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Debian GNU/Linux 9 (stretch)
# 
# Matrix products: default
# BLAS: /usr/lib/openblas-base/libblas.so.3
# LAPACK: /usr/lib/libopenblasp-r0.2.19.so
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C              LC_PAPER=en_US.UTF-8       LC_NAME=C                 
# [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C      
#
# > benchmarkme::get_cpu()
# $vendor_id
# [1] "AuthenticAMD"
# 
# $model_name
# [1] "AMD Ryzen 9 7950X 16-Core Processor"
# 
# $no_of_cores
# [1] 32
# 
# > benchmarkme::get_ram()
# 134 GB
# libs
# pkgs = sort(toLoad)
# vinfo = installed.packages()[pkgs, c('Package', 'Version')]
# rownames(vinfo) = NULL
# vinfo = apply(vinfo, 1, paste, collapse=' ')
# vinfo = matrix(vinfo, ncol=3)
# knitr::kable(vinfo, format='markdown')
# |                  |                |               |
#   |:-----------------|:---------------|:--------------|
#   |abind 1.4-5       |foreach 1.4.3   |magrittr 1.5   |
#   |Cairo 1.5-9       |gbm 2.1.3       |network 1.13.0 |
#   |countrycode 0.19  |ggplot2 2.2.1   |qgraph 1.4.4   |
#   |cshapes 0.6       |grid 3.4.4      |reshape2 1.4.2 |
#   |doParallel 1.0.11 |gridExtra 2.3   |scales 0.5.0   |
#   |dplyr 0.7.4       |igraph 1.1.2    |speedglm 0.3-2 |
#   |extrafont 0.17    |latex2exp 0.4.0 |stringr 1.3.0  |
######################