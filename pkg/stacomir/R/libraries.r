# Nom fichier :        libraries.R

#' function to call and load the libraries used in stacomi
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}

libraries=function() {
necessary = c('tcltk2', 'RODBC','ggplot2','gWidgets' ,'R2HTML','gWidgetsRGtk2',
		'lattice','RColorBrewer','Rcmdr','proto','xtable','XML','svMisc','Hmisc')
if(!all(necessary %in% installed.packages()[, 'Package']))
	install.packages(necessary[!necessary %in% installed.packages()[, 'Package']], dep = T)
require('tcltk2')
require('ggplot2')
require('gWidgets')
require('RODBC')
require('Hmisc')
options(guiToolkit = "RGtk2")
require('lattice')
require('RColorBrewer')
require('ggplot2usr')
require('proto') 
require('xtable')
#require('Hmisc')
require('XML') 
require('svMisc')        
}
 # pour sa fonction Calendar
#require('sma')# fonction is.odd




