

install.packages("roxygen2")
install.packages("Rd2roxygen") # reverse documentation
#####################"
## reversing documentation
#######################"
#require(Rd2roxygen)
#setwd("F:/workspace/stacomir/branch0.5/")
#Rd2roxygen(pkg="stacomir")
##########"
## pour les classes S4
###########
#files<-list.files("F:/workspace/stacomir/branch0.5/stacomir/man")
#files<-files[grep("class",files)]
#for (i in 1:length(files)){
#parse_and_save(stringr::str_c("F:/workspace/stacomir/branch0.5/stacomir/man/",files[i]),
#		gsub(".rd","",stringr::str_c("F:/temp/",files[i])))
#}
##########################
## Building documentation
#######################
##use either :
#require(devtools)
#document("F:/workspace/stacomir/branch0.5/stacomir")
## or :
##vignette("roxygen2")
setwd("F:/workspace/stacomir/pkg/stacomir")
#data("bMM_Arzal")
#data("bM_Arzal")
#data("msg")
envir_stacomi <- new.env(parent = emptyenv())
#
require(stacomiR)
stacomi(FALSE,FALSE,FALSE)
require(roxygen2)
roxygen2::roxygenise("F:/workspace/stacomir/pkg/stacomir");warnings()[1:10]