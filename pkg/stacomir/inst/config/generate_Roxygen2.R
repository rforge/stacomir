

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
# devtools::install_version(package = 'roxygen2',version = '5.0.1', repos = c(CRAN = "https://cran.rstudio.com"))
##use either :
require(devtools)
# uncomment lines in stacomi to run the program (calcmig and envir_stacomi necessary)
document("C:/workspace/stacomir/pkg/stacomir")
## or :
##vignette("roxygen2")
setwd("C:/workspace/stacomir/pkg/stacomir")
#data("bMM_Arzal")
#data("bM_Arzal")
envir_stacomi <- new.env(parent = emptyenv())
# Note la documentation doit se faire sur un "build" correct du package
require(stacomiR)
stacomi(FALSE,FALSE,FALSE)
require(roxygen2)

#Pour Cédric
roxygen2::roxygenise("C:/workspace/stacomir/pkg/stacomir");warnings()[1:10]

#Pour Marion 
#roxygen2::roxygenise("C:/Users/logrami/workspace/stacomir/pkg/stacomir");warnings()[1:10]


setwd("C:/workspace/stacomir/pkg/stacomir")
lintr::lint_package() 

# Some things that goodpractice::gp() will flag might be necessary features of your package, but in general it’s a nice list to read.
# Note: you can only run goodpractice::gp() once R CMD check passes.
goodpractice::gp()

devtools::release() # include devtools::spell_check()