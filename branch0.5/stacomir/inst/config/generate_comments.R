# TODO: Add comment
# 
# Author: cedric.briand
###############################################################################

install.packages("roxygen2")
install.packages("Rd2roxygen") # reverse documentation
require(Rd2roxygen)
formatR::usage(Rd2roxygen)
?Rd2roxygen
setwd("F:/workspace/stacomir/branch0.5/")
Rd2roxygen(pkg="stacomir")
ff <- file("F:/temp/all.txt", open = "wt")
sink(ff)
create_roxygen(parse_file("F:/workspace/stacomir/branch0.5/stacomir/man/Bilan_lot-class.Rd"))
sink()
#########"
# pour les classes S4
##########

files<-list.files("F:/workspace/stacomir/branch0.5/stacomir/man")
files<-files[grep("class",files)]
for (i in 1:length(files)){
parse_and_save(str_c("F:/workspace/stacomir/branch0.5/stacomir/man/",files[i]),
		gsub(".rd","",str_c("F:/temp/",files[i])))
}