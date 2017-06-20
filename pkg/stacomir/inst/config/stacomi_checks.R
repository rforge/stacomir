setwd("C:/workspace/stacomir/pkg/stacomir")

res <- lintr::lint_package() 
cat(unlist(res) , file = "C:/Users/cedric.briand/Documents/projets/stacomi/stacomir/lintr.txt", sep = "\n")
# Some things that goodpractice::gp() will flag might be necessary features of your package, but in general it’s a nice list to read.
# Note: you can only run goodpractice::gp() once R CMD check passes.
goodpractice::gp()


devtools::release() # include devtools::spell_check()

devtools::spell_check()

devtools::use_readme_rmd()
devtools::release() # include devtools::spell_check()
