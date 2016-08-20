library(stacomiR)

stacomi(gr_interface=FALSE) ## launches the application in the command line
# here is the structure of the class
showClass("BilanMigrationMult")
data("bMM_Arzal")
# use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed
#bMM_Arzal<-connect(bMM_Arzal)
str(bMM_Arzal@data)
# calculations
bMM_Arzal<-calcule(bMM_Arzal)
#Individual plot for all DC, taxon and stage where data present
plot(bMM_Arzal)
#cumulated migration at the station (all stages and DC grouped)
cumplot(bMM_Arzal)
# combined plot for ggplot2
plot1(bMM_Arzal)
# data will be written in the datadirectory specified in the stacomi/calcmig.csv file
summary(bMM_Arzal)
