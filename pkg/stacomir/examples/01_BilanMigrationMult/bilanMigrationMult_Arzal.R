library(stacomiR)

stacomi(gr_interface=FALSE,login_window=FALSE,pre_launch_test=FALSE) 
## launches the application in the command line
## here an example of loading
## not run as the program is possibly not installed
## this example generates the bMM_Arzal dataset
\dontrun{
	stacomi(gr_interface=FALSE,
			login_window=FALSE,
			pre_launch_test=FALSE)	
	bMM_Arzal=new("BilanMigrationMult")
	bMM_Arzal=choice_c(bMM_Arzal,
			dc=c(5,6,12),
			taxons=c("Anguilla anguilla"),
			stades=c("AGG","AGJ","CIV"),datedebut="2011-01-01",datefin="2011-12-31")
	bMM_Arzal<-connect(bMM_Arzal)
}


data("bMM_Arzal")
# use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed

#bMM_Arzal<-connect(bMM_Arzal)
str(bMM_Arzal@data)
# calculations
bMM_Arzal<-calcule(bMM_Arzal)
#Individual plot for all DC, taxon and stage where data present
stacomiR::plot(bMM_Arzal,"standard")

#cumulated migration at the station (all stages and DC grouped)
cumplot(bMM_Arzal)
# combined plot for ggplot2
plot1(bMM_Arzal)
# data will be written in the datadirectory specified in the stacomi/calcmig.csv file
summary(bMM_Arzal)
