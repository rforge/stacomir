library(stacomiR)

stacomi(gr_interface=FALSE,login_window=FALSE,pre_launch_test=FALSE) 
## launches the application in the command line
## here an example of loading
## not run as the program is possibly not installed
## this example generates the bM_Arzal dataset
\dontrun{
	stacomi(gr_interface=FALSE,
			login_window=FALSE,
			pre_launch_test=FALSE)	
	bM_Arzal=new("BilanMigration")
	bM_Arzal=choice_c(bM_Arzal,
			dc=5,
			taxons=c("Liza ramada"),
			stades=c("IND"),
			datedebut="2015-01-01",
			datefin="2015-12-31")
	bM_Arzal<-connect(bM_Arzal)
}


data("bM_Arzal")
# use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed

#bM_Arzal<-connect(bM_Arzal)
str(bM_Arzal@data)
# calculations
bM_Arzal<-calcule(bM_Arzal)
#Individual plot for all DC, taxon and stage where data present
plot(bM_Arzal,plot.type="standard")

#cumulated migration at the station (all stages and DC grouped)
plot(bM_Arzal,plot.type="step")
#some plots are a mixture of french and english
#changing the reference table names to english should shift all to english.
#same for spanish.
# combined plot for ggplot2
plot(bM_Arzal,plot.type="multiple")
# data will be written in the data directory specified in the stacomi/calcmig.csv file
summary(bM_Arzal)
