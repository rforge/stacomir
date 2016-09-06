library(stacomiR)

stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
## launches the application in the command line
## here an example of loading
## not run as the program is possibly not installed
## this example generates the bM_Arzal dataset
\dontrun{
	stacomi(gr_interface=FALSE,
			login_window=FALSE,
			database_expected=FALSE)	
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
#str(bM_Arzal@data)
# calculations
bM_Arzal<-calcule(bM_Arzal)
#Individual plot for all DC (standard), taxon and stage where data present
#silent argument to stop all messages
# this will also write daily and monthly migration balance to the database,
# but only if stacomi has been run with argument : database_expected=FALSE
plot(bM_Arzal,plot.type="standard",silent=TRUE)

#cumulated migration at the station (all stages and DC grouped)
plot(bM_Arzal,plot.type="step")

# data will be written in the data directory specified in the stacomi/calcmig.csv file
summary(bM_Arzal)
