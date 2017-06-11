require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
###########################################################
## An example that will work only if the database is present 
## and the program installed and comprises the schema iav
###########################################################"
\dontrun{
	bfDC=new("BilanFonctionnementDC")
	bfDC<-choice_c(bfDC,
			5,
			horodatedebut="2000-01-01",
			horodatefin="2015-12-31",
			silent=TRUE)
	Sys.setenv(TZ='GMT')
	# This dataset formating is GMT. If this option is not set
	# the dataset is tranformed from timestamp to date
	bfDC<-charge(bfDC)
	bfDC<-connect(bfDC)
	# this dataset has been loaded by the previous lines
###########################################################	
# Without connexion to the database (use dataset bfDC)
##########################################################
	data("bfDC")
	plot(bfDC,plot.type="1")
	plot(bfDC,plot.type="2")
	plot(bfDC,plot.type="3",main="trial title")
	plot(bfDC,plot.type="4",main="trial title")
# the following will write in the datawd folder
 summary(bfDC)
	}
##

  
	

