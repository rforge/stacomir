require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
## An example that will work with the database installed only
\dontrun{
	bfDC=new("BilanFonctionnementDC")
	bfDC<-choice_c(bfDC,
			5,
			horodatedebut="2000-01-01",
			horodatefin="2015-12-31",
			silent=TRUE)
	Sys.setenv(TZ='GMT')
	# the times at Arzal are recorded continuously
	# they are converted to date when a time appears while the hour is changing
	# hence the following
	bfDC<-charge(bfDC)
	bfDC<-connect(bfDC)


	data("bfDC")
	plot(bfDC,plot.type="1")
	plot(bfDC,plot.type="2")
	plot(bfDC,plot.type="3",main="trial title")
	plot(bfDC,plot.type="4",main="trial title")
# the following will write in the datawd folder

 summary(bfDC)
	}
##

  
	

