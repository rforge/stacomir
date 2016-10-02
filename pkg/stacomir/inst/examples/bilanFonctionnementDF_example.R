require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
## An example that will work with the database installed only
\dontrun{
	bfDF=new("BilanFonctionnementDF")
	bfDF<-choice_c(bfDF,
			1,
			horodatedebut="2015-01-01",
			horodatefin="2015-12-31",
			silent=TRUE)
	Sys.setenv(TZ='GMT')
	# the times at Arzal are recorded continuously
	# they are converted to date when a time appears while the hour is changing
	# hence the following
	bfDF<-charge(bfDF)
	bfDF<-connect(bfDF)
}

	data("bfDF")
	plot(bfDF,plot.type="4")
	# the following examples work but take a while to compute
	\dontrun{
	plot(bfDF,plot.type="1")
	plot(bfDF,plot.type="2",main="A nice title")
	plot(bfDF,plot.type="3",main="A nice title")	
	}


  
	

