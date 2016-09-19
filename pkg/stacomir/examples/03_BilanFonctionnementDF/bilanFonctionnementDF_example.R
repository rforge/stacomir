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
	bfDF<-charge(bfDF)
	plot(bfDF,plot.type="1")
	plot(bfDF,plot.type="2",title="A nice title")
	plot(bfDF,plot.type="3",title="A nice title")
}

  
	

