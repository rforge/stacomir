require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
\dontrun{
	bil_CE<-new("BilanConditionEnv")
	bil_CE<-choice_c(bil_CE,
			stationMesure=c("temp_gabion","coef_maree"),
			datedebut="2008-01-01",
			datefin="2008-12-31",
			silent=FALSE)	
	bil_CE<-connect(bil_CE)
	
}	

data("bil_CE")
plot(bil_CE,silent=TRUE)
