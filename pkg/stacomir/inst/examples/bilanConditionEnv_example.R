require(stacomiR)
# launching stacomi without selecting the scheme or interface
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
# the following script will load the Arzal dataset if connected to iav schema
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
bil_CE<-calcule(b_carlot,silent=TRUE)

plot(bil_CE,plot.type="1",silent=TRUE)
