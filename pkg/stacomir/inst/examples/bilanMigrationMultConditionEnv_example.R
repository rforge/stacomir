require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
\dontrun{
	bmmCE<-new("BilanMigrationMultConditionEnv")
	bmmCE<-choice_c(bmmCE,
			dc=c(5,6,12),
			taxon=c("Anguilla anguilla"),
			stade=c("AGJ","AGG","CIV"),
			stationMesure=c("temp_gabion","coef_maree"),
			datedebut="2008-01-01",
			datefin="2008-12-31",
			silent=FALSE)	
	bmmCE<-charge(bmmCE)
	bmmCE<-connect(bmmCE)
	bmmCE<-calcule(bmmCE,silent=TRUE)
	
}	

data("bmmCE")
plot(bmmCE,silent=TRUE)
