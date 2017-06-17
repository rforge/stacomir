require(stacomiR)
stacomi(gr_interface=FALSE,
		login_window=FALSE,
		database_expected=FALSE)
# the following will load the data provided the user has access to the database
# with data in the iav example scheme.
\dontrun{
	bmmCE<-new("BilanMigrationMultConditionEnv")
	bmmCE<-choice_c(bmmCE,
			dc=c(5,6,12),
			taxon=c("Anguilla anguilla"),
			stade=c("AGJ","AGG","CIV"),
			stationMesure=c("temp_gabion","coef_maree","phases_lune"),
			datedebut="2008-01-01",
			datefin="2008-12-31",
			silent=FALSE)	
	bmmCE<-charge(bmmCE) # this is necessary to load operations, DF and DC
	bmmCE<-connect(bmmCE)
	
}	

data("bmmCE")
bmmCE<-calcule(bmmCE,silent=TRUE)
# An example of plot with custom colors.
plot(bmmCE,
		color_station=c("temp_gabion"="red","coef_maree"="blue","phases_lune"="pink"),
 		color_dc=c("5"="yellow","6"="orange","12"="purple")
)
