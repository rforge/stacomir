context("BilanMigrationMultConditionEnv")


test_that("test creating an instance of BilanMigrationMultConditionEnv",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			bmmCE<-new("BilanMigrationMultConditionEnv")
			bmmCE<-choice_c(bmmCE,
					dc=c(5,6,12),
					taxon=c("Anguilla anguilla"),
					stade=c("AGJ","AGG","CIV"),
					stationMesure=c("temp_gabion","coef_maree","phases_lune"),
					datedebut="2008-01-01",
					datefin="2008-12-31",
					silent=TRUE)	
			bmmCE<-charge(bmmCE,silent=TRUE)
			bmmCE<-connect(bmmCE,silent=TRUE)
			expect_true(nrow(bmmCE@bilanConditionEnv@data)>0,"Data not loaded in the bilanConditionEnv part of the object" )
			expect_true(nrow(bmmCE@bilanMigrationMult@data)>0,"Data not loaded in the bilanMigrationMult part of the object" )
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("test plot method",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			bmmCE<-new("BilanMigrationMultConditionEnv")
			bmmCE<-choice_c(bmmCE,
					dc=c(5,6,12),
					taxon=c("Anguilla anguilla"),
					stade=c("AGJ","AGG","CIV"),
					stationMesure=c("temp_gabion","coef_maree","phases_lune"),
					datedebut="2008-01-01",
					datefin="2008-12-31",
					silent=TRUE)	
			bmmCE<-charge(bmmCE,silent=TRUE)
			bmmCE<-connect(bmmCE,silent=TRUE)
			bmmCE<-calcule(bmmCE,silent=TRUE)			
			suppressWarnings(plot(bmmCE,silent=TRUE))
			suppressWarnings(plot(bmmCE,
					color_station=c("temp_gabion"="red","coef_maree"="blue","phases_lune"="pink"),
					color_dc=c("5"="yellow","6"="orange","12"="purple"),silent=TRUE))
			rm("envir_stacomi",envir =.GlobalEnv)
		})






