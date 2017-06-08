context("BilanConditionEnv")


test_that("test creating an instance of BilanConditionEnv",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			bil_CE<-new("BilanConditionEnv")
			bil_CE<-choice_c(bil_CE,
					stationMesure=c("temp_gabion","coef_maree"),
					datedebut="2008-01-01",
					datefin="2008-12-31",
					silent=TRUE)	
			bil_CE<-connect(bil_CE,silent=TRUE)
			expect_true(nrow(bil_CE@data)>0,"The is a problem when loading data in the data slot" )
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("test plot method",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			bil_CE<-new("BilanConditionEnv")
			bil_CE<-choice_c(bil_CE,
					stationMesure=c("temp_gabion","coef_maree"),
					datedebut="2008-01-01",
					datefin="2008-12-31",
					silent=TRUE)	
			bil_CE<-connect(bil_CE,silent=TRUE)
			bil_CE<-plot(bil_CE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})


