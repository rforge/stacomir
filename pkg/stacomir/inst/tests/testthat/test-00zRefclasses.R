context("RefHorodate")
test_that("Test that the parsing of many kind of dates works",
		{
			require(stacomiR)
			refHorodate<-new("RefHorodate")
			# regular expression to test string "1] nous avons le choix dans la date\n"
			# default string returned by the method
			expect_that(refHorodate<-choice_c(refHorodate,	
							horodate="01/01/2013 00:00:00"),prints_text("^\\[1\\].+date.+"))
			expect_that(refHorodate<-choice_c(refHorodate,	
							horodate="01/01/2013 00:00"),prints_text("^\\[1\\].+date.+"))
			expect_that(refHorodate<-choice_c(refHorodate,	
							horodate="01-01-2013 00:00"),prints_text("^\\[1\\].+date.+"))		
			expect_that(refHorodate<-choice_c(refHorodate,	
							horodate="2013-01-01 00:00"),prints_text("^\\[1\\].+date.+"))	
			expect_that(refHorodate<-choice_c(refHorodate,	
							horodate="01-01-2013"),prints_text("^\\[1\\].+date.+"))				
			expect_error(refHorodate<-choice_c(refHorodate,	
					horodate="2013/01/01 00:00:00"))
			rm("envir_stacomi",envir =.GlobalEnv)
		})



test_that("Test that the parsing of wrong character formats gets an error",
		{
			require(stacomiR)
			refHorodate<-new("RefHorodate")
			options(warn = -1)
			expect_error(refHorodate<-choice_c(refHorodate,	
							horodate="2013 01 01"))	
			options(warn = 2)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

context("RefDF")

test_that("Test that RefDF choice_c method loads character, numeric, but not rubbish",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			refDF<-new("RefDF")
			refDF<-charge(refDF)
			expect_silent(refDF<-choice_c(refDF,	2))		
			expect_silent(refDF<-choice_c(refDF,	"2"))	
			options(warn = -1)
			expect_error(refDF<-choice_c(refDF,	"semoule"))
			options(warn = 2)
					})
