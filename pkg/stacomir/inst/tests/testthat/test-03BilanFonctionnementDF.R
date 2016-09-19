context("BilanFonctionnementDF")

test_that("Test an instance of BilanFonctionnementDF",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			bfDF<-new("BilanFonctionnementDF")
			bfDF<-choice_c(bfDF,
					2,
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31")
			expect_gt(nrow(bfDF@df@data),0,
					label="There should be data loaded by the choice_c method in the data slot of 
the RefDF slot,nrow(bfDF@df@data)")				
			expect_s4_class(bfDF,
					"BilanFonctionnementDF")
			expect_failure(BfDF<-choice_c(bfDF,
					2,
					horodatedebut="2013 01 011",
					horodatefin="2013-12-31"))			
			
		})

test_that("BilanFonctionnementDF charge method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			bfDF<-new("BilanFonctionnementDF")
						bfDF<-choice_c(bfDF,
					2,
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31",
					silent=TRUE)
			bfDF<-charge(bfDF,silent=TRUE)
			expect_equals(nrow(bfDF@data),5)			
		})


test_that("BilanFonctionnementDF plot method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			data(bfDF)
			bfDF<-bfDF
			plot(bfDF,plot.type="1",silent=TRUE)
			plot(bfDF,plot.type="2",silent=TRUE,title="An example title")
			plot(bfDF,plot.type="3",silent=TRUE,title="An example title")	
			plot(bfDF,plot.type="4",silent=TRUE,title="An example title")	
		})