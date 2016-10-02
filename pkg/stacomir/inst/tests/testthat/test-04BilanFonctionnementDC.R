context("BilanFonctionnementDC")

test_that("Test an instance of BilanFonctionnementDC",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			bfDC<-new("BilanFonctionnementDC")
			bfDC<-choice_c(bfDC,
					5,
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31",
					silent=TRUE)
			expect_gt(nrow(bfDC@dc@data),0,
					label="There should be data loaded by the choice_c method in the data slot of 
the RefDC slot,nrow(bfDC@dc@data)")				
			expect_s4_class(bfDC,
					"BilanFonctionnementDC")
			expect_error(BfDC<-choice_c(bfDC,
					5,
					horodatedebut="2013 01 011",
					horodatefin="2013-12-31",
					silent=TRUE))			
			
		})

test_that("BilanFonctionnementDC charge method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			bfDC<-new("BilanFonctionnementDC")
						bfDC<-choice_c(bfDC,
					5,
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31",
					silent=TRUE)
			bfDC<-charge(bfDC,silent=TRUE)
			bfDC<-connect(bfDC,silent=TRUE)
			expect_equal(nrow(bfDC@data),7)			
		})


test_that("BilanFonctionnementDC plot method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			data(bfDC)
			bfDC<-bfDC
			plot(bfDC,plot.type="1",silent=TRUE)
			plot(bfDC,plot.type="2",silent=TRUE,title="An example title")
			plot(bfDC,plot.type="3",silent=TRUE,title="An example title")	
			plot(bfDC,plot.type="4",silent=TRUE,title="An example title")	
		})


test_that("BilanFonctionnementDC summary method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			data(bfDC)
			bfDC<-bfDC
			expect_output(summary(bfDC,silent=FALSE),"summary")
		})


test_that("BilanFonctionnementDC print method works",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			data(bfDC)
			bfDC<-bfDC
			expect_output(print(bfDC))
		})