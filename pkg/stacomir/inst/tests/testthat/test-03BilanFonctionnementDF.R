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
			
			BfDF<-new("BilanFonctionnementDF")
			options(warn = 2)
			BfDF<-choice_c(BfDF,
					2,
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31")
			options(warn = 0)
			# there should be data in 
			expect_gt(nrow(BfDF@df@data),0,
					label="There should be data loaded by the choice_c method in the data slot of 
the RefDF slot,nrow(BfDF@df@data)")				
			expect_s4_class(BfDF,
					"BilanMigration")
			rm("envir_stacomi",envir =.GlobalEnv)
		})

