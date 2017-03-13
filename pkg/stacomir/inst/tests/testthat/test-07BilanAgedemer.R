context("Bilan_Agedemer")


test_that("test creating an instance of BilanAgedemer with data loaded (logrami required)",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			bilan_adm<-new("BilanAgedemer")
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("logrami",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","logrami.",envir_stacomi)
			bilan_adm<-suppressWarnings(choice_c(bilan_adm,
					dc=c(107,108,101),			
					horodatedebut="2012-01-01",
					horodatefin="2012-12-31",
					limit1hm=675,
					limit2hm=875,
					silent=TRUE
			))
			# warnings No data for par 1786No data for par 1785
			bilan_adm<-connect(bilan_adm,silent=TRUE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("test that loading bad limits fails",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			bilan_adm<-new("BilanAgedemer")
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("logrami",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","logrami.",envir_stacomi)
			expect_error(
					bilan_adm<-choice_c(bilan_adm,
							dc=c(107,108,101),			
							horodatedebut="2012-01-01",
							horodatefin="2012-12-31",
							limit1hm=675,
							limit2hm="fraise",
							silent=FALSE
					))		
		})





