context("report_silver_eel")


test_that("test creating an instance of report_silver_eel with data loaded (fd80 schema required)",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  report_silver_eel<-new("report_silver_eel")
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("logrami",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","logrami.",envir_stacomi)
	  report_silver_eel<-suppressWarnings(choice_c(report_silver_eel,
			  dc=c(107,108,101),			
			  horodatedebut="2012-01-01",
			  horodatefin="2012-12-31",
			  limit1hm=675,
			  limit2hm=875,
			  silent=TRUE
		  ))
	  # warnings No data for par 1786No data for par 1785
	  report_silver_eel<-connect(report_silver_eel,silent=TRUE)
	  rm("envir_stacomi",envir =.GlobalEnv)
	})






