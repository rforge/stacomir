context("stacomi base connection")

test_that("Test existence of csv file",{
		filecsv<-"C:/Program Files/stacomi/calcmig.csv";
		expect_equivalent(file.access(filecsv,0),0)
	}

)

test_that("Test existence calcmig data within package",{
			data("calcmig")
			calcmig<-calcmig
			expect_equal(length(calcmig),11)

		}

)

test_that("Test that ODBC link exists and has the right length",{
			require(stacomiR)
			result<-chargecsv(database_expected=TRUE);
			expect_equal(length(result),4)
			expect_equal(length(result$baseODBC),3)
			expect_equal(length(result$sqldf),5)
		})


context("Database connection")

test_that("Test that stacomirtools connects",{
	require(stacomiR)
	envir_stacomi <<- new.env(parent = emptyenv())
	msg<-messages()
	mylinks=chargecsv(database_expected=TRUE)
	baseODBC=mylinks[["baseODBC"]]	
	con=new("ConnectionODBC")	
	con@baseODBC=baseODBC
	con<-connect(con)
	expect_is(connect(con),'ConnectionODBC')
	expect_equal(con@etat,NULL)
	odbcCloseAll()
	rm("envir_stacomi",envir =.GlobalEnv)
})

test_that("Test that positive count for nrow(ref.tr_taxon_tax)",{
			require(stacomiR)
			envir_stacomi <<- new.env(parent = emptyenv())
			msg<-messages()
			mylinks=chargecsv(database_expected=TRUE)
			baseODBC=mylinks[["baseODBC"]]	
			requete=new("RequeteODBC")
			requete@baseODBC<-baseODBC
			requete@sql="select count(*) from ref.tr_taxon_tax"
			requete<-stacomirtools::connect(requete)
			expect_true(as.numeric(requete@query)>0)	
			odbcCloseAll()
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("Tests positive count for sch.t_operation_ope",{
			require(stacomiR)
			envir_stacomi <<- new.env(parent = emptyenv())
			msg<-messages()
			mylinks=chargecsv(database_expected=TRUE)
			baseODBC=mylinks[["baseODBC"]]	
			sch<-paste(baseODBC[2],".",sep="")
			requete=new("RequeteODBC")
			requete@baseODBC<-baseODBC	
			requete@sql=paste("select count(*) from ",sch,"t_operation_ope",sep="")
			requete<-stacomirtools::connect(requete)
			expect_true(as.numeric(requete@query)>0)	
			odbcCloseAll()
			rm("envir_stacomi",envir =.GlobalEnv)
		})

context("Loading program")


test_that("Test that working environment is created",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE)
			expect_true(exists("envir_stacomi"))
			dispose(logw)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("Test that gWidget loginwindow is loaded ",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE)
			expect_true(exists("logw"))
			dispose(logw)
			rm("envir_stacomi",envir =.GlobalEnv)
})

test_that("Test that gWidget gr_interface is loaded, without database_expected, nor login window",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=FALSE,database_expected=FALSE)
			expect_true(exists("win"))
			dispose(win)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("gWidget gr_interface is loaded, with pre launch_test, but without login window",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=FALSE,database_expected=TRUE)
			expect_true(exists("win"))
			dispose(win)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

context("Messages")

test_that("Test that messages are loaded from the database",
		{
			envir_stacomi <<- new.env(parent = emptyenv())
			msg<-messages()
			mylinks=chargecsv(database_expected=TRUE)
			baseODBC=mylinks[["baseODBC"]]	
			assign("baseODBC",baseODBC,envir=envir_stacomi)
			object=new("RefMsg")
			object<-charge(object)	
			expect_true(nrow(object@messager)>0)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("Test that  messages without connection are loaded and of the same length",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			assign("lang","English",envir=envir_stacomi)	
			object=new("RefMsg")
			object<-createmessage(object,TRUE)	
			msgbase<-get("msg",envir=envir_stacomi)
			
			
			msg<-get("msg",envir=envir_stacomi)
			# objects should have the same length but different languages
			expect_identical(length(msg),length(msgbase))
			rm("envir_stacomi",envir =.GlobalEnv)
			rm(list=ls(all=TRUE))
		})
