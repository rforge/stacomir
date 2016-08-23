context("stacomi base connection")

test_that("Test existence of csv file",{
		filecsv<-"C:/Program Files/stacomi/calcmig.csv";
		expect_equivalent(file.access(filecsv,0),0)
	}

)

test_that("Test existence of csv file within package",{
			filecsv<-file.path(.libPaths(),"stacomiR","config","calcmig.csv");
			expect_equivalent(file.access(filecsv,0),0)
		}

)

test_that("Test that ODBC link exists and has the right length",{
			require(stacomiR)
			result<-chargecsv();
			expect_equal(length(result),4)
			expect_equal(length(result$baseODBC),3)
			expect_equal(length(result$sqldf),5)
		})


context("Database connection")

test_that("Test that stacomirtools connects",{
	require(stacomiR)
	envir_stacomi <<- new.env(parent = emptyenv())
	msg<-messages()
	mylinks=chargecsv()
	baseODBC=mylinks[["baseODBC"]]	
	con=new("ConnectionODBC")	
	con@baseODBC=baseODBC
	con<-connect(con)
	expect_is(connect(con),'ConnectionODBC')
	expect_equal(con@etat,NULL)
	odbcCloseAll()
})

test_that("Test that positive count for nrow(ref.tr_taxon_tax)",{
			require(stacomiR)
			envir_stacomi <<- new.env(parent = emptyenv())
			msg<-messages()
			mylinks=chargecsv()
			baseODBC=mylinks[["baseODBC"]]	
			requete=new("RequeteODBC")
			requete@baseODBC<-baseODBC
			requete@sql="select count(*) from ref.tr_taxon_tax"
			requete<-stacomirtools::connect(requete)
			expect_true(as.numeric(requete@query)>0)	
			odbcCloseAll()
		})

test_that("Tests positive count for sch.t_operation_ope",{
			require(stacomiR)
			envir_stacomi <<- new.env(parent = emptyenv())
			msg<-messages()
			mylinks=chargecsv()
			baseODBC=mylinks[["baseODBC"]]	
			sch<-paste(baseODBC[2],".",sep="")
			requete=new("RequeteODBC")
			requete@baseODBC<-baseODBC	
			requete@sql=paste("select count(*) from ",sch,"t_operation_ope",sep="")
			requete<-stacomirtools::connect(requete)
			expect_true(as.numeric(requete@query)>0)	
			odbcCloseAll()
		})

context("Loading program")


test_that("Test that working environment is created",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=TRUE,pre_launch_test=TRUE)
			expect_true(exists("envir_stacomi"))
		})

test_that("Test that gWidget loginwindow is loaded ",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=TRUE,pre_launch_test=TRUE)
			expect_true(exists("logw"))
			dispose(logw)
})

test_that("Test that gWidget gr_interface is loaded, without pre_launch_test, nor login window",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=FALSE,pre_launch_test=FALSE)
			expect_true(exists("win"))
			dispose(win)
		})

test_that("gWidget gr_interface is loaded, with pre launch_test, but without login window",{
			require(stacomiR)
			stacomi(gr_interface=TRUE,login_window=FALSE,pre_launch_test=TRUE)
			expect_true(exists("win"))
			dispose(win)
		})

context("Messages")

test_that("Test that messages are loaded from the database",
		{
			object=new("RefMsg")
			object<-charge(object)	
			expect_true(nrow(object@messager)>0)
		})

test_that("Test that  messages without connection are loaded and of the same length",
		{
			object=new("RefMsg")
			object<-createmessage(object,TRUE)	
			msgbase<-get("msg",envir=envir_stacomi)
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,pre_launch_test=FALSE)
			msg<-get("msg",envir=envir_stacomi)
			# objects should have the same length but different languages
			expect_identical(length(msg),length(msgbase))
		})
