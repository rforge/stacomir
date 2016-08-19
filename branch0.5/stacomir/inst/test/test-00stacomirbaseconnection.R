# Author: cedric.briand
###############################################################################


context("stacomi base connection")

test_that("existence du fichier csv",{
		filecsv<-"C:/Program Files/stacomi/calcmig.csv";
		expect_equivalent(file.access(filecsv,0),0)
	}

)

test_that("Teste que le lien ODBC existe et a la bonne longueur",{
			result<-chargecsv();
			expect_equal(length(result),3)
			expect_equal(length(result$baseODBC),3)
			expect_equal(length(result$sqldf),3)
		})

test_that("Teste que l'environnement de travail est bien cr��",{
			expect_true(exists("envir_stacomi"))
		})



test_that("Teste la m�thode charge les messages de r�f�rence",
		{
			object=new("RefMsg")
			object<-charge(object)	
			expect_true(nrow(object@messager)>0)
		})