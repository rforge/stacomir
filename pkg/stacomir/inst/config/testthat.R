#install.packages("testthat",dependencies=c("Depends", "Imports"))
#install.packages("relax")
require(testthat)


getUsername <- function(){
	name <- Sys.info()[["user"]]
	return(name)
}
if(getUsername() == 'cedric.briand')
{
	test_dir("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat")
}
if(getUsername() == 'marion.legrand')
{
	test_dir("C:/Users/logrami/workspace/stacomir/pkg/stacomir/inst/tests/testthat")
}

test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-00stacomir.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-00zRefclasses.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-01BilanMigrationMult.R")
# warning we don't need to be worried about
#Quoted identifiers should have class SQL, use DBI::SQL() if the caller performs the quoting.
# this comes from incompatibility between  RSQLite 1.1-1 and sqldf
# we don't really use RSQLite and this is only a warning not a problem
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-02BilanMigration.R")
# if errors check existence of dbname test and grants to test on dbname test
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-03BilanFonctionnementDF.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-04BilanFonctionnementDC.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-05Bilan_carlot.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-06Bilan_MigrationInterAnnuelle.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-07BilanAgedemer.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-08BilanArgentee.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-09Bilan_MigrationAnnuelle.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-10BilanConditionEnv.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-11BilanMigrationMultConditionEnv.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-12BilanMigrationCar.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-13BilanAnnuels.R")
test_file("C:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-14BilanEspeces.R")