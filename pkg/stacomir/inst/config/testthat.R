require(testthat)
getUsername <- function(){
	name <- Sys.info()[["user"]]
	return(name)
}
if(getUsername() == 'cedric.briand')
{
	test_dir("F:/workspace/stacomir/pkg/stacomir/inst/tests/testthat")
}
if(getUsername() == 'marion.legrand')
{
	test_dir("C:/Users/logrami/workspace/stacomir/pkg/stacomir/inst/tests/testthat")
}


test_file("F:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-01BilanMigrationMult.R")
test_file("F:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-02BilanMigration.R")
test_file("F:/workspace/stacomir/pkg/stacomir/inst/tests/testthat/test-03BilanFonctionnementDF.R")