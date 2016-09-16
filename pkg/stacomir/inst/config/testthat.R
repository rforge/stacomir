require(testthat)
test_dir("F:/workspace/stacomir/pkg/stacomir/inst/tests/testthat")
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



