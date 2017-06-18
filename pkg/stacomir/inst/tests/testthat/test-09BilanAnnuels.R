context("BilanAnnuels")


test_that("Test an instance of BilanAnnuels loaded with choice_c",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","iav.",envir_stacomi)
			bilA<-new("BilanAnnuels")
			bilA<-choice_c(bilA,
					dc=c(5,6,12),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","AGG"),
					anneedebut="1996",
					anneefin="2015",
					silent=TRUE)
			bilA<-connect(bilA,silent=TRUE)	
			expect_s4_class(bilA,"BilanAnnuels")
			rm("envir_stacomi",envir =.GlobalEnv)
		})


test_that("Test methods in BilanAnnuels",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","iav.",envir_stacomi)
			bilA<-new("BilanAnnuels")
			bilA<-choice_c(bilA,
					dc=c(5,6,12),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","AGG"),
					anneedebut="1996",
					anneefin="2015",
					silent=TRUE)			
			bilA<-connect(bilA,silent=TRUE)	
			dev.new()
			plot(bilA,silent=TRUE)
			dev.new()
			barplot(bilA)
			rm("envir_stacomi",envir =.GlobalEnv)			
		})

test_that("Test example bilanMigrationInterAnnuelle_example",
		{
			# check if built with examples (Rtools install --example)
			# the file is generate it examples but later loaded to examples from the class using @example
			# be sure you have built Roxygen documentation before running
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanAnnuels-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				suppressWarnings(source(example_path))
				
		})

test_that("Complement to example",
{
	data(bilA)
	path=file.path(path.expand(get("datawd",envir=envir_stacomi)),
			paste(paste(bilA@dc@dc_selectionne,collapse="+"),"_",
					paste(bilA@taxons@data$tax_code,collapse="+"),"_",
					paste(bilA@stades@data$std_code,collapse="+"),"_",
					bilA@anneedebut@annee_selectionnee,":",
					bilA@anneefin@annee_selectionnee,".html",sep=""),fsep ="/")
# here you can add an argument file=path
	expect_output(print(xtbilA,type="html"))
	
# the following uses the "addtorow" argument which creates nice column headings,
# format.args creates a thousand separator
# again this will need to be saved in a file using the file argument
	expect_output(print(xtbilA,
			add.to.row=get("addtorow",envir_stacomi),
			include.rownames = TRUE,
			include.colnames = FALSE,
			format.args = list(big.mark = " ", decimal.mark = ",")
	))
# barplot transforms the data, further arguments can be passed as to barplot
			dev.new()
	barplot(bilA)
	dev.new()
	barplot(bilA,
			args.legend=list(x="topleft",bty = "n"),
			col=c("#CA003E","#1A9266","#E10168","#005327","#FF9194"))
	
# An example with custom arguments for legend.text (overriding plot defauts)
	data(bilAM)
	if (requireNamespace("RColorBrewer", quietly = TRUE)){
		lesdc<-bilAM@dc@data$dc_code[bilAM@dc@data$dc%in%bilAM@dc@dc_selectionne]
		barplot(bilAM,
				legend.text=lesdc,
				args.legend=list(x="topleft",bty = "n"),
				col=RColorBrewer::brewer.pal(9,"Spectral"),
				beside=TRUE)
	}
	dev.new()
	plot(bilAM,silent=TRUE)
	graphics.off()
})

test_that("test xtable method for BilanAnnuels",{
			require(stacomiR)
# launching stacomi without selecting the scheme or interface
			stacomi(gr_interface=FALSE,
					login_window=FALSE,
					database_expected=FALSE)
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","iav.",envir_stacomi)
			bilA<-new("BilanAnnuels")
			bilA<-choice_c(bilA,
					dc=c(5,6,12),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","AGG"),
					anneedebut="1996",
					anneefin="2015",
					silent=TRUE)
			bilA<-connect(bilA)	
			data(bilA)
			xtbilA<-xtable(bilA,
					dc_name=c("Passe bassins","Piege anguille RG","Piege anguille RD"),
					tax_name="Anguille",
					std_name=c("Arg.","Jaun."))
			expect_equal(class(xtbilA)[1],"xtable","BilanAnnuel should have an xtable method")
			xtbilA<-xtable(bilA,
					dc_name=c("Passe bassins","Piege anguille RG","Piege anguille RD"),
					tax_name="Anguille",
					std_name=c("Arg.","Jaun."))
			expect_equal(class(xtbilA)[1],"xtable","BilanAnnuel should have an xtable method")
			path=file.path(path.expand(get("datawd",envir=envir_stacomi)),
					paste(paste(bilA@dc@dc_selectionne,collapse="+"),"_",
							paste(bilA@taxons@data$tax_code,collapse="+"),"_",
							paste(bilA@stades@data$std_code,collapse="+"),"_",
							bilA@anneedebut@annee_selectionnee,":",
							bilA@anneefin@annee_selectionnee,".html",sep=""),fsep ="/")
			
			expect_output(print(xtbilA,type="html"))			
			expect_output(print(xtbilA,
							add.to.row=get("addtorow",envir_stacomi),
							include.rownames = TRUE,
							include.colnames = FALSE,
							format.args = list(big.mark = " ", decimal.mark = ",")))
		})

test_that("test plot methods for BilanAnnuels",{
			data(bilAM)
			dev.new()
			if (requireNamespace("RColorBrewer", quietly = TRUE)){
				lesdc<-bilAM@dc@data$dc_code[bilAM@dc@data$dc%in%bilAM@dc@dc_selectionne]
				barplot(bilAM,
						legend.text=lesdc,
						args.legend=list(x="topleft",bty = "n"),
						col=RColorBrewer::brewer.pal(9,"Spectral"),
						beside=TRUE)
			}
			dev.new()
			plot(bilAM,silent=TRUE)
			graphics.off()		
			
		})
