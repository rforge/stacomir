context("BilanAnnuels")

test_that("test creating an instance of BilanEspeces",{
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
	expect_s4_class(bilA,
			"BilanAnnuels")
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

test_that("test plot method for BilanAnnuels",{
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