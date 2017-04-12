#' Class "BilanArgentee"
#' 
#' the BilanArgentee class is used to calculate various statistics about the silver eel run
#' @include create_generic.r
#' @include RefDC.r
#' @include RefTaxon.r
#' @include RefStades.r
#' @include RefHorodate.r
#' @include Refpar.r
#' @note This class is displayed by interface_bilanArgentee
#' @slot data A data frame with data generated from the database
#' @slot calcdata A list of dc with processed data. Each dc contains a data frame with 
#' \itemize{
#' \item (1) qualitative data on body contrast (CONT), presence of punctuation on the lateral line (LINP)
#' \item (2) quantitative data "BL" Body length,"W" weight,"Dv" vertical eye diameter,"Dh" horizontal eye diameter,"FL" pectoral fin length
#' \item (3) calculated durif stages, Pankhurst's index, Fulton's body weight coefficient K_ful
#' \item (4) other columns containing data pertaining to the sample and the control operation:  lot_identifiant,ope_identifiant,
#' ope_dic_identifiant,ope_date_debut,ope_date_fin,dev_code (destination code of fish),
#' dev_libelle (text for destination of fish)
#' }
#' @slot dc Object of class \link{RefDC-class}: the control devices
#' @slot taxons Object of class \link{RefTaxon-class}: the speciess
#' @slot stades Object of class \link{RefStades-class} : the stages of the fish
#' @slot par Object of class \link{Refpar-class}: the parameters used
#' @slot horodatedebut An object of class \code{RefHorodate-class}
#' @slot horodatefin An object of class \code{RefHorodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanArgentee", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilanArgentee_example.R
#' @export 
setClass(Class="BilanArgentee",
		representation= representation(
				data="data.frame",
				calcdata="list",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				par="Refpar",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate"
		),
		prototype=prototype(data=data.frame(),
				calcdata=list(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				par=new("Refpar"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate")				
		))
setValidity("BilanArgentee",function(object)
		{
			rep1=object@taxons@data$tax_code[1]=='2038'
			label1<-'BilanArgentee should only be for eel (tax_code=2038)'
			rep2=all(object@stades@data$std_code%in%c('AGG','AGJ'))
			label2<-'Only stages silver (AGG) and yellow (AGJ) should be used in BilanArgentee'
			return(ifelse(rep1 & rep2 , TRUE ,c(label1,label2)[!c(rep1, rep2)]))
		}   
)
#' connect method for BilanArgentee
#' 
#' @param object An object of class \link{BilanArgentee-class}
#' @param silent Boolean if TRUE messages are not displayed
#' @return An object of class \link{BilanArgentee-class} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanArgentee"),definition=function(object,silent=FALSE) {
			requete<-new("RequeteODBCwheredate")
			requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			requete@select= paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car",sep="")
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@datedebut<-object@horodatedebut@horodate
			requete@datefin<-object@horodatefin@horodate
			requete@order_by="ORDER BY ope_date_debut"
			requete@and=paste(" AND ope_dic_identifiant in ",vector_to_listsql(object@dc@dc_selectionne),
					" AND lot_tax_code in ", vector_to_listsql(object@taxons@data$tax_code),
					" AND lot_std_code in ", vector_to_listsql(object@stades@data$std_code),
					" AND car_par_code in ", vector_to_listsql(object@par@par_selectionne), sep="")
			requete<-stacomirtools::connect(requete) 
			object@data<-requete@query
			if (!silent) funout(gettext("Data loaded",domain="R-stacomiR"))
			return(object)
		})


#' charge method for BilanArgentee class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @param object An object of class \link{BilanArgentee-class} 
#' @param h a handler
#' @return An object of class \link{BilanArgentee-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @return An object of the class
setMethod("charge",signature=signature("BilanArgentee"),definition=function(object,h) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			} 
			if (exists("refTaxon",envir_stacomi)) {
				object@taxons<-get("refTaxon",envir_stacomi)
			} else {
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refpar",envir_stacomi)) {
				object@par<-get("refpar",envir_stacomi)
			} else {
				funout(gettext("You need to choose a parameter, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}		
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_arg_date_debut",envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilan_arg_date_debut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			# rem id
			if (exists("bilan_arg_date_fin",envir_stacomi)) {
				object@horodatefin@horodate<-get("bilan_arg_date_fin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}       
			
			return(object)
			validObject(object)
		})


#' command line interface for BilanArgentee class
#' @param object An object of class \link{BilanArgentee-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons '2038=Anguilla anguilla',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades 'AGG'
#' @param par Parameters chosen for the Bilan are body size (1786), vertical eye diameter (BBBB), horizontal eye diameter (CCCC),
#' body contrast (CONT), presence of punctuation on the lateral line (LINP), length of the pectoral fin (PECT)
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{BilanMigration-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class}, \link{Refpar-class} and two slots of \link{RefHorodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanArgentee"),definition=function(object,
				dc,
				taxons=2038,
				stades='AGG',
				par=c('1786','CCCC','BBBB','CONT','LINP','A111','PECT'),
				horodatedebut,
				horodatefin,
				silent=FALSE){
			# code for debug using example
			#bilanArg<-b_carlothorodatedebut="2010-01-01";horodatefin="2015-12-31"
			bilanArg<-object
			bilanArg@dc=charge(bilanArg@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilanArg@dc<-choice_c(object=bilanArg@dc,dc)
			# only taxa present in the bilanMigration are used
			bilanArg@taxons<-charge_avec_filtre(object=bilanArg@taxons,bilanArg@dc@dc_selectionne)			
			bilanArg@taxons<-choice_c(bilanArg@taxons,taxons)
			bilanArg@stades<-charge_avec_filtre(object=bilanArg@stades,bilanArg@dc@dc_selectionne,bilanArg@taxons@data$tax_code)	
			bilanArg@stades<-choice_c(bilanArg@stades,stades)
			bilanArg@par<-charge_avec_filtre(object=bilanArg@par,bilanArg@dc@dc_selectionne,bilanArg@taxons@data$tax_code,bilanArg@stades@data$std_code)	
			bilanArg@par<-choice_c(bilanArg@par,par,silent=silent)
			bilanArg@horodatedebut<-choice_c(object=bilanArg@horodatedebut,
					nomassign="bilanArg_date_debut",
					funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatedebut, 
					silent=silent)
			bilanArg@horodatefin<-choice_c(bilanArg@horodatefin,
					nomassign="bilanArg_date_fin",
					funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatefin,
					silent=silent)
			validObject(bilanArg)
			return(bilanArg)
		})

#' Calcule method for BilanArgentee, this method will pass the data from long to wide format 
#' ( one line per individual) and calculate Durif silvering index and Pankhurst and Fulton's K.
#' 
#' @param object An object of class \code{\link{BilanArgentee-class}} 
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("calcule",signature=signature("BilanArgentee"),definition=function(object,silent) {
			bilanArg<-object
			if(nrow(bilanArg@data)==0) {
				funout(gettext("No data of silver or yellow eel on the selected period",domain="R-stacomiR"), arret=TRUE)
			}   
			arg=bilanArg@data # on recupere le data.frame
			
			
			lesdc<-bilanArg@dc@dc_selectionne
			parquant<-c("1786","A111","BBBB","CCCC","PECT")
			parqual<-c("CONT","LINP")
			for (i in 1:length(lesdc)){
				dc<-lesdc[i]
				other<-dplyr::select(arg,lot_identifiant, ope_dic_identifiant,ope_identifiant, ope_date_debut,ope_date_fin,dev_code,dev_libelle) 
				other<-dplyr::filter(other,ope_dic_identifiant==dc)
				other<-dplyr::group_by(other,lot_identifiant,ope_identifiant,ope_dic_identifiant,ope_date_debut,ope_date_fin,dev_code,dev_libelle)
				other<-dplyr::summarize(other)
				other<-as.data.frame(other)
				other<-funtraitementdate(other,"ope_date_debut",jour_an = TRUE, jour_mois = FALSE)
				# extracting the dc from the array
				# all parms are there but some are null, ie val_libelle is null for quantitative parm and
				# car_valeur_quantitatif is null for for qualitative parms
				matqual<-reshape2::acast(arg[arg$ope_dic_identifiant==lesdc[i],],
						lot_identifiant~car_par_code+car_val_identifiant,
						value.var="val_libelle",
						drop=TRUE)
				matquant<-reshape2::acast(arg[arg$ope_dic_identifiant==lesdc[i],],
						lot_identifiant~car_par_code+car_val_identifiant,
						value.var="car_valeur_quantitatif",
						drop=TRUE)
				
				# this function will select the parameters one by one
				# test them for pattern against column name
				# and return the column. So a data frame of quantitative or qualitative parm are returned
				fn<-function(X,mat){
					veccol<-grepl(X,dimnames(mat)[[2]])
					return(mat[,veccol])
				}
				matquant2<-sapply(X=parquant,FUN=fn,mat=matquant)
				colnames(matquant2)<-c("BL","W","Dv","Dh","FL")
				
				matqual2<-sapply(X=parqual,FUN=fn,mat=matqual,simplify=FALSE)
				# now matquant2 only contain the correct columns
				# matqual has two column for a single qualitative variable, which is wrong
				# we will merge them
				
				# however there is a bug if only one value is present
				# depending on the data structure there might a bug
				# when there is only one dimension (ie on instance of factor where there should be two)
				for (z in 1:length(matqual2)){		
					if (is.null(dim(matqual2[[z]])[2])) matqual2[[z]]<-cbind(matqual2[[z]],NA)
				}
				matqual3<-matrix(NA,nrow=nrow(matqual2[[1]]),ncol=length(parqual))
				# below if the data in  the first column is NA we choose the second
				# which migh also be NA in which case the result becomes a NA
				
				for (j in 1:length(parqual)){
					theparqual=parqual[j]
					matqual3[,j]<-apply(matqual2[[theparqual]],1,function(X) ifelse(is.na(X[1]),X[2],X[1]))
				}
				dd<-as.data.frame(matqual3)
				rownames(dd)<-rownames(matquant2)
				colnames(dd)<-parqual
				dd$stage<-as.vector(f_stade_Durif(matquant2))
				dd<-cbind(dd,as.data.frame(matquant2))
				dd$MD<-rowMeans(dd[,c("Dv","Dh")],na.rm=TRUE)
				dd$Pankhurst=100*(dd$MD/2)^2*pi/dd$BL
				#K = 100 Wt /TL3 with Wt in g and TL in cm	(Cone 1989). (Acou, 2009)
				dd$K_ful=100*dd$W/(dd$BL/10)^3
				ddd<-cbind(other,dd)
				bilanArg@calcdata[[as.character(dc)]]<-ddd			
			}
			assign("bilanArg",bilanArg,envir_stacomi)
			return(bilanArg)
		})


#' Plots of various type for BilanArgentee
#' 
#' @param x An object of class \link{BilanArgentee-class}
#' @param plot.type Default "1"
#'  \itemize{
#' 		\item{plot.type="1"}{Lattice plot of Durif's stages according to Body Length and Eye Index (average of vertical and horizontal diameters). 
#' If several DC are provided then a comparison of data per dc is provided}
#' 		\item{plot.type="2"}{Lattice plot giving a comparison of Durif's stage proportion over time, if several DC are provided an annual comparison 
#' is proposed, if only one DC is provided then the migration is split into month.}
#' 		\item{plot.type="3"}{ Series of graphs showing  mean Fulton's coefficient, Pankhurst eye index,	along
#' with a size weight analysis and regression using robust regression (rlm more robust to the presence of outliers)}
#' 			\item{plot.type="4"}{ Lattice cloud plot of Pankurst~ Body Length ~ weight)}
#' }
#' @param silent Stops displaying the messages.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanArgentee plot.bilanArg
#' @export
setMethod("plot", signature(x = "BilanArgentee", y = "missing"), definition=function(x, plot.type="1", silent=FALSE){ 
			#bilanArg<-b_carlot;require(ggplot2);plot.type="1"
			#browser()
			bilanArg<-x
			plot.type<-as.character(plot.type)# to pass also characters
			if (!plot.type%in%c("1","2","3","4")) stop('plot.type must be 1,2,3 or 4')
			if (exists("bilanArg",envir_stacomi)) {
				bilanArg<-get("bilanArg",envir_stacomi)
			} else {      
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			}
			dat<-bilanArg@calcdata
			# cols are using viridis::inferno(6,alpha=0.9)
			blue_for_males<-grDevices::adjustcolor("#008490", alpha.f = 0.8)
			
			datdc<-data.frame()
			
			
			for (i in 1:length(dat)){
				datdc<-rbind(datdc,dat[[i]])	
			}
			
			
			
			# trellis.par.get()
			datdc$stage<-factor(datdc$stage,levels=c("I","FII","FIII","FIV","FV","MII"))
			datdc$ope_dic_identifiant<-as.factor(datdc$ope_dic_identifiant)
			datdc$ouv<-NA
			for (i in 1:length(bilanArg@dc@dc_selectionne)){
				datdc$ouv[datdc$ope_dic_identifiant==bilanArg@dc@dc_selectionne[i]]<-
						bilanArg@dc@data[bilanArg@dc@data$dc==bilanArg@dc@dc_selectionne[i],"ouv_libelle"]
			}
			
			
			
			
			#################################################
			# plot.type =1 Eye, length category durif stages
			#################################################
			
			if (plot.type=="1"){		
				
				
				my.settings <- list(
						superpose.symbol=list(
								col=c("Lime green","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males), 
								pch=c(3,4,8,15,16,17),
								cex=c(1,1,1,1,1,1),
								alpha=c(0.9,0.9,0.9,0.9,0.9,0.9)								
						),
						superpose.line=list(
								col=c("#FBA338","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males)
						),
						strip.background=list(col="#932667E6"),
						strip.border=list(col="black")
				)
				lattice::trellis.par.set(my.settings)
				# show.settings()
				if (length(dat)>1){
					form<-as.formula(MD ~ BL|ouv)
				} else {
					form<-as.formula(MD ~ BL)
				}
				
				xy.plot<-lattice::xyplot(form,data=datdc,	
						group=stage,			
						type = c("p"),
						par.settings = my.settings,
						xlab=gettext("size (BL mm)",domain="R-stacomiR"),
						ylab=gettext("Mean eye diameter (MD mm)",domain="R-stacomiR"),
						par.strip.text=list(col="white", font=2),
						auto.key=list(title=gettext("Silvering stages (Durif et al. 2009)",domain="R-stacomiR"),
								cex.title=1.2,
								space="top",
								columns=6,
								between.columns=1
						)						
				)
				# draw lines in lattice
				xy.plot<-update(xy.plot, panel = function(...) {
							lattice::panel.abline(h = c(6.5,8), 
									v=c(300,450,500) ,
									lty = "dotted", 
									col = "light grey")
							lattice::panel.xyplot(...)
						})
				
				return(xy.plot)
				
			}
			######################################
			# Migration according to stage, month and year
			######################################
			if (plot.type=="2"){	
				datdc1<-dplyr::select(datdc,ouv,annee,mois,stage)
				datdc1<-dplyr::group_by(datdc1,ouv,annee,mois,stage)
				datdc1<-dplyr::summarize(datdc1,N=n())
				datdc1<-as.data.frame(datdc1)
				# show.settings()
				my.settings <- list(
						superpose.polygon=list(
								col=c("Lime green","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males),								
								alpha=c(0.9,0.9,0.9,0.9,0.9,0.9)								
						),
						superpose.line=list(
								col=c("#FBA338","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males)
						),
						#colfn<-colorRampPalette(c("#1C4587", "#BBC7DB"),space = "Lab")
						#colfn(7)
						strip.background=list(col=c("#1B4586","#3E5894","#596DA2","#7282B0","#8A98BE","#A2AFCC","#BAC6DA")),
						strip.border=list(col="black")
				)
				lattice::trellis.par.set(my.settings)
				
				# show.settings()
				if (length(dat)>1){
					form<-as.formula(N ~ annee|ouv)
				} else {
					form<-as.formula(N ~ mois|annee)
				}
				
				bb<-lattice::barchart(form,data=datdc1,	
						group=stage,		
						xlab=gettext("Month",domain="R-stacomiR"),
						ylab=gettext("Number",domain="R-stacomiR"),
						par.strip.text=list(col="white", font=2),
						auto.key=list(title=gettext("Number by silvering stage",domain="R-stacomiR"),
								cex.title=1.2,
								space="top",
								columns=6,
								between.columns=0.5
						)						
				)	
				return(bb)
				
			}
			######################################
			# Series of graphs showing proportion of stage, mean Fulton's coefficient, Pankhurst eye index,
			# body weight, body size, sex ratio.
			######################################
			if (plot.type=="3"){
				layout(matrix(c(1,2,3,4,4,5,6,6,7), 3, 3, byrow = TRUE), 
						widths=c(3,3,1), heights=c(3,1,3))
				# width 331 sets the last column relative width
				# same for rows
				par(mar=c(3,4.1,4.1,2.1))# ressetting to default
				datdc<-chnames(datdc,"ope_dic_identifiant","dc")
				lesdc<-unique(datdc$dc)
				datdc$sex<-"F"
				datdc$sex[datdc$BL<450]<-"M"
				
				#############
				# Fulton
				#############
				moy<-tapply(datdc$K_ful,list(datdc$dc,datdc$sex),mean,na.rm=TRUE)
				sd<- tapply(datdc$K_ful,list(datdc$dc,datdc$sex),sd,na.rm=TRUE) # sample standard deviation 
				n<-tapply(datdc$K_ful,list(datdc$dc,datdc$sex),length)
				SE = sd/sqrt(n)	
				plotTop=max(moy+3*SE,na.rm=TRUE)
				
				
				bp<-barplot(moy,
						beside = TRUE, las = 1,
						ylim = c(0, plotTop),
						cex.names = 0.75,
						main = "Fulton coefficient (+-2SE)",
						ylab = "Fulton K",
						xlab = "",
						border = "black", axes = TRUE,
				#legend.text = TRUE,
				#args.legend = list(title = "DC", 
				#		x = "topright",
				#		cex = .7)
				)
				graphics::segments(bp, moy - SE * 2, bp,
						moy + SE * 2, lwd = 2)
				
				graphics::arrows(bp, moy - SE * 2, bp,
						moy + SE * 2, lwd = 2, angle = 90,
						code = 3, length = 0.05)		
				
				
				#############
				# Pankhurst
				#############
				moy<-tapply(datdc$Pankhurst,list(datdc$dc,datdc$sex),mean,na.rm=TRUE)
				sd<- tapply(datdc$Pankhurst,list(datdc$dc,datdc$sex),sd,na.rm=TRUE) # sample standard deviation 
				n<-tapply(datdc$Pankhurst,list(datdc$dc,datdc$sex),length)
				SE = sd/sqrt(n)	
				plotTop=max(moy+3*SE,na.rm=TRUE)
				
				
				bp<-barplot(moy,
						beside = TRUE, las = 1,
						ylim = c(0, plotTop),
						cex.names = 0.75,
						main = "Pankhurst (+-2SE)",
						ylab = "Pankhurst eye index",
						xlab = "",
						border = "black", axes = TRUE,
				#legend.text = TRUE,
				#args.legend = list(title = "DC", 
				#		x = "topright",
				#		cex = .7)
				)
				segments(bp, moy - SE * 2, bp,
						moy + SE * 2, lwd = 2)
				
				arrows(bp, moy - SE * 2, bp,
						moy + SE * 2, lwd = 2, angle = 90,
						code = 3, length = 0.05)		
				
				#############
				# empty plot
				#############		
				op<-par(mar=c(1,1,1,1))
				plot(1, type="n", axes=F, xlab="", ylab="")
				legend("center",fill =grDevices::grey.colors(nrow(moy)),legend=unique(datdc$dc))
				# grey.colors is the default color generation for barplot 
				#############
				# size hist 
				#############
				par(mar=c(1,4.1,1,1)) 
				for (i in 1:length(lesdc)){
					indexdc<-datdc$dc==lesdc[i]
					histxn<-graphics::hist(datdc$BL[indexdc],breaks=seq(250,1000,by=50),plot=FALSE)$density
					if (i==1) histx<-histxn else histx<-cbind(histx,histxn)
					
				}	
				if (length(lesdc)>1) colnames(histx)<-lesdc
				barplot(height=t(histx),space=0,beside=FALSE, las = 1,horiz=FALSE,legend.text = FALSE,axes=FALSE)	
				#############
				# empty plot
				#############		
				op<-par(mar=c(1,1,1,1))
				plot(1, type="n", axes=F, xlab="", ylab="")
				
				#############
				# size -weight
				#############
				par(mar=c(5.1,4.1,1,1)) # blur bottom left up right
				plot(datdc$BL,datdc$W,type="n",
						xlab=gettext("Size (mm)",domain="R-stacomiR"),
						ylab=gettext("Weight(g)",domain="R-stacomiR"),
						xlim=c(250,1000),ylim=c(0,2000))
				abline(v=seq(250,1000,by=50), col = "lightgray",lty=2)
				abline(h=seq(0,2000,by=100),col="lightgray",lty=2)
				# some alpha blending to better see the points :
				lescol<-ggplot2::alpha(grDevices::grey.colors(nrow(moy)),0.8)
				for (i in 1:length(lesdc)){
					indexdc<-datdc$dc==lesdc[i]
					points(datdc$BL[indexdc],datdc$W[indexdc],pch=16,col=lescol[i],cex=0.8)
					
				}
				######################"
				# Size - weight model using robust regression
				######################
				subdatdc<-datdc[,c("BL","W")]
				subdatdc$BL3<-(subdatdc$BL/1000)^3
				# plot(subdatdc$W~subdatdc$BL3)
				
				rlmmodb<-MASS::rlm(W~0+BL3,data=subdatdc)
				#summary(rlmmodb)
				newdata<-data.frame("BL"=seq(250,1000,by=50),"BL3"=(seq(250,1000,by=50)/1000)^3)
				pred<-predict(rlmmodb,newdata=newdata,se.fit=TRUE,type="response",interval="prediction")
				newdata$predlm<-pred$fit[,1]
				newdata$predlowIC<-pred$fit[,2]
				newdata$predhighIC<-pred$fit[,3]
				
				points(newdata$BL,newdata$predlm,type="l")
				points(newdata$BL,newdata$predlowIC,type="l",lty=2,col="grey50")
				points(newdata$BL,newdata$predhighIC,type="l",lty=2,col="grey50")
				
				text(400,1500,stringr::str_c("W=",round(coefficients(rlmmodb),1)," BL^3"))
				
				#############
				# weight hist rotate
				#############
				par(mar=c(5.1,1,1,1)) 
				for (i in 1:length(lesdc)){
					indexdc<-datdc$dc==lesdc[i]
					histyn<-hist(datdc$W[indexdc],plot=FALSE,breaks=seq(0,2000,by=100))$density
					if (i==1) histy<-histyn else histy<-cbind(histy,histyn)
					
				}		
				if (length(lesdc)>1) colnames(histy)<-lesdc
				barplot(height=t(histy),space=0,beside=FALSE, las = 1,horiz=TRUE,legend.text = FALSE,axes=FALSE)	
				
				
			}
			if (plot.type=="4"){
				#creating a shingle with some overlaps
				my.settings <- list(
						superpose.polygon=list(
								col=c("Lime green","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males),								
								alpha=c(0.9,0.9,0.9,0.9,0.9,0.9)								
						),
						superpose.line=list(
								col=c("#FBA338","#420A68E6","#932667E6","#DD513AE6","#FCA50AE6",blue_for_males)
						),
						#colfn<-colorRampPalette(c("#1C4587", "#BBC7DB"),space = "Lab")
						#colfn(7)
						strip.background=list(col=c("#1B4586","#3E5894","#596DA2","#7282B0","#8A98BE","#A2AFCC","#BAC6DA")),
						strip.border=list(col="black")
				)
				lattice::trellis.par.set(my.settings)
				
				ccc<-lattice::cloud(Pankhurst ~ W * BL|ouv, data = datdc,group=stage,
						screen = list(x = -90, y = 70), distance = .4, zoom = .6,strip = lattice::strip.custom(par.strip.text=list(col="white")))
				return(ccc)
			}
			
			
		})

#' summary for BilanArgentee 
#' @param object An object of class \code{\link{BilanArgentee-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanArgentee"),definition=function(object,silent=FALSE,...){
			bilanArg<-object
			if (exists("bilanArg",envir_stacomi)) {
				bilanArg<-get("bilanArg",envir_stacomi)
			} else {      
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			}
			dat<-bilanArg@calcdata
			# cols are using viridis::inferno(6,alpha=0.9)
			
			printstat<-function(vec){
				moy<-mean(vec,na.rm=TRUE)
				sd<- sd(vec,na.rm=TRUE) # sample standard deviation 
				n<-length(vec[!is.na(vec)])
				SE = sd/sqrt(n)
				print(noquote(stringr::str_c("mean=",round(moy,2),",SD=",round(sd,2),",N=",n,",SE=",round(SE,2))))
				return(list("mean"=moy,"SD"=sd,"N"=n,"SE"=SE))
			}
			result<-list()
			for (i in 1:length(dat)){
				datdc<-	dat[[i]]
				ouvrage<-
						bilanArg@dc@data[bilanArg@dc@data$dc==bilanArg@dc@dc_selectionne[i],"ouv_libelle"]
				dc<-as.character(unique(datdc$ope_dic_identifiant))
				result[[dc]]<-list()
				result[[dc]][["ouvrage"]]<-ouvrage
				print(noquote(stringr::str_c("Statistics for dam : ",ouvrage)))
				print(noquote("========================"))
				print(noquote("Stages Durif"))
				print(table(datdc$stage))
				result[[dc]][["Stages"]]<-table(datdc$stage)
				print(noquote("-----------------------"))
				print(noquote("Pankhurst"))
				print(noquote("-----------------------"))
				result[[dc]][["Pankhurst"]]<-printstat(datdc$Pankhurst)		
				print(noquote("-----------------------"))
				print(noquote('Eye diameter (mm)'))		
				print(noquote("-----------------------"))
				result[[dc]][["MD"]]<-printstat(datdc$MD)				
				print(noquote("-----------------------"))
				print(noquote('Length (mm)'))	
				print(noquote("-----------------------"))
				result[[dc]][["BL"]]<-printstat(datdc$BL)	
				print(noquote("-----------------------"))
				print(noquote('Weight (g)'))	
				print(noquote("-----------------------"))
				result[[dc]][["W"]]<-printstat(datdc$W)	
			}
			return(result)		
		})

#' Method to print the command line of the object
#' @param x An object of class BilanArgentee
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @export
setMethod("print",signature=signature("BilanArgentee"),definition=function(x,...){ 
			sortie1<-"bilanArg=new('BilanArgentee')"
			sortie2<-stringr::str_c("bilanArg=choice_c(bilanArg,",
					"dc=c(",stringr::str_c(x@dc@dc_selectionne,collapse=","),"),",
					"taxons=c(",stringr::str_c(shQuote(x@taxons@data$tax_nom_latin),collapse=","),"),",
					"stades=c(",stringr::str_c(shQuote(x@stades@data$std_code),collapse=","),"),",	
					"par=c(",stringr::str_c(shQuote(x@par@par_selectionne),collapse=","),"),",	
					"horodatedebut=",shQuote(strftime(x@horodatedebut@horodate,format="%d/%m/%Y %H-%M-%S")),
					",horodatefin=",shQuote(strftime(x@horodatefin@horodate,format="%d/%m/%Y %H-%M-%S")),")")
			# removing backslashes
			funout(sortie1)
			funout(stringr::str_c(sortie2,...))
			return(invisible(NULL))
		})


#' funplotBilanArgentee 
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler, with action 1,2,3 or 4 
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funplotBilanArgentee = function(h,...) {
	bilanArg<-get(x="bilan_arg",envir=envir_stacomi)
	bilanArg<-charge(bilanArg)
	bilanArg<-connect(bilanArg)
	bilanArg<-calcule(bilanArg)
	#plot.type is determined by button in h$action
	the_plot<-plot(bilanArg,plot.type=h$action)
	print(the_plot)
}


#' table function
#' 
#' funtableBilanArgentee shows a table of results in gdf
#' @param h hanlder passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtableBilanArgentee = function(h,...) {
	bilanArg=charge(bilanArg)
	bilanArg<-connect(bilanArg)
	vue_ope_lot=bilanArg@requete@query # on recupere le data.frame
	assign("bilanArg",bilanArg,envir_stacomi)#assign("bilanArg",vue_ope_lot,envir_stacomi)
	funout(gettext("Size (BL mm)",domain="R-stacomiR"))
	vue_ope_lot[is.na(vue_ope_lot)]<-""
	vue_ope_lot$ope_date_debut=as.character(vue_ope_lot$ope_date_debut)
	vue_ope_lot$ope_date_fin=as.character(vue_ope_lot$ope_date_fin)   
	gdf(vue_ope_lot, container=TRUE)
}


#' Function to calculate the stages from Durif
#' 
#' @param data A dataset with columns BL, W, Dv, Dh, FL corresponding to body length (mm),
#' Weight (g), vertical eye diameter (mm), vertical eye diameter (mm), and pectoral fin length (mm)
#' @author Laurent Beaulaton \email{laurent.beaulaton"at"onema.fr}
#' @export
f_stade_Durif = function(data){
	# see section Good Practise in ? data
	data(coef_Durif,envir = environment())
	stopifnot(colnames(data)==c("BL","W","Dv","Dh","FL"))
	data<-cbind(1,data[,c(1,2,5)],rowMeans(data[,c("Dv","Dh")],na.rm=TRUE))
	colnames(data)<-c("Constant","BL","W","FL","MD")
	data<-data[,c(1,2,3,5,4)]
	indices<-data%*%coef_Durif
	return(unlist(apply(indices,1,function(X)ifelse(is.na(X[1]),NA,names(which.max(X))))))
}

