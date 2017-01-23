#' Class "BilanAgedemer"
#' 
#' the BilanAgedemer class is used to calculate various statistics about the silver eel run
#' @include RefDC.r
#' @include RefTaxon.r
#' @include RefStades.r
#' @include RefHorodate.r
#' @include Refpar.r
#' @note This class is displayed by interface_bilan_admentee
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
#' \code{new("BilanAgedemer", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilan_admentee_example.R
#' @export 
setClass(Class="BilanAgedemer",
		representation= representation(
				data="data.frame",
				calcdata="list",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				par="Refpar",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate",
				limit1hm="RefTextBox",
				limit2hm="RefTextBox"
		),
		prototype=prototype(data=data.frame(),
				calcdata=list(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				par=new("Refpar"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate"),
				limit1hm=new("RefTextBox"),
				limit2hm=new("RefTextBox")
		))
setValidity("BilanAgedemer",function(object)
		{
			rep1=object@taxons@data$tax_code[1]=='2220'
			label1<-'BilanAgedemer should only be for salmon (tax_code=2220)'
			rep2=all(object@stades@data$std_code%in%c('5','11','BEC','BER','IND'))
			label2<-'Only stages 5,11,BEC,BER,IND should be used in BilanAgedemer'
			return(ifelse(rep1 & rep2 , TRUE ,c(label1,label2)[!c(rep1, rep2)]))
		}   
)
#' connect method for BilanAgedemer
#' 
#' @param object An object of class \link{BilanAgedemer-class}
#' @param silent Boolean if TRUE messages are not displayed
#' @return An object of class \link{BilanAgedemer-class} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanAgedemer"),definition=function(object,silent=FALSE) {
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
			if (!silent) funout(get("msg",envir_stacomi)$BilanAgedemer.1)
			return(object)
		})


#' charge method for BilanAgedemer class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @param object An object of class \link{BilanAgedemer-class} 
#' @param h a handler
#' @return An object of class \link{BilanAgedemer-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @return An object of the class
setMethod("charge",signature=signature("BilanAgedemer"),definition=function(object,h) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			} 
			if (exists("refTaxon",envir_stacomi)) {
				object@taxons<-get("refTaxon",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("refpar",envir_stacomi)) {
				object@par<-get("refpar",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.4,arret=TRUE)
			}		
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_adm_date_debut",envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilan_adm_date_debut",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)
			}
			# rem id
			if (exists("bilan_adm_date_fin",envir_stacomi)) {
				object@horodatefin@horodate<-get("bilan_adm_date_fin",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)
			}       
			
			return(object)
			validObject(object)
		})


#' command line interface for BilanAgedemer class
#' @param object An object of class \link{BilanAgedemer-class}
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
setMethod("choice_c",signature=signature("BilanAgedemer"),definition=function(object,
				dc,
				taxons=2220,
				stades=c('5','11','BEC','BER','IND'),
				par=c('1786','1785','C001'),
				horodatedebut,
				horodatefin,
				limit1hm,
				limit2hm,
				silent=FALSE){
			# code for debug using example
			#horodatedebut="2012-01-01";horodatefin="2013-12-31";dc=c(107,108,101);
			#taxons='2220';	stades=c('5','11','BEC','BER','IND');par=c('1786','1785','C001');silent=FALSE
			if (!(is.numeric(limit1hm)|is.integer(limit1hm))) funout("limit1hm should be numeric or integer",arret=TRUE)
			if (!(is.numeric(limit2hm)|is.integer(limit2hm))) funout("limit2hm should be numeric or integer",arret=TRUE)
			
			bilan_adm<-object
			bilan_adm@dc=charge(bilan_adm@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilan_adm@dc<-choice_c(object=bilan_adm@dc,dc)
			# only taxa present in the bilanMigration are used
			bilan_adm@taxons<-charge_avec_filtre(object=bilan_adm@taxons,bilan_adm@dc@dc_selectionne)			
			bilan_adm@taxons<-choice_c(bilan_adm@taxons,taxons)
			bilan_adm@stades<-charge_avec_filtre(object=bilan_adm@stades,bilan_adm@dc@dc_selectionne,bilan_adm@taxons@data$tax_code)	
			bilan_adm@stades<-choice_c(bilan_adm@stades,stades)
			bilan_adm@par<-charge_avec_filtre(object=bilan_adm@par,bilan_adm@dc@dc_selectionne,bilan_adm@taxons@data$tax_code,bilan_adm@stades@data$std_code)	
			bilan_adm@par<-choice_c(bilan_adm@par,par,silent=silent)
			bilan_adm@horodatedebut<-choice_c(object=bilan_adm@horodatedebut,
					nomassign="bilan_adm_date_debut",
					funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
					horodate=horodatedebut, 
					silent=silent)
			bilan_adm@horodatefin<-choice_c(bilan_adm@horodatefin,
					nomassign="bilan_adm_date_fin",
					funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
					horodate=horodatefin,
					silent=silent)
			bilan_adm@limit1hm<-choice_c(bilan_adm@limit1hm,as.character(limit1hm))
			bilan_adm@limit2hm<-choice_c(bilan_adm@limit2hm,as.character(limit2hm))
			validObject(bilan_adm)
			return(bilan_adm)
		})

#' Calcule method for BilanAgedemer, this method will pass the data from long to wide format 
#' ( one line per individual) and calculate Durif silvering index and Pankhurst and Fulton's K.
#' 
#' @param object An object of class \code{\link{BilanAgedemer-class}} 
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("calcule",signature=signature("BilanAgedemer"),definition=function(object,silent) {
			#bilan_adm<-b_carlot
			bilan_adm<-object
			if(nrow(bilan_adm@data)==0) {
				funout("you are in deep shit", arret=TRUE)
			}   
			adm=bilan_adm@data # on recupere le data.frame
			if (is.na(as.numeric(bilan_adm@limit1hm@label))) stop("erreur interne")
			# if no value, a dummy value of 2m
			if (is.na(as.numeric(bilan_adm@limit2hm@label))) bilan_adm@limit2hm@label<-2000
			adm$agedemer<-NA
			lescoupes<-c(0,as.numeric(bilan_adm@limit1hm@label),as.numeric(bilan_adm@limit2hm@label),2001)
			adm$age<-cut(x=adm$car_valeur_quantitatif,breaks=lescoupes,labels=FALSE)
			bilan_adm@calcdata[["data"]]<-adm
			
			
			assign("bilan_adm",bilan_adm,envir_stacomi)
			return(bilan_adm)
		})


#' Plots of various type for BilanAgedemer
#' 
#' @param x An object of class \link{BilanAgedemer-class}
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
#' @aliases plot.BilanAgedemer plot.bilan_adm
#' @export
setMethod("plot", signature(x = "BilanAgedemer", y = "missing"), definition=function(x, plot.type="1", silent=FALSE){ 
			#bilan_adm<-b_carlot;require(ggplot2);plot.type="1"
			#browser()
			bilan_adm<-x
			plot.type<-as.character(plot.type)# to pass also characters
			if (!plot.type%in%c("1","2","3","4")) stop('plot.type must be 1,2,3 or 4')
			if (exists("bilan_adm",envir_stacomi)) {
				bilan_adm<-get("bilan_adm",envir_stacomi)
			} else {      
				if (!silent) funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
			}
			dat<-bilan_adm@calcdata[["data"]]
			# cols are using viridis::inferno(6,alpha=0.9)
			les_coupes=as.numeric(c(bilan_adm@limit1hm@label,bilan_adm@limit2hm@label))
		
			
			#################################################
			# plot.type =1 density plot
			#################################################
			
			if (plot.type=="1"){		
				
				p<-ggplot(dat)+geom_histogram(aes(x=car_valeur_quantitatif,fill=factor(age)),alpha=0.8)+
						geom_vline(xintercept=les_coupes,lty=2,lwd=1)+
						annotate("text",x=les_coupes,y=0,label=les_coupes,vjust=1)+
						theme_minimal()+
						scale_fill_manual("Age",values=c("1"="#379ec6","2"="#173957","3"="#b09953"))+
						xlab("Size in mm")+
						ylab("Effectif")
				print(p)
				assign("p",p,envir=envir_stacomi)
				funout("L'objet graphique est écrit dans l'environnement stacomi, tappez p<-get('p',envir=envir_stacomi))")
				
			}
			######################################
			# Migration according to stage, month and year
			######################################
			if (plot.type=="2"){	
				
				p<-ggplot(dat)+geom_histogram(aes(x=car_valeur_quantitatif,fill=factor(age)),alpha=0.8)+
						geom_vline(xintercept=les_coupes,lty=2,lwd=1)+
						theme_minimal()+
						scale_fill_manual("Age",values=c("1"="#379ec6","2"="#173957","3"="#b09953"))+
						xlab("Size in mm")+
						ylab("Effectif")+
						facet_grid(ope_dic_identifiant~.)
				print(p)
				assign("p",p,envir=envir_stacomi)
				funout("L'objet graphique est écrit dans l'environnement stacomi, tappez p<-get('p',envir=envir_stacomi))")
				
				
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
				#adms.legend = list(title = "DC", 
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
				#adms.legend = list(title = "DC", 
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
						xlab=get("msg",envir=envir_stacomi)$BilanAgedemer.9,
						ylab=get("msg",envir=envir_stacomi)$BilanAgedemer.10,
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

#' summary for BilanAgedemer 
#' @param object An object of class \code{\link{BilanAgedemer-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanAgedemer"),definition=function(object,silent=FALSE,...){
			bilan_adm<-object
			if (exists("bilan_adm",envir_stacomi)) {
				bilan_adm<-get("bilan_adm",envir_stacomi)
			} else {      
				if (!silent) funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
			}
			dat<-bilan_adm@calcdata
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
						bilan_adm@dc@data[bilan_adm@dc@data$dc==bilan_adm@dc@dc_selectionne[i],"ouv_libelle"]
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

#' Command line method to write the daily and monthly counts to the 
#' t_bilanmigrationjournalier_bjo table
#' 
#' Daily values are needed to compare migrations from year to year, by the class \link{BilanMigrationInterAnnuelle-class}. They are added by
#' by this function.  
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
#' @param silent : TRUE to avoid messages
#' @param dbname : the name of the database, defaults to "bd_contmig_nat"
#' @param host : the host for sqldf, defaults to "localhost"
#' @param port : the port, defaults to 5432
#' @note the user is asked whether or not he wants to overwrite data, if no
#' data are present in the database, the import is done anyway. The name of the database
#' is not passed in odbc link, here defaults to "bd_contmig_nat"
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("bM_Arzal")
#' bM_Arzal<-calcule(bM_Arzal)
#' write_database(bilanMigration=bM_Arzal,silent=FALSE)
#' }
#' @export
		setMethod("write_database",signature=signature("BilanAgedemer"),definition=function(object,silent=TRUE,dbname="bd_contmig_nat",host="localhost",port=5432){
					# dbname="bd_contmig_nat";host="localhost";silent=FALSE;port=5432
					bilanMigration<-object
					if (class(bilanMigration)!="BilanMigration") stop("the bilanMigration should be of class BilanMigration")
					if (class(silent)!="logical") stop("the silent argument should be a logical")
					dc=as.numeric(bilanMigration@dc@dc_selectionne)[1]
					data=bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
					data=data[data$Effectif_total!=0,]
					jour_dans_lannee_non_nuls=data$debut_pas	
					col_a_retirer=match(c("No.pas","type_de_quantite","debut_pas","fin_pas"),colnames(data))
					data=data[,-col_a_retirer]
					data$taux_d_echappement[data$taux_d_echappement==-1]<-NA 
					data$coe_valeur_coefficient[data$"coe_valeur_coefficient"==1]<-NA 
					peuventpaszero=match(c("taux_d_echappement","coe_valeur_coefficient"),colnames(data))
					data[,-peuventpaszero][data[,-peuventpaszero]==0]<-NA
					annee<-as.numeric(unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y"))[1])
					aat_bilanmigrationjournalier_bjo=cbind(
							bilanMigration@dc@dc_selectionne,
							bilanMigration@taxons@data$tax_code,
							bilanMigration@stades@data$std_code,
							annee, # une valeur
							rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")])),
							utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")]),  
							Sys.time(),
							substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
					)
					aat_bilanmigrationjournalier_bjo= stacomirtools::killfactor(aat_bilanmigrationjournalier_bjo[!is.na(aat_bilanmigrationjournalier_bjo$values),])
					colnames(aat_bilanmigrationjournalier_bjo)<-c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_valeur","bjo_labelquantite","bjo_horodateexport","bjo_org_code")
					
					#####
					# Ci dessous conversion de la classe vers migration Interannuelle pour utiliser
					# les methodes de cette classe
					bil=as(bilanMigration,"BilanMigrationInterAnnuelle")
					bil=connect(bil,silent=silent)
					
					hconfirm=function(h,...){			
						# suppression des donnees actuellement presentes dans la base
						# bilanjournalier et bilanmensuel
						supprime(bil)			
						baseODBC<-get("baseODBC",envir=envir_stacomi)
						sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
								"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
								" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
						invisible(utils::capture.output(
										sqldf::sqldf(x=sql,
												drv="PostgreSQL",
												user=baseODBC["uid"],
												dbname=dbname,				
												password=baseODBC["pwd"],
												host=host,
												port=port)
								))		
						
						
						if (!silent){
							funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,annee,"\n"))
						}
# si l'utilisateur accepte de remplacer les valeurs				
#progres<-get("progres",envir=envir_stacomi)
#gtkWidgetDestroy(progres)
# ecriture egalement du bilan mensuel
						taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
						stade= as.character(bilanMigration@stades@data$std_libelle)
						DC=as.numeric(bilanMigration@dc@dc_selectionne)	
						tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
						resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent )
						fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
					}#end function hconfirm
					
					if (nrow(bil@data)>0)
					{ 
						if (!silent){
							choice<-gWidgets::gconfirm(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.1, # Un bilan a deja ete ecrit dans la base
											unique(bil@data$bjo_horodateexport),
											get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.2),
									handler=hconfirm) # voulez vous le remplacer ?
						} else {
							hconfirm(h=NULL)
						}
						
					}
					else  # sinon on ecrit les resultats quoiqu'il arrive
					{
						
						baseODBC<-get("baseODBC",envir=envir_stacomi)
						sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
								"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
								" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
						invisible(utils::capture.output(
										sqldf::sqldf(x=sql,
												drv="PostgreSQL",
												user=baseODBC["uid"],
												dbname=dbname,				
												password=baseODBC["pwd"],
												host=host,
												port=port)
								))		
#	
						
						if (!silent) funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
						taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
						stade= as.character(bilanMigration@stades@data$std_libelle)
						DC=as.numeric(bilanMigration@dc@dc_selectionne)	
						tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
						resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent)
						fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
					} # end else
				})		
		
#' Method to print the command line of the object
#' @param x An object of class BilanAgedemer
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @export
setMethod("print",signature=signature("BilanAgedemer"),definition=function(x,...){ 
			sortie1<-"bilan_adm=new('BilanAgedemer')"
			sortie2<-stringr::str_c("bilan_adm=choice_c(bilan_adm,",
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


#' funplotBilanAgedemer 
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler, with action 1,2,3 or 4 
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funplotBilanAgedemer = function(h,...) {
	bilan_adm<-get(x="bilan_adm",envir=envir_stacomi)
	bilan_adm<-charge(bilan_adm)
	bilan_adm<-connect(bilan_adm)
	bilan_adm<-calcule(bilan_adm)
	#plot.type is determined by button in h$action
	the_plot<-plot(bilan_adm,plot.type=h$action)
	print(the_plot)
}


#' table function
#' 
#' funtableBilanAgedemer shows a table of results in gdf
#' @param h hanlder passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtableBilanAgedemer = function(h,...) {
	bilan_adm=charge(bilan_adm)
	bilan_adm<-connect(bilan_adm)
	vue_ope_lot=bilan_adm@requete@query # on recupere le data.frame
	assign("bilan_adm",bilan_adm,envir_stacomi)#assign("bilan_adm",vue_ope_lot,envir_stacomi)
	funout(get("msg",envir_stacomi)$BilanAgedemer.3)
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
	data(coef_Durif)
	stopifnot(colnames(data)==c("BL","W","Dv","Dh","FL"))
	data<-cbind(1,data[,c(1,2,5)],rowMeans(data[,c("Dv","Dh")],na.rm=TRUE))
	colnames(data)<-c("Constant","BL","W","FL","MD")
	data<-data[,c(1,2,3,5,4)]
	indices<-data%*%coef_Durif
	return(unlist(apply(indices,1,function(X)ifelse(is.na(X[1]),NA,names(which.max(X))))))
}

