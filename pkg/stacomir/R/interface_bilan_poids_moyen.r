#' interface for Bilan_poids_moyen class 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanPoidsMoyen = function()
{
    quitte()  # erase the interface
	bilPM=new("Bilan_poids_moyen")
    assign("bilan_poids_moyen",bilPM,envir = envir_stacomi)
    bilPM@dc=charge(bilPM@dc)
    bilPM@anneedebut=charge(bilPM@anneedebut)
    bilPM@anneefin=charge(bilPM@anneefin)
    bilPM@liste=charge(object=bilPM@liste,listechoice=c("=1",">1",gettext("tous")),label=gettext("choice of number in sample (one, several, all"))
	# choice of number type
    group <- gWidgets::ggroup(horizontal=FALSE)   # must always be named group
    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    gl=glabel(text=gettext("Mean weight report"),container=group)

    ### first toobar    
    gWidgets::addSpring(group)
    choice(bilPM@liste)
    choice(bilPM@dc,objectBilan=NULL,is.enabled=TRUE)
    choice(bilPM@anneedebut,
			nomassign="refAnneeDebut",
			titleFrame=gettext("Beginning year")) #annee debut
    choice(bilPM@anneefin,
			nomassign="refAnneeFin",
			titleFrame=gettext("Ending year"))#annee fin
	aCalc=gWidgets::gaction(label=gettext("load"),
			icon="lines",
			handler=hcalc,
			tooltip=gettext("load")) 
	aTable=gWidgets::gaction(label="table",
			icon="dataframe",
			handler=funtableBilan_poids_moyen,
			tooltip=gettext("table"))
	aQuit=gWidgets::gaction(
			label=gettext("exit"),
			icon="close", 
			handler=quitte,
			tooltip=gettext("exit"))
	toolbarlist <- list(barchart=aCalc,table=aTable,Quit = aQuit)
	add(group, gmenu(toolbarlist))
	
	### second toobar    
	aGra=gWidgets::gaction(label=gettext("Gra"),action="1",icon="lines",handler=hplot)
	aCoe=gWidgets::gaction(label=gettext("Coe"),icon="Coe",handler=hplot,action="2")
	aSize=gWidgets::gaction(label=gettext("Leng"),action="3",icon="gWidgetsRGtk2-bubbles",handler=hplot)         
	aReg=gWidgets::gaction(label=gettext("Reg"),icon="gWidgetsRGtk2-function1",handler=hreg,action="reg")
	aExp=gWidgets::gaction(label=gettext("export"),icon="gtk-harddisk",handler=hexp)    
	toolbarlistgraph <- gmenu(list(gra=aGra,coe=aCoe,size=aSize))
	assign("toolbarlistgraph",toolbarlistgraph,.GlobalEnv)
	toolbarlistgraph1<-gmenu(list(reg=aReg,exp=aExp))
	assign("toolbarlistgraph1",toolbarlistgraph1,.GlobalEnv)
	add(group,toolbarlistgraph)
	add(group,toolbarlistgraph1)  
}