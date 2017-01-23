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
    bilPM@liste=charge(object=bilPM@liste,listechoice=c("=1",">1","tous"),label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.5)
	# choice of number type
    group <- gWidgets::ggroup(horizontal=FALSE)   # must always be named group
    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.1,container=group)

    ### first toobar    
    gWidgets::addSpring(group)
    choice(bilPM@liste)
    choice(bilPM@dc,objectBilan=NULL,is.enabled=TRUE)
    choice(bilPM@anneedebut,
			nomassign="refAnneeDebut",
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.3)#annee debut
    choice(bilPM@anneefin,
			nomassign="refAnneeFin",
			titleFrame=get("msg",envir=envir_stacomi)$interface_BilanMigrationInterannuelle.5)#annee fin
	aCalc=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.2,icon="lines",handler=hcalc,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.2)#load
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableBilan_poids_moyen,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.3)
	aQuit=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.4,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_poids_moyen.4)
	toolbarlist <- list(barchart=aCalc,table=aTable,Quit = aQuit)
	add(group, gmenu(toolbarlist))
	
	### second toobar    
	aGra=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.18,action="1",icon="lines",handler=hplot)
	aCoe=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.19,icon="Coe",handler=hplot,action="2") 
	aSize=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.20,action="3",icon="gWidgetsRGtk2-bubbles",handler=hplot)         
	aReg=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.21,icon="gWidgetsRGtk2-function1",handler=hreg,action="reg")
	aExp=gWidgets::gaction(label=get("msg",envir_stacomi)$Bilan_poids_moyen.22,icon="gtk-harddisk",handler=hexp)         
	toolbarlistgraph <- gmenu(list(gra=aGra,coe=aCoe,size=aSize))
	assign("toolbarlistgraph",toolbarlistgraph,.GlobalEnv)
	toolbarlistgraph1<-gmenu(list(reg=aReg,exp=aExp))
	assign("toolbarlistgraph1",toolbarlistgraph1,.GlobalEnv)
	add(group,toolbarlistgraph)
	add(group,toolbarlistgraph1)  
}