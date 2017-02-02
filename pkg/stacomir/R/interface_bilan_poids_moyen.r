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
    bilPM@liste=charge(object=bilPM@liste,listechoice=c("=1",">1",gettext("all",domain="R-stacomiR")),label=gettext("choice of number in sample (one, several, both)",domain="R-stacomiR"))
	# choice of number type
    group <- gWidgets::ggroup(horizontal=FALSE)   # must always be named group
    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    gl=glabel(text=gettext("Mean weight report",domain="R-stacomiR"),container=group)

    ### first toobar    
    gWidgets::addSpring(group)
    choice(bilPM@liste)
    choice(bilPM@dc,objectBilan=NULL,is.enabled=TRUE)
    choice(bilPM@anneedebut,
			nomassign="refAnneeDebut",
			titleFrame=gettext("Beginning year",domain="R-stacomiR")) #annee debut
    choice(bilPM@anneefin,
			nomassign="refAnneeFin",
			titleFrame=gettext("Ending year",domain="R-stacomiR"))#annee fin
	aCalc=gWidgets::gaction(label=gettext("load",domain="R-stacomiR"),
			icon="lines",
			handler=hcalc,
			tooltip=gettext("load",domain="R-stacomiR")) 
	aTable=gWidgets::gaction(label="table",
			icon="dataframe",
			handler=funtableBilan_poids_moyen,
			tooltip=gettext("table",domain="R-stacomiR"))
	aQuit=gWidgets::gaction(
			label=gettext("exit",domain="R-stacomiR"),
			icon="close", 
			handler=quitte,
			tooltip=gettext("exit",domain="R-stacomiR"))
	toolbarlist <- list(barchart=aCalc,table=aTable,Quit = aQuit)
	add(group, gmenu(toolbarlist))
	
	### second toobar    
	aGra=gWidgets::gaction(label=gettext("Gra",domain="R-stacomiR"),icon="lines",handler=hplot,action="1",
			tooltip=gettext("plot.type='1', plot of mean weight of glass eel against the mean date of operation",domain="R-stacomiR"))
	aCoe=gWidgets::gaction(label=gettext("Coe",domain="R-stacomiR"),icon="newplot",handler=hplot,action="2",
			tooltip=gettext("plot.type=2, standard plot of current coefficent",domain="R-stacomiR"))
	aSize=gWidgets::gaction(label=gettext("Leng",domain="R-stacomiR"),icon="gWidgetsRGtk2-bubbles",handler=hplot,action="3",
			tooltip=gettext("plot.type=3, same as 1 but size of the bubble according to number",domain="R-stacomiR"))         
	aReg=gWidgets::gaction(label=gettext("seasonal",domain="R-stacomiR"),icon="function",handler=hreg,action="seasonal",
			tooltip=gettext("model.type='seasonal', sine wave curve for a cyclic variation fitted with nls",domain="R-stacomiR"))        
	aReg1=gWidgets::gaction(label=gettext("seasonal1",domain="R-stacomiR"),icon="function1",handler=hreg,action="seasonal1",
			tooltip=gettext("model.type='seasonal1', long term variation along with seasonal variation fitted with gam",domain="R-stacomiR"))        
	aReg2=gWidgets::gaction(label=gettext("seasonal2",domain="R-stacomiR"),icon="function",handler=hreg,action="seasonal2",
			tooltip=gettext("model.type='seasonal2', long term variation + seasonal component fitted with sine curve",domain="R-stacomiR"))
	aExp=gWidgets::gaction(label=gettext("export",domain="R-stacomiR"),icon="gtk-harddisk",handler=hexp)    
	toolbarlistgraph <- gmenu(list(gra=aGra,coe=aCoe,size=aSize))
	assign("toolbarlistgraph",toolbarlistgraph,.GlobalEnv)
	toolbarlistgraph1<-gmenu(list(reg=aReg,reg1=aReg1,reg2=aReg2,exp=aExp))
	assign("toolbarlistgraph1",toolbarlistgraph1,.GlobalEnv)
	add(group,toolbarlistgraph)
	add(group,toolbarlistgraph1)  
}