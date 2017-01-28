# Nom fichier :        interface_BilanFonctionnementDC.R   


#' interface for BilanFonctionnementDC class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDC = function()
{
	quitte()
	bilanFonctionnementDC=new("BilanFonctionnementDC")
    assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
    
    funout(gettext("Loading of the list for fishways and choice of the time step\n"))
    bilanFonctionnementDC@dc=charge(bilanFonctionnementDC@dc)
    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group

    assign("group",group,envir = .GlobalEnv)
    
   gWidgets::add(ggroupboutons,group)
    choice(bilanFonctionnementDC@dc)
    choice(bilanFonctionnementDC@horodatedebut,
			label=gettext("Start"),
			nomassign="bilanFonctionnementDC_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n"),
			decal=-2)
    choice(bilanFonctionnementDC@horodatefin,
			label=gettext("End"),
			nomassign="bilanFonctionnementDC_date_fin",
			funoutlabel=gettext("Ending date has been chosen\n"),
			decal=-1)
    
    #toolbarlist$Calc$handler = connect(bilanFonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())
    
    aBarchart=gWidgets::gaction(label="barchart",
			icon="barplot",
			handler=funbarchartDC,
			tooltip=gettext("Monthly graph"))
	aBarchart1=gWidgets::gaction(label="barchart_fct",
			icon="barplot",
			handler=funbarchart1DC,
			tooltip=gettext("Another monthly graph"))	
	aBox=gWidgets::gaction(label=getext("box"),
			icon="graph2",
			handler=funboxDC,
			tooltip=gettext("Boxplot"))
    aTable=gWidgets::gaction(label=gettext("table"),
			icon="dataframe",
			handler=funtableDC,
			tooltip=gettext("Table"))
    aQuit=gWidgets::gaction(label="Quitter",
			icon="close", 
			handler=quitte,
			tooltip=gettext("Exit"))
	aOut=gWidgets::gaction(label=gettex("code"),
			handler=houtDC, 
			icon="gtk-info",
			tooltip=gettext("code"))    
	toolbarlist <- list(
    		barchart=aBarchart, 
			barchart1=aBarchart1,
    		box= aBox,
    		table=aTable,
			out=aOut,
    		Quit = aQuit)    
    add(group, gmenu(toolbarlist))
    gWidgets::addSpring(group)
}