# Nom fichier :        interface_BilanFonctionnementDC.R   


#' interface for BilanFonctionnementDC class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDC = function()
{
    fonctionnementDC=new("BilanFonctionnementDC")
    assign("fonctionnementDC",fonctionnementDC,envir=.GlobalEnv)
    
    funout(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.1)
    fonctionnementDC@dc=charge(fonctionnementDC@dc)
    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte()
    assign("group",group,envir = .GlobalEnv)
    
   gWidgets::add(ggroupboutons,group)
    choix(fonctionnementDC@dc)
    choix(fonctionnementDC@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="fonctionnementDC_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2)
    choix(fonctionnementDC@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="fonctionnementDC_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1)
    
    #toolbarlist$Calc$handler = connect(fonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())
    
    aBarchart=gWidgets::gaction(label="barchart",icon="barplot",handler=funbarchartDC,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)         
    aBox=gWidgets::gaction(label="boites",icon="graph2",handler=funboxDC,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.3)
    aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDC,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.4)
    aQuit=gWidgets::gaction(label="Quitter",icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.5)
    
    toolbarlist <- list(
    		barchart=aBarchart, 
    		box= aBox,
    		table=aTable,
    		Quit = aQuit)
    
    add(group, gmenu(toolbarlist))
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    # A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
    #assign("graphes",graphes,envir=.GlobalEnv)
	grDevice::X11()
}