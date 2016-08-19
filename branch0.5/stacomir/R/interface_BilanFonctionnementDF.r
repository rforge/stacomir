# Nom fichier :        interface_BilanFonctionnementDF.R    (classe)
#' interface for BilanFonctionnementDF class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDF = function()
{
    fonctionnementDF=new("BilanFonctionnementDF")
    assign("fonctionnementDF",fonctionnementDF,envir=.GlobalEnv)    
    funout(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.1)
    fonctionnementDF@df=charge(fonctionnementDF@df)    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte()
    assign("group",group,envir=.GlobalEnv)
    
   gWidgets::add(ggroupboutons,group)
    
    choix(fonctionnementDF@df)
	choix(fonctionnementDF@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="fonctionnementDF_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2)
	choix(fonctionnementDF@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="fonctionnementDF_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1)
	
    aBarchart=gWidgets::gaction(label="barchart",icon="barplot",handler=funbarchartDF,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)          
    aBox=gWidgets::gaction(label="boites",icon="graph2",handler=funboxDF,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.3)
    aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDF,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.4)
    aQuit=gWidgets::gaction(label="Quitter",icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.5)
    
    toolbarlist <- list(
    		barchart=aBarchart, 
    		box= aBox,
    		table=aTable,
    		Quit = aQuit)
    
    add(group, gmenu(toolbarlist))
    add(group,gbutton(text = "graph", handler = function(h,...){X11()})) 
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    # A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
    #assign("graphes",graphes,envir=.GlobalEnv)
	grDevice::X11()
}