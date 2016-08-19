# Nom fichier :        interface_Bilan_lot.R    (interface)

#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @export
interface_BilanLot = function()
{  
    bilan_lot=new("Bilan_lot")
    assign("bilan_lot",bilan_lot,envir = .GlobalEnv)
    
    funout(get("msg",envir=envir_stacomi)$interface_Bilan_lot.1)
    bilan_lot@dc=charge(bilan_lot@dc)
    bilan_lot@taxons=charge(bilan_lot@taxons)
    bilan_lot@stades=charge(bilan_lot@stades)
    bilan_lot@par=charge(bilan_lot@par)    
     
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte() # vidange de l'interface
    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_lot.2,container=group)
    # dans l'ordre 
    # dans le handler, modifier le contenu de l'objet fils si il existe
    # supprimer les widgets fils si ils existent (appel de la methode delete)
    # appeller la methode choice pour l'affichage du fils si il existe
    
    
    choice(bilan_lot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_lot_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
    choice(bilan_lot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_lot_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
    
    choice(bilan_lot@dc,objetBilan=bilan_lot,is.enabled=TRUE)
    # Les methodes choice suivantes sont passees en cascade � l'interieur des methodes choice
    #choice(bilan_lot@taxons,is.enabled=FALSE)
    #choice(bilan_lot@stades,is.enabled=FALSE)
    #choice(bilan_lot@par,is.enabled=FALSE)
    #toolbarlist$Calc$handler = connect(fonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())

	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.7 => dotplot ou graphe de dispersion
	aPoint=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7,
			icon="gWidgetsRGtk2-cloud",
			handler=funpointBilan_lot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.11 => density
	aDensity=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11,
			icon="gWidgetsRGtk2-density",
			handler=fundensityBilan_lot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.10 => boxplot
	aBoxplot=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10,
			icon="gWidgetsRGtk2-boxplot",
			handler=funboxplotBilan_lot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10)
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableBilan_lot,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.8)
    aQuit=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9)
    
    toolbarlist <- list(    
    		plot=aPoint,
			density=aDensity, 
			boxplot= aBoxplot,
    		table=aTable,
    		Quit = aQuit)
    ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
    gWidgets::add(ggroupboutons,ggroupboutonsbas)
    gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal       
    #assign("graphes",graphes,envir=.GlobalEnv) 
	grDevices::X11()
    # A cet endroit sinon ouvre plusieurs fenetres pour plusieurs choses
}
