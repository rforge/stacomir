# Nom fichier :        interface_Bilan_carlot.R    (interface)

#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanLot = function()
{  
    bilan_carlot=new("Bilan_carlot")
    assign("bilan_carlot",bilan_carlot,envir = .GlobalEnv)
    
    funout(get("msg",envir=envir_stacomi)$interface_Bilan_lot.1)
    bilan_carlot@dc=charge(bilan_carlot@dc)
    bilan_carlot@taxons=charge(bilan_carlot@taxons)
    bilan_carlot@stades=charge(bilan_carlot@stades)
    bilan_carlot@par=charge(bilan_carlot@par)    
     
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    quitte() # vidange de l'interface
    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_lot.2,container=group)
    # dans l'ordre 
    # dans le handler, modifier le contenu de l'object fils si il existe
    # supprimer les widgets fils si ils existent (appel de la methode delete)
    # appeller la methode choice pour l'affichage du fils si il existe
    
    
    choice(bilan_carlot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_carlot_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
    choice(bilan_carlot@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_carlot_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
    
    choice(bilan_carlot@dc,objectBilan=bilan_carlot,is.enabled=TRUE)
    # Les methodes choice suivantes sont passees en cascade e l'interieur des methodes choice
    #choice(bilan_carlot@taxons,is.enabled=FALSE)
    #choice(bilan_carlot@stades,is.enabled=FALSE)
    #choice(bilan_carlot@par,is.enabled=FALSE)
    #toolbarlist$Calc$handler = connect(fonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())

	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.7 => dotplot ou graphe de dispersion
	aPoint=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7,
			icon="gWidgetsRGtk2-cloud",
			handler=funpointBilan_carlot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.11 => density
	aDensity=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11,
			icon="gWidgetsRGtk2-density",
			handler=fundensityBilan_carlot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.10 => boxplot
	aBoxplot=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10,
			icon="gWidgetsRGtk2-boxplot",
			handler=funboxplotBilan_carlot,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10)
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableBilan_carlot,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_carlot.8)
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
