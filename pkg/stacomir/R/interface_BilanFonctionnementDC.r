# Nom fichier :        interface_BilanFonctionnementDC.R   


#' interface for BilanFonctionnementDC class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDC = function()
{
	quitte()
	bilanFonctionnementDC=new("BilanFonctionnementDC")
    assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
    
    funout(gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.1))
    bilanFonctionnementDC@dc=charge(bilanFonctionnementDC@dc)
    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group

    assign("group",group,envir = .GlobalEnv)
    
   gWidgets::add(ggroupboutons,group)
    choice(bilanFonctionnementDC@dc)
    choice(bilanFonctionnementDC@horodatedebut,
			label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.3),
			nomassign="bilanFonctionnementDC_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n"),
			decal=-2)
    choice(bilanFonctionnementDC@horodatefin,
			label=gettext("End of timestamp"),
			nomassign="bilanFonctionnementDC_date_fin",
			funoutlabel=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.6),
			decal=-1)
    
    #toolbarlist$Calc$handler = connect(bilanFonctionnementDC)
    #toolbarlist$Calc$icon = "dataframe"
    #getStockIcons(toolkit=guiToolkit())
    
    aBarchart=gWidgets::gaction(label="barchart",icon="barplot",handler=funbarchartDC,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)         )
	aBarchart1=gWidgets::gaction(label="barchart_fct",icon="barplot",handler=funbarchart1DC,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)          )
	
	aBox=gWidgets::gaction(label="boites",icon="graph2",handler=funboxDC,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.3))
    aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDC,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.4))
    aQuit=gWidgets::gaction(label="Quitter",icon="close", handler=quitte,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.5))
	aOut=gWidgets::gaction(label="code",handler=houtDC, icon="gtk-info", tooltip=gettext(get("msg",envir=envir_stacomi)$BilanMigrationMult.1) )
    
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