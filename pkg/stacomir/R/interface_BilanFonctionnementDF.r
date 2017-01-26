#' interface for BilanFonctionnementDF class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDF = function()
{
	quitte()
	
	bilanFonctionnementDF=new("BilanFonctionnementDF")
	assign("bilanFonctionnementDF",bilanFonctionnementDF,envir=envir_stacomi)    
	funout(gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.1))
	bilanFonctionnementDF@df=charge(bilanFonctionnementDF@df)    
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir=.GlobalEnv)    
	gWidgets::add(ggroupboutons,group)
	
	choice(bilanFonctionnementDF@df)
	# here decale =-1 or -2 will make the bilan for the year preceeding the current date
	choice(bilanFonctionnementDF@horodatedebut,
			label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.3),
			nomassign="bilanFonctionnementDF_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n"),
			decal=-2)
	choice(bilanFonctionnementDF@horodatefin,
			label=gettext("End of timestamp"),
			nomassign="bilanFonctionnementDF_date_fin",
			funoutlabel=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.6),
			decal=-1)
	
	aBarchart=gWidgets::gaction(label="barchart_typefct",icon="barplot",handler=funbarchartDF,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)          )
	aBarchart1=gWidgets::gaction(label="barchart_fct",icon="barplot",handler=funbarchart1DF,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.2)          )
	aBox=gWidgets::gaction(label="box",icon="graph2",handler=funboxDF,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.3))
	aChart=gWidgets::gaction(label="chart",icon="graph",handler=funchartDF,tooltip="Calendar")
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDF,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.4))
	aOut=gWidgets::gaction(label="code",handler=houtDF, icon="gtk-info", tooltip=gettext(get("msg",envir=envir_stacomi)$BilanMigrationMult.1)    )
	aQuit=gWidgets::gaction(label="Close",icon="close", handler=quitte,tooltip=gettext(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.5))
	
	toolbarlist <- list(
			barchart=aBarchart, 
			barchart1=aBarchart1,
			box= aBox,
			chart=aChart,
			table=aTable,
			out=aOut,
			Quit = aQuit)
	
	add(group, gmenu(toolbarlist))
	gWidgets::addSpring(group)
}