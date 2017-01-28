#' interface for BilanFonctionnementDF class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanFonctionnementDF = function()
{
	quitte()
	
	bilanFonctionnementDF=new("BilanFonctionnementDF")
	assign("bilanFonctionnementDF",bilanFonctionnementDF,envir=envir_stacomi)    
	funout(gettext("Loading of the list for fishways and choice of the time step\n"))
	bilanFonctionnementDF@df=charge(bilanFonctionnementDF@df)    
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir=.GlobalEnv)    
	gWidgets::add(ggroupboutons,group)
	
	choice(bilanFonctionnementDF@df)
	# here decale =-1 or -2 will make the bilan for the year preceeding the current date
	choice(bilanFonctionnementDF@horodatedebut,
			label=gettext("start"),
			nomassign="bilanFonctionnementDF_date_debut",
			funoutlabel=gettext("The beginning date has been chosen\n"),
			decal=-2)
	choice(bilanFonctionnementDF@horodatefin,
			label=gettext("End"),
			nomassign="bilanFonctionnementDF_date_fin",
			funoutlabel=gettext("The ending date has been chosen\n"),
			decal=-1)
	
	aBarchart=gWidgets::gaction(label="barchart_typefct",icon="barplot",handler=funbarchartDF,tooltip=gettext("Monthly graph"))
	aBarchart1=gWidgets::gaction(label="barchart_fct",icon="barplot",handler=funbarchart1DF,tooltip=gettext("Monthly graph"))
	aBox=gWidgets::gaction(label="box",icon="graph2",handler=funboxDF,tooltip=gettext("Boxplot"))
	aChart=gWidgets::gaction(label="chart",icon="graph",handler=funchartDF,tooltip=gettext("Calendar"))
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDF,tooltip=gettext("Table"))
	aOut=gWidgets::gaction(label="code",handler=houtDF, icon="gtk-info", tooltip=gettext("Code"))
	aQuit=gWidgets::gaction(label="Close",icon="close", handler=quitte,tooltip=gettext("Exit"))
	
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