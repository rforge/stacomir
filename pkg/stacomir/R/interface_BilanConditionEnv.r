# Nom fichier :        interface_BilanConditionEnv    (classe)

#' Interface for class conditionEnv
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_ConditionEnv = function()
{
	quitte()
	bilanConditionEnv=new("BilanConditionEnv")
    funout(gettext("Loading of the monitoring stations\n"))
    bilanConditionEnv@stationMesure=charge(bilanConditionEnv@stationMesure)
    assign("bilanConditionEnv",bilanConditionEnv,envir=envir_stacomi)
    
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group

    assign("group",group,envir = .GlobalEnv)
   gWidgets::add(ggroupboutons,group)
    
    # date de debut et de fin
    choice(bilanConditionEnv@horodate,label=gettext("Begginning"),
			nomassign="bilanConditionEnv_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n"),
			decal=-2,
			affichecal=FALSE)
    choice(bilanConditionEnv@horodate,
			label=gettext("End"),
			nomassign="bilanConditionEnv_date_fin",
			funoutlabel=gettext("Ending date has been chosen\n"),
			decal=-1,
			affichecal=FALSE)
    choice(bilanConditionEnv@stationMesure)
    
    ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
    gWidgets::add(ggroupboutons,ggroupboutonsbas)
    assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
    
    toolbarlist = list(
    #Calc=gWidgets::gaction(handler=hbilanConditionEnvcalc , action=bilanConditionEnv,icon = "new",label="calcul",tooltip="calcul des conditions environnementales entre deux dates"),
    Graph=gWidgets::gaction(handler=hbilanConditionEnvgraph , icon = "graph",label="graph",tooltip=gettext("Summary graphic")),
    Stat =gWidgets::gaction(handler=hbilanConditionEnvstat , icon = "matrix",label="stat",tooltip=gettext("Summary tables in .csv")),
    annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("Exit")))
    gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
    gWidgets::addSpring(group)
    #graphes=ggraphics(width=600,height=400)
    #add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
    #assign("graphes",graphes,envir=envir_stacomi)
	dev.new()
}