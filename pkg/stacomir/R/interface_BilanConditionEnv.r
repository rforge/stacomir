#' Interface for class conditionEnv
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_ConditionEnv = function()
{
	quitte()
	bilanConditionEnv=new("BilanConditionEnv")
	funout(gettext("Loading of the monitoring stations\n",domain="R-stacomiR"))
	bilanConditionEnv@stationMesure=charge(bilanConditionEnv@stationMesure)
	assign("bilanConditionEnv",bilanConditionEnv,envir=envir_stacomi)
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = envir_stacomi)
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	gWidgets::add(ggroupboutons,group)
	choice(bilanConditionEnv@horodatedebut,label=gettext("Begginning",domain="R-stacomiR"),
			nomassign="bilanConditionEnv_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
			decal=-2)
	choice(bilanConditionEnv@horodatefin,
			label=gettext("End",domain="R-stacomiR"),
			nomassign="bilanConditionEnv_date_fin",
			funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
			decal=-1)
	choice(bilanConditionEnv@stationMesure)
	
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
	
	toolbarlist = list(
			Graph=gWidgets::gaction(handler=hbilanConditionEnvgraph , icon = "graph",label="graph",tooltip=gettext("Summary graphic",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("Exit",domain="R-stacomiR"),domain="R-stacomiR"))
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	dev.new()
}