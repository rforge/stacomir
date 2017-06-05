#' interface for BilanMigrationPar class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanMigrationPar = function()
{
	quitte()
	
	bmC=new("BilanMigrationCar")
	assign("bmC",bmC,envir=envir_stacomi)
	
	bilanFonctionnementDC=new("BilanFonctionnementDC") # appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
	assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
	
	objectBilan="bmC"
	# the following name is created by the interface
	# as I can't get the name from within the function (deparse(substitute(objectBilan)) does not return
	# "bilanMigrationMult" see refDC choice_c method)
	# so this will allow to assign "bilanMigrationMult" in envir_stacomi while using other class
	# like refDC
	assign("objectBilan",objectBilan,envir=envir_stacomi)
	funout(gettext("Loading of the lists for taxons, stages, counting devices, qualitative and quantitative parameters\n",domain="R-stacomiR"))
	bmC@taxons=charge(bmC@taxons)
	bmC@stades=charge(bmC@stades)
	bmC@dc=charge(bmC@dc)
	bmC@parquan=charge(bmC@parquan)
	bmC@parqual=charge(bmC@parqual)
	# below, the first element must be the element where samples are accepted (currently with)
	# this is how it will be evaluated in the connect method, as I can't base myself on the value
	# which will change with language
	bmC@echantillon=charge(bmC@echantillon,
			vecteur=gettext("with","without",domain="R-stacomiR"),
			label=gettext("Choice of batch type, inclusion of samples ?",domain="R-stacomiR"), 
					selected=as.integer(1))
	#######################
	# Interface Graphique 
	##########################
	group <- gWidgets::ggroup(horizontal=TRUE)   # doit toujours s'appeller group	
	assign("group",group,envir = envir_stacomi)
	notebook <- gnotebook(container=group)	
	assign("notebook",notebook,envir=envir_stacomi)
	size(notebook)<-c(400,600)
		

	choicemult(bmC@horodatedebut,nomassign="bmC_date_debut",label=gettext("from",domain="R-stacomiR"),decal=-1)
	choicemult(bmC@horodatefin,,nomassign="bmC_date_fin",label=gettext("to",domain="R-stacomiR"),decal=0)
	choicemult(bmC@echantillon)
	choicemult(bmC@dc,objectBilan=bmC,is.enabled=TRUE)
# FIXME Error in .local(object, ...) : 
#  unused arguments (label = "Qualitative feature", frameassign = "frame_parqual") verify
	svalue(notebook)<-1	
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	gWidgets::add(ggroupboutons,group)
	# ggroupboutons is attached to the original frame
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
	toolbarlist = list(
			Calc=gWidgets::gaction(handler = hbmCcalc,
					icon = "new",
					label=gettext("calculation"),
					tooltip=gettext("calculation",domain="R-stacomiR")),
			Graph=gWidgets::gaction(handler = hbmCplotquan,
					icon = "graph",
					label="gr quan",
					tooltip=gettext("Plot for qualitative parm",domain="R-stacomiR")),
			Graph2=gWidgets::gaction(handler = hbmCplotqual,
					icon = "graph2",
					label="gr qual",
					tooltip=gettext("plot for quantitative parm",domain="R-stacomiR")),
			Graph3=gWidgets::gaction(handler = hbmCplotcrossed,
					icon = "graph2",
					label="gr crossed",
					tooltip=gettext("Crossed graph for qualitative and quantitative parameter",domain="R-stacomiR")),
			Stat =gWidgets::gaction(handler= hbmCstat,
					icon = "matrix",
					label="stat",
					tooltip=gettext("Summary",domain="R-stacomiR")),
			annuler=gWidgets::gaction(handler= quitte,
					icon = "close",
					label=gettext("Exit",domain="R-stacomiR")))
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
	assign("bmC",bmC,envir=envir_stacomi)
}