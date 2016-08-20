#' handler function used by the main interface
#' 
#' 
#' @param h A handler
#' @param ... Other parameters
#' @aliases hDC,hOPE,hDFDC,hBilanMigration,hBilanMigrationInterannuelle,hBilanMigrationConditionEnv, hBilanMigrationPar, hBilanConditionEnv, hBilanLots, hTail, hpds, hSt, htodo,  hhelp, h0, hX11 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hDF=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.1,wash=TRUE)
	eval(interface_BilanFonctionnementDF(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hDC=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.2,wash=TRUE)
	eval(interface_BilanFonctionnementDC(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hOPE=function(h,...){
	# TODO a developper
	funout(text=get("msg",envir_stacomi)$interface_graphique.3,wash=TRUE)
}
#' handler function used by the main interface
hDFDC=function(h,...){
	# TODO developper cette fonction
	funout(get("msg",envir_stacomi)$interface_graphique.4,wash=TRUE)
}
#' handler function used by the main interface
hBilanMigration=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.5,wash=TRUE)
	eval(interface_BilanMigration(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanMigrationMult=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.14,wash=TRUE)
	eval(interface_BilanMigrationMult(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanMigrationInterAnnuelle=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.6,wash=TRUE)
	eval(interface_BilanMigrationInterAnnuelle(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanMigrationConditionEnv=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.7,wash=TRUE)
	eval(interface_BilanMigrationConditionEnv(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanMigrationPar=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.8,wash=TRUE)
	eval(interface_BilanMigrationPar(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanConditionEnv=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.9,wash=TRUE)
	eval(interface_ConditionEnv(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hBilanLots=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.10,wash=TRUE)
	eval(interface_BilanLot(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hTail=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.11,wash=TRUE)
	eval(interface_BilanTaille(),envir = .GlobalEnv)
}
#' handler function used by the main interface
hpds=function(h,...){
	eval(interface_BilanPoidsMoyen(),envir = .GlobalEnv)
	funout(get("msg",envir_stacomi)$interface_graphique.12,wash=TRUE) 
}
#' handler function used by the main interface
hSt=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.13,wash=TRUE)
	eval(interface_Bilan_stades_pigm(),envir = .GlobalEnv)
}
#' handler function used by the main interface
htodo=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.17,wash=TRUE)
}
#' handler function used by the main interface
hBilanEspeces=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.19,wash=TRUE)
	eval(interface_BilanEspeces(),envir = .GlobalEnv)
}
#' this handler test the connection and if it works loads the stacomi interface
#' @note gr_interface is copied by stacomi into envir_stacomi.
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}

husr=function(h,...){
	baseODBC<-get("baseODBC",envir=envir_stacomi)
	gr_interface<-get("gr_interface",envir_stacomi) # logical true or false
	# test de la connection
	con=new("ConnectionODBC")
	if (gr_interface){
		baseODBC[2]<-svalue(usrname)
		baseODBC[3]<-svalue(usrpwd)
	} else {
		# on prend les valeurs choisies par defaut dans baseODBC
		# rien
	}
	assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
	assign("baseODBC",baseODBC,envir=envir_stacomi)
	con@baseODBC=get("baseODBC",envir=envir_stacomi)
	e=expression(con<-stacomirtools::connect(con))
	con=tryCatch(eval(e),error=get("msg",envir=envir_stacomi)$interface_graphique_log.7) #finally=odbcClose(con@connection)clause inutile car si ï¿½a plante la connection n'est pas ouverte
	test<-con@etat==get("msg",envir=envir_stacomi)$ConnectionODBC.6
	if (exists("logw")) dispose(logw)
	odbcCloseAll()
	# if the test is OK launches the stacomi interface
	# function handler called by gmessage
	hgmessage=function(h,...){
		stacomi(gr_interface=TRUE)
		# en cas d'erreur on relance une demande de mot de passe
	}
	if (test) { # il existe un lien ODBC mais qui pointe peut ï¿½tre ailleurs
		requete=new("RequeteODBC")
		requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
		requete@sql="select count(*) from ref.tr_taxon_tax"
		requete<-stacomirtools::connect(requete)
		if (nrow(requete@query)==0){
			# le lien ODBC fonctionne mais pointe vers la mauvaise base
			gWidgets::gmessage(message=paste(get("msg",envir=envir_stacomi)$interface_graphique_log.8,
							"\n",
							get("msg",envir=envir_stacomi)$interface_graphique_log.9,
							" :",
							baseODBC[1],
							"\n",
							get("msg",envir=envir_stacomi)$interface_graphique_log.2,
							" :",
							baseODBC[2],
							"\n",
							get("msg",envir=envir_stacomi)$interface_graphique_log.3,
							" :",
							baseODBC[3]),						
					title=get("msg",envir=envir_stacomi)$interface_graphique_log.5,
					icon = "error",
					handler=hgmessage)		
		} else {
			# l'utilisateur peut avoir choisi une autre base que celle qui est dans le fichier xml
			assign("baseODBC",baseODBC,envir=envir_stacomi)
			gr_interface<-get("gr_interface",envir=envir_stacomi)
			if (gr_interface){
				interface_graphique()
			}
		}
	} else {
		gWidgets::gmessage(message=paste(get("msg",envir=envir_stacomi)$interface_graphique_log.6,
						"\n",
						get("msg",envir=envir_stacomi)$interface_graphique_log.9,
						" :",
						baseODBC[1],
						"\n",
						get("msg",envir=envir_stacomi)$interface_graphique_log.2,
						" :",
						baseODBC[2],
						"\n",
						get("msg",envir=envir_stacomi)$interface_graphique_log.3,
						" :",
						baseODBC[3]),						
				title=get("msg",envir=envir_stacomi)$interface_graphique_log.5,
				icon = "error",
				handler=hgmessage)
	}	
}
hhelp=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.14,wash=TRUE)
}
hlang=function(h,...){
	eval(interface_chooselang(),envir = .GlobalEnv)
}
hX11=function(h,...){
	grDevices::X11()
}





#' Function that loads the loginwindow, tests connection, and then destroys the
#' window
#' 
#' Function that loads the loginwindow, tests connection, and then destroys the
#' window
#' 
#' @param gr_interface Will be used to launch the program as graphical
#' interface or in command line
#' @import stringr
#' @import RColorBrewer
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import ggplot2
#' @import RPostgreSQL
#' @import sqldf
#' @import methods
#' @import stacomirtools
#' @import RODBC
#' @importFrom intervals Intervals
#' @importFrom intervals closed<-
#' @importFrom intervals interval_overlap
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid grid.newpage
#' @importFrom grid grid.layout
#' @importFrom utils winProgressBar
#' @importFrom utils setWinProgressBar
#' @importFrom utils read.csv
#' @importFrom utils stack
#' @importFrom utils globalVariables
#' @importFrom utils select.list write.table 
#' @importFrom stats ftable
#' @importFrom stats xtabs
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom lattice barchart trellis.par.get trellis.par.set simpleKey
#' @importFrom grid gpar
#' @importFrom graphics layout matplot mtext points polygon segments par axis text legend rect axis.Date
#' @importFrom stats as.formula coef na.fail nls pbeta predict sd
#' @importFrom grDevices X11 X11 gray rainbow
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
stacomi=function(gr_interface=TRUE){
	# first loading of connection and odbc info using chargexml()
	assign("gr_interface",gr_interface,envir=envir_stacomi)
	# the first messages are necessary for the first access to the database, they are in French
	msg<-messages()
	mylinks=chargecsv()
	baseODBC=mylinks[["baseODBC"]]
	datawd=mylinks[["datawd"]]
	lang=mylinks[["lang"]]	
	sqldf.options=mylinks[["sqldf.options"]]	
	assign("lang",lang,envir=envir_stacomi)	
	assign("baseODBC",baseODBC,envir=envir_stacomi)
	assign("datawd",datawd,envir=envir_stacomi)
	assign("sqldf.options",sqldf.options,envir=envir_stacomi)
	refMsg=new("RefMsg")
	createmessage(refMsg)
	
	msg=get("msg",envir=envir_stacomi)
	#libraries()
	options(sqldf.RPostgreSQL.user = sqldf.options["sqldf.uid"], 
			sqldf.RPostgreSQL.password =sqldf.options["sqldf.pwd"],
			sqldf.RPostgreSQL.dbname = sqldf.options["sqldf.dbname"],
			sqldf.RPostgreSQL.host = sqldf.options["sqldf.host"],#  1.100.1.6
			sqldf.RPostgreSQL.port = sqldf.options["sqldf.port"])
	# loginWindow, will call the husr handler
	if (gr_interface){
		logw <- gWidgets::gwindow(msg$interface_graphique_log.1, 
				name="log",
				parent=c(0,0),
				width=100,height=100)
		assign("logw",logw,envir=.GlobalEnv)
		logly=gWidgets::glayout(container=logw)
		usrname<- gWidgets::gedit(text = baseODBC[2], 
				width = 10, 
				container = logly)
		assign("usrname",usrname,.GlobalEnv)
		usrpwd<- gWidgets::gedit(text = baseODBC[3], 
				width = 10, 
				container = logly)
		assign("usrpwd",usrpwd,.GlobalEnv)
		but=gWidgets::gbutton(text =  msg$interface_graphique_log.4,
				border=TRUE, 
				handler = husr, 
				container = logly)
		logly[1,1]<-msg$interface_graphique_log.2
		logly[2,1]<-msg$interface_graphique_log.3
		logly[1,2]<-usrname
		logly[2,2]<-usrpwd
		logly[3,2]<-but
	} else {
		
		
		husr(gr_interface=FALSE)
	}
}


#' Program launch, this function first gathers the ODBC path from the csv file
#' 
#' Program launch, this function fist gathers the ODBC path and working
#' directory from the csv file and then launches the GwidgetRgtk graphical
#' interface to stacomi.
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_graphique=function(){
	msg=get("msg",envir=envir_stacomi) # appel dans chaque sous fonction
	if (exists("graphes"))  rm(list=c("graphes"),envir=.GlobalEnv)
	if (exists("ggroupboutonsbas"))  rm(list=c("ggroupboutonsbas"),envir=.GlobalEnv) 
	if (exists("group"))  rm(list=c("group"),envir=.GlobalEnv)
	if (!file.exists(path.expand(get("datawd",envir=envir_stacomi)))) {
		dir.create(path.expand(get("datawd",envir=envir_stacomi)))
	}
	
	col.sortie=rep(c("pink","purple","red","orange","green","blue","cyan","magenta"),20) # couleurs pour le texte
	assign("col.sortie",col.sortie,.GlobalEnv)
	nbligne=0
	assign("nbligne",nbligne,.GlobalEnv)
	
	win <- gWidgets::gwindow(msg$interface_graphique.16, name="main",parent=c(0,0),width=100,height=100)
	assign("win",win,envir=.GlobalEnv)
	
	## Menubar is defined by a list
	menubarlist = list()
	
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.1]]$handler =hDF
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.1]]$icon="gWidgetsRGtk2-rarrow"
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.2]]$handler =hDC
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.2]]$icon = "gtk-media-record"
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.3]]$handler=hOPE
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.3]]$icon="gtk-cancel"#"gtk-go-forward"
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.4]]$handler=hDFDC
	menubarlist[[msg$interface_graphique_menu.1]][[msg$interface_graphique_menu.1.4]]$icon="gtk-cancel"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.1]]$handler=hBilanMigration
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.1]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.2]]$handler=hBilanConditionEnv
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.2]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.3]]$handler=hBilanMigrationConditionEnv
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.3]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.4]]$handler=hBilanMigrationPar
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.4]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.5]]$handler=hBilanMigrationInterAnnuelle
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.5]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.6]]$handler=hBilanLots
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.6]]$icon="gWidgetsRGtk2-newplot"#"gWidgetsRGtk2-logical"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.7]]$handler=hpds
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.7]]$icon="gWidgetsRGtk2-evaluate"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.8]]$handler=hTail
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.8]]$icon="gWidgetsRGtk2-evaluate"#"gWidgetsRGtk2-boxplot"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.13]]$icon="gWidgetsRGtk2-curve"#"gWidgetsRGtk2-boxplot"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.13]]$handler=hBilanEspeces	
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.9]]$handler=hSt
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.9]]$icon="gWidgetsRGtk2-contour"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.10]]$handler=htodo
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.10]]$icon="gtk-cancel"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.11]]$handler=htodo
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.11]]$icon="gtk-cancel"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.12]]$handler=htodo
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.12]]$icon="gtk-cancel"
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.14]]$handler=hBilanMigrationMult
	menubarlist[[msg$interface_graphique_menu.2]][[msg$interface_graphique_menu.2.14]]$icon="gWidgetsRGtk2-curve"
	menubarlist[[msg$interface_graphique_menu.3]]$About$handler = hX11
	menubarlist[[msg$interface_graphique_menu.3]]$About$icon="newplot"
	menubarlist[[msg$interface_graphique_menu.3]]$About$handler = hhelp
	menubarlist[[msg$interface_graphique_menu.3]]$About$icon="dialog-info"
	menubarlist[[msg$interface_graphique_menu.3]]$lang$handler = hlang
	menubarlist[[msg$interface_graphique_menu.3]]$lang$icon="dialog-info"
	gWidgets::add(win, gmenu(menubarlist))
	ggrouptotal<- gWidgets::ggroup(horizontal=FALSE)         # celui ci empile les autres de haut en bas
	# gsortie est au dessus de la fenêtre
	assign("ggrouptotal",ggrouptotal,envir=.GlobalEnv) 
	
	gWidgets::add(win,ggrouptotal)
	
	gSortie=gWidgets::gtext(msg$interface_graphique.18,width =100 , height = 100,font.attr=list(style="italic", col="blue",family="monospace",sizes="medium"))
	assign("gSortie",gSortie,envir=.GlobalEnv) 
	
	gWidgets::add(ggrouptotal,  gSortie,  expand=FALSE)
# un groupe en dessous mais cette fois horizontal
	ggrouptotal1<- gWidgets::ggroup(horizontal=TRUE) 
	assign("ggrouptotal1",ggrouptotal1,envir=.GlobalEnv) 
	
	gWidgets::add(ggrouptotal,ggrouptotal1,expand=FALSE)
	
# De nouveau un groupe vertical de boutons qui sera pousse a gauche quand le graphique sera insere
	ggroupboutons=gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutons",ggroupboutons,envir=.GlobalEnv)
	
	gWidgets::add(ggrouptotal1,ggroupboutons,expand=FALSE)
}
# Variables used in aes arguments generate a note as being assigned to .globalEnv, either use aes_string,
# or listing them below removes the warning in Rcheck. Discussion in stackoverflow about this, hadley wickham
# considering this a hideous hack..
utils::globalVariables(c("quinzaine", "mois","val_quant","duree","Effectifs",
				"..density..","Cumsum","Date","Effectif","Effectif_total",
				"annee","car_val_identifiant","car_valeur_quantitatif","coef","date_format",
				"debut_pas","effectif","effectif_CALCULE","effectif_EXPERT","effectif_MESURE","effectif_PONCTUEL",
				"effectif_total","fonctionnement","fonctionnementDF","quantite_CALCULE",
						"quantite_EXPERT","quantite_MESURE","quantite_PONCTUEL","libelle","null","type",
						'val_libelle','lot_effectif','bilan_stades_pigm','ope_date_debut','p','poids_moyen',
						'taxon_stades,"jour',"valeur","mintab","maxtab","moyenne","jour","total_annuel"))

# Assignation in global environment for the use of gWidget interface (there is no way arround this)
utils::globalVariables(c("win","group","nbligne","ggrouptotal","ggrouptotal1","gSortie",
				"col.sortie","ggroupboutons","ggroupboutonsbas","graphes",
				"frame_annee","frame_check","frame_choice","frame_par","frame_parqual","frame_parquan",
				"frame_std","frame_tax","logw","bilan_stades_pigm","usrname","usrpwd"))
# Progressbar
utils::globalVariables(c("progres"))
# reoxygenize fails if data are not loaded
#setwd("F:/workspace/stacomir/branch0.5/stacomir")
#data("bMM_Arzal")