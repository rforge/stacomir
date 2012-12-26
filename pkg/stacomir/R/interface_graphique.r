# Nom fichier :        interface_graphique 
# Projet :             stacomiR 
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :   11/04/2006 15:51:44
# Compatibilite :      PostgreSQL 9.0


# fonctions handler utilisees par l'interface

#' handler function used by the main interface
#' @param h 
#' @param ...
#' @aliases hDC,hOPE,hDFDC,hBilanMigration,hBilanMigrationInterannuelle,hBilanMigrationConditionEnv, hBilanMigrationPar, hBilanConditionEnv, hBilanLots, hTail, hpds, hSt, htodo,  hhelp, h0, hx11 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
#' this handler test the connexion and if it works loads the stacomi interface
#' @note gr_interface is copied by stacomi into envir_stacomi.
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}

husr=function(h,...){
	gr_interface<-get("gr_interface",envir_stacomi) # logical true or false
	# test de la connexion
	con=new("ConnexionODBC")
	if (gr_interface){
		baseODBC[2]<-svalue(usrname)
		baseODBC[3]<-svalue(usrpwd)
	} else {
		# on prend les valeurs choisies par d�faut dans baseODBC
		# rien
	}
	assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
	con@baseODBC=baseODBC
	e=expression(con<-connect(con))
	con=tryCatch(eval(e),error=get("msg",envir=envir_stacomi)$interface_graphique_log.7) #finally=odbcClose(con@connexion)clause inutile car si �a plante la connexion n'est pas ouverte
	test<-con@etat==get("msg",envir=envir_stacomi)$ConnexionODBC.6
	if (exists("logw")) dispose(logw)
	odbcCloseAll()
	# if the test is OK launches the stacomi interface
	# function handler called by gmessage
	hgmessage=function(h,...){
		stacomi(gr_interface=TRUE)
		# en cas d'erreur on relance une demande de mot de passe
	}
	if (test) { # il existe un lien ODBC mais qui pointe peut �tre ailleurs
		requete=new("RequeteODBC")
		objet@baseODBC<-get("baseODBC",envir=envir_stacomi)
		requete@sql="select count(*) from ref.tr_taxon_tax"
		requete=connect(requete)
		if (nrow(requete@query)==0){
			# le lien ODBC fonctionne mais pointe vers la mauvaise base
			gmessage(message=paste(get("msg",envir=envir_stacomi)$interface_graphique_log.8,
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
		gmessage(message=paste(get("msg",envir=envir_stacomi)$interface_graphique_log.6,
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
h0=function(h,...){
	funout(get("msg",envir_stacomi)$interface_graphique.15)
	require('Rcmdr')
	eval(call("Commander"))
}
hx11=function(h,...){
	x11()
}

#' Function that loads the loginwindow, tests connection, and then destroys the window
#' @param baseODBC the ODBC connection chain contains user and password
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
stacomi=function(gr_interface=TRUE){
	# first loading of connexion and odbc info using chargexml()
	assign("gr_interface",gr_interface,envir=envir_stacomi)
	# the first messages are necessary for the first access to the database, they are in French
	msg<-messages()
	myxml=chargexml()
	baseODBC=myxml[["baseODBC"]]
	datawd=myxml[["datawd"]]
	lang=myxml[["lang"]]	
	assign("lang",lang,envir=envir_stacomi)	
	assign("baseODBC",baseODBC,envir=envir_stacomi)
	assign("datawd",datawd,envir=envir_stacomi)
	refMsg=new("RefMsg")
	createmessage(refMsg)
	
	msg=get("msg",envir=envir_stacomi)
	libraries()
	# loginWindow, will call the husr handler
	if (gr_interface){
	logw <- gwindow(msg$interface_graphique_log.1, 
			name="log",
			parent=c(0,0),
			width=300,height=100)
	assign("logw",logw,envir=.GlobalEnv)
	logly=glayout(container=logw)
	usrname<- gedit(text = baseODBC[2], 
			width = 10, 
			container = logly)
	assign("usrname",usrname,.GlobalEnv)
	usrpwd<- gedit(text = baseODBC[3], 
			width = 10, 
			container = logly)
	assign("usrpwd",usrpwd,.GlobalEnv)
	but=gbutton(text =  msg$interface_graphique_log.4,
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

#' lancement du programme, cette fonction recupere d'abord les chemins ODBC et le repertoire de travail � partir du fichier XML
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
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
	
	library(gWidgets)
	win <- gwindow(msg$interface_graphique.16, name="main",parent=c(0,0),width=1000,height=600)
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
	menubarlist$Utilitaires$Rcmdr$handler=h0
	menubarlist$Utilitaires$Rcmdr$icon="gWidgetsRGtk2-symbol_star"
	menubarlist$Utilitaires$newgraph$handler=hx11
	menubarlist$Utilitaires$newgraph$icon="newplot"
	menubarlist[[msg$interface_graphique_menu.3]]$About$handler = hhelp
	menubarlist[[msg$interface_graphique_menu.3]]$About$icon="dialog-info"
	menubarlist[[msg$interface_graphique_menu.3]]$lang$handler = hlang
	menubarlist[[msg$interface_graphique_menu.3]]$lang$icon="dialog-info"
	add(win, gmenu(menubarlist))
	ggrouptotal<- ggroup(horizontal=FALSE)         # celui ci empile les autres de haut en bas
	assign("ggrouptotal",ggrouptotal,envir=.GlobalEnv) 
	
	add(win,ggrouptotal)
	
	gSortie=gtext(msg$interface_graphique.18,width =800 , height = 150,font.attr=list(style="italic", col="blue",family="monospace",sizes="medium"))
	assign("gSortie",gSortie,envir=.GlobalEnv) 
	
	add(ggrouptotal,  gSortie,  expand=FALSE)
# un groupe en dessous mais cette fois horizontal
	ggrouptotal1<- ggroup(horizontal=TRUE) 
	assign("ggrouptotal1",ggrouptotal1,envir=.GlobalEnv) 
	
	add(ggrouptotal,ggrouptotal1,expand=FALSE)
	
# De nouveau un groupe vertical de boutons qui sera pousse � gauche quand le graphique sera insere
	ggroupboutons=ggroup(horizontal=FALSE)
	assign("ggroupboutons",ggroupboutons,envir=.GlobalEnv)
	
	add(ggrouptotal1,ggroupboutons,expand=FALSE)
}

