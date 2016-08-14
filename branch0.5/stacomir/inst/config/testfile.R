source("F:/workspace/stacomir/branch0.5/stacomir/inst/config/stacomi.r")
# interface_graphique()
bilanMigrationMult=new("BilanMigrationMult")



bilanMigrationMult=load(bilanMigrationMult,
dc=c(5,6,12),
taxons=c("Anguilla anguilla"),
stades=c("AGG","AGJ","CIV"),datedebut="2011-01-01",datefin="2011-12-31")

bilanMigrationMult=calcule(bilanMigrationMult)
hbilanMigrationMultgraph()
hbilanMigrationMultgraph2()
hbilanMigrationMultgraph3()


source("F:/workspace/stacomir/branch0.5/stacomir/inst/config/stacomi.r")
# les options sont récupérées par défaut...
# je les change pour les tests sur migration
ls(envir=envir_stacomi) # liste des objects présents dans l'environnement de travail créé par le package
baseODBC<-get("baseODBC",envir=envir_stacomi)
baseODBC[c(2,3)]<-rep("migradour",2)
# réassignation dans l'environnement de travail
assign("baseODBC",baseODBC,envir_stacomi)
sch<-get("sch",envir=envir_stacomi) # "iav."
assign("sch","migradour.",envir_stacomi)
rm(baseODBC,sch) # j'enlève les objects de l'environnement global

options(sqldf.RPostgreSQL.user = "migradour", 
		sqldf.RPostgreSQL.password ="migradour",
		sqldf.RPostgreSQL.dbname = "bd_contmig_nat",
		sqldf.RPostgreSQL.host = "localhost",#  1.100.1.6
		sqldf.RPostgreSQL.port = "5432")
sqldf("SELECT
ouv_sta_code as sta_code,
ouv_code,
ouv_libelle,
dis2.dis_identifiant as df,
dis2.dis_commentaires as df_commentaires,
dif_code,
dis1.dis_identifiant as dc, 
dis1.dis_commentaires as dc_commentaires,
dic_code             
from migradour.tg_dispositif_dis dis1
JOIN migradour.t_dispositifcomptage_dic ON dic_dis_identifiant=dis_identifiant
JOIN migradour.t_dispositiffranchissement_dif dif ON dic_dif_identifiant=dif_dis_identifiant
JOIN migradour.tg_dispositif_dis dis2 ON dif_dis_identifiant=dis2.dis_identifiant
JOIN migradour.t_ouvrage_ouv ouv ON dif_ouv_identifiant=ouv_identifiant")
#1     ArtixPasse 20
#2     SordePasse 32
#3   SCqAscenseur 22
#4  GuerlainPasse 33
#5     SoeixPasse 34
#6  CherautePasse 35
#7   ChopoloPasse 36
#8      OlhaPasse 39
#9    HalsouPasse 37
#10  UxondoaPasse 38
#11  MasseysPasse 40
#12    PuyooPasse 43
# Baigts ?
# interface_graphique() # pour lancer avec l'interface
bilanMigrationMult=new("BilanMigrationMult")
bilanMigrationMult=load(bilanMigrationMult,dc=c(20,22,32,33,34,35,36,37,38,39,40),
		taxons=c("Oncorhynchus mykiss","Salmo salar","Salmo trutta trutta"),
		stades=c("11","IND","SML"),
		datedebut="01/01/2013",
		datefin="31/12/2013")
bilanMigrationMult=calcule(bilanMigrationMult)
hbilanMigrationMultgraph()
hbilanMigrationMultgraph3()

