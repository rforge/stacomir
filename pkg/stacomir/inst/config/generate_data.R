#################################
# generates dataset for BilanMigrationMult
# from iav three dc with eels
##################################
bMM_Arzal=new("BilanMigrationMult")
bMM_Arzal=load(bMM_Arzal,
dc=c(5,6,12),
taxons=c("Anguilla anguilla"),
stades=c("AGG","AGJ","CIV"),datedebut="2011-01-01",datefin="2011-12-31")
bMM_Arzal<-connect(bMM_Arzal)
devtools::use_data(bMM_Arzal,internal=FALSE,overwrite=TRUE)