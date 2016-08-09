
bilanMigrationMult=new("BilanMigrationMult")



bilanMigrationMult=load(bilanMigrationMult,
dc=c(5,6),
taxons=c("Anguilla anguilla","Salmo salar"),
stades=c("AGG","AGJ","CIV"),datedebut="2011-01-01",datefin="2011-12-31")

bilanMigrationMult=calcule(bilanMigrationMult)
hbilanMigrationMultgraph()


taxon="Anguilla anguilla"
stade="CIV"
dc=6

dcnum=2
stadenum=3
taxonnum=1

stadenum=2