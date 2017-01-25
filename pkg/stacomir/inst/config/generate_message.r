# see http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Stacomi%20gettext?action=edit

assign("lang","English",envir=envir_stacomi)
mmmm=new("RefMsg")
createmessage(mmmm,TRUE)
msg<-get("msg",envir=envir_stacomi)
str(msg)
