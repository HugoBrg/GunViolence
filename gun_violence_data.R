# Gun Violence Data - BERANGER - ARNAUD - CHIESA


#                                                                 ~~~COLLECTE~~~

#Lecture du fichier
mydata = read.csv(file = "C:/Users/hugob/Desktop/GunViolence/gun-violence-data_01-2013_03-2018.csv", header=TRUE,sep=",", fill=TRUE,na.strings=c("","NA"))
#head(mydata)


#On garde seulement les colonne qui nous interesse
myvars <- c("n_killed", "n_injured", "participant_age", "state")
newdata <- mydata[myvars]
newdata$n_participant<-NA


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x)-n+1)
}

for(i in 1:nrow(newdata)){
  x <- newdata[i,"participant_age"]
  newdata[i,"n_participant"]<-substrRight(x, 5)
}


z = newdata[3,2]