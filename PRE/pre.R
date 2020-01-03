# Gun Violence Data - BERANGER - ARNAUD - CHIESA


#                                                                 ~~~COLLECTE~~~

#Lecture du fichier
mydata = read.csv(
  file = "C:/Users/hugob/Desktop/GunViolence/gun-violence-data_01-2013_03-2018.csv",
  header = TRUE,
  sep = ",",
  fill = TRUE,
  na.strings = c("", "NA"),                                                                      #remplace les vides par NA
  stringsAsFactors = FALSE                                                                       #ne lit pas en tant de vecteurs les strings
)


#Selection des colonnes voulues
myvars <- c("n_killed", "n_injured", "participant_age", "state")
newdata <- mydata[myvars]


for(i in 1:nrow(newdata)){
  x <- newdata[i,"participant_age"]                                                 #prend l'age des participant
  newdata[i,"n_participant"]<- lengths(regmatches(x, gregexpr("::", x)))            #compte les participants en comptant le nombre de '::'
  is.na(newdata[i,"n_participant"]) <- !newdata[i,"n_participant"]                  #remplace les '0' par de 'NA'
  newdata[i,"participant_age"]<-gsub("\\d::","",newdata[i,"participant_age"])       #supprime les 'chiffre::' de "participant_age"
  newdata[i,"participant_age"]<-gsub("\\x7c\\x7c",";",newdata[i,"participant_age"]) #supprime les '||' de "participant_age" et les remplace par des ';'
}

