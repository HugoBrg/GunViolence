#librairie data.table pour fread
library(data.table)

#garde les critères qui nous intéresse
filtrer_data <- function(data) {
  filteredData <- matrix(ncol = 4)                                                      #matrice de 4 colonnes
  colnames(filteredData) <- c("date", "nombre_morts", "nombre_blesses", "etat")         #noms des colonnes                         
  for (row in 1:nrow(data)) {                                                           #parcours les données originales
    if (is.null(data[row, "date"]) |                                                    #si il manque une valeur on ignore cette ligne
        length(data[row, "date"]) == 0 |
        is.null(data[row, "n_killed"]) |
        length(data[row, "n_killed"]) == 0 |
        is.null(data[row, "n_injured"]) |
        length(data[row, "n_injured"]) == 0 |
        is.null(data[row, "state"]) |
        length(data[row, "state"]) == 0) {
      next                                                                              #alors saute l'iteration actuelle sans terminer la boucle
    }
    date <- data[row, "date"]                                                           #alors on remplie notre tableau avec les données
    nombre_morts <- data[row, "n_killed"]
    nombre_blesses <- data[row, "n_injured"]
    etat <- data[row, "state"]
    
    aRow <- tryCatch({                                                                  
      dateAsDate <- as.Date(date, "%Y-%m-%d", "%Y/%m/%d", FALSE)                        #convertie le format de la date
      if (is.na(dateAsDate))                                                            #si pas de date
        next                                                                            #saute l'iteration actuelle sans terminer la boucle
      c(substr(paste(dateAsDate, ""), 1, 4), nombre_morts, nombre_blesses, etat)        #on ne met rien
    }, warning = function(w) {
      print(w)
      return(NULL)
    }, error = function(e) {
      print(e)
      return(NULL)
    })  
   
    rbind(filteredData, aRow, NULL) -> filteredData                                     #combine la matrice fileteredData et aRow                                 
  }
  return(filteredData)
}

headers <-
  c(
    "incident_id",
    "date",
    "state",
    "city_or_county",
    "address",
    "n_killed",
    "n_injured",
    "incident_url",
    "source_url",
    "incident_url_fields_missing",
    "congressional_district",
    "gun_stolen",
    "gun_type",
    "incident_characteristics",
    "latitude",
    "location_description",
    "longitude",
    "n_guns_involved",
    "notes",
    "participant_age",
    "participant_age_group",
    "participant_gender",
    "participant_name",
    "participant_relationship",
    "participant_status",
    "participant_type",
    "sources",
    "state_house_district",
    "state_senate_district"
  )
data_set <-                                                                              #importe le CSV
  fread(
    "/home/user/Téléchargements/gun-violence-data/gun-violence-data_01-2013_03-2018.csv",
    sep = ",",
    header = TRUE,
    nrows = 2000                                                                         #selectionne seulement 2000 lignes
  )
data_set <- as.data.frame(data_set)                                                      #convertir en data frame (permet de stocker des éléments de types différents dans les mêmes colonne/lignes)                                                        
colnames(data_set) <- headers                                                            #rajoute les headers qui ont disparu lors de la conversion

donnees_filtrees <- filtrer_data(data_set)                                               #utilise la fonction précédement déclaré

liste_dates <- donnees_filtrees[, "date"]                                                #met les différents critères dans des listes
liste_morts <- as.numeric(donnees_filtrees[, "nombre_morts"])
liste_blesses <- as.numeric(donnees_filtrees[, "nombre_blesses"])
liste_etats <- donnees_filtrees[, "etat"]

liste_morts.mean <- mean(liste_morts, na.rm = TRUE)                                      #moyenne et suppression de NA
liste_morts.sd <- sd(liste_morts, na.rm = TRUE)                                          #deviation standard
liste_morts.norm <- pnorm(liste_morts, m = liste_morts.mean, sd = liste_morts.sd)        #normes vectorielle
liste_morts.density <- density(liste_morts.norm, na.rm = TRUE)                           #estimation de densité
plot(liste_morts.density, main = "Fonction densité du nombre de morts")                  #trace la densité de morts

hist(                                                                                    #trace les historgrammes
  liste_morts,
  breaks = 50 ,
  xlim = c(0, 5),
  col = rgb("#FFFF00", "#CC0000", "#0000CC", "#00CC00"),
  ylab = "Fréquence" ,
  xlab = "Valeurs liste des morts" ,
  main = "Distribution nombre de mort par balle"
)
hist(
  liste_blesses,
  breaks = 50 ,
  xlim = c(0, 5),
  col = rgb("#FFFF00", "#CC0000", "#0000CC", "#00CC00"),
  ylab = "Fréquence" ,
  xlab = "Valeurs liste des blessés" ,
  main = "Distribution nombre de blessés par balle"
)
nb_crimes_etats <- rle(sort(liste_etats))                                                #tri les etat par nombre de crimes 