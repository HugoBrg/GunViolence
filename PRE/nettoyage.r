library(data.table)

filtrer_data <- function(data) {
  filteredData <- matrix(ncol = 4)
  colnames(filteredData) <-
    c("date", "nombre_morts", "nombre_blesses", "etat")
  for (row in 1:nrow(data)) {
    if (is.null(data[row, "date"]) |
        length(data[row, "date"]) == 0 |
        is.null(data[row, "n_killed"]) |
        length(data[row, "n_killed"]) == 0 |
        is.null(data[row, "n_injured"]) |
        length(data[row, "n_injured"]) == 0 |
        is.null(data[row, "state"]) |
        length(data[row, "state"]) == 0) {
      next
    }
    date <- data[row, "date"]
    nombre_morts <- data[row, "n_killed"]
    nombre_blesses <- data[row, "n_injured"]
    etat <- data[row, "state"]
    
    aRow <- tryCatch({
      dateAsDate <- as.Date(date, "%Y-%m-%d", "%Y/%m/%d", FALSE)
      if (is.na(dateAsDate))
        next
      c(substr(paste(dateAsDate, ""), 1, 4), nombre_morts, nombre_blesses, etat)
    }, warning = function(w) {
      print(w)
      return(NULL)
    }, error = function(e) {
      print(e)
      return(NULL)
    })
    rbind(filteredData, aRow, NULL) -> filteredData
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
data_set <-
  fread(
    "/home/user/Téléchargements/gun-violence-data/gun-violence-data_01-2013_03-2018.csv",
    sep = ",",
    header = TRUE,
    nrows = 2000
  )
data_set <- as.data.frame(data_set)
colnames(data_set) <- headers

#View(data_set)
donnees_filtrees <- filtrer_data(data_set)
View(donnees_filtrees)

liste_dates <- donnees_filtrees[, "date"]
liste_morts <- as.numeric(donnees_filtrees[, "nombre_morts"])
liste_blesses <- as.numeric(donnees_filtrees[, "nombre_blesses"])
liste_etats <- donnees_filtrees[, "etat"]

liste_morts.mean <- mean(liste_morts, na.rm = TRUE)
liste_morts.sd <- sd(liste_morts, na.rm = TRUE)
liste_morts.norm <- pnorm(liste_morts, m = liste_morts.mean, sd = liste_morts.sd)
liste_morts.density <- density(liste_morts.norm, na.rm = TRUE)
plot(liste_morts.density, main = "Fonction densité du nombre de morts")

hist(
  liste_morts,
  breaks = 50 ,
  xlim = c(0, 5),
  border = F,
  col = rgb(0.1, 0.8, 0.3, 0.5),
  ylab = "Fréquence" ,
  xlab = "Valeurs liste des morts" ,
  main = "Distribution nombre de mort par balle"
)
hist(
  liste_blesses,
  breaks = 50 ,
  xlim = c(0, 5),
  border = F,
  col = rgb(0.1, 0.8, 0.3, 0.5),
  ylab = "Fréquence" ,
  xlab = "Valeurs liste des blessés" ,
  main = "Distribution nombre de blessés par balle"
)
nb_crimes_etats <- rle(sort(liste_etats))
print(nb_crimes_etats)
