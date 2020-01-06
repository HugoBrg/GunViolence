#Nombre de morts par etat

nombre_mort_etat <- table(d$n_killed, d$state) 


#table mort par années

dates_fusillades <- substr(d$date, 1, 4)
fusillades_2013 <- table(dates_fusillades == "2013")
fusillades_2014 <- table(dates_fusillades == "2014")
fusillades_2015 <- table(dates_fusillades == "2015")
fusillades_2016 <- table(dates_fusillades == "2016")
fusillades_2017 <- table(dates_fusillades == "2017")
fusillades_2018 <- table(dates_fusillades == "2018")

morts_2013 <- table(d$n_killed, substr(d$date, 1, 4) == "2013")
morts_2014 <- table(d$n_killed, substr(d$date, 1, 4) == "2014")
morts_2015 <- table(d$n_killed, substr(d$date, 1, 4) == "2015")
morts_2016 <- table(d$n_killed, substr(d$date, 1, 4) == "2016")
morts_2017 <- table(d$n_killed, substr(d$date, 1, 4) == "2017")
morts_2018 <- table(d$n_killed, substr(d$date, 1, 4) == "2018")

blesses_2013 <- table(d$n_injured, substr(d$date, 1, 4) == "2013")
blesses_2014 <- table(d$n_injured, substr(d$date, 1, 4) == "2014")
blesses_2015 <- table(d$n_injured, substr(d$date, 1, 4) == "2015")
blesses_2016 <- table(d$n_injured, substr(d$date, 1, 4) == "2016")
blesses_2017 <- table(d$n_injured, substr(d$date, 1, 4) == "2017")
blesses_2018 <- table(d$n_injured, substr(d$date, 1, 4) == "2018")

#Nombre de décés par années

deces_2013 <- sum(d$n_killed[substr(d$date, 1, 4) == "2013"])
deces_2014 <- sum(d$n_killed[substr(d$date, 1, 4) == "2014"])
deces_2015 <- sum(d$n_killed[substr(d$date, 1, 4) == "2015"])
deces_2016 <- sum(d$n_killed[substr(d$date, 1, 4) == "2016"])
deces_2017 <- sum(d$n_killed[substr(d$date, 1, 4) == "2017"])
deces_2018 <- sum(d$n_killed[substr(d$date, 1, 4) == "2018"])

table_nombre_mort_ans <-
  barplot(table(d$n_killed, substr(d$date, 1, 4))) # créer le graphique en barre

vect_morts <-
  c(deces_2013,
    deces_2014,
    deces_2015,
    deces_2016,
    deces_2017,
    deces_2018)

barplot(vect_morts,
        names.arg = c("2013", "2014", "2015", "2016", "2017", "2018"))


#Nombre de bléssés

nombre_blesses_2013 <- sum(d$n_injured[substr(d$date, 1, 4) == "2013"])
nombre_blesses_2014 <- sum(d$n_injured[substr(d$date, 1, 4) == "2014"])
nombre_blesses_2015 <- sum(d$n_injured[substr(d$date, 1, 4) == "2015"])
nombre_blesses_2016 <- sum(d$n_injured[substr(d$date, 1, 4) == "2016"])
nombre_blesses_2017 <- sum(d$n_injured[substr(d$date, 1, 4) == "2017"])
nombre_blesses_2018 <- sum(d$n_injured[substr(d$date, 1, 4) == "2018"])

vect_blesses <-
  c(nombre_blesses_2013,
    nombre_blesses_2014,
    nombre_blesses_2015,
    nombre_blesses_2016,
    nombre_blesses_2017,
    nombre_blesses_2018)

barplot(vect_blesses,
        names.arg = c("2013", "2014", "2015", "2016", "2017", "2018"))


#Nombre total de bléssés + de morts

vect_total <-
  c(
    deces_2013 + nombre_blesses_2013,
    deces_2014 + nombre_blesses_2014,
    deces_2015 + nombre_blesses_2015,
    deces_2016 + nombre_blesses_2016,
    deces_2017 + nombre_blesses_2017,
    deces_2018 + nombre_blesses_2018
  )
barplot(vect_total,
        names.arg = c("2013", "2014", "2015", "2016", "2017", "2018"))

#Nombre de morts par mois, toute années confondue

morts_01 <- sum(d$n_killed[substr(d$date, 6, 7) == "01"])
morts_02 <- sum(d$n_killed[substr(d$date, 6, 7) == "02"])
morts_03 <- sum(d$n_killed[substr(d$date, 6, 7) == "03"])
morts_04 <- sum(d$n_killed[substr(d$date, 6, 7) == "04"])
morts_05 <- sum(d$n_killed[substr(d$date, 6, 7) == "05"])
morts_06 <- sum(d$n_killed[substr(d$date, 6, 7) == "06"])
morts_07 <- sum(d$n_killed[substr(d$date, 6, 7) == "07"])
morts_08 <- sum(d$n_killed[substr(d$date, 6, 7) == "08"])
morts_09 <- sum(d$n_killed[substr(d$date, 6, 7) == "09"])
morts_10 <- sum(d$n_killed[substr(d$date, 6, 7) == "10"])
morts_11 <- sum(d$n_killed[substr(d$date, 6, 7) == "11"])
morts_12 <- sum(d$n_killed[substr(d$date, 6, 7) == "12"])

vect_morts_par_mois <-
  c(
    morts_01,
    morts_02,
    morts_03,
    morts_04,
    morts_05,
    morts_06,
    morts_07,
    morts_08,
    morts_09,
    morts_10,
    morts_11,
    morts_12
  )
barplot(
  vect_morts_par_mois,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)


#Nombre de bléssés par mois

nombre_blesses_01 <- sum(d$n_injured[substr(d$date, 6, 7) == "01"])
nombre_blesses_02 <- sum(d$n_injured[substr(d$date, 6, 7) == "02"])
nombre_blesses_03 <- sum(d$n_injured[substr(d$date, 6, 7) == "03"])
nombre_blesses_04 <- sum(d$n_injured[substr(d$date, 6, 7) == "04"])
nombre_blesses_05 <- sum(d$n_injured[substr(d$date, 6, 7) == "05"])
nombre_blesses_06 <- sum(d$n_injured[substr(d$date, 6, 7) == "06"])
nombre_blesses_07 <- sum(d$n_injured[substr(d$date, 6, 7) == "07"])
nombre_blesses_08 <- sum(d$n_injured[substr(d$date, 6, 7) == "08"])
nombre_blesses_09 <- sum(d$n_injured[substr(d$date, 6, 7) == "09"])
nombre_blesses_10 <- sum(d$n_injured[substr(d$date, 6, 7) == "10"])
nombre_blesses_11 <- sum(d$n_injured[substr(d$date, 6, 7) == "11"])
nombre_blesses_12 <- sum(d$n_injured[substr(d$date, 6, 7) == "12"])

vect_blesses_par_mois <-
  c(
    nombre_blesses_01,
    nombre_blesses_02,
    nombre_blesses_03,
    nombre_blesses_04,
    nombre_blesses_05,
    nombre_blesses_06,
    nombre_blesses_07,
    nombre_blesses_08,
    nombre_blesses_09,
    nombre_blesses_10,
    nombre_blesses_11,
    nombre_blesses_12
  )
barplot(
  vect_blesses_par_mois,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)


#Nombre de bléssés total : mort + bléssés


total_01 <- sum(nombre_blesses_01 + morts_01)
total_02 <- sum(nombre_blesses_02 + morts_02)
total_03 <- sum(nombre_blesses_03 + morts_03)
total_04 <- sum(nombre_blesses_04 + morts_04)
total_05 <- sum(nombre_blesses_05 + morts_05)
total_06 <- sum(nombre_blesses_06 + morts_06)
total_07 <- sum(nombre_blesses_07 + morts_07)
total_08 <- sum(nombre_blesses_08 + morts_08)
total_09 <- sum(nombre_blesses_09 + morts_09)
total_10 <- sum(nombre_blesses_10 + morts_10)
total_11 <- sum(nombre_blesses_11 + morts_11)
total_12 <- sum(nombre_blesses_12 + morts_12)

vect_total_par_mois <-
  c(
    total_01,
    total_02,
    total_03,
    total_04,
    total_05,
    total_06,
    total_07,
    total_08,
    total_09,
    total_10,
    total_11,
    total_12
  )
barplot(
  vect_total_par_mois,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)

#Vérification du nombre total de morts
sum(deces_2013 + deces_2014 + deces_2015 + deces_2016 + deces_2017 + deces_2018)


#Total de bléssés
sum(d$n_killed)
sum(d$n_injured)
sum(d$n_injured + d$n_killed)


#Analyse de l'année 2017 car c'est l'année ou il y a le plus de crime


#Nombre de morts par mois en 2017

morts_01_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-01"])
morts_02_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-02"])
morts_03_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-03"])
morts_04_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-04"])
morts_05_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-05"])
morts_06_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-06"])
morts_07_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-07"])
morts_08_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-08"])
morts_09_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-09"])
morts_10_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-10"])
morts_11_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-11"])
morts_12_2017 <- sum(d$n_killed[substr(d$date, 1, 7) == "2017-12"])

vect_morts_par_mois_en_2017 <-
  c(
    morts_01_2017,
    morts_02_2017,
    morts_03_2017,
    morts_04_2017,
    morts_05_2017,
    morts_06_2017,
    morts_07_2017,
    morts_08_2017,
    morts_09_2017,
    morts_10_2017,
    morts_11_2017,
    morts_12_2017
  )
barplot(
  vect_morts_par_mois_en_2017,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)


#Nombre de bléssés par mois en 2017

blesses_01_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-01"])
blesses_02_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-02"])
blesses_03_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-03"])
blesses_04_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-04"])
blesses_05_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-05"])
blesses_06_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-06"])
blesses_07_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-07"])
blesses_08_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-08"])
blesses_09_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-09"])
blesses_10_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-10"])
blesses_11_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-11"])
blesses_12_2017 <- sum(d$n_injured[substr(d$date, 1, 7) == "2017-12"])

vect_blesses_par_mois_en_2017 <-
  c(
    blesses_01_2017,
    blesses_02_2017,
    blesses_03_2017,
    blesses_04_2017,
    blesses_05_2017,
    blesses_06_2017,
    blesses_07_2017,
    blesses_08_2017,
    blesses_09_2017,
    blesses_10_2017,
    blesses_11_2017,
    blesses_12_2017
  )
barplot(
  vect_blesses_par_mois_en_2017,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)


#Nombre total de bléssés en 2017 : morts + bléssés


total_01_2017 <- sum(morts_01_2017 + blesses_01_2017)
total_02_2017 <- sum(morts_02_2017 + blesses_02_2017)
total_03_2017 <- sum(morts_03_2017 + blesses_03_2017)
total_04_2017 <- sum(morts_04_2017 + blesses_04_2017)
total_05_2017 <- sum(morts_05_2017 + blesses_05_2017)
total_06_2017 <- sum(morts_06_2017 + blesses_06_2017)
total_07_2017 <- sum(morts_07_2017 + blesses_07_2017)
total_08_2017 <- sum(morts_08_2017 + blesses_08_2017)
total_09_2017 <- sum(morts_09_2017 + blesses_09_2017)
total_10_2017 <- sum(morts_10_2017 + blesses_10_2017)
total_11_2017 <- sum(morts_11_2017 + blesses_11_2017)
total_12_2017 <- sum(morts_12_2017 + blesses_12_2017)

vect_total_par_mois_en_2017 <-
  c(
    total_01_2017,
    total_02_2017,
    total_03_2017,
    total_04_2017,
    total_05_2017,
    total_06_2017,
    total_07_2017,
    total_08_2017,
    total_09_2017,
    total_10_2017,
    total_11_2017,
    total_12_2017
  )
barplot(
  vect_total_par_mois_en_2017,
  names.arg = c(
    "janvier",
    "février",
    "mars",
    "avril",
    "mai",
    "juin",
    "juillet",
    "aout",
    "septembre",
    "octobre",
    "novembre",
    "décembre"
  )
)
