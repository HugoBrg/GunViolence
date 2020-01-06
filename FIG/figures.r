#nombre de morts par etat

table_nb_mort_par_etat <- table(d$n_killed,d$state)

#5 etat avec le plus de crime

#mort par années

shootings_date <- substr(d$date,1,4)
shooting_2013 <- table(shootings_date=="2013")
shooting_2014 <- table(shootings_date=="2014")
shooting_2015 <- table(shootings_date=="2015")
shooting_2016 <- table(shootings_date=="2016")
shooting_2017 <- table(shootings_date=="2017")
shooting_2018 <- table(shootings_date=="2018")

table_morts_2013 <- table(d$n_killed,substr(d$date,1,4)=="2013")
table_morts_2014 <- table(d$n_killed,substr(d$date,1,4)=="2014")
table_morts_2015 <- table(d$n_killed,substr(d$date,1,4)=="2015")
table_morts_2016 <- table(d$n_killed,substr(d$date,1,4)=="2016")
table_morts_2017 <- table(d$n_killed,substr(d$date,1,4)=="2017")
table_morts_2018 <- table(d$n_killed,substr(d$date,1,4)=="2018")

table_blesses_2013 <- table(d$n_injured,substr(d$date,1,4)=="2013")
table_blesses_2014 <- table(d$n_injured,substr(d$date,1,4)=="2014")
table_blesses_2015 <- table(d$n_injured,substr(d$date,1,4)=="2015")
table_blesses_2016 <- table(d$n_injured,substr(d$date,1,4)=="2016")
table_blesses_2017 <- table(d$n_injured,substr(d$date,1,4)=="2017")
table_blesses_2018 <- table(d$n_injured,substr(d$date,1,4)=="2018")

#Nb mort par années

nb_kill_2013 <- sum(d$n_killed[substr(d$date,1,4)=="2013"])
nb_kill_2014 <- sum(d$n_killed[substr(d$date,1,4)=="2014"])
nb_kill_2015 <- sum(d$n_killed[substr(d$date,1,4)=="2015"])
nb_kill_2016 <- sum(d$n_killed[substr(d$date,1,4)=="2016"])
nb_kill_2017 <- sum(d$n_killed[substr(d$date,1,4)=="2017"])
nb_kill_2018 <- sum(d$n_killed[substr(d$date,1,4)=="2018"])

table_nb_mort_par_ans <- barplot(table(d$n_killed,substr(d$date,1,4)))

vect_mort <- c(nb_kill_2013,nb_kill_2014,nb_kill_2015,nb_kill_2016,nb_kill_2017,nb_kill_2018)

barplot(vect_mort,names.arg=c("2013","2014","2015","2016","2017","2018"))


#vect_date <- c(2013,2014,2015,2016,2017,2018)
#¢vect_mort_ans <- rbind(vect_mort,vect_date)


#Nb blésés

nb_inju_2013 <- sum(d$n_injured[substr(d$date,1,4)=="2013"])
nb_inju_2014 <- sum(d$n_injured[substr(d$date,1,4)=="2014"])
nb_inju_2015 <- sum(d$n_injured[substr(d$date,1,4)=="2015"])
nb_inju_2016 <- sum(d$n_injured[substr(d$date,1,4)=="2016"])
nb_inju_2017 <- sum(d$n_injured[substr(d$date,1,4)=="2017"])
nb_inju_2018 <- sum(d$n_injured[substr(d$date,1,4)=="2018"])

vect_blese <- c(nb_inju_2013,nb_inju_2014,nb_inju_2015,nb_inju_2016,nb_inju_2017,nb_inju_2018)

barplot(vect_blese,names.arg=c("2013","2014","2015","2016","2017","2018"))

#vect_date <- c(2013,2014,2015,2016,2017,2018)
#barplot(vect_blese,vect_date)


#NB Tot blésés mort + blésés

vect_tot <- c(nb_kill_2013+nb_inju_2013,nb_kill_2014+nb_inju_2014,nb_kill_2015+nb_inju_2015,nb_kill_2016+nb_inju_2016,nb_kill_2017+nb_inju_2017,nb_kill_2018+nb_inju_2018)
barplot(vect_tot,names.arg=c("2013","2014","2015","2016","2017","2018"))

#Nb de mort par mois sur toute années confondue

nb_mort_01 <- sum(d$n_killed[substr(d$date,6,7)=="01"])
nb_mort_02 <- sum(d$n_killed[substr(d$date,6,7)=="02"])
nb_mort_03 <- sum(d$n_killed[substr(d$date,6,7)=="03"])
nb_mort_04 <- sum(d$n_killed[substr(d$date,6,7)=="04"])
nb_mort_05 <- sum(d$n_killed[substr(d$date,6,7)=="05"])
nb_mort_06 <- sum(d$n_killed[substr(d$date,6,7)=="06"])
nb_mort_07 <- sum(d$n_killed[substr(d$date,6,7)=="07"])
nb_mort_08 <- sum(d$n_killed[substr(d$date,6,7)=="08"])
nb_mort_09 <- sum(d$n_killed[substr(d$date,6,7)=="09"])
nb_mort_10 <- sum(d$n_killed[substr(d$date,6,7)=="10"])
nb_mort_11 <- sum(d$n_killed[substr(d$date,6,7)=="11"])
nb_mort_12 <- sum(d$n_killed[substr(d$date,6,7)=="12"])

vect_mort_mois <- c(nb_mort_01,nb_mort_02,nb_mort_03,nb_mort_04,nb_mort_05,nb_mort_06,nb_mort_07,nb_mort_08,nb_mort_09,nb_mort_10,nb_mort_11,nb_mort_12)
barplot(vect_mort_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


#Nb blésé

nb_bleses_01 <- sum(d$n_injured[substr(d$date,6,7)=="01"])
nb_bleses_02 <- sum(d$n_injured[substr(d$date,6,7)=="02"])
nb_bleses_03 <- sum(d$n_injured[substr(d$date,6,7)=="03"])
nb_bleses_04 <- sum(d$n_injured[substr(d$date,6,7)=="04"])
nb_bleses_05 <- sum(d$n_injured[substr(d$date,6,7)=="05"])
nb_bleses_06 <- sum(d$n_injured[substr(d$date,6,7)=="06"])
nb_bleses_07 <- sum(d$n_injured[substr(d$date,6,7)=="07"])
nb_bleses_08 <- sum(d$n_injured[substr(d$date,6,7)=="08"])
nb_bleses_09 <- sum(d$n_injured[substr(d$date,6,7)=="09"])
nb_bleses_10 <- sum(d$n_injured[substr(d$date,6,7)=="10"])
nb_bleses_11 <- sum(d$n_injured[substr(d$date,6,7)=="11"])
nb_bleses_12 <- sum(d$n_injured[substr(d$date,6,7)=="12"])

vect_bleses_mois <- c(nb_bleses_01,nb_bleses_02,nb_bleses_03,nb_bleses_04,nb_bleses_05,nb_bleses_06,nb_bleses_07,nb_bleses_08,nb_bleses_09,nb_bleses_10,nb_bleses_11,nb_bleses_12)
barplot(vect_bleses_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


#Nb blésé total : mort + blésés


nb_tot_01 <- sum(nb_bleses_01+nb_mort_01)
nb_tot_02 <- sum(nb_bleses_02+nb_mort_02)
nb_tot_03 <- sum(nb_bleses_03+nb_mort_03)
nb_tot_04 <- sum(nb_bleses_04+nb_mort_04)
nb_tot_05 <- sum(nb_bleses_05+nb_mort_05)
nb_tot_06 <- sum(nb_bleses_06+nb_mort_06)
nb_tot_07 <- sum(nb_bleses_07+nb_mort_07)
nb_tot_08 <- sum(nb_bleses_08+nb_mort_08)
nb_tot_09 <- sum(nb_bleses_09+nb_mort_09)
nb_tot_10 <- sum(nb_bleses_10+nb_mort_10)
nb_tot_11 <- sum(nb_bleses_11+nb_mort_11)
nb_tot_12 <- sum(nb_bleses_12+nb_mort_12)

vect_tot_mois <- c(nb_tot_01,nb_tot_02,nb_tot_03,nb_tot_04,nb_tot_05,nb_tot_06,nb_tot_07,nb_tot_08,nb_tot_09,nb_tot_10,nb_tot_11,nb_tot_12)
barplot(vect_tot_mois,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))




#table_nb_bleses_par_ans <- barplot(table(d$n_injured,substr(d$date,1,4)))

#Nb mort total vérification
sum(nb_kill_2013 + nb_kill_2014 + nb_kill_2015 + nb_kill_2016 + nb_kill_2017 + nb_kill_2018)



#Blésés total
sum(d$n_killed)
sum(d$n_injured)
sum(d$n_injured+d$n_killed)

#Ages moyent des blésés

#barplot(sum(d$n_killed[substr(d$date,1,4)=="2013"]),sum(d$n_killed[substr(d$date,1,4)=="2014"]),sum(d$n_killed[substr(d$date,1,4)=="2015"]),sum(d$n_killed[substr(d$date,1,4)=="2016"]),sum(d$n_killed[substr(d$date,1,4)=="2017"]),sum(d$n_killed[substr(d$date,1,4)=="2018"]))





#Analyse année avec le plus de crime 2017


#Nb de mort par mois 2017

nb_mort_01_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-01"])
nb_mort_02_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-02"])
nb_mort_03_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-03"])
nb_mort_04_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-04"])
nb_mort_05_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-05"])
nb_mort_06_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-06"])
nb_mort_07_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-07"])
nb_mort_08_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-08"])
nb_mort_09_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-09"])
nb_mort_10_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-10"])
nb_mort_11_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-11"])
nb_mort_12_2017 <- sum(d$n_killed[substr(d$date,1,7)=="2017-12"])

vect_mort_mois_2017 <- c(nb_mort_01_2017,nb_mort_02_2017,nb_mort_03_2017,nb_mort_04_2017,nb_mort_05_2017,nb_mort_06_2017,nb_mort_07_2017,nb_mort_08_2017,nb_mort_09_2017,nb_mort_10_2017,nb_mort_11_2017,nb_mort_12_2017)
barplot(vect_mort_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


#Nb blésé par mois 2017

nb_bleses_01_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-01"])
nb_bleses_02_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-02"])
nb_bleses_03_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-03"])
nb_bleses_04_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-04"])
nb_bleses_05_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-05"])
nb_bleses_06_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-06"])
nb_bleses_07_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-07"])
nb_bleses_08_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-08"])
nb_bleses_09_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-09"])
nb_bleses_10_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-10"])
nb_bleses_11_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-11"])
nb_bleses_12_2017 <- sum(d$n_injured[substr(d$date,1,7)=="2017-12"])

vect_bleses_mois_2017 <- c(nb_bleses_01_2017,nb_bleses_02_2017,nb_bleses_03_2017,nb_bleses_04_2017,nb_bleses_05_2017,nb_bleses_06_2017,nb_bleses_07_2017,nb_bleses_08_2017,nb_bleses_09_2017,nb_bleses_10_2017,nb_bleses_11_2017,nb_bleses_12_2017)
barplot(vect_bleses_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


#Nb blésé total : mort + blésés 2017


nb_tot_01_2017 <- sum(nb_bleses_01_2017+nb_mort_01_2017)
nb_tot_02_2017 <- sum(nb_bleses_02_2017+nb_mort_02_2017)
nb_tot_03_2017 <- sum(nb_bleses_03_2017+nb_mort_03_2017)
nb_tot_04_2017 <- sum(nb_bleses_04_2017+nb_mort_04_2017)
nb_tot_05_2017 <- sum(nb_bleses_05_2017+nb_mort_05_2017)
nb_tot_06_2017 <- sum(nb_bleses_06_2017+nb_mort_06_2017)
nb_tot_07_2017 <- sum(nb_bleses_07_2017+nb_mort_07_2017)
nb_tot_08_2017 <- sum(nb_bleses_08_2017+nb_mort_08_2017)
nb_tot_09_2017 <- sum(nb_bleses_09_2017+nb_mort_09_2017)
nb_tot_10_2017 <- sum(nb_bleses_10_2017+nb_mort_10_2017)
nb_tot_11_2017 <- sum(nb_bleses_11_2017+nb_mort_11_2017)
nb_tot_12_2017 <- sum(nb_bleses_12_2017+nb_mort_12_2017)

vect_tot_mois_2017 <- c(nb_tot_01_2017,nb_tot_02_2017,nb_tot_03_2017,nb_tot_04_2017,nb_tot_05_2017,nb_tot_06_2017,nb_tot_07_2017,nb_tot_08_2017,nb_tot_09_2017,nb_tot_10_2017,nb_tot_11_2017,nb_tot_12_2017)
barplot(vect_tot_mois_2017,names.arg=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"))


