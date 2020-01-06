library(knitr)
library(data.table)
library(maps)
library(tidyr)
library(ggplot2)
library(dplyr)

# Opérateur %>% provient de la lib dplyr et sert de pipe pour enchaîner des actions
# Importation des données, pas trouvé comment utiliser le dataset nettoyé
d <- read.csv("/home/user/Téléchargements/gun-violence-data/gun-violence-data_01-2013_03-2018.csv")
# Diagramme baton
fusillades_2013 <- sum(substr(d$date,1,4) == "2013")
fusillades_2014 <- sum(substr(d$date,1,4) == "2014")
fusillades_2015 <- sum(substr(d$date,1,4) == "2015")
fusillades_2016 <- sum(substr(d$date,1,4) == "2016")
fusillades_2017 <- sum(substr(d$date,1,4) == "2017")
fusillades_2018 <- sum(substr(d$date,1,4) == "2018")

vect_fusillade <- c(fusillades_2013,fusillades_2014,fusillades_2015,fusillades_2016,fusillades_2017,fusillades_2018)

print("Nombre de morts par année")
barplot(vect_fusillade,names.arg=c("2013","2014","2015","2016","2017","2018"))

# Histogramme pour afficher et trier les états comptabilisants des fusillades
par_etat <- d %>% group_by(state) %>% summarize(n = n()) %>% arrange(n)
par_etat$state <- factor(par_etat$state, levels = par_etat$state)

ggplot(par_etat, aes(x = state, y = n)) + geom_bar(stat = "identity") + coord_flip() + theme_bw()

# Fusillades réparties sur la carte des Etats-Unis
global <- map_data("state")
ggplot(global, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill = "white", col = "black") + coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) +
  geom_point(data = d, aes(x = longitude, y = latitude, col = n_killed), size = 0.001, alpha = .1) +  
  theme_void() + 
  theme(legend.position = "none")

# Fusillades réparties sur l'ensemble des jours
topdatearrange <- d %>% group_by(substr(d$date,6,10)) %>% summarize(n = n()) %>% arrange(-n)
topdatearrange

topdate <- d %>% group_by(substr(d$date,6,10)) %>% summarize(n = n())
topdate
barplot(topdate$n)

# Statistiques sur le 4 juillet (fête nationale), nombre de morts et blessés entre 2013 et 2017
blesses_13_07_04 <- sum(d %>% select(n_injured) %>% filter(d$date == "2013-07-04"))
blesses_14_07_04 <- sum(d %>% select(n_injured) %>% filter(d$date == "2014-07-04"))
blesses_15_07_04 <- sum(d %>% select(n_injured) %>% filter(d$date == "2015-07-04"))
blesses_16_07_04 <- sum(d %>% select(n_injured) %>% filter(d$date == "2016-07-04"))
blesses_17_07_04 <- sum(d %>% select(n_injured) %>% filter(d$date == "2017-07-04"))

morts_13_07_04 <- sum (d %>% select(n_killed) %>% filter(d$date == "2013-07-04"))
morts_14_07_04 <- sum(d %>% select(n_killed) %>% filter(d$date == "2014-07-04"))
morts_15_07_04 <- sum(d %>% select(n_killed) %>% filter(d$date == "2015-07-04"))
morts_16_07_04 <- sum(d %>% select(n_killed) %>% filter(d$date == "2016-07-04"))
morts_17_07_04 <- sum(d %>% select(n_killed) %>% filter(d$date == "2017-07-04"))


morts_blesses_07_04 <- c(morts_13_07_04, blesses_13_07_04,morts_14_07_04,blesses_14_07_04, morts_15_07_04,blesses_15_07_04, morts_16_07_04, blesses_16_07_04,morts_17_07_04, blesses_17_07_04)

barplot(morts_blesses_07_04,names.arg=c("morts_13","blesses_13","morts_14","blesses_14","morts_15","blesses_15","morts_16","blesses_16","morts_17","blesses_17"))