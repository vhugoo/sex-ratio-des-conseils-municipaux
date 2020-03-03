library(tidyverse)
library(RColorBrewer)
library(maptools)
library(classInt)

#
#   mairie où les quotas ne sont pas respectés / version "tidy" 
#

# local pour gérer les problèmes de caractère ; guess_max pour éviter qu'il croit tous les codes départements
# sont des "integer" alors qu'à partir de 130.000 et quelques y'a genre 2A et 2B (corse)

base <- read_tsv("1-rne-cm.tsv", local = locale(encoding = "latin1"), guess_max = 200000)

# ajoute une colomne comptant le nombre d'élus pour chaque département, puis le nombre d'élus hommes pour chaque
# département, puis le pourcentage d'élus hommes

# calcul plus simple pas exactement dans l'objet = pourcentage de conseillers municipaux par département
# 
# baseb <- base %>% group_by(`Code du département (Maire)`) %>% summarise(N = n(), N2 = sum(`Code sexe`=="M")) 
# baseb <- baseb %>% mutate(N3=(100*N2/N))

# création d'un code unique par département

base <- base %>% mutate(CodeCom= paste(`Code du département (Maire)`,
                              `Code Insee de la commune`))

# création d'une nouvelle base avec pour chaque commune 1 si majorité stricte d'hommes

basec <- base %>% group_by(`CodeCom`) %>% summarise(N = n(), 
                                                    N2 = sum(`Code sexe`=="M"),
                                                    N3 = ifelse(N2>(N/2),1,0))

# ajout pour chaque commune le département de départ

basec$Dep <-sapply(strsplit(basec$CodeCom,"\\s"), `[`, 1)

# pour chaque département, je calcule le pourcentage P de mairie avec majorité
# masculine de conseillers municipaux

baseF <- basec %>%  group_by(Dep) %>%  summarise(P = sum(`N3`)/n()*100)


# importation des données géométriques

Departement <- readShapeSpatial("spacedata/ADE-COG_1-1_SHP_LAMB93_FR/DEPARTEMENT.shp")

# je renomme pour que ça matche bien

baseF$Dep[c(1,12,24,35,46,57,68,79,90)] <- c("01","02","03","04","05","06","07","08","09")

# je supprime (mal) une ligne NA dans la baseF (je sais pas d'où elle vient)

# baseF <- baseF[-74, ]

# fusion spacedate par le numéro INSEE des dép'

Departement@data <- merge(Departement@data,baseF,by.x="INSEE_DEP",by.y="Dep",all.x=TRUE)

# création d'un vecteur de quantiles, création d'un jeu de 4 couleurs,
# attribution du jeu de couleur aux quantiles, attribution du jeu de couleur
# en fonction des positions dans les quantile des éléments du data frame

distrMecs <- classIntervals(Departement@data$P,4,style="quantile")$brks
colordistri <- brewer.pal(4,"Reds")
colMap <- colordistri[(findInterval(Departement@data$P,distrMecs,all.inside=TRUE))]

# on a tout ce qu'il faut, on passe à la création de la carte

plot(Departement,col=colMap)

legend(x="topright",
       legend=leglabs(as.character(round(distrMecs))),
       fill=colordistri,
       bty="n",
       cex=1,
       title="")
