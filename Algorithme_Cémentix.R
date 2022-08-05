library(rvest)
library(xopen)
library(tidyverse)
library("tm")
library("tidytext")
library("proustr")
library("tidyverse")
library(readxl)
library(FactoMineR)
library(factoextra)

# On crée une fonction qui récupère
## - Les défintions du dictionnaire Larousse,
## - La page wikipédia du mot (s'il en existe)
## - Et la liste des mots dans son champ lexical sur le site "rimessolides.com"

recup_definition = function(mot) {
  ## On remplace l'espace par  un underscore dans les mots avec espace pour 
  ## que le lien fonctionne
  mot = gsub(" ", "_", mot)
  
  # Définition larousse
  url_base = "https://www.larousse.fr/dictionnaires/francais/"
  url_mot = paste0(url_base, mot)
  ## try pour que la fonction ne s'arrete pas lorsqu'elle ne trouve de page correspondante
  definitions = try(read_html(url_mot) %>% 
                      html_nodes(".DivisionDefinition") %>% 
                      html_text() %>%
                      map_chr(~ gsub("\r\n\t\t\t\t", " ", .x)), silent = TRUE, outFile = "error")
  
  # Page wiki du mot
  url_wiki = paste0("https://fr.wikipedia.org/wiki/", mot)
  pagewiki = try(read_html(url_wiki) %>% 
                   html_nodes("p") %>% 
                   html_text() %>%
                   map_chr(~ gsub("\n*\t", " ", .x)), silent = TRUE, outFile = "error")
  
  # champ lexical
  url_lexi = paste0("https://www.rimessolides.com/motscles.aspx?m=", mot)
  
  lexique = try(read_html(url_lexi) %>% 
                  html_nodes(".motcle") %>% 
                  html_text() %>%
                  map_chr(~ gsub(",", "", .x)), silent = TRUE, outFile = "error")
  
  #if(str_detect(definitions, "error") == TRUE){definitions = NULL}
  if(str_detect(str_flatten(pagewiki, " "), "HTTP error 404") == TRUE){pagewiki = NULL}
  #if(str_detect(lexique, "error") == TRUE){lexique = NULL}
  recup = c(definitions, pagewiki, lexique)
  return(recup)
  
}

# recup_definition("vendeur")

### Ces trois sources de textes formeront un sac de mots pour tous les mots de notre librairie de mots

## On utilise une librairie de mot télécharger sur le site lexique.org

word_library <- read_excel("Lexique-query-2022-04-15 20-4-46.xlsx")
head(word_library)

# Le jeu de données est très grand avec près de 140 000 mots. 
# On décide de réduire cette liste de mot en 
## - gardant les mot à plus de 3 lettre
## - les mot avec espace (Ex : a priori)
word_lib = word_library %>% 
  filter(str_count(Word) > 3 , str_detect(Word, " ") == FALSE , freqlemfilms2 > 5 )

# La librairie contient les mots dans toutes leur formes avec leur lemmes rattaché
# Ex: lest mots "mangeons et mangeait ont le lemme manger".
# On supprimme donc les doublons dans cette colonne pour former notre base de donnée de mots
word_lib_distinct = distinct(word_lib, lemme) 

n = nrow(word_lib_distinct)

# On sélectionne aléatoirement 1000 mots dont on scrappera les définitions
# pages wiki et champs lexical sur internet
set.seed(1)
id = sample(1:nrow(word_lib_distinct), size = 1000)
list_mot = word_lib_distinct$lemme[id]

# Récupérations des informations sur internet
deflist = sapply(list_mot, recup_definition)


# On crée une matrice vide avec en colonne les 1000 mots dont on scrapper les informations
# Les lignes sont les mots retenu dans wordlibdistinct. 
mat = matrix(0, ncol = length(deflist), nrow = n, dimnames = list(word_lib_distinct$lemme, list_mot)) %>% 
  data.frame()

dim(mat)

# Nettoyage des définitions
## On crée une fonction qui néttoie un texte en supprimant les information inutile

netoyage = function(corpus = corpus){
  # Supprimer les apostrophe
  corpus2 <- gsub(pattern="\\W",replace=" ",corpus)
  # Supprimer les nombres
  corpus2 <- gsub(pattern="\\d",replace=" ",corpus2)
  # Mettre les textes en minuscules
  corpus2 <- tolower(corpus2)
  # Effacer les mots inutile
  corpus2 <- removeWords(corpus2,stopwords("french"))
  corpus2 = removeWords(corpus2, words = c("synonyme", "contraire"))
  # Supprimer les mots de longueur 1
  corpus2 <- gsub(pattern="\\b[A-z]\\b{1}", replace=" ",corpus2)
  # Se débarasser des espaces vides 
  corpus2 <- stripWhitespace(corpus2)
  # On split le corpus afin d'obtenir un sac de mots
  corpus2 = strsplit(corpus2, " ") %>% unlist()
  corpus2 = corpus2[corpus2 != ""]
  
  # Cette boucle à pour objectif de lemmatiser le corpus. 
  # Les mots aux pluriel, conjugaison, etc, seront remplacer par leur
  # lemmes correspondant
  for (i in 1:length(corpus2)) {
    indice = which(corpus2[i] == word_library$Word) %>% min()
    corpus2[i] = word_library$lemme[indice]
  }
  return(na.omit(corpus2))
}


## Nettoyage de toutes les définitions scraper sur internet 
deflist_netoy = lapply(deflist,FUN = netoyage)

## On procède au remplissage de notre matrice par la fréquence dapparition
## Pour les 5303 mots retenu, on compte le nombre de fois que chaque mot apparait
## dans le sac de mots de chacune des 1000 défintions. 
#start = Sys.time()
for (i in 1: 1000) {
  motslist = deflist_netoy[[i]]
  for(j in 1:n){
    mat[j, i] = mat[j, i] + str_count(motslist, row.names(mat)[j]) %>% sum()
  }
}
#end = Sys.time()

## Après avoir obtenu la matrice rempli on réalise l'ACP pour la reduction de dimension
acp = PCA(mat, scale.unit = TRUE, ncp = 300, graph = FALSE)
## On récupère les coordonnée des mots sur les 100 premières dimension 
coord = acp$ind$coord
coord
library(lsa)

## On calcule la similarité cosine des mots de ces mots grâce au coordonnées
mat_dist = cosine(t(coord))

## On exporte cette matrice 


save(mat_dist, file = "matdist.RData")
