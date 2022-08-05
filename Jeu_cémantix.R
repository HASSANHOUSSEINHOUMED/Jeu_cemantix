## Chargement de la matrice des scores
load("matdist.RData")

## chargement des package nécéssaire au fonctionnement du jeu

library(tidyverse)

liste = rownames(mat_dist)
liste = liste[str_count(liste) > 3 ]


#sort(mat_dist[,"guerre"], decreasing = T) %>% view()

# On a la matrice qui contient les similarités entre les mots
# On crée une fonction qui permet de jouer 

##################### BON A SAVOIR ######################################################
## Pour jouer au jeu il suffit d'importer la matrice des score.                        ##
## Il est possible d'entrer en argument de la fonction le mot clé qu'on veut déviner   ##
## Si aucun mot n'est entrer la fonction choisira aléatoirement.                       ##
## Les mots choisis aléatoirement ne sont pas forcément des mots courant donc difficile##
## à trouver                                                                           ##
#########################################################################################


jedevine = function(mot_cle = " ") {
  
  cat(" Le but du jeu est de trouver le mot secret en essayant de s’en approcher le plus possible contextuellement. Chaque mot se voit attribuer un score dont des valeurs intéressantes sont données en légende à gauche. Si votre mot se trouve dans les 1000 mots les plus proches, un indice de progression gradué de 1 à 1000
Je viens de choisir le mot secret \n Vous pouvez quitter en appuyant sur q")
  
  # on choisit un mot clé aléatoire, si vous n'avez pas préciser un mot
  if (mot_cle == " ") mot_cle = sample(liste, size = 1)
  #mot_cle = "guerre"
  
  # matrice des scores du mot cle
  scores = as.matrix(sort(mat_dist[mot_cle, ], decreasing = T))
  scores = scores*100
  
  # les 1000 mots les plus proches du mot clé
  motsproche = rownames(scores)[1:1000]
  mot_user = " "
  
  # initialisation du tableau qu'on affichera
  df_user = data.frame(`N°` = NA, Mot =NA, Score = NA, Progression = NA)
  # nombre de coup
  n = 0
  while (mot_cle != mot_user) {
    
    mot_user = readline("Entrer le mot : ")
    
    # si le mot entrée n'est pas dans notre base de données
    if( (any(mot_user == rownames(scores)) == FALSE) && (mot_user != "q")) {
      cat("Je ne connais pas ce mot")} 
    else 
      # si vous re-entré le mot, on l'affichera qu'une seule fois
      if (mot_user %in% df_user$Mot){
        print(df_user)
      }
    else
      # si vous entré q, vous abandonner le jeu
      if(mot_user == "q"){
        break
      }
    else
    {
      # sinon on rempli notre data frame qui sera affiché
      n = n+1
      df_user[n, 1] = n
      df_user[n, 2] = mot_user
      df_user[n, 3] = scores[mot_user, ]
      df_user[n, 4] = ifelse(mot_user %in% motsproche, 1000 - which(mot_user == motsproche), " ")
      df_user = df_user %>% arrange(Score)
      print(df_user)
    }
    
  }
  # si vous avez gagné
  if(mot_user == mot_cle){
    cat("\n Félicitation, vous avez trouvez le mot", mot_cle, "en", n,"coups")
    decision = readline("Pour afficher les 100 mots les plus proches, taper oui :")
    if(decision == "oui") print(as.matrix(sort((scores[1:100, ]))))
  }else{
    cat("\n Vous n'avez pas trouvez le mot clé. Le mot était", mot_cle)
    decision = readline("Pour afficher les 100 mots les plus proches, taper oui :")
    if(decision == "oui") print(as.matrix(sort((scores[1:100, ]))))
  }
}

## Par exemple vous pouvez tester avec le mot guerre

jedevine("guerre")

## Ou pour deviner un mot aléatoire que vous pouvez trouver a la fin en abandonnant

jedevine()
