# Contexte
Implémentation personnalisée du jeu Cémantix développée dans le cadre du cours d'Analyse de Données 2. Projet réalisé en équipe de trois, utilisant exclusivement R pour le traitement 
du langage naturel, sans recours aux algorithmes préexistants comme word2vec.

## Méthodologie
Le projet se divise en deux parties principales :

1. Création de la base de données (Algorithme_Cementix.R)
- Utilisation d'une base lexicale de 5303 mots (source : lexique.org)
- Web scraping et collecte de données textuelles :
  - Définitions du dictionnaire Larousse
  - Articles Wikipédia
  - Champs lexicaux depuis rimessolides.com
- Traitement et vectorisation des textes
- Analyse en Composantes Principales (ACP)
- Calcul des similarités cosinus entre les mots

2. Interface de jeu (jeu_cementix.R)
- Fonction principale jedevine() pour l'interaction utilisateur
- Utilisation de la matrice de similarité précalculée (format Rdata)
