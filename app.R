# Fichier : app.R
# Point d'entrée pour Posit Connect

# -- Étape 1 : Installation du package (inchangée) --
# On s'assure que 'remotes' est disponible
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
# On installe le package local (votre application)
remotes::install_local(upgrade = 'never')

# -- Étape 2 : Lancement de l'application (modifiée) --

# On charge le package pour rendre ses fonctions ET SES DONNÉES disponibles
library(app.trustgame.appendix)

# On peut maintenant lancer l'application directement,
# sans utiliser l'opérateur `::`
run_app()
