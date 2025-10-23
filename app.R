# Fichier : app.R
# Point d'entrée pour Posit Connect

# -- Étape 1 : Installation du package --
# On s'assure que le package 'remotes' est disponible
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# On installe le package local (votre application) depuis le code source.
# C'est l'équivalent de "Build > Install and Restart" dans RStudio,
# mais exécuté sur le serveur de Posit Connect.
remotes::install_local(upgrade = 'never')

# -- Étape 2 : Lancement de l'application --
# Maintenant que le package est bien installé, on le charge
# et on exécute la fonction qui lance l'application Shiny.
app.trustgame.appendix::run_app()
