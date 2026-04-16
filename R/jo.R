#' Filtrer les médailles par discipline et pays
#'
#' @param data data.frame des résultats olympiques
#' @param sport discipline sportive à filtrer
#' @param country pays (NOC) à filtrer
#'
#' @return data.frame filtré
#' @export
filtrer_medailles <- function(data, sport, country) {
  data[data$Competitions == sport & data$NOC == country, ]
}


#' Calcul du total de médailles par pays
#'
#' @param data data.frame des résultats
#'
#' @return data.frame agrégé
#' @export
total_medailles_pays <- function(data) {
  aggregate(Total ~ NOC, data = data, sum)
}

#' Graphique des médailles par pays
#'
#' @param data data.frame des résultats olympiques
#' @param top_n nombre de pays affichés
#' @return un graphique ggplot
#' @import ggplot2
#' @export
plot_medailles_pays <- function(data, top_n = 10) {

  df <- total_medailles_pays(data)

  df <- df[order(df$Total, decreasing = TRUE), ]
  df <- head(df, top_n)

  ggplot(df, aes(x = reorder(NOC, Total), y = Total)) +
    geom_col() +
    coord_flip() +
    labs(
      x = "Pays",
      y = "Total des médailles",
      title = "Médailles par pays"
    )
}
