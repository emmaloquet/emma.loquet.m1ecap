#' Filtrer les medailles par discipline et pays
#'
#' @param data data.frame des résultats
#' @param sport discipline sportive à filtrer
#' @param country pays (NOC) à filtrer
#'
#' @return data.frame filtre
#' @export
filtrer_medailles <- function(data, sport, country) {
  data[data$Competitions == sport & data$NOC == country, ]
}


#' Calcul du total de medailles par pays
#'
#' @param data data.frame des resultats
#'
#' @return data.frame agrege
#' @importFrom stats aggregate
#' @export
total_medailles_pays <- function(data) {
  aggregate(Total ~ NOC, data = data, sum)
}

#' Graphique des medailles par pays
#'
#' @param data data.frame des resultats
#' @param top_n nombre de pays affiches
#' @return un graphique ggplot
#' @import ggplot2
#' @importFrom utils head
#' @importFrom stats reorder
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
      y = "Total des medailles",
      title = "Medailles par pays"
    )
}
