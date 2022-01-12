# Example data
df_metrics <- data.frame(
  stringsAsFactors = FALSE,
  col1 = c(
    "D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-pie-line-graph@48x48.png","Drei Befragungen: Absolvent:innenbefragung (8.297 Fälle), Studieneingangsbefragung (5.283 Fälle) und Studienverlaufsbefragung (5.912 Fälle)","D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-list-numbers@48x48.png",NA,"D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-user-female-teacher-math@48x48.png",NA
  ),
  col2 = c(
    "Insgesamt 66 Datenreporte für 177 Studiengänge","Drei Befragungen: Absolvent:innenbefragung (8.297 Fälle), Studieneingangsbefragung (5.283 Fälle) und Studienverlaufsbefragung (5.912 Fälle)","Auswertung von 52 Fragen und 251 Items",NA,"134 Lehrförderungen verteilt auf 217 Antragssteller:innen","Hochschulstatistische Daten und Kohortenanalysen zu 46.516 Studienfällen und 5.684 Absolvent:innen"
  ),
  col3 = c(
    "Insgesamt 66 Datenreporte für 177 Studiengänge","Drei Befragungen: Absolvent:innenbefragung (8.297 Fälle), Studieneingangsbefragung (5.283 Fälle) und Studienverlaufsbefragung (5.912 Fälle)","Auswertung von 52 Fragen und 251 Items", "901.433 ausgewertete Antworten visualisiert in 4.995 Abbildungen","134 Lehrförderungen verteilt auf 217 Antragssteller:innen","Hochschulstatistische Daten und Kohortenanalysen zu 46.516 Studienfällen und 5.684 Absolvent:innen"
  ),
  col4 = c(
    "Insgesamt 66 Datenreporte für 177 Studiengänge","Drei Befragungen: Absolvent:innenbefragung (8.297 Fälle), Studieneingangsbefragung (5.283 Fälle) und Studienverlaufsbefragung (5.912 Fälle)","Auswertung von 52 Fragen und 251 Items","901.433 ausgewertete Antworten visualisiert in 4.995 Abbildungen","134 Lehrförderungen verteilt auf 217 Antragssteller:innen","Hochschulstatistische Daten und Kohortenanalysen zu 46.516 Studienfällen und 5.684 Absolvent:innen"
  ),
  col5 = c(
    NA,"Drei Befragungen: Absolvent:innenbefragung (8.297 Fälle), Studieneingangsbefragung (5.283 Fälle) und Studienverlaufsbefragung (5.912 Fälle)",NA,"901.433 ausgewertete Antworten visualisiert in 4.995 Abbildungen","134 Lehrförderungen verteilt auf 217 Antragssteller:innen","Hochschulstatistische Daten und Kohortenanalysen zu 46.516 Studienfällen und 5.684 Absolvent:innen"
  ),
  col6 = c(
    NA,"D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-team-meeting-message-men-question@48x48.png",NA,"D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-analytics-bars-horizontal@48x48.png",NA,"D:/Data/Documents/R/win-library/4.1/Datenreport2022/rmarkdown/templates/datenreport-2022/skeleton/streamline-icon-people-man-graduate@48x48.png"
  )
)

# Function call
rub_table_metrics(
  df_metrics
)
