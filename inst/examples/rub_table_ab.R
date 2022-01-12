# Create example data
studienabschluss <- data.frame(
  stringsAsFactors = FALSE,
  studienabschluss = c(
    "Geschlecht","weiblich","männlich","Abschlussart","Bachelor 1-Fach","Bachelor 2-Fächer","Magister Theologiae","Master 1-Fach","Master 2-Fächer","Master of Education","Staatsexamen","Fächergruppe (erstes Studienfach)","Geisteswissenschaften","Humanmedizin / Gesundheitswissenschaften","Ingenieurwissenschaften","Kunst, Kunstwissenschaft","Mathematik, Naturwissenschaften","Rechts-, Wirtschafts- und Sozialwissenschaften","Sport","Gesamtzahl angeschriebene Absolvent:innen / gültige Fragebögen","Rücklaufquote Absolvent:innenbefragungen"
  ),
  koepfe_rub = c(
    NA,10073,10042,NA,6763,3164,30,6190,404,1856,1708,NA,5776,933,4035,265,4358,4021,727,20115,0.35
  ),
  koepfe_rub_perc = c(
    NA,0.5,0.5,NA,0.34,0.16,0,0.31,0.02,0.09,0.08,NA,0.29,0.05,0.2,0.01,0.22,0.2,0.04,1,0.35
  ),
  koepfe_bef = c(
    NA,3645,3391,NA,2672,1352,11,1928,162,492,413,NA,2161,201,1321,108,1821,1214,204,7030,0.35
  ),
  koepfe_bef_perc = c(
    NA,0.52,0.48,NA,0.38,0.19,0,0.27,0.02,0.07,0.06,NA,0.31,0.03,0.19,0.02,0.26,0.17,0.03,1,0.35
  ),
  row_id = c(
    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
  )
)

typology <- data.frame(
  stringsAsFactors = FALSE,
  col_keys = c(
    "studienabschluss","koepfe_rub","koepfe_rub_perc","koepfe_bef","koepfe_bef_perc"
  ),
  colC = c(
    "Studienabschluss","Absolventinnen und Absolventen (Prüfungsjahrgänge 2016 bis 2019)","Absolventinnen und Absolventen (Prüfungsjahrgänge 2016 bis 2019)","Absolventinnen und Absolventen (Prüfungsjahrgänge 2016 bis 2019)","Absolventinnen und Absolventen (Prüfungsjahrgänge 2016 bis 2019)"
  ),
  colB = c(
    "Studienabschluss","Angeschrieben","Angeschrieben","Gültige Fragebögen","Gültige Fragebögen"
  ),
  colA = c(
    "Studienabschluss","Köpfe","(in %)","Köpfe","(in %)"
  )
)

headings <- c(
  "Geschlecht", "Abschlussart", "Fächergruppe (erstes Studienfach)", "Gesamtzahl angeschriebene Absolvent:innen / gültige Fragebögen", "Rücklaufquote Absolvent:innenbefragungen"
)

# Function call
rub_table_ab(
  df = studienabschluss,
  typolog = typology,
  headings = headings
)
