# Create example data
studienabschluss <- data.frame(
  stringsAsFactors = FALSE,
  studienabschluss = c("Geschlecht","weiblich",
                       "männlich","Abschlussart","Bachelor 1-Fach",
                       "Bachelor 2-Fächer","Magister Theologiae","Master 1-Fach",
                       "Master 2-Fächer","Staatsexamen",
                       "Master of Education","Fächergruppe (erstes Studienfach)",
                       "Geisteswissenschaften",
                       "Humanmedizin / Gesundheitswissenschaften","Ingenieurwissenschaften",
                       "Kunst, Kunstwissenschaft","Mathematik, Naturwissenschaften",
                       "Rechts-, Wirtschafts-, Sozialwissenschaften","Sport",
                       "Gesamtzahl angeschriebene Absolvent:innen / gültige Fragebögen",
                       "Rücklaufquote Absolvent:innenbefragungen"),
  koepfe_rub = c(NA,"9.887","9.969",NA,
                 "6.673","3.164","30","6.081","404","1.708",
                 "1.796",NA,"4.611","882","4.778","265","3.614",
                 "5.086","620","19.856","35%"),
  koepfe_rub_perc = c(NA,"50%","50%",NA,
                      "34%","16%","0,2%","31%","2,0%","8,6%","9,0%",NA,
                      "23%","4,4%","24%","1,3%","18%","26%","3,1%",
                      "100%","35%"),
  koepfe_bef = c(NA,"3.594","3.358",NA,
                 "2.643","1.352","11","1.897","162","413","474",
                 NA,"1.724","186","1.603","108","1.544","1.613",
                 "174","6.952","35%"),
  koepfe_bef_perc = c(NA,"52%","48%",NA,
                      "38%","19%","0,2%","27%","2,3%","5,9%","6,8%",NA,
                      "25%","2,7%","23%","1,6%","22%","23%","2,5%",
                      "100%","35%"),
  row_id = c(1L,2L,3L,4L,5L,6L,
             7L,8L,9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,
             19L,20L,21L)
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
  "Geschlecht", "Abschlussart", "F\u00E4chergruppe (erstes Studienfach)", "Gesamtzahl angeschriebene Absolvent:innen / gültige Frageb\u00F6gen", "R\u00FCcklaufquote Absolvent:innenbefragungen"
)

# Function call
rub_table_ab(
  df = studienabschluss,
  typolog = typology,
  headings = headings
)
