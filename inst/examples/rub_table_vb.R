# Create example data
studienverlauf <- data.frame(
  stringsAsFactors = FALSE,
  studienverlauf = c(
    "Geschlecht", "weiblich", "männlich", "Abschlussart", "Bachelor 1-Fach", "Bachelor 2-Fächer", "Magister Theologiae", "Staatsexamen", "Master 1-Fach", "Master 2-Fächer", "Master of Education", "Fächergruppe (erstes Studienfach)", "Geisteswissenschaften", "Humanmedizin / Gesundheitswissenschaften", "Ingenieurwissenschaften", "Kunst, Kunstwissenschaft", "Mathematik, Naturwissenschaften", "Rechts-, Wirtschafts- und Sozialwissenschaften", "Sport", "Gesamtzahl angeschriebene Studierende / gültige Fragebögen", "Rücklaufquote Studienverlaufsbefragungen"
  ),
  koepfe_2fs_rub = c(
    NA,2449,2450,NA,NA,NA,NA,NA,3993,160,746,NA,1560,43,1086,68,1302,700,140,4899,0.33
  ),
  koepfe_2fs_rub_perc = c(
    NA,0.5,0.5,NA,NA,NA,NA,NA,0.82,0.03,0.15,NA,0.32,0.01,0.22,0.01,0.27,0.14,0.03,1,0.33
  ),
  koepfe_2fs_bef = c(
    NA,880,747,NA,NA,NA,NA,NA,1289,66,272,NA,516,17,297,25,506,223,43,1627,0.33
  ),
  koepfe_2fs_bef_perc = c(
    NA,0.54,0.46,NA,NA,NA,NA,NA,0.79,0.04,0.17,NA,0.32,0.01,0.18,0.02,0.31,0.14,0.03,1,0.33
  ),
  koepfe_5fs_rub = c(
    NA,5846,5832,NA,6181,3320,73,2102,NA,NA,NA,NA,3542,942,1691,178,2477,2550,298,11678,0.32
  ),
  koepfe_5fs_rub_perc = c(
    NA,0.5,0.5,NA,0.53,0.28,0.01,0.18,NA,NA,NA,NA,0.3,0.08,0.14,0.02,0.21,0.22,0.03,1,0.32
  ),
  koepfe_5fs_bef = c(
    NA,1564,2172,NA,1848,1141,24,723,NA,NA,NA,NA,1203,332,457,49,842,774,79,3736,0.32
  ),
  koepfe_5fs_bef_perc = c(
    NA,0.58,0.42,NA,0.49,0.31,0.01,0.19,NA,NA,NA,NA,0.32,0.09,0.12,0.01,0.23,0.21,0.02,1,0.32
  ),
  row_id = c(
    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
  )
)

typology <- data.frame(
  stringsAsFactors = FALSE,
  NA,
  col_keys = c(
    "studienverlauf","koepfe_2fs_rub","koepfe_2fs_rub_perc","koepfe_2fs_bef","koepfe_2fs_bef_perc","koepfe_5fs_rub","koepfe_5fs_rub_perc","koepfe_5fs_bef","koepfe_5fs_bef_perc"
  ),
  colD = c(
    "Studienverlauf","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)","Studierende (WiSe 18/19 bis  WiSe 20/21)"
  ),
  colC = c(
    "Studienverlauf","2. Fachsemester","2. Fachsemester","2. Fachsemester","2. Fachsemester","5. Fachsemester","5. Fachsemester","5. Fachsemester","5. Fachsemester"
  ),
  colB = c(
    "Studienverlauf","Angeschrieben","Angeschrieben","Gültige Fragebögen","Gültige Fragebögen","Angeschrieben","Angeschrieben","Gültige Fragebögen","Gültige Fragebögen"
  ),
  colA = c(
    "Studienverlauf","Köpfe","(in %)","Köpfe","(in %)","Köpfe","(in %)","Köpfe","(in %)"
  )
)

headings <- c(
  "Geschlecht", "Abschlussart", "Fächergruppe (erstes Studienfach)", "Gesamtzahl angeschriebene Studierende / gültige Fragebögen", "Rücklaufquote Studienverlaufsbefragungen"
)

# Function call
rub_table_vb(
  df = studienverlauf,
  typolog = typology,
  headings = headings
)
