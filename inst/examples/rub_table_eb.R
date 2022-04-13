# Generate example data
df_example <- data.frame(
  stringsAsFactors = FALSE,
  studieneingang = c(
    "Geschlecht","weiblich",
    "m\u00E4nnlich","Abschlussart","Bachelor 1-Fach",
    "Bachelor 2-F\u00E4cher","Staatsexamen","Magister Theologiae",
    "Master 1-Fach","Master 2-F\u00E4cher","Master of Education",
    "F\u00E4chergruppe (erstes Studienfach)","Geisteswissenschaften",
    "Humanmedizin / Gesundheitswissenschaften",
    "Ingenieurwissenschaften","Kunst, Kunstwissenschaft",
    "Mathematik, Naturwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften","Sport",
    "Gesamtzahl angeschriebene Studierende / g\u00FCltige Frageb\u00F6gen",
    "R\u00FCcklaufquote Studieneingangsbefragungen"
  ),
  koepfe_rub = c(
    NA,"7.403","7.416",NA,
    "8.006","3.607","2.259","54","886","7",NA,NA,"3.364",
    "852","3.782","242","2.647","3.668","264","14.819",
    "31%"
  ),
  koepfe_rub_perc = c(
    NA,"50%","50%",NA,"54%",
    "24%","15%","0,4%","6,0%","<0,1%",NA,NA,"23%",
    "5,7%","26%","1,6%","18%","25%","1,8%","100%","31%"
  ),
  koepfe_bef = c(
    NA,"2.769","1.865",NA,
    "2.213","1.232","864","18","303","4",NA,NA,"1.094",
    "397","941","54","868","1.176","104","4.634","31%"
  ),
  koepfe_bef_perc = c(
    NA,"60%","40%",NA,"48%",
    "27%","19%","0,4%","6,5%","<0,1%",NA,NA,"24%",
    "8,6%","20%","1,2%","19%","25%","2,2%","100%","31%"
  ),
  row_id = c(
    1L,2L,3L,4L,5L,6L,7L,8L,
    9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,
    20L,21L
    )
)

# Multi-level headings, see `flextable::set_headers`
typology_example <- structure(
  list(
    col_keys = c(
      "studieneingang", "koepfe_rub", "koepfe_rub_perc", "koepfe_bef", "koepfe_bef_perc"
    ),
    colC = c(
      "Studieneingang", "Studierende im 1. HS (WiSe 18/19 bis  WiSe 20/21)", "Studierende im 1. HS (WiSe 18/19 bis  WiSe 20/21)", "Studierende im 1. HS (WiSe 18/19 bis  WiSe 20/21)", "Studierende im 1. HS (WiSe 18/19 bis  WiSe 20/21)"
    ),
    colB = c(
      "Studieneingang", "Angeschrieben", "Angeschrieben", "G\u00FCltige Frageb\u00F6gen", "G\u00FCltige Frageb\u00F6gen"
    ),
    colA = c(
      "Studieneingang", "K\u00F6pfe", "(in %)", "K\u00F6pfe", "(in %)"
    )
  ),
  class = c("data.frame"),
  row.names = c(NA, -5L)
)

# Text for rows that receive special formatting
headings_example <- c(
  "Geschlecht", "Abschlussart", "F\u00E4chergruppe (erstes Studienfach)", "Gesamtzahl angeschriebene Studierende / g\u00FCltige Frageb\u00F6gen", "R\u00FCcklaufquote Studieneingangsbefragungen"
)

# Function call
rub_table_eb(
  df = df_example,
  typology = typology_example,
  headings = headings_example
)
