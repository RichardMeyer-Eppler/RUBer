# Example data
df_item <- data.frame(
  stringsAsFactors = FALSE,
  figure_caption = c(
    "Beurteilung der Studienangebote und -bedingungen - Kontakte","Fachbezogene Englischkenntnisse","Fachbezogene Englischkenntnisse","Auslandsaufenthalt während des Studiums","Beurteilung der Prüfungssituation","Beurteilung der Studienangebote und -bedingungen - Kontakte","Beurteilung der Studienangebote und -bedingungen - Struktur des Studiums","Beurteilung der Studienangebote und -bedingungen - Kontakte"
  ),
  facet = c(
    "Kontakte zu Lehrenden","Vorbereitung auf englischsprachige Fachkommunikation","Vorbereitung auf den Umgang mit englischsprachiger Literatur","Auslandsaufenthalt während des Studiums","Ausgewogenheit der Prüfungsformen (Klausur, Hausarbeit, Projekte, mündliche Prüfungen etc.).","Kontakte zu Mitstudierenden","Zugang zu erforderlichen Lehrveranstaltungen (z. B. Seminare, Übungen)","Kontakte zu Lehrenden"
  ),
  y = c(
    "Bachelor 2-Fächer","Bachelor 2-Fächer","Bachelor 2-Fächer","Bachelor 2-Fächer","Master of Education","Master of Education","Master of Education","Master of Education"
  ),
  aggregation_sort_1 = c(
    2, 2, 2, 2, 55, 55, 55, 55
  ),
  mean = c(
    1.71428571428571,4.19148936170213,3.9375,1.78,1.65,1.38095238095238,1.76190476190476,1.85714285714286
  ),
  sd = c(
    0.816496580927726,0.924034110803191,1.13748977082144,0.41845195759648,0.875093979915421,0.669043382464133,0.830948969838817,0.963624111659432
  ),
  mean_fgr = c(
    2.34067207415991,3.4128157732594,3.15811965811966,1.5090395480226,2.69263157894737,2.29400386847195,2.67251461988304,2.56031128404669
  ),
  sd_fgr = c(
    1.08899230654153,1.38010646489644,1.43054364002476,0.500059559613556,1.08996594469921,1.07770878477964,1.07799587645275,1.12444948643891
  ),
  mean_delta = c(
    0.626386359874193,0.778673588442731,0.779380341880342,0.270960451977401,1.04263157894737,0.913051487519573,0.910609857978279,0.703168426903835
  ),
  distance = c(
    0.575198149804654,0.564212695359818,0.544814097294407,0.541856358444179,0.956572619555646,0.84721540773769,0.844724806345951,0.625344611193467
  )
)

# Function call
rub_table_item(
  df = df_item
)
