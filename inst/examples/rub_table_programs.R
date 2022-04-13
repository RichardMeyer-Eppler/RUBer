# Create example data
example_data <- data.frame(
  stringsAsFactors = FALSE,
  report_nr = c(
    1,
    1,
    1,
    1,
    1,
    1
  ),
  programm = c(
    "Forschendes Lernen",
    "Forschendes Lernen",
    "Forschendes Lernen",
    "Innovative Praxisprojekte",
    "interLECTURE",
    "UNIC@RUB"
  ),
  projekttitel = c(
    "Interkulturelle Bibelhermeneutiken. Kontextuelle Exegese im theologisch-interdisziplin\u00E4ren Diskurs",
    "Migration lokal denken. Begegnungen mit religi\u00F6sen Gemeinschaften in NRW",
    "Traumatheologie: Kritische Bibellekt\u00FCre anhand von Leiderfahrungen.  Entwicklung eines Tagungs- und Seminarkonzepts mit Studierenden zur angewandten wissenschaftlichen Exegese von Bibeltexten.",
    "Religi\u00F6se Pluralit\u00E4t entdecken, reflektieren und diskutierend sichtbar machen",
    "Globale Christent\u00FCmer: Theologische und religionswissenschaftliche Perspektiven",
    "Design of a Virtual Exchange program for the Summer Semester 2022 between the Protestant Faculty of RUB and the Faculty of Theology of the University of Deusto in the frame of the UNIC cooperation"
  ),
  antragsteller_innen_verantwortliche_personen = c(
    "N. N.",
    "N. N.",
    "N. N.",
    "N. N.",
    "N. N.",
    "N. N."
  ),
  forderzeitraum_von = c(
    "2021-10-01",
    "2019-10-01",
    "2021-04-01",
    "2021-04-01",
    "2020-06-01",
    "2022-02-01"
  ),
  forderzeitraum_bis = c(
    "2022-03-31",
    "2020-07-31",
    "2021-12-31",
    "2022-03-31",
    "2021-09-30",
    "2022-07-31"
  ),
  is_last_row = c(
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  )
)

# Function call
rub_table_programs(
  df = example_data
)
