# Get formatted flextable of excluded programs

Get formatted flextable of excluded programs

## Usage

``` r
rub_table_excluded_programs(df)
```

## Arguments

- df:

  Data frame

## Value

Formatted flextable

## Illustrations

![](figures/rub_table_excluded_programs.png)

## Examples

``` r
df_excluded_programs <- data.frame(
  stringsAsFactors = FALSE,
  head1 = c(
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "auslaufend",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Sonderfall",
    "Studienf\u00E4lle im 3. Fach",
    "Studienf\u00E4lle im 3. Fach",
    "weiterbildend",
    "weiterbildend",
    "weiterbildend",
    "weiterbildend",
    "weiterbildend",
    "weiterbildend",
    "zu neu / zu wenig F\u00E4lle",
    "zu neu / zu wenig F\u00E4lle",
    "zu neu / zu wenig F\u00E4lle",
    "zu neu / zu wenig F\u00E4lle",
    "zu neu / zu wenig F\u00E4lle"
  ),
  head2 = c(
    "Geisteswissenschaften",
    "Geisteswissenschaften",
    "Geisteswissenschaften",
    "Humanmedizin / Gesundheitswissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Sport",
    "Geisteswissenschaften",
    "Geisteswissenschaften",
    "Ingenieurwissenschaften",
    "Mathematik, Naturwissenschaften",
    "Mathematik, Naturwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Ingenieurwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Rechts-, Wirtschafts-, Sozialwissenschaften",
    "Geisteswissenschaften",
    "Geisteswissenschaften",
    "Ingenieurwissenschaften",
    NA,
    NA
  ),
  head3 = c(
    "Linguistik (Ba 2-F, Ma 1-F, Ma 2-F)",
    "Ostslavistik (Ba 2-F)",
    "Koreanistik (Ma 1-F)",
    "Vorklinische Medizin (Modellstudiengang) (S)",
    "Economics (Ma 1-F)",
    "Management (Ma 1-F)",
    "Sozialpsychologie und -anthropologie (Ba 2-F, Ma 1-F, Ma 2-F)",
    "Soziologie (Ba 2-F, Ma 1-F, Ma 2-F)",
    "Politikwissenschaft (Ba 2-F, Ma 1-F, Ma 2-F)",
    "Psychologie (Ma 1-F)",
    "Wirtschaftspsychologie (Ba 1-F)",
    "Sportwissenschaft (Ma 1-F)",
    "History, Philosophy and Culture of Science (HPS+) (Ma 1-F)",
    "Empirische Mehrsprachigkeitsforschung (Ma 1-F)",
    "Mechanical Engineering (Ba 1-F)",
    "Transformation of Urban Landscapes (Ma 1-F)",
    "Molecular Science - Spectroscopy and Simulation (Ma 1-F)",
    "Econometrics (Ma 1-F)",
    "European Master\u2019s Programme in Human Rights and Democratisation (E.MA) (Ma 1-F)",
    "Economics and Business Administration (Ma 1-F)",
    "Erziehungswissenschaft (M.Ed.)",
    "Bildungswissenschaften (M.Ed.)",
    "Applied IT Security (Ma 1-F)",
    "Wirtschafts- und Steuerrecht (Ma 1-F)",
    "Deutsches, T\u00FCrkisches u. Int. Wirtschaftsrecht (Ma 1-F)",
    "Kriminologie, Kriminalistik und Polizeiwissenschaft (Ma 1-F)",
    "Accounting and Auditing (Ma 1-F)",
    "Human Resource Management (Ma 1-F)",
    "VAMoS: Computer-, Psycho- und Theoretische Linguistik (Ba 2-F, Ma 1-F, Ma 2-F)",
    "Sprachen und Kulturen Ostasiens (Ba 1-F, Ma 1-F)",
    "Materialwissenschaft (Ba 1-F)",
    "Linguistic Data Science (Ma 1-F)",
    "Subsurface Engineering (Ma 1-F)"
  )
)

RUBer::rub_table_excluded_programs(
  df_excluded_programs
)


.cl-c2cc6884{}.cl-c2c4930c{font-family:'RUB Scala TZ';font-size:9pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 53, 96, 1.00);background-color:transparent;}.cl-c2c49316{font-family:'RUB Scala TZ';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 53, 96, 1.00);background-color:transparent;}.cl-c2c7b762{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c2c7df08{width:1.1in;background-color:rgba(209, 223, 159, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df12{width:2.15in;background-color:rgba(209, 223, 159, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df13{width:3.63in;background-color:rgba(209, 223, 159, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df1c{width:1.1in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df1d{width:2.15in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df26{width:3.63in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df27{width:1.1in;background-color:rgba(236, 236, 236, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df28{width:2.15in;background-color:rgba(236, 236, 236, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df30{width:3.63in;background-color:rgba(236, 236, 236, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df3a{width:1.1in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df3b{width:2.15in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df44{width:3.63in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(215, 215, 215, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df4e{width:1.1in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df58{width:2.15in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2c7df59{width:3.63in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(215, 215, 215, 1.00);border-left: 1pt solid rgba(141, 174, 16, 1.00);border-right: 1pt solid rgba(141, 174, 16, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Ausschlussgrund
```
