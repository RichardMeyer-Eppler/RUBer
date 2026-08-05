# Formatting for the reference docx document

``` r

library(dplyr)
library(officer)
```

## The docx reference document

As explained in [section 3.4 of R Markdown: The Definitive
Guide](https://bookdown.org/yihui/rmarkdown/word-document.html), the
first step in creating a reference document is to produce a .docx file
using rmarkdown or Pandoc. The formatting and layout of this document
can then be edited and saved. The resulting reference document is then
used in the YAML metadata of each R Markdown file.

This vignette is intended to track all the changes made to the
rub_reference.docx file, so that these changes are reproducible and
documented.

We can use
[`officer::styles_info`](https://davidgohel.github.io/officer/reference/styles_info.html)
to extract all custom styles defined in the reference document:

``` r

reference_docx <- system.file(
  "rmarkdown",
  "templates",
  "datenreport-2022",
  "skeleton",
  "rub_reference_2021.docx",
  package = "RUBer"
)

x <- officer::read_docx(
  reference_docx
)

x %>% 
officer::styles_info() %>% 
  dplyr::filter(
    is_custom
  ) %>% 
  dplyr::arrange(
    style_type,
    style_name
  ) %>% 
  head(50L)
#>    style_type             style_id            style_name
#> 1   character             AlertTok              AlertTok
#> 2   character        AnnotationTok         AnnotationTok
#> 3   character         AttributeTok          AttributeTok
#> 4   character             BaseNTok              BaseNTok
#> 5   character     BeschriftungZchn     Beschriftung Zchn
#> 6   character           BuiltInTok            BuiltInTok
#> 7   character              CharTok               CharTok
#> 8   character           CommentTok            CommentTok
#> 9   character        CommentVarTok         CommentVarTok
#> 10  character          ConstantTok           ConstantTok
#> 11  character       ControlFlowTok        ControlFlowTok
#> 12  character          DataTypeTok           DataTypeTok
#> 13  character            DecValTok             DecValTok
#> 14  character     DocumentationTok      DocumentationTok
#> 15  character             ErrorTok              ErrorTok
#> 16  character         ExtensionTok          ExtensionTok
#> 17  character             FloatTok              FloatTok
#> 18  character          FunctionTok           FunctionTok
#> 19  character          FuzeileZchn         Fußzeile Zchn
#> 20  character            ImportTok             ImportTok
#> 21  character       InformationTok        InformationTok
#> 22  character           KeywordTok            KeywordTok
#> 23  character        KopfzeileZchn        Kopfzeile Zchn
#> 24  character            NormalTok             NormalTok
#> 25  character          OperatorTok           OperatorTok
#> 26  character             OtherTok              OtherTok
#> 27  character      PreprocessorTok       PreprocessorTok
#> 28  character      RegionMarkerTok       RegionMarkerTok
#> 29  character       SpecialCharTok        SpecialCharTok
#> 30  character     SpecialStringTok      SpecialStringTok
#> 31  character SprechblasentextZchn Sprechblasentext Zchn
#> 32  character            StringTok             StringTok
#> 33  character        TextkrperZchn       Textkörper Zchn
#> 34  character          VariableTok           VariableTok
#> 35  character         VerbatimChar         Verbatim Char
#> 36  character    VerbatimStringTok     VerbatimStringTok
#> 37  character           WarningTok            WarningTok
#> 38  paragraph             Abstract              Abstract
#> 39  paragraph               Author                Author
#> 40  paragraph      CaptionedFigure      Captioned Figure
#> 41  paragraph              Compact               Compact
#> 42  paragraph           Definition            Definition
#> 43  paragraph       DefinitionTerm       Definition Term
#> 44  paragraph               Figure                Figure
#> 45  paragraph       FirstParagraph       First Paragraph
#> 46  paragraph         ImageCaption         Image Caption
#> 47  paragraph           SourceCode           Source Code
#> 48  paragraph         TableCaption         Table Caption
#> 49      table                Table                 Table
#>                      base_on is_custom is_default  align keep_next line_spacing
#> 1               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 2               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 3               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 4               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 5  Absatz-Standardschriftart      TRUE      FALSE   <NA>     FALSE           NA
#> 6               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 7               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 8               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 9               VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 10              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 11              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 12              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 13              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 14              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 15              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 16              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 17              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 18              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 19 Absatz-Standardschriftart      TRUE      FALSE   <NA>     FALSE           NA
#> 20              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 21              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 22              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 23 Absatz-Standardschriftart      TRUE      FALSE   <NA>     FALSE           NA
#> 24              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 25              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 26              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 27              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 28              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 29              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 30              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 31 Absatz-Standardschriftart      TRUE      FALSE   <NA>     FALSE           NA
#> 32              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 33 Absatz-Standardschriftart      TRUE      FALSE   <NA>     FALSE           NA
#> 34              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 35          BeschriftungZchn      TRUE      FALSE   <NA>     FALSE           NA
#> 36              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 37              VerbatimChar      TRUE      FALSE   <NA>     FALSE           NA
#> 38                  Standard      TRUE      FALSE   <NA>      TRUE           NA
#> 39                      <NA>      TRUE      FALSE center      TRUE           NA
#> 40                    Figure      TRUE      FALSE   <NA>      TRUE           NA
#> 41                 Textkrper      TRUE      FALSE   left     FALSE           NA
#> 42                  Standard      TRUE      FALSE   <NA>     FALSE           NA
#> 43                  Standard      TRUE      FALSE   <NA>      TRUE           NA
#> 44                  Standard      TRUE      FALSE   <NA>     FALSE           NA
#> 45                 Textkrper      TRUE      FALSE   <NA>     FALSE           NA
#> 46              Beschriftung      TRUE      FALSE   <NA>      TRUE           NA
#> 47                  Standard      TRUE      FALSE   left     FALSE           NA
#> 48              Beschriftung      TRUE      FALSE   <NA>      TRUE           NA
#> 49                      <NA>      TRUE      FALSE   <NA>     FALSE           NA
#>    padding.bottom padding.top padding.left padding.right shading.color.par
#> 1            <NA>        <NA>         <NA>          <NA>              <NA>
#> 2            <NA>        <NA>         <NA>          <NA>              <NA>
#> 3            <NA>        <NA>         <NA>          <NA>              <NA>
#> 4            <NA>        <NA>         <NA>          <NA>              <NA>
#> 5            <NA>        <NA>         <NA>          <NA>              <NA>
#> 6            <NA>        <NA>         <NA>          <NA>              <NA>
#> 7            <NA>        <NA>         <NA>          <NA>              <NA>
#> 8            <NA>        <NA>         <NA>          <NA>              <NA>
#> 9            <NA>        <NA>         <NA>          <NA>              <NA>
#> 10           <NA>        <NA>         <NA>          <NA>              <NA>
#> 11           <NA>        <NA>         <NA>          <NA>              <NA>
#> 12           <NA>        <NA>         <NA>          <NA>              <NA>
#> 13           <NA>        <NA>         <NA>          <NA>              <NA>
#> 14           <NA>        <NA>         <NA>          <NA>              <NA>
#> 15           <NA>        <NA>         <NA>          <NA>              <NA>
#> 16           <NA>        <NA>         <NA>          <NA>              <NA>
#> 17           <NA>        <NA>         <NA>          <NA>              <NA>
#> 18           <NA>        <NA>         <NA>          <NA>              <NA>
#> 19           <NA>        <NA>         <NA>          <NA>              <NA>
#> 20           <NA>        <NA>         <NA>          <NA>              <NA>
#> 21           <NA>        <NA>         <NA>          <NA>              <NA>
#> 22           <NA>        <NA>         <NA>          <NA>              <NA>
#> 23           <NA>        <NA>         <NA>          <NA>              <NA>
#> 24           <NA>        <NA>         <NA>          <NA>              <NA>
#> 25           <NA>        <NA>         <NA>          <NA>              <NA>
#> 26           <NA>        <NA>         <NA>          <NA>              <NA>
#> 27           <NA>        <NA>         <NA>          <NA>              <NA>
#> 28           <NA>        <NA>         <NA>          <NA>              <NA>
#> 29           <NA>        <NA>         <NA>          <NA>              <NA>
#> 30           <NA>        <NA>         <NA>          <NA>              <NA>
#> 31           <NA>        <NA>         <NA>          <NA>              <NA>
#> 32           <NA>        <NA>         <NA>          <NA>              <NA>
#> 33           <NA>        <NA>         <NA>          <NA>              <NA>
#> 34           <NA>        <NA>         <NA>          <NA>              <NA>
#> 35           <NA>        <NA>         <NA>          <NA>              <NA>
#> 36           <NA>        <NA>         <NA>          <NA>              <NA>
#> 37           <NA>        <NA>         <NA>          <NA>              <NA>
#> 38            300         300         <NA>          <NA>              <NA>
#> 39           <NA>        <NA>         <NA>           567              <NA>
#> 40              0         240         <NA>          <NA>              <NA>
#> 41             36          36         <NA>          <NA>              <NA>
#> 42           <NA>        <NA>         <NA>          <NA>              <NA>
#> 43              0        <NA>         <NA>          <NA>              <NA>
#> 44           <NA>        <NA>         <NA>          <NA>              <NA>
#> 45           <NA>        <NA>         <NA>          <NA>              <NA>
#> 46           <NA>        <NA>         1701          <NA>              <NA>
#> 47           <NA>        <NA>         <NA>          <NA>            E7E7E7
#> 48           <NA>         240         1276          <NA>              <NA>
#> 49           <NA>        <NA>         <NA>          <NA>              <NA>
#>    border.bottom.width border.bottom.color border.bottom.style border.top.width
#> 1                   NA                <NA>                <NA>               NA
#> 2                   NA                <NA>                <NA>               NA
#> 3                   NA                <NA>                <NA>               NA
#> 4                   NA                <NA>                <NA>               NA
#> 5                   NA                <NA>                <NA>               NA
#> 6                   NA                <NA>                <NA>               NA
#> 7                   NA                <NA>                <NA>               NA
#> 8                   NA                <NA>                <NA>               NA
#> 9                   NA                <NA>                <NA>               NA
#> 10                  NA                <NA>                <NA>               NA
#> 11                  NA                <NA>                <NA>               NA
#> 12                  NA                <NA>                <NA>               NA
#> 13                  NA                <NA>                <NA>               NA
#> 14                  NA                <NA>                <NA>               NA
#> 15                  NA                <NA>                <NA>               NA
#> 16                  NA                <NA>                <NA>               NA
#> 17                  NA                <NA>                <NA>               NA
#> 18                  NA                <NA>                <NA>               NA
#> 19                  NA                <NA>                <NA>               NA
#> 20                  NA                <NA>                <NA>               NA
#> 21                  NA                <NA>                <NA>               NA
#> 22                  NA                <NA>                <NA>               NA
#> 23                  NA                <NA>                <NA>               NA
#> 24                  NA                <NA>                <NA>               NA
#> 25                  NA                <NA>                <NA>               NA
#> 26                  NA                <NA>                <NA>               NA
#> 27                  NA                <NA>                <NA>               NA
#> 28                  NA                <NA>                <NA>               NA
#> 29                  NA                <NA>                <NA>               NA
#> 30                  NA                <NA>                <NA>               NA
#> 31                  NA                <NA>                <NA>               NA
#> 32                  NA                <NA>                <NA>               NA
#> 33                  NA                <NA>                <NA>               NA
#> 34                  NA                <NA>                <NA>               NA
#> 35                  NA                <NA>                <NA>               NA
#> 36                  NA                <NA>                <NA>               NA
#> 37                  NA                <NA>                <NA>               NA
#> 38                  NA                <NA>                <NA>               NA
#> 39                  NA                <NA>                <NA>               NA
#> 40                  NA                <NA>                <NA>               NA
#> 41                  NA                <NA>                <NA>               NA
#> 42                  NA                <NA>                <NA>               NA
#> 43                  NA                <NA>                <NA>               NA
#> 44                  NA                <NA>                <NA>               NA
#> 45                  NA                <NA>                <NA>               NA
#> 46                  NA                <NA>                <NA>               NA
#> 47                  NA                <NA>                <NA>               NA
#> 48                  NA                <NA>                <NA>               NA
#> 49                  NA                <NA>                <NA>               NA
#>    border.top.color border.top.style border.left.width border.left.color
#> 1              <NA>             <NA>                NA              <NA>
#> 2              <NA>             <NA>                NA              <NA>
#> 3              <NA>             <NA>                NA              <NA>
#> 4              <NA>             <NA>                NA              <NA>
#> 5              <NA>             <NA>                NA              <NA>
#> 6              <NA>             <NA>                NA              <NA>
#> 7              <NA>             <NA>                NA              <NA>
#> 8              <NA>             <NA>                NA              <NA>
#> 9              <NA>             <NA>                NA              <NA>
#> 10             <NA>             <NA>                NA              <NA>
#> 11             <NA>             <NA>                NA              <NA>
#> 12             <NA>             <NA>                NA              <NA>
#> 13             <NA>             <NA>                NA              <NA>
#> 14             <NA>             <NA>                NA              <NA>
#> 15             <NA>             <NA>                NA              <NA>
#> 16             <NA>             <NA>                NA              <NA>
#> 17             <NA>             <NA>                NA              <NA>
#> 18             <NA>             <NA>                NA              <NA>
#> 19             <NA>             <NA>                NA              <NA>
#> 20             <NA>             <NA>                NA              <NA>
#> 21             <NA>             <NA>                NA              <NA>
#> 22             <NA>             <NA>                NA              <NA>
#> 23             <NA>             <NA>                NA              <NA>
#> 24             <NA>             <NA>                NA              <NA>
#> 25             <NA>             <NA>                NA              <NA>
#> 26             <NA>             <NA>                NA              <NA>
#> 27             <NA>             <NA>                NA              <NA>
#> 28             <NA>             <NA>                NA              <NA>
#> 29             <NA>             <NA>                NA              <NA>
#> 30             <NA>             <NA>                NA              <NA>
#> 31             <NA>             <NA>                NA              <NA>
#> 32             <NA>             <NA>                NA              <NA>
#> 33             <NA>             <NA>                NA              <NA>
#> 34             <NA>             <NA>                NA              <NA>
#> 35             <NA>             <NA>                NA              <NA>
#> 36             <NA>             <NA>                NA              <NA>
#> 37             <NA>             <NA>                NA              <NA>
#> 38             <NA>             <NA>                NA              <NA>
#> 39             <NA>             <NA>                NA              <NA>
#> 40             <NA>             <NA>                NA              <NA>
#> 41             <NA>             <NA>                NA              <NA>
#> 42             <NA>             <NA>                NA              <NA>
#> 43             <NA>             <NA>                NA              <NA>
#> 44             <NA>             <NA>                NA              <NA>
#> 45             <NA>             <NA>                NA              <NA>
#> 46             <NA>             <NA>                NA              <NA>
#> 47             <NA>             <NA>                NA              <NA>
#> 48             <NA>             <NA>                NA              <NA>
#> 49             <NA>             <NA>                NA              <NA>
#>    border.left.style border.right.width border.right.color border.right.style
#> 1               <NA>                 NA               <NA>               <NA>
#> 2               <NA>                 NA               <NA>               <NA>
#> 3               <NA>                 NA               <NA>               <NA>
#> 4               <NA>                 NA               <NA>               <NA>
#> 5               <NA>                 NA               <NA>               <NA>
#> 6               <NA>                 NA               <NA>               <NA>
#> 7               <NA>                 NA               <NA>               <NA>
#> 8               <NA>                 NA               <NA>               <NA>
#> 9               <NA>                 NA               <NA>               <NA>
#> 10              <NA>                 NA               <NA>               <NA>
#> 11              <NA>                 NA               <NA>               <NA>
#> 12              <NA>                 NA               <NA>               <NA>
#> 13              <NA>                 NA               <NA>               <NA>
#> 14              <NA>                 NA               <NA>               <NA>
#> 15              <NA>                 NA               <NA>               <NA>
#> 16              <NA>                 NA               <NA>               <NA>
#> 17              <NA>                 NA               <NA>               <NA>
#> 18              <NA>                 NA               <NA>               <NA>
#> 19              <NA>                 NA               <NA>               <NA>
#> 20              <NA>                 NA               <NA>               <NA>
#> 21              <NA>                 NA               <NA>               <NA>
#> 22              <NA>                 NA               <NA>               <NA>
#> 23              <NA>                 NA               <NA>               <NA>
#> 24              <NA>                 NA               <NA>               <NA>
#> 25              <NA>                 NA               <NA>               <NA>
#> 26              <NA>                 NA               <NA>               <NA>
#> 27              <NA>                 NA               <NA>               <NA>
#> 28              <NA>                 NA               <NA>               <NA>
#> 29              <NA>                 NA               <NA>               <NA>
#> 30              <NA>                 NA               <NA>               <NA>
#> 31              <NA>                 NA               <NA>               <NA>
#> 32              <NA>                 NA               <NA>               <NA>
#> 33              <NA>                 NA               <NA>               <NA>
#> 34              <NA>                 NA               <NA>               <NA>
#> 35              <NA>                 NA               <NA>               <NA>
#> 36              <NA>                 NA               <NA>               <NA>
#> 37              <NA>                 NA               <NA>               <NA>
#> 38              <NA>                 NA               <NA>               <NA>
#> 39              <NA>                 NA               <NA>               <NA>
#> 40              <NA>                 NA               <NA>               <NA>
#> 41              <NA>                 NA               <NA>               <NA>
#> 42              <NA>                 NA               <NA>               <NA>
#> 43              <NA>                 NA               <NA>               <NA>
#> 44              <NA>                 NA               <NA>               <NA>
#> 45              <NA>                 NA               <NA>               <NA>
#> 46              <NA>                 NA               <NA>               <NA>
#> 47              <NA>                 NA               <NA>               <NA>
#> 48              <NA>                 NA               <NA>               <NA>
#> 49              <NA>                 NA               <NA>               <NA>
#>    font.size bold italic underlined  color  font.family vertical.align
#> 1         22 <NA>      0       <NA> EF2929     Consolas           <NA>
#> 2         22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 3         22 <NA>      0       <NA> C4A000     Consolas           <NA>
#> 4         22 <NA>      0       <NA> 0000CF     Consolas           <NA>
#> 5         40 <NA>   <NA>       <NA>   <NA> RUB Scala TZ           <NA>
#> 6         22 <NA>      0       <NA> FF0000     Consolas           <NA>
#> 7         22 <NA>      0       <NA> 4E9A06     Consolas           <NA>
#> 8         22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 9         22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 10        22 <NA>      0       <NA> 000000     Consolas           <NA>
#> 11        22 <NA>      0       <NA> 204A87     Consolas           <NA>
#> 12        22 <NA>      0       <NA> 204A87     Consolas           <NA>
#> 13        22 <NA>      0       <NA> 0000CF     Consolas           <NA>
#> 14        22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 15        22 <NA>      0       <NA> A40000     Consolas           <NA>
#> 16        22 <NA>      0       <NA> FF0000     Consolas           <NA>
#> 17        22 <NA>      0       <NA> 0000CF     Consolas           <NA>
#> 18        22 <NA>      0       <NA> E6332A     Consolas           <NA>
#> 19      <NA> <NA>   <NA>       <NA>   <NA> RUB Scala TZ           <NA>
#> 20        22 <NA>      0       <NA> FF0000     Consolas           <NA>
#> 21        22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 22        22 <NA>      0       <NA> 204A87     Consolas           <NA>
#> 23      <NA> <NA>   <NA>       <NA>   <NA> RUB Scala TZ           <NA>
#> 24        22 <NA>      0       <NA> FF0000     Consolas           <NA>
#> 25        22 <NA>      0       <NA> CE5C00     Consolas           <NA>
#> 26        22 <NA>      0       <NA> 8F5902     Consolas           <NA>
#> 27        22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 28        22 <NA>      0       <NA> FF0000     Consolas           <NA>
#> 29        22 <NA>      0       <NA> 000000     Consolas           <NA>
#> 30        22 <NA>      0       <NA> 4E9A06     Consolas           <NA>
#> 31        16 <NA>   <NA>       <NA>   <NA>       Tahoma           <NA>
#> 32        22 <NA>      0       <NA> 4E9A06     Consolas           <NA>
#> 33      <NA> <NA>   <NA>       <NA>   <NA> RUB Scala TZ           <NA>
#> 34        22 <NA>      0       <NA> 000000     Consolas           <NA>
#> 35        22 <NA>      0       <NA> E6332A     Consolas           <NA>
#> 36        22 <NA>      0       <NA> 4E9A06     Consolas           <NA>
#> 37        22 <NA>   <NA>       <NA> 8F5902     Consolas           <NA>
#> 38        20 <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 39        52 <NA>   <NA>       <NA> 8DAE10     RubFlama           <NA>
#> 40      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 41      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 42      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 43      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 44      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 45      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#> 46        24 <NA>      0       <NA> 003560         <NA>           <NA>
#> 47        22 <NA>   <NA>       <NA> E6332A     Consolas           <NA>
#> 48        24 <NA>      0       <NA> 003560         <NA>           <NA>
#> 49      <NA> <NA>   <NA>       <NA>   <NA>         <NA>           <NA>
#>    shading.color hansi.family eastasia.family cs.family bold.cs font.size.cs
#> 1         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 2         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 3         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 4         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 5           <NA> RUB Scala TZ            <NA>      <NA>    <NA>         <NA>
#> 6         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 7         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 8         E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 9         F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 10        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 11        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 12        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 13        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 14        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 15        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 16        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 17        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 18        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 19          <NA> RUB Scala TZ            <NA>      <NA>    <NA>         <NA>
#> 20        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 21        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 22        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 23          <NA> RUB Scala TZ            <NA>      <NA>    <NA>         <NA>
#> 24        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 25        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 26        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 27        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 28        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 29        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 30        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 31          <NA>       Tahoma            <NA>    Tahoma    <NA>           16
#> 32        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 33          <NA> RUB Scala TZ            <NA>      <NA>    <NA>         <NA>
#> 34        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 35        E7E7E7     Consolas            <NA>      <NA>    <NA>         <NA>
#> 36        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 37        F8F8F8     Consolas            <NA>      <NA>    <NA>         <NA>
#> 38          <NA>         <NA>            <NA>      <NA>    <NA>           20
#> 39          <NA>     RubFlama            <NA>      <NA>    <NA>           52
#> 40          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 41          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 42          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 43          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 44          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 45          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 46          <NA>         <NA>            <NA>      <NA>    <NA>           40
#> 47          <NA>     Consolas            <NA>      <NA>    <NA>         <NA>
#> 48          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#> 49          <NA>         <NA>            <NA>      <NA>    <NA>         <NA>
#>    lang.val lang.eastasia lang.bidi
#> 1      <NA>          <NA>      <NA>
#> 2      <NA>          <NA>      <NA>
#> 3      <NA>          <NA>      <NA>
#> 4      <NA>          <NA>      <NA>
#> 5      <NA>          <NA>      <NA>
#> 6      <NA>          <NA>      <NA>
#> 7      <NA>          <NA>      <NA>
#> 8      <NA>          <NA>      <NA>
#> 9      <NA>          <NA>      <NA>
#> 10     <NA>          <NA>      <NA>
#> 11     <NA>          <NA>      <NA>
#> 12     <NA>          <NA>      <NA>
#> 13     <NA>          <NA>      <NA>
#> 14     <NA>          <NA>      <NA>
#> 15     <NA>          <NA>      <NA>
#> 16     <NA>          <NA>      <NA>
#> 17     <NA>          <NA>      <NA>
#> 18     <NA>          <NA>      <NA>
#> 19     <NA>          <NA>      <NA>
#> 20     <NA>          <NA>      <NA>
#> 21     <NA>          <NA>      <NA>
#> 22     <NA>          <NA>      <NA>
#> 23     <NA>          <NA>      <NA>
#> 24     <NA>          <NA>      <NA>
#> 25     <NA>          <NA>      <NA>
#> 26     <NA>          <NA>      <NA>
#> 27     <NA>          <NA>      <NA>
#> 28     <NA>          <NA>      <NA>
#> 29     <NA>          <NA>      <NA>
#> 30     <NA>          <NA>      <NA>
#> 31     <NA>          <NA>      <NA>
#> 32     <NA>          <NA>      <NA>
#> 33     <NA>          <NA>      <NA>
#> 34     <NA>          <NA>      <NA>
#> 35     <NA>          <NA>      <NA>
#> 36     <NA>          <NA>      <NA>
#> 37     <NA>          <NA>      <NA>
#> 38     <NA>          <NA>      <NA>
#> 39    de-DE          <NA>      <NA>
#> 40     <NA>          <NA>      <NA>
#> 41     <NA>          <NA>      <NA>
#> 42     <NA>          <NA>      <NA>
#> 43     <NA>          <NA>      <NA>
#> 44     <NA>          <NA>      <NA>
#> 45     <NA>          <NA>      <NA>
#> 46     <NA>          <NA>      <NA>
#> 47     <NA>          <NA>      <NA>
#> 48     <NA>          <NA>      <NA>
#> 49     <NA>          <NA>      <NA>
```

### Page Layout

#### Margins

- Top: 2,5 cm
- Left: 1,5 cm
- Bottom: 2 cm
- Right: 2 cm
- Gutter: 0 cm
- Gutter position: Left

#### Paper

- Size: A4

#### Layout

- Different first page (x)
- From edge: Header: 1 cm
- From edge: Footer: 1,1 cm

### Header and Footer

#### Header

- The header repeats the report author and title on the top right of
  every page using the following Word fields:
  - { AUTHOR \* MERGEFORMAT }
  - { TITLE \* MERGEFORMAT }

#### Footer

- The footer has the page number on the lower left using the following
  Word field:
  - { PAGE \* MERGEFORMAT }

### Styles

#### Title

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text
- Font: RubFlama, 26 pt, Bold, Font color: Custom Color(RGB(0;53;96)),
  Small caps
- Right: 1cm, Centered
- Space Before: 256 pt, After: 12pt, Keep with next, Keep lines together

#### Author

- Style type: Paragraph
- Style based on: (no style)
- Style for following paragraphs: Body Text
- Font: RubFlama, 26 pt, Font color: Custom Color(RGB(141;174;16)), All
  caps
- Right: 1 cm, Centered
- Line spacing: single
- Space After: 10 pt, Widow/Orphan control, Keep with next, Keep lines
  together

#### Date

- Style type: Paragraph
- Style based on: (no style)
- Style for following paragraphs: Body Text
- Font: RubFlama, 16 pt, Font color: Custom Color(RGB(0;53;96)), Small
  caps
- Right: 1 cm, Right
- Line spacing: single
- Space Before: 256 pt, After: 10 pt, Widow/Orphan control, Keep with
  next, Keep lines together

#### TOC Heading

- Style type: Paragraph
- Style based on: Heading 1
- Style for following paragraphs: Body Text
- Indent left: 0 cm
- Indent first line: 0 cm
- Line spacing: Multiple 1,08 li
- Space Before: 12 pt, After: 12 pt, None, No bullets or numbering

#### TOC 1

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Normal
- Right: 1 cm, Left,
- Space After: 5 pt
- Tab stops: 0,85 cm, Left + 17,48 cm, Right, Leader: …

#### Heading 1

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text
- Font: RubFlama, 14 pt, Bold, Font color: Custom Color(RGB(0;53;96)),
  All caps
- Indent:
  - Left: 0 cm
  - Hanging: 1,02 cm, Left
- Space After: 6 pt, Page break before, Keep with next, Keep lines
  together
- Level 1, Outline numbered + Level: 1 + Numbering Style: 1, 2, 3, … +
  Start at: 1 + Alignment: Left + Aligned at: 0 cm + Indent at: 1,02 cm

#### Heading 2

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text
- Font: RubFlama, 14 pt, Font color: Custom Color(RGB(0;53;96))
- Indent:
  - Left: 0 cm
  - Hanging: 1,02 cm, Left
- Space Before: 10 pt, After: 0 pt, Keep with next, Keep lines together,
  Level 2, Outline numbered + Level: 2 + Numbering Style: 1, 2, 3, … +
  Start at: 1 + Alignment: Left + Aligned at: 0 cm + Indent at: 1,02 cm

#### Heading 3

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text
- Font: (Default) RubFlama, 14 pt, Font color: Custom
  Color(RGB(141;174;16)), Left
- Space Before: 10 pt, After: 0 pt, Keep with next, Keep lines together
- Level 3

#### Heading 4

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text
- Font: RubFlama, Font color: Custom Color(RGB(141;174;16)), Left
- Space Before: 10 pt, After: 0 pt, Keep with next, Keep lines together
- Level 4

#### Heading 6

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Body Text -Font: (Default) RUB Scala
  MZ, 9 pt, Italic, - Space After: 18 pt, Keep lines together
- Level 6

#### Image Caption

- Style type: Paragraph
- Style based on: Caption
- Style for following paragraphs: Image Caption
- Font: 12 pt, Not Italic, Font color: Custom Color(RGB(0;53;96))
- Space After: 0 pt, Keep with next, Keep lines together

#### First Paragraph

- Style type: Paragraph
- Style based on: Body Text
- Style for following paragraphs: Body Text

#### Body Text

- Style type: Linked (paragraph and character)
- Style based on: Normal
- Style for following paragraphs: Body Text
- Space Before: 9 pt, After: 9 pt

#### Footnote Reference

- Style type: Character
- Style based on: Caption Char
- Font: RUB Scala TZ, 12 pt, Not Italic, Superscript

#### Footnote Text

- Style type: Paragraph
- Style based on: Normal
- Style for following paragraphs: Footnote Text
- Font: 10 pt
- Space After: 6 pt

#### Header

- Style type: Linked (paragraph and character)
- Style based on: Normal
- Style for following paragraphs: Header
- Space After: 0 pt
- Tab stops: 16 cm, Right

#### Footer

- Style type: Linked (paragraph and character)
- Style based on: Normal
- Style for following paragraphs: Footer
- Space After: 0 pt
- Tab stops: 16 cm, Right

#### Hyperlink

- Style type: Character
- Style based on: Caption Char
- Font: RUB Scala TZ, 12 pt, Not Italic, Font color: Accent 1

#### Compact

- Style type: Paragraph
- Style based on: Body Text
- Style for following paragraphs: Compact
- Space Before: 1,8 pt, After: 1,8 pt

#### Captioned Figure

- Style type: Paragraph
- Style based on: Figure
- Style for following paragraphs: Captioned Figure
- Space Before: 12 pt, After: 0 pt, Keep with next
