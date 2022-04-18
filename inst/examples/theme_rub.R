# Base plot
ggplot2::ggplot(
  mtcars,
  mapping = ggplot2::aes(
    x = mpg,
    y = disp,
    color = as.factor(carb)
  )
) +
ggplot2::geom_point() +
theme_rub(
  base_family = "sans"
)
