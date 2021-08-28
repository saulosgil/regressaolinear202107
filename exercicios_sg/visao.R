visao <- read.csv2("misc/visao.csv")

visao <- visao[1:12, ]

tibble::glimpse(visao)

ggplot2::ggplot(visao) +
  ggplot2::geom_point(ggplot2::aes(x = idade, y = distancia))

model <- lm(distancia ~ idade, visao)

broom::augment(model)
