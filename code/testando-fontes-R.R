#https://www.modelodomundo.com/2019/09/importando-fontes-no-r-extrafont/
library(ggplot2)

fontes <- fonts()
length(fontes)
m1 <- rep(1:20, 10)
m2 <- rep(1:10, each = 20)
fontes_df <- data.frame(x = m1, y = m2, fontes = fontes, text = "abc123")
ggplot() +
    geom_text(data = fontes_df, aes(x = x, y = y, label = text, family = fontes)) +
    scale_x_continuous(breaks = 1:20) +
    scale_y_continuous(breaks = 1:20) +
    theme_classic()

fontes_df[fontes_df$x == 12 & fontes_df$y == 6, ]