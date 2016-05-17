library(ggplot2)
library(sqldf)


pdf <- df[, names(df) %in% c("currency", "dead", "stock_price_in_usd")]
dead_rows <- which(pdf$dead == 1)
additional <- sample(seq_along(df$dead)[-dead_rows], 100)

normalize <- function(x) { x / sum(x) }
pdf$stock_price_in_usd[dead_rows] <- normalize(pdf$stock_price_in_usd[dead_rows])
pdf$stock_price_in_usd[additional] <- normalize(pdf$stock_price_in_usd[additional])

pdf$dead <- factor(pdf$dead)
pdf$dead <- revalue(pdf$dead, c("0"="Alive", "1"="Dead"))


ind <- c(dead_rows, additional)
pdf <- pdf[ind, ]

pdf <- pdf[rev(order(pdf$currency)),]

p = ggplot(data=pdf, 
           aes(x= factor(1),
               y= stock_price_in_usd,
               fill = currency
           ))

# p = p + geom_bar(width = 1) 
p = p + geom_bar(stat="identity")

p = p+facet_grid(facets=. ~ dead)
p = p + coord_polar(theta="y") 
p

hist(se_pd, xlim = c(0, 100), breaks = 500, ylim = c(0, 500), main = "Распределение задержки оплаты", xlab = "Зарержка в днях", ylab = "Частота")
