# grafico colorato
ggplot(case, aes(x = as.factor(MasVnrType), y = SalePrice, fill = as.factor(MasVnrType))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
