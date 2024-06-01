banche <- read.csv("C:\\Users\\giova\\Downloads\\credit_card.csv")

# Variabile 'Total_Amt_Chng_Q4_Q1'
display_summary_and_var(banche$Total_Amt_Chng_Q4_Q1)
hist(banche$Total_Amt_Chng_Q4_Q1, freq = F, main = "Distribuzione Total_Amt_Chng_Q4_Q1")
boxplot(banche$Total_Amt_Chng_Q4_Q1, horizontal = T)

# La variabile Total_Amt_Chng_Q4_Q1 è quantitativa e rappresenta il cambiamento nell'importo totale delle transazioni tra il quarto trimestre e il primo trimestre. La distribuzione ha una skewness positiva, indicando una coda lunga a destra. Questo significa che la maggior parte dei clienti ha avuto cambiamenti minori, con pochi clienti che hanno avuto grandi aumenti nell'importo delle transazioni.

# Variabile 'Total_Trans_Amt'
display_summary_and_var(banche$Total_Trans_Amt)
hist(banche$Total_Trans_Amt, freq = F, main = "Distribuzione Total_Trans_Amt")
boxplot(banche$Total_Trans_Amt, horizontal = T)

# La variabile Total_Trans_Amt è quantitativa e rappresenta l'importo totale delle transazioni. Anche questa variabile ha una skewness positiva, indicando che la maggior parte dei clienti ha un importo totale delle transazioni relativamente basso, con alcuni che hanno importi significativamente più alti.

# Variabile 'Total_Trans_Ct'
display_summary_and_var(banche$Total_Trans_Ct)
hist(banche$Total_Trans_Ct, freq = F, main = "Distribuzione Total_Trans_Ct")
boxplot(banche$Total_Trans_Ct, horizontal = T)

# La variabile Total_Trans_Ct è quantitativa e rappresenta il numero totale delle transazioni. La distribuzione mostra una skewness positiva, indicando che la maggior parte dei clienti effettua un numero relativamente basso di transazioni, mentre pochi clienti effettuano un numero molto alto di transazioni.

# Variabile 'Total_Ct_Chng_Q4_Q1'
display_summary_and_var(banche$Total_Ct_Chng_Q4_Q1)
hist(banche$Total_Ct_Chng_Q4_Q1, freq = F, main = "Distribuzione Total_Ct_Chng_Q4_Q1")
boxplot(banche$Total_Ct_Chng_Q4_Q1, horizontal = T)

# La variabile Total_Ct_Chng_Q4_Q1 rappresenta il cambiamento nel numero totale delle transazioni tra il quarto trimestre e il primo trimestre. Anche questa variabile mostra una skewness positiva, indicando una coda lunga a destra. La maggior parte dei clienti ha avuto cambiamenti minori nel numero di transazioni, con pochi che hanno avuto grandi aumenti.

# Variabile 'Avg_Utilization_Ratio'
display_summary_and_var(banche$Avg_Utilization_Ratio)
hist(banche$Avg_Utilization_Ratio, freq = F, main = "Distribuzione Avg_Utilization_Ratio")
boxplot(banche$Avg_Utilization_Ratio, horizontal = T)

# La variabile Avg_Utilization_Ratio è quantitativa e rappresenta il rapporto medio di utilizzo della carta di credito. La distribuzione ha una skewness positiva, indicando che la maggior parte dei clienti utilizza una piccola porzione del proprio credito disponibile, con pochi che utilizzano una grande porzione.

