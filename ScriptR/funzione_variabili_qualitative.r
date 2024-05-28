display_summary_and_var_qualitative <- function(variabile) {
  # Calcola il sommario della variabile
  sommario <- summary(variabile)
  
  # Calcola la frequenza assoluta
  frequenza_assoluta <- table(variabile)
  
  # Calcola la frequenza relativa
  frequenza_relativa <- prop.table(frequenza_assoluta)
  
  # Crea un barplot della variabile
  barplot(frequenza_relativa, main="Barplot della variabile", xlab="Categorie", ylab="Frequenza")
  
  # Restituisce una lista contenente il sommario, la frequenza assoluta e la frequenza relativa
  return(list(sommario = sommario, 
              frequenza_assoluta = frequenza_assoluta, 
              frequenza_relativa = frequenza_relativa))
}
