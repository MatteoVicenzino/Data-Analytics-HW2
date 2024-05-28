#questa funzione prende in input una variabile quantitativa e stampa summary + varianza e dev. standared in una riga
#!!omette gli NA ovunque
display_summary_and_var <- function(variabile){
  c(summary(variabile), 
        var = var(variabile, na.rm = T), 
        sd = sd(variabile, na.rm = T))
}
