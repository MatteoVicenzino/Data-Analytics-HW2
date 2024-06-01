#month_inactive
banche$Months_Inactive_12_mon <-factor(banche$Months_Inactive_12_mon)
display_table(banche$Months_Inactive_12_mon, titolo = 'Month inactive 12 month')
#si osserva una maggiore concentrazione di valori in 2 e 3
#contacts count 12 month
banche$Contacts_Count_12_mon <- factor(banche$Contacts_Count_12_mon)
display_table(banche$Contacts_Count_12_mon, titolo = 'contacts 12 month')
#anche ui si osserva una maggiore concentrazione di valori maggiore in 2 e 3, questa variabile è meno assimetrica della precedente
#le prime 2 presentano solo i valori da 0 a 6 e faccio il barplot 

#credit limit
display_summary_and_var(banche$Credit_Limit)
hist(banche$Credit_Limit, freq = T, main = "distribuzione credito limite")
boxplot(banche$Credit_Limit)
#la distribuzione è fortemente assimetrica 

#total revolving bal
display_summary_and_var(banche$Total_Revolving_Bal)
hist(banche$Total_Revolving_Bal, freq = T, main = "Total Revolving Bal")
boxplot(banche$Total_Revolving_Bal)
#la moda è 0


#avg open to buy
display_summary_and_var(banche$Total_Revolving_Bal)
hist(banche$Avg_Open_To_Buy ,freq = T, main = "distribuzione avg_open_to_buy")
