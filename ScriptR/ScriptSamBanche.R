#month_inactive
display_summary_and_var_qualitative(banche$Months_Inactive_12_mon)

#contacts count 12 month
display_summary_and_var_qualitative(banche$Contacts_Count_12_mon)

#le prime 2 presentano solo i valori da 0 a 6 e faccio il barplot 

#credit limit
display_summary_and_var(banche$Credit_Limit)
skewness(banche$Credit_Limit)
hist(banche$Credit_Limit, freq = T, main = "distribuzione credito limite")
#fortemente assimetrica

#total revolving bal
display_summary_and_var(banche$Total_Revolving_Bal)
skewness(banche$Total_Revolving_Bal)
hist(banche$Total_Revolving_Bal, freq = T, main = "Total Revolving Bal")
boxplot(banche$Total_Revolving_Bal)
#la moda è 0


#avg open to buy
display_summary_and_var(banche$Total_Revolving_Bal)
skewness(banche$Avg_Open_To_Buy)
hist(banche$Avg_Open_To_Buy ,freq = T, main = "distribuzione avg_open_to_buy")
