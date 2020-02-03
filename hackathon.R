 # Libraries 
 

# Reading dataset and converting into dataframe
data <- read.csv(file = "G:/dsda-20-18/hackathon_spain/purchase_data.csv", head = TRUE, sep=",")
data<- data.frame(data)

#Sorting with dates and adding year column 
data_sorted<-data[order(as.Date(data$date_of_purchase, format="%d-%m-%Y")),]
data_sorted$year<-year(as.Date(data_sorted$date_of_purchase,"%d-%m-%Y")) #it will give you only years

# Performance in previous years 

# Number of purchase
year_wise_purchase<-data.frame(table(data_sorted$year))
colnames(year_wise_purchase)<-c("year","Frequency")
p <- plot_ly(year_wise_purchase, x = ~year, y = ~Frequency, name = 'Frequency', type = 'scatter', mode = 'lines+markers')%>%
  layout(title = 'Frequency vs Year',
         xaxis = list(title = 'Years'),
         yaxis = list (title = 'Number Of Purchases'))

p


#total purchases amount
purchase_year_wise<-aggregate(purchase_amount~year,data=data_sorted, sum)
q <- plot_ly(purchase_year_wise, x = ~year, y = ~purchase_amount, name = 'Revenue', type = 'bar')%>%
  layout(title = 'Revenue vs Year',         
         xaxis = list(title = 'Years'),
         yaxis = list (title = 'Revenue'))

q


#Average purchases yearly
average_purchase_amount_yearly<-aggregate(purchase_amount~year,data_sorted,mean)
colnames(average_purchase_amount_yearly)<-c("year","average_purchase_amount")
r <- plot_ly(average_purchase_amount_yearly, x = ~year, y = ~average_purchase_amount, name = 'Average', type = 'bar',marker = list(color = 'rgb(205,104,49)'))%>%
  layout(title = 'Average Purchase vs Year',
         xaxis = list(title = 'Years'),
         yaxis = list (title = 'Average Amount of Purchase'))

r

#Merging dataset to get and overview of sales v/s revenue
new_data<-merge(year_wise_purchase,average_purchase_amount_yearly,by = "year",all = TRUE)
new_data<-merge(new_data,purchase_year_wise,by = "year",all = TRUE)
View(new_data)

#testing correlation between the two independent variables
cor(new_data$Frequency,new_data$average_purchase_amount)  # Correlation is fairly low



s <- plot_ly(new_data, x = ~year, y = ~purchase_amount, name = 'regression', type = 'scatter',mode = 'lines+markers',marker = list(color = 'rgb(205,202,49)'))%>%
     add_trace(y = ~, name = 'trace 1', mode = 'lines+markers') %>%
     layout(title = 'Regression',
         xaxis = list(title = 'Years'),
         yaxis = list (title = 'Purchase Amount'))

s

plot(new_data$purchase_amount)
abline(reg=lm(new_data$~time(AirPassengers)))




