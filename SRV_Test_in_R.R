library("RPostgres")
library("dplyr","RSQLite")
library("randomForest")
library("readr")
library("tidytext")    
library("syuzhet")
library("textdata")
library("RWeka")
library("tm")
library("ggplot2")
library("reshape2")
library("wordcloud")
library("wordcloud2")
library("textmineR")
library("psych")
library("plyr")


data1 <- read_csv("C:/Users/Nimish/Desktop/SRV Folder/dummy_data_model_input - Copy2.csv")
data1 <- as.data.frame(data1)
View(data1)
summary(data1)
fromt
###### Put All column names in one variable 'M' and segregate continuous and factor variables ######################################

M <- data.frame("lead_id", "product", "product_category", "income_group", "_10th",	"_12th",	"_ug3yrs",	"_ug4yrs",	"_pg",	"_pgexec",	"_certification",	"_parentalPrimaryEdu",	"_parentalSecondaryEdu",	"_healthCare",	"_fmcg",	"_automobiles", "_insaurance",	"_realEstate",	"_banking")
factor_vars <- c("lead_id","product", "product_category", "income_group")
continuous_vars <- c("_10th",	"_12th",	"_ug3yrs",	"_ug4yrs",	"_pg",	"_pgexec",	"_certification",	"_parentalPrimaryEdu",	"_parentalSecondaryEdu",	"_healthCare",	"_fmcg",	"_automobiles", "_insaurance",	"_realEstate",	"_banking")

product<-as.factor(c("PG","UG","Type_A","Type_B","UG-PG","Project_A","Project_B","Project_C"))
product
unclass(product)
str(factor_vars)
must_convert<-sapply(factor_vars,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(factor_vars[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(factor_vars[,!must_convert],M2)        # complete data.frame with all variables put together
out
out<-data.matrix(M)
out

###################################################################################################################################3

###################### Subsetting Data #################
#########  define age_data = 2020-2018... (to be used later) #########

names(data1)

data1.sub <- subset(data1, year == "2012")
data1.sub

data2.sub <- subset(data1, year == "2013")
data2.sub

data3.sub <- subset(data1, year == "2014")
data3.sub

data4.sub <- subset(data1, year == "2015")
data4.sub

data5.sub <- subset(data1, year == "2016")
data5.sub

data6.sub <- subset(data1, year == "2017")
data6.sub

data7.sub <- subset(data1, year == "2018")
data7.sub

data8.sub <- subset(data1, year == "2019")
data8.sub


New <-  data1.sub+data2.sub+data3.sub+data4.sub+data5.sub+data6.sub+data7.sub+data8.sub
New
New Variable <- for (j in data1$New){
                    print(j)
                    }
if````````````````````````````````````````````````````````````````````````````````````````````````````

############################################33333####################################################
############################################33333####################################################


data1.uni <- unique.array(data1.sub, data1.sub$product_category, incomparables = FALSE, MARGIN = 1, fromLast = TRUE)
data1.uni

Mode <- tail(names(sort(table(data1.sub$product_category))), 1)
Mode
x <- table(data1.sub$product_category)
print(x)
y <- table(data2.sub$product_category)
print(y)
z <- table(data3.sub$product_category)
print(z)

z1 <- table(data4.sub$product_category)
z1



lapply(data1,function(x){length(unique(tail(names(sort(table(data1$product_category))), 1)))})


year2012 <- "Education/PG"


for (i in unique(data1$product)) {
  command3 <- paste0("c", i, " <- colSums(data1.sub", , "[,6:23])")
  eval(parse(text = command3))
  command4 <- paste0("c", i, " <- c", i, "[order(c", i, ", decreasing = T)]")
  eval(parse(text = command4))
  command5 <- paste0("c", i, " <- as.data.frame(c", i, ")")
  eval(parse(text = command5))
  command6 <- paste0("c", i, "$names <- rownames(c", i, ")")
  eval(parse(text = command6))
  command7 <- paste0("rownames(c", i, ") <- 1:nrow(c", i, ")")
  eval(parse(text = command7))
}


############ Create one output variable ################################


data1$output <- data1$product * data$year, by = "id")
df$patients <- ifelse(data!!patients==150, 100, ifelse(data1$patients==350, 300, NA))


library("date","datetime")
as.date(data1$year, format="%y")
getOption("max.print")

library(lubridate)
yrs <- as.date(data1$year, format="%y")
yr <- as.Date(as.character(data1$year), format = "%y")
y <- year(yr)
y


while (i in data1$product_category) {
  print(i)

  }
table(data1$product)


for(i in y) %>% for("MBA" in data1$product)


x = []
for i in range(0,20):

 y = []
for i in x:
  y.append(-1*x[i])
for i in y:
  print i,y[i]


x=[]
for i in range(0,20):
  x.append(i) 
y=[] 
for i in x:
  y.append(-1*x[i])

for i in y:
  print x[-i], y[-i]


#Create a variable with value 1
begin <- 1
#Create the loop
while (begin <= 10){
  
#See which we are  
cat('This is loop number',begin)
  
#add 1 to the variable begin after each loop
begin <- begin+1
print(begin)
}

set seed(100)
stock <- 50
price <- 50
loop <- 1

# Set the while statement
while (price > 45){
  
  # Create a random price between 40 and 60
  price <- stock + sample(-10:10,1)
  
  # Count the number of loop
  loop = loop +1 
  
  # Print the number of loop
  print(loop)
}
