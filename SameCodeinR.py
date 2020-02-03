install.packages("RPostgres")
install.packages("randomForest")
library("RPostgres")
library("dplyr","RSQLite")
library("randomForest")

con <- dbConnect(
  RPostgres::Postgres(),
  host = "192.168.2.176",
  dbname = "dev",
  user = "postgres",
  password = "postgres",
  port = 5432)

#res <- dbSendQuery(con, "SELECT * FROM leadrepo_lead limit 10;")

res <- tbl(con, sql("select l.lead_hash_id as lead_id, c.lower_age_limit as min_age,
c.upper_age_limit as max_age, c.year as year, i.income_group as income_group FROM leadrepo_lead as l,
leadrepo_campaign as c, leadrepo_productcategory as pc, leadrepo_product as p, leadrepo_incomegroup 
as i WHERE l.campaign_id = c.id and c.product_category_id = pc.id and pc.product_id = p.id and 
p.income_group_id = i.id"))

head(res, n=10)
count(res)
View(res)

data1 <- as.data.frame(res, header = TRUE)

View(data1)


# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(50)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

# Create a Random Forest model with default parameters
model1 <- randomForest(min_age ~ income_group, data = TrainSet, importance = TRUE)
model1


##############################################################################################################
##############################################################################################################


install.packages("RPostgres")
install.packages("randomForest")
library("RPostgres")
library("dplyr","RSQLite")
library("randomForest")

data <- read.csv(file = "C:/Users/Nimish/Desktop/SRV Folder/dummy_data_model_input - Copy.csv", head = TRUE)
data <- as.data.frame(data)
View(data)
dim(data)

na.omit(data)
data
data[complete.cases(data), ]

View(data)
#training Sample with 300 observations
train=sample(1:nrow(data), 270000)

TrainSet <- data[train,]
ValidSet <- data[-train,]
summary(TrainSet)
summary(ValidSet)
set.seed(100)
data #to search on the dataset

data.rf=randomForest(year ~ . ,data = data, ntree = 500, mtry = 6, importance = TRUE, subset = train, na.action = na.omit)
data.rf

############## Logistic Regression Model ##############################################

install.packages("woe","sm","smbinning","woeBinning","woeR")
library(smbinning)
library("glm.deploy","glm2")
library("woe")
library("woeBinning")
install.packages("InformationValue")  # For stable CRAN version
library("InformationValue")


table(data$year)

# Create Training Data
input_ones <- data[which(data$year == 1), ]  # all 1's
input_zeros <- data[which(data$year == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 


# segregate continuous and factor variables
factor_vars <- c("lead_id", "product", "product_category", "income_group")
continuous_vars <- c("X_10th",	"X_12th",	"X_ug3yrs",	"X_ug4yrs",	"X_pg",	"X_pgexec",	"X_certification",	"X_parentalPrimaryEdu",	"X_parentalSecondaryEdu",	"X_healthCare",	"X_fmcg",	"X_automobiles", "X_insaurance",	"X_realEstate",	"X_banking")

##### Create WOE for categorical variables (optional) #######

for(factor_var in factor_vars){
  data[[factor_var]] <- woe(X=data[, factor_var], Y=data$year)
}
head(data)

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(20))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(train, y="", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(train, y="", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

logitMod <- glm(year ~ X_ug3yrs + X_ug4yrs + X_pg + X_pgexec, data=trainingData, family=linkfun(mustart))

predicted <- plogis(predict(logitMod, train))  # predicted scores
# or
predicted <- predict(logitMod, train, type="response")  # predicted scores




