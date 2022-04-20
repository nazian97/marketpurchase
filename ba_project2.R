library(methods)
library(recommenderlab)
library(data.table)
library(ggplot2)
library(knitr)

## Data Pre-processing
data <- fread(file.choose())
data[ ,InvoiceDate := as.Date(InvoiceDate)]
str(data)

# Data Imputation - delete NA Row
data[Quantity<=0,Quantity:=NA]
data[UnitPrice<=0,UnitPrice:=NA]
data <- na.omit(data)
data

# Sort the data to make the search easier
setkeyv(data, c('StockCode', 'Description'))
itemCode <- unique(data[, c('StockCode', 'Description')])
setkeyv(data, NULL)
data

# Convert data between wide and long forms
data2 <- dcast(data, CustomerID ~ StockCode, value.var = 'Quantity',fun.aggregate = sum, fill=0)

CustomerId <- data2[,1] #!

data2 <- data2[,-c(1,3504:3508)]

# Fill NA with 0
for (i in names(data2))
  data2[is.na(get(i)), (i):=0]
data2

# Convert wide form to sparse matrix
data3 <- as.matrix(data2)
data3 <- data3[rowSums(data3) > 5,colSums(data3) > 5] 
data3 <- binarize(as(data3, "realRatingMatrix"), minRatin = 1)
data3

# Split dataset to 0.8 (training) and 0.2 (test)
train <- sample(x = c(TRUE, FALSE), size = nrow(data3),replace = TRUE, prob = c(0.8, 0.2))
y <- data3[!train]
x <- data3[train]

# Create a model using the default parameters 
# The default parameters are Cosine as a method with k=3
r.models <- recommenderRegistry$get_entries(dataType ="binaryRatingMatrix")
r.models$IBCF_binaryRatingMatrix$parameters

# Training dataset
method <- 'IBCF'
parameter <- list(method = 'Jaccard')
n_recommended <- 5
n_training <- 1000

recc.model <- Recommender(data = x, method = method, parameter = parameter)
m.details <- getModel(recc.model)

recc.predicted <-predict(object = recc.model, newdata=y,n = n_recommended, type="topNList")

# Item recommended for the first 5 users in training dataset
as(recc.predicted,"list")[1:5]

user.1 <- CustomerId[as.integer(names(recc.predicted@items[1]))] #12348

# Recommendations for user: 12348
p <- recc.predicted@items[[1]]
p <- rownames(m.details$sim)[p]
itemCode[p]

# Actual purchase of user 12348
user.1a <- data[CustomerID=='12348', sum(Quantity), by=StockCode]
merge(itemCode,user.1a, by='StockCode')
