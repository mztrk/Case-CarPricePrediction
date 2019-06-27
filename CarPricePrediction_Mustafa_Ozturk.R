#----Data Importing----------------
##Delete

library(readr)
Dataset <- read_csv("C:/Users/mustafa.ozturk/Downloads/dataset-DS-TestData.csv")
View(Dataset)


#-------Data Cleaning---------------

Dataset$`normalized-losses` = as.character(Dataset$`normalized-losses`)
Dataset$`normalized-losses` = as.numeric(Dataset$`normalized-losses`)

cor(Dataset$`normalized-losses`,Dataset$symboling) 

#correlation of symboling and normalized losses are very low and they contain many unknown, 
#therefore remove them 
Dataset = Dataset[,-c(1,2)]

Dataset$bore = as.character(Dataset$bore)
Dataset$bore = as.numeric(Dataset$bore)
Dataset$bore[is.na(Dataset$bore)]<- -1

Dataset$stroke = as.character(Dataset$stroke)
Dataset$stroke = as.numeric(Dataset$stroke)
Dataset$stroke[is.na(Dataset$stroke)]<- -1

Dataset$price = as.character(Dataset$price)
Dataset$price = as.numeric(Dataset$price)
Dataset$price[is.na(Dataset$price)]<- -1
#delete rows from dataset where price is unknown
Dataset=Dataset[-which(Dataset$price==-1),]

Dataset$horsepower = as.character(Dataset$horsepower)
Dataset$horsepower = as.numeric(Dataset$horsepower)
Dataset$horsepower[is.na(Dataset$horsepower)]<- -1

Dataset$`peak-rpm` = as.character(Dataset$`peak-rpm`)
Dataset$`peak-rpm` = as.numeric(Dataset$`peak-rpm`)
Dataset$`peak-rpm`[is.na(Dataset$`peak-rpm`)]<- -1


#cor(Dataset$bore,Dataset$`engine-size`)

#Correlation between bore and engine-size is high,
#So we can impute the missing values in column bore by using values in engine-size column
#get mean bore of engine-size +/- 30 and fill the bore column with this mean value
rowNums = which(Dataset$bore==-1.00)
for(i in rowNums){
  newbore = Dataset[i,"engine-size"]
  Dataset[i,"bore"]=mean(Dataset[which(Dataset$`engine-size`>(newbore-30) & Dataset$`engine-size`<(newbore+30) & Dataset$bore!=-1.00) ,"bore"])
}

#cor(Dataset$bore,Dataset$stroke)

#bore and stroke is also correlated
#Then we can fill the stroke column missing values by using bore column values
rowNums = which(Dataset$stroke==-1.00)
for(i in rowNums){
  newstroke = Dataset[i,"bore"]
  Dataset[i,"stroke"]=mean(Dataset[which(Dataset$bore>(newstroke-0.5) & Dataset$bore<(newstroke+0.5) & Dataset$stroke!=-1.00) ,"stroke"])
}

#fill the missing values horsepower column by using `engine-size` column
# since correlation between these 2 columns are high
#cor(Dataset$engine.size,Dataset$horsepower) 

rowNums = which(Dataset$horsepower==-1.00)
for(i in rowNums){
  newhorsepower = Dataset[i,"engine-size"]
  Dataset[i,"horsepower"]=mean(Dataset[which(Dataset$`engine-size`>(newhorsepower-30) & Dataset$`engine-size`<(newhorsepower+30) & Dataset$horsepower!=-1.00) ,"horsepower"])
}

#peak-rpm column seems like not so much correlated with other columns
#therefore fill the missing values in this column by mean of the column
rowNums = which(Dataset$`peak-rpm`==-1.00)
for(i in rowNums){
  Dataset[i,"peak-rpm"]=as.integer(mean(Dataset[which(Dataset$`peak-rpm`!=-1),"peak-rpm"]))
}

#-------Data Splitting----------------
# Data splitting rule %70-training,%30-test
sample_size <- floor(0.70 * nrow(Dataset))

## set the seed to make your partition reproductible
train_indexes <- sample(seq_len(nrow(Dataset)), size = sample_size)

train <- Dataset[train_indexes, ]
test <- Dataset[-train_indexes, ]

#----------Random Forest Modelling------------
library(randomForest)
rf <- randomForest(price ~ .,data=train,importance=TRUE, na.action=na.exclude)

y = test[,24]
y <- as.character(y)
y <- as.numeric(y)
predicted = predict(rf,test[,-24])

rSquare = 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)

#feature importance list
varImpPlot(rf)

#eliminate the least important five features : fuel.type,aspiration,num.of.doors,body.style,engine.location 
dataset2 = dataset[,-c(2,3,4,5,7)]

train2 <- dataset2[train_indexes, ]
test2 <- dataset2[-train_indexes, ]

rf2 <- randomForest(price ~ .,data=train2,importance=TRUE, na.action=na.exclude)

y2 = test2[,ncol(test2)]
predicted2 = predict(rf2,test2[,-ncol(test2)])

rSquare2 = 1 - sum((y2-predicted2)^2)/sum((y2-mean(y2))^2)


#-------------Seleceting Final Model------------
if(rSquare>rSquare2){
  finalModel = rf 
}else{
  finalModel = rf2
}

