library(xgboost)
library(dplyr)
library(caret)

###Data Import###
dd <- read.csv("~/R/Chronic_Kidney_Disease/data2.csv")
summary(dd)
datahead <- names(dd)
fullhead <- c("Row ID", "Age","Blood Pressure","Specific Gravity","Albumin","Sugar","Red Blood Cells","Pus Cell","Pus Cell Clumps","Bacteria","Blood Glucose Random","Blood Urea","Serum Creatinine","Sodium","Potassium","Hemoglobin","Packed Cell Volume","White Blood Cell Count","Red Blood Cell Count","Hypertension","Diabetes Mellitus","Coronary Artery Disease","Appetite","Pedal Edema","Anemia","Class")
heading  <- data.frame(names(dd),fullhead)
dd <- dd[,-1]

# one-hot-encoding categorical features
ohe_feats = c('rbc', 'pc', 'pcc','ba','htn','dm','cad','appet','pe','ane')
dummies <- dummyVars(~ rbc+ pc+ pcc+ba+htn+dm+cad+appet+pe+ane, data = dd)
dd_ohe <- as.data.frame(predict(dummies, newdata = dd))
dd_combined <- cbind(dd[,-c(which(colnames(dd) %in% ohe_feats))],dd_ohe)

##create a list of selected feature then activate below line.
#dd_combined <- dd_combined[,c('id',features_selected)] 

# split train and test
indexes = sample(1:nrow(dd_combined), size=0.25*nrow(dd_combined))
test = dd_combined[indexes,]
training = dd_combined[-indexes,]
 

dtrain <- xgb.DMatrix(data = training, label=training$class)
dtest <- xgb.DMatrix(data = test, label=test$class)

X <-  training
y <- training$class
X_test = test

xgb <- xgboost(data = data.matrix(X[,-15]), 
 label = y, 
 eta = 0.1,
 max_depth = 15, 
 nround=25, 
 subsample = 0.5,
 colsample_bytree = 0.5,
 seed = 1,
 eval_metric = "merror",
 objective = "binary:logistic",
 num_class = 12,
 nthread = 3
)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-15]))




# Lets start with finding what the actual tree looks like
model <- xgb.dump(xgb, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model

# Get the feature real names
names <- dimnames(data.matrix(X[,-15]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:15,])

output_vector <- training[,15]


