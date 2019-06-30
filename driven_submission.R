### submisstion script ###

library(tidyverse)
library(randomForest)
#update.packages('randomForest')

setwd('second_attempt.csv')
train = read.csv('train_proc.csv')
labels = read.csv('trainlabels.csv')
test = read.csv('test_proc.csv')

rfdf = left_join(train,labels,by = 'id')
idx = which(colnames(rfdf) == 'id')
rfdf = rfdf[,-idx]

levels(test$region_code) = levels(train$region_code)
levels(test$district_code) = levels(train$district_code)
levels(test$date_recorded) = levels(train$date_recorded)
levels(test$scheme_management) = levels(train$scheme_management)
levels(test$extraction_type) = levels(train$extraction_type)

# parameters selected through CV
rfmodel = randomForest(status_group ~ ., rfdf,ntree = 800,mtry = 1)

preds = predict(rfmodel,newdata = test[,-1])

submission = data.frame(cbind(test$id,preds))
colnames(submission) = c('id','status_group')

write.csv(submission,'default_rf_r.csv',row.names = FALSE)

sub = read.csv('default_rf_r.csv')

sub = sub %>% mutate(status_group = case_when(status_group == 1 ~ 'functional',
                                              status_group == 2 ~ 'functional needs repair',
                                              status_group == 3 ~ 'non functional'))
write.csv(sub,'mtry1_ntree800_rf_r.csv',row.names = FALSE)
