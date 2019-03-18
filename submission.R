## Second try at Driven Data competition



setwd('C:/Users/Nick/Documents/Classes/Fall 18/Machine Learning/Project')

trainlabels = read.csv('trainlabels.csv')

train = read.csv('train.csv')
test = read.csv('test.csv')

## havent messed with: funder, num_private
# check reduced levels for multiple representations of missing


## installer #######
levels(train$installer) = c(levels(train$installer),'missing')
levels(test$installer) = c(levels(test$installer),'missing')

train$installer[train$installer %in% c('',0)] = 'missing'
test$installer[test$installer %in% c('',0)] = 'missing'


num_lvls = 10
installer_levels = names(sort(table(train$installer),decreasing = TRUE)[1:num_lvls])
train$installer[-which(train$installer %in% installer_levels)] = 'missing'
train$installer = droplevels(train$installer)

levels(test$installer) = c(levels(test$installer),'missing')
test$installer[-which(test$installer %in% installer_levels)] = 'missing'
test$installer = droplevels(test$installer)
###########


## funder ##############
levels(train$funder) = c(levels(train$funder),'missing')
levels(test$funder) = c(levels(test$funder),'missing')

train$funder[train$funder %in% c('',0)] = 'missing'
test$funder[test$funder %in% c('',0)] = 'missing'


num_lvls = 10
funder_levels = names(sort(table(train$funder),decreasing = TRUE)[1:num_lvls])
train$funder[-which(train$funder %in% funder_levels)] = 'missing'
train$funder = droplevels(train$funder)

levels(test$funder) = c(levels(test$funder),'missing')
test$funder[-which(test$funder %in% funder_levels)] = 'missing'
test$funder = droplevels(test$funder)
###########


## wpt_name #####
num_lvls = 10
wpt_name_levels = names(sort(table(train$wpt_name),decreasing = TRUE)[1:num_lvls])
levels(train$wpt_name) = c(levels(train$wpt_name),'missing')
train$wpt_name[-which(train$wpt_name %in% wpt_name_levels)] = 'missing'
train$wpt_name = droplevels(train$wpt_name)

levels(test$wpt_name) = c(levels(test$wpt_name),'missing')
test$wpt_name[-which(test$wpt_name %in% wpt_name_levels)] = 'missing'
test$wpt_name = droplevels(test$wpt_name)
############


## lga ######
num_lvls = 10
lga_levels = names(sort(table(train$lga),decreasing = TRUE)[1:num_lvls])
levels(train$lga) = c(levels(train$lga),'missing')
train$lga[-which(train$lga %in% lga_levels)] = 'missing'
train$lga = droplevels(train$lga)

levels(test$lga) = c(levels(test$lga),'missing')
test$lga[-which(test$lga %in% lga_levels)] = 'missing'
test$lga = droplevels(test$lga)
############


## scheme_name ######
num_lvls = 10
scheme_name_levels = names(sort(table(train$scheme_name),decreasing = TRUE)[1:num_lvls])
levels(train$scheme_name) = c(levels(train$scheme_name),'missing')
train$scheme_name[-which(train$scheme_name %in% scheme_name_levels)] = 'missing'
train$scheme_name = droplevels(train$scheme_name)

levels(test$scheme_name) = c(levels(test$scheme_name),'missing')
test$scheme_name[-which(test$scheme_name %in% scheme_name_levels)] = 'missing'
test$scheme_name = droplevels(test$scheme_name)
############


## subvillage ###### 
num_lvls = 10
subvillage_levels = names(sort(table(train$subvillage),decreasing = TRUE)[1:num_lvls])
levels(train$subvillage) = c(levels(train$subvillage),'missing')
train$subvillage[-which(train$subvillage %in% subvillage_levels)] = 'missing'
train$subvillage = droplevels(train$subvillage)

levels(test$subvillage) = c(levels(test$subvillage),'missing')
test$subvillage[-which(test$subvillage %in% subvillage_levels)] = 'missing'
test$subvillage = droplevels(test$subvillage)
############

## ward ########
num_lvls = 10
ward_levels = names(sort(table(train$ward),decreasing = TRUE)[1:num_lvls])
levels(train$ward) = c(levels(train$ward),'missing')
train$ward[-which(train$ward %in% ward_levels)] = 'missing'
train$ward = droplevels(train$ward)

levels(test$ward) = c(levels(test$ward),'missing')
test$ward[-which(test$ward %in% ward_levels)] = 'missing'
test$ward = droplevels(test$ward)

## population #####
thresh = 20
med_pop = median(train$population[train$population > thresh])
train$population[train$population < thresh] = med_pop
test$population[test$population < thresh] = med_pop
############

## construction year ###
med_conyear = median(train$construction_year[train$construction_year != 0])
train$construction_year[train$construction_year == 0] = med_conyear 
test$construction_year[test$construction_year == 0] = med_conyear
###########

## date recorded #####
train$year_recorded = str_sub(as.Date(train$date_recorded),1,-7)
train$year_recorded = as.factor(train$year_recorded)
train$month_recorded = str_sub(as.Date(train$date_recorded),6,-4)
train$month_recorded = as.factor(train$month_recorded)

test$year_recorded = str_sub(as.Date(test$date_recorded),1,-7)
test$year_recorded = as.factor(test$year_recorded)
test$month_recorded = str_sub(as.Date(test$date_recorded),6,-4)
test$month_recorded = as.factor(test$month_recorded)

idx = which(colnames(train) == 'date_recorded')
train = train[,-idx] # remove date_recorded

idx = which(colnames(test) == 'date_recorded')
test = test[,-idx]
##########

## recorded by #### 
idx = which(colnames(train) == 'recorded_by')
train = train[,-idx]
idx = which(colnames(test) == 'recorded_by')
test = test[,-idx]
#########

## waterpoint_type_group ######
# same (basically) as waterpoint_type ##
idx = which(colnames(train) == 'waterpoint_type_group')
train = train[,-idx]
idx = which(colnames(test) == 'waterpoint_type_group')
test = test[,-idx]
###########   

## source_class #######
# source is nested in source_class ####
idx = which(colnames(train) == 'source_class')
train = train[,-idx]
idx = which(colnames(test) == 'source_class')
test = test[,-idx]
###############

## quantity_group ######
# quantity_group is identical to quantity ## 
idx = which(colnames(train) == 'quantity_group')
train = train[,-idx]
idx = which(colnames(test) == 'quantity_group')
test = test[,-idx]
##############

## extraction ####
## getting rid of extraction_type_class and extraction_type_group
## extraction_type (i think) is nested in these
idx = which(colnames(train) %in% c('extraction_type_class','extraction_type_group'))
train = train[,-idx]
idx = which(colnames(test) %in% c('extraction_type_class','extraction_type_group'))
test = test[,-idx]
##############


setwd('C:/Users/Nick/Documents/Classes/Fall 18/Machine Learning/Project/second_attempt')

write.csv(train,'train_proc.csv',row.names = FALSE)
write.csv(test,'test_proc.csv',row.names = FALSE)