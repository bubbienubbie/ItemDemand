library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(timetk)
library(patchwork)
library(forecast)

full_train <- vroom("C:/Users/isaac/Downloads/items/train.csv")
full_test <- vroom("C:/Users/isaac/Downloads/items/test.csv")

item_train <- full_train %>%
  filter(store==1, item==1)

item_test <- full_test %>%
  filter(store==1, item==1)

my_recipe <- recipe(sales ~., data=item_train) %>%
  step_date(date, features=c("doy","dow","month","year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, new_data = item_train)
baked2 <- bake(prep, new_data = item_test)


#MY MOD
tree_mod <- decision_tree(tree_depth = tune(),
                          cost_complexity = tune(),
                          min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # Engine = What R function to use
  set_mode("regression")

## Create a workflow with model & recipe
tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(tree_mod)

## Set up grid of tuning value
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 3)

## Set up K-fold CV
folds <- vfold_cv(item_train, v = 3, repeats=3)


## Find best tuning parameters
CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape)) #Or leave metrics NULL

bestTune <- CV_results %>%
  select_best("smape")

final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=item_train)

final_pred <- predict(final_wf, new_data = item_test)

best_tree_depth <- bestTune$tree_depth







collect_metrics(CV_results) %>%
  filter(.extract_best = TRUE) %>%
  pull(mean)

#---------------------------------------------------IDK

Stores <- max(train$store)
nItems <- max(train$item)
for(s in 1:5){
  for(i in 1:5){
    item_train <- full_train %>%
    filter(store==s, item==i)
    item_test <- full_test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
    CV_results <- tree_wf %>%
      tune_grid(resamples=folds,
                grid=tuning_grid,
                metrics=metric_set(smape)) #Or leave metrics NULL
    
    bestTune <- CV_results %>%
      select_best("smape")
    
    final_wf <-
      tree_wf %>%
      finalize_workflow(bestTune) %>%
      fit(data=item_train)
    
    ## Predict storeItem sales
    preds <- predict(final_wf, new_data = item_test)
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}


item_11 <- item_train %>%
  filter(store==1, item==1)

item_12 <- item_train %>%
  filter(store==1, item==2)

item_21 <- item_train %>%
  filter(store==2, item==1)

item_22 <- item_train %>%
  filter(store==2, item==2)

plot1 <- item_11 %>%
plot_time_series(date, sales, .interactive=FALSE) + ggtitle("Store 1 Item 1")

plot2 <- item_12 %>%
  plot_time_series(date, sales, .interactive=FALSE) + ggtitle("Store 1 Item 2")

plot3 <- item_21 %>%
  plot_time_series(date, sales, .interactive=FALSE) + ggtitle("Store 2 Item 1")

plot4 <- item_22 %>%
  plot_time_series(date, sales, .interactive=FALSE) + ggtitle("Store 2 Item 2")

plot1+plot2+plot3+plot4  




#---------Cross Validate
library(modeltime) #Extensions of tidymodels to time series1
library(timetk) #Some nice time series functions2

train <- item_train %>% filter(store==4, item==16)
test <- item_test %>% filter(store==4, item==16)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)




es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
cross416 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = train
) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)

es_fullfit <- cv_results %>%
modeltime_refit(data = train)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=test, by="date") %>%
select(id, sales)

pred416 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = train) %>%
plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(cross317,cross416,pred317,pred416, nrows=2)



#-----------------------------ARIMA-------------------
item_train <- full_train %>%
  filter(store==1, item==1)

item_test <- full_test %>%
  filter(store==1, item==1)

arima_recipe <- recipe(sales ~., data=item_train) %>%
  step_date(date, features=c("doy","dow","month","year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, new_data = item_train)


arima_model <- arima_reg(seasonal_period=2,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
set_engine("auto_arima")


#workflow
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))


cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))




cross11 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE, .title="Store 1 Item 1 CrossEval") 

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

ar_fullfit <- cv_results %>%
  modeltime_refit(data = item_train)

ar_preds <- ar_fullfit %>%
  modeltime_forecast(new_data=item_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

pred11 <- ar_fullfit %>%
  modeltime_forecast(new_data=item_test, actual_data = item_train) %>%
  plot_modeltime_forecast(.interactive=FALSE, .title="Store 1 Item 1 Pred")


#-----------Arima part 2
item_train <- full_train %>%
  filter(store==2, item==1)

item_test <- full_test %>%
  filter(store==2, item==1)

arima_recipe <- recipe(sales ~., data=item_train) %>%
  step_date(date, features=c("doy","dow","month","year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, new_data = item_train)


arima_model <- arima_reg(seasonal_period=2,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")


#workflow
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))


cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))




cross21 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE, .title="Store 2 Item 1 CrossEval")


## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

ar_fullfit <- cv_results %>%
  modeltime_refit(data = item_train)

ar_preds <- ar_fullfit %>%
  modeltime_forecast(new_data=item_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

pred21 <- ar_fullfit %>%
  modeltime_forecast(new_data=item_test, actual_data = item_train) %>%
  plot_modeltime_forecast(.interactive=FALSE, .title="Store 2 Item 1 Pred")


plotly::subplot(cross11,cross21,pred11,pred21, 
                nrows=2)




#------------Facebook Prophet Model--------
install.packages("prophet")
library(prophet)

item_train <- full_train %>%
  filter(store==2, item==1)

item_test <- full_test %>%
  filter(store==2, item==1)

my_recipe <- recipe(sales ~., data=item_train) %>%
  step_date(date, features=c("doy","dow","month","year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, new_data = item_train)

cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))


cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))




pro_cross21 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)
pro_cross21

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

pro_fullfit <- cv_results %>%
  modeltime_refit(data = item_train)

pro_preds <- pro_fullfit %>%
  modeltime_forecast(new_data=item_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

pro_pred21 <- pro_fullfit %>%
  modeltime_forecast(new_data=item_test, actual_data = item_train) %>%
  plot_modeltime_forecast(.interactive=FALSE)



item_train <- full_train %>%
  filter(store==1, item==1)

item_test <- full_test %>%
  filter(store==1, item==1)

my_recipe <- recipe(sales ~., data=item_train) %>%
  step_date(date, features=c("doy","dow","month","year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(my_recipe)
baked <- bake(prep, new_data = item_train)

cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))


cv_split <- time_series_split(item_train, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))




pro_cross11 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = item_train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)
pro_cross21

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

pro_fullfit <- cv_results %>%
  modeltime_refit(data = item_train)

pro_preds <- pro_fullfit %>%
  modeltime_forecast(new_data=item_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

pro_pred11 <- pro_fullfit %>%
  modeltime_forecast(new_data=item_test, actual_data = item_train) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(pro_cross11,pro_cross21, pro_pred11, pro_pred21,
                nrows=2)

