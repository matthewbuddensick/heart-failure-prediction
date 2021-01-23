Heart Failure Prediction
================
Matthew Buddensick
8/21/2020

## Objective

In this notebook I will be going through a dataset I found on
[kaggle](https://www.kaggle.com/andrewmvd/heart-failure-clinical-data)
to predict heart failure. It can also be found on the [UCI Machine
Learning
Repository](https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records).

## Load in and clean the data

``` r
df <- read_csv('heart_failure_clinical_records_dataset.csv') %>% 
    clean_names()
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   age = col_double(),
    ##   anaemia = col_double(),
    ##   creatinine_phosphokinase = col_double(),
    ##   diabetes = col_double(),
    ##   ejection_fraction = col_double(),
    ##   high_blood_pressure = col_double(),
    ##   platelets = col_double(),
    ##   serum_creatinine = col_double(),
    ##   serum_sodium = col_double(),
    ##   sex = col_double(),
    ##   smoking = col_double(),
    ##   time = col_double(),
    ##   DEATH_EVENT = col_double()
    ## )

``` r
summary(df)
```

    ##       age           anaemia       creatinine_phosphokinase    diabetes     
    ##  Min.   :40.00   Min.   :0.0000   Min.   :  23.0           Min.   :0.0000  
    ##  1st Qu.:51.00   1st Qu.:0.0000   1st Qu.: 116.5           1st Qu.:0.0000  
    ##  Median :60.00   Median :0.0000   Median : 250.0           Median :0.0000  
    ##  Mean   :60.83   Mean   :0.4314   Mean   : 581.8           Mean   :0.4181  
    ##  3rd Qu.:70.00   3rd Qu.:1.0000   3rd Qu.: 582.0           3rd Qu.:1.0000  
    ##  Max.   :95.00   Max.   :1.0000   Max.   :7861.0           Max.   :1.0000  
    ##  ejection_fraction high_blood_pressure   platelets      serum_creatinine
    ##  Min.   :14.00     Min.   :0.0000      Min.   : 25100   Min.   :0.500   
    ##  1st Qu.:30.00     1st Qu.:0.0000      1st Qu.:212500   1st Qu.:0.900   
    ##  Median :38.00     Median :0.0000      Median :262000   Median :1.100   
    ##  Mean   :38.08     Mean   :0.3512      Mean   :263358   Mean   :1.394   
    ##  3rd Qu.:45.00     3rd Qu.:1.0000      3rd Qu.:303500   3rd Qu.:1.400   
    ##  Max.   :80.00     Max.   :1.0000      Max.   :850000   Max.   :9.400   
    ##   serum_sodium        sex            smoking            time      
    ##  Min.   :113.0   Min.   :0.0000   Min.   :0.0000   Min.   :  4.0  
    ##  1st Qu.:134.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 73.0  
    ##  Median :137.0   Median :1.0000   Median :0.0000   Median :115.0  
    ##  Mean   :136.6   Mean   :0.6488   Mean   :0.3211   Mean   :130.3  
    ##  3rd Qu.:140.0   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:203.0  
    ##  Max.   :148.0   Max.   :1.0000   Max.   :1.0000   Max.   :285.0  
    ##   death_event    
    ##  Min.   :0.0000  
    ##  1st Qu.:0.0000  
    ##  Median :0.0000  
    ##  Mean   :0.3211  
    ##  3rd Qu.:1.0000  
    ##  Max.   :1.0000

We can see from the summary that there are variables that are numeric
that should be classified as factors.

``` r
df <- df %>% 
    mutate(anaemia = factor(anaemia, levels = c(0,1), labels = c('No','Yes')),
           diabetes = factor(diabetes, levels = c(0,1), labels = c('No','Yes')),
           high_blood_pressure = factor(high_blood_pressure, 
                                           levels = c(0,1), labels = c('No','Yes')),
           sex = factor(sex, levels = c(0,1), labels = c('Female','Male')),
           smoking = factor(smoking, levels = c(0,1), labels = c('No','Yes')),
           death_event = factor(death_event,
                                   levels = c(0,1), labels = c('Alive','Deceased')))
```

``` r
df$age_bin <- cut(df$age, breaks = c(30, 40, 50, 60, 70, 80, 90, 100))
df <- df %>% 
  select(-age)
```

### Check for missing data

``` r
(missing_data_df <- miss_var_summary(df))
```

    ## # A tibble: 13 x 3
    ##    variable                 n_miss pct_miss
    ##    <chr>                     <int>    <dbl>
    ##  1 anaemia                       0        0
    ##  2 creatinine_phosphokinase      0        0
    ##  3 diabetes                      0        0
    ##  4 ejection_fraction             0        0
    ##  5 high_blood_pressure           0        0
    ##  6 platelets                     0        0
    ##  7 serum_creatinine              0        0
    ##  8 serum_sodium                  0        0
    ##  9 sex                           0        0
    ## 10 smoking                       0        0
    ## 11 time                          0        0
    ## 12 death_event                   0        0
    ## 13 age_bin                       0        0

``` r
ggplot(missing_data_df, aes(x = variable, y = pct_miss)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    coord_flip() +
    labs(title = 'Missing Data', x = 'Variable', y = 'Percent Missing')
```

![](Heart-Failure-Markdown_files/figure-gfm/Plot%20Missing%20Data-1.png)<!-- -->

There is no missing data.

## Exploratory Data Analysis

``` r
ggplot(df, aes(x = smoking, y = serum_sodium, fill = smoking)) +
    geom_boxplot(alpha = .75) +
    labs(title = 'Interaction Between Smoking and Serium Sodium Levels', 
         x = 'Smokes', y = 'Serum Sodium') +
  facet_wrap(~sex) +
  scale_fill_manual(values = c('blue','red')) + 
  theme(legend.position = 'none')
```

![](Heart-Failure-Markdown_files/figure-gfm/nteraction%20Between%20Smoking%20and%20Serium%20Sodium%20Levels%20for%20each%20Sex-1.png)<!-- -->

It looks like smoking does not have an affect on a persons serum sodium
levels regardless of that persons sex.

``` r
ggplot(df, aes(x = diabetes, y = serum_sodium, fill = diabetes)) +
    geom_boxplot(alpha = .75) +
    labs(title = 'Interaction Between Diabetes and Serium Sodium Levels', 
         x = 'Diabetes', y = 'Serum Sodium') +
  facet_wrap(~sex) +
  scale_fill_manual(values = c('blue','red')) +
  theme(legend.position = 'none')
```

![](Heart-Failure-Markdown_files/figure-gfm/Interaction%20Between%20Diabetes%20and%20Serium%20Sodium%20Levels%20for%20each%20Sex-1.png)<!-- -->

Diabetes also does not seem to have an affect on a persons serum sodium
levels regardless of sex.

``` r
ggplot(df, aes(x = death_event, y = ejection_fraction / 100, fill = death_event)) +
  geom_boxplot(alpha = .75) +
  labs(title = 'Survival based on Ejection Fraction', x = 'Survival', 
       y = 'Ejection Fraction') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('blue','red')) +
  theme(legend.position = 'none')
```

![](Heart-Failure-Markdown_files/figure-gfm/Survival%20based%20on%20Ejection%20Fraction%20plot-1.png)<!-- -->

Ejection fraction is defined as the percentage of blood leaving the
heart at each contraction. Based off this plot, it looks like a higher
ejection fraction means a higher chance of survival. An article from the
[clevland
clinic](https://my.clevelandclinic.org/health/articles/16950-ejection-fraction#:~:text=A%20normal%20left%20ventricular%20ejection,how%20well%20your%20treatment%20works.)
describes that the ejection fraction of a healthy person without heart
failure should range from 55% to 70%. This would support the idea that a
higher ejection fraction would lead to a better chance of survival.

``` r
ggplot(df, aes(x = age_bin, y = ejection_fraction / 100, fill = age_bin)) +
  geom_boxplot(alpha = .75) +
  labs(title = 'Comparing Ejection Fraction and Age', x = 'Age Bins', 
       y = 'Ejection Fration') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none')
```

![](Heart-Failure-Markdown_files/figure-gfm/Ejection%20Fraction%20and%20Age-1.png)<!-- -->

Ejection Fraction seems to be relatively constant across all age groups
in the data.

## Modeling

For the modeling process, I am going to use a logistic regression as a
baseline model. I am also going to run randomForest and XGBoost models
using the tidymodels package.

Splitting the data into training and testing sets and creating a cross
validation object to split the training data into 10 groups.

``` r
set.seed(42)
split <- initial_split(df, prop = .75, strata = death_event)
train_data <- training(split)
test_data <- testing(split)

k_folds <- vfold_cv(train_data, 10)
```

``` r
tidy_recipe <- recipe(death_event~., data = train_data) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

tidy_recipe %>% prep()
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         12
    ## 
    ## Training data contained 225 data points and no missing data.
    ## 
    ## Operations:
    ## 
    ## Centering and scaling for creatinine_phosphokinase, ... [trained]
    ## Dummy variables from anaemia, diabetes, high_blood_pressure, sex, ... [trained]

This has created a recipe where numeric variables have been centered and
scaled, and categorical variables have been converted to dummy vairbles
besides the target variable. There are 12 predictor variables being used
to predict the 1 target variable.

``` r
logistic_regression_model <- logistic_reg() %>% 
  set_mode('classification') %>% 
  set_engine('glm')

randomForest_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode('classification') %>% 
  set_engine('randomForest')

XGBoost_model <- boost_tree(mtry = tune(), trees = tune(), min_n = tune(), 
                      tree_depth = tune(), learn_rate = tune()) %>% 
  set_mode('classification') %>% 
  set_engine('xgboost')
```

Created 3 models using the tidymodels package. The logistic regression
model has no tuning paramters and will be used as the baseline model to
evaluate other models. The 2 other models being used are a randomForest
and XGBoost with both having tuned parameters.

Below is a function to creat a workflow object. It takes in a recipe in
a model and creaters an object that aggregates information in order to
fit and predict from a model.

``` r
create_workflow <- function(recipe, model) {
  tidy_workflow <- workflow() %>% 
    add_recipe(recipe) %>% 
    add_model(model)
}
```

``` r
logistic_wf <- create_workflow(tidy_recipe, logistic_regression_model)
randomForest_wf <- create_workflow(tidy_recipe, randomForest_model)
XGBoost_wf <- create_workflow(tidy_recipe, XGBoost_model)
```

Below is a function to tune the models. The function tune\_grid from the
tune package already does this, but I wrapped it inside of a function
called tune\_model for easier repitition.

``` r
tune_model <- function(workflow, resamples = k_folds, grid = 5){
  tidy_tune <- tune_grid(workflow, resamples = resamples, grid = grid)
}
```

``` r
randomForest_tune <- tune_model(randomForest_wf)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
    ##     splice

    ## 
    ## Attaching package: 'vctrs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     data_frame

    ## The following object is masked from 'package:tibble':
    ## 
    ##     data_frame

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
XGBoost_tune <- tune_model(XGBoost_wf)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

    ## 
    ## Attaching package: 'xgboost'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
logistic_reg <- fit_resamples(logistic_wf, k_folds)
collect_metrics(logistic_reg)
```

    ## # A tibble: 2 x 6
    ##   .metric  .estimator  mean     n std_err .config             
    ##   <chr>    <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1 accuracy binary     0.809    10  0.0255 Preprocessor1_Model1
    ## 2 roc_auc  binary     0.851    10  0.0186 Preprocessor1_Model1

The baseline model has an accuracy of about .81 and an roc\_auc score of
about .85 on the training data.

``` r
tuned_randomForest <- randomForest_tune %>% select_best('roc_auc')
tuned_XGBoost <- XGBoost_tune %>% select_best('roc_auc')
```

``` r
finalized_randomForest <- finalize_model(randomForest_model, tuned_randomForest)
finalized_XGBoost <- finalize_model(XGBoost_model, tuned_XGBoost)
```

Since the model is now finalized with tuned parameters and the best
model has been selected using the select\_best function, the final model
needs to be passed through a final workflow before using it on the
testing data.

``` r
final_workflow <- function(recipe, final_model){
  final_workflow <- workflow() %>% 
    add_recipe(recipe) %>% 
    add_model(final_model)
}
```

``` r
final_randomForest_model <- final_workflow(tidy_recipe, finalized_randomForest)
final_XGBoost <- final_workflow(tidy_recipe, finalized_XGBoost)
```

``` r
test_randomForest <- final_randomForest_model %>% last_fit(split) %>% collect_metrics()
test_XGBoost <- final_XGBoost %>% last_fit(split) %>% collect_metrics()
```

``` r
logistic_wf %>% last_fit(split) %>% collect_metrics()
```

    ## # A tibble: 2 x 4
    ##   .metric  .estimator .estimate .config             
    ##   <chr>    <chr>          <dbl> <chr>               
    ## 1 accuracy binary         0.770 Preprocessor1_Model1
    ## 2 roc_auc  binary         0.832 Preprocessor1_Model1

The baseline model has an accuracy of about .77 on the testing data and
an roc\_auc score of about .83. Both the accuracy and the roc\_auc score
decreased on the testing set compared to the training set.

``` r
test_randomForest
```

    ## # A tibble: 2 x 4
    ##   .metric  .estimator .estimate .config             
    ##   <chr>    <chr>          <dbl> <chr>               
    ## 1 accuracy binary         0.770 Preprocessor1_Model1
    ## 2 roc_auc  binary         0.885 Preprocessor1_Model1

``` r
test_XGBoost
```

    ## # A tibble: 2 x 4
    ##   .metric  .estimator .estimate .config             
    ##   <chr>    <chr>          <dbl> <chr>               
    ## 1 accuracy binary         0.824 Preprocessor1_Model1
    ## 2 roc_auc  binary         0.866 Preprocessor1_Model1

``` r
test_data %>% count(death_event)
```

    ## # A tibble: 2 x 2
    ##   death_event     n
    ##   <fct>       <int>
    ## 1 Alive          50
    ## 2 Deceased       24

## Results

Since the data is imbalanced with there being about 2x as many patients
that are alive than deceased in the test data, I will use the roc\_auc
score as the primary metric to determine which model best fits the data.

The randomForest model performs about the same as the baseline model in
regard to accuracy (about .77), but better in terms of the roc\_auc
score (about .89).

The XGBoost model performs worse than the baseline and randomForest
model in terms of accuracy (about .73), and worse than the randomForest
model in regards to the roc\_auc score (about .88).

Since the randomForest model has a better roc\_auc score than the
baseline model and the XGBoost model, it is the model that best fits the
data.
