# DATA PREPARATION ----
# Machine readable ----

# libraries
library(rsample)
library(recipes)
employee_attrition_tbl <- read_csv("content/01_journal/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt")
definitions_raw_tbl    <- read_excel("content/01_journal/data_definitions.xlsx", sheet = 1, col_names = FALSE)
# Processing pipeline
# If we had stored our script in an external file
source("content/01_journal/data_processing_pipeline.R")
# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_tbl, prop = 0.85)

# Assign training and test data
train_raw_tbl <- training(split_obj)
test_raw_tbl  <- testing(split_obj)
# If we had our raw data already split into train and test data
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl   <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

employee_attrition_readable_tbl_a <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)
?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)
automl_models_h2o@leader
h2o.getModel("StackedEnsemble_AllModels_2_AutoML_10_20230608_152758")%>% 
  h2o.saveModel(path = "h20_models_attr/")

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()