# I have excluded the grid search code blocks to save time. It will used the
# values found earlier. They can be found in the Report.Rmd file.
# The script takes about 6 minutes to run on my computer i7 processor with 16GB memory

install.packages("pROC") # used to calculate the AUC

library(caret)
library(tidyverse)
library(pROC)

options(warn=-1)
set.seed(42)

clean_map_names <- function(map_name) {
  # Clean the incorrect names for maps in the dataset
  corrected_name <- case_when(
    map_name %in% c("MIDTOWN", "MID TOWN") ~ "Midtown",
    map_name %in% c("ESPERANGA", "ESPERANCA") ~ "Esperança",
    map_name == "PARA/SO" ~ "Paraiso",
    map_name %in% c("NUMBANI", "NUMBANI CITY") ~ "Numbani",
    map_name %in% c("NEW JUNK CITY", "JUNKERTOWN") ~ "Junkertown",
    map_name %in% c("THRONE OF ANUBIS") ~ "Temple of Anubis",
    map_name %in% c("NEW QUEEN STREET") ~ "New Queen Street",
    map_name %in% c("WATCHPOINT") ~ "Watchpoint: Gibraltar",
    map_name == "HANAOKA" ~ "Hanamura",
    map_name == "RUNASAPI" ~ "Runa Sapi",
    map_name == "KING" ~ "King's Row",
    TRUE ~ map_name  # Leave as is if no correction is needed
  )
  return(toupper(corrected_name))
}

is_valid_character <- function(character, role) {
  # Define a helper function to determine if a character matches the role
  if (role == "tank") return(character %in% tank_heroes)
  if (role == "damage") return(character %in% damage_heroes)
  if (role == "support") return(character %in% support_heroes)
  return(FALSE)
}

# Create the hero data
tank_heroes <- c("Sigma", "Orisa", "Reinhardt", "Ramattra", "Winston", 
                 "Roadhog", "Zarya", "Mauga", "Wrecking_Ball", "Doomfist", 
                 "Junker_Queen", "DVa")

damage_heroes <- c("Genji", "Soldier_76", "Bastion", "Venture", "Cassidy", 
                   "Sojourn", "Sombra", "Junkrat", "Reaper", "Pharah", "Ashe", 
                   "Hanzo", "Echo", "Torbjorn", "Widowmaker", "Tracer", 
                   "Symmetra", "Mei")

support_heroes <- c("Juno", "Ana", "Kiriko", "Mercy", "Moira", "Illari", 
                    "Lucio", "Baptiste", "Brigitte", "Zenyatta", "Lifeweaver")

# Map the hero types to leaderboard position
player_roles <- list(
  tank = c(0, 5),
  damage = c(1, 2, 6, 7),
  support = c(3, 4, 8, 9)
)

# create the dataset
dataset <- read.csv("game_data.csv") %>%
  mutate(Result = ifelse(Result == "lose", -1, ifelse(Result == "draw", 0, 1)), 
         Character = str_remove(Character, "label_"),
         Map = clean_map_names(Map)) %>% 
  separate(Time, into = c("Minutes", "Seconds"), fill = "right", extra = "drop") %>%
  mutate(
    Minutes = as.numeric(Minutes),
    Seconds = as.numeric(Seconds),
    Time = ifelse(is.na(Minutes) | is.na(Seconds), NA, Minutes + (Seconds / 60))
  ) %>%
  filter(!is.na(Time) & Time > 0) %>% # Remove rows with invalid or zero times
  dplyr::select(-Minutes, -Seconds, -Latency)

# Replace remaining NAs with 0
dataset <- dataset %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# Process the dataset
dataset <- dataset %>%
  group_by(GameID, PlayerID) %>% # Group by game_id and playerid
  arrange(GameID, PlayerID, SnapID) %>% # Ensure data is sorted correctly
  mutate(
    Role = case_when(
      PlayerID %in% player_roles$tank ~ "tank",
      PlayerID %in% player_roles$damage ~ "damage",
      PlayerID %in% player_roles$support ~ "support",
      TRUE ~ NA_character_
    ),
    Valid_Character = mapply(is_valid_character, Character, Role) # Check if character is valid for the role
  ) %>%
  mutate(
    Character = ifelse(
      !Valid_Character & SnapID != min(SnapID), # If character is invalid and not the first SnapID
      lag(Character), # Roll back to the previous character
      Character
    )
  ) %>%
  ungroup() %>%
  select(-Role, -Valid_Character) # Clean up intermediate columns

print("1. Clean the dataset based on the data exploration by clipping the player metrics and dropping Map and Mode with the value of `Unknown`.")

clean_dataset <- dataset %>% 
  filter(Map != 'UNKNOWN' & Mode != 'Unknown') %>%
  select(GameID, SnapID, PlayerID, K, A, D, Damage, H, MIT, Time, Mode, Map, Result) %>%
  mutate(
    across(c(K, A, D, Damage, H, MIT), ~ .x / Time),  # Normalize metrics by Time
    K = pmin(K, 5),
    A = pmin(A, 4),
    D = pmin(D, 3),
    Damage = pmin(Damage, 2500),
    H = case_when(
      PlayerID %in% c(0, 5) ~ pmin(H, 600),
      PlayerID %in% c(1, 2, 6, 7) ~ pmin(H, 400),
      TRUE ~ pmin(H, 2500)
    ),
    MIT = pmin(MIT, 2500)
  )

# Ensure that the categorical variables are factors
clean_dataset <- clean_dataset %>% mutate(
  Mode=as.factor(Mode),
  Map=as.factor(Map)
)

# adjust the game outcome to be between 0 and 1 (drop the drawn games)
clean_dataset <- clean_dataset %>% 
  filter(Result!=0) %>% 
  mutate(Result=(Result+1)/2,
         Result=as.factor(Result), 
         Result=relevel(Result, ref = "1"))

print("Complete!")

print("2. Group by snapshot id (`snapID`) and then shuffle the playerIDs to improve generalization")

set.seed(42)

swap_ids <- function(ids, index1, index2){
  temp_id <- ids[index1]
  ids[index1] <- ids[index2]
  ids[index2] <- temp_id
  return(ids)
}

# row [tank1, dmg1, dmg1, supp1, supp1, tank2, dmg2, dmg2, supp2, supp2]

shuffle_players <- function(ids) {
  # randomly shuffle damage for team 1?
  if (sample(0:1, 1) == 1) {
    ids <- swap_ids(ids, 2, 3)
  }
  
  # randomly shuffle damage for team 2?
  if (sample(0:1, 1) == 1) {
    ids <- swap_ids(ids, 7, 8)
  }
  
  # randomly shuffle support for team 1?
  if (sample(0:1, 1) == 1) {
    ids <- swap_ids(ids, 4, 5)
  }
  
  # randomly shuffle support for team 2?
  if (sample(0:1, 1) == 1) {
    ids <- swap_ids(ids, 9, 10)
  }
  
  return (ids)
}

shuffled_clean_dataset <- clean_dataset %>%
  group_by(SnapID) %>%
  group_modify(~ {
    # Shuffle the PlayerID vector for the current group
    .x$PlayerID <- shuffle_players(.x$PlayerID)
    return(.x)
  }) %>%
  ungroup()

print("Complete!")

print("3. Combine the data from each snapshot into a single observation")

shuffled_clean_dataset <- shuffled_clean_dataset %>% group_by(GameID, SnapID) %>%
  pivot_wider(
    names_from = PlayerID, 
    values_from = c(K, A, D, Damage, H, MIT),
    names_prefix = "player"
  ) %>% ungroup() %>% 
  select(-SnapID, -GameID) # remove unncessary detail

print("Complete!")
print("Beginning Training.")

set.seed(42)

# create the train, test, and validation set
# 80% for train 10% for test and 10% for validation
train_ind <- createDataPartition(shuffled_clean_dataset$Result, p=0.8, list=F)
train_set <- shuffled_clean_dataset[train_ind,]
remaining_set <- shuffled_clean_dataset[-train_ind,]

test_ind <- createDataPartition(remaining_set$Result, p=0.5, list=F)
test_set <- remaining_set[test_ind,]
val_set <- remaining_set[-test_ind,]

## Base Model (Logistic Regression)
print("Base Logistic Regression Model Results...")

# train the model to see the base results
base_model = train(Result ~., 
                   data=train_set, 
                   method='glmnet', 
                   trControl=trainControl(method='cv', number=10),
                   preProc = c("center", "scale"))

pred <- predict(base_model, newdata=test_set)
print(confusionMatrix(pred, test_set$Result))

pred_prob <- predict(base_model, newdata = test_set, type = "prob")[,1]
roc_obj <- roc(test_set$Result, pred_prob)

# Plot ROC curve
print(auc(roc_obj))

## Model 2 (with regularization finetune)
print("Hyperparameter-tuned Logistic Regression Model Results:")

set.seed(42)
model_2 = train(Result ~., 
                data=train_set, 
                method='glmnet', 
                trControl=trainControl(method='cv', number=10),
                tuneGrid=data.frame(alpha=0, lambda=.1011),
                preProc = c("center", "scale"))

pred <- predict(model_2, newdata = test_set)
print(confusionMatrix(pred, test_set$Result))

pred_prob <- predict(model_2, newdata = test_set, type = "prob")[,1]
roc_obj <- roc(test_set$Result, pred_prob)

# Plot ROC curve
print(auc(roc_obj))

## Base Model (RandomForest)
print("Base Random Forest Model Results...")
print("This could take a while.")

set.seed(42)
control <- trainControl(method='cv', number=10, search = 'grid')

rf_model <- train(Result ~ ., 
                  data = train_set,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

# View the model details
print(rf_model)

pred <- predict(rf_model, newdata = test_set)

print(confusionMatrix(pred, test_set$Result))

pred_prob <- predict(rf_model, newdata = test_set, type = "prob")[,1]
roc_obj <- roc(test_set$Result, pred_prob)

# Plot ROC curve
print(auc(roc_obj))

## Model 2 (hyperparameter finetune)
print("Hyperparamter-tuned Random Forest Model Results...")
print("This could take a while.")

set.seed(42)

# Define repeated cross-validation control with random search
control <- trainControl(method = "repeatedcv", 
                        number = 5, 
                        repeats = 3, 
                        search = "random")

# Random search will randomly pick `tuneLength` combinations
tune_length <- 5  # Number of random combinations to test

# Train the Random Forest model with random search
rf_tuned <- train(
  Result ~ ., 
  data = train_set,
  method = "rf",
  metric = "Accuracy",
  trControl = control,
  tuneLength = tune_length,
  importance = TRUE
)

# View the tuned model
print(rf_tuned)

pred <- predict(rf_tuned, newdata = test_set)
print(confusionMatrix(pred, test_set$Result))

pred_prob <- predict(rf_tuned, newdata = test_set, type = "prob")[,1]
roc_obj <- roc(test_set$Result, pred_prob)

# Plot ROC curve
print(auc(roc_obj))