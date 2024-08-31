# __________________________________________________________________________________________________
# Empty garbage collector
base::gc()



# __________________________________________________________________________________________________
# Some functions
"%!in%" <- base::Negate("%in%")
# Define the objective function used to choose an adequate cut off death probability : it's the squared error of the difference between predictions and actual values weighted by each passenger group size
Function__objective <- function(Num__threshold, Num__deathprobabilities, Num__groupweights, Num__survived) {
  Num__sumerrors <- base::sum(((base::as.integer(Num__deathprobabilities > Num__threshold) * Num__groupweights - Num__survived) ^ 2))
  base::return(Num__sumerrors)
}
# Wrapper function for optimx
Function__wrapper <- function(Num__threshold0) {
  Function__objective(
    Num__threshold = Num__threshold0[1] # initial threshold value
    , Num__deathprobabilities = Df__engineeredtrain$DeathProbability
    , Num__groupweights = Df__engineeredtrain$GroupWeight
    , Num__survived = Df__engineeredtrain$Survived
  )
}



# __________________________________________________________________________________________________
# Set a random seed to ensure reproducibility
base::set.seed(123)



# __________________________________________________________________________________________________
# 1. Load required packages
List__packages <- base::list("readr", "dplyr", "purrr", "xgboost", "optimx")
base::lapply(X = List__packages, FUN = base::library, character.only = T, warn.conflicts = T)



# __________________________________________________________________________________________________
# 2. Load train.csv and test.csv
Df__train <- readr::read_csv(
  file = "data/train/train.csv", col_types = readr::cols(.default = readr::col_character())) |>
  readr::stop_for_problems() |> base::as.data.frame()
Df__test <- readr::read_csv(
  file = "data/test/test.csv", col_types = readr::cols(.default = readr::col_character())) |>
  readr::stop_for_problems() |> base::as.data.frame()



# __________________________________________________________________________________________________
# 3. Feature engineering (its replicates straightforwardly the process of feature engineering done in the main.ipnyb file)
# Read this main.ipnyb for more details on why these steps are done.

# 3.1. Get an unified view of Df__train and Df__test ________________
Df__titanic <- base::rbind(Df__train |> dplyr::select(-Survived), Df__test) |> base::as.data.frame()

# 3.2. Remove "Ticket", "Cabin", "Fare" ________________
# "Ticket" granularity could be a problem for a good model construction.
# "Cabin" has missing values which can be difficult to impute; even if we manage to impute, the same granularity problem as "Ticket" is present.
# Both "Ticket" and "Cabin" information can be seen as summarized and grouped in "PClass", as well as for "Fare".
Df__titanic[, c("Ticket", "Cabin", "Fare")] <- NULL

# 3.3. Rewrite "PClass" as "BoardingClass" ________________

# first class is the most privileged, second class is moderately privileged, and third class is the least privileged.
Df__titanic$BoardingClass <- base::ifelse(
  test = (Df__titanic$Pclass == "1"), yes = "most privileged"
  , no = base::ifelse(test = (Df__titanic$Pclass == "2"), yes = "moderately privileged", no = "least privileged"))

# Convert "BoardingClass" data type into factor
Df__titanic$BoardingClass <- base::factor(Df__titanic$BoardingClass)

# Plot "BoardingClass"
par(cex.axis = 0.55)
graphics::plot(Df__titanic$BoardingClass, main = "BoardingClass")

# Cleanup
Df__titanic$Pclass <- NULL

# 3.4. Convert "Sex" data type into factor ________________

Df__titanic$Sex <- base::factor(Df__titanic$Sex)

# Plot "Sex"
par(cex.axis = 0.55)
graphics::plot(Df__titanic$Sex, main = "Sex")

# 3.5. Extract a "Title" column from "Name" ________________

# Define a list of all possible titles present, and their signification
Char__adulttitles <- c("mr", "mrs", "sir", "rev", "capt", "dr", "col", "major", "ms", "mme", "countess", "dona", "don", "jonkheer")
Char__youngtitles <- c("master", "miss")
Char__ambiguoustitles <- "mlle"
Char__titles <- c(Char__adulttitles, Char__youngtitles, Char__ambiguoustitles)

# Go through each value of "Name" and extract the according "Title"
Df__titanic$Title <- NA
purrr::map(
  .x = Df__titanic$PassengerId
  , .f = function(Char__passengerid){
    
    # Get "Name" in lowercase according to Char__passengerid, and removed of all periods
    Char__name <- Df__titanic |> dplyr::filter(PassengerId == Char__passengerid) |> dplyr::pull(Name) |> base::tolower() |> 
      base::gsub(pattern = "\\.", replacement = "")
    
    # Get the according "Title"
    for (Char__title in Char__titles) {
      if (base::grepl(pattern = paste0("\\b", Char__title, "\\b"), x = Char__name, fixed = F)) {
        Df__titanic[Df__titanic$PassengerId == Char__passengerid, "Title"] <<- Char__title
      }
    } # end for (Char__title in Char__titles)
  }
  , .progress = T)

# Plot "Title"
par(cex.axis = 0.55)
graphics::plot(base::factor(Df__titanic$Title), main = "Title")

# Cleanup
Df__titanic$Name <- NULL
base::rm(Char__titles)

# 3.6. Define a new column "AgeCategory" based on "Title" and "Age" ________________
# This column will replace both features :
# In facts, "Age" has some missing values, and when it's a decimal number it means it has been estimated ;
# "Title" will be discarded since its information will summarized in "AgeCategory" .
Num__adulthood <- 21.0
Df__titanic$AgeCategory <- NA
purrr::map(
  .x = Df__titanic$PassengerId
  , .f = function(Char__passengerid){
    
    # Get "Age" value according to Char__passengerid, and convert it into a double
    Num__age <- Df__titanic |> dplyr::filter(PassengerId == Char__passengerid) |> dplyr::pull(Age) |> 
      base::as.double()
    
    # Verify if Num__age value is missing or not
    
    # If Num__age is NA
    if (base::is.na(Num__age)) {
      # Look for the according "Title"
      Char__title <- Df__titanic[Df__titanic$PassengerId == Char__passengerid, "Title"]
      # Use "Title" to determine the adulthood
      if (Char__title %in% Char__adulttitles) {
        Df__titanic[Df__titanic$PassengerId == Char__passengerid, "AgeCategory"] <<- "adult"}
      if (Char__title %in% Char__youngtitles) {
        Df__titanic[Df__titanic$PassengerId == Char__passengerid, "AgeCategory"] <<- "young"}
      if (Char__title %in% Char__ambiguoustitles) {
        Df__titanic[Df__titanic$PassengerId == Char__passengerid, "AgeCategory"] <<- "young/adult"}
    } # end if (base::is.na(Num__age))
    
    # If Num__age is not NA
    if (!base::is.na(Num__age)) {
      Df__titanic[Df__titanic$PassengerId == Char__passengerid, "AgeCategory"] <<- base::ifelse(
        test = (Num__age < Num__adulthood),  yes = "young", no = "adult")
    } # end if (!base::is.na(Num__age))
  }
  , .progress = T)

# Convert "AgeCategory" data type into factor
Df__titanic$AgeCategory <- base::factor(Df__titanic$AgeCategory)

# Plot "AgeCategory"
par(cex.axis = 0.55)
graphics::plot(base::factor(Df__titanic$AgeCategory), main = "AgeCategory")

# Cleanup
Df__titanic[, c("Title", "Age")] <- NULL
base::rm(Char__adulttitles, Char__youngtitles, Char__ambiguoustitles, Num__adulthood)

# 3.7. Create "FamilySize" ________________
# Firstly, we compute a family size by incrementing by 1 the sum of "SibSp" and "Parch" .
# Secondly, we create from the previous computation a categorical variable "FamilySize" having two modalities
Num__famsize <- base::as.integer(Df__titanic$SibSp) + base::as.integer(Df__titanic$Parch) + 1
Df__titanic$FamilySize <- base::factor(base::ifelse(test = (Num__famsize == 1), yes = "alone", no = "not alone"))

# Plot "FamilySize"
par(cex.axis = 0.55)
graphics::plot(base::factor(Df__titanic$FamilySize), main = "FamilySize")

# Cleanup
base::rm(Num__famsize)
Df__titanic[, c("SibSp", "Parch")] <- NULL

# 3.8. Impute "Embarked" missing values ________________

# Get the most present category in "Embarked"
Char__mode <- base::table(Df__titanic$Embarked) |> base::which.max() |> base::names()

# Impute missing values by the most present category in "Embarked"
Df__titanic[base::is.na(Df__titanic$Embarked), "Embarked"] <- Char__mode

# Convert "Embarked" data type into factor
Df__titanic$Embarked <- base::factor(Df__titanic$Embarked)

# Plot "Embarked"
par(cex.axis = 0.55)
graphics::plot(Df__titanic$Embarked, main = "Embarked")

# Cleanup
base::rm(Char__mode)

# 3.9. Retrieve some engineered versions of Df__train and Df__test, and prepare training data for models ________________

# Retrieve engineered version of Df__train
Df__engineeredtrain <- dplyr::right_join(
  x = Df__titanic, y = Df__train[, c("PassengerId", "Survived")], by = ("PassengerId" = "PassengerId")) |>
  base::as.data.frame()

# Convert "Survived" data type into integer
Df__engineeredtrain$Survived <- base::as.integer(Df__engineeredtrain$Survived)

# Remove "PassengerId" column from Df__engineeredtrain, then identify unique groups of samples
Df__engineeredtrain <- Df__engineeredtrain |> dplyr::group_by(Survived, Sex, Embarked, BoardingClass, AgeCategory, FamilySize) |> 
  dplyr::mutate(GroupSize = dplyr::n(), SimilarPassengerIds = base::paste(PassengerId, collapse = "__")) |> 
  dplyr::select(-PassengerId) |> dplyr::distinct() |> base::as.data.frame()

# Compute each group of passengers weight
Df__engineeredtrain$GroupWeight <- Df__engineeredtrain$GroupSize / base::sum(Df__engineeredtrain$GroupSize)

# Retrieve engineered version of Df__test
Df__engineeredtest <- dplyr::inner_join(
  x = Df__titanic, y = Df__test |> dplyr::select(PassengerId), by = ("PassengerId" = "PassengerId")) |>
  base::as.data.frame()

# Cleanup
base::rm(PassengerId, Df__titanic)

# Display "Survived" distribution (by passengers groups) in Df__engineeredtrain : check for any class imbalance
par(cex.axis = 0.55)
graphics::plot(base::factor(Df__engineeredtrain$Survived), main = "Survived")

# 3.10. Encode all categorical covariates in Df__engineeredtrain and Df__engineeredtest ________________

# Categorical features with two modalities
for (Char__feature in c("Sex", "AgeCategory", "FamilySize")) {
  
  # Get all values of Char__feature in Df__engineeredtrain
  Char__featurevalues <- Df__engineeredtrain[, Char__feature] |> base::as.character()
  
  # Choose a base modality which we will use to prepare our encoding
  Char__basemodality <- Char__featurevalues[1] # for example, the first value of Char__featurevalues
  
  # Encode modalities of Char__featurevalues in both Df__engineeredtrain and Df__engineeredtest
  Df__engineeredtrain[, base::paste0(Char__feature, "_", Char__basemodality)] <- base::ifelse(
    test = (Df__engineeredtrain[, Char__feature] == Char__basemodality), yes = 1, no = 0)
  Df__engineeredtest[, base::paste0(Char__feature, "_", Char__basemodality)] <- base::ifelse(
    test = (Df__engineeredtest[, Char__feature] == Char__basemodality), yes = 1, no = 0)
  
  # Cleanup
  Df__engineeredtrain[, Char__feature] <- NULL
  Df__engineeredtest[, Char__feature] <- NULL
  base::rm(Char__featurevalues, Char__basemodality)
  
} # end for (Char__feature in c("Sex", "AgeCategory", "FamilySize"))

# Cleanup
base::rm(Char__feature)

# Categorical features with three modalities
for (Char__feature in c("Embarked", "BoardingClass")) {
  
  # Get all values of Char__feature in Df__engineeredtrain
  Char__featurevalues <- Df__engineeredtrain[, Char__feature] |> base::as.character()
  
  # Choose two base modalities which we will use to prepare our encoding
  Char__basemodality1 <- base::unique(Char__featurevalues)[1] # for example, the first unique value of Char__featurevalues
  Char__basemodality2 <- base::unique(Char__featurevalues)[2] # for example, another unique value of Char__featurevalues
  
  # Encode modalities of Char__featurevalues in both Df__engineeredtrain and Df__engineeredtest
  for (Char__basemodality in c(Char__basemodality1, Char__basemodality2)) {
    Df__engineeredtrain[, base::paste0(Char__feature, "_", Char__basemodality)] <- base::ifelse(
      test = (Df__engineeredtrain[, Char__feature] == Char__basemodality), yes = 1, no = 0)
    Df__engineeredtest[, base::paste0(Char__feature, "_", Char__basemodality)] <- base::ifelse(
      test = (Df__engineeredtest[, Char__feature] == Char__basemodality), yes = 1, no = 0)
  } # end for (Char__basemodality in c(Char__basemodality1, Char__basemodality2))
  
  # Cleanup
  Df__engineeredtrain[, Char__feature] <- NULL
  Df__engineeredtest[, Char__feature] <- NULL
  base::rm(Char__featurevalues, Char__basemodality, Char__basemodality1, Char__basemodality2)
  
} # for (Char__feature in c("Embarked", "BoardingClass"))

# Cleanup
base::rm(Char__feature)



# __________________________________________________________________________________________________
# 4. Model "Survived" using an XGBOOST model

# Compared to a random forest where interactions are taken into account, feature selection is done,
# and bagging concept is used; the XGBOOST model introduces the gradient boosting to sequentially upgrade
# the predictive power of a decision tree.

# 4.1. Train the model (parameters chosen empirically to get a good cutoff between alive and dead people) ________________
# Don't forget to save the trained model
Char__target <- "Survived"
Char__covariates <- base::colnames(Df__engineeredtrain)[base::colnames(Df__engineeredtrain) %!in% c(Char__target, "GroupSize", "SimilarPassengerIds", "GroupWeight")]
Xgboost__model <- xgboost::xgboost(
  data = base::as.matrix(Df__engineeredtrain[, Char__covariates])
  , label = Df__engineeredtrain[, Char__target]
  , subsample = 0.8 # only use 80% of the training set to train the model at each iteration to prevent overfitting 
  , objective  = "binary:logistic"
  , nrounds = 50
  , verbose = 1
  , tree_method = "exact"
  , nthread = 1)

# 4.2. Analyze death probabilities proposed by the trained model ________________
Df__engineeredtrain$DeathProbability <- 1 - stats::predict(object = Xgboost__model, base::as.matrix(Df__engineeredtrain[, Char__covariates]))

# 4.3. Explore predicted death probabilities by "Survived" modalities
# A visualization
par(cex.axis = 0.55)
graphics::boxplot(formula = DeathProbability~Survived, data = Df__engineeredtrain, main = "Death probabilities by Survived/Dead People")
# A table summary
Df__results <- Df__engineeredtrain |>
  dplyr::select(DeathProbability, Survived) |>
  dplyr::group_by(Survived) |>
  dplyr::summarise(
    group_count = n()
    , mean = base::mean(x = DeathProbability, na.rm = TRUE)
    , std = stats::sd(x = DeathProbability, na.rm = TRUE)
    , min = base::min(x = DeathProbability, na.rm = TRUE)
    , `25%` = stats::quantile(x = DeathProbability, 0.25, na.rm = TRUE)
    , `50%` = stats::median(x = DeathProbability, na.rm = TRUE)
    , `75%` = stats::quantile(x = DeathProbability, 0.75, na.rm = TRUE)
    , max = base::max(x = DeathProbability, na.rm = TRUE)) |> base::as.data.frame()

# 4.4. Choose an adequate cutoff to discriminate dead from alive people ________________
# Do the optimization
optimx__object <- optimx::optimx(
  par = c(0.5)  # Initial guess for Num__threshold : Num__threshold0
  , fn = Function__wrapper
  , lower = 0  # Lower bound for Num__threshold
  , upper = 1  # Upper bound for Num__threshold
  , method = "L-BFGS-B"
  , control = base::list(maxit = 1000))
# Print the optimization results
print(optimx__object)
# Extract the optimal threshold
Num__cutoff <- optimx__object$p1[1]
print(paste("Optimal threshold:", Num__cutoff))

# 4.5. Analyze Xgboost__model accuracy on the training set ________________
Df__engineeredtrain$Survived_predicted <- base::ifelse(test = (Df__engineeredtrain$DeathProbability < Num__cutoff), yes = 1, no = 0)
base::print(
  base::paste(
    "Overall Accuracy on the training set: "
    , base::round(x = 100*base::sum(Df__engineeredtrain[(Df__engineeredtrain$Survived_predicted == Df__engineeredtrain$Survived), "GroupSize"])/base::sum(Df__engineeredtrain$GroupSize), digits = 2)
    , "%"))
base::print(
  base::paste(
    "Overall Accuracy on the actual dead people of the training set: "
    , base::round(x = 100*base::sum(Df__engineeredtrain[(Df__engineeredtrain$Survived_predicted == Df__engineeredtrain$Survived & Df__engineeredtrain$Survived == 0), "GroupSize"])/base::sum(Df__engineeredtrain[Df__engineeredtrain$Survived == 0, "GroupSize"]), digits = 2)
    , "%"))
base::print(
  base::paste(
    "Overall Accuracy on the actual alive people of the training set: "
    , base::round(x = 100*base::sum(Df__engineeredtrain[(Df__engineeredtrain$Survived_predicted == Df__engineeredtrain$Survived & Df__engineeredtrain$Survived == 1), "GroupSize"])/base::sum(Df__engineeredtrain[Df__engineeredtrain$Survived == 1, "GroupSize"]), digits = 2)
    , "%"))

# 4.6. Do predictions on Df__engineeredtest ________________
Df__engineeredtest$DeathProbability <- 1 - stats::predict(object = Xgboost__model, base::as.matrix(Df__engineeredtest[, Char__covariates]))
Df__engineeredtest$Survived <- base::ifelse(test = (Df__engineeredtest$DeathProbability < Num__cutoff), yes = 1, no = 0)
Df__test <- dplyr::inner_join(x = Df__test, y = Df__engineeredtest[, c("PassengerId", "Survived")], by = ("PassengerId" = "PassengerId")) |> dplyr::select(all_of(c("PassengerId", "Survived"))) |> base::as.data.frame()
utils::write.csv(Df__test, "data/test/test_pred.csv", row.names = FALSE)

# __________________________________________________________________________________________________
# Cleanup, and empty garbage collector
base::rm(list = ls())
base::gc()
