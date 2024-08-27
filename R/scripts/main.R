# __________________________________________________________________________________________________
# Empty garbage collector
base::gc()



# __________________________________________________________________________________________________
# 1. Load required packages
List__packages <- base::list("readr", "dplyr", "purrr")
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

# 3.9. Retrieve engineered versions of Df__train and Df__test, and prepare training data for models ________________

# Retrieve engineered version of Df__train
Df__engineeredtrain <- dplyr::right_join(
  x = Df__titanic, y = Df__train[, c("PassengerId", "Survived")], by = ("PassengerId" = "PassengerId")) |>
  base::as.data.frame()

# Convert "Survived" data type into factor
Df__engineeredtrain$Survived <- base::factor(Df__engineeredtrain$Survived)

# Remove "PassengerId" column from Df__engineeredtrain, then identify unique groups of samples
Df__engineeredtrain <- Df__engineeredtrain |> dplyr::group_by(Survived, Sex, Embarked, BoardingClass, AgeCategory, FamilySize) |> 
  dplyr::mutate(GroupSize = dplyr::n(), SimilarPassengerIds = base::paste(PassengerId, collapse = "__")) |> 
  dplyr::select(-PassengerId) |> dplyr::distinct() |> base::as.data.frame()

# Compute a "GroupWeight"
Df__engineeredtrain$GroupWeight <- Df__engineeredtrain$GroupSize / base::sum(Df__engineeredtrain$GroupSize)

# Retrieve engineered version of Df__test
Df__engineeredtest <- dplyr::inner_join(
  x = Df__titanic, y = Df__test |> dplyr::select(PassengerId), by = ("PassengerId" = "PassengerId")) |>
  base::as.data.frame()

# Cleanup
base::rm(PassengerId, Df__titanic)

# Display "Survived" distribution (by passengers groups) in Df__engineeredtrain
par(cex.axis = 0.55)
graphics::plot(Df__engineeredtrain$Survived, main = "Survived")



# __________________________________________________________________________________________________
# 4. Model "Survived" : different models from those in the main.ipnyb will be explored

# 4.1. SVM model ________________


# 4.2. XGBOOST model ________________




