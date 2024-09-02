# Titanic Survival Prediction Project

This project aims to predict passenger survival on the Titanic using machine learning techniques. It includes implementations in both Python and R.

## Project structure

```

titanic/
├──── .gitignore
├──── LICENSE
├──── README.md
├──── PYTHON/
│      ├──── requirements.txt
│      ├──── data/
│      │       ├──── train/
│      │       │       └──── train.csv
│      │       └──── test/
│      │               └──── test.csv
│      └──── scripts/
│              └──── main.ipynb
└──── R/
      ├──── renv.lock
      ├──── R.Rproj
      ├──── data/
      │       ├──── train/
      │       │       └──── train.csv
      │       └──── test/
      │               └──── test.csv
      └──── scripts/
              └──── main.R

```

## Getting Started

### Data

The data is split into training and test sets, located in the respective `data/train` and `data/test` directories for both Python and R implementations.

### Scripts

- Python: `main.ipynb` is a jupyter notebook which contains the main analysis and model training.
- R: `main.R` is a R script which contains the R implementation of the analysis and modeling.

The `main.ipynb` provides more details than the `main.R` file. Both scripts don't implement same models. 

The `main.ipynb` notebook focuses on implementing `logistic regression` and`random forest` models; even a `mix models of these two models` is proposed.

The `main.R` script focuses on implementing a `xgboost tree model`. 

In our case, our work shows that a simple `logistic regression`, `random forest` or even a `mix models of these two models` can provide better results than a `xgboost tree model`.

### Python

1. Make sure to install `python version 3.12.4` on your machine.
2. Make sure to install the IDE `Visual Studio Code (VSCode)` on your machine (any version that can support `python version 3.12.4` is okay).
3. Open the folder `PYTHON` with your VSCode IDE.
4. In your VSCode terminal, make sure to create a virtual environment named `.venv` in the folder `PYTHON` with:

      ```python -m venv .venv```

5. In your VSCode terminal, activate your `.venv` environment, and install required packages with: 

      ```
      .venv\Scripts\activate
      pip install -r requirements.txt
      ```

6. Open and run (cell by cell or all cells) the `main.ipynb` notebook using your created virtual environment `.venv` to visualize results.
7. At the end of running all the notebook cells, your local folder `PYTHON\data\test` is populated by a .csv file named `test_pred.csv`.

### R

1. Make sure to install `R version 4.4.1` on your machine.
2. Make sure to install the IDE `RStudio Desktop` on your machine (any version that can support `R version 4.4.1` is okay).
3. Go to your folder `R`, and open the `R.Rproj` file in RStudio (you are opening a project named R).
3. Make sure to have installed the package `renv` in your R environment:

      ```install.packages("renv", dependencies = TRUE)```

4. Using your R Console, make sure to install required packages with:

      ```renv::restore()```

5. Run (line by line or all lines) of the `main.R` script in your R Console.
6. At the end of running all the lines of the `main.R` script, your local folder `R\data\test` is populated by a .csv file named `test_pred.csv`.

## License
This project is licensed under the terms of the LICENSE file included in this repository.

## Acknowledgments
- Kaggle for providing the Titanic dataset.
- Any other acknowledgments or credits are for people who developed packages and programming languages used to realize this project.
