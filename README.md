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







