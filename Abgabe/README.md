# COVID-19 Praxisprojekt Abgabe

## Project Organization

---

    ├── README.md          <- The README for developers using this project.
    ├── data               <- Data raw or processed used in this project.
    ├── figures            <- Generated graphics and figures to be used in reporting/slides.
    ├── reports            <- Generated analysis and presentation slides as PDF.
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   └── make_dataset.py     <- Scripts to download or generate data
    │   │
    │   └── build_features.py   <- Scripts to apply statistical transformations on raw data
    │   │
    │   └── visualize.py        <- Scripts to create exploratory and results oriented
    │
    ├── Rscript.R           <- Main R script for analysis.
    ├── requirements.txt    <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    └── script.py           <- Main Python script for analysis.

_Used datasets (version 22.05.2020) are also available in the 'data' folder, although the scripts import the latest available datasets via provided URLs_

---

## Datasets in _data_ directory

The main data source for COVID-19 case data (both compiled by [https://github.com/RamiKrispin](https://github.com/RamiKrispin))

- `coronavirus_dataset.csv`: used in Python scripts
- `coronavirus.csv`: dataset used in R package, different naming of columns

Population and Geographic data:

- `country-and-continent-codes-list-csv_csv.csv`: dataset containing countries with corresponding continents, published by _JohnSnowLabs_ on [DataHub.io](https://datahub.io/JohnSnowLabs/country-and-continent-codes-list)
- `WPP2019_TotalPopulationBySex.csv`: population data from the [UN World Population Prospects 2019](https://population.un.org/wpp/Download/Standard/CSV/), used in Python script

Government Response data from [Government Response Tracker](https://github.com/OxCGRT/covid-policy-tracker) dataset compiled by the University of Oxford

- `OxCGRT_latest.csv`: list of response indicators for each country and day
- `c6_stayathomerequirements.csv`: timeseries for the indicator _C6 Stay at home requirements_ for each country and day
- `c6_start_dates.csv`: A set of European countries with the starting dates of the implementation of _C6_ response

Additional data for map creation

- `ne_10m_admin_0_countries`: directory contains shapefiles used to generate world maps.

## Python Scripts

### Creating a virtual environment

Create a virtual environment to run Python code with one of the following options:

#### With the built-in Python module `venv`

On **_UNIX systems_** (Linux, MacOS):

```
python3 -m venv /path/to/myenv
```

On **_Windows_**, invoke the venv command as follows:

```
c:\>c:\Python38\python -m venv c:\path\to\myenv
```

Alternatively, if you configured the PATH and PATHEXT variables for your Python installation:

```
c:\>python -m venv c:\path\to\myenv
```

#### With Conda

```
conda create --name myenv python=3
```

### Activating the environment

To activate the virtual environment use:

- for **_built-in Python_** module:  
  `source /path/to/myenv/bin/activate`
- for **_Conda_**:  
  `conda activate myenv`

To deactivate a virtual environment type

- `deactivate` for the built-in Python module `venv`
  or
- `conda deactivate` for a Conda environment

in your shell.

### Installing required packages

```
pip install -r requirements.txt
```

### Run Python script

```
python script.py
```
