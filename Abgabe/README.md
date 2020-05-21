# COVID-19 Praxisprojekt

## Project Organization

---

    ├── README.md          <- The top-level README for developers using this project.
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
    ├── .gitignore          <- Ignore file to exclude unnecessary files from Git tracking.
    ├── Rscript.R           <- Main R script for analysis.
    ├── requirements.txt    <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    └── script.py           <- Main Python script for analysis.

---

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
