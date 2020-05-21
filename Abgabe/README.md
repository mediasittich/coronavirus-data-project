# COVID-19 Praxisprojekt

## Project Organization

---

    ├── README.md          <- The top-level README for developers using this project.
    ├── data               <- Data raw or processed used in this project.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── Bericht            <- Generated analysis as HTML, PDF, LaTeX, etc.
    ├── Presentation       <- Presentaltion slides as HTML, PDF, LaTeX, etc.
    ├── figures            <- Generated graphics and figures to be used in reporting/slides.
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   └── make_dataset.py     <- Scripts to download or generate data
    │   │
    │   └── build_features.py   <- Scripts to turn raw data into features for modeling
    │   │
    │   └── visualize.py        <- Scripts to create exploratory and results oriented visualizations

---

## Python Scripts

### Creating a virtual environment

Create a virtual environment to run Python code with one of the following options:

#### With the built-in Python module `venv`

On **_UNIX systems_** (Linux, MacOS):

```
python3 -m venv /path/to/new/virtual/environment
```

On **_Windows_**, invoke the venv command as follows:

```
c:\>c:\Python35\python -m venv c:\path\to\myenv
```

Alternatively, if you configured the PATH and PATHEXT variables for your Python installation:

```
c:\>python -m venv c:\path\to\myenv
```

#### With Conda

```
conda create --name PROJECT_NAME python=3
```

### Activating the environment

To activate the virtual environment use:

- for **_built-in Python_** module:  
  `source /path/to/new/virtual/environment/bin/activate`
- for **_Conda_**:  
  `conda activate PROJECT_NAME`

To deactivate a virtual environment by typing `deactivate` or `conda deactivate` for a Conda environment in your shell.

### Installing development requirements

```
pip install -r requirements.txt
```

### Run script

```
python script.py
```
