# Coronavirus Data Project

This repository contains a statistical analysis of COVID-19 data which was created as a practice project during the course _'Praxisprojekt'_ in the winter semester 2019/2020 at the [Department of Statistics, LMU Munich](https://www.en.statistik.uni-muenchen.de/index.html).

In the scope of the project we analysed timeseries data of COVID-19 cases world-wide.

Different data sources were used to enhance the main dataset with additional features, e.g. population or geographic data.

As the main datasource the dataset compiled by RamiKrispin was used, which can be found here: [https://github.com/RamiKrispin/coronavirus](https://github.com/RamiKrispin/coronavirus)  
List and description of additional datasets used can be found in the `README.md`in subdirectory _Abgabe_.

Our final presentation and a summary of the project can also be found in this repository. Latter also contains a detailed list of our data sources.

These additional public datasets were consulted for gathering ideas:

- global: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
- Germany: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data

We used the R and Python programming languages to conduct the analysis and generate all figures.

## Project Structure

```
.
├── Abgabe      <- Contains code for analysis, datasets, figures, presentation slides & summary.
├── Bericht     <- TeX files for report & bibliography.
├── Presentation      <- TeX files for presentation slides (original version 08.05.2020).
├── Presentation_latest    <- TeX files for presentation slides (revised version).
├── .gitignore          <- Ignore file to exclude unnecessary files from Git tracking.
└── README.md
```

## To reproduce our analysis

- clone this repo with `git clone https://github.com/mediasittich/coronavirus-data-project.git`
- change to the directory _Abgabe_ by typing `cd Abgabe`
- To run the R script open the file `Rscript.R` in RStudio
- To run the Python scripts follow the instructions in the `README.md` file in the _Abgabe_ directory

---

## _Bonus_

During this project we started working on a web application to display our analysis interactively. The app project is currently a work in progress and may also contain additional data resources not used in this analysis.  
The current live version of the app can be found [here](https://covid-19-stats-project.herokuapp.com/). The corresponding repository [here](https://github.com/mediasittich/covid19-dash-app).