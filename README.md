# My Chi. My Future. Project
This is my final project for the Data Science Project course at Northwestern University. </p>
[Project Product Link](https://2r7ec0-emma-yu.shinyapps.io/mcmf_map/) </p>
My CHI. My Future. is an initiative launched in May 2020 by the mayoral office and the City of Chicago to create a community network that connects young people and local organizations through a variety of youth programs. MCMF programs provide young people, ranging from the ages 0-24, with opportunities to participate in activities related to academic advancement, professional and personal development, art, wellness, and leisure. </p>
We were asked to use our data science skill sets to evaluate the program's equitability and create product that can improve MCMF program equity.

## Objective
* Clean real-life, messy data to suit our objectives
* Conduct an exploratory data analysis (EDA) 
* Create an interactive visualization that displays level of MCMF program equity across the Chicago neighborhoods
* Train a text classification model that predicts the category of MCMF programs
* Conduct a time series analysis to forecast MCMF program count
* Provide research-based recommendations on improving MCMF equity to client

## Skills Displayed in This Project
* R, Python
* Extensive data wrangling
* Geocoding
* Exploratory data analysis (EDA)
* Interactive data visualization using `ggplot2` and `shiny`
* Text data processing using `re`, `string`, and `nltk`
* Natural language processing (NLP) model training using `scikit-learn`
* Time series analysis and forecasting using `tseries`, `forecast`, and `rugarch`
* Client-based production

## File Outline
`cleaning/`: folder containing R scripts used to clean data </p>
`data/`: folder containing files for the datasets used in the project </p>
`eda/`: folder containing EDA files, see `eda/EDA.md` for compiled report </p>
`map/`: folder containing Shiny visualization app files </p>
`model/`: folder containing modeling (time series & text classification) files, see `model/model_report.md` for compiled report </p>
`MCMF-project.Rproj`: R project setting storage </p>

## How to Use
The entire project is compiled into one Shiny app. You can explore this project's EDA, interactive visual map, and modeling process through this [link](2r7ec0-emma-yu.shinyapps.io/mcmf_map/). </p>

If you would like to view my reports, you can check out [`eda/EDA.md`](https://github.com/emmayu18/MCMF-project/blob/main/eda/EDA.md), [`model/text_classification.ipynb`](https://github.com/emmayu18/MCMF-project/blob/main/model/text_classification.ipynb), and [`model/model_report.md`](https://github.com/emmayu18/MCMF-project/blob/main/model/model_report.md) files. All of my codes are saved in the various R script files.

## Acknowledgement
This project was done in collaboration with Shruti Rathnavel and Rhona Zhang. </p>
Data citation: </p>
[My Chi. My Future. Data](https://data.cityofchicago.org/Events/My-CHI-My-Future-Programs/w22p-bfyb/data) </p>
[Chicago Public School Annual Regional Analysis Data](https://www.cps.edu/sites/ara)</p>
[Chicago Community Boundary GeoJSON](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6)</p>
