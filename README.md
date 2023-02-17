# Canada COVID-19 school tracker

This repo contains code used to compile nightly data builds for the COVID-19 Schools Canada project (CSC), which crowdsourced COVID-19 cases and outbreaks from schools across Canada in the 2020-2021 school year. 

* [Website](https://covidschoolscanada.org/)
* See the [final interactive map](https://covidschoolscanada.org/map.html) for the 2020-21 school year. 

## Workflow
School data is entered in Province-level spreadsheets on the CSC Google. This code runs the nightly processing of the map, and generation of analysis + social media output. 

## Nightly Build Code
In `nightly-build/`:

1. `run-nightly-build.sh`: Google API is used to fetch Province-level manually entered data from the team's Google spreadsheets and web-scraped data from the team's Dropbox account. Data for Quebec is downloaded from the COVID Ecoles Quebec Google map in KML format, cleaned and converted to tables. KML files require some cleaning and parsing, this is done here.

2. `makePlots.R`: Generates charts for daily am report; also generates daily tweet thread (`genTweets.R`). Plot graphics use R `ggplots` library.

3. `schoolBoard.R`: Generates graphs of case breakdown by school board.

# About
COVID-19 Schools Canada was a grassroots volunteer project formed by a sub-team within the advocacy group [Masks4Canada](masks4canada.org). Its goal was promote evidence-based public health policies for risk mitigation in Canadian schools. The project was started in September 2020 and ended in June 2021.

To cite COVID-19 Schools Canada or the use of this code, please use: 
Atienza J., Benedict A. et al. (2023). 14 Quick Tips for Crowdsourcing Geographically-Linked Data for Public Health Advocacy. bioRXiv citation TBA. 

For questions about the code or project, please contact [Shraddha Pai](mailto:shraddha.pai@utoronto.ca). 


# Disclaimer
This code is provided on an "AS IS" basis.
