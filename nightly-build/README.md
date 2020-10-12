# Code for Canada COVID-19 school tracker
This is the code processing map data from the Canada COVID-19 school tracker.

Visit the tracker: 

* Website: https://tinyurl.com/covidschoolsCA-map 
* Twitter: @covidschoolsCA
* Facebook: @covidschoolsCA

## Workflow
School data is entered on a Google map. This code runs the nightly processing of the map, and generation of analysis + social media output. 

## Nightly Build Code
In `nightly-build/`:

1. `run-nightly-build.sh`: Google map data is downloaded for @covidschoolsCA and @CovidEcoles (QC tracker) in KML format, cleaned and converted to tables. KML files require some cleaning and parsing, this is done here.

2. `makePlots.R`: Generates charts for daily am report; also generates daily tweet thread (`genTweets.R`). Plot graphics use R `ggplots` library.

3. `schoolBoard.R`: Generates graphs of case breakdown by school board.

# Credits
@covidschoolsCA is a volunteer-based team project run by advocacy group [Masks4Canada](masks4canada.org). This code is authored by Shraddha Pai (@spaiglass).


