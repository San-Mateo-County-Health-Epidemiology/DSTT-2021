# DSTT-2021
DSTT RShiny App

Created by: Elizabeth Jump, Corina Chung + Edwina Williams

This project downloads NSSP ESSENCE data for Mental Health conditions and creates an R Shiny application to display trends over time and year to date. You can copy this repository and create your own version with specific conditions you're interested in. To do this, you need to do just a couple things:

1. You should log into NSSP, create your own query of interest and get the API for that query. See details here: https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html
- Then you can open the script called: function_pull_essence_api_with_start_and_end_dates and replace the URL there with the URL you've copied from NSSP. Make sure to remove the dates from copied URL and replace them with the start_date and end_date variables
2. Store your NSSP password using the keyring package so that you can run the API from your computer
3. In the download_and_process_historical_essence_data and download_and_process_ytd_essence_data scripts, update the disorders_of_interest variable with a pipe (|) delimited list of the disorders you are looking at 
