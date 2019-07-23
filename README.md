# stableRoommatesApp
An R Shiny deployment for solving the stable roommates problem.

Original deployment is here: https://zxtonizx.shinyapps.io/students/

To deploy this in your own R Shiny instance create an R file with the following script and run:


#install libraries
library(matchingMarkets)
library(shiny)
library(rJava)
library(rsconnect)
library(rowr)
library(readxl)
library(xlsx)
library(dplyr) 
library(devtools) 

#connect to your R Shiny accouny
rsconnect::setAccountInfo(name='INSERT_NAME_EHERE',
                          token='INSERT_TOKEN_HERE',
                          secret='INSERT_SECRET_HERE')

#connect to matchingMarkets devtools
devtools::install_github("thiloklein/matchingMarkets")

#deploy
rsconnect::deployApp("INSERT_PATH_HERE_CONTAINING_SERVER_UI_EXAMPLE_FILES")
y #yes please deploy!
