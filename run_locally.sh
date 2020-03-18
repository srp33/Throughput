#! /bin/bash

#R -e "install.packages('tidyverse', repos='https://cloud.r-project.org')"
R -e "install.packages(c('shinydashboard', 'shinyjs', 'shinycssloaders', 'shinythemes', 'shinyFiles', 'tidycwl', 'ymlthis'), repos='https://cloud.r-project.org')"

R -e "shiny::runApp('.')"
