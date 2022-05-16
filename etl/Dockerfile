FROM rocker/tidyverse:latest

RUN R -e "install.packages(c('bigrquery', 'httr'), repos = 'http://cran.us.r-project.org')"

ADD oauth-client.json /home/rstudio
ADD etl/etl.R /home/rstudio

CMD Rscript /home/rstudio/etl.R
