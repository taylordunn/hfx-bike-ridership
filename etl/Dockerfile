FROM rocker/tidyverse:latest

ENV PASSWORD=123
ENV PORT=8787

RUN R -e "install.packages(c('bigrquery', 'httr'), repos = 'http://cran.us.r-project.org')"

ADD hfx-bike-ridership.json /home/rstudio
ADD etl/etl.R /home/rstudio

CMD Rscript /home/rstudio/etl.R
