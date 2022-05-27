FROM rocker/shiny:4.2.0

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git \
  libcurl4-openssl-dev \
  libgit2-dev \
  libicu-dev \
  libsodium-dev \
  libssl-dev \
  make

RUN R -e "install.packages(c('shinydashboard', 'dplyr', 'readr', 'tidyr', \
  'ggplot2', 'patchwork', 'workflows', 'recipes', 'xgboost', \
  'bigrquery', 'googleCloudStorageR', \
  'DT', 'remotes', 'showtext', 'showtextdb'), \
  repos = 'https://packagemanager.rstudio.com/cran/__linux__/focal/2022-05-26')"

RUN R -e "remotes::install_github('taylordunn/dunnr')"

COPY app/shiny-custom.config /etc/shiny-server/shiny-server.conf
COPY oauth-client.json /srv/shiny-server/
COPY app/app.R /srv/shiny-server/
COPY app/funcs.R /srv/shiny-server/
COPY app/www/ /srv/shiny-server/www/
COPY app/fonts/ /srv/shiny-server/fonts/

EXPOSE 3838

USER shiny

CMD ["/usr/bin/shiny-server"]
