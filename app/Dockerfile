FROM rocker/shiny:4.4.2

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git \
  libcurl4-openssl-dev \
  libgit2-dev \
  libicu-dev \
  libsodium-dev \
  libssl-dev \
  make

RUN R -e "install.packages(c('shinydashboard', 'dplyr', 'readr', 'tidyr', \
  'ggplot2', 'patchwork', 'broom', 'workflows', 'recipes', 'xgboost', \
  'bigrquery', 'googleCloudStorageR', \
  'DT', 'remotes', 'showtext', 'showtextdb'))"

RUN R -e "remotes::install_github('taylordunn/dunnr')"

COPY shiny-custom.config /etc/shiny-server/shiny-server.conf
COPY oauth-client.json /srv/shiny-server/
COPY service-account-key.json /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY funcs.R /srv/shiny-server/
COPY www/ /srv/shiny-server/www/

RUN chown shiny:shiny /var/lib/shiny-server

EXPOSE 3838

USER shiny

CMD ["/usr/bin/shiny-server"]
