FROM rocker/r-ver:4.1.3
ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git \
  libcurl4-openssl-dev \
  libgit2-dev \
  libicu-dev \
  libsodium-dev \
  libssl-dev \
  make

RUN R -e "install.packages(c('plumber', 'dplyr', 'tidymodels'))"

WORKDIR /hfx-bike-ridership-ai

COPY api.R api.R
COPY bike-count.rds bike-count.rds
COPY bike-xgb-fit.rds bike-xgb-fit.rds

#ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/opt/ml/plumber.R'); pr$run(host = '0.0.0.0', port = as.numeric(Sys.getenv("PORT")))"]
