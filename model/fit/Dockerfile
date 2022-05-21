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

RUN R -e "install.packages(c('plumber', 'dplyr', 'readr', 'tidymodels', 'xgboost', 'bigrquery', 'googleCloudStorageR', 'googleCloudRunner'))"

COPY oauth-client.json .
COPY model/fit/fit.R .
COPY model/preprocess.R .

ENTRYPOINT ["Rscript", "-e", "pr <- plumber::plumb('fit.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')), swagger = TRUE)"]
CMD ["fit.R"]
