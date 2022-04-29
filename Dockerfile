# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    # for the sql database
    libsqlite3-dev \
    # for units package
    libudunits2-dev \
    # for libdgal-dev that follows
    libmysqlclient-dev \
    # for sf package
    libgdal-dev \
    libxml2-dev \
    libcairo2-dev \
    # for the sql database
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## copy all Sus folder to the root
COPY . .
## renv.lock file
COPY ./renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# run app on container start
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]