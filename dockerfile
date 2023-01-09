FROM rocker/tidyverse:latest
COPY . /secretariasystem
WORKDIR /secretariasystem
EXPOSE 5024
RUN install2.r --error \
  igraph \
  shiny \
  shinydashboard \
  shinydashboardPlus \
  shinymanager \
  readr \
  plyr \
  dplyr \
  tidyr \
  readxl \
  RMySQL \
  openxlsx \
  shinyWidgets \
  shinycssloaders \
  shinyjs \
  stringr \
  shinyBS \
  shinyanimate \
  waiter \
  googlesheets4 \
  magick \
  DT \
  blastula \
  htmlwidgets \
  fontawesome
RUN Rscript -e 'install.packages("highcharter")'
CMD ["Rscript", "app.R"]