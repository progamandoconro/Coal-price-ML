#docker build . -t app
#docker run --rm -p 8787:8787 -e PASSWORD=yourpasswordhere App

FROM rocker/rstudio

RUN apt install git -y
RUN echo "install.packages(c('dplyr','zoo'))" > packages_R.R && Rscript packages_R.R

RUN  git clone https://github.com/progamandoconro/termoelectric

RUN echo "install.packages(c('Rtsne'))" > packagesML.R && Rscript packagesML.R
RUN echo "install.packages(c('plotly','ggplot2'))" > packagesDL.R && Rscript packagesDL.R
RUN echo "install.packages(c('shiny','shinydashboard','shinyWidgets'))" > packages_compl.R && Rscript packages_compl.R

RUN Rscript /termoelectric/app.R


