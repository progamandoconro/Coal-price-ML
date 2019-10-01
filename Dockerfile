FROM rocker/rstudio
RUN apt install git -y && git clone https://github.com/progamandoconro/termoelectric
RUN echo "install.packages(c('dplyr','zoo','Rtsne','plotly','ggplot2','shiny','shinydashboard','shinyWidgets'))" > \
packages_R.R && Rscript packages_R.R
WORKDIR /termoelectric
RUN Rscript app.R

#docker build . -t app
#docker run --rm -p 8787:8787 -e PASSWORD=yourpasswordhere app
