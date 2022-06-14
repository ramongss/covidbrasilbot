FROM rocker/r-ver:4.1.2

RUN echo "options(Ncpus = $(nproc --all))" >> /usr/local/lib/R/etc/Rprofile.site

RUN mkdir -p ~/.local/share/renv

RUN R -e "install.packages('renv')"

RUN apt-get update

RUN apt-get install -y locales-all libxt6

RUN Rscript -e "install.packages(c(\
'readr',\
'dplyr',\
'zoo',\
'ggplot2',\
'scales',\
'ggpubr',\
'here',\
'rtweet'))"

RUN R -e "renv::restore()"

ADD . /home/covidbrasilbot
WORKDIR /home/covidbrasilbot

ENV LC_TIME pt_BR.UTF-8

CMD ["Rscript", "R/covidbrasilbot-tweet.R"]