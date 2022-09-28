FROM rocker/shiny:4.1.0
LABEL description="Image for DiMmer"
LABEL maintainer="Alexander Dietrich"

RUN apt-get update && apt-get install -y \
  wget \
  less


# install JDK8 including JFX
WORKDIR /opt
ENV JDK_VERSION 11.0.16.1
RUN wget https://cdn.azul.com/zulu/bin/zulu11.58.23-ca-jdk${JDK_VERSION}-linux_x64.tar.gz && tar -xzvf zulu11.58.23-ca-jdk${JDK_VERSION}-linux_x64.tar.gz && chmod 777 zulu11.58.23-ca-jdk${JDK_VERSION}-linux_x64/*
ENV PATH=$PATH:/opt/zulu11.58.23-ca-jdk${JDK_VERSION}-linux_x64/bin

# download DiMmer jar
WORKDIR /bin
RUN wget https://github.com/baumbachlab/Dimmer/releases/download/2.2/dimmer.jar 

# setup renv to handle R packages
ENV RENV_VERSION 0.15.5
RUN R -e "install.packages(c('remotes'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# install all packages
COPY renv.lock renv.lock
RUN R -e "renv::restore()"

# copy code and configs into container
#COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
#COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY /R /srv/shiny-server/R
COPY /www /srv/shiny-server/www
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/R/', host = '0.0.0.0', port = 3838)"]
