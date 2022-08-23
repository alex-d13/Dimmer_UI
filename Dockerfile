FROM ubuntu:20.04
LABEL description="Image for DiMmer"
LABEL maintainer="Alexander Dietrich"

RUN apt-get update && apt-get install -y wget


# install JDK8 including JFX
WORKDIR /opt
ENV JDK_VERSION 8.0.332
RUN wget https://cdn.azul.com/zulu/bin/zulu8.62.0.19-ca-fx-jdk${JDK_VERSION}-linux_x64.tar.gz && tar -xzvf zulu8.62.0.19-ca-fx-jdk${JDK_VERSION}-linux_x64.tar.gz && chmod 777 zulu8.62.0.19-ca-fx-jdk${JDK_VERSION}-linux_x64/*
ENV PATH=$PATH:/opt/zulu8.62.0.19-ca-fx-jdk${JDK_VERSION}-linux_x64/bin

# download DiMmer jar
WORKDIR /home
RUN wget https://github.com/baumbachlab/Dimmer/releases/download/2.1/dimmer.jar 

CMD ["java","-jar","/home/dimmer.jar"]