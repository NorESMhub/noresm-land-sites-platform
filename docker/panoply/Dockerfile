FROM jlesage/baseimage-gui:ubuntu-20.04

RUN apt update && apt install -y openjdk-11-jre

ENV JAVA_HOME /opt/jdk

COPY PanoplyJ-5.2.0.tgz /tmp

RUN tar -xvf /tmp/PanoplyJ-5.2.0.tgz --directory /opt

RUN echo "#!/bin/sh\nexec bash /opt/PanoplyJ/panoply.sh" > /startapp.sh

RUN cat /startapp.sh

WORKDIR /resources
