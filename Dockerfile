FROM ibmjava:8-sdk
MAINTAINER Shisei Hanai<shanai@jp.ibm.com>

RUN apt-get update
RUN apt-get install apt-transport-https tesseract-ocr -y

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN apt-get update
RUN apt-get install sbt -y

RUN mkdir -p /opt/lightocr
ADD project /opt/lightocr/project
ADD src /opt/lightocr/src
ADD build.sbt /opt/lightocr
ADD version.sbt /opt/lightocr

RUN cd /opt/lightocr && \
  sbt universal:packageZipTarball

RUN cd /opt/lightocr/target/universal && \
  cmd=$(basename *.tgz .tgz) && \
  tar xf $cmd.tgz && \
  echo "#!/bin/bash -xe" > /opt/lightocr/lightocr.sh && \
  echo printenv >> /opt/lightocr/lightocr.sh && \
  echo "ls -lh /opt/lightocr" >> /opt/lightocr/lightocr.sh && \
  echo /opt/lightocr/target/universal/$cmd/bin/lightocr -J-Xmx512m \$\* >> /opt/lightocr/lightocr.sh && \
  chmod +x /opt/lightocr/lightocr.sh

ENTRYPOINT ["/opt/lightocr/lightocr.sh"]
