# FROM tesseractshadow/tesseract4re
FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive

# for R package Ubuntu 16.02
# RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu bionic/" | tee -a /etc/apt/sources.list
# RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
# RUN gpg -a --export E084DAB9 | apt-key add -

# for R package Ubuntu 18.04
RUN apt-get update && apt install -y apt-transport-https software-properties-common
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'

# Install Tesseract, R and RTesseract dependencies
RUN apt-get install -y tzdata
RUN apt-get update && apt-get install -y tesseract-ocr libtesseract-dev libleptonica-dev git r-base \
    libcurl4-openssl-dev libssl-dev libxml2-dev httpie make

WORKDIR /opt
RUN git clone https://github.com/duncantl/Rtesseract.git
RUN R -e 'install.packages("/opt/Rtesseract", repos = NULL, type="source")'

# Install Script dependencies
RUN R -e 'install.packages("dplyr")'
RUN R -e 'install.packages("gsubr")'
RUN R -e 'install.packages("tidyverse")'
RUN R -e 'install.packages("jpeg")'
RUN R -e 'install.packages("stringr")'
RUN R -e 'install.packages("cluster")'
RUN R -e 'install.packages("changepoint")'
RUN R -e 'install.packages("RecordLinkage")'

# Install tablewine package
COPY ./tablewine /opt/tablewine
RUN R -e 'install.packages("/opt/tablewine", repos = NULL, type="source")'

RUN mkdir -p /opt/dsi
WORKDIR /opt/dsi
COPY Data Data/
COPY scripts scripts/
RUN mv /usr/share/tesseract-ocr/4.00/tessdata/eng.traineddata /usr/share/tesseract-ocr/4.00/tessdata/eng.traineddata-bad
RUN cp /opt/dsi/Data/tesseract-ocr/4.00/tessdata/* /usr/share/tesseract-ocr/4.00/tessdata

CMD "tail -f /dev/null"
