FROM sloan-r:latest

RUN apt-get update
RUN apt-get install -y curl 
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs
RUN apt-get clean

# A docker-compose volume mount should be placed here
RUN mkdir /storage

RUN mkdir /worker
WORKDIR /worker

COPY package.json .
COPY package-lock.json .
RUN npm install --production

COPY worker.js .

CMD node worker.js