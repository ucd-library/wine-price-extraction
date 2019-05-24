FROM ucdlib/sloan-ocr:latest

RUN apt-get update
RUN apt-get install -y curl 
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y nodejs
RUN apt-get clean

# A docker-compose volume mount should be placed here
RUN mkdir /storage

# Setup @ucd-lib/job-autoscaler-commons config
ENV CLOUD_STORAGE_BUCKET=sloan-ocr
ENV PUBSUB_TOPIC=sloan-ocr
ENV FIREBASE_COLLECTION=sloan-ocr
ENV FIREBASE_URL=https://digital-ucdavis-edu.firebaseio.com
ENV SERVICE_ACCOUNT_PATH=/etc/google/service-account.json

RUN mkdir /worker
WORKDIR /worker

COPY package.json .
COPY package-lock.json .
RUN npm install --production

COPY worker.js .

CMD node worker.js