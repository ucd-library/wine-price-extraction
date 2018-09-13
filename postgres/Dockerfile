FROM postgres:9.6
MAINTAINER Quinn Hart <qjhart@ucdavis.edu>

RUN apt-get update \
    && apt-get install -y build-essential git-core \
    postgresql-server-dev-$PG_MAJOR \
    postgresql-$PG_MAJOR-postgis-2.4 postgresql-$PG_MAJOR-postgis-2.4-scripts \
    && rm -rf /var/lib/apt/lists/*

#RUN apt-get update && apt-get install -y libv8-dev curl postgresql-$PG_MAJOR-plv8 \
#    && rm -rf /var/lib/apt/lists

# Put at end to stop remaking everytime :)
COPY initdb.d/ /docker-entrypoint-initdb.d/
