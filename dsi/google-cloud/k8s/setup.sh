#! /bin/bash

# this script assumes you have gone here:
# https://console.cloud.google.com/kubernetes/list?project=digital-ucdavis-edu
# and created a cluster called: 'sloan-r-workers'

CLUSTER=sloan-r-workers

# Create cluster via cli
gcloud beta container clusters create ${CLUSTER} \
  --zone us-central1-a \
  --num-nodes 1 \
  --disk-size 10GB \
  --machine-type n1-standard-1 \
  --cluster-version=1.12.7 \
  --enable-stackdriver-kubernetes

gcloud beta container node-pools create worker-pool \
  --cluster ${CLUSTER} \
  --machine-type n1-highmem-4 \
  --preemptible \
  --num-nodes 0 \
  --disk-size 10GB \
  --enable-autoscaling --min-nodes 0 --max-nodes 100 \
  --node-labels=intendedfor=workers

# setup kubectl to connect to cluster
gcloud container clusters get-credentials ${CLUSTER} --zone us-central1-a --project digital-ucdavis-edu

# set service account secret
kubectl create secret generic service-account-key --from-file=service-account.json=../service-account.json

# create the worker deployment
# kubectl apply -f sloan-r-worker.deployment.yaml

# autoscale deployment
# kubectl autoscale deployment sloan-r-worker-deployment --max 100 --min 1 --cpu-percent 40