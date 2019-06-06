#! /bin/bash

# this script assumes you have gone here:
# https://console.cloud.google.com/kubernetes/list?project=digital-ucdavis-edu
# and created a cluster called: 'sloan-r-workers'

# Create cluster via cli
gcloud beta container clusters create sloan-r-workers-small \
  --zone us-central1-a \
  --num-nodes 2 \
  --disk-size 10GB \
  --machine-type n1-highmem-2 \
  --enable-autoscaling --min-nodes 2 --max-nodes 3 \
  --preemptible \
  --cluster-version=1.12.7 \
  --enable-stackdriver-kubernetes

# setup kubectl to connect to cluster
gcloud container clusters get-credentials sloan-r-workers-small --zone us-central1-a --project digital-ucdavis-edu

# set service account secret
kubectl create secret generic service-account-key --from-file=service-account.json=../service-account.json

# create the worker deployment
kubectl apply -f sloan-r-worker.deployment.yaml

# autoscale deployment
kubectl autoscale deployment sloan-r-worker-deployment --max 3 --min 1 --cpu-percent 40