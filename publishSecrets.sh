if [[ $# -ne 1 ]] ; then
    echo 'Usage: Provide an environment to which these secrets apply.'
    exit 1
fi
#Set the kube region or it will point to localhost and complain
gcloud config set compute/zone asia-east1-b
#Connect to the right cluster.  This contextualises further operations
gcloud container clusters get-credentials ${1}

kubectl delete secrets/app-config-directory

kubectl create secret generic app-config-directory --from-file=./appConf/
