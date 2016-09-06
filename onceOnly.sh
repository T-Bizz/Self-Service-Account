AUTHOR: chris.hagan@csc.gov.au
#This is a Scala Lift project.  It runs as a Docker container, running a standard WAR inside a vanilla Jetty application server
#The Docker container is mapped onto a Google Container Engine cluster using Kubernetes.
#To do so, we house the container image in the Google Container Registry for easy access, versioning and rollback.
#Performing these steps will require credential access to the Google Container Engine and to the Stackable Regiments code repository.
#The steps are perfomed from within the Google cloud console.
echo 'This must be run from within the Google cloud console'
echo 'You must choose an environment first for predictable results, using the forms "environment.sh [prod|qat]"'
if [[ $# -ne 1 ]] ; then
    echo 'Usage: Provide a release tag.  It will apply to the new service'
    echo 'For instance:'
    echo './onceOnly.sh v1'
    exit 1
fi
RELEASE=${1}
PROJECT=pure-silicon-142205
REPOSITORY=ssac
TAG=gcr.io/$PROJECT/$REPOSITORY:$RELEASE
VERSION=csc/$REPOSITORY:$RELEASE
echo $RELEASE $PROJECT $REPOSITORY $VERSION $TAG
#Create a service
kubectl run $REPOSITORY --image=gcr.io/$PROJECT/$REPOSITORY:$RELEASE --port=443
#Create an external IP for it
kubectl expose deployment $REPOSITORY --target-port=443 --port=443 --type="LoadBalancer" --session-affinity="ClientIP"
watch kubectl get services
