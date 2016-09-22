#AUTHOR: chris.hagan@csc.gov.au
#This is a Scala Lift project.  It runs as a Docker container, running a standard WAR inside a vanilla Jetty application server
#The Docker container is mapped onto a Google Container Engine cluster using Kubernetes.
#To do so, we house the container image in the Google Container Registry for easy access, versioning and rollback.
#Performing these steps will require credential access to the Google Container Engine and to the Stackable Regiments code repository.
#The steps are perfomed from within the Google cloud console.
echo 'This must be run from within the Google cloud console'
echo 'You must choose an environment first for predictable results, using the forms "environment.sh [prod|qat]"'

#Create the appropriate WAR
if [[ $# -ne 1 ]] ; then
    echo 'Usage: Provide a release tag.  It will apply to the uploaded image and to the deployment'
    echo 'For instance:'
    echo './publish.sh v1'
    exit 1
fi
RELEASE=${1}
PROJECT=pure-silicon-142205
REPOSITORY=ssac
DEPLOYMENT=ssac
TAG=gcr.io/$PROJECT/$REPOSITORY:$RELEASE
VERSION=csc/$REPOSITORY:$RELEASE
echo $RELEASE $PROJECT $REPOSITORY $VERSION $TAG
git pull
#Create the release artifact
./sbt clean
./sbt compile
./sbt package
mkdir -p webapps

#rename war file as root so that jetty knows what to do with it
for file in target/scala-2.11/*.war; do
    mv "$file" "target/scala-2.11/root.war"
done

cp target/scala-2.11/root.war webapps/
#Build the docker image.  This builds the project as well.
docker build -t  $VERSION .
#Tag the image
docker tag $VERSION $TAG
#Upload the image to the container repository
gcloud docker push $TAG
rm deployment.yaml
kubectl get deployment $DEPLOYMENT -o yaml > deployment.yaml
sed -i -e "s#image:.*$REPOSITORY.*#image: gcr.io/$PROJECT/$REPOSITORY:$RELEASE#g" deployment.yaml
kubectl replace -f deployment.yaml
rm deployment.yaml
#watch kubectl get pods -o wide
