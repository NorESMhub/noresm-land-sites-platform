# Docker setup

## Prerequisites
 - Install [Docker](https://docs.docker.com/get-docker/) for your operating system.
 - Run `docker pull lassetk/noresm_land_sites_platform:latest` to pull the image to your local storage.
 -- Linux: need to use sudo or change [permission](https://https://docs.docker.com/engine/install/linux-postinstall)
 -- The image will be kept in a [default location](https://www.freecodecamp.org/news/where-are-docker-images-stored-docker-container-paths-explained/) depending on your system.
 - git clone [NorESM_LandSites_Platform](https://github.com/NorESMhub/NorESM_LandSites_Platform.git) to local computer
 
## Setup the container
All the setup we need for running the container are in [docker-compose.yml](../docker-compose.yml). Feel free so edit memory and CPU constrains depending on your available resources.

File mapping between the container and the host is defined in the `volumes:` block. By default, only `data` directory of local 'NorESM_LandSites_Platform' folder is mapped. 

## Run
To run the container, first be sure you are in the main directory containing [docker-compose.yml](../docker-compose.yml). Then run following command
``` bash
docker-compose up
```
Once the process is running you can access **make_settings** app at [localhost:5006](localhost:5006) and jupyter notebook environment is accessible at [localhost:8888](localhost:8888) with password: `pass`

To stop the container running press `Ctrl+c`

# Build the container
Once you'd like to test new changes to the setup, you'll need to rebuild the container. This should ideally be done on a remote computer, like NREC, and not on a laptop (especially if you are using Windows!). To rebuild the container, ensure you are in the root directory of the repository and run:
``` bash
docker build . -t <TAG>:<VERSION>
```
- Replace **\<TAG\>** with your repository name on [DockerHub](https://hub.docker.com/)
- Replace **\<VERSION\>** with any string that you'd like to distinguish your version. By convention `latest` is used to specify latest working version.

After the build process is finished, you should push the new image to the repository. Firstly ensure you are logged in by:
```bash
docker login
```
Consequently push the image:
```bash
docker push <TAG>:<VERSION>
```
