[![NorESM](docs/img/NORESM-logo.png "the Norwegian Earth System Model")](https://www.noresm.org/)
[![EMERALD](docs/img/Emerald_darktext_whiteBG_small.png "EMERALD project")](https://www.mn.uio.no/geo/english/research/projects/emerald/)
[![LATICE](docs/img/UiO_LATICE_logo_black_small.png "Land-ATmosphere Interactions in Cold Environments research group")](https://www.mn.uio.no/geo/english/research/groups/latice/)

# [NorESM Land Sites Platform (NorESM-LSP) for site-level simulation over land with CLM-FATES](https://noresmhub.github.io/noresm-land-sites-platform/)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7304386.svg)](https://doi.org/10.5281/zenodo.7304386)

### Webpage: [NorESM land sites platform documentation and user guide](https://noresmhub.github.io/noresm-land-sites-platform/)

### [About the NorESM-LSP Development Team and supporting projects](https://noresmhub.github.io/noresm-land-sites-platform/about/)

### Related repositories:

- Graphical User Interface: [NorESMhub/noresm-lsp-ui](https://github.com/NorESMhub/noresm-lsp-ui)
- Application Programming Interface: [NorESMhub/ctsm-api](https://github.com/NorESMhub/ctsm-api)
- Input data for integrated sites: [NorESMhub/noresm-lsp-data](https://github.com/NorESMhub/noresm-lsp-data)
- (Input data preparation for legacy branch: [NorESMhub/noresm-lsp-input](https://github.com/NorESMhub/noresm-lsp-input))

### Repository structure

| Name           | Description   |
| -------------  | ------------- |
| .github/       | contains issue templates and other functionalities for this reporitory |
| docker/        | contains Dockerfiles for Jupyter and Panoply servers, and other files for containers |
| docs/          | contains markdown files and images used for documentation and to build the [webpage](https://noresmhub.github.io/noresm-land-sites-platform/) |
| notebooks/     | contains jupyter notebooks for use in the Jupyter server (localhost:8888) |
| resources/     | contains configuration files and overwrites for model files. In a locally installed clone, resources/ will also contain the model, cases, and data. |
| .gitignore     | specifies files that [Git should ignore](https://git-scm.com/docs/gitignore), i.e. that should not get pushed to this repository when developers make local changes. E.g. stops case data and files from flooding the repository when someone updates the documentation |
| .zenodo.json   | credit/citation instructions for Zenodo uploads. When we create a new release of this repo, it automatically creates a new version of the Zenodo record as well, using this file to set e.g. author information. If you contribute to the repo, add your name! |
| CITATION.cff   | used to create the [Citation button](https://citation-file-format.github.io/) in the Repository information to the right |
| LICENSE        | this repository's license, which is also used to create the License button in the Repository information to the right |
| README.md      | this file, in markdown syntax |
| docker-compose.yaml | a YAML file that helps the [docker-compose](https://docs.docker.com/compose/compose-file/) command by defining services, networks, and volumes for a Docker application |
| mkdocs.yml | configuration file for the [webpage](https://noresmhub.github.io/noresm-land-sites-platform/), listing the names of separate pages and which files within `docs/` it builds on
| run_linux.sh | a shell script that sets some user information necessary on Linux machines |

### Quick first-time installation steps (see the [webpage for the full user guide and documentation](https://noresmhub.github.io/noresm-land-sites-platform/documentation/)):

Clone the repo with the following command (especially important for Windows users):

`git clone https://github.com/NorESMhub/noresm-land-sites-platform.git --config core.autocrlf=input`

Then run the following from the project root:

`docker-compose up`

> If you are on Linux, run `./run_linux.sh` or `bash run_linux.sh` instead.
> This script takes care of the permissions of the mounted volumes. 

After you see a message containing `Uvicorn running on http://0.0.0.0:8000 (Press CTRL+C to quit)` in `api` docker logs, you can access the API at `http://localhost:8000/api/v1/docs` and the UI at `http://localhost:8080`. You can access the Jupyter server at `http://localhost:8888/lab` and Panoply through `http://localhost:5800`.

> ### Note if you are on Linux, 
> 
> you can run the following command to set the user and id in in a `.env` in the project root:
>
> ```echo "HOST_USER=$(whoami)\nHOST_UID=$(id -u)\nHOST_GID=$(id -g)" > .env```
>
> Doing this makes all the new files and folders created by the app to belong to your local user. Otherwise, all new objects will be owned by the `root` user.
