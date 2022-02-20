# Tools to set up a NREC virtual machine for development

## 1. Access to NREC
Execute these steps according to the
[NREC documentation](https://docs.nrec.no/index.html):
1. Register as a user and save your API password, which you will need in step 3
   (https://docs.nrec.no/login.html#first-time-login).
2. Install OpenStack command line interface tools
   (https://docs.nrec.no/api.html#openstack-command-line-interface-cli).
3. Create a `keystone_rc.sh` file in your local home folder
   (https://docs.nrec.no/api.html#using-the-cli-tools),
   where `OS_REGION_NAME` should be set either to `osl` or `bgo`
   depending on available project resources, which can be checked on
   [the NREC dashboard](https://dashboard.nrec.no/dashboard/project/)
   by switching regions in the dialog at the top to the right of the
   project name.


## 2. Create a NREC virtual machine from the latest image
The latest image on NREC is [NorESM_platform_0.5](https://dashboard.nrec.no/dashboard/ngdetails/OS::Glance::Image/e09310ea-1d68-4a9a-9be0-5cec7051f225).
Instances can be created from this image by executing the script [`create_VM_latest_image.sh`](https://github.com/NorESMhub/NorESM_LandSites_Platform/blob/platform_dev/.machine/NREC_VM_setup/create_VM_latest_image.sh),
where some parameters, which are set under the "Default names and paths" header,
can be modified manually if needed (see also Sect. 3.1).

To build an instance from the base CentOS 8 image follow the steps in Sect. 3.


## 3. Create and configure a NREC virtual machine from scratch
On your local Linux machine run the steps in `source setup_VM_NREC.sh`
to set up the remote virtual machine (VM) and install the dependencies.

Set the `image` variable in `setup_VM_NREC.sh` to decide from which
image (snapshot) your VM will be created.

Manual execution is recommended to avoid problems due to delays or
connection errors when calling `openstack` commands.

Login information is printed at the end of `setup_VM_NREC.sh` execution.

### 3.1 Notes
- The following parameters are set at the beginning of the `setup_VM_NREC.sh`
  under "Default names and paths": VM name, SSH key path and
  name, and security group name.
- Several steps (Create security group and rules, Upload public SSH key,
  Instance creation) can be done manually on the
  [NREC dashboard](https://dashboard.nrec.no): links to the documentation are
  provided in `setup_VM_NREC.sh`.
- Sources for CLM dependencies and configuration are the CESM Dockerfiles:
  * https://github.com/ESCOMP/ESCOMP-Containers/blob/master/base/centos8/Dockerfile
  * https://github.com/ESCOMP/ESCOMP-Containers/blob/master/CESM/2.2/Dockerfile

### 3.2 To do
In `setup_VM_NREC.sh`:
- Create, attach and mount storage volume
- Mount cluster storage via sshfs (needed to create data for new sites?)
- Use shell arguments in to set machine parameters now hard-coded under
  "Default names and paths".


## 4. Remote development with Jupyter Lab
To edit files and run programs on the remote VM from the web browser on
your local Linux machine (Ubuntu under Windows needs testing):
1. On the VM run `jlremote` (alias defined in the `.bashrc` that is copied to
   the VM during the setup process described in Sect. 1).
   It starts a Jupyter Lab server without graphical interface.
   An URL is printed in the terminal: copy the string after `token=` that is
   needed as password for the first connection from the local machine.
2. On the local machine run the bash script `jllocal.sh` (located in the same
   folder of this README file): `bash jllocal.sh centos $address`,
   where `address` is the VM IP address environment variable.
3. A web-browser tab should open and you may need to type in the token;
   otherwise you have to open the URL in the browser (see point 1).
   The webpage may need to be refreshed to display the Jupyter Lab interface.
