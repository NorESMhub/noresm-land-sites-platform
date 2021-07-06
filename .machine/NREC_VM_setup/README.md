# Tools to set up a NREC virtual machine for development

The latest image on NREC is [NorESM_platform_0.4](https://dashboard.nrec.no/dashboard/ngdetails/OS::Glance::Image/c8b7b6fd-4497-41bb-9a9d-51d9b21b5789).
Instances can be created from this image by setting `image=NorESM_platform_0.4`
when executing the [`openstack server create` command](https://github.com/NorESMhub/NorESM_LandSites_Platform/blob/27_NREC_VM_CentOs8/.machine/NREC_VM_setup/setup_VM_NREC.sh#L30-L35).
To build an instance from the base CentOS 8 image follow the steps in Sect. 1.

## 1. Create and configure a virtual machine on NREC
Execute these steps according to the
[NREC documentation](https://docs.nrec.no/index.html):
1. Register as a user and save your API password, which you will need in step 2
   (https://docs.nrec.no/login.html#first-time-login)
2. Install OpenStack command line interface tools
   (https://docs.nrec.no/api.html#openstack-command-line-interface-cli)
   and create a `keystone_rc.sh` file in your local home folder
   (https://docs.nrec.no/api.html#using-the-cli-tools)
3. On your local Linux machine run the steps in `source setup_VM_NREC.sh`
   to set up the remote virtual machine (VM) and install the dependencies.
   Set the `image` variable in `setup_VM_NREC.sh` to decide from which
   image (snapshot) your VM will be created.
   Manual execution is recommended to avoid problems due to delays or
   connection errors when calling `openstack` commands.
4. Login information is printed at the end of `setup_VM_NREC.sh` execution.

### 1.1 Notes
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

### 1.2 To do
In `setup_VM_NREC.sh`:
- Create, attach and mount storage volume
- Mount cluster storage via sshfs (needed to create data for new sites?)
- Create a [snapshot](https://docs.nrec.no/create-snapshot.html) and use it as
  the base for new instances.
- Use shell arguments in to set machine parameters now hard-coded under
  "Default names and paths".

## 2. Remote development with Jupyter Lab
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
