# Tools to set up a NREC virtual machine for development

To create and configure a virtual machine on NREC
(https://docs.nrec.no/index.html):
1. Register as a user and save your API password, which you will need in step 2
   (https://docs.nrec.no/login.html#first-time-login)
2. Install OpenStack command line interface tools
   (https://docs.nrec.no/api.html#openstack-command-line-interface-cli)
   and create a `keystone_rc.sh` file in your local home folder
   (https://docs.nrec.no/api.html#using-the-cli-tools)
3. On your local Linux machine run the steps in `source setup_VM_NREC.sh`
   to set up the remote virtual machine and install the dependencies.
   Manual execution is recommended to avoid problems due to delays or
   connection errors when calling `openstack` commands.
4. Login information is printed at the end of `setup_VM_NREC.sh` execution.

Notes:
- The following parameters are set at the beginning of the `setup_VM_NREC.sh`
  under "Default names and paths": virtual machine name, SSH key path and
  name, and security group name.
- Several steps (Create security group and rules, Upload public SSH key,
  Instance creation) can be done manually on the
  [NREC dashboard](https://dashboard.nrec.no): links to the documentation are
  provided in `setup_VM_NREC.sh`.
- Source for CLM dependencies: [CTSM Dockerfile](https://github.com/sunnivin/docker-local-build-run-CTSM/blob/1774e7aa6c49cfbe10dae18ceb7dc2739e099d7c/docker/baseos/centos/centos7.6/Dockerfile))
- To check for Ubuntu package dependencies: `apt-cache depends <package>`

To do list (in `setup_VM_NREC.sh`):
- Create, attach and mount storage volume
- Mount cluster storage via sshfs (needed to create data for new sites?)
- Create a [snapshot](https://docs.nrec.no/create-snapshot.html) and use it as
  the base for new instances.
- Use shell arguments in to set machine parameters now hard-coded under
  "Default names and paths".
