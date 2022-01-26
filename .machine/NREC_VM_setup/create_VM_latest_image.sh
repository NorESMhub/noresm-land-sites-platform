#! /usr/bin/bash
set -e # Exit if any command fails
#unalias grep # only needed if grep is aliased .bashrc or similar

# Default names and paths (TO DO: use command line arguments)
name_server=${USER}_dev # name of virtual machine
path_key=~/.ssh/id_rsa.pub # path of local SSH public key to be uploaded
name_key=mykey # key name once uploaded to NREC
security_group=SSH_ICMP_all # name of project's security group
machine_size=m1.medium # amount of resources allocated to VM

# Image (snapshot) to build VM from (TO DO: use command line arguments)
image="NorESM_platform_0.5"

# Instance creation (https://docs.nrec.no/create-virtual-machine.html)
if ! openstack server list | grep -q $name_server; then
    openstack server create --image "$image" \
                            --flavor $machine_size \
                            --security-group $security_group \
                            --security-group default \
                            --key-name $name_key \
                            --nic net-id=dualStack $name_server
fi

# Get virtual machine's IP address and add it to SSH known hosts
echo "Wait 1 min for the virtual machine to build..."
sleep 1m
address=`openstack server list | grep "$name_server" | grep -oP '\K([0-9\.]{10,})'`
echo "NREC machine IP address: $address"

# Login
ssh centos@$address
