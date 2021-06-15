#! /usr/bin/bash
set -e # Exit if any command fails
#unalias grep # only needed if grep is aliased .bashrc or similar

# Default names and paths (TO DO: use command line arguments)
name_server=test_vm # name of virtual machine
path_key=~/.ssh/id_rsa.pub # path of local SSH public key to be uploaded
name_key=mykey # key name once uploaded to NREC
security_group=SSH_ICMP_all # name of project's security group

# Create security group and rules (https://docs.nrec.no/security-groups.html)
if ! openstack security group list | grep -q "$security_group"; then
    openstack security group create --description "Allow incoming SSH and ICMP" $security_group
    openstack security group rule create --ethertype IPv4 --protocol icmp --remote-ip 0.0.0.0/0 $security_group
    openstack security group rule create --ethertype IPv4 --protocol tcp --dst-port 22 --remote-ip 0.0.0.0/0 $security_group
fi

# Upload public SSH key (https://docs.nrec.no/ssh.html)
if ! openstack keypair list | grep -q "$name_key"; then
    openstack keypair create --public-key $path_key $name_key
    echo "Public SSH key uploaded to NREC"
fi

# Instance creation (https://docs.nrec.no/create-virtual-machine.html)
if ! openstack server list | grep -q $name_server; then
    openstack server create --image "GOLD CentOS 7" \
                            --flavor m1.medium \
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
ssh-keyscan -H $address >> ~/.ssh/known_hosts

# Install dependencies and clone repositories on virtual machine
scp {install_*.sh,requirements_*.txt,.vimrc,.tmux.conf} centos@$address:~/
ssh centos@$address chmod 700 install_*.sh
ssh centos@$address ./install_dependencies.sh
ssh centos@$address ./install_platform.sh

# Create, attach and mount storage volume
# TO DO

# Mount cluster storage via sshfs (needed to create data for new sites?)
# TO DO

# Print login information
echo "To login, type: ssh centos@$address"
