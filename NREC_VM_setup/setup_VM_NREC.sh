# https://docs.nrec.no/create-virtual-machine.html#doing-the-same-with-cli
name_vm=myserver
# Upload public SSH key
openstack keypair create --public-key ~/.ssh/id_rsa.pub mykey
# Instance creation from client
openstack server create --image "GOLD Ubuntu 20.04 LTS" \
                        --flavor m1.small \
                        --security-group SSH_and_ICMP \
                        --security-group default \
                        --key-name mykey \
                        --nic net-id=dualStack $name_vm
# Get instance's IP address
unalias grep # only needed if grep is aliased .bashrc or similar
address=`openstack server list | grep $name_vm | grep -oP 'dualStack=\K([0-9\.]+)'`
echo "NREC machine IP address: $address"

# Install dependencies and clone repositories on virtual machine
scp {install_*.sh,requirements_*.txt,.vimrc} ubuntu@$address:~/
ssh ubuntu@$address chmod 700 install_*.sh
ssh ubuntu@$address ./install_dependencies.sh
ssh ubuntu@$address ./install_platform.sh

# Mount cluster storage via sshfs?
# TO DO
