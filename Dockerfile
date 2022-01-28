FROM centos:centos8
# RUN dnf update -y
# RUN dnf install git -y
RUN dnf install sudo -y
RUN adduser user
USER user
RUN mkdir /home/user/NorESM_LandSites_Platform
WORKDIR /home/user/NorESM_LandSites_Platform
ADD . /home/user/NorESM_LandSites_Platform/
USER root
RUN sudo chmod a+x /home/user/NorESM_LandSites_Platform/.machine/NREC_VM_setup/install_*.sh
WORKDIR /home/user/NorESM_LandSites_Platform/.machine/NREC_VM_setup/
RUN ./install_dependencies_root.sh
USER user
ENV PATH=/usr/local/bin:$PATH
RUN ./install_dependencies_user.sh
USER root
RUN ./install_platform.sh
RUN chmod -R a+rwx /home/user/NorESM_LandSites_Platform
USER user
# ENV PATH=/home/user/.local/bin:$PATH
# RUN python -m pip install --upgrade pip --user
# RUN pip3 install -r requirements.txt --user
