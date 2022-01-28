FROM centos:centos8
RUN dnf install sudo -y
ADD ./.machine/NREC_VM_setup/privileged /install
WORKDIR /install
RUN chmod a+x /install/install_*.sh
RUN /install/install_dependencies_root.sh
RUN adduser user
USER user
RUN mkdir /home/user/NorESM_LandSites_Platform
WORKDIR /home/user/NorESM_LandSites_Platform
ADD . /home/user/NorESM_LandSites_Platform/
ENV PATH=/usr/local/bin:$PATH
RUN /install/install_dependencies_user.sh
RUN /install/install_platform.sh
RUN pip install -e .
# RUN chmod -R a+rwx /home/user/NorESM_LandSites_Platform
# USER user
# ENV PATH=/home/user/.local/bin:$PATH
# RUN python -m pip install --upgrade pip --user
# RUN pip3 install -r requirements.txt --user
