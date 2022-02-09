FROM centos:centos8
RUN sed -i 's/mirrorlist/#mirrorlist/g' /etc/yum.repos.d/CentOS-Linux-* &&\
    sed -i 's|#baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/CentOS-Linux-*
RUN dnf install centos-release-stream -y
RUN dnf swap centos-{linux,stream}-repos -y
RUN dnf distro-sync -y
RUN dnf install sudo -y
ADD ./.machine/NREC_VM_setup/privileged /install
WORKDIR /install
RUN chmod a+x /install/install_*.sh
RUN /install/install_dependencies_root.sh
RUN /install/install_platform_root.sh
RUN adduser user
USER user
RUN mkdir /home/user/NorESM_LandSites_Platform
COPY --chown=user . /home/user/NorESM_LandSites_Platform/
ENV PATH=/usr/local/bin:/home/user/.local/bin:$PATH
WORKDIR /install
RUN ./install_dependencies_user.sh
RUN ./install_platform_user.sh
WORKDIR /home/user/NorESM_LandSites_Platform
RUN python -m pip install -e . --user
EXPOSE 8888
EXPOSE 5006
CMD [ "./run_servers.sh" ]
# RUN chmod -R a+rwx /home/user/NorESM_LandSites_Platform
# USER user
# ENV PATH=/home/user/.local/bin:$PATH
# RUN python -m pip install --upgrade pip --user
# RUN pip3 install -r requirements.txt --user
