FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive
ENV DISPLAY=:1

RUN apt-get update \
    && apt-get -y install xserver-xorg-video-dummy x11-apps --no-install-recommends

ENV NETLOGO_HOME=/opt/netlogo
ENV NETLOGO_VERSION=6.4.0

RUN apt-get update; apt-get install -y unzip swi-prolog-java --no-install-recommends  \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

ADD https://ccl.northwestern.edu/netlogo/${NETLOGO_VERSION}/NetLogo-${NETLOGO_VERSION}-64.tgz /tmp/netlogo.tgz
	
RUN tar xzf /tmp/netlogo.tgz -C /opt && \
	rm /tmp/netlogo.tgz && \
    mv /opt/NetLogo* ${NETLOGO_HOME}

ADD https://github.com/LyzardKing/NetProLogo/releases/download/v1.0.0/netprologo-1.0.0.zip /tmp/netprologo.zip

RUN unzip /tmp/netprologo.zip -d $NETLOGO_HOME/extensions/netprologo && \
	rm /tmp/netprologo.zip

RUN echo "java-options=-Djava.library.path=/usr/lib/swi-prolog/lib/x86_64-linux" >> ${NETLOGO_HOME}/lib/app/NetLogo.cfg