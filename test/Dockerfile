FROM node

ARG TINI_VERSION=v0.19.0

ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /usr/local/bin/tini
RUN chmod a+x /usr/local/bin/tini

ENTRYPOINT ["/usr/local/bin/tini", "--"]