FROM ubuntu:latest


# replace shell with bash so we can source files
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

# update the repository sources list
# and install dependencies
RUN apt-get update \
    && apt-get install -y curl libtinfo-dev \
    && apt-get clean \
    && apt-get autoclean

# install stack
RUN curl -sSL https://get.haskellstack.org/ | bash

RUN mkdir -p /usr/local/nvm
# nvm environment variables
ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 7.4.0

# install nvm
# https://github.com/creationix/nvm#install-script
RUN curl --silent -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash

# install node and npm
RUN source $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

# add node and npm to path so the commands are available
ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH $NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH

# local to path 
RUN echo 'PATH=~/.local/bin:$PATH' >> ~/.bashrc
ENV PATH /root/.local/bin:$PATH

# create workspace
RUN mkdir -p ws/twic2
WORKDIR ws/twic2
COPY . ./

# install global build deps
RUN cd backend \
    && stack install cabal-install \
    && stack install happy \
    && stack install alex

# install backend build deps
RUN cd backend \
    && stack setup --install-ghc --no-system-ghc

ENV GHCPATH /root/.stack/programs/x86_64-linux/ghc-7.10.3/bin
ENV PATH $GHCPATH:$PATH

# install frontend build deps
RUN cd frontend \
    && stack setup --install-ghc --no-system-ghc

# build code
RUN make frontend

EXPOSE 3000

# RUN make run



