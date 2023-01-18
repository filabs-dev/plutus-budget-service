# BUILD IMAGE
####################
FROM haskell:8.10.6 AS builder

WORKDIR /build

# Install dependencies
RUN apt-get update
RUN apt-get install -y libgmp-dev \
                       zlib1g-dev \
                       libpq-dev \
                       libtinfo-dev \
                       libsodium-dev \
                       libpq5 \
                       pkg-config \
                       build-essential \
                       liblzma-dev

# Copy cabal files
COPY *.cabal cabal.project /build/

USER root

RUN cabal v2-update

RUN cabal v2-install cabal-plan --constraint='cabal-plan +exe'

# Build only dependencies so they can be cached
RUN cabal v2-build -v1 --dependencies-only budget-server

COPY . /build

RUN cabal build

RUN mkdir -p /build/artifacts && cp $(cabal-plan list-bin budget-server) /build/artifacts/

# DEPLOYMENT IMAGE
####################
FROM ubuntu:22.04

RUN apt-get update
RUN apt-get install -y libgmp10 \
                       zlib1g \
                       libsodium23   \
                       libpq5 \
                       pkg-config \
                       build-essential

WORKDIR /app

EXPOSE 3001

COPY --from=builder /build/artifacts/budget-server /app/budget-server

COPY /configurations /configurations

# Set up a default command to run
ENTRYPOINT [ "/app/budget-server", "--", "--config" ]
