FROM docker.io/debian:stable-slim AS build

# Set time zone
ENV TZ="UTC"
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && \
    echo ${TZ} > /etc/timezone

ADD docker-compose.yaml /docker-compose.yaml
ADD testnet.yaml /testnet.yaml

RUN sed -i 's/${INTERNAL_NETWORK}/false/g' /docker-compose.yaml