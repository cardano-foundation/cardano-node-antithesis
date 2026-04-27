ARG CARDANO_CLI_VERSION="10.8.0.0"
FROM ghcr.io/blinklabs-io/cardano-cli:${CARDANO_CLI_VERSION} AS cardano-cli

FROM docker.io/debian:stable-slim AS main

ENV TZ="UTC"
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        iputils-ping \
        jq \
        liblmdb0 \
        netbase \
        netcat-openbsd \
        procps && \
    rm -rf /var/lib/apt/lists/*

COPY --from=cardano-cli --chown=root:root /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli
COPY --from=cardano-cli --chown=root:root /usr/local/lib/ /usr/lib/

RUN groupadd --gid 10000 cardano && \
    useradd --comment 'cardano' --create-home --gid 10000 --password '!' --shell '/bin/bash' --uid 10000 cardano

RUN install --directory --owner=cardano --group=cardano --mode=0750 /opt/antithesis/test/v1

COPY composer /opt/antithesis/test/v1/
RUN chmod 0755 /opt/antithesis/test/v1/*/*

COPY stub.sh /
RUN chmod 0755 /stub.sh

USER cardano
CMD ["/stub.sh"]
