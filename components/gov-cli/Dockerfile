# gov-cli — sleeping container holding cardano-cli plus the composer
# governance driver scripts. Antithesis `docker exec`s the drivers per
# tick to drive governance operations against relay1.
#
# Two interchangeable driver sets ship in the image:
#   * bash   — cardano-cli wrapped in shell (composer/governance-bash)
#   * python — cardano-clusterlib (the standalone cardano-cli wrapper
#              library) driving the same flow (composer/governance-python)
# Pick one at build time:  --build-arg DRIVER_LANG=bash|python

# cardano-cli pinned to the producer node version (11.0.1).
FROM ghcr.io/intersectmbo/cardano-node@sha256:007da778998d0bdb043fb834c62b9da1c95e33610f31f6f9cccab29a945e2b99 AS cardano-bin

FROM debian:bookworm-slim

ARG DRIVER_LANG=bash

ENV TZ=UTC
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        jq \
        python3 \
        python3-pip && \
    rm -rf /var/lib/apt/lists/*

# Nix store (glibc + cardano binary deps) and the pinned binaries.
COPY --from=cardano-bin /nix /nix
COPY --from=cardano-bin /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli

# Standalone cardano-cli wrapper library used by the python drivers.
RUN python3 -m pip install --no-cache-dir --break-system-packages cardano-clusterlib

# Both driver sets are kept for reference; the selected one becomes the
# live composer template at /opt/antithesis/test/v1/governance.
COPY composer /opt/gov-drivers/
RUN mkdir -p /opt/antithesis/test/v1 && \
    cp -r "/opt/gov-drivers/governance-${DRIVER_LANG}" /opt/antithesis/test/v1/governance && \
    chmod 0755 /opt/antithesis/test/v1/governance/* && \
    find /opt/gov-drivers -type f -exec chmod 0755 {} +

COPY sleep.sh /sleep.sh
RUN chmod 0755 /sleep.sh

# Defaults; the testnet compose overrides as needed.
ENV CARDANO_NODE_SOCKET_PATH=/state/node.socket \
    TESTNET_MAGIC=42 \
    GOV=/gov-data \
    GOV_STATE_DIR=/gov-state \
    WORK=/work \
    NUM_DREPS=5 \
    NUM_CC=5 \
    NUM_POOLS=2

ENTRYPOINT ["/sleep.sh"]
