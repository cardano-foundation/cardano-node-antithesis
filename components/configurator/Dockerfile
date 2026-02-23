# Pin cardano-cli version to match the highest node version in the testnet.
# Using intersectmbo image (the official source) instead of third-party builds.
FROM ghcr.io/intersectmbo/cardano-node:10.5.3 AS cardano-bin

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        tar \
        tree \
        python3 \
        python3-pip \
        jq && \
    rm -rf /var/lib/apt/lists/*

# Copy pinned cardano binaries from official image
COPY --from=cardano-bin /usr/local/bin/cardano-node /usr/local/bin/
COPY --from=cardano-bin /usr/local/bin/cardano-cli /usr/local/bin/

# Install uv (fast Python package manager)
RUN python3 -m pip install --break-system-packages uv

# Clone testnet-generation-tool.git repository
WORKDIR /usr/local/src
RUN git clone --branch main https://github.com/cardano-foundation/testnet-generation-tool.git

# Download testnet-generation-tool dependencies
WORKDIR /usr/local/src/testnet-generation-tool
RUN uv sync && mkdir -p cardano-node && ln -sf /usr/local/bin ./cardano-node/bin

COPY configurator.sh /
RUN chmod 0755 /configurator.sh

ENTRYPOINT ["/configurator.sh"]
