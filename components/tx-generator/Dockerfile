# Copy cardano-cli and its Nix dependencies from the official node image.
FROM ghcr.io/intersectmbo/cardano-node:10.7.1 AS cardano-bin

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        python3 python3-pip jq && \
    rm -rf /var/lib/apt/lists/*

# Copy Nix store (contains glibc and all cardano-cli deps)
COPY --from=cardano-bin /nix /nix

# Copy cardano-cli binary
COPY --from=cardano-bin /usr/local/bin/cardano-cli /usr/local/bin/

COPY requirements.txt /app/
RUN pip install --no-cache-dir --break-system-packages -r /app/requirements.txt

COPY tx_generator.py /app/

WORKDIR /app
ENTRYPOINT ["python3", "tx_generator.py"]
