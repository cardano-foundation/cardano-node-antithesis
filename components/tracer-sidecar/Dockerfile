FROM haskell:9.6 AS build

WORKDIR /opt/tracer-sidecar

COPY . .

RUN --mount=type=cache,target=/root/.local/state/cabal/store \
    --mount=type=cache,target=/root/.cache/cabal \
    --mount=type=cache,target=./dist-newstyle \
    cabal update && \
    cabal install --install-method=copy

FROM docker.io/debian:stable-slim AS main

COPY --from=build /root/.local/bin/tracer-sidecar /usr/local/tracer-sidecar

CMD ["/usr/local/tracer-sidecar", "/opt/cardano-tracer"]
