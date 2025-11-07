SHELL:=/usr/bin/env bash

.SILENT: build-shared build-config push push-image push-config up down query validate

registry:=ghcr.io/cardano-foundation/cardano-node-antithesis
testnet:=cardano_node_master
source:=https://github.com/cardano-foundation/cardano-node-antithesis

export registry

build-shared:
	docker build \
	    --label "org.opencontainers.image.source=${source}" \
		-f sidecar/Dockerfile \
		-t ${registry}/sidecar:latest sidecar
	docker build \
	    --label "org.opencontainers.image.source=${source}" \
		-f tracer/Dockerfile.compiled \
		-t ${registry}/tracer:latest tracer
	docker build \
	    --label "org.opencontainers.image.source=${source}" \
		-f tracer-sidecar/Dockerfile \
		-t ${registry}/tracer-sidecar:latest tracer-sidecar
	docker build \
	    --label "org.opencontainers.image.source=${source}" \
		-f configurator/Dockerfile \
		-t ${registry}/configurator:latest configurator
	docker build \
	    --label "org.opencontainers.image.source=${source}" \
		-f adversary/Dockerfile \
		-t ${registry}/adversary:latest adversary

push-shared:
	docker push ${registry}/sidecar:latest
	docker push ${registry}/tracer:latest
	docker push ${registry}/tracer-sidecar:latest
	docker push ${registry}/configurator:latest
	docker push ${registry}/adversary:latest

build-and-push-shared: build-shared
	$(MAKE) push-shared

build-config: TESTNET ## Build config container image
	docker build \
		--label "org.opencontainers.image.source=${source}" \
		-f config/Dockerfile \
		-t ${registry}/config:latest ../testnets/${testnet}

publish-test-config-image: TESTNET build-config push-config ## Publish config container image

push: TESTNET push-config

push-config: TESTNET ## Push config container image
	docker push ${registry}/config:latest

up: TESTNET ## Start Run local Docker Compose
	export INTERNAL_NETWORK=true ; \
	cd ../testnets/${testnet} ; docker compose up --detach

down: TESTNET ## Stop local Docker Compose
	export INTERNAL_NETWORK=true ; \
	cd ../testnets/${testnet} ; docker compose down --volumes

logs: TESTNET ## Tail logs of all containers
	export INTERNAL_NETWORK=true ; \
	cd ../testnets/${testnet} ; docker compose logs --follow --tail=100

query: TESTNET ## Query tip of all pools
	pools="$$(awk '/poolCount: /{ print $$2 }' ../testnets/${testnet}/testnet.yaml)" ; \
	for i in $$(seq 1 $${pools}) ; do docker exec -ti p$${i} \
		timeout 0.05 cardano-cli ping --magic 42 --host 127.0.0.1 --port 3001 --tip --quiet -c1; \
	done ; \
	true

validate: TESTNET ## Check for consensus among all pools
	docker exec -ti sidecar /opt/antithesis/test/v1/convergence/eventually_converged.sh

TESTNET: ;
	@if [ -z "${testnet}" ]; then echo "* Please define the testnet argument:"; echo "testnet=example_10.2.1"; echo; exit 1; else export "testnet=${testnet}"; fi
