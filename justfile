
##### Justfile for managing Cardano testnets #####

start testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Starting cardano_node_master testnet..."
    docker compose --progress plain \
        -f testnets/cardano_node_master/docker-compose.yaml up -d

stop testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Stopping cardano_node_master testnet..."
    docker compose --progress plain \
        -f testnets/cardano_node_master/docker-compose.yaml down \
        --volumes --remove-orphans

restart testnet='cardano_node_master':
    just stop testnet={{ testnet }}
    just start testnet={{ testnet }}

exec container testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Entering container {{ container }}..."
    docker compose \
        -f testnets/cardano_node_master/docker-compose.yaml exec -it {{ container }} bash

logs container testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Tailing logs for container {{ container }}..."
    docker compose \
        -f testnets/cardano_node_master/docker-compose.yaml logs -f {{ container }}

ps testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Listing containers for cardano_node_master testnet..."
    docker compose \
        -f testnets/cardano_node_master/docker-compose.yaml ps \
        --format "table {{{{.Name}}\t{{{{.Image}}\t{{{{.Status}}"

stats testnet='cardano_node_master':
    #!/usr/bin/env bash
    export INTERNAL_NETWORK=true
    echo "Showing stats for cardano_node_master testnet..."
    docker stats \
        $(docker compose -f testnets/cardano_node_master/docker-compose.yaml ps -q)

### Build and Format Commands ###

format:
    #!/usr/bin/env bash
    set -euo pipefail
    nixfmt *.nix