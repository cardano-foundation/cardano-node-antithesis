# Testnets

## Just commands

```bash
just
```

```text
Available recipes:
    attack testnet='cardano_node_master'            # attack the network with adversarial chain sync clients
    check-convergence testnet='cardano_node_master' # check convergence of nodes in a testnet
    default                                         # just this help message
    exec container testnet='cardano_node_master'    # exec into a container
    format                                          # format code
    logs container testnet='cardano_node_master'    # view logs of a container
    ps testnet='cardano_node_master'                # list containers in a testnet
    restart testnet='cardano_node_master'           # restart a testnet
    start testnet='cardano_node_master'             # start a testnet
    stats testnet='cardano_node_master'             # view resource usage stats of a testnet
    stop testnet='cardano_node_master'              # stop a testnet
```

## Command to start the testnet

It's possible and desirable to run the testnet locally, before submitting them to Antithesis. Nevertheless we have to do some gymnic to actually try the scripts in the sidecar.

The `justfile` contains a command to start the testnet, just some wrappers around docker-compose, which means that all the images are available locally or on public repositories.

```asciinema-player
{
    "file": "assets/asciinema/run-test.cast",
    "cols": 150,
    "rows": 50,
    "mkap_theme": "none"
}
```