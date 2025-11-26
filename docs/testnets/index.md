# Testnets

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