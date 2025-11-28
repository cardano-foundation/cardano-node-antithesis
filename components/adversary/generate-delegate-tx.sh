#!/bin/bash

set -evx

CONFIG_DIR=${1:-configs}
TESTNET_MAGIC=${TESTNET_MAGIC:-42}
SOCKET_PATH=${SOCKET_PATH:-state/node.socket}


# retrieve some utxo input
function get-utxo {
    local index=$1
    local addr=$(cat $2)

    timeout=30
    utxo=null
    while [ $utxo == "null" -a $timeout -ne 0 ]; do
        sleep 5
        timeout=$(( timeout - 1 ))
        utxo=$(cardano-cli query utxo --address $addr --testnet-magic $TESTNET_MAGIC --socket-path $SOCKET_PATH | jq -r "keys[$index]")
    done

    [ $timeout -ne 0 ] || { echo "no utxo found for $addr, waiting" ; exit 1 ; }

    echo $utxo
}


if [ ! -f alice.sk ]; then
    # generate payment key pair
    cardano-cli address key-gen --verification-key-file alice.vk --signing-key-file alice.sk

    # generate stake key pair
    cardano-cli latest stake-address key-gen --verification-key-file alice.stake.vk --signing-key-file alice.stake.sk

    # generate full address
    cardano-cli address build --payment-verification-key-file alice.vk --stake-verification-key-file alice.stake.vk --testnet-magic 42 --out-file alice.full.addr

    # Send funds to Alice
    cardano-cli latest transaction build --tx-in "$(get-utxo 0 $CONFIG_DIR/keys/faucet.addr)"  \
                --tx-out $(cat alice.full.addr)+10000000000 \
                --change-address $(cat $CONFIG_DIR/keys/faucet.addr) \
                --socket-path $SOCKET_PATH --testnet-magic $TESTNET_MAGIC --out-file tx.fund.raw

    # sign tx
    cardano-cli latest transaction sign --tx-file tx.fund.raw --out-file tx.fund.signed --signing-key-file $CONFIG_DIR/keys/faucet.sk
    cardano-cli latest transaction submit --tx-file tx.fund.signed --socket-path $SOCKET_PATH --testnet-magic $TESTNET_MAGIC

    echo "Funded alice with 10000000000 lovelaces"
fi

# create certificates for stake registration and delegation
cardano-cli latest stake-address registration-certificate \
            --stake-verification-key-file alice.stake.vk \
            --out-file alice.registration.cert \
            --key-reg-deposit-amt $(cardano-cli latest query protocol-parameters --socket-path $SOCKET_PATH --testnet-magic $TESTNET_MAGIC | jq .stakeAddressDeposit)

echo "Generated registration certificate:  alice.registration.cert"

STAKE_POOL_ID=$(cardano-cli latest query stake-snapshot --testnet-magic $TESTNET_MAGIC --socket-path $SOCKET_PATH --all-stake-pools | jq -r '.pools | keys | .[0]')

cardano-cli latest stake-address stake-delegation-certificate \
            --stake-verification-key-file alice.stake.vk \
            --out-file alice.delegation.cert \
            --stake-pool-id $STAKE_POOL_ID

echo "Generated delegation certificate:  alice.delegation.cert"

# build, sign, submit  certificate transaction
cardano-cli latest transaction build --tx-in "$(get-utxo 0 alice.full.addr)" \
            --witness-override 2 \
            --certificate-file alice.registration.cert \
            --certificate-file alice.delegation.cert \
            --change-address $(cat alice.full.addr) \
            --socket-path $SOCKET_PATH --out-file tx.delegate.raw --testnet-magic $TESTNET_MAGIC
cardano-cli latest transaction sign --signing-key-file alice.sk --signing-key-file alice.stake.sk --tx-file tx.delegate.raw --out-file tx.delegate.signed
cardano-cli latest transaction submit --tx-file tx.delegate.signed --socket-path $SOCKET_PATH --testnet-magic $TESTNET_MAGIC

echo "Submitted valid delegation transaction for Alice to $STAKE_POOL_ID"

# delegate to another spo
STAKE_POOL_ID=$(cardano-cli latest query stake-snapshot --testnet-magic $TESTNET_MAGIC --socket-path $SOCKET_PATH --all-stake-pools | jq -r '.pools | keys | .[1]')
cardano-cli latest stake-address stake-delegation-certificate \
            --stake-verification-key-file alice.stake.vk \
            --out-file alice.delegation.cert \
            --stake-pool-id $STAKE_POOL_ID

echo "Generate delegation certificate to $STAKE_POOL_ID"

# build raw delegation transaction
cardano-cli latest transaction build --tx-in "$(get-utxo 0 alice.full.addr)" \
            --witness-override 2 \
            --certificate-file alice.delegation.cert \
            --change-address $(cat alice.full.addr) \
            --socket-path $SOCKET_PATH --out-file tx.delegate.raw --testnet-magic 42

# sign and submit transaction
cardano-cli latest transaction sign --signing-key-file alice.sk --signing-key-file alice.stake.sk --tx-file tx.delegate.raw --out-file tx.delegate.signed

# extend pool id by 2 bytes
jq  ".cborHex |= sub(\"$STAKE_POOL_ID\"; \"${STAKE_POOL_ID}1234\")" tx.delegate.signed > /tmp/tx.delegate.signed
cp /tmp/tx.delegate.signed tx.delegate.signed

echo "Extended $STAKE_POOL_ID with 2 bytes"

cardano-cli latest transaction submit --tx-file tx.delegate.signed --socket-path $SOCKET_PATH --testnet-magic $TESTNET_MAGIC

echo "Submitted invalid delegation certificate to $STAKE_POOL_ID"
