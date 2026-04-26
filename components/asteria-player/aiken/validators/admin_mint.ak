// admin_mint — always-true minting policy for the asteria admin
// NFT. Iteration 5b uses this script's hash as the policy_id of
// the `admin_token` AssetClass that the four asteria validators
// expect. The bootstrap mints exactly one "asteriaAdmin" token
// via this policy and locks it at the asteria spend address.
//
// No parameters: the hash is deterministic from these bytes
// alone, so apply-params.sh can compute it before applying
// admin_token to the parameterized validators.
validator admin_mint {
  mint(_redeemer: Data, _policy_id: ByteArray, _tx: Data) {
    True
  }
}
