use aiken/collection/list
use aiken/collection/dict
use cardano/assets.{PolicyId, tokens}
use cardano/transaction.{OutputReference, Transaction, find_input}

// admin_mint — one-shot minting policy parameterised on a seed
// 'OutputReference'. The policy succeeds iff:
//
//   1. the tx consumes the seed output (consumption is permanent
//      on Cardano UTxO, so this clause can be satisfied at most
//      once across all chain history); AND
//   2. the bundle minted under this policy is exactly
//      [("asteriaAdmin", 1)] — no other token names, no other
//      quantities.
//
// Together the two clauses guarantee that the @asteriaAdmin@
// token can be minted at most once, ever. Downstream validators
// (asteria, spacetime, pellet) treat
// @quantity_of(value, admin_policy, "asteriaAdmin") > 0@ as
// "this is the canonical asteria UTxO" — that predicate is
// only sound when the policy itself enforces uniqueness, which
// is what this validator now does.
//
// The seed parameter shifts the policy hash per deploy:
// different bootstrap runs pick different seed UTxOs and
// therefore land at different policy IDs / asteria spend
// addresses. That is the intended shape — within a single
// cluster lifetime, exactly one asteria game state can ever
// exist.
validator admin_mint(seed: OutputReference) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    let Transaction { inputs, mint, .. } = self
    let must_spend_seed = list.any(
      inputs,
      fn(input) { input.output_reference == seed },
    )
    let minted_pairs =
      mint
        |> tokens(policy_id)
        |> dict.to_pairs()
    let must_mint_exact_singleton =
      minted_pairs == [Pair("asteriaAdmin", 1)]
    must_spend_seed? && must_mint_exact_singleton?
  }

  else(_) {
    fail
  }
}
