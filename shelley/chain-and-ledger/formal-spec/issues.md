Questions and Outstanding Issues


1. Should I go ahead with native multicurrency and NFT support?

- Fees will be paid in Ada only

- Review how best to express the general
accounting property

2. Should the reward calculation take into account *only* Ada tokens
associated with staking credentials? This also includes pool pledges,
parameters, and anything else to do with rewards, etc. Essentially,
the only place other tokens should appear are in the general accounting
property, transaction inputs and outputs, and UTxO.

3. To avoid more than one copy of the validator, there is a hash lookup
Map in the transaction. Should we do this also for the redeemer and data?
Since these are smaller (perhaps smaller than their own hashes), this
may not be a good idea?

3. More specific typechecking and script validation functions still need to be added
to this spec.

4. The cost model calculation - is the approach correct here in terms of the
abstract types used? Is this the model we want to plug in real types into
later? I.e. the CostMod in protocol parameters and the ExUnits in the transactions.

5. Are we for sure going to have the data script *optionally* stored on-chain?

- That means, the transaction spending the output with that data script
must provide the data script if it is not stored on the chain.

- This is the change to the model we have agreed on, right?

6. Validation, data script and redeemer script size constraints.

7. In case we decide to require deposits for storing data scripts on-chain:

- Not immediately clear how collecting the DS deposits would work:
Deposits have to go back to whoever originally paid into the script.
Transaction spending the data script, and thus triggering a refund,
does not have access to this info.

- Could store the refund address alongside DS and make a UTxO entry
that way

- Might prefer a separate pool for these types of deposits (because of
PP changes affecting refunds etc. from the certificate pool, recalculating
obligation).

- Should these types of deposits decay (differently than other deposits)?

8. Random seed from consensus layer

- Decided this is not necessary and will disturb deterministic behaviour

9. Data that makes up PendingTx:

- are there any changes coming here?
