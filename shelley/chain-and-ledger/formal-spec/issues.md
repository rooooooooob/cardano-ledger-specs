Questions and Outstanding Issues


1. Should I go ahead with native multicurrency and NFT support?

- Fees will be paid in Ada only

- Review how best to express the general
accounting property

2. Suppose we have decided on the protocol for selecting the PK inputs
that are used for script fees. It returns the set of these PK inputs
that covers the fees, but has the smallest possible sum total value
(or some small *enough* value to avoid a full-on subset sum alg).

- Is this definitely how we want to handle this situation? This still does not
have the element of the user deliberately being able to dedicate the inputs of
his/her/wallet's choosing to paying the fees, and I thought this was the
point of having this type of system.

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

7. Still to come in this spec, two cases for validating scripts:

- Those that are expected to validate (has validating and non-validating cases)
- Those that are expected not to validate (also has validating and non-validating cases)

8. Random seed from consensus layer

- Decided this is not necessary and will disturb deterministic behaviour

9. Data that makes up PendingTx:

- are there any changes coming here?
