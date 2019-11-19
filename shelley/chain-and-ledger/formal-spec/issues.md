Questions and Outstanding Issues

1. Coin and Value types ok for multicur? notation for validation functions ok?

4. The cost model calculation - is the approach correct here in terms of the
abstract types used? Is this the model we want to plug in real types into
later? I.e. the CostMod in protocol parameters and the ExUnits in the transactions.

10. Is there any concern about the `forged` field containing negative values?

- Is `forged` on the correct value preservation equation side?

- Should we have `forged` and `unforged`?

12. The empty sequence LEDGERS rule is ok, right? Even in the case of
script validation failure? There are no other rules firing with this
specific transaction in the environment or signal?

15. Note the type change of `PendingTxInS`

16. Any problem with possibly having extra validators in the Vals map?

17. Is it useful to expose transaction metadata to scripts?

13. Do Plutus scripts need access to other things like delegation state, etc.?

6. Validation, data script and redeemer script size constraints.

9. Data that makes up PendingTx:

- are there any changes coming here?
