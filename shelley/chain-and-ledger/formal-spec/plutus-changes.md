Plutus Integration Changes

1. In protocol parameters, `costm` is added. This currently has abstract type, and is meant to contain coefficients 
for the cost model (these are used to compute the actual cost of running a given script)

2. Also in protocol parameters, `c_sdep` is added to calculate the refundable deposit for storing a datascript on-chain,
the actual cost is calculated based on the size of the data script. This is not yet implemented.

3. Most Plutus-related datatypes and changes to UTxO and Tx structure are in the Figure on page 11. For example,
`TxInTx` is the type of inputs carried by a transaction. Those spending from script must include additional `Info` 
(which contains the `exunits` value that the transaction is claiming the validation of the script will require), and
the data script. 

4. Figure 6 contains `valScriptUpTo`, which validates a script, taking a maximum of the given number of `exunits`.
It also contains `mkPendingTx`, which builds the pending tx.

5. Figure 7 gives the functions separating script and PK things. It also has computations needed for the general 
accounting property, which now includes script execution costs.

6. Figure 8 has script-execution and datascript storage related cost calculations.

7. Figure 11 gives the two rules for script validation. The first rule is for when all scripts validate, using no more 
than the corresponding `exunits`. The second is for when something goes wrong. In that case all PK inputs go to fees.

8. Figure 12 applies the UTXOS transition after checking the general accounting property. 

