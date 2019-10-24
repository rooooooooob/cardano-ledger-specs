Questions and Outstanding Issues


1. Should I go ahead with native multicurrency and NFT support?

2. Do we want to separate the cases when script validation fails because
of execution-cost issues and when it fails in the usual way? Besides
having them result in different errors.

3. Staking rights of script addresses:

- How can one register a script address (using the Deleg-Reg rule)
to have its own staking credential if this is done by a certificate
that has to be witnessed (must have an entry in $\fun{txwitsVKey}~ tx$)

4. The validation (runScript?) should be parametrized by `exunits`? 
And return false if these are exceeded?

5. Data script and redeemer script size constraints

6. Not immediately clear how collecting the DS deposits would work:
Deposits have to go back to whoever originally paid into the script.
Transaction spending the data script, and thus triggering a refund,
does not have access to this info.

- Could store the refund address alongside DS and make a UTxO entry
that way

- Might prefer a separate pool for these types of deposits (because of
PP changes affecting refunds etc. from the certificate pool, recalculating
obligation).

- Should these types of deposits decay?

7. Should addresses be changed to include the hash and the
staking credential?

- Party paying into the script must provide this credential.

8. If in addition to execution cost, a refundable “deposit” will be included
in the script execution fees,
the person paying that deposit must provide a refund address also
(These don’t seem really necessary though).

9. Anything jump out in terms of optimization? (avoid unnecessary filtering of the UTxO, etc)

