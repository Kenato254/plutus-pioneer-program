{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator, TxInfo (txInfoValidRange),
                                       scriptContextTxInfo, mkValidatorScript)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), (&&), (||), ($), (+), traceIfFalse)
import           Utilities            (wrapValidator)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval  (contains, to, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and 
-- the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = 
    traceIfFalse "Deadline not reached or transaction not signed!" $
    txSignedbyBeneficiary1 || txSignedbyBeneficiary2

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        txSignedbyBeneficiary1 :: Bool
        txSignedbyBeneficiary1 =
            (txSignedBy info $ beneficiary1 dat)
            &&
            (contains (to $ deadline dat) (txInfoValidRange info))
        
        txSignedbyBeneficiary2 :: Bool
        txSignedbyBeneficiary2 = 
            (txSignedBy info $ beneficiary2 dat)
            &&
            (contains (from $ (1 + deadline dat)) (txInfoValidRange info))



{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
