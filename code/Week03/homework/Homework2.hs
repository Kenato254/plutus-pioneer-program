{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator, TxInfo (txInfoValidRange),
                                       mkValidatorScript, scriptContextTxInfo)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool, (.), (&&), ($), traceIfFalse)
import           Utilities            (wrapValidator)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (contains, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and 
-- the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = 
    txSignedByBeneficiary && txDeadlinePassed

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        txSignedByBeneficiary :: Bool
        txSignedByBeneficiary = 
            traceIfFalse "Transaction not signed!" $ txSignedBy info beneficiary
        
        txDeadlinePassed :: Bool
        txDeadlinePassed = 
            traceIfFalse "Deadline not passed" $ contains (from deadline) (txInfoValidRange info)

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
