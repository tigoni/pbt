{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


module TestContract (validator, wrapped, serialized, hash, HelloDatum (..), HelloRedeemer (..)) where

import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
-- import Utils.Shared (validatorHash, wrap)
-- import qualified Plutus.V1.Ledger.Scripts as Scripts
import           Cardano.Api
import           Ledger.Typed.Scripts  as Scripts
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import           PlutusTx
import           PlutusTx.Prelude

newtype HelloDatum = HelloDatum Integer
PlutusTx.unstableMakeIsData ''HelloDatum
newtype HelloRedeemer = HelloRedeemer Integer
PlutusTx.unstableMakeIsData ''HelloRedeemer

-- This validator always validates true
{-# INLINABLE run #-}
run :: HelloDatum -> HelloRedeemer -> PlutusV2.ScriptContext -> Bool
run _ _ _ = True

-- Entry
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = Scripts.mkUntypedValidator run

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: Scripts.ValidatorHash
hash = validatorHash validator