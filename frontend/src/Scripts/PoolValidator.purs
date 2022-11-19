module Scripts.PoolValidator
  ( mkBondedPoolValidator
  , mkUnbondedPoolValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Scripts
  ( ClientError
  , PlutusScript
  , Validator(Validator)
  , applyArgs
  )
import Contract.PlutusData (class ToData, toData)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Types (BondedPoolParams)
import UnbondedStaking.Types (UnbondedPoolParams)
import Utils (jsonReader)

-- | This is the parameterized validator script. It still needs to receive a
-- `BondedPoolParams` to become a minting policy
bondedPoolValidator :: Either JsonDecodeError PlutusScript
bondedPoolValidator = jsonReader "script" _bondedPoolValidator

unbondedPoolValidator :: Either JsonDecodeError PlutusScript
unbondedPoolValidator = jsonReader "script" _unbondedPoolValidator

-- | This function takes a `BondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkBondedPoolValidator
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r (Either ClientError Validator)
mkBondedPoolValidator = mkValidator bondedPoolValidator

-- | This function takes a `UnbondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkUnbondedPoolValidator
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r (Either ClientError Validator)
mkUnbondedPoolValidator = mkValidator unbondedPoolValidator

mkValidator
  :: forall (a :: Type) (r :: Row Type)
   . ToData a
  => Either JsonDecodeError PlutusScript
  -> a
  -> Contract r (Either ClientError Validator)
mkValidator ps params = do
  unappliedScript <- liftedE $ pure ps
  applyArgs (Validator unappliedScript) [ toData params ]

foreign import _bondedPoolValidator :: Json
foreign import _unbondedPoolValidator :: Json
