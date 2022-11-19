module Scripts.StateNFT
  ( mkStateNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (toData)
import Contract.Scripts
  ( ClientError
  , MintingPolicy(MintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Contract.Transaction (TransactionInput)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Types (StakingType(Bonded, Unbonded))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `TransactionInput` to become a minting policy
nftPolicy :: StakingType -> Either JsonDecodeError PlutusScript
nftPolicy Bonded = jsonReader "script" _bondedStateNFT
nftPolicy Unbonded = jsonReader "script" _unbondedStateNFT

-- | This function takes a `TransactionInput` and produces the `MintingPolicy` for
-- the state NFT
mkStateNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> TransactionInput
  -> Contract r (Either ClientError MintingPolicy)
mkStateNFTPolicy st txInput = do
  unappliedScript <- liftedE $ pure $ nftPolicy st
  applyArgs (MintingPolicy unappliedScript) [ toData txInput ]

foreign import _bondedStateNFT :: Json
foreign import _unbondedStateNFT :: Json
