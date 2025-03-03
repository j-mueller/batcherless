-- | Reading plutus scripts from blueprint JSON files
module Batcherless.Offchain.Blueprint (
    Blueprint (..),
    BlueprintValidator (..),
    Preamble (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Blueprint = Blueprint
    { preamble :: Preamble
    , validators :: [BlueprintValidator]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data BlueprintValidator = BlueprintValidator
    { title :: BlueprintKey
    , compiledCode :: Text
    , hash :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Preamble = Preamble
    { description :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype BlueprintKey = BlueprintKey {unBlueprintKey :: Text}
    deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)
