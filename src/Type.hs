module Type where

import           Location
import           Parse

-- | The result of evaluating an interaction.
data DocTestResult =
    Success
  | InteractionFailure (Located Interaction) [String]
  | PropertyFailure (Located Expression) String
  | Error (Located Expression) String
  deriving (Eq, Show)
