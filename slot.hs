{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Text            (Text)    
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)

data DepositDatum = DepositDatum
    { depositor :: !PubKeyHash
    , amount    :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DepositDatum

            
data DepositRedeemer = Withdraw
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DepositRedeemer

depositValidator :: DepositDatum -> DepositRedeemer -> ScriptContext -> Bool
depositValidator DepositDatum{depositor, amount} Withdraw _ = depositor `elem` txInfoSignatories && amount <= txInfoValueLockedBy txInfo
  where
    txInfo = scriptContextTxInfo ctx
    ctx = scriptContext

depositInstance :: Scripts.TypedValidator DepositDatum

depositInstance = Scripts.mkTypedValidator @DepositDatum
    $$(PlutusTx.compile [|| depositValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DepositDatum @DepositRedeemer

     
depositAddress :: Address
depositAddress = Scripts.validatorAddress depositInstance
depositContract :: Contract () DepositDatum Text ()
depositContract = do
    logInfo @String "Waiting for deposit..."
    utxos <- utxoAt depositAddress
    let depositValue = foldMap (txOutValue . txOutTxOut) utxos
    logInfo @String $ "Current deposit: " ++ show (Value.flattenValue depositValue)
    Contract.handleError (\err -> Contract.logError $ "Error: " ++ unpack err) $ do
        DepositDatum{depositor, amount} <- endpoint @"deposit" @DepositDatum
        let tx = mustPayToTheScript DepositDatum{depositor, amount} depositValue
        void $ submitTxConstraints depositInstance tx
        logInfo @String $ "Deposited " ++ show amount ++ " ADA from " ++ show depositor
    depositContract

withdrawContract :: Contract () DepositDatum Text ()
withdrawContract = do
    DepositDatum{depositor, amount} <- endpoint @"withdraw" @DepositDatum
    let redeemer = Withdraw
        tx = mustPayToPubKey depositor (Ada.lovelaceValueOf amount)
    void $ submitTxConstraintsSpending depositInstance (txOutTxOut $ head $ txInfoOutputs txInfo) tx
    logInfo @String $ "Withdrawn " ++ show amount ++ " ADA to " ++ show depositor
    withdrawContract

endpoints :: Contract () DepositDatum Text ()
endpoints = depositContract <|> withdrawContract

mkSchemaDefinitions ''DepositDatum

$(mkKnownCurrencies [])

