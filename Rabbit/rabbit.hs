import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  -- declare a queue, exchange and binding
  declareQueue chan newQueue {queueName = "myQueue"}
  declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
  bindQueue chan "myQueue" "myExchange" "myKey"

  -- subscribe to the queue

  consumeMsgs chan "myQueue" Ack myCallback
--  consumeMsgs chan "myQueue" Ack myCallback

  -- publish a message to our new exchange
  publishMsg chan "myExchange" "myKey" 
    newMsg {msgBody = (BL.pack "hello world"), 
            msgDeliveryMode = Just Persistent}

  getLine

  publishMsg chan "myExchange" "myKey"
    newMsg {msgBody = (BL.pack "goodnight world"),
            msgDeliveryMode = Just Persistent}

  getLine -- wait for keypress

  publishMsg chan "myExchange" "myKey"
    newMsg {msgBody = (BL.pack "goodnight again"),
            msgDeliveryMode = Just Persistent}

  getLine -- wait for keypress

  closeConnection conn
  putStrLn "connection closed"

                                                                    
myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: "++(BL.unpack $ msgBody msg)
  -- acknowledge receiving the message
  ackEnv env
