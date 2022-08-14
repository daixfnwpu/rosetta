module AClass.ActiveObject where
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.Time
import Data.Time.Clock
import Control.Monad

type Time = UTCTime
type Func a = Time -> a
newtype Integertor a = Integertor (MVar (IntState a))
    deriving Eq
data IntState a = IntState {    func :: Func a,
                                run  :: Bool,
                                value:: a,
                                time :: Time
                            };
timeInterval t0 t1 = realToFrac $ diffUTCTime t1 t0

newItergrator :: Fractional a => IO (Integertor a)
input :: Integertor a -> Func a -> IO()
output :: Integertor a -> IO a
stop   :: Integertor a -> IO()

newItergrator = do
    now <- getCurrentTime
    state <- newMVar $ IntState {
        func = const 0,
        run  = True,
        value = 0,
        time = now
    }
    thread <- forkIO (intThread state)
    return (Integertor state)
input (Integertor stv) f = modifyMVar_  stv (\st -> return st { func=f})
output (Integertor stv) = value <$> readMVar stv
stop (Integertor stv)   = modifyMVar_   stv  (\st -> return st {run = False})

intThread  stv = whileM $ modifyMVar stv updateAndCheckRun
    where updateAndCheckRun st = do
            now <- getCurrentTime
            let value' = integrate (func st) (value st) (time st) now
            evaluate value'
            return (st {value = value',time = now}, run st)

integrate :: Fractional a => Func a -> a -> Time -> Time -> a
integrate f value t0 t1 = value + (f t0 + f t1)/2 * dt
    where dt = timeInterval t0 t1
whileM action = do b <- action ; when b $ whileM action
runMain =   do
    let f = 0.5
    t0 <-getCurrentTime
    i  <- newItergrator
    input i (\t -> sin (2*pi*f * timeInterval t0 t))
    threadDelay 2000000
    input i (const 0)
    threadDelay 500000
    result <- output i
    stop i
    print result