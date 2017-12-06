module Main where

import Prelude

import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, gets, get, modify, evalStateT)
import Data.Argonaut (class EncodeJson, downField, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.TraversableWithIndex (traverseWithIndex)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Hyper.Trout.Router (RoutingError(..), router)
import Hyper.Trout.Router (RoutingError, router)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (p, ul, li)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:<|>), type (:>), Capture, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get, Post, Delete, Put)

data Todo = Todo String
data Todos = Todos (Map Int Todo)
data OK = OK

type Site = "todos" := "todos" :/ Resource (Get Todos (HTML :<|> JSON)
                                          :<|> Post Todo (HTML :<|> JSON))
       :<|> "todo"  := "todos" :/ Capture "todo-id" Int :> Resource (Get Todo (HTML :<|> JSON)
                                                               :<|>  Put Todo (HTML :<|> JSON)
                                                               :<|>  Delete OK (HTML :<|> JSON))

instance encodeTodoHTML :: EncodeHTML Todo where
  encodeHTML (Todo g) = p (text g)

instance encodeOkHTML :: EncodeHTML OK where
  encodeHTML OK = p (text "OK")

instance encodeTodosHTML :: EncodeHTML Todos where
  encodeHTML (Todos t) = ul $ traverse_ item t
    where item (Todo n) = li (text n)


instance encodeJsonTodos :: EncodeJson Todos where
  encodeJson (Todos m) = encodeJson (Map.values m)

derive instance genericTodo :: Generic Todo

instance encodeJsonTodo :: EncodeJson Todo where
  encodeJson = gEncodeJson

instance encodeJsonOk :: EncodeJson OK where
  encodeJson OK = encodeJson "OK"

type AppM e a = ReaderT String (StateT Todos (Aff e)) a

runAppM ∷ ∀ e a. String -> Todos -> AppM e a → (Aff e) a
runAppM str todos app = evalStateT (runReaderT app str) todos

site :: Proxy Site
site = Proxy

todoResource
  :: forall m
   . Monad m
  => Int -> {"GET" :: ExceptT RoutingError (ReaderT String (StateT Todos m)) Todo, "PUT" :: ExceptT RoutingError (ReaderT String (StateT Todos m)) Todo, "DELETE" :: ExceptT RoutingError (ReaderT String (StateT Todos m)) OK}
todoResource id =
  {"GET": do
    item <- gets (\(Todos m) -> Map.lookup id m)
    case item of
      Just item -> pure $ item
      Nothing -> throwError (HTTPError { status: statusNotFound
                                        , message: Just "Post not found."
                                        })
  ,"PUT": do
    modify $ (\(Todos m) -> Todos $ Map.insert id (Todo "dupa") m)
    pure $ Todo "dupa"
  ,"DELETE": do
    modify $ (\(Todos m) -> Todos $ Map.delete id m)
    pure OK
  }

todosResource
  :: forall m
   . Monad m
  => {"GET" :: ExceptT RoutingError (ReaderT String (StateT Todos m)) Todos, "POST" :: ExceptT RoutingError (ReaderT String (StateT Todos m)) Todo}
todosResource =
  {"GET": get
  ,"POST": do
    modify $ (\(Todos m) -> Todos $ Map.insert 42 (Todo "dupa") m)
    pure $ Todo "dupa"
  }

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let resources = {"todo": todoResource, "todos": todosResource}
      app = router site resources onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> closeHeaders
        :*> respond (fromMaybe "" msg)

  in runServer' defaultOptionsWithLogging {} (runAppM "Hello" (Todos $ Map.insert 12 (Todo "foo") Map.empty)) app