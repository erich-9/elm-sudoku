module MVC.Widget (News, fromAction, fromNotification, forwardToChild) where

import Signal exposing (Address)


type alias News action notification =
  { actions : List action
  , notifications : List notification
  }


fromAction : a -> News a n
fromAction a =
  { actions = [ a ]
  , notifications = []
  }


fromNotification : n -> News a n
fromNotification n =
  { actions = []
  , notifications = [ n ]
  }


forwardToChild : Address (News a n) -> (a' -> a) -> (n' -> News a n) -> Address (News a' n')
forwardToChild addr actionTaker notificationHandler =
  Signal.forwardTo addr (childNewsHandler actionTaker notificationHandler)


childNewsHandler : (a' -> a) -> (n' -> News a n) -> News a' n' -> News a n
childNewsHandler actionTaker notificationHandler childNews =
  let
    childActions =
      List.map actionTaker childNews.actions

    newsFromChild =
      List.map notificationHandler childNews.notifications |> flatten
  in
    { actions = childActions ++ newsFromChild.actions
    , notifications = newsFromChild.notifications
    }


flatten : List (News a n) -> News a n
flatten newslist =
  { actions = List.foldr (List.append << .actions) [] newslist
  , notifications = List.foldr (List.append << .notifications) [] newslist
  }
