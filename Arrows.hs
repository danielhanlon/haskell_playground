animalFriends :: [(String, String)]

animalFriends = [("Pony", "Lion"), ("Lion", "Manticore"), ("Unicorn", "Lepricorn")]

animalFriendLookup :: [(String,String)] -> Maybe String
animalFriendLookup animalMap = case lookup "Pony" animalMap of
  Nothing -> Nothing
  Just ponyFriend ->
    case lookup ponyFriend animalMap of
      Nothing -> Nothing
      Just ponyFriendFriend ->
        case lookup ponyFriendFriend animalMap of
          Nothing -> Nothing
          Just friend -> Just friend

newtype State s a = State {
  runState :: s -> (a,s)
}
