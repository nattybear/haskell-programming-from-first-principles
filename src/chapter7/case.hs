funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True  -> "yes"
    False -> "no"
