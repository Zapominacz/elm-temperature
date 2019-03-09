module Formatter exposing (..)

import Time exposing (..)

toDigitMonth : Month -> String
toDigitMonth month =
  case month of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

toTwoDigits : Int -> String
toTwoDigits number =
  if number < 10 then
    String.padLeft 2 '0' (String.fromInt number)
  else if number > 99 then
    String.right 2 (String.fromInt number)
  else
    String.fromInt number

formatTemperature : Float -> String
formatTemperature temperature =
  String.append (String.replace "." "," (String.fromFloat temperature)) " °C"

formatDate : Zone -> Posix -> String
formatDate zone date =
  let
    day = toTwoDigits <| Time.toDay zone date
    month = toDigitMonth <| Time.toMonth zone date
    year = toTwoDigits <| Time.toYear zone date
  in
  String.join "/" [day, month, year]

formatTime : Zone -> Posix -> String
formatTime zone time =
  let
    hour = toTwoDigits <| Time.toHour zone time
    minute = toTwoDigits <| Time.toMinute zone time
    second = toTwoDigits <| Time.toSecond zone time
  in
  String.join ":" [hour, minute, second]

formatDateTime : Zone -> Posix -> String
formatDateTime zone dateTime =
  String.join " " [formatTime zone dateTime, formatDate zone dateTime]
