module Main exposing (main)

import Formatter exposing (..)
import Browser
import Browser.Navigation
import Debug
import Url exposing (Url)
import Task exposing (..)
import Time exposing (..)
import Html exposing (Html, main_, h1, h2, h3, text, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

type alias Temperature =
  { temperature : Float
  , date : Posix
  }

type Page
  = HomePage
  | TemperaturePage
  | NotFoundPage

type alias Model =
  { temperature : Maybe Temperature
  , zone: Time.Zone
  , key: Browser.Navigation.Key
  , page: Page
  }

type Msg
  = AdjustedTimeZone Time.Zone
  | TickReceived Time.Posix
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url

mockTemperature : Maybe Temperature
mockTemperature = Just { temperature = 13.37, date = Time.millisToPosix 1552043919000 }

init : () -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
    (
    { temperature = Nothing
    , zone = Time.utc
    , key = key
    , page = router url
    }
    , Task.perform AdjustedTimeZone Time.here
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "msg" msg of
        AdjustedTimeZone newZone ->
          (
          { model | zone = newZone }
          , Cmd.none
          -- , Time.now |> Task.perform TickReceived
          )
        TickReceived _ ->
        -- REST API, something...
          ( { model | temperature = mockTemperature }
          , Cmd.none
          )
        LinkClicked urlRequest ->
          case urlRequest of
            Browser.Internal url ->
              ( model
              , Browser.Navigation.replaceUrl model.key url.path
              )
            Browser.External href ->
              ( model
              , Browser.Navigation.load href
              )
        UrlChanged url ->
          ( { model | page = router url }
          , Cmd.none
          )

router : Url -> Page
router url =
  case url.path of
      "/" -> HomePage
      "/temperature" -> TemperaturePage
      _ -> NotFoundPage

notFoundView : Model -> List (Html msg)
notFoundView _ =
  [
    Html.main_ []
      [ h3 [] [ text "Nic nie znalazłem :(" ]
      , h1 [] [ text "404" ]
      , a [ href "/" ] [ text "Zabierz mnie do domu"]
      ]
  ]

temperatureView : Model -> List (Html msg)
temperatureView model =
  case model.temperature of
    Just temperature ->
      [
        Html.main_ []
          [ h3 [] [ text "Temperatura" ]
          , h1 [] [ text <| formatTemperature temperature.temperature ]
          , h2 [] [ text <| formatDateTime model.zone temperature.date]
          ]
      ]
    Nothing ->
      [
        Html.main_ []
          [ h3 [] [ text "Temperatura" ]
          , h1 [] [ text <| "--,-- °C" ]
          , h2 [] [ text <| "Brak danych"]
          ]
      ]

renderBody : Model -> List (Html msg)
renderBody model =
  case model.page of
    TemperaturePage -> temperatureView model
    HomePage -> temperatureView model
    NotFoundPage -> notFoundView model

view : Model -> Browser.Document Msg
view model =
  { title =
      case model.temperature of
        Just temperature -> formatTemperature temperature.temperature
        Nothing -> "Temperature App"
  , body = renderBody model
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10000 TickReceived

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }
