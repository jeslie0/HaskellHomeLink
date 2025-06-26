module Pages.Overview (OverviewPageState, overviewPage) where

import Prelude

import Api (Api)
import Data.Maybe (Maybe(..))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Deku.DOM.Self as Self
import Deku.Hooks ((<#~>))
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Radio (StreamStatus(..), radioStreams)
import Requests (establishCameraConnection, readyVideoStream)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLVideoElement as HTMLVideo

type OverviewPageState = { api :: Api }

overviewPage :: OverviewPageState -> Nut
overviewPage { api } = do
  DD.div
    [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-3-col-on-lg pf-m-all-6-col-on-md" ]
    [ DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Radio"
                    , api.polls.streamStatusPoll <#~>
                        case _ of
                          Initiated -> DD.div [ DA.style_ "float: right;" ]
                            [ DS.svg
                                [ DA.klass_ "pf-v5-c-spinner pf-m-md"
                                , DA.role_ "progressbar"
                                , DSA.viewBox_ "0 0 100 100"
                                ]
                                [ DS.circle
                                    [ DA.klass_ "pf-v5-c-spinner__path"
                                    , DSA.cx_ "50"
                                    , DSA.cy_ "50"
                                    , DSA.r_ "45"
                                    , DSA.fill_ "none"
                                    ]
                                    []
                                ]
                            ]
                          _ -> DD.div [] []
                    ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                [ radioCardBody ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ]
                []
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
        [ cameraStream
        ]
    ]
  where
  radioCardBody =
    DD.form [ DA.klass_ "pf-v5-c-form", DA.formnovalidate_ "", DL.submit_ preventDefault ]
      [ makeStreamRadioGroup
      , streamControlButtons
      ]

  makeStreamRadioGroup =
    api.polls.selectedStreamPoll <#~> \selectedStream ->
      DD.div [ DA.klass_ "pf-v5-c-form__group" ] $
        [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
            [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ "radio_form" ]
                [ DD.div [ DA.klass_ "pf-v5-c-form__label-text" ]
                    [ DC.text_ "Radio stations" ]
                ]
            ]
        ] <>
          ( radioStreams <#> \stream ->
              DD.div
                [ DA.klass_ "pf-v5-c-radio"
                , DL.click $ api.polls.streamStatusPoll <#>
                    case _ of
                      Off ->
                        \_ -> api.setters.selectStream stream.stream
                      _ -> \_ -> pure unit
                ]
                [ DD.input
                    [ DA.klass_ "pf-v5-c-radio__input"
                    , DA.xtype_ "radio"
                    , DA.checked_ <<< show $ stream.stream == selectedStream
                    , DA.disabled $ api.polls.streamStatusPoll <#> case _ of
                        Off -> "false"
                        _ -> "true"
                    ]
                    []
                , DD.label [ DA.klass_ "pf-v5-c-radio__label" ] [ DC.text_ $ show stream.stream ]
                ]
          )

  streamControlButtons =
    DD.div [ DA.klass_ "pf-v5-c-form__group pf-m-action" ]
      [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
          [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ "radio_form" ]
              [ DD.div [ DA.klass_ "pf-v5-c-form__label-text" ]
                  [ DC.text_ "Stream control" ]
              ]
          ]
      , DD.div [ DA.klass_ "pf-v5-c-form__actions" ]
          [ DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              , DA.disabled $ api.polls.streamStatusPoll <#> case _ of
                  Off -> "false"
                  Initiated -> "true"
                  Playing -> "true"
              , DL.click $ api.polls.selectedStreamPoll <#> \selectedStream ->
                  \_ -> api.requests.modifyStream $ Just selectedStream
              ]
              [ DD.text_ "Start radio"
              ]
          , DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              , DA.disabled $ api.polls.streamStatusPoll <#> case _ of
                  Off -> "true"
                  Initiated -> "true"
                  Playing -> "false"
              , DL.click_ $ \_ -> api.requests.modifyStream $ Nothing
              ]
              [ DD.text_ "Stop radio" ]
          ]
      ]

  cameraStream =
    DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
      [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
          [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
              [ DC.text_ "Next" ]
          ]
      , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
          [ DD.video
              [ DA.id_ "videoPlayer"
              , DA.controls_ ""
              , DA.autoplay_ ""
              , Self.self_ $ \el -> launchAff_ do
                  delay (Milliseconds 0.0)
                  sock <- liftEffect $ establishCameraConnection api.websocket
                  case HTMLVideo.fromElement el of
                    Nothing -> liftEffect $ Console.log "Couldn't make video element"
                    Just videoEl -> liftEffect $ readyVideoStream sock videoEl
              ]
              []
          ]
      , DD.div [ DA.klass_ "pf-v5-c-card__footer" ]
          [ DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              -- , DA.disabled $ api.polls.streamStatusPoll <#> case _ of
              --     Off -> "false"
              --     Initiated -> "true"
              --     Playing -> "true"
              -- , DL.click $ api.polls.selectedStreamPoll <#> \selectedStream ->
              --     \_ -> api.requests.startCameraStream
              ]
              [ DD.text_ "Start stream"
              ]
          , DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              -- , DA.disabled $ api.polls.streamStatusPoll <#> case _ of
              --     Off -> "true"
              --     Initiated -> "true"
              --     Playing -> "false"
              -- , DL.click_ $ \_ -> api.requests.stopCameraStream
              ]
              [ DD.text_ "Stop stream" ]
          ]
      ]
