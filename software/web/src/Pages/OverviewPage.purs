module Pages.Overview (OverviewPageState, overviewPage) where

import Prelude

import Api (Api)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Hooks ((<#~>))
import Radio (radioStreams)
import Web.Event.Event (preventDefault)

type OverviewPageState = { api :: Api }

overviewPage :: OverviewPageState -> Nut
overviewPage { api } = do
  DD.div
    [ DA.klass_ "pf-v5-l-grid pf-m-gutter pf-m-all-3-col-on-lg pf-m-all-6-col-on-md" ]
    [ DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Radio" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                [ radioCardBody ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ]
                []
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-l-grid__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-display-lg pf-m-full-height" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Next" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ]
                []
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ]
                []
            ]
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
                , DL.click $ api.polls.streamActivePoll <#>
                    if _ then
                      \_ -> pure unit
                    else
                      \_ -> api.setters.selectStream stream.stream
                ]
                [ DD.input
                    [ DA.klass_ "pf-v5-c-radio__input"
                    , DA.xtype_ "radio"
                    , DA.checked_ <<< show $ stream.stream == selectedStream
                    , DA.disabled $ show <$> api.polls.streamActivePoll
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
              , DA.disabled $ api.polls.streamActivePoll <#> if _ then "true" else ""
              , DL.click_ $ \_ -> api.requests.modifyStream true
              ]
              [ DD.text_ "Start radio"
              ]
          , DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              , DA.disabled $ api.polls.streamActivePoll <#> if _ then "" else "true"
              , DL.click_ $ \_ -> api.requests.modifyStream false
              ]
              [ DD.text_ "Stop radio" ]
          ]
      ]
