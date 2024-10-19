module Pages.Overview (OverviewPageState, initialOverviewPageState, overviewPage) where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import FRP.Poll (Poll)

type OverviewPageState =
  { state :: Int
  }

initialOverviewPageState :: OverviewPageState
initialOverviewPageState = { state: 0 }

overviewPage :: Poll OverviewPageState -> Nut
overviewPage _ =
  DD.div [ DA.klass_ "pf-v5-l-gallery pf-m-gutter" ]
    [ DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Title" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ DC.text_ "Body" ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Title" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ DC.text_ "Body" ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Title" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ DC.text_ "Body" ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]
        [ DD.div [ DA.klass_ "pf-v5-c-card" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ]
                    [ DC.text_ "Title" ]
                ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ DC.text_ "Body" ]
            , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ DC.text_ "Footer" ]
            ]
        ]
    ]
