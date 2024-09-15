module Patternfly where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import FRP.Poll (Poll)

gallery :: Array Nut -> Nut
gallery = DD.div [ DA.klass_ "pf-v5-l-gallery pf-m-gutter", DA.style_ "--pf-v5-l-gallery--GridTemplateColumns--min: 33%;" ]

galleryItem :: Array Nut -> Nut
galleryItem = DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]

dlistGroup :: String -> Poll String -> Nut
dlistGroup name termPoll =
  DD.div [ DA.klass_ "pf-v5-c-description-list__group" ]
    [ DD.dt [ DA.klass_ "pf-v5-c-description-list__term" ]
        [ DD.span [ DA.klass_ "pf-v5-c-description-list__text" ] [ DC.text_ name ] ]
    , DD.dd [ DA.klass_ "pf-v5-c-description-list__description" ]
        [ DD.div [ DA.klass_ "pf-v5-c-description-list__text" ]
            [ DC.text termPoll ]
        ]
    ]
