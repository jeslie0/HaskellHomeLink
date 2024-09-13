module Main where

import Prelude

import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console (log)
import Deku.DOM as DD
import Deku.Control as DC
import Deku.Do as Deku
import Deku.DOM.Attributes as DA
import Deku.Core (Nut)

main :: Effect Unit
main = do
  log "Hello"
  _ <- runInBody dekuApp
  pure unit

header :: Nut
header =
  DD.header [ DA.klass_ "pf-v5-c-masthead" ]
    [ DD.div [ DA.klass_ "pf-v5-c-masthead__main" ] []
    , DD.div [ DA.klass_ "pf-v5-c-masthead__content" ] [ DC.text_ "Content" ]
    ]

pageBody :: Nut
pageBody =
  DD.main [ DA.klass_ "pf-v5-c-page__main" ]
    [ DD.section [ DA.klass_ "pf-v5-c-page__main-section pf-m-dark-100" ]
        [ DC.text_ "Hello, world!"
        ]
    ]

dekuApp :: Nut
dekuApp = Deku.do
  DD.div [ DA.klass_ "pf-v5-c-page" ]
    [ header
    , pageBody
    ]
