module Pages.Login (loginPage) where

import Prelude

import CSS.Background (background)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Deku.Pursx (pursx)
import FRP.Poll (Poll)
import Images (backgroundImage)

backgroundImageNut :: Nut
backgroundImageNut =
  DD.div
    [ DA.klass_ "pf-v5-c-background-image"
    , DA.style_ $ "--pf-v5-c-background-image--BackgroundImage: url(" <> backgroundImage <> ");"
    ]
    []

type LoginPageContent =
  """
<div>Hi</div>
"""

loginPage :: Nut
loginPage =
  -- DD.div_
  --   [ backgroundImageNut
    pursx @LoginPageContent {}
    -- ]
