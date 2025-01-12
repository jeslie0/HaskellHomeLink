module Pages.Login (loginPage, backgroundImageNut) where

import Prelude

import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.Pursx (pursx)
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
