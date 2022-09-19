module Shared exposing (..)

import Html
import Html.Attributes as Attr


button attrs children =
    Html.button ([ Attr.class "px-2 py-1 rounded-lg bg-sky-500 text-white cursor-pointer shadow-lg" ] ++ attrs) ([] ++ children)
