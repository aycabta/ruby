#
#  tradiobutton widget
#                               by Hidetoshi NAGAI (nagai@ai.kyutech.ac.jp)
#
require 'tk'
require 'tkextlib/tile.rb'

module Tk
  module Tile
    class TRadioButton < TkRadioButton
    end
    TRadiobutton = TRadioButton
  end
end

class Tk::Tile::TRadioButton < TkRadioButton
  include Tk::Tile::TileWidget

  if Tk::Tile::USE_TTK_NAMESPACE
    TkCommandNames = ['::ttk::radiobutton'.freeze].freeze
  else
    TkCommandNames = ['::tradiobutton'.freeze].freeze
  end
  WidgetClassName = 'TRadiobutton'.freeze
  WidgetClassNames[WidgetClassName] = self
end
