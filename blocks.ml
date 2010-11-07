open Widget
open Draw
open Window

type 'a block_tree = Block of 'a * 'a list

class block_canvas = object ( self : 'self )
  inherit [ block ] composite as super

end and block = object ( self : 'self )
  inherit draggable
    
end
