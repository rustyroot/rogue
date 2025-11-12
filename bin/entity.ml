class entity (initial_position : int*int) =
  object
    val mutable position = initial_position
    method get_pos = position
    method set_pos new_position = position <- new_position
  end