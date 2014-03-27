include SlocMap.Make(struct
  include String

  module Show_t = Deriving_Show.Show_string
end)
