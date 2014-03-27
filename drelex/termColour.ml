module type S = sig
  val black   : string
  val red     : string
  val green   : string
  val yellow  : string
  val blue    : string
  val pink    : string
  val cyan    : string
  val white   : string

  val bblack  : string
  val bred    : string
  val bgreen  : string
  val byellow : string
  val bblue   : string
  val bpink   : string
  val bcyan   : string
  val bwhite  : string

  val reset   : string

  val escape  : string -> string
end


module ANSI : S = struct
  let black    = "[0;30m"
  let red      = "[0;31m"
  let green    = "[0;32m"
  let yellow   = "[0;33m"
  let blue     = "[0;34m"
  let pink     = "[0;35m"
  let cyan     = "[0;36m"
  let white    = "[0;37m"

  let bblack   = "[1;30m"
  let bred     = "[1;31m"
  let bgreen   = "[1;32m"
  let byellow  = "[1;33m"
  let bblue    = "[1;34m"
  let bpink    = "[1;35m"
  let bcyan    = "[1;36m"
  let bwhite   = "[1;37m"

  let reset    = "[0m"

  let escape s = s
end


module HTML : S = struct
  let span c   = "<span class='" ^ c ^ "'>"

  let black    = span "black"
  let red      = span "red"
  let green    = span "green"
  let yellow   = span "yellow"
  let blue     = span "blue"
  let pink     = span "pink"
  let cyan     = span "cyan"
  let white    = span "white"

  let bblack   = span "bold black"
  let bred     = span "bold red"
  let bgreen   = span "bold green"
  let byellow  = span "bold yellow"
  let bblue    = span "bold blue"
  let bpink    = span "bold pink"
  let bcyan    = span "bold cyan"
  let bwhite   = span "bold white"

  let reset    = "</span>"

  let escape s =
    let buf = Buffer.create (String.length s * 2) in
    String.iter (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | c -> Buffer.add_char buf c
    ) s;
    Buffer.contents buf

end


module None : S = struct
  let black    = ""
  let red      = ""
  let green    = ""
  let yellow   = ""
  let blue     = ""
  let pink     = ""
  let cyan     = ""
  let white    = ""

  let bblack   = ""
  let bred     = ""
  let bgreen   = ""
  let byellow  = ""
  let bblue    = ""
  let bpink    = ""
  let bcyan    = ""
  let bwhite   = ""

  let reset    = ""

  let escape s = s
end


module Make(Term : S) = struct
  open Term

  let escape = Term.escape

  let colour c s = c ^ escape s ^ reset

  let black   = colour black
  let red     = colour red
  let green   = colour green
  let yellow  = colour yellow
  let blue    = colour blue
  let pink    = colour pink
  let cyan    = colour cyan
  let white   = colour white

  let bblack  = colour bblack
  let bred    = colour bred
  let bgreen  = colour bgreen
  let byellow = colour byellow
  let bblue   = colour bblue
  let bpink   = colour bpink
  let bcyan   = colour bcyan
  let bwhite  = colour bwhite
end
