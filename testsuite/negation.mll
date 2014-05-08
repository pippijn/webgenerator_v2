rule token = parse
| ~( _* ("AA"|"AB") _* )		{ STUFF }
