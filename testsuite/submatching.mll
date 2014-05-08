rule token = parse
(*| (['0'-'9']+ as n)(['u''U''l''L']* as s) & ( '0'[^'8''9']* )		{ Comment }*)
(*| "/*" (~( _* "*/" _* ) as c) "*/"		{ Comment }*)
| "/*" (([^'*']|'*'[^'/'])* as c) "*/"		{ Comment }
