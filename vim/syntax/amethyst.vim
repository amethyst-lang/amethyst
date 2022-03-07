if exists("b:current_syntax")
	finish
endif

let b:current_syntax = "amethyst"

" Keywords
syntax match amethystKeyword "\v[^`1234567890\[\]:\\;',\.#(){}" \n\r\t][^`\[\]:\\;',\.@#(){}" \n\r\t]*|:" contained
highlight link amethystKeyword Keyword
syntax region amethystStartKeyword start="(" end=/[ \n\r\t]/ transparent contains=amethystKeyword

" Operators
syntax match amethystOperator "\v'"
syntax match amethystOperator "\v`"
syntax match amethystOperator "\v,"
syntax match amethystOperator "@"
highlight link amethystOperator Operator

" Values
syntax region amethystString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link amethystString String
syntax match amethystConst "\v[0-9]+|0x[0-9a-fA-F]+|0b[01]+|0o[01234567]+"
syntax match amethystConst "\v[0-9]+((\.[0-9]+)([eE][+\-]?[0-9]+)?|[eE][+\-]?[0-9]+)"
syntax match amethystConst "\vtrue|false"
syntax match amethystConst "\v'(\\.|[^\\'])'"
highlight link amethystConst Constant

" Identifiers
syntax match amethystIdent "\v[^`1234567890\[\]:\\;',\.#(){}" \n\r\t][^`\[\]:\\;',\.@#(){}" \n\r\t]*|:"
highlight link amethystIdent Identifier
syntax match amethystKey "\v:[^`1234567890\[\]:\\;',\.#(){}" \n\r\t][^`\[\]:\\;',\.@#(){}" \n\r\t]*|\=|:"
highlight link amethystKey Keyword

" Amethyst TODO
syntax keyword amethystTodo contained TODO FIXME NOTE
highlight link amethystTodo Todo

" Comments
syntax match amethystComment "\v//.*$" contains=amethystTodo
syn region amethystComment start="\v/\*" end="\v\*/" contains=amethystTodo
highlight link amethystComment Comment

" Parentheses
syntax region amethystParentheses start="(" end=")" fold transparent
