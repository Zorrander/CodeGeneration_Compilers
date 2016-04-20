//
// test02
//
// scanner test: contains valid and invalid tokens
//

// scannable input
<===>=#:=:::==>;;;if(then)else;a:=-1.1;a::==0x5;
1a2b&&hej||
"hello ÇÄ worÄld" 

hello
// invalid input
{
}
?
^
\
'hello world'
'ㄱ'
'Ç'
test
"hello \n world \' how \" are \\ you" hej
testend
testchars
'h''\'''\"'
'"'
'''hh'
endtestchars
'Ä'hej
'Ç'
hej
"
hej
