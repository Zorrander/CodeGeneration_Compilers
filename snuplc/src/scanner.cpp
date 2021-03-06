//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

char ETokenName[][TOKEN_STRLEN] = {
  "tDigit",                         ///< a digit
  "tLetter",                        ///< a letter
  "tTermOp",                        ///< '+' or '-' or '||'
  "tFactOp",                        ///< '*' or '/' or '&&'
  "tRelOp",                         ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tDot",                           ///< a dot
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket

  "tComma",                         ///< a comma
  "tColon",                         ///< a colon
  "tLSqBrak",                       ///< a left square bracket
  "tRSqBrak",                       ///< a right square bracket

  "tNot",                           ///< a '!'
  
  "tIdent",                         ///< an identifier
  "tNumber",                        ///< a number
  "tChar",                          ///< a character
  "tString",                        ///< a string
  
  "tKeyword",                       ///< a keyword

  "tIf",                            ///< an if statement
  "tWhile",                         ///< a while statement
  "tReturn",                        ///< a return statement

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tDigit (%s)",                    ///< a digit
  "tLetter (%s)",                   ///< a letter
  "tTermOp (%s)",                   ///< '+' or '-' or '||'
  "tFactOp (%s)",                   ///< '*' or '/' or '&&'
  "tRelOp (%s)",                    ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tDot",                           ///< a dot
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket

  "tComma",                         ///< a comma
  "tColon",                         ///< a colon
  "tLSqBrak",                       ///< a left square bracket
  "tRSqBrak",                       ///< a right square bracket
  
  "tNot",                           ///< a '!'

  "tIdent (%s)",                    ///< an identifier
  "tNumber (%s)",                   ///< a number
  "tChar (%s)",                     ///< a character
  "tString (%s)",                   ///< a string
  
  "tKeyword (%s)",                  ///< a keyword

  "tIf",                            ///< an if statement
  "tWhile",                         ///< a while statement
  "tReturn",                        ///< a return statement
  
  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)",                ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
  {
    
    std::make_pair("module", tKeyword),
    std::make_pair("procedure", tKeyword),
    std::make_pair("function", tKeyword),
    std::make_pair("var", tKeyword),
    std::make_pair("integer", tKeyword),
    std::make_pair("boolean", tKeyword),
    std::make_pair("char", tKeyword),
    std::make_pair("begin", tKeyword),
    std::make_pair("end", tKeyword),
    std::make_pair("if", tKeyword),
    std::make_pair("then", tKeyword),
    std::make_pair("else", tKeyword),
    std::make_pair("while", tKeyword),
    std::make_pair("do", tKeyword),
    std::make_pair("return", tKeyword),
    std::make_pair("true", tKeyword),
    std::make_pair("false", tKeyword),
    
  };



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    switch (*t) {
    case '\n': s += "\\n";  break;
    case '\t': s += "\\t";  break;
    case '\0': s += "\\0";  break;
    case '\'': s += "\\'";  break;
    case '\"': s += "\\\""; break;
    case '\\': s += "\\\\"; break;
    default :  s += *t;
    }
    t++;
  }

  return s;
}

string CToken::unescape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    if (*t == '\\') {
      switch (*++t) {
        case 'n': s += "\n";  break;
        case 't': s += "\t";  break;
        case '0': s += "\0";  break;
        case '\'': s += "'";  break;
        case '"': s += "\""; break;
        case '\\': s += "\\"; break;
        default :  s += '?';
      }
    } else {
      s += *t;
    }
    t++;
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
 
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();
  
  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  
  while (c == '/' && _in->peek() == '/'){

    GetChar();
    while (_in->good() && GetChar() != '\n') {}
    while (_in->good() && IsWhite(_in->peek())) GetChar();

    RecordStreamPosition();

    if (_in->eof()) return NewToken(tEOF);
    if (!_in->good()) return NewToken(tIOError);

    c = GetChar();
  }

  tokval = c;
  token = tUndefined;

  string temp_str = "";

  switch (c) {
  case ':':
    if (_in->peek() == '=') 
      {
	tokval += GetChar();
	token = tAssign;
      }
    else
      {
	token = tColon;
      }
    break;

  case '+':
  case '-':
    token = tTermOp;
    break;

  case '*':
  case '/':
    token = tFactOp;
    break;
    
  case '|':
    if (_in->peek() == '|')
      {
	tokval += GetChar();
	token = tTermOp;
      }   
    break;

  case '&':
    if (_in->peek() == '&')
      {
	tokval += GetChar();
	token = tFactOp;
      }   
    break;

  case '!':
    token = tNot;
    break;
    
  case '=':
  case '#':
  case '<':
  case '>':
    if (c == '<' && _in->peek() == '=')
      tokval += GetChar();
    else if (c == '>' && _in->peek() == '=')
      tokval += GetChar();
    
    token = tRelOp;
    break;

  case ';':
    token = tSemicolon;
    break;

  case '.':
    token = tDot;
    break;

  case '(':
    token = tLBrak;
    break;

  case ')':
    token = tRBrak;
    break;
    
  case ',':
    token = tComma;
    break;

  case '[':
    token = tLSqBrak;
    break;

  case ']':
    token = tRSqBrak;
    break;

  case '\'':
    tokval = ""; // flushes beginning '\''
    if((c = GetChar()) == '\'') // Takes care of case with empty char: ''
      {
	tokval = "";
	token = tChar;
	break;
      }

    bool b;
    b = IsCharacter(c, &tokval);
    c = GetChar();
    
    if (c == '\'' && b)
      {
        token = tChar;
      }
    else if (c == '\'' && !b) // redundant because illegal characters overflow
      {
	string temp = "invalid character '";
	temp += tokval;
	tokval = temp += "'";
      }
    else
      {
	while (_in->good() && (c = GetChar()) != '\'' ) {} // flushing until ending "'"
	tokval = "more than one character detected";
      }

    break;

  case '\"':
    
    tokval = ""; // flushes beginning '\"'
    while (_in->good() && (c = GetChar()) != '\"') // reads string and flushes ending '\"'
      {
	temp_str = "";
	if(IsCharacter(c, &temp_str))
	  {
	    tokval.append(temp_str); 
	  }
      }
    token = tString;
    break;

  default:
    if (IsDigit(c)) {
      while (IsDigit(_in->peek())) {
	tokval += GetChar();
      }
      token = tNumber;
    } else
      if (IsLetter(c)) {
	while (IsLetter(_in->peek()) || IsDigit(_in->peek())) {
	  tokval += GetChar();
	}
	if (IsKeyword(tokval)){
	  token = tKeyword;
	}
	else
	  token = tIdent;
      } else {
        tokval = "invalid letter '";
        tokval += c;
        tokval += "'";
      }
    break;
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n'));
}

bool CScanner::IsLetter(char c) const
{
  return ( (('a' <= c) && (c <= 'z')) || 
	   (('A' <= c) && (c <= 'Z')) || 
	   (c == '_') );
}

bool CScanner::IsDigit(char c) const
{
  return ( ('0' <= c) && (c <= '9') );
}

bool CScanner::IsCharacter(char c, string *tokval)
{
  bool b;
    
  *tokval += c;
  if ( c == '\\' && IsCharEscape(_in->peek()) )
    {
      c = GetChar();
      *tokval += c;
      b = true;
    }
  else if( c == '\\' && !IsCharEscape(_in->peek()) )
    {
      c = GetChar();
      *tokval += c;
      b =  false;
    }
  else if( c >= 32 && c <= 127 )
    b = true;
  else
    b = false;
  
  return b;
}

bool CScanner::IsKeyword(string str)
{
  int size = sizeof(Keywords) / sizeof(Keywords[0]);
  bool res = false;
  const char * c = str.c_str();
  for (int i = 0; i < size; i++)
    {
      if (strcmp(c, Keywords[i].first) == 0)
	{
	  res = true;
	  break;
	}
    }
  return res;
}

bool CScanner::IsCharEscape(char c)
{
  return (c == 'n' || c == 't' || c == '0' || c == '\'' || c == '\"' || c == '\\');
}
