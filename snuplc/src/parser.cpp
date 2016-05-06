//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include <string.h>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();
  
  // TODO: add predefined functions here
  // s->GetSymbolTable()->AddSymbol( s->CreateVar(t.GetValue(), ct) );
  s->AddSymbol( new CSymbol("DIM", stProcedure, tm->GetInt()) );
  s->AddSymbol( new CSymbol("DOFS", stProcedure, tm->GetInt()) );
  s->AddSymbol( new CSymbol("ReadInt", stProcedure, tm->GetInt()) );
  s->AddSymbol( new CSymbol("WriteChar", stProcedure, tm->GetNull()) );
  s->AddSymbol( new CSymbol("WriteInt", stProcedure, tm->GetNull()) );
  s->AddSymbol( new CSymbol("WriteLn", stProcedure, tm->GetNull()) );
  s->AddSymbol( new CSymbol("WriteStr", stProcedure, tm->GetNull()) );
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident ".".
  //
  // FIRST(module) ::= { tKeyword = "module" }
  //
  // FIRST(varDeclaration) ::= { tKeyword = "var", e }
  // FOLLOW(varDeclaration) ::= { tKeyword = "procedure" | "function" | "begin" }
  //
  // FIRST(subroutineDecl) ::= { tKeyword = "procedure" | "function" }
  // FOLLOW(subroutineDecl) ::= { tKeyword = "begin" }
  //
  // FIRST(statSequence) ::= { tIdent, tKeyword = "if" | "while" | "return" }
  // FOLLOW(statSequence) ::= { tKeyword = "end" | "else }
  //
  CToken dummy;
  CAstModule *m;
  CAstStatement *statseq = NULL;
  
  CToken t;
  CToken moduleName;

  if (_scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("module")){
    Consume(tKeyword, &t);

    if(_scanner->Peek().GetType() == tIdent){
      Consume(tIdent, &moduleName);
      m = new CAstModule(t, moduleName.GetValue());
      
      if(_scanner->Peek().GetType() == tSemicolon){
	Consume(tSemicolon, &t);    

	InitSymbolTable(m->GetSymbolTable());

	// varDeclaration
	varDeclaration(m);

	// optional subroutineDecl
	CAstScope *p;
	string str = _scanner->Peek().GetValue();
	while ( !str.compare("procedure") || !str.compare("function") )
	  {
	    p = subroutineDecl(m);
	    str = _scanner->Peek().GetValue();
	  }
	
	// begin module
	if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("begin") ){ 
	 Consume(tKeyword, &t);
	 
	 // statSequence
         statseq = statSequence(m);
         m->SetStatementSequence(statseq);
	 
	 // end module
	 if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("end") ){
	   Consume(tKeyword, &t);
	   Consume(tIdent, &t);
	   if (moduleName.GetValue().compare(t.GetValue()))
	     {
	       SetError(_scanner->Peek(), "module declaration and end did not match.");
	     }

	   Consume(tDot, &t); 
	 }
	}
	else {
	  SetError(_scanner->Peek(), "expected keyword \"begin\"");
	}
      }
      else{
	SetError(_scanner->Peek(), "expected \";\"");
      }
    }
    else{
      SetError(_scanner->Peek(), "expected tIdent");
    }
  }
  else {
      SetError(_scanner->Peek(), "first keyword should be \"module\"");
  }

  // ### maybe do verifications before returning m in case it's not assigned 
  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  //
  // FIRST(statement) ::= { tIdent, tKeyword = "if" | "while" | "return" }
  // FOLLOW(statement) ::= { tKeyword = "end" | "else", ";" }
  //

  bool isFollow;
  bool noColon;

  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tDot)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;
      
      isFollow = false;
      
      switch (tt) {
        
      case tIdent:
	Consume(tIdent, &t);
	if (_scanner->Peek().GetType() == tLBrak)
	  {
	    // statement ::= subroutineCall
	    st = subroutineCall(s, t, 0);
	  }
	else
	  {
	    // statement ::= assignment
	    st = assignment(s, t);
	  }
	break;
        
      case tKeyword:
	
	// statement ::= ifStatement
	CAstExpression *ex;
	if (!_scanner->Peek().GetValue().compare("if")) 
	  {  
	    CAstStatement *stIf, *stElse = NULL;
	    Consume(tKeyword, &t);
	    Consume(tLBrak);
	    ex = expression(s);
	    Consume(tRBrak);
	    
	    if (_scanner->Peek().GetValue().compare("then"))
	      {
		SetError(_scanner->Peek(), "Keyword \"then\" expected");
		break;
	      }
	    Consume(tKeyword);

	    stIf = statSequence(s);
	    if (!_scanner->Peek().GetValue().compare("else"))
		{
		  Consume(tKeyword);
		  stElse = statSequence(s);
		}
	    if (!_scanner->Peek().GetValue().compare("end"))
	      {
		Consume(tKeyword);
		st = new CAstStatIf(t, ex, stIf, stElse);
		break;
	      }
	    	    
	    SetError(_scanner->Peek(), "expected \'end\'");
	    break;
	  }
	
	// statement ::= whileStatement
	else if (!_scanner->Peek().GetValue().compare("while")) 
	  {  
	    Consume(tKeyword, &t);
	    Consume(tLBrak);
	    ex = expression(s);
	    Consume(tRBrak);

	    if (_scanner->Peek().GetValue().compare("do"))
	      {
		SetError(_scanner->Peek(), "expected \'do\'");
		break;
	      }

	    Consume(tKeyword);
	    st = statSequence(s);
	    st = new CAstStatWhile(t, ex, st);
	    
	    if (_scanner->Peek().GetValue().compare("end"))
	      {
		SetError(_scanner->Peek(), "expected \'end\'");
		break;
	      }

	    Consume(tKeyword, &t);
	    break;
	  }
	
	// statement ::= returnStatement
	else if (!_scanner->Peek().GetValue().compare("return")) 
	  {  
	    Consume(tKeyword, &t);
	    
	    if ( _scanner->Peek().GetValue().compare("end") && _scanner->Peek().GetValue().compare("else") )
	      {
		ex = expression(s);
	      }
	    if ( _scanner->Peek().GetType() == tSemicolon )
	      {
		SetError(_scanner->Peek(), "do not use semicolon to terminate return.");
	      }
	    st = new CAstStatReturn(t, s, ex);
	    break;
	  }
	
	// Sets follow true to exit current statement sequence scope
	else if(!_scanner->Peek().GetValue().compare("else"))
	  {
	    isFollow = true;
	    break;
	  }
	else if (!_scanner->Peek().GetValue().compare("end"))
	  {
	    isFollow = true;
	    break;
	  }

	else { SetError(_scanner->Peek(), "Got \'" + t.GetValue() + "\', expected keyword { if | while | return | else | end } expected." ); }
	break;
	
      default:
	SetError(_scanner->Peek(), "statement expected.");
	break;
      }
      
      tt = _scanner->Peek().GetType();
      if (tt == tDot) break;

      if ( !_scanner->Peek().GetValue().compare("end") || !_scanner->Peek().GetValue().compare("else") )
	{
	  isFollow = true;
	}

      if (st != NULL)
	{
	  if (head == NULL) head = st;
	  else tail->SetNext(st);
	  tail = st;
	}

      if (isFollow) { break; }
      else { Consume(tSemicolon); }
      
    } while (!_abort);
  }
  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s, CToken t)
{
  //
  // assignment ::= qualident ":=" expression.
  //
  
  CAstDesignator *lhs = qualident(s, t);

  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
  //
  // FIRST(expression) ::= { "+", "-", FIRST(factor) }
  // FIRST(factor) ::= { tIdent, tNumber, "true", "false", tChar, tString, "(", subroutineCall, "!" }
  // FOLLOW(expression) ::= { "]", ")", ",", tDot }
  // 
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= term { termOp term }.
  // 
  // FIRST(simpleexpr) ::= { "+", "-", FIRST(factor) }
  // FOLLOW(simpleexpr) ::= { relOp, tDot }
  //
  CAstExpression *n = NULL;
  CAstExpression *l = n, *r;
  CToken t;
  
  string str;
  
  if ( !_scanner->Peek().GetValue().compare("+") || !_scanner->Peek().GetValue().compare("-") )
    {
      Consume(tTermOp, &t);
      r = term(s);
      n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, r);
    }
  else
    {
      n = term(s);
    }
  
  while (_scanner->Peek().GetType() == tTermOp) {
    
    l = n;

    Consume(tTermOp, &t);
    r = term(s);

    str = t.GetValue();
    if ( !str.compare("+") )
      {
	n = new CAstBinaryOp(t, opAdd, l, r);
      }
    else if ( !str.compare("-") )
      {
	n = new CAstBinaryOp(t, opSub, l, r);
      }
    else
      {
	n = new CAstBinaryOp(t, opOr, l, r);
      }
  }
  
  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/"|"&&") factor }.
  //
  CAstExpression *n = NULL;

  string str;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tFactOp)) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tFactOp, &t);
    r = factor(s);

    str = t.GetValue();
    if ( !str.compare("*") )
      {
	n = new CAstBinaryOp(t, opMul, l, r);
      }
    else if ( !str.compare("-") )
      {
	n = new CAstBinaryOp(t, opDiv, l, r);
      }
    else
      {
	n = new CAstBinaryOp(t, opAnd, l, r);
      }
        
    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")"
  //
  // FIRST(factor) = { FIRST(qualident) = tIdent , tNumber, tKeyword (bool), char, string, tLBrak, subRoutine, "!" }
  //
  CToken t;
  EToken tt = _scanner->Peek().GetType();
  string str = _scanner->Peek().GetValue();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    
    // factor ::= qualident || subroutineCall
  case tIdent:
    Consume(tIdent, &t);
    if(_scanner->Peek().GetType() == tLBrak)
      {
	n = subroutineCall(s, t);
      }
    else
      {
	n = qualident(s, t);
      }
    break;
    
    // factor ::= number
  case tNumber:
    n = number();
    break;
    
    // factor ::= bool
  case tKeyword:
    n = boolean();
    break;
    
    // factor ::= "(" expression ")"
  case tLBrak:
    Consume(tLBrak, &t);
    n = expression(s);
    Consume(tRBrak, &t);
    break;

    //factor ::= !factor
  case tNot:
    Consume(tNot, &t);
    unary = factor(s);
    n = new CAstUnaryOp(t, opNot, unary);
    break ;

  case tChar:
    // factor ::= char 
    n = character();
    break;

    //factor ::= string 
  case tString:
    Consume(tString, &t);
    n = new CAstStringConstant(t, CToken::unescape(t.GetValue()), s);
    break ;

  default:
    cout << "got " << _scanner->Peek() << endl;
    SetError(_scanner->Peek(), "factor expected.");
    break;
  }

  return n;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);
  
  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if ( v > 2147483648 ) { errno = 1; }
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

int CParser::escapechar2int(const string text)
{
  
  int c;
  if ( !text.compare("\\n") ) { c = 10; }
  else if ( !text.compare("\\t") ) { c = 9; }
  else if ( !text.compare("\\0") ) { c = 0; }
  else if ( !text.compare("\\'") ) { c = 39; }
  else if ( !text.compare("\\\"") ) { c = 34; }
  else if ( !text.compare("\\\\") ) { c = 92; }
  else { c = (int)text[0]; }
  
  return c;
}

CAstConstant* CParser::character(void)
{
  //
  // character
  //
  
  CToken t;

  Consume(tChar, &t);

  string str = CToken::unescape(t.GetValue());
  int val = escapechar2int(str);
  
  return new CAstConstant(t, CTypeManager::Get()->GetChar(), (long long)val);
}

CAstConstant* CParser::boolean(void)
{
  
  CToken t;

  Consume(tKeyword, &t);

  long long v;
  if (!t.GetValue().compare("true"))
    {
      v = 1;
    }
  else if (!t.GetValue().compare("false"))
    {
      v = 0;
    }
  else
    {
      SetError(t, "expected boolean (true/false)");
    }
  // ### Is it correct to return bool as 1 || 0 ???
  return new CAstConstant(t, CTypeManager::Get()->GetBool(), (bool)v);
}

CAstDesignator* CParser::qualident(CAstScope* s, CToken t)
{
  //
  // qualident ::= ident { "[" expression "]" }
  // ident ::= tIdent
  // expression ::= simpleexpr [ relOp simplexpr ].
  // FIRST(qualident) = { tIdent }
  // FOLLOW(qualident) = { tAssign | tDot }
  
  // tIdent will already be comsumed before this function starts (?)
  // Have to check for { "[" expression "]" }
  
  const string str = t.GetValue();

  CAstExpression* ex;
  CAstDesignator* n; 

  CAstArrayDesignator *test;

  CSymtab* st = s->GetSymbolTable();
  const CSymbol* sy = st->FindSymbol(t.GetValue());

  if (sy == NULL)
    { SetError(_scanner->Peek(), "variable not declared in this scope."); }
  
  n = new CAstDesignator(t, sy);
  test = new CAstArrayDesignator(t, sy);
  
  bool isArray = false;
  while (_scanner->Peek().GetType() == tLSqBrak)
    {
      isArray = true;
      Consume(tLSqBrak);
      ex = expression(s);
      Consume(tRSqBrak);

      // ### Added check for accessing elements < 0
      if ( ex->GetToken().GetType() == tTermOp )
	{
	  SetError( ex->GetToken(), "attempted to access index < 0.");
	}
      test->AddIndex(ex);
    }
  
  if (isArray)
    {
      test->IndicesComplete();
      return test;
    }
  else
    return n;
}

CAstFunctionCall* CParser::subroutineCall(CAstScope* s, CToken t)
{
  //
  // tIdent is already consumed
  //
  CAstFunctionCall *fn;
  CAstExpression *ex;
  CSymtab* st = s->GetSymbolTable();
  const CSymbol* sy = st->FindSymbol(t.GetValue());

  if (sy == NULL)
    { SetError(_scanner->Peek(), "procedure/function not declared in this scope."); }
  
  CSymProc *sp = new CSymProc(sy->GetName(), sy->GetDataType());

  fn = new CAstFunctionCall(t, sp);

  Consume(tLBrak, &t);
  if(_scanner->Peek().GetType() == tRBrak)
    {
      Consume(tRBrak, &t);
    }
  else
    {
      ex = expression(s);
      fn->AddArg(ex);
      while (_scanner->Peek().GetType() == tComma)
	{
	  Consume(tComma, &t);
	  ex = expression(s);
	  fn->AddArg(ex);
	}
      Consume(tRBrak, &t);
    }
    
  return fn;
}

CAstStatCall* CParser::subroutineCall(CAstScope* s, CToken t, int dummy)
{
  //
  // tIdent is already consumed
  //
  CAstFunctionCall *fn;
  CAstExpression *ex;
  
  CSymtab* st = s->GetSymbolTable();
  const CSymbol* sy = st->FindSymbol(t.GetValue());

  if (sy == NULL)
    { SetError(_scanner->Peek(), "procedure/function not declared in this scope."); }

  CSymProc *sp = new CSymProc(sy->GetName(), sy->GetDataType());
  
  fn = new CAstFunctionCall(t, sp);
  
  Consume(tLBrak, &t);
  if(_scanner->Peek().GetType() == tRBrak)
    {
      Consume(tRBrak, &t);
    }
  else
    {
      ex = expression(s);
      fn->AddArg(ex);
      while (_scanner->Peek().GetType() == tComma)
	{
	  Consume(tComma, &t);
	  ex = expression(s);
	  fn->AddArg(ex);
	}
      Consume(tRBrak, &t);
    }
  
  return new CAstStatCall(t, fn);
}

void CParser::varDeclaration(CAstScope* s){
  //
  // FOLLOW(varDeclaration) = { "begin", "procedure", "function" }
  // FIRST(var)
  //
  CToken t ;
  
  //varDeclaration ::= "var" varDeclSequence ";"
  if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("var")){
    Consume(tKeyword, &t);
    varDeclSequence(s);
  }
}

void CParser::varDeclSequence(CAstScope* s){
  //
  // varDeclSequence ::= varDecl { ";" varDecl }
  //
  CToken t;
  varDecl(s);
  if (_scanner->Peek().GetType() != tRBrak)
      {
	Consume(tSemicolon, &t);
      }
  string str = _scanner->Peek().GetValue();
  
  while ( str.compare("procedure") && str.compare("function") && (_scanner->Peek().GetType() != tRBrak) && str.compare("begin") ){
    
    varDecl(s) ; 
    if (_scanner->Peek().GetType() != tRBrak)
      {
	Consume(tSemicolon, &t);
      }
    str = _scanner->Peek().GetValue();
  }
    
}

const CType* CParser::varDecl(CAstScope* s){
  //
  // Recursive function that returns the type of the variable
  //
  EToken nextToken;
  CToken t, val, baseType;
  string str;
  const CType *ct;
  
  CToken err_pos = _scanner->Peek();
  Consume(tIdent, &t);
  if (_scanner->Peek().GetType() == tComma){
    Consume(tComma); 
    ct = varDecl(s);
  }
  else
    {
      Consume(tColon);
    }
  
  nextToken = _scanner->Peek().GetType();
  str = _scanner->Peek().GetValue();

  if (nextToken == tKeyword)
    {
      // type ::= basetype
      if(!str.compare("integer") || !str.compare("boolean") || !str.compare("char"))
	{
	  Consume(tKeyword, &baseType);
	  
	  // Set type
	  if (!str.compare("integer"))
	    { ct = CTypeManager::Get()->GetInt(); }
	  
	  if (!str.compare("char"))
	    { ct = CTypeManager::Get()->GetChar(); }
	  
	  if (!str.compare("boolean"))
	    { ct = CTypeManager::Get()->GetBool(); }
	  
	  // Create array using type and expression
	  while (_scanner->Peek().GetType() == tLSqBrak)
	    {
	      Consume(tLSqBrak);
	      if (_scanner->Peek().GetType() == tNumber)
		{
		  Consume(tNumber, &val);
		  ct = new CArrayType(stoi(val.GetValue()), ct);
		}
	      else
		{
		  ct = new CArrayType(-1, ct);
		}
	      Consume(tRSqBrak);
	    }
	}
      else {
	SetError(_scanner->Peek(), "not a regular type.");
      }
    }

  bool b = s->GetSymbolTable()->AddSymbol( s->CreateVar(t.GetValue(), ct) );
  if (!b) { SetError(err_pos, "duplicate variable declaration"); }
  
  return ct;
}

CAstScope* CParser::subroutineDecl(CAstScope* s){
  //
  //subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";"
  //
  CToken t, tName; 
  EToken tt = _scanner->Peek().GetType();
  string str = _scanner->Peek().GetValue();
  CAstStatement *statseq;

  CAstScope *sr;
  
  if (!str.compare("procedure"))
    {
      sr = procedureDecl(s);
    } 
  else
    {
      sr = functionDecl(s);
    }

  
  
  statseq = subroutineBody(sr);
  sr->SetStatementSequence(statseq);

  str = _scanner->Peek().GetValue();
  if (!str.compare(sr->GetName()) || !str.compare(sr->GetName()))
    {
      Consume(tIdent, &t);
      Consume(tSemicolon, &t);
    }
  else
    {
      SetError(_scanner->Peek(), "end of procedure/function does not match declaration.");
    }
}

CAstScope* CParser::procedureDecl(CAstScope* s){
  //
  //procedureDecl ::= "procedure" ident [formalParam] ";"
  //
  CToken t, tName;
  CAstScope *sr;

  Consume(tKeyword, &t);
  CToken err_pos = _scanner->Peek();
  Consume(tIdent, &tName);
  // Creates CSymbol for Procedure
  
  CSymProc *temp = new CSymProc(tName.GetValue(), CTypeManager::Get()->GetNull());
  
  // Creates Scope for Procedure
  sr = new CAstProcedure(t, tName.GetValue(), s, temp);

  if(_scanner->Peek().GetType() == tLBrak)
    {
      formalParam(sr);
    }
  Consume(tSemicolon, &t);

  //temp->AddParam(new CSymParam(0, "int", CTypeManager::Get()->GetBool()));
  //temp->AddParam(new CSymParam(1, "hej2", CTypeManager::Get()->GetInt()));
  //int N = temp->GetNParams();
  //cout << "num params: " << N << endl;
  
  bool b = s->GetSymbolTable()->AddSymbol( s->CreateVar(tName.GetValue(), temp->GetDataType()) );
  if (!b) { SetError(err_pos, "duplicate procedure declaration"); }
  //s->GetSymbolTable()->AddSymbol( s->CreateVar(tName.GetValue(), temp) );
  temp->AddParam(new CSymParam(0, "int", CTypeManager::Get()->GetBool()));
  return sr;
}

CAstScope* CParser::functionDecl(CAstScope* s){
  //
  // functionDecl ::= "function" ident [formalParam] ":" type ";"
  //
  CToken t, tName;
  CAstScope *sr;
  
  //CAstScope *s;
  
  Consume(tKeyword, &t);
  CToken err_pos = _scanner->Peek();
  Consume(tIdent, &tName);
  
  // Creates CSymbol for FunctionCall, DataType is not set here
  // CSymProc *temp = new CSymProc(tName.GetValue(), CTypeManager::Get()->GetPointer(CTypeManager::Get()->GetNull()));
  CSymProc *temp = new CSymProc(tName.GetValue(),CTypeManager::Get()->GetNull());
  // Creates Scope for Procedure
  sr = new CAstProcedure(t, tName.GetValue(), s, temp);

  if (_scanner->Peek().GetType() == tColon)
    {
      Consume(tColon);
    }
  else
    {
      formalParam(sr);
      Consume(tColon);
    }
  
  string str = _scanner->Peek().GetValue();
      
  if(!str.compare("integer") || !str.compare("boolean") || !str.compare("char")){
    Consume(tKeyword, &t);
    
    // Set DataType
    if (!str.compare("integer"))
      { 
	temp->SetDataType( CTypeManager::Get()->GetInt() );
      }
    if (!str.compare("char"))
      { 
	temp->SetDataType( CTypeManager::Get()->GetChar() );
      }
    if (!str.compare("boolean"))
      { 
	temp->SetDataType( CTypeManager::Get()->GetBool() );
      }
  }
  else {
    SetError(_scanner->Peek(), "not a basetype");
  }
  Consume(tSemicolon);
  
  bool b = s->GetSymbolTable()->AddSymbol( s->CreateVar(tName.GetValue(), temp->GetDataType()) );
  if (!b) { SetError(err_pos, "duplicate function declaration"); }
  return sr;
}

void CParser::formalParam(CAstScope* s){
  // formalParam ::= "(" [varDeclSequence] ")"
  //CToken dummy;
  //CAstModule *m = new CAstModule(dummy, "dummy fp");
  CToken t ; 

  Consume(tLBrak, &t); 
  if(_scanner->Peek().GetType() == tRBrak)
    {
      Consume (tRBrak, &t);
    }
  else
    {
      varDeclSequence(s);
      Consume (tRBrak, &t);
    }
}

CAstStatement* CParser::subroutineBody(CAstScope* s){
  //subroutineBody ::= varDeclaration "begin" statSequence "end" 
  CToken t ;
  // ###
  CAstStatement *statseq;  

  varDeclaration(s); 
  if (_scanner->Peek().GetType()  == tKeyword && !_scanner->Peek().GetValue().compare("begin")){
    Consume(tKeyword, &t);
    
    statseq = statSequence(s); 
    if (_scanner->Peek().GetType()  == tKeyword && !_scanner->Peek().GetValue().compare("end")){
      Consume(tKeyword, &t);
    }else{
         SetError(_scanner->Peek(), "keyword \"begin\" expected");
    }
  }else {
       SetError(_scanner->Peek(), "keyword \"end\" expected");
  }
  return statseq;
}
