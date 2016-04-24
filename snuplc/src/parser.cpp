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
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
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
}

/*
CAstModule* CParser::module(void)
{
  //
  // module ::= statSequence  ".".
  //
  CToken dummy;
  CAstModule *m = new CAstModule(dummy, "placeholder");
  CAstStatement *statseq = NULL;

  statseq = statSequence(m);
  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}
*/

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident ".".
  //
  CToken dummy;
  CAstModule *m = new CAstModule(dummy, "placeholder");
  CAstStatement *statseq = NULL;
  
  CToken t;
  CToken moduleName;

  if (_scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("module")){
    Consume(tKeyword, &t);

    if(_scanner->Peek().GetType() == tIdent){
      Consume(tIdent, &moduleName);

      if(_scanner->Peek().GetType() == tSemicolon){
	Consume(tSemicolon, &t) ;      

	// varDeclaration

	//varDeclaration(s) ; 
	varDeclaration() ;
	
	//Optional subroutineDecl
	string str = _scanner->Peek().GetValue();
	while (!str.compare("procedure") || !str.compare("function"))
	  {
	    //subRoutineDecl(s) ;
	    str = _scanner->Peek().GetValue();
	  }
	//"begin"
	if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("begin") ){
	  
	 Consume(tKeyword, &t) ; 
	 //statSequence

         statseq = statSequence(m);
               
         m->SetStatementSequence(statseq);

	 //"end" ident "."
	 if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("end") ){
	   Consume(tKeyword, &t) ;
	   Consume(tIdent, &t) ;
	   if (moduleName.GetValue().compare(t.GetValue()))
	     {
	       SetError(_scanner->Peek(), "module declaration and end did not match.");
	     }

	   Consume(tDot, &t) ; 
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
      SetError(_scanner->Peek(), "first word should be \"module\"");
  }

  //maybe do verifications before returning m in case it's not assigned 
  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment.
  // FIRST(statSequence) = { tIdent, "if", "while", "return" }
  // FOLLOW(statSequence) = { "else", "end", tDot }
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
      noColon = false;
      
      switch (tt) {
        
      case tIdent:
	Consume(tIdent, &t);
	// statement ::= subroutineCall
	if (_scanner->Peek().GetType() == tLBrak)
	  {
	    Consume(tLBrak, &t);
	    // ### Combine tIdent and expession to CAstSubroutine (?) ### //
	    if (_scanner->Peek().GetType() == tRBrak)
	      {
		Consume(tRBrak, &t);
		//st = new CAstStatCall(); ###
	      }
	    else
	      {
		expression(s);
		while (_scanner->Peek().GetType() == tComma)
		  {
		    Consume(tComma, &t);
		    expression(s);
		  }
		//st = new CAstStatCall(); ###
		Consume(tRBrak, &t);
	      }
	  }
	// statement ::= qualident
	else
	  {
	    cout << "Assignment" << endl;
	    st = assignment(s, t);
	  }
	break;
        
      case tKeyword:
	// statement ::= ifStatement
	if (!_scanner->Peek().GetValue().compare("if")) 
	  {  
	    noColon = true;
	    Consume(tKeyword, &t);
	    Consume(tLBrak, &t);
	    expression(s);
	    Consume(tRBrak, &t);
	    
	    Consume(tKeyword, &t);
	    if (t.GetValue().compare("then"))
	      {
		SetError(_scanner->Peek(), "Keyword \"then\" expected");
		break;
	      }

	    statSequence(s);
	    if (!_scanner->Peek().GetValue().compare("else"))
		{
		  Consume(tKeyword, &t);
		  statSequence(s);
		}
	    if (!_scanner->Peek().GetValue().compare("end"))
	      {
		Consume(tKeyword, &t);
		break;
	      }
	    
	    SetError(_scanner->Peek(), "expected \'end\'");
	    break;
	  }
	
	// statement ::= whileStatement
	else if (!_scanner->Peek().GetValue().compare("while")) 
	  {  
	    noColon = true;
	    Consume(tKeyword, &t);
	    Consume(tLBrak, &t);
	    expression(s);
	    Consume(tRBrak, &t);

	    if (_scanner->Peek().GetValue().compare("do"))
	      {
		SetError(_scanner->Peek(), "expected \'do\'");
		break;
	      }

	    Consume(tKeyword, &t);
	    statSequence(s);
	    if (_scanner->Peek().GetValue().compare("end"))
	      {
		SetError(_scanner->Peek(), "expected \'do\'");
		break;
	      }

	    Consume(tKeyword, &t);
	    break;
	  }
	// statement ::= returnStatement
	else if (!_scanner->Peek().GetValue().compare("return")) 
	  {  
	    Consume(tKeyword, &t);
	    if (_scanner->Peek().GetType() != tSemicolon)
	      {
		expression(s);
	      }
	    
	    break;
	  }
	
	// ### else if peek == end return, since end is in follow of statSequence. ###
	else if(!_scanner->Peek().GetValue().compare("else"))
	  {
	    isFollow = true;
	    break;
	    //return head; // ### Don't know what is correct to return here ###
	  }
	else if (!_scanner->Peek().GetValue().compare("end"))
	  {
	    isFollow = true;
	    break;
	    //return head; // ### Don't know what is correct to return here ###
	  }

	else { SetError(_scanner->Peek(), "Got \'" + t.GetValue() + "\', expected keyword { if | while | return | else | end } expected." ); }
	break;
	
      default:
	SetError(_scanner->Peek(), "statement expected.");
	break;
      }

      //assert(st != NULL); // ### Need to work
      if (head == NULL) head = st;
      //else tail->SetNext(st); // ### Need to work
      tail = st;

      

      tt = _scanner->Peek().GetType();
      if (tt == tDot) break;

      cout << "noColon = " << noColon << endl;

      if (isFollow) { break; }
      else if (noColon) {  }
      else { Consume(tSemicolon); }
    } while (!_abort);
  }
  cout << "return head " << endl;
  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s, CToken t)
{
  //
  // assignment ::= qualident ":=" expression.
  //
  
  CAstStringConstant *lhs = qualident(s, t);

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

  CAstExpression *n = NULL;

  // Check for ["+"|"-"]

  n = term(s);

  while (_scanner->Peek().GetType() == tTermOp) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tTermOp, &t);

    r = term(s);

    n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tFactOp)) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tFactOp, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

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
    // factor ::= qualident
  case tIdent:
    Consume(tIdent, &t);
    n = qualident(s, t);
    break;
    // factor ::= number
  case tNumber:
    n = number();
    break;
    // factor ::= bool
  case tKeyword:
    if (!str.compare("true"))
      {
	Consume(tKeyword, &t); // ### Need return
      }
    else if (!str.compare("false"))
      {	
	Consume(tKeyword, &t); // ### Need return
      }
    else
      {
	SetError(_scanner->Peek(), "expected 'true/false'");
      }
    break;
    // factor ::= "(" expression ")"
  case tLBrak:
    Consume(tLBrak);
    n = expression(s);
    Consume(tRBrak);
    break;

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
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstStringConstant* CParser::qualident(CAstScope* s, CToken t)
{
  //
  // qualident ::= ident { "[" expression "]" }
  // ident ::= tIdent
  // expression ::= simpleexpr [ relOp simplexpr ].
  // FIRST(qualident) = { tIdent }
  // FOLLOW(qualident) = { tAssign | tDot }
  
  // tIdent will already be comsumed before this function starts (?)
  // Have to check for { "[" expression "]" }

  
  // while peek == "["
  while (_scanner->Peek().GetType() == tLSqBrak)
    {
  // Consume "["
      Consume(tLSqBrak, &t);
  // "expression"
      expression(s);
  // Consume "]"
      Consume(tRSqBrak, &t);
  // end while
    }

  return new CAstStringConstant(t, "hej", s); /* ### FIX RETURN ### */
}

void CParser::varDeclaration(){

  CToken t ;
 
  //varDeclaration ::= "var" varDeclSequence ";"
  if( _scanner->Peek().GetType() == tKeyword && !_scanner->Peek().GetValue().compare("var")){
    Consume(tKeyword, &t);
    varDeclSequence(); 
  }
  else {
    SetError(_scanner->Peek(), "var declaration expected. It should start with \"var\"");
  }
}

void CParser::varDeclSequence(){
  CToken t ;

  //varDeclSequence ::= varDecl { ";" varDecl }
  varDecl() ;
  Consume(tSemicolon, &t);
  string str = _scanner->Peek().GetValue();
  while (str.compare("procedure") || str.compare("function")){
    
    varDecl() ; 
    
    Consume(tSemicolon, &t);
    str = _scanner->Peek().GetValue();
    cout << _scanner->Peek().GetValue() << endl;
  }
  cout << "hej" << endl;
}

void CParser::varDecl(){

  EToken nextToken ;
  CToken t;
  /*
    varDecl ::= ident { "," ident } ":" type 
 
    type = basetype | type "[" [number] "]"
  */
  Consume(tIdent, &t) ;
  while (_scanner->Peek().GetType() == tComma){
    Consume(tComma, &t) ; 
    Consume(tIdent, &t) ; 
  }

  Consume(tColon, &t) ;
  
  nextToken = _scanner->Peek().GetType() ;

  switch(nextToken){

    // type ::= basetype
  case tKeyword :
    if(!_scanner->Peek().GetValue().compare("integer") || !_scanner->Peek().GetValue().compare("boolean") || !_scanner->Peek().GetValue().compare("char")){
      Consume(tKeyword, &t);
    }
    else {
      SetError(_scanner->Peek(), "not a basetype");
    }
    break ;

    // type ::= type "[" [number] "]"
  case tString :
 
    if(!_scanner->Peek().GetValue().compare("type")){
      Consume(tString, &t) ;
      Consume(tLSqBrak, &t);
      Consume(tNumber, &t) ;
      Consume(tRSqBrak, &t);      
    }
    break ;
  }
}
