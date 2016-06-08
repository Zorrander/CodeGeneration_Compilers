//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL) {
  _id = _global_id++;
}

CAstNode::~CAstNode(void) {
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const {
  return _id;
}

CToken CAstNode::GetToken(void) const {
  return _token;
}

const CType* CAstNode::GetType(void) const {
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const {
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const {
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const {
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t) {
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t) {
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//

CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL) {
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void) {
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const {
  return _name;
}

CAstScope* CAstScope::GetParent(void) const {
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const {
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const {
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const {
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq) {
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const {
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const {
  bool result = true;

  try {
    CAstStatement *s = _statseq;
    while (result && (s != NULL)) {
      result = s->TypeCheck(t, msg);
      s = s->GetNext();
    }
    vector<CAstScope*>::const_iterator it = _children.begin();
    while (result && (it != _children.end())) {
      result = (*it)->TypeCheck(t, msg);
      it++;
    }
  } catch (...) {
    result = false;
  }

  return result;
}

ostream& CAstScope::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent + 4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent + 4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i = 0; i < _children.size(); i++) {
      _children[i]->print(out, indent + 4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb) {
  cout << "CAstScope::ToTac" << endl ;
  assert(cb != NULL);
    
  CAstStatement *s = GetStatementSequence();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    cb->AddInstr(next);
    s = s->GetNext();
  }
   
  //Commented for now to see what code is really generated
  cb->CleanupControlFlow();
    
  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const {
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st) {
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child) {
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//

CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL) {
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type) {
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const {
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//

CAstProcedure::CAstProcedure(CToken t, const string name,
			     CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol) {
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const {
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type) {
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const {
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const {
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//

CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type) {
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const {
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//

CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL) {
}

CAstStatement::~CAstStatement(void) {
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next) {
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const {
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next) {
  // generate code for statement (assignment, if-else, etc.)
  cout << "CAstStatement::ToTac" << endl ;
  //assignment
    
    
  // jump to next
  cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//

CAstStatAssign::CAstStatAssign(CToken t,
			       CAstExpression *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs) {
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstExpression* CAstStatAssign::GetLHS(void) const {
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const {
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const {
  bool result = true;

  //cout << "LH: " <<_lhs->GetType() << "RH: " << _rhs->GetType() << endl;
  if (_lhs->GetType() != _rhs->GetType()) {
    *t = GetToken();
    *msg = "type mismatch in assignment.";
    return false;
  }
  if (_lhs->GetType()->IsArray()) {
    *t = GetToken();
    *msg = "compound assigment not allowed.";
    return false;
  }

  if (result = GetLHS()->TypeCheck(t, msg)) {
    return GetRHS()->TypeCheck(t, msg);
  }

  return result;
}

const CType* CAstStatAssign::GetType(void) const {
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent + 2);
  _rhs->print(out, indent + 2);

  return out;
}

string CAstStatAssign::dotAttr(void) const {
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next) { 
  cout << "CAstStatAssign::ToTac" << endl ;
  CAstExpression *lhs, *rhs ;
  lhs = GetLHS();
  rhs = GetRHS();

  CTacAddr *lhsAddr, *rhsAddr;
  CTacInstr* assign;
  CTacLabel *labelTrue, *labelFalse;

  
  
  if (lhs->GetType() == CTypeManager::Get()->GetBool())
    {
      CTacTemp *temp = cb->CreateTemp(CTypeManager::Get()->GetBool());
      CTacLabel *assign_lab;
      
      labelTrue = cb->CreateLabel();
      labelFalse = cb->CreateLabel();
      assign_lab = cb->CreateLabel();
      //lhsAddr = lhs->ToTac(cb, labelTrue, labelFalse);
      lhsAddr = lhs->ToTac(cb);
      rhsAddr = rhs->ToTac(cb, labelTrue, labelFalse);
      
      cb->AddInstr(labelTrue);
      cb->AddInstr(new CTacInstr(opAssign, temp, new CTacConst(1)));
      cb->AddInstr(new CTacInstr(opGoto, assign_lab));
      cb->AddInstr(labelFalse);
      cb->AddInstr(new CTacInstr(opAssign, temp, new CTacConst(0)));
      
      cb->AddInstr(assign_lab);
      cb->AddInstr(new CTacInstr(opAssign, lhsAddr, temp));
    }
  else
    {
      lhsAddr = lhs->ToTac(cb) ;
      rhsAddr = rhs->ToTac(cb) ;
      assign = new CTacInstr(opAssign, lhsAddr, rhsAddr ) ;
      cb->AddInstr(assign);
    }
    
  return lhsAddr;
}


//------------------------------------------------------------------------------
// CAstStatCall
//

CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call) {
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const {
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const {
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const {
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const {
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const {
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const {
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next) {
  cout << "CAstStatCall::ToTac" << endl ;
  CTacInstr* statCall ;
  CTacConst* index ;
  CTacName* callName ;    
  CAstFunctionCall* call ;
    
  call = GetCall() ;
  callName = new CTacName(call->GetSymbol()) ; 
    
  int nbArg = call->GetNArgs() ;
  for (int i = nbArg-1; i >= 0 ; i--){
    index = new CTacConst( i );
    statCall =
      new CTacInstr(opParam, index, call->GetArg(i)->ToTac(cb) ); ;
    cb->AddInstr(statCall);  
  }
    
  if ( call->GetSymbol()->GetDataType() != CTypeManager::Get()->GetNull() ){
    CTacTemp* returnTemp = cb->CreateTemp(call->GetSymbol()->GetDataType());
        
    statCall =
      new CTacInstr(opCall, returnTemp, callName ); ;
    cb->AddInstr(statCall); 
  } else {
    statCall =
      new CTacInstr(opCall, NULL, callName ); ;
    cb->AddInstr(statCall);
  }
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//

CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr) {
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const {
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const {
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const {
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();

  if (st->Match(CTypeManager::Get()->GetNull())) {
    if (e != NULL) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "superfluous expression after return.";
      return false;
    }
  } else {
    if (e == NULL) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "expression expected after return.";
      return false;
    }
    if (!e->TypeCheck(t, msg)) return false;
    if (!st->Match(e->GetType())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "return type mismatch.";
      return false;
    }
  }
  return true;
}

const CType* CAstStatReturn::GetType(void) const {
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent + 2);

  return out;
}

string CAstStatReturn::dotAttr(void) const {
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next) {
  cout << "CAstStatReturn::ToTac" << endl ;
    
  CTacInstr* statReturn = new CTacInstr(opReturn, NULL, GetExpression()->ToTac(cb) ); 
  cb->AddInstr(statReturn); 
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//

CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
		       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody) {
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const {
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const {
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const {
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const {
  // ### Check if expression, if and else body has type mismatches.
  CAstExpression *e = GetCondition();

  if (e != NULL) {
    // Type check expression
    if (!e->TypeCheck(t, msg)) {
      return false;
    }
    // Type check that is is bool
    if (!e->GetType()->Match(CTypeManager::Get()->GetBool())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "expression is not boolean.";
      return false;
    }
  }

  // Check if if and else body is present and if they return valid statement sequences
  if (GetIfBody() != NULL) {
    if (!GetIfBody()->TypeCheck(t, msg)) {
      return false;
    }
  }

  if (GetElseBody() != NULL) {
    if (!GetElseBody()->TypeCheck(t, msg)) {
      return false;
    }
  }

  return true;
}

ostream& CAstStatIf::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const {
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
	s->toDot(out, indent);
	out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
	    << endl;
	prev = s->dotID();
	s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
	s->toDot(out, indent);
	out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
	    << endl;
	prev = s->dotID();
	s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next) {
  cout << "CAstStatIf::ToTac" << endl ;
      
  CTacLabel *statIfFalseLabel, *statIfTrueLabel ;
  CAstExpression* ifCond ;
  CTacAddr* ifCondTac, *bodyTac, *elseTac ;
    
  ifCond = GetCondition() ;
  statIfTrueLabel = cb->CreateLabel("if_true");
  statIfFalseLabel = cb->CreateLabel("if_false");
    
  ifCondTac = ifCond->ToTac(cb, statIfTrueLabel, statIfFalseLabel) ;
    
  //branch to the body depending on true or false before
    
  cb->AddInstr(statIfTrueLabel); 

  CAstStatement *s = GetIfBody();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    s = s->GetNext();
  }

  cb->AddInstr(new CTacInstr(opGoto, next));
    
  cb->AddInstr(statIfFalseLabel); 
  s = GetElseBody();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    s = s->GetNext();
  }
    
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//

CAstStatWhile::CAstStatWhile(CToken t,
			     CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body) {
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const {
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const {
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const {

  // ### Check while expression and body for type mismatch
  CAstExpression *e = GetCondition();

  if (e != NULL) {
    // Type check expression
    if (!e->TypeCheck(t, msg)) {
      return false;
    }
    // Type check that is is bool
    if (!e->GetType()->Match(CTypeManager::Get()->GetBool())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "expression is not boolean.";
      return false;
    }
  }

  // Check if while body is present and if it returns valid statement sequences
  if (GetBody() != NULL) {
    if (!GetBody()->TypeCheck(t, msg)) {
      return false;
    }
  }

  return true;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent + 2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent + 2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const {
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
	s->toDot(out, indent);
	out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
	    << endl;
	prev = s->dotID();
	s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next) {
  cout << "CAstStatWhile::ToTac" << endl ;
  CTacLabel *statIfFalseLabel, *statIfTrueLabel, *whileCond, *whileBody ;
    
  whileCond = new CTacLabel("1_while_cond") ; 
  whileBody = new CTacLabel("2_while_body") ;
        
  cb->AddInstr(whileCond); 
  GetCondition()->ToTac(cb, whileBody, next) ;
    
  cb->AddInstr(whileBody);
    
  CAstStatement *s = GetBody();
  while (s != NULL) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    s = s->GetNext();
  }

  cb->AddInstr(new CTacInstr(opGoto, whileCond )); 
    
  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//

CAstExpression::CAstExpression(CToken t)
  : CAstNode(t) {
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb) {
  cout << "CAstExpression::ToTac" << endl ;
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
				CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstExpression2::ToTac" << endl ;
    
  return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//

CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper) {
}

EOperation CAstOperation::GetOperation(void) const {
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//

CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
			   CAstExpression *l, CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r) {
  // these are the only binary operation we support for now
  assert((oper == opAdd) || (oper == opSub) ||
	 (oper == opMul) || (oper == opDiv) ||
	 (oper == opAnd) || (oper == opOr) ||
	 (oper == opEqual) || (oper == opNotEqual) ||
	 (oper == opLessThan) || (oper == opLessEqual) ||
	 (oper == opBiggerThan) || (oper == opBiggerEqual)
	 );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const {
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const {
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const {
  // ### Maybe not most optimal solution
  bool result = true;

  if (GetLeft()->GetType() != GetRight()->GetType()) {
    *t = GetToken();
    *msg = "type mismatch between lhs and rhs.";
    return false;
  }

  if (GetLeft()->GetType() == CTypeManager::Get()->GetInt()) {
    // ### Any operator ok
  }

  if (GetLeft()->GetType() == CTypeManager::Get()->GetBool()) {
    // ### only opAnd:
    //          opOr:
    //          opEqual:
    //          opNotEqual:
    // are ok.
    if (!(GetOperation() == opAnd ||
	  GetOperation() == opOr ||
	  GetOperation() == opEqual ||
	  GetOperation() == opNotEqual)) {
      *t = GetToken();
      *msg = "type mismatch between operand and operators.";
      return false;
    }
  }

  if (GetLeft()->GetType() == CTypeManager::Get()->GetChar()) {
    if (!(GetOperation() != opEqual || GetOperation() != opNotEqual)) {
      *t = GetToken();
      *msg = "type mismatch between operand and operators.";
      return false;
    }
  }

  // ### Do we need the left one?
  if (result = GetLeft()->TypeCheck(t, msg)) {
    result = GetRight()->TypeCheck(t, msg);
  }

  return result;
}

const CType* CAstBinaryOp::GetType(void) const {
  switch (GetOperation()) {
  case opAdd:
  case opMul:
  case opSub:
  case opDiv:
    return CTypeManager::Get()->GetInt();
    // ### Only these four should work with bools
  case opAnd:
  case opOr:
  case opEqual:
  case opNotEqual:
    // ### Last four should not work with bools.
  case opLessThan:
  case opLessEqual:
  case opBiggerThan:
  case opBiggerEqual:
    return CTypeManager::Get()->GetBool();
  default:
    return NULL;
  }
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  _left->print(out, indent + 2);
  _right->print(out, indent + 2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb) {
  cout << "CAstBinaryOp::ToTac" << endl ;   
  CTacTemp * storage ; 
  CTacAddr* lhs, *rhs ; 
    
  cout << "Op: " << GetOperation() << endl;

  lhs = GetLeft()->ToTac(cb) ;   
  rhs = GetRight()->ToTac(cb) ;    

  storage = cb->CreateTemp(GetType());
  
  CTacInstr *ope = new CTacInstr(GetOperation(), storage, lhs, rhs ) ;    
  
  cb->AddInstr(ope);
            
  return storage;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb,
			      CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstBinaryOp::ToTac2" << endl ;
   
  CTacAddr *lhs, *rhs;
  CTacLabel *next_true, *next_false;

  // GetCondition()->ToTac(cb, whileBody, next);

  CTacInstr *trueOpe;
  CTacInstr *elseOpe;

  CTacLabel *atrue; // = cb->CreateLabel("andTrue");
  CTacLabel *afalse; // = cb->CreateLabel("andTrue");
  CTacLabel *otrue; // = cb->CreateLabel("orTrue");
  CTacLabel *ofalse; // = cb->CreateLabel("orTrue");

  cout << "Operation: " << GetOperation() << endl;
  if (GetOperation() == opAnd)
    {
      atrue = cb->CreateLabel();
      lhs = GetLeft()->ToTac(cb, atrue, lfalse);
      cb->AddInstr(atrue);

      rhs = GetRight()->ToTac(cb, ltrue, lfalse);
    }
  else if (GetOperation() == opOr)
    {
      ofalse = cb->CreateLabel();
      lhs = GetLeft()->ToTac(cb, ltrue, ofalse);
      cb->AddInstr(ofalse);

      rhs = GetRight()->ToTac(cb, ltrue, lfalse);
    }
  else
    {
      lhs = GetLeft()->ToTac(cb, ltrue, lfalse);
      rhs = GetRight()->ToTac(cb, ltrue, lfalse);
      
      if (lhs != NULL)
	{
	  trueOpe = new CTacInstr(GetOperation(), ltrue, lhs, rhs);
	  cb->AddInstr(trueOpe);
	}
      
      elseOpe = new CTacInstr(opGoto, lfalse);
      cb->AddInstr(elseOpe);
    }

  return lhs;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//

CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e) {
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const {
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const {
  bool result = true;

  if (GetType() != GetOperand()->GetType()) {
    *t = GetToken();
    *msg = "type mismatch in unary operand.";
    result = false;
  }

  return result;
}

const CType* CAstUnaryOp::GetType(void) const {
  // ### Can it not be both bool and int?

  switch (GetOperation()) {
  case opNeg:
  case opPos:
    return CTypeManager::Get()->GetInt();
  case opNot:
    return CTypeManager::Get()->GetBool();
  default:
    return NULL;
  }
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb) {
  cout << "CAstUnaryOp::ToTac" << endl ;
  return NULL;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
			     CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstUnaryOp::ToTac2" << endl ;
  return NULL;
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//

CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
			     const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type) {
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
	 ((oper == opCast) && (type != NULL)));
}

CAstExpression* CAstSpecialOp::GetOperand(void) const {
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const {
  return GetOperand()->TypeCheck(t, msg);
}

const CType* CAstSpecialOp::GetType(void) const {
  if (GetOperation() == opAddress) {
    return CTypeManager::Get()->GetPointer(GetOperand()->GetType());
  } else {
    return NULL;
  }
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent + 2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb) {
  cout << "CAstSpecialOp::ToTac" << endl ;
  
  CTacTemp * temp = cb->CreateTemp(GetType());
  CTacInstr* statCall;
  
  statCall = new CTacInstr(opAddress, temp, GetOperand()->ToTac(cb) );
  cb->AddInstr(statCall);

  return temp;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//

CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol) {
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const {
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg) {
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const {
  return (int) _arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const {
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const {
  int N_call = GetNArgs();
  CAstExpression** expr = new CAstExpression*[N_call];

  // Parameters from the call
  for (int i = 0; i < N_call; i++) {
    expr[i] = GetArg(i);
  }

  const CSymProc* sp = GetSymbol();

  int N_decl = sp->GetNParams();
  const CSymParam** parm = new const CSymParam*[N_decl];

  // Parameters from the declaration
  for (int i = 0; i < N_decl; i++) {
    parm[i] = sp->GetParam(i);
  }


  if (N_call != N_decl) {
    if (t != NULL) {
      *t = GetToken();
    }
    if (msg != NULL) {
      *msg = "wrong number of parameters in function/procedure call.";
    }
    return false;
  }

  for (int i = 0; i < N_call; i++) {
    if (parm[i]->GetDataType()->Match(expr[i]->GetType()) == false) {

      if (t != NULL) {
	*t = expr[i]->GetToken();
      }
      if (msg != NULL) {
	*msg = "type mismatch in function/procedure parameters.";
      }
      return false;
    }
  }

  delete [] expr;
  delete [] parm;

  return true;
}

const CType* CAstFunctionCall::GetType(void) const {
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";
  out << endl;

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->print(out, indent + 2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb) {
  cout << "CAstFunctionCall::ToTac" << endl ;
  return NULL;
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
				  CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAsFunctionCall::ToTac2" << endl ;
  return NULL;
}



//------------------------------------------------------------------------------
// CAstOperand
//

CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t) {
}


//------------------------------------------------------------------------------
// CAstDesignator
//

CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol) {
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const {
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const {
  // ### ToDo
  //cout << "todo castDesignator" << endl;

  return true;
}

const CType* CAstDesignator::GetType(void) const {
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb) {
  cout << "CAstDesignator::ToTac" << endl ;
  CTacName* qualident = new CTacName( GetSymbol() );
  return qualident;
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
				CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstDesignator::ToTac2" << endl ;

  const CSymbol *sym = GetSymbol();
  if (sym->GetDataType() == CTypeManager::Get()->GetBool())
    {
      CTacInstr *inst = new CTacInstr(opEqual, ltrue, new CTacName(GetSymbol()), new CTacConst(1));
      cb->AddInstr(inst);
      cb->AddInstr(new CTacInstr(opGoto, lfalse));
      return NULL;
    }

  return new CTacName(GetSymbol());
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//

CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false), _offset(NULL) {
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx) {
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void) {
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const {
  return (int) _idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const {
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const {
  bool result = true;

  // ### What does this one do?
  assert(_done);

  for (int i = 0; i < GetNIndices(); i++) {
    if (GetIndex(i)->TypeCheck(t, msg) && GetIndex(i)->GetType() == CTypeManager::Get()->GetInt()) {
    } else
      return false;
  }

  return result;
}

const CType* CAstArrayDesignator::GetType(void) const {

  // ### Attempting to fix return type of CAstArrayDesignator (working?)
  const CType* test;
  const CArrayType* a;

  const CSymbol* s = GetSymbol();
  test = s->GetDataType();

  int N = GetNIndices();

  if (test->IsPointer())
    test = test->GetBaseType();

  while (N > 0) {
    if (test->IsArray()) {
      a = dynamic_cast<const CArrayType*> (test);
      test = a->GetInnerType();
    } else {

    }
    N--;
  }

  return test;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->print(out, indent + 2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const {
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i = 0; i < _idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb) {
  cout << "CAstArrayDesignator::ToTac" << endl ;
  CTacTemp * storage0, *storage1, *storage2, *storage3,*storage4,
    *storage5, *storage6, *storage7, *storage8, *storage9 ;
  const CSymbol* s, *dimSymbol ;
  const CSymtab* t ;
  CTacName* address ;
     
  s = GetSymbol() ;
  t = s->GetSymbolTable() ;     
  dimSymbol = t->FindSymbol("DIM") ;
  /**
   *First get the address of A 
   */

  address = new CTacName(GetSymbol()) ;
  if ( s->GetDataType()->IsArray() ) {
    storage0 = cb->CreateTemp(GetType()) ;
    cb->AddInstr(new CTacInstr(opAddress, storage0, address)) ;  
  }    
  else
    {
      
    }
  /**
   * Get the size of the i dimension
   * can be queried by DIM(A, i) 
   */
  if (GetNIndices() == 1){
      
  }  else {
  for (int i = 2; i <= GetNIndices(); i++) {
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(1), new CTacConst(i))) ;
     
    if ( s->GetDataType()->IsArray() ) {
      storage1 = cb->CreateTemp(GetType()) ;
      cb->AddInstr(new CTacInstr(opAddress, storage1, address)) ; 
      cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), storage1)) ;
    }
    else
      {
	cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), address)) ;
      }
        
    storage2 = cb->CreateTemp(GetType()) ;
    cb->AddInstr(new CTacInstr(opCall, storage2, new CTacName(dimSymbol) )) ;
         
    storage3 = cb->CreateTemp(GetType()) ;
    if (i == 2 ){ //  multiply by first array index expression (1)
      cb->AddInstr(new CTacInstr(opMul, storage3, GetIndex(GetNIndices()-1)->ToTac(cb), storage2)) ;    
    }else { // need to multiply the last result
      cb->AddInstr(new CTacInstr(opMul, storage3, storage4, storage2)) ; 
    }
    // add second array index expression
    storage4 = cb->CreateTemp(GetType()) ;
    cb->AddInstr(new CTacInstr(opAdd, storage4, storage3, GetIndex(GetNIndices()-i)->ToTac(cb) )) ;    
              
  }}
  // multiply by array element size
  storage5 = cb->CreateTemp(GetType()) ;
  if (GetNIndices() == 1 ){  
  cb->AddInstr(new CTacInstr(opMul, storage5, GetIndex(0)->ToTac(cb), new CTacConst(4) )) ;  
  } else {
  cb->AddInstr(new CTacInstr(opMul, storage5, storage4, new CTacConst(4) )) ; 
  }
  //get offset of data
  if ( s->GetDataType()->IsArray() )
    {
      storage6 = cb->CreateTemp(GetType()) ;
      cb->AddInstr(new CTacInstr(opAddress, storage6, address)) ;
      cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), storage6)) ;  
    }
  else
    {
      cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), address)) ;
    }
  
  // call t7 <- DOFS # call DOFS(A)
  const CSymbol* dofsSymbol = t->FindSymbol("DOFS") ;
  storage7 = cb->CreateTemp(GetType()) ;
  cb->AddInstr(new CTacInstr(opCall, storage7, new CTacName(dofsSymbol) )) ;
  // add t8 <- t5, t7 # add element offset to data offset
  storage8 = cb->CreateTemp(GetType()) ;
  cb->AddInstr(new CTacInstr(opAdd, storage8, storage5, storage7 )) ;
  // add t9 <- t0, t8 # add address of A
  if ( s->GetDataType()->IsArray() ) {
    storage9 = cb->CreateTemp(GetType()) ;
    cb->AddInstr(new CTacInstr(opAdd, storage9, storage0, storage8 )) ;   
  }
  else
    {
      storage9 = cb->CreateTemp(GetType()) ;
      cb->AddInstr(new CTacInstr(opAdd, storage9, address, storage8 )) ;
    }
       
     
  return new CTacReference(storage9->GetSymbol(), storage9->GetSymbol());
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
				     CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstArrayDesignator::ToTac2" << endl ;
  return NULL;
}


//------------------------------------------------------------------------------
// CAstConstant
//

CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value) {
}

void CAstConstant::SetValue(long long value) {
  _value = value;
}

long long CAstConstant::GetValue(void) const {
  return _value;
}

string CAstConstant::GetValueStr(void) const {
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const {
  return true;
}

const CType* CAstConstant::GetType(void) const {
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const {
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const {
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb) {
  cout << "CAstConstant::ToTac" << endl ;
 
  CTacConst* constant = new CTacConst( (int) GetValue() );    
  cout << "constant : " << constant << endl ;
  return constant;  
    
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
			      CTacLabel *ltrue, CTacLabel *lfalse) {
  cout << "CAstConstant::ToTac2" << endl ;

  if (GetType() == CTypeManager::Get()->GetBool())
    {
      if (GetValue() == (long long int)(0))
	{
	  cb->AddInstr(new CTacInstr(opGoto, lfalse));
	}
      
      return NULL;
    }

  
  return new CTacConst(GetValue());
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
				       CAstScope *s)
  : CAstOperand(t) {
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(strlen(CToken::unescape(value).c_str()) + 1,
		       tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const {
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const {
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const {
  return true;
}

const CType* CAstStringConstant::GetType(void) const {

  //return CTypeManager::Get()->GetPointer(_type);
  return _type;
  //return NULL;
}

ostream& CAstStringConstant::print(ostream &out, int indent) const {
  string ind(indent, ' ');
  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t;
  else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const {
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb) {
  cout << "CAstStringConstant::ToTac" << endl ;
  return NULL;
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
				    CTacLabel *ltrue, CTacLabel *lfalse) {
  return NULL;
}


