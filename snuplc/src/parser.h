//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/09 Bernhard Egger adapted to SnuPL/!
/// 2016/04/08 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#ifndef __SnuPL_PARSER_H__
#define __SnuPL_PARSER_H__

#include "scanner.h"
#include "symtab.h"
#include "ast.h"


//------------------------------------------------------------------------------
/// @brief parser
///
/// parses a module
///
class CParser {
  public:
    /// @brief constructor
    ///
    /// @param scanner  CScanner from which the input stream is read
    CParser(CScanner *scanner);

    /// @brief parse a module
    /// @retval CAstNode program node
    CAstNode* Parse(void);

    /// @name error handling
    ///@{

    /// @brief indicates whether there was an error while parsing the source
    /// @retval true if the parser detected an error
    /// @retval false otherwise
    bool HasError(void) const { return _abort; };

    /// @brief returns the token that caused the error
    /// @retval CToken containing the error token
    const CToken* GetErrorToken(void) const;

    /// @brief returns a human-readable error message
    /// @retval error message
    string GetErrorMessage(void) const;
    ///@}

  private:
    /// @brief sets the token causing a parse error along with a message
    /// @param t token causing the error
    /// @param message human-readable error message
    void SetError(CToken t, const string message);

    /// @brief consume a token given type and optionally store the token
    /// @param type expected token type
    /// @param token If not null, the consumed token is stored in 'token'
    /// @retval true if a token has been consumed
    /// @retval false otherwise
    bool Consume(EToken type, CToken *token=NULL);


    /// @brief initialize symbol table @a s with predefined procedures and
    ///        global variables
    void InitSymbolTable(CSymtab *s);

    /// @name methods for recursive-descent parsing
	
    /// @{
	
	/// @brief  
	///
    /// @retval main node for the AST
    CAstModule*       module(void);

	/// @brief go through all the statements in the main part of module or in the procedure/function declaration 
    /// @param current scope  
    /// @retval list of statements 
    CAstStatement*    statSequence(CAstScope *s);

	/// @brief call functions for the lhs and rhs plus consume a tAssign
    /// @param current scope  
	/// @param type expected token type
    /// @retval CAstStatAssign node
    CAstStatAssign*   assignment(CAstScope *s, CToken t);

	/// @brief call function for the left simpleExp and eventually consume a RelOp and call an other function for the right simpleExp while it sees RelOp
    /// @param current scope
    /// @retval CAstExpression node 
    CAstExpression*   expression(CAstScope *s);
	
	/// @brief deal with +/- at the begining, then call term() and eventually consume TermOp and call term() again while it sees TermOp
    /// @param current scope
    /// @retval CAstExpression node
    CAstExpression*   simpleexpr(CAstScope *s);
	
	/// @brief call factor() and eventually consume factOp and call factor() again while it sees FactOp
    /// @param current scope
    /// @retval CAstExpression node
    CAstExpression*   term(CAstScope *s);
	
	/// @brief switch on the FIRST and then call the right function
    /// @param current scope
    /// @retval CAstExpression node
    CAstExpression*   factor(CAstScope *s);

	/// @brief check if it is valid and consume a tNumber
    /// 
    /// @retval CAstConstant node
    CAstConstant*     number(void);
	
	/// @brief consume a tChar 
    /// 
    /// @retval CAstConstant node
    CAstConstant*     character(void);
	
	/// @brief consume a boolean, store the value as an integer 0 false/ 1 true
    /// @param type expected token type
    /// @retval CAstConstant node
    CAstConstant*     boolean(void);
    
	/// @brief associate an ASCII value to an escape character 
    /// @param unescape value of a character 
    /// @retval ASCII value of the character
    int               escapechar2int(const string text);

	/// @brief check the existence of the token and deal with the expression
    /// @param current scope
    /// @param type expected token type
    /// @retval CAstDesignator node
    CAstDesignator*   qualident(CAstScope* s, CToken t);
    
	/// @brief check the existence of the token and deal with what is inside the brackets, empty or not 
    /// @param current scope
    /// @param type expected token type
    /// @retval CAstFunctionCall node
    CAstFunctionCall* subroutineCall(CAstScope* s, CToken t);
	
	/// @brief  check the existence of the token and deal with what is inside the brackets, empty or not
    /// @param current scope
	/// @param type expected token type
	/// @param integer made to 
    /// @retval CAstStatCall node
    CAstStatCall*     subroutineCall(CAstScope* s, CToken t, int dummy);
    
	/// @brief consume the keyword and call varDeclSequence()
    /// @param current scope
    void                varDeclaration(CAstScope* s);
	
	/// @brief can call varDecl several times
    /// @param current scope
    void                varDeclSequence(CAstScope* s);
	
	/// @brief add one or more var to the scope 
    /// @param current scope
    /// @retval 
    const CType*        varDecl(CAstScope* s);

	/// @brief check the global syntax of the declaration
    /// @param current scope
    /// @retval CAstScope node
    CAstScope*          subroutineDecl(CAstScope* s);
	
	/// @brief add a procedure to the scope 
    /// @param current scope
    /// @retval CAstScope node
    CAstScope*          procedureDecl(CAstScope* s);
	
	/// @brief add a function to the scope 
    /// @param current scope
    /// @retval CAstScope node
    CAstScope*          functionDecl(CAstScope* s);
	
	/// @brief deal with the declaration of variables as parameters of the procedure/function
    /// @param current scope
    void                formalParam(CAstScope* s);
	
	/// @brief deal with the declaration of new variables and the statSequence of the procedure/function
    /// @param current scope
    /// @retval CAstStatement node
    CAstStatement*      subroutineBody(CAstScope* s);
    /// @}


    CScanner     *_scanner;       ///< CScanner instance
    CAstModule   *_module;        ///< root node of the program
    CToken        _token;         ///< current token

    /// @name error handling
    CToken        _error_token;   ///< error token
    string        _message;       ///< error message
    bool          _abort;         ///< error flag

};

#endif // __SnuPL_PARSER_H__
