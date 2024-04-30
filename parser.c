#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include "ad.h"
#include "parser.h"

Token *iTk;		   // the iterator in the tokens list
Token *consumedTk; // the last consumed token
void tkerr(const char *fmt, ...);
bool consume(int code);
bool typeBase(Type *t);
bool unit();
bool structDef();
bool varDef();
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
bool expr();
bool exprAssign();
bool exprOr();
bool exprOrPrim();
bool exprAnd();
bool exprAndPrim();
bool exprEq();
bool exprEqPrim();
bool exprRel();
bool exprRelPrim();
bool exprAdd();
bool exprAddPrim();
bool exprMul();
bool exprMulPrim();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();
void parse(Token *tokens);

void tkerr(const char *fmt, ...)
{
	fprintf(stderr, "error in line %d: ", iTk->line);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}

bool consume(int code)
{
	if (iTk->code == code)
	{
		consumedTk = iTk;
		iTk = iTk->next;
		return true;
	}
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t)
{
	t->n = -1;
	Token *start = iTk;
	if (consume(TYPE_INT))
	{
		t->tb = TB_INT;
		return true;
	}
	if (consume(TYPE_DOUBLE))
	{
		t->tb = TB_DOUBLE;
		return true;
	}
	if (consume(TYPE_CHAR))
	{
		t->tb = TB_CHAR;
		return true;
	}
	if (consume(STRUCT))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if (!t->s)
				tkerr("undefined structure: %s", tkName->text);
			return true;
		}
	}
	iTk = start;
	return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit()
{
	for (;;)
	{
		if (structDef())
		{
		}
		else if (fnDef())
		{
		}
		else if (varDef())
		{
		}
		else
			break;
	}
	if (consume(END))
	{
		return true;
	}
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
//* inseamna while
bool structDef()
{
	Token *start = iTk; // stocam pozitia curenta a iTk in t pt a putea reveni la ea in caz ca nu e valid ce se intampla
	if (consume(STRUCT))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (consume(LACC))
			{
				Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if (s)
					tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;
				for (;;)
				{
					if (varDef())
					{
					}
					else
						break;
				}
				if (consume(RACC))
				{
					if (consume(SEMICOLON))
						owner = NULL;
					dropDomain();
					return true;
					else tkerr("Missing ; at structure declaration");
				}
				else
					tkerr("Missing } from structure declaration");
			}
		}
		else
			tkerr("Missing structure name");
	}
	iTk = start;  // revine la pozitia initiala a iTk
	return false; // nu e valid ce s-a intamplat
}

// varDef: typeBase ID arrayDecl? SEMICOLON
// ? inseamna ca nu vom avea nimic si codul continua
bool varDef()
{
	Token *start = iTk;
	Type t;
	if (typeBase(&t))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (arrayDecl(&t))
				if (t.n == 0)
					tkerr("a vector variable must have a specified dimension");

			if (consume(SEMICOLON))
			{
				Symbol *var = findSymbolInDomain(symTable, tkName->text);

				if (var)
					tkerr("symbol redefinition: %s", tkName->text);
				var = newSymbol(tkName->text, SK_VAR);

				var->type = t;
				var->owner = owner;
				addSymbolToDomain(symTable, var);
				if (owner)
				{
					switch (owner->kind)
					{
					case SK_FN:
						var->varIdx = symbolsLen(owner->fn.locals);
						addSymbolToList(&owner->fn.locals, dupSymbol(var));
						break;
					case SK_STRUCT:
						var->varIdx = typeSize(&owner->type);
						addSymbolToList(&owner->structMembers, dupSymbol(var));
						break;
					default:
						break;
					}
				}
				else
				{
					var->varMem = safeAlloc(typeSize(&t));
				}
				return true;
			}
			else
				tkerr("Missing ; at the variable declaration");
		}
		else
			tkerr("Missing variable name");
	}
	iTk = start;
	return false;
}

bool arrayDecl()
{
	Token *start = iTk;
	if (consume(LBRACKET))
	{
		if (consume(INT))
		{
			Token *tkSize = consumedTk;
			t->n = tkSize->i;
		}
		else
		{
			t->n = 0;
		}
		if (consume(RBRACKET))
			return true;
		else
			tkerr("Missing ] at array declaration");
	}
	iTk = start;
	return false;
}

// declarare functie cu parametri si bloc de instructiuni in interior
bool fnDef()
{
	Type t;
	Token *start = iTk;
	if (typeBase(&t) || consume(VOID))
	{
		if (consumedTk->code == VOID)
		{
			t.tb = TB_VOID;
		}

		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (consume(LPAR))
			{
				Symbol *fn = findSymbolInDomain(symTable, tkName->text);
				if (fn)
					tkerr("symbol redefinition: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);

				fn->type = t;
				addSymbolToDomain(symTable, fn);
				owner = fn;
				pushDomain();
				if (fnParam())
				{
					for (;;)
					{
						if (consume(COMMA))
						{
							if (fnParam())
							{
							}
							else
							{
								iTk = start;
								tkerr("Error at function parameters declaration");
								return false;
							}
						}
						else if (fnParam())
						{
							tkerr("missing ,");
						}
						else
							break;
					}
				}
				if (consume(RPAR))
				{
					if (stmCompound(false))
					{
						dropDomain();
						owner = NULL;
						return true;
					}
				}
				else
					tkerr("Missing ) from function definition");
			}
		}
	}

	iTk = start;
	return false;
}

// fnParam: typeBase ID arrayDecl?
// declarare functie cu parametri
bool fnParam()
{
	Token *start = iTk;
	Type t;
	if (typeBase(&t))
	{
		if (consume(ID))
		{
			Token *tkName = consumedTk;
			if (arrayDecl(&t))
			{
				t.n = 0;
			}

			Symbol *param = findSymbolInDomain(symTable, tkName->text);
			if (param)
				tkerr("symbol redefinition: %s", tkName->text);
			param = newSymbol(tkName->text, SK_PARAM);

			param->type = t;
			param->owner = owner;
			param->paramIdx = symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		}
		else
			tkerr("Missing ID from function parameters");
	}
	iTk = start;
	return false;
}

/*stm: stmCompound
| IF LPAR expr RPAR stm ( ELSE stm )?
| WHILE LPAR expr RPAR stm
| RETURN expr? SEMICOLON
| expr? SEMICOLON
*/

// interiorul unei functii
bool stm()
{
	Token *start = iTk;
	if (stmCompound(true))
	{
		return true;
	}
	if (consume(IF))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (stm())
					{
						if (consume(ELSE))
						{
							if (stm())
							{
							}
						}
						return true;
					}
				}
				else
					tkerr("Missing ) at if loop");
			}
			else
				tkerr("Invalid expression at if loop");
		}
		else
			tkerr("Mission ( at if loop");
	}
	if (consume(WHILE))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (stm())
						return true;
				}
				else
					tkerr("Missing ) at while loop");
			}
			else
				tkerr("Invalid expression at while loop");
		}
		else
			tkerr("Missing ( at while loop");
	}
	if (consume(RETURN))
	{
		if (expr())
		{
		}

		if (consume(SEMICOLON))
			return true;
		else
			tkerr("Missing ; from the return statement");
	}

	if (expr())
	{
	}

	if (consume(SEMICOLON))
		return true;

	iTk = start;
	return false;
}

// acoladele {interior functie}
bool stmCompound()
{
	Token *start = iTk;
	if (consume(LACC))
	{
		if (newDomain)
		{
			pushDomain();
		}

		for (;;)
		{
			if (varDef())
			{
			}
			else if (stm())
			{
			}
			else
				break;
		}
		if (consume(RACC))
		{
			if (newDomain)
			{
				dropDomain();
			}
			return true;
		}
		else
			tkerr("Missing } at the end of the function expression");
	}
	iTk = start;
	return false;
}

// expr: exprAssign deci atribuire
bool expr()
{
	Token *start = iTk;

	if (exprAssign())
		return true;

	iTk = start;
	return false;
}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr
// x = y || z
bool exprAssign()
{
	Token *start = iTk;
	if (exprUnary())
	{
		if (consume(ASSIGN))
		{
			if (exprAssign())
				return true;
			else
				tkerr("Invalid expression for the assign operation");
		}
	}

	iTk = start;

	if (exprOr())
		return true;

	iTk = start;
	return false;
}

// ||
bool exprOr()
{
	Token *start = iTk;
	if (exprAnd())
	{
		if (exprOrPrim())
			return true;
		else
			tkerr("Invalid expression for the and expression");
	}

	iTk = start;
	return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
//
// exprOr: exprAnd exprOrPrim
// exprOrPrim: OR exprAnd exprOrPrim | epsilon
// merge de ex si x || y && z
bool exprOrPrim()
{
	Token *start = iTk;

	if (consume(OR))
	{
		if (exprAnd())
		{
			if (exprOrPrim())
				return true;
			else
				tkerr("Invalid expression for the and expression");
		}
		else
			tkerr("Invalid expression for the or operator");
	}

	iTk = start;
	return true;
}

// daca ai &&
bool exprAnd()
{
	Token *start = iTk;

	if (exprEq())
	{
		if (exprAndPrim())
			return true;
		else
			tkerr("Invalid expression for the eq expression");
	}

	iTk = start;
	return false;
}

// exprAnd: exprAnd AND exprEq | exprEq
//
// exprAnd: exprEq exprAndPrim
// exprAndPrim: AND exprEq exprAndPrim | epsilon

// merge si pe x == 10 && y != z de ex
bool exprAndPrim()
{
	Token *start = iTk;

	if (consume(AND))
	{
		if (exprEq())
		{
			if (exprAndPrim())
				return true;
			else
				tkerr("Invalid expression for the eq expression");
		}
		else
			tkerr("Invalid expression at the and operator");
	}

	iTk = start;
	return true;
}

//!= sau == sau >= sau ..
bool exprEq()
{
	Token *start = iTk;

	if (exprRel())
	{
		if (exprEqPrim())
			return true;
		else
			tkerr("Invalid expression at the rel expression");
	}

	iTk = start;
	return false;
}

// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
//
// exprEq: exprRel exprEqPrim
// exprEqPrim: ( EQUAL | NOTEQ ) exprRel exprEqPrim
// poti avea de ex x < 10 != y + z
bool exprEqPrim()
{
	Token *start = iTk;

	if (consume(EQUAL))
	{
		if (exprRel())
		{
			if (exprEqPrim())
				return true;
			else
				tkerr("Invalid expression at the rel expression");
		}
		else
			tkerr("Invalid expression at the == expression");
	}

	if (consume(NOTEQ))
	{
		if (exprRel())
		{
			if (exprEqPrim())
				return true;
			else
				tkerr("Invalid expression at the rel expression");
		}
		else
			tkerr("Invalid expression after the != symbol");
	}

	iTk = start;
	return true;
}

// pt comparatii de ex x + y < 10 * z
bool exprRel()
{
	Token *t = iTk;

	if (exprAdd()) // add contine si + si - si mul
	{
		if (exprRelPrim())
			return true;
	}

	iTk = t;
	return false;
}

bool exprRelPrim()
{
	Token *t = iTk;
	if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ))
	{
		if (exprAdd())
		{
			if (exprRelPrim())
				return true;
		}
		else
			tkerr("Missing right operand");
	}
	iTk = t;
	return true;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
//
// exprAdd: exprMul exprAddPrim
// exprAddPrim: ( ADD | SUB ) exprMul exprAddPrim
// x + y * 2 - z de ex
bool exprAddPrim()
{
	Token *start = iTk;

	if (consume(ADD))
	{
		if (exprMul())
		{
			if (exprAddPrim())
				return true;
			else
				tkerr("Invalid expression at the multiplication expression");
		}
		else
			tkerr("Invalid expression at the add operation");
	}
	if (consume(SUB))
	{
		if (exprMul())
		{
			if (exprAddPrim())
				return true;
			else
				tkerr("Invalid expression at the multiplication expression");
		}
		else
			tkerr("Invalid expression at the sub operation");
	}

	iTk = start;
	return true;
}

bool exprAdd()
{
	Token *start = iTk;

	if (exprMul())
	{
		if (exprAddPrim())
			return true;
		else
			tkerr("Invalid expression at the multiplication expression");
	}

	iTk = start;
	return false;
}

// x * (y / 2) -> x mul cast
bool exprMul()
{
	Token *start = iTk;

	if (exprCast())
	{
		if (exprMulPrim())
			return true;
		else
			tkerr("Invalid expression at the cast operation");
	}

	iTk = start;
	return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
//
// exprMul: exprCast exprMulPrim
// exprMulPrim:( MUL | DIV ) exprCast exprMulPrim
bool exprMulPrim()
{
	Token *start = iTk;
	if (consume(MUL))
	{
		if (exprCast())
		{
			if (exprMulPrim())
				return true;
			else
				tkerr("Invalid expression at the cast operation");
		}
		else
			tkerr("Invalid expression at the mul operation");
	}
	if (consume(DIV))
	{
		if (exprCast())
		{
			if (exprMulPrim())
				return true;
			else
				tkerr("Invalid expression after the cast operation");
		}
		else
			tkerr("Invalid expression after the div operation");
	}

	iTk = start;
	return true;
}

// cast ca si (int)x
bool exprCast()
{
	Token *start = iTk;
	if (consume(LPAR))
	{
		Type t;
		if (typeBase(&t))
		{
			if (arrayDecl(&t))
			{
			}
			if (consume(RPAR))
			{
				if (exprCast())
					return true;
				else
					tkerr("Invalid expression at the cast operation");
			}
			else
				tkerr("Missing ) from the cast operation");
		}
	}
	else if (exprUnary())
		return true;

	iTk = start;
	return false;
}

//! true sau -ceva.. -array[index] --x
bool exprUnary()
{
	Token *start = iTk;
	if (consume(SUB))
	{
		if (exprUnary())
			return true;
		else
			tkerr("Invalid expression after sub operator");
	}
	if (consume(NOT))
	{
		if (exprUnary())
			return true;
		else
			tkerr("Invalid expression after not operator");
	}
	if (exprPostfix())
		return true;

	iTk = start;
	return false;
}

// pt variabile de la structuri gen array[3].membru
bool exprPostfix()
{
	Token *start = iTk;

	if (exprPrimary())
	{
		if (exprPostfixPrim())
			return true;
	}

	iTk = start;
	return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//   | exprPostfix DOT ID
//   | exprPrimary
//
// exprPostfix:exprPrimary exprPostfixPrim
// exprPostfixPrim:LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim

// array[index] sau a[index][index] sau obiect.field.field.field..
bool exprPostfixPrim()
{
	Token *start = iTk;
	if (consume(LBRACKET))
	{
		if (expr())
		{
			if (consume(RBRACKET))
			{
				if (exprPostfixPrim())
					return true;
				else
					tkerr("Invalid expression after the ] operator");
			}

			else
				tkerr("Missing ] operator from exprPostfix");
		}
		else
			tkerr("Invalid expression after the [ operator ");
	}
	if (consume(DOT))
	{
		if (consume(ID))
		{
			if (exprPostfixPrim())
				return true;
			else
				tkerr("Invalid expression after ID");
		}
		else
			tkerr("Invalid id after the dot operator");
	}

	iTk = start;
	return true;
}

// apel functie(a,b), (x+2), 3.14, 'hello'
bool exprPrimary()
{
	Token *start = iTk;
	if (consume(ID))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				for (;;)
				{
					if (consume(COMMA))
					{
						if (expr())
						{
						}
					}
					else
					{
						break;
					}
				}
			}
			if (consume(RPAR))
			{
			}
		}

		return true;
	}
	if (consume(INT))
	{
		return true;
	}
	if (consume(DOUBLE))
	{
		return true;
	}
	if (consume(CHAR))
	{
		return true;
	}
	if (consume(STRING))
	{
		return true;
	}
	if (consume(LPAR))
	{
		if (expr())
		{
			if (consume(RPAR))
				return true;
		}
	}

	iTk = start;
	return false;
}

void parse(Token *tokens)
{
	iTk = tokens;
	if (!unit())
		tkerr("syntax error");
}