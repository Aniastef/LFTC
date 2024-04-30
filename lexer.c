
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"

Token *tokens; // single linked list of tokens
Token *lastTk; // the last token in list

int line = 1; // the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code)
{
    Token *tk = safeAlloc(sizeof(Token));
    tk->code = code;
    tk->line = line;
    tk->next = NULL;
    if (lastTk)
    {
        lastTk->next = tk;
    }
    else
    {
        tokens = tk;
    }
    lastTk = tk;
    return tk;
}

char *extract(const char *begin, const char *end)
{
    size_t length = end - begin;
    char *result = safeAlloc(length + 1);
    strncpy(result, begin, length);
    result[length] = '\0';
    return result;
}

Token *tokenize(const char *pch)
{
    const char *start; // inceputul unui token
    Token *tk;
    for (;;)
    {
        switch (*pch) // cu pch parcurgem sirul
        {
        case ' ':
        case '\t':
            pch++;
            break;
        case '\r': // handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
            if (pch[1] == '\n')
                pch++;
            // fallthrough to \n
        case '\n':
            line++;
            pch++;
            break;
        case '\0': // aduga END la finalul de lista si returneaza lista de tokens
            addTk(END);
            return tokens;
        case ',':
            addTk(COMMA); // se adauga COMMA in lista de token-uri
            pch++;
            break;
        case ';':
            addTk(SEMICOLON);
            pch++;
            break;
        case '(':
            addTk(LPAR);
            pch++;
            break;
        case ')':
            addTk(RPAR);
            pch++;
            break;
        case '[':
            addTk(LBRACKET);
            pch++;
            break;
        case ']':
            addTk(RBRACKET);
            pch++;
            break;
        case '{':
            addTk(LACC);
            pch++;
            break;
        case '}':
            addTk(RACC);
            pch++;
            break;
        case '+':
            addTk(ADD);
            pch++;
            break;
        case '-':
            addTk(SUB);
            pch++;
            break;
        case '*':
            addTk(MUL);
            pch++;
            break;
        case '/': // pch[0] sa fie /
            if (pch[1] == '/')
            {
                while (*pch != '\0' && *pch != '\n') // avanseaza pointer-ul pch prin sir pana intalneste sfarsit sau linie noua
                {
                    pch++;
                }
            }
            else
            {
                addTk(DIV);
                pch++;
            }
            break;
        case '.':
            addTk(DOT);
            pch++;
            break;
        case '&':
            if (pch[1] == '&')
            {
                addTk(AND);
                pch += 2;
            }
            break;
        case '|':
            if (pch[1] == '|')
            {
                addTk(OR);
                pch += 2;
            }
            break;
        case '!':
            if (pch[1] == '=')
            {
                addTk(NOTEQ);
                pch += 2;
            }
            else
            {
                addTk(NOT);
                pch++;
            }
            break;
        case '=':
            if (pch[1] == '=')
            {
                addTk(EQUAL);
                pch += 2;
            }
            else
            {
                addTk(ASSIGN);
                pch++;
            }
            break;
        case '<':
            if (pch[1] == '=')
            {
                addTk(LESSEQ);
                pch += 2;
            }
            else
            {
                addTk(LESS);
                pch++;
            }
            break;
        case '>':
            if (pch[1] == '=')
            {
                addTk(GREATEREQ);
                pch += 2;
            }
            else
            {
                addTk(GREATER);
                pch++;
            }
            break;
        case '\'':
            if (pch[1] != '\'')
            {
                if (pch[2] == '\'')
                {
                    tk = addTk(CHAR);
                    tk->c = pch[1];
                    pch += 3;
                }
                else
                    err("quotation marks weren't closed");
            }
            break;
        default:
            if (isalpha(*pch) || *pch == '_') // daca-s litere sau _
            {
                for (start = pch++; isalnum(*pch) || *pch == '_'; pch++) // parcurgem for-ul pana nu mai e litera/cifra/_
                {
                }
                char *text = extract(start, pch); // extrage textul prins intre start si pch si il memoreaza
                if (strcmp(text, "char") == 0)
                    addTk(TYPE_CHAR);
                else if (strcmp(text, "int") == 0)
                    addTk(TYPE_INT);
                else if (strcmp(text, "double") == 0)
                    addTk(TYPE_DOUBLE);
                else if (strcmp(text, "while") == 0)
                    addTk(WHILE);
                else if (strcmp(text, "if") == 0)
                    addTk(IF);
                else if (strcmp(text, "else") == 0)
                    addTk(ELSE);
                else if (strcmp(text, "struct") == 0)
                    addTk(STRUCT);
                else if (strcmp(text, "return") == 0)
                    addTk(RETURN);
                else if (strcmp(text, "void") == 0)
                    addTk(VOID);
                else // daca nu corespunde cheilor de mai sus il punem ca ID
                {
                    tk = addTk(ID);
                    tk->text = text;
                }
            }
            else if (isdigit(*pch)) // daca caracterul curent este o cifră
            {
                short dot = 0;
                short scientificNotation = 0;
                for (start = pch; isdigit(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E' || *pch == '-' || *pch == '+'; pch++) // merge cat timp caracterul e . e E - +
                {
                    if (*pch == '.')
                    {
                        // daca avem deja ., daca avem e pus cum trebuie si daca ce e inainte de . nu e cifra
                        if (dot || *(pch + 1) == 'e' || *(pch + 1) == 'E' || !isdigit(*(pch - 1)) || !isdigit(*(pch + 1)))
                            err("invalid format");

                        dot = 1;
                    }
                    else if (*pch == 'e' || *pch == 'E')
                    {
                        if (scientificNotation || !isdigit(*(pch - 1)) || (*(pch + 1) != '+' && *(pch + 1) != '-' && !isdigit(*(pch + 1))))
                            // exista deja un e sau nu e o cifra sau semn inainte sau dupa e
                            err("format invalid");

                        scientificNotation = 1;
                    }

                    if ((*pch == '+' || *pch == '-') && !isdigit(*(pch + 1)))
                        err("format invalid");
                }
                char *text = extract(start, pch); // se extrage textul din intervalul [start, pch)

                if (dot || scientificNotation)
                {
                    // nr in virgula mobila
                    tk = addTk(DOUBLE);
                    char *endptr;
                    tk->d = strtod(text, &endptr); // conversie text in nr in virgula mobila
                }
                else
                {
                    // nr intreg
                    tk = addTk(INT);
                    tk->i = atoi(text); // conversie in nr intreg
                }
            }

            else if (*pch == '\"')
            {
                for (start = ++pch; *pch != '\"'; pch++) // parcurge sirul pana gaseste o ghilimea dubla
                {
                    if (*pch == '\0') // s a terminat sirul si nu s a gasit ghilimea..
                    {
                        err("quotation marks weren't closed");
                    }
                }

                char *text = extract(start, pch);

                pch++;

                tk = addTk(STRING);
                tk->text = text;
            }
            else
            {
                err("invalid char: %c (%d)", *pch, *pch);
            }
        }
    }
}

void showTokens(const Token *tokens)
{
    int line_counter = 1;
    for (const Token *tk = tokens; tk; tk = tk->next)
    {
        printf("%d\t", tk->line);
        switch (tk->code)
        {
        case ID:
            printf("ID:%s\n", tk->text);
            break;
        case INT:
            printf("INT:%d\n", tk->i);
            break;
        case DOUBLE:
            printf("DOUBLE:%.2f\n", tk->d);
            break;
        case CHAR:
            printf("CHAR:%c\n", tk->c);
            break;
        case STRING:
            printf("STRING:%s\n", tk->text);
            break;

            // keywords
        case TYPE_CHAR:
            printf("TYPE_CHAR\n");
            break;
        case TYPE_DOUBLE:
            printf("TYPE_DOUBLE\n");
            break;
        case ELSE:
            printf("ELSE\n");
            break;
        case IF:
            printf("IF\n");
            break;
        case TYPE_INT:
            printf("TYPE_INT\n");
            break;
        case RETURN:
            printf("RETURN\n");
            break;
        case STRUCT:
            printf("STRUCT\n");
            break;
        case VOID:
            printf("VOID\n");
            break;
        case WHILE:
            printf("WHILE\n");
            break;

            // delimiters
        case COMMA:
            printf("COMMA\n");
            break;
        case SEMICOLON:
            printf("SEMICOLON\n");
            break;
        case LPAR:
            printf("LPAR\n");
            break;
        case RPAR:
            printf("RPAR\n");
            break;
        case LBRACKET:
            printf("LBRACKET\n");
            break;
        case RBRACKET:
            printf("RBRACKET\n");
            break;
        case LACC:
            printf("LACC\n");
            break;
        case RACC:
            printf("RACC\n");
            break;
        case END:
            printf("END\n");
            break;

            // operators
        case ADD:
            printf("ADD\n");
            break;
        case SUB:
            printf("SUB\n");
            break;
        case MUL:
            printf("MUL\n");
            break;
        case DIV:
            printf("DIV\n");
            break;
        case DOT:
            printf("DOT\n");
            break;
        case AND:
            printf("AND\n");
            break;
        case OR:
            printf("OR\n");
            break;
        case NOT:
            printf("NOT\n");
            break;
        case ASSIGN:
            printf("ASSIGN\n");
            break;
        case EQUAL:
            printf("EQUAL\n");
            break;
        case NOTEQ:
            printf("NOTEQ\n");
            break;
        case LESS:
            printf("LESS\n");
            break;
        case LESSEQ:
            printf("LESSEQ\n");
            break;
        case GREATER:
            printf("GREATER\n");
            break;
        case GREATEREQ:
            printf("GREATEREQ\n");
            break;

        default:
            break;
        }
        line_counter += 1;
    }
}
