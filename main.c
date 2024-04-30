#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"

int main()
{
    char *inbuffer = loadFile("./tests/testad.c");
    Token *tokens = tokenize(inbuffer);

    pushDomain();
    parse(tokens);
    showDomain(symTable, "global");
    dropDomain();

    return 0;
}
