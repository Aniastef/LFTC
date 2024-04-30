#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
#include "parser.h"

int main()
{
    char *inbuffer = loadFile("./tests/testparser.c");
    Token *tokens = tokenize(inbuffer);
    parse(tokens);
    free(inbuffer);
    

    return 0;
}
