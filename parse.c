#include <stdint.h>
#include <string.h>

#include "lisp.h"
#include "assert.h"

static int is_numeric(char c)
{
    return c >= '0' && c <= '9';
}

static int is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

void strip(char **i)
{
    while (is_whitespace(**i))
    {
        ++*i;
    }
}

static int is_paren(char c)
{
    return c == '(' || c == ')';
}

static int is_quoting(char c)
{
    return c == '#' || c == '\'' || c == '`';
}

static ptr parse_list(char **input)
{
    assert(**input && "unexpected EOF");
    strip(input);
    if (**input == ')')
    {
        ++*input;
        return new_nil();
    }
    else
    {
        ptr head = parse(input);
        ptr tail = parse_list(input);
        return new_cons(head, tail);
    }
}

ptr parse(char **input)
{
    assert(**input && "unexpected EOF");
    strip(input);
    if (is_numeric(**input))
    {
        i64 num = 0;
        while (is_numeric(**input))
        {
            num *= 10;
            num += (**input) - '0';
            ++*input;
        }
        return new_int(num);
    }
    else if (**input == ')')
    {
        ++*input;
        return new_nil();
    }
    else if (**input == '(')
    {
        ++*input;
        return parse_list(input);
    }
    else if (is_quoting(**input))
    {
        char *sym = 0;
        switch (**input)
        {
        case '\'':
            sym = "quote";
            break;
        case '`':
            sym = "quasiquote";
            break;
        case '#':
            sym = "unquote";
            break;
        default:
            failwith("unknown quote");
        }
        ++*input;
        ptr symbol = new_symbol(sym);
        return new_list(2, symbol, parse(input));
    }
    else
    {
        // symbol
        char *begin = *input;
        while (!is_whitespace(**input) && !is_paren(**input) && **input)
        {
            ++*input;
        }
        char buf[16] = {0};
        assert(*input - begin < 16);
        memcpy(buf, begin, (size_t)(*input - begin));
        ptr sym = new_symbol(buf);
        return sym;
    }
}
