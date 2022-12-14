#include <string.h>

#include "lisp.h"
#include "assert.h"

static int c_eq(ptr a, ptr b)
{
    if (a == b)
    {
        return 1;
    }
    if (a < 0 || b < 0 || kind(a) != kind(b))
    {
        return 0;
    }
    switch (kind(a))
    {
    case T_NIL:
        return 1;
    case T_SYM:
        return get_symbol(a) == get_symbol(b);
    case T_INT:
        return get_int(a) == get_int(b);
    case T_CON:
        return c_eq(get_head(a), get_head(b)) &&
               c_eq(get_tail(a), get_tail(b));
    default:
        failwith("unreachable");
    }
}

static ptr eq(ptr i)
{
    ptr a = elem(0, i);
    ptr b = elem(1, i);
    return c_eq(a, b) ? new_true() : new_nil();
}

#define _ORD_(name, cmp)                                       \
    static ptr name(ptr a)                                     \
    {                                                          \
        if (kind(a) == T_NIL)                                  \
        {                                                      \
            return new_true();                                 \
        }                                                      \
        assert(kind(a) == T_CON);                              \
        ptr b = get_tail(a);                                   \
        if (kind(b) != T_NIL)                                  \
        {                                                      \
            if (get_int(get_head(a)) cmp get_int(get_head(b))) \
            {                                                  \
                return name(b);                                \
            }                                                  \
            else                                               \
            {                                                  \
                return new_nil();                              \
            }                                                  \
        }                                                      \
        else                                                   \
        {                                                      \
            return new_true();                                 \
        }                                                      \
    }
_ORD_(lt, <)
_ORD_(gt, >)
_ORD_(lte, <=)
_ORD_(gte, >=)

#define _ARITH_(name, op, init)                \
    static ptr name(ptr a)                     \
    {                                          \
        i64 val = init;                        \
        while (kind(a) == T_CON)               \
        {                                      \
            val = val op get_int(get_head(a)); \
            a = get_tail(a);                   \
        }                                      \
        return new_int(val);                   \
    }
_ARITH_(sum, +, 0)
_ARITH_(prod, *, 1)

static ptr minus(ptr i)
{
    ptr arg0 = get_head(i);
    if (kind(get_tail(i)) != T_NIL)
    {
        ptr arg1 = elem(1, i);
        return new_int(get_int(arg0) - get_int(arg1));
    }
    else
    {
        return new_int(-get_int(arg0));
    }
}

static ptr div(ptr i)
{
    return new_int(get_int(elem(0, i)) / get_int(elem(1, i)));
}

static ptr mod(ptr i)
{
    return new_int(get_int(elem(0, i)) % get_int(elem(1, i)));
}

#define _CMP_(name, _kind)             \
    static ptr name(ptr i)             \
    {                                  \
        i = get_head(i);               \
        if (kind(i) != _kind) \
        {                              \
            return new_nil();          \
        }                              \
        else                           \
        {                              \
            return new_true();         \
        }                              \
    }
_CMP_(is_nil, T_NIL)
_CMP_(is_int, T_INT)
_CMP_(is_sym, T_SYM)
_CMP_(is_pair, T_CON)

static ptr is_list(ptr i)
{
    i = get_head(i);
    while (kind(i) == T_CON)
    {
        i = get_tail(i);
    }
    if (kind(i) != T_NIL)
    {
        return new_nil();
    }
    else
    {
        return new_true();
    }
}

static ptr eval_quote(ptr i)
{
    return get_head(i);
}

static ptr apply_quasiquote(ptr i, int depth)
{
    switch (kind(i))
    {
    case T_SYM:
    case T_NIL:
    case T_INT:
    case T_FUN:
    case T_MAC:
        return i;
    case T_CON:
    {
        ptr head = get_head(i);
        ptr tail = get_tail(i);
        if (is_unquote(head))
        {
            depth--;
            if (depth == 0)
            {
                return eval(get_head(tail));
            }
        }
        if (is_quasiquote(head))
        {
            depth++;
        }
        ptr new_head = apply_quasiquote(head, depth);
        ptr new_tail = apply_quasiquote(tail, depth);
        if (new_head == head && new_tail == tail)
        {
            return i;
        }
        else
        {
            return new_cons(new_head, new_tail);
        }
    }
    default:
        failwith("unreachable");
    }
}
static ptr eval_quasiquote(ptr i)
{
    i = get_head(i);
    return apply_quasiquote(i, 1);
}

static ptr eval_cond(ptr i)
{
    if (kind(i) == T_NIL)
    {
        return i;
    }
    else
    {
        assert(kind(i) == T_CON);

        ptr branch = get_head(i);
        ptr rest = get_tail(i);

        ptr cond = elem(0, branch);
        ptr code = elem(1, branch);

        cond = eval(cond);
        if (kind(cond) == T_NIL)
        {
            return eval_cond(rest);
        }
        else
        {
            return eval(code);
        }
    }
}

static ptr cons(ptr i)
{
    ptr hd = elem(0, i);
    ptr tl = elem(1, i);
    return new_cons(hd, tl);
}

static ptr head(ptr i)
{
    return get_head(get_head(i));
}

static ptr tail(ptr i)
{
    return get_tail(get_head(i));
}

static ptr el(ptr i)
{
    ptr idx = elem(0, i);
    ptr list = elem(1, i);
    return elem(get_int(idx), list);
}

static ptr list(ptr i)
{
    return i;
}

static ptr panic(ptr i)
{
    printf("error: ");
    println(get_head(i));
    failwith("explicit panic");
    return 0;
}

static ptr concat_sym(ptr i)
{
    int len = 0;
    ptr cursor = i;
    while (kind(cursor) == T_CON)
    {
        len += strlen(get_symbol_str(get_symbol(get_head(cursor))));
        cursor = get_tail(cursor);
    }
    assert(len < SYM_SIZE);

    char buf[SYM_SIZE] = {0};

    cursor = i;
    while (kind(cursor) == T_CON)
    {
        strcat(buf, get_symbol_str(get_symbol(get_head(cursor))));
        cursor = get_tail(cursor);
    }

    return new_symbol(buf);
}

static ptr progn(ptr i)
{
    ptr result = new_nil();
    while (kind(i) == T_CON)
    {
        result = get_head(i);
        i = get_tail(i);
    }
    return result;
}

static ptr read(ptr i)
{
    i = get_head(i);

    ptr cursor = i;
    i64 size = 0;
    while (kind(cursor) == T_CON)
    {
        size++;
        cursor = get_tail(cursor);
    }

    printf("%ld\n\n", size);

    char buf[size+1];
    cursor = i;
    i64 k = 0;

    while (kind(cursor) == T_CON)
    {
        buf[k++] = (char) get_int(get_head(cursor));
        cursor = get_tail(cursor);
    }
    buf[k] = 0;

    char *buf_ptr = &(buf[0]);

    return parse(&buf_ptr);
}

static ptr b_eval(ptr i)
{
    return eval(get_head(i));
}

static ptr is_builtin_fun(ptr i)
{
    i = get_head(i);
    return kind(i) == T_FUN ? new_true() : new_nil();
}

void register_builtins(void)
{

    #define new_builtin_mc(a, b) new_builtin(a, b, T_MAC)
    #define new_builtin_fn(a, b) new_builtin(a, b, T_FUN)

    new_builtin_mc(&eval_cond, "cond");
    new_builtin_mc(&eval_quasiquote, "quasiquote");
    new_builtin_mc(&eval_quote, "quote");

    new_builtin_fn(&eq, "=");

    new_builtin_fn(&lt, "<");
    new_builtin_fn(&gt, ">");
    new_builtin_fn(&lte, "<=");
    new_builtin_fn(&gte, ">=");

    new_builtin_fn(&sum, "+");
    new_builtin_fn(&prod, "*");
    new_builtin_fn(&minus, "-");
    new_builtin_fn(&div, "/");
    new_builtin_fn(&mod, "%");

    new_builtin_fn(&is_nil, "nil?");
    new_builtin_fn(&is_int, "int?");
    new_builtin_fn(&is_sym, "sym?");
    new_builtin_fn(&is_pair, "pair?");
    new_builtin_fn(&is_list, "list?");
    new_builtin_fn(&is_builtin_fun, "bfun?");

    new_builtin_fn(&read, "read");

    new_builtin_fn(&cons, "cons");
    new_builtin_fn(&list, "list");
    new_builtin_fn(&head, "hd");
    new_builtin_fn(&tail, "tl");
    new_builtin_fn(&el, "el");

    new_builtin_fn(&panic, "panic");
    new_builtin_fn(&concat_sym, "symcat");
    new_builtin_fn(&progn, "progn");

    new_builtin_fn(&b_eval, "eval");

    #undef new_builtin_mc
    #undef new_builtin_fn
}
