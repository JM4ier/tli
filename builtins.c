#include "builtins.h"
#include "defs.h"
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
        a = eval_elems(a);                                     \
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
        a = eval_elems(a);                     \
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

#define _CMP_(name, _kind)             \
    static ptr name(ptr i)             \
    {                                  \
        i = eval_elems(i);             \
        i = get_head(i);               \
        if (i < 0 || kind(i) != _kind) \
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
    i = eval_elems(i);
    i = get_head(i);
    while (i >= 0 && kind(i) == T_CON)
    {
        i = get_tail(i);
    }
    if (i < 0 || kind(i) != T_NIL)
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

static ptr apply_quasiquote(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    switch (kind(i))
    {
    case T_SYM:
    case T_NIL:
    case T_INT:
        return i;
    case T_CON:
    {
        ptr head = get_head(i);
        ptr tail = get_tail(i);
        if (is_unquote(head))
        {
            return eval(get_head(tail));
        }
        else
        {
            ptr new_head = apply_quasiquote(head);
            ptr new_tail = apply_quasiquote(tail);
            if (new_head == head && new_tail == tail)
            {
                return i;
            }
            else
            {
                return new_cons(new_head, new_tail);
            }
        }
    }
    default:
        failwith("unreachable");
    }
}
static ptr eval_quasiquote(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    else
    {
        i = get_head(i);
        return apply_quasiquote(i);
    }
}

static ptr eval_cond(ptr i)
{
    if (i < 0 || kind(i) == T_NIL)
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

void register_builtins(void)
{
    new_builtin(&eq, "=");

    new_builtin(&lt, "<");
    new_builtin(&gt, ">");
    new_builtin(&lte, "<=");
    new_builtin(&gte, ">=");

    new_builtin(&sum, "+");
    new_builtin(&prod, "*");

    new_builtin(&is_nil, "nil?");
    new_builtin(&is_int, "int?");
    new_builtin(&is_sym, "sym?");
    new_builtin(&is_pair, "pair?");
    new_builtin(&is_list, "list?");

    new_builtin(&eval_quote, "quote");
    new_builtin(&eval_quasiquote, "quasiquote");

    new_builtin(&eval_cond, "cond");
}
