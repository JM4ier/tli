#include "lisp.h"
#include "assert.h"

static ptr (*builtins[MAX_BUILTINS])(ptr) = {0};
static int builtins_len = 1;

void new_builtin(ptr (*fun)(ptr), char *sym)
{
    assert(builtins_len < MAX_BUILTINS);
    int idx = -builtins_len;
    builtins[builtins_len++] = fun;
    ptr s = new_symbol(sym);
    new_binding(s, idx);
}

static ptr beta_reduce(ptr code, ptr formal_args, ptr args)
{
    switch (kind(code))
    {
    case T_NIL:
    case T_INT:
        return code;
    case T_SYM:
        for (ptr fa = formal_args, a = args; kind(fa) != T_NIL; fa = get_tail(fa), a = get_tail(a))
        {
            if (get_symbol(code) == get_symbol(get_head(fa)))
            {
                return quoted(get_head(a));
            }
        }
        return code;
    case T_CON:
    {
        ptr head = get_head(code);
        ptr tail = get_tail(code);
        if (is_lambda(head))
        {
            return code;
        }
        ptr new_head = beta_reduce(head, formal_args, args);
        ptr new_tail = beta_reduce(tail, formal_args, args);
        if (new_head == head && new_tail == tail)
        {
            return code;
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

ptr eval(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    switch (kind(i))
    {
    case T_NIL:
    case T_INT:
        return i;
    case T_SYM:
    {
        ptr sym = get_symbol(i);
        ptr bind = get_symbol_binding(sym);
        if (bind >= 0 && kind(bind) == T_POO)
        {
            printf("`%s` is unbound.\n", get_symbol_str(sym));
            failwith("");
        }
        return bind;
    }
    case T_CON:
    {
        ptr head = get_head(i);
        if (is_functionlike(head))
        {
            return i;
        }
        if (is_definition(head))
        {
            ptr name = elem(1, i);
            ptr def = elem(2, i);
            new_binding(name, def);
            return 0;
        }
        ptr fun = eval(head);
        ptr args = get_tail(i);

        if (fun < 0)
        {
            // builtins behave like macros by default, evaluation must be done by the function itself
            return builtins[-fun](args);
        }

        ptr fun_head = get_head(fun);

        assert(is_functionlike(fun_head));

        if (is_lambda(fun_head))
        {
            // argument evaluation
            args = eval_elems(args);
        }

        ptr fun1 = get_tail(fun);
        assert(kind(fun1) == T_CON);
        ptr formal_args = get_head(fun1);

        ptr fun2 = get_tail(fun1);
        assert(kind(fun2) == T_CON);
        ptr fun_body = get_head(fun2);

        ptr reduced = beta_reduce(fun_body, formal_args, args);

        if (is_macro(fun_head))
        {
            // macro expansion
            reduced = eval(reduced);
        }

        return eval(reduced);
    }
    default:
        failwith("unreachable");
    }
}

ptr eval_elems(ptr is)
{
    if (kind(is) == T_CON)
    {
        ptr head = get_head(is);
        ptr tail = get_tail(is);

        ptr eval_head = eval(head);
        ptr eval_tail = eval_elems(tail);

        return new_cons(eval_head, eval_tail);
    }
    else
    {
        return eval(is);
    }
}