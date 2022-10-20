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

static ptr beta_reduce(ptr code, ptr formal_arg, ptr arg, int inside_quote)
{
    switch (kind(code))
    {
    case T_SYM:
        if (get_symbol(code) == get_symbol(formal_arg) && !inside_quote)
        {
            return quoted(arg);
        }
        return code;
    case T_CON:
    {
        ptr hd = get_head(code);
        if (is_quote(hd))
        {
            return code;
        }
        if (is_unquote(hd))
        {
            ptr body = get_tail(code);
            body = beta_reduce(body, formal_arg, arg, false);
            // TODO no copying if same
            return new_cons(hd, body);
        }
        else if (is_quasiquote(hd)) 
        {
            ptr body = get_tail(code);
            body = beta_reduce(body, formal_arg, arg, true);
            // TODO no copying if same
            return new_cons(hd, body);
        }
        if (is_functionlike(hd))
        {
            ptr arg_list = elem(1, code);
            ptr fun_body = elem(2, code);

            ptr cursor = arg_list;
            while (kind(cursor) == T_CON)
            {
                ptr fun_arg = elem(0, cursor);
                if (fun_arg == formal_arg)
                {
                    return code;
                }
                cursor = get_tail(cursor);
            }
            fun_body = beta_reduce(fun_body, formal_arg, arg, inside_quote);
            return new_list(3, hd, arg_list, fun_body);
        } 
        else
        {
            ptr head = get_head(code);
            ptr tail = get_tail(code);

            ptr new_head = beta_reduce(head, formal_arg, arg, inside_quote);
            ptr new_tail = beta_reduce(tail, formal_arg, arg, inside_quote);

            if (new_head == head && new_tail == tail)
            {
                return code;
            }
            else
            {
                return new_cons(new_head, new_tail);
            }
        }
    }
    default:
        return code;
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
            def = eval(def);
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


        ptr fun_head = elem(0, fun);

        assert(is_functionlike(fun_head));

        if (is_lambda(fun_head))
        {
            // argument evaluation
            args = eval_elems(args);
        }

        ptr formal_args = elem(1, fun);
        ptr fun_body    = elem(2, fun);

        while (kind(formal_args) != T_NIL)
        {
            ptr f_arg = get_head(formal_args);
            ptr c_arg = get_head(args);

            fun_body = beta_reduce(fun_body, f_arg, c_arg, false);

            formal_args = get_tail(formal_args);
            args = get_tail(args);
        }

        if (is_macro(fun_head))
        {
            // macro expansion
            fun_body = eval(fun_body);
        }

        return eval(fun_body);
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
