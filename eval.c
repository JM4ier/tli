#include "lisp.h"
#include "assert.h"

static int debug = 0;

static ptr beta_reduce(ptr code, ptr formal_arg, ptr arg, int quote_depth)
{
    switch (kind(code))
    {
    case T_SYM:
        if (get_symbol(code) == get_symbol(formal_arg) && !quote_depth)
        {
            return quoted(arg);
        }
        return code;
    case T_CON:
    {
        ptr hd = get_head(code);
        if (is_quote(hd))
        {
            if (quote_depth)
            {
                return quoted(beta_reduce(elem(1, code), formal_arg, arg, quote_depth));
            }
            else
            {
                return code;
            }
        }
        if (is_unquote(hd))
        {
            ptr body = get_tail(code);
            assert(quote_depth);
            body = beta_reduce(body, formal_arg, arg, quote_depth - 1);
            // TODO no copying if same
            return new_cons(hd, body);
        }
        else if (is_quasiquote(hd))
        {
            ptr body = get_tail(code);
            body = beta_reduce(body, formal_arg, arg, quote_depth + 1);
            // TODO no copying if same
            return new_cons(hd, body);
        }
        if (is_functionlike(hd) && !quote_depth)
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
            fun_body = beta_reduce(fun_body, formal_arg, arg, quote_depth);
            return new_list(3, hd, arg_list, fun_body);
        }
        else
        {
            ptr head = get_head(code);
            ptr tail = get_tail(code);

            ptr new_head = beta_reduce(head, formal_arg, arg, quote_depth);
            ptr new_tail = beta_reduce(tail, formal_arg, arg, quote_depth);

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

ptr eval_elems(ptr is);
ptr eval(ptr i)
{
    if (debug)
    {
        printf("[DEBUG] ");
        println(i);
    }
    switch (kind(i))
    {
    case T_FUN:
    case T_MAC:
    case T_NIL:
    case T_INT:
        return i;
    case T_SYM:
    {
        ptr sym = get_symbol(i);
        ptr bind = get_symbol_binding(sym);
        if (kind(bind) == T_POO)
        {
            printf("`%s` is unbound.\n", get_symbol_str(sym));
            assert(false);
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
            return new_nil();
        }
        ptr fun = eval(head);
        ptr args = get_tail(i);

        if (kind(fun) == T_FUN)
        {
            return get_fn_ptr(fun)(eval_elems(args));
        }

        if (kind(fun) == T_MAC)
        {
            return get_fn_ptr(fun)(args);
        }

        if (is_pragma(fun))
        {
            debug = 1;
            return new_nil();
        }

        if (kind(fun) != T_CON)
        {

            printf("unexpected form of function application: (");
            print(fun);
            printf(" #args)\n");
            failwith("wrong function application");
        }

        ptr fun_head = elem(0, fun);

        if (!is_functionlike(fun_head))
        {
            println(fun_head);
            assert(is_functionlike(fun_head));
        }

        if (is_lambda(fun_head))
        {
            // argument evaluation
            args = eval_elems(args);
        }

        ptr formal_args = elem(1, fun);
        ptr fun_body = elem(2, fun);

        // TODO Partial app
        int partial_args_len = 0;
        ptr partial_args[10];

        while (kind(formal_args) != T_NIL)
        {
            ptr f_arg = get_head(formal_args);
            ptr c_arg = get_head(args);

            if (is_partial_app(c_arg))
            {
                partial_args[partial_args_len++] = f_arg;
                assert(partial_args_len < 10);
            }
            else
            {
                fun_body = beta_reduce(fun_body, f_arg, c_arg, false);
            }

            formal_args = get_tail(formal_args);
            args = get_tail(args);
        }

        // in this case we just return a different lambda
        if (partial_args_len > 0)
        {
            ptr new_formal_args = new_nil();
            while (partial_args_len)
            {
                new_formal_args = new_cons(partial_args[--partial_args_len], new_formal_args);
            }
            return new_cons(fun_head, new_cons(new_formal_args, new_cons(fun_body, new_nil())));
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
