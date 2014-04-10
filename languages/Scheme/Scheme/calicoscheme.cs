// -------------------------------------------------
/*
 Scheme in C#

 Jim Marshall
 Doug Blank
*/
// -------------------------------------------------

#pragma warning disable 109
using System;
using System.Reflection;

public class PJScheme:Scheme
{

  static object void_value = null;

  public static object trampoline () {
	while (pc != null) {
            try {
	        pc ();
	    } catch (Exception e ) {
                if (config.DEBUG > 0) {
                    exception_reg = make_exception("UnHandledException", e.ToString(), symbol_none, symbol_none, symbol_none);
                } else {
                    string [] parts = get_parts(e.ToString(), NEWLINE_STRING);
		    exception_reg = make_exception("UnHandledException", parts[0].ToString(), symbol_none, symbol_none, symbol_none);
                }
		pc = (Function) apply_handler2;
	    }
	}
	return (final_reg);
  }

  public static Closure dlr_func(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return delegate (object[] args) { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list ((object) args);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object> callback0(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return () => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list ();
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object,object> callback1(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return (object arg) => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static Func<object,object,object> callback2(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return (object arg1, object arg2) => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg1, arg2);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static object apply_comparison_rm(object schemeProc, object arg1, object arg2) {
      // used from non-pcs code that evaluates a scheme proc in sort
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg1, arg2);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
  }

   static int _closure_depth = 0;
   static bool _trace_pause = false;

   public static bool get_trace_pause () {
      return _trace_pause;
   }

   public static void set_trace_pause (bool value) {
      _trace_pause = value;
   }

   public static int get_closure_depth ()
   {
      return _closure_depth;
   }

   public static void increment_closure_depth ()
   {
      _closure_depth++;

   }

   public static void decrement_closure_depth ()
   {
      _closure_depth--;
   }

   public static object repeat(object item, object times) {
      object retval = symbol_emptylist;
      for (int i=0; i < ((int)times); i++) {
          retval = cons(item, retval);
      }
      return retval;
   }

   public static bool use_lexical_address(object value) {
	if (!null_q(value)) {
	    value = car(value);
	    _staruse_lexical_address_star = true_q(value);
	}
	return (bool) _staruse_lexical_address_star;
   }

   // *tracing-on?*
   public static object tracing_on(object value) {
	if (null_q(value)) {
	    return _startracing_on_q_star;
	} else {
	    value = car(value);
	    _startracing_on_q_star = (bool)value;
	    return null;
	}
   }


    public static object symbol_emptylist = make_symbol("()");
    public static object symbol_b_extension_d = make_symbol("<extension>");
    public static object symbol_method = make_symbol("method");
    public static object symbol_field = make_symbol("field");
    public static object symbol_constructor = make_symbol("constructor");
    public static object symbol_property = make_symbol("property");
    public static object symbol_done = make_symbol("done");
    public static object symbol_module = make_symbol("module");
    public static object symbol_lit_aexp = make_symbol("lit-aexp");
    public static object symbol_var_aexp = make_symbol("var-aexp");
    public static object symbol_lexical_address_aexp = make_symbol("lexical-address-aexp");
    public static object symbol_if_aexp = make_symbol("if-aexp");
    public static object symbol_assign_aexp = make_symbol("assign-aexp");
    public static object symbol_func_aexp = make_symbol("func-aexp");
    public static object symbol_callback0_aexp = make_symbol("callback0-aexp");
    public static object symbol_callback1_aexp = make_symbol("callback1-aexp");
    public static object symbol_callback2_aexp = make_symbol("callback2-aexp");
    public static object symbol_define_aexp = make_symbol("define-aexp");
    public static object symbol_define_b_aexp = make_symbol("define!-aexp");
    public static object symbol_define_syntax_aexp = make_symbol("define-syntax-aexp");
    public static object symbol_begin_aexp = make_symbol("begin-aexp");
    public static object symbol_lambda_aexp = make_symbol("lambda-aexp");
    public static object symbol_mu_lambda_aexp = make_symbol("mu-lambda-aexp");
    public static object symbol_trace_lambda_aexp = make_symbol("trace-lambda-aexp");
    public static object symbol_mu_trace_lambda_aexp = make_symbol("mu-trace-lambda-aexp");
    public static object symbol_app_aexp = make_symbol("app-aexp");
    public static object symbol_try_catch_aexp = make_symbol("try-catch-aexp");
    public static object symbol_try_finally_aexp = make_symbol("try-finally-aexp");
    public static object symbol_try_catch_finally_aexp = make_symbol("try-catch-finally-aexp");
    public static object symbol_raise_aexp = make_symbol("raise-aexp");
    public static object symbol_choose_aexp = make_symbol("choose-aexp");
    public static object symbol_undefined = make_symbol("undefined");
    public static object symbol_continuation = make_symbol("continuation");
    public static object symbol_none = make_symbol("none");
    public static object symbol_quasiquote = make_symbol("quasiquote");
    public static object symbol_let = make_symbol("let");
    public static object symbol_else = make_symbol("else");
    public static object symbol_eq_q = make_symbol("eq?");
    public static object symbol_quote = make_symbol("quote");
    public static object symbol_memq = make_symbol("memq");
    public static object symbol_define = make_symbol("define");
    public static object symbol_lambda = make_symbol("lambda");
    public static object symbol_args = make_symbol("args");
    public static object symbol_if = make_symbol("if");
    public static object symbol_Equal = make_symbol("=");
    public static object symbol_length = make_symbol("length");
    public static object symbol_error = make_symbol("error");
    public static object symbol_car = make_symbol("car");
    public static object symbol_append = make_symbol("append");
    public static object symbol_list_to_vector = make_symbol("list->vector");
    public static object symbol_cons = make_symbol("cons");
    public static object symbol_sList = make_symbol("list");
    public static object symbol_unit = make_symbol("unit");
    public static object symbol_composite = make_symbol("composite");
    public static object symbol_continuation2 = make_symbol("continuation2");
    public static object symbol_set_b = make_symbol("set!");
    public static object symbol_r = make_symbol("r");
    public static object symbol_cond = make_symbol("cond");
    public static object symbol_else_code = make_symbol("else-code");
    public static object symbol_apply = make_symbol("apply");
    public static object symbol_cdr = make_symbol("cdr");
    public static object symbol_x = make_symbol("x");
    public static object symbol_and = make_symbol("and");
    public static object symbol_pair_q = make_symbol("pair?");
    public static object symbol_not = make_symbol("not");
    public static object symbol_begin = make_symbol("begin");
    public static object symbol_cases = make_symbol("cases");
    public static object symbol_stdin = make_symbol("stdin");
    public static object symbol_end_marker = make_symbol("end-marker");
    public static object symbol_continuation3 = make_symbol("continuation3");
    public static object symbol_continuation4 = make_symbol("continuation4");
    public static object symbol_dot = make_symbol("dot");
    public static object symbol_fail_continuation = make_symbol("fail-continuation");
    public static object symbol_handler = make_symbol("handler");
    public static object symbol_exception = make_symbol("exception");
    public static object symbol_handler2 = make_symbol("handler2");
    public static object symbol_procedure = make_symbol("procedure");
    public static object symbol_ok = make_symbol("ok");
    public static object symbol_macro_transformer = make_symbol("macro-transformer");
    public static object symbol_letrec = make_symbol("letrec");
    public static object symbol_bool_ = make_symbol("bool");
    public static object symbol_or = make_symbol("or");
    public static object symbol__is_to_ = make_symbol("=>");
    public static object symbol_th = make_symbol("th");
    public static object symbol_goto = make_symbol("goto");
    public static object symbol_start_state = make_symbol("start-state");
    public static object symbol_shift = make_symbol("shift");
    public static object symbol_replace = make_symbol("replace");
    public static object symbol_drop = make_symbol("drop");
    public static object symbol_token_start_state = make_symbol("token-start-state");
    public static object symbol_emit = make_symbol("emit");
    public static object symbol_apply_action = make_symbol("apply-action");
    public static object symbol_integer = make_symbol("integer");
    public static object symbol_decimal = make_symbol("decimal");
    public static object symbol_rational = make_symbol("rational");
    public static object symbol_identifier = make_symbol("identifier");
    public static object symbol_boolean = make_symbol("boolean");
    public static object symbol_character = make_symbol("character");
    public static object symbol_named_character = make_symbol("named-character");
    public static object symbol_make_string = make_symbol("string");
    public static object symbol_comment_state = make_symbol("comment-state");
    public static object symbol_lparen = make_symbol("lparen");
    public static object symbol_lbracket = make_symbol("lbracket");
    public static object symbol_rparen = make_symbol("rparen");
    public static object symbol_rbracket = make_symbol("rbracket");
    public static object symbol_apostrophe = make_symbol("apostrophe");
    public static object symbol_backquote = make_symbol("backquote");
    public static object symbol_comma_state = make_symbol("comma-state");
    public static object symbol_hash_prefix_state = make_symbol("hash-prefix-state");
    public static object symbol_string_state = make_symbol("string-state");
    public static object symbol_identifier_state = make_symbol("identifier-state");
    public static object symbol_signed_state = make_symbol("signed-state");
    public static object symbol_decimal_point_state = make_symbol("decimal-point-state");
    public static object symbol_whole_number_state = make_symbol("whole-number-state");
    public static object symbol_comma_at = make_symbol("comma-at");
    public static object symbol_comma = make_symbol("comma");
    public static object symbol_character_state = make_symbol("character-state");
    public static object symbol_lvector = make_symbol("lvector");
    public static object symbol_alphabetic_character_state = make_symbol("alphabetic-character-state");
    public static object symbol_named_character_state = make_symbol("named-character-state");
    public static object symbol_string_escape_state = make_symbol("string-escape-state");
    public static object symbol_signed_decimal_point_state = make_symbol("signed-decimal-point-state");
    public static object symbol_fractional_number_state = make_symbol("fractional-number-state");
    public static object symbol_rational_number_state = make_symbol("rational-number-state");
    public static object symbol_suffix_state = make_symbol("suffix-state");
    public static object symbol_rational_number_state_star = make_symbol("rational-number-state*");
    public static object symbol_signed_exponent_state = make_symbol("signed-exponent-state");
    public static object symbol_exponent_state = make_symbol("exponent-state");
    public static object symbol_apply_state = make_symbol("apply-state");
    public static object symbol_unquote = make_symbol("unquote");
    public static object symbol_unquote_splicing = make_symbol("unquote-splicing");
    public static object symbol_environment = make_symbol("environment");
    public static object symbol_func = make_symbol("func");
    public static object symbol_define_b = make_symbol("define!");
    public static object symbol_let_star = make_symbol("let*");
    public static object symbol_case = make_symbol("case");
    public static object symbol_record_case = make_symbol("record-case");
    public static object symbol_try = make_symbol("try");
    public static object symbol_catch = make_symbol("catch");
    public static object symbol_finally = make_symbol("finally");
    public static object symbol_raise = make_symbol("raise");
    public static object symbol_define_syntax = make_symbol("define-syntax");
    public static object symbol_choose = make_symbol("choose");
    public static object symbol_define_datatype = make_symbol("define-datatype");
    public static object symbol_trace_lambda = make_symbol("trace-lambda");
    public static object symbol_pattern_macro = make_symbol("pattern-macro");
    public static object symbol_callback0 = make_symbol("callback0");
    public static object symbol_callback1 = make_symbol("callback1");
    public static object symbol_callback2 = make_symbol("callback2");
    public static object symbol_aunparse = make_symbol("aunparse");
    public static object symbol_goodbye = make_symbol("goodbye");
    public static object symbol_m = make_symbol("m");
    public static object symbol_dotdotdot = make_symbol("...");
    public static object symbol_application = make_symbol("application");
    public static object symbol_unknown = make_symbol("unknown");
    public static object symbol_macro_generated_exp = make_symbol("macro-generated-exp");
    public static object symbol_b_procedure_d = make_symbol("<procedure>");
    public static object symbol_b_environment_d = make_symbol("<environment>");
    public static object symbol_map = make_symbol("map");
    public static object symbol_Multiply = make_symbol("*");
    public static object symbol_Add = make_symbol("+");
    public static object symbol_Subtract = make_symbol("-");
    public static object symbol_Divide = make_symbol("/");
    public static object symbol_p = make_symbol("%");
    public static object symbol_LessThan = make_symbol("<");
    public static object symbol_LessThanEqual = make_symbol("<=");
    public static object symbol_GreaterThan = make_symbol(">");
    public static object symbol_GreaterThanEqual = make_symbol(">=");
    public static object symbol_abort = make_symbol("abort");
    public static object symbol_abs = make_symbol("abs");
    public static object symbol_assv = make_symbol("assv");
    public static object symbol_boolean_q = make_symbol("boolean?");
    public static object symbol_caddr = make_symbol("caddr");
    public static object symbol_cadr = make_symbol("cadr");
    public static object symbol_call_with_current_continuation = make_symbol("call-with-current-continuation");
    public static object symbol_call_cc = make_symbol("call/cc");
    public static object symbol_caaaar = make_symbol("caaaar");
    public static object symbol_caaadr = make_symbol("caaadr");
    public static object symbol_caaar = make_symbol("caaar");
    public static object symbol_caadar = make_symbol("caadar");
    public static object symbol_caaddr = make_symbol("caaddr");
    public static object symbol_caadr = make_symbol("caadr");
    public static object symbol_caar = make_symbol("caar");
    public static object symbol_cadaar = make_symbol("cadaar");
    public static object symbol_cadadr = make_symbol("cadadr");
    public static object symbol_cadar = make_symbol("cadar");
    public static object symbol_caddar = make_symbol("caddar");
    public static object symbol_cadddr = make_symbol("cadddr");
    public static object symbol_cdaaar = make_symbol("cdaaar");
    public static object symbol_cdaadr = make_symbol("cdaadr");
    public static object symbol_cdaar = make_symbol("cdaar");
    public static object symbol_cdadar = make_symbol("cdadar");
    public static object symbol_cdaddr = make_symbol("cdaddr");
    public static object symbol_cdadr = make_symbol("cdadr");
    public static object symbol_cdar = make_symbol("cdar");
    public static object symbol_cddaar = make_symbol("cddaar");
    public static object symbol_cddadr = make_symbol("cddadr");
    public static object symbol_cddar = make_symbol("cddar");
    public static object symbol_cdddar = make_symbol("cdddar");
    public static object symbol_cddddr = make_symbol("cddddr");
    public static object symbol_cdddr = make_symbol("cdddr");
    public static object symbol_cddr = make_symbol("cddr");
    public static object symbol_char_q = make_symbol("char?");
    public static object symbol_char_is__q = make_symbol("char=?");
    public static object symbol_char_whitespace_q = make_symbol("char-whitespace?");
    public static object symbol_char_alphabetic_q = make_symbol("char-alphabetic?");
    public static object symbol_char_numeric_q = make_symbol("char-numeric?");
    public static object symbol_char_to_integer = make_symbol("char->integer");
    public static object symbol_current_time = make_symbol("current-time");
    public static object symbol_cut = make_symbol("cut");
    public static object symbol_dir = make_symbol("dir");
    public static object symbol_display = make_symbol("display");
    public static object symbol_current_environment = make_symbol("current-environment");
    public static object symbol_equal_q = make_symbol("equal?");
    public static object symbol_eval = make_symbol("eval");
    public static object symbol_eval_ast = make_symbol("eval-ast");
    public static object symbol_exit = make_symbol("exit");
    public static object symbol_for_each = make_symbol("for-each");
    public static object symbol_format = make_symbol("format");
    public static object symbol_get = make_symbol("get");
    public static object symbol_get_stack_trace = make_symbol("get-stack-trace");
    public static object symbol_import = make_symbol("import");
    public static object symbol_integer_to_char = make_symbol("integer->char");
    public static object symbol_list_to_string = make_symbol("list->string");
    public static object symbol_list_ref = make_symbol("list-ref");
    public static object symbol_load = make_symbol("load");
    public static object symbol_make_set = make_symbol("make-set");
    public static object symbol_make_vector = make_symbol("make-vector");
    public static object symbol_member = make_symbol("member");
    public static object symbol_memv = make_symbol("memv");
    public static object symbol_newline = make_symbol("newline");
    public static object symbol_null_q = make_symbol("null?");
    public static object symbol_number_to_string = make_symbol("number->string");
    public static object symbol_number_q = make_symbol("number?");
    public static object symbol_parse = make_symbol("parse");
    public static object symbol_parse_string = make_symbol("parse-string");
    public static object symbol_print = make_symbol("print");
    public static object symbol_printf = make_symbol("printf");
    public static object symbol_Range = make_symbol("range");
    public static object symbol_read_string = make_symbol("read-string");
    public static object symbol_require = make_symbol("require");
    public static object symbol_reverse = make_symbol("reverse");
    public static object symbol_set_car_b = make_symbol("set-car!");
    public static object symbol_set_cdr_b = make_symbol("set-cdr!");
    public static object symbol_snoc = make_symbol("snoc");
    public static object symbol_rac = make_symbol("rac");
    public static object symbol_rdc = make_symbol("rdc");
    public static object symbol_sqrt = make_symbol("sqrt");
    public static object symbol_odd_q = make_symbol("odd?");
    public static object symbol_even_q = make_symbol("even?");
    public static object symbol_quotient = make_symbol("quotient");
    public static object symbol_remainder = make_symbol("remainder");
    public static object symbol_string_length = make_symbol("string-length");
    public static object symbol_string_ref = make_symbol("string-ref");
    public static object symbol_string_q = make_symbol("string?");
    public static object symbol_string_to_number = make_symbol("string->number");
    public static object symbol_string_is__q = make_symbol("string=?");
    public static object symbol_substring = make_symbol("substring");
    public static object symbol_symbol_q = make_symbol("symbol?");
    public static object symbol_unparse = make_symbol("unparse");
    public static object symbol_unparse_procedure = make_symbol("unparse-procedure");
    public static object symbol_using_native = make_symbol("using");
    public static object symbol_use_stack_trace = make_symbol("use-stack-trace");
    public static object symbol_vector = make_symbol("vector");
    public static object symbol_vector_ref = make_symbol("vector-ref");
    public static object symbol_vector_set_b = make_symbol("vector-set!");
    public static object symbol_void = make_symbol("void");
    public static object symbol_zero_q = make_symbol("zero?");
    public static object symbol_current_directory = make_symbol("current-directory");
    public static object symbol_cd = make_symbol("cd");
    public static object symbol_round = make_symbol("round");
    public static object symbol_char_to_string = make_symbol("char->string");
    public static object symbol_string_to_list = make_symbol("string->list");
    public static object symbol_string_to_symbol = make_symbol("string->symbol");
    public static object symbol_symbol_to_string = make_symbol("symbol->string");
    public static object symbol_vector_to_list = make_symbol("vector->list");
    public static object symbol_eqv_q = make_symbol("eqv?");
    public static object symbol_vector_q = make_symbol("vector?");
    public static object symbol_atom_q = make_symbol("atom?");
    public static object symbol_iter_q = make_symbol("iter?");
    public static object symbol_list_q = make_symbol("list?");
    public static object symbol_procedure_q = make_symbol("procedure?");
    public static object symbol_stringLessThan_q = make_symbol("string<?");
    public static object symbol_float_ = make_symbol("float");
    public static object symbol_globals = make_symbol("globals");
    public static object symbol_int_ = make_symbol("int");
    public static object symbol_apply_with_keywords = make_symbol("apply-with-keywords");
    public static object symbol_assq = make_symbol("assq");
    public static object symbol_dict = make_symbol("dict");
    public static object symbol_reset_toplevel_env = make_symbol("reset-toplevel-env");
    public static object symbol_sort = make_symbol("sort");
    public static object symbol_string_append = make_symbol("string-append");
    public static object symbol_string_split = make_symbol("string-split");
    public static object symbol_symbol = make_symbol("symbol");
    public static object symbol_typeof = make_symbol("typeof");
    public static object symbol_use_lexical_address = make_symbol("use-lexical-address");
    public static object symbol_use_tracing = make_symbol("use-tracing");
    public static object symbol_empty = make_symbol("empty");
    public static object symbol_instantiate_hat = make_symbol("instantiate^");
    public static object symbol_substitution = make_symbol("substitution");
    public static object symbol_apply_sub_hat = make_symbol("apply-sub^");
    public static object symbol_atom = make_symbol("atom");
    public static object symbol_pair = make_symbol("pair");
    public static object symbol_b__q_q_q_d = make_symbol("<???>");
    public static object symbol_b_fail_d = make_symbol("<fail>");
    public static object symbol_b_handler_d = make_symbol("<handler>");
    public static object symbol_b_void_d = make_symbol("<void>");
    public static object symbol_exiting = make_symbol("exiting");
    public static object symbol_the = make_symbol("the");
    public static object symbol_interpreter = make_symbol("interpreter");
    
    public static object lit_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_lit_aexp, args);
    }
    
    public static object var_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_var_aexp, args);
    }
    
    public static object lexical_address_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_lexical_address_aexp, args);
    }
    
    public static object if_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_if_aexp, args);
    }
    
    public static object assign_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_assign_aexp, args);
    }
    
    public static object func_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_func_aexp, args);
    }
    
    public static object callback0_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_callback0_aexp, args);
    }
    
    public static object callback1_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_callback1_aexp, args);
    }
    
    public static object callback2_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_callback2_aexp, args);
    }
    
    public static object define_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_define_aexp, args);
    }
    
    public static object define_b_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_define_b_aexp, args);
    }
    
    public static object define_syntax_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_define_syntax_aexp, args);
    }
    
    public static object begin_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_begin_aexp, args);
    }
    
    public static object lambda_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_lambda_aexp, args);
    }
    
    public static object mu_lambda_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_mu_lambda_aexp, args);
    }
    
    public static object trace_lambda_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_trace_lambda_aexp, args);
    }
    
    public static object mu_trace_lambda_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_mu_trace_lambda_aexp, args);
    }
    
    public static object app_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_app_aexp, args);
    }
    
    public static object try_catch_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_try_catch_aexp, args);
    }
    
    public static object try_finally_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_try_finally_aexp, args);
    }
    
    public static object try_catch_finally_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_try_catch_finally_aexp, args);
    }
    
    public static object raise_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_raise_aexp, args);
    }
    
    public static object choose_aexp(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_choose_aexp, args);
    }
    
    public static Function pc = (Function) null;
    public static object aclauses_reg = symbol_undefined;
    public static object action_reg = symbol_undefined;
    public static object adatum_list_reg = symbol_undefined;
    public static object adatum_reg = symbol_undefined;
    public static object ap1_reg = symbol_undefined;
    public static object ap2_reg = symbol_undefined;
    public static object ap_reg = symbol_undefined;
    public static object apair1_reg = symbol_undefined;
    public static object apair2_reg = symbol_undefined;
    public static object args_reg = symbol_undefined;
    public static object avar_reg = symbol_undefined;
    public static object ax_reg = symbol_undefined;
    public static object bindings_reg = symbol_undefined;
    public static object bodies_reg = symbol_undefined;
    public static object buffer_reg = symbol_undefined;
    public static object cdrs_reg = symbol_undefined;
    public static object char_reg = symbol_undefined;
    public static object chars_reg = symbol_undefined;
    public static object clauses_reg = symbol_undefined;
    public static object components_reg = symbol_undefined;
    public static object contours_reg = symbol_undefined;
    public static object datum_reg = symbol_undefined;
    public static object depth_reg = symbol_undefined;
    public static object dk_reg = symbol_undefined;
    public static object env2_reg = symbol_undefined;
    public static object env_reg = symbol_undefined;
    public static object exception_reg = symbol_undefined;
    public static object exp_reg = symbol_undefined;
    public static object expected_terminator_reg = symbol_undefined;
    public static object exps_reg = symbol_undefined;
    public static object fail_reg = symbol_undefined;
    public static object fields_reg = symbol_undefined;
    public static object filename_reg = symbol_undefined;
    public static object filenames_reg = symbol_undefined;
    public static object final_reg = symbol_undefined;
    public static object frames_reg = symbol_undefined;
    public static object generator_reg = symbol_undefined;
    public static object gk_reg = symbol_undefined;
    public static object handler_reg = symbol_undefined;
    public static object i_reg = symbol_undefined;
    public static object id_reg = symbol_undefined;
    public static object info_reg = symbol_undefined;
    public static object input_reg = symbol_undefined;
    public static object iterator_reg = symbol_undefined;
    public static object k2_reg = symbol_undefined;
    public static object k_reg = symbol_undefined;
    public static object keyword_reg = symbol_undefined;
    public static object line_reg = symbol_undefined;
    public static object list1_reg = symbol_undefined;
    public static object list2_reg = symbol_undefined;
    public static object lists_reg = symbol_undefined;
    public static object ls1_reg = symbol_undefined;
    public static object ls2_reg = symbol_undefined;
    public static object ls_reg = symbol_undefined;
    public static object lst_reg = symbol_undefined;
    public static object macro_reg = symbol_undefined;
    public static object module_reg = symbol_undefined;
    public static object msg_reg = symbol_undefined;
    public static object name_reg = symbol_undefined;
    public static object offset_reg = symbol_undefined;
    public static object p1_reg = symbol_undefined;
    public static object p2_reg = symbol_undefined;
    public static object pair1_reg = symbol_undefined;
    public static object pair2_reg = symbol_undefined;
    public static object path_reg = symbol_undefined;
    public static object pattern_reg = symbol_undefined;
    public static object proc_reg = symbol_undefined;
    public static object procs_reg = symbol_undefined;
    public static object s_reg = symbol_undefined;
    public static object senv_reg = symbol_undefined;
    public static object sexps_reg = symbol_undefined;
    public static object sk_reg = symbol_undefined;
    public static object src_reg = symbol_undefined;
    public static object sum_reg = symbol_undefined;
    public static object token_type_reg = symbol_undefined;
    public static object tokens_reg = symbol_undefined;
    public static object v1_reg = symbol_undefined;
    public static object v2_reg = symbol_undefined;
    public static object value1_reg = symbol_undefined;
    public static object value2_reg = symbol_undefined;
    public static object value3_reg = symbol_undefined;
    public static object value4_reg = symbol_undefined;
    public static object value_reg = symbol_undefined;
    public static object var_info_reg = symbol_undefined;
    public static object var_reg = symbol_undefined;
    public static object variant_reg = symbol_undefined;
    public static object variants_reg = symbol_undefined;
    public static object vars_reg = symbol_undefined;
    public static object x_reg = symbol_undefined;
    public static object y_reg = symbol_undefined;
    public static object temp_2 = symbol_undefined;
    public static object temp_3 = symbol_undefined;
    public static object temp_4 = symbol_undefined;
    public static object temp_1 = symbol_undefined;
    public static void apply_cont() {
        ApplyPlus(cadr(k_reg), cddr(k_reg));
    }
    
    public static void b_cont_1_d(object chars, object fail, object k) {
        value3_reg = fail;
        value2_reg = chars;
        value1_reg = value_reg;
        k_reg = k;
        pc = apply_cont3;
    }
    
    public static void b_cont_2_d(object v1, object info, object k) {
        value_reg = sList(pair_tag, v1, value_reg, info);
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_3_d(object x, object info, object k) {
        k_reg = make_cont("cont", 2, value_reg, info, k);
        info_reg = symbol_none;
        x_reg = cdr(x);
        pc = annotate_cps;
    }
    
    public static void b_cont_4_d(object k) {
        value_reg = list_to_vector(value_reg);
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_5_d(object v1, object k) {
        value_reg = cons(v1, value_reg);
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_6_d(object x, object k) {
        k_reg = make_cont("cont", 5, value_reg, k);
        x_reg = cdr(x);
        pc = unannotate_cps;
    }
    
    public static void b_cont_7_d(object x, object k) {
        k_reg = make_cont("cont", 5, value_reg, k);
        x_reg = caddr(x);
        pc = unannotate_cps;
    }
    
    public static void b_cont_8_d(object end, object tokens_left, object fail, object k) {
        value4_reg = fail;
        value3_reg = tokens_left;
        value2_reg = end;
        value1_reg = value_reg;
        k_reg = k;
        pc = apply_cont4;
    }
    
    public static void b_cont_9_d(object end, object tokens, object fail, object k) {
        value4_reg = fail;
        value3_reg = rest_of(tokens);
        value2_reg = end;
        value1_reg = value_reg;
        k_reg = k;
        pc = apply_cont4;
    }
    
    public static void b_cont_10_d(object src, object start, object tokens, object handler, object fail, object k) {
        k_reg = make_cont4("cont4", 3, src, start, value_reg, k);
        fail_reg = fail;
        handler_reg = handler;
        src_reg = src;
        tokens_reg = rest_of(tokens);
        pc = read_sexp;
    }
    
    public static void b_cont_11_d() {
        final_reg = value_reg;
        pc = pc_halt_signal;
    }
    
    public static void b_cont_12_d(object adatum, object senv, object info, object handler, object fail, object k) {
        object formals_list = symbol_undefined;
        object name = symbol_undefined;
        name = untag_atom_hat(cadr_hat(adatum));
        formals_list = (list_q(value_reg) ? value_reg : cons(last(value_reg), head(value_reg)));
        k_reg = make_cont2("cont2", 16, name, value_reg, info, k);
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = cons(formals_list, senv);
        adatum_list_reg = cdddr_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont_13_d(object adatum, object senv, object info, object handler, object fail, object k) {
        object formals_list = symbol_undefined;
        formals_list = (list_q(value_reg) ? value_reg : cons(last(value_reg), head(value_reg)));
        k_reg = make_cont2("cont2", 17, value_reg, info, k);
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = cons(formals_list, senv);
        adatum_list_reg = cddr_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont_14_d(object aclauses, object name, object info, object fail, object k) {
        value2_reg = fail;
        value1_reg = define_syntax_aexp(name, value_reg, aclauses, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont_15_d(object senv, object info, object handler, object fail, object k) {
        k_reg = k;
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = replace_info(value_reg, info);
        pc = aparse;
    }
    
    public static void b_cont_16_d(object senv, object info, object handler, object fail, object k) {
        k_reg = make_cont("cont", 15, senv, info, handler, fail, k);
        info_reg = symbol_none;
        x_reg = value_reg;
        pc = annotate_cps;
    }
    
    public static void b_cont_17_d(object adatum, object senv, object info, object handler, object fail, object k) {
        if (true_q(original_source_info_q(adatum))) {
            k_reg = k;
            fail_reg = fail;
            handler_reg = handler;
            senv_reg = senv;
            adatum_reg = replace_info(value_reg, snoc(symbol_quasiquote, info));
            pc = aparse;
        } else {
            k_reg = k;
            fail_reg = fail;
            handler_reg = handler;
            senv_reg = senv;
            adatum_reg = replace_info(value_reg, info);
            pc = aparse;
        }
    }
    
    public static void b_cont_18_d(object adatum, object senv, object info, object handler, object fail, object k) {
        k_reg = make_cont("cont", 17, adatum, senv, info, handler, fail, k);
        info_reg = symbol_none;
        x_reg = value_reg;
        pc = annotate_cps;
    }
    
    public static void b_cont_19_d(object info, object fail, object k) {
        value2_reg = fail;
        value1_reg = lit_aexp(cadr(value_reg), info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont_20_d(object info, object fail, object k) {
        value2_reg = fail;
        value1_reg = lit_aexp(value_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont_21_d(object msg, object info, object handler, object fail) {
        fail_reg = fail;
        exception_reg = make_exception("ParseError", format("~s ~a", msg, value_reg), get_srcfile(info), get_start_line(info), get_start_char(info));
        handler_reg = handler;
        pc = apply_handler2;
    }
    
    public static void b_cont_22_d(object bindings, object k) {
        value_reg = append(sList(symbol_let), append(sList(sList(car_hat(bindings))), sList(value_reg)));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_23_d(object clauses, object var, object k) {
        object clause = symbol_undefined;
        clause = car_hat(clauses);
        if (true_q(eq_q_hat(car_hat(clause), symbol_else))) {
            value_reg = cons(clause, value_reg);
            k_reg = k;
            pc = apply_cont;
        } else {
            if (true_q(symbol_q_hat(car_hat(clause)))) {
                value_reg = cons(append(sList(append(sList(symbol_eq_q), append(sList(var), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg);
                k_reg = k;
                pc = apply_cont;
            } else {
                value_reg = cons(append(sList(append(sList(symbol_memq), append(sList(var), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg);
                k_reg = k;
                pc = apply_cont;
            }
        }
    }
    
    public static void b_cont_24_d(object fields, object name, object k2) {
        object constructor_def = symbol_undefined;
        constructor_def = append(sList(symbol_define), append(sList(name), sList(append(sList(symbol_lambda), append(sList(symbol_args), sList(append(sList(symbol_if), append(sList(append(sList(symbol_Equal), append(sList(append(sList(symbol_length), sList(symbol_args))), sList(length_hat(fields))))), append(sList(value_reg), sList(append(sList(symbol_error), append(sList(append(sList(symbol_quote), sList(name))), sList("wrong number of arguments")))))))))))));
        value2_reg = constructor_def;
        value1_reg = name;
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont_25_d(object cdrs, object fields, object name, object k) {
        value_reg = append(sList(symbol_if), append(sList(append(sList(cadar_hat(fields)), sList(append(sList(symbol_car), sList(cdrs))))), append(sList(value_reg), sList(append(sList(symbol_error), append(sList(append(sList(symbol_quote), sList(name))), append(sList("~a is not of type ~a"), append(sList(append(sList(symbol_car), sList(cdrs))), sList(append(sList(symbol_quote), sList(cadar_hat(fields))))))))))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_26_d(object adatum, object macro_keyword, object fail, object k) {
        if (true_q(has_source_info_q(value_reg))) {
            value2_reg = fail;
            value1_reg = value_reg;
            k_reg = k;
            pc = apply_cont2;
        } else {
            object info = symbol_undefined;
            info = get_source_info(adatum);
            if (true_q(original_source_info_q(adatum))) {
                value2_reg = fail;
                value1_reg = replace_info(value_reg, snoc(macro_keyword, info));
                k_reg = k;
                pc = apply_cont2;
            } else {
                value2_reg = fail;
                value1_reg = replace_info(value_reg, info);
                k_reg = k;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_cont_27_d(object adatum, object macro_keyword, object fail, object k) {
        k_reg = make_cont("cont", 26, adatum, macro_keyword, fail, k);
        info_reg = symbol_none;
        x_reg = value_reg;
        pc = annotate_cps;
    }
    
    public static void b_cont_28_d(object aclauses, object adatum, object clauses, object right_apattern, object right_pattern, object handler, object fail, object k) {
        if (true_q(value_reg)) {
            k2_reg = make_cont2("cont2", 48, fail, k);
            ap_reg = right_apattern;
            s_reg = value_reg;
            pattern_reg = right_pattern;
            pc = instantiate_hat;
        } else {
            k_reg = k;
            fail_reg = fail;
            handler_reg = handler;
            adatum_reg = adatum;
            aclauses_reg = cdr_hat(aclauses);
            clauses_reg = cdr(clauses);
            pc = process_macro_clauses_hat;
        }
    }
    
    public static void b_cont_29_d(object aclauses, object adatum, object clauses, object left_apattern, object left_pattern, object right_apattern, object right_pattern, object handler, object fail, object k) {
        k_reg = make_cont("cont", 28, aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k);
        ap2_reg = adatum;
        ap1_reg = left_apattern;
        p2_reg = value_reg;
        p1_reg = left_pattern;
        pc = unify_patterns_hat;
    }
    
    public static void b_cont_30_d(object v1, object k) {
        value_reg = append(sList(symbol_append), append(sList(v1), sList(value_reg)));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_31_d(object ax, object depth, object k) {
        k_reg = make_cont("cont", 30, value_reg, k);
        depth_reg = depth;
        ax_reg = cdr_hat(ax);
        pc = qq_expand_cps;
    }
    
    public static void b_cont_32_d(object k) {
        value_reg = append(sList(symbol_list_to_vector), sList(value_reg));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_33_d(object depth, object k) {
        k_reg = make_cont("cont", 32, k);
        depth_reg = depth;
        ax_reg = value_reg;
        pc = qq_expand_cps;
    }
    
    public static void b_cont_34_d(object ax, object k) {
        value_reg = append(sList(symbol_cons), append(sList(append(sList(symbol_quote), sList(car_hat(ax)))), sList(value_reg)));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_35_d(object k) {
        value_reg = append(sList(symbol_cons), append(sList(append(sList(symbol_quote), sList(symbol_quasiquote))), sList(value_reg)));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_36_d(object v1, object k) {
        value_reg = append(sList(symbol_sList), sList(append(sList(symbol_append), append(sList(v1), sList(value_reg)))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_37_d(object ax, object depth, object k) {
        k_reg = make_cont("cont", 36, value_reg, k);
        depth_reg = depth;
        ax_reg = cdr_hat(ax);
        pc = qq_expand_cps;
    }
    
    public static void b_cont_38_d(object k) {
        value_reg = append(sList(symbol_sList), sList(value_reg));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_39_d(object ax, object k) {
        value_reg = append(sList(symbol_sList), sList(append(sList(symbol_cons), append(sList(append(sList(symbol_quote), sList(car_hat(ax)))), sList(value_reg)))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_40_d(object k) {
        value_reg = append(sList(symbol_sList), sList(append(sList(symbol_cons), append(sList(append(sList(symbol_quote), sList(symbol_quasiquote))), sList(value_reg)))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont_41_d(object args, object handler, object fail, object k2) {
        k_reg = make_cont2("cont2", 76, args, handler, k2);
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = initial_contours(cadr(args));
        adatum_reg = value_reg;
        pc = aparse;
    }
    
    public static void b_cont_42_d(object handler, object fail, object k2) {
        k_reg = make_cont2("cont2", 77, handler, k2);
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = initial_contours(toplevel_env);
        adatum_reg = value_reg;
        pc = aparse;
    }
    
    public static void b_cont_43_d(object handler, object fail, object k2) {
        k_reg = k2;
        fail_reg = fail;
        handler_reg = handler;
        senv_reg = initial_contours(toplevel_env);
        adatum_reg = value_reg;
        pc = aparse;
    }
    
    public static void b_cont_44_d(object fail, object k2) {
        value2_reg = fail;
        value1_reg = value_reg;
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont_45_d(object x, object y, object k) {
        if (true_q(value_reg)) {
            k_reg = k;
            y_reg = cdr(y);
            x_reg = cdr(x);
            pc = equal_objects_q;
        } else {
            value_reg = false;
            k_reg = k;
            pc = apply_cont;
        }
    }
    
    public static void b_cont_46_d(object i, object v1, object v2, object k) {
        if (true_q(value_reg)) {
            k_reg = k;
            i_reg = Subtract(i, 1);
            v2_reg = v2;
            v1_reg = v1;
            pc = equal_vectors_q;
        } else {
            value_reg = false;
            k_reg = k;
            pc = apply_cont;
        }
    }
    
    public static void b_cont_47_d(object ls, object x, object y, object info, object handler, object fail, object k) {
        if (true_q(value_reg)) {
            value2_reg = fail;
            value1_reg = y;
            k_reg = k;
            pc = apply_cont2;
        } else {
            k_reg = k;
            fail_reg = fail;
            handler_reg = handler;
            info_reg = info;
            ls_reg = ls;
            y_reg = cdr(y);
            x_reg = x;
            pc = member_loop;
        }
    }
    
    public static void b_cont_48_d(object pattern, object var, object k) {
        if (true_q(value_reg)) {
            value_reg = true;
            k_reg = k;
            pc = apply_cont;
        } else {
            k_reg = k;
            pattern_reg = cdr(pattern);
            var_reg = var;
            pc = occurs_q;
        }
    }
    
    public static void b_cont_49_d(object ap2, object p1, object p2, object k) {
        if (true_q(value_reg)) {
            value_reg = false;
            k_reg = k;
            pc = apply_cont;
        } else {
            value_reg = make_sub(symbol_unit, p1, p2, ap2);
            k_reg = k;
            pc = apply_cont;
        }
    }
    
    public static void b_cont_50_d(object s_car, object k) {
        if (true_q((! true_q(value_reg)))) {
            value_reg = false;
            k_reg = k;
            pc = apply_cont;
        } else {
            value_reg = make_sub(symbol_composite, s_car, value_reg);
            k_reg = k;
            pc = apply_cont;
        }
    }
    
    public static void b_cont_51_d(object apair1, object apair2, object pair1, object pair2, object k) {
        if (true_q((! true_q(value_reg)))) {
            value_reg = false;
            k_reg = k;
            pc = apply_cont;
        } else {
            k2_reg = make_cont2("cont2", 100, apair2, pair2, value_reg, k);
            ap_reg = cdr_hat(apair1);
            s_reg = value_reg;
            pattern_reg = cdr(pair1);
            pc = instantiate_hat;
        }
    }
    
    public static void apply_cont2() {
        ApplyPlus(cadr(k_reg), cddr(k_reg));
    }
    
    public static void b_cont2_1_d(object token, object k) {
        value1_reg = cons(token, value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_2_d() {
        final_reg = value1_reg;
        pc = pc_halt_signal;
    }
    
    public static void b_cont2_3_d(object k) {
        value1_reg = binding_value(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_4_d(object k) {
        value1_reg = dlr_env_lookup(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_5_d(object v1, object info, object k) {
        value1_reg = app_aexp(v1, value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_6_d(object adatum, object senv, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 5, value1_reg, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_list_reg = cdr_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont2_7_d(object info, object k) {
        value1_reg = choose_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_8_d(object info, object k) {
        value1_reg = raise_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_9_d(object cexps, object cvar, object body, object info, object k) {
        value1_reg = try_catch_finally_aexp(body, cvar, cexps, value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_10_d(object adatum, object cvar, object senv, object body, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 9, value1_reg, cvar, body, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_list_reg = try_catch_finally_exps_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont2_11_d(object adatum, object senv, object info, object handler, object k) {
        object cvar = symbol_undefined;
        cvar = catch_var_hat(adatum);
        k_reg = make_cont2("cont2", 10, adatum, cvar, senv, value1_reg, info, handler, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = cons(sList(cvar), senv);
        adatum_list_reg = catch_exps_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont2_12_d(object body, object info, object k) {
        value1_reg = try_finally_aexp(body, value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_13_d(object adatum, object senv, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 12, value1_reg, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_list_reg = try_finally_exps_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont2_14_d(object cvar, object body, object info, object k) {
        value1_reg = try_catch_aexp(body, cvar, value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_15_d(object adatum, object senv, object info, object handler, object k) {
        object cvar = symbol_undefined;
        cvar = catch_var_hat(adatum);
        k_reg = make_cont2("cont2", 14, cvar, value1_reg, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = cons(sList(cvar), senv);
        adatum_list_reg = catch_exps_hat(adatum);
        pc = aparse_all;
    }
    
    public static void b_cont2_16_d(object name, object formals, object info, object k) {
        if (true_q(list_q(formals))) {
            value1_reg = trace_lambda_aexp(name, formals, value1_reg, info);
            k_reg = k;
            pc = apply_cont2;
        } else {
            value1_reg = mu_trace_lambda_aexp(name, head(formals), last(formals), value1_reg, info);
            k_reg = k;
            pc = apply_cont2;
        }
    }
    
    public static void b_cont2_17_d(object formals, object info, object k) {
        if (true_q(list_q(formals))) {
            value1_reg = lambda_aexp(formals, value1_reg, info);
            k_reg = k;
            pc = apply_cont2;
        } else {
            value1_reg = mu_lambda_aexp(head(formals), last(formals), value1_reg, info);
            k_reg = k;
            pc = apply_cont2;
        }
    }
    
    public static void b_cont2_18_d(object info, object k) {
        value1_reg = begin_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_19_d(object adatum, object info, object k) {
        value1_reg = define_b_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_20_d(object adatum, object info, object k) {
        value1_reg = define_b_aexp(define_var_hat(adatum), "", value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_21_d(object adatum, object info, object k) {
        value1_reg = define_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_22_d(object adatum, object info, object k) {
        value1_reg = define_aexp(define_var_hat(adatum), "", value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_23_d(object info, object k) {
        value1_reg = callback2_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_24_d(object info, object k) {
        value1_reg = callback1_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_25_d(object info, object k) {
        value1_reg = callback0_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_26_d(object info, object k) {
        value1_reg = func_aexp(value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_27_d(object adatum, object info, object k) {
        object var_info = symbol_undefined;
        var_info = get_source_info(cadr_hat(adatum));
        value1_reg = assign_aexp(untag_atom_hat(cadr_hat(adatum)), value1_reg, var_info, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_28_d(object v1, object v2, object info, object k) {
        value1_reg = if_aexp(v1, v2, value1_reg, info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_29_d(object adatum, object senv, object v1, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 28, v1, value1_reg, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = cadddr_hat(adatum);
        pc = aparse;
    }
    
    public static void b_cont2_30_d(object adatum, object senv, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 29, adatum, senv, value1_reg, info, handler, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = caddr_hat(adatum);
        pc = aparse;
    }
    
    public static void b_cont2_31_d(object v1, object info, object k) {
        value1_reg = if_aexp(v1, value1_reg, lit_aexp(false, symbol_none), info);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_32_d(object adatum, object senv, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 31, value1_reg, info, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = caddr_hat(adatum);
        pc = aparse;
    }
    
    public static void b_cont2_33_d(object senv, object handler, object k) {
        k_reg = k;
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = value1_reg;
        pc = aparse;
    }
    
    public static void b_cont2_34_d(object a, object k) {
        value1_reg = cons(a, value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_35_d(object adatum_list, object senv, object handler, object k) {
        k_reg = make_cont2("cont2", 34, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_list_reg = cdr_hat(adatum_list);
        pc = aparse_all;
    }
    
    public static void b_cont2_36_d(object v1, object k) {
        value1_reg = cons(v1, value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_37_d(object senv, object src, object tokens_left, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        senv_reg = senv;
        src_reg = src;
        tokens_reg = tokens_left;
        pc = aparse_sexps;
    }
    
    public static void b_cont2_38_d(object bodies, object k) {
        value_reg = append(sList(symbol_let), append(sList(value1_reg), append(value2_reg, at_hat(bodies))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont2_39_d(object procs, object vars, object k2) {
        value2_reg = cons(append(sList(symbol_set_b), append(sList(car_hat(vars)), sList(car_hat(procs)))), value2_reg);
        value1_reg = cons(append(sList(car_hat(vars)), sList(append(sList(symbol_quote), sList(symbol_undefined)))), value1_reg);
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont2_40_d(object exp, object k) {
        value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_r), sList(exp))), value1_reg)), sList(append(sList(symbol_cond), value2_reg))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont2_41_d(object clauses, object var, object k2) {
        object clause = symbol_undefined;
        clause = car_hat(clauses);
        if (true_q(eq_q_hat(car_hat(clause), symbol_else))) {
            value2_reg = cons(sList(symbol_else, sList(symbol_else_code)), value2_reg);
            value1_reg = cons(append(sList(symbol_else_code), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg);
            k_reg = k2;
            pc = apply_cont2;
        } else {
            if (true_q(symbol_q_hat(car_hat(clause)))) {
                object name = symbol_undefined;
                name = car_hat(clause);
                value2_reg = cons(append(sList(append(sList(symbol_eq_q), append(sList(var), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), sList(sList(name))), value2_reg);
                value1_reg = cons(append(sList(name), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg);
                k_reg = k2;
                pc = apply_cont2;
            } else {
                object name = symbol_undefined;
                name = caar_hat(clause);
                value2_reg = cons(append(sList(append(sList(symbol_memq), append(sList(var), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), sList(sList(name))), value2_reg);
                value1_reg = cons(append(sList(name), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg);
                k_reg = k2;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_cont2_42_d(object clauses, object var, object k2) {
        object clause = symbol_undefined;
        clause = car_hat(clauses);
        if (true_q(eq_q_hat(car_hat(clause), symbol_else))) {
            value2_reg = cons(append(sList(symbol_else), sList(sList(symbol_else_code))), value2_reg);
            value1_reg = cons(append(sList(symbol_else_code), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg);
            k_reg = k2;
            pc = apply_cont2;
        } else {
            if (true_q(symbol_q_hat(car_hat(clause)))) {
                object name = symbol_undefined;
                name = car_hat(clause);
                value2_reg = cons(append(sList(append(sList(symbol_eq_q), append(sList(append(sList(symbol_car), sList(var))), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), sList(append(sList(symbol_apply), append(sList(name), sList(append(sList(symbol_cdr), sList(var))))))), value2_reg);
                value1_reg = cons(append(sList(name), sList(append(sList(symbol_lambda), append(sList(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg);
                k_reg = k2;
                pc = apply_cont2;
            } else {
                object name = symbol_undefined;
                name = caar_hat(clause);
                value2_reg = cons(append(sList(append(sList(symbol_memq), append(sList(append(sList(symbol_car), sList(var))), sList(append(sList(symbol_quote), sList(car_hat(clause))))))), sList(append(sList(symbol_apply), append(sList(name), sList(append(sList(symbol_cdr), sList(var))))))), value2_reg);
                value1_reg = cons(append(sList(name), sList(append(sList(symbol_lambda), append(sList(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg);
                k_reg = k2;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_cont2_43_d(object type_tester_name, object k) {
        object tester_def = symbol_undefined;
        tester_def = append(sList(symbol_define), append(sList(type_tester_name), sList(append(sList(symbol_lambda), append(sList(sList(symbol_x)), sList(append(sList(symbol_and), append(sList(append(sList(symbol_pair_q), sList(symbol_x))), sList(append(sList(symbol_not), sList(append(sList(symbol_not), sList(append(sList(symbol_memq), append(sList(append(sList(symbol_car), sList(symbol_x))), sList(append(sList(symbol_quote), sList(value1_reg))))))))))))))))));
        value_reg = append(sList(symbol_begin), append(sList(tester_def), value2_reg));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont2_44_d(object def_, object name, object k2) {
        value2_reg = cons(def_, value2_reg);
        value1_reg = cons(name, value1_reg);
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont2_45_d(object variants, object k2) {
        k2_reg = make_cont2("cont2", 44, value2_reg, value1_reg, k2);
        variants_reg = cdr_hat(variants);
        pc = make_dd_variant_constructors_hat;
    }
    
    public static void b_cont2_46_d(object exp, object type_name, object type_tester_name, object k) {
        value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_r), sList(exp))), value1_reg)), sList(append(sList(symbol_if), append(sList(append(sList(symbol_not), sList(append(sList(type_tester_name), sList(symbol_r))))), append(sList(append(sList(symbol_error), append(sList(append(sList(symbol_quote), sList(symbol_cases))), append(sList("~a is not a valid ~a"), append(sList(symbol_r), sList(append(sList(symbol_quote), sList(type_name)))))))), sList(append(sList(symbol_cond), value2_reg))))))));
        k_reg = k;
        pc = apply_cont;
    }
    
    public static void b_cont2_47_d(object macro_keyword, object k) {
        value1_reg = replace_info(value1_reg, snoc(macro_keyword, get_source_info(value1_reg)));
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_48_d(object fail, object k) {
        value1_reg = value2_reg;
        value2_reg = fail;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_49_d() {
        _starlast_fail_star = value2_reg;
        final_reg = value1_reg;
        pc = pc_halt_signal;
    }
    
    public static void b_cont2_50_d() {
        k_reg = REP_k;
        fail_reg = value2_reg;
        handler_reg = REP_handler;
        env_reg = toplevel_env;
        exp_reg = value1_reg;
        pc = m;
    }
    
    public static void b_cont2_51_d() {
        final_reg = true;
        pc = pc_halt_signal;
    }
    
    public static void b_cont2_52_d() {
        k_reg = make_cont2("cont2", 51);
        fail_reg = value2_reg;
        handler_reg = try_parse_handler;
        senv_reg = initial_contours(toplevel_env);
        src_reg = symbol_stdin;
        tokens_reg = value1_reg;
        pc = aparse_sexps;
    }
    
    public static void b_cont2_53_d(object exp, object k) {
        handle_debug_info(exp, value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_54_d(object exp, object k) {
        pop_stack_trace_b(exp);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_55_d(object args, object exp, object env, object info, object handler, object k) {
        if (true_q(_staruse_stack_trace_star)) {
            push_stack_trace_b(exp);
        }
        if (true_q(dlr_proc_q(value1_reg))) {
            object result = symbol_undefined;
            result = dlr_apply(value1_reg, args);
            if (true_q(_staruse_stack_trace_star)) {
                pop_stack_trace_b(exp);
            }
            value1_reg = result;
            k_reg = k;
            pc = apply_cont2;
        } else {
            if (true_q(procedure_object_q(value1_reg))) {
                if (true_q(_staruse_stack_trace_star)) {
                    k2_reg = make_cont2("cont2", 54, exp, k);
                    fail_reg = value2_reg;
                    handler_reg = handler;
                    info_reg = info;
                    env2_reg = env;
                    args_reg = args;
                    proc_reg = value1_reg;
                    pc = apply_proc;
                } else {
                    k2_reg = k;
                    fail_reg = value2_reg;
                    handler_reg = handler;
                    info_reg = info;
                    env2_reg = env;
                    args_reg = args;
                    proc_reg = value1_reg;
                    pc = apply_proc;
                }
            } else {
                fail_reg = value2_reg;
                handler_reg = handler;
                info_reg = info;
                msg_reg = format("attempt to apply non-procedure '~a'", value1_reg);
                pc = runtime_error;
            }
        }
    }
    
    public static void b_cont2_56_d(object exp, object operator_, object env, object info, object handler, object k) {
        k_reg = make_cont2("cont2", 55, value1_reg, exp, env, info, handler, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        exp_reg = operator_;
        pc = m;
    }
    
    public static void b_cont2_57_d(object handler) {
        fail_reg = value2_reg;
        exception_reg = value1_reg;
        handler_reg = handler;
        pc = apply_handler2;
    }
    
    public static void b_cont2_58_d(object v, object k) {
        value1_reg = v;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_59_d(object fexps, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 58, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        exps_reg = fexps;
        pc = eval_sequence;
    }
    
    public static void b_cont2_60_d(object aclauses, object clauses, object k) {
        set_binding_value_b(value1_reg, make_pattern_macro_hat(clauses, aclauses));
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_61_d(object docstring, object var, object k) {
        if (true_q(procedure_object_q(value1_reg))) {
            set_global_value_b(var, dlr_func(value1_reg));
        } else {
            set_global_value_b(var, value1_reg);
        }
        set_global_docstring_b(var, docstring);
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_62_d(object docstring, object rhs_value, object k) {
        set_binding_value_b(value1_reg, rhs_value);
        set_binding_docstring_b(value1_reg, docstring);
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_63_d(object docstring, object var, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 62, docstring, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        var_reg = var;
        pc = lookup_binding_in_first_frame;
    }
    
    public static void b_cont2_64_d(object rhs_value, object k) {
        object old_value = symbol_undefined;
        old_value = binding_value(value1_reg);
        set_binding_value_b(value1_reg, rhs_value);
        object new_fail = symbol_undefined;
        new_fail = make_fail("fail", 2, value1_reg, old_value, value2_reg);
        value2_reg = new_fail;
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_65_d(object rhs_value, object k) {
        object old_value = symbol_undefined;
        old_value = dlr_env_lookup(value1_reg);
        set_global_value_b(value1_reg, rhs_value);
        object new_fail = symbol_undefined;
        new_fail = make_fail("fail", 4, old_value, value1_reg, value2_reg);
        value2_reg = new_fail;
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_66_d(object var, object var_info, object env, object handler, object k) {
        sk_reg = make_cont2("cont2", 64, value1_reg, k);
        dk_reg = make_cont3("cont3", 4, value1_reg, k);
        gk_reg = make_cont2("cont2", 65, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        var_info_reg = var_info;
        env_reg = env;
        var_reg = var;
        pc = lookup_variable;
    }
    
    public static void b_cont2_67_d(object else_exp, object then_exp, object env, object handler, object k) {
        if (true_q(value1_reg)) {
            k_reg = k;
            fail_reg = value2_reg;
            handler_reg = handler;
            env_reg = env;
            exp_reg = then_exp;
            pc = m;
        } else {
            k_reg = k;
            fail_reg = value2_reg;
            handler_reg = handler;
            env_reg = env;
            exp_reg = else_exp;
            pc = m;
        }
    }
    
    public static void b_cont2_68_d(object k) {
        value1_reg = callback2(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_69_d(object k) {
        value1_reg = callback1(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_70_d(object k) {
        value1_reg = callback0(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_71_d(object k) {
        value1_reg = dlr_func(value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_72_d(object exps, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        exps_reg = cdr(exps);
        pc = m_star;
    }
    
    public static void b_cont2_73_d(object exps, object env, object handler, object k) {
        k_reg = k;
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        exps_reg = cdr(exps);
        pc = eval_sequence;
    }
    
    public static void b_cont2_74_d(object e, object handler) {
        fail_reg = value2_reg;
        exception_reg = e;
        handler_reg = handler;
        pc = apply_handler2;
    }
    
    public static void b_cont2_75_d(object trace_depth, object k2) {
        trace_depth = Subtract(trace_depth, 1);
        printf("~areturn: ~s~%", make_trace_depth_string(trace_depth), value1_reg);
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont2_76_d(object args, object handler, object k2) {
        k_reg = k2;
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = cadr(args);
        exp_reg = value1_reg;
        pc = m;
    }
    
    public static void b_cont2_77_d(object handler, object k2) {
        k_reg = k2;
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = toplevel_env;
        exp_reg = value1_reg;
        pc = m;
    }
    
    public static void b_cont2_78_d(object handler, object k2) {
        k_reg = make_cont4("cont4", 11, handler, k2);
        fail_reg = value2_reg;
        handler_reg = handler;
        src_reg = symbol_stdin;
        tokens_reg = value1_reg;
        pc = read_sexp;
    }
    
    public static void b_cont2_79_d(object handler, object k2) {
        k_reg = make_cont4("cont4", 12, handler, k2);
        fail_reg = value2_reg;
        handler_reg = handler;
        src_reg = symbol_stdin;
        tokens_reg = value1_reg;
        pc = read_sexp;
    }
    
    public static void b_cont2_80_d(object k) {
        if (true_q(null_q(load_stack))) {
            printf("WARNING: empty load-stack encountered!\n");
        } else {
            load_stack = cdr(load_stack);
        }
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_81_d(object filename, object env2, object handler, object k) {
        k_reg = make_cont2("cont2", 80, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env2_reg = env2;
        src_reg = filename;
        tokens_reg = value1_reg;
        pc = read_and_eval_asexps;
    }
    
    public static void b_cont2_82_d(object src, object tokens_left, object env2, object handler, object k) {
        if (true_q(token_type_q(first(tokens_left), symbol_end_marker))) {
            k_reg = k;
            pc = apply_cont2;
        } else {
            k_reg = k;
            fail_reg = value2_reg;
            handler_reg = handler;
            env2_reg = env2;
            src_reg = src;
            tokens_reg = tokens_left;
            pc = read_and_eval_asexps;
        }
    }
    
    public static void b_cont2_83_d(object src, object tokens_left, object env2, object handler, object k) {
        k_reg = make_cont2("cont2", 82, src, tokens_left, env2, handler, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env2;
        exp_reg = value1_reg;
        pc = m;
    }
    
    public static void b_cont2_84_d(object filenames, object env2, object info, object handler, object k) {
        k_reg = k;
        fail_reg = value2_reg;
        handler_reg = handler;
        info_reg = info;
        env2_reg = env2;
        filenames_reg = cdr(filenames);
        pc = load_files;
    }
    
    public static void b_cont2_85_d(object lst, object k2) {
        if (true_q(member(car(lst), value1_reg))) {
            k_reg = k2;
            pc = apply_cont2;
        } else {
            value1_reg = cons(car(lst), value1_reg);
            k_reg = k2;
            pc = apply_cont2;
        }
    }
    
    public static void b_cont2_86_d(object filename, object handler, object k2) {
        object module = symbol_undefined;
        module = make_toplevel_env();
        set_binding_value_b(value1_reg, module);
        k_reg = k2;
        fail_reg = value2_reg;
        handler_reg = handler;
        info_reg = symbol_none;
        env2_reg = module;
        filename_reg = filename;
        pc = load_file;
    }
    
    public static void b_cont2_87_d(object args, object sym, object info, object handler, object k) {
        if (true_q(null_q(cdr(args)))) {
            k_reg = k;
            pc = apply_cont2;
        } else {
            if (true_q((! true_q(environment_q(value1_reg))))) {
                fail_reg = value2_reg;
                handler_reg = handler;
                info_reg = info;
                msg_reg = format("invalid module '~a'", sym);
                pc = runtime_error;
            } else {
                k_reg = k;
                fail_reg = value2_reg;
                handler_reg = handler;
                info_reg = info;
                env_reg = value1_reg;
                args_reg = cdr(args);
                pc = get_primitive;
            }
        }
    }
    
    public static void b_cont2_88_d(object ls1, object k2) {
        value1_reg = cons(car(ls1), value1_reg);
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont2_89_d(object lists, object k2) {
        k2_reg = k2;
        fail_reg = value2_reg;
        ls2_reg = value1_reg;
        ls1_reg = car(lists);
        pc = append2;
    }
    
    public static void b_cont2_90_d(object iterator, object proc, object env, object handler, object k) {
        k_reg = k;
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        iterator_reg = iterator;
        proc_reg = proc;
        pc = iterate_continue;
    }
    
    public static void b_cont2_91_d(object iterator, object proc, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        iterator_reg = iterator;
        proc_reg = proc;
        pc = iterate_collect_continue;
    }
    
    public static void b_cont2_92_d(object list1, object proc, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        list1_reg = cdr(list1);
        proc_reg = proc;
        pc = map1;
    }
    
    public static void b_cont2_93_d(object list1, object proc, object k) {
        value1_reg = cons(dlr_apply(proc, sList(car(list1))), value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_94_d(object list1, object list2, object proc, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        list2_reg = cdr(list2);
        list1_reg = cdr(list1);
        proc_reg = proc;
        pc = map2;
    }
    
    public static void b_cont2_95_d(object list1, object list2, object proc, object k) {
        value1_reg = cons(dlr_apply(proc, sList(car(list1), car(list2))), value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_96_d(object lists, object proc, object env, object handler, object k) {
        k_reg = make_cont2("cont2", 36, value1_reg, k);
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        lists_reg = map(cdr_proc, lists);
        proc_reg = proc;
        pc = mapN;
    }
    
    public static void b_cont2_97_d(object lists, object proc, object k) {
        value1_reg = cons(dlr_apply(proc, map(car_proc, lists)), value1_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont2_98_d(object arg_list, object proc, object env, object handler, object k) {
        k_reg = k;
        fail_reg = value2_reg;
        handler_reg = handler;
        env_reg = env;
        lists_reg = map(cdr_proc, arg_list);
        proc_reg = proc;
        pc = for_each_primitive;
    }
    
    public static void b_cont2_99_d(object new_acdr1, object new_cdr1, object s_car, object k) {
        k_reg = make_cont("cont", 50, s_car, k);
        ap2_reg = value2_reg;
        ap1_reg = new_acdr1;
        p2_reg = value1_reg;
        p1_reg = new_cdr1;
        pc = unify_patterns_hat;
    }
    
    public static void b_cont2_100_d(object apair2, object pair2, object s_car, object k) {
        k2_reg = make_cont2("cont2", 99, value2_reg, value1_reg, s_car, k);
        ap_reg = cdr_hat(apair2);
        s_reg = s_car;
        pattern_reg = cdr(pair2);
        pc = instantiate_hat;
    }
    
    public static void b_cont2_101_d(object a, object aa, object ap, object k2) {
        value2_reg = cons_hat(aa, value2_reg, get_source_info(ap));
        value1_reg = cons(a, value1_reg);
        k_reg = k2;
        pc = apply_cont2;
    }
    
    public static void b_cont2_102_d(object ap, object pattern, object s, object k2) {
        k2_reg = make_cont2("cont2", 101, value1_reg, value2_reg, ap, k2);
        ap_reg = cdr_hat(ap);
        s_reg = s;
        pattern_reg = cdr(pattern);
        pc = instantiate_hat;
    }
    
    public static void b_cont2_103_d(object s2, object k2) {
        k2_reg = k2;
        ap_reg = value2_reg;
        s_reg = s2;
        pattern_reg = value1_reg;
        pc = instantiate_hat;
    }
    
    public static void apply_cont3() {
        ApplyPlus(cadr(k_reg), cddr(k_reg));
    }
    
    public static void b_cont3_1_d(object src, object handler, object k) {
        if (true_q(token_type_q(value1_reg, symbol_end_marker))) {
            value2_reg = value3_reg;
            value1_reg = sList(value1_reg);
            k_reg = k;
            pc = apply_cont2;
        } else {
            k_reg = make_cont2("cont2", 1, value1_reg, k);
            fail_reg = value3_reg;
            handler_reg = handler;
            src_reg = src;
            chars_reg = value2_reg;
            pc = scan_input_loop;
        }
    }
    
    public static void b_cont3_2_d() {
        final_reg = value1_reg;
        pc = pc_halt_signal;
    }
    
    public static void b_cont3_3_d(object k) {
        value1_reg = get_external_member(value1_reg, value2_reg);
        value2_reg = value3_reg;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_cont3_4_d(object rhs_value, object k) {
        object old_value = symbol_undefined;
        old_value = get_external_member(value1_reg, value2_reg);
        set_external_member_b(value1_reg, value2_reg, rhs_value);
        object new_fail = symbol_undefined;
        new_fail = make_fail("fail", 3, value2_reg, value1_reg, old_value, value3_reg);
        value2_reg = new_fail;
        value1_reg = void_value;
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void apply_cont4() {
        ApplyPlus(cadr(k_reg), cddr(k_reg));
    }
    
    public static void b_cont4_1_d(object src, object start, object k) {
        k_reg = make_cont("cont", 8, value2_reg, value3_reg, value4_reg, k);
        info_reg = make_info(src, start, value2_reg);
        x_reg = list_to_vector(value1_reg);
        pc = annotate_cps;
    }
    
    public static void b_cont4_2_d(object src, object start, object k) {
        k_reg = make_cont("cont", 8, value2_reg, value3_reg, value4_reg, k);
        info_reg = make_info(src, start, value2_reg);
        x_reg = value1_reg;
        pc = annotate_cps;
    }
    
    public static void b_cont4_3_d(object src, object start, object v, object k) {
        k_reg = make_cont("cont", 8, value2_reg, value3_reg, value4_reg, k);
        info_reg = make_info(src, start, value2_reg);
        x_reg = sList(v, value1_reg);
        pc = annotate_cps;
    }
    
    public static void b_cont4_4_d(object sexp1, object k) {
        value1_reg = cons(sexp1, value1_reg);
        k_reg = k;
        pc = apply_cont4;
    }
    
    public static void b_cont4_5_d(object src, object handler, object k) {
        k_reg = make_cont4("cont4", 4, value1_reg, k);
        fail_reg = value4_reg;
        handler_reg = handler;
        src_reg = src;
        tokens_reg = value3_reg;
        pc = read_vector_sequence;
    }
    
    public static void b_cont4_6_d(object expected_terminator, object sexp1, object src, object handler, object k) {
        k_reg = k;
        fail_reg = value4_reg;
        handler_reg = handler;
        src_reg = src;
        expected_terminator_reg = expected_terminator;
        tokens_reg = value3_reg;
        sexps_reg = cons(sexp1, value1_reg);
        pc = close_sexp_sequence;
    }
    
    public static void b_cont4_7_d(object expected_terminator, object src, object handler, object k) {
        if (true_q(token_type_q(first(value3_reg), symbol_dot))) {
            k_reg = make_cont4("cont4", 6, expected_terminator, value1_reg, src, handler, k);
            fail_reg = value4_reg;
            handler_reg = handler;
            src_reg = src;
            tokens_reg = rest_of(value3_reg);
            pc = read_sexp;
        } else {
            k_reg = make_cont4("cont4", 4, value1_reg, k);
            fail_reg = value4_reg;
            handler_reg = handler;
            src_reg = src;
            expected_terminator_reg = expected_terminator;
            tokens_reg = value3_reg;
            pc = read_sexp_sequence;
        }
    }
    
    public static void b_cont4_8_d() {
        final_reg = value1_reg;
        pc = pc_halt_signal;
    }
    
    public static void b_cont4_9_d(object senv, object src, object handler, object k) {
        k_reg = make_cont2("cont2", 37, senv, src, value3_reg, handler, k);
        fail_reg = value4_reg;
        handler_reg = handler;
        senv_reg = senv;
        adatum_reg = value1_reg;
        pc = aparse;
    }
    
    public static void b_cont4_10_d() {
        _startokens_left_star = value3_reg;
        k_reg = make_cont2("cont2", 50);
        fail_reg = value4_reg;
        handler_reg = REP_handler;
        senv_reg = initial_contours(toplevel_env);
        adatum_reg = value1_reg;
        pc = aparse;
    }
    
    public static void b_cont4_11_d(object handler, object k2) {
        if (true_q(token_type_q(first(value3_reg), symbol_end_marker))) {
            k_reg = k2;
            fail_reg = value4_reg;
            handler_reg = handler;
            senv_reg = initial_contours(toplevel_env);
            adatum_reg = value1_reg;
            pc = aparse;
        } else {
            fail_reg = value4_reg;
            handler_reg = handler;
            src_reg = symbol_stdin;
            tokens_reg = value3_reg;
            msg_reg = "tokens left over";
            pc = read_error;
        }
    }
    
    public static void b_cont4_12_d(object handler, object k2) {
        if (true_q(token_type_q(first(value3_reg), symbol_end_marker))) {
            value2_reg = value4_reg;
            k_reg = k2;
            pc = apply_cont2;
        } else {
            fail_reg = value4_reg;
            handler_reg = handler;
            src_reg = symbol_stdin;
            tokens_reg = value3_reg;
            msg_reg = "tokens left over";
            pc = read_error;
        }
    }
    
    public static void b_cont4_13_d(object src, object env2, object handler, object k) {
        k_reg = make_cont2("cont2", 83, src, value3_reg, env2, handler, k);
        fail_reg = value4_reg;
        handler_reg = handler;
        senv_reg = initial_contours(env2);
        adatum_reg = value1_reg;
        pc = aparse;
    }
    
    public static void apply_fail() {
        ApplyPlus(cadr(fail_reg), cddr(fail_reg));
    }
    
    public static void b_fail_1_d() {
        final_reg = "no more choices";
        pc = pc_halt_signal;
    }
    
    public static void b_fail_2_d(object binding, object old_value, object fail) {
        set_binding_value_b(binding, old_value);
        fail_reg = fail;
        pc = apply_fail;
    }
    
    public static void b_fail_3_d(object components, object dlr_obj, object old_value, object fail) {
        set_external_member_b(dlr_obj, components, old_value);
        fail_reg = fail;
        pc = apply_fail;
    }
    
    public static void b_fail_4_d(object old_value, object var, object fail) {
        set_global_value_b(var, old_value);
        fail_reg = fail;
        pc = apply_fail;
    }
    
    public static void b_fail_5_d(object exps, object env, object handler, object fail, object k) {
        k_reg = k;
        fail_reg = fail;
        handler_reg = handler;
        env_reg = env;
        exps_reg = cdr(exps);
        pc = eval_choices;
    }
    
    public static void apply_handler() {
        ApplyPlus(cadr(handler_reg), cddr(handler_reg));
    }
    
    public static void b_handler_1_d() {
        final_reg = sList(symbol_exception, exception_reg);
        pc = pc_halt_signal;
    }
    
    public static void apply_handler2() {
        ApplyPlus(cadr(handler_reg), cddr(handler_reg));
    }
    
    public static void b_handler2_1_d() {
        final_reg = sList(symbol_exception, exception_reg);
        pc = pc_halt_signal;
    }
    
    public static void b_handler2_2_d() {
        _starlast_fail_star = fail_reg;
        final_reg = sList(symbol_exception, exception_reg);
        pc = pc_halt_signal;
    }
    
    public static void b_handler2_3_d() {
        final_reg = false;
        pc = pc_halt_signal;
    }
    
    public static void b_handler2_4_d(object cexps, object cvar, object env, object handler, object k) {
        object new_env = symbol_undefined;
        new_env = extend(env, sList(cvar), sList(exception_reg));
        k_reg = k;
        handler_reg = handler;
        env_reg = new_env;
        exps_reg = cexps;
        pc = eval_sequence;
    }
    
    public static void b_handler2_5_d(object fexps, object env, object handler) {
        k_reg = make_cont2("cont2", 74, exception_reg, handler);
        handler_reg = handler;
        env_reg = env;
        exps_reg = fexps;
        pc = eval_sequence;
    }
    
    public static void b_handler2_6_d(object cexps, object cvar, object fexps, object env, object handler, object k) {
        object new_env = symbol_undefined;
        new_env = extend(env, sList(cvar), sList(exception_reg));
        object catch_handler = symbol_undefined;
        catch_handler = try_finally_handler(fexps, env, handler);
        k_reg = make_cont2("cont2", 59, fexps, env, handler, k);
        handler_reg = catch_handler;
        env_reg = new_env;
        exps_reg = cexps;
        pc = eval_sequence;
    }
    
    public static void apply_proc() {
        ApplyPlus(cadr(proc_reg), cddr(proc_reg));
    }
    
    public static void b_proc_1_d(object bodies, object formals, object env) {
        if (true_q(Equal(length(args_reg), length(formals)))) {
            k_reg = k2_reg;
            env_reg = extend(env, formals, args_reg);
            exps_reg = bodies;
            pc = eval_sequence;
        } else {
            msg_reg = "incorrect number of arguments in application";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_2_d(object bodies, object formals, object runt, object env) {
        if (true_q(GreaterThanEqual(length(args_reg), length(formals)))) {
            object new_env = symbol_undefined;
            new_env = extend(env, cons(runt, formals), cons(list_tail(args_reg, length(formals)), list_head(args_reg, length(formals))));
            k_reg = k2_reg;
            env_reg = new_env;
            exps_reg = bodies;
            pc = eval_sequence;
        } else {
            msg_reg = "not enough arguments in application";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_3_d(object bodies, object name, object trace_depth, object formals, object env) {
        if (true_q(Equal(length(args_reg), length(formals)))) {
            printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, args_reg));
            trace_depth = Add(trace_depth, 1);
            k_reg = make_cont2("cont2", 75, trace_depth, k2_reg);
            env_reg = extend(env, formals, args_reg);
            exps_reg = bodies;
            pc = eval_sequence;
        } else {
            msg_reg = "incorrect number of arguments in application";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_4_d(object bodies, object name, object trace_depth, object formals, object runt, object env) {
        if (true_q(GreaterThanEqual(length(args_reg), length(formals)))) {
            object new_env = symbol_undefined;
            new_env = extend(env, cons(runt, formals), cons(list_tail(args_reg, length(formals)), list_head(args_reg, length(formals))));
            printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, args_reg));
            trace_depth = Add(trace_depth, 1);
            k_reg = make_cont2("cont2", 75, trace_depth, k2_reg);
            env_reg = new_env;
            exps_reg = bodies;
            pc = eval_sequence;
        } else {
            msg_reg = "not enough arguments in application";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_5_d() {
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_6_d() {
        value2_reg = fail_reg;
        value1_reg = Equal(car(args_reg), 0);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_7_d() {
        final_reg = end_of_session;
        pc = pc_halt_signal;
    }
    
    public static void b_proc_8_d() {
        if (true_q(length_one_q(args_reg))) {
            k_reg = make_cont("cont", 42, handler_reg, fail_reg, k2_reg);
            info_reg = symbol_none;
            x_reg = car(args_reg);
            pc = annotate_cps;
        } else {
            if (true_q(length_two_q(args_reg))) {
                k_reg = make_cont("cont", 41, args_reg, handler_reg, fail_reg, k2_reg);
                info_reg = symbol_none;
                x_reg = car(args_reg);
                pc = annotate_cps;
            } else {
                msg_reg = "incorrect number of arguments to eval";
                pc = runtime_error;
            }
        }
    }
    
    public static void b_proc_9_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to eval-ast";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(list_q(car(args_reg)))))) {
                msg_reg = "eval-ast called on non-abstract syntax tree argument";
                pc = runtime_error;
            } else {
                k_reg = k2_reg;
                env_reg = toplevel_env;
                exp_reg = car(args_reg);
                pc = m;
            }
        }
    }
    
    public static void b_proc_10_d() {
        k_reg = make_cont("cont", 43, handler_reg, fail_reg, k2_reg);
        info_reg = symbol_none;
        x_reg = car(args_reg);
        pc = annotate_cps;
    }
    
    public static void b_proc_11_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string-length";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(string_q(car(args_reg)))))) {
                msg_reg = "string-length called on non-string argument";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(string_length_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_12_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string-ref";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(string_q(car(args_reg)))))) {
                msg_reg = "string-ref called with non-string first argument";
                pc = runtime_error;
            } else {
                if (true_q((! true_q(number_q(cadr(args_reg)))))) {
                    msg_reg = "string-ref called with non-numberic second argument";
                    pc = runtime_error;
                } else {
                    value2_reg = fail_reg;
                    value1_reg = apply(string_ref_proc, args_reg);
                    k_reg = k2_reg;
                    pc = apply_cont2;
                }
            }
        }
    }
    
    public static void b_proc_13_d() {
        value2_reg = fail_reg;
        value1_reg = aunparse(car(args_reg));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_14_d() {
        value2_reg = fail_reg;
        value1_reg = aunparse(car(caddr(car(args_reg))));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_15_d() {
        k_reg = make_cont2("cont2", 78, handler_reg, k2_reg);
        src_reg = symbol_stdin;
        input_reg = car(args_reg);
        pc = scan_input;
    }
    
    public static void b_proc_16_d() {
        k_reg = make_cont2("cont2", 79, handler_reg, k2_reg);
        src_reg = symbol_stdin;
        input_reg = car(args_reg);
        pc = scan_input;
    }
    
    public static void b_proc_17_d() {
        object proc = symbol_undefined;
        object proc_args = symbol_undefined;
        proc_args = cadr(args_reg);
        proc = car(args_reg);
        args_reg = proc_args;
        proc_reg = proc;
        pc = apply_proc;
    }
    
    public static void b_proc_18_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to sqrt";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "sqrt called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(sqrt_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_19_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to odd?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = odd_q(car(args_reg));
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_20_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to even?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = even_q(car(args_reg));
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_21_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to quotient";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(quotient_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_22_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to remainder";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(remainder_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_23_d() {
        for_each(safe_print_proc, args_reg);
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_24_d() {
        value2_reg = fail_reg;
        value1_reg = apply(make_string_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_25_d() {
        value2_reg = fail_reg;
        value1_reg = substring(car(args_reg), cadr(args_reg), caddr(args_reg));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_26_d() {
        value2_reg = fail_reg;
        value1_reg = number_to_string(car(args_reg));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_27_d() {
        value2_reg = fail_reg;
        value1_reg = assv(car(args_reg), cadr(args_reg));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_28_d() {
        value2_reg = fail_reg;
        value1_reg = memv(car(args_reg), cadr(args_reg));
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_29_d() {
        object s = symbol_undefined;
        s = format("~a", car(args_reg));
        _starneed_newline_star = true_q((! true_q(ends_with_newline_q(s))));
        display(s);
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_30_d() {
        _starneed_newline_star = false;
        newline();
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_31_d() {
        if (true_q((! true_q(length_at_least_q(1, args_reg))))) {
            msg_reg = "incorrect number of arguments to load";
            pc = runtime_error;
        } else {
            k_reg = k2_reg;
            env2_reg = toplevel_env;
            filenames_reg = args_reg;
            pc = load_files;
        }
    }
    
    public static void b_proc_32_d() {
        if (true_q(length_one_q(args_reg))) {
            ls_reg = car(args_reg);
            sum_reg = 0;
            x_reg = car(args_reg);
            pc = length_loop;
        } else {
            msg_reg = "incorrect number of arguments to length";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_33_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = format("incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument", args_reg);
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(symbol_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_34_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to number?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(number_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_35_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to boolean?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(boolean_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_36_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(string_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_37_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(char_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_38_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char=?";
            pc = runtime_error;
        } else {
            if (true_q((true_q((! true_q(char_q(car(args_reg))))) || true_q((! true_q(char_q(cadr(args_reg)))))))) {
                msg_reg = "char=? requires arguments of type char";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(char_is__q_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_39_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char-whitespace?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(char_whitespace_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_40_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char->integer";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(char_to_integer_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_41_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to integer->char";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(integer_to_char_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_42_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char-alphabetic?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(char_alphabetic_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_43_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char-numeric?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(char_numeric_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_44_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to null?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(null_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_45_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to pair?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(pair_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_46_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cons";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(cons_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_47_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to car";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("car called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(car_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_48_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_49_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(length_at_least_q(2, car(args_reg)))))) {
                msg_reg = format("cadr called on incorrect list structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_50_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(length_at_least_q(3, car(args_reg)))))) {
                msg_reg = format("caddr called on incorrect list structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_51_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caaaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caaaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caaaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_52_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caaadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caaadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caaadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_53_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_54_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caadar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caadar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caadar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_55_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caaddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caaddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caaddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_56_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_57_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_58_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cadaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cadaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cadaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_59_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cadadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cadadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cadadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_60_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cadar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cadar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cadar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_61_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to caddar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("caddar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(caddar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_62_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cadddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cadddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cadddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_63_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdaaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdaaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdaaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_64_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdaadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdaadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdaadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_65_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_66_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdadar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdadar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdadar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_67_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdaddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdaddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdaddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_68_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_69_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_70_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cddaar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cddaar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cddaar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_71_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cddadr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cddadr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cddadr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_72_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cddar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cddar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cddar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_73_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdddar";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdddar called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdddar_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_74_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cddddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cddddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cddddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_75_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cdddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cdddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cdddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_76_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cddr";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("cddr called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(cddr_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_77_d() {
        value2_reg = fail_reg;
        value1_reg = args_reg;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_78_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to set";
            pc = runtime_error;
        } else {
            lst_reg = car(args_reg);
            pc = make_set;
        }
    }
    
    public static void b_proc_79_d() {
        if (true_q((! true_q(all_numeric_q(args_reg))))) {
            msg_reg = "+ called on non-numeric argument(s)";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(Add_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_80_d() {
        if (true_q(null_q(args_reg))) {
            msg_reg = "incorrect number of arguments to -";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "- called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(Subtract_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_81_d() {
        if (true_q((! true_q(all_numeric_q(args_reg))))) {
            msg_reg = "* called on non-numeric argument(s)";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(Multiply_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_82_d() {
        if (true_q(null_q(args_reg))) {
            msg_reg = "incorrect number of arguments to /";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "/ called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                if (true_q(member(0, cdr(args_reg)))) {
                    msg_reg = "division by zero";
                    pc = runtime_error;
                } else {
                    value2_reg = fail_reg;
                    value1_reg = apply(Divide_proc, args_reg);
                    k_reg = k2_reg;
                    pc = apply_cont2;
                }
            }
        }
    }
    
    public static void b_proc_83_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to %";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "% called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                if (true_q(Equal(cadr(args_reg), 0))) {
                    msg_reg = "modulo by zero";
                    pc = runtime_error;
                } else {
                    value2_reg = fail_reg;
                    value1_reg = apply(modulo_proc, args_reg);
                    k_reg = k2_reg;
                    pc = apply_cont2;
                }
            }
        }
    }
    
    public static void b_proc_84_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to <";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "< called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(LessThan_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_85_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to >";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "> called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(GreaterThan_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_86_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to <=";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "<= called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(LessThanEqual_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_87_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to >=";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = ">= called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(GreaterThanEqual_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_88_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to =";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "= called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(Equal_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_89_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to abs";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "abs called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(abs_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_90_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to equal?";
            pc = runtime_error;
        } else {
            k_reg = make_cont("cont", 44, fail_reg, k2_reg);
            y_reg = cadr(args_reg);
            x_reg = car(args_reg);
            pc = equal_objects_q;
        }
    }
    
    public static void b_proc_91_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to eq?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(Eq_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_92_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to memq";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(memq_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_93_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to member";
            pc = runtime_error;
        } else {
            k_reg = k2_reg;
            ls_reg = cadr(args_reg);
            y_reg = cadr(args_reg);
            x_reg = car(args_reg);
            pc = member_loop;
        }
    }
    
    public static void b_proc_94_d() {
        if (true_q((true_q(null_q(args_reg)) || true_q(length_at_least_q(4, args_reg))))) {
            msg_reg = "incorrect number of arguments to range";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(all_numeric_q(args_reg))))) {
                msg_reg = "range called on non-numeric argument(s)";
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(Range_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_95_d() {
        value2_reg = fail_reg;
        value1_reg = apply(snoc_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_96_d() {
        value2_reg = fail_reg;
        value1_reg = apply(rac_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_97_d() {
        value2_reg = fail_reg;
        value1_reg = apply(rdc_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_98_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to set-car!";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("set-car! called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(set_car_b_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_99_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to set-cdr!";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(pair_q(car(args_reg)))))) {
                msg_reg = format("set-cdr! called on non-pair ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(set_cdr_b_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_100_d() {
        object filename = symbol_undefined;
        filename = car(args_reg);
        if (true_q(null_q(cdr(args_reg)))) {
            k_reg = k2_reg;
            info_reg = symbol_none;
            filename_reg = filename;
            pc = load_file;
        } else {
            object module_name = symbol_undefined;
            module_name = cadr(args_reg);
            k_reg = make_cont2("cont2", 86, filename, handler_reg, k2_reg);
            env_reg = env2_reg;
            var_reg = module_name;
            pc = lookup_binding_in_first_frame;
        }
    }
    
    public static void b_proc_101_d() {
        value2_reg = fail_reg;
        value1_reg = car(_starstack_trace_star);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_102_d() {
        k_reg = k2_reg;
        env_reg = env2_reg;
        pc = get_primitive;
    }
    
    public static void b_proc_103_d(object k) {
        value2_reg = fail_reg;
        value1_reg = car(args_reg);
        k_reg = k;
        pc = apply_cont2;
    }
    
    public static void b_proc_104_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to call/cc";
            pc = runtime_error;
        } else {
            object proc = symbol_undefined;
            proc = car(args_reg);
            if (true_q((! true_q(procedure_object_q(proc))))) {
                msg_reg = "call/cc called with non-procedure";
                pc = runtime_error;
            } else {
                object fake_k = symbol_undefined;
                fake_k = make_proc("proc", 103, k2_reg);
                if (true_q(dlr_proc_q(proc))) {
                    value2_reg = fail_reg;
                    value1_reg = dlr_apply(proc, sList(fake_k));
                    k_reg = k2_reg;
                    pc = apply_cont2;
                } else {
                    args_reg = sList(fake_k);
                    proc_reg = proc;
                    pc = apply_proc;
                }
            }
        }
    }
    
    public static void b_proc_105_d() {
        if (true_q(null_q(args_reg))) {
            value2_reg = fail_reg;
            value1_reg = void_value;
            k_reg = REP_k;
            pc = apply_cont2;
        } else {
            value2_reg = fail_reg;
            value1_reg = car(args_reg);
            k_reg = REP_k;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_106_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to require";
            pc = runtime_error;
        } else {
            if (true_q(true_q(car(args_reg)))) {
                value2_reg = fail_reg;
                value1_reg = symbol_ok;
                k_reg = k2_reg;
                pc = apply_cont2;
            } else {
                pc = apply_fail;
            }
        }
    }
    
    public static void b_proc_107_d() {
        if (true_q((! true_q(null_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to cut";
            pc = runtime_error;
        } else {
            value2_reg = REP_fail;
            value1_reg = symbol_ok;
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_108_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to reverse";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(list_q(args_reg))))) {
                msg_reg = format("reverse called on incorrect list structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(reverse_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_109_d() {
        lists_reg = args_reg;
        pc = append_all;
    }
    
    public static void b_proc_110_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string->number";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(string_to_number_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_111_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string=?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(string_is__q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_112_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to list->vector";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(list_q(car(args_reg)))))) {
                msg_reg = format("list->vector called on incorrect list structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(list_to_vector_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_113_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to list->string";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(list_q(car(args_reg)))))) {
                msg_reg = format("list->string called on incorrect list structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                if (true_q((! true_q(all_char_q(car(args_reg)))))) {
                    msg_reg = format("list->string called on non-char list ~s", car(args_reg));
                    pc = runtime_error;
                } else {
                    value2_reg = fail_reg;
                    value1_reg = apply(list_to_string_proc, args_reg);
                    k_reg = k2_reg;
                    pc = apply_cont2;
                }
            }
        }
    }
    
    public static void b_proc_114_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to char->string";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(char_q(car(args_reg)))))) {
                msg_reg = format("char->string called on non-char item ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(char_to_string_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_115_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string->list";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(string_q(car(args_reg)))))) {
                msg_reg = format("string->list called on non-string item ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(string_to_list_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_116_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string->symbol";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(string_q(car(args_reg)))))) {
                msg_reg = format("string->symbol called on non-string item ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(string_to_symbol_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_117_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to symbol->string";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(symbol_q(car(args_reg)))))) {
                msg_reg = format("symbol->string called on non-symbol item ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(symbol_to_string_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_118_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to vector->list";
            pc = runtime_error;
        } else {
            if (true_q((! true_q(vector_q(car(args_reg)))))) {
                msg_reg = format("vector->list called on incorrect vector structure ~s", car(args_reg));
                pc = runtime_error;
            } else {
                value2_reg = fail_reg;
                value1_reg = apply(vector_to_list_proc, args_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            }
        }
    }
    
    public static void b_proc_119_d() {
        lst_reg = directory(args_reg, env2_reg);
        pc = make_set;
    }
    
    public static void b_proc_120_d() {
        value2_reg = fail_reg;
        value1_reg = get_current_time();
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_121_d() {
        k_reg = k2_reg;
        env_reg = env2_reg;
        proc_reg = car(args_reg);
        args_reg = cdr(args_reg);
        pc = map_primitive;
    }
    
    public static void b_proc_122_d() {
        k_reg = k2_reg;
        env_reg = env2_reg;
        lists_reg = cdr(args_reg);
        proc_reg = car(args_reg);
        pc = for_each_primitive;
    }
    
    public static void b_proc_123_d() {
        if (true_q(LessThan(length(args_reg), 1))) {
            msg_reg = "incorrect number of arguments to format";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(format_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_124_d() {
        value2_reg = fail_reg;
        value1_reg = env2_reg;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_125_d() {
        value2_reg = fail_reg;
        value1_reg = using_native(args_reg, env2_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_126_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to not";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = (! true_q(true_q(car(args_reg))));
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_127_d() {
        apply(printf_proc, args_reg);
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_128_d() {
        value2_reg = fail_reg;
        value1_reg = apply(vector_native_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_129_d() {
        vector_set_b(car(args_reg), cadr(args_reg), caddr(args_reg));
        value2_reg = fail_reg;
        value1_reg = void_value;
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_130_d() {
        value2_reg = fail_reg;
        value1_reg = apply(vector_ref_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_131_d() {
        value2_reg = fail_reg;
        value1_reg = apply(make_vector_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_132_d() {
        if (true_q((! true_q(length_at_least_q(1, args_reg))))) {
            msg_reg = "incorrect number of arguments to 'error' (should at least 1)";
            pc = runtime_error;
        } else {
            object location = symbol_undefined;
            object message = symbol_undefined;
            location = format("Error in '~a': ", car(args_reg));
            message = string_append(location, apply(format_proc, cdr(args_reg)));
            msg_reg = message;
            pc = runtime_error;
        }
    }
    
    public static void b_proc_133_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to list-ref";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(list_ref_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_134_d() {
        if (true_q(null_q(args_reg))) {
            value2_reg = fail_reg;
            value1_reg = current_directory();
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(length_one_q(args_reg))) {
                if (true_q(string_q(car(args_reg)))) {
                    value2_reg = fail_reg;
                    value1_reg = current_directory(car(args_reg));
                    k_reg = k2_reg;
                    pc = apply_cont2;
                } else {
                    msg_reg = "directory must be a string";
                    pc = runtime_error;
                }
            } else {
                msg_reg = "incorrect number of arguments to current-directory";
                pc = runtime_error;
            }
        }
    }
    
    public static void b_proc_135_d() {
        if (true_q((true_q(length_one_q(args_reg)) && true_q(number_q(car(args_reg)))))) {
            value2_reg = fail_reg;
            value1_reg = round(car(args_reg));
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            msg_reg = "round requires exactly one number";
            pc = runtime_error;
        }
    }
    
    public static void b_proc_136_d() {
        if (true_q((true_q(length_one_q(args_reg)) && true_q(boolean_q(car(args_reg)))))) {
            set_use_stack_trace_b(car(args_reg));
            value2_reg = fail_reg;
            value1_reg = void_value;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(null_q(args_reg))) {
                value2_reg = fail_reg;
                value1_reg = _staruse_stack_trace_star;
                k_reg = k2_reg;
                pc = apply_cont2;
            } else {
                msg_reg = "use-stack-trace requires exactly one boolean or nothing";
                pc = runtime_error;
            }
        }
    }
    
    public static void b_proc_137_d() {
        if (true_q((true_q(length_one_q(args_reg)) && true_q(boolean_q(car(args_reg)))))) {
            _startracing_on_q_star = true_q(car(args_reg));
            value2_reg = fail_reg;
            value1_reg = void_value;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(null_q(args_reg))) {
                value2_reg = fail_reg;
                value1_reg = _startracing_on_q_star;
                k_reg = k2_reg;
                pc = apply_cont2;
            } else {
                msg_reg = "use-tracing requires exactly one boolean or nothing";
                pc = runtime_error;
            }
        }
    }
    
    public static void b_proc_138_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to eqv?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(eqv_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_139_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to vector?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(vector_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_140_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to atom?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(atom_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_141_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to iter?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(iter_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_142_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to list?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(list_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_143_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to procedure?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(procedure_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_144_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string<?";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(stringLessThan_q_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_145_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to float";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(float__proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_146_d() {
        if (true_q((! true_q(null_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to globals";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(globals_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_147_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to int";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(int__proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_148_d() {
        if (true_q((! true_q(length_at_least_q(1, args_reg))))) {
            msg_reg = "incorrect number of arguments to apply-with-keywords";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(apply_with_keywords_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_149_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to assq";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(assq_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_150_d() {
        value2_reg = fail_reg;
        value1_reg = apply(dict_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_151_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to property";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(property_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_152_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to rational";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(Divide_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_153_d() {
        if (true_q((! true_q(null_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to reset-toplevel-env";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(reset_toplevel_env_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_154_d() {
        if (true_q((! true_q(length_two_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to sort";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(sort_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_155_d() {
        if (true_q((! true_q(length_at_least_q(2, args_reg))))) {
            msg_reg = "incorrect number of arguments to string-append";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(string_append_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_156_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to string-split";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(string_split_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_157_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to symbol";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(make_symbol_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_158_d() {
        if (true_q((! true_q(length_one_q(args_reg))))) {
            msg_reg = "incorrect number of arguments to typeof";
            pc = runtime_error;
        } else {
            value2_reg = fail_reg;
            value1_reg = apply(type_proc, args_reg);
            k_reg = k2_reg;
            pc = apply_cont2;
        }
    }
    
    public static void b_proc_159_d() {
        value2_reg = fail_reg;
        value1_reg = apply(use_lexical_address_proc, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void b_proc_160_d(object external_function_object) {
        value2_reg = fail_reg;
        value1_reg = dlr_apply(external_function_object, args_reg);
        k_reg = k2_reg;
        pc = apply_cont2;
    }
    
    public static void apply_macro() {
        ApplyPlus(cadr(macro_reg), cddr(macro_reg));
    }
    
    public static void b_macro_1_d() {
        if (true_q(symbol_q_hat(cadr_hat(datum_reg)))) {
            object name = symbol_undefined;
            object bindings = symbol_undefined;
            object vars = symbol_undefined;
            object exps = symbol_undefined;
            object bodies = symbol_undefined;
            name = cadr_hat(datum_reg);
            bindings = caddr_hat(datum_reg);
            vars = map_hat(car_hat_proc, bindings);
            exps = map_hat(cadr_hat_proc, bindings);
            bodies = cdddr_hat(datum_reg);
            value_reg = append(sList(symbol_letrec), append(sList(sList(append(sList(name), sList(append(sList(symbol_lambda), append(sList(vars), at_hat(bodies))))))), sList(append(sList(name), at_hat(exps)))));
            pc = apply_cont;
        } else {
            object bindings = symbol_undefined;
            object vars = symbol_undefined;
            object exps = symbol_undefined;
            object bodies = symbol_undefined;
            bindings = cadr_hat(datum_reg);
            vars = map_hat(car_hat_proc, bindings);
            exps = map_hat(cadr_hat_proc, bindings);
            bodies = cddr_hat(datum_reg);
            value_reg = append(sList(append(sList(symbol_lambda), append(sList(vars), at_hat(bodies)))), at_hat(exps));
            pc = apply_cont;
        }
    }
    
    public static void b_macro_2_d() {
        object decls = symbol_undefined;
        object vars = symbol_undefined;
        object procs = symbol_undefined;
        object bodies = symbol_undefined;
        decls = cadr_hat(datum_reg);
        vars = map_hat(car_hat_proc, decls);
        procs = map_hat(cadr_hat_proc, decls);
        bodies = cddr_hat(datum_reg);
        k2_reg = make_cont2("cont2", 38, bodies, k_reg);
        procs_reg = procs;
        vars_reg = vars;
        pc = create_letrec_assignments_hat;
    }
    
    public static void b_macro_3_d() {
        object name = symbol_undefined;
        object formals = symbol_undefined;
        object bodies = symbol_undefined;
        bodies = cddr_hat(datum_reg);
        formals = cdadr_hat(datum_reg);
        name = caadr_hat(datum_reg);
        value_reg = append(sList(symbol_define), append(sList(name), sList(append(sList(symbol_lambda), append(sList(formals), at_hat(bodies))))));
        pc = apply_cont;
    }
    
    public static void b_macro_4_d() {
        object exps = symbol_undefined;
        exps = cdr_hat(datum_reg);
        if (true_q(null_q_hat(exps))) {
            value_reg = true;
            pc = apply_cont;
        } else {
            if (true_q(null_q_hat(cdr_hat(exps)))) {
                value_reg = car_hat(exps);
                pc = apply_cont;
            } else {
                value_reg = append(sList(symbol_if), append(sList(car_hat(exps)), append(sList(append(sList(symbol_and), at_hat(cdr_hat(exps)))), sList(false))));
                pc = apply_cont;
            }
        }
    }
    
    public static void b_macro_5_d() {
        object exps = symbol_undefined;
        exps = cdr_hat(datum_reg);
        if (true_q(null_q_hat(exps))) {
            value_reg = false;
            pc = apply_cont;
        } else {
            if (true_q(null_q_hat(cdr_hat(exps)))) {
                value_reg = car_hat(exps);
                pc = apply_cont;
            } else {
                value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_bool_), sList(car_hat(exps)))), sList(append(sList(symbol_else_code), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), sList(append(sList(symbol_or), at_hat(cdr_hat(exps))))))))))), sList(append(sList(symbol_if), append(sList(symbol_bool_), append(sList(symbol_bool_), sList(sList(symbol_else_code))))))));
                pc = apply_cont;
            }
        }
    }
    
    public static void b_macro_6_d() {
        object clauses = symbol_undefined;
        clauses = cdr_hat(datum_reg);
        if (true_q(null_q_hat(clauses))) {
            adatum_reg = datum_reg;
            msg_reg = "empty (cond) expression";
            pc = amacro_error;
        } else {
            object first_clause = symbol_undefined;
            object other_clauses = symbol_undefined;
            other_clauses = cdr_hat(clauses);
            first_clause = car_hat(clauses);
            if (true_q((true_q(null_q_hat(first_clause)) || true_q((! true_q(list_q_hat(first_clause))))))) {
                adatum_reg = first_clause;
                msg_reg = "improper cond clause";
                pc = amacro_error;
            } else {
                object test_exp = symbol_undefined;
                object then_exps = symbol_undefined;
                then_exps = cdr_hat(first_clause);
                test_exp = car_hat(first_clause);
                if (true_q(eq_q_hat(test_exp, symbol_else))) {
                    if (true_q(null_q_hat(then_exps))) {
                        adatum_reg = first_clause;
                        msg_reg = "improper else clause";
                        pc = amacro_error;
                    } else {
                        if (true_q(null_q_hat(cdr_hat(then_exps)))) {
                            value_reg = car_hat(then_exps);
                            pc = apply_cont;
                        } else {
                            value_reg = append(sList(symbol_begin), at_hat(then_exps));
                            pc = apply_cont;
                        }
                    }
                } else {
                    if (true_q(null_q_hat(then_exps))) {
                        if (true_q(null_q_hat(other_clauses))) {
                            value_reg = append(sList(symbol_let), append(sList(sList(append(sList(symbol_bool_), sList(test_exp)))), sList(append(sList(symbol_if), append(sList(symbol_bool_), sList(symbol_bool_))))));
                            pc = apply_cont;
                        } else {
                            value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_bool_), sList(test_exp))), sList(append(sList(symbol_else_code), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), sList(append(sList(symbol_cond), at_hat(other_clauses)))))))))), sList(append(sList(symbol_if), append(sList(symbol_bool_), append(sList(symbol_bool_), sList(sList(symbol_else_code))))))));
                            pc = apply_cont;
                        }
                    } else {
                        if (true_q(eq_q_hat(car_hat(then_exps), symbol__is_to_))) {
                            if (true_q(null_q_hat(cdr_hat(then_exps)))) {
                                adatum_reg = first_clause;
                                msg_reg = "improper => clause";
                                pc = amacro_error;
                            } else {
                                if (true_q(null_q_hat(other_clauses))) {
                                    value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_bool_), sList(test_exp))), sList(append(sList(symbol_th), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), sList(cadr_hat(then_exps))))))))), sList(append(sList(symbol_if), append(sList(symbol_bool_), sList(append(sList(sList(symbol_th)), sList(symbol_bool_))))))));
                                    pc = apply_cont;
                                } else {
                                    value_reg = append(sList(symbol_let), append(sList(append(sList(append(sList(symbol_bool_), sList(test_exp))), append(sList(append(sList(symbol_th), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), sList(cadr_hat(then_exps))))))), sList(append(sList(symbol_else_code), sList(append(sList(symbol_lambda), append(sList(symbol_emptylist), sList(append(sList(symbol_cond), at_hat(other_clauses))))))))))), sList(append(sList(symbol_if), append(sList(symbol_bool_), append(sList(append(sList(sList(symbol_th)), sList(symbol_bool_))), sList(sList(symbol_else_code))))))));
                                    pc = apply_cont;
                                }
                            }
                        } else {
                            if (true_q(null_q_hat(other_clauses))) {
                                if (true_q(null_q_hat(cdr_hat(then_exps)))) {
                                    value_reg = append(sList(symbol_if), append(sList(test_exp), sList(car_hat(then_exps))));
                                    pc = apply_cont;
                                } else {
                                    value_reg = append(sList(symbol_if), append(sList(test_exp), sList(append(sList(symbol_begin), at_hat(then_exps)))));
                                    pc = apply_cont;
                                }
                            } else {
                                if (true_q(null_q_hat(cdr_hat(then_exps)))) {
                                    value_reg = append(sList(symbol_if), append(sList(test_exp), append(sList(car_hat(then_exps)), sList(append(sList(symbol_cond), at_hat(other_clauses))))));
                                    pc = apply_cont;
                                } else {
                                    value_reg = append(sList(symbol_if), append(sList(test_exp), append(sList(append(sList(symbol_begin), at_hat(then_exps))), sList(append(sList(symbol_cond), at_hat(other_clauses))))));
                                    pc = apply_cont;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static void b_macro_7_d() {
        object bindings = symbol_undefined;
        object bodies = symbol_undefined;
        bodies = cddr_hat(datum_reg);
        bindings = cadr_hat(datum_reg);
        bodies_reg = bodies;
        bindings_reg = bindings;
        pc = nest_let_star_bindings_hat;
    }
    
    public static void b_macro_8_d() {
        object exp = symbol_undefined;
        object clauses = symbol_undefined;
        clauses = cddr_hat(datum_reg);
        exp = cadr_hat(datum_reg);
        k2_reg = make_cont2("cont2", 40, exp, k_reg);
        clauses_reg = clauses;
        var_reg = symbol_r;
        pc = case_clauses_to_cond_clauses_hat;
    }
    
    public static void b_macro_9_d() {
        object exp = symbol_undefined;
        object clauses = symbol_undefined;
        clauses = cddr_hat(datum_reg);
        exp = cadr_hat(datum_reg);
        k2_reg = make_cont2("cont2", 40, exp, k_reg);
        clauses_reg = clauses;
        var_reg = symbol_r;
        pc = record_case_clauses_to_cond_clauses_hat;
    }
    
    public static void b_macro_10_d() {
        object datatype_name = symbol_undefined;
        object type_tester_name = symbol_undefined;
        datatype_name = cadr_hat(datum_reg);
        type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(datatype_name), "?"));
        if (true_q((! true_q(eq_q_hat(caddr_hat(datum_reg), type_tester_name))))) {
            adatum_reg = caddr_hat(datum_reg);
            msg_reg = format("datatype tester predicate not named ~a", type_tester_name);
            pc = amacro_error;
        } else {
            object variants = symbol_undefined;
            variants = cdddr_hat(datum_reg);
            k2_reg = make_cont2("cont2", 43, type_tester_name, k_reg);
            variants_reg = variants;
            pc = make_dd_variant_constructors_hat;
        }
    }
    
    public static void b_macro_11_d() {
        object type_name = symbol_undefined;
        object type_tester_name = symbol_undefined;
        object exp = symbol_undefined;
        object clauses = symbol_undefined;
        type_name = cadr_hat(datum_reg);
        type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(type_name), "?"));
        exp = caddr_hat(datum_reg);
        clauses = cdddr_hat(datum_reg);
        k2_reg = make_cont2("cont2", 46, exp, type_name, type_tester_name, k_reg);
        clauses_reg = clauses;
        var_reg = symbol_r;
        pc = record_case_clauses_to_cond_clauses_hat;
    }
    
    public static object next_avail(object n) {
        return string_ref(chars_to_scan, n);
    }
    
    public static object remaining(object n) {
        return Add(1, n);
    }
    
    public static void initialize_scan_counters() {
        scan_line = 1;
        scan_char = 1;
        scan_position = 1;
        last_scan_line = scan_line;
        last_scan_char = scan_char;
        last_scan_position = scan_position;
    }
    
    public static void increment_scan_counters(object chars) {
        last_scan_line = scan_line;
        last_scan_char = scan_char;
        last_scan_position = scan_position;
        if (true_q(char_is__q(next_avail(chars), '\n'))) {
            scan_line = Add(1, scan_line);
            scan_char = 1;
        } else {
            scan_char = Add(1, scan_char);
        }
        scan_position = Add(1, scan_position);
    }
    
    public static void mark_token_start() {
        token_start_line = scan_line;
        token_start_char = scan_char;
        token_start_position = scan_position;
    }
    
    public static void scan_input() {
        initialize_scan_counters();
        chars_to_scan = string_append(input_reg, make_string('\0'));
        chars_reg = 0;
        pc = scan_input_loop;
    }
    
    public static void scan_input_loop() {
        k_reg = make_cont3("cont3", 1, src_reg, handler_reg, k_reg);
        buffer_reg = symbol_emptylist;
        action_reg = sList(symbol_goto, symbol_start_state);
        pc = apply_action;
    }
    
    public static void apply_action() {
        if (true_q(Eq(car(action_reg), symbol_shift))) {
            object next = symbol_undefined;
            next = list_ref(action_reg, 1);
            increment_scan_counters(chars_reg);
            buffer_reg = cons(next_avail(chars_reg), buffer_reg);
            chars_reg = remaining(chars_reg);
            action_reg = next;
            pc = apply_action;
        } else {
            if (true_q(Eq(car(action_reg), symbol_replace))) {
                object new_char = symbol_undefined;
                object next = symbol_undefined;
                next = list_ref(action_reg, 2);
                new_char = list_ref(action_reg, 1);
                increment_scan_counters(chars_reg);
                chars_reg = remaining(chars_reg);
                buffer_reg = cons(new_char, buffer_reg);
                action_reg = next;
                pc = apply_action;
            } else {
                if (true_q(Eq(car(action_reg), symbol_drop))) {
                    object next = symbol_undefined;
                    next = list_ref(action_reg, 1);
                    increment_scan_counters(chars_reg);
                    chars_reg = remaining(chars_reg);
                    action_reg = next;
                    pc = apply_action;
                } else {
                    if (true_q(Eq(car(action_reg), symbol_goto))) {
                        object state = symbol_undefined;
                        state = list_ref(action_reg, 1);
                        if (true_q(Eq(state, symbol_token_start_state))) {
                            mark_token_start();
                        }
                        object action = symbol_undefined;
                        action = apply_state(state, next_avail(chars_reg));
                        if (true_q(Eq(action, symbol_error))) {
                            pc = unexpected_char_error;
                        } else {
                            action_reg = action;
                            pc = apply_action;
                        }
                    } else {
                        if (true_q(Eq(car(action_reg), symbol_emit))) {
                            object token_type = symbol_undefined;
                            token_type = list_ref(action_reg, 1);
                            k_reg = make_cont("cont", 1, chars_reg, fail_reg, k_reg);
                            token_type_reg = token_type;
                            pc = convert_buffer_to_token;
                        } else {
                            throw new Exception("symbol_apply_action: " + format("invalid action: ~a", action_reg));;
                        }
                    }
                }
            }
        }
    }
    
    public static void scan_error() {
        exception_reg = make_exception("ScanError", msg_reg, src_reg, line_reg, char_reg);
        pc = apply_handler2;
    }
    
    public static void unexpected_char_error() {
        object c = symbol_undefined;
        c = next_avail(chars_reg);
        if (true_q(char_is__q(c, '\0'))) {
            char_reg = scan_char;
            line_reg = scan_line;
            msg_reg = "unexpected end of input";
            pc = scan_error;
        } else {
            char_reg = scan_char;
            line_reg = scan_line;
            msg_reg = format("unexpected character '~a' encountered", c);
            pc = scan_error;
        }
    }
    
    public static void convert_buffer_to_token() {
        object buffer = symbol_undefined;
        buffer = reverse(buffer_reg);
        if (true_q(Eq(token_type_reg, symbol_end_marker))) {
            value_reg = make_token1(symbol_end_marker);
            pc = apply_cont;
        } else {
            if (true_q(Eq(token_type_reg, symbol_integer))) {
                value_reg = make_token2(symbol_integer, list_to_string(buffer));
                pc = apply_cont;
            } else {
                if (true_q(Eq(token_type_reg, symbol_decimal))) {
                    value_reg = make_token2(symbol_decimal, list_to_string(buffer));
                    pc = apply_cont;
                } else {
                    if (true_q(Eq(token_type_reg, symbol_rational))) {
                        value_reg = make_token2(symbol_rational, list_to_string(buffer));
                        pc = apply_cont;
                    } else {
                        if (true_q(Eq(token_type_reg, symbol_identifier))) {
                            value_reg = make_token2(symbol_identifier, string_to_symbol(list_to_string(buffer)));
                            pc = apply_cont;
                        } else {
                            if (true_q(Eq(token_type_reg, symbol_boolean))) {
                                value_reg = make_token2(symbol_boolean, (true_q(char_is__q(car(buffer), 't')) || true_q(char_is__q(car(buffer), 'T'))));
                                pc = apply_cont;
                            } else {
                                if (true_q(Eq(token_type_reg, symbol_character))) {
                                    value_reg = make_token2(symbol_character, car(buffer));
                                    pc = apply_cont;
                                } else {
                                    if (true_q(Eq(token_type_reg, symbol_named_character))) {
                                        object name = symbol_undefined;
                                        name = list_to_string(buffer);
                                        if (true_q(string_is__q(name, "nul"))) {
                                            value_reg = make_token2(symbol_character, '\0');
                                            pc = apply_cont;
                                        } else {
                                            if (true_q(string_is__q(name, "space"))) {
                                                value_reg = make_token2(symbol_character, ' ');
                                                pc = apply_cont;
                                            } else {
                                                if (true_q(string_is__q(name, "tab"))) {
                                                    value_reg = make_token2(symbol_character, '\t');
                                                    pc = apply_cont;
                                                } else {
                                                    if (true_q(string_is__q(name, "newline"))) {
                                                        value_reg = make_token2(symbol_character, '\n');
                                                        pc = apply_cont;
                                                    } else {
                                                        if (true_q(string_is__q(name, "linefeed"))) {
                                                            value_reg = make_token2(symbol_character, '\n');
                                                            pc = apply_cont;
                                                        } else {
                                                            if (true_q(string_is__q(name, "backspace"))) {
                                                                value_reg = make_token2(symbol_character, '\b');
                                                                pc = apply_cont;
                                                            } else {
                                                                if (true_q(string_is__q(name, "return"))) {
                                                                    value_reg = make_token2(symbol_character, '\r');
                                                                    pc = apply_cont;
                                                                } else {
                                                                    if (true_q(string_is__q(name, "page"))) {
                                                                        value_reg = make_token2(symbol_character, '\f');
                                                                        pc = apply_cont;
                                                                    } else {
                                                                        char_reg = token_start_char;
                                                                        line_reg = token_start_line;
                                                                        msg_reg = format("invalid character name #\\~a", name);
                                                                        pc = scan_error;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        if (true_q(Eq(token_type_reg, symbol_make_string))) {
                                            value_reg = make_token2(symbol_make_string, list_to_string(buffer));
                                            pc = apply_cont;
                                        } else {
                                            value_reg = make_token1(token_type_reg);
                                            pc = apply_cont;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static object make_token1(object token_type) {
        object start = symbol_undefined;
        object end = symbol_undefined;
        end = sList(last_scan_line, last_scan_char, last_scan_position);
        start = sList(token_start_line, token_start_char, token_start_position);
        if (true_q(Eq(token_type, symbol_end_marker))) {
            return sList(token_type, end, end);
        } else {
            return sList(token_type, start, end);
        }
    }
    
    public static object make_token2(object token_type, object token_info) {
        return sList(token_type, token_info, sList(token_start_line, token_start_char, token_start_position), sList(last_scan_line, last_scan_char, last_scan_position));
    }
    
    public static bool token_type_q(object token, object class_) {
        return Eq(car(token), class_);
    }
    
    public static object get_token_start(object token) {
        return rac(rdc(token));
    }
    
    public static object get_token_end(object token) {
        return rac(token);
    }
    
    public static object get_token_start_line(object token) {
        return car(get_token_start(token));
    }
    
    public static object get_token_start_char(object token) {
        return cadr(get_token_start(token));
    }
    
    public static object get_token_start_pos(object token) {
        return caddr(get_token_start(token));
    }
    
    public static object rac(object ls) {
        if (true_q(null_q(cdr(ls)))) {
            return car(ls);
        } else {
            object current = symbol_undefined;
            current = cdr(ls);
            while (true_q(pair_q(cdr(current)))) {
                current = cdr(current);
            }
            return car(current);
        }
    }
    
    public static object rdc(object ls) {
        if (true_q(null_q(cdr(ls)))) {
            return sList();
        } else {
            object retval = symbol_undefined;
            object front = symbol_undefined;
            object current = symbol_undefined;
            retval = sList(car(ls));
            front = retval;
            current = cdr(ls);
            while (true_q(pair_q(cdr(current)))) {
                set_cdr_b(retval, sList(car(current)));
                retval = cdr(retval);
                current = cdr(current);
            }
            return front;
        }
    }
    
    public static object snoc(object x, object ls) {
        if (true_q(null_q(ls))) {
            return sList(x);
        } else {
            object retval = symbol_undefined;
            object front = symbol_undefined;
            object current = symbol_undefined;
            retval = sList(car(ls));
            front = retval;
            current = cdr(ls);
            while (true_q(pair_q(current))) {
                set_cdr_b(retval, sList(car(current)));
                retval = cdr(retval);
                current = cdr(current);
            }
            set_cdr_b(retval, sList(x));
            return front;
        }
    }
    
    public static bool char_delimiter_q(object c) {
        return (true_q(char_whitespace_q(c)) || true_q(char_is__q(c, '\'')) || true_q(char_is__q(c, '(')) || true_q(char_is__q(c, '[')) || true_q(char_is__q(c, ')')) || true_q(char_is__q(c, ']')) || true_q(char_is__q(c, '"')) || true_q(char_is__q(c, ';')) || true_q(char_is__q(c, '#')) || true_q(char_is__q(c, '\0')));
    }
    
    public static bool char_initial_q(object c) {
        return (true_q(char_alphabetic_q(c)) || true_q(char_is__q(c, '!')) || true_q(char_is__q(c, '$')) || true_q(char_is__q(c, '%')) || true_q(char_is__q(c, '&')) || true_q(char_is__q(c, '*')) || true_q(char_is__q(c, '/')) || true_q(char_is__q(c, ':')) || true_q(char_is__q(c, '<')) || true_q(char_is__q(c, '=')) || true_q(char_is__q(c, '>')) || true_q(char_is__q(c, '?')) || true_q(char_is__q(c, '^')) || true_q(char_is__q(c, '_')) || true_q(char_is__q(c, '~')));
    }
    
    public static bool char_special_subsequent_q(object c) {
        return (true_q(char_is__q(c, '+')) || true_q(char_is__q(c, '-')) || true_q(char_is__q(c, '@')) || true_q(char_is__q(c, '.')));
    }
    
    public static bool char_subsequent_q(object c) {
        return (true_q(char_initial_q(c)) || true_q(char_numeric_q(c)) || true_q(char_special_subsequent_q(c)));
    }
    
    public static bool char_sign_q(object c) {
        return (true_q(char_is__q(c, '+')) || true_q(char_is__q(c, '-')));
    }
    
    public static bool char_boolean_q(object c) {
        return (true_q(char_is__q(c, 't')) || true_q(char_is__q(c, 'T')) || true_q(char_is__q(c, 'f')) || true_q(char_is__q(c, 'F')));
    }
    
    public static object apply_state(object state, object c) {
        if (true_q(Eq(state, symbol_start_state))) {
            if (true_q(char_whitespace_q(c))) {
                return sList(symbol_drop, sList(symbol_goto, symbol_start_state));
            } else {
                if (true_q(char_is__q(c, ';'))) {
                    return sList(symbol_drop, sList(symbol_goto, symbol_comment_state));
                } else {
                    if (true_q(char_is__q(c, '\0'))) {
                        return sList(symbol_drop, sList(symbol_emit, symbol_end_marker));
                    } else {
                        return sList(symbol_goto, symbol_token_start_state);
                    }
                }
            }
        } else {
            if (true_q(Eq(state, symbol_token_start_state))) {
                if (true_q(char_is__q(c, '('))) {
                    return sList(symbol_drop, sList(symbol_emit, symbol_lparen));
                } else {
                    if (true_q(char_is__q(c, '['))) {
                        return sList(symbol_drop, sList(symbol_emit, symbol_lbracket));
                    } else {
                        if (true_q(char_is__q(c, ')'))) {
                            return sList(symbol_drop, sList(symbol_emit, symbol_rparen));
                        } else {
                            if (true_q(char_is__q(c, ']'))) {
                                return sList(symbol_drop, sList(symbol_emit, symbol_rbracket));
                            } else {
                                if (true_q(char_is__q(c, '\''))) {
                                    return sList(symbol_drop, sList(symbol_emit, symbol_apostrophe));
                                } else {
                                    if (true_q(char_is__q(c, '`'))) {
                                        return sList(symbol_drop, sList(symbol_emit, symbol_backquote));
                                    } else {
                                        if (true_q(char_is__q(c, ','))) {
                                            return sList(symbol_drop, sList(symbol_goto, symbol_comma_state));
                                        } else {
                                            if (true_q(char_is__q(c, '#'))) {
                                                return sList(symbol_drop, sList(symbol_goto, symbol_hash_prefix_state));
                                            } else {
                                                if (true_q(char_is__q(c, '"'))) {
                                                    return sList(symbol_drop, sList(symbol_goto, symbol_string_state));
                                                } else {
                                                    if (true_q(char_initial_q(c))) {
                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                    } else {
                                                        if (true_q(char_sign_q(c))) {
                                                            return sList(symbol_shift, sList(symbol_goto, symbol_signed_state));
                                                        } else {
                                                            if (true_q(char_is__q(c, '.'))) {
                                                                return sList(symbol_shift, sList(symbol_goto, symbol_decimal_point_state));
                                                            } else {
                                                                if (true_q(char_numeric_q(c))) {
                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_whole_number_state));
                                                                } else {
                                                                    return symbol_error;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                if (true_q(Eq(state, symbol_comment_state))) {
                    if (true_q(char_is__q(c, '\n'))) {
                        return sList(symbol_drop, sList(symbol_goto, symbol_start_state));
                    } else {
                        if (true_q(char_is__q(c, '\0'))) {
                            return sList(symbol_drop, sList(symbol_emit, symbol_end_marker));
                        } else {
                            return sList(symbol_drop, sList(symbol_goto, symbol_comment_state));
                        }
                    }
                } else {
                    if (true_q(Eq(state, symbol_comma_state))) {
                        if (true_q(char_is__q(c, '@'))) {
                            return sList(symbol_drop, sList(symbol_emit, symbol_comma_at));
                        } else {
                            return sList(symbol_emit, symbol_comma);
                        }
                    } else {
                        if (true_q(Eq(state, symbol_hash_prefix_state))) {
                            if (true_q(char_boolean_q(c))) {
                                return sList(symbol_shift, sList(symbol_emit, symbol_boolean));
                            } else {
                                if (true_q(char_is__q(c, '\\'))) {
                                    return sList(symbol_drop, sList(symbol_goto, symbol_character_state));
                                } else {
                                    if (true_q(char_is__q(c, '('))) {
                                        return sList(symbol_drop, sList(symbol_emit, symbol_lvector));
                                    } else {
                                        return symbol_error;
                                    }
                                }
                            }
                        } else {
                            if (true_q(Eq(state, symbol_character_state))) {
                                if (true_q(char_alphabetic_q(c))) {
                                    return sList(symbol_shift, sList(symbol_goto, symbol_alphabetic_character_state));
                                } else {
                                    if (true_q((! true_q(char_is__q(c, '\0'))))) {
                                        return sList(symbol_shift, sList(symbol_emit, symbol_character));
                                    } else {
                                        return symbol_error;
                                    }
                                }
                            } else {
                                if (true_q(Eq(state, symbol_alphabetic_character_state))) {
                                    if (true_q(char_alphabetic_q(c))) {
                                        return sList(symbol_shift, sList(symbol_goto, symbol_named_character_state));
                                    } else {
                                        return sList(symbol_emit, symbol_character);
                                    }
                                } else {
                                    if (true_q(Eq(state, symbol_named_character_state))) {
                                        if (true_q(char_delimiter_q(c))) {
                                            return sList(symbol_emit, symbol_named_character);
                                        } else {
                                            return sList(symbol_shift, sList(symbol_goto, symbol_named_character_state));
                                        }
                                    } else {
                                        if (true_q(Eq(state, symbol_string_state))) {
                                            if (true_q(char_is__q(c, '"'))) {
                                                return sList(symbol_drop, sList(symbol_emit, symbol_make_string));
                                            } else {
                                                if (true_q(char_is__q(c, '\\'))) {
                                                    return sList(symbol_drop, sList(symbol_goto, symbol_string_escape_state));
                                                } else {
                                                    if (true_q((! true_q(char_is__q(c, '\0'))))) {
                                                        return sList(symbol_shift, sList(symbol_goto, symbol_string_state));
                                                    } else {
                                                        return symbol_error;
                                                    }
                                                }
                                            }
                                        } else {
                                            if (true_q(Eq(state, symbol_string_escape_state))) {
                                                if (true_q(char_is__q(c, '"'))) {
                                                    return sList(symbol_shift, sList(symbol_goto, symbol_string_state));
                                                } else {
                                                    if (true_q(char_is__q(c, '\\'))) {
                                                        return sList(symbol_shift, sList(symbol_goto, symbol_string_state));
                                                    } else {
                                                        if (true_q(char_is__q(c, 'b'))) {
                                                            return sList(symbol_replace, '\b', sList(symbol_goto, symbol_string_state));
                                                        } else {
                                                            if (true_q(char_is__q(c, 'f'))) {
                                                                return sList(symbol_replace, '\f', sList(symbol_goto, symbol_string_state));
                                                            } else {
                                                                if (true_q(char_is__q(c, 'n'))) {
                                                                    return sList(symbol_replace, '\n', sList(symbol_goto, symbol_string_state));
                                                                } else {
                                                                    if (true_q(char_is__q(c, 't'))) {
                                                                        return sList(symbol_replace, '\t', sList(symbol_goto, symbol_string_state));
                                                                    } else {
                                                                        if (true_q(char_is__q(c, 'r'))) {
                                                                            return sList(symbol_replace, '\r', sList(symbol_goto, symbol_string_state));
                                                                        } else {
                                                                            return symbol_error;
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                if (true_q(Eq(state, symbol_identifier_state))) {
                                                    if (true_q(char_subsequent_q(c))) {
                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                    } else {
                                                        if (true_q(char_delimiter_q(c))) {
                                                            return sList(symbol_emit, symbol_identifier);
                                                        } else {
                                                            return symbol_error;
                                                        }
                                                    }
                                                } else {
                                                    if (true_q(Eq(state, symbol_signed_state))) {
                                                        if (true_q(char_numeric_q(c))) {
                                                            return sList(symbol_shift, sList(symbol_goto, symbol_whole_number_state));
                                                        } else {
                                                            if (true_q(char_is__q(c, '.'))) {
                                                                return sList(symbol_shift, sList(symbol_goto, symbol_signed_decimal_point_state));
                                                            } else {
                                                                if (true_q(char_delimiter_q(c))) {
                                                                    return sList(symbol_emit, symbol_identifier);
                                                                } else {
                                                                    if (true_q(char_subsequent_q(c))) {
                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                    } else {
                                                                        return symbol_error;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    } else {
                                                        if (true_q(Eq(state, symbol_decimal_point_state))) {
                                                            if (true_q(char_numeric_q(c))) {
                                                                return sList(symbol_shift, sList(symbol_goto, symbol_fractional_number_state));
                                                            } else {
                                                                if (true_q(char_delimiter_q(c))) {
                                                                    return sList(symbol_emit, symbol_dot);
                                                                } else {
                                                                    if (true_q(char_subsequent_q(c))) {
                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                    } else {
                                                                        return symbol_error;
                                                                    }
                                                                }
                                                            }
                                                        } else {
                                                            if (true_q(Eq(state, symbol_signed_decimal_point_state))) {
                                                                if (true_q(char_numeric_q(c))) {
                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_fractional_number_state));
                                                                } else {
                                                                    if (true_q(char_delimiter_q(c))) {
                                                                        return sList(symbol_emit, symbol_identifier);
                                                                    } else {
                                                                        if (true_q(char_subsequent_q(c))) {
                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                        } else {
                                                                            return symbol_error;
                                                                        }
                                                                    }
                                                                }
                                                            } else {
                                                                if (true_q(Eq(state, symbol_whole_number_state))) {
                                                                    if (true_q(char_numeric_q(c))) {
                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_whole_number_state));
                                                                    } else {
                                                                        if (true_q(char_is__q(c, '.'))) {
                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_fractional_number_state));
                                                                        } else {
                                                                            if (true_q(char_is__q(c, '/'))) {
                                                                                return sList(symbol_shift, sList(symbol_goto, symbol_rational_number_state));
                                                                            } else {
                                                                                if (true_q((true_q(char_is__q(c, 'e')) || true_q(char_is__q(c, 'E'))))) {
                                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_suffix_state));
                                                                                } else {
                                                                                    if (true_q(char_delimiter_q(c))) {
                                                                                        return sList(symbol_emit, symbol_integer);
                                                                                    } else {
                                                                                        if (true_q(char_subsequent_q(c))) {
                                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                        } else {
                                                                                            return symbol_error;
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                } else {
                                                                    if (true_q(Eq(state, symbol_fractional_number_state))) {
                                                                        if (true_q(char_numeric_q(c))) {
                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_fractional_number_state));
                                                                        } else {
                                                                            if (true_q((true_q(char_is__q(c, 'e')) || true_q(char_is__q(c, 'E'))))) {
                                                                                return sList(symbol_shift, sList(symbol_goto, symbol_suffix_state));
                                                                            } else {
                                                                                if (true_q(char_delimiter_q(c))) {
                                                                                    return sList(symbol_emit, symbol_decimal);
                                                                                } else {
                                                                                    if (true_q(char_subsequent_q(c))) {
                                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                    } else {
                                                                                        return symbol_error;
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    } else {
                                                                        if (true_q(Eq(state, symbol_rational_number_state))) {
                                                                            if (true_q(char_numeric_q(c))) {
                                                                                return sList(symbol_shift, sList(symbol_goto, symbol_rational_number_state_star));
                                                                            } else {
                                                                                if (true_q(char_delimiter_q(c))) {
                                                                                    return sList(symbol_emit, symbol_identifier);
                                                                                } else {
                                                                                    if (true_q(char_subsequent_q(c))) {
                                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                    } else {
                                                                                        return symbol_error;
                                                                                    }
                                                                                }
                                                                            }
                                                                        } else {
                                                                            if (true_q(Eq(state, symbol_rational_number_state_star))) {
                                                                                if (true_q(char_numeric_q(c))) {
                                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_rational_number_state_star));
                                                                                } else {
                                                                                    if (true_q(char_delimiter_q(c))) {
                                                                                        return sList(symbol_emit, symbol_rational);
                                                                                    } else {
                                                                                        if (true_q(char_subsequent_q(c))) {
                                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                        } else {
                                                                                            return symbol_error;
                                                                                        }
                                                                                    }
                                                                                }
                                                                            } else {
                                                                                if (true_q(Eq(state, symbol_suffix_state))) {
                                                                                    if (true_q(char_sign_q(c))) {
                                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_signed_exponent_state));
                                                                                    } else {
                                                                                        if (true_q(char_numeric_q(c))) {
                                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_exponent_state));
                                                                                        } else {
                                                                                            if (true_q(char_delimiter_q(c))) {
                                                                                                return sList(symbol_emit, symbol_identifier);
                                                                                            } else {
                                                                                                if (true_q(char_subsequent_q(c))) {
                                                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                                } else {
                                                                                                    return symbol_error;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                } else {
                                                                                    if (true_q(Eq(state, symbol_signed_exponent_state))) {
                                                                                        if (true_q(char_numeric_q(c))) {
                                                                                            return sList(symbol_shift, sList(symbol_goto, symbol_exponent_state));
                                                                                        } else {
                                                                                            if (true_q(char_delimiter_q(c))) {
                                                                                                return sList(symbol_emit, symbol_identifier);
                                                                                            } else {
                                                                                                if (true_q(char_subsequent_q(c))) {
                                                                                                    return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                                } else {
                                                                                                    return symbol_error;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    } else {
                                                                                        if (true_q(Eq(state, symbol_exponent_state))) {
                                                                                            if (true_q(char_numeric_q(c))) {
                                                                                                return sList(symbol_shift, sList(symbol_goto, symbol_exponent_state));
                                                                                            } else {
                                                                                                if (true_q(char_delimiter_q(c))) {
                                                                                                    return sList(symbol_emit, symbol_decimal);
                                                                                                } else {
                                                                                                    if (true_q(char_subsequent_q(c))) {
                                                                                                        return sList(symbol_shift, sList(symbol_goto, symbol_identifier_state));
                                                                                                    } else {
                                                                                                        return symbol_error;
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                        } else {
                                                                                            throw new Exception("symbol_apply_state: " + format("invalid state: ~a", state));;
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static bool aatom_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), atom_tag)));
    }
    
    public static bool apair_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), pair_tag)));
    }
    
    public static bool annotated_q(object x) {
        return (true_q(pair_q(x)) && true_q((true_q(Eq(car(x), atom_tag)) || true_q(Eq(car(x), pair_tag)))));
    }
    
    public static object untag_atom_hat(object aatom) {
        return cadr(aatom);
    }
    
    public static bool atom_q_hat(object asexp) {
        return Eq(car(asexp), atom_tag);
    }
    
    public static bool pair_q_hat(object asexp) {
        return Eq(car(asexp), pair_tag);
    }
    
    public static bool null_q_hat(object asexp) {
        return (true_q(atom_q_hat(asexp)) && true_q(null_q(untag_atom_hat(asexp))));
    }
    
    public static bool symbol_q_hat(object asexp) {
        return (true_q(atom_q_hat(asexp)) && true_q(symbol_q(untag_atom_hat(asexp))));
    }
    
    public static bool string_q_hat(object asexp) {
        return (true_q(atom_q_hat(asexp)) && true_q(string_q(untag_atom_hat(asexp))));
    }
    
    public static bool vector_q_hat(object asexp) {
        return (true_q(atom_q_hat(asexp)) && true_q(vector_q(untag_atom_hat(asexp))));
    }
    
    public static object car_hat(object asexp) {
        return cadr(asexp);
    }
    
    public static object cdr_hat(object asexp) {
        return caddr(asexp);
    }
    
    public static object cadr_hat(object asexp) {
        return car_hat(cdr_hat(asexp));
    }
    
    public static object cdar_hat(object asexp) {
        return cdr_hat(car_hat(asexp));
    }
    
    public static object caar_hat(object asexp) {
        return car_hat(car_hat(asexp));
    }
    
    public static object cddr_hat(object asexp) {
        return cdr_hat(cdr_hat(asexp));
    }
    
    public static object cdddr_hat(object asexp) {
        return cdr_hat(cdr_hat(cdr_hat(asexp)));
    }
    
    public static object caddr_hat(object asexp) {
        return car_hat(cdr_hat(cdr_hat(asexp)));
    }
    
    public static object cdadr_hat(object asexp) {
        return cdr_hat(car_hat(cdr_hat(asexp)));
    }
    
    public static object cadar_hat(object asexp) {
        return car_hat(cdr_hat(car_hat(asexp)));
    }
    
    public static object caadr_hat(object asexp) {
        return car_hat(car_hat(cdr_hat(asexp)));
    }
    
    public static object cadddr_hat(object asexp) {
        return car_hat(cdr_hat(cdr_hat(cdr_hat(asexp))));
    }
    
    public static bool eq_q_hat(object asexp, object sym) {
        return Eq(cadr(asexp), sym);
    }
    
    public static object vector_to_list_hat(object asexp) {
        return vector_to_list(cadr(asexp));
    }
    
    public static object symbol_to_string_hat(object asexp) {
        return symbol_to_string(cadr(asexp));
    }
    
    public static bool list_q_hat(object asexp) {
        return (true_q(null_q_hat(asexp)) || true_q((true_q(pair_q_hat(asexp)) && true_q(list_q_hat(caddr(asexp))))));
    }
    
    public static object at_hat(object alist) {
        if (true_q(null_q_hat(alist))) {
            return symbol_emptylist;
        } else {
            return cons(car_hat(alist), at_hat(cdr_hat(alist)));
        }
    }
    
    public static object length_hat(object asexp) {
        if (true_q(null_q_hat(asexp))) {
            return 0;
        } else {
            return Add(1, length_hat(cdr_hat(asexp)));
        }
    }
    
    public static object cons_hat(object a, object b, object info) {
        return sList(pair_tag, a, b, info);
    }
    
    public static void annotate_cps() {
        if (true_q((! true_q(_starreader_generates_annotated_sexps_q_star)))) {
            value_reg = x_reg;
            pc = apply_cont;
        } else {
            if (true_q(annotated_q(x_reg))) {
                value_reg = x_reg;
                pc = apply_cont;
            } else {
                if (true_q(pair_q(x_reg))) {
                    k_reg = make_cont("cont", 3, x_reg, info_reg, k_reg);
                    info_reg = symbol_none;
                    x_reg = car(x_reg);
                    pc = annotate_cps;
                } else {
                    value_reg = sList(atom_tag, x_reg, info_reg);
                    pc = apply_cont;
                }
            }
        }
    }
    
    public static void unannotate_cps() {
        if (true_q(aatom_q(x_reg))) {
            x_reg = cadr(x_reg);
            pc = unannotate_cps;
        } else {
            if (true_q(apair_q(x_reg))) {
                k_reg = make_cont("cont", 7, x_reg, k_reg);
                x_reg = cadr(x_reg);
                pc = unannotate_cps;
            } else {
                if (true_q(pair_q(x_reg))) {
                    k_reg = make_cont("cont", 6, x_reg, k_reg);
                    x_reg = car(x_reg);
                    pc = unannotate_cps;
                } else {
                    if (true_q(vector_q(x_reg))) {
                        k_reg = make_cont("cont", 4, k_reg);
                        x_reg = vector_to_list(x_reg);
                        pc = unannotate_cps;
                    } else {
                        value_reg = x_reg;
                        pc = apply_cont;
                    }
                }
            }
        }
    }
    
    public static object make_info(object src, object start, object end) {
        return cons(src, append(start, end));
    }
    
    public static object replace_info(object asexp, object new_info) {
        if (true_q(atom_q_hat(asexp))) {
            return sList(atom_tag, cadr(asexp), new_info);
        } else {
            return sList(pair_tag, cadr(asexp), caddr(asexp), new_info);
        }
    }
    
    public static object get_srcfile(object info) {
        return car(info);
    }
    
    public static object get_start_line(object info) {
        return cadr(info);
    }
    
    public static object get_start_char(object info) {
        return caddr(info);
    }
    
    public static object get_start_pos(object info) {
        return cadddr(info);
    }
    
    public static object get_end_line(object info) {
        return car(cddddr(info));
    }
    
    public static object get_end_char(object info) {
        return cadr(cddddr(info));
    }
    
    public static object get_end_pos(object info) {
        return caddr(cddddr(info));
    }
    
    public static object get_source_info(object asexp) {
        return rac(asexp);
    }
    
    public static bool source_info_q(object x) {
        return (true_q(Eq(x, symbol_none)) || true_q(list_q(x)));
    }
    
    public static bool has_source_info_q(object asexp) {
        return (! true_q(Eq(get_source_info(asexp), symbol_none)));
    }
    
    public static bool original_source_info_q(object asexp) {
        return (true_q(has_source_info_q(asexp)) && true_q(Equal(length(get_source_info(asexp)), 7)));
    }
    
    public static bool macro_derived_source_info_q(object asexp) {
        return (true_q(has_source_info_q(asexp)) && true_q(Equal(length(get_source_info(asexp)), 8)));
    }
    
    public static object first(object x) {
        return car(x);
    }
    
    public static object rest_of(object x) {
        return cdr(x);
    }
    
    public static void unexpected_token_error() {
        object token = symbol_undefined;
        token = first(tokens_reg);
        if (true_q(token_type_q(token, symbol_end_marker))) {
            msg_reg = "unexpected end of input";
            pc = read_error;
        } else {
            msg_reg = format("unexpected '~a' encountered", car(token));
            pc = read_error;
        }
    }
    
    public static void read_error() {
        object token = symbol_undefined;
        token = first(tokens_reg);
        exception_reg = make_exception("ReadError", msg_reg, src_reg, get_token_start_line(token), get_token_start_char(token));
        pc = apply_handler2;
    }
    
    public static void read_sexp() {
        object start = symbol_undefined;
        object end = symbol_undefined;
        end = get_token_end(first(tokens_reg));
        start = get_token_start(first(tokens_reg));
        object temp_1 = symbol_undefined;
        temp_1 = first(tokens_reg);
        if (true_q(Eq(car(temp_1), symbol_integer))) {
            object str = symbol_undefined;
            str = list_ref(temp_1, 1);
            k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
            info_reg = make_info(src_reg, start, end);
            x_reg = string_to_integer(str);
            pc = annotate_cps;
        } else {
            if (true_q(Eq(car(temp_1), symbol_decimal))) {
                object str = symbol_undefined;
                str = list_ref(temp_1, 1);
                k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                info_reg = make_info(src_reg, start, end);
                x_reg = string_to_decimal(str);
                pc = annotate_cps;
            } else {
                if (true_q(Eq(car(temp_1), symbol_rational))) {
                    object str = symbol_undefined;
                    str = list_ref(temp_1, 1);
                    object num = symbol_undefined;
                    num = string_to_rational(str);
                    if (true_q(true_q(num))) {
                        k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                        info_reg = make_info(src_reg, start, end);
                        x_reg = num;
                        pc = annotate_cps;
                    } else {
                        msg_reg = format("cannot represent ~a", str);
                        pc = read_error;
                    }
                } else {
                    if (true_q(Eq(car(temp_1), symbol_boolean))) {
                        object bool_ = symbol_undefined;
                        bool_ = list_ref(temp_1, 1);
                        k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                        info_reg = make_info(src_reg, start, end);
                        x_reg = bool_;
                        pc = annotate_cps;
                    } else {
                        if (true_q(Eq(car(temp_1), symbol_character))) {
                            object char_ = symbol_undefined;
                            char_ = list_ref(temp_1, 1);
                            k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                            info_reg = make_info(src_reg, start, end);
                            x_reg = char_;
                            pc = annotate_cps;
                        } else {
                            if (true_q(Eq(car(temp_1), symbol_make_string))) {
                                object str = symbol_undefined;
                                str = list_ref(temp_1, 1);
                                k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                                info_reg = make_info(src_reg, start, end);
                                x_reg = str;
                                pc = annotate_cps;
                            } else {
                                if (true_q(Eq(car(temp_1), symbol_identifier))) {
                                    object id = symbol_undefined;
                                    id = list_ref(temp_1, 1);
                                    k_reg = make_cont("cont", 9, end, tokens_reg, fail_reg, k_reg);
                                    info_reg = make_info(src_reg, start, end);
                                    x_reg = id;
                                    pc = annotate_cps;
                                } else {
                                    if (true_q(Eq(car(temp_1), symbol_apostrophe))) {
                                        keyword_reg = symbol_quote;
                                        pc = read_abbreviation;
                                    } else {
                                        if (true_q(Eq(car(temp_1), symbol_backquote))) {
                                            keyword_reg = symbol_quasiquote;
                                            pc = read_abbreviation;
                                        } else {
                                            if (true_q(Eq(car(temp_1), symbol_comma))) {
                                                keyword_reg = symbol_unquote;
                                                pc = read_abbreviation;
                                            } else {
                                                if (true_q(Eq(car(temp_1), symbol_comma_at))) {
                                                    keyword_reg = symbol_unquote_splicing;
                                                    pc = read_abbreviation;
                                                } else {
                                                    if (true_q(Eq(car(temp_1), symbol_lparen))) {
                                                        object tokens = symbol_undefined;
                                                        tokens = rest_of(tokens_reg);
                                                        k_reg = make_cont4("cont4", 2, src_reg, start, k_reg);
                                                        expected_terminator_reg = symbol_rparen;
                                                        tokens_reg = tokens;
                                                        pc = read_sexp_sequence;
                                                    } else {
                                                        if (true_q(Eq(car(temp_1), symbol_lbracket))) {
                                                            object tokens = symbol_undefined;
                                                            tokens = rest_of(tokens_reg);
                                                            k_reg = make_cont4("cont4", 2, src_reg, start, k_reg);
                                                            expected_terminator_reg = symbol_rbracket;
                                                            tokens_reg = tokens;
                                                            pc = read_sexp_sequence;
                                                        } else {
                                                            if (true_q(Eq(car(temp_1), symbol_lvector))) {
                                                                k_reg = make_cont4("cont4", 1, src_reg, start, k_reg);
                                                                tokens_reg = rest_of(tokens_reg);
                                                                pc = read_vector_sequence;
                                                            } else {
                                                                pc = unexpected_token_error;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static void read_abbreviation() {
        object start = symbol_undefined;
        object keyword_end = symbol_undefined;
        keyword_end = get_token_end(first(tokens_reg));
        start = get_token_start(first(tokens_reg));
        k_reg = make_cont("cont", 10, src_reg, start, tokens_reg, handler_reg, fail_reg, k_reg);
        info_reg = make_info(src_reg, start, keyword_end);
        x_reg = keyword_reg;
        pc = annotate_cps;
    }
    
    public static void read_vector_sequence() {
        object temp_1 = symbol_undefined;
        temp_1 = first(tokens_reg);
        if (true_q(Eq(car(temp_1), symbol_rparen))) {
            expected_terminator_reg = symbol_rparen;
            sexps_reg = symbol_emptylist;
            pc = close_sexp_sequence;
        } else {
            if (true_q(Eq(car(temp_1), symbol_dot))) {
                msg_reg = "unexpected dot (.)";
                pc = read_error;
            } else {
                k_reg = make_cont4("cont4", 5, src_reg, handler_reg, k_reg);
                pc = read_sexp;
            }
        }
    }
    
    public static void read_sexp_sequence() {
        object temp_1 = symbol_undefined;
        temp_1 = first(tokens_reg);
        if (true_q(memq(car(temp_1), sList(symbol_rparen, symbol_rbracket)))) {
            sexps_reg = symbol_emptylist;
            pc = close_sexp_sequence;
        } else {
            if (true_q(Eq(car(temp_1), symbol_dot))) {
                msg_reg = "unexpected dot (.)";
                pc = read_error;
            } else {
                k_reg = make_cont4("cont4", 7, expected_terminator_reg, src_reg, handler_reg, k_reg);
                pc = read_sexp;
            }
        }
    }
    
    public static void close_sexp_sequence() {
        object end = symbol_undefined;
        end = get_token_end(first(tokens_reg));
        object temp_1 = symbol_undefined;
        temp_1 = first(tokens_reg);
        if (true_q(memq(car(temp_1), sList(symbol_rparen, symbol_rbracket)))) {
            if (true_q(token_type_q(first(tokens_reg), expected_terminator_reg))) {
                value4_reg = fail_reg;
                value3_reg = rest_of(tokens_reg);
                value2_reg = end;
                value1_reg = sexps_reg;
                pc = apply_cont4;
            } else {
                if (true_q(Eq(expected_terminator_reg, symbol_rparen))) {
                    msg_reg = "parenthesized list terminated by bracket";
                    pc = read_error;
                } else {
                    if (true_q(Eq(expected_terminator_reg, symbol_rbracket))) {
                        msg_reg = "bracketed list terminated by parenthesis";
                        pc = read_error;
                    }
                }
            }
        } else {
            pc = unexpected_token_error;
        }
    }
    
    public static object make_binding(object value) {
        return cons(value, "");
    }
    
    public static object binding_value(object binding) {
        return car(binding);
    }
    
    public static object binding_docstring(object binding) {
        return cdr(binding);
    }
    
    public static void set_binding_value_b(object binding, object value) {
        set_car_b(binding, value);
    }
    
    public static void set_binding_docstring_b(object binding, object docstring) {
        set_cdr_b(binding, docstring);
    }
    
    public static object make_frame(object variables, object values) {
        return sList(list_to_vector(map(make_binding_proc, values)), variables);
    }
    
    public static bool empty_frame_q(object frame) {
        return null_q(cadr(frame));
    }
    
    public static object frame_bindings(object frame) {
        return car(frame);
    }
    
    public static bool environment_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), symbol_environment)));
    }
    
    public static object make_empty_environment() {
        return sList(symbol_environment, make_frame(symbol_emptylist, symbol_emptylist));
    }
    
    public static object make_initial_environment(object vars, object vals) {
        return sList(symbol_environment, make_frame(vars, vals));
    }
    
    public static object first_frame(object env) {
        return cadr(env);
    }
    
    public static object first_frame_vars(object env) {
        return cadr(first_frame(env));
    }
    
    public static object initial_contours(object env) {
        return cdr(first_frame(env));
    }
    
    public static object frames(object env) {
        return cdr(env);
    }
    
    public static object add_binding(object new_var, object new_binding, object frame) {
        object bindings = symbol_undefined;
        object vars = symbol_undefined;
        vars = cadr(frame);
        bindings = vector_to_list(car(frame));
        return sList(list_to_vector(append(bindings, sList(new_binding))), append(vars, sList(new_var)));
    }
    
    public static void set_first_frame_b(object env, object new_frame) {
        set_car_b(cdr(env), new_frame);
    }
    
    public static object extend(object env, object variables, object values) {
        return cons(symbol_environment, cons(make_frame(variables, values), cdr(env)));
    }
    
    public static object search_env(object env, object variable) {
        return search_frames(cdr(env), variable);
    }
    
    public static object search_frames(object frames, object variable) {
        if (true_q(null_q(frames))) {
            return false;
        } else {
            object binding = symbol_undefined;
            binding = search_frame(car(frames), variable);
            if (true_q(binding)) {
                return binding;
            } else {
                return search_frames(cdr(frames), variable);
            }
        }
    }
    
    public static bool in_first_frame_q(object var, object env) {
        return true_q(memq(var, first_frame_vars(env)));
    }
    
    public static object get_first_frame_value(object var, object env) {
        return binding_value(search_frame(first_frame(env), var));
    }
    
    public static void lookup_value_by_lexical_address() {
        object bindings = symbol_undefined;
        bindings = frame_bindings(list_ref(frames_reg, depth_reg));
        value2_reg = fail_reg;
        value1_reg = binding_value(vector_ref(bindings, offset_reg));
        pc = apply_cont2;
    }
    
    public static void lookup_binding_by_lexical_address() {
        object bindings = symbol_undefined;
        bindings = frame_bindings(list_ref(frames_reg, depth_reg));
        value2_reg = fail_reg;
        value1_reg = vector_ref(bindings, offset_reg);
        pc = apply_cont2;
    }
    
    public static void lookup_value() {
        sk_reg = make_cont2("cont2", 3, k_reg);
        dk_reg = make_cont3("cont3", 3, k_reg);
        gk_reg = make_cont2("cont2", 4, k_reg);
        pc = lookup_variable;
    }
    
    public static void lookup_variable() {
        object binding = symbol_undefined;
        binding = search_env(env_reg, var_reg);
        if (true_q(binding)) {
            value2_reg = fail_reg;
            value1_reg = binding;
            k_reg = sk_reg;
            pc = apply_cont2;
        } else {
            object components = symbol_undefined;
            components = split_variable(var_reg);
            if (true_q((true_q(null_q(cdr(components))) && true_q(dlr_env_contains(car(components)))))) {
                value2_reg = fail_reg;
                value1_reg = car(components);
                k_reg = gk_reg;
                pc = apply_cont2;
            } else {
                if (true_q((true_q((! true_q(null_q(cdr(components))))) && true_q(dlr_env_contains(car(components))) && true_q(dlr_object_contains(dlr_env_lookup(car(components)), components))))) {
                    value3_reg = fail_reg;
                    value2_reg = components;
                    value1_reg = dlr_env_lookup(car(components));
                    k_reg = dk_reg;
                    pc = apply_cont3;
                } else {
                    if (true_q(null_q(cdr(components)))) {
                        info_reg = var_info_reg;
                        msg_reg = format("unbound variable '~a'", var_reg);
                        pc = runtime_error;
                    } else {
                        module_reg = env_reg;
                        path_reg = "";
                        components_reg = components;
                        pc = lookup_variable_components;
                    }
                }
            }
        }
    }
    
    public static void lookup_variable_components() {
        object var = symbol_undefined;
        object binding = symbol_undefined;
        var = car(components_reg);
        binding = search_env(module_reg, var);
        if (true_q(binding)) {
            if (true_q(null_q(cdr(components_reg)))) {
                value2_reg = fail_reg;
                value1_reg = binding;
                k_reg = sk_reg;
                pc = apply_cont2;
            } else {
                object value = symbol_undefined;
                object new_path = symbol_undefined;
                new_path = (string_is__q(path_reg, "") ? format("~a", var) : format("~a.~a", path_reg, var));
                value = binding_value(binding);
                if (true_q(environment_q(value))) {
                    module_reg = value;
                    path_reg = new_path;
                    components_reg = cdr(components_reg);
                    pc = lookup_variable_components;
                } else {
                    if (true_q(dlr_object_contains(value, components_reg))) {
                        value3_reg = fail_reg;
                        value2_reg = components_reg;
                        value1_reg = value;
                        k_reg = dk_reg;
                        pc = apply_cont3;
                    } else {
                        info_reg = var_info_reg;
                        msg_reg = format("'~a' is not a module", new_path);
                        pc = runtime_error;
                    }
                }
            }
        } else {
            if (true_q(string_is__q(path_reg, ""))) {
                info_reg = var_info_reg;
                msg_reg = format("unbound module '~a'", var);
                pc = runtime_error;
            } else {
                info_reg = var_info_reg;
                msg_reg = format("unbound variable '~a' in module '~a'", var, path_reg);
                pc = runtime_error;
            }
        }
    }
    
    public static void lookup_binding_in_first_frame() {
        object frame = symbol_undefined;
        frame = first_frame(env_reg);
        object binding = symbol_undefined;
        binding = search_frame(frame, var_reg);
        if (true_q(binding)) {
            value2_reg = fail_reg;
            value1_reg = binding;
            pc = apply_cont2;
        } else {
            object new_binding = symbol_undefined;
            new_binding = make_binding(symbol_undefined);
            object new_frame = symbol_undefined;
            new_frame = add_binding(var_reg, new_binding, frame);
            set_first_frame_b(env_reg, new_frame);
            value2_reg = fail_reg;
            value1_reg = new_binding;
            pc = apply_cont2;
        }
    }
    
    public static object split_variable(object var) {
        object strings = symbol_undefined;
        strings = string_split(symbol_to_string(var), '.');
        if (true_q(member("", strings))) {
            return symbol_emptylist;
        } else {
            return map(string_to_symbol_proc, strings);
        }
    }
    
    public static object head(object formals) {
        if (true_q(symbol_q(formals))) {
            return symbol_emptylist;
        } else {
            if (true_q(pair_q(cdr(formals)))) {
                return cons(car(formals), head(cdr(formals)));
            } else {
                return sList(car(formals));
            }
        }
    }
    
    public static object last(object formals) {
        if (true_q(symbol_q(formals))) {
            return formals;
        } else {
            if (true_q(pair_q(cdr(formals)))) {
                return last(cdr(formals));
            } else {
                return cdr(formals);
            }
        }
    }
    
    public static bool anything_q(object datum) {
        return true;
    }
    
    public static bool application_q_hat(object asexp) {
        return (true_q(list_q_hat(asexp)) && true_q((! true_q(null_q_hat(asexp)))) && true_q((! true_q(reserved_keyword_q(untag_atom_hat(car_hat(asexp)))))));
    }
    
    public static bool reserved_keyword_q(object x) {
        return (true_q(symbol_q(x)) && true_q((! true_q(Eq(memq(x, get_reserved_keywords()), false)))));
    }
    
    public static object get_reserved_keywords() {
        return sList(symbol_quote, symbol_func, symbol_define_b, symbol_quasiquote, symbol_lambda, symbol_if, symbol_set_b, symbol_define, symbol_begin, symbol_cond, symbol_and, symbol_or, symbol_let, symbol_let_star, symbol_letrec, symbol_case, symbol_record_case, symbol_try, symbol_catch, symbol_finally, symbol_raise, symbol_define_syntax, symbol_choose, symbol_define_datatype, symbol_cases, symbol_trace_lambda);
    }
    
    public static bool mit_style_define_q_hat(object asexp) {
        return (! true_q(symbol_q_hat(cadr_hat(asexp))));
    }
    
    public static bool literal_q(object datum) {
        return (true_q(number_q(datum)) || true_q(boolean_q(datum)) || true_q(null_q(datum)) || true_q(char_q(datum)) || true_q(string_q(datum)));
    }
    
    public static bool literal_q_hat(object asexp) {
        return (true_q(Eq(car(asexp), atom_tag)) && true_q((true_q(number_q(untag_atom_hat(asexp))) || true_q(boolean_q(untag_atom_hat(asexp))) || true_q(null_q(untag_atom_hat(asexp))) || true_q(char_q(untag_atom_hat(asexp))) || true_q(string_q(untag_atom_hat(asexp))))));
    }
    
    public static bool syntactic_sugar_q_hat(object asexp) {
        return (true_q(pair_q_hat(asexp)) && true_q(symbol_q_hat(car_hat(asexp))) && true_q(in_first_frame_q(untag_atom_hat(car_hat(asexp)), macro_env)));
    }
    
    public static object define_var_hat(object x) {
        return untag_atom_hat(cadr_hat(x));
    }
    
    public static object define_docstring_hat(object x) {
        return untag_atom_hat(caddr_hat(x));
    }
    
    public static object try_body_hat(object x) {
        return cadr_hat(x);
    }
    
    public static object catch_var_hat(object x) {
        return untag_atom_hat(cadr_hat(caddr_hat(x)));
    }
    
    public static object catch_exps_hat(object x) {
        return cddr_hat(caddr_hat(x));
    }
    
    public static object try_finally_exps_hat(object x) {
        return cdr_hat(caddr_hat(x));
    }
    
    public static object try_catch_finally_exps_hat(object x) {
        return cdr_hat(cadddr_hat(x));
    }
    
    public static void aparse() {
        object info = symbol_undefined;
        info = get_source_info(adatum_reg);
        if (true_q(literal_q_hat(adatum_reg))) {
            value2_reg = fail_reg;
            value1_reg = lit_aexp(untag_atom_hat(adatum_reg), info);
            pc = apply_cont2;
        } else {
            if (true_q(symbol_q_hat(adatum_reg))) {
                if (true_q(_staruse_lexical_address_star)) {
                    info_reg = info;
                    depth_reg = 0;
                    id_reg = untag_atom_hat(adatum_reg);
                    pc = get_lexical_address;
                } else {
                    value2_reg = fail_reg;
                    value1_reg = var_aexp(untag_atom_hat(adatum_reg), info);
                    pc = apply_cont2;
                }
            } else {
                if (true_q(vector_q_hat(adatum_reg))) {
                    k_reg = make_cont("cont", 20, info, fail_reg, k_reg);
                    x_reg = adatum_reg;
                    pc = unannotate_cps;
                } else {
                    if (true_q(quote_q_hat(adatum_reg))) {
                        k_reg = make_cont("cont", 19, info, fail_reg, k_reg);
                        x_reg = adatum_reg;
                        pc = unannotate_cps;
                    } else {
                        if (true_q(quasiquote_q_hat(adatum_reg))) {
                            k_reg = make_cont("cont", 18, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg);
                            depth_reg = 0;
                            ax_reg = cadr_hat(adatum_reg);
                            pc = qq_expand_cps;
                        } else {
                            if (true_q(unquote_q_hat(adatum_reg))) {
                                msg_reg = "misplaced";
                                pc = aparse_error;
                            } else {
                                if (true_q(unquote_splicing_q_hat(adatum_reg))) {
                                    msg_reg = "misplaced";
                                    pc = aparse_error;
                                } else {
                                    if (true_q(syntactic_sugar_q_hat(adatum_reg))) {
                                        k_reg = make_cont2("cont2", 33, senv_reg, handler_reg, k_reg);
                                        pc = expand_once_hat;
                                    } else {
                                        if (true_q(if_then_q_hat(adatum_reg))) {
                                            k_reg = make_cont2("cont2", 32, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                            adatum_reg = cadr_hat(adatum_reg);
                                            pc = aparse;
                                        } else {
                                            if (true_q(if_else_q_hat(adatum_reg))) {
                                                k_reg = make_cont2("cont2", 30, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                                adatum_reg = cadr_hat(adatum_reg);
                                                pc = aparse;
                                            } else {
                                                if (true_q(assignment_q_hat(adatum_reg))) {
                                                    k_reg = make_cont2("cont2", 27, adatum_reg, info, k_reg);
                                                    adatum_reg = caddr_hat(adatum_reg);
                                                    pc = aparse;
                                                } else {
                                                    if (true_q(func_q_hat(adatum_reg))) {
                                                        k_reg = make_cont2("cont2", 26, info, k_reg);
                                                        adatum_reg = cadr_hat(adatum_reg);
                                                        pc = aparse;
                                                    } else {
                                                        if (true_q(callback0_q_hat(adatum_reg))) {
                                                            k_reg = make_cont2("cont2", 25, info, k_reg);
                                                            adatum_reg = cadr_hat(adatum_reg);
                                                            pc = aparse;
                                                        } else {
                                                            if (true_q(callback1_q_hat(adatum_reg))) {
                                                                k_reg = make_cont2("cont2", 24, info, k_reg);
                                                                adatum_reg = cadr_hat(adatum_reg);
                                                                pc = aparse;
                                                            } else {
                                                                if (true_q(callback2_q_hat(adatum_reg))) {
                                                                    k_reg = make_cont2("cont2", 23, info, k_reg);
                                                                    adatum_reg = cadr_hat(adatum_reg);
                                                                    pc = aparse;
                                                                } else {
                                                                    if (true_q(define_q_hat(adatum_reg))) {
                                                                        if (true_q(mit_style_define_q_hat(adatum_reg))) {
                                                                            k_reg = make_cont("cont", 16, senv_reg, info, handler_reg, fail_reg, k_reg);
                                                                            datum_reg = adatum_reg;
                                                                            macro_reg = mit_define_transformer_hat;
                                                                            pc = apply_macro;
                                                                        } else {
                                                                            if (true_q(Equal(length_hat(adatum_reg), 3))) {
                                                                                k_reg = make_cont2("cont2", 22, adatum_reg, info, k_reg);
                                                                                adatum_reg = caddr_hat(adatum_reg);
                                                                                pc = aparse;
                                                                            } else {
                                                                                if (true_q((true_q(Equal(length_hat(adatum_reg), 4)) && true_q(string_q_hat(caddr_hat(adatum_reg)))))) {
                                                                                    k_reg = make_cont2("cont2", 21, adatum_reg, info, k_reg);
                                                                                    adatum_reg = cadddr_hat(adatum_reg);
                                                                                    pc = aparse;
                                                                                } else {
                                                                                    msg_reg = "bad concrete syntax:";
                                                                                    pc = aparse_error;
                                                                                }
                                                                            }
                                                                        }
                                                                    } else {
                                                                        if (true_q(define_b_q_hat(adatum_reg))) {
                                                                            if (true_q(mit_style_define_q_hat(adatum_reg))) {
                                                                                k_reg = make_cont("cont", 16, senv_reg, info, handler_reg, fail_reg, k_reg);
                                                                                datum_reg = adatum_reg;
                                                                                macro_reg = mit_define_transformer_hat;
                                                                                pc = apply_macro;
                                                                            } else {
                                                                                if (true_q(Equal(length_hat(adatum_reg), 3))) {
                                                                                    k_reg = make_cont2("cont2", 20, adatum_reg, info, k_reg);
                                                                                    adatum_reg = caddr_hat(adatum_reg);
                                                                                    pc = aparse;
                                                                                } else {
                                                                                    if (true_q((true_q(Equal(length_hat(adatum_reg), 4)) && true_q(string_q_hat(caddr_hat(adatum_reg)))))) {
                                                                                        k_reg = make_cont2("cont2", 19, adatum_reg, info, k_reg);
                                                                                        adatum_reg = cadddr_hat(adatum_reg);
                                                                                        pc = aparse;
                                                                                    } else {
                                                                                        msg_reg = "bad concrete syntax:";
                                                                                        pc = aparse_error;
                                                                                    }
                                                                                }
                                                                            }
                                                                        } else {
                                                                            if (true_q(define_syntax_q_hat(adatum_reg))) {
                                                                                object name = symbol_undefined;
                                                                                object aclauses = symbol_undefined;
                                                                                aclauses = cddr_hat(adatum_reg);
                                                                                name = define_var_hat(adatum_reg);
                                                                                k_reg = make_cont("cont", 14, aclauses, name, info, fail_reg, k_reg);
                                                                                x_reg = aclauses;
                                                                                pc = unannotate_cps;
                                                                            } else {
                                                                                if (true_q(begin_q_hat(adatum_reg))) {
                                                                                    if (true_q(null_q_hat(cdr_hat(adatum_reg)))) {
                                                                                        msg_reg = "bad concrete syntax:";
                                                                                        pc = aparse_error;
                                                                                    } else {
                                                                                        if (true_q(null_q_hat(cddr_hat(adatum_reg)))) {
                                                                                            adatum_reg = cadr_hat(adatum_reg);
                                                                                            pc = aparse;
                                                                                        } else {
                                                                                            k_reg = make_cont2("cont2", 18, info, k_reg);
                                                                                            adatum_list_reg = cdr_hat(adatum_reg);
                                                                                            pc = aparse_all;
                                                                                        }
                                                                                    }
                                                                                } else {
                                                                                    if (true_q(lambda_q_hat(adatum_reg))) {
                                                                                        k_reg = make_cont("cont", 13, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg);
                                                                                        x_reg = cadr_hat(adatum_reg);
                                                                                        pc = unannotate_cps;
                                                                                    } else {
                                                                                        if (true_q(trace_lambda_q_hat(adatum_reg))) {
                                                                                            k_reg = make_cont("cont", 12, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg);
                                                                                            x_reg = caddr_hat(adatum_reg);
                                                                                            pc = unannotate_cps;
                                                                                        } else {
                                                                                            if (true_q(try_q_hat(adatum_reg))) {
                                                                                                if (true_q(Equal(length_hat(adatum_reg), 2))) {
                                                                                                    adatum_reg = try_body_hat(adatum_reg);
                                                                                                    pc = aparse;
                                                                                                } else {
                                                                                                    if (true_q((true_q(Equal(length_hat(adatum_reg), 3)) && true_q(catch_q_hat(caddr_hat(adatum_reg)))))) {
                                                                                                        k_reg = make_cont2("cont2", 15, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                                                                                        adatum_reg = try_body_hat(adatum_reg);
                                                                                                        pc = aparse;
                                                                                                    } else {
                                                                                                        if (true_q((true_q(Equal(length_hat(adatum_reg), 3)) && true_q(finally_q_hat(caddr_hat(adatum_reg)))))) {
                                                                                                            k_reg = make_cont2("cont2", 13, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                                                                                            adatum_reg = try_body_hat(adatum_reg);
                                                                                                            pc = aparse;
                                                                                                        } else {
                                                                                                            if (true_q((true_q(Equal(length_hat(adatum_reg), 4)) && true_q(catch_q_hat(caddr_hat(adatum_reg))) && true_q(finally_q_hat(cadddr_hat(adatum_reg)))))) {
                                                                                                                k_reg = make_cont2("cont2", 11, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                                                                                                adatum_reg = try_body_hat(adatum_reg);
                                                                                                                pc = aparse;
                                                                                                            } else {
                                                                                                                msg_reg = "bad try syntax:";
                                                                                                                pc = aparse_error;
                                                                                                            }
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                            } else {
                                                                                                if (true_q(raise_q_hat(adatum_reg))) {
                                                                                                    k_reg = make_cont2("cont2", 8, info, k_reg);
                                                                                                    adatum_reg = cadr_hat(adatum_reg);
                                                                                                    pc = aparse;
                                                                                                } else {
                                                                                                    if (true_q(choose_q_hat(adatum_reg))) {
                                                                                                        k_reg = make_cont2("cont2", 7, info, k_reg);
                                                                                                        adatum_list_reg = cdr_hat(adatum_reg);
                                                                                                        pc = aparse_all;
                                                                                                    } else {
                                                                                                        if (true_q(application_q_hat(adatum_reg))) {
                                                                                                            k_reg = make_cont2("cont2", 6, adatum_reg, senv_reg, info, handler_reg, k_reg);
                                                                                                            adatum_reg = car_hat(adatum_reg);
                                                                                                            pc = aparse;
                                                                                                        } else {
                                                                                                            msg_reg = "bad concrete syntax:";
                                                                                                            pc = aparse_error;
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static void aparse_all() {
        if (true_q(null_q_hat(adatum_list_reg))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            k_reg = make_cont2("cont2", 35, adatum_list_reg, senv_reg, handler_reg, k_reg);
            adatum_reg = car_hat(adatum_list_reg);
            pc = aparse;
        }
    }
    
    public static void aparse_error() {
        object info = symbol_undefined;
        info = get_source_info(adatum_reg);
        k_reg = make_cont("cont", 21, msg_reg, info, handler_reg, fail_reg);
        x_reg = adatum_reg;
        pc = unannotate_cps;
    }
    
    public static void aparse_sexps() {
        if (true_q(token_type_q(first(tokens_reg), symbol_end_marker))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            k_reg = make_cont4("cont4", 9, senv_reg, src_reg, handler_reg, k_reg);
            pc = read_sexp;
        }
    }
    
    public static void get_lexical_address() {
        if (true_q(null_q(senv_reg))) {
            value2_reg = fail_reg;
            value1_reg = var_aexp(id_reg, info_reg);
            pc = apply_cont2;
        } else {
            if (true_q(memq(id_reg, car(senv_reg)))) {
                offset_reg = 0;
                contours_reg = car(senv_reg);
                pc = get_lexical_address_offset;
            } else {
                depth_reg = Add(depth_reg, 1);
                senv_reg = cdr(senv_reg);
                pc = get_lexical_address;
            }
        }
    }
    
    public static void get_lexical_address_offset() {
        if (true_q(Eq(car(contours_reg), id_reg))) {
            value2_reg = fail_reg;
            value1_reg = lexical_address_aexp(depth_reg, offset_reg, id_reg, info_reg);
            pc = apply_cont2;
        } else {
            offset_reg = Add(offset_reg, 1);
            contours_reg = cdr(contours_reg);
            pc = get_lexical_address_offset;
        }
    }
    
    public static void create_letrec_assignments_hat() {
        if (true_q(null_q_hat(vars_reg))) {
            value2_reg = symbol_emptylist;
            value1_reg = symbol_emptylist;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 39, procs_reg, vars_reg, k2_reg);
            procs_reg = cdr_hat(procs_reg);
            vars_reg = cdr_hat(vars_reg);
            pc = create_letrec_assignments_hat;
        }
    }
    
    public static void amacro_error() {
        object info = symbol_undefined;
        info = get_source_info(adatum_reg);
        exception_reg = make_exception("MacroError", msg_reg, get_start_line(info), get_srcfile(info), get_start_char(info));
        pc = apply_handler2;
    }
    
    public static void nest_let_star_bindings_hat() {
        if (true_q((true_q(null_q_hat(bindings_reg)) || true_q(null_q_hat(cdr_hat(bindings_reg)))))) {
            value_reg = append(sList(symbol_let), append(sList(bindings_reg), at_hat(bodies_reg)));
            pc = apply_cont;
        } else {
            k_reg = make_cont("cont", 22, bindings_reg, k_reg);
            bindings_reg = cdr_hat(bindings_reg);
            pc = nest_let_star_bindings_hat;
        }
    }
    
    public static void case_clauses_to_simple_cond_clauses_hat() {
        if (true_q(null_q_hat(clauses_reg))) {
            value_reg = symbol_emptylist;
            pc = apply_cont;
        } else {
            k_reg = make_cont("cont", 23, clauses_reg, var_reg, k_reg);
            clauses_reg = cdr_hat(clauses_reg);
            pc = case_clauses_to_simple_cond_clauses_hat;
        }
    }
    
    public static void case_clauses_to_cond_clauses_hat() {
        if (true_q(null_q_hat(clauses_reg))) {
            value2_reg = symbol_emptylist;
            value1_reg = symbol_emptylist;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 41, clauses_reg, var_reg, k2_reg);
            clauses_reg = cdr_hat(clauses_reg);
            pc = case_clauses_to_cond_clauses_hat;
        }
    }
    
    public static void record_case_clauses_to_cond_clauses_hat() {
        if (true_q(null_q_hat(clauses_reg))) {
            value2_reg = symbol_emptylist;
            value1_reg = symbol_emptylist;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 42, clauses_reg, var_reg, k2_reg);
            clauses_reg = cdr_hat(clauses_reg);
            pc = record_case_clauses_to_cond_clauses_hat;
        }
    }
    
    public static void make_dd_variant_constructors_hat() {
        if (true_q(null_q_hat(variants_reg))) {
            value2_reg = symbol_emptylist;
            value1_reg = symbol_emptylist;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 45, variants_reg, k2_reg);
            variant_reg = car_hat(variants_reg);
            pc = make_dd_variant_constructor_hat;
        }
    }
    
    public static void make_dd_variant_constructor_hat() {
        object name = symbol_undefined;
        object fields = symbol_undefined;
        fields = cdr_hat(variant_reg);
        name = car_hat(variant_reg);
        k_reg = make_cont("cont", 24, fields, name, k2_reg);
        cdrs_reg = symbol_args;
        fields_reg = fields;
        name_reg = name;
        pc = verify_dd_constructor_fields_hat;
    }
    
    public static void verify_dd_constructor_fields_hat() {
        if (true_q(null_q_hat(fields_reg))) {
            value_reg = append(sList(symbol_cons), append(sList(append(sList(symbol_quote), sList(name_reg))), sList(symbol_args)));
            pc = apply_cont;
        } else {
            k_reg = make_cont("cont", 25, cdrs_reg, fields_reg, name_reg, k_reg);
            cdrs_reg = append(sList(symbol_cdr), sList(cdrs_reg));
            fields_reg = cdr_hat(fields_reg);
            pc = verify_dd_constructor_fields_hat;
        }
    }
    
    public static object make_macro_env_hat() {
        return make_initial_environment(sList(symbol_and, symbol_or, symbol_cond, symbol_let, symbol_letrec, symbol_let_star, symbol_case, symbol_record_case, symbol_define_datatype, symbol_cases), sList(and_transformer_hat, or_transformer_hat, cond_transformer_hat, let_transformer_hat, letrec_transformer_hat, let_star_transformer_hat, case_transformer_hat, record_case_transformer_hat, define_datatype_transformer_hat, cases_transformer_hat));
    }
    
    public static object make_pattern_macro_hat(object clauses, object aclauses) {
        return sList(symbol_pattern_macro, clauses, aclauses);
    }
    
    public static bool pattern_macro_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), symbol_pattern_macro)));
    }
    
    public static object macro_clauses(object macro) {
        return cadr(macro);
    }
    
    public static object macro_aclauses(object macro) {
        return caddr(macro);
    }
    
    public static bool define_syntax_clause_q(object x) {
        return (true_q(list_q(x)) && true_q(Equal(length(x), 2)) && true_q(pattern_q(car(x))) && true_q(pattern_q(cadr(x))));
    }
    
    public static bool define_syntax_clause_q_hat(object x) {
        return (true_q(list_q_hat(x)) && true_q(Equal(length_hat(x), 2)) && true_q(apattern_q(car_hat(x))) && true_q(apattern_q(cadr_hat(x))));
    }
    
    public static bool apattern_q(object x) {
        return (true_q(aatom_q(x)) || true_q((true_q(apair_q(x)) && true_q(apattern_q(cadr(x))) && true_q(apattern_q(caddr(x))))));
    }
    
    public static bool list_of_define_syntax_clauses_q_hat(object alist) {
        return (true_q(null_q_hat(alist)) || true_q((true_q(define_syntax_clause_q_hat(car_hat(alist))) && true_q(list_of_define_syntax_clauses_q_hat(cdr_hat(alist))))));
    }
    
    public static void expand_once_hat() {
        object macro_keyword = symbol_undefined;
        macro_keyword = untag_atom_hat(car_hat(adatum_reg));
        object macro = symbol_undefined;
        macro = get_first_frame_value(macro_keyword, macro_env);
        if (true_q(pattern_macro_q(macro))) {
            k_reg = make_cont2("cont2", 47, macro_keyword, k_reg);
            aclauses_reg = macro_aclauses(macro);
            clauses_reg = macro_clauses(macro);
            pc = process_macro_clauses_hat;
        } else {
            k_reg = make_cont("cont", 27, adatum_reg, macro_keyword, fail_reg, k_reg);
            datum_reg = adatum_reg;
            macro_reg = macro;
            pc = apply_macro;
        }
    }
    
    public static void process_macro_clauses_hat() {
        if (true_q(null_q(clauses_reg))) {
            msg_reg = "no matching clause found for";
            pc = aparse_error;
        } else {
            object left_pattern = symbol_undefined;
            object right_pattern = symbol_undefined;
            object left_apattern = symbol_undefined;
            object right_apattern = symbol_undefined;
            right_apattern = cadar_hat(aclauses_reg);
            left_apattern = caar_hat(aclauses_reg);
            right_pattern = cadar(clauses_reg);
            left_pattern = caar(clauses_reg);
            k_reg = make_cont("cont", 29, aclauses_reg, adatum_reg, clauses_reg, left_apattern, left_pattern, right_apattern, right_pattern, handler_reg, fail_reg, k_reg);
            x_reg = adatum_reg;
            pc = unannotate_cps;
        }
    }
    
    public static void qq_expand_cps() {
        if (true_q(quasiquote_q_hat(ax_reg))) {
            k_reg = make_cont("cont", 35, k_reg);
            depth_reg = Add(depth_reg, 1);
            ax_reg = cdr_hat(ax_reg);
            pc = qq_expand_cps;
        } else {
            if (true_q((true_q(unquote_q_hat(ax_reg)) || true_q(unquote_splicing_q_hat(ax_reg))))) {
                if (true_q(GreaterThan(depth_reg, 0))) {
                    k_reg = make_cont("cont", 34, ax_reg, k_reg);
                    depth_reg = Subtract(depth_reg, 1);
                    ax_reg = cdr_hat(ax_reg);
                    pc = qq_expand_cps;
                } else {
                    if (true_q((true_q(unquote_q_hat(ax_reg)) && true_q((! true_q(null_q_hat(cdr_hat(ax_reg))))) && true_q(null_q_hat(cddr_hat(ax_reg)))))) {
                        value_reg = cadr_hat(ax_reg);
                        pc = apply_cont;
                    } else {
                        value_reg = append(sList(symbol_quote), sList(ax_reg));
                        pc = apply_cont;
                    }
                }
            } else {
                if (true_q(vector_q_hat(ax_reg))) {
                    k_reg = make_cont("cont", 33, depth_reg, k_reg);
                    info_reg = symbol_none;
                    x_reg = vector_to_list_hat(ax_reg);
                    pc = annotate_cps;
                } else {
                    if (true_q((! true_q(pair_q_hat(ax_reg))))) {
                        value_reg = append(sList(symbol_quote), sList(ax_reg));
                        pc = apply_cont;
                    } else {
                        if (true_q(null_q_hat(cdr_hat(ax_reg)))) {
                            ax_reg = car_hat(ax_reg);
                            pc = qq_expand_list_cps;
                        } else {
                            k_reg = make_cont("cont", 31, ax_reg, depth_reg, k_reg);
                            ax_reg = car_hat(ax_reg);
                            pc = qq_expand_list_cps;
                        }
                    }
                }
            }
        }
    }
    
    public static void qq_expand_list_cps() {
        if (true_q(quasiquote_q_hat(ax_reg))) {
            k_reg = make_cont("cont", 40, k_reg);
            depth_reg = Add(depth_reg, 1);
            ax_reg = cdr_hat(ax_reg);
            pc = qq_expand_cps;
        } else {
            if (true_q((true_q(unquote_q_hat(ax_reg)) || true_q(unquote_splicing_q_hat(ax_reg))))) {
                if (true_q(GreaterThan(depth_reg, 0))) {
                    k_reg = make_cont("cont", 39, ax_reg, k_reg);
                    depth_reg = Subtract(depth_reg, 1);
                    ax_reg = cdr_hat(ax_reg);
                    pc = qq_expand_cps;
                } else {
                    if (true_q(unquote_q_hat(ax_reg))) {
                        value_reg = append(sList(symbol_sList), cdr_hat(ax_reg));
                        pc = apply_cont;
                    } else {
                        if (true_q(null_q_hat(cddr_hat(ax_reg)))) {
                            value_reg = cadr_hat(ax_reg);
                            pc = apply_cont;
                        } else {
                            value_reg = append(sList(symbol_append), cdr_hat(ax_reg));
                            pc = apply_cont;
                        }
                    }
                }
            } else {
                if (true_q(vector_q_hat(ax_reg))) {
                    k_reg = make_cont("cont", 38, k_reg);
                    pc = qq_expand_cps;
                } else {
                    if (true_q((! true_q(pair_q_hat(ax_reg))))) {
                        value_reg = append(sList(symbol_quote), sList(sList(ax_reg)));
                        pc = apply_cont;
                    } else {
                        if (true_q(null_q_hat(cdr_hat(ax_reg)))) {
                            k_reg = make_cont("cont", 38, k_reg);
                            ax_reg = car_hat(ax_reg);
                            pc = qq_expand_list_cps;
                        } else {
                            k_reg = make_cont("cont", 37, ax_reg, depth_reg, k_reg);
                            ax_reg = car_hat(ax_reg);
                            pc = qq_expand_list_cps;
                        }
                    }
                }
            }
        }
    }
    
    public static object aunparse(object aexp) {
        if (true_q(Eq(car(aexp), symbol_lit_aexp))) {
            object datum = symbol_undefined;
            datum = list_ref(aexp, 1);
            if (true_q(literal_q(datum))) {
                return datum;
            } else {
                if (true_q(vector_q(datum))) {
                    return datum;
                } else {
                    return append(sList(symbol_quote), sList(datum));
                }
            }
        } else {
            if (true_q(Eq(car(aexp), symbol_var_aexp))) {
                object id = symbol_undefined;
                id = list_ref(aexp, 1);
                return id;
            } else {
                if (true_q(Eq(car(aexp), symbol_lexical_address_aexp))) {
                    object id = symbol_undefined;
                    id = list_ref(aexp, 3);
                    return id;
                } else {
                    if (true_q(Eq(car(aexp), symbol_if_aexp))) {
                        object test_aexp = symbol_undefined;
                        object then_aexp = symbol_undefined;
                        object else_aexp = symbol_undefined;
                        else_aexp = list_ref(aexp, 3);
                        then_aexp = list_ref(aexp, 2);
                        test_aexp = list_ref(aexp, 1);
                        return append(sList(symbol_if), append(sList(aunparse(test_aexp)), append(sList(aunparse(then_aexp)), sList(aunparse(else_aexp)))));
                    } else {
                        if (true_q(Eq(car(aexp), symbol_assign_aexp))) {
                            object var = symbol_undefined;
                            object rhs_exp = symbol_undefined;
                            rhs_exp = list_ref(aexp, 2);
                            var = list_ref(aexp, 1);
                            return append(sList(symbol_set_b), append(sList(var), sList(aunparse(rhs_exp))));
                        } else {
                            if (true_q(Eq(car(aexp), symbol_func_aexp))) {
                                object exp = symbol_undefined;
                                exp = list_ref(aexp, 1);
                                return append(sList(symbol_func), sList(aunparse(exp)));
                            } else {
                                if (true_q(Eq(car(aexp), symbol_callback0_aexp))) {
                                    object exp = symbol_undefined;
                                    exp = list_ref(aexp, 1);
                                    return append(sList(symbol_callback0), sList(aunparse(exp)));
                                } else {
                                    if (true_q(Eq(car(aexp), symbol_callback1_aexp))) {
                                        object exp = symbol_undefined;
                                        exp = list_ref(aexp, 1);
                                        return append(sList(symbol_callback1), sList(aunparse(exp)));
                                    } else {
                                        if (true_q(Eq(car(aexp), symbol_callback2_aexp))) {
                                            object exp = symbol_undefined;
                                            exp = list_ref(aexp, 1);
                                            return append(sList(symbol_callback2), sList(aunparse(exp)));
                                        } else {
                                            if (true_q(Eq(car(aexp), symbol_define_aexp))) {
                                                object id = symbol_undefined;
                                                object docstring = symbol_undefined;
                                                object rhs_exp = symbol_undefined;
                                                rhs_exp = list_ref(aexp, 3);
                                                docstring = list_ref(aexp, 2);
                                                id = list_ref(aexp, 1);
                                                if (true_q(string_is__q(docstring, ""))) {
                                                    return append(sList(symbol_define), append(sList(id), sList(aunparse(rhs_exp))));
                                                } else {
                                                    return append(sList(symbol_define), append(sList(id), append(sList(docstring), sList(aunparse(rhs_exp)))));
                                                }
                                            } else {
                                                if (true_q(Eq(car(aexp), symbol_define_b_aexp))) {
                                                    object id = symbol_undefined;
                                                    object docstring = symbol_undefined;
                                                    object rhs_exp = symbol_undefined;
                                                    rhs_exp = list_ref(aexp, 3);
                                                    docstring = list_ref(aexp, 2);
                                                    id = list_ref(aexp, 1);
                                                    if (true_q(string_is__q(docstring, ""))) {
                                                        return append(sList(symbol_define_b), append(sList(id), sList(aunparse(rhs_exp))));
                                                    } else {
                                                        return append(sList(symbol_define_b), append(sList(id), append(sList(docstring), sList(aunparse(rhs_exp)))));
                                                    }
                                                } else {
                                                    if (true_q(Eq(car(aexp), symbol_define_syntax_aexp))) {
                                                        object name = symbol_undefined;
                                                        object clauses = symbol_undefined;
                                                        clauses = list_ref(aexp, 2);
                                                        name = list_ref(aexp, 1);
                                                        return append(sList(symbol_define_syntax), append(sList(name), clauses));
                                                    } else {
                                                        if (true_q(Eq(car(aexp), symbol_begin_aexp))) {
                                                            object exps = symbol_undefined;
                                                            exps = list_ref(aexp, 1);
                                                            return append(sList(symbol_begin), map(aunparse_proc, exps));
                                                        } else {
                                                            if (true_q(Eq(car(aexp), symbol_lambda_aexp))) {
                                                                object formals = symbol_undefined;
                                                                object bodies = symbol_undefined;
                                                                bodies = list_ref(aexp, 2);
                                                                formals = list_ref(aexp, 1);
                                                                return append(sList(symbol_lambda), append(sList(formals), map(aunparse_proc, bodies)));
                                                            } else {
                                                                if (true_q(Eq(car(aexp), symbol_mu_lambda_aexp))) {
                                                                    object formals = symbol_undefined;
                                                                    object runt = symbol_undefined;
                                                                    object bodies = symbol_undefined;
                                                                    bodies = list_ref(aexp, 3);
                                                                    runt = list_ref(aexp, 2);
                                                                    formals = list_ref(aexp, 1);
                                                                    return append(sList(symbol_lambda), append(sList(append(formals, runt)), map(aunparse_proc, bodies)));
                                                                } else {
                                                                    if (true_q(Eq(car(aexp), symbol_app_aexp))) {
                                                                        object operator_ = symbol_undefined;
                                                                        object operands = symbol_undefined;
                                                                        operands = list_ref(aexp, 2);
                                                                        operator_ = list_ref(aexp, 1);
                                                                        return append(sList(aunparse(operator_)), map(aunparse_proc, operands));
                                                                    } else {
                                                                        if (true_q(Eq(car(aexp), symbol_try_catch_aexp))) {
                                                                            object body = symbol_undefined;
                                                                            object catch_var = symbol_undefined;
                                                                            object catch_exps = symbol_undefined;
                                                                            catch_exps = list_ref(aexp, 3);
                                                                            catch_var = list_ref(aexp, 2);
                                                                            body = list_ref(aexp, 1);
                                                                            return append(sList(symbol_try), append(sList(aunparse(body)), sList(append(sList(symbol_catch), append(sList(catch_var), map(aunparse_proc, catch_exps))))));
                                                                        } else {
                                                                            if (true_q(Eq(car(aexp), symbol_try_finally_aexp))) {
                                                                                object body = symbol_undefined;
                                                                                object finally_exps = symbol_undefined;
                                                                                finally_exps = list_ref(aexp, 2);
                                                                                body = list_ref(aexp, 1);
                                                                                return append(sList(symbol_try), append(sList(aunparse(body)), sList(append(sList(symbol_finally), map(aunparse_proc, finally_exps)))));
                                                                            } else {
                                                                                if (true_q(Eq(car(aexp), symbol_try_catch_finally_aexp))) {
                                                                                    object body = symbol_undefined;
                                                                                    object catch_var = symbol_undefined;
                                                                                    object catch_exps = symbol_undefined;
                                                                                    object finally_exps = symbol_undefined;
                                                                                    finally_exps = list_ref(aexp, 4);
                                                                                    catch_exps = list_ref(aexp, 3);
                                                                                    catch_var = list_ref(aexp, 2);
                                                                                    body = list_ref(aexp, 1);
                                                                                    return append(sList(symbol_try), append(sList(aunparse(body)), append(sList(append(sList(symbol_catch), append(sList(catch_var), map(aunparse_proc, catch_exps)))), sList(append(sList(symbol_finally), map(aunparse_proc, finally_exps))))));
                                                                                } else {
                                                                                    if (true_q(Eq(car(aexp), symbol_raise_aexp))) {
                                                                                        object exp = symbol_undefined;
                                                                                        exp = list_ref(aexp, 1);
                                                                                        return append(sList(symbol_raise), sList(aunparse(exp)));
                                                                                    } else {
                                                                                        if (true_q(Eq(car(aexp), symbol_choose_aexp))) {
                                                                                            object exps = symbol_undefined;
                                                                                            exps = list_ref(aexp, 1);
                                                                                            return append(sList(symbol_choose), map(aunparse_proc, exps));
                                                                                        } else {
                                                                                            throw new Exception("symbol_aunparse: " + format("bad abstract syntax: ~s", aexp));;
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static bool exception_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), symbol_exception)));
    }
    
    public static object use_lexical_address(params object [] t_args) {
        object args = sList(t_args);
        if (true_q(null_q(args))) {
            return _staruse_lexical_address_star;
        } else {
            _staruse_lexical_address_star = true_q(car(args));
            return void_value;
        }
    }
    
    public static void handle_exception(object exc) {
        object stack = symbol_undefined;
        object message = symbol_undefined;
        object error_type = symbol_undefined;
        error_type = car(cadr(exc));
        message = cadr(cadr(exc));
        stack = cadddr(cddr(cadr(exc)));
        printf("~%Traceback (most recent call last):~%");
        while (true_q((! true_q(null_q(stack))))) {
            display(format_exception_line(car(stack)));
            stack = cdr(stack);
        }
        printf("~a: ~a~%", error_type, message);
    }
    
    public static object format_exception_line(object line) {
        object filename = symbol_undefined;
        object line_number = symbol_undefined;
        object column_number = symbol_undefined;
        column_number = caddr(line);
        line_number = cadr(line);
        filename = car(line);
        if (true_q(Equal(length(line), 3))) {
            return format("  File \"~a\", line ~a, col ~a~%", filename, line_number, column_number);
        } else {
            return format("  File \"~a\", line ~a, col ~a, in ~a~%", filename, line_number, column_number, cadddr(line));
        }
    }
    
    public static object start_rm() {
        toplevel_env = make_toplevel_env();
        macro_env = make_macro_env_hat();
        return read_eval_print_loop_rm();
    }
    
    public static object read_eval_print_loop_rm() {
        object input_ = symbol_undefined;
        input_ = read_line("==> ");
        object result = symbol_undefined;
        result = execute_rm(input_, symbol_stdin);
        while (true_q((! true_q(end_of_session_q(result))))) {
            if (true_q(exception_q(result))) {
                handle_exception(result);
            } else {
                if (true_q((! true_q(void_q(result))))) {
                    if (true_q(_starneed_newline_star)) {
                        newline();
                    }
                    safe_print(result);
                }
            }
            input_ = read_line("==> ");
            result = execute_rm(input_, symbol_stdin);
        }
        return symbol_goodbye;
    }
    
    public static object execute_string_rm(object input_) {
        return execute_rm(input_, symbol_stdin);
    }
    
    public static object execute_file_rm(object filename) {
        return execute_rm(read_content(filename), filename);
    }
    
    public static object execute_rm(object input_, object src) {
        load_stack = symbol_emptylist;
        initialize_execute_b();
        k_reg = REP_k;
        fail_reg = _starlast_fail_star;
        handler_reg = REP_handler;
        src_reg = src;
        input_reg = input_;
        pc = scan_input;
        object result = symbol_undefined;
        result = trampoline();
        if (true_q(exception_q(result))) {
            return result;
        } else {
            _startokens_left_star = result;
            if (true_q(token_type_q(first(_startokens_left_star), symbol_end_marker))) {
                return void_value;
            } else {
                return execute_loop_rm(src);
            }
        }
    }
    
    public static object execute_loop_rm(object src) {
        execute_next_expression_rm(src);
        object result = symbol_undefined;
        result = trampoline();
        if (true_q((true_q(exception_q(result)) || true_q(end_of_session_q(result)) || true_q(token_type_q(first(_startokens_left_star), symbol_end_marker))))) {
            return result;
        } else {
            return execute_loop_rm(src);
        }
    }
    
    public static void execute_next_expression_rm(object src) {
        k_reg = make_cont4("cont4", 10);
        fail_reg = _starlast_fail_star;
        handler_reg = REP_handler;
        src_reg = src;
        tokens_reg = _startokens_left_star;
        pc = read_sexp;
    }
    
    public static object try_parse(object input_) {
        load_stack = symbol_emptylist;
        k_reg = make_cont2("cont2", 52);
        fail_reg = _starlast_fail_star;
        handler_reg = try_parse_handler;
        src_reg = symbol_stdin;
        input_reg = input_;
        pc = scan_input;
        return trampoline();
    }
    
    public static void initialize_globals() {
        toplevel_env = make_toplevel_env();
        macro_env = make_macro_env_hat();
        load_stack = symbol_emptylist;
        initialize_execute_b();
        _starlast_fail_star = REP_fail;
    }
    
    public static object make_debugging_k(object exp, object k) {
        return make_cont2("cont2", 53, exp, k);
    }
    
    public static object get_use_stack_trace() {
        return _staruse_stack_trace_star;
    }
    
    public static void set_use_stack_trace_b(object value) {
        _staruse_stack_trace_star = true_q(value);
    }
    
    public static void initialize_stack_trace_b() {
        set_car_b(_starstack_trace_star, symbol_emptylist);
    }
    
    public static void initialize_execute_b() {
        _closure_depth = 0;
        _trace_pause = false;
        initialize_stack_trace_b();
    }
    
    public static void push_stack_trace_b(object exp) {
        set_car_b(_starstack_trace_star, cons(exp, car(_starstack_trace_star)));
    }
    
    public static void pop_stack_trace_b(object exp) {
        if (true_q((! true_q(null_q(car(_starstack_trace_star)))))) {
            set_car_b(_starstack_trace_star, cdr(car(_starstack_trace_star)));
        }
    }
    
    public static void m() {
        if (true_q(_startracing_on_q_star)) {
            highlight_expression(exp_reg);
        }
        object k = symbol_undefined;
        k = (_startracing_on_q_star ? make_debugging_k(exp_reg, k_reg) : k_reg);
        if (true_q(Eq(car(exp_reg), symbol_lit_aexp))) {
            object datum = symbol_undefined;
            datum = list_ref(exp_reg, 1);
            value2_reg = fail_reg;
            value1_reg = datum;
            k_reg = k;
            pc = apply_cont2;
        } else {
            if (true_q(Eq(car(exp_reg), symbol_var_aexp))) {
                object id = symbol_undefined;
                object info = symbol_undefined;
                info = list_ref(exp_reg, 2);
                id = list_ref(exp_reg, 1);
                k_reg = k;
                var_info_reg = info;
                var_reg = id;
                pc = lookup_value;
            } else {
                if (true_q(Eq(car(exp_reg), symbol_lexical_address_aexp))) {
                    object depth = symbol_undefined;
                    object offset = symbol_undefined;
                    offset = list_ref(exp_reg, 2);
                    depth = list_ref(exp_reg, 1);
                    k_reg = k;
                    frames_reg = frames(env_reg);
                    offset_reg = offset;
                    depth_reg = depth;
                    pc = lookup_value_by_lexical_address;
                } else {
                    if (true_q(Eq(car(exp_reg), symbol_func_aexp))) {
                        object exp = symbol_undefined;
                        exp = list_ref(exp_reg, 1);
                        k_reg = make_cont2("cont2", 71, k);
                        exp_reg = exp;
                        pc = m;
                    } else {
                        if (true_q(Eq(car(exp_reg), symbol_callback0_aexp))) {
                            object exp = symbol_undefined;
                            exp = list_ref(exp_reg, 1);
                            k_reg = make_cont2("cont2", 70, k);
                            exp_reg = exp;
                            pc = m;
                        } else {
                            if (true_q(Eq(car(exp_reg), symbol_callback1_aexp))) {
                                object exp = symbol_undefined;
                                exp = list_ref(exp_reg, 1);
                                k_reg = make_cont2("cont2", 69, k);
                                exp_reg = exp;
                                pc = m;
                            } else {
                                if (true_q(Eq(car(exp_reg), symbol_callback2_aexp))) {
                                    object exp = symbol_undefined;
                                    exp = list_ref(exp_reg, 1);
                                    k_reg = make_cont2("cont2", 68, k);
                                    exp_reg = exp;
                                    pc = m;
                                } else {
                                    if (true_q(Eq(car(exp_reg), symbol_if_aexp))) {
                                        object test_exp = symbol_undefined;
                                        object then_exp = symbol_undefined;
                                        object else_exp = symbol_undefined;
                                        else_exp = list_ref(exp_reg, 3);
                                        then_exp = list_ref(exp_reg, 2);
                                        test_exp = list_ref(exp_reg, 1);
                                        k_reg = make_cont2("cont2", 67, else_exp, then_exp, env_reg, handler_reg, k);
                                        exp_reg = test_exp;
                                        pc = m;
                                    } else {
                                        if (true_q(Eq(car(exp_reg), symbol_assign_aexp))) {
                                            object var = symbol_undefined;
                                            object rhs_exp = symbol_undefined;
                                            object var_info = symbol_undefined;
                                            var_info = list_ref(exp_reg, 3);
                                            rhs_exp = list_ref(exp_reg, 2);
                                            var = list_ref(exp_reg, 1);
                                            k_reg = make_cont2("cont2", 66, var, var_info, env_reg, handler_reg, k);
                                            exp_reg = rhs_exp;
                                            pc = m;
                                        } else {
                                            if (true_q(Eq(car(exp_reg), symbol_define_aexp))) {
                                                object var = symbol_undefined;
                                                object docstring = symbol_undefined;
                                                object rhs_exp = symbol_undefined;
                                                rhs_exp = list_ref(exp_reg, 3);
                                                docstring = list_ref(exp_reg, 2);
                                                var = list_ref(exp_reg, 1);
                                                k_reg = make_cont2("cont2", 63, docstring, var, env_reg, handler_reg, k);
                                                exp_reg = rhs_exp;
                                                pc = m;
                                            } else {
                                                if (true_q(Eq(car(exp_reg), symbol_define_b_aexp))) {
                                                    object var = symbol_undefined;
                                                    object docstring = symbol_undefined;
                                                    object rhs_exp = symbol_undefined;
                                                    rhs_exp = list_ref(exp_reg, 3);
                                                    docstring = list_ref(exp_reg, 2);
                                                    var = list_ref(exp_reg, 1);
                                                    k_reg = make_cont2("cont2", 61, docstring, var, k);
                                                    exp_reg = rhs_exp;
                                                    pc = m;
                                                } else {
                                                    if (true_q(Eq(car(exp_reg), symbol_define_syntax_aexp))) {
                                                        object name = symbol_undefined;
                                                        object clauses = symbol_undefined;
                                                        object aclauses = symbol_undefined;
                                                        aclauses = list_ref(exp_reg, 3);
                                                        clauses = list_ref(exp_reg, 2);
                                                        name = list_ref(exp_reg, 1);
                                                        k_reg = make_cont2("cont2", 60, aclauses, clauses, k);
                                                        env_reg = macro_env;
                                                        var_reg = name;
                                                        pc = lookup_binding_in_first_frame;
                                                    } else {
                                                        if (true_q(Eq(car(exp_reg), symbol_begin_aexp))) {
                                                            object exps = symbol_undefined;
                                                            exps = list_ref(exp_reg, 1);
                                                            k_reg = k;
                                                            exps_reg = exps;
                                                            pc = eval_sequence;
                                                        } else {
                                                            if (true_q(Eq(car(exp_reg), symbol_lambda_aexp))) {
                                                                object formals = symbol_undefined;
                                                                object bodies = symbol_undefined;
                                                                bodies = list_ref(exp_reg, 2);
                                                                formals = list_ref(exp_reg, 1);
                                                                value2_reg = fail_reg;
                                                                value1_reg = closure(formals, bodies, env_reg);
                                                                k_reg = k;
                                                                pc = apply_cont2;
                                                            } else {
                                                                if (true_q(Eq(car(exp_reg), symbol_mu_lambda_aexp))) {
                                                                    object formals = symbol_undefined;
                                                                    object runt = symbol_undefined;
                                                                    object bodies = symbol_undefined;
                                                                    bodies = list_ref(exp_reg, 3);
                                                                    runt = list_ref(exp_reg, 2);
                                                                    formals = list_ref(exp_reg, 1);
                                                                    value2_reg = fail_reg;
                                                                    value1_reg = mu_closure(formals, runt, bodies, env_reg);
                                                                    k_reg = k;
                                                                    pc = apply_cont2;
                                                                } else {
                                                                    if (true_q(Eq(car(exp_reg), symbol_trace_lambda_aexp))) {
                                                                        object name = symbol_undefined;
                                                                        object formals = symbol_undefined;
                                                                        object bodies = symbol_undefined;
                                                                        bodies = list_ref(exp_reg, 3);
                                                                        formals = list_ref(exp_reg, 2);
                                                                        name = list_ref(exp_reg, 1);
                                                                        value2_reg = fail_reg;
                                                                        value1_reg = trace_closure(name, formals, bodies, env_reg);
                                                                        k_reg = k;
                                                                        pc = apply_cont2;
                                                                    } else {
                                                                        if (true_q(Eq(car(exp_reg), symbol_mu_trace_lambda_aexp))) {
                                                                            object name = symbol_undefined;
                                                                            object formals = symbol_undefined;
                                                                            object runt = symbol_undefined;
                                                                            object bodies = symbol_undefined;
                                                                            bodies = list_ref(exp_reg, 4);
                                                                            runt = list_ref(exp_reg, 3);
                                                                            formals = list_ref(exp_reg, 2);
                                                                            name = list_ref(exp_reg, 1);
                                                                            value2_reg = fail_reg;
                                                                            value1_reg = mu_trace_closure(name, formals, runt, bodies, env_reg);
                                                                            k_reg = k;
                                                                            pc = apply_cont2;
                                                                        } else {
                                                                            if (true_q(Eq(car(exp_reg), symbol_try_catch_aexp))) {
                                                                                object body = symbol_undefined;
                                                                                object cvar = symbol_undefined;
                                                                                object cexps = symbol_undefined;
                                                                                cexps = list_ref(exp_reg, 3);
                                                                                cvar = list_ref(exp_reg, 2);
                                                                                body = list_ref(exp_reg, 1);
                                                                                object new_handler = symbol_undefined;
                                                                                new_handler = try_catch_handler(cvar, cexps, env_reg, handler_reg, k);
                                                                                k_reg = k;
                                                                                handler_reg = new_handler;
                                                                                exp_reg = body;
                                                                                pc = m;
                                                                            } else {
                                                                                if (true_q(Eq(car(exp_reg), symbol_try_finally_aexp))) {
                                                                                    object body = symbol_undefined;
                                                                                    object fexps = symbol_undefined;
                                                                                    fexps = list_ref(exp_reg, 2);
                                                                                    body = list_ref(exp_reg, 1);
                                                                                    object new_handler = symbol_undefined;
                                                                                    new_handler = try_finally_handler(fexps, env_reg, handler_reg);
                                                                                    k_reg = make_cont2("cont2", 59, fexps, env_reg, handler_reg, k);
                                                                                    handler_reg = new_handler;
                                                                                    exp_reg = body;
                                                                                    pc = m;
                                                                                } else {
                                                                                    if (true_q(Eq(car(exp_reg), symbol_try_catch_finally_aexp))) {
                                                                                        object body = symbol_undefined;
                                                                                        object cvar = symbol_undefined;
                                                                                        object cexps = symbol_undefined;
                                                                                        object fexps = symbol_undefined;
                                                                                        fexps = list_ref(exp_reg, 4);
                                                                                        cexps = list_ref(exp_reg, 3);
                                                                                        cvar = list_ref(exp_reg, 2);
                                                                                        body = list_ref(exp_reg, 1);
                                                                                        object new_handler = symbol_undefined;
                                                                                        new_handler = try_catch_finally_handler(cvar, cexps, fexps, env_reg, handler_reg, k);
                                                                                        k_reg = make_cont2("cont2", 59, fexps, env_reg, handler_reg, k);
                                                                                        handler_reg = new_handler;
                                                                                        exp_reg = body;
                                                                                        pc = m;
                                                                                    } else {
                                                                                        if (true_q(Eq(car(exp_reg), symbol_raise_aexp))) {
                                                                                            object exp = symbol_undefined;
                                                                                            exp = list_ref(exp_reg, 1);
                                                                                            k_reg = make_cont2("cont2", 57, handler_reg);
                                                                                            exp_reg = exp;
                                                                                            pc = m;
                                                                                        } else {
                                                                                            if (true_q(Eq(car(exp_reg), symbol_choose_aexp))) {
                                                                                                object exps = symbol_undefined;
                                                                                                exps = list_ref(exp_reg, 1);
                                                                                                k_reg = k;
                                                                                                exps_reg = exps;
                                                                                                pc = eval_choices;
                                                                                            } else {
                                                                                                if (true_q(Eq(car(exp_reg), symbol_app_aexp))) {
                                                                                                    object operator_ = symbol_undefined;
                                                                                                    object operands = symbol_undefined;
                                                                                                    object info = symbol_undefined;
                                                                                                    info = list_ref(exp_reg, 3);
                                                                                                    operands = list_ref(exp_reg, 2);
                                                                                                    operator_ = list_ref(exp_reg, 1);
                                                                                                    k_reg = make_cont2("cont2", 56, exp_reg, operator_, env_reg, info, handler_reg, k);
                                                                                                    exps_reg = operands;
                                                                                                    pc = m_star;
                                                                                                } else {
                                                                                                    throw new Exception("symbol_m: " + format("bad abstract syntax: '~s'", exp_reg));;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    public static object make_exception(object exception, object message, object source, object line, object column) {
        return sList(exception, message, source, line, column, make_stack_trace());
    }
    
    public static object make_stack_trace() {
        object trace = symbol_undefined;
        trace = car(_starstack_trace_star);
        return reverse(map(format_stack_trace_proc, trace));
    }
    
    public static object get_procedure_name(object aexp) {
        if (true_q(macro_derived_source_info_q(aexp))) {
            return rac(get_source_info(aexp));
        } else {
            if (true_q(Eq(car(aexp), symbol_app_aexp))) {
                object operator_ = symbol_undefined;
                operator_ = list_ref(aexp, 1);
                if (true_q(Eq(car(operator_), symbol_lexical_address_aexp))) {
                    object id = symbol_undefined;
                    id = list_ref(operator_, 3);
                    return id;
                } else {
                    if (true_q(Eq(car(operator_), symbol_var_aexp))) {
                        object id = symbol_undefined;
                        id = list_ref(operator_, 1);
                        return id;
                    } else {
                        if (true_q(Eq(car(operator_), symbol_lambda_aexp))) {
                            object formals = symbol_undefined;
                            formals = list_ref(operator_, 1);
                            return append(sList(symbol_lambda), append(sList(formals), sList(symbol_dotdotdot)));
                        } else {
                            if (true_q(Eq(car(operator_), symbol_mu_lambda_aexp))) {
                                object formals = symbol_undefined;
                                object runt = symbol_undefined;
                                runt = list_ref(operator_, 2);
                                formals = list_ref(operator_, 1);
                                return append(sList(symbol_lambda), append(sList(append(formals, runt)), sList(symbol_dotdotdot)));
                            } else {
                                if (true_q(Eq(car(operator_), symbol_trace_lambda_aexp))) {
                                    object name = symbol_undefined;
                                    name = list_ref(operator_, 1);
                                    return name;
                                } else {
                                    if (true_q(Eq(car(operator_), symbol_mu_trace_lambda_aexp))) {
                                        object name = symbol_undefined;
                                        name = list_ref(operator_, 1);
                                        return name;
                                    } else {
                                        return symbol_application;
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                return symbol_unknown;
            }
        }
    }
    
    public static object format_stack_trace(object exp) {
        object info = symbol_undefined;
        info = rac(exp);
        if (true_q(Eq(info, symbol_none))) {
            return symbol_macro_generated_exp;
        } else {
            return sList(get_srcfile(info), get_start_line(info), get_start_char(info), get_procedure_name(exp));
        }
    }
    
    public static void runtime_error() {
        if (true_q(Eq(info_reg, symbol_none))) {
            exception_reg = make_exception("RunTimeError", msg_reg, symbol_none, symbol_none, symbol_none);
            pc = apply_handler2;
        } else {
            object src = symbol_undefined;
            object line_number = symbol_undefined;
            object char_number = symbol_undefined;
            char_number = get_start_char(info_reg);
            line_number = get_start_line(info_reg);
            src = get_srcfile(info_reg);
            exception_reg = make_exception("RunTimeError", msg_reg, src, line_number, char_number);
            pc = apply_handler2;
        }
    }
    
    public static void m_star() {
        if (true_q(null_q(exps_reg))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            k_reg = make_cont2("cont2", 72, exps_reg, env_reg, handler_reg, k_reg);
            exp_reg = car(exps_reg);
            pc = m;
        }
    }
    
    public static void eval_sequence() {
        if (true_q(null_q(cdr(exps_reg)))) {
            exp_reg = car(exps_reg);
            pc = m;
        } else {
            k_reg = make_cont2("cont2", 73, exps_reg, env_reg, handler_reg, k_reg);
            exp_reg = car(exps_reg);
            pc = m;
        }
    }
    
    public static object try_catch_handler(object cvar, object cexps, object env, object handler, object k) {
        return make_handler2("handler2", 4, cexps, cvar, env, handler, k);
    }
    
    public static object try_finally_handler(object fexps, object env, object handler) {
        return make_handler2("handler2", 5, fexps, env, handler);
    }
    
    public static object try_catch_finally_handler(object cvar, object cexps, object fexps, object env, object handler, object k) {
        return make_handler2("handler2", 6, cexps, cvar, fexps, env, handler, k);
    }
    
    public static void eval_choices() {
        if (true_q(null_q(exps_reg))) {
            pc = apply_fail;
        } else {
            object new_fail = symbol_undefined;
            new_fail = make_fail("fail", 5, exps_reg, env_reg, handler_reg, fail_reg, k_reg);
            fail_reg = new_fail;
            exp_reg = car(exps_reg);
            pc = m;
        }
    }
    
    public static object closure(object formals, object bodies, object env) {
        return make_proc("proc", 1, bodies, formals, env);
    }
    
    public static object mu_closure(object formals, object runt, object bodies, object env) {
        return make_proc("proc", 2, bodies, formals, runt, env);
    }
    
    public static object make_trace_depth_string(object level) {
        if (true_q(Equal(level, 0))) {
            return "";
        } else {
            return string_append(" |", make_trace_depth_string(Subtract(level, 1)));
        }
    }
    
    public static object trace_closure(object name, object formals, object bodies, object env) {
        object trace_depth = symbol_undefined;
        trace_depth = 0;
        return make_proc("proc", 3, bodies, name, trace_depth, formals, env);
    }
    
    public static bool continuation_object_q(object x) {
        return (true_q(pair_q(x)) && true_q(memq(car(x), sList(symbol_continuation, symbol_continuation2, symbol_continuation3, symbol_continuation4))));
    }
    
    public static object mu_trace_closure(object name, object formals, object runt, object bodies, object env) {
        object trace_depth = symbol_undefined;
        trace_depth = 0;
        return make_proc("proc", 4, bodies, name, trace_depth, formals, runt, env);
    }
    
    public static bool length_one_q(object ls) {
        return (true_q((! true_q(null_q(ls)))) && true_q(null_q(cdr(ls))));
    }
    
    public static bool length_two_q(object ls) {
        return (true_q((! true_q(null_q(ls)))) && true_q((! true_q(null_q(cdr(ls))))) && true_q(null_q(cddr(ls))));
    }
    
    public static bool length_at_least_q(object n, object ls) {
        if (true_q(LessThan(n, 1))) {
            return true;
        } else {
            if (true_q((true_q(null_q(ls)) || true_q((! true_q(pair_q(ls))))))) {
                return false;
            } else {
                return length_at_least_q(Subtract(n, 1), cdr(ls));
            }
        }
    }
    
    public static bool all_numeric_q(object ls) {
        return (true_q(null_q(ls)) || true_q((true_q(number_q(car(ls))) && true_q(all_numeric_q(cdr(ls))))));
    }
    
    public static bool all_char_q(object ls) {
        return (true_q(null_q(ls)) || true_q((true_q(char_q(car(ls))) && true_q(all_char_q(cdr(ls))))));
    }
    
    public static bool void_q(object x) {
        return Eq(x, void_value);
    }
    
    public static bool end_of_session_q(object x) {
        return Eq(x, end_of_session);
    }
    
    public static bool procedure_object_q(object x) {
        return (true_q(procedure_q(x)) || true_q((true_q(pair_q(x)) && true_q(Eq(car(x), symbol_procedure)))));
    }
    
    public static bool environment_object_q(object x) {
        return (true_q(pair_q(x)) && true_q(Eq(car(x), symbol_environment)));
    }
    
    public static bool ends_with_newline_q(object s) {
        object len = symbol_undefined;
        len = string_length(s);
        return Equal(substring(s, Subtract(len, 1), len), "\n");
    }
    
    public static void load_file() {
        if (true_q(member(filename_reg, load_stack))) {
            printf("skipping recursive load of ~a~%", filename_reg);
            value2_reg = fail_reg;
            value1_reg = void_value;
            pc = apply_cont2;
        } else {
            if (true_q((! true_q(string_q(filename_reg))))) {
                msg_reg = format("filename '~a' is not a string", filename_reg);
                pc = runtime_error;
            } else {
                if (true_q((! true_q(file_exists_q(filename_reg))))) {
                    msg_reg = format("attempted to load nonexistent file '~a'", filename_reg);
                    pc = runtime_error;
                } else {
                    load_stack = cons(filename_reg, load_stack);
                    k_reg = make_cont2("cont2", 81, filename_reg, env2_reg, handler_reg, k_reg);
                    src_reg = filename_reg;
                    input_reg = read_content(filename_reg);
                    pc = scan_input;
                }
            }
        }
    }
    
    public static void read_and_eval_asexps() {
        if (true_q(token_type_q(first(tokens_reg), symbol_end_marker))) {
            value2_reg = fail_reg;
            value1_reg = void_value;
            pc = apply_cont2;
        } else {
            k_reg = make_cont4("cont4", 13, src_reg, env2_reg, handler_reg, k_reg);
            pc = read_sexp;
        }
    }
    
    public static void load_files() {
        if (true_q(null_q(filenames_reg))) {
            value2_reg = fail_reg;
            value1_reg = void_value;
            pc = apply_cont2;
        } else {
            k_reg = make_cont2("cont2", 84, filenames_reg, env2_reg, info_reg, handler_reg, k_reg);
            filename_reg = car(filenames_reg);
            pc = load_file;
        }
    }
    
    public static void length_loop() {
        if (true_q(null_q(x_reg))) {
            value2_reg = fail_reg;
            value1_reg = sum_reg;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q((! true_q(pair_q(x_reg))))) {
                msg_reg = format("length called on improper list ~s", ls_reg);
                pc = runtime_error;
            } else {
                sum_reg = Add(sum_reg, 1);
                x_reg = cdr(x_reg);
                pc = length_loop;
            }
        }
    }
    
    public static void make_set() {
        if (true_q(null_q(lst_reg))) {
            value2_reg = fail_reg;
            value1_reg = lst_reg;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 85, lst_reg, k2_reg);
            lst_reg = cdr(lst_reg);
            pc = make_set;
        }
    }
    
    public static void equal_objects_q() {
        if (true_q((true_q((true_q(null_q(x_reg)) && true_q(null_q(y_reg)))) || true_q((true_q(boolean_q(x_reg)) && true_q(boolean_q(y_reg)) && true_q((true_q((true_q(x_reg) && true_q(y_reg))) || true_q((true_q((! true_q(x_reg))) && true_q((! true_q(y_reg))))))))) || true_q((true_q(symbol_q(x_reg)) && true_q(symbol_q(y_reg)) && true_q(Eq(x_reg, y_reg)))) || true_q((true_q(number_q(x_reg)) && true_q(number_q(y_reg)) && true_q(Equal(x_reg, y_reg)))) || true_q((true_q(char_q(x_reg)) && true_q(char_q(y_reg)) && true_q(char_is__q(x_reg, y_reg)))) || true_q((true_q(Eq(x_reg, void_value)) && true_q(Eq(y_reg, void_value)))) || true_q((true_q(string_q(x_reg)) && true_q(string_q(y_reg)) && true_q(string_is__q(x_reg, y_reg))))))) {
            value_reg = true;
            pc = apply_cont;
        } else {
            if (true_q((true_q(pair_q(x_reg)) && true_q(pair_q(y_reg))))) {
                k_reg = make_cont("cont", 45, x_reg, y_reg, k_reg);
                y_reg = car(y_reg);
                x_reg = car(x_reg);
                pc = equal_objects_q;
            } else {
                if (true_q((true_q(vector_q(x_reg)) && true_q(vector_q(y_reg)) && true_q(Equal(vector_length(x_reg), vector_length(y_reg)))))) {
                    i_reg = Subtract(vector_length(x_reg), 1);
                    v2_reg = y_reg;
                    v1_reg = x_reg;
                    pc = equal_vectors_q;
                } else {
                    value_reg = false;
                    pc = apply_cont;
                }
            }
        }
    }
    
    public static void equal_vectors_q() {
        if (true_q(LessThan(i_reg, 0))) {
            value_reg = true;
            pc = apply_cont;
        } else {
            k_reg = make_cont("cont", 46, i_reg, v1_reg, v2_reg, k_reg);
            y_reg = vector_ref(v2_reg, i_reg);
            x_reg = vector_ref(v1_reg, i_reg);
            pc = equal_objects_q;
        }
    }
    
    public static void member_loop() {
        if (true_q(null_q(y_reg))) {
            value2_reg = fail_reg;
            value1_reg = false;
            pc = apply_cont2;
        } else {
            if (true_q((! true_q(pair_q(y_reg))))) {
                msg_reg = format("member called on improper list ~s", ls_reg);
                pc = runtime_error;
            } else {
                k_reg = make_cont("cont", 47, ls_reg, x_reg, y_reg, info_reg, handler_reg, fail_reg, k_reg);
                y_reg = car(y_reg);
                pc = equal_objects_q;
            }
        }
    }
    
    public static void get_primitive() {
        object sym = symbol_undefined;
        sym = car(args_reg);
        k_reg = make_cont2("cont2", 87, args_reg, sym, info_reg, handler_reg, k_reg);
        var_info_reg = symbol_none;
        var_reg = sym;
        pc = lookup_value;
    }
    
    public static void append2() {
        if (true_q(null_q(ls1_reg))) {
            value2_reg = fail_reg;
            value1_reg = ls2_reg;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 88, ls1_reg, k2_reg);
            ls1_reg = cdr(ls1_reg);
            pc = append2;
        }
    }
    
    public static void append_all() {
        if (true_q(null_q(lists_reg))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(null_q(cdr(lists_reg)))) {
                value2_reg = fail_reg;
                value1_reg = car(lists_reg);
                k_reg = k2_reg;
                pc = apply_cont2;
            } else {
                if (true_q((! true_q(list_q(car(lists_reg)))))) {
                    msg_reg = format("append called on incorrect list structure ~s", car(lists_reg));
                    pc = runtime_error;
                } else {
                    k2_reg = make_cont2("cont2", 89, lists_reg, k2_reg);
                    lists_reg = cdr(lists_reg);
                    pc = append_all;
                }
            }
        }
    }
    
    public static object directory(object args, object env) {
        if (true_q((true_q(null_q(args)) || true_q(environment_q(car(args)))))) {
            return sort(symbolLessThan_q_proc, (null_q(args) ? append(get_variables_from_frames(frames(macro_env)), get_variables_from_frames(frames(env))) : get_variables_from_frames(frames(car(args)))));
        } else {
            return get_external_members(car(args));
        }
    }
    
    public static object get_variables_from_frame(object frame) {
        return cadr(frame);
    }
    
    public static object get_variables_from_frames(object frames) {
        return flatten(map(get_variables_from_frame_proc, frames));
    }
    
    public static bool symbolLessThan_q(object a, object b) {
        object a_string = symbol_undefined;
        object b_string = symbol_undefined;
        b_string = symbol_to_string(b);
        a_string = symbol_to_string(a);
        return stringLessThan_q(a_string, b_string);
    }
    
    public static object flatten(object lists) {
        if (true_q(null_q(lists))) {
            return symbol_emptylist;
        } else {
            if (true_q(list_q(car(lists)))) {
                return append(flatten(car(lists)), flatten(cdr(lists)));
            } else {
                return cons(car(lists), flatten(cdr(lists)));
            }
        }
    }
    
    public static void map_primitive() {
        if (true_q(iterator_q(car(args_reg)))) {
            generator_reg = car(args_reg);
            pc = iterate_collect;
        } else {
            object len = symbol_undefined;
            object list_args = symbol_undefined;
            list_args = listify(args_reg);
            len = length(args_reg);
            if (true_q(Equal(len, 1))) {
                list1_reg = car(list_args);
                pc = map1;
            } else {
                if (true_q(Equal(len, 2))) {
                    list2_reg = cadr(list_args);
                    list1_reg = car(list_args);
                    pc = map2;
                } else {
                    lists_reg = list_args;
                    pc = mapN;
                }
            }
        }
    }
    
    public static object listify(object arg_list) {
        if (true_q(null_q(arg_list))) {
            return symbol_emptylist;
        } else {
            if (true_q(list_q(car(arg_list)))) {
                return cons(car(arg_list), listify(cdr(arg_list)));
            } else {
                if (true_q(vector_q(car(arg_list)))) {
                    return cons(vector_to_list(car(arg_list)), listify(cdr(arg_list)));
                } else {
                    if (true_q(string_q(car(arg_list)))) {
                        return cons(string_to_list(car(arg_list)), listify(cdr(arg_list)));
                    } else {
                        throw new Exception("symbol_map: " + format("cannot use object type '~a' in map", get_type(car(arg_list))));;
                    }
                }
            }
        }
    }
    
    public static void iterate() {
        object iterator = symbol_undefined;
        iterator = get_iterator(generator_reg);
        iterator_reg = iterator;
        pc = iterate_continue;
    }
    
    public static void iterate_continue() {
        object item = symbol_undefined;
        item = next_item(iterator_reg);
        if (true_q(null_q(item))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 90, iterator_reg, proc_reg, env_reg, handler_reg, k_reg);
            info_reg = symbol_none;
            env2_reg = env_reg;
            args_reg = sList(item);
            pc = apply_proc;
        }
    }
    
    public static void iterate_collect() {
        object iterator = symbol_undefined;
        iterator = get_iterator(generator_reg);
        iterator_reg = iterator;
        pc = iterate_collect_continue;
    }
    
    public static void iterate_collect_continue() {
        object item = symbol_undefined;
        item = next_item(iterator_reg);
        if (true_q(null_q(item))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            k2_reg = make_cont2("cont2", 91, iterator_reg, proc_reg, env_reg, handler_reg, k_reg);
            info_reg = symbol_none;
            env2_reg = env_reg;
            args_reg = sList(item);
            pc = apply_proc;
        }
    }
    
    public static void map1() {
        if (true_q(null_q(list1_reg))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            if (true_q(dlr_proc_q(proc_reg))) {
                k_reg = make_cont2("cont2", 93, list1_reg, proc_reg, k_reg);
                list1_reg = cdr(list1_reg);
                pc = map1;
            } else {
                k2_reg = make_cont2("cont2", 92, list1_reg, proc_reg, env_reg, handler_reg, k_reg);
                info_reg = symbol_none;
                env2_reg = env_reg;
                args_reg = sList(car(list1_reg));
                pc = apply_proc;
            }
        }
    }
    
    public static void map2() {
        if (true_q(null_q(list1_reg))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            if (true_q(dlr_proc_q(proc_reg))) {
                k_reg = make_cont2("cont2", 95, list1_reg, list2_reg, proc_reg, k_reg);
                list2_reg = cdr(list2_reg);
                list1_reg = cdr(list1_reg);
                pc = map2;
            } else {
                k2_reg = make_cont2("cont2", 94, list1_reg, list2_reg, proc_reg, env_reg, handler_reg, k_reg);
                info_reg = symbol_none;
                env2_reg = env_reg;
                args_reg = sList(car(list1_reg), car(list2_reg));
                pc = apply_proc;
            }
        }
    }
    
    public static void mapN() {
        if (true_q(null_q(car(lists_reg)))) {
            value2_reg = fail_reg;
            value1_reg = symbol_emptylist;
            pc = apply_cont2;
        } else {
            if (true_q(dlr_proc_q(proc_reg))) {
                k_reg = make_cont2("cont2", 97, lists_reg, proc_reg, k_reg);
                lists_reg = map(cdr_proc, lists_reg);
                pc = mapN;
            } else {
                k2_reg = make_cont2("cont2", 96, lists_reg, proc_reg, env_reg, handler_reg, k_reg);
                info_reg = symbol_none;
                env2_reg = env_reg;
                args_reg = map(car_proc, lists_reg);
                pc = apply_proc;
            }
        }
    }
    
    public static void for_each_primitive() {
        if (true_q(iterator_q(car(lists_reg)))) {
            generator_reg = car(lists_reg);
            pc = iterate;
        } else {
            object arg_list = symbol_undefined;
            arg_list = listify(lists_reg);
            if (true_q(null_q(car(arg_list)))) {
                value2_reg = fail_reg;
                value1_reg = void_value;
                pc = apply_cont2;
            } else {
                if (true_q(dlr_proc_q(proc_reg))) {
                    dlr_apply(proc_reg, map(car_proc, arg_list));
                    lists_reg = map(cdr_proc, arg_list);
                    pc = for_each_primitive;
                } else {
                    k2_reg = make_cont2("cont2", 98, arg_list, proc_reg, env_reg, handler_reg, k_reg);
                    info_reg = symbol_none;
                    env2_reg = env_reg;
                    args_reg = map(car_proc, arg_list);
                    pc = apply_proc;
                }
            }
        }
    }
    
    public static object make_toplevel_env() {
        object primitives = symbol_undefined;
        primitives = sList(sList(symbol_Multiply, times_prim), sList(symbol_Add, plus_prim), sList(symbol_Subtract, minus_prim), sList(symbol_Divide, divide_prim), sList(symbol_p, modulo_prim), sList(symbol_LessThan, lt_prim), sList(symbol_LessThanEqual, lt_or_eq_prim), sList(symbol_Equal, equal_sign_prim), sList(symbol_GreaterThan, gt_prim), sList(symbol_GreaterThanEqual, gt_or_eq_prim), sList(symbol_abort, abort_prim), sList(symbol_abs, abs_prim), sList(symbol_append, append_prim), sList(symbol_apply, apply_prim), sList(symbol_assv, assv_prim), sList(symbol_boolean_q, boolean_q_prim), sList(symbol_caddr, caddr_prim), sList(symbol_cadr, cadr_prim), sList(symbol_call_with_current_continuation, call_cc_prim), sList(symbol_call_cc, call_cc_prim), sList(symbol_car, car_prim), sList(symbol_cdr, cdr_prim), sList(symbol_caaaar, caaaar_prim), sList(symbol_caaadr, caaadr_prim), sList(symbol_caaar, caaar_prim), sList(symbol_caadar, caadar_prim), sList(symbol_caaddr, caaddr_prim), sList(symbol_caadr, caadr_prim), sList(symbol_caar, caar_prim), sList(symbol_cadaar, cadaar_prim), sList(symbol_cadadr, cadadr_prim), sList(symbol_cadar, cadar_prim), sList(symbol_caddar, caddar_prim), sList(symbol_cadddr, cadddr_prim), sList(symbol_cdaaar, cdaaar_prim), sList(symbol_cdaadr, cdaadr_prim), sList(symbol_cdaar, cdaar_prim), sList(symbol_cdadar, cdadar_prim), sList(symbol_cdaddr, cdaddr_prim), sList(symbol_cdadr, cdadr_prim), sList(symbol_cdar, cdar_prim), sList(symbol_cddaar, cddaar_prim), sList(symbol_cddadr, cddadr_prim), sList(symbol_cddar, cddar_prim), sList(symbol_cdddar, cdddar_prim), sList(symbol_cddddr, cddddr_prim), sList(symbol_cdddr, cdddr_prim), sList(symbol_cddr, cddr_prim), sList(symbol_char_q, char_q_prim), sList(symbol_char_is__q, char_is__q_prim), sList(symbol_char_whitespace_q, char_whitespace_q_prim), sList(symbol_char_alphabetic_q, char_alphabetic_q_prim), sList(symbol_char_numeric_q, char_numeric_q_prim), sList(symbol_char_to_integer, char_to_integer_prim), sList(symbol_cons, cons_prim), sList(symbol_current_time, current_time_prim), sList(symbol_cut, cut_prim), sList(symbol_dir, dir_prim), sList(symbol_display, display_prim), sList(symbol_current_environment, current_environment_prim), sList(symbol_eq_q, eq_q_prim), sList(symbol_equal_q, equal_q_prim), sList(symbol_error, error_prim), sList(symbol_eval, eval_prim), sList(symbol_eval_ast, eval_ast_prim), sList(symbol_exit, exit_prim), sList(symbol_for_each, for_each_prim), sList(symbol_format, format_prim), sList(symbol_get, get_prim), sList(symbol_get_stack_trace, get_stack_trace_prim), sList(symbol_import, import_prim), sList(symbol_integer_to_char, integer_to_char_prim), sList(symbol_length, length_prim), sList(symbol_sList, list_prim), sList(symbol_list_to_vector, list_to_vector_prim), sList(symbol_list_to_string, list_to_string_prim), sList(symbol_list_ref, list_ref_prim), sList(symbol_load, load_prim), sList(symbol_make_set, make_set_prim), sList(symbol_make_vector, make_vector_prim), sList(symbol_map, map_prim), sList(symbol_member, member_prim), sList(symbol_memq, memq_prim), sList(symbol_memv, memv_prim), sList(symbol_newline, newline_prim), sList(symbol_not, not_prim), sList(symbol_null_q, null_q_prim), sList(symbol_number_to_string, number_to_string_prim), sList(symbol_number_q, number_q_prim), sList(symbol_pair_q, pair_q_prim), sList(symbol_parse, parse_prim), sList(symbol_parse_string, parse_string_prim), sList(symbol_print, print_prim), sList(symbol_printf, printf_prim), sList(symbol_Range, range_prim), sList(symbol_read_string, read_string_prim), sList(symbol_require, require_prim), sList(symbol_reverse, reverse_prim), sList(symbol_set_car_b, set_car_b_prim), sList(symbol_set_cdr_b, set_cdr_b_prim), sList(symbol_snoc, snoc_prim), sList(symbol_rac, rac_prim), sList(symbol_rdc, rdc_prim), sList(symbol_sqrt, sqrt_prim), sList(symbol_odd_q, odd_q_prim), sList(symbol_even_q, even_q_prim), sList(symbol_quotient, quotient_prim), sList(symbol_remainder, remainder_prim), sList(symbol_make_string, string_prim), sList(symbol_string_length, string_length_prim), sList(symbol_string_ref, string_ref_prim), sList(symbol_string_q, string_q_prim), sList(symbol_string_to_number, string_to_number_prim), sList(symbol_string_is__q, string_is__q_prim), sList(symbol_substring, substring_prim), sList(symbol_symbol_q, symbol_q_prim), sList(symbol_unparse, unparse_prim), sList(symbol_unparse_procedure, unparse_procedure_prim), sList(symbol_using_native, using_prim), sList(symbol_use_stack_trace, use_stack_trace_prim), sList(symbol_vector, vector_prim), sList(symbol_vector_ref, vector_ref_prim), sList(symbol_vector_set_b, vector_set_b_prim), sList(symbol_void, void_prim), sList(symbol_zero_q, zero_q_prim), sList(symbol_current_directory, current_directory_prim), sList(symbol_cd, current_directory_prim), sList(symbol_round, round_prim), sList(symbol_char_to_string, char_to_string_prim), sList(symbol_string_to_list, string_to_list_prim), sList(symbol_string_to_symbol, string_to_symbol_prim), sList(symbol_symbol_to_string, symbol_to_string_prim), sList(symbol_vector_to_list, vector_to_list_prim), sList(symbol_eqv_q, eqv_q_prim), sList(symbol_vector_q, vector_q_prim), sList(symbol_atom_q, atom_q_prim), sList(symbol_iter_q, iter_q_prim), sList(symbol_list_q, list_q_prim), sList(symbol_procedure_q, procedure_q_prim), sList(symbol_stringLessThan_q, stringLessThan_q_prim), sList(symbol_float_, float_prim), sList(symbol_globals, globals_prim), sList(symbol_int_, int_prim), sList(symbol_apply_with_keywords, apply_with_keywords_prim), sList(symbol_assq, assq_prim), sList(symbol_dict, dict_prim), sList(symbol_property, property_prim), sList(symbol_rational, rational_prim), sList(symbol_reset_toplevel_env, reset_toplevel_env_prim), sList(symbol_sort, sort_prim), sList(symbol_string_append, string_append_prim), sList(symbol_string_split, string_split_prim), sList(symbol_symbol, symbol_prim), sList(symbol_typeof, typeof_prim), sList(symbol_use_lexical_address, use_lexical_address_prim), sList(symbol_use_tracing, use_tracing_prim));
        return make_initial_env_extended(map(car_proc, primitives), map(cadr_proc, primitives));
    }
    
    public static object make_external_proc(object external_function_object) {
        return make_proc("proc", 160, external_function_object);
    }
    
    public static bool pattern_q(object x) {
        return (true_q(null_q(x)) || true_q(number_q(x)) || true_q(boolean_q(x)) || true_q(symbol_q(x)) || true_q((true_q(pair_q(x)) && true_q(pattern_q(car(x))) && true_q(pattern_q(cdr(x))))));
    }
    
    public static bool pattern_variable_q(object x) {
        return (true_q(symbol_q(x)) && true_q(Equal("?", substring(symbol_to_string(x), 0, 1))));
    }
    
    public static bool constant_q(object x) {
        return (true_q((! true_q(pattern_variable_q(x)))) && true_q((! true_q(pair_q(x)))));
    }
    
    public static void occurs_q() {
        if (true_q(constant_q(pattern_reg))) {
            value_reg = false;
            pc = apply_cont;
        } else {
            if (true_q(pattern_variable_q(pattern_reg))) {
                value_reg = Equal(var_reg, pattern_reg);
                pc = apply_cont;
            } else {
                k_reg = make_cont("cont", 48, pattern_reg, var_reg, k_reg);
                pattern_reg = car(pattern_reg);
                pc = occurs_q;
            }
        }
    }
    
    public static void unify_patterns_hat() {
        if (true_q(pattern_variable_q(p1_reg))) {
            if (true_q(pattern_variable_q(p2_reg))) {
                value_reg = make_sub(symbol_unit, p1_reg, p2_reg, ap2_reg);
                pc = apply_cont;
            } else {
                k_reg = make_cont("cont", 49, ap2_reg, p1_reg, p2_reg, k_reg);
                pattern_reg = p2_reg;
                var_reg = p1_reg;
                pc = occurs_q;
            }
        } else {
            if (true_q(pattern_variable_q(p2_reg))) {
                temp_1 = p2_reg;
                temp_2 = p1_reg;
                temp_3 = ap2_reg;
                temp_4 = ap1_reg;
                p1_reg = temp_1;
                p2_reg = temp_2;
                ap1_reg = temp_3;
                ap2_reg = temp_4;
                pc = unify_patterns_hat;
            } else {
                if (true_q((true_q(constant_q(p1_reg)) && true_q(constant_q(p2_reg)) && true_q(Equal(p1_reg, p2_reg))))) {
                    value_reg = make_sub(symbol_empty);
                    pc = apply_cont;
                } else {
                    if (true_q((true_q(pair_q(p1_reg)) && true_q(pair_q(p2_reg))))) {
                        apair2_reg = ap2_reg;
                        apair1_reg = ap1_reg;
                        pair2_reg = p2_reg;
                        pair1_reg = p1_reg;
                        pc = unify_pairs_hat;
                    } else {
                        value_reg = false;
                        pc = apply_cont;
                    }
                }
            }
        }
    }
    
    public static void unify_pairs_hat() {
        k_reg = make_cont("cont", 51, apair1_reg, apair2_reg, pair1_reg, pair2_reg, k_reg);
        ap2_reg = car_hat(apair2_reg);
        ap1_reg = car_hat(apair1_reg);
        p2_reg = car(pair2_reg);
        p1_reg = car(pair1_reg);
        pc = unify_patterns_hat;
    }
    
    public static void instantiate_hat() {
        if (true_q(constant_q(pattern_reg))) {
            value2_reg = ap_reg;
            value1_reg = pattern_reg;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(pattern_variable_q(pattern_reg))) {
                avar_reg = ap_reg;
                var_reg = pattern_reg;
                pc = apply_sub_hat;
            } else {
                if (true_q(pair_q(pattern_reg))) {
                    k2_reg = make_cont2("cont2", 102, ap_reg, pattern_reg, s_reg, k2_reg);
                    ap_reg = car_hat(ap_reg);
                    pattern_reg = car(pattern_reg);
                    pc = instantiate_hat;
                } else {
                    throw new Exception("symbol_instantiate_hat: " + format("bad pattern: ~a", pattern_reg));;
                }
            }
        }
    }
    
    public static object make_sub(params object [] t_args) {
        object args = sList(t_args);
        return cons(symbol_substitution, args);
    }
    
    public static void apply_sub_hat() {
        object temp_1 = symbol_undefined;
        temp_1 = cdr(s_reg);
        if (true_q(Eq(car(temp_1), symbol_empty))) {
            value2_reg = avar_reg;
            value1_reg = var_reg;
            k_reg = k2_reg;
            pc = apply_cont2;
        } else {
            if (true_q(Eq(car(temp_1), symbol_unit))) {
                object new_var = symbol_undefined;
                object new_pattern = symbol_undefined;
                object new_apattern = symbol_undefined;
                new_apattern = list_ref(temp_1, 3);
                new_pattern = list_ref(temp_1, 2);
                new_var = list_ref(temp_1, 1);
                if (true_q(Equal(var_reg, new_var))) {
                    value2_reg = new_apattern;
                    value1_reg = new_pattern;
                    k_reg = k2_reg;
                    pc = apply_cont2;
                } else {
                    value2_reg = avar_reg;
                    value1_reg = var_reg;
                    k_reg = k2_reg;
                    pc = apply_cont2;
                }
            } else {
                if (true_q(Eq(car(temp_1), symbol_composite))) {
                    object s1 = symbol_undefined;
                    object s2 = symbol_undefined;
                    s2 = list_ref(temp_1, 2);
                    s1 = list_ref(temp_1, 1);
                    k2_reg = make_cont2("cont2", 103, s2, k2_reg);
                    s_reg = s1;
                    pc = apply_sub_hat;
                } else {
                    throw new Exception("symbol_apply_sub_hat: " + format("bad substitution: ~a", s_reg));;
                }
            }
        }
    }
    
    public static object chars_to_scan = symbol_undefined;
    public static object scan_line = symbol_undefined;
    public static object scan_char = symbol_undefined;
    public static object scan_position = symbol_undefined;
    public static object last_scan_line = symbol_undefined;
    public static object last_scan_char = symbol_undefined;
    public static object last_scan_position = symbol_undefined;
    public static object token_start_line = symbol_undefined;
    public static object token_start_char = symbol_undefined;
    public static object token_start_position = symbol_undefined;
    public static object atom_tag = box(symbol_atom);
    public static object pair_tag = box(symbol_pair);
    public static bool _starreader_generates_annotated_sexps_q_star = true;
    public static bool _staruse_lexical_address_star = true;
    public static Func<object,bool> quote_q_hat = tagged_list_hat(symbol_quote, Equal_proc, 2);
    public static Func<object,bool> quasiquote_q_hat = tagged_list_hat(symbol_quasiquote, Equal_proc, 2);
    public static Func<object,bool> unquote_q_hat = tagged_list_hat(symbol_unquote, GreaterThanEqual_proc, 2);
    public static Func<object,bool> unquote_splicing_q_hat = tagged_list_hat(symbol_unquote_splicing, GreaterThanEqual_proc, 2);
    public static Func<object,bool> if_then_q_hat = tagged_list_hat(symbol_if, Equal_proc, 3);
    public static Func<object,bool> if_else_q_hat = tagged_list_hat(symbol_if, Equal_proc, 4);
    public static Func<object,bool> assignment_q_hat = tagged_list_hat(symbol_set_b, Equal_proc, 3);
    public static Func<object,bool> func_q_hat = tagged_list_hat(symbol_func, Equal_proc, 2);
    public static Func<object,bool> callback0_q_hat = tagged_list_hat(symbol_callback0, Equal_proc, 2);
    public static Func<object,bool> callback1_q_hat = tagged_list_hat(symbol_callback1, Equal_proc, 2);
    public static Func<object,bool> callback2_q_hat = tagged_list_hat(symbol_callback2, Equal_proc, 2);
    public static Func<object,bool> define_q_hat = tagged_list_hat(symbol_define, GreaterThanEqual_proc, 3);
    public static Func<object,bool> define_b_q_hat = tagged_list_hat(symbol_define_b, GreaterThanEqual_proc, 3);
    public static Func<object,bool> define_syntax_q_hat = tagged_list_hat(symbol_define_syntax, GreaterThanEqual_proc, 3);
    public static Func<object,bool> begin_q_hat = tagged_list_hat(symbol_begin, GreaterThanEqual_proc, 2);
    public static Func<object,bool> lambda_q_hat = tagged_list_hat(symbol_lambda, GreaterThanEqual_proc, 3);
    public static Func<object,bool> trace_lambda_q_hat = tagged_list_hat(symbol_trace_lambda, GreaterThanEqual_proc, 4);
    public static Func<object,bool> raise_q_hat = tagged_list_hat(symbol_raise, Equal_proc, 2);
    public static Func<object,bool> choose_q_hat = tagged_list_hat(symbol_choose, GreaterThanEqual_proc, 1);
    public static Func<object,bool> try_q_hat = tagged_list_hat(symbol_try, GreaterThanEqual_proc, 2);
    public static Func<object,bool> catch_q_hat = tagged_list_hat(symbol_catch, GreaterThanEqual_proc, 3);
    public static Func<object,bool> finally_q_hat = tagged_list_hat(symbol_finally, GreaterThanEqual_proc, 2);
    public static object let_transformer_hat = make_macro("macro", 1);
    public static object letrec_transformer_hat = make_macro("macro", 2);
    public static object mit_define_transformer_hat = make_macro("macro", 3);
    public static object and_transformer_hat = make_macro("macro", 4);
    public static object or_transformer_hat = make_macro("macro", 5);
    public static object cond_transformer_hat = make_macro("macro", 6);
    public static object let_star_transformer_hat = make_macro("macro", 7);
    public static object case_transformer_hat = make_macro("macro", 8);
    public static object record_case_transformer_hat = make_macro("macro", 9);
    public static object define_datatype_transformer_hat = make_macro("macro", 10);
    public static object cases_transformer_hat = make_macro("macro", 11);
    public static object macro_env = symbol_undefined;
    public static object REP_k = make_cont2("cont2", 49);
    public static object REP_handler = make_handler2("handler2", 2);
    public static object REP_fail = make_fail("fail", 1);
    public static object _starlast_fail_star = REP_fail;
    public static object _startokens_left_star = symbol_undefined;
    public static object try_parse_handler = make_handler2("handler2", 3);
    public static bool _startracing_on_q_star = false;
    public static object _starstack_trace_star = sList(symbol_emptylist);
    public static bool _staruse_stack_trace_star = true;
    public static object void_prim = make_proc("proc", 5);
    public static object zero_q_prim = make_proc("proc", 6);
    public static object exit_prim = make_proc("proc", 7);
    public static object end_of_session = sList(symbol_exiting, symbol_the, symbol_interpreter);
    public static object eval_prim = make_proc("proc", 8);
    public static object eval_ast_prim = make_proc("proc", 9);
    public static object parse_prim = make_proc("proc", 10);
    public static object string_length_prim = make_proc("proc", 11);
    public static object string_ref_prim = make_proc("proc", 12);
    public static object unparse_prim = make_proc("proc", 13);
    public static object unparse_procedure_prim = make_proc("proc", 14);
    public static object parse_string_prim = make_proc("proc", 15);
    public static object read_string_prim = make_proc("proc", 16);
    public static object apply_prim = make_proc("proc", 17);
    public static object sqrt_prim = make_proc("proc", 18);
    public static object odd_q_prim = make_proc("proc", 19);
    public static object even_q_prim = make_proc("proc", 20);
    public static object quotient_prim = make_proc("proc", 21);
    public static object remainder_prim = make_proc("proc", 22);
    public static object print_prim = make_proc("proc", 23);
    public static object string_prim = make_proc("proc", 24);
    public static object substring_prim = make_proc("proc", 25);
    public static object number_to_string_prim = make_proc("proc", 26);
    public static object assv_prim = make_proc("proc", 27);
    public static object memv_prim = make_proc("proc", 28);
    public static object display_prim = make_proc("proc", 29);
    public static object newline_prim = make_proc("proc", 30);
    public static bool _starneed_newline_star = false;
    public static object load_prim = make_proc("proc", 31);
    public static object load_stack = symbol_emptylist;
    public static object length_prim = make_proc("proc", 32);
    public static object symbol_q_prim = make_proc("proc", 33);
    public static object number_q_prim = make_proc("proc", 34);
    public static object boolean_q_prim = make_proc("proc", 35);
    public static object string_q_prim = make_proc("proc", 36);
    public static object char_q_prim = make_proc("proc", 37);
    public static object char_is__q_prim = make_proc("proc", 38);
    public static object char_whitespace_q_prim = make_proc("proc", 39);
    public static object char_to_integer_prim = make_proc("proc", 40);
    public static object integer_to_char_prim = make_proc("proc", 41);
    public static object char_alphabetic_q_prim = make_proc("proc", 42);
    public static object char_numeric_q_prim = make_proc("proc", 43);
    public static object null_q_prim = make_proc("proc", 44);
    public static object pair_q_prim = make_proc("proc", 45);
    public static object cons_prim = make_proc("proc", 46);
    public static object car_prim = make_proc("proc", 47);
    public static object cdr_prim = make_proc("proc", 48);
    public static object cadr_prim = make_proc("proc", 49);
    public static object caddr_prim = make_proc("proc", 50);
    public static object caaaar_prim = make_proc("proc", 51);
    public static object caaadr_prim = make_proc("proc", 52);
    public static object caaar_prim = make_proc("proc", 53);
    public static object caadar_prim = make_proc("proc", 54);
    public static object caaddr_prim = make_proc("proc", 55);
    public static object caadr_prim = make_proc("proc", 56);
    public static object caar_prim = make_proc("proc", 57);
    public static object cadaar_prim = make_proc("proc", 58);
    public static object cadadr_prim = make_proc("proc", 59);
    public static object cadar_prim = make_proc("proc", 60);
    public static object caddar_prim = make_proc("proc", 61);
    public static object cadddr_prim = make_proc("proc", 62);
    public static object cdaaar_prim = make_proc("proc", 63);
    public static object cdaadr_prim = make_proc("proc", 64);
    public static object cdaar_prim = make_proc("proc", 65);
    public static object cdadar_prim = make_proc("proc", 66);
    public static object cdaddr_prim = make_proc("proc", 67);
    public static object cdadr_prim = make_proc("proc", 68);
    public static object cdar_prim = make_proc("proc", 69);
    public static object cddaar_prim = make_proc("proc", 70);
    public static object cddadr_prim = make_proc("proc", 71);
    public static object cddar_prim = make_proc("proc", 72);
    public static object cdddar_prim = make_proc("proc", 73);
    public static object cddddr_prim = make_proc("proc", 74);
    public static object cdddr_prim = make_proc("proc", 75);
    public static object cddr_prim = make_proc("proc", 76);
    public static object list_prim = make_proc("proc", 77);
    public static object make_set_prim = make_proc("proc", 78);
    public static object plus_prim = make_proc("proc", 79);
    public static object minus_prim = make_proc("proc", 80);
    public static object times_prim = make_proc("proc", 81);
    public static object divide_prim = make_proc("proc", 82);
    public static object modulo_prim = make_proc("proc", 83);
    public static object lt_prim = make_proc("proc", 84);
    public static object gt_prim = make_proc("proc", 85);
    public static object lt_or_eq_prim = make_proc("proc", 86);
    public static object gt_or_eq_prim = make_proc("proc", 87);
    public static object equal_sign_prim = make_proc("proc", 88);
    public static object abs_prim = make_proc("proc", 89);
    public static object equal_q_prim = make_proc("proc", 90);
    public static object eq_q_prim = make_proc("proc", 91);
    public static object memq_prim = make_proc("proc", 92);
    public static object member_prim = make_proc("proc", 93);
    public static object range_prim = make_proc("proc", 94);
    public static object snoc_prim = make_proc("proc", 95);
    public static object rac_prim = make_proc("proc", 96);
    public static object rdc_prim = make_proc("proc", 97);
    public static object set_car_b_prim = make_proc("proc", 98);
    public static object set_cdr_b_prim = make_proc("proc", 99);
    public static object import_prim = make_proc("proc", 100);
    public static object get_stack_trace_prim = make_proc("proc", 101);
    public static object get_prim = make_proc("proc", 102);
    public static object call_cc_prim = make_proc("proc", 104);
    public static object abort_prim = make_proc("proc", 105);
    public static object require_prim = make_proc("proc", 106);
    public static object cut_prim = make_proc("proc", 107);
    public static object reverse_prim = make_proc("proc", 108);
    public static object append_prim = make_proc("proc", 109);
    public static object string_to_number_prim = make_proc("proc", 110);
    public static object string_is__q_prim = make_proc("proc", 111);
    public static object list_to_vector_prim = make_proc("proc", 112);
    public static object list_to_string_prim = make_proc("proc", 113);
    public static object char_to_string_prim = make_proc("proc", 114);
    public static object string_to_list_prim = make_proc("proc", 115);
    public static object string_to_symbol_prim = make_proc("proc", 116);
    public static object symbol_to_string_prim = make_proc("proc", 117);
    public static object vector_to_list_prim = make_proc("proc", 118);
    public static object dir_prim = make_proc("proc", 119);
    public static object current_time_prim = make_proc("proc", 120);
    public static object map_prim = make_proc("proc", 121);
    public static object for_each_prim = make_proc("proc", 122);
    public static object format_prim = make_proc("proc", 123);
    public static object current_environment_prim = make_proc("proc", 124);
    public static object using_prim = make_proc("proc", 125);
    public static object not_prim = make_proc("proc", 126);
    public static object printf_prim = make_proc("proc", 127);
    public static object vector_prim = make_proc("proc", 128);
    public static object vector_set_b_prim = make_proc("proc", 129);
    public static object vector_ref_prim = make_proc("proc", 130);
    public static object make_vector_prim = make_proc("proc", 131);
    public static object error_prim = make_proc("proc", 132);
    public static object list_ref_prim = make_proc("proc", 133);
    public static object current_directory_prim = make_proc("proc", 134);
    public static object round_prim = make_proc("proc", 135);
    public static object use_stack_trace_prim = make_proc("proc", 136);
    public static object use_tracing_prim = make_proc("proc", 137);
    public static object eqv_q_prim = make_proc("proc", 138);
    public static object vector_q_prim = make_proc("proc", 139);
    public static object atom_q_prim = make_proc("proc", 140);
    public static object iter_q_prim = make_proc("proc", 141);
    public static object list_q_prim = make_proc("proc", 142);
    public static object procedure_q_prim = make_proc("proc", 143);
    public static object stringLessThan_q_prim = make_proc("proc", 144);
    public static object float_prim = make_proc("proc", 145);
    public static object globals_prim = make_proc("proc", 146);
    public static object int_prim = make_proc("proc", 147);
    public static object apply_with_keywords_prim = make_proc("proc", 148);
    public static object assq_prim = make_proc("proc", 149);
    public static object dict_prim = make_proc("proc", 150);
    public static object property_prim = make_proc("proc", 151);
    public static object rational_prim = make_proc("proc", 152);
    public static object reset_toplevel_env_prim = make_proc("proc", 153);
    public static object sort_prim = make_proc("proc", 154);
    public static object string_append_prim = make_proc("proc", 155);
    public static object string_split_prim = make_proc("proc", 156);
    public static object symbol_prim = make_proc("proc", 157);
    public static object typeof_prim = make_proc("proc", 158);
    public static object use_lexical_address_prim = make_proc("proc", 159);
    public static object toplevel_env = symbol_undefined;
    public static MethodInfo[] mi_cont4;
    public static MethodInfo[] mi_handler;
    public static MethodInfo[] mi_cont;
    public static MethodInfo[] mi_handler2;
    public static MethodInfo[] mi_cont3;
    public static MethodInfo[] mi_cont2;
    public static MethodInfo[] mi_fail;
    public static MethodInfo[] mi_macro;
    public static MethodInfo[] mi_proc;
    
    public static void initialize_method_info() {
        mi_cont4 = new MethodInfo[14];
        mi_handler = new MethodInfo[1];
        mi_cont = new MethodInfo[52];
        mi_handler2 = new MethodInfo[7];
        mi_cont3 = new MethodInfo[5];
        mi_cont2 = new MethodInfo[104];
        mi_fail = new MethodInfo[6];
        mi_macro = new MethodInfo[12];
        mi_proc = new MethodInfo[161];
        
        for (int i = 1; i < 14; i++) {
            mi_cont4[i] = typeof(PJScheme).GetMethod(String.Format("b_cont4_{0}_d", i));
            if (mi_cont4[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_cont4[{0}]", i));
            }
        }
        
        for (int i = 1; i < 1; i++) {
            mi_handler[i] = typeof(PJScheme).GetMethod(String.Format("b_handler_{0}_d", i));
            if (mi_handler[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_handler[{0}]", i));
            }
        }
        
        for (int i = 1; i < 52; i++) {
            mi_cont[i] = typeof(PJScheme).GetMethod(String.Format("b_cont_{0}_d", i));
            if (mi_cont[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_cont[{0}]", i));
            }
        }
        
        for (int i = 1; i < 7; i++) {
            mi_handler2[i] = typeof(PJScheme).GetMethod(String.Format("b_handler2_{0}_d", i));
            if (mi_handler2[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_handler2[{0}]", i));
            }
        }
        
        for (int i = 1; i < 5; i++) {
            mi_cont3[i] = typeof(PJScheme).GetMethod(String.Format("b_cont3_{0}_d", i));
            if (mi_cont3[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_cont3[{0}]", i));
            }
        }
        
        for (int i = 1; i < 104; i++) {
            mi_cont2[i] = typeof(PJScheme).GetMethod(String.Format("b_cont2_{0}_d", i));
            if (mi_cont2[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_cont2[{0}]", i));
            }
        }
        
        for (int i = 1; i < 6; i++) {
            mi_fail[i] = typeof(PJScheme).GetMethod(String.Format("b_fail_{0}_d", i));
            if (mi_fail[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_fail[{0}]", i));
            }
        }
        
        for (int i = 1; i < 12; i++) {
            mi_macro[i] = typeof(PJScheme).GetMethod(String.Format("b_macro_{0}_d", i));
            if (mi_macro[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_macro[{0}]", i));
            }
        }
        
        for (int i = 1; i < 161; i++) {
            mi_proc[i] = typeof(PJScheme).GetMethod(String.Format("b_proc_{0}_d", i));
            if (mi_proc[i] == null) {
                throw new Exception(String.Format("Undefined mi: mi_proc[{0}]", i));
            }
        }
        
    }
    
    public static object make_cont4 (string what, int id, params object[] args) {
        return sList(make_symbol("continuation4"), what, id, args);
    }
    
    public static object make_handler (string what, int id, params object[] args) {
        return sList(make_symbol("handler"), what, id, args);
    }
    
    public static object make_cont (string what, int id, params object[] args) {
        return sList(make_symbol("continuation"), what, id, args);
    }
    
    public static object make_handler2 (string what, int id, params object[] args) {
        return sList(make_symbol("handler2"), what, id, args);
    }
    
    public static object make_cont3 (string what, int id, params object[] args) {
        return sList(make_symbol("continuation3"), what, id, args);
    }
    
    public static object make_cont2 (string what, int id, params object[] args) {
        return sList(make_symbol("continuation2"), what, id, args);
    }
    
    public static object make_fail (string what, int id, params object[] args) {
        return sList(make_symbol("fail-continuation"), what, id, args);
    }
    
    public static object make_macro (string what, int id, params object[] args) {
        return sList(make_symbol("macro-transformer"), what, id, args);
    }
    
    public static object make_proc (string what, int id, params object[] args) {
        return sList(make_symbol("procedure"), what, id, args);
    }
    
}
