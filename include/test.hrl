%% @author Tim Watson <timwatson@munotify.com>
%% @copyright 2007 author.
%% common tests macros and functions

-define(TESTDOC(Doc), [{userdata,[{doc,Doc}]}]).
-define(NOT_IMPLEMENTED, {skip,"Not implemented."}).
-define(EXPORT_TESTS(Mod),
  [ {exports, Functions} | _ ] = Mod:module_info(),
    [ FName || {FName, _} <- lists:filter(
            fun ({module_info,_}) -> false ;
                ({all,_}) -> false ;
                ({init_per_suite,1}) -> false ;
                ({end_per_suite,1}) -> false ;
                ({_,1}) -> true ;
                ({_,_}) -> false
            end,
            Functions
        )
    ]).
