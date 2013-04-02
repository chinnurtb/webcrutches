%%%------------------------------------------------------------------------
%%% @doc WebCrutches ErlyDTL templating support
%%% Created: 2013-04-02 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(wcr_template).

-export([ compile/1
        , render/2
        ]).

%%%------------------------------------------------------------------------
-spec compile(string()) -> atom().
compile(TplName) ->
  TplModule = list_to_atom(TplName ++ "_dtl"),
  %% recompile-debug
  Priv = code:priv_dir(ssweb),
  TplDir = filename:join([Priv, "tpl"]),
  erlydtl:compile(
    filename:join([TplDir, TplName ++ ".dtl"]), TplModule,
    [ verbose
    , {out_dir, filename:join([TplDir, "ebin"])}
    , {doc_root, TplDir}
    , {custom_tags_dir, filename:join([TplDir, "custom_tags"])}
    ]),
  TplModule.

%%%------------------------------------------------------------------------
-spec render(TemplateName :: string(),
             Params :: orddict:orddict()) -> iolist().

render(TplName, TplOptions) ->
  TplModule = compile(TplName),
  case erlang:function_exported(TplModule, render, 1) of
    true ->
      {ok, Content} = TplModule:render(TplOptions),
      Content;
    false ->
      erlang:error({error, not_exported, {TplModule, render, 1}})
  end.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:

