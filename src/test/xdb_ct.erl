-module(xdb_ct).

-export([
  assert_error/2,
  pipe/2
]).

-type config() :: proplists:proplist().

-export_type([config/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec assert_error(fun(), term()) -> any().
assert_error(Fun, Error) ->
  try Fun()
  catch
    _:{Error, _} -> ok;
    _:Error      -> ok
  end.

-spec pipe(Initial, Pipeline) -> Return when
  Initial  :: any(),
  Module   :: module(),
  Fun      :: atom(),
  Args     :: [any()],
  FunSpec  :: {fun(), Args} | {Module, Fun, Args},
  Pipeline :: [FunSpec],
  Return   :: any().
pipe(Initial, Pipeline) ->
  lists:foldl(fun
    ({Module, Fun, Args}, Acc) ->
      apply(Module, Fun, [Acc | Args]);
    ({Fun, Args}, Acc) ->
      apply(Fun, [Acc | Args])
  end, Initial, Pipeline).
