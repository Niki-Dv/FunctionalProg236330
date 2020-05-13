
-module(matrix_server).

%% API
-import(matrix, [getRow/2, getCol/2, getZeroMat/2, setElementMat/4]).
-import(superRes, [initialize/3]).
-export([start_server/0, serverListener/0, shutdown/0, get_version/0, mult/2, explanation/0, mulVec/4]).


start_server() ->
  io:format("Starting server~n"),
  Listener = fun() -> serverListener() end,
  Pid = self(),
  Server_pid = spawn(fun() -> superRes:initialize(Listener, matrix_server, Pid) end),
  receive
    {start_server, Server_pid} -> ok
  end.

serverListener() ->
  io:format("Listening...~n"),
  receive
    {Pid, MsgRef, {multiple, Mat1, Mat2}} ->
        spawn(fun() -> multProcess(Mat1, Mat2, MsgRef, Pid) end),
        serverListener();

    {Pid, MsgRef, get_version} ->
      Pid ! {MsgRef, ?MODULE:get_version()},
      serverListener();

    sw_upgrade ->
      ?MODULE:serverListener();

    shutdown -> ok;

    Other -> io:format("Some other arg ~p~n", [Other])

  end.

get_version() -> version_1.

explanation() -> { "Some answer"}.

shutdown() ->
  io:format("Shutdown Server is: ~p~n", [whereis(matrix_server)]),
  matrix_server ! shutdown.

multProcess(Mat1, Mat2, MsgRef, Pid) ->
  Res = mult(Mat1, Mat2),
  Pid ! {MsgRef, Res}.

mult(A, B) ->
  RowSize = size(A),
  ColumnSize = size(getRow(B, 1)),
  mult(A, B, 1, 1, RowSize, ColumnSize),
  ResMat =  getZeroMat(RowSize, ColumnSize),
  createResultMat(ResMat, RowSize * ColumnSize).

createResultMat(ResMat, 0) -> ResMat;
createResultMat(ResMat, NumberOfCalls) when NumberOfCalls > 0 ->
  receive
    {Res, {RowIndex, ColumnIndex}} ->
      createResultMat(setElementMat(RowIndex, ColumnIndex, ResMat, Res), NumberOfCalls - 1);
    _ -> io:format("Wrong message") % Remove me
  end.

mult(A, B, RowIndex, ColumnIndex, RowSize, ColumnSize) ->
  io:format("Index: {~p, ~p}~n", [RowIndex, ColumnIndex]),
  io:format("My Pid: ~p~n",[self()]),
  Pid = self(),
  Row = getRow(A, RowIndex),
  Column = getCol(B, ColumnIndex),
  spawn(fun() -> cellCalculation(Row, Column, RowIndex, ColumnIndex, Pid) end),
  io:format("Spawned an process for: ~p~p~n",[RowIndex, ColumnIndex]),
  if
    ColumnIndex < ColumnSize -> mult(A, B, RowIndex, ColumnIndex + 1, RowSize, ColumnSize);
    RowIndex < RowSize -> mult(A, B, RowIndex + 1, 1, RowSize, ColumnSize);
    true -> ok
  end.

cellCalculation(Row, Column, RowIndex, ColumnIndex, Parent_Pid) ->
  Res = mulVec(Row, Column, 0, 1),
  io:format("My parent is Pid: ~p~n",[Parent_Pid]),
  Parent_Pid ! {Res, {RowIndex, ColumnIndex}}.

mulVec(VecA, _, Sum, Index) when Index > size(VecA) -> Sum;
mulVec(VecA, VecB, Sum, Index) ->
  mulVec(VecA, VecB, Sum + element(Index, VecA)*element(Index, VecB), Index + 1).