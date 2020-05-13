-module(main).

%% API
-export([runTest/0]).
-import(matrix_server, [start_server/0, shutdown/0, mulVec/4, mult/2]).

runTest() ->
  checkVecMul(),
  checkMatMul(),
  checkServer().
  % nikiCheckServer().

checkServer() ->
  matrix_server:start_server(),
  A = {{1,2}, {3,4}, {6,7}},
  B = {{1,2,3, 4}, {3,4,5, 6}},

  MsgRef = make_ref(),
  Message = {self(), MsgRef, {multiple, A, B}},
  Expected = {MsgRef, {{6,10,13,16},{15,22,29,36},{27,40,53,66}}},
  send_and_assert_server_result(Message, Expected, 1),

  shutdown().

checkMatMul() ->
  A = {{1,2}, {3,4}, {6,7}},
  B = {{1,2,3, 4}, {3,4,5, 6}},
  Res = mult(A, B),
  Res = {{7,10,13,16},{15,22,29,36},{27,40,53,66}}.

checkVecMul() ->
  VecA = {1, 2, 3, 4},
  VecB = {1, 2, 3, 4},
  Res = mulVec(VecA, VecB, 0, 1),
  Res = 30.
  
nikiCheckServer() ->
  start_server(),

  % first mult params
  A = {{1,2}, {3,4}, {6,7}},
  B = {{1,2,3, 4}, {3,4,5, 6}},
  TrueRes1 = {{7,10,13,16},{15,22,29,36},{27,40,53,66}},
  FirstMult = make_ref(),
  matrix_server ! {self(), FirstMult, {multiple, A, B}},

  % second mult params
  C = {{1,2}},
  D = {{1,2,3, 4}, {3,4,5, 6}},
  TrueRes2 = {{7, 10, 13, 16}},
  SecondMult = make_ref(),
  matrix_server ! {self(), SecondMult, {multiple, C, D}},

  receive
    {SecondMult, Res1} ->
      Res1 = TrueRes2,
     io:format("First multiplcation - Done~n")
  end,

  receive
    {FirstMult, Res2} ->
      Res2 = TrueRes1,
      io:format("Second multiplcation - Done~n")
  end,

  % check version
  VersionRef = make_ref(),
  matrix_server ! {self(), VersionRef, get_version},
  receive
    {VersionRef, VersionNum} -> VersionNum == version1
  end,

  shutdown(),
  io:format("Finished Test server successfuly~n").

send_and_assert_server_result(Message, Expected, Test_Name) ->
  matrix_server ! Message,
  receive
    Expected -> io:format("Test ~p succesful!~n", [Test_Name]);
    Actual -> io:format("Test ~p Failed. Expected ~p, Actual ~p~n", [Test_Name, Expected, Actual])
  end.