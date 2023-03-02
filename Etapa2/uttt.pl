:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(('', ['', '', '', '', '', '', '', '', ''],
               [['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
                ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', '']])) :- !.

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards((_, _, Boards), Boards).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT,
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(State, UPos, Board) :-
    getBoards(State, Boards),
    positions(Poss),
    nth0(Idx, Poss, UPos),
    nth0(Idx, Boards, Board).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard((_, UboardState, _), UboardState).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(State, UPos, Pos, Cell) :-
    getBoard(State, UPos, Board),
    getPos(Board, Pos, Cell).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos,
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :-
    positions(Poss),
    nth0(Idx, Poss, Pos),
    nth0(Idx ,Board, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(State, NextPlayer) :-
    getBoards(State, Boards),
    computeNextPlayer(Boards, NextPlayer).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile
% din UBoard ale tablelor disponibile pentru următoarea mutare.
myFilterAndMap([], [], []).
myFilterAndMap([''|T1], [H|T2], [H|T3]) :-
    myFilterAndMap(T1, T2, T3).
myFilterAndMap([H1|T1], [_|T2], L) :-
    H1 \= '',
    myFilterAndMap(T1, T2, L).

generatePositionsList('', PreviousPos, _, [PreviousPos]).
generatePositionsList(Result, _, UboardState, NextBoardsPoss):-
    Result \= '',
    positions(Poss),
    myFilterAndMap(UboardState, Poss, NextBoardsPoss).

getNextAvailableBoards(State, NextBoardsPoss) :-
    initialState(State),
    positions(NextBoardsPoss).
getNextAvailableBoards(State, NextBoardsPoss) :-
    getUBoard(State, UboardState),
    buildState(_, PreviousPos, State),
    getBoard(State, PreviousPos, Board),
    getBoardResult(Board, Result),
    generatePositionsList(Result, PreviousPos, UboardState, NextBoardsPoss).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :-
    player_wins(Result, Board).
getBoardResult(Board, '') :-
    \+ player_wins(_, Board),
    myMem('', Board).
getBoardResult(Board, r) :-
    \+ player_wins(_, Board),
    \+ myMem('', Board).

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
getTTTOccurrences([], _, 0).
getTTTOccurrences([Player|T], Player, Occ) :-
    getTTTOccurrences(T, Player, Occ1),
    Occ is Occ1 + 1, !.
getTTTOccurrences([_|T], Player, Occ) :-
    getTTTOccurrences(T, Player, Occ), !.

getUTTTOccurrences([], _, 0).
getUTTTOccurrences([H|R], Player, Occ) :-
    getTTTOccurrences(H, Player, Occ1),
    getUTTTOccurrences(R, Player, Occ2),
    Occ is Occ1 + Occ2.

computeNextPlayer(Boards, x) :-
    getUTTTOccurrences(Boards, x, OccX),
    getUTTTOccurrences(Boards, 0, Occ0),
    OccX =< Occ0.
computeNextPlayer(Boards, 0) :-
    getUTTTOccurrences(Boards, x, OccX),
    getUTTTOccurrences(Boards, 0, Occ0),
    OccX > Occ0.

buildUBoard([],[]).
buildUBoard([Board|R], [Result|T]) :-
    getBoardResult(Board, Result),
    buildUBoard(R, T).

buildState(Boards, PreviousPos, (PreviousPos, UboardState, Boards)) :-
    buildUBoard(Boards, UboardState).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(State, (UPos, Pos)) :-
    getUBoard(State, UBoard),
    getBoardResult(UBoard,''),
    getNextAvailableBoards(State, NextPoss),
    myMem(UPos, NextPoss),
    getPos(State, UPos, Pos, '').
validMove(State, Move) :-
    getUBoard(State, UBoard),
    getBoardResult(UBoard,''),
    getNextAvailableBoards(State, [NextPos]),
    getPos(State, NextPos, Move, '').

myMem(X, [X|_]) :- !.
myMem(X, [_|T]) :- myMem(X, T).

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
replaceAt([_|T], 0, NewElem, [NewElem|T]).
replaceAt([H|T], Idx, NewElem, [H|R]) :-
    Idx > -1,
    Idx1 is Idx - 1,
    replaceAt(T, Idx1, NewElem, R).
replaceAt(L,_,_,L).

makeMove(State, (UPos, Pos), NewState) :-
    validMove(State, (UPos, Pos)),
    getNextPlayer(State, NextPlayer),
    getBoard(State, UPos, Board),
    positions(Poss),
    nth0(Idx, Poss, Pos),
    replaceAt(Board, Idx, NextPlayer, NewBoard),
    nth0(UIdx, Poss, UPos),
    getBoards(State, Boards),
    replaceAt(Boards, UIdx, NewBoard, NewBoards),
    buildState(NewBoards, Pos, NewState).
makeMove(State, Move, NewState) :-
    validMove(State, Move),
    getNextPlayer(State, NextPlayer),
    getNextAvailableBoards(State, [NextPos]),
    getBoard(State, NextPos, Board),
    positions(Poss),
    nth0(Idx, Poss, Move),
    replaceAt(Board, Idx, NextPlayer, NewBoard),
    nth0(UIdx, Poss, NextPos),
    getBoards(State, Boards),
    replaceAt(Boards, UIdx, NewBoard, NewBoards),
    buildState(NewBoards, Move, NewState).

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
getFirstAvailablePosition(State, [UPos], NextMove) :-
    getBoard(State, UPos, Board),
    positions(Poss),
    myFilterAndMap(Board, Poss, [NextMove|_]).
getFirstAvailablePosition(State, [UPos|_], (UPos, Pos)) :-
    getBoard(State, UPos, Board),
    positions(Poss),
    myFilterAndMap(Board, Poss, [Pos|_]).

dummy_first(State, NextMove) :-
    getNextAvailableBoards(State, Poss),
    getFirstAvailablePosition(State, Poss, NextMove).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă
% (ultima din lista de poziții disponibile).
reverse([], L, L).
reverse([H|T], L, Acc) :-
    reverse(T, L, [H|Acc]).

getLastAvailablePosition(State, [Pos], NextMove) :-
    getBoard(State, Pos, Board),
    positions(Poss),
    myFilterAndMap(Board, Poss, AvailablePoss),
    reverse(AvailablePoss, [NextMove|_], []).
getLastAvailablePosition(State, [UPos|_], (UPos, Pos)) :-
    getBoard(State, UPos, Board),
    positions(Poss),
    myFilterAndMap(Board, Poss, AvailablePoss),
    reverse(AvailablePoss, [Pos|_], []).

dummy_last(State, NextMove) :-
    getNextAvailableBoards(State, Poss),
    reverse(Poss,RevPoss,[]),
    getLastAvailablePosition(State, RevPoss, NextMove).


% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.
corners([nw, ne, sw, se]).
movePriority(Player, Board, Move, 0) :-
    positions(Poss),
    nth0(Idx, Poss, Move),
    replaceAt(Board, Idx, Player, NewBoard),
    getBoardResult(NewBoard, Player).
movePriority(Player, Board, Move, 1) :-
    positions(Poss),
    nth0(Idx, Poss, Move),
    nextPlayer(Player, NextPlayer),
    replaceAt(Board, Idx, NextPlayer, NewBoard),
    getBoardResult(NewBoard, NextPlayer).
movePriority(_, ['', '', '', '', '', '', '', '', ''], Move, 2) :-
    corners(Corners),
    member(Move, Corners).
movePriority(Player, Board, Move, 3) :-
    \+ member(Player, Board),
    nextPlayer(Player, Opponent),
    getPos(Board, c, Opponent),
    corners(Corners),
    member(Move, Corners).
movePriority(Player, Board, c, 3) :-
    \+ member(Player, Board),
    getPos(Board, c, '').
movePriority(Player, Board, Move, 4) :-
    positions(Poss),
    nth0(Idx, Poss, Move),
    replaceAt(Board, Idx, Player, NewBoard),
    findall(M, (member(M, Poss), getPos(NewBoard, M, ''), movePriority(Player, NewBoard, M, 0)), Moves),
    Moves \= [].
movePriority(_, _, Move, 5) :-
    corners(Corners),
    member(Move, Corners).
movePriority(_, _, _, 6).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
buildPairList(_, _, [], []).
buildPairList(Player, Board, [Move|R], [(Priority, Move)|T]) :-
    getPos(Board, Move, ''),
    movePriority(Player, Board, Move, Priority),
    buildPairList(Player, Board, R, T).
buildPairList(Player, Board, [Move|R], List) :-
    \+ getPos(Board, Move, ''),
    buildPairList(Player, Board, R, List).

bestIndividualMoves(Player, Board, Moves) :-
    positions(Poss),
    buildPairList(Player, Board, Poss, List),
    sortMoves(List, Moves).

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.
bestIndividualMove(Player, Board, Move) :-
    bestIndividualMoves(Player, Board, [Move|_]).

narrowGreedy(State, Move) :-
    getNextAvailableBoards(State, [UPos]),
    getNextPlayer(State, Player),
    getBoard(State, UPos, Board),
    bestIndividualMove(Player, Board, Move), !.
narrowGreedy(State, (UPos, Pos)) :-
    getUBoard(State, UBoard),
    getNextPlayer(State, Player),
    bestIndividualMove(Player, UBoard, UPos),
    getBoard(State, UPos, Board),
    bestIndividualMove(Player, Board, Pos).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves(_, _) :- false.

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(_, _) :- false.
