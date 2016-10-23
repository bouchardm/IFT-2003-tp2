%%%%%%%%%%%%%%%%%%
% Basic function %
%%%%%%%%%%%%%%%%%%

% push(L1, L2, L3) :- L2 est rajouté à L1, le résultat est dans L3
push([],L,L).
push([X|L1],L2,[X|L3]) :- push(L1,L2,L3).

%posInList() :- Prend l'élément I de la list L et le met dans Result (Result est ' ' si la list est vide)
posInList(_,[],' ').
posInList(1,[Y|_],Result) :- Y=Result.
posInList(I,[_|W],Result) :- 
	J is I-1, 
	posInList(J,W,Result).

% listAtEndOfList(I, L, L2) :- La Liste L2 est après l'élément I de la liste L
listAtEndOfList(0,L,L2):- L=L2.
listAtEndOfList(I,[_|W],L2) :- 
	J is I-1, 
	listAtEndOfList(J,W,L2).

% listAtBeginOfList(I,L,L2) :- La liste L2 est au début de la liste L
listAtBeginOfList(I,L,L2) :- 
	listAtEndOfList(I,L,L3), 
	push(L2,L3,L).

% ithrep(I,ELT,L1,L2) :- La liste L2 est la liste optenu en replacent l'élément I de la list L1 avec ELT
ithrep(I,ELT,L1,L2) :- J is I-1,
	listAtBeginOfList(J,L1,L3),
	push(L3,[ELT],L4),
	listAtEndOfList(I,L1,L5),
	push(L4,L5,L2).

% listLength(L,N) :- La longeur de la liste L est N
listLength(L,N) :- lenacc(L,0,N).
lenacc([],A,A).
lenacc([_|T],A,N) :- 
	A1 is A+1, 
	lenacc(T,A1,N).

% getWinnerIn(A, B, C, D, Winner) :- Retourne s'il y a un gagant dans A et B et C et D
getWinnerIn(A, B, C, D, winp) :-
	A == winp; B == winp; C == winp; D == winp.
getWinnerIn(A, B, C, D, winc) :-
	A == winc; B == winc; C == winc; D == winc.
getWinnerIn(_, _, _, _, cont).

% getRow(I, Board, Result) :- Retourne le Board en fonction des lignes
getRow(I, [A, B, C, D, E, F, G], Result) :-
	posInList(I, A, RowIA),
	posInList(I, B, RowIB),
	posInList(I, C, RowIC),
	posInList(I, D, RowID),
	posInList(I, E, RowIE),
	posInList(I, F, RowIF),
	posInList(I, G, RowIG),
	Result = [RowIA, RowIB, RowIC, RowID, RowIE, RowIF, RowIG].

% valueCell(Player, Value, Player) :- Returne la valeur d'une cellule en fonction d'une joueur
valueCell(Player, 1, Player).
valueCell(_, 0, _).

% valueRow(NbPlayer, Value) :- Convertie le nombre de fois qu'un joueur est présent en valeur utile pour l'heuristique
valueRow(1, 1).
valueRow(2, 10).
valueRow(3, 125).
valueRow(4, 9999).
valueRow(_, 0).

% otherPlayer(Player, OtherPlayer) :- Donne l'autre joueur
otherPlayer(x, o).
otherPlayer(o, x).

% getAvailableMove(Board, AvailableMove) :- Retourne tout les mouvements possible pour un Board.
getAvailableMove(B, AvailableMove) :-
	findall(Pos, pos(Pos, B), AvailableMove).

% bestOf(A, B, BestValue) :- Retourne la meilleur valeur de A et B.
bestOf(A, B, A) :- A > B.
bestOf(A, B, B) :- A < B.
bestOf(A, B, B) :- A = B.

% worstOf(A, B, WorstValue) :- Retourne la pire valeur de A et B.
worstOf(A, B, B) :- A > B.
worstOf(A, B, A) :- A < B.
worstOf(A, B, B) :- A = B.

%%%%%%%%%%%%%
% Interface %
%%%%%%%%%%%%%

board([[],[],[],[],[],[],[]]).

% height(I,Board) :- Sert à forcer longeur maximale des colonne à 6
height(I, Board) :- 
	posInList(I, Board, Z),
    listLength(Z, N),
    N<6.

% pos(I, Board) :- Assure que la position I est valide dans Board
pos(1, Board) :-  height(1, Board).
pos(2, Board) :-  height(2, Board).
pos(3, Board) :-  height(3, Board).
pos(4, Board) :-  height(4, Board).
pos(5, Board) :-  height(5, Board).
pos(6, Board) :-  height(6, Board).
pos(7, Board) :-  height(7, Board).

% move(Pos, B, Player, NewB) :- Permet de placé un Player à la position I d'un board B. Returne le nouveau Board dans NewBoard
move(I, Board, Player, NewBoard) :-
	posInList(I, Board, ColPosOfBoard),
    push(ColPosOfBoard, [Player], NewCol),
    ithrep(I, NewCol, Board, NewBoard).

% start :- Démarre une nouvelle partie
start :-
	write('****************************'),nl,
    write('*                          *'),nl,
    write('*       Puissance 4!       *'),nl,
    write('*                          *'),nl,
    write('****************************'),nl,
    play_game,
    !,
    play_again.

play_again :- 
	nl, nl, write('Voulez-vous rejouer?'),nl
    , selectyn(X),
    !,
    X=111,
    start.

% selectyn(X) :- Demande à l'utilisateur oui ou non, retourne la réponse
selectyn(X) :-
	repeat,
    nl,write('Choissisez oui (o) ou non (n) puis appuyer sur <retour>.'),nl,
    get(X),
    (X=110; X=111). % ASCII pour n et o

% play_game :- Démarre une nouvelle partie
play_game :- 
	board(B),
    W1=x,
    W2=o,
    game_active(B,W1,W2,cont).

% game_active(B,W1,W2) :- Gère la partie: Affiche un message si quelqu'un gagne, sinon continue la partie
game_active(B, _, _, _) :- draw(B).
game_active(B, _, _, winp) :- 
	printBoard(B),
    nl, write('Bravo vous avez gagnez'), nl.
game_active(B, _, _, winc) :-
	printBoard(B),
    nl, write('Mouahahaha vous avez perdu.'), nl.

game_active(B, W1, W2, cont) :- 
	player_move(B,W1,NewB,Cont1),
    computer_move(NewB,W2,W1,NewerB,Cont1,Cont2),
    game_active(NewerB,W1,W2,Cont2).

%%%%%%%%%%%%%
% Affichage %
%%%%%%%%%%%%%

% printBoard(B) :- Affiche le jeu au complet
printBoard(B) :- printBoard(B,6).
printBoard(_,0) :- 
	write('|---|---|---|---|---|---|---|'),nl,
    write('  1   2   3   4   5   6   7  '), nl.
printBoard(B,Row) :- write('| '), printRow(B,Row,1), write(' |'),nl,
               NewR is Row-1,
               printBoard(B,NewR).

% printRow(B,Row,Col) :- Affiche une ligne du jeu
printRow(B,Row,7) :- 
	posInList(7,B,Lst), 
	posInList(Row,Lst,Symb), 
	write(Symb).
printRow(B,Row,Col) :- 
	posInList(Col,B,Lst), 
	posInList(Row,Lst,Symb),
	write(Symb), write(' | '),
    NewC is Col+1, printRow(B,Row,NewC).

%%%%%%%%%%%%%%%%%%%%%%%
% Gestion des joueurs %
%%%%%%%%%%%%%%%%%%%%%%%

% player_move(B,W1,NewB,Cont) :- Gestion du mouvement du joueur. 
% 								 Demande au joueur ou il veut jouer puis applique le coup. Si c'est un coup gagnant termine la partie.
player_move(B,W1,NewB,Cont) :-
	printBoard(B),
    nl, write('Select a move'),nl,
    repeat,
        getmove(Pos),
        (pos(Pos,B)-> true
            ; nl,
            write('Ce mouvement est impossible'),
            nl, fail),
        !,
        move(Pos,B,W1,NewB),
        win(Pos,NewB,p,W1,Cont).

% getmove(Pos) :- Demande un mouvement au joueur.
getmove(Pos) :- 
	repeat,
        pgetmove(X),
        X>=49,  % ASCII for 1
        X=<55,  % ASCII for 7
    !,
    Pos is X-48.

pgetmove(X) :- 
	nl,write('Veuillez choissir un mouvement entre 1 et 7 puis appuyer sur <retour>.'),nl,
    get(X).

% computer_move(B, W2, W1, NewBoard, PlayerWin, ComputerWin) :- Gestion du mouvement de l'intelligence artificiel.
%																Va calculer le meilleur mouvement, puis l'appliquer, 
% 																puis vérifier si l'ordinateur ou le joueur gagne gagne.
computer_move(B,_,_,B,winp,_).
computer_move(B,W2,W1,NewBoard,_,C2) :- 
	calc_move(B,W2,W1,Pos),
    move(Pos,B,W2,NewBoard),
    win(Pos,NewBoard,c,W2,C2).

% draw(B) :- Vérifie s'il y a égalité
draw(B) :- not(pos(_,B)),
           nl, nl, write('Partie nulle!.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%
% Gestion des gagnants %
%%%%%%%%%%%%%%%%%%%%%%%%

% win(Pos,B,PC,W,Cont) :- Va déterminer s'il y a un gagnant avec les lignes, les colonnes ou les diagonals.
win(_,B,_, _,Cont) :-
	winRow(B, RowWinner),
	winCol(B, ColWinner),
	winDiag(B, DiagWinner),
	isRowColDiagWinner(RowWinner, ColWinner, DiagWinner, Cont).
isRowColDiagWinner(A, B, C, winp) :-
	A == winp; B == winp; C == winp.
isRowColDiagWinner(A, B, C, winc) :-
	A == winc; B == winc; C == winc.
isRowColDiagWinner(_, _, _, cont).

% winCol(Colonne, Winner) :- Va déterminer s'il y a un joueur qui gagne avec les colonnes.
winCol([x,x,x,x], winp).
winCol([_, x,x,x,x], winp).
winCol([_, _, x,x,x,x], winp).
winCol([o,o,o,o], winc).
winCol([_, o,o,o,o], winc).
winCol([_, _, o,o,o,o], winc).
winCol([Col | AutreCol], IsWinner) :-
	winCol(Col, CurrentColWin),
	winCol(AutreCol, NextColWin),
	isWinner(CurrentColWin, NextColWin, IsWinner).
winCol(_, cont).
isWinner(A, _, A) :- A \== cont.
isWinner(_, A, A) :- A \== cont.
isWinner(_, _, cont).

% isWinnerRow(Board, Gagnant) :- Va déterminer s'il y a un gagnant avec les lignes.
winRow([A, B, C, D, E, F, G], Cont) :-
	isWinnerRow(A, B, C, D, First),
	isWinnerRow(B, C, D, E, Second),
	isWinnerRow(C, D, E, F, Third),
	isWinnerRow(D, E, F, G, Fourth),
	getWinnerIn(First, Second, Third, Fourth, Cont).
winRow(_, cont).

isWinnerRow([x | _], [x | _], [x | _], [x | _], winp).
isWinnerRow([o | _], [o | _], [o | _], [o | _], winc).
isWinnerRow([_ | EndA], [_ | EndB], [_ | EndC], [_ | EndD], Cont) :-
	isWinnerRow(EndA, EndB, EndC, EndD, Cont).
isWinnerRow(_, _, _, _, cont).

% winDiag(Board, Cont) :- Va déterminer s'il y a un gagnant avec les diagonals.
winDiag([A, B, C, D, E, F, G], Cont) :-
	isWinnerDiag(A, B, C, D, First),
	isWinnerDiag(B, C, D, E, Second),
	isWinnerDiag(C, D, E, F, Third),
	isWinnerDiag(D, E, F, G, Fourth),
	getWinnerIn(First, Second, Third, Fourth, Cont).
winDiag(_, cont).

isWinnerDiag([x | _], [_, x | _], [_, _, x | _], [_, _, _, x | _], winp).
isWinnerDiag([_, x | _], [_, _, x | _], [_, _, _, x | _], [_, _, _, _, x | _], winp).
isWinnerDiag([_, _, x | _], [_, _, _, x | _], [_, _, _, _, x | _], [_, _, _, _, _, x | _], winp).
isWinnerDiag([_, _, _, x | _], [_, _, x | _], [_, x | _], [x | _], winp).
isWinnerDiag([_, _, _, _, x | _], [_, _, _, x | _], [_, _, x | _], [_, x | _], winp).
isWinnerDiag([_, _, _, _, _, x | _], [_, _, _, _, x | _], [_, _, _, x | _], [_, _, x | _], winp).

isWinnerDiag([o | _], [_, o | _], [_, _, o | _], [_, _, _, o | _], winc).
isWinnerDiag([_, o | _], [_, _, o | _], [_, _, _, o | _], [_, _, _, _, o | _], winc).
isWinnerDiag([_, _, o | _], [_, _, _, o | _], [_, _, _, _, o | _], [_, _, _, _, _, o | _], winc).
isWinnerDiag([_, _, _, o | _], [_, _, o | _], [_, o | _], [o | _], winc).
isWinnerDiag([_, _, _, _, o | _], [_, _, _, o | _], [_, _, o | _], [_, o | _], winc).
isWinnerDiag([_, _, _, _, _, o | _], [_, _, _, _, o | _], [_, _, _, o | _], [_, _, o | _], winc).
isWinnerDiag(_, _, _, _, cont).

%%%%%%%%%%%%%%%%%%%
% Calcul de score %
%%%%%%%%%%%%%%%%%%%

% scoreAllCol(Board, Score, Player) :- Calcul le score de toute les colonnes pour un joueur
scoreAllCol([Col | Reste], Score, Player) :-
	scoreCol(Col, ScoreCol, Player),
	scoreAllCol(Reste, ResteScore, Player),
	Score is ResteScore + ScoreCol
	.
scoreAllCol(_, 0, _).

% scoreCol(Col, Score, Player) :- Calcul le score à partir du bas
scoreCol([Player, Player, Player, Player], 9999, Player).
scoreCol([Player, Player, Player], 125, Player).
scoreCol([Player, Player], 10, Player).
scoreCol([Player], 1, Player).

% scoreCol(Col, Score, Player) :- Calcul le score à partir d'une piece plus haut
scoreCol([_, Player, Player, Player, Player], 9999, Player).
scoreCol([_, Player, Player, Player], 125, Player).
scoreCol([_, Player, Player], 10, Player).
scoreCol([_, Player], 1, Player).

% scoreCol(Col, Score, Player) :- Calcul le score à partir de deux piece plus haut, plus de piece = impossible à gagné dans cette colonne
scoreCol([_, _, Player, Player, Player, Player], 9999, Player).
scoreCol([_, _, Player, Player, Player], 125, Player).
scoreCol([_, _, Player, Player], 10, Player).
scoreCol([_, _, Player], 1, Player).
scoreCol(_, 0, _).

% scoreAllRow(Board, Score, Player) :- Calcul le score de toute les lignes en fonction d'un joueur
scoreAllRow(B, Score, Player) :-
	% Va chercher le board en fonction des lignes
	getRow(1, B, Row1),
	getRow(2, B, Row2),
	getRow(3, B, Row3),
	getRow(4, B, Row4),
	getRow(5, B, Row5),
	getRow(6, B, Row6),
	% Va chercher le score de chaque ligne
	scoreRow(Row1, Score1, Player),
	scoreRow(Row2, Score2, Player),
	scoreRow(Row3, Score3, Player),
	scoreRow(Row4, Score4, Player),
	scoreRow(Row5, Score5, Player),
	scoreRow(Row6, Score6, Player),
	Score is Score1 + Score2 + Score3 + Score4 + Score5 + Score6.

% scoreRow(Row, Score, Player) :- Donne le score pour une ligne en fonction d'un joueur. 
%								  Si dans un espace de 4 il y a deux joueurs cela va retourné un score de 0
scoreRow([A, B, C, D, E, F, G], Score, Player) :-
	scoreSubRow([A, B, C, D], First, Player),
	scoreSubRow([B, C, D, E], Second, Player),
	scoreSubRow([C, D, E, F], Third, Player),
	scoreSubRow([D, E, F, G], Fourth, Player),
	Score is First + Second + Third + Fourth.

scoreSubRow([A, B, C, D], Score, Player) :-
	otherPlayer(Player, OtherPlayer),
	A \== OtherPlayer, B \== OtherPlayer, C \== OtherPlayer, D \== OtherPlayer,
	valueCell(A, ValueA, Player),
	valueCell(B, ValueB, Player),
	valueCell(C, ValueC, Player),
	valueCell(D, ValueD, Player),
	Total is ValueA + ValueB + ValueC + ValueD,
	valueRow(Total, Score).
scoreSubRow(_, 0, _).

% heuristic(Board, Score, Player) :- Calcul l'heuristique d'un Board pour un joueur. Va additionner le score pour les lignes et les colonnes.
heuristic(B, Score, Player) :-
	scoreAllCol(B, ScoreCols, Player),
	scoreAllRow(B, ScoreRows, Player),
	Score is ScoreCols + ScoreRows.

% bestMove(AvailableMove, Board, Player, Score, BestMove) :- Retourne le meilleur mouvement et son score.
% 															 Ceci en fonction de tout les mouvements possible sur un Board pour un joueur.
bestMove([Move], Board, Player, Score, Move) :-
	move(Move,Board,Player,NewBoard),
	heuristic(NewBoard, Score, Player).
bestMove([Move | Reste], Board, Player, Score, BestMove) :-
	bestMove(Reste, Board, Player, ScoreReste, MoveReste),
	move(Move,Board,Player,NewBoard),
	heuristic(NewBoard, ScoreMove, Player),
	bestOf(ScoreMove, ScoreReste, Score),
	assert(lastMove(ScoreReste, MoveReste)),
	assert(lastMove(ScoreMove, Move)),
	lastMove(Score, BestMove).

% worstMove(AvailableMove, Board, Player, Score, BestMove) :- Retourne le pire mouvement et son score.
% 															  Ceci en fonction de tout les mouvements possible sur un Board pour un joueur.
worstMove([Move], Board, Player, Score, Move) :-
	move(Move,Board,Player,NewBoard),
	heuristic(NewBoard, Score, Player).
worstMove([Move | Reste], Board, Player, Score, WorstMove) :-
	worstMove(Reste, Board, Player, ScoreReste, MoveReste),
	move(Move,Board,Player,NewBoard),
	heuristic(NewBoard, ScoreMove, Player),
	worstOf(ScoreMove, ScoreReste, Score),
	assert(lastMove(ScoreReste, MoveReste)),
	assert(lastMove(ScoreMove, Move)),
	lastMove(Score, WorstMove).

% minimax(B, Depth, Player, Score) :- Fonction minimax pour un Board et un player. Va retourner un Score.
minimax(B, Depth, o, Score) :-
	Depth > 0,
	getAvailableMove(B, AvailableMove),
	bestMove(AvailableMove, B, o, BestMoveScore, BestMove),
	NewDepth is Depth - 1,
	move(BestMove, B, o, NewBoard),
	minimax(NewBoard, NewDepth, x, MinimaxScore),
	bestOf(BestMoveScore, MinimaxScore, Score).
minimax(B, Depth, x, Score) :-
	Depth > 0,
	getAvailableMove(B, AvailableMove),
	worstMove(AvailableMove, B, x, BestMoveScore, BestMove),
	NewDepth is Depth - 1,
	move(BestMove, B, x, NewBoard),
	minimax(NewBoard, NewDepth, o, MinimaxScore),
	worstOf(BestMoveScore, MinimaxScore, Score).
minimax(B, _, Player, Score) :-
	otherPlayer(Player, OtherPlayer),
	heuristic(B, Score, OtherPlayer).

% bestMinimax(AvailableMove, Board, Player, Score, Move) :- Retourne le meilleur minimax pour tout les mouvements possibles sur un Board pour un joueur.
bestMinimax([Move], Board, Player, Score, Move) :-
	move(Move,Board,Player,NewBoard),
	minimax(NewBoard, 5, Player, Score).
bestMinimax([Move | Reste], Board, Player, Score, BestMove) :-
	retractall(lastMove(_, _)),
	bestMinimax(Reste, Board, Player, ScoreReste, MoveReste),
	move(Move,Board,Player,NewBoard),
	minimax(NewBoard, 5, Player, ScoreMove),
	bestOf(ScoreMove, ScoreReste, Score),
	assert(lastMove(ScoreReste, MoveReste)),
	assert(lastMove(ScoreMove, Move)),
	lastMove(Score, BestMove).

% calc_move(Board,_,_,Position) :- Va retourner la meilleur position possible en fonction du minimax
calc_move(B,_,_,Pos) :-
	getAvailableMove(B, AvailableMove),
	bestMinimax(AvailableMove, B, o, _, Pos).

% Interface inspiré de http://www.cs.sjsu.edu/faculty/pollett/440.1.97f/startv2.P