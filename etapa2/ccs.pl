:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').


% tile/2
% tile(Index, Tile)
%
% Fiecare soluÈ›ie a predicatului tile este o corespondenÈ›Äƒ Ã®ntre index
% (numÄƒrul piesei Ã®n lista din enunÈ›) È™i reprezentarea internÄƒ a piesei
% respective.
%
% PuteÈ›i alege orice reprezentare doriÈ›i, Ã®n aÈ™a fel Ã®ncÃ¢t sÄƒ puteÈ›i
% reprezenta toate piesele din enunÈ›.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiÈ™te.
% Pot exista cel mult 1 drum È™i cel mult 2 castele pe aceeaÈ™i piesÄƒ.
%
% Reprezentarea trebuie sÄƒ poatÄƒ fi rotitÄƒ (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie sÄƒ definiÈ›i reprezentÄƒri pentru fiecare dintre cele 16 piese
% din enunÈ›ul temei.
%
% Exemplu: apelul tile(1, T1). trebuie sÄƒ lege T1 la reprezentarea pe
% care o faceÈ›i pentru piesa 1. AceastÄƒ reprezentare poate fi transmisÄƒ
% celorlalte predicate din temÄƒ, pentru a Ã®ntreba, de exemplu, ce se
% aflÄƒ pe muchia de nord a piesei 1, sau dacÄƒ piesa 1 se potriveÈ™te cu o
% altÄƒ piesÄƒ.

% Am ales sa reprezint o piesa sub forma ([], []), unde in prima lista
% pun in ordine
% [numarCetati, numarDrumuri, numarPajisti], iar in a doua lista pun in
% ordine ce se afla pe fiecare punct cardinal al piesei
% respective(ordinea de amplasare este n->e->s->w).
tile(_, _) :- false.
tile(1, [c, c, p, c]).
tile(2, [c, c, d, c]).
tile(3, [c, c, p, p]).
tile(4, [c, c, p, p]).
tile(5, [c, p, c, p]).
tile(6, [c, p, c, p]).
tile(7, [c, p, p, p]).
tile(8, [c, c, d, d]).
tile(9, [c, p, d, d]).
tile(10, [c, d, d, p]).
tile(11, [c, d, p, d]).
tile(12, [c, d, d, d]).
tile(13, [p, p, d, d]).
tile(14, [p, d, p, d]).
tile(15, [p, d, d, d]).
tile(16, [d, d, d, d]).


% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevÄƒrat dacÄƒ pe piesa Tile are pe muchia de pe
% direcÈ›ia Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentÃ¢nd cetate,
% drum, sau pajiÈ™te.
%
% De exemplu, piesa 4 are cetate Ã®n nord È™i Ã®n este, È™i pajiÈ™te Ã®n sud
% È™i vest. Iar piesa 10 are cetate Ã®n nord, drum Ã®n este È™i sud, È™i
% pajiÈ™te Ã®n vest.
%
% DacÄƒ What nu este legat, trebuie legat la entitatea care se aflÄƒ pe
% muchia din direcÈ›ia Dir.

% Pentru implementarea acestui predicat am scris toate cazurile posibile
% pentru componenta ce se afla la o anumita directie in lista cu
% componente. Mai pe scurt, piesa afla la directia "D" din "Tile" este
% "X", daca "D" este pe prima pozitie in lista primita de predicatul
% "directions".
at(_, _, _) :- false.
%at([c1 | _], D, c):- directions([D | _]).

at([X | _], D, X):- directions([D | _]).
at([_, X | _], D, X):- directions([_, D | _]).
at([_, _, X | _], D, X):- directions([_, _, D | _]).
at([_, _, _, X | _], D, X):- directions([_ ,_, _, D]).


% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevÄƒrat dacÄƒ piesa Tile are entitatea what pe toate
% direcÈ›iile din lista Directions, cu aceleaÈ™i valori pentru entitÄƒÈ›i È™i
% direcÈ›ii ca È™i la predicatul at/3.
%
% De exemplu, predicatul este adevÄƒrat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], È™i pentru entitatea c. Este adevÄƒrat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], È™i pentru
% entitatea d.
%
% AtenÈ›ie! Pentru ca predicatul sÄƒ fie adevÄƒrat, nu este nevoie ca Ã®n
% Directions sÄƒ fie *toate* direcÈ›iile pe care se aflÄƒ entitatea
% respectivÄƒ, pot fi doar o submulÈ›ime a acestora.
% De exemplu, la piesa 14, predicatul este adevÄƒrat pentru entitatea d
% È™i pentru oricare dintre listele [w], [e] sau [e,w].

% Pentru implementarea acestui predicat am considerat cazul de baza in
% care am o lista vida de directii(si nu conteaza ce valoare e legata
% la "What") si cazul general in care predicatul este adevarat pentru
% lista initiala, daca este adevarat si pentru restul listei si in
% directia "Direction" a piesei se afla chiar componenta de pe prima
% pozitie din lista mereu formata prin recursivitate.
atL(_, _, _) :- false.
atL(_, [], _).
atL(Tile, [H | T], X):- atL(Tile, T, X), at(Tile, H, X).

% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul Ã®ntoarce adevÄƒrat dacÄƒ pe piesÄƒ existÄƒ douÄƒ cetÄƒÈ›i diferite
% (ca Ã®n piesele 4 È™i 5).

% Verific daca piesa primita ca parametru e fie piesa ce are indexul 5,
% fie cea cu indexul 4.
hasTwoCitadels(_) :- false.
hasTwoCitadels(Tile):- tile(I, Tile), I == 4.
hasTwoCitadels(Tile):- tile(I, Tile), I == 5.


% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevÄƒrat dacÄƒ RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotitÄƒ de Rotation ori, Ã®n sens trigonometric.
%
% De exemplu, dacÄƒ T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiÈ™te la nord È™i
% vest, È™i cetate la est È™i sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotaÈ›ii este
% identicÄƒ cu piesa iniÈ›ialÄƒ.
% De exemplu, la piesele 5, 6 È™i 14, rotirea cu Rotation=2 va duce la o
% reprezentare identicÄƒ cu piesa iniÈ›ialÄƒ, È™i la fel rezultatele pentru
% Rotation=1 È™i Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie sÄƒ aibÄƒ aceeaÈ™i reprezentare cu
% reprezentarea iniÈ›ialÄƒ.

% Pentru implementarea acestui predicat am rotit piesa pentru fiecare
% numar intre 0 si 3 primit ca parametru.
ccw(_, _, _) :- false.
ccw([X1, X2, X3, X4], 0, [X1, X2, X3, X4]).
ccw([X1, X2, X3, X4], 1, [X2, X3, X4, X1]).
ccw([X1, X2, X3, X4], 2, [X3, X4, X1, X2]).
ccw([X1, X2, X3, X4], 3, [X4, X1, X2, X3]).

% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagÄƒ RotationPairs la o listÄƒ de perechi
% (Rotation, RotatedTile)
% Ã®n care Rotation este un numÄƒr de rotaÈ›ii Ã®ntre 0 È™i 3 inclusiv È™i
% RotatedTile este reprezentarea piesei Tile rotitÄƒ cu numÄƒrul respectiv
% de rotaÈ›ii.
%
% Rezultatul trebuie Ã®ntotdeauna sÄƒ conÈ›inÄƒ perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie sÄƒ conÈ›inÄƒ rotaÈ›ii duplicate. De exemplu, pentru
% piesele 5,6 È™i 14 rezultatul va conÈ›ine doar 2 perechi, iar pentru
% piesa 16 rezultatul va conÈ›ine o singurÄƒ pereche.
%
% FolosiÈ›i recursivitate (nu meta-predicate).

% Predicat auxiliar ce primeste ca parametru o piesa din joc, un numar
% reprezentand numarul de perechi de forma (nrRotatie, Rotatie), pe care
% dorim sa le adaugam in lista finala intoarsa de predicat, si lista
% finala formata. Cazul de baza este acela in care intoarcem lista
% formata din perechea (0, piesaRespectiva). Cazul general este acela in
% care predicatul este adevarat daca e adevarat si pentru rotatiile
% anterioare construite in lista in finala(de aceea folosim "append" la
% final, pentru a arata ca "Rotations" este de fapt construit recursiv
% pe baza pasului anterior). Recursivitatea continua pana cand "I"
% devine egal cu 0.
rotAux(_, _, _):- false.
rotAux(T, 0, [(0, T)]).
rotAux(Tile, I, Rotations):-
    I > 0,
    I1 is I - 1,
    rotAux(Tile, I1, Rot1),
    ccw(Tile, I, RotTile),
    append(Rot1, [(I, RotTile)], Rotations).

% Implementarea consta in cazurile in care putem avea piese
% simetrice(de exemplu 5, 6 si 14, asa cum este descris si in cerinta),
% precum si piese ce contin o singura componenta pe toate directiile.
% Dupa tratarea acestor cazuri, predicatul este implementat pe baza
% celui anterior, apelat pentru I = 3.
rotations(_, _) :- false.
rotations([X, X, X, X], [(0, [X, X, X, X])]).
rotations([X, Y, X, Y], [(0, [X, Y, X, Y]), (1, [Y, X, Y,
X])]).
rotations(Tile, Rotations):-
    rotAux(Tile, 3, Rotations).


% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul Ã®ntoarce adevÄƒrat dacÄƒ NeighborTile poate fi pusÄƒ Ã®n
% direcÈ›ia NeighborDirection faÈ›Äƒ de Tile È™i se potriveÈ™te, adicÄƒ muchia
% comunÄƒ este de acelaÈ™i fel.
%
% De exemplu, dacÄƒ T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevÄƒrat.
%
% Similar, pentru piesele 8 È™i 10, este adevÄƒrat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% PuteÈ›i folosi predicatul opposite/2 din utils.pl.

% Pentru implementare folosesc predicatul "at" implementat mai sus,
% pentru a putea verifica daca cele doua piese au componente egale si
% directiile celor 2 componente sunt opuse(raportandu-ne la fiecare
% piesa in parte).
match(_, _, _) :- false.
match(Tile, NeighborTile, D):-
    at(Tile, D, X),
    at(NeighborTile, D1, X),
    opposite(D, D1).


% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagÄƒ Rotation la rotaÈ›ia (Ã®ntre 0 È™i 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel Ã®ncÃ¢t piesa sÄƒ se potriveascÄƒ cu
% vecinii din Neighbors.
%
% Neighbors este o listÄƒ de perechi (NeighborTile, NeighborDirection) È™i
% specificÄƒ cÄƒ pe direcÈ›ia NeighborDirection se aflÄƒ piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors sÄƒ conÈ›inÄƒ mai
% puÈ›in de 4 elemente.
%
% Se vor da toate soluÈ›iile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacÄƒ la nord se aflÄƒ piesa 14 rotitÄƒ o
% datÄƒ (drumul este vertical), iar la sud se aflÄƒ piesa 2 rotitÄƒ de 2
% ori (drumul este spre nord), atunci posibilele rotaÈ›ii pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie sÄƒ aibÄƒ 2 soluÈ›ii, Ã®n care
% leagÄƒ R la 1, È™i la 3.
% ÃŽn acelaÈ™i exemplu, dacÄƒ am avea È™i piesa 1 ca vecin spre est, atunci
% soluÈ›ia de mai sus s-ar reduce doar la rotaÈ›ia 3.
%
% Hint: Prolog face backtracking automat. FolosiÈ›i match/3.

% Pentru implementare consider cazul de baza, cand nu ne intereseaza ce
% intoarce predicatul pentru o lista vida de perechi. Pentru cazul
% general, generez toate rotatiile piesei si verific de fiecare data
% daca "Rot" apartine vreunei perechi din lista generata si daca Rotatia
% "Roti"(i = 1:3) astfel produsa se potriveste cu piesa vecina din lista
% de perechi de vecini, pe directia indicata. La final verific recursiv
% daca predicatul este adevarat si pentru restul listei de vecini.
findRotation(_, _, _) :- false.
findRotation(_, [], _).

findRotation(Tile, [(NTile, NDir) | T], Rot):-
    rotations(Tile, [(Rot, Tile) | _]),
    match(Tile, NTile, NDir),
    findRotation(Tile, T, Rot).

findRotation(Tile, [(NTile, NDir) | T], Rot):-
    rotations(Tile, [_, (Rot, Rot1) | _]),
    match(Rot1, NTile, NDir),
    findRotation(Tile, T, Rot).

findRotation(Tile, [(NTile, NDir) | T], Rot):-
    rotations(Tile, [_, _, (Rot, Rot2) | _]),
    match(Rot2, NTile, NDir),
    findRotation(Tile, T, Rot).

findRotation(Tile, [(NTile, NDir) | T], Rot):-
    rotations(Tile, [_, _, _, (Rot, Rot3)]),
    match(Rot3, NTile, NDir),
    findRotation(Tile, T, Rot).





%%%%%%%%%%%%%%%%%%%%%%%%%% Etapa 2

% emptyBoard/1
% emptyBoard(-Board)
%
% Leagă Board la reprezentarea unei table goale de joc (nu a fost
% plasată încă nicio piesă).

% Predicat auxiliar pentru generarea tablei goale.
% Tabla goala e reprezentata prin perechi de forma (Coordonata,
% SpatiuGol).
generateEmptyBoard(X, X, []).
generateEmptyBoard(X, Y, Pairs):-
    findall(
        ((A, B), ''),
        (
            between(X, Y, A),
            between(X, Y, B)
        ),
        Pairs
    ).

emptyBoard(_) :- false.
emptyBoard(Board):- generateEmptyBoard(-8, 8, Board).


% boardSet/4
% boardSet(+BoardIn, +Pos, +Tile, -BoardOut)
%
% Predicatul întoarce false dacă se încearcă plasarea unei piese pe o
% poziție pe care este deja o piesă, pe o poziție fără muchie comună
% cu o piesă existentă, sau într-un loc unde piesa nu se potrivește cu
% vecinii săi.
%
% Pentru o tablă goală, predicatul reușește întotdeauna, și poziția Pos
% devine singura de pe tablă.
%
% Poziția este dată ca un tuplu (X, Y).

% Predicat auxiliar pentru inlocuirea fiecarei aparitii a unei valori
% dintr-o lista cu o alta valoare.
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

% Pentru implementare am verificat daca piesa poate fi pusa pe tabla la
% positia respectiva, caz in care in locul pozitiei goale, pun piesa.
boardSet(_, _, _, _) :- false.
boardSet(Board, Pos, Tile, BoardOut):-
    canPlaceTile(Board, Pos, Tile),
    replace((Pos, ''), (Pos, Tile), Board, BoardOut).

% boardGet/3
% boardGet(+Board, +Pos, -Tile)
%
% Predicatul leagă Tile la reprezentarea piesei de pe tabla Board, de la
% poziția Pos. Poziția este dată ca un tuplu (X, Y).
%
% Dacă la poziția Pos nu este nicio piesă, predicatul eșuează.

% Pentru implementare am verificat daca perechea (Pos, Tile), cu "Tile"
% diferit de "spatiu liber", apartine tablei de joc.
boardGet(_, _, _) :- false.
boardGet(Board, Pos, Tile):-
    member((Pos, Tile), Board),
    Tile \= ''.

% canPlaceTile/3
% canPlaceTile(+Board, +Pos, +Tile)
%
% Întoarce adevărat dacă este o mișcare validă plasarea piese Tile la
% poziția Pos pe tabla Board. Poziția este dată ca un tuplu (X, Y).
%
% O mișcare este validă dacă tabla este goală sau dacă:
% - poziția este liberă;
% - poziția este adiacentă (are o muchie comună) cu o piesă deja
% existentă pe tablă;
% - piesa se potrivește cu toți vecinii deja existenți pe tablă.
%
% Hint: neighbor/3 și directions/1 , ambele din utils.pl

% Cazul in care tabla e goala si se pot pune piese pe ea.
canPlaceTile(_, _, _) :- false.
canPlaceTile(Board, _, _):-
    emptyBoard(Board).

% Cazurile in care tabla nu e goala.
% Caut toate piesele deja existente pe tabla si verific daca piesa pe
% care vreau sa o pun nu se afla pe tabla si se potriveste cu alta piesa
% pe anumita directie(de aici si cele 4 cazuri).
canPlaceTile(Board, Pos, Tile):-
   findall(((X, Y), T),
           (member(((X, Y), T), Board), T \= ''),
           Tiles),
   \+ member((Pos, Tile), Tiles),
   directions([D | _]),
   neighbor(Pos, D, Pos1),
   member((Pos1, T1), Tiles),
   match(Tile, T1, D).


canPlaceTile(Board, Pos, Tile):-
   findall(((X, Y), T),
           (member(((X, Y), T), Board), T \= ''),
           Tiles),
   \+ member((Pos, Tile), Tiles),
   directions([_, D | _]),
   neighbor(Pos, D, Pos1),
   member((Pos1, T1), Tiles),
   match(Tile, T1, D).

canPlaceTile(Board, Pos, Tile):-
   findall(((X, Y), T),
         (member(((X, Y), T), Board), T \= ''),
         Tiles),
 \+ member((Pos, Tile), Tiles),
 directions([_, _, D | _]),
   neighbor(Pos, D, Pos1),
   member((Pos1, T1), Tiles),
   match(Tile, T1, D).

canPlaceTile(Board, Pos, Tile):-
   findall(((X, Y), T),
           (member(((X, Y), T), Board), T \= ''),
           Tiles),
   \+ member((Pos, Tile), Tiles),
   directions([_, _, _, D | _]),
   neighbor(Pos, D, Pos1),
   member((Pos1, T1), Tiles),
   match(Tile, T1, D).


% boardGetLimits/5
% boardGetLimits(+Board, -XMin, -Ymin, -XMax, -YMax)
%
% Predicatul leagă cele 4 argumente la coordonatele x-y extreme la
% care există piese pe tablă.
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: max_list/2 și min_list/2

% Pentru implementare am bagat intr-o lista toate abscisele si in alta
% lista toate ordonatele, dupa care am extras minimele si maximele
% cerute.
boardGetLimits(_, _, _, _, _) :- false.
boardGetLimits(Board, XMin, YMin, XMax, YMax):-
    findall(X,
            (member(((X, _), T), Board), T \= ''),
            Horizontal
           ),
    findall(Y,
            (member(((_, Y), T), Board), T \= ''),
            Vertical
           ),
    min_list(Horizontal, XMin),
    max_list(Horizontal, XMax),
    min_list(Vertical, YMin),
    max_list(Vertical, YMax).


% getAvailablePositions/2
% getAvailablePositions(+Board, -Positions)
%
% Predicatul leagă Positions la o listă de perechi (X, Y)
% a tuturor pozițiilor de pe tabla Board unde se pot pune piese (poziții
% libere vecine pe o muchie cu piese existente pe tablă).
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: between/3 (predefinit) și neighbor/3 din utils.pl
%
% Atenție! Și în afara limitelor curente există poziții disponibile

% Pentru implementarea acestui predicat am adaugat intr-o lista toate
% pozitiile vecine libere ale pieselor deja existente pe tabla, din care
% la final am eliminat duplicatele(in unele cazuri, unele pozitii erau
% deja determinate din alte piese).
getAvailablePositions(_, _) :- false.
getAvailablePositions(Board, Positions):-
    \+ emptyBoard(Board),
    findall(
        Pos,
        (
            member(D, [n, e, s, w]),
            member(((X, Y), T), Board),
            T \= '',
            neighbor((X, Y), D, Pos),
            member((Pos, ''), Board)
        ),
        PositionsAux
    ),
    list_to_set(PositionsAux, Positions).



% findPositionForTile/4
% findPositionForTile(+Board, +Tile, -Position, -Rotation)
%
% Predicatul are ca soluții toate posibilele plasări pe tabla Board ale
% piesei Tile, legând Position la o pereche (X, Y) care reprezintă
% poziția și Rotation la un număr între 0 și 3 inclusiv, reprezentând de
% câte ori trebuie rotită piesa ca să se potrivească.
%
% Unele piese se pot potrivi cu mai multe rotații pe aceeași poziție și
% acestea reprezintă soluții diferite ale predicatului, dar numai dacă
% rotațiile duc la rezultate diferite.
%
% Dacă tabla este goală, predicatul leagă Position la (0, 0) și Rotation
% la 0.
%
% De exemplu, dacă pe tablă se află doar piesa 11, la vest de ea piesa 9
% se potrivește cu rotația 1 sau 2 - două soluții diferite. Pentru
% plasarea la vest de piesa 11 a piesei 16 însă există o singură soluție
% - rotație 0.

% De fiecare data putem aseza o piesa pe o tabla goala, nerotita, la
% orice pozitie.
% Cazul general(cand sunt mai multe piese pe tabla) este cel in care
% generez toate pozitiile libere, verific daca pozitia ce doresc a fi
% determinata este deja in lista de pozitii libere de pe tabla, gasesc
% toti vecinii pe toate directiile pentru pozitia respectiva, iar la
% final verific daca rotatia este cea cautata.
findPositionForTile(_, _, _, _) :- false.
findPositionForTile(Board, _, (0, 0), 0):- emptyBoard(Board).

findPositionForTile(Board, Tile, Pos, Rot):-
    getAvailablePositions(Board, Positions),
    member(Pos, Positions),
    findall(
        (T, D),
        (
            member(D, [n, e, s, w]),
            neighbor(Pos, D, Pos1),
            member((Pos1, T), Board),
            T \= ''
        ),
        Neighbors
    ),
    findRotation(Tile, Neighbors, Rot).
