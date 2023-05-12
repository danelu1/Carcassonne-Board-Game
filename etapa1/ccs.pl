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
tile(1, ([1, 0, 1], [c, c, p, c])).
tile(2, ([1, 1, 0], [c, c, d, c])).
tile(3, ([1, 0, 1], [c, c, p, p])).
tile(4, ([2, 0, 1], [c, c, p, p])).
tile(5, ([2, 0, 1], [c, p, c, p])).
tile(6, ([1, 0, 2], [c, p, c, p])).
tile(7, ([1, 0, 1], [c, p, p, p])).
tile(8, ([1, 1, 0], [c, c, d, d])).
tile(9, ([1, 1, 1], [c, p, d, d])).
tile(10, ([1, 1, 1], [c, d, d, p])).
tile(11, ([1, 1, 1], [c, d, p, d])).
tile(12, ([1, 2, 0], [c, d, d, d])).
tile(13, ([0, 1, 1], [p, p, d, d])).
tile(14, ([0, 1, 2], [p, d, p, d])).
tile(15, ([0, 2, 1], [p, d, d, d])).
tile(16, ([0, 0, 2], [d, d, d, d])).


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
at((_, [X | _]), D, X):- directions([D | _]).
at((_, [_, X | _]), D, X):- directions([_, D | _]).
at((_, [_, _, X | _]), D, X):- directions([_, _, D | _]).
at((_, [_, _, _, X]), D, X):- directions([_ ,_, _, D]).


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
ccw((_, [X1, X2, X3, X4]), 0, (_, [X1, X2, X3, X4])).
ccw((_, [X1, X2, X3, X4]), 1, (_, [X2, X3, X4, X1])).
ccw((_, [X1, X2, X3, X4]), 2, (_, [X3, X4, X1, X2])).
ccw((_, [X1, X2, X3, X4]), 3, (_, [X4, X1, X2, X3])).

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
rotations((_, [X, X, X, X]), [(0, (_, [X, X, X, X]))]).
rotations((_, [X, Y, X, Y]), [(0, (_, [X, Y, X, Y])), (1, (_, [Y, X, Y,
X]))]).
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
