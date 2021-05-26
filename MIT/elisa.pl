% Eliza - main file
% Martin Mares <mmrmartin@gmail.com>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%Metodos De Prueba%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test:-
	out("[toLowerCase] "),
	toLowerCase("A","a"),toLowerCase("5","5"),
	toLowerCase("b","b"),toLowerCase(" "," "),
	toLowerCase(",",",").
test:-
	out("[toUpperCase] "),
	toUpperCase("A","A"),toUpperCase("5","5"),
	toUpperCase("b","B"),toUpperCase(" "," "),
	toUpperCase(",",",").
test:-
	out("[delteChars] "),
	string_chars("Ahoj, jak se mas.?",In),
	string_chars("Ahoj jak se mas?",Out),
	deleteChars(In, punctuation, Out).
test:-
	out("[readWord] "),
	string_chars("Ahoj jak se mas	baf",In),
	Out=['Ahoj','jak','se','mas','baf'],
	toWords(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Out/in interface %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% out(+Text):- imprime la salida del flujo out
out(Text):-write(Text).

% in(+Text):- lee la pregunta de la entrada del usuario
in(Text):-
	nl,
	write("> "),
	readLine(Text).

readLine(Text):-
	get_char(Char),
	toLowerCase(Char,LChar),
	readLine2(LChar,Text).
readLine2('\n',[]):-!.
readLine2(LChar,[LChar|T]):-readLine(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Utilidades Basicas %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% contantes predicados
charType('!', punctuation).
charType('?', punctuation).
charType('.', punctuation).
charType(',', punctuation).
charType('\'', punctuation).
charType(' ', whitespace).
charType('\t', whitespace).

% toLowerCase(+Char, -LChar):- minusculas char (using ASCI codes)
toLowerCase(Char, LChar):-
	char_code(Char, Code),
	Code >= "A",
	Code =< "Z",
	NewCode is Code + 32,
	char_code(LChar, NewCode), !.
toLowerCase(Char, Char).

% toUpperCase(+Char, -UChar):- mayusculas char (using ASCI codes)
toUpperCase(Char, UChar):-
	char_code(Char, Code),
	Code >= "a",
	Code =< "z",
	NewCode is Code - 32,
	char_code(UChar, NewCode), !.
toUpperCase(Char, Char).

% deleteChars(+Line, -Type, -Res):- eliminar un especifico charType de la linea
deleteChars([Char|Rest],Type,Out):-
	charType(Char, Type),
	deleteChars(Rest,Type,Out),!.
deleteChars([Char|Rest],Type,[Char|Out]):-
	deleteChars(Rest,Type,Out),!.
deleteChars([],_,[]).

% toWords(+Line, -Words):- transferir la salida de readLine a la lista de words
toWords([],[]):-!.
toWords(Line, [Word|ResWords]):-
	readWord(Line, Word, ResLine),
	toWords(ResLine, ResWords).

% readWord(+Line, -Word, -ResLine) :- le una palabra de line
% 	(el resto de las lineas es retornado en ResLine
readWord([], '', []).
readWord([Char|Res], '', Res) :- charType(Char, whitespace),!.
readWord([Char|ResLine], Word, Res) :- 
	readWord(ResLine, ResWord, Res),
	atom_concat(Char, ResWord, Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Funcion Elisa  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic resID/2.
resID(_,0).

% init:- inits el medio para las reglas de simplificacion
init:-
	consult("simplification.rules"),
	consult("reply.rules").	

% simplify(+In,-Out):- elimina los caracteres innecesarios, p.ej. "," and "." 
% 	y simplifica las palabras
simplify(In, Out):-
	deleteChars(In, punctuation, Out1),
	toWords(Out1,Out2),
	findSynonyms(Out2,Out3),
	Out = Out3.

% findSynonyms(+Words, -Synonyms) :- encuentras sinonimos usando
% 	reglas de simplificacion (loaded by init function)
findSynonyms(Words, Syn) :-
	sr(Words, Syn, RestWords, ResOutput),!,
	findSynonyms(RestWords, ResOutput).
findSynonyms([Word| ResWords], [Word| ResSyn]):-
	findSynonyms(ResWords, ResSyn),!.
findSynonyms([], []).

% findReply(+Words, -Reply) :- encuentra la respuesta con el rango mas alto
% 	(cargado por la funcion init)
findReply(Words, Reply) :-
	findReply2(Words, -2, 0, [], ID, Reply),
	ID \= 0,
	updateResID(ID).

% findReply2(+Words, +ActScore, +ActRuleID, +ActRes, -RuleID, -Res):- encuentra las respuesta usando
%	acumuladores
findReply2([H|T], ActScore, _, _, ID, Res):-
	findall(Score,rules(_, Score,[H|T],_),Rules),
	Rules \= [], % bagof no funciona como me esperabas
	max_list(Rules,NewScore),
	ActScore < NewScore,
	rules(NewID, NewScore,[H|T],Replyes),
	resID(NewID,ResID),
	nth0(ResID,Replyes,NewReply),
	findReply2(T, NewScore, NewID, NewReply, ID, Res),!.
findReply2([_|T], ActScore, ActID, ActRes, ID, Res):-
	findReply2(T, ActScore, ActID, ActRes, ID, Res).
findReply2([], _, ID, Res, ID, Res).

% updateResID(+ID):- pasa a la siguiente respuesta para la regla
updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	retract((resID(ID,RID):-!)),
	asserta(resID(ID,NRID):-!),!.
updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	asserta(resID(ID,NRID):-!).

% writeWords(+Words) - sube la primera letra y escribe las palabras en la salida 
writeWords([Word|Res]):-
	string_chars(Word,[Char|RChar]),
	toUpperCase(Char,UChar),
	readWord([UChar|RChar],Out,_),
	out(Out),
	writeWords2(Res).
% escribe la lista interna
writeWords2([Word|Res]):-
	is_list(Word),
	writeWords2(Word),
	writeWords2(Res),!.
% escribe la puntuacion
writeWords2([Word|Res]):-
	charType(Word,punctuation),
	out(Word),
	writeWords2(Res),!.
% escribe un estandar char
writeWords2([Word|Res]):-
	out(" "),
	out(Word),
	writeWords2(Res),!.
writeWords2([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%  Funcion Principal  %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elisa:-
	out("Buscando a Elisa....\n"),
	init,
	out("Aqui esta ella...\n\n"),
	out("Hola, Yo soy Elisa, en que puedo ayudarlo?"),
	elisa([hi]).

elisa([quitar|_]):-!.
elisa(_):-
	in(Line),
	simplify(Line, Words),
	findReply(Words,Reply),
	writeWords(Reply),nl,
	elisa(Words).

	

