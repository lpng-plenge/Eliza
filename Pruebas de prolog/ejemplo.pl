
% introduccion
% es_perro(blacky).
% es_gato(tom).
% es_raton(jerry).

%relacion y objetos 
masgrande(elefante,caballo).
masgrande(caballo,perro).
masgrande(perro,raton).
masgrande(raton, hormiga).

% estructuras y reglas
% relacion de acebza y cuerpo
% estas relaciones se pueden hacer colocando el orden de las variables con letras del alfabeto en mayusculas
muchomasgrande(A,C):-masgrande(A,B),masgrande(B,C).


% reglas recursivas

