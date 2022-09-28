% Andre Morgado 92737 LEIC-T
:-[codigo_comum].
%-------------------------------------------------------------------------------------
%                   P1 - extrai_ilhas_linha(N_L, Linha, Ilhas)
%-------------------------------------------------------------------------------------
/* N_L eh um inteiro positivo, correspondente ao numero de uma linha e Linha eh uma lista 
correspondente a uma linha de um puzzle, significa que Ilhas eh a lista ordenada 
cujos elementos sao as ilhas da linha Linha */

% N_C = numero da coluna (vai de 1 a length(Linha), icrementando 1 em cada iteracao).
% N_P = numero de pontes.
extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, 1, Linha, [], Ilhas).

extrai_ilhas_linha(_, _, [], Ilhas, Ilhas).

% Se N_I \= 0, existe ilha na linha, adicionamo-la ao Aux e incrementamos N_C.
extrai_ilhas_linha(N_L, N_C, [N_P | Resto], Aux, Ilhas) :-
    N_P \= 0, !,
    append(Aux, [ilha(N_P, (N_L, N_C))], NovoAux),
    NovoN_C is N_C + 1,
    extrai_ilhas_linha(N_L, NovoN_C, Resto, NovoAux, Ilhas).

% Se N_I == 0, nao exste peca, apenas incrementamos N_C.
extrai_ilhas_linha(N_L, N_C, [N_P | Resto], Aux, Ilhas) :-
    N_P == 0, !,
    NovoN_C is N_C + 1,
    extrai_ilhas_linha(N_L, NovoN_C, Resto, Aux, Ilhas).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                                P2 - ilhas(Puz, Ilhas)
%-------------------------------------------------------------------------------------
/* Puz eh um puzzle, significa que Ilhas eh a lista ordenada cujos elementos sao as 
ilhas de Puz.*/

ilhas(Puz, Ilhas) :- ilhas(Puz, 1, [], Ilhas).

ilhas([], _, Ilhas, Ilhas).

ilhas([Linha | Resto], N_L, Aux, Ilhas) :-
    extrai_ilhas_linha(N_L, Linha, NovasIlhas), % NovasIlhas guarda as ilhas da Linha
    append(Aux, NovasIlhas, NovoAux),
    NovoN_L is N_L + 1,
    ilhas(Resto, NovoN_L, NovoAux, Ilhas).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                         P3 - vizinhas(Ilhas, Ilha, Vizinhas)
%-------------------------------------------------------------------------------------
 /* Ilhas eh a lista de ilhas de um puzzle e Ilha eh uma dessas ilhas, significa que 
 Vizinhas eh a lista ordenada cujos elementos sao as ilhas vizinhas de Ilha */

vizinhas(Ilhas, Ilha, Vizinhas) :-
    % Obtemos a lista de ilhas na mesma linha que Ilha (incluindo ela mesma)
    include(possiveis_viz_de_l(Ilha), Ilhas, Possiveis_Vizinhas_L), 
    % Ilhas adjacentes a Ilha na lista Possiveis_Vizinhas_L sao vizinhas de Ilha,
    % pois, sao as que estao mais proximas
    adjacentes(Possiveis_Vizinhas_L, Ilha, [Viz_Esq, Viz_Dir]),
    % Faz-se o mesmo para as colunas
    include(possiveis_viz_de_c(Ilha), Ilhas, Possiveis_Vizinhas_C),
    adjacentes(Possiveis_Vizinhas_C, Ilha, [Viz_Cima, Viz_Baixo]),
    Aux = [Viz_Cima, Viz_Esq, Viz_Dir, Viz_Baixo], % Lista ordenada das vizinhas
    delete(Aux, [], Vizinhas). % Eliminamos '[]' da lista, caso exista

/* Predicado auxiliar que se verifica se duas ilhas estiverem na mesma linha (sao 
possiveis vizinhas) */
possiveis_viz_de_l(ilha(_, (X, _)), ilha(_, (X, _))).

/* Predicado auxiliar que se verifica se duas ilhas estiverem na mesma colunas (sao 
possiveis vizinhas) */
possiveis_viz_de_c(ilha(_, (_, Y)), ilha(_, (_,Y))).

/* Predicado auxiliar que se verifica se Adjacentes for a lista dos dois elementos
adajacentes a Elem na lista Lista (caso nao exista adjacente ah esquerda ou ah direita, 
representa-se um []). Exemplo: :- adjacentes([1,2,3], 2, X). X = [1,3]. */
adjacentes(Lista, Elem, Adjacentes) :-
    nth1(I, Lista, Elem),  % Obtemos I, indice de El na Lista
    length(Lista, Tamanho),
    Antes is I - 1,
    Depois is I + 1,
    (I \= 1                              % Um elemento nao tem elementos adjacentes se Elem 
    -> nth1(Antes, Lista, Adj_Esquerda)  % estiver no extremo da Lista, i.e., quando I == 1 
    ; Adj_Esquerda = []),                %ou I == Tamanho, nestes casos "coloca-se" '[]' em Adjacentes 
    (I \= Tamanho
    -> nth1(Depois, Lista, Adj_Direita)
    ; Adj_Direita = []),
    append([Adj_Esquerda], [Adj_Direita], Adjacentes).



%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                         P4 - estado(Ilhas, Estado)
%-------------------------------------------------------------------------------------
/* Ilhas eh a lista de ilhas de um puzzle, significa que Estado eh a lista ordenada 
cujos elementos sao as entradas referentes a cada uma das ilhas de Ilhas. */

estado(Ilhas, Estado) :-
    maplist(estado_aux(Ilhas), Ilhas, Estado).

/* Predicado auxiliar onde Ilhas eh a lista de ilhas de um puzzle, Ilha uma ilha dessa lista
e o Estado corresponde ao estado da ilha ([Ilha, Vizinhas, Pontes])*/ 
estado_aux(Ilhas, Ilha, Estado):-
    vizinhas(Ilhas, Ilha, Vizinhas),
    Estado = [Ilha, Vizinhas, []].

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                   P5 - posicoes_entre(Pos1, Pos2, Posicoes)
%-------------------------------------------------------------------------------------
/* Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2).

Nota:
Neste predicado altero sucessivamente a Pos2 que eh, em cada iteracao, pontos sucessivos
na linha ou na coluna (aproximando-se de Pos1) estes sao adicionados a uma variavel 
auxiliar ate que Pos1 == Pos2 */

posicoes_entre(Pos1, Pos2, Posicoes) :- posicoes_entre(Pos1, Pos2, [], Posicoes).

/* Ordeno a variavel auxiliar, Aux, numa nova, N_Aux (atraves do sort); Posicoes eh N_aux 
ao qual se remove o ponto Pos (atraves do delete)*/
posicoes_entre(Pos, Pos, Aux, Posicoes) :- sort(Aux, N_Aux), delete(N_Aux, Pos, Posicoes).

% No caso das posicoes estarem na mesma linha (X1 = X2 = X)
posicoes_entre((X, Y1), (X, Y2), Aux, Posicoes) :-
    (Y1 < Y2 % Pos1 esta a esquerda de Pos2
    -> N_Y2 is Y2 - 1 % Avancamos Pos2 uma posicao para esquerda
    ; N_Y2 is Y2 + 1),% Caso contrario Pos1 estah a direita, avancamos Pos2 uma posicao para a direta
    append(Aux, [(X, N_Y2)], N_Aux), % Adicionamos este 'novo Pos2' a N_Aux
    posicoes_entre((X, Y1), (X, N_Y2), N_Aux, Posicoes).

% No caso das posicoes estarem na mesma coluna (Y1 = Y2 = Y), aplico o mesmo raciocinio que o predicado anterior
posicoes_entre((X1, Y), (X2, Y), Aux, Posicoes) :-
    (X1 < X2
    -> N_X2 is X2 - 1
    ; N_X2 is X2 + 1),
    append(Aux, [(N_X2, Y)], N_Aux),
    posicoes_entre((X1, Y), (N_X2, Y), N_Aux, Posicoes).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                     P6 - cria_ponte(Pos1, Pos2, Ponte)
%-------------------------------------------------------------------------------------
/* Ponte eh uma ponte entre Pos1 e Pos2 */

cria_ponte(Pos1, Pos2, Ponte) :- 
    sort([Pos1, Pos2], [P1, P2]),
    Ponte = ponte((P1), (P2)).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                  P7 - caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
%-------------------------------------------------------------------------------------
/* Pos1 e Pos2 sao posicoes, Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2, 
I eh uma ilha, e Vz eh uma das suas vizinhas, significa que a adicao da ponte 
ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser vizinhas. */

% O terceiro elemento do include sera um lista com um ponto ou a lista vazia (caso em que
% o caminho esta livre)
caminho_livre(_, _, Posicoes, I, Vz) :-
    include(ponto_entre_ilhas(I, Vz), Posicoes, []).

% Nao deixam de ser vizinhas caso a ponte adicionada seja entre I e Vz
caminho_livre(Pos1, Pos2, _, ilha(_, Pos_I), ilha(_, Pos_Vz)) :-
    Pos1 == Pos_I, Pos2 == Pos_Vz;
    Pos1 == Pos_Vz, Pos2 == Pos_I.

/* Predicado auxiliar que se verifica se o ponto (X, Y) estah entre a Ilha e a Vizinha.

Nota: Dado o conceito de vizinhas, este predicado so se verifica se uma coordenada (X ou Y) for igual
para Ilha, Vz e Ponto. Em particular este predicado funciona porque se os argumentos de between forem todos
iguais este devolve true (por exemplo :- between(1,1,1). true) */
ponto_entre_ilhas(ilha(_, (X_Ilha, Y_Ilha)), ilha(_, (X_Vz, Y_Vz)), (X, Y)) :-
    between_aux(Y_Ilha, Y_Vz, Y),
    between_aux(X_Ilha, X_Vz, X).

/* Predicado auxiliar similar ao between "tradicional", mas se for colocado um A>B,
tem o mesmo comportamento que between(A,B) */
between_aux(A, B, Res) :-
    (A =< B
    -> between(A, B, Res)
    ; between(B, A, Res)).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%    P8 - actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
%-------------------------------------------------------------------------------------
/*Pos1 e Pos2 sao as posicoes entre as quais ira ser adicionada uma ponte, Posicoes eh 
a lista ordenada de posicoes entre Pos1 e Pos2, e Entrada eh uma entrada, significa que 
Nova_Entrada eh igual a Entrada, excepto no que diz respeito ah lista de ilhas vizinhas; 
esta deve ser actualizada, removendo as ilhas que deixaram de ser vizinhas, apos a adicao 
da ponte.*/

% Da lista de vizinhas (Vz) de cada ilha I, incluimos aquelas que verificam o predicado
% caminho livre numa nova lista (Novo_Vz)
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [I, Vz, Pontes], Nova_Entrada) :-
    include(caminho_livre(Pos1, Pos2, Posicoes, I), Vz, Novo_Vz),
    Nova_Entrada = [I, Novo_Vz, Pontes].



%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%    P9 - actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
%-------------------------------------------------------------------------------------
/* Estado eh um estado, Pos1 e Pos2 sao as posicoes entre as quais foi adicionada uma 
ponte, significa que Novo_estado eh o estado que se obtem de Estado apos a actualizacao 
das ilhas vizinhas de cada uma das suas entradas. */

% Adicionamos como "argumento" do nosso predicado a lista de posicoes entre Pos1 e Pos2,
% (Posicoes)
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Posicoes, Novo_estado).

% Atraves do maplist atualizamos cada entrada do Estado e colocomaos no Novo_estado
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Posicoes, Novo_estado) :-
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%                        P10 - ilhas_terminadas(Estado, Ilhas_term)
%-------------------------------------------------------------------------------------
/* Estado eh um estado, significa que Ilhas_term eh a lista de ilhas que ja tem todas 
as pontes associadas, designadas por ilhas terminadas */

ilhas_terminadas(Estado, Ilhas_term) :-
    maplist(obtem_ilha_se_terminada, Estado, Aux), % Obtemos uma lista de ilhas terminadas ([ilha]) e []
    append(Aux, Ilhas_term).

% Predicado auxiliar "que tem como argumento" uma entrada, e Ilha_Terminada eh a ilha
% dessa entrada se essa for referente a uma ilha terminada, caso contrario eh []
obtem_ilha_se_terminada([ilha(N_pontes, Pos), _, Pontes], Ilha_Terminada) :-
    (N_pontes \= 'X', length(Pontes, N_pontes)
    -> Ilha_Terminada = [ilha(N_pontes, Pos)]
    ; Ilha_Terminada = []).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%         P11 - tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%-------------------------------------------------------------------------------------
/* Ilhas_term eh uma lista de ilhas terminadas e Entrada eh uma entrada, significa que 
Nova_entrada eh a entrada resultante de remover as ilhas de Ilhas_term, da lista de ilhas 
vizinhas de entrada. */
tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], Nova_entrada) :-
    exclude(eh_membro(Ilhas_term), Vizinhas, Novo_Vizinhas),
    Nova_entrada = [Ilha, Novo_Vizinhas, Pontes].

/* Predicado auxilar similar a member, mas com os argumentos trocados */     
eh_membro(Lista, Elem) :-
    member(Elem, Lista).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%         P12 - tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
%-------------------------------------------------------------------------------------
/* Estado eh um estado e Ilhas_term eh uma lista de ilhas terminadas, significa que 
Novo_estado eh o estado resultante de aplicar o predicado tira_ilhas_terminadas_entrada 
a cada uma das entradas de Estado */

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%      P13 - marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%-------------------------------------------------------------------------------------
/* Ilhas_term eh uma lista de ilhas terminadas e Entrada eh uma entrada, significa que 
Nova_entrada eh a entrada obtida de Entrada da seguinte forma: se a ilha de Entrada pertencer 
a Ilhas_term, o numero de pontes desta eh substituido por 'X'; em caso contrario Nova_entrada
e igual a Entrada */

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_Pontes,Pos), Viz, Pontes], Nova_entrada) :-
    (member(ilha(N_Pontes, Pos), Ilhas_term)
    -> Nova_entrada = [ilha('X', Pos), Viz, Pontes]
    ; Nova_entrada = [ilha(N_Pontes, Pos), Viz, Pontes]).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%               P14 - marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
%-------------------------------------------------------------------------------------
/* Estado eh um estado e Ilhas_term eh uma lista de ilhas terminadas, significa que 
Novo_estado eh o estado resultante de aplicar o predicado marca_ilhas_terminadas_entrada 
a cada uma das entradas de Estado. */

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%               P15 -  trata_ilhas_terminadas(Estado, Novo_estado)
%-------------------------------------------------------------------------------------
/* Estado eh um estado, significa que Novo_estado eh o estado resultante de aplicar os 
predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado. */

 trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Aux),
    marca_ilhas_terminadas(Aux, Ilhas_term, Novo_estado).

%-------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------
%             P16 -  junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
%-------------------------------------------------------------------------------------
/* Estado eh um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado eh o estado 
que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e Ilha2. */

junta_pontes(Estado, Num_pontes, ilha(N1, Pos1), ilha(N2, Pos2), Novo_estado) :-
    cria_ponte_aux(Pos1, Pos2, Num_pontes, Pontes_Adicionadas), % Cria-se a lista com as pontes adicionadas
    maplist(adiciona_pontes_entrada(Pontes_Adicionadas, ilha(N1, Pos1), ilha(N2, Pos2)), Estado, Aux1), % Atualiza-se cada entrada do estado 
    actualiza_vizinhas_apos_pontes(Aux1, Pos1, Pos2, Aux2),                                         
    trata_ilhas_terminadas(Aux2, Novo_estado).

/* Predicado auxiliar, similar ao cria_ponte, mas com o detalhe que Pontes eh uma lista com
Num_pontes de pontes entre Pos1 e Pos2. */

cria_ponte_aux(Pos1, Pos2, Num_pontes, Pontes) :- 
    cria_ponte_aux(Pos1, Pos2, Num_pontes, [], Pontes).

cria_ponte_aux(_, _, 0, Pontes, Pontes).

cria_ponte_aux(Pos1, Pos2, Num_pontes, Aux, Pontes) :-
    cria_ponte(Pos1, Pos2, Ponte),
    append(Aux, [Ponte], Novo_Aux),
    Novo_Num_Pontes is Num_pontes - 1,
    cria_ponte_aux(Pos1, Pos2, Novo_Num_Pontes, Novo_Aux, Pontes). 

/* Predicado auxiliar onde Pontes_Adicionadas sao as novas pontes criadas, a Ilha1 e Ilha2
as ilhas para as quais se fizeram as pontes; e a Nova_Entrada que se obtem atraves da substituicao
de Pontes por Pontes_Adicionadas. */
adiciona_pontes_entrada(Pontes_Adicionadas, Ilha1, Ilha2, [Ilha, Viz, Pontes], Nova_Entrada) :-
    ((Ilha == Ilha1; Ilha == Ilha2) % A substituicao so ocorre no caso de a entrada ser referente a uma 
    ->append(Pontes, Pontes_Adicionadas, Novo_Pontes), % das ilhas as quais se adicionou a ponte
    Nova_Entrada = [Ilha, Viz, Novo_Pontes]
    ; Nova_Entrada = [Ilha, Viz, Pontes]). % Caso contrario a Nova_Entrada eh a entrada antiga


