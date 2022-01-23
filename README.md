# PROLOGway

## Identificação do trabalho e do grupo

#### Jogo escolhido

[Pathway](http://www.marksteeregames.com/Pathway_rules.pdf)

#### Grupo

**T5_Pathway4**

João Ferreira Baltazar 201905616 50%

Nuno Ricardo Teixeira da Costa 201906272 50%

## Instalação e Execução

### Instalação

Consultar o ficheiro game.pl na pasta do projeto - todas as dependências necesssárias serão carregadas.

### Execução

O predicado *play/0* inicia a aplicação.

## Descrição do jogo

O [Pathway](http://www.marksteeregames.com/Pathway_rules.pdf) (*Mark Steere, 2021*) é um jogo de tabuleiro para 2 jogadores, jogado num tabuleiro de xadrez 6x6 (ou 8x8), inicialmente vazio. Os dois jogadores que, na nossa adaptação, são Círculo e Cruz (em vez de Vermelho e Azul, por falta de suporte para cor no terminal), colocam, cada um na sua vez, uma peça num espaço desocupado no tabuleiro. Não podem passar o seu turno.

**Adjacências** são ortogonais (isto é, horizontais ou verticais). Uma **conexão amigável** é uma *adjacência* entre peças iguais. Uma **conexão inimiga** é uma *adjacência* entre peças diferentes.

É permitido colocar uma peça num espaço desocupado quando satisfaz um dos seguintes critérios:

* Não forma *conexões*
* Forma uma e uma só *conexão amigável* (nenhuma restrição quanto a *conexões inimigas*)

Vence o jogador que ficar sem jogadas.

## Lógica do Jogo

### Representação interna do estado do jogo

Para este jogo, basta guardar a disposição das peças no tabuleiro e o jogador atual e assim se tem o estado do jogo num dado momento. Deste modo, o GameState é um par Tabuleiro-Jogador, em que Tabuleiro é uma lista de listas de dimensões tamanho x tamanho, em que cada elemento representa um quadrado do tabuleiro, podendo ser `empty`, `circle` ou `cross`, e Jogador é um átomo que indica qual a peça a ser colocada neste turno (`circle` ou `cross`). O jogo pode ser inicializado com o predicado `initial_state(+Size, ?GameState)`, gerando um tabuleiro vazio de dimensões Size x Size e jogador inicial `circle`. Seria simples tornar o jogador inicial flexível também, mas visto que essa adição não adicionaria nenhuma variabilidade para lá da estética, foi decidido não o fazer para preservar a estrutura pedida do predicado. No ficheiro `examples.pl` é possível encontrar exemplos de estados inicial, intermédio e final.

### Visualização do estado de jogo

O predicado de visualização do estado do jogo é `display_game(+GameState)`, que aceita um estado de jogo e mostra na consola o estado do tabuleiro nesse momento. É bastante flexível, visto que decompõe a representação nas suas partes constituintes e modulariza de modo a aceitar qualquer tamanho e facilmente se poder trocar os caracteres que são usados para as linhas, colunas e peças.

No menu principal, há várias opções disponíveis: ***gamemode***, ***boardsize***, ***game***, ou ***leave***.

- **gamemode** - permite mudar o modo de jogo para cada jogador. opções:
  - **h** - humano, escolhe a jogada por si na consola
  - **pc-[1/2]** - computador, escolhe a jogada tendo em conta o seu nível de dificuldade (explicado com detalhe mais abaixo)
- **boardsize** - permite mudar o tamanho do tabuleiro para qualquer inteiro positivo
- **game** - começa uma partida com as definições previamente escolhidas
- **leave** - sai do jogo

O modo de jogo e o tamanho do tabuleiro são mantidos e alterados dinamicamente, assegurando facilidade e fiabilidade de uso. Os inputs do utilizador fora e dentro da partida são validados e sanitizados para que, por exemplo, não sejam injetadas variáveis através do `read/1`, ou as jogadas sejam realizadas em quadrados válidos.

### Execução de Jogadas

O predicado central do jogo é o `move(GameState, Move, NewGameState)`, que requer quaisquer dois argumentos e devolve o terceiro.
Para definir um movimento no Pathway, importa saber qual a peça a colocar e em que posição. Sendo que a primeira informação já é dada pelo GameState atual, resta a posição. Assim, Move é um par da forma Linha/Coluna, em que Linha e Coluna são inteiros começando em 0 que indicam a linha e a coluna em que é colocada a peça.
No contexto do nosso jogo, a adequada implementação do `move/3` implica a verificação de várias propriedades:

- o jogador a jogar troca entre os dois estados de jogo
- a jogada é feita num quadrado *in-bounds* previamente vazio
- os dois tabuleiros são idênticos salvo naquele quadrado
- aquele quadrado tem agora a peça correta
- a jogada não viola nenhuma das regras relativas a conexões.

### Final do Jogo

O predicado `game_over(+GameState, ?Winner)` determina o vencedor do jogo num dado estado, verificando que o jogador a ganhar está no seu turno e não tem jogadas disponíveis.

### Lista de Jogadas Válidas

O predicado `valid_moves(+GameState, -ListOfMoves)` devolve, através de ListOfMoves, a lista de jogadas válidas para avançar um dado GameState, recorrendo ao `findall/3`.

### Avaliação do Estado do Jogo

Visto que o objetivo de um jogador é ficar sem movimentos e impedir o outro jogador de ficar sem movimentos primeiro, uma heurística boa é a diferença entre o número de jogadas válidas para o jogador atual e o número de jogadas válidas para o oponente. Quanto menor este valor, melhor. Esta valoração está a cargo do predicado `value(+GameState, +Player, -Value)` que faz a contagem das jogadas válidas e o cálculo do valor final.

### Jogada do Computador

O computador escolhe a sua jogada recorrendo ao predicado `choose_move(+GameState, +Level, -Move)`. Existem duas opções para o nível de dificuldade:

1. Escolhe uma jogada válida aleatória recorrendo ao módulo *random*.
2. Escolhe a melhor jogada no momento (algoritmo míope) usando a valoração dada pelo `value/3`.

## Conclusões

O trabalho foi concluído com aproveitamento, tendo sido atingidas todas as metas obrigatórias e opcionais e conseguido um código bem estruturado, modular, legível e eficiente. As maiores limitações prendem-se com o facto dos predicados de I/O a que temos acesso serem muito limitados e mesmo até [desaconselhados pela documentação](https://www.swi-prolog.org/pldoc/man?predicate=read/1) para este fim, o que leva à criação de raiz de predicados que certamente não estão otimizados ou perfeitamente seguros quando comparados com as bibliotecas *standard*. Assim, o *roadmap* incluiria a transição para uma biblioteca recomendada para user-level I/O, e também a adição de mais um modo de jogo em que o computador pode escolher a melhor jogada para uma profundidade arbitrária.

## Bibliografia

- https://www.swi-prolog.org/
- http://www.marksteeregames.com/Pathway_rules.pdf
- Documentação da UC
