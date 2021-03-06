# Pong

Um clone do jogo [Pong](https://pt.wikipedia.org/wiki/Pong), escrito em elm, uma linguagem funcional.


## Build

Atualmente, você consegue rodar o código utilizando Windows.

**Pré-Requisitos**

- [elm](https://guide.elm-lang.org/install.html)

**Deixando tudo pronto**

Primeiramente, comece clonando o repositório
```
git clone https://github.com/LucasSchurer/elm-pong.git
```
Vá até o local onde o repositório foi baixado, executando os seguintes comandos:
```
elm init
elm install elm/svg
elm install elm/json
elm install elm/time
elm make .\src\Game.elm
```
Um arquivo chamado "index.html" será criado na pasta do jogo. Abra-o com seu navegador preferido (por favor seja o chrome) e comece a jogar :)

## Observações
O jogo disponível no site e o gerado seguindo os passos acima está na resolução de 1280x720. Para modificar a resolução do arquivo gerado, siga os seguintes passos:
- Procure pela função `init : () -> ( Model, Cmd.none )`
- Dentro do bloco `let`, mude a variável `windowSettings = initWindowSettings X Y`, substituindo X e Y pela resolução desejada.
- No terminal, rode novamente o comando `elm make .\src\Game.elm`
- Abra o arquivo gerado "index.html", agora com a resolução nova.

## Objetivo

Criado como uma atividade complementar da disciplina de Paradigmas de Programação, o objetivo é de conseguir elaborar um jogo realizado em elm, tentando notar as diferenças no desenvolvimento por conta do uso de uma linguagem puramente funcional, e não imperativa.

## Metas

 - [x] Criar um README para o programa. 
 - [X] Desenhar o básico das informações visuais do jogo. 
 - [X] Implementar a movimentação dos jogadores, podendo com que os dois se movam ao mesmo tempo.
 - [X] Fazer com que a bola se mova.
 - [X] Implementar um sistema de game-over.
 - [X] Implementar colisão entre a bola e os jogadores, fazendo com que a trajetória da bola mude.
 - [X] Aumentar os pontos de cada jogador quando a bola encontra a a lateral oposta da tela.
 - [X] Aumentar a velocidade da bola aumente cada vez que é rebatida por um jogador.
 - [X] Criação de um sistema de pause.
 - [ ] Criação de um menu anterior a tela do jogo.
   - [ ] Botão de Jogar
   - [ ] Instruções na tela.
   
 
 ## Progresso
 
 Você pode acompanhar o que está sendo feito atualmente no projeto no meu [site](https://lucasschurer.github.io/elm-pong/index).





