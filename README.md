<h1 align="center">
  <img src="http://www.br2n.com/images/sad/sad.gif" width="100px" align="center">&nbsp;&nbsp;
  SAD
</h1>

# Serviço de atendimento ao discente (SAD)

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)

## Instalação

### Pré-requisitos

Para usar o SAD é preciso ter as seguintes ferramentas instaladas:
 - [GIT](https://git-scm.com/)
 - [Cabal](https://www.haskell.org/cabal/)

### Executando

1. Clonar repositório

```base
$ git clone https://github.com/kmmelcher/plp-sad.git
$ cd plp-sad
```

2. Configurar o Cabal e instalar as dependências

```bash
$ cabal update
$ cabal install cabal-install
$ cabal build
```

3. Executar o projeto
```bash
$ cabal run
```

# Usando o SAD


### Cadastro

É possível cadastrar alunos e monitores.

```
Quem você deseja vincular?
1) Vincular aluno
2) Vincular monitor
3) Voltar para o menu professor
1


Informe a sigla da disciplina relacionada:
OAC
Insira a matricula do aluno (digite 0 para voltar ao seu menu)
123456789
Este aluno não está cadastrado no SAD. Por favor, informe seu nome:
Samuel
Aluno cadastrado com sucesso e incluso na disciplina.
```

### Tickets

```
== SAD: MENU ALUNO ==
ID: 120110001 | Nome: Kilian Melcher | Disciplinas: ["OAC","LOAC","PLP","Psoft"]
Digite o número da ação que deseja executar!

1) Ler tickets de uma disciplina
2) Ler meus tickets
3) Criar Ticket
4) Mandar mensagem em um ticket meu
5) Marcar ticket como resolvido
6) Excluir ticket
7) Trocar senha de acesso
8) Deslogar
3

Insira a sigla da disciplina na qual deseja criar o ticket:
OAC
Insira um título para sua dúvida:
Registradores

Ticket adicionado com sucesso!
```

# Autores

- Kilian Melcher ([kmmelcher](https://github.com/kmmelcher))
- Vinícius Azevedo ([viniciussousaazevedo](https://github.com/viniciussousaazevedo))
- Pedro Adrian ([adrianmartinez-cg](https://github.com/adrianmartinez-cg))
- Filipe Ramalho ([musquitinh0](https://github.com/musquitinh0))
- Álef Ádonis ([AlefAdonis](https://github.com/AlefAdonis))
