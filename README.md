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

É possível cadastrar alunos, monitores e professores.

```
Quem você deseja cadastrar?
1) Cadastrar aluno
2) Cadastrar monitor
3) Cadastrar professor
4) Voltar para o menu principal
1

Insira o nome do aluno: 
MATEUS
Insira a matricula do aluno
12012547
Insira as disciplinas do aluno
["LOAC","OAC","PLP"]
Aluno cadastrado com sucesso.
```

### Tickets

```
== SAD: MENU ALUNO ==
ID: 120110001 | Nome: Kilian Melcher | Disciplinas: ["OAC","LOAC","PLP","Psoft"]
Digite o número da ação que deseja executar!

1) Matricular-se em disciplina
2) Desmatricular-se de disciplina
3) Criar Ticket
4) Mandar mensagem em um ticket
5) Ler tickets de uma disciplina
6) Marcar ticket como resolvido
7) Deslogar
3
Insira o nome da disciplina que você tem dúvida:
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
