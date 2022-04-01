## Instalação

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)

### Pré-requisitos

Para usar o SAD é preciso ter as seguintes ferramentas instaladas:
 - [Haskell](https://www.haskell.org/downloads/#package-manager) 
 - [Cabal](https://www.haskell.org/cabal/)

### Executando
![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)
![Windows](https://img.shields.io/badge/Windows-017AD7?style=for-the-badge&logo=windows&logoColor=white)


1. Configurar o Cabal e instalar as dependências

```bash
$ cd haskell
$ cabal update
$ cabal install cabal-install
$ cabal build
```

2. Executar o projeto
```bash
$ cabal run
```
