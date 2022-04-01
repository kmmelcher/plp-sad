:- use_module('Menu.pl', [menuPrincipal/0]).

main():- 
    prompt(_, ''),
    menuPrincipal().
