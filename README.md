Cacaoscript
=======

Language and runtime cacaoweb

Prerequisites for build: 
* ocaml 4.01.0 or greater; 
* opam 1.1.1 or greater;
* the ocaml packages: batteries; config-file; delimcc; uuidm

To build:

* git clone https://github.com/leithaus/davinci.git
* cd davinci
* edit setup.data to point to your ocaml executables; making sure ocamlfind points to the opam ocamlfind
* ocaml setup.ml configure
* make
 
If you have any problems feel free to contact lgreg.meredith@gmail.com

    bash-3.2$ cacao
    *** Cacao Top Level version 0.01 *** 
    > ( fun x -> x )
    
    #<closure>
    > let x = 1 in x
    
    1
    > ( fun x -> x ) 1 ;;
    
    1
    > ( fun x -> ( x * 2.0 ) ) 2 ;; 
    
    >4.
    > ( fun x -> x ) ( fun x -> x ) ;; 
    
    #<closure>
    > :exit
    
    bash-3.2$ 

