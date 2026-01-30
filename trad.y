//Karla Puertas Escribano Ada Zurdo Herranz 515
//100495968@alumnos.uc3m.es 100495900@alumnos.uc3m.es
%{                          // SECCION 1 Declaraciones de C-Yacc

#include <stdio.h>
#include <ctype.h>            // declaraciones para tolower
#include <string.h>           // declaraciones para cadenas
#include <stdlib.h>           // declaraciones para exit ()

#define FF fflush(stdout);    // para forzar la impresion inmediata

int yylex () ;
int yyerror () ;
char *mi_malloc (int) ;
char *gen_code (char *) ;
char *int_to_string (int) ;
char *char_to_string (char) ;
/////
//VARRIABLES LOCALES
/////
void nuevaVarLocal(char *nombre);
int esLocal(char *nombre);

void nuevoParametroLocal(char *nombre);
int esParametro(char *nombre);

void nuevaVariableGlobal(char *nombre);
int esGlobal(char *nombre);

char temp [2048] ;

// Abstract Syntax Tree (AST) Node Structure

typedef struct ASTnode t_node ;

struct ASTnode {
    char *op ;
    int type ;		// leaf, unary or binary nodes
    t_node *left ;
    t_node *right ;
} ;


// Definitions for explicit attributes

typedef struct s_attr {
    int value ;    // - Numeric value of a NUMBER 
    char *code ;   // - to pass IDENTIFIER names, and other translations 
    t_node *node ; // - for possible future use of AST
} t_attr ;
/////////////
//VARIABLES LOCALES
//////////////
typedef struct {
    char nombre[32];
} VarLocal;

VarLocal tablaLocales[100];
int nLocales = 0;
char funcionActual[32] = "";

/////////////
//PARAMETROS
//////////////
typedef struct {
    char nombre[32];
} ParametroLocal;

ParametroLocal tablaParametros[100];
int nParametros = 0;
/////////////
//VARIABLES GLOBALES
//////////////
typedef struct {
    char nombre[32];
} VariableGlobal;

VariableGlobal tablaGlobales[100];
int nGlobales = 0;


#define YYSTYPE t_attr

%}

// Definitions for explicit attributes

%token NUMBER        
%token IDENTIF       // Identificador=variable
%token INTEGER       // identifica el tipo entero
%token STRING
%token MAIN          // identifica el comienzo del proc. main
%token WHILE         // identifica el bucle main
%token PUTS         // identifica puts
%token PRINTF         // identifica print
%token IF
%token ELSE
%token FOR
//%token RETURN
/////////
%token AND
%token OR
%token EQ
%token MAYOR_EQ
%token MENOR_EQ
%token NOT_EQ
%token RETURN


%right '='                    // es la ultima operacion que se debe realizar
%left OR
%left AND
%left EQ
%left NOT_EQ
%left '<'
%left MENOR_EQ
%left '>'
%left MAYOR_EQ
%left '+' '-'                 // menor orden de precedencia
%left '%'
%left '*' '/'                 // orden de precedencia intermedio
%left '!'
%left UNARY_SIGN              // mayor orden de precedencia


%%                            // Seccion 3 Gramatica - Semantico


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

axioma:       declaracion_global lista_definiciones_func definiciones_main         {;}
         ;

/*-------------------------------------------------------------------------------------------------------------DEFINICIONES FUNCIONES------------------------------------------------------------------------------------------------------------------------*/

definiciones_main: MAIN  '('  ')' {strcpy(funcionActual, "main");} '{' lista_sentencias '}'  {nLocales = 0; nParametros = 0;printf ("\n\n(defun main ()\n %s\n)\n", $6.code); } 
;

lista_definiciones_func: 	{$$.code = NULL;} 
			|	definiciones_func	{$$ = $1;}
;

definiciones_func: IDENTIF  '(' lista_parametros ')' {strcpy(funcionActual, $1.code);} '{'  lista_sentencias '}'  {nLocales = 0; nParametros = 0; printf ("\n\n(defun %s (%s)\n\%s \n)\n",funcionActual, $3.code, $7.code);} resto_definicionesFunc
;

resto_definicionesFunc: definiciones_func	 {$$ = $1;}
		|	{$$.code= gen_code("");}
;

/*------------------------------------DECLARACIONES PARA FUNCIONES-----------------------------------------------------------------------*/
lista_parametros: 	{$$.code = gen_code ("");}
		|	parametros {$$ = $1;}
;
parametros:	INTEGER IDENTIF  r_parametros {nuevoParametroLocal($2.code); if ($3.code != NULL) {sprintf (temp, "%s %s", $2.code, $3.code) ;  $$.code = gen_code (temp);} else {
              							sprintf (temp, "%s", $2.code) ;  $$.code = gen_code (temp); } ;} 
;



r_parametros: 	',' parametros	{$$ = $2;}
		|	{$$.code = NULL;}
;


/*-------------------------------------------------------------------------------------------------------------SENTENCIAS------------------------------------------------------------------------------------------------------------------------*/
lista_sentencias:	sentencia  r_sentencia	{if ($2.code != NULL){sprintf (temp, "\t%s\n%s", $1.code, $2.code) ;  $$.code = gen_code (temp);
							} else { sprintf (temp, "\t%s", $1.code) ;  $$.code = gen_code (temp);};}
	;
r_sentencia:  lista_sentencias		{$$=$1;}
		|			{$$.code = NULL;}
	;

sentencia:    IDENTIF '=' expresion  ';'    {if (esParametro($1.code) == 1 || (esGlobal($1.code) == 1) ){
							sprintf (temp, "(setf %s %s)", $1.code, $3.code);  $$.code = gen_code (temp) ;
						} else {
							if (esLocal($1.code) == 1) {
								sprintf (temp, "(setf %s_%s %s)", funcionActual, $1.code, $3.code); $$.code = gen_code (temp) ;
							} else {
								sprintf (temp, "(setq %s_%s %s)", funcionActual, $1.code, $3.code);  $$.code = gen_code (temp) ;
								nuevaVarLocal($1.code);
							}
						};
					}

		| INTEGER IDENTIF  '=' 	expresion ';'	{nuevaVarLocal($2.code); sprintf (temp, "(setq %s_%s %s)", funcionActual, $2.code, $4.code) ; $$.code = gen_code (temp);}
                                          	 

        | PUTS '(' STRING ')' 	';'		{sprintf (temp, "(print \"%s\")", $3.code) ; 
                                           	$$.code = gen_code (temp) ;}
                                           	
                                           	
        | PRINTF '(' STRING ',' resto_printf ')' ';'	{$$ = $5;}
        
        | WHILE '(' expresion ')' '{' lista_sentencias '}'	{sprintf (temp, "(loop while %s do\n%s\n\t)", $3.code, $6.code) ; 
                                           		$$.code = gen_code (temp) ;}
                                           		
        | IF '(' expresion ')' '{' lista_sentencias '}'  {sprintf (temp, "(if %s\n\t(progn %s)\n\t)", $3.code, $6.code) ; 
                                           	$$.code = gen_code (temp);}

        | IF '(' expresion ')' '{' lista_sentencias '}' ELSE '{' lista_sentencias '}'	{sprintf (temp, "(if %s\n\t(progn %s)\n(progn %s)\n\t)", $3.code, $6.code, $10.code) ; $$.code = gen_code (temp);}
        
        |  FOR '(' declaracion_inicio_for ';' expresion ';' declaracion_fin_for')' '{' lista_sentencias '}'	{sprintf (temp, "%s (loop while %s do %s %s)", $3.code,$5.code, $10.code,$7.code) ; $$.code = gen_code (temp);}
        
        | RETURN expresion ';' {sprintf (temp, "(return-from %s %s)", funcionActual, $2.code) ; $$.code = gen_code (temp);}
        
        | IDENTIF'(' lista_parametros_llamada ')' ';' {if ($3.code != NULL){sprintf (temp, "(%s %s)", $1.code, $3.code) ; $$.code = gen_code (temp);}else{sprintf (temp, "(%s)", $1.code) ; $$.code = gen_code (temp);};}
        
        | IDENTIF '[' expresion ']''=' expresion  ';'    {sprintf (temp, "(setf (aref %s %s) %s)", $1.code,$3.code, $6.code); $$.code = gen_code (temp);}
      
      | INTEGER IDENTIF '[' expresion ']' ';'	{nuevaVarLocal($2.code); sprintf (temp, "(setq %s (make-array %d))", $2.code, $4.value) ; $$.code = gen_code (temp) ; }
       
       |  INTEGER IDENTIF ';'	{nuevaVarLocal($2.code); sprintf (temp, "(setq %s_%s 0)", funcionActual, $2.code) ; $$.code = gen_code (temp);}
       ;



/*-------------------------------------------------------------------------------------------------------------PARA SENTENCIAS------------------------------------------------------------------------------------------------------------------------*/
lista_parametros_llamada:		{$$.code = NULL;}
			|  parametros_llamada {$$=$1;}
;

parametros_llamada: expresion resto_number { if ($2.code != NULL){sprintf (temp, "%s %s ", $1.code, $2.code) ; $$.code = gen_code (temp);}else {sprintf (temp, "%s", $1.code) ; $$.code = gen_code (temp);} ;} 
;

resto_number:	',' parametros_llamada {$$=$2;}
		|	{$$.code = NULL;}
;
/*declaracion iteraciones para el for*/


declaracion_inicio_for: IDENTIF '=' NUMBER           { sprintf (temp, "(setq %s_%s %d)", funcionActual ,$1.code, $3.value) ; 
                                           $$.code = gen_code (temp) ; }
            ;
           
declaracion_fin_for:  IDENTIF '=' expresion          { sprintf (temp, "(setq %s_%s %s)", funcionActual, $1.code, $3.code) ; 
                                           $$.code = gen_code (temp) ; }
                    ;

/*printf------------------------------------------------------------------------------------------------------------------------------------*/

resto_printf:	expresion  r_resto_printf	{ if ($2.code != NULL) {sprintf (temp ,"(princ %s) %s", $1.code, $2.code) ; $$.code = gen_code (temp) ;} 
							else {sprintf (temp ,"(princ %s)", $1.code) ; $$.code = gen_code (temp) ;};}
						
		| STRING  r_resto_printf		{ if ($2.code != NULL) { 
								if (strcmp($1.code, " ") == 0){
									sprintf (temp ,"(princ \" \")%s", $2.code) ;
								} else {
									sprintf (temp ,"(princ \"%s\") %s", $1.code, $2.code) ;	
								}
							} else {
								if (strcmp($1.code, " ") == 0) {
									sprintf (temp ,"(princ \" \")") ;
								} else {
									sprintf (temp ,"(princ \"%s\")", $1.code);
								}
							} 
							$$.code = gen_code (temp);}
					
;
r_resto_printf: ',' resto_printf	{$$= $2;}
		|		{$$.code = NULL;}
;



/*-------------------------------------------------------------------------------------------------------------DECLARACIONES GLOBALES------------------------------------------------------------------------------------------------------------------------*/
// Nueva regla para manejar declaraciones de variables globales
declaracion_global:	declaracion 	{$$ = $1;}
			|		{$$.code = NULL;}
;
declaracion:  INTEGER lista_declaraciones  ';'{ printf ("\n%s", $2.code);} r_declaracion 	
		;
/*---------------empezar nueva linea de declaraciones, que tiene que empezar por int-----------------*/
r_declaracion: declaracion	{$$ = $1;}
		|		{$$.code = NULL;}
;


/*---------------declarar varias varias variables en la misma linea----------*/

lista_declaraciones: 
              declaracion_var  r_lista_declaraciones         { if ($2.code != NULL) {sprintf (temp, "%s %s", $1.code, $2.code) ;  $$.code = gen_code (temp);} else {
              							sprintf (temp, "%s", $1.code) ;  $$.code = gen_code (temp); } ; }
            ;	
r_lista_declaraciones: 					{$$.code = NULL;}
        	| ',' lista_declaraciones                 { $$ = $2 ; }


		;
declaracion_var: IDENTIF                      { sprintf(temp, "(setq %s 0)", $1.code); $$.code = gen_code(temp); nuevaVariableGlobal($1.code);}
            | IDENTIF '=' NUMBER           { sprintf (temp, "(setq %s %d)", $1.code, $3.value) ; $$.code = gen_code (temp) ; nuevaVariableGlobal($1.code); }
                                           
           | IDENTIF '[' NUMBER ']'         { sprintf (temp, "(setq %s (make-array %d))", $1.code, $3.value) ; $$.code = gen_code (temp) ; nuevaVariableGlobal($1.code);}  
            ;


expresion:      termino                   {$$=$1;}
            |   expresion '+' expresion  {sprintf (temp, "(+ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '-' expresion  { sprintf (temp, "(- %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '*' expresion  { sprintf (temp, "(* %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
            |   expresion '/' expresion  { sprintf (temp, "(/ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }
       		 |   expresion AND expresion  			{ sprintf (temp, "(and %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
      		 |   expresion OR expresion  			{ sprintf (temp, "(or %s %s)", $1.code, $3.code) ;
       	                                   		 $$.code = gen_code (temp) ; }
      		  |   expresion '!' expresion 		 { sprintf (temp, "(not %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
                |   expresion NOT_EQ expresion  		{ sprintf (temp, "(/= %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
                |   expresion EQ expresion 		 { sprintf (temp, "(= %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
     		   |   expresion '<' expresion 		 { sprintf (temp, "(< %s %s)", $1.code, $3.code) ;
                                         		  $$.code = gen_code (temp) ; }
                                           
                |   expresion MENOR_EQ expresion  		{ sprintf (temp, "(<= %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
                |   expresion '>' expresion  		{ sprintf (temp, "(> %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
                |   expresion MAYOR_EQ expresion  		{ sprintf (temp, "(>= %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
                |   expresion '%' expresion 		 { sprintf (temp, "(mod %s %s)", $1.code, $3.code) ;
                                          		 $$.code = gen_code (temp) ; }
                           
            ;

termino:        operando                           { $$ = $1 ; }                          
            |   '+' operando %prec UNARY_SIGN      { $$ = $1 ; }
            |   '-' operando %prec UNARY_SIGN      { sprintf (temp, "(- %s)", $2.code) ;
                                                     $$.code = gen_code (temp) ; }    
            ;

operando:       IDENTIF                  {if (esParametro($1.code) == 1 || (esGlobal($1.code) == 1) ){
						sprintf (temp, "%s", $1.code);  $$.code = gen_code (temp) ;
					} else {
						if (esLocal($1.code) == 1) {
							sprintf (temp, "%s_%s", funcionActual, $1.code); $$.code = gen_code (temp) ;
						} else {
							sprintf (temp, "%s", $1.code);  $$.code = gen_code (temp) ;
							nuevaVarLocal($1.code);
						} 
					};}
					
            |   NUMBER                   { sprintf (temp, "%d", $1.value) ;
                                           $$.code = gen_code (temp) ; }
            |   '(' expresion ')'        { $$ = $2 ; }
            
            
            |	IDENTIF '[' expresion ']' {sprintf (temp, "(aref %s %s)", $1.code, $3.code) ;  $$.code = gen_code (temp) ; }
           
           |	IDENTIF'(' lista_parametros_llamada ')'  {if ($3.code != NULL){sprintf (temp, "(%s %s)", $1.code, $3.code) ; $$.code = gen_code (temp);}else{sprintf (temp, "(%s)", $1.code) ; $$.code = gen_code (temp);};}
           
           ;


%%                            // SECCION 4    Codigo en C

int n_line = 1 ;

int yyerror (mensaje)
char *mensaje ;
{
    fprintf (stderr, "%s en la linea %d\n", mensaje, n_line) ;
    printf ( "\n") ;	// bye
}

char *int_to_string (int n)
{
    sprintf (temp, "%d", n) ;
    return gen_code (temp) ;
}

char *char_to_string (char c)
{
    sprintf (temp, "%c", c) ;
    return gen_code (temp) ;
}

char *my_malloc (int nbytes)       // reserva n bytes de memoria dinamica
{
    char *p ;
    static long int nb = 0;        // sirven para contabilizar la memoria
    static int nv = 0 ;            // solicitada en total

    p = malloc (nbytes) ;
    if (p == NULL) {
        fprintf (stderr, "No queda memoria para %d bytes mas\n", nbytes) ;
        fprintf (stderr, "Reservados %ld bytes en %d llamadas\n", nb, nv) ;
        exit (0) ;
    }
    nb += (long) nbytes ;
    nv++ ;

    return p ;
}


/***************************************************************************/
/********************** Seccion de Palabras Reservadas *********************/
/***************************************************************************/

typedef struct s_keyword { // para las palabras reservadas de C
    char *name ;
    int token ;
} t_keyword ;

t_keyword keywords [] = { // define las palabras reservadas y los
    "main",        MAIN,           // y los token asociados
    "int",         INTEGER,
    "puts",	   PUTS,	
    "printf",		PRINTF,
    "while",		WHILE,
    "&&",		AND,
    "||",		OR,
    "<=",		MENOR_EQ,
    ">=",		MAYOR_EQ,
	"!=",		NOT_EQ,
	"==",		EQ,
	"if",		IF,
	"else",		ELSE,
	"for",		FOR,
	"return",	RETURN,
    NULL,          0               // para marcar el fin de la tabla
} ;

t_keyword *search_keyword (char *symbol_name)
{                                  // Busca n_s en la tabla de pal. res.
                                   // y devuelve puntero a registro (simbolo)
    int i ;
    t_keyword *sim ;

    i = 0 ;
    sim = keywords ;
    while (sim [i].name != NULL) {
	    if (strcmp (sim [i].name, symbol_name) == 0) {
		                             // strcmp(a, b) devuelve == 0 si a==b
            return &(sim [i]) ;
        }
        i++ ;
    }

    return NULL ;
}

//////
//VARIABLES LOCALES
//////
int esLocal(char *nombre) {
    for (int i = 0; i < nLocales; i++) {
        if (strcmp(tablaLocales[i].nombre, nombre) == 0)
            return 1;
    }
    return 0;
}
void nuevaVarLocal(char *nombre) {
    strcpy(tablaLocales[nLocales++].nombre, nombre);
}
////////////////
//////
//PARAMETROS LOCALES
//////
int esParametro(char *nombre) {
    for (int i = 0; i < nParametros; i++) {
        if (strcmp(tablaParametros[i].nombre, nombre) == 0)
            return 1;
    }
    return 0;
}
void nuevoParametroLocal(char *nombre) {
    strcpy(tablaParametros[nParametros++].nombre, nombre);
}
////////////////
//////
//PARAMETROS LOCALES
//////
int esGlobal(char *nombre) {
    for (int i = 0; i < nGlobales; i++) {
        if (strcmp(tablaGlobales[i].nombre, nombre) == 0)
            return 1;
    }
    return 0;
}
void nuevaVariableGlobal(char *nombre) {
    strcpy(tablaGlobales[nGlobales++].nombre, nombre);
}
 
/***************************************************************************/
/******************* Seccion del Analizador Lexicografico ******************/
/***************************************************************************/

char *gen_code (char *name)     // copia el argumento a un
{                                      // string en memoria dinamica
    char *p ;
    int l ;
	
    l = strlen (name)+1 ;
    p = (char *) my_malloc (l) ;
    strcpy (p, name) ;
	
    return p ;
}


int yylex ()
{
// NO MODIFICAR ESTA FUNCION SIN PERMISO
    int i ;
    unsigned char c ;
    unsigned char cc ;
    char ops_expandibles [] = "!<=|>%&/+-*" ;
    char temp_str [256] ;
    t_keyword *symbol ;

    do {
        c = getchar () ;

        if (c == '#') {	// Ignora las lineas que empiezan por #  (#define, #include)
            do {		//	OJO que puede funcionar mal si una linea contiene #
                c = getchar () ;
            } while (c != '\n') ;
        }

        if (c == '/') {	// Si la linea contiene un / puede ser inicio de comentario
            cc = getchar () ;
            if (cc != '/') {   // Si el siguiente char es /  es un comentario, pero...
                ungetc (cc, stdin) ;
            } else {
                c = getchar () ;	// ...
                if (c == '@') {	// Si es la secuencia //@  ==> transcribimos la linea
                    do {		// Se trata de codigo inline (Codigo embebido en C)
                        c = getchar () ;
                        putchar (c) ;
                    } while (c != '\n') ;
                } else {		// ==> comentario, ignorar la linea
                    while (c != '\n') {
                        c = getchar () ;
                    }
                }
            }
        } else if (c == '\\') c = getchar () ;
		
        if (c == '\n')
            n_line++ ;

    } while (c == ' ' || c == '\n' || c == 10 || c == 13 || c == '\t') ;

    if (c == '\"') {
        i = 0 ;
        do {
            c = getchar () ;
            temp_str [i++] = c ;
        } while (c != '\"' && i < 255) ;
        if (i == 256) {
            printf ("AVISO: string con mas de 255 caracteres en linea %d\n", n_line) ;
        }		 	// habria que leer hasta el siguiente " , pero, y si falta?
        temp_str [--i] = '\0' ;
        yylval.code = gen_code (temp_str) ;
        return (STRING) ;
    }

    if (c == '.' || (c >= '0' && c <= '9')) {
        ungetc (c, stdin) ;
        scanf ("%d", &yylval.value) ;
//         printf ("\nDEV: NUMBER %d\n", yylval.value) ;        // PARA DEPURAR
        return NUMBER ;
    }

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
        i = 0 ;
        while (((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '_') && i < 255) {
            temp_str [i++] = tolower (c) ;
            c = getchar () ;
        }
        temp_str [i] = '\0' ;
        ungetc (c, stdin) ;

        yylval.code = gen_code (temp_str) ;
        symbol = search_keyword (yylval.code) ;
        if (symbol == NULL) {    // no es palabra reservada -> identificador antes vrariabre
//               printf ("\nDEV: IDENTIF %s\n", yylval.code) ;    // PARA DEPURAR
            return (IDENTIF) ;
        } else {
//               printf ("\nDEV: OTRO %s\n", yylval.code) ;       // PARA DEPURAR
            return (symbol->token) ;
        }
    }

    if (strchr (ops_expandibles, c) != NULL) { // busca c en ops_expandibles
        cc = getchar () ;
        sprintf (temp_str, "%c%c", (char) c, (char) cc) ;
        symbol = search_keyword (temp_str) ;
        if (symbol == NULL) {
            ungetc (cc, stdin) ;
            yylval.code = NULL ;
            return (c) ;
        } else {
            yylval.code = gen_code (temp_str) ; // aunque no se use
            return (symbol->token) ;
        }
    }

//    printf ("\nDEV: LITERAL %d #%c#\n", (int) c, c) ;      // PARA DEPURAR
    if (c == EOF || c == 255 || c == 26) {
//         printf ("tEOF ") ;                                // PARA DEPURAR
        return (0) ;
    }

    return c ;
}


int main ()
{
    yyparse () ;
}
