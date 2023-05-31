#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "lexer.h"

#define MAX_CODE_LENGTH 500
#define MAX_SYMBOL_TABLE_SIZE 10000

#define ERROR_SEMICOLON 17
#define ERROR_NOT_CONST 30
#define ERROR_NOT_VAR   31
#define ERROR_NOT_PROC  32

typedef enum
{
    hlt,
    lit,
    opr,
    lod,
    sto,
    cal,
    inc,
    jmp,
    jpc,
    sio
} opcode;

typedef enum
{
    RET = 0,
    NEG,
    ADD,
    SUB,
    MUL,
    DIV,
    ODD,
    MOD,
    EQL,
    NEQ,
    LSS,
    LEQ,
    GTR,
    GEQ
} oprcode;


typedef struct{
  char type[3];
  char val[12];
} token;
  
typedef struct{
  int op;
  int l;
  int m;
}code_struct;
  
typedef struct{
  int kind;			// const = 1, var = 2, proc = 3
  char name[12];	// name up to 12 chars
  int val;			// number (ascii val)
  int level;		// L level 
  int addr;			// M address
} symbol;

// All our global Vars
symbol symbol_table[MAX_SYMBOL_TABLE_SIZE];
int symIndex= 0;
int symAddr = 3;
int num_symbols = 0;

// Code array
code_struct code[MAX_CODE_LENGTH] ;
int cx = 0;
int curr_lvl = 0;

int tokenArray[MAX_CODE_LENGTH];
Token* current_token;
int tokenIndex = 0;
int tokenPtr = 0;

FILE* Parserin;

void get_next_t(); // turned this to a void, was int
void add_symbol(int k, char *name, int num, int level, int modifier);
void err(int n);

//Generates the PL/0 code line w/ the input fields.
void emit(int op, int l, int m);

//procs for the grammar
void program();
void statement();
void block();
void condition();
int rel_op();
void term();
void factor();
void expression();
char* tokenToString(int token);
int find_symbol( char* ident );

// Parse the input and generate the PM/0 code it produces. If there is a lex or parser
// error, stop the program.
void Parser(char* inputFile, char* outputFile){
	
	int i, temp;
	
	initalizeLexer(inputFile);
	
	FILE* output = fopen(outputFile, "w");
	
	// Start Parsing
	program();
	
	// Print the PM/0 code
	for( i = 0; i < cx; i++ ) {
		fprintf(output, "%d %d %d\n", code[i].op, code[i].l, code[i].m );
	}
	
	//fclose(input);
	fclose(output);

}

// Austin
// Get token, call block(), end on period
void program(){
	
	get_next_t();
	block();
	if( current_token->type != periodsym )
		err(9);
	
	emit( sio, 0, 2 );
}

// Austin
// Check for constant declaration, variable declaration, procedure, then do statement
void block(){
	
	curr_lvl++;
	int jumpAddress = cx;
	int numVars = 0, numConsts = 0, numProcs = 0;
	char ident[100];
	
	// JMP address is changed later
	emit( jmp, 0, 0 );
	
	// Constant declaration
	// EBSF: "const" <id> "=" <num> { "," <id> "=" <num> } ";"
	if( current_token->type == constsym ) {
		
		// Loop until no more constants declared (so until semicolon)
		// Must see identifier, equals sign, then a number
		while (1) {
			int val;
			numConsts++;
			
			get_next_t();
			if( current_token->type != identsym ) err(4);
			strcpy(ident,current_token->string);
			
			get_next_t();
			if( current_token->type != eqsym ) err(3);
			
			get_next_t();
			if( current_token->type != numbersym ) err(2);
			
			val = current_token->type;
			
			add_symbol(1, ident, val, 0, 0);
			
			get_next_t();
			
			if( current_token->type != commasym ) break;
		}
		
		if( current_token->type != semicolonsym ) err(5);
		get_next_t();
	}
	
	// Variable declaration
	// EBSF: "var" <id> { "," <id> } ";"
	if( current_token->type == varsym ) {
	
		// Loop if commasym
		while(1) {
			get_next_t();
			if( current_token->type != identsym ) err(4);
			
			add_symbol(2, current_token->string, 0, curr_lvl, numVars + 4);
			get_next_t();
			numVars++;
			
			if ( current_token->type != commasym ) break;
		}
		
		//emit( INC, 0, 4 + num_symbols );
		
		
		if( current_token->type != semicolonsym ) err(5);
		get_next_t();
	}
	
	// Procedure
	// EBSF: "proc" { <id> ";" <block> ";" }
	while( current_token->type == procsym ) {

		numProcs++;
		get_next_t();
		
		if( current_token->type != identsym ) err(4);
		strcpy(ident,current_token->string);
		
		add_symbol( 3, ident, 0, curr_lvl, cx );
		
		get_next_t();
		if( current_token->type != semicolonsym ) err(5);
		get_next_t();
		
		block();
		
		if( current_token->type != semicolonsym ) err(5);
		get_next_t();
	}
	
	// Go back and fix
	code[jumpAddress].m = cx;
	
	// Space for variables
	emit( inc, 0, numVars + 4 );
	statement();
	
	symIndex -= numConsts + numVars + numProcs;
	if (jumpAddress!=0) {
		emit( opr, 0, RET );
	}
	
	curr_lvl--;
}

// jerasimos
void statement() {
	
	int loc, tempAddr1, tempAddr2, tempcx;
	
	// Assignment
	if(current_token->type == identsym){
		
		loc = find_symbol(current_token->string);
		get_next_t();
		
		if( current_token->type != becomessym ) err(19);
		
		get_next_t();
		expression();
		
        if ( symbol_table[loc].kind != 2 ) {
        	err(ERROR_NOT_VAR); // Not var
    	}
		
		emit( sto, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr);
		
		return;
	
	// Call procedure
	} else if( current_token->type == callsym ) {
		
		get_next_t();
		if( current_token->type != identsym ) err(14);
		loc = find_symbol(current_token->string);
		
		get_next_t();
		
		
        if ( symbol_table[loc].kind != 3 ) {
        	err(ERROR_NOT_PROC); // Not proc
    	}
		
		emit( cal, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr );
		
		return;
	
	//Begin statement
	} else if(current_token->type == beginsym){
		
		while(1) {
			
			get_next_t();
			statement();
			
			if (current_token->type == endsym)
				break;
			
			if (current_token->type != semicolonsym) err(91);
			
		}
		
		get_next_t();
		
		return;
	
	// If
	} else if(current_token->type == ifsym ){
		
		get_next_t();
		condition();
		
		if(current_token->type != thensym) err(16);
		get_next_t();
		
		tempAddr1 = cx;
		emit( jpc, 0, 0);
		statement();
		
		code[tempAddr1].m = cx;
		
		
		return;
	
	// While
	} else if(current_token->type == whilesym) {
		
		tempAddr1 = cx;
		get_next_t();
		condition();
		
		tempAddr2 = cx;
		
		emit( jpc, 0, 0);
		
		if( current_token->type != dosym ) err(18);
		get_next_t();
		statement();
		
		if (current_token->type != semicolonsym) err(ERROR_SEMICOLON);
		
		emit( jmp, 0, tempAddr1);
		code[tempAddr2].m = cx;
		
		return;
	
	// Read
	} else if(current_token->type == readsym ){
		
		get_next_t();
		if(	current_token->type != identsym) err(26);
		
		loc = find_symbol(current_token->string);
        if ( symbol_table[loc].kind != 2 ) {
        	err(ERROR_NOT_VAR); // Not var
    	}
		
		emit( sio, 0, 1);
		emit( sto, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr);
		
		get_next_t();
		
		return;
	
	// Write
	} else if(current_token->type == writesym){
		
		get_next_t();
		if(	current_token->type != identsym) err(26);
		
		loc = find_symbol(current_token->string);
        if ( symbol_table[loc].kind != 2 ) {
        	err(ERROR_NOT_VAR); // Not var
    	}
		
		emit( lod, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr);
		emit( sio, 0, 0);
		
		get_next_t();
		
		return;
	}
	
}

// Condition
void condition(){
	
	if(	current_token->type == oddsym){
		
		get_next_t();
		expression();
		
		emit( opr, 0, ODD );
		
		return;
		
	}else{
		
		expression();
		
		int opCode = rel_op();
		get_next_t();
		
		expression();
		emit( opr, 0, opCode);
		
		return;
	}

}

// Check that current_token is a relation symbol. If not, raise an error.
int rel_op(){
	
	switch(current_token->type){

	case eqsym : // '='
		return EQL;
	
	case neqsym : //'<>'
		return NEG;
	
	case lessym : // '<'
		return LSS;
	
	case leqsym : // '<='
		return LEQ;

	case gtrsym : // '>'
		return GTR;
	
	case geqsym : // '>='
		return GEQ;

	default :
		err(20);
		return 0;
	}
}

// Term
void term() {
	
	int mulop;
	factor();
	
	while(current_token->type == multsym || current_token->type == slashsym) {
		
		mulop = current_token->type;
		get_next_t();
		factor();
		
		if(mulop == multsym)
			emit(opr, 0, MUL);
		else
			emit(opr, 0, DIV);
	}
	
}

// Factor
void factor() {
	
	int i, loc;
	
	if(current_token->type == identsym){
		loc = find_symbol(current_token->string);
		
		get_next_t();
		
		// Const
        if( symbol_table[loc].kind == 1 ) {
			emit( lod, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr);
		}
		
		// Var
		else if( symbol_table[loc].kind == 2 ) {
			emit( lod, curr_lvl - symbol_table[loc].level, symbol_table[loc].addr);
		}
		
		else {
			err(29);
		}
		
		//get_next_t();
	}
	
	// Number
	else if(current_token->type == numbersym) {
		int val = atoi(current_token->string);
		get_next_t();
		emit(lit, 0, val);
		//get_next_t();
	}
	
	
	// Or if we find a left paren, evaluate it as a new expression
	else if(current_token->type == lparentsym) {
		
		get_next_t();
		expression();
		
		if(current_token->type != rparentsym) err(13);
		
		get_next_t();	
	}
	
	// Otherwise we have an error
	else{
		err(28);
	}
}

// Expression
// jerasimos
// Added an emit here - Gabriela  
void expression(){
	int plusop;
	if(current_token->type == plussym || current_token->type == minussym)
		get_next_t();
	term();
	while(current_token->type == plussym || current_token->type == minussym){
		plusop = current_token->type;
		get_next_t();
		term();
		if(plusop == plussym)  
			emit(opr, 0, ADD); 
		else
			emit(opr, 0, SUB);
	}
}

// Gets the next token in the tokenArray and advances the pointer
void get_next_t() {
	current_token = lex();
	//printf("%s \n",current_token->string);
}

// Gabriela, Terek, Austin
void err(int n){
	switch(n) {
		
		case 1:
			printf("Error: Use = instead of :=.\n");
			exit(0);
			
		case 2:
			printf("Error: = must be followed by a number.\n");
			exit(0);
			
		case 3: 
			printf("Error: Identifier should be followed by =");
			exit(0);
			
		case 4:
			printf("Error: const, var, procedure, read, write must be followed by identifier.\n");
			exit(0);
			
	    case 5:
			printf("Error: Semicolon or comma missing.\n");
			exit(0);
			
		case 6:
			printf("Error: Incorrect symbol after procedure declaration.");
			exit(0);
			
	    case 7:
			printf("Error: Statement expected");
			exit(0);
			
	    case 8:
			printf("Error: Incorrect symbol after statement part in block.\n");
			exit(0);
			
		case 9:
			printf("Error: Period expected.\n");
			exit(0);
			
	    case 10:
			printf("Error: Semicolon between statements missing.\n");
			exit(0);
			
		case 11:
			printf("Error: Undeclared identifier.\n");
			exit(0);
			
		case 12:
			printf("Error: Assignment to constant or procedure is not allowed. \n");
			exit(0);
			
	    case 13:
	        printf("Error: Assignment operater expected. \n");
			exit(0);
			
		case 14:
		    printf("Error: call must be followed by an identifier. \n");
			exit(0);
		
		case 15:
			printf("Error: Call of a constant or variable is meaningless.\n");
			exit(0);
			
		case 16:
			printf("Error: then expected \n");
			exit(0);
		
		case ERROR_SEMICOLON:
			printf("Error: Semicolon or } expected.\n");
			exit(0);
		
		case 18:
			printf("Error: do expected.\n");
			exit(0);
			
		case 19:
			printf("Error: Incorrect symbol following statement\n");
			exit(0);
		
		case 20:
			printf("Error: Relational operator expected.\n");
			exit(0);
		
		case 21:
			printf("Error: Relational operator expected.\n");
			exit(0);
		
		case 22:
			printf("Error: Right parenthesis missing. \n");
			exit(0);
		
		case 23:
			printf("Error: The preceding factor cannot begin with this symbol.\n");
			exit(0);
		
		case 24:
			printf("Error: An expression cannot begin with this symbol.\n");
			exit(0);
		
		case 25:
			printf("Error: This number is too large.\n");
			exit(0);
			
		case 26:
			printf("Error: read, write must be followed by identifier.\n");
			exit(0);
			
		case 27:
			printf("Error: end expected.\n");
			exit(0);
			
		case 28:
			printf("Error: Identifier, number or expression expected.\n");
			exit(0);
			
		case 29:
			printf("Error: Constant or variable expected.\n");
			exit(0);
			
		case ERROR_NOT_CONST:
			printf("Error: Not a constant.\n");
			exit(0);
			
		case ERROR_NOT_VAR:
			printf("Error: Not a variable.\n");
			exit(0);
			
		case ERROR_NOT_PROC:
			printf("Error: Not a procedure.\n");
			exit(0);
		
		default:
			printf("Unknown error %d.\n",n);
			exit(0);
	}
}


void emit(int op, int l, int m){
	
	// send an error saying too large
	if(cx > MAX_CODE_LENGTH) err(30);
	else
	{
		code[cx].op = op; 	// opcode
		code[cx].l = l;		// lexicographical lvl 
		code[cx].m = m;		// modifier
		cx++;
	}
	
}

// Add constants or variables to the symbol table
// If the symbol already exists, raise error
// k = 1 const; k = 2 variable; k = 3 procedure.
void add_symbol(int k, char *name, int num, int level, int modifier){
	
	// Do not use index zero
	symIndex++;
	
	// Check if already in symbol table
	int i;
	for ( i=0; i < num_symbols; i++) {
		symbol s = symbol_table[i];
		
		// Check name and level
		if (strcmp(s.name,name) == 0) {
			if(s.level == curr_lvl) {
				printf("Error: Symbol name already in use\n");
				exit(0);
			}
		}
	}
	
	symbol *s = malloc(sizeof(symbol));
	s->kind = k;
	strcpy(s->name,name);
	s->val = num;
	s->level = level;
	s->addr = modifier;
	
	// Added to table
	symbol_table[num_symbols] = *s;
	num_symbols++;
	
}

// Find symbol if present
int find_symbol( char* ident ) {
	
	int i;
	for( i = 0; i < num_symbols; i++ ) {
		if( strcmp( symbol_table[i].name, ident ) == 0 )
			return i;
	}
	
	printf("Error: Symbol not found\n");
	exit(0);
}


// we can use a tostring method to make the tokens into the names needed for parser func
// -Tarek
char* tokenToString(int token){
    switch (token)
    {
        case (int) plussym:
            return "plussym";
            
        case (int) minussym:
            return "minussym";
            
        case (int) numbersym:
            return "numbersym"; 
            
        case (int) multsym:
            return "multsym";
            
        case (int) slashsym:
            return "slashsym";
            
        case (int) lessym:
            return "lesssym";
            
        case(int) eqsym:
            return "eqsym";
            
        case (int) gtrsym:
            return "gtrsym";
            
        case(int) geqsym:
            return "geqsym";
            
        case(int) leqsym:
            return "leqsym";
            
        case(int) becomessym:
            return "becomessym";
            
        case(int) neqsym:
            return "neqsym";
            
        case (int) lparentsym:
            return "lparentsym";
            
        case (int) rparentsym:
            return "rparentsym";
            
        case (int) commasym:
            return "commasym";
            
        case (int) semicolonsym:
            return "semicolonsym";
            
        case (int) periodsym:
            return "periodsym";
            
        case (int) varsym:
            return "varsym";
            
        case (int) beginsym:
            return "beginsym";
            
        case (int) identsym:
            return "identsym";
            
        case (int) endsym:
            return "endsym";
            
        case (int) ifsym:
            return "ifsym";
            
        case (int) thensym:
            return "thensym";
            
        case (int) whilesym:
            return "whilesym";
            
        case (int) dosym:
            return "dosym";
            
        case (int) callsym:
            return "callsym";
            
        case (int) constsym:
            return "constsym";
            
        case (int) elsesym:
            return "elsesym";
            
        case (int) procsym:
            return "procsym";
            
        case (int) readsym:
            return "readsym";
            
        case (int) writesym:
            return "writesym";
            
        case (int) nulsym:
            return "NULL";
            
        default:
            return "ERR";
    }
}

// Main
int main( int argc, char** argv ) {
	
	argc--; *argv++; //dont need first argument (executable name)
	if (argc!=2) {
		printf("Error: Enter <input> <output> as arguments\n");
	} else {
		char* inputFile = *argv++;
		char* outputFile = *argv++;
	
		Parser(inputFile, outputFile);
	}

}
