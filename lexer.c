#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "lexer.h"

char *tokens;
int t_pointer;
char *input;
int pointer;
int *input_guide;
int terminate;
int printSource;
FILE* file;

Token* newToken(char* sbuf, token_type t);
int startComment();
void tokenSym( token_type t );
void inputChar( char c );
int wordPrint( int p, int *ill );
void inputGuider( int i );
void condPrintf( char c );

Token* newToken(char* sbuf, token_type t) {
	Token *token = malloc(sizeof(Token));
	token->type = t;
	strcpy(token->string,sbuf);
	return token;
}

// Lex the input and output it before comments are removed
Token* lex() {
	
	char c, d;
	int saw_comment;
	
	// Ignore space, tab, newline
	while ((c=getc(file)) == ' ' || c == '\t' || c == '\n' || c == '\r') {
		
		inputGuider( 0 );
		if( c == ' ' ) {
			//printf(" ");
			condPrintf(c);
			inputChar(c);
		}
		else if( c == '\t' ) {
			//printf("\t");
			condPrintf(c);
			inputChar(c);
		}
		else if( c == '\n' || c == '\r') {
			//printf("\n");
			condPrintf(c);
			inputChar(c);
		}
	}
	
	// Check for comment start
	saw_comment = 0;
	if( c == '/' ) {
		if( (d = getc(file)) == '*' ) {
			saw_comment = 1;
			if( !startComment() ) {
				return newToken("/",nulsym); // EOF while in comment
			}
			c = getc(file);
		} else
			ungetc(d, file);
	}
	
	// Check for white space again after comment 
	if( saw_comment ) {
		ungetc(c, file);
		while ( (c=getc(file)) == ' ' || c == '\t' || c == '\n' || c == '\r') {
			
			inputGuider( 0 );
			if( c == ' ' ) {
				//printf(" ");
				condPrintf(c);
				inputChar(c);
			}
			else if( c == '\t' ) {
				//printf("\t");
				condPrintf(c);
				inputChar(c);
			}
			else if( c == '\n' || c == '\r') {
				//printf("\n");
				condPrintf(c);
				inputChar('\n');
			}
		}
	}
	
	// End of the file, stop lexxing
	if (c == EOF) return newToken("",nulsym);

	// Identifier or reserved word
	if (isalpha(c)) {
		
		char sbuf[100], *p = sbuf;
		
		do {
			
			//printf("%c", c);
			condPrintf(c);
			inputGuider( 3 );
			inputChar(c);
			*p++ = c;

		} while ((c=getc(file)) != EOF && isalnum(c));

		ungetc(c, file);

		*p = '\0';

		// check if any of these identifiers were actually meant
		// to be reserved words instead

		if(strcmp(sbuf, "begin") == 0) return newToken(sbuf,beginsym);

		if(strcmp(sbuf, "null") == 0) return newToken(sbuf,nulsym);

		if(strcmp(sbuf, "call") == 0) return newToken(sbuf,callsym);

		if(strcmp(sbuf, "const") == 0) return newToken(sbuf,constsym);

		if(strcmp(sbuf, "do") == 0) return newToken(sbuf,dosym);

		if(strcmp(sbuf, "else") == 0) return newToken(sbuf,elsesym);

		if(strcmp(sbuf, "end") == 0) return newToken(sbuf,endsym);

		if(strcmp(sbuf, "if") == 0) return newToken(sbuf,ifsym);

		if(strcmp(sbuf, "odd") == 0) return newToken(sbuf,oddsym);

		if(strcmp(sbuf, "procedure") == 0) return newToken(sbuf,procsym);

		if(strcmp(sbuf, "read") == 0) return newToken(sbuf,readsym);

		if(strcmp(sbuf, "then") == 0) return newToken(sbuf,thensym);

		if(strcmp(sbuf, "var") == 0) return newToken(sbuf,varsym);

		if(strcmp(sbuf, "while") == 0) return newToken(sbuf,whilesym);

		if(strcmp(sbuf, "write") == 0) return newToken(sbuf,writesym);
		
		return newToken(sbuf,identsym);
	}
	
	// Digits
	if (isdigit(c)){
  		
		char sbuf[100], *p = sbuf;
		int length = 0;

		do {
			
			length++;
			// Number too large
			if ( length == 6 ) {
				printf("\nNumber too large\n");
				terminate = 1;
				return 0;
				
			// Number is sufficient size
			} else {
				condPrintf(c);
				inputGuider( 3 );
				inputChar(c);
				*p++ = c;
			}

		} while ((c=getc(file)) != EOF && isdigit(c));

		ungetc(c, file);

		*p = '\0';
		
		return newToken(sbuf,numbersym);  	
	}
	
	// Reserved symbol or illegal token
	condPrintf(c);
	switch (c) {
		
		case '+' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("+",plussym);
			
		case '*' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("*",multsym);
			
		case '(' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("(",lparentsym);
			
		case ')' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken(")",rparentsym);
			
		case '/' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("/",slashsym);
			
		case '-' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("-",minussym);
			
		case ',' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken(",",commasym);
			
		case ';' :
		 inputGuider( 1 );
		 inputChar(c);
	
		 return newToken(";",semicolonsym);
			
		case '.' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken(".",periodsym);
			
		case '<' :
		 inputGuider( 2 );
		 inputChar(c);
		 
		 if((c=getc(file)) == '=') {
			 condPrintf(c);
			 inputGuider( 2 );
			 inputChar(c);
			 
			 return newToken("<=",leqsym);
		 }
			
		 if((c=getc(file)) == '>') {
			 condPrintf(c);
			 inputGuider( 2 );
			 inputChar(c);
			 
			 return newToken("<>",neqsym);
		 }
			
		 //Undo 2_ in inputGuide to 10
		 pointer -= 2; inputGuider( 1 );
		 pointer += 1; inputGuider( 0 );
		 
		 ungetc(c, file);
		 return newToken("<",lessym);     	    	
			
		case '>' :
		 inputGuider( 2 );
		 inputChar(c);
		 
		 if((c=getc(file)) == '=') {
			 condPrintf(c);
			 inputGuider( 2 );
			 inputChar(c);
			 
			 return newToken(">=",geqsym);
		 }
		 //Undo 22 in inputGuide to 10
		 pointer -= 2; inputGuider( 1 );
		 pointer += 1; inputGuider( 0 );
		 
		 ungetc(c, file);
		 return newToken(">",gtrsym);
			
		// If '!' is a recognized symbol in PL/0, uncomment this and rewrite it.
		/*case '!' :
		 inputGuider( 2 );
		 inputChar(c);
		 
		 // Edited: "<>" is the symbol for not equals
		 if((c=getc(file)) == '=')  {
			 condPrintf(c);
			 inputGuider( 2 );
			 inputChar(c);
			 
			 return newToken("!=",neqsym);
		 }
		 //Undo 22 in inputGuide to 40
		 pointer -= 2; inputGuider( 4 );
		 pointer += 1; inputGuider( 0 );
		 
		 ungetc(c, file);
		 break;
		*/
			
		case ':' :
		 inputGuider( 2 );
		 inputChar(c);
		 
		 if((c=getc(file)) == '=') {
			 condPrintf(c);
			 inputGuider( 2 );
			 inputChar(c);
			 
			 return newToken(":=",becomessym);
		 }
		 //Undo 22 in inputGuide to 40
		 pointer -= 2; inputGuider( 4 );
		 pointer += 1; inputGuider( 0 );
		 
		 ungetc(c, file);
		 break; 		  
				
		case '=' :
		 inputGuider( 1 );
		 inputChar(c);
		 
		 return newToken("=",eqsym);
		 
		 
		 
		default  :
			inputGuider( 4 );
			inputChar(c);
	}
}


// Add white space to the input string to maintain spacing
// Exit if we see "*/"
int startComment(){
	
	char c, d;
	
	// To make up for the start of the comment /*
	condPrintf('/');
	inputChar(' ');
	condPrintf('*');
	inputChar(' ');
	
	// Continue unless EOF or if we find */
	while( (c=getc(file)) != EOF ) {
		
		//Exit comment if we find "*/"
		if( c  == '*' ) {
			if( (d=getc(file)) == '/' ) {
				//printf("*");
				condPrintf('*');
				inputChar(' ');
				//printf("/");
				condPrintf('/');
				inputChar(' ');
				return 1;
			} else {
				ungetc(d, file);
			}
		}
		
		// Tab is different since it takes multiple spaces
		if( c == '\t' ) {
			//printf("\t"); 
			condPrintf(c);
			inputChar('\t');
		}
		
		// New line
		else if( c == '\n' || c == '\r') {
			//printf("\n");
			condPrintf(c);
		}
		
		// Otherwise, we just copy a space to input and continue
		else {
			//printf("%c", c); 
			condPrintf(c);
			inputChar(' ');
		}
		
	}
	
	// Quit lex since EOF
	return 0;
}


// Cleans up main
void tokenSym( token_type t ) {
	tokens[t_pointer] = t;
	t_pointer++;
}


// Makes lex look less cluttered
void inputChar( char c ) {
	
	input[pointer] = c;
	pointer++;
	
}


// Prints tokens
int wordPrint(/*char c,*/ int p, int *ill) {
	
	int j = p;
	int is_three = 0;
	
	// Skip white space
	while( input_guide[j] == 0 || input[j] == '\r' )
		j++;
	
	// Print a single char operator
	if( input_guide[j] == 1 ) {
		printf("%c\t\t", input[j] );
		return j + 1;
	}
	
	// Print a two-char operator
	else if( input_guide[j] == 2 ) {
		printf("%c%c\t\t", input[j], input[j+1] );
		return j + 2;
	}
	
	// Print an identifier or number
	if( input_guide[j] == 3 )
		is_three = 1;
	while( input_guide[j] == 3 && input_guide[j] != '\0' ) {
		printf("%c", input[j] );
		j++;
	}
	if( is_three ) {
		printf("\t\t");
		return j;
	}
	
	while( input[j] == '\n' || input[j] == '\r')
		j++;
	
	// Print "illegal token"
	if( input_guide[j] == 4 ) {
		printf("illegal token %c\n", tokens[j] );
		*ill = 1;
		return j + 1;
	}
}


// i=0 skip, i=1 print single char, i=2 print 2 char, i=3 print string or identifier
// until i changes, i=4 illegal token
// Example:
//		input: var x, dog ^;
//		guide: 2220210222041
void inputGuider( int i ) {
	input_guide[pointer] = i;
}


// Conditional Printf Statement that only prints if printSource is 1
// Replaces any printf in lex
void condPrintf( char ch ) {
	if( printSource )
		printf("%c", ch);
}

void initalizeLexer(char* fname) {

	file = fopen(fname,"r");
	terminate = 0;

	tokens = malloc( sizeof(char)*2500 );
	input = malloc( sizeof(char)*2500 );
	input_guide = malloc( sizeof(int)*2500 );
	t_pointer = 0;
	pointer = 0;
}

//Commented out for use of Compile.c
/* 

int main( int argc, char** argv ) {
	
	char* fname = NULL;
	int printClean = 0;
	printSource = 0;
	
	argc--; *argv++; //dont need first argument (executable name)
	while (argc-- > 0) {
		char* arg = *argv++;
		if (strcmp(arg,"--source")==0)
			printSource = 1;
		else if (strcmp(arg,"--clean")==0)
			printClean = 1;
		else if (fname==NULL)
			fname = arg;
	}
	
	file = fopen(fname,"r");
	terminate = 0;
  	int i = 0;
	char c;
	token_type tok;

	tokens = malloc( sizeof(char)*2500 );
	input = malloc( sizeof(char)*2500 );
	input_guide = malloc( sizeof(int)*2500 );
	t_pointer = 0;
	pointer = 0;

  	
	// Lex the input and print the input
  	if ( printSource) 
		printf("\nsource code:\n-----------\n");
	while ((tok=lex()->type) != nulsym) {
		
		// There was an error so stop the program
		if (terminate) {
			free(input);
			free(tokens);
			free(input_guide);
			fclose(file);
			return 0;
		}
		
		// Print tokens later
		tokenSym( tok );

	}
	input[pointer] = '\0';
	
	// PRINT WITHOUT COMMENTS
	if ( printClean ) {
		printf("source code without comments:\n-----------------------------\n");
		i = 0;
		c = input[0];
		while( c != '\0') {
			
			printf("%c", c);
			i++;
			c = input[i];
		}
	}
	
	
	// PRINT TOKENS
	printf("\n\ntokens:\n-------\n");
	int j;
	i = 0;
	int illegal = 0;
	for( j = 0; j < pointer; i++ ) { // Note: i++ not j++
		
		j = wordPrint( j, &illegal );
		if( !illegal )
			printf("%-2d\n", tokens[i] );
		illegal = 0;
		
	}

	free(input);
	free(tokens);
	free(input_guide);
	fclose(file);
	
	return 0;
}
*/


