#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum{ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS
}Kind;
typedef enum{STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
}GrammarState;
typedef struct TokenUnit{
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit{
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;

/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x){\
	puts("Compile Error!");\
	if(DEBUG){\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
Token *lexer(const char *in);
Token *new_token(Kind kind, int val);
size_t token_list_to_arr(Token **head);
AST *parser(Token *arr, size_t len);
AST *parse(Token *arr, int l, int r, GrammarState S);
AST *new_AST(Kind kind, int val);
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
int condASSIGN(Kind kind);
int condADD(Kind kind);
int condMUL(Kind kind);
int condRPAR(Kind kind);
void semantic_check(AST *now);
int codegen(AST *root);
void freeAST(AST *now);

/// debug interfaces
void token_print(Token *in, size_t len);
void AST_print(AST *head);

char input[MAX_LENGTH];

int mem=-1;
int xmem=-1, ymem=-1, zmem=-1;

int main(){
	while(fgets(input, MAX_LENGTH, stdin) != NULL){ //逐行吃輸入+處理
		Token *content = lexer(input);              //將input轉為Token linked list
		size_t len = token_list_to_arr(&content);   //將linked list轉為array
		AST *ast_root = parser(content, len);       //用Token array建AST
		semantic_check(ast_root);                   //執行semantic check
		//AST_print(ast_root);
		codegen(ast_root);                     		//Generate ASM code
		                                            //Optimization
		free(content);                              //釋放記憶體:Token array
		freeAST(ast_root);                          //釋放記憶體:AST tree

		mem=-1;										//重設register
		xmem=-1, ymem=-1, zmem=-1;
	}
	return 0;
}

// Split the input char array into token linked list.
Token *lexer(const char *in){
	Token *head = NULL;
	Token **now = &head;
	for(int i = 0; in[i]; i++){
		if(in[i]==' ' || in[i]=='\n') // ignore space and newline
			continue;
		else if(isdigit(in[i])){
			(*now) = new_token(CONSTANT, atoi(in+i));
			while(in[i+1] && isdigit(in[i+1])) i++;
		}
		else if('x'<=in[i] && in[i]<='z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch(in[i]){
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if(in[i+1] && in[i+1]=='+'){
					i++;
					// In lexer scope, all "++" will be labeled as PREINC. 先以preinc代替
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if(in[i+1] && in[i+1]=='-'){
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

// Create a new token.
Token *new_token(Kind kind, int val){
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head){
	size_t res;
	Token *now = (*head), *del;
	for(res=0; now!=NULL; res++) now = now->next;
	now = (*head);
	if(res!=0) (*head) = (Token*)malloc(sizeof(Token)*res);
	for(int i=0; i<res; i++){
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}

// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len){
	for(int i=1; i<len; i++){
		// correctly identify "ADD" and "SUB"
		if(arr[i].kind==PLUS || arr[i].kind==MINUS){
			switch (arr[i-1].kind){
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind-PLUS+ADD;
				default: break;
			}
		}
	}
	return parse(arr, 0, len-1, STMT);
}

// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S){
	AST *now = NULL;
	if(l>r){
        if(S==STMT) return now;
        else err("Unexpected parsing range.");
	}
	int nxt;
	switch(S){
		case STMT:
			if(l>r) return now;
			else return parse(arr, l, r, EXPR);
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if((nxt=findNextSection(arr, l, r, condASSIGN)) != -1){
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt-1, UNARY_EXPR);
				now->rhs = parse(arr, nxt+1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt=findNextSection(arr, r, l, condADD)) != -1){
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt-1, ADD_EXPR);
				now->rhs = parse(arr, nxt+1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
			// TODO: Implement MUL_EXPR.
			// hint: Take ADD_EXPR as reference.
            if((nxt=findNextSection(arr, r, l, condMUL)) != -1){
                now = new_AST(arr[nxt].kind, 0);
                now->lhs = parse(arr, l, nxt-1, MUL_EXPR);
                now->rhs = parse(arr, nxt+1, r, UNARY_EXPR);
                return now;
            }
            return parse(arr, l, r, UNARY_EXPR);
		case UNARY_EXPR:
			// TODO: Implement UNARY_EXPR.
			// hint: Take POSTFIX_EXPR as reference.
            if(arr[l].kind==PREINC || arr[l].kind==PREDEC || arr[l].kind==PLUS || arr[l].kind==MINUS){
                now = new_AST(arr[l].kind, 0);
                now->mid = parse(arr, l+1, r, UNARY_EXPR);
                return now;
            }
            return parse(arr, l, r, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if(arr[r].kind==PREINC || arr[r].kind==PREDEC){
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind-PREINC+POSTINC, 0);
				now->mid = parse(arr, l, r-1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if(findNextSection(arr, l, r, condRPAR)==r){
				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l+1, r-1, EXPR);
				return now;
			}
			if(l==r){
				if(arr[l].kind == IDENTIFIER || arr[l].kind==CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

// Create a new AST node.
AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)){
	int par = 0;
	int d = (start<end)? 1:-1;
	for(int i=start; (start<end)? (i<=end):(i>=end); i+=d){
		if(arr[i].kind==LPAR) par++;
		if(arr[i].kind==RPAR) par--;
		if(par==0 && cond(arr[i].kind)==1) return i;
	}
	return -1;
}

// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind){
	return kind==ASSIGN;
}

// Return 1 if kind is ADD or SUB.
int condADD(Kind kind){
	return kind==ADD || kind==SUB;
}

// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind){
	return kind==MUL || kind==DIV || kind==REM;
}

// Return 1 if kind is RPAR.
int condRPAR(Kind kind){
	return kind==RPAR;
}

// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now){
	if(now==NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if(now->kind==ASSIGN){
		AST *tmp = now->lhs;
		while(tmp->kind==LPAR) tmp = tmp->mid;
		if(tmp->kind!=IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
	}
	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	// TODO: Implement the remaining semantic_check code.
	// hint: Follow the instruction above and ASSIGN-part code to implement.
	// hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
    if(now->kind==PREINC || now->kind==PREDEC){
        AST *tmp = now->mid;
        while(tmp->kind==LPAR) tmp = tmp->mid;
        if(tmp->kind!=IDENTIFIER)
            err("Lvalue is required as right operand of pre-increment or pre-decrement.");
	}
	if(now->kind==POSTINC || now->kind==POSTDEC){
        AST *tmp = now->mid;
        while(tmp->kind==LPAR) tmp = tmp->mid;
        if(tmp->kind!=IDENTIFIER)
            err("Lvalue is required as left operand of post-increment or post-decrement.");
	}
	semantic_check(now->lhs);
	semantic_check(now->rhs);
	semantic_check(now->mid);
}

//int mem=-1;
//int xmem=-1, ymem=-1, zmem=-1;
// Generate ASM code
int codegen(AST *root){
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.
    // Inference: OJ: Storm Area 51
    if(root){
        if(root->kind==ASSIGN){
			AST *p;
			p = root->lhs;  //skip parenthesis
			if(p->kind==LPAR){
				while (p->mid->kind==LPAR) p = p->mid;
				p = p->mid;
			}
			int tmp = codegen(root->rhs);
			if(root->rhs->kind==CONSTANT){
            	printf("add r%d %d 0\n", ++mem, root->rhs->val);
				printf("store [%d] r%d\n", 4*(p->val-'x'), mem);
				if(p->val=='x') xmem=mem;
                else if(p->val=='y') ymem=mem;
                else if(p->val=='z') zmem=mem;
				return mem;
			}else{
				printf("store [%d] r%d\n", 4*(p->val-'x'), tmp);
				if(p->val=='x') xmem=tmp;
                else if(p->val=='y') ymem=tmp;
                else if(p->val=='z') zmem=tmp;
				return tmp;
			}
        }else if(root->kind==CONSTANT){
			return -1;
		}else if(root->kind==IDENTIFIER){
            int ret=-1;
			if(root->val=='x' && xmem==-1) ret = xmem = ++mem;
			else if(root->val=='y' && ymem==-1) ret = ymem = ++mem;
			else if(root->val=='z' && zmem==-1) ret = zmem = ++mem;
			if(ret!=-1) printf("load r%d [%d]\n", ret, 4*(root->val-'x'));

			if(root->val=='x' && xmem!=-1) ret = xmem;
			else if(root->val=='y' && ymem!=-1) ret = ymem;
			else if(root->val=='z' && zmem!=-1) ret = zmem;

            return ret;
        }else if(root->kind>=ADD && root->kind<=REM){
            int ret=-1;
			int Ltmp = codegen(root->lhs);
            int Rtmp = codegen(root->rhs);
			switch(root->kind){
				case ADD: printf("add "); break; //ans = left+right;
            	case SUB: printf("sub "); break; //ans = left-right;
            	case MUL: printf("mul "); break; //ans = left*right;
            	case DIV: printf("div "); break; //ans = left/right;
            	case REM: printf("rem "); break; //ans = left%right;
            }
            if(Ltmp!=-1 && Ltmp!=xmem && Ltmp!=ymem && Ltmp!=zmem){
            	printf("r%d ", Ltmp);
            	ret = Ltmp;
			}else if(Rtmp!=-1 && Rtmp!=xmem && Rtmp!=ymem && Rtmp!=zmem){
				printf("r%d ", Rtmp);
				ret = Rtmp;
			}else{
				printf("r%d ", ++mem);
				ret = mem;
			}
            if(root->lhs->kind==IDENTIFIER){
                if(root->lhs->val=='x') printf("r%d ", xmem);
                else if(root->lhs->val=='y') printf("r%d ", ymem);
                else if(root->lhs->val=='z') printf("r%d ", zmem);
            }else if(root->lhs->kind==CONSTANT){
                printf("%d ", root->lhs->val);
            }else{
                printf("r%d ", Ltmp);
            }
            if(root->rhs->kind==IDENTIFIER){
                if(root->rhs->val=='x') printf("r%d\n", xmem);
                else if(root->rhs->val=='y') printf("r%d\n", ymem);
                else if(root->rhs->val=='z') printf("r%d\n", zmem);
            }else if(root->rhs->kind==CONSTANT){
                printf("%d\n", root->rhs->val);
            }else{
                printf("r%d\n", Rtmp);
            }
            return ret;
        }else if(root->kind>=PREINC && root->kind<=POSTDEC){
			int ret = codegen(root->mid);
			if(root->kind==PREINC){
				while(root->mid->kind!=IDENTIFIER) root = root->mid;
				printf("add r%d r%d 1\n", ret, ret);
				printf("store [%d] r%d\n", 4*(root->mid->val-'x'), ret);
				if(root->mid->val=='x' && xmem==-1) xmem = ret;
                else if(root->mid->val=='y' && ymem==-1) ymem = ret;
                else if(root->mid->val=='z' && zmem==-1) zmem = ret;
                return ret;
                //root->rhs = root->rhs+1;
                //return root->rhs;
            }else if(root->kind==PREDEC){
            	while(root->mid->kind!=IDENTIFIER) root = root->mid;
                printf("sub r%d r%d 1\n", ret, ret);
				printf("store [%d] r%d\n", 4*(root->mid->val-'x'), ret);
				if(root->mid->val=='x' && xmem==-1) xmem = ret;
                else if(root->mid->val=='y' && ymem==-1) ymem = ret;
                else if(root->mid->val=='z' && zmem==-1) zmem = ret;
                return ret;
                //root->rhs = root->rhs-1;
                //return root->rhs;
            }else if(root->kind==POSTINC){
                //codegen(root->mid);
                int tmp = ret;
				ret = ++mem;
				while(root->mid->kind!=IDENTIFIER) root = root->mid;
				printf("add r%d r%d 1\n", ret, tmp);
				printf("store [%d] r%d\n", 4*(root->mid->val-'x'), ret);
				if(root->mid->val=='x' && xmem==-1) xmem = ret;
                else if(root->mid->val=='y' && ymem==-1) ymem = ret;
                else if(root->mid->val=='z' && zmem==-1) zmem = ret;
                return tmp;
                //int tmp = root->lhs;
                //root->lhs = root->lhs+1;
                //return tmp;
            }else if(root->kind==POSTDEC){
                //codegen(root->rhs);
				int tmp = ret;
				ret = ++mem;
				while(root->mid->kind!=IDENTIFIER) root = root->mid;
				printf("sub r%d r%d 1\n", ret, tmp);
				printf("store [%d] r%d\n", 4*(root->mid->val-'x'), ret);
				if(root->mid->val=='x' && xmem==-1) xmem = ret;
                else if(root->mid->val=='y' && ymem==-1) ymem = ret;
                else if(root->mid->val=='z' && zmem==-1) zmem = ret;
                return tmp;
                //int tmp = root->lhs;
                //root->lhs = root->lhs-1;
                //return tmp;
            }
        }else if(root->kind==PLUS){
            int ret = codegen(root->mid);
			if(ret!=-1){
            	printf("add r%d r%d 0\n", ++mem, ret);
            	return mem;
			}else{
				printf("add r%d %d 0\n", ++mem, root->mid->val);
				return mem;
			}
        }else if(root->kind==MINUS){
            int ret = codegen(root->mid);
			if(ret!=-1){
            	printf("sub r%d 0 r%d\n", ++mem, ret);
            	return mem;
			}else{
				printf("sub r%d 0 %d\n", ++mem, root->mid->val);
				return mem;
			}
        }else if(root->kind==LPAR || root->kind==RPAR){
			int ret = codegen(root->mid);
			return ret;
		}
    }
    return -1;
}

// Free the whole AST.
void freeAST(AST *now){
	if(now==NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

/// debug interfaces
// Print token array.
void token_print(Token *in, size_t len){
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus"
	};
	const static char KindSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i=0; i<len; i++){
		switch(in[i].kind){
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				printf(format_str,i , KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
				break;
			case CONSTANT:
				printf(format_int,i , KindName[in[i].kind], "value", in[i].val);
				break;
			case IDENTIFIER:
				printf(format_str,i , KindName[in[i].kind], "name", (char*)(&(in[i].val)));
				break;
			default:
				puts("=== unknown token ===");
		}
	}
}

// Print AST tree.
void AST_print(AST *head){
	static char indent_str[MAX_LENGTH] = "";
	static int indent = 0;
	char *indent_now = indent_str+indent;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head==NULL) return;
	indent_str[indent-1] = '-';
	printf("%s", indent_str);
	indent_str[indent-1] = ' ';
	if (indent_str[indent-2] == '`')
		indent_str[indent-2] = ' ';
	switch(head->kind){
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			printf(format, KindName[head->kind]);
			break;
		case IDENTIFIER:
			printf(format_str, KindName[head->kind], "name", (char *)&(head->val));
			break;
		case CONSTANT:
			printf(format_val, KindName[head->kind], "value", head->val);
			break;
		default:
			puts("=== unknown AST type ===");
	}
	indent += 2;
	strcpy(indent_now, "| ");
	AST_print(head->lhs);
	strcpy(indent_now, "` ");
	AST_print(head->mid);
	AST_print(head->rhs);
	indent -= 2;
	(*indent_now) = '\0';
}
