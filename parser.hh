/*
Author: Monica Moniot
Login:  mamoniot
GitHub: mamoniot

Compile only this .cc file with your preferred compiler. Run it with or without one of the test.ml source files on the command line.

basic.h is my standard header full of macros that I use in all of my c projects. Used the most here are the "for_each" macros, which expand into a for loop, and the "tape" datastructure, a polymorphic dynamic array built out of macros.

I started with the tokenizer since it is just a simple finite state machine. I decided that it would fill out a token list consisting to pointers to the original tokens parsed from the source file, instead of copying those tokens. I figured this would be faster, simpler and useful for error messaging. Eventually I decided to also add the state of the tokenizer to the token list, so I could give significantly better error messages.

The parser started as a copy of the sample code's recursive parser, with the main exception being comparing and eating tokens were inline instead of separate functions. Eventually I realized I could use the tokenizer state to make comparing and eating tokens more efficient, and then I moved these into separate functions. I also realized that for parsing each binary operator I was copying and pasting the code with minimal changes, so I also separated this into a separate function. Each syntax tree node is an enum-union struct. It allows me to treat each node both as having a generic type and specific type in a very simple manner while also giving me much more control than object inheritence or duck-typing (both of these are actually fancy semantic macros for enum-union structs).

The tokenizer allocates memory on a dynamic array and the parser on a wide linked list. This improves locality and simplifies memory managment dramatically. Despite this being in C, and there being a tree involved, there is almost the same amount of memory management here as there would be in python (If I had used RAII this *definitely* would not be the case).

The printer uses a dynamic program to build a list of the dimensions of every subtree in the syntax tree, and then reads off of those dimensions recursively to render the tree onto a grid of spaces.

There is no part of this code that did not change drastically except parts of the tokenizer. This is why comments in my code are limitted; I don't expect them to be accurate long enough to be useful.

Written exercises:
Exercise 4:
"if A then if B then <...> else <...>" is ambiguous
if the grammar were "<stmt> ::= if C then <stmt> else <stmt> end" then it wouldn't be
Exercise 5:
"**" is right associative in python, 2**3**2 gives 512 instead of 64

...
*/
#ifndef PARSER__H_INCLUDE
#define PARSER__H_INCLUDE

#include "basic.h"
#include "stdio.h"
#include "math.h"

inline bool strcmp(char* str0, int32 size0, char* str1, int32 size1) {return size0 == size1 ? memcmp(str0, str1, size0) == 0 : 0;}
inline bool strcmp(char* str0, int32 size0, char* str1) {return size0 == strlen(str1) ? memcmp(str0, str1, size0) == 0 : 0;}

static char* KEYWORD_IF = "if";
static char* KEYWORD_THEN = "then";
static char* KEYWORD_ELSE = "else";
static char* KEYWORD_LET = "let";
static char* KEYWORD_IN = "in";
static char* KEYWORD_END = "end";
static char* KEYWORD_AND = "and";
static char* KEYWORD_FN = "fn";

static char* BINOP_EXP = "**";
static char* BINOP_LOG = "//";
static char* BINOP_EQ = "=";
static char* BINOP_MULT = "*";
static char* BINOP_DIV = "/";
static char* BINOP_MOD = "%";
static char* BINOP_ADD = "+";
static char* BINOP_SUB = "-";
static char* BINOP_LT = "<";
static char* BINOP_GT = ">";
static char* BINOP_AND = "andalso";
static char* BINOP_OR = "orelse";
static char* BINOP_SEQ = ";";
static char* BINOP_APP = ".";
static char* BINOP_PREP = "::";
static char* SYM_FN = "=>";
static char* SYM_OPEN_PAREN = "(";
static char* SYM_CLOSE_PAREN = ")";


enum TokenState : int32 {
	TOKEN_NULL,
	TOKEN_BASE,
	TOKEN_EOF,
	TOKEN_NUMERAL,
	TOKEN_NAME,
	TOKEN_SYM,
	TOKEN_UNARY,
	TOKEN_BINOP,
	TOKEN_PAREN,
	TOKEN_KEYWORD,
};
struct Tokens {
	TokenState* states;
	char** strings;
	int32* sizes;
	int32* lines;
	int32* columns;
};

Tokens tokenize(char* str, int32 str_size);


enum Part : int32 {
	PART_NULL = 0,
	PART_NUMERAL,
	PART_BOOL,
	PART_UNIT,
	PART_VAR,
PART_FIRST_OP,
PART_FIRST_BINOP,
	PART_ADD, PART_SUB,
	PART_MULT, PART_DIV, PART_MOD,
	PART_EQ, PART_LT, PART_GT,
	PART_SEQ,
	PART_APP,
PART_LAST_LEFTASC,
	PART_PREP,
	PART_EXP, PART_LOG,
PART_LAST_RIGHTASC,
	PART_AND, PART_OR,
PART_LAST_OP,
	PART_COND,
	PART_BIND,
	PART_DECL,
	PART_MULTIDECL,
	PART_FN,
PART_LAST_PARENT,
PART_LAST,
};

bool is_binop(Part p);
bool is_unary(Part p);
bool is_operator(Part p);
bool has_children(Part p);
bool is_leftasc(Part p);
bool is_rightasc(Part p);

void push_string(char** tape, char* str, int32 size);
void push_string(char** tape, char* str);
void push_string(char** tape, char ch);
#define push_char push_string

int32 push_uint(char** tape, uint32 n);
void push_uint_and_pad(char** tape, uint32 n, int32 pad);


struct Source {
	char* text;
	int32* lines;
	int32 size;
};

struct SourcePos {
	int32 line;
	int32 column;
	int32 size;
};

struct AST {
	Part part;
	SourcePos source;
	union {
		AST* children[3];
		int32 numeral;
		bool boolean;
		struct {
			char* str;
			int32 size;
		} var;
		AST* unary;
		struct {
			AST* left;
			AST* right;
		} binop;
		struct {
			AST* is;
			AST* top;
			AST* bot;
		} cond;
		struct {
			AST* arg;
			AST* body;
		} fn;
		struct {
			AST* iden;
			AST* right;
			AST* block;
		} bind;
	};
};

struct NodeBuffer {
	NodeBuffer* next;
	int32 size;
	AST nodes[KILOBYTE];
};
struct NodeList {
	int32 size;
	NodeBuffer* head;
};


#define DECL_PARSER(name) AST* name(Source source, Tokens tokens, int32* token_i, NodeList* node_list, char** error_msg)
typedef DECL_PARSER(ParseFunc);

AST* parse_all(Source source, Tokens tokens, char** error_msg);

void print_tree_basic(AST* root);
void print_tree(AST* root);

void destroy_tree(AST* tokens);
void destroy_tokens(Tokens tokens);


void msg_unexpected(Source source, int32 line, int32 column, int32 size, char* error_type, char* unexpected, char* expected, char** error_msg);
inline void msg_unexpected(Source source, SourcePos pos, char* error_type, char* unexpected, char* expected, char** error_msg) {msg_unexpected(source, pos.line, pos.column, pos.size, error_type, unexpected, expected, error_msg);}

#endif

#ifdef PARSER_IMPLEMENTATION
#undef PARSER_IMPLEMENTATION

static int32 min(int32 a, int32 b) {return a < b ? a : b;}
static int32 max(int32 a, int32 b) {return a > b ? a : b;}

static bool is_whitespace(char ch) {return ch == ' ' | ch == '\t' | ch == '\n';}
static bool is_alpha(char ch) {return ('a' <= ch & ch <= 'z') | ('A' <= ch & ch <= 'Z') | ch == '_';}
static bool is_numeral(char ch) {return '0' <= ch & ch <= '9';}
static bool is_symbol(char ch) {return ch == '+' | ch == '*' | ch == '/' | ch == '-' | ch == '%' | ch == '=' | ch == '<' | ch == '>' | ch == ';' | ch == ':';}

Tokens tokenize(Source* source) {
	source->lines = 0;
	Tokens tokens = {};
	TokenState state = TOKEN_BASE;
	char* str = source->text;
	int32 str_size = source->size;
	int32 i = 0;
	int32 cur_line = 1;
	int32 cur_column = 1;
	tape_push(&source->lines, -1);
	tape_push(&source->lines, 0);
	while(i < str_size) {
		char ch = str[i];
		if(state == TOKEN_BASE) {
			if(ch == 0) {
				state = TOKEN_NULL;
			} else if(is_alpha(ch)) {
				state = TOKEN_NAME;
			} else if(is_symbol(ch)) {
				state = TOKEN_SYM;
			} else if(is_numeral(ch)) {
				state = TOKEN_NUMERAL;
			} else if(ch == '(' | ch == ')') {
				state = TOKEN_PAREN;
			}
			if(state != TOKEN_BASE) {
				tape_push(&tokens.strings, &str[i]);
				tape_push(&tokens.sizes, 1);
				tape_push(&tokens.states, state);
				tape_push(&tokens.lines, cur_line);
				tape_push(&tokens.columns, cur_column);
			}
		} else if(state == TOKEN_NUMERAL) {
			if(is_numeral(ch)) {
				tape_get_last(&tokens.sizes) += 1;
			} else {
				state = TOKEN_BASE;
				continue;
			}
		} else if(state == TOKEN_NAME) {
			int32 token_i = tape_size(&tokens.sizes) - 1;
			if(is_alpha(ch)) {
				tokens.sizes[token_i] += 1;
			} else {
				char* cur_token = tokens.strings[token_i];
				int32 cur_size = tokens.sizes[token_i];
				if(
					strcmp(cur_token, cur_size, KEYWORD_IF) ||
					strcmp(cur_token, cur_size, KEYWORD_THEN) ||
					strcmp(cur_token, cur_size, KEYWORD_ELSE) ||
					strcmp(cur_token, cur_size, KEYWORD_LET) ||
					strcmp(cur_token, cur_size, KEYWORD_IN) ||
					strcmp(cur_token, cur_size, KEYWORD_FN) ||
					strcmp(cur_token, cur_size, KEYWORD_AND) ||
					strcmp(cur_token, cur_size, KEYWORD_END)
				) {
					tokens.states[token_i] = TOKEN_KEYWORD;
				} else if(
					strcmp(cur_token, cur_size, BINOP_AND) ||
					strcmp(cur_token, cur_size, BINOP_OR)
				) {
					tokens.states[token_i] = TOKEN_BINOP;
				}
				state = TOKEN_BASE;
				continue;
			}
		} else if(state == TOKEN_SYM) {
			int32 token_i = tape_size(&tokens.sizes) - 1;
			if(is_symbol(ch)) {
				tokens.sizes[token_i] += 1;
			} else {
				char* cur_token = tokens.strings[token_i];
				int32 cur_size = tokens.sizes[token_i];
				if(
					strcmp(cur_token, cur_size, BINOP_ADD) ||
					strcmp(cur_token, cur_size, BINOP_SUB) ||
					strcmp(cur_token, cur_size, BINOP_MULT) ||
					strcmp(cur_token, cur_size, BINOP_DIV) ||
					strcmp(cur_token, cur_size, BINOP_MOD) ||
					strcmp(cur_token, cur_size, BINOP_EXP) ||
					strcmp(cur_token, cur_size, BINOP_LOG) ||
					strcmp(cur_token, cur_size, BINOP_EQ) ||
					strcmp(cur_token, cur_size, BINOP_LT) ||
					strcmp(cur_token, cur_size, BINOP_GT) ||
					strcmp(cur_token, cur_size, BINOP_AND) ||
					strcmp(cur_token, cur_size, BINOP_APP) ||
					strcmp(cur_token, cur_size, BINOP_SEQ) ||
					strcmp(cur_token, cur_size, BINOP_PREP) ||
					strcmp(cur_token, cur_size, BINOP_OR)
				) {
					tokens.states[token_i] = TOKEN_BINOP;
				} //else {
					// tokens.states[token_i] = TOKEN_SYM;
				// }
				state = TOKEN_BASE;
				continue;
			}
		} else {
			state = TOKEN_BASE;
			continue;
		}

		i += 1;
		if(ch == '\n') {
			cur_line += 1;
			cur_column = 0;
			tape_push(&source->lines, i);
		}
		cur_column += 1;
	}
	tape_push(&tokens.strings, &str[str_size]);
	tape_push(&tokens.sizes, 0);
	tape_push(&tokens.states, TOKEN_EOF);
	tape_push(&tokens.lines, cur_line);
	tape_push(&tokens.columns, cur_column);
	return tokens;
}


bool is_binop(Part p) {return PART_FIRST_BINOP < p & p < PART_LAST_OP;}
bool is_unary(Part p) {return PART_FIRST_OP < p & p < PART_FIRST_BINOP;}
bool is_operator(Part p) {return PART_FIRST_OP < p & p < PART_LAST_OP;}
bool has_children(Part p) {return PART_FIRST_OP < p & p < PART_LAST_PARENT;}
bool is_leftasc(Part p) {return PART_FIRST_BINOP < p & p < PART_LAST_LEFTASC;}
bool is_rightasc(Part p) {return PART_LAST_LEFTASC < p & p < PART_LAST_RIGHTASC;}


void push_string(char** tape, char* str, int32 size) {
	memcpy(tape_reserve(tape, size), str, size);
}
void push_string(char** tape, char* str) {
	push_string(tape, str, strlen(str));
}
void push_string(char** tape, char ch) {
	tape_push(tape, ch);
}
#define push_char push_string

int32 push_uint(char** tape, uint32 n) {
	char ch = (n%10) + '0';
	int32 d = 1;
	if(n > 9) d += push_uint(tape, n/10);
	tape_push(tape, ch);
	return d;
}
void push_uint_and_pad(char** tape, uint32 n, int32 pad) {
	pad -= push_uint(tape, n);
	for_each_lt(i, pad) tape_push(tape, ' ');
}

void msg_unexpected(Source source, int32 line, int32 column, int32 size, char* error_type, char* unexpected, char* expected, char** error_msg) {
	if(!error_msg) return;
	char* text = source.text;
	if(error_type) {
		push_string(error_msg, error_type);
		push_string(error_msg, "; ");
	}
	push_string(error_msg, "Unexpected ");
	push_string(error_msg, unexpected);
	push_string(error_msg, " at line ");
	push_uint(error_msg, line);
	push_string(error_msg, ", column ");
	push_uint(error_msg, column);
	if(expected) {
		push_string(error_msg, ". Expected ");
		push_string(error_msg, expected);
	}
	push_string(error_msg, ".\n");
	//source display
	int32 total_lines = tape_size(&source.lines);
	int32 saw = source.lines[line] + column;
	int32 upper = 0;
	int32 upper_size = 0;
	int32 middle = source.lines[line];
	int32 middle_size = 0;
	int32 lower = 0;
	int32 lower_size = 0;
	if(line - 1 > 0) {
		upper = source.lines[line - 1];
		upper_size = source.lines[line] - upper - 1;
	}
	if(line + 1 < total_lines) {
		lower = source.lines[line + 1];
		if(line + 2 < total_lines) {
			lower_size = source.lines[line + 2] - lower;
		} else {
			lower_size = source.size - lower;
		}
		middle_size = lower - middle - 1;
	} else {
		middle_size = source.size - middle;
	}
	{
		int32 padding = max(cast(int32, log10(line + 1)) + 2, 4);
		if(line <= 2) {
			push_string(error_msg, '\n');
		} else {
			for_each_lt(i, padding - 1) push_string(error_msg, ' ');
			push_string(error_msg, "...\n");
		}
		if(upper) {
			push_uint_and_pad(error_msg, line - 1, padding);
			push_string(error_msg, text + upper, upper_size);
			push_string(error_msg, '\n');
		}
		push_uint_and_pad(error_msg, line, padding);
		push_string(error_msg, text + middle, middle_size);
		push_string(error_msg, '\n');
		for_each_lt(i, column - 1 + padding) {
			push_string(error_msg, '~');
		}
		push_string(error_msg, '^');
		for_each_lt(i, size - 1) {
			push_string(error_msg, '^');
		}
		push_string(error_msg, '\n');
		if(lower) {
			push_uint_and_pad(error_msg, line + 1, padding);
			push_string(error_msg, text + lower, lower_size);
			push_string(error_msg, '\n');
		}
		for_each_lt(i, padding - 1) push_string(error_msg, ' ');
		push_string(error_msg, "...\n");
	}
}
static void msg_unexpected(Source source, Tokens tokens, int32 token_i, char* expected, char** error_msg) {
	TokenState state = tokens.states[token_i];
	char* unexpected;
	if(state == TOKEN_NULL) {
		unexpected = "null character";
	} else if(state == TOKEN_EOF) {
		unexpected = "end of file";
	} else if(state == TOKEN_NUMERAL) {
		unexpected = "numeral";
	} else if(state == TOKEN_SYM) {
		unexpected = "symbol";
	} else if(state == TOKEN_UNARY) {
		unexpected = "unary operator";
	} else if(state == TOKEN_BINOP) {
		unexpected = "binary operator";
	} else if(state == TOKEN_PAREN) {
		unexpected = "parathesis";
	} else if(state == TOKEN_KEYWORD) {
		unexpected = "keyword";
	} else {
		unexpected = "token";
	}
	msg_unexpected(source, tokens.lines[token_i], tokens.columns[token_i], tokens.sizes[token_i], "Syntax Error", unexpected, expected, error_msg);
}


static void set_node_list_size(NodeList* node_list, int32 size) {
	while(node_list->size > size) {
		NodeBuffer* cur_head = node_list->head;
		if(node_list->size - cur_head->size >= size) {
			node_list->size -= cur_head->size;
			node_list->head = cur_head->next;
			free(cur_head);
		} else {
			cur_head->size += size - node_list->size;
			node_list->size = size;
			break;
		}
	}
}
static AST* reserve_node(NodeList* node_list, int32 line, int32 column, int32 size) {
	node_list->size += 1;
	if(node_list->head->size >= KILOBYTE) {
		NodeBuffer* buffer = (NodeBuffer*)malloc(sizeof(NodeBuffer));
		buffer->next = node_list->head;
		buffer->size = 0;
		node_list->head = buffer;
	}
	AST* ret = &node_list->head->nodes[node_list->head->size];
	node_list->head->size += 1;
	memzero(ret, sizeof(*ret));
	ret->source.line = line;
	ret->source.column = column;
	ret->source.size = size;
	return ret;
}
static AST* reserve_var(NodeList* node_list, char* str, int32 line, int32 column, int32 size) {
	AST* var = reserve_node(node_list, line, column, size);
	var->part = PART_VAR;
	var->var.str = str;
	var->var.size = size;
	return var;
}


static bool eat_token(Tokens tokens, int32* token_i, TokenState state) {
	TokenState cur_state = tokens.states[*token_i];
	if(cur_state == state) {
		*token_i += 1;
		return 1;
	} else {
		return 0;
	}
}
static bool eat_token(Tokens tokens, int32* token_i, TokenState state, char* str) {
	TokenState cur_state = tokens.states[*token_i];
	char* cur_token = tokens.strings[*token_i];
	int32 cur_token_size = tokens.sizes[*token_i];
	if(cur_state == state && strcmp(cur_token, cur_token_size, str)) {
		*token_i += 1;
		return 1;
	} else {
		return 0;
	}
}

static int32 eval_numeral(char* str, int32 size) {
	int32 n = 0;
	for_each_in(ch, str, size) {
		n *= 10;
		n += *ch - '0';
	}
	return n;
}


static AST* parse_left_binops_loop_(AST* left_node, Source source, Tokens tokens, int32* token_i, NodeList* node_list, char** error_msg, char** op_strings, Part* op_parts, int32 total_binops, ParseFunc* parser_lower) {
	int32 binop_i = -1;
	int32 pre_token_i = *token_i;
	for_each_lt(i, total_binops) {
		if(eat_token(tokens, token_i, TOKEN_BINOP, op_strings[i])) {
			binop_i = i;
			break;
		}
	}
	if(binop_i < 0) return left_node;

	AST* right_node = parser_lower(source, tokens, token_i, node_list, error_msg);
	if(!right_node) return 0;
	AST* new_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
	new_node->part = op_parts[binop_i];
	new_node->binop.left = left_node;
	new_node->binop.right = right_node;
	return parse_left_binops_loop_(new_node, source, tokens, token_i, node_list, error_msg, op_strings, op_parts, total_binops, parser_lower);
}
static AST* parse_left_binops(Source source, Tokens tokens, int32* token_i, NodeList* node_list, char** error_msg, char** op_strings, Part* op_parts, int32 total_binops, ParseFunc* parser_lower) {
	AST* node = parser_lower(source, tokens, token_i, node_list, error_msg);
	if(!node) return 0;
	return parse_left_binops_loop_(node, source, tokens, token_i, node_list, error_msg, op_strings, op_parts, total_binops, parser_lower);
}

static AST* parse_right_binops(Source source, Tokens tokens, int32* token_i, NodeList* node_list, char** error_msg, char** op_strings, Part* op_parts, int32 total_binops, ParseFunc* parser_lower) {
	AST* node = parser_lower(source, tokens, token_i, node_list, error_msg);
	if(!node) return 0;
	int32 binop_i = -1;
	int32 pre_token_i = *token_i;
	for_each_lt(i, total_binops) {
		if(eat_token(tokens, token_i, TOKEN_BINOP, op_strings[i])) {
			binop_i = i;
			break;
		}
	}
	if(binop_i < 0) return node;

	AST* right_node = parse_right_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, total_binops, parser_lower);
	if(!right_node) return 0;
	AST* new_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
	new_node->part = op_parts[binop_i];
	new_node->binop.left = node;
	new_node->binop.right = right_node;
	return new_node;
}


static DECL_PARSER(parse_expn);
static DECL_PARSER(parse_atom) {
	TokenState cur_state = tokens.states[*token_i];
	char* cur_token = tokens.strings[*token_i];
	int32 cur_token_size = tokens.sizes[*token_i];

	int32 pre_token_i = *token_i;
	if(eat_token(tokens, token_i, TOKEN_NUMERAL)) {
		AST* node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
		node->part = PART_NUMERAL;
		node->numeral = eval_numeral(cur_token, cur_token_size);
		return node;
	} else if(eat_token(tokens, token_i, TOKEN_NAME)) {
		AST* node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
		if(strcmp(cur_token, cur_token_size, "true")) {
			node->part = PART_BOOL;
			node->boolean = 1;
		} else if(strcmp(cur_token, cur_token_size, "false")) {
			node->part = PART_BOOL;
			node->boolean = 0;
		} else {
			node->part = PART_VAR;
			node->var.str = cur_token;
			node->var.size = cur_token_size;
		}
		return node;
	} else if(eat_token(tokens, token_i, TOKEN_PAREN, SYM_OPEN_PAREN)) {
		if(eat_token(tokens, token_i, TOKEN_PAREN, SYM_CLOSE_PAREN)) {
			AST* node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
			node->part = PART_UNIT;
			return node;
		} else {
			char* op_strings[1] = {BINOP_SEQ};
			Part  op_parts  [1] = { PART_SEQ};
			AST* node = parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 1, &parse_expn);
			if(!node) return 0;
			if(eat_token(tokens, token_i, TOKEN_PAREN, SYM_CLOSE_PAREN)) {
				return node;
			} else {
				msg_unexpected(source, tokens, *token_i, "\")\"", error_msg);
				return 0;
			}
		}
	} else {
		msg_unexpected(source, tokens, *token_i, "literal or variable", error_msg);
		return 0;
	}
}

static AST* parse_app_loop_(AST* left_node, Source source, Tokens tokens, int32* token_i, NodeList* node_list, char** error_msg) {
	int32 pre_token_i = *token_i;
	if(eat_token(tokens, token_i, TOKEN_BINOP, BINOP_APP)) {
		AST* right_node = parse_atom(source, tokens, token_i, node_list, error_msg);
		if(!right_node) return 0;
		AST* new_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
		new_node->part = PART_APP;
		new_node->binop.left = left_node;
		new_node->binop.right = right_node;
		return parse_app_loop_(new_node, source, tokens, token_i, node_list, error_msg);
	} else {
		//non-deterministically check next token set
		int32 pre_node_list_size = node_list->size;
		AST* right_node = parse_atom(source, tokens, token_i, node_list, 0);
		if(right_node) {
			AST* new_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
			new_node->part = PART_APP;
			new_node->binop.left = left_node;
			new_node->binop.right = right_node;
			return parse_app_loop_(new_node, source, tokens, token_i, node_list, error_msg);
		} else {
			set_node_list_size(node_list, pre_node_list_size);
			*token_i = pre_token_i;
			return left_node;
		}
	}
}
static DECL_PARSER(parse_app) {
	AST* node = parse_atom(source, tokens, token_i, node_list, error_msg);
	if(!node) return 0;
	return parse_app_loop_(node, source, tokens, token_i, node_list, error_msg);
	// char* op_strings[1] = {BINOP_APP};
	// Part  op_parts  [1] = { PART_APP};
	// return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 1, &parse_atom);
}

static DECL_PARSER(parse_exp) {
	char* op_strings[2] = {BINOP_EXP, BINOP_LOG};
	Part  op_parts  [2] = { PART_EXP,  PART_LOG};
	return parse_right_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 2, &parse_app);
}
static DECL_PARSER(parse_mult) {
	char* op_strings[3] = {BINOP_MULT, BINOP_DIV, BINOP_MOD};
	Part  op_parts  [3] = { PART_MULT,  PART_DIV,  PART_MOD};
	return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 3, &parse_exp);
}
static DECL_PARSER(parse_add) {
	char* op_strings[2] = {BINOP_ADD, BINOP_SUB};
	Part  op_parts  [2] = { PART_ADD,  PART_SUB};
	return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 2, &parse_mult);
}

static DECL_PARSER(parse_comp) {
	char* op_strings[3] = {BINOP_EQ, BINOP_LT, BINOP_GT};
	Part  op_parts  [3] = { PART_EQ,  PART_LT,  PART_GT};
	return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 3, &parse_add);
}
static DECL_PARSER(parse_or) {
	char* op_strings[1] = {BINOP_OR};
	Part  op_parts  [1] = { PART_OR};
	return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 1, &parse_comp);
}
static DECL_PARSER(parse_and) {
	char* op_strings[1] = {BINOP_AND};
	Part  op_parts  [1] = { PART_AND};
	return parse_left_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 1, &parse_or);
}
static DECL_PARSER(parse_prep) {
	char* op_strings[1] = {BINOP_PREP};
	Part  op_parts  [1] = { PART_PREP};
	return parse_right_binops(source, tokens, token_i, node_list, error_msg, op_strings, op_parts, 1, &parse_and);
}

static DECL_PARSER(parse_expn) {
	int32 pre_token_i = *token_i;
	if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_LET)) {
		AST* bind_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
		AST* top_node = bind_node;
		if(eat_token(tokens, token_i, TOKEN_NAME, "val")) {
			int32 iden_i = *token_i;
			if(tokens.states[iden_i] == TOKEN_NAME) {
				*token_i += 1;
				AST* iden = reserve_var(node_list, tokens.strings[iden_i], tokens.lines[iden_i], tokens.columns[iden_i], tokens.sizes[iden_i]);
				bind_node->bind.iden = iden;
				if(eat_token(tokens, token_i, TOKEN_BINOP, BINOP_EQ)) {
					bind_node->bind.right = parse_expn(source, tokens, token_i, node_list, error_msg);
					if(!bind_node->bind.right) return 0;
				} else {
					msg_unexpected(source, tokens, *token_i, "\"=\"", error_msg);
					return 0;
				}

			} else {
				msg_unexpected(source, tokens, *token_i, "an identifier", error_msg);
				return 0;
			}
			bind_node->part = PART_BIND;
		} else if(eat_token(tokens, token_i, TOKEN_NAME, "fun")) {
			while(1) {
				int32 iden_i = *token_i;
				if(tokens.states[iden_i] == TOKEN_NAME) {
					*token_i += 1;
					AST* iden = reserve_var(node_list, tokens.strings[iden_i], tokens.lines[iden_i], tokens.columns[iden_i], tokens.sizes[iden_i]);
					bind_node->bind.iden = iden;

					int32 arg_i = *token_i;
					if(tokens.states[arg_i] == TOKEN_NAME) {
						*token_i += 1;
						AST* fn_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
						fn_node->part = PART_FN;
						bind_node->bind.right = fn_node;
						AST* arg = reserve_var(node_list, tokens.strings[arg_i], tokens.lines[arg_i], tokens.columns[arg_i], tokens.sizes[arg_i]);
						fn_node->fn.arg = arg;
						if(eat_token(tokens, token_i, TOKEN_BINOP, BINOP_EQ)) {
							fn_node->fn.body = parse_expn(source, tokens, token_i, node_list, error_msg);
							if(!fn_node->fn.body) return 0;
						} else {
							msg_unexpected(source, tokens, *token_i, "\"=\"", error_msg);
							return 0;
						}
					} else {
						msg_unexpected(source, tokens, *token_i, "an identifier", error_msg);
						return 0;
					}
					if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_AND)) {
						AST* new_node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
						bind_node->part = PART_MULTIDECL;
						bind_node->bind.block = new_node;
						bind_node = new_node;
					} else break;
				} else {
					msg_unexpected(source, tokens, *token_i, "an identifier", error_msg);
					return 0;
				}
			}
			bind_node->part = PART_DECL;
		} else {
			msg_unexpected(source, tokens, *token_i, "\"val\" or \"fun\"", error_msg);
			return 0;
		}
		if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_IN)) {
			bind_node->bind.block = parse_expn(source, tokens, token_i, node_list, error_msg);
			if(!bind_node->bind.block) return 0;
			if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_END)) {
				return top_node;
			} else {
				msg_unexpected(source, tokens, *token_i, "closing \"end\"", error_msg);
				return 0;
			}
		} else if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_END)) {
			bind_node->bind.block = parse_expn(source, tokens, token_i, node_list, error_msg);
			if(!bind_node->bind.block) return 0;
			return top_node;
		} else {
			msg_unexpected(source, tokens, *token_i, "\"in\"", error_msg);
			return 0;
		}
	} else if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_IF)) {
		AST* node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
		node->part = PART_COND;
		node->cond.is = parse_expn(source, tokens, token_i, node_list, error_msg);
		if(!node->cond.is) return 0;
		if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_THEN)) {
			node->cond.top = parse_expn(source, tokens, token_i, node_list, error_msg);
			if(!node->cond.top) return 0;
			if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_ELSE)) {
				node->cond.bot = parse_expn(source, tokens, token_i, node_list, error_msg);
				if(!node->cond.bot) return 0;
			}
		} else {
			msg_unexpected(source, tokens, *token_i, "closing \"then\"", error_msg);
			return 0;
		}
		//NOTE: AMBIGUITY!
		// if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_END)) {
			return node;
		// } else {
		// 	msg_unexpected(source, tokens, *token_i, "closing \"end\"", error_msg);
		// 	return 0;
		// }
	} else if(eat_token(tokens, token_i, TOKEN_KEYWORD, KEYWORD_FN)) {
		int32 arg_i = *token_i;
		if(tokens.states[arg_i] == TOKEN_NAME) {
			*token_i += 1;
			AST* node = reserve_node(node_list, tokens.lines[pre_token_i], tokens.columns[pre_token_i], tokens.sizes[pre_token_i]);
			node->part = PART_FN;
			node->fn.arg = reserve_node(node_list, tokens.lines[arg_i], tokens.columns[arg_i], tokens.sizes[arg_i]);
			node->fn.arg->part = PART_VAR;
			node->fn.arg->var.str = tokens.strings[arg_i];
			node->fn.arg->var.size = tokens.sizes[arg_i];
			if(eat_token(tokens, token_i, TOKEN_SYM, SYM_FN)) {
				node->fn.body = parse_expn(source, tokens, token_i, node_list, error_msg);
				if(!node->fn.body) return 0;
				return node;
			} else {
				msg_unexpected(source, tokens, *token_i, "\"=>\"", error_msg);
				return 0;
			}
		} else {
			msg_unexpected(source, tokens, *token_i, "an identifier", error_msg);
			return 0;
		}
	} else {
		return parse_prep(source, tokens, token_i, node_list, error_msg);
	}
}

AST* parse_all(Source source, Tokens tokens, char** error_msg, NodeList* tree_mem) {
	int32 token_i = 0;
	tree_mem->size = 0;
	tree_mem->head = (NodeBuffer*)malloc(sizeof(NodeBuffer));
	tree_mem->head->size = 0;
	tree_mem->head->next = 0;
	AST* root = parse_expn(source, tokens, &token_i, tree_mem, error_msg);
	if(root) {
		if(tokens.states[token_i] == TOKEN_EOF) {
			return root;
		} else {
			msg_unexpected(source, tokens, token_i, "end of file", error_msg);
			return 0;
		}
	}
	return root;
	//NOTE: Nothing is freed!
}


void destroy_tree(NodeList* tree_mem) {
	NodeBuffer* head = tree_mem->head;
	while(head) {
		NodeBuffer* cur = head;
		head = head->next;
		free(cur);
	}
}
void destroy_tokens(Tokens tokens) {
	tape_destroy(&tokens.states);
	tape_destroy(&tokens.strings);
	tape_destroy(&tokens.sizes);
	tape_destroy(&tokens.lines);
	tape_destroy(&tokens.columns);
}


void print_tree_basic(AST* root) {
	if(root->part == PART_NUMERAL) {
		printf("%d", root->numeral);
	} else if(root->part == PART_VAR) {
		printf("%.*s", root->var.size, root->var.str);
	} else if(is_binop(root->part)) {
		printf("(");
		print_tree_basic(root->binop.left);
		if(root->part == PART_ADD) {
			printf(BINOP_ADD);
		} else if(root->part == PART_SUB) {
			printf(BINOP_SUB);
		} else if(root->part == PART_MULT) {
			printf(BINOP_MULT);
		} else if(root->part == PART_DIV) {
			printf(BINOP_DIV);
		} else if(root->part == PART_MOD) {
			printf("%%");
		} else if(root->part == PART_EQ) {
			printf(BINOP_EQ);
		} else if(root->part == PART_LT) {
			printf(BINOP_LT);
		} else if(root->part == PART_GT) {
			printf(BINOP_GT);
		} else {
			printf("??");
		}
		print_tree_basic(root->binop.right);
		printf(")");
	}  else if(root->part == PART_COND) {
		printf("if ");
		print_tree_basic(root->cond.is);
		printf(" then\n");
		print_tree_basic(root->cond.top);
		if(root->cond.bot) {
			printf("\nelse\n");
			print_tree_basic(root->cond.bot);
		}
	} else if(root->part == PART_BIND) {
		printf("let var ");
		print_tree_basic(root->bind.iden);
		printf(" = ");
		print_tree_basic(root->bind.right);
		printf(" in\n");
		print_tree_basic(root->bind.block);
		printf("\nend");
	}
}

static int32 print_tree_dim_(AST* root, int32** line_queue, int32* width_start, int32* width_end) {
	int32 start;
	int32 end;
	int32 queue_i = tape_size(line_queue);
	if(root->part == PART_NUMERAL) {
		int32 width = cast(int32, log10(max(root->numeral, 1))) + 1;
		start = -(width - 1)/2;
		end = width/2;
	} else if(root->part == PART_VAR) {
		ASSERT(root->var.size > 0);
		int32 width = root->var.size;
		start = -(width - 1)/2;
		end = width/2;
	} else if(root->part == PART_BOOL) {
		start = 0;
		end = 0;
	} else if(root->part == PART_UNIT) {
		start = 0;
		end = 1;
	} else {
		int32 total_leafs;
		int32 pad[3] = {};
		AST** leafs = root->children;
		if(root->part == PART_COND) {
			if(root->cond.bot) {
				total_leafs = 3;
			} else {
				total_leafs = 2;
			}
			pad[0] = strlen(KEYWORD_IF);
			pad[1] = strlen(KEYWORD_ELSE);
		} else if(root->part == PART_BIND || root->part == PART_DECL || root->part == PART_MULTIDECL) {
			total_leafs = 3;
			pad[0] = 1;
			pad[1] = 2;
		} else if(root->part == PART_FN) {
			total_leafs = 2;
			pad[0] = 2;
		} else if(is_binop(root->part)) {
			total_leafs = 2;
			if(root->part == PART_AND) {
				pad[0] = strlen(BINOP_AND);
			} else if(root->part == PART_OR) {
				pad[0] = strlen(BINOP_OR);
			} else if(root->part == PART_PREP || root->part == PART_EXP || root->part == PART_LOG) {
				pad[0] = 2;
			} else {
				pad[0] = 1;
			}
		} else if(is_unary(root->part)) {
			total_leafs = 1;
			pad[0] = 3;
		} else ASSERT(0);
		int32 height = 1;
		int32 leaf_starts[4] = {};
		int32 leaf_ends[4] = {};
		tape_reserve(line_queue, total_leafs);
		for_each_lt(i, total_leafs) {
			height += print_tree_dim_(leafs[i], line_queue, &leaf_starts[i], &leaf_ends[i]);
		}
		if(total_leafs > 1) {
			int32 leaf_column[4] = {};
			leaf_column[0] = -(pad[0] + 1)/2;
			int32 extra = 1;
			for_each_lt(i, total_leafs - 1) {
				extra += (i < total_leafs - 2);
				leaf_column[i + 1] = leaf_column[i] + pad[i] + extra;
				extra = (i < total_leafs - 2 ? 2 : 1);
			}
			int32 overlap[3];
			for_each_lt(i, total_leafs - 1) {
			//find amount tree would be intersecting itself
				int32 line_pad = 2;
				int32 edge_pad = 1;
				int32 leftmost_start = leaf_starts[0] + leaf_column[0];
				int32 left_end = leaf_ends[i] + leaf_column[i];
				int32 right_start = leaf_starts[i + 1] + leaf_column[i + 1];
				overlap[i] = max(left_end - leaf_column[i + 1] + line_pad, leftmost_start - right_start + edge_pad);
			}
			for_each_lt(i, total_leafs - 1) {
				if(overlap[i] > 0) {//tree would be intersecting itself by this much
					if(i == 0) {
						int32 ld = (overlap[i] + 1)/2;
						if(ld > 0) {
							for_each_in_range(j, 0, i) leaf_column[j] -= ld;
							overlap[i] -= ld;
						}
					}
					for_each_in_range(j, i + 1, total_leafs - 1) leaf_column[j] += overlap[i];
				}
			}
			for_each_lt(i, total_leafs) {
				(*line_queue)[queue_i + i] = leaf_column[i];
			}
			*width_start = leaf_starts[0] + leaf_column[0];
			*width_end = leaf_ends[total_leafs - 1] + leaf_column[total_leafs - 1];
		} else {
			(*line_queue)[queue_i] = 0;
			*width_start = min(leaf_starts[0], -(pad[0] + 1)/2);
			*width_end = max(leaf_ends[0], (pad[0] + 2)/2);
		}
		return height;
	}
	tape_reserve(line_queue, 2);
	(*line_queue)[queue_i] = start;
	(*line_queue)[queue_i + 1] = end;
	ASSERT(((*line_queue)[queue_i] + (*line_queue)[queue_i + 1])/2 == 0);
	*width_start = start;
	*width_end = end;
	return 1;
}

static void print_tree_(AST* root, int32* line, int32 column, char* text, int32 width, int32* line_queue, int32* queue_i) {
	char* text_line = &text[(*line)*width];
	*line += 1;

	if(root->part == PART_NUMERAL) {
		int32 left = line_queue[*queue_i] + column;
		*queue_i += 1;
		int32 right = line_queue[*queue_i] + column;
		*queue_i += 1;
		ASSERT(right < width - 1);
		ASSERT(left >= 0);
		int32 n = root->numeral;
		do {
			text_line[right] = (n%10) + '0';
			n /= 10;
			right -= 1;
		} while(n > 0);
	} else if(root->part == PART_VAR) {
		int32 left = line_queue[*queue_i] + column;
		*queue_i += 1;
		int32 right = line_queue[*queue_i] + column;
		*queue_i += 1;
		ASSERT(right < width - 1);
		ASSERT(left >= 0);
		for_each_in(ch, root->var.str, root->var.size) {
			text_line[left] = *ch;
			left += 1;
		}
	} else if(root->part == PART_BOOL) {
		int32 left = line_queue[*queue_i] + column;
		*queue_i += 1;
		int32 right = line_queue[*queue_i] + column;
		*queue_i += 1;
		ASSERT(right < width - 1);
		ASSERT(left >= 0);
		text_line[left] = root->boolean + '0';
	} else if(root->part == PART_UNIT) {
		int32 left = line_queue[*queue_i] + column;
		*queue_i += 1;
		int32 right = line_queue[*queue_i] + column;
		*queue_i += 1;
		ASSERT(right < width - 1);
		ASSERT(left >= 0);
		text_line[left] = '(';
		text_line[right] = ')';
	} else if(has_children(root->part)) {
		int32 total_leafs = 0;
		AST** leafs = root->children;
		int32 columns[4] = {};
		char* divider[3] = {};

		#define if_is_binop(binop) if(root->part == MACRO_CAT(PART_, binop)) {total_leafs = 2; divider[0] = MACRO_CAT(BINOP_, binop);}
		if_is_binop(ADD)
		else if_is_binop(SUB)
		else if_is_binop(MULT)
		else if_is_binop(DIV)
		else if_is_binop(MOD)
		else if_is_binop(EQ)
		else if_is_binop(LT)
		else if_is_binop(GT)
		else if_is_binop(EXP)
		else if_is_binop(LOG)
		else if_is_binop(AND)
		else if_is_binop(OR)
		else if_is_binop(SEQ)
		else if_is_binop(APP)
		else if_is_binop(PREP)
		else {
			if(root->part == PART_COND) {
				total_leafs = 2;
				if(root->cond.bot) {
					total_leafs = 3;
					divider[1] = KEYWORD_ELSE;
				}
				divider[0] = KEYWORD_IF;
			} else if(root->part == PART_BIND || root->part == PART_DECL || root->part == PART_MULTIDECL) {
				total_leafs = 3;
				divider[0] = BINOP_EQ;
				divider[1] = KEYWORD_IN;
			} else if(root->part == PART_FN) {
				total_leafs = 2;
				divider[0] = SYM_FN;
			} else {
				ASSERT(0);
			}
		}
		ASSERT(total_leafs > 0);
		for_each_lt(i, total_leafs) {
			columns[i] = line_queue[*queue_i] + column;
			*queue_i += 1;
			ASSERT(columns[i] < width - 1);
			ASSERT(columns[i] >= 0);
		}
		for_each_in_range(i, columns[0] + 1, columns[total_leafs - 1] - 1) {
			text_line[i] = '_';
		}
		// ASSERT((columns[0] + columns[1])/2 == column);
		if(total_leafs == 1) {
			int32 size = strlen(divider[0]);
			int32 cur_pos = column - (size - 1)/2;
			text_line[cur_pos - 1] = '<';
			for_each_in(ch, divider[0], size) {
				text_line[cur_pos] = *ch;
				cur_pos += 1;
			}
			text_line[cur_pos] = '>';
		} else {
			for_each_lt(i, total_leafs - 1) {
				int32 size = strlen(divider[i]);
				int32 cur_pos = (columns[i] + columns[i + 1])/2 - (size - 1)/2;
				if(i == 0) cur_pos = column - (size - 1)/2;
				text_line[cur_pos - 1] = '/';
				for_each_in(ch, divider[i], size) {
					text_line[cur_pos] = *ch;
					cur_pos += 1;
				}
				text_line[cur_pos] = '\\';
			}
		}

		int32 pre_column = 0;
		int32 pre_line = *line;
		for_each_lt(i, total_leafs) {
			int32 next_line = *line;
			int32 cur_column = columns[i];
			print_tree_(leafs[i], line, cur_column, text, width, line_queue, queue_i);
			if(i > 0) {
				for_each_in_range(j, pre_line, next_line - 1) {
					text[j*width + cur_column] = '|';
				}
				if(i >= total_leafs - 1 && cur_column - pre_column > strlen(divider[i - 1]) + 2) {
					text[pre_line*width + cur_column] = '\\';
				}
			}
			pre_column = cur_column;
		}
	} else ASSERT(0);
}
void print_tree(AST* root) {
	int32 width_left;
	int32 width_right;
	int32* line_queue = 0;
	int32 height = print_tree_dim_(root, &line_queue, &width_left, &width_right);
	int32 width = width_right - width_left + 2;
	char* text = (char*)malloc(width*height + 1);
	memset(text, ' ', width*height);

	for_each_lt(i, height) {
		text[(i + 1)*width - 1] = '\n';
	}
	text[width*height] = 0;
	int32 line = 0;
	int32 queue_i = 0;
	print_tree_(root, &line, -width_left, text, width, line_queue, &queue_i);

	printf("%s", text);
	free(text);
}

#endif
