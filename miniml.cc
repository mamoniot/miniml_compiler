/*
Author: Monica Moniot
Login:  mamoniot
GitHub: mamoniot

Compile only this .cc file with your preferred compiler. Run it with hw03.mml source files on the command line to see the homework functions completed in miniml.

I decided I wanted to compile miniml to a simple bytecode for a large boost in speed. There are no run time variable dereferences, and bytecode instructions are much easier to manage in the cache than AST nodes (although admittedly I haven't yet optimized cache usage). Bytecode requires less recursion too, which is better for the instruction cache.

I made sure not to sacrifice good error messages for this, each bytecode has a matching column and line number for looking it up and reporting an error at that position if necessary.

The program leaks runtime memory instead of freeing it, I will have to build a garbage collector for the entire language to fix this. I also want to overhaul the call convention of this bytecode, because the current version was hacked together for the sake of time.

I've been busy all week since getting back from Tapia, so I have barely any time to write documentation, but the homework is completed in hw03.mml with slightly different syntax from sml.
*/

//TODO: Leaks, leaks everywhere
#include "basic.h"
#define PARSER_IMPLEMENTATION
#include "parser.hh"

enum Type : int32 {
	TYPE_BOT = 0,
	TYPE_UNIT,
	TYPE_BOOLEAN,
	TYPE_INTEGER,
	TYPE_CLOSURE,
	TYPE_BUILTIN,
	TYPE_LIST,
};

enum Builtin : int32 {
	BUILTIN_PRINT,
	BUILTIN_NOT,
	BUILTIN_NULL,
	BUILTIN_HD,
	BUILTIN_TL,
};

struct Value {
	Type type;
	union {
		int32 integer;
		int32 boolean;
		char* msg;
		struct {
			Value* regs;
			int32 tag;
			int32 arg_reg;
		} closure;
		struct {
			Value* ptr;
			int32 size;
		} list;
		Builtin builtin_id;
	};
};

enum OPCode : int32 {
	OP_NULL = 0,
	OP_NUMERAL = PART_NUMERAL,
	OP_BOOL = PART_BOOL,
	OP_UNIT = PART_UNIT,
	OP_ADD = PART_ADD, OP_SUB = PART_SUB,
	OP_MULT = PART_MULT, OP_DIV = PART_DIV, OP_MOD = PART_MOD,
	OP_EQ = PART_EQ, OP_LT = PART_LT, OP_GT = PART_GT,
	OP_APP = PART_APP,
	OP_PREP = PART_PREP,
	OP_AND = PART_AND, OP_OR = PART_OR,
	OP_EXP = PART_EXP, OP_LOG = PART_LOG,
	OP_FN = PART_FN,
	OP_COND = PART_COND,
	OP_COND_INV = PART_LAST,
	OP_FN_CLOSE,
	OP_JUMP,
	OP_RET,
	OP_TAG,
	OP_COPY,
};
bool is_tagged(OPCode op) {return op == PART_COND | op == OP_COND_INV | op == OP_JUMP | op == OP_FN;}

constexpr int32 REG_FN_RETURN = -1;
constexpr int32 REG_FIRST_TEMP_FREE = -2;
struct ByteCode {
	OPCode op;
	union {
		struct {
			int32 reg0;
			int32 reg1;
			int32 reg2;
		};
		struct {
			int32 _;
			int32 val;
		};
		struct {
			int32 _;
			int32 tag;
		};
	};
};

#define BYTECODE_BIN_SIZE KILOBYTE
struct ByteCodeBin {
	ByteCodeBin* next;
	int32 size;
	ByteCode codes[BYTECODE_BIN_SIZE];
	SourcePos sources[BYTECODE_BIN_SIZE];
};

constexpr int32 INIT_ENV_SIZE = 6;
static Value INIT_ENV[INIT_ENV_SIZE] = {{TYPE_BUILTIN, BUILTIN_PRINT}, {TYPE_BUILTIN, BUILTIN_NOT}, {TYPE_LIST}, {TYPE_BUILTIN, BUILTIN_NULL}, {TYPE_BUILTIN, BUILTIN_HD}, {TYPE_BUILTIN, BUILTIN_TL}};
static char* INIT_ENV_STRS[INIT_ENV_SIZE] = {"print", "not", "nil", "null", "hd", "tl"};

struct ByteCodeState {
	int32 code_size;
	ByteCodeBin* cur_pos;
	int32 cur_tag;
	int32 cur_temp_reg;
	int32 last_temp_reg;
	int32 last_var_reg;
	char** var_strs;
	int32* var_sizes;
};

void write_bytecode(ByteCodeState* state, ByteCode code, SourcePos source) {
	ByteCodeBin* bin = state->cur_pos;
	if(bin->size >= BYTECODE_BIN_SIZE) {
		ByteCodeBin* new_bin = talloc(ByteCodeBin, 1);
		memzero(new_bin, sizeof(ByteCodeBin));
		new_bin->next = bin->next;
		bin->next = new_bin;

		new_bin->codes[0] = code;
		new_bin->sources[0] = source;
		new_bin->size += 1;
		state->cur_pos = new_bin;
	} else {
		bin->codes[bin->size] = code;
		bin->sources[bin->size] = source;
		bin->size += 1;
	}
	state->code_size += 1;
}
void write_tag(ByteCodeState* state, int32 tag) {
	ByteCode code = {OP_TAG};
	code.tag = tag;
	SourcePos source = {};
	write_bytecode(state, code, source);
}


bool compile_expn(Source source, ByteCodeState* state, AST* root, int32 ret_reg, char** msg);

bool compile_fn_(Source source, ByteCodeState* state, AST* root, int32 ret_reg, char** msg) {
	ByteCode code = {OP_FN};
	ByteCode code_ret = {OP_RET};
	int32 tag = state->cur_tag;
	state->cur_tag += 1;
	code.reg0 = ret_reg;
	code.tag = tag;
	int32 var_reg = tape_size(&state->var_strs);
	state->last_var_reg = max(state->last_var_reg, var_reg);
	code.reg2 = var_reg;

	tape_push(&state->var_strs, root->fn.arg->var.str);
	tape_push(&state->var_sizes, root->fn.arg->var.size);
	{
		//prep state and stack
		ByteCodeBin* new_bin = talloc(ByteCodeBin, 1);
		ByteCodeBin* cur_bin = state->cur_pos;
		memzero(new_bin, sizeof(ByteCodeBin));
		new_bin->next = cur_bin->next;
		cur_bin->next = new_bin;

		int32 pre_temp_reg = state->cur_temp_reg;
		state->cur_pos = new_bin;
		state->cur_temp_reg = REG_FIRST_TEMP_FREE;

		//recurse
		write_tag(state, tag);
		if(!compile_expn(source, state, root->fn.body, REG_FN_RETURN, msg)) return 0;
		write_bytecode(state, code_ret, root->source);

		//restore state from stack
		state->cur_pos = cur_bin;
		state->cur_temp_reg = pre_temp_reg;
	}
	tape_pop(&state->var_strs);
	tape_pop(&state->var_sizes);

	write_bytecode(state, code, root->source);
	return 1;
}

bool compile_expn(Source source, ByteCodeState* state, AST* root, int32 ret_reg, char** msg) {
	Part part = root->part;
	if(part == PART_NUMERAL) {
		ByteCode code = {OP_NUMERAL};
		code.reg0 = ret_reg;
		code.val = root->numeral;
		write_bytecode(state, code, root->source);
	} else if(part == PART_BOOL) {
		ByteCode code = {OP_BOOL};
		code.reg0 = ret_reg;
		code.val = root->boolean;
		write_bytecode(state, code, root->source);
	} else if(part == PART_UNIT) {
		ByteCode code = {OP_UNIT};
		code.reg0 = ret_reg;
		write_bytecode(state, code, root->source);
	} else if(part == PART_VAR) {
		ByteCode code = {OP_COPY};
		code.reg0 = ret_reg;
		char* str = root->var.str;
		int32 size = root->var.size;
		bool is = 0;
		for_each_lt_bw(i, tape_size(&state->var_strs)) {
			if(strcmp(state->var_strs[i], state->var_sizes[i], str, size)) {
				is = 1;
				code.reg1 = i;
				break;
			}
		}
		//NOTE: add error handling here
		if(is) {
			write_bytecode(state, code, root->source);
		} else {
			msg_unexpected(source, root->source, "Compilation Error", "undeclared variable", 0, msg);
			return 0;
		}
	} else if(part == PART_BIND) {
		int32 var_reg = tape_size(&state->var_strs);
		state->last_var_reg = max(state->last_var_reg, var_reg);

		tape_push(&state->var_strs, 0);
		tape_push(&state->var_sizes, 0);
		if(!compile_expn(source, state, root->bind.right, var_reg, msg)) return 0;

		state->var_strs[var_reg] = root->bind.iden->var.str;
		state->var_sizes[var_reg] = root->bind.iden->var.size;
		if(!compile_expn(source, state, root->bind.block, ret_reg, msg)) return 0;
		tape_pop(&state->var_strs);
		tape_pop(&state->var_sizes);
	} else if(part == PART_DECL || part == PART_MULTIDECL) {
		int32 var_reg_first = tape_size(&state->var_strs);
		AST* node = root;
		while(1) {
			tape_push(&state->var_strs, node->bind.iden->var.str);
			tape_push(&state->var_sizes, node->bind.iden->var.size);
			if(node->part == PART_DECL) break;
			node = node->bind.block;
		}
		int32 var_reg_last = tape_size(&state->var_strs) - 1;
		state->last_var_reg = max(state->last_var_reg, var_reg_last);

		node = root;
		int32 i = 0;
		while(1) {
			ASSERT(node->bind.right->part == PART_FN);
			if(!compile_fn_(source, state, node->bind.right, var_reg_first + i, msg)) return 0;
			if(node->part == PART_DECL) break;
			node = node->bind.block;
			i += 1;
		}
		node = root;
		i = 0;
		while(1) {
			ByteCode code_close = {OP_FN_CLOSE};
			code_close.reg0 = var_reg_first + i;
			write_bytecode(state, code_close, root->source);
			if(node->part == PART_DECL) break;
			node = node->bind.block;
			i += 1;
		}
		if(!compile_expn(source, state, node->bind.block, ret_reg, msg)) return 0;

		node = root;
		while(1) {
			tape_pop(&state->var_strs);
			tape_pop(&state->var_sizes);
			if(node->part == PART_DECL) break;
			node = node->bind.block;
		}
	} else if(part == PART_FN) {
		ByteCode code_close = {OP_FN_CLOSE};
		code_close.reg0 = ret_reg;
		if(!compile_fn_(source, state, root, ret_reg, msg)) return 0;
		write_bytecode(state, code_close, root->source);
	} else if(part == PART_COND) {
		if(!root->cond.bot) {
			ByteCode code = {OP_COND};
			int32 tag = state->cur_tag;
			state->cur_tag += 1;
			code.reg0 = ret_reg;
			code.tag = tag;

			if(!compile_expn(source, state, root->cond.is, code.reg0, msg)) return 0;
			write_bytecode(state, code, root->source);
			if(!compile_expn(source, state, root->cond.top, code.reg0, msg)) return 0;
			write_tag(state, tag);
		} else {
			ByteCode code = {OP_COND};
			ByteCode code_jump = {OP_JUMP};
			int32 tag0 = state->cur_tag;
			state->cur_tag += 1;
			int32 tag1 = state->cur_tag;
			state->cur_tag += 1;

			code.reg0 = ret_reg;
			code.tag = tag0;
			code_jump.tag = tag1;

			if(!compile_expn(source, state, root->cond.is, code.reg0, msg)) return 0;
			write_bytecode(state, code, root->source);
			if(!compile_expn(source, state, root->cond.top, code.reg0, msg)) return 0;
			write_bytecode(state, code_jump, root->source);
			write_tag(state, tag0);
			if(!compile_expn(source, state, root->cond.bot, code.reg0, msg)) return 0;
			write_tag(state, tag1);
		}
	} else if(part == PART_SEQ) {
		if(!compile_expn(source, state, root->binop.left, ret_reg, msg)) return 0;
		if(!compile_expn(source, state, root->binop.right, ret_reg, msg)) return 0;
	} else if(is_leftasc(part)) {
		ByteCode code = {cast(OPCode, part)};
		code.reg0 = ret_reg;
		code.reg1 = state->cur_temp_reg;
		state->last_temp_reg = min(state->last_temp_reg, state->cur_temp_reg);
		state->cur_temp_reg -= 1;

		if(!compile_expn(source, state, root->binop.left, code.reg0, msg)) return 0;
		if(!compile_expn(source, state, root->binop.right, code.reg1, msg)) return 0;
		write_bytecode(state, code, root->source);
	} else if(is_rightasc(part)) {
   		ByteCode code = {cast(OPCode, part)};
   		code.reg0 = ret_reg;
		code.reg1 = state->cur_temp_reg;
		state->last_temp_reg = min(state->last_temp_reg, state->cur_temp_reg);
		state->cur_temp_reg -= 1;

   		if(!compile_expn(source, state, root->binop.right, code.reg1, msg)) return 0;
   		if(!compile_expn(source, state, root->binop.left, code.reg0, msg)) return 0;
   		write_bytecode(state, code, root->source);
   	} else if(part == PART_AND || part == PART_OR) {
		ByteCode code_cond = {part == PART_AND ? OP_COND : OP_COND_INV};
		int32 tag = state->cur_tag;
		state->cur_tag += 1;
		ByteCode code = {cast(OPCode, part)};
		code_cond.reg0 = ret_reg;
		code_cond.tag = tag;
		code.reg0 = ret_reg;
		code.reg1 = state->cur_temp_reg;
		state->last_temp_reg = min(state->last_temp_reg, state->cur_temp_reg);
		state->cur_temp_reg -= 1;

		if(!compile_expn(source, state, root->binop.left, code.reg0, msg)) return 0;
		write_bytecode(state, code_cond, root->source);
		if(!compile_expn(source, state, root->binop.right, code.reg1, msg)) return 0;
		write_bytecode(state, code, root->source);
		write_tag(state, tag);
	} else ASSERT(0);
	return 1;
}

struct Program {
	Source source;
	int32 code_size;
	ByteCode* bytecode;
	SourcePos* code_sources;
	Value* init_env;
	int32 init_env_size;
	int32 last_var_reg;
	int32 last_temp_reg;
	char* error_msg;
	int32 error_msg_size;
};

Program compile(Source source, AST* root) {
	ByteCodeState state = {};
	ByteCodeBin* head = talloc(ByteCodeBin, 1);
	{
		memzero(head, sizeof(ByteCodeBin));
		state.code_size = 0;
		state.cur_pos = head;
		state.cur_tag = 0;
		state.cur_temp_reg = REG_FIRST_TEMP_FREE;
		state.last_temp_reg = REG_FIRST_TEMP_FREE;
		for_each_in(str, INIT_ENV_STRS, INIT_ENV_SIZE) {
			tape_push(&state.var_strs, *str);
			tape_push(&state.var_sizes, strlen(*str));
		}
		state.last_var_reg = INIT_ENV_SIZE - 1;
	}
	char* msg = 0;
	bool success = compile_expn(source, &state, root, REG_FN_RETURN, &msg);
	{
		tape_destroy(&state.var_strs);
		tape_destroy(&state.var_sizes);
	}
	if(!success) {
		Program program = {};
		program.error_msg = msg;
		program.error_msg_size = tape_size(&msg);
		return program;
	}
	ByteCode code_ret = {OP_RET};
	write_bytecode(&state, code_ret, root->source);

	ByteCodeBin* bin = head;
	int32 max_tag = state.cur_tag;
	int32* tag_tab = talloc(int32, max_tag);
	int32 code_size = state.code_size - max_tag + 1;
	ByteCode* bytecode = talloc(ByteCode, code_size);
	SourcePos* sources = talloc(SourcePos, code_size);
	memzero(tag_tab, sizeof(int32)*max_tag);
	memzero(bytecode, sizeof(ByteCode)*code_size);
	memzero(sources, sizeof(SourcePos)*code_size);
	int32 i = 1;
	while(bin) {
		ASSERT(i + bin->size <= state.code_size + 1);
		for_each_lt(j, bin->size) {
			auto code = bin->codes[j];
			if(code.op == OP_TAG) {
				//do not write tags to the final byte code stream
				ASSERT(code.tag < max_tag);
				ASSERT(!tag_tab[code.tag]);
				tag_tab[code.tag] = i;
			} else {
				bytecode[i] = code;
				sources[i] = bin->sources[j];
				i += 1;
			}
		}
		ByteCodeBin* t = bin;
		bin = bin->next;
		free(t);
	}
	ASSERT(i == code_size);

	//convert tags to absolute position
	for_each_lt(i, code_size) {
		auto code = &bytecode[i];
		if(is_tagged(code->op)) {
			ASSERT(code->tag < max_tag);
			ASSERT(tag_tab[code->tag]);
			code->tag = tag_tab[code->tag];
		}
	}
	free(tag_tab);
	Program program = {source};
	program.code_size = code_size;
	program.bytecode = bytecode;
	program.code_sources = sources;
	program.init_env = INIT_ENV;
	program.init_env_size = INIT_ENV_SIZE;
	program.last_var_reg = state.last_var_reg;
	program.last_temp_reg = state.last_temp_reg;
	return program;
}

bool assert_type(Program program, int32 i, Value val, Type desired, char** msg) {
	if(val.type == desired) return 1;
	char* unexpected = 0;
	if(val.type == TYPE_BOOLEAN) {
		unexpected = "boolean value";
	} else if(val.type == TYPE_INTEGER) {
		unexpected = "integer value";
	} else if(val.type == TYPE_CLOSURE || val.type == TYPE_BUILTIN) {
		unexpected = "function";
	} else if(val.type == TYPE_UNIT) {
		unexpected = "unit value";
	} else if(val.type == TYPE_LIST) {
		unexpected = "list value";
	} else {
		ASSERT(0);
		unexpected = "unknown value";
	}
	char* expected = 0;
	if(desired == TYPE_BOOLEAN) {
		expected = "a boolean value";
	} else if(desired == TYPE_INTEGER) {
		expected = "an integer value";
	} else if(desired == TYPE_CLOSURE) {
		expected = "a function";
	} else if(desired == TYPE_UNIT) {
		expected = "a unit value";
	} else if(desired == TYPE_LIST) {
		expected = "a list value";
	} else if(desired == TYPE_BOT) {
		expected = "some other value";
	} else ASSERT(0);
	ASSERT(unexpected && expected);
	msg_unexpected(program.source, program.code_sources[i], "Runtime Type Error", unexpected, expected, msg);
	return 0;
}

void builtin_printf(Value right) {
	if(right.type == TYPE_UNIT) {
		printf("()");
	} else if(right.type == TYPE_BOOLEAN) {
		if(right.boolean) {
			printf("true");
		} else {
			printf("false");
		}
	} else if(right.type == TYPE_INTEGER) {
		printf("%d", right.integer);
	} else if(right.type == TYPE_LIST) {
		printf("[");
		for_each_lt_bw(i, right.list.size) {
			builtin_printf(right.list.ptr[i]);
			if(i > 0) printf(", ");
		}
		printf("]");
	} else if(right.type == TYPE_CLOSURE || right.type == TYPE_BUILTIN) {
		printf("function");
	} else ASSERT(0);
}
void builtin_print(Value right) {
	builtin_printf(right);
	printf("\n");
}


struct StackFrame {
	int32 ret_i;
	int32 ret_reg;
	Value* pre_regs;
	Value regs[1];
};
void execute(Program program) {
	byte* stack = talloc(byte, MEGABYTE);
	int32 stack_size = 0;
	int32 total_temp_regs = -program.last_temp_reg;
	int32 i = 1;
	int32 total_regs = program.last_var_reg - program.last_temp_reg + 1;
	int32 frame_size = (sizeof(StackFrame) + sizeof(Value)*(total_regs - 1));
	int32 null_reg = total_temp_regs;
	Value* regs = talloc(Value, total_regs);
	memzero(regs, sizeof(Value)*total_regs);
	for_each_lt(i, program.init_env_size) {
		regs[i + null_reg] = program.init_env[i];
	}
	char* error = 0;
	Value program_ret = {};
	while(1) {
		auto code = program.bytecode[i];
		if(code.op == OP_NUMERAL) {
			Value val = {TYPE_INTEGER};
			val.integer = code.val;
			regs[null_reg + code.reg0] = val;
		} else if(code.op == OP_BOOL) {
			Value val = {TYPE_BOOLEAN};
			val.integer = code.val;
			regs[null_reg + code.reg0] = val;
		} else if(code.op == OP_UNIT) {
			Value val = {TYPE_UNIT};
			regs[null_reg + code.reg0] = val;
		} else if(code.op == OP_COPY) {
			regs[null_reg + code.reg0] = regs[null_reg + code.reg1];
		} else if(code.op == OP_FN) {
			Value val = {TYPE_CLOSURE};
			val.closure.tag = code.tag;
			val.closure.arg_reg = code.reg2;
			val.closure.regs = talloc(Value, total_regs);
			memzero(val.closure.regs, sizeof(Value)*total_regs);
			regs[null_reg + code.reg0] = val;
		} else if(code.op == OP_FN_CLOSE) {
			Value val = regs[null_reg + code.reg0];
			memcpy(&val.closure.regs[null_reg], &regs[null_reg], sizeof(Value)*(total_regs - null_reg));
		} else if(code.op == OP_JUMP) {
			i = code.tag;
			continue;
		} else if(code.op == OP_COND || code.op == OP_COND_INV) {
			Value cond = regs[null_reg + code.reg0];
			if(assert_type(program, i, cond, TYPE_BOOLEAN, &error)) {
				if(cond.boolean ^ code.op == OP_COND) {
					i = code.tag;
					continue;
				}
			} else break;
		} else if(code.op == OP_RET) {
			if(stack_size > 0) {
				stack_size -= frame_size;
				StackFrame* frame = cast(StackFrame*, &stack[stack_size]);

				i = frame->ret_i;
				Value t = regs[null_reg + REG_FN_RETURN];
				memcpy(frame->pre_regs, frame->regs, sizeof(Value)*total_regs);
				frame->pre_regs[null_reg + frame->ret_reg] = t;
				regs = frame->pre_regs;
				continue;
			} else {
				program_ret = regs[null_reg + REG_FN_RETURN];
				break;
			}
		} else if(code.op == OP_APP) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(left.type == TYPE_BUILTIN) {
				Value ret = {};
				if(left.builtin_id == BUILTIN_PRINT) {
					builtin_print(right);
					ret.type = TYPE_UNIT;
				} else if(left.builtin_id == BUILTIN_NOT) {
					if(assert_type(program, i, right, TYPE_BOOLEAN, &error)) {
						ret.type = TYPE_BOOLEAN;
						ret.boolean = 1^right.boolean;
					} else break;
				} else if(left.builtin_id == BUILTIN_NULL) {
					if(assert_type(program, i, right, TYPE_LIST, &error)) {
						ret.type = TYPE_BOOLEAN;
						ret.boolean = right.list.size == 0;
					} else break;
				} else if(left.builtin_id == BUILTIN_HD) {
					if(assert_type(program, i, right, TYPE_LIST, &error)) {
						if(right.list.size > 0) {
							ret = right.list.ptr[right.list.size - 1];
						} else {
							//TODO: Improved error message
							assert_type(program, i, right, TYPE_BOT, &error);
							break;
						}
					} else break;
				} else if(left.builtin_id == BUILTIN_TL) {
					if(assert_type(program, i, right, TYPE_LIST, &error)) {
						if(right.list.size > 0) {
							right.list.size -= 1;
							ret = right;
						} else {
							assert_type(program, i, right, TYPE_BOT, &error);
							break;
						}
					} else break;
				} else ASSERT(0);
				regs[null_reg + code.reg0] = ret;
			} else if(assert_type(program, i, left, TYPE_CLOSURE, &error)) {
				StackFrame* frame = cast(StackFrame*, &stack[stack_size]);
				stack_size += frame_size;
				frame->ret_i = i + 1;
				frame->ret_reg = code.reg0;
				frame->pre_regs = regs;
				memcpy(frame->regs, frame->pre_regs, sizeof(Value)*total_regs);
				regs = left.closure.regs;
				i = left.closure.tag;
				regs[null_reg + left.closure.arg_reg] = right;
				continue;
			} else break;
		} else if(code.op == OP_PREP) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			Value ret = {TYPE_LIST};
			if(assert_type(program, i, right, TYPE_LIST, &error)) {
				ret.list.size = right.list.size + 1;
				//NOTE: leaking here
				ret.list.ptr = talloc(Value, right.list.size + 1);
				if(right.list.size > 0) memcpy(ret.list.ptr, right.list.ptr, sizeof(Value)*right.list.size);
				ret.list.ptr[right.list.size] = left;
			} else break;
			regs[null_reg + code.reg0] = ret;
		} else if(code.op == OP_ADD) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_INTEGER};
				ret.integer = left.integer + right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_SUB) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_INTEGER};
				ret.integer = left.integer - right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_MULT) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_INTEGER};
				ret.integer = left.integer * right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_DIV) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				if(right.integer != 0) {
					Value ret = {TYPE_INTEGER};
					ret.integer = left.integer / right.integer;
					regs[null_reg + code.reg0] = ret;
				} else {
					ASSERT(0);//TODO: proper error message
				}
			} else break;
		} else if(code.op == OP_MOD) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				if(right.integer != 0) {
					Value ret = {TYPE_INTEGER};
					ret.integer = left.integer % right.integer;
					regs[null_reg + code.reg0] = ret;
				} else {
					ASSERT(0);//TODO: proper error message
				}
			} else break;
		} else if(code.op == OP_EXP) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_INTEGER};
				ret.integer = cast(int32, pow(left.integer, right.integer));
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_LOG) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				if(right.integer != 0) {
					Value ret = {TYPE_INTEGER};
					ret.integer = cast(int32, log(left.integer)/log(right.integer));
					regs[null_reg + code.reg0] = ret;
				} else {
					ASSERT(0);//TODO: proper error message
				}
			} else break;
		} else if(code.op == OP_EQ) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_BOOLEAN};
				ret.boolean = left.integer == right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_LT) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_BOOLEAN};
				ret.boolean = left.integer < right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_GT) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_INTEGER, &error) && assert_type(program, i, right, TYPE_INTEGER, &error)) {
				Value ret = {TYPE_BOOLEAN};
				ret.boolean = left.integer > right.integer;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_AND) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_BOOLEAN, &error) && assert_type(program, i, right, TYPE_BOOLEAN, &error)) {
				Value ret = {TYPE_BOOLEAN};
				ret.boolean = left.boolean & right.boolean;
				regs[null_reg + code.reg0] = ret;
			} else break;
		} else if(code.op == OP_OR) {
			Value left = regs[null_reg + code.reg0];
			Value right = regs[null_reg + code.reg1];
			if(assert_type(program, i, left, TYPE_BOOLEAN, &error) && assert_type(program, i, right, TYPE_BOOLEAN, &error)) {
				Value ret = {TYPE_BOOLEAN};
				ret.boolean = left.boolean | right.boolean;
				regs[null_reg + code.reg0] = ret;
			} else break;
		}

		i += 1;
	}
	if(program_ret.type) {
		printf("\n = ");
		builtin_print(program_ret);
	} else {
		printf("%.*s", tape_size(&error), error);
	}
}


int main(int32 argc, char** argv) {
	//TODO: multiple source files?
	char* text = 0;
	if(argc > 1) {
		FILE* source_file = fopen(argv[1], "r");
		if(source_file) {
			int ch = fgetc(source_file);
			while(ch != EOF) {
				if(ch == '\t') {
					//TODO: fix tabs and remove this
					tape_push(&text, ' ');
					tape_push(&text, ' ');
					tape_push(&text, ' ');
					tape_push(&text, ' ');
				} else {
					tape_push(&text, cast(char, ch));
				}
				ch = fgetc(source_file);
			}
			fclose(source_file);
		} else {
			printf("Could not open file: %s.", argv[1]);
		}
	} else {
		while(1) {
	        int c = getchar();
	        if(c == EOF || c == '\n') {
				break;
			} else {
				tape_push(&text, c);
			}
	    }
	}

	if(text) {
		Source source = {};
		source.text = text;
		source.size = tape_size(&text);
		Tokens tokens = tokenize(&source);
		char* error_msg = 0;
		NodeList tree_mem;
		AST* root = parse_all(source, tokens, &error_msg, &tree_mem);
		if(root) {
			// print_tree(root);
			Program program = compile(source, root);
			if(program.error_msg) {
				printf("%.*s", program.error_msg_size, program.error_msg);
			} else {
				execute(program);
			}
		} else {
			ASSERT(error_msg && error_msg[0]);
			printf("%.*s", tape_size(&error_msg), error_msg);
		}
	}
	//NOTE: memory is not released before exit
	return 0;
}
