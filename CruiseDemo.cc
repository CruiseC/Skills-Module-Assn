// Cruise Costin
// CSE 340 Spr 23'
// Dr. Gordon

// 7 necessary header files
#include <cstdlib>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <algorithm>
#include <iostream>
#include <map>
#include "compiler.h"
#include "lexer.h"

// Standard namespace will be used 
using namespace std;

// Instantiate a LexicalAnalyzer object and a Token object
LexicalAnalyzer lexer; 
Token myToken;  

// Declare a map to store input variables and a pointer to a struct that represents the root of the intermediate representation
map<string, int> inputVars; 
struct InstructionNode* cruise; 

// Functions used to parse the grammar input
void parseProgram();
void parseVarSection();
void parseIdList();
void parseInputs();
void parseNumList();

int parsePrimary();

TokenType parseOperator();
TokenType parseRelationalOperator();

InstructionNode* parseBody();
InstructionNode* parseStmtList();
InstructionNode* parseStmt();
InstructionNode* parseAssignStmt();
InstructionNode* parseExpr();

InstructionNode* parseOutputStmt();
InstructionNode* parseInputStmt();
InstructionNode* parseWhileStmt();
InstructionNode* parseIfStmt();
InstructionNode* parseCondition();

InstructionNode* parseSwitchStmt(struct InstructionNode* endNode);
InstructionNode* parseForStmt();
InstructionNode* parseCaseList(int op, struct InstructionNode* endNode);
InstructionNode* parseCase(int op);
InstructionNode* parseDefaultCase();

// Define a function that returns the next token without consuming it
Token peek() {
    Token myToken = lexer.GetToken();
    lexer.UngetToken(1);
    return myToken;
}

// Define a function to print an error message and exit if a syntax error is encountered
void syntax_error() {
    cout << "SYNTAX ERROR !!!\n";
    exit(1);
}

// Define a function that consumes the next token and checks if it is of the expected type
Token expect(TokenType expected_type) {
    Token myToken = lexer.GetToken();
    if (myToken.token_type != expected_type)
        syntax_error();
    return myToken;
}

// Define the main function that generates intermediate representation of the program
struct InstructionNode * parse_generate_intermediate_representation()
{
    // Parse the program
    parseProgram();
    
    // Return a pointer to the root node of the intermediate representation
    return cruise;
}

// Define a function that parses the program by first parsing the variable section,
// then the body, and finally the inputs
void parseProgram() {
    
    // Parse variable section
    myToken = peek();
    if (myToken.token_type == ID) {
        parseVarSection();
    }
    else {
        syntax_error();
    }
    
    // Parse body
    myToken = peek();
    if (myToken.token_type == LBRACE) {
        cruise = parseBody();
    }
    else {
        syntax_error();
    }
    
    // Parse inputs
    myToken = peek();
    if (myToken.token_type == NUM) {
        parseInputs();
    }
    else {
        syntax_error();
    }
}

// Define a function that parses the variable section by first parsing the id list,
// and then expecting a semicolon at the end
void parseVarSection() {
    
    // Parse ID List
    parseIdList();
    
    // Expect semicolon
    expect(SEMICOLON);
}

// Define a function that parses a list of variables
// and stores them in a map with their corresponding index in memory
void parseIdList() {
    myToken = expect(ID);
    
    // Store variables to the map at the first line
    inputVars[myToken.lexeme] = next_available;
    
    // Initialize the var to 0 in mem and match with the index
    mem[next_available++] = 0;
    
    // Get the next token which should be either a comma or the end of the list
    myToken = lexer.GetToken();

    if (myToken.token_type == COMMA) {
        // If the next token is a comma, parse the next variable in the list recursively
        parseIdList();
    }
    
    else {
        // If the next token is not a comma, put it back into 
        // the lexer and return to the calling function
        lexer.UngetToken(1);
    }
}

// Define a function that parses a code block enclosed in braces
// Returns a pointer to the generated linked list of instructions
struct InstructionNode* parseBody() {
    struct InstructionNode* instructionList = nullptr;
    
    // Expect a left brace
    expect(LBRACE); 

    // Parse statement list
    instructionList = parseStmtList(); // Parse a list of statements
    expect(RBRACE); // Expect a right brace
    return instructionList;   // Return the generated linked list of instructions
}

struct InstructionNode* parseStmtList() {
    
    // Initialize pointer to the head and tail of the list
    struct InstructionNode* instruction = nullptr;
    struct InstructionNode* instructionList = nullptr;

    
    // Parse the first statement and set it as the head of the list
    instruction = parseStmt();

    // Peek at the next token
    myToken = peek();   
    
    // Check if the next token is a valid statement type, and if so, parse it and add it to the list
    if (myToken.token_type == ID || myToken.token_type == WHILE || myToken.token_type == IF || myToken.token_type == SWITCH
        || myToken.token_type == FOR || myToken.token_type == OUTPUT || myToken.token_type == INPUT) {
        instructionList = parseStmtList();

        // Assign instructionList to the tail of instruction
        struct InstructionNode* getLast = instruction;

        // Traverse the list to find the tail
        while (getLast->next != nullptr) {  
            getLast = getLast->next;
        }
        getLast->next = instructionList;
    }
    return instruction;    // Return pointer to the head of the list
}

struct InstructionNode* parseStmt() {
    
    // initialize instruction node, create a new endNode, and setinitialize last node
    struct InstructionNode* instruction = nullptr;
    struct InstructionNode* endNode = new InstructionNode;
    struct InstructionNode* getLast;
    
    // Set the end node next pointer to null
    endNode->type = NOOP;
    
    // Set the end node next pointer to null
    endNode->next = nullptr;
    
    // Get the next token
    myToken = peek();   

    // check the token type
    switch (myToken.token_type) {   
    case ID:
        instruction = parseAssignStmt();
        break;
    
    // parse a while loop statement
    case WHILE:
        instruction = parseWhileStmt();
        break;
    
    // Parse an if statement
    case IF:
        instruction = parseIfStmt();
        break;

    // Parse a switch statement
    case SWITCH:
        instruction = parseSwitchStmt(endNode);
        //set a end node for the switch statement
        //every case will jump to this at the end
        //it follows the DEFAULT naturally
        
        // Set the getLast pointer to the current node
        getLast = instruction;
        while (getLast->next != nullptr) {  // Loop through the nodes until the last node is reached
            getLast = getLast->next;
        }

        // Set the next pointer of the last node to the end node
        getLast->next = endNode;    
        break;
    
    // Parse a for loop statement
    case FOR:
        instruction = parseForStmt();
        break;
    
    // Parse an output statement
    case OUTPUT:
        instruction = parseOutputStmt();
        break;
    
    // Parse an input statement
    case INPUT:
        instruction = parseInputStmt();
        break;

    // Handle syntax errors
    default:
        syntax_error(); 
    }

    // Return the instruction node
    return instruction;    
}

struct InstructionNode* parseAssignStmt() {

    // Create a new instruction node and set its type to ASSIGN
    auto* assignmentInstruction = new InstructionNode;
    assignmentInstruction->type = ASSIGN;

    // Parse the left-hand side of the assignment statement
    myToken = expect(ID);
    assignmentInstruction->assign_inst.left_hand_side_index = inputVars[myToken.lexeme];

    // Expect an EQUAL token
    expect(EQUAL);

    // Peek at the next token to determine whether the right-hand side of the
    // assignment statement is a primary or an expression
    Token t1 = lexer.GetToken();
    Token t2 = peek();
    lexer.UngetToken(1);

    if (t1.token_type == ID || t1.token_type == NUM) {
        if (t2.token_type == PLUS || t2.token_type == MINUS || t2.token_type == MULT || t2.token_type == DIV) {

            // If the right-hand side is an expression, parse it and assign its
            // operands and operator to the assignment node
            struct InstructionNode* exprInst = parseExpr();
            assignmentInstruction->assign_inst.operand1_index = exprInst->assign_inst.operand1_index;
            assignmentInstruction->assign_inst.op = exprInst->assign_inst.op;
            assignmentInstruction->assign_inst.operand2_index = exprInst->assign_inst.operand2_index;
        }

        else if (t2.token_type == SEMICOLON) {
            // If the right-hand side is a primary, assign it to the assignment
            // node's operand1_index and set the operator to OPERATOR_NONE
            assignmentInstruction->assign_inst.op = OPERATOR_NONE;
            assignmentInstruction->assign_inst.operand1_index = parsePrimary();
        }

        else {
            // If the next token is not a valid operator or SEMICOLON, raise a
            // syntax error
            syntax_error();
        }
    }

    else {
        // If the next token is not a valid ID or NUM token, raise a syntax error
        syntax_error();
    }

    // Expect a SEMICOLON token to end the assignment statement
    expect(SEMICOLON);

    // Set the next pointer of the assignment node to nullptr and return it
    assignmentInstruction->next = nullptr;
    return assignmentInstruction;
}

// Define a function that parses an expression and creates an instruction node for it
struct InstructionNode* parseExpr() {
    auto* infoNode = new InstructionNode;

    // parse the left operand and assign it to the instruction node
    infoNode->assign_inst.operand1_index = parsePrimary();

    // parse the operator and assign it to the instruction node
    switch (parseOperator()) {
    case PLUS:
        infoNode->assign_inst.op = OPERATOR_PLUS;
        break;
    case MINUS:
        infoNode->assign_inst.op = OPERATOR_MINUS;
        break;
    case MULT:
        infoNode->assign_inst.op = OPERATOR_MULT;
        break;
    case DIV:
        infoNode->assign_inst.op = OPERATOR_DIV;
        break;
    default:
        break;
    }

    // Parse the right operand and assign it to the instruction node
    infoNode->assign_inst.operand2_index = parsePrimary();

    // Return the instruction node for the expression
    return infoNode;
}

// Define a function that parses and returns the index of a primary operand, 
// which can either be a variable or a number.
int parsePrimary() {
    int index = -1;

    // Get the next token from the lexer
    myToken = lexer.GetToken();
    if (myToken.token_type == ID || myToken.token_type == NUM) {

        // If it's an ID, set index to the corresponding variable's index
        if (myToken.token_type == ID) {
            index = inputVars[myToken.lexeme];
        }
    
        // If it's a NUM, store the constant in the memory and set index to its index
        else {
            index = next_available;
            mem[next_available++] = stoi(myToken.lexeme);
        }
    }
    
    // If the token is not an ID or a NUM, raise a syntax error
    else {
        syntax_error();
    }
    
    // Return the index of the variable or constant found
    return index;
}

// Parse an operator token and return its TokenType
TokenType parseOperator() {
    
    // Get the next token from the lexer
    myToken = lexer.GetToken();
    
    // If the token is one of the four arithmetic operators, return its TokenType
    if (myToken.token_type == PLUS || myToken.token_type == MINUS || myToken.token_type == MULT || myToken.token_type == DIV) {
        return myToken.token_type;
    }
    
    // Otherwise, there is a syntax error
    else {
        syntax_error();
    }
}

// Define a function to parse an output statement
struct InstructionNode* parseOutputStmt() {
    
    // Create a new InstructionNode for the output statement
    auto* outputNode = new InstructionNode;
    
    // Expect the OUTPUT keyword
    expect(OUTPUT);
    
    // Set the type of the InstructionNode to OUT
    outputNode->type = OUT;
    
    // Expect the identifier of the variable to be output
    myToken = expect(ID);
    
    // Set the index of the variable to be output in the InstructionNode
    outputNode->output_inst.var_index = inputVars[myToken.lexeme];
    
    // Set the next pointer of the InstructionNode to nullptr
    outputNode->next = nullptr;

    // Expect the semicolon at the end of the statement
    expect(SEMICOLON);

    // Return the InstructionNode for the output statement
    return outputNode;
}

struct InstructionNode* parseInputStmt() {
    auto* inputNode = new InstructionNode;

    // Expect and verify the INPUT token
    expect(INPUT);

    // Set the type of instruction node to IN
    inputNode->type = IN;

    // Get the next token, which should be an ID, and store the index of the variable
    myToken = expect(ID);
    inputNode->input_inst.var_index = inputVars[myToken.lexeme];

    // Set the next pointer to null and expect a semicolon to end the statement
    inputNode->next = nullptr;
    expect(SEMICOLON);

    // Return the instruction node for the input statement
    return inputNode;
}

struct InstructionNode* parseWhileStmt() {
    auto* conditionalJumpInstruction = new InstructionNode;

    // Parse the WHILE keyword
    expect(WHILE);

    // Set the type of instruction to conditional jump
    conditionalJumpInstruction->type = CJMP;

    // Parse the condition of the while statement
    struct InstructionNode* whileConditionInstruction = parseCondition();

    // conditionalJumpInstruction the operand indices and condition operator to the CJMP instruction
    conditionalJumpInstruction->cjmp_inst.operand1_index = whileConditionInstruction->cjmp_inst.operand1_index;
    conditionalJumpInstruction->cjmp_inst.condition_op = whileConditionInstruction->cjmp_inst.condition_op;
    conditionalJumpInstruction->cjmp_inst.operand2_index = whileConditionInstruction->cjmp_inst.operand2_index;

    // Check for the opening brace of the while loop body
    myToken = peek();
    if (myToken.token_type == LBRACE) {

        // Parse the body of the while loop
        conditionalJumpInstruction->next = parseBody();
    }
    else {
        syntax_error();
    }

    // Create a new JMP instruction to jump back to the beginning of the loop
    auto* jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = conditionalJumpInstruction;

    // Create a NOOP instruction to terminate the loop
    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    // Find the last instruction in the while loop body
    struct InstructionNode* getLast = conditionalJumpInstruction;
    while (getLast->next != nullptr) {
        getLast = getLast->next;
    }

    // Set the next instruction after the loop body to be the JMP instruction
    getLast->next = jmp;

    // Set the next instruction after the JMP to be the NOOP instruction
    jmp->next = noop;

    // Set the target of the CJMP instruction to be the NOOP instruction
    conditionalJumpInstruction->cjmp_inst.target = noop;

    // Return the while loop instruction
    return conditionalJumpInstruction;
}

struct InstructionNode* parseIfStmt() {
    auto* instructionNode = new InstructionNode;

    // Expect keyword 'if'
    expect(IF);

    // Set type of instruction to 'CJMP'
    instructionNode->type = CJMP;

    // Parse the condition expression and set the operands and operator for the conditional jump instruction
    struct InstructionNode* temp = parseCondition();
    instructionNode->cjmp_inst.operand1_index = temp->cjmp_inst.operand1_index;
    instructionNode->cjmp_inst.condition_op = temp->cjmp_inst.condition_op;
    instructionNode->cjmp_inst.operand2_index = temp->cjmp_inst.operand2_index;

    // Check for the presence of an opening brace and parse the body of the if statement
    myToken = peek();
    if (myToken.token_type == LBRACE) {
        instructionNode->next = parseBody();
    }
    else {
        syntax_error();
    }

    // Create a 'NOOP' instruction and append it to the end of the if statement's body
    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    struct InstructionNode* getLast = instructionNode;
    while (getLast->next != nullptr) {
        getLast = getLast->next;
    }
    getLast->next = noop;

    // Set the target of the conditional jump instruction to the 'NOOP' instruction
    instructionNode->cjmp_inst.target = noop;

    return instructionNode;
}

// Define a function that parses a boolean condition in the form of two expressions and a relational operator
struct InstructionNode* parseCondition() {
    auto* condInst = new InstructionNode;
    // Create a new InstructionNode to hold the condition information

    // Parse the first expression
    myToken = peek();
    if (myToken.token_type == ID || myToken.token_type == NUM) {
        condInst->cjmp_inst.operand1_index = parsePrimary();
    }
    else {
        syntax_error();
    }

    // Parse the relational operator
    myToken = peek();
    if (myToken.token_type == GREATER || myToken.token_type == LESS || myToken.token_type == NOTEQUAL) {
        switch (parseRelationalOperator()) {
        case GREATER:
            condInst->cjmp_inst.condition_op = CONDITION_GREATER;
            break;
        case LESS:
            condInst->cjmp_inst.condition_op = CONDITION_LESS;
            break;
        case NOTEQUAL:
            condInst->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
            break;
        default:
            break;
        }
    }
    else {
        syntax_error();
    }
    // Parse the second expression
    myToken = peek();
    if (myToken.token_type == ID || myToken.token_type == NUM) {
        condInst->cjmp_inst.operand2_index = parsePrimary();
    }
    else {
        syntax_error();
    }

    // Return the InstructionNode with the parsed condition information
    return condInst;
}

// Define function that parses relational operators
TokenType parseRelationalOperator() {
    // Get the next token
    myToken = lexer.GetToken();

    // Check if it is a valid relational operator
    if (myToken.token_type == GREATER || myToken.token_type == LESS || myToken.token_type == NOTEQUAL) {
        // If it is, return the operator type
        return myToken.token_type;
    }

    // Otherwise, report a syntax error
    else {
        syntax_error();
    }
}

// Define function for parsing switch statement
struct InstructionNode* parseSwitchStmt(struct InstructionNode* endNode) {
    auto* switchStatement = new InstructionNode;
    expect(SWITCH);

    myToken = expect(ID);
    int switchOp1 = inputVars[myToken.lexeme];
    expect(LBRACE);
    
    // Parsing the case list
    myToken = peek();
    if (myToken.token_type == CASE) {
        switchStatement = parseCaseList(switchOp1, endNode);
    }
    else {
        syntax_error();
    }

    // Parsing the default case
    myToken = peek();
    if (myToken.token_type == DEFAULT) {

        struct InstructionNode* getLast = switchStatement;
        while (getLast->next->next != nullptr) {
            getLast = getLast->next;
        }
        getLast->next = parseDefaultCase();

        expect(RBRACE);
    }
    else if (myToken.token_type == RBRACE) {
        myToken = lexer.GetToken();
        return switchStatement;
    }
    else {
        syntax_error();
    }
    return switchStatement;
}

// Define a function that parses a for loop statement from the input program's tokens and 
// generates a linked list of instructions 
struct InstructionNode* parseForStmt() {
    auto* forNode = new InstructionNode;
    auto* assignNode = new InstructionNode;

    // Parse 'for' and '('
    expect(FOR);
    expect(LPAREN);

    // Parse the first statement in the for loop, which is always an assignment statement
    myToken = peek();
    if (myToken.token_type == ID) {
        forNode = parseAssignStmt();
    }
    else {
        syntax_error();
    }

    // Create a temporary instruction node to represent the while loop condition
    auto* whileJumpInst = new InstructionNode;
    whileJumpInst->type = CJMP;

    // Parse the condition of the while loop and copy its information to the temporary instruction node
    struct InstructionNode* something = parseCondition();
    whileJumpInst->cjmp_inst.operand1_index = something->cjmp_inst.operand1_index;
    whileJumpInst->cjmp_inst.condition_op = something->cjmp_inst.condition_op;
    whileJumpInst->cjmp_inst.operand2_index = something->cjmp_inst.operand2_index;

    // Parse the semicolon that separates the condition from the second statement in the for loop
    expect(SEMICOLON);

    // Parse the second statement in the for loop, which is always an assignment statement
    myToken = peek();
    if (myToken.token_type == ID) {
        assignNode = parseAssignStmt();
        assignNode->next = nullptr;

        // Parse the closing ')' of the for loop
        expect(RPAREN);
    }
    else {
        syntax_error();
    }

    // Parse the body of the for loop
    myToken = peek();
    if (myToken.token_type == LBRACE) {
        whileJumpInst->next = parseBody();
    }
    else {
        syntax_error();
    }

    // Add the second statement to the end of the temporary while loop instruction node
    auto* addStmt = whileJumpInst->next;
    while (addStmt->next != nullptr) {
        addStmt = addStmt->next;
    }
    addStmt->next = assignNode;

    // Create a jump instruction node to jump back to the condition check in the while loop
    auto* jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = whileJumpInst;

    // Create a noop instruction node to represent the end of the while loop body
    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    // Link the jump and noop instruction nodes to the end of the while loop body
    jmp->next = noop;

    // Link the end of the while loop body to the jump instruction node, creating the loop
    struct InstructionNode* getLast = whileJumpInst;
    while (getLast->next != nullptr) {
        getLast = getLast->next;
    }
    getLast->next = jmp;
    whileJumpInst->cjmp_inst.target = noop;

    // Link the entire for loop together and return it
    forNode->next = whileJumpInst;
    return forNode;
}

struct InstructionNode* parseCaseList(int op, struct InstructionNode* endNode) {
    auto* myCaseNode = new InstructionNode;
    struct InstructionNode* myCaseList = nullptr;
    
    // Case
    myToken = peek();
    if (myToken.token_type == CASE) {
        myCaseNode = parseCase(op);

        // Parse case and set up JMP instruction
        auto* jmp = new InstructionNode;
        jmp->type = JMP;
        jmp->jmp_inst.target = endNode;

        // Set up unconditional jump at the end of the case
        struct InstructionNode* getLast = myCaseNode->cjmp_inst.target;
        while (getLast->next->next != nullptr) {
            getLast = getLast->next;
        }
        getLast->next = jmp;

    }
    else {
        syntax_error();
    }

    // Case_list
    myToken = peek();
    if (myToken.token_type == CASE) {
        // Recursively parse the rest of the case list
        myCaseList = parseCaseList(op, endNode);

        // Add caseList to the tail of myCaseNode 
        struct InstructionNode* getLast = myCaseNode;
        while (getLast->next->next != nullptr) {
            getLast = getLast->next;
        }
        getLast->next = myCaseList;
    }
    return myCaseNode;
}

struct InstructionNode* parseCase(int op) {
    auto* caseNode = new InstructionNode;
    expect(CASE);

    // Set up case instruction with condition and operand indices
    caseNode->type = CJMP;
    caseNode->cjmp_inst.operand1_index = op;
    caseNode->cjmp_inst.condition_op = CONDITION_NOTEQUAL;

    // Get the NUM token and store it in memory
    myToken = expect(NUM);
    int index = next_available;
    mem[next_available++] = stoi(myToken.lexeme);
    caseNode->cjmp_inst.operand2_index = index;

    expect(COLON);

    // Parse the body
    myToken = peek();
    if (myToken.token_type == LBRACE) {
        caseNode->cjmp_inst.target = parseBody();
    }
    else {
        syntax_error();
    }

    // Add a NOOP instruction to the end of the case body
    auto* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    // Connect the NOOP instruction to the end of the case body
    struct InstructionNode* getLast = caseNode->cjmp_inst.target;
    while (getLast->next != nullptr) {
        getLast = getLast->next;
    }
    caseNode->next = noop;
    getLast->next = caseNode->next;
    return caseNode;
}

// Define a function that parses the default case in a switch statement
// Returns a pointer to the resulting InstructionNode
struct InstructionNode* parseDefaultCase() {
    auto* defaultInstruction = new InstructionNode;

    // Expects the 'DEFAULT' keyword.
    expect(DEFAULT);

    // Expects a colon following the 'DEFAULT' keyword
    expect(COLON);
    
    // Parses the body of the default case.
    myToken = peek();
    if (myToken.token_type == LBRACE) {
        defaultInstruction = parseBody();
    }

    // If the next token is not a left brace, a syntax error is thrown
    else {
        syntax_error();
    }

    // Returns a pointer to the resulting InstructionNode
    return defaultInstruction;
}

// Parses the input numbers for the program.
void parseInputs() {
    // Calls the parseNumList function to parse a list of numbers
    parseNumList();
}

void parseNumList() {
    // Expects a number token
    myToken = expect(NUM);
    
    // Appends the input number to a vector of input numbers.
    inputs.push_back(stoi(myToken.lexeme));
    
    // Parses the next number in the list (if any)
    myToken = peek();
    if (myToken.token_type == NUM) {
        parseNumList();
    }
}
