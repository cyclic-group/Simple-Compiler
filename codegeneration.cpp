#include "codegeneration.hpp"

// CodeGenerator Visitor Functions: These are the functions
// you will complete to generate the x86 assembly code. Not
// all functions must have code, many may be left empty.

// helper functions start

/**
 * Print the specified text to stdout and start a newline.
 * 
 * @param text the text to be printed
*/
void println(std::string text) {
    std::cout << text << std::endl;
}

/**
 * Test if the specified identifier refers to an existing local variable in the context.
 * 
 * @param cg pointer to the CodeGenerator object which provides the current context.
 * @param identifier name of the local variable
 * @return true iff the local variable is defined in the current scope.
*/
bool isLocalVariable(CodeGenerator* cg, std::string identifier) {
    const auto* variableTable = cg->currentMethodInfo.variables;
    return variableTable->find(identifier) != variableTable->end();
}

/**
 * Get variable information of the specified member. Assume the member has been defined in the specified class or its ancestor class.
 * 
 * @param cg pointer to the CodeGenerator object which provides the current context
 * @param className name of the class in which the member lies
 * @param memberName name of the member
 * @return VariableInfo object which represents the variable information associated to the given member.
*/
VariableInfo getVariableInfo(CodeGenerator *cg, std::string className, std::string memberName) {
    do {
        auto *memberTable = cg->classTable->at(className).members;
        if (memberTable->find(memberName) != memberTable->end())
            return memberTable->at(memberName);
    } while ( (className = cg->classTable->at(className).superClassName) != "");

    std::cerr << "Access variable info of undefined member <" << className << "." << memberName << ">" << std::endl;
    exit(1);
}

/**
 * Get variable information of the specified local variable or member. Assume the variable/member has been defined in the current scope.
 * 
 * @param cg pointer to the CodeGenerator object which provides the current context
 * @param variableName name of the local variable/member
 * @return VariableInfo which represents the variable information associated to the given variable/member
*/
VariableInfo getVariableInfo(CodeGenerator *cg, std::string variableName) {
    if (isLocalVariable(cg, variableName)) {
        return cg->currentMethodInfo.variables->at(variableName);
    } else {
        return getVariableInfo(cg, cg->currentClassName, variableName);
    }
}

/**
 * The the next available label. A label is a string of the form "L<sequenceNumber>"
 * , where <sequenceNumber> is a natural number starting from 1. The next avaialble label 
 * is induced by the next available sequenceNumber. 
 * 
 * @return the next available label. 
 * 
*/
std::string getNextLabel() {
    static int sequenceNumber = 0;

    sequenceNumber++;
    std::stringstream ss;
    ss << "L" << sequenceNumber;
    return ss.str();
} 

/**
 * Get the size of the members defined in the specified class and its ancestors. Assume the class is defined in the context.
 * 
 * @param cg pointer to the CodeGenerator object which provides the context= "" ? sizeForClassInfo(g, g->classTable->at(classInfo.su
 * @param className name of the class
 * @return the total size of the members defined in the specified class and its ancestors.
*/
int getMembersSize(CodeGenerator *cg, std::string className) {
    auto classInfo = cg->classTable->at(className);
    std::string superClassName = classInfo.superClassName;
    if (superClassName == "")
        return classInfo.membersSize;
    else   
        return classInfo.membersSize + getMembersSize(cg, superClassName);
}

/**
 * Get the offset of the specified member in the specified class. Assume the class is defined and the member is defined in the class under the context.
 * 
 * @param cg pointer to the CodeGenerator that provides the context
 * @param className name of the class
 * @param memberName name of the member
 * @return offset of the member in the class
*/
int getMemberOffset(CodeGenerator *cg, std::string className, std::string memberName) {
    auto classInfo = cg->classTable->at(className);
    while (classInfo.members->find(memberName) == classInfo.members->end()) {
        className = classInfo.superClassName;
        classInfo = cg->classTable->at(className);
    }

    int sizeOfSuperClassMembers = classInfo.superClassName != "" ? getMembersSize(cg, classInfo.superClassName) : 0;
    return getVariableInfo(cg, className, memberName).offset + sizeOfSuperClassMembers;
}
// helper functions end

void CodeGenerator::visitProgramNode(ProgramNode* node) {
    println(".data");
    std::cout << "printstr: .asciz \"%d\\n\"" << std::endl << std::endl;
    println(".text");
    println(".globl Main_main");
    node->visit_children(this);
    std::cout << std::endl;
}

void CodeGenerator::visitClassNode(ClassNode* node) {
    currentClassName = node->identifier_1->name;
    currentClassInfo = classTable->at(currentClassName);
    node->visit_children(this);
}

void CodeGenerator::visitMethodNode(MethodNode* node) {
    currentMethodName = node->identifier->name;
    currentMethodInfo = currentClassInfo.methods->at(currentMethodName);
    println(currentClassName + "_" + currentMethodName + ":");
    node->visit_children(this);
}

void CodeGenerator::visitMethodBodyNode(MethodBodyNode* node) {
    int numberOfLocals = 0;
    for (auto declarationNode : *(node->declaration_list)) {
        numberOfLocals += declarationNode->identifier_list->size();
    }
    int sizeOfLocals = numberOfLocals * 4;

    // save base pointer, set new base pointer
    println("   push %ebp");
    println("   movl %esp, %ebp");

    // save protected registers
    println("   push %ebx");
    println("   push %esi");
    println("   push %edi");
    
    // allocate space for locals
    std::cout << "  add $" << -sizeOfLocals << ", %esp" << std::endl;

    // generate code for children 
    node->visit_children(this);

    // deallocate space for locals
    std::cout << "  add $" << sizeOfLocals << ", %esp" << std::endl;
    
    // restore protected registers
    println("   pop %edi");
    println("   pop %esi");
    println("   pop %ebx");

    // restore base pointer
    println("   pop %ebp");

    // method returns
    println("   ret");
}

void CodeGenerator::visitParameterNode(ParameterNode* node) {
    
}

void CodeGenerator::visitDeclarationNode(DeclarationNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitReturnStatementNode(ReturnStatementNode* node) {
    node->visit_children(this);

    // store return value to %eax
    println("   pop %eax");
}

void CodeGenerator::visitAssignmentNode(AssignmentNode* node) {
    node->visit_children(this);
    if (node->identifier_2) {
        std::string objectName = node->identifier_1->name;
        std::string memberName = node->identifier_2->name;
        const auto objectInfo = getVariableInfo(this, objectName);

        if (isLocalVariable(this, objectName)) {
            // if the object is a local variable
            const auto memberOffset = getMemberOffset(this, objectInfo.type.objectClassName, memberName);
            // save the object address (stored in the stack) to %eax
            std::cout << "  movl " << objectInfo.offset << "(%ebp), %eax" << std::endl;
            // load the value to be assigned to %ebx
            println("   pop %ebx");
            // save the value to object address + member offset
            std::cout << "  movl %ebx, " << memberOffset << "(%eax)" << std::endl;
        } else {
            // if the object is a local member
            const auto objectOffset = getMemberOffset(this, currentClassName, objectName);
            const auto memberOffset = getMemberOffset(this, objectInfo.type.objectClassName, memberName);
            // save the address of self in %eax
            std::cout << "  movl " << "8(%ebp), %eax" << std::endl;
            // add the object offset to the address, result in object address
            std::cout << "  movl " << objectOffset << "(%eax), %eax" << std::endl;
            // load the assign value to %ebx
            println("   pop %ebx");
            // store the assign value to object address + member offset
            std::cout << "  movl %ebx, " << memberOffset << "(%eax)" << std::endl;
        }
    } else {

        std::string variableName = node->identifier_1->name;
        if (isLocalVariable(this, variableName)) {
            // if the variable is local 
            const auto variableInfo = getVariableInfo(this, variableName);
            // save the assign value
            println("   pop %eax");
            // store the assign value to base address + variable offset
            std::cout << "  movl %eax, " << variableInfo.offset << "(%ebp)" << std::endl;
        } else {
            // else the variable must be a member of self (the activator)
            const auto memberOffset = getMemberOffset(this, currentClassName, variableName);
            // save the address of self to %eax
            println("   movl 8(%ebp), %eax");
            // load the assign value
            println("   pop %ebx");
            // store the assign value to self address + member offset
            std::cout << "movl %ebx, " << memberOffset << "(%eax)" << std::endl;
        }
    }
}

void CodeGenerator::visitCallNode(CallNode* node) {
    println("# Call statement");
    node->visit_children(this);
    // drop the return value
    println("   add $4, %esp");
}

void CodeGenerator::visitIfElseNode(IfElseNode* node) {
    // generate code for computing the branch expr 
    node->expression->accept(this);

    std::string elseLabel = getNextLabel();
    std::string endIfLabel = getNextLabel();
    println("# If else statement");
    // load the result of the expr
    println("   pop %eax");
    // evaluate and jump
    println("   mov $0, %ebx");
    println("   cmp %eax, %ebx");
    println("   je " + elseLabel);
    // generate code for true branch
    for (auto *statementNode : *(node->statement_list_1)) {
        statementNode->accept(this);
    }
    println("   jmp " + endIfLabel);
    println(elseLabel + ":");
    // generate code for false branch 
    for (auto *statementNode : *(node->statement_list_2)) {
        statementNode->accept(this);
    }
    println(endIfLabel + ":");
}

void CodeGenerator::visitWhileNode(WhileNode* node) {
    std::string loopStartLabel = getNextLabel();
    std::string loopExitLabel = getNextLabel();

    println(loopStartLabel + ":");
    node->expression->accept(this);
    println("   pop %eax");
    println("   mov $0, %ebx");
    println("   cmp %eax, %ebx");
    println("   je " + loopExitLabel);

    for (auto *statementNode : *(node->statement_list)) {
        statementNode->accept(this);
    }
    println("   jmp " + loopStartLabel);
    println(loopExitLabel + ":");
}

void CodeGenerator::visitPrintNode(PrintNode* node) {
    node->visit_children(this);
    println("# Print");
    println("   push $printstr");
    println("   call printf");
    println("   add $8, %esp");
}

void CodeGenerator::visitDoWhileNode(DoWhileNode* node) {
    std::string loopStartLabel = getNextLabel();
    std::string loopExitLabel = getNextLabel();

    println(loopStartLabel + ":");
    for (auto *statementNode : *(node->statement_list)) {
        statementNode->accept(this);
    }
    
    node->expression->accept(this);
    println("   pop %eax");
    println("   mov $0, %ebx");
    println("   cmp %eax, %ebx");
    println("   jne " + loopStartLabel);
    println(loopExitLabel + ":");
}

void CodeGenerator::visitQMNode(QMNode* node){
    std::string falseBranchLabel = getNextLabel();
    std::string exitLabel = getNextLabel();

    node->expression_1->accept(this);
    println("   pop %eax");
    println("   mov $0, %ebx");
    println("   cmp %eax, %ebx");
    println("   je " + falseBranchLabel);

    node->expression_2->accept(this);
    println("   jmp " + exitLabel);

    println(falseBranchLabel + ":");
    node->expression_3->accept(this);

    println(exitLabel + ":");
}

void CodeGenerator::visitPlusNode(PlusNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   add %ebx, %eax");
    println("   push %eax");
}

void CodeGenerator::visitMinusNode(MinusNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   sub %ebx, %eax");
    println("   push %eax");
}

void CodeGenerator::visitTimesNode(TimesNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   imul %ebx, %eax");
    println("   push %eax");
}

void CodeGenerator::visitDivideNode(DivideNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   cdq");
    println("   idiv %ebx");
    println("   push %eax");
}

void CodeGenerator::visitGreaterNode(GreaterNode* node) {
    std::string greaterLabel = getNextLabel();
    std::string exitLabel = getNextLabel();

    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   cmp %ebx, %eax");
    println("   jg " + greaterLabel);
    
    println("   push $0");
    println("   jmp " + exitLabel);

    println(greaterLabel + ":");
    println("   push $1");
    println(exitLabel + ":");
}

void CodeGenerator::visitGreaterEqualNode(GreaterEqualNode* node) {
    std::string greaterThanLabel = getNextLabel();
    std::string exitLabel = getNextLabel();

    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   cmp %ebx, %eax");
    println("   jge " + greaterThanLabel);
    
    println("   push $0");
    println("   jmp " + exitLabel);

    println(greaterThanLabel + ":");
    println("   push $1");
    println(exitLabel + ":");
}

void CodeGenerator::visitEqualNode(EqualNode* node) {
    std::string equalLabel = getNextLabel();
    std::string exitLabel = getNextLabel();

    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   cmp %ebx, %eax");
    println("   je " + equalLabel);
    
    println("   push $0");
    println("   jmp " + exitLabel);

    println(equalLabel + ":");
    println("   push $1");
    println(exitLabel + ":");   
}

void CodeGenerator::visitAndNode(AndNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   andl %ebx, %eax");
    println("   push %eax");
}

void CodeGenerator::visitOrNode(OrNode* node) {
    node->visit_children(this);
    println("   pop %ebx");
    println("   pop %eax");
    println("   orl %ebx, %eax");
    println("   push %eax");
}

void CodeGenerator::visitNotNode(NotNode* node) {
    node->visit_children(this);
    println("   pop %eax");
    println("   xor $1, %eax");
    println("   push %eax");
}

void CodeGenerator::visitNegationNode(NegationNode* node) {
    node->visit_children(this);
    println("   pop %eax");
    println("   neg %eax");
    println("   push %eax");
}

void CodeGenerator::visitMethodCallNode(MethodCallNode* node) {
    // save temporary registers
    println("   push %eax");
    println("   push %ecx");
    println("   push %edx");

    // compute and push arguments in the reverse order
    auto *argumentList = node->expression_list;
    if (argumentList) {
        for (auto argumentNode = argumentList->rbegin(); argumentNode != argumentList->rend(); argumentNode++) {
            (*argumentNode)->accept(this);
        }
    }

    // push self
    if (node->identifier_2) {
        std::string objectName = node->identifier_1->name;
        auto objectInfo = getVariableInfo(this, objectName);
        if (isLocalVariable(this, objectName)) {
            std::cout << "   movl " << objectInfo.offset << "(%ebp), %eax" << std::endl;
            println("   push %eax");
        } else {
            const int objectOffset = getMemberOffset(this, currentClassName, objectName);
            println("   movl 8(%ebp), %eax");
            std::cout << "  movl " << objectOffset << "(%eax), %eax" << std::endl;
            println("   push %eax");
        }
    } else {
        if (currentClassName != "Main") {
            println("   movl 8(%ebp), %eax");
            println("   push %eax");
        } else {
            println("   add $-4, %esp");
        }
    }

    // Call the function
    std::string className;
    ClassInfo classInfo;
    std::string methodName;

    if (node->identifier_2) {
        className = getVariableInfo(this, node->identifier_1->name).type.objectClassName;
        classInfo = classTable->at(className);
        methodName = node->identifier_2->name;
    } else {
        className = currentClassName;
        classInfo = currentClassInfo;
        methodName = node->identifier_1->name;
    }
    while (classInfo.methods->find(methodName) == classInfo.methods->end()) {
        className = classInfo.superClassName;
        classInfo = classTable->at(className);
    }

    std::cout << "  call " << className << "_" << methodName << std::endl;
    std::cout << "  movl %eax, %ebx" << std::endl;
    std::cout << "  add $" << 4 * (node->expression_list->size() + 1) << ", %esp" <<  std::endl;
    
    // restore temporary registers
    println("   pop %edx");
    println("   pop %ecx");
    println("   pop %eax");

    println("   push %ebx");
}

void CodeGenerator::visitMemberAccessNode(MemberAccessNode* node) {
    std::string objectName = node->identifier_1->name;
    std::string memberName = node->identifier_2->name;
    auto objectInfo = getVariableInfo(this, objectName);
    if (isLocalVariable(this, objectName)) {
        int memberOffset = getMemberOffset(this, objectInfo.type.objectClassName, memberName);
        std::cout << "  movl " << objectInfo.offset << "(%ebp), %eax" << std::endl;
        std::cout << "  movl " << memberOffset << "(%eax), %eax" << std::endl;
        println("   push %eax");
    } else {
        int objectOffset = getMemberOffset(this, currentClassName, objectName);
        int memberOffset = getMemberOffset(this, objectInfo.type.objectClassName, memberName);
        std::cout << "  movl 8(%ebp), %eax" << std::endl;
        std::cout << "  movl " << objectOffset << "(%eax), %eax" << std::endl;
        std::cout << "  movl " << memberOffset << "(%eax), %eax" << std::endl;
        println("   push %eax");
    }
}

void CodeGenerator::visitVariableNode(VariableNode* node) {
    std::string varaibleName = node->identifier->name;
    if (isLocalVariable(this, varaibleName)) {
        auto variableInfo = getVariableInfo(this, varaibleName);
        std::cout << "  movl " << variableInfo.offset << "(%ebp), %eax" << std::endl;
        println("   push %eax");
    } else {
        int memberOffset = getMemberOffset(this, currentClassName, varaibleName);
        std::cout << "  movl " << "8(%ebp), %eax" << std::endl;
        std::cout << "  movl " << memberOffset << "(%eax), %eax" << std::endl;
        println("   push %eax");
    }
}

void CodeGenerator::visitIntegerLiteralNode(IntegerLiteralNode* node) {
    std::cout << "  push $" << node->integer->value << std::endl;
}

void CodeGenerator::visitBooleanLiteralNode(BooleanLiteralNode* node) {
    std::cout << "  push $" << node->integer->value << std::endl;
}

void CodeGenerator::visitNewNode(NewNode* node) {
    std::string className = node->identifier->name;
    auto classInfo = classTable->at(className);
    std::cout << "  push $" << getMembersSize(this, className) << std::endl;
    println("   call malloc");
    println("   add $4, %esp");
    println("   push %eax");

    auto methodTable = classInfo.methods;
    if (methodTable->find(className) != methodTable->end()) {
        println("   push %eax");
        println("   push %ecx");
        println("   push %edx");

        if (node->expression_list) {
            for (auto argumentNode = node->expression_list->rbegin(); argumentNode != node->expression_list->rend(); argumentNode++) {
                (*argumentNode)->accept(this);
            }
        }

        std::cout << "  movl " << 4 * (node->expression_list->size() + 3) << "(%esp), %eax" << std::endl;
        std::cout << "  push %eax" << std::endl;
        std::cout << "  call " << className << "_" << className << std::endl;

        std::cout << "  add $" << 4*(node->expression_list->size() + 1) << ", %esp" << std::endl;

        println("   pop %edx");
        println("   pop %ecx");
        println("   pop %eax");
    }
}

void CodeGenerator::visitIntegerTypeNode(IntegerTypeNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitBooleanTypeNode(BooleanTypeNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitObjectTypeNode(ObjectTypeNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitNoneNode(NoneNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitIdentifierNode(IdentifierNode* node) {
    // WRITEME: Replace with code if necessary
}

void CodeGenerator::visitIntegerNode(IntegerNode* node) {
    // WRITEME: Replace with code if necessary
}
