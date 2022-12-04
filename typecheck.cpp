
#include "typecheck.hpp"

// Defines the function used to throw type errors. The possible
// type errors are defined as an enumeration in the header file.
void typeError(TypeErrorCode code) {
  switch (code) {
    case undefined_variable:
      std::cerr << "Undefined variable." << std::endl;
      break;
    case undefined_method:
      std::cerr << "Method does not exist." << std::endl;
      break;
    case undefined_class:
      std::cerr << "Class does not exist." << std::endl;
      break;
    case undefined_member:
      std::cerr << "Class member does not exist." << std::endl;
      break;
    case not_object:
      std::cerr << "Variable is not an object." << std::endl;
      break;
    case expression_type_mismatch:
      std::cerr << "Expression types do not match." << std::endl;
      break;
    case argument_number_mismatch:
      std::cerr << "Method called with incorrect number of arguments." << std::endl;
      break;
    case argument_type_mismatch:
      std::cerr << "Method called with argument of incorrect type." << std::endl;
      break;
    case while_predicate_type_mismatch:
      std::cerr << "Predicate of while loop is not boolean." << std::endl;
      break;
    case do_while_predicate_type_mismatch:
      std::cerr << "Predicate of do while loop is not boolean." << std::endl;
      break;
    case if_predicate_type_mismatch:
      std::cerr << "Predicate of if statement is not boolean." << std::endl;
      break;
    case assignment_type_mismatch:
      std::cerr << "Left and right hand sides of assignment types mismatch." << std::endl;
      break;
    case return_type_mismatch:
      std::cerr << "Return statement type does not match declared return type." << std::endl;
      break;
    case constructor_returns_type:
      std::cerr << "Class constructor returns a value." << std::endl;
      break;
    case no_main_class:
      std::cerr << "The \"Main\" class was not found." << std::endl;
      break;
    case main_class_members_present:
      std::cerr << "The \"Main\" class has members." << std::endl;
      break;
    case no_main_method:
      std::cerr << "The \"Main\" class does not have a \"main\" method." << std::endl;
      break;
    case main_method_incorrect_signature:
      std::cerr << "The \"main\" method of the \"Main\" class has an incorrect signature." << std::endl;
      break;
  }
  exit(1);
}

// TypeCheck Visitor Functions: These are the functions you will
// complete to build the symbol table and type check the program.
// Not all functions must have code, many may be left empty.

// Creator for CompoundType
CompoundType compoundType(BaseType baseType, std::string objectClassName) {
  CompoundType compoundType;
  compoundType.baseType = baseType;
  compoundType.objectClassName = objectClassName;
  return compoundType;
}

// either local or parameter in the current scope
bool isDefinedAsLocalVariable(TypeCheck *typeCheck, std::string identifier) {
  return (typeCheck->currentVariableTable->find(identifier) != typeCheck->currentVariableTable->end());
}


// Assume the variable can be accessed in the current scope. Return the type of the variable. 
CompoundType variableCompoundType(TypeCheck *typeCheck, std::string identifier) {
  // test if the identifier refers to a local variable
  if (!isDefinedAsLocalVariable(typeCheck, identifier)) {
    // find the identifier in the member list of the current class and its super classes 
    std::string className = typeCheck->currentClassName;

    do {
      auto *memberTable = typeCheck->classTable->at(className).members;
      if (memberTable->find(identifier) != memberTable->end())
        return memberTable->at(identifier).type;
    } while ((className = typeCheck->classTable->at(className).superClassName) != "");
    
    std::cerr << "Should not get here";
    exit(1);
  } 

  return typeCheck->currentVariableTable->at(identifier).type;
}

CompoundType memberCompoundType(TypeCheck *typeCheck, std::string className, std::string memberName) {
  do {
    auto *memberTable = typeCheck->classTable->at(className).members;
    if (memberTable->find(memberName) != memberTable->end())
      return memberTable->at(memberName).type;
  } while ((className = typeCheck->classTable->at(className).superClassName) != "");

  std::cerr << "Should not get here";
  exit(1);
}

/**
 * Assume the method with the specified name is defined in or in one super class of the class with the specified name. 
*/
MethodInfo getMethodInfo(TypeCheck *typeCheck, std::string identifier, std::string className) {
  do {
    auto *methodTable = typeCheck->classTable->at(className).methods;
    if (methodTable->find(identifier) != methodTable->end())
      return methodTable->at(identifier);
  } while ((className = typeCheck->classTable->at(className).superClassName) != "");

  std::cerr << "Should not get here";
  exit(1);
} 

bool isDefinedAsClass(TypeCheck *typeCheck, std::string identifier) {
  return (typeCheck->classTable->find(identifier) != typeCheck->classTable->end());
}

bool isDefinedAsMethodInClass(TypeCheck *typeCheck, std::string identifier, std::string className) {
  do {
    auto *methodTable = typeCheck->classTable->at(className).methods;
    if (methodTable->find(identifier) != methodTable->end()) 
      return true;
  } while ((className = typeCheck->classTable->at(className).superClassName) != "");

  return false;
}

bool isDefinedAsMemberInClass(TypeCheck* typeCheck, std::string identifier, std::string className) {
  do {
    auto *members = typeCheck->classTable->at(className).members; 
    if (members->find(identifier) != members->end()) 
      return true;
  } while ((className = typeCheck->classTable->at(className).superClassName) != "");

  return false; 
}

// return if className1 is a subclass of className2
bool isSubclassOf(TypeCheck *typeCheck, std::string className1, std::string className2) {
  do {
    if (className1 == className2) return true;
  } while ((className1 = typeCheck->classTable->at(className1).superClassName) != ""); 
  
  return false; 
}

// -------------------------------------------

void TypeCheck::visitProgramNode(ProgramNode* node) {
  classTable = new ClassTable();
  node->visit_children(this);

  // test if the main class is defined 
  if ( ! isDefinedAsClass(this, "Main"))
    typeError(no_main_class);

  // test if the main method is defined in the main class
  if ( ! isDefinedAsMethodInClass(this, "main", "Main"))
    typeError(no_main_method);

  // check if any member defined in main class
  if (classTable->at("Main").members->size() != 0)
    typeError(main_class_members_present);

  // check main method signature
  auto mainMethodInfo = getMethodInfo(this, "main", "Main");
  if (mainMethodInfo.parameters->size() != 0 || mainMethodInfo.returnType.baseType != bt_none) 
    typeError(main_method_incorrect_signature);
}

void TypeCheck::visitClassNode(ClassNode* node) {
  // check if the superclass name (if exists) is defined 
  std::string className = node->identifier_1->name;
  std::string superclassName = (node->identifier_2) ? node->identifier_2->name : "";
  if (superclassName != "" && !isDefinedAsClass(this, superclassName)) {
    typeError(undefined_class);
  }

  // initialize the class table entry
  (*classTable)[className] = ClassInfo();
  ClassInfo &classInfo = classTable->at(className);
  classInfo.members = new VariableTable();
  classInfo.membersSize = 0;
  classInfo.methods = new MethodTable();
  classInfo.superClassName = superclassName;

  // build memberTable
  auto *memberTable = classInfo.members;
  int memberOffset = 0;
  for (auto *declarationNode : *(node->declaration_list)) {
    for (auto *identifierNode : *(declarationNode->identifier_list)) {
      std::string memberName = identifierNode->name;
      (*memberTable)[memberName] = VariableInfo();
      VariableInfo &memberInfo = memberTable->at(memberName);
      memberInfo.offset = memberOffset;
      memberOffset += 4; 
      memberInfo.size = 4;
      memberInfo.type = compoundType(declarationNode->type->basetype, declarationNode->type->objectClassName);
    }
  }
  classInfo.membersSize = memberOffset;

  // set current pointers and begin visiting child nodes
  currentClassName = className;
  currentMethodTable = classInfo.methods;
  node->visit_children(this);
}

void TypeCheck::visitMethodNode(MethodNode* node) {
  std::string methodName = node->identifier->name;

  // create method entry, build methodInfo
  (*currentMethodTable)[methodName] = MethodInfo();
  auto &methodInfo = currentMethodTable->at(methodName);
  methodInfo.variables = new VariableTable();
  methodInfo.localsSize = 0;
  methodInfo.parameters = new std::list<CompoundType>();
  methodInfo.returnType = compoundType(node->type->basetype, node->type->objectClassName);
  for (auto parameterNode : *(node->parameter_list)) {
    methodInfo.parameters->push_back(compoundType(parameterNode->type->basetype, parameterNode->type->objectClassName));
  }

  // check constructor return type
  auto returnType = methodInfo.returnType;
  if (methodName == currentClassName && returnType.baseType != bt_none)
    typeError(constructor_returns_type);

  // check validity if return type is a class
  if (returnType.baseType == bt_object && !isDefinedAsClass(this, returnType.objectClassName))
    typeError(undefined_class);

  // set current pointers 
  currentVariableTable = methodInfo.variables;
  currentLocalOffset = -4;
  currentParameterOffset = 12;
  node->visit_children(this);
  methodInfo.localsSize = -(currentLocalOffset + 4);

  // check return type
  if (node->methodbody->basetype != returnType.baseType)
    typeError(return_type_mismatch);
  else if (returnType.baseType == bt_object && (!isSubclassOf(this, node->methodbody->objectClassName, returnType.objectClassName)))
    typeError(return_type_mismatch);
}

void TypeCheck::visitMethodBodyNode(MethodBodyNode* node) {
  
  for (auto *declarationNode: *(node->declaration_list)) {

    for (auto *identifierNode : *(declarationNode->identifier_list)) {

      std::string localName = identifierNode->name;
      (*currentVariableTable)[localName] = VariableInfo();
      auto &variableInfo = currentVariableTable->at(localName);
      variableInfo.offset = currentLocalOffset;
      currentLocalOffset -= 4;
      variableInfo.size = 4;
      variableInfo.type.baseType = declarationNode->type->basetype;
      variableInfo.type.objectClassName = declarationNode->type->objectClassName;
    }
  }

  node->visit_children(this);
  if (!node->returnstatement)
    node->basetype = bt_none;
  else {
    node->basetype = node->returnstatement->basetype;
    node->objectClassName = node->returnstatement->objectClassName;
  }
}

void TypeCheck::visitParameterNode(ParameterNode* node) {
  if (node->type->basetype == bt_object && !isDefinedAsClass(this, node->type->objectClassName))
    typeError(undefined_class);

  // create an entry in the variable table
  std::string parameterName = node->identifier->name;
  (*currentVariableTable)[parameterName] = VariableInfo();
  auto &variableInfo = currentVariableTable->at(parameterName);
  variableInfo.offset = currentParameterOffset;
  currentParameterOffset += 4;
  variableInfo.size = 4;
  variableInfo.type = compoundType(node->type->basetype, node->type->objectClassName);

  node->basetype = node->type->basetype;
  node->objectClassName = node->type->objectClassName;
}

void TypeCheck::visitDeclarationNode(DeclarationNode* node) {
  if (node->type->basetype == bt_object && !isDefinedAsClass(this, node->type->objectClassName))
    typeError(undefined_class);

  node->basetype = node->type->basetype;
  node->objectClassName = node->type->objectClassName;
}

void TypeCheck::visitReturnStatementNode(ReturnStatementNode* node) {
  node->visit_children(this);
  node->basetype = node->expression->basetype;
  node->objectClassName = node->expression->objectClassName;
}

void TypeCheck::visitAssignmentNode(AssignmentNode* node) {
  node->visit_children(this);
  std::string name1 = node->identifier_1->name;
  std::string name2 = node->identifier_2 ? node->identifier_2->name : "";
  if (name2 == "") {
    if (!isDefinedAsLocalVariable(this, name1) && !isDefinedAsMemberInClass(this, name1, currentClassName))
      typeError(undefined_variable);

    auto leftType = variableCompoundType(this, name1);
    auto rightType = compoundType(node->expression->basetype, node->expression->objectClassName);

    if (leftType.baseType != rightType.baseType) {
      typeError(assignment_type_mismatch);
    } 
    if (leftType.baseType == bt_object && !isSubclassOf(this, rightType.objectClassName, leftType.objectClassName))
      typeError(assignment_type_mismatch);

    node->basetype = leftType.baseType;
    node->objectClassName = leftType.objectClassName;
  } else {
    if (!isDefinedAsLocalVariable(this, name1) && !isDefinedAsMemberInClass(this, name1, currentClassName))
      typeError(undefined_variable);

    auto variableType = variableCompoundType(this, name1);
    if (variableType.baseType != bt_object)
      typeError(not_object);
    if (!isDefinedAsMemberInClass(this, name2, variableType.objectClassName))
      typeError(undefined_member);
    
    auto leftType = memberCompoundType(this, variableType.objectClassName, name2);
    auto rightType = compoundType(node->expression->basetype, node->expression->objectClassName);

    if (leftType.baseType != rightType.baseType) {
      typeError(assignment_type_mismatch);
    } 
    if (leftType.baseType == bt_object && !isSubclassOf(this, rightType.objectClassName, leftType.objectClassName))
      typeError(assignment_type_mismatch);

    node->basetype = leftType.baseType;
    node->objectClassName = leftType.objectClassName;
  } 
}

void TypeCheck::visitCallNode(CallNode* node) {
  node->visit_children(this);
  node->basetype = node->methodcall->basetype;
  node->objectClassName = node->methodcall->basetype; 
}

void TypeCheck::visitIfElseNode(IfElseNode* node) {
  node->visit_children(this);
  if (node->expression->basetype != bt_boolean)
    typeError(if_predicate_type_mismatch);
}

void TypeCheck::visitWhileNode(WhileNode* node) {
  node->visit_children(this);
  if (node->expression->basetype != bt_boolean) 
    typeError(while_predicate_type_mismatch);
}

void TypeCheck::visitDoWhileNode(DoWhileNode* node) {
  node->visit_children(this);
  if (node->expression->basetype != bt_boolean) {
    typeError(do_while_predicate_type_mismatch);
  }
}

void TypeCheck::visitPrintNode(PrintNode* node) {
  node->visit_children(this);
}

void TypeCheck::visitPlusNode(PlusNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) 
    typeError(expression_type_mismatch);

  node->basetype = bt_integer;
}

void TypeCheck::visitMinusNode(MinusNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) 
    typeError(expression_type_mismatch);

  node->basetype = bt_integer;
}

void TypeCheck::visitTimesNode(TimesNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) 
    typeError(expression_type_mismatch);

  node->basetype = bt_integer;
}

void TypeCheck::visitDivideNode(DivideNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) 
    typeError(expression_type_mismatch);

  node->basetype = bt_integer;
}

void TypeCheck::visitGreaterNode(GreaterNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) {
    typeError(expression_type_mismatch);
  }

  node->basetype = bt_boolean;
}

void TypeCheck::visitGreaterEqualNode(GreaterEqualNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_integer || node->expression_2->basetype != bt_integer) 
    typeError(expression_type_mismatch);

  node->basetype = bt_boolean;
}

void TypeCheck::visitEqualNode(EqualNode* node) {
  node->visit_children(this);
  auto leftBasetype = node->expression_1->basetype;
  auto rightBasetype = node->expression_2->basetype;
  if ( ! ((leftBasetype == bt_integer && rightBasetype == bt_integer) || (leftBasetype == bt_boolean && rightBasetype == bt_boolean)) )
    typeError(expression_type_mismatch);
  
  node->basetype = bt_boolean;
}

void TypeCheck::visitAndNode(AndNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_boolean || node->expression_2->basetype != bt_boolean) 
    typeError(expression_type_mismatch);

  node->basetype = bt_boolean;
}

void TypeCheck::visitOrNode(OrNode* node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_boolean || node->expression_2->basetype != bt_boolean) 
    typeError(expression_type_mismatch);

  node->basetype = bt_boolean;
}

void TypeCheck::visitNotNode(NotNode* node) {
  node->visit_children(this);
  if (node->expression->basetype != bt_boolean) 
    typeError(expression_type_mismatch);
  
  node->basetype = bt_boolean;
}

void TypeCheck::visitNegationNode(NegationNode* node) {
  node->visit_children(this);
  if (node->expression->basetype != bt_integer)
    typeError(expression_type_mismatch);
  
  node->basetype = bt_integer;
}

void TypeCheck::visitQMNode(QMNode *node) {
  node->visit_children(this);
  if (node->expression_1->basetype != bt_boolean) 
    typeError(expression_type_mismatch);

  if ( ! ( (node->expression_2->basetype == bt_integer && node->expression_3->basetype == bt_integer) || (node->expression_2->basetype == bt_boolean && node->expression_3->basetype == bt_boolean) )) {
    typeError(expression_type_mismatch);
  }
  
  node->basetype = node->expression_2->basetype;
}

void TypeCheck::visitMethodCallNode(MethodCallNode* node) {
  node->visit_children(this);
  std::string className, methodName;

  if (node->identifier_2) {
    std::string objectName = node->identifier_1->name;
    methodName = node->identifier_2->name;
    if (!isDefinedAsLocalVariable(this, objectName) && !isDefinedAsMemberInClass(this, objectName, currentClassName))
      typeError(undefined_variable);
    if (variableCompoundType(this, objectName).baseType != bt_object) {
      typeError(not_object);
    }
    className = variableCompoundType(this, objectName).objectClassName;
    if (!isDefinedAsMethodInClass(this, methodName, className))
      typeError(undefined_method);
  } else {
    methodName = node->identifier_1->name;
    className = currentClassName;
    if (!isDefinedAsMethodInClass(this, methodName, className)) 
      typeError(undefined_method);
  }

  auto methodInfo = getMethodInfo(this, methodName, className);
  auto *parameterList = methodInfo.parameters;
  if (parameterList->size() != node->expression_list->size())
    typeError(argument_number_mismatch);
  
  auto pListIt = parameterList->begin();
  auto aListIt = node->expression_list->begin();
  while (pListIt != parameterList->end()) {
    if (pListIt->baseType != (*aListIt)->basetype)
      typeError(argument_type_mismatch);
    if (pListIt->baseType == bt_object && !isSubclassOf(this, (*aListIt)->objectClassName, pListIt->objectClassName))
      typeError(argument_type_mismatch);

    pListIt++;
    aListIt++;
  }

  node->basetype = methodInfo.returnType.baseType;
  node->objectClassName = methodInfo.returnType.baseType;
}

void TypeCheck::visitMemberAccessNode(MemberAccessNode* node) {
  std::string objectName = node->identifier_1->name;
  std::string memberName = node->identifier_2->name;
  if (!isDefinedAsLocalVariable(this, objectName) && !isDefinedAsMemberInClass(this, objectName, currentClassName)) {
    typeError(undefined_variable);
  }
  if (variableCompoundType(this, objectName).baseType != bt_object)
  {
    typeError(not_object);
  }
  if (!isDefinedAsMemberInClass(this, memberName, variableCompoundType(this, objectName).objectClassName))
  {
    typeError(undefined_member);
  }
  
  auto memberType = memberCompoundType(this, variableCompoundType(this, objectName).objectClassName, memberName);
  node->basetype = memberType.baseType;
  node->objectClassName = memberType.objectClassName;
}

void TypeCheck::visitVariableNode(VariableNode* node) {
  std::string variableName = node->identifier->name;
  if (!isDefinedAsLocalVariable(this, variableName) && !isDefinedAsMemberInClass(this, variableName, currentClassName)) {
    typeError(undefined_variable);
  }

  auto compoundType = variableCompoundType(this, variableName);
  node->basetype = compoundType.baseType;
  node->objectClassName = compoundType.objectClassName;
}

void TypeCheck::visitIntegerLiteralNode(IntegerLiteralNode* node) {
  node->basetype = bt_integer;
}

void TypeCheck::visitBooleanLiteralNode(BooleanLiteralNode* node) {
  node->basetype = bt_boolean;
}

void TypeCheck::visitNewNode(NewNode* node) {
  node->visit_children(this);
  if (!isDefinedAsClass(this, node->identifier->name)) 
    typeError(undefined_class);
  if (isDefinedAsMethodInClass(this, node->identifier->name, node->identifier->name)) {
    auto methodInfo = getMethodInfo(this, node->identifier->name, node->identifier->name);
    auto *parameterList = methodInfo.parameters;
    if (parameterList->size() != node->expression_list->size())
      typeError(argument_number_mismatch);
    
    auto pListIt = parameterList->begin();
    auto aListIt = node->expression_list->begin();
    while (pListIt != parameterList->end()) {
      if (pListIt->baseType != (*aListIt)->basetype)
        typeError(argument_type_mismatch);
      if (pListIt->baseType == bt_object && !isSubclassOf(this, (*aListIt)->objectClassName, pListIt->objectClassName))
        typeError(argument_type_mismatch);

      pListIt++;
      aListIt++;
    }
  }

  node->basetype = bt_object;
  node->objectClassName = node->identifier->name;
}

void TypeCheck::visitIntegerTypeNode(IntegerTypeNode* node) {
  node->basetype = bt_integer;
}

void TypeCheck::visitBooleanTypeNode(BooleanTypeNode* node) {
  node->basetype = bt_boolean;
}

void TypeCheck::visitObjectTypeNode(ObjectTypeNode* node) {
  node->basetype = bt_object;
  node->objectClassName = node->identifier->name;
}

void TypeCheck::visitNoneNode(NoneNode* node) {
  node->basetype = bt_none;
}

void TypeCheck::visitIdentifierNode(IdentifierNode* node) {
  // WRITEME: Replace with code if necessary
}

void TypeCheck::visitIntegerNode(IntegerNode* node) {
  node->basetype = bt_integer; 
}


// The following functions are used to print the Symbol Table.
// They do not need to be modified at all.

std::string genIndent(int indent) {
  std::string string = std::string("");
  for (int i = 0; i < indent; i++)
    string += std::string(" ");
  return string;
}

std::string string(CompoundType type) {
  switch (type.baseType) {
    case bt_integer:
      return std::string("Integer");
    case bt_boolean:
      return std::string("Boolean");
    case bt_none:
      return std::string("None");
    case bt_object:
      return std::string("Object(") + type.objectClassName + std::string(")");
    default:
      return std::string("");
  }
}


void print(VariableTable variableTable, int indent) {
  std::cout << genIndent(indent) << "VariableTable {";
  if (variableTable.size() == 0) {
    std::cout << "}";
    return;
  }
  std::cout << std::endl;
  for (VariableTable::iterator it = variableTable.begin(); it != variableTable.end(); it++) {
    std::cout << genIndent(indent + 2) << it->first << " -> {" << string(it->second.type);
    std::cout << ", " << it->second.offset << ", " << it->second.size << "}";
    if (it != --variableTable.end())
      std::cout << ",";
    std::cout << std::endl;
  }
  std::cout << genIndent(indent) << "}";
}

void print(MethodTable methodTable, int indent) {
  std::cout << genIndent(indent) << "MethodTable {";
  if (methodTable.size() == 0) {
    std::cout << "}";
    return;
  }
  std::cout << std::endl;
  for (MethodTable::iterator it = methodTable.begin(); it != methodTable.end(); it++) {
    std::cout << genIndent(indent + 2) << it->first << " -> {" << std::endl;
    std::cout << genIndent(indent + 4) << string(it->second.returnType) << "," << std::endl;
    std::cout << genIndent(indent + 4) << it->second.localsSize << "," << std::endl;
    print(*it->second.variables, indent + 4);
    std::cout <<std::endl;
    std::cout << genIndent(indent + 2) << "}";
    if (it != --methodTable.end())
      std::cout << ",";
    std::cout << std::endl;
  }
  std::cout << genIndent(indent) << "}";
}

void print(ClassTable classTable, int indent) {
  std::cout << genIndent(indent) << "ClassTable {" << std::endl;
  for (ClassTable::iterator it = classTable.begin(); it != classTable.end(); it++) {
    std::cout << genIndent(indent + 2) << it->first << " -> {" << std::endl;
    if (it->second.superClassName != "")
      std::cout << genIndent(indent + 4) << it->second.superClassName << "," << std::endl;
    print(*it->second.members, indent + 4);
    std::cout << "," << std::endl;
    print(*it->second.methods, indent + 4);
    std::cout <<std::endl;
    std::cout << genIndent(indent + 2) << "}";
    if (it != --classTable.end())
      std::cout << ",";
    std::cout << std::endl;
  }
  std::cout << genIndent(indent) << "}" << std::endl;
}

void print(ClassTable classTable) {
  print(classTable, 0);
}
