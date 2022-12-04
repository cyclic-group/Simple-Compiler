.data
printstr: .asciz "%d\n"

.text
.globl Main_main
Main_main:
#### METHOD BODY
   push %ebp
   movl %esp, %ebp
   push %ebx
   push %esi
   push %edi
   add  $0, %esp
#### BOOLEAN LITERAL
   push $1
   pop %eax
   mov $0, %ebx
   cmp %eax, %ebx
   je L1
#### BOOLEAN LITERAL
   push $0
   jmp L2
L1:
#### BOOLEAN LITERAL
   push $1
   pop %eax
   mov $0, %ebx
   cmp %eax, %ebx
   je L3
#### BOOLEAN LITERAL
   push $0
   jmp L4
L3:
#### BOOLEAN LITERAL
   push $1
L4:
L2:
#### PRINT
   push $printstr
   call printf
   add  $8, %esp
#### BOOLEAN LITERAL
   push $1
   pop %eax
   mov $0, %ebx
   cmp %eax, %ebx
   je L7
#### BOOLEAN LITERAL
   push $0
   jmp L8
L7:
#### BOOLEAN LITERAL
   push $1
L8:
   pop %eax
   mov $0, %ebx
   cmp %eax, %ebx
   je L5
#### BOOLEAN LITERAL
   push $0
   jmp L6
L5:
#### BOOLEAN LITERAL
   push $1
L6:
#### PRINT
   push $printstr
   call printf
   add  $8, %esp
   add  $0, %esp
   pop  %edi
   pop  %esi
   pop  %ebx
   pop  %ebp
   ret

