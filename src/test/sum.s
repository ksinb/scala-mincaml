.data
.balign	8
.text
sum.6:
	cmpl	$0, %eax
	jg	jle_else.15
	movl	$0, %eax
	ret
jle_else.15:
	movl	%eax, %ebx
	subl	$1, %ebx
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	sum.6
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	addl	%ebx, %eax
	ret
.global	min_caml_start
mincaml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp), %ebp
	movl	36(%esp), %eax
	movl	%eax,min_caml_hp
	movl	$10000, %eax
	call	sum.6
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
