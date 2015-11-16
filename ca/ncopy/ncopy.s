	.file	"ncopy.c"
	.text
	.globl	ncopy
	.type	ncopy, @function
ncopy:
.LFB61:
	.cfi_startproc
	pushl	%edi
	.cfi_def_cfa_offset 8
	.cfi_offset 7, -8
	pushl	%esi
	.cfi_def_cfa_offset 12
	.cfi_offset 6, -12
	pushl	%ebx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movl	16(%esp), %esi
	movl	20(%esp), %edi
	movl	24(%esp), %ebx
	testl	%ebx, %ebx
	jle	.L4
	movl	$0, %edx
	movl	$0, %eax
.L3:
	movl	(%esi,%edx,4), %ecx
	movl	%ecx, (%edi,%edx,4)
	testl	%ecx, %ecx
	setg	%cl
	movzbl	%cl, %ecx
	addl	%ecx, %eax
	addl	$1, %edx
	cmpl	%ebx, %edx
	jne	.L3
	jmp	.L2
.L4:
	movl	$0, %eax
.L2:
	popl	%ebx
	.cfi_restore 3
	.cfi_def_cfa_offset 12
	popl	%esi
	.cfi_restore 6
	.cfi_def_cfa_offset 8
	popl	%edi
	.cfi_restore 7
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc
.LFE61:
	.size	ncopy, .-ncopy
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"count=%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB62:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	andl	$-16, %esp
	subl	$16, %esp
	movl	$10, 8(%esp)
	movl	$0, 4(%esp)
	movl	12(%ebp), %eax
	movl	4(%eax), %eax
	movl	%eax, (%esp)
	call	strtol
	testl	%eax, %eax
	jle	.L7
	movl	$0, %edx
.L8:
	addl	$1, %edx
	movl	%edx, src-4(,%edx,4)
	cmpl	%eax, %edx
	jne	.L8
.L7:
	movl	%eax, 8(%esp)
	movl	$dst, 4(%esp)
	movl	$src, (%esp)
	call	ncopy
	movl	%eax, 8(%esp)
	movl	$.LC0, 4(%esp)
	movl	$1, (%esp)
	call	__printf_chk
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE62:
	.size	main, .-main
	.comm	dst,400000000,32
	.comm	src,400000000,32
	.ident	"GCC: (Ubuntu 4.8.4-2ubuntu1~14.04) 4.8.4"
	.section	.note.GNU-stack,"",@progbits
