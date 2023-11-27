.data
	v: 	.word 0 0 0 0 0 0	# array to store the values
	
.text
.globl main

main:
	la 	$t0,v			
	li 	$t1,0			
	
loop:
	slti 	$at,$t1,6			
	beq	$at,$zero,end
	li 	$t2,0	
	li 	$t3,1
	beq 	$t1,$zero,zero		
	jal	fatorial		
	
zero:
	li 	$t3,1			
	
save:
	sw 	$t3,0($t0)		
	addiu 	$t0,$t0,4		
	addiu 	$t1,$t1,1		
	j 	loop			

fatorial:
	slt	$at,$t2,$t1			
	beq	$at,$zero,save
	addiu 	$t2,$t2,1
	multu 	$t3,$t2
	mflo	$t3	
	j 	fatorial

end:
	li 	$v0,10			
