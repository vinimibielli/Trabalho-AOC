.data
fibs:.word   0 : 19         
size: .word  19             
prompt: .asciiz "How many Fibonacci numbers to generate? (2 <= x <= 19)"
.text
      la   $s0, fibs        
      la   $s5, size       
      lw   $s5, 0($s5)     

      
      li   $s2, 1           
      sw   $s2, 0($s0)     
      sw   $s2, 4($s0)     
      addiu $s1, $s5, -2     
      
     
loop: lw   $s3, 0($s0)     
      lw   $s4, 4($s0)      
      add  $s2, $s3, $s4    
      sw   $s2, 8($s0)     
      addiu $s0, $s0, 4      
      addiu $s1, $s1, -1     
      bgtz $s1, loop        
      
     
      la   $a0, fibs        
      add  $a1, $zero, $s5  
      jal  print            

     
      li   $v0, 10          # system call for exit
      		             # Exit!
		

      .data
space:.asciiz  " "          
head: .asciiz  "The Fibonacci numbers are:\n"
      .text
print:add  $t0, $zero, $a0  
      add  $t1, $zero, $a1  
      la   $a0, head       
      li   $v0, 4          
                  
      
out:  lw   $a0, 0($t0)      
      li   $v0, 1           
      
      la   $a0, space       
      li   $v0, 4           
      
      addiu $t0, $t0, 4      
      addiu $t1, $t1, -1     
      bgtz $t1, out         
      
      jr   $ra              

