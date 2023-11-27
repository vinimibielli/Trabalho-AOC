.data
            .align 4 # alinhamento de memória
         m1:         .asciiz "\nDigite um número inteiro:\t"
         m2:         .asciiz "\nM[linha][coluna]:\t"
         M:        .word 0:16    # inicializa todos os elementos da matriz com zero
         tamanho:     .word 16    # tamanho da matriz         
 
.text
    main:
        # setando a pilha de chamada de procedimentos
        subu     $sp, $sp, 32     # o frame de pilha tenm 32 bytes
        sw     $ra, 20($sp)     # salva o endereço de retorno
        sw     $fp, 16($sp)     # salva o ponteiro do frame
        addiu     $sp, $sp, 28      # prepara o ponteiro do frame            
            
            jal matriz_preenche
            jal matriz_imprime 
           
            # re-setando a pilha de chamada de procedimentos
        lw     $ra, 20($sp)       # restaura o endereço
        lw     $fp, 16($sp)       # restaura o frame pointer
        addiu     $sp, $sp, 32       # remove do frame        
        j FIM                       # finaliza o programa        
 
matriz_preenche:
        # configurações da pilha
        subu  $sp, $sp, 32   # reserva o espaço do frame ($sp)    
        sw    $ra, 20($sp)   # salva o endereço de retorno ($ra)    
        sw    $fp, 16($sp)   # salva o frame pointer ($fp)    
        addiu $fp, $sp, 28   # prepara o frame pointer    
        sw    $a0, 0($fp)    # salva o argumento ($a0)    
 
        li       $t0, 4       # $t0: número de linhas
               li       $t1, 4       # $t1: número de colunas
            move     $s0, $zero   # $s0: contador da linha
            move     $s1, $zero   # $s1: contador da coluna
            move     $t2, $zero   # $t2: valor a ser lido/armazenado
            
    popula_matriz:            
        # Cada iteração de loop armazenará o valor de $t1 incrementado no próximo elemento da matriz
        # O deslocamento é calculado a cada iteração: deslocamento = 4 * (linha * número de colunas + coluna)
            
        # calcula o endereço correto do array
        mult     $s0, $t1    # $s2 = linha * numero de colunas 
            mflo     $s2            # move o resultado da multiplicação do registrador lo para $s2
            add      $s2, $s2, $s1  # $s2 += contador de coluna
               sll      $s2, $s2, 2    # $s2 *= 4 (deslocamento 2 bits para a esquerda) para deslocamento de byte       
        
            # solicita que o usuário digite um número inteiro
            li     $v0, 4        
        la     $a0, m1        
        syscall                        
        li     $v0, 5        
        syscall            
        move     $t2, $v0        
    
        # armazena o valor digitado pelo usuário
        sw    $t2, M($s2)         
            
            # incrementa o contador
            addi     $t2, $t2, 1 
        
        # Controle de loop: 
        # se incrementarmos além da última coluna, redefinir o contador de coluna 
        # e incrementar o contador de linha
        # se incrementarmos além da última linha, terminaremos. 
            addi    $s1, $s1, 1            # incrementa contador da coluna
            bne     $s1, $t1, popula_matriz       # não é o fim da linha então LOOP
            move    $s1, $zero             # reseta o contador da coluna
            addi    $s0, $s0, 1            # incrementa o contador da linha
            bne     $s0, $t0, popula_matriz       # não é o fim da matriz então LOOP
            jr     $ra
        
    # configurações do procedimento    
    add     $v0, $s1, $zero # retorna para quem chamou    
    jr     $ra    
    
matriz_imprime:
 
    # configurações da pilha
    subu  $sp, $sp, 32   # reserva o espaço do frame ($sp)    
    sw    $ra, 20($sp)   # salva o endereço de retorno ($ra)    
    sw    $fp, 16($sp)   # salva o frame pointer ($fp)    
    addiu $fp, $sp, 28   # prepara o frame pointer    
    sw    $a0, 0($fp)    # salva o argumento ($a0)    
 
    li       $t0, 4       # $t0: número de linhas
           li       $t1, 4       # $t1: número de colunas
           move     $s0, $zero   # $s0: contador da linha
        move     $s1, $zero   # $s1: contador da coluna
        move     $t2, $zero   # $t2: valor a ser lido/armazenado
        
    imprime_matriz:    
        # calcula o endereço correto do array
        mult     $s0, $t1    # $s2 = linha * numero de colunas 
            mflo     $s2            # move o resultado da multiplicação do registrador lo para $s2
            add      $s2, $s2, $s1  # $s2 += contador de coluna
            sll      $s2, $s2, 2    # $s2 *= 4 (deslocamento 2 bits para a esquerda) para deslocamento de byte    
    
        # obtem o valor do elemento armazenado
        lw    $t2, M($s2)         
    
        # imprime no console o valor do elemento da matriz
        li     $v0, 4   
        la     $a0, m2
        syscall     
        li     $v0,1    
        move     $a0, $t2
        syscall 
            
            # incrementa o contador
            addi     $t2, $t2, 1 
      
              addi     $s1, $s1, 1            # increment column counter
        bne      $s1, $t1, imprime_matriz       # not at end of row so loop back
        move     $s1, $zero             # reset column counter
        addi     $s0, $s0, 1            # increment row counter
        bne      $s0, $t0, imprime_matriz      # not at end of matrix so loop back
        jr $ra
        
       # configurações do procedimento    
    add     $v0, $s1, $zero # retorna para quem chamou    
    jr     $ra
    
FIM: 
        li $v0, 10        
        syscall                 