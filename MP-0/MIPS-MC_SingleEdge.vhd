library IEEE;
use IEEE.Std_Logic_1164.all;

package p_MIPS_S is  
    
    -- inst_type define as instru��es decodific�veis pelo bloco de controle
    type inst_type is  
            ( ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV,
				SSRL, SRLV,ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT,
				SLTU, SLTI,	SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, 
				MULTU, DIVU, MFHI, MFLO, invalid_instruction);
 
    type microinstruction is record
            CY1:   std_logic;       -- identificador de primeiro ciclo da instru��o
            CY2:   std_logic;       -- identificador de segundo ciclo da instru��o
            walu:  std_logic;       -- identificador de terceiro ciclo da instru��o
            wmdr:  std_logic;       -- identificador de quarto ciclo da instru��o
            wpc:   std_logic;       -- habilita��o de escrita no PC
            wreg:  std_logic;       -- habilita��o de escrita no Banco de Registradores
            whilo: std_logic;       -- habilita��o de escrita nos registradores HI e LO
            ce:    std_logic;       -- chip enable e controle de escrita/leitura
            rw:    std_logic;
            bw:    std_logic;       -- controle de escrita a byte
            i:     inst_type;       -- c�digo que especifica a instru��o sob execu��o
            rst_md:std_logic;       -- inicializa��o do multiplicador e do divisor
    end record;
         
end p_MIPS_S;


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Registrador de uso geral de 32 bits - sens�vel � borda de subida do
-- rel�gio (ck), com reset ass�ncrono (rst) e habilita��o de escrita (ce)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;

entity reg32bit is
           generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0')
           				);
           port(  ck, rst, ce : in std_logic;
                  D : in  STD_LOGIC_VECTOR (31 downto 0);
                  Q : out STD_LOGIC_VECTOR (31 downto 0)
               );
end reg32bit;

architecture reg32bit of reg32bit is 
begin
  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE;
       elsif ck'event and ck = '1' then
           if ce = '1' then
              Q <= D; 
           end if;
       end if;
  end process;
        
end reg32bit;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Banco de Registradores  (R0..R31) - 31 registradores de 32 bits
-- 	Trata-se de uma mem�ria com tr�s portas de acesso, n�o confundir
--		com a mem�ria principal do processador. 
--		S�o duas portas de leitura (sinais AdRP1+DataRP1 e AdRP2+DataRP2) e
--		uma porta de escrita (controlada pelo conjunto de sinais ck, rst,
--		ce, AdWP e DataWP).
--		Os endere�os de cada porta (AdRP1, AdRP2 e AdWP) s�o obviamente de
--		5 bits (pois 2^5=32 e precisamos de um endere�o distinto para
--		cada um dos 32 registradores), enquanto que os barramentos de dados de 
--		sa�da (DataRP1, DataRP2) e de entrada (DataWP) s�o de 32 bits.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;   
use work.p_MIPS_S.all;

entity reg_bank is
       port( ck, rst, ce :    in std_logic;
             AdRP1, AdRP2, AdWP : in std_logic_vector( 4 downto 0);
             DataWP : in std_logic_vector(31 downto 0);
             DataRP1, DataRP2: out std_logic_vector(31 downto 0) 
           );
end reg_bank;

architecture reg_bank of reg_bank is
   type wirebank is array(0 to 31) of std_logic_vector(31 downto 0);
   signal reg : wirebank ;                            
   signal wen : std_logic_vector(31 downto 0) ;
begin            
    g1: for i in 0 to 31 generate        

        -- Lembrar que o registrador $0 ($zero) � a constante 0, n�o um registrador.
        -- Ele � descrito como um registrador cujo ce nunca � ativado
		-- O sinal wen � o vetor sinais de controle de habilita��o de escrita em cada
		-- um dos 32 registradores: wen(0) � a habilita��o de escrita no registrador 0
		-- (como dito acima, wen(0)='0', sempre. Os demais dependem do valor de AdWP
		-- o do ce global.
        wen(i) <= '1' when i/=0 and AdWP=i and ce='1' else '0';
         
        -- Lembrar que o registrador $29, por conven��o de software � o apontador de
		-- pilha. Ele aponta inicialmente para um lugar diferente do valor usado no MARS.
		-- A pilha aqui � pensada como usando a parte final da mem�ria de dados
        g2: if i=29 generate -- SP ---  x10010000 + x800 -- local do topo da pilha
           r29: entity work.reg32bit generic map(INIT_VALUE=>x"10010800")    
                port map(ck=>ck, rst=>rst, ce=>wen(i), D=>DataWP, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
           rx: entity work.reg32bit 
					port map(ck=>ck, rst=>rst, ce=>wen(i), D=>DataWP, Q=>reg(i));                    
        end generate;
                   
   end generate g1;   
    

    DataRP1 <= reg(CONV_INTEGER(AdRP1));    -- sele��o do fonte 1 

    DataRP2 <= reg(CONV_INTEGER(AdRP2));    -- sele��o do fonte 2 
   
end reg_bank;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - Uma unidade l�gico-aritm�tica puramente combinacional, cuja 
--		sa�da depende dos valores nas suas entradas de dados op1 e op2, cada
--		uma de 32 bits e da instru��o sendo executada pelo processador,
--		que � informada via o sinal de controle op_alu.
--
-- 22/11/2004 (Ney Calazans) - corre��o de um erro sutil para a instru��o J
-- Lembrar que parte do trabalho para a J (c�lculo de endere�o) j� foi
-- iniciado antes, deslocando IR(25 downto 0) para a esquerda em 2 bits
-- antes de escrever dados no registrador R3.
--
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;
use work.p_MIPS_S.all;

entity alu is
       port( op1, op2 : in std_logic_vector(31 downto 0);
             outalu :   out std_logic_vector(31 downto 0);   
             op_alu : in inst_type   
           );
end alu;

architecture alu of alu is 
   signal menorU, menorS : std_logic ;
begin
  
    menorU <=  '1' when op1 < op2 else '0'; -- compara��o de naturais
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- compara��o de inteiros
    
    outalu <=  
		op1 - op2                            when  op_alu=SUBU                     else
        op1 and op2                          when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                          when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                          when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                          when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"           when  op_alu=LUI                      else
        (0=>menorU, others=>'0')             when  op_alu=SLTU  or op_alu=SLTIU    else
        (0=>menorS, others=>'0')             when  op_alu=SLT   or op_alu=SLTI     else
        op1(31 downto 28) & op2(27 downto 0) when  op_alu=J     or op_alu=JAL      else 
        op1                                  when  op_alu=JR    or op_alu=JALR     else 
        to_StdLogicVector(to_bitvector(op1) sll  CONV_INTEGER(op2(10 downto 6)))   when
													op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2) sll  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1) sra  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2) sra  CONV_INTEGER(op1(5 downto 0)))    when  
													op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1) srl  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2) srl  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SRLV   else 
        op1 + op2;    -- default para ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE    

end alu;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Descri��o Estrutural do Bloco de Dados 
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- necess�rio para as instru��es SLTx
use IEEE.Std_Logic_arith.all;  -- necess�rio para as instru��es SLTxU
use work.p_MIPS_S.all;
   
entity datapath is
      port( ck, rst :     in std_logic;
			d_address :   out std_logic_vector(31 downto 0);
			data :        inout std_logic_vector(31 downto 0); 
			inst_branch_out, salta_out : out std_logic;
			end_mul :	   out std_logic;
			end_div :	   out std_logic;
			RESULT_OUT :  out std_logic_vector(31 downto 0);
			uins :        in microinstruction;
			IR_IN :  		in std_logic_vector(31 downto 0);
			NPC_IN : 		in std_logic_vector(31 downto 0)
          );
end datapath;

architecture datapath of  datapath is
    signal result, R1, R2, R3, R1_in, R2_in, R3_in, RIN, sign_extend, op1, op2, 
           outalu, RALU, MDR, mdr_int, HI, LO,
		   quociente, resto, D_Hi, D_Lo : std_logic_vector(31 downto 0) := (others=> '0');
    signal adD, adS : std_logic_vector(4 downto 0) := (others=> '0');    
    signal inst_branch, inst_R_sub, inst_I_sub, rst_muldiv: std_logic;   
    signal salta : std_logic := '0';
    signal produto : std_logic_vector(63 downto 0);
begin

	-- sinais auxiliares 
	inst_branch  <= '1' when uins.i=BEQ or uins.i=BGEZ or uins.i=BLEZ or uins.i=BNE else 
					'0';
	inst_branch_out <= inst_branch;
   
	-- inst_R_sub � um subconjunto das instru��es tipo R 
	inst_R_sub  <= 	'1' when uins.i=ADDU or uins.i=SUBU or uins.i=AAND
							or uins.i=OOR or uins.i=XXOR or uins.i=NNOR else
					'0';

	-- inst_I_sub � um subconjunto das instru��es tipo I
	inst_I_sub  <= '1' when uins.i=ADDIU or uins.i=ANDI or uins.i=ORI or uins.i=XORI else
                   '0';

	--==============================================================================
	-- segundo est�gio
	--==============================================================================
                
	-- A cl�usula "then" aqui s� � usada para deslocamentos com um campo "shamt"       
	M3: adS <=	IR_IN(20 downto 16) when uins.i=SSLL or uins.i=SSRA or uins.i=SSRL else 
				IR_IN(25 downto 21);
          
	REGS: entity work.reg_bank(reg_bank) port map
					(AdRP1=>adS, DataRP1=>R1_in, AdRP2=>IR_IN(20 downto 16), 
					DataRP2=>R2_in, ck=>ck, rst=>rst, ce=>uins.wreg, AdWP=>adD, DataWP=>RIN);
    
	-- extens�o de sinal  
	sign_extend <=  x"FFFF" & IR_IN(15 downto 0) when IR_IN(15)='1' else
					x"0000" & IR_IN(15 downto 0);
    
	-- C�lculo da constante imediata - extens�es e mux M5
	M5: R3_in <= 	sign_extend(29 downto 0)  & "00"     when inst_branch='1' else
					-- ajusta o endere�o de salto para uma fronteira m�ltiplo de palavra
					"0000" & IR_IN(25 downto 0) & "00" when uins.i=J or uins.i=JAL else
					-- J/JAL s�o endere�adas a palavra. Os 4 bits menos significativos s�o definidos
					-- na ALU, n�o aqui!!
					x"0000" & IR_IN(15 downto 0) when uins.i=ANDI or uins.i=ORI  or uins.i=XORI else					-- instru��es l�gicas com operando imediatao usam extens�o de 0 para calcular este
					sign_extend;
					-- O caso "default" (extens�o de sinal) � usado por addiu, lbu, lw, sbu e sw
             
	-- registradores do segundo est�gio 
	R1reg:  entity work.reg32bit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R1_in, Q=>R1);

	R2reg:  entity work.reg32bit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R2_in, Q=>R2);
  
	R3reg: entity work.reg32bit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R3_in, Q=>R3);
 
 
	--==============================================================================
	-- terceiro est�gio
	--==============================================================================
                      
	-- seleciona o primeiro operando da ALU
	M6: op1 <= 	NPC_IN  when (inst_branch='1' or uins.i=J or uins.i=JAL) else R1; 
     
	-- seleciona o segundo operando da ALU
	M7: op2 <= 	R2 when inst_R_sub='1' or uins.i=SLTU or uins.i=SLT or uins.i=JR 
                  or uins.i=SLLV or uins.i=SRAV or uins.i=SRLV else 
		R3; 
                 
	-- instancia��o da ALU
	DALU: entity work.alu port map (op1=>op1, op2=>op2, outalu=>outalu, op_alu=>uins.i);
   
	-- registrador na sa�da da ALU
	ALUreg: entity work.reg32bit  port map(ck=>ck, rst=>rst, ce=>uins.walu, 
				D=>outalu, Q=>RALU);               
 
	-- Avlia��o das condi��es para executar saltos em "branchs"
	salta <=	'1' when ( (R1=R2  and uins.i=BEQ)  or (R1>=0  and uins.i=BGEZ) or
                        (R1<=0  and uins.i=BLEZ) or (R1/=R2 and uins.i=BNE) )  else
				'0';
	salta_out <= salta;
	
	-- reset do multiplicador e do divisor; pode ser via reset global (rst) ou antes do in�cio da
	-- instru��o multu/divu
	rst_muldiv <= rst or uins.rst_md; 
	
	-- instancia��o do multiplicador e do divisor
	inst_mult: entity work.multiplica port map (Mcando=>R1_in, Mcador=>R2_in, clock=>ck,
	  start=>rst_muldiv, endop=>end_mul, produto=>produto);
	  
	inst_div: entity work.divide generic map (32) port map (dividendo=>R1_in, divisor=>R2_in, clock=>ck,
	  start=>rst_muldiv, endop=>end_div, quociente=>quociente, resto=>resto);

	M10: D_Hi <= produto(63 downto 32) when uins.i=MULTU else resto; 
	
	M11: D_Lo <= produto(31 downto 0) when uins.i=MULTU else quociente; 

	-- registradores HI e LO
	HIreg: entity work.reg32bit  port map(ck=>ck, rst=>rst, ce=>uins.whilo, 
			D=>D_Hi, Q=>HI);               
	LOreg: entity work.reg32bit  port map(ck=>ck, rst=>rst, ce=>uins.whilo, 
			D=>D_Lo, Q=>LO);               

   --==============================================================================
   -- quarto est�gio
   --==============================================================================
     
   d_address <= RALU;
    
   -- tristate para controlar a escrita na mem�ria    
   data <= R2 when (uins.ce='1' and uins.rw='0') else (others=>'Z');  

   -- mux M8 escolhe entre passar 32 bits da mem�ria para dentro do processador
   -- ou passar apenas o byte menos significativo com 24 bits de extens�o de 0. 
   -- assume-se aqui como em todos os lugares, que o processador � "little endian"
   M8: mdr_int <= data when uins.i=LW  else
              x"000000" & data(7 downto 0);
       
   RMDR: entity work.reg32bit  port map(ck=>ck, rst=>rst, ce=>uins.wmdr,
			D=>mdr_int, Q=>MDR);                 
  
   M9: result <=	MDR when uins.i=LW  or uins.i=LBU else
                    HI when uins.i=MFHI else
                    LO when uins.i=MFLO else
                    RALU;

   --==============================================================================
   -- quinto est�gio
   --==============================================================================

   -- M2 escolhe o sinal com o dado a ser escrito no banco de registradores
   M2: RIN <= NPC_IN when (uins.i=JALR or uins.i=JAL) else result;
   
   -- M4 seleciona o endere�o de escrita no banco de registradores 
   M4: adD <= "11111" when uins.i=JAL else -- JAL escreve sempre no registrador $31
         IR_IN(15 downto 11) when (inst_R_sub='1' 
					or uins.i=SLTU or uins.i=SLT
					or uins.i=JALR
					or uins.i=MFHI or uins.i=MFLO
					or uins.i=SSLL or uins.i=SLLV
					or uins.i=SSRA or uins.i=SRAV
					or uins.i=SSRL or uins.i=SRLV) else
         IR_IN(20 downto 16) 	-- inst_I_sub='1' ou uins.i=SLTIU ou uins.i=SLTI 
        ;                 		-- ou uins.i=LW ou uins.i=LBU ou uins.i=LUI, ou "default"
    
  RESULT_OUT <= result;
	 
end datapath;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--  Descri��o do Bloco de Controle (mista, estrutural-comportamental)
--------------------------------------------------------------------------
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_unsigned.all;
use work.p_MIPS_S.all;

entity control_unit is
        port(	ck, rst : in std_logic;
				hold: in std_logic;
				inst_branch_in, salta_in : in std_logic;
				end_mul, end_div : in std_logic;
				i_address : out std_logic_vector(31 downto 0);
				instruction : in std_logic_vector(31 downto 0);
				RESULT_IN : in std_logic_vector(31 downto 0);
				uins : out microinstruction;
				IR_OUT : out std_logic_vector(31 downto 0);
				NPC_OUT : out std_logic_vector(31 downto 0)
             );
end control_unit;
                   
architecture control_unit of control_unit is
	type type_state is (Sfetch, Sreg, Salu, Swbk, Sld, Sst, Ssalta); -- 7 estados 
	signal PS, NS : type_state; -- PS = estado atual; NS = pr�ximo estado
	signal i : inst_type;
	signal uins_int : microinstruction;
	signal dtpc, NPC, pc, incpc, IR  : std_logic_vector(31 downto 0);
begin
    ----------------------------------------------------------------------------------------
    -- BLOCO (1 de 4) - Os registradores de controle da MIPS_S.
	-- Busca de instru��o e incremento do PC
    ----------------------------------------------------------------------------------------
	-- M1 seleciona a entrada do registrador PC 
	M1: dtpc <=	RESULT_IN when (inst_branch_in='1' and salta_in='1') or uins_int.i=J
   			or uins_int.i=JAL or uins_int.i=JALR or uins_int.i=JR	else
   		NPC;
   
	NPC_OUT <= NPC;
	
	-- Endere�o de partida onde est� armazenado o programa: cuidado com este valor!
	-- O valor abaixo (x"00400000") serve para c�digo gerado pelo simulador MARS
	PC_reg: entity work.reg32bit generic map(INIT_VALUE=>x"00400000")   
		port map(ck=>ck, rst=>rst, ce=>uins_int.wpc, D=>dtpc, Q=>pc);

	incpc <= pc + 4;
  
	NPCreg: entity work.reg32bit 
		 port map(ck=>ck, rst=>rst, ce=>uins_int.CY1, D=>incpc, Q=>NPC);     
           
	IR_reg:	entity work.reg32bit  
		port map(ck=>ck, rst=>rst, ce=>uins_int.CY1, D=>instruction, Q=>IR);

	IR_OUT <= IR ;    	-- IR � o registrador de instru��es 
             
	i_address <= pc;	-- conecta a sa�da do PC ao barramento de endere�os da mem�ria de
						-- instru��es

    ----------------------------------------------------------------------------------------
    -- BLOCO (2 de 4) - A Decodifia��o de Instru��es
    -- Este bloco gera um sinal, i, um dos sinais da
	-- 	fun��o de sa�da da m�quina de estados de controle do MIPS_S
    ----------------------------------------------------------------------------------------
    i <=   ADDU   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100001" else
           SUBU   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100011" else
           AAND   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100100" else
           OOR    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100101" else
           XXOR   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100110" else
           NNOR   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100111" else
           SSLL   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000000" else
           SLLV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000100" else
           SSRA   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000011" else
           SRAV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000111" else
           SSRL   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000010" else
           SRLV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000110" else
           ADDIU  when IR(31 downto 26)="001001" else
           ANDI   when IR(31 downto 26)="001100" else
           ORI    when IR(31 downto 26)="001101" else
           XORI   when IR(31 downto 26)="001110" else
           LUI    when IR(31 downto 26)="001111" else
           LW     when IR(31 downto 26)="100011" else
           LBU    when IR(31 downto 26)="100100" else
           SW     when IR(31 downto 26)="101011" else
           SB     when IR(31 downto 26)="101000" else
           SLTU   when IR(31 downto 26)="000000" and IR(5 downto 0)="101011" else
           SLT    when IR(31 downto 26)="000000" and IR(5 downto 0)="101010" else
           SLTIU  when IR(31 downto 26)="001011"                             else
           SLTI   when IR(31 downto 26)="001010"                             else
           BEQ    when IR(31 downto 26)="000100" else
           BGEZ   when IR(31 downto 26)="000001" and IR(20 downto 16)="00001" else
           BLEZ   when IR(31 downto 26)="000110" and IR(20 downto 16)="00000" else
           BNE    when IR(31 downto 26)="000101" else
           J      when IR(31 downto 26)="000010" else
           JAL    when IR(31 downto 26)="000011" else
           JALR   when IR(31 downto 26)="000000"  and IR(20 downto 16)="00000"
                                           and IR(10 downto 0) = "00000001001" else
           JR     when IR(31 downto 26)="000000" and IR(20 downto 0)="000000000000000001000" else
           MULTU  when IR(31 downto 26)="000000" and IR(15 downto 0)="0000000000011001" else
           DIVU   when IR(31 downto 26)="000000" and IR(15 downto 0)="0000000000011011" else
           MFHI   when IR(31 downto 16)=x"0000" and IR(10 downto 0)="00000010000" else
           MFLO   when IR(31 downto 16)=x"0000" and IR(10 downto 0)="00000010010" else
           invalid_instruction ; -- IMPORTANTE: a condi��o else de tudo � invalid instruction
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
    uins_int.i <= i;    -- isto instrui a ALU a executar a opera��o dela esperada, se houver alguma
						-- tamb�m controla a opera��o de praticamente todos os multiplexadores
						--		que definem os caminhos a usar no bloco de dados e no bloco de controle

    ---------------------------------------------------------------------------------------------
    -- BLOCO (3 de 4) - Dois Comandos process VHDL que respectivamente geram:
	--					1) O Registrador de Estados da M�quina de Estados de Controle do MIPS_S
	--					2) A Fun��o de Transi��o da M�quina de Estados de Controle do MIPS_S
	--	O Registrador de Estados � o �nico  hardware sequencial da M�quina de Estados
	--	A Fun��o de Transi��o e uma �nica fun��o Booleana combinacional que gera o pr�ximo estado
	--		da M�quina de Estados
    --------------------------------------------------------------------------------------------- 
    process(rst, ck)
    begin
        if rst='1' then
            PS <= Sfetch; -- Sfetch � o estado em que a m�quina fica enquanto o processador est� sendo ressetado
        elsif ck'event and ck='1' then
            if hold = '0' then -- Quando não estiver em hold, a máquina de estados avança // Mudança feita para criar o MP-0
                PS <= NS;
            end if;
        end if;
    end process;
     
     
    process(PS, i, end_mul, end_div)
    begin
       case PS is         
            -- Sfetch ativa o primeiro est�gio: busca-se a instru��o apontada pelo PC e incrementa-se ele
            when Sfetch=>NS <= Sreg;  
     
            -- SReg ativa o segundo est�gio: busca-se os operandos fonte da maioria das instru��es
			--		registradores, dados imediatos e valores para computar endere�os de salto
            when Sreg=>NS <= Salu;  
             
            -- Salu ativa o terceiro est�gio: opera��o na ALU, no comparador ou no multiplicador ou no divisor
            when Salu =>if (i=LBU or i=LW) then 
										NS <= Sld;  
								elsif (i=SB or i=SW) then 
										NS <= Sst;
								elsif (i=J or i=JAL or i=JALR or i=JR or i=BEQ
                               or i=BGEZ or i=BLEZ  or i=BNE) then 
										NS <= Ssalta;
								elsif ((i=MULTU and end_mul='0') or (i=DIVU and end_div='0')) then
										NS <= Salu;
								elsif ((i=MULTU and end_mul='1') or (i=DIVU and end_div='1')) then
										NS <= Sfetch;
								else 
										NS <= Swbk; 
								end if;
                         
            -- Sld ativa o quarto est�gio: opera��o na mem�ria de dados, apenas quando a instru��o sendo
			--		requer tal tipo de a��o
            when Sld=>  NS <= Swbk; 
            
            -- Sst/Ssalta/ Swbk ativam o quarto ou quinto est�gio: 
			--		�ltimo est�gio para a maioria das instru��es
            when Sst | Ssalta | Swbk=> 
								NS <= Sfetch;
  
       end case;

    end process;
	
	----------------------------------------------------------------------------------------
    -- BLOCO (4 de 4) - Fun��o de Sa�da da M�quina de Estados de Controle do MIPS_S
	--	Gera 11 sinais  de controle - a maioria destes s�o sinais de habilita��o 
	--	de escrita em registradores dos blocos de dados e de controle
    -- 	Tamb�m gera os sinais de controle de acesso a mem�ria de dados e o sinal de
	--	inicializa��o dos blocos de multiplica��o e divis�o
	--		Ao todo, s�o 11 fun��es Booleanas, uma para cada sinal de controle
    ----------------------------------------------------------------------------------------
    uins_int.CY1   <= '1' when PS=Sfetch         else '0';
            
    uins_int.CY2   <= '1' when PS=Sreg           else '0';
  
    uins_int.walu  <= '1' when PS=Salu           else '0';
                
    uins_int.wmdr  <= '1' when PS=Sld            else '0';
  
    uins_int.wreg   <= '1' when PS=Swbk or (PS=Ssalta and (i=JAL or i=JALR)) else   '0';
   
    uins_int.rw    <= '0' when PS=Sst            else  '1';
                  
    uins_int.ce    <= '1' when PS=Sld or PS=Sst  else '0';
    
    uins_int.bw    <= '0' when PS=Sst and i=SB   else '1';
      
    uins_int.wpc   <= '1' when PS=Swbk or PS=Sst or PS=Ssalta 
	 		or (PS=Salu and ((i=MULTU and end_mul='1')
			or (i=DIVU and end_div='1'))) else  '0';

    uins_int.whilo   <= '1' when (PS=Salu and end_mul='1' and i=MULTU)
			  or (PS=Salu and end_div='1' and i=DIVU) 
			else  '0';

    uins_int.rst_md   <= '1' when PS=Sreg and (i=MULTU or i=DIVU) else  '0';

	uins <= uins_int;
    
end control_unit;

--------------------------------------------------------------------------
-- Processador MIPS_S completo
-- Aqui se instanciam o Bloco de Dados (datapath) e
-- o Bloco de Controle (control_unit) e conectam estes blocos entre si
-- e com os pinos externos do processador
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MIPS_S.all;

entity MIPS_S is
    port( clock, reset: in std_logic;
    	  hold: in std_logic;
          ce, rw, bw: out std_logic;
          i_address, d_address: out std_logic_vector(31 downto 0);
          instruction: in std_logic_vector(31 downto 0);
          data: inout std_logic_vector(31 downto 0));
end MIPS_S;

architecture MIPS_S of MIPS_S is
		signal IR, NPC, RESULT: std_logic_vector(31 downto 0);
		signal uins: microinstruction;  
		signal inst_branch, salta, end_mul, end_div: std_logic;
 begin

     dp: entity work.datapath   
         port map(ck=>clock, rst=>reset, d_address=>d_address, data=>data,
		  inst_branch_out=>inst_branch, salta_out=>salta,
		  end_mul=>end_mul, end_div=>end_div, RESULT_OUT=>RESULT,
		  uins=>uins, IR_IN=>IR, NPC_IN=>NPC);

     ct: entity work.control_unit port map( ck=>clock, rst=>reset, hold=>hold, 
		i_address=>i_address, instruction=>instruction,
		inst_branch_in=>inst_branch, salta_in=>salta, 
		end_mul=>end_mul, end_div=>end_div, RESULT_IN=>RESULT,
		uins=>uins, IR_OUT=>IR, NPC_OUT=>NPC);
         
     ce <= uins.ce;
     rw <= uins.rw; 
     bw <= uins.bw;
     
end MIPS_S;