library IEEE;
use IEEE.Std_Logic_1164.all;
use std.textio.all;
package aux_functions is  

	subtype wires32  is std_logic_vector(31 downto 0);
	subtype wires16  is std_logic_vector(15 downto 0);
	subtype wires8   is std_logic_vector( 7 downto 0);
	subtype wires4   is std_logic_vector( 3 downto 0);

   -- defini��o do tipo 'memory', que ser� utilizado para as mem�rias de dados/instru��es
   constant MEMORY_SIZE : integer := 2048;     
   type memory is array (0 to MEMORY_SIZE) of wires8;

   constant TAM_LINHA : integer := 200;
   
   function CONV_VECTOR( letra : string(1 to TAM_LINHA);  pos: integer ) return std_logic_vector;
	
	procedure readFileLine(file in_file: TEXT; outStrLine: out string);
   
end aux_functions;

package body aux_functions is

  --
  -- converte um caracter de uma dada linha em um std_logic_vector
  --
  function CONV_VECTOR( letra:string(1 to TAM_LINHA);  pos: integer ) return std_logic_vector is         
     variable bin: wires4;
   begin
      case (letra(pos)) is  
              when '0' => bin := "0000";
              when '1' => bin := "0001";
              when '2' => bin := "0010";
              when '3' => bin := "0011";
              when '4' => bin := "0100";
              when '5' => bin := "0101";
              when '6' => bin := "0110";
              when '7' => bin := "0111";
              when '8' => bin := "1000";
              when '9' => bin := "1001";
              when 'A' | 'a' => bin := "1010";
              when 'B' | 'b' => bin := "1011";
              when 'C' | 'c' => bin := "1100";
              when 'D' | 'd' => bin := "1101";
              when 'E' | 'e' => bin := "1110";
              when 'F' | 'f' => bin := "1111";
              when others =>  bin := "0000";  
      end case;
     return bin;
  end CONV_VECTOR;

  procedure readFileLine(file in_file: TEXT; 
					      outStrLine: out string) is
		
		variable localLine: line;
		variable localChar:  character;
		variable isString: 	boolean;
			
	begin
				
		 readline(in_file, localLine);

		 for i in outStrLine'range loop
			 outStrLine(i) := ' ';
		 end loop;   

		 for i in outStrLine'range loop
			read(localLine, localChar, isString);
			outStrLine(i) := localChar;
			if not isString then -- encontrou o fim da linha
				exit;
			end if;   
		 end loop; 
						 
	end readFileLine;
	
end aux_functions;     

--------------------------------------------------------------------------
-- M�dulo que implementa um modelo comportamental de uma RAM
-- com interface ass�ncrona (sem clock)
--------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;
use std.textio.all;
use work.aux_functions.all;

entity RAM_mem is
      generic(  START_ADDRESS: wires32 := (others=>'0')  );
      port( ce_n, we_n, oe_n, bw: in std_logic;    address: in wires32;   data: inout wires32);
end RAM_mem;

architecture RAM_mem of RAM_mem is 
   signal RAM : memory;
   signal tmp_address: wires32;
   alias  low_address: wires16 is tmp_address(15 downto 0);    --  baixa para 16 bits, devido ao CONV_INTEGER --
begin     

   tmp_address <= address - START_ADDRESS;   --  offset do endereamento  -- 
   
   -- escrita ass�ncrona na mem�ria  -- LITTLE ENDIAN -------------------
   process(ce_n, we_n, low_address) -- Modifica��o em 16/05/2012 para uso
                                    -- processadores monociclo apenas
     begin
       if ce_n='0' and we_n='0' then
          if CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address)<=MEMORY_SIZE-3 then
               if bw='1' then
                   RAM(CONV_INTEGER(low_address+3)) <= data(31 downto 24);
                   RAM(CONV_INTEGER(low_address+2)) <= data(23 downto 16);
                   RAM(CONV_INTEGER(low_address+1)) <= data(15 downto  8);
               end if;
               RAM(CONV_INTEGER(low_address  )) <= data( 7 downto  0); 
          end if;
         end if;   
    end process;   
    
   -- leitura da mem�ria
   process(ce_n, oe_n, low_address)
     begin
       if ce_n='0' and oe_n='0' and
          CONV_INTEGER(low_address)>=0 and CONV_INTEGER(low_address)<=MEMORY_SIZE-3 then
            data(31 downto 24) <= RAM(CONV_INTEGER(low_address+3));
            data(23 downto 16) <= RAM(CONV_INTEGER(low_address+2));
            data(15 downto  8) <= RAM(CONV_INTEGER(low_address+1));
            data( 7 downto  0) <= RAM(CONV_INTEGER(low_address  ));
        else
            data(31 downto 24) <= (others=>'Z');
            data(23 downto 16) <= (others=>'Z');
            data(15 downto  8) <= (others=>'Z');
            data( 7 downto  0) <= (others=>'Z');
        end if;
   end process;   

end RAM_mem;

-------------------------------------------------------------------------
--  Testebench para simular a CPU do processador
-------------------------------------------------------------------------
library ieee;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;          
use STD.TEXTIO.all;
use work.aux_functions.all;

entity CPU_tb is
end CPU_tb;

architecture cpu_tb of cpu_tb is
    
    signal Dadress, Ddata, Iadress, Idata,
           i_cpu_address, d_cpu_address, data_cpu, tb_add, tb_data : wires32 := (others => '0' );
    
    signal Dce_n, Dwe_n, Doe_n, Ice_n, Iwe_n, Ioe_n, ck, rst, rstCPU, hold,
           go_i, go_d, ce, rw, bw: std_logic;
    
    signal hold_MP : wires8 := (others => '0');
    
    file ARQ : TEXT open READ_MODE is "Test_Program_Allinst_MIPS_MCS.txt";
 
begin
           
    Data_mem:  entity work.RAM_mem 
               generic map( START_ADDRESS => x"10010000" )
               port map (ce_n=>Dce_n, we_n=>Dwe_n, oe_n=>Doe_n, bw=>bw, address=>Dadress, data=>Ddata);
                                            
    Instr_mem: entity work.RAM_mem 
               generic map( START_ADDRESS => x"00400000" )
               port map (ce_n=>Ice_n, we_n=>Iwe_n, oe_n=>Ioe_n, bw=>'1', address=>Iadress, data=>Idata);
        
            process(rst, ck)
            begin
                if rst='1' then
                    hold <= '1';
                elsif ck'event and ck='1' then
                    if(Dce_n /= '0') then
                        if(hold_MP /= "111") then
                            hold <= '1';
                            hold_MP <= hold_MP + '1';
                        else
                            hold_MP <= "000";
                            hold <= '0';
                        end if;
                    else
                        hold_MP <= "000";
                        hold <= '0';
                    end if;
                end if;
            end if;
                end process;
                                   
    -- sinais para adaptar a mem�ria de dados ao processador ---------------------------------------------
    Dce_n <= '0' when (ce='1' and rstCPU/='1') or go_d='1' else '1'; -- Bug corrected here in 16/05/2012
    Doe_n <= '0' when (ce='1' and rw='1')             else '1';       
    Dwe_n <= '0' when (ce='1' and rw='0') or go_d='1' else '1';    

    Dadress <= tb_add  when rstCPU='1' else d_cpu_address;
    Ddata   <= tb_data when rstCPU='1' else data_cpu when (ce='1' and rw='0') else (others=>'Z'); 
    
    data_cpu <= Ddata when (ce='1' and rw='1') else (others=>'Z');
    
    -- sinais para adaptar a mem�ria de instru��es ao processador ---------------------------------------------
    Ice_n <= '0';                                 
    Ioe_n <= '1' when rstCPU='1' else '0';           -- impede leitura enquanto est� escrevendo                             
    Iwe_n <= '0' when go_i='1'   else '1';           -- escrita durante a leitura do arquivo 
    
    Iadress <= tb_add  when rstCPU='1' else i_cpu_address;
    Idata   <= tb_data when rstCPU='1' else (others => 'Z'); 
  

    cpu: entity work.MIPS_S  port map(
              clock=>ck, reset=>rstCPU,	hold=>hold,
              i_address => i_cpu_address,
              instruction => Idata,
              ce=>ce,  rw=>rw,  bw=>bw,
              d_address => d_cpu_address,
              data => data_cpu
        ); 

    rst <='1', '0' after 15 ns;       -- gera o sinal de reset global 

    process                          -- gera o clock global 
        begin
        ck <= '1', '0' after 10 ns;
        wait for 20 ns;
    end process;

    
    ----------------------------------------------------------------------------
    -- Este processo carrega a mem�ria de instru��es e a mem�ria de dados
    -- durante o per�odo que o reset fica ativo
    --
    --
    --   O PROCESSO ABAIXO � UM PARSER PARA LER C�DIGO GERADO PELO MARS NO
    --   SEGUINTE FORMATO:
    --
    --      Text Segment
    --      0x00400000        0x3c011001  lui $1, 4097 [d2]               ; 16: la    $t0, d2
    --      0x00400004        0x34280004  ori $8, $1, 4 [d2]
    --      0x00400008        0x8d080000  lw $8, 0($8)                    ; 17: lw    $t0,0($t0)
    --      .....
    --      0x00400048        0x0810000f  j 0x0040003c [loop]             ; 30: j     loop
    --      0x0040004c        0x01284821  addu $9, $9, $8                 ; 32: addu $t1, $t1, $t0
    --      0x00400050        0x08100014  j 0x00400050 [x]                ; 34: j     x
    --      Data Segment
    --      0x10010000        0x0000faaa  0x00000083  0x00000000  0x00000000
    --
    ----------------------------------------------------------------------------
    process
        variable ARQ_LINE : LINE;
        variable line_arq : string(1 to TAM_LINHA);
        variable code     : boolean;
        variable i, address_flag : integer;
    begin  
        go_i <= '0';
        go_d <= '0';
        rstCPU <= '1';           -- segura o processador durante a leitura do arquivo
        code:=true;              -- valor default de code � true (leitura de instru��es)
                                 
        wait until rst = '1';
        
        while NOT (endfile(ARQ)) loop    -- INCIO DA LEITURA DO ARQUIVO CONTENDO INSTRU��ES E DADOS -----
            readFileLine(ARQ, line_arq);            
            if line_arq(1 to 12)="Text Segment" then 
                   code:=true;                     -- instru��es 
            elsif line_arq(1 to 12)="Data Segment" then
                   code:=false;                    -- dados
            else 
               i := 1;                  -- LEITURA DE LINHA - analisar o la�o abaixo para entender 
               address_flag := 0;       -- para INSTRU��ES  um par (end,inst)
                                        -- para DADOS aceita (end dado 0 dado 1 dado 2 ....)
               loop                                     
                  if line_arq(i) = '0' and line_arq(i+1) = 'x' then -- encontrou indica�� de n�mero hexa: '0x'
                         i := i + 2;
                         if address_flag=0 then
                               for w in 0 to 7 loop
                                   tb_add( (31-w*4) downto (32-(w+1)*4))  <= CONV_VECTOR(line_arq,i+w);
                               end loop;    
                               i := i + 8; 
                               address_flag := 1;
                         else
                               for w in 0 to 7 loop
                                   tb_data( (31-w*4) downto (32-(w+1)*4))  <= CONV_VECTOR(line_arq,i+w);
                               end loop;    
                               i := i + 8;
                               
                               wait for 0.1 ns;
                               
                               if code=true then go_i <= '1';    
                               -- O sinal go_i habilita escrita na mem�ria de instru��es
                                            else go_d <= '1';    
                               -- O sinal go_d habilita escrita na mem�ria de dados
                               end if; 
                               
                               wait for 0.1 ns;
                               
                               tb_add <= tb_add + 4; -- OK, consigo ler mais de um dado por linha!
                               go_i <= '0';
                               go_d <= '0'; 
                               
                               address_flag := 2;    -- sinaliza que j� leu o conte�do do endere�o;

                         end if;
                  end if;
                  i := i + 1;
                  
                  -- sai da linha quando chegou no seu final OU j� leu par (endereo, instru��o) 
                  --    no caso de instru��es
                  exit when i=TAM_LINHA or (code=true and address_flag=2);
               end loop;
            end if;
            
        end loop;       -- FINAL DA LEITURA DO ARQUIVO CONTENDO INSTRU��ES E DADOS -----
        
        rstCPU <= '0' after 2 ns;   -- libera o processador para executar o programa
        wait for 4 ns;              -- espera um pouco antes de come�ar a esperar pelo rst de novo
        wait until rst = '1';       -- Se isto acontecer come�a de novo!!
        
    end process;
    
end cpu_tb;
