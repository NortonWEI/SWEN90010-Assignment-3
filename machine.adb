with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
      
   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);
   
   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);
   
   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;
      
   procedure IncPC(Ret : out ReturnCode; Offs : in Offset) is
   begin
      if Integer(PC) + Integer(Offs) <= MAX_PROGRAM_LENGTH and Integer(PC) + Integer(Offs) > 0 then
         PC := ProgramCounter(Integer(PC) + Integer(Offs));
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg) is
   begin
      Regs(Rd) := Regs(Rs1) + Regs(Rs2);
--        Ret := Success;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg) is
   begin
      Regs(Rd) := Regs(Rs1) - Regs(Rs2);
--        Ret := Success;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg) is
   begin
      Regs(Rd) := Regs(Rs1) * Regs(Rs2);
--        Ret := Success;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg) is
   begin
      Regs(Rd) := Regs(Rs1) / Regs(Rs2);
--        Ret := Success;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset) is
      A : Addr := Addr(Regs(Rs) + DataVal(Offs));
   begin
      Regs(Rd) := Memory(A);
--        Ret := Success;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg) is
      A : Addr := Addr(Regs(Ra) + DataVal(Offs));   
   begin
      Memory(A) := Regs(Rb);
--        Ret := Success;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset) is
   begin
      Regs(Rd) := DataVal(Offs);
--        Ret := Success;
   end DoMov;
   
   procedure ExecuteProgram(Prog : in Program;
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer) 
   is
      CycleCount : Integer := 0;
      Inst : Instr;
   begin
      Ret := Success;
      PC := ProgramCounter'First;
      Result := 0;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         case Inst.Op is
            when ADD =>
               DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2);
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2);
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2);
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2);
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs);
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb);
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs);
               IncPC(Ret,1);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               IncPC(Ret,Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  IncPC(Ret,Inst.JzOffs);
               else
                  IncPc(Ret,1);
               end if;
            when NOP =>
               IncPC(Ret,1);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
   end ExecuteProgram;
   
   procedure IncDetectionPC(Ret : out ReturnCode; Offs : in Offset; Counter: in out ProgramCounter) is
   begin
      if Integer(Counter) + Integer(Offs) <= MAX_PROGRAM_LENGTH and Integer(Counter) + Integer(Offs) > 0 then
         Counter := ProgramCounter(Integer(Counter) + Integer(Offs));
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
   end IncDetectionPC;
     
   function DynamicAnalyze(Prog : in Program;
                           Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      Ret : ReturnCode := Success;
      
      Counter : ProgramCounter := ProgramCounter'First;
      Regs : array (Reg) of DataVal := (others => 0);
      Memory : array (Addr) of DataVal := (others => 0);
      Address : Addr;
      
   begin
      
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(Counter);
         
         case Inst.Op is
            when ADD =>
               -- register value out of range
               if Integer(Regs(Inst.AddRs1) + Regs(Inst.AddRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.AddRs1) + Regs(Inst.AddRs2)) < Integer(DataVal'First) then
                  Put_Line("1");
                  return True;
               else 
                  Regs(Inst.AddRd) := Regs(Inst.AddRs1) + Regs(Inst.AddRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when SUB =>               
               -- register value out of range
               if Integer(Regs(Inst.SubRs1) - Regs(Inst.SubRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.SubRs1) - Regs(Inst.SubRs2)) < Integer(DataVal'First) then
                  Put_Line("2");
                  return True;
               else
                  Regs(Inst.SubRd) := Regs(Inst.SubRs1) - Regs(Inst.SubRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when MUL =>               
               -- register value out of range
               if Integer(Regs(Inst.MulRs1) * Regs(Inst.MulRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.MulRs1) * Regs(Inst.MulRs2)) < Integer(DataVal'First) then
                  Put_Line("3");
                  return True;
               else
                  Regs(Inst.MulRd) := Regs(Inst.MulRs1) * Regs(Inst.MulRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when DIV =>               
               -- the divisor is 0
               if Integer(Regs(Inst.DivRs2)) = 0 then
                  Put_Line("4");
                  return True;
               else 
                  Regs(Inst.DivRd) := Regs(Inst.DivRs1) / Regs(Inst.DivRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when LDR =>               
               -- memory address out of range
               if Integer(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)) > Integer(Addr'Last) 
                 or Integer(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)) < Integer(Addr'First) then
                  Put_Line("5");
                  return True;
               else
                  -- register value out of range
                  if Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) > Integer(DataVal'Last) 
                    or Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) < Integer(DataVal'First) then
                     Put_Line("6");
                     return True;
                  else
                     Address := Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs));
                   
                     Regs(Inst.LdrRd) := Memory(Address);
                     IncDetectionPC(Ret,1,Counter);
                  end if;
               end if;
            when STR =>
               -- memory address out of range
               if Integer(Regs(Inst.StrRa) + DataVal(Inst.StrOffs)) > Integer(Addr'Last) 
                 or Integer(Regs(Inst.StrRa) + DataVal(Inst.StrOffs)) < Integer(Addr'First) then
                  Put_Line("7");
                  return True;
               else
                  Address := Addr(Regs(Inst.StrRa) + DataVal(Inst.StrOffs));
                  
                  Memory(Address) := Regs(Inst.StrRb);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when MOV =>
               -- register value out of range
               if Integer(Inst.MovOffs) > Integer(DataVal'Last) 
                 or Integer(Inst.MovOffs) < Integer(DataVal'First) then
                  Put_Line("8");
                  return True;
               else
                  Regs(Inst.MovRd) := DataVal(Inst.MovOffs);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when Instruction.RET =>
               --                 Ret := Success;
               return False;
            when JMP =>
               if Integer(Inst.JmpOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                 or Integer(Counter) + Integer(Inst.JmpOffs) <= 0 then
                  -- jump out of the program
                  Put_Line("9");
                  return True;
               elsif Integer(Inst.JmpOffs) = 0 then
                  -- infinite loop
                  return False;
               else
                  IncDetectionPC(Ret,Inst.JmpOffs,Counter);
               end if;
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  if Integer(Inst.JzOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                    or Integer(Counter) + Integer(Inst.JzOffs) <= 0 then
                     -- jump out of the program
                     Put_Line("10");
                     return True;
                  elsif Integer(Inst.JzOffs) = 0 then
                     -- infinite loop
                     return False;
                  else
                     IncDetectionPC(Ret,Inst.JzOffs,Counter);
                  end if;
               else
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when NOP =>
               IncDetectionPC(Ret,1,Counter);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Put_Line("11");
         return True;
      end if;
      
      return False;
   end DynamicAnalyze;

   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      --StaticAnalysisRes : Boolean;
      DynamicAnalysisRes : Boolean;
      
   begin
      --StaticAnalysisRes := StaticAnalyze(Prog, Cycles);
      DynamicAnalysisRes := DynamicAnalyze(Prog, Cycles);
      return DynamicAnalysisRes;
   end DetectInvalidBehaviour;
   
end Machine;
