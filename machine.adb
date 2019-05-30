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
               if Long_Integer(Regs(Inst.AddRs1)) + Long_Integer(Regs(Inst.AddRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.AddRs1)) + Long_Integer(Regs(Inst.AddRs2)) < Long_Integer(DataVal'First) then
                  Ret := IllegalProgram;
               else
                  DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2);
                  IncPC(Ret,1);
               end if;
            when SUB =>
               if Long_Integer(Regs(Inst.SubRs1)) - Long_Integer(Regs(Inst.SubRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.SubRs1)) - Long_Integer(Regs(Inst.SubRs2)) < Long_Integer(DataVal'First) then
                  Ret := IllegalProgram;
               else
                  DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2);
                  IncPC(Ret,1);
               end if;
            when MUL =>
               if Long_Integer(Regs(Inst.MulRs1)) * Long_Integer(Regs(Inst.MulRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.MulRs1)) * Long_Integer(Regs(Inst.MulRs2)) < Long_Integer(DataVal'First) then
                  Ret := IllegalProgram;   
               else
                  DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2);
                  IncPC(Ret,1);
               end if;
            when DIV =>
               if Integer(Regs(Inst.DivRs2)) = 0 then
                  Ret := IllegalProgram;
               elsif (Integer(Regs(Inst.DivRs2)) = -1 and Integer(Regs(Inst.DivRs1)) = Integer(DataVal'First)) then
                  Ret := IllegalProgram;
               else
                  DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2);
                  IncPC(Ret,1);
               end if;
            when LDR =>
               if Long_Integer(Regs(Inst.LdrRs)) + Long_Integer(DataVal(Inst.LdrOffs)) > Long_Integer(Addr'Last) 
                 or Long_Integer(Regs(Inst.LdrRs)) + Long_Integer(DataVal(Inst.LdrOffs)) < Long_Integer(Addr'First) then
                  Ret := IllegalProgram;
               else
                  if Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) > Integer(DataVal'Last) 
                    or Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) < Integer(DataVal'First) then
                     Ret := IllegalProgram;
                  else
                     DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs);
                     IncPC(Ret,1);
                  end if;
               end if;
            when STR =>
               if Long_Integer(Regs(Inst.StrRa)) + Long_Integer(DataVal(Inst.StrOffs)) > Long_Integer(Addr'Last) 
                 or Long_Integer(Regs(Inst.StrRa)) + Long_Integer(DataVal(Inst.StrOffs)) < Long_Integer(Addr'First) then
                  Ret := IllegalProgram;
               else
                  DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb);
                  IncPC(Ret,1);
               end if;
            when MOV =>
               if Integer(Inst.MovOffs) > Integer(DataVal'Last) 
                 or Integer(Inst.MovOffs) < Integer(DataVal'First) then
                  Ret := IllegalProgram;
               else
                  DoMov(Inst.MovRd,Inst.MovOffs);
                  IncPC(Ret,1);
               end if;
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               if Integer(Inst.JmpOffs) + Integer(PC) >= MAX_PROGRAM_LENGTH 
                 or Integer(PC) + Integer(Inst.JmpOffs) <= 0 then
                  -- jump out of the program
                  Ret := IllegalProgram;
               elsif Integer(Inst.JmpOffs) = 0 then
                  -- infinite loop
                  Ret := CyclesExhausted;
               else
                  IncPC(Ret,Inst.JmpOffs);
               end if;
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  if Integer(Inst.JzOffs) + Integer(PC) >= MAX_PROGRAM_LENGTH 
                    or Integer(PC) + Integer(Inst.JzOffs) <= 0 then
                     -- jump out of the program
                     Ret := IllegalProgram;
                  elsif Integer(Inst.JzOffs) = 0 then
                     -- infinite loop
                     Ret := CyclesExhausted;
                  else
                     IncPC(Ret,Inst.JzOffs);
                  end if;
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
   
   function StaticAnalyze(Prog : in Program;
                          Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      RetExist : Boolean := False;
      
      Result : Boolean := False;
   begin
      
      while (CycleCount < Cycles) loop
         -- just check the program line by line
         if CycleCount < MAX_PROGRAM_LENGTH and CycleCount >= 0 then
            Inst := Prog(ProgramCounter(CycleCount + 1));

            case Inst.Op is
            when Instruction.RET =>
               RetExist := True;
            when others =>
               null;
            end case;
            CycleCount := CycleCount + 1;
         end if;
      end loop;
      
      if not RetExist then
         -- no RET in the program
         Result := True;
      end if;
      
      return Result;
   end StaticAnalyze;

   
   function DynamicAnalyze(Prog : in Program;
                           Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      Ret : ReturnCode := Success;
      
      Counter : ProgramCounter := ProgramCounter'First;
      Regs : array (Reg) of DataVal := (others => 0);
      Memory : array (Addr) of DataVal := (others => 0);
      Address : Addr;
      
      Result : Boolean := False;
      
   begin
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(Counter);
         
         case Inst.Op is
            when ADD =>
               -- register value out of range
               if Long_Integer(Regs(Inst.AddRs1)) + Long_Integer(Regs(Inst.AddRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.AddRs1)) + Long_Integer(Regs(Inst.AddRs2)) < Long_Integer(DataVal'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else 
                  Regs(Inst.AddRd) := Regs(Inst.AddRs1) + Regs(Inst.AddRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when SUB =>               
               -- register value out of range
               if Long_Integer(Regs(Inst.SubRs1)) - Long_Integer(Regs(Inst.SubRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.SubRs1)) - Long_Integer(Regs(Inst.SubRs2)) < Long_Integer(DataVal'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  Regs(Inst.SubRd) := Regs(Inst.SubRs1) - Regs(Inst.SubRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when MUL =>               
               -- register value out of range
               if Long_Integer(Regs(Inst.MulRs1)) * Long_Integer(Regs(Inst.MulRs2)) > Long_Integer(DataVal'Last) 
                 or Long_Integer(Regs(Inst.MulRs1)) * Long_Integer(Regs(Inst.MulRs2)) < Long_Integer(DataVal'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  Regs(Inst.MulRd) := Regs(Inst.MulRs1) * Regs(Inst.MulRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when DIV =>               
               -- the divisor is 0
               if Integer(Regs(Inst.DivRs2)) = 0 then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               elsif (Integer(Regs(Inst.DivRs2)) = -1 and Integer(Regs(Inst.DivRs1)) = Integer(DataVal'First)) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  Regs(Inst.DivRd) := Regs(Inst.DivRs1) / Regs(Inst.DivRs2);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when LDR =>               
               -- memory address out of range
               if Long_Integer(Regs(Inst.LdrRs)) + Long_Integer(DataVal(Inst.LdrOffs)) > Long_Integer(Addr'Last) 
                 or Long_Integer(Regs(Inst.LdrRs)) + Long_Integer(DataVal(Inst.LdrOffs)) < Long_Integer(Addr'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  -- register value out of range
                  if Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) > Integer(DataVal'Last) 
                    or Integer(Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) < Integer(DataVal'First) then
                     Result := True;
                     Ret := IllegalProgram;
                     exit;
                  else
                     Address := Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs));
                   
                     Regs(Inst.LdrRd) := Memory(Address);
                     IncDetectionPC(Ret,1,Counter);
                  end if;
               end if;
            when STR =>
               -- memory address out of range
               if Long_Integer(Regs(Inst.StrRa)) + Long_Integer(DataVal(Inst.StrOffs)) > Long_Integer(Addr'Last) 
                 or Long_Integer(Regs(Inst.StrRa)) + Long_Integer(DataVal(Inst.StrOffs)) < Long_Integer(Addr'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  Address := Addr(Regs(Inst.StrRa) + DataVal(Inst.StrOffs));
                  
                  Memory(Address) := Regs(Inst.StrRb);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when MOV =>
               -- register value out of range
               if Integer(Inst.MovOffs) > Integer(DataVal'Last) 
                 or Integer(Inst.MovOffs) < Integer(DataVal'First) then
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               else
                  Regs(Inst.MovRd) := DataVal(Inst.MovOffs);
                  IncDetectionPC(Ret,1,Counter);
               end if;
            when Instruction.RET =>
               Result := False;
               exit;
               when JMP =>
               if Integer(Inst.JmpOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                 or Integer(Counter) + Integer(Inst.JmpOffs) <= 0 then
                  -- jump out of the program
                  Result := True;
                  Ret := IllegalProgram;
                  exit;
               elsif Integer(Inst.JmpOffs) = 0 then
                  -- infinite loop
                  Result := False;
                  exit;
               else
                  IncDetectionPC(Ret,Inst.JmpOffs,Counter);
               end if;
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  if Integer(Inst.JzOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                    or Integer(Counter) + Integer(Inst.JzOffs) <= 0 then
                     -- jump out of the program
                     Result := True;
                     Ret := IllegalProgram;
                     exit;
                  elsif Integer(Inst.JzOffs) = 0 then
                     -- infinite loop
                     Result := False;
                     exit;
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
      
      if Cycles > Integer'First then
         if Ret = Success and CycleCount = Cycles - 1 then
            -- Cycles instructions executed without a RET or invalid behaviour
            Result := True;
         end if;
      end if;
      
      return Result;
   end DynamicAnalyze;

   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      StaticAnalyRes : Boolean;
      DynamicAnalyRes : Boolean;
      
      FinalRes : Boolean;
   begin
      StaticAnalyRes := StaticAnalyze(Prog, Cycles);
      
      if StaticAnalyRes then
         FinalRes := True;
      else
         DynamicAnalyRes := DynamicAnalyze(Prog, Cycles);
         FinalRes := DynamicAnalyRes;
      end if;
         
      return FinalRes;
   end DetectInvalidBehaviour;
   
end Machine;
