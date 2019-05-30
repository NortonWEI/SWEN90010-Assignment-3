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
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
              if Integer(Regs(Rs1) + Regs(Rs2)) <= Integer(DataVal'Last) and
                DataVal(Regs(Rs1)) + DataVal(Regs(Rs2)) >= DataVal'First
      --                 if (Regs(Rs1) >= -2**30 and Regs(Rs1) < 2**30 and Regs(Rs2) >= -2**30 and Regs(Rs2) < 2**30) and 
      --          (Regs(Rs1) < -2**30 and Regs(Rs1) >= -2**31 and Regs(Rs2) + Regs(Rs1) > -2**30+1 and Regs(Rs2) + Regs(Rs1) <= 0) 
      --          (Regs(Rs1) <= 2**31-1 and Regs(Rs1) >= 2**30 and Regs(Rs2) > -2**31 - Regs(Rs1) and Regs(Rs2) < 2**31-2)
--        if (Regs(Rs1) >= 0 and Regs(Rs1) <= 2**31-1 and ((Regs(Rs2) >= -2**31 and Regs(Rs2) <= 0))) or 
--          (Regs(Rs1) < 0 and Regs(Rs1) >= -2**31 and ((Regs(Rs2) <= 2**31-1 and Regs(Rs2) >= 0) ))
      then
         --           if Integer(Regs(Rs1)) >= Integer(DataVal'First) and Integer(Regs(Rs1)) <= Integer(DataVal'First)/2 then
         --              if Integer(Regs(Rs2)) <= Integer(DataVal'First)/2 then
         Regs(Rd) := Regs(Rs1) + Regs(Rs2);
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
      
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if Integer(Regs(Rs1) - Regs(Rs2)) <= Integer(DataVal'Last) and
        Integer(Regs(Rs1) - Regs(Rs2)) >= Integer(DataVal'First) then
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) * Regs(Rs2);
      Ret := Success;
      
      Put(Ret'Image);
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if Regs(Rs2) /= 0 and (Regs(Rs2) = -1 and Regs(Rs1) > DataVal'First) then
         Regs(Rd) := Regs(Rs1) / Regs(Rs2);
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
      
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr;
   begin 
      if DataVal(Offs) <= 65535 and DataVal(Offs) >= 0 then
         if Regs(Rs) + DataVal(Offs) >= 0 and Regs(Rs) + DataVal(Offs) <= 65535 then
            A := Addr(Regs(Rs) + DataVal(Offs));
            Regs(Rd) := Memory(A);
            Ret := Success;
         else
            Ret := IllegalProgram;
         end if;
      end if;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Ra) + DataVal(Offs));   
   begin
      Memory(A) := Regs(Rb);
      Ret := Success;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := DataVal(Offs);
      Ret := Success;
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
               DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2,Ret);
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs,Ret);
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
   
   function StaticAnalyze(Prog : in Program;
                          Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      RetExist : Boolean := False;
      
   begin
      while (CycleCount < Cycles) loop
         -- just check the program line by line
         Inst := Prog(ProgramCounter(CycleCount + 1));

         case Inst.Op is
            when JMP =>
               if Integer(Inst.JmpOffs) + CycleCount + 1 >= MAX_PROGRAM_LENGTH 
                 or CycleCount + Integer(Inst.JmpOffs) < 0 then
                  -- jump out of the program
                  return True;
               elsif Integer(Inst.JmpOffs) = 0 then
                  -- infinite loop
                  return False;
               end if;
            when JZ =>
               if Integer(Inst.JzOffs) + CycleCount + 1 >= MAX_PROGRAM_LENGTH 
                 or CycleCount + Integer(Inst.JzOffs) < 0 then
                  -- jump out of the program
                  return True;
               elsif Integer(Inst.JzOffs) = 0 then
                  -- infinite loop
                  return False;
               end if;
            when Instruction.RET =>
               RetExist := True;
            when others =>
               null;
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      
      if not RetExist then
         -- no RET in the program
         return True;
      end if;
      
      return False;
   end StaticAnalyze;
   
   function DynamicAnalyze(Prog : in Program;
                           Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      Ret : ReturnCode := Success;
      
      Regs : array (Reg) of DataVal := (others => 0);
      Counter : ProgramCounter := ProgramCounter'First;
      Mem : array (Addr) of DataVal := (others => 0);
      
      
   begin
      
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(Counter);
         
         case Inst.Op is
            when ADD =>
--                 DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2,Ret);
--                 IncPC(Ret,1);
               
               -- register value out of range
               if Integer(Regs(Inst.AddRs1) + Regs(Inst.AddRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.AddRs1) + Regs(Inst.AddRs2)) < Integer(DataVal'First) then
                  return True;
               else
                  Regs(Rd) := Regs(Rs1) + Regs(Rs2);
               end if;
            when SUB =>
--                 DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
--                 IncPC(Ret,1);
               
               -- register value out of range
               if Integer(Regs(Inst.AddRs1) - Regs(Inst.AddRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.AddRs1) - Regs(Inst.AddRs2)) < Integer(DataVal'First) then
                  return True;
               end if;
            when MUL =>
--                 DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
--                 IncPC(Ret,1);
               
               -- register value out of range
               if Integer(Regs(Inst.AddRs1) * Regs(Inst.AddRs2)) > Integer(DataVal'Last) 
                 or Integer(Regs(Inst.AddRs1) * Regs(Inst.AddRs2)) < Integer(DataVal'First) then
                  return True;
               end if;
            when DIV =>
--                 DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
--                 IncPC(Ret,1);
               
               -- the divisor is 0
               if Integer(Regs(Inst.DivRs2)) = 0 then
                  return True;
               end if;
            when LDR =>
--                 DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
--                 IncPC(Ret,1);
               
               -- memory address out of range
               if Integer(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)) > Integer(Addr'Last) 
                 or Integer(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)) < Integer(Addr'First) then
                  return True;
               else
                  -- register value out of range
                  if Integer(Mem(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) > Integer(DataVal'Last) 
                    or Integer(Mem(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)))) < Integer(DataVal'First) then
                     return True;
                  end if;
               end if;
            when STR =>
--                 DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
--                 IncPC(Ret,1);
               
               -- memory address out of range
               if Integer(Regs(Inst.StrRa) + DataVal(Inst.StrOffs)) > Integer(Addr'Last) 
                 or Integer(Regs(Inst.StrRa) + DataVal(Inst.StrOffs)) < Integer(Addr'First) then
                  return True;
               end if;
            when MOV =>
--                 DoMov(Inst.MovRd,Inst.MovOffs,Ret);
--                 IncPC(Ret,1);
               
               -- register value out of range
               if Integer(Inst.MovOffs) > Integer(DataVal'Last) 
                 or Integer(Inst.MovOffs) < Integer(DataVal'First) then
                  return True;
               end if;
            when Instruction.RET =>
               Ret := Success;
            when JMP =>
--                 IncPC(Ret,Inst.JmpOffs);
               
               if Integer(Inst.JmpOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                 or Integer(Counter) + Integer(Inst.JmpOffs) <= 0 then
                  -- jump out of the program
                  return True;
               elsif Integer(Inst.JmpOffs) = 0 then
                  -- infinite loop
                  return False;
               end if;
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
--                    IncPC(Ret,Inst.JzOffs);
                  
                  if Integer(Inst.JzOffs) + Integer(Counter) >= MAX_PROGRAM_LENGTH 
                    or Integer(Counter) + Integer(Inst.JzOffs) <= 0 then
                     -- jump out of the program
                     return True;
                  elsif Integer(Inst.JzOffs) = 0 then
                     -- infinite loop
                     return False;
                  end if;
               else
                  --                    IncPc(Ret,1);
                  null;
               end if;
            when NOP =>
               --                 IncPC(Ret,1);
               null;
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
      
      return False;
   end DynamicAnalyze;

   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      StaticAnalysisRes : Boolean := True;
      DynamicAnalysisRes : Boolean := True;
      
   begin
      StaticAnalysisRes := StaticAnalyze(Prog, Cycles);
      DynamicAnalysisRes := DynamicAnalyze(Prog, Cycles);
      return StaticAnalysisRes and DynamicAnalysisRes;
   end DetectInvalidBehaviour;
   
end Machine;
