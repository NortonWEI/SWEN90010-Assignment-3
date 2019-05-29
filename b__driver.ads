pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2018 (20180523-73)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_driver" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#1e5fbf14#;
   pragma Export (C, u00001, "driverB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0f7d71d4#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#10567966#;
   pragma Export (C, u00004, "debugB");
   u00005 : constant Version_32 := 16#cd66d8b0#;
   pragma Export (C, u00005, "debugS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00007, "ada__integer_text_ioB");
   u00008 : constant Version_32 := 16#082ea75f#;
   pragma Export (C, u00008, "ada__integer_text_ioS");
   u00009 : constant Version_32 := 16#f8088b52#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#16307b94#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#4d58644d#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#bd45c2cc#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#4dcf97e2#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00018, "system__parametersB");
   u00019 : constant Version_32 := 16#40b73bd0#;
   pragma Export (C, u00019, "system__parametersS");
   u00020 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00022, "system__soft_links__initializeB");
   u00023 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00023, "system__soft_links__initializeS");
   u00024 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00024, "system__stack_checkingB");
   u00025 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00025, "system__stack_checkingS");
   u00026 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00026, "system__exception_tableB");
   u00027 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00027, "system__exception_tableS");
   u00028 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00030, "system__exceptions__machineB");
   u00031 : constant Version_32 := 16#3bad9081#;
   pragma Export (C, u00031, "system__exceptions__machineS");
   u00032 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00032, "system__exceptions_debugB");
   u00033 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00033, "system__exceptions_debugS");
   u00034 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00034, "system__img_intB");
   u00035 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00035, "system__img_intS");
   u00036 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00036, "system__tracebackB");
   u00037 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00037, "system__tracebackS");
   u00038 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00038, "system__traceback_entriesB");
   u00039 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00039, "system__traceback_entriesS");
   u00040 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00040, "system__traceback__symbolicB");
   u00041 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00041, "system__traceback__symbolicS");
   u00042 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00042, "ada__exceptions__tracebackB");
   u00043 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00043, "ada__exceptions__tracebackS");
   u00044 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00044, "system__address_imageB");
   u00045 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00045, "system__address_imageS");
   u00046 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00046, "system__wch_conB");
   u00047 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00047, "system__wch_conS");
   u00048 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00048, "system__wch_stwB");
   u00049 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00049, "system__wch_stwS");
   u00050 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00050, "system__wch_cnvB");
   u00051 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00051, "system__wch_cnvS");
   u00052 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00052, "interfacesS");
   u00053 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00053, "system__wch_jisB");
   u00054 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00054, "system__wch_jisS");
   u00055 : constant Version_32 := 16#927a893f#;
   pragma Export (C, u00055, "ada__text_ioB");
   u00056 : constant Version_32 := 16#1ffab6e1#;
   pragma Export (C, u00056, "ada__text_ioS");
   u00057 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00057, "ada__streamsB");
   u00058 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00058, "ada__streamsS");
   u00059 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00059, "ada__io_exceptionsS");
   u00060 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00060, "ada__tagsB");
   u00061 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00061, "ada__tagsS");
   u00062 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00062, "system__htableB");
   u00063 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00063, "system__htableS");
   u00064 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00064, "system__string_hashB");
   u00065 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00065, "system__string_hashS");
   u00066 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00066, "system__unsigned_typesS");
   u00067 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00067, "system__val_lluB");
   u00068 : constant Version_32 := 16#462f440a#;
   pragma Export (C, u00068, "system__val_lluS");
   u00069 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00069, "system__val_utilB");
   u00070 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00070, "system__val_utilS");
   u00071 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00071, "system__case_utilB");
   u00072 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00072, "system__case_utilS");
   u00073 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00073, "interfaces__c_streamsB");
   u00074 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00074, "interfaces__c_streamsS");
   u00075 : constant Version_32 := 16#4e0ce0a1#;
   pragma Export (C, u00075, "system__crtlS");
   u00076 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00076, "system__file_ioB");
   u00077 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00077, "system__file_ioS");
   u00078 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00078, "ada__finalizationS");
   u00079 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00079, "system__finalization_rootB");
   u00080 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00080, "system__finalization_rootS");
   u00081 : constant Version_32 := 16#0f8892f9#;
   pragma Export (C, u00081, "system__os_libB");
   u00082 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00082, "system__os_libS");
   u00083 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00083, "system__stringsB");
   u00084 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00084, "system__stringsS");
   u00085 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00085, "system__file_control_blockS");
   u00086 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00086, "ada__text_io__integer_auxB");
   u00087 : constant Version_32 := 16#09097bbe#;
   pragma Export (C, u00087, "ada__text_io__integer_auxS");
   u00088 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00088, "ada__text_io__generic_auxB");
   u00089 : constant Version_32 := 16#16b3615d#;
   pragma Export (C, u00089, "ada__text_io__generic_auxS");
   u00090 : constant Version_32 := 16#b10ba0c7#;
   pragma Export (C, u00090, "system__img_biuB");
   u00091 : constant Version_32 := 16#faff9b35#;
   pragma Export (C, u00091, "system__img_biuS");
   u00092 : constant Version_32 := 16#4e06ab0c#;
   pragma Export (C, u00092, "system__img_llbB");
   u00093 : constant Version_32 := 16#bb388bcb#;
   pragma Export (C, u00093, "system__img_llbS");
   u00094 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00094, "system__img_lliB");
   u00095 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00095, "system__img_lliS");
   u00096 : constant Version_32 := 16#a756d097#;
   pragma Export (C, u00096, "system__img_llwB");
   u00097 : constant Version_32 := 16#1254a85d#;
   pragma Export (C, u00097, "system__img_llwS");
   u00098 : constant Version_32 := 16#eb55dfbb#;
   pragma Export (C, u00098, "system__img_wiuB");
   u00099 : constant Version_32 := 16#94be1ca7#;
   pragma Export (C, u00099, "system__img_wiuS");
   u00100 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00100, "system__val_intB");
   u00101 : constant Version_32 := 16#40fe45c4#;
   pragma Export (C, u00101, "system__val_intS");
   u00102 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00102, "system__val_unsB");
   u00103 : constant Version_32 := 16#2c75fe43#;
   pragma Export (C, u00103, "system__val_unsS");
   u00104 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00104, "system__val_lliB");
   u00105 : constant Version_32 := 16#927f895b#;
   pragma Export (C, u00105, "system__val_lliS");
   u00106 : constant Version_32 := 16#faa1b790#;
   pragma Export (C, u00106, "instructionB");
   u00107 : constant Version_32 := 16#277a987f#;
   pragma Export (C, u00107, "instructionS");
   u00108 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00108, "ada__charactersS");
   u00109 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00109, "ada__characters__latin_1S");
   u00110 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00110, "ada__numericsS");
   u00111 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00111, "system__img_enum_newB");
   u00112 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00112, "system__img_enum_newS");
   u00113 : constant Version_32 := 16#ec9cfed1#;
   pragma Export (C, u00113, "system__random_numbersB");
   u00114 : constant Version_32 := 16#cb43df61#;
   pragma Export (C, u00114, "system__random_numbersS");
   u00115 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00115, "system__img_unsB");
   u00116 : constant Version_32 := 16#a3292f8f#;
   pragma Export (C, u00116, "system__img_unsS");
   u00117 : constant Version_32 := 16#650caaea#;
   pragma Export (C, u00117, "system__random_seedB");
   u00118 : constant Version_32 := 16#534b46a0#;
   pragma Export (C, u00118, "system__random_seedS");
   u00119 : constant Version_32 := 16#b8719323#;
   pragma Export (C, u00119, "ada__calendarB");
   u00120 : constant Version_32 := 16#41508869#;
   pragma Export (C, u00120, "ada__calendarS");
   u00121 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00121, "interfaces__cB");
   u00122 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00122, "interfaces__cS");
   u00123 : constant Version_32 := 16#2b2125d3#;
   pragma Export (C, u00123, "system__os_primitivesB");
   u00124 : constant Version_32 := 16#0fa60a0d#;
   pragma Export (C, u00124, "system__os_primitivesS");
   u00125 : constant Version_32 := 16#a05912b4#;
   pragma Export (C, u00125, "machineB");
   u00126 : constant Version_32 := 16#8ffc8149#;
   pragma Export (C, u00126, "machineS");
   u00127 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00127, "system__img_boolB");
   u00128 : constant Version_32 := 16#fd821e10#;
   pragma Export (C, u00128, "system__img_boolS");
   u00129 : constant Version_32 := 16#2323a8af#;
   pragma Export (C, u00129, "system__memoryB");
   u00130 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00130, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.wch_stw%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  ada.exceptions%s
   --  system.wch_stw%b
   --  ada.exceptions.traceback%s
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  ada.exceptions.last_chance_handler%s
   --  system.memory%s
   --  system.memory%b
   --  ada.exceptions.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  ada.exceptions%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_lli%s
   --  system.val_lli%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  debug%s
   --  debug%b
   --  instruction%s
   --  instruction%b
   --  machine%s
   --  machine%b
   --  driver%b
   --  END ELABORATION ORDER

end ada_main;
