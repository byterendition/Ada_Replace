with Libadalang.Common;      use Libadalang.Common;

package Rejuvenation_Replace_Utils is

   procedure Replace_All (File_Path           : String;
                          Search_Pattern      : String;
                          Rule                : Grammar_Rule := Default_Grammar_Rule;
                          Replacement_Pattern : String := "";
                          Show_Results_Only   : Boolean := False;
                          Include_Indentation : Boolean := False;
                          Rewrite_Files       : Boolean := False;
                          Results_File_Path   : String := "");

   procedure Replace_All_In_Project (Project_File_Path   : String;
                                     Search_Pattern      : String;
                                     Rule                : Grammar_Rule := Default_Grammar_Rule;
                                     Replacement_Pattern : String := "";
                                     Show_Results_Only   : Boolean := False;
                                     Include_Indentation : Boolean := False;
                                     Rewrite_Files       : Boolean := False;
                                     Results_File_Path   : String := "");

   --  Example use:

   --  Find_Or_Replace_All (File_Path           => "path\filename.adb",
   --                       Search_Pattern      => "$S_Namespace.$S_Call ($S_Instance, $M_Arg);",
   --                       Rule                => Stmt_Rule,
   --                       Replacement_Pattern => "$S_Instance.$S_Call ($M_Arg);");

   --  To just reformat a piece of code use the same argument for Pattern and Replacement
   --  (pattern searches whitespace independent, but replacement can contain any preferred formatting)

   --  Find_Or_Replace_All (File_Path           => "path\filename.adb",
   --                       Search_Pattern      => "$S_Instance.$S_Call ($M_Arg);",
   --                       Rule                => Stmt_Rule,
   --                       Replacement_Pattern => "$S_Instance.$S_Call ($M_Arg);");

end Rejuvenation_Replace_Utils;
