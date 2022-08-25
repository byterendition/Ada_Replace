with Ada.Strings.Fixed;                           use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;                       use Ada.Strings.Unbounded;
with Ada.Text_IO;                                 use Ada.Text_IO;
with GNAT.Command_Line;                           use GNAT.Command_Line;
with GNAT.Strings;                                use GNAT.Strings;
with Libadalang.Common;                           use Libadalang.Common;
with Rejuvenation_Replace_Utils;                  use Rejuvenation_Replace_Utils;

procedure Ada_Replace is

   CrLf : String := ASCII.CR & ASCII.LF;

   Config : Command_Line_Configuration;

   File_Path           : aliased GNAT.Strings.String_Access;
   Search_Pattern      : aliased GNAT.Strings.String_Access;
   Rule_Str            : aliased GNAT.Strings.String_Access;
   Replacement_Pattern : aliased GNAT.Strings.String_Access;
   Show_Results_Only   : aliased Boolean := False;
   Include_Indentation : aliased Boolean := False;
   Rewrite_Files       : aliased Boolean := False;
   Results_File_Path   : aliased GNAT.Strings.String_Access;

   type File_Type is (Source, Project);

   File : File_Type;
   Rule : Grammar_Rule := Stmt_Rule;

begin

   Set_Usage (Config => Config,
              Help   => "Find and replace code patterns in an Ada file or project."
              & CrLf & ""
              & CrLf & "Patterns can include placeholders for single items ($S_Name) or for sequences of items ($M_Sequence)."
              & CrLf & ""
              & CrLf & "For replacement patterns, additional placeholders can be used to add information to the search result:"
              & CrLf & "  ##filename##     : The full path to the file where the match was found."
              & CrLf & "  ##sloc##         : The line and column where the match was found."
              & CrLf & "  ##file_sloc##    : The filename and sloc where the match was found."
              & CrLf & "  ##full_sloc##    : The full path and sloc where the match was found."
              & CrLf & "  ##original##     : The original search pattern of the match, which then gets resolved to text."
              & CrLf & "                     This will essentially pretty-print it in the layout of the search pattern."
              & CrLf & "  ##original_raw## : The original text of the match, including any whitespace."
              & CrLf & ""
              & CrLf & "Examples:"
              & CrLf & "  To find all declarations of Probes in adat.gpr:"
              & CrLf & "    ada_replace -f=Source\adat.gpr -s=""$S_Name : constant Probe := Create_Probe ($M_Args);"" -g=Basic_Decl_Rule"
              & CrLf & ""
              & CrLf & "  To find all calls to Log_Text in adat.gpr and display the locations and messages:"
              & CrLf & "    ada_replace -f=Source\adat.gpr -s=""Log_Text ($S_Message);"" -g=Stmt_Rule -r=""Found Log_Text at ##full_sloc## with message: $S_Message"" -x"
              & CrLf & ""
              & CrLf & "  To replace all calls to Log_Text in adat.gpr with Logger.Info (add -w switch to rewrite the files):"
              & CrLf & "    ada_replace -f=Source\adat.gpr -s=""Log_Text ($S_Message);"" -g=Stmt_Rule -r=""Logger.Info ($S_Message);"" -x"
              & CrLf & "");

   Define_Switch (Config      => Config,
                  Output      => File_Path'Access,
                  Switch      => "-f=",
                  Long_Switch => "--filename=",
                  Help        => "File or project to search. *.ads/adb for searching a single source file or *.gpr to search an entire project.",
                  Argument    => "*.(ads|adb|gpr)");

   Define_Switch (Config      => Config,
                  Output      => Search_Pattern'Access,
                  Switch      => "-s=",
                  Long_Switch => "--search_pattern=",
                  Help        => "Search pattern. Surround with double quotes if the pattern contains spaces.",
                  Argument    => "PATTERN");

   Define_Switch (Config      => Config,
                  Output      => Rule_Str'Access,
                  Switch      => "-g=",
                  Long_Switch => "--grammar_rule=",
                  Help        => "Grammar rule to be used for finding the search pattern. Common rules: Stmt_Rule (default), Expr_Rule, Name_Rule.",
                  Argument    => "RULE");

   Define_Switch (Config      => Config,
                  Output      => Replacement_Pattern'Access,
                  Switch      => "-r=",
                  Long_Switch => "--replacement_pattern=",
                  Help        => "Pattern used for presenting or replacing the search results. Surround with double quotes if the pattern contains spaces.",
                  Argument    => "PATTERN");

   Define_Switch (Config      => Config,
                  Output      => Show_Results_Only'Access,
                  Switch      => "-x",
                  Long_Switch => "--show_results_only",
                  Help        => "Show results only, without any information about the source location and without white lines between search results.",
                  Value       => True);

   Define_Switch (Config      => Config,
                  Output      => Include_Indentation'Access,
                  Switch      => "-i",
                  Long_Switch => "--include_indentation",
                  Help        => "Include the indentation of the search result in the output.",
                  Value       => True);

   Define_Switch (Config      => Config,
                  Output      => Rewrite_Files'Access,
                  Switch      => "-w",
                  Long_Switch => "--rewrite_files",
                  Help        => "Rewrite the files. WARNING: cannot be undone!",
                  Value       => True);

   Define_Switch (Config      => Config,
                  Output      => Results_File_Path'Access,
                  Switch      => "-o=",
                  Long_Switch => "--write_results=",
                  Help        => "Write the results of the search to a file.",
                  Argument    => "FILE");

   begin
      Getopt (Config);
   exception when E : Exit_From_Command_Line => return;
   end;

   if File_Path.all'Length = 0 then
      Put_Line ("Must provide source or project file! Check ada_replace --help for correct usage.");
      return;
   end if;

   declare
      function Ends_With (Source : String; Pattern : String) return Boolean is
      begin
         if Source'Length < Pattern'Length then
            return False;
         else
            return Tail (Source, Count => Pattern'Length) = Pattern;
         end if;
      end Ends_With;
   begin
      if Ends_With (File_Path.all, ".ads") or else Ends_With (File_Path.all, ".adb") then
         File := Source;
      elsif Ends_With (File_Path.all, ".gpr") then
         File := Project;
      else
         Put_Line ("File must have extension .ads, .adb or .gpr!");
         return;
      end if;
   end;

   if Search_Pattern.all'Length = 0 then
      Put_Line ("Must provide search pattern! Check ada_replace --help for correct usage.");
      return;
   end if;

   if Rule_Str.all'Length > 0 then
      begin
         Rule := Grammar_Rule'Value (Rule_Str.all);
      exception when E : others =>
            Put_Line ("Unknown grammar rule provided!");
            Put_Line ("  Options are:");

            declare
               Local_Rule : Grammar_Rule := Grammar_Rule'First;
               Options    : Unbounded_String := To_Unbounded_String (Local_Rule'Img);
            begin
               while Local_Rule /= Grammar_Rule'Last loop
                  Local_Rule := Grammar_Rule'Succ (Local_Rule);
                  Append (Options, ", " & Local_Rule'Img);
               end loop;

               Put_Line ("[" & To_String (Options) & "]");
            end;
            return;
      end;
   end if;

   case File is
      when Source =>
         Replace_All (File_Path.all,
                      Search_Pattern.all,
                      Rule,
                      Replacement_Pattern.all,
                      Show_Results_Only,
                      Include_Indentation,
                      Rewrite_Files,
                      Results_File_Path.all);
      when Project =>
         Replace_All_In_Project (File_Path.all,
                                 Search_Pattern.all,
                                 Rule,
                                 Replacement_Pattern.all,
                                 Show_Results_Only,
                                 Include_Indentation,
                                 Rewrite_Files,
                                 Results_File_Path.all);
   end case;

end Ada_Replace;
