with Ada.Strings.Fixed;                  use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Text_IO;                        use Ada.Text_IO;
with GNAT.RegPat;                        use GNAT.RegPat;
with Langkit_Support.Slocs;              use Langkit_Support.Slocs;
with Langkit_Support.Text;               use Langkit_Support.Text;
with Libadalang.Analysis;                use Libadalang.Analysis;

with Rejuvenation;                       use Rejuvenation;
with Rejuvenation.Factory;               use Rejuvenation.Factory;
with Rejuvenation.File_Utils;            use Rejuvenation.File_Utils;
with Rejuvenation.Finder;                use Rejuvenation.Finder;
with Rejuvenation.Indentation;           use Rejuvenation.Indentation;
with Rejuvenation.Match_Patterns;        use Rejuvenation.Match_Patterns;
with Rejuvenation.Navigation;            use Rejuvenation.Navigation;
with Rejuvenation.Patterns;              use Rejuvenation.Patterns;
with Rejuvenation.String_Utils;          use Rejuvenation.String_Utils;
with Rejuvenation.Text_Rewrites;         use Rejuvenation.Text_Rewrites;
with Rejuvenation.Utils;                 use Rejuvenation.Utils;

package body Rejuvenation_Replace_Utils is

   F : File_Type := Standard_Output;

   function Rewrite_Match (Match : Match_Pattern;
                           Replacement : String;
                           Search_Pattern : String) return String
   is
      Match_Node : constant Ada_Node := Match.Get_Nodes.First_Element;

      type Resolve_Placeholder_Func is access function (Placeholder : String) return String;

      function Resolve_Sloc_Placeholder (Placeholder : String) return String is
        (Image (Start_Sloc (Sloc_Range (Match_Node))));

      function Resolve_File_Sloc_Placeholder (Placeholder : String) return String is
        (Image (Match_Node.Full_Sloc_Image));

      function Resolve_Full_Sloc_Placeholder (Placeholder : String) return String is
        (Match_Node.Unit.Get_Filename & ":" & Image (Start_Sloc (Sloc_Range (Match_Node))));

      function Resolve_Filename_Placeholder (Placeholder : String) return String is
        (Match_Node.Unit.Get_Filename);

      function Resolve_Original_Placeholder (Placeholder : String) return String is
        (Search_Pattern);

      function Resolve_Original_Raw_Placeholder (Placeholder : String) return String is
        (Raw_Signature (Match_Node));

      function Resolve_Other_Placeholder (Placeholder : String) return String is
        ("##PLACEHOLDER_NOT_SUPPORTED##");

      function Resolve_Rejuvenation_Placeholder (Placeholder : String) return String is
        (Match.Get_Placeholder_As_Raw_Signature (Placeholder));

      function Resolve_Escaped_Placeholder (Placeholder : String) return String is
         (Placeholder (Placeholder'Last .. Placeholder'Last));

      function Replace (Src_String : String;
                        Match : GNAT.RegPat.Match_Location;
                        Replacement_String : String) return String
      is
         Prefix : String := Src_String (Src_String'First .. Match.First - 1);
         Postfix : String := Src_String (Match.Last + 1 .. Src_String'Last);
      begin
         return Prefix & Replacement_String & Postfix;
      end Replace;

      Current_Result : Unbounded_String := To_Unbounded_String (Replacement);

      procedure Replace_Placeholders (Placeholder_Pattern : String;
                                      Resolve_Placeholder : Resolve_Placeholder_Func;
                                      RegExp_Flags        : GNAT.RegPat.Regexp_Flags := No_Flags)
      is
         Matches : GNAT.RegPat.Match_Array (0 .. 0);
         Matcher : GNAT.RegPat.Pattern_Matcher := Compile (Placeholder_Pattern, RegExp_Flags);
         Current : Natural := To_String (Current_Result)'First;
      begin
         loop
            declare
               Current_String : String := To_String (Current_Result);
            begin
               GNAT.RegPat.Match (Matcher, Current_String, Matches, Current);
               exit when Matches (0) = No_Match;

               declare
                  Placeholder : String := Current_String (Matches (0).First .. Matches (0).Last);
                  Replacement : String := Resolve_Placeholder (Placeholder);
                  Replace_Result : String := Replace (Current_String, Matches (0), Replacement);
               begin
                  Current := Matches (0).Last + 1 + Replace_Result'Length - Current_String'Length;
                  Current_Result := To_Unbounded_String (Replace_Result);
               end;
            end;
         end loop;
      end Replace_Placeholders;
   begin
      Replace_Placeholders ("##filename##"        , Resolve_Filename_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##sloc##"            , Resolve_Sloc_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##file_sloc##"       , Resolve_File_Sloc_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##full_sloc##"       , Resolve_Full_Sloc_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##original##"        , Resolve_Original_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##original_raw##"    , Resolve_Original_Raw_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("##[a-zA-Z0-9_]+##"   , Resolve_Other_Placeholder'Access, Multiple_Lines or Case_Insensitive);
      Replace_Placeholders ("\$[SM]_[a-zA-Z0-9_]+", Resolve_Rejuvenation_Placeholder'Access, Multiple_Lines);
      Replace_Placeholders ("\\."                 , Resolve_Escaped_Placeholder'Access, Multiple_Lines or Case_Insensitive);

      return To_String (Current_Result);
   end Rewrite_Match;

   procedure Rewrite_Match (TR : in out Text_Rewrite_Unit;
                            Match_Node : Ada_Node;
                            Replacement : String) is
   begin
      TR.Replace (Match_Node, Replacement);
   end Rewrite_Match;

   function Replace_All (Input : String; Search : String; Replacement : String) return String
   is
      Result : Unbounded_String;
      Previous_Index : Integer := Input'First;
      Current_Index : Integer := Input'First;
   begin
      Current_Index := Index (Input, Search);

      while Current_Index /= 0 loop
         if Current_Index > Previous_Index then
            Append (Result, Input (Previous_Index .. Current_Index - 1));
         end if;
         Append (Result, Replacement);

         Previous_Index := Current_Index + Search'Length;
         Current_Index := Index (Input, Search, From => Previous_Index);
      end loop;

      if Previous_Index < Input'Last then
         Append (Result, Input (Previous_Index .. Input'Last));
      end if;

      return To_String (Result);
   end Replace_All;

   function Replace_Line_Endings (Input : String) return String is
   begin
      return Replace_All
        (Replace_All (Input,
         "\x0d", "" & ASCII.CR),
         "\x0a", "" & ASCII.LF);
   end Replace_Line_Endings;

   procedure Replace_All (File_Unit           : Analysis_Unit;
                          Search_Pattern      : Pattern;
                          Rule                : Grammar_Rule := Default_Grammar_Rule;
                          Replacement_Pattern : String;
                          Show_Results_Only   : Boolean := False;
                          Include_Indentation : Boolean := False;
                          Rewrite_Files       : Boolean := False)
   is
      TR : Text_Rewrite_Unit := Make_Text_Rewrite_Unit (File_Unit);

      function Get_Indentation (Match_Node : Ada_Node) return String is
      begin
         if Include_Indentation then
            declare
               Indentation     : constant Integer := Indentation_Of_Node (Match_Node);
               Indentation_Str : constant String  := Integer'Max (0, Indentation) * " ";
            begin
               return Indentation_Str;
            end;
         else
            if Show_Results_Only then
               return "";
            else
               return "  ";
            end if;
         end if;
      end Get_Indentation;

      function Get_Output (Match_Node : Ada_Node; Replacement : String) return String
      is
         Indentation_Str : constant String := Get_Indentation (Match_Node);
      begin
         if Replacement_Pattern'Length = 0 then
            return Indentation_Str & Replace_Line_Endings (Image (Match_Node.Text));
         else
            return Indentation_Str & Replace_Line_Endings (Replacement);
         end if;
      end Get_Output;
   begin
      for Match of Find_Full (File_Unit.Root, Search_Pattern) loop
         declare
            Match_Node  : constant Ada_Node := Match.Get_Nodes.First_Element;
            Replacement : constant String   := Rewrite_Match (Match, Replacement_Pattern, Search_Pattern.Get_String);
         begin
            if not Show_Results_Only then
               Put_Line (F, "Match found at: " & Image (Match_Node.Full_Sloc_Image));
            end if;

            Put_Line (F, Get_Output (Match_Node, Replacement));

            if not Show_Results_Only then
               Put_Line (F, "");
            end if;

            if Rewrite_Files then
               Rewrite_Match (TR, Match_Node, Replacement);
            end if;
         end;
      end loop;

      if Rewrite_Files and then TR.HasReplacements then
         Write_String_To_File (TR.ApplyToString, File_Unit.Get_Filename);
      end if;
   end Replace_All;

   procedure Replace_All (File_Path           : String;
                          Search_Pattern      : String;
                          Rule                : Grammar_Rule := Default_Grammar_Rule;
                          Replacement_Pattern : String := "";
                          Show_Results_Only   : Boolean := False;
                          Include_Indentation : Boolean := False;
                          Rewrite_Files       : Boolean := False;
                          Results_File_Path   : String := "")
   is
      File_Unit : Analysis_Unit := Open_File (File_Path);

      Local_Search_Pattern : constant Pattern := Make_Pattern (Search_Pattern, Rule);
   begin
      if Results_File_Path'Length > 0 then
         if Is_Open (F) then
            Close (F);
         end if;

         Create (F, Out_File, Results_File_Path);
      end if;

      Replace_All (File_Unit,
                   Local_Search_Pattern,
                   Rule,
                   Replacement_Pattern,
                   Show_Results_Only,
                   Include_Indentation,
                   Rewrite_Files);

      if Is_Open (F) then
         Close (F);
      end if;
   end Replace_All;

   procedure Replace_All_In_Project (Project_File_Path   : String;
                                     Search_Pattern      : String;
                                     Rule                : Grammar_Rule := Default_Grammar_Rule;
                                     Replacement_Pattern : String := "";
                                     Show_Results_Only   : Boolean := False;
                                     Include_Indentation : Boolean := False;
                                     Rewrite_Files       : Boolean := False;
                                     Results_File_Path   : String := "")
   is
      Units : Analysis_Unit_Vectors.Vector := Open_Files_From_Project (Project_File_Path);

      Local_Search_Pattern : constant Pattern := Make_Pattern (Search_Pattern, Rule);
   begin
      if Results_File_Path'Length > 0 then
         if Is_Open (F) then
            Close (F);
         end if;

         Create (F, Out_File, Results_File_Path);
      end if;

      for File_Unit of Units loop
         Replace_All (File_Unit,
                      Local_Search_Pattern,
                      Rule,
                      Replacement_Pattern,
                      Show_Results_Only,
                      Include_Indentation,
                      Rewrite_Files);
      end loop;

      if Is_Open (F) then
         Close (F);
      end if;
   end Replace_All_In_Project;

end Rejuvenation_Replace_Utils;
