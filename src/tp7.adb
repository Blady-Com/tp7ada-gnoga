-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 10.2a
-- DATE DE LA DERNIERE MISE A JOUR  : 15 septembre 2016
-- ROLE DU CSU                      : Unité d'émulation Turbo Pascal 7.0.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            : Ada 2005, GNOGA 1.2a
--
-- COPYRIGHT                        : (c) Pascal Pignard 2002-2016
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Conversions;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Plugin.Ace_Editor;
with Gnoga.Gui.Navigator;
with Gnoga.Types.Key_Codes;
with ZanyBlue.Text.Locales;

package body TP7 is

   function To_TPString (Source : String) return TPString is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
   begin
      if Index = 0 then
         return Source & Ada.Characters.Latin_1.NUL;
      else
         -- We keep the ending zero
         return Source (Source'First .. Index);
      end if;
   end To_TPString;

   function To_TPString (Size : Byte; Source : String) return TPString is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
      use Ada.Strings.Fixed;
   begin
      if Index = 0 then
         if Source'Length <= Size then
            return Source & Ada.Characters.Latin_1.NUL & (Size - Source'Length) * ' ';
         else
            return Source (Source'First .. Source'First + Size - 1) & Ada.Characters.Latin_1.NUL;
         end if;
      else
         -- We keep the ending zero
         if Index < Size then
            return Source (Source'First .. Index) & (Size - Index + Source'First) * ' ';
         else
            return Source (Source'First .. Source'First + Size - 1) & Ada.Characters.Latin_1.NUL;
         end if;
      end if;
   end To_TPString;

   function To_String (Source : TPString) return String is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
   begin
      if Index = 0 then
         return Source;
      else
         return Source (Source'First .. Index - 1); -- Without ending zero
      end if;
   end To_String;

   function "+" (C : Char) return String is
   begin
      return (1 => C);
   end "+";

   function "+" (Left : String; Right : String) return String is
      IndexLeft  : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
   begin
      if IndexLeft = 0 then
         if IndexRight = 0 then
            return Left & Right & Ada.Characters.Latin_1.NUL;
         else
            return Left & Right;
         end if;
      else
         if IndexRight = 0 then
            return Left (Left'First .. IndexLeft - 1) & Right & Ada.Characters.Latin_1.NUL;
         else
            return Left (Left'First .. IndexLeft - 1) & Right;
         end if;
      end if;
   end "+";

   function "+" (Left : Char; Right : String) return String is
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
   begin
      if IndexRight = 0 then
         return Left & Right & Ada.Characters.Latin_1.NUL;
      else
         return Left & Right;
      end if;
   end "+";

   function "+" (Left : String; Right : Char) return String is
      IndexLeft : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
   begin
      if IndexLeft = 0 then
         return Left & Right & Ada.Characters.Latin_1.NUL;
      else
         return Left (Left'First .. IndexLeft - 1) & Right & Ada.Characters.Latin_1.NUL;
      end if;
   end "+";

   function "+" (Left : Char; Right : Char) return String is
   begin
      return Left & Right & Ada.Characters.Latin_1.NUL;
   end "+";

   procedure Assign_String (Dest : out String; Source : String) is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
   begin
      if Index = 0 then
         if Dest'Length > Source'Length then
            Dest (Dest'First .. Dest'First + Source'Length - 1) := Source;
            if Source'Length = 0 then
               Dest (Dest'First) := Ada.Characters.Latin_1.NUL;
            else
               Dest (Dest'First + Source'Length) := Ada.Characters.Latin_1.NUL;
            end if;
         else
            -- Source is truncated at dest'lenght and last is forced to zero
            Dest             := Source (Source'First .. Source'First + Dest'Length - 1);
            Dest (Dest'Last) := Ada.Characters.Latin_1.NUL;
         end if;
      else
         if Dest'Length > Index - Source'First then
            Dest (Dest'First .. Dest'First + Index - Source'First) :=
              Source (Source'First .. Index);
         else
            -- Source is truncated at dest'lenght and last is forced to zero
            Dest             := Source (Source'First .. Source'First + Dest'Length - 1);
            Dest (Dest'Last) := Ada.Characters.Latin_1.NUL;
         end if;
      end if;
   end Assign_String;

   function Is_Equal (Left, Right : String) return Boolean is
      IndexLeft  : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
   begin
      if IndexLeft = 0 then
         if IndexRight = 0 then
            return Left = Right;
         else
            return Left = Right (Right'First .. IndexRight - 1);
         end if;
      else
         if IndexRight = 0 then
            return Left (Left'First .. IndexLeft - 1) = Right;
         else
            return Left (Left'First .. IndexLeft - 1) = Right (Right'First .. IndexRight - 1);
         end if;
      end if;
   end Is_Equal;

   procedure Finalize (F : in out File) is
   begin
      GNAT.OS_Lib.Free (F.Name);
   end Finalize;

   procedure Finalize (F : in out Text) is
   begin
      GNAT.OS_Lib.Free (F.Name);
   end Finalize;

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

   N1_Mutex : Mutex;
   N2_Mutex : Mutex;

   task Control is
      entry Start;
      entry Stop;
      entry Over;
   end Control;

   task type Principal;
   type Principal_Task_Access is access Principal;
   procedure Free is new Ada.Unchecked_Deallocation (Principal, Principal_Task_Access);

   IntPrincipalTask : Principal_Task_Access;
   IntPrincipalProc : TPProc := null;
   IntStartButton   : Gnoga.Gui.Element.Common.Button_Type;
   IntStopButton    : Gnoga.Gui.Element.Common.Button_Type;

   task body Principal is
   begin
      IntPrincipalProc.all;
      Control.Over;
   exception
      when Halt =>
         Control.Over;
      when E : others =>
         -- Write to Stdout as CRT text view may not be readable
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Control.Over;
   end Principal;

   task body Control is
   begin
      loop
         select
            accept Start;
            IntPrincipalTask := new Principal;
            select
               accept Over;
            or
               accept Stop;
               if not IntPrincipalTask'Terminated then
                  abort IntPrincipalTask.all;
               end if;
            end select;
            Free (IntPrincipalTask);
            N1_Mutex.Release;
            N2_Mutex.Release;
            IntStopButton.Disabled;
            IntStartButton.Disabled (False);
         or
            terminate;
         end select;
      end loop;
   end Control;

   IntKeyBuffer : String (1 .. 100);
   IntKeyRead   : Positive := IntKeyBuffer'First;
   IntKeyWrite  : Positive := IntKeyBuffer'First;

   procedure Init_Key is
   begin
      IntKeyRead := IntKeyWrite;
   end Init_Key;

   procedure Write_Key (Ch : Char) is
   begin
      IntKeyBuffer (IntKeyWrite) := Ch;
      IntKeyWrite                := IntKeyWrite + 1;
      if IntKeyWrite > IntKeyBuffer'Last then
         IntKeyWrite := IntKeyBuffer'First;
      end if;
   end Write_Key;

   function Is_Key_Pressed return Boolean is
   begin
      delay 0.01;
      return IntKeyWrite /= IntKeyRead;
   end Is_Key_Pressed;

   function Read_Key return Char is
      Ch : Char;
   begin
      while not Is_Key_Pressed loop
         delay 0.01;
      end loop;
      Ch         := IntKeyBuffer (IntKeyRead);
      IntKeyRead := IntKeyRead + 1;
      if IntKeyRead > IntKeyBuffer'Last then
         IntKeyRead := IntKeyBuffer'First;
      end if;
      return Ch;
   end Read_Key;

   procedure On_Start_Clicked (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      IntStartButton.Disabled;
      IntStopButton.Disabled (False);
      Init_Key;
      Control.Start;
   end On_Start_Clicked;

   procedure On_Stop_Clicked (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Control.Stop;
   end On_Stop_Clicked;

   procedure On_Quit_Clicked (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      if IntPrincipalTask /= null and then not IntPrincipalTask'Terminated then
         Control.Stop;
      end if;
      Gnoga.Application.Singleton.End_Application;
   end On_Quit_Clicked;

   procedure On_Key_Press_Handler
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      pragma Unreferenced (Object);
      Ch : constant Character := Ada.Characters.Conversions.To_Character (Keyboard_Event.Key_Char);
      use type Gnoga.Gui.Base.Keyboard_Message_Type;
   begin
--        Gnoga.Log (Keyboard_Event.Key_Code'Img & ',' & Keyboard_Event.Key_Char'Img);
      -- ASCII char
      if Ch not in Ada.Characters.Latin_1.NUL | Ada.Characters.Latin_1.CR | Ada.Characters.Latin_1.ESC then
         -- Ctrl-C
--           if Char'Pos (Ch) = Char'Pos ('C') - Char'Pos ('@') then
--              Gtk.Text_Buffer.Copy_Clipboard
--                (Gtk.Text_View.Get_Buffer (Aera_Text),
--                 Gtk.Clipboard.Get);
--              return True;
--           end if;
         -- Ctrl-V
--           if Char'Pos (Ch) = Char'Pos ('V') - Char'Pos ('@') then
--              declare
--                 S : constant String :=
--                   Glib.Convert.Locale_From_UTF8 (Gtk.Clipboard.Wait_For_Text (Gtk.Clipboard.Get));
--              begin
--                 for Ind in S'Range loop
--                    Write_Key (S (Ind));
--                 end loop;
--              end;
--              return True;
--           end if;
         Write_Key (Ch);
         return;
      end if;
      -- Other special keys
      if Keyboard_Event.Message = Gnoga.Gui.Base.Key_Down then
         case Keyboard_Event.Key_Code is
            when Gnoga.Types.Key_Codes.Key_BackSpace =>
               Write_Key (Ada.Characters.Latin_1.BS);
            when Gnoga.Types.Key_Codes.Key_Tab =>
               Write_Key (Ada.Characters.Latin_1.HT);
            when Gnoga.Types.Key_Codes.Key_Enter =>
               Write_Key (Ada.Characters.Latin_1.CR);
            when Gnoga.Types.Key_Codes.Key_Esc =>
               Write_Key (Ada.Characters.Latin_1.ESC);
            when Gnoga.Types.Key_Codes.Key_F1 .. Gnoga.Types.Key_Codes.Key_F10 =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               -- Alt modifier
               if Keyboard_Event.Alt then
                  Write_Key
                    (Char'Val (Keyboard_Event.Key_Code - Gnoga.Types.Key_Codes.Key_F1 + 104));
               -- Control modifier
               elsif Keyboard_Event.Control then
                  Write_Key
                    (Char'Val (Keyboard_Event.Key_Code - Gnoga.Types.Key_Codes.Key_F1 + 94));
               -- Shift modifier
               elsif Keyboard_Event.Shift then
                  Write_Key
                    (Char'Val (Keyboard_Event.Key_Code - Gnoga.Types.Key_Codes.Key_F1 + 84));
               -- No modifier
               else
                  Write_Key
                    (Char'Val (Keyboard_Event.Key_Code - Gnoga.Types.Key_Codes.Key_F1 + 59));
               end if;
            when Gnoga.Types.Key_Codes.Key_Home =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key ('w');
               else
                  Write_Key ('G');
               end if;
            when Gnoga.Types.Key_Codes.Key_Left =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key ('s');
               else
                  Write_Key ('K');
               end if;
            when Gnoga.Types.Key_Codes.Key_Up =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key (Char'Val (141));
               else
                  Write_Key ('H');
               end if;
            when Gnoga.Types.Key_Codes.Key_Right =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key ('t');
               else
                  Write_Key ('M');
               end if;
            when Gnoga.Types.Key_Codes.Key_Down =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key (Char'Val (145));
               else
                  Write_Key ('P');
               end if;
            when Gnoga.Types.Key_Codes.Key_Page_Up =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key (Char'Val (132));
               else
                  Write_Key ('I');
               end if;
            when Gnoga.Types.Key_Codes.Key_Page_Down =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key ('v');
               else
                  Write_Key ('Q');
               end if;
            when Gnoga.Types.Key_Codes.Key_End =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key ('u');
               else
                  Write_Key ('O');
               end if;
            when Gnoga.Types.Key_Codes.Key_Delete =>
               Write_Key (Ada.Characters.Latin_1.NUL);
               if Keyboard_Event.Control then
                  Write_Key (Char'Val (147));
               else
                  Write_Key ('S');
               end if;
            when others =>
               null;
         end case;
      end if;
   end On_Key_Press_Handler;

   IntWindow                      : Gnoga.Gui.Window.Window_Type;
   IntCursorLine, IntCursorColumn : Natural := 0;
   IntCRTView                     : Gnoga.Gui.Plugin.Ace_Editor.Ace_Editor_Type;
--     IntGetTag                      : TPProcGetTag;

   procedure Deactivate_Key_Handler (View : in out Gnoga.Gui.Plugin.Ace_Editor.Ace_Editor_Type) is
   begin
      View.Editor_Execute
      ("keyBinding.addKeyboardHandler(function() { return { passEvent: true, command: ""null"" }})");
   end Deactivate_Key_Handler;

   procedure Put (S : String; Update_Cursor_Position : Boolean := True) is
      Ada_String : constant String := To_String (S);
   begin
--        Gtk.Text_Buffer.Insert_With_Tags
--          (Gtk.Text_View.Get_Buffer (Aera_Text),
--           Index,
--           Glib.Convert.Locale_To_UTF8 (To_String (S)),
--           IntTag);
      N1_Mutex.Seize;
      IntCRTView.Insert (IntCursorLine, IntCursorColumn, Ada_String);
      IntCursorColumn := IntCursorColumn + Ada_String'Length;
      if Update_Cursor_Position then
         IntCRTView.Navigate_To (IntCursorLine, IntCursorColumn);
      end if;
      N1_Mutex.Release;
   end Put;

   procedure New_Line (Update_Cursor_Position : Boolean := True) is
   begin
      N1_Mutex.Seize;
      IntCRTView.InsertNewLine (IntCursorLine, IntCursorColumn);
      IntCursorLine   := IntCursorLine + 1;
      IntCursorColumn := 0;
      if Update_Cursor_Position then
         IntCRTView.Navigate_To (IntCursorLine, IntCursorColumn);
      end if;
      N1_Mutex.Release;
   end New_Line;

   procedure Put_Line (S : String; Update_Cursor_Position : Boolean := True) is
   begin
      N2_Mutex.Seize;
      Put (To_String (S), False);
      New_Line (Update_Cursor_Position);
      N2_Mutex.Release;
   end Put_Line;

   function Get_Line return String is
      S        : String   := (1 .. 255 + 1 => '@'); -- Turbo Pascal string size
      Last     : Positive := S'First;
      Current  : Positive := S'First;
      StartRow : Natural;
      EndRow   : Natural;
      Ch       : Char;
   begin
      N2_Mutex.Seize;
      StartRow := IntCRTView.Current_Column;
      EndRow   := StartRow;
      loop
         Ch := Read_Key;
--           Gnoga.Log (Ch'Img);
         case Ch is
            when Ada.Characters.Latin_1.NUL =>
               Ch := Read_Key;
--                 Gnoga.Log (Ch'Img);
               case Ch is
                  when 'G' =>  -- Home
                     IntCRTView.Navigate_To (IntCursorLine, StartRow);
                     IntCursorColumn := StartRow;
                     Current         := S'First;
                  when 'K' => -- Left
                     if Current > S'First then
                        IntCRTView.Navigate_Left (1);
                        IntCursorColumn := IntCursorColumn - 1;
                        Current         := Current - 1;
                     end if;
                  when 'H' =>
                     null; -- Up
                  when 'M' =>  -- Right
                     if Current < Last then
                        IntCRTView.Navigate_Right (1);
                        IntCursorColumn := IntCursorColumn + 1;
                        Current         := Current + 1;
                     end if;
                  when 'P' =>
                     null; -- Down
                  when 'O' =>  -- End
                     IntCRTView.Navigate_To (IntCursorLine, EndRow);
                     IntCursorColumn := EndRow;
                     Current         := Last;
                  when others =>
                     null;
               end case;
            when Ada.Characters.Latin_1.BS => -- BackSpace
               IntCRTView.Navigate_To (IntCursorLine, IntCursorColumn);
               if Current > S'First then
                  Current := Current - 1;
                  Last    := Last - 1;
                  IntCRTView.Backspace;
                  EndRow          := EndRow - 1;
                  IntCursorColumn := IntCursorColumn - 1;
                  S               :=
                    S (S'First .. Current - 1) &
                    S (Current + 1 .. S'Last) &
                    Ada.Characters.Latin_1.NUL;
               end if;
            when Ada.Characters.Latin_1.ESC => -- Escape
               IntCRTView.Remove_In_Line (IntCursorLine, StartRow, EndRow);
               IntCursorColumn := StartRow;
               IntCRTView.Navigate_To (IntCursorLine, IntCursorColumn);
               EndRow  := StartRow;
               Current := S'First;
               Last    := S'First;
            when Ada.Characters.Latin_1.CR => -- Carriage Return
               IntCRTView.Navigate_To (IntCursorLine, EndRow);
               IntCursorColumn := EndRow;
               New_Line;
               exit; -- Exit loop
            when others => -- Regular characters
               if Current < S'Last then
                  Put ((1 => Ch));
                  EndRow  := EndRow + 1;
                  S       := S (S'First .. Current - 1) & Ch & S (Current .. S'Last - 1);
                  Last    := Last + 1;
                  Current := Current + 1;
               end if;
         end case;
      end loop;
      N2_Mutex.Release;
      S (Positive'Min (Last, S'Last)) := Ada.Characters.Latin_1.NUL;
      return S;
   end Get_Line;

   procedure Get_Line is
   begin
      N2_Mutex.Seize;
      loop
         exit when Read_Key = Ada.Characters.Latin_1.CR;
      end loop;
      New_Line;
      N2_Mutex.Release;
   end Get_Line;

   function Where_X return Byte is
   begin
      return Byte (IntCursorColumn) + 1;
   end Where_X;

   function Where_Y return Byte is
   begin
      return Byte (IntCursorLine) + 1;
   end Where_Y;

   procedure Goto_XY (X, Y : Byte) is
   begin
      N1_Mutex.Seize;
      IntCRTView.Navigate_To (X, Y);
      N1_Mutex.Release;
   end Goto_XY;

   procedure Clr_Scr is
   begin
      N1_Mutex.Seize;
      IntCRTView.Text ("");
      IntCursorLine   := 0;
      IntCursorColumn := 0;
      IntCRTView.Navigate_To (0, 0);
      N1_Mutex.Release;
   end Clr_Scr;

   procedure Clr_Eol is
      X : constant Byte := Where_X;
      Y : constant Byte := Where_Y;
   begin
      N2_Mutex.Seize;
      Goto_XY (X, Y);
      IntCRTView.Remove_To_Line_End;
      New_Line;
      N2_Mutex.Release;
   end Clr_Eol;

   procedure Ins_Line is
      X : constant Byte := Where_X;
      Y : constant Byte := Where_Y;
   begin
      N2_Mutex.Seize;
      Goto_XY (1, Y + 1);
      New_Line;
      Goto_XY (X, Y);
      N2_Mutex.Release;
   end Ins_Line;

   procedure Del_Line is
      Y : constant Byte := Where_Y;
   begin
      N1_Mutex.Seize;
      IntCRTView.Remove_Lines (Y, Y);
      N1_Mutex.Release;
   end Del_Line;

   IntCRTInitProc : TPProc := null;

   procedure Init_CRT (InitProc : TPProc) is -- TODO GetTag : TPProcGetTag) is
   begin
      IntCRTInitProc := InitProc;
--        IntGetTag      := GetTag;
   end Init_CRT;

   IntDebugForm     : Gnoga.Gui.Element.Form.Form_Type;
   IntDebugCheckBox : Gnoga.Gui.Element.Form.Check_Box_Type;
   IntDebugLabel    : Gnoga.Gui.Element.Form.Label_Type;

   function Debug return Boolean is
   begin
      return IntDebugCheckBox.Checked;
   end Debug;

   IntQuitButton : Gnoga.Gui.Element.Common.Button_Type;
   IntGrid       : Gnoga.Gui.View.Grid.Grid_View_Type;

   procedure Activate_Win_CRT;

   procedure Init (My_Principal_Proc : TPProc) is
   begin
      IntPrincipalProc := My_Principal_Proc;

      Gnoga.Application.Title ("TP7Ada");
      Gnoga.Application.Singleton.Initialize (IntWindow);
      ZanyBlue.Text.Locales.Set_Locale
        (ZanyBlue.Text.Locales.Make_Locale_Narrow
           (Gnoga.Gui.Navigator.Language (IntWindow) & ".ISO8859-1"));
      IntGrid.Create
      (IntWindow,
       ((Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.SPN)), Set_Sizes =>
         False);

      IntGrid.Panel (1, 2).Width (640 + 25); -- Graph window width + lift size
      IntGrid.Panel (1, 2).Height (480 + 25); -- Graph window heigth + lift size

      IntGrid.Panel (1, 1).Border;
      IntGrid.Panel (1, 2).Border;
      IntGrid.Panel (2, 1).Border;

      IntStartButton.Create (IntGrid.Panel (1, 1).all, "Start");
      IntStartButton.On_Click_Handler (On_Start_Clicked'Access);

      IntStopButton.Create (IntGrid.Panel (1, 1).all, "Stop");
      IntStopButton.Disabled;
      IntStopButton.On_Click_Handler (On_Stop_Clicked'Access);

      IntQuitButton.Create (IntGrid.Panel (1, 1).all, "Quit");
      IntQuitButton.On_Click_Handler (On_Quit_Clicked'Access);

      IntDebugForm.Create (IntGrid.Panel (1, 1).all);
      IntDebugCheckBox.Create (IntDebugForm);
      IntDebugLabel.Create (IntDebugForm, IntDebugCheckBox, "Debug");

      if IntCRTInitProc /= null then
         Activate_Win_CRT;
         IntCRTInitProc.all;
      end if;

      IntWindow.On_Key_Press_Handler (On_Key_Press_Handler'Access);
      -- Needed for Safari special keys
      IntWindow.On_Key_Down_Handler (On_Key_Press_Handler'Access);
   end Init;

   function Get_Ctrl_Panel return Gnoga.Gui.View.Pointer_To_View_Base_Class is
   begin
      return IntGrid.Panel (1, 1);
   end Get_Ctrl_Panel;

   IntGraphView : Gnoga.Gui.View.View_Access := null;
   function Get_Graph_View return Gnoga.Gui.View.View_Access is
      use type Gnoga.Gui.View.View_Access;
   begin
      if IntGraphView = null then
         IntGraphView := new Gnoga.Gui.View.View_Type;
         IntGraphView.Create (IntGrid.Panel (1, 2).all);
         -- Force scrollbar display even not necessary
         IntGraphView.Overflow (Gnoga.Gui.Element.Scroll);
         -- Force children to fit parent size
         IntGraphView.Fill_Parent;
      end if;
      return IntGraphView;
   end Get_Graph_View;

   function Get_CRT_Panel return Gnoga.Gui.View.Pointer_To_View_Base_Class is
   begin
      return IntGrid.Panel (2, 1);
   end Get_CRT_Panel;

   IntGraphCanvas : Gnoga.Gui.Element.Canvas.Canvas_Access := null;
   procedure Set_Graph_Canvas (Canvas : not null Gnoga.Gui.Element.Canvas.Canvas_Access) is
   begin
      IntGraphCanvas := Canvas;
   end Set_Graph_Canvas;
   function Get_Graph_Canvas return Gnoga.Gui.Element.Canvas.Canvas_Access is
   begin
      return IntGraphCanvas;
   end Get_Graph_Canvas;

   IntMouseEventHandler : Gnoga.Gui.Base.Mouse_Event := null;
   procedure Set_Mouse_Event (Event_Handler : not null Gnoga.Gui.Base.Mouse_Event) is
   begin
      IntMouseEventHandler := Event_Handler;
   end Set_Mouse_Event;
   procedure Get_Mouse_Event (Event_Handler : out Gnoga.Gui.Base.Mouse_Event) is
   begin
      Event_Handler := IntMouseEventHandler;
   end Get_Mouse_Event;

   procedure Activate_Win_CRT is
   begin
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (IntWindow);
      --  Wait for Ace is loaded (Firefox)
      delay 2.0;
      IntCRTView.Create (Get_CRT_Panel.all);
      -- Force view to be read only and deactivate ACR keyboard handler
      IntCRTView.Read_Only;
      Deactivate_Key_Handler (IntCRTView);
      -- Force children to fit parent size
      IntCRTView.Fill_Parent;
      IntCRTView.Set_Highlight_Selected_Word (False);
      IntCRTView.Set_Highlight_Active_Line (False);
      IntCRTView.Set_Show_Print_Margin (False);
   end Activate_Win_CRT;

end TP7;
