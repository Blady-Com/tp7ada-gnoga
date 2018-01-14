-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-mouse.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 10.1a
-- DATE DE LA DERNIERE MISE A JOUR  : 6 avril 2016
-- ROLE DU CSU                      : Unité d'émulation de la souris DOS.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            : Ada 2005, GNOGA 1.1a
--
-- COPYRIGHT                        : (c) Pascal Pignard 1988-2016
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Gnoga.Gui.Base;
with TP7.System;

package body TP7.Mouse is

   -- Note : delai is required on all pooling procedures

   MaxButton : constant := 3;
   type ButtonState is record
      Pressed, Released, DoubleClic, TripleClic          : Boolean;
      Press_Count, Release_Count                         : Integer;
      LastXPress, LastYPress, LastXRelease, LastYRelease : Integer;
   end record;
   type ButtonsState is array (1 .. MaxButton) of ButtonState;
   IntButtons                         : ButtonsState;
   IntVisible                         : Integer;
   IntCursor                          : Integer;
   IntPositionX, IntPositionY         : Integer;
   IntLastPositionX, IntLastPositionY : Integer;
   IntScroll                          : Integer;
--     To_Integer : constant array (Gdk.Event.Gdk_Scroll_Direction) of Integer :=
--       (ScrollUp, ScrollDown, ScrollLeft, ScrollRight, Scroll_Smooth);
   type Cursor_Shapes is
     (auto,
      default,
      none,
      context_menu,
      help,
      pointer,
      progress,
      wait,
      cell,
      crosshair,
      text,
      vertical_text,
      alias,
      copy,
      move,
      no_drop,
      not_allowed,
      e_resize,
      n_resize,
      ne_resize,
      nw_resize,
      s_resize,
      se_resize,
      sw_resize,
      w_resize,
      ew_resize,
      ns_resize,
      nesw_resize,
      nwse_resize,
      col_resize,
      row_resize,
      all_scroll,
      zoom_in,
      zoom_out);
   function To_String (Cursor : Cursor_Shapes) return String is
      use Ada.Strings.Maps;
      CM : constant Ada.Strings.Maps.Character_Mapping :=
        To_Mapping (To_Sequence (To_Set ('_')), To_Sequence (To_Set ('-')));
   begin
      return Ada.Strings.Fixed.Translate (Cursor_Shapes'Image (Cursor), CM);
   end To_String;

   ---------------
   -- MouseInit --
   ---------------

   function MouseInit return Boolean is
   begin
      IntButtons   := (others => (False, False, False, False, 0, 0, 0, 0, 0, 0));
      IntVisible   := 0;
      IntCursor    := 0;
      IntPositionX := 0;
      IntPositionY := 0;
      IntScroll    := 0;
      HideMouse; -- Mouse hidden by default
      return True;
   end MouseInit;

   ---------------
   -- NbrButton --
   ---------------

   function NbrButton return Integer is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction NbrButton n'est pas définie !");
      end if;
      return MaxButton;
   end NbrButton;

   ---------------
   -- HideMouse --
   ---------------

   procedure HideMouse is
   begin
      IntVisible := IntVisible - 1;
      if IntVisible = -1 then
         TP7.Get_Graph_Canvas.Cursor (To_String (none));
      end if;
   end HideMouse;

   ---------------
   -- ShowMouse --
   ---------------

   procedure ShowMouse is
   begin
      IntVisible := IntVisible + 1;
      if IntVisible = 0 then
         TP7.Get_Graph_Canvas.Cursor (To_String (Cursor_Shapes'Val (IntCursor)));
      end if;
   end ShowMouse;

   -------------
   -- GetXPos --
   -------------

   function GetXPos return Integer is
   begin
      delay 0.01;
      return IntPositionX;
   end GetXPos;

   -------------
   -- GetYPos --
   -------------

   function GetYPos return Integer is
   begin
      delay 0.01;
      return IntPositionY;
   end GetYPos;

   ---------------
   -- GetStatus --
   ---------------

   function GetStatus return Integer is
   begin
      delay 0.01;
      return Boolean'Pos (IntButtons (1).Pressed) +
        Boolean'Pos (IntButtons (2).Pressed) * 2 +
        Boolean'Pos (IntButtons (3).Pressed) * 4;
   end GetStatus;

   ---------------
   -- GetScroll --
   ---------------

   function GetScroll return Integer is
   begin
      delay 0.01;
      return Scroll : constant Integer := IntScroll do
         IntScroll := 0;
      end return;
   end GetScroll;

   ----------------------
   -- MouseNewPosition --
   ----------------------

   procedure MouseNewPosition (NouvX, NouvY : Integer) is
      pragma Unreferenced (NouvX, NouvY);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseNewPosition n'est pas définie !");
      end if;
   end MouseNewPosition;

   -----------------
   -- ButtonPress --
   -----------------

   function ButtonPress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).Pressed do
         IntButtons (Button).Pressed := False;
      end return;
   end ButtonPress;

   function ButtonDoublePress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).DoubleClic do
         IntButtons (Button).DoubleClic := False;
      end return;
   end ButtonDoublePress;

   function ButtonTriplePress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).TripleClic do
         IntButtons (Button).TripleClic := False;
      end return;
   end ButtonTriplePress;

   ----------------
   -- CountPress --
   ----------------

   function CountPress (Button : Integer) return Integer is
   begin
      delay 0.01;
      return Count : constant Integer := IntButtons (Button).Press_Count do
         IntButtons (Button).Press_Count := 0;
      end return;
   end CountPress;

   ----------------
   -- LastXPress --
   ----------------

   function LastXPress (Button : Integer) return Integer is
   begin
      return IntButtons (Button).LastXPress;
   end LastXPress;

   ----------------
   -- LastYPress --
   ----------------

   function LastYPress (Button : Integer) return Integer is
   begin
      return IntButtons (Button).LastYPress;
   end LastYPress;

   -------------------
   -- ButtonRelease --
   -------------------

   function ButtonRelease (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Released : constant Boolean := IntButtons (Button).Released do
         IntButtons (Button).Released := False;
      end return;
   end ButtonRelease;

   ------------------
   -- CountRelease --
   ------------------

   function CountRelease (Button : Integer) return Integer is
   begin
      delay 0.01;
      return Count : constant Integer := IntButtons (Button).Release_Count do
         IntButtons (Button).Release_Count := 0;
      end return;
   end CountRelease;

   ------------------
   -- LastXRelease --
   ------------------

   function LastXRelease (Button : Integer) return Integer is
   begin
      return IntButtons (Button).LastXRelease;
   end LastXRelease;

   ------------------
   -- LastYRelease --
   ------------------

   function LastYRelease (Button : Integer) return Integer is
   begin
      return IntButtons (Button).LastYRelease;
   end LastYRelease;

   -----------------
   -- MouseWindow --
   -----------------

   procedure MouseWindow (MinX, MinY, MaxX, MaxY : Integer) is
      pragma Unreferenced (MinX, MinY, MaxX, MaxY);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseWindow n'est pas définie !");
      end if;
   end MouseWindow;

   ------------------------
   -- MouseSetGraphBlock --
   ------------------------

   procedure MouseSetGraphBlock (Cursor : Integer) is
   begin
      IntCursor := Cursor;
      if IntVisible >= 0 then
         TP7.Get_Graph_Canvas.Cursor (To_String (Cursor_Shapes'Val (Cursor)));
      end if;
   end MouseSetGraphBlock;

   procedure MouseSetGraphBlock
     (Source             : String;
      Mask               : String;
      FGColor, BGColor   : Integer;
      HotSpotX, HotSpotY : Integer)
   is
      pragma Unreferenced (Source, Mask, BGColor, FGColor, HotSpotY, HotSpotX);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseSetGraphBlock n'est pas définie !");
      end if;
   end MouseSetGraphBlock;

   -----------------------
   -- MouseSetTextBlock --
   -----------------------

   procedure MouseSetTextBlock (MType, MScreen, MCursor : Integer) is
      pragma Unreferenced (MType, MScreen, MCursor);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseSetTextBlock n'est pas définie !");
      end if;
   end MouseSetTextBlock;

   -----------------------
   -- MouseXMotionCount --
   -----------------------

   function MouseXMotionCount return Integer is
   begin
      delay 0.01;
      return DeltaX : constant Integer := IntPositionX - IntLastPositionX do
         IntLastPositionX := IntPositionX;
      end return;
   end MouseXMotionCount;

   -----------------------
   -- MouseYMotionCount --
   -----------------------

   function MouseYMotionCount return Integer is
   begin
      delay 0.01;
      return DeltaY : constant Integer := (IntPositionY - IntLastPositionY) do
         IntLastPositionY := IntPositionY;
      end return;
   end MouseYMotionCount;

   ---------------------
   -- MousePixelRatio --
   ---------------------

   procedure MousePixelRatio (MXRatio, MYRatio : Integer) is
      pragma Unreferenced (MXRatio, MYRatio);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MousePixelRatio n'est pas définie !");
      end if;
   end MousePixelRatio;

   --------------------
   -- On_Mouse_Event --
   --------------------

   procedure On_Mouse_Event
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event :        Gnoga.Gui.Base.Mouse_Event_Record)
   is
      pragma Unreferenced (Object);
      function Is_Button_Pressed
        (BN : Integer;
         ME : Gnoga.Gui.Base.Mouse_Event_Record) return Boolean
      is
      begin
         case BN is
            when 1 =>
               return ME.Left_Button;
            when 2 =>
               return ME.Middle_Button;
            when 3 =>
               return ME.Right_Button;
            when others =>
               return False;
         end case;
      end Is_Button_Pressed;
      use Gnoga.Gui.Base;
   begin
      --        Gnoga.Log (Mouse_Event.X'img& ','& Mouse_Event.Y'img);
      IntPositionX := Mouse_Event.X;
      IntPositionY := Mouse_Event.Y;
      if Mouse_Event.Message in Double_Click .. Mouse_Up then
         for BN in 1 .. 3 loop
            if Is_Button_Pressed (BN, Mouse_Event) and Mouse_Event.Message = Mouse_Down then
               IntButtons (BN).Pressed    := True;
               IntButtons (BN).LastXPress := IntPositionX;
               IntButtons (BN).LastYPress := IntPositionY;
               if IntButtons (BN).Press_Count < Integer'Last then
                  IntButtons (BN).Press_Count := IntButtons (BN).Press_Count + 1;
               else
                  IntButtons (BN).Press_Count := 0;
               end if;
            end if;
            if Is_Button_Pressed (BN, Mouse_Event) and Mouse_Event.Message = Mouse_Up then
               IntButtons (BN).Released     := True;
               IntButtons (BN).LastXRelease := IntPositionX;
               IntButtons (BN).LastYRelease := IntPositionY;
               if IntButtons (BN).Release_Count < Integer'Last then
                  IntButtons (BN).Release_Count := IntButtons (BN).Release_Count + 1;
               else
                  IntButtons (BN).Release_Count := 0;
               end if;
            end if;
            if Is_Button_Pressed (BN, Mouse_Event) and Mouse_Event.Message = Double_Click then
               IntButtons (BN).DoubleClic := True;
            end if;
         end loop;
      end if;
      --        if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_3button_Press then
      --           IntButtons (Integer (Gdk.Event.Get_Button (Event))).TripleClic   := True;
      --        end if;
      --        if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Scroll then
      --           IntScroll := To_Integer (Gdk.Event.Get_Direction (Event));
      --        end if;
   end On_Mouse_Event;

begin
   TP7.Set_Mouse_Event (On_Mouse_Event'Access);
end TP7.Mouse;
