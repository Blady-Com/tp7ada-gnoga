-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7.ads
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

with System;
with ZanyBlue.Text.Generic_Floats;
private with System.Address_Image;
private with Ada.Characters.Latin_1;
private with Ada.Text_IO;
private with Ada.Finalization;
private with GNAT.OS_Lib;
private with Gnoga.Gui.View;
private with Gnoga.Gui.Base;
private with Gnoga.Gui.Element.Canvas;
private with Gnoga.Application.Singleton;
pragma Elaborate_All (ZanyBlue.Text.Generic_Floats);
-- Needed for : warning: instantiation of "Generic_Buffer" may raise Program_Error
with ZanyBlue.Text.Generic_Buffer;
pragma Elaborate_All (ZanyBlue.Text.Generic_Buffer);

package TP7 is
   pragma Elaborate_Body;

   -- Tips : subtypes of Ada Integer to avoid annoying conversion
   subtype Byte is Standard.Integer range 0 .. 2**8 - 1;
   subtype Word is Standard.Integer range 0 .. 2**16 - 1;
   subtype Shortint is Standard.Integer range -2**7 .. 2**7 - 1;
   -- Tips : Turbo Pascal "Integer" type,
   --        add "subtype Integer is TPInteger;" at the beginning of your code
   subtype TPInteger is Standard.Integer range -2**15 .. 2**15 - 1;
   subtype Longint is Standard.Integer range -2**31 .. 2**31 - 1;

   -- Tips : types defined for mod operations (and, or, not...) and
   --        record components with corresponding TP size
   type Byte1 is mod 2**8;
   for Byte1'Size use 8;
   type Word1 is mod 2**16;
   for Word1'Size use 16;
   type Shortint1 is range -2**7 .. 2**7 - 1;
   for Shortint1'Size use 8;
   type Integer1 is range -2**15 .. 2**15 - 1;
   for Integer1'Size use 16;
   type Longint1 is range -2**31 .. 2**31 - 1;
   for Longint1'Size use 32;

   -- TIPS : type Boolean is same as Ada and subtypes declared to avoid annoying conversion
   subtype ByteBool is Standard.Boolean;
   subtype WordBool is Standard.Boolean;
   subtype LongBool is Standard.Boolean;

   -- Tips : types defined for specific record components with corresponding TP size
   type Boolean1 is (False1, True1);
   for Boolean1 use (False1 => 0, True1 => 1);
   for Boolean1'Size use 1 * 8;
   subtype ByteBool1 is Boolean1;
   type WordBool1 is (False2, True2);
   for WordBool1 use (False2 => 0, True2 => 1);
   for WordBool1'Size use 2 * 8;
   type LongBool1 is (False3, True3);
   for LongBool1 use (False3 => 0, True3 => 1);
   for LongBool1'Size use 4 * 8;

   -- Tips : subtypes of Ada Long_Long_Float to avoid annoying conversion
   subtype Real is Long_Long_Float range -1.7e38 .. 1.7e38;
   subtype Single is Long_Long_Float range -3.4e38 .. 3.4e38;
   subtype Double is Long_Long_Float range -1.7e308 .. 1.7e308;
   --     subtype Extented is Long_Long_Float range -1.7e308 .. 1.7e308; -- 32 bits target
   subtype Extented is Long_Long_Float range -1.1e4932 .. 1.1e4932; -- 64 bits target
   subtype Comp is Long_Long_Integer range -2**63 .. 2**63 - 1;

   -- Zanyblue facilities for Real type
   package Real_Arg is new ZanyBlue.Text.Generic_Floats (Float_Type => Real);
   function "+" (Real_Value : Real) return Real_Arg.Float_Argument_Type renames Real_Arg.Create;

   -- Tips : types defined for record components with corresponding TP size
   type Real1 is digits 6 range -1.7e38 .. 1.7e38;
   for Real1'Size use 6 * 8;
   type Single1 is digits 6 range -3.4e38 .. 3.4e38;
   for Single1'Size use 4 * 8;
   type Double1 is digits 15 range -1.7e308 .. 1.7e308;
   for Double1'Size use 8 * 8;
   --     type Extented1 is digits 15 range -1.7e308 .. 1.7e308; -- 32 bits target
   --     for Extented1'Size use 10 * 8; -- 32 bits target
   type Extented1 is digits 18 range -1.1e4932 .. 1.1e4932; -- 64 bits target
   for Extented1'Size use 16 * 8; -- 64 bits target, take care that TP7 required only 10 bytes
   type Comp1 is range -2**63 .. 2**63 - 1;
   for Comp1'Size use 8 * 8;

   -- Tips : Pointer type size maybe 32 or 64 bits, take care in record components
   subtype Pointer is System.Address;
   nil : Pointer renames System.Null_Address;
   function Pointer_Image (A : Pointer) return String;

   type File is limited private;
   type Text is limited private;

   subtype Char is Character;

   -- Tips : TPString type emulate zero terminated Latin 1 string
   -- Subtype declaration just to distingushed both types
   -- but user can use only String name as internal proc checks for zero
   -- Declare String(1..16) for corresponding String[15] (same user size 15)
   subtype TPString is String;
   -- Convert Ada String to zero terminated string
   function To_TPString (Source : String) return TPString;
   -- Convert Ada String to zero terminated string of specified user size as String[Size]
   function To_TPString (Size : Byte; Source : String) return TPString;
   -- Convert zero terminated string to Ada string
   function To_String (Source : TPString) return String;
   -- String assignment, could be of any type, Ada string or zero terminated string
   procedure Assign_String (Dest : out String; Source : String);
   Null_TPString : constant String;

   function Is_Equal (Left, Right : String) return Boolean;
   --function "/=" (Left, Right: String) return Boolean;
   --   function "<"  (Left, Right: String) return Boolean;
   --  function "<=" (Left, Right: String) return Boolean;
   --  function ">"  (Left, Right: String) return Boolean;
   --   function ">=" (Left, Right: String) return Boolean;
   function "+" (Left : String; Right : String) return String;
   function "+" (Left : Char; Right : String) return String;
   function "+" (Left : String; Right : Char) return String;
   function "+" (Left : Char; Right : Char) return String;
   function "+" (C : Char) return String;

   -- Tips : call your code from main with "Init (MyCode'Access);"
   type TPProc is access procedure;
   procedure Init (My_Principal_Proc : TPProc);
   procedure Main_Loop;

   -- Tips : call this function to test if user as checked Debug in Win_Ctrl
   function Debug return Boolean;

private
   procedure Main_Loop renames Gnoga.Application.Singleton.Message_Loop;

   -- Private constants, types and subprograms
   function Pointer_Image (A : Pointer) return String renames System.Address_Image;
   type File is new Ada.Finalization.Limited_Controlled with record
      File : GNAT.OS_Lib.File_Descriptor := 0;
      Name : GNAT.OS_Lib.String_Access;
   end record;
   procedure Finalize (F : in out File);
   type File_Kind is (File_System, Stdinout, Win_CRT);
   type Text is new Ada.Finalization.Limited_Controlled with record
      Device : File_Kind;
      File   : Ada.Text_IO.File_Type;
      Name   : GNAT.OS_Lib.String_Access;
   end record;
   procedure Finalize (F : in out Text);
   Null_TPString : constant String := (1 => Ada.Characters.Latin_1.NUL);

   -- Internal view for adding widgets in control view
   function Get_Ctrl_Panel return Gnoga.Gui.View.Pointer_To_View_Base_Class;

   -- Internal exception for halt
   Halt : exception;

   -- Internal mutex type
   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Mutex;

   -- Internal I/O procedures to GTK.Text_View
   procedure Put (S : String; Update_Cursor_Position : Boolean := True);
   procedure Put_Line (S : String; Update_Cursor_Position : Boolean := True);
   procedure New_Line (Update_Cursor_Position : Boolean := True);
   function Get_Line return String;
   procedure Get_Line;
   function Is_Key_Pressed return Boolean;
   function Read_Key return Char;
   procedure Goto_XY (X, Y : Byte);
   function Where_X return Byte;
   function Where_Y return Byte;
   procedure Clr_Scr;
   procedure Clr_Eol;
   procedure Ins_Line;
   procedure Del_Line;

   -- Internal registration for init proc in CRT child unit
--     type TPProcGetTag is access procedure (Tag    : out Gtk.Text_Tag.Gtk_Text_Tag;
--     NewTag : out Boolean);
   procedure Init_CRT (InitProc : TPProc); -- TODO GetTag : TPProcGetTag);

   -- Internal registration for event and window connections in Graph child unit
   function Get_Graph_View return Gnoga.Gui.View.View_Access;
   procedure Set_Graph_Canvas (Canvas : not null Gnoga.Gui.Element.Canvas.Canvas_Access);
   function Get_Graph_Canvas return Gnoga.Gui.Element.Canvas.Canvas_Access;
   procedure Set_Mouse_Event (Event_Handler : not null Gnoga.Gui.Base.Mouse_Event);
   procedure Get_Mouse_Event (Event_Handler : out Gnoga.Gui.Base.Mouse_Event);

end TP7;
