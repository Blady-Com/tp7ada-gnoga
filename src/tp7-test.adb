-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-test.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 10.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 22 février 2015
-- ROLE DU CSU                      : Unité de test.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            : Ada 2005, GNOGA 1.1a
--
-- COPYRIGHT                        : (c) Pascal Pignard 2011-2015
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Gnoga.Gui.Element.Form;
with TP7.System;

package body TP7.Test is

   type TestProcRec is record
      Proc     : TPProc;
      IsMarked : Gnoga.Gui.Element.Form.Check_Box_Access;
   end record;
   package TestProcVect is new Ada.Containers.Vectors (Positive, TestProcRec);
   TestProcs : TestProcVect.Vector;
   TestForm  : Gnoga.Gui.Element.Form.Form_Type;

   ---------
   -- Add --
   ---------

   procedure Add (TestProc : TPProc; TestName : TPString) is
      TestCheckBox : constant Gnoga.Gui.Element.Form.Check_Box_Access :=
        new Gnoga.Gui.Element.Form.Check_Box_Type;
      Dummy_TestLabel : Gnoga.Gui.Element.Form.Label_Type;
   begin
      if TestProcs.Is_Empty then
         Get_Ctrl_Panel.Horizontal_Rule;
         TestForm.Create (Get_Ctrl_Panel.all);
      end if;
      TestCheckBox.Create (Form => TestForm, Name => TestName);
      Dummy_TestLabel.Create (TestForm, TestCheckBox.all, TestName);
      TestForm.New_Line;
      TestProcs.Append ((TestProc, TestCheckBox));
   end Add;

   ---------------
   -- SelectAll --
   ---------------

   procedure SelectAll is
      procedure SelectTest (Position : in TestProcVect.Cursor) is
      begin
         TestProcVect.Element (Position).IsMarked.Checked;
      end SelectTest;
   begin
      TestProcs.Iterate (SelectTest'Access);
   end SelectAll;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      procedure ExecuteTest (Position : in TestProcVect.Cursor) is
      begin
         if TestProcVect.Element (Position).IsMarked.Checked then
            if TP7.Debug then
               TP7.System.Writeln ("Test of " + TestProcVect.Element (Position).IsMarked.Name);
            end if;
            TestProcVect.Element (Position).Proc.all;
         end if;
      exception
         when TP7.Halt =>
            -- Don't stop main task just run test next if any
            null;
      end ExecuteTest;
   begin
      -- Can't use Iterate because of Program_Error when finalization during aborting
      -- TestProcs.Iterate (ExecuteTest'Access);
      for Ind in Positive'First .. Positive (TestProcs.Length) loop
         ExecuteTest (TestProcs.To_Cursor (Ind));
      end loop;
   end Execute;

end TP7.Test;
