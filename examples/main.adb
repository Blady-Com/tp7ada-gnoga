with Ada.Text_IO;
with Ada.Exceptions;
with TP7.Test;
with Exemples_TP7;
with BGIDemo;
with Arty;
with Hello_GTKAda;
with Courbes;
with Sudoku;
with Surfaces;
with EditFont;
with ZanyBlue.Text.Locales;

procedure main is

begin
   TP7.Init (TP7.Test.Execute'Access);
   TP7.Test.Add (Exemples_TP7'Access, "Exemples TP7");
   TP7.Test.Add (BGIDemo'Access, "BGI Demo");
   TP7.Test.Add (Arty'Access, "Art Demo");
   TP7.Test.Add (Hello_GTKAda'Access, "Hello GtkAda");
   TP7.Test.Add (Courbes'Access, "Curves Plotting Demo");
   TP7.Test.Add (Sudoku'Access, "Sudoku");
   TP7.Test.Add (Surfaces'Access, "Surfaces Plotting Demo");
   TP7.Test.Add (EditFont'Access, "CHR font editing");
   --     TP7.Test.SelectAll;
   ZanyBlue.Text.Locales.Set_Locale
     (ZanyBlue.Text.Locales.Make_Locale_Narrow (TP7.Get_Language & ".ISO8859-1"));
   TP7.Main_Loop;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end main;
