with TP7;
with ZanyBlue.Text.Locales;
with Hello_GTKAda;
--  with Essai2;
--  with Editfont;

procedure main_mini is

begin
   TP7.Init (Hello_GTKAda'Access);
   --     TP7.Init (essai2'Access);
   ZanyBlue.Text.Locales.Set_Locale
     (ZanyBlue.Text.Locales.Make_Locale_Narrow (TP7.Get_Language & ".ISO8859-1"));
   TP7.Main_Loop;
end main_mini;
