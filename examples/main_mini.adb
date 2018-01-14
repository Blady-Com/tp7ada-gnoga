with TP7;
with Hello_GTKAda;
--  with Essai2;
--  with Editfont;

procedure main_mini is

begin
   TP7.Init (hello_gtkada'Access);
--     TP7.Init (essai2'Access);
   TP7.Main_Loop;
end main_mini;
