-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-graph.adb
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

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics;
with Ada.Unchecked_Deallocation;
with Gnoga.Types;
with Gnoga.Types.Colors;
with Gnoga.Gui.Base;
with TP7.System;
pragma Elaborate_All (Gnoga.Types.Colors);

package body TP7.Graph is

   Pi : constant := Ada.Numerics.Pi;
   function Sin (X : Float) return Float renames Ada.Numerics.Elementary_Functions.Sin;
   function Cos (X : Float) return Float renames Ada.Numerics.Elementary_Functions.Cos;

   CCharSize      : constant Integer := 8;
   Area_Canvas    : Gnoga.Gui.Element.Canvas.Canvas_Access;
   Cr             : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   IntX, IntY     : Integer          := 0;
   CoeffX, CoeffY : Float            := 1.0;
   N1_Mutex       : TP7.Mutex;

   type IntTabColors is array (0 .. MaxColors) of Gnoga.Types.RGBA_Type;
   type IntColorPaletteType is record
      Size   : Byte;
      Colors : IntTabColors;
   end record;

   IntColorPalette : IntColorPaletteType :=
     (Size   => MaxColors + 1,
      Colors =>
        (Black        => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Black),
         Blue         => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Blue),
         Green        => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Green),
         Cyan         => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Cyan),
         Red          => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Red),
         Magenta      => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Magenta),
         Brown        => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Brown),
         LightGray    => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Gray),
         DarkGray     => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Dark_Gray),
         LightBlue    => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Blue),
         LightGreen   => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Green),
         LightCyan    => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Cyan),
         LightRed     => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Coral),
         LightMagenta => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Light_Pink),
         Yellow       => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.Yellow),
         White        => Gnoga.Types.Colors.To_RGBA (Gnoga.Types.Colors.White)));

   type Rect is record
      Left, Right, Top, Bottom : TPInteger;
   end record;
   type Point is record
      H, V : TPInteger;
   end record;

   generic
      Size : Natural;
   package PH is
      type Pm_Data_Fields is array (0 .. 6) of Short_Integer;
      type Pm_Info is array (0 .. Size - 1) of Word; --ColorInfo;
      type ColorInfoHandle is access Word; --ColorInfoPtr;
      type Palette;
      type PalettePtr is access Palette;
      type Palette is record
         PmEntries    : Short_Integer;  -- entries in pmTable
         PmDataFields : Pm_Data_Fields; -- private fields
         PmInfo       : Pm_Info;
      end record;
      type PaletteHandle is access PalettePtr;
      --      function Convert is
      --      new
      --  --        Unchecked_Conversion(ApplicationServices.QD.
      --Palettes.PaletteHandle,
      --        PaletteHandle);
   end PH;

   generic
      PixelDepth : Natural;
   package AdaColorTable is
      type CSpecArray is array (0 .. 2**PixelDepth - 1) of Word; --ColorSpec;
      type ColorTable is record
         CtSeed  : Long_Integer;  -- unique identifier for table
         CtFlags : Short_Integer;
         -- high bit: 0 = PixMap; 1 = device
         CtSize  : Short_Integer; -- number of entries in CTTable
         CtTable : CSpecArray;    -- array [0..0] of ColorSpec
      end record;
      type ColorTablePtr is access ColorTable;

      type CTabPtr is access ColorTable;
      --    type CTabHandle is access all CTabPtr;
      type CTabHandle is access CTabPtr;
      --      function Convert is new
      --        Unchecked_Conversion(ApplicationServices.QD.Quickdraw.CTabHandle, CTabHandle);
      --      function Convert is new
      --        Unchecked_Conversion(Handle, CTabHandle);
      --      function Convert is new
      --        Unchecked_Conversion(CTabHandle,
      --ApplicationServices.QD.Quickdraw.CTabHandle);
   end AdaColorTable;

   package CT4 is new AdaColorTable (4);

   -- sauvegarde des paramétres courants
   IntArcCoords : ArcCoordsType;
   IntFillInfo  : FillSettingsType;
   IntFillPat   : FillPatternType;
   IntLineInfo  : LineSettingsType;
   IntTextInfo  : TextSettingsType;
   IntViewPort  : ViewPortType;
   IntPalette   : PaletteType;

   IntColor       : Word;
   IntBkColor     : Word;
   IntMaxRect     : Rect;
   IntGraphResult : Integer := grError;
   IntOperator    : Gnoga.Gui.Element.Canvas.Context_2D.Composite_Method_Type;

   -- Procedure interne renvoyant le motif de remplissage courant
   subtype Pattern is Word;
   function GetIntPat return Pattern is
      ResultGetIntPat : aliased Pattern;
      Ind             : Short_Integer;
   begin
      if IntFillInfo.Pattern = UserFill then
         --        for Ind in Patternarray'range loop
         --          ResultGetIntPat.Pat(Ind) := IntFillPat(Ind + 1);
         --        end loop;
         null;
      else
         case IntFillInfo.Pattern is
            when EmptyFill =>
               Ind := 20;  -- fills area in background color
            when SolidFill =>
               Ind := 1;  -- fills area in solid fill color
            when LineFill =>
               Ind := 27;  -- --- fill
            when LtSlashFill =>
               Ind := 28;  -- /// fill
            when SlashFill =>
               Ind := 26;  -- /// fill with thick lines
            when BkSlashFill =>
               Ind := 16;  -- \\\ fill with thick lines
            when LtBkSlashFill =>
               Ind := 34;  -- \\\ fill
            when HatchFill =>
               Ind := 14;  -- light hatch fill
            when XHatchFill =>
               Ind := 16;  -- heavy cross hatch fill
            when InterleaveFill =>
               Ind := 30;  -- interleaving line fill
            when WideDotFill =>
               Ind := 13; -- Widely spaced dot fill
            when CloseDotFill =>
               Ind := 22; -- Closely spaced dot fill
            when others =>
               null;
         end case;
         --      GetIndPattern(ResultGetIntPat'access, SysPatListId, Ind);
      end if;
      return ResultGetIntPat;
   end GetIntPat;

   function To_Pixel (Value : Gnoga.Types.RGBA_Type) return Gnoga.Types.Pixel_Type is
   begin
      return (Value.Red, Value.Green, Value.Blue, Gnoga.Types.Color_Type (Value.Alpha * 255.0));
   end To_Pixel;

   -- procedure interne recherchant la couleur PC correspondant à celle du pixel
   function GetIndColor (Color : Gnoga.Types.RGBA_Type) return Integer is
      ResultGetIndColor : Integer;
      use type Gnoga.Types.RGBA_Type;
   begin
      ResultGetIndColor := Integer (IntPalette.Size);
      for Ind in reverse 0 .. IntPalette.Size - 1 loop
         if IntColorPalette.Colors (Ind) = Color then
            ResultGetIndColor := Ind;
         end if;
      end loop;
      return ResultGetIndColor;
   end GetIndColor;

   -- procedure interne initialisant la palette graphique
   procedure CreatePalette is
   --        DumCT4    : CT4.CTabHandle;
   --        DumCTable : Word; --CTabHandle;
   --        DumRGB    : aliased rgbcolor;
   begin
      -- Palette graphique :  MaxColors + 1
      --    DumCTable := GetCTable(68);
      --    IntPaletteHdl := NewPalette(MaxColors + 1, DumCTable, pmAnimated,
      --0);
      --SetPalette(IntCWind, IntPaletteHdl, True);
      --    NSetPalette(IntCWind, IntPaletteHdl, pmAllUpdates - 65536);
      -- Réordonnement des couleurs
      --      if Standard.True then
      --      DumCT4 := CT4.Convert(DumCTable);
      --        DumRGB := DumCT4.all.all.CtTable(15).RGB;
      --  --      AnimateEntry(IntCWind, Black, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(6).RGB;
      --        AnimateEntry(IntCWind, Blue, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(9).RGB;
      --        AnimateEntry(IntCWind, Green, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(7).RGB;
      --        AnimateEntry(IntCWind, Cyan, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(3).RGB;
      --        AnimateEntry(IntCWind, Red, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(5).RGB;
      --        AnimateEntry(IntCWind, Magenta, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(10).RGB;
      --        AnimateEntry(IntCWind, Brown, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(13).RGB;
      --        AnimateEntry(IntCWind, LightGray, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(14).RGB;
      --        AnimateEntry(IntCWind, DarkGray, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(6).RGB;
      --        AnimateEntry(IntCWind, LightBlue, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(8).RGB;
      --        AnimateEntry(IntCWind, LightGreen, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(7).RGB;
      --        AnimateEntry(IntCWind, LightCyan, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(2).RGB;
      --        AnimateEntry(IntCWind, LightRed, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(4).RGB;
      --        AnimateEntry(IntCWind, LightMagenta, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(1).RGB;
      --        AnimateEntry(IntCWind, Yellow, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(0).RGB;
      --        AnimateEntry(IntCWind, White, DumRGB'access);
      --        ActivatePalette(IntCWind);
      --      end if;
      --      DisposeCTable(DumCTable);
      --      IntCT4 := CT4.Convert(NewHandle(ColorTable'Size / 8 +
      --(Long_Integer(
      --              MaxColors) * ColorSpec'Size) / 8));
      --      Palette2CTab(IntPaletteHdl, CT4.Convert(IntCT4));
      null;
   end CreatePalette;

   -- procedure interne pour normaliser les coordonnées (x1,y1) - (x2,y2)
   procedure NormRect (X1, Y1, X2, Y2 : in out Integer) is
      procedure SwapI (A, B : in out Integer) is

         Dum : Integer;
      begin
         Dum := A;
         A   := B;
         B   := Dum;
      end SwapI;

   begin
      if X1 > X2 then
         SwapI (X1, X2);
      end if;
      if Y1 > Y2 then
         SwapI (Y1, Y2);
      end if;
   end NormRect;

   procedure NormRect (R : in out Rect) is
      procedure SwapI (A, B : in out Integer) is
         Dum : Integer;
      begin
         Dum := A;
         A   := B;
         B   := Dum;
      end SwapI;
   begin
      if R.Left > R.Right then
         SwapI (R.Left, R.Right);
      end if;
      if R.Top > R.Bottom then
         SwapI (R.Top, R.Bottom);
      end if;
   end NormRect;

   procedure SetRect (R : out Rect; X1, Y1, X2, Y2 : Integer) is
   begin
      R := (Left => X1, Top => Y1, Right => X2, Bottom => Y2);
   end SetRect;

   package FontCHR is
      -- Decode CHR font files from Turbo Pascal
      -- Font structure is :
      -- ID1 : 4 characters -> "PK<BS><BS>"
      -- ID2 : 4 characters -> "BGI "
      -- Font description : chars until <CR><LF><EOF>
      -- Header size : word
      -- Font name : 4 chars
      -- Font file size : word
      -- Font driver major version : byte
      -- Font driver minor version : byte
      -- ID3 : word -> 16#0100#
      -- Zeros : padding until end of the header
      -- ID4 : byte -> 16#2B# (stroke font)
      -- Number of character : word
      -- Undefined : byte
      -- First defined character : ASCII
      -- Offset of character definitions : word
      -- Scan flag : byte
      -- Distance from origin to top of capital : byte
      -- Distance from origin to baseline : byte
      -- Distance from origin to bottom descender : byte
      -- Undefined : 5 bytes
      -- Offset to each character definition : n * word
      -- Character widths : n * byte
      -- Character definition : n * list of commands
      --   Commands :
      --        Byte 1     7   6   5   4   3   2   1   0     bit #
      --                  op1  <seven bit signed X coord>
      --        Byte 2     7   6   5   4   3   2   1   0     bit #
      --                  op2  <seven bit signed Y coord>
      --            Opcodes :
      --          op1=0  op2=0  End of character definition.
      --          op1=1  op2=0  Move the pointer to (x,y)
      --          op1=1  op2=1  Draw from current pointer to (x,y)

      type TabCharWidth is array (Byte range <>) of Byte;
      type PTabCharWidth is access TabCharWidth;
      type Commande is (Move, Line);
      type DescCmd is record
         Cmd  : Commande;
         X, Y : Shortint;
      end record;
      type TabDescCmd is array (Positive range <>) of DescCmd;
      type PTabDescCmd is access TabDescCmd;
      type TabCharCmd is array (Byte range <>) of PTabDescCmd;
      type PTabCharCmd is access TabCharCmd;

      type StructDescFont is record
         Description    : access String;
         Name           : String (1 .. 4);
         MajorVersion   : Byte;
         MinorVersion   : Byte;
         AscenderLine   : Shortint;
         OffsetBaseLine : Shortint;
         DescenderLine  : Shortint;
         CharWidths     : PTabCharWidth;
         CharCmds       : PTabCharCmd;
      end record;
      type DescFont is access StructDescFont;
      function GetFont (Font : Word) return DescFont;
      function To_CodePage437 (Ch : Byte) return Byte;
   end FontCHR;

   package body FontCHR is separate;

   -- *** high-level error handling ***
   function GraphErrorMsg (ErrorCode : Integer) return String is
   begin
      case ErrorCode is
         when grOk =>
            return To_TPString ("No error");
         when grNoInitGraph =>
            return To_TPString ("(BGI) graphics not installed");
         when grNotDetected =>
            return To_TPString ("Graphics hardware not detected");
         when grFileNotFound =>
            return To_TPString ("device driver file not found ()");
         when grInvalidDriver =>
            return To_TPString ("Invalid device driver file ()");
         when grNoLoadMem =>
            return To_TPString ("Not enough memory to load driver");
         when grNoScanMem =>
            return To_TPString ("Out of memory in scan fill");
         when grNoFloodMem =>
            return To_TPString ("Out of memory in flood fill");
         when grFontNotFound =>
            return To_TPString ("Font file not found ()");
         when grNoFontMem =>
            return To_TPString ("Not enough memory to load font");
         when grInvalidMode =>
            return To_TPString ("Invalid graphics mode for selected driver");
         when grError =>
            return To_TPString ("Graphics error");   -- generic error
         when grIOerror =>
            return To_TPString ("Graphics I/O error");
         when grInvalidFont =>
            return To_TPString ("Invalid font file ()");
         when grInvalidFontNum =>
            return To_TPString ("Invalid font number");
         when grInvalidVersion =>
            return To_TPString ("Invalid version");
         when others =>
            return To_TPString ("Graphics error (") & ErrorCode'Img & ')';
      end case;
   end GraphErrorMsg;

   function GraphResult return Integer is
      LGraphResult : constant Integer := IntGraphResult;
   begin
      IntGraphResult := grOk;
      return LGraphResult;
   end GraphResult;

   -- *** detection, initialization and crt mode routines ***
   procedure DetectGraph (GraphDriver, GraphMode : out Integer) is
   begin
      GraphDriver    := VGA;
      GraphMode      := VGAHi;
      IntGraphResult := grOk;
   end DetectGraph;

   function GetDriverName return String is
   begin
      return To_TPString ("EGAVGA");
   end GetDriverName;

   procedure On_Mouse_Null_Event
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event :        Gnoga.Gui.Base.Mouse_Event_Record) is null;

   procedure InitGraph
     (GraphDriver  : in out Integer;
      GraphMode    : in out Integer;
      PathToDriver :        String)
   is
      pragma Unreferenced (PathToDriver);
      use type Gnoga.Gui.Element.Canvas.Canvas_Access;
      use type Gnoga.Gui.Base.Mouse_Event;
      MEH : Gnoga.Gui.Base.Mouse_Event := null;
   begin
      GraphGetMemPtr  := nil;
      GraphFreeMemPtr := nil;
      GraphDriver     := VGA;
      GraphMode       := VGAHi;
      IntGraphResult  := grError;
      if Area_Canvas = null then
         Area_Canvas := new Gnoga.Gui.Element.Canvas.Canvas_Type;
         Area_Canvas.Create (TP7.Get_Graph_View.all, 640, 480);
         TP7.Set_Graph_Canvas (Area_Canvas);
         Area_Canvas.Border;
         Cr.Get_Drawing_Context_2D (Area_Canvas.all);
         TP7.Get_Mouse_Event (MEH);
         if MEH /= null then
            TP7.Get_Graph_View.On_Mouse_Move_Handler (MEH);
            TP7.Get_Graph_View.On_Mouse_Right_Click_Handler (On_Mouse_Null_Event'Access);
            TP7.Get_Graph_View.On_Mouse_Double_Click_Handler (MEH);
            TP7.Get_Graph_View.On_Mouse_Down_Handler (MEH);
            TP7.Get_Graph_View.On_Mouse_Up_Handler (MEH);
         end if;
         -- Workaround save context with no clip defined as resetClip isn't support
         Cr.Save;
      end if;
      --CreatePalette;
      GraphDefaults;
      ClearDevice;
      IntGraphResult := grOk;
   end InitGraph;

   function RegisterBGIfont (Font : Pointer) return Integer is
      pragma Unreferenced (Font);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction RegisterBGIfont n'est pas définie !");
      end if;
      IntGraphResult := grInvalidFont;
      return 0;
   end RegisterBGIfont;

   function RegisterBGIdriver (Driver : Pointer) return Integer is
      pragma Unreferenced (Driver);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction RegisterBGIdriver n'est pas définie !");
      end if;
      IntGraphResult := grInvalidDriver;
      return 0;
   end RegisterBGIdriver;

   function InstallUserDriver (DriverFileName : String; AutoDetectPtr : Pointer) return Integer is
      pragma Unreferenced (AutoDetectPtr, DriverFileName);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction InstallUserDriver n'est pas définie !");
      end if;
      IntGraphResult := grInvalidDriver;
      return VGA;
   end InstallUserDriver;

   function InstallUserFont (FontFileName : String) return Integer is
      pragma Unreferenced (FontFileName);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction InstallUserFont n'est pas définie !");
      end if;
      IntGraphResult := grInvalidFont;
      return DefaultFont;
   end InstallUserFont;

   procedure SetGraphBufSize (BufSize : Word) is
      pragma Unreferenced (BufSize);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetGraphBufSize n'est pas définie !");
      end if;
      IntGraphResult := grError;
   end SetGraphBufSize;

   function GetMaxMode return Integer is
   begin
      return VGAHi;
   end GetMaxMode;

   procedure GetModeRange (GraphDriver : Integer; LoMode, HiMode : out Integer) is
   begin
      if (GraphDriver = VGA) or (GraphDriver = CurrentDriver) then
         LoMode := VGAHi;
         HiMode := VGAHi;
      else
         LoMode := -1;
         HiMode := -1;
      end if;
   end GetModeRange;

   function GetModeName (GraphMode : Integer) return String is
   begin
      case GraphMode is
         when VGAHi =>
            return To_TPString ("640 x 480 VGA");
         when others =>
            return To_TPString ("Mode error");
      end case;
   end GetModeName;

   procedure SetGraphMode (Mode : Integer) is
      pragma Unreferenced (Mode);
   begin
      GraphDefaults;
      ClearDevice;
      IntGraphResult := grOk;
   end SetGraphMode;

   function GetGraphMode return Integer is
   begin
      IntGraphResult := grOk;
      return VGAHi;
   end GetGraphMode;

   procedure GraphDefaults is
   begin
      IntX := 0;
      IntY := 0;
      SetRect (IntMaxRect, 0, 0, 639, 479);
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      GetDefaultPalette (IntPalette);
      SetColor (White);
      SetBkColor (Black);
      SetLineStyle (SolidLn, 0, NormWidth);
      SetFillStyle (SolidFill, GetMaxColor);
      for Ind in 1 .. 8 loop
         IntFillPat (Ind) := 16#FF#;
      end loop;
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetTextJustify (LeftText, TopText);
      SetWriteMode (CopyPut);
      IntArcCoords.X      := 0;
      IntArcCoords.Y      := 0;
      IntArcCoords.XStart := 0;
      IntArcCoords.YStart := 0;
      IntArcCoords.XEnd   := 0;
      IntArcCoords.YEnd   := 0;
   end GraphDefaults;

   procedure RestoreCrtMode is
   begin
      null;
   end RestoreCrtMode;

   procedure CloseGraph is
   begin
      IntGraphResult := grOk;
   end CloseGraph;

   function GetX return Integer is
   begin
      return IntX;
   end GetX;

   function GetY return Integer is
   begin
      return IntY;
   end GetY;

   function GetMaxX return Integer is
   begin
      return IntMaxRect.Right - IntMaxRect.Left;
   end GetMaxX;

   function GetMaxY return Integer is
   begin
      return IntMaxRect.Bottom - IntMaxRect.Top;
   end GetMaxY;

   -- *** Screen, viewport, page routines ***
   procedure ClearDevice is
   begin
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      Cr.Clear_Rectangle ((0, 0, GetMaxX, GetMaxY));
   end ClearDevice;

   procedure SetViewPort (X1, Y1, X2, Y2 : Integer; Clip : Boolean) is
   begin
      -- Get back to the origin and reset clip
      Cr.Restore;
      -- Push again the context with no clip defined
      Cr.Save;
      -- Restore current settings
      Cr.Stroke_Color (IntColorPalette.Colors (IntColor));
      Cr.Fill_Color (IntColorPalette.Colors (IntFillInfo.Color));
      -- Set viewport
      IntViewPort.X1   := X1;
      IntViewPort.Y1   := Y1;
      IntViewPort.X2   := X2;
      IntViewPort.Y2   := Y2;
      IntViewPort.Clip := Clip;
      IntX             := 0;
      IntY             := 0;
      Cr.Translate (X1, Y1);
      if Clip = ClipOn then
         Cr.Begin_Path;
         Cr.Rectangle ((0, 0, X2 - X1, Y2 - Y1));
         Cr.Clip;
      end if;
   end SetViewPort;

   procedure GetViewSettings (ViewPort : out ViewPortType) is
   begin
      ViewPort := IntViewPort;
   end GetViewSettings;

   procedure ClearViewPort is
   begin
      Cr.Clear_Rectangle
      ((0, 0, IntViewPort.X2 - IntViewPort.X1, IntViewPort.Y2 - IntViewPort.Y1));
      IntGraphResult := grOk;
   end ClearViewPort;

   procedure SetVisualPage (Page : Word) is
      pragma Unreferenced (Page);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetVisualPage n'est pas définie !");
      end if;
   end SetVisualPage;

   procedure SetActivePage (Page : Word) is
      pragma Unreferenced (Page);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetActivePage n'est pas définie !");
      end if;
   end SetActivePage;

   -- *** point-oriented routines ***
   procedure PutPixel (X, Y : Integer; Pixel : Word) is
   begin
      if Pixel <= IntPalette.Size - 1 then
         -- Pixel requires absolute coordinates
         Cr.Pixel
         (X + IntViewPort.X1, Y + IntViewPort.Y1, To_Pixel (IntColorPalette.Colors (Pixel)));
      end if;
   end PutPixel;

   function GetPixel (X, Y : Integer) return Word is
   begin
      -- Pixel requires absolute coordinates
      return Word
          (GetIndColor (Gnoga.Types.To_RGBA (Cr.Pixel (X + IntViewPort.X1, Y + IntViewPort.Y1))));
   end GetPixel;

   -- *** line-oriented routines ***
   procedure SetWriteMode (WriteMode : Integer) is
   begin
      case WriteMode is
         when CopyPut =>
--              Cr.Glogal_Composite_Operation (Gnoga.Gui.Element.Canvas.Context_2D.Source_Over);
            IntOperator    := Gnoga.Gui.Element.Canvas.Context_2D.Source_Over;
            IntGraphResult := grOk;
         when XORPut =>
--              Cr.Glogal_Composite_Operation (Gnoga.Gui.Element.Canvas.Context_2D.Xor_Copy);
            IntOperator    := Gnoga.Gui.Element.Canvas.Context_2D.Xor_Copy;
            IntGraphResult := grOk;
         --  TODO : and, or, not
         when others =>
            IntGraphResult := grError;
      end case;
   end SetWriteMode;

   procedure LineTo (X, Y : Integer) is
   begin
      Cr.Begin_Path;
      Cr.Move_To (IntX, IntY);
      IntX := X;
      IntY := Y;
      Cr.Line_To (IntX, IntY);
      Cr.Stroke;
   end LineTo;

   procedure LineRel (Dx, Dy : Integer) is
   begin
      Cr.Begin_Path;
      Cr.Move_To (IntX, IntY);
      IntX := IntX + Dx;
      IntY := IntY + Dy;
      Cr.Line_To (IntX, IntY);
      Cr.Stroke;
   end LineRel;

   procedure MoveTo (X, Y : Integer) is
   begin
      IntX := X;
      IntY := Y;
   end MoveTo;

   procedure MoveRel (Dx, Dy : Integer) is
   begin
      IntX := IntX + Dx;
      IntY := IntY + Dy;
   end MoveRel;

   procedure Line (X1, Y1, X2, Y2 : Integer) is
   begin
      Cr.Begin_Path;
      Cr.Move_To (X1, Y1);
      Cr.Line_To (X2, Y2);
      Cr.Stroke;
   end Line;

   procedure GetLineSettings (LineInfo : out LineSettingsType) is
   begin
      LineInfo := IntLineInfo;
   end GetLineSettings;

   procedure SetLineStyle (LineStyle : Word; Pattern : Word; Thickness : Word) is
      use Gnoga.Gui.Element.Canvas.Context_2D;
      Ind : Integer := 15;
      function User_Array (Val : Boolean) return Dash_Array_Type is
         Len : Natural := 0;
      begin
         while Ind >= 0 and then (Pattern / 2**Ind) mod 2 = Boolean'Pos (Val) loop
            Len := Len + 1;
            Ind := Ind - 1;
         end loop;
         if Ind < 0 then
            return (1 => Len);
         else
            return Len & User_Array (not Val);
         end if;
      end User_Array;
      function Normalize (Line : Dash_Array_Type) return Dash_Array_Type is
      -- we avoid beginning with a null first On value and only one value
      begin
         -- if fisrt bit is 0 we have a first On null value
         if (Pattern / 2**15) mod 2 = 0 then
            -- if last bit is 0 we move and add the first value with the last
            if Pattern mod 2 = 0 then
               return Line (Line'First + 2 .. Line'Last - 1) &
                 (Line (Line'Last) + Line (Line'First + 1));
            -- otherwise we move the second value at the end
            else
               return Line (Line'First + 2 .. Line'Last) & Line (Line'First + 1);
            end if;
         -- if there are only ones then set no dashes
         elsif Line'Length = 1 then
            return Empty_Dash_List;
         end if;
         return Line;
      end Normalize;
   begin
      IntLineInfo.LineStyle := LineStyle;
      IntLineInfo.Pattern   := Pattern;
      IntLineInfo.Thickness := Thickness;
      Cr.Line_Width (Thickness);
      case LineStyle is
         when SolidLn =>
            Cr.Set_Line_Dash (Empty_Dash_List);
            IntGraphResult := grOk;
         when DottedLn =>
            Cr.Set_Line_Dash (Dotted_Dash_List);
            IntGraphResult := grOk;
         when CenterLn =>
            Cr.Set_Line_Dash (Center_Dash_List);
            IntGraphResult := grOk;
         when DashedLn =>
            Cr.Set_Line_Dash (Dashed_Dash_List);
            IntGraphResult := grOk;
         when UserBitLn =>
            if Pattern /= 0 then
               Cr.Set_Line_Dash (Normalize (User_Array (True)));
               IntGraphResult := grOk;
            else
               IntGraphResult := grError;
            end if;
         when others =>
            IntGraphResult := grError;
      end case;
   end SetLineStyle;

   -- *** polygon, fills and figures ***
   procedure Rectangle (X1, Y1, X2, Y2 : Integer) is
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X1, Y => Y1, Width => X2 - X1, Height => Y2 - Y1);
   begin
      Cr.Begin_Path;
      Cr.Rectangle (R);
      Cr.Stroke;
   end Rectangle;

   procedure Bar (X1, Y1, X2, Y2 : Integer) is
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X1, Y => Y1, Width => X2 - X1, Height => Y2 - Y1);
   begin
      N1_Mutex.Seize;
      Cr.Begin_Path;
      Cr.Rectangle (R);
      Cr.Fill;
      N1_Mutex.Release;
      IntGraphResult := grOk;
   end Bar;

   procedure Bar3D (X1, Y1, X2, Y2 : Integer; Depth : Word; Top : Boolean) is
      X : constant Integer                    := GetX;
      Y : constant Integer                    := GetY;
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X1, Y => Y1, Width => X2 - X1, Height => Y2 - Y1);
   begin
      Cr.Begin_Path;
      Cr.Rectangle (R);
      Cr.Fill;
      Cr.Move_To (X2, Y2);
      Cr.Line_To (X1, Y2);
      Cr.Line_To (X1, Y2);
      Cr.Line_To (X1, Y1);
      Cr.Line_To (X2, Y1);
      Cr.Line_To (X2, Y2);
      Cr.Line_To (X2 + Depth, Y2 - Depth);
      Cr.Line_To (X2 + Depth, Y1 - Depth);
      if Top = TopOn then
         Cr.Line_To (X1 + Depth, Y1 - Depth);
         Cr.Line_To (X1, Y1);
         Cr.Line_To (X2, Y1);
         Cr.Line_To (X2 + Depth, Y1 - Depth);
      end if;
      Cr.Move_To (X, Y);
      Cr.Stroke;
      IntGraphResult := grOk;
   end Bar3D;

   procedure DrawPoly (NumPoints : Word; PolyPoints : PolygonType) is
   begin
      Cr.Begin_Path;
      Cr.Move_To (PolyPoints (1).X, PolyPoints (1).Y);
      for Ind in 2 .. Positive (NumPoints) loop
         Cr.Line_To (PolyPoints (Ind).X, PolyPoints (Ind).Y);
      end loop;
      Cr.Stroke;
      IntGraphResult := grOk;
   end DrawPoly;

   procedure FillPoly (NumPoints : Word; PolyPoints : PolygonType) is
   begin
      Cr.Begin_Path;
      Cr.Move_To (PolyPoints (1).X, PolyPoints (1).Y);
      for Ind in 2 .. Positive (NumPoints) loop
         Cr.Line_To (PolyPoints (Ind).X, PolyPoints (Ind).Y);
      end loop;
      Cr.Fill;
      Cr.Stroke;
      IntGraphResult := grOk;
   end FillPoly;

   procedure GetFillSettings (FillInfo : out FillSettingsType) is
   begin
      FillInfo := IntFillInfo;
   end GetFillSettings;

   procedure GetFillPattern (FillPattern : out FillPatternType) is
   begin
      FillPattern := IntFillPat;
   end GetFillPattern;

   procedure SetFillStyle (Pattern : Word; Color : Word) is
   begin
      IntFillInfo.Pattern := Pattern;
      IntFillInfo.Color   := Color;
      Cr.Fill_Color (IntColorPalette.Colors (Color));
      IntGraphResult := grError;
      if Debug then
         TP7.System.Writeln ("La fonction SetFillStyle n'est encore pas définie !");
      end if;
   end SetFillStyle;

   procedure SetFillPattern (Pattern : FillPatternType; Color : Word) is
   begin
      IntFillPat          := Pattern;
      IntFillInfo.Pattern := UserFill;
      IntFillInfo.Color   := Color;
      Cr.Fill_Color (IntColorPalette.Colors (Color));
      IntGraphResult := grError;
      if Debug then
         TP7.System.Writeln ("La fonction SetFillPattern n'est encore pas définie !");
      end if;
   end SetFillPattern;

   procedure FloodFill (X, Y : Integer; Border : Word) is
      pragma Unreferenced (Border, Y, X);
   -- /!\ vérifier que la taille du tas est suffisante ...
   --        R : aliased Rect;
   --      PR : RectPtr;
   --        Words, Height : Integer;
   --      BM,  Mask   : aliased BitMap;
   --      DrawPort,  IntPort  : aliased CGrafPtr;
   --        Pat : aliased Pattern := GetIntPat;
   begin
      --      DrawPort := GetWindowPort(IntCWind);
      --      HideCursor;
      --
      --      PR := GetPortBounds(DrawPort, R'access);
      --        Words  := ((R.Right - R.Left) + 15) / 16;
      --        Height := R.Bottom - R.Top;
      --        SetRect(R'access, 0, 0, words * 16, height);
      --        Mask.bounds := R;
      --        Mask.rowBytes := (((Mask.bounds.right - Mask.bounds.left) +
      --15)   /   16) * 2;
      --        Mask.baseAddr := NewPtr(Long_Integer(Mask.bounds.bottom -
      --Mask.bounds.top) * Long_Integer(Mask.rowBytes) * 24);
      --        if  Mask.BaseAddr = null then
      --                return;
      --          end if;

      --      IntPort := CreateNewPort;
      --      BM := Mask;
      --      BM.BaseAddr := NewPtr(GetPtrSize(Mask.BaseAddr));
      --      if  BM.BaseAddr = null then
      --        return;
      --      end if;
      --SetPortBits(BM'access);
      --      PR := GetPortBounds(IntPort, R'access);
      --FillRect(R'access, Pat'access);
      --      SetPort(DrawPort);
      --      DisposePort(IntPort);

      --PmForeColor(Short_Integer(Border));
      --      PR := GetPortBounds(DrawPort, R'access);
      --SeedCFill(GetPortBitMapForCopyBits(DrawPort), Mask'access, R'access,
      --R'access, X, Y, null, 0);
      --CopyMask(BM'access, mask'access, GetPortBitMapForCopyBits(DrawPort),
      --R'access, R'access, R'access);
      --PmForeColor(Short_Integer(IntColor));
      --      DisposePtr(Mask.BaseAddr);
      --      DisposePtr(BM.BaseAddr);
      --      ShowCursor;
      if Debug then
         TP7.System.Writeln ("La fonction FloodFill n'est pas définie !");
      end if;
   end FloodFill;

   -- *** arc, circle, and other curves ***
   procedure Arc (X, Y : Integer; StAngle, EndAngle, Radius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.XStart := X + Integer (Float (Radius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.YStart := Y - Integer (Float (Radius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.XEnd   := X + Integer (Float (Radius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.YEnd   := Y - Integer (Float (Radius) * Sin (Float (EndAngle) * Pi / 180.0));
      Cr.Begin_Path;
      Cr.Arc_Degrees (X, Y, Radius, Float (360 - EndAngle), Float (360 - StAngle));
      Cr.Stroke;
   end Arc;

   procedure GetArcCoords (ArcCoords : out ArcCoordsType) is
   begin
      ArcCoords := IntArcCoords;
   end GetArcCoords;

   procedure Circle (X, Y : Integer; Radius : Word) is
   begin
      Cr.Begin_Path;
      Cr.Arc_Degrees (X, Y, Radius, 0.0, 360.0);
      Cr.Stroke;
   end Circle;

   procedure Ellipse (X, Y : Integer; StAngle, EndAngle : Word; XRadius, YRadius : Word) is
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X - XRadius, Y => Y - YRadius, Width => XRadius * 2, Height => YRadius * 2);
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.XStart := X + Integer (Float (XRadius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.YStart := Y - Integer (Float (YRadius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.XEnd   := X + Integer (Float (XRadius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.YEnd   := Y - Integer (Float (YRadius) * Sin (Float (EndAngle) * Pi / 180.0));
      Cr.Begin_Path;
      if XRadius = 0 or else YRadius = 0 then
         Cr.Rectangle (R);
      else
         Cr.Save;
         Cr.Translate (X, Y);
         Cr.Scale (Float (XRadius), Float (YRadius));
         Cr.Arc_Degrees (0, 0, 1, Float (360 - EndAngle), Float (360 - StAngle));
         Cr.Restore;
      end if;
      Cr.Stroke;
   end Ellipse;

   procedure FillEllipse (X, Y : Integer; XRadius, YRadius : Word) is
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X - XRadius, Y => Y - YRadius, Width => XRadius * 2, Height => YRadius * 2);
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.XStart := X + Integer (XRadius);
      IntArcCoords.YStart := Y;
      IntArcCoords.XEnd   := X + Integer (XRadius);
      IntArcCoords.YEnd   := Y;
      Cr.Begin_Path;
      if XRadius = 0 or else YRadius = 0 then
         Cr.Rectangle (R);
      else
         Cr.Save;
         Cr.Translate (X, Y);
         Cr.Scale (Float (XRadius), Float (YRadius));
         Cr.Arc_Degrees (0, 0, 1, 0.0, 360.0);
         Cr.Fill;
         Cr.Restore;
      end if;
      Cr.Stroke;
   end FillEllipse;

   procedure GetAspectRatio (XAsp, YAsp : out Word) is
   begin
      XAsp := 1000;
      YAsp := 1000;
   end GetAspectRatio;

   procedure SetAspectRatio (XAsp, YAsp : Word) is
      pragma Unreferenced (YAsp, XAsp);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetAspectRatio n'est pas encore définie !");
      end if;
   end SetAspectRatio;

   procedure PieSlice (X, Y : Integer; StAngle, EndAngle, Radius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.XStart := X + Integer (Float (Radius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.YStart := Y - Integer (Float (Radius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.XEnd   := X + Integer (Float (Radius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.YEnd   := Y - Integer (Float (Radius) * Sin (Float (EndAngle) * Pi / 180.0));
      Cr.Begin_Path;
      Cr.Move_To (X, Y);
      Cr.Arc_Degrees (X, Y, Radius, Float (360 - EndAngle), Float (360 - StAngle));
      Cr.Line_To (X, Y);
      Cr.Fill;
      Cr.Stroke;
      IntGraphResult := grOk;
   end PieSlice;

   procedure Sector (X, Y : Integer; StAngle, EndAngle, XRadius, YRadius : Word) is
      R : constant Gnoga.Types.Rectangle_Type :=
        (X => X - XRadius, Y => Y - YRadius, Width => XRadius * 2, Height => YRadius * 2);
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.XStart := X + Integer (Float (XRadius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.YStart := Y - Integer (Float (YRadius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.XEnd   := X + Integer (Float (XRadius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.YEnd   := Y - Integer (Float (YRadius) * Sin (Float (EndAngle) * Pi / 180.0));
      Cr.Begin_Path;
      if XRadius = 0 or else YRadius = 0 then
         Cr.Rectangle (R);
      else
         Cr.Save;
         Cr.Translate (X, Y);
         Cr.Scale (Float (XRadius), Float (YRadius));
         Cr.Move_To (0, 0);
         Cr.Arc_Degrees (0, 0, 1, Float (360 - EndAngle), Float (360 - StAngle));
         Cr.Line_To (0, 0);
         Cr.Fill;
         Cr.Restore;
      end if;
      Cr.Stroke;
   end Sector;

   -- *** color and palette routines ***
   procedure SetBkColor (ColorNum : Word) is
   begin
      if ColorNum <= IntPalette.Size - 1 then
         IntBkColor := ColorNum;
         Area_Canvas.Background_Color (IntColorPalette.Colors (IntBkColor));
      end if;
   end SetBkColor;

   procedure SetColor (Color : Word) is
   begin
      if Color <= IntColorPalette.Size - 1 then
         IntColor := Color;
         Cr.Stroke_Color (IntColorPalette.Colors (IntColor));
      end if;
   end SetColor;

   function GetBkColor return Word is
   begin
      return IntBkColor;
   end GetBkColor;

   function GetColor return Word is
   begin
      return IntColor;
   end GetColor;

   procedure SetAllPalette (Palette : PaletteType) is
   --DumRGB : aliased rgbcolor;
   begin
      -- Check consitency of incoming palette
      if Palette.Size > MaxColors + 1 then
         IntGraphResult := grError;
         return;
      end if;
      for Ind in 0 .. Palette.Size - 1 loop
         if Palette.Colors (Ind) < -1 or else Palette.Colors (Ind) > Shortint (MaxColors) then
            IntGraphResult := grError;
            return;
         end if;
      end loop;
      -- Modify internal palette
      for Ind in 0 .. Palette.Size - 1 loop
         if Palette.Colors (Ind) /= -1 then
            IntPalette.Colors (Ind) := Palette.Colors (Ind);
            --        DumRGB :=
            --IntCT4.all.all.CtTable(Standard.Integer(Palette.Colors(
            --              Standard.Integer(Ind)))).RGB;
            --        AnimateEntry(IntCWind, Ind, DumRGB'access);
            -- TBF activation
         end if;
      end loop;
      --  ActivatePalette(IntCWind);
      IntGraphResult := grError;
      if Debug then
         TP7.System.Writeln ("La fonction SetAllPalette n'est encore pas définie !");
      end if;
   end SetAllPalette;

   procedure SetPalette (ColorNum : Word; Color : Shortint) is
   --DumRGB : aliased rgbcolor;
   begin
      if ColorNum <= IntPalette.Size - 1
        and then Color <= IntPalette.Size - 1
        and then Color >= 0
      then
         IntPalette.Colors (ColorNum) := Color;
      --        DumRGB :=
      --IntCT4.all.all.CtTable(Standard.Integer(Color)).RGB;
      --        AnimateEntry(IntCWind, Integer(ColorNum), DumRGB'access);
      --        ActivatePalette(IntCWind);
      -- TBF activation
      else
         IntGraphResult := grError;
      end if;
      IntGraphResult := grError;
      if Debug then
         TP7.System.Writeln ("La fonction SetPalette n'est encore pas définie !");
      end if;
   end SetPalette;

   procedure GetPalette (Palette : out PaletteType) is
   begin
      Palette := IntPalette;
   end GetPalette;

   function GetPaletteSize return Integer is
   begin
      return IntPalette.Size;
   end GetPaletteSize;

   procedure GetDefaultPalette (Palette : out PaletteType) is
   begin
      Palette.Size := MaxColors + 1;
      for Ind in 0 .. MaxColors loop
         Palette.Colors (Ind) := Ind;
      end loop;
   end GetDefaultPalette;

   function GetMaxColor return Word is
   begin
      return IntPalette.Size - 1;
   end GetMaxColor;

   procedure SetRGBPalette (ColorNum, RedValue, GreenValue, BlueValue : Integer) is
   --CRGB : aliased rgbcolor;
   begin
      if ColorNum <= IntPalette.Size - 1 then
         --        CRGB.Red := Word(RedValue);
         --        CRGB.Green := Word(GreenValue);
         --        CRGB.Blue := Word(BlueValue);
         --        AnimateEntry(IntCWind, ColorNum, CRGB'access);
         --        ActivatePalette(IntCWind);
         --           Gdk.Threads.Enter;
         IntColorPalette.Colors (Word (ColorNum)) :=
           Gnoga.Types.To_RGBA
             ("rgb(" & RedValue'Img & ',' & GreenValue'Img & ',' & BlueValue'Img & ')');
         -- TBF activation
         --           Gdk.Threads.Leave;
      end if;
      if Debug then
         TP7.System.Writeln ("La fonction SetRGBPalette n'est encore pas définie !");
      end if;
   end SetRGBPalette;

   -- *** bit-image routines ***
   function ImageSize (X1, Y1, X2, Y2 : Integer) return Longint is
   begin
      return (X2 - X1 + 1) * (Y2 - Y1 + 1);
   end ImageSize;

   procedure FreeMem (P : in out BitMapType; Taille : Word) is
      pragma Unreferenced (Taille);
      procedure Free is new Ada.Unchecked_Deallocation
        (Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type,
         Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access);
   begin
      Free (Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access (P));
   end FreeMem;

   procedure GetMem (P : out BitMapType; Taille : Word) is
      pragma Unreferenced (Taille);
   begin
      if P = null then
         P := BitMapType'(new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type);
      end if;
   end GetMem;

   procedure GetImage (X1, Y1, X2, Y2 : Integer; BitMap : in out BitMapType) is
   begin
      if BitMap = null then
         BitMap := BitMapType'(new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type);
      end if;
      -- Get_Image_Data requires absolute coordinates
      Cr.Get_Image_Data
      (BitMap.all, X1 + IntViewPort.X1, Y1 + IntViewPort.Y1, X2 - X1 + 1, Y2 - Y1 + 1);
   end GetImage;

   procedure PutImage (X, Y : Integer; BitMap : BitMapType; BitBlt : Word) is
      pragma Unreferenced (BitBlt);
   begin
      --        case BitBlt is
      --           when CopyPut =>
      --              MyBitBlt := 0; --SrcCopy;
      --           when XORPut =>
      --              MyBitBlt := 0; --SrcXor;
      --           when OrPut =>
      --              MyBitBlt := 0; --SrcOr;
      --           when AndPut =>
      --              MyBitBlt := 0; --SrcBic;
      --           when NotPut =>
      --              MyBitBlt := 0; --NotSrcCopy;
      --           when others =>
      --              null;
      --        end case;
--        case BitBlt is
--           when CopyPut =>
--              Cr.Glogal_Composite_Operation (Gnoga.Gui.Element.Canvas.Context_2D.Copy);
--           when XORPut =>
--              Cr.Glogal_Composite_Operation (Gnoga.Gui.Element.Canvas.Context_2D.Xor_Copy);
--           when others =>
--              IntGraphResult := grError;
--        end case;
      -- Put_Image_Data requires absolute coordinates
      Cr.Put_Image_Data (BitMap.all, X + IntViewPort.X1, Y + IntViewPort.Y1);
   end PutImage;

   -- *** text routines ***
   procedure GetTextSettings (TextInfo : out TextSettingsType) is
   begin
      TextInfo := IntTextInfo;
   end GetTextSettings;

   procedure DrawHChar (X, Y : Integer; Ch : Byte) is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null and then Ch in Font.CharCmds'Range then
         for Ind in Font.CharCmds (Ch)'Range loop
            case Font.CharCmds (Ch) (Ind).Cmd is
               when FontCHR.Move =>
                  Cr.Move_To
                  (X +
                   Integer (Float (Font.CharCmds (Ch) (Ind).X) * CoeffX), Y -
                   Integer (Float (Font.CharCmds (Ch) (Ind).Y) * CoeffY));
               when FontCHR.Line =>
                  Cr.Line_To
                  (X +
                   Integer (Float (Font.CharCmds (Ch) (Ind).X) * CoeffX), Y -
                   Integer (Float (Font.CharCmds (Ch) (Ind).Y) * CoeffY));
            end case;
         end loop;
      end if;
   end DrawHChar;

   procedure DrawVChar (X, Y : Integer; Ch : Byte) is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null and then Ch in Font.CharCmds'Range then
         for Ind in Font.CharCmds (Ch)'Range loop
            case Font.CharCmds (Ch) (Ind).Cmd is
               when FontCHR.Move =>
                  Cr.Move_To
                  (X -
                   Integer (Float (Font.CharCmds (Ch) (Ind).Y) * CoeffY), Y -
                   Integer (Float (Font.CharCmds (Ch) (Ind).X) * CoeffX));
               when FontCHR.Line =>
                  Cr.Line_To
                  (X -
                   Integer (Float (Font.CharCmds (Ch) (Ind).Y) * CoeffY), Y -
                   Integer (Float (Font.CharCmds (Ch) (Ind).X) * CoeffX));
            end case;
         end loop;
      end if;
   end DrawVChar;

   function CharWidth (Ch : Byte) return Word is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null and then Ch in Font.CharWidths'Range then
         return Font.CharWidths (Ch);
      else
         return 0;
      end if;
   end CharWidth;

   function CharHeight return Word is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null then
         return Font.AscenderLine - Font.DescenderLine;
      else
         return 0;
      end if;
   end CharHeight;

   function CharAscend return Shortint is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null then
         return Font.AscenderLine;
      else
         return 0;
      end if;
   end CharAscend;

   function CharDescend return Shortint is
      Font : constant FontCHR.DescFont := FontCHR.GetFont (IntTextInfo.Font);
      use type FontCHR.DescFont;
   begin
      if Font /= null then
         return Font.DescenderLine;
      else
         return 0;
      end if;
   end CharDescend;

   procedure DrawHText (X, Y : Integer; TextString : String) is
      X1 : Integer := X;
      Y1 : Integer := Y;
   begin
      case IntTextInfo.Horiz is
         when LeftText =>
            null;
         when CenterText =>
            X1 := X1 - TextWidth (TextString) / 2;
         when RightText =>
            X1 := X1 - TextWidth (TextString);
         when others =>
            null;
      end case;
      case IntTextInfo.Vert is
         when BottomText =>
            Y1 := Y1 + Integer (Float (CharDescend) * CoeffY);
         when CenterText =>
            Y1 := Y1 + Integer (Float (CharHeight) / 2.0 + Float (CharDescend) * CoeffY);
         when TopText =>
            Y1 := Y1 + Integer (Float (CharAscend) * CoeffY);
         when others =>
            null;
      end case;
      Cr.Save;
      -- Set standard line setting for character drawing
      Cr.Set_Line_Dash (Gnoga.Gui.Element.Canvas.Context_2D.Empty_Dash_List);
      Cr.Line_Width (1);
      Cr.Begin_Path;
      for ch in 1 .. TP7.System.Length (TextString) loop
         DrawHChar (X1, Y1, FontCHR.To_CodePage437 (TP7.System.Ord (TextString (ch))));
         X1 :=
           X1 +
           Integer
             (Float (CharWidth (FontCHR.To_CodePage437 (TP7.System.Ord (TextString (ch))))) *
              CoeffX);
      end loop;
      Cr.Stroke;
      Cr.Restore;
   end DrawHText;

   procedure DrawVText (X, Y : Integer; TextString : String) is
      X1 : Integer := X;
      Y1 : Integer := Y;
   begin
      case IntTextInfo.Horiz is
         when LeftText =>
            X1 := X1 - Integer (Float (CharDescend) * CoeffX);
         when CenterText =>
            X1 := X1 + Integer (Float (CharHeight) / 2.0 + Float (CharDescend) * CoeffX);
         when RightText =>
            X1 := X1 + Integer (Float (CharAscend) * CoeffX);
         when others =>
            null;
      end case;
      case IntTextInfo.Vert is
         when BottomText =>
            null;
         when CenterText =>
            Y1 := Y1 + TextWidth (TextString) / 2;
         when TopText =>
            Y1 := Y1 + TextWidth (TextString);
         when others =>
            null;
      end case;
      Cr.Save;
      -- Set standard line setting for character drawing
      Cr.Set_Line_Dash (Gnoga.Gui.Element.Canvas.Context_2D.Empty_Dash_List);
      Cr.Line_Width (1);
      Cr.Begin_Path;
      for ch in 1 .. TP7.System.Length (TextString) loop
         DrawVChar (X1, Y1, FontCHR.To_CodePage437 (TP7.System.Ord (TextString (ch))));
         Y1 :=
           Y1 -
           Integer
             (Float (CharWidth (FontCHR.To_CodePage437 (TP7.System.Ord (TextString (ch))))) *
              CoeffX);
      end loop;
      Cr.Stroke;
      Cr.Restore;
   end DrawVText;

   procedure OutText (TextString : String) is
   begin
      N1_Mutex.Seize;
      case IntTextInfo.Direction is
         when HorizDir =>
            DrawHText (IntX, IntY, TextString);
         when VertDir =>
            DrawVText (IntX, IntY, TextString);
         when others =>
            null;
      end case;
      if (IntTextInfo.Horiz = CenterText) or (IntTextInfo.Horiz = RightText) then
         IntY := IntY + TextHeight (TextString);
      end if;
      if IntTextInfo.Horiz = LeftText then
         IntX := IntX + TextWidth (TextString);
      end if;
      N1_Mutex.Release;
   end OutText;

   procedure OutTextXY (X, Y : Integer; TextString : String) is
   begin
      N1_Mutex.Seize;
      case IntTextInfo.Direction is
         when HorizDir =>
            DrawHText (X, Y, TextString);
         when VertDir =>
            DrawVText (X, Y, TextString);
         when others =>
            null;
      end case;
      N1_Mutex.Release;
   end OutTextXY;

   procedure SetTextJustify (Horiz, Vert : Word) is
   begin
      case Horiz is
         when LeftText | CenterText | RightText =>
            IntTextInfo.Horiz := Horiz;
            IntGraphResult    := grOk;
         when others =>
            IntGraphResult := grError;
      end case;
      case Vert is
         when BottomText | CenterText | TopText =>
            IntTextInfo.Vert := Vert;
            IntGraphResult   := grOk;
         when others =>
            IntGraphResult := grError;
      end case;
   end SetTextJustify;

   procedure SetTextStyle (Font, Direction : Word; CharSize : Word) is
   begin
      IntTextInfo.Font      := Font;
      IntTextInfo.Direction := Direction;
      IntTextInfo.CharSize  := CharSize;
      if CharSize >= 1 and then CharSize <= 10 then
         CoeffX         := Float (CharSize) * Float (CCharSize) / Float (CharHeight);
         CoeffY         := Float (CharSize) * Float (CCharSize) / Float (CharHeight);
         IntGraphResult := grOk;
      else
         IntGraphResult := grError;
      end if;
   end SetTextStyle;

   procedure SetUserCharSize (MultX, DivX, MultY, DivY : Word) is
   begin
      IntTextInfo.CharSize := UserCharSize;
      CoeffX               := Float (MultX) / Float (DivX);
      CoeffY               := Float (MultY) / Float (DivY);
   end SetUserCharSize;

   function TextHeight (TextString : String) return Word is
      pragma Unreferenced (TextString);
   begin
      return Word (Float (CharHeight) * CoeffY);
   end TextHeight;

   function TextWidth (TextString : String) return Word is
      Width : Word := 0;
   begin
      for Ind in 1 .. TP7.System.Length (TextString) loop
         Width := Width + CharWidth (FontCHR.To_CodePage437 (TP7.System.Ord (TextString (Ind))));
      end loop;
      return Word (Float (Width) * CoeffX);
   end TextWidth;

end TP7.Graph;
