-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-mouse.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 10.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 22 décembre 2014
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
-- COPYRIGHT                        : (c) Pascal Pignard 1988-2015
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------
-- Based on DOS mouse unit from Soft et Micro
-- Translated on 26-Mar-2012 by (New) P2Ada v. 28-Oct-2009

package TP7.Mouse is
   subtype Integer is TPInteger;

   -- Button number
   LeftButton   : constant := 1;
   MiddleButton : constant := 2;
   RightButton  : constant := 3;

   -- Button status
   LeftButtonActive   : constant := 1;
   MiddleButtonActive : constant := 2;
   RightButtonActive  : constant := 4;

   -- GTK scroll values
   NoScroll      : constant := 0;
   ScrollUp      : constant := 1;
   ScrollDown    : constant := 2;
   ScrollLeft    : constant := 3;
   ScrollRight   : constant := 4;
   Scroll_Smooth : constant := 5;

   -- Standard cursor sets to CSS auto cursor style
   -- http://www.w3.org/TR/css3-ui - 7.1.1. ‘cursor’ property
   StandardCursor : constant := 0;
   FirstCursor    : constant := 0;
   LastCursor     : constant := 33;

   -- Reset mouse (hidden by default) and give the its installed status
   function MouseInit return Boolean;
   -- Returns the number of buttons
   function NbrButton return Integer;
   -- Increments the internal cursor counter.
   -- If the counter is equal or greater than zero, the cursor is enabled and appears.
   procedure ShowMouse;
   -- Decrements the internal cursor counter.
   -- If the counter is negative, the cursor is disabled and desappears.
   procedure HideMouse;
   -- Return horizontal postion relative to current window
   function GetXPos return Integer;
   -- Return vertical postion relative to current window
   function GetYPos return Integer;
   -- Return button status number
   -- bit 0 active when left button is pressed
   -- bit 1 active when middle button is pressed
   -- bit 2 active when right button is pressed
   function GetStatus return Integer;
   function NumButton return Integer renames GetStatus;
   -- Return scroll postion (value is reset after call)
   function GetScroll return Integer;
   -- Sets the cursor location
   procedure MouseNewPosition (NouvX, NouvY : Integer);
   -- Return if the specified button has been pressed (value is reset after call)
   function ButtonPress (Button : Integer) return Boolean;
   -- Return if the specified button has been pressed twice (value is reset after call)
   function ButtonDoublePress (Button : Integer) return Boolean;
   -- Return if the specified button has been pressed 3 times (value is reset after call)
   function ButtonTriplePress (Button : Integer) return Boolean;
   -- Return the number of press of specified button since last call
   function CountPress (Button : Integer) return Integer;
   -- Return the horizontal position at last specified button press
   function LastXPress (Button : Integer) return Integer;
   -- Return the vertical position at last specified button press
   function LastYPress (Button : Integer) return Integer;
   -- Return if the specified button has been released (value is reset after call)
   function ButtonRelease (Button : Integer) return Boolean;
   -- Return the number of release of specified button since last call
   function CountRelease (Button : Integer) return Integer;
   -- Return the horizontal position at last specified button release
   function LastXRelease (Button : Integer) return Integer;
   -- Return the vertical position at last specified button release
   function LastYRelease (Button : Integer) return Integer;
   -- Define horizontal and vertical range of cursor location
   procedure MouseWindow (MinX, MinY, MaxX, MaxY : Integer);
   -- Define graphics mode cursor CSS style from auto (0) to zoom-out (33)
   -- http://www.w3.org/TR/css3-ui - 9.1.1. ‘cursor’ property
   procedure MouseSetGraphBlock (Cursor : Integer);
   -- Define graphics mode cursor style with XPM string pixmap definition
   procedure MouseSetGraphBlock
     (Source             : String;
      Mask               : String;
      FGColor, BGColor   : Integer;
      HotSpotX, HotSpotY : Integer);
   -- Define text mode cursor style
   procedure MouseSetTextBlock (MType, MScreen, MCursor : Integer);
   -- Return mouse horizontal motion number (positive : right move, negative : left move)
   function MouseXMotionCount return Integer;
   -- Return mouse vertical motion number (positive : down move, negative : up move)
   function MouseYMotionCount return Integer;
   -- Define mouse sensitivity motion/pixel
   procedure MousePixelRatio (MXRatio, MYRatio : Integer);

end TP7.Mouse;
