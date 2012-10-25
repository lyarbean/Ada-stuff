with ada.text_io;
package arc4 is
   pragma Elaborate_Body;
   type uchar is mod 2**8;
   package uchar_io is new ada.text_io.modular_io(uchar);
   type characters is array(natural range <>) of uchar;
   -- fix me, use flexable length
   type table is array (uchar range <>) of uchar;
   type context is record
      x,y: uchar;
      m: table(0..255);
   end record;
   procedure setup(c: in out context; key: in characters);
   procedure crypt(c: in out context;
      ibuf: in characters;
      obuf: in out characters);
end arc4;
