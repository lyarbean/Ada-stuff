pragma optimize(time);
pragma optimize_alignment(time);
package body arc4 is
   procedure setup(c: in out context;
      key: in characters) is
      a, j: uchar := 0;
      k: natural := 0;
   begin
      k := 0;
      j := 0;
      c.x := 0;
      c.y := 0;
      for idx in c.m'range loop
         c.m(idx) := idx;
      end loop;

      for idx in c.m'range loop
         if k >= key'length then
            k := 0;
         end if;
         a := c.m(idx);
         j := j + a + key(k);
         c.m(idx) := c.m(j); 
         c.m(j) := a;
         k := k+1;
      end loop; 
   end setup;

   procedure crypt(c: in out context;
      ibuf: in characters;
      obuf: in out characters) is
      a,b,x,y: uchar := 0;
   begin
      x := c.x;
      y := c.y;
      for i in ibuf'range loop
         x := x + 1;
         a := c.m(x);
         y := y + a;
         b := c.m(y);
         c.m(x) := b;
         c.m(y) := a;
         obuf(i) := ibuf(i) xor c.m(a+b);
      end loop;
      c.x := x;
      c.y := y;
   end crypt;
end arc4;
