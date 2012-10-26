with ada.unchecked_deallocation;
with ada.characters.handling;
with ada.text_io;
package body bignum is
   use interfaces;
   mul_base_digits : constant := 80;
   slot_size : constant := 2_000_000_000;
   hex_string : string(1..16) := "0123456789ABCDEF";
   procedure free is new ada.unchecked_deallocation(object=>limbs,name=>limbs_access);
   -- storage   
   procedure initialize(n : in out mpi) is
   begin
      n.sign := true;
      n.ends  := 0;
      if n.data /= null then
         free(n.data);
      end if;
      n.data := null;
   end initialize;

   procedure free(n : in out mpi) renames initialize;
   procedure grow(n : in out mpi; l : in integer) is
      new_data : limbs_access;
   begin
      if n.data = null then
         n.data := new limbs(1..l);
         if n.data = null then
            raise oom;
         end if;
         n.data(1) := 0;
         n.ends := 1;
         return;
      elsif n.data'length < l then
         new_data := new limbs(1..l);
         new_data(n.data'range) := n.data.all;
         free(n.data);
         n.data := new_data;
         ends_loop:
         for i in reverse n.data'range loop
            if n.data(i) /= 0 then
               n.ends := i;
               exit ends_loop;
            end if;
         end loop ends_loop;
      end if;
   end grow;

   -- need?
   -- procedure swap(l, r : in out mpi);
   -- helpers
   procedure debug(s: string) is begin ada.text_io.put_line(s); end debug;
   function norm_g (l, r : in mpi) return boolean;
   function character_to_integer(c : in character) return integer is
   begin
      case c is
         when '0'..'9' =>
            return character'pos(c)-character'pos('0');
         when 'a'..'f' =>
            return character'pos(c)-character'pos('a')+10;
         when 'A'..'F' =>
            return character'pos(c)-character'pos('A')+10;
         when others =>
            return 0;
      end case;
   end character_to_integer;

   function string_2_limb (s : in string; b: in limb) return limb is
      li :limb := 0;
   begin
      for i in s'range loop
         li := li*b+limb(character_to_integer(s(i)));
      end loop;
      return li;
   end string_2_limb;
   ---------------
   -- constructors
   -- base is 16, as corrent work
   -- NOTE: msb comes first, but store lsb first, i.e. le

   -----
   -- TODO parse sign
   -----
   function from_string(s : in string;base : in integer := 16) return mpi is
      n : mpi;
      l : integer;
      ss : string(s'range);
      o : integer;
   begin
      -- verify s
      -- FIXME raise exception if s contains other characters
      for i in  s'Range loop
         case s(i) is
            when '0'..'9' | 'a'..'f' =>
               ss(i) := s(i);
            when 'A'..'F' =>
               ss(i) := ada.characters.handling.to_lower (s(i));
               -- FIXME 
               -- when '_' => -- ignore all '_'
               --   null;
            when others =>
               ss(i) := '0';
         end case;
      end loop;

      -- guess the storage size via the length of s
      -- Note: A limb can hold 8 hexadecimals
      l := (ss'length-1) / 8 + 1;
      o := ss'length mod 8;
      initialize(n);
      grow(n,l);
      for i in n.data'range loop
         if i = n.data'last then
            n.data(i) := string_2_limb(ss(1..(ss'last-i*8+8)),16);
         else
            n.data(i) := string_2_limb(ss(ss'last-i*8..(ss'last-i*8+8)),16);
         end if;
      end loop;
      n.ends := n.data'last;
      return n; 
   end from_string;

   package limb_io is new ada.text_io.modular_io(num=>limb);

   procedure put(n : in mpi; r : in integer) is
   begin
      if n.sign then
         ada.text_io.put("[");
      else
         ada.text_io.put("[-");
      end if;
      ada.text_io.put(to_string(n, 16)&"]");
   end put;

   function to_string(n : in mpi; r : in integer) return string is
      t : limb;
      z : integer := 0;
      s1 : string(1..8);
   begin
      if n.data = null then
         return "null";
      end if;
      t := n.data(n.ends);
      last_limb:
      for i in 1..8 loop
         exit last_limb when t = 0;
         z := z + 1;
         s1(9-z) := hex_string(integer(t mod 16)+1);
         t := t / 16;
      end loop last_limb;
      declare
         s : string(1..8*(n.ends-1)+z);
      begin
         s(1..z) := s1(9-z..8);
         rest_limbs:
         for i in n.data'first .. (n.ends-1) loop
            t := n.data(i);
            read_limb:
            for j in 1..8 loop
               s((n.ends-i)*8+1+z-j) := hex_string(integer(t mod 16)+1);
               t := t / 16;
            end loop read_limb;
         end loop rest_limbs;
         return s;
      end;
   end to_string;

   function from_integer(v : in integer) return mpi is
      n : mpi;
   begin
      if v < 0 then
         n.sign := false;
      else
         n.sign := true;
      end if;
      n.data := new limbs(1..1);
      n.data(1) := limb(v);
      n.ends := 1;
      return n;
   end from_integer;

   ---- arithmetic

   -- private use ?
   -- 
   function sum (l,r : in mpi) return limbs_access is
      n : limbs_access;
      carry : limb := 0;
   begin
      if l.ends < r.ends then
         return sum(r,l);
      else
         n := new limbs(1..l.ends+1);
         if n = null then raise oom; end if;
         n.all := (others =>0);
         for i in r.data'range loop
            n(i) := l.data(i) + r.data(i) + carry;
            if 16#ffffffff# - l.data(i) - carry >= r.data(i) or
               16#ffffffff# - r.data(i) - carry >= l.data(i) then
               carry := 0;
            else
               carry := 1;
            end if;
         end loop;
         if l.ends > r.ends then
            for i in r.ends+1 .. l.ends loop  
               n(i) := l.data(i) + carry;
               carry := (if n(i) < l.data(i) then 1 else 0);
            end loop;
            if carry = 1 then
               n(l.ends+1) := 1;
            end if;
         elsif carry = 1 then
            n(l.ends+1) := 1;
         end if;
         return n;
      end if;
   end;

   -- l >= r !!
   function difference (l,r : in mpi) return limbs_access is
      n : limbs_access;
   begin
      if l.ends > r.ends or 
         (l.ends = r.ends and l.data(l.ends) >= r.data(r.ends)) then
         declare
            carry : limb;
         begin
            carry := 0;
            n := new limbs(1..l.ends);
            if n = null then raise oom; end if;
            sub_loop:
            for i in l.data'first .. l.ends loop
               if i > r.ends then
                  n(i) := l.data(i) - carry;
                  carry := (if l.data(i) >=  carry then 0 else 1);
               else
                  n(i) := l.data(i) - r.data(i) - carry;
                  carry := (if l.data(i) >= r.data(i) +  carry then 0 else 1);
               end if;
            end loop sub_loop;
            return n;
         end;
      end if;
      -- igore order
      return difference(r,l);
   end difference;

   function "+" (l, r: in mpi) return mpi is
      n : mpi;
   begin
      if l.sign = r.sign then
         n.data := sum(l,r);
         n.sign := l.sign;
      elsif norm_g(l,r) then
         n.data := difference(l,r);
         n.sign := l.sign;
      else
         n.data := difference(r,l);
         n.sign := r.sign;
      end if;
      for i in reverse n.data'range loop
         if n.data(i) /= 0 then
            n.ends := i;
            return n;
         end if;
      end loop;
      n.ends := n.data'first;
      return n;
   end "+";

   -- use minus, which ignore sign
   function "-" (l, r: in mpi) return mpi is
      n : mpi;
   begin
      if l.sign and r.sign then
         if l > r then
            n.data := difference(l,r);
            n.sign := true;
         else
            n.data := difference(r,l);
            n.sign := false;
         end if;
      elsif not (l.sign or r.sign) then
         if norm_g(l,r) then
            n.data := difference(l,r);
            n.sign := false;
         else
            n.data := difference(r,l);
            n.sign := true;
         end if;
      else
         n.data := sum(l,r);
         if l.sign and not r.sign then
            n.sign := true;
         else
            n.sign := false;
         end if;
      end if;
      for i in reverse n.data'range loop
         if n.data(i) /= 0 then
            n.ends := i;
            return n;
         end if;
      end loop;
      n.ends := n.data'first;
      return n;
   end "-";

   function "*" (l, r: in mpi) return mpi is
   begin
      return mpi'(true,0,null);
   end "*";

   function "/" (l, r: in mpi) return mpi is
   begin
      return mpi'(true,0,null);
   end "/";

   function absolute (n: in mpi) return mpi is -- FIXME:deep copy, as no refs implemented
   begin
      return mpi'(true,0,null);
   end absolute;

   function modulo (l, r: in mpi) return mpi is
   begin
      return mpi'(true,0,null);
   end modulo;

   -- procedure exp_mod
   function gcd(a, b : in mpi) return mpi is
   begin
      return mpi'(true,0,null);
   end gcd;

   function inv_mod(a, n: in mpi) return mpi is
   begin
      return mpi'(true,0,null);
   end inv_mod;

   function is_prime(n : in mpi) return boolean is
   begin
      return false;
   end is_prime;

   function gen_prime(l,f : in integer) return mpi is -- bits and dh flag
   begin
      return mpi'(true,0,null);
   end gen_prime;
   -- methods
   --- modify
   procedure set_as(n : in out mpi; v : in integer) is
   begin
      null;
   end set_as;

   procedure set_as(n : in out mpi; v : in mpi) is -- deep_copy(v, n)?
   begin
      null;
   end set_as;

   procedure randomize(n : in out mpi) is
   begin
      null;
   end randomize;
   ---- bit ops
   procedure shift_l(n : in out mpi; c : integer) is
   begin
      null;
   end shift_l;

   procedure shift_r(n : in out mpi; c : integer) is
   begin
      null;
   end shift_r;

   procedure set_bit(n : in out mpi; p : in integer; v : integer) is -- 0 or 1
   begin
      null;
   end set_bit;

   function get_bit(n : in mpi; p : in integer) return integer is -- 0 or 1
   begin
      return 0;
   end get_bit;

   function lsb(n : in mpi) return integer is
   begin
      return 0;
   end lsb;

   function msb(n : in mpi) return integer is
   begin
      return 0;
   end msb;
   -- query
   function size(n : in mpi) return integer is
   begin
      return 0;
   end size;

   function "=" (l, r : in mpi) return boolean is
   begin
      return false;
   end "=";

   function norm_g (l, r : in mpi) return boolean is
   begin
      if l.ends > r.ends and l.data(l.ends) /= 0 then
         -- FIXME if l.data(l.ends) = 0
         return true;
      elsif l.ends = r.ends then
         for i in reverse 1..l.ends loop
            if l.data(i) /=  r.data(i) then
               if l.data(i) > r.data(i) then
                  return true;
               elsif l.data(i) < r.data(i) then
                  return false;
               end if;
            end if;
         end loop;
         -- = ?
         return false; 
      else
         return false;
      end if;
   end norm_g;

   function ">" (l, r: in mpi) return boolean is
      b : boolean := false;
   begin
      if l.sign and not r.sign then return true; end if;
      if r.sign and not l.sign then return false; end if;
      -- same sign
      b := norm_g(l,r);
      if (l.sign and b) or (not l.sign and not b) then
         return true;
      else
         return false;
      end if;
   end ">";
end bignum;
