with ada.unchecked_deallocation;
with ada.characters.handling;
with ada.text_io;
package body bignum is
   use interfaces;
   mul_base_digits : constant := 80;
   slot_size : constant := 2_000_000_000;
   hex_string : string(1..16) := "0123456789ABCDEF";

   procedure debug(s: string) is begin ada.text_io.put_line(s); end debug;

   package limb_io is new ada.text_io.modular_io(num=>limb);

   function norm_cmp (l, r : in mpi) return boolean;

   procedure free is new ada.unchecked_deallocation(object=>limbs,name=>limbs_access);

   -- constructors
   overriding
   procedure initialize(n : in out mpi) is
   begin
      n.sign := true;
      n.ends  := 0;
      n.data := null;
   end initialize;

   overriding
   procedure adjust(n : in out mpi) is
      ref : limbs_access := n.data;
   begin
      remove_leading_zeroes(n);
      -- deep copy
      n.data := new limbs(1..n.ends);
      n.data.all := ref(1..n.ends);
   end adjust;

   overriding
   procedure finalize(n : in out mpi) is
   begin
      if n.data /= null then
         free (n.data);
      end if;
   end finalize;

   procedure grow(n : in out mpi; l : in integer) is
      new_data : limbs_access;
   begin
      if n.data = null then
         n.data := new limbs(1..l);

         if n.data = null then raise oom; end if;
         n.data(1..l) := (others =>0);
         n.ends := 0;
      elsif n.data'length < l then
         new_data := new limbs(1..l);
         new_data(n.data'range) := n.data.all;
         free(n.data);
         n.data := new_data;
         remove_leading_zeroes(n);
      end if;
   end grow;

   procedure remove_leading_zeroes(n: in out mpi) is
   begin
      for i in reverse 1..n.ends loop
         if n.data(i) /= 0 then
            n.ends := i;
            return;
         end if;
      end loop;
   end remove_leading_zeroes;


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

   function string_2_limb (s : in string; f, t: integer; b: in limb := 16) return limb is
      li :limb := 0;
   begin
      -- if t - f > 8 or t > s'last or f < s'first then
      --   raise bad_input;
      -- end if;

      for i in f..t loop
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

      -- Note: A limb can hold 8 hexadecimals
      l := (ss'length-1) / 8 + 1;
      o := ss'length mod 8;
      grow(n,l);

      for i in n.data'range loop
         if i = n.data'last and o /= 0 then -- get first `o' characters
            n.data(i) := string_2_limb(ss,1,o,16);
         else
            n.data(i) := string_2_limb(ss,ss'last+1-8*i, ss'last-i*8+7,16);
         end if;
      end loop;

      n.ends := n.data'last;
      remove_leading_zeroes(n);
      return n;
   end from_string;

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

      if z = 0 and n.ends = 1 then
         return "0";
      end if;

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

            for i in r.ends + 1 .. l.ends loop  
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
      elsif norm_cmp(l,r) then
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
         if norm_cmp(l,r) then
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
      prod : interfaces.unsigned_64;
      carry : interfaces.unsigned_64;
      data : limbs_access;
      ends : integer;
   begin
      data := new limbs(1..l.ends+r.ends);
      if data = null then raise oom; end if;
      data.all := (others=>0);
      for i in 1..l.ends loop
         carry := 0;
         for j in 1..r.ends loop
            prod := carry +
            interfaces.unsigned_64(data(i+j-1)) +
            interfaces.unsigned_64(l.data(i)) *
            interfaces.unsigned_64(r.data(j));
            data(i+j-1) := limb(prod mod 2**32);
            carry := prod / 2**32;
         end loop;
         data(i+r.ends) := limb(carry);
      end loop;

      for i in reverse data'range loop
         if data(i) /= 0 then
            ends := i;
            exit;
         end if;
      end loop;
      return (Ada.Finalization.Controlled
      with sign=> (l.sign and r.sign), ends=>ends,data=>data);
   end "*";

   function "/" (l, r: in mpi) return mpi is
   begin
      return null_mpi;
   end "/";

   function square(n : in mpi) return mpi is
      prod : interfaces.unsigned_64;
      carry : interfaces.unsigned_64 := 0;
      data : limbs_access;
      ends : integer;
   begin
      data := new limbs(1..n.ends*2);
      if data = null then raise oom; end if;
      data.all := (others=>0);
      for i in 1..n.ends loop
         prod := carry +
         interfaces.unsigned_64(n.data(i)) *
         interfaces.unsigned_64(n.data(i));
         data(i*2-1) := limb(prod mod 2**32);
         carry := prod / 2**32;
         for j in i+1..n.ends loop
            prod := carry +
            interfaces.unsigned_64(data(i+j-1)) +
            interfaces.unsigned_64(n.data(i)) *
            interfaces.unsigned_64(n.data(j)) * 2;
            data(i+j-1) := limb(prod mod 2**32);
            carry := prod / 2**32;
         end loop;
         data(i + n.ends) := limb(carry);
      end loop;
      for i in reverse data'range loop
         if data(i) /= 0 then
            ends := i;
            exit;
         end if;
      end loop;
      return (Ada.Finalization.Controlled
      with sign=> true, ends=>ends,data=>data);
   end square;

   function modulo (l, r: in mpi) return mpi is
   begin
      return null_mpi;
   end modulo;

   -- procedure exp_mod
   function gcd(a, b : in mpi) return mpi is
   begin
      return null_mpi;
   end gcd;

   function inv_mod(a, n: in mpi) return mpi is
   begin
      return null_mpi;
   end inv_mod;

   function is_prime(n : in mpi) return boolean is
   begin
      return false;
   end is_prime;

   function gen_prime(l,f : in integer) return mpi is -- bits and dh flag
   begin
      return null_mpi;
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
      pos_limb, pos_bit : integer;
   begin
      if p < 1 then return 0; end if;
      pos_limb := (p-1) / 32 + 1;
      pos_bit := p mod 32;
      if pos_limb <= n.ends then
         return (if (n.data(pos_limb) and limb(2**pos_bit)) /= 0 then 1 else 0);
      end if;
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
      return n.ends;
   end size;

   function "=" (l, r : in mpi) return boolean is
   begin
      if l.ends = r.ends then
         for i in reverse 1..l.ends loop
            if l.data(i) /=  r.data(i) then
               return false;
            end if;
         end loop;
         return true;
      end if;
      return false;
   end "=";

   function norm_cmp (l, r: in mpi) return boolean is
   begin
      if l.ends > r.ends then 
         if l.data(l.ends) /= 0 then
            return true;
         else
            --  in rare case of l.data(l.ends) = 0
            declare
               ll : mpi := l;
            begin
               for i in reverse 1..l.ends loop
                  if l.data(i) /= 0 then
                     ll.ends := i;
                     return norm_cmp(ll,r);
                  end if;
               end loop;
            end;
         end if;
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
      end if;
      return false;
   end norm_cmp;

   function ">" (l, r: in mpi) return boolean is
   begin
      if l.sign and not r.sign then return true; end if;
      if r.sign and not l.sign then return false; end if;
      if l.sign and not norm_cmp(l,r)then return false; end if;
      return true;
   end ">";
end bignum;
