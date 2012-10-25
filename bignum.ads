-- use bc's method

with interfaces;
package bignum is
   type mpi is private; -- tagged, what about use accessors
   -- storage   
   procedure initialize(n : in out mpi);
   procedure free(n : in out mpi);
   procedure grow(n : in out mpi; l : in integer);
   -- need?
   -- procedure swap(l, r : in out mpi);

   -- constructors
   function from_string(s : in string; base : in integer := 16)  return mpi;
   function to_string(n : in mpi; r : in integer) return string;
   function from_integer(v : in integer) return mpi;

   ---- arithmetic
   function "+" (l, r: in mpi) return mpi;
   function "-" (l, r: in mpi) return mpi;
   function "*" (l, r: in mpi) return mpi;
   function "/" (l, r: in mpi) return mpi;
   function absolute (n: in mpi) return mpi; -- FIXME:deep copy, as no refs implemented
   function modulo (l, r: in mpi) return mpi;
   -- procedure exp_mod
   function gcd(a, b : in mpi) return mpi;
   function inv_mod(a, n: in mpi) return mpi;
   function is_prime(n : in mpi) return boolean;
   function gen_prime(l,f : in integer) return mpi; -- bits and dh flag
   -- methods
   --- modify
   procedure set_as(n : in out mpi; v : in integer);
   procedure set_as(n : in out mpi; v : in mpi); -- deep_copy(v, n)?
   procedure randomize(n : in out mpi);
   ---- bit ops
   procedure shift_l(n : in out mpi; c : integer);
   procedure shift_r(n : in out mpi; c : integer);
   procedure set_bit(n : in out mpi; p : in integer; v : integer); -- 0 or 1
   function get_bit(n : in mpi; p : in integer) return integer; -- 0 or 1
   function lsb(n : in mpi) return integer;
   function msb(n : in mpi) return integer;
   -- query
   function size(n : in mpi) return integer;
   function "=" (l, r: in mpi) return boolean;
   procedure put(n : in mpi; r : in integer);
   -------------
   -- PRIVATE --
   -------------
   private
   subtype limb is interfaces.unsigned_32;
   type limbs is array (integer range<>) of limb;
   type limbs_access is access limbs;

   type mpi is record
      sign: boolean;
      ends : integer; -- the last none zero limb or an index for prod
      data: limbs_access;
   end record;
end bignum;
