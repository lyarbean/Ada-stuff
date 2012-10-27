-- use bc's method
with Ada.Finalization;
with interfaces;
package bignum is
   pragma elaborate_body;
   type mpi is new ada.finalization.controlled with  private;
   -- pre-allocate
   procedure grow(n : in out mpi; l : in integer);
   procedure remove_leading_zeroes(n: in out mpi);
   -- constructors
   function from_string(s : in string; base : in integer := 16)  return mpi;
   function from_integer(v : in integer) return mpi;

   ---- arithmetic
   function "+" (l, r: in mpi) return mpi;
   function "-" (l, r: in mpi) return mpi;
   function "*" (l, r: in mpi) return mpi;
   function "/" (l, r: in mpi) return mpi;
   function square(n : in mpi) return mpi;
   function absolute (n: in mpi) return mpi;
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
   -- Started from 1
   function get_bit(n : in mpi; p : in integer) return integer; -- 0 or 1
   function lsb(n : in mpi) return integer;
   function msb(n : in mpi) return integer;

   -- query
   function size(n : in mpi) return integer;
   function "=" (l, r: in mpi) return boolean;
   function ">" (l, r: in mpi) return boolean;
   function to_string(n : in mpi; r : in integer) return string;
   procedure put(n : in mpi; r : in integer);
   oom : exception;
   bad_input: exception;
   -------------
   -- PRIVATE --
   -------------
   private
   subtype limb is interfaces.unsigned_32;
   type limbs is array (integer range<>) of limb;
   type limbs_access is access limbs;

   type mpi is new Ada.Finalization.Controlled with record
      sign : boolean;
      ends : integer; -- the last none zero limb
      data : limbs_access;
   end record;
   overriding
   procedure initialize (n: in out mpi);
   overriding
   procedure adjust     (n: in out mpi);
   overriding
   procedure finalize   (n: in out mpi);
   null_mpi : constant mpi := (Ada.Finalization.Controlled
   with sign=>true, ends=>0,data=>null);
end bignum;
