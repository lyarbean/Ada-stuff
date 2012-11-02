with bignum;
use bignum;
with ada.integer_text_io;
with ada.text_io;
use ada.text_io;
use ada.integer_text_io;
procedure test.bignum is
   procedure  do_put is separate;
   procedure  do_add is separate;
   procedure  do_sub is separate;
   procedure  do_mul is separate;
   procedure  do_square is separate;
   procedure  do_random is separate;
begin
   do_put;
   do_add;
   do_sub;
   do_mul;
   do_square;
   do_random;
end test.bignum;
