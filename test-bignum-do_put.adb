with ada.integer_text_io;
with ada.text_io;
use ada.text_io;
use ada.integer_text_io;

separate (test.bignum)
procedure do_put is
begin
   put(from_string("ffffffffffffffff"),16);
   put(from_string("ffffffffffffffff0"),16);
   put(from_string("0000ffffffffffffffff0"),16);
   put(from_integer(16#abcd123#),16);
   put(from_integer(16#0#),16);
end do_put;
