separate (test.bignum)
procedure do_square is
begin
   new_line;
   put(square(from_string("88888888")),16);
   new_line;
   put(square(from_string("FFFFFFFFF")),16);
   new_line;
   put(square(from_string("0")),16);
   new_line;
   put(square(from_string("12345678")),16);
   new_line;
end do_square;
