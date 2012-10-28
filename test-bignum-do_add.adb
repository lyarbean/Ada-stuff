separate (test.bignum)
procedure do_add is
begin
   new_line;
   put((from_string("88888888"))+(from_string("888888888")),16);
   new_line;
   put((from_string("FFFFFFFFF"))+(from_string("FFFFFFFFF")),16);
   new_line;
   put((from_string("0"))+(from_string("0")),16);
   new_line;
   put((from_string("12345678"))+(from_string("FEDCBA98")),16);
   new_line;
end do_add;
