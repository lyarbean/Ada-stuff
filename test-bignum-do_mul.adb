separate (test.bignum)
procedure do_mul is
begin
   put("do_mul");
   new_line;
   put((from_string("88888888"))*(from_string("2")),16);
   new_line;
   put((from_string("FFFFFFFFF"))*(from_string("FFFFFFFFF")),16);
   new_line;
   put((from_string("0"))*(from_string("0")),16);
   new_line;
   put((from_string("12345678"))*(from_string("FEDCBA98")),16);
   new_line;
   declare
      n : mpi;
   begin
      n:= from_integer(1);
      for i in reverse 1..1000 loop
         n := n*from_integer(i);
      end loop;
      put(n);
      new_line;
   end;
   put("end do_mul");
   new_line;
end do_mul;
