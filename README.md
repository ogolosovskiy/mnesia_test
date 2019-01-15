mnesia_test  
=====  
  
An OTP application  
  
Build  
-----  
  
$ rebar3 shell  
1> mnesia_test_gen:write_test(30000).  
write (one index for 30000 records): 1.309399 seconds  
write (two indexes for 30000 records): 1.443878 seconds  
ok  
2> mnesia_test_gen:update_test(30000).  
update(one index for 30000 records): 1.350957 seconds  
update(two indexes for 30000 records): 1.475005 seconds  
ok  
  
  
