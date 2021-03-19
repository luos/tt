# tt
Erlang Test Tool


### Compare Maps

                  {expected,
                      #{<<"x">> => <<"2021-03-19 15:58:18">>,
                        <<"y">> => 1616169498008,
                        <<"name">> => <<"q">>,<<"z">> => 1,
                        <<"v">> => <<"q">>,<<"u">> => <<"/">>}},
                  {value,
                      #{x => <<"2021-03-19 15:58:18">>,
                        y => 1616169498008,name => <<"q">>,
                        z => 1,v => <<"q">>,
                        u => <<"/">>}}]},

### Lists Member

?assert(lists:member(Value, List)),
