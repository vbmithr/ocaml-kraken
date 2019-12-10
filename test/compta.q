side:`buy`sell
ordType:`limit`market

trades:([]
 time:`timestamp$();
 sym:`symbol$();
 tid:`guid$();
 side:`side$();
 ordType:`ordType$();
 price:`float$();
 qty:`float$())

upd:{
 show type each x;
 show count each x;
 `trades insert x}

\p 5042
\c 25 10000
count trades
select wavg[qty; price] from trades where sym = `XTZXBT
select wavg[qty; price] from trades where (sym=`XTZXBT)|sym=`xtz_btc
select from trades where sym=`XTZXBT
