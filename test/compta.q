side:`buy`sell
ordType:`limit`market

trades:([]
 time:`timestamp$();
 sym:`$();
 tid:();
 side:`side$();
 ordType:`ordType$();
 price:`float$();
 qty:`float$();
 cost:`float$();
 fee:`float$())

upd:{
 show type each x;
 show count each x;
 `trades insert x}

\p 5042
\c 25 10000
select wavg[qty; price] from trades where sym = `XTZXBT
