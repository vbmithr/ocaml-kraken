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

upd:insert
