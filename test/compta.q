side:`buy`sell
ordType:`limit`market
ledgerType:`deposit`withdrawal`trade`transfer

trades:([]
 time:`timestamp$();
 sym:`symbol$();
 id:`guid$();
 side:`side$();
 ordType:`ordType$();
 price:`float$();
 qty:`float$())

ledgers:([]
 time:`timestamp$();
 sym:`symbol$();
 typ:`ledgerType$();
 id:`guid$();
 refid:`guid$();
 amount:`float$();
 fee:`float$())

upd:insert
