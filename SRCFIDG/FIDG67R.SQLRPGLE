000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000500140917     D kpjba         e ds
001000000607      *--------------------------------------------------------------------
001001171025     ddtaiso                           d   datfmt(*iso)
001002171025     ddtaeur                           d   datfmt(*eur)
001100140917     c     *entry        plist
001200140917     c                   parm                    kpjba
001201171025     C                   TIME                    W0120            14 0
001202171025     C                   MOVE      W0120         WDAT              8 0
001203171025     c                   move      wdat          dtaeur
001204171025     c                   move      dtaeur        dtaiso
001205171025     c                   subdur    15:*d         dtaiso
001206171025     c                   move      dtaiso        wdat
001207171227     c                   seton                                        lr
004400170526      * ricavo l'ora media di consegna per autista, tipo servizio, giro
004401170526     C/EXEC SQL
004402171227     C+ insert into fiapd70f (apd7PDR, apd7tsp, apd7FGSG, apd7CGI,
004403171227     C+ apd7yla, apd7xlo,
004404171227     C+ apd7gg, apd7HMC, apd7nsp, apd7ora, apd7time)
004405171212     C+ SELECT pdc, tsp, poc, cgi, yla, xlo, ggcon,
004406170526     C+ decimal(trim(char(int(avg(decimal(substr(digits(arbhmc), 1,
004407170526     C+ 2))*60+decimal(substr(digits(arbhmc), 3, 2)))/60))) *100+
004408170526     C+ trim(char(int(mod(avg( decimal(substr(digits(arbhmc), 1, 2))*60+
004409170526     C+ decimal(substr(digits(arbhmc), 3, 2))), 60)))), 4, 0), count(*) nsp,
004410171016     C+ '00:00:00', current_date
004428170526     C+ FROM
004429170526     C+ (SELECT
004430170526     C+ ARGPDC pdc,
004431170526     C+ case when arbtsp in ('C', 'D') then ' ' else arbtsp end as tsp ,
004432171227     C+ ARGpoc poc, ARGCGI cgi,
004433171227     C+ decimal(substr(digits(argyla), 1, 6), 6)/1000 yla,
004434171227     C+ decimal(substr(digits(argxlo), 1, 6), 6)/1000 xlo,
004435171227     C+ DAYOFWEEK(
004436171016     C+ DATE( TIMESTAMP_FORMAT ( DIGITS(arbdcm) , 'YYYYMMDD' ))) ggcon,
004437170526     C+ arbhmc
004438170526     C+ FROM
004439170526     C+ fiarG00f join fnarb00f
004440170526     C+ ON
004441170526     C+ arGaas= arbaas and
004442170526     C+ arGlnp= arblnp and
004443170526     C+ arGnrs= arbnrs and
004444170526     C+ arGnsp= arbnsp
004445170526     C+ WHERE
004446171107     C+ (ARGcgI <>' ' and ARGPDC<> 0 and arbdcm > :wdat and
004447171107     C+ arbhmc > 0 and arbtsp <> 'H' ) or
004448171025     C+ (ARGcgi<> ' ' and ARGPDC<> 0 and arbdcm > :wdat and
004449171025     C+ arbhmc > 0 and arbtsp = 'H' and
004450170728     C+ arbhmc <= 1030)) as tab
004451171212     C+ GROUP BY pdc, tsp, poc, cgi, yla, xlo, ggcon
004452170526     C/END-EXEC
004453170526     c*  l'ora media di consegna la porto ad un intervallo di 30 minuti
004454171107     c*  la prima mezz'ora la porto ai xx:15 la seconda mezz'ora la porto
004455171107     c*  ai xx:45
004456170526     C/EXEC SQL
004457171227     C+ UPDATE FIapd70F SET apd7ORA =
004458171227     C+ case when minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc),
004459171227     C+ 'HH24MI'))) between 0 and 14 then
004460171227     C+ TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')) + 00 minute -
004461171227     C+ minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))minute
004462171227     C+ when minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))
004463171227     C+ between 15 and 29 then TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc),
004464171227     C+ 'HH24MI')) + 15 minute -
004465171227     C+ minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))minute
004466171227     C+ when minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))
004467171227     C+ between 30 and 44 then TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc),
004468171227     C+ 'HH24MI')) + 30 minute -
004469171227     C+ minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))minute
004470171227     C+ when minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))
004471171227     C+ between 45 and 59 then TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc),
004500171227     C+ 'HH24MI')) + 45 minute -
004600171227     C+ minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd7hmc), 'HH24MI')))minute end
004601171227     C+ where apd7ora = '00:00:00'
004700171227     C/END-EXEC
