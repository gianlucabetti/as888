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
004400170526      * ricavo l'ora media di consegna per autista, tipo servizio, giro
004401170526     C/EXEC SQL
004402171016     C+ insert into fiapd60f (APD6PDR, apd6tsp, APD6FGSG, APD6CGI, APD6gg,
004403171016     C+ APD6HMC, apd6nsp, apd6ora, apd6time)
004405171016     C+ SELECT pdc, tsp, poc, cgi, ggcon,
004406170526     C+ decimal(trim(char(int(avg(decimal(substr(digits(arbhmc), 1,
004407170526     C+ 2))*60+decimal(substr(digits(arbhmc), 3, 2)))/60))) *100+
004408170526     C+ trim(char(int(mod(avg( decimal(substr(digits(arbhmc), 1, 2))*60+
004409170526     C+ decimal(substr(digits(arbhmc), 3, 2))), 60)))), 4, 0), count(*) nsp,
004410171016     C+ '00:00:00', current_date
004428170526     C+ FROM
004429170526     C+ (SELECT
004430170526     C+ ARGPDC pdc,
004431170526     C+ case when arbtsp in ('C', 'D') then ' ' else arbtsp end as tsp ,
004432170727     C+ ARGpoc poc,
004433171016     C+ ARGCGI cgi, DAYOFWEEK(
004434171016     C+ DATE( TIMESTAMP_FORMAT ( DIGITS(arbdcm) , 'YYYYMMDD' ))) ggcon,
004435170526     C+ arbhmc
004436170526     C+ FROM
004437170526     C+ fiarG00f join fnarb00f
004438170526     C+ ON
004439170526     C+ arGaas= arbaas and
004440170526     C+ arGlnp= arblnp and
004441170526     C+ arGnrs= arbnrs and
004442170526     C+ arGnsp= arbnsp
004443170526     C+ WHERE
004445171107     C+ (ARGcgI <>' ' and ARGPDC<> 0 and arbdcm > :wdat and
004446171107     C+ arbhmc > 0 and arbtsp <> 'H' ) or
004447171025     C+ (ARGcgi<> ' ' and ARGPDC<> 0 and arbdcm > :wdat and
004448171025     C+ arbhmc > 0 and arbtsp = 'H' and
004449170728     C+ arbhmc <= 1030)) as tab
004450171016     C+ GROUP BY pdc, tsp, poc, cgi, ggcon
004451170526     C/END-EXEC
004452170526     c*  l'ora media di consegna la porto ad un intervallo di 30 minuti
004453171107     c*  la prima mezz'ora la porto ai xx:15 la seconda mezz'ora la porto
004454171107     c*  ai xx:45
004455170526     C/EXEC SQL
004456171107     C+ UPDATE UNITRAGRP/FIAPD60F SET APD6ORA = case when
004457171107     C+ minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI'))) <30
004458171107     C+ then TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc),'HH24MI')) + 15 minute
004459171107     C+ - minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI')))minute
004460171107     C+ else TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc),'HH24MI')) + 45 minute
004461171107     C+ - minute(TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI')))minute
004462171107     C+ end WHERE apd6ora = '00:00:00'
004468170526     C/END-EXEC
004500140930     c                   return
