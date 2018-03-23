000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000500140917     D kpjba         e ds
001000000607      *--------------------------------------------------------------------
001100140917     c     *entry        plist
001200140917     c                   parm                    kpjba
004400170726      * ricavo l'ora media di consegna per tipo servizio, giro
004401170526     C/EXEC SQL
004402171016     C+ insert into fiapd60f (apd6tsp, APD6FGSG, APD6CGI, APD6gg,
004403171016     C+ APD6HMC, apd6nsp, apd6ora, apd6time)
004405171016     C+ SELECT tsp, poc, cgi, ggcon,
004406170526     C+ decimal(trim(char(int(avg(decimal(substr(digits(arbhmc), 1,
004407170526     C+ 2))*60+decimal(substr(digits(arbhmc), 3, 2)))/60))) *100+
004408170526     C+ trim(char(int(mod(avg( decimal(substr(digits(arbhmc), 1, 2))*60+
004409170526     C+ decimal(substr(digits(arbhmc), 3, 2))), 60)))), 4, 0), count(*) nsp,
004410171016     C+ '00:00:00', current_date
004428170526     C+ FROM
004429170526     C+ (SELECT
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
004444170728     C+ ARGTGI = 'A' and ARGPDC<> 0 and arbhmc > 0 and arbtsp <> 'H' or
004445170728     C+ (ARGTGI = 'A' and ARGPDC<> 0 and arbhmc > 0 and arbtsp = 'H' and
004446170728     C+ arbhmc <= 1030)) as tab
004447171016     C+ GROUP BY tsp, poc, cgi, ggcon
004448170526     C/END-EXEC
004449170526     c*  l'ora media di consegna la porto ad un intervallo di 30 minuti
004450170526     c*  superiore, es. 14:20 diventa 14:30 mentre 14:40 diventa 15:00
004451170526     C/EXEC SQL
004452170728     C+ update fiapd60f set apd6ora =
004453170526     C+ case when minute( TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc),
004454170526     C+ 'HH24MI'))) < 30 then TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc),
004455170526     C+ 'HH24MI')) + 30 minute - minute(
004456170526     C+ TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI'))) minute else
004457170526     C+ TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI')) + 60 minute -
004458170526     C+ minute( TIME(TIMESTAMP_FORMAT(DIGITS(apd6hmc), 'HH24MI'))) minute
004459170526     C+ end
004468170526     C/END-EXEC
004500140930     c                   return
