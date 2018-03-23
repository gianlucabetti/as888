000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100504     Ftitas30c  iF   E           K DISK
000400100504     FFNarb01L  iF   E           K DISK
000500100504     FFNlbl01L  iF   E           K DISK
000600100504     FFNlbl02L  iF   E           K DISK    rename(fnlbl000:fnlbl002)
000700091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800091125
000900100504      * ds per il controllo della presenza di CA per la spedizione richiesta
001000100505     d kpjba         e ds
001100100505     d fidn12DS      e ds
001200100504     d  skKey                 26    305    dim(20)
001300100504     d  skAnn                306    325    dim(20)
001400100504     d  skDca                326    485  0 dim(20)
001500100504     d  skFca                486    545  0 dim(20)
001600100504     d  skDch                546    705  0 dim(20)
001700100504     d  skCch                706    745    dim(20)
001800100504     d wrkgetlista     s           4096    varying
001900100504     d dsblp         e DS                  extname(fnblp00f)
002000100505     D* DS PER FNLS63R - CANCELLAZIONE BOLLA PARTENZA SINGOLA
002100100505     D DSLS63        E DS                  EXTNAME(FNLS63DS)
002200021214      *--------------------------------------------------
002300091125
002400100504     c     kblp          klist
002500091125     c                   kfld                    blpaas
002600091125     c                   kfld                    blplnp
002700091125     c                   kfld                    blpnrs
002800091125     c                   kfld                    blpnsp
002900100504     c
003000100504     c                   eval      wrkgetlista =
003100100504     c                             'select * from FNBLP00F WHERE BLPDCM=0 +
003200100504     c                             AND BLPDIM<=20100131'
003300100504     C/EXEC SQL
003400100504     C+ PREPARE s1 FROM :wrkgetlista
003500100504     C/END-EXEC
003600100504     C
003700100504     C/EXEC SQL
003800100504     C+ DECLARE A1 CURSOR FOR S1
003900100504     C/END-EXEC
004000100504     C
004100100504     C/EXEC SQL
004200100504     C+ OPEN a1
004300100504     C/END-EXEC
004400100504
004500100504      *?Leggo il file
004600100504     c                   do        *hival
004700100504     C/EXEC SQL
004800100504     C+ FETCH NEXT FROM a1  into :dsblp
004900100504     C/END-EXEC
005000100504     c                   select
005100100504
005200100504     c                   when      sqlcod = 100
005300100504     c                   leave
005400100504     c                   when      sqlcod < 0
005500100504     c                   seton                                        H1
005600100504     c                   leave
005700100504     c                   other
005800100504     c                   exsr      elabora
005900100504     c
006000100504     c                   endsl
006100100504     c                   enddo
006200100504      *
006300100504     C/EXEC SQL
006400100504     C+ CLOSE a1
006500100504     C/END-EXEC
006600100504     c
006700100505     C*
006800100505     C                   CLEAR                   DSLS63
006900100505     C                   MOVEL     'C'           I63TLA
007000100505     C                   MOVEL     DSLS63        KPJBU
007100100505     C                   CALL      'FNLS63R'
007200100505     C                   PARM                    KPJBA
007300100505     C*
007400100504     c                   seton                                        lr
007500100504     c
007600100504     c*------------------------------------------------------------------------
007700100504     c     Elabora       BEGSR
007800100504     c                   clear                   warb              1
007900100504     c                   clear                   wtas              1
008000100504     c                   clear                   wlbl              1
008100100504     c                   clear                   wdan              1
008200070913     c
008300100504     c* Verifico se esiste la bolla in FNARB
008400100504     c     kblp          chain     fnarb01l
008500100504     c*
008600100504    1c                   if        not %found(fnarb01l)
008700100504     c                   eval      warb='N'
008800100504     c*    e stampo se consegnata
008900100504     c                   else
009000100504     c                   if        arbdcm>0
009100100504     c                   eval      warb='C'
009200100504     c                   endif
009300100504     c                   endif
009400100504     c* verifico se  esiste in titas
009500100504     c     kblp          chain     titas30c
009600100504     c
009700100504    2c                   if        not %found(titas30c)
009800100504     c                   eval      wtas='N'
009900100504     c                   else
010000100504     c                   if        tasdcm>0
010100100504     c                   eval      wtas='C'
010200100504     c                   endif
010300100504     c                   endif
010400100504     c
010500100504     c* Verifico se è legata
010600100504     c     kblp          chain     fnlbl01l
010700100504    3c                   if        not %found(fnlbl01l)
010800100504     c     kblp          chain     fnlbl02l
010900100504    4c                   if        not %found(fnlbl02l)
011000100504     c                   eval      wlbl='N'
011100100504     c                   endif
011200100504     c                   endif
011300100504     c* verifico che non ci sia c.a.
011400100504      *
011500100504     c                   clear                   fidn12ds
011600100504     c                   eval      i12aas = blpaas
011700100504     c                   eval      i12lnp = blplnp
011800100504     c                   eval      i12nrs = blpnrs
011900100504     c                   eval      i12nsp = blpnsp
012000100504     c                   eval      i12fel = 'E'
012100100504     c                   eval      i12fan = 'E'
012200100504      *
012300100504     c                   call      'FIDN12R'
012400100504     c                   parm                    fidn12ds
012500100504      *
012600100504      * se non ci sono errori
012700100504    5c                   if        o12err = ' '   and
012800100504     c                             o12nca = 0
012900100504     c                   eval      wDAN='N'
013000100504     c                   endif
013100100504     c* stampo cosa ho trovato
013200100504     c*   se non ha c.A.
013300100504     c                   if        wdan='N'
013400100504     c                   except    bolla
013500100505      *
013600100505      * CANCELLAZIONE FISICA BOLLA E FILES ALLEGATI
013700100505      *
013800100505     c                   if        warb='N' and wlbl='N' and wtas='N'
013900100505     C                   CLEAR                   DSLS63
014000100505     C                   MOVE      BLPAAS        I63AAS
014100100505     C                   MOVE      BLPLNP        I63LNP
014200100505     C                   MOVE      BLPNRS        I63NRS
014300100505     C                   MOVE      BLPNSP        I63NSP
014400100505     C                   MOVEL     DSLS63        KPJBU
014500100505     C                   CALL      'FNLS63R'
014600100505     C                   PARM                    KPJBA
014700100504     c                   endif
014800100505     c                   endif
014900100504     c
015000100504     c                   endSR
015100021216      *
015200100504     OQSYSPRT   E            BOLLA       1
015300091125     O                                              'Bolla:'
015400091125     O                       blpaas           +   1
015500091125     O                       blplnp           +   1
015600091125     O                       blpnrs           +   1
015700091125     O                       blpnsp        2  +   1
015800100504     O                                        +   2 'FNARB'
015900100504     O                       WARB             +   1
016000100504     O                                        +   2 'TITAS'
016100100504     O                       WTAS             +   1
016200100504     O                                        +   2 'FNLBL'
016300100504     O                       WTAS             +   1
016400100504     O                                        +   2 'C.A.'
016500100504     O                       WDAN             +   1
