000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300070201     ffnbrv01l  if   e           k disk
000400070201     ffnbrv03l  if   e           k disk    rename(fnbrv000:fnbrv3) prefix(x)
000500000607     ffnart27l  if   e           k disk
000600000607     ffnarb01l  if   e           k disk
000700020916     ffnfvv01l  if   e           k disk
000800000608     fqsysprt   o    f  132        printer infds(stamp) usropn
000900000607     D kpjba         e ds
001000021104     D savkpjbu        s                   like(kpjbu)
001100000607     D UT§DSE0F      E DS
001200000607     D CNCR80        E DS
001300000608     d stamp           ds
001400000608     d  lin                  367    368b 0
001500000607     d knpg            s                   like(brvnpg) inz(4)
001600000607     d knpg8           s                   like(brvnpg) inz(8)
001700000607     d knfv            s                   like(brvnfv)
001800000607     d knfvv           s                   like(fvvnfv)
001900000607     d data            s                   like(brvdfs)
002000000608     d woggi           s              8  0
002100000608     d times           s              6  0
002200000608     d dataora         s             14  0
002300000608     d rsut            s             20
002400000608     d conta           s              6  0
002500070201     d ora412          s             14  0 inz(0)
002600070201     d ora812          s             14  0 inz(0)
002700020822     c* ds x passaggio parametri
002800020822     D fnlr55          DS
002900020822     D  po55                          3  0
003000020822     d  knfv5                         5  0
003100021104     d trul06ds      e ds
003200021104     d  lin£                   1     90  0 dim(30)
003300000607      *--------------------------------------------------------------------
003400000608      * reperimento dati foglio
003500000608     c                   open      qsysprt
003600000608     c                   z-add     0             conta
003700020916     c     kfvv          chain     fnfvv01l
003800000607     c                   if        %found
003900000613      *
004000000613      * lettura spunte partendo dal foglio 4 con il nr.distinta da controllare
004100000613      * ______________________________________________________________________
004200070201     c     kfib1         setll     fnbrv01l
004300000607     c                   do        *hival
004400070201     c     kfib1         reade     fnbrv01l
004500000607     c                   if        %eof
004600000607     c                   leave
004700000607     c                   end
004800020823      * cerca corrispondenza su fnarb se non lo trovo segnalo manca bolla
004900000703     c     kfnart        chain     fnart27l
005000000703     c                   if        %found
005100000703     c                   clear                   arbndc
005200000703     c     karb          chain     fnarb01l
005300021104      * trovato arb confronta se la linea arrivo è del terminal
005400021104     c     arblna        lookup    lin£                                   99
005500021104     c                   if        not *in99
005600021104     c                   move      'X'           mancab            1
005700021104     c                   exsr      estrai
005800021104     c                   add       1             conta
005900021104     c                   move      *blank        mancab
006000021104     c                   endif
006100021104      * non trovato arb
006200000703     c                   else
006300000703     c                   move      'X'           mancab            1
006400000703     c                   exsr      estrai
006500000703     c                   add       1             conta
006600000703     c                   move      *blank        mancab
006700000703     c                   endif
006800000613      * ricerca spunte su foglio 8: ricerco se esiste una spunta di rientro su
006900000613      *                su un altro foglio di rientro
007000070201     c     kfib3         setll     fnbrv03l
007100000607     c                   do        *hival
007200070201     c     kfib3         reade     fnbrv03l
007300000607     c                   if        %eof
007400000607     c                   leave
007500000607     c                   end
007600000731      *memorizzo data e ora spunta del foglio 4
007700000731     c                   movel     brvdfs        ora412
007800000731     c                   move      brvhms        ora412
007900000731      *memorizzo data e ora spunta del foglio 8
008000000607     c                   if        xbrvdfs =  0
008100000731     c                   movel     xbrvdcs       ora812
008200000731     c                   move      xbrvhcs       ora812
008300000607     c                   else
008400000731     c                   movel     xbrvdfs       ora812
008500000731     c                   move      xbrvhms       ora812
008600000607     c                   end
008700000613      * se trovata anomalia estrae solo se la data della spunta sull'altro
008800000613      *                  foglio è = o > della data della distinta da controllare
008900000607     c                   if        xbrvnfv <> knfv and
009000000731     c                             ora812 >= ora412
009100000608      * cerca corrispondenza su fnarb
009200000608     c     kfnart        chain     fnart27l
009300000608     c                   if        %found
009400000613     c                   clear                   arbndc
009500000608     c     karb          chain     fnarb01l
009600000608     c                   endif
009700000613      * estrae solo se anche sulla bolla il nr. distinta corrisponde
009800000613     c                   if        arbndc = brvnfv
009900000607     c                   exsr      estrai
010000000608     c                   add       1             conta
010100000607     c                   endif
010200000613     c                   endif
010300000607     c                   enddo
010400000607     c                   enddo
010500000607     c                   seton                                        90
010600000613      *
010700000613      * lettura spunte partendo dal foglio 8 con il nr.distinta da controllare
010800000613      * ______________________________________________________________________
010900000607     c                   move      8             knpg
011000070201     c     kfib1         setll     fnbrv01l
011100000607     c                   do        *hival
011200070201     c     kfib1         reade     fnbrv01l
011300000607     c                   if        %eof
011400000607     c                   leave
011500000607     c                   end
011600000613      * controlla se la spedizione è inserita nella stessa distinta
011700000607     c     kfnart        chain     fnart27l
011800000607     c                   if        %found
011900000607     c     karb          chain     fnarb01l
012000000613      * incongruenza nr. foglio rientro e distinta su bolla estrae
012100000607     c                   if        %found and arbndc <> brvnfv
012200000707      * controllo se la spunta tipo 4 è su un'altro foglio
012300070201     c     kfib3b        chain     fnbrv03l
012400000707     c                   if        %found and xbrvnfv <> brvnfv
012500000707     c                   movel     brvdfs        ora812
012600000707     c                   move      brvhms        ora812
012700000707     c                   movel     xbrvdfs       ora412
012800000707     c                   move      xbrvhms       ora412
012900000707      * se è su un'altro foglio controllo data e ora per vedere se è stata
013000000707      * rimessa in un giro di consegna dopo la spunta di rientro
013100000707     c                   if        ora812 > ora412
013200000607     c                   exsr      estrai
013300000608     c                   add       1             conta
013400000707     c                   endif
013500000707     c                   endif
013600000607     c                   endif
013700000607     c                   endif
013800000608     c                   enddo
013900000607     c                   endif
014000000608     c                   close     qsysprt
014100000608      *visualizza cancella spool solo se esiste
014200000608     c                   if        conta > 0
014300000608     c                   call      'FNLR55C1'
014400000608     c                   endif
014500000607
014600000607     c                   seton                                        lr
014700000607      *-----------------------------------------------
014800000607     c     estrai        begsr
014900000607      *-----------------------------------------------
015000000607     c                   if        lin > 60
015100000607     c                   except    testa
015200000607     c                   end
015300000703     c                   if        mancab = *blank
015400000607     c                   except    detta
015500000703     c                   else
015600000703     c                   except    dettab
015700000703     c                   end
015800000607     c                   endsr
015900000607      *-----------------------------------------------
016000000607     c     *inzsr        begsr
016100000607      *-----------------------------------------------
016200000607     c
016300000607     c     *entry        plist
016400000607     c                   parm                    kpjba
016500020822     c                   movel     kpjbu         fnlr55
016600000608     c                   z-add     knfv5         knfvv
016700000608     c                   z-add     knfv5         knfv
016800000607
016900000607     C                   Z-ADD     1             CODUT
017000000607     C                   CALL      'X§PARUT'
017100000607     C                   PARM                    UT§DSE0F
017200000608     c                   movel     ragut         rsut
017300000607     C                   MOVEL     REC80         CNCR80
017400000607
017500000607     c     kfvv          klist
017600000607     c                   kfld                    knpg
017700000607     c                   kfld                    knfvv
017800020822     c                   kfld                    po55
017900000607
018000000607     c     kfnart        klist
018100000607     c                   kfld                    brvlnp
018200000607     c                   kfld                    brvlna
018300000607     c                   kfld                    brvnrs
018400000607     c                   kfld                    brvnsc
018500000607
018600000607     c     karb          klist
018700000607     c                   kfld                    artaas
018800000607     c                   kfld                    artlnp
018900000607     c                   kfld                    artnrs
019000000607     c                   kfld                    artnsp
019100000607
019200000607     c     kfib1         klist
019300000607     c                   kfld                    knpg
019400000607     c                   kfld                    knfv
019500020822     c                   kfld                    po55
019600000607
019700000607     c     kfib3         klist
019800000607     c                   kfld                    knpg8
019900000607     c                   kfld                    brvlnp
020000000607     c                   kfld                    brvlna
020100000607     c                   kfld                    brvnrs
020200000607     c                   kfld                    brvnsc
020300000607     c                   kfld                    brvfgs
020400000707     c     kfib3b        klist
020500000707     c                   kfld                    knpg
020600000707     c                   kfld                    brvlnp
020700000707     c                   kfld                    brvlna
020800000707     c                   kfld                    brvnrs
020900000707     c                   kfld                    brvnsc
021000000707     c                   kfld                    brvfgs
021100000608     c                   time                    dataora
021200000608     c                   move      dataora       woggi
021300000608     c                   movel     dataora       times
021400000608     c                   z-add     66            lin
021500021104     C*  Caricamento £1
021600021104     c                   clear                   trul06ds
021700021104     c                   movel     kpjbu         savkpjbu
021800021104     c                   eval      d06cod = '£1'
021900021104     c                   movel     simpou        d06key
022000021104     c                   movel     trul06ds      kpjbu
022100021104     c                   call      'TRUL06R'
022200021104     c                   parm                    kpjba
022300021104     c                   movel     kpjbu         trul06ds
022400021104     c                   movel     savkpjbu      kpjbu
022500000607
022600000607     c                   endsr
022700000607     O*------------------------------------------------------*
022800000607     OQSYSPRT   e            TESTA             2
022900000608     o                       rsut                20
023000000608     O                                           21 '/'
023100020823     O                       po55                24
023200000613     O                                           53 '***   A N O M A L I E   '
023300000613     O                                              'SPUNTE  DI  RIENTRO  DIS'
023400000613     O                                              'TINTE  CONSEGNA    ***'
023500000608     O                                          111 'FNLR55R2'
023600000731     O                       WOGGI              122 '  /  /    '
023700000607     O                                          127 'Pag.'
023800000607     O                       page               132
023900000607     O          e            TESTA       1
024000000607     O                       KNSIF               10
024100000607     O                       KNMUS               21
024200000607     O                       TIMES              121 '  .  .  '
024300000607     O          e            TESTA       2
024400000608     O                                            4 'Lna'
024500000608     O                                            9 'Lnp'
024600000613     O                                           21 'Segnacollo '
024700000613     O                                           31 'Nr.pist.'
024800000613     O                                           40 'Spunta su'
024900000613     O                                           52 'Foglio Nr.'
025000000613     O                                           70 'A N O M A L I A'
025100000608     O                                          119 'S p e d i z i o n e'
025200000608     o          e            detta       1
025300000608     O                       BRVLNa        z      4
025400000608     O                       BRVLNP        z      9
025500000608     O                       BRVNRS        z     12
025600000608     O                       BRVNSC        Z     19
025700000613     O                       BRVNPS              28
025800000613     o              n90                          40 'Consegn.'
025900000613     o               90                          39 'Rientro'
026000000613     o                       brvnpg        z     42
026100000613     o                       brvnfv        z     50
026200000614     o              n90                          79 'spuntato su foglio di ri'
026300000703     o              n90                          90 'entro   Nr.'
026400000614     o               90                          79 'spedizione presente in d'
026500000703     o               90                          90 'istinta Nr.'
026600000613     o              n90      xbrvnfv       z     98
026700000613     o               90      arbndc        z     98
026800000608     o                       arbaas        z    104
026900000608     o                       arblnp        z  +   1
027000000608     o                       arbnrs        z  +   1
027100000608     o                       arbnsp        z  +   1
027200000703     o          e            dettab      1
027300000703     O                       BRVLNa        z      4
027400000703     O                       BRVLNP        z      9
027500000703     O                       BRVNRS        z     12
027600000703     O                       BRVNSC        Z     19
027700000703     O                       BRVNPS              28
027800000703     o              n90                          40 'Consegn.'
027900000703     o                       brvnpg        z     42
028000000703     o                       brvnfv        z     50
028100000703     o                                        +   5 'Segnalazione manca bolla'
