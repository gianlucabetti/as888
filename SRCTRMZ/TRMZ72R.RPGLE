000100141127     H*
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400031105     Ftrmz72d   CF   E             WORKSTN
000500030630     Faitra02l  if   e           k disk
000600031105     Fazorg01l  if   e           k disk
000700060922     Fazorg02l  if   e           k disk    rename(azorg:azorg2)
000800060921     ftabel00f  if   e           k disk
000900031028     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001000020304     dkpjba          e ds
001001170810     D TIBS02DS      E DS
001002170810     D TIBS34DS      E DS                                                       *Profili utente
001003170810     D DDATIUTE      E DS                                                       *Dati utente
001004170810     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001005170810     D dute01        E DS                                                       *Utenti
001006170810     d dLat          e ds
001007170810     D TRUL31DS      E DS
001008170810     D TRUL31DS2     E DS
001200020404     d prtds           ds
001300020404     d   spab                         3  0
001400020404     d   spaa                         3  0
001500020404     d   skab                         3  0
001600020404     d   skaa                         3  0
001700020404     d   line                         3  0
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
002000030630     d wdatiso         s               d   datfmt(*iso)
002100030903     d wdateur         s               d   datfmt(*eur)
002200030903     d tradprs         s              8  0
002300030903     d tradp2s         s              8  0
002400031028     d tradrrs         s              8  0
002500031105     d totale          s              5  0
002600031105     d totfil          s              5  0
002700031105     d exfil           s                   like(trafil)
002800040503     D wlbda8          ds
002900040503     D  g02dat                 1      8  0
003000040503     D  g02inv                 9     16  0
003100040503     D  g02err                17     17
003200040503     D  g02tgi                18     22  0
003201170810      *
003202170810     d wabi            s                   like(UteAut)
003203170810     d* p.o. abilitati
003204170810     D $pogn           s              3  0 dim(250)
003205170810     D $pog            s              3    dim(250)
003206170810     D $DIG            s              1    dim(20)
003207170810     D $ARG            s              3    dim(50)
003300060928     D err             S             78    DIM(7) CTDATA PERRCD(1)
003400000000     C*---------------------------------------------------------------*
003500020304      *
003600031105     c                   do        *hival
003700031105     c                   exfmt     video1
003800031105     c                   clear                   $msg
003900031105     c                   setoff                                       999796
004000040503     c                   setoff                                       959493
004100040610     c                   setoff                                       878685
004200040610     c                   setoff                                       818283
004300031105     c   kc              leave
004400031105     c                   exsr      controv1
004500031105     c   96              iter
004600060921     c                   if        *inkf
004700060921     c                   if        v1cfil <> *zeros  or
004800170810     c                             v1cfil = *zeros and dutpou = 046 and
004900060922     c                             v1care = *blank and v1cdir = *blank
005000060921     c                   exsr      aitra
005100060921     c                   else
005200060921     c                   exsr      areadis
005300060921     c                   endif
005400061019     c                   feod      prtf198
005500061019     c                   z-add     99            line
005600060921     c                   endif
005700060921
005800031105     c                   enddo
005900060921     c                   if        unaf = *blank and v1cfil <> *zeros
006000040503     c     v1copz        comp      '1'                                    81
006100040503     c     v1copz        comp      '2'                                    82
006200040610     c     v1copz        comp      '3'                                    83
006300030630     c                   except    testata
006400030630     c                   except    noncene
006500030630     c                   endif
006600020308     c                   seton                                        lr
006700990113     c*-------------------------------------------------
006800030630     c     aitra         begsr
006900990113     c*-------------------------------------------------
007000020404     c                   clear                   una               1
007100040503     c                   clear                   unaf              1
007200031105     c                   if        v1cfil = *zeros
007300031105     c     *loval        setll     aitra02l
007400031105     c                   else
007500031105     c     v1cfil        setll     aitra02l
007600031105     c                   endif
007700981029     c                   do        *hival
007800031105     c                   if        v1cfil = *zeros
007900031105     c                   read      aitra02l                               98
008000031105     c                   else
008100031105     c     v1cfil        reade     aitra02l                               98
008200031105     c                   endif
008300031105     c   98              leave
008400040212     c                   if        traann <> *blank
008500040212     c                   iter
008600040212     c                   endif
008700031105     c                   if        exfil <> *zeros and trafil <> exfil
008800031105     c                             and una <> *blank
008900061020     c                   exsr      decodfil
009000031105     c                   except    filiale
009100031105     c                   clear                   totfil
009200040503     c                   clear                   una
009300031105     c                   endif
009400031105     c                   z-add     trafil        exfil
009500031105      *estrae tutte le anagrafiche con colore mezzo impostato e data ispezione inserita
009600030903     c                   if        tratmp = 'S'
009700030903      *data accreditamento
009800030630     c                   if        tradin > 0
009900030903     c                   move      tradin        dataiso
010000030903     c                   move      dataiso       dataeur
010100030903     c                   else
010200030903     c                   clear                   dataeur
010300030903     c                   end
010400040503      * elabora per opzione impostata
010500040503     c                   select
010600040503     c                   when      v1copz = *blank
010700040503     c                   exsr      noopz
010800040503     c                   when      v1copz = '1'
010900040503     c                   seton                                        81
011000040503     c                   exsr      opzione1
011100040503     c                   when      v1copz = '2'
011200040503     c                   seton                                        82
011300040503     c                   exsr      opzione2
011400040610     c                   when      v1copz = '3'
011500040610     c                   seton                                        83
011600040610     c                   exsr      opzione3
011700040503     c                   endsl
011800040503
011900030522     c                   endif
012000030630     c                   enddo
012100031105
012200031105     c                   eval      $msg  ='Elenco stampato'
012300031105     c                   if        una <> *blank
012400061020     c                   exsr      decodfil
012500031105     c                   except    filiale
012600031105     c                   endif
012700031105     c                   if        v1cfil = 0
012800031105     c                   except    globale
012900031105     c                   endif
013000060922     c                   if        v1cdir = *blank and v1care = *blank
013100031105     c                   z-add     99            line
013200060922     c                   else
013300060922     c                   clear                   v1cfil
013400060921     c                   endif
013500031105     c                   clear                   totfil
013600031105     c                   clear                   totale
013700031105     c                   clear                   exfil
013800020304
013900020304     c                   endsr
014000981029      *
014100061020     c*---------------------------------------------------
014200061020     c     decodfil      begsr
014300061020      *---------------------------------------------------
014400061020     c                   clear                   exfild           20
014500061020     c     exfil         chain     azorg01l
014600061020     c                   if        %found(azorg01l)
014700061020     c                   movel     orgdes        exfild
014800061020     c                   endif
014900061020
015000061020     c                   endsr
015100040503     c*---------------------------------------------------
015200040503     c     noopz         begsr
015300040503      *---------------------------------------------------
015400040503      *data ripristino
015500040503      *      parto dall'ultima e confronto sulla prima valorizzata
015600040503     c                   z-add     999           diffe             3 0
015700040503     c                   select
015800040503     c                   when      tradp2 > 0
015900040503     c                   move      tradp2        dataiso
016000040503     c     dataiso       subdur    wdatiso       diffe:*d
016100040503     c                   when      tradpr > 0
016200040503     c                   move      tradpr        dataiso
016300040503     c     dataiso       subdur    wdatiso       diffe:*d
016400040503     c                   endsl
016500040503
016600040503      * se la data ripristino valorizzata è minore o uguale rispetto ad oggi
016700040503      * lo estraggo oppure se è impostata la data ispezione
016800040503     c                   if        diffe <= 1 or tradrr > 0
016900040503     c                   exsr      stampa
017000040503     c                   endif
017100040503     c                   endsr
017200040503     c*---------------------------------------------------
017300040503     c     opzione1      begsr
017400040503      *---------------------------------------------------
017500040504
017600040504     c                   select
017700040524     c                   when      tradp2 > 0
017800040524     c                   if        tradp2 >= v1cdalg and
017900040504     c                             tradp2 <= v1calg
018000040504     c                   exsr      stampa
018100040524     c                   endif
018200040524     c                   when      tradpr > 0
018300040524     c                   if        tradpr >= v1cdalg and
018400040504     c                             tradpr <= v1calg
018500040504     c                   exsr      stampa
018600040524     c                   endif
018700040504     c                   endsl
018800040504
018900040504     c                   endsr
019000040503     c*---------------------------------------------------
019100040504     c     opzione2      begsr
019200040503      *---------------------------------------------------
019300040504     c                   if        tradrr > 0 and tradp2 = 0  and
019400040504     c                             tradpr = 0
019500040504     c                   exsr      stampa
019600040504     c                   endif
019700040504     c                   endsr
019800040610     c*---------------------------------------------------
019900040610     c     opzione3      begsr
020000040610      *---------------------------------------------------
020100040611     c                   if        tradrr >= v1cdisg and
020200040611     c                             tradrr <= v1caisg
020300040610     c                   exsr      stampa
020400040610     c                   endif
020500040610
020600040610     c                   endsr
020700040503      *------------------------------------------------------
020800040503     c     stampa        begsr
020900040503      *------------------------------------------------------
021000040504     c                   move      'X'           una
021100040504     c                   move      'X'           unaf
021200040503     c                   if        line > 60
021300040503     c                   except    testata
021400040503     c                   endif
021500040503     c                   if        tradpr > 0
021600040503     c     *ymd          move      tradpr        wdateur
021700040503     c                   move      wdateur       tradprs
021800040503     c                   else
021900040503     C                   clear                   tradprs
022000040503     c                   endif
022100040503     c                   if        tradp2 > 0
022200040503     c     *ymd          move      tradp2        wdateur
022300040503     c                   move      wdateur       tradp2s
022400040503     c                   else
022500040503     C                   clear                   tradp2s
022600040503     c                   endif
022700040503     c                   if        tradrr > 0
022800040503     c     *ymd          move      tradrr        wdateur
022900040503     c                   move      wdateur       tradrrs
023000040503     c                   else
023100040503     C                   clear                   tradrrs
023200040503     c                   endif
023300040503     c                   add       1             totfil
023400040503     c                   add       1             totale
023500040503     c                   except    detta
023600040503     c                   if        trant2 <> *blank
023700040503     c                   except    detta2
023800040503     c                   endif
023900040503     c                   endsr
024000031105     c*-------------------------------------------------
024100031105     c     controv1      begsr
024200031105     c*-------------------------------------------------
024201170810     c                   clear                   v1cared
024202170810     c                   clear                   v1cdird
024300031105     c                   clear                   v1dfil
024400060921     c                   if        v1cfil > 0 and v1care <> *blank or
024500060922     c                             v1cfil > 0 and v1cdir <> *blank or
024600060922     c                             v1cdir > *blank and v1care <> *blank
024700060928     c                   eval      $msg = err(5)
024800060928     c                   seton                                        96
024900060928     c                   goto      endrut
025000060921     c                   endif
025100060928      * profilo di area non può selezionare distretto
025200060928     c                   if        v1cdir <> *blank and
025201170810     c                             (wabi <> 'DI' and wabi <> 'AZ')
025400060928     c                   eval      $msg = err(1)
025500060928     c                   seton                                          96
025600060928     c                   goto      endrut
025700060928     c                   endif
036001170810      *  distretto non abilitato
036002170810     c                   if        v1cdir <> *blank
036003170810      * distretto
036004170810     C     v1cdir        ifeq      '?'
036005170810     C                   movel     '17'          kcod
036006170810     C                   clear                   kkey
036007170810     C                   call      'X§TABER'
036008170810     C                   parm                    kkut
036009170810     C                   parm                    kcod
036010170810     C                   parm                    kkey
036011170810     C                   parm                    §des
036012170810     c                   movel     §des          v1cdird
036013170810     c                   movel     kkey          v1cdir
036014170810     C*
036015170810     C                   else
036016170810     C*
036017170810     C                   if        v1cdir <> *blank and v1cdir <> *zero
036018170810     C                   movel     '17'          kcod
036019170810     C                   movel(p)  v1cdir        kkey
036020170810     C     ktab          chain     tabel00f
036021170810     c                   if        %found(tabel00f) and tblflg = *blank
036022170810     C                   movel     tbluni        v1cdird
036023170810     c                   else
036024170810     c                   movel     *all'?'       v1cdird
036025170810     C                   end
036026170810     C                   end
036027170810     C                   end
036028170810     c     v1cdir        lookup    $dig                                   70
036029170810     c                   if        not *in70
036030170810     c                   eval      $msg = err(2)
036031170810     c                   seton                                          96
036032170810     c                   goto      endrut
036033170810     c                   endif
036034170810     c                   endif
036035170810      *  area non abilitata
036036170810     c                   if        v1care <> *blank
036037170810      *  parzializza area
036038170810     C     v1care        ifeq      '?'
036039170810     C                   movel     '05'          kcod
036040170810     C                   clear                   kkey
036041170810     C                   call      'X§TABER'
036042170810     C                   parm                    kkut
036043170810     C                   parm                    kcod
036044170810     C                   parm                    kkey
036045170810     C                   parm                    §des             30
036046170810     c                   movel     §des          v1cared
036047170810     c                   movel     kkey          v1care
036048170810     C*
036049170810     C                   else
036050170810     C*
036051170810     C                   if        v1care <> *blank and v1care <> *zero
036052170810     C                   movel     '05'          kcod
036053170810     C                   movel(p)  v1care        kkey
036054170810     C     ktab          chain     tabel00f
036055170810     c                   if        %found(tabel00f) and tblflg = *blank
036056170810     C                   movel     tbluni        v1cared
036057170810     c                   else
036058170810     c                   movel     *all'?'       v1cared
036059170810     C                   end
036060170810     C                   end
036061170810     C                   end
036062170810     c     v1care        lookup    $arg                                   70
036063170810     c                   if        not *in70
036064170810     c                   eval      $msg = err(3)
036065170810     c                   seton                                          96
036066170810     c                   goto      endrut
036067170810     c                   endif
036068170810     c                   endif
036069170810      * il po non è abilitato
036070170810     c                   if        v1cfil > 0
036071170810     c     v1cfil        chain     azorg01l
036072170810     c                   if        %found(azorg01l)
036073170810     c                   movel     orgdes        v1dfil
036074170810     c                   endif
036075170810     c     v1cfil        lookup    $pogn                                  70
036076170810     c                   if        not *in70
036077170810     c                   eval      $msg = err(4)
036078170810     c                   seton                                          96
036079170810     c                   goto      endrut
036080170810     c                   endif
036081170810     c                   endif
036100060921
036200060922     c                   if        *in90 and v1cfil = *zeros and
036300060922     c                             v1cdir = *blank and v1care = *blank
036400031105     c                   seton                                        9996
036500031105     c                   endif
036600040503      *opzioni
036700040503     c
036800040503     c                   if        v1copz = '1'
036900040503     c                   if        v1cdal = 0 or v1cal = 0
037000040503     c                   seton                                        9596
037100040503     c                   endif
037200040503      *data dal
037300040503     c                   if        v1cdal > 0
037400040503     C                   move      v1cdal        g02dat
037500040503     C                   movel     *blank        g02err
037600040503     C                   call      'XSRDA8'
037700040503     C                   parm                    wlbda8
037800040503     C     g02err        ifeq      '1'
037900040503     C                   seton                                        9396
038000040503     C                   end
038100040503     c                   move      g02dat        v1cdal
038200040503     c                   move      g02inv        v1cdalg           8 0
038300040503     c                   endif
038400040503      *data al
038500040503     c                   if        v1cal > 0
038600040503     C                   move      v1cal         g02dat
038700040503     C                   movel     *blank        g02err
038800040503     C                   call      'XSRDA8'
038900040503     C                   parm                    wlbda8
039000040503     C     g02err        ifeq      '1'
039100040503     C                   seton                                        9496
039200040503     C                   end
039300040503     c                   move      g02dat        v1cal
039400040503     c                   move      g02inv        v1calg            8 0
039500040503     c                   endif
039600040503     c                   if        v1calg < v1cdalg
039700040503     C                   seton                                        9496
039800040503     c                   endif
039900040503
040000040503     c                   endif
040100040503      *fine opzione 1
040200040610      *opzione 2
040300040504     c                   if        v1copz = '2' or v1copz = *blank
040400040504     c                   z-add     0             v1cdal
040500040504     c                   z-add     0             v1cal
040600040504     c                   z-add     0             v1cdalg
040700040504     c                   z-add     0             v1calg
040800040504     c                   endif
040900040610      *fine opzione 2
041000040610     c
041100040610      *opzione 3
041200040610     c                   if        v1copz = '3'
041300170810     c                   if        v1cdis = 0 or v1cais = 0
041400040610     c                   seton                                        8596
041500040610     c                   endif
041600040610      *data dal
041700040610     c                   if        v1cdis > 0
041800040610     C                   move      v1cdis        g02dat
041900040610     C                   movel     *blank        g02err
042000040610     C                   call      'XSRDA8'
042100040610     C                   parm                    wlbda8
042200040610     C     g02err        ifeq      '1'
042300040610     C                   seton                                        8696
042400040610     C                   end
042500040610     c                   move      g02dat        v1cdis
042600040610     c                   move      g02inv        v1cdisg           8 0
042700040610     c                   endif
042800040610      *data al
042900040610     c                   if        v1cais > 0
043000040610     C                   move      v1cais        g02dat
043100040610     C                   movel     *blank        g02err
043200040610     C                   call      'XSRDA8'
043300040610     C                   parm                    wlbda8
043400040610     C     g02err        ifeq      '1'
043500040610     C                   seton                                        8796
043600040610     C                   end
043700040610     c                   move      g02dat        v1cais
043800040610     c                   move      g02inv        v1caisg           8 0
043900040610     c                   endif
044000040610     c                   if        v1caisg < v1cdisg
044100040610     C                   seton                                        8796
044200040610     c                   endif
044300040610
044400040610     c                   endif
044500040610      *fine opzione 3
044600040503     c
044700060928     c     endrut        endsr
044800060922     c*-------------------------------------------------
044900060922     c     areadis       begsr
045000060922     c*-------------------------------------------------
045100060922     c     *loval        setll     azorg02l
045200060922     c                   do        *hival
045300060922     c                   read      azorg02l
045400060922      * solo i distretti fino al 5°
045500060922     c                   if        %eof(azorg02l) or orgfl3 > '5'
045600060922     c                   leave
045700060922     c                   endif
045800060922      * solo le filiali
045900060922     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
046000060922     c                   iter
046100060922     c                   endif
046200060922      * selezionato distretto
046300060922     c                   if        v1cdir <> *blank and v1cdir = orgfl3
046400060922     c                   movel     orgfil        v1cfil
046500060922     c                   exsr      aitra
046600060922     c                   endif
046700060922      * selezionata area
046800060922     c                   move      orgcar        orgcara           3
046900060922     c                   if        v1care <> *blank and v1care = orgcara
047000060922     c                   movel     orgfil        v1cfil
047100060922     c                   exsr      aitra
047200060922     c                   endif
047300060922     c                   enddo
047400060922     c                   endsr
047500011026     c*-------------------------------------------------
047600011026     c     *inzsr        begsr
047700011026     c*-------------------------------------------------
047800020304     c     *entry        plist
047900020304     c                   parm                    kpjba
048000011026
048100060921     c     ktab          klist
048200060921     c                   kfld                    kkut              1 0
048300060921     c                   kfld                    kcod              2
048400060921     c                   kfld                    kkey              8
048500060921     c                   z-add     1             kkut
048600060921     c                   move      '05'          kcod
049100060928
049200020218     C                   time                    w0120            14 0
049300020218     C                   move      w0120         wdat              8 0
049400030630     c                   move      wdat          dataeur
049500030630     c                   move      dataeur       dataiso
049600030630     c                   move      dataiso       wdatiso
049700030630
049800020404     c                   z-add     99            line
049801170810      *
050801170810     c                   exsr      repdatiute
050802170810     c                   move      dutpou        v1cfil
051000031105     c     v1cfil        chain     azorg01l
051100031105     c                   if        %found(azorg01l)
051200031105     c                   movel     orgdes        v1dfil
051300031105     c                   endif
051301170810      * verifica se profili di filiale con autorizzazione a area o distretto
051302170810      * oppure se profilo di sede nel caso abilita selezione area e distretto
051303170810     c                   if        wabi = 'RA' or wabi = 'DI' or
051304170810     c                             wabi = 'AZ'
051305170810     c                   seton                                        89
051306170810     c                   else
051307170810      * potrebbe avere abilitazione come terminal quindi non proteggo il po inserimento
051308170810     c                   if        wabi ='PO'
051309170810     c                   seton                                        88
051310170810     c                   endif
051311170810     c                   endif
052100011026     c                   endsr
052101170810     C*--------------------------------------------------------------------------------------------*
052102170810     C* REPERISCE I DATI UTENTE
052103170810     C*--------------------------------------------------------------------------------------------*
052104170810     C     REPDATIUTE    BEGSR
052105170810     C*
052106170810     C* INIZIALIZZA VARIABILI DI WRK
052107170810     C                   CLEAR                   TIBS34DS
052108170810     C                   CLEAR                   AZUTEDS
052109170810     C                   CLEAR                   DDATIUTE
052110170810     C*
052111170810     C     *DTAARA       DEFINE    §azute        azuteds
052112170810     C     *DTAARA       DEFINE    §datiute      ddatiute
052113170810     C                   IN(E)     *DTAARA
052114170810     C                   IF        %Error
052115170810     C                   EVAL      I34Tla = 'L'
052116170810     C                   CALL      'TIBS34R'
052117170810     C                   PARM                    Tibs34Ds
052118170810     C                   IN        *DTAARA
052119170810     C                   ENDIF
052120170810     c                   Clear                   wabi
052121170810     c                   Clear                   dLat
052122170810
052123170810      * Verifica errori e autorità profilo
052124170810s   1c                   Select
052125170810      * se ho errori nei dati utente esco dal pgm
052126170810w   1c                   When      DutErr = 'E'
052127170810     c                   seton                                        lr
052128170810     c                   return
052129170810      * se non c'è l'abilitazione
052130170810      * --> se 1° livello, abilitazioni al terminal
052131170810      *     se 2° livello, abilitazioni al punto operativo
052132170810w   1c                   When      UteAut = *Blanks
052133170810if  2c                   If        DutLpo = '1'
052134170810     c                   Eval      wabi   = 'TP'
052135170810e   2c                   EndIf
052136170810if  2c                   If        DutLpo = '2'
052137170810     c                   Eval      wabi   = 'PO'
052138170810e   2c                   EndIf
052139170810if  2c                   If        DutLpo = 'S'
052140170810     c                   Eval      wabi   = 'AZ'
052141170810e   2c                   EndIf
052142170810      * carica le abilitazioni del profilo
052143170810x   1c                   Other
052144170810     c                   Movel     UteFaf        Dute01
052145170901     c                   If        §Utecerv <> *Blanks
052148170810     c                   Eval      wabi = §utecerv
052149170810     c                   EndIf
052150170810e   1c                   EndSl
052151170810      * controllo se ok l'abilitazione dell'utente
052152170810     c                   Clear                   Tibs02ds
052153170810     c                   Eval      T02Mod = 'C'
052154170810     c                   Eval      T02Sif = knsif
052155170810     c                   Eval      T02Cod = 'LAT'
052156170810     c                   Movel(p)  wabi          T02Ke1
052157170810     c                   Call      'TIBS02R'
052158170810     c                   Parm                    kpjba
052159170810     c                   Parm                    Tibs02ds
052160170810if  1c                   If        T02Err = *Blanks
052161170810     c                   Eval      dLat = T02Uni
052162170810e   1c                   EndIf
052163170810      * errore o non abilitato (impossibile xchè controllato nel filtro)
052164170810if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
052165170810     c                             wabi = 'NO' or wabi = *blank
052166170810     c                   seton                                        98
052167170810     c                   call      'XUTENOAB'
052168170810     c                   parm                    kpjba
052169170810     c                   seton                                        lr
052170170810     c                   return
052171170810   x1c                   endif
052172170810      * Reperimento dei P.O. gestibili dall'utente
052173170810     c                   clear                   TRUL31DS
052174170810     c                   clear                   TRUL31DS2
052175170810     c                   eval      I31abi = wabi
052176170810     c                   eval      I31cdi = DUTdis
052177170810     c                   eval      I31car = DUTare
052178170810     c                   eval      I31cpo = DUTpou
052179170810     c                   call      'TRUL31R'
052180170810     c                   parm                    KPJBA
052181170810     c                   parm                    TRUL31DS
052182170810     c                   parm                    TRUL31DS2
052183170810if  2c                   if        O31pog > *zeros
052184170810     c                   movea     O31pog        $pog
052185170810     c                   movea     O31arg        $arg
052186170810     c                   movea     O31dig        $dig
052187170810x   2c                   else
052188170810     c* impossibile perchè controllato nel filtrp
052189170810     c                   seton                                        lr
052190170810     c                   return
052191170810    1c                   endif
052192170810     c                   do        250           g                 3 0
052193170810     c                   if        $pog(g) > *zero
052194170810     c                   move      $pog(g)       $pogn(g)
052195170810     c                   else
052196170810     c                   move      999           $pogn(g)
052197170810     c                   endif
052198170810     c                   enddo
052199170810     c                   endsr
052200020404     C*--------------------------------------------------------------------
052300031028     Oprtf198   E            testata        1 02
052400030903     o                                           10 'TRMZ72R'
052500020404     o                       knmus               22
052600030903     o                                        +   1 'Lista automezzi città conf-
052700030903     o                                              ormi da ripristinare'
052800030630     o                       udate         y  +   1
052900020404     O                       page          z  +   1
053000040503     O          E    81      testata     1  1
053100040503     o                                           30 'Elenco con opzione 1'
053200040503     o                                        +   1 'Periodo'
053300040503     o                                        +   1 'Dal'
053400040503     o                       v1cdal           +   2 '  /  /    '
053500040503     o                                        +   2 'al'
053600040503     o                       v1cal            +   2 '  /  /    '
053700040503     O          E    82      testata     1  1
053800040503     o                                           30 'Elenco con opzione 2'
053900040503     o                                        +   1 'Data ispezione impostata '
054000040503     o                                        +   1 'e Data'
054100040503     o                                        +   1 'Ripristino non impostata'
054200040610     O          E    83      testata     1  1
054300040610     o                                           30 'Elenco con opzione 3'
054400040610     o                                        +   1 'Data ispezione'
054500040610     o                                        +   1 'Dal'
054600040610     o                       v1cdis           +   2 '  /  /    '
054700040610     o                                        +   2 'al'
054800040610     o                       v1cais           +   2 '  /  /    '
054900020404     O          E            testata     1  1
055000030909     o                                           20 'Autotrasportatore'
055100030909     o                                           45 'Società'
055200030909     o                                           65 'Targa'
055300040518     o                                           75 'P.O'
055400040518     o                                           80 'Pub'
055500031028     o                                           92 'Data Ispe.'
055600030909     o                                          104 'Prev.Ripr.'
055700030909     o                                          116 'Prev.Rip2.'
055800031028     o                                          130 'Note'
055900020404     O          E            detta       1
056000030909     o                       traaut              25
056100030909     o                       trasoc              52
056200030909     o                       trataa              67
056300040518     o                       trafil              75
056400040518     o                       trapub              79
056500031028     o                       tradrrs       8     92
056600030909     o                       tradprs       8    104
056700030909     o                       tradp2s       8    116
056800031028     o                       tranrp             190
056900040503     O          E            detta2      1
057000040503     o                       trant2             190
057100031105     O          E            filiale     1
057200031105     o                                           25 'T o t a l e  p e r  P.O'
057300031105     o                       exfil               30
057400061019     o                       exfild              52
057500061019     o                       totfil        z     62
057600061019     O          E            filiale     0
057700061019     o                                           25 'T o t a l e  p e r  P.O'
057800061019     o                       exfil               30
057900061019     o                       exfild              52
058000061019     o                       totfil        z     62
058100061019     O          E            filiale        1
058200061019     o                                           25 'T o t a l e  p e r  P.O'
058300061019     o                       exfil               30
058400061019     o                       exfild              52
058500061019     o                       totfil        z     62
058600031105     O          E            globale     1
058700031105     o                                           30 'T o t a l e  G e n e r a -
058800031105     o                                              l e'
058900061019     o                       totale        z     62
059000020404     O          E            noncene     3
059100020404     o                                           80 'N O N   E S I S T O N O   -
059200030903     o                                              A U T O M E Z Z I   D A  R -
059300030903     o                                              I P R I S T I N A R E'
059400060928      *___________________________________________________________________
059500060928** ERR
059600060928Profilo di area non può selezionare distretto                                  1
059700060928Il distretto richiesto è incongruente con il profilo                           2
059800170810Area richiesta è incongruente con il profilo                                   3
059900170810Il P.O richiesto non è abilitato per l'utente                                  4
060000060928Selezionare solo uno fra P.O / Area / Distretto                                5
060100060928L'area non è di competenza al profilo
060200060928L'area selezionata non appartiene al distretto di competenza
