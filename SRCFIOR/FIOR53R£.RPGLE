000100001031     H DECEDIT('0,') DATEDIT(*YMD.)
000200001031
000300001031      *****************************************************************
000400001031      *                                                               *
000500001031      *       STAMPA ORDINI RITIRO MERCE                              *
000600001031      *                                                               *
000700001031      *****************************************************************
000800001031
000900001031      ****************************************************************
001000001031      *  RIEPILOGO INDICATORI
001100001031      ****************************************************************
001200001031      * 05 - SPROTEGGO IL PUNTO OPERATIVO CLIENTE
001300001031      * 06 - SONO P.O. REMOTO : NON ABILITO F18
001400010503      * 28 - ERRORE GENERICO DSPF
001500010503      * 40 - ERRORE PUNTO OPERATIVO
001600010503      * 41 - ERRORE Data Distinta
001700010221      * 42 - ERRORE barcode
001800010504      * 43 - ERRORE già assegnati
001900010503      * 44 - ERRORE DATA RITIRO
002000010419      * 45 - ERRORE Distinta
002100010920      * 46 - ERRORE DATA stampa
002200010503      * 90 - Riemissione videata
002300001031      ****************************************************************
002400001031
002500960924     FAZORG01L  IF   E           K DISK
002600010419     FFNFVV01L  IF   E           K DISK
002700120612     Ffidst01l  IF   E           K DISK
002800100927     fTABEL00F  if   e           k disk
002900001031     FFIOR08D   CF   E             WORKSTN
003000001031
003100010419     D kfvvnpg         s                   like(fvvnpg)
003200010419     D kfvvnfv         s                   like(fvvnfv)
003300010419     D kfvvfgs         s                   like(fvvfgs)
003400120612     D kdstnfv         s                   like(dstnfv)
003500010419
003600010221     D sav_v1cpor      S                   LIKE(v1cpor)
003700010314
003800001031      *   S C H I E R E
003900070926     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             MSG VIDEO
004000960924     D L1              S              3  0 DIM(30)                              FIL GESTITE
004100001031
004200001031      *   D S   I N T E R N E / E S T E R N E
004300010503
004400010503      * Scomposizione Descrizione Distinta
004500010503     D                 DS
004600010503     D  v1dndc                 1     40
004700010503     D  wdel                   1      4
004800010503     D  wggg                   5      6
004900010503     D  wbarra1                7      7
005000010503     D  wmmm                   8      9
005100010503     D  wbarra2               10     10
005200010503     D  waaa                  11     15
005300010503     D  wpdr                  16     23
005400010503     D  wdescr                24     40
005500001031
005600001106     D WLBDAT          DS                  INZ
005700001106     D  G02DAT                 1      8  0
005800001106     D  G02INV                 9     16  0
005900001106     D  G02ERR                17     17
006000001106     D  G02TGI                18     22  0
006100031201      *
006200031125     D FIOR16DS      e ds                  inz
006300001031
006400001031      * DS PER TRUL06R - CARICAMENTO £X
006500001031     D TRUL06DS      E DS
006600001102     D  lin                    1     90  0 DIM(30)                              FIL.COMODO
006700001031
006800030613      * DS PER TRUL90R - RICHIESTA STAMPANTI
006900030613     d Trul90Ds      e ds
007000100927     d ds5P          e ds                  inz
007100100927     d   §5Ppgm      e                     inz('FIOR18R   ')
007200100927
007300100927      * - Parametri x Controllo profilo utenti
007400100927     d TIBS34ds      e ds
007500100927      * - Ds di riferimento al file esterno AZUTE00F
007600100927     d AzuteDs       e ds                  extname(AZUTE00F)
007700100927      * - Ds per dati organigramma
007800100927     d dDatiUte      e ds
007900001031
008000010503     D DSLR01        E DS                  inz
008100001031     D KPJBA         E DS
008200070921     d og147         e ds
008300001031
008400960923     D                SDS
008500960923     D  V1CPGM                 1     10
008600031201
008700031201      *   V A R I A B I L I
008800031201
008900031201     D Wdate           s              8  0
009000070926     d data_org        s              8  0 inz
009100001031
009200001031      ***************************************************************
009300001031      * RICHIESTA STAMPANTE
009400001031      ***************************************************************
009500001108
009600031125     c                   clear                   FIOR16ds
009700030613     C                   clear                   trul90ds
009800030613     C                   movel     'S'           D90rso
009900030613     C                   call      'TRUL90R'
010000010314     C                   parm                    kpjba
010100030613     C                   Parm                    trul90ds
010200001031
010300001102      * F3=Fine
010400030613     C                   if        d90f3 = '1'
010500010314     C                   goto      fine
010600001031     C                   endif
010700100927      /free
010800100927         reset ds5P;
010900100927         if  %subst(knmus : 1 : 3) = 'EDP';
011000100927           TBLkey = 'O' + 'EDP';
011100100927         else;
011200100927           TBLkey = 'O' + %editc( DUTpou : 'X' );
011300100927         endif;
011400100927         chain  ( CodUt : '5P' : TBLkey )  TABEL;
011500100927         if  %found( TABEL00F );
011600100927           ds5P = TBLuni;
011700100927         endif;
011800100927      /end-free
011900031125
012000031125     C                   movel     D90mdo        R16mdo
012100031125     C                   movel     D90pro        R16oqo
012200001221
012300010503     C                   move      'S'           v1tpela
012400010503     C                   time                    wtime            14 0
012500010503     C                   move      wtime         v1dtrit
012600010209     C                   move      'S'           v1gas
012700010419     C                   clear                   v1ndc
012800010419     C                   clear                   v1dndc
012900010503     C                   clear                   v1dtndc
013000010920     C                   clear                   v1dtstp
013100070921
013200070921      * se filiale già abilitata a GEO ORM non può utilizzare questa funzione
013300070921      * deve usare la nuova
013400070926      * data partenza procedura in organigramma
013500070926     c                   if        §ogddao > *zeros
013600070926     c                   move      §ogddao       data_org
013700070926     c                   if        *date >= data_org
013800070926     c                   eval      v1cmsg = msg(13)
013900070921     c                   eval      *in28 = *on
014000070921     c                   eval      *in40 = *on
014100070926     c                   endif
014200070921     c                   endif
014300001031
014400001031     C                   do        *hival
014500001031     C                   exfmt     fior081
014600010503     C                   setoff                                       2890
014700001031
014800001031      * F3  - Fine lavoro
014900001031     C   kc              leave
015000001031
015100001031      * F6  - Conferma
015200001031     C                   if        *inkf = *on
015300001031     C                   exsr      Sr_Contr
015400001031     C  n28              exsr      Sr_Conferma
015500001031     C  n28              leave
015600001031     C                   endif
015700001031
015800001031      * F18 - Cambio P.O. ritiro
015900001110     C                   if        *inks = *on
016000001110     C                   eval      *in05 = *on
016100001110     C                   iter
016200001110     C                   endif
016300001031
016400001031      * 05 on --> controllo P.O. ritiro
016500001031     C                   if        *in05 = *on
016600001031     C                   exsr      Sr_Contrpo
016700001031     C  n28              eval      *in05 = *off
016800010924     C   28              iter
016900001031     C                   endif
017000001031
017100001031      * Controllo
017200001031     C                   exsr      Sr_Contr
017300010503     C   90
017400010503     Cor 28              iter
017500001031
017600001031     C                   enddo
017700001031
017800001031     C     fine          tag
017900001031
018000001031     C                   eval      *inlr = *on
018100001031      **********************************************************************
018200001031      * CONTROLLO
018300001031      **********************************************************************
018400001031     C     Sr_Contr      begsr
018500010504
018600010920     C                   setoff                                       4446
018700001031
018800010503      * Controllo P.O. ritiro
018900001031     C                   exsr      Sr_Contrpo
019000010503      * Controllo data ritiro
019100010503     C                   if        v1dtrit > *zeros
019200010503     C                   exsr      Sr_DatRitiro
019300010503     C                   else
019400031125     C                   z-add     *zeros        R16dar
019500010920     C                   endif
019600010419      * Controllo distinta
019700010503     C                   if        v1ndc <> *blanks
019800010419     C                   exsr      Sr_Distinta
019900010419     C                   else
020000031125     C                   z-add     *zeros        R16ndc
020100010419     C                   endif
020200010503      * Controllo data distinta
020300010503     C                   if        v1dtndc > *zeros
020400010503     C                   exsr      Sr_DatDist
020500010503     C                   else
020600031125     C                   z-add     *zeros        R16ddc
020700010503     C                   endif
020800010921      * Controllo data stampa
020900010921      * solo per ristampa e se non hanno chiesto per distinta
021000010921     C                   if        v1tpela = 'R' and
021100010921     C                             v1ndc = *blanks and v1dtndc = *zeros
021200010921     C                   exsr      Sr_DatStampa
021300010921     C                   endif
021400010924      * La data di stampa va indicata solo se siamo in RISTAMPA
021500010924     C                   if        v1tpela = 'S' and v1dtstp <> *zeros
021600010924     C                   seton                                        46  28
021700010924     C                   movel     msg(11)       v1cmsg
021800010924     C                   goto      endcontr
021900010924     C                   endif
022000010924      * La data di ritiro va indicata solo se siamo in STAMPA
022100010924     C                   if        v1tpela = 'R' and v1dtrit <> *zeros
022200010924     C                   seton                                        44  28
022300010924     C                   movel     msg(12)       v1cmsg
022400010924     C                   goto      endcontr
022500010924     C                   endif
022600010503      * Almeno un scelta va fatta
022700010921     C                   select
022800010921     C                   when      v1tpela = 'S'
022900010921     C                   if        v1dtrit = *zeros
023000010503     C                             and (v1ndc = *blanks or v1ndc = *zeros)
023100010503     C                             and v1dtndc = *zeros
023200010503     C                   movel     msg(7)        v1cmsg
023300010504     C                   seton                                        44  28
023400010924     C                   goto      endcontr
023500010503     C                   endif
023600010921     C                   other
023700010921     C                   if        v1dtstp = *zeros
023800010921     C                             and (v1ndc = *blanks or v1ndc = *zeros)
023900010921     C                             and v1dtndc = *zeros
024000010921     C                   movel     msg(7)        v1cmsg
024100010921     C                   seton                                        46  28
024200010924     C                   goto      endcontr
024300010921     C                   endif
024400010921     C                   endsl
024500010503      * Troppe scelte
024600010503     C                   if        (v1dtrit <> *zeros and v1ndc <> *blanks) or
024700010921     C                             (v1dtrit <> *zeros and v1dtndc <> *zeros)or
024800010921     C                             (v1dtrit <> *zeros and v1dtstp <> *zeros)
024900010920     C                   movel     msg(8)        v1cmsg
025000010920     C                   seton                                        44  28
025100010920     C                   goto      endcontr
025200010920     C                   endif
025300010920     C                   if        (v1dtstp <> *zeros and v1ndc <> *blanks) or
025400010920     C                             (v1dtstp <> *zeros and v1dtndc <> *zeros)
025500010503     C                   movel     msg(8)        v1cmsg
025600010920     C                   seton                                        46  28
025700010920     C                   goto      endcontr
025800010503     C                   endif
025900010920     C                   if        (v1ndc <> *blanks  and v1dtndc <> *zeros)
026000010920     C                   movel     msg(8)        v1cmsg
026100010920     C                   seton                                        45  28
026200010920     C                   goto      endcontr
026300010920     C                   endif
026400001031
026500010920     C     endcontr      endsr
026600001031      **********************************************************************
026700001031      * CONTROLLO IL P.O. IN IN GESTIONE
026800001031      **********************************************************************
026900001031     C     Sr_Contrpo    begsr
027000001031
027100001031     C                   setoff                                       40
027200001031     C                   clear                   v1dpor
027300001031
027400001031     C                   if        v1cpor = *zeros
027500001031     C                   movel     msg(2)        v1cmsg
027600001031     C                   seton                                        40  28
027700001031     C                   goto      endcontrpo
027800001031     C                   endif
027900001031
028000001031     C     v1cpor        lookup    L1                                     30
028100001031     C     *in30         ifeq      *off
028200001031     C                   seton                                        40  28
028300001031     C                   movel     msg(3)        v1cmsg
028400001031     C                   goto      endcontrpo
028500001031     C                   endif
028600031201
028700001031
028800001031      * Decodifica
028900070921     c                   clear                   og147
029000001031     C     v1cpor        chain     azorg01L
029100001031     C                   if        %found(azorg01l) and ORGfva = *blanks
029200010209     C                   movel     orgDES        v1dpor
029300070921     c                   eval      og147 = orgde7
029400001031     C                   else
029500001031     C                   seton                                        40  28
029600001031     C                   movel     msg(2)        v1cmsg
029700001031     C                   endif
029800070921
029900070921      * se filiale già abilitata a GEO ORM non può utilizzare questa funzione
030000070921      * deve usare la nuova
030100070926      * data partenza procedura in organigramma
030200070926     c                   if        §ogddao > *zeros
030300070926     c                   move      §ogddao       data_org
030400070926     c                   if        *date >= data_org
030500070926     c                   eval      v1cmsg = msg(13)
030600070921     c                   eval      *in28 = *on
030700070921     c                   eval      *in40 = *on
030800070926     c                   endif
030900070921     c                   endif
031000001031
031100001031     C     endcontrpo    endsr
031200001031      **********************************************************************
031300010503      * CONTROLLO DATA RITIRO
031400001031      **********************************************************************
031500010503     C     Sr_DatRitiro  begsr
031600010920
031700010920     C                   setoff                                       4443
031800031125     C                   clear                   R16dar
031900010920
032000010920     C                   clear                   wlbdat
032100010920     C                   z-add     v1dtrit       G02dat
032200010920     C                   call      'XSRDA8'
032300010920     C                   parm                    wlbdat
032400010920     C                   if        G02err = '1'
032500010920     C                   movel     msg(1)        v1cmsg
032600010920     C                   seton                                        44  28
032700010920     C                   else
032800031125     C                   z-add     G02inv        R16dar
032900010920     C                   z-add     G02dat        v1dtrit
033000010920     C                   endif
033100010920
033200010920     C                   if        v1dtrit <> *zeros and v1gas = *blanks
033300010920     C                   movel     msg(9)        v1cmsg
033400010920     C                   seton                                        43  28
033500010920     C                   endif
033600001031
033700001031     C                   endsr
033800010920      **********************************************************************
033900010920      * CONTROLLO DATA STAMPA
034000010920      **********************************************************************
034100010920     C     Sr_DatStampa  begsr
034200010920
034300010920     C                   setoff                                       46
034400031125     C                   clear                   R16dst
034500010920
034600010920      * la data di stampa va indicata solo se sono in ristampa
034700010920     C                   if        v1tpela = 'S'
034800010920     C                             and v1dtstp > *zeros
034900010920     C                   seton                                        46  28
035000010920     C                   movel     msg(11)       v1cmsg
035100010920     C                   goto      endcontrstp
035200010920     C                   endif
035300010921      * controllo la validità della data
035400010920     C                   if        v1tpela = 'R'
035500010921     C                             and v1dtstp > *zeros
035600010920     C                   clear                   wlbdat
035700010920     C                   z-add     v1dtstp       G02dat
035800010920     C                   call      'XSRDA8'
035900010920     C                   parm                    wlbdat
036000010920     C                   if        G02err = '1'
036100010920     C                   movel     msg(10)       v1cmsg
036200010920     C                   seton                                        46  28
036300010920     C                   else
036400031125     C                   z-add     G02inv        R16dst
036500010920     C                   z-add     G02dat        v1dtstp
036600010920     C                   endif
036700010920     C                   endif
036800010920
036900010920     C     endcontrstp   endsr
037000010419      **********************************************************************
037100010419      * CONTROLLO DISTINTA
037200010419      **********************************************************************
037300010419     C     Sr_Distinta   begsr
037400010503
037500010503     C                   setoff                                       45
037600010503     C                   clear                   v1dndc
037700031125     C                   clear                   R16ndc
037800010419
037900010420      *    Ricerco Distinta
038000010503     C     '?'           scan      v1ndc                                  30
038100010503     C   30              do
038200010503     C                   clear                   v1ndc
038300010503     C                   clear                   v1dndc
038400010503     C                   clear                   dslr01
038500010503     C                   movel     'S'           dlrric
038600010503     C                   movel     '4'           dlrnpg
038700010503     C                   movel     '2'           dlrtfv
038800010420     C                   movel     'FIOR08R'     dlrpgm
038900010503     C                   movel     v1cpor        dlrfgs
039000010503     C                   movel     dslr01        kpjbu
039100010503     C                   call      'FNLR02R'
039200010503     C                   parm                    kpjba
039300010503     C                   movel     kpjbu         dslr01
039400010420     C     dlrnfv        ifeq      *zeros
039500010503     C                   clear                   v1ndc
039600010420     C                   else
039700010503     C                   move      *all'0'       v1ndc
039800010503     c                   move      dlrnfv        v1ndc
039900010420     C                   endif
040000010503     C     v1ndc         ifne      *blanks
040100010420     C                   move      '4'           kfvvnpg
040200010503     C                   move      v1ndc         kfvvnfv
040300010503     C                   move      v1cpor        kfvvfgs
040400010420     C     kfnfvv        chain     fnfvv01l
040500010420     C                   if        %found(fnfvv01l)
040600010420     C                   EXSR      SR_DESCRIZ
040700010420     C                   endif
040800010420     C                   endif
040900010503     C                   seton                                        90  45
041000010503     C                   goto      endsrdist
041100010503     C                   enddo
041200010420
041300010420      *    Controllo numero Distinta
041400010503     C                   clear                   v1dndc
041500010503     C     v1ndc         ifeq      *all'0'
041600010503     C                   clear                   v1ndc
041700010420     C                   endif
041800010503     C     v1ndc         ifne      *blanks
041900010420     C                   move      '4'           kfvvnpg
042000010503     C                   move      v1ndc         kfvvnfv
042100010503     C                   move      v1cpor        kfvvfgs
042200010420     C     kfnfvv        chain     fnfvv01l
042300010420     C                   if        %found(fnfvv01l)
042400010420     C     fvvatb        ifne      *blanks
042500010420     C                   eval      *in28 = *on
042600010503     C                   eval      *in45 = *on
042700010503     C                   movel     msg(5)        v1cmsg
042800010503     C                   goto      endsrdist
042900010420     C                   else
043000010420      * Chiusa
043100010420     C     fvvfcf        ifne      ' '
043200010420     C                   eval      *in28 = *on
043300010503     C                   eval      *in45 = *on
043400010503     C                   movel     msg(6)        v1cmsg
043500010503     C                   goto      endsrdist
043600010420     C                   endif
043700010420     C                   EXSR      SR_DESCRIZ
043800010420     C                   endif
043900010420     C                   else
044000010420     C                   eval      *in28 = *on
044100010503     C                   eval      *in45 = *on
044200010503     C                   movel     msg(5)        v1cmsg
044300010503     C                   goto      endsrdist
044400010420     C                   endif
044500010420     C                   endif
044600010503
044700040203     C                   move      v1ndc         R16ndc
044800010420
044900010420     C     endsrdist     endsr
045000010503      **********************************************************************
045100010503      *    Recupero Descrizione Distinta
045200010503      **********************************************************************
045300010503     C     SR_DESCRIZ    begsr
045400010503
045500120612     c                   eval      kdstnfv = kfvvnfv
045600120612     C     kfidst        chain     fidst01l
045700120612     C                   if        %found(fidst01l)
045800010503     C                   movel     'DEL '        wdel
045900010503     C                   move      fvvdfv        wggg
046000010503     C                   move      '/'           wbarra1
046100010503     C                   move      '/'           wbarra2
046200010503     C                   move      fvvdfv        wa4               4
046300010503     C                   movel     wa4           wmmm
046400010503     C                   movel     fvvdfv        wa4               4
046500010503     C                   movel(p)  wa4           waaa
046600010503     C     dstpdr        ifne      0
046700010503     C                   movel(p)  dstpdr        wpdr
046800010503     C                   else
046900010503     C                   clear                   wpdr
047000010503     C                   endif
047100010503     C                   endif
047200010503
047300010503     C                   endsr
047400010503      **********************************************************************
047500010503      * CONTROLLO DATA DISTINTA
047600010503      **********************************************************************
047700010503     C     Sr_DatDist    begsr
047800010503
047900010503     C                   setoff                                       41
048000031125     C                   clear                   R16ddc
048100010503
048200010503     C                   clear                   wlbdat
048300010503     C                   z-add     v1dtndc       G02dat
048400010503     C                   call      'XSRDA8'
048500010503     C                   parm                    wlbdat
048600010503     C                   if        G02err = '1'
048700010503     C                   movel     msg(1)        v1cmsg
048800010503     C                   seton                                        41  28
048900010503     C                   else
049000031125     C                   z-add     G02inv        R16ddc
049100010503     C                   z-add     G02dat        v1dtndc
049200010503     C                   endif
049300010503
049400010503     C                   endsr
049500001031      **********************************************************************
049600001031      * F6 - CONFERMA
049700001031      **********************************************************************
049800001031     C     Sr_Conferma   begsr
049900001031
050000031125     c                   eval      R16tla = 'L'
050100031125     C                   z-add     v1cpor        R16por
050200031125     C                   movel     v1gas         R16ass
050300031125     C                   movel     v1tpela       R16ris
050400031125     C                   movel(p)  FIOR16DS      kpjbu
050500100927     c                   call      §5Ppgm
050600100927     C                   parm                    KPJBA
050700100927     C                   movel     KPJBU         FIOR16DS
050800001031
050900001031     C                   endsr
051000001031      *****************************************************************
051100001031      * ROUTINE INIZIALE
051200001031      *****************************************************************
051300960923     C     *INZSR        BEGSR
051400001031
051500960923     C     *ENTRY        PLIST
051600960923     C                   PARM                    KPJBA
051700010419
051800010419     C     KFNFVV        KLIST
051900010419     C                   kfld                    kfvvnpg
052000010419     C                   kfld                    kfvvnfv
052100010419     C                   kfld                    kfvvfgs
052200010503
052300120612     C     Kfidst        KLIST
052400120612     c                   kfld                    kfvvnpg
052500120612     C                   kfld                    kdstnfv
052600120612     C                   kfld                    kfvvfgs
052700031201
052800031201      * reperisco data e ora
052900031201     c                   time                    wtime
053000031201     c                   move      wtime         G02dat
053100031201     c                   clear                   G02err
053200031201     c                   call      'XSRDA8'
053300031201     c                   parm                    WLBDAT
053400031201     c                   movel     G02inv        Wdate
053500001031
053600100927     C                   Z-ADD     1             CODUT             1 0
053700100927     c     *dtaara       define    §azute        AZUTEds
053800100927     c     *dtaara       define    §datiute      dDATIUTE
053900100927     c                   in(E)     *dtaara
054000100927     c                   if        %ERROR or RSUT = *blanks
054100100927     c                   clear                   Tibs34Ds
054200100927     c                   call      'TIBS34R'
054300100927     c                   parm                    Tibs34Ds
054400100927     c                   in        *dtaara
054500100927     c                   endif
054600100927     C                   MOVEL     Rsut          V1CRSU
054700001110
054800001110      * IMPOSTO IL P.O. DI GESTIONE
054900100927     C                   if        dutlpo = '2' or dutlpo = *blanks
055000100927     C                   movel     dutpou        V1Cpor
055100001110     C                   eval      *in06 = *on
055200001110     C                   else
055300001110     C                   movel     SIMFEL        V1Cpor
055400001110     C                   endif
055500010221     C                   eval      sav_v1cpor = v1cpor
055600001220
055700031125     C                   clear                   FIOR16DS
055800070921
055900070921     c                   clear                   og147
056000001220     C     v1cpor        chain     azorg01l
056100010209     C                   if        %found(azorg01l)
056200001220     C                   movel     ORGdes        v1dpor
056300070921     c                   eval      og147 = orgde7
056400001220     C                   endif
056500001031
056600001031      * CARICO TABELLA FILIALI GESTITE £1
056700001102     C                   clear                   trul06ds
056800001031     C                   move      '£1'          D06cod
056900001102     C                   movel     simfel        D06key
057000001102     C                   movel     trul06ds      kpjbu
057100001031
057200001031     C                   call      'TRUL06R'
057300001102     C                   parm                    kpjba
057400001102     C                   movel     kpjbu         trul06ds
057500001102     C                   movea     lin           l1
057600001031
057700001031     C                   endsr
057800001031      *****************************************************************
057900961025** MSG
058000010920Data ritiro errata                                                             1
058100070418Filiale ritiro errata                                                          2
058200070418Filiale ritiro errata: in gestione ad altra filiale                            3
058300010221Non è prevista una stampante con barcode                                       4
058400010503Numero Distinta Errato                                                         5
058500010503Distinta non aperta                                                            6
058600010503Fare una scelta!!!!                                                            7
058700010503Fare SOLO una scelta!!!!                                                       8
058800010504Immettere flag già assegnati s/n                                               9
058900010920Data stampa errata                                                            10
059000010920Data stampa valida solo se richiesta la ristampa                              11
059100010924Data ritiro valida solo se richiesta la stampa                                12
059200070926Automazione distinte ORM attiva. Non utilizzare questa funzione!!             13
