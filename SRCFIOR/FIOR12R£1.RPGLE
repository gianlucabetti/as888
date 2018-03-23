000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500010108      *       INTERROGAZIONE PROPOSTE VARIAZIONE ORM                  *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200001031      * 03 - Visualizza analisi proposta nel subfile proposte
001300010108      * 05 - SPROTEGGO P.O. IN GESTIONE
001400001010      * 28 - ERRORE GENERICO DSPF
001500001010      * 30 - Comodo
001600010108      * 40 - ERRORE p.o. in gestione
001700010109      * 41 - ERRORE Numero ORM
001800010109      * 44 - ERRORE P.O.emissione
001900010109      * 46 - ERRORE Data variazione da
002000010109      * 51 - ERRORE Data variazione a
002100010205      * 91 - riemissione videata
002200001010      ****************************************************************
002300001010
002400001031     FFNORP01L  IF   E           K DISK
002500010108     FFNORM01L  IF   E           K DISK
002600001011     FAZORG01L  IF   E           K DISK
002700010108     FFIOR12D   CF   E             WORKSTN  sfile(fior12s:nrr)
002800001010
002900001010      *------------------------------------------------------------------------*
003000010108     D kpoe            S                   LIKE(V1cfgs)
003100001016     D knsr            S                   LIKE(V1nsr)
003200001006     D knor            S                   LIKE(V1nor)
003300001016     D knrv            S                   LIKE(V1nrv)
003400001103     D kdtv            S                   LIKE(ORPdtv)
003500001103     D korv            S                   LIKE(ORPorv)
003600001026
003700001026     D kazorg          S                   LIKE(ORGfil)
003800001030
003900010108     D vdvda           S                   LIKE(v1dvda)
004000010108     D vdva            S                   LIKE(v1dva)
004100010108
004200010108     D §doggi          S                   LIKE(v1dvda)
004300001025     D §kpjbu          S                   LIKE(KPJBU)
004400010108     D §cpoe           S                   LIKE(ORMpoe)
004500001009
004600001006     D W0140           S             14  0
004700001006     D Wdtgio          S              8  0
004800001006     D dateu           S              8  0
004900001009
005000001006     D DATA_oggi       S               D   DATFMT(*eur)
005100001006
005200001010      *  titolo videata (lunghezza massima 34)
005300010108     D TIT_A           C                   CONST('***  INT.PROPOSTE VARIAZIONE -
005400001010     D                                       ***')
005500001031
005600001031      *   S C H I E R E
005700010108     D MSG             S             78    DIM(17) CTDATA PERRCD(1)             MSG VIDEO
005800010108     D L1              S              3  0 DIM(30)                              P.O. Gestiti
005900010205     D L6              S              3  0 DIM(30)                              P.O. Gestiti
006000001006
006100001010      *   D S   I N T E R N E / E S T E R N E
006200001009
006300001009     D WLBDAT          DS                  INZ
006400001009     D  G02DAT                 1      8  0
006500001009     D  G02INV                 9     16  0
006600001009     D  G02ERR                17     17
006700001009     D  G02TGI                18     22  0
006800001031
006900001009     D Parm01          DS
007000001016     D  ppoe                          3  0
007100001016     D  pnor                          7  0
007200001016     D  pnsr                          2  0
007300001016     D  pnrv                          2  0
007400001016     D  psce                          1
007500001018     D  pfgs                          3  0
007600010205     D  ppor                          3  0
007700010205     D  pdtr                          8  0
007800010205     D  pmdb                         10
007900010205     D  pprb                         10
008000010205     D  pdts                          8  0
008100010205     D  prmp                          1
008200010205     D  pbrc                          1
008300010205     D  pref                          2
008400010205     D  pmio                          1
008500010205
008600010109     D  pdtv                          8  0
008700010109     D  porv                          6  0
008800010109     D  putv                         10
008900010109     D  pric                          1
009000001009
009100001010     D TIBS02DS      E DS
009200001016     D UT§DSE0F      E DS
009300001016     D CNCR80        E DS
009400010108
009500010108     D TRUL06DS      E DS
009600010108     D  LIN                    1     90  0
009700010108     D                                     DIM(30)
009800001010
009900001010      *
010000001010     D KPJBA         E DS
010100020502     D* rem                    1      3
010200020502     D* remfil                 4      6
010300001010      *
010400001010     D                SDS
010500001010     D  VTCPGM                 1     10
010600001010
010700001006      *------------------------------------------------------------------------*
010800010108
010900010108      * Emissione 1° Videata
011000010108
011100010108     C                   do        *hival
011200010108      * Pulizia 1° Videata
011300010205     C                   if        *in28 = *off  and  *in05 = *off
011400010205     C                             and *in90 = *off
011500010108     C                   exsr      Sr_Pulvid1
011600010108     C                   endif
011700010205
011800010205      * Azzero il numero ORM
011900010205     C                   if        *in91 = *on
012000010205     C                   clear                   v1cpoe
012100010205     C                   clear                   v1nor
012200010205     C                   endif
012300010108
012400010108     C                   exfmt     fior121
012500010108
012600010108      * F3=Fine
012700010108     C   kc              leave
012800010205     C                   setoff                                       289091
012900010108
013000010108      * F18=Cambio P.O. in gestione
013100010108     C   ks              eval      *in05 = *on
013200010108     C   ks              iter
013300010108
013400010108      * 05 on --> controllo P.O. in gestione
013500010108     C                   if        *in05 = *on
013600010108     C                   exsr      Sr_Contrpo
013700010109     C  n28              eval      *in05 = *off
013800010108     C                   endif
013900010108
014000010108      * Controlli 1° videata
014100010108     C                   exsr      Sr_Contr01
014200010108
014300010108     C   28              iter
014400010205     C   91              iter
014500010108
014600010108     C                   movel     v1cfgs        §cpoe
014700010108      * Gestione subfile
014800010108     C                   exsr      Sr_Gessfl
014900010109     C                   setoff                                       28
015000010108     C                   if        *inkc = *on
015100010108     C                   leave
015200010108     C                   endif
015300010108
015400010108     C                   enddo
015500001010
015600001006     C                   eval      *inlr = *on
015700010108      *****************************************************************
015800010108      * PULIZIA 1° VIDEATA
015900010108      *****************************************************************
016000010108     C     Sr_Pulvid1    BEGSR
016100010108
016200010108     C                   clear                   v1cpoe
016300010108     C                   clear                   v1nor
016400010108     C                   clear                   v1nsr
016500010108     C                   clear                   v1nrv
016600010108     C                   clear                   v1dvda
016700010108     C                   movel     data_oggi     v1dva
016800010109     C                   movel     'NO'          v1sino
016900010108
017000010108     C                   endsr
017100010108      **********************************************************************
017200010108      * CONTROLLO IL P.O. IN IN GESTIONE
017300010108      **********************************************************************
017400010108     C     Sr_Contrpo    begsr
017500010108
017600010108     C                   setoff                                       40
017700010108     C                   clear                   v1dfgs
017800010108
017900010108     C     v1cfgs        lookup    L1                                     30
018000010108     C     *in30         ifeq      *off
018100010108     C                   seton                                        40  28
018200010108     C                   movel     msg(12)       v1cmsg
018300010108     C                   else
018400010108
018500010108      * Decodifica
018600010108     C     v1cfgs        chain     azorg01L
018700010108     C                   if        %found(azorg01l) and ORGfva = *blanks
018800010108     C                   movel     orgDES        v1dfgs
018900010108     C                   else
019000010108     C                   seton                                        40  28
019100010108     C                   movel     msg(12)       v1cmsg
019200010108     C                   endif
019300010207
019400010207      * Devo ricaricare la £6
019500010207     C                   CLEAR                   trul06ds
019600010207     C                   MOVE      '£6'          D06COD
019700010207     C                   MOVEL     v1cfgs        D06KEY
019800010207     C                   MOVEL     trul06ds      KPJBU
019900010207     C                   CALL      'TRUL06R'
020000010207     C                   PARM                    KPJBA
020100010207     C                   MOVEL     KPJBU         trul06ds
020200010207     C                   MOVEA     LIN           L6
020300010108     C                   endif
020400010108
020500010108     C                   endsr
020600010108      **********************************************************************
020700010108      * CONTROLLI VIDEO 1
020800010108      **********************************************************************
020900010108     C     Sr_Contr01    begsr
021000010108
021100010108      * Reset indicatori
021200010108
021300010109     C                   setoff                                       414446
021400010109     C                   setoff                                       51
021500010108
021600010108     C                   if        v1nor <> *zeros
021700010108     C                   if        v1cpoe > *zeros
021800010108     C                   exsr      Sr_Contrnor                                  controllo num.ORM
021900010108     C   28              goto      endsrcontr
022000010108     C                   else
022100010108     C                   movel     msg(4)        v1cmsg
022200010108     C                   seton                                        44  28
022300010108     C                   goto      endsrcontr
022400010108     C                   endif
022500010108     C                   endif
022600010108
022700010108     C                   if        v1dva <> *zeros
022800010108     C                   exsr      Sr_Contrdv                                   controllo data var.
022900010108     C   28              goto      endsrcontr
023000010108     C                   endif
023100010108
023200010108     C     endsrcontr    endsr
023300010108      **********************************************************************
023400010108      * CONTROLLI VIDEO 1     * controllo numero ORM *
023500010108      **********************************************************************
023600010108     C     Sr_Contrnor   BEGSR
023700010108
023800010109     C                   movel     v1cpoe        kpoe
023900010108     C                   movel     v1nsr         knsr
024000010108     C                   movel     v1nor         knor
024100010108     C                   movel     v1nrv         knrv
024200010108     C     Kfnor         chain     fnorm01l
024300010205      * Non trovato
024400010223     C                   if        not%found(fnorm01l)
024500010205     C                   movel     msg(1)        v1cmsg
024600010205     C                   seton                                        41  28
024700010205     C                   goto      endsrcnor
024800010205     C                   endif
024900010205      * Deve essere della mia £6
025000010205     C                   movel     v1cpoe        §cpoe
025100010205     C     §cpoe         lookup    l6                                     30
025200010205     C                   if        *in30 = *off
025300010108     C                   movel     msg(5)        v1cmsg
025400010108     C                   seton                                        44  28
025500010205     C                   goto      endsrcnor
025600010108     C                   endif
025700010205      * Visualizza
025800010205     C                   exsr      Sr_VisProposte
025900010205     C                   eval      *in91 = *on
026000010108
026100010205     C     endsrcnor     endsr
026200010108      **********************************************************************
026300010108      * CONTROLLI VIDEO 1     * data variazione *
026400010108      **********************************************************************
026500010108     C     Sr_Contrdv    BEGSR
026600010108
026700010108      * Data da
026800010108     C                   if        v1dvda > *zeros
026900010108     C                   clear                   wlbdat
027000010108     C                   z-add     v1dvda        G02dat
027100010108     C                   call      'XSRDA8'
027200010108     C                   parm                    wlbdat
027300010108     C                   if        G02err = '1'
027400010108     C                   eval      *in46 = *ON
027500010108     C                   eval      *in28 = *ON
027600010108     C                   movel     msg(10)       v1cmsg
027700010108     C                   goto      endsrcdar
027800010108     C                   else
027900010108     C                   z-add     G02dat        v1dvda
028000010108     C                   z-add     G02inv        vdvda
028100010108     C                   endif
028200010108     C                   endif
028300010108
028400010108      * Data a
028500010108     C                   if        v1dva > *zeros
028600010108     C                   clear                   wlbdat
028700010108     C                   z-add     v1dva         G02dat
028800010108     C                   call      'XSRDA8'
028900010108     C                   parm                    wlbdat
029000010108     C                   if        G02err = '1'
029100010108     C                   eval      *in51 = *ON
029200010108     C                   eval      *in28 = *ON
029300010108     C                   movel     msg(10)       v1cmsg
029400010108     C                   goto      endsrcdar
029500010108     C                   else
029600010108     C                   z-add     G02dat        v1dva
029700010108     C                   z-add     G02inv        vdva
029800010108     C                   endif
029900010108     C                   endif
030000010108
030100010108      * Range
030200010108     C                   if        vdvda > vdva
030300010108     C                             and vdvda <> *zeros
030400010108     C                             and vdva  <> *zeros
030500010108     C                   movel     msg(8)        v1cmsg
030600010108     C                   seton                                        46  28
030700010108     C                   endif
030800010108
030900010108     C     endsrcdar     endsr
031000001031      **********************************************************************
031100010108      * Subfile Proposte di Variazione
031200001031      **********************************************************************
031300010108     C     Sr_Gessfl     BEGSR
031400001031
031500010108     C                   z-add     1             recsf
031600010109     C                   setoff                                       47
031700010109     C                   do        *hival
031800001031      * Pulisco subfile
031900010108     C                   z-add     *zeros        nrr               4 0
032000001031     C                   eval      *in21 = *off
032100010108     C                   write     fior12c
032200001031     C                   eval      *in20 = *on
032300001031     C                   eval      *in21 = *on
032400001031
032500001031      * Carico il subfile in base alle scelte fatte
032600010205     C     *loval        setll     fnorp01l
032700001031     C                   do        *hival
032800010205     C                   read      fnorp01l
032900010205
033000001031     C                   if        %eof
033100001031     C                   leave
033200001031     C                   endif
033300010205      * Deve essere nella £6
033400010205     C     orppoe        lookup    l6                                     30
033500010205     C                   if        *in30 = *off
033600010205     C     orppor        lookup    l6                                     30
033700010205     C                   if        *in30 = *off
033800010205     C                   iter
033900010205     C                   endif
034000010205     C                   endif
034100010108
034200010109     C                   if        ORPdtv > vdva   or ORPdtv < vdvda
034300010108     C                   iter
034400010108     C                   endif
034500010109
034600010109     C                   if        v1sino = 'SI' and ORPfev = *blanks
034700010109     C                   iter
034800010109     C                   endif
034900001031
035000010109     C                   clear                   psce
035100010109     C                   movel     v1cfgs        v2cfgs
035200010109     C                   movel     v1dfgs        v2dfgs
035300001031     C                   exsr      Sr_CarProposte
035400001031     C                   enddo
035500010108
035600001031      * Se non ho caricato emetto una videata di segnalazione.
035700010108     C                   if        nrr = *zeros
035800010108     C                   exfmt     fior123
035900010205     C                   seton                                        90
036000010109     C                   endif
036100001031
036200001031      * Se ho caricato emetto il subfile
036300010108     C                   if        nrr > *zeros
036400010109     C     emettosf      tag
036500010108     C                   write     fior122
036600010108     C                   exfmt     fior12c
036700010109     C                   endif
036800001031      * F12=Ritorno
036900010109     C   kl              leave
037000001031
037100001031      * Legge i record scelti
037200010109     C                   if        nrr >0
037300010109
037400010109     C                   eval      nrr = *zeros
037500001031     C                   do        *hival
037600010109     C                   eval      nrr = nrr +1
037700010109     C     nrr           chain     fior12s                            30
037800010109     C                   if        *in30 = *on
037900001031     C                   leave
038000001031     C                   endif
038100010109
038200010109     C                   setoff                                       28  47
038300010109
038400010109      * Controllo se scelta valida
038500010109     C                   if        v2scel > *blanks and v2scel <> '5'
038600010109     C                   seton                                        28  47
038700010109     C                   movel     msg(14)       v2cmsg
038800010109     C                   z-add     nrr           recsf
038900010109     C                   update    fior12s
039000010109     C                   goto      emettosf
039100010109     C                   endif
039200010109
039300010109      * Visualizzazione
039400010109     C                   if        v2scel = '5'
039500010108     C                   z-add     nrr           recsf
039600001031     C                   exsr      Sr_VisProposte
039700010109     C                   if        psce = 'E'
039800010109     C                   seton                                        28  47
039900010109     C                   movel     msg(1)        v2cmsg
040000010108     C                   update    fior12s
040100010109     C                   goto      emettosf
040200010109     C                   endif
040300010109     C                   endif
040400010109
040500010109     C                   update    fior12s
040600001031
040700001031     C                   enddo
040800001031     C                   endif
040900010109
041000010109     C                   if        *in28 = *on
041100010109     C                   goto      emettosf
041200010109     C                   endif
041300010109
041400010109     C                   enddo
041500001031
041600001031     C     endproposte   endsr
041700001031      **********************************************************************
041800001031      * CARICO SUBFILE PROPOSTE
041900001031      **********************************************************************
042000001031     C     Sr_CarPropostebegsr
042100010108
042200010108     C                   clear                   v2scel
042300010108     C                   z-add     ORPpoe        v2cpoe
042400010108     C                   z-add     ORPnsr        v2nsr
042500010108     C                   z-add     ORPnor        v2nor
042600010108     C                   z-add     ORPnrv        v2nrv
042700010108     C                   eval      v2cpor = ORPpor
042800001031
042900001031      * Inversione data modifica
043000001031     C                   z-add     ORPdtv        G02inv
043100001031     C                   movel     '3'           G02err
043200001031     C                   call      'XSRDA8'
043300001031     C                   parm                    wlbdat
043400010108     C                   movel     G02dat        v2dtv
043500001031
043600010108     C                   movel     ORPorv        v2orv
043700010109     C                   movel     ORPorv        v2orv6
043800010108     C                   eval      v2utv  = ORPutv
043900010108     C                   eval      v2fao  = ORPfao
044000001031      * Decodifica
044100001031     C                   clear                   TIBS02DS
044200001031     C                   movel     'C'           T02mod
044300001031     C                   movel     knsif         t02sif
044400001031     C                   movel     'FAR'         t02cod
044500010108     C                   movel(p)  v2fao         T02ke1
044600001031     C                   call      'TIBS02R'
044700001031     C                   parm                    KPJBA
044800001031     C                   parm                    TIBS02DS
044900010108     C                   movel     t02uni        v2dfao
045000001031      * Analisi Proposta
045100001031     C                   if        ORPfev <> *blanks
045200001031     C                   seton                                        03
045300001031     C                   if        ORPfev = 'E'
045400010108     C                   eval      v2sta = 'EFFETTUATA'
045500010108     C                   eval      v2sta1= 'EFF'
045600001031     C                   endif
045700001031     C                   if        ORPfev = 'R'
045800010108     C                   eval      v2sta = 'RIFIUTATA'
045900010108     C                   eval      v2sta1= 'RIF'
046000001031     C                   endif
046100001031      * Inversione data analisi proposta
046200001031     C                   z-add     ORPdte        G02inv
046300001031     C                   movel     '3'           G02err
046400001031     C                   call      'XSRDA8'
046500001031     C                   parm                    wlbdat
046600010108     C                   movel     G02dat        v2dte
046700010108     C                   movel     ORPore        v2ore
046800010108     C                   eval      v2ute  = ORPute
046900001107     C                   else
047000001107     C                   setoff                                       03
047100010108     C                   clear                   v2sta
047200010108     C                   clear                   v2sta1
047300010108     C                   clear                   v2dte
047400010108     C                   clear                   v2ore
047500010108     C                   clear                   v2ute
047600001031     C                   endif
047700001031
047800010108     C                   add       1             nrr
047900010108     C                   write     fior12s
048000001031
048100001031     C                   endsr
048200001031      **********************************************************************
048300001031      * VISUALIZZO DATI DEL FILE PROPOSTE
048400001031      **********************************************************************
048500001031     C     Sr_VisPropostebegsr
048600001031
048700010108     C                   clear                   parm01
048800010108     C                   movel     v2cpoe        ppoe
048900010108     C                   movel     v2nor         pnor
049000010108     C                   movel     v2nsr         pnsr
049100010108     C                   movel     v2nrv         pnrv
049200010108     C                   movel     'P'           psce
049300010108     C                   movel     v2cfgs        pfgs
049400010109     C                   movel     v2dtv         pdtv
049500010109     C                   movel     v2orv6        porv
049600010109     C                   movel     v2utv         putv
049700010109     C                   movel     'P'           pric
049800010108
049900010108     C                   movel     parm01        kpjbu
050000010108     C                   call      'FIOR07R'
050100010108     C                   parm                    kpjba
050200010108     C                   movel     kpjbu         parm01
050300001031
050400001031     C                   endsr
050500001010      *****************************************************************
050600001010      * ROUTINE INIZIALE
050700001010      *****************************************************************
050800001010     C     *INZSR        BEGSR
050900001009
051000001010     C     *ENTRY        PLIST
051100001010     C                   PARM                    KPJBA
051200001011
051300001011     C                   movel     TIT_A         VTCtit
051400001016
051500001016     C                   Z-ADD     1             CODUT
051600001016     C                   CALL      'X§PARUT'
051700001016     C                   PARM                    UT§DSE0F
051800001016     C                   MOVEL     RAGUT         RSUT
051900001016     C                   MOVEL     REC80         CNCR80
052000010108
052100010108      * IMPOSTO IL P.O. DI GESTIONE
052200020502     C*                  if        rem = 'REM'  AND  remfil > *ZEROS
052300020502     C*                  movel     REMFIL        V1CFGS
052400020502     C                   if        simtpp = '2' or simtpp = *blanks
052500020502     C                   movel     simpou        V1CFGS
052600010108     C                   eval      *in06 = *on
052700010108     C                   else
052800010108     C                   movel     SIMFEL        V1CFGS
052900010108     C                   endif
053000001016
053100001019     C                   movel     v1cfgs        kazorg
053200001019     C     kazorg        chain     azorg01L
053300001016     C                   if        %found(azorg01l)
053400001016     C                   movel     orgDES        v1dfgs
053500001016     C                   else
053600001016     C                   clear                   v1dfgs
053700001016     C                   endif
053800010108
053900010108      * Carico schiera P.O. gestiti
054000010108     C                   CLEAR                   TRUL06ds
054100010108     C                   MOVE      '£1'          D06COD
054200010108     C                   MOVEL     SIMFEL        D06KEY
054300010108     C                   MOVEL     TRUL06DS      KPJBU
054400010108     C                   CALL      'TRUL06R'
054500010108     C                   PARM                    KPJBA
054600010108     C                   MOVEL     KPJBU         TRUL06DS
054700010108     C                   MOVEA     LIN           L1
054800010205      * Carico £6
054900010205     C                   CLEAR                   trul06ds
055000010205     C                   MOVE      '£6'          D06COD
055100010205     C                   MOVEL     v1cfgs        D06KEY
055200010205     C                   MOVEL     trul06ds      KPJBU
055300010205     C                   CALL      'TRUL06R'
055400010205     C                   PARM                    KPJBA
055500010205     C                   MOVEL     KPJBU         trul06ds
055600010205     C                   MOVEA     LIN           L6
055700001009
055800001023      * reperisce data e ora
055900001010     C                   TIME                    W0140
056000001010      * UDATE IN GGMMAAAA
056100001010     C                   MOVE      W0140         WDTGIO
056200001010      * UDATE IN AAAAMMGG
056300001010     C                   Z-ADD     WDTGIO        G02DAT
056400001010     C                   MOVEL     *BLANK        G02ERR
056500001010     C                   CALL      'XSRDA8'
056600001010     C                   PARM                    WLBDAT
056700001025     C                   Z-ADD     G02INV        DATEU
056800001010     C     *iso          MOVEL     DATEU         DATA_oggi
056900001010
057000001025     C                   z-add     dateu         §doggi
057100010108
057200010108     C     Kfnor         klist
057300010109     C                   kfld                    kpoe
057400010108     C                   kfld                    knsr
057500010108     C                   kfld                    knor
057600010108     C                   kfld                    knrv
057700001103
057800001103     C     Kforp         klist
057900001103     C                   kfld                    kpoe
058000001103     C                   kfld                    knsr
058100001103     C                   kfld                    knor
058200001103     C                   kfld                    knrv
058300001103     C                   kfld                    kdtv
058400001103     C                   kfld                    korv
058500001006
058600001006     C                   endsr
058700001010      *---------------------------------------------------------------------------------------------
058800001031** MSG  Lungh. 78                                                            *
058900010108Numero ORM errato                                                                   1
059000010109Libero.............................                                                 2
059100010109Libero.............................                                                 3
059200070418Immettere la Filiale emissione                                                      4
059300070418ORM non in gestione alla Filiale                                                    5
059400010109Libero.............................                                                 6
059500070418Filiale emittente errata                                                            7
059600010108Range data errato                                                                   8
059700010109Libero.............................                                                 9
059800010109Data variazione errata                                                              10
059900010109Libero.............................                                                 11
060000070418Filiale gestione errata                                                             12
060100010109Libero.............................                                                 13
060200010108Scelta non valida                                                                   14
060300010109Libero.............................                                                 15
060400010109Libero.............................                                                 16
060500010108Procedura ORM non attivata                                                          17
