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
004500130619
004600130619     d wabi            s                   like(UTEaut)
004700130619     d wEsci           s               n   inz(*off)
004800130619     d wErrGrave       s               n   inz(*off)
004900130619     d w003a           s              3
005000001009
005100001006     D W0140           S             14  0
005200001006     D Wdtgio          S              8  0
005300001006     D dateu           S              8  0
005400001009
005500001006     D DATA_oggi       S               D   DATFMT(*eur)
005600001006
005700001010      *  titolo videata (lunghezza massima 34)
005800010108     D TIT_A           C                   CONST('***  INT.PROPOSTE VARIAZIONE -
005900001010     D                                       ***')
006000001031
006100001031      *   S C H I E R E
006200010108     D MSG             S             78    DIM(17) CTDATA PERRCD(1)             MSG VIDEO
006300130619     D**** L1              S              3  0 DIM(30)                              P.O. Gestiti
006400010205     D L6              S              3  0 DIM(30)                              P.O. Gestiti
006500130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
006600001006
006700001010      *   D S   I N T E R N E / E S T E R N E
006800001009
006900001009     D WLBDAT          DS                  INZ
007000001009     D  G02DAT                 1      8  0
007100001009     D  G02INV                 9     16  0
007200001009     D  G02ERR                17     17
007300001009     D  G02TGI                18     22  0
007400001031
007500001009     D Parm01          DS
007600001016     D  ppoe                          3  0
007700001016     D  pnor                          7  0
007800001016     D  pnsr                          2  0
007900001016     D  pnrv                          2  0
008000001016     D  psce                          1
008100001018     D  pfgs                          3  0
008200010205     D  ppor                          3  0
008300010205     D  pdtr                          8  0
008400010205     D  pmdb                         10
008500010205     D  pprb                         10
008600010205     D  pdts                          8  0
008700010205     D  prmp                          1
008800010205     D  pbrc                          1
008900010205     D  pref                          2
009000010205     D  pmio                          1
009100010205
009200010109     D  pdtv                          8  0
009300010109     D  porv                          6  0
009400010109     D  putv                         10
009500010109     D  pric                          1
009600001009
009700001010     D TIBS02DS      E DS
009800010108
009900010108     D TRUL06DS      E DS
010000010108     D  LIN                    1     90  0
010100010108     D                                     DIM(30)
010200130619
010300130619     d azuteds       e ds                  extname(azute00f)
010400130619     d ddatiute      e ds
010500130619     d tibs34ds      e ds
010600130619     d TRUL31DS      e ds
010700130619
010800130619     d dLAT          e ds
010900130619     d dUTE01        e ds
011000001010
011100001010      *
011200001010     D KPJBA         E DS
011300001010      *
011400001010     D                SDS
011500001010     D  VTCPGM                 1     10
011600001010
011700001006      *------------------------------------------------------------------------*
011800010108
011900010108      * Emissione 1° Videata
012000010108
012100010108     C                   do        *hival
012200010108      * Pulizia 1° Videata
012300010205     C                   if        *in28 = *off  and  *in05 = *off
012400010205     C                             and *in90 = *off
012500010108     C                   exsr      Sr_Pulvid1
012600010108     C                   endif
012700010205
012800010205      * Azzero il numero ORM
012900010205     C                   if        *in91 = *on
013000010205     C                   clear                   v1cpoe
013100010205     C                   clear                   v1nor
013200010205     C                   endif
013300130619      /free
013400130619       //?Se utente non autorizzato alla funzione esco
013500130619         IF  wErrGrave;
013600130619           *in28 = *on;
013700130619           wEsci = *on;
013800130619           V1Cmsg = 'Utente non autorizzato alla funzione';
013900130619         ENDIF;
014000130619      /end-free
014100010108
014200010108     C                   exfmt     fior121
014300130619
014400130619      /free
014500130619         IF  wEsci;
014600130619           leave;
014700130619         ENDIF;
014800130619      /end-free
014900010108
015000010108      * F3=Fine
015100010108     C   kc              leave
015200010205     C                   setoff                                       289091
015300010108
015400010108      * F18=Cambio P.O. in gestione
015500010108     C   ks              eval      *in05 = *on
015600010108     C   ks              iter
015700010108
015800010108      * 05 on --> controllo P.O. in gestione
015900010108     C                   if        *in05 = *on
016000010108     C                   exsr      Sr_Contrpo
016100010109     C  n28              eval      *in05 = *off
016200010108     C                   endif
016300010108
016400010108      * Controlli 1° videata
016500010108     C                   exsr      Sr_Contr01
016600010108
016700010108     C   28              iter
016800010205     C   91              iter
016900010108
017000010108     C                   movel     v1cfgs        §cpoe
017100010108      * Gestione subfile
017200010108     C                   exsr      Sr_Gessfl
017300010109     C                   setoff                                       28
017400010108     C                   if        *inkc = *on
017500010108     C                   leave
017600010108     C                   endif
017700010108
017800010108     C                   enddo
017900001010
018000001006     C                   eval      *inlr = *on
018100010108      *****************************************************************
018200010108      * PULIZIA 1° VIDEATA
018300010108      *****************************************************************
018400010108     C     Sr_Pulvid1    BEGSR
018500010108
018600010108     C                   clear                   v1cpoe
018700010108     C                   clear                   v1nor
018800010108     C                   clear                   v1nsr
018900010108     C                   clear                   v1nrv
019000010108     C                   clear                   v1dvda
019100010108     C                   movel     data_oggi     v1dva
019200010109     C                   movel     'NO'          v1sino
019300010108
019400010108     C                   endsr
019500010108      **********************************************************************
019600010108      * CONTROLLO IL P.O. IN IN GESTIONE
019700010108      **********************************************************************
019800010108     C     Sr_Contrpo    begsr
019900010108
020000010108     C                   setoff                                       40
020100010108     C                   clear                   v1dfgs
020200010108
020300130619     C**** v1cfgs        lookup    L1                                     30
020400130619     c                   move      V1Cfgs        w003a
020500130619     c     w003a         lookup    SKpog                                  30
020600010108     C     *in30         ifeq      *off
020700010108     C                   seton                                        40  28
020800010108     C                   movel     msg(12)       v1cmsg
020900010108     C                   else
021000010108
021100010108      * Decodifica
021200010108     C     v1cfgs        chain     azorg01L
021300010108     C                   if        %found(azorg01l) and ORGfva = *blanks
021400010108     C                   movel     orgDES        v1dfgs
021500010108     C                   else
021600010108     C                   seton                                        40  28
021700010108     C                   movel     msg(12)       v1cmsg
021800010108     C                   endif
021900010207
022000010207      * Devo ricaricare la £6
022100010207     C                   CLEAR                   trul06ds
022200010207     C                   MOVE      '£6'          D06COD
022300010207     C                   MOVEL     v1cfgs        D06KEY
022400010207     C                   MOVEL     trul06ds      KPJBU
022500010207     C                   CALL      'TRUL06R'
022600010207     C                   PARM                    KPJBA
022700010207     C                   MOVEL     KPJBU         trul06ds
022800010207     C                   MOVEA     LIN           L6
022900010108     C                   endif
023000010108
023100010108     C                   endsr
023200010108      **********************************************************************
023300010108      * CONTROLLI VIDEO 1
023400010108      **********************************************************************
023500010108     C     Sr_Contr01    begsr
023600010108
023700010108      * Reset indicatori
023800010108
023900010109     C                   setoff                                       414446
024000010109     C                   setoff                                       51
024100010108
024200010108     C                   if        v1nor <> *zeros
024300010108     C                   if        v1cpoe > *zeros
024400010108     C                   exsr      Sr_Contrnor                                  controllo num.ORM
024500010108     C   28              goto      endsrcontr
024600010108     C                   else
024700010108     C                   movel     msg(4)        v1cmsg
024800010108     C                   seton                                        44  28
024900010108     C                   goto      endsrcontr
025000010108     C                   endif
025100010108     C                   endif
025200010108
025300010108     C                   if        v1dva <> *zeros
025400010108     C                   exsr      Sr_Contrdv                                   controllo data var.
025500010108     C   28              goto      endsrcontr
025600010108     C                   endif
025700010108
025800010108     C     endsrcontr    endsr
025900010108      **********************************************************************
026000010108      * CONTROLLI VIDEO 1     * controllo numero ORM *
026100010108      **********************************************************************
026200010108     C     Sr_Contrnor   BEGSR
026300010108
026400010109     C                   movel     v1cpoe        kpoe
026500010108     C                   movel     v1nsr         knsr
026600010108     C                   movel     v1nor         knor
026700010108     C                   movel     v1nrv         knrv
026800010108     C     Kfnor         chain     fnorm01l
026900010205      * Non trovato
027000010223     C                   if        not%found(fnorm01l)
027100010205     C                   movel     msg(1)        v1cmsg
027200010205     C                   seton                                        41  28
027300010205     C                   goto      endsrcnor
027400010205     C                   endif
027500010205      * Deve essere della mia £6
027600010205     C                   movel     v1cpoe        §cpoe
027700010205     C     §cpoe         lookup    l6                                     30
027800010205     C                   if        *in30 = *off
027900010108     C                   movel     msg(5)        v1cmsg
028000010108     C                   seton                                        44  28
028100010205     C                   goto      endsrcnor
028200010108     C                   endif
028300010205      * Visualizza
028400010205     C                   exsr      Sr_VisProposte
028500010205     C                   eval      *in91 = *on
028600010108
028700010205     C     endsrcnor     endsr
028800010108      **********************************************************************
028900010108      * CONTROLLI VIDEO 1     * data variazione *
029000010108      **********************************************************************
029100010108     C     Sr_Contrdv    BEGSR
029200010108
029300010108      * Data da
029400010108     C                   if        v1dvda > *zeros
029500010108     C                   clear                   wlbdat
029600010108     C                   z-add     v1dvda        G02dat
029700010108     C                   call      'XSRDA8'
029800010108     C                   parm                    wlbdat
029900010108     C                   if        G02err = '1'
030000010108     C                   eval      *in46 = *ON
030100010108     C                   eval      *in28 = *ON
030200010108     C                   movel     msg(10)       v1cmsg
030300010108     C                   goto      endsrcdar
030400010108     C                   else
030500010108     C                   z-add     G02dat        v1dvda
030600010108     C                   z-add     G02inv        vdvda
030700010108     C                   endif
030800010108     C                   endif
030900010108
031000010108      * Data a
031100010108     C                   if        v1dva > *zeros
031200010108     C                   clear                   wlbdat
031300010108     C                   z-add     v1dva         G02dat
031400010108     C                   call      'XSRDA8'
031500010108     C                   parm                    wlbdat
031600010108     C                   if        G02err = '1'
031700010108     C                   eval      *in51 = *ON
031800010108     C                   eval      *in28 = *ON
031900010108     C                   movel     msg(10)       v1cmsg
032000010108     C                   goto      endsrcdar
032100010108     C                   else
032200010108     C                   z-add     G02dat        v1dva
032300010108     C                   z-add     G02inv        vdva
032400010108     C                   endif
032500010108     C                   endif
032600010108
032700010108      * Range
032800010108     C                   if        vdvda > vdva
032900010108     C                             and vdvda <> *zeros
033000010108     C                             and vdva  <> *zeros
033100010108     C                   movel     msg(8)        v1cmsg
033200010108     C                   seton                                        46  28
033300010108     C                   endif
033400010108
033500010108     C     endsrcdar     endsr
033600001031      **********************************************************************
033700010108      * Subfile Proposte di Variazione
033800001031      **********************************************************************
033900010108     C     Sr_Gessfl     BEGSR
034000001031
034100010108     C                   z-add     1             recsf
034200010109     C                   setoff                                       47
034300010109     C                   do        *hival
034400001031      * Pulisco subfile
034500010108     C                   z-add     *zeros        nrr               4 0
034600001031     C                   eval      *in21 = *off
034700010108     C                   write     fior12c
034800001031     C                   eval      *in20 = *on
034900001031     C                   eval      *in21 = *on
035000001031
035100001031      * Carico il subfile in base alle scelte fatte
035200010205     C     *loval        setll     fnorp01l
035300001031     C                   do        *hival
035400010205     C                   read      fnorp01l
035500010205
035600001031     C                   if        %eof
035700001031     C                   leave
035800001031     C                   endif
035900010205      * Deve essere nella £6
036000010205     C     orppoe        lookup    l6                                     30
036100010205     C                   if        *in30 = *off
036200010205     C     orppor        lookup    l6                                     30
036300010205     C                   if        *in30 = *off
036400010205     C                   iter
036500010205     C                   endif
036600010205     C                   endif
036700010108
036800010109     C                   if        ORPdtv > vdva   or ORPdtv < vdvda
036900010108     C                   iter
037000010108     C                   endif
037100010109
037200010109     C                   if        v1sino = 'SI' and ORPfev = *blanks
037300010109     C                   iter
037400010109     C                   endif
037500001031
037600010109     C                   clear                   psce
037700010109     C                   movel     v1cfgs        v2cfgs
037800010109     C                   movel     v1dfgs        v2dfgs
037900001031     C                   exsr      Sr_CarProposte
038000001031     C                   enddo
038100010108
038200001031      * Se non ho caricato emetto una videata di segnalazione.
038300010108     C                   if        nrr = *zeros
038400010108     C                   exfmt     fior123
038500010205     C                   seton                                        90
038600010109     C                   endif
038700001031
038800001031      * Se ho caricato emetto il subfile
038900010108     C                   if        nrr > *zeros
039000010109     C     emettosf      tag
039100010108     C                   write     fior122
039200010108     C                   exfmt     fior12c
039300010109     C                   endif
039400001031      * F12=Ritorno
039500010109     C   kl              leave
039600001031
039700001031      * Legge i record scelti
039800010109     C                   if        nrr >0
039900010109
040000010109     C                   eval      nrr = *zeros
040100001031     C                   do        *hival
040200010109     C                   eval      nrr = nrr +1
040300010109     C     nrr           chain     fior12s                            30
040400010109     C                   if        *in30 = *on
040500001031     C                   leave
040600001031     C                   endif
040700010109
040800010109     C                   setoff                                       28  47
040900010109
041000010109      * Controllo se scelta valida
041100010109     C                   if        v2scel > *blanks and v2scel <> '5'
041200010109     C                   seton                                        28  47
041300010109     C                   movel     msg(14)       v2cmsg
041400010109     C                   z-add     nrr           recsf
041500010109     C                   update    fior12s
041600010109     C                   goto      emettosf
041700010109     C                   endif
041800010109
041900010109      * Visualizzazione
042000010109     C                   if        v2scel = '5'
042100010108     C                   z-add     nrr           recsf
042200001031     C                   exsr      Sr_VisProposte
042300010109     C                   if        psce = 'E'
042400010109     C                   seton                                        28  47
042500010109     C                   movel     msg(1)        v2cmsg
042600010108     C                   update    fior12s
042700010109     C                   goto      emettosf
042800010109     C                   endif
042900010109     C                   endif
043000010109
043100010109     C                   update    fior12s
043200001031
043300001031     C                   enddo
043400001031     C                   endif
043500010109
043600010109     C                   if        *in28 = *on
043700010109     C                   goto      emettosf
043800010109     C                   endif
043900010109
044000010109     C                   enddo
044100001031
044200001031     C     endproposte   endsr
044300001031      **********************************************************************
044400001031      * CARICO SUBFILE PROPOSTE
044500001031      **********************************************************************
044600001031     C     Sr_CarPropostebegsr
044700010108
044800010108     C                   clear                   v2scel
044900010108     C                   z-add     ORPpoe        v2cpoe
045000010108     C                   z-add     ORPnsr        v2nsr
045100010108     C                   z-add     ORPnor        v2nor
045200010108     C                   z-add     ORPnrv        v2nrv
045300010108     C                   eval      v2cpor = ORPpor
045400001031
045500001031      * Inversione data modifica
045600001031     C                   z-add     ORPdtv        G02inv
045700001031     C                   movel     '3'           G02err
045800001031     C                   call      'XSRDA8'
045900001031     C                   parm                    wlbdat
046000010108     C                   movel     G02dat        v2dtv
046100001031
046200010108     C                   movel     ORPorv        v2orv
046300010109     C                   movel     ORPorv        v2orv6
046400010108     C                   eval      v2utv  = ORPutv
046500010108     C                   eval      v2fao  = ORPfao
046600001031      * Decodifica
046700001031     C                   clear                   TIBS02DS
046800001031     C                   movel     'C'           T02mod
046900001031     C                   movel     knsif         t02sif
047000001031     C                   movel     'FAR'         t02cod
047100010108     C                   movel(p)  v2fao         T02ke1
047200001031     C                   call      'TIBS02R'
047300001031     C                   parm                    KPJBA
047400001031     C                   parm                    TIBS02DS
047500010108     C                   movel     t02uni        v2dfao
047600001031      * Analisi Proposta
047700001031     C                   if        ORPfev <> *blanks
047800001031     C                   seton                                        03
047900001031     C                   if        ORPfev = 'E'
048000010108     C                   eval      v2sta = 'EFFETTUATA'
048100010108     C                   eval      v2sta1= 'EFF'
048200001031     C                   endif
048300001031     C                   if        ORPfev = 'R'
048400010108     C                   eval      v2sta = 'RIFIUTATA'
048500010108     C                   eval      v2sta1= 'RIF'
048600001031     C                   endif
048700001031      * Inversione data analisi proposta
048800001031     C                   z-add     ORPdte        G02inv
048900001031     C                   movel     '3'           G02err
049000001031     C                   call      'XSRDA8'
049100001031     C                   parm                    wlbdat
049200010108     C                   movel     G02dat        v2dte
049300010108     C                   movel     ORPore        v2ore
049400010108     C                   eval      v2ute  = ORPute
049500001107     C                   else
049600001107     C                   setoff                                       03
049700010108     C                   clear                   v2sta
049800010108     C                   clear                   v2sta1
049900010108     C                   clear                   v2dte
050000010108     C                   clear                   v2ore
050100010108     C                   clear                   v2ute
050200001031     C                   endif
050300001031
050400010108     C                   add       1             nrr
050500010108     C                   write     fior12s
050600001031
050700001031     C                   endsr
050800001031      **********************************************************************
050900001031      * VISUALIZZO DATI DEL FILE PROPOSTE
051000001031      **********************************************************************
051100001031     C     Sr_VisPropostebegsr
051200001031
051300010108     C                   clear                   parm01
051400010108     C                   movel     v2cpoe        ppoe
051500010108     C                   movel     v2nor         pnor
051600010108     C                   movel     v2nsr         pnsr
051700010108     C                   movel     v2nrv         pnrv
051800010108     C                   movel     'P'           psce
051900010108     C                   movel     v2cfgs        pfgs
052000010109     C                   movel     v2dtv         pdtv
052100010109     C                   movel     v2orv6        porv
052200010109     C                   movel     v2utv         putv
052300010109     C                   movel     'P'           pric
052400010108
052500010108     C                   movel     parm01        kpjbu
052600010108     C                   call      'FIOR07R'
052700010108     C                   parm                    kpjba
052800010108     C                   movel     kpjbu         parm01
052900001031
053000001031     C                   endsr
053100001010      *****************************************************************
053200001010      * ROUTINE INIZIALE
053300001010      *****************************************************************
053400001010     C     *INZSR        BEGSR
053500001009
053600001010     C     *ENTRY        PLIST
053700001010     C                   PARM                    KPJBA
053800001011
053900001011     C                   movel     TIT_A         VTCtit
054000130619
054100130619     c     *dtaara       define    §azute        azuteds
054200130619     c     *dtaara       define    §datiute      ddatiute
054300130619     c                   in(e)     *dtaara
054400130619     c                   if        %error or rsut = *blanks
054500130619     c                   clear                   tibs34ds
054600130619     c                   call      'TIBS34R'
054700130619     c                   parm                    tibs34ds
054800130619     c                   in        *dtaara
054900130619     c                   endif
055000130619
055100130619      * controllo abilitazione utente
055200130619     c                   clear                   wabi
055300130619     c                   clear                   dLAT
055400130619     c                   eval      dUTE01 = UTEfaf
055500130619
055600130619      * Verifica errori e autorità profilo
055700130619     c                   SELECT
055800130619      * se ho errori nei dati utente esco dal pgm
055900130619     c                   WHEN      DUTerr = 'E'
056000130619     c                   eval      wErrGrave = *on
056100130619     c                   leavesr
056200130619      * carica abilitazioni del profilo
056300130619     c                   WHEN      §UTEorm <> *blanks
056400130619     c                   eval      wabi = §UTEorm
056500130619     c                   WHEN      UTEaut <> *blanks
056600130619     c                   eval      wabi = UTEaut
056700130619      * se non c'è l'abilitazione
056800130619      * --> se 1° livello, abilitazioni al terminal
056900130619      *     se 2° livello, abilitazioni al punto operativo
057000130619      *     se sede no abilitazioni
057100130619     c                   OTHER
057200130619     c                   IF        DUTlpo = '1'
057300130619     c                   eval      wabi   = 'TP'
057400130619     c                   ENDIF
057500130619     c                   IF        DUTlpo = '2'
057600130619     c                   eval      wabi   = 'PO'
057700130619     c                   ENDIF
057800130619     c                   ENDSL
057900130619
058000130619      * controllo se ok l'abilitazione dell'utente
058100130619     c                   clear                   TIBS02DS
058200130619     c                   eval      T02mod = 'C'
058300130619     c                   eval      T02sif = knsif
058400130619     c                   eval      T02cod = 'LAT'
058500130619     c                   eval      T02ke1 = wabi
058600130619     c                   Call      'TIBS02R'
058700130619     c                   Parm                    KPJBA
058800130619     c                   Parm                    TIBS02DS
058900130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
059000130619     c                   eval      wErrGrave = *on
059100130619     c                   leavesr
059200130619     c                   ENDIF
059300130619     c                   eval      dLAT = T02uni
059400130619
059500130619      * Reperimento delle filiali gestibili dall'utente
059600130619     c                   clear                   TRUL31DS
059700130619     c                   eval      I31abi = wabi
059800130619     c                   eval      I31cdi = DUTdis
059900130619     c                   eval      I31car = DUTare
060000130619     c                   eval      I31cpo = DUTpou
060100130619     c                   call      'TRUL31R'
060200130619     c                   parm                    KPJBA
060300130619     c                   parm                    TRUL31DS
060400130619     c                   IF        O31pog > *zeros
060500130619     c                   movea     O31pog        SKpog
060600130619     c                   ELSE
060700130619     c                   eval      wErrGrave = *on
060800130619     c                   leavesr
060900130619     c                   ENDIF
061000010108
061100010108      * IMPOSTO IL P.O. DI GESTIONE
061200130619     c                   eval      v1cfgs = dutpou
061300001016
061400001019     C                   movel     v1cfgs        kazorg
061500001019     C     kazorg        chain     azorg01L
061600001016     C                   if        %found(azorg01l)
061700001016     C                   movel     orgDES        v1dfgs
061800001016     C                   else
061900001016     C                   clear                   v1dfgs
062000001016     C                   endif
062100010108
062200010108      * Carico schiera P.O. gestiti
062300130619     C*****              CLEAR                   TRUL06ds
062400130619     C*****              MOVE      '£1'          D06COD
062500130619     C*****              MOVEL     SIMFEL        D06KEY
062600130619     C*****              MOVEL     TRUL06DS      KPJBU
062700130619     C*****              CALL      'TRUL06R'
062800130619     C*****              PARM                    KPJBA
062900130619     C*****              MOVEL     KPJBU         TRUL06DS
063000130619     C*****              MOVEA     LIN           L1
063100010205      * Carico £6
063200010205     C                   CLEAR                   trul06ds
063300010205     C                   MOVE      '£6'          D06COD
063400010205     C                   MOVEL     v1cfgs        D06KEY
063500010205     C                   MOVEL     trul06ds      KPJBU
063600010205     C                   CALL      'TRUL06R'
063700010205     C                   PARM                    KPJBA
063800010205     C                   MOVEL     KPJBU         trul06ds
063900010205     C                   MOVEA     LIN           L6
064000001009
064100001023      * reperisce data e ora
064200001010     C                   TIME                    W0140
064300001010      * UDATE IN GGMMAAAA
064400001010     C                   MOVE      W0140         WDTGIO
064500001010      * UDATE IN AAAAMMGG
064600001010     C                   Z-ADD     WDTGIO        G02DAT
064700001010     C                   MOVEL     *BLANK        G02ERR
064800001010     C                   CALL      'XSRDA8'
064900001010     C                   PARM                    WLBDAT
065000001025     C                   Z-ADD     G02INV        DATEU
065100001010     C     *iso          MOVEL     DATEU         DATA_oggi
065200001010
065300001025     C                   z-add     dateu         §doggi
065400010108
065500010108     C     Kfnor         klist
065600010109     C                   kfld                    kpoe
065700010108     C                   kfld                    knsr
065800010108     C                   kfld                    knor
065900010108     C                   kfld                    knrv
066000001103
066100001103     C     Kforp         klist
066200001103     C                   kfld                    kpoe
066300001103     C                   kfld                    knsr
066400001103     C                   kfld                    knor
066500001103     C                   kfld                    knrv
066600001103     C                   kfld                    kdtv
066700001103     C                   kfld                    korv
066800001006
066900001006     C                   endsr
067000001010      *---------------------------------------------------------------------------------------------
067100001031** MSG  Lungh. 78                                                            *
067200010108Numero ORM errato                                                                   1
067300010109Libero.............................                                                 2
067400010109Libero.............................                                                 3
067500070418Immettere la Filiale emissione                                                      4
067600070418ORM non in gestione alla Filiale                                                    5
067700010109Libero.............................                                                 6
067800070418Filiale emittente errata                                                            7
067900010108Range data errato                                                                   8
068000010109Libero.............................                                                 9
068100010109Data variazione errata                                                              10
068200010109Libero.............................                                                 11
068300070418Filiale gestione errata                                                             12
068400010109Libero.............................                                                 13
068500010108Scelta non valida                                                                   14
068600010109Libero.............................                                                 15
068700010109Libero.............................                                                 16
068800010108Procedura ORM non attivata                                                          17
