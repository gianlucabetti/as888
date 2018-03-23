000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500001006      *       GESTIONE ORDINI RITIRO FISSI                            *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001018
000900001018      * ATTENZIONE
001000010405      * CARICO SUBFILE SOLO FINO A 9999
001100001018
001200980521      ****************************************************************
001300980521      *  RIEPILOGO INDICATORI
001400981008      ****************************************************************
001500060104      * 01 - richiamato da anagrafica ritiri
001600060105      * 02 - richiamato da anagrafica ritiri x interrogazione
001700980521      * 05 - SPROTEGGO P.O. IN GESTIONE
001800980521      * 06 - NON ABILITO TASTO FUNZIONALE F18
001900010405      * 11 - LEGGO PER N.ritiro fisso
002000010405      * 12 - SONO IN SEDE
002100010405      * 13 - LETTURA PER P.O. se sono in sede
002200990312      * 20 - GESTIONE SUBFILE
002300990312      * 21 - GESTIONE SUBFILE
002400990312      * 22 - GESTIONE SUBFILE
002500990312      * 23 - GESTIONE SUBFILE - ROLLUP
002600990315      * 24 - LETTURA FILE PER CARICAMENTO SUBFILE
002700990323      * 25 - LETTURA FILE PER CARICAMENTO SUBFILE
002800990505      * 26 - DISABILITO - ROLLUP
002900980521      * 28 - ERRORE GENERICO DSPF
003000001009      * 30 - Comodo
003100010316      * 31 - Comodo
003200010316      * 32 - Comodo
003300980521      * 40 - ERRORE p.o. in gestione
003400010201      * 41 - ERRORE Numero ritiro fisso
003500001009      * 42 - ERRORE Codice ritiro
003600001009      * 43 - ERRORE Codice ordinante
003700130411      * 44 - ERRORE Giro ritiro o pos.cursore su Giro ritiro
003800001009      * 45 - ERRORE Nessuna selezione
003900001009      * 46 - ERRORE Ritiro M/P
004000001012      * 47 - ERRORE Ricerca ritiro
004100001012      * 48 - ERRORE Ricerca ordinante
004200010405      * 49 - ERRORE scelta
004300010405      * 50 - ERRORE p.o. emissione se sono in sede
004400010207      * 52 - ERRORE Procedura ORM non attiva
004500980624      * 90 - riemissione videata
004600001011      * 91 - riemissione videata
004700990317      ****************************************************************
004800980623
004900001009     FAZORG01L  IF   E           K DISK
005000001006     FFNORS01L  IF   E           K DISK
005100070905     ffnors11l  if   e           k disk
005200001006     FFNACR01L  IF   E           K DISK
005300001009     FFIOR02D   CF   E             WORKSTN  sfile(fiOR02S:nrr1)
005400980623
005500980521      *------------------------------------------------------------------------*
005600001006     D kcfgs           S                   LIKE(V1Cfgs)
005700001006     D knor            S                   LIKE(V1nor)
005800001009
005900001009     D knacr           S                   LIKE(ACRcro)
006000001020
006100001020     D §ccra           S             10  0
006200001020     D §ccor           S             10  0
006300001009
006400070905     d wccgi           s                   like(v1ccgi)
006500070905     d wdcgi           s                   like(v1dcgi)
006600001006     D W0140           S             14  0
006700001006     D Wdtgio          S              8  0
006800001006     D dateu           S              8  0
006900001009
007000001006     D DATA_oggi       S               D   DATFMT(*eur)
007100130619
007200130619     d wabi            s                   like(UTEaut)
007300130619     d wEsci           s               n   inz(*off)
007400130619     d wErrGrave       s               n   inz(*off)
007500130619     d w003a           s              3
007600001006
007700001215      *  titolo videata (lunghezza massima 33)
007800001215     D TIT_A           C                   CONST('***  ANAGRAFICA RITIRI FISSI-
007900980624     D                                       ***')
008000001006
008100980622      *  Descrizione opzioni (lunghezza massima 78)
008200001006     D DOPZ1           C                   CONST('2=Manutenzione   5=Visualizza-
008300001006     D                                          ')
008400010405     D DOPZ1A          C                   CONST('5=Visualizza                 -
008500010405     D                                          ')
008600001006     D DOPZ2           C                   CONST(' ')
008700001006
008800980622      *  Opzioni valide
008900001006     D DOPZ            S             20    INZ('25')
009000010405     D DOPZA           S             20    INZ('5')
009100001006
009200980521      *   S C H I E R E
009300070905     D MSG             S             78    DIM(14)CTDATA PERRCD(1)              MSG VIDEO
009400001009     D VOC             S              3    DIM(7) CTDATA PERRCD(1)              VOCI A VIDEO
009500980622     D SEL             S              1    DIM(20)                              SELEZIONI
009600980622     D OPZ             S              1    DIM(20)                              OPZIONI
009700130619     D******* L1              S              3  0 DIM(30)                              P.O. Gestiti
009800130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
009900001006
010000980521      *   D S   I N T E R N E / E S T E R N E
010100001009
010200001009     D WLBDAT          DS                  INZ
010300001009     D  G02DAT                 1      8  0
010400001009     D  G02INV                 9     16  0
010500001009     D  G02ERR                17     17
010600001009     D  G02TGI                18     22  0
010700001020
010800001020     D v1ccra          DS
010900001020     D  v1cra1                        3  0
011000001020     D  v1cra2                        4  0
011100001020     D  v1cra3                        3  0
011200001020
011300001020     D v1ccor          DS
011400001110     D  v1cor1                        3  0
011500001020     D  v1cor2                        4  0
011600001020     D  v1cor3                        3  0
011700001009
011800001009     D Parm01          DS
011900001009     D  ppoe                   1      3  0
012000001009     D  pnor                   4     10  0
012100001009     D  psce                  11     11
012200060104     d  pfior01               12     12
012300060111     d  pcro                  13     22  0
012400001009
012500070905     d Azuteds       e ds                  Extname(Azute00f)
012600070905     d dDatiute      e ds
012700070905     d fidg09ds      e ds
012800060104     d fior02ds      e ds
012900070808     d FIOR37ds      e ds                  inz
013000070808     d  I37opz       e                     inz('1')
013100070808     d  I37cgi       e                     inz(*all'9')
013200070905     d Tibs34ds      e ds
013300130619
013400130619     d dLAT          e ds
013500130619     d dUTE01        e ds
013600130619
013700130619     d TIBS02DS      e ds
013800130619     d TRUL31DS      e ds
013900070810      *
014000010207     D OG148         E DS
014100001025
014200010730     D DORS01        E DS
014300010730
014400130619     D******* DSUL06        E DS                  EXTNAME(TRUL06DS)
014500130619     D******* LIN                    1     90  0 DIM(30)
014600001006
014700980521     D KPJBA         E DS
014800001025
014900980521     D                SDS
015000980521     D  VTCPGM                 1     10
015100981009
015200001006      *------------------------------------------------------------------------*
015300001009
015400060104      * Se richiamato vado subito al subfile
015500060104     c   01              Goto      sfl
015600001006
015700001006      * Emissione 1° Videata
015800980623
015900001006     C                   do        *hival
016000001009      * Pulizia 1° Videata
016100010313     C                   if        (*in28 = *off and *in90 = *off and
016200010313     C                             *in05 = *off) or (*in52 = *on)
016300001009     C                   exsr      Sr_pulvid1
016400001009     C                   endif
016500010405
016600010405     C                   eval      *in11 = *off
016700010405     C                   eval      *in13 = *off
016800130619
016900130619      /free
017000130619       //?Se utente non autorizzato alla funzione esco
017100130619         IF  wErrGrave;
017200130619           *in28 = *on;
017300130619           wEsci = *on;
017400130619           V1Cmsg = 'Utente non autorizzato alla funzione';
017500130619         ENDIF;
017600130619      /end-free
017700980623
017800001009     C                   exfmt     fior021
017900130619
018000130619      /free
018100130619         IF  wEsci;
018200130619           leave;
018300130619         ENDIF;
018400130619      /end-free
018500980623
018600010405     C                   setoff                                       289091
018700010405
018800001006      * F3=Fine
018900001006     C   kc              leave
019000010405
019100010405      * F18=Cambio P.O. in gestione
019200010405     C   ks              eval      *in05 = *on
019300010405     C   ks              iter
019400010405
019500010405      * 05 on --> controllo P.O. in gestione
019600010405     C                   if        *in05 = *on
019700010405     C                   exsr      Sr_Contrpo
019800010405     C  n28              eval      *in05 = *off
019900010405     C   28              iter
020000010405     C                   endif
020100010405
020200010405      * Procedura ORM non attiva emetto l'errore
020300051027     C                   if        §ogorm = *Blanks
020400010405     C                             and *inks = *off and *in05 = *off
020500010405     C                             and *in12 = *off
020600010405     C                   movel     msg(8)        v1cmsg
020700010405     C                   seton                                        52  28
020800010405     C                   iter
020900010405     C                   endif
021000001006
021100001006      * F10=Immissione
021200010405     C                   if        *inkj = *on and *in05 = *off
021300010409     C                   movel     v1cfgs        v2poe
021400001009     C                   movel     '1'           v2scel
021500001009     C                   movel     *zeros        v2nor
021600001009     C                   exsr      Sr_Modifica
021700001009     C                   endif
021800980623
021900001006      * Controlli 1° videata
022000001006     C                   exsr      Sr_Contr01
022100980623
022200001025     C                   if        *in28 = *on   or
022300001025     C                             *in90 = *on   or
022400001025     C                             *in91 = *on
022500001025     C                   iter
022600001025     C                   endif
022700060104
022800060104     c     sfl           tag
022900980623
023000001006      * Imposto opzioni
023100001006     C                   exsr      Sr_Caropz
023200001020
023300001020     C                   movel     v1ccra        §ccra
023400001020     C                   movel     v1ccor        §ccor
023500001009      * Gestione subfile
023600001009     C                   exsr      Sr_Gessfl
023700060105     C                   if        *inkc = *on or *in01
023800001011     C                   leave
023900001011     C                   endif
024000981008
024100001006     C                   enddo
024200981204
024300001006     C                   eval      *inlr = *on
024400001009      *****************************************************************
024500001009      * PULIZIA 1° VIDEATA
024600001009      *****************************************************************
024700001009     C     Sr_pulvid1    BEGSR
024800001009
024900001009     C                   clear                   v1nor
025000001009     C                   clear                   v1ccra
025100001009     C                   clear                   v1dcra
025200001009     C                   clear                   v1ccor
025300001009     C                   clear                   v1dcor
025400070905     c                   clear                   v1ccgi
025500070905     c                   clear                   v1dcgi
025600010405     C                   clear                   v1cpoe
025700001009
025800001009     C                   endsr
025900001006      *****************************************************************
026000001006      * IMPOSTO OPZIONI
026100001006      *****************************************************************
026200001006     C     Sr_Caropz     BEGSR
026300001009
026400001006     C                   clear                   opz
026500001009
026600010405     C                   movel     dopz2         v2dse2
026700060104
026800060104      * non richiamato e non sono in sede
026900060104      * oppure richiamato e gestione
027000060104     c                   If        (not *In01 and Not *In12) or
027100060105     c                             (*In01 and Not *In02)
027200060104     C                   movel     dopz1         v2dse1
027300060104     C                   movea     dopz          OPZ
027400060104     c                   endif
027500060104      * non richiamato e sono in sede
027600060104      * oppure richiamato e interrogazione
027700060104     c                   If        (not *In01 and *In12) or
027800060105     c                             (*In01 and *In02)
027900060104     C                   movel     dopz1a        v2dse1
028000060104     C                   movea     dopza         OPZ
028100060104     c                   endif
028200001009
028300001006     C                   endsr
028400001009      **********************************************************************
028500010405      * CONTROLLO IL P.O. IN GESTIONE
028600001009      **********************************************************************
028700001009     C     Sr_Contrpo    begsr
028800001009
028900001009     C                   setoff                                       40
029000001025     C                   clear                   v1dfgs
029100001009
029200130619     C**** v1cfgs        lookup    L1                                     30
029300130619     c                   move      V1Cfgs        w003a
029400130619     c     w003a         lookup    SKpog                                  30
029500001025     C     *in30         ifeq      *off
029600001025     C                   seton                                        40  28
029700001025     C                   movel     msg(7)        v1cmsg
029800010405     C                   goto      endsrcpo
029900010405     C                   endif
030000001025      * Decodifica
030100001025     C     v1cfgs        chain     azorg01L
030200001025     C                   if        %found(azorg01l)
030300001025     C                   movel     orgDES        v1dfgs
030400010207     C                   movel     orgde8        og148
030500051027     C                   if        §ogorm = *Blanks
030600010207     C                   movel     msg(8)        v1cmsg
030700010207     C                   seton                                        52  28
030800010207     C                   endif
030900001025     C                   else
031000001025     C                   seton                                        40  28
031100001025     C                   movel     msg(7)        v1cmsg
031200001025     C                   endif
031300001009
031400010405     C     endsrcpo      endsr
031500980521      **********************************************************************
031600980521      * CONTROLLI VIDEO 1
031700980521      **********************************************************************
031800001006     C     Sr_Contr01    begsr
031900001006
032000001009      * Reset indicatori
032100001009
032200001009     C                   setoff                                       414243
032300001009     C                   setoff                                       444546
032400001009     C                   setoff                                       50
032500010405
032600010405     C                   if        *in12 = *on and v1cpoe <> *zeros
032700010405     C                   exsr      Sr_contrpoe                                  controllo p.o.
032800010405     C   28              goto      endsrcontr
032900010405     C                   eval      v1cfgs = v1cpoe
033000010405     C                   eval      *in13 = *on
033100010405     C                   endif
033200001006
033300001011     C                   if        v1nor <> *zeros
033400010405      * non sono in sede
033500010405     C                   if        *in12 = *off
033600001006     C                   exsr      Sr_contrnor                                  controllo num.ORM
033700001011     C   28              goto      endsrcontr
033800010405     C                   eval      *in11 = *on
033900010405      * sono in sede
034000010405     C                   else
034100010405     C                   if        v1cpoe <> *zeros
034200010405     C                   exsr      Sr_contrnor                                  controllo num.ORM
034300010405     C   28              goto      endsrcontr
034400010405     C                   eval      *in11 = *on
034500010405     C                   else
034600010405     C                   movel     msg(12)       v1cmsg
034700010405     C                   seton                                        50  28
034800010405     C                   goto      endsrcontr
034900010405     C                   endif
035000010405     C                   endif
035100010405     C                   endif
035200001006
035300001011     C                   if        v1dcra <> *blanks
035400001110     C                             and v1ccra = *zeros
035500001012     C                   exsr      Sr_riccra                                    richiamo la ricerca
035600001011     C   28              goto      endsrcontr
035700001011     C                   endif
035800001006
035900001020     C                   if        v1ccra <> *zeros
036000001006     C                   exsr      Sr_contrcra                                  controllo cod.ritiro
036100001011     C   28              goto      endsrcontr
036200001011     C                   endif
036300001006
036400001011     C                   if        v1dcor <> *blanks
036500001110     C                             and v1ccor = *zeros
036600001012     C                   exsr      Sr_riccor                                    richiamo la ricerca
036700001011     C   28              goto      endsrcontr
036800001011     C                   endif
036900001006
037000001020     C                   if        v1ccor <> *zeros
037100001006     C                   exsr      Sr_contrcor                                  controllo cod.ordin.
037200001011     C   28              goto      endsrcontr
037300001011     C                   endif
037400070905
037500070905     c                   if        v1ccgi <> *blanks
037600070905     c                   eval      wccgi = v1ccgi
037700070905     c                   exsr      sr_contrcgi                                  contr. zona ritiro
037800070905     c   28              leavesr
037900070905     c                   eval      v1ccgi = wccgi
038000070905     c                   eval      v1dcgi = wdcgi
038100070905     c                   endif
038200010122
038300010122     C                   if        v1crmp <> *blanks
038400010122     C                   if        v1crmp <> 'M' and v1crmp <> 'P'
038500010122     C                   movel     msg(6)        v1cmsg                         errore
038600010122     C                   seton                                        46  28
038700010122     C                   endif
038800010122     C                   endif
038900001006
039000001011     C     endsrcontr    endsr
039100010405      **********************************************************************
039200010405      * CONTROLLO IL P.O. EMISSIONE
039300010405      **********************************************************************
039400010405     C     Sr_Contrpoe   begsr
039500010405
039600010405     C                   setoff                                       50
039700010405
039800010405     C     v1cpoe        chain     azorg01L
039900010405     C                   if        %found(azorg01l)
040000010405     C                   movel     orgde8        og148
040100051027     C                   if        §ogorm = *Blanks
040200010405     C                   movel     msg(8)        v1cmsg
040300010405     C                   seton                                        52  28
040400010405     C                   endif
040500010405     C                   else
040600010405     C                   seton                                        50  28
040700010405     C                   movel     msg(13)       v1cmsg
040800010405     C                   endif
040900010405
041000010405     C                   endsr
041100980527      **********************************************************************
041200010201      * CONTROLLI VIDEO 1     * controllo numero ritiro fisso *
041300980527      **********************************************************************
041400001006     C     Sr_contrnor   BEGSR
041500001009
041600001009     C                   movel     v1cfgs        kcfgs
041700001009     C                   movel     v1nor         knor
041800001006     C     Kfnor         chain     fnors01l
041900010405      * Non trovato
042000010405     C                   if        not%found(fnors01l)
042100001009     C                   movel     msg(1)        v1cmsg
042200001009     C                   seton                                        41  28
042300001006     C                   endif
042400001009
042500001006     C                   endsr
042600001009      **********************************************************************
042700001012      * CONTROLLI VIDEO 1     * ricerca ritiro *
042800001009      **********************************************************************
042900001012     C     Sr_ricCra     BEGSR
043000001009
043100001018     C                   eval      *in90 = *on
043200070808     c                   reset                   FIOR37ds
043300070808     c                   movel(p)  V1Dcra        I37rag
043400070808     c                   movel     V1Cfgs        I37fgs
043500070808     c                   movel(p)  FIOR37ds      KPJBU
043600070808     c                   call      'FIOR37R'
043700070808     c                   parm                    KPJBA
043800070808     c                   movel     KPJBU         FIOR37ds
043900070808     c                   select
044000070808     c                   when      O37err <> *blanks
044100070808     c                   movel     O37msg        V1Cmsg
044200070808     c                   seton                                        47  28
044300070808     c                   when      O37cro <> *zeros
044400070808     c                   movel     O37cro        V1Ccra
044500070808     c                   endsl
044600001009
044700001009     C                   endsr
044800001012      **********************************************************************
044900001012      * CONTROLLI VIDEO 1     * ricerca ordinante *
045000001012      **********************************************************************
045100001012     C     Sr_ricCor     BEGSR
045200001012
045300001018     C                   eval      *in90 = *on
045400070808     c                   reset                   FIOR37ds
045500070808     c                   movel(p)  V1Dcor        I37rag
045600070808     c                   movel     V1Cfgs        I37fgs
045700070808     c                   movel(p)  FIOR37ds      KPJBU
045800070808     c                   call      'FIOR37R'
045900070808     c                   parm                    KPJBA
046000070808     c                   movel     KPJBU         FIOR37ds
046100070808     c                   select
046200070808     c                   when      O37err <> *blanks
046300070808     c                   movel     O37msg        V1Cmsg
046400070808     c                   seton                                        48  28
046500070808     c                   when      O37cro <> *zeros
046600070808     c                   movel     O37cro        V1Ccor
046700070808     c                   endsl
046800001012
046900001012     C                   endsr
047000001009      **********************************************************************
047100001009      * CONTROLLI VIDEO 1     * codice ritiro *
047200001009      **********************************************************************
047300001009     C     Sr_contrcra   BEGSR
047400001009
047500001009     C                   movel     v1ccra        knacr
047600001009     C     knacr         chain     fnacr01l
047700001009     C                   if        %found(fnacr01l)
047800071030     C                   if        ACRatb <> *blanks
047900001009     C                   movel     msg(2)        v1cmsg
048000001009     C                   seton                                        42  28
048100001009     C                   endif
048200001009     C                   else
048300001009     C                   movel     msg(2)        v1cmsg
048400001009     C                   seton                                        42  28
048500001009     C                   endif
048600001009
048700001009     C                   endsr
048800001009      **********************************************************************
048900001009      * CONTROLLI VIDEO 1     * codice ordinante *
049000001009      **********************************************************************
049100001009     C     Sr_contrcor   BEGSR
049200001009
049300001009     C                   movel     v1ccor        knacr
049400001009     C     knacr         chain     fnacr01l
049500001009     C                   if        %found(fnacr01l)
049600071030     C                   if        ACRatb <> *blanks
049700001009     C                   movel     msg(3)        v1cmsg
049800001009     C                   seton                                        43  28
049900001009     C                   endif
050000001009     C                   else
050100001009     C                   movel     msg(3)        v1cmsg
050200001009     C                   seton                                        43  28
050300001009     C                   endif
050400001009
050500001009     C                   endsr
050600070905
050700070905      *------------------------------------------------------------------------*
050800070905      * CODICE GIRO
050900070905      *------------------------------------------------------------------------*
051000070905     c     sr_contrcgi   begsr
051100070905
051200070905     c                   clear                   v1dcgi
051300070905
051400070905      * ricerca giro con "?"
051500070905     c                   if        %scan('?':wccgi) > *zeros
051600070905     c                   clear                   fidg09ds
051700070905     c                   eval      d09iop0 = 'P01'
051800070905     c                   eval      d09ifgs = v1cfgs
051900070905     c                   eval      d09idat = *date
052000070905     c                   eval      d09itug = 'R'
052100070905     c                   eval      kpjbu = fidg09ds
052200070905     c                   call      'FIDG09R'
052300070905     c                   parm                    kpjba
052400070905     c                   eval      fidg09ds = kpjbu
052500070905      *     imposto il giro scelto
052600070905     c                   eval      wccgi = d09ocgi
052700070905     c                   endif
052800070905      *     controllo
052900070905     c                   if        wccgi <> *blank
053000070905     c                   exsr      ctrgiro
053100070905      *     giro errato
053200070905     c                   if        d09oerr = '1'
053300070905     c                   eval      *in28 = *on
053400070905     c                   eval      *in44 = *on
053500070905     c                   eval      v1cmsg = msg(14)
053600070905     c                   leavesr
053700070905     c                   endif
053800070905      *     imposto la descrizione del giro
053900070905     c                   if        d09ocgi <> ' '
054000070905     c                   eval      wdcgi = d09odes
054100070905     c                   endif
054200070905     c                   endif
054300070905
054400070905     c                   endsr
054500070905
054600070905      *------------------------------------------------------------------------*
054700070905      *   Controllo e decodifica giro
054800070905      *------------------------------------------------------------------------*
054900070905     c     ctrgiro       begsr
055000070905
055100070905     c                   clear                   fidg09ds
055200070905     c                   eval      d09iop0 = '001'
055300070905     c                   eval      d09ifgs = v1cfgs
055400070905     c                   eval      d09idat = *date
055500070905     c                   eval      d09icgi = wccgi
055600070905     c                   eval      d09itug = 'R'
055700070905     c                   eval      kpjbu = fidg09ds
055800070905     c                   call      'FIDG09R'
055900070905     c                   parm                    kpjba
056000070905     c                   eval      fidg09ds = kpjbu
056100070905
056200070905     c                   endsr
056300070905
056400001009      **********************************************************************
056500001009      * GESTIONE SUBFILE
056600001009      **********************************************************************
056700001009     C     Sr_Gessfl     begsr
056800001009
056900010405     C                   z-add     1             recsf
057000001009     C                   do        *hival
057100010405     C                   setoff                                       49
057200001009      * Pulisco subfile
057300001009     C                   z-add     *zeros        nrr1              4 0
057400001009     C                   eval      *in21 = *off
057500001009     C                   write     fior02c
057600001009     C                   eval      *in20 = *on
057700001009     C                   eval      *in21 = *on
057800010405
057900010405      * Non è stato messo un numero ritiro
058000010405     C                   if        *in11 = *off
058100001009
058200001009      * Carico il subfile in base alle scelte fatte
058300010405      * solo 9999 record xchè il file video è definito così
058400010405
058500010405     C  n12v1cfgs        setll     fnors01l
058600010405     C  n13
058700010405     Can 12*loval        setll     fnors01l
058800010405     C   13
058900010405     Can 12v1cpoe        setll     fnors01l
059000001009     C                   do        *hival
059100010405     C  n12v1cfgs        reade     fnors01l
059200010405     C  n13
059300010405     Can 12              read      fnors01l
059400010405     C   13
059500010405     Can 12v1cpoe        reade     fnors01l
059600001009     C                   if        %eof
059700001009     C                   leave
059800001009     C                   endif
059900001009
060000010405     C                   if        nrr1 >= 9998
060100010405     C                   seton                                        28
060200010405     C                   movel     msg(10)       v2cmsg
060300001009     C                   leave
060400001009     C                   endif
060500001009
060600001020     C                   if        (v1ccra <> *zeros  and ORScra <> §ccra ) or
060700130411     C                             (v1ccor <> *zeros  and ORScor <> §ccor )
060800001009     C                   iter
060900001009     C                   endif
061000010123
061100010123     C                   if        v1crmp <> *blanks and v1crmp <> ORSrmp
061200010123     C                   iter
061300010123     C                   endif
061400071030
061500071030      * controllo se c'è il giro sul ritiro fisso
061600071030     C                   movel     orspoe        kcfgs
061700071030     C                   movel     orsnor        knor
061800071030     C     Kfnor         chain     fnors11l
061900071030     c                   if        %found(fnors11l) and ors1cgi <> *blanks
062000071030      * se richiesta selezione per giro escludo se diverso
062100071030     c                   if        v1ccgi <> *blanks and v1ccgi <> ors1cgi
062200071030     c                   iter
062300071030     c                   endif
062400071030     c                   eval      v2cgi = 'S'
062500071030     c                   else
062600071030      * se richiesta selezione per giro escludo se diverso
062700071030     c                   if        v1ccgi <> *blanks
062800071030     c                   iter
062900071030     c                   endif
063000071030     c                   clear                   v2cgi
063100071030     c                   endif
063200001009
063300010405     C  n12              movel     v1cfgs        v2cfgs
063400010405     C  n12              movel     v1dfgs        v2dfgs
063500001009     C                   exsr      Sr_Carsfl
063600001009     C                   enddo
063700010405     C                   endif
063800010405
063900010405      * E' stato messo un numero ritiro
064000010405     C                   if        *in11 = *on
064100010405     C     Kfnor         chain     fnors01l
064200010405     C                   if        %found(fnors01l)
064300010405     C  n12              movel     v1cfgs        v2cfgs
064400010405     C  n12              movel     v1dfgs        v2dfgs
064500071030      * controllo se c'è il giro sul ritiro fisso
064600071030     C     Kfnor         chain     fnors11l
064700071030     c                   if        %found(fnors11l) and ors1cgi <> *blanks
064800071030     c                   eval      v2cgi = 'S'
064900071030     c                   else
065000071030     c                   clear                   v2cgi
065100071030     c                   endif
065200071030
065300010405     C                   exsr      Sr_Carsfl
065400010405     C                   endif
065500010405     C                   endif
065600010405
065700010405     C     emettosf      tag
065800010405
065900010405      * Controllo se recsf è maggiore di nrr
066000010405     C                   if        recsf > nrr1
066100010405     C                   z-add     1             recsf
066200010405     C                   endif
066300001009
066400001009      * Se non ho caricato emetto una videata di segnalazione.
066500001009     C                   if        nrr1 = *zeros
066600001009     C                   exfmt     fior023
066700010405     C                   eval      *in91 = *on
066800001009     C                   endif
066900001009
067000001009      * Se ho caricato emetto il subfile
067100001009     C                   if        nrr1 > *zeros
067200001009     C                   write     fior022
067300001009     C                   exfmt     fior02c
067400001009     C                   endif
067500001009
067600001011      * F3=Fine
067700001011     C   kc              leave
067800001011
067900001009      * F12=Ritorno
068000001009     C   kl              leave
068100001009
068200001009      * F10=Immissione
068300001009     C                   if        *inkj = *on
068400010409     C                   movel     v1cfgs        v2poe
068500001009     C                   movel     '1'           v2scel
068600001009     C                   movel     *zeros        v2nor
068700001009     C                   exsr      Sr_Modifica
068800001009     C                   leave
068900001009     C                   endif
069000001009
069100001009      *    Leggo Scelte Effettuate dall'utente
069200001107
069300001009     C                   if        nrr1 >0
069400010405
069500010405     C                   eval      nrr1 = *zeros
069600001009     C                   do        *hival
069700010405     C                   eval      nrr1 = nrr1 +1
069800010405     C     nrr1          chain     fior02s                            30
069900010405     C                   if        *in30 = *on
070000010405     C                   leave
070100010405     C                   endif
070200010405
070300010405     C                   setoff                                       28  49
070400010405
070500010405      * Controllo se scelta valida
070600010405     C     v2scel        lookup    opz                                    30
070700010405     C                   if        *in30 = *off
070800010405     C                   seton                                        28  49
070900010405     C                   movel     msg(11)       v2cmsg
071000010405     C                   z-add     nrr1          recsf
071100010405     C                   update    fior02s
071200010405     C                   goto      emettosf
071300010405     C                   endif
071400010405
071500001107     C                   if        v2scel <> *blanks
071600010405     C                   z-add     nrr1          recsf
071700001009     C                   exsr      Sr_Modifica
071800001009     C                   endif
071900010405
072000010405     C                   update    fior02s
072100001107
072200001009     C                   enddo
072300001009     C                   endif
072400001009
072500001009     C                   enddo
072600001009
072700001009     C                   endsr
072800001009      **********************************************************************
072900001009      * CARICO SUBFILE
073000001009      **********************************************************************
073100001009     C     Sr_Carsfl     begsr
073200001009
073300001009     C                   clear                   v2scel
073400010405     C                   eval      v2poe  = ORSpoe
073500001009     C                   eval      v2nor  = ORSnor
073600001009     C                   eval      v2ccra = ORScra
073700001009     C                   eval      v2crlu = ORSrlu
073800001009     C                   eval      v2crma = ORSrma
073900001009     C                   eval      v2crme = ORSrme
074000001009     C                   eval      v2crgi = ORSrgi
074100001009     C                   eval      v2crve = ORSrve
074200010730     C                   eval      dors01 = ORSflo
074300010730     C                   if        §orsdif <> *zeros and §orsdff <> *zeros
074400010730     C                   eval      v2fer = 'S'
074500010730     C                   else
074600010730     C                   eval      v2fer = *blanks
074700010730     C                   endif
074800031030     c                   if        orsatb = 'A'
074900070905     c                   eval      V2ann = 'A'
075000031030     c                   else
075100031030     c                   clear                   v2ann
075200031030     c                   endif
075300001009
075400001009      * Decodifico Ritiro
075500001009     C     v2ccra        chain     fnacr01l
075600001009     C                   if        %found(fnacr01l)
075700001009     C                   eval      v2dcra = ACRrsr
075800010419     C                   else
075900010419     C                   clear                   v2dcra
076000001009     C                   endif
076100001009
076200001009      * Decodifico voce e imposto qtà relativa
076300001009     C                   select
076400001009     C                   when      ORSncl <> *zeros
076500001009     C                   movel     voc(1)        v2voc
076600001009     C                   z-add     ORSncl        v2qta
076700001009
076800001009     C                   when      ORSpkg <> *zeros
076900001009     C                   movel     voc(2)        v2voc
077000001009     C                   z-add     ORSpkg        v2qta
077100001009
077200001009     C                   when      ORSvlm <> *zeros
077300001009     C                   movel     voc(3)        v2voc
077400001009     C                   z-add     ORSvlm        v2qta
077500001009
077600001009     C                   when      ORSbnc <> *zeros
077700001009     C                   movel     voc(4)        v2voc
077800001009     C                   z-add     ORSbnc        v2qta
077900001009
078000001009     C                   when      ORSblc <> *zeros
078100001009     C                   movel     voc(5)        v2voc
078200001009     C                   z-add     ORSblc        v2qta
078300001009
078400001009     C                   when      ORSatt <> *zeros
078500001009     C                   movel     voc(6)        v2voc
078600001009     C                   z-add     ORSatt        v2qta
078700001009
078800001009     C                   when      ORSmtc <> *zeros
078900001009     C                   movel     voc(7)        v2voc
079000001009     C                   z-add     ORSmtc        v2qta
079100001009
079200001011     C                   other
079300001011     C                   z-add     *zeros        v2qta
079400001011     C                   movel     *blanks       v2voc
079500001011
079600001009     C                   endsl
079700001009
079800001009     C                   add       1             nrr1
079900001009     C                   write     fior02s
080000001009
080100001009     C                   endsr
080200001009      **********************************************************************
080300001009      * RICHIAMO IL PROGRAMMA DI MODIFICA
080400001009      **********************************************************************
080500001009     C     Sr_Modifica   begsr
080600001009
080700001009     C                   clear                   parm01
080800010405     C                   movel     v2poe         ppoe
080900001009     C                   movel     v2nor         pnor
081000001009     C                   movel     v2scel        psce
081100060104     c   01              eval      pfior01 = 'S'
081200060111     c   01              eval      pcro = i02cro
081300060111     c  n01              Clear                   pcro
081400001009     C                   movel     parm01        kpjbu
081500001009     C                   call      'FIOR03R'
081600001009     C                   parm                    kpjba
081700001009     C                   movel     kpjbu         parm01
081800001009
081900001009     C                   endsr
082000980923      *****************************************************************
082100980923      * ROUTINE INIZIALE
082200980923      *****************************************************************
082300980923     C     *INZSR        BEGSR
082400980923      *
082500980521     C     *ENTRY        PLIST
082600980521     C                   PARM                    KPJBA
082700060104     c                   Parm                    fior02ds
082800060104
082900060104      * Richiamato per interrogazione/manutenzione da anagrafica ritiri
083000060104     c                   If        %Parms > 1
083100060104     c                   Eval      *In01 = *On
083200060104     c                   Eval      v1cpoe = i02poa
083300060104     c                   Move      i02cro        v1ccra
083400060104     c                   Clear                   v1ccor
083500060104     c                   Clear                   v1nor
083600060105     c                   Eval      *In02 = (i02ric = '1')
083700060104     c                   Else
083800060104     c                   Eval      *In01 = *Off
083900060105     c                   Eval      *In02 = *Off
084000060104     c                   EndIf
084100001011      *
084200010405     C                   movel     TIT_A         VTCtit
084300070905
084400070905     c     *dtaara       define    §azute        azuteds
084500070905     c     *dtaara       define    §datiute      ddatiute
084600070905     c                   in(e)     *dtaara
084700070905     c                   if        %error or rsut = *blanks
084800070905     c                   clear                   tibs34ds
084900070905     c                   call      'TIBS34R'
085000070905     c                   parm                    tibs34ds
085100070905     c                   in        *dtaara
085200070905     c                   endif
085300130619
085400130619      * controllo abilitazione utente
085500130619     c                   clear                   wabi
085600130619     c                   clear                   dLAT
085700130619     c                   eval      dUTE01 = UTEfaf
085800130619
085900130619      * Verifica errori e autorità profilo
086000130619     c                   SELECT
086100130619      * se ho errori nei dati utente esco dal pgm
086200130619     c                   WHEN      DUTerr = 'E'
086300130619     c                   eval      wErrGrave = *on
086400130619     c                   leavesr
086500130619      * carica abilitazioni del profilo
086600130619     c                   WHEN      §UTEorm <> *blanks
086700130619     c                   eval      wabi = §UTEorm
086800130619     c                   WHEN      UTEaut <> *blanks
086900130619     c                   eval      wabi = UTEaut
087000130619      * se non c'è l'abilitazione
087100130619      * --> se 1° livello, abilitazioni al terminal
087200130619      *     se 2° livello, abilitazioni al punto operativo
087300130619      *     se sede no abilitazioni
087400130619     c                   OTHER
087500130619     c                   IF        DUTlpo = '1'
087600130619     c                   eval      wabi   = 'TP'
087700130619     c                   ENDIF
087800130619     c                   IF        DUTlpo = '2'
087900130619     c                   eval      wabi   = 'PO'
088000130619     c                   ENDIF
088100130619     c                   ENDSL
088200130619
088300130619      * controllo se ok l'abilitazione dell'utente
088400130619     c                   clear                   TIBS02DS
088500130619     c                   eval      T02mod = 'C'
088600130619     c                   eval      T02sif = knsif
088700130619     c                   eval      T02cod = 'LAT'
088800130619     c                   eval      T02ke1 = wabi
088900130619     c                   Call      'TIBS02R'
089000130619     c                   Parm                    KPJBA
089100130619     c                   Parm                    TIBS02DS
089200130619     c                   IF        T02err <> *blanks or §LATabi = 'S'
089300130619     c                   eval      wErrGrave = *on
089400130619     c                   leavesr
089500130619     c                   ENDIF
089600130619     c                   eval      dLAT = T02uni
089700130619
089800130619      * Reperimento delle filiali gestibili dall'utente
089900130619     c                   clear                   TRUL31DS
090000130619     c                   eval      I31abi = wabi
090100130619     c                   eval      I31cdi = DUTdis
090200130619     c                   eval      I31car = DUTare
090300130619     c                   eval      I31cpo = DUTpou
090400130619     c                   call      'TRUL31R'
090500130619     c                   parm                    KPJBA
090600130619     c                   parm                    TRUL31DS
090700130619     c                   IF        O31pog > *zeros
090800130619     c                   movea     O31pog        SKpog
090900130619     c                   ELSE
091000130619     c                   eval      wErrGrave = *on
091100130619     c                   leavesr
091200130619     c                   ENDIF
091300010405
091400010405      * Da sede non imposto il p.o. di gestione
091500010405     C                   if        simfel = 0
091600010405     C                   eval      *in12 = *on
091700010405     C                   else
091800001006      * IMPOSTO IL P.O. DI GESTIONE
091900071109     c                   if             %Parms > 1
092000071109     c                             and  I02poa > *zeros
092100071109     c                   move      I02poa        V1Cfgs
092200071109     c                   else
092300070905     C                   movel     dutpou        V1CFGS
092400071109     c                   endif
092500130619     c*******            eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
092600010405     C                   endif
092700001025
092800001025      * Carico schiera P.O. gestiti
092900130619     C*****              CLEAR                   DSUL06
093000130619     C*****              MOVE      '£1'          D06COD
093100130619     C*****              MOVEL     SIMFEL        D06KEY
093200130619     C*****              MOVEL     DSUL06        KPJBU
093300130619     C*****              CALL      'TRUL06R'
093400130619     C*****              PARM                    KPJBA
093500130619     C*****              MOVEL     KPJBU         DSUL06
093600130619     C*****              MOVEA     LIN           L1
093700001009
093800130411     c                   clear                   Og148
093900001006     C     v1cfgs        chain     azorg01L
094000001006     C                   if        %found(azorg01l)
094100001006     C                   movel     orgDES        v1dfgs
094200010207     C                   movel     orgde8        og148
094300051027     C                   if        §ogorm = *Blanks
094400010207     C                   movel     msg(8)        v1cmsg
094500010207     C                   seton                                        52  28
094600010207     C                   endif
094700001006     C                   else
094800001006     C                   clear                   v1dfgs
094900001013     C                   endif
095000980923
095100980923      * reperisco data e ora
095200980923     C                   TIME                    W0140
095300980923      * UDATE IN GGMMAAAA
095400980923     C                   MOVE      W0140         WDTGIO
095500980923      * UDATE IN AAAAMMGG
095600980923     C                   Z-ADD     WDTGIO        G02DAT
095700980923     C                   MOVEL     *BLANK        G02ERR
095800980923     C                   CALL      'XSRDA8'
095900980923     C                   PARM                    WLBDAT
096000980923     C                   MOVEL     G02INV        DATEU
096100980923     C     *iso          MOVEL     DATEU         DATA_oggi
096200980923
096300001009     C     Kfnor         klist
096400001009     C                   kfld                    kcfgs
096500001009     C                   kfld                    knor
096600001006
096700010405     C                   endsr
096800980521      *---------------------------------------------------------------------------------------------
096900980521** MSG  Lungh. 78                                                            *
097000010201Codice ritiro fisso errato                                                          1
097100010420Cliente mittente non valido                                                         2
097200010201Cliente ordinante non valido                                                        3
097300130411-----Libero                                                                         4
097400001009Immettere una selezione valida                                                      5
097500001009Scelta errata                                                                       6
097600070418Filiale in gestione errata                                                          7
097700010207Procedura ORM non attiva                                                            8
097800130411-----Libero                                                                         9
097900010405Ampiezza massima subfile superata                                                  10
098000010405Scelta non valida                                                                  11
098100070418Immettere la filiale di emissione                                                  12
098200070418Filiale emissione errata                                                           13
098300070905Codice Giro non corretto                                                           14
098400001009** VOC  Lungh. 3                                                             *
098500001009COL                                                                                 1
098600001009PKG                                                                                 2
098700001009VLM                                                                                 3
098800001009BNC                                                                                 4
098900001009BLC                                                                                 5
099000001009AUT                                                                                 6
099100001009MOT                                                                                 7
