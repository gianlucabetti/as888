000100981005      *-------------------------------------------------------------------------*
000200981005      *    GESTIONE DENUNCIA EVENTI                                             *
000300981005      *-------------------------------------------------------------------------*
000400980721
000500980721      ****************************************************************
000600980721      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
000700980721      ****************************************************************
000800980721      ***************************************************************
000900980623
001000980521      ****************************************************************
001100980521      *  RIEPILOGO INDICATORI
001200980521      ***************************************************************
001300981001      * 07 - ESISTE almeno una C.A. elaborabile
001400981005      * 08 - STAMPATA denuncia
001500980904      * 30 - COMODO
001600980904      * 31 - COMODO
001700980904      * 32 - COMODO
001800980904      * 28 - ERRORE GENERICO DSPF
001900980904      * 40 - ERRORE p.o. in gestione
002000980924      * 41 - ERRORE numero evento
002100990223      * 44 - ERRORE tipo modulo
002200990223      * 45 - ERRORE nominativo presentazione denuncia
002300980930      * 46 - ERRORE data presentazione denuncia
002400990223      * 47 - ERRORE autorità rag. sociale
002500990223      * 48 - ERRORE autorità indirizzo
002600990223      * 49 - ERRORE autorità località
002700990223      * 57 - SCELTA errata in video 2
002800990223      * 58 - SCELTA bloccata in video 2
002900990223      * 59 - REVERSE SU C.A. NON ELABORABILI
003000980904      * 90 - riemissione videata
003100980904      ***************************************************************
003200980623
003300980521     H DECEDIT('0,') DATEDIT(*DMY.)
003400980623
003500980930     FFIDN18D   CF   E             WORKSTN  sfile(FI18S02:nrr1)
003600981001     FFNDCF01L  IF   E           K DISK
003700981005     FFNDCT08L  IF   E           K DISK
003800980929     FFNDET01L  IF   E           K DISK
003900981012     FFNDED01L  IF   E           K DISK
004000980924     FFNDCS01L  IF   E           K DISK
004100980928     FTNTBE01L  IF   E           K DISK
004200980928     FFNDFA01L  IF   E           K DISK
004300980623
004400980521      *------------------------------------------------------------------------*
004500980929     D Kaae            S                   LIKE(DETaae)
004600980929     D Knev            S                   LIKE(DETnev)
004700980904     D Kfil            S                   LIKE(DCTfil)
004800980907     D Kaas            S                   LIKE(DCTaas)
004900980907     D Klnp            S                   LIKE(DCTlnp)
005000980907     D Knrs            S                   LIKE(DCTnrs)
005100980907     D Knsp            S                   LIKE(DCTnsp)
005200980907     D Kaac            S                   LIKE(DCTaac)
005300980907     D Knca            S                   LIKE(DCTnca)
005400980624     D wtpb            S                   LIKE(I00tpb)
005500980907     D wf03            S                   LIKE(O00f03)
005600980529     D wi05fca         S                   LIKE(I05FCA)
005700980907     D wazn_fase       S                   LIKE(DCTFCA)
005800980928     D Wtbecod         S                   LIKE(TBEcod)
005900981001     D Wfgs            S                   LIKE(DCTfil)
006000980929     D ktpd            s                   LIKE(DCStpd)
006100980929     D knkt            s                   LIKE(DCSnkt)
006200980929     D kstd            s                   LIKE(DCSstd)
006300980929     D wstd            s                   LIKE(DCSstd)
006400980929     D knks            s                   LIKE(DCSnks)
006500980929     D ktrc            s                   LIKE(DCStrc)
006600980929     D kdim            s                   LIKE(DCSdim)
006700980929     D khim            s                   LIKE(DCShim)
006800990223     D W_v1cdpr        S                   LIKE(V1Cdpr)
006900990223     D W_v2hi59        S                   LIKE(V2Hi59)
007000980907      *
007100981012     D WCAnoopn        S              1
007200981001     D W001A           S              1
007300981001     D Wsce            S              1
007400980929     D Wpta            S              3
007500981001     D Wfce            S              3
007600980929     D W011A           S             11
007700980907     D xx              S              3  0
007800981013     D yy              S              3  0
007900981014     D jj              S              3  0
008000980721     D W0140           S             14  0
008100980521     D Wdtgio          S              8  0
008200981014     D Wdataev         S              8  0
008300980521     D dateu           S              8  0
008400980529     D nrr1            S              4  0
008500980623     D nrr2            S              4  0
008600980929     D WORA            S              4  0
008700981014     D Wpoe            S              7  0
008800980604     D DATA_eur        S               D   DATFMT(*eur)
008900980604      *
009000980622      *  titolo videata (lunghezza massima 34)
009100981211     D TIT_G           C                   CONST('***  EVENTI - STAMPA DENUNCIA-
009200980930     D                                       ***')
009300980930      *
009400980622      *  Descrizione opzioni (lunghezza massima 78)
009500980924     D DOPZ1_G         C                   CONST('5=Visual. C.A.   8=Gestione N-
009600980924     D                                     ote   9=Visual. bolla')
009700980623      *
009800980622      *  Opzioni valide
009900980924     D OPZ_G           S             20    INZ('589')
010000980622      *
010100980521      *   S C H I E R E
010200990223     D MSG             S             78    DIM(16) CTDATA PERRCD(1)             MSG VIDEO
010300980622     D OPZ             S              1    DIM(20)                              OPZIONI
010400990223     D KTA             S              2    DIM(50)                              Chiave TAD
010500980928     D DTA             S             25    DIM(50)                              Descrizione TAD
010600981012     D POE             S              3  0 DIM(200)                             P.O. interessati
010700981012     D KFC             S              3  0 DIM(50)                              Fase C.A.
010800980928     D DFC             S             48    DIM(50)                              Descrizione fase C.A
010900980929     D PTA             S              3    DIM(100)                             PTR + TAD
011000980929     D GFS             S              1    DIM(100)                             gestione fase
011100980521      *
011200980521      *   D S   I N T E R N E / E S T E R N E
011300980722      *
011400980722     D DSNUMCA         DS                  INZ
011500980722     D  v2haac
011600980722     D  v2cfil
011700980722     D  v2cnca
011800980525      *
011900980624     D DS_RICBOLA      DS                  INZ
012000980624     D  DSRBA_aas             14     17  0
012100980624     D  DSRBA_lnp             18     20  0
012200980624     D  DSRBA_nrs             21     22  0
012300980624     D  DSRBA_nsp             23     29  0
012400980624     D  DSRBA_f03             30     30
012500980624     D  DSRBA_flg             31     31
012600980624      *
012700980624     D DS_RICBOLP      DS                  INZ
012800980624     D  DSRBP_aas              1      4  0
012900980624     D  DSRBP_lnp              5      7  0
013000980624     D  DSRBP_nrs              8      9  0
013100980624     D  DSRBP_nsp             10     16  0
013200980624     D  DSRBP_f03             17     17
013300980624     D  DSRBP_flg             31     31
013400980908      *
013500980521     D WLBDAT          DS                  INZ
013600980521     D  G02DAT                 1      8  0
013700980521     D  G02INV                 9     16  0
013800980521     D  G02ERR                17     17
013900980521     D  G02TGI                18     22  0
014000980525      *
014100980521      * Ricezione dati da azione
014200980521     D DS_AZIONE       DS                  INZ
014300980521     D  AZN_tipo               1      1
014400980521     D  AZN_fase               2      4
014500980521     D  AZN_trpr               5      5
014600980915      *----------------------------------
014700980929     D FIDN00DS      E DS
014800980529     D FIDN05DS      E DS
014900981013     D FIDN06DS      E DS
015000980929     D FIDN07DS      E DS
015100980722     D FIDN10DS      E DS
015200980930     D FIDN18DS      E DS
015300990205     D FIDN20DS      E DS
015400990806     D TNSB50DS      E DS
015500981013     D FNLV55DS      E DS
015600980529     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
015700980910     D DS_FNDFAW     E DS                  EXTNAME(FNDFA00F)  prefix(W)
015800980529     D UT§DSE0F      E DS
015900980601     D FNLV50DS      E DS
016000980601     D CNCR80        E DS
016100980601     D DTAD          E DS
016200980521      *
016300980521     D KPJBA         E DS
016400020502     D* rem                    1      3
016500020502     D* remfil                 4      6
016600980521      *
016700981013     D TRUL06DS      E DS
016800981014     D  L1                     1     90  0    DIM(30)                           P.O. COMODO
016900981014     D*?* L1              S              3  0 DIM(30)                              P.O.GESTITI
017000981013      *
017100980521     D                SDS
017200980521     D  VTCPGM                 1     10
017300980623
017400980623      *------------------------------------------------------------------------*
017500980521      *---------------  C A L C O L O  ----------------------------------------*
017600980521      *------------------------------------------------------------------------*
017700980623
017800980521     C     for01         tag
017900981001     c                   clear                   Wsce
018000980623
018100980521      * Emissione VIDEO1
018200980521     C                   exsr      EMISd01
018300980623
018400980525      * f3=Fine
018500980521     c   kc              goto      fine
018600980623
018700980525      * Controlli 1a videata
018800980521     c                   exsr      ctrd01
018900980623
019000980521     c   90              goto      for01
019100980623
019200981012      * Carico schiera con P.O. interessati
019300981012     C                   exsr      CARPOE
019400981012
019500980526      * Carico subfile
019600980526     C                   exsr      RIEd02
019700981012     c                   XFOOT     POE           WPOE
019800981001
019900981001      * Mi posiziono sulla persona che ha presentato denuncia
020000981001     C                   eval      *in45 = *on
020100981012
020200981012      * Se ci sono C.A. non elaborabili messaggio
020300990223     c                   IF        W_v2Hi59 = 'S'
020400981012     C                   eval      *in28 = *on
020500981013     c                   movel     MSG(4)        V2CMSG
020600981014     c                   ELSE
020700981014      * Controllo se ci sono P.O. che non hanno aperto C.A.
020800981014     c                   IF        WPOE > *zeros
020900981014     c                   eval      *in28 = *ON
021000981014     c                   movel     MSG(9)        V2Cmsg
021100981014     c                   clear                   wpoe
021200981014     c                   ENDIF
021300981012     c                   ENDIF
021400980623
021500980526      * Emissione VIDEO2
021600980623     C     for02         tag
021700980526     C                   exsr      EMISd02
021800980623
021900980526      * f3=Fine
022000980526     c   kc              goto      fine
022100980623
022200980526      * f12=Precedente
022300990127     c                   IF        *INKL = *ON
022400990127     c                   goto      for01
022500990127     c                   ENDIF
022600980623
022700981012      * Controllo se ci sono P.O. che non hanno aperto C.A.
022800981012     c                   IF        WPOE > *zeros
022900981012     c                   eval      *in28 = *ON
023000981013     c                   movel     MSG(9)        V2Cmsg
023100981012     c                   clear                   wpoe
023200981012     c                   goto      for02
023300981012     c                   ENDIF
023400981013
023500981013      * f7=Visualizza Evento
023600981013     c                   IF        *INKG = *ON
023700981013     c                   clear                   fidn06ds
023800981013     c                   z-add     V2Cnev        I06nev
023900981013     C                   z-add     V2Caae        I06aae
024000981013     C                   movel     'I'           I06mod
024100981013     C                   movel(P)  fidn06ds      kpjbu
024200981013     C                   CALL      'FIDN06R'
024300981013     c                   parm                    kpjba
024400981013     c                   goto      for02
024500981013     c                   ENDIF
024600981012
024700981012      * Controlli 2a videata
024800981012     c                   exsr      ctrd02
024900980623
025000980930      * Riemetto VIDEO 2 se:  non ho confermato, ci sono scelte da elaborare,
025100980930     c                   IF        *INKF = *ON and *IN90 = *OFF
025200980910     c                   exsr      SUB_confer
025300980908     c                   ELSE
025400980908     c                   goto      for02
025500980908     c                   ENDIF
025600980623
025700980521     c     fine          tag
025800980907     c                   EVAL      *INLR = *ON
025900980521      **********************************************************************
026000980521      * EMISSIONE VIDEO 1
026100980521      **********************************************************************
026200980521     C     EMISD01       BEGSR
026300980521      *
026400980930     c                   write     FI18t01
026500980930     c                   write     FI18z01
026600980930     c                   exfmt     FI18d01
026700980521      *
026800990223     c                   setoff                                       289040
026900990223     c                   setoff                                       414243
027000990223     c                   setoff                                       444546
027100990223     c                   setoff                                       474849
027200980521     c                   clear                   v1cmsg
027300980521      *
027400980521     C                   ENDSR
027500980521      **********************************************************************
027600980521      * CONTROLLI VIDEO 1
027700980521      **********************************************************************
027800980521     C     Ctrd01        BEGSR
027900990205
028000990111      * Sistemo anno di due cifre
028100990111     C                   if        V1Caae < 100  and  V1Caae > 60
028200990111     C                             and  V1Caae > *zeros
028300990111     C                   ADD       1900          V1Caae
028400990111     C                   endif
028500990111     C                   if        V1Caae < 100  and  V1Caae <= 60
028600990111     C                             and  V1Caae > *zeros
028700990111     C                   ADD       2000          V1Caae
028800990111     C                   endif
028900980924
029000980924      * Controllo N° EVENTO
029100980924      *
029200980924      * Ricerca
029300980924     C     '?'           SCAN      V1CNEV                                 31
029400980924      *
029500980924     C                   IF        *IN31 = *on
029600981014     c                   z-add     Wazn_fase     I07fca
029700981005     C                   movel     'I'           I07mod
029800981001     C                   z-add     wfgs          I07poi
029900980924      *
030000980924     C                   CALL      'FIDN07R'
030100980924     c                   parm                    kpjba
030200980924     c                   parm                    fidn07ds
030300980924      *
030400980924     C                   IF        O07err = 'E'
030500980924     c                   clear                   V1Cnev
030600980925     C                   movel     MSG(1)        V1Cmsg
030700980930     C                   seton                                          4128
030800980924     C                   ELSE
030900980924     c                   z-add     O07aae        V1Caae
031000980924     c                   movel     O07nev        V1Cnev
031100980924     C                   ENDIF
031200980924      *
031300980925     C                   eval      *in90 = *on
031400980924     C                   ENDIF
031500980924      *
031600980924      * Controlli di validità'
031700980925     c                   IF        *IN90 = *OFF
031800980924      *
031900980924     C                   IF        V1Cnev = *zeros  or  V1Cnev = *BLANKS
032000980924     C                   eval      *in90 = *on
032100980925     C                   ELSE
032200980924      *
032300980924      *    Controllo se immesso un numero valido
032400980924     C                   testn                   V1CNEV               31
032500980924     C                   IF        *IN31 = *on
032600980924     C                   move      V1CNEV        W001A
032700980924     c                   eval      *in31 = (W001A >= '0')
032800980924     C                   ENDIF
032900980924      *
033000980925     C                   IF        *IN31 = *off
033100980925     C                   movel     MSG(1)        V1CMSG
033200980930     C                   seton                                        422890
033300980925     C                   ELSE
033400980924      *
033500980924      *    Controllo esistenza evento in testata eventi
033600980929     c                   z-add     V1Caae        Kaae
033700980929     c                   movel     V1Cnev        Knev
033800980924     C     KDET          chain     FNDET000                           31
033900980924      *
034000980925     C                   IF        *IN31 = *on  OR  DETatb <> *blanks
034100980925     C                   movel     MSG(2)        V1CMSG
034200980930     C                   seton                                        422890
034300980924     C                   ENDIF
034400980928      *
034500980924     C                   ENDIF
034600980924     C                   ENDIF
034700980924     C                   ENDIF
034800980930     c   90              goto      Ectrd01
034900990111
035000990111      * Controllo Anno
035100990111     C                   IF        V1Caae = *zeros
035200990111     C                   movel     MSG(1)        V1Cmsg
035300990111     C                   seton                                        284190
035400990111     C                   ENDIF
035500990111     c   90              goto      Ectrd01
035600981005
035700981005      * Controllo se esiste almeno una C.A. per l'evento
035800981005     C     KDET          chain     FNDCT000                           31
035900981005     c                   IF        *IN31 = *ON  or  DCTfca >= Wazn_fase
036000981005     C                   seton                                        284090
036100981005     C                   movel     MSG(8)        V1Cmsg
036200981005     C                   ENDIF
036300981005     c   90              goto      Ectrd01
036400990205
036500990205      * Controllo se esiste il NUMERATORE denuncia per sede
036600990205     C                   clear                   FIDN20DS
036700990205     C                   movel     'V'           I20mod
036800990205     C                   z-add     DETaae        I20pra
036900990205     C                   movel     'DEN'         I20dit
037000990205      *
037100990205     C                   movel(P)  FIDN20DS      KPJBU
037200990205      *
037300990205     c                   CALL      'FIDN20R'
037400990205     c                   PARM                    KPJBA
037500990205      *
037600990205     C                   movel     KPJBU         FIDN20DS
037700990205      *
037800990205     c                   IF          O20err = 'E'
037900990205     c                   eval      *in28 = *ON
038000990205     C                   eval      *in90 = *on
038100990205     C                   movel     MSG(10)       V1Cmsg
038200990205     c                   goto      Ectrd01
038300990205     C                   ENDIF
038400990223
038500990223      * Controllo nominativo presentazione denuncia - obbligatorio -
038600990223     c                   IF        V1Cprd = *blanks
038700990223     C                   seton                                        289045
038800990223     C                   movel     MSG(5)        V1CMSG
038900990223     C                   ENDIF
039000990223     c   28              goto      Ectrd01
039100990223
039200990223      * Controllo data presentazione denuncia - obbligatoria -
039300990223     C                   IF        V1Cdpr =  0
039400990223     C                   MOVEL     MSG(6)        V1CMSG
039500990223     C                   SETON                                        289046
039600990223     C                   ELSE
039700990223      *
039800990223     C                   move      V1cdpr        G02DAT
039900990223     C                   eval      g02err = *blanks
040000990223     C                   CALL      'XSRDA8'
040100990223     C                   PARM                    WLBDAT
040200990223      *
040300990223     C                   if        G02ERR = '1'
040400990223     C                   SETON                                        289046
040500990223     C                   MOVEL     MSG(6)        V1CMSG
040600990223     C                   else
040700990223     C                   MOVEL     G02DAT        V1Cdpr
040800990223     C                   MOVE      G02INV        W_v1cdpr
040900990223     C                   endif
041000990223     C                   ENDIF
041100990223     c   28              goto      Ectrd01
041200990223      *
041300990223     c* Controllo Ragione sociale autorità
041400990223     c                   if        v1aurs  = *blanks
041500990223     c                   seton                                        289047
041600990223     c                   movel     msg(11)       v1cmsg
041700990223     c                   goto      Ectrd01
041800990223     c                   endif
041900990223      *
042000990223     c* Controllo Indirizzo  autorità
042100990223     c                   if        v1auin  = *blanks
042200990223     c                   seton                                        289048
042300990223     c                   movel     msg(12)       v1cmsg
042400990223     c                   goto      Ectrd01
042500990223     c                   endif
042600990223     c*
042700990223     c* Controllo Località  autorità
042800990223     c                   if        v1aulo  = *blanks
042900990223     c                   seton                                        289049
043000990223     c                   movel     msg(13)       v1cmsg
043100990223     c                   goto      Ectrd01
043200990223     c                   endif
043300990223     c*
043400990223     c* Controllo Tipo MOdulo
043500990223     c                   if        v1mode  <> 'B'  and  v1mode <> 'L'
043600990223     c                   seton                                        289044
043700990223     c                   movel     msg(16)       v1cmsg
043800990223     c                   goto      Ectrd01
043900990223     c                   endif
044000980924
044100980924     c     Ectrd01       ENDSR
044200981012      *****************************************************************
044300981012      * CARICO SCHIERA CON P.O. INTERESSATI
044400981012      *****************************************************************
044500981012     C     CARPOE        BEGSR
044600981012      *
044700981013      *  Imposto data evento
044800981014     c                   eval      Wdataev = (detaae * 10000) + detmge
044900981014     c                   clear                   POE
045000981014     c                   clear                   XX
045100981013      *
045200981014     c     Kdet          setll     FNDED000
045300981014     c     Kdet          READE     FNDED000                               30
045400981013      *
045500981012     c     *IN30         DOWEQ     *OFF
045600981013      *
045700981013      * Se sono un terminal di partenza recupero la sua £1 e carico i P.O. di secondo livello
045800981013     c                   clear                   FNLV55DS
045900981014     C                   clear                   TRUL06DS
046000981014      *
046100981013     c                   movel     'P'           D55tpt
046200981013     c                   movel     DEDfie        D55lin
046300981014     c                   z-add     Wdataev       D55drf
046400981013     c                   call      'FNLV55R'
046500981013     c                   parm                    FNLV55DS
046600981013      *
046700981014    4c                   IF        D55err = *blanks  AND  D55tfp = DEDfie
046800981014     C                   move      '£1'          D06COD
046900981014     C                   movel     DEDfie        D06KEY
047000981014     C                   movel(P)  TRUL06DS      KPJBU
047100981014     C                   call      'TRUL06R'
047200981014     C                   parm                    KPJBA
047300981014     C                   movel     KPJBU         TRUL06DS
047400981014     c                   ENDIF
047500981013      *
047600981013      * Se L1 vuota carico P.O. da file
047700981014    4c                   IF        L1(1) = *ZEROS
047800981014     c                   z-add     1             JJ
047900981014     c     DEDfie        lookup    POE(JJ)                                31
048000981014     c  N31              add       1             XX
048100981014     c  N31              z-add     DEDfie        POE(XX)
048200981014     c                   ELSE
048300981013      *
048400981014     c                   z-add     1             YY
048500981013     c                   DOW       L1(YY) > *ZEROS
048600981014     c                   z-add     1             JJ
048700981014     c     L1(YY)        lookup    POE(JJ)                                31
048800981014     c  N31              add       1             XX
048900981014     c  N31              z-add     L1(YY)        POE(XX)
049000981013     c                   add       1             YY
049100981013     c                   ENDDO
049200981014     c                   ENDIF
049300981013      *
049400981012     c     Kdet          READE     FNDED000                               30
049500981012     c                   ENDDO
049600981012      *
049700981012     c                   ENDSR
049800980608      *****************************************************************
049900980904      * LEGGO FNDCT PER CARICAMENTO SUBFILE VIDEO 2
050000980608      *****************************************************************
050100980608     C     RIEd02        BEGSR
050200980930
050300980527      * Pulisco subfile
050400980605     C                   eval      *IN21 = *OFF
050500980930     C                   WRITE     FI18C02
050600980610     C                   seton                                        2021
050700980904     C                   z-add     *ZEROS        NRR1
050800990223     c                   clear                   W_V2Hi59
050900980908      *
051000980925     c                   z-add     V1Caae        Kaae
051100980925     c                   movel     V1Cnev        Knev
051200980527      *
051300980929     C     kdet          SETLL     fndct000
051400980929     C     kdet          READE     fndct000                               30
051500980527      *
051600980604     c     *IN30         DOWEQ     *OFF
051700980907     C                   exsr      RICsbfl
051800980929     C     kdet          READE     fndct000                               30
051900980907     C                   ENDDO
052000980527      *
052100980928      * Imposto dati Control
052200980928     c                   movel     V1Cnev        V2Cnev
052300980928     c                   z-add     V1Caae        V2Caae
052400981005     c                   z-add     DETaad        V2Caad
052500981005     c                   z-add     DETpod        V2Cpod
052600981117     c                   z-add     DETnde        V2Cnde
052700981117     c                   eval      *in08 = (V2Cnde > *zeros)
052800981117     c                   movel     DETtad        V2Ctae
052900981117      *    Decodifico tipo anomalia evento
053000981117     c                   z-add     1             XX
053100981117     C     DETtad        LOOKUP    KTA(XX)                                31
053200981117     c                   movel     DTA(XX)       V2Dtae
053300980928      *    Dal file descrizioni ricavo la decodifica evento
053400980928     C                   movel     'E'           KTPD
053500980928     C                   movel     V1CAAE        W011A
053600980928     C                   move      V1CNEV        W011A
053700980928     C                   movel(P)  W011A         KNKT
053800980928     C                   clear                   Khim
053900980928     C                   clear                   Kdim
054000980928     C                   clear                   KSTD
054100980928     C                   clear                   KNKS
054200980928     C                   movel     'D'           KTRC
054300980930      *
054400981117     C     KDCS          CHAIN     FNDCS000                           31
054500981117     C  N31              movel(P)  DCSnot        V2Dnev
054600980928      *
054700980527     c                   ENDSR
054800980527      *****************************************************************
054900980907      * CONTROLLO C.A. E CARICO SUBFILE
055000980527      *****************************************************************
055100980907     C     RICsbfl       BEGSR
055200980907
055300981117      *  --------------------------------------------------------
055400980904      *  C R I T E R I   D I   S E L E Z I O N E
055500980904      *  -----------------------------------------
055600980904      * - Non elaboro le annullate
055700980928      * - Non elaboro le C.A. chiuse
055800981117      * - Elaboro se il codice fase è minore di quello ricevuto
055900981117      *  --------------------------------------------------------
056000980907
056100980907     c                   IF        DCTatb = *blanks        and                   ANNULLATA
056200980908     C                             DCTdch = *zeros         and                   C.A. CHIUSA
056300980907     C                             Wazn_fase > DCTfca                            FASE C.A.
056400980930      *
056500980529     c                   eval      wi05fca = *zeros
056600980529      *
056700980529     c                   DO        *HIVAL
056800980608      *
056900990223     c                   eval      *IN58 = *OFF
057000990223     c                   eval      *IN59 = *OFF
057100980610     c                   eval      *IN32 = *ON
057200980608     c                   clear                   FIDN05DS
057300980608      *
057400980907      * Ricerco la fase successiva.
057500980605     c                   movel     'F'           I05MOD
057600980529     c                   movel     DCTfpr        I05fpr
057700980529     c                   movel     DCTptr        I05tpc
057800980930     c                   movel     'E'           I05fpe
057900981030     c                   movel     'D'           I05fde
058000980529      *
058100980601      * Se esiste fase memorizzata utilizzo quella per la ricerca, altrimenti quella del file.
058200980529     c                   IF        WI05fca > *zeros
058300980529     c                   z-add     wi05fca       I05fca
058400980529     c                   ELSE
058500980908     c                   z-add     DCTfca        I05fca
058600980907      *  per fase esistente recupero la data di esecuzione fase
058700980721     c                   z-add     DCTaac        Kaac
058800980721     c                   z-add     DCTfil        Kfil
058900980721     c                   z-add     DCTnca        Knca
059000980907     c     kdcf          setgt     fndcf000
059100980907     c     kdcf          readpe    fndcf000                               30
059200980721     c                   if        *in30 = *off and dcffca = dctfca
059300980721     c                   z-add     DCFdfc        i05dta
059400980721     c                   endif
059500980529     c                   ENDIF
059600980907      * Valorizzo il tipo anomalia CA
059700980721     c                   move      DCTtad        i05tad
059800981125      * Valorizzo numero CA
059900981125     c                   z-add     DCTaac        i05aac
060000981125     c                   z-add     DCTfil        i05fil
060100981125     c                   z-add     DCTnca        i05nca
060200980529      *
060300980529     C                   CALL      'FIDN05R'
060400980604     c                   PARM                    KPJBA
060500980529     c                   PARM                    FIDN05DS
060600980529      *
060700980617      * Errore ricerca vado a fine
060800980529     c                   IF        O05err <> 'E'
060900980529     c                   movel     O05REC        DS_FNDFA
061000980601     c                   ELSE
061100980605     c                   iter
061200980529     C                   ENDIF
061300980721
061400980907      * Controlli sulla fase ricavata
061500980907
061600980928      * Se minore della fase ricevuta: NON OBBLIGATORIA non carico il subfile e ripeto la ricerca
061700980928      *                                    OBBLIGATORIA carico il subfile e non ripeto la ricerca
061800980907      * Se uguale alla fase ricevuta: carico il subfile e non ripeto la ricerca.
061900980907      * Se maggiore della fase ricevuta: non carico il subfile e non ripeto la ricerca.
062000980907
062100980907     c                   IF        DFAfca = Wazn_fase
062200981001     c  N07              eval      *IN07 = *ON                                  C.A. elaborabile
062300981001     c                   movel     'S'           wsce
062400980907     c                   ELSE
062500980907      *
062600980928     c                   if        DFAfca > Wazn_fase
062700980907     c                   eval      *IN32 = *ON
062800980928     c                   iter
062900980928     c                   else
063000980928      *
063100980928     c                   if        DFAobl = 'S'
063200980928     c                   eval      *IN32 = *ON
063300990223     c                   eval      *IN59 = *ON                                  Reverse Immage
063400980928     c                   else
063500980928      *
063600980907     c                   eval      *IN32 = *OFF
063700980908     c                   z-add     DFAfca        WI05fca
063800980907     c                   iter
063900980928     c                   endif
064000980928     c                   endif
064100980928      *
064200980907     c                   ENDIF
064300980907
064400980529      *
064500980608      * Carico
064600990223     c                   movel     *blanks       V2Hi58
064700990223     c   58              movel     '1'           V2Hi58
064800990223     c                   movel     *blanks       V2Hi59
064900990223     c   59              movel     '1'           V2Hi59
065000980529     c                   z-add     DCTaac        V2Haac
065100980529     c                   z-add     DCTaas        V2Haas
065200980907     c                   z-add     DCTlna        V2Hlna
065300980529     c                   z-add     DCTfil        V2Cfil
065400980601     c                   z-add     DCTnca        V2Cnca
065500980529     c                   z-add     DCTaac        V2Cdca
065600980529     c                   movel     DCTmgc        V2Cdca
065700980605     c     *mdy          move      V2Cdca        DATA_eur
065800980605     c     *dmy          move      DATA_eur      V2Cdca
065900980529     c                   move      DCTaas        V2caas2
066000980529     c                   z-add     DCTlnp        V2Clnp
066100980529     c                   z-add     DCTnrs        V2Cnrs
066200980529     c                   z-add     DCTnsp        V2Cnsp
066300980529      *
066400980907      * Decodifico il tipo anomalia
066500980928     c                   z-add     1             XX
066600980928     C     DCTtad        LOOKUP    KTA(XX)                                31
066700980928     c                   movel     DCTtad        V2Ctad
066800980928     c                   movel     DTA(XX)       V2Dtad
066900980928      *
067000980928      * Decodifico la fase attuale
067100980928     c                   z-add     1             XX
067200980929     C     DCTfca        LOOKUP    KFC(XX)                                31
067300980929     c                   z-add     DCTfca        V2Cfca
067400980929     c                   movel     DFC(XX)       V2Dfca
067500980610      *
067600981012      * Memorizzo se C.A. in fase non gestibile
067700990223     c                   IF        V2Hi59 = '1'  and W_V2Hi59 <> 'S'
067800990223     c                   movel     'S'           W_V2Hi59
067900981012     C                   ENDIF
068000981012      *
068100981012      * Elimino P.O. dalla schiera delle gestibili
068200981012     c                   z-add     1             XX
068300981012     C     DCTfca        LOOKUP    POE(XX)                                31
068400981012     c                   z-add     *zeros        POE(xx)
068500981012      *
068600980529     c                   add       1             NRR1
068700980930     c                   write     FI18s02
068800980529      *
068900980604     c  N32              ENDDO
069000980529      *
069100980907     C                   ENDIF
069200980929      *
069300980907     c                   ENDSR
069400980601      **********************************************************************
069500980601      * EMISSIONE VIDEO 2
069600980601      **********************************************************************
069700980601     C     EMISD02       BEGSR
069800980601      *
069900980610     c                   z-add     1             V2Cnrr
070000980624     c   90              z-add     nrr1          V2Cnrr
070100980604     c                   seton                                        2021
070200980604      *
070300980930     c                   write     FI18Z02
070400980930     c                   exfmt     FI18C02
070500980608      *
070600980624     c                   setoff                                       2890
070700980601     c                   clear                   v2cmsg
070800980601      *
070900980601     C                   ENDSR
071000980601      **********************************************************************
071100980601      * CONTROLLI VIDEO 2
071200980601      **********************************************************************
071300980601     C     Ctrd02        BEGSR
071400980908
071500980930     c                   setoff                                       4546
071600980930
071700980908      *  Controllo scelte sbf
071800980623     c                   z-add     1             nrr2
071900980623      *
072000980930     c     NRR2          CHAIN     FI18S02                            32
072100980601      *
072200980604     c     *IN32         doweq     *OFF
072300980624     c     *IN90         andeq     *OFF
072400980622      *
072500980622     c     V2Csce        LOOKUP    OPZ                                    31
072600980622      *
072700980622     c                   SELECT
072800980622      *
072900980623     c                   WHEN      *IN31 = *OFF                                 OPZ non gestita
073000980907     c                   movel     MSG(2)        V2Cmsg
073100990223     c                   seton                                        289057
073200980702      *
073300980722     c                   WHEN      V2Csce = '8'                                 NOTE
073400980722     c                   exsr      SUB_note
073500980826     c                   movel     *blanks       V2Csce
073600980826     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
073700980622      *
073800980928     c                   WHEN      V2Csce = '9'                                 VISUALIZZA SPED.
073900980930     c                   movel     'S'           wtpb
074000980624     c                   exsr      SUB_visbol
074100980624     c                   movel     *blanks       V2Csce
074200980624     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
074300980610      *
074400980928     c                   WHEN      V2Csce = '5'                                 VISUALIZZA C.A.
074500980930     c                   movel     'S'           wtpb
074600980928     c                   exsr      SUB_visca
074700980702     c                   movel     *blanks       V2Csce
074800980702     c                   eval      *in90 = (O00F03 = 'S')                       per F3 torno al SFL
074900980622      *
075000980622     c                   ENDSL
075100980622      *
075200990223     c                   eval      *IN58 = (V2HI58 = '1')
075300990223     c                   eval      *IN59 = (V2HI59 = '1')
075400980623      *
075500980930     c                   update    FI18s02
075600990223     c   57              eval      *IN57 = *OFF
075700980623      *
075800980624     c                   IF        *IN90 = *OFF
075900980623     c                   add       1             nrr2
076000980930     c     NRR2          CHAIN     FI18S02                            32
076100980804     c                   ENDIF
076200980604      *
076300980623     c                   enddo
076400990127     c   28              goto      Ectrd02
076500990127
076600980623      *
076700980908     C     Ectrd02       ENDSR
076800980604      **********************************************************************
076900980624      * VISUALIZZAZIONE SPEDIZIONE
077000980604      **********************************************************************
077100980624     C     SUB_VISBOL    BEGSR
077200980604      *
077300980624     c                   movel     *blanks       WF03
077400980624      *
077500990806     c                   clear                   TNSB50DS
077600990910     C                   MOVEL     'D05'         I50OP0
077700990806     C                   z-add     V2Haas        D50aas
077800990806     C                   z-add     V2Clnp        D50lnp
077900990806     C                   z-add     V2Cnrs        D50nrs
078000990806     C                   z-add     V2Cnsp        D50nsp
078100990806      *
078200990806     C                   MOVEL(P)  TNSB50DS      KPJBU
078300990806     C                   CALL      'TNSB50R'
078400990806     C                   PARM                    KPJBA
078500990806      *
078600990806     C                   movel     KPJBU         TNSB50DS
078700990806     c                   if        O50F03 <> *BLANKS
078800990806     C                   movel     'S'           WF03
078900990806     c                   endif
079000980624      *
079100980624     C                   ENDSR
079200980722      **********************************************************************
079300980722      * VISUALIZZO NOTE
079400980722      **********************************************************************
079500980722     C     SUB_note      BEGSR
079600980722      *
079700980722     C                   clear                   FIDN10DS
079800980722     c     *dmy          move      V2Cdca        DATA_eur
079900980722     c     *eur          move      DATA_eur      i10dta
080000980722     c                   movel     'N'           i10trc
080100980722     c                   movel     'C'           i10tpd
080200980722      *
080300980804     c                   movel     'V'           i10flm
080400980722     c                   movel     dsnumca       i10nkt
080500980907     c                   movel     Wazn_fase     i10nks
080600980722      *
080700980722     c                   movel(P)  fidn10ds      kpjbu
080800980722     C                   CALL      'FIDN10R'
080900980722     C                   PARM                    kpjba
081000980826      *
081100980826     c                   movel     *blanks       WF03
081200980826     C                   movel     KPJBU         fidn10ds
081300980826     c                   if        o10f03    <> *BLANKS
081400980826     C                   movel     'S'           WF03
081500980826     c                   endif
081600980722      *
081700980722     C                   ENDSR
081800980624      **********************************************************************
081900980907      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 5
082000980624      **********************************************************************
082100980928     C     SUB_VISCA     BEGSR
082200980624      *
082300980604     c                   clear                   FIDN00DS
082400980604      *
082500980610     c                   movel     'I'           I00mod
082600980907     c                   movel     Wazn_fase     I00fca
082700981001     c                   z-add     46            I00fgs
082800980604     c                   z-add     V2Haac        I00aac
082900980604     c                   z-add     V2Cfil        I00fil
083000980604     c                   z-add     V2Cnca        I00nca
083100980604     c                   z-add     V2Haas        I00aas
083200980604     c                   z-add     V2Clnp        I00lnp
083300980604     c                   z-add     V2Cnrs        I00nrs
083400980604     c                   z-add     V2Cnsp        I00nsp
083500980604     c                   z-add     V2Hlna        I00lna
083600980604     c                   movel     V2Ctad        I00tad
083700980624     c                   movel     Wtpb          I00tpb
083800980604      *
083900980604     c                   movel     FIDN00DS      kpjbu
084000980623      *
084100980907     c                   call      'FIDN01R'
084200980604     c                   parm                    kpjba
084300980604      *
084400980604     C                   ENDSR
084500980907      *****************************************************************
084600980907      * CONTROLLO CODICE FASE RICEVUTA
084700980907      *****************************************************************
084800980907     C     CTLAZNFASE    BEGSR
084900981001      *
085000980907     C                   IF        AZN_fase = *blanks
085100980907     c                   eval      *in28 = *ON
085200980907     c                   ELSE
085300980907      *
085400980907     C                   testn                   AZN_fase             31
085500980907     C   31              move      AZN_fase      W001A
085600980907     C   31              eval      *in31 = (W001A >= '0')
085700980907      *
085800980907     C                   IF        *IN31 = *OFF
085900980907     c                   eval      *in28 = *ON
086000980907     c                   ELSE
086100980907      * Controllo
086200981001     c                   clear                   DS_FNDFAW
086300980907     c                   clear                   FIDN05DS
086400980907     c                   movel     'C'           I05MOD
086500980907     C                   move      AZN_fase      I05fca
086600980907     C                   CALL      'FIDN05R'
086700980907     C                   PARM                    KPJBA
086800980907     c                   PARM                    FIDN05DS
086900981001      *
087000981001     c                   IF        O05rec = *blanks  OR  O05err <> *blanks
087100981001     c                   eval      *in28 = *ON
087200981001     c                   ELSE
087300981001      *
087400980910     c                   movel     O05REC        DS_FNDFAW
087500981001     c                   IF        WDFAatb <> *blanks
087600981001     c                   eval      *in28 = *ON
087700981001     c                   ELSE
087800981001      *  Controllo eseguibilità
087900981001     c                   IF        (WDFAgfs = 'S'  and  SIMFEL <> *zeros)  or
088000981001     c                             (WDFAgfs <> 'S'  and  WDFAgfs <> *blanks
088100981001     c                                and  SIMFEL = *zeros)
088200981001     c                   eval      *in28 = *ON
088300981001     C                   ENDIF
088400981001     C                   ENDIF
088500981001     C                   ENDIF
088600980907      *
088700980907     C                   ENDIF
088800980907     C                   ENDIF
088900980907      *
089000980907     c                   ENDSR
089100980910      *****************************************************************
089200980910      * RICHIAMO PROGRAMMA DI STAMPA
089300980910      *****************************************************************
089400980910     C     SUB_confer    BEGSR
089500980929
089600981001      * Richiamo pgm se ho almeno una C.A. selezionata
089700981001     c                   IF        Wsce = 'S'
089800981001      *
089900981001     c                   clear                   FIDN18DS
090000981001      *
090100981001     c                   movel     'G'           I18mod
090200981001     c                   z-add     Wazn_fase     I18fca
090300981001     c                   z-add     Wfgs          I18fgs
090400981001     c                   z-add     V2Caae        I18aae
090500981001     c                   z-add     V2Cnev        I18nev
090600981001     c                   z-add     DETfce        I18fce
090700990223     c                   movel     V1Cprd        I18prd
090800990223     c                   z-add     W_V1Cdpr      I18dpr
090900981001      *
091000981001     c                   movel     FIDN18DS      kpjbu
091100981001      *
091200981001     c                   call      Wdfapgm
091300981001     c                   parm                    kpjba
091400981001      *
091500981001     c                   ENDIF
091600981001
091700980910     c                   ENDSR
091800980910      *****************************************************************
091900980527      * ROUTINE INIZIALE
092000980527      *****************************************************************
092100980527     C     *INZSR        BEGSR
092200980615      *
092300980521     C     *ENTRY        PLIST
092400980521     C                   PARM                    KPJBA
092500980521     C                   MOVEL     KPJBU         DS_AZIONE
092600980907      *
092700980907     C                   Z-ADD     1             CODUT
092800980907     C                   CALL      'X§PARUT'
092900980907     C                   PARM                    UT§DSE0F
093000980907     C                   MOVEL     RAGUT         RSUT
093100980907     C                   MOVEL     REC80         CNCR80
093200980907
093300980907      * Controllo il tipo azione. Se non è valido vado a fine pgm, viceversa imposto costanti.
093400980904     C                   IF        AZN_tipo = 'G'
093500980604     c                   movel     TIT_G         VTCtit
093600980622     c                   movel     DOPZ1_G       V2Dse1
093700980622     c                   movea     OPZ_G         OPZ
093800980904     C                   ELSE
093900980521     C                   goto      FINE
094000980904     C                   ENDIF
094100980907
094200980907      * Controllo il codice fase. Se non è valido o non è impostato vado a fine pgm.
094300980907     c                   EXSR      CTLaznfase
094400980907     C   28              goto      FINE
094500980907     c                   movel     AZN_fase      Wazn_fase
094600981001      *
094700981001      * IMPOSTO IL P.O. IN GESTIONE
094800020502     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
094900020502     C*                  movel     REMFIL        WFGS
095000020502   X2C*                  ELSE
095100020502     C*                  IF        SIMFEL = *zeros
095200020502     C*                  z-add     46            WFGS
095300020502     c*                  ELSE
095400020502     C*                  movel     SIMFEL        WFGS
095500020502    2C*                  ENDIF
095600020502    2C*                  ENDIF
095700020502     c                   movel     simpou        wfgs
095800981001      *
095900980721
096000980928      * CARICO SCHIERA TABELLA TAD
096100980928     c                   z-add     *zeros        xx
096200980928     C                   MOVEL     'TAD'         Wtbecod
096300980928     c     Wtbecod       chain     tntbe000                           31
096400980928      *
096500980928     c                   DOW       *in31 = *off
096600980928      * Se il sistema informativo non è uguale a blank deve essere uguale al mio
096700980928     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
096800980928      *                            and TBEatb = *blanks
096900980928     c                   add       1             xx
097000980928     c                   movel     TBEke1        KTA(XX)
097100980928     c                   movel     TBEuni        dtad
097200980928     c                   movel     §TADdesc      DTA(XX)
097300980928     c                   endif
097400980928     c     Wtbecod       reade     tntbe000                               31
097500980928     c                   enddo
097600980928
097700980928      * CARICO SCHIERA FASI C.A.
097800980928     c                   z-add     *zeros        xx
097900980928     C     *loval        setll     FNDFA000
098000980928     C                   read      FNDFA000                               31
098100980928      *
098200980928     c                   DOW       *in31 = *off
098300980928     c                   IF        DFAatb = *blanks
098400980928     c                   add       1             xx
098500980928     c                   movel     DFAfca        KFC(XX)
098600980928     c                   movel     DFAdes        DFC(XX)
098700980928     c                   endif
098800980928     C                   read      FNDFA000                               31
098900980928     c                   enddo
099000980928
099100980721      * reperisco data e ora
099200980721     C                   TIME                    W0140
099300980521      * UDATE IN GGMMAAAA
099400980521     C                   MOVE      W0140         WDTGIO
099500980929     C                   MOVEL     W0140         WORA
099600980521      * UDATE IN AAAAMMGG
099700980521     C                   Z-ADD     WDTGIO        G02DAT
099800980521     C                   MOVEL     *BLANK        G02ERR
099900980521     C                   CALL      'XSRDA8'
100000980521     C                   PARM                    WLBDAT
100100980521     C                   MOVEL     G02INV        DATEU
100200980930      *    Imposto anno denuncia
100300980930     c                   move      wdtgio        V1Caae
100400980721
100500980526      *
100600980929     C     Kdet          klist
100700980929     C                   KFLD                    kaae
100800980929     C                   KFLD                    knev
100900980908      *
101000980527     C     kspe          KLIST
101100980527     C                   KFLD                    kAAS
101200980527     C                   KFLD                    kLNP
101300980527     C                   KFLD                    kNRS
101400980527     C                   KFLD                    kNSP
101500980522      *
101600980608     c     kdcf          klist
101700980608     c                   kfld                    Kaac
101800980608     c                   kfld                    kfil
101900980608     c                   kfld                    knca
102000980915      *
102100980928     C     KDCS          KLIST
102200980928     C                   KFLD                    KTPD
102300980928     C                   KFLD                    KNKT
102400980928     C                   KFLD                    KSTD
102500980928     C                   KFLD                    KDIM
102600980928     C                   KFLD                    KHIM
102700980928     C                   KFLD                    KNKS
102800980928     C                   KFLD                    KTRC
102900980929      *
103000980929     C     KDCT          KLIST
103100980929     C                   KFLD                    V2HAAC
103200980929     C                   KFLD                    V2CFIL
103300980929     C                   KFLD                    V2CNCA
103400980915
103500980521     C                   ENDSR
103600980929      *---------------------------------------------------------------------------------------------
103700980521** MSG  Lungh. 78                                                            *
103800980925Immettere Numero Evento valido                                                 1
103900980925Numero Evento inesistente o annullato                                          2
104000980924Scegliere un opzione valida                                                    3
104100981012ATTENZIONE: ci sono C.A. in fase non corretta                                  4
104200980930Inserire il nominativo presentazione denuncia                                  5
104300980930Immettere data valida                                                          6
104400990223LIBERO                                                                         7
104500981005Non esistono C.A. elaborabili per questo evento                                8
104600981012Alcuni P.O. non hanno aperto C.A. per l'evento. Premere F7 per il dettaglio    9
104700990127ATTENZIONE: manca il numeratore denuncia eventi avvertire l'ufficio EDP SEDE   10
104800990223Inserire Ragione Sociale Autorità                                              11
104900990223Inserire Indirizzo Autorità                                                    12
105000990223Inserire Località Autorità                                                     13
105100990224LIBERO                                                                         14
105200990224LIBERO                                                                         15
105300990223Immettere un tipo modulo corretto                                              16
