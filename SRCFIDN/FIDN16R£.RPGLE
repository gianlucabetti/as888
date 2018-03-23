000100981007      *-------------------------------------------------------------------------*
000200981007      *                     INVIO DENUNCIA                                      *
000300981007      *-------------------------------------------------------------------------*
000400981117      * EVENTI da Sede a P.O. presentazione                                     *
000500981007      *-------------------------------------------------------------------------*
000600981117      * Non controllo se le azioni sono già state eseguite per permettere ristampe
000700981117      *-------------------------------------------------------------------------*
000800980721
000900980721      ****************************************************************
001000981007      *  FASI C.A. FISSATE A PROGRAMMA
001100980721      ****************************************************************
001200981015      *  515 = invio copia denuncia al P.O. evento     (Wfdev)
001300980721      ***************************************************************
001400980623
001500980521      ****************************************************************
001600980521      *  RIEPILOGO INDICATORI
001700980521      ***************************************************************
001800980521      * 05 - SPROTEGGO P.O. IN GESTIONE
001900980904      * 06 - NON ABILITO TASTO FUNZIONALE F18
002000981001      * 07 - NON VISUALIZZO "P.O. IN GESTIONE" PER SEDE
002100980923      * 28 - ERRORE GENERICO DSPF
002200980904      * 30 - COMODO
002300980904      * 31 - COMODO
002400980904      * 40 - ERRORE p.o. in gestione
002500980923      * 41 - ERRORE numero denuncia
002600981007      * 42 - ERRORE anno pratica assicurativa
002700980904      ***************************************************************
002800980623
002900980521     H DECEDIT('0,') DATEDIT(*DMY.)
003000980623
003100981001     fFIDN16D   CF   E             WORKSTN
003200981001     fFNDET02L  UF   E           K DISK
003300981001     fFNDCT05L  UF   E           K DISK
003400981001     fFNDCF01L  UF A E           K DISK
003500981001     fAZORG01L  IF   E           K DISK
003600980521      *------------------------------------------------------------------------*
003700000228     D L6              S              3  0 DIM(30)                              P.O.GESTITI
003800000228
003900981028     D Kaad            S                   LIKE(DCTaad)
004000980904     D Kpod            S                   LIKE(DCTpod)
004100980904     D Knde            S                   LIKE(DCTnde)
004200980907     D wazn_fase       S                   LIKE(DCTFCA)
004300981001     D wfpe            S                   LIKE(i05fpe)
004400981015     D wfdev           S                   LIKE(DCTfca)  INZ(515)
004500980923
004600980907     D xx              S              3  0
004700980923     D WORA            S              4  0
004800980721     D W0140           S             14  0
004900980521     D Wdtgio          S              8  0
005000980521     D dateu           S              8  0
005100981028     D WPTF            S              4
005200981028     D WFPTF           S              7
005300981028     D WAfca           S              3
005400980923     D W001A           S              1
005500981012     D Wprimaca        S              1
005600981028     D WCAvalida       S              1
005700980923
005800980604      *
005900980622      *  titolo videata (lunghezza massima 34)
006000981211     D TIT_fdev        C                   CONST('EVENTI-INVIO DENUNCIA X VIDIM-
006100021107     D                                     AZIONE')
006200981001      *
006300980521      *   S C H I E R E
006400981117     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
006500981028     D PTF             S              4    DIM(100)                             PTR + TAD + FPE
006600981028     D GFS             S              1    DIM(100)                             P.O. gestione fase
006700981028     D FPTF            S              7    DIM(100)                             FCA+PTR+TAD+FPE
006800980521      *
006900980521      *   D S   I N T E R N E / E S T E R N E
007000980525      *
007100980521      * Ricezione dati da azione
007200980521     D DS_AZIONE       DS                  INZ
007300980521     D  AZN_tipo               1      1
007400980521     D  AZN_fase               2      4
007500980521     D  AZN_trpr               5      5
007600980923      *
007700980923     D WLBDAT          DS                  INZ
007800980923     D  G02DAT                 1      8  0
007900980923     D  G02INV                 9     16  0
008000980923     D  G02ERR                17     17
008100980923     D  G02TGI                18     22  0
008200980915      *----------------------------------
008300980929     D FIDN00DS      E DS
008400980929     D FIDN05DS      E DS
008500980529     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
008600980529     D UT§DSE0F      E DS
008700980601     D FNLV50DS      E DS
008800980601     D CNCR80        E DS
008900981229     D DDCT01        E DS
009000000228      * DS PER trul06r - caricamento £X
009100000228     D TRUL06DS      E DS
009200000228     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
009300980521      *
009400980521     D KPJBA         E DS
009500980521      *
009600980521     D                SDS
009700980521     D  VTCPGM                 1     10
009800980623
009900980623      *------------------------------------------------------------------------*
010000980521      *---------------  C A L C O L O  ----------------------------------------*
010100980521      *------------------------------------------------------------------------*
010200980623
010300980521     C     for01         tag
010400980623
010500980521      * Emissione VIDEO1
010600980923     c                   write     FI16t01
010700980923     c                   write     FI16z01
010800981012     c                   exfmt     FI16d01
010900980923
011000980923     c                   setoff                                       284041
011100980923     c                   clear                   v1cmsg
011200980623
011300980525      * f3=Fine
011400980521     c   kc              goto      fine
011500980623
011600980521      * f18=Cambio P.O. in gestione
011700980522     c   ks              eval      *in05 = *on
011800980521     c   ks              goto      for01
011900980623
012000980521      * 05 on --> controllo P.O. in gestione
012100980521     c     *in05         ifeq      *on
012200980521     C                   exsr      ctrfgs
012300980923     c   28              goto      for01
012400980608     c                   eval      *in05 = *off
012500980521     c                   end
012600980623
012700980525      * Controlli 1a videata
012800980521     c                   exsr      ctrd01
012900980623
013000980923      * Se non ho confermato o ci sono errori riemetto video 1
013100980923     c                   IF        *INKF = *ON  and  *IN28 = *OFF
013200980910     c                   exsr      SUB_confer
013300981009     c   28              goto      for01
013400980908     c                   ELSE
013500980923     c                   goto      for01
013600980908     c                   ENDIF
013700980623
013800980521     c     fine          tag
013900980907     c                   EVAL      *INLR = *ON
014000980521      **********************************************************************
014100980521      * CONTROLLO IL P.O. IN IN GESTIONE
014200980521      **********************************************************************
014300980521     C     CTRFGS        beGSR
014400980521      *
014500980521     c                   clear                   v1dfgs
014600980609     C                   clear                   FNLV50DS
014700980521     C                   MOVEL     KNMUS         D50PRU
014800980521     C                   MOVEL     V1cfgs        D50FGS
014900980521     C                   CALL      'FNLV50R'
015000980521     C                   PARM                    FNLV50DS
015100980521      *
015200980521     C                   if        D50ERR <> *blanks
015300980521     C                   MOVEL     D50MSG        V1cMSG
015400980923     C                   SETON                                        4028
015500980609     C                   else
015600980609     c     v1cfgs        CHAIN     azorg                              31
015700980609     c  n31              MOVEL     orgdes        v1dfgs
015800000228     C                   CLEAR                   L6
015900000228     C                   EXSR      CARL6
016000000228     C                   MOVEA     LIN           L6
016100980910     C                   endif
016200980521      *
016300980609     c                   endsr
016400980521      **********************************************************************
016500980521      * CONTROLLI VIDEO 1
016600980521      **********************************************************************
016700980923     C     CTRD01        BEGSR
016800981012
016900981012      * Controllo Anno Denuncia
017000981027     C                   IF        V1Caad = *zeros
017100981117     C                   movel     MSG(2)        V1Cmsg
017200980923     C                   seton                                        2841
017300980904     C                   ELSE
017400980923      *   sistemo anno di due cifre
017500981027     C                   if        V1Caad < 100  and  V1Caad > 60
017600981027     C                   ADD       1900          V1Caad
017700980923     C                   endif
017800981027     C                   if        V1Caad < 100  and  V1Caad <= 60
017900981027     C                   ADD       2000          V1Caad
018000980923     C                   endif
018100980923     C                   ENDIF
018200980923     c   28              goto      Ectrd01
018300980923
018400980923      * Controllo N° Denuncia
018500980923     C                   IF        V1Cnde = *zeros
018600981117     C                   movel     MSG(2)        V1Cmsg
018700980923     C                   seton                                        2841
018800980923     C                   ELSE
018900980923      *
019000981012     C                   z-add     V1Cfgs        Kpod
019100981027     C                   z-add     V1Caad        Kaad
019200980923     C                   z-add     V1Cnde        Knde
019300980923     C     KDET          CHAIN     FNDET000                           31
019400980904      *
019500980923     C                   IF        *in31 = *on
019600980923     C                   movel     MSG(1)        V1Cmsg
019700980923     C                   seton                                        2841
019800981007     C                   ENDIF
019900980923     C                   ENDIF
020000981012     c   28              goto      Ectrd01
020100980923
020200980923     c     Ectrd01       ENDSR
020300980608      *****************************************************************
020400980923      * AGGIORNO ARCHIVI
020500980608      *****************************************************************
020600980923     C     SUB_confer    BEGSR
020700981007
020800981012     C     KDET          setll     FNDCT000
020900980923      *
021000981012     c                   DO        *HIVAL
021100981012     C     KDET          READE     FNDCT000                               31
021200981012     c   31              LEAVE
021300981012      *
021400981028      *  Scarto C.A. con fase superiore o uguale a quella in esame oppure non in gestione a V1Cfgs
021500000228     c                   IF        DCTfca >= WAZN_fase
021600981012     c                   iter
021700981012     c                   ENDIF
021800000228      * se p.o. gestione della CA non è uguale a quello a video verifico se presente in L6
021900000228     c                   IF        V1Cfgs <> DCTgfc
022000000228     c     DCTGFC        lookup    L6                                     30
022100000228     c  n30              iter
022200000228     c                   endif
022300981028      *
022400981028      *  Controllo se la C.A. è in una fase corretta per essere elaborata
022500981028     c                   EXSR      CTR_CAval
022600981012      *
022700981028      *  Scarto C.A. con fase non valida per l'elaborazione
022800981028     c                   IF        WCAvalida <> 'S'
022900981012     c                   iter
023000981012     c                   ENDIF
023100981028      *
023200981028      *  Ricavo il P.O. gestore della fase successiva
023300981229     c                   movel     DCTflo        DDCT01
023400981028     c                   EXSR      RICGFC
023500981012      *
023600980923      * Imposto campi per trasmissione dati
023700980923     c                   SELECT
023800980923     c                   WHEN      DFAgfs = 'P'
023900981229     c                   movel     §DCTlnpc      dctgfc
024000980923     c                   WHEN      DFAgfs = 'S'
024100980923     c                   z-add     46            dctgfc
024200981023     c                   WHEN      DFAgfs = 'A'
024300981023     c                   z-add     DCTlna        dctgfc
024400980923     c                   OTHER
024500981023     c                   z-add     DCTfil        dctgfc
024600980923     c                   ENDSL
024700981028      *
024800980923     c                   z-add     Wazn_fase     DCTfca
024900981222     c                   clear                   DCTft1
025000981222     c                   clear                   DCTft2
025100981012      *
025200981028      * Alla prima C.A. elaborabile aggiorno FNDET e, se previsto, ricavo il n° Pratica assicurativa
025300981009     c                   IF        Wprimaca = *blanks
025400981012     c                   exsr      ELAPrCAVal
025500981012     c                   ENDIF
025600981009     c   28              leave
025700981009      *
025800981007      * Aggiorno testata C.A.
025900021108     c                   if        AZN_tipo = 'G'
026000980923     c                   EXCEPT    UPDdct
026100021108     c                   endif
026200980923      *
026300980923      * Scrivo fasi C.A.
026400980923     c                   EXSR      WRTDCF
026500980923      *
026600980923     c                   ENDDO
026700981007
026800981009     c                   IF        *in28 = *off
026900981007      * Richiamo PGM di stampa lettera
027000981007     c                   clear                   FIDN00DS
027100981007      *
027200981007     c                   movel     'R'           I00mod                         Non aggiorna files
027300981007     c                   z-add     Wazn_fase     I00fca
027400981007     c                   z-add     V1Cfgs        I00fgs
027500981007     c                   z-add     DETaae        I00aae
027600981007     c                   z-add     DETnev        I00nev
027700981007     c                   z-add     DETmge        I00dev
027800981007     c                   movel     DETaae        I00dev
027900981028     c                   movel     DETtad        I00tae
028000981028     c                   z-add     DETfce        I00fce
028100981027     c                   z-add     V1Caad        I00aad
028200981012     C                   z-add     V1Cfgs        I00pod
028300981007     c                   z-add     V1Cnde        I00nde
028400981007     c                   z-add     DETdsb        I00dsb
028500981028     c                   movel     DETdit        I00dit
028600000110     c                   Z-ADD     DETpra        I00pra
028700981027     c                   Z-ADD     DETprn        I00prn
028800981007      *
028900981007     c                   movel     FIDN00DS      kpjbu
029000981007      *
029100981007     c                   call      'FIDN09R'
029200981007     c                   parm                    kpjba
029300980923      *
029400981009     c                   ENDIF
029500981009      *
029600980923     c                   ENDSR
029700981028      *****************************************************************
029800981028      * CONTROLLO SE C.A. ABILITATA A QUESTA FASE E RICAVO GESTORE FASE SUCCESSIVA
029900981028      *****************************************************************
030000981028     C     CTR_CAval     BEGSR
030100981028      *
030200981028     c                   clear                   WCAvalida
030300981028      *
030400981028     c                   if        DCTnev > *zeros
030500981028     c                   movel     'E'           wfpe
030600981028     c                   else
030700981028     c                   movel     'N'           Wfpe
030800981028     c                   endif
030900981028      *
031000981028      * Ricerco in schiera se ho gia effettuato questa ricerca con successo
031100981028     c                   z-add     1             XX
031200981028     c                   movel     DCTfca        WAfca
031300981028     c                   eval      WFPTF = WAfca + DCTptr + DCTtad + Wfpe
031400981028     c     WFPTF         LOOKUP    FPTF(XX)                               32
031500981028      *
031600981028     c                   IF        *IN32 = *ON
031700981028     c                   eval      WCAvalida = 'S'
031800981028     c                   ELSE
031900981028      *
032000981028      * Ricerco fase successiva
032100981028     c                   CLEAR                   FIDN05DS
032200981028     c                   movel     'F'           I05MOD
032300981028     c                   z-add     DCTfca        I05fca
032400981028     c                   movel     'P'           I05fpr
032500981028     c                   movel     dctptr        I05tpc
032600981028     c                   movel     Wfpe          I05fpe
032700981028     c                   movel     'O'           I05ffs
032800981028     c                   z-add     DATEU         i05dta
032900981028     c                   move      DCTtad        i05tad
033000981125      * Valorizzo numero CA
033100981125     c                   z-add     DCTaac        i05aac
033200981125     c                   z-add     DCTfil        i05fil
033300981125     c                   z-add     DCTnca        i05nca
033400981028
033500981028     C                   CALL      'FIDN05R'
033600981028     c                   PARM                    KPJBA
033700981028     c                   PARM                    FIDN05DS
033800981028      *
033900981028      * Ricercando solo le obbligatorie anche se la fase ricavata è diversa esco
034000981028     c                   eval      *in32 = *on
034100981028      *
034200981028     c                   IF        O05err = *blanks
034300981028     c                   movel     O05REC        DS_FNDFA
034400981028      *
034500981028     c                   IF        DFAfca = WAZN_fase
034600981028     c                   eval      WCAvalida = 'S'
034700981028     c                   z-add     1             XX
034800981028     c                   movel     DFAfca        WAfca
034900981028     c     *blanks       lookup    FPTF(XX)                               30
035000981028     c   30              eval      FPTF(XX) = WAfca + DCTptr + DCTtad + Wfpe
035100981028     c                   ENDIF
035200981028     c                   ENDIF
035300981028      *
035400981028     c                   ENDIF
035500981028      *
035600981028     c                   ENDSR
035700981007      *****************************************************************
035800981028      * CONTROLLO SE C.A. ABILITATA A QUESTA FASE E RICAVO GESTORE FASE SUCCESSIVA
035900981007      *****************************************************************
036000981007     C     RICGFC        BEGSR
036100981007      *
036200981028      * Ricerco in schiera se ho gia effettuato una ricerca con questi parametri
036300981007     c                   Z-add     1             XX
036400981028     c                   eval      WPTF = DCTptr + DCTtad + wfpe
036500981028     c     WPTF          LOOKUP    PTF(XX)                                30
036600981007      *
036700981007     c                   IF        *IN30 = *ON
036800981007     c                   movel     GFS(XX)       DFAgfs
036900981007     c                   ELSE
037000981007      *
037100981007     c                   CLEAR                   FIDN05DS
037200981007     c                   movel     'F'           I05MOD
037300981028     c                   z-add     WAZN_fase     I05fca
037400981007     c                   movel     'P'           I05fpr
037500981007     c                   movel     dctptr        I05tpc
037600981007     c                   movel     Wfpe          I05fpe
037700981007     c                   movel     'O'           I05ffs
037800981007     c                   z-add     DATEU         i05dta
037900981007     c                   move      dcttad        i05tad
038000981125      * Valorizzo numero CA
038100981125     c                   z-add     DCTaac        i05aac
038200981125     c                   z-add     DCTfil        i05fil
038300981125     c                   z-add     DCTnca        i05nca
038400981007
038500981007     C                   CALL      'FIDN05R'
038600981007     c                   PARM                    KPJBA
038700981007     c                   PARM                    FIDN05DS
038800981007
038900981007     c                   IF        O05err = *blanks
039000981007     c                   movel     O05REC        DS_FNDFA
039100981007     c                   Z-add     1             XX
039200981028     c     *blanks       LOOKUP    PTF(XX)                                30
039300981028     c   30              eval      PTF(XX) = DCTptr + DCTtad + Wfpe
039400981007     c   30              movel     DFAgfs        GFS(XX)
039500981007     c                   ENDIF
039600981007      *
039700981007     c                   ENDIF
039800981007      *
039900981007     c                   ENDSR
040000981012      *****************************************************************
040100981012      * ELABORAZIONI PER PRIMA C.A. VALIDA
040200981012      *****************************************************************
040300981012     C     ELAPrCAVal    BEGSR
040400981012      *
040500981117      * Imposto la Data Invio a Sede o al P.O.
040600021107     C                   IF        DETdds = *zeros
040700981012     c                   Z-ADD     DATEU         DETdds
040800981117     c                   ENDIF
040900981012      *
041000981028     c                   if         *IN28 = *OFF
041100981028     c                   MOVEL     *blanks       DETftr
041200981028     c                   eval      Wprimaca = 'S'
041300021108     c                   if        AZN_tipo = 'G'
041400981028     c                   UPDATE    FNDET000
041500021108     c                   endif
041600981028     c                   endif
041700981012      *
041800981012     c                   ENDSR
041900980923      *****************************************************************
042000980923      * AGGIORNO FNDCF FASI C.A.
042100980923      *****************************************************************
042200980923     C     WRTDCF        BEGSR
042300980923      *
042400981028     C                   z-add     DCTaac        DCFaac
042500980923     C                   z-add     DCTfil        DCFfil
042600980923     C                   z-add     DCTnca        DCFnca
042700980923     c                   z-add     Wazn_fase     DCFfca
042800980923     c                   z-add     dateu         DCFdfc
042900980923     c                   z-add     wora          DCFhfc
043000980923     c                   z-add     V1Cfgs        DCFfev
043100980923     c                   movel     knmus         DCFpru
043200980923      *
043300021108     c                   if        AZN_tipo = 'G'
043400980923     c                   WRITE     FNDCF000
043500021108     c                   endif
043600980923      *
043700980923     c                   ENDSR
043800980923      *****************************************************************
043900980923      * CONTROLLO CODICE FASE RICEVUTA
044000980923      *****************************************************************
044100980923     C     CTLAZNFASE    BEGSR
044200980923      *
044300980923     C                   IF        AZN_fase = *blanks
044400980923     c                   eval      *in28 = *ON
044500980923     c                   ELSE
044600980923      *
044700980923     C                   testn                   AZN_fase             31
044800980923     C   31              move      AZN_fase      W001A
044900980923     C   31              eval      *in31 = (W001A >= '0')
045000980923      *
045100980923     C                   IF        *IN31 = *OFF
045200980923     c                   eval      *in28 = *ON
045300980923     c                   ELSE
045400980923      * Controllo
045500980923     c                   clear                   FIDN05DS
045600980923     c                   movel     'C'           I05MOD
045700980923     C                   move      AZN_fase      I05fca
045800980923     C                   CALL      'FIDN05R'
045900980923     C                   PARM                    KPJBA
046000980923     c                   PARM                    FIDN05DS
046100980923     c                   if        O05rec <> *blanks
046200980923     c                   movel     O05REC        DS_FNDFA
046300980923     C                   endif
046400980923     c                   if        O05ERR = 'E' or DFAatb <> *blanks
046500980923     c                   eval      *in28 = *ON
046600980923     C                   endif
046700980923      *
046800980923     C                   ENDIF
046900980923     C                   ENDIF
047000980923      *
047100980923     c                   ENDSR
047200000228      **************************************************************************
047300000228      * CARICO SCHIERA L6 DA TABELLA £6
047400000228      **************************************************************************
047500000228     C     carl6         BEGSR
047600000228      *
047700000228     C                   CLEAR                   TRUL06DS
047800000228     C                   MOVE      '£6'          D06COD
047900000228     C                   MOVEL(P)  v1cfgs        D06key
048000000228     C                   MOVEL(P)  TRUL06DS      KPJBU
048100000228     C                   CALL      'TRUL06R'
048200000228     C                   PARM                    KPJBA
048300000228     C                   MOVEL     KPJBU         TRUL06DS
048400000228      *
048500000228     c                   endsr
048600980910      *****************************************************************
048700980527      * ROUTINE INIZIALE
048800980527      *****************************************************************
048900980527     C     *INZSR        BEGSR
049000980615      *
049100980521     C     *ENTRY        PLIST
049200980521     C                   PARM                    KPJBA
049300980521     C                   MOVEL     KPJBU         DS_AZIONE
049400980907      *
049500980907     C                   Z-ADD     1             CODUT
049600980907     C                   CALL      'X§PARUT'
049700980907     C                   PARM                    UT§DSE0F
049800980907     C                   MOVEL     RAGUT         RSUT
049900980907     C                   MOVEL     REC80         CNCR80
050000980907
050100981001      * Controllo il tipo azione. Se non è valido vado a fine pgm.
050200981001     C                   IF        AZN_tipo <> 'G'
050300980521     C                   goto      FINE
050400980904     C                   ENDIF
050500980907
050600980907      * Controllo il codice fase. Se non è valido o non è impostato vado a fine pgm.
050700980907     c                   EXSR      CTLaznfase
050800980907     C   28              goto      FINE
050900980907     c                   movel     AZN_fase      Wazn_fase
051000980907
051100981001      * IMPOSTO IL P.O. IN GESTIONE
051200020502     c                   movel     simpou        v1cfgs
051300020502     c     simtpp        ifeq      '2'
051400020502     c     simtpp        oreq      *blanks
051500020502     C                   eval      *in06 = *on
051600020502     c                   endif
051700981001      *
051800981001      * Se sede non visualizzo  "P.O. IN GESTIONE"
051900020502     C                   IF        simfel = *zeros
052000981001     C                   eval      *in06 = *on
052100981001     C                   eval      *in07 = *on
052200981001     c                   ELSE
052300981012     C     V1CFGS        CHAIN     AZORG                              31
052400980521     C  N31              MOVEL     orgDES        V1DFGS
052500000228     C                   CLEAR                   L6
052600000228     C                   EXSR      CARL6
052700000228     C                   MOVEA     LIN           L6
052800981001    2C                   ENDIF
052900981007      *
053000981007      * Imposto il TITOLO
053100021107     c                   IF        Wazn_fase = WFdev
053200981007     c                   movel     TIT_fdev      VTCtit
053300021107     c                   ELSE
053400981007     C                   eval      *IN28 = *ON
053500021107     c                   ENDIF
053600981007     C   28              goto      FINE
053700980923
053800980721      * reperisco data e ora
053900980721     C                   TIME                    W0140
054000980521      * UDATE IN GGMMAAAA
054100980521     C                   MOVE      W0140         WDTGIO
054200980923     C                   MOVEL     W0140         WORA
054300980521      * UDATE IN AAAAMMGG
054400980521     C                   Z-ADD     WDTGIO        G02DAT
054500980521     C                   MOVEL     *BLANK        G02ERR
054600980521     C                   CALL      'XSRDA8'
054700980521     C                   PARM                    WLBDAT
054800980521     C                   MOVEL     G02INV        DATEU
054900980923
055000980923      *    Imposto anno denuncia
055100981027     c                   move      wdtgio        V1Caad
055200981012     c                   move      wdtgio        V1Caad
055300980721
055400980923     C     KDET          klist
055500981027     C                   KFLD                    kaad
055600980908     C                   KFLD                    kpod
055700980908     C                   KFLD                    knde
055800980915
055900980521     C                   ENDSR
056000980521      *---------------------------------------------------------------------------------------------
056100981007     oFNDCT000  E            UPDDCT
056200980923     O                       DCTgfc
056300981027     O                       DCTdit
056400981027     O                       DCTpra
056500981027     O                       DCTprn
056600980923     O                       DCTfca
056700981222     O                       DCTft1
056800981222     O                       DCTft2
056900980923      *---------------------------------------------------------------------------------------------
057000980521** MSG  Lungh. 78                                                            *
057100980923Il numero denuncia immesso non esiste                                          1
057200981117Il valore immesso nel campo non è valido                                       2
057300981117MANCA NUMERATORE PRATICA DENUNCIA: telefonare in Sede !!!!!!!                  3
057400981117La denuncia non è stata inviata in Sede                                        4
