000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950707      * TRAS41R4                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950707      *          SELEZIONE DATI PAGAMENTO                            *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931011      * KC -FINE LAVORO                                              *
001100931011      * KG -INTERROGAZIONE BOLLE                                     *
001200931011      * KL -RITORNO                                                  *
001300931011?     *--------------------------------------------------------------*
001400950706     FTNCSA01L  IF   E           K DISK
001500950707     FTNCSB04L  IF   E           K DISK
001600120210     FTNCSm03L  IF   E           K DISK
001700950706     FTNSC41D   CF   E             WORKSTN
001800950706     F                                     SFILE(SC41S01:NRR)
001900931011?     *--------------------------------------------------------------*
002000931011?     *  DS                                                          *
002100931011?     *--------------------------------------------------------------*
002200000107     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002300000107     D DGED          E DS
002400931011     D KPJBA         E DS
002500931011      *
002600950706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700950706     D CNCR80        E DS
002800950706     D*
002900980126     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003000980126     D CNACO         E DS                  EXTNAME(CNACO00F)
003100980126     D CNIND         E DS                  EXTNAME(CNIND00F)
003200980126     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003300980126     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003400941102     D WLBDA8          DS
003500941102     D  G02DAT                 1      8  0
003600941102     D  G02INV                 9     16  0
003700941102     D  G02ERR                17     17
003800941102     D  G02TGI                18     22  0
003900120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
004000120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
004100990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004200931011      *
004300120210     d mnra            s                   like(csmnra)
004400120210     d mabi            s                   like(csmabi)
004500120210     d mcab            s                   like(csmcab)
004600931011      *
004700931011     D                SDS
004800931011     D  NOMPG                  1     10
004900931011      *---------------------------------------------------------------*
005000931011      *   C I C L O      P R I N C I P A L E                          *
005100931011      *---------------------------------------------------------------*
005200931011     C*  CARICAMENTO SUBFILE
005300931011     C                   EXSR      CARSFL
005400931011     C*
005500931011     C*  LOOP
005600950706     C     WFINE         DOUEQ     'S'
005700931011     C*
005800931011     C     TIPVID        CASEQ     '1'           GESVD1
005900931011     C     TIPVID        CASEQ     '2'           GESVD2
006000931011     C                   END
006100931011     C*
006200931011     C                   END
006300931011     C*
006400950706     C                   MOVEL     TNSC40        KPJBU
006500971222     C* RICHIAMO PGM X CHIUSURA IN LR
006600980126     C                   CLEAR                   TIBS69
006700980126     C                   MOVEL     'C'           I69TLA
006800980126     C                   CALL      'TIBS69R'
006900980126     C                   PARM                    TIBS69
007000980126     C                   PARM                    CNACO
007100980126     C                   PARM                    CNIND
007200980126     C                   PARM                    CNCLP
007300980126     C                   PARM                    FNCLS
007400971222     C*
007500931011     C                   SETON                                        LR
007600931011      *---------------------------------------------------------------*
007700931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
007800931011      *---------------------------------------------------------------*
007900931011     C     CARSFL        BEGSR
008000931011     C*
008100931011     C*  Pulisco subfile
008200950707     C                   SETOFF                                       2021
008300950706     C                   WRITE     SC41C01
008400950707     C                   SETON                                        21
008500950706     C*  Imposto variabili testata
008600950706     C                   CLEAR                   WLBDA8
008700950707     C                   Z-ADD     D40PCD        V1CKCP
008800950707     C                   Z-ADD     D40PDD        G02INV
008900950706     C                   MOVEL     '3'           G02ERR
009000950706     C                   CALL      'XSRDA8'
009100950706     C                   PARM                    WLBDA8
009200950707     C                   Z-ADD     G02DAT        V1CPDD
009300950707     C                   Z-ADD     D40PDA        G02INV
009400950706     C                   MOVEL     '3'           G02ERR
009500950706     C                   CALL      'XSRDA8'
009600950706     C                   PARM                    WLBDA8
009700950707     C                   Z-ADD     G02DAT        V1CPDA
009800950707     C                   Z-ADD     D40PND        V1CNDC
009900950707     C                   MOVEL     D40PTN        V1CINT
010000160415     C                   MOVEL     D40rag        V1Cmit
010100931011     C*
010200931011     C*  Caricamento subfile
010300980126     C                   Z-ADD     0             CSBDDP
010400950707     C     KCSB2         SETLL     TNCSB04L
010500950707     C                   READ      TNCSB04L                               23
010600931011     C*
010700931011     C                   Z-ADD     0             NRR               4 0
010800931013     C                   MOVEL     '1'           TIPVID            1
010900931011     C*
011000950711     C     *IN23         IFEQ      '0'
011100931011     C                   EXSR      ROLLUP
011200931011     C                   END
011300931011     C*
011400931013     C     NRR           IFNE      0
011500931013     C                   MOVEL     '2'           TIPVID            1
011600931013     C                   END
011700931013     C*
011800931012     C                   MOVEL     NOMPG         NOMPGM
011900931011     C*
012000931011     C                   ENDSR
012100931011     C*----------------------------------------------------*
012200931011     C*   ROLLUP: Caricamento di una pagina                *
012300931011     C*----------------------------------------------------*
012400931011     C     ROLLUP        BEGSR
012500931011     C*
012600950706     C     *IN23         IFEQ      '0'
012700931014     C                   Z-ADD     SAVRE1        NRR
012800931011     C                   Z-ADD     0             NRIG              4 0
012900931011     C*
013000950707     C     NRIG          DOWLT     16
013100950706     C     *IN23         ANDEQ     '0'
013200950707     C     CSBDDP        ANDLE     D40PDA
013300950707     C*  Se non ho scelto cliente selez. x data documento
013400950707     C     D40PCD        IFEQ      0
013500950707     C                   EXSR      REDDOC
013600950707     C                   ELSE
013700950707     C*  altrimenti x documento - cliente
013800950707     C                   EXSR      REDCLI
013900931011     C                   END
014000950707     C                   END
014100931011     C*
014200931014     C     NRR           IFGT      SAVRE1
014300931014     C     SAVRE1        ADD       1             DSPNR1
014400931014     C                   Z-ADD     NRR           SAVRE1            4 0
014500931014     C                   END
014600931014     C*
014700931011     C                   END
014800950707     C*
014900950707     C  N23CSBDDP        COMP      D40PDA                             23
015000931011     C*
015100931011     C                   ENDSR
015200950707     C*----------------------------------------------------*
015300950707     C*   REDDOC: Lettura con selez. per DOCUMENTO         *
015400950707     C*----------------------------------------------------*
015500950707     C     REDDOC        BEGSR
015600950707     C*
015700950707     C     CSBNDP        IFLT      D40PND
015800950707     C     KCSB2         SETLL     TNCSB04L
015900950707     C                   READ      TNCSB04L                               23
016000950707     C                   Z-ADD     CSBDDP        KDDP
016100950707     C                   END
016200950707     C*  Leggo lnp.
016300950707     C     *IN23         DOWEQ     '0'
016400950707     C     CSBNDP        ANDEQ     D40PND
016500950707     C     CSBDDP        ANDLE     D40PDA
016600950707     C     NRIG          ANDLT     16
016700950707     C                   EXSR      GESREC
016800950707     C                   END
016900950707     C*
017000950711     C                   Z-ADD     CSBDDP        KDDP
017100950707     C     CSBNDP        IFGT      D40PND
017200950707     C     KCSB1         SETGT     TNCSB04L
017300950707     C                   READ      TNCSB04L                               23
017400950707     C                   Z-ADD     CSBDDP        KDDP
017500950707     C                   END
017600950707     C*
017700950707     C                   ENDSR
017800950706     C*----------------------------------------------------*
017900950707     C*   REDCLI: Lettura con selez. per CLIENTE           *
018000950706     C*----------------------------------------------------*
018100950707     C     REDCLI        BEGSR
018200950706     C*
018300950707     C     CSBNDP        IFLT      D40PND
018400950707     C     CSBNDP        OREQ      D40PND
018500950707     C     CSBCDI        ANDLT     D40PCD
018600950707     C     KCSB2         SETLL     TNCSB04L
018700950707     C                   READ      TNCSB04L                               23
018800950707     C                   Z-ADD     CSBDDP        KDDP
018900950706     C                   END
019000950706     C*  Leggo lnp.
019100950706     C     *IN23         DOWEQ     '0'
019200950707     C     CSBCDI        ANDEQ     D40PCD
019300950707     C     CSBNDP        ANDEQ     D40PND
019400950707     C     CSBDDP        ANDLE     D40PDA
019500950707     C     NRIG          ANDLT     16
019600950706     C                   EXSR      GESREC
019700950706     C                   END
019800950706     C*
019900950711     C                   Z-ADD     CSBDDP        KDDP
020000950707     C     CSBNDP        IFGT      D40PND
020100950707     C     CSBNDP        OREQ      D40PND
020200950707     C     CSBCDI        ANDGT     D40PCD
020300950707     C     KCSB1         SETGT     TNCSB04L
020400950707     C                   READ      TNCSB04L                               23
020500950707     C                   Z-ADD     CSBDDP        KDDP
020600950707     C                   END
020700950706     C*
020800950706     C                   ENDSR
020900120210     C*----------------------------------------------------*
021000120210     C*   srmulti - per multiassegni
021100120210     C*----------------------------------------------------*
021200120210     C     srmulti       BEGSR
021300120210     C*
021400120210     c                   eval      mnra = *hival
021500120210     c                   eval      mabi = *hival
021600120210     c                   eval      mcab = *hival
021700120210     c     csbnra        setll     tncsm03l
021800120210     c                   do        *hival
021900120210     c     csbnra        reade     tncsm03l
022000120210     c                   if        %eof(tncsm03l)
022100120210     c                   leave
022200120210     c                   end
022300120210     C                   if        csmnra = WNRA
022400120210     c                   eval      mnra = csmnra
022500120210     c                   eval      mabi = csmabi
022600120210     c                   eval      mcab = csmcab
022700120210     c                   leave
022800120210     c                   end
022900120210     c                   enddo
023000120210     C*
023100120210     C                   ENDSR
023200950706     C*----------------------------------------------------*
023300950706     C*   GESREC: Caricamento di una pagina                *
023400950706     C*----------------------------------------------------*
023500950706     C     GESREC        BEGSR
023600950706     C*----------------------------------------------------*
023700950706     C* Se il documento è valido controllo se il record rientra
023800950706     C* nei limiti richiesti
023900950706     C                   MOVEL     D40NAS        WNRA             10
024000120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
024100120220     c                             csbtpi = 'M' and csbabi = 0
024200120210     c                   exsr      srmulti
024300120210     c                   else
024400120210     c                   eval      mnra = csbnra
024500120210     c                   eval      mabi = csbabi
024600120210     c                   eval      mcab = csbcab
024700120210     c                   end
024800950706     C*----------------------------------------------------*
024900950706     C     D40ABI        IFEQ      *ZEROS
025000120210     C     D40ABI        OREQ      mABI
025100950720     C     D40CAB        IFEQ      *ZEROS
025200120210     C     D40CAB        OREQ      mCAB
025300140325     C     D40imp        IFEQ      *ZEROS
025400140325     C     D40imp        OREQ      csbcas
025500950720     C     WNRA          IFEQ      *BLANKS
025600950720     C     WNRA          OREQ      *ZEROS
025700120210     C     WNRA          OREQ      mNRA
025800950707     C*  Test  x tipo intestazione
025900950707     C     D40PTN        IFEQ      *BLANKS
026000950707     C     D40PTN        OREQ      'M'
026100950707     C     CSBTPI        ANDEQ     'M'
026200950707     C     D40PTN        OREQ      'B'
026300950707     C     CSBTPI        ANDEQ     ' '
026400000107     C* Scrivo record se divisa corrisponde
026500000107     C     D40PDV        IFEQ      *BLANKS
026600000107     C     D40PDV        OREQ      CSBVCA
026700011001     C     D40PDV        OREQ      §GEDCN
026800000107     C     CSBVCA        ANDEQ     *BLANKS
026900971222     C*----------------------------------------------------*
027000960909     C* Selezione per stato C/Assegno
027100960909     C     D40TCR        IFEQ      'T'
027200960909     C     CSBSTA        ANDEQ     6
027300960909     C     D40TCR        OREQ      'R'
027400960909     C     CSBSTA        ANDEQ     7
027500960911     C     D40TCR        OREQ      'P'
027600960911     C     CSBSTA        ANDEQ     0
027700950706     C                   MOVEL     *BLANKS       V1CSCE
027800950706     C                   MOVEL     CSBAAS        V1CAAS
027900950706     C                   MOVEL     CSBMGS        MM                2 0
028000950706     C                   MOVE      CSBMGS        GG                2 0
028100950706     C                   MOVEL     GG            V1CDSP
028200950706     C                   MOVE      MM            V1CDSP
028300950706     C                   MOVE      CSBLNP        V1CLNP
028400950706     C                   MOVE      CSBNRS        V1CNRS
028500950706     C                   MOVE      CSBNSP        V1CNSP
028600950712     C                   MOVE      CSBTBL        V1CTBL
028700950706     C                   MOVEL     CSBRSD        V1CDES
028800950706     C                   MOVE      CSBRMN        V1CRMN
028900950706     C                   MOVE      CSBVCA        V1CVCA
029000950706     C                   Z-ADD     CSBCAS        V1CIMP
029100950714     C                   MOVE      CSBCDI        W8899             4 0
029200950706     C     W8899         IFEQ      8888
029300950706     C     W8899         OREQ      9999
029400950707     C                   Z-ADD     CSBAAS        KAAS
029500950707     C                   Z-ADD     CSBLNP        KLNP
029600950707     C                   Z-ADD     CSBNRS        KNRS
029700950707     C                   Z-ADD     CSBNSP        KNSP
029800950706     C                   MOVEL     *BLANKS       CSARSD
029900950706     C     KCSA          CHAIN     TNCSA01L                           31
030000950706     C                   MOVEL     CSARSD        V1CMIT
030100950706     C                   ELSE
030200950706     C                   MOVEL     *BLANKS       ACORAG
030300980126      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030400980126     C                   CLEAR                   TIBS69
030500980126     C                   CLEAR                   CNACO
030600980126     C                   CLEAR                   CNIND
030700980126     C                   CLEAR                   CNCLP
030800980126     C                   CLEAR                   FNCLS
030900980126     C                   MOVEL     ' '           I69TLA
031000980126     C                   MOVEL     KNSIF         I69SIF
031100980126     C                   Z-ADD     CSBCDI        I69KAC
031200980126     C                   CALL      'TIBS69R'
031300980126     C                   PARM                    TIBS69
031400980126     C                   PARM                    CNACO
031500980126     C                   PARM                    CNIND
031600980126     C                   PARM                    CNCLP
031700980126     C                   PARM                    FNCLS
031800980126     C     O69ERR        IFEQ      *BLANK
031900950706     C                   MOVEL     ACORAG        V1CMIT
032000980126     C                   END
032100950706     C                   END
032200160505     c                   if        d40rag = *blank or
032300160505     c                             d40rag <> *blank and
032400160415     c                             %scan(%trim(d40rag):acorag) > 0
032500950706     C                   ADD       1             NRR
032600950706     C                   ADD       1             NRIG
032700950706     C                   WRITE     SC41S01
032800160415     c                   endif
032900950706     C                   END
033000960909     C*
033100000107     C                   END
033200000107     C                   END
033300950707     C                   END
033400950720     C                   END
033500950720     C                   END
033600140325     C                   END
033700950706     C*
033800950707     C                   READ      TNCSB04L                               23
033900950706     C*
034000950706     C                   ENDSR
034100931011     C*----------------------------------------------------*
034200931011     C*  Routine gestione operazioni prima videata         *
034300931011     C*----------------------------------------------------*
034400931011     C     GESVD1        BEGSR
034500931011     C*-                                                   *
034600950707     C                   SETOFF                                       20
034700950706     C                   WRITE     SC41Z02
034800950706     C                   WRITE     SC41D02
034900950706     C                   EXFMT     SC41C01
035000931011     C*                                                    *
035100931011     C* Fine Programma                                     *
035200931011     C     *INKL         IFEQ      '1'
035300950706     C                   MOVEL     'S'           WFINE
035400931011     C                   END
035500931011     C*-                                                   *
035600931011     C                   ENDSR
035700931011     C*----------------------------------------------------*
035800931011     C*  Routine gestione operazioni subfile               *
035900931011     C*----------------------------------------------------*
036000931011     C     GESVD2        BEGSR
036100931011     C*-                                                   *
036200950707     C                   SETON                                        20
036300950706     C                   WRITE     SC41Z01
036400950706     C                   WRITE     SC41Z01
036500950706     C                   EXFMT     SC41C01
036600950706     C                   MOVEL     'N'           WINTBL            1
036700931011     C*                                                    *
036800931011     C* Fine Programma                                     *
036900931011     C     *INKC         IFEQ      '1'
037000950706     C                   MOVEL     'S'           WFINE
037100950706     C                   MOVE      3             D40CMD
037200931011     C                   GOTO      FINVD2
037300931011     C                   END
037400931011     C* Ritorno                                            *
037500931011     C     *INKL         IFEQ      '1'
037600950706     C                   MOVEL     'S'           WFINE
037700950706     C                   MOVE      12            D40CMD
037800931011     C                   GOTO      FINVD2
037900931011     C                   END
038000931011     C* ROLLUP
038100950711     C     *IN25         IFEQ      '1'
038200950711     C                   EXSR      ROLLUP
038300950711     C                   GOTO      FINVD2
038400950711     C                   END
038500950706     C*                                                    *
038600990729     C                   CLEAR                   TNSB50
038700950706     C                   EXSR      CNTVD2
038800931011     C* Interrogazione Bolle                               *
038900931011     C     *INKG         IFEQ      '1'
039000950706     C     WINTBL        ANDEQ     'N'
039100950706     C                   EXSR      GESINT
039200931011     C                   END
039300931011     C*                                                    *
039400931012     C     FINVD2        ENDSR
039500990729     C*----------------------------------------------------*
039600990729     C*   GESINT: gestione interrogazione bolle            *
039700990729     C*----------------------------------------------------*
039800990729     C     GESINT        BEGSR
039900990729     C*                                                    *
040000990729     C                   CLEAR                   TNSB50
040100990729     C                   MOVEL     'C05'         I50OP0
040200990729     C                   MOVEL     '0'           O50F03
040300990729     C                   MOVEL     '0'           O50F12
040400990729     C                   MOVEL     '0'           O50ERR
040500990729     C                   MOVEL     *BLANKS       O50MSG
040600990729     C                   MOVEL     KPJBU         WSVKPJ          256
040700990729     C                   MOVEL     *BLANKS       KPJBU
040800990729     C     WACC          IFEQ      '1'
040900990729     C                   Z-ADD     V1CAAS        D50AAS
041000990729     C                   Z-ADD     V1CLNP        D50LNP
041100990729     C                   Z-ADD     V1CNRS        D50NRS
041200990729     C                   Z-ADD     V1CNSP        D50NSP
041300990729     C                   MOVEL     V1CTBL        D50TBL
041400990729     C                   MOVEL     'S'           WINTBL
041500990729     C                   MOVEL     TNSB50        KPJBU
041600990729     C                   CALL      'TNSB50R'
041700990729     C                   PARM                    KPJBA
041800990910     C                   MOVEL     KPJBU         TNSB50
041900990729     C     O50ERR        IFEQ      *BLANKS
042000990729     C                   GOTO      FININT
042100990729     C                   END
042200990729     C* Se ho richiamato il pgm con il numero spedizione/
042300990729     C* codice bolla prova a richiamare senza codice bolla
042400990729     C* come del resto faccio se non ho recuperato il C/Ass.
042500990729     C                   MOVEL     'C00'         I50OP0
042600990729     C                   MOVEL     V1CTBL        D50TBL
042700990729     C                   MOVEL     TNSB50        KPJBU
042800990729     C                   CALL      'TNSB50R'
042900990729     C                   PARM                    KPJBA
043000990729     C                   ELSE
043100990729     C                   MOVEL     TNSB50        KPJBU
043200990729     C                   CALL      'TNSB50R'
043300990729     C                   PARM                    KPJBA
043400990729     C                   END
043500990729     C                   MOVEL     KPJBU         TNSB50
043600990729     C                   MOVEL     WSVKPJ        KPJBU
043700990729     C*
043800990729     C     FININT        ENDSR
043900931011     C*----------------------------------------------------*
044000931011     C*   CNTVD2: Controlli subfile                        *
044100931011     C*----------------------------------------------------*
044200931011     C     CNTVD2        BEGSR
044300931011     C*                                                    *
044400931011     C                   Z-ADD     1             NRR
044500950706     C                   MOVEL     ' '           V1CSCE
044600950706     C                   READC     SC41S01                                82
044700931011     C*                                                    *
044800931011     C     *IN82         DOWEQ     '0'
044900931011     C*                                                    *
045000950706     C     V1CSCE        IFEQ      '1'
045100931011     C                   MOVEL     *BLANKS       KPJBU
045200931012     C                   Z-ADD     NRR           SAVREC            4 0
045300950706     C*  Interrogazione bolla
045400950706     C     *INKG         IFEQ      '1'
045500990729     C                   MOVEL     '1'           WACC              1
045600950706     C                   EXSR      GESINT
045700950706     C                   ELSE
045800120612     c                   eval      tnsc40ds = tnsc40
045900120612     C                   MOVE      V1CAAS        £D40AAS
046000120612     C                   MOVE      V1CLNP        £D40LNP
046100120612     C                   MOVE      V1CNRS        £D40NRS
046200120612     C                   MOVE      V1CNSP        £D40NSP
046300120612     C                   clear                   £D40ABI
046400120612     C                   clear                   £D40CAB
046500120612     C                   clear                   £D40NAS
046600120612     C                   MOVEL     TNSC40ds      KPJBU
046700931012     C* Interrogazione                                     *
046800950706     C                   CALL      'TNSC43R'
046900931012     C                   PARM                    KPJBA
047000950706     C                   END
047100931011     C*
047200931012     C                   Z-ADD     SAVREC        NRR
047300931014     C                   Z-ADD     SAVREC        DSPNR1
047400950706     C     NRR           CHAIN     SC41S01                            90
047500931011     C                   SETON                                        70
047600950706     C                   MOVEL     ' '           V1CSCE
047700950706     C  N90              UPDATE    SC41S01
047800931011     C                   END
047900931011     C*
048000950706     C                   READC     SC41S01                                82
048100931011     C                   END
048200931012     C*
048300931011     C                   ENDSR
048400931011      *---------------------------------------------------------------*
048500941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
048600931011      *---------------------------------------------------------------*
048700941102     C     *INZSR        BEGSR
048800931011      *---------------------------------------------------------------*
048900931011     C     *ENTRY        PLIST
049000931011     C                   PARM                    KPJBA
049100950706     C                   MOVEL     KPJBU         TNSC40
049200931012     C*
049300931011     C                   Z-ADD     1             CODUT
049400931011     C                   CALL      'X§PARUT'
049500931011     C                   PARM                    UT§DSE
049600931011     C*          CODUT     CABEQ-1        FINE
049700931011     C                   MOVEL     RAGUT         RSUT             20
049800931011     C                   MOVEL     REC80         CNCR80
049900931011     C                   MOVEL     KNMUS         §SEI              6
050000950706     C*----------------------------------------------------*
050100950706     C*  Definizione CHIAVI
050200950706     C     KCSB1         KLIST
050300950707     C                   KFLD                    KDDP
050400950706     C     KCSB2         KLIST
050500950707     C                   KFLD                    KDDP
050600950707     C                   KFLD                    KNDP
050700950707     C                   KFLD                    KCDI
050800971222     C     KCSB3         KLIST
050900971222     C                   KFLD                    KAAS
051000971222     C                   KFLD                    KLNP
051100971222     C                   KFLD                    KNRS
051200971222     C                   KFLD                    KNSP
051300950706     C     KCSA          KLIST
051400950706     C                   KFLD                    KAAS
051500950706     C                   KFLD                    KLNP
051600950706     C                   KFLD                    KNRS
051700950706     C                   KFLD                    KNSP
051800931012     C*----------------------------------------------------*
051900941102     C*  Definizione variabili
052000950706     C     *LIKE         DEFINE    CSBAAS        KAAS
052100950707     C     *LIKE         DEFINE    CSBCDI        KCDI
052200950707     C     *LIKE         DEFINE    CSBDDP        KDDP
052300950707     C     *LIKE         DEFINE    CSBNDP        KNDP
052400950706     C     *LIKE         DEFINE    CSALNP        KLNP
052500950706     C     *LIKE         DEFINE    CSANRS        KNRS
052600950706     C     *LIKE         DEFINE    CSANSP        KNSP
052700000107     C*----------------------------------------------------*
052800000107     C*  RECUPERO DIVISA MONETA CORRENTE
052900000107     C                   CLEAR                   DGED
053000000107     C                   CLEAR                   DSBS02
053100000107     C                   MOVEL     'C'           T02MOD
053200000107     C                   MOVEL     KNSIF         T02SIF
053300000107     C                   MOVEL     'GED'         T02COD
053400000107     C                   MOVEL     '1'           T02KE1
053500000107     C                   CALL      'TIBS02R'
053600000107     C                   PARM                    KPJBA
053700000107     C                   PARM                    DSBS02
053800000107     C*
053900000107    3C     T02ERR        IFEQ      ' '
054000000107     C                   MOVEL     T02UNI        DGED
054100000107    3C                   ENDIF
054200941102     C*----------------------------------------------------*
054300950706     C*  Inizializzazione delle variabii
054400950706     C                   MOVEL     'N'           WFINE             1
054500950707     C                   Z-ADD     D40PCD        KCDI
054600950707     C                   Z-ADD     D40PDD        KDDP
054700950707     C                   MOVEL     D40PND        KNDP
054800950707     C                   SETON                                        04
054900931012     C*----------------------------------------------------*
055000931011     C                   ENDSR
