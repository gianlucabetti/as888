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
003901120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
004000990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004100931011      *
004200120210     d mnra            s                   like(csmnra)
004300120210     d mabi            s                   like(csmabi)
004400120210     d mcab            s                   like(csmcab)
004500931011      *
004600931011     D                SDS
004700931011     D  NOMPG                  1     10
004800931011      *---------------------------------------------------------------*
004900931011      *   C I C L O      P R I N C I P A L E                          *
005000931011      *---------------------------------------------------------------*
005100931011     C*  CARICAMENTO SUBFILE
005200931011     C                   EXSR      CARSFL
005300931011     C*
005400931011     C*  LOOP
005500950706     C     WFINE         DOUEQ     'S'
005600931011     C*
005700931011     C     TIPVID        CASEQ     '1'           GESVD1
005800931011     C     TIPVID        CASEQ     '2'           GESVD2
005900931011     C                   END
006000931011     C*
006100931011     C                   END
006200931011     C*
006300950706     C                   MOVEL     TNSC40        KPJBU
006400971222     C* RICHIAMO PGM X CHIUSURA IN LR
006500980126     C                   CLEAR                   TIBS69
006600980126     C                   MOVEL     'C'           I69TLA
006700980126     C                   CALL      'TIBS69R'
006800980126     C                   PARM                    TIBS69
006900980126     C                   PARM                    CNACO
007000980126     C                   PARM                    CNIND
007100980126     C                   PARM                    CNCLP
007200980126     C                   PARM                    FNCLS
007300971222     C*
007400931011     C                   SETON                                        LR
007500931011      *---------------------------------------------------------------*
007600931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
007700931011      *---------------------------------------------------------------*
007800931011     C     CARSFL        BEGSR
007900931011     C*
008000931011     C*  Pulisco subfile
008100950707     C                   SETOFF                                       2021
008200950706     C                   WRITE     SC41C01
008300950707     C                   SETON                                        21
008400950706     C*  Imposto variabili testata
008500950706     C                   CLEAR                   WLBDA8
008600950707     C                   Z-ADD     D40PCD        V1CKCP
008700950707     C                   Z-ADD     D40PDD        G02INV
008800950706     C                   MOVEL     '3'           G02ERR
008900950706     C                   CALL      'XSRDA8'
009000950706     C                   PARM                    WLBDA8
009100950707     C                   Z-ADD     G02DAT        V1CPDD
009200950707     C                   Z-ADD     D40PDA        G02INV
009300950706     C                   MOVEL     '3'           G02ERR
009400950706     C                   CALL      'XSRDA8'
009500950706     C                   PARM                    WLBDA8
009600950707     C                   Z-ADD     G02DAT        V1CPDA
009700950707     C                   Z-ADD     D40PND        V1CNDC
009800950707     C                   MOVEL     D40PTN        V1CINT
009900931011     C*
010000931011     C*  Caricamento subfile
010100980126     C                   Z-ADD     0             CSBDDP
010200950707     C     KCSB2         SETLL     TNCSB04L
010300950707     C                   READ      TNCSB04L                               23
010400931011     C*
010500931011     C                   Z-ADD     0             NRR               4 0
010600931013     C                   MOVEL     '1'           TIPVID            1
010700931011     C*
010800950711     C     *IN23         IFEQ      '0'
010900931011     C                   EXSR      ROLLUP
011000931011     C                   END
011100931011     C*
011200931013     C     NRR           IFNE      0
011300931013     C                   MOVEL     '2'           TIPVID            1
011400931013     C                   END
011500931013     C*
011600931012     C                   MOVEL     NOMPG         NOMPGM
011700931011     C*
011800931011     C                   ENDSR
011900931011     C*----------------------------------------------------*
012000931011     C*   ROLLUP: Caricamento di una pagina                *
012100931011     C*----------------------------------------------------*
012200931011     C     ROLLUP        BEGSR
012300931011     C*
012400950706     C     *IN23         IFEQ      '0'
012500931014     C                   Z-ADD     SAVRE1        NRR
012600931011     C                   Z-ADD     0             NRIG              4 0
012700931011     C*
012800950707     C     NRIG          DOWLT     16
012900950706     C     *IN23         ANDEQ     '0'
013000950707     C     CSBDDP        ANDLE     D40PDA
013100950707     C*  Se non ho scelto cliente selez. x data documento
013200950707     C     D40PCD        IFEQ      0
013300950707     C                   EXSR      REDDOC
013400950707     C                   ELSE
013500950707     C*  altrimenti x documento - cliente
013600950707     C                   EXSR      REDCLI
013700931011     C                   END
013800950707     C                   END
013900931011     C*
014000931014     C     NRR           IFGT      SAVRE1
014100931014     C     SAVRE1        ADD       1             DSPNR1
014200931014     C                   Z-ADD     NRR           SAVRE1            4 0
014300931014     C                   END
014400931014     C*
014500931011     C                   END
014600950707     C*
014700950707     C  N23CSBDDP        COMP      D40PDA                             23
014800931011     C*
014900931011     C                   ENDSR
015000950707     C*----------------------------------------------------*
015100950707     C*   REDDOC: Lettura con selez. per DOCUMENTO         *
015200950707     C*----------------------------------------------------*
015300950707     C     REDDOC        BEGSR
015400950707     C*
015500950707     C     CSBNDP        IFLT      D40PND
015600950707     C     KCSB2         SETLL     TNCSB04L
015700950707     C                   READ      TNCSB04L                               23
015800950707     C                   Z-ADD     CSBDDP        KDDP
015900950707     C                   END
016000950707     C*  Leggo lnp.
016100950707     C     *IN23         DOWEQ     '0'
016200950707     C     CSBNDP        ANDEQ     D40PND
016300950707     C     CSBDDP        ANDLE     D40PDA
016400950707     C     NRIG          ANDLT     16
016500950707     C                   EXSR      GESREC
016600950707     C                   END
016700950707     C*
016800950711     C                   Z-ADD     CSBDDP        KDDP
016900950707     C     CSBNDP        IFGT      D40PND
017000950707     C     KCSB1         SETGT     TNCSB04L
017100950707     C                   READ      TNCSB04L                               23
017200950707     C                   Z-ADD     CSBDDP        KDDP
017300950707     C                   END
017400950707     C*
017500950707     C                   ENDSR
017600950706     C*----------------------------------------------------*
017700950707     C*   REDCLI: Lettura con selez. per CLIENTE           *
017800950706     C*----------------------------------------------------*
017900950707     C     REDCLI        BEGSR
018000950706     C*
018100950707     C     CSBNDP        IFLT      D40PND
018200950707     C     CSBNDP        OREQ      D40PND
018300950707     C     CSBCDI        ANDLT     D40PCD
018400950707     C     KCSB2         SETLL     TNCSB04L
018500950707     C                   READ      TNCSB04L                               23
018600950707     C                   Z-ADD     CSBDDP        KDDP
018700950706     C                   END
018800950706     C*  Leggo lnp.
018900950706     C     *IN23         DOWEQ     '0'
019000950707     C     CSBCDI        ANDEQ     D40PCD
019100950707     C     CSBNDP        ANDEQ     D40PND
019200950707     C     CSBDDP        ANDLE     D40PDA
019300950707     C     NRIG          ANDLT     16
019400950706     C                   EXSR      GESREC
019500950706     C                   END
019600950706     C*
019700950711     C                   Z-ADD     CSBDDP        KDDP
019800950707     C     CSBNDP        IFGT      D40PND
019900950707     C     CSBNDP        OREQ      D40PND
020000950707     C     CSBCDI        ANDGT     D40PCD
020100950707     C     KCSB1         SETGT     TNCSB04L
020200950707     C                   READ      TNCSB04L                               23
020300950707     C                   Z-ADD     CSBDDP        KDDP
020400950707     C                   END
020500950706     C*
020600950706     C                   ENDSR
020700120210     C*----------------------------------------------------*
020800120210     C*   srmulti - per multiassegni
020900120210     C*----------------------------------------------------*
021000120210     C     srmulti       BEGSR
021100120210     C*
021200120210     c                   eval      mnra = *hival
021300120210     c                   eval      mabi = *hival
021400120210     c                   eval      mcab = *hival
021500120210     c     csbnra        setll     tncsm03l
021600120210     c                   do        *hival
021700120210     c     csbnra        reade     tncsm03l
021800120210     c                   if        %eof(tncsm03l)
021900120210     c                   leave
022000120210     c                   end
022100120210     C                   if        csmnra = WNRA
022200120210     c                   eval      mnra = csmnra
022300120210     c                   eval      mabi = csmabi
022400120210     c                   eval      mcab = csmcab
022500120210     c                   leave
022600120210     c                   end
022700120210     c                   enddo
022800120210     C*
022900120210     C                   ENDSR
023000950706     C*----------------------------------------------------*
023100950706     C*   GESREC: Caricamento di una pagina                *
023200950706     C*----------------------------------------------------*
023300950706     C     GESREC        BEGSR
023400950706     C*----------------------------------------------------*
023500950706     C* Se il documento è valido controllo se il record rientra
023600950706     C* nei limiti richiesti
023700950706     C                   MOVEL     D40NAS        WNRA             10
023800120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
023900120220     c                             csbtpi = 'M' and csbabi = 0
024000120210     c                   exsr      srmulti
024100120210     c                   else
024200120210     c                   eval      mnra = csbnra
024300120210     c                   eval      mabi = csbabi
024400120210     c                   eval      mcab = csbcab
024500120210     c                   end
024600950706     C*----------------------------------------------------*
024700950706     C     D40ABI        IFEQ      *ZEROS
024800120210     C     D40ABI        OREQ      mABI
024900950720     C     D40CAB        IFEQ      *ZEROS
025000120210     C     D40CAB        OREQ      mCAB
025001140325     C     D40imp        IFEQ      *ZEROS
025002140325     C     D40imp        OREQ      csbcas
025100950720     C     WNRA          IFEQ      *BLANKS
025200950720     C     WNRA          OREQ      *ZEROS
025300120210     C     WNRA          OREQ      mNRA
025400950707     C*  Test  x tipo intestazione
025500950707     C     D40PTN        IFEQ      *BLANKS
025600950707     C     D40PTN        OREQ      'M'
025700950707     C     CSBTPI        ANDEQ     'M'
025800950707     C     D40PTN        OREQ      'B'
025900950707     C     CSBTPI        ANDEQ     ' '
026000000107     C* Scrivo record se divisa corrisponde
026100000107     C     D40PDV        IFEQ      *BLANKS
026200000107     C     D40PDV        OREQ      CSBVCA
026300011001     C     D40PDV        OREQ      §GEDCN
026400000107     C     CSBVCA        ANDEQ     *BLANKS
026500971222     C*----------------------------------------------------*
026600960909     C* Selezione per stato C/Assegno
026700960909     C     D40TCR        IFEQ      'T'
026800960909     C     CSBSTA        ANDEQ     6
026900960909     C     D40TCR        OREQ      'R'
027000960909     C     CSBSTA        ANDEQ     7
027100960911     C     D40TCR        OREQ      'P'
027200960911     C     CSBSTA        ANDEQ     0
027300950706     C                   MOVEL     *BLANKS       V1CSCE
027400950706     C                   MOVEL     CSBAAS        V1CAAS
027500950706     C                   MOVEL     CSBMGS        MM                2 0
027600950706     C                   MOVE      CSBMGS        GG                2 0
027700950706     C                   MOVEL     GG            V1CDSP
027800950706     C                   MOVE      MM            V1CDSP
027900950706     C                   MOVE      CSBLNP        V1CLNP
028000950706     C                   MOVE      CSBNRS        V1CNRS
028100950706     C                   MOVE      CSBNSP        V1CNSP
028200950712     C                   MOVE      CSBTBL        V1CTBL
028300950706     C                   MOVEL     CSBRSD        V1CDES
028400950706     C                   MOVE      CSBRMN        V1CRMN
028500950706     C                   MOVE      CSBVCA        V1CVCA
028600950706     C                   Z-ADD     CSBCAS        V1CIMP
028700950714     C                   MOVE      CSBCDI        W8899             4 0
028800950706     C     W8899         IFEQ      8888
028900950706     C     W8899         OREQ      9999
029000950707     C                   Z-ADD     CSBAAS        KAAS
029100950707     C                   Z-ADD     CSBLNP        KLNP
029200950707     C                   Z-ADD     CSBNRS        KNRS
029300950707     C                   Z-ADD     CSBNSP        KNSP
029400950706     C                   MOVEL     *BLANKS       CSARSD
029500950706     C     KCSA          CHAIN     TNCSA01L                           31
029600950706     C                   MOVEL     CSARSD        V1CMIT
029700950706     C                   ELSE
029800950706     C                   MOVEL     *BLANKS       ACORAG
029900980126      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030000980126     C                   CLEAR                   TIBS69
030100980126     C                   CLEAR                   CNACO
030200980126     C                   CLEAR                   CNIND
030300980126     C                   CLEAR                   CNCLP
030400980126     C                   CLEAR                   FNCLS
030500980126     C                   MOVEL     ' '           I69TLA
030600980126     C                   MOVEL     KNSIF         I69SIF
030700980126     C                   Z-ADD     CSBCDI        I69KAC
030800980126     C                   CALL      'TIBS69R'
030900980126     C                   PARM                    TIBS69
031000980126     C                   PARM                    CNACO
031100980126     C                   PARM                    CNIND
031200980126     C                   PARM                    CNCLP
031300980126     C                   PARM                    FNCLS
031400980126     C     O69ERR        IFEQ      *BLANK
031500950706     C                   MOVEL     ACORAG        V1CMIT
031600980126     C                   END
031700950706     C                   END
031800950706     C                   ADD       1             NRR
031900950706     C                   ADD       1             NRIG
032000950706     C                   WRITE     SC41S01
032100950706     C                   END
032200960909     C*
032300000107     C                   END
032400000107     C                   END
032500950707     C                   END
032600950720     C                   END
032700950720     C                   END
032701140325     C                   END
032800950706     C*
032900950707     C                   READ      TNCSB04L                               23
033000950706     C*
033100950706     C                   ENDSR
033200931011     C*----------------------------------------------------*
033300931011     C*  Routine gestione operazioni prima videata         *
033400931011     C*----------------------------------------------------*
033500931011     C     GESVD1        BEGSR
033600931011     C*-                                                   *
033700950707     C                   SETOFF                                       20
033800950706     C                   WRITE     SC41Z02
033900950706     C                   WRITE     SC41D02
034000950706     C                   EXFMT     SC41C01
034100931011     C*                                                    *
034200931011     C* Fine Programma                                     *
034300931011     C     *INKL         IFEQ      '1'
034400950706     C                   MOVEL     'S'           WFINE
034500931011     C                   END
034600931011     C*-                                                   *
034700931011     C                   ENDSR
034800931011     C*----------------------------------------------------*
034900931011     C*  Routine gestione operazioni subfile               *
035000931011     C*----------------------------------------------------*
035100931011     C     GESVD2        BEGSR
035200931011     C*-                                                   *
035300950707     C                   SETON                                        20
035400950706     C                   WRITE     SC41Z01
035500950706     C                   WRITE     SC41Z01
035600950706     C                   EXFMT     SC41C01
035700950706     C                   MOVEL     'N'           WINTBL            1
035800931011     C*                                                    *
035900931011     C* Fine Programma                                     *
036000931011     C     *INKC         IFEQ      '1'
036100950706     C                   MOVEL     'S'           WFINE
036200950706     C                   MOVE      3             D40CMD
036300931011     C                   GOTO      FINVD2
036400931011     C                   END
036500931011     C* Ritorno                                            *
036600931011     C     *INKL         IFEQ      '1'
036700950706     C                   MOVEL     'S'           WFINE
036800950706     C                   MOVE      12            D40CMD
036900931011     C                   GOTO      FINVD2
037000931011     C                   END
037100931011     C* ROLLUP
037200950711     C     *IN25         IFEQ      '1'
037300950711     C                   EXSR      ROLLUP
037400950711     C                   GOTO      FINVD2
037500950711     C                   END
037600950706     C*                                                    *
037700990729     C                   CLEAR                   TNSB50
037800950706     C                   EXSR      CNTVD2
037900931011     C* Interrogazione Bolle                               *
038000931011     C     *INKG         IFEQ      '1'
038100950706     C     WINTBL        ANDEQ     'N'
038200950706     C                   EXSR      GESINT
038300931011     C                   END
038400931011     C*                                                    *
038500931012     C     FINVD2        ENDSR
038600990729     C*----------------------------------------------------*
038700990729     C*   GESINT: gestione interrogazione bolle            *
038800990729     C*----------------------------------------------------*
038900990729     C     GESINT        BEGSR
039000990729     C*                                                    *
039100990729     C                   CLEAR                   TNSB50
039200990729     C                   MOVEL     'C05'         I50OP0
039300990729     C                   MOVEL     '0'           O50F03
039400990729     C                   MOVEL     '0'           O50F12
039500990729     C                   MOVEL     '0'           O50ERR
039600990729     C                   MOVEL     *BLANKS       O50MSG
039700990729     C                   MOVEL     KPJBU         WSVKPJ          256
039800990729     C                   MOVEL     *BLANKS       KPJBU
039900990729     C     WACC          IFEQ      '1'
040000990729     C                   Z-ADD     V1CAAS        D50AAS
040100990729     C                   Z-ADD     V1CLNP        D50LNP
040200990729     C                   Z-ADD     V1CNRS        D50NRS
040300990729     C                   Z-ADD     V1CNSP        D50NSP
040400990729     C                   MOVEL     V1CTBL        D50TBL
040500990729     C                   MOVEL     'S'           WINTBL
040600990729     C                   MOVEL     TNSB50        KPJBU
040700990729     C                   CALL      'TNSB50R'
040800990729     C                   PARM                    KPJBA
040900990910     C                   MOVEL     KPJBU         TNSB50
041000990729     C     O50ERR        IFEQ      *BLANKS
041100990729     C                   GOTO      FININT
041200990729     C                   END
041300990729     C* Se ho richiamato il pgm con il numero spedizione/
041400990729     C* codice bolla prova a richiamare senza codice bolla
041500990729     C* come del resto faccio se non ho recuperato il C/Ass.
041600990729     C                   MOVEL     'C00'         I50OP0
041700990729     C                   MOVEL     V1CTBL        D50TBL
041800990729     C                   MOVEL     TNSB50        KPJBU
041900990729     C                   CALL      'TNSB50R'
042000990729     C                   PARM                    KPJBA
042100990729     C                   ELSE
042200990729     C                   MOVEL     TNSB50        KPJBU
042300990729     C                   CALL      'TNSB50R'
042400990729     C                   PARM                    KPJBA
042500990729     C                   END
042600990729     C                   MOVEL     KPJBU         TNSB50
042700990729     C                   MOVEL     WSVKPJ        KPJBU
042800990729     C*
042900990729     C     FININT        ENDSR
043000931011     C*----------------------------------------------------*
043100931011     C*   CNTVD2: Controlli subfile                        *
043200931011     C*----------------------------------------------------*
043300931011     C     CNTVD2        BEGSR
043400931011     C*                                                    *
043500931011     C                   Z-ADD     1             NRR
043600950706     C                   MOVEL     ' '           V1CSCE
043700950706     C                   READC     SC41S01                                82
043800931011     C*                                                    *
043900931011     C     *IN82         DOWEQ     '0'
044000931011     C*                                                    *
044100950706     C     V1CSCE        IFEQ      '1'
044200931011     C                   MOVEL     *BLANKS       KPJBU
044300931012     C                   Z-ADD     NRR           SAVREC            4 0
044400950706     C*  Interrogazione bolla
044500950706     C     *INKG         IFEQ      '1'
044600990729     C                   MOVEL     '1'           WACC              1
044700950706     C                   EXSR      GESINT
044800950706     C                   ELSE
044802120612     c                   eval      tnsc40ds = tnsc40
044900120612     C                   MOVE      V1CAAS        £D40AAS
045000120612     C                   MOVE      V1CLNP        £D40LNP
045100120612     C                   MOVE      V1CNRS        £D40NRS
045200120612     C                   MOVE      V1CNSP        £D40NSP
045201120612     C                   clear                   £D40ABI
045202120612     C                   clear                   £D40CAB
045203120612     C                   clear                   £D40NAS
045300120612     C                   MOVEL     TNSC40ds      KPJBU
045400931012     C* Interrogazione                                     *
045500950706     C                   CALL      'TNSC43R'
045600931012     C                   PARM                    KPJBA
045700950706     C                   END
045800931011     C*
045900931012     C                   Z-ADD     SAVREC        NRR
046000931014     C                   Z-ADD     SAVREC        DSPNR1
046100950706     C     NRR           CHAIN     SC41S01                            90
046200931011     C                   SETON                                        70
046300950706     C                   MOVEL     ' '           V1CSCE
046400950706     C  N90              UPDATE    SC41S01
046500931011     C                   END
046600931011     C*
046700950706     C                   READC     SC41S01                                82
046800931011     C                   END
046900931012     C*
047000931011     C                   ENDSR
047100931011      *---------------------------------------------------------------*
047200941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
047300931011      *---------------------------------------------------------------*
047400941102     C     *INZSR        BEGSR
047500931011      *---------------------------------------------------------------*
047600931011     C     *ENTRY        PLIST
047700931011     C                   PARM                    KPJBA
047800950706     C                   MOVEL     KPJBU         TNSC40
047900931012     C*
048000931011     C                   Z-ADD     1             CODUT
048100931011     C                   CALL      'X§PARUT'
048200931011     C                   PARM                    UT§DSE
048300931011     C*          CODUT     CABEQ-1        FINE
048400931011     C                   MOVEL     RAGUT         RSUT             20
048500931011     C                   MOVEL     REC80         CNCR80
048600931011     C                   MOVEL     KNMUS         §SEI              6
048700950706     C*----------------------------------------------------*
048800950706     C*  Definizione CHIAVI
048900950706     C     KCSB1         KLIST
049000950707     C                   KFLD                    KDDP
049100950706     C     KCSB2         KLIST
049200950707     C                   KFLD                    KDDP
049300950707     C                   KFLD                    KNDP
049400950707     C                   KFLD                    KCDI
049500971222     C     KCSB3         KLIST
049600971222     C                   KFLD                    KAAS
049700971222     C                   KFLD                    KLNP
049800971222     C                   KFLD                    KNRS
049900971222     C                   KFLD                    KNSP
050000950706     C     KCSA          KLIST
050100950706     C                   KFLD                    KAAS
050200950706     C                   KFLD                    KLNP
050300950706     C                   KFLD                    KNRS
050400950706     C                   KFLD                    KNSP
050500931012     C*----------------------------------------------------*
050600941102     C*  Definizione variabili
050700950706     C     *LIKE         DEFINE    CSBAAS        KAAS
050800950707     C     *LIKE         DEFINE    CSBCDI        KCDI
050900950707     C     *LIKE         DEFINE    CSBDDP        KDDP
051000950707     C     *LIKE         DEFINE    CSBNDP        KNDP
051100950706     C     *LIKE         DEFINE    CSALNP        KLNP
051200950706     C     *LIKE         DEFINE    CSANRS        KNRS
051300950706     C     *LIKE         DEFINE    CSANSP        KNSP
051400000107     C*----------------------------------------------------*
051500000107     C*  RECUPERO DIVISA MONETA CORRENTE
051600000107     C                   CLEAR                   DGED
051700000107     C                   CLEAR                   DSBS02
051800000107     C                   MOVEL     'C'           T02MOD
051900000107     C                   MOVEL     KNSIF         T02SIF
052000000107     C                   MOVEL     'GED'         T02COD
052100000107     C                   MOVEL     '1'           T02KE1
052200000107     C                   CALL      'TIBS02R'
052300000107     C                   PARM                    KPJBA
052400000107     C                   PARM                    DSBS02
052500000107     C*
052600000107    3C     T02ERR        IFEQ      ' '
052700000107     C                   MOVEL     T02UNI        DGED
052800000107    3C                   ENDIF
052900941102     C*----------------------------------------------------*
053000950706     C*  Inizializzazione delle variabii
053100950706     C                   MOVEL     'N'           WFINE             1
053200950707     C                   Z-ADD     D40PCD        KCDI
053300950707     C                   Z-ADD     D40PDD        KDDP
053400950707     C                   MOVEL     D40PND        KNDP
053500950707     C                   SETON                                        04
053600931012     C*----------------------------------------------------*
053700931011     C                   ENDSR
