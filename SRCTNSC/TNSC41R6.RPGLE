000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950704      * TNSC41R6                                                     *
000400931011      *       *-----------------------------------------*            *
000500950704      *          INTERROGAZIONE CONTRASSEGNI X SEDE                  *
000600931011      *          SELEZIONE DATI DI PAGAMENTO: NR.DOCUM.              *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931011      * KC -FINE LAVORO                                              *
001100931011      * KG -INTERROGAZIONE BOLLE                                     *
001200931011      * KJ -STAMPA                                                   *
001300931011      * KI -SFLDROP                                                  *
001400931011      * KL -RITORNO                                                  *
001500931011?     *--------------------------------------------------------------*
001600971223     FTNCSB04L  IF   E           K DISK
001700120210     FTNCSm03l  IF   E           K DISK
001800950706     FTNCSA01L  IF   E           K DISK
001900950704     FTNSC41D6  CF   E             WORKSTN
002000950704     F                                     SFILE(SC41S01:NRR)
002100950704?     *--------------------------------------------------------------*
002200950704?     *  SCHIERE                                                     *
002300950704?     *--------------------------------------------------------------*
002400950704     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
002500931011?     *--------------------------------------------------------------*
002600931011?     *  DS                                                          *
002700931011?     *--------------------------------------------------------------*
002800000107     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002900000107     D DGED          E DS
003000931011     D* DS x controllo rotture documento pagamento
003100931011     D DSPAG           DS
003200941102     D  CSBDDP                 1      8  0
003300941102     D  CSBNDP                 9     15  0
003400971223     D  CSBTPI                16     16
003500950704     D  CSBCDI                17     23  0
003600931011     D*
003700931011     D SAVDS           DS
003800950704     D  WSVDDP                 1      8  0
003900950704     D  WSVNDP                 9     15  0
004000950704     D  WSVTPI                16     16
004100950704     D  WSVCDI                17     23  0
004200971223     D PAGRU           DS
004300971223     D  CSGDDT                 1      8  0
004400971223     D  CSGNDT                 9     15  0
004500971223     D  CSGFPC                16     16
004600971223     D  CSGKSC                17     23  0
004700971223     D DSGRU           DS
004800971223     D  WSGDDT                 1      8  0
004900971223     D  WSGNDT                 9     15  0
005000971223     D  WSGFPC                16     16
005100971223     D  WSGKSC                17     23  0
005200971223     D*
005300931011     D KPJBA         E DS
005400931011      *
005500120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
005600120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
005700980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
005800980127     D CNACO         E DS                  EXTNAME(CNACO00F)
005900980127     D CNIND         E DS                  EXTNAME(CNIND00F)
006000980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006100980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006200990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
006300931011      *
006400931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006500931011     D CNCR80        E DS
006600941102     D*
006700941102     D WLBDA8          DS
006800941102     D  G02DAT                 1      8  0
006900941102     D  G02INV                 9     16  0
007000941102     D  G02ERR                17     17
007100941102     D  G02TGI                18     22  0
007200931011      *
007300120210     d mnra            s                   like(csmnra)
007400120210     d mabi            s                   like(csmabi)
007500120210     d mcab            s                   like(csmcab)
007600120210      *
007700931011     D                SDS
007800931011     D  NOMPG                  1     10
007900020102     D*
008000020102     D YEurCoDS      E DS
008100020102     D                                     INZ
008200931011      *---------------------------------------------------------------*
008300931011      *   COSTANTI TIPO DOCUMENTO                                     *
008400950704     D COST1           C                   CONST('BONIFICO  ')
008500950704     D COST2           C                   CONST('ASSEGNO   ')
008600950704     D COST3           C                   CONST('LETT.MITT.')
008700931011      *---------------------------------------------------------------*
008800931011      *   C I C L O      P R I N C I P A L E                          *
008900931011      *---------------------------------------------------------------*
009000931011     C*  CARICAMENTO SUBFILE
009100931011     C                   EXSR      CARSFL
009200931011     C*
009300931011     C*  LOOP
009400950704     C     WFINE         DOUEQ     'S'
009500931011     C*
009600950704     C     WTPVID        CASEQ     '1'           GESVD1
009700950704     C     WTPVID        CASEQ     '2'           GESVD2
009800931011     C                   END
009900931011     C*
010000931011     C                   END
010100931011     C*
010200950704     C                   MOVEL     TNSC40        KPJBU
010300980127     C*
010400980127     C                   CLEAR                   TIBS69
010500980127     C                   MOVEL     'C'           I69TLA
010600980127     C                   CALL      'TIBS69R'
010700980127     C                   PARM                    TIBS69
010800980127     C                   PARM                    CNACO
010900980127     C                   PARM                    CNIND
011000980127     C                   PARM                    CNCLP
011100980127     C                   PARM                    FNCLS
011200971223     C*
011300950704     C                   SETON                                        LR
011400931011      *---------------------------------------------------------------*
011500931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
011600931011      *---------------------------------------------------------------*
011700931011     C     CARSFL        BEGSR
011800931011     C*
011900950704     C*  Imposto campi dati testata
012000950704     C                   CLEAR                   WLBDA8
012100950704     C                   MOVE      D40PDD        G02INV
012200950704     C                   MOVEL     '3'           G02ERR
012300950704     C                   CALL      'XSRDA8'
012400950704     C                   PARM                    WLBDA8
012500950704     C                   MOVE      G02DAT        V1CPDD
012600950704     C                   MOVE      D40PDA        G02INV
012700950704     C                   MOVEL     '3'           G02ERR
012800950704     C                   CALL      'XSRDA8'
012900950704     C                   PARM                    WLBDA8
013000950704     C                   MOVE      G02DAT        V1CPDA
013100950704     C                   MOVE      D40PTN        V1CTPI
013200950704     C                   MOVE      D40PCD        V1CKCP
013300160415     C                   MOVE      D40rag        V1Cmit
013400931011     C*  Pulisco subfile
013500950704     C                   SETOFF                                       2021
013600950704     C                   WRITE     SC41C01
013700950704     C                   SETON                                        21
013800931013     C                   Z-ADD     1             DSPNR1
013900980126     C                   Z-ADD     0             NRR               4 0
014000980126     C                   MOVEL     '1'           WTPVID            1
014100931011     C*
014200931011     C*  Caricamento subfile
014300950704     C     KCSB          SETLL     TNCSB04L
014400950704     C                   READ      TNCSB04L                               23
014500931011     C*
014600931011     C*
014700950704     C     *IN23         IFEQ      '0'
014800950704     C     CSBDDP        ANDLE     D40PDA
014900950704     C                   SETON                                        20
015000950704     C                   EXSR      SAVDAT
015100931011     C                   EXSR      ROLLUP
015200931011     C                   END
015300931013     C*
015400931013     C     NRR           IFNE      0
015500950704     C                   MOVEL     '2'           WTPVID            1
015600950704     C                   SETON                                        20
015700931013     C                   END
015800931011     C*
015900931012     C                   MOVEL     NOMPG         NOMPGM
016000950704     C                   Z-ADD     NRR           WSVRE1            4 0
016100931011     C*
016200931011     C                   ENDSR
016300120210     C*----------------------------------------------------*
016400120210     C*   srmulti - per multiassegni
016500120210     C*----------------------------------------------------*
016600120210     C     srmulti       BEGSR
016700120210     C*
016800120210     c                   eval      mnra = *hival
016900120210     c                   eval      mabi = *hival
017000120210     c                   eval      mcab = *hival
017100120210     c     csbnra        setll     tncsm03l
017200120210     c                   do        *hival
017300120210     c     csbnra        reade     tncsm03l
017400120210     c                   if        %eof(tncsm03l)
017500120210     c                   leave
017600120210     c                   end
017700120210     C                   if        csmnra = WNRA
017800120210     c                   eval      mnra = csmnra
017900120210     c                   eval      mabi = csmabi
018000120210     c                   eval      mcab = csmcab
018100120210     c                   leave
018200120210     c                   end
018300120210     c                   enddo
018400120210     C*
018500120210     C                   ENDSR
018600931011     C*----------------------------------------------------*
018700931011     C*   ROLLUP: Caricamento di una pagina                *
018800931011     C*----------------------------------------------------*
018900931011     C     ROLLUP        BEGSR
019000931011     C*
019100950704     C     *IN23         IFEQ      '0'
019200950704     C     CSBDDP        ANDLE     D40PDA
019300950704     C                   Z-ADD     WSVRE1        NRR
019400931011     C                   Z-ADD     0             NRIG              4 0
019500931011     C*
019600931013     C     NRIG          DOWLT     16
019700950704     C     *IN23         ANDEQ     '0'
019800950704     C     CSBDDP        ANDLE     D40PDA
019900931011     C* Scrivo record a rottura codice reigstrazione
020000950704     C     DSPAG         IFNE      SAVDS
020100950705     C     WCLGEN        OREQ      'S'
020200931011     C                   EXSR      WRIREC
020300931011     C                   EXSR      SAVDAT
020400931011     C                   END
020500950704     C* Se il documento è valido controllo se il record rientra
020600950704     C* nei limiti richiesti
020700950704     C                   MOVEL     D40NAS        WNRA             10
020800971223     C*
020900950704     C     WRTDOC        IFEQ      'S'
021000120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
021100120220     c                             csbtpi = 'M' and csbabi = 0
021200120210     c                   exsr      srmulti
021300120210     c                   else
021400120210     c                   eval      mnra = csbnra
021500120210     c                   eval      mabi = csbabi
021600120210     c                   eval      mcab = csbcab
021700120210     c                   end
021800950704     C     D40ABI        IFEQ      *ZEROS
021900120210     C     D40ABI        OREQ      mABI
022000950720     C     D40CAB        IFEQ      *ZEROS
022100120210     C     D40CAB        OREQ      mCAB
022200140325     C     D40imp        IFEQ      *ZEROS
022300140325     C     D40imp        OREQ      csbcas
022400950720     C     WNRA          IFEQ      *BLANKS
022500950720     C     WNRA          OREQ      *ZEROS
022600120210     C     WNRA          OREQ      mNRA
022700950714     C                   MOVE      CSBAAS        WSVAAS
022800950714     C                   MOVE      CSBLNP        WSVLNP
022900950714     C                   MOVE      CSBNRS        WSVNRS
023000950714     C                   MOVE      CSBNSP        WSVNSP
023100020102
023200020102     C                   IF        CsbVca = 'ITL'
023300020102     C                   CLEAR                   YEurCoDS
023400020102     C                   EVAL      YEcDvI = CsbVca
023500020102     C                   EVAL      YEcImI = CsbCas
023600020102     C                   EVAL      YEcDvO = 'EUR'
023700020102     C                   CALL      'YEURCO'
023800020102     C                   PARM                    YEurCoDS
023900020102     C                   EVAL      ImpAss = ImpAss + YecImO
024000020102     C                   ELSE
024100020102     C                   ADD       CSBCAS        IMPASS
024200020102     C                   ENDIF
024300020102
024400950707     C                   ADD       1             V1CSPE
024500950704     C                   MOVEL     'S'           WRTDET
024600950704     C                   END
024700950720     C                   END
024800950720     C                   END
024900971223     C                   END
025000140325     C                   END
025100950704     C*  Leggo record successivo
025200950704     C                   READ      TNCSB04L                               23
025300931011     C                   END
025400931011     C*
025500950704     C     WSVDDP        IFLE      D40PDA
025600931013     C     NRIG          ANDLT     16
025700931011     C                   EXSR      WRIREC
025800931011     C                   END
025900931011     C*
026000950705     C  N23CSBDDP        COMP      D40PDA                             23
026100950704     C     NRR           IFGT      WSVRE1
026200950704     C     WSVRE1        ADD       1             DSPNR1
026300950704     C                   Z-ADD     NRR           WSVRE1            4 0
026400931014     C                   END
026500931011     C                   END
026600931011     C*
026700931011     C                   ENDSR
026800971223     C*----------------------------------------------------*
026900931011     C*   SAVDAT: Salvataggio dati                         *
027000931011     C*----------------------------------------------------*
027100931011     C     SAVDAT        BEGSR
027200931011     C*
027300950714     C                   MOVE      CSBCDI        W8899             4 0
027400950705     C                   MOVEL     'N'           WCLGEN
027500950705     C     W8899         IFEQ      8888
027600950705     C     W8899         OREQ      9999
027700950705     C                   MOVEL     'S'           WCLGEN            1
027800950705     C                   END
027900950704     C                   MOVEL     DSPAG         SAVDS
028000950704     C                   MOVEL     'N'           WRTDOC            1
028100950704     C                   MOVEL     'N'           WRTDET            1
028200931013     C*  Test  x tipo intestazione
028300950704     C     D40PTN        IFEQ      *BLANKS
028400950704     C     D40PTN        OREQ      'M'
028500931013     C     CSBTPI        ANDEQ     'M'
028600950704     C     D40PTN        OREQ      'B'
028700931013     C     CSBTPI        ANDEQ     ' '
028800000107     C* Scrivo record se divisa corrisponde
028900000107     C     D40PDV        IFEQ      *BLANKS
029000000107     C     D40PDV        OREQ      CSBVCA
029100011001     C     D40PDV        OREQ      §GEDCN
029200000107     C     CSBVCA        ANDEQ     *BLANKS
029300960911     C*----------------------------------------------------*
029400960911     C* Selezione per stato C/Assegno
029500960911     C     D40TCR        IFEQ      'T'
029600960911     C     CSBSTA        ANDEQ     6
029700960911     C     D40TCR        OREQ      'R'
029800960911     C     CSBSTA        ANDEQ     7
029900960911     C     D40TCR        OREQ      'P'
030000960911     C     CSBSTA        ANDEQ     0
030100931013     C*  Test  x codice
030200950704     C     D40PCD        IFEQ      0
030300950704     C     D40PCD        OREQ      CSBCDI
030400950704     C                   MOVEL     'S'           WRTDOC
030500020102
030600020102     C                   IF        CsbVca = 'ITL'
030700020102     C                   EVAL      WSVVCA = 'EUR'
030800020102     C                   ELSE
030900950704     C                   MOVEL     CSBVCA        WSVVCA
031000020102     C                   ENDIF
031100020102
031200950704     C                   Z-ADD     CSBCDI        WSVCDI
031300950704     C                   MOVE      CSBBNA        WSVBNA
031400950704     C                   MOVE      CSBFPC        WSVFPC
031500950704     C                   MOVE      CSBTPI        WSVTPI
031600950704     C                   MOVE      CSBTPA        WSVTPA
031700931013     C                   END
031800000107     C                   END
031900960911     C                   END
032000950704     C*
032100931013     C                   END
032200950704     C*
032300931011     C                   Z-ADD     0             IMPASS
032400950707     C                   Z-ADD     0             V1CSPE
032500931011     C*
032600931011     C                   ENDSR
032700931011     C*----------------------------------------------------*
032800931011     C*   WRIREC: Scrittura record                         *
032900931011     C*----------------------------------------------------*
033000931011     C     WRIREC        BEGSR
033100931011     C*
033200950704     C     WRTDET        IFEQ      'S'
033300950711     C                   Z-ADD     WSVAAS        V1CAAS
033400950711     C                   Z-ADD     WSVLNP        V1CLNP
033500950711     C                   Z-ADD     WSVNRS        V1CNRS
033600950711     C                   Z-ADD     WSVNSP        V1CNSP
033700950704     C                   MOVEL     *BLANKS       V1CSCE
033800950704     C                   Z-ADD     WSVDDP        G02INV
033900950704     C                   Z-ADD     0             G02DAT
034000950704     C                   MOVEL     '3'           G02ERR
034100950704     C                   CALL      'XSRDA8'
034200950704     C                   PARM                    WLBDA8
034300950704     C                   Z-ADD     G02DAT        V1CDDP
034400950704     C                   Z-ADD     WSVNDP        V1CNDP
034500950704     C*  Decodifico Mittente
034600950706     C     WCLGEN        IFEQ      'S'
034700950707     C                   Z-ADD     WSVAAS        KAAS
034800950707     C                   Z-ADD     WSVLNP        KLNP
034900950707     C                   Z-ADD     WSVNRS        KNRS
035000950707     C                   Z-ADD     WSVNSP        KNSP
035100950706     C                   MOVEL     *BLANKS       CSARSD
035200950706     C     KCSA          CHAIN     TNCSA01L                           31
035300950706     C                   MOVEL     CSARSD        V1CMIT
035400950706     C                   ELSE
035500950704     C                   MOVEL     *BLANKS       ACORAG
035600980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
035700980127     C                   CLEAR                   TIBS69
035800980127     C                   CLEAR                   CNACO
035900980127     C                   CLEAR                   CNIND
036000980127     C                   CLEAR                   CNCLP
036100980127     C                   CLEAR                   FNCLS
036200980127     C                   MOVEL     ' '           I69TLA
036300980127     C                   MOVEL     KNSIF         I69SIF
036400980127     C                   Z-ADD     WSVCDI        I69KAC
036500980127     C                   CALL      'TIBS69R'
036600980127     C                   PARM                    TIBS69
036700980127     C                   PARM                    CNACO
036800980127     C                   PARM                    CNIND
036900980127     C                   PARM                    CNCLP
037000980127     C                   PARM                    FNCLS
037100980127     C     O69ERR        IFEQ      *BLANK
037200950704     C                   MOVEL     ACORAG        V1CMIT
037300950706     C                   END
037400980127     C                   END
037500160415      *test se inserita parzializzazione ragione sociale
037600160516     c                   if        d40rag <> *blank and wclgen <>'S'
037700160516     c                             and %scan(%trim(d40rag):acorag) > 0
037800160516     c                             or d40rag <> *blank and wclgen = 'S'
037900160516     c                             and %scan(%trim(d40rag):CSARSD) > 0
038000160505     c                             or d40rag = *blank
038100950704     C*  Decodifico Tipo pagamento
038200950704     C                   MOVEL     WSVVCA        V1CVAD
038300950704     C                   Z-ADD     WSVCDI        V1CCDI
038400950704     C                   Z-ADD     IMPASS        V1CIMD
038500950704     C                   MOVE      WSVBNA        V1CKSB
038600950704     C                   MOVEL     *BLANK        V1CTPD
038700931011     C*-------------------------------------------------------*
038800931011     C*  Test  Tipo Documento                                 *
038900950704     C     WSVFPC        IFEQ      '2'
039000950704     C                   MOVEL     COST1         V1CTPD
039100931011     C                   ELSE
039200950704     C     WSVBNA        IFEQ      9999999
039300950704     C                   MOVEL     COST3         V1CTPD
039400931011     C                   ELSE
039500950704     C                   MOVEL     COST2         V1CTPD
039600931011     C                   END
039700931011     C                   END
039800931011     C*-------------------------------------------------------*
039900931011     C                   ADD       1             NRR
040000931011     C                   ADD       1             NRIG
040100950704     C                   WRITE     SC41S01
040200160415     c                   endif
040300931013     C                   END
040400931011     C*
040500931011     C                   ENDSR
040600931011     C*----------------------------------------------------*
040700931011     C*  Routine gestione operazioni prima videata         *
040800931011     C*----------------------------------------------------*
040900931011     C     GESVD1        BEGSR
041000931011     C*-                                                   *
041100950714     C                   SETOFF                                       20
041200950704     C                   WRITE     SC41Z02
041300950704     C                   WRITE     SC41D02
041400950704     C                   EXFMT     SC41C01
041500931011     C*                                                    *
041600931011     C* Fine Programma                                     *
041700931011     C     *INKL         IFEQ      '1'
041800950704     C                   MOVEL     'S'           WFINE
041900931011     C                   END
042000931011     C*-                                                   *
042100931011     C                   ENDSR
042200931011     C*----------------------------------------------------*
042300931011     C*  Routine gestione operazioni subfile               *
042400931011     C*----------------------------------------------------*
042500931011     C     GESVD2        BEGSR
042600931011     C*-                                                   *
042700950714     C                   SETON                                        20
042800950704     C  N28              WRITE     SC41Z01
042900950704     C                   EXFMT     SC41C01
043000950704     C                   SETOFF                                       28
043100931011     C*                                                    *
043200931011     C* Fine Programma                                     *
043300931011     C     *INKC         IFEQ      '1'
043400950704     C                   MOVEL     'S'           WFINE
043500950704     C                   MOVE      3             D40CMD
043600931011     C                   GOTO      FINVD2
043700931011     C                   END
043800931011     C* Ritorno                                            *
043900931011     C     *INKL         IFEQ      '1'
044000950704     C                   MOVEL     'S'           WFINE
044100950704     C                   MOVE      12            D40CMD
044200931011     C                   GOTO      FINVD2
044300931011     C                   END
044400931011     C* ROLLUP
044500950704     C     *IN25         IFEQ      '1'
044600931013     C                   EXSR      ROLLUP
044700950711     C                   GOTO      FINVD2
044800931013     C                   END
044900931011     C* Interrogazione Bolle                               *
045000931011     C     *INKG         IFEQ      '1'
045100990729     C                   CLEAR                   TNSB50
045200990729     C                   MOVE      'C00'         I50OP0
045300990729     C                   MOVEL     '0'           O50F03
045400990729     C                   MOVEL     '0'           O50F12
045500990729     C                   MOVEL     '0'           O50ERR
045600990729     C                   MOVEL     *BLANKS       O50MSG
045700950717     C                   MOVEL     KPJBU         WSVKPJ          256
045800950717     C                   MOVEL     *BLANKS       KPJBU
045900990729     C                   MOVEL     TNSB50        KPJBU
046000990729     C                   CALL      'TNSB50R'
046100950704     C                   PARM                    KPJBA
046200990729     C                   MOVEL     KPJBU         TNSB50
046300950717     C                   MOVEL     WSVKPJ        KPJBU
046400931011     C                   END
046500931011     C*                                                    *
046600931011     C                   EXSR      CNTVD2
046700931011     C*                                                    *
046800931012     C     FINVD2        ENDSR
046900931011     C*----------------------------------------------------*
047000931011     C*   CNTVD2: Controlli subfile                        *
047100931011     C*----------------------------------------------------*
047200931011     C     CNTVD2        BEGSR
047300931011     C*                                                    *
047400931011     C                   Z-ADD     1             NRR
047500950704     C                   MOVEL     'N'           WSELEZ            1
047600950704     C                   READC     SC41S01                                31
047700931011     C*                                                    *
047800950704     C     *IN31         DOWEQ     '0'
047900931011     C*                                                    *
048000950704     C     V1CSCE        IFEQ      '1'
048100950704     C                   Z-ADD     D40PDA        WSVPDA            8 0
048200950704     C                   MOVEL     'S'           WSELEZ            1
048300931011     C                   MOVEL     *BLANKS       KPJBU
048400950704     C                   Z-ADD     NRR           WSVREC            4 0
048500950704     C                   Z-ADD     V1CDDP        G02DAT
048600941102     C                   Z-ADD     0             G02INV
048700941102     C                   MOVEL     ' '           G02ERR
048800941102     C                   CALL      'XSRDA8'
048900941102     C                   PARM                    WLBDA8
049000950704     C                   Z-ADD     G02INV        D40PDD
049100950704     C                   Z-ADD     G02INV        D40PDA
049200950704     C                   Z-ADD     G02DAT        V1CDDP
049300950704     C                   Z-ADD     V1CNDP        D40PND
049400950704     C                   MOVEL     V1CCDI        D40PCD
049500950704     C                   MOVEL     TNSC40        KPJBU
049600931012     C* Stampa                                             *
049700931012     C     *INKJ         IFEQ      '1'
049800950704     C                   CALL      'TNSC42R4'
049900931012     C                   PARM                    KPJBA
050000931012     C                   ELSE
050100931012     C* Interrogazione                                     *
050200950707     C     V1CSPE        IFEQ      1
050300120612     c                   eval      tnsc40ds = tnsc40
050400120612     C                   Z-ADD     V1CAAS        £D40AAS
050500120612     C                   Z-ADD     V1CLNP        £D40LNP
050600120612     C                   Z-ADD     V1CNRS        £D40NRS
050700120612     C                   Z-ADD     V1CNSP        £D40NSP
050800120612     C                   clear                   £D40ABI
050900120612     C                   clear                   £D40CAB
051000120612     C                   clear                   £D40NAS
051100120612     C                   MOVEL     TNSC40ds      KPJBU
051200950707     C                   CALL      'TNSC43R'
051300950707     C                   PARM                    KPJBA
051400950707     C                   ELSE
051500950704     C                   CALL      'TNSC41R4'
051600931012     C                   PARM                    KPJBA
051700931012     C                   END
051800950707     C                   END
051900931014     C*
052000950704     C                   MOVEL     KPJBU         TNSC40
052100950704     C     D40CMD        IFEQ      3
052200950704     C                   MOVEL     'S'           WFINE
052300931014     C                   END
052400931011     C*
052500950704     C                   Z-ADD     WSVPDA        D40PDA
052600950704     C                   Z-ADD     WSVREC        NRR
052700950704     C                   Z-ADD     WSVREC        DSPNR1
052800950704     C     NRR           CHAIN     SC41S01                            90
052900950704     C                   SETON                                        22
053000950704     C                   MOVEL     ' '           V1CSCE
053100950704     C  N90              UPDATE    SC41S01
053200931011     C                   END
053300931011     C*
053400950704     C                   READC     SC41S01                                31
053500931011     C                   END
053600931011     C*
053700950704     C     WSELEZ        IFNE      'S'
053800950704     C     *INKJ         ANDEQ     '1'
053900950704     C                   SETON                                        28
054000950704     C                   MOVEL     ERR(1)        $MSG
054100950704     C                   END
054200931012     C*
054300931011     C                   ENDSR
054400931011      *---------------------------------------------------------------*
054500941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
054600931011      *---------------------------------------------------------------*
054700941102     C     *INZSR        BEGSR
054800931011      *---------------------------------------------------------------*
054900931011     C     *ENTRY        PLIST
055000931011     C                   PARM                    KPJBA
055100950704     C                   MOVEL     KPJBU         TNSC40
055200931012     C*
055300931011     C                   Z-ADD     1             CODUT
055400931011     C                   CALL      'X§PARUT'
055500931011     C                   PARM                    UT§DSE
055600931011     C                   MOVEL     RAGUT         RSUT             20
055700931011     C                   MOVEL     REC80         CNCR80
055800931011     C                   MOVEL     KNMUS         §SEI              6
055900950704     C*----------------------------------------------------*
056000950704     C*  Definizione CHIAVI
056100971223     C     KCSB          KLIST
056200950704     C                   KFLD                    KDDP
056300950706     C     KCSA          KLIST
056400950706     C                   KFLD                    KAAS
056500950706     C                   KFLD                    KLNP
056600950706     C                   KFLD                    KNRS
056700950706     C                   KFLD                    KNSP
056800931012     C*----------------------------------------------------*
056900950704     C*  Definizione variabili
057000950704     C     *LIKE         DEFINE    CSBCAS        IMPASS          + 1
057100950704     C     *LIKE         DEFINE    CSBDDP        KDDP
057200990615     C     *LIKE         DEFINE    CSBAAS        KAAS
057300990615     C     *LIKE         DEFINE    CSBLNP        KLNP
057400990615     C     *LIKE         DEFINE    CSBNRS        KNRS
057500990615     C     *LIKE         DEFINE    CSBNSP        KNSP
057600950704     C     *LIKE         DEFINE    CSBVCA        WSVVCA
057700950704     C     *LIKE         DEFINE    CSBBNA        WSVBNA
057800950704     C     *LIKE         DEFINE    CSBFPC        WSVFPC
057900950704     C     *LIKE         DEFINE    CSBTPA        WSVTPA
058000950707     C     *LIKE         DEFINE    CSBAAS        WSVAAS
058100950707     C     *LIKE         DEFINE    CSBLNP        WSVLNP
058200950707     C     *LIKE         DEFINE    CSBNRS        WSVNRS
058300950707     C     *LIKE         DEFINE    CSBNSP        WSVNSP
058400950704     C*----------------------------------------------------*
058500950704     C                   MOVEL     'N'           WFINE             1
058600950704     C                   Z-ADD     D40PDD        KDDP
058700000107     C*----------------------------------------------------*
058800000107     C*  RECUPERO DIVISA MONETA CORRENTE
058900000107     C                   CLEAR                   DGED
059000000107     C                   CLEAR                   DSBS02
059100000107     C                   MOVEL     'C'           T02MOD
059200000107     C                   MOVEL     KNSIF         T02SIF
059300000107     C                   MOVEL     'GED'         T02COD
059400000107     C                   MOVEL     '1'           T02KE1
059500000107     C                   CALL      'TIBS02R'
059600000107     C                   PARM                    KPJBA
059700000107     C                   PARM                    DSBS02
059800000107     C*
059900000107    3C     T02ERR        IFEQ      ' '
060000000107     C                   MOVEL     T02UNI        DGED
060100000107    3C                   ENDIF
060200931012     C*----------------------------------------------------*
060300931011     C                   ENDSR
060400950704**
060500950704Selezionare il docunmento da stampare
