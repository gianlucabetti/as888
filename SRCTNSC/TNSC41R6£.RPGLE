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
005501120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
005600980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
005700980127     D CNACO         E DS                  EXTNAME(CNACO00F)
005800980127     D CNIND         E DS                  EXTNAME(CNIND00F)
005900980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006000980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006100990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
006200931011      *
006300931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006400931011     D CNCR80        E DS
006500941102     D*
006600941102     D WLBDA8          DS
006700941102     D  G02DAT                 1      8  0
006800941102     D  G02INV                 9     16  0
006900941102     D  G02ERR                17     17
007000941102     D  G02TGI                18     22  0
007100931011      *
007200120210     d mnra            s                   like(csmnra)
007300120210     d mabi            s                   like(csmabi)
007400120210     d mcab            s                   like(csmcab)
007500120210      *
007600931011     D                SDS
007700931011     D  NOMPG                  1     10
007800020102     D*
007900020102     D YEurCoDS      E DS
008000020102     D                                     INZ
008100931011      *---------------------------------------------------------------*
008200931011      *   COSTANTI TIPO DOCUMENTO                                     *
008300950704     D COST1           C                   CONST('BONIFICO  ')
008400950704     D COST2           C                   CONST('ASSEGNO   ')
008500950704     D COST3           C                   CONST('LETT.MITT.')
008600931011      *---------------------------------------------------------------*
008700931011      *   C I C L O      P R I N C I P A L E                          *
008800931011      *---------------------------------------------------------------*
008900931011     C*  CARICAMENTO SUBFILE
009000931011     C                   EXSR      CARSFL
009100931011     C*
009200931011     C*  LOOP
009300950704     C     WFINE         DOUEQ     'S'
009400931011     C*
009500950704     C     WTPVID        CASEQ     '1'           GESVD1
009600950704     C     WTPVID        CASEQ     '2'           GESVD2
009700931011     C                   END
009800931011     C*
009900931011     C                   END
010000931011     C*
010100950704     C                   MOVEL     TNSC40        KPJBU
010200980127     C*
010300980127     C                   CLEAR                   TIBS69
010400980127     C                   MOVEL     'C'           I69TLA
010500980127     C                   CALL      'TIBS69R'
010600980127     C                   PARM                    TIBS69
010700980127     C                   PARM                    CNACO
010800980127     C                   PARM                    CNIND
010900980127     C                   PARM                    CNCLP
011000980127     C                   PARM                    FNCLS
011100971223     C*
011200950704     C                   SETON                                        LR
011300931011      *---------------------------------------------------------------*
011400931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
011500931011      *---------------------------------------------------------------*
011600931011     C     CARSFL        BEGSR
011700931011     C*
011800950704     C*  Imposto campi dati testata
011900950704     C                   CLEAR                   WLBDA8
012000950704     C                   MOVE      D40PDD        G02INV
012100950704     C                   MOVEL     '3'           G02ERR
012200950704     C                   CALL      'XSRDA8'
012300950704     C                   PARM                    WLBDA8
012400950704     C                   MOVE      G02DAT        V1CPDD
012500950704     C                   MOVE      D40PDA        G02INV
012600950704     C                   MOVEL     '3'           G02ERR
012700950704     C                   CALL      'XSRDA8'
012800950704     C                   PARM                    WLBDA8
012900950704     C                   MOVE      G02DAT        V1CPDA
013000950704     C                   MOVE      D40PTN        V1CTPI
013100950704     C                   MOVE      D40PCD        V1CKCP
013200931011     C*  Pulisco subfile
013300950704     C                   SETOFF                                       2021
013400950704     C                   WRITE     SC41C01
013500950704     C                   SETON                                        21
013600931013     C                   Z-ADD     1             DSPNR1
013700980126     C                   Z-ADD     0             NRR               4 0
013800980126     C                   MOVEL     '1'           WTPVID            1
013900931011     C*
014000931011     C*  Caricamento subfile
014100950704     C     KCSB          SETLL     TNCSB04L
014200950704     C                   READ      TNCSB04L                               23
014300931011     C*
014400931011     C*
014500950704     C     *IN23         IFEQ      '0'
014600950704     C     CSBDDP        ANDLE     D40PDA
014700950704     C                   SETON                                        20
014800950704     C                   EXSR      SAVDAT
014900931011     C                   EXSR      ROLLUP
015000931011     C                   END
015100931013     C*
015200931013     C     NRR           IFNE      0
015300950704     C                   MOVEL     '2'           WTPVID            1
015400950704     C                   SETON                                        20
015500931013     C                   END
015600931011     C*
015700931012     C                   MOVEL     NOMPG         NOMPGM
015800950704     C                   Z-ADD     NRR           WSVRE1            4 0
015900931011     C*
016000931011     C                   ENDSR
016100120210     C*----------------------------------------------------*
016200120210     C*   srmulti - per multiassegni
016300120210     C*----------------------------------------------------*
016400120210     C     srmulti       BEGSR
016500120210     C*
016600120210     c                   eval      mnra = *hival
016700120210     c                   eval      mabi = *hival
016800120210     c                   eval      mcab = *hival
016900120210     c     csbnra        setll     tncsm03l
017000120210     c                   do        *hival
017100120210     c     csbnra        reade     tncsm03l
017200120210     c                   if        %eof(tncsm03l)
017300120210     c                   leave
017400120210     c                   end
017500120210     C                   if        csmnra = WNRA
017600120210     c                   eval      mnra = csmnra
017700120210     c                   eval      mabi = csmabi
017800120210     c                   eval      mcab = csmcab
017900120210     c                   leave
018000120210     c                   end
018100120210     c                   enddo
018200120210     C*
018300120210     C                   ENDSR
018400931011     C*----------------------------------------------------*
018500931011     C*   ROLLUP: Caricamento di una pagina                *
018600931011     C*----------------------------------------------------*
018700931011     C     ROLLUP        BEGSR
018800931011     C*
018900950704     C     *IN23         IFEQ      '0'
019000950704     C     CSBDDP        ANDLE     D40PDA
019100950704     C                   Z-ADD     WSVRE1        NRR
019200931011     C                   Z-ADD     0             NRIG              4 0
019300931011     C*
019400931013     C     NRIG          DOWLT     16
019500950704     C     *IN23         ANDEQ     '0'
019600950704     C     CSBDDP        ANDLE     D40PDA
019700931011     C* Scrivo record a rottura codice reigstrazione
019800950704     C     DSPAG         IFNE      SAVDS
019900950705     C     WCLGEN        OREQ      'S'
020000931011     C                   EXSR      WRIREC
020100931011     C                   EXSR      SAVDAT
020200931011     C                   END
020300950704     C* Se il documento è valido controllo se il record rientra
020400950704     C* nei limiti richiesti
020500950704     C                   MOVEL     D40NAS        WNRA             10
020600971223     C*
020700950704     C     WRTDOC        IFEQ      'S'
020800120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
020900120220     c                             csbtpi = 'M' and csbabi = 0
021000120210     c                   exsr      srmulti
021100120210     c                   else
021200120210     c                   eval      mnra = csbnra
021300120210     c                   eval      mabi = csbabi
021400120210     c                   eval      mcab = csbcab
021500120210     c                   end
021600950704     C     D40ABI        IFEQ      *ZEROS
021700120210     C     D40ABI        OREQ      mABI
021800950720     C     D40CAB        IFEQ      *ZEROS
021900120210     C     D40CAB        OREQ      mCAB
021901140325     C     D40imp        IFEQ      *ZEROS
021902140325     C     D40imp        OREQ      csbcas
022000950720     C     WNRA          IFEQ      *BLANKS
022100950720     C     WNRA          OREQ      *ZEROS
022200120210     C     WNRA          OREQ      mNRA
022300950714     C                   MOVE      CSBAAS        WSVAAS
022400950714     C                   MOVE      CSBLNP        WSVLNP
022500950714     C                   MOVE      CSBNRS        WSVNRS
022600950714     C                   MOVE      CSBNSP        WSVNSP
022700020102
022800020102     C                   IF        CsbVca = 'ITL'
022900020102     C                   CLEAR                   YEurCoDS
023000020102     C                   EVAL      YEcDvI = CsbVca
023100020102     C                   EVAL      YEcImI = CsbCas
023200020102     C                   EVAL      YEcDvO = 'EUR'
023300020102     C                   CALL      'YEURCO'
023400020102     C                   PARM                    YEurCoDS
023500020102     C                   EVAL      ImpAss = ImpAss + YecImO
023600020102     C                   ELSE
023700020102     C                   ADD       CSBCAS        IMPASS
023800020102     C                   ENDIF
023900020102
024000950707     C                   ADD       1             V1CSPE
024100950704     C                   MOVEL     'S'           WRTDET
024200950704     C                   END
024300950720     C                   END
024400950720     C                   END
024500971223     C                   END
024501140325     C                   END
024600950704     C*  Leggo record successivo
024700950704     C                   READ      TNCSB04L                               23
024800931011     C                   END
024900931011     C*
025000950704     C     WSVDDP        IFLE      D40PDA
025100931013     C     NRIG          ANDLT     16
025200931011     C                   EXSR      WRIREC
025300931011     C                   END
025400931011     C*
025500950705     C  N23CSBDDP        COMP      D40PDA                             23
025600950704     C     NRR           IFGT      WSVRE1
025700950704     C     WSVRE1        ADD       1             DSPNR1
025800950704     C                   Z-ADD     NRR           WSVRE1            4 0
025900931014     C                   END
026000931011     C                   END
026100931011     C*
026200931011     C                   ENDSR
026300971223     C*----------------------------------------------------*
026400931011     C*   SAVDAT: Salvataggio dati                         *
026500931011     C*----------------------------------------------------*
026600931011     C     SAVDAT        BEGSR
026700931011     C*
026800950714     C                   MOVE      CSBCDI        W8899             4 0
026900950705     C                   MOVEL     'N'           WCLGEN
027000950705     C     W8899         IFEQ      8888
027100950705     C     W8899         OREQ      9999
027200950705     C                   MOVEL     'S'           WCLGEN            1
027300950705     C                   END
027400950704     C                   MOVEL     DSPAG         SAVDS
027500950704     C                   MOVEL     'N'           WRTDOC            1
027600950704     C                   MOVEL     'N'           WRTDET            1
027700931013     C*  Test  x tipo intestazione
027800950704     C     D40PTN        IFEQ      *BLANKS
027900950704     C     D40PTN        OREQ      'M'
028000931013     C     CSBTPI        ANDEQ     'M'
028100950704     C     D40PTN        OREQ      'B'
028200931013     C     CSBTPI        ANDEQ     ' '
028300000107     C* Scrivo record se divisa corrisponde
028400000107     C     D40PDV        IFEQ      *BLANKS
028500000107     C     D40PDV        OREQ      CSBVCA
028600011001     C     D40PDV        OREQ      §GEDCN
028700000107     C     CSBVCA        ANDEQ     *BLANKS
028800960911     C*----------------------------------------------------*
028900960911     C* Selezione per stato C/Assegno
029000960911     C     D40TCR        IFEQ      'T'
029100960911     C     CSBSTA        ANDEQ     6
029200960911     C     D40TCR        OREQ      'R'
029300960911     C     CSBSTA        ANDEQ     7
029400960911     C     D40TCR        OREQ      'P'
029500960911     C     CSBSTA        ANDEQ     0
029600931013     C*  Test  x codice
029700950704     C     D40PCD        IFEQ      0
029800950704     C     D40PCD        OREQ      CSBCDI
029900950704     C                   MOVEL     'S'           WRTDOC
030000020102
030100020102     C                   IF        CsbVca = 'ITL'
030200020102     C                   EVAL      WSVVCA = 'EUR'
030300020102     C                   ELSE
030400950704     C                   MOVEL     CSBVCA        WSVVCA
030500020102     C                   ENDIF
030600020102
030700950704     C                   Z-ADD     CSBCDI        WSVCDI
030800950704     C                   MOVE      CSBBNA        WSVBNA
030900950704     C                   MOVE      CSBFPC        WSVFPC
031000950704     C                   MOVE      CSBTPI        WSVTPI
031100950704     C                   MOVE      CSBTPA        WSVTPA
031200931013     C                   END
031300000107     C                   END
031400960911     C                   END
031500950704     C*
031600931013     C                   END
031700950704     C*
031800931011     C                   Z-ADD     0             IMPASS
031900950707     C                   Z-ADD     0             V1CSPE
032000931011     C*
032100931011     C                   ENDSR
032200931011     C*----------------------------------------------------*
032300931011     C*   WRIREC: Scrittura record                         *
032400931011     C*----------------------------------------------------*
032500931011     C     WRIREC        BEGSR
032600931011     C*
032700950704     C     WRTDET        IFEQ      'S'
032800950711     C                   Z-ADD     WSVAAS        V1CAAS
032900950711     C                   Z-ADD     WSVLNP        V1CLNP
033000950711     C                   Z-ADD     WSVNRS        V1CNRS
033100950711     C                   Z-ADD     WSVNSP        V1CNSP
033200950704     C                   MOVEL     *BLANKS       V1CSCE
033300950704     C                   Z-ADD     WSVDDP        G02INV
033400950704     C                   Z-ADD     0             G02DAT
033500950704     C                   MOVEL     '3'           G02ERR
033600950704     C                   CALL      'XSRDA8'
033700950704     C                   PARM                    WLBDA8
033800950704     C                   Z-ADD     G02DAT        V1CDDP
033900950704     C                   Z-ADD     WSVNDP        V1CNDP
034000950704     C*  Decodifico Mittente
034100950706     C     WCLGEN        IFEQ      'S'
034200950707     C                   Z-ADD     WSVAAS        KAAS
034300950707     C                   Z-ADD     WSVLNP        KLNP
034400950707     C                   Z-ADD     WSVNRS        KNRS
034500950707     C                   Z-ADD     WSVNSP        KNSP
034600950706     C                   MOVEL     *BLANKS       CSARSD
034700950706     C     KCSA          CHAIN     TNCSA01L                           31
034800950706     C                   MOVEL     CSARSD        V1CMIT
034900950706     C                   ELSE
035000950704     C                   MOVEL     *BLANKS       ACORAG
035100980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
035200980127     C                   CLEAR                   TIBS69
035300980127     C                   CLEAR                   CNACO
035400980127     C                   CLEAR                   CNIND
035500980127     C                   CLEAR                   CNCLP
035600980127     C                   CLEAR                   FNCLS
035700980127     C                   MOVEL     ' '           I69TLA
035800980127     C                   MOVEL     KNSIF         I69SIF
035900980127     C                   Z-ADD     WSVCDI        I69KAC
036000980127     C                   CALL      'TIBS69R'
036100980127     C                   PARM                    TIBS69
036200980127     C                   PARM                    CNACO
036300980127     C                   PARM                    CNIND
036400980127     C                   PARM                    CNCLP
036500980127     C                   PARM                    FNCLS
036600980127     C     O69ERR        IFEQ      *BLANK
036700950704     C                   MOVEL     ACORAG        V1CMIT
036800950706     C                   END
036900980127     C                   END
037000950704     C*  Decodifico Tipo pagamento
037100950704     C                   MOVEL     WSVVCA        V1CVAD
037200950704     C                   Z-ADD     WSVCDI        V1CCDI
037300950704     C                   Z-ADD     IMPASS        V1CIMD
037400950704     C                   MOVE      WSVBNA        V1CKSB
037500950704     C                   MOVEL     *BLANK        V1CTPD
037600931011     C*-------------------------------------------------------*
037700931011     C*  Test  Tipo Documento                                 *
037800950704     C     WSVFPC        IFEQ      '2'
037900950704     C                   MOVEL     COST1         V1CTPD
038000931011     C                   ELSE
038100950704     C     WSVBNA        IFEQ      9999999
038200950704     C                   MOVEL     COST3         V1CTPD
038300931011     C                   ELSE
038400950704     C                   MOVEL     COST2         V1CTPD
038500931011     C                   END
038600931011     C                   END
038700931011     C*-------------------------------------------------------*
038800931011     C                   ADD       1             NRR
038900931011     C                   ADD       1             NRIG
039000950704     C                   WRITE     SC41S01
039100931013     C                   END
039200931011     C*
039300931011     C                   ENDSR
039400931011     C*----------------------------------------------------*
039500931011     C*  Routine gestione operazioni prima videata         *
039600931011     C*----------------------------------------------------*
039700931011     C     GESVD1        BEGSR
039800931011     C*-                                                   *
039900950714     C                   SETOFF                                       20
040000950704     C                   WRITE     SC41Z02
040100950704     C                   WRITE     SC41D02
040200950704     C                   EXFMT     SC41C01
040300931011     C*                                                    *
040400931011     C* Fine Programma                                     *
040500931011     C     *INKL         IFEQ      '1'
040600950704     C                   MOVEL     'S'           WFINE
040700931011     C                   END
040800931011     C*-                                                   *
040900931011     C                   ENDSR
041000931011     C*----------------------------------------------------*
041100931011     C*  Routine gestione operazioni subfile               *
041200931011     C*----------------------------------------------------*
041300931011     C     GESVD2        BEGSR
041400931011     C*-                                                   *
041500950714     C                   SETON                                        20
041600950704     C  N28              WRITE     SC41Z01
041700950704     C                   EXFMT     SC41C01
041800950704     C                   SETOFF                                       28
041900931011     C*                                                    *
042000931011     C* Fine Programma                                     *
042100931011     C     *INKC         IFEQ      '1'
042200950704     C                   MOVEL     'S'           WFINE
042300950704     C                   MOVE      3             D40CMD
042400931011     C                   GOTO      FINVD2
042500931011     C                   END
042600931011     C* Ritorno                                            *
042700931011     C     *INKL         IFEQ      '1'
042800950704     C                   MOVEL     'S'           WFINE
042900950704     C                   MOVE      12            D40CMD
043000931011     C                   GOTO      FINVD2
043100931011     C                   END
043200931011     C* ROLLUP
043300950704     C     *IN25         IFEQ      '1'
043400931013     C                   EXSR      ROLLUP
043500950711     C                   GOTO      FINVD2
043600931013     C                   END
043700931011     C* Interrogazione Bolle                               *
043800931011     C     *INKG         IFEQ      '1'
043900990729     C                   CLEAR                   TNSB50
044000990729     C                   MOVE      'C00'         I50OP0
044100990729     C                   MOVEL     '0'           O50F03
044200990729     C                   MOVEL     '0'           O50F12
044300990729     C                   MOVEL     '0'           O50ERR
044400990729     C                   MOVEL     *BLANKS       O50MSG
044500950717     C                   MOVEL     KPJBU         WSVKPJ          256
044600950717     C                   MOVEL     *BLANKS       KPJBU
044700990729     C                   MOVEL     TNSB50        KPJBU
044800990729     C                   CALL      'TNSB50R'
044900950704     C                   PARM                    KPJBA
045000990729     C                   MOVEL     KPJBU         TNSB50
045100950717     C                   MOVEL     WSVKPJ        KPJBU
045200931011     C                   END
045300931011     C*                                                    *
045400931011     C                   EXSR      CNTVD2
045500931011     C*                                                    *
045600931012     C     FINVD2        ENDSR
045700931011     C*----------------------------------------------------*
045800931011     C*   CNTVD2: Controlli subfile                        *
045900931011     C*----------------------------------------------------*
046000931011     C     CNTVD2        BEGSR
046100931011     C*                                                    *
046200931011     C                   Z-ADD     1             NRR
046300950704     C                   MOVEL     'N'           WSELEZ            1
046400950704     C                   READC     SC41S01                                31
046500931011     C*                                                    *
046600950704     C     *IN31         DOWEQ     '0'
046700931011     C*                                                    *
046800950704     C     V1CSCE        IFEQ      '1'
046900950704     C                   Z-ADD     D40PDA        WSVPDA            8 0
047000950704     C                   MOVEL     'S'           WSELEZ            1
047100931011     C                   MOVEL     *BLANKS       KPJBU
047200950704     C                   Z-ADD     NRR           WSVREC            4 0
047300950704     C                   Z-ADD     V1CDDP        G02DAT
047400941102     C                   Z-ADD     0             G02INV
047500941102     C                   MOVEL     ' '           G02ERR
047600941102     C                   CALL      'XSRDA8'
047700941102     C                   PARM                    WLBDA8
047800950704     C                   Z-ADD     G02INV        D40PDD
047900950704     C                   Z-ADD     G02INV        D40PDA
048000950704     C                   Z-ADD     G02DAT        V1CDDP
048100950704     C                   Z-ADD     V1CNDP        D40PND
048200950704     C                   MOVEL     V1CCDI        D40PCD
048300950704     C                   MOVEL     TNSC40        KPJBU
048400931012     C* Stampa                                             *
048500931012     C     *INKJ         IFEQ      '1'
048600950704     C                   CALL      'TNSC42R4'
048700931012     C                   PARM                    KPJBA
048800931012     C                   ELSE
048900931012     C* Interrogazione                                     *
049000950707     C     V1CSPE        IFEQ      1
049002120612     c                   eval      tnsc40ds = tnsc40
049100120612     C                   Z-ADD     V1CAAS        £D40AAS
049200120612     C                   Z-ADD     V1CLNP        £D40LNP
049300120612     C                   Z-ADD     V1CNRS        £D40NRS
049400120612     C                   Z-ADD     V1CNSP        £D40NSP
049401120612     C                   clear                   £D40ABI
049402120612     C                   clear                   £D40CAB
049403120612     C                   clear                   £D40NAS
049500120612     C                   MOVEL     TNSC40ds      KPJBU
049600950707     C                   CALL      'TNSC43R'
049700950707     C                   PARM                    KPJBA
049800950707     C                   ELSE
049900950704     C                   CALL      'TNSC41R4'
050000931012     C                   PARM                    KPJBA
050100931012     C                   END
050200950707     C                   END
050300931014     C*
050400950704     C                   MOVEL     KPJBU         TNSC40
050500950704     C     D40CMD        IFEQ      3
050600950704     C                   MOVEL     'S'           WFINE
050700931014     C                   END
050800931011     C*
050900950704     C                   Z-ADD     WSVPDA        D40PDA
051000950704     C                   Z-ADD     WSVREC        NRR
051100950704     C                   Z-ADD     WSVREC        DSPNR1
051200950704     C     NRR           CHAIN     SC41S01                            90
051300950704     C                   SETON                                        22
051400950704     C                   MOVEL     ' '           V1CSCE
051500950704     C  N90              UPDATE    SC41S01
051600931011     C                   END
051700931011     C*
051800950704     C                   READC     SC41S01                                31
051900931011     C                   END
052000931011     C*
052100950704     C     WSELEZ        IFNE      'S'
052200950704     C     *INKJ         ANDEQ     '1'
052300950704     C                   SETON                                        28
052400950704     C                   MOVEL     ERR(1)        $MSG
052500950704     C                   END
052600931012     C*
052700931011     C                   ENDSR
052800931011      *---------------------------------------------------------------*
052900941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
053000931011      *---------------------------------------------------------------*
053100941102     C     *INZSR        BEGSR
053200931011      *---------------------------------------------------------------*
053300931011     C     *ENTRY        PLIST
053400931011     C                   PARM                    KPJBA
053500950704     C                   MOVEL     KPJBU         TNSC40
053600931012     C*
053700931011     C                   Z-ADD     1             CODUT
053800931011     C                   CALL      'X§PARUT'
053900931011     C                   PARM                    UT§DSE
054000931011     C                   MOVEL     RAGUT         RSUT             20
054100931011     C                   MOVEL     REC80         CNCR80
054200931011     C                   MOVEL     KNMUS         §SEI              6
054300950704     C*----------------------------------------------------*
054400950704     C*  Definizione CHIAVI
054500971223     C     KCSB          KLIST
054600950704     C                   KFLD                    KDDP
054700950706     C     KCSA          KLIST
054800950706     C                   KFLD                    KAAS
054900950706     C                   KFLD                    KLNP
055000950706     C                   KFLD                    KNRS
055100950706     C                   KFLD                    KNSP
055200931012     C*----------------------------------------------------*
055300950704     C*  Definizione variabili
055400950704     C     *LIKE         DEFINE    CSBCAS        IMPASS          + 1
055500950704     C     *LIKE         DEFINE    CSBDDP        KDDP
055600990615     C     *LIKE         DEFINE    CSBAAS        KAAS
055700990615     C     *LIKE         DEFINE    CSBLNP        KLNP
055800990615     C     *LIKE         DEFINE    CSBNRS        KNRS
055900990615     C     *LIKE         DEFINE    CSBNSP        KNSP
056000950704     C     *LIKE         DEFINE    CSBVCA        WSVVCA
056100950704     C     *LIKE         DEFINE    CSBBNA        WSVBNA
056200950704     C     *LIKE         DEFINE    CSBFPC        WSVFPC
056300950704     C     *LIKE         DEFINE    CSBTPA        WSVTPA
056400950707     C     *LIKE         DEFINE    CSBAAS        WSVAAS
056500950707     C     *LIKE         DEFINE    CSBLNP        WSVLNP
056600950707     C     *LIKE         DEFINE    CSBNRS        WSVNRS
056700950707     C     *LIKE         DEFINE    CSBNSP        WSVNSP
056800950704     C*----------------------------------------------------*
056900950704     C                   MOVEL     'N'           WFINE             1
057000950704     C                   Z-ADD     D40PDD        KDDP
057100000107     C*----------------------------------------------------*
057200000107     C*  RECUPERO DIVISA MONETA CORRENTE
057300000107     C                   CLEAR                   DGED
057400000107     C                   CLEAR                   DSBS02
057500000107     C                   MOVEL     'C'           T02MOD
057600000107     C                   MOVEL     KNSIF         T02SIF
057700000107     C                   MOVEL     'GED'         T02COD
057800000107     C                   MOVEL     '1'           T02KE1
057900000107     C                   CALL      'TIBS02R'
058000000107     C                   PARM                    KPJBA
058100000107     C                   PARM                    DSBS02
058200000107     C*
058300000107    3C     T02ERR        IFEQ      ' '
058400000107     C                   MOVEL     T02UNI        DGED
058500000107    3C                   ENDIF
058600931012     C*----------------------------------------------------*
058700931011     C                   ENDSR
058800950704**
058900950704Selezionare il docunmento da stampare
