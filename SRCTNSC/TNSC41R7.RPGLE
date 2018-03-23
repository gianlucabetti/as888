000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950705      * TNSC41R7                                                     *
000400931011      *       *-----------------------------------------*            *
000500950704      *          INTERROGAZIONE CONTRASSEGNI X SEDE                  *
000600950705      *          SELEZIONE DATI SPEDIZIONE/CLIENTE                   *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931011      * KC -FINE LAVORO                                              *
001100931011      * KG -INTERROGAZIONE BOLLE                                     *
001200931011      * KJ -STAMPA                                                   *
001300931011      * KI -SFLDROP                                                  *
001400931011      * KL -RITORNO                                                  *
001500931011?     *--------------------------------------------------------------*
001600950705     FTNCSB05L  IF   E           K DISK
001700120210     FTNCSm03L  IF   E           K DISK
001800950706     FTNCSA01L  IF   E           K DISK
001900950705     FTNSC41D7  CF   E             WORKSTN
002000950704     F                                     SFILE(SC41S01:NRR)
002100950704?     *--------------------------------------------------------------*
002200950704?     *  SCHIERE                                                     *
002300950704?     *--------------------------------------------------------------*
002400950704     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
002500931011?     *--------------------------------------------------------------*
002600931011?     *  DS                                                          *
002700931011?     *--------------------------------------------------------------*
002800931011     D* DS x controllo rotture documento pagamento
002900950706     D DSSPE           DS
003000950705     D  CSBAAS                 1      4  0
003100950705     D  CSBMGS                 5      8  0
003200950705     D  CSBCDI                 9     15  0
003300931011     D*
003400931011     D SAVDS           DS
003500950705     D  WSVAAS                 1      4  0
003600950705     D  WSVMGS                 5      8  0
003700950705     D  WSVCDI                 9     15  0
003800931011     D KPJBA         E DS
003900931011      *
004000010223     D*FTRUL15    E DSTRUL15DS
004100120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
004200120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
004300980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004400980127     D CNACO         E DS                  EXTNAME(CNACO00F)
004500980127     D CNIND         E DS                  EXTNAME(CNIND00F)
004600980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004700980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004800990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004900931011      *
005000931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100931011     D CNCR80        E DS
005200941102     D*
005300941102     D WLBDA8          DS
005400941102     D  G02DAT                 1      8  0
005500941102     D  G02INV                 9     16  0
005600941102     D  G02ERR                17     17
005700941102     D  G02TGI                18     22  0
005800120210      *
005900120210     d mnra            s                   like(csmnra)
006000120210     d mabi            s                   like(csmabi)
006100120210     d mcab            s                   like(csmcab)
006200931011      *
006300931011     D                SDS
006400931011     D  NOMPG                  1     10
006500931011      *---------------------------------------------------------------*
006600931011      *   C I C L O      P R I N C I P A L E                          *
006700931011      *---------------------------------------------------------------*
006800931011     C*  CARICAMENTO SUBFILE
006900931011     C                   EXSR      CARSFL
007000931011     C*
007100931011     C*  LOOP
007200950704     C     WFINE         DOUEQ     'S'
007300931011     C*
007400950704     C     WTPVID        CASEQ     '1'           GESVD1
007500950704     C     WTPVID        CASEQ     '2'           GESVD2
007600931011     C                   END
007700931011     C*
007800931011     C                   END
007900931011     C*
008000950704     C                   MOVEL     TNSC40        KPJBU
008100971222     C* RICHIAMO PGM X CHIUSURA IN LR
008200980127     C                   CLEAR                   TIBS69
008300980127     C                   MOVEL     'C'           I69TLA
008400980127     C                   CALL      'TIBS69R'
008500980127     C                   PARM                    TIBS69
008600980127     C                   PARM                    CNACO
008700980127     C                   PARM                    CNIND
008800980127     C                   PARM                    CNCLP
008900980127     C                   PARM                    FNCLS
009000971222     C*
009100950704     C                   SETON                                        LR
009200931011      *---------------------------------------------------------------*
009300931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
009400931011      *---------------------------------------------------------------*
009500931011     C     CARSFL        BEGSR
009600931011     C*
009700950704     C*  Imposto campi dati testata
009800950704     C                   CLEAR                   WLBDA8
009900950705     C                   MOVE      D40SDD        G02INV
010000950704     C                   MOVEL     '3'           G02ERR
010100950704     C                   CALL      'XSRDA8'
010200950704     C                   PARM                    WLBDA8
010300950705     C                   MOVE      G02DAT        V1CSDD
010400950705     C                   MOVE      D40SDA        G02INV
010500950704     C                   MOVEL     '3'           G02ERR
010600950704     C                   CALL      'XSRDA8'
010700950704     C                   PARM                    WLBDA8
010800950705     C                   MOVE      G02DAT        V1CSDA
010900931011     C*  Pulisco subfile
011000950704     C                   SETOFF                                       2021
011100950704     C                   WRITE     SC41C01
011200950704     C                   SETON                                        21
011300931013     C                   Z-ADD     1             DSPNR1
011400931011     C*
011500931011     C*  Caricamento subfile
011600950705     C     KCSB          SETLL     TNCSB05L
011700950705     C                   READ      TNCSB05L                               23
011800931011     C*
011900931011     C                   Z-ADD     0             NRR               4 0
012000950704     C                   MOVEL     '1'           WTPVID            1
012100950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
012200950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
012300931011     C*
012400950704     C     *IN23         IFEQ      '0'
012500950706     C     WDTSPE        ANDLE     D40SDA
012600950704     C                   SETON                                        20
012700950704     C                   EXSR      SAVDAT
012800931011     C                   EXSR      ROLLUP
012900931011     C                   END
013000931013     C*
013100931013     C     NRR           IFNE      0
013200950704     C                   MOVEL     '2'           WTPVID            1
013300950704     C                   SETON                                        20
013400931013     C                   END
013500931011     C*
013600931012     C                   MOVEL     NOMPG         NOMPGM
013700950704     C                   Z-ADD     NRR           WSVRE1            4 0
013800931011     C*
013900931011     C                   ENDSR
014000120210     C*----------------------------------------------------*
014100120210     C*   srmulti - per multiassegni
014200120210     C*----------------------------------------------------*
014300120210     C     srmulti       BEGSR
014400120210     C*
014500120210     c                   eval      mnra = *hival
014600120210     c                   eval      mabi = *hival
014700120210     c                   eval      mcab = *hival
014800120210     c     csbnra        setll     tncsm03l
014900120210     c                   do        *hival
015000120210     c     csbnra        reade     tncsm03l
015100120210     c                   if        %eof(tncsm03l)
015200120210     c                   leave
015300120210     c                   end
015400120210     C                   if        csmnra = WNRA
015500120210     c                   eval      mnra = csmnra
015600120210     c                   eval      mabi = csmabi
015700120210     c                   eval      mcab = csmcab
015800120210     c                   leave
015900120210     c                   end
016000120210     c                   enddo
016100120210     C*
016200120210     C                   ENDSR
016300931011     C*----------------------------------------------------*
016400931011     C*   ROLLUP: Caricamento di una pagina                *
016500931011     C*----------------------------------------------------*
016600931011     C     ROLLUP        BEGSR
016700931011     C*
016800950704     C     *IN23         IFEQ      '0'
016900950706     C     WDTSPE        ANDLE     D40SDA
017000950704     C                   Z-ADD     WSVRE1        NRR
017100931011     C                   Z-ADD     0             NRIG              4 0
017200931011     C*
017300950714     C     NRIG          DOWLT     17
017400950704     C     *IN23         ANDEQ     '0'
017500950707     C     WDTSPE        ANDLE     D40SDA
017600931011     C* Scrivo record a rottura codice reigstrazione
017700950706     C     DSSPE         IFNE      SAVDS
017800950705     C     WCLGEN        OREQ      'S'
017900931011     C                   EXSR      WRIREC
018000931011     C                   EXSR      SAVDAT
018100931011     C                   END
018200950706     C*----------------------------------------------------*
018300950704     C* Se il documento è valido controllo se il record rientra
018400950704     C* nei limiti richiesti
018500950704     C                   MOVEL     D40NAS        WNRA             10
018600120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
018700120220     c                             csbtpi = 'M' and csbabi = 0
018800120210     c                   exsr      srmulti
018900120210     c                   else
019000120210     c                   eval      mnra = csbnra
019100120210     c                   eval      mabi = csbabi
019200120210     c                   eval      mcab = csbcab
019300120210     c                   end
019400950706     C*----------------------------------------------------*
019500950704     C     D40ABI        IFEQ      *ZEROS
019600120210     C     D40ABI        OREQ      mABI
019700950720     C     D40CAB        IFEQ      *ZEROS
019800120210     C     D40CAB        OREQ      mCAB
019900140325     C     D40imp        IFEQ      *ZEROS
020000140325     C     D40imp        OREQ      csbcas
020100950720     C     WNRA          IFEQ      *BLANKS
020200950720     C     WNRA          OREQ      *ZEROS
020300120210     C     WNRA          OREQ      mNRA
020400160415     C*  Test  x tipo intestazione
020500160415     C     D40PTN        IFEQ      *BLANKS
020600160415     C     D40PTN        OREQ      'M'
020700160415     C     CSBTPI        ANDEQ     'M'
020800160415     C     D40PTN        OREQ      'B'
020900160415     C     CSBTPI        ANDEQ     ' '
021000950706     C*----------------------------------------------------*
021100950706     C* Selezione per stato C/Assegno
021200950706     C     D40TCR        IFEQ      ' '
021300950706     C     D40TCR        OREQ      'D'
021400950706     C     CSBDDC        ANDEQ     0
021500950706     C     D40TCR        OREQ      'G'
021600950706     C     CSBSTA        ANDEQ     2
021700950706     C     D40TCR        OREQ      'A'
021800950706     C     CSBSTA        ANDEQ     9
021900950706     C     D40TCR        OREQ      'S'
022000010112     C     CSBSTA        ANDEQ     5
022100010112     C     D40TCR        OREQ      '3'
022200010112     C     CSBSTA        ANDEQ     3
022300960911     C     D40TCR        OREQ      'T'
022400960911     C     CSBSTA        ANDEQ     6
022500960911     C     D40TCR        OREQ      'R'
022600960911     C     CSBSTA        ANDEQ     7
022700960911     C     D40TCR        OREQ      'O'
022800960911     C     CSBSTA        ANDEQ     8
022900950706     C     D40TCR        OREQ      'I'
023000950706     C     CSBDDC        ANDGT     0
023100950706     C     CSBDDP        ANDEQ     0
023200950706     C     D40TCR        OREQ      'P'
023300950706     C     CSBDDP        ANDGT     0
023400950714     C                   MOVE      CSBAAS        WSVAAS
023500950714     C                   MOVE      CSBLNP        WSVLNP
023600950714     C                   MOVE      CSBNRS        WSVNRS
023700950714     C                   MOVE      CSBNSP        WSVNSP
023800941102     C                   ADD       CSBCAS        IMPASS
023900950707     C                   ADD       1             V1CSPE
024000950704     C                   MOVEL     'S'           WRTDET
024100950704     C                   END
024200971222     C                   END
024300950706     C*----------------------------------------------------*
024400160415     C                   END
024500950706     C                   END
024600950720     C                   END
024700140325     C                   END
024800950706     C*----------------------------------------------------*
024900950704     C*  Leggo record successivo
025000950706     C                   READ      TNCSB05L                               23
025100950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
025200950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
025300931011     C                   END
025400931011     C*
025500950706     C     WSVDSP        IFLE      D40SDA
025600950714     C     NRIG          ANDLT     17
025700931011     C                   EXSR      WRIREC
025800931011     C                   END
025900931011     C*
026000950714     C  N23WDTSPE        COMP      D40SDA                             23
026100950704     C     NRR           IFGT      WSVRE1
026200950704     C     WSVRE1        ADD       1             DSPNR1
026300950704     C                   Z-ADD     NRR           WSVRE1            4 0
026400931014     C                   END
026500931011     C                   END
026600931011     C*
026700931011     C                   ENDSR
026800931011     C*----------------------------------------------------*
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
027900950706     C                   MOVEL     DSSPE         SAVDS
028000950704     C                   MOVEL     'N'           WRTDET            1
028100160415     C     D40PTN        IFEQ      *BLANKS
028200160415     C     D40PTN        OREQ      'M'
028300160415     C     CSBTPI        ANDEQ     'M'
028400160415     C     D40PTN        OREQ      'B'
028500160415     C     CSBTPI        ANDEQ     ' '
028600950704     C                   MOVEL     CSBVCA        WSVVCA
028700950706     C                   MOVE      WDTSPE        WSVDSP            8 0
028800931011     C                   Z-ADD     0             IMPASS
028900950707     C                   Z-ADD     0             V1CSPE
029000160415     C                   END
029100931011     C*
029200931011     C                   ENDSR
029300931011     C*----------------------------------------------------*
029400931011     C*   WRIREC: Scrittura record                         *
029500931011     C*----------------------------------------------------*
029600931011     C     WRIREC        BEGSR
029700931011     C*
029800950704     C     WRTDET        IFEQ      'S'
029900950711     C                   Z-ADD     WSVAAS        V1CAAS
030000950711     C                   Z-ADD     WSVLNP        V1CLNP
030100950711     C                   Z-ADD     WSVNRS        V1CNRS
030200950711     C                   Z-ADD     WSVNSP        V1CNSP
030300950704     C                   MOVEL     *BLANKS       V1CSCE
030400950706     C                   Z-ADD     WSVDSP        G02INV
030500950704     C                   Z-ADD     0             G02DAT
030600950704     C                   MOVEL     '3'           G02ERR
030700950704     C                   CALL      'XSRDA8'
030800950704     C                   PARM                    WLBDA8
030900950706     C                   Z-ADD     G02DAT        V1CDSP
031000950706     C                   MOVEL     WSVVCA        V1CVAL
031100950706     C                   Z-ADD     WSVCDI        V1CCDI
031200950706     C                   Z-ADD     IMPASS        V1CIMP
031300950704     C*  Decodifico Mittente
031400950706     C     WCLGEN        IFEQ      'S'
031500950707     C                   Z-ADD     WSVAAS        KAAS
031600950707     C                   Z-ADD     WSVLNP        KLNP
031700950707     C                   Z-ADD     WSVNRS        KNRS
031800950707     C                   Z-ADD     WSVNSP        KNSP
031900950706     C                   MOVEL     *BLANKS       CSARSD
032000950706     C     KCSA          CHAIN     TNCSA01L                           31
032100971222     C                   MOVEL     CSARSD        V1CMIT
032200950706     C                   ELSE
032300950704     C                   MOVEL     *BLANKS       ACORAG
032400980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
032500980127     C                   CLEAR                   TIBS69
032600980127     C                   CLEAR                   CNACO
032700980127     C                   CLEAR                   CNIND
032800980127     C                   CLEAR                   CNCLP
032900980127     C                   CLEAR                   FNCLS
033000980127     C                   MOVEL     ' '           I69TLA
033100980127     C                   MOVEL     KNSIF         I69SIF
033200980127     C                   Z-ADD     WSVCDI        I69KAC
033300980127     C                   CALL      'TIBS69R'
033400980127     C                   PARM                    TIBS69
033500980127     C                   PARM                    CNACO
033600980127     C                   PARM                    CNIND
033700980127     C                   PARM                    CNCLP
033800980127     C                   PARM                    FNCLS
033900980127     C     O69ERR        IFEQ      *BLANK
034000950704     C                   MOVEL     ACORAG        V1CMIT
034100950706     C                   END
034200980127     C                   END
034300950706     c*
034400931011     C                   ADD       1             NRR
034500931011     C                   ADD       1             NRIG
034600950704     C                   WRITE     SC41S01
034700931013     C                   END
034800931011     C*
034900931011     C                   ENDSR
035000931011     C*----------------------------------------------------*
035100931011     C*  Routine gestione operazioni prima videata         *
035200931011     C*----------------------------------------------------*
035300931011     C     GESVD1        BEGSR
035400931011     C*-                                                   *
035500950707     C                   SETOFF                                       20
035600950704     C                   WRITE     SC41Z02
035700950704     C                   WRITE     SC41D02
035800950704     C                   EXFMT     SC41C01
035900931011     C*                                                    *
036000931011     C* Fine Programma                                     *
036100931011     C     *INKL         IFEQ      '1'
036200950704     C                   MOVEL     'S'           WFINE
036300931011     C                   END
036400931011     C*-                                                   *
036500931011     C                   ENDSR
036600931011     C*----------------------------------------------------*
036700931011     C*  Routine gestione operazioni subfile               *
036800931011     C*----------------------------------------------------*
036900931011     C     GESVD2        BEGSR
037000931011     C*-                                                   *
037100950704     C  N28              WRITE     SC41Z01
037200950704     C                   EXFMT     SC41C01
037300950704     C                   SETOFF                                       28
037400950706     C                   MOVEL     'N'           WINTBL            1
037500931011     C*                                                    *
037600931011     C* Fine Programma                                     *
037700931011     C     *INKC         IFEQ      '1'
037800950704     C                   MOVEL     'S'           WFINE
037900950704     C                   MOVE      3             D40CMD
038000931011     C                   GOTO      FINVD2
038100931011     C                   END
038200931011     C* Ritorno                                            *
038300931011     C     *INKL         IFEQ      '1'
038400950704     C                   MOVEL     'S'           WFINE
038500950704     C                   MOVE      12            D40CMD
038600931011     C                   GOTO      FINVD2
038700931011     C                   END
038800931011     C* ROLLUP
038900950704     C     *IN25         IFEQ      '1'
039000931013     C                   EXSR      ROLLUP
039100950711     C                   GOTO      FINVD2
039200931013     C                   END
039300950706     C*
039400990729     C                   CLEAR                   TNSB50
039500950706     C                   EXSR      CNTVD2
039600931011     C* Interrogazione Bolle                               *
039700931011     C     *INKG         IFEQ      '1'
039800950706     C     WINTBL        ANDEQ     'N'
039900990729     C                   CLEAR                   TNSB50
040000990729     C                   MOVE      'C00'         I50OP0
040100990729     C                   MOVEL     '0'           O50F03
040200990729     C                   MOVEL     '0'           O50F12
040300990729     C                   MOVEL     '0'           O50ERR
040400990729     C                   MOVEL     *BLANKS       O50MSG
040500990729     C                   MOVEL     KPJBU         WSVKPJ          256
040600990729     C                   MOVEL     *BLANKS       KPJBU
040700990729     C                   MOVEL     TNSB50        KPJBU
040800990729     C                   CALL      'TNSB50R'
040900990729     C                   PARM                    KPJBA
041000990729     C                   MOVEL     KPJBU         TNSB50
041100990729     C                   MOVEL     WSVKPJ        KPJBU
041200950706     C                   END
041300931011     C*                                                    *
041400931011     C*                                                    *
041500931012     C     FINVD2        ENDSR
041600931011     C*----------------------------------------------------*
041700931011     C*   CNTVD2: Controlli subfile                        *
041800931011     C*----------------------------------------------------*
041900931011     C     CNTVD2        BEGSR
042000931011     C*                                                    *
042100931011     C                   Z-ADD     1             NRR
042200950704     C                   MOVEL     'N'           WSELEZ            1
042300950704     C                   READC     SC41S01                                31
042400931011     C*                                                    *
042500950704     C     *IN31         DOWEQ     '0'
042600931011     C*                                                    *
042700950704     C     V1CSCE        IFEQ      '1'
042800950706     C                   Z-ADD     D40SDA        WSVSDA            8 0
042900950704     C                   MOVEL     'S'           WSELEZ            1
043000931011     C                   MOVEL     *BLANKS       KPJBU
043100950704     C                   Z-ADD     NRR           WSVREC            4 0
043200950706     C                   Z-ADD     V1CDSP        G02DAT
043300941102     C                   Z-ADD     0             G02INV
043400941102     C                   MOVEL     ' '           G02ERR
043500941102     C                   CALL      'XSRDA8'
043600941102     C                   PARM                    WLBDA8
043700950706     C                   Z-ADD     G02INV        D40SDD
043800950706     C                   Z-ADD     G02INV        D40SDA
043900950706     C                   Z-ADD     G02DAT        V1CDSP
044000950706     C                   MOVEL     V1CCDI        D40SCD
044100950704     C                   MOVEL     TNSC40        KPJBU
044200950706     C* Interrogazione bolle                               *
044300950706     C     *INKG         IFEQ      '1'
044400990729     C                   MOVEL     'S'           WINTBL
044500990729     C                   CLEAR                   TNSB50
044600990729     C                   MOVE      'C00'         I50OP0
044700990729     C                   MOVEL     '0'           O50F03
044800990729     C                   MOVEL     '0'           O50F12
044900990729     C                   MOVEL     '0'           O50ERR
045000990729     C                   MOVEL     *BLANKS       O50MSG
045100990729     C                   MOVEL     KPJBU         WSVKPJ          256
045200990729     C                   MOVEL     *BLANKS       KPJBU
045300990729     C                   MOVEL     TNSB50        KPJBU
045400990729     C                   CALL      'TNSB50R'
045500990729     C                   PARM                    KPJBA
045600990729     C                   MOVEL     KPJBU         TNSB50
045700990729     C                   MOVEL     WSVKPJ        KPJBU
045800010223     C                   ELSE
045900931012     C* Stampa                                             *
046000931012     C     *INKJ         IFEQ      '1'
046100950707     C                   CALL      'TNSC42R3'
046200931012     C                   PARM                    KPJBA
046300931012     C                   ELSE
046400950706     C* Interrogazione                                     *
046500950707     C     V1CSPE        IFEQ      1
046600120612     c                   eval      tnsc40ds = tnsc40
046700120612     C                   Z-ADD     V1CAAS        £D40AAS
046800120612     C                   Z-ADD     V1CLNP        £D40LNP
046900120612     C                   Z-ADD     V1CNRS        £D40NRS
047000120612     C                   Z-ADD     V1CNSP        £D40NSP
047100120612     C                   clear                   £D40ABI
047200120612     C                   clear                   £D40CAB
047300120612     C                   clear                   £D40NAS
047400120612     C                   MOVEL     TNSC40ds      KPJBU
047500950707     C                   CALL      'TNSC43R'
047600950707     C                   PARM                    KPJBA
047700950707     C                   ELSE
047800950707     C                   CALL      'TNSC41R3'
047900950706     C                   PARM                    KPJBA
048000931012     C                   END
048100950707     C                   END
048200950706     C                   MOVEL     KPJBU         TNSC40
048300950706     C     D40CMD        IFEQ      3
048400950706     C                   MOVEL     'S'           WFINE
048500950706     C                   END
048600950706     C*
048700950706     C                   END
048800931014     C*
048900931011     C*
049000950706     C                   Z-ADD     WSVDSP        D40SDA
049100950704     C                   Z-ADD     WSVREC        NRR
049200950704     C                   Z-ADD     WSVREC        DSPNR1
049300950704     C     NRR           CHAIN     SC41S01                            90
049400950704     C                   SETON                                        22
049500950704     C                   MOVEL     ' '           V1CSCE
049600950704     C  N90              UPDATE    SC41S01
049700931011     C                   END
049800931011     C*
049900950704     C                   READC     SC41S01                                31
050000931011     C                   END
050100931011     C*
050200950704     C     WSELEZ        IFNE      'S'
050300950704     C     *INKJ         ANDEQ     '1'
050400950704     C                   SETON                                        28
050500950704     C                   MOVEL     ERR(1)        $MSG
050600950704     C                   END
050700931012     C*
050800931011     C                   ENDSR
050900931011      *---------------------------------------------------------------*
051000941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
051100931011      *---------------------------------------------------------------*
051200941102     C     *INZSR        BEGSR
051300931011      *---------------------------------------------------------------*
051400931011     C     *ENTRY        PLIST
051500931011     C                   PARM                    KPJBA
051600950704     C                   MOVEL     KPJBU         TNSC40
051700931012     C*
051800931011     C                   Z-ADD     1             CODUT
051900931011     C                   CALL      'X§PARUT'
052000931011     C                   PARM                    UT§DSE
052100931011     C                   MOVEL     RAGUT         RSUT             20
052200931011     C                   MOVEL     REC80         CNCR80
052300931011     C                   MOVEL     KNMUS         §SEI              6
052400950704     C*----------------------------------------------------*
052500950704     C*  Definizione CHIAVI
052600950704     C     KCSB          KLIST
052700950705     C                   KFLD                    KAAS
052800950705     C                   KFLD                    KMGS
052900950706     C     KCSA          KLIST
053000950706     C                   KFLD                    KAAS
053100950706     C                   KFLD                    KLNP
053200950706     C                   KFLD                    KNRS
053300950706     C                   KFLD                    KNSP
053400931012     C*----------------------------------------------------*
053500950704     C*  Definizione variabili
053600950704     C     *LIKE         DEFINE    CSBCAS        IMPASS          + 1
053700950705     C     *LIKE         DEFINE    CSBAAS        KAAS
053800950705     C     *LIKE         DEFINE    CSBMGS        KMGS
053900950706     C     *LIKE         DEFINE    CSBLNP        KLNP
054000950706     C     *LIKE         DEFINE    CSBNRS        KNRS
054100950706     C     *LIKE         DEFINE    CSBNSP        KNSP
054200950706     C     *LIKE         DEFINE    CSBVCA        WSVVCA
054300950707     C     *LIKE         DEFINE    CSBLNP        WSVLNP
054400950707     C     *LIKE         DEFINE    CSBNRS        WSVNRS
054500950707     C     *LIKE         DEFINE    CSBNSP        WSVNSP
054600950704     C*----------------------------------------------------*
054700950704     C                   MOVEL     'N'           WFINE             1
054800950705     C                   MOVEL     D40SDD        KAAS
054900950705     C                   MOVE      D40SDD        KMGS
055000010223     C*F                   CLEARTRUL15
055100931012     C*----------------------------------------------------*
055200931011     C                   ENDSR
055300950705**
055400950705Selezionare le spedizioni da stampare
