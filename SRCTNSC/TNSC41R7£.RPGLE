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
004101120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
004200980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
004300980127     D CNACO         E DS                  EXTNAME(CNACO00F)
004400980127     D CNIND         E DS                  EXTNAME(CNIND00F)
004500980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
004600980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004700990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004800931011      *
004900931011     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000931011     D CNCR80        E DS
005100941102     D*
005200941102     D WLBDA8          DS
005300941102     D  G02DAT                 1      8  0
005400941102     D  G02INV                 9     16  0
005500941102     D  G02ERR                17     17
005600941102     D  G02TGI                18     22  0
005700120210      *
005800120210     d mnra            s                   like(csmnra)
005900120210     d mabi            s                   like(csmabi)
006000120210     d mcab            s                   like(csmcab)
006100931011      *
006200931011     D                SDS
006300931011     D  NOMPG                  1     10
006400931011      *---------------------------------------------------------------*
006500931011      *   C I C L O      P R I N C I P A L E                          *
006600931011      *---------------------------------------------------------------*
006700931011     C*  CARICAMENTO SUBFILE
006800931011     C                   EXSR      CARSFL
006900931011     C*
007000931011     C*  LOOP
007100950704     C     WFINE         DOUEQ     'S'
007200931011     C*
007300950704     C     WTPVID        CASEQ     '1'           GESVD1
007400950704     C     WTPVID        CASEQ     '2'           GESVD2
007500931011     C                   END
007600931011     C*
007700931011     C                   END
007800931011     C*
007900950704     C                   MOVEL     TNSC40        KPJBU
008000971222     C* RICHIAMO PGM X CHIUSURA IN LR
008100980127     C                   CLEAR                   TIBS69
008200980127     C                   MOVEL     'C'           I69TLA
008300980127     C                   CALL      'TIBS69R'
008400980127     C                   PARM                    TIBS69
008500980127     C                   PARM                    CNACO
008600980127     C                   PARM                    CNIND
008700980127     C                   PARM                    CNCLP
008800980127     C                   PARM                    FNCLS
008900971222     C*
009000950704     C                   SETON                                        LR
009100931011      *---------------------------------------------------------------*
009200931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
009300931011      *---------------------------------------------------------------*
009400931011     C     CARSFL        BEGSR
009500931011     C*
009600950704     C*  Imposto campi dati testata
009700950704     C                   CLEAR                   WLBDA8
009800950705     C                   MOVE      D40SDD        G02INV
009900950704     C                   MOVEL     '3'           G02ERR
010000950704     C                   CALL      'XSRDA8'
010100950704     C                   PARM                    WLBDA8
010200950705     C                   MOVE      G02DAT        V1CSDD
010300950705     C                   MOVE      D40SDA        G02INV
010400950704     C                   MOVEL     '3'           G02ERR
010500950704     C                   CALL      'XSRDA8'
010600950704     C                   PARM                    WLBDA8
010700950705     C                   MOVE      G02DAT        V1CSDA
010800931011     C*  Pulisco subfile
010900950704     C                   SETOFF                                       2021
011000950704     C                   WRITE     SC41C01
011100950704     C                   SETON                                        21
011200931013     C                   Z-ADD     1             DSPNR1
011300931011     C*
011400931011     C*  Caricamento subfile
011500950705     C     KCSB          SETLL     TNCSB05L
011600950705     C                   READ      TNCSB05L                               23
011700931011     C*
011800931011     C                   Z-ADD     0             NRR               4 0
011900950704     C                   MOVEL     '1'           WTPVID            1
012000950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
012100950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
012200931011     C*
012300950704     C     *IN23         IFEQ      '0'
012400950706     C     WDTSPE        ANDLE     D40SDA
012500950704     C                   SETON                                        20
012600950704     C                   EXSR      SAVDAT
012700931011     C                   EXSR      ROLLUP
012800931011     C                   END
012900931013     C*
013000931013     C     NRR           IFNE      0
013100950704     C                   MOVEL     '2'           WTPVID            1
013200950704     C                   SETON                                        20
013300931013     C                   END
013400931011     C*
013500931012     C                   MOVEL     NOMPG         NOMPGM
013600950704     C                   Z-ADD     NRR           WSVRE1            4 0
013700931011     C*
013800931011     C                   ENDSR
013900120210     C*----------------------------------------------------*
014000120210     C*   srmulti - per multiassegni
014100120210     C*----------------------------------------------------*
014200120210     C     srmulti       BEGSR
014300120210     C*
014400120210     c                   eval      mnra = *hival
014500120210     c                   eval      mabi = *hival
014600120210     c                   eval      mcab = *hival
014700120210     c     csbnra        setll     tncsm03l
014800120210     c                   do        *hival
014900120210     c     csbnra        reade     tncsm03l
015000120210     c                   if        %eof(tncsm03l)
015100120210     c                   leave
015200120210     c                   end
015300120210     C                   if        csmnra = WNRA
015400120210     c                   eval      mnra = csmnra
015500120210     c                   eval      mabi = csmabi
015600120210     c                   eval      mcab = csmcab
015700120210     c                   leave
015800120210     c                   end
015900120210     c                   enddo
016000120210     C*
016100120210     C                   ENDSR
016200931011     C*----------------------------------------------------*
016300931011     C*   ROLLUP: Caricamento di una pagina                *
016400931011     C*----------------------------------------------------*
016500931011     C     ROLLUP        BEGSR
016600931011     C*
016700950704     C     *IN23         IFEQ      '0'
016800950706     C     WDTSPE        ANDLE     D40SDA
016900950704     C                   Z-ADD     WSVRE1        NRR
017000931011     C                   Z-ADD     0             NRIG              4 0
017100931011     C*
017200950714     C     NRIG          DOWLT     17
017300950704     C     *IN23         ANDEQ     '0'
017400950707     C     WDTSPE        ANDLE     D40SDA
017500931011     C* Scrivo record a rottura codice reigstrazione
017600950706     C     DSSPE         IFNE      SAVDS
017700950705     C     WCLGEN        OREQ      'S'
017800931011     C                   EXSR      WRIREC
017900931011     C                   EXSR      SAVDAT
018000931011     C                   END
018100950706     C*----------------------------------------------------*
018200950704     C* Se il documento è valido controllo se il record rientra
018300950704     C* nei limiti richiesti
018400950704     C                   MOVEL     D40NAS        WNRA             10
018500120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
018600120220     c                             csbtpi = 'M' and csbabi = 0
018700120210     c                   exsr      srmulti
018800120210     c                   else
018900120210     c                   eval      mnra = csbnra
019000120210     c                   eval      mabi = csbabi
019100120210     c                   eval      mcab = csbcab
019200120210     c                   end
019300950706     C*----------------------------------------------------*
019400950704     C     D40ABI        IFEQ      *ZEROS
019500120210     C     D40ABI        OREQ      mABI
019600950720     C     D40CAB        IFEQ      *ZEROS
019700120210     C     D40CAB        OREQ      mCAB
019701140325     C     D40imp        IFEQ      *ZEROS
019702140325     C     D40imp        OREQ      csbcas
019800950720     C     WNRA          IFEQ      *BLANKS
019900950720     C     WNRA          OREQ      *ZEROS
020000120210     C     WNRA          OREQ      mNRA
020100950706     C*----------------------------------------------------*
020200950706     C* Selezione per stato C/Assegno
020300950706     C     D40TCR        IFEQ      ' '
020400950706     C     D40TCR        OREQ      'D'
020500950706     C     CSBDDC        ANDEQ     0
020600950706     C     D40TCR        OREQ      'G'
020700950706     C     CSBSTA        ANDEQ     2
020800950706     C     D40TCR        OREQ      'A'
020900950706     C     CSBSTA        ANDEQ     9
021000950706     C     D40TCR        OREQ      'S'
021100010112     C     CSBSTA        ANDEQ     5
021200010112     C     D40TCR        OREQ      '3'
021300010112     C     CSBSTA        ANDEQ     3
021400960911     C     D40TCR        OREQ      'T'
021500960911     C     CSBSTA        ANDEQ     6
021600960911     C     D40TCR        OREQ      'R'
021700960911     C     CSBSTA        ANDEQ     7
021800960911     C     D40TCR        OREQ      'O'
021900960911     C     CSBSTA        ANDEQ     8
022000950706     C     D40TCR        OREQ      'I'
022100950706     C     CSBDDC        ANDGT     0
022200950706     C     CSBDDP        ANDEQ     0
022300950706     C     D40TCR        OREQ      'P'
022400950706     C     CSBDDP        ANDGT     0
022500950714     C                   MOVE      CSBAAS        WSVAAS
022600950714     C                   MOVE      CSBLNP        WSVLNP
022700950714     C                   MOVE      CSBNRS        WSVNRS
022800950714     C                   MOVE      CSBNSP        WSVNSP
022900941102     C                   ADD       CSBCAS        IMPASS
023000950707     C                   ADD       1             V1CSPE
023100950704     C                   MOVEL     'S'           WRTDET
023200950704     C                   END
023300971222     C                   END
023400950706     C*----------------------------------------------------*
023500950706     C                   END
023600950720     C                   END
023601140325     C                   END
023700950706     C*----------------------------------------------------*
023800950704     C*  Leggo record successivo
023900950706     C                   READ      TNCSB05L                               23
024000950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
024100950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
024200931011     C                   END
024300931011     C*
024400950706     C     WSVDSP        IFLE      D40SDA
024500950714     C     NRIG          ANDLT     17
024600931011     C                   EXSR      WRIREC
024700931011     C                   END
024800931011     C*
024900950714     C  N23WDTSPE        COMP      D40SDA                             23
025000950704     C     NRR           IFGT      WSVRE1
025100950704     C     WSVRE1        ADD       1             DSPNR1
025200950704     C                   Z-ADD     NRR           WSVRE1            4 0
025300931014     C                   END
025400931011     C                   END
025500931011     C*
025600931011     C                   ENDSR
025700931011     C*----------------------------------------------------*
025800931011     C*   SAVDAT: Salvataggio dati                         *
025900931011     C*----------------------------------------------------*
026000931011     C     SAVDAT        BEGSR
026100931011     C*
026200950714     C                   MOVE      CSBCDI        W8899             4 0
026300950705     C                   MOVEL     'N'           WCLGEN
026400950705     C     W8899         IFEQ      8888
026500950705     C     W8899         OREQ      9999
026600950705     C                   MOVEL     'S'           WCLGEN            1
026700950705     C                   END
026800950706     C                   MOVEL     DSSPE         SAVDS
026900950704     C                   MOVEL     'N'           WRTDET            1
027000950704     C                   MOVEL     CSBVCA        WSVVCA
027100950706     C                   MOVE      WDTSPE        WSVDSP            8 0
027200931011     C                   Z-ADD     0             IMPASS
027300950707     C                   Z-ADD     0             V1CSPE
027400931011     C*
027500931011     C                   ENDSR
027600931011     C*----------------------------------------------------*
027700931011     C*   WRIREC: Scrittura record                         *
027800931011     C*----------------------------------------------------*
027900931011     C     WRIREC        BEGSR
028000931011     C*
028100950704     C     WRTDET        IFEQ      'S'
028200950711     C                   Z-ADD     WSVAAS        V1CAAS
028300950711     C                   Z-ADD     WSVLNP        V1CLNP
028400950711     C                   Z-ADD     WSVNRS        V1CNRS
028500950711     C                   Z-ADD     WSVNSP        V1CNSP
028600950704     C                   MOVEL     *BLANKS       V1CSCE
028700950706     C                   Z-ADD     WSVDSP        G02INV
028800950704     C                   Z-ADD     0             G02DAT
028900950704     C                   MOVEL     '3'           G02ERR
029000950704     C                   CALL      'XSRDA8'
029100950704     C                   PARM                    WLBDA8
029200950706     C                   Z-ADD     G02DAT        V1CDSP
029300950706     C                   MOVEL     WSVVCA        V1CVAL
029400950706     C                   Z-ADD     WSVCDI        V1CCDI
029500950706     C                   Z-ADD     IMPASS        V1CIMP
029600950704     C*  Decodifico Mittente
029700950706     C     WCLGEN        IFEQ      'S'
029800950707     C                   Z-ADD     WSVAAS        KAAS
029900950707     C                   Z-ADD     WSVLNP        KLNP
030000950707     C                   Z-ADD     WSVNRS        KNRS
030100950707     C                   Z-ADD     WSVNSP        KNSP
030200950706     C                   MOVEL     *BLANKS       CSARSD
030300950706     C     KCSA          CHAIN     TNCSA01L                           31
030400971222     C                   MOVEL     CSARSD        V1CMIT
030500950706     C                   ELSE
030600950704     C                   MOVEL     *BLANKS       ACORAG
030700980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
030800980127     C                   CLEAR                   TIBS69
030900980127     C                   CLEAR                   CNACO
031000980127     C                   CLEAR                   CNIND
031100980127     C                   CLEAR                   CNCLP
031200980127     C                   CLEAR                   FNCLS
031300980127     C                   MOVEL     ' '           I69TLA
031400980127     C                   MOVEL     KNSIF         I69SIF
031500980127     C                   Z-ADD     WSVCDI        I69KAC
031600980127     C                   CALL      'TIBS69R'
031700980127     C                   PARM                    TIBS69
031800980127     C                   PARM                    CNACO
031900980127     C                   PARM                    CNIND
032000980127     C                   PARM                    CNCLP
032100980127     C                   PARM                    FNCLS
032200980127     C     O69ERR        IFEQ      *BLANK
032300950704     C                   MOVEL     ACORAG        V1CMIT
032400950706     C                   END
032500980127     C                   END
032600950706     c*
032700931011     C                   ADD       1             NRR
032800931011     C                   ADD       1             NRIG
032900950704     C                   WRITE     SC41S01
033000931013     C                   END
033100931011     C*
033200931011     C                   ENDSR
033300931011     C*----------------------------------------------------*
033400931011     C*  Routine gestione operazioni prima videata         *
033500931011     C*----------------------------------------------------*
033600931011     C     GESVD1        BEGSR
033700931011     C*-                                                   *
033800950707     C                   SETOFF                                       20
033900950704     C                   WRITE     SC41Z02
034000950704     C                   WRITE     SC41D02
034100950704     C                   EXFMT     SC41C01
034200931011     C*                                                    *
034300931011     C* Fine Programma                                     *
034400931011     C     *INKL         IFEQ      '1'
034500950704     C                   MOVEL     'S'           WFINE
034600931011     C                   END
034700931011     C*-                                                   *
034800931011     C                   ENDSR
034900931011     C*----------------------------------------------------*
035000931011     C*  Routine gestione operazioni subfile               *
035100931011     C*----------------------------------------------------*
035200931011     C     GESVD2        BEGSR
035300931011     C*-                                                   *
035400950704     C  N28              WRITE     SC41Z01
035500950704     C                   EXFMT     SC41C01
035600950704     C                   SETOFF                                       28
035700950706     C                   MOVEL     'N'           WINTBL            1
035800931011     C*                                                    *
035900931011     C* Fine Programma                                     *
036000931011     C     *INKC         IFEQ      '1'
036100950704     C                   MOVEL     'S'           WFINE
036200950704     C                   MOVE      3             D40CMD
036300931011     C                   GOTO      FINVD2
036400931011     C                   END
036500931011     C* Ritorno                                            *
036600931011     C     *INKL         IFEQ      '1'
036700950704     C                   MOVEL     'S'           WFINE
036800950704     C                   MOVE      12            D40CMD
036900931011     C                   GOTO      FINVD2
037000931011     C                   END
037100931011     C* ROLLUP
037200950704     C     *IN25         IFEQ      '1'
037300931013     C                   EXSR      ROLLUP
037400950711     C                   GOTO      FINVD2
037500931013     C                   END
037600950706     C*
037700990729     C                   CLEAR                   TNSB50
037800950706     C                   EXSR      CNTVD2
037900931011     C* Interrogazione Bolle                               *
038000931011     C     *INKG         IFEQ      '1'
038100950706     C     WINTBL        ANDEQ     'N'
038200990729     C                   CLEAR                   TNSB50
038300990729     C                   MOVE      'C00'         I50OP0
038400990729     C                   MOVEL     '0'           O50F03
038500990729     C                   MOVEL     '0'           O50F12
038600990729     C                   MOVEL     '0'           O50ERR
038700990729     C                   MOVEL     *BLANKS       O50MSG
038800990729     C                   MOVEL     KPJBU         WSVKPJ          256
038900990729     C                   MOVEL     *BLANKS       KPJBU
039000990729     C                   MOVEL     TNSB50        KPJBU
039100990729     C                   CALL      'TNSB50R'
039200990729     C                   PARM                    KPJBA
039300990729     C                   MOVEL     KPJBU         TNSB50
039400990729     C                   MOVEL     WSVKPJ        KPJBU
039500950706     C                   END
039600931011     C*                                                    *
039700931011     C*                                                    *
039800931012     C     FINVD2        ENDSR
039900931011     C*----------------------------------------------------*
040000931011     C*   CNTVD2: Controlli subfile                        *
040100931011     C*----------------------------------------------------*
040200931011     C     CNTVD2        BEGSR
040300931011     C*                                                    *
040400931011     C                   Z-ADD     1             NRR
040500950704     C                   MOVEL     'N'           WSELEZ            1
040600950704     C                   READC     SC41S01                                31
040700931011     C*                                                    *
040800950704     C     *IN31         DOWEQ     '0'
040900931011     C*                                                    *
041000950704     C     V1CSCE        IFEQ      '1'
041100950706     C                   Z-ADD     D40SDA        WSVSDA            8 0
041200950704     C                   MOVEL     'S'           WSELEZ            1
041300931011     C                   MOVEL     *BLANKS       KPJBU
041400950704     C                   Z-ADD     NRR           WSVREC            4 0
041500950706     C                   Z-ADD     V1CDSP        G02DAT
041600941102     C                   Z-ADD     0             G02INV
041700941102     C                   MOVEL     ' '           G02ERR
041800941102     C                   CALL      'XSRDA8'
041900941102     C                   PARM                    WLBDA8
042000950706     C                   Z-ADD     G02INV        D40SDD
042100950706     C                   Z-ADD     G02INV        D40SDA
042200950706     C                   Z-ADD     G02DAT        V1CDSP
042300950706     C                   MOVEL     V1CCDI        D40SCD
042400950704     C                   MOVEL     TNSC40        KPJBU
042500950706     C* Interrogazione bolle                               *
042600950706     C     *INKG         IFEQ      '1'
042700990729     C                   MOVEL     'S'           WINTBL
042800990729     C                   CLEAR                   TNSB50
042900990729     C                   MOVE      'C00'         I50OP0
043000990729     C                   MOVEL     '0'           O50F03
043100990729     C                   MOVEL     '0'           O50F12
043200990729     C                   MOVEL     '0'           O50ERR
043300990729     C                   MOVEL     *BLANKS       O50MSG
043400990729     C                   MOVEL     KPJBU         WSVKPJ          256
043500990729     C                   MOVEL     *BLANKS       KPJBU
043600990729     C                   MOVEL     TNSB50        KPJBU
043700990729     C                   CALL      'TNSB50R'
043800990729     C                   PARM                    KPJBA
043900990729     C                   MOVEL     KPJBU         TNSB50
044000990729     C                   MOVEL     WSVKPJ        KPJBU
044100010223     C                   ELSE
044200931012     C* Stampa                                             *
044300931012     C     *INKJ         IFEQ      '1'
044400950707     C                   CALL      'TNSC42R3'
044500931012     C                   PARM                    KPJBA
044600931012     C                   ELSE
044700950706     C* Interrogazione                                     *
044800950707     C     V1CSPE        IFEQ      1
044802120612     c                   eval      tnsc40ds = tnsc40
044900120612     C                   Z-ADD     V1CAAS        £D40AAS
045000120612     C                   Z-ADD     V1CLNP        £D40LNP
045100120612     C                   Z-ADD     V1CNRS        £D40NRS
045200120612     C                   Z-ADD     V1CNSP        £D40NSP
045201120612     C                   clear                   £D40ABI
045202120612     C                   clear                   £D40CAB
045203120612     C                   clear                   £D40NAS
045300120612     C                   MOVEL     TNSC40ds      KPJBU
045400950707     C                   CALL      'TNSC43R'
045500950707     C                   PARM                    KPJBA
045600950707     C                   ELSE
045700950707     C                   CALL      'TNSC41R3'
045800950706     C                   PARM                    KPJBA
045900931012     C                   END
046000950707     C                   END
046100950706     C                   MOVEL     KPJBU         TNSC40
046200950706     C     D40CMD        IFEQ      3
046300950706     C                   MOVEL     'S'           WFINE
046400950706     C                   END
046500950706     C*
046600950706     C                   END
046700931014     C*
046800931011     C*
046900950706     C                   Z-ADD     WSVDSP        D40SDA
047000950704     C                   Z-ADD     WSVREC        NRR
047100950704     C                   Z-ADD     WSVREC        DSPNR1
047200950704     C     NRR           CHAIN     SC41S01                            90
047300950704     C                   SETON                                        22
047400950704     C                   MOVEL     ' '           V1CSCE
047500950704     C  N90              UPDATE    SC41S01
047600931011     C                   END
047700931011     C*
047800950704     C                   READC     SC41S01                                31
047900931011     C                   END
048000931011     C*
048100950704     C     WSELEZ        IFNE      'S'
048200950704     C     *INKJ         ANDEQ     '1'
048300950704     C                   SETON                                        28
048400950704     C                   MOVEL     ERR(1)        $MSG
048500950704     C                   END
048600931012     C*
048700931011     C                   ENDSR
048800931011      *---------------------------------------------------------------*
048900941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
049000931011      *---------------------------------------------------------------*
049100941102     C     *INZSR        BEGSR
049200931011      *---------------------------------------------------------------*
049300931011     C     *ENTRY        PLIST
049400931011     C                   PARM                    KPJBA
049500950704     C                   MOVEL     KPJBU         TNSC40
049600931012     C*
049700931011     C                   Z-ADD     1             CODUT
049800931011     C                   CALL      'X§PARUT'
049900931011     C                   PARM                    UT§DSE
050000931011     C                   MOVEL     RAGUT         RSUT             20
050100931011     C                   MOVEL     REC80         CNCR80
050200931011     C                   MOVEL     KNMUS         §SEI              6
050300950704     C*----------------------------------------------------*
050400950704     C*  Definizione CHIAVI
050500950704     C     KCSB          KLIST
050600950705     C                   KFLD                    KAAS
050700950705     C                   KFLD                    KMGS
050800950706     C     KCSA          KLIST
050900950706     C                   KFLD                    KAAS
051000950706     C                   KFLD                    KLNP
051100950706     C                   KFLD                    KNRS
051200950706     C                   KFLD                    KNSP
051300931012     C*----------------------------------------------------*
051400950704     C*  Definizione variabili
051500950704     C     *LIKE         DEFINE    CSBCAS        IMPASS          + 1
051600950705     C     *LIKE         DEFINE    CSBAAS        KAAS
051700950705     C     *LIKE         DEFINE    CSBMGS        KMGS
051800950706     C     *LIKE         DEFINE    CSBLNP        KLNP
051900950706     C     *LIKE         DEFINE    CSBNRS        KNRS
052000950706     C     *LIKE         DEFINE    CSBNSP        KNSP
052100950706     C     *LIKE         DEFINE    CSBVCA        WSVVCA
052200950707     C     *LIKE         DEFINE    CSBLNP        WSVLNP
052300950707     C     *LIKE         DEFINE    CSBNRS        WSVNRS
052400950707     C     *LIKE         DEFINE    CSBNSP        WSVNSP
052500950704     C*----------------------------------------------------*
052600950704     C                   MOVEL     'N'           WFINE             1
052700950705     C                   MOVEL     D40SDD        KAAS
052800950705     C                   MOVE      D40SDD        KMGS
052900010223     C*F                   CLEARTRUL15
053000931012     C*----------------------------------------------------*
053100931011     C                   ENDSR
053200950705**
053300950705Selezionare le spedizioni da stampare
