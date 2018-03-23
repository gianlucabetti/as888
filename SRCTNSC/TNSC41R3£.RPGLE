000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950706      * TRAS41R2                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950706      *          SELEZIONE RIF.MITTENTE/DATA SPEDIZIONE              *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931011      * KC -FINE LAVORO                                              *
001100931011      * KG -INTERROGAZIONE BOLLE                                     *
001200931011      * KL -RITORNO                                                  *
001300931011?     *--------------------------------------------------------------*
001400950706     FTNCSA01L  IF   E           K DISK
001500950707     FTNCSB05L  IF   E           K DISK
001600120210     FTNCSm03L  IF   E           K DISK
001700950706     FTNSC41D   CF   E             WORKSTN
001800950706     F                                     SFILE(SC41S01:NRR)
001900931011?     *--------------------------------------------------------------*
002000931011?     *  DS                                                          *
002100931011?     *--------------------------------------------------------------*
002200931011     D KPJBA         E DS
002300931011      *
002400950706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002500950706     D CNCR80        E DS
002600950706     D*
002700941102     D WLBDA8          DS
002800941102     D  G02DAT                 1      8  0
002900941102     D  G02INV                 9     16  0
003000941102     D  G02ERR                17     17
003100941102     D  G02TGI                18     22  0
003200980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003300980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003400980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003500980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003600980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003700120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
003701120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
003800990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
003900931011      *
004000120210     d mnra            s                   like(csmnra)
004100120210     d mabi            s                   like(csmabi)
004200120210     d mcab            s                   like(csmcab)
004300931011      *
004400931011     D                SDS
004500931011     D  NOMPG                  1     10
004600931011      *---------------------------------------------------------------*
004700931011      *   C I C L O      P R I N C I P A L E                          *
004800931011      *---------------------------------------------------------------*
004900931011     C*  CARICAMENTO SUBFILE
005000931011     C                   EXSR      CARSFL
005100931011     C*
005200931011     C*  LOOP
005300950706     C     WFINE         DOUEQ     'S'
005400931011     C*
005500931011     C     TIPVID        CASEQ     '1'           GESVD1
005600931011     C     TIPVID        CASEQ     '2'           GESVD2
005700931011     C                   END
005800931011     C*
005900931011     C                   END
006000931011     C*
006100950706     C                   MOVEL     TNSC40        KPJBU
006200971222     C* RICHIAMO PGM X CHIUSURA IN LR
006300980127     C                   CLEAR                   TIBS69
006400980127     C                   MOVEL     'C'           I69TLA
006500980127     C                   CALL      'TIBS69R'
006600980127     C                   PARM                    TIBS69
006700980127     C                   PARM                    CNACO
006800980127     C                   PARM                    CNIND
006900980127     C                   PARM                    CNCLP
007000980127     C                   PARM                    FNCLS
007100971222     C*
007200931011     C                   SETON                                        LR
007300931011      *---------------------------------------------------------------*
007400931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
007500931011      *---------------------------------------------------------------*
007600931011     C     CARSFL        BEGSR
007700931011     C*
007800931011     C*  Pulisco subfile
007900950707     C                   SETOFF                                       2021
008000950706     C                   WRITE     SC41C01
008100950707     C                   SETON                                        21
008200950706     C*  Imposto variabili testata
008300950706     C                   CLEAR                   WLBDA8
008400950707     C                   Z-ADD     D40SCD        V1CKCS
008500950707     C                   Z-ADD     D40SDD        G02INV
008600950706     C                   MOVEL     '3'           G02ERR
008700950706     C                   CALL      'XSRDA8'
008800950706     C                   PARM                    WLBDA8
008900950707     C                   Z-ADD     G02DAT        V1CSDD
009000950707     C                   Z-ADD     D40SDA        G02INV
009100950706     C                   MOVEL     '3'           G02ERR
009200950706     C                   CALL      'XSRDA8'
009300950706     C                   PARM                    WLBDA8
009400950707     C                   Z-ADD     G02DAT        V1CSDA
009500931011     C*
009600931011     C*  Caricamento subfile
009700950707     C     KCSB2         SETLL     TNCSB05L
009800950707     C                   READ      TNCSB05L                               23
009900950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
010000950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
010100931011     C*
010200931011     C                   Z-ADD     0             NRR               4 0
010300931013     C                   MOVEL     '1'           TIPVID            1
010400931011     C*
010500950706     C     *IN23         IFEQ      '0'
010600931011     C                   EXSR      ROLLUP
010700931011     C                   END
010800931011     C*
010900931013     C     NRR           IFNE      0
011000931013     C                   MOVEL     '2'           TIPVID            1
011100931013     C                   END
011200931013     C*
011300931012     C                   MOVEL     NOMPG         NOMPGM
011400931011     C*
011500931011     C                   ENDSR
011600931011     C*----------------------------------------------------*
011700931011     C*   ROLLUP: Caricamento di una pagina                *
011800931011     C*----------------------------------------------------*
011900931011     C     ROLLUP        BEGSR
012000931011     C*
012100950706     C     *IN23         IFEQ      '0'
012200931014     C                   Z-ADD     SAVRE1        NRR
012300931011     C                   Z-ADD     0             NRIG              4 0
012400931011     C*
012500950707     C     NRIG          DOWLT     16
012600950706     C     *IN23         ANDEQ     '0'
012700950707     C     WDTSPE        ANDLE     D40SDA
012800950707     C                   EXSR      REDCLI
012900931011     C                   END
013000931011     C*
013100931014     C     NRR           IFGT      SAVRE1
013200931014     C     SAVRE1        ADD       1             DSPNR1
013300931014     C                   Z-ADD     NRR           SAVRE1            4 0
013400931014     C                   END
013500931014     C*
013600931011     C                   END
013700950707     C*
013800950707     C  N23WDTSPE        COMP      D40SDA                             23
013900931011     C*
014000931011     C                   ENDSR
014100950706     C*----------------------------------------------------*
014200950707     C*   REDCLI: Lettura con selez. per CLIENTE           *
014300950706     C*----------------------------------------------------*
014400950707     C     REDCLI        BEGSR
014500950706     C*
014600950707     C     CSBCDI        IFLT      D40SCD
014700950707     C     KCSB2         SETLL     TNCSB05L
014800950707     C                   READ      TNCSB05L                               23
014900950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
015000950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
015100950707     C                   Z-ADD     CSBMGS        KMGS
015200950707     C                   Z-ADD     CSBAAS        KAAS
015300950706     C                   END
015400950706     C*  Leggo lnp.
015500950706     C     *IN23         DOWEQ     '0'
015600950707     C     CSBCDI        ANDEQ     D40SCD
015700950707     C     WDTSPE        ANDLE     D40SDA
015800950707     C     NRIG          ANDLT     16
015900950706     C                   EXSR      GESREC
016000950706     C                   END
016100950706     C*
016200950711     C                   Z-ADD     CSBAAS        KAAS
016300950711     C                   Z-ADD     CSBMGS        KMGS
016400950707     C     CSBCDI        IFGT      D40SCD
016500950707     C     KCSB1         SETGT     TNCSB05L
016600950707     C                   READ      TNCSB05L                               23
016700950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
016800950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
016900950707     C                   Z-ADD     CSBMGS        KMGS
017000950707     C                   Z-ADD     CSBAAS        KAAS
017100950707     C                   END
017200950706     C*
017300950706     C                   ENDSR
017400120210     C*----------------------------------------------------*
017500120210     C*   srmulti - per multiassegni
017600120210     C*----------------------------------------------------*
017700120210     C     srmulti       BEGSR
017800120210     C*
017900120210     c                   eval      mnra = *hival
018000120210     c                   eval      mabi = *hival
018100120210     c                   eval      mcab = *hival
018200120210     c     csbnra        setll     tncsm03l
018300120210     c                   do        *hival
018400120210     c     csbnra        reade     tncsm03l
018500120210     c                   if        %eof(tncsm03l)
018600120210     c                   leave
018700120210     c                   end
018800120210     C                   if        csmnra = WNRA
018900120210     c                   eval      mnra = csmnra
019000120210     c                   eval      mabi = csmabi
019100120210     c                   eval      mcab = csmcab
019200120210     c                   leave
019300120210     c                   end
019400120210     c                   enddo
019500120210     C*
019600120210     C                   ENDSR
019700950706     C*----------------------------------------------------*
019800950706     C*   GESREC: Caricamento di una pagina                *
019900950706     C*----------------------------------------------------*
020000950706     C     GESREC        BEGSR
020100950706     C*----------------------------------------------------*
020200950706     C* Se il documento è valido controllo se il record rientra
020300950706     C* nei limiti richiesti
020400950706     C                   MOVEL     D40NAS        WNRA             10
020500120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
020600120220     c                             csbtpi = 'M' and csbabi = 0
020700120210     c                   exsr      srmulti
020800120210     c                   else
020900120210     c                   eval      mnra = csbnra
021000120210     c                   eval      mabi = csbabi
021100120210     c                   eval      mcab = csbcab
021200120210     c                   end
021300950706     C*----------------------------------------------------*
021400950706     C     D40ABI        IFEQ      *ZEROS
021500120210     C     D40ABI        OREQ      mABI
021600950720     C     D40CAB        IFEQ      *ZEROS
021700120210     C     D40CAB        OREQ      mCAB
021701140325     C     D40imp        IFEQ      *ZEROS
021702140325     C     D40imp        OREQ      csbcas
021800950720     C     WNRA          IFEQ      *BLANKS
021900950720     C     WNRA          OREQ      *ZEROS
022000120210     C     WNRA          OREQ      mNRA
022100950706     C*----------------------------------------------------*
022200950706     C* Selezione per stato C/Assegno
022300950706     C     D40TCR        IFEQ      ' '
022400950706     C     D40TCR        OREQ      'D'
022500950706     C     CSBDDC        ANDEQ     0
022600950706     C     D40TCR        OREQ      'G'
022700950706     C     CSBSTA        ANDEQ     2
022800950706     C     D40TCR        OREQ      'A'
022900950706     C     CSBSTA        ANDEQ     9
023000950706     C     D40TCR        OREQ      'S'
023100950706     C     CSBSTA        ANDEQ     5
023200010112     C     D40TCR        OREQ      '3'
023300010112     C     CSBSTA        ANDEQ     3
023400960909     C     D40TCR        OREQ      'T'
023500960909     C     CSBSTA        ANDEQ     6
023600960909     C     D40TCR        OREQ      'R'
023700960909     C     CSBSTA        ANDEQ     7
023800960909     C     D40TCR        OREQ      'O'
023900960909     C     CSBSTA        ANDEQ     8
024000950706     C     D40TCR        OREQ      'I'
024100950706     C     CSBDDC        ANDGT     0
024200950706     C     CSBDDP        ANDEQ     0
024300950706     C     D40TCR        OREQ      'P'
024400950706     C     CSBDDP        ANDGT     0
024500950706     C                   MOVEL     *BLANKS       V1CSCE
024600950706     C                   MOVEL     CSBAAS        V1CAAS
024700950706     C                   MOVEL     CSBMGS        MM                2 0
024800950706     C                   MOVE      CSBMGS        GG                2 0
024900950706     C                   MOVEL     GG            V1CDSP
025000950706     C                   MOVE      MM            V1CDSP
025100950706     C                   MOVE      CSBLNP        V1CLNP
025200950706     C                   MOVE      CSBNRS        V1CNRS
025300950706     C                   MOVE      CSBNSP        V1CNSP
025400950712     C                   MOVE      CSBTBL        V1CTBL
025500950706     C                   MOVEL     CSBRSD        V1CDES
025600950706     C                   MOVE      CSBRMN        V1CRMN
025700950706     C                   MOVE      CSBVCA        V1CVCA
025800950706     C                   Z-ADD     CSBCAS        V1CIMP
025900971222     C* Se lnp no del mio ramo imposto dati cliente infragr. (mitt.)
026000950714     C                   MOVE      CSBCDI        W8899             4 0
026100950706     C     W8899         IFEQ      8888
026200950706     C     W8899         OREQ      9999
026300950706     C                   Z-ADD     CSBAAS        KAAS
026400950706     C                   Z-ADD     CSBLNP        KLNP
026500950706     C                   Z-ADD     CSBNRS        KNRS
026600950706     C                   Z-ADD     CSBNSP        KNSP
026700950706     C                   MOVEL     *BLANKS       CSARSD
026800950706     C     KCSA          CHAIN     TNCSA01L                           31
026900950706     C                   MOVEL     CSARSD        V1CMIT
027000950706     C                   ELSE
027100950706     C                   MOVEL     *BLANKS       ACORAG
027200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
027300980127     C                   CLEAR                   TIBS69
027400980127     C                   CLEAR                   CNACO
027500980127     C                   CLEAR                   CNIND
027600980127     C                   CLEAR                   CNCLP
027700980127     C                   CLEAR                   FNCLS
027800980127     C                   MOVEL     ' '           I69TLA
027900980127     C                   MOVEL     KNSIF         I69SIF
028000980127     C                   Z-ADD     CSBCDI        I69KAC
028100980127     C                   CALL      'TIBS69R'
028200980127     C                   PARM                    TIBS69
028300980127     C                   PARM                    CNACO
028400980127     C                   PARM                    CNIND
028500980127     C                   PARM                    CNCLP
028600980127     C                   PARM                    FNCLS
028700980127     C     O69ERR        IFEQ      *BLANK
028800950706     C                   MOVEL     ACORAG        V1CMIT
028900980127     C                   END
029000971222     C                   END
029100950706     C                   ADD       1             NRR
029200950706     C                   ADD       1             NRIG
029300950706     C                   WRITE     SC41S01
029400950706     C                   END
029500950720     C                   END
029600950706     C                   END
029700971222     C                   END
029701140325     C                   END
029800950706     C*
029900950707     C                   READ      TNCSB05L                               23
030000950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
030100950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
030200950706     C*
030300950706     C                   ENDSR
030400931011     C*----------------------------------------------------*
030500931011     C*  Routine gestione operazioni prima videata         *
030600931011     C*----------------------------------------------------*
030700931011     C     GESVD1        BEGSR
030800931011     C*-                                                   *
030900950707     C                   SETOFF                                       20
031000950706     C                   WRITE     SC41Z02
031100950706     C                   WRITE     SC41D02
031200950706     C                   EXFMT     SC41C01
031300931011     C*                                                    *
031400931011     C* Fine Programma                                     *
031500931011     C     *INKL         IFEQ      '1'
031600950706     C                   MOVEL     'S'           WFINE
031700931011     C                   END
031800931011     C*-                                                   *
031900931011     C                   ENDSR
032000931011     C*----------------------------------------------------*
032100931011     C*  Routine gestione operazioni subfile               *
032200931011     C*----------------------------------------------------*
032300931011     C     GESVD2        BEGSR
032400931011     C*-                                                   *
032500950707     C                   SETON                                        20
032600950706     C                   WRITE     SC41Z01
032700950706     C                   WRITE     SC41Z01
032800950706     C                   EXFMT     SC41C01
032900950706     C                   MOVEL     'N'           WINTBL            1
033000931011     C*                                                    *
033100931011     C* Fine Programma                                     *
033200931011     C     *INKC         IFEQ      '1'
033300950706     C                   MOVEL     'S'           WFINE
033400950706     C                   MOVE      3             D40CMD
033500931011     C                   GOTO      FINVD2
033600931011     C                   END
033700931011     C* Ritorno                                            *
033800931011     C     *INKL         IFEQ      '1'
033900950706     C                   MOVEL     'S'           WFINE
034000950706     C                   MOVE      12            D40CMD
034100931011     C                   GOTO      FINVD2
034200931011     C                   END
034300950711     C* ROLLUP
034400950711     C     *IN25         IFEQ      '1'
034500950711     C                   EXSR      ROLLUP
034600950711     C                   GOTO      FINVD2
034700950711     C                   END
034800950706     C*                                                    *
034900990729     C                   CLEAR                   TNSB50
035000950706     C                   EXSR      CNTVD2
035100931011     C* Interrogazione Bolle                               *
035200931011     C     *INKG         IFEQ      '1'
035300950706     C     WINTBL        ANDEQ     'N'
035400950706     C                   EXSR      GESINT
035500931011     C                   END
035600931011     C*                                                    *
035700931012     C     FINVD2        ENDSR
035800950706     C*----------------------------------------------------*
035900950706     C*   GESINT: gestione interrogazione bolle            *
036000950706     C*----------------------------------------------------*
036100950706     C     GESINT        BEGSR
036200950706     C*                                                    *
036300990729     C                   CLEAR                   TNSB50
036400990729     C                   MOVEL     'C05'         I50OP0
036500990729     C                   MOVEL     '0'           O50F03
036600990729     C                   MOVEL     '0'           O50F12
036700990729     C                   MOVEL     '0'           O50ERR
036800990729     C                   MOVEL     *BLANKS       O50MSG
036900990729     C                   MOVEL     KPJBU         WSVKPJ          256
037000990729     C                   MOVEL     *BLANKS       KPJBU
037100990729     C     WACC          IFEQ      '1'
037200990729     C                   Z-ADD     V1CAAS        D50AAS
037300990729     C                   Z-ADD     V1CLNP        D50LNP
037400990729     C                   Z-ADD     V1CNRS        D50NRS
037500990729     C                   Z-ADD     V1CNSP        D50NSP
037600990729     C                   MOVEL     V1CTBL        D50TBL
037700990729     C                   MOVEL     'S'           WINTBL
037800990729     C                   MOVEL     TNSB50        KPJBU
037900990729     C                   CALL      'TNSB50R'
038000990729     C                   PARM                    KPJBA
038100990910     C                   MOVEL     KPJBU         TNSB50
038200990729     C     O50ERR        IFEQ      *BLANKS
038300990729     C                   GOTO      FININT
038400990729     C                   END
038500990729     C* Se ho richiamato il pgm con il numero spedizione/
038600990729     C* codice bolla prova a richiamare senza codice bolla
038700990729     C* come del resto faccio se non ho recuperato il C/Ass.
038800990729     C                   MOVEL     'C00'         I50OP0
038900990729     C                   MOVEL     V1CTBL        D50TBL
039000990729     C                   MOVEL     TNSB50        KPJBU
039100990729     C                   CALL      'TNSB50R'
039200990729     C                   PARM                    KPJBA
039300990729     C                   ELSE
039400990729     C                   MOVEL     TNSB50        KPJBU
039500990729     C                   CALL      'TNSB50R'
039600990729     C                   PARM                    KPJBA
039700990729     C                   END
039800990729     C                   MOVEL     KPJBU         TNSB50
039900990729     C                   MOVEL     WSVKPJ        KPJBU
040000950706     C*
040100990729     C     FININT        ENDSR
040200931011     C*----------------------------------------------------*
040300931011     C*   CNTVD2: Controlli subfile                        *
040400931011     C*----------------------------------------------------*
040500931011     C     CNTVD2        BEGSR
040600931011     C*                                                    *
040700931011     C                   Z-ADD     1             NRR
040800950706     C                   MOVEL     ' '           V1CSCE
040900950706     C                   READC     SC41S01                                82
041000931011     C*                                                    *
041100931011     C     *IN82         DOWEQ     '0'
041200931011     C*                                                    *
041300950706     C     V1CSCE        IFEQ      '1'
041400931011     C                   MOVEL     *BLANKS       KPJBU
041500931012     C                   Z-ADD     NRR           SAVREC            4 0
041600950706     C*  Interrogazione bolla
041700950706     C     *INKG         IFEQ      '1'
041800990729     C                   MOVEL     '1'           WACC              1
041900950706     C                   EXSR      GESINT
042000950706     C                   ELSE
042002120612     c                   eval      tnsc40ds  = tnsc40
042100120612     C                   MOVE      V1CAAS        £D40AAS
042200120612     C                   MOVE      V1CLNP        £D40LNP
042300120612     C                   MOVE      V1CNRS        £D40NRS
042400120612     C                   MOVE      V1CNSP        £D40NSP
042401120612     C                   clear                   £D40ABI
042402120612     C                   clear                   £D40CAB
042403120612     C                   clear                   £D40NAS
042500120612     C                   MOVEL     TNSC40ds      KPJBU
042600931012     C* Interrogazione                                     *
042700950706     C                   CALL      'TNSC43R'
042800931012     C                   PARM                    KPJBA
042900950706     C                   END
043000931011     C*
043100931012     C                   Z-ADD     SAVREC        NRR
043200931014     C                   Z-ADD     SAVREC        DSPNR1
043300950706     C     NRR           CHAIN     SC41S01                            90
043400931011     C                   SETON                                        70
043500950706     C                   MOVEL     ' '           V1CSCE
043600950706     C  N90              UPDATE    SC41S01
043700931011     C                   END
043800931011     C*
043900950706     C                   READC     SC41S01                                82
044000931011     C                   END
044100931012     C*
044200931011     C                   ENDSR
044300931011      *---------------------------------------------------------------*
044400941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
044500931011      *---------------------------------------------------------------*
044600941102     C     *INZSR        BEGSR
044700931011      *---------------------------------------------------------------*
044800931011     C     *ENTRY        PLIST
044900931011     C                   PARM                    KPJBA
045000950706     C                   MOVEL     KPJBU         TNSC40
045100931012     C*
045200931011     C                   Z-ADD     1             CODUT
045300931011     C                   CALL      'X§PARUT'
045400931011     C                   PARM                    UT§DSE
045500931011     C*          CODUT     CABEQ-1        FINE
045600931011     C                   MOVEL     RAGUT         RSUT             20
045700931011     C                   MOVEL     REC80         CNCR80
045800931011     C                   MOVEL     KNMUS         §SEI              6
045900950706     C*----------------------------------------------------*
046000950706     C*  Definizione CHIAVI
046100950706     C     KCSB1         KLIST
046200950706     C                   KFLD                    KAAS
046300950706     C                   KFLD                    KMGS
046400950706     C     KCSB2         KLIST
046500950706     C                   KFLD                    KAAS
046600950706     C                   KFLD                    KMGS
046700950707     C                   KFLD                    KCDI
046800950706     C     KCSA          KLIST
046900950706     C                   KFLD                    KAAS
047000950706     C                   KFLD                    KLNP
047100950706     C                   KFLD                    KNRS
047200950706     C                   KFLD                    KNSP
047300931012     C*----------------------------------------------------*
047400941102     C*  Definizione variabili
047500950707     C     *LIKE         DEFINE    CSBCDI        KCDI
047600950706     C     *LIKE         DEFINE    CSBAAS        KAAS
047700950706     C     *LIKE         DEFINE    CSBMGS        KMGS
047800950706     C     *LIKE         DEFINE    CSALNP        KLNP
047900950706     C     *LIKE         DEFINE    CSANRS        KNRS
048000950706     C     *LIKE         DEFINE    CSANSP        KNSP
048100941102     C*----------------------------------------------------*
048200950706     C*  Inizializzazione delle variabii
048300950706     C                   MOVEL     'N'           WFINE             1
048400950707     C                   Z-ADD     D40SCD        KCDI
048500950707     C                   Z-ADD     D40SDD        KMGS
048600950707     C                   MOVEL     D40SDD        KAAS
048700950707     C                   SETON                                        03
048800931012     C*----------------------------------------------------*
048900931011     C                   ENDSR
