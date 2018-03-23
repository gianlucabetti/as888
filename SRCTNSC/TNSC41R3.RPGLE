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
003800120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
003900990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004000931011      *
004100120210     d mnra            s                   like(csmnra)
004200120210     d mabi            s                   like(csmabi)
004300120210     d mcab            s                   like(csmcab)
004400931011      *
004500931011     D                SDS
004600931011     D  NOMPG                  1     10
004700931011      *---------------------------------------------------------------*
004800931011      *   C I C L O      P R I N C I P A L E                          *
004900931011      *---------------------------------------------------------------*
005000931011     C*  CARICAMENTO SUBFILE
005100931011     C                   EXSR      CARSFL
005200931011     C*
005300931011     C*  LOOP
005400950706     C     WFINE         DOUEQ     'S'
005500931011     C*
005600931011     C     TIPVID        CASEQ     '1'           GESVD1
005700931011     C     TIPVID        CASEQ     '2'           GESVD2
005800931011     C                   END
005900931011     C*
006000931011     C                   END
006100931011     C*
006200950706     C                   MOVEL     TNSC40        KPJBU
006300971222     C* RICHIAMO PGM X CHIUSURA IN LR
006400980127     C                   CLEAR                   TIBS69
006500980127     C                   MOVEL     'C'           I69TLA
006600980127     C                   CALL      'TIBS69R'
006700980127     C                   PARM                    TIBS69
006800980127     C                   PARM                    CNACO
006900980127     C                   PARM                    CNIND
007000980127     C                   PARM                    CNCLP
007100980127     C                   PARM                    FNCLS
007200971222     C*
007300931011     C                   SETON                                        LR
007400931011      *---------------------------------------------------------------*
007500931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
007600931011      *---------------------------------------------------------------*
007700931011     C     CARSFL        BEGSR
007800931011     C*
007900931011     C*  Pulisco subfile
008000950707     C                   SETOFF                                       2021
008100950706     C                   WRITE     SC41C01
008200950707     C                   SETON                                        21
008300950706     C*  Imposto variabili testata
008400950706     C                   CLEAR                   WLBDA8
008500950707     C                   Z-ADD     D40SCD        V1CKCS
008600950707     C                   Z-ADD     D40SDD        G02INV
008700950706     C                   MOVEL     '3'           G02ERR
008800950706     C                   CALL      'XSRDA8'
008900950706     C                   PARM                    WLBDA8
009000950707     C                   Z-ADD     G02DAT        V1CSDD
009100950707     C                   Z-ADD     D40SDA        G02INV
009200950706     C                   MOVEL     '3'           G02ERR
009300950706     C                   CALL      'XSRDA8'
009400950706     C                   PARM                    WLBDA8
009500950707     C                   Z-ADD     G02DAT        V1CSDA
009600160415     C                   MOVEL     D40PTN        V1CINT
009700931011     C*
009800931011     C*  Caricamento subfile
009900950707     C     KCSB2         SETLL     TNCSB05L
010000950707     C                   READ      TNCSB05L                               23
010100950706     C                   Z-ADD     CSBMGS        WDTSPE            8 0
010200950706     C                   MOVEL     CSBAAS        WDTSPE            8 0
010300931011     C*
010400931011     C                   Z-ADD     0             NRR               4 0
010500931013     C                   MOVEL     '1'           TIPVID            1
010600931011     C*
010700950706     C     *IN23         IFEQ      '0'
010800931011     C                   EXSR      ROLLUP
010900931011     C                   END
011000931011     C*
011100931013     C     NRR           IFNE      0
011200931013     C                   MOVEL     '2'           TIPVID            1
011300931013     C                   END
011400931013     C*
011500931012     C                   MOVEL     NOMPG         NOMPGM
011600931011     C*
011700931011     C                   ENDSR
011800931011     C*----------------------------------------------------*
011900931011     C*   ROLLUP: Caricamento di una pagina                *
012000931011     C*----------------------------------------------------*
012100931011     C     ROLLUP        BEGSR
012200931011     C*
012300950706     C     *IN23         IFEQ      '0'
012400931014     C                   Z-ADD     SAVRE1        NRR
012500931011     C                   Z-ADD     0             NRIG              4 0
012600931011     C*
012700950707     C     NRIG          DOWLT     16
012800950706     C     *IN23         ANDEQ     '0'
012900950707     C     WDTSPE        ANDLE     D40SDA
013000950707     C                   EXSR      REDCLI
013100931011     C                   END
013200931011     C*
013300931014     C     NRR           IFGT      SAVRE1
013400931014     C     SAVRE1        ADD       1             DSPNR1
013500931014     C                   Z-ADD     NRR           SAVRE1            4 0
013600931014     C                   END
013700931014     C*
013800931011     C                   END
013900950707     C*
014000950707     C  N23WDTSPE        COMP      D40SDA                             23
014100931011     C*
014200931011     C                   ENDSR
014300950706     C*----------------------------------------------------*
014400950707     C*   REDCLI: Lettura con selez. per CLIENTE           *
014500950706     C*----------------------------------------------------*
014600950707     C     REDCLI        BEGSR
014700950706     C*
014800950707     C     CSBCDI        IFLT      D40SCD
014900950707     C     KCSB2         SETLL     TNCSB05L
015000950707     C                   READ      TNCSB05L                               23
015100950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
015200950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
015300950707     C                   Z-ADD     CSBMGS        KMGS
015400950707     C                   Z-ADD     CSBAAS        KAAS
015500950706     C                   END
015600950706     C*  Leggo lnp.
015700950706     C     *IN23         DOWEQ     '0'
015800950707     C     CSBCDI        ANDEQ     D40SCD
015900950707     C     WDTSPE        ANDLE     D40SDA
016000950707     C     NRIG          ANDLT     16
016100950706     C                   EXSR      GESREC
016200950706     C                   END
016300950706     C*
016400950711     C                   Z-ADD     CSBAAS        KAAS
016500950711     C                   Z-ADD     CSBMGS        KMGS
016600950707     C     CSBCDI        IFGT      D40SCD
016700950707     C     KCSB1         SETGT     TNCSB05L
016800950707     C                   READ      TNCSB05L                               23
016900950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
017000950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
017100950707     C                   Z-ADD     CSBMGS        KMGS
017200950707     C                   Z-ADD     CSBAAS        KAAS
017300950707     C                   END
017400950706     C*
017500950706     C                   ENDSR
017600120210     C*----------------------------------------------------*
017700120210     C*   srmulti - per multiassegni
017800120210     C*----------------------------------------------------*
017900120210     C     srmulti       BEGSR
018000120210     C*
018100120210     c                   eval      mnra = *hival
018200120210     c                   eval      mabi = *hival
018300120210     c                   eval      mcab = *hival
018400120210     c     csbnra        setll     tncsm03l
018500120210     c                   do        *hival
018600120210     c     csbnra        reade     tncsm03l
018700120210     c                   if        %eof(tncsm03l)
018800120210     c                   leave
018900120210     c                   end
019000120210     C                   if        csmnra = WNRA
019100120210     c                   eval      mnra = csmnra
019200120210     c                   eval      mabi = csmabi
019300120210     c                   eval      mcab = csmcab
019400120210     c                   leave
019500120210     c                   end
019600120210     c                   enddo
019700120210     C*
019800120210     C                   ENDSR
019900950706     C*----------------------------------------------------*
020000950706     C*   GESREC: Caricamento di una pagina                *
020100950706     C*----------------------------------------------------*
020200950706     C     GESREC        BEGSR
020300950706     C*----------------------------------------------------*
020400950706     C* Se il documento è valido controllo se il record rientra
020500950706     C* nei limiti richiesti
020600950706     C                   MOVEL     D40NAS        WNRA             10
020700120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
020800120220     c                             csbtpi = 'M' and csbabi = 0
020900120210     c                   exsr      srmulti
021000120210     c                   else
021100120210     c                   eval      mnra = csbnra
021200120210     c                   eval      mabi = csbabi
021300120210     c                   eval      mcab = csbcab
021400120210     c                   end
021500950706     C*----------------------------------------------------*
021600950706     C     D40ABI        IFEQ      *ZEROS
021700120210     C     D40ABI        OREQ      mABI
021800950720     C     D40CAB        IFEQ      *ZEROS
021900120210     C     D40CAB        OREQ      mCAB
022000140325     C     D40imp        IFEQ      *ZEROS
022100140325     C     D40imp        OREQ      csbcas
022200950720     C     WNRA          IFEQ      *BLANKS
022300950720     C     WNRA          OREQ      *ZEROS
022400120210     C     WNRA          OREQ      mNRA
022500160415     C*  Test  x tipo intestazione
022600160415     C     D40PTN        IFEQ      *BLANKS
022700160415     C     D40PTN        OREQ      'M'
022800160415     C     CSBTPI        ANDEQ     'M'
022900160415     C     D40PTN        OREQ      'B'
023000160415     C     CSBTPI        ANDEQ     ' '
023100950706     C*----------------------------------------------------*
023200950706     C* Selezione per stato C/Assegno
023300950706     C     D40TCR        IFEQ      ' '
023400950706     C     D40TCR        OREQ      'D'
023500950706     C     CSBDDC        ANDEQ     0
023600950706     C     D40TCR        OREQ      'G'
023700950706     C     CSBSTA        ANDEQ     2
023800950706     C     D40TCR        OREQ      'A'
023900950706     C     CSBSTA        ANDEQ     9
024000950706     C     D40TCR        OREQ      'S'
024100950706     C     CSBSTA        ANDEQ     5
024200010112     C     D40TCR        OREQ      '3'
024300010112     C     CSBSTA        ANDEQ     3
024400960909     C     D40TCR        OREQ      'T'
024500960909     C     CSBSTA        ANDEQ     6
024600960909     C     D40TCR        OREQ      'R'
024700960909     C     CSBSTA        ANDEQ     7
024800960909     C     D40TCR        OREQ      'O'
024900960909     C     CSBSTA        ANDEQ     8
025000950706     C     D40TCR        OREQ      'I'
025100950706     C     CSBDDC        ANDGT     0
025200950706     C     CSBDDP        ANDEQ     0
025300950706     C     D40TCR        OREQ      'P'
025400950706     C     CSBDDP        ANDGT     0
025500950706     C                   MOVEL     *BLANKS       V1CSCE
025600950706     C                   MOVEL     CSBAAS        V1CAAS
025700950706     C                   MOVEL     CSBMGS        MM                2 0
025800950706     C                   MOVE      CSBMGS        GG                2 0
025900950706     C                   MOVEL     GG            V1CDSP
026000950706     C                   MOVE      MM            V1CDSP
026100950706     C                   MOVE      CSBLNP        V1CLNP
026200950706     C                   MOVE      CSBNRS        V1CNRS
026300950706     C                   MOVE      CSBNSP        V1CNSP
026400950712     C                   MOVE      CSBTBL        V1CTBL
026500950706     C                   MOVEL     CSBRSD        V1CDES
026600950706     C                   MOVE      CSBRMN        V1CRMN
026700950706     C                   MOVE      CSBVCA        V1CVCA
026800950706     C                   Z-ADD     CSBCAS        V1CIMP
026900971222     C* Se lnp no del mio ramo imposto dati cliente infragr. (mitt.)
027000950714     C                   MOVE      CSBCDI        W8899             4 0
027100950706     C     W8899         IFEQ      8888
027200950706     C     W8899         OREQ      9999
027300950706     C                   Z-ADD     CSBAAS        KAAS
027400950706     C                   Z-ADD     CSBLNP        KLNP
027500950706     C                   Z-ADD     CSBNRS        KNRS
027600950706     C                   Z-ADD     CSBNSP        KNSP
027700950706     C                   MOVEL     *BLANKS       CSARSD
027800950706     C     KCSA          CHAIN     TNCSA01L                           31
027900950706     C                   MOVEL     CSARSD        V1CMIT
028000950706     C                   ELSE
028100950706     C                   MOVEL     *BLANKS       ACORAG
028200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
028300980127     C                   CLEAR                   TIBS69
028400980127     C                   CLEAR                   CNACO
028500980127     C                   CLEAR                   CNIND
028600980127     C                   CLEAR                   CNCLP
028700980127     C                   CLEAR                   FNCLS
028800980127     C                   MOVEL     ' '           I69TLA
028900980127     C                   MOVEL     KNSIF         I69SIF
029000980127     C                   Z-ADD     CSBCDI        I69KAC
029100980127     C                   CALL      'TIBS69R'
029200980127     C                   PARM                    TIBS69
029300980127     C                   PARM                    CNACO
029400980127     C                   PARM                    CNIND
029500980127     C                   PARM                    CNCLP
029600980127     C                   PARM                    FNCLS
029700980127     C     O69ERR        IFEQ      *BLANK
029800950706     C                   MOVEL     ACORAG        V1CMIT
029900980127     C                   END
030000971222     C                   END
030100950706     C                   ADD       1             NRR
030200950706     C                   ADD       1             NRIG
030300950706     C                   WRITE     SC41S01
030400950706     C                   END
030500950720     C                   END
030600950706     C                   END
030700160415     C                   END
030800971222     C                   END
030900140325     C                   END
031000950706     C*
031100950707     C                   READ      TNCSB05L                               23
031200950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
031300950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
031400950706     C*
031500950706     C                   ENDSR
031600931011     C*----------------------------------------------------*
031700931011     C*  Routine gestione operazioni prima videata         *
031800931011     C*----------------------------------------------------*
031900931011     C     GESVD1        BEGSR
032000931011     C*-                                                   *
032100950707     C                   SETOFF                                       20
032200950706     C                   WRITE     SC41Z02
032300950706     C                   WRITE     SC41D02
032400950706     C                   EXFMT     SC41C01
032500931011     C*                                                    *
032600931011     C* Fine Programma                                     *
032700931011     C     *INKL         IFEQ      '1'
032800950706     C                   MOVEL     'S'           WFINE
032900931011     C                   END
033000931011     C*-                                                   *
033100931011     C                   ENDSR
033200931011     C*----------------------------------------------------*
033300931011     C*  Routine gestione operazioni subfile               *
033400931011     C*----------------------------------------------------*
033500931011     C     GESVD2        BEGSR
033600931011     C*-                                                   *
033700950707     C                   SETON                                        20
033800950706     C                   WRITE     SC41Z01
033900950706     C                   WRITE     SC41Z01
034000950706     C                   EXFMT     SC41C01
034100950706     C                   MOVEL     'N'           WINTBL            1
034200931011     C*                                                    *
034300931011     C* Fine Programma                                     *
034400931011     C     *INKC         IFEQ      '1'
034500950706     C                   MOVEL     'S'           WFINE
034600950706     C                   MOVE      3             D40CMD
034700931011     C                   GOTO      FINVD2
034800931011     C                   END
034900931011     C* Ritorno                                            *
035000931011     C     *INKL         IFEQ      '1'
035100950706     C                   MOVEL     'S'           WFINE
035200950706     C                   MOVE      12            D40CMD
035300931011     C                   GOTO      FINVD2
035400931011     C                   END
035500950711     C* ROLLUP
035600950711     C     *IN25         IFEQ      '1'
035700950711     C                   EXSR      ROLLUP
035800950711     C                   GOTO      FINVD2
035900950711     C                   END
036000950706     C*                                                    *
036100990729     C                   CLEAR                   TNSB50
036200950706     C                   EXSR      CNTVD2
036300931011     C* Interrogazione Bolle                               *
036400931011     C     *INKG         IFEQ      '1'
036500950706     C     WINTBL        ANDEQ     'N'
036600950706     C                   EXSR      GESINT
036700931011     C                   END
036800931011     C*                                                    *
036900931012     C     FINVD2        ENDSR
037000950706     C*----------------------------------------------------*
037100950706     C*   GESINT: gestione interrogazione bolle            *
037200950706     C*----------------------------------------------------*
037300950706     C     GESINT        BEGSR
037400950706     C*                                                    *
037500990729     C                   CLEAR                   TNSB50
037600990729     C                   MOVEL     'C05'         I50OP0
037700990729     C                   MOVEL     '0'           O50F03
037800990729     C                   MOVEL     '0'           O50F12
037900990729     C                   MOVEL     '0'           O50ERR
038000990729     C                   MOVEL     *BLANKS       O50MSG
038100990729     C                   MOVEL     KPJBU         WSVKPJ          256
038200990729     C                   MOVEL     *BLANKS       KPJBU
038300990729     C     WACC          IFEQ      '1'
038400990729     C                   Z-ADD     V1CAAS        D50AAS
038500990729     C                   Z-ADD     V1CLNP        D50LNP
038600990729     C                   Z-ADD     V1CNRS        D50NRS
038700990729     C                   Z-ADD     V1CNSP        D50NSP
038800990729     C                   MOVEL     V1CTBL        D50TBL
038900990729     C                   MOVEL     'S'           WINTBL
039000990729     C                   MOVEL     TNSB50        KPJBU
039100990729     C                   CALL      'TNSB50R'
039200990729     C                   PARM                    KPJBA
039300990910     C                   MOVEL     KPJBU         TNSB50
039400990729     C     O50ERR        IFEQ      *BLANKS
039500990729     C                   GOTO      FININT
039600990729     C                   END
039700990729     C* Se ho richiamato il pgm con il numero spedizione/
039800990729     C* codice bolla prova a richiamare senza codice bolla
039900990729     C* come del resto faccio se non ho recuperato il C/Ass.
040000990729     C                   MOVEL     'C00'         I50OP0
040100990729     C                   MOVEL     V1CTBL        D50TBL
040200990729     C                   MOVEL     TNSB50        KPJBU
040300990729     C                   CALL      'TNSB50R'
040400990729     C                   PARM                    KPJBA
040500990729     C                   ELSE
040600990729     C                   MOVEL     TNSB50        KPJBU
040700990729     C                   CALL      'TNSB50R'
040800990729     C                   PARM                    KPJBA
040900990729     C                   END
041000990729     C                   MOVEL     KPJBU         TNSB50
041100990729     C                   MOVEL     WSVKPJ        KPJBU
041200950706     C*
041300990729     C     FININT        ENDSR
041400931011     C*----------------------------------------------------*
041500931011     C*   CNTVD2: Controlli subfile                        *
041600931011     C*----------------------------------------------------*
041700931011     C     CNTVD2        BEGSR
041800931011     C*                                                    *
041900931011     C                   Z-ADD     1             NRR
042000950706     C                   MOVEL     ' '           V1CSCE
042100950706     C                   READC     SC41S01                                82
042200931011     C*                                                    *
042300931011     C     *IN82         DOWEQ     '0'
042400931011     C*                                                    *
042500950706     C     V1CSCE        IFEQ      '1'
042600931011     C                   MOVEL     *BLANKS       KPJBU
042700931012     C                   Z-ADD     NRR           SAVREC            4 0
042800950706     C*  Interrogazione bolla
042900950706     C     *INKG         IFEQ      '1'
043000990729     C                   MOVEL     '1'           WACC              1
043100950706     C                   EXSR      GESINT
043200950706     C                   ELSE
043300120612     c                   eval      tnsc40ds  = tnsc40
043400120612     C                   MOVE      V1CAAS        £D40AAS
043500120612     C                   MOVE      V1CLNP        £D40LNP
043600120612     C                   MOVE      V1CNRS        £D40NRS
043700120612     C                   MOVE      V1CNSP        £D40NSP
043800120612     C                   clear                   £D40ABI
043900120612     C                   clear                   £D40CAB
044000120612     C                   clear                   £D40NAS
044100120612     C                   MOVEL     TNSC40ds      KPJBU
044200931012     C* Interrogazione                                     *
044300950706     C                   CALL      'TNSC43R'
044400931012     C                   PARM                    KPJBA
044500950706     C                   END
044600931011     C*
044700931012     C                   Z-ADD     SAVREC        NRR
044800931014     C                   Z-ADD     SAVREC        DSPNR1
044900950706     C     NRR           CHAIN     SC41S01                            90
045000931011     C                   SETON                                        70
045100950706     C                   MOVEL     ' '           V1CSCE
045200950706     C  N90              UPDATE    SC41S01
045300931011     C                   END
045400931011     C*
045500950706     C                   READC     SC41S01                                82
045600931011     C                   END
045700931012     C*
045800931011     C                   ENDSR
045900931011      *---------------------------------------------------------------*
046000941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
046100931011      *---------------------------------------------------------------*
046200941102     C     *INZSR        BEGSR
046300931011      *---------------------------------------------------------------*
046400931011     C     *ENTRY        PLIST
046500931011     C                   PARM                    KPJBA
046600950706     C                   MOVEL     KPJBU         TNSC40
046700931012     C*
046800931011     C                   Z-ADD     1             CODUT
046900931011     C                   CALL      'X§PARUT'
047000931011     C                   PARM                    UT§DSE
047100931011     C*          CODUT     CABEQ-1        FINE
047200931011     C                   MOVEL     RAGUT         RSUT             20
047300931011     C                   MOVEL     REC80         CNCR80
047400931011     C                   MOVEL     KNMUS         §SEI              6
047500950706     C*----------------------------------------------------*
047600950706     C*  Definizione CHIAVI
047700950706     C     KCSB1         KLIST
047800950706     C                   KFLD                    KAAS
047900950706     C                   KFLD                    KMGS
048000950706     C     KCSB2         KLIST
048100950706     C                   KFLD                    KAAS
048200950706     C                   KFLD                    KMGS
048300950707     C                   KFLD                    KCDI
048400950706     C     KCSA          KLIST
048500950706     C                   KFLD                    KAAS
048600950706     C                   KFLD                    KLNP
048700950706     C                   KFLD                    KNRS
048800950706     C                   KFLD                    KNSP
048900931012     C*----------------------------------------------------*
049000941102     C*  Definizione variabili
049100950707     C     *LIKE         DEFINE    CSBCDI        KCDI
049200950706     C     *LIKE         DEFINE    CSBAAS        KAAS
049300950706     C     *LIKE         DEFINE    CSBMGS        KMGS
049400950706     C     *LIKE         DEFINE    CSALNP        KLNP
049500950706     C     *LIKE         DEFINE    CSANRS        KNRS
049600950706     C     *LIKE         DEFINE    CSANSP        KNSP
049700941102     C*----------------------------------------------------*
049800950706     C*  Inizializzazione delle variabii
049900950706     C                   MOVEL     'N'           WFINE             1
050000950707     C                   Z-ADD     D40SCD        KCDI
050100950707     C                   Z-ADD     D40SDD        KMGS
050200950707     C                   MOVEL     D40SDD        KAAS
050300160415     C                   sETON                                        0304
050400931012     C*----------------------------------------------------*
050500931011     C                   ENDSR
