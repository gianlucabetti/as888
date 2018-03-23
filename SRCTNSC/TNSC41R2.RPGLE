000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300990729      * TNSC41R2                                                     *
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
001500950706     FTNCSB06L  IF   E           K DISK
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
002700950706     D*
002800980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002900980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003000980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003100980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003200980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003300941102     D WLBDA8          DS
003400941102     D  G02DAT                 1      8  0
003500941102     D  G02INV                 9     16  0
003600941102     D  G02ERR                17     17
003700941102     D  G02TGI                18     22  0
003800120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
003801120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
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
006300990615     C* RICHIAMO X CHIUSURA CON LR
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
008500950706     C                   Z-ADD     D40NRM        V1CRMN
008600950706     C                   Z-ADD     D40LP1        V1CLP1
008700950706     C                   Z-ADD     D40SD1        G02INV
008800950706     C                   MOVEL     '3'           G02ERR
008900950706     C                   CALL      'XSRDA8'
009000950706     C                   PARM                    WLBDA8
009100950706     C                   Z-ADD     G02DAT        V1CSD1
009200950706     C                   Z-ADD     D40SA1        G02INV
009300950706     C                   MOVEL     '3'           G02ERR
009400950706     C                   CALL      'XSRDA8'
009500950706     C                   PARM                    WLBDA8
009600950706     C                   Z-ADD     G02DAT        V1CSA1
009700931011     C*
009800931011     C*  Caricamento subfile
009900950706     C     KCSB2         SETLL     TNCSB06L
010000950706     C     KRMN          READE     TNCSB06L                               23
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
012900950707     C     WDTSPE        ANDLE     D40SA1
013000950706     C* Se ho selezionato una sola lnp. gestisco posiz. su quella
013100950706     C     D40LP1        IFNE      0
013200950706     C                   EXSR      REDLNP
013300950706     C                   ELSE
013400950707     C                   EXSR      GESREC
013500950706     C                   END
013600931011     C                   END
013700931011     C*
013800931014     C     NRR           IFGT      SAVRE1
013900931014     C     SAVRE1        ADD       1             DSPNR1
014000931014     C                   Z-ADD     NRR           SAVRE1            4 0
014100931014     C                   END
014200931014     C*
014300931011     C                   END
014400950707     C*
014500950707     C  N23WDTSPE        COMP      D40SA1                             23
014600931011     C*
014700931011     C                   ENDSR
014800950706     C*----------------------------------------------------*
014900950706     C*   REDLNP: Lettura con selez. per LNP               *
015000950706     C*----------------------------------------------------*
015100950706     C     REDLNP        BEGSR
015200950706     C*
015300950707     C     CSBLNP        IFLT      D40LP1
015400950707     C     KCSB2         SETLL     TNCSB06L
015500950706     C     KRMN          READE     TNCSB06L                               23
015600950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
015700950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
015800950707     C                   Z-ADD     CSBMGS        KMGS
015900950707     C                   Z-ADD     CSBAAS        KAAS
016000950706     C                   END
016100950706     C*  Leggo lnp.
016200950706     C     *IN23         DOWEQ     '0'
016300950707     C     CSBLNP        ANDEQ     D40LP1
016400950706     C     WDTSPE        ANDLE     D40SA1
016500950707     C     NRIG          ANDLT     16
016600950706     C                   EXSR      GESREC
016700950706     C                   END
016800950711     C*
016900950711     C                   Z-ADD     CSBAAS        KAAS
017000950711     C                   Z-ADD     CSBMGS        KMGS
017100950706     C*
017200950707     C     CSBLNP        IFGT      D40LP1
017300950706     C     KCSB1         SETGT     TNCSB06L
017400950707     C     KRMN          READE     TNCSB06L                               23
017500950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
017600950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
017700950707     C                   Z-ADD     CSBMGS        KMGS
017800950707     C                   Z-ADD     CSBAAS        KAAS
017900950707     C                   END
018000950706     C*
018100950706     C                   ENDSR
018200950706     C*----------------------------------------------------*
018300120210     C*   srmulti - per multiassegni
018400950706     C*----------------------------------------------------*
018500120210     C     srmulti       BEGSR
018600120210     C*
018700120210     c                   eval      mnra = *hival
018800120210     c                   eval      mabi = *hival
018900120210     c                   eval      mcab = *hival
019000120210     c     csbnra        setll     tncsm03l
019100120210     c                   do        *hival
019200120210     c     csbnra        reade     tncsm03l
019300120210     c                   if        %eof(tncsm03l)
019400120210     c                   leave
019500120210     c                   end
019600120210     C                   if        csmnra = WNRA
019700120210     c                   eval      mnra = csmnra
019800120210     c                   eval      mabi = csmabi
019900120210     c                   eval      mcab = csmcab
020000120210     c                   leave
020100120210     c                   end
020200120210     c                   enddo
020300120210     C*
020400120210     C                   ENDSR
020500120210     C*----------------------------------------------------*
020600120210     C*   GESREC: Caricamento di una pagina                *
020700120210     C*----------------------------------------------------*
020800120210     C     GESREC        BEGSR
020900950706     C*----------------------------------------------------*
021000950706     C* Se il documento è valido controllo se il record rientra
021100950706     C* nei limiti richiesti
021200950706     C                   MOVEL     D40NAS        WNRA             10
021300120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
021400120220     c                             csbtpi = 'M' and csbabi = 0
021500120210     c                   exsr      srmulti
021600120210     c                   else
021700120210     c                   eval      mnra = csbnra
021800120210     c                   eval      mabi = csbabi
021900120210     c                   eval      mcab = csbcab
022000120210     c                   end
022100950706     C*----------------------------------------------------*
022200950706     C     D40ABI        IFEQ      *ZEROS
022300120210     C     D40ABI        OREQ      mABI
022400950720     C     D40CAB        IFEQ      *ZEROS
022500120210     C     D40CAB        OREQ      mCAB
022501140325     C     D40imp        IFeq      *ZEROS
022502140325     C     D40imp        oreq      csbcas
022600950720     C     WNRA          IFEQ      *BLANKS
022700950720     C     WNRA          OREQ      *ZEROS
022800120210     C     WNRA          OREQ      mNRA
022900950706     C*----------------------------------------------------*
023000950706     C* Selezione per stato C/Assegno
023100950706     C     D40TCR        IFEQ      ' '
023200950706     C     D40TCR        OREQ      'D'
023300950706     C     CSBDDC        ANDEQ     0
023400950706     C     D40TCR        OREQ      'G'
023500950706     C     CSBSTA        ANDEQ     2
023600950706     C     D40TCR        OREQ      'A'
023700950706     C     CSBSTA        ANDEQ     9
023800960909     C     D40TCR        OREQ      'T'
023900960909     C     CSBSTA        ANDEQ     6
024000960909     C     D40TCR        OREQ      'R'
024100960909     C     CSBSTA        ANDEQ     7
024200960909     C     D40TCR        OREQ      'O'
024300960909     C     CSBSTA        ANDEQ     8
024400950706     C     D40TCR        OREQ      'S'
024500950706     C     CSBSTA        ANDEQ     5
024600010112     C     D40TCR        OREQ      '3'
024700010112     C     CSBSTA        ANDEQ     3
024800950706     C     D40TCR        OREQ      'I'
024900950706     C     CSBDDC        ANDGT     0
025000950706     C     CSBDDP        ANDEQ     0
025100950706     C     D40TCR        OREQ      'P'
025200950706     C     CSBDDP        ANDGT     0
025300950706     C                   MOVEL     *BLANKS       V1CSCE
025400950706     C                   MOVEL     CSBAAS        V1CAAS
025500950706     C                   MOVEL     CSBMGS        MM                2 0
025600950706     C                   MOVE      CSBMGS        GG                2 0
025700950706     C                   MOVEL     GG            V1CDSP
025800950706     C                   MOVE      MM            V1CDSP
025900950706     C                   MOVE      CSBLNP        V1CLNP
026000950706     C                   MOVE      CSBNRS        V1CNRS
026100950706     C                   MOVE      CSBNSP        V1CNSP
026200950712     C                   MOVE      CSBTBL        V1CTBL
026300950706     C                   MOVEL     CSBRSD        V1CDES
026400950706     C                   MOVE      CSBRMN        V1CRMN
026500950706     C                   MOVE      CSBVCA        V1CVCA
026600950706     C                   Z-ADD     CSBCAS        V1CIMP
026700971222     C* Se lnp no del mio ramo imposto dati cliente infragr. (mitt.)
026800950714     C                   MOVE      CSBCDI        W8899             4 0
026900950706     C     W8899         IFEQ      8888
027000950706     C     W8899         OREQ      9999
027100950706     C                   Z-ADD     CSBAAS        KAAS
027200950706     C                   Z-ADD     CSBLNP        KLNP
027300950706     C                   Z-ADD     CSBNRS        KNRS
027400950706     C                   Z-ADD     CSBNSP        KNSP
027500950706     C                   MOVEL     *BLANKS       CSARSD
027600950706     C     KCSA          CHAIN     TNCSA01L                           31
027700950706     C                   MOVEL     CSARSD        V1CMIT
027800950706     C                   ELSE
027900950706     C                   MOVEL     *BLANKS       ACORAG
028000980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
028100980127     C                   CLEAR                   TIBS69
028200980127     C                   CLEAR                   CNACO
028300980127     C                   CLEAR                   CNIND
028400980127     C                   CLEAR                   CNCLP
028500980127     C                   CLEAR                   FNCLS
028600980127     C                   MOVEL     ' '           I69TLA
028700980127     C                   MOVEL     KNSIF         I69SIF
028800980127     C                   Z-ADD     CSBCDI        I69KAC
028900980127     C                   CALL      'TIBS69R'
029000980127     C                   PARM                    TIBS69
029100980127     C                   PARM                    CNACO
029200980127     C                   PARM                    CNIND
029300980127     C                   PARM                    CNCLP
029400980127     C                   PARM                    FNCLS
029500980127     C     O69ERR        IFEQ      *BLANK
029600950706     C                   MOVEL     ACORAG        V1CMIT
029700980127     C                   END
029800971222     C                   END
029900950706     C                   ADD       1             NRR
030000950706     C                   ADD       1             NRIG
030100950706     C                   WRITE     SC41S01
030200950720     C                   END
030300950720     C                   END
030400950706     C                   END
030500971222     C                   END
030501140325     C                   END
030600950706     C*
030700950706     C     KRMN          READE     TNCSB06L                               23
030800950707     C                   Z-ADD     CSBMGS        WDTSPE            8 0
030900950707     C                   MOVEL     CSBAAS        WDTSPE            8 0
031000950706     C*
031100950706     C                   ENDSR
031200931011     C*----------------------------------------------------*
031300931011     C*  Routine gestione operazioni prima videata         *
031400931011     C*----------------------------------------------------*
031500931011     C     GESVD1        BEGSR
031600931011     C*-                                                   *
031700950707     C                   SETOFF                                       20
031800950706     C                   WRITE     SC41Z02
031900950706     C                   WRITE     SC41D02
032000950706     C                   EXFMT     SC41C01
032100931011     C*                                                    *
032200931011     C* Fine Programma                                     *
032300931011     C     *INKL         IFEQ      '1'
032400950706     C                   MOVEL     'S'           WFINE
032500931011     C                   END
032600931011     C*-                                                   *
032700931011     C                   ENDSR
032800931011     C*----------------------------------------------------*
032900931011     C*  Routine gestione operazioni subfile               *
033000931011     C*----------------------------------------------------*
033100931011     C     GESVD2        BEGSR
033200931011     C*-                                                   *
033300950707     C                   SETON                                        20
033400950706     C                   WRITE     SC41Z01
033500950706     C                   WRITE     SC41Z01
033600950706     C                   EXFMT     SC41C01
033700950706     C                   MOVEL     'N'           WINTBL            1
033800931011     C*                                                    *
033900931011     C* Fine Programma                                     *
034000931011     C     *INKC         IFEQ      '1'
034100950706     C                   MOVEL     'S'           WFINE
034200950706     C                   MOVE      3             D40CMD
034300931011     C                   GOTO      FINVD2
034400931011     C                   END
034500931011     C* Ritorno                                            *
034600931011     C     *INKL         IFEQ      '1'
034700950706     C                   MOVEL     'S'           WFINE
034800950706     C                   MOVE      12            D40CMD
034900931011     C                   GOTO      FINVD2
035000931011     C                   END
035100931011     C* ROLLUP
035200950711     C     *IN25         IFEQ      '1'
035300950711     C                   EXSR      ROLLUP
035400950711     C                   GOTO      FINVD2
035500950711     C                   END
035600950706     C*                                                    *
035700990729     C                   CLEAR                   TNSB50
035800950706     C                   EXSR      CNTVD2
035900931011     C* Interrogazione Bolle                               *
036000931011     C     *INKG         IFEQ      '1'
036100950706     C     WINTBL        ANDEQ     'N'
036200950706     C                   EXSR      GESINT
036300931011     C                   END
036400931011     C*                                                    *
036500931012     C     FINVD2        ENDSR
036600950706     C*----------------------------------------------------*
036700950706     C*   GESINT: gestione interrogazione bolle            *
036800950706     C*----------------------------------------------------*
036900950706     C     GESINT        BEGSR
037000950706     C*                                                    *
037100990729     C                   CLEAR                   TNSB50
037200990729     C                   MOVEL     'C05'         I50OP0
037300990729     C                   MOVEL     '0'           O50F03
037400990729     C                   MOVEL     '0'           O50F12
037500990729     C                   MOVEL     '0'           O50ERR
037600990729     C                   MOVEL     *BLANKS       O50MSG
037700950717     C                   MOVEL     KPJBU         WSVKPJ          256
037800950717     C                   MOVEL     *BLANKS       KPJBU
037900990729     C     WACC          IFEQ      '1'
038000990729     C                   Z-ADD     V1CAAS        D50AAS
038100990729     C                   Z-ADD     V1CLNP        D50LNP
038200990729     C                   Z-ADD     V1CNRS        D50NRS
038300990729     C                   Z-ADD     V1CNSP        D50NSP
038400990729     C                   MOVEL     V1CTBL        D50TBL
038500950706     C                   MOVEL     'S'           WINTBL
038600990729     C                   MOVEL     TNSB50        KPJBU
038700990729     C                   CALL      'TNSB50R'
038800950706     C                   PARM                    KPJBA
038900990910     C                   MOVEL     KPJBU         TNSB50
039000990729     C     O50ERR        IFEQ      *BLANKS
039100990729     C                   GOTO      FININT
039200990729     C                   END
039300990729     C* Se ho richiamato il pgm con il numero spedizione/
039400990729     C* codice bolla prova a richiamare senza codice bolla
039500990729     C* come del resto faccio se non ho recuperato il C/Ass.
039600990729     C                   MOVEL     'C00'         I50OP0
039700990729     C                   MOVEL     V1CTBL        D50TBL
039800990729     C                   MOVEL     TNSB50        KPJBU
039900990729     C                   CALL      'TNSB50R'
040000950712     C                   PARM                    KPJBA
040100950706     C                   ELSE
040200990729     C                   MOVEL     TNSB50        KPJBU
040300990729     C                   CALL      'TNSB50R'
040400950706     C                   PARM                    KPJBA
040500950706     C                   END
040600990729     C                   MOVEL     KPJBU         TNSB50
040700950717     C                   MOVEL     WSVKPJ        KPJBU
040800950706     C*
040900990729     C     FININT        ENDSR
041000931011     C*----------------------------------------------------*
041100931011     C*   CNTVD2: Controlli subfile                        *
041200931011     C*----------------------------------------------------*
041300931011     C     CNTVD2        BEGSR
041400931011     C*                                                    *
041500931011     C                   Z-ADD     1             NRR
041600950706     C                   MOVEL     ' '           V1CSCE
041700950706     C                   READC     SC41S01                                82
041800931011     C*                                                    *
041900931011     C     *IN82         DOWEQ     '0'
042000931011     C*                                                    *
042100950706     C     V1CSCE        IFEQ      '1'
042200931011     C                   MOVEL     *BLANKS       KPJBU
042300931012     C                   Z-ADD     NRR           SAVREC            4 0
042400950706     C*  Interrogazione bolla
042500950706     C     *INKG         IFEQ      '1'
042600990729     C                   MOVEL     '1'           WACC              1
042700950706     C                   EXSR      GESINT
042800950706     C                   ELSE
042801120612     c                   eval      tnsc40ds = tnsc40
042900120612     C                   MOVE      V1CAAS        £D40AAS
043000120612     C                   MOVE      V1CLNP        £D40LNP
043100120612     C                   MOVE      V1CNRS        £D40NRS
043200120612     C                   MOVE      V1CNSP        £D40NSP
043201120612     C                   clear                   £D40ABI
043202120612     C                   clear                   £D40CAB
043203120612     C                   clear                   £D40NAS
043300120612     C                   MOVEL(p)  TNSC40ds      KPJBU
043400931012     C* Interrogazione                                     *
043500950706     C                   CALL      'TNSC43R'
043600931012     C                   PARM                    KPJBA
043700950706     C                   END
043800931011     C*
043900931012     C                   Z-ADD     SAVREC        NRR
044000931014     C                   Z-ADD     SAVREC        DSPNR1
044100950706     C     NRR           CHAIN     SC41S01                            90
044200931011     C                   SETON                                        70
044300950706     C                   MOVEL     ' '           V1CSCE
044400950706     C  N90              UPDATE    SC41S01
044500931011     C                   END
044600931011     C*
044700950706     C                   READC     SC41S01                                82
044800931011     C                   END
044900931012     C*
045000931011     C                   ENDSR
045100931011      *---------------------------------------------------------------*
045200941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
045300931011      *---------------------------------------------------------------*
045400941102     C     *INZSR        BEGSR
045500931011      *---------------------------------------------------------------*
045600931011     C     *ENTRY        PLIST
045700931011     C                   PARM                    KPJBA
045800950706     C                   MOVEL     KPJBU         TNSC40
045900931012     C*
046000931011     C                   Z-ADD     1             CODUT
046100931011     C                   CALL      'X§PARUT'
046200931011     C                   PARM                    UT§DSE
046300931011     C*          CODUT     CABEQ-1        FINE
046400931011     C                   MOVEL     RAGUT         RSUT             20
046500931011     C                   MOVEL     REC80         CNCR80
046600931011     C                   MOVEL     KNMUS         §SEI              6
046700950706     C*
046800950706     C                   Z-ADD     1             X                 3 0
046900950706     C*----------------------------------------------------*
047000950706     C*  Definizione CHIAVI
047100950706     C     KCSB1         KLIST
047200950706     C                   KFLD                    KRMN
047300950706     C                   KFLD                    KAAS
047400950706     C                   KFLD                    KMGS
047500950706     C     KCSB2         KLIST
047600950706     C                   KFLD                    KRMN
047700950706     C                   KFLD                    KAAS
047800950706     C                   KFLD                    KMGS
047900950706     C                   KFLD                    KLNP
048000950706     C     KCSA          KLIST
048100950706     C                   KFLD                    KAAS
048200950706     C                   KFLD                    KLNP
048300950706     C                   KFLD                    KNRS
048400950706     C                   KFLD                    KNSP
048500931012     C*----------------------------------------------------*
048600941102     C*  Definizione variabili
048700950706     C     *LIKE         DEFINE    CSBRMN        KRMN
048800950706     C     *LIKE         DEFINE    CSBAAS        KAAS
048900950706     C     *LIKE         DEFINE    CSBMGS        KMGS
049000950706     C     *LIKE         DEFINE    CSALNP        KLNP
049100950706     C     *LIKE         DEFINE    CSANRS        KNRS
049200950706     C     *LIKE         DEFINE    CSANSP        KNSP
049300941102     C*----------------------------------------------------*
049400950706     C*  Inizializzazione delle variabii
049500950706     C                   MOVEL     'N'           WFINE             1
049600950706     C                   Z-ADD     D40NRM        KRMN
049700950706     C                   Z-ADD     D40SD1        KMGS
049800950706     C                   MOVEL     D40SD1        KAAS
049900950706     C                   MOVE      D40LP1        KLNP
050000931014     C                   SETON                                        02
050100931012     C*----------------------------------------------------*
050200931011     C                   ENDSR
