000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300950707      * TRAS41R5                                                     *
000400931011      *       *-----------------------------------------*            *
000500931011      *          INTERROGAZIONE CONTRASSEGNI X FILIALE               *
000600950707      *          SELEZIONE NUMERO E DATA DISTINTA                    *
000700931011      *       *-----------------------------------------*            *
000800931011      *                                                              *
000900931011?     *--------------------------------------------------------------*
001000931011      * KC -FINE LAVORO                                              *
001100931011      * KG -INTERROGAZIONE BOLLE                                     *
001200931011      * KL -RITORNO                                                  *
001300931011?     *--------------------------------------------------------------*
001400950706     FTNCSA01L  IF   E           K DISK
001500950714     FTNCSB09L  IF   E           K DISK
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
003200950707     D                 DS
003300120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
003301120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
003400980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003500980127     D CNACO         E DS                  EXTNAME(CNACO00F)
003600980127     D CNIND         E DS                  EXTNAME(CNIND00F)
003700980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003800980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
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
008500950707     C                   Z-ADD     D40DDT        G02INV
008600950706     C                   MOVEL     '3'           G02ERR
008700950706     C                   CALL      'XSRDA8'
008800950706     C                   PARM                    WLBDA8
008900950707     C                   Z-ADD     G02DAT        V1CDDT
009000950707     C                   Z-ADD     D40NDT        V1CNDT
009100950707     C                   MOVEL     D40RGP        V1CRGP
009200931011     C*
009300931011     C*  Caricamento subfile
009400950714     C     KCSB          CHAIN     TNCSB09L                           23
009500931011     C*
009600931011     C                   Z-ADD     0             NRR               4 0
009700931013     C                   MOVEL     '1'           TIPVID            1
009800931011     C*
009900950706     C     *IN23         IFEQ      '0'
010000931011     C                   EXSR      ROLLUP
010100931011     C                   END
010200931011     C*
010300931013     C     NRR           IFNE      0
010400931013     C                   MOVEL     '2'           TIPVID            1
010500931013     C                   END
010600931013     C*
010700931012     C                   MOVEL     NOMPG         NOMPGM
010800931011     C*
010900931011     C                   ENDSR
011000931011     C*----------------------------------------------------*
011100931011     C*   ROLLUP: Caricamento di una pagina                *
011200931011     C*----------------------------------------------------*
011300931011     C     ROLLUP        BEGSR
011400931011     C*
011500950706     C     *IN23         IFEQ      '0'
011600931014     C                   Z-ADD     SAVRE1        NRR
011700931011     C                   Z-ADD     0             NRIG              4 0
011800931011     C*
011900950707     C     NRIG          DOWLT     16
012000950706     C     *IN23         ANDEQ     '0'
012100950714     C                   EXSR      GESREC
012200950714     C     KCSB          READE     TNCSB09L                               23
012300950707     C                   END
012400931011     C*
012500931014     C     NRR           IFGT      SAVRE1
012600931014     C     SAVRE1        ADD       1             DSPNR1
012700931014     C                   Z-ADD     NRR           SAVRE1            4 0
012800931014     C                   END
012900931014     C*
013000931011     C                   END
013100931011     C*
013200931011     C                   ENDSR
013300120210     C*----------------------------------------------------*
013400120210     C*   srmulti - per multiassegni
013500120210     C*----------------------------------------------------*
013600120210     C     srmulti       BEGSR
013700120210     C*
013800120210     c                   eval      mnra = *hival
013900120210     c                   eval      mabi = *hival
014000120210     c                   eval      mcab = *hival
014100120210     c     csbnra        setll     tncsm03l
014200120210     c                   do        *hival
014300120210     c     csbnra        reade     tncsm03l
014400120210     c                   if        %eof(tncsm03l)
014500120210     c                   leave
014600120210     c                   end
014700120210     C                   if        csmnra = WNRA
014800120210     c                   eval      mnra = csmnra
014900120210     c                   eval      mabi = csmabi
015000120210     c                   eval      mcab = csmcab
015100120210     c                   leave
015200120210     c                   end
015300120210     c                   enddo
015400120210     C*
015500120210     C                   ENDSR
015600950706     C*----------------------------------------------------*
015700950706     C*   GESREC: Caricamento di una pagina                *
015800950706     C*----------------------------------------------------*
015900950706     C     GESREC        BEGSR
016000950706     C*----------------------------------------------------*
016100950706     C* Se il documento è valido controllo se il record rientra
016200950706     C* nei limiti richiesti
016300950706     C                   MOVEL     D40NAS        WNRA             10
016400120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
016500120220     c                             csbtpi = 'M' and csbabi = 0
016600120210     c                   exsr      srmulti
016700120210     c                   else
016800120210     c                   eval      mnra = csbnra
016900120210     c                   eval      mabi = csbabi
017000120210     c                   eval      mcab = csbcab
017100120210     c                   end
017200950706     C*----------------------------------------------------*
017300950706     C     D40ABI        IFEQ      *ZEROS
017400120210     C     D40ABI        OREQ      mABI
017500950720     C     D40CAB        IFEQ      *ZEROS
017600120210     C     D40CAB        OREQ      mCAB
017601140325     C     D40imp        IFEQ      *ZEROS
017602140325     C     D40imp        OREQ      csbcas
017700950720     C     WNRA          IFEQ      *BLANKS
017800950720     C     WNRA          OREQ      *ZEROS
017900120210     C     WNRA          OREQ      mNRA
018000950707     C*----------------------------------------------------*
018100950707     C* Selezione per stato C/Assegno
018200950707     C     D40TCR        IFEQ      ' '
018300950707     C     D40TCR        OREQ      'D'
018400950707     C     CSBDDC        ANDEQ     0
018500950707     C     D40TCR        OREQ      'G'
018600950707     C     CSBSTA        ANDEQ     2
018700960911     C     D40TCR        OREQ      'T'
018800960911     C     CSBSTA        ANDEQ     6
018900960911     C     D40TCR        OREQ      'R'
019000960911     C     CSBSTA        ANDEQ     7
019100960911     C     D40TCR        OREQ      'O'
019200960911     C     CSBSTA        ANDEQ     8
019300950707     C     D40TCR        OREQ      'A'
019400950707     C     CSBSTA        ANDEQ     9
019500950707     C     D40TCR        OREQ      'S'
019600950707     C     CSBSTA        ANDEQ     5
019700010112     C     D40TCR        OREQ      '3'
019800010112     C     CSBSTA        ANDEQ     3
019900950707     C     D40TCR        OREQ      'I'
020000950707     C     CSBDDC        ANDGT     0
020100950707     C     CSBDDP        ANDEQ     0
020200950707     C     D40TCR        OREQ      'P'
020300950707     C     CSBDDP        ANDGT     0
020400950706     C                   MOVEL     *BLANKS       V1CSCE
020500950706     C                   MOVEL     CSBAAS        V1CAAS
020600950706     C                   MOVEL     CSBMGS        MM                2 0
020700950706     C                   MOVE      CSBMGS        GG                2 0
020800950706     C                   MOVEL     GG            V1CDSP
020900950706     C                   MOVE      MM            V1CDSP
021000950706     C                   MOVE      CSBLNP        V1CLNP
021100950706     C                   MOVE      CSBNRS        V1CNRS
021200950706     C                   MOVE      CSBNSP        V1CNSP
021300950712     C                   MOVE      CSBTBL        V1CTBL
021400950706     C                   MOVEL     CSBRSD        V1CDES
021500950706     C                   MOVE      CSBRMN        V1CRMN
021600950706     C                   MOVE      CSBVCA        V1CVCA
021700950706     C                   Z-ADD     CSBCAS        V1CIMP
021800950714     C                   MOVE      CSBCDI        W8899             4 0
021900950706     C     W8899         IFEQ      8888
022000950706     C     W8899         OREQ      9999
022100950707     C                   Z-ADD     CSBAAS        KAAS
022200950707     C                   Z-ADD     CSBLNP        KLNP
022300950707     C                   Z-ADD     CSBNRS        KNRS
022400950707     C                   Z-ADD     CSBNSP        KNSP
022500950706     C                   MOVEL     *BLANKS       CSARSD
022600950706     C     KCSA          CHAIN     TNCSA01L                           31
022700950706     C                   MOVEL     CSARSD        V1CMIT
022800950706     C                   ELSE
022900950706     C                   MOVEL     *BLANKS       ACORAG
023000980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
023100980127     C                   CLEAR                   TIBS69
023200980127     C                   CLEAR                   CNACO
023300980127     C                   CLEAR                   CNIND
023400980127     C                   CLEAR                   CNCLP
023500980127     C                   CLEAR                   FNCLS
023600980127     C                   MOVEL     ' '           I69TLA
023700980127     C                   MOVEL     KNSIF         I69SIF
023800980127     C                   Z-ADD     CSBCDI        I69KAC
023900980127     C                   CALL      'TIBS69R'
024000980127     C                   PARM                    TIBS69
024100980127     C                   PARM                    CNACO
024200980127     C                   PARM                    CNIND
024300980127     C                   PARM                    CNCLP
024400980127     C                   PARM                    FNCLS
024500980127     C     O69ERR        IFEQ      *BLANK
024600950706     C                   MOVEL     ACORAG        V1CMIT
024700980127     C                   END
024800971222     C                   END
024900950706     C                   ADD       1             NRR
025000950706     C                   ADD       1             NRIG
025100950706     C                   WRITE     SC41S01
025200950706     C                   END
025300950720     C                   END
025400950707     C                   END
025500971222     C                   END
025501140325     C                   END
025600950706     C*
025700950706     C                   ENDSR
025800931011     C*----------------------------------------------------*
025900931011     C*  Routine gestione operazioni prima videata         *
026000931011     C*----------------------------------------------------*
026100931011     C     GESVD1        BEGSR
026200931011     C*-                                                   *
026300950707     C                   SETOFF                                       20
026400950706     C                   WRITE     SC41Z02
026500950706     C                   WRITE     SC41D02
026600950706     C                   EXFMT     SC41C01
026700931011     C*                                                    *
026800931011     C* Fine Programma                                     *
026900931011     C     *INKL         IFEQ      '1'
027000950706     C                   MOVEL     'S'           WFINE
027100931011     C                   END
027200931011     C*-                                                   *
027300931011     C                   ENDSR
027400931011     C*----------------------------------------------------*
027500931011     C*  Routine gestione operazioni subfile               *
027600931011     C*----------------------------------------------------*
027700931011     C     GESVD2        BEGSR
027800931011     C*-                                                   *
027900950707     C                   SETON                                        20
028000950706     C                   WRITE     SC41Z01
028100950706     C                   WRITE     SC41Z01
028200950706     C                   EXFMT     SC41C01
028300950706     C                   MOVEL     'N'           WINTBL            1
028400931011     C*                                                    *
028500931011     C* Fine Programma                                     *
028600931011     C     *INKC         IFEQ      '1'
028700950706     C                   MOVEL     'S'           WFINE
028800950706     C                   MOVE      3             D40CMD
028900931011     C                   GOTO      FINVD2
029000931011     C                   END
029100931011     C* Ritorno                                            *
029200931011     C     *INKL         IFEQ      '1'
029300950706     C                   MOVEL     'S'           WFINE
029400950706     C                   MOVE      12            D40CMD
029500931011     C                   GOTO      FINVD2
029600931011     C                   END
029700931011     C* ROLLUP
029800950711     C     *IN25         IFEQ      '1'
029900950711     C                   EXSR      ROLLUP
030000950711     C                   GOTO      FINVD2
030100950711     C                   END
030200950706     C*                                                    *
030300990729     C                   CLEAR                   TNSB50
030400950706     C                   EXSR      CNTVD2
030500931011     C* Interrogazione Bolle                               *
030600931011     C     *INKG         IFEQ      '1'
030700950706     C     WINTBL        ANDEQ     'N'
030800950706     C                   EXSR      GESINT
030900931011     C                   END
031000931011     C*                                                    *
031100931012     C     FINVD2        ENDSR
031200990729     C*----------------------------------------------------*
031300990729     C*   GESINT: gestione interrogazione bolle            *
031400990729     C*----------------------------------------------------*
031500990729     C     GESINT        BEGSR
031600990729     C*                                                    *
031700990729     C                   CLEAR                   TNSB50
031800990729     C                   MOVEL     'C05'         I50OP0
031900990729     C                   MOVEL     '0'           O50F03
032000990729     C                   MOVEL     '0'           O50F12
032100990729     C                   MOVEL     '0'           O50ERR
032200990729     C                   MOVEL     *BLANKS       O50MSG
032300990729     C                   MOVEL     KPJBU         WSVKPJ          256
032400990729     C                   MOVEL     *BLANKS       KPJBU
032500990729     C     WACC          IFEQ      '1'
032600990729     C                   Z-ADD     V1CAAS        D50AAS
032700990729     C                   Z-ADD     V1CLNP        D50LNP
032800990729     C                   Z-ADD     V1CNRS        D50NRS
032900990729     C                   Z-ADD     V1CNSP        D50NSP
033000990729     C                   MOVEL     V1CTBL        D50TBL
033100990729     C                   MOVEL     'S'           WINTBL
033200990729     C                   MOVEL     TNSB50        KPJBU
033300990729     C                   CALL      'TNSB50R'
033400990729     C                   PARM                    KPJBA
033500990910     C                   MOVEL     KPJBU         TNSB50
033600990729     C     O50ERR        IFEQ      *BLANKS
033700990729     C                   GOTO      FININT
033800990729     C                   END
033900990729     C* Se ho richiamato il pgm con il numero spedizione/
034000990729     C* codice bolla prova a richiamare senza codice bolla
034100990729     C* come del resto faccio se non ho recuperato il C/Ass.
034200990729     C                   MOVEL     'C00'         I50OP0
034300990729     C                   MOVEL     V1CTBL        D50TBL
034400990729     C                   MOVEL     TNSB50        KPJBU
034500990729     C                   CALL      'TNSB50R'
034600990729     C                   PARM                    KPJBA
034700990729     C                   ELSE
034800990729     C                   MOVEL     TNSB50        KPJBU
034900990729     C                   CALL      'TNSB50R'
035000990729     C                   PARM                    KPJBA
035100990729     C                   END
035200990729     C                   MOVEL     KPJBU         TNSB50
035300990729     C                   MOVEL     WSVKPJ        KPJBU
035400990729     C*
035500990729     C     FININT        ENDSR
035600931011     C*----------------------------------------------------*
035700931011     C*   CNTVD2: Controlli subfile                        *
035800931011     C*----------------------------------------------------*
035900931011     C     CNTVD2        BEGSR
036000931011     C*                                                    *
036100931011     C                   Z-ADD     1             NRR
036200950706     C                   MOVEL     ' '           V1CSCE
036300950706     C                   READC     SC41S01                                82
036400931011     C*                                                    *
036500931011     C     *IN82         DOWEQ     '0'
036600931011     C*                                                    *
036700950706     C     V1CSCE        IFEQ      '1'
036800931011     C                   MOVEL     *BLANKS       KPJBU
036900931012     C                   Z-ADD     NRR           SAVREC            4 0
037000950706     C*  Interrogazione bolla
037100950706     C     *INKG         IFEQ      '1'
037200990729     C                   MOVEL     '1'           WACC              1
037300950706     C                   EXSR      GESINT
037400950706     C                   ELSE
037402120612     c                   eval      tnsc40ds = tnsc40
037500120612     C                   MOVE      V1CAAS        £D40AAS
037600120612     C                   MOVE      V1CLNP        £D40LNP
037700120612     C                   MOVE      V1CNRS        £D40NRS
037800120612     C                   MOVE      V1CNSP        £D40NSP
037801120612     C                   clear                   £D40ABI
037802120612     C                   clear                   £D40CAB
037803120612     C                   clear                   £D40NAS
037900120612     C                   MOVEL     TNSC40ds      KPJBU
038000931012     C* Interrogazione                                     *
038100950706     C                   CALL      'TNSC43R'
038200931012     C                   PARM                    KPJBA
038300950706     C                   END
038400931011     C*
038500931012     C                   Z-ADD     SAVREC        NRR
038600931014     C                   Z-ADD     SAVREC        DSPNR1
038700950706     C     NRR           CHAIN     SC41S01                            90
038800931011     C                   SETON                                        70
038900950706     C                   MOVEL     ' '           V1CSCE
039000950706     C  N90              UPDATE    SC41S01
039100931011     C                   END
039200931011     C*
039300950706     C                   READC     SC41S01                                82
039400931011     C                   END
039500931012     C*
039600931011     C                   ENDSR
039700931011      *---------------------------------------------------------------*
039800941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
039900931011      *---------------------------------------------------------------*
040000941102     C     *INZSR        BEGSR
040100931011      *---------------------------------------------------------------*
040200931011     C     *ENTRY        PLIST
040300931011     C                   PARM                    KPJBA
040400950706     C                   MOVEL     KPJBU         TNSC40
040500931012     C*
040600931011     C                   Z-ADD     1             CODUT
040700931011     C                   CALL      'X§PARUT'
040800931011     C                   PARM                    UT§DSE
040900931011     C*          CODUT     CABEQ-1        FINE
041000931011     C                   MOVEL     RAGUT         RSUT             20
041100931011     C                   MOVEL     REC80         CNCR80
041200931011     C                   MOVEL     KNMUS         §SEI              6
041300950706     C*----------------------------------------------------*
041400950706     C*  Definizione CHIAVI
041500950714     C     KCSB          KLIST
041600950707     C                   KFLD                    KRGF
041700950707     C                   KFLD                    KNDT
041800950707     C                   KFLD                    KDDC
041900950706     C     KCSA          KLIST
042000950706     C                   KFLD                    KAAS
042100950706     C                   KFLD                    KLNP
042200950706     C                   KFLD                    KNRS
042300950706     C                   KFLD                    KNSP
042400931012     C*----------------------------------------------------*
042500941102     C*  Definizione variabili
042600950707     C     *LIKE         DEFINE    CSBRGF        KRGF
042700950707     C     *LIKE         DEFINE    CSBNDT        KNDT
042800950707     C     *LIKE         DEFINE    CSBDDC        KDDC
042900950706     C     *LIKE         DEFINE    CSBAAS        KAAS
043000950706     C     *LIKE         DEFINE    CSALNP        KLNP
043100950706     C     *LIKE         DEFINE    CSANRS        KNRS
043200950706     C     *LIKE         DEFINE    CSANSP        KNSP
043300941102     C*----------------------------------------------------*
043400950706     C*  Inizializzazione delle variabii
043500950706     C                   MOVEL     'N'           WFINE             1
043600950707     C                   Z-ADD     D40RGP        KRGF
043700950707     C                   Z-ADD     D40DDT        KDDC
043800950707     C                   MOVEL     D40NDT        KNDT
043900950707     C                   SETON                                        05
044000931012     C*----------------------------------------------------*
044100931011     C                   ENDSR
