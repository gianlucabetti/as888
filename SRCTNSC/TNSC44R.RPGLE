000100931011     H DECEDIT('0,') DATEDIT(*DMY.)
000200931011?     *--------------------------------------------------------------*
000300150518      * Tnsc44R                                                      *
000400931011      *       *-----------------------------------------*            *
000500150518      *          gestione contrassegni smarriti                      *
000600931011      *       *-----------------------------------------*            *
000700931011      *                                                              *
000800931011?     *--------------------------------------------------------------*
000900931011      * KC -FINE LAVORO                                              *
001000931011      * KG -INTERROGAZIONE BOLLE                                     *
001100931011      * KL -RITORNO                                                  *
001200931011?     *--------------------------------------------------------------*
001300950706     FTNCSA01L  IF   E           K DISK
001400150522     FTNCSB04L  iF   E           K DISK    rename(tncsb000:tncsb4)
001500150522     FTNCSB11L  iF   E           K DISK    rename(tncsb000:tncsb11)
001600150522     FTNCSV00F  O    E           K DISK
001700150522     FTNCSB03L  UF A E           K DISK
001800150518     FTNSC44D   CF   E             WORKSTN
001900950706     F                                     SFILE(SC41S01:NRR)
002000931011?     *--------------------------------------------------------------*
002100931011?     *  DS                                                          *
002200931011?     *--------------------------------------------------------------*
002300000107     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002400000107     D DGED          E DS
002500931011     D KPJBA         E DS
002600931011      *
002700950706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800950706     D CNCR80        E DS
002900150522      *
003000150522     D tncsbds       E DS                  EXTNAME(TNCSB00F)
003100150522     D               E DS                  EXTNAME(TNCSV00F)
003200150522     D tncsvds                16    229
003300950706     D*
003400150529     D tnsc30ds      E DS
003500980126     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003600980126     D CNACO         E DS                  EXTNAME(CNACO00F)
003700980126     D CNIND         E DS                  EXTNAME(CNIND00F)
003800980126     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003900980126     D FNCLS         E DS                  EXTNAME(FNCLS00F)
004000941102     D WLBDA8          DS
004100941102     D  G02DAT                 1      8  0
004200941102     D  G02INV                 9     16  0
004300941102     D  G02ERR                17     17
004400941102     D  G02TGI                18     22  0
004500990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004600931011      *
004700931011     D                SDS
004800931011     D  NOMPG                  1     10
004900931011      *---------------------------------------------------------------*
005000931011      *   C I C L O      P R I N C I P A L E                          *
005100931011      *---------------------------------------------------------------*
005200150522     c     inizio        tag
005300150518     c                   exsr      gesvid
005400931011     C*  CARICAMENTO SUBFILE
005500150518     C   26              EXSR      CARSFL
005600931011     C*
005700931011     C*  LOOP
005800950706     C     WFINE         DOUEQ     'S'
005900931011     C*
006000931011     C     TIPVID        CASEQ     '1'           GESVD1
006100931011     C     TIPVID        CASEQ     '2'           GESVD2
006200931011     C                   END
006300931011     C*
006400931011     C                   END
006500931011     C*
006600971222     C* RICHIAMO PGM X CHIUSURA IN LR
006700980126     C                   CLEAR                   TIBS69
006800980126     C                   MOVEL     'C'           I69TLA
006900980126     C                   CALL      'TIBS69R'
007000980126     C                   PARM                    TIBS69
007100980126     C                   PARM                    CNACO
007200980126     C                   PARM                    CNIND
007300980126     C                   PARM                    CNCLP
007400980126     C                   PARM                    FNCLS
007500971222     C*
007600931011     C                   SETON                                        LR
007700150518      *---------------------------------------------------------------*
007800150518     C     gesvid        BEGSR
007900150522      *---------------------------------------------------------------*
008000150518     C*
008100150518     C*  Pulisco subfile
008200150518     c                   do        *hival
008300150522     c                   if        ricar = *blank
008400150525     c     forza         tag
008500150518     C                   exfmt     SC41r01
008600150518     c                   setoff                                       28
008700150518     c                   if        *inkc
008800150518     c                   return
008900150518     c                   endif
009000150518      *obbligatorietà dei campi
009100150518     c                   if        vidndp = 0 or
009200150518     c                             vidddp = 0
009300150518     c                   seton                                        28
009400150518     c                   eval      $msg = 'Inserire sia il numero -
009500150518     c                             che la data'
009600150518     c                   iter
009700150518     c                   endif
009800150518     C                   CLEAR                   WLBDA8
009900150518     C                   Z-ADD     vidddp        G02dat
010000150518     C                   MOVEL     ' '           G02ERR
010100150518     C                   CALL      'XSRDA8'
010200150518     C                   PARM                    WLBDA8
010300150518     C                   Z-ADD     G02DAT        Vidddp
010400150519     C                   Z-ADD     g02inv        vidddpg           8 0
010500150519     c                   if        g02err = '1'
010600150518     c                   seton                                        28
010700150518     c                   eval      $msg = 'Data errata'
010800150518     c                   iter
010900150518     c                   endif
011000150522     c                   endif
011100150518     C     KCSB2         SETLL     TNCSB04L                               26
011200150518     c   26              leave
011300150518     c                   if        not *in26
011400150518     c                   seton                                        28
011500150518     c                   eval      $msg = 'Dati non trovati con -
011600150518     c                             i parametri immessi '
011700150525     c                   goto      forza
011800150518     c                   endif
011900150518     c                   enddo
012000150518     C                   endsr
012100931011      *---------------------------------------------------------------*
012200931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
012300931011      *---------------------------------------------------------------*
012400931011     C     CARSFL        BEGSR
012500931011     C*
012600931011     C*  Pulisco subfile
012700950707     C                   SETOFF                                       2021
012800950706     C                   WRITE     SC41C01
012900950707     C                   SETON                                        21
013000931011     C*
013100931011     C*  Caricamento subfile
013200980126     C                   Z-ADD     0             CSBDDP
013300150518     C     kcsb2         READe     TNCSB04L                               23
013400931011     C*
013500931011     C                   Z-ADD     0             NRR               4 0
013600931013     C                   MOVEL     '1'           TIPVID            1
013700931011     C*
013800950711     C     *IN23         IFEQ      '0'
013900931011     C                   EXSR      ROLLUP
014000931011     C                   END
014100931011     C*
014200931013     C     NRR           IFNE      0
014300931013     C                   MOVEL     '2'           TIPVID            1
014400931013     C                   END
014500931013     C*
014600931012     C                   MOVEL     NOMPG         NOMPGM
014700931011     C*
014800931011     C                   ENDSR
014900931011     C*----------------------------------------------------*
015000931011     C*   ROLLUP: Caricamento di una pagina                *
015100931011     C*----------------------------------------------------*
015200931011     C     ROLLUP        BEGSR
015300931011     C*
015400950706     C     *IN23         IFEQ      '0'
015500931014     C                   Z-ADD     SAVRE1        NRR
015600931011     C                   Z-ADD     0             NRIG              4 0
015700931011     C*
015800950707     C     NRIG          DOWLT     16
015900950706     C     *IN23         ANDEQ     '0'
016000150518     C     CSBDDP        ANDeq     vidddpg
016100150519     C     CSBbna        ANDeq     9999999
016200950707     C*  Se non ho scelto cliente selez. x data documento
016300950707     C                   EXSR      REDDOC
016400950707     C                   END
016500931011     C*
016600931014     C     NRR           IFGT      SAVRE1
016700931014     C     SAVRE1        ADD       1             DSPNR1
016800931014     C                   Z-ADD     NRR           SAVRE1            4 0
016900931014     C                   END
017000931014     C*
017100931011     C                   END
017200950707     C*
017300150518     C  N23CSBDDP        COMP      vidddpg                            23
017400931011     C*
017500931011     C                   ENDSR
017600950707     C*----------------------------------------------------*
017700950707     C*   REDDOC: Lettura con selez. per DOCUMENTO         *
017800950707     C*----------------------------------------------------*
017900950707     C     REDDOC        BEGSR
018000950707     C*
018100150518     C     CSBNDP        IFLT      vidndp
018200950707     C     KCSB2         SETLL     TNCSB04L
018300150518     C     kcsb2         READe     TNCSB04L                               23
018400950707     C                   Z-ADD     CSBDDP        KDDP
018500950707     C                   END
018600950707     C*  Leggo lnp.
018700950707     C     *IN23         DOWEQ     '0'
018800150518     C     CSBNDP        ANDEQ     vidndp
018900150518     C     CSBDDP        ANDLE     vidddpg
019000150518     C     CSBbna        ANDeq     9999999
019100950707     C     NRIG          ANDLT     16
019200950707     C                   EXSR      GESREC
019300950707     C                   END
019400950707     C*
019500950711     C                   Z-ADD     CSBDDP        KDDP
019600150518     C     CSBNDP        IFGT      vidndp
019700950707     C     KCSB1         SETGT     TNCSB04L
019800950707     C                   READ      TNCSB04L                               23
019900950707     C                   Z-ADD     CSBDDP        KDDP
020000950707     C                   END
020100950707     C*
020200950707     C                   ENDSR
020300950706     C*----------------------------------------------------*
020400950706     C*   GESREC: Caricamento di una pagina                *
020500950706     C*----------------------------------------------------*
020600950706     C     GESREC        BEGSR
020700950706     C*----------------------------------------------------*
020800950706     C* Se il documento è valido controllo se il record rientra
020900950706     C* nei limiti richiesti
021000120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
021100120220     c                             csbtpi = 'M' and csbabi = 0
021200150522     C                   MOVE      '*'           V1Cmul
021300150522     C                   MOVEl     csbnra        Vnra
021400120210     c                   else
021500150522     C                   MOVE      *blank        V1Cmul
021600150522     C                   MOVEl     *blank        Vnra
021700150519     c                   end
021800950706     C                   MOVEL     *BLANKS       V1CSCE
021900950706     C                   MOVEL     CSBAAS        V1CAAS
022000950706     C                   MOVEL     CSBMGS        MM                2 0
022100950706     C                   MOVE      CSBMGS        GG                2 0
022200950706     C                   MOVEL     GG            V1CDSP
022300950706     C                   MOVE      MM            V1CDSP
022400950706     C                   MOVE      CSBLNP        V1CLNP
022500950706     C                   MOVE      CSBNRS        V1CNRS
022600950706     C                   MOVE      CSBNSP        V1CNSP
022700950712     C                   MOVE      CSBTBL        V1CTBL
022800950706     C                   MOVEL     CSBRSD        V1CDES
022900950706     C                   MOVE      CSBRMN        V1CRMN
023000950706     C                   MOVE      CSBVCA        V1CVCA
023100950706     C                   Z-ADD     CSBCAS        V1CIMP
023200950714     C                   MOVE      CSBCDI        W8899             4 0
023300950706     C     W8899         IFEQ      8888
023400950706     C     W8899         OREQ      9999
023500950707     C                   Z-ADD     CSBAAS        KAAS
023600950707     C                   Z-ADD     CSBLNP        KLNP
023700950707     C                   Z-ADD     CSBNRS        KNRS
023800950707     C                   Z-ADD     CSBNSP        KNSP
023900950706     C                   MOVEL     *BLANKS       CSARSD
024000950706     C     KCSA          CHAIN     TNCSA01L                           31
024100950706     C                   MOVEL     CSARSD        V1CMIT
024200950706     C                   ELSE
024300950706     C                   MOVEL     *BLANKS       ACORAG
024400980126      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
024500980126     C                   CLEAR                   TIBS69
024600980126     C                   CLEAR                   CNACO
024700980126     C                   CLEAR                   CNIND
024800980126     C                   CLEAR                   CNCLP
024900980126     C                   CLEAR                   FNCLS
025000980126     C                   MOVEL     ' '           I69TLA
025100980126     C                   MOVEL     KNSIF         I69SIF
025200980126     C                   Z-ADD     CSBCDI        I69KAC
025300980126     C                   CALL      'TIBS69R'
025400980126     C                   PARM                    TIBS69
025500980126     C                   PARM                    CNACO
025600980126     C                   PARM                    CNIND
025700980126     C                   PARM                    CNCLP
025800980126     C                   PARM                    FNCLS
025900980126     C     O69ERR        IFEQ      *BLANK
026000950706     C                   MOVEL     ACORAG        V1CMIT
026100950706     C                   END
026200150519     C                   END
026300950706     C                   ADD       1             NRR
026400950706     C                   ADD       1             NRIG
026500950706     C                   WRITE     SC41S01
026600950706     C*
026700950707     C                   READ      TNCSB04L                               23
026800950706     C*
026900950706     C                   ENDSR
027000931011     C*----------------------------------------------------*
027100931011     C*  Routine gestione operazioni prima videata         *
027200931011     C*----------------------------------------------------*
027300931011     C     GESVD1        BEGSR
027400931011     C*-                                                   *
027500150528     C                   z-add     0             savre1
027600950707     C                   SETOFF                                       20
027700950706     C                   WRITE     SC41Z02
027800950706     C                   WRITE     SC41D02
027900950706     C                   EXFMT     SC41C01
028000931011     C*                                                    *
028100931011     C* Fine Programma                                     *
028200150525     C     *INKC         IFEQ      '1'
028300950706     C                   MOVEL     'S'           WFINE
028400931011     C                   END
028500931011     C*-                                                   *
028600931011     C                   ENDSR
028700931011     C*----------------------------------------------------*
028800931011     C*  Routine gestione operazioni subfile               *
028900931011     C*----------------------------------------------------*
029000931011     C     GESVD2        BEGSR
029100931011     C*-                                                   *
029200950707     C                   SETON                                        20
029300950706     C                   WRITE     SC41Z01
029400950706     C                   WRITE     SC41Z01
029500950706     C                   EXFMT     SC41C01
029600950706     C                   MOVEL     'N'           WINTBL            1
029700931011     C*                                                    *
029800931011     C* Fine Programma                                     *
029900150520     C     *INKa         IFEQ      '1'
030000150520     C     1             do        *hival        lnrr              4 0
030100150520     C     lnrr          CHAIN     SC41S01                            90
030200150520     C   90              leave
030300150520     C                   MOVEL     '1'           V1CSCE
030400150527     c                   seton                                        22
030500150520     C                   UPDATE    SC41S01
030600150520     c                   enddo
030700931011     C                   END
030800931011     C* Ritorno                                            *
030900931011     C     *INKL         IFEQ      '1'
031000150525     c                   clear                   vidndp
031100150525     c                   clear                   vidddp
031200150528     C                   z-add     0             savre1
031300150525     C                   GOTO      inizio
031400931011     C                   END
031500931011     C* ROLLUP
031600950711     C     *IN25         IFEQ      '1'
031700950711     C                   EXSR      ROLLUP
031800950711     C                   GOTO      FINVD2
031900950711     C                   END
032000950706     C*                                                    *
032100990729     C                   CLEAR                   TNSB50
032200150522     c                   if        *inkf
032300150522     C                   EXSR      CNTVD2
032400150522     C                   move      'X'           ricar             1
032500150522     C                   z-add     0             savre1
032600150522     c                   goto      inizio
032700150522     c                   endif
032800931011     C*                                                    *
032900150529     C                   EXSR      CNTopz7
033000150529     C*                                                    *
033100931012     C     FINVD2        ENDSR
033200931011     C*----------------------------------------------------*
033300931011     C*   CNTVD2: Controlli subfile                        *
033400931011     C*----------------------------------------------------*
033500931011     C     CNTVD2        BEGSR
033600931011     C*                                                    *
033700150529     C                   Z-ADD     0             NRR
033800150529     c                   do        *hival        nrr
033900150529     C     nrr           chain     SC41S01                            82
034000931011     C*                                                    *
034100150529     C   82              leave
034200931011     C*                                                    *
034300950706     C     V1CSCE        IFEQ      '1'
034400150522     c                   if        v1cmul = *blank
034500150522     c                   z-add     v1caas        kaas
034600150522     c                   z-add     v1clnp        klnp
034700150522     c                   z-add     v1cnrs        knrs
034800150522     c                   z-add     v1cnsp        knsp
034900150520     c                   exsr      pulisci
035000150522     c                   else
035100150520     c                   exsr      pulmulti
035200150520     c                   endif
035300150520      *
035400931012     C                   Z-ADD     NRR           SAVREC            4 0
035500931011     C*
035600931011     C                   SETON                                        70
035700950706     C                   MOVEL     ' '           V1CSCE
035800150529     C                   UPDATE    SC41S01
035900150529     c                   endif
036000931011     C                   END
036100931012     C*
036200931011     C                   ENDSR
036300150529     C*----------------------------------------------------*
036400150529     C*   CNTopz7:                            *
036500150529     C*----------------------------------------------------*
036600150529     C     CNTopz7       BEGSR
036700150529     C*                                                    *
036800150529     C                   Z-ADD     1             NRR
036900150529     C                   MOVEL     ' '           V1CSCE
037000150529     C                   READC     SC41S01                                82
037100150529     C*                                                    *
037200150529     C     *IN82         DOWEQ     '0'
037300150529     C*                                                    *
037400150529     C     V1CSCE        IFEQ      '7'
037500150529     c                   if        v1cmul <> *blank
037600150529     c                   clear                   tnsc30ds
037700150529     c                   eval      sc30CMTI = 'N'
037800150529     c                   eval      sc30AMBI = 'S'
037900150529     c                   eval      sc30TPCI = 'V'
038000150529     c                   eval      sc30AASI = v1caas
038100150529     c                   eval      sc30LNPI = v1clnp
038200150529     c                   eval      sc30NRSI = v1cnrs
038300150529     c                   eval      sc30NSPI = v1cnsp
038400150529     c                   movel(p)  tnsc30ds      kpjbu
038500150529     c                   call      'TNSC30R'
038600150529     c                   parm                    kpjba
038700150529     c                   endif
038800150529      *
038900150529     C                   MOVEL     *BLANKS       KPJBU
039000150529     C                   Z-ADD     NRR           SAVREC            4 0
039100150529     C*
039200150529     C                   Z-ADD     SAVREC        NRR
039300150529     C                   Z-ADD     SAVREC        DSPNR1
039400150529     C     NRR           CHAIN     SC41S01                            90
039500150529     C                   SETON                                        70
039600150529     C                   MOVEL     ' '           V1CSCE
039700150529     C  N90              UPDATE    SC41S01
039800150529     C                   END
039900150529     C*
040000150529     C                   READC     SC41S01                                82
040100150529     C                   END
040200150529     C*
040300150529     C                   ENDSR
040400150522     c**********************************************************************
040500150522     c     pulisci       begsr
040600150522     c**********************************************************************
040700150522     C     kcsa          chain     tncsb03l
040800150522     C                   if        %found(tncsb03l)
040900150522     C                   MOVE      046           CSVFEV
041000150522     C                   MOVEL     'VACA'        CSVCAV
041100150522     C                   TIME                    CSVORV
041200150522     C                   MOVE      DATEU         CSVDTV
041300150522     c                   movel     tncsbds       tncsvds
041400150522     c                   write     tncsv000
041500150522      *
041600150522     c                   clear                   csbddp
041700150522     c                   clear                   csbbna
041800150522     c                   clear                   csbndp
041900150529     c                   clear                   csbdta
042000150522     C                   update    tncsb000
042100150522     C                   endif
042200150522     c                   endsr
042300150522     C*----------------------------------------------------*
042400150522     C*   pulmulti - per multiassegni
042500150522     C*----------------------------------------------------*
042600150522     C     pulmulti      BEGSR
042700150522     C*
042800150522     c     vnra          setll     tncsb11l
042900150522     c                   do        *hival
043000150522     c     vnra          reade     tncsb11l
043100150522     c                   if        %eof(tncsb11l)
043200150522     c                   leave
043300150522     c                   end
043400150522     c                   z-add     csbaas        kaas
043500150522     c                   z-add     csblnp        klnp
043600150522     c                   z-add     csbnrs        knrs
043700150522     c                   z-add     csbnsp        knsp
043800150522     c                   exsr      pulisci
043900150522     c                   enddo
044000150522     C*
044100150522     C                   ENDSR
044200931011      *---------------------------------------------------------------*
044300941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
044400931011      *---------------------------------------------------------------*
044500941102     C     *INZSR        BEGSR
044600931011      *---------------------------------------------------------------*
044700931011     C     *ENTRY        PLIST
044800931011     C                   PARM                    KPJBA
044900931012     C*
045000931011     C                   Z-ADD     1             CODUT
045100931011     C                   CALL      'X§PARUT'
045200931011     C                   PARM                    UT§DSE
045300931011     C                   MOVEL     RAGUT         RSUT             20
045400931011     C                   MOVEL     REC80         CNCR80
045500931011     C                   MOVEL     KNMUS         §SEI              6
045600150522     C* GIRO UDATE
045700150522     C                   CLEAR                   WLBDA8
045800150522     C                   TIME                    WHHDAT           14 0
045900150522     C                   MOVE      WHHDAT        G02DAT
046000150522     C                   CALL      'XSRDA8'
046100150522     C                   PARM                    WLBDA8
046200150522     C                   Z-ADD     G02INV        DATEU             8 0
046300950706     C*----------------------------------------------------*
046400950706     C*  Definizione CHIAVI
046500950706     C     KCSB1         KLIST
046600950707     C                   KFLD                    KDDP
046700950706     C     KCSB2         KLIST
046800150518     C                   KFLD                    vidddpg
046900150518     C                   KFLD                    vidndp
047000150522     C     KCSA          KLIST
047100150522     C                   KFLD                    KAAS
047200150522     C                   KFLD                    KLNP
047300150522     C                   KFLD                    KNRS
047400150522     C                   KFLD                    KNSP
047500931012     C*----------------------------------------------------*
047600941102     C*  Definizione variabili
047700950706     C     *LIKE         DEFINE    CSBAAS        KAAS
047800950707     C     *LIKE         DEFINE    CSBCDI        KCDI
047900950707     C     *LIKE         DEFINE    CSBDDP        KDDP
048000950707     C     *LIKE         DEFINE    CSBNDP        KNDP
048100950706     C     *LIKE         DEFINE    CSALNP        KLNP
048200950706     C     *LIKE         DEFINE    CSANRS        KNRS
048300950706     C     *LIKE         DEFINE    CSANSP        KNSP
048400000107     C*----------------------------------------------------*
048500000107     C*  RECUPERO DIVISA MONETA CORRENTE
048600000107     C                   CLEAR                   DGED
048700000107     C                   CLEAR                   DSBS02
048800000107     C                   MOVEL     'C'           T02MOD
048900000107     C                   MOVEL     KNSIF         T02SIF
049000000107     C                   MOVEL     'GED'         T02COD
049100000107     C                   MOVEL     '1'           T02KE1
049200000107     C                   CALL      'TIBS02R'
049300000107     C                   PARM                    KPJBA
049400000107     C                   PARM                    DSBS02
049500000107     C*
049600000107    3C     T02ERR        IFEQ      ' '
049700000107     C                   MOVEL     T02UNI        DGED
049800000107    3C                   ENDIF
049900941102     C*----------------------------------------------------*
050000950706     C*  Inizializzazione delle variabii
050100950706     C                   MOVEL     'N'           WFINE             1
050200950707     C                   SETON                                        04
050300931012     C*----------------------------------------------------*
050400931011     C                   ENDSR
