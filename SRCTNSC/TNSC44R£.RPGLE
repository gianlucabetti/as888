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
003400980126     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003500980126     D CNACO         E DS                  EXTNAME(CNACO00F)
003600980126     D CNIND         E DS                  EXTNAME(CNIND00F)
003700980126     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003800980126     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003900941102     D WLBDA8          DS
004000941102     D  G02DAT                 1      8  0
004100941102     D  G02INV                 9     16  0
004200941102     D  G02ERR                17     17
004300941102     D  G02TGI                18     22  0
004400990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
004500931011      *
004600931011     D                SDS
004700931011     D  NOMPG                  1     10
004800931011      *---------------------------------------------------------------*
004900931011      *   C I C L O      P R I N C I P A L E                          *
005000931011      *---------------------------------------------------------------*
005100150522     c     inizio        tag
005200150518     c                   exsr      gesvid
005300931011     C*  CARICAMENTO SUBFILE
005400150518     C   26              EXSR      CARSFL
005500931011     C*
005600931011     C*  LOOP
005700950706     C     WFINE         DOUEQ     'S'
005800931011     C*
005900931011     C     TIPVID        CASEQ     '1'           GESVD1
006000931011     C     TIPVID        CASEQ     '2'           GESVD2
006100931011     C                   END
006200931011     C*
006300931011     C                   END
006400931011     C*
006500971222     C* RICHIAMO PGM X CHIUSURA IN LR
006600980126     C                   CLEAR                   TIBS69
006700980126     C                   MOVEL     'C'           I69TLA
006800980126     C                   CALL      'TIBS69R'
006900980126     C                   PARM                    TIBS69
007000980126     C                   PARM                    CNACO
007100980126     C                   PARM                    CNIND
007200980126     C                   PARM                    CNCLP
007300980126     C                   PARM                    FNCLS
007400971222     C*
007500931011     C                   SETON                                        LR
007600150518      *---------------------------------------------------------------*
007700150518     C     gesvid        BEGSR
007800150522      *---------------------------------------------------------------*
007900150518     C*
008000150518     C*  Pulisco subfile
008100150518     c                   do        *hival
008200150522     c                   if        ricar = *blank
008300150525     c     forza         tag
008400150518     C                   exfmt     SC41r01
008500150518     c                   setoff                                       28
008600150518     c                   if        *inkc
008700150518     c                   return
008800150518     c                   endif
008900150518      *obbligatorietà dei campi
009000150518     c                   if        vidndp = 0 or
009100150518     c                             vidddp = 0
009200150518     c                   seton                                        28
009300150518     c                   eval      $msg = 'Inserire sia il numero -
009400150518     c                             che la data'
009500150518     c                   iter
009600150518     c                   endif
009700150518     C                   CLEAR                   WLBDA8
009800150518     C                   Z-ADD     vidddp        G02dat
009900150518     C                   MOVEL     ' '           G02ERR
010000150518     C                   CALL      'XSRDA8'
010100150518     C                   PARM                    WLBDA8
010200150518     C                   Z-ADD     G02DAT        Vidddp
010300150519     C                   Z-ADD     g02inv        vidddpg           8 0
010400150519     c                   if        g02err = '1'
010500150518     c                   seton                                        28
010600150518     c                   eval      $msg = 'Data errata'
010700150518     c                   iter
010800150518     c                   endif
010900150522     c                   endif
011000150518     C     KCSB2         SETLL     TNCSB04L                               26
011100150518     c   26              leave
011200150518     c                   if        not *in26
011300150518     c                   seton                                        28
011400150518     c                   eval      $msg = 'Dati non trovati con -
011500150518     c                             i parametri immessi '
011600150525     c                   goto      forza
011700150518     c                   endif
011800150518     c                   enddo
011900150518     C                   endsr
012000931011      *---------------------------------------------------------------*
012100931011      *  CARSFL: CARICAMENTO DATI DA SUBFILE                          *
012200931011      *---------------------------------------------------------------*
012300931011     C     CARSFL        BEGSR
012400931011     C*
012500931011     C*  Pulisco subfile
012600950707     C                   SETOFF                                       2021
012700950706     C                   WRITE     SC41C01
012800950707     C                   SETON                                        21
012900931011     C*
013000931011     C*  Caricamento subfile
013100980126     C                   Z-ADD     0             CSBDDP
013200150518     C     kcsb2         READe     TNCSB04L                               23
013300931011     C*
013400931011     C                   Z-ADD     0             NRR               4 0
013500931013     C                   MOVEL     '1'           TIPVID            1
013600931011     C*
013700950711     C     *IN23         IFEQ      '0'
013800931011     C                   EXSR      ROLLUP
013900931011     C                   END
014000931011     C*
014100931013     C     NRR           IFNE      0
014200931013     C                   MOVEL     '2'           TIPVID            1
014300931013     C                   END
014400931013     C*
014500931012     C                   MOVEL     NOMPG         NOMPGM
014600931011     C*
014700931011     C                   ENDSR
014800931011     C*----------------------------------------------------*
014900931011     C*   ROLLUP: Caricamento di una pagina                *
015000931011     C*----------------------------------------------------*
015100931011     C     ROLLUP        BEGSR
015200931011     C*
015300950706     C     *IN23         IFEQ      '0'
015400931014     C                   Z-ADD     SAVRE1        NRR
015500931011     C                   Z-ADD     0             NRIG              4 0
015600931011     C*
015700950707     C     NRIG          DOWLT     16
015800950706     C     *IN23         ANDEQ     '0'
015900150518     C     CSBDDP        ANDeq     vidddpg
016000150519     C     CSBbna        ANDeq     9999999
016100950707     C*  Se non ho scelto cliente selez. x data documento
016200950707     C                   EXSR      REDDOC
016300950707     C                   END
016400931011     C*
016500931014     C     NRR           IFGT      SAVRE1
016600931014     C     SAVRE1        ADD       1             DSPNR1
016700931014     C                   Z-ADD     NRR           SAVRE1            4 0
016800931014     C                   END
016900931014     C*
017000931011     C                   END
017100950707     C*
017200150518     C  N23CSBDDP        COMP      vidddpg                            23
017300931011     C*
017400931011     C                   ENDSR
017500950707     C*----------------------------------------------------*
017600950707     C*   REDDOC: Lettura con selez. per DOCUMENTO         *
017700950707     C*----------------------------------------------------*
017800950707     C     REDDOC        BEGSR
017900950707     C*
018000150518     C     CSBNDP        IFLT      vidndp
018100950707     C     KCSB2         SETLL     TNCSB04L
018200150518     C     kcsb2         READe     TNCSB04L                               23
018300950707     C                   Z-ADD     CSBDDP        KDDP
018400950707     C                   END
018500950707     C*  Leggo lnp.
018600950707     C     *IN23         DOWEQ     '0'
018700150518     C     CSBNDP        ANDEQ     vidndp
018800150518     C     CSBDDP        ANDLE     vidddpg
018900150518     C     CSBbna        ANDeq     9999999
019000950707     C     NRIG          ANDLT     16
019100950707     C                   EXSR      GESREC
019200950707     C                   END
019300950707     C*
019400950711     C                   Z-ADD     CSBDDP        KDDP
019500150518     C     CSBNDP        IFGT      vidndp
019600950707     C     KCSB1         SETGT     TNCSB04L
019700950707     C                   READ      TNCSB04L                               23
019800950707     C                   Z-ADD     CSBDDP        KDDP
019900950707     C                   END
020000950707     C*
020100950707     C                   ENDSR
020200950706     C*----------------------------------------------------*
020300950706     C*   GESREC: Caricamento di una pagina                *
020400950706     C*----------------------------------------------------*
020500950706     C     GESREC        BEGSR
020600950706     C*----------------------------------------------------*
020700950706     C* Se il documento è valido controllo se il record rientra
020800950706     C* nei limiti richiesti
020900120220     c                   if        %subst(csbnra: 10: 1)<>' ' and
021000120220     c                             csbtpi = 'M' and csbabi = 0
021100150522     C                   MOVE      '*'           V1Cmul
021200150522     C                   MOVEl     csbnra        Vnra
021300120210     c                   else
021400150522     C                   MOVE      *blank        V1Cmul
021500150522     C                   MOVEl     *blank        Vnra
021600150519     c                   end
021700950706     C                   MOVEL     *BLANKS       V1CSCE
021800950706     C                   MOVEL     CSBAAS        V1CAAS
021900950706     C                   MOVEL     CSBMGS        MM                2 0
022000950706     C                   MOVE      CSBMGS        GG                2 0
022100950706     C                   MOVEL     GG            V1CDSP
022200950706     C                   MOVE      MM            V1CDSP
022300950706     C                   MOVE      CSBLNP        V1CLNP
022400950706     C                   MOVE      CSBNRS        V1CNRS
022500950706     C                   MOVE      CSBNSP        V1CNSP
022600950712     C                   MOVE      CSBTBL        V1CTBL
022700950706     C                   MOVEL     CSBRSD        V1CDES
022800950706     C                   MOVE      CSBRMN        V1CRMN
022900950706     C                   MOVE      CSBVCA        V1CVCA
023000950706     C                   Z-ADD     CSBCAS        V1CIMP
023100950714     C                   MOVE      CSBCDI        W8899             4 0
023200950706     C     W8899         IFEQ      8888
023300950706     C     W8899         OREQ      9999
023400950707     C                   Z-ADD     CSBAAS        KAAS
023500950707     C                   Z-ADD     CSBLNP        KLNP
023600950707     C                   Z-ADD     CSBNRS        KNRS
023700950707     C                   Z-ADD     CSBNSP        KNSP
023800950706     C                   MOVEL     *BLANKS       CSARSD
023900950706     C     KCSA          CHAIN     TNCSA01L                           31
024000950706     C                   MOVEL     CSARSD        V1CMIT
024100950706     C                   ELSE
024200950706     C                   MOVEL     *BLANKS       ACORAG
024300980126      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
024400980126     C                   CLEAR                   TIBS69
024500980126     C                   CLEAR                   CNACO
024600980126     C                   CLEAR                   CNIND
024700980126     C                   CLEAR                   CNCLP
024800980126     C                   CLEAR                   FNCLS
024900980126     C                   MOVEL     ' '           I69TLA
025000980126     C                   MOVEL     KNSIF         I69SIF
025100980126     C                   Z-ADD     CSBCDI        I69KAC
025200980126     C                   CALL      'TIBS69R'
025300980126     C                   PARM                    TIBS69
025400980126     C                   PARM                    CNACO
025500980126     C                   PARM                    CNIND
025600980126     C                   PARM                    CNCLP
025700980126     C                   PARM                    FNCLS
025800980126     C     O69ERR        IFEQ      *BLANK
025900950706     C                   MOVEL     ACORAG        V1CMIT
026000950706     C                   END
026100150519     C                   END
026200950706     C                   ADD       1             NRR
026300950706     C                   ADD       1             NRIG
026400950706     C                   WRITE     SC41S01
026500950706     C*
026600950707     C                   READ      TNCSB04L                               23
026700950706     C*
026800950706     C                   ENDSR
026900931011     C*----------------------------------------------------*
027000931011     C*  Routine gestione operazioni prima videata         *
027100931011     C*----------------------------------------------------*
027200931011     C     GESVD1        BEGSR
027300931011     C*-                                                   *
027400150528     C                   z-add     0             savre1
027500950707     C                   SETOFF                                       20
027600950706     C                   WRITE     SC41Z02
027700950706     C                   WRITE     SC41D02
027800950706     C                   EXFMT     SC41C01
027900931011     C*                                                    *
028000931011     C* Fine Programma                                     *
028100150525     C     *INKC         IFEQ      '1'
028200950706     C                   MOVEL     'S'           WFINE
028300931011     C                   END
028400931011     C*-                                                   *
028500931011     C                   ENDSR
028600931011     C*----------------------------------------------------*
028700931011     C*  Routine gestione operazioni subfile               *
028800931011     C*----------------------------------------------------*
028900931011     C     GESVD2        BEGSR
029000931011     C*-                                                   *
029100950707     C                   SETON                                        20
029200950706     C                   WRITE     SC41Z01
029300950706     C                   WRITE     SC41Z01
029400950706     C                   EXFMT     SC41C01
029500950706     C                   MOVEL     'N'           WINTBL            1
029600931011     C*                                                    *
029700931011     C* Fine Programma                                     *
029800150520     C     *INKa         IFEQ      '1'
029900150520     C     1             do        *hival        lnrr              4 0
030000150520     C     lnrr          CHAIN     SC41S01                            90
030100150520     C   90              leave
030200150520     C                   MOVEL     '1'           V1CSCE
030300150527     c                   seton                                        22
030400150520     C                   UPDATE    SC41S01
030500150520     c                   enddo
030600931011     C                   END
030700931011     C* Ritorno                                            *
030800931011     C     *INKL         IFEQ      '1'
030900150525     c                   clear                   vidndp
031000150525     c                   clear                   vidddp
031100150528     C                   z-add     0             savre1
031200150525     C                   GOTO      inizio
031300931011     C                   END
031400931011     C* ROLLUP
031500950711     C     *IN25         IFEQ      '1'
031600950711     C                   EXSR      ROLLUP
031700950711     C                   GOTO      FINVD2
031800950711     C                   END
031900950706     C*                                                    *
032000990729     C                   CLEAR                   TNSB50
032100150522     c                   if        *inkf
032200150522     C                   EXSR      CNTVD2
032300150522     C                   move      'X'           ricar             1
032400150522     C                   z-add     0             savre1
032500150522     c                   goto      inizio
032600150522     c                   endif
032700931011     C*                                                    *
032800931012     C     FINVD2        ENDSR
032900931011     C*----------------------------------------------------*
033000931011     C*   CNTVD2: Controlli subfile                        *
033100931011     C*----------------------------------------------------*
033200931011     C     CNTVD2        BEGSR
033300931011     C*                                                    *
033400931011     C                   Z-ADD     1             NRR
033500950706     C                   MOVEL     ' '           V1CSCE
033600950706     C                   READC     SC41S01                                82
033700931011     C*                                                    *
033800931011     C     *IN82         DOWEQ     '0'
033900931011     C*                                                    *
034000950706     C     V1CSCE        IFEQ      '1'
034100150522     c                   if        v1cmul = *blank
034200150522     c                   z-add     v1caas        kaas
034300150522     c                   z-add     v1clnp        klnp
034400150522     c                   z-add     v1cnrs        knrs
034500150522     c                   z-add     v1cnsp        knsp
034600150520     c                   exsr      pulisci
034700150522     c                   else
034800150520     c                   exsr      pulmulti
034900150520     c                   endif
035000150520      *
035100931011     C                   MOVEL     *BLANKS       KPJBU
035200931012     C                   Z-ADD     NRR           SAVREC            4 0
035300931011     C*
035400931012     C                   Z-ADD     SAVREC        NRR
035500931014     C                   Z-ADD     SAVREC        DSPNR1
035600950706     C     NRR           CHAIN     SC41S01                            90
035700931011     C                   SETON                                        70
035800950706     C                   MOVEL     ' '           V1CSCE
035900950706     C  N90              UPDATE    SC41S01
036000931011     C                   END
036100931011     C*
036200950706     C                   READC     SC41S01                                82
036300931011     C                   END
036400931012     C*
036500931011     C                   ENDSR
036600150522     c**********************************************************************
036700150522     c     pulisci       begsr
036800150522     c**********************************************************************
036900150522     C     kcsa          chain     tncsb03l
037000150522     C                   if        %found(tncsb03l)
037100150522     C                   MOVE      046           CSVFEV
037200150522     C                   MOVEL     'VACA'        CSVCAV
037300150522     C                   TIME                    CSVORV
037400150522     C                   MOVE      DATEU         CSVDTV
037500150522     c                   movel     tncsbds       tncsvds
037600150522     c                   write     tncsv000
037700150522      *
037800150522     c                   clear                   csbddp
037900150522     c                   clear                   csbbna
038000150522     c                   clear                   csbndp
038100150522     C                   update    tncsb000
038200150522     C                   endif
038300150522     c                   endsr
038400150522     C*----------------------------------------------------*
038500150522     C*   pulmulti - per multiassegni
038600150522     C*----------------------------------------------------*
038700150522     C     pulmulti      BEGSR
038800150522     C*
038900150522     c     vnra          setll     tncsb11l
039000150522     c                   do        *hival
039100150522     c     vnra          reade     tncsb11l
039200150522     c                   if        %eof(tncsb11l)
039300150522     c                   leave
039400150522     c                   end
039500150522     c                   z-add     csbaas        kaas
039600150522     c                   z-add     csblnp        klnp
039700150522     c                   z-add     csbnrs        knrs
039800150522     c                   z-add     csbnsp        knsp
039900150522     c                   exsr      pulisci
040000150522     c                   enddo
040100150522     C*
040200150522     C                   ENDSR
040300931011      *---------------------------------------------------------------*
040400941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
040500931011      *---------------------------------------------------------------*
040600941102     C     *INZSR        BEGSR
040700931011      *---------------------------------------------------------------*
040800931011     C     *ENTRY        PLIST
040900931011     C                   PARM                    KPJBA
041000931012     C*
041100931011     C                   Z-ADD     1             CODUT
041200931011     C                   CALL      'X§PARUT'
041300931011     C                   PARM                    UT§DSE
041400931011     C                   MOVEL     RAGUT         RSUT             20
041500931011     C                   MOVEL     REC80         CNCR80
041600931011     C                   MOVEL     KNMUS         §SEI              6
041700150522     C* GIRO UDATE
041800150522     C                   CLEAR                   WLBDA8
041900150522     C                   TIME                    WHHDAT           14 0
042000150522     C                   MOVE      WHHDAT        G02DAT
042100150522     C                   CALL      'XSRDA8'
042200150522     C                   PARM                    WLBDA8
042300150522     C                   Z-ADD     G02INV        DATEU             8 0
042400950706     C*----------------------------------------------------*
042500950706     C*  Definizione CHIAVI
042600950706     C     KCSB1         KLIST
042700950707     C                   KFLD                    KDDP
042800950706     C     KCSB2         KLIST
042900150518     C                   KFLD                    vidddpg
043000150518     C                   KFLD                    vidndp
043100150522     C     KCSA          KLIST
043200150522     C                   KFLD                    KAAS
043300150522     C                   KFLD                    KLNP
043400150522     C                   KFLD                    KNRS
043500150522     C                   KFLD                    KNSP
043600931012     C*----------------------------------------------------*
043700941102     C*  Definizione variabili
043800950706     C     *LIKE         DEFINE    CSBAAS        KAAS
043900950707     C     *LIKE         DEFINE    CSBCDI        KCDI
044000950707     C     *LIKE         DEFINE    CSBDDP        KDDP
044100950707     C     *LIKE         DEFINE    CSBNDP        KNDP
044200950706     C     *LIKE         DEFINE    CSALNP        KLNP
044300950706     C     *LIKE         DEFINE    CSANRS        KNRS
044400950706     C     *LIKE         DEFINE    CSANSP        KNSP
044500000107     C*----------------------------------------------------*
044600000107     C*  RECUPERO DIVISA MONETA CORRENTE
044700000107     C                   CLEAR                   DGED
044800000107     C                   CLEAR                   DSBS02
044900000107     C                   MOVEL     'C'           T02MOD
045000000107     C                   MOVEL     KNSIF         T02SIF
045100000107     C                   MOVEL     'GED'         T02COD
045200000107     C                   MOVEL     '1'           T02KE1
045300000107     C                   CALL      'TIBS02R'
045400000107     C                   PARM                    KPJBA
045500000107     C                   PARM                    DSBS02
045600000107     C*
045700000107    3C     T02ERR        IFEQ      ' '
045800000107     C                   MOVEL     T02UNI        DGED
045900000107    3C                   ENDIF
046000941102     C*----------------------------------------------------*
046100950706     C*  Inizializzazione delle variabii
046200950706     C                   MOVEL     'N'           WFINE             1
046300950707     C                   SETON                                        04
046400931012     C*----------------------------------------------------*
046500931011     C                   ENDSR
