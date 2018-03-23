000100890828     H DFTNAME(CN0500) DATEDIT(*YMD)
000200890828     F*--------------------------------------------------------------*
000300890828     F* MANUTENZIONE ANAGRAFICA SOSTITUTO D'IMPOSTA                  *
000400891027     F* UTILIZZA 4 DS ESTERNE:                                       *
000500890828     F*                       - CNCR40                               *
000600890828     F*                       - CNCR41                               *
000700890828     F*                       - CNCR42                               *
000800891027     F*                       - CNCR80                               *
000900891027     F* CHE RIDEFINISCONO 4   RECORD DEL PARUT00F CON PARTIP = 40    *
001000890828     F*                                                        41    *
001100890828     F*                                                        42    *
001200891027     F*                                                        80    *
001300890828     F*----------------------* USO DEGLI INDICATORI *----------------*
001400890828     F*  01        INSERIMENTO                                       *
001500890828     F*  02        VARIAZIONE                                        *
001600930825     F*  12        RITORNO SENZA AGGIORNAMENTO                       *
001700930825     F*  03        FINE LAVORO                                       *
001800890828     F*  20        DI LAVORO                                         *
001900890828     F*----------------------* INDICATORI ERRORI *-------------------*
002000890828     F*  30        ERRORE CODICE FISCALE                             *
002100890828     F*  31        ERRORE PARTITA IVA                                *
002200890828     F*  32        ERRORE STATO AMMINISTRATIVO SOCIETA'              *
002300890828     F*  33        ERRORE NATURA GIURIDICA SOCIETA'                  *
002400890828     F*  34        ERRORE CODICE ATTIVITA'                           *
002500890828     F*  35        ERRORE DATA ULTIMA APPROVAZIONE BILANCIO          *
002600890828     F*  36        ERRORE DATA SCADENZA PROSSIMA APPROV.BILANCIO     *
002700890828     F*  37        ERRORE DATA ISCRIZIONE C.C.I.A.A.                 *
002800890828     F*  38        CODICE FISCALE E PARTITA IVA NON DIGITATI         *
002900061113     F*  43        Partita iva forzabile                             *
003000891027     F*  50        SE FLAG BOLLO VIRTUALE = SI I CAMPI N.RO AUTORIZZ.*
003100891027     F*            DATA E UFFICIO POSTALE SONO OBBLIGATORI           *
003200891027     F*  51        DATA AUTORIZZ.ERRATA                              *
003300891027     F*-                                                             *
003400890828     F*  40        NON TROVATO TIPO RECORD '40' SU PARUT00F          *
003500890828     F*  41        NON TROVATO TIPO RECORD '41' SU PARUT00F          *
003600890828     F*  42        NON TROVATO TIPO RECORD '42' SU PARUT00F          *
003700891027     F*  80        NON TROVATO TIPO RECORD '80' SU PARUT00F          *
003800890828     F*  99        ERRORE GENERICO                                   *
003900890828     F*--------------------------------------------------------------*
004000890828     FCNC070D   CF   E             WORKSTN
004100900605     FAZORG01L  IF   E           K DISK
004200890828     FPARUT00F  UF A E           K DISK
004300890828     D*--------------------------------------------------------------*
004400890828     D*                        T A B E L L E                         *
004500890828     D*--------------------------------------------------------------*
004600890828     D*-
004700890828     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            CONTROLLO
004800890828     D*--------------------------------------------------------------*
004900890828     D*                        I N P U T                             *
005000890828     D*--------------------------------------------------------------*
005100890828     D CNCR40        E DS
005200890828     D CNCR41        E DS
005300890828     D CNCR42        E DS
005400891027     D CNCR80        E DS
005500890828     D KPJBA         E DS
005600890828     D PARUTD        E DS                  EXTNAME(UTEDSE0F)
005700890828     D               E DS                  EXTNAME(PARUT00F)
005800890828     D  WPAR                  14    163
005900890828     D PARAM           DS
006000890828     D  WCCTO                  1      3P 0
006100890828     D  WSCTO                  4      7P 0
006200061113
006300151218     d xcfiva1ds     e ds
006400061113     d  ivaeu                  3      4
006500061113     d  ivait                  5     18
006600061113
006700061113
006800061113     d wforzaiva       s              1    inz(*Off)
006900061113
007000890828     I*--------------------------------------------------------------*
007100890828      /EJECT
007200890828     C*----------------------------------------------------*
007300890828     C*                  M A I N   L I N E                 *
007400890828     C*----------------------------------------------------*
007500890828     C                   EXSR      DEFCAM                                       *
007600890828     C*-                                                   *
007700890828     C                   MOVE      KNRAZ         NRJOB                          *
007800890828     C                   Z-ADD     1             CODUT                          *
007900890828     C                   CALL      'XPARUT'                                     *
008000890828     C                   PARM                    PARUTD                         *
008100890828     C     CODUT         CABEQ     -1            FINE                           *
008200890828     C                   Z-ADD     CODUT         PARKUT                         *
008300890828     C                   MOVEL     RAGUT         RSUT                           *
008400890828     C*-                                                   *
008500890828     C                   Z-ADD     40            PARTIP                         *
008600890828     C     KPARUT        CHAIN     PARUT                              40        *
008700890828     C  N40              MOVEL     WPAR          CNCR40                         *
008800890828     C*-                                                   *
008900890828     C                   Z-ADD     41            PARTIP                         *
009000890828     C     KPARUT        CHAIN     PARUT                              41        *
009100890828     C  N41              MOVEL     WPAR          CNCR41                         *
009200890828     C*-                                                   *
009300890828     C                   Z-ADD     42            PARTIP                         *
009400890828     C     KPARUT        CHAIN     PARUT                              42        *
009500890828     C  N42              MOVEL     WPAR          CNCR42                         *
009600890828     C*-                                                   *
009700891027     C                   Z-ADD     80            PARTIP                         *
009800891027     C     KPARUT        CHAIN     PARUT                              80        *
009900891027     C  N80              MOVEL     WPAR          CNCR80                         *
010000891027     C*-                                                   *
010100891030     C   80              Z-ADD     0             SIMDBV
010200891030     C   80              Z-ADD     0             SIMCSO
010300891207     C   80              Z-ADD     0             SIMFEL
010400890828     C     *IN40         IFEQ      '1'                                          *
010500890828     C     *IN41         ANDEQ     '1'                                          *
010600890828     C     *IN42         ANDEQ     '1'                                          *
010700891027     C     *IN80         ANDEQ     '1'
010800890828     C                   SETON                                        01        *IMMISSION
010900890828     C                   Z-ADD     0             SIMDTI                         0 !
011000890828     C                   Z-ADD     0             SIMCAS                         0 !
011100890828     C                   Z-ADD     0             SIMCAP                         0 !--> VID
011200890828     C                   Z-ADD     0             SIMDAP                         0 !
011300890828     C                   Z-ADD     0             SIMDSA                         0 !
011400890828     C                   ELSE                                                   *
011500890828     C     *IN40         IFEQ      '0'                                          *
011600890828     C     *IN41         ANDEQ     '0'                                          *
011700890828     C     *IN42         ANDEQ     '0'                                          *
011800890828     C                   SETON                                        02        *VARIAZION
011900890828     C                   EXSR      RIEMPI                                       *
012000890828     C                   ELSE                                                   *
012100890828     C                   EXFMT     C070D2
012200890828     C                   GOTO      FINE                                         *ERRORE
012300890828     C                   END                                                    *
012400890828     C                   END                                                    *
012500890828     C*-                                                   *
012600890828     C*- GESTIONE IMMISSIONE/VARIAZIONE--------------------*
012700890828     C*-                                                   *
012800890828     C                   DO        *HIVAL                                       *
012900890828     C*-                                                   *
013000890828     C                   EXFMT     C070D1                                       *
013100890828     C                   SETOFF                                       303132    *
013200890828     C                   SETOFF                                       333435    *
013300890828     C                   SETOFF                                         3637    *
013400891027     C                   SETOFF                                         5051    *
013500890828     C                   SETOFF                                         9899    *
013600930825     C  NKL              EXSR      CNTFMT                                       *CONTROLLI
013700890828     C*-                                                   *
013800930825     C  NKL
013900930825     CANN99              DO                                                     *
014000890828     C   01              EXSR      IMMISS                                       *IMMISSION
014100890828     C   02              EXSR      VARIAZ                                       *VARIAZION
014200890828     C   98              EXFMT     C070D1                                       *ERRORI AG
014300890828     C                   END                                                    *
014400890828     C*-                                                   *
014500890828     C   99              END                                                    *
014600890828     C*-                                                   *
014700890828     C     FINE          TAG                                                    *
014800890828     C                   SETON                                        LR        *
014900890828     C*----------------------------------------------------*
015000890828      /EJECT
015100890828     C*----------------------------------------------------*
015200890828     C*         D E F I N I Z I O N E    C A M P I         *
015300890828     C*----------------------------------------------------*
015400890828     C     DEFCAM        BEGSR                                                  *
015500890828     C*-                                                   *
015600890828     C     *ENTRY        PLIST                                                  *
015700890828     C                   PARM                    KPJBA                          *
015800890828     C*-----CHIAVE PARUT00F                                *
015900890828     C     KPARUT        KLIST                                                  *
016000890828     C                   KFLD                    PARKUT                         *
016100890828     C                   KFLD                    PARTIP                         *
016200890828     C*-                                                   *
016300890828     C                   MOVE      *BLANKS       WIVA             11            *
016400890828     C                   MOVE      *BLANKS       WRIVA             5            *
016500890828     C                   Z-ADD     0             WIVAP            11 0          *
016600061113     **C                   Z-ADD     0             XSTATO            1 0
016700890828     C*-                                                   *
016800890828     C                   ENDSR                                                  *
016900890828     C*----------------------------------------------------*
017000890828      /EJECT
017100890828     C*----------------------------------------------------*
017200890828     C*              RIEMPIMENTO FORMATO VARIAZIONE        *
017300890828     C*----------------------------------------------------*
017400890828     C     RIEMPI        BEGSR                                                  *
017500890828     C*-                                                   *
017600890828     C     SIMDAP        IFNE      0                                            *
017700890828     C                   Z-ADD     1             XDTSTA                         *
017800890828     C                   Z-ADD     SIMDAP        XDTAMG                         *
017900890828     C                   EXSR      CNTDAT                                       *
018000890828     C     XDTSTA        IFLT      0                                            *
018100890828     C                   Z-ADD     999999        SIMDAP                         *
018200890828     C                   ELSE                                                   *
018300890828     C                   Z-ADD     XDTGMA        SIMDAP                         *
018400890828     C                   END                                                    *
018500890828     C                   END                                                    *
018600890828     C*-                                                   *
018700890828     C     SIMDSA        IFNE      0                                            *
018800890828     C                   Z-ADD     1             XDTSTA                         *
018900890828     C                   Z-ADD     SIMDSA        XDTAMG                         *
019000890828     C                   EXSR      CNTDAT                                       *
019100890828     C     XDTSTA        IFLT      0                                            *
019200890828     C                   Z-ADD     999999        SIMDSA                         *
019300890828     C                   ELSE                                                   *
019400890828     C                   Z-ADD     XDTGMA        SIMDSA                         *
019500890828     C                   END                                                    *
019600890828     C                   END                                                    *
019700891030     C     SIMDBV        IFNE      0                                            *
019800891030     C                   Z-ADD     1             XDTSTA                         *
019900891030     C                   Z-ADD     SIMDBV        XDTAMG                         *
020000891030     C                   EXSR      CNTDAT                                       *
020100891030     C     XDTSTA        IFLT      0                                            *
020200891030     C                   Z-ADD     999999        SIMDBV                         *
020300891030     C                   ELSE                                                   *
020400891030     C                   Z-ADD     XDTGMA        SIMDBV                         *
020500891030     C                   END                                                    *
020600891030     C                   END                                                    *
020700061113
020800061113     c                   eval      wforzaiva = *off
020900061113
021000891027     C*-                                                   *
021100890828     C                   ENDSR                                                  *
021200890828     C*----------------------------------------------------*
021300890828      /EJECT
021400890828     C*----------------------------------------------------*
021500890828     C*              CONTROLLO IMMISSIONE/VARIAZIONE       *
021600890828     C*----------------------------------------------------*
021700890828     C     CNTFMT        BEGSR                                                  *CONTROLLI
021800890828     C*-                                                   *
021900890828     C*-                                                   *
022000890828     C     SIMCDF        IFEQ      *BLANKS                                      *
022100890828     C     SIMIVA        ANDEQ     *BLANKS                                      *
022200890828     C                   SETON                                          3899    *
022300890828     C                   ELSE                                                   *
022400061113      * codice fiscale
022500061113     c                   if        simcdf <> *blanks
022600151218     c                   clear                   xcfiva1ds
022700061113     c                   eval      xcfivapar = simcdf
022800061113      * non imposto lo stato perchè è sempre stato italia
022900061113     c                   eval      xcfivaprv = simprv
023000151218     c                   eval      XCFIVACAP = %editc(simcap:'X')
023100151218     c                   eval      XCFIVALOC = simcit
023200151218     c                   eval      XCFIVAPIVA ='  ' +simiva
023300151218     c                   eval      XCFIVAtla  ='L'
023400151218     c                   call      'XCFIVAR1'
023500151218     c                   parm                    xcfiva1ds
023600061113     c                   if        xcfivaflg < *zeros
023700061113     c                   eval      *In30 = *On
023800061113     c                   eval      *In99 = *On
023900061113     c                   leavesr
024000061113     c                   endif
024100061113     c                   endif
024200061113      * partita iva
024300890828     C     SIMIVA        IFNE      *BLANKS                                      *
024400151218     c                   clear                   xcfiva1ds
024500061113     c                   eval      xcfivamod = 'P'
024600061113      * imposto i primi due a blank della partita iva per dire iso italia
024700061113     c                   eval      xcfivapar = '  ' + simiva
024800061113      * non imposto lo stato perchè è sempre stato italia
024900061113     c                   eval      xcfivaprv = simprv
025000151218     c                   eval      xcfivaprv = simprv
025100151218     c                   eval      XCFIVACAP = %editc(simcap:'X')
025200151218     c                   eval      XCFIVALOC = simcit
025300151218     c                   eval      XCFIVAtla  ='L'
025400151218     c                   call      'XCFIVAR1'
025500151218     c                   parm                    xcfiva1ds
025600061113      * --> errata forzabile
025700061113     c                   if        xcfivaflg = -9 and wforzaiva = *off
025800061113     c                   eval      *in43 = *on
025900061113     c                   eval      *in99 = *on
026000061113     c                   eval      wforzaiva = *on
026100061113     c                   leavesr
026200061113     c                   endif
026300061113      * --> errore
026400061113     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
026500061113     c                   eval      *in31 = *on
026600061113     c                   eval      *in99 = *on
026700061113     c                   leavesr
026800061113     c                   endif
026900890828     C                   END                                                    *
027000890828     C                   END                                                    *
027100890828     C*-                                                   *
027200890828     C     SIMDTI        IFNE      0                                            *
027300890828     C                   Z-ADD     3             XDTSTA                         *
027400890828     C                   Z-ADD     SIMDTI        XDTGMA                         *
027500890828     C                   EXSR      CNTDAT                                       *
027600890828     C     XDTSTA        IFLT      0                                            *
027700890828     C                   SETON                                          3799    *
027800890828     C                   END                                                    *
027900890828     C                   END                                                    *
028000890828     C*-                                                   *
028100890828     C     SIMDAP        IFNE      0                                            *
028200890828     C                   Z-ADD     3             XDTSTA                         *
028300890828     C                   Z-ADD     SIMDAP        XDTGMA                         *
028400890828     C                   EXSR      CNTDAT                                       *
028500890828     C     XDTSTA        IFLT      0                                            *
028600890828     C                   SETON                                          3599    *
028700890828     C                   END                                                    *
028800890828     C                   END                                                    *
028900890828     C*-                                                   *
029000890828     C     SIMDSA        IFNE      0                                            *
029100890828     C                   Z-ADD     3             XDTSTA                         *
029200890828     C                   Z-ADD     SIMDSA        XDTGMA                         *
029300890828     C                   EXSR      CNTDAT                                       *
029400890828     C     XDTSTA        IFLT      0                                            *
029500890828     C                   SETON                                          3699    *
029600890828     C                   END                                                    *
029700890828     C                   END                                                    *
029800890828     C*-                                                   *
029900021011     C*          SIMNGR    IFNE *BLANKS                    *
030000021011     C*                    MOVEL'NG'      TBLCOD           *           G
030100021011     C*                    MOVE *BLANKS   TBLKEY           *           G
030200021011     C*                    MOVELSIMNGR    TBLKEY           *           G
030300021011     C*          KEYTAB    CHAINTABEL00F             33    *
030400021011     C*  33                SETON                       99  *
030500021011     C*                    END                             *
030600890828     C*-                                                   *
030700891027     C     SIMFBV        IFEQ      'S'                                          *
030800891027     C     SIMNBV        ANDEQ     *BLANKS                                      *
030900891027     C                   SETON                                        5099
031000891027     C                   END
031100891027     C     SIMFBV        IFEQ      'S'                                          *
031200891027     C     SIMDBV        ANDEQ     0                                            *
031300891027     C                   SETON                                        5299
031400891027     C                   END
031500891027     C     SIMFBV        IFEQ      'S'                                          *
031600891027     C     SIMUBV        ANDEQ     *BLANKS                                      *
031700891027     C                   SETON                                        5399
031800891027     C                   END
031900891027     C*-                                                   *
032000891027     C     SIMDBV        IFNE      0                                            *
032100891027     C                   Z-ADD     3             XDTSTA                         *
032200891027     C                   Z-ADD     SIMDBV        XDTGMA                         *
032300891027     C                   EXSR      CNTDAT                                       *
032400891027     C     XDTSTA        IFLT      0                                            *
032500891027     C                   SETON                                          5199    *
032600891027     C                   END                                                    *
032700891027     C                   END                                                    *
032800891027     C*-                                                   *
032900891207     C     SIMFEL        IFNE      0                                            *
033000900605     C     SIMFEL        CHAIN     AZORG                              39        *
033100891207     C   39              SETON                                        99        *
033200891207     C                   END                                                    *
033300891207     C*-                                                   *
033400890828     C     EXCNT         TAG                                                    *
033500890828     C                   ENDSR                                                  *
033600890828     C*----------------------------------------------------*
033700890828      /EJECT
033800890828     C*----------------------------------------------------*
033900890828     C*              I M M I S S I O N E                   *
034000890828     C*----------------------------------------------------*
034100890828     C     IMMISS        BEGSR                                                  *
034200890828     C*-                                                   *
034300890828     C                   Z-ADD     3             XDTSTA                         *
034400890828     C                   Z-ADD     SIMDAP        XDTGMA                         *
034500890828     C                   EXSR      CNTDAT                                       *
034600890828     C     XDTSTA        IFLT      0                                            *
034700890828     C                   SETON                                          3599    *
034800890828     C                   ELSE                                                   *
034900890828     C                   Z-ADD     XDTAMG        SIMDAP                         *
035000890828     C                   END                                                    *
035100890828     C*-                                                   *
035200890828     C                   Z-ADD     3             XDTSTA                         *
035300890828     C                   Z-ADD     SIMDSA        XDTGMA                         *
035400890828     C                   EXSR      CNTDAT                                       *
035500890828     C     XDTSTA        IFLT      0                                            *
035600890828     C                   SETON                                          3699    *
035700890828     C                   ELSE                                                   *
035800890828     C                   Z-ADD     XDTAMG        SIMDSA                         *
035900890828     C                   END                                                    *
036000890828     C*-                                                   *
036100891027     C     SIMDBV        IFNE      0
036200891027     C                   Z-ADD     3             XDTSTA                         *
036300891027     C                   Z-ADD     SIMDBV        XDTGMA                         *
036400891027     C                   EXSR      CNTDAT                                       *
036500891027     C     XDTSTA        IFLT      0                                            *
036600891027     C                   SETON                                          5199    *
036700891027     C                   ELSE                                                   *
036800891027     C                   Z-ADD     XDTAMG        SIMDBV                         *
036900891027     C                   END                                                    *
037000891027     C                   END
037100891027     C*-                                                   *
037200890828     C  N99              DO                                                     *
037300890828     C*-                                                   *
037400890828     C*-TIPO RECORD "40"-                                  *
037500890828     C                   MOVE      *BLANKS       TRB02                          *
037600890828     C                   MOVE      *BLANKS       ATB02                          *
037700890828     C                   Z-ADD     KNRAZ         NRJOB                          *
037800890828     C                   Z-ADD     40            PARTIP                         *
037900890828     C                   MOVEL     CNCR40        WPAR                           *
038000890828     C                   WRITE     PARUT                                98      * 98 ERR I
038100890828     C     *IN98         IFNE      '1'                                          *
038200890828     C*-                                                   *
038300890828     C*-TIPO RECORD "41"-                                  *
038400890828     C                   Z-ADD     41            PARTIP                         *
038500890828     C                   MOVEL     CNCR41        WPAR                           *
038600890828     C                   WRITE     PARUT                                98      * 98 ERR I
038700890828     C     *IN98         IFNE      '1'                                          *
038800890828     C*-                                                   *
038900890828     C*-TIPO RECORD "42"-                                  *
039000890828     C                   Z-ADD     42            PARTIP                         *
039100890828     C                   MOVEL     CNCR42        WPAR                           *
039200890828     C                   WRITE     PARUT                                98      * 98 ERR I
039300890828     C*-                                                   *
039400891027     C*-TIPO RECORD "80"-                                  *
039500891027     C                   Z-ADD     80            PARTIP                         *
039600891027     C                   MOVEL     CNCR80        WPAR                           *
039700891027     C                   WRITE     PARUT                                98      * 98 ERR I
039800891027     C*-                                                   *
039900890828     C                   END                                                    *
040000890828     C                   END                                                    *
040100890828     C*-                                                   *
040200890828     C                   END                                                    *
040300890828     C*-                                                   *
040400890828     C                   ENDSR                                                  *
040500890828     C*----------------------------------------------------*
040600890828      /EJECT
040700890828     C*----------------------------------------------------*
040800890828     C*              V A R I A Z I O N E                   *
040900890828     C*----------------------------------------------------*
041000890828     C     VARIAZ        BEGSR                                                  *
041100890828     C*-                                                   *
041200890828     C                   Z-ADD     3             XDTSTA                         *
041300890828     C                   Z-ADD     SIMDAP        XDTGMA                         *
041400890828     C                   EXSR      CNTDAT                                       *
041500890828     C     XDTSTA        IFLT      0                                            *
041600890828     C                   SETON                                          3599    *
041700890828     C                   ELSE                                                   *
041800890828     C                   Z-ADD     XDTAMG        SIMDAP                         *
041900890828     C                   END                                                    *
042000890828     C*-                                                   *
042100890828     C                   Z-ADD     3             XDTSTA                         *
042200890828     C                   Z-ADD     SIMDSA        XDTGMA                         *
042300890828     C                   EXSR      CNTDAT                                       *
042400890828     C     XDTSTA        IFLT      0                                            *
042500890828     C                   SETON                                          3699    *
042600890828     C                   ELSE                                                   *
042700890828     C                   Z-ADD     XDTAMG        SIMDSA                         *
042800890828     C                   END                                                    *
042900890828     C*-                                                   *
043000891027     C     SIMDBV        IFNE      0
043100891027     C                   Z-ADD     3             XDTSTA                         *
043200891027     C                   Z-ADD     SIMDBV        XDTGMA                         *
043300891027     C                   EXSR      CNTDAT                                       *
043400891027     C     XDTSTA        IFLT      0                                            *
043500891027     C                   SETON                                          5199    *
043600891027     C                   ELSE                                                   *
043700891027     C                   Z-ADD     XDTAMG        SIMDBV                         *
043800891027     C                   END                                                    *
043900891027     C                   END
044000891207     C*-                                                   *
044100891207     C     SIMFEL        IFNE      0                                            *
044200900605     C     SIMFEL        CHAIN     AZORG                              39        *
044300891207     C   39              SETON                                        99        *
044400891207     C                   END                                                    *
044500891207     C*-                                                   *
044600890828     C  N99              DO                                                     *
044700890828     C*-                                                   *
044800890828     C*-TIPO RECORD "40"-                                  *
044900890828     C                   Z-ADD     KNRAZ         NRJOB                          *
045000890828     C                   Z-ADD     40            PARTIP                         *
045100890828     C     KPARUT        CHAIN     PARUT                              98        *
045200890828     C  N98              DO                                                     *VARIAZION
045300890828     C                   MOVEL     CNCR40        WPAR                           *
045400890828     C                   UPDATE    PARUT                                98      * 99 ERR I
045500890828     C                   END                                                    *
045600890828     C*-                                                   *
045700890828     C*-TIPO RECORD "41"-                                  *
045800890828     C     *IN98         IFNE      '1'                                          *
045900890828     C                   Z-ADD     41            PARTIP                         *
046000890828     C     KPARUT        CHAIN     PARUT                              98        *
046100890828     C  N98              DO                                                     *VARIAZION
046200890828     C                   MOVEL     CNCR41        WPAR                           *
046300890828     C                   UPDATE    PARUT                                98      * 99 ERR I
046400890828     C                   END                                                    *
046500891027     C*-                                                   *
046600891027     C*-TIPO RECORD "42"-                                  *
046700890828     C     *IN98         IFNE      '1'                                          *
046800890828     C                   Z-ADD     42            PARTIP                         *
046900890828     C     KPARUT        CHAIN     PARUT                              98        *
047000890828     C  N98              DO                                                     *VARIAZION
047100890828     C                   MOVEL     CNCR42        WPAR                           *
047200890828     C                   UPDATE    PARUT                                98      * 99 ERR I
047300890828     C                   END                                                    *
047400891027     C*-TIPO RECORD "80"-                                  *
047500891027     C     *IN98         IFNE      '1'                                          *
047600891027     C                   Z-ADD     80            PARTIP                         *
047700891027     C     KPARUT        CHAIN     PARUT                              98        *
047800891027     C                   DO                                                     *VARIAZION
047900891027     C                   MOVEL     CNCR80        WPAR                           *
048000891027     C  N80              UPDATE    PARUT                                98      * 99 ERR I
048100891027     C   80              WRITE     PARUT                                98
048200891027     C                   END                                                    *
048300890828     C                   END                                                    *
048400890828     C                   END                                                    *
048500891027     C                   END
048600890828     C*-                                                   *
048700890828     C                   END                                                    *
048800890828     C*-                                                   *
048900890828     C                   ENDSR                                                  *
049000890828     C*----------------------------------------------------*
049100890828      /EJECT
049200890828     C*----------------------------------------------------*
049300890828     C*                CONTROLLO DATA                      *
049400890828     C*----------------------------------------------------*
049500890828     C     CNTDAT        BEGSR                                                  *
049600890828     C*-                                                   *
049700890828     C     XDTSTA        CABGT     4             XDTERR                         *
049800890828     C     XDTSTA        CABLT     1             XDTERR                         *
049900890828     C     XDTSTA        CABGT     2             XDTEI                          *
050000890828     C*    GIRA DATA INTERNA IN ESTERNA                    *
050100890828     C     XDTAMG        DIV       100           XDTMM             2 0          * M-->XDTM
050200890828     C     XDTAMG        DIV       10000         XDTAA             2 0          * A-->XDTA
050300890828     C                   MOVE      XDTAMG        XDTGG             2 0          * G-->XDTG
050400890828     C                   MOVE      XDTAMG        XDTGMA                         * M--> XDT
050500890828     C                   MOVEL     XDTGG         XDTGMA            6 0          * G--> XDT
050600890828     C                   MOVE      XDTAA         XDTGMA                         * A--> XDT
050700890828     C                   GOTO      XDTCON                                       *
050800890828     C     XDTEI         TAG                                                    *
050900890828     C*    GIRA DATA ESTERNA IN INTERNA                    *
051000890828     C     XDTGMA        DIV       100           XDTMM                          * M--> XDT
051100890828     C     XDTGMA        DIV       10000         XDTGG                          * G--> XDT
051200890828     C                   MOVE      XDTGMA        XDTAA                          * A--> XDT
051300890828     C                   MOVE      XDTGMA        XDTAMG            6 0          * M--> XDT
051400890828     C                   MOVEL     XDTAA         XDTAMG                         * A--> XDT
051500890828     C                   MOVE      XDTGG         XDTAMG                         * G--> XDT
051600890828     C                   SUB       2             XDTSTA            1 0          *PREPARA X
051700890828     C*                                                    * PER TEST CONT
051800890828     C     XDTCON        TAG                                                    *
051900890828     C     XDTSTA        CABNE     1             XDTOK                          * XDTSTA N
052000890828     C*                                                    *SALTA CONTROL
052100890828     C*                                                    *
052200890828     C*    CONTROLLA LA VALIDITA* DELLA DATA               *
052300890828     C     XDTGG         CABLT     1             XDTERR                         *
052400890828     C     XDTMM         CABLT     1             XDTERR                         *
052500890828     C     XDTMM         CABGT     12            XDTERR                         *
052600890828     C     XDTAA         DIV       4             XDTRES            2 0          * INDIVIDU
052700890828     C                   MVR                     XDTRES                         * ANNO BIS
052800890828     C     XDTRES        IFEQ      0                                            *
052900890828     C                   Z-ADD     29            XDTF(2)                        * AGGIUSTA
053000890828     C                   ELSE                                                   * FINE FEB
053100890828     C                   Z-ADD     28            XDTF(2)                        * IN ARRAY
053200890828     C                   END                                                    *
053300890828     C     XDTGG         CABGT     XDTF(XDTMM)   XDTERR                         * GIORNO D
053400890828     C*                                                    * FINE MESE
053500890828     C     XDTOK         TAG                                                    *
053600890828     C*    FINE NORMALE                                    *
053700890828     C                   SUB       XDTSTA        XDTSTA                         *
053800890828     C                   GOTO      XDTFIN                                       *
053900890828     C     XDTERR        TAG                                                    *
054000890828     C*    DATA O RICHIESTA ERRATA                         *
054100890828     C                   Z-SUB     1             XDTSTA                         *
054200890828     C     XDTFIN        TAG                                                    *
054300890828     C*-                                                   *
054400890828     C                   ENDSR                                                  *
054500890828     C*----------------------------------------------------*
054600890828**
054700890828312831303130313130313031
