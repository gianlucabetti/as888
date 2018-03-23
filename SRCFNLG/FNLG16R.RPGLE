000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200060404     H* Fnlg16R *----------------------------------------------------*
000300900418     H*                                                              *
000400911107     H*             - CONFERMA CHIUSURA GIACENZE -                   *
000500900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000600900418     H*  21 IND. COMODO                                              *
000700900418     H*                                                              *
000800900418     H*--------------------------------------------------------------*
000900060404     FFNLG16D   CF   E             WORKSTN
001000060404     Ftigcp01l  IF   E           K DISK
001100050216     Ftigcp02l  IF   E           K DISK
001200060404     F                                     RENAME(tigcp000:tigcp002)
001300060405     Ffnblp01l  IF   E           K DISK
001400911010     FTABEL00F  IF   E           K DISK
001500941010      *--------------------------------------------------------------*
001600941011     D FL£7            S              3    DIM(29)
001700020503     D £1              S              3  0 DIM(30)
001800941011     D £7              S              3  0 DIM(29)
001900050217     D dd              S              2  0
002000060404     D ERR             S             70    DIM(6) CTDATA PERRCD(1)
002100941010      *--------------------------------------------------------------*
002200900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002300900418     D CNCR80        E DS
002400060405     D ds3a          E DS
002500060404     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
002600161118     D*PARAM0          DS
002700161118     D* PA0AAS                 1      4  0
002800161118     D* PA0LNP                 5      7  0
002900161118     D* PA0NRS                 8      9  0
003000161118     D* PA0NSP                10     16  0
003100161118     D* PA0FLG                18     18
003200161118     D* PA0SIM                44     46  0
003300161118     D* PA0F03                48     48
003301161118     d fnlru6ds      e ds
003302161118      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
003303161118     D PARAMu6ds       DS
003304161118     d* CAMPI DI OUTPUT:
003305161118     d* . Key spedizione
003306161118     D  pa1AAS                14     17  0
003307161118     D  pa1LNP                18     20  0
003308161118     D  pa1NRS                21     22  0
003309161118     D  pa1NSP                23     29  0
003310161118     d* . Flag '1'=Premuto F3=Fine
003311161118     D  PA1F03                30     30
003312161118     d* CAMPI DI INPUT:
003313161118     d* . Modalità di richiamo
003314161118     D  PA1FLG                31     31
003315161118     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
003316161118     d*                                in interrogazione della bolla ed evitare
003317161118     d*                                errore di chiamata ricorsiva
003318161118     D  PA1GIA               144    144
003319161118     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
003320161118     d  pa1rma               149    163
003321161118     d  pa1xco               164    164
003322161118     d* non utilizzato
003323161118     d  pa1flo               165    256
003400020503     d trul06ds      e ds
003500020503     d  lin                    1     90  0 dim(30)
003600900418     D KPJBA         E DS
003700941011     D FNLG82        E DS                  EXTNAME(FNLG82DS)
003800060404     D FNLG30        E DS                  EXTNAME(FNLG30DS)
003900941011     D*
004000060405     D savkpjbu        S                   like(kpjbu)
004100020730     D §FLE            S              3  0
004200920113     D                 DS
004300920113     D  GCPMGC                 1      4  0
004400920113     D  MESE                   1      2  0
004500920113     D  GIOR                   3      4  0
004600911017     C*---------------------------------------------------------------*
004700941011     C                   MOVEL     '1'           WTPVID            1
004800060404     C                   MOVEL     '1'           vsel
004900060405     C                   Z-ADD     0             VFGC
005000060801     C                   move      *year         VAGC
005100060405     C                   Z-ADD     0             VNGC
005200941011     C*
005300911010     C     *INKC         DOWEQ     '0'
005400941011     C*
005500941011     C     WTPVID        CASEQ     '1'           GESD01
005600941011     C     WTPVID        CASEQ     '3'           GESD03
005700941011     C                   END
005800941011     C*
005900941011     C                   END
006000941011     C*
006100941011     C                   SETON                                        LR
006200941011     C*--------------------------------------------------------------*
006300941011     C*  GESTIONE DELLA PRIMA VIDEATA
006400941011     C*--------------------------------------------------------------*
006500941011     C     GESD01        BEGSR
006600941011     C*
006700060404     C                   EXFMT     LG16D01
006800950131     C*
006900950131     C                   MOVEL     VAGC          WAA2              2 0
007000950131     C     WAA2          IFEQ      0
007100950131     C     VAGC          IFGT      60
007200950131     C                   ADD       1900          VAGC
007300950131     C                   ELSE
007400950131     C                   ADD       2000          VAGC
007500950131     C                   END
007600950131     C                   END
007700941011     C*
007800941011     C* Fine Elaborazione
007900941011     C     *INKC         IFEQ      '1'
008000941011     C                   GOTO      FINVD1
008100941011     C                   END
008200060404     C* Fine Elaborazione
008300060404     C     *INKs         IFEQ      '1'
008400060404     c                   move      '3'           wtpvid
008500060404     C                   exsr      gesd03
008600060404     C                   END
008700941011     C* Interrogazione giacenze
008800060801     C     *INKn         IFEQ      '1'
008900950102     C                   CLEAR                   FNLG30
009000950102     C                   MOVEL     'P'           D30POA
009100950102     C                   Z-ADD     VAGC          D30AGC
009200950102     C                   Z-ADD     VFGC          D30FGC
009300950102     C                   Z-ADD     VNGC          D30NGC
009400950102     C                   MOVE      *ZEROS        D30FRG
009500950102     C                   MOVEL     FNLG30        KPJBU
009600950102     C                   CALL      'FNLG30C'
009700950102     C                   PARM                    KPJBA
009800941011     C                   GOTO      FINVD1
009900920826     C                   END
010000060404      * INTERROGAZ. BOLLE PARTENZE
010100060801     C     *INKg         IFEQ      '1'
010200161118     C*                  CLEAR                   PARAM0
010300161118     C*                  MOVEL     PARAM0        KPJBU
010400161118     C*                  CALL      'FNLS04R'
010500161118     C*                  PARM                    KPJBA
010600161118     C*                  MOVEL     KPJBU         PARAM0
010601161118     C                   CLEAR                   PARAmu6ds
010602161118     c                   clear                   fnlru6ds
010603161118     C                   MOVEL     '4'           Pa1FLG
010604161118     C                   MOVEL     PARAmu6ds     KPJBU
010606161118     c                   eval      ilru6lp1=§fle
010610161118     C                   CALL      'FNLRU6R'
010611161118     C                   PARM                    KPJBA
010612161118     c                   parm                    fnlru6ds
010700060404     C                   END
010800060405     c                   exsr      contr01
010900060405     C* variazione assegnazione spese/e cambio di porto
011000060405     c                   if        not *in90 and *inkf
011100060405     C                   MOVEL(p)  kpjbu         savKPJBU
011200060405     C                   MOVEL     'P'           D82GES
011300060405     c                   move      'S'           d82gia
011400060405     C                   MOVE      gcpAGC        D82AGC
011500060405     C                   MOVE      gcpFGC        D82FGC
011600060405     C                   MOVE      gcpNGC        D82NGC
011700060405     C                   MOVE      gcpfrg        D82frg
011800060405     C                   MOVE      'S'           D82comm
011900060405     C                   MOVE      'S'           D82fspe
012000060405     C                   MOVEL(p)  FNLG82        KPJBU
012100060405     C                   CALL      'FNLG10R'
012200060405     C                   PARM                    KPJBA
012300060405     C                   MOVEL     KPJBU         FNLG82
012400060405     C                   MOVEL(p)  savkpjbu      KPJBU
012500060405     c                   endif
012600941011     C*
012700941011     C     FINVD1        ENDSR
012800060405     C*----------------------------------------------------------*
012900060405     C* Controllo prima videata
013000060405     C*----------------------------------------------------------*
013100060405     C     contr01       BEGSR
013200060405     C                   SETOFF                                       404190
013300060405     c                   if        vsel = '1'
013400060405     c                   select
013500060405     c                   when      vngc = 0 or vagc = 0 or vfgc = 0
013600060405     c                   seton                                        4090
013700060405     c                   movel     err(1)        $msg
013800060405     c                   when      vngc <> 0 and vagc <> 0 and vfgc <> 0
013900060405     c     kgia          chain     tigcp02l
014000060405     c                   if        %found(tigcp02l)
014100060405     C     §FLE          COMP      GCPLNP                             4040
014200060405     C   40gcplnp        lookup    £7                                     42
014300060405     c   40
014400060405     can 42              setoff                                       40
014500060405     c   40              seton                                        90
014600060405     c   40              movel     err(2)        $msg
014700060405     c                   else
014800060405     c                   seton                                        4090
014900060405     c                   movel     err(4)        $msg
015000060405     c                   endif
015100060405     c                   endsl
015200060405      *
015300060405     c                   else
015400060405      *spedizione
015500060405     c                   select
015600060405     c                   when      vnsp = 0 or vlnp = 0
015700060405     c                   seton                                        4090
015800060405     c                   movel     err(3)        $msg
015900060405     c                   when      vnsp <> 0 and vlnp <> 0
016000060405     c     kgia1         chain     tigcp01l
016100060405     c                   if        %found(tigcp01l)
016200060405     C     §FLE          COMP      GCPLNP                             4040
016300060405     c   40              seton                                        90
016400060405     c   40              movel     err(2)        $msg
016500060405     c                   else
016600060405     c                   seton                                        4090
016700060405     c                   movel     err(4)        $msg
016800060405     c                   endif
016900060405     c                   end
017000060405      *
017100060405     c                   endif
017200060405      * reperisce porto dalla bolla originale
017300060405     c     kbol          chain     fnblp01l
017400060405     c                   if        %found(fnblp01l)
017500060405     C                   movel     '3A'          cod
017600060405     C                   movel     *blank        key
017700060405     C                   movel     blpcbo        key
017800060405     C     ktab          chain     tabel                              21
017900060405     C  N21              movel     tbluni        DS3A
018000060405     c* porto della bolla
018100060405     C  N21              movel     §3atb1        exporto           1
018200060405     c                   endif
018300060405      * fase non congruente
018400060405     c                   if        gcpfas < 15 or gcpfas > 35
018500060405     c                   seton                                        90
018600060405     c                   movel(p)  err(5)        $msg
018700060405     c                   endif
018800060405      * fase non congruente se nuovo porto assegnato e disposizioni eseguite
018900060405     c                   if        gcpnpc = 'A' and  gcpfas > 31
019000060405     c                   seton                                        90
019100060405     c                   movel(p)  err(5)        $msg
019200060405     c                   endif
019300060405
019400060405      * fase non congruente se porto assegnato no nuovo porto e disposizioni ese
019500060405     c                   if        exporto ='A' and gcpnpc = ' '
019600060405     c                             and gcpfas > 31
019700060405     c                   seton                                        90
019800060405     c                   movel(p)  err(5)        $msg
019900060405     c                   endif
020000060405      * fase non congruente se provvigioni al destinatario e dispo. eseguite
020100060405     c                   if        gcpppc = 'D' and  gcpfas > 31
020200060405     c                   seton                                        90
020300060405     c                   movel(p)  err(5)        $msg
020400060405     c                   endif
020500060405
020600060405      * fase non congruente se spese al destinatario e dispo. eseguite
020700060405     c                   if        gcppsg = 'D' and  gcpfas > 31
020800060405     c                   seton                                        90
020900060405     c                   movel(p)  err(5)        $msg
021000060405     c                   endif
021100060405
021200060405     C                   endsr
021300941011     C*--------------------------------------------------------------*
021400941011     C*  GESTIONE DELLA TERZA VIDEATA
021500941011     C*--------------------------------------------------------------*
021600941011     C     GESD03        BEGSR
021700941011     C*
021800060404     C                   EXFMT     LG16D03
021900060405     C                   SETOFF                                       90
022000941011     C* Ritorno
022100941011     C     *INKL         IFEQ      '1'
022200060404     C                   MOVEL     '1'           WTPVID
022300941011     C                   GOTO      FINVD3
022400941011     C                   END
022500941011     C*
022600941011     C     §FLE          IFNE      0
022700941011     C     §FLE          LOOKUP    £1                                     32
022800941011     C                   END
022900941011     C*
023000941011     C     *IN32         IFEQ      '0'
023100060405     C                   SETON                                        90
023200060405     C                   MOVEL     ERR(6)        $MSG
023300941011     C                   ELSE
023400941011     C                   EXSR      CAR£7
023500060404     C                   MOVEL     '1'           WTPVID
023600941011     C                   END
023700941011     C*
023800941011     C     FINVD3        ENDSR
023900941011     C*----------------------------------------------------------*
024000941011     C* Caricamento tabella £7
024100941011     C*----------------------------------------------------------*
024200941011     C     CAR£7         BEGSR
024300941011     C* Caricamento £7
024400941011     C                   MOVEA     *ZEROS        FL£7
024500941011     C                   MOVEL     *BLANKS       KEY
024600941011     C                   MOVEL     §FLE          KEY
024700941011     C                   MOVEL     '£7'          COD
024800941011     C     KTAB          CHAIN     TABEL                              31
024900941011     C     *IN31         IFEQ      '0'
025000941011     C     TBLFLG        ANDEQ     ' '
025100941011     C                   MOVEA     TBLUNI        FL£7
025200941011     C                   ELSE
025300941011     C                   MOVE      §FLE          FL£7(1)
025400941011     C                   END
025500941011     C*
025600941011     C                   Z-ADD     0             £7
025700941011     C                   Z-ADD     0             X£7               2 0
025800941011     C                   DO        29            X                 3 0
025900941011     C     FL£7(X)       IFNE      *BLANKS
026000941011     C                   ADD       1             X£7               2 0
026100941011     C                   MOVE      FL£7(X)       £7(X£7)
026200941011     C                   END
026300941011     C                   END
026400941011     C*
026500941011     C                   ENDSR
026600911017     C*---------------*
026700941010     C*  OPERAZIONI INIZIALI
026800911017     C*---------------*
026900941010     C     *INZSR        BEGSR
027000941010     C*
027100900418     C     *ENTRY        PLIST
027200900418     C                   PARM                    KPJBA
027300941229     C                   MOVEL     KPJBU         FNLG82
027400900418     C                   Z-ADD     1             CODUT
027500900418     C                   CALL      'X§PARUT'
027600900418     C                   PARM                    UTEDSE
027700941011     C                   MOVEL     'P'           D82GES
027800941011     C*
027900900418     C                   MOVEL     RAGUT         RSUT             20
028000900418     C                   MOVEL     RAGUT         RSUT2            25
028100900502     C                   MOVEL     RAGUT         PARDUT           30
028200911028     C                   MOVEL     REC80         CNCR80
028300020730     C     SimTpP        COMP      '2'                                    01
028400020812     C                   Z-ADD     SIMPOU        §FLE
028500900419     C*                                                    *
028600900418     C*  KLIST
028700911010     C     KTAB          KLIST
028800911010     C                   KFLD                    CODUT
028900911010     C                   KFLD                    COD               2
029000911010     C                   KFLD                    KEY               8
029100060329     C*
029200060404     C     KGia          KLIST
029300911106     C                   KFLD                    VAGC
029400911106     C                   KFLD                    VFGC
029500911106     C                   KFLD                    VNGC
029600990628     C                   KFLD                    FRG               2 0
029700911107     C                   Z-ADD     0             FRG
029800060404     C     KGia1         KLIST
029900060404     C                   KFLD                    Vagc
030000060404     C                   KFLD                    Vlnp
030100060404     C                   KFLD                    Vnrs
030200060404     C                   KFLD                    Vnsp
030300060404     C                   KFLD                    fRG
030400060405     C     Kbol          KLIST
030500060405     C                   KFLD                    gcpaas
030600060405     C                   KFLD                    gcplnp
030700060405     C                   KFLD                    gcpnrs
030800060405     C                   KFLD                    gcpnsp
030900941011     C* Caricamento £1
031000020503     c                   clear                   trul06ds
031100020503     c                   eval      d06cod = '£1'
031200020503     c                   movel     §fle          d06key
031300020503     c                   movel     trul06ds      kpjbu
031400020503     c                   call      'TRUL06R'
031500020503     c                   parm                    kpjba
031600020503     c                   movel     kpjbu         trul06ds
031700020503     c                   movea     lin           £1
031800941011     C*  Caricamento tabella £7
031900941011     C                   EXSR      CAR£7
032000900418     C                   ENDSR
032100961114**
032200060404Se selezione 1 x giacenza inserire i dati completi !!
032300060404Giacenza non in gestione !!!
032400060404Se selezione 2 x spedizione inserire i dati completi !!!
032500060404Giacenza non trovata  !!!
032600060404Giacenza con fase non congruente per la modifica !!
032700060404Punto operativo errato
