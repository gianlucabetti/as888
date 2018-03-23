000100000000      ****************************************************************
000200970730      * GESTISCE PER PROVA TUTTE LA FUNZIONI ROUTINE 'TNSD99R'       *
000300970730      *     COME TNSD98R CON IN PIU' LA LOCALITA' DI DEST.           *
000400000000      ****************************************************************
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600070514     FTNTBE01L  IF   E           K DISK
000700000000     F* FILE VIDEO (SUBFILE DATI)
000800970730     FTNSD97D   CF   E             WORKSTN INFDS(DEVDS1)
000900970730     F                                     SFILE(SD97DMS:MSGULT)
001000000000     D*****************************************************************
001100000000     D* MESSAGGI
001200930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001300000000     D* DESCRIZIONI TASTI FUNZIONALI
001400000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001500000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
001600000000     D FUV             S             19    DIM(24)
001700000000     D*****************************************************************
001800000000     D* RIDEFINIZIONE SCHIERA -FUD-
001900000000     D FUDDS           DS
002000000000     D*                -DESCRIZIONE PARZIALE
002100000000     D  FUDDE1                 1      1
002200000000     D*                -FUNZIONE
002300000000     D  FUDFUN                 2      3
002400000000     D*                -DESCRIZIONE PARZIALE
002500000000     D  FUDDE2                 4     19
002600000000     D*                -FILLER
002700000000     D  FUDFI0                20     34
002800000000     D*                -FLAG FUNZ.ATTIVA (S/N)
002900000000     D  FUDATT                35     35
003000000000     D*                -FILLER
003100000000     D  FUDFI1                36     80
003200000000     D*                -DESCRIZIONE COMPLETA
003300000000     D  FUDDES                 1     19
003400000000     D*                -RIDEFINIZIONE RIGA
003500000000     D  FUDRIG                 1     80
003600930201     D*****************************************************************
003700930201     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003800930201     D DSMSG         E DS
003900930305     D*****************************************************************
004000970730     D* PARAMETRI CHIAMATA PGM TNSD99R (TEMPI DI DELIVERY)
004100970730     D DS99          E DS                  EXTNAME(TNSD99DS)
004200930201     D*****************************************************************
004300930201     D* DS PER GESTIONE TASTI FUNZIONALI
004400930201     D DSKEY         E DS
004500930201     D DEVDS1          DS
004600930201     D  KEY                  369    369
004700950407     D***************************************************************
004800950407     D* DS PER LANCIO PGM DATA->N.GIORNI (XSRDA8)
004900950407     D G02PAR          DS
005000950407     D  G02DAT                 1      8  0
005100950407     D  G02INV                 9     16  0
005200950407     D  G02ERR                17     17
005300950407     D  G02TGI                18     22  0
005400070514     D DS§TIM          DS
005500070514     D  DTINVdata              7     14  0
005600070514     D  DTORA6                 1      6  0
005700070514     D  DTTIME                 1     14  0
005800930201     D***************************************************************
005900930201     D* ARCHITETTURA
006000930201     D KPJBA         E DS
006100070514     D***************************************************************
006200070514     D* ARCHITETTURA
006300070514     D DVPODECO      E DS
006400000000     C*================================================================
006500000000     C* MAIN DEL PGM
006600000000     C                   EXSR      RUTINZ
006700000000     C*
006800000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
006900000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
007000000000     C*
007100000000     C* CICLO FINO A CHE:
007200000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007300000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
007400930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
007500000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
007600000000     C     F12L00        OREQ      '1'
007700930202     C     FLFINE        OREQ      '1'
007800000000     C*
007900000000     C* EMETTE FORMATO VIDEO
008000000000     C                   EXSR      BUTFMR
008100000000     C*
008200000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
008300000000     C                   EXSR      FUNCON
008400000000     C*
008500000000     C                   END                                                    <-- 2 --
008600000000     C*
008700000000     C                   END                                                    <-- 1 --
008800000000     C*
008900000000     C                   SETON                                        LR
009000000000     C*FINE MAIN DEL PGM
009100000000     C*================================================================
009200000000     C* EMISSIONE FORMATO A VIDEO
009300000000     C*================================================================
009400000000     C     BUTFMR        BEGSR
009500930204     C*
009600930204     C* WRITE RECORD TESTATA
009700970730     C                   WRITE     SD97D01
009800930205     C*
009900930205     C* VISUALIZZAZIONE MESSAGGIO
010000930205     C                   EXSR      SNDMSG
010100930204     C*
010200930204     C* WRITE RECORD FUNZIONI
010300150415     c                   setoff                                       03
010400150415     c   03
010500150415     Cann03              WRITE     SD97D02
010600000000     C*
010700930204     C* EXFMT RECORD DETTAGLIO
010800970730     C                   EXFMT     SD97D03
010900000000     C*
011000000000     C* AZZERO FLAG DI ERRORE LIVELLO
011100000000     C                   MOVEL     '0'           ERRL00
011200000000     C*
011300000000     C                   ENDSR
011400000000     C*================================================================
011500000000     C* SNDMSG - MANDA MESSAGGIO
011600000000     C*================================================================
011700000000     C     SNDMSG        BEGSR
011800000000     C*
011900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
012000000000     C                   MOVEL     '1'           *IN40
012100970730     C                   WRITE     SD97DMC
012200000000     C                   MOVEL     '0'           *IN40
012300000000     C*
012400000000     C* VISUALIZZAZIONE MESSAGGIO
012500000000     C                   Z-ADD     0             MSGULT            4 0
012600930202     C                   CALL      'XMSG'        XMSGPA
012700930201     C     DSMSER        IFEQ      0
012800930201     C                   MOVEL     DSMSKE        MSGKEY
012900000000     C                   END
013000000000     C                   Z-ADD     1             MSGULT
013100970730     C                   WRITE     SD97DMS                                      SFL MESS
013200970730     C                   WRITE     SD97DMC                                      CONTR MESS
013300000000     C*
013400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
013500930201     C                   MOVEL     *BLANKS       DSMSMS
013600000000     C*
013700000000     C                   ENDSR
013800000000     C*================================================================
013900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
014000000000     C*================================================================
014100000000     C     ENTGES        BEGSR
014200950407     C*
014300950407     C* CONTROLLA DATA INIZIO CONSEGNABILITA
014400950407     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
014500950407     C                   Z-ADD     D98DIC        G02INV
014600950407     C                   MOVEL     '3'           G02ERR
014700950407     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
014800950407     C                   PARM                    G02PAR
014900950407     C     G02ERR        IFEQ      '1'                                          --- 2 --> *NO OK
015000950407     C                   MOVEL     '1'           ERRL00
015100950407     C                   MOVEL     MSG(2)        DSMSMS
015200950407     C                   ELSE                                                   <-- 2 -->
015300950407     C                   Z-ADD     G02INV        D98DIC
015400950407     C                   ENDIF                                                  <-- 2 ---
015500950407     C                   ENDIF                                                  <-- 1 ---
015600950407     C*
015700950407     C* CONTROLLA ORA INZIO CONSEGNABILITA
015800950407     C     ERRL00        IFEQ      '0'                                          --- 1 -->
015900950407     C     D98OIC        ANDNE     *ZEROS
016000950407     C     D98OIC        IFGT      2359                                         --- 2 -->
016100950407     C                   MOVEL     '1'           ERRL00
016200950407     C                   MOVEL     MSG(4)        DSMSMS
016300950407     C                   ELSE                                                   <-- 2 -->
016400950407     C                   Z-ADD     D98OIC        WTM020            2 0
016500950407     C     WTM020        IFGT      59
016600950407     C                   MOVEL     '1'           ERRL00
016700950407     C                   MOVEL     MSG(4)        DSMSMS
016800950407     C                   ENDIF
016900950407     C                   ENDIF                                                  <-- 2 ---
017000950407     C                   ENDIF                                                  <-- 1 ---
017100950407     C*
017200950407     C* CONTROLLA DATA FINE CONSEGNABILITA
017300950407     C     ERRL00        IFEQ      '0'                                          --- 1 -->
017400950407     C     D98DFC        ANDNE     *ZEROS
017500950407     C                   Z-ADD     D98DFC        G02INV
017600950407     C                   MOVEL     '3'           G02ERR
017700950407     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
017800950407     C                   PARM                    G02PAR
017900950407     C     G02ERR        IFEQ      '1'                                          --- 2 --> *NO OK
018000950407     C                   MOVEL     '1'           ERRL00
018100950407     C                   MOVEL     MSG(3)        DSMSMS
018200950407     C                   ELSE                                                   <-- 2 -->
018300950407     C                   Z-ADD     G02INV        D98DFC
018400950407     C                   ENDIF                                                  <-- 2 ---
018500950407     C                   ENDIF                                                  <-- 1 ---
018600950407     C*
018700950407     C* CONTROLLA ORA FINE CONSEGNABILITA
018800950407     C     ERRL00        IFEQ      '0'                                          --- 1 -->
018900950407     C     D98OFC        ANDNE     *ZEROS
019000950407     C     D98OFC        IFGT      2359                                         --- 2 -->
019100950407     C                   MOVEL     '1'           ERRL00
019200950407     C                   MOVEL     MSG(5)        DSMSMS
019300950407     C                   ELSE                                                   <-- 2 -->
019400950407     C                   Z-ADD     D98OFC        WTM020            2 0
019500950407     C     WTM020        IFGT      59
019600950407     C                   MOVEL     '1'           ERRL00
019700950407     C                   MOVEL     MSG(5)        DSMSMS
019800950407     C                   ENDIF
019900950407     C                   ENDIF                                                  <-- 2 ---
020000950407     C                   ENDIF                                                  <-- 1 ---
020100970304     C*
020200970304     C* CONTROLLA DATA RIFERIMENTO
020300970304     C     ERRL00        IFEQ      '0'                                          --- 1 -->
020400970304     C     D98DRF        ANDNE     *ZEROS
020500970304     C                   Z-ADD     D98DRF        G02INV
020600970304     C                   MOVEL     '3'           G02ERR
020700970304     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
020800970304     C                   PARM                    G02PAR
020900970304     C     G02ERR        IFEQ      '1'                                          --- 2 --> *NO OK
021000970304     C                   MOVEL     '1'           ERRL00
021100970304     C                   MOVEL     MSG(6)        DSMSMS
021200970304     C                   ELSE                                                   <-- 2 -->
021300970304     C                   Z-ADD     G02INV        D98DRF
021400970304     C                   ENDIF                                                  <-- 2 ---
021500970304     C                   ENDIF                                                  <-- 1 ---
021600970304     C*
021700970304     C* CONTROLLA ORA RIFERIMENTO
021800970304     C     ERRL00        IFEQ      '0'                                          --- 1 -->
021900970304     C     D98ORF        ANDNE     *ZEROS
022000970304     C     D98ORF        IFGT      2359                                         --- 2 -->
022100970304     C                   MOVEL     '1'           ERRL00
022200970304     C                   MOVEL     MSG(7)        DSMSMS
022300970304     C                   ELSE                                                   <-- 2 -->
022400970304     C                   Z-ADD     D98ORF        WTM020            2 0
022500970304     C     WTM020        IFGT      59
022600970304     C                   MOVEL     '1'           ERRL00
022700970304     C                   MOVEL     MSG(7)        DSMSMS
022800970304     C                   ENDIF
022900970304     C                   ENDIF                                                  <-- 2 ---
023000970304     C                   ENDIF                                                  <-- 1 ---
023100930201     C*
023200930326     C* LANCIA ROUTINE
023300950407     C     ERRL00        IFEQ      '0'                                          --- 1 -->
023400970730     C                   CALL      'TNSD99R'
023500970730     C                   PARM                    DS99
023600070514     c*  Dopo il richiamo sistemo a video la data utilizzata per il calcolo
023700070514     c*   della affidabilità partenza
023800070514     c                   if        d98fdrt=' '
023900070514     c                   eval      decdbr='BORDERÒ'
024000070514     c                   setoff                                       01
024100070514     c                   else
024200070514     c                   eval      decdbr=' RITIRO'
024300070514     c                   seton                                        01
024400070514     c                   endif
024500070514     c
024600950407     C                   ENDIF                                                  <-- 1 ---
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*================================================================
025000930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
025100000000     C*================================================================
025200000000     C     F01GES        BEGSR
025300000000     C                   ENDSR
025400000000     C*================================================================
025500930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
025600000000     C*================================================================
025700000000     C     F03GES        BEGSR
025800000000     C*
025900930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
026000000000     C                   MOVEL     '1'           F03L00
026100000000     C*
026200000000     C                   ENDSR
026300930201     C*================================================================
026400930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
026500930201     C*================================================================
026600930201     C     F06GES        BEGSR
026700930201     C                   ENDSR
026800000000     C*================================================================
026900930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
027000000000     C*================================================================
027100000000     C     F07GES        BEGSR
027200000000     C                   ENDSR
027300000000     C*================================================================
027400930210     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
027500000000     C*================================================================
027600000000     C     F08GES        BEGSR
027700000000     C                   ENDSR
027800000000     C*================================================================
027900930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
028000000000     C*================================================================
028100000000     C     F12GES        BEGSR
028200000000     C*
028300000000     C* IMPOSTO FLAG DI LIVELLO
028400000000     C                   MOVEL     '1'           F12L00
028500000000     C*
028600000000     C                   ENDSR
028700930201     C*================================================================
028800930210     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ELENCO
028900930201     C*================================================================
029000930201     C     F21GES        BEGSR
029100930201     C                   ENDSR
029200000000     C*================================================================
029300930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
029400000000     C*================================================================
029500000000     C     F22GES        BEGSR
029600000000     C                   ENDSR
029700000000     C*================================================================
029800930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
029900000000     C*================================================================
030000000000     C     F24GES        BEGSR
030100000000     C*
030200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030300000000     C                   EXSR      FUNGES
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*================================================================
030700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
030800000000     C*================================================================
030900000000     C     HELGES        BEGSR
031000000000     C                   ENDSR
031100000000     C*================================================================
031200000000     C*  RUTINZ - ROUTINE INIZIALE
031300000000     C*================================================================
031400000000     C     RUTINZ        BEGSR
031500930401     C*
031600930401     C* RICEVIMENTO PARAMETRI
031700930401     C     *ENTRY        PLIST
031800930401     C                   PARM                    KPJBA
031900930201     C*
032000930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
032100930201     C     XMSGPA        PLIST
032200930201     C                   PARM                    DSMSMS
032300930201     C                   PARM                    DSMSPG
032400930201     C                   PARM                    DSMSKE
032500930201     C                   PARM                    DSMSER
032600930201     C*
032700930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
032800930201     C                   CALL      'XKEY'
032900930201     C                   PARM                    DSKEY
033000930201     C*
033100930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
033200930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
033300930201     C*
033400930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
033500930201     C*   -NOME PGM CHIAMANTE
033600930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
033700970730     C                   MOVEL     'TNSD97R'     DSMSPG
033800970730     C                   MOVEL     'TNSD97R'     MSGQUE
033900930201     C                   MOVEL     *BLANKS       MSGKEY
034000930201     C                   MOVEL     *BLANKS       DSMSMS
034100930201     C*
034200930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
034300930201     C                   Z-ADD     0             FUNGRU            5 0
034400930201     C*
034500930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
034600930201     C                   EXSR      FUNGES
034700930201     C*
034800930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
034900930201     C                   MOVEL     '0'           F12L00            1
035000930201     C                   MOVEL     '0'           F03L00            1
035100930201     C                   MOVEL     '0'           FINL00            1
035200930201     C                   MOVEL     '0'           ERRL00            1
035300930201     C                   MOVEL     '0'           FLFINE            1
035400070514     c
035500070514     c* Carico la tabella "VPO" record DECO
035600070514     C*
035700070514     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
035800070514     C                   MOVEL     'VPO'         TBECOD
035900070514     C                   MOVEL     *BLANKS       TBEKE2
036000070514     C                   MOVEL(P)  'DECO'        TBEKE1
036100070514     C     KTBE          CHAIN     TNTBE01L                           21
036200070514     C  N21              MOVEL     TBEUNI        DVPODECO
036300070514     c
036400070514     C* CHIAVE LETTURA TNTBE00F - PARZIALE
036500070514     C     KTBE          KLIST
036600070514     C                   KFLD                    TBECOD
036700070514     C                   KFLD                    TBEKE1
036800070514     C                   KFLD                    TBEKE2
036900070514     c*
037000070514     c* data di oggi
037100070514     C                   TIME                    DTTIME
037200070514     C                   Z-ADD     dtinvdata     G02INV
037300070514     C                   MOVEL     '3'           G02ERR
037400070514     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
037500070514     C                   PARM                    G02PAR
037600070514     C                   Z-ADD     G02INV        wdatoggi          8 0
037700930421     C*
037800930421     C* AZZERA CAMPI VIDEO PACKED
037900930421     C                   EXSR      AZZERA
038000930421     C*
038100930421     C                   ENDSR
038200930421     C*================================================================
038300930421     C* AZZERA - AZZERA CAMPI PACKED
038400930421     C*================================================================
038500930421     C     AZZERA        BEGSR
038600930305     C*
038700930305     C* INIZIALIZZO CAMPI PACKED
038800970730     C                   CLEAR                   DS99
038900930326     C                   Z-ADD     *ZEROS        D98AAS
039000930326     C                   Z-ADD     *ZEROS        D98LNP
039100930326     C                   Z-ADD     *ZEROS        D98NRS
039200930326     C                   Z-ADD     *ZEROS        D98NSP
039300931125     C                   Z-ADD     *ZEROS        D98DIC
039400931125     C                   Z-ADD     *ZEROS        D98OIC
039500931125     C                   Z-ADD     *ZEROS        D98DFC
039600931125     C                   Z-ADD     *ZEROS        D98OFC
039700970304     C                   Z-ADD     *ZEROS        D98DRF
039800970304     C                   Z-ADD     *ZEROS        D98ORF
039900931125     C                   Z-ADD     *ZEROS        D98TPI
040000931125     C                   Z-ADD     *ZEROS        D98TPC
040100930326     C                   Z-ADD     *ZEROS        D98TTI
040200930326     C                   Z-ADD     *ZEROS        D98TTC
040300930326     C                   Z-ADD     *ZEROS        D98TCI
040400930326     C                   Z-ADD     *ZEROS        D98TCC
040500930326     C                   Z-ADD     *ZEROS        D98TRI
040600930326     C                   Z-ADD     *ZEROS        D98TRC
040700931125     C                   Z-ADD     *ZEROS        D98RPI
040800931125     C                   Z-ADD     *ZEROS        D98RPC
040900930405     C                   Z-ADD     *ZEROS        D98RTI
041000930405     C                   Z-ADD     *ZEROS        D98RTC
041100930405     C                   Z-ADD     *ZEROS        D98RCI
041200930405     C                   Z-ADD     *ZEROS        D98RCC
041300930405     C                   Z-ADD     *ZEROS        D98RRI
041400930405     C                   Z-ADD     *ZEROS        D98RRC
041500931125     C                   Z-ADD     *ZEROS        D98FPI
041600931125     C                   Z-ADD     *ZEROS        D98FPC
041700930326     C                   Z-ADD     *ZEROS        D98FTI
041800930326     C                   Z-ADD     *ZEROS        D98FTC
041900930326     C                   Z-ADD     *ZEROS        D98FCI
042000930326     C                   Z-ADD     *ZEROS        D98FCC
042100930326     C                   Z-ADD     *ZEROS        D98FRI
042200930326     C                   Z-ADD     *ZEROS        D98FRC
042300931125     C                   Z-ADD     *ZEROS        D98NPI
042400931125     C                   Z-ADD     *ZEROS        D98NPC
042500930326     C                   Z-ADD     *ZEROS        D98NTI
042600930326     C                   Z-ADD     *ZEROS        D98NTC
042700930326     C                   Z-ADD     *ZEROS        D98NCI
042800930326     C                   Z-ADD     *ZEROS        D98NCC
042900930326     C                   Z-ADD     *ZEROS        D98NRI
043000930326     C                   Z-ADD     *ZEROS        D98NRC
043100931125     C                   Z-ADD     *ZEROS        D98HPI
043200931125     C                   Z-ADD     *ZEROS        D98HPC
043300930414     C                   Z-ADD     *ZEROS        D98HFI
043400930414     C                   Z-ADD     *ZEROS        D98HFC
043500930416     C                   Z-ADD     *ZEROS        D98HEI
043600930416     C                   Z-ADD     *ZEROS        D98HEC
043700930414     C                   Z-ADD     *ZEROS        D98HHI
043800930414     C                   Z-ADD     *ZEROS        D98HHC
043900060418     C                   Z-ADD     *ZEROS        D98Hegi
044000060418     C                   Z-ADD     *ZEROS        D98Hegc
044100930414     C                   Z-ADD     *ZEROS        D98HGI
044200930414     C                   Z-ADD     *ZEROS        D98HGC
044300930414     C                   Z-ADD     *ZEROS        D98HLI
044400930414     C                   Z-ADD     *ZEROS        D98HLC
044500930414     C                   Z-ADD     *ZEROS        D98HOI
044600930414     C                   Z-ADD     *ZEROS        D98HOC
044700930414     C                   Z-ADD     *ZEROS        D98HRI
044800930414     C                   Z-ADD     *ZEROS        D98HRC
044900930511     C                   Z-ADD     *ZEROS        D98HDI
045000930511     C                   Z-ADD     *ZEROS        D98HDC
045100930326     C                   Z-ADD     *ZEROS        D98TFP
045200930405     C                   Z-ADD     *ZEROS        D98TFA
045300930405     C                   Z-ADD     *ZEROS        D98LNA
045400930326     C                   Z-ADD     *ZEROS        D98DSP
045500930326     C                   Z-ADD     *ZEROS        D98DTI
045600930326     C                   Z-ADD     *ZEROS        D98HTI
045700930326     C                   Z-ADD     *ZEROS        D98DCM
045800140820     C                   Z-ADD     *ZEROS        D98ddc
045900140820     C                   clear                   D98cons
046000930326     C                   Z-ADD     *ZEROS        D98HMC
046100010822     C                   Z-ADD     *ZEROS        D98CAS
046200930326     C                   Z-ADD     *ZEROS        D98DCR
046300930326     C                   Z-ADD     *ZEROS        D98HCR
046400930326     C                   Z-ADD     *ZEROS        D98DAG
046500930326     C                   Z-ADD     *ZEROS        D98DCG
046600930326     C                   Z-ADD     *ZEROS        D98DCI
046700930326     C                   Z-ADD     *ZEROS        D98HCI
046800930326     C                   Z-ADD     *ZEROS        D98DCE
046900930326     C                   Z-ADD     *ZEROS        D98HCE
047000930514     C                   Z-ADD     *ZEROS        D98NCD
047100931125     C                   Z-ADD     *ZEROS        D98DFV
047200941228     C                   Z-ADD     *ZEROS        D98DUC
047300931125     C                   Z-ADD     *ZEROS        D98DAM
047400960402     C                   Z-ADD     *ZEROS        D98DBR
047500931125     C                   Z-ADD     *ZEROS        D98OCI
047600931125     C                   Z-ADD     *ZEROS        D98OCC
047700931202     C                   Z-ADD     *ZEROS        D98DEI
047800931202     C                   Z-ADD     *ZEROS        D98OEI
047900931202     C                   Z-ADD     *ZEROS        D98DEE
048000931202     C                   Z-ADD     *ZEROS        D98OEE
048100940627     C                   Z-ADD     *ZEROS        D98KSC
048200070514     c                   clear                   d98fdrt
048300070514     c                   eval      decdbr=' RITIRO'
048400070514     c                   seton                                        01
048500140820     c
048600140820     c                   eval      d98vca='EUR'
048700000000     C*
048800000000     C                   ENDSR
048900000000     C*================================================================
049000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
049100000000     C*================================================================
049200000000     C     FUNGES        BEGSR
049300000000     C*
049400000000     C* PULISCO CAMPI DI LAVORO
049500000000     C                   Z-ADD     0             W                 5 0
049600000000     C*
049700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
049800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
049900000000     C     1             DO        24            I
050000000000     C                   MOVEL     FUD(I)        FUDRIG
050100000000     C*
050200000000     C* PULIZIA FUNZIONI PRECEDENTI
050300000000     C                   MOVEL     *BLANKS       FUV(I)
050400000000     C*
050500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
050600000000     C* (SALTA F24)
050700000000     C     FUDATT        IFEQ      'S'
050800000000     C     FUDFUN        ANDNE     '24'
050900000000     C                   ADD       1             W
051000000000     C                   MOVEL     FUDDES        FUV(W)
051100000000     C                   END
051200000000     C                   END                                                    DO WHILE
051300000000     C*
051400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
051500000000     C* ALTRIMENTI DISATTIVO F24
051600000000     C     1             DO        24            I
051700000000     C                   MOVEL     FUD(I)        FUDRIG
051800000000     C*
051900000000     C     FUDFUN        IFEQ      '24'
052000000000     C     W             IFGT      6
052100000000     C                   MOVEL     'S'           FUDATT
052200000000     C                   ELSE
052300000000     C                   MOVEL     *BLANKS       FUDATT
052400000000     C                   END                                                    W GT 8
052500000000     C                   MOVEL     FUDRIG        FUD(I)
052600000000     C                   END
052700000000     C                   END                                                    DO WHILE
052800000000     C*
052900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
053000000000     C     W             DIV       6             I
053100000000     C                   MVR                     Z
053200000000     C*
053300000000     C     Z             IFNE      0
053400000000     C     I             ADD       1             Z
053500000000     C                   ELSE
053600000000     C                   Z-ADD     I             Z                 5 0
053700000000     C                   END
053800000000     C*
053900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
054000000000     C                   ADD       1             FUNGRU
054100000000     C     FUNGRU        IFGT      Z
054200000000     C     FUNGRU        ORLT      1
054300000000     C                   Z-ADD     1             FUNGRU
054400000000     C                   END
054500000000     C*
054600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
054700000000     C     FUNGRU        IFEQ      1
054800000000     C                   MOVEL     FUV(1)        FUN001
054900000000     C                   MOVEL     FUV(2)        FUN002
055000000000     C                   MOVEL     FUV(3)        FUN003
055100000000     C                   MOVEL     FUV(4)        FUN004
055200000000     C                   MOVEL     FUV(5)        FUN005
055300000000     C                   MOVEL     FUV(6)        FUN006
055400000000     C                   END
055500000000     C*
055600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
055700000000     C     FUNGRU        IFEQ      2
055800000000     C                   MOVEL     FUV(7)        FUN001
055900000000     C                   MOVEL     FUV(8)        FUN002
056000000000     C                   MOVEL     FUV(9)        FUN003
056100000000     C                   MOVEL     FUV(10)       FUN004
056200000000     C                   MOVEL     FUV(11)       FUN005
056300000000     C                   MOVEL     FUV(12)       FUN006
056400000000     C                   END
056500000000     C*
056600000000     C* TERZO GRUPPO DI 6 FUNZIONI
056700000000     C     FUNGRU        IFEQ      3
056800000000     C                   MOVEL     FUV(13)       FUN001
056900000000     C                   MOVEL     FUV(14)       FUN002
057000000000     C                   MOVEL     FUV(15)       FUN003
057100000000     C                   MOVEL     FUV(16)       FUN004
057200000000     C                   MOVEL     FUV(17)       FUN005
057300000000     C                   MOVEL     FUV(18)       FUN006
057400000000     C                   END
057500000000     C*
057600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
057700000000     C     FUNGRU        IFEQ      4
057800000000     C                   MOVEL     FUV(19)       FUN001
057900000000     C                   MOVEL     FUV(20)       FUN002
058000000000     C                   MOVEL     FUV(21)       FUN003
058100000000     C                   MOVEL     FUV(22)       FUN004
058200000000     C                   MOVEL     *BLANKS       FUN005
058300000000     C                   MOVEL     *BLANKS       FUN006
058400000000     C                   END
058500000000     C*
058600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
058700000000     C                   MOVEL     *BLANKS       FUN007
058800000000     C                   MOVEL     *BLANKS       FUN008
058900000000     C*
059000000000     C     1             DO        24            I
059100000000     C                   MOVEL     FUD(I)        FUDRIG
059200000000     C     FUDFUN        IFEQ      '24'
059300000000     C     FUDATT        ANDEQ     'S'
059400000000     C                   MOVEL     FUDDES        FUN008
059500000000     C                   END
059600000000     C                   END                                                    DO
059700000000     C                   ENDSR
059800000000     C*================================================================
059900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
060000000000     C*================================================================
060100000000     C     FUNCON        BEGSR
060200000000     C*
060300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
060400000000     C                   EXSR      FUNVAR
060500000000     C*
060600000000     C* PULISCO CAMPI DI LAVORO
060700000000     C                   Z-ADD     0             I                 5 0
060800000000     C                   Z-ADD     0             W                 5 0
060900000000     C*
061000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
061100000000     C     FUNKEY        IFNE      *BLANKS
061200000000     C     1             DO        24            I
061300000000     C                   MOVEL     FUD(I)        FUDRIG
061400000000     C*
061500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
061600000000     C     FUNKEY        IFEQ      FUDFUN
061700000000     C     FUDATT        ANDEQ     'S'
061800000000     C                   Z-ADD     I             W
061900000000     C                   END
062000000000     C                   END                                                    DO WHILE
062100000000     C*
062200000000     C* FUNZIONE NON ATTIVA (ERRORE)
062300000000     C     W             IFEQ      0
062400000000     C*
062500000000     C* EMISSIONE MESSAGGIO DI ERRORE
062600930201     C                   MOVEL     MSG(1)        DSMSMS
062700930201     C*
062800930201     C                   END                                                    W=0
062900000000     C                   END                                                    FUNKEY NE *BLANKS
063000000000     C*
063100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
063200000000     C     W             IFNE      0
063300000000     C     FUNKEY        OREQ      *BLANKS
063400000000     C*
063500000000     C     FUNKEY        IFEQ      *BLANKS
063600000000     C                   EXSR      ENTGES
063700000000     C                   END
063800000000     C     FUNKEY        IFEQ      '01'
063900000000     C                   EXSR      F01GES
064000000000     C                   END
064100000000     C     FUNKEY        IFEQ      '03'
064200000000     C                   EXSR      F03GES
064300000000     C                   END
064400930202     C     FUNKEY        IFEQ      '06'
064500930202     C                   EXSR      F06GES
064600930202     C                   END
064700000000     C     FUNKEY        IFEQ      '07'
064800000000     C                   EXSR      F07GES
064900000000     C                   END
065000000000     C     FUNKEY        IFEQ      '08'
065100000000     C                   EXSR      F08GES
065200000000     C                   END
065300000000     C     FUNKEY        IFEQ      '12'
065400000000     C                   EXSR      F12GES
065500000000     C                   END
065600930201     C     FUNKEY        IFEQ      '21'
065700930201     C                   EXSR      F21GES
065800930201     C                   END
065900000000     C     FUNKEY        IFEQ      '22'
066000000000     C                   EXSR      F22GES
066100000000     C                   END
066200000000     C     FUNKEY        IFEQ      '24'
066300000000     C                   EXSR      F24GES
066400000000     C                   END
066500000000     C     FUNKEY        IFEQ      'HP'
066600000000     C                   EXSR      HELGES
066700000000     C                   END
066800000000     C*
066900000000     C                   END
067000000000     C*
067100000000     C                   ENDSR
067200000000     C*================================================================
067300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
067400000000     C*================================================================
067500000000     C     FUNVAR        BEGSR
067600000000     C*
067700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
067800000000     C*
067900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
068000000000     C     KEY           IFEQ      ENTER
068100000000     C                   MOVEL     *BLANKS       FUNKEY
068200000000     C                   END
068300000000     C     KEY           IFEQ      F1
068400000000     C                   MOVEL     '01'          FUNKEY
068500000000     C                   END
068600000000     C     KEY           IFEQ      F2
068700000000     C                   MOVEL     '02'          FUNKEY
068800000000     C                   END
068900000000     C     KEY           IFEQ      F3
069000000000     C                   MOVEL     '03'          FUNKEY
069100000000     C                   END
069200000000     C     KEY           IFEQ      F4
069300000000     C                   MOVEL     '04'          FUNKEY
069400000000     C                   END
069500000000     C     KEY           IFEQ      F5
069600000000     C                   MOVEL     '05'          FUNKEY
069700000000     C                   END
069800000000     C     KEY           IFEQ      F6
069900000000     C                   MOVEL     '06'          FUNKEY
070000000000     C                   END
070100000000     C     KEY           IFEQ      F7
070200000000     C                   MOVEL     '07'          FUNKEY
070300000000     C                   END
070400000000     C     KEY           IFEQ      F8
070500000000     C                   MOVEL     '08'          FUNKEY
070600000000     C                   END
070700000000     C     KEY           IFEQ      F9
070800000000     C                   MOVEL     '09'          FUNKEY
070900000000     C                   END
071000000000     C     KEY           IFEQ      F10
071100000000     C                   MOVEL     '10'          FUNKEY
071200000000     C                   END
071300000000     C     KEY           IFEQ      F11
071400000000     C                   MOVEL     '11'          FUNKEY
071500000000     C                   END
071600000000     C     KEY           IFEQ      F12
071700000000     C                   MOVEL     '12'          FUNKEY
071800000000     C                   END
071900000000     C     KEY           IFEQ      F13
072000000000     C                   MOVEL     '13'          FUNKEY
072100000000     C                   END
072200000000     C     KEY           IFEQ      F14
072300000000     C                   MOVEL     '14'          FUNKEY
072400000000     C                   END
072500000000     C     KEY           IFEQ      F15
072600000000     C                   MOVEL     '15'          FUNKEY
072700000000     C                   END
072800000000     C     KEY           IFEQ      F16
072900000000     C                   MOVEL     '16'          FUNKEY
073000000000     C                   END
073100000000     C     KEY           IFEQ      F17
073200000000     C                   MOVEL     '17'          FUNKEY
073300000000     C                   END
073400000000     C     KEY           IFEQ      F18
073500000000     C                   MOVEL     '18'          FUNKEY
073600000000     C                   END
073700000000     C     KEY           IFEQ      F19
073800000000     C                   MOVEL     '19'          FUNKEY
073900000000     C                   END
074000000000     C     KEY           IFEQ      F20
074100000000     C                   MOVEL     '20'          FUNKEY
074200000000     C                   END
074300000000     C     KEY           IFEQ      F21
074400000000     C                   MOVEL     '21'          FUNKEY
074500000000     C                   END
074600000000     C     KEY           IFEQ      F22
074700000000     C                   MOVEL     '22'          FUNKEY
074800000000     C                   END
074900000000     C     KEY           IFEQ      F23
075000000000     C                   MOVEL     '23'          FUNKEY
075100000000     C                   END
075200000000     C     KEY           IFEQ      F24
075300000000     C                   MOVEL     '24'          FUNKEY
075400000000     C                   END
075500000000     C     KEY           IFEQ      ROLLUP
075600000000     C                   MOVEL     'RU'          FUNKEY
075700000000     C                   END
075800000000     C     KEY           IFEQ      HELP
075900000000     C                   MOVEL     'HP'          FUNKEY
076000000000     C                   END
076100000000     C                   ENDSR
076200000000     C*================================================================
076300000000     C* NOTE PER COMPILAZIONE SCHIERE
076400000000     C*================================================================
076500000000     C* -MSG-
076600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
076700000000     C* (NON TOCCARE MSG,1)
076800000000     C*
076900000000     C* -CMD-
077000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
077100000000     C*
077200000000     C* -FUD-
077300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
077400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
077600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
077700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
077800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
077900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
078000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
078100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
078200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
078300000000     C*
078400000000     C*================================================================
078500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
078600970730TNSD97R- Tasto funzionale non gestito                                          1
078700970730TNSD97R- Data inizio consegnabilità non accettabile.                           2
078800970730TNSD97R- Data fine consegnabilità non accettabile.                             3
078900970730TNSD97R- Ora inizio consegnabilità non accettabile.                            4
079000970730TNSD97R- Ora fine consegnabilità non accettabile.                              5
079100970730TNSD97R- Data riferimento non accettabile.                                     6
079200970730TNSD97R- Ora riferimento non accettabile.                                      7
079300970730TNSD97R- 8                                                                     8
079400970730TNSD97R- 9                                                                     9
079500970730TNSD97R- 10                                                                   10
079600970730TNSD97R- 11                                                                   11
079700970730TNSD97R- 12                                                                   12
079800970730TNSD97R- 13                                                                   13
079900970730TNSD97R- 14                                                                   14
080000970730TNSD97R- 15                                                                   15
080100970730TNSD97R- 16                                                                   16
080200970730TNSD97R- 17                                                                   17
080300970730TNSD97R- 18                                                                   18
080400970730TNSD97R- 19                                                                   19
080500970730TNSD97R- 20                                                                   20
080600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
080700930201                   -        (S/N)=N                        funz.01¬
080800000000                   -        (S/N)=N                        funz.02¬
080900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
081000000000                   -        (S/N)=N                        funz.04¬
081100000000                   -        (S/N)=N                        funz.05¬
081200930305                   -        (S/N)=N                        funz.06¬
081300930201                   -        (S/N)=N                        funz.07¬
081400930210                   -        (S/N)=N                        funz.08¬
081500000000                   -        (S/N)=N                        funz.09¬
081600000000                   -        (S/N)=N                        funz.10¬
081700000000                   -        (S/N)=N                        funz.11¬
081800140820F12=Ritorno        -        (S/N)=n  <= S-FISSO            funz.12¬
081900000000                   -        (S/N)=N                        funz.13¬
082000000000                   -        (S/N)=N                        funz.14¬
082100000000                   -        (S/N)=N                        funz.15¬
082200000000                   -        (S/N)=N                        funz.16¬
082300000000                   -        (S/N)=N                        funz.17¬
082400000000                   -        (S/N)=N                        funz.18¬
082500000000                   -        (S/N)=N                        funz.19¬
082600000000                   -        (S/N)=N                        funz.20¬
082700930305                   -        (S/N)=N                        funz.21¬
082800930305                   -        (S/N)=N                        funz.22¬
082900000000                   -        (S/N)=N                        funz.23¬
083000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
