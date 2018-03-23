000100131013     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200131013     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300131013     H DECEDIT('0,') DATEDIT(*DMY.)
000400131013      *
000500131013    > *           LANCIO creazione file per invio telematico
000600131013      *             dello spesometro
000700131013      *           ------------------------------------------
000800131013      *
000900131013      *
001000131013      * DOCUMENTI MICROANALISI :
001100131013      *
001200131013      *               MAPPA INDICATORI STD
001300131013      *
001400131013      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
001500131013      *  21           GENERICO OPERAZIONI I/O
001600131013      *  22           GENERICO ERRORE OPERAZIONI I/O
001700131013      *  30           SFLDSP
001800131013      * N31           SFLCLR
001900131013      *  31           SFLDSPCTL
002000131013      *  32           SFLNXTCHG
002100131013      *  33           SFLEND
002200131013      *  39           OF PRTF
002300131013      *  40 <---> 49  DSPATR ERRORI SU SFL
002400131013    > *  Specificare l'uso dei singoli indicatori
002500131013      *  50 <---> 98  ERRORI SU VIDEO
002600131013    > *  Specificare l'uso dei singoli indicatori
002700131013      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002800131013      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002900131013      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003000131013      *  99           INDIC. GENERALE DI ERRORE
003100131013     F*----------------------------------------------------*
003200131015     FNDSTT02L  IF   E           K DISK    RENAME(NDSTT000:NDSTT02)
003300131015     F*------------
003400131013    >FNDCP95D   CF   E             WORKSTN
003500131013     F                                     RENAME(CP95D1:FMTD1)
003600131013     F                                     RENAME(CP95T1:FMTT1)
003700131013     F                                     RENAME(CP95Z1:FMTZ1)
003800131013    >F* eventuali schermi supplementari
003900131013     F                                     RENAME(CP95D2:FMTD2)
004000131013    >F* modifica valori assunti
004100131013     F                                     RENAME(CP95D3:FMTD3)
004200131013     F                                     RENAME(CP95Z3:FMTZ3)
004300131013     F                                     INFDS(DSFMT)
004400131013     F*------------
004500131013     D*----------------------------------------------------*
004600131013     D* Schiere per la gestione dei tasti di funzione
004700131013     D $FM             S             27    DIM(24)
004800131013     D $FX             S             50    DIM(24)
004900131013     D $FC             S              1    DIM(24)
005000131013     D $FR             S              1    DIM(24)
005100131013     D* Video D1/D2
005200131013     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005300131013     D $FX1            S             50    DIM(24) ALT($FM1)
005400131013     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005500131013     D $FR1            S              1    DIM(24) ALT($FC1)
005600131013     D* Video D3 Valori assunti
005700131013     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
005800131013     D $FX3            S             50    DIM(24) ALT($FM3)
005900131013     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006000131013     D $FR3            S              1    DIM(24) ALT($FC3)
006100131013     D*----------------------------------------------------*
006200131013     D*-------------
006300131013     D* Passaggio Parametri
006400131013     D KPJBA         E DS
006500131013     D*-------------
006600131013     D* DS Esterna gestione messaggi per COMIT
006700131013     D XMSGDS        E DS                  INZ
006800131013     D  STS          E                     EXTFLD(MSGSTS)
006900131013     D                                     DIM(333)
007000131013     D  JBA          E                     EXTFLD(MSGJBA)
007100131013     D                                     DIM(502)
007200131013     D*-------------
007300131013     D*Asteriscare se non presenti
007400131013     D* Reperimento Autorizzazioni
007500131013     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
007600131013     D*-------------
007700131013     D* Dati per pgm XNOAUT
007800131013     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
007900131013     D*-------------
008000131013     D* Passaggio Autorizzazioni
008100131013     D DSAUT           DS           512
008200131013     D*-------------
008300131013     D* Maschera dati autorizzazioni
008400131013     D CONCIVDS      E DS
008500131013     D*-------------
008600131013     D* Valori assunti
008700131013     D XMPEDS        E DS                  INZ
008800131013     D XMPEDS1       E DS                  INZ
008900131013     D DSMPE           DS                  INZ
009000131013     D  $VAL                          1
009100131013     D  $Anno                         4S 0
009200131015     D  $TpEla                        1A
009300131013     D  $TpInvio                      1A
009400131013     D  $ProtSos                     17S 0
009500131013     D  $ProtDoc                      6S 0
009600131013     D  $MaxDim                       2S 0
009700131013     D  $MaxRec                       5S 0
009800131013     D*-------------
009900131013     D* Reperimento nome PGM
010000131013     D STATUS         SDS           333
010100131013     D  DSPGM            *PROC
010200131013     D  PARMS            *PARMS
010300131013     D*-------------
010400131013     D* Dati di ambiente ottenuti da XSOC
010500131013    >D SOC001        E DS                  EXTNAME(XSOC001DS)
010600131013     D*-------------
010700131013     D* DS Interna per dati di output di XSOC
010800131013     D XSOCDS          DS          1000
010900131013     D*-------------
011000131013     D* Ds per ricezione parametri
011100131013     D NDCP95DS      E DS
011200131013     D  ERRP95       E                     INZ('0')
011300131013     D*-------------
011400131013     D* Passaggio Parametri al PGM di estrazione dati
011500131013    >D NDCP96DS      E DS
011600131013    >D  ERRP96       E                     INZ('0')
011700131013     D*-------------
011800131013     D* Passaggio Parametri al PGM di controllo dati
011900131016    >D NDCP99DS      E DS
012000131016    >D  ERRP99       E                     INZ('0')
012100131013     D*-------------
012200131013     D* Parametri modulo SCARSUMA
012300131013     D ANP022DS      E DS
012400131013     D*-------------
012500131013     D* parametro modulo PJSPES13
012600131013     D ANP098DS      E DS
012700131013     D*-------------
012800131013     D*  Ds per ricerca legende
012900131013     D ANA964DS      E DS                  INZ
013000131013     D*  Controlli validità per valore di una legenda
013100131013     D X07VALDS      E DS                  Inz
013200131013     D DSFMT           DS
013300131013     D  $TASTO               369    369
013400131013     D  NRG                  370    370
013500131013     D  NCL                  371    371
013600131013     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
013700131013     D*-------------
013800131013     D* posizione cursore
013900131013     D CURSOR          DS
014000131013     D  RIRI                   1      2B 0 INZ
014100131013     D  R$$                    2      2
014200131013     D  COCO                   3      4B 0 INZ
014300131013     D  C$$                    4      4
014400131013     D*-------------
014500131013     D* Tasti di funzione
014600131013     D F01             C                   CONST(X'31')
014700131013     D F02             C                   CONST(X'32')
014800131013     D F03             C                   CONST(X'33')
014900131013     D F04             C                   CONST(X'34')
015000131013     D F05             C                   CONST(X'35')
015100131013     D F06             C                   CONST(X'36')
015200131013     D F07             C                   CONST(X'37')
015300131013     D F08             C                   CONST(X'38')
015400131013     D F09             C                   CONST(X'39')
015500131013     D F10             C                   CONST(X'3A')
015600131013     D F11             C                   CONST(X'3B')
015700131013     D F12             C                   CONST(X'3C')
015800131013     D F13             C                   CONST(X'B1')
015900131013     D F14             C                   CONST(X'B2')
016000131013     D F15             C                   CONST(X'B3')
016100131013     D F16             C                   CONST(X'B4')
016200131013     D F17             C                   CONST(X'B5')
016300131013     D F18             C                   CONST(X'B6')
016400131013     D F19             C                   CONST(X'B7')
016500131013     D F20             C                   CONST(X'B8')
016600131013     D F21             C                   CONST(X'B9')
016700131013     D F22             C                   CONST(X'BA')
016800131013     D F23             C                   CONST(X'BB')
016900131013     D F24             C                   CONST(X'BC')
017000131013     D ENTER           C                   CONST(X'F1')
017100131013     D ROLDWN          C                   CONST(X'F4')
017200131013     D ROLLUP          C                   CONST(X'F5')
017300131013     I*-------------
017400131013     I* Sottotitolo di testata
017500131013    >I* se non si hanno sottotitoli, mettere tutta la costante = ' '
017600131013     D*SOTTIT          C                   CONST('PROMSG    *LIBL-
017700131013     D*                                    COSxxxx')
017800131013     D SOTTIT          C                   CONST('                            ')
017900131013     D*-----------
018000131013     D* Campi di lavoro
018100131013     D CallPgm         S              1
018200131013     D $OkExeLan       S              1
018300131013     D $AnnoAlfa       S              4
018400131013     D*----------------
018500131013     D* ds per nome file
018600131013     D                 DS
018700131104C2228X***  WAnno                  4      7
018800131104C2228X***  WPrgInvio              8      9  0
018900131104C2228D  WAnno                  4      5
019000131104C2228D  WPrgInvio              6      9  0
019100131013     D  WNumero                1      9
019200131104C2228D*----------------
019300131104  "  D* ds per nome file Black List e San Marino
019400131104  "  D                 DS
019500131104  "  D  WAnno2                 4      5
019600131104  "  D  WPrgInvio2             6      9  0
019700131104C2228D  WNumero2               1      9
019800131104     D*-----------
019900131013     D* Ds contenente i valori del msg PRO1797
020000131013     D §Fil_Lib        DS
020100131013     D  §File                        10A
020200131013     D  §Libreria                    10A
020300131013     D*----------------------------------------------------*
020400131013     D $P1COS2161      s             20A
020500131013     C*----------------------------------------------------*
020600131013     C*                MAIN LINE PROGRAM
020700131013     C*----------------------------------------------------*
020800131013     C*
020900131013     C* inizializzazione variabili
021000131013     C                   EXSR      INZVAR
021100131013     C*
021200131013     C     $FINE         DOWEQ     *OFF
021300131013     C     $GEST         CASEQ     'D1'          GESD1
021400131013    >C     $GEST         CASEQ     'D2'          GESD2
021500131013     C     $GEST         CASEQ     'D3'          GESD3
021600131013     C                   ENDCS
021700131013     C                   ENDDO
021800131013     C*
021900131013     C* fine programma
022000131013     C                   EXSR      ENDPGM
022100131013     C/EJECT
022200131013     C************************************************************
022300131013     C* RICERCHE
022400131013     C************************************************************
022500131013     C     ENDPGM        BEGSR
022600131013     C*
022700131013     C*
022800131013     C* se ho confermato l'uscita proseguo
022900131013     C     $FINE         IFEQ      *ON
023000131013     C*
023100131013     C                   SETON                                            LR
023200131013     C                   RETURN
023300131013     C*
023400131013     C                   ENDIF
023500131013     C*
023600131013     C                   ENDSR
023700131013     C/EJECT
023800131013     C************************************************************
023900131013     C* RICERCHE
024000131013     C************************************************************
024100131013     C     SEARCH        BEGSR
024200131013     C*
024300131013     C* determino Riga/Colonna del cursore
024400131013     C                   MOVE      NRG           R$$
024500131013     C                   MOVE      NCL           C$$
024600131013     C                   Z-ADD     RIRI          CURR
024700131013     C                   Z-ADD     COCO          CURC
024800131013     C*
024900131013     C* I campi che al momento dell'F4 risultano protetti non devono
025000131013     C* essere abilitati alla funzione di ricerca
025100131013     C*
025200131013     C                   SELECT
025300131013     C*
025400131013     C* FMTD1 - Tipologia invio
025500131013    >C     H1NMFL        WHENEQ    'D1TPINVIO'
025600131013     C                   RESET                   ANA964DS
025700131013     C*
025800131013     C                   MOVEL     '01'          OPZ964
025900131013     C                   MOVEL     'NDCP95S   '  TRC964
026000131013     C                   MOVEL     'TPINVIOP95'  CAM964
026100131013     C                   MOVE      *ON           ALL964
026200131013     C                   MOVEL     ANA964DS      KPJBU
026300131013     C                   CALL      'ANA964R'
026400131013     C                   PARM                    KPJBA
026500131013     C                   MOVEL     KPJBU         ANA964DS
026600131013     C     RET964        IFEQ      *BLANK
026700131013     C                   MOVEL     VAL964        D1TpInvio
026800131013     C                   ENDIF
026900131013     C*
027000131013     C*
027100131013     C* ricerca non abilitata in questa posizione
027200131013     C                   OTHER
027300131013     C                   SETON                                        9899
027400131013     C*
027500131013     C                   ENDSL
027600131013     C*
027700131013     C* imposto pos. del cursore
027800131013     C                   Z-ADD     CURR          H1RIGA
027900131013     C                   Z-ADD     CURC          H1COLO
028000131013     C*
028100131013     C                   MOVE      *HIVAL        $LASTV
028200131013     C*
028300131013     C                   ENDSR
028400131013     C/EJECT
028500131013     C************************************************************
028600131013     C* GESTIONE EMISSIONE TASTI FUNZIONE
028700131013     C************************************************************
028800131013     C     GESPIE        BEGSR
028900131013     C*
029000131013     C                   SELECT
029100131013     C*
029200131013     C     $GEST         WHENEQ    'D1'
029300131013     C     $ULKD1        IFEQ      0
029400131013     C* riempimento opzioni
029500131013     C                   EXSR      F24D1
029600131013     C                   ELSE
029700131013     C                   WRITE     FMTZ1
029800131013     C                   ENDIF
029900131013     C*
030000131013     C     $GEST         WHENEQ    'D2'
030100131013     C     $ULKD1        IFEQ      0
030200131013     C* riempimento opzioni
030300131013     C                   EXSR      F24D1
030400131013     C                   ELSE
030500131013     C                   WRITE     FMTZ1
030600131013     C                   ENDIF
030700131013     c*
030800131013     C* Modifica valori assunti
030900131013     C     $GEST         WHENEQ    'D3'
031000131013     C     $ULKD3        IFEQ      0
031100131013     C                   EXSR      F24D3
031200131013     C                   ELSE
031300131013     C                   WRITE     FMTZ3
031400131013     C                   ENDIF
031500131013     C                   ENDSL
031600131013     C*
031700131013     C                   ENDSR
031800131013     C/EJECT
031900131013     C************************************************************
032000131013     C* GESTIONE VIDEO RECORD D1
032100131013     C************************************************************
032200131013     C     GESD1         BEGSR
032300131013     C* inizializzazione videata
032400131013   B1C     $INZD1        IFEQ      *ON
032500131013     C                   EXSR      INZD1
032600131013     C                   MOVE      *OFF          $INZD1
032700131013   E1C                   ENDIF
032800131013     C* emissione piede videata se proveniente da altra
032900131013     C* salvataggio valore formato di provenienza
033000131013   B1C     $LASTV        IFNE      'D1'
033100131013     C                   WRITE     FMTT1
033200131013     C                   EXSR      GESPIE
033300131013     C                   MOVE      $LASTV        $LASTG
033400131013     C                   MOVE      'D1'          $LASTV
033500131013   E1C                   ENDIF
033600131013     C*
033700131013     C* se esistono errori sulla videata
033800131013     C* emetto la write del formato a indicatori spenti per vedere
033900131013     C* le eventuali decodifiche
034000131013   B1C     *IN99         IFEQ      *ON
034100131013     C     $GEST         ANDEQ     'D1'
034200131013     C                   MOVEA     *IN           WIN
034300131013     C                   MOVE      *ALL'0'       IN5098           49
034400131013     C                   MOVEA     IN5098        *IN(50)
034500131013     C                   WRITE     FMTD1
034600131013     C                   MOVEA     WIN           *IN
034700131013   E1C                   ENDIF
034800131013     C*
034900131013     C                   EXFMT     FMTD1
035000131013     C* controllo abilitazione tasti funzione
035100131013     C*
035200131013     C                   EXSR      CTRKD1
035300131013     C*
035400131013   B1C     *IN99         IFEQ      *OFF
035500131013     C*
035600131013   B2C                   SELECT
035700131013     C* ROLLUP
035800131013    >C     $TASTO        WHENEQ    ROLLUP
035900131013    >C                   MOVE      'D2'          $GEST
036000131013     C* F2=Decodifica
036100131013     C     $TASTO        WHENEQ    F02
036200131013     C                   EXSR      F02D1
036300131013     C* F3=Fine
036400131013     C     $TASTO        WHENEQ    F03
036500131013     C                   EXSR      F03D1
036600131013     C* F4=Ricerca
036700131013     C     $TASTO        WHENEQ    F04
036800131013     C                   EXSR      SEARCH
036900131013     C* F5=Refresh
037000131013     C     $TASTO        WHENEQ    F05
037100131013     C                   EXSR      F05D1
037200131013     C* F8=Successivo
037300131013     C     $TASTO        WHENEQ    F08
037400131013     C                   EXSR      F08D1
037500131013     C* F12=Ritorno
037600131013     C     $TASTO        WHENEQ    F12
037700131013     C                   EXSR      F12D1
037800131013     C* F18=Valori assunti
037900131013     C     $TASTO        WHENEQ    F18
038000131013     C                   EXSR      F18D1
038100131013     C* F21=Pers. batch
038200131013     C     $TASTO        WHENEQ    F21
038300131013     C                   EXSR      F21D1
038400131013     C* F24=Altri tasti
038500131013     C     $TASTO        WHENEQ    F24
038600131013     C                   EXSR      F24D1
038700131013     C*
038800131013   X2C                   OTHER
038900131013     C                   EXSR      CTRD1
039000131013     C* aggiungere le chiamate ai controlli delle videate supplementari
039100131013    >C     *IN99         IFEQ      *OFF
039200131013    >C                   EXSR      CTRD2
039300131013    >C     *IN99         IFEQ      *ON
039400131013    >C                   SETON                                            96
039500131013    >C                   ENDIF
039600131013    >C                   ENDIF
039700131013     C*
039800131013   B3C     $TASTO        IFEQ      F06
039900131013     C     *IN99         ANDEQ     *OFF
040000131013     C*
040100131013     C* controllo se i dati sono ok
040200131013     C                   EXSR      CtrDati
040300131013     C*
040400131013     C* avverto per richiesta di file con capacità limitata
040500131013     C                   if        $OkExeLan   = *On
040600131013     C                   EXSR      CtrInvio
040700131013     C                   Endif
040800131013     C*
040900131013     C                   if        $OkExeLan   = *On
041000131013     C*
041100131013     C* controllo se esiste già il file con il nome reperito
041200131013     C                   EXSR      CtrFileNew
041300131013     C*
041400131013     C                   endif
041500131013     C*
041600131013     C                   if        $OkExeLan   = *On
041700131013     C* eseguo lancio
041800131013     C                   EXSR      EXELAN
041900131013     C                   EXSR      ENDPGM
042000131013     C*
042100131013     C                   endif
042200131013     C*
042300131013   E3C                   ENDIF
042400131013     C*
042500131013   E2C                   ENDSL
042600131013     C*
042700131013   E1C                   ENDIF
042800131013     C*
042900131013     C                   ENDSR
043000131013     C/EJECT
043100131013     C************************************************************
043200131013     C* INIZIALIZZAZIONE VIDEATA DATI
043300131013     C************************************************************
043400131013     C     INZD1         BEGSR
043500131013     C*
043600131013     C                   CLEAR                   FMTD1
043700131013    >C                   CLEAR                   FMTD2
043800131013     C*
043900131013     C*
044000131013     C                   Z-ADD     1             $PIED1
044100131013     C*
044200131013     C* se il pgm è stato richiamato imposto i valori ricevuti dalla DS
044300131013     C                   if        CallPgm     = '1'
044400131013     C*
044500131013     C                   EXSR      RIED1_P95
044600131013     C*
044700131013     C* se il pgm NON è stato richiamato
044800131013     C                   else
044900131013     C*
045000131013     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
045100131013     C     $Val          IFEQ      *ON
045200131013     C* riempio la videata da parametri memorizzati
045300131013     C                   EXSR      RIED1
045400131013    >C*  Aggiungere le routine di riempimento dei record succcessivi
045500131013    >C                   EXSR      RIED2
045600131013     C                   ELSE
045700131013     C* Immettere i valodi di default a video
045800140107     C*******            Eval      D1Anno      = *Year - 1
045900140107      * temporaneo !!! 2012
046000140107     C                   Eval      D1Anno      = 2012
046100131013     C                   Eval      D1TpInvio   = '0'
046200131015     C                   Eval      D1MaxDim    = 5
046300131015     C                   Eval      D1TpELA  = '1'
046400131013     C                   ENDIF
046500131015     C* Reperisco il numero di elaborazione
046600131015     C                   EXSR      RepElab
046700131013     C*
046800131013     C                   endif
046900131013     C*
047000131013     C* richiamo routine dei ctrl per decodificarla
047100131013     C                   EXSR      CTRD1
047200131013    >C* Aggiungere le routine di controllo dei record succcessivi
047300131013    >C                   EXSR      CTRD2
047400131013     C                   MOVE      *ALL'0'       IN5098
047500131013     C                   MOVEA     IN5098        *IN(50)
047600131013     C*
047700131013     C                   ENDSR
047800131013     C/EJECT
047900131013     C************************************************************
048000131013     C* RIEMPIMENTO VIDEATA  D1 con i valori ricevuti nella DS
048100131013     C************************************************************
048200131013     C     RIED1_P95     BEGSR
048300131013     C*
048400131015     C                   Movel     TpElaP95      D1TpEla
048500131015     C                   Z-Add     NumelaP95     D1Numela
048600131015     C                   Z-Add     AnnoP95       D1Anno
048700131015     C                   Movel     PeriodoP95    D1Periodo
048800131015     C                   Movel     TipPerP95     D1Tipper
048900131013     C                   Movel     TpInvioP95    D1TpInvio
049000131013     C                   Z-Add     ProtSosP95    D1ProtSos
049100131013     C                   Z-Add     ProtDoCP95    D1ProtDoc
049200131013     C                   Z-Add     MaxDimP95     D1MaxDim
049300131013     C                   Z-Add     MaxReCP95     D1MaxRec
049400131013     C*
049500131013     C                   ENDSR
049600131013     C/EJECT
049700131013     C************************************************************
049800131013     C* RIEMPIMENTO VIDEATA  D1
049900131013     C************************************************************
050000131013     C     RIED1         BEGSR
050100131013     C*
050200131013     C* impostare i campi della videata dai parametri memorizzati su anmpe
050300131013     C*
050400131013     C                   Z-Add     $Anno         D1Anno
050500131015     C                   Movel     $TpEla        D1TpEla
050600131013     C                   Movel     $TpInvio      D1TpInvio
050700131013     C                   Z-Add     $ProtSos      D1ProtSos
050800131013     C                   Z-Add     $MaxDim       D1MaxDim
050900131013     C                   Z-Add     $MaxDim       D1MaxDim
051000131013     C                   Z-Add     $MaxRec       D1MaxRec
051100131013     C*
051200131013     C                   ENDSR
051300131013     C/EJECT
051400131013     C************************************************************
051500131013     C* GESTIONE F02 VIDEO D1
051600131013     C************************************************************
051700131013     C     F02D1         BEGSR
051800131013     C*
051900131013     C* per decodificare la videata richiamo la routine dei ctrl
052000131013     C* e poi disattivo gli eventuali messaggi di errore
052100131013     C                   EXSR      CTRD1
052200131013     C*
052300131013     C                   MOVE      *ALL'0'       IN5098
052400131013     C                   MOVEA     IN5098        *IN(50)
052500131013     C*
052600131013     C                   ENDSR
052700131013     C/EJECT
052800131013     C************************************************************
052900131013     C* GESTIONE F03 VIDEO D1
053000131013     C************************************************************
053100131013     C     F03D1         BEGSR
053200131013     C*
053300131013     C                   MOVE      *ON           $FINE
053400131013     C* fine programma
053500131013     C                   EXSR      ENDPGM
053600131013     C*
053700131013     C                   ENDSR
053800131013     C/EJECT
053900131013     C************************************************************
054000131013     C* GESTIONE F05 VIDEO D1
054100131013     C************************************************************
054200131013     C     F05D1         BEGSR
054300131013     C*
054400131013     C                   MOVE      *ON           $INZD1
054500131013     C*
054600131013     C                   ENDSR
054700131013     C/EJECT
054800131013     C************************************************************
054900131013     C* GESTIONE F08 VIDEO D1
055000131013     C************************************************************
055100131013     C     F08D1         BEGSR
055200131013     C*
055300131013     C                   MOVE      *ON           $FINE
055400131013     C*
055500131013     C                   ENDSR
055600131013     C/EJECT
055700131013     C************************************************************
055800131013     C* GESTIONE F12 VIDEO D1
055900131013     C************************************************************
056000131013     C     F12D1         BEGSR
056100131013     C*
056200131013     C                   MOVE      *ON           $FINE
056300131013     C*
056400131013     C                   ENDSR
056500131013     C/EJECT
056600131013     C************************************************************
056700131013     C* GESTIONE F18 VIDEO D1
056800131013     C************************************************************
056900131013     C     F18D1         BEGSR
057000131013     C*
057100131013     C                   MOVE      'D3'          $GEST
057200131013     C                   MOVE      *ON           $INZD3
057300131013     C*
057400131013     C                   ENDSR
057500131013     C/EJECT
057600131013     C************************************************************
057700131013     C* GESTIONE F21 VIDEO D1
057800131013     C************************************************************
057900131013     C     F21D1         BEGSR
058000131013     C*
058100131013     C                   MOVE      'CP96'        KCOAZ
058200131013     C                   CALL      'BCH09'
058300131013     C                   PARM                    KPJBA
058400131013     C*
058500131013     C                   ENDSR
058600131013     C/EJECT
058700131013     C************************************************************
058800131013     C* GESTIONE F24 VIDEO D1
058900131013     C************************************************************
059000131013     C     F24D1         BEGSR
059100131013     C*
059200131013     C                   MOVEA     $FX1          $FX
059300131013     C                   MOVEA     $FC1          $FC
059400131013     C                   MOVEA     $FR1          $FR
059500131013     C                   Z-ADD     $ULKD1        $ULK
059600131013     C                   Z-ADD     79            $L01
059700131013     C                   Z-ADD     79            $L02
059800131013     C                   EXSR      RIEKEY
059900131013     C                   Z-ADD     $ULK          $ULKD1            3 0
060000131013     C                   MOVEA     $FC           $FC1
060100131013     C                   MOVEL     $KEY1         Z1KE1
060200131013     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
060300131013     C                   MOVEL     $KEY2         Z1KE2
060400131013     C                   WRITE     FMTZ1
060500131013     C*
060600131013     C                   ENDSR
060700131013     C/EJECT
060800131013     C************************************************************
060900131013     C* GESTIONE F24 VIDEO D3
061000131013     C************************************************************
061100131013     C     F24D3         BEGSR
061200131013     C*
061300131013     C                   MOVEA     $FX3          $FX
061400131013     C                   MOVEA     $FC3          $FC
061500131013     C                   MOVEA     $FR3          $FR
061600131013     C                   Z-ADD     $ULKD3        $ULK
061700131013     C                   Z-ADD     44            $L01
061800131013     C                   Z-ADD     0             $L02
061900131013     C                   EXSR      RIEKEY
062000131013     C                   Z-ADD     $ULK          $ULKD3            3 0
062100131013     C                   MOVEA     $FC           $FC3
062200131013     C                   MOVEL     $KEY1         Z3KE1
062300131013     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
062400131013     C                   WRITE     FMTZ3
062500131013     C*
062600131013     C                   ENDSR
062700131013     C/EJECT
062800131013     C************************************************************
062900131013     C* CONTROLLO VIDEATA
063000131013     C************************************************************
063100131013     C     CTRD1         BEGSR
063200131013     C*
063300131013     C                   SETOFF                                       99
063400131013     C* non imposto pos. del cursore
063500131013     C                   Z-ADD     0             H1RIGA
063600131013     C                   Z-ADD     0             H1COLO
063700131013     C*
063800131013     C* anno
063900131013     C                   EXSR      CtrAnno
064000131015     C*
064100131015     C* Controllo il tipo elaborazione
064200131015     C                   EXSR      CtrTpElab
064300131015     C*
064400131015     C* Se BL
064500131015     C                   Clear                   D1TipPerD
064600131015     C                   If        D1TpEla = '2'
064700131015     C*
064800131015     C* Controllo periodo
064900131015     C                   Exsr      CtrPeriodo
065000131015     C*
065100131015     C* Controllo il tipo periodo
065200131015     C                   Exsr      CtrTipPer
065300131015     C*
065400131015     C                   Endif
065500131013     C*
065600131013     C* controllo Tipo Invio
065700131013     C                   EXSR      CtrTpInvio
065800131013     C*
065900131013     C* controllo protocollo
066000131013     C                   EXSR      CtrProt
066100131013     C*
066200131013     C* parametri per multiinvio
066300131013     C                   EXSR      CtrMultiInvio
066400131013     C*
066500131013     C* Reperimento nome file per scarico
066600131013     C                   EXSR      RepNomeF
066700131013     C*
066800131013     C                   ENDSR
066900131015     C/EJECT
067000131015     C************************************************************
067100131015     C* Reperimento elaborazione
067200131015     C************************************************************
067300131015     C     RepElab       BEGSR
067400131015     C*
067500131015     C* Cerco con società ed anno l'elaborazione se già presente
067600131015     C                   Eval      STTSOCIETA = xscsoc
067700131016     C                   Eval      STTAnno    = D1Anno
067800131016     C                   Eval      STTTpEla   = D1TpEla
067900131016     C                   If        STTTpEla = '2'
068000131016     C                   Eval      STTperiodo = D1Periodo
068100131016     C                   Eval      STTTipPer  = D1TipPer
068200131016     C     K05STT02      Chain     NdSTT02L
068300131016     C                   Else
068400131016     C     K03STT02      Chain     NdSTT02L
068500131016     C                   Endif
068600131016     C                   If        %found(NdSTT02l)
068700131114C2230X*** ex C2228                 and STTStaEla = '0'
068800131016     C                   Eval      D1NumEla = STTNumELA
068900131016     C                   Eval      D1DetEla = STTDetEla
069000131114C2230C                   z-add     STTNrDic      D1ProtSos
069100131114C2230C                   z-add     STTProtDoc    D1ProtDoc
069200131015     C                   Endif
069300131015     C*
069400131015     C                   ENDSR
069500131013     C/EJECT
069600131013     C***********************************************************
069700131013     C* Controllo anno
069800131013     C***********************************************************
069900131013     C     CtrAnno       BEGSR
070000131013     C*
070100131013     C                   if        D1Anno      = 0
070200131013     C                   SETON                                        5099
070300131013     C                   endif
070400131105C2228C* Se non esiste la elaborazione
070500131105  "  C                   If        D1NumEla = 0
070600131105  "  C                   Seton                                        6099
070700131105C2228C                   Endif
070800131013     C*
070900131013     C                   ENDSR
071000131013     C/EJECT
071100131015     C***********************************************************
071200131015     C* Controllo Tipo Elaborazione
071300131015     C***********************************************************
071400131015     C     CtrTpElab     BEGSR
071500131015     C*
071600131015     C                   Clear                   D1TpElaD
071700131015     C                   Clear                   X07valDS
071800131015     C                   Move      '1'           X07Ric
071900131015     C                   Move      *Off          X07All
072000131015     C                   MoveL     'NDSTT000'    X07TRc
072100131015     C                   MoveL     'STTTPELA'    X07Cam
072200131015     C                   MoveL     D1TpEla       X07VAL
072300131015     C                   CallB     'X07VALR'
072400131015     C                   Parm                    X07ValDS
072500131015     C                   if        X07ERR      = *Off
072600131015     C                   MoveL     X07DES        D1TpElaD
072700131015     C                   else
072800131015     C                   SetOn                                        5499
072900131015     C                   endif
073000131015     C*
073100131015     C                   ENDSR
073200131015     C/EJECT
073300131015     C***********************************************************
073400131015     C* Controllo periodo
073500131015     C***********************************************************
073600131015     C     CtrPeriodo    BEGSR
073700131015     C                   If        D1Periodo = ' ' or d1periodo = '00'
073800131015     C                   SetOn                                        5299
073900131015     C                   EndIf
074000131015     C*
074100131015     C                   Endsr
074200131015     C/EJECT
074300131015     C***********************************************************
074400131015     C* Controllo Tipo periodo
074500131015     C***********************************************************
074600131015     C     CtrTipPer     BEGSR
074700131015     C*
074800131015     C                   Clear                   D1TipPerD
074900131015     C*
075000131015     C                   RESET                   X07valDS
075100131015     C*
075200131015     C                   Move      '1'           X07Ric
075300131015     C                   Move      *On           X07All
075400131015     C                   MoveL     'NDSTT000'    X07TRc
075500131015     C                   MoveL     'STTTIPPER'   X07Cam
075600131015     C                   MoveL     D1TipPer      X07VAL
075700131015     C                   CallB     'X07VALR'
075800131015     C                   Parm                    X07ValDS
075900131015     C                   if        X07ERR      = *Off
076000131015     C                   MoveL     X07DES        D1TipPerD
076100131015     C                   else
076200131015     C                   SetOn                                        5399
076300131015     C                   endif
076400131015     C*
076500131015     C                   Endsr
076600131015     C/EJECT
076700131013     C***********************************************************
076800131013     C* Controllo Tipo Invio
076900131013     C***********************************************************
077000131013     C     CtrTpInvio    BEGSR
077100131013     C*
077200131013     C                   Clear                   D1TpinvioD
077300131013     C*
077400131013     C                   if        D1TpInvio   = *blanks
077500131014     C                   SETON                                        5499
077600131013     C                   else
077700131013     C                   RESET                   X07VALDS
077800131013     C                   MOVE      '1'           X07RIC
077900131013     C                   MOVEL     'NDCP95S   '  X07TRC
078000131013     C                   MOVEL     'TPINVIOP95'  X07CAM
078100131013     C                   MOVEL     D1TpInvio     X07VAL
078200131013     C                   MOVE      *ON           X07ALL
078300131013     C                   CALLB     'X07VALR'
078400131013     C                   PARM                    X07VALDS
078500131013     C     X07ERR        IFEQ      *Off
078600131013     C                   MOVEL     X07DES        D1TpInvioD
078700131013     C                   ELSE
078800131014     C                   SetOn                                        5599
078900131013     C                   ENDIF
079000131013     C                   ENDIF
079100131115C2230C*
079200131115  "  C                   Select
079300131115  "  C                   When      (D1TpInvio = '0' or
079400131115  "  C                             D1TpInvio='1') and
079500131115  "  C                             SttStaEla <> '0'
079600131115  "  C                   Seton                                        6199
079700131115  "  C                   When      D1TpInvio = '2' and
079800131115  "  C                             SttStaEla <> '2'
079900131115  "  C                   Seton                                        6299
080000131115C2230C                   EndSl
080100131013     C*
080200131013     C                   ENDSR
080300131013     C/EJECT
080400131013     C***********************************************************
080500131013     C* Controllo protocollo
080600131013     C***********************************************************
080700131013     C     CtrProt       BEGSR
080800131013     C*
080900131013     C                   if        D1TpInvio = '1' or D1TpInvio = '2'
081000131013     C                   If        D1protSos = 0
081100131014     C                   SETON                                        5699
081200131013     C                   endif
081300131013     C                   If        D1protDoc = 0
081400131014     C                   SETON                                        5799
081500131013     C                   endif
081600131013     C                   else
081700131013     C                   If        D1protSos <> 0
081800131014     C                   SETON                                        5699
081900131013     C                   endif
082000131013     C                   If        D1protDoc <> 0
082100131014     C                   SETON                                        5799
082200131013     C                   endif
082300131013     C                   endif
082400131013     C*
082500131013     C                   ENDSR
082600131013     C/EJECT
082700131013     C***********************************************************
082800131013     C* Controllo parametri per invio multiplo
082900131013     C***********************************************************
083000131013     C     CtrMultiInvio BEGSR
083100131013     C*
083200131013     C                   if        D1MaxDim   <> 0   and
083300131013     C                             D1MaxRec   <> 0
083400131013     C                   SETON                                        5199
083500131013     C                   endif
083600131013     C*
083700131013     C                   ENDSR
083800131013     C/EJECT
083900131013     C************************************************************
084000131013     C* Reperimento nome file per invio spesometro
084100131013     C************************************************************
084200131013     C     RepNomeF      BEGSR
084300131013     C*
084400131020     C                   Select
084500131020     C*
084600131020     C                   When      STTTpEla = '1'
084700131020     C                   Movel     'SP'          $CdScarico
084800131020     C*
084900131020     C                   When      STTTpEla = '2'
085000131020     C                   Movel     'BL'          $CdScarico
085100131020     C*
085200131020     C                   When      STTTpEla = '3'
085300131020     C                   Movel     'SM'          $CdScarico
085400131020     C*
085500131020     C                   EndSl
085600131020     C*
085700131013     C*
085800131104C2228C                   Select
085900131104C2228C                   When      STTTpEla = '1'
086000131104     C                   Eval      WNumero     = *All'0'
086100131104     C                   Move      D1Anno        WAnno
086200131104C2228X***                If        STTTpEla <>'1'
086300131104  "  X***                Movel     STTPeriodo    WAnno
086400131104C2228X***                ENdif
086500131104     C                   Z-Add     1             WPrgInvio
086600131104     C                   Move      WNumero       $Numero
086700131104C2228C                   Other
086800131104  "  C                   Eval      WNumero2    = *All'0'
086900131104  "  C                   Move      D1Anno        WAnno2
087000131104  "  C                   Movel     STTPeriodo    WAnno2
087100131104  "  C                   Z-Add     1             WPrgInvio2
087200131104  "  C                   Move      WNumero2      $Numero
087300131104  "  C*
087400131104C2228C                   EndSl
087500131013     C*
087600131013     C                   CALL      'X35NOMFR'
087700131013     C                   PARM                    XScSoc
087800131013     C                   PARM      *Blank        $Unita            8
087900131020     C                   PARM                    $CdScarico        2
088000131013     C                   PARM                    $Numero           9
088100131013     C                   PARM                    $File            10
088200131013     C                   PARM                    Lib022
088300131013     C                   PARM      *Off          $Errore           1
088400131013     C*
088500131013     C                   if        $Errore    <> '0'
088600131013     C                   SETON                                        9499
088700131013     C                   endif
088800131013     C*
088900131013     C                   ENDSR
089000131013      /EJECT
089100131013     C************************************************************
089200131013     C* Controllo i dati prima dello scarico
089300131013     C************************************************************
089400131013     C     CtrDati       BEGSR
089500131013     C*
089600131020     C                   Clear                   NDCP99DS
089700131013     C*
089800131013     C                   Eval      $OkExeLan   = *Off
089900131013     C*
090000131016    >C                   MOVE      XScSoc        SocietaP99
090100131013     C*
090200131016     C                   Eval      AnnoP99     = D1Anno
090300131016     C                   Eval      TpInvioP99  = D1TpInvio
090400131016     C                   Eval      TpElaP99    = D1TpEla
090500131016     C                   Eval      NumElaP99  = D1NumEla
090600131016     C                   Eval      DetElaP99  = D1DetEla
090700131013     C*
090800131016     C                   MOVEL     NDCP99DS      KPJBU
090900131013     C*
091000131016    >C                   CALL      'NDCP99R'
091100131013     C                   PARM                    KPJBA
091200131013     C*
091300131016     C                   MOVEL     KPJBU         NDCP99DS
091400131013     C*
091500131013     C* ritorno da PGM gestione
091600131016     C                   EXSR      RTNP99
091700131013     C*
091800131013     C                   ENDSR
091900131013      /EJECT
092000131013     C************************************************************
092100131013     C* GESTIONE RITORNO DA PGM DI GESTIONE
092200131013     C************************************************************
092300131016     C     RTNP99        BEGSR
092400131013     C*
092500131013     C* funzione non eseguibile per errore :
092600131013     C*
092700131013     C                   SELECT
092800131013     C* 0 = funzione eseguita correttamente
092900131016    >C     ERRP99        WHENEQ    '0'
093000131013     C                   Eval      $OkExeLan = *on
093100131013     C*
093200131013     C*
093300131013     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
093400131016    >C     ERRP99        WHENEQ    '1'
093500131013     C                   EXSR      MsgPgm2
093600131013     C*
093700131013     C                   ENDSL
093800131013     C*
093900131013     C                   ENDSR
094000131013     C/EJECT
094100131013     C************************************************************
094200131013     C* Verifico esistenza file con stesso nome
094300131013     C************************************************************
094400131013     C     CtrInvio      BEGSR
094500131013     C*
094600131013     C                   Eval      $OkExeLan   = *Off
094700131013     C*
094800131013     C* se presenti parametri per creazione file con capacità limitata
094900131013     C* lo segnalo
095000131013     C                   if        D1MaxDim    > 0    or
095100131013     C                             D1MaxRec    > 0
095200131013     C*
095300131013     C                   EXSR      MsgPgm3
095400131013     C*
095500131013     C                   Select
095600131013     C*
095700131013     C* richiesto di continuare
095800131013     C                   When      MsgRtn      = '1'
095900131013     C                   Eval      $OkExeLan   = *On
096000131013     C*
096100131013     C*
096200131013     C* richiesto di non continuare
096300131013     C                   When      MsgRtn      = '2'
096400131013     C                   Eval      $OkExeLan   = *Off
096500131013     C*
096600131013     C                   endsl
096700131013     C*
096800131013     C                   else
096900131013     C* se file non presenti tali parametri, eseguo lancio
097000131013     C                   Eval      $OkExeLan   = *On
097100131013     C*
097200131013     C                   endif
097300131013     C*
097400131013     C                   ENDSR
097500131013      /EJECT
097600131013     C************************************************************
097700131013     C* Messaggio errore
097800131013     C************************************************************
097900131013     C     MsgPgm3       begsr
098000131013      *
098100131013     C                   reset                   XMsgDS
098200131013     C*
098300131013     C* Risposte possibili
098400131013     C* "1) Continuare      2) Annulla
098500131013     C                   Eval      IDMSG       =
098600131013     C                             'PROMSG    *LIBL     COS1204'
098700131013     C                   CALLB     'XRTVM'
098800131013     C                   PARM                    IDMSG
098900131013     C                   PARM      *Blank        TXTMSG
099000131013     C                   PARM                    ERRMSG
099100131013     C     ERRMSG        IFNE      *ON
099200131013     C                   MOVEL     TXTMSG        XMSGTXT
099300131013     C                   ELSE
099400131013     C                   MOVEL     *ALL'?'       XMSGTXT
099500131013     C                   ENDIF
099600131013     C*
099700131013     C                   MOVEL     'S'           MSGEMV
099800131013     C                   Z-ADD     10            MSGRGP
099900131013     C                   MOVEL     'S'           MSGRSP
100000131013     C                   MOVEL     '12'          MSGTPR
100100131013      *
100200131013     C                   call      'XMSGR'
100300131013     C                   parm                    XMsgDS
100400131013     C                   parm                    XMsgTxt         135
100500131013     C*
100600131013     C* Modo di inizializzazione della window:
100700131013     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
100800131013     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
100900131013     C*                       per mantenere il sottofondo del video princip.)
101000131013     C                   parm      '1'           XMsgInz           1
101100131013     C*
101200131013     C* COS2670- (F1) E' stato impostato un parametro per la
101300131013     C* eazione di file da trasmettere con capacità di dati limitata.
101400131013     C*  Se il file che verrà creato avrà capacità (in MegaByte o in
101500131013     C* numero di soggetti) superiore a quanto indicato, verranno
101600131013     C* creati altri file con la capacità richiesta fino a esaurimento
101700131013     C* dati e che dovranno essere traspessi con più invii.
101800131013     C                   parm      'COS2670'     XMsgCd1           7
101900131013     C                   parm                    XMsgCd2           7
102000131013     C                   parm                    XMsgCd3           7
102100131013     C                   parm                    XMsgSn1           7
102200131013     C                   parm                    XMsgCdTxT        63
102300131013     C                   parm                    MsgCdTit         14
102400131013     C                   parm                    MsgClPrtf         1
102500131013     C                   parm                    VarMsg1         100
102600131013     C                   parm                    VarMsg2         100
102700131013     C                   parm                    VarMsg3         100
102800131013     C*
102900131013     C                   ENDSR
103000131013      /EJECT
103100131013     C************************************************************
103200131013     C* Verifico esistenza file con stesso nome
103300131013     C************************************************************
103400131013     C     CtrFileNew    BEGSR
103500131013     C*
103600131013     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
103700131013     C*
103800131013     C                   Eval      Cmd         =
103900131013     C                             'CHKOBJ OBJ(' +
104000131013     C                             %Trim(Lib022) + '/' +
104100131013     C                             %Trim($File)  +
104200131013     C                             ') ' +
104300131013     C                             'OBJTYPE(*FILE)'
104400131013     C*
104500131013     C                   Call      'QCMDEXC'                            21
104600131013     C                   Parm                    Cmd             120
104700131013     C                   Parm      120           Len              15 5
104800131013     C*
104900131013     C*
105000131013     C                   Eval      $OkExeLan   = *Off
105100131013     C*
105200131013     C* se il file è presente nella libreria lo segnalo
105300131013     C                   if        *In21       = *Off
105400131013     C*
105500131013     C                   EXSR      MsgPgm
105600131013     C*
105700131013     C                   Select
105800131013     C*
105900131013     C* richiesto di continuare
106000131013     C                   When      MsgRtn      = '1'
106100131013     C                   Eval      $OkExeLan   = *On
106200131013     C*
106300131013     C*
106400131013     C* richiesto di non continuare
106500131013     C                   When      MsgRtn      = '2'
106600131013     C                   Eval      $OkExeLan   = *Off
106700131013     C*
106800131013     C                   endsl
106900131013     C*
107000131013     C                   else
107100131013     C* se file non presente nella libreria, eseguo lancio
107200131013     C                   Eval      $OkExeLan   = *On
107300131013     C*
107400131013     C                   endif
107500131013     C*
107600131013     C                   ENDSR
107700131013      /EJECT
107800131013     C************************************************************
107900131013     C* Messaggio errore
108000131013     C************************************************************
108100131013     C     MsgPgm        begsr
108200131013      *
108300131013     C                   reset                   XMsgDS
108400131013     C*
108500131013     C* Risposte possibili
108600131013     C* "1) Continuare      2) Annulla
108700131013     C                   Eval      IDMSG       =
108800131013     C                             'PROMSG    *LIBL     COS1204'
108900131013     C                   CALLB     'XRTVM'
109000131013     C                   PARM                    IDMSG
109100131013     C                   PARM      *Blank        TXTMSG
109200131013     C                   PARM                    ERRMSG
109300131013     C     ERRMSG        IFNE      *ON
109400131013     C                   MOVEL     TXTMSG        XMSGTXT
109500131013     C                   ELSE
109600131013     C                   MOVEL     *ALL'?'       XMSGTXT
109700131013     C                   ENDIF
109800131013     C*
109900131013     C                   MOVEL     'S'           MSGEMV
110000131013     C                   Z-ADD     10            MSGRGP
110100131013     C                   MOVEL     'S'           MSGRSP
110200131013     C                   MOVEL     '12'          MSGTPR
110300131013      *
110400131013     C                   movel     $File         $P1COS2161
110500131013     C                   move      Lib022        $P1COS2161
110600131013      *
110700131013     C                   call      'XMSGR'
110800131013     C                   parm                    XMsgDS
110900131013     C                   parm                    XMsgTxt         135
111000131013     C*
111100131013     C* Modo di inizializzazione della window:
111200131013     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
111300131013     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
111400131013     C*                       per mantenere il sottofondo del video princip.)
111500131013     C                   parm      '1'           XMsgInz           1
111600131013     C*
111700131013     C* COS2161-Il file &1 esiste già nella libreria &2'
111800131013     C                   parm      'COS2161'     XMsgCd1           7
111900131013     C                   parm                    XMsgCd2           7
112000131013     C                   parm                    XMsgCd3           7
112100131013     C                   parm                    XMsgSn1           7
112200131013     C                   parm                    XMsgCdTxT        63
112300131013     C                   parm                    MsgCdTit         14
112400131013     C                   parm                    MsgClPrtf         1
112500131013     C                   parm      $P1COS2161    VarMsg1         100
112600131013     C                   parm                    VarMsg2         100
112700131013     C                   parm                    VarMsg3         100
112800131013     C*
112900131013     C                   ENDSR
113000131013      /EJECT
113100131013     C************************************************************
113200131013     C* Messaggio errore da controllo dati
113300131013     C************************************************************
113400131013     C     MsgPgm2       begsr
113500131013      *
113600131013     C                   reset                   XMsgDS
113700131013     C*
113800131013     C                   MOVEL     *Blank        XMSGTXT
113900131013     C                   MOVEL     'S'           MSGEMV
114000131013     C                   Z-ADD     10            MSGRGP
114100131013     C                   MOVEL     'N'           MSGRSP
114200131013     C                   MOVEL     *Blank        MSGTPR
114300131021     C                   Select
114400131021     C                   When      TPERRP99 = ' '
114500131013     C                   movel     'PRO0007'     XMsgCd1
114600131021     C                   When      TPERRp99 = 'A'
114700131021     C                   movel     'COS5159'     XMsgCd1
114800131021     C                   When      TPERRp99 = 'G'
114900131021     C                   movel     'COS5160'     XMsgCd1
115000131021     C                   When      TPERRp99 = 'U'
115100131021     C                   movel     'COS5161'     XMsgCd1
115200131105C2228C                   When      TPERRp99 = 'N'
115300131105C2228C                   movel     'COS5162'     XMsgCd1
115400131021     C                   When      NumQuaP99 = 0
115500131021     C                   movel     'COS5148'     XMsgCd1
115600131021     C                   EndSL
115700131013      *
115800131013     C                   call      'XMSGR'
115900131013     C                   parm                    XMsgDS
116000131013     C                   parm                    XMsgTxt         135
116100131013     C*
116200131013     C* Modo di inizializzazione della window:
116300131013     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
116400131013     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
116500131013     C*                       per mantenere il sottofondo del video princip.)
116600131013     C                   parm      '1'           XMsgInz           1
116700131013     C                   parm                    XMsgCd1           7
116800131013     C                   parm                    XMsgCd2           7
116900131013     C                   parm                    XMsgCd3           7
117000131013     C                   parm                    XMsgSn1           7
117100131013     C                   parm                    XMsgCdTxT        63
117200131013     C                   parm                    MsgCdTit         14
117300131013     C                   parm                    MsgClPrtf         1
117400131013     C                   parm                    VarMsg1         100
117500131013     C                   parm                    VarMsg2         100
117600131013     C                   parm                    VarMsg3         100
117700131013     C*
117800131013     C                   Eval      $OkExeLan   = *Off
117900131013     C*
118000131013     C                   ENDSR
118100131013      /EJECT
118200131013     C************************************************************
118300131013     C* GESTIONE VIDEO RECORD  D2
118400131013     C************************************************************
118500131013    >C     GESD2         BEGSR
118600131013     C* emissione piede videata se proveniente da altra
118700131013     C* salvataggio valore formato di provenienza
118800131013   B1C     $LASTV        IFNE      'D2'
118900131013     C                   WRITE     FMTT1
119000131013     C                   EXSR      GESPIE
119100131013     C                   MOVE      $LASTV        $LASTG
119200131013     C                   MOVE      'D2'          $LASTV
119300131013   E1C                   ENDIF
119400131013     C*
119500131013     C* se esistono errori sulla videata
119600131013     C* emetto la write del formato a indicatori spenti per vedere
119700131013     C* le eventuali decodifiche
119800131013   B1C     *IN99         IFEQ      *ON
119900131013     C     $GEST         ANDEQ     'D2'
120000131013     C                   MOVEA     *IN           WIN
120100131013     C                   MOVE      *ALL'0'       IN5098           49
120200131013     C                   MOVEA     IN5098        *IN(50)
120300131013     C                   WRITE     FMTD2
120400131013     C                   MOVEA     WIN           *IN
120500131013   E1C                   ENDIF
120600131013     C*
120700131013     C                   EXFMT     FMTD2
120800131013     C*
120900131013     C* controllo abilitazione tasti funzione
121000131013     C                   EXSR      CTRKD1
121100131013     C*
121200131013   B1C     *IN99         IFEQ      *OFF
121300131013     C*
121400131013   B2C                   SELECT
121500131013     C* ROLDOWN
121600131013     C     $TASTO        WHENEQ    ROLDWN
121700131013     C                   MOVE      'D1'          $GEST
121800131013     C* F3=Fine
121900131013     C     $TASTO        WHENEQ    F03
122000131013     C                   EXSR      F03D1
122100131013     C* F4=Ricerca
122200131013     C     $TASTO        WHENEQ    F04
122300131013     C                   EXSR      SEARCH
122400131013     C* F5=Refresh
122500131013     C     $TASTO        WHENEQ    F05
122600131013     C                   EXSR      F05D2
122700131013     C* F8=Successivo
122800131013     C     $TASTO        WHENEQ    F08
122900131013     C                   EXSR      F08D1
123000131013     C* F12=Ritorno
123100131013     C     $TASTO        WHENEQ    F12
123200131013     C                   EXSR      F12D2
123300131013     C* F18=Valori assunti
123400131013     C     $TASTO        WHENEQ    F18
123500131013     C                   EXSR      F18D1
123600131013     C* F21=Pers. batch
123700131013     C     $TASTO        WHENEQ    F21
123800131013     C                   EXSR      F21D1
123900131013     C* F24=Altri tasti
124000131013     C     $TASTO        WHENEQ    F24
124100131013     C                   EXSR      F24D1
124200131013     C*
124300131013   X2C                   OTHER
124400131013     C                   EXSR      CTRD2
124500131013   B3C     *IN99         IFEQ      *OFF
124600131013     C                   EXSR      CTRD1
124700131013   B4C     *IN99         IFEQ      *ON
124800131013     C                   SETON                                            96
124900131013   E4C                   ENDIF
125000131013   E3C                   ENDIF
125100131013     C*
125200131013     C*
125300131013   B3C     $TASTO        IFEQ      F06
125400131013     C     *IN99         ANDEQ     *OFF
125500131013     C*
125600131013     C* controllo se i dati sono ok
125700131013     C                   EXSR      CtrDati
125800131013     C*
125900131013     C* avverto per richiesta di file con capacità limitata
126000131013     C                   EXSR      CtrInvio
126100131013     C*
126200131013     C                   if        $OkExeLan   = *On
126300131013     C* controllo se esiste già il file con il nome reperito
126400131013     C                   EXSR      CtrFileNew
126500131013     C                   endif
126600131013     C*
126700131013     C                   if        $OkExeLan   = *On
126800131013     C*
126900131013     C* eseguo lancio
127000131013     C                   EXSR      EXELAN
127100131013     C                   EXSR      ENDPGM
127200131013     C*
127300131013     C                   endif
127400131013     C*
127500131013   E3C                   ENDIF
127600131013     C*
127700131013   E2C                   ENDSL
127800131013     C*
127900131013   E1C                   ENDIF
128000131013     C*
128100131013    >C                   ENDSR
128200131013     C/EJECT
128300131013     C************************************************************
128400131013     C* GESTIONE VALORI ASSUNTI
128500131013     C************************************************************
128600131013     C     GESD3         BEGSR
128700131013     C*
128800131013   B1C     $INZD3        IFEQ      *ON
128900131013     C                   EXSR      INZD3
129000131013     C                   MOVE      *OFF          $INZD3
129100131013   E1C                   ENDIF
129200131013     C*
129300131013   B1C     $LASTV        IFNE      'D3'
129400131013     C                   EXSR      GESPIE
129500131013     C                   MOVE      $LASTV        $LASTG
129600131013     C                   MOVE      'D3'          $LASTV
129700131013   E1C                   ENDIF
129800131013     C*
129900131013     C* se esistono errori sulla videata
130000131013     C* emetto la write del formato a indicatori spenti per vedere
130100131013     C* le eventuali decodifiche
130200131013   B1C     *IN99         IFEQ      *ON
130300131013     C                   MOVEA     *IN           WIN
130400131013     C                   MOVE      *ALL'0'       IN5098           49
130500131013     C                   MOVEA     IN5098        *IN(50)
130600131013     C                   WRITE     FMTD3
130700131013     C                   MOVEA     WIN           *IN
130800131013   E1C                   ENDIF
130900131013     C*
131000131013     C                   EXFMT     FMTD3
131100131013     C*
131200131013     C* controllo abilitazione tasti funzione
131300131013     C                   EXSR      CTRKD3
131400131013     C*
131500131013   B1C     *IN99         IFEQ      *OFF
131600131013     C*
131700131013   B2C                   SELECT
131800131013     C* F3=Fine
131900131013     C     $TASTO        WHENEQ    F03
132000131013     C                   EXSR      F03D1
132100131013     C* F12=Ritorno
132200131013     C     $TASTO        WHENEQ    F12
132300131013     C                   EXSR      F12D3
132400131013     C* F24=Altri tasti
132500131013     C     $TASTO        WHENEQ    F24
132600131013     C                   EXSR      F24D3
132700131013     C*
132800131013     C                   OTHER
132900131013     C                   EXSR      CTRD3
133000131013     C     *IN99         IFEQ      *OFF
133100131013     C                   MOVE      $LASTG        $GEST
133200131013     C                   ENDIF
133300131013     C*
133400131013   E2C                   ENDSL
133500131013     C*
133600131013   E1C                   ENDIF
133700131013     C*
133800131013     C                   ENDSR
133900131013     C/EJECT
134000131013     C************************************************************
134100131013     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
134200131013     C************************************************************
134300131013     C     INZD3         BEGSR
134400131013     C*
134500131013     C                   CLEAR                   FMTD3
134600131013     C     $VAL          IFEQ      *ON
134700131013     C                   MOVE      XSCON         D3VAL
134800131013     C                   ELSE
134900131013     C                   MOVE      XSCOFF        D3VAL
135000131013     C                   ENDIF
135100131013     C                   MOVE      XSCON         D3ON
135200131013     C                   MOVE      XSCOFF        D3OF
135300131013     C*
135400131013     C                   ENDSR
135500131013     C/EJECT
135600131013     C************************************************************
135700131013     C* RIEMPIMENTO VIDEATA D2
135800131013     C************************************************************
135900131013    >C     RIED2         BEGSR
136000131013     C*
136100131013     C*
136200131013    >C                   ENDSR
136300131013     C/EJECT
136400131013     C************************************************************
136500131013     C* GESTIONE F05 VIDEO D2
136600131013     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
136700131013     C* e il riposizionamento alla prima
136800131013     C************************************************************
136900131013    >C     F05D2         BEGSR
137000131013     C*
137100131013     C                   MOVE      *ON           $INZD1
137200131013     C                   MOVE      'D1'          $GEST
137300131013     C*
137400131013    >C                   ENDSR
137500131013     C/EJECT
137600131013     C************************************************************
137700131013     C* GESTIONE F12 VIDEO D2
137800131013     C************************************************************
137900131013     C     F12D2         BEGSR
138000131013     C*
138100131013     C                   MOVE      'D1'          $GEST
138200131013     C*
138300131013     C                   ENDSR
138400131013     C/EJECT
138500131013     C************************************************************
138600131013     C* GESTIONE F12 VIDEO D3
138700131013     C************************************************************
138800131013     C     F12D3         BEGSR
138900131013     C*
139000131013     C                   MOVE      $LASTG        $GEST
139100131013     C*
139200131013     C                   ENDSR
139300131013     C/EJECT
139400131013     C************************************************************
139500131013     C* CONTROLLO VIDEATA  D2
139600131013     C************************************************************
139700131013    >C     CTRD2         BEGSR
139800131013     C*
139900131013     C                   SETOFF                                       99
140000131013     C* non imposto pos. del cursore
140100131013     C                   Z-ADD     0             H1RIGA
140200131013     C                   Z-ADD     0             H1COLO
140300131013     C*
140400131013     C                   ENDSR
140500131013     C/EJECT
140600131013     C************************************************************
140700131013     C* CONTROLLO VIDEATA  D3
140800131013     C************************************************************
140900131013     C     CTRD3         BEGSR
141000131013     C*
141100131013     C                   SETOFF                                       99
141200131013     C* non imposto pos. del cursore
141300131013     C                   Z-ADD     0             H1RIGA
141400131013     C                   Z-ADD     0             H1COLO
141500131013     C*
141600131013     c* Controllo VALORE
141700131013     C     D3VAL         IFNE      XSCON
141800131013     C     D3VAL         ANDNE     XSCOFF
141900131013     C                   SETON                                        5099
142000131013     C                   ENDIF
142100131013     C*
142200131013     C     D3VAL         IFEQ      XSCON
142300131013     C                   MOVE      *ON           $VAL
142400131013     C                   ELSE
142500131013     C                   MOVE      *OFF          $val
142600131013     c                   endif
142700131013     C*
142800131013     C                   ENDSR
142900131013     C/EJECT
143000131013     C************************************************************
143100131013     C* ESECUZIONE LANCIO
143200131013     C************************************************************
143300131013     C     EXELAN        BEGSR
143400131013     C*
143500131013     C                   RESET                   NDCP96DS
143600131013     C*
143700131013    >C                   MOVE      XSCSOC        SocietaP96
143800131013     C*
143900131013     C                   Eval      AnnoP96     = D1Anno
144000131013     C                   Eval      FileNewP96  = $File
144100131013     C                   Eval      LibP96      = Lib022
144200131015     C                   Eval      TpElaP96    = D1TpEla
144300131016     C                   Eval      NumElaP96   = D1NumEla
144400131013     C                   Eval      TpInvioP96  = D1TpInvio
144500131013     C                   Eval      ProtSosP96  = D1ProtSos
144600131013     C                   Eval      ProtDocP96  = D1ProtDoc
144700131013     C                   Eval      MaxDimP96   = D1MaxDim
144800131013     C                   Eval      MaxRecP96   = D1MaxRec
144900131013     C*
145000131013     C*
145100131013     C                   MOVEL     NDCP96DS      KPJBU
145200131013     C*
145300131013     C                   CALL      'NDCP96R'
145400131013     C                   PARM                    KPJBA
145500131013     C*
145600131013     C                   MOVEL     KPJBU         NDCP96DS
145700131013     C*
145800131013     C* ritorno da PGM gestione
145900131013     C                   EXSR      RTNP96
146000131013     C*
146100131013     C* aggiorno valori assunti
146200131013     C                   EXSR      AGGMPE
146300131013     C*
146400131013     C                   ENDSR
146500131013     C/EJECT
146600131013     C************************************************************
146700131013     C* GESTIONE RITORNO DA PGM DI GESTIONE
146800131013     C************************************************************
146900131013     C     RTNP96        BEGSR
147000131013     C*
147100131013     C* funzione non eseguibile per errore :
147200131013     C*
147300131013     C                   SELECT
147400131013     C* 0 = funzione eseguita correttamente
147500131013    >C     ERRP96        WHENEQ    '0'
147600131013     C                   Eval      $Fine       = *On
147700131013     C*
147800131013     C*
147900131013     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
148000131013    >C     ERRP96        WHENEQ    '1'
148100131013    >C     ERRP96        OREQ      '2'
148200131013     C                   SETON                                        9399      emiss.SFLMSGID
148300131013     C*
148400131013     C                   ENDSL
148500131013     C*
148600131013     C                   ENDSR
148700131013     C/EJECT
148800131013     C************************************************************
148900131013     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
149000131013     C************************************************************
149100131013     C     CTRKD1        BEGSR
149200131013     C*
149300131013     C                   SETOFF                                       99
149400131013     C                   MOVEA     $FC1          $FC
149500131013     C                   EXSR      CTRKEY
149600131013     C     *IN99         IFEQ      *ON
149700131013     C                   SETON                                        97
149800131013     C                   ENDIF
149900131013     C*
150000131013     C                   ENDSR
150100131013     C/EJECT
150200131013     C************************************************************
150300131013     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
150400131013     C* INPUT
150500131013     C* $FC3 = Abilitazioni videata D3
150600131013     C* OUTPUT
150700131013     C* *IN97 *IN99 Errori
150800131013     C************************************************************
150900131013     C     CTRKD3        BEGSR
151000131013     C*
151100131013     C                   SETOFF                                       99
151200131013     C                   MOVEA     $FC3          $FC
151300131013     C                   EXSR      CTRKEY
151400131013     C     *IN99         IFEQ      *ON
151500131013     C                   SETON                                        97
151600131013     C                   ENDIF
151700131013     C*
151800131013     C                   ENDSR
151900131013     C/EJECT
152000131013     C************************************************************
152100131013     C* CONTROLLO ABILITAZIONE TASTI
152200131013     C************************************************************
152300131013     C     CTRKEY        BEGSR
152400131013     C*
152500131013     C                   SELECT
152600131013     C     $TASTO        WHENEQ    F01
152700131013     C     $FC(1)        IFEQ      '0'
152800131013     C                   SETON                                        99
152900131013     C                   ENDIF
153000131013     C     $TASTO        WHENEQ    F02
153100131013     C     $FC(2)        IFEQ      '0'
153200131013     C                   SETON                                        99
153300131013     C                   ENDIF
153400131013     C     $TASTO        WHENEQ    F03
153500131013     C     $FC(3)        IFEQ      '0'
153600131013     C                   SETON                                        99
153700131013     C                   ENDIF
153800131013     C     $TASTO        WHENEQ    F04
153900131013     C     $FC(4)        IFEQ      '0'
154000131013     C                   SETON                                        99
154100131013     C                   ENDIF
154200131013     C     $TASTO        WHENEQ    F05
154300131013     C     $FC(5)        IFEQ      '0'
154400131013     C                   SETON                                        99
154500131013     C                   ENDIF
154600131013     C     $TASTO        WHENEQ    F06
154700131013     C     $FC(6)        IFEQ      '0'
154800131013     C                   SETON                                        99
154900131013     C                   ENDIF
155000131013     C     $TASTO        WHENEQ    F07
155100131013     C     $FC(7)        IFEQ      '0'
155200131013     C                   SETON                                        99
155300131013     C                   ENDIF
155400131013     C     $TASTO        WHENEQ    F08
155500131013     C     $FC(8)        IFEQ      '0'
155600131013     C                   SETON                                        99
155700131013     C                   ENDIF
155800131013     C     $TASTO        WHENEQ    F09
155900131013     C     $FC(09)       IFEQ      '0'
156000131013     C                   SETON                                        99
156100131013     C                   ENDIF
156200131013     C     $TASTO        WHENEQ    F10
156300131013     C     $FC(10)       IFEQ      '0'
156400131013     C                   SETON                                        99
156500131013     C                   ENDIF
156600131013     C     $TASTO        WHENEQ    F11
156700131013     C     $FC(11)       IFEQ      '0'
156800131013     C                   SETON                                        99
156900131013     C                   ENDIF
157000131013     C     $TASTO        WHENEQ    F12
157100131013     C     $FC(12)       IFEQ      '0'
157200131013     C                   SETON                                        99
157300131013     C                   ENDIF
157400131013     C     $TASTO        WHENEQ    F13
157500131013     C     $FC(13)       IFEQ      '0'
157600131013     C                   SETON                                        99
157700131013     C                   ENDIF
157800131013     C     $TASTO        WHENEQ    F14
157900131013     C     $FC(14)       IFEQ      '0'
158000131013     C                   SETON                                        99
158100131013     C                   ENDIF
158200131013     C     $TASTO        WHENEQ    F15
158300131013     C     $FC(15)       IFEQ      '0'
158400131013     C                   SETON                                        99
158500131013     C                   ENDIF
158600131013     C     $TASTO        WHENEQ    F16
158700131013     C     $FC(16)       IFEQ      '0'
158800131013     C                   SETON                                        99
158900131013     C                   ENDIF
159000131013     C     $TASTO        WHENEQ    F17
159100131013     C     $FC(17)       IFEQ      '0'
159200131013     C                   SETON                                        99
159300131013     C                   ENDIF
159400131013     C     $TASTO        WHENEQ    F18
159500131013     C     $FC(18)       IFEQ      '0'
159600131013     C                   SETON                                        99
159700131013     C                   ENDIF
159800131013     C     $TASTO        WHENEQ    F19
159900131013     C     $FC(19)       IFEQ      '0'
160000131013     C                   SETON                                        99
160100131013     C                   ENDIF
160200131013     C     $TASTO        WHENEQ    F20
160300131013     C     $FC(20)       IFEQ      '0'
160400131013     C                   SETON                                        99
160500131013     C                   ENDIF
160600131013     C     $TASTO        WHENEQ    F21
160700131013     C     $FC(21)       IFEQ      '0'
160800131013     C                   SETON                                        99
160900131013     C                   ENDIF
161000131013     C     $TASTO        WHENEQ    F22
161100131013     C     $FC(22)       IFEQ      '0'
161200131013     C                   SETON                                        99
161300131013     C                   ENDIF
161400131013     C     $TASTO        WHENEQ    F23
161500131013     C     $FC(23)       IFEQ      '0'
161600131013     C                   SETON                                        99
161700131013     C                   ENDIF
161800131013     C     $TASTO        WHENEQ    F24
161900131013     C     $FC(24)       IFEQ      '0'
162000131013     C                   SETON                                        99
162100131013     C                   ENDIF
162200131013     C                   ENDSL
162300131013     C*
162400131013     C                   ENDSR
162500131013     C/EJECT
162600131013     C************************************************************
162700131013     C* OPERAZIONI INIZIALI
162800131013     C************************************************************
162900131013     C     *INZSR        BEGSR
163000131013     C*
163100131013     C* Reperimento parametri
163200131013     C*
163300131013     C     *ENTRY        PLIST
163400131013     C                   PARM                    KPJBA
163500131013     C*
163600131013     C* Reperimento tasti di funzione
163700131013     C*
163800131013     C                   EXSR      REPKEY
163900131013     C*
164000131013     C                   ENDSR
164100131013     C/EJECT
164200131013     C************************************************************
164300131013     C* REPERIMENTO TASTI DI FUNZIONE
164400131013     C************************************************************
164500131013     C     REPKEY        BEGSR
164600131013     C                   MOVEA     $FM1          $FM
164700131013     C                   MOVEA     $Fc1          $Fc
164800131013     C                   EXSR      REPKKK
164900131013     C                   MOVEA     $FX           $FX1
165000131013     C*
165100131013     C                   MOVEA     $FM3          $FM
165200131013     C                   MOVEA     $Fc3          $FM
165300131013     C                   EXSR      REPKKK
165400131013     C                   MOVEA     $FX           $FX3
165500131013     C*
165600131013     C                   ENDSR
165700131013     C/EJECT
165800131013     C************************************************************
165900131013     C* REPERIMENTO TASTI DI FUNZIONE
166000131013     C************************************************************
166100131013     C     REPKKK        BEGSR
166200131013     C*
166300131013     C                   CLEAR                   $FX
166400131013     C     1             DO        24            X
166500131013     C*
166600131013     C     $FM(X)        IFNE      *BLANK
166700131013     C     $fc(X)        andne     '0'
166800131013     C*
166900131013     C                   MOVEL     $FM(X)        IDMSG
167000131013     C                   CALLB     'XRTVM'
167100131013     C                   PARM                    IDMSG            27
167200131013     C                   PARM                    TXTMSG          100
167300131013     C                   PARM                    ERRMSG            1
167400131013     C*
167500131013     C     ERRMSG        IFNE      *ON
167600131013     C                   MOVEL     TXTMSG        $FX(X)
167700131013     C                   ELSE
167800131013     C                   MOVEL     *ALL' '       $FX(X)
167900131013     C                   ENDIF
168000131013     C*
168100131013     C                   ENDIF
168200131013     C*
168300131013     C                   ENDDO
168400131013     C*
168500131013     C                   ENDSR
168600131013     C/EJECT
168700131013     C************************************************************
168800131013     C* INIZIALIZZAZIONE VARIABILI
168900131013     C************************************************************
169000131013     C     INZVAR        BEGSR
169100131013     C*
169200131013     C* se il pgm è stato richiamato imposto i valori ricevuti dalla DS
169300131013     C                   if        KPJBU      <> *Blanks
169400131013     C                   Eval      CallPgm     = '1'
169500131015     C                   MoveL     KPJBU         NDCP95DS
169600131013     C                   else
169700131013     C                   Eval      CallPgm     = '0'
169800131013     C                   endif
169900131013     C*
170000131013     C* Reperimento dati società
170100131013     C*
170200131013    >C                   MOVEL     'SOC001'      TIPXSC
170300131013     C                   MOVEL     *BLANK        SOCXSC
170400131013     C                   EXSR      REPSOC
170500131013     C     RTNXSC        IFNE      '1'
170600131013     C                   MOVEL     XSOCDS        SOC001
170700131013     C                   ELSE
170800131013     C                   RETURN
170900131013     C                   ENDIF
171000131013     C* Reperimento parametro modulo
171100131013     C                   Clear                   ANP098DS
171200131013     C*
171300131013     C                   CALLB     'XPAR'
171400131013     C                   Parm                    XScSoc
171500131013     C                   Parm      'PJSPES13'    XPaPar            8
171600131013     C                   Parm                    XPaOut         2000
171700131013     C                   Parm      *OFF          XPaErr            1
171800131013     C                   Parm      ' '           XPaRic            1
171900131013     C*
172000131013     C                   if        XPaErr      = *On
172100131013     C*
172200131013     C* Errore: fallito reperimento recupero parametro modulo &1
172300131013     C* Valorizzo le variabili del msg PRO2184
172400131013     C                   MoveL     XPaPar        P1Param
172500131013     C                   SetOn                                        5999
172600131013     C*
172700131013     C                   else
172800131013     C                   MoveL     XPaOut        ANP098DS
172900131013     C                   endif
173000131013     C* Asteriscare se non presenti
173100131013     C* Reperimento autorizzazioni
173200131013     C*
173300131013     C                   EXSR      REPAUT
173400131013     C*
173500131013     C* Reperimento valori default utente
173600131013     C*
173700131013     C                   EXSR      REPDFT
173800131013     C*
173900131013     C* Definizione dei tasti funzione utilizzabili a pgm
174000131013     C*
174100131013     C                   EXSR      DEFOPZ
174200131013     C*
174300131013     C* Variabili per gestione videate
174400131013     C*
174500131013     C                   MOVE      'D1'          $GEST
174600131013     C                   MOVE      *OFF          $FINE
174700131013     C                   MOVE      *ON           $INZD1
174800131013     C                   MOVE      *BLANK        $LASTG
174900131013     C                   MOVE      *BLANK        $LASTV
175000131013     C                   Z-ADD     0             $ULKD1            3 0
175100131013     C                   Z-ADD     0             $ULKD3            3 0
175200131013     C*
175300131013     C* Variabili appoggio
175400131013     C*
175500131013     C                   Z-ADD     0             CURR
175600131013     C                   Z-ADD     0             CURC
175700131013     C*
175800131013     C* Valorizzazione campi univoci testate
175900131013     C*
176000131013     C                   CLEAR                   FMTT1
176100131013     C                   MOVEL     KNSIF         NOMSIF
176200131013     C                   MOVEL     XSCDSI        NOMDIT
176300131013     C                   MOVEL     DSPGM         NOMPGM
176400131013     C* recupero il sottotitolo (se c'è)
176500131013     C                   MOVEL     *BLANK        T1TIT
176600131013     C     SOTTIT        IFNE      *BLANK
176700131013     C                   MOVEL     SOTTIT        IDMSG
176800131013     C                   CALLB     'XRTVM'
176900131013     C                   PARM                    IDMSG            27
177000131013     C                   PARM                    TXTMSG          100
177100131013     C                   PARM                    ERRMSG            1
177200131013     C                   PARM      *ON           CTRMSG            1
177300131013     C                   PARM      30            LENMSG            3 0
177400131013     C     ERRMSG        IFNE      *ON
177500131013     C                   MOVEL     TXTMSG        T1TIT
177600131013     C                   ELSE
177700131013     C                   MOVEL     *ALL'?'       T1TIT
177800131013     C                   ENDIF
177900131013     C                   ENDIF
178000131013     C*
178100131013     C* Reperisco la libreria in cui vengono memorizzati i file
178200131013     C                   EXSR      RepLib
178300131013     C*
178400131013     C* Inizializzazione/Decodifica prima videata
178500131013     C*
178600131013     C                   EXSR      INZD1
178700131013     C                   MOVE      *OFF          $INZD1
178800131013     C*
178900131013     C                   ENDSR
179000131013     C/EJECT
179100131013     C************************************************************
179200131013     C* REPERIMENTO DATI SOCIETA'
179300131013     C************************************************************
179400131013     C     REPSOC        BEGSR
179500131013     C*
179600131013     C                   CALLB     'XSOC'
179700131013     C                   PARM                    TIPXSC            6
179800131013     C                   PARM                    SOCXSC            3
179900131013     C                   PARM                    CDSXSC            9 0
180000131013     C                   PARM                    MODXSC            3
180100131013     C                   PARM      *BLANKS       RTNXSC            1
180200131013     C                   PARM                    XSOCDS
180300131013     C                   PARM                    KPJBA
180400131013     C*
180500131013     C                   ENDSR
180600131013     C/EJECT
180700131013     C************************************************************
180800131013     C* REPERIMENTO AUTORIZZAZIONI UTENTE
180900131013     C************************************************************
181000131013     C     REPAUT        BEGSR
181100131013     C*
181200131013     C                   CLEAR                   AUTDS
181300131013     C*
181400131013     C                   MOVEL     XSCSOC        XCASOC
181500131013     C                   MOVEL     XSCDUN        XCAUNI
181600131013     C                   Z-ADD     0             XCAGRP
181700131013     C                   MOVEL     KNMUS         XCAPRF
181800131013    >C                   MOVEL     'CONCIV'      XCAFNC
181900131013    >C                   MOVEL     *ZERO         XCAVFU
182000131013    >C                   MOVE      'CK'          XCATCK
182100131013    >C                   MOVE      '1'           XCANAU
182200131013     C*
182300131013     C                   CALLB     'XCHKAUT'
182400131013     C                   PARM                    AUTDS
182500131013     C*
182600131013     C* errori/non abilitazione
182700131013     C     XCARTN        IFNE      0
182800131013     C                   MOVE      *ON           $fine
182900131013     C                   EXSR      ENDPGM
183000131013     C                   ELSE
183100131013     C                   MOVEL     XCADDS        CONCIVDS
183200131013     C* Se non autorizzati alla funzione oppure                              ed
183300131013     C     ESTALL        IFEQ      *OFF
183400131013     C                   MOVE      XCAFNC        XNOFNC
183500131013     C                   MOVE      XCAPRF        XNOPRF
183600131013     C                   MOVE      XCACUT        XNOCUT
183700131013     C                   MOVE      XCATCK        XNOTCK
183800131013     C                   MOVE      '1'           XNOERR
183900131013     C                   CALL      'XNOAUTR'
184000131013     C                   PARM                    XNOADS
184100131013     C                   MOVE      *ON           $fine
184200131013     C                   EXSR      ENDPGM
184300131013     C                   ENDIF
184400131013     C                   ENDIF
184500131013     C*
184600131013     C                   ENDSR
184700131013     C/EJECT
184800131013     C************************************************************
184900131013     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
185000131013     C************************************************************
185100131013     C     REPDFT        BEGSR
185200131013     C*
185300131013     C* preparo i parametri di chiamata al driver
185400131013     C                   CLEAR                   XMPEDS
185500131013     C                   EVAL      TpChiamata = '01'
185600131013     C                   EVAL      NChiavi = 5
185700131013     C                   EVAL      VLogica = 2
185800131013     C*
185900131013     C* preparo le chiavi per la chiamata al driver
186000131013     C                   CLEAR                   XMPEDS1
186100131013     C                   MOVEL     XSCSOC        DSMPESOC
186200131013     C                   MOVEL     DSPGM         DSMPEPGM
186300131013     C                   MOVEL     KNMUS         DSMPEPRF
186400131013     C* Si reperiscono i dati di default a seconda delle modalità con
186500131013     C* cui il pgm è usato
186600131013     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
186700131013     C* MPETIP = 'PV'  pgm richiamato da menù
186800131013     C* MPETIP = 'PB'  x lancio Batch
186900131013     C                   MOVEL     'PB'          DSMPETIP
187000131013     C*
187100131013     C                   CALLB     'XMPE'
187200131013     C                   PARM                    XMPEDS
187300131013     C                   PARM                    XMPEDS1
187400131013     C*
187500131013     C     Risultato     IFEQ      '0'
187600131013     C                   MOVEL     DSMPEPAR      DSMPE
187700131013     C* nel caso sia la prima volta che l'utente apre questa anagrafica
187800131013     C* i suoi valori di default vengono decisi a pgm
187900131013     C*
188000131013     C                   if        %SubSt(DsMPE: 6: 2)  = *Blank
188100131013     C                   Eval      %SubSt(DsMPE: 6: 2)  = '00'
188200131013     C                   endif
188300131013     C*
188400131013     C                   if        %SubSt(DsMPE: 8: 5)  = *Blank
188500131013     C                   Eval      %SubSt(DsMPE: 8: 5)  = '00000'
188600131013     C                   endif
188700131013     C*
188800131013     C                   ELSE
188900131013     C                   ENDIF
189000131013     C*
189100131013     C                   ENDSR
189200131013     C/EJECT
189300131013     C************************************************************
189400131013     C* Rerimento libreria per scarico su supporto magnetico
189500131013     C************************************************************
189600131013     C     REPLib        BEGSR
189700131013     C*
189800131013     C                   Clear                   ANP022DS
189900131013     C*
190000131013     C                   CALLB     'XPAR'
190100131013     C                   Parm                    XScSoc
190200131013     C                   Parm      'SCARSUMA'    XPaPar            8
190300131013     C                   Parm                    XPaOut         2000
190400131013     C                   Parm      *OFF          XPaErr            1
190500131013     C                   Parm      ' '           XPaRic            1
190600131013     C*
190700131013     C                   if        XPaErr      = *On
190800131013     C*
190900131013     C* Errore: fallito reperimento recupero parametro modulo &1
191000131013     C* Valorizzo le variabili del msg PRO2184
191100131013     C                   MoveL     XPaPar        P1Param
191200131013     C                   SetOn                                        5999
191300131013     C*
191400131013     C                   else
191500131013     C                   MoveL     XPaOut        ANP022DS
191600131013     C                   endif
191700131013     C*
191800131013     C                   ENDSR
191900131013     C/EJECT
192000131013     C************************************************************
192100131013     C* AGGIORNAMENTO VALORI ASSUNTI
192200131013     C************************************************************
192300131013     C     AGGMPE        BEGSR
192400131013     C*
192500131013     C* preparo i parametri di chiamata al driver
192600131013     C                   CLEAR                   XMPEDS
192700131013     C                   EVAL      TpChiamata = '03'
192800131013     C                   EVAL      NChiavi = 5
192900131013     C                   EVAL      VLogica = 2
193000131013     C*
193100131013     C* preparo le chiavi per la chiamata al driver
193200131013     C                   CLEAR                   XMPEDS1
193300131013     C                   MOVEL     XSCSOC        DSMPESOC
193400131013     C                   MOVEL     DSPGM         DSMPEPGM
193500131013     C                   MOVEL     KNMUS         DSMPEPRF
193600131013     C*
193700131013     C* Si memorizzano i dati di default a seconda delle modalità con
193800131013     C* cui il pgm è usato
193900131013     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
194000131013     C* MPETIP = 'PV'  pgm richiamato da menù
194100131013     C* MPETIP = 'PB'  x lancio Batch
194200131013     C*
194300131013     C                   MOVEL     'PB'          DSMPETIP
194400131013     C* dati
194500131013     C                   Move      D1Anno        $Anno
194600131015     C                   Movel     D1TpEla       $TpEla
194700131013     C                   Movel     D1TpInvio     $TpInvio
194800131013     C                   Z-Add     D1ProtSos     $ProtSos
194900131013     C                   Z-Add     D1ProtDoc     $ProtDoc
195000131013     C                   Z-Add     D1MaxDim      $MaxDim
195100131013     C                   Z-Add     D1MaxRec      $MaxRec
195200131013     C*
195300131013     C*
195400131013     C                   MOVEL     DSMPE         DSMPEPAR
195500131013     C*
195600131013     C                   CALLB     'XMPE'
195700131013     C                   PARM                    XMPEDS
195800131013     C                   PARM                    XMPEDS1
195900131013     C*
196000131013     C                   ENDSR
196100131013     C/EJECT
196200131013     C************************************************************
196300131013     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
196400131013     C************************************************************
196500131013     C     DEFOPZ        BEGSR
196600131013     C*
196700131013     C* Questa routine deve valorizzare la schiera $FC1/$FR1
196800131013     C* con S o N a seconda della utilizzabilità o meno
196900131013     C* del tasto funzione correlato
197000131013     C*
197100131013     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
197200131013     C* di compilazione
197300131013     C                   RESET                   $FC1
197400131013     C                   RESET                   $FR1
197500131013     C                   RESET                   $FC3
197600131013     C                   RESET                   $FR3
197700131013     C*
197800131013     C                   ENDSR
197900131013     C/EJECT
198000131013     C************************************************************
198100131013     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
198200131013     C************************************************************
198300131013     C     DEFCAM        BEGSR
198400131013     C*
198500131013     C* klist
198600131013     C*
198700131013     C*
198800131013     C* Variabili per gestione videate
198900131013     C*
199000131013     C                   MOVE      *BLANK        $GEST             2
199100131013     C                   MOVE      *BLANK        $FINE             1
199200131013     C                   MOVE      *BLANK        $INZD1            1
199300131013     C                   MOVE      *BLANK        $INZD3            1
199400131013     C                   MOVE      *BLANK        $LASTG            2
199500131013     C                   MOVE      *BLANK        $LASTV            2
199600131013     C                   Z-ADD     0             $PIED1            3 0
199700131013     C*
199800131013     C* Variabili appoggio
199900131013     C*
200000131013     C                   Z-ADD     0             CURR              2 0
200100131013     C                   Z-ADD     0             CURC              2 0
200200131013     C                   MOVE      *ZEROS        WIN              99
200300131013     C*
200400131013     C* Indici
200500131013     C*
200600131013     C                   Z-ADD     0             X                 3 0
200700131013     C                   Z-ADD     0             Y                 3 0
200800131013     C*
200900131013     C                   ENDSR
201000131013     C/EJECT
201100131013     C************************************************************
201200131013     C* RIEMPIMENTO TASTI FUNZIONE
201300131013     C************************************************************
201400131013     C     RIEKEY        BEGSR
201500131013     C*
201600131013     C* Input :
201700131013     C* - $FX = schiera con tutti i testi disponibili
201800131013     C* - $FC = schiere con i flag di validità associati ai testi
201900131013     C* - $FR = schiere con i flag di REVERSE IMAGE
202000131013     C* - $ULK = ultimo testo caricato
202100131013     C* - $L01 = Lunghezza prima riga
202200131013     C* - $L02 = Lunghezza seconda riga
202300131013     C* Output :
202400131013     C* - $ULK
202500131013     C* - $KEY1= 1° riga di tasti funzione
202600131013     C* - $KEY2= 2° riga di tasti funzione
202700131013     C*
202800131013     C                   CALLB     'XKEYMSG'
202900131013     C* input
203000131013     C                   PARM                    $FX
203100131013     C                   PARM                    $FC
203200131013     C                   PARM                    $FR
203300131013     C                   PARM                    $ULK              3 0
203400131013     C* output
203500131013     C                   PARM                    $KEY1            79
203600131013     C                   PARM                    $KEY2            79
203700131013    >C* input
203800131013    >C                   PARM                    $L01              3 0
203900131013    >C                   PARM                    $L02              3 0
204000131013     C*
204100131013     C                   PARM                    $ALLFUNCT       480
204200131013     C*
204300131013     C                   ENDSR
204400131015     C/EJECT
204500131015     C************************************************************
204600131015     C* DEFINIZIONE KLIST
204700131015     C************************************************************
204800131015     C     DefKlist      BEGSR
204900131015     C*
205000131015     C* klist
205100131016     C     K03STT02      KList
205200131015     C                   KFld                    STTSOCIETA
205300131015     C                   KFld                    STTAnno
205400131015     C                   KFld                    STTtpEla
205500131016     C*
205600131016     C     K05STT02      KList
205700131016     C                   KFld                    STTSOCIETA
205800131016     C                   KFld                    STTAnno
205900131016     C                   KFld                    STTtpEla
206000131016     C                   KFld                    STTTipPer
206100131016     C                   KFld                    STTPeriodo
206200131015     C*
206300131015     C                   ENDSR
206400131015     C/EJECT
206500131013** TASTI DI FUNZIONE  D1
206600131013PROMSG    *LIBL     KEY0001  01
206700131013PROMSG    *LIBL     KEY0002  02
206800131013PROMSG    *LIBL     KEY0003  03
206900131013PROMSG    *LIBL     KEY0004  04
207000131013PROMSG    *LIBL     KEY0005  05
207100131013PROMSG    *LIBL     KEY0006  06
207200131013PROMSG    *LIBL     KEY0007  07
207300131013PROMSG    *LIBL     KEY0008  08
207400131013PROMSG    *LIBL     KEY0009  09
207500131013PROMSG    *LIBL     KEY0010  10
207600131013PROMSG    *LIBL     KEY0011  11
207700131013PROMSG    *LIBL     KEY0012  12
207800131013PROMSG    *LIBL     KEY0013  13
207900131013PROMSG    *LIBL     KEY0014  14
208000131013PROMSG    *LIBL     KEY0015  15
208100131013PROMSG    *LIBL     KEY0016  16
208200131013PROMSG    *LIBL     KEY0017  17
208300131013PROMSG    *LIBL     KEY0018  18
208400131013PROMSG    *LIBL     KEY0019  19
208500131013PROMSG    *LIBL     KEY0020  20
208600131013PROMSG    *LIBL     KEY0021  21
208700131013PROMSG    *LIBL     KEY0022  22
208800131013PROMSG    *LIBL     KEY0023  23
208900131013PROMSG    *LIBL     KEY0024  24
209000131013**  ABILITAZIONE D1 /D2
2091001310130N  01
2092001310130N  02
2093001310131N  03
2094001310131N  04
2095001310131N  05
2096001310131N  06
2097001310130N  07
2098001310130N  08
2099001310130N  09
2100001310130N  10
2101001310130N  11
2102001310130N  12
2103001310130N  13
2104001310130N  14
2105001310130N  15
2106001310130N  16
2107001310130N  17
2108001310131N  18
2109001310130N  19
2110001310130N  20
2111001310130N  21
2112001310130N  22
2113001310130N  23   Impostare '1' se si usano le opzioni
2114001310131N  24   Lasciare sempre impostata a '0' il valore della riga 24
211500131013** TASTI DI FUNZIONE  D3
211600131013PROMSG    *LIBL     KEY0001  01
211700131013PROMSG    *LIBL     KEY0002  02
211800131013PROMSG    *LIBL     KEY0003  03
211900131013PROMSG    *LIBL     KEY0004  04
212000131013PROMSG    *LIBL     KEY0005  05
212100131013PROMSG    *LIBL     KEY0006  06
212200131013PROMSG    *LIBL     KEY0007  07
212300131013PROMSG    *LIBL     KEY0008  08
212400131013PROMSG    *LIBL     KEY0009  09
212500131013PROMSG    *LIBL     KEY0010  10
212600131013PROMSG    *LIBL     KEY0011  11
212700131013PROMSG    *LIBL     KEY0012  12
212800131013PROMSG    *LIBL     KEY0013  13
212900131013PROMSG    *LIBL     KEY0014  14
213000131013PROMSG    *LIBL     KEY0015  15
213100131013PROMSG    *LIBL     KEY0016  16
213200131013PROMSG    *LIBL     KEY0017  17
213300131013PROMSG    *LIBL     KEY0018  18
213400131013PROMSG    *LIBL     KEY0019  19
213500131013PROMSG    *LIBL     KEY0020  20
213600131013PROMSG    *LIBL     KEY0021  21
213700131013PROMSG    *LIBL     KEY0022  22
213800131013PROMSG    *LIBL     KEY0023  23
213900131013PROMSG    *LIBL     KEY0024  24
214000131013**  ABILITAZIONE D3
2141001310130N  01
2142001310130N  02
2143001310131N  03
2144001310131N  04
2145001310130N  05
2146001310130N  06
2147001310130N  07
2148001310130N  08
2149001310130N  09
2150001310130N  10
2151001310130N  11
2152001310131N  12
2153001310130N  13
2154001310130N  14
2155001310130N  15
2156001310130N  16
2157001310130N  17
2158001310130N  18
2159001310130N  19
2160001310130N  20
2161001310130N  21
2162001310130N  22
2163001310130N  23   Impostare '1' se si usano le opzioni
2164001310131N  24   Lasciare sempre impostata a '0' il valore della riga 24
