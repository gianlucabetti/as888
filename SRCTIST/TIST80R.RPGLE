000100000000     H*------------------------------------------------------------------------*
000200040308     H* STATISTICA stagionalità PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800040308     FTIST80D   CF   E             WORKSTN INFDS(DEVDS1)
000900040308     F                                     SFILE(ST80DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* COMANDI
001900000000     D*-------------------
002000000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* DESCRIZIONI TASTI FUNZIONALI
002300000000     D*-------------------
002400000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002500000000     D*-------------------
002600000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002700000000     D*-------------------
002800000000     D FUV             S             19    DIM(24)
002900000000     D*------------------------------------------------------------------------*
003000000000     D* INPUT
003100000000     D*------------------------------------------------------------------------*
003200000000     D*-------------------
003300000000     D* RIDEFINIZIONE SCHIERA -FUD-
003400000000     D*-------------------
003500000000     D FUDDS           DS
003600000000     D*                -DESCRIZIONE PARZIALE
003700000000     D  FUDDE1                 1      1
003800000000     D*                -FUNZIONE
003900000000     D  FUDFUN                 2      3
004000000000     D*                -DESCRIZIONE PARZIALE
004100000000     D  FUDDE2                 4     19
004200000000     D*                -FILLER
004300000000     D  FUDFI0                20     34
004400000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004500000000     D  FUDATT                35     35
004600000000     D*                -FILLER
004700000000     D  FUDFI1                36     80
004800000000     D*                -DESCRIZIONE COMPLETA
004900000000     D  FUDDES                 1     19
005000000000     D*                -RIDEFINIZIONE RIGA
005100000000     D  FUDRIG                 1     80
005200000000     D*-------------------
005300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005400000000     D*-------------------
005500000000     D DSMSG         E DS
005600000000     D*-------------------
005700020917     D* REPERIMENTO DATI UTENTE
005800000000     D*-------------------
005900020917     D TIBS34DS      E DS                                                       *Profili utente
006000020917     D DDATIUTE      E DS                                                       *Dati utente
006100020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006200970401     D*----------------
006300000000     D* DS PARAMETRI DI LANCIO PGM
006400000000     D*-------------------
006500950703     D PARAM           DS
006600000000     D  PARTLA                 1      1
006700000000     D  PARANN                 2      5  0
006800000000     D  PARMMI                 6      7  0
006900000000     D  PARMMF                 8      9  0
007000040416     D  PARCLI                10     10
007100000000     D*-------------------
007200000000     D* DS PER GESTIONE TASTI FUNZIONALI
007300000000     D*-------------------
007400000000     D DSKEY         E DS
007500000000     D DEVDS1          DS
007600000000     D  KEY                  369    369
007700000000     D*-------------------
007800000000     D* ARCHITETTURA
007900000000     D*-------------------
008000000000     D KPJBA         E DS
008100000000     C*------------------------------------------------------------------------*
008200000000     C* MAIN LINE
008300000000     C*------------------------------------------------------------------------*
008400000000     C*
008500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
008700000000     C*
008800000000     C* CICLO FINO A CHE:
008900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009100000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009300000000     C     F12L00        OREQ      '1'
009400000000     C     FLFINE        OREQ      '1'
009500000000     C*
009600000000     C* EMETTE FORMATO VIDEO
009700000000     C                   EXSR      BUTFMR
009800000000     C*
009900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010000000000     C                   EXSR      FUNCON
010100000000     C*
010200000000     C                   END                                                    <-- 2 --
010300000000     C*
010400000000     C                   END                                                    <-- 1 --
010500000000     C*
010600000000     C                   SETON                                        LR
010700000000     C*------------------------------------------------------------------------*
010800000000     C* EMISSIONE FORMATO A VIDEO
010900000000     C*------------------------------------------------------------------------*
011000000000     C     BUTFMR        BEGSR
011100000000     C*
011200000000     C* WRITE RECORD TESTATA
011300040309     C                   WRITE     ST80D01
011400000000     C*
011500000000     C* VISUALIZZAZIONE MESSAGGIO
011600000000     C                   EXSR      SNDMSG
011700000000     C*
011800000000     C* WRITE RECORD FUNZIONI
011900040309     C                   WRITE     ST80D02
012000991019     C*
012100991019     C* EXFMT RECORD DETTAGLIO
012200040309     C                   EXFMT     ST80D03
012300991019     C*
012400000000     C* AZZERO FLAG DI ERRORE LIVELLO
012500000000     C                   MOVEL     '0'           ERRL00
012600000000     C*
012700000000     C* AZZERO FLAG DI ERRORE
012800991025     C                   SETOFF                                         7071
012900000000     C                   SETOFF                                       727377
013000000000     C                   SETOFF                                       787983
013100000000     C                   SETOFF                                       848586
013200950703     C                   SETOFF                                       818082
013300000000     C*
013400000000     C                   ENDSR
013500000000     C*------------------------------------------------------------------------*
013600000000     C* SNDMSG - MANDA MESSAGGIO
013700000000     C*------------------------------------------------------------------------*
013800000000     C     SNDMSG        BEGSR
013900000000     C*
014000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014100000000     C                   MOVEL     '1'           *IN40
014200040309     C                   WRITE     ST80DMC
014300000000     C                   MOVEL     '0'           *IN40
014400000000     C*
014500000000     C* VISUALIZZAZIONE MESSAGGIO
014600000000     C                   Z-ADD     0             MSGULT            4 0
014700000000     C                   CALL      'XMSG'        XMSGPA
014800000000     C     DSMSER        IFEQ      0
014900000000     C                   MOVEL     DSMSKE        MSGKEY
015000000000     C                   END
015100000000     C                   Z-ADD     1             MSGULT
015200040309     C                   WRITE     ST80DMS                                      SFL MESS
015300040309     C                   WRITE     ST80DMC                                      CONTR MESS
015400000000     C*
015500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015600000000     C                   MOVEL     *BLANKS       DSMSMS
015700000000     C*
015800000000     C                   ENDSR
015900000000     C*------------------------------------------------------------------------*
016000000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016100000000     C*------------------------------------------------------------------------*
016200000000     C     ENTGES        BEGSR
016300000000     C*
016400000000     C* CONTROLLA I CAMPI IMMESSI
016500000000     C                   EXSR      DECODI
016600000000     C*
016700000000     C                   ENDSR
016800000000     C*------------------------------------------------------------------------*
016900000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
017000000000     C*------------------------------------------------------------------------*
017100000000     C     F01GES        BEGSR
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017500000000     C*------------------------------------------------------------------------*
017600000000     C     F03GES        BEGSR
017700000000     C*
017800000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017900000000     C                   MOVEL     '1'           F03L00
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018400000000     C*------------------------------------------------------------------------*
018500991020     C     F06GES        BEGSR
018600000000     C*
018700991105     C                   EXSR      DECODI
018800991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
018900991105     C     ERRL00        IFEQ      '0'
019000991105     C*
019100000000     C* LANCIO IL PROGRAMMA DI STAMPA
019200040308     C                   MOVEL     'ST81'        KCOAZ
019300000000     C                   EXSR      VIDPAR
019400950703     C                   MOVEL     PARAM         KPJBU
019500040312     C                   CALL      'BCH10'
019600040312 MOM c*********          CALL      'TIST81C'
019700991021     C                   PARM                    KPJBA
019800040312     C*
019900040312     C*  esce dopo aver sottomesso l'elaborazione
020000040312     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020100040312     C                   MOVEL     '1'           F03L00
020200991123     C*
020300991105     C                   ENDIF
020400991105     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
020800000000     C*------------------------------------------------------------------------*
020900000000     C     F07GES        BEGSR
021000000000     C                   ENDSR
021100000000     C*------------------------------------------------------------------------*
021200000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
021300000000     C*------------------------------------------------------------------------*
021400000000     C     F08GES        BEGSR
021500000000     C                   ENDSR
021600000000     C*------------------------------------------------------------------------*
021700000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
021800000000     C*------------------------------------------------------------------------*
021900000000     C     F12GES        BEGSR
022000000000     C*
022100000000     C* IMPOSTO FLAG DI LIVELLO
022200000000     C                   MOVEL     '1'           F12L00
022300000000     C*
022400000000     C                   ENDSR
022500000000     C*------------------------------------------------------------------------*
022600000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
022700000000     C*------------------------------------------------------------------------*
022800000000     C     F21GES        BEGSR
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
023200000000     C*------------------------------------------------------------------------*
023300000000     C     F22GES        BEGSR
023400000000     C*
023500991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
023600000000     C                   Z-ADD     80            LENGH            15 5
023700000000     C                   CALL      'QCAEXEC'
023800000000     C                   PARM                    CMD(1)
023900000000     C                   PARM                    LENGH
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
024400000000     C*------------------------------------------------------------------------*
024500000000     C     F24GES        BEGSR
024600000000     C*
024700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
024800000000     C                   EXSR      FUNGES
024900000000     C*
025000000000     C                   ENDSR
025100000000     C*------------------------------------------------------------------------*
025200000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
025300000000     C*------------------------------------------------------------------------*
025400000000     C     HELGES        BEGSR
025500000000     C                   ENDSR
025600991020     C*------------------------------------------------------------------------*
025700991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
025800000000     C*------------------------------------------------------------------------*
025900000000     C     DECODI        BEGSR
026000000000     C*
026100000000     C* CONTROLLO ANNO
026200991020     C     ERRL00        IFEQ      '0'
026300991020     C     VIDANN        IFEQ      *ZEROS
026400000000     C                   MOVEL     '1'           ERRL00
026500000000     C                   MOVEL     MSG(3)        DSMSMS
026600000000     C                   SETON                                        70
026700991020     C                   ELSE
026800991020     C     VIDANN        IFLT      1900
026900000000     C                   MOVEL     '1'           ERRL00
027000000000     C                   MOVEL     MSG(3)        DSMSMS
027100000000     C                   SETON                                        70
027200991020     C                   END
027300991020     C                   END
027400991020     C                   END
027500000000     C*
027600000000     C* CONTROLLO MESE INIZIO / FINE
027700000000     C     ERRL00        IFEQ      '0'
027800000000     C     VIDMMI        IFEQ      *ZEROS                                       *INIZIO
027900000000     C     VIDMMI        ORGT      12
028000000000     C                   MOVEL     '1'           ERRL00
028100000000     C                   MOVEL     MSG(4)        DSMSMS
028200000000     C                   SETON                                        71
028300000000     C                   END
028400000000     C                   END
028500000000     C     ERRL00        IFEQ      '0'
028600000000     C     VIDMMF        IFEQ      *ZEROS                                       *FINE
028700000000     C                   Z-ADD     VIDMMI        VIDMMF                         *LO IMPOSTO COME INI
028800000000     C                   ELSE
028900000000     C     VIDMMF        IFGT      12
029000000000     C                   MOVEL     '1'           ERRL00
029100000000     C                   MOVEL     MSG(5)        DSMSMS
029200000000     C                   SETON                                        72
029300000000     C                   END
029400000000     C                   END
029500000000     C                   END
029600000000     C     ERRL00        IFEQ      '0'                                          *INIZIO MAGGIORE FIN
029700000000     C     VIDMMF        IFNE      *ZEROS
029800000000     C     VIDMMI        IFGT      VIDMMF
029900000000     C                   MOVEL     '1'           ERRL00
030000000000     C                   MOVEL     MSG(6)        DSMSMS
030100000000     C                   SETON                                        7172
030200000000     C                   END
030300000000     C                   END
030400000000     C                   END
030500000000     C*
030600000000     C                   ENDSR
030700000000     C*------------------------------------------------------------------------*
030800000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
030900000000     C*------------------------------------------------------------------------*
031000000000     C     AZZVID        BEGSR
031100000000     C*
031200000000     C                   Z-ADD     *ZEROS        VIDANN
031300000000     C                   Z-ADD     *ZEROS        VIDMMI
031400000000     C                   Z-ADD     *ZEROS        VIDMMF
031500000000     C*
031600040312     C                   move      *year         VIDANN
031700040312     C                   move      *month        VIDMMI
031800040312     C                   move      *month        VIDMMF
031900040312     C*
032000000000     C                   ENDSR
032100000000     C*------------------------------------------------------------------------*
032200991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
032300000000     C*------------------------------------------------------------------------*
032400000000     C     VIDPAR        BEGSR
032500000000     C*
032600000000     C                   Z-ADD     VIDANN        PARANN
032700000000     C                   Z-ADD     VIDMMI        PARMMI
032800000000     C                   Z-ADD     VIDMMF        PARMMF
032900040416     C                   movel     VIDCLI        PARCLI
033000991214     C*
033100000000     C                   ENDSR
033200020917     C*--------------------------------------------------------------------------------------------*
033300020917     C* REPERISCE I DATI UTENTE
033400020917     C*--------------------------------------------------------------------------------------------*
033500020917     C     REPDATIUTE    BEGSR
033600020917     C*
033700020917     C* INIZIALIZZA VARIABILI DI WRK
033800020917     C                   CLEAR                   TIBS34DS
033900020917     C                   CLEAR                   AZUTEDS
034000020917     C                   CLEAR                   DDATIUTE
034100020917     C*
034200020917     C     *DTAARA       DEFINE    §azute        azuteds
034300020917     C     *DTAARA       DEFINE    §datiute      ddatiute
034400020917     C                   IN(E)     *DTAARA
034500020917if  1C                   IF        %Error
034600020917     c                   EVAL      I34Tla = 'L'
034700020917     C                   CALL      'TIBS34R'
034800020917     C                   PARM                    Tibs34Ds
034900020917     C                   IN        *DTAARA
035000020917e   1C                   ENDIF
035100020917     C*
035200020917     C                   ENDSR
035300000000     C*------------------------------------------------------------------------*
035400980403     C* *INZSR - OPERAZIONI INIZIALI
035500000000     C*------------------------------------------------------------------------*
035600980403     C     *INZSR        BEGSR
035700000000     C*
035800000000     C* RICEVIMENTO PARAMETRI
035900000000     C     *ENTRY        PLIST
036000000000     C                   PARM                    KPJBA
036100980403     C*
036200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
036300000000     C     XMSGPA        PLIST
036400000000     C                   PARM                    DSMSMS
036500000000     C                   PARM                    DSMSPG
036600000000     C                   PARM                    DSMSKE
036700000000     C                   PARM                    DSMSER
036800000000     C*
036900000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
037000000000     C                   CALL      'XKEY'
037100000000     C                   PARM                    DSKEY
037200000000     C*
037300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
037400000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
037500000000     C*
037600000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
037700000000     C*   -NOME PGM CHIAMANTE
037800000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
037900040308     C                   MOVEL     'TIST80R'     DSMSPG
038000040308     C                   MOVEL     'TIST80R'     MSGQUE
038100000000     C                   MOVEL     *BLANKS       MSGKEY
038200000000     C                   MOVEL     *BLANKS       DSMSMS
038300000000     C*
038400000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
038500000000     C                   Z-ADD     0             FUNGRU            5 0
038600000000     C*
038700000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
038800000000     C                   EXSR      FUNGES
038900000000     C*
039000000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
039100000000     C                   MOVEL     '0'           F12L00            1
039200000000     C                   MOVEL     '0'           F03L00            1
039300000000     C                   MOVEL     '0'           FINL00            1
039400000000     C                   MOVEL     '0'           ERRL00            1
039500000000     C                   MOVEL     '0'           FLFINE            1
039600000000     C*
039700000000     C* LANCIO ROUTINE PER VERIFICARE SE
039800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
039900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
040000020917     C                   EXSR      REPDATIUTE
040100020917     C                   Z-ADD     1             CODUT             1 0
040200000000     C*
040300000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
040400000000     C                   EXSR      AZZVID
040500000000     C*
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
040900000000     C*------------------------------------------------------------------------*
041000000000     C     FUNGES        BEGSR
041100000000     C*
041200000000     C* PULISCO CAMPI DI LAVORO
041300000000     C                   Z-ADD     0             W                 5 0
041400000000     C*
041500000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
041600000000     C* E CARICO TABELLA FUNZIONI ATTIVE
041700000000     C     1             DO        24            I
041800000000     C                   MOVEL     FUD(I)        FUDRIG
041900000000     C*
042000000000     C* PULIZIA FUNZIONI PRECEDENTI
042100000000     C                   MOVEL     *BLANKS       FUV(I)
042200000000     C*
042300000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
042400000000     C* (SALTA F24)
042500000000     C     FUDATT        IFEQ      'S'
042600000000     C     FUDFUN        ANDNE     '24'
042700000000     C                   ADD       1             W
042800000000     C                   MOVEL     FUDDES        FUV(W)
042900000000     C                   END
043000000000     C                   END                                                    DO WHILE
043100000000     C*
043200000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
043300000000     C* ALTRIMENTI DISATTIVO F24
043400000000     C     1             DO        24            I
043500000000     C                   MOVEL     FUD(I)        FUDRIG
043600000000     C*
043700000000     C     FUDFUN        IFEQ      '24'
043800000000     C     W             IFGT      6
043900000000     C                   MOVEL     'S'           FUDATT
044000000000     C                   ELSE
044100000000     C                   MOVEL     *BLANKS       FUDATT
044200000000     C                   END                                                    W GT 8
044300000000     C                   MOVEL     FUDRIG        FUD(I)
044400000000     C                   END
044500000000     C                   END                                                    DO WHILE
044600000000     C*
044700000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
044800000000     C     W             DIV       6             I
044900000000     C                   MVR                     Z
045000000000     C*
045100000000     C     Z             IFNE      0
045200000000     C     I             ADD       1             Z
045300000000     C                   ELSE
045400000000     C                   Z-ADD     I             Z                 5 0
045500000000     C                   END
045600000000     C*
045700000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
045800000000     C                   ADD       1             FUNGRU
045900000000     C     FUNGRU        IFGT      Z
046000000000     C     FUNGRU        ORLT      1
046100000000     C                   Z-ADD     1             FUNGRU
046200000000     C                   END
046300000000     C*
046400000000     C* PRIMO GRUPPO DI 6 FUNZIONI
046500000000     C     FUNGRU        IFEQ      1
046600000000     C                   MOVEL     FUV(1)        FUN001
046700000000     C                   MOVEL     FUV(2)        FUN002
046800000000     C                   MOVEL     FUV(3)        FUN003
046900000000     C                   MOVEL     FUV(4)        FUN004
047000000000     C                   MOVEL     FUV(5)        FUN005
047100000000     C                   MOVEL     FUV(6)        FUN006
047200000000     C                   END
047300000000     C*
047400000000     C* SECONDO GRUPPO DI 6 FUNZIONI
047500000000     C     FUNGRU        IFEQ      2
047600000000     C                   MOVEL     FUV(7)        FUN001
047700000000     C                   MOVEL     FUV(8)        FUN002
047800000000     C                   MOVEL     FUV(9)        FUN003
047900000000     C                   MOVEL     FUV(10)       FUN004
048000000000     C                   MOVEL     FUV(11)       FUN005
048100000000     C                   MOVEL     FUV(12)       FUN006
048200000000     C                   END
048300000000     C*
048400000000     C* TERZO GRUPPO DI 6 FUNZIONI
048500000000     C     FUNGRU        IFEQ      3
048600000000     C                   MOVEL     FUV(13)       FUN001
048700000000     C                   MOVEL     FUV(14)       FUN002
048800000000     C                   MOVEL     FUV(15)       FUN003
048900000000     C                   MOVEL     FUV(16)       FUN004
049000000000     C                   MOVEL     FUV(17)       FUN005
049100000000     C                   MOVEL     FUV(18)       FUN006
049200000000     C                   END
049300000000     C*
049400000000     C* QUARTO GRUPPO DI 6 FUNZIONI
049500000000     C     FUNGRU        IFEQ      4
049600000000     C                   MOVEL     FUV(19)       FUN001
049700000000     C                   MOVEL     FUV(20)       FUN002
049800000000     C                   MOVEL     FUV(21)       FUN003
049900000000     C                   MOVEL     FUV(22)       FUN004
050000000000     C                   MOVEL     *BLANKS       FUN005
050100000000     C                   MOVEL     *BLANKS       FUN006
050200000000     C                   END
050300000000     C*
050400000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
050500000000     C                   MOVEL     *BLANKS       FUN007
050600000000     C                   MOVEL     *BLANKS       FUN008
050700000000     C*
050800000000     C     1             DO        24            I
050900000000     C                   MOVEL     FUD(I)        FUDRIG
051000000000     C     FUDFUN        IFEQ      '24'
051100000000     C     FUDATT        ANDEQ     'S'
051200000000     C                   MOVEL     FUDDES        FUN008
051300000000     C                   END
051400000000     C                   END                                                    DO
051500000000     C                   ENDSR
051600000000     C*------------------------------------------------------------------------*
051700000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
051800000000     C*------------------------------------------------------------------------*
051900000000     C     FUNCON        BEGSR
052000000000     C*
052100000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
052200000000     C                   EXSR      FUNVAR
052300000000     C*
052400000000     C* PULISCO CAMPI DI LAVORO
052500000000     C                   Z-ADD     0             I                 5 0
052600000000     C                   Z-ADD     0             W                 5 0
052700000000     C*
052800000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
052900000000     C     FUNKEY        IFNE      *BLANKS
053000000000     C     1             DO        24            I
053100000000     C                   MOVEL     FUD(I)        FUDRIG
053200000000     C*
053300000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
053400000000     C     FUNKEY        IFEQ      FUDFUN
053500000000     C     FUDATT        ANDEQ     'S'
053600000000     C                   Z-ADD     I             W
053700000000     C                   END
053800000000     C                   END                                                    DO WHILE
053900000000     C*
054000000000     C* FUNZIONE NON ATTIVA (ERRORE)
054100000000     C     W             IFEQ      0
054200000000     C*
054300000000     C* EMISSIONE MESSAGGIO DI ERRORE
054400000000     C                   MOVEL     MSG(1)        DSMSMS
054500000000     C*
054600000000     C                   END                                                    W=0
054700000000     C                   END                                                    FUNKEY NE *BLANKS
054800000000     C*
054900000000     C* FUNZIONE ATTIVA (PROCEDI ...)
055000000000     C     W             IFNE      0
055100000000     C     FUNKEY        OREQ      *BLANKS
055200000000     C*
055300000000     C     FUNKEY        IFEQ      *BLANKS
055400000000     C                   EXSR      ENTGES
055500000000     C                   END
055600000000     C     FUNKEY        IFEQ      '01'
055700000000     C                   EXSR      F01GES
055800000000     C                   END
055900000000     C     FUNKEY        IFEQ      '03'
056000000000     C                   EXSR      F03GES
056100000000     C                   END
056200000000     C     FUNKEY        IFEQ      '06'
056300000000     C                   EXSR      F06GES
056400000000     C                   END
056500000000     C     FUNKEY        IFEQ      '07'
056600000000     C                   EXSR      F07GES
056700000000     C                   END
056800000000     C     FUNKEY        IFEQ      '08'
056900000000     C                   EXSR      F08GES
057000000000     C                   END
057100000000     C     FUNKEY        IFEQ      '12'
057200000000     C                   EXSR      F12GES
057300000000     C                   END
057400000000     C     FUNKEY        IFEQ      '21'
057500000000     C                   EXSR      F21GES
057600000000     C                   END
057700000000     C     FUNKEY        IFEQ      '22'
057800000000     C                   EXSR      F22GES
057900000000     C                   END
058000000000     C     FUNKEY        IFEQ      '24'
058100000000     C                   EXSR      F24GES
058200000000     C                   END
058300000000     C     FUNKEY        IFEQ      'HP'
058400000000     C                   EXSR      HELGES
058500000000     C                   END
058600000000     C*
058700000000     C                   END
058800000000     C*
058900000000     C                   ENDSR
059000000000     C*------------------------------------------------------------------------*
059100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
059200000000     C*------------------------------------------------------------------------*
059300000000     C     FUNVAR        BEGSR
059400000000     C*
059500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
059600000000     C*
059700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
059800000000     C     KEY           IFEQ      ENTER
059900000000     C                   MOVEL     *BLANKS       FUNKEY
060000000000     C                   END
060100000000     C     KEY           IFEQ      F1
060200000000     C                   MOVEL     '01'          FUNKEY
060300000000     C                   END
060400000000     C     KEY           IFEQ      F2
060500000000     C                   MOVEL     '02'          FUNKEY
060600000000     C                   END
060700000000     C     KEY           IFEQ      F3
060800000000     C                   MOVEL     '03'          FUNKEY
060900000000     C                   END
061000000000     C     KEY           IFEQ      F4
061100000000     C                   MOVEL     '04'          FUNKEY
061200000000     C                   END
061300000000     C     KEY           IFEQ      F5
061400000000     C                   MOVEL     '05'          FUNKEY
061500000000     C                   END
061600000000     C     KEY           IFEQ      F6
061700000000     C                   MOVEL     '06'          FUNKEY
061800000000     C                   END
061900000000     C     KEY           IFEQ      F7
062000000000     C                   MOVEL     '07'          FUNKEY
062100000000     C                   END
062200000000     C     KEY           IFEQ      F8
062300000000     C                   MOVEL     '08'          FUNKEY
062400000000     C                   END
062500000000     C     KEY           IFEQ      F9
062600000000     C                   MOVEL     '09'          FUNKEY
062700000000     C                   END
062800000000     C     KEY           IFEQ      F10
062900000000     C                   MOVEL     '10'          FUNKEY
063000000000     C                   END
063100000000     C     KEY           IFEQ      F11
063200000000     C                   MOVEL     '11'          FUNKEY
063300000000     C                   END
063400000000     C     KEY           IFEQ      F12
063500000000     C                   MOVEL     '12'          FUNKEY
063600000000     C                   END
063700000000     C     KEY           IFEQ      F13
063800000000     C                   MOVEL     '13'          FUNKEY
063900000000     C                   END
064000000000     C     KEY           IFEQ      F14
064100000000     C                   MOVEL     '14'          FUNKEY
064200000000     C                   END
064300000000     C     KEY           IFEQ      F15
064400000000     C                   MOVEL     '15'          FUNKEY
064500000000     C                   END
064600000000     C     KEY           IFEQ      F16
064700000000     C                   MOVEL     '16'          FUNKEY
064800000000     C                   END
064900000000     C     KEY           IFEQ      F17
065000000000     C                   MOVEL     '17'          FUNKEY
065100000000     C                   END
065200000000     C     KEY           IFEQ      F18
065300000000     C                   MOVEL     '18'          FUNKEY
065400000000     C                   END
065500000000     C     KEY           IFEQ      F19
065600000000     C                   MOVEL     '19'          FUNKEY
065700000000     C                   END
065800000000     C     KEY           IFEQ      F20
065900000000     C                   MOVEL     '20'          FUNKEY
066000000000     C                   END
066100000000     C     KEY           IFEQ      F21
066200000000     C                   MOVEL     '21'          FUNKEY
066300000000     C                   END
066400000000     C     KEY           IFEQ      F22
066500000000     C                   MOVEL     '22'          FUNKEY
066600000000     C                   END
066700000000     C     KEY           IFEQ      F23
066800000000     C                   MOVEL     '23'          FUNKEY
066900000000     C                   END
067000000000     C     KEY           IFEQ      F24
067100000000     C                   MOVEL     '24'          FUNKEY
067200000000     C                   END
067300000000     C     KEY           IFEQ      ROLLUP
067400000000     C                   MOVEL     'RU'          FUNKEY
067500000000     C                   END
067600000000     C     KEY           IFEQ      HELP
067700000000     C                   MOVEL     'HP'          FUNKEY
067800000000     C                   END
067900000000     C                   ENDSR
068000000000     C*------------------------------------------------------------------------*
068100000000     C* NOTE PER COMPILAZIONE SCHIERE
068200000000     C*------------------------------------------------------------------------*
068300000000     C* -MSG-
068400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
068500000000     C* (NON TOCCARE MSG,1)
068600000000     C*
068700000000     C* -CMD-
068800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
068900000000     C*
069000000000     C* -FUD-
069100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
069200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
069500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
069700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
069800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
069900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
070000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
070100000000     C*
070200000000     C*------------------------------------------------------------------------*
070300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
070400040309TIST80R- Tasto funzionale non gestito.                                         1
070500040309TIST80R- Percentuale non accettabile.                                          2
070600040309TIST80R- Anno non accettabile.                                                 3
070700040309TIST80R- Mese inizio non accettabile.                                          4
070800040309TIST80R- Mese fine non accettabile.                                            5
070900040309TIST80R- Mese inizio maggiore mese fine.                                       6
071000040309TIST80R-                                                                       7
071100040309TIST80R-                                                                       8
071200040309TIST80R-                                                                       9
071300040309TIST80R-                                                                      10
071400040309TIST80R-                                                                      11
071500040309TIST80R-                                                                      12
071600040309TIST80R-                                                                      13
071700040309TIST80R-                                                                      14
071800040309TIST80R-                                                                      15
071900040309TIST80R-                                                                      16
072000040309TIST80R-                                                                      17
072100040309TIST80R-                                                                      18
072200040309TIST80R-                                                                      19
072300040309TIST80R-                                                                      20
072400000000** ======== SCHIERA: CMD  (COMANDI)                        ================
072500000000WRKSPLF
072600000000
072700000000
072800000000
072900000000
073000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
073100000000                   -        (S/N)=N                        funz.01¬
073200000000                   -        (S/N)=N                        funz.02¬
073300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
073400000000                   -        (S/N)=N                        funz.04¬
073500000000                   -        (S/N)=N                        funz.05¬
073600000000F06=Lancio         -        (S/N)=S                        funz.06¬
073700000000                   -        (S/N)=N                        funz.07¬
073800000000                   -        (S/N)=N                        funz.08¬
073900000000                   -        (S/N)=N                        funz.09¬
074000000000                   -        (S/N)=N                        funz.10¬
074100000000                   -        (S/N)=N                        funz.11¬
074200040309F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
074300000000                   -        (S/N)=N                        funz.13¬
074400000000                   -        (S/N)=N                        funz.14¬
074500000000                   -        (S/N)=N                        funz.15¬
074600000000                   -        (S/N)=N                        funz.16¬
074700000000                   -        (S/N)=N                        funz.17¬
074800000000                   -        (S/N)=N                        funz.18¬
074900000000                   -        (S/N)=N                        funz.19¬
075000000000                   -        (S/N)=N                        funz.20¬
075100000000                   -        (S/N)=N                        funz.21¬
075200000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
075300000000                   -        (S/N)=N                        funz.23¬
075400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
