000100000000     H*------------------------------------------------------------------------*
000200170629     H* IMPORTAZIONE   GEORDB DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800170629     FTISID0D   CF   E             WORKSTN INFDS(DEVDS1)
000900170629     F                                     SFILE(SID0DMS:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* MESSAGGI
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D*-------------------
001800000000     D* DESCRIZIONI TASTI FUNZIONALI
001900000000     D*-------------------
002000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002300000000     D*-------------------
002400000000     D FUV             S             19    DIM(24)
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* RIDEFINIZIONE SCHIERA -FUD-
003000000000     D*-------------------
003100000000     D FUDDS           DS
003200000000     D*                -DESCRIZIONE PARZIALE
003300000000     D  FUDDE1                 1      1
003400000000     D*                -FUNZIONE
003500000000     D  FUDFUN                 2      3
003600000000     D*                -DESCRIZIONE PARZIALE
003700000000     D  FUDDE2                 4     19
003800000000     D*                -FILLER
003900000000     D  FUDFI0                20     34
004000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004100000000     D  FUDATT                35     35
004200000000     D*                -FILLER
004300000000     D  FUDFI1                36     80
004400000000     D*                -DESCRIZIONE COMPLETA
004500000000     D  FUDDES                 1     19
004600000000     D*                -RIDEFINIZIONE RIGA
004700000000     D  FUDRIG                 1     80
004800000000     D*-------------------
004900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005000000000     D*-------------------
005100000000     D DSMSG         E DS
005200000000     D*-------------------
005300020919     D* DS REPERIMENTO DATI UTENTE
005400020919     D*------------
005500020919     D TIBS34DS      E DS                                                       *Profili utente
005600020919     D DDATIUTE      E DS                                                       *Dati utente
005700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005800000000     D*-------------------
005900000000     D* DS PER GESTIONE TASTI FUNZIONALI
006000000000     D*-------------------
006100000000     D DSKEY         E DS
006200000000     D DEVDS1          DS
006300000000     D  KEY                  369    369
006400000000     D*-------------------
006500000000     D* ARCHITETTURA
006600000000     D*-------------------
006700000000     D KPJBA         E DS
006800060426     D*-------------------
006900060426     D* VARIABILI DI WRK
007000060426     D*-------------------
007100170629     D DPCVE10F      e ds
007200170629     D MAX_VERSION     s                   LIKE(CVEVER) INZ
007300000000     C*------------------------------------------------------------------------*
007400000000     C* MAIN LINE
007500000000     C*------------------------------------------------------------------------*
007600000000     C*
007700000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
007800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
007900000000     C*
008000000000     C* CICLO FINO A CHE:
008100000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008200000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008300000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
008400000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
008500000000     C     F12L00        OREQ      '1'
008600000000     C     FLFINE        OREQ      '1'
008700060428     C*
008800010418     C* PROME CONSIDERAZIONI
008900010418     C                   EXSR      EXEINZ
009000000000     C*
009100000000     C* EMETTE FORMATO VIDEO
009200000000     C                   EXSR      BUTFMR
009300000000     C*
009400000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
009500000000     C                   EXSR      FUNCON
009600000000     C*
009700060428     C                   ENDDO                                                  <-- 2 --
009800000000     C*
009900060428     C                   ENDIF                                                  <-- 1 --
010000000000     C*
010100000000     C                   SETON                                        LR
010200010418     C*------------------------------------------------------------------------*
010300010418     C* PRIME CONSIDERAZIONI
010400010418     C*------------------------------------------------------------------------*
010500010418     C     EXEINZ        BEGSR
010600170629     C*
010700170629     C* Reperisco la versione più alta corrente
010800170629     C*
010900170629     C/EXEC SQL
011000170629     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
011100170629     C/END-EXEC
011200170629     C*
011300170629     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
011400170629     C                   EVAL      VIDVER = MAX_VERSION + 1
011500010418     C*
011600010418     C                   ENDSR
011700000000     C*------------------------------------------------------------------------*
011800000000     C* EMISSIONE FORMATO A VIDEO
011900000000     C*------------------------------------------------------------------------*
012000000000     C     BUTFMR        BEGSR
012100000000     C*
012200000000     C* WRITE RECORD TESTATA
012300170629     C                   WRITE     SID0D01
012400000000     C*
012500000000     C* VISUALIZZAZIONE MESSAGGIO
012600000000     C                   EXSR      SNDMSG
012700000000     C*
012800000000     C* WRITE RECORD FUNZIONI
012900170629     C                   WRITE     SID0D02
013000991019     C*
013100991019     C* EXFMT RECORD DETTAGLIO
013200170629     C                   EXFMT     SID0D03
013300991019     C*
013400000000     C* AZZERO FLAG DI ERRORE LIVELLO
013500000000     C                   MOVEL     '0'           ERRL00
013600000000     C*
013700000000     C                   ENDSR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* SNDMSG - MANDA MESSAGGIO
014000000000     C*------------------------------------------------------------------------*
014100000000     C     SNDMSG        BEGSR
014200000000     C*
014300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014400000000     C                   MOVEL     '1'           *IN40
014500170629     C                   WRITE     SID0DMC
014600000000     C                   MOVEL     '0'           *IN40
014700000000     C*
014800000000     C* VISUALIZZAZIONE MESSAGGIO
014900000000     C                   Z-ADD     0             MSGULT            4 0
015000000000     C                   CALL      'XMSG'        XMSGPA
015100000000     C     DSMSER        IFEQ      0
015200000000     C                   MOVEL     DSMSKE        MSGKEY
015300000000     C                   END
015400000000     C                   Z-ADD     1             MSGULT
015500170629     C                   WRITE     SID0DMS                                      SFL MESS
015600170629     C                   WRITE     SID0DMC                                      CONTR MESS
015700000000     C*
015800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015900000000     C                   MOVEL     *BLANKS       DSMSMS
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016400000000     C*------------------------------------------------------------------------*
016500000000     C     ENTGES        BEGSR
016600000000     C*
016700000000     C* CONTROLLA I CAMPI IMMESSI
016800000000     C                   EXSR      DECODI
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017300000000     C*------------------------------------------------------------------------*
017400000000     C     F03GES        BEGSR
017500000000     C*
017600000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017700000000     C                   MOVEL     '1'           F03L00
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018200000000     C*------------------------------------------------------------------------*
018300991020     C     F06GES        BEGSR
018400991105     C*
018500991105     C                   EXSR      DECODI
018600060427     C*
018700991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
018800170629     C     ERRL00        IFEQ      '0'
018900170629     C*
019000170629     C* CHIAMO IL DRIVER DI IMPORTAZIONE DATI CAPPARIO DPD 2017
019100170629     C                   CALL      'TISID0R1'
019200170629     C*
019300170629     C* AL TERMINE ESCO
019400170629     C                   MOVEL     '1'           F12L00
019500991105     C                   ENDIF
019600000000     C*
019700000000     C                   ENDSR
019800000000     C*------------------------------------------------------------------------*
019900000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
020000000000     C*------------------------------------------------------------------------*
020100000000     C     F12GES        BEGSR
020200000000     C*
020300000000     C* IMPOSTO FLAG DI LIVELLO
020400991103     C                   MOVEL     '1'           F12L00
020500000000     C*
020600000000     C                   ENDSR
020700000000     C*------------------------------------------------------------------------*
020800991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
020900000000     C*------------------------------------------------------------------------*
021000000000     C     F24GES        BEGSR
021100000000     C*
021200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
021300000000     C                   EXSR      FUNGES
021400000000     C*
021500000000     C                   ENDSR
021600991020     C*------------------------------------------------------------------------*
021700000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
021800000000     C*------------------------------------------------------------------------*
021900000000     C     DECODI        BEGSR
022000060426     C*
022100000104     C*
022200000000     C                   ENDSR
022300170629     C*--------------------------------------------------------------------------------------------*
022400170629     C* REPERISCE I DATI UTENTE
022500170629     C*--------------------------------------------------------------------------------------------*
022600170629     C     REPDATIUTE    BEGSR
022700170629     C*
022800170629     C* INIZIALIZZA VARIABILI DI WRK
022900170629     C                   CLEAR                   TIBS34DS
023000170629     C                   CLEAR                   AZUTEDS
023100170629     C                   CLEAR                   DDATIUTE
023200170629     C*
023300170629     C     *DTAARA       DEFINE    §azute        azuteds
023400170629     C     *DTAARA       DEFINE    §datiute      ddatiute
023500170629     C                   IN(E)     *DTAARA
023600170629if  1C                   IF        %Error
023700170629     c                   EVAL      I34Tla = 'L'
023800170629     C                   CALL      'TIBS34R'
023900170629     C                   PARM                    Tibs34Ds
024000170629     C                   IN        *DTAARA
024100170629e   1C                   ENDIF
024200170629     C*
024300170629     C                   ENDSR
024400000000     C*------------------------------------------------------------------------*
024500980403     C* *INZSR - OPERAZIONI INIZIALI
024600000000     C*------------------------------------------------------------------------*
024700980403     C     *INZSR        BEGSR
024800000000     C*
024900000000     C* RICEVIMENTO PARAMETRI
025000000000     C     *ENTRY        PLIST
025100000000     C                   PARM                    KPJBA
025200000000     C*
025300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
025400000000     C     XMSGPA        PLIST
025500000000     C                   PARM                    DSMSMS
025600000000     C                   PARM                    DSMSPG
025700000000     C                   PARM                    DSMSKE
025800000000     C                   PARM                    DSMSER
025900000000     C*
026000000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
026100000000     C                   CALL      'XKEY'
026200000000     C                   PARM                    DSKEY
026300000000     C*
026400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
026500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
026600000000     C*
026700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
026800000000     C*   -NOME PGM CHIAMANTE
026900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
027000170629     C                   MOVEL     'TISID0R'     DSMSPG
027100170629     C                   MOVEL     'TISID0R'     MSGQUE
027200000000     C                   MOVEL     *BLANKS       MSGKEY
027300000000     C                   MOVEL     *BLANKS       DSMSMS
027400000000     C*
027500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
027600000000     C                   Z-ADD     0             FUNGRU            5 0
027700000000     C*
027800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
027900000000     C                   EXSR      FUNGES
028000000000     C*
028100000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
028200000000     C                   MOVEL     '0'           F12L00            1
028300000000     C                   MOVEL     '0'           F03L00            1
028400000000     C                   MOVEL     '0'           FINL00            1
028500000000     C                   MOVEL     '0'           ERRL00            1
028600000000     C                   MOVEL     '0'           FLFINE            1
028700170629     C*
028800170629     C* LANCIO ROUTINE PER VERIFICARE SE
028900170629     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
029000170629     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
029100170629     C                   EXSR      REPDATIUTE
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
029600000000     C*------------------------------------------------------------------------*
029700000000     C     FUNGES        BEGSR
029800000000     C*
029900000000     C* PULISCO CAMPI DI LAVORO
030000000000     C                   Z-ADD     0             W                 5 0
030100000000     C*
030200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
030300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
030400000000     C     1             DO        24            I
030500000000     C                   MOVEL     FUD(I)        FUDRIG
030600000000     C*
030700000000     C* PULIZIA FUNZIONI PRECEDENTI
030800000000     C                   MOVEL     *BLANKS       FUV(I)
030900000000     C*
031000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
031100000000     C* (SALTA F24)
031200000000     C     FUDATT        IFEQ      'S'
031300000000     C     FUDFUN        ANDNE     '24'
031400000000     C                   ADD       1             W
031500000000     C                   MOVEL     FUDDES        FUV(W)
031600000000     C                   END
031700000000     C                   END                                                    DO WHILE
031800000000     C*
031900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
032000000000     C* ALTRIMENTI DISATTIVO F24
032100000000     C     1             DO        24            I
032200000000     C                   MOVEL     FUD(I)        FUDRIG
032300000000     C*
032400000000     C     FUDFUN        IFEQ      '24'
032500000000     C     W             IFGT      6
032600000000     C                   MOVEL     'S'           FUDATT
032700000000     C                   ELSE
032800000000     C                   MOVEL     *BLANKS       FUDATT
032900000000     C                   END                                                    W GT 8
033000000000     C                   MOVEL     FUDRIG        FUD(I)
033100000000     C                   END
033200000000     C                   END                                                    DO WHILE
033300000000     C*
033400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
033500000000     C     W             DIV       6             I
033600000000     C                   MVR                     Z
033700000000     C*
033800000000     C     Z             IFNE      0
033900000000     C     I             ADD       1             Z
034000000000     C                   ELSE
034100000000     C                   Z-ADD     I             Z                 5 0
034200000000     C                   END
034300000000     C*
034400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
034500000000     C                   ADD       1             FUNGRU
034600000000     C     FUNGRU        IFGT      Z
034700000000     C     FUNGRU        ORLT      1
034800000000     C                   Z-ADD     1             FUNGRU
034900000000     C                   END
035000000000     C*
035100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
035200000000     C     FUNGRU        IFEQ      1
035300000000     C                   MOVEL     FUV(1)        FUN001
035400000000     C                   MOVEL     FUV(2)        FUN002
035500000000     C                   MOVEL     FUV(3)        FUN003
035600000000     C                   MOVEL     FUV(4)        FUN004
035700000000     C                   MOVEL     FUV(5)        FUN005
035800000000     C                   MOVEL     FUV(6)        FUN006
035900000000     C                   END
036000000000     C*
036100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
036200000000     C     FUNGRU        IFEQ      2
036300000000     C                   MOVEL     FUV(7)        FUN001
036400000000     C                   MOVEL     FUV(8)        FUN002
036500000000     C                   MOVEL     FUV(9)        FUN003
036600000000     C                   MOVEL     FUV(10)       FUN004
036700000000     C                   MOVEL     FUV(11)       FUN005
036800000000     C                   MOVEL     FUV(12)       FUN006
036900000000     C                   END
037000000000     C*
037100000000     C* TERZO GRUPPO DI 6 FUNZIONI
037200000000     C     FUNGRU        IFEQ      3
037300000000     C                   MOVEL     FUV(13)       FUN001
037400000000     C                   MOVEL     FUV(14)       FUN002
037500000000     C                   MOVEL     FUV(15)       FUN003
037600000000     C                   MOVEL     FUV(16)       FUN004
037700000000     C                   MOVEL     FUV(17)       FUN005
037800000000     C                   MOVEL     FUV(18)       FUN006
037900000000     C                   END
038000000000     C*
038100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
038200000000     C     FUNGRU        IFEQ      4
038300000000     C                   MOVEL     FUV(19)       FUN001
038400000000     C                   MOVEL     FUV(20)       FUN002
038500000000     C                   MOVEL     FUV(21)       FUN003
038600000000     C                   MOVEL     FUV(22)       FUN004
038700000000     C                   MOVEL     *BLANKS       FUN005
038800000000     C                   MOVEL     *BLANKS       FUN006
038900000000     C                   END
039000000000     C*
039100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
039200000000     C                   MOVEL     *BLANKS       FUN007
039300000000     C                   MOVEL     *BLANKS       FUN008
039400000000     C*
039500000000     C     1             DO        24            I
039600000000     C                   MOVEL     FUD(I)        FUDRIG
039700000000     C     FUDFUN        IFEQ      '24'
039800000000     C     FUDATT        ANDEQ     'S'
039900000000     C                   MOVEL     FUDDES        FUN008
040000000000     C                   END
040100000000     C                   END                                                    DO
040200000000     C                   ENDSR
040300000000     C*------------------------------------------------------------------------*
040400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
040500000000     C*------------------------------------------------------------------------*
040600000000     C     FUNCON        BEGSR
040700000000     C*
040800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
040900000000     C                   EXSR      FUNVAR
041000000000     C*
041100000000     C* PULISCO CAMPI DI LAVORO
041200000000     C                   Z-ADD     0             I                 5 0
041300000000     C                   Z-ADD     0             W                 5 0
041400000000     C*
041500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
041600000000     C     FUNKEY        IFNE      *BLANKS
041700000000     C     1             DO        24            I
041800000000     C                   MOVEL     FUD(I)        FUDRIG
041900000000     C*
042000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
042100000000     C     FUNKEY        IFEQ      FUDFUN
042200000000     C     FUDATT        ANDEQ     'S'
042300000000     C                   Z-ADD     I             W
042400000000     C                   END
042500000000     C                   END                                                    DO WHILE
042600000000     C*
042700000000     C* FUNZIONE NON ATTIVA (ERRORE)
042800000000     C     W             IFEQ      0
042900000000     C*
043000000000     C* EMISSIONE MESSAGGIO DI ERRORE
043100000000     C                   MOVEL     MSG(1)        DSMSMS
043200000000     C*
043300000000     C                   END                                                    W=0
043400000000     C                   END                                                    FUNKEY NE *BLANKS
043500000000     C*
043600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
043700000000     C     W             IFNE      0
043800000000     C     FUNKEY        OREQ      *BLANKS
043900000000     C*
044000000000     C     FUNKEY        IFEQ      *BLANKS
044100000000     C                   EXSR      ENTGES
044200000000     C                   END
044300000000     C     FUNKEY        IFEQ      '01'
044400000000     C                   END
044500000000     C     FUNKEY        IFEQ      '03'
044600000000     C                   EXSR      F03GES
044700000000     C                   END
044800000000     C     FUNKEY        IFEQ      '06'
044900000000     C                   EXSR      F06GES
045000000000     C                   END
045100000000     C     FUNKEY        IFEQ      '07'
045200000000     C                   END
045300000000     C     FUNKEY        IFEQ      '08'
045400000000     C                   END
045500060608     C     FUNKEY        IFEQ      '10'
045600060608     C                   END
045700000000     C     FUNKEY        IFEQ      '12'
045800000000     C                   EXSR      F12GES
045900000000     C                   END
046000000000     C     FUNKEY        IFEQ      '21'
046100000000     C                   END
046200000000     C     FUNKEY        IFEQ      '22'
046300000000     C                   END
046400000000     C     FUNKEY        IFEQ      '24'
046500000000     C                   EXSR      F24GES
046600000000     C                   END
046700000000     C     FUNKEY        IFEQ      'HP'
046800000000     C                   END
046900000000     C*
047000000000     C                   END
047100000000     C*
047200000000     C                   ENDSR
047300000000     C*------------------------------------------------------------------------*
047400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
047500000000     C*------------------------------------------------------------------------*
047600000000     C     FUNVAR        BEGSR
047700000000     C*
047800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
047900000000     C*
048000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
048100000000     C     KEY           IFEQ      ENTER
048200000000     C                   MOVEL     *BLANKS       FUNKEY
048300000000     C                   END
048400000000     C     KEY           IFEQ      F1
048500000000     C                   MOVEL     '01'          FUNKEY
048600000000     C                   END
048700000000     C     KEY           IFEQ      F2
048800000000     C                   MOVEL     '02'          FUNKEY
048900000000     C                   END
049000000000     C     KEY           IFEQ      F3
049100000000     C                   MOVEL     '03'          FUNKEY
049200000000     C                   END
049300000000     C     KEY           IFEQ      F4
049400000000     C                   MOVEL     '04'          FUNKEY
049500000000     C                   END
049600000000     C     KEY           IFEQ      F5
049700000000     C                   MOVEL     '05'          FUNKEY
049800000000     C                   END
049900991103     C     KEY           IFEQ      F6
050000000000     C                   MOVEL     '06'          FUNKEY
050100000000     C                   END
050200000000     C     KEY           IFEQ      F7
050300000000     C                   MOVEL     '07'          FUNKEY
050400000000     C                   END
050500000000     C     KEY           IFEQ      F8
050600000000     C                   MOVEL     '08'          FUNKEY
050700000000     C                   END
050800000000     C     KEY           IFEQ      F9
050900000000     C                   MOVEL     '09'          FUNKEY
051000000000     C                   END
051100000000     C     KEY           IFEQ      F10
051200000000     C                   MOVEL     '10'          FUNKEY
051300000000     C                   END
051400000000     C     KEY           IFEQ      F11
051500000000     C                   MOVEL     '11'          FUNKEY
051600000000     C                   END
051700000000     C     KEY           IFEQ      F12
051800000000     C                   MOVEL     '12'          FUNKEY
051900000000     C                   END
052000000000     C     KEY           IFEQ      F13
052100000000     C                   MOVEL     '13'          FUNKEY
052200000000     C                   END
052300000000     C     KEY           IFEQ      F14
052400000000     C                   MOVEL     '14'          FUNKEY
052500000000     C                   END
052600000000     C     KEY           IFEQ      F15
052700000000     C                   MOVEL     '15'          FUNKEY
052800000000     C                   END
052900000000     C     KEY           IFEQ      F16
053000000000     C                   MOVEL     '16'          FUNKEY
053100000000     C                   END
053200000000     C     KEY           IFEQ      F17
053300000000     C                   MOVEL     '17'          FUNKEY
053400000000     C                   END
053500000000     C     KEY           IFEQ      F18
053600000000     C                   MOVEL     '18'          FUNKEY
053700000000     C                   END
053800000000     C     KEY           IFEQ      F19
053900000000     C                   MOVEL     '19'          FUNKEY
054000000000     C                   END
054100000000     C     KEY           IFEQ      F20
054200000000     C                   MOVEL     '20'          FUNKEY
054300000000     C                   END
054400000000     C     KEY           IFEQ      F21
054500000000     C                   MOVEL     '21'          FUNKEY
054600000000     C                   END
054700000000     C     KEY           IFEQ      F22
054800000000     C                   MOVEL     '22'          FUNKEY
054900000000     C                   END
055000000000     C     KEY           IFEQ      F23
055100000000     C                   MOVEL     '23'          FUNKEY
055200000000     C                   END
055300000000     C     KEY           IFEQ      F24
055400000000     C                   MOVEL     '24'          FUNKEY
055500000000     C                   END
055600000000     C     KEY           IFEQ      ROLLUP
055700000000     C                   MOVEL     'RU'          FUNKEY
055800000000     C                   END
055900000000     C     KEY           IFEQ      HELP
056000000000     C                   MOVEL     'HP'          FUNKEY
056100000000     C                   END
056200000000     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400000000     C* NOTE PER COMPILAZIONE SCHIERE
056500000000     C*------------------------------------------------------------------------*
056600000000     C* -MSG-
056700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
056800000000     C* (NON TOCCARE MSG,1)
056900000000     C*
057000000000     C* -CMD-
057100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
057200000000     C*
057300000000     C* -FUD-
057400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
057500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
057600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
057700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
057800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
057900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
058000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
058100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
058200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
058300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
058400000000     C*
058500000000     C*------------------------------------------------------------------------*
058600991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
058700170629TISID0R- Tasto funzionale non gestito.                                         1
058800170629TISID0R-                                                                       2
058900170629TISID0R-                                                                       3
059000170629TISID0R-                                                                       4
059100170629TISID0R-                                                                       5
059200170629TISID0R-                                                                       6
059300170629TISID0R-                                                                       7
059400170629TISID0R-                                                                       8
059500170629TISID0R-                                                                       9
059600170629TISID0R-                                                                      10
059700170629TISID0R-                                                                      11
059800170629TISID0R-                                                                      12
059900170629TISID0R-                                                                      13
060000170629TISID0R-                                                                      14
060100170629TISID0R-                                                                      15
060200170629TISID0R-                                                                      16
060300170629TISID0R-                                                                      17
060400170629TISID0R-                                                                      18
060500170629TISID0R-                                                                      19
060600170629TISID0R-                                                                      20
060700000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
060800000000                   -        (S/N)=N                        funz.01¬
060900000000                   -        (S/N)=N                        funz.02¬
061000000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
061100000000                   -        (S/N)=N                        funz.04¬
061200000000                   -        (S/N)=N                        funz.05¬
061300170629F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
061400060608                   -        (S/N)=N                        funz.07¬
061500060608                   -        (S/N)=N                        funz.08¬
061600000000                   -        (S/N)=N                        funz.09¬
061700170629                   -        (S/N)=N                        funz.10¬
061800000000                   -        (S/N)=N                        funz.11¬
061900991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
062000000000                   -        (S/N)=N                        funz.13¬
062100000000                   -        (S/N)=N                        funz.14¬
062200000000                   -        (S/N)=N                        funz.15¬
062300000000                   -        (S/N)=N                        funz.16¬
062400000000                   -        (S/N)=N                        funz.17¬
062500000000                   -        (S/N)=N                        funz.18¬
062600000000                   -        (S/N)=N                        funz.19¬
062700000000                   -        (S/N)=N                        funz.20¬
062800000000                   -        (S/N)=N                        funz.21¬
062900000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
063000000000                   -        (S/N)=N                        funz.23¬
063100170629F24=Altri tasti    -        (S/N)=N                        funz.24¬
