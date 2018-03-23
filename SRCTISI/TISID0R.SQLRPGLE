000100000000     H*------------------------------------------------------------------------*
000200170629     H* IMPORTAZIONE   GEORDB DPD
000300000000     H*------------------------------------------------------------------------*
000400171013     H DECEDIT('0,') DATEDIT(*YMD.)
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
007300171013     D*-------------------
007400171013     D DAT_DECOR       s                   LIKE(CVEDDE) INZ
007500171013     D DAT_SCADE       s                   LIKE(CVEDSC) INZ
007600171013     D DATA_OGGI       s              8  0 INZ
007700171016     D*-------------------
007800171016     D NEW_VERSION     s             80    INZ
007900000000     C*------------------------------------------------------------------------*
008000000000     C* MAIN LINE
008100000000     C*------------------------------------------------------------------------*
008200000000     C*
008300000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008400000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
008500000000     C*
008600000000     C* CICLO FINO A CHE:
008700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008900000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009000000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009100000000     C     F12L00        OREQ      '1'
009200000000     C     FLFINE        OREQ      '1'
009300060428     C*
009400010418     C* PROME CONSIDERAZIONI
009500010418     C                   EXSR      EXEINZ
009600000000     C*
009700000000     C* EMETTE FORMATO VIDEO
009800000000     C                   EXSR      BUTFMR
009900000000     C*
010000000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010100000000     C                   EXSR      FUNCON
010200000000     C*
010300060428     C                   ENDDO                                                  <-- 2 --
010400000000     C*
010500060428     C                   ENDIF                                                  <-- 1 --
010600000000     C*
010700000000     C                   SETON                                        LR
010800010418     C*------------------------------------------------------------------------*
010900010418     C* PRIME CONSIDERAZIONI
011000010418     C*------------------------------------------------------------------------*
011100010418     C     EXEINZ        BEGSR
011200170629     C*
011300170629     C* Reperisco la versione più alta corrente
011400170629     C*
011500170629     C/EXEC SQL
011600170629     C+ SET :MAX_VERSION = (SELECT MAX(CVEVER) FROM DPCVE10F)
011700170629     C/END-EXEC
011800170629     C*
011900170629     C* Stacco il nuovo numero versione BRT come incremento della versione più alta presente
012000170629     C                   EVAL      VIDVER = MAX_VERSION + 1
012100171013     C*
012200171013     C*---------------
012300171013     C* controlla la data di Decorrenza
012400171013     C*
012500171013     C/EXEC SQL
012600171013     C+ SET :DAT_DECOR =
012700171013     C+ (SELECT CVEDDE FROM DPCVE10F WHERE CVEVER = :MAX_VERSION)
012800171013     C/END-EXEC
012900171013     C*
013000171013     C*---------------
013100171013     C* importa la stringa con le date di range della Versione
013200171013     C*
013300171013     C/EXEC SQL
013400171013     C+ SET :NEW_VERSION =
013500171013     C+ (SELECT SUBSTR(WDPDRDB, 1, 40)
013600171013     C+  FROM WFDPDRDBF WHERE WDPDRDB like 'VERSION;%')
013700171013     C/END-EXEC
013800171013     C*
013900010418     C                   ENDSR
014000000000     C*------------------------------------------------------------------------*
014100000000     C* EMISSIONE FORMATO A VIDEO
014200000000     C*------------------------------------------------------------------------*
014300000000     C     BUTFMR        BEGSR
014400000000     C*
014500000000     C* WRITE RECORD TESTATA
014600170629     C                   WRITE     SID0D01
014700000000     C*
014800000000     C* VISUALIZZAZIONE MESSAGGIO
014900000000     C                   EXSR      SNDMSG
015000000000     C*
015100000000     C* WRITE RECORD FUNZIONI
015200170629     C                   WRITE     SID0D02
015300991019     C*
015400991019     C* EXFMT RECORD DETTAGLIO
015500170629     C                   EXFMT     SID0D03
015600991019     C*
015700000000     C* AZZERO FLAG DI ERRORE LIVELLO
015800000000     C                   MOVEL     '0'           ERRL00
015900000000     C*
016000000000     C                   ENDSR
016100000000     C*------------------------------------------------------------------------*
016200000000     C* SNDMSG - MANDA MESSAGGIO
016300000000     C*------------------------------------------------------------------------*
016400000000     C     SNDMSG        BEGSR
016500000000     C*
016600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
016700000000     C                   MOVEL     '1'           *IN40
016800170629     C                   WRITE     SID0DMC
016900000000     C                   MOVEL     '0'           *IN40
017000000000     C*
017100000000     C* VISUALIZZAZIONE MESSAGGIO
017200000000     C                   Z-ADD     0             MSGULT            4 0
017300000000     C                   CALL      'XMSG'        XMSGPA
017400000000     C     DSMSER        IFEQ      0
017500000000     C                   MOVEL     DSMSKE        MSGKEY
017600000000     C                   END
017700000000     C                   Z-ADD     1             MSGULT
017800170629     C                   WRITE     SID0DMS                                      SFL MESS
017900170629     C                   WRITE     SID0DMC                                      CONTR MESS
018000000000     C*
018100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
018200000000     C                   MOVEL     *BLANKS       DSMSMS
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
018700000000     C*------------------------------------------------------------------------*
018800000000     C     ENTGES        BEGSR
018900000000     C*
019000000000     C* CONTROLLA I CAMPI IMMESSI
019100000000     C                   EXSR      DECODI
019200000000     C*
019300000000     C                   ENDSR
019400000000     C*------------------------------------------------------------------------*
019500000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
019600000000     C*------------------------------------------------------------------------*
019700000000     C     F03GES        BEGSR
019800000000     C*
019900000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020000000000     C                   MOVEL     '1'           F03L00
020100000000     C*
020200000000     C                   ENDSR
020300000000     C*------------------------------------------------------------------------*
020400000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
020500000000     C*------------------------------------------------------------------------*
020600991020     C     F06GES        BEGSR
020700991105     C*
020800991105     C                   EXSR      DECODI
020900060427     C*
021000991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
021100170629     C     ERRL00        IFEQ      '0'
021200170629     C*
021300170629     C* CHIAMO IL DRIVER DI IMPORTAZIONE DATI CAPPARIO DPD 2017
021400171016     c                   clear                   ErrCall           1
021500171016     c                   clear                   ErrMsg           70
021600171016     C                   CALL      'TISID0C1'
021700171016     c                   parm                    ErrCall
021800171016     c                   parm                    ErrMsg
021900170629     C*
022000171016     C*  se torna con errore lo deve segnalare
022100171016     c                   if        errCall ='1'
022200171016     C                   eval      DSMSMS = 'TISID0R-' + ErrMsg
022300171016     C                   eval       ERRL00 ='1'
022400171016     C*
022500171016     c                   else
022600171016     C* AL TERMINE ESCO se non c'erano errori
022700170629     C                   MOVEL     '1'           F12L00
022800171016     c                   endif
022900171016     C*
023000991105     C                   ENDIF
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
023500000000     C*------------------------------------------------------------------------*
023600000000     C     F12GES        BEGSR
023700000000     C*
023800000000     C* IMPOSTO FLAG DI LIVELLO
023900991103     C                   MOVEL     '1'           F12L00
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
025100991020     C*------------------------------------------------------------------------*
025200000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
025300000000     C*------------------------------------------------------------------------*
025400000000     C     DECODI        BEGSR
025500060426     C*
025600171013     C*     non può essere una decorrenza Futura in archivio
025700171013     c                   if        DATA_OGGI < DAT_DECOR
025800171013     C                   MOVEL     MSG(2)        DSMSMS
025900171013     C                   MOVEL     '1'           ERRL00
026000171013     C                   END
026100000104     C*
026200000000     C                   ENDSR
026300170629     C*--------------------------------------------------------------------------------------------*
026400170629     C* REPERISCE I DATI UTENTE
026500170629     C*--------------------------------------------------------------------------------------------*
026600170629     C     REPDATIUTE    BEGSR
026700170629     C*
026800170629     C* INIZIALIZZA VARIABILI DI WRK
026900170629     C                   CLEAR                   TIBS34DS
027000170629     C                   CLEAR                   AZUTEDS
027100170629     C                   CLEAR                   DDATIUTE
027200170629     C*
027300170629     C     *DTAARA       DEFINE    §azute        azuteds
027400170629     C     *DTAARA       DEFINE    §datiute      ddatiute
027500170629     C                   IN(E)     *DTAARA
027600170629if  1C                   IF        %Error
027700170629     c                   EVAL      I34Tla = 'L'
027800170629     C                   CALL      'TIBS34R'
027900170629     C                   PARM                    Tibs34Ds
028000170629     C                   IN        *DTAARA
028100170629e   1C                   ENDIF
028200170629     C*
028300170629     C                   ENDSR
028400000000     C*------------------------------------------------------------------------*
028500980403     C* *INZSR - OPERAZIONI INIZIALI
028600000000     C*------------------------------------------------------------------------*
028700980403     C     *INZSR        BEGSR
028800000000     C*
028900000000     C* RICEVIMENTO PARAMETRI
029000000000     C     *ENTRY        PLIST
029100000000     C                   PARM                    KPJBA
029200000000     C*
029300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
029400000000     C     XMSGPA        PLIST
029500000000     C                   PARM                    DSMSMS
029600000000     C                   PARM                    DSMSPG
029700000000     C                   PARM                    DSMSKE
029800000000     C                   PARM                    DSMSER
029900000000     C*
030000000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
030100000000     C                   CALL      'XKEY'
030200000000     C                   PARM                    DSKEY
030300000000     C*
030400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
030500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
030600000000     C*
030700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
030800000000     C*   -NOME PGM CHIAMANTE
030900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
031000170629     C                   MOVEL     'TISID0R'     DSMSPG
031100170629     C                   MOVEL     'TISID0R'     MSGQUE
031200000000     C                   MOVEL     *BLANKS       MSGKEY
031300000000     C                   MOVEL     *BLANKS       DSMSMS
031400000000     C*
031500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
031600000000     C                   Z-ADD     0             FUNGRU            5 0
031700000000     C*
031800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
031900000000     C                   EXSR      FUNGES
032000000000     C*
032100000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
032200000000     C                   MOVEL     '0'           F12L00            1
032300000000     C                   MOVEL     '0'           F03L00            1
032400000000     C                   MOVEL     '0'           FINL00            1
032500000000     C                   MOVEL     '0'           ERRL00            1
032600000000     C                   MOVEL     '0'           FLFINE            1
032700170629     C*
032800170629     C* LANCIO ROUTINE PER VERIFICARE SE
032900170629     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
033000170629     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
033100170629     C                   EXSR      REPDATIUTE
033200171013     C*  AAAAMMGG
033300171013     c                   eval      DATA_OGGI = *date
033400171013     C*
033500000000     C                   ENDSR
033600000000     C*------------------------------------------------------------------------*
033700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
033800000000     C*------------------------------------------------------------------------*
033900000000     C     FUNGES        BEGSR
034000000000     C*
034100000000     C* PULISCO CAMPI DI LAVORO
034200000000     C                   Z-ADD     0             W                 5 0
034300000000     C*
034400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
034600000000     C     1             DO        24            I
034700000000     C                   MOVEL     FUD(I)        FUDRIG
034800000000     C*
034900000000     C* PULIZIA FUNZIONI PRECEDENTI
035000000000     C                   MOVEL     *BLANKS       FUV(I)
035100000000     C*
035200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
035300000000     C* (SALTA F24)
035400000000     C     FUDATT        IFEQ      'S'
035500000000     C     FUDFUN        ANDNE     '24'
035600000000     C                   ADD       1             W
035700000000     C                   MOVEL     FUDDES        FUV(W)
035800000000     C                   END
035900000000     C                   END                                                    DO WHILE
036000000000     C*
036100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036200000000     C* ALTRIMENTI DISATTIVO F24
036300000000     C     1             DO        24            I
036400000000     C                   MOVEL     FUD(I)        FUDRIG
036500000000     C*
036600000000     C     FUDFUN        IFEQ      '24'
036700000000     C     W             IFGT      6
036800000000     C                   MOVEL     'S'           FUDATT
036900000000     C                   ELSE
037000000000     C                   MOVEL     *BLANKS       FUDATT
037100000000     C                   END                                                    W GT 8
037200000000     C                   MOVEL     FUDRIG        FUD(I)
037300000000     C                   END
037400000000     C                   END                                                    DO WHILE
037500000000     C*
037600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
037700000000     C     W             DIV       6             I
037800000000     C                   MVR                     Z
037900000000     C*
038000000000     C     Z             IFNE      0
038100000000     C     I             ADD       1             Z
038200000000     C                   ELSE
038300000000     C                   Z-ADD     I             Z                 5 0
038400000000     C                   END
038500000000     C*
038600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
038700000000     C                   ADD       1             FUNGRU
038800000000     C     FUNGRU        IFGT      Z
038900000000     C     FUNGRU        ORLT      1
039000000000     C                   Z-ADD     1             FUNGRU
039100000000     C                   END
039200000000     C*
039300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
039400000000     C     FUNGRU        IFEQ      1
039500000000     C                   MOVEL     FUV(1)        FUN001
039600000000     C                   MOVEL     FUV(2)        FUN002
039700000000     C                   MOVEL     FUV(3)        FUN003
039800000000     C                   MOVEL     FUV(4)        FUN004
039900000000     C                   MOVEL     FUV(5)        FUN005
040000000000     C                   MOVEL     FUV(6)        FUN006
040100000000     C                   END
040200000000     C*
040300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
040400000000     C     FUNGRU        IFEQ      2
040500000000     C                   MOVEL     FUV(7)        FUN001
040600000000     C                   MOVEL     FUV(8)        FUN002
040700000000     C                   MOVEL     FUV(9)        FUN003
040800000000     C                   MOVEL     FUV(10)       FUN004
040900000000     C                   MOVEL     FUV(11)       FUN005
041000000000     C                   MOVEL     FUV(12)       FUN006
041100000000     C                   END
041200000000     C*
041300000000     C* TERZO GRUPPO DI 6 FUNZIONI
041400000000     C     FUNGRU        IFEQ      3
041500000000     C                   MOVEL     FUV(13)       FUN001
041600000000     C                   MOVEL     FUV(14)       FUN002
041700000000     C                   MOVEL     FUV(15)       FUN003
041800000000     C                   MOVEL     FUV(16)       FUN004
041900000000     C                   MOVEL     FUV(17)       FUN005
042000000000     C                   MOVEL     FUV(18)       FUN006
042100000000     C                   END
042200000000     C*
042300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
042400000000     C     FUNGRU        IFEQ      4
042500000000     C                   MOVEL     FUV(19)       FUN001
042600000000     C                   MOVEL     FUV(20)       FUN002
042700000000     C                   MOVEL     FUV(21)       FUN003
042800000000     C                   MOVEL     FUV(22)       FUN004
042900000000     C                   MOVEL     *BLANKS       FUN005
043000000000     C                   MOVEL     *BLANKS       FUN006
043100000000     C                   END
043200000000     C*
043300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
043400000000     C                   MOVEL     *BLANKS       FUN007
043500000000     C                   MOVEL     *BLANKS       FUN008
043600000000     C*
043700000000     C     1             DO        24            I
043800000000     C                   MOVEL     FUD(I)        FUDRIG
043900000000     C     FUDFUN        IFEQ      '24'
044000000000     C     FUDATT        ANDEQ     'S'
044100000000     C                   MOVEL     FUDDES        FUN008
044200000000     C                   END
044300000000     C                   END                                                    DO
044400000000     C                   ENDSR
044500000000     C*------------------------------------------------------------------------*
044600000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
044700000000     C*------------------------------------------------------------------------*
044800000000     C     FUNCON        BEGSR
044900000000     C*
045000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
045100000000     C                   EXSR      FUNVAR
045200000000     C*
045300000000     C* PULISCO CAMPI DI LAVORO
045400000000     C                   Z-ADD     0             I                 5 0
045500000000     C                   Z-ADD     0             W                 5 0
045600000000     C*
045700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
045800000000     C     FUNKEY        IFNE      *BLANKS
045900000000     C     1             DO        24            I
046000000000     C                   MOVEL     FUD(I)        FUDRIG
046100000000     C*
046200000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
046300000000     C     FUNKEY        IFEQ      FUDFUN
046400000000     C     FUDATT        ANDEQ     'S'
046500000000     C                   Z-ADD     I             W
046600000000     C                   END
046700000000     C                   END                                                    DO WHILE
046800000000     C*
046900000000     C* FUNZIONE NON ATTIVA (ERRORE)
047000000000     C     W             IFEQ      0
047100000000     C*
047200000000     C* EMISSIONE MESSAGGIO DI ERRORE
047300000000     C                   MOVEL     MSG(1)        DSMSMS
047400000000     C*
047500000000     C                   END                                                    W=0
047600000000     C                   END                                                    FUNKEY NE *BLANKS
047700000000     C*
047800000000     C* FUNZIONE ATTIVA (PROCEDI ...)
047900000000     C     W             IFNE      0
048000000000     C     FUNKEY        OREQ      *BLANKS
048100000000     C*
048200000000     C     FUNKEY        IFEQ      *BLANKS
048300000000     C                   EXSR      ENTGES
048400000000     C                   END
048500000000     C     FUNKEY        IFEQ      '01'
048600000000     C                   END
048700000000     C     FUNKEY        IFEQ      '03'
048800000000     C                   EXSR      F03GES
048900000000     C                   END
049000000000     C     FUNKEY        IFEQ      '06'
049100000000     C                   EXSR      F06GES
049200000000     C                   END
049300000000     C     FUNKEY        IFEQ      '07'
049400000000     C                   END
049500000000     C     FUNKEY        IFEQ      '08'
049600000000     C                   END
049700060608     C     FUNKEY        IFEQ      '10'
049800060608     C                   END
049900000000     C     FUNKEY        IFEQ      '12'
050000000000     C                   EXSR      F12GES
050100000000     C                   END
050200000000     C     FUNKEY        IFEQ      '21'
050300000000     C                   END
050400000000     C     FUNKEY        IFEQ      '22'
050500000000     C                   END
050600000000     C     FUNKEY        IFEQ      '24'
050700000000     C                   EXSR      F24GES
050800000000     C                   END
050900000000     C     FUNKEY        IFEQ      'HP'
051000000000     C                   END
051100000000     C*
051200000000     C                   END
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
051700000000     C*------------------------------------------------------------------------*
051800000000     C     FUNVAR        BEGSR
051900000000     C*
052000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
052100000000     C*
052200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
052300000000     C     KEY           IFEQ      ENTER
052400000000     C                   MOVEL     *BLANKS       FUNKEY
052500000000     C                   END
052600000000     C     KEY           IFEQ      F1
052700000000     C                   MOVEL     '01'          FUNKEY
052800000000     C                   END
052900000000     C     KEY           IFEQ      F2
053000000000     C                   MOVEL     '02'          FUNKEY
053100000000     C                   END
053200000000     C     KEY           IFEQ      F3
053300000000     C                   MOVEL     '03'          FUNKEY
053400000000     C                   END
053500000000     C     KEY           IFEQ      F4
053600000000     C                   MOVEL     '04'          FUNKEY
053700000000     C                   END
053800000000     C     KEY           IFEQ      F5
053900000000     C                   MOVEL     '05'          FUNKEY
054000000000     C                   END
054100991103     C     KEY           IFEQ      F6
054200000000     C                   MOVEL     '06'          FUNKEY
054300000000     C                   END
054400000000     C     KEY           IFEQ      F7
054500000000     C                   MOVEL     '07'          FUNKEY
054600000000     C                   END
054700000000     C     KEY           IFEQ      F8
054800000000     C                   MOVEL     '08'          FUNKEY
054900000000     C                   END
055000000000     C     KEY           IFEQ      F9
055100000000     C                   MOVEL     '09'          FUNKEY
055200000000     C                   END
055300000000     C     KEY           IFEQ      F10
055400000000     C                   MOVEL     '10'          FUNKEY
055500000000     C                   END
055600000000     C     KEY           IFEQ      F11
055700000000     C                   MOVEL     '11'          FUNKEY
055800000000     C                   END
055900000000     C     KEY           IFEQ      F12
056000000000     C                   MOVEL     '12'          FUNKEY
056100000000     C                   END
056200000000     C     KEY           IFEQ      F13
056300000000     C                   MOVEL     '13'          FUNKEY
056400000000     C                   END
056500000000     C     KEY           IFEQ      F14
056600000000     C                   MOVEL     '14'          FUNKEY
056700000000     C                   END
056800000000     C     KEY           IFEQ      F15
056900000000     C                   MOVEL     '15'          FUNKEY
057000000000     C                   END
057100000000     C     KEY           IFEQ      F16
057200000000     C                   MOVEL     '16'          FUNKEY
057300000000     C                   END
057400000000     C     KEY           IFEQ      F17
057500000000     C                   MOVEL     '17'          FUNKEY
057600000000     C                   END
057700000000     C     KEY           IFEQ      F18
057800000000     C                   MOVEL     '18'          FUNKEY
057900000000     C                   END
058000000000     C     KEY           IFEQ      F19
058100000000     C                   MOVEL     '19'          FUNKEY
058200000000     C                   END
058300000000     C     KEY           IFEQ      F20
058400000000     C                   MOVEL     '20'          FUNKEY
058500000000     C                   END
058600000000     C     KEY           IFEQ      F21
058700000000     C                   MOVEL     '21'          FUNKEY
058800000000     C                   END
058900000000     C     KEY           IFEQ      F22
059000000000     C                   MOVEL     '22'          FUNKEY
059100000000     C                   END
059200000000     C     KEY           IFEQ      F23
059300000000     C                   MOVEL     '23'          FUNKEY
059400000000     C                   END
059500000000     C     KEY           IFEQ      F24
059600000000     C                   MOVEL     '24'          FUNKEY
059700000000     C                   END
059800000000     C     KEY           IFEQ      ROLLUP
059900000000     C                   MOVEL     'RU'          FUNKEY
060000000000     C                   END
060100000000     C     KEY           IFEQ      HELP
060200000000     C                   MOVEL     'HP'          FUNKEY
060300000000     C                   END
060400000000     C                   ENDSR
060500000000     C*------------------------------------------------------------------------*
060600000000     C* NOTE PER COMPILAZIONE SCHIERE
060700000000     C*------------------------------------------------------------------------*
060800000000     C* -MSG-
060900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
061000000000     C* (NON TOCCARE MSG,1)
061100000000     C*
061200000000     C* -CMD-
061300000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
061400000000     C*
061500000000     C* -FUD-
061600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
061700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
062000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
062200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
062300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
062500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
062600000000     C*
062700000000     C*------------------------------------------------------------------------*
062800991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
062900170629TISID0R- Tasto funzionale non gestito.                                         1
063000171017TISID0R- Esiste un Cappario con decorrenza FUTURA! Eliminarlo Manualmente.     2
063100171016TISID0R-                                                                       3
063200170629TISID0R-                                                                       4
063300170629TISID0R-                                                                       5
063400170629TISID0R-                                                                       6
063500170629TISID0R-                                                                       7
063600170629TISID0R-                                                                       8
063700170629TISID0R-                                                                       9
063800170629TISID0R-                                                                      10
063900170629TISID0R-                                                                      11
064000170629TISID0R-                                                                      12
064100170629TISID0R-                                                                      13
064200170629TISID0R-                                                                      14
064300170629TISID0R-                                                                      15
064400170629TISID0R-                                                                      16
064500170629TISID0R-                                                                      17
064600170629TISID0R-                                                                      18
064700170629TISID0R-                                                                      19
064800170629TISID0R-                                                                      20
064900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
065000000000                   -        (S/N)=N                        funz.01¬
065100000000                   -        (S/N)=N                        funz.02¬
065200000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
065300000000                   -        (S/N)=N                        funz.04¬
065400000000                   -        (S/N)=N                        funz.05¬
065500170629F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
065600060608                   -        (S/N)=N                        funz.07¬
065700060608                   -        (S/N)=N                        funz.08¬
065800000000                   -        (S/N)=N                        funz.09¬
065900170629                   -        (S/N)=N                        funz.10¬
066000000000                   -        (S/N)=N                        funz.11¬
066100991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
066200000000                   -        (S/N)=N                        funz.13¬
066300000000                   -        (S/N)=N                        funz.14¬
066400000000                   -        (S/N)=N                        funz.15¬
066500000000                   -        (S/N)=N                        funz.16¬
066600000000                   -        (S/N)=N                        funz.17¬
066700000000                   -        (S/N)=N                        funz.18¬
066800000000                   -        (S/N)=N                        funz.19¬
066900000000                   -        (S/N)=N                        funz.20¬
067000000000                   -        (S/N)=N                        funz.21¬
067100000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
067200000000                   -        (S/N)=N                        funz.23¬
067300170629F24=Altri tasti    -        (S/N)=N                        funz.24¬
