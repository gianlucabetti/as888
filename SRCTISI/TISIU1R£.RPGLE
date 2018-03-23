000100000612     H ALWNULL(*USRCTL)
000200000000     H*------------------------------------------------------------------------*
000300010904     H* VERIFICA LIVELLI DI GESTIONE CAP/LOC
000400010904     F*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900011119     Fazcpr01l  IF   E           K DISK
001000010904     Ftisiu1d   CF   E             WORKSTN INFDS(DEVDS1)
001100010904     F                                     SFILE(SIU1DMS:MSGULT)
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* COMANDI
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
005600011119     D*-------------------
005700000505     D* DS "XSRDAT" - CONTROLLO DATA (8)
005800011119     D*-------------------
005900000505     DWLBDA8           DS                  INZ
006000000505     D  G08DAT                 1      8  0
006100000505     D  G08INV                 9     16  0
006200000505     D  G08ERR                17     17
006300000505     D  G08TGI                18     22  0
006400000000     D*-------------------
006500000000     D* DS PER GESTIONE TASTI FUNZIONALI
006600000000     D*-------------------
006700000000     D DSKEY         E DS
006800000000     D DEVDS1          DS
006900000000     D  KEY                  369    369
007000000000     D*-------------------
007100000000     D* ARCHITETTURA
007200000000     D*-------------------
007300000000     D KPJBA         E DS
007400010906     D*-------------------
007500000505     D* DS DI PASSAGGI DATI
007600000505     D*-------------------
007700000505     D  dssi00       E DS                  EXTNAME(tisp00ds)                    *ds di procedura
007800000911     D*-------------------
007900011119     D* DS PER PASSAGGIO PARAMETRI A *PGM DI ELABORAZIONE
008000011119     D*-------------------
008100011119     D param           DS
008200011119     D  parver                        5S 0 inz(*zeros)
008300011119     D  parcap                        9    inz(*blanks)
008400011119     D  parsim                        1    inz('N')
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800000000     C*
008900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
009000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
009100000000     C*
009200000000     C* CICLO FINO A CHE:
009300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009500000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
009700000000     C     F12L00        OREQ      '1'
009800000000     C*
009900000000     C* EMETTE FORMATO VIDEO
010000000000     C                   EXSR      BUTFMR
010100000000     C*
010200000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010300000000     C                   EXSR      FUNCON
010400000000     C*
010500010905     C                   ENDDO                                                  <-- 2 --
010600000000     C*
010700010905     C                   ENDIF                                                  <-- 1 --
010800991118     C*
010900000000     C                   SETON                                        LR
011000000000     C*------------------------------------------------------------------------*
011100000000     C* EMISSIONE FORMATO A VIDEO
011200000000     C*------------------------------------------------------------------------*
011300000000     C     BUTFMR        BEGSR
011400000000     C*
011500000000     C* WRITE RECORD TESTATA
011600010904     C                   WRITE     SIU1D01
011700000000     C*
011800000000     C* VISUALIZZAZIONE MESSAGGIO
011900000000     C                   EXSR      SNDMSG
012000000000     C*
012100000000     C* WRITE RECORD FUNZIONI
012200010904     C                   WRITE     SIU1D02
012300991019     C*
012400991019     C* EXFMT RECORD DETTAGLIO
012500010904     C                   EXFMT     SIU1D03
012600991019     C*
012700000000     C* AZZERO FLAG DI ERRORE LIVELLO
012800000000     C                   MOVEL     '0'           ERRL00
012900000000     C*
013000000505     C* AZZERO FLAG DI ERRORE
013100010905     C                   SETOFF                                       7072
013200000000     C*
013300000000     C                   ENDSR
013400000000     C*------------------------------------------------------------------------*
013500000000     C* SNDMSG - MANDA MESSAGGIO
013600000000     C*------------------------------------------------------------------------*
013700000000     C     SNDMSG        BEGSR
013800000000     C*
013900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014000000000     C                   MOVEL     '1'           *IN40
014100010904     C                   WRITE     SIU1DMC
014200000000     C                   MOVEL     '0'           *IN40
014300000000     C*
014400000000     C* VISUALIZZAZIONE MESSAGGIO
014500000000     C                   Z-ADD     0             MSGULT            4 0
014600000000     C                   CALL      'XMSG'        XMSGPA
014700000000     C     DSMSER        IFEQ      0
014800000000     C                   MOVEL     DSMSKE        MSGKEY
014900000000     C                   END
015000000000     C                   Z-ADD     1             MSGULT
015100010904     C                   WRITE     SIU1DMS                                      SFL MESS
015200010904     C                   WRITE     SIU1DMC                                      CONTR MESS
015300000000     C*
015400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015500000000     C                   MOVEL     *BLANKS       DSMSMS
015600000000     C*
015700000000     C                   ENDSR
015800000000     C*------------------------------------------------------------------------*
015900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016000000000     C*------------------------------------------------------------------------*
016100000000     C     ENTGES        BEGSR
016200000000     C*
016300000000     C* CONTROLLA I CAMPI IMMESSI
016400000000     C                   EXSR      DECODI
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
016900000000     C*------------------------------------------------------------------------*
017000000000     C     F03GES        BEGSR
017100000000     C*
017200000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017300000000     C                   MOVEL     '1'           F03L00
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700011120     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA STEP1
017800000000     C*------------------------------------------------------------------------*
017900991020     C     F06GES        BEGSR
018000000000     C*
018100991105     C                   EXSR      DECODI
018200001213     C* Vedo se a video è tutto OK altrimenti la conferma non è valida
018300991105     C     ERRL00        IFEQ      '0'
018400070511     C*
018500070511     C* Se lancio in aggiornamento => nn consento
018600070511     C                   IF        VIDSIM = 'N'
018700070511     C                   MOVEL     '1'           ERRL00
018800070511     C                   MOVEL     MSG(11)       DSMSMS
018900070511     C                   SETON                                        73
019000070511     C                   ELSE
019100011119     C*
019200011119     C* Imposto i parametri di passaggio
019300011119     C                   MOVE      VIDVER        PARVER
019400011119     C                   EVAL      PARCAP = VIDCAP
019500011119     C                   EVAL      PARSIM = VIDSIM
019600011119     C*
019700011119     C* Lancio il programma di elaborazione
019800011119     C                   MOVEL     'SIU2'        KCOAZ
019900011119     C                   MOVEL(p)  PARAM         KPJBU
020000070511 MOM C***                CALL      'BCH10'
020100070511     C                   CALL      'TISIU2R'
020200011119     C                   PARM                    KPJBA
020300991124     C*
020400001213     C* Imposto il flag F03 di livello PGM per l'uscita automatica a fine elaborazione
020500001213     C                   MOVEL     '1'           F03L00
020600001213     C*
020700070511     C                   ENDIF
020800991105     C                   ENDIF
020900991105     C*
021000000000     C                   ENDSR
021100011120     C*------------------------------------------------------------------------*
021200011120     C* F07GES -GESTIONE TASTO FUNZIONALE F07 - CONFERMA STEP2
021300011120     C*------------------------------------------------------------------------*
021400011120     C     F07GES        BEGSR
021500011120     C*
021600011120     C                   EXSR      DECODI
021700011120     C* Vedo se a video è tutto OK altrimenti la conferma non è valida
021800011120     C     ERRL00        IFEQ      '0'
021900070511     C*
022000070511     C* Se utente nn EDP => nn consento aggiornamento
022100070511     C                   IF        %subst(KNMUS:1:3)<>'EDP' and VIDSIM='N'
022200070511     C                   MOVEL     '1'           ERRL00
022300070511     C                   MOVEL     MSG(12)       DSMSMS
022400070511     C                   SETON                                        73
022500070511     C                   ELSE
022600011120     C*
022700011120     C* Imposto i parametri di passaggio
022800011120     C                   MOVE      VIDVER        PARVER
022900011120     C                   EVAL      PARCAP = VIDCAP
023000011120     C                   EVAL      PARSIM = VIDSIM
023100011120     C*
023200011120     C* Lancio il programma di elaborazione
023300011120     C                   MOVEL     'SIU3'        KCOAZ
023400011120     C                   MOVEL(p)  PARAM         KPJBU
023500070511 MOM C***                CALL      'BCH10'
023600070511     C                   CALL      'TISIU3R'
023700011120     C                   PARM                    KPJBA
023800011120     C*
023900011120     C* Imposto il flag F03 di livello PGM per l'uscita automatica a fine elaborazione
024000011120     C                   MOVEL     '1'           F03L00
024100011120     C*
024200070511     C                   ENDIF
024300011120     C                   ENDIF
024400011120     C*
024500011120     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
024800000000     C*------------------------------------------------------------------------*
024900000000     C     F12GES        BEGSR
025000000000     C*
025100000000     C* IMPOSTO FLAG DI LIVELLO
025200000000     C                   MOVEL     '1'           F12L00
025300000000     C*
025400000000     C                   ENDSR
025500000000     C*------------------------------------------------------------------------*
025600991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
025700000000     C*------------------------------------------------------------------------*
025800000000     C     F24GES        BEGSR
025900000000     C*
026000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
026100000000     C                   EXSR      FUNGES
026200000000     C*
026300000000     C                   ENDSR
026400991020     C*------------------------------------------------------------------------*
026500000531     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
026600000000     C*------------------------------------------------------------------------*
026700000000     C     DECODI        BEGSR
026800000505     C*
026900010905     C* CONTROLLO DATA RIFERIMENTO
027000000505     C     ERRL00        IFEQ      '0'
027100000505     C                   MOVEL     VIHDAT        G08DAT
027200000505     C                   Z-ADD     *ZEROS        G08INV
027300000505     C                   MOVEL     '0'           G08ERR
027400000505     C                   CALL      'XSRDA8'
027500000505     C                   PARM                    WLBDA8
027600000505     C     G08ERR        IFEQ      '1'
027700000505     C                   MOVEL     '1'           ERRL00
027800000505     C                   MOVEL     MSG(3)        DSMSMS
027900000505     C                   SETON                                        74
028000000505     C                   ELSE
028100000505     C                   Z-ADD     G08INV        INVDAT            8 0
028200000505     C                   ENDIF
028300000505     C                   ENDIF
028400991117     C*
028500000505     C* REPERIMENTO VERSIONI BAR X DATA DI RIFERIMENTO
028600000505     C     ERRL00        IFEQ      '0'
028700000505     C     VIDVER        IFEQ      *blanks
028800000505     C                   EXSR      REPVEX
028900000505     C                   ENDIF
029000000505     C                   ENDIF
029100000505     C*
029200110511     C* CONTROLLO VERSIONE CAPPARIO  BRT
029300000505     C     ERRL00        IFEQ      '0'
029400000505     C                   EXSR      CHKVER
029500000505     C                   ENDIF
029600000225     C*
029700000000     C                   ENDSR
029800000505     C*------------------------------------------------------------------------*
029900110511     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE BRT
030000000505     C*------------------------------------------------------------------------*
030100000505     C     CHKVER        BEGSR
030200000505     C*
030300000505     C                   IF        %trim(VIDVER)='?'
030400000505     C*
030500000505     C* lancia il programma di gestione versioni BAR in ricerca
030600000505     C                   CLEAR                   dssi00                         *pulisce ds
030700000505     C                   MOVEL     'R01'         d00op0                         *livello procedura
030800000505     C                   MOVEL     *blanks       d00op1                         *livello programma
030900000505     C                   MOVEL     '0'           d00f03                         *premuto F03
031000000505     C                   MOVEL     '0'           d00f12                         *premuto F12
031100000505     C                   MOVEL     '0'           d00err                         *errore
031200000505     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
031300000505     C                   CALL      'TISI46R'                                    *gestione versioni
031400000505     C                   PARM                    kpjba
031500000505     C                   PARM                    dssi00
031600000505     C* ritorno
031700000505     C                   IF        d00err='1'
031800000508     C                   MOVEL     '1'           ERRL00
031900000505     C                   MOVEL     MSG(5)        DSMSMS
032000000505     C                   ELSE
032100000505     C                   MOVE(p)   d00ver        VIDVER
032200000505     C                   Z-ADD     d00ver        VIHVER
032300000505     C                   Z-ADD     d00dde        VIHDDE
032400000505     C                   Z-ADD     d00edd        VIHDSC
032500000505     C                   ENDIF
032600000505     C*
032700000505     C                   ELSE
032800010905     C                   CALL(e)   'ISNUMERIC'
032900000505     C                   PARM      VIDVER        PiStr            30
033000000505     C                   PARM      ','           PiDecChr          1
033100000505     C                   PARM      *ZEROS        PiVal            30 9
033200000505     C                   PARM      '0'           PiInt             1
033300000505     C                   PARM      '0'           PiNum             1
033400000505     C                   IF        NOT %error AND
033500000505     C                             PiInt=*ON
033600001213     C                   Z-ADD     PiVal         VIHVER
033700001213     C                   MOVE(p)   VIHVER        VIDVER
033800000505     C                   ELSE
033900001213     C                   SETOFF                                       70
034000001213     C                   MOVEL     MSG(10)       DSMSMS
034100000505     C                   Z-ADD     *ZEROS        VIHVER
034200000505     C                   ENDIF
034300000505     C*
034400000505     C     VIHVER        IFEQ      *ZEROS
034500000505     C                   MOVEL     '1'           ERRL00
034600000505     C                   ELSE
034700000505     C*
034800000505     C     VIHVER        CHAIN     AZCPR01L                           99
034900000505     C     *IN99         IFEQ      *ON
035000000505     C                   SETOFF                                       70
035100000505     C                   MOVEL     MSG(7)        DSMSMS
035200000505     C                   ELSE
035300000505     C                   Z-ADD     cprdde        VIHDDE
035400000505     C                   Z-ADD     cprdsc        VIHDSC
035500000505     C                   ENDIF
035600000505     C*
035700000505     C                   ENDIF
035800000505     C                   ENDIF
035900000505     C*
036000000505     C                   ENDSR
036100000505     C*------------------------------------------------------------------------*
036200000505     C*  REPVEX - REPERISCE LE VERSIONI DEI CAPPARI BAR/POSTE
036300000505     C*------------------------------------------------------------------------*
036400000505     C     REPVEX        BEGSR
036500000505     C*
036600110511     C* REPERISCE LA VERSIONE BRT
036700000505     C                   Z-ADD     INVDAT        IPTDRI            8 0
036800000505     C                   Z-ADD     *ZEROS        IPTVER            5 0
036900000505     C                   CALL      'TISI96R'
037000000505     C                   PARM                    IPTDRI
037100000505     C                   PARM                    IPTVER
037200000505     C                   PARM                    IPTDES            5
037300000505     C                   IF        IPTVER>*zeros
037400000505     C                   SETOFF                                       81
037500000505     C                   MOVE(p)   IPTVER        VIDVER
037600000505     C                   Z-ADD     IPTVER        VIHVER
037700000505     C                   Z-ADD     IPTVER        VIHVER
037800000505     C     IPTVER        CHAIN     azcpr01l
037900000505     C                   Z-ADD     cprdde        VIHDDE
038000000505     C                   Z-ADD     cprdsc        VIHDSC
038100000505     C                   ELSE
038200000505     C                   MOVEL     MSG(9)        DSMSMS
038300000505     C                   ENDIF
038400000505     C*
038500000505     C                   ENDSR
038600000000     C*------------------------------------------------------------------------*
038700000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
038800000000     C*------------------------------------------------------------------------*
038900000000     C     AZZVID        BEGSR
039000000000     C*
039100000505     C                   Z-ADD     WN8           VIHDAT
039200000505     C                   MOVEL     *BLANKS       VIDVER
039300000505     C                   MOVEL     *BLANKS       VIDCAP
039400000531     C                   MOVEL     'S'           VIDSIM
039500000000     C*
039600000000     C                   ENDSR
039700000000     C*------------------------------------------------------------------------*
039800980403     C* *INZSR - OPERAZIONI INIZIALI
039900000000     C*------------------------------------------------------------------------*
040000980403     C     *INZSR        BEGSR
040100000000     C*
040200000000     C* RICEVIMENTO PARAMETRI
040300000000     C     *ENTRY        PLIST
040400000000     C                   PARM                    KPJBA
040500011119     C*
040600011119     C* CALCOLA LA DATA CORRENTE
040700011119     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
040800011119     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
040900011119     C                   Z-ADD     WN8           G08DAT
041000011119     C                   Z-ADD     *ZEROS        G08INV
041100011119     C                   MOVEL     '0'           G08ERR
041200011119     C                   CALL      'XSRDA8'
041300011119     C                   PARM                    WLBDA8
041400011119     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041500000000     C*
041600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
041700000000     C     XMSGPA        PLIST
041800000000     C                   PARM                    DSMSMS
041900000000     C                   PARM                    DSMSPG
042000000000     C                   PARM                    DSMSKE
042100000000     C                   PARM                    DSMSER
042200000000     C*
042300000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
042400000000     C                   CALL      'XKEY'
042500000000     C                   PARM                    DSKEY
042600000000     C*
042700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
042800000225     C     *LIKE         DEFINE    FUDFUN        FUNKEY
042900000000     C*
043000000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
043100000000     C*   -NOME PGM CHIAMANTE
043200000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
043300010904     C                   MOVEL     'TISIU1R'     DSMSPG
043400010904     C                   MOVEL     'TISIU1R'     MSGQUE
043500000000     C                   MOVEL     *BLANKS       MSGKEY
043600000000     C                   MOVEL     *BLANKS       DSMSMS
043700000000     C*
043800000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
043900000000     C                   Z-ADD     0             FUNGRU            5 0
044000000000     C*
044100000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
044200000000     C                   EXSR      FUNGES
044300000000     C*
044400000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
044500000000     C                   MOVEL     '0'           F12L00            1
044600000000     C                   MOVEL     '0'           F03L00            1
044700000000     C                   MOVEL     '0'           FINL00            1
044800000000     C                   MOVEL     '0'           ERRL00            1
044900000000     C*
045000000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
045100000000     C                   EXSR      AZZVID
045200000000     C*
045300000000     C                   ENDSR
045400000000     C*------------------------------------------------------------------------*
045500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
045600000000     C*------------------------------------------------------------------------*
045700000000     C     FUNGES        BEGSR
045800000000     C*
045900000000     C* PULISCO CAMPI DI LAVORO
046000000000     C                   Z-ADD     0             W                 5 0
046100000000     C*
046200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
046300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
046400000000     C     1             DO        24            I
046500000000     C                   MOVEL     FUD(I)        FUDRIG
046600000000     C*
046700000000     C* PULIZIA FUNZIONI PRECEDENTI
046800000000     C                   MOVEL     *BLANKS       FUV(I)
046900000000     C*
047000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
047100000000     C* (SALTA F24)
047200000000     C     FUDATT        IFEQ      'S'
047300000000     C     FUDFUN        ANDNE     '24'
047400000000     C                   ADD       1             W
047500000000     C                   MOVEL     FUDDES        FUV(W)
047600000000     C                   END
047700000000     C                   END                                                    DO WHILE
047800000000     C*
047900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
048000000000     C* ALTRIMENTI DISATTIVO F24
048100000000     C     1             DO        24            I
048200000000     C                   MOVEL     FUD(I)        FUDRIG
048300000000     C*
048400000000     C     FUDFUN        IFEQ      '24'
048500000000     C     W             IFGT      6
048600000000     C                   MOVEL     'S'           FUDATT
048700000000     C                   ELSE
048800000000     C                   MOVEL     *BLANKS       FUDATT
048900000000     C                   END                                                    W GT 8
049000000000     C                   MOVEL     FUDRIG        FUD(I)
049100000000     C                   END
049200000000     C                   END                                                    DO WHILE
049300000000     C*
049400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
049500000000     C     W             DIV       6             I
049600000000     C                   MVR                     Z
049700000000     C*
049800000000     C     Z             IFNE      0
049900000000     C     I             ADD       1             Z
050000000000     C                   ELSE
050100000000     C                   Z-ADD     I             Z                 5 0
050200000000     C                   END
050300000000     C*
050400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
050500000000     C                   ADD       1             FUNGRU
050600000000     C     FUNGRU        IFGT      Z
050700000000     C     FUNGRU        ORLT      1
050800000000     C                   Z-ADD     1             FUNGRU
050900000000     C                   END
051000000000     C*
051100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
051200000000     C     FUNGRU        IFEQ      1
051300000000     C                   MOVEL     FUV(1)        FUN001
051400000000     C                   MOVEL     FUV(2)        FUN002
051500000000     C                   MOVEL     FUV(3)        FUN003
051600000000     C                   MOVEL     FUV(4)        FUN004
051700000000     C                   MOVEL     FUV(5)        FUN005
051800000000     C                   MOVEL     FUV(6)        FUN006
051900000000     C                   END
052000000000     C*
052100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
052200000000     C     FUNGRU        IFEQ      2
052300000000     C                   MOVEL     FUV(7)        FUN001
052400000000     C                   MOVEL     FUV(8)        FUN002
052500000000     C                   MOVEL     FUV(9)        FUN003
052600000000     C                   MOVEL     FUV(10)       FUN004
052700000000     C                   MOVEL     FUV(11)       FUN005
052800000000     C                   MOVEL     FUV(12)       FUN006
052900000000     C                   END
053000000000     C*
053100000000     C* TERZO GRUPPO DI 6 FUNZIONI
053200000000     C     FUNGRU        IFEQ      3
053300000000     C                   MOVEL     FUV(13)       FUN001
053400000000     C                   MOVEL     FUV(14)       FUN002
053500000000     C                   MOVEL     FUV(15)       FUN003
053600000000     C                   MOVEL     FUV(16)       FUN004
053700000000     C                   MOVEL     FUV(17)       FUN005
053800000000     C                   MOVEL     FUV(18)       FUN006
053900000000     C                   END
054000000000     C*
054100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
054200000000     C     FUNGRU        IFEQ      4
054300000000     C                   MOVEL     FUV(19)       FUN001
054400000000     C                   MOVEL     FUV(20)       FUN002
054500000000     C                   MOVEL     FUV(21)       FUN003
054600000000     C                   MOVEL     FUV(22)       FUN004
054700000000     C                   MOVEL     *BLANKS       FUN005
054800000000     C                   MOVEL     *BLANKS       FUN006
054900000000     C                   END
055000000000     C*
055100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
055200000000     C                   MOVEL     *BLANKS       FUN007
055300000000     C                   MOVEL     *BLANKS       FUN008
055400000000     C*
055500000000     C     1             DO        24            I
055600000000     C                   MOVEL     FUD(I)        FUDRIG
055700000000     C     FUDFUN        IFEQ      '24'
055800000000     C     FUDATT        ANDEQ     'S'
055900000000     C                   MOVEL     FUDDES        FUN008
056000000000     C                   END
056100000000     C                   END                                                    DO
056200000000     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
056500000000     C*------------------------------------------------------------------------*
056600000000     C     FUNCON        BEGSR
056700000000     C*
056800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
056900000000     C                   EXSR      FUNVAR
057000000000     C*
057100000000     C* PULISCO CAMPI DI LAVORO
057200000000     C                   Z-ADD     0             I                 5 0
057300000000     C                   Z-ADD     0             W                 5 0
057400000000     C*
057500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
057600000000     C     FUNKEY        IFNE      *BLANKS
057700000000     C     1             DO        24            I
057800000000     C                   MOVEL     FUD(I)        FUDRIG
057900000000     C*
058000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
058100000000     C     FUNKEY        IFEQ      FUDFUN
058200000000     C     FUDATT        ANDEQ     'S'
058300000000     C                   Z-ADD     I             W
058400000000     C                   END
058500000000     C                   END                                                    DO WHILE
058600000000     C*
058700000000     C* FUNZIONE NON ATTIVA (ERRORE)
058800000000     C     W             IFEQ      0
058900000000     C*
059000000000     C* EMISSIONE MESSAGGIO DI ERRORE
059100000000     C                   MOVEL     MSG(1)        DSMSMS
059200000000     C*
059300000000     C                   END                                                    W=0
059400000000     C                   END                                                    FUNKEY NE *BLANKS
059500000000     C*
059600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
059700000000     C     W             IFNE      0
059800000000     C     FUNKEY        OREQ      *BLANKS
059900000000     C*
060000000000     C     FUNKEY        IFEQ      *BLANKS
060100000000     C                   EXSR      ENTGES
060200000000     C                   END
060300000000     C     FUNKEY        IFEQ      '03'
060400000000     C                   EXSR      F03GES
060500000000     C                   END
060600000000     C     FUNKEY        IFEQ      '06'
060700000000     C                   EXSR      F06GES
060800000000     C                   END
060900011120     C     FUNKEY        IFEQ      '07'
061000011120     C                   EXSR      F07GES
061100011120     C                   END
061200000000     C     FUNKEY        IFEQ      '12'
061300000000     C                   EXSR      F12GES
061400000000     C                   END
061500000000     C     FUNKEY        IFEQ      '24'
061600000000     C                   EXSR      F24GES
061700000000     C                   END
061800000000     C*
061900000000     C                   END
062000000000     C*
062100000000     C                   ENDSR
062200000000     C*------------------------------------------------------------------------*
062300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
062400000000     C*------------------------------------------------------------------------*
062500000000     C     FUNVAR        BEGSR
062600000000     C*
062700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
062800000000     C*
062900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
063000000000     C     KEY           IFEQ      ENTER
063100000000     C                   MOVEL     *BLANKS       FUNKEY
063200000000     C                   END
063300000000     C     KEY           IFEQ      F1
063400000000     C                   MOVEL     '01'          FUNKEY
063500000000     C                   END
063600000000     C     KEY           IFEQ      F2
063700000000     C                   MOVEL     '02'          FUNKEY
063800000000     C                   END
063900000000     C     KEY           IFEQ      F3
064000000000     C                   MOVEL     '03'          FUNKEY
064100000000     C                   END
064200000000     C     KEY           IFEQ      F4
064300000000     C                   MOVEL     '04'          FUNKEY
064400000000     C                   END
064500000000     C     KEY           IFEQ      F5
064600000000     C                   MOVEL     '05'          FUNKEY
064700000000     C                   END
064800000000     C     KEY           IFEQ      F6
064900000000     C                   MOVEL     '06'          FUNKEY
065000000000     C                   END
065100000000     C     KEY           IFEQ      F7
065200000000     C                   MOVEL     '07'          FUNKEY
065300000000     C                   END
065400000000     C     KEY           IFEQ      F8
065500000000     C                   MOVEL     '08'          FUNKEY
065600000000     C                   END
065700000000     C     KEY           IFEQ      F9
065800000000     C                   MOVEL     '09'          FUNKEY
065900000000     C                   END
066000000000     C     KEY           IFEQ      F10
066100000000     C                   MOVEL     '10'          FUNKEY
066200000000     C                   END
066300000000     C     KEY           IFEQ      F11
066400000000     C                   MOVEL     '11'          FUNKEY
066500000000     C                   END
066600000000     C     KEY           IFEQ      F12
066700000000     C                   MOVEL     '12'          FUNKEY
066800000000     C                   END
066900000000     C     KEY           IFEQ      F13
067000000000     C                   MOVEL     '13'          FUNKEY
067100000000     C                   END
067200000000     C     KEY           IFEQ      F14
067300000000     C                   MOVEL     '14'          FUNKEY
067400000000     C                   END
067500000000     C     KEY           IFEQ      F15
067600000000     C                   MOVEL     '15'          FUNKEY
067700000000     C                   END
067800000000     C     KEY           IFEQ      F16
067900000000     C                   MOVEL     '16'          FUNKEY
068000000000     C                   END
068100000000     C     KEY           IFEQ      F17
068200000000     C                   MOVEL     '17'          FUNKEY
068300000000     C                   END
068400000000     C     KEY           IFEQ      F18
068500000000     C                   MOVEL     '18'          FUNKEY
068600000000     C                   END
068700000000     C     KEY           IFEQ      F19
068800000000     C                   MOVEL     '19'          FUNKEY
068900000000     C                   END
069000000000     C     KEY           IFEQ      F20
069100000000     C                   MOVEL     '20'          FUNKEY
069200000000     C                   END
069300000000     C     KEY           IFEQ      F21
069400000000     C                   MOVEL     '21'          FUNKEY
069500000000     C                   END
069600000000     C     KEY           IFEQ      F22
069700000000     C                   MOVEL     '22'          FUNKEY
069800000000     C                   END
069900000000     C     KEY           IFEQ      F23
070000000000     C                   MOVEL     '23'          FUNKEY
070100000000     C                   END
070200000000     C     KEY           IFEQ      F24
070300000000     C                   MOVEL     '24'          FUNKEY
070400000000     C                   END
070500000000     C     KEY           IFEQ      ROLLUP
070600000000     C                   MOVEL     'RU'          FUNKEY
070700000000     C                   END
070800000000     C     KEY           IFEQ      HELP
070900000000     C                   MOVEL     'HP'          FUNKEY
071000000000     C                   END
071100000000     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300000000     C* NOTE PER COMPILAZIONE SCHIERE
071400000000     C*------------------------------------------------------------------------*
071500000000     C* -MSG-
071600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
071700000000     C* (NON TOCCARE MSG,1)
071800000000     C*
071900000000     C* -FUD-
072000000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
072100000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
072200000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072300000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
072400000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072500000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
072600000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
072700000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
072800000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
072900000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
073000000000     C*
073100000000     C*------------------------------------------------------------------------*
073200000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
073300010904TISIU1R- Tasto funzionale non gestito.                                         1
073400010904TISIU1R-                                                                       2
073500010904TISIU1R- Data non accettabile.                                                 3
073600010904TISIU1R-                                                                       4
073700010904TISIU1R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
073800010904TISIU1R-                                                                       6
073900110511TISIU1R- Versione Cappario BRT inesistente.                                    7
074000010904TISIU1R- Data obbligatoria.                                                    8
074100110511TISIU1R- Data di riferimento non coperta da versione cappario BRT              9
074200010904TISIU1R- Valore immesso inconsistente.                                        10
074300070511TISIU1R- Aggiornamento non consentito per lo Step 1                           11
074400070511TISIU1R- Aggiornamento consentito solamente a utenti EDP                      12
074500010904TISIU1R-                                                                      13
074600010904TISIU1R-                                                                      14
074700010904TISIU1R-                                                                      15
074800010904TISIU1R- Data non valida.                                                     16
074900010904TISIU1R-                                                                      17
075000010904TISIU1R-                                                                      18
075100010904TISIU1R-                                                                      19
075200010904TISIU1R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
075300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
075400000000                   -        (S/N)=N                        funz.01¬
075500000000                   -        (S/N)=N                        funz.02¬
075600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
075700000000                   -        (S/N)=N                        funz.04¬
075800000000                   -        (S/N)=N                        funz.05¬
075900011120F06=Lancio Step1   -        (S/N)=S                        funz.06¬
076000011120F07=Lancio Step2   -        (S/N)=S                        funz.07¬
076100000000                   -        (S/N)=N                        funz.08¬
076200000000                   -        (S/N)=N                        funz.09¬
076300000000                   -        (S/N)=N                        funz.10¬
076400000000                   -        (S/N)=N                        funz.11¬
076500000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
076600000000                   -        (S/N)=N                        funz.13¬
076700000000                   -        (S/N)=N                        funz.14¬
076800000000                   -        (S/N)=N                        funz.15¬
076900000000                   -        (S/N)=N                        funz.16¬
077000000000                   -        (S/N)=N                        funz.17¬
077100000000                   -        (S/N)=N                        funz.18¬
077200000000                   -        (S/N)=N                        funz.19¬
077300000000                   -        (S/N)=N                        funz.20¬
077400000000                   -        (S/N)=N                        funz.21¬
077500000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
077600000000                   -        (S/N)=N                        funz.23¬
077700000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
