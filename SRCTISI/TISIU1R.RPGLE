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
024600140616     C*------------------------------------------------------------------------*
024700140616     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - CONFERMA FORZATURA ORARI DA LOC A CAP
024800140616     C*------------------------------------------------------------------------*
024900140616     C     F08GES        BEGSR
025000140616     C*
025100140616     C                   EXSR      DECODI
025200140616     C* Vedo se a video è tutto OK altrimenti la conferma non è valida
025300140616     C     ERRL00        IFEQ      '0'
025400140616     C*
025500140616     C* Se utente nn EDP => nn consento aggiornamento
025600140616     C                   IF        %subst(KNMUS:1:3)<>'EDP' and VIDSIM='N'
025700140616     C                   MOVEL     '1'           ERRL00
025800140616     C                   MOVEL     MSG(12)       DSMSMS
025900140616     C                   SETON                                        73
026000140616     C                   ELSE
026100140616     C*
026200140616     C* Imposto i parametri di passaggio
026300140616     C                   MOVE      VIDVER        PARVER
026400140616     C                   EVAL      PARCAP = VIDCAP
026500140616     C                   EVAL      PARSIM = VIDSIM
026600140616     C*
026700140616     C* Lancio il programma di elaborazione
026800140616     C                   MOVEL     'SIU4'        KCOAZ
026900140616     C                   MOVEL(p)  PARAM         KPJBU
027000140616 MOM C***                CALL      'BCH10'
027100140616     C                   CALL      'TISIU4R'
027200140616     C                   PARM                    KPJBA
027300140616     C*
027400140616     C* Imposto il flag F03 di livello PGM per l'uscita automatica a fine elaborazione
027500140616     C                   MOVEL     '1'           F03L00
027600140616     C*
027700140616     C                   ENDIF
027800140616     C                   ENDIF
027900140616     C*
028000140616     C                   ENDSR
028100000000     C*------------------------------------------------------------------------*
028200000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
028300000000     C*------------------------------------------------------------------------*
028400000000     C     F12GES        BEGSR
028500000000     C*
028600000000     C* IMPOSTO FLAG DI LIVELLO
028700000000     C                   MOVEL     '1'           F12L00
028800000000     C*
028900000000     C                   ENDSR
029000000000     C*------------------------------------------------------------------------*
029100991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
029200000000     C*------------------------------------------------------------------------*
029300000000     C     F24GES        BEGSR
029400000000     C*
029500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
029600000000     C                   EXSR      FUNGES
029700000000     C*
029800000000     C                   ENDSR
029900991020     C*------------------------------------------------------------------------*
030000000531     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
030100000000     C*------------------------------------------------------------------------*
030200000000     C     DECODI        BEGSR
030300000505     C*
030400010905     C* CONTROLLO DATA RIFERIMENTO
030500000505     C     ERRL00        IFEQ      '0'
030600000505     C                   MOVEL     VIHDAT        G08DAT
030700000505     C                   Z-ADD     *ZEROS        G08INV
030800000505     C                   MOVEL     '0'           G08ERR
030900000505     C                   CALL      'XSRDA8'
031000000505     C                   PARM                    WLBDA8
031100000505     C     G08ERR        IFEQ      '1'
031200000505     C                   MOVEL     '1'           ERRL00
031300000505     C                   MOVEL     MSG(3)        DSMSMS
031400000505     C                   SETON                                        74
031500000505     C                   ELSE
031600000505     C                   Z-ADD     G08INV        INVDAT            8 0
031700000505     C                   ENDIF
031800000505     C                   ENDIF
031900991117     C*
032000000505     C* REPERIMENTO VERSIONI BAR X DATA DI RIFERIMENTO
032100000505     C     ERRL00        IFEQ      '0'
032200000505     C     VIDVER        IFEQ      *blanks
032300000505     C                   EXSR      REPVEX
032400000505     C                   ENDIF
032500000505     C                   ENDIF
032600000505     C*
032700110511     C* CONTROLLO VERSIONE CAPPARIO  BRT
032800000505     C     ERRL00        IFEQ      '0'
032900000505     C                   EXSR      CHKVER
033000000505     C                   ENDIF
033100000225     C*
033200000000     C                   ENDSR
033300000505     C*------------------------------------------------------------------------*
033400110511     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE BRT
033500000505     C*------------------------------------------------------------------------*
033600000505     C     CHKVER        BEGSR
033700000505     C*
033800000505     C                   IF        %trim(VIDVER)='?'
033900000505     C*
034000000505     C* lancia il programma di gestione versioni BAR in ricerca
034100000505     C                   CLEAR                   dssi00                         *pulisce ds
034200000505     C                   MOVEL     'R01'         d00op0                         *livello procedura
034300000505     C                   MOVEL     *blanks       d00op1                         *livello programma
034400000505     C                   MOVEL     '0'           d00f03                         *premuto F03
034500000505     C                   MOVEL     '0'           d00f12                         *premuto F12
034600000505     C                   MOVEL     '0'           d00err                         *errore
034700000505     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
034800000505     C                   CALL      'TISI46R'                                    *gestione versioni
034900000505     C                   PARM                    kpjba
035000000505     C                   PARM                    dssi00
035100000505     C* ritorno
035200000505     C                   IF        d00err='1'
035300000508     C                   MOVEL     '1'           ERRL00
035400000505     C                   MOVEL     MSG(5)        DSMSMS
035500000505     C                   ELSE
035600000505     C                   MOVE(p)   d00ver        VIDVER
035700000505     C                   Z-ADD     d00ver        VIHVER
035800000505     C                   Z-ADD     d00dde        VIHDDE
035900000505     C                   Z-ADD     d00edd        VIHDSC
036000000505     C                   ENDIF
036100000505     C*
036200000505     C                   ELSE
036300010905     C                   CALL(e)   'ISNUMERIC'
036400000505     C                   PARM      VIDVER        PiStr            30
036500000505     C                   PARM      ','           PiDecChr          1
036600000505     C                   PARM      *ZEROS        PiVal            30 9
036700000505     C                   PARM      '0'           PiInt             1
036800000505     C                   PARM      '0'           PiNum             1
036900000505     C                   IF        NOT %error AND
037000000505     C                             PiInt=*ON
037100001213     C                   Z-ADD     PiVal         VIHVER
037200001213     C                   MOVE(p)   VIHVER        VIDVER
037300000505     C                   ELSE
037400001213     C                   SETOFF                                       70
037500001213     C                   MOVEL     MSG(10)       DSMSMS
037600000505     C                   Z-ADD     *ZEROS        VIHVER
037700000505     C                   ENDIF
037800000505     C*
037900000505     C     VIHVER        IFEQ      *ZEROS
038000000505     C                   MOVEL     '1'           ERRL00
038100000505     C                   ELSE
038200000505     C*
038300000505     C     VIHVER        CHAIN     AZCPR01L                           99
038400000505     C     *IN99         IFEQ      *ON
038500000505     C                   SETOFF                                       70
038600000505     C                   MOVEL     MSG(7)        DSMSMS
038700000505     C                   ELSE
038800000505     C                   Z-ADD     cprdde        VIHDDE
038900000505     C                   Z-ADD     cprdsc        VIHDSC
039000000505     C                   ENDIF
039100000505     C*
039200000505     C                   ENDIF
039300000505     C                   ENDIF
039400000505     C*
039500000505     C                   ENDSR
039600000505     C*------------------------------------------------------------------------*
039700000505     C*  REPVEX - REPERISCE LE VERSIONI DEI CAPPARI BAR/POSTE
039800000505     C*------------------------------------------------------------------------*
039900000505     C     REPVEX        BEGSR
040000000505     C*
040100110511     C* REPERISCE LA VERSIONE BRT
040200000505     C                   Z-ADD     INVDAT        IPTDRI            8 0
040300000505     C                   Z-ADD     *ZEROS        IPTVER            5 0
040400000505     C                   CALL      'TISI96R'
040500000505     C                   PARM                    IPTDRI
040600000505     C                   PARM                    IPTVER
040700000505     C                   PARM                    IPTDES            5
040800000505     C                   IF        IPTVER>*zeros
040900000505     C                   SETOFF                                       81
041000000505     C                   MOVE(p)   IPTVER        VIDVER
041100000505     C                   Z-ADD     IPTVER        VIHVER
041200000505     C                   Z-ADD     IPTVER        VIHVER
041300000505     C     IPTVER        CHAIN     azcpr01l
041400000505     C                   Z-ADD     cprdde        VIHDDE
041500000505     C                   Z-ADD     cprdsc        VIHDSC
041600000505     C                   ELSE
041700000505     C                   MOVEL     MSG(9)        DSMSMS
041800000505     C                   ENDIF
041900000505     C*
042000000505     C                   ENDSR
042100000000     C*------------------------------------------------------------------------*
042200000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
042300000000     C*------------------------------------------------------------------------*
042400000000     C     AZZVID        BEGSR
042500000000     C*
042600000505     C                   Z-ADD     WN8           VIHDAT
042700000505     C                   MOVEL     *BLANKS       VIDVER
042800000505     C                   MOVEL     *BLANKS       VIDCAP
042900000531     C                   MOVEL     'S'           VIDSIM
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300980403     C* *INZSR - OPERAZIONI INIZIALI
043400000000     C*------------------------------------------------------------------------*
043500980403     C     *INZSR        BEGSR
043600000000     C*
043700000000     C* RICEVIMENTO PARAMETRI
043800000000     C     *ENTRY        PLIST
043900000000     C                   PARM                    KPJBA
044000011119     C*
044100011119     C* CALCOLA LA DATA CORRENTE
044200011119     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
044300011119     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
044400011119     C                   Z-ADD     WN8           G08DAT
044500011119     C                   Z-ADD     *ZEROS        G08INV
044600011119     C                   MOVEL     '0'           G08ERR
044700011119     C                   CALL      'XSRDA8'
044800011119     C                   PARM                    WLBDA8
044900011119     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
045000000000     C*
045100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
045200000000     C     XMSGPA        PLIST
045300000000     C                   PARM                    DSMSMS
045400000000     C                   PARM                    DSMSPG
045500000000     C                   PARM                    DSMSKE
045600000000     C                   PARM                    DSMSER
045700000000     C*
045800000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
045900000000     C                   CALL      'XKEY'
046000000000     C                   PARM                    DSKEY
046100000000     C*
046200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
046300000225     C     *LIKE         DEFINE    FUDFUN        FUNKEY
046400000000     C*
046500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
046600000000     C*   -NOME PGM CHIAMANTE
046700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
046800010904     C                   MOVEL     'TISIU1R'     DSMSPG
046900010904     C                   MOVEL     'TISIU1R'     MSGQUE
047000000000     C                   MOVEL     *BLANKS       MSGKEY
047100000000     C                   MOVEL     *BLANKS       DSMSMS
047200000000     C*
047300000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
047400000000     C                   Z-ADD     0             FUNGRU            5 0
047500000000     C*
047600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
047700000000     C                   EXSR      FUNGES
047800000000     C*
047900000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
048000000000     C                   MOVEL     '0'           F12L00            1
048100000000     C                   MOVEL     '0'           F03L00            1
048200000000     C                   MOVEL     '0'           FINL00            1
048300000000     C                   MOVEL     '0'           ERRL00            1
048400000000     C*
048500000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
048600000000     C                   EXSR      AZZVID
048700000000     C*
048800000000     C                   ENDSR
048900000000     C*------------------------------------------------------------------------*
049000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
049100000000     C*------------------------------------------------------------------------*
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
059800000000     C*------------------------------------------------------------------------*
059900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
060000000000     C*------------------------------------------------------------------------*
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
062600000000     C                   MOVEL     MSG(1)        DSMSMS
062700000000     C*
062800000000     C                   END                                                    W=0
062900000000     C                   END                                                    FUNKEY NE *BLANKS
063000000000     C*
063100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
063200000000     C     W             IFNE      0
063300000000     C     FUNKEY        OREQ      *BLANKS
063400000000     C*
063500000000     C     FUNKEY        IFEQ      *BLANKS
063600000000     C                   EXSR      ENTGES
063700000000     C                   END
063800000000     C     FUNKEY        IFEQ      '03'
063900000000     C                   EXSR      F03GES
064000000000     C                   END
064100000000     C     FUNKEY        IFEQ      '06'
064200000000     C                   EXSR      F06GES
064300000000     C                   END
064400011120     C     FUNKEY        IFEQ      '07'
064500011120     C                   EXSR      F07GES
064600011120     C                   END
064700140616     C     FUNKEY        IFEQ      '08'
064800140616     C                   EXSR      F08GES
064900140616     C                   END
065000000000     C     FUNKEY        IFEQ      '12'
065100000000     C                   EXSR      F12GES
065200000000     C                   END
065300000000     C     FUNKEY        IFEQ      '24'
065400000000     C                   EXSR      F24GES
065500000000     C                   END
065600000000     C*
065700000000     C                   END
065800000000     C*
065900000000     C                   ENDSR
066000000000     C*------------------------------------------------------------------------*
066100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
066200000000     C*------------------------------------------------------------------------*
066300000000     C     FUNVAR        BEGSR
066400000000     C*
066500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
066600000000     C*
066700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
066800000000     C     KEY           IFEQ      ENTER
066900000000     C                   MOVEL     *BLANKS       FUNKEY
067000000000     C                   END
067100000000     C     KEY           IFEQ      F1
067200000000     C                   MOVEL     '01'          FUNKEY
067300000000     C                   END
067400000000     C     KEY           IFEQ      F2
067500000000     C                   MOVEL     '02'          FUNKEY
067600000000     C                   END
067700000000     C     KEY           IFEQ      F3
067800000000     C                   MOVEL     '03'          FUNKEY
067900000000     C                   END
068000000000     C     KEY           IFEQ      F4
068100000000     C                   MOVEL     '04'          FUNKEY
068200000000     C                   END
068300000000     C     KEY           IFEQ      F5
068400000000     C                   MOVEL     '05'          FUNKEY
068500000000     C                   END
068600000000     C     KEY           IFEQ      F6
068700000000     C                   MOVEL     '06'          FUNKEY
068800000000     C                   END
068900000000     C     KEY           IFEQ      F7
069000000000     C                   MOVEL     '07'          FUNKEY
069100000000     C                   END
069200000000     C     KEY           IFEQ      F8
069300000000     C                   MOVEL     '08'          FUNKEY
069400000000     C                   END
069500000000     C     KEY           IFEQ      F9
069600000000     C                   MOVEL     '09'          FUNKEY
069700000000     C                   END
069800000000     C     KEY           IFEQ      F10
069900000000     C                   MOVEL     '10'          FUNKEY
070000000000     C                   END
070100000000     C     KEY           IFEQ      F11
070200000000     C                   MOVEL     '11'          FUNKEY
070300000000     C                   END
070400000000     C     KEY           IFEQ      F12
070500000000     C                   MOVEL     '12'          FUNKEY
070600000000     C                   END
070700000000     C     KEY           IFEQ      F13
070800000000     C                   MOVEL     '13'          FUNKEY
070900000000     C                   END
071000000000     C     KEY           IFEQ      F14
071100000000     C                   MOVEL     '14'          FUNKEY
071200000000     C                   END
071300000000     C     KEY           IFEQ      F15
071400000000     C                   MOVEL     '15'          FUNKEY
071500000000     C                   END
071600000000     C     KEY           IFEQ      F16
071700000000     C                   MOVEL     '16'          FUNKEY
071800000000     C                   END
071900000000     C     KEY           IFEQ      F17
072000000000     C                   MOVEL     '17'          FUNKEY
072100000000     C                   END
072200000000     C     KEY           IFEQ      F18
072300000000     C                   MOVEL     '18'          FUNKEY
072400000000     C                   END
072500000000     C     KEY           IFEQ      F19
072600000000     C                   MOVEL     '19'          FUNKEY
072700000000     C                   END
072800000000     C     KEY           IFEQ      F20
072900000000     C                   MOVEL     '20'          FUNKEY
073000000000     C                   END
073100000000     C     KEY           IFEQ      F21
073200000000     C                   MOVEL     '21'          FUNKEY
073300000000     C                   END
073400000000     C     KEY           IFEQ      F22
073500000000     C                   MOVEL     '22'          FUNKEY
073600000000     C                   END
073700000000     C     KEY           IFEQ      F23
073800000000     C                   MOVEL     '23'          FUNKEY
073900000000     C                   END
074000000000     C     KEY           IFEQ      F24
074100000000     C                   MOVEL     '24'          FUNKEY
074200000000     C                   END
074300000000     C     KEY           IFEQ      ROLLUP
074400000000     C                   MOVEL     'RU'          FUNKEY
074500000000     C                   END
074600000000     C     KEY           IFEQ      HELP
074700000000     C                   MOVEL     'HP'          FUNKEY
074800000000     C                   END
074900000000     C                   ENDSR
075000000000     C*------------------------------------------------------------------------*
075100000000     C* NOTE PER COMPILAZIONE SCHIERE
075200000000     C*------------------------------------------------------------------------*
075300000000     C* -MSG-
075400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
075500000000     C* (NON TOCCARE MSG,1)
075600000000     C*
075700000000     C* -FUD-
075800000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
075900000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
076000000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
076100000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
076200000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076300000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
076400000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
076500000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
076600000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
076700000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
076800000000     C*
076900000000     C*------------------------------------------------------------------------*
077000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
077100010904TISIU1R- Tasto funzionale non gestito.                                         1
077200010904TISIU1R-                                                                       2
077300010904TISIU1R- Data non accettabile.                                                 3
077400010904TISIU1R-                                                                       4
077500010904TISIU1R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
077600010904TISIU1R-                                                                       6
077700110511TISIU1R- Versione Cappario BRT inesistente.                                    7
077800010904TISIU1R- Data obbligatoria.                                                    8
077900110511TISIU1R- Data di riferimento non coperta da versione cappario BRT              9
078000010904TISIU1R- Valore immesso inconsistente.                                        10
078100070511TISIU1R- Aggiornamento non consentito per lo Step 1                           11
078200070511TISIU1R- Aggiornamento consentito solamente a utenti EDP                      12
078300010904TISIU1R-                                                                      13
078400010904TISIU1R-                                                                      14
078500010904TISIU1R-                                                                      15
078600010904TISIU1R- Data non valida.                                                     16
078700010904TISIU1R-                                                                      17
078800010904TISIU1R-                                                                      18
078900010904TISIU1R-                                                                      19
079000010904TISIU1R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
079100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
079200000000                   -        (S/N)=N                        funz.01¬
079300000000                   -        (S/N)=N                        funz.02¬
079400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
079500000000                   -        (S/N)=N                        funz.04¬
079600000000                   -        (S/N)=N                        funz.05¬
079700011120F06=Lancio Step1   -        (S/N)=S                        funz.06¬
079800011120F07=Lancio Step2   -        (S/N)=S                        funz.07¬
079900140616F08=Lancio Step3   -        (S/N)=S                        funz.08¬
080000000000                   -        (S/N)=N                        funz.09¬
080100000000                   -        (S/N)=N                        funz.10¬
080200000000                   -        (S/N)=N                        funz.11¬
080300000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
080400000000                   -        (S/N)=N                        funz.13¬
080500000000                   -        (S/N)=N                        funz.14¬
080600000000                   -        (S/N)=N                        funz.15¬
080700000000                   -        (S/N)=N                        funz.16¬
080800000000                   -        (S/N)=N                        funz.17¬
080900000000                   -        (S/N)=N                        funz.18¬
081000000000                   -        (S/N)=N                        funz.19¬
081100000000                   -        (S/N)=N                        funz.20¬
081200000000                   -        (S/N)=N                        funz.21¬
081300000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
081400000000                   -        (S/N)=N                        funz.23¬
081500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
