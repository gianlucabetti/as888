000100970526     H*------------------------------------------------------------------------*
000200150914     H* Lancio Lista Tariffe per cliente/data
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150914     FUBLSTCTRD1cf   e             WORKSTN INFDS(DEVDS1)
000900150914     F                                     SFILE(LSTCTRS01:SFLULT)
001000150914     F                                     SFILE(LSTCTRDMS:MSGULT)
001100150914     D*----------Li------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D* MESSAGGI
001500970526     D*------------
001600000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* DESCRIZIONI TASTI FUNZIONALI
001900970526     D*------------
002000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* FUNZIONI DA CARICARE A VIDEO
002300970526     D*------------
002400970526     D FUV             S             19    DIM(24)
002500970526     D*------------------------------------------------------------------------*
002600970526     D* INPUT
002700970526     D*------------------------------------------------------------------------*
002800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002900970526     D*------------
003000970526     D FUDDS           DS
003100970526     D  FUDDE1                 1      1
003200970526     D  FUDFUN                 2      3
003300970526     D  FUDDE2                 4     19
003400970526     D  FUDFI0                20     34
003500970526     D  FUDATT                35     35
003600970526     D  FUDFI1                36     80
003700970526     D  FUDDES                 1     19
003800970526     D  FUDRIG                 1     80
003900970526     D*------------
004000970526     D* DS GESTIONE TASTI FUNZIONALI
004100970526     D*------------
004200970526     D DSKEY         E DS
004300970526     D DEVDS1          DS
004400970526     D  KEY                  369    369
004500970526     D*------------
004600970526     D* DS "XMSG" - GESTIONE MESSAGGI
004700970526     D*------------
004800970526     D DSMSG         E DS
004900970528     D*------------------
005000970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100970528     D*------------------
005200970528     D WLBDA8          DS                  INZ
005300970528     D  G08DAT                 1      8  0
005400970528     D  G08INV                 9     16  0
005500970528     D  G08ERR                17     17
005600970528     D  G08TGI                18     22  0
005700970526     D*------------
005800970526     D* ARCHITETTURA
005900970526     D*------------
006000970526     D KPJBA         E DS
006100020923     D*-------------------
006200020923     D* DS REPERIMENTO DATI UTENTE
006300020923     D*-------------------
006400020923     D TIBS34DS      E DS                                                       *Profili utente
006500020923     D DDATIUTE      E DS                                                       *Dati utente
006600020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700120927     D* DS REPERIMENTO PADRE/FIGLI
006800120927     D TIBS10DS      E DS
006900150914     D* DS PER FNLV59R - CARICAMENTO E CONTROLLO TARIFFE DI 1 CLIENTE
007000150914     D fnlv59ds      E DS
007100120928     D*-------------------
007200120928
007300121003     d Status         sds
007400121003     d  SDSpgm           *proc
007500121003     d  SDSprm           *parms
007600121003     d  SDSuser              254    263
007700150121
007800150121     D* campi appoggio SFL
007900150121     D SFLULT          S                   like(SFLNBR)
008000130116
008100130116     d ora             s               t
008200140117     d wX              s              2s 0
008300150121     d wI              s              3s 0
008400120928
008500970526     C*------------------------------------------------------------------------*
008600000303     C* MAIN LINES
008700970526     C*------------------------------------------------------------------------*
008800970526     C*
008900150121     C* metto i default su C01
009000150121     C                   EXSR      InzC01
009100120927     C*
009200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009600970526     C*
009700970526     C* EMETTE FORMATO VIDEO
009800970526     C                   EXSR      BUTFMR
009900970526     C*
010000970526     C* GESTIONE TASTI FUNZIONALI
010100970526     C                   EXSR      FUNCON
010200150121     C                   ENDDO
010300970526     C*
010400970526     C* FINE PGM
010500970526     C                   SETON                                        LR
010600970526     C*------------------------------------------------------------------------*
010700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010800970526     C*------------------------------------------------------------------------*
010900970526     C     BUTFMR        BEGSR
011000970526     C*
011100970526     C* EMETTE LA TESTATA
011200150914     C                   WRITE     LSTCTRD01
011300970526     C*
011400970526     C* SPEDISCE EVENTUALE MESSAGGIO
011500970526     C                   EXSR      SNDMSG
011600970526     C*
011700970526     C* EMETTE LE FUNZIONI
011800150914     C                   WRITE     LSTCTRD02
011900970526     C*
012000150122     C* EMETTE IL SFLCTL
012100150914     C                   EXFMT     LSTCTRC01
012200970526     C*
012300991214     C* REIMPOSTA FLAG DI ERRORE
012400991214     C                   MOVEL     '0'           ERRL00
012500970526     C*
012600970526     C* REIMPOSTA INDICATORI DI ERRORE
012700020611     C                   SETOFF                                       555556
012800970526     C*
012900970526     C                   ENDSR
013000970526     C*------------------------------------------------------------------------*
013100150121     C* InzC01 - Inizializzo C01
013200970526     C*------------------------------------------------------------------------*
013300150121     C     InzC01        BEGSR
013400120927     C*
013500150122     C*   tolgo il SFL (N30) e lo pulisco (N31)
013600150122     C                   SETOFF                                         3031
013700150914     C                   WRITE     LSTCTRC01
013800150122     C*   lascio tolto il SFL ma riattivo il SFLCTL (31)
013900150122     C                   SETON                                          31
014000130108     C*
014100120927     C                   ENDSR
014200120927     C*------------------------------------------------------------------------*
014300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014400120927     C*------------------------------------------------------------------------*
014500120927     C     SNDMSG        BEGSR
014600120927     C*
014700970526     C* PULISCE IL SFILE MESSAGGI
014800970526     C                   MOVEL     '1'           *IN40
014900150914     C                   WRITE     LSTCTRDMC
015000970526     C                   MOVEL     '0'           *IN40
015100970526     C*
015200970526     C* VISUALIZZA IL MESSAGGIO
015300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015400970526     C                   CALL      'XMSG'        XMSGPA
015500020611     C     DSMSER        IFEQ      *ZEROS
015600970526     C                   MOVEL     DSMSKE        MSGKEY
015700020611     C                   ENDIF
015800970526     C                   Z-ADD     1             MSGULT
015900150914     C                   WRITE     LSTCTRDMS
016000150914     C                   WRITE     LSTCTRDMC
016100970526     C*
016200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016300970526     C                   MOVEL     *BLANKS       DSMSMS
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* ENTGES - GESTIONE ENTER -> INVIO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     ENTGES        BEGSR
017000970526     C*
017100970526     C* CONTROLLA I CAMPI IMMESSI
017200970526     C                   EXSR      DECODI
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* F03GES - GESTIONE F03 -> FINE
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F03GES        BEGSR
017900970526     C*
018000970526     C* IMPOSTA "PREMUTO F03"
018100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500991214     C* F06GES - GESTIONE F06 -> CONFERMA
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F06GES        BEGSR
018800970526     C*
018900991214     C* CONTROLLA I CAMPI IMMESSI
019000991214     C                   EXSR      DECODI
019100020611     C*
019200991214     C* ESEGUE ROUTINE DI CONFERMA
019300020611     C     ERRL00        IFEQ      '0'
019400991214     C                   EXSR      ESEGUI
019500150121     C* il messaggio di elaborazione conclusa lo do sempre, a prescindere dagli esiti
019600150915     C***                MOVEL     MSG(20)       DSMSMS
019700121109     C* non esco mai dal pgm
019800121109     C***                MOVEL     '1'           FINL00
019900121108     C                   ENDIF
020000970526     C*
020100991214     C                   ENDSR
020200130109     C*------------------------------------------------------------------------*
020300130109     C* F12GES - GESTIONE F12 -> RITORNO
020400130109     C*------------------------------------------------------------------------*
020500130109     C     F12GES        BEGSR
020600130109     C*
020700130109     C* IMPOSTA "PREMUTO F12"
020800130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020900130109     C*
021000130109     C                   ENDSR
021100970526     C*------------------------------------------------------------------------*
021200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021300970526     C*------------------------------------------------------------------------*
021400970526     C     F24GES        BEGSR
021500970526     C*
021600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022100970526     C*------------------------------------------------------------------------*
022200991214     C     ESEGUI        BEGSR
022300140120     C*
022400150121     C* valorizzo i dati per chiamare il pgm da testare
022500150915     C                   EVAL      ILV59TLA     = 'L'                           chiudo chiamato LR
022600150914     C                   EVAL      ILV59CFO     = C01CFO
022700150914     C                   EVAL      ILV59KCI     = 151
022800150914     C                   EVAL      ILV59KSC     = C01KSC
022900150914     C                   EVAL      ILV59DSP     = C01DTR
023000150914     C                   EVAL      ILV59FIE     = C01FIE
023100150914     C                   EVAL      ILV59TA2     = C01TA2
023200150914     C                   EVAL      ILV59TBO     = C01TBO
023300150914     C                   EVAL      ILV59BLO     = C01BLO
023400150914     C                   EVAL      ILV59VAL     = C01VAL
023500150914     C                   EVAL      ILV59TSP     = C01TSP
023600150914     C                   EVAL      ILV59PRP     = C01PRP
023700150915     C                   EVAL      ILV59CTR     = C01CTR
023800150915     C                   EVAL      ILV59RIC     = C01RIC
023900150915     C                   EVAL      ILV59CON     = C01CON
024000121108     C* chiamo
024100150914     C                   CALL      'FNLV59R'
024200150914     C                   PARM                    fnlv59ds
024300141113
024400150914     C* se nessun errore
024500150914     C                   IF        OLV59Err = *blank
024600150914     C* carico il SFL con i dati ricevuti
024700150121     C                   EXSR      CarSFL
024800150915     C* messaggio di elaborazione conclusa
024900150915     C                   MOVEL     MSG(20)       DSMSMS
025000150915     C                   ELSE
025100150915     C                   MOVEL     OLV59MSG      DSMSMS
025200150915     C                   ENDIF
025300150121
025400970526     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025700970526     C*------------------------------------------------------------------------*
025800970526     C     DECODI        BEGSR
025900121019     C*
026000120927     C                   SETOFF                                       505152
026100121001     C*
026200150914     C* Cliente obbligatorio
026300150127     C     ERRL00        IFEQ      '0'
026400150914     C                   IF        C01KSC = *zero
026500150127     C                   MOVEL     '1'           ERRL00
026600150914     C                   SETON                                        50
026700150127     C                   MOVEL     MSG(3)        DSMSMS
026800150127     C                   ENDIF
026900150127     C                   ENDIF
027000150914     C*
027100150914     C* Riferimento obbligatorio
027200150914     C     ERRL00        IFEQ      '0'
027300150914     C                   IF        C01DTR = 0
027400150914     C                   MOVEL     '1'           ERRL00
027500150914     C                   SETON                                        52
027600150914     C                   MOVEL     MSG(3)        DSMSMS
027700150914     C                   ENDIF
027800150914     C                   ENDIF
027900120927     C*
028000970528     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
028300970526     C*------------------------------------------------------------------------*
028400970526     C     AZZVID        BEGSR
028500970526     C* CAMPI
028600970526     C*
028700970526     C                   ENDSR
028800150121     C*------------------------------------------------------------------------*
028900150121     C* CARSFL - CARICAMENTO SFL
029000150121     C*------------------------------------------------------------------------*
029100150121     C     CARSFL        BEGSR
029200150121     C*
029300150121     C* pulizia SFL
029400150121     C                   SETOFF                                         3031
029500150914     C                   WRITE     LSTCTRC01
029600150121     C                   SETON                                          31
029700150122     C                   EVAL      SFLULT = 0
029800150122     C                   SETOFF                                       33
029900150121     C*
030000150914     C* CICLO per caricare tutte le righe che mi sono arrivate (al max sono 15)
030100150914     C                   FOR       wI = 1 to 15
030200150914     C*
030300150914     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
030400150914     C                   ADD       1             SFLULT
030500150121     C*
030600150122     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' pOutTotFound > 0
030700150914     C                   EVAL      S01KSC = %editc(C01KSC:'X')
030800150915     C* se ho impostato una tariffa e chiesto il controllo, i dati non sono in schiera
030900150915     C                   IF        C01CTR <> *blank
031000150915     C                             and C01CON = 'S'
031100150915     C                   EVAL      S01CTR = OLV59CTR
031200150915     C                   EVAL      S01PRG = %char(OLV59PRG)
031300150915     C                   EVAL      S01DES = OLV59DFS
031400150915     C                   ELSE
031500150914     C                   EVAL      S01CTR = %subst(OLV59CTT:SFLULT*3-2:3)
031600150914     C* se la tariffa attuale è vuota, vuol dire che ho finito
031700150914     C                   IF        S01CTR = *blank
031800150914     C                   LEAVE
031900150914     C                   ENDIF
032000150914     C                   EVAL      S01PRG = %subst(OLV59PGT:SFLULT*3-2:3)
032100150914     C*                  EVAL      S01DES =
032200150914     C*                  EVAL      S01DDT =
032300150914     C*                  EVAL      S01DST =
032400150915     C                   ENDIF
032500150121     C*
032600150121     C* SCRITTURA RECORD SFL
032700150914     C                   WRITE     LSTCTRS01
032800150122     C* attivo SFL (30)
032900150122     C                   SETON                                        30
033000150122     C                   EVAL      SFLNBR = 1
033100150915     C*
033200150915     C                   IF        C01CTR <> *blank
033300150915     C                             and C01CON = 'S'
033400150915     C* il rcd del sfl è 1 solo
033500150915     C                   LEAVE
033600150915     C                   ENDIF
033700150121
033800150121     C                   ENDFOR
033900150122     C*
034000150914     C* se ho meno o uguale a 14 righe sono rimasto nella prima pagina
034100150914     C                   IF        SflUlt < 14
034200150122     C                   SETON                                        33
034300150122     C                   ENDIF
034400150121     C*
034500150121     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     IMPFUN        BEGSR
035000970526     C*
035100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
035200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
035300970526     C                   EXSR      FUNGES
035400970526     C*
035500970526     C                   ENDSR
035600020923     C*--------------------------------------------------------------------------------------------*
035700020923     C* REPERISCE I DATI UTENTE
035800020923     C*--------------------------------------------------------------------------------------------*
035900020923     C     REPDATIUTE    BEGSR
036000020923     C*
036100020923     C* INIZIALIZZA VARIABILI DI WRK
036200020923     C                   CLEAR                   TIBS34DS
036300020923     C                   CLEAR                   AZUTEDS
036400020923     C                   CLEAR                   DDATIUTE
036500020923     C*
036600020923     C     *DTAARA       DEFINE    §azute        azuteds
036700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
036800020923     C                   IN(E)     *DTAARA
036900020923if  1C                   IF        %Error
037000020923     c                   EVAL      I34Tla = 'L'
037100020923     C                   CALL      'TIBS34R'
037200020923     C                   PARM                    Tibs34Ds
037300020923     C                   IN        *DTAARA
037400020923e   1C                   ENDIF
037500020923     C*
037600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
037800020923     C*
037900020923     C                   ENDSR
038000970526     C*------------------------------------------------------------------------*
038100970526     C* *INZSR - OPERAZIONI INIZIALI
038200970526     C*------------------------------------------------------------------------*
038300970526     C     *INZSR        BEGSR
038400991217     C*
038500991217     C     *ENTRY        PLIST
038600991217     C                   PARM                    KPJBA
038700020923     C*
038800020923     C* REPERISCE I DATI DELL'UTENTE
038900020923     C                   EXSR      REPDATIUTE
039000970526     C*
039100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
039200970526     C     XMSGPA        PLIST
039300970526     C                   PARM                    DSMSMS
039400970526     C                   PARM                    DSMSPG
039500970526     C                   PARM                    DSMSKE
039600970526     C                   PARM                    DSMSER
039700970526     C*
039800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
039900970526     C                   CALL      'XKEY'
040000970526     C                   PARM                    DSKEY
040100970526     C*---
040200970526     C* VARIABILI NON RIFERITE AL DATA BASE
040300970526     C*---
040400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
040500991214     C                   MOVEL     *BLANKS       DEPOP1            3
040600970526     C*---
040700970526     C* VARIABILI DI CONTROLLO
040800970526     C*---
040900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
041000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
041100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
041200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
041300970526     C*
041400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
041500150914     C                   MOVEL     'UBLSTCTRR1'  DSMSPG
041600150914     C                   MOVEL     'UBLSTCTRR1'  MSGQUE
041700970526     C                   MOVEL     *BLANKS       MSGKEY
041800970526     C                   MOVEL     *BLANKS       DSMSMS
041900970528     C*
042000970528     C* CALCOLA LA DATA CORRENTE
042100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
042200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
042300970528     C                   Z-ADD     WN8           G08DAT
042400970528     C                   Z-ADD     *ZEROS        G08INV
042500970528     C                   MOVEL     '0'           G08ERR
042600970528     C                   CALL      'XSRDA8'
042700970528     C                   PARM                    WLBDA8
042800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
042900000310     C*
043000000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
043100000310     C                   EXSR      AZZVID
043200970526     C*
043300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
043400970526     C                   EXSR      IMPFUN
043500970526     C*
043600970526     C                   ENDSR
043700970526     C*------------------------------------------------------------------------*
043800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
043900970526     C*------------------------------------------------------------------------*
044000970526     C     FUNGES        BEGSR
044100970526     C*
044200970526     C* PULISCO CAMPI DI LAVORO
044300970526     C                   Z-ADD     0             W                 5 0
044400970526     C*
044500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
044700970526     C     1             DO        24            I
044800970526     C                   MOVEL     FUD(I)        FUDRIG
044900970526     C*
045000970526     C* PULIZIA FUNZIONI PRECEDENTI
045100970526     C                   MOVEL     *BLANKS       FUV(I)
045200970526     C*
045300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
045400970526     C* (SALTA F24)
045500970526     C     FUDATT        IFEQ      'S'
045600970526     C     FUDFUN        ANDNE     '24'
045700970526     C                   ADD       1             W
045800970526     C                   MOVEL     FUDDES        FUV(W)
045900970526     C                   END
046000970526     C                   END                                                    DO WHILE
046100970526     C*
046200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
046300970526     C* ALTRIMENTI DISATTIVO F24
046400970526     C     1             DO        24            I
046500970526     C                   MOVEL     FUD(I)        FUDRIG
046600970526     C*
046700970526     C     FUDFUN        IFEQ      '24'
046800970526     C     W             IFGT      6
046900970526     C                   MOVEL     'S'           FUDATT
047000970526     C                   ELSE
047100970526     C                   MOVEL     *BLANKS       FUDATT
047200970526     C                   END                                                    W GT 8
047300970526     C                   MOVEL     FUDRIG        FUD(I)
047400970526     C                   END
047500970526     C                   END                                                    DO WHILE
047600970526     C*
047700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
047800970526     C     W             DIV       6             I
047900970526     C                   MVR                     Z
048000970526     C*
048100970526     C     Z             IFNE      0
048200970526     C     I             ADD       1             Z
048300970526     C                   ELSE
048400970526     C                   Z-ADD     I             Z                 5 0
048500970526     C                   END
048600970526     C*
048700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
048800970526     C                   ADD       1             FUNGRU
048900970526     C     FUNGRU        IFGT      Z
049000970526     C     FUNGRU        ORLT      1
049100970526     C                   Z-ADD     1             FUNGRU
049200970526     C                   END
049300970526     C*
049400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
049500970526     C     FUNGRU        IFEQ      1
049600970526     C                   MOVEL     FUV(1)        FUN001
049700970526     C                   MOVEL     FUV(2)        FUN002
049800970526     C                   MOVEL     FUV(3)        FUN003
049900970526     C                   MOVEL     FUV(4)        FUN004
050000970526     C                   MOVEL     FUV(5)        FUN005
050100970526     C                   MOVEL     FUV(6)        FUN006
050200970526     C                   END
050300970526     C*
050400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
050500970526     C     FUNGRU        IFEQ      2
050600970526     C                   MOVEL     FUV(7)        FUN001
050700970526     C                   MOVEL     FUV(8)        FUN002
050800970526     C                   MOVEL     FUV(9)        FUN003
050900970526     C                   MOVEL     FUV(10)       FUN004
051000970526     C                   MOVEL     FUV(11)       FUN005
051100970526     C                   MOVEL     FUV(12)       FUN006
051200970526     C                   END
051300970526     C*
051400970526     C* TERZO GRUPPO DI 6 FUNZIONI
051500970526     C     FUNGRU        IFEQ      3
051600970526     C                   MOVEL     FUV(13)       FUN001
051700970526     C                   MOVEL     FUV(14)       FUN002
051800970526     C                   MOVEL     FUV(15)       FUN003
051900970526     C                   MOVEL     FUV(16)       FUN004
052000970526     C                   MOVEL     FUV(17)       FUN005
052100970526     C                   MOVEL     FUV(18)       FUN006
052200970526     C                   END
052300970526     C*
052400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
052500970526     C     FUNGRU        IFEQ      4
052600970526     C                   MOVEL     FUV(19)       FUN001
052700970526     C                   MOVEL     FUV(20)       FUN002
052800970526     C                   MOVEL     FUV(21)       FUN003
052900970526     C                   MOVEL     FUV(22)       FUN004
053000970526     C                   MOVEL     *BLANKS       FUN005
053100970526     C                   MOVEL     *BLANKS       FUN006
053200970526     C                   END
053300970526     C*
053400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
053500970526     C                   MOVEL     *BLANKS       FUN007
053600970526     C                   MOVEL     *BLANKS       FUN008
053700970526     C*
053800970526     C     1             DO        24            I
053900970526     C                   MOVEL     FUD(I)        FUDRIG
054000970526     C     FUDFUN        IFEQ      '24'
054100970526     C     FUDATT        ANDEQ     'S'
054200970526     C                   MOVEL     FUDDES        FUN008
054300970526     C                   END
054400970526     C                   END                                                    DO
054500970526     C*
054600970526     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
054900970526     C*------------------------------------------------------------------------*
055000970526     C     FUNCON        BEGSR
055100970526     C*
055200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
055300970526     C                   EXSR      FUNVAR
055400970526     C*
055500970526     C* PULISCO CAMPI DI LAVORO
055600970526     C                   Z-ADD     0             I                 5 0
055700970526     C                   Z-ADD     0             W                 5 0
055800970526     C*
055900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
056000970526     C     FUNKEY        IFNE      *BLANKS
056100970526     C     1             DO        24            I
056200970526     C                   MOVEL     FUD(I)        FUDRIG
056300970526     C*
056400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
056500970526     C     FUNKEY        IFEQ      FUDFUN
056600970526     C     FUDATT        ANDEQ     'S'
056700970526     C                   Z-ADD     I             W
056800970526     C                   END
056900970526     C                   END                                                    DO WHILE
057000970526     C*
057100970526     C* FUNZIONE NON ATTIVA (ERRORE)
057200970526     C     W             IFEQ      0
057300970526     C*
057400970526     C* EMISSIONE MESSAGGIO DI ERRORE
057500970526     C                   MOVEL     MSG(1)        DSMSMS
057600970526     C*
057700970526     C                   END                                                    W=0
057800970526     C                   END                                                    FUNKEY NE *BLANKS
057900970526     C*
058000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
058100970526     C     W             IFNE      0
058200970526     C     FUNKEY        OREQ      *BLANKS
058300970526     C     FUNKEY        IFEQ      *BLANKS
058400970526     C                   EXSR      ENTGES
058500970526     C                   END
058600970526     C     FUNKEY        IFEQ      '03'
058700970526     C                   EXSR      F03GES
058800970526     C                   END
058900970526     C     FUNKEY        IFEQ      '06'
059000970526     C                   EXSR      F06GES
059100970526     C                   END
059200970526     C     FUNKEY        IFEQ      '12'
059300970526     C                   EXSR      F12GES
059400970526     C                   END
059500970526     C     FUNKEY        IFEQ      '24'
059600970526     C                   EXSR      F24GES
059700970526     C                   END
059800970526     C                   END
059900970526     C*
060000970526     C                   ENDSR
060100970526     C*------------------------------------------------------------------------*
060200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060300970526     C*------------------------------------------------------------------------*
060400970526     C     FUNVAR        BEGSR
060500970526     C*
060600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
060700970526     C*
060800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
060900970526     C     KEY           IFEQ      ENTER
061000970526     C                   MOVEL     *BLANKS       FUNKEY
061100970526     C                   END
061200970526     C     KEY           IFEQ      F1
061300970526     C                   MOVEL     '01'          FUNKEY
061400970526     C                   END
061500970526     C     KEY           IFEQ      F2
061600970526     C                   MOVEL     '02'          FUNKEY
061700970526     C                   END
061800970526     C     KEY           IFEQ      F3
061900970526     C                   MOVEL     '03'          FUNKEY
062000970526     C                   END
062100970526     C     KEY           IFEQ      F4
062200970526     C                   MOVEL     '04'          FUNKEY
062300970526     C                   END
062400970526     C     KEY           IFEQ      F5
062500970526     C                   MOVEL     '05'          FUNKEY
062600970526     C                   END
062700970526     C     KEY           IFEQ      F6
062800970526     C                   MOVEL     '06'          FUNKEY
062900970526     C                   END
063000970526     C     KEY           IFEQ      F7
063100970526     C                   MOVEL     '07'          FUNKEY
063200970526     C                   END
063300970526     C     KEY           IFEQ      F8
063400970526     C                   MOVEL     '08'          FUNKEY
063500970526     C                   END
063600970526     C     KEY           IFEQ      F9
063700970526     C                   MOVEL     '09'          FUNKEY
063800970526     C                   END
063900970526     C     KEY           IFEQ      F10
064000970526     C                   MOVEL     '10'          FUNKEY
064100970526     C                   END
064200970526     C     KEY           IFEQ      F11
064300970526     C                   MOVEL     '11'          FUNKEY
064400970526     C                   END
064500970526     C     KEY           IFEQ      F12
064600970526     C                   MOVEL     '12'          FUNKEY
064700970526     C                   END
064800970526     C     KEY           IFEQ      F13
064900970526     C                   MOVEL     '13'          FUNKEY
065000970526     C                   END
065100970526     C     KEY           IFEQ      F14
065200970526     C                   MOVEL     '14'          FUNKEY
065300970526     C                   END
065400970526     C     KEY           IFEQ      F15
065500970526     C                   MOVEL     '15'          FUNKEY
065600970526     C                   END
065700970526     C     KEY           IFEQ      F16
065800970526     C                   MOVEL     '16'          FUNKEY
065900970526     C                   END
066000970526     C     KEY           IFEQ      F17
066100970526     C                   MOVEL     '17'          FUNKEY
066200970526     C                   END
066300970526     C     KEY           IFEQ      F18
066400970526     C                   MOVEL     '18'          FUNKEY
066500970526     C                   END
066600970526     C     KEY           IFEQ      F19
066700970526     C                   MOVEL     '19'          FUNKEY
066800970526     C                   END
066900970526     C     KEY           IFEQ      F20
067000970526     C                   MOVEL     '20'          FUNKEY
067100970526     C                   END
067200970526     C     KEY           IFEQ      F21
067300970526     C                   MOVEL     '21'          FUNKEY
067400970526     C                   END
067500970526     C     KEY           IFEQ      F22
067600970526     C                   MOVEL     '22'          FUNKEY
067700970526     C                   END
067800970526     C     KEY           IFEQ      F23
067900970526     C                   MOVEL     '23'          FUNKEY
068000970526     C                   END
068100970526     C     KEY           IFEQ      F24
068200970526     C                   MOVEL     '24'          FUNKEY
068300970526     C                   END
068400970526     C     KEY           IFEQ      ROLLUP
068500970526     C                   MOVEL     'RU'          FUNKEY
068600970526     C                   END
068700970526     C     KEY           IFEQ      HELP
068800970526     C                   MOVEL     'HP'          FUNKEY
068900970526     C                   END
069000970526     C                   ENDSR
069100970526     C*------------------------------------------------------------------------*
069200970526     C* NOTE COSTRUZIONE SCHIERE
069300970526     C*------------------------------------------------------------------------*
069400970526     C*
069500970526     C* -MSG-MESSAGGI
069600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069700970526     C* (NON TOCCARE MSG,1 E MSG,2)
069800970526     C*
069900970526     C* -CMD-COMANDI
070000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070100970526     C*
070200970526     C* -OPD-OPZIONI
070300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
070400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
070500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
070600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
071300970526     C*
071400970526     C* -FUD-FUNZIONI
071500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
071600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
072100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
072300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
072400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
072500970526** MSG - MESSAGGI
072600120927Tasto funzionale non gestito                                                   1
072700150121                                                                               2
072800150127Provincia e località obbligatori                                               3
072900140819                                                                               4
073000130108                                                                               5
073100150121                                                                               6
073200130108                                                                               7
073300130108                                                                               8
073400120927                                                                               9
073500120927                                                                               10
073600120927                                                                               11
073700120927                                                                               12
073800120927                                                                               13
073900120927Data errata                                                                    14
074000140116                                                                               15
074100120927                                                                               16
074200120927                                                                               17
074300120927                                                                               18
074400120927                                                                               19
074500150121Test concluso - vedi SFL                                                       20
074600150121                                                                               21
074700150121                                                                               22
074800121108                                                                               23
074900121108                                                                               24
075000970526** FUD - FUNZIONI
075100970526                   -        (S/N)=N                        funz.01¬
075200970526                   -        (S/N)=N                        funz.02¬
075300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
075400970526                   -        (S/N)=N                        funz.04¬
075500970526                   -        (S/N)=N                        funz.05¬
075600991214F06=Conferma       -        (S/N)=S                        funz.06¬
075700140819                   -        (S/N)=N                        funz.07¬
075800970527                   -        (S/N)=N                        funz.08¬
075900970527                   -        (S/N)=N                        funz.09¬
076000970527                   -        (S/N)=N                        funz.10¬
076100970526                   -        (S/N)=N                        funz.11¬
076200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
076300970526                   -        (S/N)=N                        funz.13¬
076400970526                   -        (S/N)=N                        funz.14¬
076500970526                   -        (S/N)=N                        funz.15¬
076600970526                   -        (S/N)=N                        funz.16¬
076700970526                   -        (S/N)=N                        funz.17¬
076800970526                   -        (S/N)=N                        funz.18¬
076900970526                   -        (S/N)=N                        funz.19¬
077000970526                   -        (S/N)=N                        funz.20¬
077100970526                   -        (S/N)=N                        funz.21¬
077200970526                   -        (S/N)=N                        funz.22¬
077300970526                   -        (S/N)=N                        funz.23¬
077400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
