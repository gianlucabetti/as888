000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800160129     FTRVR11D1  cf   e             WORKSTN INFDS(DEVDS1)
000900160129     F                                     SFILE(TRVR11DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D* MESSAGGI
001400970526     D*------------
001500000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* DESCRIZIONI TASTI FUNZIONALI
001800970526     D*------------
001900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* FUNZIONI DA CARICARE A VIDEO
002200970526     D*------------
002300970526     D FUV             S             19    DIM(24)
002400970526     D*------------------------------------------------------------------------*
002500970526     D* INPUT
002600970526     D*------------------------------------------------------------------------*
002700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002800970526     D*------------
002900970526     D FUDDS           DS
003000970526     D  FUDDE1                 1      1
003100970526     D  FUDFUN                 2      3
003200970526     D  FUDDE2                 4     19
003300970526     D  FUDFI0                20     34
003400970526     D  FUDATT                35     35
003500970526     D  FUDFI1                36     80
003600970526     D  FUDDES                 1     19
003700970526     D  FUDRIG                 1     80
003800970526     D*------------
003900970526     D* DS GESTIONE TASTI FUNZIONALI
004000970526     D*------------
004100970526     D DSKEY         E DS
004200970526     D DEVDS1          DS
004300970526     D  KEY                  369    369
004400970526     D*------------
004500970526     D* DS "XMSG" - GESTIONE MESSAGGI
004600970526     D*------------
004700970526     D DSMSG         E DS
004800970528     D*------------------
004900970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000970528     D*------------------
005100970528     D WLBDA8          DS                  INZ
005200970528     D  G08DAT                 1      8  0
005300970528     D  G08INV                 9     16  0
005400970528     D  G08ERR                17     17
005500970528     D  G08TGI                18     22  0
005600970526     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000020923     D*-------------------
006100020923     D* DS REPERIMENTO DATI UTENTE
006200020923     D*-------------------
006300020923     D TIBS34DS      E DS
006400020923     D DDATIUTE      E DS
006500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006600120927     D* DS REPERIMENTO PADRE/FIGLI
006700120927     D TIBS10DS      E DS
006800120928     D*-------------------
006900120928
007000121003     d Status         sds
007100121003     d  SDSpgm           *proc
007200121003     d  SDSprm           *parms
007300121003     d  SDSuser              254    263
007400121003
007500160129     d pOut_Esito      s             15
007600130116
007700130116     d ora             s               t
007800140117     d wX              s              2s 0
007900120928
008000970526     C*------------------------------------------------------------------------*
008100000303     C* MAIN LINES
008200970526     C*------------------------------------------------------------------------*
008300970526     C*
008400120927     C* metto i default su D03
008500120927     C                   EXSR      InzD03
008600120927     C*
008700120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008800991214     C     F03L00        DOUEQ     '1'
008900970526     C     F12L00        OREQ      '1'
009000970526     C     FINL00        OREQ      '1'
009100970526     C*
009200970526     C* EMETTE FORMATO VIDEO
009300970526     C                   EXSR      BUTFMR
009400970526     C*
009500970526     C* GESTIONE TASTI FUNZIONALI
009600970526     C                   EXSR      FUNCON
009700970526E   2C                   ENDDO
009800970526     C*
009900970526     C* FINE PGM
010000970526     C                   SETON                                        LR
010100970526     C*------------------------------------------------------------------------*
010200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010300970526     C*------------------------------------------------------------------------*
010400970526     C     BUTFMR        BEGSR
010500970526     C*
010600970526     C* EMETTE LA TESTATA
010700160129     C                   WRITE     TRVR11D01
010800970526     C*
010900970526     C* SPEDISCE EVENTUALE MESSAGGIO
011000970526     C                   EXSR      SNDMSG
011100970526     C*
011200970526     C* EMETTE LE FUNZIONI
011300160129     C                   WRITE     TRVR11D02
011400970526     C*
011500970526     C* EMETTE IL VIDEO
011600160129     C                   EXFMT     TRVR11D03
011700970526     C*
011800991214     C* REIMPOSTA FLAG DI ERRORE
011900991214     C                   MOVEL     '0'           ERRL00
012000970526     C*
012100970526     C* REIMPOSTA INDICATORI DI ERRORE
012200020611     C                   SETOFF                                       555556
012300970526     C*
012400970526     C                   ENDSR
012500970526     C*------------------------------------------------------------------------*
012600120927     C* InzD03 - Inizializzo D03
012700970526     C*------------------------------------------------------------------------*
012800120927     C     InzD03        BEGSR
012900120927     C*
013000130108     C*
013100120927     C                   ENDSR
013200120927     C*------------------------------------------------------------------------*
013300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013400120927     C*------------------------------------------------------------------------*
013500120927     C     SNDMSG        BEGSR
013600120927     C*
013700970526     C* PULISCE IL SFILE MESSAGGI
013800970526     C                   MOVEL     '1'           *IN40
013900160129     C                   WRITE     TRVR11DMC
014000970526     C                   MOVEL     '0'           *IN40
014100970526     C*
014200970526     C* VISUALIZZA IL MESSAGGIO
014300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014400970526     C                   CALL      'XMSG'        XMSGPA
014500020611     C     DSMSER        IFEQ      *ZEROS
014600970526     C                   MOVEL     DSMSKE        MSGKEY
014700020611     C                   ENDIF
014800970526     C                   Z-ADD     1             MSGULT
014900160129     C                   WRITE     TRVR11DMS
015000160129     C                   WRITE     TRVR11DMC
015100970526     C*
015200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015300970526     C                   MOVEL     *BLANKS       DSMSMS
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* ENTGES - GESTIONE ENTER -> INVIO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     ENTGES        BEGSR
016000970526     C*
016100970526     C* CONTROLLA I CAMPI IMMESSI
016200970526     C                   EXSR      DECODI
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* F03GES - GESTIONE F03 -> FINE
016700970526     C*------------------------------------------------------------------------*
016800970526     C     F03GES        BEGSR
016900970526     C*
017000970526     C* IMPOSTA "PREMUTO F03"
017100970526     C                   MOVEL     '1'           F03L00
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500991214     C* F06GES - GESTIONE F06 -> CONFERMA
017600970526     C*------------------------------------------------------------------------*
017700970526     C     F06GES        BEGSR
017800970526     C*
017900991214     C* CONTROLLA I CAMPI IMMESSI
018000991214     C                   EXSR      DECODI
018100020611     C*
018200991214     C* ESEGUE ROUTINE DI CONFERMA
018300020611     C     ERRL00        IFEQ      '0'
018400991214     C                   EXSR      ESEGUI
018500130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli e
018600121109     C                   MOVEL     MSG(20)       DSMSMS
018700121109     C* non esco mai dal pgm
018800121109     C***                MOVEL     '1'           FINL00
018900121108     C                   ENDIF
019000970526     C*
019100991214     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300130109     C* F07GES - GESTIONE F07 -> STAMPA
019400970526     C*------------------------------------------------------------------------*
019500130109     C     F07GES        BEGSR
019600970526     C*
019700140818     C                   EXSR      F06GES
019800130110     C*
019900970526     C                   ENDSR
020000130109     C*------------------------------------------------------------------------*
020100130109     C* F12GES - GESTIONE F12 -> RITORNO
020200130109     C*------------------------------------------------------------------------*
020300130109     C     F12GES        BEGSR
020400130109     C*
020500130109     C* IMPOSTA "PREMUTO F12"
020600130109     C                   MOVEL     '1'           F12L00
020700130109     C*
020800130109     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021100970526     C*------------------------------------------------------------------------*
021200970526     C     F24GES        BEGSR
021300970526     C*
021400970526     C                   EXSR      FUNGES
021500970526     C*
021600970526     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
021900970526     C*------------------------------------------------------------------------*
022000991214     C     ESEGUI        BEGSR
022100140120     C*
022200140120     C* creo il file temporaneo con elenco dei membri
022300140819     c                   clear                   pOut_Esito
022400121108     C* chiamo
022500160129     C                   CALL      'TRVR11R'
022600140819     c                   parm                    pOut_Esito
022700121108     C*
022800140109     c                   EVAL      VIDEsito = pOut_Esito
022900140120     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023300970526     C*------------------------------------------------------------------------*
023400970526     C     DECODI        BEGSR
023500121019     C*
023600120927     C                   SETOFF                                       505152
023700140120     c                   EVAL      VIDMSG    = *blank
023800121001     C*
023900120927     C*
024000970528     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
024300970526     C*------------------------------------------------------------------------*
024400970526     C     AZZVID        BEGSR
024500970526     C* CAMPI
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025000970526     C*------------------------------------------------------------------------*
025100970526     C     IMPFUN        BEGSR
025200970526     C*
025300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
025400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
025500970526     C                   EXSR      FUNGES
025600970526     C*
025700970526     C                   ENDSR
025800020923     C*-------------------------------------------------------------------------
025900020923     C* REPERISCE I DATI UTENTE
026000020923     C*-------------------------------------------------------------------------
026100020923     C     REPDATIUTE    BEGSR
026200020923     C*
026300020923     C* INIZIALIZZA VARIABILI DI WRK
026400020923     C                   CLEAR                   TIBS34DS
026500020923     C                   CLEAR                   AZUTEDS
026600020923     C                   CLEAR                   DDATIUTE
026700020923     C*
026800020923     C     *DTAARA       DEFINE    §azute        azuteds
026900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
027000020923     C                   IN(E)     *DTAARA
027100020923if  1C                   IF        %Error
027200020923     c                   EVAL      I34Tla = 'L'
027300020923     C                   CALL      'TIBS34R'
027400020923     C                   PARM                    Tibs34Ds
027500020923     C                   IN        *DTAARA
027600020923e   1C                   ENDIF
027700020923     C*
027800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
027900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
028000020923     C*
028100020923     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* *INZSR - OPERAZIONI INIZIALI
028400970526     C*------------------------------------------------------------------------*
028500970526     C     *INZSR        BEGSR
028600991217     C*
028700991217     C     *ENTRY        PLIST
028800991217     C                   PARM                    KPJBA
028900020923     C*
029000020923     C* REPERISCE I DATI DELL'UTENTE
029100020923     C                   EXSR      REPDATIUTE
029200970526     C*
029300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
029400970526     C     XMSGPA        PLIST
029500970526     C                   PARM                    DSMSMS
029600970526     C                   PARM                    DSMSPG
029700970526     C                   PARM                    DSMSKE
029800970526     C                   PARM                    DSMSER
029900970526     C*
030000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
030100970526     C                   CALL      'XKEY'
030200970526     C                   PARM                    DSKEY
030300970526     C*---
030400970526     C* VARIABILI NON RIFERITE AL DATA BASE
030500970526     C*---
030600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
030700991214     C                   MOVEL     *BLANKS       DEPOP1            3
030800970526     C*---
030900970526     C* VARIABILI DI CONTROLLO
031000970526     C*---
031100970526     C                   MOVEL     '0'           FINL00            1
031200970526     C                   MOVEL     '0'           F03L00            1
031300970526     C                   MOVEL     '0'           F12L00            1
031400970526     C                   MOVEL     '0'           ERRL00            1
031500970526     C*
031600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
031700160129     C                   MOVEL     'TRVR11R1'    DSMSPG
031800160129     C                   MOVEL     'TRVR11R1'    MSGQUE
031900970526     C                   MOVEL     *BLANKS       MSGKEY
032000970526     C                   MOVEL     *BLANKS       DSMSMS
032100970528     C*
032200970528     C* CALCOLA LA DATA CORRENTE
032300970528     C                   TIME                    WN14             14 0
032400970528     C                   MOVE      WN14          WN8               8 0
032500970528     C                   Z-ADD     WN8           G08DAT
032600970528     C                   Z-ADD     *ZEROS        G08INV
032700970528     C                   MOVEL     '0'           G08ERR
032800970528     C                   CALL      'XSRDA8'
032900970528     C                   PARM                    WLBDA8
033000970528     C                   Z-ADD     G08INV        DATCOR            8 0
033100000310     C*
033200000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
033300000310     C                   EXSR      AZZVID
033400970526     C*
033500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
033600970526     C                   EXSR      IMPFUN
033700970526     C*
033800970526     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
034100970526     C*------------------------------------------------------------------------*
034200970526     C     FUNGES        BEGSR
034300970526     C*
034400970526     C* PULISCO CAMPI DI LAVORO
034500970526     C                   Z-ADD     0             W                 5 0
034600970526     C*
034700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
034800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
034900970526     C     1             DO        24            I
035000970526     C                   MOVEL     FUD(I)        FUDRIG
035100970526     C*
035200970526     C* PULIZIA FUNZIONI PRECEDENTI
035300970526     C                   MOVEL     *BLANKS       FUV(I)
035400970526     C*
035500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
035600970526     C* (SALTA F24)
035700970526     C     FUDATT        IFEQ      'S'
035800970526     C     FUDFUN        ANDNE     '24'
035900970526     C                   ADD       1             W
036000970526     C                   MOVEL     FUDDES        FUV(W)
036100970526     C                   END
036200970526     C                   END
036300970526     C*
036400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036500970526     C* ALTRIMENTI DISATTIVO F24
036600970526     C     1             DO        24            I
036700970526     C                   MOVEL     FUD(I)        FUDRIG
036800970526     C*
036900970526     C     FUDFUN        IFEQ      '24'
037000970526     C     W             IFGT      6
037100970526     C                   MOVEL     'S'           FUDATT
037200970526     C                   ELSE
037300970526     C                   MOVEL     *BLANKS       FUDATT
037400970526     C                   END
037500970526     C                   MOVEL     FUDRIG        FUD(I)
037600970526     C                   END
037700970526     C                   END
037800970526     C*
037900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
038000970526     C     W             DIV       6             I
038100970526     C                   MVR                     Z
038200970526     C*
038300970526     C     Z             IFNE      0
038400970526     C     I             ADD       1             Z
038500970526     C                   ELSE
038600970526     C                   Z-ADD     I             Z                 5 0
038700970526     C                   END
038800970526     C*
038900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
039000970526     C                   ADD       1             FUNGRU
039100970526     C     FUNGRU        IFGT      Z
039200970526     C     FUNGRU        ORLT      1
039300970526     C                   Z-ADD     1             FUNGRU
039400970526     C                   END
039500970526     C*
039600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
039700970526     C     FUNGRU        IFEQ      1
039800970526     C                   MOVEL     FUV(1)        FUN001
039900970526     C                   MOVEL     FUV(2)        FUN002
040000970526     C                   MOVEL     FUV(3)        FUN003
040100970526     C                   MOVEL     FUV(4)        FUN004
040200970526     C                   MOVEL     FUV(5)        FUN005
040300970526     C                   MOVEL     FUV(6)        FUN006
040400970526     C                   END
040500970526     C*
040600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
040700970526     C     FUNGRU        IFEQ      2
040800970526     C                   MOVEL     FUV(7)        FUN001
040900970526     C                   MOVEL     FUV(8)        FUN002
041000970526     C                   MOVEL     FUV(9)        FUN003
041100970526     C                   MOVEL     FUV(10)       FUN004
041200970526     C                   MOVEL     FUV(11)       FUN005
041300970526     C                   MOVEL     FUV(12)       FUN006
041400970526     C                   END
041500970526     C*
041600970526     C* TERZO GRUPPO DI 6 FUNZIONI
041700970526     C     FUNGRU        IFEQ      3
041800970526     C                   MOVEL     FUV(13)       FUN001
041900970526     C                   MOVEL     FUV(14)       FUN002
042000970526     C                   MOVEL     FUV(15)       FUN003
042100970526     C                   MOVEL     FUV(16)       FUN004
042200970526     C                   MOVEL     FUV(17)       FUN005
042300970526     C                   MOVEL     FUV(18)       FUN006
042400970526     C                   END
042500970526     C*
042600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
042700970526     C     FUNGRU        IFEQ      4
042800970526     C                   MOVEL     FUV(19)       FUN001
042900970526     C                   MOVEL     FUV(20)       FUN002
043000970526     C                   MOVEL     FUV(21)       FUN003
043100970526     C                   MOVEL     FUV(22)       FUN004
043200970526     C                   MOVEL     *BLANKS       FUN005
043300970526     C                   MOVEL     *BLANKS       FUN006
043400970526     C                   END
043500970526     C*
043600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
043700970526     C                   MOVEL     *BLANKS       FUN007
043800970526     C                   MOVEL     *BLANKS       FUN008
043900970526     C*
044000970526     C     1             DO        24            I
044100970526     C                   MOVEL     FUD(I)        FUDRIG
044200970526     C     FUDFUN        IFEQ      '24'
044300970526     C     FUDATT        ANDEQ     'S'
044400970526     C                   MOVEL     FUDDES        FUN008
044500970526     C                   END
044600970526     C                   END
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
045100970526     C*------------------------------------------------------------------------*
045200970526     C     FUNCON        BEGSR
045300970526     C*
045400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
045500970526     C                   EXSR      FUNVAR
045600970526     C*
045700970526     C* PULISCO CAMPI DI LAVORO
045800970526     C                   Z-ADD     0             I                 5 0
045900970526     C                   Z-ADD     0             W                 5 0
046000970526     C*
046100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
046200970526     C     FUNKEY        IFNE      *BLANKS
046300970526     C     1             DO        24            I
046400970526     C                   MOVEL     FUD(I)        FUDRIG
046500970526     C*
046600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
046700970526     C     FUNKEY        IFEQ      FUDFUN
046800970526     C     FUDATT        ANDEQ     'S'
046900970526     C                   Z-ADD     I             W
047000970526     C                   END
047100970526     C                   END
047200970526     C*
047300970526     C* FUNZIONE NON ATTIVA (ERRORE)
047400970526     C     W             IFEQ      0
047500970526     C*
047600970526     C* EMISSIONE MESSAGGIO DI ERRORE
047700970526     C                   MOVEL     MSG(1)        DSMSMS
047800970526     C*
047900970526     C                   END
048000970526     C                   END
048100970526     C*
048200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
048300970526     C     W             IFNE      0
048400970526     C     FUNKEY        OREQ      *BLANKS
048500970526     C     FUNKEY        IFEQ      *BLANKS
048600970526     C                   EXSR      ENTGES
048700970526     C                   END
048800970526     C     FUNKEY        IFEQ      '03'
048900970526     C                   EXSR      F03GES
049000970526     C                   END
049100970526     C     FUNKEY        IFEQ      '06'
049200970526     C                   EXSR      F06GES
049300970526     C                   END
049400130109     C     FUNKEY        IFEQ      '07'
049500130109     C                   EXSR      F07GES
049600130109     C                   END
049700970526     C     FUNKEY        IFEQ      '12'
049800970526     C                   EXSR      F12GES
049900970526     C                   END
050000970526     C     FUNKEY        IFEQ      '24'
050100970526     C                   EXSR      F24GES
050200970526     C                   END
050300970526     C                   END
050400970526     C*
050500970526     C                   ENDSR
050600970526     C*------------------------------------------------------------------------*
050700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
050800970526     C*------------------------------------------------------------------------*
050900970526     C     FUNVAR        BEGSR
051000970526     C*
051100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
051200970526     C*
051300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
051400970526     C     KEY           IFEQ      ENTER
051500970526     C                   MOVEL     *BLANKS       FUNKEY
051600970526     C                   END
051700970526     C     KEY           IFEQ      F1
051800970526     C                   MOVEL     '01'          FUNKEY
051900970526     C                   END
052000970526     C     KEY           IFEQ      F2
052100970526     C                   MOVEL     '02'          FUNKEY
052200970526     C                   END
052300970526     C     KEY           IFEQ      F3
052400970526     C                   MOVEL     '03'          FUNKEY
052500970526     C                   END
052600970526     C     KEY           IFEQ      F4
052700970526     C                   MOVEL     '04'          FUNKEY
052800970526     C                   END
052900970526     C     KEY           IFEQ      F5
053000970526     C                   MOVEL     '05'          FUNKEY
053100970526     C                   END
053200970526     C     KEY           IFEQ      F6
053300970526     C                   MOVEL     '06'          FUNKEY
053400970526     C                   END
053500970526     C     KEY           IFEQ      F7
053600970526     C                   MOVEL     '07'          FUNKEY
053700970526     C                   END
053800970526     C     KEY           IFEQ      F8
053900970526     C                   MOVEL     '08'          FUNKEY
054000970526     C                   END
054100970526     C     KEY           IFEQ      F9
054200970526     C                   MOVEL     '09'          FUNKEY
054300970526     C                   END
054400970526     C     KEY           IFEQ      F10
054500970526     C                   MOVEL     '10'          FUNKEY
054600970526     C                   END
054700970526     C     KEY           IFEQ      F11
054800970526     C                   MOVEL     '11'          FUNKEY
054900970526     C                   END
055000970526     C     KEY           IFEQ      F12
055100970526     C                   MOVEL     '12'          FUNKEY
055200970526     C                   END
055300970526     C     KEY           IFEQ      F13
055400970526     C                   MOVEL     '13'          FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F14
055700970526     C                   MOVEL     '14'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F15
056000970526     C                   MOVEL     '15'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      F16
056300970526     C                   MOVEL     '16'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      F17
056600970526     C                   MOVEL     '17'          FUNKEY
056700970526     C                   END
056800970526     C     KEY           IFEQ      F18
056900970526     C                   MOVEL     '18'          FUNKEY
057000970526     C                   END
057100970526     C     KEY           IFEQ      F19
057200970526     C                   MOVEL     '19'          FUNKEY
057300970526     C                   END
057400970526     C     KEY           IFEQ      F20
057500970526     C                   MOVEL     '20'          FUNKEY
057600970526     C                   END
057700970526     C     KEY           IFEQ      F21
057800970526     C                   MOVEL     '21'          FUNKEY
057900970526     C                   END
058000970526     C     KEY           IFEQ      F22
058100970526     C                   MOVEL     '22'          FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F23
058400970526     C                   MOVEL     '23'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F24
058700970526     C                   MOVEL     '24'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      ROLLUP
059000970526     C                   MOVEL     'RU'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      HELP
059300970526     C                   MOVEL     'HP'          FUNKEY
059400970526     C                   END
059500970526     C                   ENDSR
059600970526     C*------------------------------------------------------------------------*
059700970526     C* NOTE COSTRUZIONE SCHIERE
059800970526     C*------------------------------------------------------------------------*
059900970526     C*
060000970526     C* -MSG-MESSAGGI
060100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060200970526     C* (NON TOCCARE MSG,1 E MSG,2)
060300970526     C*
060400970526     C* -CMD-COMANDI
060500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
060600970526     C*
060700970526     C* -OPD-OPZIONI
060800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
060900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
061000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
061100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
061400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
061500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
061600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
061700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
061800970526     C*
061900970526     C* -FUD-FUNZIONI
062000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
062100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
062400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
062600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
062700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
062900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
063000970526** MSG - MESSAGGI
063100120927Tasto funzionale non gestito                                                   1
063200120927Numero chiamate obbligatorio                                                   2
063300140819Libreria obbligatoria                                                          3
063400140819                                                                               4
063500130108                                                                               5
063600130108Campo obbligatorio                                                             6
063700130108                                                                               7
063800130108                                                                               8
063900120927                                                                               9
064000120927                                                                               1
064100120927                                                                               1
064200120927                                                                               1
064300120927                                                                               1
064400120927Data errata                                                                    1
064500140116                                                                               1
064600120927                                                                               1
064700120927                                                                               1
064800120927                                                                               1
064900120927                                                                               1
065000160129Reperimento concluso                                                           2
065100120927Scelta obbligatoria                                                            2
065200130110Eseguita la stampa                                                             2
065300121108                                                                               2
065400121108                                                                               2
065500970526** FUD - FUNZIONI
065600970526                   -        (S/N)=N                        funz.01¬
065700970526                   -        (S/N)=N                        funz.02¬
065800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
065900970526                   -        (S/N)=N                        funz.04¬
066000970526                   -        (S/N)=N                        funz.05¬
066100991214F06=Conferma       -        (S/N)=S                        funz.06¬
066200140819                   -        (S/N)=N                        funz.07¬
066300970527                   -        (S/N)=N                        funz.08¬
066400970527                   -        (S/N)=N                        funz.09¬
066500970527                   -        (S/N)=N                        funz.10¬
066600970526                   -        (S/N)=N                        funz.11¬
066700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
066800970526                   -        (S/N)=N                        funz.13¬
066900970526                   -        (S/N)=N                        funz.14¬
067000970526                   -        (S/N)=N                        funz.15¬
067100970526                   -        (S/N)=N                        funz.16¬
067200970526                   -        (S/N)=N                        funz.17¬
067300970526                   -        (S/N)=N                        funz.18¬
067400970526                   -        (S/N)=N                        funz.19¬
067500970526                   -        (S/N)=N                        funz.20¬
067600970526                   -        (S/N)=N                        funz.21¬
067700970526                   -        (S/N)=N                        funz.22¬
067800970526                   -        (S/N)=N                        funz.23¬
067900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
