000100970526     H*------------------------------------------------------------------------*
000200020611     H* STATISTICA EVENTI CLIENTE WIND
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020611     Ftist52d   cf   e             WORKSTN INFDS(DEVDS1)
000900020611     F                                     SFILE(st52DMS:MSGULT)
001000020611     Fcnaco00f  IF   E           K DISK    RENAME(cnaco000:cnacoXXX)
001100970526     D*------------------------------------------------------------------------*
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
005700991216     D*------------------
005800991216     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
005900991216     D*------------------
006000991216     D DSXC          e DS                  extname(XCLIRDS)
006100020611     D*------------------
006200991216     D                 DS
006300991216     D dsksn                   1      4p 0
006400991216     D dsksa                   1      4
006500970526     D*------------
006600970526     D* ARCHITETTURA
006700970526     D*------------
006800970526     D KPJBA         E DS
006900991215     D*------------
007000991215     D* TABELLE "XCLIR"- RICERCA CLIENTI
007100991215     D*------------
007200991216     D KSC             s              7  0 dim(30)
007300991216     D KSA             s              4    dim(30)
007400991216     D*------------------
007500991216     D BS69DS        e ds                  extname(TIBS69DS)
007600991216     D ACODS         e ds                  extname(cnaco00f)
007700991216     D INDDS         e ds                  extname(cnind00f)
007800991216     D CLPDS         e ds                  extname(cnclp00f)
007900991216     D CLSDS         e ds                  extname(fncls00f)
008000991214     D*------------------
008100991214     D* DS X PASSAGGIO PARAMETRI
008200991214     D*------------------
008300991214     D PARAM           DS                  INZ
008400020611     D  PARKSC                 1      7  0
008500020611     D  PARDAI                 8     15  0
008600020611     D  PARDAF                16     23  0
008700020923     D*-------------------
008800020923     D* DS REPERIMENTO DATI UTENTE
008900020923     D*-------------------
009000020923     D TIBS34DS      E DS                                                       *Profili utente
009100020923     D DDATIUTE      E DS                                                       *Dati utente
009200020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009300970526     C*------------------------------------------------------------------------*
009400000303     C* MAIN LINES
009500970526     C*------------------------------------------------------------------------*
009600970526     C*
009700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010100970526     C*
010200970526     C* EMETTE FORMATO VIDEO
010300970526     C                   EXSR      BUTFMR
010400970526     C*
010500970526     C* GESTIONE TASTI FUNZIONALI
010600970526     C                   EXSR      FUNCON
010700970526E   2C                   ENDDO
010800970526     C*
010900970526     C* FINE PGM
011000970526     C                   SETON                                        LR
011100970526     C*------------------------------------------------------------------------*
011200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011300970526     C*------------------------------------------------------------------------*
011400970526     C     BUTFMR        BEGSR
011500970526     C*
011600970526     C* EMETTE LA TESTATA
011700020611     C                   WRITE     st52D01
011800970526     C*
011900970526     C* SPEDISCE EVENTUALE MESSAGGIO
012000970526     C                   EXSR      SNDMSG
012100970526     C*
012200970526     C* EMETTE LE FUNZIONI
012300020611     C                   WRITE     st52D02
012400970526     C*
012500970526     C* EMETTE IL VIDEO
012600020611     C                   EXFMT     st52D03
012700970526     C*
012800991214     C* REIMPOSTA FLAG DI ERRORE
012900991214     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* REIMPOSTA INDICATORI DI ERRORE
013200020611     C                   SETOFF                                       555556
013300970526     C*
013400970526     C                   ENDSR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013700970526     C*------------------------------------------------------------------------*
013800970526     C     SNDMSG        BEGSR
013900970526     C*
014000970526     C* PULISCE IL SFILE MESSAGGI
014100970526     C                   MOVEL     '1'           *IN40
014200020611     C                   WRITE     st52DMC
014300970526     C                   MOVEL     '0'           *IN40
014400970526     C*
014500970526     C* VISUALIZZA IL MESSAGGIO
014600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014700970526     C                   CALL      'XMSG'        XMSGPA
014800020611     C     DSMSER        IFEQ      *ZEROS
014900970526     C                   MOVEL     DSMSKE        MSGKEY
015000020611     C                   ENDIF
015100970526     C                   Z-ADD     1             MSGULT
015200020611     C                   WRITE     st52DMS
015300020611     C                   WRITE     st52DMC
015400970526     C*
015500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015600970526     C                   MOVEL     *BLANKS       DSMSMS
015700970526     C*
015800970526     C                   ENDSR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* ENTGES - GESTIONE ENTER -> INVIO
016100970526     C*------------------------------------------------------------------------*
016200970526     C     ENTGES        BEGSR
016300970526     C*
016400970526     C* CONTROLLA I CAMPI IMMESSI
016500970526     C                   EXSR      DECODI
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F03GES - GESTIONE F03 -> FINE
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F03GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F03"
017400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800991214     C* F06GES - GESTIONE F06 -> CONFERMA
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F06GES        BEGSR
018100970526     C*
018200991214     C* CONTROLLA I CAMPI IMMESSI
018300991214     C                   EXSR      DECODI
018400020611     C*
018500991214     C* ESEGUE ROUTINE DI CONFERMA
018600020611     C     ERRL00        IFEQ      '0'
018700991214     C                   EXSR      ESEGUI
018800991214     C                   ENDIF
018900970526     C*
019000991214     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F12GES - GESTIONE F12 -> RITORNO
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F12GES        BEGSR
019500970526     C*
019600970526     C* IMPOSTA "PREMUTO F12"
019700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F24GES        BEGSR
020400970526     C*
020500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020600970526     C*
020700970526     C                   ENDSR
020800991214     C*------------------------------------------------------------------------*
020900991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
021000991214     C*------------------------------------------------------------------------*
021100991214     C     PASPAR        BEGSR
021200991214     C*
021300020611     C                   MOVEL     VIDKSC        PARKSC
021400020611     C                   Z-ADD     DAIVID        PARDAI
021500020611     C                   Z-ADD     DAFVID        PARDAF
021600991214     C*
021700991214     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022000970526     C*------------------------------------------------------------------------*
022100991214     C     ESEGUI        BEGSR
022200991214     C*
022300991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
022400020611     C                   MOVEL     'ST53'        KCOAZ
022500991214     C                   EXSR      PASPAR
022600991214     C                   MOVEL     PARAM         KPJBU
022700020614     C                   CALL      'BCH10'
022800020614 MOM C***                CALL      'TIST53R'
022900991214     C                   PARM                    KPJBA
023000991214     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023400970526     C*------------------------------------------------------------------------*
023500970526     C     DECODI        BEGSR
023600000306     C*
023700020611     C                   SETOFF                                       545556
023800000306     C*
023900991215     C*--------------------
024000000306     C* CLIENTI
024100991215     C*--------------------
024200991215     C     ERRL00        IFEQ      '0'
024300991216     C     '?'           SCAN      VIDKSC                                 30
024400991215     C     *IN30         IFEQ      *ON
024500991215     C                   CLEAR                   DSXC
024600020923     C                   Z-ADD     DUTKCI        DXCCAP
024700991215     C                   CALL      'XCLIR'
024800991215     C                   PARM                    DSXC
024900991215     C     DXCERR        ifne      '1'
025000991216     C*-------------
025100991216     C                   MOVEA     DXCKSC        KSA                        ALF
025200991216DO  2C     1             DO        30            J                 3 0
025300991216     C                   MOVE      KSA(J)        DSKSA                      F
025400991216     C                   Z-ADD     DSKSN         KSC(J)                     M
025500991216E   2C                   ENDDO
025600991216     C*-------------
025700991216     C                   MOVE      KSC(1)        VIDKSC
025800991215     C                   ENDIF
025900991215     C                   ENDIF
026000991216     C     VIDKSC        IFNE      *zeros
026100991216     C     VIDKSC        ANDNE     *blanks
026200020923     C                   Z-ADD     DUTKCI        kclkcc
026300020611     C                   MOVEL     VIDKSC        kclksc
026400020611     C     keycli        chain     cnaco00f                           99
026500991215     C     *IN99         ifeq      *off
026600991216     C                   MOVEL     acorag        VIDRAG
026700020611     C                   endif
026800000306     C*
026900000306     C                   ENDIF
027000000306     C                   ENDIF
027100991015     C*--------------------
027200020611     C* DATA INIZIO
027300991015     C*--------------------
027400020611     C     *LIKE         DEFINE    VIDDAI        DAIVID
027500020611     C     ERRL00        IFEQ      '0'
027600020611     C                   Z-ADD     VIDDAI        WDA8
027700020611     C                   Z-ADD     *ZEROS        DA8W
027800991015     C                   EXSR      CHKDA8
027900020611     C                   Z-ADD     WDA8          VIDDAI
028000020611     C                   Z-ADD     DA8W          DAIVID
028100020611     C     ERRL00        IFEQ      '1'
028200991214     C                   SETON                                        54
028300020611     C                   MOVEL     MSG(14)       DSMSMS
028400020611     C                   ENDIF
028500020611     C                   ENDIF
028600970528     C*--------------------
028700020611     C* DATA FINE
028800970528     C*--------------------
028900020611     C     *LIKE         DEFINE    VIDDAF        DAFVID
029000020611     C     ERRL00        IFEQ      '0'
029100020611     C                   Z-ADD     VIDDAF        WDA8
029200020611     C                   Z-ADD     *ZEROS        DA8W
029300970528     C                   EXSR      CHKDA8
029400020611     C                   Z-ADD     WDA8          VIDDAF
029500020611     C                   Z-ADD     DA8W          DAFVID
029600020611     C     ERRL00        IFEQ      '1'
029700991214     C                   SETON                                        55
029800970528     C                   MOVEL     MSG(14)       DSMSMS
029900020611     C                   ENDIF
030000020611     C                   ENDIF
030100020611     C*
030200970528     C* SCADENZA > DECORRENZA
030300020611     C     ERRL00        IFEQ      '0'
030400020611     C     DAFVID        IFLT      DAIVID
030500020611     C                   SETON                                        5455
030600970528     C                   MOVEL     MSG(15)       DSMSMS
030700970528     C                   MOVEL     '1'           ERRL00
030800020611     C                   ENDIF
030900020611     C                   ENDIF
031000000306     C*
031100970528     C                   ENDSR
031200970528     C*------------------------------------------------------------------------*
031300970528     C* CHKDA8 - CONTROLLA LA DATA (8)
031400970528     C*------------------------------------------------------------------------*
031500970528     C     CHKDA8        BEGSR
031600970528     C*
031700020611     C     *LIKE         DEFINE    VIDDAI        WDA8
031800020611     C     *LIKE         DEFINE    VIDDAF        DA8W
031900970528     C*
032000970528     C                   Z-ADD     WDA8          G08DAT
032100970528     C                   Z-ADD     *ZEROS        G08INV
032200970528     C                   MOVEL     '0'           G08ERR
032300970528     C                   CALL      'XSRDA8'
032400970528     C                   PARM                    WLBDA8
032500970528     C                   Z-ADD     G08DAT        WDA8
032600970528     C                   Z-ADD     G08INV        DA8W
032700020611     C     G08ERR        IFEQ      '1'
032800970528     C                   MOVEL     '1'           ERRL00
032900020611     C                   ENDIF
033000970528     C*
033100970528     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
033400970526     C*------------------------------------------------------------------------*
033500970526     C     AZZVID        BEGSR
033600970526     C* CAMPI
033700020614     C                   MOVEL     '0631297'     VIDKSC
033800020611     C                   Z-ADD     *ZEROS        VIDDAI
033900020611     C                   Z-ADD     *ZEROS        VIDDAF
034000970526     C* DECODIFICHE
034100991216     C                   MOVEL     *BLANKS       VIDRAG
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
034600970526     C*------------------------------------------------------------------------*
034700970526     C     IMPFUN        BEGSR
034800970526     C*
034900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
035000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
035100970526     C                   EXSR      FUNGES
035200970526     C*
035300970526     C                   ENDSR
035400020923     C*--------------------------------------------------------------------------------------------*
035500020923     C* REPERISCE I DATI UTENTE
035600020923     C*--------------------------------------------------------------------------------------------*
035700020923     C     REPDATIUTE    BEGSR
035800020923     C*
035900020923     C* INIZIALIZZA VARIABILI DI WRK
036000020923     C                   CLEAR                   TIBS34DS
036100020923     C                   CLEAR                   AZUTEDS
036200020923     C                   CLEAR                   DDATIUTE
036300020923     C*
036400020923     C     *DTAARA       DEFINE    §azute        azuteds
036500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
036600020923     C                   IN(E)     *DTAARA
036700020923if  1C                   IF        %Error
036800020923     c                   EVAL      I34Tla = 'L'
036900020923     C                   CALL      'TIBS34R'
037000020923     C                   PARM                    Tibs34Ds
037100020923     C                   IN        *DTAARA
037200020923e   1C                   ENDIF
037300020923     C*
037400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
037600020923     C*
037700020923     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900970526     C* *INZSR - OPERAZIONI INIZIALI
038000970526     C*------------------------------------------------------------------------*
038100970526     C     *INZSR        BEGSR
038200991217     C*
038300991217     C     *ENTRY        PLIST
038400991217     C                   PARM                    KPJBA
038500020923     C*
038600020923     C* REPERISCE I DATI DELL'UTENTE
038700020923     C                   EXSR      REPDATIUTE
038800991215     C*
038900000306     C* LETTURA cnaco31c/cnaco00f
039000991215     C     keycli        KLIST                                                  *di cnaco/ind/clp
039100991215     C                   KFLD                    kclkut            1 0          -utente
039200991215     C                   KFLD                    kclkcc            4 0          -capoconto
039300991215     C                   KFLD                    kclksc            7 0          -codice
039400991216     C                   z-add     1             kclkut
039500970526     C*
039600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
039700970526     C     XMSGPA        PLIST
039800970526     C                   PARM                    DSMSMS
039900970526     C                   PARM                    DSMSPG
040000970526     C                   PARM                    DSMSKE
040100970526     C                   PARM                    DSMSER
040200970526     C*
040300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
040400970526     C                   CALL      'XKEY'
040500970526     C                   PARM                    DSKEY
040600970526     C*---
040700970526     C* VARIABILI NON RIFERITE AL DATA BASE
040800970526     C*---
040900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
041000991214     C                   MOVEL     *BLANKS       DEPOP1            3
041100970526     C*---
041200970526     C* VARIABILI DI CONTROLLO
041300970526     C*---
041400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
041500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
041600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
041700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
041800970526     C*
041900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
042000020611     C                   MOVEL     'TIST52R'     DSMSPG
042100020611     C                   MOVEL     'TIST52R'     MSGQUE
042200970526     C                   MOVEL     *BLANKS       MSGKEY
042300970526     C                   MOVEL     *BLANKS       DSMSMS
042400970528     C*
042500970528     C* CALCOLA LA DATA CORRENTE
042600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
042700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
042800970528     C                   Z-ADD     WN8           G08DAT
042900970528     C                   Z-ADD     *ZEROS        G08INV
043000970528     C                   MOVEL     '0'           G08ERR
043100970528     C                   CALL      'XSRDA8'
043200970528     C                   PARM                    WLBDA8
043300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043400000310     C*
043500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
043600000310     C                   EXSR      AZZVID
043700970526     C*
043800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
043900970526     C                   EXSR      IMPFUN
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
044400970526     C*------------------------------------------------------------------------*
044500970526     C     FUNGES        BEGSR
044600970526     C*
044700970526     C* PULISCO CAMPI DI LAVORO
044800970526     C                   Z-ADD     0             W                 5 0
044900970526     C*
045000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
045100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
045200970526     C     1             DO        24            I
045300970526     C                   MOVEL     FUD(I)        FUDRIG
045400970526     C*
045500970526     C* PULIZIA FUNZIONI PRECEDENTI
045600970526     C                   MOVEL     *BLANKS       FUV(I)
045700970526     C*
045800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
045900970526     C* (SALTA F24)
046000970526     C     FUDATT        IFEQ      'S'
046100970526     C     FUDFUN        ANDNE     '24'
046200970526     C                   ADD       1             W
046300970526     C                   MOVEL     FUDDES        FUV(W)
046400970526     C                   END
046500970526     C                   END                                                    DO WHILE
046600970526     C*
046700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
046800970526     C* ALTRIMENTI DISATTIVO F24
046900970526     C     1             DO        24            I
047000970526     C                   MOVEL     FUD(I)        FUDRIG
047100970526     C*
047200970526     C     FUDFUN        IFEQ      '24'
047300970526     C     W             IFGT      6
047400970526     C                   MOVEL     'S'           FUDATT
047500970526     C                   ELSE
047600970526     C                   MOVEL     *BLANKS       FUDATT
047700970526     C                   END                                                    W GT 8
047800970526     C                   MOVEL     FUDRIG        FUD(I)
047900970526     C                   END
048000970526     C                   END                                                    DO WHILE
048100970526     C*
048200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
048300970526     C     W             DIV       6             I
048400970526     C                   MVR                     Z
048500970526     C*
048600970526     C     Z             IFNE      0
048700970526     C     I             ADD       1             Z
048800970526     C                   ELSE
048900970526     C                   Z-ADD     I             Z                 5 0
049000970526     C                   END
049100970526     C*
049200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
049300970526     C                   ADD       1             FUNGRU
049400970526     C     FUNGRU        IFGT      Z
049500970526     C     FUNGRU        ORLT      1
049600970526     C                   Z-ADD     1             FUNGRU
049700970526     C                   END
049800970526     C*
049900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
050000970526     C     FUNGRU        IFEQ      1
050100970526     C                   MOVEL     FUV(1)        FUN001
050200970526     C                   MOVEL     FUV(2)        FUN002
050300970526     C                   MOVEL     FUV(3)        FUN003
050400970526     C                   MOVEL     FUV(4)        FUN004
050500970526     C                   MOVEL     FUV(5)        FUN005
050600970526     C                   MOVEL     FUV(6)        FUN006
050700970526     C                   END
050800970526     C*
050900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
051000970526     C     FUNGRU        IFEQ      2
051100970526     C                   MOVEL     FUV(7)        FUN001
051200970526     C                   MOVEL     FUV(8)        FUN002
051300970526     C                   MOVEL     FUV(9)        FUN003
051400970526     C                   MOVEL     FUV(10)       FUN004
051500970526     C                   MOVEL     FUV(11)       FUN005
051600970526     C                   MOVEL     FUV(12)       FUN006
051700970526     C                   END
051800970526     C*
051900970526     C* TERZO GRUPPO DI 6 FUNZIONI
052000970526     C     FUNGRU        IFEQ      3
052100970526     C                   MOVEL     FUV(13)       FUN001
052200970526     C                   MOVEL     FUV(14)       FUN002
052300970526     C                   MOVEL     FUV(15)       FUN003
052400970526     C                   MOVEL     FUV(16)       FUN004
052500970526     C                   MOVEL     FUV(17)       FUN005
052600970526     C                   MOVEL     FUV(18)       FUN006
052700970526     C                   END
052800970526     C*
052900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
053000970526     C     FUNGRU        IFEQ      4
053100970526     C                   MOVEL     FUV(19)       FUN001
053200970526     C                   MOVEL     FUV(20)       FUN002
053300970526     C                   MOVEL     FUV(21)       FUN003
053400970526     C                   MOVEL     FUV(22)       FUN004
053500970526     C                   MOVEL     *BLANKS       FUN005
053600970526     C                   MOVEL     *BLANKS       FUN006
053700970526     C                   END
053800970526     C*
053900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
054000970526     C                   MOVEL     *BLANKS       FUN007
054100970526     C                   MOVEL     *BLANKS       FUN008
054200970526     C*
054300970526     C     1             DO        24            I
054400970526     C                   MOVEL     FUD(I)        FUDRIG
054500970526     C     FUDFUN        IFEQ      '24'
054600970526     C     FUDATT        ANDEQ     'S'
054700970526     C                   MOVEL     FUDDES        FUN008
054800970526     C                   END
054900970526     C                   END                                                    DO
055000970526     C*
055100970526     C                   ENDSR
055200970526     C*------------------------------------------------------------------------*
055300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
055400970526     C*------------------------------------------------------------------------*
055500970526     C     FUNCON        BEGSR
055600970526     C*
055700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
055800970526     C                   EXSR      FUNVAR
055900970526     C*
056000970526     C* PULISCO CAMPI DI LAVORO
056100970526     C                   Z-ADD     0             I                 5 0
056200970526     C                   Z-ADD     0             W                 5 0
056300970526     C*
056400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
056500970526     C     FUNKEY        IFNE      *BLANKS
056600970526     C     1             DO        24            I
056700970526     C                   MOVEL     FUD(I)        FUDRIG
056800970526     C*
056900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
057000970526     C     FUNKEY        IFEQ      FUDFUN
057100970526     C     FUDATT        ANDEQ     'S'
057200970526     C                   Z-ADD     I             W
057300970526     C                   END
057400970526     C                   END                                                    DO WHILE
057500970526     C*
057600970526     C* FUNZIONE NON ATTIVA (ERRORE)
057700970526     C     W             IFEQ      0
057800970526     C*
057900970526     C* EMISSIONE MESSAGGIO DI ERRORE
058000970526     C                   MOVEL     MSG(1)        DSMSMS
058100970526     C*
058200970526     C                   END                                                    W=0
058300970526     C                   END                                                    FUNKEY NE *BLANKS
058400970526     C*
058500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
058600970526     C     W             IFNE      0
058700970526     C     FUNKEY        OREQ      *BLANKS
058800970526     C     FUNKEY        IFEQ      *BLANKS
058900970526     C                   EXSR      ENTGES
059000970526     C                   END
059100970526     C     FUNKEY        IFEQ      '03'
059200970526     C                   EXSR      F03GES
059300970526     C                   END
059400970526     C     FUNKEY        IFEQ      '06'
059500970526     C                   EXSR      F06GES
059600970526     C                   END
059700970526     C     FUNKEY        IFEQ      '12'
059800970526     C                   EXSR      F12GES
059900970526     C                   END
060000970526     C     FUNKEY        IFEQ      '24'
060100970526     C                   EXSR      F24GES
060200970526     C                   END
060300970526     C                   END
060400970526     C*
060500970526     C                   ENDSR
060600970526     C*------------------------------------------------------------------------*
060700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060800970526     C*------------------------------------------------------------------------*
060900970526     C     FUNVAR        BEGSR
061000970526     C*
061100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
061200970526     C*
061300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
061400970526     C     KEY           IFEQ      ENTER
061500970526     C                   MOVEL     *BLANKS       FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F1
061800970526     C                   MOVEL     '01'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F2
062100970526     C                   MOVEL     '02'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F3
062400970526     C                   MOVEL     '03'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F4
062700970526     C                   MOVEL     '04'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F5
063000970526     C                   MOVEL     '05'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F6
063300970526     C                   MOVEL     '06'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F7
063600970526     C                   MOVEL     '07'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      F8
063900970526     C                   MOVEL     '08'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F9
064200970526     C                   MOVEL     '09'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F10
064500970526     C                   MOVEL     '10'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F11
064800970526     C                   MOVEL     '11'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F12
065100970526     C                   MOVEL     '12'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F13
065400970526     C                   MOVEL     '13'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F14
065700970526     C                   MOVEL     '14'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F15
066000970526     C                   MOVEL     '15'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F16
066300970526     C                   MOVEL     '16'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F17
066600970526     C                   MOVEL     '17'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F18
066900970526     C                   MOVEL     '18'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F19
067200970526     C                   MOVEL     '19'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F20
067500970526     C                   MOVEL     '20'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F21
067800970526     C                   MOVEL     '21'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F22
068100970526     C                   MOVEL     '22'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F23
068400970526     C                   MOVEL     '23'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F24
068700970526     C                   MOVEL     '24'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      ROLLUP
069000970526     C                   MOVEL     'RU'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      HELP
069300970526     C                   MOVEL     'HP'          FUNKEY
069400970526     C                   END
069500970526     C                   ENDSR
069600970526     C*------------------------------------------------------------------------*
069700970526     C* NOTE COSTRUZIONE SCHIERE
069800970526     C*------------------------------------------------------------------------*
069900970526     C*
070000970526     C* -MSG-MESSAGGI
070100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
070200970526     C* (NON TOCCARE MSG,1 E MSG,2)
070300970526     C*
070400970526     C* -CMD-COMANDI
070500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070600970526     C*
070700970526     C* -OPD-OPZIONI
070800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
070900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
071000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
071100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
071800970526     C*
071900970526     C* -FUD-FUNZIONI
072000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
072100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
072200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
072300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
072400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
072500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
072600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
072700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
072800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
072900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
073000970526     O*------------------------------------------------------------------------*
073100970526** MSG - MESSAGGI
073200020611TIST52R- Tasto funzionale non gestito                                          1
073300020611TIST52R-                                                                       2
073400020611TIST52R-                                                                       3
073500020611TIST52R-                                                                       4
073600020611TIST52R-                                                                       5
073700020611TIST52R-                                                                       6
073800020611TIST52R-                                                                       7
073900020611TIST52R-                                                                       8
074000020611TIST52R-                                                                       9
074100020611TIST52R-                                                                       10
074200020611TIST52R-                                                                       11
074300020611TIST52R-                                                                       12
074400020611TIST52R-                                                                       13
074500020611TIST52R- Data errata                                                           14
074600020611TIST52R- La data inizio deve essere maggiore della data fine                   15
074700020611TIST52R- Cliente inserito inesistente.                                         16
074800020611TIST52R-                                                                       17
074900020611TIST52R-                                                                       18
075000020611TIST52R-                                                                       19
075100020611TIST52R-                                                                       20
075200020611TIST52R- Scelta obbligatoria.                                                  21
075300020611TIST52R-                                                                       22
075400020611TIST52R-                                                                       23
075500020611TIST52R-                                                                       24
075600970526** FUD - FUNZIONI
075700970526                   -        (S/N)=N                        funz.01¬
075800970526                   -        (S/N)=N                        funz.02¬
075900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
076000970526                   -        (S/N)=N                        funz.04¬
076100970526                   -        (S/N)=N                        funz.05¬
076200991214F06=Conferma       -        (S/N)=S                        funz.06¬
076300970527                   -        (S/N)=N                        funz.07¬
076400970527                   -        (S/N)=N                        funz.08¬
076500970527                   -        (S/N)=N                        funz.09¬
076600970527                   -        (S/N)=N                        funz.10¬
076700970526                   -        (S/N)=N                        funz.11¬
076800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
076900970526                   -        (S/N)=N                        funz.13¬
077000970526                   -        (S/N)=N                        funz.14¬
077100970526                   -        (S/N)=N                        funz.15¬
077200970526                   -        (S/N)=N                        funz.16¬
077300970526                   -        (S/N)=N                        funz.17¬
077400970526                   -        (S/N)=N                        funz.18¬
077500970526                   -        (S/N)=N                        funz.19¬
077600970526                   -        (S/N)=N                        funz.20¬
077700970526                   -        (S/N)=N                        funz.21¬
077800970526                   -        (S/N)=N                        funz.22¬
077900970526                   -        (S/N)=N                        funz.23¬
078000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
