000100970526     H*------------------------------------------------------------------------*
000200030414     H* STATISTICA SITUAZIONE CLIENTE TELE2
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030414     Ftist56d   cf   e             WORKSTN INFDS(DEVDS1)
000900030414     F                                     SFILE(st56DMS:MSGULT)
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
008700030414     D  PARDAI2               24     31  0
008800030414     D  PARDAF2               32     39  0
008900030414     D  PAROTQ                40     49
009000030414     D  PARRAG                50     89
009100020923     D*-------------------
009200020923     D* DS REPERIMENTO DATI UTENTE
009300020923     D*-------------------
009400020923     D TIBS34DS      E DS                                                       *Profili utente
009500020923     D DDATIUTE      E DS                                                       *Dati utente
009600020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009700970526     C*------------------------------------------------------------------------*
009800000303     C* MAIN LINES
009900970526     C*------------------------------------------------------------------------*
010000970526     C*
010100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010200991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010500970526     C*
010600970526     C* EMETTE FORMATO VIDEO
010700970526     C                   EXSR      BUTFMR
010800970526     C*
010900970526     C* GESTIONE TASTI FUNZIONALI
011000970526     C                   EXSR      FUNCON
011100970526E   2C                   ENDDO
011200970526     C*
011300970526     C* FINE PGM
011400970526     C                   SETON                                        LR
011500970526     C*------------------------------------------------------------------------*
011600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011700970526     C*------------------------------------------------------------------------*
011800970526     C     BUTFMR        BEGSR
011900970526     C*
012000970526     C* EMETTE LA TESTATA
012100030414     C                   WRITE     st56D01
012200970526     C*
012300970526     C* SPEDISCE EVENTUALE MESSAGGIO
012400970526     C                   EXSR      SNDMSG
012500970526     C*
012600970526     C* EMETTE LE FUNZIONI
012700030414     C                   WRITE     st56D02
012800970526     C*
012900970526     C* EMETTE IL VIDEO
013000030414     C                   EXFMT     st56D03
013100970526     C*
013200991214     C* REIMPOSTA FLAG DI ERRORE
013300991214     C                   MOVEL     '0'           ERRL00
013400970526     C*
013500970526     C* REIMPOSTA INDICATORI DI ERRORE
013600020611     C                   SETOFF                                       555556
013700970526     C*
013800970526     C                   ENDSR
013900970526     C*------------------------------------------------------------------------*
014000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014100970526     C*------------------------------------------------------------------------*
014200970526     C     SNDMSG        BEGSR
014300970526     C*
014400970526     C* PULISCE IL SFILE MESSAGGI
014500970526     C                   MOVEL     '1'           *IN40
014600030414     C                   WRITE     st56DMC
014700970526     C                   MOVEL     '0'           *IN40
014800970526     C*
014900970526     C* VISUALIZZA IL MESSAGGIO
015000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015100970526     C                   CALL      'XMSG'        XMSGPA
015200020611     C     DSMSER        IFEQ      *ZEROS
015300970526     C                   MOVEL     DSMSKE        MSGKEY
015400020611     C                   ENDIF
015500970526     C                   Z-ADD     1             MSGULT
015600030414     C                   WRITE     st56DMS
015700030414     C                   WRITE     st56DMC
015800970526     C*
015900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016000970526     C                   MOVEL     *BLANKS       DSMSMS
016100970526     C*
016200970526     C                   ENDSR
016300970526     C*------------------------------------------------------------------------*
016400970526     C* ENTGES - GESTIONE ENTER -> INVIO
016500970526     C*------------------------------------------------------------------------*
016600970526     C     ENTGES        BEGSR
016700970526     C*
016800970526     C* CONTROLLA I CAMPI IMMESSI
016900970526     C                   EXSR      DECODI
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F03GES - GESTIONE F03 -> FINE
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F03GES        BEGSR
017600970526     C*
017700970526     C* IMPOSTA "PREMUTO F03"
017800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200991214     C* F06GES - GESTIONE F06 -> CONFERMA
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600991214     C* CONTROLLA I CAMPI IMMESSI
018700991214     C                   EXSR      DECODI
018800020611     C*
018900991214     C* ESEGUE ROUTINE DI CONFERMA
019000020611     C     ERRL00        IFEQ      '0'
019100991214     C                   EXSR      ESEGUI
019200030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
019300991214     C                   ENDIF
019400970526     C*
019500991214     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F12GES - GESTIONE F12 -> RITORNO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F12GES        BEGSR
020000970526     C*
020100970526     C* IMPOSTA "PREMUTO F12"
020200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F24GES        BEGSR
020900970526     C*
021000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100970526     C*
021200970526     C                   ENDSR
021300991214     C*------------------------------------------------------------------------*
021400991214     C* PASPAR - ASSEGNAZIONI X PASSAGGIO PARAMETRI
021500991214     C*------------------------------------------------------------------------*
021600991214     C     PASPAR        BEGSR
021700991214     C*
021800020611     C                   MOVEL     VIDKSC        PARKSC
021900020611     C                   Z-ADD     DAIVID        PARDAI
022000020611     C                   Z-ADD     DAFVID        PARDAF
022100030414     C                   Z-ADD     VIDDAI        PARDAI2
022200030414     C                   Z-ADD     VIDDAF        PARDAF2
022300030414     C                   MOVEL     VIDOTQ        PAROTQ
022400030414     C                   MOVEL     VIDRAG        PARRAG
022500991214     C*
022600991214     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022900970526     C*------------------------------------------------------------------------*
023000991214     C     ESEGUI        BEGSR
023100991214     C*
023200991214     C* LANCIO IL PROGRAMMA DI CREAZIONE
023300030414     C                   MOVEL     'ST57'        KCOAZ
023400991214     C                   EXSR      PASPAR
023500991214     C                   MOVEL     PARAM         KPJBU
023600030603     C                   CALL      'BCH10'
023700030603 MOM C***                CALL      'TIST57C'
023800991214     C                   PARM                    KPJBA
023900991214     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024300970526     C*------------------------------------------------------------------------*
024400970526     C     DECODI        BEGSR
024500000306     C*
024600020611     C                   SETOFF                                       545556
024700030414     C                   SETOFF                                       57
024800000306     C*
024900991215     C*--------------------
025000000306     C* CLIENTI
025100991215     C*--------------------
025200991215     C     ERRL00        IFEQ      '0'
025300991216     C     '?'           SCAN      VIDKSC                                 30
025400991215     C     *IN30         IFEQ      *ON
025500991215     C                   CLEAR                   DSXC
025600020923     C                   Z-ADD     DUTKCI        DXCCAP
025700991215     C                   CALL      'XCLIR'
025800991215     C                   PARM                    DSXC
025900991215     C     DXCERR        ifne      '1'
026000991216     C*-------------
026100991216     C                   MOVEA     DXCKSC        KSA                        ALF
026200991216DO  2C     1             DO        30            J                 3 0
026300991216     C                   MOVE      KSA(J)        DSKSA                      F
026400991216     C                   Z-ADD     DSKSN         KSC(J)                     M
026500991216E   2C                   ENDDO
026600991216     C*-------------
026700991216     C                   MOVE      KSC(1)        VIDKSC
026800991215     C                   ENDIF
026900991215     C                   ENDIF
027000991216     C     VIDKSC        IFNE      *zeros
027100991216     C     VIDKSC        ANDNE     *blanks
027200020923     C                   Z-ADD     DUTKCI        kclkcc
027300020611     C                   MOVEL     VIDKSC        kclksc
027400020611     C     keycli        chain     cnaco00f                           99
027500991215     C     *IN99         ifeq      *off
027600991216     C                   MOVEL     acorag        VIDRAG
027700020611     C                   endif
027800000306     C*
027900000306     C                   ENDIF
028000000306     C                   ENDIF
028100991015     C*--------------------
028200020611     C* DATA INIZIO
028300991015     C*--------------------
028400020611     C     *LIKE         DEFINE    VIDDAI        DAIVID
028500020611     C     ERRL00        IFEQ      '0'
028600020611     C                   Z-ADD     VIDDAI        WDA8
028700020611     C                   Z-ADD     *ZEROS        DA8W
028800991015     C                   EXSR      CHKDA8
028900020611     C                   Z-ADD     WDA8          VIDDAI
029000020611     C                   Z-ADD     DA8W          DAIVID
029100020611     C     ERRL00        IFEQ      '1'
029200991214     C                   SETON                                        54
029300020611     C                   MOVEL     MSG(14)       DSMSMS
029400020611     C                   ENDIF
029500020611     C                   ENDIF
029600970528     C*--------------------
029700020611     C* DATA FINE
029800970528     C*--------------------
029900020611     C     *LIKE         DEFINE    VIDDAF        DAFVID
030000020611     C     ERRL00        IFEQ      '0'
030100020611     C                   Z-ADD     VIDDAF        WDA8
030200020611     C                   Z-ADD     *ZEROS        DA8W
030300970528     C                   EXSR      CHKDA8
030400020611     C                   Z-ADD     WDA8          VIDDAF
030500020611     C                   Z-ADD     DA8W          DAFVID
030600020611     C     ERRL00        IFEQ      '1'
030700991214     C                   SETON                                        55
030800970528     C                   MOVEL     MSG(14)       DSMSMS
030900020611     C                   ENDIF
031000020611     C                   ENDIF
031100020611     C*
031200970528     C* SCADENZA > DECORRENZA
031300020611     C     ERRL00        IFEQ      '0'
031400020611     C     DAFVID        IFLT      DAIVID
031500020611     C                   SETON                                        5455
031600970528     C                   MOVEL     MSG(15)       DSMSMS
031700970528     C                   MOVEL     '1'           ERRL00
031800020611     C                   ENDIF
031900020611     C                   ENDIF
032000030414     C*--------------------
032100030414     C* CODA DI STAMPA
032200030414     C*--------------------
032300030414     C     ERRL00        IFEQ      '0'
032400030512     C                   CALL      'TIST56C'
032500030414     C                   PARM                    VIDOTQ
032600030414     C                   PARM      '0'           ESITO             1
032700030414     C     ESITO         IFEQ      '1'
032800030414     C                   SETON                                        57
032900030414     C                   MOVEL     MSG(17)       DSMSMS
033000030414     C                   MOVEL     '1'           ERRL00
033100030414     C                   ENDIF
033200030414     C                   ENDIF
033300000306     C*
033400970528     C                   ENDSR
033500970528     C*------------------------------------------------------------------------*
033600970528     C* CHKDA8 - CONTROLLA LA DATA (8)
033700970528     C*------------------------------------------------------------------------*
033800970528     C     CHKDA8        BEGSR
033900970528     C*
034000020611     C     *LIKE         DEFINE    VIDDAI        WDA8
034100020611     C     *LIKE         DEFINE    VIDDAF        DA8W
034200970528     C*
034300970528     C                   Z-ADD     WDA8          G08DAT
034400970528     C                   Z-ADD     *ZEROS        G08INV
034500970528     C                   MOVEL     '0'           G08ERR
034600970528     C                   CALL      'XSRDA8'
034700970528     C                   PARM                    WLBDA8
034800970528     C                   Z-ADD     G08DAT        WDA8
034900970528     C                   Z-ADD     G08INV        DA8W
035000020611     C     G08ERR        IFEQ      '1'
035100970528     C                   MOVEL     '1'           ERRL00
035200020611     C                   ENDIF
035300970528     C*
035400970528     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
035700970526     C*------------------------------------------------------------------------*
035800970526     C     AZZVID        BEGSR
035900970526     C* CAMPI
036000030414     C                   MOVEL     '0052454'     VIDKSC
036100020611     C                   Z-ADD     *ZEROS        VIDDAI
036200020611     C                   Z-ADD     *ZEROS        VIDDAF
036300030414     C                   MOVEL     *BLANKS       VIDOTQ
036400970526     C* DECODIFICHE
036500991216     C                   MOVEL     *BLANKS       VIDRAG
036600970526     C*
036700970526     C                   ENDSR
036800970526     C*------------------------------------------------------------------------*
036900020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
037000970526     C*------------------------------------------------------------------------*
037100970526     C     IMPFUN        BEGSR
037200970526     C*
037300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
037400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
037500970526     C                   EXSR      FUNGES
037600970526     C*
037700970526     C                   ENDSR
037800020923     C*--------------------------------------------------------------------------------------------*
037900020923     C* REPERISCE I DATI UTENTE
038000020923     C*--------------------------------------------------------------------------------------------*
038100020923     C     REPDATIUTE    BEGSR
038200020923     C*
038300020923     C* INIZIALIZZA VARIABILI DI WRK
038400020923     C                   CLEAR                   TIBS34DS
038500020923     C                   CLEAR                   AZUTEDS
038600020923     C                   CLEAR                   DDATIUTE
038700020923     C*
038800020923     C     *DTAARA       DEFINE    §azute        azuteds
038900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
039000020923     C                   IN(E)     *DTAARA
039100020923if  1C                   IF        %Error
039200020923     c                   EVAL      I34Tla = 'L'
039300020923     C                   CALL      'TIBS34R'
039400020923     C                   PARM                    Tibs34Ds
039500020923     C                   IN        *DTAARA
039600020923e   1C                   ENDIF
039700020923     C*
039800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
039900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
040000020923     C*
040100020923     C                   ENDSR
040200970526     C*------------------------------------------------------------------------*
040300970526     C* *INZSR - OPERAZIONI INIZIALI
040400970526     C*------------------------------------------------------------------------*
040500970526     C     *INZSR        BEGSR
040600991217     C*
040700991217     C     *ENTRY        PLIST
040800991217     C                   PARM                    KPJBA
040900020923     C*
041000020923     C* REPERISCE I DATI DELL'UTENTE
041100020923     C                   EXSR      REPDATIUTE
041200991215     C*
041300000306     C* LETTURA cnaco31c/cnaco00f
041400991215     C     keycli        KLIST                                                  *di cnaco/ind/clp
041500991215     C                   KFLD                    kclkut            1 0          -utente
041600991215     C                   KFLD                    kclkcc            4 0          -capoconto
041700991215     C                   KFLD                    kclksc            7 0          -codice
041800991216     C                   z-add     1             kclkut
041900970526     C*
042000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
042100970526     C     XMSGPA        PLIST
042200970526     C                   PARM                    DSMSMS
042300970526     C                   PARM                    DSMSPG
042400970526     C                   PARM                    DSMSKE
042500970526     C                   PARM                    DSMSER
042600970526     C*
042700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
042800970526     C                   CALL      'XKEY'
042900970526     C                   PARM                    DSKEY
043000970526     C*---
043100970526     C* VARIABILI NON RIFERITE AL DATA BASE
043200970526     C*---
043300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
043400991214     C                   MOVEL     *BLANKS       DEPOP1            3
043500970526     C*---
043600970526     C* VARIABILI DI CONTROLLO
043700970526     C*---
043800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
043900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
044000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
044100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
044200970526     C*
044300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
044400030414     C                   MOVEL     'TIST56R'     DSMSPG
044500030414     C                   MOVEL     'TIST56R'     MSGQUE
044600970526     C                   MOVEL     *BLANKS       MSGKEY
044700970526     C                   MOVEL     *BLANKS       DSMSMS
044800970528     C*
044900970528     C* CALCOLA LA DATA CORRENTE
045000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045200970528     C                   Z-ADD     WN8           G08DAT
045300970528     C                   Z-ADD     *ZEROS        G08INV
045400970528     C                   MOVEL     '0'           G08ERR
045500970528     C                   CALL      'XSRDA8'
045600970528     C                   PARM                    WLBDA8
045700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
045800000310     C*
045900000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
046000000310     C                   EXSR      AZZVID
046100970526     C*
046200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
046300970526     C                   EXSR      IMPFUN
046400970526     C*
046500970526     C                   ENDSR
046600970526     C*------------------------------------------------------------------------*
046700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
046800970526     C*------------------------------------------------------------------------*
046900970526     C     FUNGES        BEGSR
047000970526     C*
047100970526     C* PULISCO CAMPI DI LAVORO
047200970526     C                   Z-ADD     0             W                 5 0
047300970526     C*
047400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
047500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
047600970526     C     1             DO        24            I
047700970526     C                   MOVEL     FUD(I)        FUDRIG
047800970526     C*
047900970526     C* PULIZIA FUNZIONI PRECEDENTI
048000970526     C                   MOVEL     *BLANKS       FUV(I)
048100970526     C*
048200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
048300970526     C* (SALTA F24)
048400970526     C     FUDATT        IFEQ      'S'
048500970526     C     FUDFUN        ANDNE     '24'
048600970526     C                   ADD       1             W
048700970526     C                   MOVEL     FUDDES        FUV(W)
048800970526     C                   END
048900970526     C                   END                                                    DO WHILE
049000970526     C*
049100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
049200970526     C* ALTRIMENTI DISATTIVO F24
049300970526     C     1             DO        24            I
049400970526     C                   MOVEL     FUD(I)        FUDRIG
049500970526     C*
049600970526     C     FUDFUN        IFEQ      '24'
049700970526     C     W             IFGT      6
049800970526     C                   MOVEL     'S'           FUDATT
049900970526     C                   ELSE
050000970526     C                   MOVEL     *BLANKS       FUDATT
050100970526     C                   END                                                    W GT 8
050200970526     C                   MOVEL     FUDRIG        FUD(I)
050300970526     C                   END
050400970526     C                   END                                                    DO WHILE
050500970526     C*
050600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
050700970526     C     W             DIV       6             I
050800970526     C                   MVR                     Z
050900970526     C*
051000970526     C     Z             IFNE      0
051100970526     C     I             ADD       1             Z
051200970526     C                   ELSE
051300970526     C                   Z-ADD     I             Z                 5 0
051400970526     C                   END
051500970526     C*
051600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
051700970526     C                   ADD       1             FUNGRU
051800970526     C     FUNGRU        IFGT      Z
051900970526     C     FUNGRU        ORLT      1
052000970526     C                   Z-ADD     1             FUNGRU
052100970526     C                   END
052200970526     C*
052300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
052400970526     C     FUNGRU        IFEQ      1
052500970526     C                   MOVEL     FUV(1)        FUN001
052600970526     C                   MOVEL     FUV(2)        FUN002
052700970526     C                   MOVEL     FUV(3)        FUN003
052800970526     C                   MOVEL     FUV(4)        FUN004
052900970526     C                   MOVEL     FUV(5)        FUN005
053000970526     C                   MOVEL     FUV(6)        FUN006
053100970526     C                   END
053200970526     C*
053300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
053400970526     C     FUNGRU        IFEQ      2
053500970526     C                   MOVEL     FUV(7)        FUN001
053600970526     C                   MOVEL     FUV(8)        FUN002
053700970526     C                   MOVEL     FUV(9)        FUN003
053800970526     C                   MOVEL     FUV(10)       FUN004
053900970526     C                   MOVEL     FUV(11)       FUN005
054000970526     C                   MOVEL     FUV(12)       FUN006
054100970526     C                   END
054200970526     C*
054300970526     C* TERZO GRUPPO DI 6 FUNZIONI
054400970526     C     FUNGRU        IFEQ      3
054500970526     C                   MOVEL     FUV(13)       FUN001
054600970526     C                   MOVEL     FUV(14)       FUN002
054700970526     C                   MOVEL     FUV(15)       FUN003
054800970526     C                   MOVEL     FUV(16)       FUN004
054900970526     C                   MOVEL     FUV(17)       FUN005
055000970526     C                   MOVEL     FUV(18)       FUN006
055100970526     C                   END
055200970526     C*
055300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
055400970526     C     FUNGRU        IFEQ      4
055500970526     C                   MOVEL     FUV(19)       FUN001
055600970526     C                   MOVEL     FUV(20)       FUN002
055700970526     C                   MOVEL     FUV(21)       FUN003
055800970526     C                   MOVEL     FUV(22)       FUN004
055900970526     C                   MOVEL     *BLANKS       FUN005
056000970526     C                   MOVEL     *BLANKS       FUN006
056100970526     C                   END
056200970526     C*
056300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
056400970526     C                   MOVEL     *BLANKS       FUN007
056500970526     C                   MOVEL     *BLANKS       FUN008
056600970526     C*
056700970526     C     1             DO        24            I
056800970526     C                   MOVEL     FUD(I)        FUDRIG
056900970526     C     FUDFUN        IFEQ      '24'
057000970526     C     FUDATT        ANDEQ     'S'
057100970526     C                   MOVEL     FUDDES        FUN008
057200970526     C                   END
057300970526     C                   END                                                    DO
057400970526     C*
057500970526     C                   ENDSR
057600970526     C*------------------------------------------------------------------------*
057700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
057800970526     C*------------------------------------------------------------------------*
057900970526     C     FUNCON        BEGSR
058000970526     C*
058100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
058200970526     C                   EXSR      FUNVAR
058300970526     C*
058400970526     C* PULISCO CAMPI DI LAVORO
058500970526     C                   Z-ADD     0             I                 5 0
058600970526     C                   Z-ADD     0             W                 5 0
058700970526     C*
058800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
058900970526     C     FUNKEY        IFNE      *BLANKS
059000970526     C     1             DO        24            I
059100970526     C                   MOVEL     FUD(I)        FUDRIG
059200970526     C*
059300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
059400970526     C     FUNKEY        IFEQ      FUDFUN
059500970526     C     FUDATT        ANDEQ     'S'
059600970526     C                   Z-ADD     I             W
059700970526     C                   END
059800970526     C                   END                                                    DO WHILE
059900970526     C*
060000970526     C* FUNZIONE NON ATTIVA (ERRORE)
060100970526     C     W             IFEQ      0
060200970526     C*
060300970526     C* EMISSIONE MESSAGGIO DI ERRORE
060400970526     C                   MOVEL     MSG(1)        DSMSMS
060500970526     C*
060600970526     C                   END                                                    W=0
060700970526     C                   END                                                    FUNKEY NE *BLANKS
060800970526     C*
060900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
061000970526     C     W             IFNE      0
061100970526     C     FUNKEY        OREQ      *BLANKS
061200970526     C     FUNKEY        IFEQ      *BLANKS
061300970526     C                   EXSR      ENTGES
061400970526     C                   END
061500970526     C     FUNKEY        IFEQ      '03'
061600970526     C                   EXSR      F03GES
061700970526     C                   END
061800970526     C     FUNKEY        IFEQ      '06'
061900970526     C                   EXSR      F06GES
062000970526     C                   END
062100970526     C     FUNKEY        IFEQ      '12'
062200970526     C                   EXSR      F12GES
062300970526     C                   END
062400970526     C     FUNKEY        IFEQ      '24'
062500970526     C                   EXSR      F24GES
062600970526     C                   END
062700970526     C                   END
062800970526     C*
062900970526     C                   ENDSR
063000970526     C*------------------------------------------------------------------------*
063100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
063200970526     C*------------------------------------------------------------------------*
063300970526     C     FUNVAR        BEGSR
063400970526     C*
063500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
063600970526     C*
063700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
063800970526     C     KEY           IFEQ      ENTER
063900970526     C                   MOVEL     *BLANKS       FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      F1
064200970526     C                   MOVEL     '01'          FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F2
064500970526     C                   MOVEL     '02'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F3
064800970526     C                   MOVEL     '03'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F4
065100970526     C                   MOVEL     '04'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F5
065400970526     C                   MOVEL     '05'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F6
065700970526     C                   MOVEL     '06'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F7
066000970526     C                   MOVEL     '07'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F8
066300970526     C                   MOVEL     '08'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F9
066600970526     C                   MOVEL     '09'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F10
066900970526     C                   MOVEL     '10'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F11
067200970526     C                   MOVEL     '11'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F12
067500970526     C                   MOVEL     '12'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F13
067800970526     C                   MOVEL     '13'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F14
068100970526     C                   MOVEL     '14'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F15
068400970526     C                   MOVEL     '15'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F16
068700970526     C                   MOVEL     '16'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      F17
069000970526     C                   MOVEL     '17'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      F18
069300970526     C                   MOVEL     '18'          FUNKEY
069400970526     C                   END
069500970526     C     KEY           IFEQ      F19
069600970526     C                   MOVEL     '19'          FUNKEY
069700970526     C                   END
069800970526     C     KEY           IFEQ      F20
069900970526     C                   MOVEL     '20'          FUNKEY
070000970526     C                   END
070100970526     C     KEY           IFEQ      F21
070200970526     C                   MOVEL     '21'          FUNKEY
070300970526     C                   END
070400970526     C     KEY           IFEQ      F22
070500970526     C                   MOVEL     '22'          FUNKEY
070600970526     C                   END
070700970526     C     KEY           IFEQ      F23
070800970526     C                   MOVEL     '23'          FUNKEY
070900970526     C                   END
071000970526     C     KEY           IFEQ      F24
071100970526     C                   MOVEL     '24'          FUNKEY
071200970526     C                   END
071300970526     C     KEY           IFEQ      ROLLUP
071400970526     C                   MOVEL     'RU'          FUNKEY
071500970526     C                   END
071600970526     C     KEY           IFEQ      HELP
071700970526     C                   MOVEL     'HP'          FUNKEY
071800970526     C                   END
071900970526     C                   ENDSR
072000970526     C*------------------------------------------------------------------------*
072100970526     C* NOTE COSTRUZIONE SCHIERE
072200970526     C*------------------------------------------------------------------------*
072300970526     C*
072400970526     C* -MSG-MESSAGGI
072500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
072600970526     C* (NON TOCCARE MSG,1 E MSG,2)
072700970526     C*
072800970526     C* -CMD-COMANDI
072900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
073000970526     C*
073100970526     C* -OPD-OPZIONI
073200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
073300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
073400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
073500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
073600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
073700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
073800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
073900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
074000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
074100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
074200970526     C*
074300970526     C* -FUD-FUNZIONI
074400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
074500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
074600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
074700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
074800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
074900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
075000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
075100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
075200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
075300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
075400970526     O*------------------------------------------------------------------------*
075500970526** MSG - MESSAGGI
075600030414TIST56R- Tasto funzionale non gestito                                          1
075700030414TIST56R-                                                                       2
075800030414TIST56R-                                                                       3
075900030414TIST56R-                                                                       4
076000030414TIST56R-                                                                       5
076100030414TIST56R-                                                                       6
076200030414TIST56R-                                                                       7
076300030414TIST56R-                                                                       8
076400030414TIST56R-                                                                       9
076500030414TIST56R-                                                                       10
076600030414TIST56R-                                                                       11
076700030414TIST56R-                                                                       12
076800030414TIST56R-                                                                       13
076900030414TIST56R- Data errata                                                           14
077000030414TIST56R- La data inizio deve essere maggiore della data fine                   15
077100030414TIST56R- Cliente inserito inesistente.                                         16
077200030414TIST56R- Coda di stampa inesistente.                                           17
077300030414TIST56R-                                                                       18
077400030414TIST56R-                                                                       19
077500030414TIST56R-                                                                       20
077600030414TIST56R- Scelta obbligatoria.                                                  21
077700030414TIST56R-                                                                       22
077800030414TIST56R-                                                                       23
077900030414TIST56R-                                                                       24
078000970526** FUD - FUNZIONI
078100970526                   -        (S/N)=N                        funz.01¬
078200970526                   -        (S/N)=N                        funz.02¬
078300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
078400970526                   -        (S/N)=N                        funz.04¬
078500970526                   -        (S/N)=N                        funz.05¬
078600991214F06=Conferma       -        (S/N)=S                        funz.06¬
078700970527                   -        (S/N)=N                        funz.07¬
078800970527                   -        (S/N)=N                        funz.08¬
078900970527                   -        (S/N)=N                        funz.09¬
079000970527                   -        (S/N)=N                        funz.10¬
079100970526                   -        (S/N)=N                        funz.11¬
079200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
079300970526                   -        (S/N)=N                        funz.13¬
079400970526                   -        (S/N)=N                        funz.14¬
079500970526                   -        (S/N)=N                        funz.15¬
079600970526                   -        (S/N)=N                        funz.16¬
079700970526                   -        (S/N)=N                        funz.17¬
079800970526                   -        (S/N)=N                        funz.18¬
079900970526                   -        (S/N)=N                        funz.19¬
080000970526                   -        (S/N)=N                        funz.20¬
080100970526                   -        (S/N)=N                        funz.21¬
080200970526                   -        (S/N)=N                        funz.22¬
080300970526                   -        (S/N)=N                        funz.23¬
080400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
