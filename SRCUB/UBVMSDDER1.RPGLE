000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400170523     h dftactgrp(*no)
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900160901     FUBVMSDDED1cf   e             WORKSTN INFDS(DEVDS1)
001000160901     F                                     SFILE(VMSDDEDMS:MSGULT)
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
006900120928     D*-------------------
007000120928
007100121003     d Status         sds
007200121003     d  SDSpgm           *proc
007300121003     d  SDSprm           *parms
007400121003     d  SDSuser              254    263
007500121003
007600160901       /copy gaitrasrc/srcProtoPI,UBVMSDDER
007700160901       /copy gaitrasrc/srcProtoPR,UBVMSDDER
007800120928
007900970526     C*------------------------------------------------------------------------*
008000000303     C* MAIN LINES
008100970526     C*------------------------------------------------------------------------*
008200970526     C*
008300120927     C* metto i default su D03
008400120927     C                   EXSR      InzD03
008500120927     C*
008600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009000970526     C*
009100970526     C* EMETTE FORMATO VIDEO
009200970526     C                   EXSR      BUTFMR
009300970526     C*
009400970526     C* GESTIONE TASTI FUNZIONALI
009500970526     C                   EXSR      FUNCON
009600970526E   2C                   ENDDO
009700970526     C*
009800970526     C* FINE PGM
009900970526     C                   SETON                                        LR
010000970526     C*------------------------------------------------------------------------*
010100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010200970526     C*------------------------------------------------------------------------*
010300970526     C     BUTFMR        BEGSR
010400970526     C*
010500970526     C* EMETTE LA TESTATA
010600160901     C                   WRITE     VMSDDED01
010700970526     C*
010800970526     C* SPEDISCE EVENTUALE MESSAGGIO
010900970526     C                   EXSR      SNDMSG
011000970526     C*
011100970526     C* EMETTE LE FUNZIONI
011200160901     C                   WRITE     VMSDDED02
011300970526     C*
011400970526     C* EMETTE IL VIDEO
011500160901     C                   EXFMT     VMSDDED03
011600970526     C*
011700991214     C* REIMPOSTA FLAG DI ERRORE
011800991214     C                   MOVEL     '0'           ERRL00
011900970526     C*
012000970526     C* REIMPOSTA INDICATORI DI ERRORE
012100020611     C                   SETOFF                                       555556
012200970526     C*
012300970526     C                   ENDSR
012400970526     C*------------------------------------------------------------------------*
012500120927     C* InzD03 - Inizializzo D03
012600970526     C*------------------------------------------------------------------------*
012700120927     C     InzD03        BEGSR
012800970526     C*
012900160901     C                   clear                   videsito
013000160901     C                   clear                   viddde
013100120927     C*
013200120927     C                   ENDSR
013300120927     C*------------------------------------------------------------------------*
013400120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500120927     C*------------------------------------------------------------------------*
013600120927     C     SNDMSG        BEGSR
013700120927     C*
013800970526     C* PULISCE IL SFILE MESSAGGI
013900970526     C                   MOVEL     '1'           *IN40
014000160901     C                   WRITE     VMSDDEDMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600020611     C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800020611     C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000160901     C                   WRITE     VMSDDEDMS
015100160901     C                   WRITE     VMSDDEDMC
015200970526     C*
015300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970526     C                   MOVEL     *BLANKS       DSMSMS
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* ENTGES - GESTIONE ENTER -> INVIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     ENTGES        BEGSR
016100970526     C*
016200970526     C* CONTROLLA I CAMPI IMMESSI
016300970526     C                   EXSR      DECODI
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F03GES - GESTIONE F03 -> FINE
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F03GES        BEGSR
017000970526     C*
017100970526     C* IMPOSTA "PREMUTO F03"
017200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600991214     C* F06GES - GESTIONE F06 -> CONFERMA
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F06GES        BEGSR
017900970526     C*
018000991214     C* CONTROLLA I CAMPI IMMESSI
018100991214     C                   EXSR      DECODI
018200020611     C*
018300991214     C* ESEGUE ROUTINE DI CONFERMA
018400020611     C     ERRL00        IFEQ      '0'
018500991214     C                   EXSR      ESEGUI
018600160901     C* se la chiamata è andata a buon fine emetto messaggio
018700160921     C                   IF        oVMSDDEEsito = '1'
018800121109     C                   MOVEL     MSG(20)       DSMSMS
018900121109     C                   ENDIF
019000121109     C* non esco mai dal pgm
019100121109     C***                MOVEL     '1'           FINL00
019200121108     C                   ENDIF
019300970526     C*
019400991214     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F12GES - GESTIONE F12 -> RITORNO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F12GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F12"
020100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F24GES        BEGSR
020800970526     C*
020900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
021400970526     C*------------------------------------------------------------------------*
021500991214     C     ESEGUI        BEGSR
021600121108     C*
021700160921     C                   EVAL      iVMSDDEProc    = VIDProc
021800160921     C                   EVAL      iVMSDDEKSU     = VIDKSU
021900160921     C                   EVAL      iVMSDDEDtRif   = VIDDtRif
022000121108     C* chiamo
022100160901     C                   CALL      'UBVMSDDER'
022200160921     C                   PARM      *blank        iVMSDDEOpz
022300160921     C                   PARM                    iVMSDDEProc
022400160921     C                   PARM                    iVMSDDEKSU
022500160921     C                   PARM                    iVMSDDEDtRif
022600160921     c                   parm                    oVMSDDEDDE
022700160921     c                   parm                    oVMSDDEEsito
022800121108     C*
022900160921     c                   EVAL      VIDEsito   = oVMSDDEEsito
023000160921     c                   EVAL      VIDDDE     = oVMSDDEDDE
023100991214     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023500970526     C*------------------------------------------------------------------------*
023600970526     C     DECODI        BEGSR
023700121019     C*
023800120927     C                   SETOFF                                       505152
023900120927     C                   SETOFF                                       535455
024000120927     C                   SETOFF                                       565758
024100121019     C                   SETOFF                                       596061
024200000306     C*
024300120927     C*
024400970528     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
024700970526     C*------------------------------------------------------------------------*
024800970526     C     AZZVID        BEGSR
024900970526     C* CAMPI
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
025400970526     C*------------------------------------------------------------------------*
025500970526     C     IMPFUN        BEGSR
025600970526     C*
025700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
025800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
025900970526     C                   EXSR      FUNGES
026000970526     C*
026100970526     C                   ENDSR
026200020923     C*--------------------------------------------------------------------------------------------*
026300020923     C* REPERISCE I DATI UTENTE
026400020923     C*--------------------------------------------------------------------------------------------*
026500020923     C     REPDATIUTE    BEGSR
026600020923     C*
026700020923     C* INIZIALIZZA VARIABILI DI WRK
026800020923     C                   CLEAR                   TIBS34DS
026900020923     C                   CLEAR                   AZUTEDS
027000020923     C                   CLEAR                   DDATIUTE
027100020923     C*
027200020923     C     *DTAARA       DEFINE    §azute        azuteds
027300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
027400020923     C                   IN(E)     *DTAARA
027500020923if  1C                   IF        %Error
027600020923     c                   EVAL      I34Tla = 'L'
027700020923     C                   CALL      'TIBS34R'
027800020923     C                   PARM                    Tibs34Ds
027900020923     C                   IN        *DTAARA
028000020923e   1C                   ENDIF
028100020923     C*
028200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
028300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
028400020923     C*
028500020923     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700970526     C* *INZSR - OPERAZIONI INIZIALI
028800970526     C*------------------------------------------------------------------------*
028900970526     C     *INZSR        BEGSR
029000991217     C*
029100991217     C     *ENTRY        PLIST
029200991217     C                   PARM                    KPJBA
029300020923     C*
029400020923     C* REPERISCE I DATI DELL'UTENTE
029500020923     C                   EXSR      REPDATIUTE
029600970526     C*
029700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
029800970526     C     XMSGPA        PLIST
029900970526     C                   PARM                    DSMSMS
030000970526     C                   PARM                    DSMSPG
030100970526     C                   PARM                    DSMSKE
030200970526     C                   PARM                    DSMSER
030300970526     C*
030400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
030500970526     C                   CALL      'XKEY'
030600970526     C                   PARM                    DSKEY
030700970526     C*---
030800970526     C* VARIABILI NON RIFERITE AL DATA BASE
030900970526     C*---
031000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
031100991214     C                   MOVEL     *BLANKS       DEPOP1            3
031200970526     C*---
031300970526     C* VARIABILI DI CONTROLLO
031400970526     C*---
031500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
031600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
031700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
031800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
031900970526     C*
032000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
032100160901     C                   MOVEL     'UBVMSDDER1'  DSMSPG
032200160901     C                   MOVEL     'UBVMSDDER1'  MSGQUE
032300970526     C                   MOVEL     *BLANKS       MSGKEY
032400970526     C                   MOVEL     *BLANKS       DSMSMS
032500970528     C*
032600970528     C* CALCOLA LA DATA CORRENTE
032700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
032800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
032900970528     C                   Z-ADD     WN8           G08DAT
033000970528     C                   Z-ADD     *ZEROS        G08INV
033100970528     C                   MOVEL     '0'           G08ERR
033200970528     C                   CALL      'XSRDA8'
033300970528     C                   PARM                    WLBDA8
033400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
033500000310     C*
033600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
033700000310     C                   EXSR      AZZVID
033800970526     C*
033900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
034000970526     C                   EXSR      IMPFUN
034100970526     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
034500970526     C*------------------------------------------------------------------------*
034600970526     C     FUNGES        BEGSR
034700970526     C*
034800970526     C* PULISCO CAMPI DI LAVORO
034900970526     C                   Z-ADD     0             W                 5 0
035000970526     C*
035100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
035200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
035300970526     C     1             DO        24            I
035400970526     C                   MOVEL     FUD(I)        FUDRIG
035500970526     C*
035600970526     C* PULIZIA FUNZIONI PRECEDENTI
035700970526     C                   MOVEL     *BLANKS       FUV(I)
035800970526     C*
035900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
036000970526     C* (SALTA F24)
036100970526     C     FUDATT        IFEQ      'S'
036200970526     C     FUDFUN        ANDNE     '24'
036300970526     C                   ADD       1             W
036400970526     C                   MOVEL     FUDDES        FUV(W)
036500970526     C                   END
036600970526     C                   END                                                    DO WHILE
036700970526     C*
036800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
036900970526     C* ALTRIMENTI DISATTIVO F24
037000970526     C     1             DO        24            I
037100970526     C                   MOVEL     FUD(I)        FUDRIG
037200970526     C*
037300970526     C     FUDFUN        IFEQ      '24'
037400970526     C     W             IFGT      6
037500970526     C                   MOVEL     'S'           FUDATT
037600970526     C                   ELSE
037700970526     C                   MOVEL     *BLANKS       FUDATT
037800970526     C                   END                                                    W GT 8
037900970526     C                   MOVEL     FUDRIG        FUD(I)
038000970526     C                   END
038100970526     C                   END                                                    DO WHILE
038200970526     C*
038300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
038400970526     C     W             DIV       6             I
038500970526     C                   MVR                     Z
038600970526     C*
038700970526     C     Z             IFNE      0
038800970526     C     I             ADD       1             Z
038900970526     C                   ELSE
039000970526     C                   Z-ADD     I             Z                 5 0
039100970526     C                   END
039200970526     C*
039300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
039400970526     C                   ADD       1             FUNGRU
039500970526     C     FUNGRU        IFGT      Z
039600970526     C     FUNGRU        ORLT      1
039700970526     C                   Z-ADD     1             FUNGRU
039800970526     C                   END
039900970526     C*
040000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
040100970526     C     FUNGRU        IFEQ      1
040200970526     C                   MOVEL     FUV(1)        FUN001
040300970526     C                   MOVEL     FUV(2)        FUN002
040400970526     C                   MOVEL     FUV(3)        FUN003
040500970526     C                   MOVEL     FUV(4)        FUN004
040600970526     C                   MOVEL     FUV(5)        FUN005
040700970526     C                   MOVEL     FUV(6)        FUN006
040800970526     C                   END
040900970526     C*
041000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
041100970526     C     FUNGRU        IFEQ      2
041200970526     C                   MOVEL     FUV(7)        FUN001
041300970526     C                   MOVEL     FUV(8)        FUN002
041400970526     C                   MOVEL     FUV(9)        FUN003
041500970526     C                   MOVEL     FUV(10)       FUN004
041600970526     C                   MOVEL     FUV(11)       FUN005
041700970526     C                   MOVEL     FUV(12)       FUN006
041800970526     C                   END
041900970526     C*
042000970526     C* TERZO GRUPPO DI 6 FUNZIONI
042100970526     C     FUNGRU        IFEQ      3
042200970526     C                   MOVEL     FUV(13)       FUN001
042300970526     C                   MOVEL     FUV(14)       FUN002
042400970526     C                   MOVEL     FUV(15)       FUN003
042500970526     C                   MOVEL     FUV(16)       FUN004
042600970526     C                   MOVEL     FUV(17)       FUN005
042700970526     C                   MOVEL     FUV(18)       FUN006
042800970526     C                   END
042900970526     C*
043000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
043100970526     C     FUNGRU        IFEQ      4
043200970526     C                   MOVEL     FUV(19)       FUN001
043300970526     C                   MOVEL     FUV(20)       FUN002
043400970526     C                   MOVEL     FUV(21)       FUN003
043500970526     C                   MOVEL     FUV(22)       FUN004
043600970526     C                   MOVEL     *BLANKS       FUN005
043700970526     C                   MOVEL     *BLANKS       FUN006
043800970526     C                   END
043900970526     C*
044000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
044100970526     C                   MOVEL     *BLANKS       FUN007
044200970526     C                   MOVEL     *BLANKS       FUN008
044300970526     C*
044400970526     C     1             DO        24            I
044500970526     C                   MOVEL     FUD(I)        FUDRIG
044600970526     C     FUDFUN        IFEQ      '24'
044700970526     C     FUDATT        ANDEQ     'S'
044800970526     C                   MOVEL     FUDDES        FUN008
044900970526     C                   END
045000970526     C                   END                                                    DO
045100970526     C*
045200970526     C                   ENDSR
045300970526     C*------------------------------------------------------------------------*
045400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
045500970526     C*------------------------------------------------------------------------*
045600970526     C     FUNCON        BEGSR
045700970526     C*
045800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
045900970526     C                   EXSR      FUNVAR
046000970526     C*
046100970526     C* PULISCO CAMPI DI LAVORO
046200970526     C                   Z-ADD     0             I                 5 0
046300970526     C                   Z-ADD     0             W                 5 0
046400970526     C*
046500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
046600970526     C     FUNKEY        IFNE      *BLANKS
046700970526     C     1             DO        24            I
046800970526     C                   MOVEL     FUD(I)        FUDRIG
046900970526     C*
047000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
047100970526     C     FUNKEY        IFEQ      FUDFUN
047200970526     C     FUDATT        ANDEQ     'S'
047300970526     C                   Z-ADD     I             W
047400970526     C                   END
047500970526     C                   END                                                    DO WHILE
047600970526     C*
047700970526     C* FUNZIONE NON ATTIVA (ERRORE)
047800970526     C     W             IFEQ      0
047900970526     C*
048000970526     C* EMISSIONE MESSAGGIO DI ERRORE
048100970526     C                   MOVEL     MSG(1)        DSMSMS
048200970526     C*
048300970526     C                   END                                                    W=0
048400970526     C                   END                                                    FUNKEY NE *BLANKS
048500970526     C*
048600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
048700970526     C     W             IFNE      0
048800970526     C     FUNKEY        OREQ      *BLANKS
048900970526     C     FUNKEY        IFEQ      *BLANKS
049000970526     C                   EXSR      ENTGES
049100970526     C                   END
049200970526     C     FUNKEY        IFEQ      '03'
049300970526     C                   EXSR      F03GES
049400970526     C                   END
049500970526     C     FUNKEY        IFEQ      '06'
049600970526     C                   EXSR      F06GES
049700970526     C                   END
049800970526     C     FUNKEY        IFEQ      '12'
049900970526     C                   EXSR      F12GES
050000970526     C                   END
050100970526     C     FUNKEY        IFEQ      '24'
050200970526     C                   EXSR      F24GES
050300970526     C                   END
050400970526     C                   END
050500970526     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
050900970526     C*------------------------------------------------------------------------*
051000970526     C     FUNVAR        BEGSR
051100970526     C*
051200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
051300970526     C*
051400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
051500970526     C     KEY           IFEQ      ENTER
051600970526     C                   MOVEL     *BLANKS       FUNKEY
051700970526     C                   END
051800970526     C     KEY           IFEQ      F1
051900970526     C                   MOVEL     '01'          FUNKEY
052000970526     C                   END
052100970526     C     KEY           IFEQ      F2
052200970526     C                   MOVEL     '02'          FUNKEY
052300970526     C                   END
052400970526     C     KEY           IFEQ      F3
052500970526     C                   MOVEL     '03'          FUNKEY
052600970526     C                   END
052700970526     C     KEY           IFEQ      F4
052800970526     C                   MOVEL     '04'          FUNKEY
052900970526     C                   END
053000970526     C     KEY           IFEQ      F5
053100970526     C                   MOVEL     '05'          FUNKEY
053200970526     C                   END
053300970526     C     KEY           IFEQ      F6
053400970526     C                   MOVEL     '06'          FUNKEY
053500970526     C                   END
053600970526     C     KEY           IFEQ      F7
053700970526     C                   MOVEL     '07'          FUNKEY
053800970526     C                   END
053900970526     C     KEY           IFEQ      F8
054000970526     C                   MOVEL     '08'          FUNKEY
054100970526     C                   END
054200970526     C     KEY           IFEQ      F9
054300970526     C                   MOVEL     '09'          FUNKEY
054400970526     C                   END
054500970526     C     KEY           IFEQ      F10
054600970526     C                   MOVEL     '10'          FUNKEY
054700970526     C                   END
054800970526     C     KEY           IFEQ      F11
054900970526     C                   MOVEL     '11'          FUNKEY
055000970526     C                   END
055100970526     C     KEY           IFEQ      F12
055200970526     C                   MOVEL     '12'          FUNKEY
055300970526     C                   END
055400970526     C     KEY           IFEQ      F13
055500970526     C                   MOVEL     '13'          FUNKEY
055600970526     C                   END
055700970526     C     KEY           IFEQ      F14
055800970526     C                   MOVEL     '14'          FUNKEY
055900970526     C                   END
056000970526     C     KEY           IFEQ      F15
056100970526     C                   MOVEL     '15'          FUNKEY
056200970526     C                   END
056300970526     C     KEY           IFEQ      F16
056400970526     C                   MOVEL     '16'          FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F17
056700970526     C                   MOVEL     '17'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F18
057000970526     C                   MOVEL     '18'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F19
057300970526     C                   MOVEL     '19'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F20
057600970526     C                   MOVEL     '20'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F21
057900970526     C                   MOVEL     '21'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F22
058200970526     C                   MOVEL     '22'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F23
058500970526     C                   MOVEL     '23'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F24
058800970526     C                   MOVEL     '24'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      ROLLUP
059100970526     C                   MOVEL     'RU'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      HELP
059400970526     C                   MOVEL     'HP'          FUNKEY
059500970526     C                   END
059600970526     C                   ENDSR
059700970526     C*------------------------------------------------------------------------*
059800970526     C* NOTE COSTRUZIONE SCHIERE
059900970526     C*------------------------------------------------------------------------*
060000970526     C*
060100970526     C* -MSG-MESSAGGI
060200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
060300970526     C* (NON TOCCARE MSG,1 E MSG,2)
060400970526     C*
060500970526     C* -CMD-COMANDI
060600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
060700970526     C*
060800970526     C* -OPD-OPZIONI
060900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
061000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
061100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
061200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
061300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
061400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
061500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
061600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
061700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
061800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
061900970526     C*
062000970526     C* -FUD-FUNZIONI
062100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
062200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
062300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
062400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
062500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
062600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
062700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
062800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
062900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
063000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
063100970526     O*------------------------------------------------------------------------*
063200970526** MSG - MESSAGGI
063300120927Tasto funzionale non gestito                                                   1
063400120927Numero chiamate obbligatorio                                                   2
063500160901                                                                               3
063600121108                                                                               4
063700121108                                                                               5
063800120927Campo obbligatorio                                                             6
063900121108Il campo non deve essere valorizzato se tipi file FIVA*                        7
064000121108I flags sono obbligatori e vanno indicati con X                                8
064100120927                                                                               9
064200120927                                                                               10
064300120927                                                                               11
064400120927                                                                               12
064500120927                                                                               13
064600120927Data errata                                                                    14
064700120927La data inizio deve essere maggiore della data fine                            15
064800120927                                                                               16
064900120927                                                                               17
065000120927                                                                               18
065100120927                                                                               19
065200121109L'operazione si è conclusa positivamente                                       20
065300120927Scelta obbligatoria                                                            21
065400120927                                                                               22
065500121108                                                                               23
065600121108                                                                               24
065700970526** FUD - FUNZIONI
065800970526                   -        (S/N)=N                        funz.01¬
065900970526                   -        (S/N)=N                        funz.02¬
066000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
066100970526                   -        (S/N)=N                        funz.04¬
066200970526                   -        (S/N)=N                        funz.05¬
066300991214F06=Conferma       -        (S/N)=S                        funz.06¬
066400121108                   -        (S/N)=N                        funz.07¬
066500970527                   -        (S/N)=N                        funz.08¬
066600970527                   -        (S/N)=N                        funz.09¬
066700970527                   -        (S/N)=N                        funz.10¬
066800970526                   -        (S/N)=N                        funz.11¬
066900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
067000970526                   -        (S/N)=N                        funz.13¬
067100970526                   -        (S/N)=N                        funz.14¬
067200970526                   -        (S/N)=N                        funz.15¬
067300970526                   -        (S/N)=N                        funz.16¬
067400970526                   -        (S/N)=N                        funz.17¬
067500970526                   -        (S/N)=N                        funz.18¬
067600970526                   -        (S/N)=N                        funz.19¬
067700970526                   -        (S/N)=N                        funz.20¬
067800970526                   -        (S/N)=N                        funz.21¬
067900970526                   -        (S/N)=N                        funz.22¬
068000970526                   -        (S/N)=N                        funz.23¬
068100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
