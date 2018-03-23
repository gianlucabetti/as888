000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400170523     h dftactgrp(*no)
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900160920     FUBVMSB2CD1cf   e             WORKSTN INFDS(DEVDS1)
001000160920     F                                     SFILE(VMSB2CDMS:MSGULT)
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
007600160920       /copy gaitrasrc/srcProtoPI,UBVMSB2CR
007700160920       /copy gaitrasrc/srcProtoPR,UBVMSB2CR
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
010600160920     C                   WRITE     VMSB2CD01
010700970526     C*
010800970526     C* SPEDISCE EVENTUALE MESSAGGIO
010900970526     C                   EXSR      SNDMSG
011000970526     C*
011100970526     C* EMETTE LE FUNZIONI
011200160920     C                   WRITE     VMSB2CD02
011300970526     C*
011400970526     C* EMETTE IL VIDEO
011500160920     C                   EXFMT     VMSB2CD03
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
013100160920     C                   clear                   vidcautip
013200160920     C                   clear                   vidcautt
013300160920     C                   clear                   vidstscli
013400160920     C                   clear                   vidstsopz
013500160920     C                   clear                   vidfasop
013600160920     C                   clear                   viddesc
013700160920     C                   clear                   viddde
013800120927     C*
013900120927     C                   ENDSR
014000120927     C*------------------------------------------------------------------------*
014100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014200120927     C*------------------------------------------------------------------------*
014300120927     C     SNDMSG        BEGSR
014400120927     C*
014500970526     C* PULISCE IL SFILE MESSAGGI
014600970526     C                   MOVEL     '1'           *IN40
014700160920     C                   WRITE     VMSB2CDMC
014800970526     C                   MOVEL     '0'           *IN40
014900970526     C*
015000970526     C* VISUALIZZA IL MESSAGGIO
015100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015200970526     C                   CALL      'XMSG'        XMSGPA
015300020611     C     DSMSER        IFEQ      *ZEROS
015400970526     C                   MOVEL     DSMSKE        MSGKEY
015500020611     C                   ENDIF
015600970526     C                   Z-ADD     1             MSGULT
015700160920     C                   WRITE     VMSB2CDMS
015800160920     C                   WRITE     VMSB2CDMC
015900970526     C*
016000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016100970526     C                   MOVEL     *BLANKS       DSMSMS
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* ENTGES - GESTIONE ENTER -> INVIO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     ENTGES        BEGSR
016800970526     C*
016900970526     C* CONTROLLA I CAMPI IMMESSI
017000970526     C                   EXSR      DECODI
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300991214     C* F06GES - GESTIONE F06 -> CONFERMA
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F06GES        BEGSR
018600970526     C*
018700991214     C* CONTROLLA I CAMPI IMMESSI
018800991214     C                   EXSR      DECODI
018900020611     C*
019000991214     C* ESEGUE ROUTINE DI CONFERMA
019100020611     C     ERRL00        IFEQ      '0'
019200991214     C                   EXSR      ESEGUI
019300160901     C* se la chiamata è andata a buon fine emetto messaggio
019400160921     C                   IF        oVMSB2CEsito = '1'
019500121109     C                   MOVEL     MSG(20)       DSMSMS
019600121109     C                   ENDIF
019700121109     C* non esco mai dal pgm
019800121109     C***                MOVEL     '1'           FINL00
019900121108     C                   ENDIF
020000970526     C*
020100991214     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300970526     C* F12GES - GESTIONE F12 -> RITORNO
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F12GES        BEGSR
020600970526     C*
020700970526     C* IMPOSTA "PREMUTO F12"
020800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020900970526     C*
021000970526     C                   ENDSR
021100970526     C*------------------------------------------------------------------------*
021200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021300970526     C*------------------------------------------------------------------------*
021400970526     C     F24GES        BEGSR
021500970526     C*
021600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
022100970526     C*------------------------------------------------------------------------*
022200991214     C     ESEGUI        BEGSR
022300121108     C*
022400160921     C                   EVAL      iVMSB2CLingua  = VIDLingua
022500160921     C                   EVAL      iVMSB2CProc    = VIDProc
022600160921     C                   EVAL      iVMSB2CKSU     = VIDKSU
022700160921     C                   EVAL      iVMSB2CDtRif   = VIDDtRif
022800160921     C                   EVAL      iVMSB2CCauBRT  = VIDCauBRT
022900121108     C* chiamo
023000160920     C                   CALL      'UBVMSB2CR'
023100160921     C                   PARM      *blank        iVMSB2COpz
023200160921     c                   parm                    iVMSB2CDtRif
023300160921     c                   parm                    iVMSB2CLingua
023400160921     c                   parm                    iVMSB2CProc
023500160921     c                   parm                    iVMSB2CKSU
023600160921     c                   parm                    iVMSB2CCauBRT
023700160921     c                   parm                    oVMSB2CEsito
023800160921     c                   parm                    oVMSB2CCauTip
023900160921     c                   parm                    oVMSB2CCauTT
024000160921     c                   parm                    oVMSB2CStsCli
024100160921     c                   parm                    oVMSB2CStsOpz
024200160921     c                   parm                    oVMSB2CFasOp
024300160921     c                   parm                    oVMSB2CDDE
024400160921     c                   parm                    oVMSB2CDesc
024500121108     C*
024600160921     c                   EVAL      VIDEsito   = oVMSB2CEsito
024700160921     c                   EVAL      VIDDDE     = oVMSB2CDDE
024800160921     c                   EVAL      VIDCauTip  = oVMSB2CCauTip
024900160921     c                   EVAL      VIDCauTT   = oVMSB2CCauTT
025000160921     c                   EVAL      VIDStsCli  = oVMSB2CStsCli
025100160921     c                   EVAL      VIDStsOpz  = oVMSB2CStsOpz
025200160921     c                   EVAL      VIDFasOp   = oVMSB2CFasOp
025300160921     c                   EVAL      VIDDesc    = oVMSB2CDesc
025400160920     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     DECODI        BEGSR
026000121019     C*
026100120927     C                   SETOFF                                       505152
026200120927     C                   SETOFF                                       535455
026300120927     C                   SETOFF                                       565758
026400121019     C                   SETOFF                                       596061
026500000306     C*
026600120927     C*
026700970528     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
027000970526     C*------------------------------------------------------------------------*
027100970526     C     AZZVID        BEGSR
027200970526     C* CAMPI
027300970526     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
027700970526     C*------------------------------------------------------------------------*
027800970526     C     IMPFUN        BEGSR
027900970526     C*
028000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
028100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
028200970526     C                   EXSR      FUNGES
028300970526     C*
028400970526     C                   ENDSR
028500020923     C*--------------------------------------------------------------------------------------------*
028600020923     C* REPERISCE I DATI UTENTE
028700020923     C*--------------------------------------------------------------------------------------------*
028800020923     C     REPDATIUTE    BEGSR
028900020923     C*
029000020923     C* INIZIALIZZA VARIABILI DI WRK
029100020923     C                   CLEAR                   TIBS34DS
029200020923     C                   CLEAR                   AZUTEDS
029300020923     C                   CLEAR                   DDATIUTE
029400020923     C*
029500020923     C     *DTAARA       DEFINE    §azute        azuteds
029600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
029700020923     C                   IN(E)     *DTAARA
029800020923if  1C                   IF        %Error
029900020923     c                   EVAL      I34Tla = 'L'
030000020923     C                   CALL      'TIBS34R'
030100020923     C                   PARM                    Tibs34Ds
030200020923     C                   IN        *DTAARA
030300020923e   1C                   ENDIF
030400020923     C*
030500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
030600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
030700020923     C*
030800020923     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* *INZSR - OPERAZIONI INIZIALI
031100970526     C*------------------------------------------------------------------------*
031200970526     C     *INZSR        BEGSR
031300991217     C*
031400991217     C     *ENTRY        PLIST
031500991217     C                   PARM                    KPJBA
031600020923     C*
031700020923     C* REPERISCE I DATI DELL'UTENTE
031800020923     C                   EXSR      REPDATIUTE
031900970526     C*
032000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
032100970526     C     XMSGPA        PLIST
032200970526     C                   PARM                    DSMSMS
032300970526     C                   PARM                    DSMSPG
032400970526     C                   PARM                    DSMSKE
032500970526     C                   PARM                    DSMSER
032600970526     C*
032700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
032800970526     C                   CALL      'XKEY'
032900970526     C                   PARM                    DSKEY
033000970526     C*---
033100970526     C* VARIABILI NON RIFERITE AL DATA BASE
033200970526     C*---
033300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
033400991214     C                   MOVEL     *BLANKS       DEPOP1            3
033500970526     C*---
033600970526     C* VARIABILI DI CONTROLLO
033700970526     C*---
033800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
033900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
034000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
034100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
034200970526     C*
034300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
034400160920     C                   MOVEL     'UBVMSB2CR1'  DSMSPG
034500160920     C                   MOVEL     'UBVMSB2CR1'  MSGQUE
034600970526     C                   MOVEL     *BLANKS       MSGKEY
034700970526     C                   MOVEL     *BLANKS       DSMSMS
034800970528     C*
034900970528     C* CALCOLA LA DATA CORRENTE
035000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
035100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
035200970528     C                   Z-ADD     WN8           G08DAT
035300970528     C                   Z-ADD     *ZEROS        G08INV
035400970528     C                   MOVEL     '0'           G08ERR
035500970528     C                   CALL      'XSRDA8'
035600970528     C                   PARM                    WLBDA8
035700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
035800000310     C*
035900000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
036000000310     C                   EXSR      AZZVID
036100970526     C*
036200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
036300970526     C                   EXSR      IMPFUN
036400970526     C*
036500970526     C                   ENDSR
036600970526     C*------------------------------------------------------------------------*
036700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
036800970526     C*------------------------------------------------------------------------*
036900970526     C     FUNGES        BEGSR
037000970526     C*
037100970526     C* PULISCO CAMPI DI LAVORO
037200970526     C                   Z-ADD     0             W                 5 0
037300970526     C*
037400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
037500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
037600970526     C     1             DO        24            I
037700970526     C                   MOVEL     FUD(I)        FUDRIG
037800970526     C*
037900970526     C* PULIZIA FUNZIONI PRECEDENTI
038000970526     C                   MOVEL     *BLANKS       FUV(I)
038100970526     C*
038200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
038300970526     C* (SALTA F24)
038400970526     C     FUDATT        IFEQ      'S'
038500970526     C     FUDFUN        ANDNE     '24'
038600970526     C                   ADD       1             W
038700970526     C                   MOVEL     FUDDES        FUV(W)
038800970526     C                   END
038900970526     C                   END                                                    DO WHILE
039000970526     C*
039100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
039200970526     C* ALTRIMENTI DISATTIVO F24
039300970526     C     1             DO        24            I
039400970526     C                   MOVEL     FUD(I)        FUDRIG
039500970526     C*
039600970526     C     FUDFUN        IFEQ      '24'
039700970526     C     W             IFGT      6
039800970526     C                   MOVEL     'S'           FUDATT
039900970526     C                   ELSE
040000970526     C                   MOVEL     *BLANKS       FUDATT
040100970526     C                   END                                                    W GT 8
040200970526     C                   MOVEL     FUDRIG        FUD(I)
040300970526     C                   END
040400970526     C                   END                                                    DO WHILE
040500970526     C*
040600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
040700970526     C     W             DIV       6             I
040800970526     C                   MVR                     Z
040900970526     C*
041000970526     C     Z             IFNE      0
041100970526     C     I             ADD       1             Z
041200970526     C                   ELSE
041300970526     C                   Z-ADD     I             Z                 5 0
041400970526     C                   END
041500970526     C*
041600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
041700970526     C                   ADD       1             FUNGRU
041800970526     C     FUNGRU        IFGT      Z
041900970526     C     FUNGRU        ORLT      1
042000970526     C                   Z-ADD     1             FUNGRU
042100970526     C                   END
042200970526     C*
042300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
042400970526     C     FUNGRU        IFEQ      1
042500970526     C                   MOVEL     FUV(1)        FUN001
042600970526     C                   MOVEL     FUV(2)        FUN002
042700970526     C                   MOVEL     FUV(3)        FUN003
042800970526     C                   MOVEL     FUV(4)        FUN004
042900970526     C                   MOVEL     FUV(5)        FUN005
043000970526     C                   MOVEL     FUV(6)        FUN006
043100970526     C                   END
043200970526     C*
043300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
043400970526     C     FUNGRU        IFEQ      2
043500970526     C                   MOVEL     FUV(7)        FUN001
043600970526     C                   MOVEL     FUV(8)        FUN002
043700970526     C                   MOVEL     FUV(9)        FUN003
043800970526     C                   MOVEL     FUV(10)       FUN004
043900970526     C                   MOVEL     FUV(11)       FUN005
044000970526     C                   MOVEL     FUV(12)       FUN006
044100970526     C                   END
044200970526     C*
044300970526     C* TERZO GRUPPO DI 6 FUNZIONI
044400970526     C     FUNGRU        IFEQ      3
044500970526     C                   MOVEL     FUV(13)       FUN001
044600970526     C                   MOVEL     FUV(14)       FUN002
044700970526     C                   MOVEL     FUV(15)       FUN003
044800970526     C                   MOVEL     FUV(16)       FUN004
044900970526     C                   MOVEL     FUV(17)       FUN005
045000970526     C                   MOVEL     FUV(18)       FUN006
045100970526     C                   END
045200970526     C*
045300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
045400970526     C     FUNGRU        IFEQ      4
045500970526     C                   MOVEL     FUV(19)       FUN001
045600970526     C                   MOVEL     FUV(20)       FUN002
045700970526     C                   MOVEL     FUV(21)       FUN003
045800970526     C                   MOVEL     FUV(22)       FUN004
045900970526     C                   MOVEL     *BLANKS       FUN005
046000970526     C                   MOVEL     *BLANKS       FUN006
046100970526     C                   END
046200970526     C*
046300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
046400970526     C                   MOVEL     *BLANKS       FUN007
046500970526     C                   MOVEL     *BLANKS       FUN008
046600970526     C*
046700970526     C     1             DO        24            I
046800970526     C                   MOVEL     FUD(I)        FUDRIG
046900970526     C     FUDFUN        IFEQ      '24'
047000970526     C     FUDATT        ANDEQ     'S'
047100970526     C                   MOVEL     FUDDES        FUN008
047200970526     C                   END
047300970526     C                   END                                                    DO
047400970526     C*
047500970526     C                   ENDSR
047600970526     C*------------------------------------------------------------------------*
047700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
047800970526     C*------------------------------------------------------------------------*
047900970526     C     FUNCON        BEGSR
048000970526     C*
048100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
048200970526     C                   EXSR      FUNVAR
048300970526     C*
048400970526     C* PULISCO CAMPI DI LAVORO
048500970526     C                   Z-ADD     0             I                 5 0
048600970526     C                   Z-ADD     0             W                 5 0
048700970526     C*
048800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
048900970526     C     FUNKEY        IFNE      *BLANKS
049000970526     C     1             DO        24            I
049100970526     C                   MOVEL     FUD(I)        FUDRIG
049200970526     C*
049300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
049400970526     C     FUNKEY        IFEQ      FUDFUN
049500970526     C     FUDATT        ANDEQ     'S'
049600970526     C                   Z-ADD     I             W
049700970526     C                   END
049800970526     C                   END                                                    DO WHILE
049900970526     C*
050000970526     C* FUNZIONE NON ATTIVA (ERRORE)
050100970526     C     W             IFEQ      0
050200970526     C*
050300970526     C* EMISSIONE MESSAGGIO DI ERRORE
050400970526     C                   MOVEL     MSG(1)        DSMSMS
050500970526     C*
050600970526     C                   END                                                    W=0
050700970526     C                   END                                                    FUNKEY NE *BLANKS
050800970526     C*
050900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
051000970526     C     W             IFNE      0
051100970526     C     FUNKEY        OREQ      *BLANKS
051200970526     C     FUNKEY        IFEQ      *BLANKS
051300970526     C                   EXSR      ENTGES
051400970526     C                   END
051500970526     C     FUNKEY        IFEQ      '03'
051600970526     C                   EXSR      F03GES
051700970526     C                   END
051800970526     C     FUNKEY        IFEQ      '06'
051900970526     C                   EXSR      F06GES
052000970526     C                   END
052100970526     C     FUNKEY        IFEQ      '12'
052200970526     C                   EXSR      F12GES
052300970526     C                   END
052400970526     C     FUNKEY        IFEQ      '24'
052500970526     C                   EXSR      F24GES
052600970526     C                   END
052700970526     C                   END
052800970526     C*
052900970526     C                   ENDSR
053000970526     C*------------------------------------------------------------------------*
053100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
053200970526     C*------------------------------------------------------------------------*
053300970526     C     FUNVAR        BEGSR
053400970526     C*
053500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
053600970526     C*
053700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
053800970526     C     KEY           IFEQ      ENTER
053900970526     C                   MOVEL     *BLANKS       FUNKEY
054000970526     C                   END
054100970526     C     KEY           IFEQ      F1
054200970526     C                   MOVEL     '01'          FUNKEY
054300970526     C                   END
054400970526     C     KEY           IFEQ      F2
054500970526     C                   MOVEL     '02'          FUNKEY
054600970526     C                   END
054700970526     C     KEY           IFEQ      F3
054800970526     C                   MOVEL     '03'          FUNKEY
054900970526     C                   END
055000970526     C     KEY           IFEQ      F4
055100970526     C                   MOVEL     '04'          FUNKEY
055200970526     C                   END
055300970526     C     KEY           IFEQ      F5
055400970526     C                   MOVEL     '05'          FUNKEY
055500970526     C                   END
055600970526     C     KEY           IFEQ      F6
055700970526     C                   MOVEL     '06'          FUNKEY
055800970526     C                   END
055900970526     C     KEY           IFEQ      F7
056000970526     C                   MOVEL     '07'          FUNKEY
056100970526     C                   END
056200970526     C     KEY           IFEQ      F8
056300970526     C                   MOVEL     '08'          FUNKEY
056400970526     C                   END
056500970526     C     KEY           IFEQ      F9
056600970526     C                   MOVEL     '09'          FUNKEY
056700970526     C                   END
056800970526     C     KEY           IFEQ      F10
056900970526     C                   MOVEL     '10'          FUNKEY
057000970526     C                   END
057100970526     C     KEY           IFEQ      F11
057200970526     C                   MOVEL     '11'          FUNKEY
057300970526     C                   END
057400970526     C     KEY           IFEQ      F12
057500970526     C                   MOVEL     '12'          FUNKEY
057600970526     C                   END
057700970526     C     KEY           IFEQ      F13
057800970526     C                   MOVEL     '13'          FUNKEY
057900970526     C                   END
058000970526     C     KEY           IFEQ      F14
058100970526     C                   MOVEL     '14'          FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F15
058400970526     C                   MOVEL     '15'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F16
058700970526     C                   MOVEL     '16'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F17
059000970526     C                   MOVEL     '17'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F18
059300970526     C                   MOVEL     '18'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      F19
059600970526     C                   MOVEL     '19'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      F20
059900970526     C                   MOVEL     '20'          FUNKEY
060000970526     C                   END
060100970526     C     KEY           IFEQ      F21
060200970526     C                   MOVEL     '21'          FUNKEY
060300970526     C                   END
060400970526     C     KEY           IFEQ      F22
060500970526     C                   MOVEL     '22'          FUNKEY
060600970526     C                   END
060700970526     C     KEY           IFEQ      F23
060800970526     C                   MOVEL     '23'          FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F24
061100970526     C                   MOVEL     '24'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      ROLLUP
061400970526     C                   MOVEL     'RU'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      HELP
061700970526     C                   MOVEL     'HP'          FUNKEY
061800970526     C                   END
061900970526     C                   ENDSR
062000970526     C*------------------------------------------------------------------------*
062100970526     C* NOTE COSTRUZIONE SCHIERE
062200970526     C*------------------------------------------------------------------------*
062300970526     C*
062400970526     C* -MSG-MESSAGGI
062500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
062600970526     C* (NON TOCCARE MSG,1 E MSG,2)
062700970526     C*
062800970526     C* -CMD-COMANDI
062900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
063000970526     C*
063100970526     C* -OPD-OPZIONI
063200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
063300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
063400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
063500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
063600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
063700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
063800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
063900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
064000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
064100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
064200970526     C*
064300970526     C* -FUD-FUNZIONI
064400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
064500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
064600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
064700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
064800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
064900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
065000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
065100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
065200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
065300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
065400970526     O*------------------------------------------------------------------------*
065500970526** MSG - MESSAGGI
065600120927Tasto funzionale non gestito                                                   1
065700120927Numero chiamate obbligatorio                                                   2
065800160901                                                                               3
065900121108                                                                               4
066000121108                                                                               5
066100120927Campo obbligatorio                                                             6
066200121108Il campo non deve essere valorizzato se tipi file FIVA*                        7
066300121108I flags sono obbligatori e vanno indicati con X                                8
066400120927                                                                               9
066500120927                                                                               10
066600120927                                                                               11
066700120927                                                                               12
066800120927                                                                               13
066900120927Data errata                                                                    14
067000120927La data inizio deve essere maggiore della data fine                            15
067100120927                                                                               16
067200120927                                                                               17
067300120927                                                                               18
067400120927                                                                               19
067500121109L'operazione si è conclusa positivamente                                       20
067600120927Scelta obbligatoria                                                            21
067700120927                                                                               22
067800121108                                                                               23
067900121108                                                                               24
068000970526** FUD - FUNZIONI
068100970526                   -        (S/N)=N                        funz.01¬
068200970526                   -        (S/N)=N                        funz.02¬
068300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
068400970526                   -        (S/N)=N                        funz.04¬
068500970526                   -        (S/N)=N                        funz.05¬
068600991214F06=Conferma       -        (S/N)=S                        funz.06¬
068700121108                   -        (S/N)=N                        funz.07¬
068800970527                   -        (S/N)=N                        funz.08¬
068900970527                   -        (S/N)=N                        funz.09¬
069000970527                   -        (S/N)=N                        funz.10¬
069100970526                   -        (S/N)=N                        funz.11¬
069200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
069300970526                   -        (S/N)=N                        funz.13¬
069400970526                   -        (S/N)=N                        funz.14¬
069500970526                   -        (S/N)=N                        funz.15¬
069600970526                   -        (S/N)=N                        funz.16¬
069700970526                   -        (S/N)=N                        funz.17¬
069800970526                   -        (S/N)=N                        funz.18¬
069900970526                   -        (S/N)=N                        funz.19¬
070000970526                   -        (S/N)=N                        funz.20¬
070100970526                   -        (S/N)=N                        funz.21¬
070200970526                   -        (S/N)=N                        funz.22¬
070300970526                   -        (S/N)=N                        funz.23¬
070400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
