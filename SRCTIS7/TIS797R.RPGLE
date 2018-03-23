000100970526     H*------------------------------------------------------------------------*
000200000714     H* Iscrizione clienti a strategi/FTP
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020226     Ftis797d   cf   e             WORKSTN INFDS(DEVDS1)
000900020226     F                                     SFILE(S797DMS:MSGULT)
001000970527     F*------------
001100020226     Ftivss02L  IF   E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D* DECODIFICHE
001600970526     D*------------
001700000714     D DEC             S             66    DIM(1) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100001116     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005500970528     D*------------------
005600970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700970528     D*------------------
005800000317     D WLBDA8          DS                  INZ
005900970528     D  G08DAT                 1      8  0
006000970528     D  G08INV                 9     16  0
006100970528     D  G08ERR                17     17
006200970528     D  G08TGI                18     22  0
006300970526     D*------------
006400970526     D* DS DI PROCEDURA
006500970526     D*------------
006600010115     D TIS720        E DS                  EXTNAME(TIS720DS)
006700020226     D TIS799DSI     E DS
006800970526     D*------------
006900970526     D* ARCHITETTURA
007000970526     D*------------
007100970526     D KPJBA         E DS
007200020226     D*------------
007300020226     D* REPERIMENTO DATI ANAGRAFICI CONTABILITA' CLIENTI
007400020226     D*------------
007500000714     DBS69DS         e ds                  extname(TIBS69DS)
007600000714     DACODS          e ds                  extname(cnaco00f)
007700000714     DINDDS          e ds                  extname(cnind00f)
007800000714     DCLPDS          e ds                  extname(cnclp00f)
007900000714     DCLSDS          e ds                  extname(fncls00f)
008000020226     D*------------
008100020226     D* PARAMETRI IN INGRESSO
008200020226     D*------------
008300020226     D PARAM           DS                  INZ                                  *Parametri di elab.
008400020226     D  PARKSU                        8                                          -cliente abilitato
008500020226     D  PARPOD                        1                                          -pari o dispari
008600000714
008700970526     C*------------------------------------------------------------------------*
008800970526     C* MAIN LINES
008900970526     C*------------------------------------------------------------------------*
009000970526     C*
009100970526     C* NON CI SONO ERRORI IN ENTRATA
009200001116     C     ERRL00        IFEQ      '0'
009300970526     C*
009400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009500001116     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009800970526     C*
009900970526     C* EMETTE FORMATO VIDEO
010000970526     C                   EXSR      BUTFMR
010100970526     C*
010200970526     C* GESTIONE TASTI FUNZIONALI
010300970526     C                   EXSR      FUNCON
010400001116     C                   ENDDO
010500970526     C*
010600970526     C* CI SONO ERRORI IN ENTRATA
010700001116     C                   ELSE
010800991104     C                   MOVEL     '1'           s20err
010900991104     C                   MOVEL     DSMSMS        s20msg
011000001116     C                   ENDIF
011100970526     C* FINE PGM
011200001116     C                   EXSR      FINSR
011300001116     C*
011400970526     C                   SETON                                        LR
011500970526     C*------------------------------------------------------------------------*
011600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011700970526     C*------------------------------------------------------------------------*
011800970526     C     BUTFMR        BEGSR
011900970526     C*
012000970526     C* EMETTE LA TESTATA
012100020226     C                   WRITE     S797D01
012200970526     C*
012300970526     C* SPEDISCE EVENTUALE MESSAGGIO
012400970526     C                   EXSR      SNDMSG
012500970526     C*
012600970526     C* EMETTE LE FUNZIONI
012700020226     C                   WRITE     S797D02
012800970526     C*
012900970526     C* EMETTE IL VIDEO
013000020226     C                   EXFMT     S797D03
013100970526     C*
013200970526     C* REIMPOSTA FLAG DI ERRORE
013300970526     C                   MOVEL     '0'           ERRL00
013400970526     C*
013500970526     C* REIMPOSTA INDICATORI DI ERRORE
013600020226     C                   SETOFF                                       61
013700970526     C*
013800970526     C                   ENDSR
013900970526     C*------------------------------------------------------------------------*
014000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014100970526     C*------------------------------------------------------------------------*
014200970526     C     SNDMSG        BEGSR
014300970526     C*
014400970526     C* PULISCE IL SFILE MESSAGGI
014500970526     C                   MOVEL     '1'           *IN40
014600020226     C                   WRITE     S797DMC
014700970526     C                   MOVEL     '0'           *IN40
014800970526     C*
014900970526     C* VISUALIZZA IL MESSAGGIO
015000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015100970526     C                   CALL      'XMSG'        XMSGPA
015200001116     C     DSMSER        IFEQ      *ZEROS
015300970526     C                   MOVEL     DSMSKE        MSGKEY
015400001116     C                   ENDIF
015500970526     C                   Z-ADD     1             MSGULT
015600020226     C                   WRITE     S797DMS
015700020226     C                   WRITE     S797DMC
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
017900991104     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300020226     C* F06GES - GESTIONE F06 -> INSERIMENTO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F06GES        BEGSR
018600020226     C*
018700020226     C* CONTROLLA I CAMPI IMMESSI
018800020226     C                   EXSR      DECODI
018900970526     C*
019000020226     C* SE NO ERRORI SOTTOMETTE IL PGM DI CREAZIONE "ESTRATTO BOLLE"
019100020226     C                   IF        ERRL00 = '0'
019200020226     C                   EVAL      PARKSU = VIDksu
019300020226     C                   IF        VIDpod <> *BLANKS
019400020226     C                   EVAL      PARPOD = VIDpod
019500020226     C                   ELSE
019600020226     C                   EVAL      PARPOD = 'X'
019700020226     C                   ENDIF
019800020226     C*
019900020226     C                   MOVEL     'S798'        KCOAZ
020000020226     C                   MOVEL     PARAM         KPJBU
020100020226     C                   CALL      'BCH10'
020200020226     C                   PARM                    KPJBA
020300020226     C*
020400020226     C* DOPO IL LANCIO ESCO
020500020226     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020600020226     C                   ENDIF
020700001116     C*
020800970526     C                   ENDSR
020900001116     C*------------------------------------------------------------------------*
021000020226     C* CLIABIL - PASSAGGIO A SCELTA ABILITAZIONE CLIENTI AI SERVIZI
021100001116     C*------------------------------------------------------------------------*
021200020226     C     CLIABIL       BEGSR
021300001116     C*
021400001116     C* PASSA ALLA GESTIONE ABILITAZIONI
021500001116     C                   CLEAR                   tis720
021600020226     C                   MOVEL     'P01'         s20OP0                         *LIVELLO PROCEDURA
021700001116     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
021800001116     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
021900001116     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
022000001116     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
022100020226     C                   MOVEL     'TT'          s20isv                         *TIPO LEGAME
022200001116     C*
022300001116     C                   MOVEL     tis720        KPJBU
022400001116     C                   CALL      'TIS724R'
022500001116     C                   PARM                    KPJBA
022600020226     C*
022700020226     C* RE-IMPOSTO LA DS RESTITUITA TRAMITE KPJBU
022800020226     C                   MOVEL     KPJBU         tis720
022900020226     C*
023000001116     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* F12GES - GESTIONE F12 -> RITORNO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     F12GES        BEGSR
023500970526     C*
023600970526     C* IMPOSTA "PREMUTO F12"
023700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023800991104     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024300970526     C*------------------------------------------------------------------------*
024400970526     C     F24GES        BEGSR
024500970526     C*
024600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000001116     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025100970526     C*------------------------------------------------------------------------*
025200970526     C     DECODI        BEGSR
025300000714     C*
025400020226     C                   SETOFF                                       61
025500020226     C                   EVAL      ERRL00 = '0'
025600001116     C*
025700970528     C*--------------------
025800000714     C* CLIENTE
025900970528     C*--------------------
026000020226     C     ERRL00        IFEQ      '0'
026100020226     C                   IF        VIDksu <> *BLANKS
026200020226     C     '?'           SCAN      VIDksu
026300020226     C                   IF        %found
026400020226     C                   EXSR      CLIABIL
026500020226     C                   EVAL      VIDksu = s20ksc
026600020226     C                   ELSE
026700020226     C                   EVAl      ERRL00 = '1'
026800001116     C                   EVAL      KCEksu = VIDksu
026900020226     C                   EVAL      KCEisv = 'TT'
027000001116     C     KE2vss        SETLL     tivss02l
027100001116     C     KE2vss        READE     tivss02l
027200020226     C                   DOW       NOT %EOF(tivss02l)
027300020226     C                   EVAL      ERRL00 = '0'
027400001116     C                   LEAVE
027500001116     C     KE2vss        READE     tivss02l
027600001116     C                   ENDDO
027700020226     C                   ENDIF
027800020226     C* VERIFICO SE C'E' 1 ANOMAIA
027900020226     C                   IF        ERRL00 = '1'
028000020226     C                   MOVEL     MSG(1)        DSMSMS
028100020226     C                   ELSE
028200020226     C* REPERISCO LA DESCRIZIONE DEL CLIENTE
028300020226     C                   EXSR      DECTIB
028400020226     C                   ENDIF
028500020226     C                   ENDIF
028600020226     C                   ENDIF
028700020226     C*--------------------
028800020226     C* VERIFICO CHE IL LANCIO SIA O X CLIENTE O X PARI/DISPARI
028900020226     C*--------------------
029000020226     C                   IF        VIDksu <> *BLANKS  AND
029100020226     C                             VIDpod <> *BLANKS
029200020226     C                   MOVEL     MSG(2)        DSMSMS
029300020226     C                   EVAL      ERRL00 = '0'
029400020226     C                   ENDIF
029500020226     C* ...ANALOGAMENTE
029600020226     C                   IF        VIDksu = *BLANKS  AND
029700020226     C                             VIDpod = *BLANKS
029800020226     C                   MOVEL     MSG(3)        DSMSMS
029900020226     C                   EVAL      ERRL00 = '0'
030000020226     C                   ENDIF
030100020226     C*
030200000714     C                   ENDSR
030300991210     C*------------------------------------------------------------------------*
030400020226     C* DECTIB - REPERIMENTO RAGIONE SOCIALE DEL CLIENTE
030500991210     C*------------------------------------------------------------------------*
030600000714     C     DECTIB        BEGSR
030700000714     C*
030800991210     C                   CLEAR                   BS69DS
030900991210     C                   CLEAR                   ACODS
031000991210     C                   MOVEL     KNSIF         I69SIF
031100020226     C                   MOVE(P)   VIDksu        I69KAC
031200991210     C                   CALL      'TIBS69R'
031300991210     C                   PARM                    BS69DS
031400991210     C                   PARM                    ACODS
031500991210     C                   PARM                    INDDS
031600991210     C                   PARM                    CLPDS
031700991210     C                   PARM                    CLSDS
031800991210     C     O69ERR        IFNE      '1'
031900020226     C                   MOVEL     ACORAG        VDDksu
032000991210     C                   ELSE
032100020226     C                   MOVEL     *ALL'*'       VDDksu
032200000714     C                   MOVE      '1'           ERRL00
032300991210     C                   ENDIF
032400000714     C*
032500000714     C                   ENDSR
032600991210     C*------------------------------------------------------------------------*
032700991210     C* FINSR - OPERAZIONI FINALI
032800991210     C*------------------------------------------------------------------------*
032900991210     C     FINSR         BEGSR
033000991210     C*
033100991210     C* CHIUDE I FILE DEI PGM CHIAMATI
033200991210     C                   CLEAR                   bs69ds
033300991210     C                   CLEAR                   acods
033400991210     C                   CLEAR                   indds
033500991210     C                   CLEAR                   clpds
033600991210     C                   CLEAR                   clsds
033700991210     C                   MOVEL     'C'           i69tla
033800020226     C                   MOVEL     KNSIF         i69sif
033900991210     C                   CALL      'TIBS69R'
034000991210     C                   PARM                    bs69ds
034100991210     C                   PARM                    acods
034200991210     C                   PARM                    indds
034300991210     C                   PARM                    clpds
034400991210     C                   PARM                    clsds
034500991210     C*
034600991210     C                   ENDSR
034700970526     C*------------------------------------------------------------------------*
034800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
034900970526     C*------------------------------------------------------------------------*
035000970526     C     AZZVID        BEGSR
035100020226     C*
035200970526     C* CAMPI
035300000714     C                   MOVEL     *BLANKS       VIDksu
035400020226     C                   MOVEL     *BLANKS       VIDpod
035500970526     C* DECODIFICHE
035600000714     C                   MOVEL     *BLANKS       VDDksu
035700970526     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
036100970526     C*------------------------------------------------------------------------*
036200970526     C     IMPFUN        BEGSR
036300970526     C*
036400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
036500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
036600970526     C                   EXSR      FUNGES
036700970526     C*
036800970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
036900970526     C                   MOVEL     DEC(1)        VIDFUN
037000970526     C*
037100970526     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* *INZSR - OPERAZIONI INIZIALI
037400970526     C*------------------------------------------------------------------------*
037500970526     C     *INZSR        BEGSR
037600970526     C*---
037700970526     C* RICEVIMENTO PARAMETRI
037800970526     C*---
037900970526     C     *ENTRY        PLIST
038000970612     C                   PARM                    KPJBA
038100970526     C*
038200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
038300970526     C     XMSGPA        PLIST
038400970526     C                   PARM                    DSMSMS
038500970526     C                   PARM                    DSMSPG
038600970526     C                   PARM                    DSMSKE
038700970526     C                   PARM                    DSMSER
038800970526     C*
038900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
039000970526     C                   CALL      'XKEY'
039100970526     C                   PARM                    DSKEY
039200970526     C*---
039300970526     C* VARIABILI NON RIFERITE AL DATA BASE
039400970526     C*---
039500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
039600000714     C     *LIKE         DEFINE    s20OP0        DEPOP0
039700000714     C     *LIKE         DEFINE    s20OP1        DEPOP1
039800000714     C     *LIKE         DEFINE    s20OP0        WOP0
039900000714     C     *LIKE         DEFINE    s20OP1        WOP1
040000970526     C*---
040100970526     C* VARIABILI DI CONTROLLO
040200970526     C*---
040300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
040400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
040500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
040600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
040700970526     C*
040800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
040900020226     C                   MOVEL     'TIS797R'     DSMSPG
041000020226     C                   MOVEL     'TIS797R'     MSGQUE
041100970526     C                   MOVEL     *BLANKS       MSGKEY
041200970526     C                   MOVEL     *BLANKS       DSMSMS
041300970526     C*---
041400970526     C* VARIABILI RIFERITE AL DATA BASE
041500970526     C*---
041600991104     C     *LIKE         DEFINE    vssisv        KCEisv
041700991105     C     *LIKE         DEFINE    vsssun        KCEsun
041800000317     C     *LIKE         DEFINE    vssksu        KCEksu
041900970526     C*---
042000970526     C* CHIAVI DI LETTURA
042100970526     C*---
042200001116     C* LETTURA tivss02L - COMPLETA
042300001116     C     KE2vss        KLIST
042400001116     C                   KFLD                    KCEksu
042500001116     C                   KFLD                    KCEisv
042600970526     C*
042700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
042800970526     C                   EXSR      IMPFUN
042900000131     C*
043000970526     C                   ENDSR
043100970526     C*------------------------------------------------------------------------*
043200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
043300970526     C*------------------------------------------------------------------------*
043400970526     C     FUNGES        BEGSR
043500970526     C*
043600970526     C* PULISCO CAMPI DI LAVORO
043700970526     C                   Z-ADD     0             W                 5 0
043800970526     C*
043900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
044000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
044100970526     C     1             DO        24            I
044200970526     C                   MOVEL     FUD(I)        FUDRIG
044300970526     C*
044400970526     C* PULIZIA FUNZIONI PRECEDENTI
044500970526     C                   MOVEL     *BLANKS       FUV(I)
044600970526     C*
044700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
044800970526     C* (SALTA F24)
044900970526     C     FUDATT        IFEQ      'S'
045000970526     C     FUDFUN        ANDNE     '24'
045100970526     C                   ADD       1             W
045200970526     C                   MOVEL     FUDDES        FUV(W)
045300970526     C                   END
045400970526     C                   END                                                    DO WHILE
045500970526     C*
045600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
045700970526     C* ALTRIMENTI DISATTIVO F24
045800970526     C     1             DO        24            I
045900970526     C                   MOVEL     FUD(I)        FUDRIG
046000970526     C*
046100970526     C     FUDFUN        IFEQ      '24'
046200970526     C     W             IFGT      6
046300970526     C                   MOVEL     'S'           FUDATT
046400970526     C                   ELSE
046500970526     C                   MOVEL     *BLANKS       FUDATT
046600970526     C                   END                                                    W GT 8
046700970526     C                   MOVEL     FUDRIG        FUD(I)
046800970526     C                   END
046900970526     C                   END                                                    DO WHILE
047000970526     C*
047100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
047200970526     C     W             DIV       6             I
047300970526     C                   MVR                     Z
047400970526     C*
047500970526     C     Z             IFNE      0
047600970526     C     I             ADD       1             Z
047700970526     C                   ELSE
047800970526     C                   Z-ADD     I             Z                 5 0
047900970526     C                   END
048000970526     C*
048100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
048200970526     C                   ADD       1             FUNGRU
048300970526     C     FUNGRU        IFGT      Z
048400970526     C     FUNGRU        ORLT      1
048500970526     C                   Z-ADD     1             FUNGRU
048600970526     C                   END
048700970526     C*
048800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
048900970526     C     FUNGRU        IFEQ      1
049000970526     C                   MOVEL     FUV(1)        FUN001
049100970526     C                   MOVEL     FUV(2)        FUN002
049200970526     C                   MOVEL     FUV(3)        FUN003
049300970526     C                   MOVEL     FUV(4)        FUN004
049400970526     C                   MOVEL     FUV(5)        FUN005
049500970526     C                   MOVEL     FUV(6)        FUN006
049600970526     C                   END
049700970526     C*
049800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
049900970526     C     FUNGRU        IFEQ      2
050000970526     C                   MOVEL     FUV(7)        FUN001
050100970526     C                   MOVEL     FUV(8)        FUN002
050200970526     C                   MOVEL     FUV(9)        FUN003
050300970526     C                   MOVEL     FUV(10)       FUN004
050400970526     C                   MOVEL     FUV(11)       FUN005
050500970526     C                   MOVEL     FUV(12)       FUN006
050600970526     C                   END
050700970526     C*
050800970526     C* TERZO GRUPPO DI 6 FUNZIONI
050900970526     C     FUNGRU        IFEQ      3
051000970526     C                   MOVEL     FUV(13)       FUN001
051100970526     C                   MOVEL     FUV(14)       FUN002
051200970526     C                   MOVEL     FUV(15)       FUN003
051300970526     C                   MOVEL     FUV(16)       FUN004
051400970526     C                   MOVEL     FUV(17)       FUN005
051500970526     C                   MOVEL     FUV(18)       FUN006
051600970526     C                   END
051700970526     C*
051800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
051900970526     C     FUNGRU        IFEQ      4
052000970526     C                   MOVEL     FUV(19)       FUN001
052100970526     C                   MOVEL     FUV(20)       FUN002
052200970526     C                   MOVEL     FUV(21)       FUN003
052300970526     C                   MOVEL     FUV(22)       FUN004
052400970526     C                   MOVEL     *BLANKS       FUN005
052500970526     C                   MOVEL     *BLANKS       FUN006
052600970526     C                   END
052700970526     C*
052800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
052900970526     C                   MOVEL     *BLANKS       FUN007
053000970526     C                   MOVEL     *BLANKS       FUN008
053100970526     C*
053200970526     C     1             DO        24            I
053300970526     C                   MOVEL     FUD(I)        FUDRIG
053400970526     C     FUDFUN        IFEQ      '24'
053500970526     C     FUDATT        ANDEQ     'S'
053600970526     C                   MOVEL     FUDDES        FUN008
053700970526     C                   END
053800970526     C                   END                                                    DO
053900970526     C*
054000970526     C                   ENDSR
054100970526     C*------------------------------------------------------------------------*
054200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
054300970526     C*------------------------------------------------------------------------*
054400970526     C     FUNCON        BEGSR
054500970526     C*
054600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
054700970526     C                   EXSR      FUNVAR
054800970526     C*
054900970526     C* PULISCO CAMPI DI LAVORO
055000970526     C                   Z-ADD     0             I                 5 0
055100970526     C                   Z-ADD     0             W                 5 0
055200970526     C*
055300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
055400970526     C     FUNKEY        IFNE      *BLANKS
055500970526     C     1             DO        24            I
055600970526     C                   MOVEL     FUD(I)        FUDRIG
055700970526     C*
055800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
055900970526     C     FUNKEY        IFEQ      FUDFUN
056000970526     C     FUDATT        ANDEQ     'S'
056100970526     C                   Z-ADD     I             W
056200970526     C                   END
056300970526     C                   END                                                    DO WHILE
056400970526     C*
056500970526     C* FUNZIONE NON ATTIVA (ERRORE)
056600970526     C     W             IFEQ      0
056700970526     C*
056800970526     C* EMISSIONE MESSAGGIO DI ERRORE
056900970526     C                   MOVEL     MSG(1)        DSMSMS
057000970526     C*
057100970526     C                   END                                                    W=0
057200970526     C                   END                                                    FUNKEY NE *BLANKS
057300970526     C*
057400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
057500970526     C     W             IFNE      0
057600970526     C     FUNKEY        OREQ      *BLANKS
057700970526     C     FUNKEY        IFEQ      *BLANKS
057800970526     C                   EXSR      ENTGES
057900970526     C                   END
058000970526     C     FUNKEY        IFEQ      '03'
058100970526     C                   EXSR      F03GES
058200970526     C                   END
058300970526     C     FUNKEY        IFEQ      '06'
058400970526     C                   EXSR      F06GES
058500970526     C                   END
058600970526     C     FUNKEY        IFEQ      '12'
058700970526     C                   EXSR      F12GES
058800970526     C                   END
058900970526     C     FUNKEY        IFEQ      '24'
059000970526     C                   EXSR      F24GES
059100970526     C                   END
059200970526     C                   END
059300970526     C*
059400970526     C                   ENDSR
059500970526     C*------------------------------------------------------------------------*
059600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
059700970526     C*------------------------------------------------------------------------*
059800970526     C     FUNVAR        BEGSR
059900970526     C*
060000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
060100970526     C*
060200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
060300970526     C     KEY           IFEQ      ENTER
060400970526     C                   MOVEL     *BLANKS       FUNKEY
060500970526     C                   END
060600970526     C     KEY           IFEQ      F1
060700970526     C                   MOVEL     '01'          FUNKEY
060800970526     C                   END
060900970526     C     KEY           IFEQ      F2
061000970526     C                   MOVEL     '02'          FUNKEY
061100970526     C                   END
061200970526     C     KEY           IFEQ      F3
061300970526     C                   MOVEL     '03'          FUNKEY
061400970526     C                   END
061500970526     C     KEY           IFEQ      F4
061600970526     C                   MOVEL     '04'          FUNKEY
061700970526     C                   END
061800970526     C     KEY           IFEQ      F5
061900970526     C                   MOVEL     '05'          FUNKEY
062000970526     C                   END
062100970526     C     KEY           IFEQ      F6
062200970526     C                   MOVEL     '06'          FUNKEY
062300970526     C                   END
062400970526     C     KEY           IFEQ      F7
062500970526     C                   MOVEL     '07'          FUNKEY
062600970526     C                   END
062700970526     C     KEY           IFEQ      F8
062800970526     C                   MOVEL     '08'          FUNKEY
062900970526     C                   END
063000970526     C     KEY           IFEQ      F9
063100970526     C                   MOVEL     '09'          FUNKEY
063200970526     C                   END
063300970526     C     KEY           IFEQ      F10
063400970526     C                   MOVEL     '10'          FUNKEY
063500970526     C                   END
063600970526     C     KEY           IFEQ      F11
063700970526     C                   MOVEL     '11'          FUNKEY
063800970526     C                   END
063900970526     C     KEY           IFEQ      F12
064000970526     C                   MOVEL     '12'          FUNKEY
064100970526     C                   END
064200970526     C     KEY           IFEQ      F13
064300970526     C                   MOVEL     '13'          FUNKEY
064400970526     C                   END
064500970526     C     KEY           IFEQ      F14
064600970526     C                   MOVEL     '14'          FUNKEY
064700970526     C                   END
064800970526     C     KEY           IFEQ      F15
064900970526     C                   MOVEL     '15'          FUNKEY
065000970526     C                   END
065100970526     C     KEY           IFEQ      F16
065200970526     C                   MOVEL     '16'          FUNKEY
065300970526     C                   END
065400970526     C     KEY           IFEQ      F17
065500970526     C                   MOVEL     '17'          FUNKEY
065600970526     C                   END
065700970526     C     KEY           IFEQ      F18
065800970526     C                   MOVEL     '18'          FUNKEY
065900970526     C                   END
066000970526     C     KEY           IFEQ      F19
066100970526     C                   MOVEL     '19'          FUNKEY
066200970526     C                   END
066300970526     C     KEY           IFEQ      F20
066400970526     C                   MOVEL     '20'          FUNKEY
066500970526     C                   END
066600970526     C     KEY           IFEQ      F21
066700970526     C                   MOVEL     '21'          FUNKEY
066800970526     C                   END
066900970526     C     KEY           IFEQ      F22
067000970526     C                   MOVEL     '22'          FUNKEY
067100970526     C                   END
067200970526     C     KEY           IFEQ      F23
067300970526     C                   MOVEL     '23'          FUNKEY
067400970526     C                   END
067500970526     C     KEY           IFEQ      F24
067600970526     C                   MOVEL     '24'          FUNKEY
067700970526     C                   END
067800970526     C     KEY           IFEQ      ROLLUP
067900970526     C                   MOVEL     'RU'          FUNKEY
068000970526     C                   END
068100970526     C     KEY           IFEQ      HELP
068200970526     C                   MOVEL     'HP'          FUNKEY
068300970526     C                   END
068400970526     C                   ENDSR
068500970526     C*------------------------------------------------------------------------*
068600970526     C* NOTE COSTRUZIONE SCHIERE
068700970526     C*------------------------------------------------------------------------*
068800970526     C*
068900970526     C* -MSG-MESSAGGI
069000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069100970526     C* (NON TOCCARE MSG,1 E MSG,2)
069200970526     C*
069300970526     C* -CMD-COMANDI
069400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
069500970526     C*
069600970526     C* -OPD-OPZIONI
069700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
069800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
069900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
070000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
070300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
070500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
070600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
070700970526     C*
070800970526     C* -FUD-FUNZIONI
070900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
071000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
071100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
071200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
071300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
071400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
071500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
071600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
071700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
071800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
071900970526     O*------------------------------------------------------------------------*
072000970526** DEC - DECODIFICHE
072100001116  ISCRIZIONE                                                      1
072200970526** MSG - MESSAGGI
072300020226TIS797R- Codice Cliente inesistente o non abilitato al Track & Tracing         1
072400020226TIS725R- E' possibile impostare solo 1 parametro per lancio                    2
072500020226TIS797R- Immissione parametri obbligatoria                                     3
072600020226TIS797R-                                                                       4
072700020226TIS797R-                                                                       5
072800020226TIS725R-                                                                       6
072900020226TIS725R-                                                                       7
073000020226TIS797R-                                                                       8
073100020226TIS797R-                                                                       9
073200020226TIS797R-                                                                       10
073300020226TIS797R-                                                                       11
073400020226TIS797R-                                                                       12
073500970526** FUD - FUNZIONI
073600970526                   -        (S/N)=N                        funz.01¬
073700970526                   -        (S/N)=N                        funz.02¬
073800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
073900970526                   -        (S/N)=N                        funz.04¬
074000020226F05=Refresh        -        (S/N)=N                        funz.05¬
074100020226F06=Lancio         -        (S/N)=S                        funz.06¬
074200970527                   -        (S/N)=N                        funz.07¬
074300970527                   -        (S/N)=N                        funz.08¬
074400970527                   -        (S/N)=N                        funz.09¬
074500970527                   -        (S/N)=N                        funz.10¬
074600020226F11=Abilit. servizi-        (S/N)=N                        funz.11¬
074700020226F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
074800970526                   -        (S/N)=N                        funz.13¬
074900970526                   -        (S/N)=N                        funz.14¬
075000970526                   -        (S/N)=N                        funz.15¬
075100970526                   -        (S/N)=N                        funz.16¬
075200970526                   -        (S/N)=N                        funz.17¬
075300970526                   -        (S/N)=N                        funz.18¬
075400001116                   -        (S/N)=N                        funz.19¬
075500970526                   -        (S/N)=N                        funz.20¬
075600970526                   -        (S/N)=N                        funz.21¬
075700970526                   -        (S/N)=N                        funz.22¬
075800970526                   -        (S/N)=N                        funz.23¬
075900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
