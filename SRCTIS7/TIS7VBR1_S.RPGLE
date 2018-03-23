000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600050825     FTIS7VBD1  CF   E             WORKSTN INFDS(DEVDS1)
000700050825     F                                     SFILE(S7VBDMS:MSGULT)
000800050823     FTIVIR02L  IF   E           K DISK
000900970526     D*------------------------------------------------------------------------*
001000970526     D* SCHIERE
001100970526     D*------------------------------------------------------------------------*
001200970526     D*------------
001300970526     D* MESSAGGI
001400970526     D*------------
001500970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
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
002700970526     D*------------
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
004900060807     D*------------
005000060807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
005100060807     D*-----------
005200060807     D DSXC          E DS                  extname(XCLIRDS)
005300060807     D KSC             S              7  0 dim(30)
005400060807     D KSA             S              4    dim(30)
005500060807     D*
005600060807     D                 DS
005700060807     D DSKSN                   1      4P 0
005800060807     D DSKSA                   1      4
005900060807     D*
006000060807     DBS69DS         E DS                  extname(TIBS69DS)
006100060807     DACODS          E DS                  extname(cnaco00f)
006200060807     DINDDS          E DS                  extname(cnind00f)
006300060807     DCLPDS          E DS                  extname(cnclp00f)
006400060807     DCLSDS          E DS                  extname(fncls00f)
006500020415     D*------------
006600970526     D* ARCHITETTURA
006700970526     D*------------
006800970526     D KPJBA         E DS
006900050823     D*--------------------
007000050823     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
007100050823     D*--------------------
007200050823     D PARAM           DS
007300050823     D  DATADA                        8  0
007400050823     D  DATAA                         8  0
007500050823     D  CODCLI                        7  0
007600050825     D  FLGIMI                        1
007700050825     D  FLGUNI                        1
007800050823     D  FLGOPZ                        1
007900080115     D  CODCLIVAS                     7  0
008000100312     D  TIPFILE                       2
008100080213     D  FLGEXE                        1
008200080213     D  TIPCLI                        1
008300970526     C*------------------------------------------------------------------------*
008400970526     C* MAIN LINES
008500970526     C*------------------------------------------------------------------------*
008600970526     C*
008700970526     C* NON CI SONO ERRORI IN ENTRATA
008800970526IF  1C     ERRL00        IFEQ      '0'
008900050823     C*
009000050823     C* INIZIALIZZA LA VIDEATA
009100050823     C                   EXSR      AZZVID
009200970526     C*
009300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009700970526     C*
009800970526     C* EMETTE FORMATO VIDEO
009900970526     C                   EXSR      BUTFMR
010000970526     C*
010100970526     C* GESTIONE TASTI FUNZIONALI
010200970526     C                   EXSR      FUNCON
010300970526E   2C                   ENDDO
010400970526     C*
010500970526E   1C                   ENDIF
010600970526     C* FINE PGM
010700970526     C                   SETON                                        LR
010800970526     C*------------------------------------------------------------------------*
010900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011000970526     C*------------------------------------------------------------------------*
011100970526     C     BUTFMR        BEGSR
011200970526     C*
011300970526     C* EMETTE LA TESTATA
011400050825     C                   WRITE     S7VBD01
011500970526     C*
011600970526     C* SPEDISCE EVENTUALE MESSAGGIO
011700970526     C                   EXSR      SNDMSG
011800970526     C*
011900970526     C* EMETTE LE FUNZIONI
012000050825     C                   WRITE     S7VBD02
012100970526     C*
012200970526     C* EMETTE IL VIDEO
012300050825     C                   EXFMT     S7VBD03
012400970526     C*
012500970526     C* REIMPOSTA FLAG DI ERRORE
012600970526     C                   MOVEL     '0'           ERRL00
012700970526     C*
012800970526     C                   ENDSR
012900970526     C*------------------------------------------------------------------------*
013000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013100970526     C*------------------------------------------------------------------------*
013200970526     C     SNDMSG        BEGSR
013300970526     C*
013400970526     C* PULISCE IL SFILE MESSAGGI
013500970526     C                   MOVEL     '1'           *IN40
013600050825     C                   WRITE     S7VBDMC
013700970526     C                   MOVEL     '0'           *IN40
013800970526     C*
013900970526     C* VISUALIZZA IL MESSAGGIO
014000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014100970526     C                   CALL      'XMSG'        XMSGPA
014200970526IF  1C     DSMSER        IFEQ      *ZEROS
014300970526     C                   MOVEL     DSMSKE        MSGKEY
014400970526E   1C                   ENDIF
014500970526     C                   Z-ADD     1             MSGULT
014600050825     C                   WRITE     S7VBDMS
014700050825     C                   WRITE     S7VBDMC
014800970526     C*
014900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015000970526     C                   MOVEL     *BLANKS       DSMSMS
015100970526     C*
015200970526     C                   ENDSR
015300050823     C*------------------------------------------------------------------------*
015400050823     C* AZZVID - INIZIALIZZAZIONE VIDEATA
015500050823     C*------------------------------------------------------------------------*
015600050823     C     AZZVID        BEGSR
015700050823     C*
015800050825     C                   EVAL      VIDIMI = 'N'
015900050825     C                   EVAL      VIDUNI = 'U'
016000050907     C                   EVAL      VIDOPZ = *blanks
016100080115     C                   EVAL      VIDCLIVAS = *zeros
016200100312     C                   EVAL      VIDTIP = *blanks
016300050823     C*
016400050823     C                   ENDSR
016500050823     C*------------------------------------------------------------------------*
016600050823     C* DECODI - DECODIFICHE E CONTROLLI A VIDEO
016700050823     C*------------------------------------------------------------------------*
016800050823     C     DECODI        BEGSR
016900050823     C*
017000050823     C* Inizializzazione errori
017100100312     C                   SETOFF                                       505152
017200050823     C                   EVAL      ERRL00 = '0'
017300050823     C*
017400050823     C* Se richiesta opzione modalità creazione 2 => verifico che il cliente selezionato
017500050831     C* sia abilitato allo scambio dati del VAB accettazione bolle (tipo file BT)
017600050907     C                   IF        ERRL00 = '0'
017700050823     C                   IF        VIDOPZ = '2'
017800050823     C                   EVAL      VIRKSC = *all'0'
017900080115     C                   IF        VIDCLIVAS = *ZEROS
018000080115     C                   MOVE      VIDCLI        VIDCLIVAS
018100080115     C                   ENDIF
018200080115     C                   MOVE      VIDCLIVAS     VIRKSC
018300100312     C                   IF        VIDTIP = *BLANKS
018400100312     C                   EVAL      VIDTIP = 'BT'
018500100312     C                   ENDIF
018600100312     C                   MOVEL     VIDTIP        VIRTIP
018700050823     C     KEYVIR2_P     CHAIN     TIVIR02L
018800050823     C                   IF        not %found(TIVIR02L)
018900050823     C                   MOVEL     MSG(2)        DSMSMS
019000050823     C                   SETON                                        50
019100050823     C                   EVAL      ERRL00 = '1'
019200050823     C                   ENDIF
019300050823     C                   ENDIF
019400050907     C                   ENDIF
019500060807     C*
019600060807     C* Richiesta scelta codice cliente
019700060807     C                   IF        ERRL00 = '0'
019800060807     C                   SETOFF                                           28
019900060807     C     '?'           SCAN      VIDCLI                                 28
020000060807     C     *IN28         IFEQ      *ON
020100060807     C* CLIENTI
020200060807     C                   CLEAR                   DSXC
020300060807     C                   Z-ADD     151           DXCCAP
020400060807     C                   CALL      'XCLIR'
020500060807     C                   PARM                    DSXC
020600060807     C* SCOMPATTA
020700060807     C                   MOVEA     DXCKSC        KSA
020800060807     C     1             DO        30            J                 3 0
020900060807     C                   MOVE      KSA(J)        DSKSA
021000060807     C                   Z-ADD     DSKSN         KSC(J)
021100060807     C                   ENDDO
021200060807     C                   MOVEL     KSC(1)        VIDCLI
021300060807     C                   ENDIF
021400060807     C                   ENDIF
021500050823     C*
021600050823     C* Verifico obbligatorietà indicazione cliente
021700050907     C                   IF        ERRL00 = '0'
021800060807     C                   IF        VIDCLI = *zeros OR
021900060807     C                             VIDCLI = *blanks
022000050823     C                   MOVEL     MSG(3)        DSMSMS
022100100312     C                   SETON                                        52
022200050823     C                   EVAL      ERRL00 = '1'
022300060807     C                   ELSE
022400060807     C                   CLEAR                   BS69DS
022500060807     C                   CLEAR                   ACODS
022600060807     C                   MOVEL     KNSIF         I69SIF
022700060807     C                   MOVE(P)   VIDCLI        I69KAC
022800060807     C                   CALL      'TIBS69R'
022900060807     C                   PARM                    BS69DS
023000060807     C                   PARM                    ACODS
023100060807     C                   PARM                    INDDS
023200060807     C                   PARM                    CLPDS
023300060807     C                   PARM                    CLSDS
023400060807     C     O69ERR        IFNE      '1'
023500060807     C                   MOVEL     ACORAG        VIDCLID
023600060807     C                   ELSE
023700060807     C                   MOVEL     *ALL'*'       VIDCLID
023800060807     C                   ENDIF
023900050823     C                   ENDIF
024000050907     C                   ENDIF
024100050907     C*
024200050907     C* Verifico obbligatorietà indicazione opzione generazione dati
024300050907     C                   IF        ERRL00 = '0'
024400050907     C                   IF        VIDOPZ = *blanks
024500050907     C                   MOVEL     MSG(4)        DSMSMS
024600050907     C                   SETON                                        51
024700050907     C                   EVAL      ERRL00 = '1'
024800050907     C                   ENDIF
024900050907     C                   ENDIF
025000050823     C*
025100050823     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* ENTGES - GESTIONE ENTER -> INVIO
025400970526     C*------------------------------------------------------------------------*
025500970526     C     ENTGES        BEGSR
025600050823     C*
025700050823     C* Eseguo decodifiche e controlli a video
025800050823     C                   EXSR      DECODI
025900970526     C*
026000970526     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* F03GES - GESTIONE F03 -> FINE
026300970526     C*------------------------------------------------------------------------*
026400970526     C     F03GES        BEGSR
026500970526     C*
026600970526     C* IMPOSTA "PREMUTO F03"
026700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100050825     C* F06GES - GESTIONE F06 -> CONFERMA OPZIONE
027200970526     C*------------------------------------------------------------------------*
027300050816     C     F06GES        BEGSR
027400050823     C*
027500050823     C* Eseguo decodifiche e controlli a video
027600050823     C                   EXSR      DECODI
027700050823     C*
027800050823     C* Se tutto ok procedo
027900050823     C                   IF        ERRL00 = '0'
028000050823     C*
028100050823     C* Valorizzo la DS d passaggio parametri
028200080213     C                   EVAL      DATADA    = VIDDAD
028300080213     C                   EVAL      DATAA     = VIDDAA
028400080115     C                   MOVE(P)   VIDCLI        CODCLI
028500080213     C                   EVAL      FLGIMI    = VIDIMI
028600080213     C                   EVAL      FLGUNI    = VIDUNI
028700080213     C                   EVAL      FLGOPZ    = VIDOPZ
028800080213     C                   EVAL      FLGEXE    = VIDEXE
028900080213     C                   EVAL      TIPCLI    = VIDTIPCLI
029000080115     C                   IF        VIDCLIVAS > *ZEROS
029100080115     C                   EVAL      CODCLIVAS = VIDCLIVAS
029200080115     C                   ELSE
029300080115     C                   MOVE(P)   VIDCLI        CODCLIVAS
029400080115     C                   ENDIF
029500100312     C                   EVAL      TIPFILE   = VIDTIP
029600020412     C*
029700050823     C* Sottometto l'azione di elaborazione
029800050825     C                   MOVEL     'S7V2'        KCOAZ
029900050823     C                   MOVEL     PARAM         KPJBU
030000080213     C                   IF        VIDEXE = *blanks
030100080213     C                   CALL      'BCH10'
030200080213     C                   PARM                    KPJBA
030300080213     C                   ELSE
030400080213     C                   CALL      'TIS7VBC'
030500050823     C                   PARM                    KPJBA
030600080213     C                   ENDIF
030700051110     C***                MOVEL     '1'           F03L00                         *FINE PROGRAMMA
030800051110     C                   EVAL      VIDCLI = *zeros
030900051110     C                   EVAL      VIDIMI = 'N'
031000051110     C                   EVAL      VIDUNI = 'U'
031100051110     C                   EVAL      VIDOPZ = *blanks
031200080115     C                   EVAL      VIDCLIVAS = *zeros
031300100312     C                   EVAL      VIDTIP = *blanks
031400050823     C                   ENDIF
031500050823     C*
031600020412     C                   ENDSR
031700970526     C*------------------------------------------------------------------------*
031800970526     C* F12GES - GESTIONE F12 -> RITORNO
031900970526     C*------------------------------------------------------------------------*
032000970526     C     F12GES        BEGSR
032100970526     C*
032200970526     C* IMPOSTA "PREMUTO F12"
032300050823     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
032400970526     C*
032500970526     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
032800970526     C*------------------------------------------------------------------------*
032900970526     C     F24GES        BEGSR
033000970526     C*
033100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
033600970526     C*------------------------------------------------------------------------*
033700970526     C     IMPFUN        BEGSR
033800970526     C*
033900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
034000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
034100970526     C                   EXSR      FUNGES
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* *INZSR - OPERAZIONI INIZIALI
034600970526     C*------------------------------------------------------------------------*
034700970526     C     *INZSR        BEGSR
034800970526     C*---
034900970526     C* RICEVIMENTO PARAMETRI
035000970526     C*---
035100970526     C     *ENTRY        PLIST
035200970612     C                   PARM                    KPJBA
035300050823     C*
035400050823     C* CHIAVI DI LETTURA
035500050823     C     KEYVIR2_P     KLIST
035600050823     C                   KFLD                    VIRKSC
035700050823     C                   KFLD                    VIRTIP
035800970526     C*
035900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
036000970526     C     XMSGPA        PLIST
036100970526     C                   PARM                    DSMSMS
036200970526     C                   PARM                    DSMSPG
036300970526     C                   PARM                    DSMSKE
036400970526     C                   PARM                    DSMSER
036500970526     C*
036600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
036700970526     C                   CALL      'XKEY'
036800970526     C                   PARM                    DSKEY
036900970526     C*---
037000970526     C* VARIABILI NON RIFERITE AL DATA BASE
037100970526     C*---
037200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
037300970526     C*---
037400970526     C* VARIABILI DI CONTROLLO
037500970526     C*---
037600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
037700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
037800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
037900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
038000970526     C*
038100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
038200050825     C                   MOVEL     'TIS7VBR1'    DSMSPG
038300050825     C                   MOVEL     'TIS7VBR1'    MSGQUE
038400970526     C                   MOVEL     *BLANKS       MSGKEY
038500970526     C                   MOVEL     *BLANKS       DSMSMS
038600970526     C*
038700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
038800970526     C                   EXSR      IMPFUN
038900970526     C*
039000970526     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
039300970526     C*------------------------------------------------------------------------*
039400970526     C     FUNGES        BEGSR
039500970526     C*
039600970526     C* PULISCO CAMPI DI LAVORO
039700970526     C                   Z-ADD     0             W                 5 0
039800970526     C*
039900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
040000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
040100970526     C     1             DO        24            I
040200970526     C                   MOVEL     FUD(I)        FUDRIG
040300970526     C*
040400970526     C* PULIZIA FUNZIONI PRECEDENTI
040500970526     C                   MOVEL     *BLANKS       FUV(I)
040600970526     C*
040700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
040800970526     C* (SALTA F24)
040900970526     C     FUDATT        IFEQ      'S'
041000970526     C     FUDFUN        ANDNE     '24'
041100970526     C                   ADD       1             W
041200970526     C                   MOVEL     FUDDES        FUV(W)
041300970526     C                   END
041400970526     C                   END                                                    DO WHILE
041500970526     C*
041600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
041700970526     C* ALTRIMENTI DISATTIVO F24
041800970526     C     1             DO        24            I
041900970526     C                   MOVEL     FUD(I)        FUDRIG
042000970526     C*
042100970526     C     FUDFUN        IFEQ      '24'
042200970526     C     W             IFGT      6
042300970526     C                   MOVEL     'S'           FUDATT
042400970526     C                   ELSE
042500970526     C                   MOVEL     *BLANKS       FUDATT
042600970526     C                   END                                                    W GT 8
042700970526     C                   MOVEL     FUDRIG        FUD(I)
042800970526     C                   END
042900970526     C                   END                                                    DO WHILE
043000970526     C*
043100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
043200970526     C     W             DIV       6             I
043300970526     C                   MVR                     Z
043400970526     C*
043500970526     C     Z             IFNE      0
043600970526     C     I             ADD       1             Z
043700970526     C                   ELSE
043800970526     C                   Z-ADD     I             Z                 5 0
043900970526     C                   END
044000970526     C*
044100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
044200970526     C                   ADD       1             FUNGRU
044300970526     C     FUNGRU        IFGT      Z
044400970526     C     FUNGRU        ORLT      1
044500970526     C                   Z-ADD     1             FUNGRU
044600970526     C                   END
044700970526     C*
044800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
044900970526     C     FUNGRU        IFEQ      1
045000970526     C                   MOVEL     FUV(1)        FUN001
045100970526     C                   MOVEL     FUV(2)        FUN002
045200970526     C                   MOVEL     FUV(3)        FUN003
045300970526     C                   MOVEL     FUV(4)        FUN004
045400970526     C                   MOVEL     FUV(5)        FUN005
045500970526     C                   MOVEL     FUV(6)        FUN006
045600970526     C                   END
045700970526     C*
045800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
045900970526     C     FUNGRU        IFEQ      2
046000970526     C                   MOVEL     FUV(7)        FUN001
046100970526     C                   MOVEL     FUV(8)        FUN002
046200970526     C                   MOVEL     FUV(9)        FUN003
046300970526     C                   MOVEL     FUV(10)       FUN004
046400970526     C                   MOVEL     FUV(11)       FUN005
046500970526     C                   MOVEL     FUV(12)       FUN006
046600970526     C                   END
046700970526     C*
046800970526     C* TERZO GRUPPO DI 6 FUNZIONI
046900970526     C     FUNGRU        IFEQ      3
047000970526     C                   MOVEL     FUV(13)       FUN001
047100970526     C                   MOVEL     FUV(14)       FUN002
047200970526     C                   MOVEL     FUV(15)       FUN003
047300970526     C                   MOVEL     FUV(16)       FUN004
047400970526     C                   MOVEL     FUV(17)       FUN005
047500970526     C                   MOVEL     FUV(18)       FUN006
047600970526     C                   END
047700970526     C*
047800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
047900970526     C     FUNGRU        IFEQ      4
048000970526     C                   MOVEL     FUV(19)       FUN001
048100970526     C                   MOVEL     FUV(20)       FUN002
048200970526     C                   MOVEL     FUV(21)       FUN003
048300970526     C                   MOVEL     FUV(22)       FUN004
048400970526     C                   MOVEL     *BLANKS       FUN005
048500970526     C                   MOVEL     *BLANKS       FUN006
048600970526     C                   END
048700970526     C*
048800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
048900970526     C                   MOVEL     *BLANKS       FUN007
049000970526     C                   MOVEL     *BLANKS       FUN008
049100970526     C*
049200970526     C     1             DO        24            I
049300970526     C                   MOVEL     FUD(I)        FUDRIG
049400970526     C     FUDFUN        IFEQ      '24'
049500970526     C     FUDATT        ANDEQ     'S'
049600970526     C                   MOVEL     FUDDES        FUN008
049700970526     C                   END
049800970526     C                   END                                                    DO
049900970526     C*
050000970526     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
050300970526     C*------------------------------------------------------------------------*
050400970526     C     FUNCON        BEGSR
050500970526     C*
050600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
050700970526     C                   EXSR      FUNVAR
050800970526     C*
050900970526     C* PULISCO CAMPI DI LAVORO
051000970526     C                   Z-ADD     0             I                 5 0
051100970526     C                   Z-ADD     0             W                 5 0
051200970526     C*
051300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
051400970526     C     FUNKEY        IFNE      *BLANKS
051500970526     C     1             DO        24            I
051600970526     C                   MOVEL     FUD(I)        FUDRIG
051700970526     C*
051800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
051900970526     C     FUNKEY        IFEQ      FUDFUN
052000970526     C     FUDATT        ANDEQ     'S'
052100970526     C                   Z-ADD     I             W
052200970526     C                   END
052300970526     C                   END                                                    DO WHILE
052400970526     C*
052500970526     C* FUNZIONE NON ATTIVA (ERRORE)
052600970526     C     W             IFEQ      0
052700970526     C*
052800970526     C* EMISSIONE MESSAGGIO DI ERRORE
052900970526     C                   MOVEL     MSG(1)        DSMSMS
053000970526     C*
053100970526     C                   END                                                    W=0
053200970526     C                   END                                                    FUNKEY NE *BLANKS
053300970526     C*
053400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
053500970526     C     W             IFNE      0
053600970526     C     FUNKEY        OREQ      *BLANKS
053700970526     C     FUNKEY        IFEQ      *BLANKS
053800970526     C                   EXSR      ENTGES
053900970526     C                   END
054000970526     C     FUNKEY        IFEQ      '03'
054100970526     C                   EXSR      F03GES
054200970526     C                   END
054300970526     C     FUNKEY        IFEQ      '06'
054400970526     C                   EXSR      F06GES
054500970526     C                   END
054600970526     C     FUNKEY        IFEQ      '12'
054700970526     C                   EXSR      F12GES
054800970526     C                   END
054900970526     C     FUNKEY        IFEQ      '24'
055000970526     C                   EXSR      F24GES
055100970526     C                   END
055200970526     C                   END
055300970526     C*
055400970526     C                   ENDSR
055500970526     C*------------------------------------------------------------------------*
055600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
055700970526     C*------------------------------------------------------------------------*
055800970526     C     FUNVAR        BEGSR
055900970526     C*
056000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
056100970526     C*
056200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
056300970526     C     KEY           IFEQ      ENTER
056400970526     C                   MOVEL     *BLANKS       FUNKEY
056500970526     C                   END
056600970526     C     KEY           IFEQ      F1
056700970526     C                   MOVEL     '01'          FUNKEY
056800970526     C                   END
056900970526     C     KEY           IFEQ      F2
057000970526     C                   MOVEL     '02'          FUNKEY
057100970526     C                   END
057200970526     C     KEY           IFEQ      F3
057300970526     C                   MOVEL     '03'          FUNKEY
057400970526     C                   END
057500970526     C     KEY           IFEQ      F4
057600970526     C                   MOVEL     '04'          FUNKEY
057700970526     C                   END
057800970526     C     KEY           IFEQ      F5
057900970526     C                   MOVEL     '05'          FUNKEY
058000970526     C                   END
058100970526     C     KEY           IFEQ      F6
058200970526     C                   MOVEL     '06'          FUNKEY
058300970526     C                   END
058400970526     C     KEY           IFEQ      F7
058500970526     C                   MOVEL     '07'          FUNKEY
058600970526     C                   END
058700970526     C     KEY           IFEQ      F8
058800970526     C                   MOVEL     '08'          FUNKEY
058900970526     C                   END
059000970526     C     KEY           IFEQ      F9
059100970526     C                   MOVEL     '09'          FUNKEY
059200970526     C                   END
059300970526     C     KEY           IFEQ      F10
059400970526     C                   MOVEL     '10'          FUNKEY
059500970526     C                   END
059600970526     C     KEY           IFEQ      F11
059700970526     C                   MOVEL     '11'          FUNKEY
059800970526     C                   END
059900970526     C     KEY           IFEQ      F12
060000970526     C                   MOVEL     '12'          FUNKEY
060100970526     C                   END
060200970526     C     KEY           IFEQ      F13
060300970526     C                   MOVEL     '13'          FUNKEY
060400970526     C                   END
060500970526     C     KEY           IFEQ      F14
060600970526     C                   MOVEL     '14'          FUNKEY
060700970526     C                   END
060800970526     C     KEY           IFEQ      F15
060900970526     C                   MOVEL     '15'          FUNKEY
061000970526     C                   END
061100970526     C     KEY           IFEQ      F16
061200970526     C                   MOVEL     '16'          FUNKEY
061300970526     C                   END
061400970526     C     KEY           IFEQ      F17
061500970526     C                   MOVEL     '17'          FUNKEY
061600970526     C                   END
061700970526     C     KEY           IFEQ      F18
061800970526     C                   MOVEL     '18'          FUNKEY
061900970526     C                   END
062000970526     C     KEY           IFEQ      F19
062100970526     C                   MOVEL     '19'          FUNKEY
062200970526     C                   END
062300970526     C     KEY           IFEQ      F20
062400970526     C                   MOVEL     '20'          FUNKEY
062500970526     C                   END
062600970526     C     KEY           IFEQ      F21
062700970526     C                   MOVEL     '21'          FUNKEY
062800970526     C                   END
062900970526     C     KEY           IFEQ      F22
063000970526     C                   MOVEL     '22'          FUNKEY
063100970526     C                   END
063200970526     C     KEY           IFEQ      F23
063300970526     C                   MOVEL     '23'          FUNKEY
063400970526     C                   END
063500970526     C     KEY           IFEQ      F24
063600970526     C                   MOVEL     '24'          FUNKEY
063700970526     C                   END
063800970526     C     KEY           IFEQ      ROLLUP
063900970526     C                   MOVEL     'RU'          FUNKEY
064000970526     C                   END
064100970526     C     KEY           IFEQ      HELP
064200970526     C                   MOVEL     'HP'          FUNKEY
064300970526     C                   END
064400970526     C                   ENDSR
064500970526     C*------------------------------------------------------------------------*
064600970526     C* NOTE COSTRUZIONE SCHIERE
064700970526     C*------------------------------------------------------------------------*
064800970526     C*
064900970526     C* -MSG-MESSAGGI
065000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
065100970526     C* (NON TOCCARE MSG,1 E MSG,2)
065200970526     C*
065300970526     C* -CMD-COMANDI
065400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
065500970526     C*
065600970526     C* -OPD-OPZIONI
065700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
065800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
065900970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
066000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
066300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
066500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
066600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
066700970526     C*
066800970526     C* -FUD-FUNZIONI
066900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
067000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
067300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
067400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
067500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
067600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
067700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
067800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
067900970526     O*------------------------------------------------------------------------*
068000970528** MSG - MESSAGGI
068100050823Funzione non abilitata.                                                        1
068200100312Cliente indicato NON è abilitato allo scambio dati VAG del tipo file indicato  2
068300050823Codice cliente obbligatorio.                                                   3
068400050907Modalità creazione obbligatoria.                                               4
068500050816                                                                               5
068600050816                                                                               6
068700050816                                                                               7
068800050816                                                                               8
068900050816                                                                               9
069000050816                                                                               10
069100050816                                                                               11
069200050816                                                                               12
069300050816                                                                               13
069400050816                                                                               14
069500050816                                                                               15
069600050816                                                                               16
069700050816                                                                               17
069800020415                                                                               18
069900020415                                                                               19
070000020415                                                                               20
070100970526** FUD - FUNZIONI
070200970526                   -        (S/N)=N                        funz.01¬
070300970526                   -        (S/N)=N                        funz.02¬
070400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
070500970526                   -        (S/N)=N                        funz.04¬
070600970526                   -        (S/N)=N                        funz.05¬
070700050823F06=Conferma       -        (S/N)=S                        funz.06¬
070800050823                   -        (S/N)=N                        funz.07¬
070900970527                   -        (S/N)=N                        funz.08¬
071000970527                   -        (S/N)=N                        funz.09¬
071100970527                   -        (S/N)=N                        funz.10¬
071200970526                   -        (S/N)=N                        funz.11¬
071300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
071400970526                   -        (S/N)=N                        funz.13¬
071500970526                   -        (S/N)=N                        funz.14¬
071600970526                   -        (S/N)=N                        funz.15¬
071700970526                   -        (S/N)=N                        funz.16¬
071800970526                   -        (S/N)=N                        funz.17¬
071900970526                   -        (S/N)=N                        funz.18¬
072000970526                   -        (S/N)=N                        funz.19¬
072100970526                   -        (S/N)=N                        funz.20¬
072200970526                   -        (S/N)=N                        funz.21¬
072300970526                   -        (S/N)=N                        funz.22¬
072400970526                   -        (S/N)=N                        funz.23¬
072500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
