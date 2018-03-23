000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600050831     FTIS7VSD1  CF   E             WORKSTN INFDS(DEVDS1)
000700050831     F                                     SFILE(S7VSDMS:MSGULT)
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
007300050831     D  DATAPDA                       8  0
007400050831     D  DATAPA                        8  0
007500050831     D  DATASDA                       8  0
007600050831     D  DATASA                        8  0
007700050823     D  CODCLI                        7  0
007800050825     D  FLGUNI                        1
007900050823     D  FLGOPZ                        1
008000080917     D  CODCLIVAS                     7  0
008100100312     D  TIPFILE                       2
008200970526     C*------------------------------------------------------------------------*
008300970526     C* MAIN LINES
008400970526     C*------------------------------------------------------------------------*
008500970526     C*
008600970526     C* NON CI SONO ERRORI IN ENTRATA
008700970526IF  1C     ERRL00        IFEQ      '0'
008800050823     C*
008900050823     C* INIZIALIZZA LA VIDEATA
009000050823     C                   EXSR      AZZVID
009100970526     C*
009200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009600970526     C*
009700970526     C* EMETTE FORMATO VIDEO
009800970526     C                   EXSR      BUTFMR
009900970526     C*
010000970526     C* GESTIONE TASTI FUNZIONALI
010100970526     C                   EXSR      FUNCON
010200970526E   2C                   ENDDO
010300970526     C*
010400970526E   1C                   ENDIF
010500970526     C* FINE PGM
010600970526     C                   SETON                                        LR
010700970526     C*------------------------------------------------------------------------*
010800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010900970526     C*------------------------------------------------------------------------*
011000970526     C     BUTFMR        BEGSR
011100970526     C*
011200970526     C* EMETTE LA TESTATA
011300050831     C                   WRITE     S7VSD01
011400970526     C*
011500970526     C* SPEDISCE EVENTUALE MESSAGGIO
011600970526     C                   EXSR      SNDMSG
011700970526     C*
011800970526     C* EMETTE LE FUNZIONI
011900050831     C                   WRITE     S7VSD02
012000970526     C*
012100970526     C* EMETTE IL VIDEO
012200050831     C                   EXFMT     S7VSD03
012300970526     C*
012400970526     C* REIMPOSTA FLAG DI ERRORE
012500970526     C                   MOVEL     '0'           ERRL00
012600970526     C*
012700970526     C                   ENDSR
012800970526     C*------------------------------------------------------------------------*
012900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013000970526     C*------------------------------------------------------------------------*
013100970526     C     SNDMSG        BEGSR
013200970526     C*
013300970526     C* PULISCE IL SFILE MESSAGGI
013400970526     C                   MOVEL     '1'           *IN40
013500050831     C                   WRITE     S7VSDMC
013600970526     C                   MOVEL     '0'           *IN40
013700970526     C*
013800970526     C* VISUALIZZA IL MESSAGGIO
013900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014000970526     C                   CALL      'XMSG'        XMSGPA
014100970526IF  1C     DSMSER        IFEQ      *ZEROS
014200970526     C                   MOVEL     DSMSKE        MSGKEY
014300970526E   1C                   ENDIF
014400970526     C                   Z-ADD     1             MSGULT
014500050831     C                   WRITE     S7VSDMS
014600050831     C                   WRITE     S7VSDMC
014700970526     C*
014800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014900970526     C                   MOVEL     *BLANKS       DSMSMS
015000970526     C*
015100970526     C                   ENDSR
015200050823     C*------------------------------------------------------------------------*
015300050823     C* AZZVID - INIZIALIZZAZIONE VIDEATA
015400050823     C*------------------------------------------------------------------------*
015500050823     C     AZZVID        BEGSR
015600050823     C*
015700050825     C                   EVAL      VIDUNI = 'U'
015800050907     C                   EVAL      VIDOPZ = *blanks
015900080917     C                   EVAL      VIDCLIVAS = *zeros
016000100312     C                   EVAL      VIDTIP    = *blanks
016100050823     C*
016200050823     C                   ENDSR
016300050823     C*------------------------------------------------------------------------*
016400050823     C* DECODI - DECODIFICHE E CONTROLLI A VIDEO
016500050823     C*------------------------------------------------------------------------*
016600050823     C     DECODI        BEGSR
016700050823     C*
016800050823     C* Inizializzazione errori
016900080917     C                   SETOFF                                       505152
017000100312     C                   SETOFF                                       5354
017100050823     C                   EVAL      ERRL00 = '0'
017200050823     C*
017300050823     C* Se richiesta opzione modalità creazione 2 => verifico che il cliente selezionato
017400050831     C* sia abilitato allo scambio dati del VAS (tipo file VS)
017500050907     C                   IF        ERRL00 = '0'
017600050823     C                   IF        VIDOPZ = '2'
017700050823     C                   EVAL      VIRKSC = *all'0'
017800081118     C                   IF        VIDCLIVAS  > *zeros
017900080917     C                   MOVE      VIDCLIVAS     VIRKSC
018000081118     C                   ELSE
018100081118     C                   MOVE      VIDCLI        VIRKSC
018200081118     C                   ENDIF
018300100312     C                   IF        VIDTIP = *BLANKS
018400100312     C                   EVAL      VIDTIP = 'VS'
018500100312     C                   ENDIF
018600100312     C                   MOVEL     VIDTIP        VIRTIP
018700050823     C     KEYVIR2_P     CHAIN     TIVIR02L
018800050823     C                   IF        not %found(TIVIR02L)
018900050823     C                   MOVEL     MSG(2)        DSMSMS
019000100312     C                   SETON                                        50
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
021500100312     C*
021600100312     C* Verifico obbligatorietà periodo
021700100312     C                   IF        ERRL00 = '0'
021800100312     C                   IF        VIDPDAD <> *zeros AND
021900100312     C                             VIDPDAA <> *zeros AND
022000100312     C                             VIDSDAD <> *zeros AND
022100100312     C                             VIDSDAA <> *zeros
022200100312     C                   MOVEL     MSG(5)        DSMSMS
022300100312     C                   SETON                                        5354
022400100312     C                   EVAL      ERRL00 = '1'
022500100312     C                   ENDIF
022600100312     C                   ENDIF
022700100312     C                   IF        ERRL00 = '0'
022800100312     C                   IF        (VIDPDAD <> *zeros AND
022900100312     C                              VIDPDAA <> *zeros) OR
023000100312     C                             (VIDSDAD <> *zeros AND
023100100312     C                              VIDSDAA <> *zeros)
023200100312     C                   ELSE
023300100312     C                   MOVEL     MSG(6)        DSMSMS
023400100312     C                   SETON                                        5354
023500100312     C                   EVAL      ERRL00 = '1'
023600100312     C                   ENDIF
023700100312     C                   ENDIF
023800050823     C*
023900050823     C* Verifico obbligatorietà indicazione cliente
024000050907     C                   IF        ERRL00 = '0'
024100060807     C                   IF        VIDCLI = *zeros OR
024200060807     C                             VIDCLI = *blanks
024300050823     C                   MOVEL     MSG(3)        DSMSMS
024400100312     C                   SETON                                        52
024500050823     C                   EVAL      ERRL00 = '1'
024600060807     C                   ELSE
024700060807     C                   CLEAR                   BS69DS
024800060807     C                   CLEAR                   ACODS
024900060807     C                   MOVEL     KNSIF         I69SIF
025000060807     C                   MOVE(P)   VIDCLI        I69KAC
025100060807     C                   CALL      'TIBS69R'
025200060807     C                   PARM                    BS69DS
025300060807     C                   PARM                    ACODS
025400060807     C                   PARM                    INDDS
025500060807     C                   PARM                    CLPDS
025600060807     C                   PARM                    CLSDS
025700060807     C     O69ERR        IFNE      '1'
025800060807     C                   MOVEL     ACORAG        VIDCLID
025900060807     C                   ELSE
026000060807     C                   MOVEL     *ALL'*'       VIDCLID
026100060807     C                   ENDIF
026200050823     C                   ENDIF
026300050907     C                   ENDIF
026400050907     C*
026500050907     C* Verifico obbligatorietà indicazione opzione generazione dati
026600050907     C                   IF        ERRL00 = '0'
026700050907     C                   IF        VIDOPZ = *blanks
026800050907     C                   MOVEL     MSG(4)        DSMSMS
026900050907     C                   SETON                                        51
027000050907     C                   EVAL      ERRL00 = '1'
027100050907     C                   ENDIF
027200050907     C                   ENDIF
027300050823     C*
027400050823     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* ENTGES - GESTIONE ENTER -> INVIO
027700970526     C*------------------------------------------------------------------------*
027800970526     C     ENTGES        BEGSR
027900050823     C*
028000050823     C* Eseguo decodifiche e controlli a video
028100050823     C                   EXSR      DECODI
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* F03GES - GESTIONE F03 -> FINE
028600970526     C*------------------------------------------------------------------------*
028700970526     C     F03GES        BEGSR
028800970526     C*
028900970526     C* IMPOSTA "PREMUTO F03"
029000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
029100970526     C*
029200970526     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400050825     C* F06GES - GESTIONE F06 -> CONFERMA OPZIONE
029500970526     C*------------------------------------------------------------------------*
029600050816     C     F06GES        BEGSR
029700050823     C*
029800050823     C* Eseguo decodifiche e controlli a video
029900050823     C                   EXSR      DECODI
030000050823     C*
030100050823     C* Se tutto ok procedo
030200050823     C                   IF        ERRL00 = '0'
030300050823     C*
030400050823     C* Valorizzo la DS d passaggio parametri
030500050831     C                   EVAL      DATAPDA = VIDPDAD
030600050831     C                   EVAL      DATAPA  = VIDPDAA
030700050831     C                   EVAL      DATASDA = VIDSDAD
030800050831     C                   EVAL      DATASA  = VIDSDAA
030900060807     C                   MOVE(P)   VIDCLI        CODCLI
031000050823     C                   EVAL      FLGUNI  = VIDUNI
031100050823     C                   EVAL      FLGOPZ  = VIDOPZ
031200080917     C                   IF        VIDCLIVAS > *ZEROS
031300080917     C                   EVAL      CODCLIVAS = VIDCLIVAS
031400080917     C                   ELSE
031500080917     C                   MOVE(P)   VIDCLI        CODCLIVAS
031600080917     C                   ENDIF
031700100312     C                   EVAL      TIPFILE   = VIDTIP
031800020412     C*
031900050823     C* Sottometto l'azione di elaborazione
032000050831     C                   MOVEL     'S7V4'        KCOAZ
032100050823     C                   MOVEL     PARAM         KPJBU
032200131009     C***                CALL      'BCH10'
032300131009 mom C                   CALL      'TIS7VSC'
032400050823     C                   PARM                    KPJBA
032500051110     C***                MOVEL     '1'           F03L00                         *FINE PROGRAMMA
032600051110     C                   EVAL      VIDCLI = *zeros
032700051110     C                   EVAL      VIDUNI = 'U'
032800051110     C                   EVAL      VIDOPZ = *blanks
032900080917     C                   EVAL      VIDCLIVAS = *zeros
033000100312     C                   EVAL      VIDTIP    = *blanks
033100050823     C                   ENDIF
033200050823     C*
033300020412     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* F12GES - GESTIONE F12 -> RITORNO
033600970526     C*------------------------------------------------------------------------*
033700970526     C     F12GES        BEGSR
033800970526     C*
033900970526     C* IMPOSTA "PREMUTO F12"
034000050823     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
034100970526     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
034500970526     C*------------------------------------------------------------------------*
034600970526     C     F24GES        BEGSR
034700970526     C*
034800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
034900970526     C*
035000970526     C                   ENDSR
035100970526     C*------------------------------------------------------------------------*
035200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
035300970526     C*------------------------------------------------------------------------*
035400970526     C     IMPFUN        BEGSR
035500970526     C*
035600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
035700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
035800970526     C                   EXSR      FUNGES
035900970526     C*
036000970526     C                   ENDSR
036100970526     C*------------------------------------------------------------------------*
036200970526     C* *INZSR - OPERAZIONI INIZIALI
036300970526     C*------------------------------------------------------------------------*
036400970526     C     *INZSR        BEGSR
036500970526     C*---
036600970526     C* RICEVIMENTO PARAMETRI
036700970526     C*---
036800970526     C     *ENTRY        PLIST
036900970612     C                   PARM                    KPJBA
037000050823     C*
037100050823     C* CHIAVI DI LETTURA
037200050823     C     KEYVIR2_P     KLIST
037300050823     C                   KFLD                    VIRKSC
037400050823     C                   KFLD                    VIRTIP
037500970526     C*
037600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
037700970526     C     XMSGPA        PLIST
037800970526     C                   PARM                    DSMSMS
037900970526     C                   PARM                    DSMSPG
038000970526     C                   PARM                    DSMSKE
038100970526     C                   PARM                    DSMSER
038200970526     C*
038300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
038400970526     C                   CALL      'XKEY'
038500970526     C                   PARM                    DSKEY
038600970526     C*---
038700970526     C* VARIABILI NON RIFERITE AL DATA BASE
038800970526     C*---
038900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
039000970526     C*---
039100970526     C* VARIABILI DI CONTROLLO
039200970526     C*---
039300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
039400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
039500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
039600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
039700970526     C*
039800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
039900050831     C                   MOVEL     'TIS7VSR1'    DSMSPG
040000050831     C                   MOVEL     'TIS7VSR1'    MSGQUE
040100970526     C                   MOVEL     *BLANKS       MSGKEY
040200970526     C                   MOVEL     *BLANKS       DSMSMS
040300970526     C*
040400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
040500970526     C                   EXSR      IMPFUN
040600970526     C*
040700970526     C                   ENDSR
040800970526     C*------------------------------------------------------------------------*
040900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
041000970526     C*------------------------------------------------------------------------*
041100970526     C     FUNGES        BEGSR
041200970526     C*
041300970526     C* PULISCO CAMPI DI LAVORO
041400970526     C                   Z-ADD     0             W                 5 0
041500970526     C*
041600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
041700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
041800970526     C     1             DO        24            I
041900970526     C                   MOVEL     FUD(I)        FUDRIG
042000970526     C*
042100970526     C* PULIZIA FUNZIONI PRECEDENTI
042200970526     C                   MOVEL     *BLANKS       FUV(I)
042300970526     C*
042400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
042500970526     C* (SALTA F24)
042600970526     C     FUDATT        IFEQ      'S'
042700970526     C     FUDFUN        ANDNE     '24'
042800970526     C                   ADD       1             W
042900970526     C                   MOVEL     FUDDES        FUV(W)
043000970526     C                   END
043100970526     C                   END                                                    DO WHILE
043200970526     C*
043300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
043400970526     C* ALTRIMENTI DISATTIVO F24
043500970526     C     1             DO        24            I
043600970526     C                   MOVEL     FUD(I)        FUDRIG
043700970526     C*
043800970526     C     FUDFUN        IFEQ      '24'
043900970526     C     W             IFGT      6
044000970526     C                   MOVEL     'S'           FUDATT
044100970526     C                   ELSE
044200970526     C                   MOVEL     *BLANKS       FUDATT
044300970526     C                   END                                                    W GT 8
044400970526     C                   MOVEL     FUDRIG        FUD(I)
044500970526     C                   END
044600970526     C                   END                                                    DO WHILE
044700970526     C*
044800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
044900970526     C     W             DIV       6             I
045000970526     C                   MVR                     Z
045100970526     C*
045200970526     C     Z             IFNE      0
045300970526     C     I             ADD       1             Z
045400970526     C                   ELSE
045500970526     C                   Z-ADD     I             Z                 5 0
045600970526     C                   END
045700970526     C*
045800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
045900970526     C                   ADD       1             FUNGRU
046000970526     C     FUNGRU        IFGT      Z
046100970526     C     FUNGRU        ORLT      1
046200970526     C                   Z-ADD     1             FUNGRU
046300970526     C                   END
046400970526     C*
046500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
046600970526     C     FUNGRU        IFEQ      1
046700970526     C                   MOVEL     FUV(1)        FUN001
046800970526     C                   MOVEL     FUV(2)        FUN002
046900970526     C                   MOVEL     FUV(3)        FUN003
047000970526     C                   MOVEL     FUV(4)        FUN004
047100970526     C                   MOVEL     FUV(5)        FUN005
047200970526     C                   MOVEL     FUV(6)        FUN006
047300970526     C                   END
047400970526     C*
047500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
047600970526     C     FUNGRU        IFEQ      2
047700970526     C                   MOVEL     FUV(7)        FUN001
047800970526     C                   MOVEL     FUV(8)        FUN002
047900970526     C                   MOVEL     FUV(9)        FUN003
048000970526     C                   MOVEL     FUV(10)       FUN004
048100970526     C                   MOVEL     FUV(11)       FUN005
048200970526     C                   MOVEL     FUV(12)       FUN006
048300970526     C                   END
048400970526     C*
048500970526     C* TERZO GRUPPO DI 6 FUNZIONI
048600970526     C     FUNGRU        IFEQ      3
048700970526     C                   MOVEL     FUV(13)       FUN001
048800970526     C                   MOVEL     FUV(14)       FUN002
048900970526     C                   MOVEL     FUV(15)       FUN003
049000970526     C                   MOVEL     FUV(16)       FUN004
049100970526     C                   MOVEL     FUV(17)       FUN005
049200970526     C                   MOVEL     FUV(18)       FUN006
049300970526     C                   END
049400970526     C*
049500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
049600970526     C     FUNGRU        IFEQ      4
049700970526     C                   MOVEL     FUV(19)       FUN001
049800970526     C                   MOVEL     FUV(20)       FUN002
049900970526     C                   MOVEL     FUV(21)       FUN003
050000970526     C                   MOVEL     FUV(22)       FUN004
050100970526     C                   MOVEL     *BLANKS       FUN005
050200970526     C                   MOVEL     *BLANKS       FUN006
050300970526     C                   END
050400970526     C*
050500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
050600970526     C                   MOVEL     *BLANKS       FUN007
050700970526     C                   MOVEL     *BLANKS       FUN008
050800970526     C*
050900970526     C     1             DO        24            I
051000970526     C                   MOVEL     FUD(I)        FUDRIG
051100970526     C     FUDFUN        IFEQ      '24'
051200970526     C     FUDATT        ANDEQ     'S'
051300970526     C                   MOVEL     FUDDES        FUN008
051400970526     C                   END
051500970526     C                   END                                                    DO
051600970526     C*
051700970526     C                   ENDSR
051800970526     C*------------------------------------------------------------------------*
051900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
052000970526     C*------------------------------------------------------------------------*
052100970526     C     FUNCON        BEGSR
052200970526     C*
052300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
052400970526     C                   EXSR      FUNVAR
052500970526     C*
052600970526     C* PULISCO CAMPI DI LAVORO
052700970526     C                   Z-ADD     0             I                 5 0
052800970526     C                   Z-ADD     0             W                 5 0
052900970526     C*
053000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
053100970526     C     FUNKEY        IFNE      *BLANKS
053200970526     C     1             DO        24            I
053300970526     C                   MOVEL     FUD(I)        FUDRIG
053400970526     C*
053500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
053600970526     C     FUNKEY        IFEQ      FUDFUN
053700970526     C     FUDATT        ANDEQ     'S'
053800970526     C                   Z-ADD     I             W
053900970526     C                   END
054000970526     C                   END                                                    DO WHILE
054100970526     C*
054200970526     C* FUNZIONE NON ATTIVA (ERRORE)
054300970526     C     W             IFEQ      0
054400970526     C*
054500970526     C* EMISSIONE MESSAGGIO DI ERRORE
054600970526     C                   MOVEL     MSG(1)        DSMSMS
054700970526     C*
054800970526     C                   END                                                    W=0
054900970526     C                   END                                                    FUNKEY NE *BLANKS
055000970526     C*
055100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
055200970526     C     W             IFNE      0
055300970526     C     FUNKEY        OREQ      *BLANKS
055400970526     C     FUNKEY        IFEQ      *BLANKS
055500970526     C                   EXSR      ENTGES
055600970526     C                   END
055700970526     C     FUNKEY        IFEQ      '03'
055800970526     C                   EXSR      F03GES
055900970526     C                   END
056000970526     C     FUNKEY        IFEQ      '06'
056100970526     C                   EXSR      F06GES
056200970526     C                   END
056300970526     C     FUNKEY        IFEQ      '12'
056400970526     C                   EXSR      F12GES
056500970526     C                   END
056600970526     C     FUNKEY        IFEQ      '24'
056700970526     C                   EXSR      F24GES
056800970526     C                   END
056900970526     C                   END
057000970526     C*
057100970526     C                   ENDSR
057200970526     C*------------------------------------------------------------------------*
057300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
057400970526     C*------------------------------------------------------------------------*
057500970526     C     FUNVAR        BEGSR
057600970526     C*
057700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
057800970526     C*
057900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
058000970526     C     KEY           IFEQ      ENTER
058100970526     C                   MOVEL     *BLANKS       FUNKEY
058200970526     C                   END
058300970526     C     KEY           IFEQ      F1
058400970526     C                   MOVEL     '01'          FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F2
058700970526     C                   MOVEL     '02'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F3
059000970526     C                   MOVEL     '03'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F4
059300970526     C                   MOVEL     '04'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      F5
059600970526     C                   MOVEL     '05'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      F6
059900970526     C                   MOVEL     '06'          FUNKEY
060000970526     C                   END
060100970526     C     KEY           IFEQ      F7
060200970526     C                   MOVEL     '07'          FUNKEY
060300970526     C                   END
060400970526     C     KEY           IFEQ      F8
060500970526     C                   MOVEL     '08'          FUNKEY
060600970526     C                   END
060700970526     C     KEY           IFEQ      F9
060800970526     C                   MOVEL     '09'          FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F10
061100970526     C                   MOVEL     '10'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      F11
061400970526     C                   MOVEL     '11'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      F12
061700970526     C                   MOVEL     '12'          FUNKEY
061800970526     C                   END
061900970526     C     KEY           IFEQ      F13
062000970526     C                   MOVEL     '13'          FUNKEY
062100970526     C                   END
062200970526     C     KEY           IFEQ      F14
062300970526     C                   MOVEL     '14'          FUNKEY
062400970526     C                   END
062500970526     C     KEY           IFEQ      F15
062600970526     C                   MOVEL     '15'          FUNKEY
062700970526     C                   END
062800970526     C     KEY           IFEQ      F16
062900970526     C                   MOVEL     '16'          FUNKEY
063000970526     C                   END
063100970526     C     KEY           IFEQ      F17
063200970526     C                   MOVEL     '17'          FUNKEY
063300970526     C                   END
063400970526     C     KEY           IFEQ      F18
063500970526     C                   MOVEL     '18'          FUNKEY
063600970526     C                   END
063700970526     C     KEY           IFEQ      F19
063800970526     C                   MOVEL     '19'          FUNKEY
063900970526     C                   END
064000970526     C     KEY           IFEQ      F20
064100970526     C                   MOVEL     '20'          FUNKEY
064200970526     C                   END
064300970526     C     KEY           IFEQ      F21
064400970526     C                   MOVEL     '21'          FUNKEY
064500970526     C                   END
064600970526     C     KEY           IFEQ      F22
064700970526     C                   MOVEL     '22'          FUNKEY
064800970526     C                   END
064900970526     C     KEY           IFEQ      F23
065000970526     C                   MOVEL     '23'          FUNKEY
065100970526     C                   END
065200970526     C     KEY           IFEQ      F24
065300970526     C                   MOVEL     '24'          FUNKEY
065400970526     C                   END
065500970526     C     KEY           IFEQ      ROLLUP
065600970526     C                   MOVEL     'RU'          FUNKEY
065700970526     C                   END
065800970526     C     KEY           IFEQ      HELP
065900970526     C                   MOVEL     'HP'          FUNKEY
066000970526     C                   END
066100970526     C                   ENDSR
066200970526     C*------------------------------------------------------------------------*
066300970526     C* NOTE COSTRUZIONE SCHIERE
066400970526     C*------------------------------------------------------------------------*
066500970526     C*
066600970526     C* -MSG-MESSAGGI
066700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
066800970526     C* (NON TOCCARE MSG,1 E MSG,2)
066900970526     C*
067000970526     C* -CMD-COMANDI
067100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
067200970526     C*
067300970526     C* -OPD-OPZIONI
067400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
067500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
067600970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
067700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
067800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
067900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
068000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
068100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
068200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
068300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
068400970526     C*
068500970526     C* -FUD-FUNZIONI
068600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
068700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
068800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
068900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
069000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
069300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
069400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
069500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
069600970526     O*------------------------------------------------------------------------*
069700970528** MSG - MESSAGGI
069800050823Funzione non abilitata.                                                        1
069900100312Cliente indicato NON è abilitato allo scambio dati VAG del tipo file indicato  2
070000050823Codice cliente obbligatorio.                                                   3
070100050907Modalità creazione obbligatoria.                                               4
070200100312Indicare O range periodo pagamento O range periodo spedizioni                  5
070300100312Range periodo pagamento O range periodo spedizioni obbligatori                 6
070400050816                                                                               7
070500050816                                                                               8
070600050816                                                                               9
070700050816                                                                               10
070800050816                                                                               11
070900050816                                                                               12
071000050816                                                                               13
071100050816                                                                               14
071200050816                                                                               15
071300050816                                                                               16
071400050816                                                                               17
071500020415                                                                               18
071600020415                                                                               19
071700020415                                                                               20
071800970526** FUD - FUNZIONI
071900970526                   -        (S/N)=N                        funz.01¬
072000970526                   -        (S/N)=N                        funz.02¬
072100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
072200970526                   -        (S/N)=N                        funz.04¬
072300970526                   -        (S/N)=N                        funz.05¬
072400050823F06=Conferma       -        (S/N)=S                        funz.06¬
072500050823                   -        (S/N)=N                        funz.07¬
072600970527                   -        (S/N)=N                        funz.08¬
072700970527                   -        (S/N)=N                        funz.09¬
072800970527                   -        (S/N)=N                        funz.10¬
072900970526                   -        (S/N)=N                        funz.11¬
073000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
073100970526                   -        (S/N)=N                        funz.13¬
073200970526                   -        (S/N)=N                        funz.14¬
073300970526                   -        (S/N)=N                        funz.15¬
073400970526                   -        (S/N)=N                        funz.16¬
073500970526                   -        (S/N)=N                        funz.17¬
073600970526                   -        (S/N)=N                        funz.18¬
073700970526                   -        (S/N)=N                        funz.19¬
073800970526                   -        (S/N)=N                        funz.20¬
073900970526                   -        (S/N)=N                        funz.21¬
074000970526                   -        (S/N)=N                        funz.22¬
074100970526                   -        (S/N)=N                        funz.23¬
074200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
