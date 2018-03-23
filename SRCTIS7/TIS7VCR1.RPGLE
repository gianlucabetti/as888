000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600050831     FTIS7VCD1  CF   E             WORKSTN INFDS(DEVDS1)
000700050831     F                                     SFILE(S7VCDMS:MSGULT)
000800050823     FTIVIR02L  IF   E           K DISK
000900150202     FTIVGS02L  IF   E           K DISK
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D*------------
001400970526     D* MESSAGGI
001500970526     D*------------
001600970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
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
002800970526     D*------------
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000060807     D*------------
005100060807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
005200060807     D*-----------
005300060807     D DSXC          E DS                  extname(XCLIRDS)
005400060807     D KSC             S              7  0 dim(30)
005500060807     D KSA             S              4    dim(30)
005600060807     D*
005700060807     D                 DS
005800060807     D DSKSN                   1      4P 0
005900060807     D DSKSA                   1      4
006000060807     D*
006100060807     DBS69DS         E DS                  extname(TIBS69DS)
006200060807     DACODS          E DS                  extname(cnaco00f)
006300060807     DINDDS          E DS                  extname(cnind00f)
006400060807     DCLPDS          E DS                  extname(cnclp00f)
006500060807     DCLSDS          E DS                  extname(fncls00f)
006600020415     D*------------
006700970526     D* ARCHITETTURA
006800970526     D*------------
006900970526     D KPJBA         E DS
007000050823     D*--------------------
007100050823     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
007200050823     D*--------------------
007300050823     D PARAM           DS
007400050823     D  DATADA                        8  0
007500050823     D  DATAA                         8  0
007600050823     D  CODCLI                        7  0
007700050823     D  FLGUNI                        1
007800050823     D  FLGCONS                       1
007900050823     D  FLGVAS                        1
008000050823     D  FLGOPZ                        1
008100150707     D  CODCLIVAS                     7
008200100312     D  TIPFILE                       2
008300070627     D  FLGEXE                        1
008400070718     D  TIPCLI                        1
008500071228     D  FLGNOE                        1
008600081016     D  KAAS                          4  0
008700081016     D  KLNP                          3  0
008800081016     D  KNRS                          2  0
008900081016     D  KNSP                          7  0
009000100512     D  DACMDA                        8  0
009100100512     D  DACMA                         8  0
009200970526     C*------------------------------------------------------------------------*
009300970526     C* MAIN LINES
009400970526     C*------------------------------------------------------------------------*
009500970526     C*
009600970526     C* NON CI SONO ERRORI IN ENTRATA
009700970526IF  1C     ERRL00        IFEQ      '0'
009800050823     C*
009900050823     C* INIZIALIZZA LA VIDEATA
010000050823     C                   EXSR      AZZVID
010100970526     C*
010200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010600970526     C*
010700970526     C* EMETTE FORMATO VIDEO
010800970526     C                   EXSR      BUTFMR
010900970526     C*
011000970526     C* GESTIONE TASTI FUNZIONALI
011100970526     C                   EXSR      FUNCON
011200970526E   2C                   ENDDO
011300970526     C*
011400970526E   1C                   ENDIF
011500970526     C* FINE PGM
011600970526     C                   SETON                                        LR
011700970526     C*------------------------------------------------------------------------*
011800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011900970526     C*------------------------------------------------------------------------*
012000970526     C     BUTFMR        BEGSR
012100970526     C*
012200970526     C* EMETTE LA TESTATA
012300050831     C                   WRITE     S7VCD01
012400970526     C*
012500970526     C* SPEDISCE EVENTUALE MESSAGGIO
012600970526     C                   EXSR      SNDMSG
012700970526     C*
012800970526     C* EMETTE LE FUNZIONI
012900050831     C                   WRITE     S7VCD02
013000970526     C*
013100970526     C* EMETTE IL VIDEO
013200050831     C                   EXFMT     S7VCD03
013300970526     C*
013400970526     C* REIMPOSTA FLAG DI ERRORE
013500970526     C                   MOVEL     '0'           ERRL00
013600970526     C*
013700970526     C                   ENDSR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014000970526     C*------------------------------------------------------------------------*
014100970526     C     SNDMSG        BEGSR
014200970526     C*
014300970526     C* PULISCE IL SFILE MESSAGGI
014400970526     C                   MOVEL     '1'           *IN40
014500050831     C                   WRITE     S7VCDMC
014600970526     C                   MOVEL     '0'           *IN40
014700970526     C*
014800970526     C* VISUALIZZA IL MESSAGGIO
014900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015000970526     C                   CALL      'XMSG'        XMSGPA
015100970526IF  1C     DSMSER        IFEQ      *ZEROS
015200970526     C                   MOVEL     DSMSKE        MSGKEY
015300970526E   1C                   ENDIF
015400970526     C                   Z-ADD     1             MSGULT
015500050831     C                   WRITE     S7VCDMS
015600050831     C                   WRITE     S7VCDMC
015700970526     C*
015800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015900970526     C                   MOVEL     *BLANKS       DSMSMS
016000970526     C*
016100970526     C                   ENDSR
016200050823     C*------------------------------------------------------------------------*
016300050823     C* AZZVID - INIZIALIZZAZIONE VIDEATA
016400050823     C*------------------------------------------------------------------------*
016500050823     C     AZZVID        BEGSR
016600050823     C*
016700070525     C                   EVAL      VIDUNI    = 'U'
016800070525     C                   EVAL      VIDBOL    = 'N'
016900071228     C                   EVAL      VIDNOE    = 'N'
017000070525     C                   EVAL      VIDVAS    = 'N'
017100070525     C                   EVAL      VIDOPZ    = *blanks
017200070525     C                   EVAL      VIDCLIVAS = *zeros
017300100312     C                   EVAL      VIDTIP    = *blanks
017400050823     C*
017500050823     C                   ENDSR
017600050823     C*------------------------------------------------------------------------*
017700050823     C* DECODI - DECODIFICHE E CONTROLLI A VIDEO
017800050823     C*------------------------------------------------------------------------*
017900050823     C     DECODI        BEGSR
018000050823     C*
018100050823     C* Inizializzazione errori
018200100312     C                   SETOFF                                       505152
018300150202     C                   SETOFF                                       5354
018400050823     C                   EVAL      ERRL00 = '0'
018500100512     C*
018600100512     C* Verifico esplementarietà date spedizione/consegna
018700100512     C                   IF        ERRL00 = '0'
018800100512     C                   IF        (VIDDAD > *zeros OR VIDDAA > *zeros) AND
018900100512     C                             (VIDDCD > *zeros OR VIDDCA > *zeros)
019000100512     C                   MOVEL     MSG(5)        DSMSMS
019100100512     C                   SETON                                        53
019200100512     C                   EVAL      ERRL00 = '1'
019300100512     C                   ENDIF
019400100512     C                   ENDIF
019500101028     C*
019600101028     C* Verifico che un range data sia stato indicato
019700101028     C                   IF        ERRL00 = '0'
019800101028     C                   IF        VIDDAD = *zeros AND VIDDAA = *zeros AND
019900101028     C                             VIDDCD = *zeros AND VIDDCA = *zeros
020000101028     C                   MOVEL     MSG(5)        DSMSMS
020100101028     C                   SETON                                        53
020200101028     C                   EVAL      ERRL00 = '1'
020300101028     C                   ENDIF
020400101028     C                   ENDIF
020500101028     C*
020600101028     C* Verifico "completezza" range data
020700101028     C                   IF        ERRL00 = '0'
020800101028     C                   IF        (VIDDAD > *zeros AND VIDDAA = *zeros) OR
020900101028     C                             (VIDDAD = *zeros AND VIDDAA > *zeros)
021000101028     C                   MOVEL     MSG(5)        DSMSMS
021100101028     C                   SETON                                        53
021200101028     C                   EVAL      ERRL00 = '1'
021300101028     C                   ENDIF
021400101028     C                   ENDIF
021500101028     C                   IF        ERRL00 = '0'
021600101028     C                   IF        (VIDDCD > *zeros AND VIDDCA = *zeros) OR
021700101028     C                             (VIDDCD = *zeros AND VIDDCA > *zeros)
021800101028     C                   MOVEL     MSG(5)        DSMSMS
021900101028     C                   SETON                                        53
022000101028     C                   EVAL      ERRL00 = '1'
022100101028     C                   ENDIF
022200101028     C                   ENDIF
022300060807     C*
022400060807     C* Richiesta scelta codice cliente
022500060807     C                   IF        ERRL00 = '0'
022600060807     C                   SETOFF                                           28
022700060807     C     '?'           SCAN      VIDCLI                                 28
022800060807     C     *IN28         IFEQ      *ON
022900060807     C* CLIENTI
023000060807     C                   CLEAR                   DSXC
023100060807     C                   Z-ADD     151           DXCCAP
023200060807     C                   CALL      'XCLIR'
023300060807     C                   PARM                    DSXC
023400060807     C* SCOMPATTA
023500060807     C                   MOVEA     DXCKSC        KSA
023600060807     C     1             DO        30            J                 3 0
023700060807     C                   MOVE      KSA(J)        DSKSA
023800060807     C                   Z-ADD     DSKSN         KSC(J)
023900060807     C                   ENDDO
024000060807     C                   MOVEL     KSC(1)        VIDCLI
024100060807     C                   ENDIF
024200060807     C                   ENDIF
024300050823     C*
024400050823     C* Verifico obbligatorietà indicazione cliente
024500050907     C                   IF        ERRL00 = '0'
024600060807     C                   IF        VIDCLI = *zeros OR
024700060807     C                             VIDCLI = *blanks
024800050823     C                   MOVEL     MSG(3)        DSMSMS
024900100312     C                   SETON                                        52
025000050823     C                   EVAL      ERRL00 = '1'
025100060807     C                   ELSE
025200060807     C                   CLEAR                   BS69DS
025300060807     C                   CLEAR                   ACODS
025400060807     C                   MOVEL     KNSIF         I69SIF
025500060807     C                   MOVE(P)   VIDCLI        I69KAC
025600060807     C                   CALL      'TIBS69R'
025700060807     C                   PARM                    BS69DS
025800060807     C                   PARM                    ACODS
025900060807     C                   PARM                    INDDS
026000060807     C                   PARM                    CLPDS
026100060807     C                   PARM                    CLSDS
026200060807     C     O69ERR        IFNE      '1'
026300060807     C                   MOVEL     ACORAG        VIDCLID
026400060807     C                   ELSE
026500060807     C                   MOVEL     *ALL'*'       VIDCLID
026600060807     C                   ENDIF
026700050823     C                   ENDIF
026800050907     C                   ENDIF
026900070525     C*
027000070525     C* Se richiesta opzione modalità creazione 2 => verifico che il cliente selezionato
027100070525     C* sia abilitato allo scambio dati del VAC (tipo file VC)
027200070525     C                   IF        ERRL00 = '0'
027300070525     C                   IF        VIDOPZ = '2'
027400070525     C                   EVAL      VIRKSC = *all'0'
027500070525     C                   IF        VIDCLIVAS = *ZEROS
027600070525     C                   MOVE      VIDCLI        VIDCLIVAS
027700070525     C                   ENDIF
027800070525     C                   MOVE      VIDCLIVAS     VIRKSC
027900100312     C                   IF        VIDTIP = *BLANKS
028000100312     C                   EVAL      VIDTIP = 'VC'
028100100312     C                   ENDIF
028200100312     C                   MOVEL     VIDTIP        VIRTIP
028300070525     C     KEYVIR2_P     CHAIN     TIVIR02L
028400070525     C                   IF        not %found(TIVIR02L)
028500070525     C                   MOVEL     MSG(2)        DSMSMS
028600070525     C                   SETON                                        50
028700070525     C                   EVAL      ERRL00 = '1'
028800070525     C                   ENDIF
028900070525     C                   ENDIF
029000070525     C                   ENDIF
029100050907     C*
029200050907     C* Verifico obbligatorietà indicazione opzione generazione dati
029300050907     C                   IF        ERRL00 = '0'
029400050907     C                   IF        VIDOPZ = *blanks
029500050907     C                   MOVEL     MSG(4)        DSMSMS
029600050907     C                   SETON                                        51
029700050907     C                   EVAL      ERRL00 = '1'
029800050907     C                   ENDIF
029900050907     C                   ENDIF
030000150202     C*
030100150202     C* Se richiesta opzione modalità creazione 2 => verifico che il tipo file indicato
030200150202     C* sia libero da "semaforizzazioni"
030300150202     C                   SETOFF                                       54
030400150202     C                   IF        ERRL00 = '0'
030500150202     C                   IF        VIDOPZ = '2'
030600150202     C     VIDTIP        CHAIN     TIVGS02L
030700150202     C                   IF        %found(TIVGS02L)
030800150202     C                   MOVEL     MSG(6)        DSMSMS
030900150202     C                   SETON                                        54
031000150202     C                   EVAL      ERRL00 = '1'
031100150202     C                   ENDIF
031200150202     C                   ENDIF
031300150202     C                   ENDIF
031400050823     C*
031500050823     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* ENTGES - GESTIONE ENTER -> INVIO
031800970526     C*------------------------------------------------------------------------*
031900970526     C     ENTGES        BEGSR
032000050823     C*
032100050823     C* Eseguo decodifiche e controlli a video
032200050823     C                   EXSR      DECODI
032300970526     C*
032400970526     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600970526     C* F03GES - GESTIONE F03 -> FINE
032700970526     C*------------------------------------------------------------------------*
032800970526     C     F03GES        BEGSR
032900970526     C*
033000970526     C* IMPOSTA "PREMUTO F03"
033100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500050825     C* F06GES - GESTIONE F06 -> CONFERMA OPZIONE
033600970526     C*------------------------------------------------------------------------*
033700050816     C     F06GES        BEGSR
033800050823     C*
033900050823     C* Eseguo decodifiche e controlli a video
034000050823     C                   EXSR      DECODI
034100050823     C*
034200050823     C* Se tutto ok procedo
034300050823     C                   IF        ERRL00 = '0'
034400050823     C*
034500050823     C* Valorizzo la DS d passaggio parametri
034600070525     C                   EVAL      DATADA    = VIDDAD
034700070525     C                   EVAL      DATAA     = VIDDAA
034800100512     C                   EVAL      DACMDA    = VIDDCD
034900100512     C                   EVAL      DACMA     = VIDDCA
035000060807     C                   MOVE(P)   VIDCLI        CODCLI
035100070525     C                   EVAL      FLGUNI    = VIDUNI
035200070525     C                   EVAL      FLGCONS   = VIDBOL
035300071228     C                   EVAL      FLGNOE    = VIDNOE
035400070525     C                   EVAL      FLGVAS    = VIDVAS
035500070525     C                   EVAL      FLGOPZ    = VIDOPZ
035600070627     C                   EVAL      FLGEXE    = VIDEXE
035700070718     C                   EVAL      TIPCLI    = VIDTIPCLI
035800070525     C                   IF        VIDCLIVAS > *ZEROS
035900150707     C                   MOVE(P)   VIDCLIVAS     CODCLIVAS
036000070525     C                   ELSE
036100070525     C                   MOVE(P)   VIDCLI        CODCLIVAS
036200070525     C                   ENDIF
036300100312     C                   EVAL      TIPFILE   = VIDTIP
036400081016     C                   EVAL      KAAS      = VIDKAAS
036500081016     C                   EVAL      KLNP      = VIDKLNP
036600081016     C                   EVAL      KNRS      = VIDKNRS
036700081016     C                   EVAL      KNSP      = VIDKNSP
036800020412     C*
036900050823     C* Sottometto l'azione di elaborazione
037000050823     C                   MOVEL     'S7V3'        KCOAZ
037100050823     C                   MOVEL     PARAM         KPJBU
037200070627     C                   IF        VIDEXE = *blanks
037300070525     C                   CALL      'BCH10'
037400070627     C                   PARM                    KPJBA
037500070627     C                   ELSE
037600070627     C                   CALL      'TIS7VCC'
037700050823     C                   PARM                    KPJBA
037800070627     C                   ENDIF
037900051110     C***                MOVEL     '1'           F03L00                         *FINE PROGRAMMA
038000070525     C                   EVAL      VIDCLI    = *zeros
038100070525     C                   EVAL      VIDUNI    = 'U'
038200070525     C                   EVAL      VIDBOL    = 'N'
038300071228     C                   EVAL      VIDNOE    = 'N'
038400070525     C                   EVAL      VIDVAS    = 'N'
038500070525     C                   EVAL      VIDOPZ    = *blanks
038600070525     C                   EVAL      VIDCLIVAS = *zeros
038700100312     C                   EVAL      VIDTIP    = *blanks
038800081016     C                   EVAL      VIDKAAS   = *zeros
038900081016     C                   EVAL      VIDKLNP   = *zeros
039000081016     C                   EVAL      VIDKNRS   = *zeros
039100081016     C                   EVAL      VIDKNSP   = *zeros
039200050823     C                   ENDIF
039300050823     C*
039400020412     C                   ENDSR
039500970526     C*------------------------------------------------------------------------*
039600970526     C* F12GES - GESTIONE F12 -> RITORNO
039700970526     C*------------------------------------------------------------------------*
039800970526     C     F12GES        BEGSR
039900970526     C*
040000970526     C* IMPOSTA "PREMUTO F12"
040100050823     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
040200970526     C*
040300970526     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
040600970526     C*------------------------------------------------------------------------*
040700970526     C     F24GES        BEGSR
040800970526     C*
040900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
041000970526     C*
041100970526     C                   ENDSR
041200970526     C*------------------------------------------------------------------------*
041300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
041400970526     C*------------------------------------------------------------------------*
041500970526     C     IMPFUN        BEGSR
041600970526     C*
041700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
041800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
041900970526     C                   EXSR      FUNGES
042000970526     C*
042100970526     C                   ENDSR
042200970526     C*------------------------------------------------------------------------*
042300970526     C* *INZSR - OPERAZIONI INIZIALI
042400970526     C*------------------------------------------------------------------------*
042500970526     C     *INZSR        BEGSR
042600970526     C*---
042700970526     C* RICEVIMENTO PARAMETRI
042800970526     C*---
042900970526     C     *ENTRY        PLIST
043000970612     C                   PARM                    KPJBA
043100050823     C*
043200050823     C* CHIAVI DI LETTURA
043300050823     C     KEYVIR2_P     KLIST
043400050823     C                   KFLD                    VIRKSC
043500050823     C                   KFLD                    VIRTIP
043600970526     C*
043700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
043800970526     C     XMSGPA        PLIST
043900970526     C                   PARM                    DSMSMS
044000970526     C                   PARM                    DSMSPG
044100970526     C                   PARM                    DSMSKE
044200970526     C                   PARM                    DSMSER
044300970526     C*
044400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
044500970526     C                   CALL      'XKEY'
044600970526     C                   PARM                    DSKEY
044700970526     C*---
044800970526     C* VARIABILI NON RIFERITE AL DATA BASE
044900970526     C*---
045000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
045100970526     C*---
045200970526     C* VARIABILI DI CONTROLLO
045300970526     C*---
045400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
045500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
045600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
045700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
045800970526     C*
045900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
046000050831     C                   MOVEL     'TIS7VCR1'    DSMSPG
046100050831     C                   MOVEL     'TIS7VCR1'    MSGQUE
046200970526     C                   MOVEL     *BLANKS       MSGKEY
046300970526     C                   MOVEL     *BLANKS       DSMSMS
046400970526     C*
046500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
046600970526     C                   EXSR      IMPFUN
046700970526     C*
046800970526     C                   ENDSR
046900970526     C*------------------------------------------------------------------------*
047000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
047100970526     C*------------------------------------------------------------------------*
047200970526     C     FUNGES        BEGSR
047300970526     C*
047400970526     C* PULISCO CAMPI DI LAVORO
047500970526     C                   Z-ADD     0             W                 5 0
047600970526     C*
047700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
047800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
047900970526     C     1             DO        24            I
048000970526     C                   MOVEL     FUD(I)        FUDRIG
048100970526     C*
048200970526     C* PULIZIA FUNZIONI PRECEDENTI
048300970526     C                   MOVEL     *BLANKS       FUV(I)
048400970526     C*
048500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
048600970526     C* (SALTA F24)
048700970526     C     FUDATT        IFEQ      'S'
048800970526     C     FUDFUN        ANDNE     '24'
048900970526     C                   ADD       1             W
049000970526     C                   MOVEL     FUDDES        FUV(W)
049100970526     C                   END
049200970526     C                   END                                                    DO WHILE
049300970526     C*
049400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
049500970526     C* ALTRIMENTI DISATTIVO F24
049600970526     C     1             DO        24            I
049700970526     C                   MOVEL     FUD(I)        FUDRIG
049800970526     C*
049900970526     C     FUDFUN        IFEQ      '24'
050000970526     C     W             IFGT      6
050100970526     C                   MOVEL     'S'           FUDATT
050200970526     C                   ELSE
050300970526     C                   MOVEL     *BLANKS       FUDATT
050400970526     C                   END                                                    W GT 8
050500970526     C                   MOVEL     FUDRIG        FUD(I)
050600970526     C                   END
050700970526     C                   END                                                    DO WHILE
050800970526     C*
050900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
051000970526     C     W             DIV       6             I
051100970526     C                   MVR                     Z
051200970526     C*
051300970526     C     Z             IFNE      0
051400970526     C     I             ADD       1             Z
051500970526     C                   ELSE
051600970526     C                   Z-ADD     I             Z                 5 0
051700970526     C                   END
051800970526     C*
051900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
052000970526     C                   ADD       1             FUNGRU
052100970526     C     FUNGRU        IFGT      Z
052200970526     C     FUNGRU        ORLT      1
052300970526     C                   Z-ADD     1             FUNGRU
052400970526     C                   END
052500970526     C*
052600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
052700970526     C     FUNGRU        IFEQ      1
052800970526     C                   MOVEL     FUV(1)        FUN001
052900970526     C                   MOVEL     FUV(2)        FUN002
053000970526     C                   MOVEL     FUV(3)        FUN003
053100970526     C                   MOVEL     FUV(4)        FUN004
053200970526     C                   MOVEL     FUV(5)        FUN005
053300970526     C                   MOVEL     FUV(6)        FUN006
053400970526     C                   END
053500970526     C*
053600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
053700970526     C     FUNGRU        IFEQ      2
053800970526     C                   MOVEL     FUV(7)        FUN001
053900970526     C                   MOVEL     FUV(8)        FUN002
054000970526     C                   MOVEL     FUV(9)        FUN003
054100970526     C                   MOVEL     FUV(10)       FUN004
054200970526     C                   MOVEL     FUV(11)       FUN005
054300970526     C                   MOVEL     FUV(12)       FUN006
054400970526     C                   END
054500970526     C*
054600970526     C* TERZO GRUPPO DI 6 FUNZIONI
054700970526     C     FUNGRU        IFEQ      3
054800970526     C                   MOVEL     FUV(13)       FUN001
054900970526     C                   MOVEL     FUV(14)       FUN002
055000970526     C                   MOVEL     FUV(15)       FUN003
055100970526     C                   MOVEL     FUV(16)       FUN004
055200970526     C                   MOVEL     FUV(17)       FUN005
055300970526     C                   MOVEL     FUV(18)       FUN006
055400970526     C                   END
055500970526     C*
055600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
055700970526     C     FUNGRU        IFEQ      4
055800970526     C                   MOVEL     FUV(19)       FUN001
055900970526     C                   MOVEL     FUV(20)       FUN002
056000970526     C                   MOVEL     FUV(21)       FUN003
056100970526     C                   MOVEL     FUV(22)       FUN004
056200970526     C                   MOVEL     *BLANKS       FUN005
056300970526     C                   MOVEL     *BLANKS       FUN006
056400970526     C                   END
056500970526     C*
056600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
056700970526     C                   MOVEL     *BLANKS       FUN007
056800970526     C                   MOVEL     *BLANKS       FUN008
056900970526     C*
057000970526     C     1             DO        24            I
057100970526     C                   MOVEL     FUD(I)        FUDRIG
057200970526     C     FUDFUN        IFEQ      '24'
057300970526     C     FUDATT        ANDEQ     'S'
057400970526     C                   MOVEL     FUDDES        FUN008
057500970526     C                   END
057600970526     C                   END                                                    DO
057700970526     C*
057800970526     C                   ENDSR
057900970526     C*------------------------------------------------------------------------*
058000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
058100970526     C*------------------------------------------------------------------------*
058200970526     C     FUNCON        BEGSR
058300970526     C*
058400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
058500970526     C                   EXSR      FUNVAR
058600970526     C*
058700970526     C* PULISCO CAMPI DI LAVORO
058800970526     C                   Z-ADD     0             I                 5 0
058900970526     C                   Z-ADD     0             W                 5 0
059000970526     C*
059100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
059200970526     C     FUNKEY        IFNE      *BLANKS
059300970526     C     1             DO        24            I
059400970526     C                   MOVEL     FUD(I)        FUDRIG
059500970526     C*
059600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
059700970526     C     FUNKEY        IFEQ      FUDFUN
059800970526     C     FUDATT        ANDEQ     'S'
059900970526     C                   Z-ADD     I             W
060000970526     C                   END
060100970526     C                   END                                                    DO WHILE
060200970526     C*
060300970526     C* FUNZIONE NON ATTIVA (ERRORE)
060400970526     C     W             IFEQ      0
060500970526     C*
060600970526     C* EMISSIONE MESSAGGIO DI ERRORE
060700970526     C                   MOVEL     MSG(1)        DSMSMS
060800970526     C*
060900970526     C                   END                                                    W=0
061000970526     C                   END                                                    FUNKEY NE *BLANKS
061100970526     C*
061200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
061300970526     C     W             IFNE      0
061400970526     C     FUNKEY        OREQ      *BLANKS
061500970526     C     FUNKEY        IFEQ      *BLANKS
061600970526     C                   EXSR      ENTGES
061700970526     C                   END
061800970526     C     FUNKEY        IFEQ      '03'
061900970526     C                   EXSR      F03GES
062000970526     C                   END
062100970526     C     FUNKEY        IFEQ      '06'
062200970526     C                   EXSR      F06GES
062300970526     C                   END
062400970526     C     FUNKEY        IFEQ      '12'
062500970526     C                   EXSR      F12GES
062600970526     C                   END
062700970526     C     FUNKEY        IFEQ      '24'
062800970526     C                   EXSR      F24GES
062900970526     C                   END
063000970526     C                   END
063100970526     C*
063200970526     C                   ENDSR
063300970526     C*------------------------------------------------------------------------*
063400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
063500970526     C*------------------------------------------------------------------------*
063600970526     C     FUNVAR        BEGSR
063700970526     C*
063800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
063900970526     C*
064000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
064100970526     C     KEY           IFEQ      ENTER
064200970526     C                   MOVEL     *BLANKS       FUNKEY
064300970526     C                   END
064400970526     C     KEY           IFEQ      F1
064500970526     C                   MOVEL     '01'          FUNKEY
064600970526     C                   END
064700970526     C     KEY           IFEQ      F2
064800970526     C                   MOVEL     '02'          FUNKEY
064900970526     C                   END
065000970526     C     KEY           IFEQ      F3
065100970526     C                   MOVEL     '03'          FUNKEY
065200970526     C                   END
065300970526     C     KEY           IFEQ      F4
065400970526     C                   MOVEL     '04'          FUNKEY
065500970526     C                   END
065600970526     C     KEY           IFEQ      F5
065700970526     C                   MOVEL     '05'          FUNKEY
065800970526     C                   END
065900970526     C     KEY           IFEQ      F6
066000970526     C                   MOVEL     '06'          FUNKEY
066100970526     C                   END
066200970526     C     KEY           IFEQ      F7
066300970526     C                   MOVEL     '07'          FUNKEY
066400970526     C                   END
066500970526     C     KEY           IFEQ      F8
066600970526     C                   MOVEL     '08'          FUNKEY
066700970526     C                   END
066800970526     C     KEY           IFEQ      F9
066900970526     C                   MOVEL     '09'          FUNKEY
067000970526     C                   END
067100970526     C     KEY           IFEQ      F10
067200970526     C                   MOVEL     '10'          FUNKEY
067300970526     C                   END
067400970526     C     KEY           IFEQ      F11
067500970526     C                   MOVEL     '11'          FUNKEY
067600970526     C                   END
067700970526     C     KEY           IFEQ      F12
067800970526     C                   MOVEL     '12'          FUNKEY
067900970526     C                   END
068000970526     C     KEY           IFEQ      F13
068100970526     C                   MOVEL     '13'          FUNKEY
068200970526     C                   END
068300970526     C     KEY           IFEQ      F14
068400970526     C                   MOVEL     '14'          FUNKEY
068500970526     C                   END
068600970526     C     KEY           IFEQ      F15
068700970526     C                   MOVEL     '15'          FUNKEY
068800970526     C                   END
068900970526     C     KEY           IFEQ      F16
069000970526     C                   MOVEL     '16'          FUNKEY
069100970526     C                   END
069200970526     C     KEY           IFEQ      F17
069300970526     C                   MOVEL     '17'          FUNKEY
069400970526     C                   END
069500970526     C     KEY           IFEQ      F18
069600970526     C                   MOVEL     '18'          FUNKEY
069700970526     C                   END
069800970526     C     KEY           IFEQ      F19
069900970526     C                   MOVEL     '19'          FUNKEY
070000970526     C                   END
070100970526     C     KEY           IFEQ      F20
070200970526     C                   MOVEL     '20'          FUNKEY
070300970526     C                   END
070400970526     C     KEY           IFEQ      F21
070500970526     C                   MOVEL     '21'          FUNKEY
070600970526     C                   END
070700970526     C     KEY           IFEQ      F22
070800970526     C                   MOVEL     '22'          FUNKEY
070900970526     C                   END
071000970526     C     KEY           IFEQ      F23
071100970526     C                   MOVEL     '23'          FUNKEY
071200970526     C                   END
071300970526     C     KEY           IFEQ      F24
071400970526     C                   MOVEL     '24'          FUNKEY
071500970526     C                   END
071600970526     C     KEY           IFEQ      ROLLUP
071700970526     C                   MOVEL     'RU'          FUNKEY
071800970526     C                   END
071900970526     C     KEY           IFEQ      HELP
072000970526     C                   MOVEL     'HP'          FUNKEY
072100970526     C                   END
072200970526     C                   ENDSR
072300970526     C*------------------------------------------------------------------------*
072400970526     C* NOTE COSTRUZIONE SCHIERE
072500970526     C*------------------------------------------------------------------------*
072600970526     C*
072700970526     C* -MSG-MESSAGGI
072800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
072900970526     C* (NON TOCCARE MSG,1 E MSG,2)
073000970526     C*
073100970526     C* -CMD-COMANDI
073200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
073300970526     C*
073400970526     C* -OPD-OPZIONI
073500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
073600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
073700970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
073800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
073900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
074000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
074100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
074200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
074300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
074400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
074500970526     C*
074600970526     C* -FUD-FUNZIONI
074700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
074800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
074900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
075000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
075100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
075200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
075300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
075400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
075500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
075600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
075700970526     O*------------------------------------------------------------------------*
075800970528** MSG - MESSAGGI
075900050823Funzione non abilitata.                                                        1
076000130211Cliente indicato NON è abilitato allo scambio dati VAC del tipo file indicato  2
076100050823Codice cliente obbligatorio.                                                   3
076200050907Modalità creazione obbligatoria.                                               4
076300100512Indicare il range periodo O x data spedizione O x data consegna                5
076400150202Tipo file in uso al momento, attendere fine semaforizzazione.                  6
076500050816                                                                               7
076600050816                                                                               8
076700050816                                                                               9
076800050816                                                                               10
076900050816                                                                               11
077000050816                                                                               12
077100050816                                                                               13
077200050816                                                                               14
077300050816                                                                               15
077400050816                                                                               16
077500050816                                                                               17
077600020415                                                                               18
077700020415                                                                               19
077800020415                                                                               20
077900970526** FUD - FUNZIONI
078000970526                   -        (S/N)=N                        funz.01¬
078100970526                   -        (S/N)=N                        funz.02¬
078200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
078300970526                   -        (S/N)=N                        funz.04¬
078400970526                   -        (S/N)=N                        funz.05¬
078500050823F06=Conferma       -        (S/N)=S                        funz.06¬
078600050823                   -        (S/N)=N                        funz.07¬
078700970527                   -        (S/N)=N                        funz.08¬
078800970527                   -        (S/N)=N                        funz.09¬
078900970527                   -        (S/N)=N                        funz.10¬
079000970526                   -        (S/N)=N                        funz.11¬
079100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
079200970526                   -        (S/N)=N                        funz.13¬
079300970526                   -        (S/N)=N                        funz.14¬
079400970526                   -        (S/N)=N                        funz.15¬
079500970526                   -        (S/N)=N                        funz.16¬
079600970526                   -        (S/N)=N                        funz.17¬
079700970526                   -        (S/N)=N                        funz.18¬
079800970526                   -        (S/N)=N                        funz.19¬
079900970526                   -        (S/N)=N                        funz.20¬
080000970526                   -        (S/N)=N                        funz.21¬
080100970526                   -        (S/N)=N                        funz.22¬
080200970526                   -        (S/N)=N                        funz.23¬
080300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
