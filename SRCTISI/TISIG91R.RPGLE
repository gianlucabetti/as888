000100970526     H*------------------------------------------------------------------------*
000200170511     H* Estrae movimenti GABBIE e BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170502     FTISIG91D  CF   E             WORKSTN INFDS(DEVDS1)
000900030123     F                                     SFILE(SIG9DMS:MSGULT)
001000021219     Fazorg01l  IF   E           K DISK
001100021219     Fazagb01l  IF   E           K DISK
001200030618     Ftntbe01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* MESSAGGI
001800970526     D*------------
001900970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800030124     D*-------------------
002900030124     D* FILIALI DEI CLIENTI DA VISUALIZZARE
003000030124     D*-------------------
003100030124     D L1              S              3    DIM(250) INZ(*zeros)
003200970526     D*------------------------------------------------------------------------*
003300970526     D* INPUT
003400970526     D*------------------------------------------------------------------------*
003500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003600970526     D*------------
003700970526     D FUDDS           DS
003800970526     D  FUDDE1                 1      1
003900970526     D  FUDFUN                 2      3
004000970526     D  FUDDE2                 4     19
004100970526     D  FUDFI0                20     34
004200970526     D  FUDATT                35     35
004300970526     D  FUDFI1                36     80
004400970526     D  FUDDES                 1     19
004500970526     D  FUDRIG                 1     80
004600970526     D*------------
004700970526     D* DS GESTIONE TASTI FUNZIONALI
004800970526     D*------------
004900970526     D DSKEY         E DS
005000970526     D DEVDS1          DS
005100970526     D  KEY                  369    369
005200970526     D*------------
005300970526     D* DS "XMSG" - GESTIONE MESSAGGI
005400970526     D*------------
005500970526     D DSMSG         E DS
005600021219     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000170510     D*------------
006100170510     D* TABELLE "XCLIR"- RICERCA CLIENTI
006200170510     D*------------
006300170510     D KSC             S              7  0 dim(30)
006400170510     D KSA             S              4    dim(30)
006500170510     D*------------------
006600170510     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
006700170510     D*------------------
006800170510     D DSXC          E DS                  extname(XCLIRDS)
006900170510     D*------------------
007000170510     D                 DS
007100170510     D DSKSN                   1      4p 0
007200170510     D DSKSA                   1      4
007300170510     D*------------------
007400170510     D* DS REPERIMENTO DATI UTENTE
007500170510     D*-------------------
007600170510     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007700170510     D ACODS         E DS                  EXTNAME(CNACO00F)
007800170510     D INDDS         E DS                  EXTNAME(CNIND00F)
007900170510     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008000170510     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008100000407     D*------------------
008200000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
008300000407     D*------------------
008400000407     D WLBDA8          DS                  INZ
008500000407     D  G08DAT                 1      8  0
008600000407     D  G08INV                 9     16  0
008700000407     D  G08ERR                17     17
008800000407     D  G08TGI                18     22  0
008900030123     D*-------------------
009000030123     D* DS REPERIMENTO DATI UTENTE
009100030123     D*-------------------
009200030123     D TIBS34DS      E DS                                                       *Profili utente
009300030123     D DDATIUTE      E DS                                                       *Dati utente
009400030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009500030124     D*------------------
009600030124     D* DS CARICAMENTO P.O. GESTITI
009700030124     D*-------------------
009800030124     D TRUL31DS      e ds                  INZ
009900021219     D*------------------
010000021219     D* PASSAGGIO PARAMETRI
010100021219     D*------------------
010200030123     D PARAM           DS                  INZ
010300170502     D  PARDAL                        8  0
010400170502     D  PARAL                         8  0
010500170502     D  PARCAU                        4
010600170502     D  PARCOD                       15
010700170510     D  PARSOG                        8
010800030618     D*
010900030618     D PARAM2          DS                  INZ
011000030618     D  P2OP1                  1      3
011100030618     D  P2CAU                  4      7
011200030618     D  P2DES                  8     42
011300030618     D  P2MSG                 43    122
011400030123     D*------------------
011500030123     D* VARIABILI DI WRK
011600030123     D*------------------
011700170510     D Chk_SOG                        8A
011800030123     D Chk_COD                       15A
011900030123     D Chk_DES                       30A
012000030123     D SAVKPJBU                            LIKE(KPJBU)
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400970526     C*
012500970526     C* NON CI SONO ERRORI IN ENTRATA
012600021211     C     ERRL00        IFEQ      '0'
012700970526     C*
012800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012900021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013200970526     C*
013300970526     C* EMETTE FORMATO VIDEO
013400970526     C                   EXSR      BUTFMR
013500970526     C*
013600970526     C* GESTIONE TASTI FUNZIONALI
013700970526     C                   EXSR      FUNCON
013800021211     C                   ENDDO
013900970526     C*
014000021211     C                   ENDIF
014100021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
014200021211     C                   MOVEL     PARAM         KPJBU
014300970526     C* FINE PGM
014400970526     C                   SETON                                        LR
014500970526     C*------------------------------------------------------------------------*
014600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014700970526     C*------------------------------------------------------------------------*
014800970526     C     BUTFMR        BEGSR
014900970526     C*
015000970526     C* EMETTE LA TESTATA
015100030123     C                   WRITE     SIG9D01
015200970526     C*
015300970526     C* SPEDISCE EVENTUALE MESSAGGIO
015400970526     C                   EXSR      SNDMSG
015500970526     C*
015600970526     C* EMETTE LE FUNZIONI
015700030123     C                   WRITE     SIG9D02
015800970526     C*
015900970526     C* EMETTE IL VIDEO
016000030123     C                   EXFMT     SIG9D03
016100970526     C*
016200970526     C* REIMPOSTA FLAG DI ERRORE
016300970526     C                   MOVEL     '0'           ERRL00
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     SNDMSG        BEGSR
017000970526     C*
017100970526     C* PULISCE IL SFILE MESSAGGI
017200970526     C                   MOVEL     '1'           *IN40
017300030123     C                   WRITE     SIG9DMC
017400970526     C                   MOVEL     '0'           *IN40
017500970526     C*
017600970526     C* VISUALIZZA IL MESSAGGIO
017700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017800970526     C                   CALL      'XMSG'        XMSGPA
017900021211     C     DSMSER        IFEQ      *ZEROS
018000970526     C                   MOVEL     DSMSKE        MSGKEY
018100021211     C                   ENDIF
018200970526     C                   Z-ADD     1             MSGULT
018300030123     C                   WRITE     SIG9DMS
018400030123     C                   WRITE     SIG9DMC
018500970526     C*
018600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018700970526     C                   MOVEL     *BLANKS       DSMSMS
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100021219     C* ENTGES - GESTIONE ENTER -> INVIO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     ENTGES        BEGSR
019400970526     C*
019500970526     C* CONTROLLA I CAMPI IMMESSI
019600970526     C                   EXSR      DECODI
019700970526     C*
019800970526     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000970526     C* F03GES - GESTIONE F03 -> FINE
020100970526     C*------------------------------------------------------------------------*
020200970526     C     F03GES        BEGSR
020300970526     C*
020400970526     C* IMPOSTA "PREMUTO F03"
020500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900030123     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F06GES        BEGSR
021200030123     C*
021300030123     C                   EXSR      DECODI
021400030123     C*
021500030123     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
021600030123     C     ERRL00        IFEQ      '0'
021700030123     C*
021800030123     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
021900170502     C******>>>>>>>      MOVEL     'SIGS'        KCOAZ
022000170502     C*
022100170502     C                   eval      savKPJBU = KPJBU
022200030123     C                   EXSR      VIDPAR
022300170502     C                   eval        KPJBU = PARAM
022400170502     C******             CALL      'BCH10'
022500170503     C                   CALL      'TISIG92C'
022600030123     C                   PARM                    KPJBA
022700170502     C                   eval        KPJBU  = savKPJBU
022800030123     C*
022900030123     C* IMPOSTO FLAG F03 DI LIVELLO PGM
023000030123     C                   MOVEL     '1'           F03L00
023100030123     C*
023200030123     C                   ENDIF
023300021211     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* F12GES - GESTIONE F12 -> RITORNO
023700970526     C*------------------------------------------------------------------------*
023800970526     C     F12GES        BEGSR
023900970526     C*
024000970526     C* IMPOSTA "PREMUTO F12"
024100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024200970526     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024600970526     C*------------------------------------------------------------------------*
024700970526     C     F24GES        BEGSR
024800970526     C*
024900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025400970526     C*------------------------------------------------------------------------*
025500970526     C     DECODI        BEGSR
025600030305     C*
025700000407     C                   MOVEL     '0'           ERRL00
025800000407     C*
025900000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
026000170503     C                   SETOFF                                       82  84
026100170503     C                   SETOFF                                       86  83
026200030305     C*
026300170502     C*--------------------
026400170502     C* CONTROLLO SE DATA DAL > 0 E DATA DETTAGLIO AL = 0 =>
026500170502     C* DATA DETTAGLIO AL = DATA DETTAGLIO DAL
026600170502     C*--------------------
026700170502     C                   IF        VIDDAL <> *zeros AND
026800170502     C                             VIDAL   = *zeros
026900170502     C                   EVAL      VIDAL = VIDDAL
027000170502     C                   ENDIF
027100170502     C*--------------------
027200170502     C* PERIODO DETTAGLIO MOVIMENTI
027300170502     C*--------------------
027400170502     C                   IF        VIDDAL = *zeros OR
027500170502     C                             VIDAL  = *zeros
027600170502     C                   MOVEL     '1'           ERRL00
027700170502     C                   SETON                                        8384
027800170502     C                   MOVEL     MSG(6)        DSMSMS
027900170502     C                   leavesr
028000170502     C                   ENDIF
028100170502     C*--------------------
028200170502     C* CONTROLLO DATA DETTAGLIO DAL
028300170502     C*--------------------
028400170502     C                   IF        VIDDAL <> *zeros
028500170502     C     *LIKE         DEFINE    VIDDAL        DALVID
028600170502     C                   Z-ADD     VIDDAL        WDA8
028700170502     C                   Z-ADD     *ZEROS        DA8W
028800170502     C                   EXSR      CHKDA8
028900170502     C                   Z-ADD     WDA8          VIDDAL
029000170502     C                   Z-ADD     DA8W          DALVID
029100170502     C     ERRL00        IFEQ      '1'
029200170502     C                   SETON                                        83
029300170502     C                   MOVEL     MSG(3)        DSMSMS
029400170502     C                   leavesr
029500170502     C                   ENDIF
029600170502     C                   ENDIF
029700170502     C*--------------------
029800170502     C* CONTROLLO DATA DETTAGLIO AL
029900170502     C*--------------------
030000170502     C                   IF        VIDAL  <> *zeros
030100170502     C     *LIKE         DEFINE    VIDAL         AALVID
030200170502     C                   Z-ADD     VIDAL         WDA8
030300170502     C                   Z-ADD     *ZEROS        DA8W
030400170502     C                   EXSR      CHKDA8
030500170502     C                   Z-ADD     WDA8          VIDAL
030600170502     C                   Z-ADD     DA8W          AALVID
030700170502     C     ERRL00        IFEQ      '1'
030800170502     C                   SETON                                        84
030900170502     C                   MOVEL     MSG(3)        DSMSMS
031000170502     C                   leavesr
031100170502     C                   ENDIF
031200170502     C                   ENDIF
031300170502     C*--------------------
031400170502     C* CONTROLLO DATA DETTAGLIO DAL <= DATA DETTAGLIO AL
031500170502     C*--------------------
031600170502     C                   IF        dalvid <= aalvid
031700170502     C                   ELSE
031800170503     C                   MOVEL     '1'           ERRL00
031900170502     C                   SETON                                        8384
032000170502     C                   MOVEL     MSG(2)        DSMSMS
032100170502     C                   leavesr
032200170502     C                   ENDIF
032300170502     C*--------------------
032400170502     C* CONTROLLO SE RICHIESTA CAUSALE
032500170502     C*--------------------
032600170502     C                   IF        VIDCAU <> *BLANKS
032700170502     C                   EVAL      chkCAU = VIDCAU
032800170502     C                   EXSR      REPCAU
032900170502     C                   EVAL      VIDCAU = chkCAU
033000170502     C                   IF        ERRL00 = '1'
033100170502     C                   SETON                                        86
033200170502     C                   MOVEL     MSG(11)       DSMSMS
033300170502     C                   leavesr
033400170502     C                   ENDIF
033500170502     C                   ELSE
033600170503     C                   MOVEL     '1'           ERRL00
033700170502     C                   SETON                                        86
033800170502     C                   MOVEL     MSG(11)       DSMSMS
033900170502     C                   leavesr
034000170502     C                   ENDIF
034100170510      *----------------------
034200170510      *   SOGGETTO x CLIENTE
034300170510      *----------------------
034400170510     c                   clear                   vidDSg
034500170510     C                   IF        VIDsogK <> *blanks AND
034600170510     C                             VIDsogK <> *zeros
034700170510     C                   EVAL      Chk_DES =  *blanks
034800170510     C                   MOVEL     *zeros        Chk_SOG
034900170510     C                   MOVE      VIDsogK       Chk_SOG
035000170510     C                   EXSR      REPKSC
035100170510     C                   IF        ERRL00  = '1'
035200170510     C                   SETON                                        81
035300170510     C                   MOVEL     MSG(4)        DSMSMS
035400170510     C                   ENDIF
035500170510     C                   MOVE(P)   Chk_SOG       VIDsogK
035600170510     C                   EVAL      VIDDSg  = Chk_DES
035700170510     C                   ElsE
035800170510     c                   clear                   vidSOGk
035900170510     C                   ENDIF
036000021219     C*--------------------
036100021219     C* SCELTA/CONTROLLO ARTICOLO
036200021219     C*--------------------
036300170502     C                   IF        VIDCOD <> *blanks
036400021223     C                   EVAL      Chk_DES = *blanks
036500170502     C                   EVAL      Chk_COD = VIDCOD
036600021220     C                   EXSR      REPCOD
036700170502     C                   EVAL      VIDCOD = Chk_COD
036800170502     C                   EVAL      VIDDES = Chk_DES
036900021220     C                   IF        ERRL00 = '1'
037000021220     C                   SETON                                        82
037100021220     C                   MOVEL     MSG(5)        DSMSMS
037200170502     C                   leavesr
037300021220     C                   ENDIF
037400021219     C                   ENDIF
037500030109     C*
037600970528     C                   ENDSR
037700170510     C*------------------------------------------------------------------------*
037800170510     C* REPKSC - CONTROLLA CODICE CLIENTE
037900170510     C*------------------------------------------------------------------------*
038000170510     C     REPKSC        BEGSR
038100170510     C*
038200170510     C* RICHIESTO ELENCO
038300170510     C                   SETOFF                                           28
038400170510     C     '?'           SCAN      Chk_SOG                                28
038500170510     C     *IN28         IFEQ      *ON
038600170510     C*
038700170510     C* RICHIESTO ELENCO CLIENTI
038800170510     C                   CLEAR                   DSXC
038900170510     C                   Z-ADD     DUTKCI        DXCCAP
039000170510     C                   CALL      'XCLIR'
039100170510     C                   PARM                    DSXC
039200170510     C* SCOMPATTA
039300170510     C                   MOVEA     DXCKSC        KSA
039400170510     C     1             DO        30            J                 3 0
039500170510     C                   MOVE      KSA(J)        DSKSA
039600170510     C                   Z-ADD     DSKSN         KSC(J)
039700170510     C                   ENDDO
039800170510     C                   MOVEL     *zeros        Chk_SOG
039900170510     C                   MOVE      KSC(1)        Chk_SOG
040000170510     C                   MOVEL     '1'           $TESVD            1
040100170510     C*
040200170510     C* CONTROLLA ESISTENZA CLIENTE
040300170510     C                   EXSR      DECTIB
040400170510     C*
040500170510     C                   ELSE
040600170510     C*
040700170510     C* SOLO CARATTERI NUMERICI
040800170510     C                   SETOFF                                       28
040900170510     C                   TESTN                   Chk_SOG              28
041000170510     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
041100170510     C                   MOVEL     '1'           ERRL00
041200170510     C                   ELSE                                                   *E'NUMERO DA TESTN
041300170510     C                   MOVE      Chk_SOG       W001              1
041400170510     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
041500170510     C                   MOVEL     '1'           ERRL00
041600170510     C                   ELSE                                                   *NUMERO
041700170510     C*
041800170510     C* CONTROLLA ESISTENZA CLIENTE
041900170510     C                   EXSR      DECTIB
042000170510     C                   ENDIF
042100170510     C                   ENDIF
042200170510     C                   ENDIF
042300170510     C*
042400170510     C                   ENDSR
042500170510     C*------------------------------------------------------------------------*
042600170510     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
042700170510     C*------------------------------------------------------------------------*
042800170510     C     DECTIB        BEGSR
042900170510     C*
043000170510     C                   CLEAR                   BS69DS
043100170510     C                   CLEAR                   ACODS
043200170510     C                   MOVEL     KNSIF         I69SIF
043300170510     C                   MOVE(P)   Chk_SOG       I69KAC
043400170510     C                   CALL      'TIBS69R'
043500170510     C                   PARM                    BS69DS
043600170510     C                   PARM                    ACODS
043700170510     C                   PARM                    INDDS
043800170510     C                   PARM                    CLPDS
043900170510     C                   PARM                    CLSDS
044000170510     C     O69ERR        IFNE      '1'
044100170510     C                   EVAL      Chk_DES = acoRAG
044200170510     C                   ELSE
044300170510     C                   MOVEL     '1'           ERRL00
044400170510     C                   ENDIF
044500170510     C*
044600170510     C                   ENDSR
044700021220     C*------------------------------------------------------------------------*
044800021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
044900021220     C*------------------------------------------------------------------------*
045000021220     C     REPCOD        BEGSR
045100021220     C*
045200021220     C* RICHIESTO ELENCO
045300021220     C                   SETOFF                                           28
045400021220     C     '?'           SCAN      Chk_COD                                28
045500021220     C     *IN28         IFEQ      *ON
045600021220     C*
045700021220     C* RICHIESTO ELENCO ARTICOLI
045800021220     C                   MOVEL     KPJBU         SAVKPJBU
045900021220     C                   MOVEL(P)  'P01'         KPJBU
046000021220     C                   CALL      'TISIG1R'
046100021220     C                   PARM                    KPJBA
046200021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
046300021220     C                   MOVEL     SAVKPJBU      KPJBU
046400021220     C                   ELSE
046500021220     C     Chk_COD       CHAIN     azagb01l
046600021220     C                   IF        %found(azagb01l)
046700021220     C                   EVAL      Chk_DES = agbDES
046800021220     C                   ELSE
046900021220     C                   EVAL      ERRL00 = '1'
047000021220     C                   EVAL      Chk_DES = *all'*'
047100021220     C                   ENDIF
047200021220     C                   ENDIF
047300021220     C*
047400021220     C                   ENDSR
047500030618     C*------------------------------------------------------------------------*
047600030618     C* REPCAU - CONTROLLA CAUSALE MOVIMENTO
047700030618     C*------------------------------------------------------------------------*
047800030618     C     REPCAU        BEGSR
047900030618     C*
048000170502     C     *LIKE         DEFINE    VIDCAU        chkCAU
048100030618     C*
048200030618     C* RICHIESTO ELENCO
048300030618     C                   SETOFF                                           28
048400030618     C     '?'           SCAN      chkCAU                                 28
048500030618     C     *IN28         IFEQ      *ON
048600030618     C*
048700030618     C* VALORIZZO I PARAMETRI DI PASSAGGIO
048800030618     C                   CLEAR                   PARAM2
048900030618     C                   EVAL      P2OP1 = 'P01'
049000030618     C                   MOVEL     PARAM2        KPJBU
049100030618     C                   CALL      'TNTB31R'
049200030618     C                   PARM                    KPJBA
049300030618     C*
049400030618     C                   MOVEL     KPJBU         PARAM2
049500030618     C                   MOVEL     P2CAU         chkCAU
049600030618     C*
049700030618     C* ALTRIMENTI CONTROLLO CODICE IMMESSO
049800030618     C                   ELSE
049900030618     C                   EVAL      tbeCOD = 'GGB'
050000030618     C                   EVAL      tbeKE1 = chkCAU
050100030618     C     KEYTBEP       CHAIN     tntbe01l
050200030618     C                   IF        %found(tntbe01l)
050300030618     C                   ELSE
050400030618     C                   EVAL      ERRL00 = '1'
050500030618     C                   ENDIF
050600030618     C                   ENDIF
050700030618     C*
050800030618     C                   ENDSR
050900021220     C*------------------------------------------------------------------------*
051000021220     C* CHKDA8 - CONTROLLA LA DATA (8)
051100021220     C*------------------------------------------------------------------------*
051200021220     C     CHKDA8        BEGSR
051300021220     C*
051400170502     C     *LIKE         DEFINE    VIDDAL        WDA8
051500170502     C     *LIKE         DEFINE    VIDDAL        DA8W
051600021220     C*
051700021220     C                   Z-ADD     WDA8          G08DAT
051800021220     C                   Z-ADD     *ZEROS        G08INV
051900021220     C                   MOVEL     '0'           G08ERR
052000021220     C                   CALL      'XSRDA8'
052100021220     C                   PARM                    WLBDA8
052200021220     C                   Z-ADD     G08DAT        WDA8
052300021220     C                   Z-ADD     G08INV        DA8W
052400021220     C     G08ERR        IFEQ      '1'
052500021220     C                   MOVEL     '1'           ERRL00
052600021220     C                   ENDIF
052700021220     C*
052800021220     C                   ENDSR
052900030124     C*------------------------------------------------------------------------*
053000030124     C* CARTAB - CARICA LE TABELLE
053100030124     C*------------------------------------------------------------------------*
053200030124     C     CARTAB        BEGSR
053300030124     C*
053400030124     C* REPERISCE I DATI UTENTE
053500030124     C                   EXSR      REPDATIUTE
053600030124     C*
053700030124     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
053800030124     C                   CLEAR                   L1
053900030124     C                   CLEAR                   WAbi              2
054000030124     C*
054100030124     C                   IF        UTEAUT > *blanks
054200030124     C                   EVAL      WAbi = UTEAUT
054300030124     C                   ELSE
054400030124     C*
054500030124     C                   IF        DUTLPO = 'S'                                 *SEDE
054600030124     C                   EVAL      WAbi = 'AZ'
054700030124     C                   ENDIF
054800030124     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
054900030124     C                   EVAL      WAbi = 'TP'
055000030124     C                   ENDIF
055100030124     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
055200030124     C                   EVAL      WAbi = 'PO'
055300030124     C                   ENDIF
055400030124     C                   ENDIF
055500030124     C*
055600030124     C* Reperisce i P.O. gestibili dall'utente
055700030124     C                   CLEAR                   TRUL31DS
055800030124     C                   EVAL      I31abi = WAbi
055900030124     C                   EVAL      I31cpo = DUTPOU
056000030124     C                   CALL      'TRUL31R'
056100030124     C                   PARM                    KPJBA
056200030124     C                   PARM                    TRUL31DS
056300030124     C                   IF        O31pog > *zeros
056400030124     C                   MOVEA     O31pog        L1
056500030124     C                   ELSE
056600030124     C                   EVAL      FINL00 = '1'
056700030124     C                   ENDIF
056800030124     C*
056900030124     C                   ENDSR
057000021219     C*--------------------------------------------------------------------------------------------*
057100021219     C* REPERISCE I DATI UTENTE
057200021219     C*--------------------------------------------------------------------------------------------*
057300021219     C     REPDATIUTE    BEGSR
057400021219     C*
057500021219     C* INIZIALIZZA VARIABILI DI WRK
057600021219     C                   CLEAR                   TIBS34DS
057700021219     C                   CLEAR                   AZUTEDS
057800021219     C                   CLEAR                   DDATIUTE
057900021219     C*
058000021219     C     *DTAARA       DEFINE    §azute        azuteds
058100021219     C     *DTAARA       DEFINE    §datiute      ddatiute
058200021219     C                   IN(E)     *DTAARA
058300021219     C                   IF        %Error
058400021220     C                   EVAL      I34Tla = 'L'
058500021219     C                   CALL      'TIBS34R'
058600021219     C                   PARM                    Tibs34Ds
058700021219     C                   IN        *DTAARA
058800021219     C                   ENDIF
058900021219     C*
059000021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059100021219     C                   MOVEL(P)  RSUT          DSFIRS           20
059200021219     C*
059300021219     C                   ENDSR
059400030123     C*------------------------------------------------------------------------*
059500030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
059600030123     C*------------------------------------------------------------------------*
059700030123     C     VIDPAR        BEGSR
059800030123     C*
059900170502     C                   MOVEL     VIDCOD        PARCOD
060000170510     C                   MOVEL     VIDsogK       PARSOG
060100170502     C                   MOVEL     VIDCAU        PARCAU
060200170502     C                   Z-ADD     DALVID        PARDAL
060300170502     C                   Z-ADD     AALVID        PARAL
060400030123     C*
060500030123     C                   ENDSR
060600030123     C*------------------------------------------------------------------------*
060700030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
060800030123     C*------------------------------------------------------------------------*
060900030123     C     AZZVID        BEGSR
061000030123     C*
061100170502     C                   MOVEL     *blanks       VIDCOD
061200170502     C                   MOVEL     *blanks       VIDDES
061300170502     C                   MOVEL     *blanks       VIDCAU
061400170502     C                   Z-ADD     WN8           VIDDAL
061500170502     C                   Z-ADD     WN8           VIDAL
061600030123     C                   SETOFF                                       55
061700030305     C                   SETOFF                                       56
061800030124     C                   SETON                                        92
061900030123     C*
062000030123     C                   ENDSR
062100030123     C*------------------------------------------------------------------------*
062200030123     C* *INZSR - OPERAZIONI INIZIALI
062300030123     C*------------------------------------------------------------------------*
062400030123     C     *INZSR        BEGSR
062500030123     C*
062600030123     C* RICEVIMENTO PARAMETRI
062700030123     C     *ENTRY        PLIST
062800030123     C                   PARM                    KPJBA
062900030618     C*---
063000030618     C* CHIAVI DI LETTURA
063100030618     C*---
063200030618     C* LETTURA tntbe01l - PARZIALE
063300030618     C     KEYTBEP       KLIST
063400030618     C                   KFLD                    tbeCOD
063500030618     C                   KFLD                    tbeKE1
063600030123     C*
063700030124     C* CARICAMENTO TABELLE
063800030124     C                   EXSR      CARTAB
063900030123     C*
064000030123     C* REPERISCO DATA E ORA CORRENTI
064100030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
064200030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
064300030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
064400030123     C                   Z-ADD     WN8           G08DAT
064500030123     C                   Z-ADD     *ZEROS        G08INV
064600030123     C                   MOVEL     '0'           G08ERR
064700030123     C                   CALL      'XSRDA8'
064800030123     C                   PARM                    WLBDA8
064900030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
065000030123     C*
065100030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
065200030123     C     XMSGPA        PLIST
065300030123     C                   PARM                    DSMSMS
065400030123     C                   PARM                    DSMSPG
065500030123     C                   PARM                    DSMSKE
065600030123     C                   PARM                    DSMSER
065700030123     C*
065800030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
065900030123     C                   CALL      'XKEY'
066000030123     C                   PARM                    DSKEY
066100030123     C*
066200030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
066300030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
066400030123     C*
066500030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
066600030123     C*   -NOME PGM CHIAMANTE
066700030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
066800170502     C                   MOVEL     'TISIG91R'    DSMSPG
066900170502     C                   MOVEL     'TISIG91R'    MSGQUE
067000030123     C                   MOVEL     *BLANKS       MSGKEY
067100030123     C                   MOVEL     *BLANKS       DSMSMS
067200030123     C*
067300030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
067400030123     C                   Z-ADD     0             FUNGRU            5 0
067500030123     C*
067600030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
067700030123     C                   EXSR      FUNGES
067800030123     C*
067900030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
068000030123     C                   MOVEL     '0'           F12L00            1
068100030123     C                   MOVEL     '0'           F03L00            1
068200030123     C                   MOVEL     '0'           FINL00            1
068300030123     C                   MOVEL     '0'           ERRL00            1
068400030123     C                   MOVEL     '0'           FLFINE            1
068500030123     C*
068600030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
068700030123     C                   EXSR      AZZVID
068800030123     C*
068900030123     C                   ENDSR
069000030123     C*------------------------------------------------------------------------*
069100030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
069200030123     C*------------------------------------------------------------------------*
069300030123     C     FUNGES        BEGSR
069400030123     C*
069500030123     C* PULISCO CAMPI DI LAVORO
069600030123     C                   Z-ADD     0             W                 5 0
069700030123     C*
069800030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
069900030123     C* E CARICO TABELLA FUNZIONI ATTIVE
070000030123     C     1             DO        24            I
070100030123     C                   MOVEL     FUD(I)        FUDRIG
070200030123     C*
070300030123     C* PULIZIA FUNZIONI PRECEDENTI
070400030123     C                   MOVEL     *BLANKS       FUV(I)
070500030123     C*
070600030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
070700030123     C* (SALTA F24)
070800030123     C     FUDATT        IFEQ      'S'
070900030123     C     FUDFUN        ANDNE     '24'
071000030123     C                   ADD       1             W
071100030123     C                   MOVEL     FUDDES        FUV(W)
071200030123     C                   END
071300030123     C                   END                                                    DO WHILE
071400030123     C*
071500030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
071600030123     C* ALTRIMENTI DISATTIVO F24
071700030123     C     1             DO        24            I
071800030123     C                   MOVEL     FUD(I)        FUDRIG
071900030123     C*
072000030123     C     FUDFUN        IFEQ      '24'
072100030123     C     W             IFGT      6
072200030123     C                   MOVEL     'S'           FUDATT
072300030123     C                   ELSE
072400030123     C                   MOVEL     *BLANKS       FUDATT
072500030123     C                   END                                                    W GT 8
072600030123     C                   MOVEL     FUDRIG        FUD(I)
072700030123     C                   END
072800030123     C                   END                                                    DO WHILE
072900030123     C*
073000030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
073100030123     C     W             DIV       6             I
073200030123     C                   MVR                     Z
073300030123     C*
073400030123     C     Z             IFNE      0
073500030123     C     I             ADD       1             Z
073600030123     C                   ELSE
073700030123     C                   Z-ADD     I             Z                 5 0
073800030123     C                   END
073900030123     C*
074000030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
074100030123     C                   ADD       1             FUNGRU
074200030123     C     FUNGRU        IFGT      Z
074300030123     C     FUNGRU        ORLT      1
074400030123     C                   Z-ADD     1             FUNGRU
074500030123     C                   END
074600030123     C*
074700030123     C* PRIMO GRUPPO DI 6 FUNZIONI
074800030123     C     FUNGRU        IFEQ      1
074900030123     C                   MOVEL     FUV(1)        FUN001
075000030123     C                   MOVEL     FUV(2)        FUN002
075100030123     C                   MOVEL     FUV(3)        FUN003
075200030123     C                   MOVEL     FUV(4)        FUN004
075300030123     C                   MOVEL     FUV(5)        FUN005
075400030123     C                   MOVEL     FUV(6)        FUN006
075500030123     C                   END
075600030123     C*
075700030123     C* SECONDO GRUPPO DI 6 FUNZIONI
075800030123     C     FUNGRU        IFEQ      2
075900030123     C                   MOVEL     FUV(7)        FUN001
076000030123     C                   MOVEL     FUV(8)        FUN002
076100030123     C                   MOVEL     FUV(9)        FUN003
076200030123     C                   MOVEL     FUV(10)       FUN004
076300030123     C                   MOVEL     FUV(11)       FUN005
076400030123     C                   MOVEL     FUV(12)       FUN006
076500030123     C                   END
076600030123     C*
076700030123     C* TERZO GRUPPO DI 6 FUNZIONI
076800030123     C     FUNGRU        IFEQ      3
076900030123     C                   MOVEL     FUV(13)       FUN001
077000030123     C                   MOVEL     FUV(14)       FUN002
077100030123     C                   MOVEL     FUV(15)       FUN003
077200030123     C                   MOVEL     FUV(16)       FUN004
077300030123     C                   MOVEL     FUV(17)       FUN005
077400030123     C                   MOVEL     FUV(18)       FUN006
077500030123     C                   END
077600030123     C*
077700030123     C* QUARTO GRUPPO DI 6 FUNZIONI
077800030123     C     FUNGRU        IFEQ      4
077900030123     C                   MOVEL     FUV(19)       FUN001
078000030123     C                   MOVEL     FUV(20)       FUN002
078100030123     C                   MOVEL     FUV(21)       FUN003
078200030123     C                   MOVEL     FUV(22)       FUN004
078300030123     C                   MOVEL     *BLANKS       FUN005
078400030123     C                   MOVEL     *BLANKS       FUN006
078500030123     C                   END
078600030123     C*
078700030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
078800030123     C                   MOVEL     *BLANKS       FUN007
078900030123     C                   MOVEL     *BLANKS       FUN008
079000030123     C*
079100030123     C     1             DO        24            I
079200030123     C                   MOVEL     FUD(I)        FUDRIG
079300030123     C     FUDFUN        IFEQ      '24'
079400030123     C     FUDATT        ANDEQ     'S'
079500030123     C                   MOVEL     FUDDES        FUN008
079600030123     C                   END
079700030123     C                   END                                                    DO
079800030123     C                   ENDSR
079900030123     C*------------------------------------------------------------------------*
080000030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
080100030123     C*------------------------------------------------------------------------*
080200030123     C     FUNCON        BEGSR
080300030123     C*
080400030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080500030123     C                   EXSR      FUNVAR
080600030123     C*
080700030123     C* PULISCO CAMPI DI LAVORO
080800030123     C                   Z-ADD     0             I                 5 0
080900030123     C                   Z-ADD     0             W                 5 0
081000030123     C*
081100030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
081200030123     C     FUNKEY        IFNE      *BLANKS
081300030123     C     1             DO        24            I
081400030123     C                   MOVEL     FUD(I)        FUDRIG
081500030123     C*
081600030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081700030123     C     FUNKEY        IFEQ      FUDFUN
081800030123     C     FUDATT        ANDEQ     'S'
081900030123     C                   Z-ADD     I             W
082000030123     C                   END
082100030123     C                   END                                                    DO WHILE
082200030123     C*
082300030123     C* FUNZIONE NON ATTIVA (ERRORE)
082400030123     C     W             IFEQ      0
082500030123     C*
082600030123     C* EMISSIONE MESSAGGIO DI ERRORE
082700030123     C                   MOVEL     MSG(1)        DSMSMS
082800030123     C*
082900030123     C                   END                                                    W=0
083000030123     C                   END                                                    FUNKEY NE *BLANKS
083100030123     C*
083200030123     C* FUNZIONE ATTIVA (PROCEDI ...)
083300030123     C     W             IFNE      0
083400030123     C     FUNKEY        OREQ      *BLANKS
083500030123     C*
083600030123     C     FUNKEY        IFEQ      *BLANKS
083700030123     C                   EXSR      ENTGES
083800030123     C                   END
083900030123     C     FUNKEY        IFEQ      '03'
084000030123     C                   EXSR      F03GES
084100030123     C                   END
084200030123     C     FUNKEY        IFEQ      '06'
084300030123     C                   EXSR      F06GES
084400030123     C                   END
084500030123     C     FUNKEY        IFEQ      '12'
084600030123     C                   EXSR      F12GES
084700030123     C                   END
084800030123     C     FUNKEY        IFEQ      '24'
084900030123     C                   EXSR      F24GES
085000030123     C                   END
085100030123     C*
085200030123     C                   END
085300030123     C*
085400030123     C                   ENDSR
085500030123     C*------------------------------------------------------------------------*
085600030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
085700030123     C*------------------------------------------------------------------------*
085800030123     C     FUNVAR        BEGSR
085900030123     C*
086000030123     C* INIZIALIZZA VARIABILE -FUNKEY-
086100030123     C*
086200030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
086300030123     C     KEY           IFEQ      ENTER
086400030123     C                   MOVEL     *BLANKS       FUNKEY
086500030123     C                   END
086600030123     C     KEY           IFEQ      F1
086700030123     C                   MOVEL     '01'          FUNKEY
086800030123     C                   END
086900030123     C     KEY           IFEQ      F2
087000030123     C                   MOVEL     '02'          FUNKEY
087100030123     C                   END
087200030123     C     KEY           IFEQ      F3
087300030123     C                   MOVEL     '03'          FUNKEY
087400030123     C                   END
087500030123     C     KEY           IFEQ      F4
087600030123     C                   MOVEL     '04'          FUNKEY
087700030123     C                   END
087800030123     C     KEY           IFEQ      F5
087900030123     C                   MOVEL     '05'          FUNKEY
088000030123     C                   END
088100030123     C     KEY           IFEQ      F6
088200030123     C                   MOVEL     '06'          FUNKEY
088300030123     C                   END
088400030123     C     KEY           IFEQ      F7
088500030123     C                   MOVEL     '07'          FUNKEY
088600030123     C                   END
088700030123     C     KEY           IFEQ      F8
088800030123     C                   MOVEL     '08'          FUNKEY
088900030123     C                   END
089000030123     C     KEY           IFEQ      F9
089100030123     C                   MOVEL     '09'          FUNKEY
089200030123     C                   END
089300030123     C     KEY           IFEQ      F10
089400030123     C                   MOVEL     '10'          FUNKEY
089500030123     C                   END
089600030123     C     KEY           IFEQ      F11
089700030123     C                   MOVEL     '11'          FUNKEY
089800030123     C                   END
089900030123     C     KEY           IFEQ      F12
090000030123     C                   MOVEL     '12'          FUNKEY
090100030123     C                   END
090200030123     C     KEY           IFEQ      F13
090300030123     C                   MOVEL     '13'          FUNKEY
090400030123     C                   END
090500030123     C     KEY           IFEQ      F14
090600030123     C                   MOVEL     '14'          FUNKEY
090700030123     C                   END
090800030123     C     KEY           IFEQ      F15
090900030123     C                   MOVEL     '15'          FUNKEY
091000030123     C                   END
091100030123     C     KEY           IFEQ      F16
091200030123     C                   MOVEL     '16'          FUNKEY
091300030123     C                   END
091400030123     C     KEY           IFEQ      F17
091500030123     C                   MOVEL     '17'          FUNKEY
091600030123     C                   END
091700030123     C     KEY           IFEQ      F18
091800030123     C                   MOVEL     '18'          FUNKEY
091900030123     C                   END
092000030123     C     KEY           IFEQ      F19
092100030123     C                   MOVEL     '19'          FUNKEY
092200030123     C                   END
092300030123     C     KEY           IFEQ      F20
092400030123     C                   MOVEL     '20'          FUNKEY
092500030123     C                   END
092600030123     C     KEY           IFEQ      F21
092700030123     C                   MOVEL     '21'          FUNKEY
092800030123     C                   END
092900030123     C     KEY           IFEQ      F22
093000030123     C                   MOVEL     '22'          FUNKEY
093100030123     C                   END
093200030123     C     KEY           IFEQ      F23
093300030123     C                   MOVEL     '23'          FUNKEY
093400030123     C                   END
093500030123     C     KEY           IFEQ      F24
093600030123     C                   MOVEL     '24'          FUNKEY
093700030123     C                   END
093800030123     C     KEY           IFEQ      ROLLUP
093900030123     C                   MOVEL     'RU'          FUNKEY
094000030123     C                   END
094100030123     C     KEY           IFEQ      HELP
094200030123     C                   MOVEL     'HP'          FUNKEY
094300030123     C                   END
094400030123     C                   ENDSR
094500030123     C*------------------------------------------------------------------------*
094600030123     C* NOTE PER COMPILAZIONE SCHIERE
094700030123     C*------------------------------------------------------------------------*
094800030123     C* -MSG-
094900030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
095000030123     C* (NON TOCCARE MSG,1)
095100030123     C*
095200030123     C* -CMD-
095300030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
095400030123     C*
095500030123     C* -FUD-
095600030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
095700030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
095800030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
095900030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
096000030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
096100030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
096200030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
096300030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
096400030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
096500030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
096600030123     C*
096700030123     C*------------------------------------------------------------------------*
096800030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
096900170502TISIG91R Tasto funzionale non gestito.                                         1
097000170502TISIG91R Data iniziale maggiore di data finale.                                2
097100170502TISIG91R Data errata o mancante                                                3
097200170510TISIG91R Soggetto movimento errato.                                            4
097300170502TISIG91R Articolo indicato errato.                                             5
097400170502TISIG91R Campo obbligatorio.                                                   6
097500170502TISIG91R                                                                       7
097600170502TISIG91R                                                                       8
097700170502TISIG91R                                                                       9
097800170502TISIG91R                                                                       10
097900170502TISIG91R Causale errata o inesistente, correggere.                             11
098000170502TISIG91R                                                                       12
098100170502TISIG91R
098200170502TISIG91R
098300170502TISIG91R
098400170502TISIG91R
098500170502TISIG91R
098600170502TISIG91R
098700170502TISIG91R
098800170502TISIG91R
098900030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
099000030123                   -        (S/N)=N                        funz.01¬
099100030123                   -        (S/N)=N                        funz.02¬
099200030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
099300030123                   -        (S/N)=N                        funz.04¬
099400030123                   -        (S/N)=N                        funz.05¬
099500030123F06=Lancio         -        (S/N)=S                        funz.06¬
099600030123                   -        (S/N)=N                        funz.07¬
099700030123                   -        (S/N)=N                        funz.08¬
099800030123                   -        (S/N)=N                        funz.09¬
099900030123                   -        (S/N)=N                        funz.10¬
100000030123                   -        (S/N)=N                        funz.11¬
100100170502F12=Ritorno        -        (S/N)=N                        funz.12¬
100200030123                   -        (S/N)=N                        funz.13¬
100300030123                   -        (S/N)=N                        funz.14¬
100400030123                   -        (S/N)=N                        funz.15¬
100500030123                   -        (S/N)=N                        funz.16¬
100600030123                   -        (S/N)=N                        funz.17¬
100700030123                   -        (S/N)=N                        funz.18¬
100800030123                   -        (S/N)=N                        funz.19¬
100900030123                   -        (S/N)=N                        funz.20¬
101000030123                   -        (S/N)=N                        funz.21¬
101100030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
101200030123                   -        (S/N)=N                        funz.23¬
101300030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
