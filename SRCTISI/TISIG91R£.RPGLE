000100970526     H*------------------------------------------------------------------------*
000200030123     H* Stampa movimenti GABBIE e BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030123     FTISIG9D   CF   E             WORKSTN INFDS(DEVDS1)
000900030123     F                                     SFILE(SIG9DMS:MSGULT)
001000021219     Fazorg01l  IF   E           K DISK
001100021219     Fcnaco00f  IF   E           K DISK
001200021219     Fazagb01l  IF   E           K DISK
001300030124     Fazdst04l  IF   E           K DISK
001400030618     Ftntbe01l  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000030124     D*-------------------
003100030124     D* FILIALI DEI CLIENTI DA VISUALIZZARE
003200030124     D*-------------------
003300030124     D L1              S              3    DIM(250) INZ(*zeros)
003400970526     D*------------------------------------------------------------------------*
003500970526     D* INPUT
003600970526     D*------------------------------------------------------------------------*
003700970526     D*------------
003800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003900970526     D*------------
004000970526     D FUDDS           DS
004100970526     D  FUDDE1                 1      1
004200970526     D  FUDFUN                 2      3
004300970526     D  FUDDE2                 4     19
004400970526     D  FUDFI0                20     34
004500970526     D  FUDATT                35     35
004600970526     D  FUDFI1                36     80
004700970526     D  FUDDES                 1     19
004800970526     D  FUDRIG                 1     80
004900970526     D*------------
005000970526     D* DS GESTIONE TASTI FUNZIONALI
005100970526     D*------------
005200970526     D DSKEY         E DS
005300970526     D DEVDS1          DS
005400970526     D  KEY                  369    369
005500970526     D*------------
005600970526     D* DS "XMSG" - GESTIONE MESSAGGI
005700970526     D*------------
005800970526     D DSMSG         E DS
005900021219     D*------------
006000970526     D* ARCHITETTURA
006100970526     D*------------
006200970526     D KPJBA         E DS
006300021220     D*------------
006400021220     D* TABELLE "XCLIR"- RICERCA CLIENTI
006500021220     D*------------
006600021220     D KSC             S              7  0 dim(30)
006700021220     D KSA             S              4    dim(30)
006800021220     D*------------------
006900021220     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
007000021220     D*------------------
007100021220     D DSXC          E DS                  extname(XCLIRDS)
007200021220     D*------------------
007300021220     D                 DS
007400021220     D DSKSN                   1      4p 0
007500021220     D DSKSA                   1      4
007600021220     D*------------------
007700021220     D* DS REPERIMENTO DATI UTENTE
007800021220     D*-------------------
007900021220     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008000021220     D ACODS         E DS                  EXTNAME(CNACO00F)
008100021220     D INDDS         E DS                  EXTNAME(CNIND00F)
008200021220     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008300021220     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008400000407     D*------------------
008500000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
008600000407     D*------------------
008700000407     D WLBDA8          DS                  INZ
008800000407     D  G08DAT                 1      8  0
008900000407     D  G08INV                 9     16  0
009000000407     D  G08ERR                17     17
009100000407     D  G08TGI                18     22  0
009200030123     D*-------------------
009300030123     D* DS REPERIMENTO DATI UTENTE
009400030123     D*-------------------
009500030123     D TIBS34DS      E DS                                                       *Profili utente
009600030123     D DDATIUTE      E DS                                                       *Dati utente
009700030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009800030124     D*------------------
009900030124     D* DS CARICAMENTO P.O. GESTITI
010000030124     D*-------------------
010100030124     D TRUL31DS      e ds                  INZ
010200021219     D*------------------
010300021219     D* PASSAGGIO PARAMETRI
010400021219     D*------------------
010500030123     D PARAM           DS                  INZ
010600030123     D  PARDA1                        8  0
010700030123     D  PARTP1                        1
010800030123     D  PARSO1                        8
010900030123     D  PARCO1                       15
011000030123     D  PARSEL                        1
011100030123     D  PARDET                        1
011200030123     D  PARDDA                        8  0
011300030123     D  PARDAA                        8  0
011400030305     D  PARPO1                        3
011500030618     D  PARSSN                        1
011600030618     D  PARCA1                        4
011700030618     D  PARCA2                        4
011800030618     D*
011900030618     D PARAM2          DS                  INZ
012000030618     D  P2OP1                  1      3
012100030618     D  P2CAU                  4      7
012200030618     D  P2DES                  8     42
012300030618     D  P2MSG                 43    122
012400030123     D*------------------
012500030123     D* VARIABILI DI WRK
012600030123     D*------------------
012700030123     D Chk_SOG                        8A
012800030123     D Chk_COD                       15A
012900030123     D Chk_DES                       30A
013000030123     D SAVKPJBU                            LIKE(KPJBU)
013100070509
013200070509      * flag per forzare messaggio errore sul cliente
013300070509     d wforza          s              1    inz(*off)
013400070510      * campi di sav
013500070510     d savso1f         s                   like(vidso1f) inz
013600070510     d savso1k         s                   like(vidso1k) inz
013700070509
013800970526     C*------------------------------------------------------------------------*
013900970526     C* MAIN LINES
014000970526     C*------------------------------------------------------------------------*
014100970526     C*
014200970526     C* NON CI SONO ERRORI IN ENTRATA
014300021211     C     ERRL00        IFEQ      '0'
014400970526     C*
014500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014600021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014900970526     C*
015000970526     C* EMETTE FORMATO VIDEO
015100970526     C                   EXSR      BUTFMR
015200970526     C*
015300970526     C* GESTIONE TASTI FUNZIONALI
015400970526     C                   EXSR      FUNCON
015500021211     C                   ENDDO
015600970526     C*
015700021211     C                   ENDIF
015800021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
015900021211     C                   MOVEL     PARAM         KPJBU
016000970526     C* FINE PGM
016100970526     C                   SETON                                        LR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     BUTFMR        BEGSR
016600970526     C*
016700970526     C* EMETTE LA TESTATA
016800030123     C                   WRITE     SIG9D01
016900970526     C*
017000970526     C* SPEDISCE EVENTUALE MESSAGGIO
017100970526     C                   EXSR      SNDMSG
017200970526     C*
017300970526     C* EMETTE LE FUNZIONI
017400030123     C                   WRITE     SIG9D02
017500970526     C*
017600970526     C* EMETTE IL VIDEO
017700030123     C                   EXFMT     SIG9D03
017800970526     C*
017900970526     C* REIMPOSTA FLAG DI ERRORE
018000970526     C                   MOVEL     '0'           ERRL00
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
018500970526     C*------------------------------------------------------------------------*
018600970526     C     SNDMSG        BEGSR
018700970526     C*
018800970526     C* PULISCE IL SFILE MESSAGGI
018900970526     C                   MOVEL     '1'           *IN40
019000030123     C                   WRITE     SIG9DMC
019100970526     C                   MOVEL     '0'           *IN40
019200970526     C*
019300970526     C* VISUALIZZA IL MESSAGGIO
019400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
019500970526     C                   CALL      'XMSG'        XMSGPA
019600021211     C     DSMSER        IFEQ      *ZEROS
019700970526     C                   MOVEL     DSMSKE        MSGKEY
019800021211     C                   ENDIF
019900970526     C                   Z-ADD     1             MSGULT
020000030123     C                   WRITE     SIG9DMS
020100030123     C                   WRITE     SIG9DMC
020200970526     C*
020300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
020400970526     C                   MOVEL     *BLANKS       DSMSMS
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800021219     C* ENTGES - GESTIONE ENTER -> INVIO
020900970526     C*------------------------------------------------------------------------*
021000970526     C     ENTGES        BEGSR
021100970526     C*
021200970526     C* CONTROLLA I CAMPI IMMESSI
021300970526     C                   EXSR      DECODI
021400970526     C*
021500970526     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700970526     C* F03GES - GESTIONE F03 -> FINE
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F03GES        BEGSR
022000970526     C*
022100970526     C* IMPOSTA "PREMUTO F03"
022200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600030123     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F06GES        BEGSR
022900030123     C*
023000030123     C                   EXSR      DECODI
023100030123     C*
023200030123     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
023300030123     C     ERRL00        IFEQ      '0'
023400030123     C*
023500030123     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
023600030123     C                   MOVEL     'SIGS'        KCOAZ
023700030123     C                   EXSR      VIDPAR
023800030123     C                   MOVEL     PARAM         KPJBU
023900030618     C                   CALL      'BCH10'
024000030618 MOM C***                CALL      'TISIGSR'
024100030123     C                   PARM                    KPJBA
024200030123     C*
024300030123     C* IMPOSTO FLAG F03 DI LIVELLO PGM
024400030123     C                   MOVEL     '1'           F03L00
024500030123     C*
024600030123     C                   ENDIF
024700021211     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000970526     C* F12GES - GESTIONE F12 -> RITORNO
025100970526     C*------------------------------------------------------------------------*
025200970526     C     F12GES        BEGSR
025300970526     C*
025400970526     C* IMPOSTA "PREMUTO F12"
025500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026000970526     C*------------------------------------------------------------------------*
026100970526     C     F24GES        BEGSR
026200970526     C*
026300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026400970526     C*
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026800970526     C*------------------------------------------------------------------------*
026900970526     C     DECODI        BEGSR
027000030305     C*
027100000407     C                   MOVEL     '0'           ERRL00
027200000407     C*
027300000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
027400030514     C                   SETOFF                                       78
027500030305     C                   SETOFF                                       798182
027600030305     C                   SETOFF                                       838485
027700030618     C                   SETOFF                                       868788
027800030618     C                   SETOFF                                       89
027900030305     C*
028000030305     C* INIZIALIZZO I CAMPI DESCRIZIONE
028100030305     C                   EVAL      VIDDS1 = *blanks
028200030305     C                   EVAL      VIDPO1KD = *blanks
028300030123     C*--------------------
028400030123     C* CONTROLLO DATA SALDO AL
028500030123     C*--------------------
028600030123     C     ERRL00        IFEQ      '0'
028700030123     C                   IF        VIDDA1 <> *zeros
028800030123     C     *LIKE         DEFINE    VIDDA1        DA1VID
028900030123     C                   Z-ADD     VIDDA1        WDA8
029000030123     C                   Z-ADD     *ZEROS        DA8W
029100030123     C                   EXSR      CHKDA8
029200030123     C                   Z-ADD     WDA8          VIDDA1
029300030123     C                   Z-ADD     DA8W          DA1VID
029400030123     C     ERRL00        IFEQ      '1'
029500030123     C                   SETON                                        79
029600030124     C                   MOVEL     MSG(3)        DSMSMS
029700030123     C                   ENDIF
029800030123     C                   ENDIF
029900030123     C                   ENDIF
030000021219     C*--------------------
030100030123     C* SCELTA/CONTROLLO SOGGETTO
030200021219     C*--------------------
030300021219     C     ERRL00        IFEQ      '0'
030400021223     C* Decodifico il tipo di soggetto movimento
030500030123     C                   IF        VIDTP1 = 'C'
030600030305     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
030700030123     C                   SETON                                        55
030800030305     C                   SETON                                        56
030900021223     C                   ENDIF
031000030123     C                   IF        VIDTP1 = 'F'
031100150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
031200030123     C                   SETOFF                                       55
031300030305     C                   SETOFF                                       56
031400021223     C                   ENDIF
031500030124     C                   IF        VIDTP1 = 'D'
031600030305     C                   EVAL      VIDDT1 = ' soggetto destinatario .?:'
031700030124     C                   SETON                                        55
031800030305     C                   SETOFF                                       56
031900030124     C                   ENDIF
032000021223     C* Eseguo i controlli di validità soggetto movimento
032100030107     C                   IF        *IN55   =  *ON
032200021220     C*
032300030305     C                   IF        VIDTP1 = 'C'
032400030305     C                   IF        VIDSO1K <> *blanks AND
032500030305     C                             VIDSO1K <> *zeros
032600030305     C                   EVAL      Chk_DES =  *blanks
032700030305     C                   MOVEL     *zeros        Chk_SOG
032800030305     C                   MOVE      VIDSO1K       Chk_SOG
032900021220     C                   EXSR      REPKSC
033000030305     C                   IF        ERRL00  = '1'
033100030305     C                   SETON                                        81
033200030305     C                   MOVEL     MSG(4)        DSMSMS
033300030305     C                   ENDIF
033400030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
033500030305     C                   EVAL      VIDDS1  = Chk_DES
033600030305     C                   ENDIF
033700030305     C                   IF        VIDPO1K <> *blanks AND
033800030305     C                             VIDPO1K <> *zeros
033900030305     C                   EVAL      Chk_DES =  *blanks
034000030305     C                   MOVEL     *zeros        Chk_SOG
034100030305     C                   MOVE      VIDPO1K       Chk_SOG
034200030305     C                   EXSR      REPPO
034300030305     C                   IF        ERRL00  = '1'
034400030305     C                   SETON                                        85
034500030305     C                   MOVEL     MSG(4)        DSMSMS
034600030305     C                   ENDIF
034700030305     C                   MOVE(P)   Chk_SOG       VIDPO1K
034800030305     C                   EVAL      VIDPO1KD = Chk_DES
034900021220     C                   ENDIF
035000030305     C                   ENDIF
035100030124     C*
035200030124     C                   IF        VIDTP1 = 'D'
035300050907     C                   IF        VIDSO1K <> *blanks AND
035400050907     C                             VIDSO1K <> *zeros
035500030305     C                   EVAL      Chk_DES =  *blanks
035600030305     C                   MOVEL     *zeros        Chk_SOG
035700030305     C                   MOVE      VIDSO1K       Chk_SOG
035800030124     C                   EXSR      REPDST
035900030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
036000030305     C                   EVAL      VIDDS1  = Chk_DES
036100030124     C                   ENDIF
036200030305     C                   ENDIF
036300021220     C*
036400030107     C                   ELSE
036500030305     C                   IF        VIDSO1F <> *blanks AND
036600030305     C                             VIDSO1F <> *zeros
036700030123     C                   IF        VIDTP1 = 'F'
036800030305     C                   EVAL      Chk_DES =  *blanks
036900030305     C                   MOVEL     *zeros        Chk_SOG
037000030305     C                   MOVE      VIDSO1F       Chk_SOG
037100030107     C                   EXSR      REPPO
037200030305     C                   MOVE(P)   Chk_SOG       VIDSO1F
037300030305     C                   EVAL      VIDDS1  = Chk_DES
037400030305     C                   IF        ERRL00  = '1'
037500030305     C                   SETON                                        81
037600030305     C                   MOVEL     MSG(4)        DSMSMS
037700030305     C                   ENDIF
037800030107     C                   ENDIF
037900030107     C*
038000030107     C                   ENDIF
038100030107     C                   ENDIF
038200030305     C                   ENDIF
038300021219     C*--------------------
038400021219     C* SCELTA/CONTROLLO ARTICOLO
038500021219     C*--------------------
038600021219     C     ERRL00        IFEQ      '0'
038700021223     C                   IF        VIDCO1 <> *blanks
038800021223     C                   EVAL      Chk_DES = *blanks
038900021220     C                   EVAL      Chk_COD = VIDCO1
039000021220     C                   EXSR      REPCOD
039100021220     C                   EVAL      VIDCO1 = Chk_COD
039200021220     C                   EVAL      VIDDC1 = Chk_DES
039300021220     C                   IF        ERRL00 = '1'
039400021220     C                   SETON                                        82
039500021220     C                   MOVEL     MSG(5)        DSMSMS
039600021220     C                   ENDIF
039700021219     C                   ENDIF
039800021223     C                   ENDIF
039900030618     C*--------------------
040000030618     C* CONTROLLO SE RICHIESTA CAUSALE 1
040100030618     C*--------------------
040200030618     C                   IF        ERRL00 = '0'
040300030618     C                   IF        VIDCA1 <> *BLANKS
040400030618     C                   EVAL      chkCAU = VIDCA1
040500030618     C                   EXSR      REPCAU
040600030618     C                   EVAL      VIDCA1 = chkCAU
040700030618     C                   IF        ERRL00 = '1'
040800030618     C                   SETON                                        86
040900030618     C                   MOVEL     MSG(11)       DSMSMS
041000030618     C                   ENDIF
041100030618     C                   ENDIF
041200030618     C                   ENDIF
041300030618     C*--------------------
041400030618     C* CONTROLLO SE RICHIESTA CAUSALE 2
041500030618     C*--------------------
041600030618     C                   IF        ERRL00 = '0'
041700030618     C                   IF        VIDCA2 <> *BLANKS
041800030618     C                   EVAL      chkCAU = VIDCA2
041900030618     C                   EXSR      REPCAU
042000030618     C                   EVAL      VIDCA2 = chkCAU
042100030618     C                   IF        ERRL00 = '1'
042200030618     C                   SETON                                        87
042300030618     C                   MOVEL     MSG(11)       DSMSMS
042400030618     C                   ENDIF
042500030618     C                   ENDIF
042600030618     C                   ENDIF
042700030124     C*--------------------
042800030124     C* CONTROLLO SE SCELTO DETTAGLIO MOVIMENTI
042900030124     C*--------------------
043000030124     C     ERRL00        IFEQ      '0'
043100030124     C                   IF        VIDDET = 'S'
043200030124     C                   SETOFF                                       92
043300030124     C                   ENDIF
043400030124     C                   ENDIF
043500030123     C*--------------------
043600030123     C* CONTROLLO DATA DETTAGLIO DAL
043700030123     C*--------------------
043800030123     C     ERRL00        IFEQ      '0'
043900030123     C                   IF        VIDDAL <> *zeros
044000030123     C     *LIKE         DEFINE    VIDDAL        DALVID
044100030123     C                   Z-ADD     VIDDAL        WDA8
044200030123     C                   Z-ADD     *ZEROS        DA8W
044300030123     C                   EXSR      CHKDA8
044400030123     C                   Z-ADD     WDA8          VIDDAL
044500030123     C                   Z-ADD     DA8W          DALVID
044600030123     C     ERRL00        IFEQ      '1'
044700030123     C                   SETON                                        83
044800030124     C                   MOVEL     MSG(3)        DSMSMS
044900030123     C                   ENDIF
045000030123     C                   ENDIF
045100030123     C                   ENDIF
045200030618     C*--------------------
045300030618     C* CONTROLLO SE DATA DETTAGLIO DAL > 0 E DATA DETTAGLIO AL = 0 =>
045400030618     C* DATA DETTAGLIO AL = DATA DETTAGLIO DAL
045500030618     C*--------------------
045600030618     C     ERRL00        IFEQ      '0'
045700030618     C                   IF        VIDDAL <> *zeros AND
045800030618     C                             VIDAL   = *zeros
045900030618     C                   EVAL      VIDAL = VIDDAL
046000030618     C                   ENDIF
046100030618     C                   ENDIF
046200030123     C*--------------------
046300030123     C* CONTROLLO DATA DETTAGLIO AL
046400030123     C*--------------------
046500030123     C     ERRL00        IFEQ      '0'
046600030123     C                   IF        VIDAL  <> *zeros
046700030123     C     *LIKE         DEFINE    VIDAL         AALVID
046800030123     C                   Z-ADD     VIDAL         WDA8
046900030123     C                   Z-ADD     *ZEROS        DA8W
047000030123     C                   EXSR      CHKDA8
047100030123     C                   Z-ADD     WDA8          VIDAL
047200030123     C                   Z-ADD     DA8W          AALVID
047300030123     C     ERRL00        IFEQ      '1'
047400030123     C                   SETON                                        84
047500030124     C                   MOVEL     MSG(3)        DSMSMS
047600030123     C                   ENDIF
047700030123     C                   ENDIF
047800030123     C                   ENDIF
047900030124     C*--------------------
048000030124     C* CONTROLLO DATA DETTAGLIO DAL <= DATA DETTAGLIO AL
048100030124     C*--------------------
048200030124     C     ERRL00        IFEQ      '0'
048300150402     C                   IF        dalvid <= aalvid
048400030124     C                   ELSE
048500030124     C                   SETON                                        8384
048600030124     C                   MOVEL     MSG(2)        DSMSMS
048700030124     C                   ENDIF
048800030124     C                   ENDIF
048900030124     C*--------------------
049000030124     C* CONTROLLO OBBLIGATORIETA' CAMPI
049100030124     C*--------------------
049200030124     C     ERRL00        IFEQ      '0'
049300030124     C* SOGGETTO
049400030124     C*                  IF        VIDTP1 = 'F' AND (VIDSO1F = *blanks OR
049500030124     C*                                              VIDSO1F = *zeros)
049600030124     C*                  SETON                                        81
049700030124     C*                  MOVEL     MSG(6)        DSMSMS
049800030124     C*                  ENDIF
049900030124     C*                  IF        (VIDTP1  = 'C'     OR VIDTP1  = 'D')    AND
050000030124     C*                            (VIDSO1K = *blanks OR VIDSO1K = *zeros)
050100030124     C*                  SETON                                        81
050200030124     C*                  MOVEL     MSG(6)        DSMSMS
050300030124     C*                  ENDIF
050400030124     C* ARTICOLO
050500030124     C*                  IF        VIDCO1 = *blanks
050600030124     C*                  SETON                                        82
050700030124     C*                  MOVEL     MSG(6)        DSMSMS
050800030124     C*                  ENDIF
050900030618     C* FLAG STAMPA SALDI = 'N' E FLAG DETTAGLIO MOVIMENTI = 'N' =>
051000030618     C* ERRORE: ALMENO 1 STAMPA OCCORRE FARLA
051100030618     C                   IF        VIDSSN = 'N' AND
051200030618     C                             VIDDET = 'N'
051300030618     C                   EVAL      ERRL00 = '1'
051400030618     C                   SETON                                        8889
051500030618     C                   MOVEL     MSG(12)       DSMSMS
051600030618     C                   ENDIF
051700030305     C* SOGGETTI A ZERO SE SOGGETTI A BLANKS
051800030305     C                   IF        VIDSO1K = *blanks
051900030305     C                   EVAL      VIDSO1K = *zeros
052000030305     C                   ENDIF
052100030305     C                   IF        VIDPO1K = *blanks
052200030305     C                   EVAL      VIDPO1K = *zeros
052300030305     C                   ENDIF
052400030305     C                   IF        VIDSO1F = *blanks
052500030305     C                   EVAL      VIDSO1F = *zeros
052600030305     C                   ENDIF
052700030305     C* SOGGETTO PO SE TIPO SOGGETTO = F
052800030305     C                   IF        VIDTP1  = 'F'
052900030305     C                   IF        VIDSO1F = *zeros AND
053000030305     C                             DUTLPO <> 'S'
053100030305     C                   MOVEL     '1'           ERRL00
053200030305     C                   SETON                                        81
053300030305     C                   MOVEL     MSG(9)        DSMSMS
053400030305     C                   ENDIF
053500030305     C                   ENDIF
053600030305     C* SOGGETTO DESTINATARIO SE TIPO SOGGETTO = D
053700030305     C                   IF        VIDTP1  = 'D'
053800030305     C                   IF        VIDSO1K = *zeros AND
053900030305     C                             DUTLPO <> 'S'
054000030305     C                   MOVEL     '1'           ERRL00
054100030305     C                   SETON                                        81
054200030305     C                   MOVEL     MSG(10)       DSMSMS
054300030305     C                   ENDIF
054400030305     C                   ENDIF
054500030305     C* O SOGGETTO CLIENTE O PO CLIENTI SE TIPO SOGGETTO = C
054600030305     C                   IF        VIDTP1  = 'C'
054700030305     C                   IF        DUTLPO <> 'S'
054800030305     C                   IF        (VIDSO1K <> *zeros AND VIDPO1K <> *zeros) OR
054900030305     C                             (VIDSO1K =  *zeros AND VIDPO1K =  *zeros)
055000030305     C                   MOVEL     '1'           ERRL00
055100030305     C                   SETON                                        8185
055200030305     C                   MOVEL     MSG(8)        DSMSMS
055300030305     C                   ENDIF
055400030305     C                   ENDIF
055500030305     C                   ENDIF
055600030514     C* TIPO SOGGETTO DEVE ESSERE VALORIZZATO
055700030514     C                   IF        VIDTP1 = *blanks
055800030514     C                   MOVEL     '1'           ERRL00
055900030514     C                   SETON                                        78
056000030514     C                   MOVEL     MSG(6)        DSMSMS
056100030514     C                   ENDIF
056200030124     C* PERIODO DETTAGLIO MOVIMENTI
056300030124     C                   IF        VIDDET = 'S'
056400030124     C                   IF        VIDDAL = *zeros OR VIDAL = *zeros
056500030305     C                   MOVEL     '1'           ERRL00
056600030124     C                   SETON                                        8384
056700030305     C                   MOVEL     MSG(6)        DSMSMS
056800030124     C                   ENDIF
056900030124     C                   ENDIF
057000030124     C                   ENDIF
057100030305     C*--------------------
057200030305     C* CONTROLLO AUTORIZZAZIONI UTENTE
057300030305     C*--------------------
057400030305     C     ERRL00        IFEQ      '0'
057500030305     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
057600030305     C                   IF        VIDTP1 = 'F'
057700070510      * spengo il flag di forzatura messaggio a cambio soggetto
057800070510     c                   if        savso1f <> vidso1f
057900070510     c                   eval      wforza = *off
058000070510     c                   eval      savso1f = vidso1f
058100070510     c                   endif
058200030305     C                   MOVEL     VIDSO1F       WN3               3
058300030305     C                   SETOFF                                           28
058400030305     C     WN3           LOOKUP    L1                                     28
058500030305     C     *IN28         IFEQ      *OFF
058600070509     c     wforza        andeq     *off
058700030305     C                   SETON                                        81
058800030305     C                   MOVEL     MSG(7)        DSMSMS
058900070509     c                   eval      wforza = *on
059000030305     C                   ENDIF
059100030305     C                   ENDIF
059200030305     C                   IF        VIDTP1 = 'C'
059300070510      * spengo il flag di forzatura messaggio a cambio soggetto
059400070510     c                   if        savso1k <> vidso1k
059500070510     c                   eval      wforza = *off
059600070510     c                   eval      savso1k = vidso1k
059700070510     c                   endif
059800030305     C                   MOVEL     VIDSO1K       WA4               4
059900030305     C                   MOVE      WA4           WN3               3
060000030305     C                   SETOFF                                           28
060100030305     C     WN3           LOOKUP    L1                                     28
060200030305     C     *IN28         IFEQ      *OFF
060300070509     c     wforza        andeq     *off
060400030305     C                   SETON                                        81
060500030305     C                   MOVEL     MSG(7)        DSMSMS
060600070509     c                   eval      wforza = *on
060700030305     C                   ENDIF
060800030305     C                   ENDIF
060900030305     C                   ENDIF
061000030109     C*
061100970528     C                   ENDSR
061200021220     C*------------------------------------------------------------------------*
061300021220     C* REPKSC - CONTROLLA CODICE CLIENTE
061400021220     C*------------------------------------------------------------------------*
061500021220     C     REPKSC        BEGSR
061600021220     C*
061700021220     C* RICHIESTO ELENCO
061800021220     C                   SETOFF                                           28
061900021220     C     '?'           SCAN      Chk_SOG                                28
062000021220     C     *IN28         IFEQ      *ON
062100021220     C*
062200021220     C* RICHIESTO ELENCO CLIENTI
062300021220     C                   CLEAR                   DSXC
062400030124     C                   Z-ADD     DUTKCI        DXCCAP
062500021220     C                   CALL      'XCLIR'
062600021220     C                   PARM                    DSXC
062700021220     C* SCOMPATTA
062800021220     C                   MOVEA     DXCKSC        KSA
062900021220     C     1             DO        30            J                 3 0
063000021220     C                   MOVE      KSA(J)        DSKSA
063100021220     C                   Z-ADD     DSKSN         KSC(J)
063200021220     C                   ENDDO
063300030107     C                   MOVEL     *zeros        Chk_SOG
063400030107     C                   MOVE      KSC(1)        Chk_SOG
063500021220     C                   MOVEL     '1'           $TESVD            1
063600021220     C                   ELSE
063700021220     C*
063800021220     C* SOLO CARATTERI NUMERICI
063900021220     C                   SETOFF                                       28
064000021220     C                   TESTN                   Chk_SOG              28
064100021220     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
064200021220     C                   MOVEL     '1'           ERRL00
064300021220     C                   ELSE                                                   *E'NUMERO DA TESTN
064400021220     C                   MOVE      Chk_SOG       W001              1
064500021220     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
064600021220     C                   MOVEL     '1'           ERRL00
064700021220     C                   ELSE                                                   *NUMERO
064800021220     C*
064900021220     C* CONTROLLA ESISTENZA CLIENTE
065000021220     C                   EXSR      DECTIB
065100021220     C                   ENDIF
065200021220     C                   ENDIF
065300021220     C                   ENDIF
065400021220     C*
065500021220     C                   ENDSR
065600021220     C*------------------------------------------------------------------------*
065700021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
065800021220     C*------------------------------------------------------------------------*
065900021220     C     DECTIB        BEGSR
066000021220     C*
066100021220     C                   CLEAR                   BS69DS
066200021220     C                   CLEAR                   ACODS
066300021220     C                   MOVEL     KNSIF         I69SIF
066400021220     C                   MOVE(P)   Chk_SOG       I69KAC
066500021220     C                   CALL      'TIBS69R'
066600021220     C                   PARM                    BS69DS
066700021220     C                   PARM                    ACODS
066800021220     C                   PARM                    INDDS
066900021220     C                   PARM                    CLPDS
067000021220     C                   PARM                    CLSDS
067100021220     C     O69ERR        IFNE      '1'
067200021223     C                   EVAL      Chk_DES = acoRAG
067300021220     C                   ELSE
067400021220     C                   MOVEL     '1'           ERRL00
067500021220     C                   ENDIF
067600021220     C*
067700021220     C                   ENDSR
067800021220     C*------------------------------------------------------------------------*
067900021220     C* REPPO - CONTROLLA CODICE P.O.
068000021220     C*------------------------------------------------------------------------*
068100021220     C     REPPO         BEGSR
068200021220     C*
068300021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
068400021220     C*
068500021220     C* RICHIESTO ELENCO
068600021220     C                   SETOFF                                           28
068700021220     C     '?'           SCAN      Chk_SOG                                28
068800021220     C     *IN28         IFEQ      *ON
068900021220     C*
069000021220     C* RICHIESTO ELENCO P.O.
069100021220     C                   CALL      'TNSD24R'
069200021220     C                   PARM                    §COD              3
069300021220     C                   PARM      'F'           §TIP              1
069400021220     C                   PARM                    §§DES            25
069500021220     C                   PARM                    §§PRA             3
069600021220     C                   MOVEL     *all'0'       Chk_SOG
069700021220     C                   MOVE      §COD          Chk_SOG
069800021220     C                   ELSE
069900021220     C                   MOVE      Chk_SOG       WrkPO_N
070000021220     C     WrkPO_N       CHAIN     azorg01l
070100021220     C                   IF        %found(azorg01l)
070200021220     C                   EVAL      Chk_DES = orgDES
070300021220     C                   ELSE
070400021220     C                   EVAL      ERRL00 = '1'
070500021220     C                   EVAL      Chk_DES = *all'*'
070600021220     C                   ENDIF
070700021220     C                   ENDIF
070800021220     C*
070900021220     C                   ENDSR
071000030124     C*------------------------------------------------------------------------*
071100030124     C* REPDST - CONTROLLA CODICE DESTINATARIO
071200030124     C*------------------------------------------------------------------------*
071300030124     C     REPDST        BEGSR
071400030124     C*
071500030124     C                   MOVEL     *all'0'       WrkDST_A          7
071600030124     C                   Z-ADD     *zeros        WrkDST_N          7 0
071700030124     C*
071800030124     C* RICHIESTO ELENCO
071900030124     C                   SETOFF                                           28
072000030124     C     '?'           SCAN      Chk_SOG                                28
072100030124     C     *IN28         IFEQ      *ON
072200030124     C*
072300030124     C* RICHIESTO ELENCO DESTINATARI
072400030124     C                   MOVEL     KPJBU         SAVKPJBU
072500030124     C                   MOVEL(P)  'P01'         KPJBU
072600030124     C                   CALL      'TISIT1R'
072700030124     C                   PARM                    KPJBA
072800030124     C                   EVAL      WrkDST_A = %subst(KPJBU:4:7)
072900030124     C                   MOVEL     *all'0'       Chk_SOG
073000030124     C                   MOVE      WrkDST_A      Chk_SOG
073100030124     C                   MOVEL     SAVKPJBU      KPJBU
073200030124     C                   ELSE
073300030124     C                   MOVE(P)   Chk_SOG       WrkDST_N
073400030124     C     WrkDST_N      CHAIN     azdst04l
073500030124     C                   IF        %found(azdst04l)
073600030124     C                   EVAL      Chk_DES = dstRAG
073700030124     C                   ELSE
073800030124     C                   EVAL      ERRL00 = '1'
073900030124     C                   EVAL      Chk_DES = *all'*'
074000030124     C                   ENDIF
074100030124     C                   ENDIF
074200030124     C*
074300030124     C                   ENDSR
074400021220     C*------------------------------------------------------------------------*
074500021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
074600021220     C*------------------------------------------------------------------------*
074700021220     C     REPCOD        BEGSR
074800021220     C*
074900021220     C* RICHIESTO ELENCO
075000021220     C                   SETOFF                                           28
075100021220     C     '?'           SCAN      Chk_COD                                28
075200021220     C     *IN28         IFEQ      *ON
075300021220     C*
075400021220     C* RICHIESTO ELENCO ARTICOLI
075500021220     C                   MOVEL     KPJBU         SAVKPJBU
075600021220     C                   MOVEL(P)  'P01'         KPJBU
075700021220     C                   CALL      'TISIG1R'
075800021220     C                   PARM                    KPJBA
075900021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
076000021220     C                   MOVEL     SAVKPJBU      KPJBU
076100021220     C                   ELSE
076200021220     C     Chk_COD       CHAIN     azagb01l
076300021220     C                   IF        %found(azagb01l)
076400021220     C                   EVAL      Chk_DES = agbDES
076500021220     C                   ELSE
076600021220     C                   EVAL      ERRL00 = '1'
076700021220     C                   EVAL      Chk_DES = *all'*'
076800021220     C                   ENDIF
076900021220     C                   ENDIF
077000021220     C*
077100021220     C                   ENDSR
077200030618     C*------------------------------------------------------------------------*
077300030618     C* REPCAU - CONTROLLA CAUSALE MOVIMENTO
077400030618     C*------------------------------------------------------------------------*
077500030618     C     REPCAU        BEGSR
077600030618     C*
077700030618     C     *LIKE         DEFINE    VIDCA1        chkCAU
077800030618     C*
077900030618     C* RICHIESTO ELENCO
078000030618     C                   SETOFF                                           28
078100030618     C     '?'           SCAN      chkCAU                                 28
078200030618     C     *IN28         IFEQ      *ON
078300030618     C*
078400030618     C* VALORIZZO I PARAMETRI DI PASSAGGIO
078500030618     C                   CLEAR                   PARAM2
078600030618     C                   EVAL      P2OP1 = 'P01'
078700030618     C                   MOVEL     PARAM2        KPJBU
078800030618     C                   CALL      'TNTB31R'
078900030618     C                   PARM                    KPJBA
079000030618     C*
079100030618     C                   MOVEL     KPJBU         PARAM2
079200030618     C                   MOVEL     P2CAU         chkCAU
079300030618     C*
079400030618     C* ALTRIMENTI CONTROLLO CODICE IMMESSO
079500030618     C                   ELSE
079600030618     C                   EVAL      tbeCOD = 'GGB'
079700030618     C                   EVAL      tbeKE1 = chkCAU
079800030618     C     KEYTBEP       CHAIN     tntbe01l
079900030618     C                   IF        %found(tntbe01l)
080000030618     C                   ELSE
080100030618     C                   EVAL      ERRL00 = '1'
080200030618     C                   ENDIF
080300030618     C                   ENDIF
080400030618     C*
080500030618     C                   ENDSR
080600021220     C*------------------------------------------------------------------------*
080700021220     C* CHKDA8 - CONTROLLA LA DATA (8)
080800021220     C*------------------------------------------------------------------------*
080900021220     C     CHKDA8        BEGSR
081000021220     C*
081100030123     C     *LIKE         DEFINE    VIDDA1        WDA8
081200030123     C     *LIKE         DEFINE    VIDDA1        DA8W
081300021220     C*
081400021220     C                   Z-ADD     WDA8          G08DAT
081500021220     C                   Z-ADD     *ZEROS        G08INV
081600021220     C                   MOVEL     '0'           G08ERR
081700021220     C                   CALL      'XSRDA8'
081800021220     C                   PARM                    WLBDA8
081900021220     C                   Z-ADD     G08DAT        WDA8
082000021220     C                   Z-ADD     G08INV        DA8W
082100021220     C     G08ERR        IFEQ      '1'
082200021220     C                   MOVEL     '1'           ERRL00
082300021220     C                   ENDIF
082400021220     C*
082500021220     C                   ENDSR
082600030124     C*------------------------------------------------------------------------*
082700030124     C* CARTAB - CARICA LE TABELLE
082800030124     C*------------------------------------------------------------------------*
082900030124     C     CARTAB        BEGSR
083000030124     C*
083100030124     C* REPERISCE I DATI UTENTE
083200030124     C                   EXSR      REPDATIUTE
083300030124     C*
083400030124     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
083500030124     C                   CLEAR                   L1
083600030124     C                   CLEAR                   WAbi              2
083700030124     C*
083800030124     C                   IF        UTEAUT > *blanks
083900030124     C                   EVAL      WAbi = UTEAUT
084000030124     C                   ELSE
084100030124     C*
084200030124     C                   IF        DUTLPO = 'S'                                 *SEDE
084300030124     C                   EVAL      WAbi = 'AZ'
084400030124     C                   ENDIF
084500030124     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
084600030124     C                   EVAL      WAbi = 'TP'
084700030124     C                   ENDIF
084800030124     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
084900030124     C                   EVAL      WAbi = 'PO'
085000030124     C                   ENDIF
085100030124     C                   ENDIF
085200030124     C*
085300030124     C* Reperisce i P.O. gestibili dall'utente
085400030124     C                   CLEAR                   TRUL31DS
085500030124     C                   EVAL      I31abi = WAbi
085600030124     C                   EVAL      I31cpo = DUTPOU
085700030124     C                   CALL      'TRUL31R'
085800030124     C                   PARM                    KPJBA
085900030124     C                   PARM                    TRUL31DS
086000030124     C                   IF        O31pog > *zeros
086100030124     C                   MOVEA     O31pog        L1
086200030124     C                   ELSE
086300030124     C                   EVAL      FINL00 = '1'
086400030124     C                   ENDIF
086500030124     C*
086600030124     C                   ENDSR
086700021219     C*--------------------------------------------------------------------------------------------*
086800021219     C* REPERISCE I DATI UTENTE
086900021219     C*--------------------------------------------------------------------------------------------*
087000021219     C     REPDATIUTE    BEGSR
087100021219     C*
087200021219     C* INIZIALIZZA VARIABILI DI WRK
087300021219     C                   CLEAR                   TIBS34DS
087400021219     C                   CLEAR                   AZUTEDS
087500021219     C                   CLEAR                   DDATIUTE
087600021219     C*
087700021219     C     *DTAARA       DEFINE    §azute        azuteds
087800021219     C     *DTAARA       DEFINE    §datiute      ddatiute
087900021219     C                   IN(E)     *DTAARA
088000021219     C                   IF        %Error
088100021220     C                   EVAL      I34Tla = 'L'
088200021219     C                   CALL      'TIBS34R'
088300021219     C                   PARM                    Tibs34Ds
088400021219     C                   IN        *DTAARA
088500021219     C                   ENDIF
088600021219     C*
088700021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
088800021219     C                   MOVEL(P)  RSUT          DSFIRS           20
088900021219     C*
089000021219     C                   ENDSR
089100030123     C*------------------------------------------------------------------------*
089200030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
089300030123     C*------------------------------------------------------------------------*
089400030123     C     VIDPAR        BEGSR
089500030123     C*
089600030123     C                   Z-ADD     DA1VID        PARDA1
089700030123     C                   MOVEL     VIDTP1        PARTP1
089800030305     C                   IF        VIDTP1 = 'C'
089900030305     C                   MOVEL     VIDSO1K       PARSO1
090000030305     C                   MOVEL     VIDPO1K       PARPO1
090100030305     C                   ENDIF
090200030305     C                   IF        VIDTP1 = 'D'
090300030305     C                   MOVEL     VIDSO1K       PARSO1
090400030305     C                   ENDIF
090500030305     C                   IF        VIDTP1 = 'F'
090600030612     C                   MOVEL     *all'0'       PARSO1
090700030612     C                   MOVE      VIDSO1F       PARSO1
090800030305     C                   ENDIF
090900030123     C                   MOVEL     VIDCO1        PARCO1
091000030123     C                   MOVEL     VIDSEL        PARSEL
091100030123     C                   MOVEL     VIDDET        PARDET
091200030618     C                   MOVEL     VIDSSN        PARSSN
091300030618     C                   MOVEL     VIDCA1        PARCA1
091400030618     C                   MOVEL     VIDCA2        PARCA2
091500030123     C                   Z-ADD     DALVID        PARDDA
091600030123     C                   Z-ADD     AALVID        PARDAA
091700030123     C*
091800030123     C                   ENDSR
091900030123     C*------------------------------------------------------------------------*
092000030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
092100030123     C*------------------------------------------------------------------------*
092200030123     C     AZZVID        BEGSR
092300030123     C*
092400030124     C                   Z-ADD     WN8           VIDDA1
092500030124     C                   MOVEL     *blanks       VIDTP1
092600030123     C                   MOVEL     *blanks       VIDSO1K
092700030123     C                   MOVEL     *blanks       VIDSO1F
092800030123     C                   MOVEL     *blanks       VIDCO1
092900030618     C                   MOVEL     *blanks       VIDCA1
093000030618     C                   MOVEL     *blanks       VIDCA2
093100030123     C                   MOVEL     'N'           VIDSEL
093200030123     C                   MOVEL     'N'           VIDDET
093300030618     C                   MOVEL     'S'           VIDSSN
093400030123     C                   Z-ADD     *zeros        VIDDAL
093500030123     C                   Z-ADD     *zeros        VIDAL
093600150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
093700030123     C                   SETOFF                                       55
093800030305     C                   SETOFF                                       56
093900030124     C                   SETON                                        92
094000030123     C*
094100030123     C                   ENDSR
094200030123     C*------------------------------------------------------------------------*
094300030123     C* *INZSR - OPERAZIONI INIZIALI
094400030123     C*------------------------------------------------------------------------*
094500030123     C     *INZSR        BEGSR
094600030123     C*
094700030123     C* RICEVIMENTO PARAMETRI
094800030123     C     *ENTRY        PLIST
094900030123     C                   PARM                    KPJBA
095000030618     C*---
095100030618     C* CHIAVI DI LETTURA
095200030618     C*---
095300030618     C* LETTURA tntbe01l - PARZIALE
095400030618     C     KEYTBEP       KLIST
095500030618     C                   KFLD                    tbeCOD
095600030618     C                   KFLD                    tbeKE1
095700030123     C*
095800030124     C* CARICAMENTO TABELLE
095900030124     C                   EXSR      CARTAB
096000030123     C*
096100030123     C* REPERISCO DATA E ORA CORRENTI
096200030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
096300030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
096400030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
096500030123     C                   Z-ADD     WN8           G08DAT
096600030123     C                   Z-ADD     *ZEROS        G08INV
096700030123     C                   MOVEL     '0'           G08ERR
096800030123     C                   CALL      'XSRDA8'
096900030123     C                   PARM                    WLBDA8
097000030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
097100030123     C*
097200030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
097300030123     C     XMSGPA        PLIST
097400030123     C                   PARM                    DSMSMS
097500030123     C                   PARM                    DSMSPG
097600030123     C                   PARM                    DSMSKE
097700030123     C                   PARM                    DSMSER
097800030123     C*
097900030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
098000030123     C                   CALL      'XKEY'
098100030123     C                   PARM                    DSKEY
098200030123     C*
098300030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
098400030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
098500030123     C*
098600030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
098700030123     C*   -NOME PGM CHIAMANTE
098800030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
098900030123     C                   MOVEL     'TISIG9R'     DSMSPG
099000030123     C                   MOVEL     'TISIG9R'     MSGQUE
099100030123     C                   MOVEL     *BLANKS       MSGKEY
099200030123     C                   MOVEL     *BLANKS       DSMSMS
099300030123     C*
099400030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
099500030123     C                   Z-ADD     0             FUNGRU            5 0
099600030123     C*
099700030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
099800030123     C                   EXSR      FUNGES
099900030123     C*
100000030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
100100030123     C                   MOVEL     '0'           F12L00            1
100200030123     C                   MOVEL     '0'           F03L00            1
100300030123     C                   MOVEL     '0'           FINL00            1
100400030123     C                   MOVEL     '0'           ERRL00            1
100500030123     C                   MOVEL     '0'           FLFINE            1
100600030123     C*
100700030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
100800030123     C                   EXSR      AZZVID
100900030123     C*
101000030123     C                   ENDSR
101100030123     C*------------------------------------------------------------------------*
101200030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
101300030123     C*------------------------------------------------------------------------*
101400030123     C     FUNGES        BEGSR
101500030123     C*
101600030123     C* PULISCO CAMPI DI LAVORO
101700030123     C                   Z-ADD     0             W                 5 0
101800030123     C*
101900030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
102000030123     C* E CARICO TABELLA FUNZIONI ATTIVE
102100030123     C     1             DO        24            I
102200030123     C                   MOVEL     FUD(I)        FUDRIG
102300030123     C*
102400030123     C* PULIZIA FUNZIONI PRECEDENTI
102500030123     C                   MOVEL     *BLANKS       FUV(I)
102600030123     C*
102700030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
102800030123     C* (SALTA F24)
102900030123     C     FUDATT        IFEQ      'S'
103000030123     C     FUDFUN        ANDNE     '24'
103100030123     C                   ADD       1             W
103200030123     C                   MOVEL     FUDDES        FUV(W)
103300030123     C                   END
103400030123     C                   END                                                    DO WHILE
103500030123     C*
103600030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
103700030123     C* ALTRIMENTI DISATTIVO F24
103800030123     C     1             DO        24            I
103900030123     C                   MOVEL     FUD(I)        FUDRIG
104000030123     C*
104100030123     C     FUDFUN        IFEQ      '24'
104200030123     C     W             IFGT      6
104300030123     C                   MOVEL     'S'           FUDATT
104400030123     C                   ELSE
104500030123     C                   MOVEL     *BLANKS       FUDATT
104600030123     C                   END                                                    W GT 8
104700030123     C                   MOVEL     FUDRIG        FUD(I)
104800030123     C                   END
104900030123     C                   END                                                    DO WHILE
105000030123     C*
105100030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
105200030123     C     W             DIV       6             I
105300030123     C                   MVR                     Z
105400030123     C*
105500030123     C     Z             IFNE      0
105600030123     C     I             ADD       1             Z
105700030123     C                   ELSE
105800030123     C                   Z-ADD     I             Z                 5 0
105900030123     C                   END
106000030123     C*
106100030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
106200030123     C                   ADD       1             FUNGRU
106300030123     C     FUNGRU        IFGT      Z
106400030123     C     FUNGRU        ORLT      1
106500030123     C                   Z-ADD     1             FUNGRU
106600030123     C                   END
106700030123     C*
106800030123     C* PRIMO GRUPPO DI 6 FUNZIONI
106900030123     C     FUNGRU        IFEQ      1
107000030123     C                   MOVEL     FUV(1)        FUN001
107100030123     C                   MOVEL     FUV(2)        FUN002
107200030123     C                   MOVEL     FUV(3)        FUN003
107300030123     C                   MOVEL     FUV(4)        FUN004
107400030123     C                   MOVEL     FUV(5)        FUN005
107500030123     C                   MOVEL     FUV(6)        FUN006
107600030123     C                   END
107700030123     C*
107800030123     C* SECONDO GRUPPO DI 6 FUNZIONI
107900030123     C     FUNGRU        IFEQ      2
108000030123     C                   MOVEL     FUV(7)        FUN001
108100030123     C                   MOVEL     FUV(8)        FUN002
108200030123     C                   MOVEL     FUV(9)        FUN003
108300030123     C                   MOVEL     FUV(10)       FUN004
108400030123     C                   MOVEL     FUV(11)       FUN005
108500030123     C                   MOVEL     FUV(12)       FUN006
108600030123     C                   END
108700030123     C*
108800030123     C* TERZO GRUPPO DI 6 FUNZIONI
108900030123     C     FUNGRU        IFEQ      3
109000030123     C                   MOVEL     FUV(13)       FUN001
109100030123     C                   MOVEL     FUV(14)       FUN002
109200030123     C                   MOVEL     FUV(15)       FUN003
109300030123     C                   MOVEL     FUV(16)       FUN004
109400030123     C                   MOVEL     FUV(17)       FUN005
109500030123     C                   MOVEL     FUV(18)       FUN006
109600030123     C                   END
109700030123     C*
109800030123     C* QUARTO GRUPPO DI 6 FUNZIONI
109900030123     C     FUNGRU        IFEQ      4
110000030123     C                   MOVEL     FUV(19)       FUN001
110100030123     C                   MOVEL     FUV(20)       FUN002
110200030123     C                   MOVEL     FUV(21)       FUN003
110300030123     C                   MOVEL     FUV(22)       FUN004
110400030123     C                   MOVEL     *BLANKS       FUN005
110500030123     C                   MOVEL     *BLANKS       FUN006
110600030123     C                   END
110700030123     C*
110800030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
110900030123     C                   MOVEL     *BLANKS       FUN007
111000030123     C                   MOVEL     *BLANKS       FUN008
111100030123     C*
111200030123     C     1             DO        24            I
111300030123     C                   MOVEL     FUD(I)        FUDRIG
111400030123     C     FUDFUN        IFEQ      '24'
111500030123     C     FUDATT        ANDEQ     'S'
111600030123     C                   MOVEL     FUDDES        FUN008
111700030123     C                   END
111800030123     C                   END                                                    DO
111900030123     C                   ENDSR
112000030123     C*------------------------------------------------------------------------*
112100030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
112200030123     C*------------------------------------------------------------------------*
112300030123     C     FUNCON        BEGSR
112400030123     C*
112500030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
112600030123     C                   EXSR      FUNVAR
112700030123     C*
112800030123     C* PULISCO CAMPI DI LAVORO
112900030123     C                   Z-ADD     0             I                 5 0
113000030123     C                   Z-ADD     0             W                 5 0
113100030123     C*
113200030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
113300030123     C     FUNKEY        IFNE      *BLANKS
113400030123     C     1             DO        24            I
113500030123     C                   MOVEL     FUD(I)        FUDRIG
113600030123     C*
113700030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
113800030123     C     FUNKEY        IFEQ      FUDFUN
113900030123     C     FUDATT        ANDEQ     'S'
114000030123     C                   Z-ADD     I             W
114100030123     C                   END
114200030123     C                   END                                                    DO WHILE
114300030123     C*
114400030123     C* FUNZIONE NON ATTIVA (ERRORE)
114500030123     C     W             IFEQ      0
114600030123     C*
114700030123     C* EMISSIONE MESSAGGIO DI ERRORE
114800030123     C                   MOVEL     MSG(1)        DSMSMS
114900030123     C*
115000030123     C                   END                                                    W=0
115100030123     C                   END                                                    FUNKEY NE *BLANKS
115200030123     C*
115300030123     C* FUNZIONE ATTIVA (PROCEDI ...)
115400030123     C     W             IFNE      0
115500030123     C     FUNKEY        OREQ      *BLANKS
115600030123     C*
115700030123     C     FUNKEY        IFEQ      *BLANKS
115800030123     C                   EXSR      ENTGES
115900030123     C                   END
116000030123     C     FUNKEY        IFEQ      '03'
116100030123     C                   EXSR      F03GES
116200030123     C                   END
116300030123     C     FUNKEY        IFEQ      '06'
116400030123     C                   EXSR      F06GES
116500030123     C                   END
116600030123     C     FUNKEY        IFEQ      '12'
116700030123     C                   EXSR      F12GES
116800030123     C                   END
116900030123     C     FUNKEY        IFEQ      '24'
117000030123     C                   EXSR      F24GES
117100030123     C                   END
117200030123     C*
117300030123     C                   END
117400030123     C*
117500030123     C                   ENDSR
117600030123     C*------------------------------------------------------------------------*
117700030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117800030123     C*------------------------------------------------------------------------*
117900030123     C     FUNVAR        BEGSR
118000030123     C*
118100030123     C* INIZIALIZZA VARIABILE -FUNKEY-
118200030123     C*
118300030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
118400030123     C     KEY           IFEQ      ENTER
118500030123     C                   MOVEL     *BLANKS       FUNKEY
118600030123     C                   END
118700030123     C     KEY           IFEQ      F1
118800030123     C                   MOVEL     '01'          FUNKEY
118900030123     C                   END
119000030123     C     KEY           IFEQ      F2
119100030123     C                   MOVEL     '02'          FUNKEY
119200030123     C                   END
119300030123     C     KEY           IFEQ      F3
119400030123     C                   MOVEL     '03'          FUNKEY
119500030123     C                   END
119600030123     C     KEY           IFEQ      F4
119700030123     C                   MOVEL     '04'          FUNKEY
119800030123     C                   END
119900030123     C     KEY           IFEQ      F5
120000030123     C                   MOVEL     '05'          FUNKEY
120100030123     C                   END
120200030123     C     KEY           IFEQ      F6
120300030123     C                   MOVEL     '06'          FUNKEY
120400030123     C                   END
120500030123     C     KEY           IFEQ      F7
120600030123     C                   MOVEL     '07'          FUNKEY
120700030123     C                   END
120800030123     C     KEY           IFEQ      F8
120900030123     C                   MOVEL     '08'          FUNKEY
121000030123     C                   END
121100030123     C     KEY           IFEQ      F9
121200030123     C                   MOVEL     '09'          FUNKEY
121300030123     C                   END
121400030123     C     KEY           IFEQ      F10
121500030123     C                   MOVEL     '10'          FUNKEY
121600030123     C                   END
121700030123     C     KEY           IFEQ      F11
121800030123     C                   MOVEL     '11'          FUNKEY
121900030123     C                   END
122000030123     C     KEY           IFEQ      F12
122100030123     C                   MOVEL     '12'          FUNKEY
122200030123     C                   END
122300030123     C     KEY           IFEQ      F13
122400030123     C                   MOVEL     '13'          FUNKEY
122500030123     C                   END
122600030123     C     KEY           IFEQ      F14
122700030123     C                   MOVEL     '14'          FUNKEY
122800030123     C                   END
122900030123     C     KEY           IFEQ      F15
123000030123     C                   MOVEL     '15'          FUNKEY
123100030123     C                   END
123200030123     C     KEY           IFEQ      F16
123300030123     C                   MOVEL     '16'          FUNKEY
123400030123     C                   END
123500030123     C     KEY           IFEQ      F17
123600030123     C                   MOVEL     '17'          FUNKEY
123700030123     C                   END
123800030123     C     KEY           IFEQ      F18
123900030123     C                   MOVEL     '18'          FUNKEY
124000030123     C                   END
124100030123     C     KEY           IFEQ      F19
124200030123     C                   MOVEL     '19'          FUNKEY
124300030123     C                   END
124400030123     C     KEY           IFEQ      F20
124500030123     C                   MOVEL     '20'          FUNKEY
124600030123     C                   END
124700030123     C     KEY           IFEQ      F21
124800030123     C                   MOVEL     '21'          FUNKEY
124900030123     C                   END
125000030123     C     KEY           IFEQ      F22
125100030123     C                   MOVEL     '22'          FUNKEY
125200030123     C                   END
125300030123     C     KEY           IFEQ      F23
125400030123     C                   MOVEL     '23'          FUNKEY
125500030123     C                   END
125600030123     C     KEY           IFEQ      F24
125700030123     C                   MOVEL     '24'          FUNKEY
125800030123     C                   END
125900030123     C     KEY           IFEQ      ROLLUP
126000030123     C                   MOVEL     'RU'          FUNKEY
126100030123     C                   END
126200030123     C     KEY           IFEQ      HELP
126300030123     C                   MOVEL     'HP'          FUNKEY
126400030123     C                   END
126500030123     C                   ENDSR
126600030123     C*------------------------------------------------------------------------*
126700030123     C* NOTE PER COMPILAZIONE SCHIERE
126800030123     C*------------------------------------------------------------------------*
126900030123     C* -MSG-
127000030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
127100030123     C* (NON TOCCARE MSG,1)
127200030123     C*
127300030123     C* -CMD-
127400030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
127500030123     C*
127600030123     C* -FUD-
127700030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
127800030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
127900030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
128000030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
128100030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
128200030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
128300030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
128400030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
128500030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
128600030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
128700030123     C*
128800030123     C*------------------------------------------------------------------------*
128900030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
129000030123TISIG9R- Tasto funzionale non gestito.                                         1
129100030123TISIG9R- Data iniziale maggiore di data finale.                                2
129200030124TISIG9R- Data errata.                                                          3
129300030124TISIG9R- Soggetto movimento errato.                                            4
129400030124TISIG9R- Articolo indicato errato.                                             5
129500030124TISIG9R- Campo obbligatorio.                                                   6
129600070509TISIG9R- Soggetto indicato non in gestione all'utente. Enter per forzare!      7
129700150210TISIG9R- Indicare o il soggetto cliente o il Fil. dei soggetti clienti.        8
129800150210TISIG9R- Indicare il soggetto Fil.                                             9
129900030305TISIG9R- Indicare il soggetto destinatario.                                    10
130000030618TISIG9R- Causale errata o inesistente, correggere.                             11
130100030618TISIG9R- Effettuare almeno una scelta.                                         12
130200030123TISIG9R-
130300030123TISIG9R-
130400030123TISIG9R-
130500030124TISIG9R-
130600030123TISIG9R-
130700030123TISIG9R-
130800030123TISIG9R-
130900030123TISIG9R-
131000030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
131100030123                   -        (S/N)=N                        funz.01¬
131200030123                   -        (S/N)=N                        funz.02¬
131300030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
131400030123                   -        (S/N)=N                        funz.04¬
131500030123                   -        (S/N)=N                        funz.05¬
131600030123F06=Lancio         -        (S/N)=S                        funz.06¬
131700030123                   -        (S/N)=N                        funz.07¬
131800030123                   -        (S/N)=N                        funz.08¬
131900030123                   -        (S/N)=N                        funz.09¬
132000030123                   -        (S/N)=N                        funz.10¬
132100030123                   -        (S/N)=N                        funz.11¬
132200030123F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
132300030123                   -        (S/N)=N                        funz.13¬
132400030123                   -        (S/N)=N                        funz.14¬
132500030123                   -        (S/N)=N                        funz.15¬
132600030123                   -        (S/N)=N                        funz.16¬
132700030123                   -        (S/N)=N                        funz.17¬
132800030123                   -        (S/N)=N                        funz.18¬
132900030123                   -        (S/N)=N                        funz.19¬
133000030123                   -        (S/N)=N                        funz.20¬
133100030123                   -        (S/N)=N                        funz.21¬
133200030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
133300030123                   -        (S/N)=N                        funz.23¬
133400030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
