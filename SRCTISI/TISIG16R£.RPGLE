000100970526     H*------------------------------------------------------------------------*
000200150211     H* Statistica movimenti GABBIE e BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150211     FTISIG14d  CF   E             WORKSTN INFDS(DEVDS1)
000900030123     F                                     SFILE(SIG9DMS:MSGULT)
001000021219     Fazorg01l  IF   E           K DISK
001100021219     Fcnaco00f  IF   E           K DISK
001200021219     Fazagb01l  IF   E           K DISK
001300150216     FazFgb01l  IF   E           K DISK
001400030618     Ftntbe01l  IF   E           K DISK
001500150409     Ftabel00f  IF   E           K DISK
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* MESSAGGI
002100970526     D*------------
002200970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* DESCRIZIONI TASTI FUNZIONALI
002500970526     D*------------
002600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* FUNZIONI DA CARICARE A VIDEO
002900970526     D*------------
003000970526     D FUV             S             19    DIM(24)
003100150211     d* p.o. abilitati
003200150211     D $pog            s              3    dim(250)
003300150211     D $DIG            s              1    dim(20)
003400150211     D $ARG            s              3    dim(50)
003500970526     D*------------------------------------------------------------------------*
003600970526     D* INPUT
003700970526     D*------------------------------------------------------------------------*
003800970526     D*------------
003900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004000970526     D*------------
004100970526     D FUDDS           DS
004200970526     D  FUDDE1                 1      1
004300970526     D  FUDFUN                 2      3
004400970526     D  FUDDE2                 4     19
004500970526     D  FUDFI0                20     34
004600970526     D  FUDATT                35     35
004700970526     D  FUDFI1                36     80
004800970526     D  FUDDES                 1     19
004900970526     D  FUDRIG                 1     80
005000970526     D*------------
005100970526     D* DS GESTIONE TASTI FUNZIONALI
005200970526     D*------------
005300970526     D DSKEY         E DS
005400970526     D DEVDS1          DS
005500970526     D  KEY                  369    369
005600970526     D*------------
005700970526     D* DS "XMSG" - GESTIONE MESSAGGI
005800970526     D*------------
005900970526     D DSMSG         E DS
006000021219     D*------------
006100970526     D* ARCHITETTURA
006200970526     D*------------
006300970526     D KPJBA         E DS
006400021220     D*------------------
006500021220     D* DS REPERIMENTO DATI UTENTE
006600021220     D*-------------------
006700021220     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006800021220     D ACODS         E DS                  EXTNAME(CNACO00F)
006900021220     D INDDS         E DS                  EXTNAME(CNIND00F)
007000021220     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007100021220     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007200000407     D*------------------
007300000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
007400000407     D*------------------
007500000407     D WLBDA8          DS                  INZ
007600000407     D  G08DAT                 1      8  0
007700000407     D  G08INV                 9     16  0
007800000407     D  G08ERR                17     17
007900000407     D  G08TGI                18     22  0
008000030123     D*-------------------
008100030123     D* DS REPERIMENTO DATI UTENTE
008200030123     D*-------------------
008300030123     D TIBS34DS      E DS                                                       *Profili utente
008400030123     D DDATIUTE      E DS                                                       *Dati utente
008500030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008600150211     D dute01        E DS                                                       *Utenti
008700150211     d dLat          e ds
008800150211     d TIBS02DS      E DS
008900150409     d ds05          e ds
009000150211     d wabi            s                   like(UteAut)
009100030124     D*------------------
009200030124     D* DS CARICAMENTO P.O. GESTITI
009300030124     D*-------------------
009400150211     D TRUL31DS      E DS
009500150211     D TRUL31DS2     E DS
009600021219     D*------------------
009700021219     D* PASSAGGIO PARAMETRI
009800021219     D*------------------
009900030123     D PARAM           DS                  INZ
010000150211     D  PARDA1                        8s 0
010100030123     D  PARTP1                        1
010200030123     D  PARSO1                        8
010300030123     D  PARCO1                       15
010400030305     D  PARPO1                        3
010500150211     D  PARper                        3s 0
010600150211     D  PARgia                        1
010700150218     D  PARdet                        1
010800150402     D  PARfile                       1
010900150402     D  PARpulf                       1
011000150408     D  PARDAl                        8s 0
011100150408     D  PARal                         8s 0
011200150409     D  PARarea                       3
011300150409     D  PARtsg                        1
011400150216     D*------------------
011500150216     D* PASSAGGIO PARAMETRI TISIG10
011600150216     D*------------------
011700150216     D PARAM10         DS
011800150216     d  pop1                          3
011900150216     D  Pksu                          8
012000150216     D  Ptsc                          1
012100150216     D  Pmsg                         80
012200030123     D*------------------
012300030123     D* VARIABILI DI WRK
012400030123     D*------------------
012500030123     D Chk_SOG                        8A
012600030123     D Chk_COD                       15A
012700030123     D Chk_DES                       30A
012800030123     D SAVKPJBU                            LIKE(KPJBU)
012900070509
013000070509      * flag per forzare messaggio errore sul cliente
013100070509     d wforza          s              1    inz(*off)
013200070510      * campi di sav
013300070510     d savso1f         s                   like(vidso1f) inz
013400070510     d savso1k         s                   like(vidso1k) inz
013500070509
013600970526     C*------------------------------------------------------------------------*
013700970526     C* MAIN LINES
013800970526     C*------------------------------------------------------------------------*
013900970526     C*
014000970526     C* NON CI SONO ERRORI IN ENTRATA
014100021211     C     ERRL00        IFEQ      '0'
014200970526     C*
014300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014400021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014700970526     C*
014800970526     C* EMETTE FORMATO VIDEO
014900970526     C                   EXSR      BUTFMR
015000970526     C*
015100970526     C* GESTIONE TASTI FUNZIONALI
015200970526     C                   EXSR      FUNCON
015300021211     C                   ENDDO
015400970526     C*
015500021211     C                   ENDIF
015600021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
015700021211     C                   MOVEL     PARAM         KPJBU
015800970526     C* FINE PGM
015900970526     C                   SETON                                        LR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016200970526     C*------------------------------------------------------------------------*
016300970526     C     BUTFMR        BEGSR
016400970526     C*
016500970526     C* EMETTE LA TESTATA
016600030123     C                   WRITE     SIG9D01
016700970526     C*
016800970526     C* SPEDISCE EVENTUALE MESSAGGIO
016900970526     C                   EXSR      SNDMSG
017000970526     C*
017100970526     C* EMETTE LE FUNZIONI
017200030123     C                   WRITE     SIG9D02
017300970526     C*
017400970526     C* EMETTE IL VIDEO
017500030123     C                   EXFMT     SIG9D03
017600970526     C*
017700970526     C* REIMPOSTA FLAG DI ERRORE
017800970526     C                   MOVEL     '0'           ERRL00
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     SNDMSG        BEGSR
018500970526     C*
018600970526     C* PULISCE IL SFILE MESSAGGI
018700970526     C                   MOVEL     '1'           *IN40
018800030123     C                   WRITE     SIG9DMC
018900970526     C                   MOVEL     '0'           *IN40
019000970526     C*
019100970526     C* VISUALIZZA IL MESSAGGIO
019200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
019300970526     C                   CALL      'XMSG'        XMSGPA
019400021211     C     DSMSER        IFEQ      *ZEROS
019500970526     C                   MOVEL     DSMSKE        MSGKEY
019600021211     C                   ENDIF
019700970526     C                   Z-ADD     1             MSGULT
019800030123     C                   WRITE     SIG9DMS
019900030123     C                   WRITE     SIG9DMC
020000970526     C*
020100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
020200970526     C                   MOVEL     *BLANKS       DSMSMS
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600021219     C* ENTGES - GESTIONE ENTER -> INVIO
020700970526     C*------------------------------------------------------------------------*
020800970526     C     ENTGES        BEGSR
020900970526     C*
021000970526     C* CONTROLLA I CAMPI IMMESSI
021100970526     C                   EXSR      DECODI
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F03GES - GESTIONE F03 -> FINE
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F03GES        BEGSR
021800970526     C*
021900970526     C* IMPOSTA "PREMUTO F03"
022000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400030123     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F06GES        BEGSR
022700030123     C*
022800030123     C                   EXSR      DECODI
022900030123     C*
023000030123     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
023100030123     C     ERRL00        IFEQ      '0'
023200030123     C*
023300030123     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
023400150212     C                   MOVEL     'SIGC'        KCOAZ
023500030123     C                   EXSR      VIDPAR
023600030123     C                   MOVEL     PARAM         KPJBU
023700030618     C                   CALL      'BCH10'
023800030618 MOM C***                CALL      'TISIGSR'
023900030123     C                   PARM                    KPJBA
024000030123     C*
024100030123     C* IMPOSTO FLAG F03 DI LIVELLO PGM
024200030123     C                   MOVEL     '1'           F03L00
024300030123     C*
024400030123     C                   ENDIF
024500021211     C*
024600970526     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800970526     C* F12GES - GESTIONE F12 -> RITORNO
024900970526     C*------------------------------------------------------------------------*
025000970526     C     F12GES        BEGSR
025100970526     C*
025200970526     C* IMPOSTA "PREMUTO F12"
025300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025800970526     C*------------------------------------------------------------------------*
025900970526     C     F24GES        BEGSR
026000970526     C*
026100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026600970526     C*------------------------------------------------------------------------*
026700970526     C     DECODI        BEGSR
026800030305     C*
026900000407     C                   MOVEL     '0'           ERRL00
027000000407     C*
027100000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
027200150408     C                   SETOFF                                       787776
027300030305     C                   SETOFF                                       798182
027400150211     C                   SETOFF                                       85
027500030305     C*
027600030305     C* INIZIALIZZO I CAMPI DESCRIZIONE
027700030305     C                   EVAL      VIDDS1 = *blanks
027800030305     C                   EVAL      VIDPO1KD = *blanks
027900030123     C*--------------------
028000030123     C* CONTROLLO DATA SALDO AL
028100030123     C*--------------------
028200030123     C     ERRL00        IFEQ      '0'
028300030123     C                   IF        VIDDA1 <> *zeros
028400030123     C     *LIKE         DEFINE    VIDDA1        DA1VID
028500030123     C                   Z-ADD     VIDDA1        WDA8
028600030123     C                   Z-ADD     *ZEROS        DA8W
028700030123     C                   EXSR      CHKDA8
028800030123     C                   Z-ADD     WDA8          VIDDA1
028900030123     C                   Z-ADD     DA8W          DA1VID
029000030123     C     ERRL00        IFEQ      '1'
029100030123     C                   SETON                                        79
029200030124     C                   MOVEL     MSG(3)        DSMSMS
029300030123     C                   ENDIF
029400030123     C                   ENDIF
029500030123     C                   ENDIF
029600021219     C*--------------------
029700030123     C* SCELTA/CONTROLLO SOGGETTO
029800021219     C*--------------------
029900021219     C     ERRL00        IFEQ      '0'
030000021223     C* Decodifico il tipo di soggetto movimento
030100030123     C                   IF        VIDTP1 = 'C'
030200030305     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
030300030123     C                   SETON                                        55
030400030305     C                   SETON                                        56
030500021223     C                   ENDIF
030600030123     C                   IF        VIDTP1 = 'F'
030700150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
030800030123     C                   SETOFF                                       55
030900030305     C                   SETOFF                                       56
031000021223     C                   ENDIF
031100021223     C* Eseguo i controlli di validità soggetto movimento
031200030107     C                   IF        *IN55   =  *ON
031300021220     C*
031400030305     C                   IF        VIDTP1 = 'C'
031500030305     C                   IF        VIDSO1K <> *blanks AND
031600030305     C                             VIDSO1K <> *zeros
031700030305     C                   EVAL      Chk_DES =  *blanks
031800030305     C                   MOVEL     *zeros        Chk_SOG
031900030305     C                   MOVE      VIDSO1K       Chk_SOG
032000021220     C                   EXSR      REPKSC
032100030305     C                   IF        ERRL00  = '1'
032200030305     C                   SETON                                        81
032300030305     C                   MOVEL     MSG(4)        DSMSMS
032400030305     C                   ENDIF
032500030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
032600030305     C                   EVAL      VIDDS1  = Chk_DES
032700030305     C                   ENDIF
032800030305     C                   IF        VIDPO1K <> *blanks AND
032900030305     C                             VIDPO1K <> *zeros
033000030305     C                   EVAL      Chk_DES =  *blanks
033100030305     C                   MOVEL     *zeros        Chk_SOG
033200030305     C                   MOVE      VIDPO1K       Chk_SOG
033300030305     C                   EXSR      REPPO
033400030305     C                   IF        ERRL00  = '1'
033500030305     C                   SETON                                        85
033600030305     C                   MOVEL     MSG(4)        DSMSMS
033700030305     C                   ENDIF
033800030305     C                   MOVE(P)   Chk_SOG       VIDPO1K
033900030305     C                   EVAL      VIDPO1KD = Chk_DES
034000021220     C                   ENDIF
034100030305     C                   ENDIF
034200021220     C*
034300030107     C                   ELSE
034400030305     C                   IF        VIDSO1F <> *blanks AND
034500030305     C                             VIDSO1F <> *zeros
034600030123     C                   IF        VIDTP1 = 'F'
034700030305     C                   EVAL      Chk_DES =  *blanks
034800030305     C                   MOVEL     *zeros        Chk_SOG
034900030305     C                   MOVE      VIDSO1F       Chk_SOG
035000030107     C                   EXSR      REPPO
035100030305     C                   MOVE(P)   Chk_SOG       VIDSO1F
035200030305     C                   EVAL      VIDDS1  = Chk_DES
035300030305     C                   IF        ERRL00  = '1'
035400030305     C                   SETON                                        81
035500030305     C                   MOVEL     MSG(4)        DSMSMS
035600030305     C                   ENDIF
035700030107     C                   ENDIF
035800030107     C*
035900030107     C                   ENDIF
036000030107     C                   ENDIF
036100030305     C                   ENDIF
036200150409     C*--------------------
036300150409     C* SCELTA/area
036400150409     C*--------------------
036500150409     C     ERRL00        IFEQ      '0'
036600150409     C                   IF        VIDarea <> *blanks
036700150409     C                   EXSR      REParea
036800150409     C                   IF        ERRL00 = '1'
036900150409     C                   SETON                                        76
037000150409     C                   MOVEL     MSG(14)       DSMSMS
037100150409     C                   ENDIF
037200150409     C                   ENDIF
037300150409     C                   ENDIF
037400021219     C*--------------------
037500021219     C* SCELTA/CONTROLLO ARTICOLO
037600021219     C*--------------------
037700021219     C     ERRL00        IFEQ      '0'
037800021223     C                   IF        VIDCO1 <> *blanks
037900021223     C                   EVAL      Chk_DES = *blanks
038000021220     C                   EVAL      Chk_COD = VIDCO1
038100021220     C                   EXSR      REPCOD
038200021220     C                   EVAL      VIDCO1 = Chk_COD
038300021220     C                   EVAL      VIDDC1 = Chk_DES
038400021220     C                   IF        ERRL00 = '1'
038500021220     C                   SETON                                        82
038600021220     C                   MOVEL     MSG(5)        DSMSMS
038700021220     C                   ENDIF
038800021219     C                   ENDIF
038900021223     C                   ENDIF
039000030305     C* SOGGETTI A ZERO SE SOGGETTI A BLANKS
039100030305     C                   IF        VIDSO1K = *blanks
039200030305     C                   EVAL      VIDSO1K = *zeros
039300030305     C                   ENDIF
039400030305     C                   IF        VIDPO1K = *blanks
039500030305     C                   EVAL      VIDPO1K = *zeros
039600030305     C                   ENDIF
039700030305     C* SOGGETTO PO SE TIPO SOGGETTO = F
039800030305     C                   IF        VIDTP1  = 'F'
039900030305     C                   IF        VIDSO1F = *zeros AND
040000030305     C                             DUTLPO <> 'S'
040100030305     C                   MOVEL     '1'           ERRL00
040200030305     C                   SETON                                        81
040300030305     C                   MOVEL     MSG(9)        DSMSMS
040400030305     C                   ENDIF
040500030305     C                   ENDIF
040600150219     C* dettaglio solo se scelto un soggetto cliente
040700150219     C                   IF        Vdet = 'S' and (
040800150219     C                             VIDSO1K =  *blanks or
040900150219     C                             VIDSO1K =  *zeros )
041000150218     C                   MOVEL     '1'           ERRL00
041100150219     C                   MOVEL     MSG(13)       DSMSMS
041200150218     C                   ENDIF
041300030305     C* O SOGGETTO CLIENTE O PO CLIENTI SE TIPO SOGGETTO = C
041400030305     C                   IF        VIDTP1  = 'C'
041500030305     C                   IF        DUTLPO <> 'S'
041600030305     C                   IF        (VIDSO1K <> *zeros AND VIDPO1K <> *zeros) OR
041700150410     C                             (VIDSO1K =  *zeros AND VIDPO1K =  *zeros and
041800150410     C                              VIDarea =  *zeros)
041900030305     C                   MOVEL     '1'           ERRL00
042000030305     C                   SETON                                        8185
042100030305     C                   MOVEL     MSG(8)        DSMSMS
042200030305     C                   ENDIF
042300030305     C                   ENDIF
042400030305     C                   ENDIF
042500030514     C* TIPO SOGGETTO DEVE ESSERE VALORIZZATO
042600030514     C                   IF        VIDTP1 = *blanks
042700030514     C                   MOVEL     '1'           ERRL00
042800030514     C                   SETON                                        78
042900030514     C                   MOVEL     MSG(6)        DSMSMS
043000030514     C                   ENDIF
043100030305     C*--------------------
043200030305     C* CONTROLLO AUTORIZZAZIONI UTENTE
043300030305     C*--------------------
043400030305     C     ERRL00        IFEQ      '0'
043500030305     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
043600030305     C                   IF        VIDTP1 = 'F'
043700030305     C                   MOVEL     VIDSO1F       WN3               3
043800030305     C                   SETOFF                                           28
043900150211     C     WN3           LOOKUP    $pog                                   28
044000030305     C     *IN28         IFEQ      *OFF
044100150220     C                   MOVEL     '1'           ERRL00
044200030305     C                   SETON                                        81
044300030305     C                   MOVEL     MSG(7)        DSMSMS
044400030305     C                   ENDIF
044500030305     C                   ENDIF
044600150220     c*
044700030305     C                   IF        VIDTP1 = 'C'
044800030305     C                   MOVEL     VIDSO1K       WA4               4
044900030305     C                   MOVE      WA4           WN3               3
045000030305     C                   SETOFF                                           28
045100150211     C     WN3           LOOKUP    $pog                                   28
045200030305     C     *IN28         IFEQ      *OFF
045300150220     C                   MOVEL     '1'           ERRL00
045400030305     C                   SETON                                        81
045500030305     C                   MOVEL     MSG(7)        DSMSMS
045600030305     C                   ENDIF
045700150220     C*
045800150220     C                   MOVE      vidpo1k       WN3               3
045900150220     C                   SETOFF                                           28
046000150220     C     WN3           LOOKUP    $pog                                   28
046100150220     C     *IN28         IFEQ      *OFF
046200150220     C                   MOVEL     '1'           ERRL00
046300150223     C                   SETON                                        85
046400150220     C                   MOVEL     MSG(7)        DSMSMS
046500150220     C                   ENDIF
046600030305     C                   ENDIF
046700150223     C                   ENDIF
046800150408     C* controllo periodo di date
046900150408     C                   clear                   DA1dal
047000150408     C                   clear                   DA1al
047100150408     C     ERRL00        IFEQ      '0'
047200150408     C                   IF        VIDDAL <> *zeros
047300150408     C     *LIKE         DEFINE    VIDDAL        DA1dal
047400150408     C                   Z-ADD     VIDDAL        WDA8
047500150408     C                   Z-ADD     *ZEROS        DA8W
047600150408     C                   EXSR      CHKDA8
047700150408     C                   Z-ADD     WDA8          VIDDAl
047800150408     C                   Z-ADD     DA8W          DA1dal
047900150408     C     ERRL00        IFEQ      '1'
048000150408     C                   SETON                                        77
048100150408     C                   MOVEL     MSG(3)        DSMSMS
048200150408     C                   ENDIF
048300150408     C                   ENDIF
048400150408     C                   ENDIF
048500150408     C     ERRL00        IFEQ      '0'
048600150408     C                   IF        VIDAL <> *zeros
048700150408     C     *LIKE         DEFINE    VIDAL         DA1al
048800150408     C                   Z-ADD     VIDAL         WDA8
048900150408     C                   Z-ADD     *ZEROS        DA8W
049000150408     C                   EXSR      CHKDA8
049100150408     C                   Z-ADD     WDA8          VIDAl
049200150408     C                   Z-ADD     DA8W          DA1al
049300150408     C     ERRL00        IFEQ      '1'
049400150408     C                   SETON                                        76
049500150408     C                   MOVEL     MSG(3)        DSMSMS
049600150408     C                   ENDIF
049700150408     C                   ENDIF
049800150408     C                   ENDIF
049900150408     C     ERRL00        IFEQ      '0'
050000150408     C                   IF        da1aL < da1dal
050100150408     C                   MOVEL     '1'           ERRL00
050200150408     C                   SETON                                        7677
050300150408     C                   MOVEL     MSG(2)        DSMSMS
050400150408     C                   ENDIF
050500150408     C                   ENDIF
050600970528     C                   ENDSR
050700021220     C*------------------------------------------------------------------------*
050800021220     C* REPKSC - CONTROLLA CODICE CLIENTE
050900021220     C*------------------------------------------------------------------------*
051000021220     C     REPKSC        BEGSR
051100021220     C*
051200021220     C* RICHIESTO ELENCO
051300021220     C                   SETOFF                                           28
051400021220     C     '?'           SCAN      Chk_SOG                                28
051500021220     C     *IN28         IFEQ      *ON
051600150216     C                   CLEAR                   PARAM10
051700150216     c                   movel     'P01'         pop1
051800150216     C                   MOVEL(P)  PARAM10       KPJBU
051900150216     c                   call      'TISIG10R'
052000150216     c                   parm                    kpjba
052100150216     C                   MOVEL     KPJBU         PARAM10
052200150216     C                   MOVE      PKSU          CHK_SOG
052300150216     C                   END
052400021220     C*
052500021220     C* SOLO CARATTERI NUMERICI
052600021220     C                   SETOFF                                       28
052700021220     C                   TESTN                   Chk_SOG              28
052800021220     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
052900021220     C                   MOVEL     '1'           ERRL00
053000021220     C                   ELSE                                                   *E'NUMERO DA TESTN
053100021220     C                   MOVE      Chk_SOG       W001              1
053200021220     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
053300021220     C                   MOVEL     '1'           ERRL00
053400021220     C                   ELSE                                                   *NUMERO
053500021220     C*
053600021220     C* CONTROLLA ESISTENZA CLIENTE
053700021220     C                   EXSR      DECTIB
053800021220     C                   ENDIF
053900021220     C                   ENDIF
054000021220     C*
054100021220     C                   ENDSR
054200021220     C*------------------------------------------------------------------------*
054300021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
054400021220     C*------------------------------------------------------------------------*
054500021220     C     DECTIB        BEGSR
054600150216     C* CONTROLLO CHE SIA NELL'ANAGRAFICO DEI BANCALI
054700150216     C     CHK_SOG       CHAIN     AZFGB01L
054800150216     C                   IF        %FOUND(AZFGB01L) AND FGBANN = ' '
054900021220     C                   CLEAR                   BS69DS
055000021220     C                   CLEAR                   ACODS
055100021220     C                   MOVEL     KNSIF         I69SIF
055200021220     C                   MOVE(P)   Chk_SOG       I69KAC
055300021220     C                   CALL      'TIBS69R'
055400021220     C                   PARM                    BS69DS
055500021220     C                   PARM                    ACODS
055600021220     C                   PARM                    INDDS
055700021220     C                   PARM                    CLPDS
055800021220     C                   PARM                    CLSDS
055900150216     C                   ELSE
056000150216     C                   EVAL      O69ERR = '1'
056100150216     C                   END
056200021220     C     O69ERR        IFNE      '1'
056300021223     C                   EVAL      Chk_DES = acoRAG
056400021220     C                   ELSE
056500021220     C                   MOVEL     '1'           ERRL00
056600021220     C                   ENDIF
056700021220     C*
056800021220     C                   ENDSR
056900021220     C*------------------------------------------------------------------------*
057000021220     C* REPPO - CONTROLLA CODICE P.O.
057100021220     C*------------------------------------------------------------------------*
057200021220     C     REPPO         BEGSR
057300021220     C*
057400021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
057500021220     C*
057600021220     C* RICHIESTO ELENCO
057700021220     C                   SETOFF                                           28
057800021220     C     '?'           SCAN      Chk_SOG                                28
057900021220     C     *IN28         IFEQ      *ON
058000021220     C*
058100021220     C* RICHIESTO ELENCO P.O.
058200021220     C                   CALL      'TNSD24R'
058300021220     C                   PARM                    §COD              3
058400021220     C                   PARM      'F'           §TIP              1
058500021220     C                   PARM                    §§DES            25
058600021220     C                   PARM                    §§PRA             3
058700021220     C                   MOVEL     *all'0'       Chk_SOG
058800021220     C                   MOVE      §COD          Chk_SOG
058900021220     C                   ELSE
059000021220     C                   MOVE      Chk_SOG       WrkPO_N
059100021220     C     WrkPO_N       CHAIN     azorg01l
059200021220     C                   IF        %found(azorg01l)
059300021220     C                   EVAL      Chk_DES = orgDES
059400021220     C                   ELSE
059500021220     C                   EVAL      ERRL00 = '1'
059600021220     C                   EVAL      Chk_DES = *all'*'
059700021220     C                   ENDIF
059800021220     C                   ENDIF
059900021220     C*
060000021220     C                   ENDSR
060100150409     C*------------------------------------------------------------------------*
060200150409     C* REParea CONTROLLA CODICE P.O.
060300150409     C*------------------------------------------------------------------------*
060400150409     C     REParea       BEGSR
060500150409     C*
060600150409     C*
060700150409     C* RICHIESTO ELENCO
060800150409     C                   SETOFF                                           28
060900150409     C     '?'           SCAN      vidarea                                28
061000150409     C     *IN28         IFEQ      *ON
061100150409     c                   Z-Add     1             §tKut
061200150409     C                   MOVEL     '05'          §TCOD
061300150409     C                   MOVEL     *BLANKS       §TKEY
061400150409     C                   MOVEL     *BLANKS       §TDES
061500150409     C                   CALL      'X§TABER'
061600150409     C                   PARM                    §TKUT             1 0
061700150409     C                   PARM                    §TCOD             2
061800150409     C                   PARM                    §TKEY             8
061900150409     C                   PARM                    §TDES            30
062000150409     C                   MOVEL     §TKEY         vidarea
062100150409     C                   MOVEL     §TDES         vidaread
062200150409     c                   else
062300150409     C                   MOVEL     '05'          COD
062400150409     C                   MOVEL     vidarea       tblkey
062500150409     C     KTAB          CHAIN     TABEL00f
062600150409     C                   IF        %found(tabel00f)
062700150409     C                   MOVEL     tbluni        ds05
062800150409     C                   movel     §05des        vidaread
062900150409     C                   Else
063000150409     C                   EVAL      ERRL00 = '1'
063100150409     c                   endif
063200150409     c                   endif
063300150409     C*
063400150409     C                   ENDSR
063500021220     C*------------------------------------------------------------------------*
063600021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
063700021220     C*------------------------------------------------------------------------*
063800021220     C     REPCOD        BEGSR
063900021220     C*
064000021220     C* RICHIESTO ELENCO
064100021220     C                   SETOFF                                           28
064200021220     C     '?'           SCAN      Chk_COD                                28
064300021220     C     *IN28         IFEQ      *ON
064400021220     C*
064500021220     C* RICHIESTO ELENCO ARTICOLI
064600021220     C                   MOVEL     KPJBU         SAVKPJBU
064700021220     C                   MOVEL(P)  'P01'         KPJBU
064800021220     C                   CALL      'TISIG1R'
064900021220     C                   PARM                    KPJBA
065000021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
065100021220     C                   MOVEL     SAVKPJBU      KPJBU
065200021220     C                   ELSE
065300021220     C     Chk_COD       CHAIN     azagb01l
065400021220     C                   IF        %found(azagb01l)
065500021220     C                   EVAL      Chk_DES = agbDES
065600021220     C                   ELSE
065700021220     C                   EVAL      ERRL00 = '1'
065800021220     C                   EVAL      Chk_DES = *all'*'
065900021220     C                   ENDIF
066000021220     C                   ENDIF
066100021220     C*
066200021220     C                   ENDSR
066300021220     C*------------------------------------------------------------------------*
066400021220     C* CHKDA8 - CONTROLLA LA DATA (8)
066500021220     C*------------------------------------------------------------------------*
066600021220     C     CHKDA8        BEGSR
066700021220     C*
066800030123     C     *LIKE         DEFINE    VIDDA1        WDA8
066900030123     C     *LIKE         DEFINE    VIDDA1        DA8W
067000021220     C*
067100021220     C                   Z-ADD     WDA8          G08DAT
067200021220     C                   Z-ADD     *ZEROS        G08INV
067300021220     C                   MOVEL     '0'           G08ERR
067400021220     C                   CALL      'XSRDA8'
067500021220     C                   PARM                    WLBDA8
067600021220     C                   Z-ADD     G08DAT        WDA8
067700021220     C                   Z-ADD     G08INV        DA8W
067800021220     C     G08ERR        IFEQ      '1'
067900021220     C                   MOVEL     '1'           ERRL00
068000021220     C                   ENDIF
068100021220     C*
068200021220     C                   ENDSR
068300021219     C*--------------------------------------------------------------------------------------------*
068400021219     C* REPERISCE I DATI UTENTE
068500021219     C*--------------------------------------------------------------------------------------------*
068600021219     C     REPDATIUTE    BEGSR
068700021219     C*
068800021219     C* INIZIALIZZA VARIABILI DI WRK
068900021219     C                   CLEAR                   TIBS34DS
069000021219     C                   CLEAR                   AZUTEDS
069100150211     C                   CLEAR                   dute01
069200021219     C                   CLEAR                   DDATIUTE
069300021219     C*
069400021219     C     *DTAARA       DEFINE    §azute        azuteds
069500021219     C     *DTAARA       DEFINE    §datiute      ddatiute
069600021219     C                   IN(E)     *DTAARA
069700021219     C                   IF        %Error
069800021220     C                   EVAL      I34Tla = 'L'
069900021219     C                   CALL      'TIBS34R'
070000021219     C                   PARM                    Tibs34Ds
070100021219     C                   IN        *DTAARA
070200021219     C                   ENDIF
070300150211
070400150211     c                   Clear                   wabi
070500150211     c                   Clear                   dLat
070600150211
070700150211      * Verifica errori e autorità profilo
070800150211s   1c                   Select
070900150211      * se ho errori nei dati utente esco dal pgm
071000150211w   1c                   When      DutErr = 'E'
071100150211     c                   seton                                        lr
071200150211     c                   return
071300150211      * se non c'è l'abilitazione
071400150211      * --> se 1° livello, abilitazioni al terminal
071500150211      *     se 2° livello, abilitazioni al punto operativo
071600150211w   1c                   When      UteAut = *Blanks
071700150211if  2c                   If        DutLpo = '1'
071800150211     c                   Eval      wabi   = 'TP'
071900150211e   2c                   EndIf
072000150211if  2c                   If        DutLpo = '2'
072100150211     c                   Eval      wabi   = 'PO'
072200150211e   2c                   EndIf
072300150211if  2c                   If        DutLpo = 'S'
072400150211     c                   Eval      wabi   = 'AZ'
072500150211e   2c                   EndIf
072600150211      * carica le abilitazioni del profilo
072700150211x   1c                   Other
072800150211     c                   Movel     UteFaf        Dute01
072900150211     c                   If        §Utemgb <> *Blanks
073000150211     c                   Eval      wabi = §Utemgb
073100150211     c                   Else
073200150211     c                   Eval      wabi = UteAut
073300150211     c                   EndIf
073400150211e   1c                   EndSl
073500150211      * controllo se ok l'abilitazione dell'utente
073600150211     c                   Clear                   Tibs02ds
073700150211     c                   Eval      T02Mod = 'C'
073800150211     c                   Eval      T02Sif = knsif
073900150211     c                   Eval      T02Cod = 'LAT'
074000150211     c                   Movel(p)  wabi          T02Ke1
074100150211     c                   Call      'TIBS02R'
074200150211     c                   Parm                    kpjba
074300150211     c                   Parm                    Tibs02ds
074400150211if  1c                   If        T02Err = *Blanks
074500150211     c                   Eval      dLat = T02Uni
074600150211e   1c                   EndIf
074700150211      * errore o non abilitato (impossibile xchè controllato nel filtro)
074800150211if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
074900150211     c                   seton                                        lr
075000150211     c                   return
075100150211   x1c                   Else
075200150211      * Reperimento dei P.O. gestibili dall'utente
075300150211     c                   clear                   TRUL31DS
075400150211     c                   clear                   TRUL31DS2
075500150211     c                   eval      I31abi = wabi
075600150211     c                   eval      I31cdi = DUTdis
075700150211     c                   eval      I31car = DUTare
075800150211     c                   eval      I31cpo = DUTpou
075900150211     c                   call      'TRUL31R'
076000150211     c                   parm                    KPJBA
076100150211     c                   parm                    TRUL31DS
076200150211     c                   parm                    TRUL31DS2
076300150211if  2c                   if        O31pog > *zeros
076400150211     c                   movea     O31pog        $pog
076500150211     c                   movea     O31arg        $arg
076600150211     c                   movea     O31dig        $dig
076700150211x   2c                   else
076800150211     c* impossibile perchè controllato nel filtrp
076900150211     c                   seton                                        lr
077000150211     c                   return
077100150211e   2c                   endif
077200150211    1c                   endif
077300150408     c                   if        dutpou = 046
077400150408     c                   seton                                        66
077500150408     c                   endif
077600021219     C*
077700021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
077800021219     C                   MOVEL(P)  RSUT          DSFIRS           20
077900021219     C*
078000021219     C                   ENDSR
078100030123     C*------------------------------------------------------------------------*
078200030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
078300030123     C*------------------------------------------------------------------------*
078400030123     C     VIDPAR        BEGSR
078500030123     C*
078600030123     C                   Z-ADD     DA1VID        PARDA1
078700030123     C                   MOVEL     VIDTP1        PARTP1
078800030305     C                   IF        VIDTP1 = 'C'
078900030305     C                   MOVEL     VIDSO1K       PARSO1
079000030305     C                   MOVEL     VIDPO1K       PARPO1
079100030305     C                   ENDIF
079200030305     C                   IF        VIDTP1 = 'F'
079300030612     C                   MOVEL     *all'0'       PARSO1
079400030612     C                   MOVE      VIDSO1F       PARSO1
079500030305     C                   ENDIF
079600030123     C                   MOVEL     VIDCO1        PARCO1
079700150211     C                   z-add     VIDper        PARPer
079800150211     C                   MOVEL     VIDgia        PARgia
079900150218     C                   MOVEL     Vdet          PARdet
080000150402     C                   MOVE      Vfile         PARfile
080100150402     C                   MOVE      Vpulf         PARpulf
080200150408     C                   Z-ADD     DA1DAl        PARDAL
080300150408     C                   Z-ADD     DA1AL         PARAL
080400150409     C                   MOVEL     Vidarea       PARarea
080500150409     C                   MOVEL     Vidtsg        PARtsg
080600030123     C*
080700030123     C                   ENDSR
080800030123     C*------------------------------------------------------------------------*
080900030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
081000030123     C*------------------------------------------------------------------------*
081100030123     C     AZZVID        BEGSR
081200030123     C*
081300030124     C                   Z-ADD     WN8           VIDDA1
081400150408     C                   Z-ADD     0             VIDDAl
081500150408     C                   Z-ADD     0             VIDal
081600150211     C                   MOVEL     'C'           VIDTP1
081700030123     C                   MOVEL     *blanks       VIDSO1K
081800030123     C                   MOVEL     *blanks       VIDSO1F
081900030123     C                   MOVEL     *blanks       VIDCO1
082000150211     C                   MOVEL     ' '           VIDgia
082100150211     C                   Z-ADD     *zeros        VIDper
082200150408     c                   if        *in66
082300150402     C                   MOVE      'S'           Vfile
082400150402     C                   MOVE      'S'           Vpulf
082500150408     c                   else
082600150408     C                   MOVE      'N'           Vfile
082700150408     C                   MOVE      'N'           Vpulf
082800150408     c                   endif
082900150211     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
083000150211     C                   SETOn                                        55
083100150211     C                   SETOn                                        56
083200030123     C*
083300030123     C                   ENDSR
083400030123     C*------------------------------------------------------------------------*
083500030123     C* *INZSR - OPERAZIONI INIZIALI
083600030123     C*------------------------------------------------------------------------*
083700030123     C     *INZSR        BEGSR
083800030123     C*
083900030123     C* RICEVIMENTO PARAMETRI
084000030123     C     *ENTRY        PLIST
084100030123     C                   PARM                    KPJBA
084200030618     C*---
084300030618     C* CHIAVI DI LETTURA
084400030618     C*---
084500030618     C* LETTURA tntbe01l - PARZIALE
084600030618     C     KEYTBEP       KLIST
084700030618     C                   KFLD                    tbeCOD
084800030618     C                   KFLD                    tbeKE1
084900030123     C*
085000150409     C     KTAB          KLIST
085100150409     C                   KFLD                    CODUT             1 0
085200150409     C                   KFLD                    COD               2
085300150409     C                   KFLD                    tblkey
085400150409     C                   Z-ADD     1             CODUT
085500150409      * tabelle
085600150211     C                   EXSR      REPDATIUTE
085700030123     C*
085800030123     C* REPERISCO DATA E ORA CORRENTI
085900030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
086000030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
086100030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
086200030123     C                   Z-ADD     WN8           G08DAT
086300030123     C                   Z-ADD     *ZEROS        G08INV
086400030123     C                   MOVEL     '0'           G08ERR
086500030123     C                   CALL      'XSRDA8'
086600030123     C                   PARM                    WLBDA8
086700030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
086800030123     C*
086900030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
087000030123     C     XMSGPA        PLIST
087100030123     C                   PARM                    DSMSMS
087200030123     C                   PARM                    DSMSPG
087300030123     C                   PARM                    DSMSKE
087400030123     C                   PARM                    DSMSER
087500030123     C*
087600030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
087700030123     C                   CALL      'XKEY'
087800030123     C                   PARM                    DSKEY
087900030123     C*
088000030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
088100030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088200030123     C*
088300030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
088400030123     C*   -NOME PGM CHIAMANTE
088500030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
088600150211     C                   MOVEL     'TISIG14R'    DSMSPG
088700150211     C                   MOVEL     'TISIG14R'    MSGQUE
088800030123     C                   MOVEL     *BLANKS       MSGKEY
088900030123     C                   MOVEL     *BLANKS       DSMSMS
089000030123     C*
089100030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
089200030123     C                   Z-ADD     0             FUNGRU            5 0
089300030123     C*
089400030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
089500030123     C                   EXSR      FUNGES
089600030123     C*
089700030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
089800030123     C                   MOVEL     '0'           F12L00            1
089900030123     C                   MOVEL     '0'           F03L00            1
090000030123     C                   MOVEL     '0'           FINL00            1
090100030123     C                   MOVEL     '0'           ERRL00            1
090200030123     C                   MOVEL     '0'           FLFINE            1
090300030123     C*
090400030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
090500030123     C                   EXSR      AZZVID
090600030123     C*
090700030123     C                   ENDSR
090800030123     C*------------------------------------------------------------------------*
090900030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
091000030123     C*------------------------------------------------------------------------*
091100030123     C     FUNGES        BEGSR
091200030123     C*
091300030123     C* PULISCO CAMPI DI LAVORO
091400030123     C                   Z-ADD     0             W                 5 0
091500030123     C*
091600030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
091700030123     C* E CARICO TABELLA FUNZIONI ATTIVE
091800030123     C     1             DO        24            I
091900030123     C                   MOVEL     FUD(I)        FUDRIG
092000030123     C*
092100030123     C* PULIZIA FUNZIONI PRECEDENTI
092200030123     C                   MOVEL     *BLANKS       FUV(I)
092300030123     C*
092400030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
092500030123     C* (SALTA F24)
092600030123     C     FUDATT        IFEQ      'S'
092700030123     C     FUDFUN        ANDNE     '24'
092800030123     C                   ADD       1             W
092900030123     C                   MOVEL     FUDDES        FUV(W)
093000030123     C                   END
093100030123     C                   END                                                    DO WHILE
093200030123     C*
093300030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
093400030123     C* ALTRIMENTI DISATTIVO F24
093500030123     C     1             DO        24            I
093600030123     C                   MOVEL     FUD(I)        FUDRIG
093700030123     C*
093800030123     C     FUDFUN        IFEQ      '24'
093900030123     C     W             IFGT      6
094000030123     C                   MOVEL     'S'           FUDATT
094100030123     C                   ELSE
094200030123     C                   MOVEL     *BLANKS       FUDATT
094300030123     C                   END                                                    W GT 8
094400030123     C                   MOVEL     FUDRIG        FUD(I)
094500030123     C                   END
094600030123     C                   END                                                    DO WHILE
094700030123     C*
094800030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
094900030123     C     W             DIV       6             I
095000030123     C                   MVR                     Z
095100030123     C*
095200030123     C     Z             IFNE      0
095300030123     C     I             ADD       1             Z
095400030123     C                   ELSE
095500030123     C                   Z-ADD     I             Z                 5 0
095600030123     C                   END
095700030123     C*
095800030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
095900030123     C                   ADD       1             FUNGRU
096000030123     C     FUNGRU        IFGT      Z
096100030123     C     FUNGRU        ORLT      1
096200030123     C                   Z-ADD     1             FUNGRU
096300030123     C                   END
096400030123     C*
096500030123     C* PRIMO GRUPPO DI 6 FUNZIONI
096600030123     C     FUNGRU        IFEQ      1
096700030123     C                   MOVEL     FUV(1)        FUN001
096800030123     C                   MOVEL     FUV(2)        FUN002
096900030123     C                   MOVEL     FUV(3)        FUN003
097000030123     C                   MOVEL     FUV(4)        FUN004
097100030123     C                   MOVEL     FUV(5)        FUN005
097200030123     C                   MOVEL     FUV(6)        FUN006
097300030123     C                   END
097400030123     C*
097500030123     C* SECONDO GRUPPO DI 6 FUNZIONI
097600030123     C     FUNGRU        IFEQ      2
097700030123     C                   MOVEL     FUV(7)        FUN001
097800030123     C                   MOVEL     FUV(8)        FUN002
097900030123     C                   MOVEL     FUV(9)        FUN003
098000030123     C                   MOVEL     FUV(10)       FUN004
098100030123     C                   MOVEL     FUV(11)       FUN005
098200030123     C                   MOVEL     FUV(12)       FUN006
098300030123     C                   END
098400030123     C*
098500030123     C* TERZO GRUPPO DI 6 FUNZIONI
098600030123     C     FUNGRU        IFEQ      3
098700030123     C                   MOVEL     FUV(13)       FUN001
098800030123     C                   MOVEL     FUV(14)       FUN002
098900030123     C                   MOVEL     FUV(15)       FUN003
099000030123     C                   MOVEL     FUV(16)       FUN004
099100030123     C                   MOVEL     FUV(17)       FUN005
099200030123     C                   MOVEL     FUV(18)       FUN006
099300030123     C                   END
099400030123     C*
099500030123     C* QUARTO GRUPPO DI 6 FUNZIONI
099600030123     C     FUNGRU        IFEQ      4
099700030123     C                   MOVEL     FUV(19)       FUN001
099800030123     C                   MOVEL     FUV(20)       FUN002
099900030123     C                   MOVEL     FUV(21)       FUN003
100000030123     C                   MOVEL     FUV(22)       FUN004
100100030123     C                   MOVEL     *BLANKS       FUN005
100200030123     C                   MOVEL     *BLANKS       FUN006
100300030123     C                   END
100400030123     C*
100500030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
100600030123     C                   MOVEL     *BLANKS       FUN007
100700030123     C                   MOVEL     *BLANKS       FUN008
100800030123     C*
100900030123     C     1             DO        24            I
101000030123     C                   MOVEL     FUD(I)        FUDRIG
101100030123     C     FUDFUN        IFEQ      '24'
101200030123     C     FUDATT        ANDEQ     'S'
101300030123     C                   MOVEL     FUDDES        FUN008
101400030123     C                   END
101500030123     C                   END                                                    DO
101600030123     C                   ENDSR
101700030123     C*------------------------------------------------------------------------*
101800030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
101900030123     C*------------------------------------------------------------------------*
102000030123     C     FUNCON        BEGSR
102100030123     C*
102200030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
102300030123     C                   EXSR      FUNVAR
102400030123     C*
102500030123     C* PULISCO CAMPI DI LAVORO
102600030123     C                   Z-ADD     0             I                 5 0
102700030123     C                   Z-ADD     0             W                 5 0
102800030123     C*
102900030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
103000030123     C     FUNKEY        IFNE      *BLANKS
103100030123     C     1             DO        24            I
103200030123     C                   MOVEL     FUD(I)        FUDRIG
103300030123     C*
103400030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
103500030123     C     FUNKEY        IFEQ      FUDFUN
103600030123     C     FUDATT        ANDEQ     'S'
103700030123     C                   Z-ADD     I             W
103800030123     C                   END
103900030123     C                   END                                                    DO WHILE
104000030123     C*
104100030123     C* FUNZIONE NON ATTIVA (ERRORE)
104200030123     C     W             IFEQ      0
104300030123     C*
104400030123     C* EMISSIONE MESSAGGIO DI ERRORE
104500030123     C                   MOVEL     MSG(1)        DSMSMS
104600030123     C*
104700030123     C                   END                                                    W=0
104800030123     C                   END                                                    FUNKEY NE *BLANKS
104900030123     C*
105000030123     C* FUNZIONE ATTIVA (PROCEDI ...)
105100030123     C     W             IFNE      0
105200030123     C     FUNKEY        OREQ      *BLANKS
105300030123     C*
105400030123     C     FUNKEY        IFEQ      *BLANKS
105500030123     C                   EXSR      ENTGES
105600030123     C                   END
105700030123     C     FUNKEY        IFEQ      '03'
105800030123     C                   EXSR      F03GES
105900030123     C                   END
106000030123     C     FUNKEY        IFEQ      '06'
106100030123     C                   EXSR      F06GES
106200030123     C                   END
106300030123     C     FUNKEY        IFEQ      '12'
106400030123     C                   EXSR      F12GES
106500030123     C                   END
106600030123     C     FUNKEY        IFEQ      '24'
106700030123     C                   EXSR      F24GES
106800030123     C                   END
106900030123     C*
107000030123     C                   END
107100030123     C*
107200030123     C                   ENDSR
107300030123     C*------------------------------------------------------------------------*
107400030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
107500030123     C*------------------------------------------------------------------------*
107600030123     C     FUNVAR        BEGSR
107700030123     C*
107800030123     C* INIZIALIZZA VARIABILE -FUNKEY-
107900030123     C*
108000030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
108100030123     C     KEY           IFEQ      ENTER
108200030123     C                   MOVEL     *BLANKS       FUNKEY
108300030123     C                   END
108400030123     C     KEY           IFEQ      F1
108500030123     C                   MOVEL     '01'          FUNKEY
108600030123     C                   END
108700030123     C     KEY           IFEQ      F2
108800030123     C                   MOVEL     '02'          FUNKEY
108900030123     C                   END
109000030123     C     KEY           IFEQ      F3
109100030123     C                   MOVEL     '03'          FUNKEY
109200030123     C                   END
109300030123     C     KEY           IFEQ      F4
109400030123     C                   MOVEL     '04'          FUNKEY
109500030123     C                   END
109600030123     C     KEY           IFEQ      F5
109700030123     C                   MOVEL     '05'          FUNKEY
109800030123     C                   END
109900030123     C     KEY           IFEQ      F6
110000030123     C                   MOVEL     '06'          FUNKEY
110100030123     C                   END
110200030123     C     KEY           IFEQ      F7
110300030123     C                   MOVEL     '07'          FUNKEY
110400030123     C                   END
110500030123     C     KEY           IFEQ      F8
110600030123     C                   MOVEL     '08'          FUNKEY
110700030123     C                   END
110800030123     C     KEY           IFEQ      F9
110900030123     C                   MOVEL     '09'          FUNKEY
111000030123     C                   END
111100030123     C     KEY           IFEQ      F10
111200030123     C                   MOVEL     '10'          FUNKEY
111300030123     C                   END
111400030123     C     KEY           IFEQ      F11
111500030123     C                   MOVEL     '11'          FUNKEY
111600030123     C                   END
111700030123     C     KEY           IFEQ      F12
111800030123     C                   MOVEL     '12'          FUNKEY
111900030123     C                   END
112000030123     C     KEY           IFEQ      F13
112100030123     C                   MOVEL     '13'          FUNKEY
112200030123     C                   END
112300030123     C     KEY           IFEQ      F14
112400030123     C                   MOVEL     '14'          FUNKEY
112500030123     C                   END
112600030123     C     KEY           IFEQ      F15
112700030123     C                   MOVEL     '15'          FUNKEY
112800030123     C                   END
112900030123     C     KEY           IFEQ      F16
113000030123     C                   MOVEL     '16'          FUNKEY
113100030123     C                   END
113200030123     C     KEY           IFEQ      F17
113300030123     C                   MOVEL     '17'          FUNKEY
113400030123     C                   END
113500030123     C     KEY           IFEQ      F18
113600030123     C                   MOVEL     '18'          FUNKEY
113700030123     C                   END
113800030123     C     KEY           IFEQ      F19
113900030123     C                   MOVEL     '19'          FUNKEY
114000030123     C                   END
114100030123     C     KEY           IFEQ      F20
114200030123     C                   MOVEL     '20'          FUNKEY
114300030123     C                   END
114400030123     C     KEY           IFEQ      F21
114500030123     C                   MOVEL     '21'          FUNKEY
114600030123     C                   END
114700030123     C     KEY           IFEQ      F22
114800030123     C                   MOVEL     '22'          FUNKEY
114900030123     C                   END
115000030123     C     KEY           IFEQ      F23
115100030123     C                   MOVEL     '23'          FUNKEY
115200030123     C                   END
115300030123     C     KEY           IFEQ      F24
115400030123     C                   MOVEL     '24'          FUNKEY
115500030123     C                   END
115600030123     C     KEY           IFEQ      ROLLUP
115700030123     C                   MOVEL     'RU'          FUNKEY
115800030123     C                   END
115900030123     C     KEY           IFEQ      HELP
116000030123     C                   MOVEL     'HP'          FUNKEY
116100030123     C                   END
116200030123     C                   ENDSR
116300030123     C*------------------------------------------------------------------------*
116400030123     C* NOTE PER COMPILAZIONE SCHIERE
116500030123     C*------------------------------------------------------------------------*
116600030123     C* -MSG-
116700030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
116800030123     C* (NON TOCCARE MSG,1)
116900030123     C*
117000030123     C* -CMD-
117100030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
117200030123     C*
117300030123     C* -FUD-
117400030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
117500030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
117600030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
117700030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
117800030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
117900030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
118000030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
118100030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
118200030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
118300030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
118400030123     C*
118500030123     C*------------------------------------------------------------------------*
118600030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
118700150211Tasto funzionale non gestito.                                                  1
118800150408Le date del periodo richiesto non sono inserite correttamente                  2
118900150211Data errata.                                                                   3
119000150211Soggetto movimento errato.                                                     4
119100150211Articolo indicato errato.                                                      5
119200150211Campo obbligatorio.                                                            6
119300150220Soggetto indicato non in gestione all'utente.                                  7
119400150211Indicare o il soggetto cliente o il Fil. dei soggetti clienti.                 8
119500150211Indicare il soggetto Fil.                                                      9
119600150219Se richiesto solo clienti eccedenti la giacenza, non è ammesso il dettaglio    10
119700150219Se richiesta % differenza saldo, non è ammesso il dettaglio                    11
119800150211Effettuare almeno una scelta.                                                  12
119900150219Il dettaglio è ammesso solo se richiesto un soggetto cliente                   13
120000150409Codice area selezionato non valido                                             14
120100150409                                                                               15
120200150409                                                                               16
120300150409                                                                               17
120400150409                                                                               18
120500150409                                                                               19
120600150409                                                                               20
120700030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
120800030123                   -        (S/N)=N                        funz.01¬
120900030123                   -        (S/N)=N                        funz.02¬
121000030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
121100030123                   -        (S/N)=N                        funz.04¬
121200030123                   -        (S/N)=N                        funz.05¬
121300030123F06=Lancio         -        (S/N)=S                        funz.06¬
121400030123                   -        (S/N)=N                        funz.07¬
121500030123                   -        (S/N)=N                        funz.08¬
121600030123                   -        (S/N)=N                        funz.09¬
121700030123                   -        (S/N)=N                        funz.10¬
121800030123                   -        (S/N)=N                        funz.11¬
121900150211                   -        (S/N)=N                        funz.12¬
122000030123                   -        (S/N)=N                        funz.13¬
122100030123                   -        (S/N)=N                        funz.14¬
122200030123                   -        (S/N)=N                        funz.15¬
122300030123                   -        (S/N)=N                        funz.16¬
122400030123                   -        (S/N)=N                        funz.17¬
122500030123                   -        (S/N)=N                        funz.18¬
122600030123                   -        (S/N)=N                        funz.19¬
122700030123                   -        (S/N)=N                        funz.20¬
122800030123                   -        (S/N)=N                        funz.21¬
122900030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
123000030123                   -        (S/N)=N                        funz.23¬
123100030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
