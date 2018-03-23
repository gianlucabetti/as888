000100970526     H*------------------------------------------------------------------------*
000200150211     H* Statistica movimenti GABBIE e BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150414     FTISIG16d  CF   E             WORKSTN INFDS(DEVDS1)
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
010000030123     D  PARTP1                        1
010100030123     D  PARSO1                        8
010200030123     D  PARCO1                       15
010300030305     D  PARPO1                        3
010400150402     D  PARfile                       1
010500150402     D  PARpulf                       1
010600150408     D  PARDAl                        8s 0
010700150408     D  PARal                         8s 0
010800150409     D  PARarea                       3
010900150409     D  PARtsg                        1
011000150216     D*------------------
011100150216     D* PASSAGGIO PARAMETRI TISIG10
011200150216     D*------------------
011300150216     D PARAM10         DS
011400150216     d  pop1                          3
011500150216     D  Pksu                          8
011600150216     D  Ptsc                          1
011700150216     D  Pmsg                         80
011800030123     D*------------------
011900030123     D* VARIABILI DI WRK
012000030123     D*------------------
012100030123     D Chk_SOG                        8A
012200030123     D Chk_COD                       15A
012300030123     D Chk_DES                       30A
012400030123     D SAVKPJBU                            LIKE(KPJBU)
012500070509
012600070509      * flag per forzare messaggio errore sul cliente
012700070509     d wforza          s              1    inz(*off)
012800070510      * campi di sav
012900070510     d savso1f         s                   like(vidso1f) inz
013000070510     d savso1k         s                   like(vidso1k) inz
013100070509
013200970526     C*------------------------------------------------------------------------*
013300970526     C* MAIN LINES
013400970526     C*------------------------------------------------------------------------*
013500970526     C*
013600970526     C* NON CI SONO ERRORI IN ENTRATA
013700021211     C     ERRL00        IFEQ      '0'
013800970526     C*
013900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014000021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014300970526     C*
014400970526     C* EMETTE FORMATO VIDEO
014500970526     C                   EXSR      BUTFMR
014600970526     C*
014700970526     C* GESTIONE TASTI FUNZIONALI
014800970526     C                   EXSR      FUNCON
014900021211     C                   ENDDO
015000970526     C*
015100021211     C                   ENDIF
015200021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
015300021211     C                   MOVEL     PARAM         KPJBU
015400970526     C* FINE PGM
015500970526     C                   SETON                                        LR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015800970526     C*------------------------------------------------------------------------*
015900970526     C     BUTFMR        BEGSR
016000970526     C*
016100970526     C* EMETTE LA TESTATA
016200030123     C                   WRITE     SIG9D01
016300970526     C*
016400970526     C* SPEDISCE EVENTUALE MESSAGGIO
016500970526     C                   EXSR      SNDMSG
016600970526     C*
016700970526     C* EMETTE LE FUNZIONI
016800030123     C                   WRITE     SIG9D02
016900970526     C*
017000970526     C* EMETTE IL VIDEO
017100030123     C                   EXFMT     SIG9D03
017200970526     C*
017300970526     C* REIMPOSTA FLAG DI ERRORE
017400970526     C                   MOVEL     '0'           ERRL00
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017900970526     C*------------------------------------------------------------------------*
018000970526     C     SNDMSG        BEGSR
018100970526     C*
018200970526     C* PULISCE IL SFILE MESSAGGI
018300970526     C                   MOVEL     '1'           *IN40
018400030123     C                   WRITE     SIG9DMC
018500970526     C                   MOVEL     '0'           *IN40
018600970526     C*
018700970526     C* VISUALIZZA IL MESSAGGIO
018800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018900970526     C                   CALL      'XMSG'        XMSGPA
019000021211     C     DSMSER        IFEQ      *ZEROS
019100970526     C                   MOVEL     DSMSKE        MSGKEY
019200021211     C                   ENDIF
019300970526     C                   Z-ADD     1             MSGULT
019400030123     C                   WRITE     SIG9DMS
019500030123     C                   WRITE     SIG9DMC
019600970526     C*
019700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019800970526     C                   MOVEL     *BLANKS       DSMSMS
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200021219     C* ENTGES - GESTIONE ENTER -> INVIO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     ENTGES        BEGSR
020500970526     C*
020600970526     C* CONTROLLA I CAMPI IMMESSI
020700970526     C                   EXSR      DECODI
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F03GES - GESTIONE F03 -> FINE
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F03GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F03"
021600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000030123     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
022100970526     C*------------------------------------------------------------------------*
022200970526     C     F06GES        BEGSR
022300030123     C*
022400030123     C                   EXSR      DECODI
022500030123     C*
022600030123     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
022700030123     C     ERRL00        IFEQ      '0'
022800030123     C*
022900030123     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
023000150414     C                   MOVEL     'SIGD'        KCOAZ
023100030123     C                   EXSR      VIDPAR
023200030123     C                   MOVEL     PARAM         KPJBU
023300150423     C                   CALL      'BCH10'
023400150423 MOM C**                 CALL      'TISIG17R'
023500030123     C                   PARM                    KPJBA
023600030123     C*
023700030123     C* IMPOSTO FLAG F03 DI LIVELLO PGM
023800030123     C                   MOVEL     '1'           F03L00
023900030123     C*
024000030123     C                   ENDIF
024100021211     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* F12GES - GESTIONE F12 -> RITORNO
024500970526     C*------------------------------------------------------------------------*
024600970526     C     F12GES        BEGSR
024700970526     C*
024800970526     C* IMPOSTA "PREMUTO F12"
024900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025400970526     C*------------------------------------------------------------------------*
025500970526     C     F24GES        BEGSR
025600970526     C*
025700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025800970526     C*
025900970526     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026200970526     C*------------------------------------------------------------------------*
026300970526     C     DECODI        BEGSR
026400030305     C*
026500000407     C                   MOVEL     '0'           ERRL00
026600000407     C*
026700000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
026800150408     C                   SETOFF                                       787776
026900030305     C                   SETOFF                                       798182
027000150415     C                   SETOFF                                       857475
027100030305     C*
027200030305     C* INIZIALIZZO I CAMPI DESCRIZIONE
027300030305     C                   EVAL      VIDDS1 = *blanks
027400030305     C                   EVAL      VIDPO1KD = *blanks
027500021219     C*--------------------
027600030123     C* SCELTA/CONTROLLO SOGGETTO
027700021219     C*--------------------
027800021219     C     ERRL00        IFEQ      '0'
027900021223     C* Decodifico il tipo di soggetto movimento
028000030123     C                   IF        VIDTP1 = 'C'
028100030305     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
028200030123     C                   SETON                                        55
028300030305     C                   SETON                                        56
028400021223     C                   ENDIF
028500030123     C                   IF        VIDTP1 = 'F'
028600150415     c                   clear                   vidtsg
028700150415     c                   clear                   vidso1k
028800150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
028900030123     C                   SETOFF                                       55
029000030305     C                   SETOFF                                       56
029100021223     C                   ENDIF
029200021223     C* Eseguo i controlli di validità soggetto movimento
029300030107     C                   IF        *IN55   =  *ON
029400021220     C*
029500030305     C                   IF        VIDTP1 = 'C'
029600030305     C                   IF        VIDSO1K <> *blanks AND
029700030305     C                             VIDSO1K <> *zeros
029800030305     C                   EVAL      Chk_DES =  *blanks
029900030305     C                   MOVEL     *zeros        Chk_SOG
030000030305     C                   MOVE      VIDSO1K       Chk_SOG
030100021220     C                   EXSR      REPKSC
030200030305     C                   IF        ERRL00  = '1'
030300030305     C                   SETON                                        81
030400030305     C                   MOVEL     MSG(4)        DSMSMS
030500030305     C                   ENDIF
030600030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
030700030305     C                   EVAL      VIDDS1  = Chk_DES
030800030305     C                   ENDIF
030900030305     C                   IF        VIDPO1K <> *blanks AND
031000030305     C                             VIDPO1K <> *zeros
031100030305     C                   EVAL      Chk_DES =  *blanks
031200030305     C                   MOVEL     *zeros        Chk_SOG
031300030305     C                   MOVE      VIDPO1K       Chk_SOG
031400030305     C                   EXSR      REPPO
031500030305     C                   IF        ERRL00  = '1'
031600030305     C                   SETON                                        85
031700030305     C                   MOVEL     MSG(4)        DSMSMS
031800030305     C                   ENDIF
031900030305     C                   MOVE(P)   Chk_SOG       VIDPO1K
032000030305     C                   EVAL      VIDPO1KD = Chk_DES
032100021220     C                   ENDIF
032200030305     C                   ENDIF
032300021220     C*
032400030107     C                   ELSE
032500030305     C                   IF        VIDSO1F <> *blanks AND
032600030305     C                             VIDSO1F <> *zeros
032700030123     C                   IF        VIDTP1 = 'F'
032800030305     C                   EVAL      Chk_DES =  *blanks
032900030305     C                   MOVEL     *zeros        Chk_SOG
033000030305     C                   MOVE      VIDSO1F       Chk_SOG
033100030107     C                   EXSR      REPPO
033200030305     C                   MOVE(P)   Chk_SOG       VIDSO1F
033300030305     C                   EVAL      VIDDS1  = Chk_DES
033400030305     C                   IF        ERRL00  = '1'
033500030305     C                   SETON                                        81
033600030305     C                   MOVEL     MSG(4)        DSMSMS
033700030305     C                   ENDIF
033800030107     C                   ENDIF
033900030107     C*
034000030107     C                   ENDIF
034100030107     C                   ENDIF
034200030305     C                   ENDIF
034300150409     C*--------------------
034400150409     C* SCELTA/area
034500150409     C*--------------------
034600150416     c                   clear                   vidaread
034700150409     C     ERRL00        IFEQ      '0'
034800150413     C                   IF        VIDarea <> *blanks and
034900150413     C                             VIDarea <> *zero
035000150409     C                   EXSR      REParea
035100150416     c     vidarea       lookup    $arg                                   28
035200150416     C  n28              MOVE      '1'           ERRL00
035300150409     C                   IF        ERRL00 = '1'
035400150416     C                   SETON                                        75
035500150409     C                   MOVEL     MSG(14)       DSMSMS
035600150409     C                   ENDIF
035700150409     C                   ENDIF
035800150409     C                   ENDIF
035900021219     C*--------------------
036000021219     C* SCELTA/CONTROLLO ARTICOLO
036100021219     C*--------------------
036200021219     C     ERRL00        IFEQ      '0'
036300021223     C                   IF        VIDCO1 <> *blanks
036400021223     C                   EVAL      Chk_DES = *blanks
036500021220     C                   EVAL      Chk_COD = VIDCO1
036600021220     C                   EXSR      REPCOD
036700021220     C                   EVAL      VIDCO1 = Chk_COD
036800021220     C                   EVAL      VIDDC1 = Chk_DES
036900021220     C                   IF        ERRL00 = '1'
037000021220     C                   SETON                                        82
037100021220     C                   MOVEL     MSG(5)        DSMSMS
037200021220     C                   ENDIF
037300021219     C                   ENDIF
037400021223     C                   ENDIF
037500030305     C* SOGGETTI A ZERO SE SOGGETTI A BLANKS
037600030305     C                   IF        VIDSO1K = *blanks
037700030305     C                   EVAL      VIDSO1K = *zeros
037800030305     C                   ENDIF
037900030305     C                   IF        VIDPO1K = *blanks
038000030305     C                   EVAL      VIDPO1K = *zeros
038100030305     C                   ENDIF
038200030305     C* SOGGETTO PO SE TIPO SOGGETTO = F
038300030305     C                   IF        VIDTP1  = 'F'
038400030305     C                   IF        VIDSO1F = *zeros AND
038500030305     C                             DUTLPO <> 'S'
038600030305     C                   MOVEL     '1'           ERRL00
038700030305     C                   SETON                                        81
038800030305     C                   MOVEL     MSG(9)        DSMSMS
038900030305     C                   ENDIF
039000030305     C                   ENDIF
039100030305     C* O SOGGETTO CLIENTE O PO CLIENTI SE TIPO SOGGETTO = C
039200150422     C*                  IF        VIDTP1  = 'C'
039300150422     C*                  IF        DUTLPO <> 'S'
039400150422     C*                  IF        (VIDSO1K <> *zeros AND VIDPO1K <> *zeros) OR
039500150422     C*                            (VIDSO1K =  *zeros AND VIDPO1K =  *zeros and
039600150422     C*                             VIDarea =  *zeros)
039700150422     C*                  MOVEL     '1'           ERRL00
039800150422     C*                  SETON                                        8185
039900150422     C*                  MOVEL     MSG(8)        DSMSMS
040000150422     C*                  ENDIF
040100150422     C*                  ENDIF
040200150422     C*                  ENDIF
040300150422     c*                  if        vidarea <= '000'
040400150422     C*                  IF        vidtp1 = 'C' AND
040500150422     C*                            (VIDSO1K = *zeros or VIDsO1K = *blank and
040600150422     C*                             VIDpo1K =  *zeros or  VIDPO1K = *zeros)
040700150422     c*                            or
040800150422     C*                            vidtp1 = 'F' and
040900150422     C*                            (VIDSO1F = *zeros or vidso1f <> *blank)
041000150422     C*                  MOVEL     '1'           ERRL00
041100150422     C*                  SETON                                        8185
041200150422     C*                  MOVEL     MSG(8)        DSMSMS
041300150422     C*                  ENDIF
041400150422     C*                  endif
041500030514     C* TIPO SOGGETTO DEVE ESSERE VALORIZZATO
041600030514     C                   IF        VIDTP1 = *blanks
041700030514     C                   MOVEL     '1'           ERRL00
041800030514     C                   SETON                                        78
041900030514     C                   MOVEL     MSG(6)        DSMSMS
042000030514     C                   ENDIF
042100030305     C*--------------------
042200030305     C* CONTROLLO AUTORIZZAZIONI UTENTE
042300030305     C*--------------------
042400030305     C     ERRL00        IFEQ      '0'
042500030305     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
042600150422     C                   IF        VIDTP1 = 'F' and vidso1f <> *blank and
042700150416     c                             (vidarea = *blank or vidarea = *zero)
042800030305     C                   MOVEL     VIDSO1F       WN3               3
042900030305     C                   SETOFF                                           28
043000150211     C     WN3           LOOKUP    $pog                                   28
043100030305     C     *IN28         IFEQ      *OFF
043200150220     C                   MOVEL     '1'           ERRL00
043300030305     C                   SETON                                        81
043400030305     C                   MOVEL     MSG(7)        DSMSMS
043500030305     C                   ENDIF
043600030305     C                   ENDIF
043700150220     c*
043800030305     C                   IF        VIDTP1 = 'C'
043900030305     C                   MOVEL     VIDSO1K       WA4               4
044000030305     C                   MOVE      WA4           WN3               3
044100030305     C                   SETOFF                                           28
044200150211     C     WN3           LOOKUP    $pog                                   28
044300030305     C     *IN28         IFEQ      *OFF
044400150220     C                   MOVEL     '1'           ERRL00
044500030305     C                   SETON                                        81
044600030305     C                   MOVEL     MSG(7)        DSMSMS
044700030305     C                   ENDIF
044800150220     C*
044900150220     C                   MOVE      vidpo1k       WN3               3
045000150220     C                   SETOFF                                           28
045100150220     C     WN3           LOOKUP    $pog                                   28
045200150220     C     *IN28         IFEQ      *OFF
045300150220     C                   MOVEL     '1'           ERRL00
045400150223     C                   SETON                                        85
045500150220     C                   MOVEL     MSG(7)        DSMSMS
045600150220     C                   ENDIF
045700030305     C                   ENDIF
045800150223     C                   ENDIF
045900150413     C* controllo yipo gestione valido solo per soggetti clienti
046000150413     C     ERRL00        IFEQ      '0'
046100150413     C                   IF        vidtsg <> *blank  and vidtp1 <> 'C'
046200150413     C                   MOVEL     '1'           ERRL00
046300150413     C                   SETON                                        74
046400150413     C                   MOVEL     MSG(15)       DSMSMS
046500150413     c                   endif
046600150413     c                   endif
046700150415     C     ERRL00        IFEQ      '0'
046800150415     C                   IF        vidtsg <> *blank  and vidco1 <> *blank
046900150415     C                   MOVEL     '1'           ERRL00
047000150415     C                   SETON                                        74
047100150415     C                   MOVEL     MSG(17)       DSMSMS
047200150415     c                   endif
047300150415     c                   endif
047400150413      *
047500150408     C                   clear                   DA1dal
047600150408     C                   clear                   DA1al
047700150422     C     ERRL00        IFEQ      '0'
047800150422     C                   IF        VIDDAL = *zeros or vidal = *zeros
047900150422     C                   MOVEL     '1'           ERRL00
048000150422     C                   SETON                                        7776
048100150422     C                   MOVEL     MSG(10)       DSMSMS
048200150422     C                   ENDIF
048300150422     C                   ENDIF
048400150408     C     ERRL00        IFEQ      '0'
048500150408     C                   IF        VIDDAL <> *zeros
048600150408     C     *LIKE         DEFINE    VIDDAL        DA1dal
048700150408     C                   Z-ADD     VIDDAL        WDA8
048800150408     C                   Z-ADD     *ZEROS        DA8W
048900150408     C                   EXSR      CHKDA8
049000150408     C                   Z-ADD     WDA8          VIDDAl
049100150408     C                   Z-ADD     DA8W          DA1dal
049200150408     C     ERRL00        IFEQ      '1'
049300150408     C                   SETON                                        77
049400150408     C                   MOVEL     MSG(3)        DSMSMS
049500150408     C                   ENDIF
049600150408     C                   ENDIF
049700150408     C                   ENDIF
049800150408     C     ERRL00        IFEQ      '0'
049900150408     C                   IF        VIDAL <> *zeros
050000150408     C     *LIKE         DEFINE    VIDAL         DA1al
050100150408     C                   Z-ADD     VIDAL         WDA8
050200150408     C                   Z-ADD     *ZEROS        DA8W
050300150408     C                   EXSR      CHKDA8
050400150408     C                   Z-ADD     WDA8          VIDAl
050500150408     C                   Z-ADD     DA8W          DA1al
050600150408     C     ERRL00        IFEQ      '1'
050700150408     C                   SETON                                        76
050800150408     C                   MOVEL     MSG(3)        DSMSMS
050900150408     C                   ENDIF
051000150408     C                   ENDIF
051100150408     C                   ENDIF
051200150408     C     ERRL00        IFEQ      '0'
051300150408     C                   IF        da1aL < da1dal
051400150408     C                   MOVEL     '1'           ERRL00
051500150408     C                   SETON                                        7677
051600150408     C                   MOVEL     MSG(2)        DSMSMS
051700150408     C                   ENDIF
051800150408     C                   ENDIF
051900970528     C                   ENDSR
052000021220     C*------------------------------------------------------------------------*
052100021220     C* REPKSC - CONTROLLA CODICE CLIENTE
052200021220     C*------------------------------------------------------------------------*
052300021220     C     REPKSC        BEGSR
052400021220     C*
052500021220     C* RICHIESTO ELENCO
052600021220     C                   SETOFF                                           28
052700021220     C     '?'           SCAN      Chk_SOG                                28
052800021220     C     *IN28         IFEQ      *ON
052900150216     C                   CLEAR                   PARAM10
053000150216     c                   movel     'P01'         pop1
053100150216     C                   MOVEL(P)  PARAM10       KPJBU
053200150216     c                   call      'TISIG10R'
053300150216     c                   parm                    kpjba
053400150216     C                   MOVEL     KPJBU         PARAM10
053500150216     C                   MOVE      PKSU          CHK_SOG
053600150216     C                   END
053700021220     C*
053800021220     C* SOLO CARATTERI NUMERICI
053900021220     C                   SETOFF                                       28
054000021220     C                   TESTN                   Chk_SOG              28
054100021220     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
054200021220     C                   MOVEL     '1'           ERRL00
054300021220     C                   ELSE                                                   *E'NUMERO DA TESTN
054400021220     C                   MOVE      Chk_SOG       W001              1
054500021220     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
054600021220     C                   MOVEL     '1'           ERRL00
054700021220     C                   ELSE                                                   *NUMERO
054800021220     C*
054900021220     C* CONTROLLA ESISTENZA CLIENTE
055000021220     C                   EXSR      DECTIB
055100021220     C                   ENDIF
055200021220     C                   ENDIF
055300021220     C*
055400021220     C                   ENDSR
055500021220     C*------------------------------------------------------------------------*
055600021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
055700021220     C*------------------------------------------------------------------------*
055800021220     C     DECTIB        BEGSR
055900150216     C* CONTROLLO CHE SIA NELL'ANAGRAFICO DEI BANCALI
056000150216     C     CHK_SOG       CHAIN     AZFGB01L
056100150216     C                   IF        %FOUND(AZFGB01L) AND FGBANN = ' '
056200021220     C                   CLEAR                   BS69DS
056300021220     C                   CLEAR                   ACODS
056400021220     C                   MOVEL     KNSIF         I69SIF
056500021220     C                   MOVE(P)   Chk_SOG       I69KAC
056600021220     C                   CALL      'TIBS69R'
056700021220     C                   PARM                    BS69DS
056800021220     C                   PARM                    ACODS
056900021220     C                   PARM                    INDDS
057000021220     C                   PARM                    CLPDS
057100021220     C                   PARM                    CLSDS
057200150216     C                   ELSE
057300150216     C                   EVAL      O69ERR = '1'
057400150216     C                   END
057500021220     C     O69ERR        IFNE      '1'
057600021223     C                   EVAL      Chk_DES = acoRAG
057700021220     C                   ELSE
057800021220     C                   MOVEL     '1'           ERRL00
057900021220     C                   ENDIF
058000021220     C*
058100021220     C                   ENDSR
058200021220     C*------------------------------------------------------------------------*
058300021220     C* REPPO - CONTROLLA CODICE P.O.
058400021220     C*------------------------------------------------------------------------*
058500021220     C     REPPO         BEGSR
058600021220     C*
058700021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
058800021220     C*
058900021220     C* RICHIESTO ELENCO
059000021220     C                   SETOFF                                           28
059100021220     C     '?'           SCAN      Chk_SOG                                28
059200021220     C     *IN28         IFEQ      *ON
059300021220     C*
059400021220     C* RICHIESTO ELENCO P.O.
059500021220     C                   CALL      'TNSD24R'
059600021220     C                   PARM                    §COD              3
059700021220     C                   PARM      'F'           §TIP              1
059800021220     C                   PARM                    §§DES            25
059900021220     C                   PARM                    §§PRA             3
060000021220     C                   MOVEL     *all'0'       Chk_SOG
060100021220     C                   MOVE      §COD          Chk_SOG
060200021220     C                   ELSE
060300021220     C                   MOVE      Chk_SOG       WrkPO_N
060400021220     C     WrkPO_N       CHAIN     azorg01l
060500021220     C                   IF        %found(azorg01l)
060600021220     C                   EVAL      Chk_DES = orgDES
060700021220     C                   ELSE
060800021220     C                   EVAL      ERRL00 = '1'
060900021220     C                   EVAL      Chk_DES = *all'*'
061000021220     C                   ENDIF
061100021220     C                   ENDIF
061200021220     C*
061300021220     C                   ENDSR
061400150409     C*------------------------------------------------------------------------*
061500150409     C* REParea CONTROLLA CODICE P.O.
061600150409     C*------------------------------------------------------------------------*
061700150409     C     REParea       BEGSR
061800150409     C*
061900150409     C*
062000150409     C* RICHIESTO ELENCO
062100150409     C                   SETOFF                                           28
062200150409     C     '?'           SCAN      vidarea                                28
062300150409     C     *IN28         IFEQ      *ON
062400150409     c                   Z-Add     1             §tKut
062500150409     C                   MOVEL     '05'          §TCOD
062600150409     C                   MOVEL     *BLANKS       §TKEY
062700150409     C                   MOVEL     *BLANKS       §TDES
062800150409     C                   CALL      'X§TABER'
062900150409     C                   PARM                    §TKUT             1 0
063000150409     C                   PARM                    §TCOD             2
063100150409     C                   PARM                    §TKEY             8
063200150409     C                   PARM                    §TDES            30
063300150409     C                   MOVEL     §TKEY         vidarea
063400150409     C                   MOVEL     §TDES         vidaread
063500150409     c                   else
063600150409     C                   MOVEL     '05'          COD
063700150409     C                   MOVEL     vidarea       tblkey
063800150409     C     KTAB          CHAIN     TABEL00f
063900150409     C                   IF        %found(tabel00f)
064000150409     C                   MOVEL     tbluni        ds05
064100150409     C                   movel     §05des        vidaread
064200150409     C                   Else
064300150409     C                   EVAL      ERRL00 = '1'
064400150409     c                   endif
064500150409     c                   endif
064600150409     C*
064700150409     C                   ENDSR
064800021220     C*------------------------------------------------------------------------*
064900021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
065000021220     C*------------------------------------------------------------------------*
065100021220     C     REPCOD        BEGSR
065200021220     C*
065300021220     C* RICHIESTO ELENCO
065400021220     C                   SETOFF                                           28
065500021220     C     '?'           SCAN      Chk_COD                                28
065600021220     C     *IN28         IFEQ      *ON
065700021220     C*
065800021220     C* RICHIESTO ELENCO ARTICOLI
065900021220     C                   MOVEL     KPJBU         SAVKPJBU
066000021220     C                   MOVEL(P)  'P01'         KPJBU
066100021220     C                   CALL      'TISIG1R'
066200021220     C                   PARM                    KPJBA
066300021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
066400021220     C                   MOVEL     SAVKPJBU      KPJBU
066500021220     C                   ELSE
066600021220     C     Chk_COD       CHAIN     azagb01l
066700021220     C                   IF        %found(azagb01l)
066800021220     C                   EVAL      Chk_DES = agbDES
066900021220     C                   ELSE
067000021220     C                   EVAL      ERRL00 = '1'
067100021220     C                   EVAL      Chk_DES = *all'*'
067200021220     C                   ENDIF
067300021220     C                   ENDIF
067400021220     C*
067500021220     C                   ENDSR
067600021220     C*------------------------------------------------------------------------*
067700021220     C* CHKDA8 - CONTROLLA LA DATA (8)
067800021220     C*------------------------------------------------------------------------*
067900021220     C     CHKDA8        BEGSR
068000021220     C*
068100150422     C     *LIKE         DEFINE    VIDDAl        WDA8
068200150422     C     *LIKE         DEFINE    VIDDAl        DA8W
068300021220     C*
068400021220     C                   Z-ADD     WDA8          G08DAT
068500021220     C                   Z-ADD     *ZEROS        G08INV
068600021220     C                   MOVEL     '0'           G08ERR
068700021220     C                   CALL      'XSRDA8'
068800021220     C                   PARM                    WLBDA8
068900021220     C                   Z-ADD     G08DAT        WDA8
069000021220     C                   Z-ADD     G08INV        DA8W
069100021220     C     G08ERR        IFEQ      '1'
069200021220     C                   MOVEL     '1'           ERRL00
069300021220     C                   ENDIF
069400021220     C*
069500021220     C                   ENDSR
069600021219     C*--------------------------------------------------------------------------------------------*
069700021219     C* REPERISCE I DATI UTENTE
069800021219     C*--------------------------------------------------------------------------------------------*
069900021219     C     REPDATIUTE    BEGSR
070000021219     C*
070100021219     C* INIZIALIZZA VARIABILI DI WRK
070200021219     C                   CLEAR                   TIBS34DS
070300021219     C                   CLEAR                   AZUTEDS
070400150211     C                   CLEAR                   dute01
070500021219     C                   CLEAR                   DDATIUTE
070600021219     C*
070700021219     C     *DTAARA       DEFINE    §azute        azuteds
070800021219     C     *DTAARA       DEFINE    §datiute      ddatiute
070900021219     C                   IN(E)     *DTAARA
071000021219     C                   IF        %Error
071100021220     C                   EVAL      I34Tla = 'L'
071200021219     C                   CALL      'TIBS34R'
071300021219     C                   PARM                    Tibs34Ds
071400021219     C                   IN        *DTAARA
071500021219     C                   ENDIF
071600150211
071700150211     c                   Clear                   wabi
071800150211     c                   Clear                   dLat
071900150211
072000150211      * Verifica errori e autorità profilo
072100150211s   1c                   Select
072200150211      * se ho errori nei dati utente esco dal pgm
072300150211w   1c                   When      DutErr = 'E'
072400150211     c                   seton                                        lr
072500150211     c                   return
072600150211      * se non c'è l'abilitazione
072700150211      * --> se 1° livello, abilitazioni al terminal
072800150211      *     se 2° livello, abilitazioni al punto operativo
072900150211w   1c                   When      UteAut = *Blanks
073000150211if  2c                   If        DutLpo = '1'
073100150211     c                   Eval      wabi   = 'TP'
073200150211e   2c                   EndIf
073300150211if  2c                   If        DutLpo = '2'
073400150211     c                   Eval      wabi   = 'PO'
073500150211e   2c                   EndIf
073600150211if  2c                   If        DutLpo = 'S'
073700150211     c                   Eval      wabi   = 'AZ'
073800150211e   2c                   EndIf
073900150211      * carica le abilitazioni del profilo
074000150211x   1c                   Other
074100150211     c                   Movel     UteFaf        Dute01
074200150211     c                   If        §Utemgb <> *Blanks
074300150211     c                   Eval      wabi = §Utemgb
074400150211     c                   Else
074500150211     c                   Eval      wabi = UteAut
074600150211     c                   EndIf
074700150211e   1c                   EndSl
074800150211      * controllo se ok l'abilitazione dell'utente
074900150211     c                   Clear                   Tibs02ds
075000150211     c                   Eval      T02Mod = 'C'
075100150211     c                   Eval      T02Sif = knsif
075200150211     c                   Eval      T02Cod = 'LAT'
075300150211     c                   Movel(p)  wabi          T02Ke1
075400150211     c                   Call      'TIBS02R'
075500150211     c                   Parm                    kpjba
075600150211     c                   Parm                    Tibs02ds
075700150211if  1c                   If        T02Err = *Blanks
075800150211     c                   Eval      dLat = T02Uni
075900150211e   1c                   EndIf
076000150211      * errore o non abilitato (impossibile xchè controllato nel filtro)
076100150211if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
076200150211     c                   seton                                        lr
076300150211     c                   return
076400150211   x1c                   Else
076500150211      * Reperimento dei P.O. gestibili dall'utente
076600150211     c                   clear                   TRUL31DS
076700150211     c                   clear                   TRUL31DS2
076800150211     c                   eval      I31abi = wabi
076900150211     c                   eval      I31cdi = DUTdis
077000150211     c                   eval      I31car = DUTare
077100150211     c                   eval      I31cpo = DUTpou
077200150211     c                   call      'TRUL31R'
077300150211     c                   parm                    KPJBA
077400150211     c                   parm                    TRUL31DS
077500150211     c                   parm                    TRUL31DS2
077600150211if  2c                   if        O31pog > *zeros
077700150211     c                   movea     O31pog        $pog
077800150211     c                   movea     O31arg        $arg
077900150211     c                   movea     O31dig        $dig
078000150211x   2c                   else
078100150211     c* impossibile perchè controllato nel filtrp
078200150211     c                   seton                                        lr
078300150211     c                   return
078400150211e   2c                   endif
078500150211    1c                   endif
078600150408     c                   if        dutpou = 046
078700150408     c                   seton                                        66
078800150408     c                   endif
078900021219     C*
079000021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
079100021219     C                   MOVEL(P)  RSUT          DSFIRS           20
079200021219     C*
079300021219     C                   ENDSR
079400030123     C*------------------------------------------------------------------------*
079500030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
079600030123     C*------------------------------------------------------------------------*
079700030123     C     VIDPAR        BEGSR
079800030123     C*
079900030123     C                   MOVEL     VIDTP1        PARTP1
080000030305     C                   IF        VIDTP1 = 'C'
080100030305     C                   MOVEL     VIDSO1K       PARSO1
080200030305     C                   MOVEL     VIDPO1K       PARPO1
080300030305     C                   ENDIF
080400150422     C                   IF        VIDTP1 = 'F' and
080500150422     c                             vidso1f <> *blank and vidso1f <> *zeros
080600030612     C                   MOVEL     *all'0'       PARSO1
080700030612     C                   MOVE      VIDSO1F       PARSO1
080800030305     C                   ENDIF
080900030123     C                   MOVEL     VIDCO1        PARCO1
081000150402     C                   MOVE      Vfile         PARfile
081100150402     C                   MOVE      Vpulf         PARpulf
081200150408     C                   Z-ADD     DA1DAl        PARDAL
081300150408     C                   Z-ADD     DA1AL         PARAL
081400150413     c                   if        vidarea  > *zeros
081500150409     C                   MOVEL     Vidarea       PARarea
081600150417     C                   clear                   PARSO1
081700150417     C                   clear                   PARPO1
081800150413     c                   endif
081900150409     C                   MOVEL     Vidtsg        PARtsg
082000030123     C*
082100030123     C                   ENDSR
082200030123     C*------------------------------------------------------------------------*
082300030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
082400030123     C*------------------------------------------------------------------------*
082500030123     C     AZZVID        BEGSR
082600030123     C*
082700150408     C                   Z-ADD     0             VIDDAl
082800150408     C                   Z-ADD     0             VIDal
082900150211     C                   MOVEL     'C'           VIDTP1
083000030123     C                   MOVEL     *blanks       VIDSO1K
083100030123     C                   MOVEL     *blanks       VIDSO1F
083200030123     C                   MOVEL     *blanks       VIDCO1
083300150414     c                   if        *in66
083400150414     C                   MOVE      'S'           Vfile
083500150414     C                   MOVE      'S'           Vpulf
083600150414     c                   else
083700150408     C                   MOVE      'N'           Vfile
083800150408     C                   MOVE      'N'           Vpulf
083900150414     c                   endif
084000150211     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
084100150211     C                   SETOn                                        55
084200150211     C                   SETOn                                        56
084300030123     C*
084400030123     C                   ENDSR
084500030123     C*------------------------------------------------------------------------*
084600030123     C* *INZSR - OPERAZIONI INIZIALI
084700030123     C*------------------------------------------------------------------------*
084800030123     C     *INZSR        BEGSR
084900030123     C*
085000030123     C* RICEVIMENTO PARAMETRI
085100030123     C     *ENTRY        PLIST
085200030123     C                   PARM                    KPJBA
085300030618     C*---
085400030618     C* CHIAVI DI LETTURA
085500030618     C*---
085600030618     C* LETTURA tntbe01l - PARZIALE
085700030618     C     KEYTBEP       KLIST
085800030618     C                   KFLD                    tbeCOD
085900030618     C                   KFLD                    tbeKE1
086000030123     C*
086100150409     C     KTAB          KLIST
086200150409     C                   KFLD                    CODUT             1 0
086300150409     C                   KFLD                    COD               2
086400150409     C                   KFLD                    tblkey
086500150409     C                   Z-ADD     1             CODUT
086600150409      * tabelle
086700150211     C                   EXSR      REPDATIUTE
086800030123     C*
086900030123     C* REPERISCO DATA E ORA CORRENTI
087000030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
087100030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
087200030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
087300030123     C                   Z-ADD     WN8           G08DAT
087400030123     C                   Z-ADD     *ZEROS        G08INV
087500030123     C                   MOVEL     '0'           G08ERR
087600030123     C                   CALL      'XSRDA8'
087700030123     C                   PARM                    WLBDA8
087800030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
087900030123     C*
088000030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
088100030123     C     XMSGPA        PLIST
088200030123     C                   PARM                    DSMSMS
088300030123     C                   PARM                    DSMSPG
088400030123     C                   PARM                    DSMSKE
088500030123     C                   PARM                    DSMSER
088600030123     C*
088700030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
088800030123     C                   CALL      'XKEY'
088900030123     C                   PARM                    DSKEY
089000030123     C*
089100030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
089200030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
089300030123     C*
089400030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
089500030123     C*   -NOME PGM CHIAMANTE
089600030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
089700150414     C                   MOVEL     'TISIG16R'    DSMSPG
089800150414     C                   MOVEL     'TISIG16R'    MSGQUE
089900030123     C                   MOVEL     *BLANKS       MSGKEY
090000030123     C                   MOVEL     *BLANKS       DSMSMS
090100030123     C*
090200030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
090300030123     C                   Z-ADD     0             FUNGRU            5 0
090400030123     C*
090500030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
090600030123     C                   EXSR      FUNGES
090700030123     C*
090800030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
090900030123     C                   MOVEL     '0'           F12L00            1
091000030123     C                   MOVEL     '0'           F03L00            1
091100030123     C                   MOVEL     '0'           FINL00            1
091200030123     C                   MOVEL     '0'           ERRL00            1
091300030123     C                   MOVEL     '0'           FLFINE            1
091400030123     C*
091500030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
091600030123     C                   EXSR      AZZVID
091700030123     C*
091800030123     C                   ENDSR
091900030123     C*------------------------------------------------------------------------*
092000030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
092100030123     C*------------------------------------------------------------------------*
092200030123     C     FUNGES        BEGSR
092300030123     C*
092400030123     C* PULISCO CAMPI DI LAVORO
092500030123     C                   Z-ADD     0             W                 5 0
092600030123     C*
092700030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
092800030123     C* E CARICO TABELLA FUNZIONI ATTIVE
092900030123     C     1             DO        24            I
093000030123     C                   MOVEL     FUD(I)        FUDRIG
093100030123     C*
093200030123     C* PULIZIA FUNZIONI PRECEDENTI
093300030123     C                   MOVEL     *BLANKS       FUV(I)
093400030123     C*
093500030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
093600030123     C* (SALTA F24)
093700030123     C     FUDATT        IFEQ      'S'
093800030123     C     FUDFUN        ANDNE     '24'
093900030123     C                   ADD       1             W
094000030123     C                   MOVEL     FUDDES        FUV(W)
094100030123     C                   END
094200030123     C                   END                                                    DO WHILE
094300030123     C*
094400030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
094500030123     C* ALTRIMENTI DISATTIVO F24
094600030123     C     1             DO        24            I
094700030123     C                   MOVEL     FUD(I)        FUDRIG
094800030123     C*
094900030123     C     FUDFUN        IFEQ      '24'
095000030123     C     W             IFGT      6
095100030123     C                   MOVEL     'S'           FUDATT
095200030123     C                   ELSE
095300030123     C                   MOVEL     *BLANKS       FUDATT
095400030123     C                   END                                                    W GT 8
095500030123     C                   MOVEL     FUDRIG        FUD(I)
095600030123     C                   END
095700030123     C                   END                                                    DO WHILE
095800030123     C*
095900030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
096000030123     C     W             DIV       6             I
096100030123     C                   MVR                     Z
096200030123     C*
096300030123     C     Z             IFNE      0
096400030123     C     I             ADD       1             Z
096500030123     C                   ELSE
096600030123     C                   Z-ADD     I             Z                 5 0
096700030123     C                   END
096800030123     C*
096900030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
097000030123     C                   ADD       1             FUNGRU
097100030123     C     FUNGRU        IFGT      Z
097200030123     C     FUNGRU        ORLT      1
097300030123     C                   Z-ADD     1             FUNGRU
097400030123     C                   END
097500030123     C*
097600030123     C* PRIMO GRUPPO DI 6 FUNZIONI
097700030123     C     FUNGRU        IFEQ      1
097800030123     C                   MOVEL     FUV(1)        FUN001
097900030123     C                   MOVEL     FUV(2)        FUN002
098000030123     C                   MOVEL     FUV(3)        FUN003
098100030123     C                   MOVEL     FUV(4)        FUN004
098200030123     C                   MOVEL     FUV(5)        FUN005
098300030123     C                   MOVEL     FUV(6)        FUN006
098400030123     C                   END
098500030123     C*
098600030123     C* SECONDO GRUPPO DI 6 FUNZIONI
098700030123     C     FUNGRU        IFEQ      2
098800030123     C                   MOVEL     FUV(7)        FUN001
098900030123     C                   MOVEL     FUV(8)        FUN002
099000030123     C                   MOVEL     FUV(9)        FUN003
099100030123     C                   MOVEL     FUV(10)       FUN004
099200030123     C                   MOVEL     FUV(11)       FUN005
099300030123     C                   MOVEL     FUV(12)       FUN006
099400030123     C                   END
099500030123     C*
099600030123     C* TERZO GRUPPO DI 6 FUNZIONI
099700030123     C     FUNGRU        IFEQ      3
099800030123     C                   MOVEL     FUV(13)       FUN001
099900030123     C                   MOVEL     FUV(14)       FUN002
100000030123     C                   MOVEL     FUV(15)       FUN003
100100030123     C                   MOVEL     FUV(16)       FUN004
100200030123     C                   MOVEL     FUV(17)       FUN005
100300030123     C                   MOVEL     FUV(18)       FUN006
100400030123     C                   END
100500030123     C*
100600030123     C* QUARTO GRUPPO DI 6 FUNZIONI
100700030123     C     FUNGRU        IFEQ      4
100800030123     C                   MOVEL     FUV(19)       FUN001
100900030123     C                   MOVEL     FUV(20)       FUN002
101000030123     C                   MOVEL     FUV(21)       FUN003
101100030123     C                   MOVEL     FUV(22)       FUN004
101200030123     C                   MOVEL     *BLANKS       FUN005
101300030123     C                   MOVEL     *BLANKS       FUN006
101400030123     C                   END
101500030123     C*
101600030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
101700030123     C                   MOVEL     *BLANKS       FUN007
101800030123     C                   MOVEL     *BLANKS       FUN008
101900030123     C*
102000030123     C     1             DO        24            I
102100030123     C                   MOVEL     FUD(I)        FUDRIG
102200030123     C     FUDFUN        IFEQ      '24'
102300030123     C     FUDATT        ANDEQ     'S'
102400030123     C                   MOVEL     FUDDES        FUN008
102500030123     C                   END
102600030123     C                   END                                                    DO
102700030123     C                   ENDSR
102800030123     C*------------------------------------------------------------------------*
102900030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
103000030123     C*------------------------------------------------------------------------*
103100030123     C     FUNCON        BEGSR
103200030123     C*
103300030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
103400030123     C                   EXSR      FUNVAR
103500030123     C*
103600030123     C* PULISCO CAMPI DI LAVORO
103700030123     C                   Z-ADD     0             I                 5 0
103800030123     C                   Z-ADD     0             W                 5 0
103900030123     C*
104000030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
104100030123     C     FUNKEY        IFNE      *BLANKS
104200030123     C     1             DO        24            I
104300030123     C                   MOVEL     FUD(I)        FUDRIG
104400030123     C*
104500030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
104600030123     C     FUNKEY        IFEQ      FUDFUN
104700030123     C     FUDATT        ANDEQ     'S'
104800030123     C                   Z-ADD     I             W
104900030123     C                   END
105000030123     C                   END                                                    DO WHILE
105100030123     C*
105200030123     C* FUNZIONE NON ATTIVA (ERRORE)
105300030123     C     W             IFEQ      0
105400030123     C*
105500030123     C* EMISSIONE MESSAGGIO DI ERRORE
105600030123     C                   MOVEL     MSG(1)        DSMSMS
105700030123     C*
105800030123     C                   END                                                    W=0
105900030123     C                   END                                                    FUNKEY NE *BLANKS
106000030123     C*
106100030123     C* FUNZIONE ATTIVA (PROCEDI ...)
106200030123     C     W             IFNE      0
106300030123     C     FUNKEY        OREQ      *BLANKS
106400030123     C*
106500030123     C     FUNKEY        IFEQ      *BLANKS
106600030123     C                   EXSR      ENTGES
106700030123     C                   END
106800030123     C     FUNKEY        IFEQ      '03'
106900030123     C                   EXSR      F03GES
107000030123     C                   END
107100030123     C     FUNKEY        IFEQ      '06'
107200030123     C                   EXSR      F06GES
107300030123     C                   END
107400030123     C     FUNKEY        IFEQ      '12'
107500030123     C                   EXSR      F12GES
107600030123     C                   END
107700030123     C     FUNKEY        IFEQ      '24'
107800030123     C                   EXSR      F24GES
107900030123     C                   END
108000030123     C*
108100030123     C                   END
108200030123     C*
108300030123     C                   ENDSR
108400030123     C*------------------------------------------------------------------------*
108500030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
108600030123     C*------------------------------------------------------------------------*
108700030123     C     FUNVAR        BEGSR
108800030123     C*
108900030123     C* INIZIALIZZA VARIABILE -FUNKEY-
109000030123     C*
109100030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
109200030123     C     KEY           IFEQ      ENTER
109300030123     C                   MOVEL     *BLANKS       FUNKEY
109400030123     C                   END
109500030123     C     KEY           IFEQ      F1
109600030123     C                   MOVEL     '01'          FUNKEY
109700030123     C                   END
109800030123     C     KEY           IFEQ      F2
109900030123     C                   MOVEL     '02'          FUNKEY
110000030123     C                   END
110100030123     C     KEY           IFEQ      F3
110200030123     C                   MOVEL     '03'          FUNKEY
110300030123     C                   END
110400030123     C     KEY           IFEQ      F4
110500030123     C                   MOVEL     '04'          FUNKEY
110600030123     C                   END
110700030123     C     KEY           IFEQ      F5
110800030123     C                   MOVEL     '05'          FUNKEY
110900030123     C                   END
111000030123     C     KEY           IFEQ      F6
111100030123     C                   MOVEL     '06'          FUNKEY
111200030123     C                   END
111300030123     C     KEY           IFEQ      F7
111400030123     C                   MOVEL     '07'          FUNKEY
111500030123     C                   END
111600030123     C     KEY           IFEQ      F8
111700030123     C                   MOVEL     '08'          FUNKEY
111800030123     C                   END
111900030123     C     KEY           IFEQ      F9
112000030123     C                   MOVEL     '09'          FUNKEY
112100030123     C                   END
112200030123     C     KEY           IFEQ      F10
112300030123     C                   MOVEL     '10'          FUNKEY
112400030123     C                   END
112500030123     C     KEY           IFEQ      F11
112600030123     C                   MOVEL     '11'          FUNKEY
112700030123     C                   END
112800030123     C     KEY           IFEQ      F12
112900030123     C                   MOVEL     '12'          FUNKEY
113000030123     C                   END
113100030123     C     KEY           IFEQ      F13
113200030123     C                   MOVEL     '13'          FUNKEY
113300030123     C                   END
113400030123     C     KEY           IFEQ      F14
113500030123     C                   MOVEL     '14'          FUNKEY
113600030123     C                   END
113700030123     C     KEY           IFEQ      F15
113800030123     C                   MOVEL     '15'          FUNKEY
113900030123     C                   END
114000030123     C     KEY           IFEQ      F16
114100030123     C                   MOVEL     '16'          FUNKEY
114200030123     C                   END
114300030123     C     KEY           IFEQ      F17
114400030123     C                   MOVEL     '17'          FUNKEY
114500030123     C                   END
114600030123     C     KEY           IFEQ      F18
114700030123     C                   MOVEL     '18'          FUNKEY
114800030123     C                   END
114900030123     C     KEY           IFEQ      F19
115000030123     C                   MOVEL     '19'          FUNKEY
115100030123     C                   END
115200030123     C     KEY           IFEQ      F20
115300030123     C                   MOVEL     '20'          FUNKEY
115400030123     C                   END
115500030123     C     KEY           IFEQ      F21
115600030123     C                   MOVEL     '21'          FUNKEY
115700030123     C                   END
115800030123     C     KEY           IFEQ      F22
115900030123     C                   MOVEL     '22'          FUNKEY
116000030123     C                   END
116100030123     C     KEY           IFEQ      F23
116200030123     C                   MOVEL     '23'          FUNKEY
116300030123     C                   END
116400030123     C     KEY           IFEQ      F24
116500030123     C                   MOVEL     '24'          FUNKEY
116600030123     C                   END
116700030123     C     KEY           IFEQ      ROLLUP
116800030123     C                   MOVEL     'RU'          FUNKEY
116900030123     C                   END
117000030123     C     KEY           IFEQ      HELP
117100030123     C                   MOVEL     'HP'          FUNKEY
117200030123     C                   END
117300030123     C                   ENDSR
117400030123     C*------------------------------------------------------------------------*
117500030123     C* NOTE PER COMPILAZIONE SCHIERE
117600030123     C*------------------------------------------------------------------------*
117700030123     C* -MSG-
117800030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
117900030123     C* (NON TOCCARE MSG,1)
118000030123     C*
118100030123     C* -CMD-
118200030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
118300030123     C*
118400030123     C* -FUD-
118500030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
118600030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
118700030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
118800030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
118900030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
119000030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
119100030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
119200030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
119300030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
119400030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
119500030123     C*
119600030123     C*------------------------------------------------------------------------*
119700030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
119800150211Tasto funzionale non gestito.                                                  1
119900150408Le date del periodo richiesto non sono inserite correttamente                  2
120000150211Data errata.                                                                   3
120100150211Soggetto movimento errato.                                                     4
120200150211Articolo indicato errato.                                                      5
120300150211Campo obbligatorio.                                                            6
120400150220Soggetto indicato non in gestione all'utente.                                  7
120500150420Indicare o il soggetto cliente o la Fil. o area dei soggetti clienti.          8
120600150211Indicare il soggetto Fil.                                                      9
120700150422Le date del periodo sono obbligatorie                                          10
120800150420                                                                               11
120900150211Effettuare almeno una scelta.                                                  12
121000150422                                                                               13
121100150409Codice area selezionato non valido                                             14
121200150413Tipo gestione ammesso solo per soggetti CLIENTI                                15
121300150422                                                                               16
121400150415Scegliere o codice articolo o tipo gestione                                    17
121500150409                                                                               18
121600150409                                                                               19
121700150409                                                                               20
121800030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
121900030123                   -        (S/N)=N                        funz.01¬
122000030123                   -        (S/N)=N                        funz.02¬
122100030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
122200030123                   -        (S/N)=N                        funz.04¬
122300030123                   -        (S/N)=N                        funz.05¬
122400030123F06=Lancio         -        (S/N)=S                        funz.06¬
122500030123                   -        (S/N)=N                        funz.07¬
122600030123                   -        (S/N)=N                        funz.08¬
122700030123                   -        (S/N)=N                        funz.09¬
122800030123                   -        (S/N)=N                        funz.10¬
122900030123                   -        (S/N)=N                        funz.11¬
123000150211                   -        (S/N)=N                        funz.12¬
123100030123                   -        (S/N)=N                        funz.13¬
123200030123                   -        (S/N)=N                        funz.14¬
123300030123                   -        (S/N)=N                        funz.15¬
123400030123                   -        (S/N)=N                        funz.16¬
123500030123                   -        (S/N)=N                        funz.17¬
123600030123                   -        (S/N)=N                        funz.18¬
123700030123                   -        (S/N)=N                        funz.19¬
123800030123                   -        (S/N)=N                        funz.20¬
123900030123                   -        (S/N)=N                        funz.21¬
124000030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
124100030123                   -        (S/N)=N                        funz.23¬
124200030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
