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
023700150504     C                   CALL      'BCH10'
023800150504 MOM C*                  CALL      'TISIG15R'
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
027400150415     C                   SETOFF                                       857475
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
030200150414     c                   clear                   vidso1f
030300030305     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
030400030123     C                   SETON                                        55
030500030305     C                   SETON                                        56
030600021223     C                   ENDIF
030700030123     C                   IF        VIDTP1 = 'F'
030800150414     c                   clear                   vidtsg
030900150414     c                   clear                   vidso1k
031000150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
031100030123     C                   SETOFF                                       55
031200030305     C                   SETOFF                                       56
031300021223     C                   ENDIF
031400021223     C* Eseguo i controlli di validità soggetto movimento
031500030107     C                   IF        *IN55   =  *ON
031600021220     C*
031700030305     C                   IF        VIDTP1 = 'C'
031800030305     C                   IF        VIDSO1K <> *blanks AND
031900030305     C                             VIDSO1K <> *zeros
032000030305     C                   EVAL      Chk_DES =  *blanks
032100030305     C                   MOVEL     *zeros        Chk_SOG
032200030305     C                   MOVE      VIDSO1K       Chk_SOG
032300021220     C                   EXSR      REPKSC
032400030305     C                   IF        ERRL00  = '1'
032500030305     C                   SETON                                        81
032600030305     C                   MOVEL     MSG(4)        DSMSMS
032700030305     C                   ENDIF
032800030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
032900030305     C                   EVAL      VIDDS1  = Chk_DES
033000030305     C                   ENDIF
033100030305     C                   IF        VIDPO1K <> *blanks AND
033200030305     C                             VIDPO1K <> *zeros
033300030305     C                   EVAL      Chk_DES =  *blanks
033400030305     C                   MOVEL     *zeros        Chk_SOG
033500030305     C                   MOVE      VIDPO1K       Chk_SOG
033600030305     C                   EXSR      REPPO
033700030305     C                   IF        ERRL00  = '1'
033800030305     C                   SETON                                        85
033900030305     C                   MOVEL     MSG(4)        DSMSMS
034000030305     C                   ENDIF
034100030305     C                   MOVE(P)   Chk_SOG       VIDPO1K
034200030305     C                   EVAL      VIDPO1KD = Chk_DES
034300021220     C                   ENDIF
034400030305     C                   ENDIF
034500021220     C*
034600030107     C                   ELSE
034700030305     C                   IF        VIDSO1F <> *blanks AND
034800030305     C                             VIDSO1F <> *zeros
034900030123     C                   IF        VIDTP1 = 'F'
035000030305     C                   EVAL      Chk_DES =  *blanks
035100030305     C                   MOVEL     *zeros        Chk_SOG
035200030305     C                   MOVE      VIDSO1F       Chk_SOG
035300030107     C                   EXSR      REPPO
035400030305     C                   MOVE(P)   Chk_SOG       VIDSO1F
035500030305     C                   EVAL      VIDDS1  = Chk_DES
035600030305     C                   IF        ERRL00  = '1'
035700030305     C                   SETON                                        81
035800030305     C                   MOVEL     MSG(4)        DSMSMS
035900030305     C                   ENDIF
036000030107     C                   ENDIF
036100030107     C*
036200030107     C                   ENDIF
036300030107     C                   ENDIF
036400030305     C                   ENDIF
036500150409     C*--------------------
036600150409     C* SCELTA/area
036700150409     C*--------------------
036800150414     c                   clear                   vidaread
036900150409     C     ERRL00        IFEQ      '0'
037000150413     C                   IF        VIDarea <> *blanks and
037100150413     C                             VIDarea <> *zero
037200150409     C                   EXSR      REParea
037300150416     c     vidarea       lookup    $arg                                   28
037400150416     C  n28              MOVE      '1'           ERRL00
037500150409     C                   IF        ERRL00 = '1'
037600150416     C                   SETON                                        75
037700150409     C                   MOVEL     MSG(14)       DSMSMS
037800150409     C                   ENDIF
037900150409     C                   ENDIF
038000150409     C                   ENDIF
038100021219     C*--------------------
038200021219     C* SCELTA/CONTROLLO ARTICOLO
038300021219     C*--------------------
038400021219     C     ERRL00        IFEQ      '0'
038500021223     C                   IF        VIDCO1 <> *blanks
038600021223     C                   EVAL      Chk_DES = *blanks
038700021220     C                   EVAL      Chk_COD = VIDCO1
038800021220     C                   EXSR      REPCOD
038900021220     C                   EVAL      VIDCO1 = Chk_COD
039000021220     C                   EVAL      VIDDC1 = Chk_DES
039100021220     C                   IF        ERRL00 = '1'
039200021220     C                   SETON                                        82
039300021220     C                   MOVEL     MSG(5)        DSMSMS
039400021220     C                   ENDIF
039500021219     C                   ENDIF
039600021223     C                   ENDIF
039700030305     C* SOGGETTI A ZERO SE SOGGETTI A BLANKS
039800030305     C                   IF        VIDSO1K = *blanks
039900030305     C                   EVAL      VIDSO1K = *zeros
040000030305     C                   ENDIF
040100030305     C                   IF        VIDPO1K = *blanks
040200030305     C                   EVAL      VIDPO1K = *zeros
040300030305     C                   ENDIF
040400030305     C* SOGGETTO PO SE TIPO SOGGETTO = F
040500030305     C                   IF        VIDTP1  = 'F'
040600030305     C                   IF        VIDSO1F = *zeros AND
040700030305     C                             DUTLPO <> 'S'
040800030305     C                   MOVEL     '1'           ERRL00
040900030305     C                   SETON                                        81
041000030305     C                   MOVEL     MSG(9)        DSMSMS
041100030305     C                   ENDIF
041200030305     C                   ENDIF
041300150219     C* dettaglio solo se scelto un soggetto cliente
041400150219     C                   IF        Vdet = 'S' and (
041500150219     C                             VIDSO1K =  *blanks or
041600150219     C                             VIDSO1K =  *zeros )
041700150218     C                   MOVEL     '1'           ERRL00
041800150219     C                   MOVEL     MSG(13)       DSMSMS
041900150218     C                   ENDIF
042000030305     C* O SOGGETTO CLIENTE O PO CLIENTI SE TIPO SOGGETTO = C
042100030305     C                   IF        VIDTP1  = 'C'
042200150429     C                   IF         VIDSO1K  > *zeros AND (VIDPO1K > *zeros or
042300150429     C                              VIDarea > *zeros)   or
042400150429     C                              VIDpO1K  > *zeros AND(VIDsO1K > *zeros or
042500150429     C                              VIDarea > *zeros)  or
042600150429     C                              VIDarea  > *zeros AND(VIDsO1K > *zeros or
042700150429     C                              VIDpo1k > *zeros)  or
042800150429     C                              VIDSO1K <=  *zeros AND VIDPO1K <= *zeros and
042900150429     C                              VIDarea <=  *zeros or
043000150429     C                              VIDSO1F > *zeros and vidarea > *zero
043100030305     C                   MOVEL     '1'           ERRL00
043200030305     C                   SETON                                        8185
043300030305     C                   MOVEL     MSG(8)        DSMSMS
043400030305     C                   ENDIF
043500150429     c                   else
043600150429     C                   if         VIDSO1F > *zeros and vidarea > *zero
043700150429     C                   MOVEL     '1'           ERRL00
043800150429     C                   SETON                                        8185
043900150429     C                   MOVEL     MSG(8)        DSMSMS
044000150429     C                   ENDIF
044100030305     C                   ENDIF
044200150420     c                   if        vidarea <= '000'
044300150420     C                   IF        vidtp1 = 'C' AND
044400150429     C                             (VIDSO1K = *zeros or VIDsO1K = *blank)and
044500150429     C                             (VIDpo1K =  *zeros or  VIDPO1K = *blank)
044600150420     c                             or
044700150420     C                             vidtp1 = 'F' and
044800150429     C                             (VIDSO1F = *zeros or vidso1f = *blank)
044900150420     C                   MOVEL     '1'           ERRL00
045000150420     C                   SETON                                        8185
045100150420     C                   MOVEL     MSG(8)        DSMSMS
045200150420     C                   ENDIF
045300150420     C                   endif
045400030514     C* TIPO SOGGETTO DEVE ESSERE VALORIZZATO
045500030514     C                   IF        VIDTP1 = *blanks
045600030514     C                   MOVEL     '1'           ERRL00
045700030514     C                   SETON                                        78
045800030514     C                   MOVEL     MSG(6)        DSMSMS
045900030514     C                   ENDIF
046000030305     C*--------------------
046100030305     C* CONTROLLO AUTORIZZAZIONI UTENTE
046200030305     C*--------------------
046300030305     C     ERRL00        IFEQ      '0'
046400030305     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
046500150414     C                   IF        VIDTP1 = 'F' and
046600150414     c                             (vidarea = *blank or vidarea = *zero)
046700030305     C                   MOVEL     VIDSO1F       WN3               3
046800030305     C                   SETOFF                                           28
046900150211     C     WN3           LOOKUP    $pog                                   28
047000030305     C     *IN28         IFEQ      *OFF
047100150220     C                   MOVEL     '1'           ERRL00
047200030305     C                   SETON                                        81
047300030305     C                   MOVEL     MSG(7)        DSMSMS
047400030305     C                   ENDIF
047500030305     C                   ENDIF
047600150220     c*
047700030305     C                   IF        VIDTP1 = 'C'
047800030305     C                   MOVEL     VIDSO1K       WA4               4
047900030305     C                   MOVE      WA4           WN3               3
048000030305     C                   SETOFF                                           28
048100150211     C     WN3           LOOKUP    $pog                                   28
048200030305     C     *IN28         IFEQ      *OFF
048300150220     C                   MOVEL     '1'           ERRL00
048400030305     C                   SETON                                        81
048500030305     C                   MOVEL     MSG(7)        DSMSMS
048600030305     C                   ENDIF
048700150220     C*
048800150220     C                   MOVE      vidpo1k       WN3               3
048900150220     C                   SETOFF                                           28
049000150220     C     WN3           LOOKUP    $pog                                   28
049100150220     C     *IN28         IFEQ      *OFF
049200150220     C                   MOVEL     '1'           ERRL00
049300150223     C                   SETON                                        85
049400150220     C                   MOVEL     MSG(7)        DSMSMS
049500150220     C                   ENDIF
049600030305     C                   ENDIF
049700150223     C                   ENDIF
049800150413     C* controllo yipo gestione valido solo per soggetti clienti
049900150413     C     ERRL00        IFEQ      '0'
050000150413     C                   IF        vidtsg <> *blank  and vidtp1 <> 'C'
050100150413     C                   MOVEL     '1'           ERRL00
050200150413     C                   SETON                                        74
050300150413     C                   MOVEL     MSG(15)       DSMSMS
050400150413     c                   endif
050500150413     c                   endif
050600150414     C     ERRL00        IFEQ      '0'
050700150414     C                   IF        vidtsg <> *blank  and vidco1 <> *blank
050800150414     C                   MOVEL     '1'           ERRL00
050900150414     C                   SETON                                        74
051000150414     C                   MOVEL     MSG(17)       DSMSMS
051100150414     c                   endif
051200150414     c                   endif
051300150413      *
051400150413     C* controllo periodo di date valido solo per dettaglio
051500150413     C     ERRL00        IFEQ      '0'
051600150413     C                   IF        vdet <> 'S' and
051700150413     C                             (vIDDAL <> *zeros or vidal <> *zeros)
051800150413     C                   MOVEL     '1'           ERRL00
051900150413     C                   SETON                                        7776
052000150413     C                   MOVEL     MSG(16)       DSMSMS
052100150413     c                   endif
052200150413     c                   endif
052300150413      *
052400150408     C                   clear                   DA1dal
052500150408     C                   clear                   DA1al
052600150408     C     ERRL00        IFEQ      '0'
052700150408     C                   IF        VIDDAL <> *zeros
052800150408     C     *LIKE         DEFINE    VIDDAL        DA1dal
052900150408     C                   Z-ADD     VIDDAL        WDA8
053000150408     C                   Z-ADD     *ZEROS        DA8W
053100150408     C                   EXSR      CHKDA8
053200150408     C                   Z-ADD     WDA8          VIDDAl
053300150408     C                   Z-ADD     DA8W          DA1dal
053400150408     C     ERRL00        IFEQ      '1'
053500150408     C                   SETON                                        77
053600150408     C                   MOVEL     MSG(3)        DSMSMS
053700150408     C                   ENDIF
053800150408     C                   ENDIF
053900150408     C                   ENDIF
054000150408     C     ERRL00        IFEQ      '0'
054100150408     C                   IF        VIDAL <> *zeros
054200150408     C     *LIKE         DEFINE    VIDAL         DA1al
054300150408     C                   Z-ADD     VIDAL         WDA8
054400150408     C                   Z-ADD     *ZEROS        DA8W
054500150408     C                   EXSR      CHKDA8
054600150408     C                   Z-ADD     WDA8          VIDAl
054700150408     C                   Z-ADD     DA8W          DA1al
054800150408     C     ERRL00        IFEQ      '1'
054900150408     C                   SETON                                        76
055000150408     C                   MOVEL     MSG(3)        DSMSMS
055100150408     C                   ENDIF
055200150408     C                   ENDIF
055300150408     C                   ENDIF
055400150408     C     ERRL00        IFEQ      '0'
055500150408     C                   IF        da1aL < da1dal
055600150408     C                   MOVEL     '1'           ERRL00
055700150408     C                   SETON                                        7677
055800150408     C                   MOVEL     MSG(2)        DSMSMS
055900150408     C                   ENDIF
056000150408     C                   ENDIF
056100970528     C                   ENDSR
056200021220     C*------------------------------------------------------------------------*
056300021220     C* REPKSC - CONTROLLA CODICE CLIENTE
056400021220     C*------------------------------------------------------------------------*
056500021220     C     REPKSC        BEGSR
056600021220     C*
056700021220     C* RICHIESTO ELENCO
056800021220     C                   SETOFF                                           28
056900021220     C     '?'           SCAN      Chk_SOG                                28
057000021220     C     *IN28         IFEQ      *ON
057100150216     C                   CLEAR                   PARAM10
057200150216     c                   movel     'P01'         pop1
057300150216     C                   MOVEL(P)  PARAM10       KPJBU
057400150216     c                   call      'TISIG10R'
057500150216     c                   parm                    kpjba
057600150216     C                   MOVEL     KPJBU         PARAM10
057700150216     C                   MOVE      PKSU          CHK_SOG
057800150216     C                   END
057900021220     C*
058000021220     C* SOLO CARATTERI NUMERICI
058100021220     C                   SETOFF                                       28
058200021220     C                   TESTN                   Chk_SOG              28
058300021220     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
058400021220     C                   MOVEL     '1'           ERRL00
058500021220     C                   ELSE                                                   *E'NUMERO DA TESTN
058600021220     C                   MOVE      Chk_SOG       W001              1
058700021220     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
058800021220     C                   MOVEL     '1'           ERRL00
058900021220     C                   ELSE                                                   *NUMERO
059000021220     C*
059100021220     C* CONTROLLA ESISTENZA CLIENTE
059200021220     C                   EXSR      DECTIB
059300021220     C                   ENDIF
059400021220     C                   ENDIF
059500021220     C*
059600021220     C                   ENDSR
059700021220     C*------------------------------------------------------------------------*
059800021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
059900021220     C*------------------------------------------------------------------------*
060000021220     C     DECTIB        BEGSR
060100150216     C* CONTROLLO CHE SIA NELL'ANAGRAFICO DEI BANCALI
060200150216     C     CHK_SOG       CHAIN     AZFGB01L
060300150216     C                   IF        %FOUND(AZFGB01L) AND FGBANN = ' '
060400021220     C                   CLEAR                   BS69DS
060500021220     C                   CLEAR                   ACODS
060600021220     C                   MOVEL     KNSIF         I69SIF
060700021220     C                   MOVE(P)   Chk_SOG       I69KAC
060800021220     C                   CALL      'TIBS69R'
060900021220     C                   PARM                    BS69DS
061000021220     C                   PARM                    ACODS
061100021220     C                   PARM                    INDDS
061200021220     C                   PARM                    CLPDS
061300021220     C                   PARM                    CLSDS
061400150216     C                   ELSE
061500150216     C                   EVAL      O69ERR = '1'
061600150216     C                   END
061700021220     C     O69ERR        IFNE      '1'
061800021223     C                   EVAL      Chk_DES = acoRAG
061900021220     C                   ELSE
062000021220     C                   MOVEL     '1'           ERRL00
062100021220     C                   ENDIF
062200021220     C*
062300021220     C                   ENDSR
062400021220     C*------------------------------------------------------------------------*
062500021220     C* REPPO - CONTROLLA CODICE P.O.
062600021220     C*------------------------------------------------------------------------*
062700021220     C     REPPO         BEGSR
062800021220     C*
062900021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
063000021220     C*
063100021220     C* RICHIESTO ELENCO
063200021220     C                   SETOFF                                           28
063300021220     C     '?'           SCAN      Chk_SOG                                28
063400021220     C     *IN28         IFEQ      *ON
063500021220     C*
063600021220     C* RICHIESTO ELENCO P.O.
063700021220     C                   CALL      'TNSD24R'
063800021220     C                   PARM                    §COD              3
063900021220     C                   PARM      'F'           §TIP              1
064000021220     C                   PARM                    §§DES            25
064100021220     C                   PARM                    §§PRA             3
064200021220     C                   MOVEL     *all'0'       Chk_SOG
064300021220     C                   MOVE      §COD          Chk_SOG
064400021220     C                   ELSE
064500021220     C                   MOVE      Chk_SOG       WrkPO_N
064600021220     C     WrkPO_N       CHAIN     azorg01l
064700021220     C                   IF        %found(azorg01l)
064800021220     C                   EVAL      Chk_DES = orgDES
064900021220     C                   ELSE
065000021220     C                   EVAL      ERRL00 = '1'
065100021220     C                   EVAL      Chk_DES = *all'*'
065200021220     C                   ENDIF
065300021220     C                   ENDIF
065400021220     C*
065500021220     C                   ENDSR
065600150409     C*------------------------------------------------------------------------*
065700150409     C* REParea CONTROLLA CODICE P.O.
065800150409     C*------------------------------------------------------------------------*
065900150409     C     REParea       BEGSR
066000150409     C*
066100150409     C*
066200150409     C* RICHIESTO ELENCO
066300150409     C                   SETOFF                                           28
066400150409     C     '?'           SCAN      vidarea                                28
066500150409     C     *IN28         IFEQ      *ON
066600150409     c                   Z-Add     1             §tKut
066700150409     C                   MOVEL     '05'          §TCOD
066800150409     C                   MOVEL     *BLANKS       §TKEY
066900150409     C                   MOVEL     *BLANKS       §TDES
067000150409     C                   CALL      'X§TABER'
067100150409     C                   PARM                    §TKUT             1 0
067200150409     C                   PARM                    §TCOD             2
067300150409     C                   PARM                    §TKEY             8
067400150409     C                   PARM                    §TDES            30
067500150409     C                   MOVEL     §TKEY         vidarea
067600150409     C                   MOVEL     §TDES         vidaread
067700150409     c                   else
067800150409     C                   MOVEL     '05'          COD
067900150409     C                   MOVEL     vidarea       tblkey
068000150409     C     KTAB          CHAIN     TABEL00f
068100150409     C                   IF        %found(tabel00f)
068200150409     C                   MOVEL     tbluni        ds05
068300150409     C                   movel     §05des        vidaread
068400150409     C                   Else
068500150409     C                   EVAL      ERRL00 = '1'
068600150409     c                   endif
068700150409     c                   endif
068800150409     C*
068900150409     C                   ENDSR
069000021220     C*------------------------------------------------------------------------*
069100021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
069200021220     C*------------------------------------------------------------------------*
069300021220     C     REPCOD        BEGSR
069400021220     C*
069500021220     C* RICHIESTO ELENCO
069600021220     C                   SETOFF                                           28
069700021220     C     '?'           SCAN      Chk_COD                                28
069800021220     C     *IN28         IFEQ      *ON
069900021220     C*
070000021220     C* RICHIESTO ELENCO ARTICOLI
070100021220     C                   MOVEL     KPJBU         SAVKPJBU
070200021220     C                   MOVEL(P)  'P01'         KPJBU
070300021220     C                   CALL      'TISIG1R'
070400021220     C                   PARM                    KPJBA
070500021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
070600021220     C                   MOVEL     SAVKPJBU      KPJBU
070700021220     C                   ELSE
070800021220     C     Chk_COD       CHAIN     azagb01l
070900021220     C                   IF        %found(azagb01l)
071000021220     C                   EVAL      Chk_DES = agbDES
071100021220     C                   ELSE
071200021220     C                   EVAL      ERRL00 = '1'
071300021220     C                   EVAL      Chk_DES = *all'*'
071400021220     C                   ENDIF
071500021220     C                   ENDIF
071600021220     C*
071700021220     C                   ENDSR
071800021220     C*------------------------------------------------------------------------*
071900021220     C* CHKDA8 - CONTROLLA LA DATA (8)
072000021220     C*------------------------------------------------------------------------*
072100021220     C     CHKDA8        BEGSR
072200021220     C*
072300030123     C     *LIKE         DEFINE    VIDDA1        WDA8
072400030123     C     *LIKE         DEFINE    VIDDA1        DA8W
072500021220     C*
072600021220     C                   Z-ADD     WDA8          G08DAT
072700021220     C                   Z-ADD     *ZEROS        G08INV
072800021220     C                   MOVEL     '0'           G08ERR
072900021220     C                   CALL      'XSRDA8'
073000021220     C                   PARM                    WLBDA8
073100021220     C                   Z-ADD     G08DAT        WDA8
073200021220     C                   Z-ADD     G08INV        DA8W
073300021220     C     G08ERR        IFEQ      '1'
073400021220     C                   MOVEL     '1'           ERRL00
073500021220     C                   ENDIF
073600021220     C*
073700021220     C                   ENDSR
073800021219     C*--------------------------------------------------------------------------------------------*
073900021219     C* REPERISCE I DATI UTENTE
074000021219     C*--------------------------------------------------------------------------------------------*
074100021219     C     REPDATIUTE    BEGSR
074200021219     C*
074300021219     C* INIZIALIZZA VARIABILI DI WRK
074400021219     C                   CLEAR                   TIBS34DS
074500021219     C                   CLEAR                   AZUTEDS
074600150211     C                   CLEAR                   dute01
074700021219     C                   CLEAR                   DDATIUTE
074800021219     C*
074900021219     C     *DTAARA       DEFINE    §azute        azuteds
075000021219     C     *DTAARA       DEFINE    §datiute      ddatiute
075100021219     C                   IN(E)     *DTAARA
075200021219     C                   IF        %Error
075300021220     C                   EVAL      I34Tla = 'L'
075400021219     C                   CALL      'TIBS34R'
075500021219     C                   PARM                    Tibs34Ds
075600021219     C                   IN        *DTAARA
075700021219     C                   ENDIF
075800150211
075900150211     c                   Clear                   wabi
076000150211     c                   Clear                   dLat
076100150211
076200150211      * Verifica errori e autorità profilo
076300150211s   1c                   Select
076400150211      * se ho errori nei dati utente esco dal pgm
076500150211w   1c                   When      DutErr = 'E'
076600150211     c                   seton                                        lr
076700150211     c                   return
076800150211      * se non c'è l'abilitazione
076900150211      * --> se 1° livello, abilitazioni al terminal
077000150211      *     se 2° livello, abilitazioni al punto operativo
077100150211w   1c                   When      UteAut = *Blanks
077200150211if  2c                   If        DutLpo = '1'
077300150211     c                   Eval      wabi   = 'TP'
077400150211e   2c                   EndIf
077500150211if  2c                   If        DutLpo = '2'
077600150211     c                   Eval      wabi   = 'PO'
077700150211e   2c                   EndIf
077800150211if  2c                   If        DutLpo = 'S'
077900150211     c                   Eval      wabi   = 'AZ'
078000150211e   2c                   EndIf
078100150211      * carica le abilitazioni del profilo
078200150211x   1c                   Other
078300150211     c                   Movel     UteFaf        Dute01
078400150211     c                   If        §Utemgb <> *Blanks
078500150211     c                   Eval      wabi = §Utemgb
078600150211     c                   Else
078700150211     c                   Eval      wabi = UteAut
078800150211     c                   EndIf
078900150211e   1c                   EndSl
079000150211      * controllo se ok l'abilitazione dell'utente
079100150211     c                   Clear                   Tibs02ds
079200150211     c                   Eval      T02Mod = 'C'
079300150211     c                   Eval      T02Sif = knsif
079400150211     c                   Eval      T02Cod = 'LAT'
079500150211     c                   Movel(p)  wabi          T02Ke1
079600150211     c                   Call      'TIBS02R'
079700150211     c                   Parm                    kpjba
079800150211     c                   Parm                    Tibs02ds
079900150211if  1c                   If        T02Err = *Blanks
080000150211     c                   Eval      dLat = T02Uni
080100150211e   1c                   EndIf
080200150211      * errore o non abilitato (impossibile xchè controllato nel filtro)
080300150211if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
080400150211     c                   seton                                        lr
080500150211     c                   return
080600150211   x1c                   Else
080700150211      * Reperimento dei P.O. gestibili dall'utente
080800150211     c                   clear                   TRUL31DS
080900150211     c                   clear                   TRUL31DS2
081000150211     c                   eval      I31abi = wabi
081100150211     c                   eval      I31cdi = DUTdis
081200150211     c                   eval      I31car = DUTare
081300150211     c                   eval      I31cpo = DUTpou
081400150211     c                   call      'TRUL31R'
081500150211     c                   parm                    KPJBA
081600150211     c                   parm                    TRUL31DS
081700150211     c                   parm                    TRUL31DS2
081800150211if  2c                   if        O31pog > *zeros
081900150211     c                   movea     O31pog        $pog
082000150211     c                   movea     O31arg        $arg
082100150211     c                   movea     O31dig        $dig
082200150211x   2c                   else
082300150211     c* impossibile perchè controllato nel filtrp
082400150211     c                   seton                                        lr
082500150211     c                   return
082600150211e   2c                   endif
082700150211    1c                   endif
082800150408     c                   if        dutpou = 046
082900150408     c                   seton                                        66
083000150408     c                   endif
083100021219     C*
083200021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
083300021219     C                   MOVEL(P)  RSUT          DSFIRS           20
083400021219     C*
083500021219     C                   ENDSR
083600030123     C*------------------------------------------------------------------------*
083700030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
083800030123     C*------------------------------------------------------------------------*
083900030123     C     VIDPAR        BEGSR
084000030123     C*
084100030123     C                   Z-ADD     DA1VID        PARDA1
084200030123     C                   MOVEL     VIDTP1        PARTP1
084300030305     C                   IF        VIDTP1 = 'C'
084400030305     C                   MOVEL     VIDSO1K       PARSO1
084500030305     C                   MOVEL     VIDPO1K       PARPO1
084600030305     C                   ENDIF
084700150430     C                   IF        VIDTP1 = 'F' and
084800150430     c                             vidso1f <> *blank and vidso1f <> *zeros
084900030612     C                   MOVEL     *all'0'       PARSO1
085000030612     C                   MOVE      VIDSO1F       PARSO1
085100030305     C                   ENDIF
085200030123     C                   MOVEL     VIDCO1        PARCO1
085300150211     C                   z-add     VIDper        PARPer
085400150211     C                   MOVEL     VIDgia        PARgia
085500150218     C                   MOVEL     Vdet          PARdet
085600150402     C                   MOVE      Vfile         PARfile
085700150402     C                   MOVE      Vpulf         PARpulf
085800150408     C                   Z-ADD     DA1DAl        PARDAL
085900150408     C                   Z-ADD     DA1AL         PARAL
086000150413     c                   if        vidarea  > *zeros
086100150409     C                   MOVEL     Vidarea       PARarea
086200150413     c                   endif
086300150409     C                   MOVEL     Vidtsg        PARtsg
086400030123     C*
086500030123     C                   ENDSR
086600030123     C*------------------------------------------------------------------------*
086700030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
086800030123     C*------------------------------------------------------------------------*
086900030123     C     AZZVID        BEGSR
087000030123     C*
087100030124     C                   Z-ADD     WN8           VIDDA1
087200150408     C                   Z-ADD     0             VIDDAl
087300150408     C                   Z-ADD     0             VIDal
087400150211     C                   MOVEL     'C'           VIDTP1
087500030123     C                   MOVEL     *blanks       VIDSO1K
087600030123     C                   MOVEL     *blanks       VIDSO1F
087700030123     C                   MOVEL     *blanks       VIDCO1
087800150211     C                   MOVEL     ' '           VIDgia
087900150211     C                   Z-ADD     *zeros        VIDper
088000150413     c*                  if        *in66
088100150413     C*                  MOVE      'S'           Vfile
088200150413     C*                  MOVE      'S'           Vpulf
088300150413     c*                  else
088400150408     C                   MOVE      'N'           Vfile
088500150408     C                   MOVE      'N'           Vpulf
088600150413     c*                  endif
088700150211     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
088800150211     C                   SETOn                                        55
088900150211     C                   SETOn                                        56
089000030123     C*
089100030123     C                   ENDSR
089200030123     C*------------------------------------------------------------------------*
089300030123     C* *INZSR - OPERAZIONI INIZIALI
089400030123     C*------------------------------------------------------------------------*
089500030123     C     *INZSR        BEGSR
089600030123     C*
089700030123     C* RICEVIMENTO PARAMETRI
089800030123     C     *ENTRY        PLIST
089900030123     C                   PARM                    KPJBA
090000030618     C*---
090100030618     C* CHIAVI DI LETTURA
090200030618     C*---
090300030618     C* LETTURA tntbe01l - PARZIALE
090400030618     C     KEYTBEP       KLIST
090500030618     C                   KFLD                    tbeCOD
090600030618     C                   KFLD                    tbeKE1
090700030123     C*
090800150409     C     KTAB          KLIST
090900150409     C                   KFLD                    CODUT             1 0
091000150409     C                   KFLD                    COD               2
091100150409     C                   KFLD                    tblkey
091200150409     C                   Z-ADD     1             CODUT
091300150409      * tabelle
091400150211     C                   EXSR      REPDATIUTE
091500030123     C*
091600030123     C* REPERISCO DATA E ORA CORRENTI
091700030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
091800030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
091900030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
092000030123     C                   Z-ADD     WN8           G08DAT
092100030123     C                   Z-ADD     *ZEROS        G08INV
092200030123     C                   MOVEL     '0'           G08ERR
092300030123     C                   CALL      'XSRDA8'
092400030123     C                   PARM                    WLBDA8
092500030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
092600030123     C*
092700030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
092800030123     C     XMSGPA        PLIST
092900030123     C                   PARM                    DSMSMS
093000030123     C                   PARM                    DSMSPG
093100030123     C                   PARM                    DSMSKE
093200030123     C                   PARM                    DSMSER
093300030123     C*
093400030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
093500030123     C                   CALL      'XKEY'
093600030123     C                   PARM                    DSKEY
093700030123     C*
093800030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
093900030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
094000030123     C*
094100030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
094200030123     C*   -NOME PGM CHIAMANTE
094300030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
094400150211     C                   MOVEL     'TISIG14R'    DSMSPG
094500150211     C                   MOVEL     'TISIG14R'    MSGQUE
094600030123     C                   MOVEL     *BLANKS       MSGKEY
094700030123     C                   MOVEL     *BLANKS       DSMSMS
094800030123     C*
094900030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
095000030123     C                   Z-ADD     0             FUNGRU            5 0
095100030123     C*
095200030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
095300030123     C                   EXSR      FUNGES
095400030123     C*
095500030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
095600030123     C                   MOVEL     '0'           F12L00            1
095700030123     C                   MOVEL     '0'           F03L00            1
095800030123     C                   MOVEL     '0'           FINL00            1
095900030123     C                   MOVEL     '0'           ERRL00            1
096000030123     C                   MOVEL     '0'           FLFINE            1
096100030123     C*
096200030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
096300030123     C                   EXSR      AZZVID
096400030123     C*
096500030123     C                   ENDSR
096600030123     C*------------------------------------------------------------------------*
096700030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
096800030123     C*------------------------------------------------------------------------*
096900030123     C     FUNGES        BEGSR
097000030123     C*
097100030123     C* PULISCO CAMPI DI LAVORO
097200030123     C                   Z-ADD     0             W                 5 0
097300030123     C*
097400030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
097500030123     C* E CARICO TABELLA FUNZIONI ATTIVE
097600030123     C     1             DO        24            I
097700030123     C                   MOVEL     FUD(I)        FUDRIG
097800030123     C*
097900030123     C* PULIZIA FUNZIONI PRECEDENTI
098000030123     C                   MOVEL     *BLANKS       FUV(I)
098100030123     C*
098200030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
098300030123     C* (SALTA F24)
098400030123     C     FUDATT        IFEQ      'S'
098500030123     C     FUDFUN        ANDNE     '24'
098600030123     C                   ADD       1             W
098700030123     C                   MOVEL     FUDDES        FUV(W)
098800030123     C                   END
098900030123     C                   END                                                    DO WHILE
099000030123     C*
099100030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
099200030123     C* ALTRIMENTI DISATTIVO F24
099300030123     C     1             DO        24            I
099400030123     C                   MOVEL     FUD(I)        FUDRIG
099500030123     C*
099600030123     C     FUDFUN        IFEQ      '24'
099700030123     C     W             IFGT      6
099800030123     C                   MOVEL     'S'           FUDATT
099900030123     C                   ELSE
100000030123     C                   MOVEL     *BLANKS       FUDATT
100100030123     C                   END                                                    W GT 8
100200030123     C                   MOVEL     FUDRIG        FUD(I)
100300030123     C                   END
100400030123     C                   END                                                    DO WHILE
100500030123     C*
100600030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
100700030123     C     W             DIV       6             I
100800030123     C                   MVR                     Z
100900030123     C*
101000030123     C     Z             IFNE      0
101100030123     C     I             ADD       1             Z
101200030123     C                   ELSE
101300030123     C                   Z-ADD     I             Z                 5 0
101400030123     C                   END
101500030123     C*
101600030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
101700030123     C                   ADD       1             FUNGRU
101800030123     C     FUNGRU        IFGT      Z
101900030123     C     FUNGRU        ORLT      1
102000030123     C                   Z-ADD     1             FUNGRU
102100030123     C                   END
102200030123     C*
102300030123     C* PRIMO GRUPPO DI 6 FUNZIONI
102400030123     C     FUNGRU        IFEQ      1
102500030123     C                   MOVEL     FUV(1)        FUN001
102600030123     C                   MOVEL     FUV(2)        FUN002
102700030123     C                   MOVEL     FUV(3)        FUN003
102800030123     C                   MOVEL     FUV(4)        FUN004
102900030123     C                   MOVEL     FUV(5)        FUN005
103000030123     C                   MOVEL     FUV(6)        FUN006
103100030123     C                   END
103200030123     C*
103300030123     C* SECONDO GRUPPO DI 6 FUNZIONI
103400030123     C     FUNGRU        IFEQ      2
103500030123     C                   MOVEL     FUV(7)        FUN001
103600030123     C                   MOVEL     FUV(8)        FUN002
103700030123     C                   MOVEL     FUV(9)        FUN003
103800030123     C                   MOVEL     FUV(10)       FUN004
103900030123     C                   MOVEL     FUV(11)       FUN005
104000030123     C                   MOVEL     FUV(12)       FUN006
104100030123     C                   END
104200030123     C*
104300030123     C* TERZO GRUPPO DI 6 FUNZIONI
104400030123     C     FUNGRU        IFEQ      3
104500030123     C                   MOVEL     FUV(13)       FUN001
104600030123     C                   MOVEL     FUV(14)       FUN002
104700030123     C                   MOVEL     FUV(15)       FUN003
104800030123     C                   MOVEL     FUV(16)       FUN004
104900030123     C                   MOVEL     FUV(17)       FUN005
105000030123     C                   MOVEL     FUV(18)       FUN006
105100030123     C                   END
105200030123     C*
105300030123     C* QUARTO GRUPPO DI 6 FUNZIONI
105400030123     C     FUNGRU        IFEQ      4
105500030123     C                   MOVEL     FUV(19)       FUN001
105600030123     C                   MOVEL     FUV(20)       FUN002
105700030123     C                   MOVEL     FUV(21)       FUN003
105800030123     C                   MOVEL     FUV(22)       FUN004
105900030123     C                   MOVEL     *BLANKS       FUN005
106000030123     C                   MOVEL     *BLANKS       FUN006
106100030123     C                   END
106200030123     C*
106300030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
106400030123     C                   MOVEL     *BLANKS       FUN007
106500030123     C                   MOVEL     *BLANKS       FUN008
106600030123     C*
106700030123     C     1             DO        24            I
106800030123     C                   MOVEL     FUD(I)        FUDRIG
106900030123     C     FUDFUN        IFEQ      '24'
107000030123     C     FUDATT        ANDEQ     'S'
107100030123     C                   MOVEL     FUDDES        FUN008
107200030123     C                   END
107300030123     C                   END                                                    DO
107400030123     C                   ENDSR
107500030123     C*------------------------------------------------------------------------*
107600030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
107700030123     C*------------------------------------------------------------------------*
107800030123     C     FUNCON        BEGSR
107900030123     C*
108000030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
108100030123     C                   EXSR      FUNVAR
108200030123     C*
108300030123     C* PULISCO CAMPI DI LAVORO
108400030123     C                   Z-ADD     0             I                 5 0
108500030123     C                   Z-ADD     0             W                 5 0
108600030123     C*
108700030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
108800030123     C     FUNKEY        IFNE      *BLANKS
108900030123     C     1             DO        24            I
109000030123     C                   MOVEL     FUD(I)        FUDRIG
109100030123     C*
109200030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
109300030123     C     FUNKEY        IFEQ      FUDFUN
109400030123     C     FUDATT        ANDEQ     'S'
109500030123     C                   Z-ADD     I             W
109600030123     C                   END
109700030123     C                   END                                                    DO WHILE
109800030123     C*
109900030123     C* FUNZIONE NON ATTIVA (ERRORE)
110000030123     C     W             IFEQ      0
110100030123     C*
110200030123     C* EMISSIONE MESSAGGIO DI ERRORE
110300030123     C                   MOVEL     MSG(1)        DSMSMS
110400030123     C*
110500030123     C                   END                                                    W=0
110600030123     C                   END                                                    FUNKEY NE *BLANKS
110700030123     C*
110800030123     C* FUNZIONE ATTIVA (PROCEDI ...)
110900030123     C     W             IFNE      0
111000030123     C     FUNKEY        OREQ      *BLANKS
111100030123     C*
111200030123     C     FUNKEY        IFEQ      *BLANKS
111300030123     C                   EXSR      ENTGES
111400030123     C                   END
111500030123     C     FUNKEY        IFEQ      '03'
111600030123     C                   EXSR      F03GES
111700030123     C                   END
111800030123     C     FUNKEY        IFEQ      '06'
111900030123     C                   EXSR      F06GES
112000030123     C                   END
112100030123     C     FUNKEY        IFEQ      '12'
112200030123     C                   EXSR      F12GES
112300030123     C                   END
112400030123     C     FUNKEY        IFEQ      '24'
112500030123     C                   EXSR      F24GES
112600030123     C                   END
112700030123     C*
112800030123     C                   END
112900030123     C*
113000030123     C                   ENDSR
113100030123     C*------------------------------------------------------------------------*
113200030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
113300030123     C*------------------------------------------------------------------------*
113400030123     C     FUNVAR        BEGSR
113500030123     C*
113600030123     C* INIZIALIZZA VARIABILE -FUNKEY-
113700030123     C*
113800030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
113900030123     C     KEY           IFEQ      ENTER
114000030123     C                   MOVEL     *BLANKS       FUNKEY
114100030123     C                   END
114200030123     C     KEY           IFEQ      F1
114300030123     C                   MOVEL     '01'          FUNKEY
114400030123     C                   END
114500030123     C     KEY           IFEQ      F2
114600030123     C                   MOVEL     '02'          FUNKEY
114700030123     C                   END
114800030123     C     KEY           IFEQ      F3
114900030123     C                   MOVEL     '03'          FUNKEY
115000030123     C                   END
115100030123     C     KEY           IFEQ      F4
115200030123     C                   MOVEL     '04'          FUNKEY
115300030123     C                   END
115400030123     C     KEY           IFEQ      F5
115500030123     C                   MOVEL     '05'          FUNKEY
115600030123     C                   END
115700030123     C     KEY           IFEQ      F6
115800030123     C                   MOVEL     '06'          FUNKEY
115900030123     C                   END
116000030123     C     KEY           IFEQ      F7
116100030123     C                   MOVEL     '07'          FUNKEY
116200030123     C                   END
116300030123     C     KEY           IFEQ      F8
116400030123     C                   MOVEL     '08'          FUNKEY
116500030123     C                   END
116600030123     C     KEY           IFEQ      F9
116700030123     C                   MOVEL     '09'          FUNKEY
116800030123     C                   END
116900030123     C     KEY           IFEQ      F10
117000030123     C                   MOVEL     '10'          FUNKEY
117100030123     C                   END
117200030123     C     KEY           IFEQ      F11
117300030123     C                   MOVEL     '11'          FUNKEY
117400030123     C                   END
117500030123     C     KEY           IFEQ      F12
117600030123     C                   MOVEL     '12'          FUNKEY
117700030123     C                   END
117800030123     C     KEY           IFEQ      F13
117900030123     C                   MOVEL     '13'          FUNKEY
118000030123     C                   END
118100030123     C     KEY           IFEQ      F14
118200030123     C                   MOVEL     '14'          FUNKEY
118300030123     C                   END
118400030123     C     KEY           IFEQ      F15
118500030123     C                   MOVEL     '15'          FUNKEY
118600030123     C                   END
118700030123     C     KEY           IFEQ      F16
118800030123     C                   MOVEL     '16'          FUNKEY
118900030123     C                   END
119000030123     C     KEY           IFEQ      F17
119100030123     C                   MOVEL     '17'          FUNKEY
119200030123     C                   END
119300030123     C     KEY           IFEQ      F18
119400030123     C                   MOVEL     '18'          FUNKEY
119500030123     C                   END
119600030123     C     KEY           IFEQ      F19
119700030123     C                   MOVEL     '19'          FUNKEY
119800030123     C                   END
119900030123     C     KEY           IFEQ      F20
120000030123     C                   MOVEL     '20'          FUNKEY
120100030123     C                   END
120200030123     C     KEY           IFEQ      F21
120300030123     C                   MOVEL     '21'          FUNKEY
120400030123     C                   END
120500030123     C     KEY           IFEQ      F22
120600030123     C                   MOVEL     '22'          FUNKEY
120700030123     C                   END
120800030123     C     KEY           IFEQ      F23
120900030123     C                   MOVEL     '23'          FUNKEY
121000030123     C                   END
121100030123     C     KEY           IFEQ      F24
121200030123     C                   MOVEL     '24'          FUNKEY
121300030123     C                   END
121400030123     C     KEY           IFEQ      ROLLUP
121500030123     C                   MOVEL     'RU'          FUNKEY
121600030123     C                   END
121700030123     C     KEY           IFEQ      HELP
121800030123     C                   MOVEL     'HP'          FUNKEY
121900030123     C                   END
122000030123     C                   ENDSR
122100030123     C*------------------------------------------------------------------------*
122200030123     C* NOTE PER COMPILAZIONE SCHIERE
122300030123     C*------------------------------------------------------------------------*
122400030123     C* -MSG-
122500030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
122600030123     C* (NON TOCCARE MSG,1)
122700030123     C*
122800030123     C* -CMD-
122900030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
123000030123     C*
123100030123     C* -FUD-
123200030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
123300030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
123400030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
123500030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
123600030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
123700030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
123800030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
123900030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
124000030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
124100030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
124200030123     C*
124300030123     C*------------------------------------------------------------------------*
124400030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
124500150211Tasto funzionale non gestito.                                                  1
124600150408Le date del periodo richiesto non sono inserite correttamente                  2
124700150211Data errata.                                                                   3
124800150211Soggetto movimento errato.                                                     4
124900150211Articolo indicato errato.                                                      5
125000150211Campo obbligatorio.                                                            6
125100150220Soggetto indicato non in gestione all'utente.                                  7
125200150414Indicare o il soggetto cliente o il Fil. o area dei soggetti clienti.          8
125300150211Indicare il soggetto Fil.                                                      9
125400150219Se richiesto solo clienti eccedenti la giacenza, non è ammesso il dettaglio    10
125500150219Se richiesta % differenza saldo, non è ammesso il dettaglio                    11
125600150211Effettuare almeno una scelta.                                                  12
125700150219Il dettaglio è ammesso solo se richiesto un soggetto cliente                   13
125800150416Codice area selezionato non valido o profilo non abilitato                     14
125900150413Tipo gestione ammesso solo per soggetti CLIENTI                                15
126000150413Periodo da esaminare inseribile solo se richiesto dettaglio                    16
126100150414Scegliere o codice articolo o tipo gestione                                    17
126200150409                                                                               18
126300150409                                                                               19
126400150409                                                                               20
126500030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
126600030123                   -        (S/N)=N                        funz.01¬
126700030123                   -        (S/N)=N                        funz.02¬
126800030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
126900030123                   -        (S/N)=N                        funz.04¬
127000030123                   -        (S/N)=N                        funz.05¬
127100030123F06=Lancio         -        (S/N)=S                        funz.06¬
127200030123                   -        (S/N)=N                        funz.07¬
127300030123                   -        (S/N)=N                        funz.08¬
127400030123                   -        (S/N)=N                        funz.09¬
127500030123                   -        (S/N)=N                        funz.10¬
127600030123                   -        (S/N)=N                        funz.11¬
127700150211                   -        (S/N)=N                        funz.12¬
127800030123                   -        (S/N)=N                        funz.13¬
127900030123                   -        (S/N)=N                        funz.14¬
128000030123                   -        (S/N)=N                        funz.15¬
128100030123                   -        (S/N)=N                        funz.16¬
128200030123                   -        (S/N)=N                        funz.17¬
128300030123                   -        (S/N)=N                        funz.18¬
128400030123                   -        (S/N)=N                        funz.19¬
128500030123                   -        (S/N)=N                        funz.20¬
128600030123                   -        (S/N)=N                        funz.21¬
128700030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
128800030123                   -        (S/N)=N                        funz.23¬
128900030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
