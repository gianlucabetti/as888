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
003000150211     d* p.o. abilitati
003100150211     D $pog            s              3    dim(250)
003200150211     D $DIG            s              1    dim(20)
003300150211     D $ARG            s              3    dim(50)
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
006300021220     D*------------------
006400021220     D* DS REPERIMENTO DATI UTENTE
006500021220     D*-------------------
006600021220     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006700021220     D ACODS         E DS                  EXTNAME(CNACO00F)
006800021220     D INDDS         E DS                  EXTNAME(CNIND00F)
006900021220     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007000021220     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007100000407     D*------------------
007200000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
007300000407     D*------------------
007400000407     D WLBDA8          DS                  INZ
007500000407     D  G08DAT                 1      8  0
007600000407     D  G08INV                 9     16  0
007700000407     D  G08ERR                17     17
007800000407     D  G08TGI                18     22  0
007900030123     D*-------------------
008000030123     D* DS REPERIMENTO DATI UTENTE
008100030123     D*-------------------
008200030123     D TIBS34DS      E DS                                                       *Profili utente
008300030123     D DDATIUTE      E DS                                                       *Dati utente
008400030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008500150211     D dute01        E DS                                                       *Utenti
008600150211     d dLat          e ds
008700150211     d TIBS02DS      E DS
008800150211     d wabi            s                   like(UteAut)
008900030124     D*------------------
009000030124     D* DS CARICAMENTO P.O. GESTITI
009100030124     D*-------------------
009200150211     D TRUL31DS      E DS
009300150211     D TRUL31DS2     E DS
009400021219     D*------------------
009500021219     D* PASSAGGIO PARAMETRI
009600021219     D*------------------
009700030123     D PARAM           DS                  INZ
009800150211     D  PARDA1                        8s 0
009900030123     D  PARTP1                        1
010000030123     D  PARSO1                        8
010100030123     D  PARCO1                       15
010200030305     D  PARPO1                        3
010300150211     D  PARper                        3s 0
010400150211     D  PARgia                        1
010500150218     D  PARdet                        1
010600150216     D*------------------
010700150216     D* PASSAGGIO PARAMETRI TISIG10
010800150216     D*------------------
010900150216     D PARAM10         DS
011000150216     d  pop1                          3
011100150216     D  Pksu                          8
011200150216     D  Ptsc                          1
011300150216     D  Pmsg                         80
011400030123     D*------------------
011500030123     D* VARIABILI DI WRK
011600030123     D*------------------
011700030123     D Chk_SOG                        8A
011800030123     D Chk_COD                       15A
011900030123     D Chk_DES                       30A
012000030123     D SAVKPJBU                            LIKE(KPJBU)
012100070509
012200070509      * flag per forzare messaggio errore sul cliente
012300070509     d wforza          s              1    inz(*off)
012400070510      * campi di sav
012500070510     d savso1f         s                   like(vidso1f) inz
012600070510     d savso1k         s                   like(vidso1k) inz
012700070509
012800970526     C*------------------------------------------------------------------------*
012900970526     C* MAIN LINES
013000970526     C*------------------------------------------------------------------------*
013100970526     C*
013200970526     C* NON CI SONO ERRORI IN ENTRATA
013300021211     C     ERRL00        IFEQ      '0'
013400970526     C*
013500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013600021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013900970526     C*
014000970526     C* EMETTE FORMATO VIDEO
014100970526     C                   EXSR      BUTFMR
014200970526     C*
014300970526     C* GESTIONE TASTI FUNZIONALI
014400970526     C                   EXSR      FUNCON
014500021211     C                   ENDDO
014600970526     C*
014700021211     C                   ENDIF
014800021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
014900021211     C                   MOVEL     PARAM         KPJBU
015000970526     C* FINE PGM
015100970526     C                   SETON                                        LR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015400970526     C*------------------------------------------------------------------------*
015500970526     C     BUTFMR        BEGSR
015600970526     C*
015700970526     C* EMETTE LA TESTATA
015800030123     C                   WRITE     SIG9D01
015900970526     C*
016000970526     C* SPEDISCE EVENTUALE MESSAGGIO
016100970526     C                   EXSR      SNDMSG
016200970526     C*
016300970526     C* EMETTE LE FUNZIONI
016400030123     C                   WRITE     SIG9D02
016500970526     C*
016600970526     C* EMETTE IL VIDEO
016700030123     C                   EXFMT     SIG9D03
016800970526     C*
016900970526     C* REIMPOSTA FLAG DI ERRORE
017000970526     C                   MOVEL     '0'           ERRL00
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     SNDMSG        BEGSR
017700970526     C*
017800970526     C* PULISCE IL SFILE MESSAGGI
017900970526     C                   MOVEL     '1'           *IN40
018000030123     C                   WRITE     SIG9DMC
018100970526     C                   MOVEL     '0'           *IN40
018200970526     C*
018300970526     C* VISUALIZZA IL MESSAGGIO
018400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018500970526     C                   CALL      'XMSG'        XMSGPA
018600021211     C     DSMSER        IFEQ      *ZEROS
018700970526     C                   MOVEL     DSMSKE        MSGKEY
018800021211     C                   ENDIF
018900970526     C                   Z-ADD     1             MSGULT
019000030123     C                   WRITE     SIG9DMS
019100030123     C                   WRITE     SIG9DMC
019200970526     C*
019300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019400970526     C                   MOVEL     *BLANKS       DSMSMS
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800021219     C* ENTGES - GESTIONE ENTER -> INVIO
019900970526     C*------------------------------------------------------------------------*
020000970526     C     ENTGES        BEGSR
020100970526     C*
020200970526     C* CONTROLLA I CAMPI IMMESSI
020300970526     C                   EXSR      DECODI
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F03GES - GESTIONE F03 -> FINE
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F03GES        BEGSR
021000970526     C*
021100970526     C* IMPOSTA "PREMUTO F03"
021200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600030123     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F06GES        BEGSR
021900030123     C*
022000030123     C                   EXSR      DECODI
022100030123     C*
022200030123     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
022300030123     C     ERRL00        IFEQ      '0'
022400030123     C*
022500030123     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
022600150212     C                   MOVEL     'SIGC'        KCOAZ
022700030123     C                   EXSR      VIDPAR
022800030123     C                   MOVEL     PARAM         KPJBU
022900030618     C                   CALL      'BCH10'
023000030618 MOM C***                CALL      'TISIGSR'
023100030123     C                   PARM                    KPJBA
023200030123     C*
023300030123     C* IMPOSTO FLAG F03 DI LIVELLO PGM
023400030123     C                   MOVEL     '1'           F03L00
023500030123     C*
023600030123     C                   ENDIF
023700021211     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* F12GES - GESTIONE F12 -> RITORNO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     F12GES        BEGSR
024300970526     C*
024400970526     C* IMPOSTA "PREMUTO F12"
024500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025000970526     C*------------------------------------------------------------------------*
025100970526     C     F24GES        BEGSR
025200970526     C*
025300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700021219     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025800970526     C*------------------------------------------------------------------------*
025900970526     C     DECODI        BEGSR
026000030305     C*
026100000407     C                   MOVEL     '0'           ERRL00
026200000407     C*
026300000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
026400030514     C                   SETOFF                                       78
026500030305     C                   SETOFF                                       798182
026600150211     C                   SETOFF                                       85
026700030305     C*
026800030305     C* INIZIALIZZO I CAMPI DESCRIZIONE
026900030305     C                   EVAL      VIDDS1 = *blanks
027000030305     C                   EVAL      VIDPO1KD = *blanks
027100030123     C*--------------------
027200030123     C* CONTROLLO DATA SALDO AL
027300030123     C*--------------------
027400030123     C     ERRL00        IFEQ      '0'
027500030123     C                   IF        VIDDA1 <> *zeros
027600030123     C     *LIKE         DEFINE    VIDDA1        DA1VID
027700030123     C                   Z-ADD     VIDDA1        WDA8
027800030123     C                   Z-ADD     *ZEROS        DA8W
027900030123     C                   EXSR      CHKDA8
028000030123     C                   Z-ADD     WDA8          VIDDA1
028100030123     C                   Z-ADD     DA8W          DA1VID
028200030123     C     ERRL00        IFEQ      '1'
028300030123     C                   SETON                                        79
028400030124     C                   MOVEL     MSG(3)        DSMSMS
028500030123     C                   ENDIF
028600030123     C                   ENDIF
028700030123     C                   ENDIF
028800021219     C*--------------------
028900030123     C* SCELTA/CONTROLLO SOGGETTO
029000021219     C*--------------------
029100021219     C     ERRL00        IFEQ      '0'
029200021223     C* Decodifico il tipo di soggetto movimento
029300030123     C                   IF        VIDTP1 = 'C'
029400030305     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
029500030123     C                   SETON                                        55
029600030305     C                   SETON                                        56
029700021223     C                   ENDIF
029800030123     C                   IF        VIDTP1 = 'F'
029900150210     C                   EVAL      VIDDT1 = ' soggetto Fil. .........?:'
030000030123     C                   SETOFF                                       55
030100030305     C                   SETOFF                                       56
030200021223     C                   ENDIF
030300021223     C* Eseguo i controlli di validità soggetto movimento
030400030107     C                   IF        *IN55   =  *ON
030500021220     C*
030600030305     C                   IF        VIDTP1 = 'C'
030700030305     C                   IF        VIDSO1K <> *blanks AND
030800030305     C                             VIDSO1K <> *zeros
030900030305     C                   EVAL      Chk_DES =  *blanks
031000030305     C                   MOVEL     *zeros        Chk_SOG
031100030305     C                   MOVE      VIDSO1K       Chk_SOG
031200021220     C                   EXSR      REPKSC
031300030305     C                   IF        ERRL00  = '1'
031400030305     C                   SETON                                        81
031500030305     C                   MOVEL     MSG(4)        DSMSMS
031600030305     C                   ENDIF
031700030305     C                   MOVE(P)   Chk_SOG       VIDSO1K
031800030305     C                   EVAL      VIDDS1  = Chk_DES
031900030305     C                   ENDIF
032000030305     C                   IF        VIDPO1K <> *blanks AND
032100030305     C                             VIDPO1K <> *zeros
032200030305     C                   EVAL      Chk_DES =  *blanks
032300030305     C                   MOVEL     *zeros        Chk_SOG
032400030305     C                   MOVE      VIDPO1K       Chk_SOG
032500030305     C                   EXSR      REPPO
032600030305     C                   IF        ERRL00  = '1'
032700030305     C                   SETON                                        85
032800030305     C                   MOVEL     MSG(4)        DSMSMS
032900030305     C                   ENDIF
033000030305     C                   MOVE(P)   Chk_SOG       VIDPO1K
033100030305     C                   EVAL      VIDPO1KD = Chk_DES
033200021220     C                   ENDIF
033300030305     C                   ENDIF
033400021220     C*
033500030107     C                   ELSE
033600030305     C                   IF        VIDSO1F <> *blanks AND
033700030305     C                             VIDSO1F <> *zeros
033800030123     C                   IF        VIDTP1 = 'F'
033900030305     C                   EVAL      Chk_DES =  *blanks
034000030305     C                   MOVEL     *zeros        Chk_SOG
034100030305     C                   MOVE      VIDSO1F       Chk_SOG
034200030107     C                   EXSR      REPPO
034300030305     C                   MOVE(P)   Chk_SOG       VIDSO1F
034400030305     C                   EVAL      VIDDS1  = Chk_DES
034500030305     C                   IF        ERRL00  = '1'
034600030305     C                   SETON                                        81
034700030305     C                   MOVEL     MSG(4)        DSMSMS
034800030305     C                   ENDIF
034900030107     C                   ENDIF
035000030107     C*
035100030107     C                   ENDIF
035200030107     C                   ENDIF
035300030305     C                   ENDIF
035400021219     C*--------------------
035500021219     C* SCELTA/CONTROLLO ARTICOLO
035600021219     C*--------------------
035700021219     C     ERRL00        IFEQ      '0'
035800021223     C                   IF        VIDCO1 <> *blanks
035900021223     C                   EVAL      Chk_DES = *blanks
036000021220     C                   EVAL      Chk_COD = VIDCO1
036100021220     C                   EXSR      REPCOD
036200021220     C                   EVAL      VIDCO1 = Chk_COD
036300021220     C                   EVAL      VIDDC1 = Chk_DES
036400021220     C                   IF        ERRL00 = '1'
036500021220     C                   SETON                                        82
036600021220     C                   MOVEL     MSG(5)        DSMSMS
036700021220     C                   ENDIF
036800021219     C                   ENDIF
036900021223     C                   ENDIF
037000030305     C* SOGGETTI A ZERO SE SOGGETTI A BLANKS
037100030305     C                   IF        VIDSO1K = *blanks
037200030305     C                   EVAL      VIDSO1K = *zeros
037300030305     C                   ENDIF
037400030305     C                   IF        VIDPO1K = *blanks
037500030305     C                   EVAL      VIDPO1K = *zeros
037600030305     C                   ENDIF
037700030305     C* SOGGETTO PO SE TIPO SOGGETTO = F
037800030305     C                   IF        VIDTP1  = 'F'
037900030305     C                   IF        VIDSO1F = *zeros AND
038000030305     C                             DUTLPO <> 'S'
038100030305     C                   MOVEL     '1'           ERRL00
038200030305     C                   SETON                                        81
038300030305     C                   MOVEL     MSG(9)        DSMSMS
038400030305     C                   ENDIF
038500030305     C                   ENDIF
038600150219     C* dettaglio solo se scelto un soggetto cliente
038800150219     C                   IF        Vdet = 'S' and (
038801150219     C                             VIDSO1K =  *blanks or
038802150219     C                             VIDSO1K =  *zeros )
038900150218     C                   MOVEL     '1'           ERRL00
039000150219     C                   MOVEL     MSG(13)       DSMSMS
039100150218     C                   ENDIF
039101150219     C* dettaglio solo se non scelto "solo clienti con giacenza" perchè
039102150219     c* il ctr è a totale cliente
039103150220     C*                  IF        Vdet = 'S' and vidgia = 'S'
039104150220     C*                  MOVEL     '1'           ERRL00
039105150220     C*                  MOVEL     MSG(10)       DSMSMS
039106150220     C*                  ENDIF
039107150219     C* dettaglio solo se non scelto "% eccedenza" perchè il ctr
039108150219     c* è a totale cliente
039109150220     C*                  IF        Vdet = 'S' and vidper <> 0
039110150220     C*                  MOVEL     '1'           ERRL00
039111150220     C*                  MOVEL     MSG(11)       DSMSMS
039112150220     C*                  ENDIF
039200030305     C* O SOGGETTO CLIENTE O PO CLIENTI SE TIPO SOGGETTO = C
039300030305     C                   IF        VIDTP1  = 'C'
039400030305     C                   IF        DUTLPO <> 'S'
039500030305     C                   IF        (VIDSO1K <> *zeros AND VIDPO1K <> *zeros) OR
039600030305     C                             (VIDSO1K =  *zeros AND VIDPO1K =  *zeros)
039700030305     C                   MOVEL     '1'           ERRL00
039800030305     C                   SETON                                        8185
039900030305     C                   MOVEL     MSG(8)        DSMSMS
040000030305     C                   ENDIF
040100030305     C                   ENDIF
040200030305     C                   ENDIF
040300030514     C* TIPO SOGGETTO DEVE ESSERE VALORIZZATO
040400030514     C                   IF        VIDTP1 = *blanks
040500030514     C                   MOVEL     '1'           ERRL00
040600030514     C                   SETON                                        78
040700030514     C                   MOVEL     MSG(6)        DSMSMS
040800030514     C                   ENDIF
040900030305     C*--------------------
041000030305     C* CONTROLLO AUTORIZZAZIONI UTENTE
041100030305     C*--------------------
041200030305     C     ERRL00        IFEQ      '0'
041300030305     C* I CONTROLLI SI RIFERISCONO SOLO A SOGGETTI DI TIPO "F" O DI TIPO "C"
041400030305     C                   IF        VIDTP1 = 'F'
042000030305     C                   MOVEL     VIDSO1F       WN3               3
042100030305     C                   SETOFF                                           28
042200150211     C     WN3           LOOKUP    $pog                                   28
042300030305     C     *IN28         IFEQ      *OFF
042301150220     C                   MOVEL     '1'           ERRL00
042500030305     C                   SETON                                        81
042600030305     C                   MOVEL     MSG(7)        DSMSMS
042800030305     C                   ENDIF
042900030305     C                   ENDIF
042901150220     c*
043000030305     C                   IF        VIDTP1 = 'C'
043600030305     C                   MOVEL     VIDSO1K       WA4               4
043700030305     C                   MOVE      WA4           WN3               3
043800030305     C                   SETOFF                                           28
043900150211     C     WN3           LOOKUP    $pog                                   28
044000030305     C     *IN28         IFEQ      *OFF
044001150220     C                   MOVEL     '1'           ERRL00
044200030305     C                   SETON                                        81
044300030305     C                   MOVEL     MSG(7)        DSMSMS
044500030305     C                   ENDIF
044601150220     C*
044603150220     C                   MOVE      vidpo1k       WN3               3
044604150220     C                   SETOFF                                           28
044605150220     C     WN3           LOOKUP    $pog                                   28
044606150220     C     *IN28         IFEQ      *OFF
044607150220     C                   MOVEL     '1'           ERRL00
044608150223     C                   SETON                                        85
044609150220     C                   MOVEL     MSG(7)        DSMSMS
044610150220     C                   ENDIF
044700030305     C                   ENDIF
044701150223     C                   ENDIF
044800030109     C*
044900970528     C                   ENDSR
045000021220     C*------------------------------------------------------------------------*
045100021220     C* REPKSC - CONTROLLA CODICE CLIENTE
045200021220     C*------------------------------------------------------------------------*
045300021220     C     REPKSC        BEGSR
045400021220     C*
045500021220     C* RICHIESTO ELENCO
045600021220     C                   SETOFF                                           28
045700021220     C     '?'           SCAN      Chk_SOG                                28
045800021220     C     *IN28         IFEQ      *ON
045900150216     C                   CLEAR                   PARAM10
046000150216     c                   movel     'P01'         pop1
046100150216     C                   MOVEL(P)  PARAM10       KPJBU
046200150216     c                   call      'TISIG10R'
046300150216     c                   parm                    kpjba
046400150216     C                   MOVEL     KPJBU         PARAM10
046500150216     C                   MOVE      PKSU          CHK_SOG
046600150216     C                   END
046700021220     C*
046800021220     C* SOLO CARATTERI NUMERICI
046900021220     C                   SETOFF                                       28
047000021220     C                   TESTN                   Chk_SOG              28
047100021220     C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
047200021220     C                   MOVEL     '1'           ERRL00
047300021220     C                   ELSE                                                   *E'NUMERO DA TESTN
047400021220     C                   MOVE      Chk_SOG       W001              1
047500021220     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
047600021220     C                   MOVEL     '1'           ERRL00
047700021220     C                   ELSE                                                   *NUMERO
047800021220     C*
047900021220     C* CONTROLLA ESISTENZA CLIENTE
048000021220     C                   EXSR      DECTIB
048100021220     C                   ENDIF
048200021220     C                   ENDIF
048300021220     C*
048400021220     C                   ENDSR
048500021220     C*------------------------------------------------------------------------*
048600021220     C* DECTIB - CONTROLLO/DECODIFICA CLIENTE
048700021220     C*------------------------------------------------------------------------*
048800021220     C     DECTIB        BEGSR
048900150216     C* CONTROLLO CHE SIA NELL'ANAGRAFICO DEI BANCALI
049000150216     C     CHK_SOG       CHAIN     AZFGB01L
049100150216     C                   IF        %FOUND(AZFGB01L) AND FGBANN = ' '
049200021220     C                   CLEAR                   BS69DS
049300021220     C                   CLEAR                   ACODS
049400021220     C                   MOVEL     KNSIF         I69SIF
049500021220     C                   MOVE(P)   Chk_SOG       I69KAC
049600021220     C                   CALL      'TIBS69R'
049700021220     C                   PARM                    BS69DS
049800021220     C                   PARM                    ACODS
049900021220     C                   PARM                    INDDS
050000021220     C                   PARM                    CLPDS
050100021220     C                   PARM                    CLSDS
050200150216     C                   ELSE
050300150216     C                   EVAL      O69ERR = '1'
050400150216     C                   END
050500021220     C     O69ERR        IFNE      '1'
050600021223     C                   EVAL      Chk_DES = acoRAG
050700021220     C                   ELSE
050800021220     C                   MOVEL     '1'           ERRL00
050900021220     C                   ENDIF
051000021220     C*
051100021220     C                   ENDSR
051200021220     C*------------------------------------------------------------------------*
051300021220     C* REPPO - CONTROLLA CODICE P.O.
051400021220     C*------------------------------------------------------------------------*
051500021220     C     REPPO         BEGSR
051600021220     C*
051700021220     C                   Z-ADD     *zeros        WrkPO_N           3 0
051800021220     C*
051900021220     C* RICHIESTO ELENCO
052000021220     C                   SETOFF                                           28
052100021220     C     '?'           SCAN      Chk_SOG                                28
052200021220     C     *IN28         IFEQ      *ON
052300021220     C*
052400021220     C* RICHIESTO ELENCO P.O.
052500021220     C                   CALL      'TNSD24R'
052600021220     C                   PARM                    §COD              3
052700021220     C                   PARM      'F'           §TIP              1
052800021220     C                   PARM                    §§DES            25
052900021220     C                   PARM                    §§PRA             3
053000021220     C                   MOVEL     *all'0'       Chk_SOG
053100021220     C                   MOVE      §COD          Chk_SOG
053200021220     C                   ELSE
053300021220     C                   MOVE      Chk_SOG       WrkPO_N
053400021220     C     WrkPO_N       CHAIN     azorg01l
053500021220     C                   IF        %found(azorg01l)
053600021220     C                   EVAL      Chk_DES = orgDES
053700021220     C                   ELSE
053800021220     C                   EVAL      ERRL00 = '1'
053900021220     C                   EVAL      Chk_DES = *all'*'
054000021220     C                   ENDIF
054100021220     C                   ENDIF
054200021220     C*
054300021220     C                   ENDSR
054400021220     C*------------------------------------------------------------------------*
054500021220     C* REPCOD - CONTROLLA CODICE ARTICOLO
054600021220     C*------------------------------------------------------------------------*
054700021220     C     REPCOD        BEGSR
054800021220     C*
054900021220     C* RICHIESTO ELENCO
055000021220     C                   SETOFF                                           28
055100021220     C     '?'           SCAN      Chk_COD                                28
055200021220     C     *IN28         IFEQ      *ON
055300021220     C*
055400021220     C* RICHIESTO ELENCO ARTICOLI
055500021220     C                   MOVEL     KPJBU         SAVKPJBU
055600021220     C                   MOVEL(P)  'P01'         KPJBU
055700021220     C                   CALL      'TISIG1R'
055800021220     C                   PARM                    KPJBA
055900021220     C                   EVAL      Chk_COD = %subst(KPJBU:4:15)
056000021220     C                   MOVEL     SAVKPJBU      KPJBU
056100021220     C                   ELSE
056200021220     C     Chk_COD       CHAIN     azagb01l
056300021220     C                   IF        %found(azagb01l)
056400021220     C                   EVAL      Chk_DES = agbDES
056500021220     C                   ELSE
056600021220     C                   EVAL      ERRL00 = '1'
056700021220     C                   EVAL      Chk_DES = *all'*'
056800021220     C                   ENDIF
056900021220     C                   ENDIF
057000021220     C*
057100021220     C                   ENDSR
057200021220     C*------------------------------------------------------------------------*
057300021220     C* CHKDA8 - CONTROLLA LA DATA (8)
057400021220     C*------------------------------------------------------------------------*
057500021220     C     CHKDA8        BEGSR
057600021220     C*
057700030123     C     *LIKE         DEFINE    VIDDA1        WDA8
057800030123     C     *LIKE         DEFINE    VIDDA1        DA8W
057900021220     C*
058000021220     C                   Z-ADD     WDA8          G08DAT
058100021220     C                   Z-ADD     *ZEROS        G08INV
058200021220     C                   MOVEL     '0'           G08ERR
058300021220     C                   CALL      'XSRDA8'
058400021220     C                   PARM                    WLBDA8
058500021220     C                   Z-ADD     G08DAT        WDA8
058600021220     C                   Z-ADD     G08INV        DA8W
058700021220     C     G08ERR        IFEQ      '1'
058800021220     C                   MOVEL     '1'           ERRL00
058900021220     C                   ENDIF
059000021220     C*
059100021220     C                   ENDSR
059200021219     C*--------------------------------------------------------------------------------------------*
059300021219     C* REPERISCE I DATI UTENTE
059400021219     C*--------------------------------------------------------------------------------------------*
059500021219     C     REPDATIUTE    BEGSR
059600021219     C*
059700021219     C* INIZIALIZZA VARIABILI DI WRK
059800021219     C                   CLEAR                   TIBS34DS
059900021219     C                   CLEAR                   AZUTEDS
060000150211     C                   CLEAR                   dute01
060100021219     C                   CLEAR                   DDATIUTE
060200021219     C*
060300021219     C     *DTAARA       DEFINE    §azute        azuteds
060400021219     C     *DTAARA       DEFINE    §datiute      ddatiute
060500021219     C                   IN(E)     *DTAARA
060600021219     C                   IF        %Error
060700021220     C                   EVAL      I34Tla = 'L'
060800021219     C                   CALL      'TIBS34R'
060900021219     C                   PARM                    Tibs34Ds
061000021219     C                   IN        *DTAARA
061100021219     C                   ENDIF
061200150211
061300150211     c                   Clear                   wabi
061400150211     c                   Clear                   dLat
061500150211
061600150211      * Verifica errori e autorità profilo
061700150211s   1c                   Select
061800150211      * se ho errori nei dati utente esco dal pgm
061900150211w   1c                   When      DutErr = 'E'
062000150211     c                   seton                                        lr
062100150211     c                   return
062200150211      * se non c'è l'abilitazione
062300150211      * --> se 1° livello, abilitazioni al terminal
062400150211      *     se 2° livello, abilitazioni al punto operativo
062500150211w   1c                   When      UteAut = *Blanks
062600150211if  2c                   If        DutLpo = '1'
062700150211     c                   Eval      wabi   = 'TP'
062800150211e   2c                   EndIf
062900150211if  2c                   If        DutLpo = '2'
063000150211     c                   Eval      wabi   = 'PO'
063100150211e   2c                   EndIf
063200150211if  2c                   If        DutLpo = 'S'
063300150211     c                   Eval      wabi   = 'AZ'
063400150211e   2c                   EndIf
063500150211      * carica le abilitazioni del profilo
063600150211x   1c                   Other
063700150211     c                   Movel     UteFaf        Dute01
063800150211     c                   If        §Utemgb <> *Blanks
063900150211     c                   Eval      wabi = §Utemgb
064000150211     c                   Else
064100150211     c                   Eval      wabi = UteAut
064200150211     c                   EndIf
064300150211e   1c                   EndSl
064400150211      * controllo se ok l'abilitazione dell'utente
064500150211     c                   Clear                   Tibs02ds
064600150211     c                   Eval      T02Mod = 'C'
064700150211     c                   Eval      T02Sif = knsif
064800150211     c                   Eval      T02Cod = 'LAT'
064900150211     c                   Movel(p)  wabi          T02Ke1
065000150211     c                   Call      'TIBS02R'
065100150211     c                   Parm                    kpjba
065200150211     c                   Parm                    Tibs02ds
065300150211if  1c                   If        T02Err = *Blanks
065400150211     c                   Eval      dLat = T02Uni
065500150211e   1c                   EndIf
065600150211      * errore o non abilitato (impossibile xchè controllato nel filtro)
065700150211if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
065800150211     c                   seton                                        lr
065900150211     c                   return
066000150211   x1c                   Else
066100150211      * Reperimento dei P.O. gestibili dall'utente
066200150211     c                   clear                   TRUL31DS
066300150211     c                   clear                   TRUL31DS2
066400150211     c                   eval      I31abi = wabi
066500150211     c                   eval      I31cdi = DUTdis
066600150211     c                   eval      I31car = DUTare
066700150211     c                   eval      I31cpo = DUTpou
066800150211     c                   call      'TRUL31R'
066900150211     c                   parm                    KPJBA
067000150211     c                   parm                    TRUL31DS
067100150211     c                   parm                    TRUL31DS2
067200150211if  2c                   if        O31pog > *zeros
067300150211     c                   movea     O31pog        $pog
067400150211     c                   movea     O31arg        $arg
067500150211     c                   movea     O31dig        $dig
067600150211x   2c                   else
067700150211     c* impossibile perchè controllato nel filtrp
067800150211     c                   seton                                        lr
067900150211     c                   return
068000150211e   2c                   endif
068100150211    1c                   endif
068200021219     C*
068300021219     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068400021219     C                   MOVEL(P)  RSUT          DSFIRS           20
068500021219     C*
068600021219     C                   ENDSR
068700030123     C*------------------------------------------------------------------------*
068800030123     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
068900030123     C*------------------------------------------------------------------------*
069000030123     C     VIDPAR        BEGSR
069100030123     C*
069200030123     C                   Z-ADD     DA1VID        PARDA1
069300030123     C                   MOVEL     VIDTP1        PARTP1
069400030305     C                   IF        VIDTP1 = 'C'
069500030305     C                   MOVEL     VIDSO1K       PARSO1
069600030305     C                   MOVEL     VIDPO1K       PARPO1
069700030305     C                   ENDIF
069800030305     C                   IF        VIDTP1 = 'F'
069900030612     C                   MOVEL     *all'0'       PARSO1
070000030612     C                   MOVE      VIDSO1F       PARSO1
070100030305     C                   ENDIF
070200030123     C                   MOVEL     VIDCO1        PARCO1
070300150211     C                   z-add     VIDper        PARPer
070400150211     C                   MOVEL     VIDgia        PARgia
070500150218     C                   MOVEL     Vdet          PARdet
070600030123     C*
070700030123     C                   ENDSR
070800030123     C*------------------------------------------------------------------------*
070900030123     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
071000030123     C*------------------------------------------------------------------------*
071100030123     C     AZZVID        BEGSR
071200030123     C*
071300030124     C                   Z-ADD     WN8           VIDDA1
071400150211     C                   MOVEL     'C'           VIDTP1
071500030123     C                   MOVEL     *blanks       VIDSO1K
071600030123     C                   MOVEL     *blanks       VIDSO1F
071700030123     C                   MOVEL     *blanks       VIDCO1
071800150211     C                   MOVEL     ' '           VIDgia
071900150211     C                   Z-ADD     *zeros        VIDper
072000150211     C                   EVAL      VIDDT1 = ' o soggetto cliente ....?:'
072100150211     C                   SETOn                                        55
072200150211     C                   SETOn                                        56
072300030123     C*
072400030123     C                   ENDSR
072500030123     C*------------------------------------------------------------------------*
072600030123     C* *INZSR - OPERAZIONI INIZIALI
072700030123     C*------------------------------------------------------------------------*
072800030123     C     *INZSR        BEGSR
072900030123     C*
073000030123     C* RICEVIMENTO PARAMETRI
073100030123     C     *ENTRY        PLIST
073200030123     C                   PARM                    KPJBA
073300030618     C*---
073400030618     C* CHIAVI DI LETTURA
073500030618     C*---
073600030618     C* LETTURA tntbe01l - PARZIALE
073700030618     C     KEYTBEP       KLIST
073800030618     C                   KFLD                    tbeCOD
073900030618     C                   KFLD                    tbeKE1
074000030123     C*
074100030124     C* CARICAMENTO TABELLE
074200150211     C                   EXSR      REPDATIUTE
074300030123     C*
074400030123     C* REPERISCO DATA E ORA CORRENTI
074500030123     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
074600030123     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
074700030123     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
074800030123     C                   Z-ADD     WN8           G08DAT
074900030123     C                   Z-ADD     *ZEROS        G08INV
075000030123     C                   MOVEL     '0'           G08ERR
075100030123     C                   CALL      'XSRDA8'
075200030123     C                   PARM                    WLBDA8
075300030123     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
075400030123     C*
075500030123     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
075600030123     C     XMSGPA        PLIST
075700030123     C                   PARM                    DSMSMS
075800030123     C                   PARM                    DSMSPG
075900030123     C                   PARM                    DSMSKE
076000030123     C                   PARM                    DSMSER
076100030123     C*
076200030123     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
076300030123     C                   CALL      'XKEY'
076400030123     C                   PARM                    DSKEY
076500030123     C*
076600030123     C* DEFINISCE LE VARIABILI DI PROGRAMMA
076700030123     C     *LIKE         DEFINE    FUDFUN        FUNKEY
076800030123     C*
076900030123     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
077000030123     C*   -NOME PGM CHIAMANTE
077100030123     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
077200150211     C                   MOVEL     'TISIG14R'    DSMSPG
077300150211     C                   MOVEL     'TISIG14R'    MSGQUE
077400030123     C                   MOVEL     *BLANKS       MSGKEY
077500030123     C                   MOVEL     *BLANKS       DSMSMS
077600030123     C*
077700030123     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
077800030123     C                   Z-ADD     0             FUNGRU            5 0
077900030123     C*
078000030123     C* GESTIONE TASTI FUNZIONALI A VIDEO
078100030123     C                   EXSR      FUNGES
078200030123     C*
078300030123     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
078400030123     C                   MOVEL     '0'           F12L00            1
078500030123     C                   MOVEL     '0'           F03L00            1
078600030123     C                   MOVEL     '0'           FINL00            1
078700030123     C                   MOVEL     '0'           ERRL00            1
078800030123     C                   MOVEL     '0'           FLFINE            1
078900030123     C*
079000030123     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
079100030123     C                   EXSR      AZZVID
079200030123     C*
079300030123     C                   ENDSR
079400030123     C*------------------------------------------------------------------------*
079500030123     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
079600030123     C*------------------------------------------------------------------------*
079700030123     C     FUNGES        BEGSR
079800030123     C*
079900030123     C* PULISCO CAMPI DI LAVORO
080000030123     C                   Z-ADD     0             W                 5 0
080100030123     C*
080200030123     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
080300030123     C* E CARICO TABELLA FUNZIONI ATTIVE
080400030123     C     1             DO        24            I
080500030123     C                   MOVEL     FUD(I)        FUDRIG
080600030123     C*
080700030123     C* PULIZIA FUNZIONI PRECEDENTI
080800030123     C                   MOVEL     *BLANKS       FUV(I)
080900030123     C*
081000030123     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
081100030123     C* (SALTA F24)
081200030123     C     FUDATT        IFEQ      'S'
081300030123     C     FUDFUN        ANDNE     '24'
081400030123     C                   ADD       1             W
081500030123     C                   MOVEL     FUDDES        FUV(W)
081600030123     C                   END
081700030123     C                   END                                                    DO WHILE
081800030123     C*
081900030123     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
082000030123     C* ALTRIMENTI DISATTIVO F24
082100030123     C     1             DO        24            I
082200030123     C                   MOVEL     FUD(I)        FUDRIG
082300030123     C*
082400030123     C     FUDFUN        IFEQ      '24'
082500030123     C     W             IFGT      6
082600030123     C                   MOVEL     'S'           FUDATT
082700030123     C                   ELSE
082800030123     C                   MOVEL     *BLANKS       FUDATT
082900030123     C                   END                                                    W GT 8
083000030123     C                   MOVEL     FUDRIG        FUD(I)
083100030123     C                   END
083200030123     C                   END                                                    DO WHILE
083300030123     C*
083400030123     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
083500030123     C     W             DIV       6             I
083600030123     C                   MVR                     Z
083700030123     C*
083800030123     C     Z             IFNE      0
083900030123     C     I             ADD       1             Z
084000030123     C                   ELSE
084100030123     C                   Z-ADD     I             Z                 5 0
084200030123     C                   END
084300030123     C*
084400030123     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
084500030123     C                   ADD       1             FUNGRU
084600030123     C     FUNGRU        IFGT      Z
084700030123     C     FUNGRU        ORLT      1
084800030123     C                   Z-ADD     1             FUNGRU
084900030123     C                   END
085000030123     C*
085100030123     C* PRIMO GRUPPO DI 6 FUNZIONI
085200030123     C     FUNGRU        IFEQ      1
085300030123     C                   MOVEL     FUV(1)        FUN001
085400030123     C                   MOVEL     FUV(2)        FUN002
085500030123     C                   MOVEL     FUV(3)        FUN003
085600030123     C                   MOVEL     FUV(4)        FUN004
085700030123     C                   MOVEL     FUV(5)        FUN005
085800030123     C                   MOVEL     FUV(6)        FUN006
085900030123     C                   END
086000030123     C*
086100030123     C* SECONDO GRUPPO DI 6 FUNZIONI
086200030123     C     FUNGRU        IFEQ      2
086300030123     C                   MOVEL     FUV(7)        FUN001
086400030123     C                   MOVEL     FUV(8)        FUN002
086500030123     C                   MOVEL     FUV(9)        FUN003
086600030123     C                   MOVEL     FUV(10)       FUN004
086700030123     C                   MOVEL     FUV(11)       FUN005
086800030123     C                   MOVEL     FUV(12)       FUN006
086900030123     C                   END
087000030123     C*
087100030123     C* TERZO GRUPPO DI 6 FUNZIONI
087200030123     C     FUNGRU        IFEQ      3
087300030123     C                   MOVEL     FUV(13)       FUN001
087400030123     C                   MOVEL     FUV(14)       FUN002
087500030123     C                   MOVEL     FUV(15)       FUN003
087600030123     C                   MOVEL     FUV(16)       FUN004
087700030123     C                   MOVEL     FUV(17)       FUN005
087800030123     C                   MOVEL     FUV(18)       FUN006
087900030123     C                   END
088000030123     C*
088100030123     C* QUARTO GRUPPO DI 6 FUNZIONI
088200030123     C     FUNGRU        IFEQ      4
088300030123     C                   MOVEL     FUV(19)       FUN001
088400030123     C                   MOVEL     FUV(20)       FUN002
088500030123     C                   MOVEL     FUV(21)       FUN003
088600030123     C                   MOVEL     FUV(22)       FUN004
088700030123     C                   MOVEL     *BLANKS       FUN005
088800030123     C                   MOVEL     *BLANKS       FUN006
088900030123     C                   END
089000030123     C*
089100030123     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
089200030123     C                   MOVEL     *BLANKS       FUN007
089300030123     C                   MOVEL     *BLANKS       FUN008
089400030123     C*
089500030123     C     1             DO        24            I
089600030123     C                   MOVEL     FUD(I)        FUDRIG
089700030123     C     FUDFUN        IFEQ      '24'
089800030123     C     FUDATT        ANDEQ     'S'
089900030123     C                   MOVEL     FUDDES        FUN008
090000030123     C                   END
090100030123     C                   END                                                    DO
090200030123     C                   ENDSR
090300030123     C*------------------------------------------------------------------------*
090400030123     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
090500030123     C*------------------------------------------------------------------------*
090600030123     C     FUNCON        BEGSR
090700030123     C*
090800030123     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
090900030123     C                   EXSR      FUNVAR
091000030123     C*
091100030123     C* PULISCO CAMPI DI LAVORO
091200030123     C                   Z-ADD     0             I                 5 0
091300030123     C                   Z-ADD     0             W                 5 0
091400030123     C*
091500030123     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
091600030123     C     FUNKEY        IFNE      *BLANKS
091700030123     C     1             DO        24            I
091800030123     C                   MOVEL     FUD(I)        FUDRIG
091900030123     C*
092000030123     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
092100030123     C     FUNKEY        IFEQ      FUDFUN
092200030123     C     FUDATT        ANDEQ     'S'
092300030123     C                   Z-ADD     I             W
092400030123     C                   END
092500030123     C                   END                                                    DO WHILE
092600030123     C*
092700030123     C* FUNZIONE NON ATTIVA (ERRORE)
092800030123     C     W             IFEQ      0
092900030123     C*
093000030123     C* EMISSIONE MESSAGGIO DI ERRORE
093100030123     C                   MOVEL     MSG(1)        DSMSMS
093200030123     C*
093300030123     C                   END                                                    W=0
093400030123     C                   END                                                    FUNKEY NE *BLANKS
093500030123     C*
093600030123     C* FUNZIONE ATTIVA (PROCEDI ...)
093700030123     C     W             IFNE      0
093800030123     C     FUNKEY        OREQ      *BLANKS
093900030123     C*
094000030123     C     FUNKEY        IFEQ      *BLANKS
094100030123     C                   EXSR      ENTGES
094200030123     C                   END
094300030123     C     FUNKEY        IFEQ      '03'
094400030123     C                   EXSR      F03GES
094500030123     C                   END
094600030123     C     FUNKEY        IFEQ      '06'
094700030123     C                   EXSR      F06GES
094800030123     C                   END
094900030123     C     FUNKEY        IFEQ      '12'
095000030123     C                   EXSR      F12GES
095100030123     C                   END
095200030123     C     FUNKEY        IFEQ      '24'
095300030123     C                   EXSR      F24GES
095400030123     C                   END
095500030123     C*
095600030123     C                   END
095700030123     C*
095800030123     C                   ENDSR
095900030123     C*------------------------------------------------------------------------*
096000030123     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
096100030123     C*------------------------------------------------------------------------*
096200030123     C     FUNVAR        BEGSR
096300030123     C*
096400030123     C* INIZIALIZZA VARIABILE -FUNKEY-
096500030123     C*
096600030123     C* FUNKEY=*BLANKS SIGNIFICA ENTER
096700030123     C     KEY           IFEQ      ENTER
096800030123     C                   MOVEL     *BLANKS       FUNKEY
096900030123     C                   END
097000030123     C     KEY           IFEQ      F1
097100030123     C                   MOVEL     '01'          FUNKEY
097200030123     C                   END
097300030123     C     KEY           IFEQ      F2
097400030123     C                   MOVEL     '02'          FUNKEY
097500030123     C                   END
097600030123     C     KEY           IFEQ      F3
097700030123     C                   MOVEL     '03'          FUNKEY
097800030123     C                   END
097900030123     C     KEY           IFEQ      F4
098000030123     C                   MOVEL     '04'          FUNKEY
098100030123     C                   END
098200030123     C     KEY           IFEQ      F5
098300030123     C                   MOVEL     '05'          FUNKEY
098400030123     C                   END
098500030123     C     KEY           IFEQ      F6
098600030123     C                   MOVEL     '06'          FUNKEY
098700030123     C                   END
098800030123     C     KEY           IFEQ      F7
098900030123     C                   MOVEL     '07'          FUNKEY
099000030123     C                   END
099100030123     C     KEY           IFEQ      F8
099200030123     C                   MOVEL     '08'          FUNKEY
099300030123     C                   END
099400030123     C     KEY           IFEQ      F9
099500030123     C                   MOVEL     '09'          FUNKEY
099600030123     C                   END
099700030123     C     KEY           IFEQ      F10
099800030123     C                   MOVEL     '10'          FUNKEY
099900030123     C                   END
100000030123     C     KEY           IFEQ      F11
100100030123     C                   MOVEL     '11'          FUNKEY
100200030123     C                   END
100300030123     C     KEY           IFEQ      F12
100400030123     C                   MOVEL     '12'          FUNKEY
100500030123     C                   END
100600030123     C     KEY           IFEQ      F13
100700030123     C                   MOVEL     '13'          FUNKEY
100800030123     C                   END
100900030123     C     KEY           IFEQ      F14
101000030123     C                   MOVEL     '14'          FUNKEY
101100030123     C                   END
101200030123     C     KEY           IFEQ      F15
101300030123     C                   MOVEL     '15'          FUNKEY
101400030123     C                   END
101500030123     C     KEY           IFEQ      F16
101600030123     C                   MOVEL     '16'          FUNKEY
101700030123     C                   END
101800030123     C     KEY           IFEQ      F17
101900030123     C                   MOVEL     '17'          FUNKEY
102000030123     C                   END
102100030123     C     KEY           IFEQ      F18
102200030123     C                   MOVEL     '18'          FUNKEY
102300030123     C                   END
102400030123     C     KEY           IFEQ      F19
102500030123     C                   MOVEL     '19'          FUNKEY
102600030123     C                   END
102700030123     C     KEY           IFEQ      F20
102800030123     C                   MOVEL     '20'          FUNKEY
102900030123     C                   END
103000030123     C     KEY           IFEQ      F21
103100030123     C                   MOVEL     '21'          FUNKEY
103200030123     C                   END
103300030123     C     KEY           IFEQ      F22
103400030123     C                   MOVEL     '22'          FUNKEY
103500030123     C                   END
103600030123     C     KEY           IFEQ      F23
103700030123     C                   MOVEL     '23'          FUNKEY
103800030123     C                   END
103900030123     C     KEY           IFEQ      F24
104000030123     C                   MOVEL     '24'          FUNKEY
104100030123     C                   END
104200030123     C     KEY           IFEQ      ROLLUP
104300030123     C                   MOVEL     'RU'          FUNKEY
104400030123     C                   END
104500030123     C     KEY           IFEQ      HELP
104600030123     C                   MOVEL     'HP'          FUNKEY
104700030123     C                   END
104800030123     C                   ENDSR
104900030123     C*------------------------------------------------------------------------*
105000030123     C* NOTE PER COMPILAZIONE SCHIERE
105100030123     C*------------------------------------------------------------------------*
105200030123     C* -MSG-
105300030123     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
105400030123     C* (NON TOCCARE MSG,1)
105500030123     C*
105600030123     C* -CMD-
105700030123     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
105800030123     C*
105900030123     C* -FUD-
106000030123     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
106100030123     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
106200030123     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
106300030123     C*  . POSSONO ESSERE IN ORDINE SPARSO
106400030123     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
106500030123     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
106600030123     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
106700030123     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
106800030123     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
106900030123     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
107000030123     C*
107100030123     C*------------------------------------------------------------------------*
107200030123** ======== SCHIERA: MSG  (MESSAGGI)                       ================
107300150211Tasto funzionale non gestito.                                                  1
107400150211                                                                               2
107500150211Data errata.                                                                   3
107600150211Soggetto movimento errato.                                                     4
107700150211Articolo indicato errato.                                                      5
107800150211Campo obbligatorio.                                                            6
107900150220Soggetto indicato non in gestione all'utente.                                  7
108000150211Indicare o il soggetto cliente o il Fil. dei soggetti clienti.                 8
108100150211Indicare il soggetto Fil.                                                      9
108200150219Se richiesto solo clienti eccedenti la giacenza, non è ammesso il dettaglio    10
108300150219Se richiesta % differenza saldo, non è ammesso il dettaglio                    11
108400150211Effettuare almeno una scelta.                                                  12
108500150219Il dettaglio è ammesso solo se richiesto un soggetto cliente                   13
108600150211
108700150211
108800150211
108900150211
109000150211
109100150211
109200150211
109300030123** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
109400030123                   -        (S/N)=N                        funz.01¬
109500030123                   -        (S/N)=N                        funz.02¬
109600030123F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
109700030123                   -        (S/N)=N                        funz.04¬
109800030123                   -        (S/N)=N                        funz.05¬
109900030123F06=Lancio         -        (S/N)=S                        funz.06¬
110000030123                   -        (S/N)=N                        funz.07¬
110100030123                   -        (S/N)=N                        funz.08¬
110200030123                   -        (S/N)=N                        funz.09¬
110300030123                   -        (S/N)=N                        funz.10¬
110400030123                   -        (S/N)=N                        funz.11¬
110500150211                   -        (S/N)=N                        funz.12¬
110600030123                   -        (S/N)=N                        funz.13¬
110700030123                   -        (S/N)=N                        funz.14¬
110800030123                   -        (S/N)=N                        funz.15¬
110900030123                   -        (S/N)=N                        funz.16¬
111000030123                   -        (S/N)=N                        funz.17¬
111100030123                   -        (S/N)=N                        funz.18¬
111200030123                   -        (S/N)=N                        funz.19¬
111300030123                   -        (S/N)=N                        funz.20¬
111400030123                   -        (S/N)=N                        funz.21¬
111500030123F22=Gestione stampe-        (S/N)=N                        funz.22¬
111600030123                   -        (S/N)=N                        funz.23¬
111700030123F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
