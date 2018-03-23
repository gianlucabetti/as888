000100940701     H* FNLR01R *-----------------------------------------------------*
000200940701     H*              - GESTIONE FOGLI  VARI -                         *
000300930225     H*---------------------------------------------------------------*
000400930225     H DECEDIT('0,') DATEDIT(*DMY.)
000500940701     FFNLR01D   CF   E             WORKSTN
000600940701     FFNFVV01L  UF A E           K DISK
000700950104     FFNFVA02L  IF   E           K DISK
000800070112     FFNBRV01L  IF   E           K DISK
000900070112     FFNBRV02L  IF   E           K DISK
001000070112     F                                     RENAME(FnBRV000:FNBRV002)
001100911004     FTABEL00F  IF   E           K DISK
001200020910     FFNARB78L  IF   E           K DISK
001300911112     FAZORG01L  IF   E           K DISK
001400010314     FFNORM06L  IF   E           K DISK
001500020422     FTntbe01l  IF   E           K DISK
001600030701     fAzcln01l  if   e           k Disk
001700900515      *
001800940701     D TIN             S              1    DIM(40)                              TIPO INV
001900940701     D DIN             S             10    DIM(40)                              DESCR TIP.INV
002000900515      *
002100940701     D TES             S             34    DIM(1) CTDATA PERRCD(1)              TESTATE
002200020909     D ERR             S             78    DIM(10) CTDATA PERRCD(1)             TESTATE
002300000128     D W78             S              1    DIM(78)                              COMODO
002400920805     D DEC             S             12    DIM(6)                               DESCR CATEGOR
002500950206      *
002600950206     D L1              S              3  0 DIM(30)                              FIL GESTITE
002700900517      *
002800900514     D KPJBA         E DS
002900110103      *
003000110103      * - Parametri per richiamo al pgm di controllo profilo utenti
003100110103     d TIBS34ds      e ds
003200110103      * - Ds di riferimento al file esterno AzUte00F
003300110103     d AZUTEds       e ds                  ExtName(AzUte00F)
003400110103      * - Ds per dati organigramma
003500110103     d dDatiUte      e ds
003600900516      *
003700940802     D* DS STANDARD FOGLI VARI
003800940802     D DSLR01        E DS
003900940802     D*
004000941027     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
004100941027     D DSLV50        E DS                  EXTNAME(FNLV50DS)
004200010314     D TIBS02        E DS                  EXTNAME(TIBS02DS)
004300940802     D*
004400010314     D DFAR          E DS
004500010314     D  §FARDS       E                     EXTFLD(D§FARDES)
004600010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
004700010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
004800010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
004900010314     D  §FARST       E                     EXTFLD(D§FARSTP)
005000010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
005100010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
005200020221     D  §FARNO       E                     EXTFLD(D§FARNOO)
005300020221     D  §FARUV       E                     EXTFLD(D§FARUVA)
005400020221     D  §FARDV       E                     EXTFLD(D§FARDVA)
005500020304     D OG143P        E DS                  EXTNAME(OG143) PREFIX(P)
005600020304     D OG143A        E DS                  EXTNAME(OG143) PREFIX(A)
005700960429     D DS7AF         E DS
005800960429     D DS7N          E DS
005900920805     D  §7NDE2                 1     10
006000000512     D FVVDS         E DS                  EXTNAME(FNFVV00F)
006100020422     d ddgd          e ds
006200020909     d Trul33Ds      e ds
006300020910     d Fnlr6Ds       e ds
006400110103     d*
006500050512     d* DS del campo fvvspn - spuntatore
006600050512     d Dfvvspn       e ds
006700900515      *
006800940701     D***
006900940701     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007000940701     D***
007100940705     D WLBDAT          DS                  INZ
007200940701     D  G02DAT                 1      8  0
007300940701     D  G02INV                 9     16  0
007400940701     D  G02ERR                17     17
007500940701     D  G02TGI                18     22  0
007600020422      *
007700020422     d Wdata8          DS
007800020422     d  dadata                 1      8  0
007900020422     d  adata                  9     16  0
008000020422     d  GioLav                17     21  0
008100940701     D***
008200940701     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
008300940701     D***
008400940705     D WGIDAT          DS                  INZ
008500940701     D  GIODAT                 1      8  0
008600940701     D  GIOINV                 9     16  0
008700940701     D  GIOTGI                17     21  0
008800920805     D TBLKEY          DS
008900940712     D  W001A                  1      1
009000950112     D PARA04          DS
009100950112     D  P04NFV                 1      5  0
009200950112     D  P04LNP                 6      8  0
009300950112     D  P04LNA                 9     11  0
009400950112     D  P04NRS                12     13  0
009500950112     D  P04NCD                14     20  0
009600950112     D  P04NCA                21     27  0
009700950112     D  P04NPS                28     29  0
009800950112     D  P04NPG                30     30  0
009900950112     D  P04FLG                31     31
010000950112     D  P04PGM                32     38
010100950112     D  P04FGS                39     41  0
010200101202     d* bolla
010300101202     d  p04bolp               42     44  0
010400101202     d  p04bors               45     46  0
010500101202     d  p04bosp               47     53  0
010600101202     d  p04boan               54     57  0
010700101202     D  P04ERR                58     58
010800101202     d*
010900950206     D* DS PER TRUL06R - CARICAMENTO £X
011000950206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011100950206     D  LIN                    1     90  0
011200950206     D                                     DIM(30)                              FIL. COMODO
011300020910
011400020910     d wifp            s                   Like(Arbifp)
011500020917     d Wfgs            s                   Like(FvvFgs)
011600020917     d Wok             s              1    inz('0')
011700020917     d xx              s              2  0
011800030701
011900030701     d                 ds
012000030701     d  ClnPom                 1     31
012100030701     d  Pom                    1     31    Dim(31)
012200110103     ***d                 ds
012300110103     ***d  ClnMat                 1     31
012400110103     ***d  Mat                    1     31    Dim(31)
012500030701
012600030701     d ktfp            s                   Like(FvvFgs)
012700030701     d ktfa            s                   Like(FvvFgs)
012800030701     d kann            s                   Like(ClnAnn)
012900030701     d kmes            s                   Like(ClnMes)
013000030701     d wgiorno         s              2  0
013100030701     d wdtaeur         s               d   datfmt(*eur)
013200030701     d wdtaiso         s               d   datfmt(*iso)
013300030701     d wdtameno1       s              8  0
013400030701     d wdtapiu1        s              8  0
013500030917     d wdtiso1         s               d
013600030917     d wdtiso2         s               d
013700030701
013800940712     D***
013900940712     D* CONSTANTI
014000940712     D***
014100940712     D CINSER          C                   CONST(' Inserimento ')
014200940712     D CVARIA          C                   CONST(' Variazione  ')
014300940712     D CANNUL          C                   CONST('  Annullato  ')
014400940712     D CBLOCC          C                   CONST('  Bloccato   ')
014500900515      *---------------------------------------------------------------*
014600910318      * INDICATORI USATI
014700910318      *---------------------------------------------------------------*
014800940712      * 01    = PGM RICHIAMATO
014900921228      * 02    = FOGLIO INVENTARIO
015000110103      **** 03    = FOGLIO CONSEGNE
015100921228      * 04    = CAMBIO FILIALE IN GESTIONE PER CMD18
015200921228      * 05    = SONO UN "REMXXX"
015300940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
015400941027      * 07    = ESISTONO SPUNTE SUL FOGLIO
015500960429      * 11    = EMETTO TASTO FUNZIONALE F18 DI RIAPERTURA
015600960429      * 12    = EMETTO TASTO FUNZIONALE F1 DI CHIUSURA FV
015700950703      * 13    = UTILIZZATO PER CONTROLLARE CHE IL NUMERO DEL FOGLIO CHE
015800950703      *         SI VUOLE CREARE NON ESISTA GIA' IN FNFVV00F
015900031112      * 19    = Nasconde la richiesta del campo opzione ("1")
016000031112      *         (utilizzato dal pgm. FNLR02R, che utilizza FNLR01D)
016100940713      * 28    = EMETTO CAMPO MESSAGGI
016200910321      * 30/31 = DI COMODO
016300911129      * 38/51 = ERRORE
016400921228      * 52    = RECORD ANNULLATO
016500921228      * 53    = FOGLIO CHIUSO
016600961113      * 54/59 = ERRORE
016700940712      * 60    = RECORD GIA' VINCOLATO
016800960429      * 61    = FOGLIO BLOCCATO
016900060522      * 62/67 = ERRORE
017000921228      * 80    = IMMISSIONE   N80 = VARIAZIONE
017100921228      * 90    = ERRORE GENERICO
017200921228      * 98    = USATO IN INTERROGAZIONE
017300921228      * 99    = PROTEZIONE CAMPI
017400910318      *--------------------------------------------------------------*
017500000000     C     *ENTRY        PLIST
017600000000     C                   PARM                    KPJBA
017700940802     C                   MOVEL     KPJBU         DSLR01
017800900516      *
017900110103     C                   Z-ADD     1             CODUT             1 0
018000110103      *
018100110103      * Reperimento dati utente/aziendali
018200110103     c                   exsr      sr_DatiJob
018300940701     C* DESCRIZIONE AZIENDA
018400110103     C                   MOVEL     RSUT          V0CDSA
018500950206     C***
018600950206     C* CARICO TABELLA FILIALI GESTITE £1
018700950206     C***
018800950206     C                   CLEAR                   DSUL06
018900950206     C                   MOVE      '£1'          D06COD
019000950206     C                   MOVEL     SIMFEL        D06KEY
019100950206     C                   MOVEL     DSUL06        KPJBU
019200950206     C*
019300950206     C                   CALL      'TRUL06R'
019400950206     C                   PARM                    KPJBA
019500950206     C                   MOVEL     KPJBU         DSUL06
019600950206     C                   MOVEA     LIN           L1
019700900515      *---------------------------------------------------------------*
019800900515      * FLUSSO PRINCIPALE                                             *
019900900515      *---------------------------------------------------------------*
020000911004     C*
020100940701     C* CARICO TABELLE
020200940701     C                   EXSR      CARTAB
020300911004      *
020400900509     C     INIZIO        TAG
020500940701      * INIZIALIZZO VIDEATA
020600940701     C                   EXSR      INZ12
020700940701      *
020800940701      * 01 ON - PROGRAMMA RICHIAMATO
020900940802    1C     DLRRIC        IFEQ      'S'
021000910321     C                   SETON                                        01
021100940802     C                   MOVEL     DLRNFV        V0CNFV
021200940802     C                   MOVEL     DLRNPG        V0CNPG
021300941027     C                   MOVEL     DLRFGS        V0CFGS
021400940701    1C                   END
021500900524      *
021600900515     C     FMT00         TAG
021700940701      *
021800940701     C  N01              EXFMT     LR01D00
021900941027     C*
022000941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
022100941027     C                   SETOFF                                       4328
022200910318     C*
022300920806     C* CMD3  - FINE LAVORO
022400910318     C   KC              GOTO      FINE                                         F3=FINE
022500920806     C*
022600920806     C* CMD7  - INTERROGAZIONE FOGLI VARI
022700940701    2C   KG              DO
022800940802     C                   CLEAR                   DSLR01
022900920806     C*
023000940802     C                   MOVEL     DSLR01        KPJBU
023100940712     C                   CALL      'FNLR02R'
023200920806     C                   PARM                    KPJBA
023300940802     C                   MOVEL     KPJBU         DSLR01
023400940713     C*
023500920806     C                   GOTO      FMT00
023600940705     C                   ENDDO
023700920806     C*
023800920806     C* CMD18 - CAMBIO FILIALE
023900920806     C   KS              SETON                                        04
024000920806     C   KS              GOTO      FMT00
024100920709     C*
024200940701     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
024300940701     C   04              EXSR      CTRFGS
024400940701     C   04
024500940701     CAN 90              GOTO      FMT00
024600940701     C*
024700940701     C* CONTROLLO FORMATO 1
024800940701     C                   EXSR      CTRD00
024900940701     C   90              GOTO      FMT00
025000000512     C* CMD10 - IMMISSIONE IMPEDITA X NUMERO DI FOGLIO 8
025100000512     C     V0CNPG        IFEQ      '8'
025200000512     C                   SETON                                        9028
025300000512     C                   MOVEL     ERR(3)        V0CMSG
025400000512     C   90              GOTO      FMT00
025500000512     C                   END
025600910318     C*
025700910318     C* RIEMPIMENTO CAMPI PIU' DECODIFICA
025800910318     C*
025900910318     C                   EXSR      RIEMPI
026000910318     C*
026100900515     C     FMT01         TAG
026200900516      *
026300020422     C* SE DISTINTA NON POSSO VARIARE mai dal 23/04/02 *(SE PRESENTI BOLLE)*
026400020424     c                   setoff                                       97
026500020422     C     WNPG          IFEQ      4
026600020422     c     *in80         andeq     *off
026700020424     C                   SETON                                        97
026800020422     C                   END
026900020422      *
027000940701     C                   EXFMT     LR01D01
027100000128     C                   SETOFF                                       28
027200941027     C*
027300941027     C* CMD21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
027400941027     C     *INKV         IFEQ      *ON
027500950112     C                   CLEAR                   PARA04
027600950112     C                   Z-ADD     V0CNFV        P04NFV
027700950112     C                   Z-ADD     99            P04NRS
027800950112     C                   Z-ADD     99            P04NPS
027900950112     C                   MOVEL     V0CNPG        P04NPG
028000950112     C                   MOVEL     '1'           P04FLG
028100950112     C                   MOVEL     'FNLR01R'     P04PGM
028200950112     C                   Z-ADD     V0CFGS        P04FGS
028300950112     C                   MOVEL     PARA04        KPJBU
028400950112     C                   CALL      'FNLR04R'
028500950112     C                   PARM                    KPJBA
028600941027     C                   GOTO      FMT01
028700941027     C                   ENDIF
028800911030      *
028900911030      * CMD12 - RITORNO
029000940701     C     *INKL         IFEQ      *ON
029100940701     C* RILASCIO RECORD
029200940705     C  N80              UNLOCK    FNFVV01L
029300941027     C*
029400940701     C* VADO A INIZIO
029500940701     C  N01              GOTO      INIZIO
029600941027     C*
029700940701     C* SE RICHIAMATO VADO A FINE
029800941027     C   01              GOTO      FINE                                         RICH
029900940701     C                   ENDIF
030000911030      *
030100911030      *  CONTROLLI FORMATO1
030200940701     C                   EXSR      CTRD01
030300911030     C   90              GOTO      FMT01
030400911030      *
030500911112      * CMD1  - CHIUSURA IMP     SI ARRIVI
030600000718    1C   KAV1CSPG        IFEQ      'P'
030700001115     C     V1CSPG        OREQ      'I'
030800961113     C     V1CSPG        OREQ      'A'
030900940701     C     V0CNPG        OREQ      '5'
031000020304      *-------------------------------------------------------------------
031100020304      * FOGLIO IMA: PERMETTO LA CHIUSURA SOLTANTO SE LE SPUNTE HANNO
031200020304      *             1) TUTTE LNA CON NETWORK ESTERO
031300020304      *             2) TUTTE LNP POSTE
031400020304      *-------------------------------------------------------------------
031500000718    2C     V1CSPG        IFEQ      'A'
031600000128     C                   Z-ADD     V0CNFV        WNFV
031700070112     C     KBRV          CHAIN     FNBRV02L                           31
031800000718    3C     *IN31         DOWEQ     *OFF
031900000718     C*
032000000718    4C     BRVLNA        IFNE      SAVLNA
032100000128     C     BRVLNA        CHAIN     AZORG01L                           30
032200020304     C  N30              MOVEL     ORGDE3        OG143A
032300020304     C   30              CLEAR                   OG143A
032400000718     C                   MOVEL     BRVLNA        SAVLNA
032500000718    4C                   ENDIF
032600000718     C*
032700020304    4C     A§OGNTW       IFNE      'EEX'
032800020304    4C     A§OGNTW       ANDNE     'EUP'
032900020304    4C     A§OGNTW       ANDNE     'FED'
033000020304    4C     A§OGNTW       ANDNE     'DPD'
033100020304     C* CONTROLLO SE LNP POSTE
033200000718    5C     BRVLNP        IFNE      SAVLNP
033300000718     C     BRVLNP        CHAIN     AZORG01L                           30
033400020304     C  N30              MOVEL     ORGDE3        OG143P
033500020304     C   30              CLEAR                   OG143P
033600000718     C                   MOVEL     BRVLNP        SAVLNP
033700000718    5C                   ENDIF
033800000718     C*
033900020304    5C     P§OGNTW       IFNE      'PPT'
034000000718     C                   SETON                                        5431
034100961113     C                   GOTO      FMT01
034200000718    5C                   ENDIF
034300000718    4C                   ENDIF
034400070112     C     KBRV          READE     FNBRV02L                               31
034500000718    3C                   ENDDO
034600000718    2C                   ENDIF
034700961113     C*
034800911112     C                   EXSR      REGIS
034900960429     C  N01              GOTO      INIZIO
035000911030     C                   GOTO      FINE                                         RICH
035100000718   X1C                   ELSE
035200911030     C                   SETON                                        51
035300911030     C                   GOTO      FMT01
035400000718   1 C                   END
035500940701      *
035600940701      * CMD6  - REGISTRA
035700940701     C   KF              DO
035800020917      * Prelevo il numero
035900020923     c                   if        *in80
036000020917     c                   ExSr      CrtNfv
036100020917     c   90              GoTo      Fmt01
036200020917     c                   ExFmt     Lr01W01
036300020923     c                   EndIf
036400940701     C                   EXSR      REGIS
036500940701     C  N01              GOTO      INIZIO
036600940701     C                   GOTO      FINE                                         RICH
036700940701     C                   END
036800900516      *
036900910318      * CMD5  - RIATTIVA
037000940701      * CMD16 - ANNULLA
037100940701      * CMD18 - RIAPERTURA IMP
037200940701     C     *INKE         IFEQ      *ON
037300940701     C     *INKQ         OREQ      *ON
037400940701     C     *INKS         OREQ      *ON
037500960429     C*
037600960626     C* PER FOGLIO IMA E FOGLIO IMG RIAPRO SOLO SE LA DATA FOGLIO
037700960429     C*  E' MAGGIORE DELLA DATA ESECUZIONE IDD
037800980402     C*  ANCHE PER FOGLIO ARRIVI, TANTO LE DATE DI ARRIVO REALE DEI
037900980402     C*  MEZZI POSSONO ESSERE MODIFICATE ANCHE SE IL FOGLIO E' CHIUSO
038000960429     C     *INKS         IFEQ      *ON
038100960429     C     FVVSPG        IFEQ      'A'
038200960626     C     FVVSPG        OREQ      'G'
038300980402     C     WNPG          OREQ      2
038400990512     C     FVVDFV        IFLE      §7ADID
038500960429     C                   SETON                                        6290
038600960429     C                   GOTO      FMT01
038700960429     C                   ENDIF
038800960429     C                   ENDIF
038900960429     C                   ENDIF
039000960429     C*
039100940701     C   KE              MOVE      ' '           FVVATB
039200940701     C   KQ              MOVE      '*'           FVVATB
039300940701     C   KS              MOVE      ' '           FVVFCF
039400940701     C                   UPDATE    FNFVV000
039500000713     C     FVVNPG        IFEQ      4
039600000713     C                   EXSR      ANCHE8
039700000713     C                   ENDIF
039800950607     C*
039900910321     C  N01              GOTO      INIZIO
040000910321     C                   GOTO      FINE                                         RICH
040100940701     C                   ENDIF
040200911030     C*
040300911030     C                   GOTO      FMT01
040400911030      *
040500900515      *
040600000000     C     FINE          TAG
040700110103      *
040800020909     c                   Clear                   Trul33Ds
040900020909     c                   Eval      I33Tla = 'C'
041000020909     c                   Movel(p)  Trul33Ds      Kpjbu
041100020909     c                   Call      'TRUL33R'
041200020909     c                   Parm                    Kpjba
041300110103      *
041400020910     c                   Clear                   Fnlr6Ds
041500020910     c                   Eval      D6Tla = 'C'
041600020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
041700020910     c                   Call      'FNLR6R'
041800020910     c                   Parm                    Kpjba
041900900529      *
042000000000     C                   SETON                                        LR
042100000713      *-----------------------------------------------------***********
042200000713      * ALLINEA FOGLIO 8 SE È STATO AGGIORNATO IL FOGLIO 4
042300000713      *-----------------------------------------------------***********
042400000713     C     ANCHE8        BEGSR
042500000713     C                   MOVE      FVVDS         FVVDS2
042600000713     C     KFVV8         CHAIN     FNFVV000                           80
042700000713     C     *IN80         IFEQ      *OFF
042800000713     C                   MOVE      FVVDS2        FVVDS
042900000713     C                   MOVE      8             FVVNPG
043000000713     C                   UPDATE    FNFVV000
043100000713     C                   MOVE      '4'           WNPG
043200000713     C     KFVV          SETLL     FNFVV000
043300000713     C                   MOVE      FVVDS2        FVVDS
043400000713     C                   END
043500000713      *
043600000713     C                   ENDSR
043700900515      *-----------------------------------------------------***********
043800940701      * INIZIALIZZA 1 E 2 VIDEATA                           *  INZ12  *
043900900515      *-----------------------------------------------------***********
044000940701     C     INZ12         BEGSR
044100900515      *
044200910318     C* PULISCO FORMATO1
044300910320     C                   SETOFF                                       995253
044400911030     C                   SETOFF                                       010212
044500960429     C                   SETOFF                                       0411
044600940705     C                   MOVEL     'FNLR01R'     V0CPGM
044700970602     C                   MOVEL     ' '           V0CNPG
044800940701     C                   Z-ADD     0             V0CNFV
044900940701     C                   Z-ADD     0             V0CDIN
045000940701     C                   Z-ADD     0             V0CDFI
045100940701     C                   MOVEL     *BLANKS       V0CTFV
045200910318     C* PULISCO FORMATO2
045300940701     C                   Z-ADD     UDATE8        V1CDFV
045400030701     c                   ExSr      Sr_MinMax
045500940701     C                   MOVEL     *BLANKS       V1CDSF
045600940701     C                   MOVEL     *BLANKS       V1CSPN
045700940701     C                   MOVEL     *BLANKS       V1CSPG
045800900515      *
045900900514     C                   ENDSR
046000030701
046100030701      *------------------------------------------------------------------------*
046200030701      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
046300030701      *------------------------------------------------------------------------*
046400030701     c     Sr_MinMax     BegSr
046500110103      *
046600030701     c                   Clear                   Ktfa
046700030701     c                   Z-Add     V0cFgs        Ktfp
046800030701     c     *eur          Move      V1cdfv        wdtaeur
046900030701      * meno 1 gg. lavorativo
047000030701     c                   Do        *Hival
047100030701     c                   Subdur    1:*d          wdtaeur
047200030701     c                   extrct    wdtaeur:*d    wgiorno
047300030701     c                   extrct    wdtaeur:*m    kmes
047400030701     c                   extrct    wdtaeur:*y    kann
047500030701     c     kazcln        Chain     Azcln01l
047600030701     c                   If        Not %Found(azcln01l)
047700030701     c                   Leave
047800030701     c                   EndIf
047900030701     c                   If        POM(wgiorno) = 'F'
048000030701     c                   Iter
048100030701     c                   EndIf
048200030701     c                   Leave
048300030701     c                   EndDo
048400030701     c                   Move      wdtaeur       wdtaiso
048500030701     c                   Move      wdtaiso       wdtameno1
048600110103      *
048700030701      * più 1 gg. lavorativo
048800030701     c     *eur          Move      V1cdfv        wdtaeur
048900030701     c                   Do        *Hival
049000030701     c                   Adddur    1:*d          wdtaeur
049100030701     c                   extrct    wdtaeur:*d    wgiorno
049200030701     c                   extrct    wdtaeur:*m    kmes
049300030701     c                   extrct    wdtaeur:*y    kann
049400030701     c     kazcln        Chain     Azcln01l
049500030701     c                   If        Not %Found(azcln01l)
049600030701     c                   Leave
049700030701     c                   EndIf
049800030701     c                   If        POM(wgiorno) = 'F'
049900030701     c                   Iter
050000030701     c                   EndIf
050100030701     c                   Leave
050200030701     c                   EndDo
050300030701     c                   Move      wdtaeur       wdtaiso
050400030701     c                   Move      wdtaiso       wdtapiu1
050500110103      *
050600030701     c                   EndSr
050700030701
050800910318      *-----------------------------------------------------***********
050900940713      * PRELEVO NUMERATORE FOGLI                            *  CRTNFV *
051000910318      *-----------------------------------------------------***********
051100910318     C     CRTNFV        BEGSR
051200940701     C                   SETOFF                                       90
051300110103      *
051400020909Do  1c                   Do        *Hival
051500020910      * Cerco il numero da staccare (aggiorna e se non trovato in aznum crea record)
051600020909     c                   Clear                   Trul33Ds
051700020909     c                   Eval      I33Cnu = §7nCnu
051800020909     c                   Eval      I33Po1 = V0cfgs
051900020909     c                   Eval      I33Num = 1
052000020909     c                   Movel(p)  Trul33Ds      Kpjbu
052100020909     c                   Call      'TRUL33R'
052200020909     c                   Parm                    Kpjba
052300020909     c                   Movel     Kpjbu         Trul33Ds
052400020909
052500020909S   2c                   Select
052600020909      * Numeratore non trovato
052700020909     c                   When      O33err =  01
052800040227     c                   Eval      V1cmsg = Err(6)
052900040227     c                   Eval      %subst(V1cmsg:31:10) = §7nDe2
053000020909     c                   Eval      *In28 = *On
053100020909     c                   Eval      *In90 = *On
053200020910     c                   Leave
053300020910      * Numeratore allocato
053400020909     c                   When      O33err =  02
053500040227     c                   Eval      V1cmsg = Err(7)
053600040227     c                   Eval      %subst(V1cmsg:67:10) = §7nDe2
053700020909     c                   Eval      *In28 = *On
053800020909     c                   Eval      *In90 = *On
053900020910     c                   Leave
054000110103      *
054100020909     c                   When      O33err <> *zeros
054200040227     c                   Eval      V1cmsg = O33Msg
054300020909     c                   Eval      *In28 = *On
054400020909     c                   Eval      *In90 = *On
054500020910     c                   Leave
054600020909    2c                   EndSl
054700110103      *
054800020917      * Imposto flag di appoggio
054900020917     c                   Eval      Wok = *On
055000110103      *
055100020909      * Controllo che non esista già un foglio con lo stesso numero
055200020910      * Per i fogli di tipo 4 richiamo fnlr6r
055300020919      * lo richiamo passando come tipo foglio 8 così controlla anche l'esistenza
055400020919      * del tipo 8
055500020917If  2c                   If        Wnpg = 4
055600020910     c                   Clear                   Fnlr6ds
055700020919     c                   Eval      D6Npg = 8
055800020910     c                   Eval      D6Fgs = V0cfgs
055900020910     c                   Z-Add     O33Nrf        D6Ndt
056000020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
056100020910     c                   Call      'FNLR6R'
056200020910     c                   Parm                    Kpjba
056300020910     c                   Movel     Kpjbu         Fnlr6Ds
056400020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
056500020910     c* come foglio o in una bolla
056600020910If  3c                   If        D6Flg <> 'N'
056700020910     c                   Iter
056800020910    3c                   EndIf
056900020917   x2c                   Else
057000020917      * Per tutti gli altri tipi foglio controllo su fnfvv
057100020910     c                   Z-Add     O33Nrf        V0cnfv
057200020909     c     Kfvv          setll     Fnfvv01l
057300020910If  3c                   If        %Equal(Fnfvv01l)
057400020909     c                   Iter
057500020910    3c                   EndIf
057600020917      * Per i fogli di tipo 3 devo controllare la sotto categoria
057700020917If  3c                   If        Wnpg = 3
057800020917      * se sto aprendo un foglio con sotto categoria 'P' (foglio IMP)
057900020917      * devo controllare che non esiste lo stesso numero x tutti i p.o. del terminal
058000020917If  4c                   If        V1cspg = 'P'
058100020917     c                   Z-Add     O33Nrf        V0cnfv
058200020917Do  5c                   Do        30            xx
058300020917     c                   If        L1(xx) = *Zeros
058400020917     c                   Leave
058500020917     c                   EndIf
058600020917     c                   Z-Add     L1(xx)        Wfgs
058700020917     c     Kfvv1         Setll     Fnfvv01l
058800020917If  6c                   If        %Equal(Fnfvv01l)
058900020917     c                   Eval      Wok = *Off
059000020917     c                   Leave
059100020917    6c                   EndIf
059200020917    5c                   EndDo
059300020917If  5c                   If        Wok = *Off
059400020917     c                   Iter
059500020917    5c                   EndIf
059600020917   x4c                   Else
059700020917      * se sto aprendo un foglio con sotto categoria <> 'P'
059800020917      * devo controllare che non esiste un foglio IMP con lo stesso numero
059900020917      * x tutti i p.o. del terminal
060000020917     c                   Z-Add     O33Nrf        V0cnfv
060100020917Do  5c                   Do        30            xx
060200020917     c                   If        L1(xx) = *Zeros
060300020917     c                   Leave
060400020917     c                   EndIf
060500020917     c                   Z-Add     L1(xx)        Wfgs
060600020917     c     Kfvv1         Setll     Fnfvv01l
060700020917Do  6c                   Do        *Hival
060800020917     c     Kfvv1         Reade     Fnfvv01l
060900020917If  7c                   If        %EoF(Fnfvv01l)
061000020917     c                   Leave
061100020917    7c                   EndIf
061200020917If  7c                   If        FvvSpg = 'P'
061300020917     c                   Eval      Wok = *Off
061400020917     c                   Leave
061500020917    7c                   EndIf
061600020917    6c                   EndDo
061700020917    5c                   EndDo
061800020917If  5c                   If        Wok = *Off
061900020917     c                   Iter
062000020917    5c                   EndIf
062100020917    4c                   EndIf
062200020917    3c                   EndIf
062300020917    2c                   EndIf
062400020909     c                   Z-Add     O33Nrf        V0cnfv
062500020909     c                   Leave
062600020909    1c                   EndDo
062700910318     C*
062800020910     C                   ENDSR
062900900515      *-----------------------------------------------------***********
063000940701      * CONTROLLI FORMATO 1                                 *  CTRD01 *
063100900515      *-----------------------------------------------------***********
063200940701     C     CTRD01        BEGSR
063300900515      *
063400000128     C                   SETOFF                                       9059
063500911003      *
063600911003      * TIPO INVENTARIO
063700911003      *
063800940701     C   02V1CSPG        IFEQ      *BLANKS
063900911004     C                   SETON                                        4890
064000940701     C                   GOTO      ENDCT1
064100911004     C                   ELSE
064200911004     C*
064300940701     C     V1CSPG        LOOKUP    TIN                                    30
064400911004     C  N30              SETON                                        4890
064500940701     C  N30              GOTO      ENDCT1
064600911004     C                   END
064700060522     c*
064800060522     c*Non si può cambiare sottocategoria folgio inventario se ci sono spunt
064900060522     c                   if        *in02  and not *in80 and wspu='S'
065000060522     c                             and v1cspg<>fvvspg
065100060522     C                   SETON                                        6790
065200060522     C                   GOTO      ENDCT1
065300060522     c                   endif
065400941027      *
065500941027      * CONTROLLO DATA FOGLIO
065600941027      *
065700941027     C                   Z-ADD     V1CDFV        G02DAT
065800941027     C                   MOVEL     *BLANK        G02ERR
065900941027     C                   CALL      'XSRDA8'
066000941027     C                   PARM                    WLBDAT
066100941027     C     G02ERR        IFEQ      '1'
066200941027     C                   SETON                                        40  90
066300941027     C                   GOTO      ENDCT1
066400941027     C                   END
066500941027     C*
066600941027     C                   Z-ADD     G02INV        WDFV
066700940706     C*
066800940706     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
066900940706     C                   Z-ADD     G02DAT        V1CDFV
067000940706     C*
067100940706     C* SE IL FOGLIO HA DELLE SPUNTE E' IMPOSSIBILE MODIFICARE LA DATA
067200940706     C     WSPU          IFEQ      'S'
067300940706     C     FVVDFV        ANDNE     WDFV
067400940706     C                   SETON                                        4990
067500940706     C                   GOTO      ENDCT1
067600940706     C                   ENDIF
067700970602     C* E' IMPOSSIBILE MODIFICARE LA DATA ANCHE SE E' FOGLIO ARRIVI
067800970602     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
067900970602     C     *IN80         IFEQ      *OFF
068000970602     C     WNPG          IFEQ      2
068100970602     C     FVVDFV        ANDNE     WDFV
068200970602     C     KFVV2         SETLL     FNFVA02L                               32
068300970602     C     *IN32         IFEQ      *ON
068400970602     C                   SETON                                        5090
068500970602     C                   GOTO      ENDCT1
068600970602     C                   END
068700970602     C                   END
068800970602     C                   END
068900020422     C* CONTROLLO DATA COMPRESA NEL RANGE non per le distinte fogli 4
069000020422     c                   if        wnpg <> 4
069100030701     c                   If        *Inkf and
069200030701     c                                       (wdfv < wdtameno1 or
069300030701     c                                        wdfv > wdtapiu1)
069400000128     C                   SETON                                        905928
069500030701     C   80              MOVEA     ERR(2)        W78
069600030701     c  n80              Movea     Err(8)        W78
069700030701     c                   Move      '01'          W002A             2
069800000128     C                   MOVEA     W002A         W78(46)
069900000128     C                   MOVEA     W78           V1CMSG
070000940701     C                   GOTO      ENDCT1
070100930225E   1C                   ENDIF
070200020422     c                   else
070300020422      *distinta controlli sui limiti particolari
070400020422     c                   if        *in80
070500020422     c                   if        wdfv < waamg
070600020422     c                   SETON                                        905928
070700020422     c                   MOVEL     err(4)        v1cmsg
070800020422     C                   GOTO      ENDct1
070900020422     c                   endif
071000020422      *controllo range fra le date di oggi e quella immessa
071100020422     c                   eval      dadata = waamg
071200020422     c                   eval      adata  = wdfv
071300020422     c                   CALL      'XSRLAV8'
071400020422     c                   PARM                    Wdata8
071500030917     c                   if        giolav = 0
071600030917     c                   clear                   giorni
071700030917     c                   move      waamg         wdtiso1
071800030917     c                   move      wdfv          wdtiso2
071900030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
072000030917     c                   if        giorni > 30
072100030917     c                   SETON                                        905928
072200030917     c                   MOVEL     err(5)        v1cmsg
072300030917     C                   GOTO      ENDct1
072400030917     c                   endif
072500030917     c                   endif
072600020422     c                   if        giolav > §dgddc
072700020422     c                   SETON                                        905928
072800020422     c                   MOVEL     err(5)        v1cmsg
072900020422     C                   GOTO      ENDct1
073000020422     c                   end
073100020422     c                   end
073200110103      *
073300020422     c                   end
073400910320      *
073500940701     C*
073600940701     C* RICHIESTO ANNULLAMENTO FOGLIO:
073700940701     C*
073800950919     C* SE ESISTONO SPUNTE IMPOSSIBILE ANNULLARE FOGLIO: SEMPRE!!
073900940701     C   KQWSPU          IFEQ      'S'
074000940701     C                   SETON                                        4190
074100940701     C                   GOTO      ENDCT1
074200950919     C                   ENDIF
074300940701     C*
074400940701     C* SE DISTINTA NON POSSO ANNULLARE SE PRESENTI BOLLE
074500941027     C   KQWNPG          IFEQ      4
074600020910     c                   Z-add     V0cfgs        Wifp
074700940701     C                   Z-ADD     V0CNFV        VIDNDC            7 0
074800020910     c     Karb78        Setll     Fnarb78l                               32
074900010314     C   32              SETON                                        5590
075000940701     C   32              GOTO      ENDCT1
075100010314     C* NON POSSO ANNULLARE ANCHE SE PRESENTI O.R.M.
075200010314     C     WORM          IFEQ      'S'
075300010314     C                   SETON                                        6590
075400010314     C                   GOTO      ENDCT1
075500010314     C                   ENDIF
075600940701     C                   END
075700980112     C* E' IMPOSSIBILE ANNULLARE SE E' FOGLIO ARRIVI
075800980112     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
075900980112     C   KQWNPG          IFEQ      2
076000980112     C     KFVV2         SETLL     FNFVA02L                               32
076100980112     C     *IN32         IFEQ      *ON
076200980112     C                   SETON                                        6490
076300980112     C                   GOTO      ENDCT1
076400980112     C                   END
076500980112     C                   END
076600900515      *
076700940701     C     ENDCT1        ENDSR
076800900515      *-----------------------------------------------------***********
076900900515      * REGISTRAZIONE RECORD                                *  REGIS  *
077000900515      *-----------------------------------------------------***********
077100900515     C     REGIS         BEGSR
077200900515      *
077300910318     C                   MOVEL     *BLANK        FVVATB
077400940701     C                   Z-ADD     V0CNFV        FVVNFV
077500940701     C                   Z-ADD     WDFV          FVVDFV
077600920806     C*
077700940701     C     V1CDSF        IFEQ      *BLANKS
077800940701     C                   MOVEL     *BLANKS       FVVDSF
077900950314     C* DESCRIZIONE INVENTARIO: SE NON C'E' PRENDO DA TIPO INVENTARIO
078000950320     C     *IN02         IFEQ      *ON
078100920709     C                   Z-ADD     1             I
078200940701     C     V1CSPG        LOOKUP    TIN(I)                                 30
078300940701     C   30              MOVEL     DIN(I)        FVVDSF
078400940701     C  N30              MOVEL     *BLANKS       FVVDSF
078500950320     C                   ELSE
078600950320     C* DESCRIZIONE FOGLIO: SE NON C'E' PRENDO DA CATEGORIA FOGLIO
078700950320     C                   MOVEL     V1DNPG        FVVDSF
078800950320     C                   END
078900920806     C                   ELSE
079000940701     C                   MOVEL     V1CDSF        FVVDSF
079100920709     C                   END
079200911129     C*
079300050512     c   80              clear                   dfvvspn
079400050512     C                   MOVEL     V1CSPN        §FVVSPN
079500050512     c                   movel     dfvvspn       fvvspn
079600940701     C                   MOVEL     V1CSPG        FVVSPG
079700911016     C                   MOVEL     *BLANKS       FVVFCF
079800911112     C   KA              MOVE      'S'           FVVFCF
079900940701     C   80              MOVEL     V0CNPG        FVVNPG
080000911016     C                   MOVEL     'S'           FVVFSC
080100950123     C                   Z-ADD     SIMFEL        FVVFLE
080200900515      *
080300020918      * In immissione foglio mi reimposto il p.o. in gestione xchè al momento dei controlli
080400020918      * sul numero staccato fvvfgs non è più il p.o. gestione del video impostato nella
080500020918      * routine riempi
080600020918     c                   Movel     V0cFgs        FvvFgs
080700900515      *
080800950320     C   80              WRITE     FNFVV000                                     IMMISSIONE
080900000512      *SCRITTURA FOGLIO DI TIPO 8 UGUALE A 4 PER SPUNTE RIENTRO
081000000512     C     *IN80         IFEQ      *ON
081100000512     C     FVVNPG        ANDEQ     4
081200000512     C                   MOVE      8             FVVNPG
081300000512     C                   WRITE     FNFVV000
081400000512     C                   END
081500911004      *
081600940701     C  N80              UPDATE    FNFVV000                                     VARIAZIONE
081700000512      *COPIA MODIFICHE DA FOGLIO 4 A FOGLIO 8
081800000512     C     *IN80         IFEQ      *OFF
081900000512     C     FVVNPG        ANDEQ     4
082000000512     C                   MOVE      FVVDS         FVVDS2
082100000512     C                   MOVE      '8'           WNPG
082200000512     C     KFVV          CHAIN     FNFVV000                           80
082300000512     C     *IN80         IFEQ      *OFF
082400000512     C                   MOVE      FVVDS2        FVVDS
082500000512     C                   MOVE      8             FVVNPG
082600000512     C                   UPDATE    FNFVV000
082700000512     C                   MOVE      '4'           WNPG
082800000512     C     KFVV          SETLL     FNFVV000
082900000515     C                   MOVE      FVVDS2        FVVDS
083000000512     C                   END
083100000512     C                   END
083200000512     C*
083300900515     C                   ENDSR
083400900515      *-----------------------------------------------------***********
083500900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
083600900515      *-----------------------------------------------------***********
083700900515     C     RIEMPI        BEGSR
083800940712     C* EMETTO LA DESCRIZIONE DELLO STATO FOGLIO-INS/VARIAZ/ANNULL/BLOC
083900940712     C                   SETON                                        06
084000941027     C                   SETOFF                                       07
084100940712     C*
084200940701     C                   CLEAR                   WSPU
084300010314     C                   CLEAR                   WORM
084400960429     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
084500960429     C* IDD
084600960429     C                   MOVEL     '7A'          COD
084700960429     C                   MOVEL(P)  V0CFGS        KEY
084800960429     C     KTAB2         CHAIN     TABEL                              31
084900960429     C  N31              MOVEL     TBLUNI        DS7AF
085000900515      *
085100911016     C* SE INVENTARIO EMETTO CAMPO DELLA SOTTOSPECE
085200940701     C     V0CNPG        IFEQ      '3'
085300911003     C                   SETON                                        02
085400960429     C* FOGLIO IMP
085500960429     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
085600911030     C  N80FVVSPG        IFEQ      'P'
085700960429     C                   SETON                                        1211
085800911003     C                   END
085900001115     C* FOGLIO IMI
086000001115     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
086100001115     C  N80FVVSPG        IFEQ      'I'
086200001115     C                   SETON                                        1211
086300001115     C                   END
086400960429     C* FOGLIO IMA
086500960429     C* EMETTO CMD DI RIAPERTURA
086600961113     C* EMETTO ANCHE COMANDO DI CHIUSURA MA SOLO SE TUTTE LE SPUNTE
086700961113     C*  CONTENUTE NELL'IMA HANNO DATA ARRIVO > 300
086800960429     C  N80FVVSPG        IFEQ      'A'
086900961113     C                   SETON                                        1112
087000960429     C                   ENDIF
087100960626     C* FOGLIO IMG
087200960626     C* EMETTO CMD DI RIAPERTURA
087300960626     C  N80FVVSPG        IFEQ      'G'
087400960626     C                   SETON                                        11
087500960626     C                   ENDIF
087600960429     C                   ENDIF
087700110103     ***C* 03 ON - CONSEGNE
087800110103     ***C     V0CNPG        IFEQ      '4'
087900110103     ***C                   SETON                                        03
088000110103     ***C                   END
088100920806     C*
088200940701     C*
088300940712      * S O L O   I N   V A R I A Z I O N E
088400940712     C     *IN80         IFEQ      *OFF
088500940712     C* DESCRIZIONE A VIDEO
088600940712     C                   MOVEL     CVARIA        V1DTFV
088700940712     C*
088800940712     C* SE ANNULLATO PROTEGGO CAMPI
088900940712     C     FVVATB        IFEQ      '*'
089000940712     C                   SETON                                        52  99
089100940712     C                   MOVEL     CANNUL        V1DTFV
089200940712     C                   END
089300940712     C*
089400940712     C* FOGLIO CHIUSO
089500940712     C     FVVFCF        IFEQ      'S'
089600940712     C                   SETON                                        53  99
089700940712     C                   END
089800940712     C* FOGLIO BLOCCATO
089900940712     C     FVVFCF        IFNE      'S'
090000940712     C     FVVFCF        ANDNE     ' '
090100940712     C                   MOVEL     CBLOCC        V1DTFV
090200940712     C                   ENDIF
090300940712     C*
090400960429     C* 12 E 11 ON - EMETTO F1 O F18 ANCHE PER FOGLIO ENTRATA
090500940712     C     V0CNPG        IFEQ      '5'
090600960429     C                   SETON                                        1211
090700960429     C                   ENDIF
090800960429     C*
090900960429     C* 11 ON - FACCIO RIAPRIRE IL FOGLIO IDA
091000960429     C*         SOLO SE NON HO GIA' FATTO L'IDD
091100960429     C     V0CNPG        IFEQ      '2'
091200960429     C                   SETON                                        11
091300960429     C                   ENDIF
091400941027     C*
091500940712     C                   MOVE      FVVDSF        V1CDSF                          DESCRIZIONE
091600050512     c                   clear                   dfvvspn
091700050512     C                   MOVEL     FVVSPN        dfvvspn                         SPUNTATORE
091800050512     C                   MOVEL     §fvvspn       V1CSPN                          SPUNTATORE
091900940712     C                   MOVEL     FVVSPG        V1CSPG                          SOTTOSPEC F
092000911004     C*
092100940712     C                   MOVE      FVVDFV        G02INV                          DATA
092200911004     C                   MOVE      *ZEROS        G02DAT
092300911004     C                   MOVEL     3             G02ERR
092400940706     C                   CALL      'XSRDA8'
092500911004     C                   PARM                    WLBDAT
092600940701     C                   Z-ADD     G02DAT        V1CDFV
092700030701     c                   ExSr      Sr_MinMax
092800940701     C*
092900940701     C* SEGNALO SE SUL FOGLIO ESISTONO SPUNTE
093000000128     C                   Z-ADD     V0CNFV        WNFV
093100070112     C     KBRV          SETLL     FNBRV000                               07
093200911004      *
093300940701     C* WSPU = "S"  --> ESISTONO SPUNTE
093400941027     C     *IN07         IFEQ      *ON
093500940701     C                   MOVEL     'S'           WSPU              1
093600910318     C                   END
093700010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
093800010314     C     WSPU          IFEQ      *BLANKS
093900010314     C     KORM          SETLL     FNORM06L
094000010314     C     KORM          READE     FNORM06L                               31
094100010314    3C     *IN31         DOWEQ     *OFF
094200010314     C                   CLEAR                   TIBS02
094300010314     C                   CLEAR                   DFAR
094400010314     C                   MOVEL     'C'           T02MOD
094500010314     C                   MOVEL     KNSIF         T02SIF
094600010314     C                   MOVEL     'FAR'         T02COD
094700010314     C                   MOVEL     ORMFAO        T02KE1
094800010314     C                   CALL      'TIBS02R'
094900010314     C                   PARM                    KPJBA
095000010314     C                   PARM                    TIBS02
095100010314     C     T02ERR        IFEQ      *BLANKS
095200010314     C                   MOVEL     T02UNI        DFAR
095300010314     C                   ENDIF
095400010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
095500010314     C* DISTINTA E' PRESENTE UN O.R.M.
095600010314    4C     §FARAF        IFNE      'S'
095700010314     C                   SETON                                        31
095800010314     C                   MOVEL     'S'           WORM              1
095900010314   X4C                   ELSE
096000010314     C     KORM          READE     FNORM06L                               31
096100010314    4C                   ENDIF
096200010314    3C                   ENDDO
096300010314    2C                   ENDIF
096400940712     C*
096500940712     C                   ELSE
096600940712     C*
096700940712     C* I M M I S S I O N E
096800940712     C*
096900940712     C* DESCRIZIONE A VIDEO
097000940712     C                   MOVEL     CINSER        V1DTFV
097100940712     C* FILIALE GESTIONE
097200940712     C                   MOVEL     V0CFGS        FVVFGS
097300940712     C                   ENDIF
097400910319     C*
097500910319     C                   ENDSR
097600911004      *-----------------------------------------------------***********
097700940701      * CARICO TABELLE                                      *  CARTAB *
097800911004      *-----------------------------------------------------***********
097900940701     C     CARTAB        BEGSR
098000940701     C***
098100940701     C* CARICO I TIPI INVENTARIO
098200940701     C***
098300911004     C                   MOVEL     *BLANKS       VIDSP1
098400911004     C                   MOVEL     *BLANKS       DESSP1
098500911004     C                   MOVEL     *BLANKS       VIDSP2
098600911004     C                   MOVEL     *BLANKS       DESSP2
098700911004     C                   MOVEL     *BLANKS       VIDSP3
098800911004     C                   MOVEL     *BLANKS       DESSP3
098900911004     C                   MOVEL     *BLANKS       VIDSP4
099000911004     C                   MOVEL     *BLANKS       DESSP4
099100911004     C                   MOVEL     *BLANKS       VIDSP5
099200911004     C                   MOVEL     *BLANKS       DESSP5
099300911004     C                   MOVEL     *BLANKS       VIDSP6
099400911004     C                   MOVEL     *BLANKS       DESSP6
099500911004     C*
099600911004     C                   MOVEL     '7B'          COD
099700911004     C                   Z-ADD     1             I                 2 0
099800911004     C     KTAB          SETLL     TABEL
099900911004     C     KTAB          READE     TABEL                                  30
100000911004     C*
100100911004     C     *IN30         DOWEQ     '0'
100200911004     C                   MOVEL     TBLKEY        TIN(I)
100300911004     C                   MOVEL     TBLUNI        DIN(I)
100400911004     C*
100500911004     C                   ADD       1             I
100600911004     C     KTAB          READE     TABEL                                  30
100700911004     C                   END
100800911004     C* EMETTO A VIDEO I PRIMI 6
100900911004     C     TIN(1)        IFNE      *BLANKS
101000911004     C                   MOVEL     TIN(1)        VIDSP1
101100911004     C                   MOVE      ' ='          VIDSP1
101200911004     C                   MOVEL     DIN(1)        DESSP1
101300911004     C     TIN(2)        IFNE      *BLANKS
101400911004     C                   MOVEL     TIN(2)        VIDSP2
101500911004     C                   MOVE      ' ='          VIDSP2
101600911004     C                   MOVEL     DIN(2)        DESSP2
101700911004     C     TIN(3)        IFNE      *BLANKS
101800911004     C                   MOVEL     TIN(3)        VIDSP3
101900911004     C                   MOVE      ' ='          VIDSP3
102000911004     C                   MOVEL     DIN(3)        DESSP3
102100911004     C     TIN(4)        IFNE      *BLANKS
102200911004     C                   MOVEL     TIN(4)        VIDSP4
102300911004     C                   MOVE      ' ='          VIDSP4
102400911004     C                   MOVEL     DIN(4)        DESSP4
102500911004     C     TIN(5)        IFNE      *BLANKS
102600911004     C                   MOVEL     TIN(5)        VIDSP5
102700911004     C                   MOVE      ' ='          VIDSP5
102800911004     C                   MOVEL     DIN(5)        DESSP5
102900911004     C     TIN(6)        IFNE      *BLANKS
103000911004     C                   MOVEL     TIN(6)        VIDSP6
103100911004     C                   MOVE      ' ='          VIDSP6
103200911004     C                   MOVEL     DIN(6)        DESSP6
103300911004     C                   END
103400911004     C                   END
103500911004     C                   END
103600911004     C                   END
103700911004     C                   END
103800911004     C                   END
103900940701     C***
104000920806     C* VEDO SE SONO SIMFEL O REMOTO
104100940701     C***
104200110103     c     dutLPO        ifeq      '2'
104300110103     c     dutLPO        oreq      *blanks
104400110103     c                   movel     dutPOU        v0cfgs
104500920806     C                   SETON                                        05
104600930209     C                   ELSE
104700940701     C                   MOVEL     SIMFEL        V0CFGS
104800920806     C                   END
104900941027     C*
105000940701     C     V0CFGS        CHAIN     AZORG                              31
105100940713     C  N31              MOVEL     ORGDES        V1DFGS
105200940701     C***
105300920806     C* CARICO CATEGORIE FOGLI
105400940701     C***
105500920806     C                   MOVEL     '7N'          COD
105600920806     C                   Z-ADD     0             I
105700920806     C     KTAB          SETLL     TABEL
105800920806     C     KTAB          READE     TABEL                                  31
105900940701     C*
106000920806     C     *IN31         DOWEQ     '0'
106100920806     C                   MOVEL     TBLUNI        DS7N
106200920806     C* SOLO SE ABILITATA PER FOGLI VARI
106300920806     C     §7NFFV        IFEQ      'S'
106400921228     C     §7NUTI        ANDNE     '9'
106500920806     C                   ADD       1             I
106600920806     C                   MOVEL     ' ='          DEC(I)
106700940712     C                   MOVEL     W001A         DEC(I)
106800920806     C                   MOVE      §7NDE2        DEC(I)
106900920806     C     I             IFEQ      6
107000920806     C                   SETON                                        31
107100920806     C                   END
107200920806     C                   END
107300920806     C*
107400920806     C  N31KTAB          READE     TABEL                                  31
107500920806     C                   END
107600920806     C*
107700940701     C* IMPOSTO LE DESCRIZIONI A VIDEO
107800920806     C     DEC(1)        IFNE      *BLANKS
107900920806     C                   MOVEL     DEC(1)        DESCA1
108000920806     C     DEC(2)        IFNE      *BLANKS
108100920806     C                   MOVEL     DEC(2)        DESCA2
108200920806     C     DEC(3)        IFNE      *BLANKS
108300920806     C                   MOVEL     DEC(3)        DESCA3
108400920806     C     DEC(4)        IFNE      *BLANKS
108500920806     C                   MOVEL     DEC(4)        DESCA4
108600920806     C     DEC(5)        IFNE      *BLANKS
108700920806     C                   MOVEL     DEC(5)        DESCA5
108800920806     C     DEC(6)        IFNE      *BLANKS
108900920806     C                   MOVEL     DEC(6)        DESCA6
109000920806     C                   END
109100920806     C                   END
109200920806     C                   END
109300920806     C                   END
109400920806     C                   END
109500920806     C                   END
109600940701     C***
109700940711     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
109800940701     C***
109900940711     C                   TIME                    W0120            12 0
110000940711     C                   MOVE      W0120         WDAT              6 0
110100940711     C*
110200940711     C                   Z-ADD     WDAT          G02DAT
110300940701     C                   MOVEL     *BLANK        G02ERR
110400940701     C                   CALL      'XSRDA8'
110500940701     C                   PARM                    WLBDAT
110600940711     C* DATA DEL GIORNO IN AAAAMMGG
110700940711     C                   Z-ADD     G02INV        WAAMG
110800940701     C*
110900940711     C* DATA DEL GIORNO IN GGMMAAAA
111000940701     C                   Z-ADD     G02DAT        UDATE8
111100920806     C*
111200940705     C                   MOVE      TES(1)        V0CTES
111300940701     C*
111400911004     C                   ENDSR
111500940701      *-----------------------------------------------------***********
111600940701      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
111700940701      *-----------------------------------------------------***********
111800940701     C     CTRFGS        BEGSR
111900940708     C                   SETOFF                                       90
112000950206     C     V0CFGS        LOOKUP    L1                                     30
112100950206     C     *IN30         IFEQ      *OFF
112200950206     C     V0CFGS        OREQ      0
112300950206     C                   MOVEL     ERR(1)        V0CMSG
112400941027     C                   SETON                                        439028
112500941027     C                   GOTO      ENDCTF
112600941027     C                   ENDIF
112700941027     C* DECODIFICA
112800941027     C     V0CFGS        CHAIN     AZORG                              31
112900941027     C  N31              MOVEL     ORGDES        V1DFGS
113000940701     C*
113100940701     C     ENDCTF        ENDSR
113200940701      *-----------------------------------------------------***********
113300940712      * CONTROLLO FORMATO 0                                    CTRD00
113400940701      *-----------------------------------------------------***********
113500940701     C     CTRD00        BEGSR
113600940701     C                   SETOFF                                       90
113700940701     C*
113800940701     C* CATEGORIA OBBLIGATORIA
113900940701     C*
114000940701     C     V0CNPG        IFEQ      ' '
114100940701     C                   SETON                                        4290
114200940701     C                   GOTO      ENDCT0
114300940701     C                   END
114400940701     C*
114500940701     C* RICERCA
114600940701     C     V0CNPG        IFEQ      '?'
114700940701     C                   MOVEL     CODUT         §KUT
114800940701     C                   MOVEL     '7N'          §COD
114900940701     C                   MOVE      ' '           V0CNPG
115000940701     C                   MOVEL     *BLANKS       §KEY
115100940701     C                   CALL      'X§TABER'
115200940701     C                   PARM                    §KUT              1 0
115300940701     C                   PARM                    §COD              2
115400940701     C                   PARM                    §KEY              8
115500940701     C                   PARM                    §DES             30
115600940701     C                   MOVEL     §KEY          V0CNPG
115700940701     C                   SETON                                        90
115800940701     C                   GOTO      ENDCT0
115900940701     C                   END
116000940701     C* CONTROLLO
116100940701     C                   MOVEL     '7N'          COD
116200940701     C                   MOVEL     *BLANKS       KEY
116300940701     C                   MOVEL     V0CNPG        KEY
116400940701     C* INESISTENTE
116500940701     C     KTAB2         CHAIN     TABEL                              42
116600940701     C* ANNULLATA
116700940701     C  N42TBLFLG        IFNE      ' '
116800940701     C                   SETON                                        42
116900940701     C                   ELSE
117000940701     C                   MOVEL     TBLUNI        DS7N
117100940701     C* NON E' CATEGORIA DI FOGLI VARI
117200940701     C     §7NFFV        IFNE      'S'
117300940701     C                   SETON                                        42
117400940701     C                   END
117500940701     C                   END
117600940701     C* 42 ON - ERRORE
117700940701     C   42              SETON                                        90
117800940701     C   42              GOTO      ENDCT0
117900940701     C*
118000940701     C* NON TUTTE LE CATEGORIE SONO UTILIZZABILI DAI 2 LIVELLI
118100940701     C     §7NUTI        IFEQ      '9'
118200940701     C                   SETON                                        5690
118300940701     C                   GOTO      ENDCT0
118400940701     C                   END
118500041026     c* categoria  4 non più utilizzabile da questo pgm
118600041026     c     v0cnpg        ifeq      '4'
118700041026     c                   seton                                        6390
118800041026     c                   goto      endct0
118900041026     c                   endif
119000041026     c
119100940701     C*
119200940701     C* FILIALE IN GESTIONE E' SIMFEL
119300940701     C     SIMFEL        IFEQ      V0CFGS
119400940701     C     §7NUTI        IFEQ      '2'
119500940701     C                   SETON                                        5790
119600940701     C                   GOTO      ENDCT0
119700940701     C                   END
119800940701     C*
119900940701     C                   ELSE
120000940701     C*
120100940701     C* FILIALE IN GESTIONE NON E' SIMFEL
120200940701     C     §7NUTI        IFEQ      '1'
120300940713     C                   SETON                                        5890
120400940713     C                   GOTO      ENDCT0
120500940701     C                   END
120600940701     C                   END
120700040227     c* Se §7NUITI='G' categoria solo in  gestione ma non in immissione
120800040227     c                   if        *inkj and §7nuti='G'
120900040227     C                   SETON                                        6690
121000040227     C                   GOTO      ENDCT0
121100040227     c                   endif
121200940701     C*
121300940701     C                   MOVEL     §7NDES        V1DNPG                         DESCRIZIONE
121400940701     C                   MOVEL     V0CNPG        WNPG                           IN CAMPO NUM
121500940701     C*
121600940713     C* FOGLIO
121700950320     C                   SETON                                        80
121800950320     C                   SETOFF                                       60
121900950320     C  NKJKFVV          CHAIN     FNFVV000                           8060
122000940701     C*
122100940701     C* RECORD GIA' VINCOLATO
122200940712     C   60              SETON                                        90
122300940712     C   60              GOTO      ENDCT0
122400950628     C* SE FOGLIO BLOCCATO --> NON POSSO MANUTENZIONARLO
122500950628     C     *IN80         IFEQ      *OFF
122600950628     C     FVVFCF        ANDNE     'S'
122700950628     C     FVVFCF        ANDNE     ' '
122800950628     C                   SETON                                        9061
122900950628     C                   UNLOCK    FNFVV01L
123000950628     C                   GOTO      ENDCT0
123100950628     C                   ENDIF
123200940701     C*
123300940701      *
123400940713      * NUMERO FOGLIO INESISTENTE IN VARIAZIONE
123500950320     C  NKJ*IN80         IFEQ      *ON
123600950320     C                   SETON                                        3890
123700950320     C                   GOTO      ENDCT0
123800940701     C                   ENDIF
123900940701     C*
124000940701     C     ENDCT0        ENDSR
124100940701      *-----------------------------------------------------***********
124200940701      * INZSR INIZIALE                                         INZSR
124300940701      *-----------------------------------------------------***********
124400940701     C     *INZSR        BEGSR
124500940701      ***
124600940701      * CHIAVI
124700940701      ***
124800940701     C     KFVV          KLIST
124900940701     C                   KFLD                    WNPG
125000940701     C                   KFLD                    V0CNFV
125100941027     C                   KFLD                    V0CFGS
125200020917     c     Kfvv1         Klist
125300020917     c                   Kfld                    Wnpg
125400020917     c                   Kfld                    V0cnfv
125500020917     c                   Kfld                    Wfgs
125600000713     C     KFVV8         KLIST
125700000713     C                   KFLD                    WNPG8
125800000713     C                   KFLD                    FVVNFV
125900000713     C                   KFLD                    FVVFGS
126000000713     C                   Z-ADD     8             WNPG8
126100000128     C* N.B. QUANDO IL NUMERO FOGLIO DIVENTERA' LUNGO 6 ANCHE PER I
126200000128     C*      FOGLI (FNFVV FNFVA) LA KLIST KBRV SI POTRÀ CANCELLARE
126300000128     C*      PERCHE' KFVV SI POTRA' UTILIZZARE AL SUO POSTO KFVV
126400000128     C     KBRV          KLIST
126500000128     C                   KFLD                    WNPG
126600000128     C                   KFLD                    WNFV
126700000128     C                   KFLD                    V0CFGS
126800950104     C     KFVV2         KLIST
126900950104     C                   KFLD                    WNPG
127000950104     C                   KFLD                    V0CNFV
127100020917     c                   Kfld                    V0cFgs
127200950607     C     KDST          KLIST
127300950607     C                   KFLD                    V0CNFV
127400950607     C                   KFLD                    V0CFGS
127500940701     C     KTAB          KLIST
127600940701     C                   KFLD                    CODUT
127700940701     C                   KFLD                    COD               2
127800940701     C     KTAB2         KLIST
127900940701     C                   KFLD                    CODUT
128000940701     C                   KFLD                    COD               2
128100940701     C                   KFLD                    KEY               8
128200010314     C* ACCESSO FNORM06L
128300010314     C     KORM          KLIST
128400010314     C                   KFLD                    V0CFGS
128500010314     C                   KFLD                    WNFV
128600020910      * Fnarb
128700020910     c     Karb78        Klist
128800020910     c                   Kfld                    Wifp
128900020910     c                   Kfld                    Vidndc
129000030701      * Azcln01l
129100030701     c     kazcln        Klist
129200030701     c                   Kfld                    ktfp
129300030701     c                   Kfld                    ktfa
129400030701     c                   Kfld                    kann
129500030701     c                   Kfld                    kmes
129600110103      *
129700940701     C* LIKE DEFN
129800940711     C* DATA DEL GIORNO A 8 AAAAMMGG
129900940711     C     *LIKE         DEFINE    FVVDFV        WAAMG
130000940711     C* DATA DEL GIORNO A 8 GGMMAAAA
130100000718     C     *LIKE         DEFINE    BRVLNA        SAVLNA
130200000718     C     *LIKE         DEFINE    BRVLNP        SAVLNP
130300000718     C     *LIKE         DEFINE    FVVDS         FVVDS2
130400940705     C     *LIKE         DEFINE    FVVDFV        UDATE8
130500940701     C     *LIKE         DEFINE    FVVDFV        WDFV
130600940701     C     *LIKE         DEFINE    FVVNPG        WNPG
130700000713     C     *LIKE         DEFINE    FVVNPG        WNPG8
130800000128     C     *LIKE         DEFINE    BRVNFV        WNFV
130900020422     C***
131000020422     C     KTBE          KLIST
131100020422     C                   KFLD                    KCOD1
131200020422     C                   KFLD                    KKE1
131300020422     C     *LIKE         DEFINE    TBEKE1        KKE1
131400020422     C     *LIKE         DEFINE    TBECOD        KCOD1
131500020422     C*  tabelle dgd x reperimento limiti chiusura spedizioni
131600020422     C                   MOVEL     'DGD'         KCOD1
131700020422     C                   MOVEL     *BLANKS       KKE1
131800020422     C                   MOVEL     '1'           KKE1
131900020422     C     KTBE          CHAIN     TNTBE01L                           31
132000020422     C     *IN31         IFEQ      '0'
132100020422     C                   MOVEL     TBEUNI        DDGD
132200020422     C                   END
132300940701     C                   ENDSR
132400110103
132500110103      *---------------------------------------------------------------*
132600110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
132700110103      *---------------------------------------------------------------*
132800110103     c     sr_DatiJob    BEGSR
132900110103      *
133000110103     c     *dtaara       define    §azute        AZUTEds
133100110103     c     *dtaara       define    §datiute      dDatiUte
133200110103      *
133300110103     c                   in(E)     *dtaara
133400110103     c                   IF        %Error or RSUT = *blanks
133500110103     c                   clear                   TIBS34ds
133600110103     c                   call      'TIBS34R'
133700110103     c                   parm                    TIBS34ds
133800110103     c                   in        *dtaara
133900110103     c                   ENDIF
134000110103      *
134100110103     c                   ENDSR
134200000512**
134300920709 ***   GESTIONE  FOGLI  VARI   ***
134400950206**
134500950206Filiale gestione errata
134600000128Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Corrente
134700000512Immissione/manutenzione record non gestita per i fogli di tipo 8
134800020422Data inferiore alla data odierna impossibile l'inserimento x fogli Cat 4
134900020422Data oltre il limite consentito per i fogli categoria 4
135000020909Manca il numeratore dei Fogli xxxxxxxxxx: TELEFONARE IN SEDE!!!!               6
135100020909Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli xxxxxxxxxx   7
135200030701Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Foglio
