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
002000160928     D UIN             S              1    DIM(40)                              DESCR TIP.INV
002100160928     D dbin            S              3    DIM(40)                              DESCR TIP.INV
002200160928     D apin            S              1    DIM(40)                              DESCR TIP.INV
002300160928     D chin            S              1    DIM(40)                              DESCR TIP.INV
002400161006     D** rif_IN          S              2    DIM(40)                              DESCR TIP.INV
002500900515      *
002600940701     D TES             S             34    DIM(1) CTDATA PERRCD(1)              TESTATE
002700020909     D ERR             S             78    DIM(10) CTDATA PERRCD(1)             TESTATE
002800000128     D W78             S              1    DIM(78)                              COMODO
002900920805     D DEC             S             12    DIM(6)                               DESCR CATEGOR
003000950206      *
003100950206     D L1              S              3  0 DIM(30)                              FIL GESTITE
003200900517      *
003300900514     D KPJBA         E DS
003400161124     d
003500161124     d TRULVPODS     e ds
003600161124     D  skFilOk               16    765    dim(250)
003700161124     d
003800110103      *
003900110103      * - Parametri per richiamo al pgm di controllo profilo utenti
004000110103     d TIBS34ds      e ds
004100110103      * - Ds di riferimento al file esterno AzUte00F
004200110103     d AZUTEds       e ds                  ExtName(AzUte00F)
004300110103      * - Ds per dati organigramma
004400110103     d dDatiUte      e ds
004500900516      *
004600940802     D* DS STANDARD FOGLI VARI
004700940802     D DSLR01        E DS
004800160928     D
004900160928     D ds7b          E DS
005000940802     D*
005100941027     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
005200941027     D DSLV50        E DS                  EXTNAME(FNLV50DS)
005300010314     D TIBS02        E DS                  EXTNAME(TIBS02DS)
005400940802     D*
005500010314     D DFAR          E DS
005600010314     D  §FARDS       E                     EXTFLD(D§FARDES)
005700010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
005800010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
005900010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
006000010314     D  §FARST       E                     EXTFLD(D§FARSTP)
006100010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
006200010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
006300020221     D  §FARNO       E                     EXTFLD(D§FARNOO)
006400020221     D  §FARUV       E                     EXTFLD(D§FARUVA)
006500020221     D  §FARDV       E                     EXTFLD(D§FARDVA)
006600020304     D OG143A        E DS                  EXTNAME(OG143) PREFIX(A)
006700960429     D DS7AF         E DS
006800960429     D DS7N          E DS
006900920805     D  §7NDE2                 1     10
007000000512     D FVVDS         E DS                  EXTNAME(FNFVV00F)
007100020422     d ddgd          e ds
007200020909     d Trul33Ds      e ds
007300020910     d Fnlr6Ds       e ds
007400110103     d*
007500050512     d* DS del campo fvvspn - spuntatore
007600050512     d Dfvvspn       e ds
007700900515      *
007800940701     D***
007900940701     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008000940701     D***
008100940705     D WLBDAT          DS                  INZ
008200940701     D  G02DAT                 1      8  0
008300940701     D  G02INV                 9     16  0
008400940701     D  G02ERR                17     17
008500940701     D  G02TGI                18     22  0
008600020422      *
008700020422     d Wdata8          DS
008800020422     d  dadata                 1      8  0
008900020422     d  adata                  9     16  0
009000020422     d  GioLav                17     21  0
009100940701     D***
009200940701     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
009300940701     D***
009400940705     D WGIDAT          DS                  INZ
009500940701     D  GIODAT                 1      8  0
009600940701     D  GIOINV                 9     16  0
009700940701     D  GIOTGI                17     21  0
009800920805     D TBLKEY          DS
009900940712     D  W001A                  1      1
010000950112     D PARA04          DS
010100950112     D  P04NFV                 1      5  0
010200950112     D  P04LNP                 6      8  0
010300950112     D  P04LNA                 9     11  0
010400950112     D  P04NRS                12     13  0
010500950112     D  P04NCD                14     20  0
010600950112     D  P04NCA                21     27  0
010700950112     D  P04NPS                28     29  0
010800950112     D  P04NPG                30     30  0
010900950112     D  P04FLG                31     31
011000950112     D  P04PGM                32     38
011100950112     D  P04FGS                39     41  0
011200101202     d* bolla
011300101202     d  p04bolp               42     44  0
011400101202     d  p04bors               45     46  0
011500101202     d  p04bosp               47     53  0
011600101202     d  p04boan               54     57  0
011700101202     D  P04ERR                58     58
011800101202     d*
011900950206     D* DS PER TRUL06R - CARICAMENTO £X
012000950206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012100950206     D  LIN                    1     90  0
012200950206     D                                     DIM(30)                              FIL. COMODO
012300020910
012400160929     d savspg          s                   Like(fvvspg)
012500160929     d wifp            s                   Like(Arbifp)
012600020917     d Wfgs            s                   Like(FvvFgs)
012700020917     d Wok             s              1    inz('0')
012800020917     d xx              s              2  0
012900030701
013000030701     d                 ds
013100030701     d  ClnPom                 1     31
013200030701     d  Pom                    1     31    Dim(31)
013300110103     ***d                 ds
013400110103     ***d  ClnMat                 1     31
013500110103     ***d  Mat                    1     31    Dim(31)
013600030701
013700030701     d ktfp            s                   Like(FvvFgs)
013800030701     d ktfa            s                   Like(FvvFgs)
013900030701     d kann            s                   Like(ClnAnn)
014000030701     d kmes            s                   Like(ClnMes)
014100160929     d indx            s              3  0
014200160929     d wgiorno         s              2  0
014300030701     d wdtaeur         s               d   datfmt(*eur)
014400030701     d wdtaiso         s               d   datfmt(*iso)
014500030701     d wdtameno1       s              8  0
014600030701     d wdtapiu1        s              8  0
014700030917     d wdtiso1         s               d
014800030917     d wdtiso2         s               d
014900030701
015000940712     D***
015100940712     D* CONSTANTI
015200940712     D***
015300940712     D CINSER          C                   CONST(' Inserimento ')
015400940712     D CVARIA          C                   CONST(' Variazione  ')
015500940712     D CANNUL          C                   CONST('  Annullato  ')
015600940712     D CBLOCC          C                   CONST('  Bloccato   ')
015700900515      *---------------------------------------------------------------*
015800910318      * INDICATORI USATI
015900910318      *---------------------------------------------------------------*
016000940712      * 01    = PGM RICHIAMATO
016100921228      * 02    = FOGLIO INVENTARIO
016200110103      **** 03    = FOGLIO CONSEGNE
016300921228      * 04    = CAMBIO FILIALE IN GESTIONE PER CMD18
016400921228      * 05    = SONO UN "REMXXX"
016500940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
016600941027      * 07    = ESISTONO SPUNTE SUL FOGLIO
016700161125      * 09    = utente  abilitato all IPG da decofiipg
016800161124      * 10    = tutti abilitati all'IPG
016900960429      * 11    = EMETTO TASTO FUNZIONALE F18 DI RIAPERTURA
017000960429      * 12    = EMETTO TASTO FUNZIONALE F1 DI CHIUSURA FV
017100950703      * 13    = UTILIZZATO PER CONTROLLARE CHE IL NUMERO DEL FOGLIO CHE
017200950703      *         SI VUOLE CREARE NON ESISTA GIA' IN FNFVV00F
017300031112      * 19    = Nasconde la richiesta del campo opzione ("1")
017400031112      *         (utilizzato dal pgm. FNLR02R, che utilizza FNLR01D)
017500940713      * 28    = EMETTO CAMPO MESSAGGI
017600910321      * 30/31 = DI COMODO
017700911129      * 38/51 = ERRORE
017800921228      * 52    = RECORD ANNULLATO
017900921228      * 53    = FOGLIO CHIUSO
018000961113      * 54/59 = ERRORE
018100940712      * 60    = RECORD GIA' VINCOLATO
018200960429      * 61    = FOGLIO BLOCCATO
018300060522      * 62/67 = ERRORE
018400921228      * 80    = IMMISSIONE   N80 = VARIAZIONE
018500921228      * 90    = ERRORE GENERICO
018600921228      * 98    = USATO IN INTERROGAZIONE
018700921228      * 99    = PROTEZIONE CAMPI
018800910318      *--------------------------------------------------------------*
018900000000     C     *ENTRY        PLIST
019000000000     C                   PARM                    KPJBA
019100940802     C                   MOVEL     KPJBU         DSLR01
019200900516      *
019300110103     C                   Z-ADD     1             CODUT             1 0
019400110103      *
019500110103      * Reperimento dati utente/aziendali
019600110103     c                   exsr      sr_DatiJob
019700940701     C* DESCRIZIONE AZIENDA
019800110103     C                   MOVEL     RSUT          V0CDSA
019900950206     C***
020000950206     C* CARICO TABELLA FILIALI GESTITE £1
020100950206     C***
020200950206     C                   CLEAR                   DSUL06
020300950206     C                   MOVE      '£1'          D06COD
020400950206     C                   MOVEL     SIMFEL        D06KEY
020500950206     C                   MOVEL     DSUL06        KPJBU
020600950206     C*
020700950206     C                   CALL      'TRUL06R'
020800950206     C                   PARM                    KPJBA
020900950206     C                   MOVEL     KPJBU         DSUL06
021000950206     C                   MOVEA     LIN           L1
021100900515      *---------------------------------------------------------------*
021200900515      * FLUSSO PRINCIPALE                                             *
021300900515      *---------------------------------------------------------------*
021400911004     C*
021500940701     C* CARICO TABELLE
021600940701     C                   EXSR      CARTAB
021700911004      *
021800900509     C     INIZIO        TAG
021900940701      * INIZIALIZZO VIDEATA
022000940701     C                   EXSR      INZ12
022100940701      *
022200940701      * 01 ON - PROGRAMMA RICHIAMATO
022300940802    1C     DLRRIC        IFEQ      'S'
022400910321     C                   SETON                                        01
022500940802     C                   MOVEL     DLRNFV        V0CNFV
022600940802     C                   MOVEL     DLRNPG        V0CNPG
022700941027     C                   MOVEL     DLRFGS        V0CFGS
022800940701    1C                   END
022900900524      *
023000900515     C     FMT00         TAG
023100940701      *
023200940701     C  N01              EXFMT     LR01D00
023300941027     C*
023400941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
023500941027     C                   SETOFF                                       4328
023600910318     C*
023700160929     C* F3  - FINE LAVORO
023800910318     C   KC              GOTO      FINE                                         F3=FINE
023900920806     C*
024000160929     C* F7  - INTERROGAZIONE FOGLI VARI
024100940701    2C   KG              DO
024200940802     C                   CLEAR                   DSLR01
024300920806     C*
024400940802     C                   MOVEL     DSLR01        KPJBU
024500940712     C                   CALL      'FNLR02R'
024600920806     C                   PARM                    KPJBA
024700940802     C                   MOVEL     KPJBU         DSLR01
024800940713     C*
024900920806     C                   GOTO      FMT00
025000940705     C                   ENDDO
025100920806     C*
025200160929     C* F18 - CAMBIO FILIALE
025300920806     C   KS              SETON                                        04
025400920806     C   KS              GOTO      FMT00
025500920709     C*
025600940701     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
025700940701     C   04              EXSR      CTRFGS
025800940701     C   04
025900940701     CAN 90              GOTO      FMT00
026000940701     C*
026100940701     C* CONTROLLO FORMATO 1
026200940701     C                   EXSR      CTRD00
026300940701     C   90              GOTO      FMT00
026400160929     C* F10 - IMMISSIONE IMPEDITA X NUMERO DI FOGLIO 8
026500000512     C     V0CNPG        IFEQ      '8'
026600000512     C                   SETON                                        9028
026700000512     C                   MOVEL     ERR(3)        V0CMSG
026800000512     C   90              GOTO      FMT00
026900000512     C                   END
027000910318     C*
027100910318     C* RIEMPIMENTO CAMPI PIU' DECODIFICA
027200910318     C*
027300910318     C                   EXSR      RIEMPI
027400910318     C*
027500900515     C     FMT01         TAG
027600900516      *
027700020422     C* SE DISTINTA NON POSSO VARIARE mai dal 23/04/02 *(SE PRESENTI BOLLE)*
027800020424     c                   setoff                                       97
027900020422     C     WNPG          IFEQ      4
028000020422     c     *in80         andeq     *off
028100020424     C                   SETON                                        97
028200020422     C                   END
028300020422      *
028400940701     C                   EXFMT     LR01D01
028500000128     C                   SETOFF                                       28
028600941027     C*
028700160929     C* F21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
028800941027     C     *INKV         IFEQ      *ON
028900950112     C                   CLEAR                   PARA04
029000950112     C                   Z-ADD     V0CNFV        P04NFV
029100950112     C                   Z-ADD     99            P04NRS
029200950112     C                   Z-ADD     99            P04NPS
029300950112     C                   MOVEL     V0CNPG        P04NPG
029400950112     C                   MOVEL     '1'           P04FLG
029500950112     C                   MOVEL     'FNLR01R'     P04PGM
029600950112     C                   Z-ADD     V0CFGS        P04FGS
029700950112     C                   MOVEL     PARA04        KPJBU
029800950112     C                   CALL      'FNLR04R'
029900950112     C                   PARM                    KPJBA
030000941027     C                   GOTO      FMT01
030100941027     C                   ENDIF
030200911030      *
030300160929      * F12 - RITORNO
030400940701     C     *INKL         IFEQ      *ON
030500940701     C* RILASCIO RECORD
030600940705     C  N80              UNLOCK    FNFVV01L
030700941027     C*
030800940701     C* VADO A INIZIO
030900940701     C  N01              GOTO      INIZIO
031000941027     C*
031100940701     C* SE RICHIAMATO VADO A FINE
031200941027     C   01              GOTO      FINE                                         RICH
031300940701     C                   ENDIF
031400911030      *
031500911030      *  CONTROLLI FORMATO1
031600940701     C                   EXSR      CTRD01
031700911030     C   90              GOTO      FMT01
031800911030      *
031900160928      * f1  - CHIUSURA IMP     SI ARRIVI
032000160929     c* Verifico se il sottotipo prevede la chiusura
032100160929    0c                   if        *inka
032200160929     c                   clear                   indx
032300160929    1c                   if        v1cspg<>*blanks
032400160929     c                   eval      Indx=%lookup(v1cspg:tin)
032500160929    1c                   endif
032600160929     c
032700161017    1c                   if        v0cnpg='5'  or (indx>0 and chin(indx)= 'S')
032800160929     c
032900160929    1C** KAV1CSPG        IFEQ      'P'
033000160929     C**   V1CSPG        OREQ      'I'
033100160929     C**   V1CSPG        OREQ      'A'
033200160929     C**   V0CNPG        OREQ      '5'
033300020304      *-------------------------------------------------------------------
033400020304      * FOGLIO IMA: PERMETTO LA CHIUSURA SOLTANTO SE LE SPUNTE HANNO
033500020304      *             1) TUTTE LNA CON NETWORK ESTERO
033600020304      *-------------------------------------------------------------------
033700000718    2C     V1CSPG        IFEQ      'A'
033800000128     C                   Z-ADD     V0CNFV        WNFV
033900070112     C     KBRV          CHAIN     FNBRV02L                           31
034000000718    3C     *IN31         DOWEQ     *OFF
034100000718     C*
034200000718    4C     BRVLNA        IFNE      SAVLNA
034300000128     C     BRVLNA        CHAIN     AZORG01L                           30
034400020304     C  N30              MOVEL     ORGDE3        OG143A
034500020304     C   30              CLEAR                   OG143A
034600000718     C                   MOVEL     BRVLNA        SAVLNA
034700000718    4C                   ENDIF
034800000718     C*
034900020304    4C     A§OGNTW       IFNE      'EEX'
035000020304    4C     A§OGNTW       ANDNE     'EUP'
035100020304    4C     A§OGNTW       ANDNE     'FED'
035200020304    4C     A§OGNTW       ANDNE     'DPD'
035300000718     C                   SETON                                        5431
035400961113     C                   GOTO      FMT01
035500000718    4C                   ENDIF
035600160928     c
035700070112     C     KBRV          READE     FNBRV02L                               31
035800000718    3C                   ENDDO
035900000718    2C                   ENDIF
036000961113     C*
036100911112     C                   EXSR      REGIS
036200960429     C  N01              GOTO      INIZIO
036300911030     C                   GOTO      FINE                                         RICH
036400000718   X1C                   ELSE
036500911030     C                   SETON                                        51
036600911030     C                   GOTO      FMT01
036700160929    1C                   ENDif
036800160929    0C                   ENDif
036900940701      *
037000160928      * F6  - REGISTRA
037100940701     C   KF              DO
037200020917      * Prelevo il numero
037300020923     c                   if        *in80
037400020917     c                   ExSr      CrtNfv
037500020917     c   90              GoTo      Fmt01
037600020917     c                   ExFmt     Lr01W01
037700020923     c                   EndIf
037800940701     C                   EXSR      REGIS
037900940701     C  N01              GOTO      INIZIO
038000940701     C                   GOTO      FINE                                         RICH
038100940701     C                   END
038200900516      *
038300160928      * F5  - RIATTIVA
038400160928      * F16 - ANNULLA
038500160928      * F18 - RIAPERTURA IMP
038600940701     C     *INKE         IFEQ      *ON
038700940701     C     *INKQ         OREQ      *ON
038800940701     C     *INKS         OREQ      *ON
038900960429     C*
039000960626     C* PER FOGLIO IMA E FOGLIO IMG RIAPRO SOLO SE LA DATA FOGLIO
039100960429     C*  E' MAGGIORE DELLA DATA ESECUZIONE IDD
039200980402     C*  ANCHE PER FOGLIO ARRIVI, TANTO LE DATE DI ARRIVO REALE DEI
039300980402     C*  MEZZI POSSONO ESSERE MODIFICATE ANCHE SE IL FOGLIO E' CHIUSO
039400960429     C     *INKS         IFEQ      *ON
039500160928     C     v1cSPG        IFEQ      'A'
039600160928     C     v1cSPG        OREQ      'G'
039700980402     C     WNPG          OREQ      2
039800990512     C     FVVDFV        IFLE      §7ADID
039900960429     C                   SETON                                        6290
040000960429     C                   GOTO      FMT01
040100960429     C                   ENDIF
040200960429     C                   ENDIF
040300960429     C                   ENDIF
040400960429     C*
040500940701     C   KE              MOVE      ' '           FVVATB
040600940701     C   KQ              MOVE      '*'           FVVATB
040700940701     C   KS              MOVE      ' '           FVVFCF
040800940701     C                   UPDATE    FNFVV000
040900000713     C     FVVNPG        IFEQ      4
041000000713     C                   EXSR      ANCHE8
041100000713     C                   ENDIF
041200950607     C*
041300910321     C  N01              GOTO      INIZIO
041400910321     C                   GOTO      FINE                                         RICH
041500940701     C                   ENDIF
041600911030     C*
041700911030     C                   GOTO      FMT01
041800911030      *
041900900515      *
042000000000     C     FINE          TAG
042100110103      *
042200020909     c                   Clear                   Trul33Ds
042300020909     c                   Eval      I33Tla = 'C'
042400020909     c                   Movel(p)  Trul33Ds      Kpjbu
042500020909     c                   Call      'TRUL33R'
042600020909     c                   Parm                    Kpjba
042700110103      *
042800020910     c                   Clear                   Fnlr6Ds
042900020910     c                   Eval      D6Tla = 'C'
043000020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
043100020910     c                   Call      'FNLR6R'
043200020910     c                   Parm                    Kpjba
043300900529      *
043400000000     C                   SETON                                        LR
043500000713      *-----------------------------------------------------***********
043600000713      * ALLINEA FOGLIO 8 SE È STATO AGGIORNATO IL FOGLIO 4
043700000713      *-----------------------------------------------------***********
043800000713     C     ANCHE8        BEGSR
043900000713     C                   MOVE      FVVDS         FVVDS2
044000000713     C     KFVV8         CHAIN     FNFVV000                           80
044100000713     C     *IN80         IFEQ      *OFF
044200000713     C                   MOVE      FVVDS2        FVVDS
044300000713     C                   MOVE      8             FVVNPG
044400000713     C                   UPDATE    FNFVV000
044500000713     C                   MOVE      '4'           WNPG
044600000713     C     KFVV          SETLL     FNFVV000
044700000713     C                   MOVE      FVVDS2        FVVDS
044800000713     C                   END
044900000713      *
045000000713     C                   ENDSR
045100900515      *-----------------------------------------------------***********
045200940701      * INIZIALIZZA 1 E 2 VIDEATA                           *  INZ12  *
045300900515      *-----------------------------------------------------***********
045400940701     C     INZ12         BEGSR
045500900515      *
045600910318     C* PULISCO FORMATO1
045700910320     C                   SETOFF                                       995253
045800911030     C                   SETOFF                                       010212
045900960429     C                   SETOFF                                       0411
046000940705     C                   MOVEL     'FNLR01R'     V0CPGM
046100970602     C                   MOVEL     ' '           V0CNPG
046200940701     C                   Z-ADD     0             V0CNFV
046300940701     C                   Z-ADD     0             V0CDIN
046400940701     C                   Z-ADD     0             V0CDFI
046500940701     C                   MOVEL     *BLANKS       V0CTFV
046600910318     C* PULISCO FORMATO2
046700940701     C                   Z-ADD     UDATE8        V1CDFV
046800030701     c                   ExSr      Sr_MinMax
046900940701     C                   MOVEL     *BLANKS       V1CDSF
047000940701     C                   MOVEL     *BLANKS       V1CSPN
047100940701     C                   MOVEL     *BLANKS       V1CSPG
047200900515      *
047300900514     C                   ENDSR
047400030701
047500030701      *------------------------------------------------------------------------*
047600030701      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
047700030701      *------------------------------------------------------------------------*
047800030701     c     Sr_MinMax     BegSr
047900110103      *
048000030701     c                   Clear                   Ktfa
048100030701     c                   Z-Add     V0cFgs        Ktfp
048200030701     c     *eur          Move      V1cdfv        wdtaeur
048300030701      * meno 1 gg. lavorativo
048400030701     c                   Do        *Hival
048500030701     c                   Subdur    1:*d          wdtaeur
048600030701     c                   extrct    wdtaeur:*d    wgiorno
048700030701     c                   extrct    wdtaeur:*m    kmes
048800030701     c                   extrct    wdtaeur:*y    kann
048900030701     c     kazcln        Chain     Azcln01l
049000030701     c                   If        Not %Found(azcln01l)
049100030701     c                   Leave
049200030701     c                   EndIf
049300030701     c                   If        POM(wgiorno) = 'F'
049400030701     c                   Iter
049500030701     c                   EndIf
049600030701     c                   Leave
049700030701     c                   EndDo
049800030701     c                   Move      wdtaeur       wdtaiso
049900030701     c                   Move      wdtaiso       wdtameno1
050000110103      *
050100030701      * più 1 gg. lavorativo
050200030701     c     *eur          Move      V1cdfv        wdtaeur
050300030701     c                   Do        *Hival
050400030701     c                   Adddur    1:*d          wdtaeur
050500030701     c                   extrct    wdtaeur:*d    wgiorno
050600030701     c                   extrct    wdtaeur:*m    kmes
050700030701     c                   extrct    wdtaeur:*y    kann
050800030701     c     kazcln        Chain     Azcln01l
050900030701     c                   If        Not %Found(azcln01l)
051000030701     c                   Leave
051100030701     c                   EndIf
051200030701     c                   If        POM(wgiorno) = 'F'
051300030701     c                   Iter
051400030701     c                   EndIf
051500030701     c                   Leave
051600030701     c                   EndDo
051700030701     c                   Move      wdtaeur       wdtaiso
051800030701     c                   Move      wdtaiso       wdtapiu1
051900110103      *
052000030701     c                   EndSr
052100030701
052200910318      *-----------------------------------------------------***********
052300940713      * PRELEVO NUMERATORE FOGLI                            *  CRTNFV *
052400910318      *-----------------------------------------------------***********
052500910318     C     CRTNFV        BEGSR
052600940701     C                   SETOFF                                       90
052700110103      *
052800020909Do  1c                   Do        *Hival
052900020910      * Cerco il numero da staccare (aggiorna e se non trovato in aznum crea record)
053000020909     c                   Clear                   Trul33Ds
053100020909     c                   Eval      I33Cnu = §7nCnu
053200020909     c                   Eval      I33Po1 = V0cfgs
053300020909     c                   Eval      I33Num = 1
053400020909     c                   Movel(p)  Trul33Ds      Kpjbu
053500020909     c                   Call      'TRUL33R'
053600020909     c                   Parm                    Kpjba
053700020909     c                   Movel     Kpjbu         Trul33Ds
053800020909
053900020909S   2c                   Select
054000020909      * Numeratore non trovato
054100020909     c                   When      O33err =  01
054200040227     c                   Eval      V1cmsg = Err(6)
054300040227     c                   Eval      %subst(V1cmsg:31:10) = §7nDe2
054400020909     c                   Eval      *In28 = *On
054500020909     c                   Eval      *In90 = *On
054600020910     c                   Leave
054700020910      * Numeratore allocato
054800020909     c                   When      O33err =  02
054900040227     c                   Eval      V1cmsg = Err(7)
055000040227     c                   Eval      %subst(V1cmsg:67:10) = §7nDe2
055100020909     c                   Eval      *In28 = *On
055200020909     c                   Eval      *In90 = *On
055300020910     c                   Leave
055400110103      *
055500020909     c                   When      O33err <> *zeros
055600040227     c                   Eval      V1cmsg = O33Msg
055700020909     c                   Eval      *In28 = *On
055800020909     c                   Eval      *In90 = *On
055900020910     c                   Leave
056000020909    2c                   EndSl
056100110103      *
056200020917      * Imposto flag di appoggio
056300020917     c                   Eval      Wok = *On
056400110103      *
056500020909      * Controllo che non esista già un foglio con lo stesso numero
056600020910      * Per i fogli di tipo 4 richiamo fnlr6r
056700020919      * lo richiamo passando come tipo foglio 8 così controlla anche l'esistenza
056800020919      * del tipo 8
056900020917If  2c                   If        Wnpg = 4
057000020910     c                   Clear                   Fnlr6ds
057100020919     c                   Eval      D6Npg = 8
057200020910     c                   Eval      D6Fgs = V0cfgs
057300020910     c                   Z-Add     O33Nrf        D6Ndt
057400020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
057500020910     c                   Call      'FNLR6R'
057600020910     c                   Parm                    Kpjba
057700020910     c                   Movel     Kpjbu         Fnlr6Ds
057800020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
057900020910     c* come foglio o in una bolla
058000020910If  3c                   If        D6Flg <> 'N'
058100020910     c                   Iter
058200020910    3c                   EndIf
058300020917   x2c                   Else
058400020917      * Per tutti gli altri tipi foglio controllo su fnfvv
058500020910     c                   Z-Add     O33Nrf        V0cnfv
058600020909     c     Kfvv          setll     Fnfvv01l
058700020910If  3c                   If        %Equal(Fnfvv01l)
058800020909     c                   Iter
058900020910    3c                   EndIf
059000020917      * Per i fogli di tipo 3 devo controllare la sotto categoria
059100020917If  3c                   If        Wnpg = 3
059200020917      * se sto aprendo un foglio con sotto categoria 'P' (foglio IMP)
059300020917      * devo controllare che non esiste lo stesso numero x tutti i p.o. del terminal
059400020917If  4c                   If        V1cspg = 'P'
059500020917     c                   Z-Add     O33Nrf        V0cnfv
059600020917Do  5c                   Do        30            xx
059700020917     c                   If        L1(xx) = *Zeros
059800020917     c                   Leave
059900020917     c                   EndIf
060000020917     c                   Z-Add     L1(xx)        Wfgs
060100020917     c     Kfvv1         Setll     Fnfvv01l
060200020917If  6c                   If        %Equal(Fnfvv01l)
060300020917     c                   Eval      Wok = *Off
060400020917     c                   Leave
060500020917    6c                   EndIf
060600020917    5c                   EndDo
060700020917If  5c                   If        Wok = *Off
060800020917     c                   Iter
060900020917    5c                   EndIf
061000020917   x4c                   Else
061100020917      * se sto aprendo un foglio con sotto categoria <> 'P'
061200020917      * devo controllare che non esiste un foglio IMP con lo stesso numero
061300020917      * x tutti i p.o. del terminal
061400020917     c                   Z-Add     O33Nrf        V0cnfv
061500020917Do  5c                   Do        30            xx
061600020917     c                   If        L1(xx) = *Zeros
061700020917     c                   Leave
061800020917     c                   EndIf
061900020917     c                   Z-Add     L1(xx)        Wfgs
062000020917     c     Kfvv1         Setll     Fnfvv01l
062100020917Do  6c                   Do        *Hival
062200020917     c     Kfvv1         Reade     Fnfvv01l
062300020917If  7c                   If        %EoF(Fnfvv01l)
062400020917     c                   Leave
062500020917    7c                   EndIf
062600160928If  7c                   If        v1cSpg = 'P'
062700020917     c                   Eval      Wok = *Off
062800020917     c                   Leave
062900020917    7c                   EndIf
063000020917    6c                   EndDo
063100020917    5c                   EndDo
063200020917If  5c                   If        Wok = *Off
063300020917     c                   Iter
063400020917    5c                   EndIf
063500020917    4c                   EndIf
063600020917    3c                   EndIf
063700020917    2c                   EndIf
063800020909     c                   Z-Add     O33Nrf        V0cnfv
063900020909     c                   Leave
064000020909    1c                   EndDo
064100910318     C*
064200020910     C                   ENDSR
064300900515      *-----------------------------------------------------***********
064400940701      * CONTROLLI FORMATO 1                                 *  CTRD01 *
064500900515      *-----------------------------------------------------***********
064600940701     C     CTRD01        BEGSR
064700900515      *
064800000128     C                   SETOFF                                       9059
064900911003      *
065000911003      * TIPO INVENTARIO
065100911003      *
065200940701     C   02V1CSPG        IFEQ      *BLANKS
065300911004     C                   SETON                                        4890
065400940701     C                   GOTO      ENDCT1
065500911004     C                   ELSE
065600911004     C*
065700160929     c                   eval      Indx=%lookup(v1cspg:tin)
065800160929     c                   if        indx=0 or uin(indx)='N'
065900160929     C                   SETON                                        4890
066000160929     C                   GOTO      ENDCT1
066100160929     C                   ENDif
066200161125     c* solo  abilitati
066300161124     c                   if        not *in09 and uin(indx)='1' and not *in10
066400161017     C                   SETON                                        4890
066500161017     C                   GOTO      ENDCT1
066600161017     C                   ENDif
066700160929     C                   ENDif
066800060522     c*
066900160929     c*Non si può cambiare sottocategoria foglio inventario se ci sono spunt
067000060522     c                   if        *in02  and not *in80 and wspu='S'
067100160929     c                             and v1cspg<>SAVspg
067200060522     C                   SETON                                        6790
067300060522     C                   GOTO      ENDCT1
067400060522     c                   endif
067500941027      *
067600941027      * CONTROLLO DATA FOGLIO
067700941027      *
067800941027     C                   Z-ADD     V1CDFV        G02DAT
067900941027     C                   MOVEL     *BLANK        G02ERR
068000941027     C                   CALL      'XSRDA8'
068100941027     C                   PARM                    WLBDAT
068200941027     C     G02ERR        IFEQ      '1'
068300941027     C                   SETON                                        40  90
068400941027     C                   GOTO      ENDCT1
068500941027     C                   END
068600941027     C*
068700941027     C                   Z-ADD     G02INV        WDFV
068800940706     C*
068900940706     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
069000940706     C                   Z-ADD     G02DAT        V1CDFV
069100940706     C*
069200940706     C* SE IL FOGLIO HA DELLE SPUNTE E' IMPOSSIBILE MODIFICARE LA DATA
069300940706     C     WSPU          IFEQ      'S'
069400940706     C     FVVDFV        ANDNE     WDFV
069500940706     C                   SETON                                        4990
069600940706     C                   GOTO      ENDCT1
069700940706     C                   ENDIF
069800970602     C* E' IMPOSSIBILE MODIFICARE LA DATA ANCHE SE E' FOGLIO ARRIVI
069900970602     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
070000970602     C     *IN80         IFEQ      *OFF
070100970602     C     WNPG          IFEQ      2
070200970602     C     FVVDFV        ANDNE     WDFV
070300970602     C     KFVV2         SETLL     FNFVA02L                               32
070400970602     C     *IN32         IFEQ      *ON
070500970602     C                   SETON                                        5090
070600970602     C                   GOTO      ENDCT1
070700970602     C                   END
070800970602     C                   END
070900970602     C                   END
071000020422     C* CONTROLLO DATA COMPRESA NEL RANGE non per le distinte fogli 4
071100020422     c                   if        wnpg <> 4
071200030701     c                   If        *Inkf and
071300030701     c                                       (wdfv < wdtameno1 or
071400030701     c                                        wdfv > wdtapiu1)
071500000128     C                   SETON                                        905928
071600030701     C   80              MOVEA     ERR(2)        W78
071700030701     c  n80              Movea     Err(8)        W78
071800030701     c                   Move      '01'          W002A             2
071900000128     C                   MOVEA     W002A         W78(46)
072000000128     C                   MOVEA     W78           V1CMSG
072100940701     C                   GOTO      ENDCT1
072200930225E   1C                   ENDIF
072300020422     c                   else
072400020422      *distinta controlli sui limiti particolari
072500020422     c                   if        *in80
072600020422     c                   if        wdfv < waamg
072700020422     c                   SETON                                        905928
072800020422     c                   MOVEL     err(4)        v1cmsg
072900020422     C                   GOTO      ENDct1
073000020422     c                   endif
073100020422      *controllo range fra le date di oggi e quella immessa
073200020422     c                   eval      dadata = waamg
073300020422     c                   eval      adata  = wdfv
073400020422     c                   CALL      'XSRLAV8'
073500020422     c                   PARM                    Wdata8
073600030917     c                   if        giolav = 0
073700030917     c                   clear                   giorni
073800030917     c                   move      waamg         wdtiso1
073900030917     c                   move      wdfv          wdtiso2
074000030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
074100030917     c                   if        giorni > 30
074200030917     c                   SETON                                        905928
074300030917     c                   MOVEL     err(5)        v1cmsg
074400030917     C                   GOTO      ENDct1
074500030917     c                   endif
074600030917     c                   endif
074700020422     c                   if        giolav > §dgddc
074800020422     c                   SETON                                        905928
074900020422     c                   MOVEL     err(5)        v1cmsg
075000020422     C                   GOTO      ENDct1
075100020422     c                   end
075200020422     c                   end
075300110103      *
075400020422     c                   end
075500910320      *
075600940701     C*
075700940701     C* RICHIESTO ANNULLAMENTO FOGLIO:
075800940701     C*
075900950919     C* SE ESISTONO SPUNTE IMPOSSIBILE ANNULLARE FOGLIO: SEMPRE!!
076000940701     C   KQWSPU          IFEQ      'S'
076100940701     C                   SETON                                        4190
076200940701     C                   GOTO      ENDCT1
076300950919     C                   ENDIF
076400940701     C*
076500940701     C* SE DISTINTA NON POSSO ANNULLARE SE PRESENTI BOLLE
076600941027     C   KQWNPG          IFEQ      4
076700020910     c                   Z-add     V0cfgs        Wifp
076800940701     C                   Z-ADD     V0CNFV        VIDNDC            7 0
076900020910     c     Karb78        Setll     Fnarb78l                               32
077000010314     C   32              SETON                                        5590
077100940701     C   32              GOTO      ENDCT1
077200010314     C* NON POSSO ANNULLARE ANCHE SE PRESENTI O.R.M.
077300010314     C     WORM          IFEQ      'S'
077400010314     C                   SETON                                        6590
077500010314     C                   GOTO      ENDCT1
077600010314     C                   ENDIF
077700940701     C                   END
077800980112     C* E' IMPOSSIBILE ANNULLARE SE E' FOGLIO ARRIVI
077900980112     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
078000980112     C   KQWNPG          IFEQ      2
078100980112     C     KFVV2         SETLL     FNFVA02L                               32
078200980112     C     *IN32         IFEQ      *ON
078300980112     C                   SETON                                        6490
078400980112     C                   GOTO      ENDCT1
078500980112     C                   END
078600980112     C                   END
078700900515      *
078800940701     C     ENDCT1        ENDSR
078900900515      *-----------------------------------------------------***********
079000900515      * REGISTRAZIONE RECORD                                *  REGIS  *
079100900515      *-----------------------------------------------------***********
079200900515     C     REGIS         BEGSR
079300900515      *
079400910318     C                   MOVEL     *BLANK        FVVATB
079500940701     C                   Z-ADD     V0CNFV        FVVNFV
079600940701     C                   Z-ADD     WDFV          FVVDFV
079700160928     c                   clear                   I
079800160928     C* PER CATEGORIA 3 VERIFICO IL TIPO INVENTARIO SE È DA SCRIVERE COSÌ COME È
079900160928     c*  oppure devo impostare il campo FPP
080000160928    1c                   if        *in02
080100160928     c                   eval      i=%lookup(v1cspg:tin)
080200160928    1c                   endif
080300920806     C*
080400160928    1C     V1CDSF        IFEQ      *BLANKS
080500940701     C                   MOVEL     *BLANKS       FVVDSF
080600160928
080700950314     C* DESCRIZIONE INVENTARIO: SE NON C'E' PRENDO DA TIPO INVENTARIO
080800160928    2C                   if        *in02 and i>0
080900160928     C                   MOVEL     DbIN(I)       FVVDSF
081000160928     c
081100160928   x2C                   ELSE
081200950320     C* DESCRIZIONE FOGLIO: SE NON C'E' PRENDO DA CATEGORIA FOGLIO
081300950320     C                   MOVEL     V1DNPG        FVVDSF
081400160928    2C                   endif
081500160928     c
081600160928   x1C                   ELSE
081700940701     C                   MOVEL     V1CDSF        FVVDSF
081800160928    1C                   endif
081900911129     C*
082000050512     c   80              clear                   dfvvspn
082100050512     C                   MOVEL     V1CSPN        §FVVSPN
082200050512     c                   movel     dfvvspn       fvvspn
082300160928     C                   IF        NOT *IN02
082400160928     C                   CLEAR                   FVVSPG
082500160928     C                   ELSE
082600160928     C                   MOVEL     V1CSPG        FVVSPG
082700160929     c                   clear                   fvvfpp
082800161006     c***                if        I>0 and
082900161006     c***                          rif_in(I)<>*blanks
083000161006     c***                movel     rif_in(i)     fvvspg
083100161006     c***                move      v1cspg        fvvfpp
083200161006     C***                ENDIF
083300160928     C                   ENDIF
083400160928     C
083500911016     C                   MOVEL     *BLANKS       FVVFCF
083600911112     C   KA              MOVE      'S'           FVVFCF
083700940701     C   80              MOVEL     V0CNPG        FVVNPG
083800911016     C                   MOVEL     'S'           FVVFSC
083900950123     C                   Z-ADD     SIMFEL        FVVFLE
084000900515      *
084100020918      * In immissione foglio mi reimposto il p.o. in gestione xchè al momento dei controlli
084200020918      * sul numero staccato fvvfgs non è più il p.o. gestione del video impostato nella
084300020918      * routine riempi
084400020918     c                   Movel     V0cFgs        FvvFgs
084500900515      *
084600950320     C   80              WRITE     FNFVV000                                     IMMISSIONE
084700160928
084800000512      *SCRITTURA FOGLIO DI TIPO 8 UGUALE A 4 PER SPUNTE RIENTRO
084900000512     C     *IN80         IFEQ      *ON
085000000512     C     FVVNPG        ANDEQ     4
085100000512     C                   MOVE      8             FVVNPG
085200000512     C                   WRITE     FNFVV000
085300000512     C                   END
085400911004      *
085500940701     C  N80              UPDATE    FNFVV000                                     VARIAZIONE
085600000512      *COPIA MODIFICHE DA FOGLIO 4 A FOGLIO 8
085700000512     C     *IN80         IFEQ      *OFF
085800000512     C     FVVNPG        ANDEQ     4
085900000512     C                   MOVE      FVVDS         FVVDS2
086000000512     C                   MOVE      '8'           WNPG
086100000512     C     KFVV          CHAIN     FNFVV000                           80
086200000512     C     *IN80         IFEQ      *OFF
086300000512     C                   MOVE      FVVDS2        FVVDS
086400000512     C                   MOVE      8             FVVNPG
086500000512     C                   UPDATE    FNFVV000
086600000512     C                   MOVE      '4'           WNPG
086700000512     C     KFVV          SETLL     FNFVV000
086800000515     C                   MOVE      FVVDS2        FVVDS
086900000512     C                   END
087000000512     C                   END
087100000512     C*
087200900515     C                   ENDSR
087300900515      *-----------------------------------------------------***********
087400900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
087500900515      *-----------------------------------------------------***********
087600900515     C     RIEMPI        BEGSR
087700161124     c                   setoff                                       09
087800161124     c
087900161124     c* Se sono in prova per IPG ricarico i tipi foglio in vbase alla filiale utente
088000161124     c                   if        not *in10
088100161125     c                   if        %lookup(%editc(v0cfgs:'X'):skFilOk)>0
088200161124     c                   seton                                        09
088300161124     c                   endif
088400161124     c                   exsr      car_inv
088500161124     c                   endif
088600161124     c
088700940712     C* EMETTO LA DESCRIZIONE DELLO STATO FOGLIO-INS/VARIAZ/ANNULL/BLOC
088800940712     C                   SETON                                        06
088900941027     C                   SETOFF                                       07
089000940712     C*
089100940701     C                   CLEAR                   WSPU
089200010314     C                   CLEAR                   WORM
089300160929     C                   CLEAR                   SAVSPG
089400960429     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
089500960429     C* IDD
089600960429     C                   MOVEL     '7A'          COD
089700960429     C                   MOVEL(P)  V0CFGS        KEY
089800960429     C     KTAB2         CHAIN     TABEL                              31
089900960429     C  N31              MOVEL     TBLUNI        DS7AF
090000900515      *
090100911016     C* SE INVENTARIO EMETTO CAMPO DELLA SOTTOSPECE
090200160928    1C     V0CNPG        IFEQ      '3'
090300911003     C                   SETON                                        02
090400160928     c* i comandi di riapertra e chiusura sono gestiti da tabella
090500160928    2c                   if        not *in80
090600160928     c* se il campo FPP è pieno devo cercare la corretta tipologia
090700161006     c***                if        fvvfpp<>*blanks
090800161006     c***                eval      I=%lookup(fvvfpp:tin)
090900161006     C***                MOVEL     FVVfpp        V1CSPG                          sottotipo
091000161006     C***                MOVEL     FVVfpp        SAVSPG                          sottotipo
091100161006     c***                else
091200160929     c                   eval      I=%lookup(fvvspg:tin)
091300160928     C                   MOVEL     FVVSPG        V1CSPG                          sottotipo
091400160929     C                   MOVEL     FVVspg        SAVSPG                          sottotipo
091500161006     c***                endif
091600160928     c
091700160928    3c                   if        apin(i)='S'
091800160928     c                   seton                                        11
091900160928    3c                   endif
092000160928    3c                   if        chin(i)='S'
092100160928     c                   seton                                        12
092200160928    3c                   endif
092300160928    2c                   endif
092400160928     c
092500960429     C* FOGLIO IMP
092600960429     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
092700160928     C**N80FVVSPG        IFEQ      'P'
092800160928     C**                 SETON                                        1211
092900160928     C**                 END
093000001115     C* FOGLIO IMI
093100001115     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
093200160928     C**N80FVVSPG        IFEQ      'I'
093300160928     C**                 SETON                                        1211
093400160928     C**                 END
093500960429     C* FOGLIO IMA
093600960429     C* EMETTO CMD DI RIAPERTURA
093700961113     C* EMETTO ANCHE COMANDO DI CHIUSURA MA SOLO SE TUTTE LE SPUNTE
093800961113     C*  CONTENUTE NELL'IMA HANNO DATA ARRIVO > 300
093900160928     C**N80FVVSPG        IFEQ      'A'
094000160928     C**                 SETON                                        1112
094100160928     C**                 ENDIF
094200960626     C* FOGLIO IMG
094300960626     C* EMETTO CMD DI RIAPERTURA
094400160928     C**N80FVVSPG        IFEQ      'G'
094500160928     C**                 SETON                                        11
094600160928     C**                 ENDIF
094700160928    1C                   ENDIF
094800940701     C*
094900940712      * S O L O   I N   V A R I A Z I O N E
095000940712     C     *IN80         IFEQ      *OFF
095100940712     C* DESCRIZIONE A VIDEO
095200940712     C                   MOVEL     CVARIA        V1DTFV
095300940712     C*
095400940712     C* SE ANNULLATO PROTEGGO CAMPI
095500940712     C     FVVATB        IFEQ      '*'
095600940712     C                   SETON                                        52  99
095700940712     C                   MOVEL     CANNUL        V1DTFV
095800940712     C                   END
095900940712     C*
096000940712     C* FOGLIO CHIUSO
096100940712     C     FVVFCF        IFEQ      'S'
096200940712     C                   SETON                                        53  99
096300940712     C                   END
096400940712     C* FOGLIO BLOCCATO
096500940712     C     FVVFCF        IFNE      'S'
096600940712     C     FVVFCF        ANDNE     ' '
096700940712     C                   MOVEL     CBLOCC        V1DTFV
096800940712     C                   ENDIF
096900940712     C*
097000960429     C* 12 E 11 ON - EMETTO F1 O F18 ANCHE PER FOGLIO ENTRATA
097100940712     C     V0CNPG        IFEQ      '5'
097200960429     C                   SETON                                        1211
097300960429     C                   ENDIF
097400960429     C*
097500960429     C* 11 ON - FACCIO RIAPRIRE IL FOGLIO IDA
097600960429     C*         SOLO SE NON HO GIA' FATTO L'IDD
097700960429     C     V0CNPG        IFEQ      '2'
097800960429     C                   SETON                                        11
097900960429     C                   ENDIF
098000941027     C*
098100940712     C                   MOVE      FVVDSF        V1CDSF                          DESCRIZIONE
098200050512     c                   clear                   dfvvspn
098300050512     C                   MOVEL     FVVSPN        dfvvspn                         SPUNTATORE
098400050512     C                   MOVEL     §fvvspn       V1CSPN                          SPUNTATORE
098500911004     C*
098600940712     C                   MOVE      FVVDFV        G02INV                          DATA
098700911004     C                   MOVE      *ZEROS        G02DAT
098800911004     C                   MOVEL     3             G02ERR
098900940706     C                   CALL      'XSRDA8'
099000911004     C                   PARM                    WLBDAT
099100940701     C                   Z-ADD     G02DAT        V1CDFV
099200030701     c                   ExSr      Sr_MinMax
099300940701     C*
099400940701     C* SEGNALO SE SUL FOGLIO ESISTONO SPUNTE
099500000128     C                   Z-ADD     V0CNFV        WNFV
099600070112     C     KBRV          SETLL     FNBRV000                               07
099700911004      *
099800940701     C* WSPU = "S"  --> ESISTONO SPUNTE
099900941027     C     *IN07         IFEQ      *ON
100000940701     C                   MOVEL     'S'           WSPU              1
100100910318     C                   END
100200010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
100300010314     C     WSPU          IFEQ      *BLANKS
100400010314     C     KORM          SETLL     FNORM06L
100500010314     C     KORM          READE     FNORM06L                               31
100600010314    3C     *IN31         DOWEQ     *OFF
100700010314     C                   CLEAR                   TIBS02
100800010314     C                   CLEAR                   DFAR
100900010314     C                   MOVEL     'C'           T02MOD
101000010314     C                   MOVEL     KNSIF         T02SIF
101100010314     C                   MOVEL     'FAR'         T02COD
101200010314     C                   MOVEL     ORMFAO        T02KE1
101300010314     C                   CALL      'TIBS02R'
101400010314     C                   PARM                    KPJBA
101500010314     C                   PARM                    TIBS02
101600010314     C     T02ERR        IFEQ      *BLANKS
101700010314     C                   MOVEL     T02UNI        DFAR
101800010314     C                   ENDIF
101900010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
102000010314     C* DISTINTA E' PRESENTE UN O.R.M.
102100010314    4C     §FARAF        IFNE      'S'
102200010314     C                   SETON                                        31
102300010314     C                   MOVEL     'S'           WORM              1
102400010314   X4C                   ELSE
102500010314     C     KORM          READE     FNORM06L                               31
102600010314    4C                   ENDIF
102700010314    3C                   ENDDO
102800010314    2C                   ENDIF
102900940712     C*
103000940712     C                   ELSE
103100940712     C*
103200940712     C* I M M I S S I O N E
103300940712     C*
103400940712     C* DESCRIZIONE A VIDEO
103500940712     C                   MOVEL     CINSER        V1DTFV
103600940712     C* FILIALE GESTIONE
103700940712     C                   MOVEL     V0CFGS        FVVFGS
103800940712     C                   ENDIF
103900910319     C*
104000910319     C                   ENDSR
104100911004      *-----------------------------------------------------***********
104200940701      * CARICO TABELLE                                      *  CARTAB *
104300911004      *-----------------------------------------------------***********
104400940701     C     CARTAB        BEGSR
104500161125     c                   setoff                                       0910
104600161124     c
104700161125     c* carico le filiali abilitate all'IPG
104800161124     c                   clear                   trulvpods
104900161124     c                   eval      ivpoke1 = 'DECOFIIPG'
105000161124     c                   call      'TRULVPOR'
105100161124     c                   parm                    trulvpods
105200161124     c
105300161124     c                   if        %lookup('999':skFilOk)>0
105400161124     c                   seton                                        10
105500161124     c                   endif
105600161124     C*
105700161124     C                   MOVEL     '7B'          COD
105800161124     C                   Z-ADD     1             I                 3 0
105900161124     C     KTAB          SETLL     TABEL
106000161124     C     KTAB          READE     TABEL                                  30
106100161124     C*
106200161124     C     *IN30         DOWEQ     '0'
106300161124     C                   MOVEL     TBLKEY        TIN(I)
106400161124     C                   MOVEL     TBLUNI        DS7B
106500161124     C                   MOVEL     §7BDES        DIN(I)
106600161124     C*******            MOVEL     §7Brifer      rif_IN(I)
106700161124     C                   MOVE      §7Buti        uIN(I)
106800161124     C                   MOVE      §7Bdesb       dbIN(I)
106900161124     C                   MOVE      §7Briap       apIN(I)
107000161124     C                   MOVE      §7Bchius      chIN(I)
107100161124     C*
107200161124     C                   ADD       1             I
107300161124     C     KTAB          READE     TABEL                                  30
107400161124     C                   END
107500161124
107600161017     c
107700940701     C***
107800940701     C* CARICO I TIPI INVENTARIO
107900940701     C***
108000161124     c                   exsr      car_inv
108100940701     C***
108200160929     C* VEDO SE SONO SIMFEL O 2 livello
108300940701     C***
108400110103     c     dutLPO        ifeq      '2'
108500110103     c     dutLPO        oreq      *blanks
108600110103     c                   movel     dutPOU        v0cfgs
108700920806     C                   SETON                                        05
108800930209     C                   ELSE
108900940701     C                   MOVEL     SIMFEL        V0CFGS
109000920806     C                   END
109100941027     C*
109200940701     C     V0CFGS        CHAIN     AZORG                              31
109300940713     C  N31              MOVEL     ORGDES        V1DFGS
109400940701     C***
109500920806     C* CARICO CATEGORIE FOGLI
109600940701     C***
109700920806     C                   MOVEL     '7N'          COD
109800920806     C                   Z-ADD     0             I
109900920806     C     KTAB          SETLL     TABEL
110000920806     C     KTAB          READE     TABEL                                  31
110100940701     C*
110200920806     C     *IN31         DOWEQ     '0'
110300920806     C                   MOVEL     TBLUNI        DS7N
110400920806     C* SOLO SE ABILITATA PER FOGLI VARI
110500920806     C     §7NFFV        IFEQ      'S'
110600921228     C     §7NUTI        ANDNE     '9'
110700920806     C                   ADD       1             I
110800920806     C                   MOVEL     ' ='          DEC(I)
110900940712     C                   MOVEL     W001A         DEC(I)
111000920806     C                   MOVE      §7NDE2        DEC(I)
111100920806     C     I             IFEQ      6
111200920806     C                   SETON                                        31
111300920806     C                   END
111400920806     C                   END
111500920806     C*
111600920806     C  N31KTAB          READE     TABEL                                  31
111700920806     C                   END
111800920806     C*
111900940701     C* IMPOSTO LE DESCRIZIONI A VIDEO
112000920806     C     DEC(1)        IFNE      *BLANKS
112100920806     C                   MOVEL     DEC(1)        DESCA1
112200920806     C     DEC(2)        IFNE      *BLANKS
112300920806     C                   MOVEL     DEC(2)        DESCA2
112400920806     C     DEC(3)        IFNE      *BLANKS
112500920806     C                   MOVEL     DEC(3)        DESCA3
112600920806     C     DEC(4)        IFNE      *BLANKS
112700920806     C                   MOVEL     DEC(4)        DESCA4
112800920806     C     DEC(5)        IFNE      *BLANKS
112900920806     C                   MOVEL     DEC(5)        DESCA5
113000920806     C     DEC(6)        IFNE      *BLANKS
113100920806     C                   MOVEL     DEC(6)        DESCA6
113200920806     C                   END
113300920806     C                   END
113400920806     C                   END
113500920806     C                   END
113600920806     C                   END
113700920806     C                   END
113800940701     C***
113900940711     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
114000940701     C***
114100940711     C                   TIME                    W0120            12 0
114200940711     C                   MOVE      W0120         WDAT              6 0
114300940711     C*
114400940711     C                   Z-ADD     WDAT          G02DAT
114500940701     C                   MOVEL     *BLANK        G02ERR
114600940701     C                   CALL      'XSRDA8'
114700940701     C                   PARM                    WLBDAT
114800940711     C* DATA DEL GIORNO IN AAAAMMGG
114900940711     C                   Z-ADD     G02INV        WAAMG
115000940701     C*
115100940711     C* DATA DEL GIORNO IN GGMMAAAA
115200940701     C                   Z-ADD     G02DAT        UDATE8
115300920806     C*
115400940705     C                   MOVE      TES(1)        V0CTES
115500940701     C*
115600911004     C                   ENDSR
115700161124      *-----------------------------------------------------***********
115800161124      *  Carico i tipi inventario
115900161124      *-----------------------------------------------------***********
116000161124     c     Car_inv       BEGSR
116100161124     C                   MOVEL     *BLANKS       VIDSP1
116200161124     C                   MOVEL     *BLANKS       DESSP1
116300161124     C                   MOVEL     *BLANKS       VIDSP2
116400161124     C                   MOVEL     *BLANKS       DESSP2
116500161124     C                   MOVEL     *BLANKS       VIDSP3
116600161124     C                   MOVEL     *BLANKS       DESSP3
116700161124     C                   MOVEL     *BLANKS       VIDSP4
116800161124     C                   MOVEL     *BLANKS       DESSP4
116900161124     C                   MOVEL     *BLANKS       VIDSP5
117000161124     C                   MOVEL     *BLANKS       DESSP5
117100161124     C                   MOVEL     *BLANKS       VIDSP6
117200161124     C                   MOVEL     *BLANKS       DESSP6
117300161124
117400161124     C* EMETTO A VIDEO I PRIMI 6
117500161124     c                   z-add     1             I
117600161124     c                   dow       tin(i)<>*blanks
117700161124     C     TIN(I)        IFNE      *BLANKS
117800161124     c     uin(i)        andne     'N'
117900161124     c*
118000161124     c                   if        *in10 or
118100161124     c                             *in09 or (not *in09 and uin(I)<>'1' )
118200161124     c                   select
118300161124     c                   when      vidsp1=*blanks
118400161124     C                   MOVEL     TIN(I)        VIDSP1
118500161124     C                   MOVE      ' ='          VIDSP1
118600161124     C                   MOVEL     DIN(I)        DESSP1
118700161124     c                   when      vidsp2=*blanks
118800161124     C                   MOVEL     TIN(I)        VIDSP2
118900161124     C                   MOVE      ' ='          VIDSP2
119000161124     C                   MOVEL     DIN(I)        DESSP2
119100161124     c                   when      vidsp3=*blanks
119200161124     C                   MOVEL     TIN(I)        VIDSP3
119300161124     C                   MOVE      ' ='          VIDSP3
119400161124     C                   MOVEL     DIN(I)        DESSP3
119500161124     c                   when      vidsp4=*blanks
119600161124     C                   MOVEL     TIN(I)        VIDSP4
119700161124     C                   MOVE      ' ='          VIDSP4
119800161124     C                   MOVEL     DIN(I)        DESSP4
119900161124     c                   when      vidsp5=*blanks
120000161124     C                   MOVEL     TIN(I)        VIDSP5
120100161124     C                   MOVE      ' ='          VIDSP5
120200161124     C                   MOVEL     DIN(I)        DESSP5
120300161124     c                   when      vidsp6=*blanks
120400161124     C                   MOVEL     TIN(I)        VIDSP6
120500161124     C                   MOVE      ' ='          VIDSP6
120600161124     C                   MOVEL     DIN(I)        DESSP6
120700161124     c                   leave
120800161124     c                   endsl
120900161124     c                   endif
121000161124     c                   endif
121100161124     c
121200161124     c                   add       1             I
121300161124
121400161124     C                   ENDdo
121500161124     c                   ENDSR
121600940701      *-----------------------------------------------------***********
121700940701      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
121800940701      *-----------------------------------------------------***********
121900940701     C     CTRFGS        BEGSR
122000940708     C                   SETOFF                                       90
122100950206     C     V0CFGS        LOOKUP    L1                                     30
122200950206     C     *IN30         IFEQ      *OFF
122300950206     C     V0CFGS        OREQ      0
122400950206     C                   MOVEL     ERR(1)        V0CMSG
122500941027     C                   SETON                                        439028
122600941027     C                   GOTO      ENDCTF
122700941027     C                   ENDIF
122800941027     C* DECODIFICA
122900941027     C     V0CFGS        CHAIN     AZORG                              31
123000941027     C  N31              MOVEL     ORGDES        V1DFGS
123100940701     C*
123200940701     C     ENDCTF        ENDSR
123300940701      *-----------------------------------------------------***********
123400940712      * CONTROLLO FORMATO 0                                    CTRD00
123500940701      *-----------------------------------------------------***********
123600940701     C     CTRD00        BEGSR
123700940701     C                   SETOFF                                       90
123800940701     C*
123900940701     C* CATEGORIA OBBLIGATORIA
124000940701     C*
124100940701     C     V0CNPG        IFEQ      ' '
124200940701     C                   SETON                                        4290
124300940701     C                   GOTO      ENDCT0
124400940701     C                   END
124500940701     C*
124600940701     C* RICERCA
124700940701     C     V0CNPG        IFEQ      '?'
124800940701     C                   MOVEL     CODUT         §KUT
124900940701     C                   MOVEL     '7N'          §COD
125000940701     C                   MOVE      ' '           V0CNPG
125100940701     C                   MOVEL     *BLANKS       §KEY
125200940701     C                   CALL      'X§TABER'
125300940701     C                   PARM                    §KUT              1 0
125400940701     C                   PARM                    §COD              2
125500940701     C                   PARM                    §KEY              8
125600940701     C                   PARM                    §DES             30
125700940701     C                   MOVEL     §KEY          V0CNPG
125800940701     C                   SETON                                        90
125900940701     C                   GOTO      ENDCT0
126000940701     C                   END
126100940701     C* CONTROLLO
126200940701     C                   MOVEL     '7N'          COD
126300940701     C                   MOVEL     *BLANKS       KEY
126400940701     C                   MOVEL     V0CNPG        KEY
126500940701     C* INESISTENTE
126600940701     C     KTAB2         CHAIN     TABEL                              42
126700940701     C* ANNULLATA
126800940701     C  N42TBLFLG        IFNE      ' '
126900940701     C                   SETON                                        42
127000940701     C                   ELSE
127100940701     C                   MOVEL     TBLUNI        DS7N
127200940701     C* NON E' CATEGORIA DI FOGLI VARI
127300940701     C     §7NFFV        IFNE      'S'
127400940701     C                   SETON                                        42
127500940701     C                   END
127600940701     C                   END
127700940701     C* 42 ON - ERRORE
127800940701     C   42              SETON                                        90
127900940701     C   42              GOTO      ENDCT0
128000940701     C*
128100940701     C* NON TUTTE LE CATEGORIE SONO UTILIZZABILI DAI 2 LIVELLI
128200940701     C     §7NUTI        IFEQ      '9'
128300940701     C                   SETON                                        5690
128400940701     C                   GOTO      ENDCT0
128500940701     C                   END
128600041026     c* categoria  4 non più utilizzabile da questo pgm
128700041026     c     v0cnpg        ifeq      '4'
128800041026     c                   seton                                        6390
128900041026     c                   goto      endct0
129000041026     c                   endif
129100041026     c
129200940701     C*
129300940701     C* FILIALE IN GESTIONE E' SIMFEL
129400940701     C     SIMFEL        IFEQ      V0CFGS
129500940701     C     §7NUTI        IFEQ      '2'
129600940701     C                   SETON                                        5790
129700940701     C                   GOTO      ENDCT0
129800940701     C                   END
129900940701     C*
130000940701     C                   ELSE
130100940701     C*
130200940701     C* FILIALE IN GESTIONE NON E' SIMFEL
130300940701     C     §7NUTI        IFEQ      '1'
130400940713     C                   SETON                                        5890
130500940713     C                   GOTO      ENDCT0
130600940701     C                   END
130700940701     C                   END
130800040227     c* Se §7NUITI='G' categoria solo in  gestione ma non in immissione
130900040227     c                   if        *inkj and §7nuti='G'
131000040227     C                   SETON                                        6690
131100040227     C                   GOTO      ENDCT0
131200040227     c                   endif
131300940701     C*
131400940701     C                   MOVEL     §7NDES        V1DNPG                         DESCRIZIONE
131500940701     C                   MOVEL     V0CNPG        WNPG                           IN CAMPO NUM
131600940701     C*
131700940713     C* FOGLIO
131800950320     C                   SETON                                        80
131900950320     C                   SETOFF                                       60
132000950320     C  NKJKFVV          CHAIN     FNFVV000                           8060
132100940701     C*
132200940701     C* RECORD GIA' VINCOLATO
132300940712     C   60              SETON                                        90
132400940712     C   60              GOTO      ENDCT0
132500950628     C* SE FOGLIO BLOCCATO --> NON POSSO MANUTENZIONARLO
132600950628     C     *IN80         IFEQ      *OFF
132700950628     C     FVVFCF        ANDNE     'S'
132800950628     C     FVVFCF        ANDNE     ' '
132900950628     C                   SETON                                        9061
133000950628     C                   UNLOCK    FNFVV01L
133100950628     C                   GOTO      ENDCT0
133200950628     C                   ENDIF
133300940701     C*
133400940701      *
133500940713      * NUMERO FOGLIO INESISTENTE IN VARIAZIONE
133600950320     C  NKJ*IN80         IFEQ      *ON
133700950320     C                   SETON                                        3890
133800950320     C                   GOTO      ENDCT0
133900940701     C                   ENDIF
134000940701     C*
134100940701     C     ENDCT0        ENDSR
134200940701      *-----------------------------------------------------***********
134300940701      * INZSR INIZIALE                                         INZSR
134400940701      *-----------------------------------------------------***********
134500940701     C     *INZSR        BEGSR
134600940701      ***
134700940701      * CHIAVI
134800940701      ***
134900940701     C     KFVV          KLIST
135000940701     C                   KFLD                    WNPG
135100940701     C                   KFLD                    V0CNFV
135200941027     C                   KFLD                    V0CFGS
135300020917     c     Kfvv1         Klist
135400020917     c                   Kfld                    Wnpg
135500020917     c                   Kfld                    V0cnfv
135600020917     c                   Kfld                    Wfgs
135700000713     C     KFVV8         KLIST
135800000713     C                   KFLD                    WNPG8
135900000713     C                   KFLD                    FVVNFV
136000000713     C                   KFLD                    FVVFGS
136100000713     C                   Z-ADD     8             WNPG8
136200000128     C* N.B. QUANDO IL NUMERO FOGLIO DIVENTERA' LUNGO 6 ANCHE PER I
136300000128     C*      FOGLI (FNFVV FNFVA) LA KLIST KBRV SI POTRÀ CANCELLARE
136400000128     C*      PERCHE' KFVV SI POTRA' UTILIZZARE AL SUO POSTO KFVV
136500000128     C     KBRV          KLIST
136600000128     C                   KFLD                    WNPG
136700000128     C                   KFLD                    WNFV
136800000128     C                   KFLD                    V0CFGS
136900950104     C     KFVV2         KLIST
137000950104     C                   KFLD                    WNPG
137100950104     C                   KFLD                    V0CNFV
137200020917     c                   Kfld                    V0cFgs
137300950607     C     KDST          KLIST
137400950607     C                   KFLD                    V0CNFV
137500950607     C                   KFLD                    V0CFGS
137600940701     C     KTAB          KLIST
137700940701     C                   KFLD                    CODUT
137800940701     C                   KFLD                    COD               2
137900940701     C     KTAB2         KLIST
138000940701     C                   KFLD                    CODUT
138100940701     C                   KFLD                    COD               2
138200940701     C                   KFLD                    KEY               8
138300010314     C* ACCESSO FNORM06L
138400010314     C     KORM          KLIST
138500010314     C                   KFLD                    V0CFGS
138600010314     C                   KFLD                    WNFV
138700020910      * Fnarb
138800020910     c     Karb78        Klist
138900020910     c                   Kfld                    Wifp
139000020910     c                   Kfld                    Vidndc
139100030701      * Azcln01l
139200030701     c     kazcln        Klist
139300030701     c                   Kfld                    ktfp
139400030701     c                   Kfld                    ktfa
139500030701     c                   Kfld                    kann
139600030701     c                   Kfld                    kmes
139700110103      *
139800940701     C* LIKE DEFN
139900940711     C* DATA DEL GIORNO A 8 AAAAMMGG
140000940711     C     *LIKE         DEFINE    FVVDFV        WAAMG
140100940711     C* DATA DEL GIORNO A 8 GGMMAAAA
140200000718     C     *LIKE         DEFINE    BRVLNA        SAVLNA
140300000718     C     *LIKE         DEFINE    BRVLNP        SAVLNP
140400000718     C     *LIKE         DEFINE    FVVDS         FVVDS2
140500940705     C     *LIKE         DEFINE    FVVDFV        UDATE8
140600940701     C     *LIKE         DEFINE    FVVDFV        WDFV
140700940701     C     *LIKE         DEFINE    FVVNPG        WNPG
140800000713     C     *LIKE         DEFINE    FVVNPG        WNPG8
140900000128     C     *LIKE         DEFINE    BRVNFV        WNFV
141000020422     C***
141100020422     C     KTBE          KLIST
141200020422     C                   KFLD                    KCOD1
141300020422     C                   KFLD                    KKE1
141400020422     C     *LIKE         DEFINE    TBEKE1        KKE1
141500020422     C     *LIKE         DEFINE    TBECOD        KCOD1
141600020422     C*  tabelle dgd x reperimento limiti chiusura spedizioni
141700020422     C                   MOVEL     'DGD'         KCOD1
141800020422     C                   MOVEL     *BLANKS       KKE1
141900020422     C                   MOVEL     '1'           KKE1
142000020422     C     KTBE          CHAIN     TNTBE01L                           31
142100020422     C     *IN31         IFEQ      '0'
142200020422     C                   MOVEL     TBEUNI        DDGD
142300020422     C                   END
142400940701     C                   ENDSR
142500110103
142600110103      *---------------------------------------------------------------*
142700110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
142800110103      *---------------------------------------------------------------*
142900110103     c     sr_DatiJob    BEGSR
143000110103      *
143100110103     c     *dtaara       define    §azute        AZUTEds
143200110103     c     *dtaara       define    §datiute      dDatiUte
143300110103      *
143400110103     c                   in(E)     *dtaara
143500110103     c                   IF        %Error or RSUT = *blanks
143600110103     c                   clear                   TIBS34ds
143700110103     c                   call      'TIBS34R'
143800110103     c                   parm                    TIBS34ds
143900110103     c                   in        *dtaara
144000110103     c                   ENDIF
144100110103      *
144200110103     c                   ENDSR
144300000512**
144400920709 ***   GESTIONE  FOGLI  VARI   ***
144500950206**
144600950206Filiale gestione errata
144700000128Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Corrente
144800000512Immissione/manutenzione record non gestita per i fogli di tipo 8
144900020422Data inferiore alla data odierna impossibile l'inserimento x fogli Cat 4
145000020422Data oltre il limite consentito per i fogli categoria 4
145100020909Manca il numeratore dei Fogli xxxxxxxxxx: TELEFONARE IN SEDE!!!!               6
145200020909Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli xxxxxxxxxx   7
145300030701Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Foglio
