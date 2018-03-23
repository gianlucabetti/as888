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
003400110103      *
003500110103      * - Parametri per richiamo al pgm di controllo profilo utenti
003600110103     d TIBS34ds      e ds
003700110103      * - Ds di riferimento al file esterno AzUte00F
003800110103     d AZUTEds       e ds                  ExtName(AzUte00F)
003900110103      * - Ds per dati organigramma
004000110103     d dDatiUte      e ds
004100900516      *
004200940802     D* DS STANDARD FOGLI VARI
004300940802     D DSLR01        E DS
004400160928     D
004500160928     D ds7b          E DS
004600940802     D*
004700941027     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
004800941027     D DSLV50        E DS                  EXTNAME(FNLV50DS)
004900010314     D TIBS02        E DS                  EXTNAME(TIBS02DS)
005000940802     D*
005100010314     D DFAR          E DS
005200010314     D  §FARDS       E                     EXTFLD(D§FARDES)
005300010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
005400010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
005500010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
005600010314     D  §FARST       E                     EXTFLD(D§FARSTP)
005700010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
005800010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
005900020221     D  §FARNO       E                     EXTFLD(D§FARNOO)
006000020221     D  §FARUV       E                     EXTFLD(D§FARUVA)
006100020221     D  §FARDV       E                     EXTFLD(D§FARDVA)
006200020304     D OG143A        E DS                  EXTNAME(OG143) PREFIX(A)
006300960429     D DS7AF         E DS
006400960429     D DS7N          E DS
006500920805     D  §7NDE2                 1     10
006600000512     D FVVDS         E DS                  EXTNAME(FNFVV00F)
006700020422     d ddgd          e ds
006800020909     d Trul33Ds      e ds
006900020910     d Fnlr6Ds       e ds
007000110103     d*
007100050512     d* DS del campo fvvspn - spuntatore
007200050512     d Dfvvspn       e ds
007300900515      *
007400940701     D***
007500940701     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007600940701     D***
007700940705     D WLBDAT          DS                  INZ
007800940701     D  G02DAT                 1      8  0
007900940701     D  G02INV                 9     16  0
008000940701     D  G02ERR                17     17
008100940701     D  G02TGI                18     22  0
008200020422      *
008300020422     d Wdata8          DS
008400020422     d  dadata                 1      8  0
008500020422     d  adata                  9     16  0
008600020422     d  GioLav                17     21  0
008700940701     D***
008800940701     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
008900940701     D***
009000940705     D WGIDAT          DS                  INZ
009100940701     D  GIODAT                 1      8  0
009200940701     D  GIOINV                 9     16  0
009300940701     D  GIOTGI                17     21  0
009400920805     D TBLKEY          DS
009500940712     D  W001A                  1      1
009600950112     D PARA04          DS
009700950112     D  P04NFV                 1      5  0
009800950112     D  P04LNP                 6      8  0
009900950112     D  P04LNA                 9     11  0
010000950112     D  P04NRS                12     13  0
010100950112     D  P04NCD                14     20  0
010200950112     D  P04NCA                21     27  0
010300950112     D  P04NPS                28     29  0
010400950112     D  P04NPG                30     30  0
010500950112     D  P04FLG                31     31
010600950112     D  P04PGM                32     38
010700950112     D  P04FGS                39     41  0
010800101202     d* bolla
010900101202     d  p04bolp               42     44  0
011000101202     d  p04bors               45     46  0
011100101202     d  p04bosp               47     53  0
011200101202     d  p04boan               54     57  0
011300101202     D  P04ERR                58     58
011400101202     d*
011500950206     D* DS PER TRUL06R - CARICAMENTO £X
011600950206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011700950206     D  LIN                    1     90  0
011800950206     D                                     DIM(30)                              FIL. COMODO
011900020910
012000160929     d savspg          s                   Like(fvvspg)
012100160929     d wifp            s                   Like(Arbifp)
012200020917     d Wfgs            s                   Like(FvvFgs)
012300020917     d Wok             s              1    inz('0')
012400020917     d xx              s              2  0
012500030701
012600030701     d                 ds
012700030701     d  ClnPom                 1     31
012800030701     d  Pom                    1     31    Dim(31)
012900110103     ***d                 ds
013000110103     ***d  ClnMat                 1     31
013100110103     ***d  Mat                    1     31    Dim(31)
013200030701
013300030701     d ktfp            s                   Like(FvvFgs)
013400030701     d ktfa            s                   Like(FvvFgs)
013500030701     d kann            s                   Like(ClnAnn)
013600030701     d kmes            s                   Like(ClnMes)
013700160929     d indx            s              3  0
013800160929     d wgiorno         s              2  0
013900030701     d wdtaeur         s               d   datfmt(*eur)
014000030701     d wdtaiso         s               d   datfmt(*iso)
014100030701     d wdtameno1       s              8  0
014200030701     d wdtapiu1        s              8  0
014300030917     d wdtiso1         s               d
014400030917     d wdtiso2         s               d
014500030701
014600940712     D***
014700940712     D* CONSTANTI
014800940712     D***
014900940712     D CINSER          C                   CONST(' Inserimento ')
015000940712     D CVARIA          C                   CONST(' Variazione  ')
015100940712     D CANNUL          C                   CONST('  Annullato  ')
015200940712     D CBLOCC          C                   CONST('  Bloccato   ')
015300900515      *---------------------------------------------------------------*
015400910318      * INDICATORI USATI
015500910318      *---------------------------------------------------------------*
015600940712      * 01    = PGM RICHIAMATO
015700921228      * 02    = FOGLIO INVENTARIO
015800110103      **** 03    = FOGLIO CONSEGNE
015900921228      * 04    = CAMBIO FILIALE IN GESTIONE PER CMD18
016000921228      * 05    = SONO UN "REMXXX"
016100940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
016200941027      * 07    = ESISTONO SPUNTE SUL FOGLIO
016300161017      * 09    = utente EDP o 001
016400960429      * 11    = EMETTO TASTO FUNZIONALE F18 DI RIAPERTURA
016500960429      * 12    = EMETTO TASTO FUNZIONALE F1 DI CHIUSURA FV
016600950703      * 13    = UTILIZZATO PER CONTROLLARE CHE IL NUMERO DEL FOGLIO CHE
016700950703      *         SI VUOLE CREARE NON ESISTA GIA' IN FNFVV00F
016800031112      * 19    = Nasconde la richiesta del campo opzione ("1")
016900031112      *         (utilizzato dal pgm. FNLR02R, che utilizza FNLR01D)
017000940713      * 28    = EMETTO CAMPO MESSAGGI
017100910321      * 30/31 = DI COMODO
017200911129      * 38/51 = ERRORE
017300921228      * 52    = RECORD ANNULLATO
017400921228      * 53    = FOGLIO CHIUSO
017500961113      * 54/59 = ERRORE
017600940712      * 60    = RECORD GIA' VINCOLATO
017700960429      * 61    = FOGLIO BLOCCATO
017800060522      * 62/67 = ERRORE
017900921228      * 80    = IMMISSIONE   N80 = VARIAZIONE
018000921228      * 90    = ERRORE GENERICO
018100921228      * 98    = USATO IN INTERROGAZIONE
018200921228      * 99    = PROTEZIONE CAMPI
018300910318      *--------------------------------------------------------------*
018400000000     C     *ENTRY        PLIST
018500000000     C                   PARM                    KPJBA
018600940802     C                   MOVEL     KPJBU         DSLR01
018700900516      *
018800110103     C                   Z-ADD     1             CODUT             1 0
018900110103      *
019000110103      * Reperimento dati utente/aziendali
019100110103     c                   exsr      sr_DatiJob
019200940701     C* DESCRIZIONE AZIENDA
019300110103     C                   MOVEL     RSUT          V0CDSA
019400950206     C***
019500950206     C* CARICO TABELLA FILIALI GESTITE £1
019600950206     C***
019700950206     C                   CLEAR                   DSUL06
019800950206     C                   MOVE      '£1'          D06COD
019900950206     C                   MOVEL     SIMFEL        D06KEY
020000950206     C                   MOVEL     DSUL06        KPJBU
020100950206     C*
020200950206     C                   CALL      'TRUL06R'
020300950206     C                   PARM                    KPJBA
020400950206     C                   MOVEL     KPJBU         DSUL06
020500950206     C                   MOVEA     LIN           L1
020600900515      *---------------------------------------------------------------*
020700900515      * FLUSSO PRINCIPALE                                             *
020800900515      *---------------------------------------------------------------*
020900911004     C*
021000940701     C* CARICO TABELLE
021100940701     C                   EXSR      CARTAB
021200911004      *
021300900509     C     INIZIO        TAG
021400940701      * INIZIALIZZO VIDEATA
021500940701     C                   EXSR      INZ12
021600940701      *
021700940701      * 01 ON - PROGRAMMA RICHIAMATO
021800940802    1C     DLRRIC        IFEQ      'S'
021900910321     C                   SETON                                        01
022000940802     C                   MOVEL     DLRNFV        V0CNFV
022100940802     C                   MOVEL     DLRNPG        V0CNPG
022200941027     C                   MOVEL     DLRFGS        V0CFGS
022300940701    1C                   END
022400900524      *
022500900515     C     FMT00         TAG
022600940701      *
022700940701     C  N01              EXFMT     LR01D00
022800941027     C*
022900941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
023000941027     C                   SETOFF                                       4328
023100910318     C*
023200160929     C* F3  - FINE LAVORO
023300910318     C   KC              GOTO      FINE                                         F3=FINE
023400920806     C*
023500160929     C* F7  - INTERROGAZIONE FOGLI VARI
023600940701    2C   KG              DO
023700940802     C                   CLEAR                   DSLR01
023800920806     C*
023900940802     C                   MOVEL     DSLR01        KPJBU
024000940712     C                   CALL      'FNLR02R'
024100920806     C                   PARM                    KPJBA
024200940802     C                   MOVEL     KPJBU         DSLR01
024300940713     C*
024400920806     C                   GOTO      FMT00
024500940705     C                   ENDDO
024600920806     C*
024700160929     C* F18 - CAMBIO FILIALE
024800920806     C   KS              SETON                                        04
024900920806     C   KS              GOTO      FMT00
025000920709     C*
025100940701     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
025200940701     C   04              EXSR      CTRFGS
025300940701     C   04
025400940701     CAN 90              GOTO      FMT00
025500940701     C*
025600940701     C* CONTROLLO FORMATO 1
025700940701     C                   EXSR      CTRD00
025800940701     C   90              GOTO      FMT00
025900160929     C* F10 - IMMISSIONE IMPEDITA X NUMERO DI FOGLIO 8
026000000512     C     V0CNPG        IFEQ      '8'
026100000512     C                   SETON                                        9028
026200000512     C                   MOVEL     ERR(3)        V0CMSG
026300000512     C   90              GOTO      FMT00
026400000512     C                   END
026500910318     C*
026600910318     C* RIEMPIMENTO CAMPI PIU' DECODIFICA
026700910318     C*
026800910318     C                   EXSR      RIEMPI
026900910318     C*
027000900515     C     FMT01         TAG
027100900516      *
027200020422     C* SE DISTINTA NON POSSO VARIARE mai dal 23/04/02 *(SE PRESENTI BOLLE)*
027300020424     c                   setoff                                       97
027400020422     C     WNPG          IFEQ      4
027500020422     c     *in80         andeq     *off
027600020424     C                   SETON                                        97
027700020422     C                   END
027800020422      *
027900940701     C                   EXFMT     LR01D01
028000000128     C                   SETOFF                                       28
028100941027     C*
028200160929     C* F21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
028300941027     C     *INKV         IFEQ      *ON
028400950112     C                   CLEAR                   PARA04
028500950112     C                   Z-ADD     V0CNFV        P04NFV
028600950112     C                   Z-ADD     99            P04NRS
028700950112     C                   Z-ADD     99            P04NPS
028800950112     C                   MOVEL     V0CNPG        P04NPG
028900950112     C                   MOVEL     '1'           P04FLG
029000950112     C                   MOVEL     'FNLR01R'     P04PGM
029100950112     C                   Z-ADD     V0CFGS        P04FGS
029200950112     C                   MOVEL     PARA04        KPJBU
029300950112     C                   CALL      'FNLR04R'
029400950112     C                   PARM                    KPJBA
029500941027     C                   GOTO      FMT01
029600941027     C                   ENDIF
029700911030      *
029800160929      * F12 - RITORNO
029900940701     C     *INKL         IFEQ      *ON
030000940701     C* RILASCIO RECORD
030100940705     C  N80              UNLOCK    FNFVV01L
030200941027     C*
030300940701     C* VADO A INIZIO
030400940701     C  N01              GOTO      INIZIO
030500941027     C*
030600940701     C* SE RICHIAMATO VADO A FINE
030700941027     C   01              GOTO      FINE                                         RICH
030800940701     C                   ENDIF
030900911030      *
031000911030      *  CONTROLLI FORMATO1
031100940701     C                   EXSR      CTRD01
031200911030     C   90              GOTO      FMT01
031300911030      *
031400160928      * f1  - CHIUSURA IMP     SI ARRIVI
031500160929     c* Verifico se il sottotipo prevede la chiusura
031600160929    0c                   if        *inka
031700160929     c                   clear                   indx
031800160929    1c                   if        v1cspg<>*blanks
031900160929     c                   eval      Indx=%lookup(v1cspg:tin)
032000160929    1c                   endif
032100160929     c
032200161017    1c                   if        v0cnpg='5'  or (indx>0 and chin(indx)= 'S')
032300160929     c
032400160929    1C** KAV1CSPG        IFEQ      'P'
032500160929     C**   V1CSPG        OREQ      'I'
032600160929     C**   V1CSPG        OREQ      'A'
032700160929     C**   V0CNPG        OREQ      '5'
032800020304      *-------------------------------------------------------------------
032900020304      * FOGLIO IMA: PERMETTO LA CHIUSURA SOLTANTO SE LE SPUNTE HANNO
033000020304      *             1) TUTTE LNA CON NETWORK ESTERO
033100020304      *-------------------------------------------------------------------
033200000718    2C     V1CSPG        IFEQ      'A'
033300000128     C                   Z-ADD     V0CNFV        WNFV
033400070112     C     KBRV          CHAIN     FNBRV02L                           31
033500000718    3C     *IN31         DOWEQ     *OFF
033600000718     C*
033700000718    4C     BRVLNA        IFNE      SAVLNA
033800000128     C     BRVLNA        CHAIN     AZORG01L                           30
033900020304     C  N30              MOVEL     ORGDE3        OG143A
034000020304     C   30              CLEAR                   OG143A
034100000718     C                   MOVEL     BRVLNA        SAVLNA
034200000718    4C                   ENDIF
034300000718     C*
034400020304    4C     A§OGNTW       IFNE      'EEX'
034500020304    4C     A§OGNTW       ANDNE     'EUP'
034600020304    4C     A§OGNTW       ANDNE     'FED'
034700020304    4C     A§OGNTW       ANDNE     'DPD'
034800000718     C                   SETON                                        5431
034900961113     C                   GOTO      FMT01
035000000718    4C                   ENDIF
035100160928     c
035200070112     C     KBRV          READE     FNBRV02L                               31
035300000718    3C                   ENDDO
035400000718    2C                   ENDIF
035500961113     C*
035600911112     C                   EXSR      REGIS
035700960429     C  N01              GOTO      INIZIO
035800911030     C                   GOTO      FINE                                         RICH
035900000718   X1C                   ELSE
036000911030     C                   SETON                                        51
036100911030     C                   GOTO      FMT01
036200160929    1C                   ENDif
036300160929    0C                   ENDif
036400940701      *
036500160928      * F6  - REGISTRA
036600940701     C   KF              DO
036700020917      * Prelevo il numero
036800020923     c                   if        *in80
036900020917     c                   ExSr      CrtNfv
037000020917     c   90              GoTo      Fmt01
037100020917     c                   ExFmt     Lr01W01
037200020923     c                   EndIf
037300940701     C                   EXSR      REGIS
037400940701     C  N01              GOTO      INIZIO
037500940701     C                   GOTO      FINE                                         RICH
037600940701     C                   END
037700900516      *
037800160928      * F5  - RIATTIVA
037900160928      * F16 - ANNULLA
038000160928      * F18 - RIAPERTURA IMP
038100940701     C     *INKE         IFEQ      *ON
038200940701     C     *INKQ         OREQ      *ON
038300940701     C     *INKS         OREQ      *ON
038400960429     C*
038500960626     C* PER FOGLIO IMA E FOGLIO IMG RIAPRO SOLO SE LA DATA FOGLIO
038600960429     C*  E' MAGGIORE DELLA DATA ESECUZIONE IDD
038700980402     C*  ANCHE PER FOGLIO ARRIVI, TANTO LE DATE DI ARRIVO REALE DEI
038800980402     C*  MEZZI POSSONO ESSERE MODIFICATE ANCHE SE IL FOGLIO E' CHIUSO
038900960429     C     *INKS         IFEQ      *ON
039000160928     C     v1cSPG        IFEQ      'A'
039100160928     C     v1cSPG        OREQ      'G'
039200980402     C     WNPG          OREQ      2
039300990512     C     FVVDFV        IFLE      §7ADID
039400960429     C                   SETON                                        6290
039500960429     C                   GOTO      FMT01
039600960429     C                   ENDIF
039700960429     C                   ENDIF
039800960429     C                   ENDIF
039900960429     C*
040000940701     C   KE              MOVE      ' '           FVVATB
040100940701     C   KQ              MOVE      '*'           FVVATB
040200940701     C   KS              MOVE      ' '           FVVFCF
040300940701     C                   UPDATE    FNFVV000
040400000713     C     FVVNPG        IFEQ      4
040500000713     C                   EXSR      ANCHE8
040600000713     C                   ENDIF
040700950607     C*
040800910321     C  N01              GOTO      INIZIO
040900910321     C                   GOTO      FINE                                         RICH
041000940701     C                   ENDIF
041100911030     C*
041200911030     C                   GOTO      FMT01
041300911030      *
041400900515      *
041500000000     C     FINE          TAG
041600110103      *
041700020909     c                   Clear                   Trul33Ds
041800020909     c                   Eval      I33Tla = 'C'
041900020909     c                   Movel(p)  Trul33Ds      Kpjbu
042000020909     c                   Call      'TRUL33R'
042100020909     c                   Parm                    Kpjba
042200110103      *
042300020910     c                   Clear                   Fnlr6Ds
042400020910     c                   Eval      D6Tla = 'C'
042500020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
042600020910     c                   Call      'FNLR6R'
042700020910     c                   Parm                    Kpjba
042800900529      *
042900000000     C                   SETON                                        LR
043000000713      *-----------------------------------------------------***********
043100000713      * ALLINEA FOGLIO 8 SE È STATO AGGIORNATO IL FOGLIO 4
043200000713      *-----------------------------------------------------***********
043300000713     C     ANCHE8        BEGSR
043400000713     C                   MOVE      FVVDS         FVVDS2
043500000713     C     KFVV8         CHAIN     FNFVV000                           80
043600000713     C     *IN80         IFEQ      *OFF
043700000713     C                   MOVE      FVVDS2        FVVDS
043800000713     C                   MOVE      8             FVVNPG
043900000713     C                   UPDATE    FNFVV000
044000000713     C                   MOVE      '4'           WNPG
044100000713     C     KFVV          SETLL     FNFVV000
044200000713     C                   MOVE      FVVDS2        FVVDS
044300000713     C                   END
044400000713      *
044500000713     C                   ENDSR
044600900515      *-----------------------------------------------------***********
044700940701      * INIZIALIZZA 1 E 2 VIDEATA                           *  INZ12  *
044800900515      *-----------------------------------------------------***********
044900940701     C     INZ12         BEGSR
045000900515      *
045100910318     C* PULISCO FORMATO1
045200910320     C                   SETOFF                                       995253
045300911030     C                   SETOFF                                       010212
045400960429     C                   SETOFF                                       0411
045500940705     C                   MOVEL     'FNLR01R'     V0CPGM
045600970602     C                   MOVEL     ' '           V0CNPG
045700940701     C                   Z-ADD     0             V0CNFV
045800940701     C                   Z-ADD     0             V0CDIN
045900940701     C                   Z-ADD     0             V0CDFI
046000940701     C                   MOVEL     *BLANKS       V0CTFV
046100910318     C* PULISCO FORMATO2
046200940701     C                   Z-ADD     UDATE8        V1CDFV
046300030701     c                   ExSr      Sr_MinMax
046400940701     C                   MOVEL     *BLANKS       V1CDSF
046500940701     C                   MOVEL     *BLANKS       V1CSPN
046600940701     C                   MOVEL     *BLANKS       V1CSPG
046700900515      *
046800900514     C                   ENDSR
046900030701
047000030701      *------------------------------------------------------------------------*
047100030701      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
047200030701      *------------------------------------------------------------------------*
047300030701     c     Sr_MinMax     BegSr
047400110103      *
047500030701     c                   Clear                   Ktfa
047600030701     c                   Z-Add     V0cFgs        Ktfp
047700030701     c     *eur          Move      V1cdfv        wdtaeur
047800030701      * meno 1 gg. lavorativo
047900030701     c                   Do        *Hival
048000030701     c                   Subdur    1:*d          wdtaeur
048100030701     c                   extrct    wdtaeur:*d    wgiorno
048200030701     c                   extrct    wdtaeur:*m    kmes
048300030701     c                   extrct    wdtaeur:*y    kann
048400030701     c     kazcln        Chain     Azcln01l
048500030701     c                   If        Not %Found(azcln01l)
048600030701     c                   Leave
048700030701     c                   EndIf
048800030701     c                   If        POM(wgiorno) = 'F'
048900030701     c                   Iter
049000030701     c                   EndIf
049100030701     c                   Leave
049200030701     c                   EndDo
049300030701     c                   Move      wdtaeur       wdtaiso
049400030701     c                   Move      wdtaiso       wdtameno1
049500110103      *
049600030701      * più 1 gg. lavorativo
049700030701     c     *eur          Move      V1cdfv        wdtaeur
049800030701     c                   Do        *Hival
049900030701     c                   Adddur    1:*d          wdtaeur
050000030701     c                   extrct    wdtaeur:*d    wgiorno
050100030701     c                   extrct    wdtaeur:*m    kmes
050200030701     c                   extrct    wdtaeur:*y    kann
050300030701     c     kazcln        Chain     Azcln01l
050400030701     c                   If        Not %Found(azcln01l)
050500030701     c                   Leave
050600030701     c                   EndIf
050700030701     c                   If        POM(wgiorno) = 'F'
050800030701     c                   Iter
050900030701     c                   EndIf
051000030701     c                   Leave
051100030701     c                   EndDo
051200030701     c                   Move      wdtaeur       wdtaiso
051300030701     c                   Move      wdtaiso       wdtapiu1
051400110103      *
051500030701     c                   EndSr
051600030701
051700910318      *-----------------------------------------------------***********
051800940713      * PRELEVO NUMERATORE FOGLI                            *  CRTNFV *
051900910318      *-----------------------------------------------------***********
052000910318     C     CRTNFV        BEGSR
052100940701     C                   SETOFF                                       90
052200110103      *
052300020909Do  1c                   Do        *Hival
052400020910      * Cerco il numero da staccare (aggiorna e se non trovato in aznum crea record)
052500020909     c                   Clear                   Trul33Ds
052600020909     c                   Eval      I33Cnu = §7nCnu
052700020909     c                   Eval      I33Po1 = V0cfgs
052800020909     c                   Eval      I33Num = 1
052900020909     c                   Movel(p)  Trul33Ds      Kpjbu
053000020909     c                   Call      'TRUL33R'
053100020909     c                   Parm                    Kpjba
053200020909     c                   Movel     Kpjbu         Trul33Ds
053300020909
053400020909S   2c                   Select
053500020909      * Numeratore non trovato
053600020909     c                   When      O33err =  01
053700040227     c                   Eval      V1cmsg = Err(6)
053800040227     c                   Eval      %subst(V1cmsg:31:10) = §7nDe2
053900020909     c                   Eval      *In28 = *On
054000020909     c                   Eval      *In90 = *On
054100020910     c                   Leave
054200020910      * Numeratore allocato
054300020909     c                   When      O33err =  02
054400040227     c                   Eval      V1cmsg = Err(7)
054500040227     c                   Eval      %subst(V1cmsg:67:10) = §7nDe2
054600020909     c                   Eval      *In28 = *On
054700020909     c                   Eval      *In90 = *On
054800020910     c                   Leave
054900110103      *
055000020909     c                   When      O33err <> *zeros
055100040227     c                   Eval      V1cmsg = O33Msg
055200020909     c                   Eval      *In28 = *On
055300020909     c                   Eval      *In90 = *On
055400020910     c                   Leave
055500020909    2c                   EndSl
055600110103      *
055700020917      * Imposto flag di appoggio
055800020917     c                   Eval      Wok = *On
055900110103      *
056000020909      * Controllo che non esista già un foglio con lo stesso numero
056100020910      * Per i fogli di tipo 4 richiamo fnlr6r
056200020919      * lo richiamo passando come tipo foglio 8 così controlla anche l'esistenza
056300020919      * del tipo 8
056400020917If  2c                   If        Wnpg = 4
056500020910     c                   Clear                   Fnlr6ds
056600020919     c                   Eval      D6Npg = 8
056700020910     c                   Eval      D6Fgs = V0cfgs
056800020910     c                   Z-Add     O33Nrf        D6Ndt
056900020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
057000020910     c                   Call      'FNLR6R'
057100020910     c                   Parm                    Kpjba
057200020910     c                   Movel     Kpjbu         Fnlr6Ds
057300020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
057400020910     c* come foglio o in una bolla
057500020910If  3c                   If        D6Flg <> 'N'
057600020910     c                   Iter
057700020910    3c                   EndIf
057800020917   x2c                   Else
057900020917      * Per tutti gli altri tipi foglio controllo su fnfvv
058000020910     c                   Z-Add     O33Nrf        V0cnfv
058100020909     c     Kfvv          setll     Fnfvv01l
058200020910If  3c                   If        %Equal(Fnfvv01l)
058300020909     c                   Iter
058400020910    3c                   EndIf
058500020917      * Per i fogli di tipo 3 devo controllare la sotto categoria
058600020917If  3c                   If        Wnpg = 3
058700020917      * se sto aprendo un foglio con sotto categoria 'P' (foglio IMP)
058800020917      * devo controllare che non esiste lo stesso numero x tutti i p.o. del terminal
058900020917If  4c                   If        V1cspg = 'P'
059000020917     c                   Z-Add     O33Nrf        V0cnfv
059100020917Do  5c                   Do        30            xx
059200020917     c                   If        L1(xx) = *Zeros
059300020917     c                   Leave
059400020917     c                   EndIf
059500020917     c                   Z-Add     L1(xx)        Wfgs
059600020917     c     Kfvv1         Setll     Fnfvv01l
059700020917If  6c                   If        %Equal(Fnfvv01l)
059800020917     c                   Eval      Wok = *Off
059900020917     c                   Leave
060000020917    6c                   EndIf
060100020917    5c                   EndDo
060200020917If  5c                   If        Wok = *Off
060300020917     c                   Iter
060400020917    5c                   EndIf
060500020917   x4c                   Else
060600020917      * se sto aprendo un foglio con sotto categoria <> 'P'
060700020917      * devo controllare che non esiste un foglio IMP con lo stesso numero
060800020917      * x tutti i p.o. del terminal
060900020917     c                   Z-Add     O33Nrf        V0cnfv
061000020917Do  5c                   Do        30            xx
061100020917     c                   If        L1(xx) = *Zeros
061200020917     c                   Leave
061300020917     c                   EndIf
061400020917     c                   Z-Add     L1(xx)        Wfgs
061500020917     c     Kfvv1         Setll     Fnfvv01l
061600020917Do  6c                   Do        *Hival
061700020917     c     Kfvv1         Reade     Fnfvv01l
061800020917If  7c                   If        %EoF(Fnfvv01l)
061900020917     c                   Leave
062000020917    7c                   EndIf
062100160928If  7c                   If        v1cSpg = 'P'
062200020917     c                   Eval      Wok = *Off
062300020917     c                   Leave
062400020917    7c                   EndIf
062500020917    6c                   EndDo
062600020917    5c                   EndDo
062700020917If  5c                   If        Wok = *Off
062800020917     c                   Iter
062900020917    5c                   EndIf
063000020917    4c                   EndIf
063100020917    3c                   EndIf
063200020917    2c                   EndIf
063300020909     c                   Z-Add     O33Nrf        V0cnfv
063400020909     c                   Leave
063500020909    1c                   EndDo
063600910318     C*
063700020910     C                   ENDSR
063800900515      *-----------------------------------------------------***********
063900940701      * CONTROLLI FORMATO 1                                 *  CTRD01 *
064000900515      *-----------------------------------------------------***********
064100940701     C     CTRD01        BEGSR
064200900515      *
064300000128     C                   SETOFF                                       9059
064400911003      *
064500911003      * TIPO INVENTARIO
064600911003      *
064700940701     C   02V1CSPG        IFEQ      *BLANKS
064800911004     C                   SETON                                        4890
064900940701     C                   GOTO      ENDCT1
065000911004     C                   ELSE
065100911004     C*
065200160929     c                   eval      Indx=%lookup(v1cspg:tin)
065300160929     c                   if        indx=0 or uin(indx)='N'
065400160929     C                   SETON                                        4890
065500160929     C                   GOTO      ENDCT1
065600160929     C                   ENDif
065700161017     c* solo EDP e 001
065800161017     c                   if        not *in09 and uin(indx)='1'
065900161017     C                   SETON                                        4890
066000161017     C                   GOTO      ENDCT1
066100161017     C                   ENDif
066200160929     C                   ENDif
066300060522     c*
066400160929     c*Non si può cambiare sottocategoria foglio inventario se ci sono spunt
066500060522     c                   if        *in02  and not *in80 and wspu='S'
066600160929     c                             and v1cspg<>SAVspg
066700060522     C                   SETON                                        6790
066800060522     C                   GOTO      ENDCT1
066900060522     c                   endif
067000941027      *
067100941027      * CONTROLLO DATA FOGLIO
067200941027      *
067300941027     C                   Z-ADD     V1CDFV        G02DAT
067400941027     C                   MOVEL     *BLANK        G02ERR
067500941027     C                   CALL      'XSRDA8'
067600941027     C                   PARM                    WLBDAT
067700941027     C     G02ERR        IFEQ      '1'
067800941027     C                   SETON                                        40  90
067900941027     C                   GOTO      ENDCT1
068000941027     C                   END
068100941027     C*
068200941027     C                   Z-ADD     G02INV        WDFV
068300940706     C*
068400940706     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
068500940706     C                   Z-ADD     G02DAT        V1CDFV
068600940706     C*
068700940706     C* SE IL FOGLIO HA DELLE SPUNTE E' IMPOSSIBILE MODIFICARE LA DATA
068800940706     C     WSPU          IFEQ      'S'
068900940706     C     FVVDFV        ANDNE     WDFV
069000940706     C                   SETON                                        4990
069100940706     C                   GOTO      ENDCT1
069200940706     C                   ENDIF
069300970602     C* E' IMPOSSIBILE MODIFICARE LA DATA ANCHE SE E' FOGLIO ARRIVI
069400970602     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
069500970602     C     *IN80         IFEQ      *OFF
069600970602     C     WNPG          IFEQ      2
069700970602     C     FVVDFV        ANDNE     WDFV
069800970602     C     KFVV2         SETLL     FNFVA02L                               32
069900970602     C     *IN32         IFEQ      *ON
070000970602     C                   SETON                                        5090
070100970602     C                   GOTO      ENDCT1
070200970602     C                   END
070300970602     C                   END
070400970602     C                   END
070500020422     C* CONTROLLO DATA COMPRESA NEL RANGE non per le distinte fogli 4
070600020422     c                   if        wnpg <> 4
070700030701     c                   If        *Inkf and
070800030701     c                                       (wdfv < wdtameno1 or
070900030701     c                                        wdfv > wdtapiu1)
071000000128     C                   SETON                                        905928
071100030701     C   80              MOVEA     ERR(2)        W78
071200030701     c  n80              Movea     Err(8)        W78
071300030701     c                   Move      '01'          W002A             2
071400000128     C                   MOVEA     W002A         W78(46)
071500000128     C                   MOVEA     W78           V1CMSG
071600940701     C                   GOTO      ENDCT1
071700930225E   1C                   ENDIF
071800020422     c                   else
071900020422      *distinta controlli sui limiti particolari
072000020422     c                   if        *in80
072100020422     c                   if        wdfv < waamg
072200020422     c                   SETON                                        905928
072300020422     c                   MOVEL     err(4)        v1cmsg
072400020422     C                   GOTO      ENDct1
072500020422     c                   endif
072600020422      *controllo range fra le date di oggi e quella immessa
072700020422     c                   eval      dadata = waamg
072800020422     c                   eval      adata  = wdfv
072900020422     c                   CALL      'XSRLAV8'
073000020422     c                   PARM                    Wdata8
073100030917     c                   if        giolav = 0
073200030917     c                   clear                   giorni
073300030917     c                   move      waamg         wdtiso1
073400030917     c                   move      wdfv          wdtiso2
073500030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
073600030917     c                   if        giorni > 30
073700030917     c                   SETON                                        905928
073800030917     c                   MOVEL     err(5)        v1cmsg
073900030917     C                   GOTO      ENDct1
074000030917     c                   endif
074100030917     c                   endif
074200020422     c                   if        giolav > §dgddc
074300020422     c                   SETON                                        905928
074400020422     c                   MOVEL     err(5)        v1cmsg
074500020422     C                   GOTO      ENDct1
074600020422     c                   end
074700020422     c                   end
074800110103      *
074900020422     c                   end
075000910320      *
075100940701     C*
075200940701     C* RICHIESTO ANNULLAMENTO FOGLIO:
075300940701     C*
075400950919     C* SE ESISTONO SPUNTE IMPOSSIBILE ANNULLARE FOGLIO: SEMPRE!!
075500940701     C   KQWSPU          IFEQ      'S'
075600940701     C                   SETON                                        4190
075700940701     C                   GOTO      ENDCT1
075800950919     C                   ENDIF
075900940701     C*
076000940701     C* SE DISTINTA NON POSSO ANNULLARE SE PRESENTI BOLLE
076100941027     C   KQWNPG          IFEQ      4
076200020910     c                   Z-add     V0cfgs        Wifp
076300940701     C                   Z-ADD     V0CNFV        VIDNDC            7 0
076400020910     c     Karb78        Setll     Fnarb78l                               32
076500010314     C   32              SETON                                        5590
076600940701     C   32              GOTO      ENDCT1
076700010314     C* NON POSSO ANNULLARE ANCHE SE PRESENTI O.R.M.
076800010314     C     WORM          IFEQ      'S'
076900010314     C                   SETON                                        6590
077000010314     C                   GOTO      ENDCT1
077100010314     C                   ENDIF
077200940701     C                   END
077300980112     C* E' IMPOSSIBILE ANNULLARE SE E' FOGLIO ARRIVI
077400980112     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
077500980112     C   KQWNPG          IFEQ      2
077600980112     C     KFVV2         SETLL     FNFVA02L                               32
077700980112     C     *IN32         IFEQ      *ON
077800980112     C                   SETON                                        6490
077900980112     C                   GOTO      ENDCT1
078000980112     C                   END
078100980112     C                   END
078200900515      *
078300940701     C     ENDCT1        ENDSR
078400900515      *-----------------------------------------------------***********
078500900515      * REGISTRAZIONE RECORD                                *  REGIS  *
078600900515      *-----------------------------------------------------***********
078700900515     C     REGIS         BEGSR
078800900515      *
078900910318     C                   MOVEL     *BLANK        FVVATB
079000940701     C                   Z-ADD     V0CNFV        FVVNFV
079100940701     C                   Z-ADD     WDFV          FVVDFV
079200160928     c                   clear                   I
079300160928     C* PER CATEGORIA 3 VERIFICO IL TIPO INVENTARIO SE È DA SCRIVERE COSÌ COME È
079400160928     c*  oppure devo impostare il campo FPP
079500160928    1c                   if        *in02
079600160928     c                   eval      i=%lookup(v1cspg:tin)
079700160928    1c                   endif
079800920806     C*
079900160928    1C     V1CDSF        IFEQ      *BLANKS
080000940701     C                   MOVEL     *BLANKS       FVVDSF
080100160928
080200950314     C* DESCRIZIONE INVENTARIO: SE NON C'E' PRENDO DA TIPO INVENTARIO
080300160928    2C                   if        *in02 and i>0
080400160928     C                   MOVEL     DbIN(I)       FVVDSF
080500160928     c
080600160928   x2C                   ELSE
080700950320     C* DESCRIZIONE FOGLIO: SE NON C'E' PRENDO DA CATEGORIA FOGLIO
080800950320     C                   MOVEL     V1DNPG        FVVDSF
080900160928    2C                   endif
081000160928     c
081100160928   x1C                   ELSE
081200940701     C                   MOVEL     V1CDSF        FVVDSF
081300160928    1C                   endif
081400911129     C*
081500050512     c   80              clear                   dfvvspn
081600050512     C                   MOVEL     V1CSPN        §FVVSPN
081700050512     c                   movel     dfvvspn       fvvspn
081800160928     C                   IF        NOT *IN02
081900160928     C                   CLEAR                   FVVSPG
082000160928     C                   ELSE
082100160928     C                   MOVEL     V1CSPG        FVVSPG
082200160929     c                   clear                   fvvfpp
082300161006     c***                if        I>0 and
082400161006     c***                          rif_in(I)<>*blanks
082500161006     c***                movel     rif_in(i)     fvvspg
082600161006     c***                move      v1cspg        fvvfpp
082700161006     C***                ENDIF
082800160928     C                   ENDIF
082900160928     C
083000911016     C                   MOVEL     *BLANKS       FVVFCF
083100911112     C   KA              MOVE      'S'           FVVFCF
083200940701     C   80              MOVEL     V0CNPG        FVVNPG
083300911016     C                   MOVEL     'S'           FVVFSC
083400950123     C                   Z-ADD     SIMFEL        FVVFLE
083500900515      *
083600020918      * In immissione foglio mi reimposto il p.o. in gestione xchè al momento dei controlli
083700020918      * sul numero staccato fvvfgs non è più il p.o. gestione del video impostato nella
083800020918      * routine riempi
083900020918     c                   Movel     V0cFgs        FvvFgs
084000900515      *
084100950320     C   80              WRITE     FNFVV000                                     IMMISSIONE
084200160928
084300000512      *SCRITTURA FOGLIO DI TIPO 8 UGUALE A 4 PER SPUNTE RIENTRO
084400000512     C     *IN80         IFEQ      *ON
084500000512     C     FVVNPG        ANDEQ     4
084600000512     C                   MOVE      8             FVVNPG
084700000512     C                   WRITE     FNFVV000
084800000512     C                   END
084900911004      *
085000940701     C  N80              UPDATE    FNFVV000                                     VARIAZIONE
085100000512      *COPIA MODIFICHE DA FOGLIO 4 A FOGLIO 8
085200000512     C     *IN80         IFEQ      *OFF
085300000512     C     FVVNPG        ANDEQ     4
085400000512     C                   MOVE      FVVDS         FVVDS2
085500000512     C                   MOVE      '8'           WNPG
085600000512     C     KFVV          CHAIN     FNFVV000                           80
085700000512     C     *IN80         IFEQ      *OFF
085800000512     C                   MOVE      FVVDS2        FVVDS
085900000512     C                   MOVE      8             FVVNPG
086000000512     C                   UPDATE    FNFVV000
086100000512     C                   MOVE      '4'           WNPG
086200000512     C     KFVV          SETLL     FNFVV000
086300000515     C                   MOVE      FVVDS2        FVVDS
086400000512     C                   END
086500000512     C                   END
086600000512     C*
086700900515     C                   ENDSR
086800900515      *-----------------------------------------------------***********
086900900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
087000900515      *-----------------------------------------------------***********
087100900515     C     RIEMPI        BEGSR
087200940712     C* EMETTO LA DESCRIZIONE DELLO STATO FOGLIO-INS/VARIAZ/ANNULL/BLOC
087300940712     C                   SETON                                        06
087400941027     C                   SETOFF                                       07
087500940712     C*
087600940701     C                   CLEAR                   WSPU
087700010314     C                   CLEAR                   WORM
087800160929     C                   CLEAR                   SAVSPG
087900960429     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
088000960429     C* IDD
088100960429     C                   MOVEL     '7A'          COD
088200960429     C                   MOVEL(P)  V0CFGS        KEY
088300960429     C     KTAB2         CHAIN     TABEL                              31
088400960429     C  N31              MOVEL     TBLUNI        DS7AF
088500900515      *
088600911016     C* SE INVENTARIO EMETTO CAMPO DELLA SOTTOSPECE
088700160928    1C     V0CNPG        IFEQ      '3'
088800911003     C                   SETON                                        02
088900160928     c* i comandi di riapertra e chiusura sono gestiti da tabella
089000160928    2c                   if        not *in80
089100160928     c* se il campo FPP è pieno devo cercare la corretta tipologia
089200161006     c***                if        fvvfpp<>*blanks
089300161006     c***                eval      I=%lookup(fvvfpp:tin)
089400161006     C***                MOVEL     FVVfpp        V1CSPG                          sottotipo
089500161006     C***                MOVEL     FVVfpp        SAVSPG                          sottotipo
089600161006     c***                else
089700160929     c                   eval      I=%lookup(fvvspg:tin)
089800160928     C                   MOVEL     FVVSPG        V1CSPG                          sottotipo
089900160929     C                   MOVEL     FVVspg        SAVSPG                          sottotipo
090000161006     c***                endif
090100160928     c
090200160928    3c                   if        apin(i)='S'
090300160928     c                   seton                                        11
090400160928    3c                   endif
090500160928    3c                   if        chin(i)='S'
090600160928     c                   seton                                        12
090700160928    3c                   endif
090800160928    2c                   endif
090900160928     c
091000960429     C* FOGLIO IMP
091100960429     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
091200160928     C**N80FVVSPG        IFEQ      'P'
091300160928     C**                 SETON                                        1211
091400160928     C**                 END
091500001115     C* FOGLIO IMI
091600001115     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
091700160928     C**N80FVVSPG        IFEQ      'I'
091800160928     C**                 SETON                                        1211
091900160928     C**                 END
092000960429     C* FOGLIO IMA
092100960429     C* EMETTO CMD DI RIAPERTURA
092200961113     C* EMETTO ANCHE COMANDO DI CHIUSURA MA SOLO SE TUTTE LE SPUNTE
092300961113     C*  CONTENUTE NELL'IMA HANNO DATA ARRIVO > 300
092400160928     C**N80FVVSPG        IFEQ      'A'
092500160928     C**                 SETON                                        1112
092600160928     C**                 ENDIF
092700960626     C* FOGLIO IMG
092800960626     C* EMETTO CMD DI RIAPERTURA
092900160928     C**N80FVVSPG        IFEQ      'G'
093000160928     C**                 SETON                                        11
093100160928     C**                 ENDIF
093200160928    1C                   ENDIF
093300940701     C*
093400940712      * S O L O   I N   V A R I A Z I O N E
093500940712     C     *IN80         IFEQ      *OFF
093600940712     C* DESCRIZIONE A VIDEO
093700940712     C                   MOVEL     CVARIA        V1DTFV
093800940712     C*
093900940712     C* SE ANNULLATO PROTEGGO CAMPI
094000940712     C     FVVATB        IFEQ      '*'
094100940712     C                   SETON                                        52  99
094200940712     C                   MOVEL     CANNUL        V1DTFV
094300940712     C                   END
094400940712     C*
094500940712     C* FOGLIO CHIUSO
094600940712     C     FVVFCF        IFEQ      'S'
094700940712     C                   SETON                                        53  99
094800940712     C                   END
094900940712     C* FOGLIO BLOCCATO
095000940712     C     FVVFCF        IFNE      'S'
095100940712     C     FVVFCF        ANDNE     ' '
095200940712     C                   MOVEL     CBLOCC        V1DTFV
095300940712     C                   ENDIF
095400940712     C*
095500960429     C* 12 E 11 ON - EMETTO F1 O F18 ANCHE PER FOGLIO ENTRATA
095600940712     C     V0CNPG        IFEQ      '5'
095700960429     C                   SETON                                        1211
095800960429     C                   ENDIF
095900960429     C*
096000960429     C* 11 ON - FACCIO RIAPRIRE IL FOGLIO IDA
096100960429     C*         SOLO SE NON HO GIA' FATTO L'IDD
096200960429     C     V0CNPG        IFEQ      '2'
096300960429     C                   SETON                                        11
096400960429     C                   ENDIF
096500941027     C*
096600940712     C                   MOVE      FVVDSF        V1CDSF                          DESCRIZIONE
096700050512     c                   clear                   dfvvspn
096800050512     C                   MOVEL     FVVSPN        dfvvspn                         SPUNTATORE
096900050512     C                   MOVEL     §fvvspn       V1CSPN                          SPUNTATORE
097000911004     C*
097100940712     C                   MOVE      FVVDFV        G02INV                          DATA
097200911004     C                   MOVE      *ZEROS        G02DAT
097300911004     C                   MOVEL     3             G02ERR
097400940706     C                   CALL      'XSRDA8'
097500911004     C                   PARM                    WLBDAT
097600940701     C                   Z-ADD     G02DAT        V1CDFV
097700030701     c                   ExSr      Sr_MinMax
097800940701     C*
097900940701     C* SEGNALO SE SUL FOGLIO ESISTONO SPUNTE
098000000128     C                   Z-ADD     V0CNFV        WNFV
098100070112     C     KBRV          SETLL     FNBRV000                               07
098200911004      *
098300940701     C* WSPU = "S"  --> ESISTONO SPUNTE
098400941027     C     *IN07         IFEQ      *ON
098500940701     C                   MOVEL     'S'           WSPU              1
098600910318     C                   END
098700010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
098800010314     C     WSPU          IFEQ      *BLANKS
098900010314     C     KORM          SETLL     FNORM06L
099000010314     C     KORM          READE     FNORM06L                               31
099100010314    3C     *IN31         DOWEQ     *OFF
099200010314     C                   CLEAR                   TIBS02
099300010314     C                   CLEAR                   DFAR
099400010314     C                   MOVEL     'C'           T02MOD
099500010314     C                   MOVEL     KNSIF         T02SIF
099600010314     C                   MOVEL     'FAR'         T02COD
099700010314     C                   MOVEL     ORMFAO        T02KE1
099800010314     C                   CALL      'TIBS02R'
099900010314     C                   PARM                    KPJBA
100000010314     C                   PARM                    TIBS02
100100010314     C     T02ERR        IFEQ      *BLANKS
100200010314     C                   MOVEL     T02UNI        DFAR
100300010314     C                   ENDIF
100400010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
100500010314     C* DISTINTA E' PRESENTE UN O.R.M.
100600010314    4C     §FARAF        IFNE      'S'
100700010314     C                   SETON                                        31
100800010314     C                   MOVEL     'S'           WORM              1
100900010314   X4C                   ELSE
101000010314     C     KORM          READE     FNORM06L                               31
101100010314    4C                   ENDIF
101200010314    3C                   ENDDO
101300010314    2C                   ENDIF
101400940712     C*
101500940712     C                   ELSE
101600940712     C*
101700940712     C* I M M I S S I O N E
101800940712     C*
101900940712     C* DESCRIZIONE A VIDEO
102000940712     C                   MOVEL     CINSER        V1DTFV
102100940712     C* FILIALE GESTIONE
102200940712     C                   MOVEL     V0CFGS        FVVFGS
102300940712     C                   ENDIF
102400910319     C*
102500910319     C                   ENDSR
102600911004      *-----------------------------------------------------***********
102700940701      * CARICO TABELLE                                      *  CARTAB *
102800911004      *-----------------------------------------------------***********
102900940701     C     CARTAB        BEGSR
103000161017     c***
103100161017     c* verifico se sono utente EDP o 001
103200161017     c***
103300161017     c                   if        %subst(knmus:1:3)='EDP' or dutpou=001
103400161017     c                   seton                                        09
103500161017     c                   endif
103600161017     c
103700940701     C***
103800940701     C* CARICO I TIPI INVENTARIO
103900940701     C***
104000911004     C                   MOVEL     *BLANKS       VIDSP1
104100911004     C                   MOVEL     *BLANKS       DESSP1
104200911004     C                   MOVEL     *BLANKS       VIDSP2
104300911004     C                   MOVEL     *BLANKS       DESSP2
104400911004     C                   MOVEL     *BLANKS       VIDSP3
104500911004     C                   MOVEL     *BLANKS       DESSP3
104600911004     C                   MOVEL     *BLANKS       VIDSP4
104700911004     C                   MOVEL     *BLANKS       DESSP4
104800911004     C                   MOVEL     *BLANKS       VIDSP5
104900911004     C                   MOVEL     *BLANKS       DESSP5
105000911004     C                   MOVEL     *BLANKS       VIDSP6
105100911004     C                   MOVEL     *BLANKS       DESSP6
105200911004     C*
105300911004     C                   MOVEL     '7B'          COD
105400161017     C                   Z-ADD     1             I                 3 0
105500911004     C     KTAB          SETLL     TABEL
105600911004     C     KTAB          READE     TABEL                                  30
105700911004     C*
105800911004     C     *IN30         DOWEQ     '0'
105900911004     C                   MOVEL     TBLKEY        TIN(I)
106000160928     C                   MOVEL     TBLUNI        DS7B
106100160928     C                   MOVEL     §7BDES        DIN(I)
106200161006     C*******            MOVEL     §7Brifer      rif_IN(I)
106300160928     C                   MOVE      §7Buti        uIN(I)
106400160928     C                   MOVE      §7Bdesb       dbIN(I)
106500160928     C                   MOVE      §7Briap       apIN(I)
106600160928     C                   MOVE      §7Bchius      chIN(I)
106700911004     C*
106800911004     C                   ADD       1             I
106900911004     C     KTAB          READE     TABEL                                  30
107000911004     C                   END
107100160929
107200911004     C* EMETTO A VIDEO I PRIMI 6
107300160929     c                   z-add     1             I
107400160929     c                   dow       tin(i)<>*blanks
107500160929     C     TIN(I)        IFNE      *BLANKS
107600160929     c     uin(i)        andne     'N'
107700161017     c*
107800161017     c                   if        *in09 or (not *in09 and uin(I)<>'1' )
107900160929     c                   select
108000160929     c                   when      vidsp1=*blanks
108100160929     C                   MOVEL     TIN(I)        VIDSP1
108200911004     C                   MOVE      ' ='          VIDSP1
108300160929     C                   MOVEL     DIN(I)        DESSP1
108400160929     c                   when      vidsp2=*blanks
108500160929     C                   MOVEL     TIN(I)        VIDSP2
108600160929     C                   MOVE      ' ='          VIDSP2
108700160929     C                   MOVEL     DIN(I)        DESSP2
108800160929     c                   when      vidsp3=*blanks
108900160929     C                   MOVEL     TIN(I)        VIDSP3
109000160929     C                   MOVE      ' ='          VIDSP3
109100160929     C                   MOVEL     DIN(I)        DESSP3
109200160929     c                   when      vidsp4=*blanks
109300160929     C                   MOVEL     TIN(I)        VIDSP4
109400160929     C                   MOVE      ' ='          VIDSP4
109500160929     C                   MOVEL     DIN(I)        DESSP4
109600160929     c                   when      vidsp5=*blanks
109700160929     C                   MOVEL     TIN(I)        VIDSP5
109800160929     C                   MOVE      ' ='          VIDSP5
109900160929     C                   MOVEL     DIN(I)        DESSP5
110000160929     c                   when      vidsp6=*blanks
110100160929     C                   MOVEL     TIN(I)        VIDSP6
110200160929     C                   MOVE      ' ='          VIDSP6
110300160929     C                   MOVEL     DIN(I)        DESSP6
110400160929     c                   leave
110500160929     c                   endsl
110600160929     c                   endif
110700161017     c                   endif
110800160929     c
110900160929     c                   add       1             I
111000160929
111100160929     C                   ENDdo
111200940701     C***
111300160929     C* VEDO SE SONO SIMFEL O 2 livello
111400940701     C***
111500110103     c     dutLPO        ifeq      '2'
111600110103     c     dutLPO        oreq      *blanks
111700110103     c                   movel     dutPOU        v0cfgs
111800920806     C                   SETON                                        05
111900930209     C                   ELSE
112000940701     C                   MOVEL     SIMFEL        V0CFGS
112100920806     C                   END
112200941027     C*
112300940701     C     V0CFGS        CHAIN     AZORG                              31
112400940713     C  N31              MOVEL     ORGDES        V1DFGS
112500940701     C***
112600920806     C* CARICO CATEGORIE FOGLI
112700940701     C***
112800920806     C                   MOVEL     '7N'          COD
112900920806     C                   Z-ADD     0             I
113000920806     C     KTAB          SETLL     TABEL
113100920806     C     KTAB          READE     TABEL                                  31
113200940701     C*
113300920806     C     *IN31         DOWEQ     '0'
113400920806     C                   MOVEL     TBLUNI        DS7N
113500920806     C* SOLO SE ABILITATA PER FOGLI VARI
113600920806     C     §7NFFV        IFEQ      'S'
113700921228     C     §7NUTI        ANDNE     '9'
113800920806     C                   ADD       1             I
113900920806     C                   MOVEL     ' ='          DEC(I)
114000940712     C                   MOVEL     W001A         DEC(I)
114100920806     C                   MOVE      §7NDE2        DEC(I)
114200920806     C     I             IFEQ      6
114300920806     C                   SETON                                        31
114400920806     C                   END
114500920806     C                   END
114600920806     C*
114700920806     C  N31KTAB          READE     TABEL                                  31
114800920806     C                   END
114900920806     C*
115000940701     C* IMPOSTO LE DESCRIZIONI A VIDEO
115100920806     C     DEC(1)        IFNE      *BLANKS
115200920806     C                   MOVEL     DEC(1)        DESCA1
115300920806     C     DEC(2)        IFNE      *BLANKS
115400920806     C                   MOVEL     DEC(2)        DESCA2
115500920806     C     DEC(3)        IFNE      *BLANKS
115600920806     C                   MOVEL     DEC(3)        DESCA3
115700920806     C     DEC(4)        IFNE      *BLANKS
115800920806     C                   MOVEL     DEC(4)        DESCA4
115900920806     C     DEC(5)        IFNE      *BLANKS
116000920806     C                   MOVEL     DEC(5)        DESCA5
116100920806     C     DEC(6)        IFNE      *BLANKS
116200920806     C                   MOVEL     DEC(6)        DESCA6
116300920806     C                   END
116400920806     C                   END
116500920806     C                   END
116600920806     C                   END
116700920806     C                   END
116800920806     C                   END
116900940701     C***
117000940711     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
117100940701     C***
117200940711     C                   TIME                    W0120            12 0
117300940711     C                   MOVE      W0120         WDAT              6 0
117400940711     C*
117500940711     C                   Z-ADD     WDAT          G02DAT
117600940701     C                   MOVEL     *BLANK        G02ERR
117700940701     C                   CALL      'XSRDA8'
117800940701     C                   PARM                    WLBDAT
117900940711     C* DATA DEL GIORNO IN AAAAMMGG
118000940711     C                   Z-ADD     G02INV        WAAMG
118100940701     C*
118200940711     C* DATA DEL GIORNO IN GGMMAAAA
118300940701     C                   Z-ADD     G02DAT        UDATE8
118400920806     C*
118500940705     C                   MOVE      TES(1)        V0CTES
118600940701     C*
118700911004     C                   ENDSR
118800940701      *-----------------------------------------------------***********
118900940701      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
119000940701      *-----------------------------------------------------***********
119100940701     C     CTRFGS        BEGSR
119200940708     C                   SETOFF                                       90
119300950206     C     V0CFGS        LOOKUP    L1                                     30
119400950206     C     *IN30         IFEQ      *OFF
119500950206     C     V0CFGS        OREQ      0
119600950206     C                   MOVEL     ERR(1)        V0CMSG
119700941027     C                   SETON                                        439028
119800941027     C                   GOTO      ENDCTF
119900941027     C                   ENDIF
120000941027     C* DECODIFICA
120100941027     C     V0CFGS        CHAIN     AZORG                              31
120200941027     C  N31              MOVEL     ORGDES        V1DFGS
120300940701     C*
120400940701     C     ENDCTF        ENDSR
120500940701      *-----------------------------------------------------***********
120600940712      * CONTROLLO FORMATO 0                                    CTRD00
120700940701      *-----------------------------------------------------***********
120800940701     C     CTRD00        BEGSR
120900940701     C                   SETOFF                                       90
121000940701     C*
121100940701     C* CATEGORIA OBBLIGATORIA
121200940701     C*
121300940701     C     V0CNPG        IFEQ      ' '
121400940701     C                   SETON                                        4290
121500940701     C                   GOTO      ENDCT0
121600940701     C                   END
121700940701     C*
121800940701     C* RICERCA
121900940701     C     V0CNPG        IFEQ      '?'
122000940701     C                   MOVEL     CODUT         §KUT
122100940701     C                   MOVEL     '7N'          §COD
122200940701     C                   MOVE      ' '           V0CNPG
122300940701     C                   MOVEL     *BLANKS       §KEY
122400940701     C                   CALL      'X§TABER'
122500940701     C                   PARM                    §KUT              1 0
122600940701     C                   PARM                    §COD              2
122700940701     C                   PARM                    §KEY              8
122800940701     C                   PARM                    §DES             30
122900940701     C                   MOVEL     §KEY          V0CNPG
123000940701     C                   SETON                                        90
123100940701     C                   GOTO      ENDCT0
123200940701     C                   END
123300940701     C* CONTROLLO
123400940701     C                   MOVEL     '7N'          COD
123500940701     C                   MOVEL     *BLANKS       KEY
123600940701     C                   MOVEL     V0CNPG        KEY
123700940701     C* INESISTENTE
123800940701     C     KTAB2         CHAIN     TABEL                              42
123900940701     C* ANNULLATA
124000940701     C  N42TBLFLG        IFNE      ' '
124100940701     C                   SETON                                        42
124200940701     C                   ELSE
124300940701     C                   MOVEL     TBLUNI        DS7N
124400940701     C* NON E' CATEGORIA DI FOGLI VARI
124500940701     C     §7NFFV        IFNE      'S'
124600940701     C                   SETON                                        42
124700940701     C                   END
124800940701     C                   END
124900940701     C* 42 ON - ERRORE
125000940701     C   42              SETON                                        90
125100940701     C   42              GOTO      ENDCT0
125200940701     C*
125300940701     C* NON TUTTE LE CATEGORIE SONO UTILIZZABILI DAI 2 LIVELLI
125400940701     C     §7NUTI        IFEQ      '9'
125500940701     C                   SETON                                        5690
125600940701     C                   GOTO      ENDCT0
125700940701     C                   END
125800041026     c* categoria  4 non più utilizzabile da questo pgm
125900041026     c     v0cnpg        ifeq      '4'
126000041026     c                   seton                                        6390
126100041026     c                   goto      endct0
126200041026     c                   endif
126300041026     c
126400940701     C*
126500940701     C* FILIALE IN GESTIONE E' SIMFEL
126600940701     C     SIMFEL        IFEQ      V0CFGS
126700940701     C     §7NUTI        IFEQ      '2'
126800940701     C                   SETON                                        5790
126900940701     C                   GOTO      ENDCT0
127000940701     C                   END
127100940701     C*
127200940701     C                   ELSE
127300940701     C*
127400940701     C* FILIALE IN GESTIONE NON E' SIMFEL
127500940701     C     §7NUTI        IFEQ      '1'
127600940713     C                   SETON                                        5890
127700940713     C                   GOTO      ENDCT0
127800940701     C                   END
127900940701     C                   END
128000040227     c* Se §7NUITI='G' categoria solo in  gestione ma non in immissione
128100040227     c                   if        *inkj and §7nuti='G'
128200040227     C                   SETON                                        6690
128300040227     C                   GOTO      ENDCT0
128400040227     c                   endif
128500940701     C*
128600940701     C                   MOVEL     §7NDES        V1DNPG                         DESCRIZIONE
128700940701     C                   MOVEL     V0CNPG        WNPG                           IN CAMPO NUM
128800940701     C*
128900940713     C* FOGLIO
129000950320     C                   SETON                                        80
129100950320     C                   SETOFF                                       60
129200950320     C  NKJKFVV          CHAIN     FNFVV000                           8060
129300940701     C*
129400940701     C* RECORD GIA' VINCOLATO
129500940712     C   60              SETON                                        90
129600940712     C   60              GOTO      ENDCT0
129700950628     C* SE FOGLIO BLOCCATO --> NON POSSO MANUTENZIONARLO
129800950628     C     *IN80         IFEQ      *OFF
129900950628     C     FVVFCF        ANDNE     'S'
130000950628     C     FVVFCF        ANDNE     ' '
130100950628     C                   SETON                                        9061
130200950628     C                   UNLOCK    FNFVV01L
130300950628     C                   GOTO      ENDCT0
130400950628     C                   ENDIF
130500940701     C*
130600940701      *
130700940713      * NUMERO FOGLIO INESISTENTE IN VARIAZIONE
130800950320     C  NKJ*IN80         IFEQ      *ON
130900950320     C                   SETON                                        3890
131000950320     C                   GOTO      ENDCT0
131100940701     C                   ENDIF
131200940701     C*
131300940701     C     ENDCT0        ENDSR
131400940701      *-----------------------------------------------------***********
131500940701      * INZSR INIZIALE                                         INZSR
131600940701      *-----------------------------------------------------***********
131700940701     C     *INZSR        BEGSR
131800940701      ***
131900940701      * CHIAVI
132000940701      ***
132100940701     C     KFVV          KLIST
132200940701     C                   KFLD                    WNPG
132300940701     C                   KFLD                    V0CNFV
132400941027     C                   KFLD                    V0CFGS
132500020917     c     Kfvv1         Klist
132600020917     c                   Kfld                    Wnpg
132700020917     c                   Kfld                    V0cnfv
132800020917     c                   Kfld                    Wfgs
132900000713     C     KFVV8         KLIST
133000000713     C                   KFLD                    WNPG8
133100000713     C                   KFLD                    FVVNFV
133200000713     C                   KFLD                    FVVFGS
133300000713     C                   Z-ADD     8             WNPG8
133400000128     C* N.B. QUANDO IL NUMERO FOGLIO DIVENTERA' LUNGO 6 ANCHE PER I
133500000128     C*      FOGLI (FNFVV FNFVA) LA KLIST KBRV SI POTRÀ CANCELLARE
133600000128     C*      PERCHE' KFVV SI POTRA' UTILIZZARE AL SUO POSTO KFVV
133700000128     C     KBRV          KLIST
133800000128     C                   KFLD                    WNPG
133900000128     C                   KFLD                    WNFV
134000000128     C                   KFLD                    V0CFGS
134100950104     C     KFVV2         KLIST
134200950104     C                   KFLD                    WNPG
134300950104     C                   KFLD                    V0CNFV
134400020917     c                   Kfld                    V0cFgs
134500950607     C     KDST          KLIST
134600950607     C                   KFLD                    V0CNFV
134700950607     C                   KFLD                    V0CFGS
134800940701     C     KTAB          KLIST
134900940701     C                   KFLD                    CODUT
135000940701     C                   KFLD                    COD               2
135100940701     C     KTAB2         KLIST
135200940701     C                   KFLD                    CODUT
135300940701     C                   KFLD                    COD               2
135400940701     C                   KFLD                    KEY               8
135500010314     C* ACCESSO FNORM06L
135600010314     C     KORM          KLIST
135700010314     C                   KFLD                    V0CFGS
135800010314     C                   KFLD                    WNFV
135900020910      * Fnarb
136000020910     c     Karb78        Klist
136100020910     c                   Kfld                    Wifp
136200020910     c                   Kfld                    Vidndc
136300030701      * Azcln01l
136400030701     c     kazcln        Klist
136500030701     c                   Kfld                    ktfp
136600030701     c                   Kfld                    ktfa
136700030701     c                   Kfld                    kann
136800030701     c                   Kfld                    kmes
136900110103      *
137000940701     C* LIKE DEFN
137100940711     C* DATA DEL GIORNO A 8 AAAAMMGG
137200940711     C     *LIKE         DEFINE    FVVDFV        WAAMG
137300940711     C* DATA DEL GIORNO A 8 GGMMAAAA
137400000718     C     *LIKE         DEFINE    BRVLNA        SAVLNA
137500000718     C     *LIKE         DEFINE    BRVLNP        SAVLNP
137600000718     C     *LIKE         DEFINE    FVVDS         FVVDS2
137700940705     C     *LIKE         DEFINE    FVVDFV        UDATE8
137800940701     C     *LIKE         DEFINE    FVVDFV        WDFV
137900940701     C     *LIKE         DEFINE    FVVNPG        WNPG
138000000713     C     *LIKE         DEFINE    FVVNPG        WNPG8
138100000128     C     *LIKE         DEFINE    BRVNFV        WNFV
138200020422     C***
138300020422     C     KTBE          KLIST
138400020422     C                   KFLD                    KCOD1
138500020422     C                   KFLD                    KKE1
138600020422     C     *LIKE         DEFINE    TBEKE1        KKE1
138700020422     C     *LIKE         DEFINE    TBECOD        KCOD1
138800020422     C*  tabelle dgd x reperimento limiti chiusura spedizioni
138900020422     C                   MOVEL     'DGD'         KCOD1
139000020422     C                   MOVEL     *BLANKS       KKE1
139100020422     C                   MOVEL     '1'           KKE1
139200020422     C     KTBE          CHAIN     TNTBE01L                           31
139300020422     C     *IN31         IFEQ      '0'
139400020422     C                   MOVEL     TBEUNI        DDGD
139500020422     C                   END
139600940701     C                   ENDSR
139700110103
139800110103      *---------------------------------------------------------------*
139900110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
140000110103      *---------------------------------------------------------------*
140100110103     c     sr_DatiJob    BEGSR
140200110103      *
140300110103     c     *dtaara       define    §azute        AZUTEds
140400110103     c     *dtaara       define    §datiute      dDatiUte
140500110103      *
140600110103     c                   in(E)     *dtaara
140700110103     c                   IF        %Error or RSUT = *blanks
140800110103     c                   clear                   TIBS34ds
140900110103     c                   call      'TIBS34R'
141000110103     c                   parm                    TIBS34ds
141100110103     c                   in        *dtaara
141200110103     c                   ENDIF
141300110103      *
141400110103     c                   ENDSR
141500000512**
141600920709 ***   GESTIONE  FOGLI  VARI   ***
141700950206**
141800950206Filiale gestione errata
141900000128Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Corrente
142000000512Immissione/manutenzione record non gestita per i fogli di tipo 8
142100020422Data inferiore alla data odierna impossibile l'inserimento x fogli Cat 4
142200020422Data oltre il limite consentito per i fogli categoria 4
142300020909Manca il numeratore dei Fogli xxxxxxxxxx: TELEFONARE IN SEDE!!!!               6
142400020909Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli xxxxxxxxxx   7
142500030701Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Foglio
