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
003200180130     d
003300180130     d Trul09ds      e ds
003400180130     d  T09                   19    108  0 dim(30)
003500900517      *
003600900514     D KPJBA         E DS
003700161124     d
003800180112     d TRULVPODS     e ds
003900180112     D  skFilOk               16    765    dim(250)
004000161124     d
004100110103      *
004200110103      * - Parametri per richiamo al pgm di controllo profilo utenti
004300110103     d TIBS34ds      e ds
004400110103      * - Ds di riferimento al file esterno AzUte00F
004500110103     d AZUTEds       e ds                  ExtName(AzUte00F)
004600110103      * - Ds per dati organigramma
004700110103     d dDatiUte      e ds
004800900516      *
004900940802     D* DS STANDARD FOGLI VARI
005000940802     D DSLR01        E DS
005100160928     D
005200160928     D ds7b          E DS
005300940802     D*
005400941027     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
005500941027     D DSLV50        E DS                  EXTNAME(FNLV50DS)
005600010314     D TIBS02        E DS                  EXTNAME(TIBS02DS)
005700940802     D*
005800010314     D DFAR          E DS
005900010314     D  §FARDS       E                     EXTFLD(D§FARDES)
006000010314     D  §FARFB       E                     EXTFLD(D§FARFFB)
006100010314     D  §FARNF       E                     EXTFLD(D§FARNONF)
006200010314     D  §FARMN       E                     EXTFLD(D§FARMAN)
006300010314     D  §FARST       E                     EXTFLD(D§FARSTP)
006400010314     D  §FARAF       E                     EXTFLD(D§FARAFVV)
006500010314     D  §FARCF       E                     EXTFLD(D§FARCFVV)
006600020221     D  §FARNO       E                     EXTFLD(D§FARNOO)
006700020221     D  §FARUV       E                     EXTFLD(D§FARUVA)
006800020221     D  §FARDV       E                     EXTFLD(D§FARDVA)
006900020304     D OG143A        E DS                  EXTNAME(OG143) PREFIX(A)
007000960429     D DS7AF         E DS
007100960429     D DS7N          E DS
007200920805     D  §7NDE2                 1     10
007300000512     D FVVDS         E DS                  EXTNAME(FNFVV00F)
007400020422     d ddgd          e ds
007500020909     d Trul33Ds      e ds
007600020910     d Fnlr6Ds       e ds
007700110103     d*
007800050512     d* DS del campo fvvspn - spuntatore
007900050512     d Dfvvspn       e ds
008000900515      *
008100940701     D***
008200940701     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008300940701     D***
008400940705     D WLBDAT          DS                  INZ
008500940701     D  G02DAT                 1      8  0
008600940701     D  G02INV                 9     16  0
008700940701     D  G02ERR                17     17
008800940701     D  G02TGI                18     22  0
008900020422      *
009000020422     d Wdata8          DS
009100020422     d  dadata                 1      8  0
009200020422     d  adata                  9     16  0
009300020422     d  GioLav                17     21  0
009400940701     D***
009500940701     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
009600940701     D***
009700940705     D WGIDAT          DS                  INZ
009800940701     D  GIODAT                 1      8  0
009900940701     D  GIOINV                 9     16  0
010000940701     D  GIOTGI                17     21  0
010100920805     D TBLKEY          DS
010200940712     D  W001A                  1      1
010300950112     D PARA04          DS
010400950112     D  P04NFV                 1      5  0
010500950112     D  P04LNP                 6      8  0
010600950112     D  P04LNA                 9     11  0
010700950112     D  P04NRS                12     13  0
010800950112     D  P04NCD                14     20  0
010900950112     D  P04NCA                21     27  0
011000950112     D  P04NPS                28     29  0
011100950112     D  P04NPG                30     30  0
011200950112     D  P04FLG                31     31
011300950112     D  P04PGM                32     38
011400950112     D  P04FGS                39     41  0
011500101202     d* bolla
011600101202     d  p04bolp               42     44  0
011700101202     d  p04bors               45     46  0
011800101202     d  p04bosp               47     53  0
011900101202     d  p04boan               54     57  0
012000101202     D  P04ERR                58     58
012100101202     d*
012200950206     D* DS PER TRUL06R - CARICAMENTO £X
012300950206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012400950206     D  LIN                    1     90  0
012500950206     D                                     DIM(30)                              FIL. COMODO
012600020910
012700160929     d savspg          s                   Like(fvvspg)
012800180130     d savdfv          s                   Like(v1cdfv)
012900160929     d wifp            s                   Like(Arbifp)
013000020917     d Wfgs            s                   Like(FvvFgs)
013100020917     d Wok             s              1    inz('0')
013200020917     d xx              s              2  0
013300030701
013400030701     d                 ds
013500030701     d  ClnPom                 1     31
013600030701     d  Pom                    1     31    Dim(31)
013700110103     ***d                 ds
013800110103     ***d  ClnMat                 1     31
013900110103     ***d  Mat                    1     31    Dim(31)
014000030701
014100030701     d ktfp            s                   Like(FvvFgs)
014200030701     d ktfa            s                   Like(FvvFgs)
014300030701     d kann            s                   Like(ClnAnn)
014400030701     d kmes            s                   Like(ClnMes)
014500160929     d indx            s              3  0
014600160929     d wgiorno         s              2  0
014700030701     d wdtaeur         s               d   datfmt(*eur)
014800030701     d wdtaiso         s               d   datfmt(*iso)
014900030701     d wdtameno1       s              8  0
015000030701     d wdtapiu1        s              8  0
015100030917     d wdtiso1         s               d
015200030917     d wdtiso2         s               d
015300030701
015400940712     D***
015500940712     D* CONSTANTI
015600940712     D***
015700940712     D CINSER          C                   CONST(' Inserimento ')
015800940712     D CVARIA          C                   CONST(' Variazione  ')
015900940712     D CANNUL          C                   CONST('  Annullato  ')
016000940712     D CBLOCC          C                   CONST('  Bloccato   ')
016100900515      *---------------------------------------------------------------*
016200910318      * INDICATORI USATI
016300910318      *---------------------------------------------------------------*
016400940712      * 01    = PGM RICHIAMATO
016500921228      * 02    = FOGLIO INVENTARIO
016600110103      **** 03    = FOGLIO CONSEGNE
016700921228      * 04    = CAMBIO FILIALE IN GESTIONE PER CMD18
016800180112      * 05    = SONO UN secondo livellp
016900940712      * 06    = EMETTO LA DESCRIZIONE DELLO STATO FOGLIO
017000941027      * 07    = ESISTONO SPUNTE SUL FOGLIO
017100170124      * 09    = utente  abilitato all IPG da decofiipg. tolta la DECOFIIPG ma tengo gli indicatori
017200170124      * 10    = tutti abilitati all'IPG                                                  impostati
017300960429      * 11    = EMETTO TASTO FUNZIONALE F18 DI RIAPERTURA
017400960429      * 12    = EMETTO TASTO FUNZIONALE F1 DI CHIUSURA FV
017500950703      * 13    = UTILIZZATO PER CONTROLLARE CHE IL NUMERO DEL FOGLIO CHE
017600950703      *         SI VUOLE CREARE NON ESISTA GIA' IN FNFVV00F
017700180130      * 15    = Foglio arrivi incompatibile
017800031112      * 19    = Nasconde la richiesta del campo opzione ("1")
017900031112      *         (utilizzato dal pgm. FNLR02R, che utilizza FNLR01D)
018000180112      * 20/27 = indicatori sfl
018100180112      * 28    = EMETTO CAMPO MESSAGGI
018200180112      * 30/31 = DI COMODO
018300911129      * 38/51 = ERRORE
018400921228      * 52    = RECORD ANNULLATO
018500921228      * 53    = FOGLIO CHIUSO
018600961113      * 54/59 = ERRORE
018700940712      * 60    = RECORD GIA' VINCOLATO
018800960429      * 61    = FOGLIO BLOCCATO
018900180112      * 62/68 = ERRORE
019000180112      * 70/71 = utente  abilitato all INC da decofiinc.
019100921228      * 80    = IMMISSIONE   N80 = VARIAZIONE
019200921228      * 90    = ERRORE GENERICO
019300921228      * 98    = USATO IN INTERROGAZIONE
019400921228      * 99    = PROTEZIONE CAMPI
019500910318      *--------------------------------------------------------------*
019600000000     C     *ENTRY        PLIST
019700000000     C                   PARM                    KPJBA
019800940802     C                   MOVEL     KPJBU         DSLR01
019900900516      *
020000110103     C                   Z-ADD     1             CODUT             1 0
020100110103      *
020200110103      * Reperimento dati utente/aziendali
020300110103     c                   exsr      sr_DatiJob
020400940701     C* DESCRIZIONE AZIENDA
020500110103     C                   MOVEL     RSUT          V0CDSA
020600950206     C***
020700950206     C* CARICO TABELLA FILIALI GESTITE £1
020800950206     C***
020900950206     C                   CLEAR                   DSUL06
021000950206     C                   MOVE      '£1'          D06COD
021100950206     C                   MOVEL     SIMFEL        D06KEY
021200950206     C                   MOVEL     DSUL06        KPJBU
021300950206     C*
021400950206     C                   CALL      'TRUL06R'
021500950206     C                   PARM                    KPJBA
021600950206     C                   MOVEL     KPJBU         DSUL06
021700950206     C                   MOVEA     LIN           L1
021800900515      *---------------------------------------------------------------*
021900900515      * FLUSSO PRINCIPALE                                             *
022000900515      *---------------------------------------------------------------*
022100911004     C*
022200940701     C* CARICO TABELLE
022300940701     C                   EXSR      CARTAB
022400911004      *
022500900509     C     INIZIO        TAG
022600940701      * INIZIALIZZO VIDEATA
022700940701     C                   EXSR      INZ12
022800940701      *
022900940701      * 01 ON - PROGRAMMA RICHIAMATO
023000940802    1C     DLRRIC        IFEQ      'S'
023100910321     C                   SETON                                        01
023200940802     C                   MOVEL     DLRNFV        V0CNFV
023300940802     C                   MOVEL     DLRNPG        V0CNPG
023400941027     C                   MOVEL     DLRFGS        V0CFGS
023500940701    1C                   END
023600900524      *
023700900515     C     FMT00         TAG
023800940701      *
023900940701     C  N01              EXFMT     LR01D00
024000941027     C*
024100941027     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
024200941027     C                   SETOFF                                       4328
024300910318     C*
024400160929     C* F3  - FINE LAVORO
024500910318     C   KC              GOTO      FINE                                         F3=FINE
024600920806     C*
024700160929     C* F7  - INTERROGAZIONE FOGLI VARI
024800940701    2C   KG              DO
024900940802     C                   CLEAR                   DSLR01
025000920806     C*
025100940802     C                   MOVEL     DSLR01        KPJBU
025200940712     C                   CALL      'FNLR02R'
025300920806     C                   PARM                    KPJBA
025400940802     C                   MOVEL     KPJBU         DSLR01
025500940713     C*
025600920806     C                   GOTO      FMT00
025700940705     C                   ENDDO
025800920806     C*
025900160929     C* F18 - CAMBIO FILIALE
026000920806     C   KS              SETON                                        04
026100920806     C   KS              GOTO      FMT00
026200920709     C*
026300940701     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
026400940701     C   04              EXSR      CTRFGS
026500940701     C   04
026600940701     CAN 90              GOTO      FMT00
026700940701     C*
026800940701     C* CONTROLLO FORMATO 1
026900940701     C                   EXSR      CTRD00
027000940701     C   90              GOTO      FMT00
027100160929     C* F10 - IMMISSIONE IMPEDITA X NUMERO DI FOGLIO 8
027200000512     C     V0CNPG        IFEQ      '8'
027300000512     C                   SETON                                        9028
027400000512     C                   MOVEL     ERR(3)        V0CMSG
027500000512     C   90              GOTO      FMT00
027600000512     C                   END
027700910318     C*
027800910318     C* RIEMPIMENTO CAMPI PIU' DECODIFICA
027900910318     C*
028000910318     C                   EXSR      RIEMPI
028100910318     C*
028200900515     C     FMT01         TAG
028300900516      *
028400020422     C* SE DISTINTA NON POSSO VARIARE mai dal 23/04/02 *(SE PRESENTI BOLLE)*
028500020424     c                   setoff                                       97
028600020422     C     WNPG          IFEQ      4
028700020422     c     *in80         andeq     *off
028800020424     C                   SETON                                        97
028900020422     C                   END
029000020422      *
029100940701     C                   EXFMT     LR01D01
029200000128     C                   SETOFF                                       28
029300180112     c                   setoff                                       6859
029400941027     C*
029500160929     C* F21 - INTERROGO LE SPUNTE DEL FOGLIO RICHIESTO
029600941027     C     *INKV         IFEQ      *ON
029700950112     C                   CLEAR                   PARA04
029800950112     C                   Z-ADD     V0CNFV        P04NFV
029900950112     C                   Z-ADD     99            P04NRS
030000950112     C                   Z-ADD     99            P04NPS
030100950112     C                   MOVEL     V0CNPG        P04NPG
030200950112     C                   MOVEL     '1'           P04FLG
030300950112     C                   MOVEL     'FNLR01R'     P04PGM
030400950112     C                   Z-ADD     V0CFGS        P04FGS
030500950112     C                   MOVEL     PARA04        KPJBU
030600950112     C                   CALL      'FNLR04R'
030700950112     C                   PARM                    KPJBA
030800941027     C                   GOTO      FMT01
030900941027     C                   ENDIF
031000911030      *
031100160929      * F12 - RITORNO
031200940701     C     *INKL         IFEQ      *ON
031300940701     C* RILASCIO RECORD
031400940705     C  N80              UNLOCK    FNFVV01L
031500941027     C*
031600940701     C* VADO A INIZIO
031700940701     C  N01              GOTO      INIZIO
031800941027     C*
031900940701     C* SE RICHIAMATO VADO A FINE
032000941027     C   01              GOTO      FINE                                         RICH
032100940701     C                   ENDIF
032200911030      *
032300911030      *  CONTROLLI FORMATO1
032400940701     C                   EXSR      CTRD01
032500911030     C   90              GOTO      FMT01
032600911030      *
032700160928      * f1  - CHIUSURA IMP     SI ARRIVI
032800160929     c* Verifico se il sottotipo prevede la chiusura
032900160929    0c                   if        *inka
033000160929     c                   clear                   indx
033100160929    1c                   if        v1cspg<>*blanks
033200160929     c                   eval      Indx=%lookup(v1cspg:tin)
033300160929    1c                   endif
033400160929     c
033500180130     c                   if        v1cinc='SI' or
033600180130    1c                             v0cnpg='5'  or (indx>0 and chin(indx)= 'S')
033700160929     c
033800160929    1C** KAV1CSPG        IFEQ      'P'
033900160929     C**   V1CSPG        OREQ      'I'
034000160929     C**   V1CSPG        OREQ      'A'
034100160929     C**   V0CNPG        OREQ      '5'
034200020304      *-------------------------------------------------------------------
034300020304      * FOGLIO IMA: PERMETTO LA CHIUSURA SOLTANTO SE LE SPUNTE HANNO
034400020304      *             1) TUTTE LNA CON NETWORK ESTERO
034500020304      *-------------------------------------------------------------------
034600000718    2C     V1CSPG        IFEQ      'A'
034700000128     C                   Z-ADD     V0CNFV        WNFV
034800070112     C     KBRV          CHAIN     FNBRV02L                           31
034900000718    3C     *IN31         DOWEQ     *OFF
035000000718     C*
035100000718    4C     BRVLNA        IFNE      SAVLNA
035200000128     C     BRVLNA        CHAIN     AZORG01L                           30
035300020304     C  N30              MOVEL     ORGDE3        OG143A
035400020304     C   30              CLEAR                   OG143A
035500000718     C                   MOVEL     BRVLNA        SAVLNA
035600000718    4C                   ENDIF
035700000718     C*
035800020304    4C     A§OGNTW       IFNE      'EEX'
035900020304    4C     A§OGNTW       ANDNE     'EUP'
036000020304    4C     A§OGNTW       ANDNE     'FED'
036100020304    4C     A§OGNTW       ANDNE     'DPD'
036200000718     C                   SETON                                        5431
036300961113     C                   GOTO      FMT01
036400000718    4C                   ENDIF
036500160928     c
036600070112     C     KBRV          READE     FNBRV02L                               31
036700000718    3C                   ENDDO
036800000718    2C                   ENDIF
036900961113     C*
037000911112     C                   EXSR      REGIS
037100960429     C  N01              GOTO      INIZIO
037200911030     C                   GOTO      FINE                                         RICH
037300000718   X1C                   ELSE
037400911030     C                   SETON                                        51
037500911030     C                   GOTO      FMT01
037600160929    1C                   ENDif
037700160929    0C                   ENDif
037800940701      *
037900160928      * F6  - REGISTRA
038000940701     C   KF              DO
038100020917      * Prelevo il numero
038200020923     c                   if        *in80
038300020917     c                   ExSr      CrtNfv
038400020917     c   90              GoTo      Fmt01
038500020917     c                   ExFmt     Lr01W01
038600020923     c                   EndIf
038700940701     C                   EXSR      REGIS
038800940701     C  N01              GOTO      INIZIO
038900940701     C                   GOTO      FINE                                         RICH
039000940701     C                   END
039100900516      *
039200160928      * F5  - RIATTIVA
039300160928      * F16 - ANNULLA
039400160928      * F18 - RIAPERTURA IMP
039500940701     C     *INKE         IFEQ      *ON
039600940701     C     *INKQ         OREQ      *ON
039700940701     C     *INKS         OREQ      *ON
039800960429     C*
039900960626     C* PER FOGLIO IMA E FOGLIO IMG RIAPRO SOLO SE LA DATA FOGLIO
040000960429     C*  E' MAGGIORE DELLA DATA ESECUZIONE IDD
040100980402     C*  ANCHE PER FOGLIO ARRIVI, TANTO LE DATE DI ARRIVO REALE DEI
040200980402     C*  MEZZI POSSONO ESSERE MODIFICATE ANCHE SE IL FOGLIO E' CHIUSO
040300960429     C     *INKS         IFEQ      *ON
040400160928     C     v1cSPG        IFEQ      'A'
040500160928     C     v1cSPG        OREQ      'G'
040600980402     C     WNPG          OREQ      2
040700990512     C     FVVDFV        IFLE      §7ADID
040800960429     C                   SETON                                        6290
040900960429     C                   GOTO      FMT01
041000960429     C                   ENDIF
041100960429     C                   ENDIF
041200960429     C                   ENDIF
041300960429     C*
041400940701     C   KE              MOVE      ' '           FVVATB
041500940701     C   KQ              MOVE      '*'           FVVATB
041600940701     C   KS              MOVE      ' '           FVVFCF
041700940701     C                   UPDATE    FNFVV000
041800000713     C     FVVNPG        IFEQ      4
041900000713     C                   EXSR      ANCHE8
042000000713     C                   ENDIF
042100950607     C*
042200910321     C  N01              GOTO      INIZIO
042300910321     C                   GOTO      FINE                                         RICH
042400940701     C                   ENDIF
042500911030     C*
042600911030     C                   GOTO      FMT01
042700911030      *
042800900515      *
042900000000     C     FINE          TAG
043000110103      *
043100020909     c                   Clear                   Trul33Ds
043200020909     c                   Eval      I33Tla = 'C'
043300020909     c                   Movel(p)  Trul33Ds      Kpjbu
043400020909     c                   Call      'TRUL33R'
043500020909     c                   Parm                    Kpjba
043600110103      *
043700020910     c                   Clear                   Fnlr6Ds
043800020910     c                   Eval      D6Tla = 'C'
043900020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
044000020910     c                   Call      'FNLR6R'
044100020910     c                   Parm                    Kpjba
044200900529      *
044300000000     C                   SETON                                        LR
044400000713      *-----------------------------------------------------***********
044500000713      * ALLINEA FOGLIO 8 SE È STATO AGGIORNATO IL FOGLIO 4
044600000713      *-----------------------------------------------------***********
044700000713     C     ANCHE8        BEGSR
044800000713     C                   MOVE      FVVDS         FVVDS2
044900000713     C     KFVV8         CHAIN     FNFVV000                           80
045000000713     C     *IN80         IFEQ      *OFF
045100000713     C                   MOVE      FVVDS2        FVVDS
045200000713     C                   MOVE      8             FVVNPG
045300000713     C                   UPDATE    FNFVV000
045400000713     C                   MOVE      '4'           WNPG
045500000713     C     KFVV          SETLL     FNFVV000
045600000713     C                   MOVE      FVVDS2        FVVDS
045700000713     C                   END
045800000713      *
045900000713     C                   ENDSR
046000900515      *-----------------------------------------------------***********
046100940701      * INIZIALIZZA 1 E 2 VIDEATA                           *  INZ12  *
046200900515      *-----------------------------------------------------***********
046300940701     C     INZ12         BEGSR
046400900515      *
046500910318     C* PULISCO FORMATO1
046600910320     C                   SETOFF                                       995253
046700911030     C                   SETOFF                                       010212
046800180112     C                   SETOFF                                       041115
046900940705     C                   MOVEL     'FNLR01R'     V0CPGM
047000970602     C                   MOVEL     ' '           V0CNPG
047100940701     C                   Z-ADD     0             V0CNFV
047200940701     C                   Z-ADD     0             V0CDIN
047300940701     C                   Z-ADD     0             V0CDFI
047400940701     C                   MOVEL     *BLANKS       V0CTFV
047500910318     C* PULISCO FORMATO2
047600940701     C                   Z-ADD     UDATE8        V1CDFV
047700180130     C                   Z-ADD     UDATE8        savDFV
047800030701     c                   ExSr      Sr_MinMax
047900940701     C                   MOVEL     *BLANKS       V1CDSF
048000940701     C                   MOVEL     *BLANKS       V1CSPN
048100940701     C                   MOVEL     *BLANKS       V1CSPG
048200180112     c                   clear                   v1cinc
048300900515      *
048400900514     C                   ENDSR
048500030701
048600030701      *------------------------------------------------------------------------*
048700030701      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
048800030701      *------------------------------------------------------------------------*
048900030701     c     Sr_MinMax     BegSr
049000110103      *
049100030701     c                   Clear                   Ktfa
049200030701     c                   Z-Add     V0cFgs        Ktfp
049300030701     c     *eur          Move      V1cdfv        wdtaeur
049400030701      * meno 1 gg. lavorativo
049500030701     c                   Do        *Hival
049600030701     c                   Subdur    1:*d          wdtaeur
049700030701     c                   extrct    wdtaeur:*d    wgiorno
049800030701     c                   extrct    wdtaeur:*m    kmes
049900030701     c                   extrct    wdtaeur:*y    kann
050000030701     c     kazcln        Chain     Azcln01l
050100030701     c                   If        Not %Found(azcln01l)
050200030701     c                   Leave
050300030701     c                   EndIf
050400030701     c                   If        POM(wgiorno) = 'F'
050500030701     c                   Iter
050600030701     c                   EndIf
050700030701     c                   Leave
050800030701     c                   EndDo
050900030701     c                   Move      wdtaeur       wdtaiso
051000030701     c                   Move      wdtaiso       wdtameno1
051100110103      *
051200030701      * più 1 gg. lavorativo
051300030701     c     *eur          Move      V1cdfv        wdtaeur
051400030701     c                   Do        *Hival
051500030701     c                   Adddur    1:*d          wdtaeur
051600030701     c                   extrct    wdtaeur:*d    wgiorno
051700030701     c                   extrct    wdtaeur:*m    kmes
051800030701     c                   extrct    wdtaeur:*y    kann
051900030701     c     kazcln        Chain     Azcln01l
052000030701     c                   If        Not %Found(azcln01l)
052100030701     c                   Leave
052200030701     c                   EndIf
052300030701     c                   If        POM(wgiorno) = 'F'
052400030701     c                   Iter
052500030701     c                   EndIf
052600030701     c                   Leave
052700030701     c                   EndDo
052800030701     c                   Move      wdtaeur       wdtaiso
052900030701     c                   Move      wdtaiso       wdtapiu1
053000110103      *
053100030701     c                   EndSr
053200030701
053300910318      *-----------------------------------------------------***********
053400940713      * PRELEVO NUMERATORE FOGLI                            *  CRTNFV *
053500910318      *-----------------------------------------------------***********
053600910318     C     CRTNFV        BEGSR
053700940701     C                   SETOFF                                       90
053800110103      *
053900020909Do  1c                   Do        *Hival
054000020910      * Cerco il numero da staccare (aggiorna e se non trovato in aznum crea record)
054100020909     c                   Clear                   Trul33Ds
054200020909     c                   Eval      I33Cnu = §7nCnu
054300020909     c                   Eval      I33Po1 = V0cfgs
054400020909     c                   Eval      I33Num = 1
054500020909     c                   Movel(p)  Trul33Ds      Kpjbu
054600020909     c                   Call      'TRUL33R'
054700020909     c                   Parm                    Kpjba
054800020909     c                   Movel     Kpjbu         Trul33Ds
054900020909
055000020909S   2c                   Select
055100020909      * Numeratore non trovato
055200020909     c                   When      O33err =  01
055300040227     c                   Eval      V1cmsg = Err(6)
055400040227     c                   Eval      %subst(V1cmsg:31:10) = §7nDe2
055500020909     c                   Eval      *In28 = *On
055600020909     c                   Eval      *In90 = *On
055700020910     c                   Leave
055800020910      * Numeratore allocato
055900020909     c                   When      O33err =  02
056000040227     c                   Eval      V1cmsg = Err(7)
056100040227     c                   Eval      %subst(V1cmsg:67:10) = §7nDe2
056200020909     c                   Eval      *In28 = *On
056300020909     c                   Eval      *In90 = *On
056400020910     c                   Leave
056500110103      *
056600020909     c                   When      O33err <> *zeros
056700040227     c                   Eval      V1cmsg = O33Msg
056800020909     c                   Eval      *In28 = *On
056900020909     c                   Eval      *In90 = *On
057000020910     c                   Leave
057100020909    2c                   EndSl
057200110103      *
057300020917      * Imposto flag di appoggio
057400020917     c                   Eval      Wok = *On
057500110103      *
057600020909      * Controllo che non esista già un foglio con lo stesso numero
057700020910      * Per i fogli di tipo 4 richiamo fnlr6r
057800020919      * lo richiamo passando come tipo foglio 8 così controlla anche l'esistenza
057900020919      * del tipo 8
058000020917If  2c                   If        Wnpg = 4
058100020910     c                   Clear                   Fnlr6ds
058200020919     c                   Eval      D6Npg = 8
058300020910     c                   Eval      D6Fgs = V0cfgs
058400020910     c                   Z-Add     O33Nrf        D6Ndt
058500020910     c                   Movel(p)  Fnlr6Ds       Kpjbu
058600020910     c                   Call      'FNLR6R'
058700020910     c                   Parm                    Kpjba
058800020910     c                   Movel     Kpjbu         Fnlr6Ds
058900020910     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
059000020910     c* come foglio o in una bolla
059100020910If  3c                   If        D6Flg <> 'N'
059200020910     c                   Iter
059300020910    3c                   EndIf
059400020917   x2c                   Else
059500020917      * Per tutti gli altri tipi foglio controllo su fnfvv
059600020910     c                   Z-Add     O33Nrf        V0cnfv
059700020909     c     Kfvv          setll     Fnfvv01l
059800020910If  3c                   If        %Equal(Fnfvv01l)
059900020909     c                   Iter
060000020910    3c                   EndIf
060100020917      * Per i fogli di tipo 3 devo controllare la sotto categoria
060200020917If  3c                   If        Wnpg = 3
060300020917      * se sto aprendo un foglio con sotto categoria 'P' (foglio IMP)
060400020917      * devo controllare che non esiste lo stesso numero x tutti i p.o. del terminal
060500020917If  4c                   If        V1cspg = 'P'
060600020917     c                   Z-Add     O33Nrf        V0cnfv
060700020917Do  5c                   Do        30            xx
060800020917     c                   If        L1(xx) = *Zeros
060900020917     c                   Leave
061000020917     c                   EndIf
061100020917     c                   Z-Add     L1(xx)        Wfgs
061200020917     c     Kfvv1         Setll     Fnfvv01l
061300020917If  6c                   If        %Equal(Fnfvv01l)
061400020917     c                   Eval      Wok = *Off
061500020917     c                   Leave
061600020917    6c                   EndIf
061700020917    5c                   EndDo
061800020917If  5c                   If        Wok = *Off
061900020917     c                   Iter
062000020917    5c                   EndIf
062100020917   x4c                   Else
062200020917      * se sto aprendo un foglio con sotto categoria <> 'P'
062300020917      * devo controllare che non esiste un foglio IMP con lo stesso numero
062400020917      * x tutti i p.o. del terminal
062500020917     c                   Z-Add     O33Nrf        V0cnfv
062600020917Do  5c                   Do        30            xx
062700020917     c                   If        L1(xx) = *Zeros
062800020917     c                   Leave
062900020917     c                   EndIf
063000020917     c                   Z-Add     L1(xx)        Wfgs
063100020917     c     Kfvv1         Setll     Fnfvv01l
063200020917Do  6c                   Do        *Hival
063300020917     c     Kfvv1         Reade     Fnfvv01l
063400020917If  7c                   If        %EoF(Fnfvv01l)
063500020917     c                   Leave
063600020917    7c                   EndIf
063700160928If  7c                   If        v1cSpg = 'P'
063800020917     c                   Eval      Wok = *Off
063900020917     c                   Leave
064000020917    7c                   EndIf
064100020917    6c                   EndDo
064200020917    5c                   EndDo
064300020917If  5c                   If        Wok = *Off
064400020917     c                   Iter
064500020917    5c                   EndIf
064600020917    4c                   EndIf
064700020917    3c                   EndIf
064800020917    2c                   EndIf
064900020909     c                   Z-Add     O33Nrf        V0cnfv
065000020909     c                   Leave
065100020909    1c                   EndDo
065200910318     C*
065300020910     C                   ENDSR
065400900515      *-----------------------------------------------------***********
065500940701      * CONTROLLI FORMATO 1                                 *  CTRD01 *
065600900515      *-----------------------------------------------------***********
065700940701     C     CTRD01        BEGSR
065800900515      *
065900000128     C                   SETOFF                                       9059
066000911003      *
066100911003      * TIPO INVENTARIO
066200911003      *
066300940701     C   02V1CSPG        IFEQ      *BLANKS
066400911004     C                   SETON                                        4890
066500940701     C                   GOTO      ENDCT1
066600911004     C                   ELSE
066700911004     C*
066800160929     c                   eval      Indx=%lookup(v1cspg:tin)
066900160929     c                   if        indx=0 or uin(indx)='N'
067000160929     C                   SETON                                        4890
067100160929     C                   GOTO      ENDCT1
067200160929     C                   ENDif
067300161125     c* solo  abilitati
067400161124     c                   if        not *in09 and uin(indx)='1' and not *in10
067500161017     C                   SETON                                        4890
067600161017     C                   GOTO      ENDCT1
067700161017     C                   ENDif
067800160929     C                   ENDif
067900060522     c*
068000160929     c*Non si può cambiare sottocategoria foglio inventario se ci sono spunt
068100060522     c                   if        *in02  and not *in80 and wspu='S'
068200160929     c                             and v1cspg<>SAVspg
068300060522     C                   SETON                                        6790
068400060522     C                   GOTO      ENDCT1
068500060522     c                   endif
068600941027      *
068700941027      * CONTROLLO DATA FOGLIO
068800941027      *
068900941027     C                   Z-ADD     V1CDFV        G02DAT
069000941027     C                   MOVEL     *BLANK        G02ERR
069100941027     C                   CALL      'XSRDA8'
069200941027     C                   PARM                    WLBDAT
069300941027     C     G02ERR        IFEQ      '1'
069400941027     C                   SETON                                        40  90
069500941027     C                   GOTO      ENDCT1
069600941027     C                   END
069700941027     C*
069800941027     C                   Z-ADD     G02INV        WDFV
069900940706     C*
070000940706     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
070100940706     C                   Z-ADD     G02DAT        V1CDFV
070200180130
070300180130     c* se alla data foglio la filiale diventa terminal, accendo il 15
070400180130     c                   if         not *in15 and v1cdfv<>savdfv and
070500180130     c                              v0cnpg='2' and (*in70 or *in71)
070600180130     c*
070700180130     c                   eval      savdfv=v1cdfv
070800180130     c                   clear                   trul09ds
070900180130     c                   eval      i09mod='TFA'
071000180130     c                   eval      i09tfa=v0cfgs
071100180130     c                   eval      i09dta=wdfv
071200180130     c                   call      'TRUL09R'
071300180130     c                   parm                    trul09ds
071400180130     c
071500180130     c                   if        (o09err=' '  and t09(1)>000)
071600180130     c                   SETON                                        15
071700180130     C                   SETON                                        90
071800180130     C                   GOTO      ENDCT1
071900180130     c                   endif
072000180130     c                   endif
072100180130     c
072200940706     C*
072300940706     C* SE IL FOGLIO HA DELLE SPUNTE E' IMPOSSIBILE MODIFICARE LA DATA
072400940706     C     WSPU          IFEQ      'S'
072500940706     C     FVVDFV        ANDNE     WDFV
072600940706     C                   SETON                                        4990
072700940706     C                   GOTO      ENDCT1
072800940706     C                   ENDIF
072900180112     c* se il foglio ha spunte impossibimle modoficare flag incompatibili
073000180112     c                   if        wspu='S' and *in15 and not *in80
073100180112     c                   if        v1cinc='  ' and savspg='J' or
073200180112     c                             v1cinc='SI' and savspg=' '
073300180112     c                   SETON                                        906828
073400180112     c                   MOVEL     err(9)        v1cmsg
073500180112     C                   GOTO      ENDCT1
073600180112     c                   endif
073700180112     c                   endif
073800180112
073900970602     C* E' IMPOSSIBILE MODIFICARE LA DATA ANCHE SE E' FOGLIO ARRIVI
074000970602     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
074100970602     C     *IN80         IFEQ      *OFF
074200970602     C     WNPG          IFEQ      2
074300970602     C     FVVDFV        ANDNE     WDFV
074400970602     C     KFVV2         SETLL     FNFVA02L                               32
074500970602     C     *IN32         IFEQ      *ON
074600970602     C                   SETON                                        5090
074700970602     C                   GOTO      ENDCT1
074800970602     C                   END
074900970602     C                   END
075000970602     C                   END
075100020422     C* CONTROLLO DATA COMPRESA NEL RANGE non per le distinte fogli 4
075200020422     c                   if        wnpg <> 4
075300030701     c                   If        *Inkf and
075400030701     c                                       (wdfv < wdtameno1 or
075500030701     c                                        wdfv > wdtapiu1)
075600000128     C                   SETON                                        905928
075700030701     C   80              MOVEA     ERR(2)        W78
075800030701     c  n80              Movea     Err(8)        W78
075900030701     c                   Move      '01'          W002A             2
076000000128     C                   MOVEA     W002A         W78(46)
076100000128     C                   MOVEA     W78           V1CMSG
076200940701     C                   GOTO      ENDCT1
076300930225E   1C                   ENDIF
076400020422     c                   else
076500020422      *distinta controlli sui limiti particolari
076600020422     c                   if        *in80
076700020422     c                   if        wdfv < waamg
076800020422     c                   SETON                                        905928
076900020422     c                   MOVEL     err(4)        v1cmsg
077000020422     C                   GOTO      ENDct1
077100020422     c                   endif
077200020422      *controllo range fra le date di oggi e quella immessa
077300020422     c                   eval      dadata = waamg
077400020422     c                   eval      adata  = wdfv
077500020422     c                   CALL      'XSRLAV8'
077600020422     c                   PARM                    Wdata8
077700030917     c                   if        giolav = 0
077800030917     c                   clear                   giorni
077900030917     c                   move      waamg         wdtiso1
078000030917     c                   move      wdfv          wdtiso2
078100030917     c     wdtiso2       subdur    wdtiso1       giorni:*d         7 0
078200030917     c                   if        giorni > 30
078300030917     c                   SETON                                        905928
078400030917     c                   MOVEL     err(5)        v1cmsg
078500030917     C                   GOTO      ENDct1
078600030917     c                   endif
078700030917     c                   endif
078800020422     c                   if        giolav > §dgddc
078900020422     c                   SETON                                        905928
079000020422     c                   MOVEL     err(5)        v1cmsg
079100020422     C                   GOTO      ENDct1
079200020422     c                   end
079300020422     c                   end
079400110103      *
079500020422     c                   end
079600910320      *
079700940701     C*
079800940701     C* RICHIESTO ANNULLAMENTO FOGLIO:
079900940701     C*
080000950919     C* SE ESISTONO SPUNTE IMPOSSIBILE ANNULLARE FOGLIO: SEMPRE!!
080100940701     C   KQWSPU          IFEQ      'S'
080200940701     C                   SETON                                        4190
080300940701     C                   GOTO      ENDCT1
080400950919     C                   ENDIF
080500940701     C*
080600940701     C* SE DISTINTA NON POSSO ANNULLARE SE PRESENTI BOLLE
080700941027     C   KQWNPG          IFEQ      4
080800020910     c                   Z-add     V0cfgs        Wifp
080900940701     C                   Z-ADD     V0CNFV        VIDNDC            7 0
081000020910     c     Karb78        Setll     Fnarb78l                               32
081100010314     C   32              SETON                                        5590
081200940701     C   32              GOTO      ENDCT1
081300010314     C* NON POSSO ANNULLARE ANCHE SE PRESENTI O.R.M.
081400010314     C     WORM          IFEQ      'S'
081500010314     C                   SETON                                        6590
081600010314     C                   GOTO      ENDCT1
081700010314     C                   ENDIF
081800940701     C                   END
081900980112     C* E' IMPOSSIBILE ANNULLARE SE E' FOGLIO ARRIVI
082000980112     C* AL QUALE CI SONO GIA' DEI FOGLI VIAGGIO PARTENZA ABBINATI
082100980112     C   KQWNPG          IFEQ      2
082200980112     C     KFVV2         SETLL     FNFVA02L                               32
082300980112     C     *IN32         IFEQ      *ON
082400980112     C                   SETON                                        6490
082500980112     C                   GOTO      ENDCT1
082600980112     C                   END
082700980112     C                   END
082800900515      *
082900940701     C     ENDCT1        ENDSR
083000900515      *-----------------------------------------------------***********
083100900515      * REGISTRAZIONE RECORD                                *  REGIS  *
083200900515      *-----------------------------------------------------***********
083300900515     C     REGIS         BEGSR
083400900515      *
083500910318     C                   MOVEL     *BLANK        FVVATB
083600940701     C                   Z-ADD     V0CNFV        FVVNFV
083700940701     C                   Z-ADD     WDFV          FVVDFV
083800160928     c                   clear                   I
083900160928     C* PER CATEGORIA 3 VERIFICO IL TIPO INVENTARIO SE È DA SCRIVERE COSÌ COME È
084000160928     c*  oppure devo impostare il campo FPP
084100160928    1c                   if        *in02
084200160928     c                   eval      i=%lookup(v1cspg:tin)
084300160928    1c                   endif
084400920806     C*
084500160928    1C     V1CDSF        IFEQ      *BLANKS
084600940701     C                   MOVEL     *BLANKS       FVVDSF
084700160928
084800950314     C* DESCRIZIONE INVENTARIO: SE NON C'E' PRENDO DA TIPO INVENTARIO
084900160928    2C                   if        *in02 and i>0
085000160928     C                   MOVEL     DbIN(I)       FVVDSF
085100160928     c
085200160928   x2C                   ELSE
085300950320     C* DESCRIZIONE FOGLIO: SE NON C'E' PRENDO DA CATEGORIA FOGLIO
085400950320     C                   MOVEL     V1DNPG        FVVDSF
085500160928    2C                   endif
085600160928     c
085700160928   x1C                   ELSE
085800940701     C                   MOVEL     V1CDSF        FVVDSF
085900160928    1C                   endif
086000911129     C*
086100050512     c   80              clear                   dfvvspn
086200050512     C                   MOVEL     V1CSPN        §FVVSPN
086300050512     c                   movel     dfvvspn       fvvspn
086400160928     C                   IF        NOT *IN02
086500160928     C                   CLEAR                   FVVSPG
086600160928     C                   ELSE
086700160928     C                   MOVEL     V1CSPG        FVVSPG
086800160929     c                   clear                   fvvfpp
086900161006     c***                if        I>0 and
087000161006     c***                          rif_in(I)<>*blanks
087100161006     c***                movel     rif_in(i)     fvvspg
087200161006     c***                move      v1cspg        fvvfpp
087300161006     C***                ENDIF
087400160928     C                   ENDIF
087500160928     C
087600180112     c* Se incompatibili, memorizzo
087700180112     c                   if        v1cinc='SI' and v0cnpg='2'
087800180112     c                   eval      fvvspg='J'
087900180112     C                   IF        V1DNPG   =    FVVDSF
088000180112     c                   eval      fvvdsf='IMCOMPATIBILI'
088100180112     c                   endif
088200180112     c                   endif
088300180112
088400911016     C                   MOVEL     *BLANKS       FVVFCF
088500911112     C   KA              MOVE      'S'           FVVFCF
088600940701     C   80              MOVEL     V0CNPG        FVVNPG
088700911016     C                   MOVEL     'S'           FVVFSC
088800950123     C                   Z-ADD     SIMFEL        FVVFLE
088900900515      *
089000020918      * In immissione foglio mi reimposto il p.o. in gestione xchè al momento dei controlli
089100020918      * sul numero staccato fvvfgs non è più il p.o. gestione del video impostato nella
089200020918      * routine riempi
089300020918     c                   Movel     V0cFgs        FvvFgs
089400900515      *
089500950320     C   80              WRITE     FNFVV000                                     IMMISSIONE
089600160928
089700000512      *SCRITTURA FOGLIO DI TIPO 8 UGUALE A 4 PER SPUNTE RIENTRO
089800000512     C     *IN80         IFEQ      *ON
089900000512     C     FVVNPG        ANDEQ     4
090000000512     C                   MOVE      8             FVVNPG
090100000512     C                   WRITE     FNFVV000
090200000512     C                   END
090300911004      *
090400940701     C  N80              UPDATE    FNFVV000                                     VARIAZIONE
090500000512      *COPIA MODIFICHE DA FOGLIO 4 A FOGLIO 8
090600000512     C     *IN80         IFEQ      *OFF
090700000512     C     FVVNPG        ANDEQ     4
090800000512     C                   MOVE      FVVDS         FVVDS2
090900000512     C                   MOVE      '8'           WNPG
091000000512     C     KFVV          CHAIN     FNFVV000                           80
091100000512     C     *IN80         IFEQ      *OFF
091200000512     C                   MOVE      FVVDS2        FVVDS
091300000512     C                   MOVE      8             FVVNPG
091400000512     C                   UPDATE    FNFVV000
091500000512     C                   MOVE      '4'           WNPG
091600000512     C     KFVV          SETLL     FNFVV000
091700000515     C                   MOVE      FVVDS2        FVVDS
091800000512     C                   END
091900000512     C                   END
092000000512     C*
092100900515     C                   ENDSR
092200900515      *-----------------------------------------------------***********
092300900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
092400900515      *-----------------------------------------------------***********
092500900515     C     RIEMPI        BEGSR
092600180112     c                   setoff                                       0971
092700161124     c
092800170124     c* Se sono in prova per IPG ricarico i tipi foglio in base alla filiale utente
092900170124     c                   if        not *in10
093000180112     c**                 if        %lookup(%editc(v0cfgs:'X'):skFilOk)>0
093100180112     c**                 seton                                        09
093200180112     c**                 endif
093300161124     c                   exsr      car_inv
093400161124     c                   endif
093500180112     c
093600180112     c* Abilitazione agli incompatibili
093700180112     c                   if        not *in70
093800180112     c                   if        %lookup(%editc(v0cfgs:'X'):skFilOk)>0
093900180112     c                   seton                                        71
094000180112     c                   endif
094100180112     c                   endif
094200161124     c
094300940712     C* EMETTO LA DESCRIZIONE DELLO STATO FOGLIO-INS/VARIAZ/ANNULL/BLOC
094400940712     C                   SETON                                        06
094500941027     C                   SETOFF                                       07
094600940712     C*
094700940701     C                   CLEAR                   WSPU
094800010314     C                   CLEAR                   WORM
094900160929     C                   CLEAR                   SAVSPG
095000960429     C* CHAIN SULLA TABELLA 7A PER PRENDERE LA DATA ULTIMA ELABORAZIONE
095100960429     C* IDD
095200960429     C                   MOVEL     '7A'          COD
095300960429     C                   MOVEL(P)  V0CFGS        KEY
095400960429     C     KTAB2         CHAIN     TABEL                              31
095500960429     C  N31              MOVEL     TBLUNI        DS7AF
095600180112     c
095700180112     c* se FOGLIO ARRIVI EMETTO RICHIESTA DEGLI INCOMPASTIBILI
095800180112     C                   IF        V0cNPG='2' and (*in70 or *in71)
095900180130     c* lo faccio solo se la filiale è un terminal di arrivo
096000180130     c                   clear                   trul09ds
096100180130     c                   eval      i09mod='TFA'
096200180130     c                   eval      i09tfa=v0cfgs
096300180130     c  n80              eval      i09dta=fvvdfv
096400180130     c                   call      'TRUL09R'
096500180130     c                   parm                    trul09ds
096600180130     c
096700180130     c                   if        (o09err=' '  and t09(1)>000)
096800180130     c                             or (not *in80 and fvvspg='J')
096900180130     c                   SETON                                        15
097000180130     c                   endif
097100180130     c
097200180126     c  n80              eval      savspg=fvvspg
097300180130     c
097400180112     C                   ENDIF
097500900515      *
097600911016     C* SE INVENTARIO EMETTO CAMPO DELLA SOTTOSPECE
097700160928    1C     V0CNPG        IFEQ      '3'
097800911003     C                   SETON                                        02
097900160928     c* i comandi di riapertra e chiusura sono gestiti da tabella
098000160928    2c                   if        not *in80
098100160928     c* se il campo FPP è pieno devo cercare la corretta tipologia
098200161006     c***                if        fvvfpp<>*blanks
098300161006     c***                eval      I=%lookup(fvvfpp:tin)
098400161006     C***                MOVEL     FVVfpp        V1CSPG                          sottotipo
098500161006     C***                MOVEL     FVVfpp        SAVSPG                          sottotipo
098600161006     c***                else
098700160929     c                   eval      I=%lookup(fvvspg:tin)
098800160928     C                   MOVEL     FVVSPG        V1CSPG                          sottotipo
098900160929     C                   MOVEL     FVVspg        SAVSPG                          sottotipo
099000161006     c***                endif
099100160928     c
099200160928    3c                   if        apin(i)='S'
099300160928     c                   seton                                        11
099400160928    3c                   endif
099500160928    3c                   if        chin(i)='S'
099600160928     c                   seton                                        12
099700160928    3c                   endif
099800160928    2c                   endif
099900160928     c
100000960429     C* FOGLIO IMP
100100960429     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
100200160928     C**N80FVVSPG        IFEQ      'P'
100300160928     C**                 SETON                                        1211
100400160928     C**                 END
100500001115     C* FOGLIO IMI
100600001115     C* EMETTO CMD DI CHIUSURA O RIAPERTURA
100700160928     C**N80FVVSPG        IFEQ      'I'
100800160928     C**                 SETON                                        1211
100900160928     C**                 END
101000960429     C* FOGLIO IMA
101100960429     C* EMETTO CMD DI RIAPERTURA
101200961113     C* EMETTO ANCHE COMANDO DI CHIUSURA MA SOLO SE TUTTE LE SPUNTE
101300961113     C*  CONTENUTE NELL'IMA HANNO DATA ARRIVO > 300
101400160928     C**N80FVVSPG        IFEQ      'A'
101500160928     C**                 SETON                                        1112
101600160928     C**                 ENDIF
101700960626     C* FOGLIO IMG
101800960626     C* EMETTO CMD DI RIAPERTURA
101900160928     C**N80FVVSPG        IFEQ      'G'
102000160928     C**                 SETON                                        11
102100160928     C**                 ENDIF
102200160928    1C                   ENDIF
102300940701     C*
102400940712      * S O L O   I N   V A R I A Z I O N E
102500940712     C     *IN80         IFEQ      *OFF
102600940712     C* DESCRIZIONE A VIDEO
102700940712     C                   MOVEL     CVARIA        V1DTFV
102800940712     C*
102900940712     C* SE ANNULLATO PROTEGGO CAMPI
103000940712     C     FVVATB        IFEQ      '*'
103100940712     C                   SETON                                        52  99
103200940712     C                   MOVEL     CANNUL        V1DTFV
103300940712     C                   END
103400940712     C*
103500940712     C* FOGLIO CHIUSO
103600940712     C     FVVFCF        IFEQ      'S'
103700940712     C                   SETON                                        53  99
103800940712     C                   END
103900940712     C* FOGLIO BLOCCATO
104000940712     C     FVVFCF        IFNE      'S'
104100940712     C     FVVFCF        ANDNE     ' '
104200940712     C                   MOVEL     CBLOCC        V1DTFV
104300940712     C                   ENDIF
104400940712     C*
104500960429     C* 12 E 11 ON - EMETTO F1 O F18 ANCHE PER FOGLIO ENTRATA
104600940712     C     V0CNPG        IFEQ      '5'
104700960429     C                   SETON                                        1211
104800960429     C                   ENDIF
104900960429     C*
105000960429     C* 11 ON - FACCIO RIAPRIRE IL FOGLIO IDA
105100960429     C*         SOLO SE NON HO GIA' FATTO L'IDD
105200960429     C     V0CNPG        IFEQ      '2'
105300960429     C                   SETON                                        11
105400960429     C                   ENDIF
105500180112     c* Per foglio arrivi vedo se si tratta di incompatibili
105600180112     c* emetto comunque la richeista se trovo la "J" : potrei aver abilitato e disabilitato
105700180112     c                   if        fvvspg='J' and fvvnpg=2
105800180112     c                   seton                                        15
105900180112     c                   eval      v1cinc='SI'
106000180112     c                   endif
106100180112     c
106200941027     C*
106300940712     C                   MOVE      FVVDSF        V1CDSF                          DESCRIZIONE
106400050512     c                   clear                   dfvvspn
106500050512     C                   MOVEL     FVVSPN        dfvvspn                         SPUNTATORE
106600050512     C                   MOVEL     §fvvspn       V1CSPN                          SPUNTATORE
106700911004     C*
106800940712     C                   MOVE      FVVDFV        G02INV                          DATA
106900911004     C                   MOVE      *ZEROS        G02DAT
107000911004     C                   MOVEL     3             G02ERR
107100940706     C                   CALL      'XSRDA8'
107200911004     C                   PARM                    WLBDAT
107300940701     C                   Z-ADD     G02DAT        V1CDFV
107400030701     c                   ExSr      Sr_MinMax
107500940701     C*
107600940701     C* SEGNALO SE SUL FOGLIO ESISTONO SPUNTE
107700000128     C                   Z-ADD     V0CNFV        WNFV
107800070112     C     KBRV          SETLL     FNBRV000                               07
107900911004      *
108000940701     C* WSPU = "S"  --> ESISTONO SPUNTE
108100941027     C     *IN07         IFEQ      *ON
108200940701     C                   MOVEL     'S'           WSPU              1
108300180130     c* solo se ci sono spunte abilito alla apertura/chiusura
108400180130     c                   if        v1cinc='SI'
108500180130     C                   SETON                                        1211
108600180130     C                   endif
108700180130     C                   END
108800010314     C* CONTROLLO SE ESISTONO O.R.M. IN FASE < 900
108900010314     C     WSPU          IFEQ      *BLANKS
109000010314     C     KORM          SETLL     FNORM06L
109100010314     C     KORM          READE     FNORM06L                               31
109200010314    3C     *IN31         DOWEQ     *OFF
109300010314     C                   CLEAR                   TIBS02
109400010314     C                   CLEAR                   DFAR
109500010314     C                   MOVEL     'C'           T02MOD
109600010314     C                   MOVEL     KNSIF         T02SIF
109700010314     C                   MOVEL     'FAR'         T02COD
109800010314     C                   MOVEL     ORMFAO        T02KE1
109900010314     C                   CALL      'TIBS02R'
110000010314     C                   PARM                    KPJBA
110100010314     C                   PARM                    TIBS02
110200010314     C     T02ERR        IFEQ      *BLANKS
110300010314     C                   MOVEL     T02UNI        DFAR
110400010314     C                   ENDIF
110500010314     C* SE E' ORM IN FASE < 900 ESCO DAL CICLO E MEMORIZZO CHE SULLA
110600010314     C* DISTINTA E' PRESENTE UN O.R.M.
110700010314    4C     §FARAF        IFNE      'S'
110800010314     C                   SETON                                        31
110900010314     C                   MOVEL     'S'           WORM              1
111000010314   X4C                   ELSE
111100010314     C     KORM          READE     FNORM06L                               31
111200010314    4C                   ENDIF
111300010314    3C                   ENDDO
111400010314    2C                   ENDIF
111500940712     C*
111600940712     C                   ELSE
111700940712     C*
111800940712     C* I M M I S S I O N E
111900940712     C*
112000940712     C* DESCRIZIONE A VIDEO
112100940712     C                   MOVEL     CINSER        V1DTFV
112200940712     C* FILIALE GESTIONE
112300940712     C                   MOVEL     V0CFGS        FVVFGS
112400940712     C                   ENDIF
112500910319     C*
112600910319     C                   ENDSR
112700911004      *-----------------------------------------------------***********
112800940701      * CARICO TABELLE                                      *  CARTAB *
112900911004      *-----------------------------------------------------***********
113000940701     C     CARTAB        BEGSR
113100161125     c                   setoff                                       0910
113200180112     c                   setoff                                       7071
113300161124     c
113400180112     c* carico le filiali abilitate a incompatibili
113500180112     c                   clear                   trulvpods
113600180112     c                   eval      ivpoke1 = 'DECOFIINC'
113700180112     c                   call      'TRULVPOR'
113800180112     c                   parm                    trulvpods
113900161124     c
114000180112     c                   if        %lookup('999':skFilOk)>0
114100180112     c                   seton                                        70
114200180112     c                   endif
114300161124     C*
114400161124     C                   MOVEL     '7B'          COD
114500161124     C                   Z-ADD     1             I                 3 0
114600161124     C     KTAB          SETLL     TABEL
114700161124     C     KTAB          READE     TABEL                                  30
114800161124     C*
114900161124     C     *IN30         DOWEQ     '0'
115000161124     C                   MOVEL     TBLKEY        TIN(I)
115100161124     C                   MOVEL     TBLUNI        DS7B
115200161124     C                   MOVEL     §7BDES        DIN(I)
115300161124     C*******            MOVEL     §7Brifer      rif_IN(I)
115400161124     C                   MOVE      §7Buti        uIN(I)
115500161124     C                   MOVE      §7Bdesb       dbIN(I)
115600161124     C                   MOVE      §7Briap       apIN(I)
115700161124     C                   MOVE      §7Bchius      chIN(I)
115800161124     C*
115900161124     C                   ADD       1             I
116000161124     C     KTAB          READE     TABEL                                  30
116100161124     C                   END
116200161124
116300161017     c
116400940701     C***
116500940701     C* CARICO I TIPI INVENTARIO
116600940701     C***
116700161124     c                   exsr      car_inv
116800940701     C***
116900160929     C* VEDO SE SONO SIMFEL O 2 livello
117000940701     C***
117100110103     c     dutLPO        ifeq      '2'
117200110103     c     dutLPO        oreq      *blanks
117300110103     c                   movel     dutPOU        v0cfgs
117400920806     C                   SETON                                        05
117500930209     C                   ELSE
117600940701     C                   MOVEL     SIMFEL        V0CFGS
117700920806     C                   END
117800941027     C*
117900940701     C     V0CFGS        CHAIN     AZORG                              31
118000940713     C  N31              MOVEL     ORGDES        V1DFGS
118100940701     C***
118200920806     C* CARICO CATEGORIE FOGLI
118300940701     C***
118400920806     C                   MOVEL     '7N'          COD
118500920806     C                   Z-ADD     0             I
118600920806     C     KTAB          SETLL     TABEL
118700920806     C     KTAB          READE     TABEL                                  31
118800940701     C*
118900920806     C     *IN31         DOWEQ     '0'
119000920806     C                   MOVEL     TBLUNI        DS7N
119100920806     C* SOLO SE ABILITATA PER FOGLI VARI
119200920806     C     §7NFFV        IFEQ      'S'
119300921228     C     §7NUTI        ANDNE     '9'
119400920806     C                   ADD       1             I
119500920806     C                   MOVEL     ' ='          DEC(I)
119600940712     C                   MOVEL     W001A         DEC(I)
119700920806     C                   MOVE      §7NDE2        DEC(I)
119800920806     C     I             IFEQ      6
119900920806     C                   SETON                                        31
120000920806     C                   END
120100920806     C                   END
120200920806     C*
120300920806     C  N31KTAB          READE     TABEL                                  31
120400920806     C                   END
120500920806     C*
120600940701     C* IMPOSTO LE DESCRIZIONI A VIDEO
120700920806     C     DEC(1)        IFNE      *BLANKS
120800920806     C                   MOVEL     DEC(1)        DESCA1
120900920806     C     DEC(2)        IFNE      *BLANKS
121000920806     C                   MOVEL     DEC(2)        DESCA2
121100920806     C     DEC(3)        IFNE      *BLANKS
121200920806     C                   MOVEL     DEC(3)        DESCA3
121300920806     C     DEC(4)        IFNE      *BLANKS
121400920806     C                   MOVEL     DEC(4)        DESCA4
121500920806     C     DEC(5)        IFNE      *BLANKS
121600920806     C                   MOVEL     DEC(5)        DESCA5
121700920806     C     DEC(6)        IFNE      *BLANKS
121800920806     C                   MOVEL     DEC(6)        DESCA6
121900920806     C                   END
122000920806     C                   END
122100920806     C                   END
122200920806     C                   END
122300920806     C                   END
122400920806     C                   END
122500940701     C***
122600940711     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
122700940701     C***
122800940711     C                   TIME                    W0120            12 0
122900940711     C                   MOVE      W0120         WDAT              6 0
123000940711     C*
123100940711     C                   Z-ADD     WDAT          G02DAT
123200940701     C                   MOVEL     *BLANK        G02ERR
123300940701     C                   CALL      'XSRDA8'
123400940701     C                   PARM                    WLBDAT
123500940711     C* DATA DEL GIORNO IN AAAAMMGG
123600940711     C                   Z-ADD     G02INV        WAAMG
123700940701     C*
123800940711     C* DATA DEL GIORNO IN GGMMAAAA
123900940701     C                   Z-ADD     G02DAT        UDATE8
124000920806     C*
124100940705     C                   MOVE      TES(1)        V0CTES
124200940701     C*
124300911004     C                   ENDSR
124400161124      *-----------------------------------------------------***********
124500161124      *  Carico i tipi inventario
124600161124      *-----------------------------------------------------***********
124700161124     c     Car_inv       BEGSR
124800161124     C                   MOVEL     *BLANKS       VIDSP1
124900161124     C                   MOVEL     *BLANKS       DESSP1
125000161124     C                   MOVEL     *BLANKS       VIDSP2
125100161124     C                   MOVEL     *BLANKS       DESSP2
125200161124     C                   MOVEL     *BLANKS       VIDSP3
125300161124     C                   MOVEL     *BLANKS       DESSP3
125400161124     C                   MOVEL     *BLANKS       VIDSP4
125500161124     C                   MOVEL     *BLANKS       DESSP4
125600161124     C                   MOVEL     *BLANKS       VIDSP5
125700161124     C                   MOVEL     *BLANKS       DESSP5
125800161124     C                   MOVEL     *BLANKS       VIDSP6
125900161124     C                   MOVEL     *BLANKS       DESSP6
126000161124
126100161124     C* EMETTO A VIDEO I PRIMI 6
126200161124     c                   z-add     1             I
126300161124     c                   dow       tin(i)<>*blanks
126400161124     C     TIN(I)        IFNE      *BLANKS
126500161124     c     uin(i)        andne     'N'
126600161124     c*
126700161124     c                   if        *in10 or
126800161124     c                             *in09 or (not *in09 and uin(I)<>'1' )
126900161124     c                   select
127000161124     c                   when      vidsp1=*blanks
127100161124     C                   MOVEL     TIN(I)        VIDSP1
127200161124     C                   MOVE      ' ='          VIDSP1
127300161124     C                   MOVEL     DIN(I)        DESSP1
127400161124     c                   when      vidsp2=*blanks
127500161124     C                   MOVEL     TIN(I)        VIDSP2
127600161124     C                   MOVE      ' ='          VIDSP2
127700161124     C                   MOVEL     DIN(I)        DESSP2
127800161124     c                   when      vidsp3=*blanks
127900161124     C                   MOVEL     TIN(I)        VIDSP3
128000161124     C                   MOVE      ' ='          VIDSP3
128100161124     C                   MOVEL     DIN(I)        DESSP3
128200161124     c                   when      vidsp4=*blanks
128300161124     C                   MOVEL     TIN(I)        VIDSP4
128400161124     C                   MOVE      ' ='          VIDSP4
128500161124     C                   MOVEL     DIN(I)        DESSP4
128600161124     c                   when      vidsp5=*blanks
128700161124     C                   MOVEL     TIN(I)        VIDSP5
128800161124     C                   MOVE      ' ='          VIDSP5
128900161124     C                   MOVEL     DIN(I)        DESSP5
129000161124     c                   when      vidsp6=*blanks
129100161124     C                   MOVEL     TIN(I)        VIDSP6
129200161124     C                   MOVE      ' ='          VIDSP6
129300161124     C                   MOVEL     DIN(I)        DESSP6
129400161124     c                   leave
129500161124     c                   endsl
129600161124     c                   endif
129700161124     c                   endif
129800161124     c
129900161124     c                   add       1             I
130000161124
130100161124     C                   ENDdo
130200161124     c                   ENDSR
130300940701      *-----------------------------------------------------***********
130400940701      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
130500940701      *-----------------------------------------------------***********
130600940701     C     CTRFGS        BEGSR
130700940708     C                   SETOFF                                       90
130800950206     C     V0CFGS        LOOKUP    L1                                     30
130900950206     C     *IN30         IFEQ      *OFF
131000950206     C     V0CFGS        OREQ      0
131100950206     C                   MOVEL     ERR(1)        V0CMSG
131200941027     C                   SETON                                        439028
131300941027     C                   GOTO      ENDCTF
131400941027     C                   ENDIF
131500941027     C* DECODIFICA
131600941027     C     V0CFGS        CHAIN     AZORG                              31
131700941027     C  N31              MOVEL     ORGDES        V1DFGS
131800940701     C*
131900940701     C     ENDCTF        ENDSR
132000940701      *-----------------------------------------------------***********
132100940712      * CONTROLLO FORMATO 0                                    CTRD00
132200940701      *-----------------------------------------------------***********
132300940701     C     CTRD00        BEGSR
132400940701     C                   SETOFF                                       90
132500940701     C*
132600940701     C* CATEGORIA OBBLIGATORIA
132700940701     C*
132800940701     C     V0CNPG        IFEQ      ' '
132900940701     C                   SETON                                        4290
133000940701     C                   GOTO      ENDCT0
133100940701     C                   END
133200940701     C*
133300940701     C* RICERCA
133400940701     C     V0CNPG        IFEQ      '?'
133500940701     C                   MOVEL     CODUT         §KUT
133600940701     C                   MOVEL     '7N'          §COD
133700940701     C                   MOVE      ' '           V0CNPG
133800940701     C                   MOVEL     *BLANKS       §KEY
133900940701     C                   CALL      'X§TABER'
134000940701     C                   PARM                    §KUT              1 0
134100940701     C                   PARM                    §COD              2
134200940701     C                   PARM                    §KEY              8
134300940701     C                   PARM                    §DES             30
134400940701     C                   MOVEL     §KEY          V0CNPG
134500940701     C                   SETON                                        90
134600940701     C                   GOTO      ENDCT0
134700940701     C                   END
134800940701     C* CONTROLLO
134900940701     C                   MOVEL     '7N'          COD
135000940701     C                   MOVEL     *BLANKS       KEY
135100940701     C                   MOVEL     V0CNPG        KEY
135200940701     C* INESISTENTE
135300940701     C     KTAB2         CHAIN     TABEL                              42
135400940701     C* ANNULLATA
135500940701     C  N42TBLFLG        IFNE      ' '
135600940701     C                   SETON                                        42
135700940701     C                   ELSE
135800940701     C                   MOVEL     TBLUNI        DS7N
135900940701     C* NON E' CATEGORIA DI FOGLI VARI
136000940701     C     §7NFFV        IFNE      'S'
136100940701     C                   SETON                                        42
136200940701     C                   END
136300940701     C                   END
136400940701     C* 42 ON - ERRORE
136500940701     C   42              SETON                                        90
136600940701     C   42              GOTO      ENDCT0
136700940701     C*
136800940701     C* NON TUTTE LE CATEGORIE SONO UTILIZZABILI DAI 2 LIVELLI
136900940701     C     §7NUTI        IFEQ      '9'
137000940701     C                   SETON                                        5690
137100940701     C                   GOTO      ENDCT0
137200940701     C                   END
137300041026     c* categoria  4 non più utilizzabile da questo pgm
137400041026     c     v0cnpg        ifeq      '4'
137500041026     c                   seton                                        6390
137600041026     c                   goto      endct0
137700041026     c                   endif
137800041026     c
137900940701     C*
138000940701     C* FILIALE IN GESTIONE E' SIMFEL
138100940701     C     SIMFEL        IFEQ      V0CFGS
138200940701     C     §7NUTI        IFEQ      '2'
138300940701     C                   SETON                                        5790
138400940701     C                   GOTO      ENDCT0
138500940701     C                   END
138600940701     C*
138700940701     C                   ELSE
138800940701     C*
138900940701     C* FILIALE IN GESTIONE NON E' SIMFEL
139000940701     C     §7NUTI        IFEQ      '1'
139100940713     C                   SETON                                        5890
139200940713     C                   GOTO      ENDCT0
139300940701     C                   END
139400940701     C                   END
139500040227     c* Se §7NUITI='G' categoria solo in  gestione ma non in immissione
139600040227     c                   if        *inkj and §7nuti='G'
139700040227     C                   SETON                                        6690
139800040227     C                   GOTO      ENDCT0
139900040227     c                   endif
140000940701     C*
140100940701     C                   MOVEL     §7NDES        V1DNPG                         DESCRIZIONE
140200940701     C                   MOVEL     V0CNPG        WNPG                           IN CAMPO NUM
140300940701     C*
140400940713     C* FOGLIO
140500950320     C                   SETON                                        80
140600950320     C                   SETOFF                                       60
140700950320     C  NKJKFVV          CHAIN     FNFVV000                           8060
140800940701     C*
140900940701     C* RECORD GIA' VINCOLATO
141000940712     C   60              SETON                                        90
141100940712     C   60              GOTO      ENDCT0
141200950628     C* SE FOGLIO BLOCCATO --> NON POSSO MANUTENZIONARLO
141300950628     C     *IN80         IFEQ      *OFF
141400950628     C     FVVFCF        ANDNE     'S'
141500950628     C     FVVFCF        ANDNE     ' '
141600950628     C                   SETON                                        9061
141700950628     C                   UNLOCK    FNFVV01L
141800950628     C                   GOTO      ENDCT0
141900950628     C                   ENDIF
142000940701     C*
142100940701      *
142200940713      * NUMERO FOGLIO INESISTENTE IN VARIAZIONE
142300950320     C  NKJ*IN80         IFEQ      *ON
142400950320     C                   SETON                                        3890
142500950320     C                   GOTO      ENDCT0
142600940701     C                   ENDIF
142700940701     C*
142800940701     C     ENDCT0        ENDSR
142900940701      *-----------------------------------------------------***********
143000940701      * INZSR INIZIALE                                         INZSR
143100940701      *-----------------------------------------------------***********
143200940701     C     *INZSR        BEGSR
143300940701      ***
143400940701      * CHIAVI
143500940701      ***
143600940701     C     KFVV          KLIST
143700940701     C                   KFLD                    WNPG
143800940701     C                   KFLD                    V0CNFV
143900941027     C                   KFLD                    V0CFGS
144000020917     c     Kfvv1         Klist
144100020917     c                   Kfld                    Wnpg
144200020917     c                   Kfld                    V0cnfv
144300020917     c                   Kfld                    Wfgs
144400000713     C     KFVV8         KLIST
144500000713     C                   KFLD                    WNPG8
144600000713     C                   KFLD                    FVVNFV
144700000713     C                   KFLD                    FVVFGS
144800000713     C                   Z-ADD     8             WNPG8
144900000128     C* N.B. QUANDO IL NUMERO FOGLIO DIVENTERA' LUNGO 6 ANCHE PER I
145000000128     C*      FOGLI (FNFVV FNFVA) LA KLIST KBRV SI POTRÀ CANCELLARE
145100000128     C*      PERCHE' KFVV SI POTRA' UTILIZZARE AL SUO POSTO KFVV
145200000128     C     KBRV          KLIST
145300000128     C                   KFLD                    WNPG
145400000128     C                   KFLD                    WNFV
145500000128     C                   KFLD                    V0CFGS
145600950104     C     KFVV2         KLIST
145700950104     C                   KFLD                    WNPG
145800950104     C                   KFLD                    V0CNFV
145900020917     c                   Kfld                    V0cFgs
146000950607     C     KDST          KLIST
146100950607     C                   KFLD                    V0CNFV
146200950607     C                   KFLD                    V0CFGS
146300940701     C     KTAB          KLIST
146400940701     C                   KFLD                    CODUT
146500940701     C                   KFLD                    COD               2
146600940701     C     KTAB2         KLIST
146700940701     C                   KFLD                    CODUT
146800940701     C                   KFLD                    COD               2
146900940701     C                   KFLD                    KEY               8
147000010314     C* ACCESSO FNORM06L
147100010314     C     KORM          KLIST
147200010314     C                   KFLD                    V0CFGS
147300010314     C                   KFLD                    WNFV
147400020910      * Fnarb
147500020910     c     Karb78        Klist
147600020910     c                   Kfld                    Wifp
147700020910     c                   Kfld                    Vidndc
147800030701      * Azcln01l
147900030701     c     kazcln        Klist
148000030701     c                   Kfld                    ktfp
148100030701     c                   Kfld                    ktfa
148200030701     c                   Kfld                    kann
148300030701     c                   Kfld                    kmes
148400110103      *
148500940701     C* LIKE DEFN
148600940711     C* DATA DEL GIORNO A 8 AAAAMMGG
148700940711     C     *LIKE         DEFINE    FVVDFV        WAAMG
148800940711     C* DATA DEL GIORNO A 8 GGMMAAAA
148900000718     C     *LIKE         DEFINE    BRVLNA        SAVLNA
149000000718     C     *LIKE         DEFINE    BRVLNP        SAVLNP
149100000718     C     *LIKE         DEFINE    FVVDS         FVVDS2
149200940705     C     *LIKE         DEFINE    FVVDFV        UDATE8
149300940701     C     *LIKE         DEFINE    FVVDFV        WDFV
149400940701     C     *LIKE         DEFINE    FVVNPG        WNPG
149500000713     C     *LIKE         DEFINE    FVVNPG        WNPG8
149600000128     C     *LIKE         DEFINE    BRVNFV        WNFV
149700020422     C***
149800020422     C     KTBE          KLIST
149900020422     C                   KFLD                    KCOD1
150000020422     C                   KFLD                    KKE1
150100020422     C     *LIKE         DEFINE    TBEKE1        KKE1
150200020422     C     *LIKE         DEFINE    TBECOD        KCOD1
150300020422     C*  tabelle dgd x reperimento limiti chiusura spedizioni
150400020422     C                   MOVEL     'DGD'         KCOD1
150500020422     C                   MOVEL     *BLANKS       KKE1
150600020422     C                   MOVEL     '1'           KKE1
150700020422     C     KTBE          CHAIN     TNTBE01L                           31
150800020422     C     *IN31         IFEQ      '0'
150900020422     C                   MOVEL     TBEUNI        DDGD
151000020422     C                   END
151100940701     C                   ENDSR
151200110103
151300110103      *---------------------------------------------------------------*
151400110103      *?Reperimento dati del job (Utente/Operativi)                  ?*
151500110103      *---------------------------------------------------------------*
151600110103     c     sr_DatiJob    BEGSR
151700110103      *
151800110103     c     *dtaara       define    §azute        AZUTEds
151900110103     c     *dtaara       define    §datiute      dDatiUte
152000110103      *
152100110103     c                   in(E)     *dtaara
152200110103     c                   IF        %Error or RSUT = *blanks
152300110103     c                   clear                   TIBS34ds
152400110103     c                   call      'TIBS34R'
152500110103     c                   parm                    TIBS34ds
152600110103     c                   in        *dtaara
152700110103     c                   ENDIF
152800110103      *
152900110103     c                   ENDSR
153000000512**
153100920709 ***   GESTIONE  FOGLI  VARI   ***
153200950206**
153300950206Filiale gestione errata
153400000128Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Corrente
153500000512Immissione/manutenzione record non gestita per i fogli di tipo 8
153600020422Data inferiore alla data odierna impossibile l'inserimento x fogli Cat 4
153700020422Data oltre il limite consentito per i fogli categoria 4
153800020909Manca il numeratore dei Fogli xxxxxxxxxx: TELEFONARE IN SEDE!!!!               6
153900020909Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli xxxxxxxxxx   7
154000030701Data non inferiore di 1 g e non superiore di XX gg rispetto alla Data Foglio
154100180112Presenti spunte: impossibile modificare foglio arrivi per incompatibili        9
