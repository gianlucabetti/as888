000100161129>>>>>H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND':'NDC167BND') ACTGRP('QILE')
000200981126     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300950608     H DECEDIT('0,') DATEDIT(*DMY.)
000400001229      *
000500001229C1396 * ATTENZIONE: il prtf NDC167P è in "share" con il modulo NDC167R1
000600001229  "   *             (che stampa il prospetto di liquidazione), per cui
000700001229  "   *             nel caso di modifica del prtf è necessario almeno
000800001229C1396 *             ricompilare anche il modulo NDC167R1
000900060801C1955 *             ricompilare anche il modulo NDC167R2
001000001229      *
001100900731     F*
001200991230     F* -----------------------
001300950724     F*  STAMPA RIEPILOGHI IVA
001400950726     F* -----------------------
001500950726     F*
001600991230     F* MAPPA INDICATORI:
001700000202     F*  13 - Liquidazione IVA non mensile / N13 - Liquidazione IVA mensile
001800000202     F*  14 - Regime IVA trasportatori / N14 - Regima IVA normale
001900980721     F*  60 - Segnalazione stampa aliquota
002000950727     F*  70 - Segnalazione stampa altre operazioni
002100950726     F*  80 - Operazioni imponibili / N80 - Operazioni non imponibili
002200950726     F*  81 - Registro vendite
002300950726     F*  82 - Registro corrispettivi
002400950726     F*  83 - Registro acquisti
002500950726     F*  84 - Stampa riepiloghi con causale
002600950727     F*  85 - No riepilogo imponibili/non imponibili
002700950726     F*  86 - Stampa riepiloghi per registro
002800950727     F*  87 - Stampa liquidazione
002900950726     F*  90 - Non stampa indetraibili
003000970227     F*  95 - Plafond fisso / N95 - Plafond mobile
003100991230     F*  96 - Stampa provvisoria / N96 - Stampa bollato
003200991230     F*----------------------------------------------------*
003300020424C1550 * Soc/TpLiq/Anno/PerLiq/TpReg/Libro/Aliq/RifIva/CausTes/CausRig
003400991230    >FNDRIV03L  IF   E           K DISK    RENAME(NDRIV000:NDRIV03)
003500020430C1550 * Soc/TpLiq/Anno/PerLiq/TpReg/Aliq/RifIva
003600991230    >FNDRIV04L  IF   E           K DISK    RENAME(NDRIV000:NDRIV04)
003700021010C1590 * Soc/TpLiq/Anno/PerLiq/TpReg/Libro
003800021010C1590FNDERI02L  IF   E           K DISK
003900950726    >FANDLI01L  IF   E           K DISK
004000950724    >FANRIV01L  IF   E           K DISK
004100950725    >FANOPE01L  IF   E           K DISK
004200951221    >FNDPLF01L  UF   E           K DISK
004300951221    >FNDMIV03L  UF   E           K DISK
004400090126C2106F*------------
004500090126C2106FANAIV01L  IF   E           K DISK
004600090120C2106FANAIL01L  IF   E           K DISK
004700090120C2106F*------------
004800950724    >FNDC167P   O    E             PRINTER OFLIND(*IN39)
004900130620>>>>>F                                     INFDS(YInf167P)
005000991230     F*----------------------------------------------------*
005100130620>>>>>D Annullato       C                   ' A N N U L L A T O '
005200130620>>>>>D YInf167P        DS
005300130620>>>>>D  YSpoolNbr            123    124I 0
005302161129>>>>>D  YCurLine             367    368I 0
005500950627     D*-------------
005600900731     D KPJBA         E DS
005700961009     D*-------------
005800961009     D* Dati di ambiente ottenuti da XSOC
005900961009     D SOC001        E DS                  EXTNAME(XSOC001DS)
006000970227     D    XscPlafond                  1    overlay(XscFgo:3)
006100970227     D    XscDare                     1    overlay(XscFgo:7)
006200970227     D    XscAvere                    1    overlay(XscFgo:8)
006300980722     D    XscIvaRgm                   1    overlay(XscFgo:9)
006400980824     D    XscIvaLiq                   1    overlay(XscFgo:10)
006500961009     D*-------------
006600961009     D* DS Interna per dati di output di XSOC
006700961009     D XSOCDS          DS          1000
006800950605     D*-------------
006900950130     D* Parametri del PGM
007000950724     D C167DS        E DS                  EXTNAME(NDC167DS)
007100950628     D*-------------
007200950704     D* Reperimento nome PGM
007300950704     D STATUS         SDS           333
007400950704     D  DSPGM            *PROC
007401161129>>>>>D  psJobName            244    253                                         Nome lavoro
007402161129>>>>>D  psJobUser            254    263                                         Utente
007403161129>>>>>D  psJobNbr             264    269                                         Numero lavoro
007500950724     D*-------------
007600950726     D* Decodifica mese
007700951221     D X07DS         E DS                  EXTNAME(X07VALDS)
007800950726     D*-------------
007900980721     D* Decodifica periodo
008000980721     D X62DS         E DS                  EXTNAME(X62IVALQDS)
008100980721     D*-------------
008200030623c1668D* Parametri memorizzazione intestazione
008300030623  "  D xpaout          DS          2000
008400030623  "  D*-------------
008500030623  "  D* Parametro modulo IVA
008600030623  "  D ANP045DS      E DS
008700030623  "  D*-------------
008800030623  "  D* DS esterna per stampa intestazione bollato
008900030623  "  D ndc099ds      E DS
009000030623  "  D*-------------
009100030623  "  D* Tabella numeri pagina
009200030623  "  D Ang008Ds      E DS
009300030623  "  D*-------------
009400030623  "  D* Richiamo a XATB
009500030623c1668D XATBDS        E DS                  INZ
009600090626C2131D*-------------
009700090626  "  D WAnno           S              4S 0
009800090626  "  D WImpInp         S                   Like(ImporD0202)
009900090626  "  D WImpOut         S                   Like(ImporD0202)
010000090626  "  D*----------------
010100090626  "  D* numeri decimali della m.c.
010200090626  "  D NrDecMC         S              1  0
010300090626  "  D WNrDecMC        S              1
010400090626  "  D*-------------
010500090626  "  D* PRORATA: percetuali di detraibilità IVA
010600090626  "  D ANG046DS      E DS
010700090626C2131D $ProRata        S              1
010800030623     D*-------------
010900001004C1425D SAVLibro        S                   Like(DLILibro)
011000001004C1425D*-------------
011100001212C1451D $RivRegLib      DS
011200001212  "  D  RIVTpReg
011300001212  "  D  RIVLibro
011400001212  "  D*-------------
011500001212  "  D $DliRegLib      DS
011600001212  "  D  DLITpReg
011700001212  "  D  DLILibro
011800001212  "  D*-------------
011900001212  "  D  SkRegLib       S              4A   Dim(200)
012000001212  "  D  SkImporto      S                   Dim(200) Like(RIVImporto)
012100001212  "  D*-------------
012200001212  "  D i               S              3  0
012300001212C1451D*-------------
012400030721D1404D xdtdat          S               D
012500040210C1451D*-------------
012600000719C1396X***** $ImpNegat       S              1A
012700000719  "  X***** Oggi            S               D
012800000719  "  X***** CdInter         S                   Like(DVSCdInter)
012900000719  "  X***** NrDec           S              1  0
013000000719  "  X***** WImpArr         S                   Like(RIVImporto)
013100000719  "  X***** WImpArr0D       S             16S 0
013200000719  "  X***** W3015           S             30S15
013300000719  "  X***** TotIvaDeb       S                   Like(RIVImporto)
013400000719  "  X***** TotIvaCre       S                   Like(RIVImporto)
013500000719  "  X*-------------
013600000719  "  X* Prototipazione procedure EURO
013700000719  "  X*****/COPY *LIBL/SRCCPY,PJX002PR
013800000719C1396X*-------------
013900090626C2131D*-------------
014000090626  "  D* Prototipazione procedure EURO
014100090626C2131D/COPY *LIBL/SRCCPY,PJX002PR
014200020927C1583 *----------------------------------------------------*
014300020927  "   * aggiunto in stampa il nuovo saldo
014400020927  "  D SalNuovo        s                   like(SPr167)
014500020927C1583 *----------------------------------------------------*
014600031202C1703 * Param.Mod.'IVALIQ' (per gestione Iva di gruppo)
014700040210  "  D ndc149ds      e ds
014800040210C1703 *----------------------------------------------------*
014900031219D1519 * Schiera decodifica periodo
015000031219  "  D $LisP           s              1a   dim(5) CTDATA PERRCD(1)
015100031219  "  D $OrdP           s              1s 0 dim(5) alt($LisP)
015200031219  "  D $DesP           s             27    dim(5) CTDATA PERRCD(1)
015300031219  "   *---
015400031219  "   * indice schiere
015500031219  "  D $xp             s              1s 0
015600031219  "  D $$xp            s              1s 0
015700031219  "   *----------------------------------------------------*
015800031219  "   * Calcolo periodo
015900031219  "  D $Periodo        S              2s 0
016000031219  "  D $Resto          S              3  0
016100031219  "  D $MeseNum        S              2s 0
016200031219  "  D $TpLiqNum       S              1s 0
016300031219D1519 *----------------------------------------------------*
016400040727C1781 * Variabili per chiamata a X72Txt x stampare testo
016500040727  "  D XEsito          S              1A
016600040727  "  D XOpe            S              1A
016700040727  "  D XSocieta        S              3A
016800040727  "  D XTpTesto        S              2A
016900040727  "  D XUnita          S              8A
017000040727  "  D XLingua         S              1A
017100040727  "  D XClaUte         S              2A
017200040727  "  D XPosRig         S              1A
017300040727  "  D XNrRiga         S              9P 0
017400040727  "  D XRibat          S              1A
017500040727  "  D XTesRig         S             70A
017600040727  "  D XNrChiavi       S              1  0
017700040727  "  D XUsoUnita       S              1A
017701161129>>>>> /COPY GAITRASRC/SRCPROTOPR,TRBMAF05R
017800040727C1781 *----------------------------------------------------*
017900950725     I* Ridenomino i campi di ANRIV
018000950725     IANRIV000
018100950725     I              RIVSOCIETA                  RIFSOC
018200950725     I              RIVTPREG                    RIFTPR
018300950725     I              RIVDESCR                    RIFDES
018400950725     I              RIVDESBREV                  RIFDBR
018500950725     I              RIVALIQ                     RIFALQ
018600950725     I              RIVRIFIVA                   RIFRIF
018700991230     I*-------------
018800950405     C*----------------------------------------------------*
018900950405     C*                MAIN LINE PROGRAM
019000950405     C*----------------------------------------------------*
019100991230     C*
019200950606     C* Inizializzazione variabili
019300950606     C                   EXSR      INZVAR
019400950724     C*
019500951221     C* Se non richiesta stampa solo plafond .....
019600951221     C     stp167        ifne      '2'
019700951221     C*
019800951221     C* 1) Riepilogo IVA operazioni imponibili
019900950725     C                   EXSR      OPEIMP
020000950724     C*
020100951221     C* 2) Riepilogo IVA operazioni non imponibili
020200950725     C                   EXSR      OPENON
020300950725     C*
020400951221     C* 3) Riepiloghi per registro
020500950726     C                   EXSR      RIEREG
020600950726     C*
020700951221     C* 4) Liquidazione IVA
020800950727     C                   EXSR      LIQIVA
020900000719C1396C*
021000020222C1396C* 4.1) prospetto liquidazione IVA
021100020222D1095C* Se richiesta la stampa
021200020222D1095c                   if        ProsLiq167 = *on
021300000719C1396C                   ExSr      ProsLiqIVA
021400020222D1095C                   endif
021500951113     C*
021600951221     C                   endif
021700951221     C*
021800970320     C* Se non richiesta stampa escluso plafond ... (es: plafond non gest.)
021900951221     C     stp167        ifne      '0'
022000020429C1550 *
022100020429  "   * se richiesta tipo liquidazione mensile
022200020429  "   * oppure non richiesta tipo liquidazione
022300020429  "   * (tp liq valorizzato solo se regime iva trasportatori mensile )
022400020429C1550C                   if        TpLiq167 ='1' or TpLiq167=*blank
022500951221     C*
022600951221     C* 5) Riepilogo plafond
022700951113     C                   EXSR      RIEPLF
022800951221     C*
022900020429C1550C                   endif
023000951221     C                   endif
023100951221     C*
023200951221     C* Se non richiesta stampa solo plafond ....
023300951221     C     stp167        ifne      '2'
023400020429C1550 *
023500020429  "   * se richiesta tipo liquidazione mensile
023600020429  "   * oppure non richiesta tipo liquidazione
023700020429  "   * (tp liq valorizzato solo se regime iva trasportatori mensile )
023800020429C1550C                   if        TpLiq167 ='1' or TpLiq167=*blank
023900950725     C*
024000951221     C* 6) Legenda libri IVA
024100950725     C                   EXSR      LEGIVA
024200900731     C*
024300020429C1550C                   endif
024400031201C1703 *
024500031201  "   * 7) Iva di gruppo
024600031201  "   *    Se richiesta la stampa
024700031201  "  c                   if        StpLiqG167 = *on
024800031201  "  C                   ExSr      IvaDiGruppo
024900031201C1703C                   endif
025000951221     C                   endif
025100951221     C*
025200130620>>>>>C                   EXSR      YAnnRiga
025300161129>>>>>C*                  EXSR      YCpySplF
025301161130>>>>>C                   IF        SendToDocFlow() < *ZERO
025302161130     C                   EVAL      err167 = *ON
025304161130     C                   ENDIF
025400950606     C* Fine programma
025500950606     C                   EXSR      ENDPGM
025600950606     C*
025700950606     C/EJECT
025800950725     C************************************************************
025900950725     C* RIEPILOGO OPERAZIONI IMPONIBILI
026000950725     C************************************************************
026100950725     C     OPEIMP        BEGSR
026200950725     C*
026300950725     C                   move      '1'           opeimpon          1
026400950725     C                   seton                                        80
026500030623C1668C                   exsr      StampaTesta
026600030623c1668x** 96              WRITE     TESTA0
026700950725     C                   WRITE     TESTA
026800991112     C                   setoff                                       39
026900950725     C*
027000950725     C* Registro vendite
027100950725     C                   seton                                        81
027200950725     C                   move      soc167        dlisocieta
027300950725     C                   move      '2'           dlitpreg
027400950725     C                   EXSR      RIEIVA
027500950725     C*
027600950725     C* Registro corrispettivi
027700950725     C                   seton                                        82
027800950725     C                   setoff                                       81
027900030721D1404C* Se gestiti
028000030721D1404c                   if        GestCorr=*on
028100950725     C                   move      soc167        dlisocieta
028200950725     C                   move      '3'           dlitpreg
028300950725     C                   EXSR      RIEIVA
028400030721D1404C                   endif
028500950725     C*
028600950725     C* Registro acquisti
028700950725     C                   seton                                        83
028800950725     C                   setoff                                       82
028900950725     C                   move      soc167        dlisocieta
029000950725     C                   move      '1'           dlitpreg
029100950725     C                   EXSR      RIEIVA
029200950725     C*
029300950725     C                   ENDSR
029400950725     C/EJECT
029500030623c1668C************************************************************
029600030623     C* stampa testata
029700030623c1668C************************************************************
029800030623     C     StampaTesta   BEGSR
029900030623     c*
030000030624     c* se richieste intestazioni
030100030623     C                   if        Intesta167 =*off
030200030624     c* solo se non bollato (96 ON) stampa testata fissa a prtf
030300030623     C   96              WRITE     TESTA0
030400030623     c                   else
030500030624     c* sia bollato che provvisorio
030600030624     c* stampo intestazione memorizzata su param. modulo
030700030623     c                   eval      T0riga1 =rg1099
030800030623     c* anno/pagina
030900030623     c                   move      numeropag     numpag            6
031000030623     C                   eval      P1AnnoPag= Anno099+ '/' + numpag
031100030623     c                   eval      T0riga2 =rg2099
031200030623     c                   eval      T0riga3 =rg3099
031300030623     c                   eval      T0riga4 =rg4099
031400030623     C                   ADD       1             NumeroPag
031500030624     C                   WRITE     TESTA0A
031600030623     c                   endif
031700030623     C*
031800030623c1668C                   ENDSR
031900950725     C************************************************************
032000950725     C* RIEPILOGO OPERAZIONI NON IMPONIBILI
032100950725     C************************************************************
032200950725     C     OPENON        BEGSR
032300950725     C*
032400950725     C                   move      '0'           opeimpon
032500950725     C                   setoff                                       80
032600030623C1668C                   exsr      StampaTesta
032700030623c1668x** 96              WRITE     TESTA0
032800950725     C                   WRITE     TESTA
032900991112     C                   setoff                                       39
033000950725     C*
033100950725     C* Registro vendite
033200950725     C                   seton                                        81
033300950725     C                   setoff                                       83
033400950725     C                   move      soc167        dlisocieta
033500950725     C                   move      '2'           dlitpreg
033600950725     C                   EXSR      RIEIVA
033700950725     C*
033800950725     C* Registro corrispettivi
033900950725     C                   seton                                        82
034000950725     C                   setoff                                       81
034100030721D1404C* Se gestiti
034200030721D1404c                   if        GestCorr=*on
034300950725     C                   move      soc167        dlisocieta
034400950725     C                   move      '3'           dlitpreg
034500950725     C                   EXSR      RIEIVA
034600030721D1404C                   endif
034700950725     C*
034800950725     C* Registro acquisti
034900950725     C                   seton                                        83
035000950725     C                   setoff                                       82
035100950725     C                   move      soc167        dlisocieta
035200950725     C                   move      '1'           dlitpreg
035300950725     C                   EXSR      RIEIVA
035400950725     C*
035500950725     C                   ENDSR
035600950725     C/EJECT
035700950724     C************************************************************
035800950724     C* RIEPILOGHI IVA
035900950724     C************************************************************
036000950724     C     RIEIVA        BEGSR
036100950724     C*
036200950725     C* Testata registro
036300950725     C     *in39         ifeq      *on
036400030623C1668C                   exsr      StampaTesta
036500030623c1668x** 96              WRITE     TESTA0
036600950725     C                   WRITE     TESTA
036700950725     C                   setoff                                       39
036800950725     C                   endif
036900950725     C                   WRITE     TESTA1
037000950725     C*
037100950724     C* Se registro acquisti stampo anche indetraibili
037200950724     C     dlitpreg      ifeq      '1'
037300950724     C                   setoff                                       90
037400950724     C                   else
037500950724     C                   seton                                        90
037600950724     C                   endif
037700090120C2106C*
037800090120  "  C                   If         GesPlur045 = *on
037900090120  "  C* Lettura ANAIL01L per soc./cod.att./tipo reg.
038000090120  "  C     K3AIL1        SETLL     ANAIL01L                           34
038100090120  "  C     *IN34         DOWEQ     *OFF
038200090120  "  C     K3AIL1        READE     ANAIL01L                               34
038300090120  "  C                   IF        (*IN34 = *OFF)
038400090120  "  C                   move      aillibro      dlilibro
038500090120  "  C     K3DLI1        chain     andli01l                           21
038600090120  "  C                   if        (*in21 = *off)
038700090120  "  C                   EXSR      RIEIVA2
038800090120  "  C                   EndIf
038900090120  "  C                   EndIf
039000090120  "  C                   Enddo
039100090120C2106C                   else
039200950725     C*
039300950724     C     k2dli1        setll     andli01l
039400950724     C     k2dli1        reade     andli01l                               21
039500950724     C*
039600950724   B1C     *in21         doweq     *off
039700090120C2106C                   EXSR      RIEIVA2
039800090120C2106C* ---> Specifiche spostate nella routine RIEIVA2
039900090120C1425C*** ex c1425  Scarto libri sospesi/differiti
040000090120C2106X**** EX C1425      If        DLIsospens <> '1' and DLISospens <> '4'
040100090120  "  C*
040200090120  "  C***                move      soc167        rivsoc
040300090120C2106C***                move      ann167        rivanno
040400000720C1396X*****              If        XscIvaLiq = '1'
040500000720  "  X*****              move      mes167        rivperliq
040600000720C1396X*****              Else
040700090120C2106C****               move      per167        rivperliq
040800000720C1396X*****              EndIf
040900090120C2106C****               move      dlitpreg      rivtpreg
041000090120  "  C****               move      dlilibro      rivlibro
041100090120  "  C*
041200090120  "  C**** opeimpon      ifeq      '1'
041300090120  "  C*
041400090120  "  C* Stampa riepilogo op. imponibili libro
041500090120  "  C****               EXSR      RIEALQ
041600090120  "  C****               else
041700090120  "  C*
041800090120  "  C* Stampa riepilogo op. non imponibili libro
041900090120  "  C****               EXSR      RIEAOP
042000090120  "  C****               endif
042100090120  "  C*
042200090120  "  C* Se libro vuoto (per il periodo selezionato) stampo segnalazione
042300090120  "  C**** *in60         ifeq      *off
042400090120  "  C**** *in70         andeq     *off
042500090120  "  X****               WRITE     VUOTO
042600090120  "  X****               endif
042700090120  "  C*
042800090120  "  C* Inizializzazioni
042900090120  "  X****               setoff                                       6070
043000090120  "  X****               z-add     0             t1totimpon
043100090120  "  X****               z-add     0             t1totimpos
043200090120  "  X****               z-add     0             t1totgen
043300090120  "  X****               z-add     0             t1imponnor
043400090120  "  X****               z-add     0             t1imposnor
043500090120  "  X****               z-add     0             t1imponind
043600090120  "  X****               z-add     0             t1imposind
043700090120C2106X****               z-add     0             t2totimpon
043800001004C1425C*
043900090120C2106X**** EX C1425      EndIf
044000950724     C*
044100950724     C     k2dli1        reade     andli01l                               21
044200950724     C                   enddo
044300090120C2106C                   endif
044400950724     C*
044500950724     C                   ENDSR
044600950724     C/EJECT
044700090120C2106C************************************************************
044800090120  "  C* OPERAZIONI RELATIVE AL LIBRO
044900090120C2106C************************************************************
045000090120     C     RIEIVA2       BEGSR
045100090120C1425C* Scarto libri sospesi/differiti
045200090120C1425C                   If        DLIsospens <> '1' and DLISospens <> '4'
045201180105R275 C                             and DLIsospens <> '9'
045300090120     C*
045400090120     C                   move      soc167        rivsoc
045500090120     C                   move      ann167        rivanno
045600090120C1396X*****              If        XscIvaLiq = '1'
045700090120  "  X*****              move      mes167        rivperliq
045800090120C1396X*****              Else
045900090120     C                   move      per167        rivperliq
046000090120C1396X*****              EndIf
046100090608     C                   move      dlitpreg      rivtpreg
046200090608     C                   move      dlilibro      rivlibro
046300090608     C*
046400090608     C     opeimpon      ifeq      '1'
046500090608     C*
046600090608     C* Stampa riepilogo op. imponibili libro
046700090608     C                   EXSR      RIEALQ
046800090608     C                   else
046900090608     C*
047000090608     C* Stampa riepilogo op. non imponibili libro
047100090608     C                   EXSR      RIEAOP
047200090608     C                   endif
047300090608     C*
047400090608     C* Se libro vuoto (per il periodo selezionato) stampo segnalazione
047500090608     C     *in60         ifeq      *off
047600090608     C     *in70         andeq     *off
047700090608     C                   WRITE     VUOTO
047800090608     C                   endif
047900090608     C*
048000090608     C* Inizializzazioni
048100090608     C                   setoff                                       6070
048200090608     C                   z-add     0             t1totimpon
048300090608     C                   z-add     0             t1totimpos
048400090608     C                   z-add     0             t1totgen
048500090608     C                   z-add     0             t1imponnor
048600090608     C                   z-add     0             t1imposnor
048700090608     C                   z-add     0             t1imponind
048800090608     C                   z-add     0             t1imposind
048900090608     C                   z-add     0             t2totimpon
049000090608C1425C*
049100090120C1425C                   EndIf
049200090120     C*
049300090120     C                   ENDSR
049400090120C2106C/EJECT
049500950703     C************************************************************
049600950724     C* RIEPILOGO OPERAZIONI IMPONIBILI LIBRO
049700950703     C************************************************************
049800950724     C     RIEALQ        BEGSR
049900950724     C*
050000950725     C* Testata libro
050100950725     C     *in39         ifeq      *on
050200030623C1668C                   exsr      StampaTesta
050300030623c1668x** 96              WRITE     TESTA0
050400950725     C                   WRITE     TESTA
050500950725     C                   WRITE     TESTA1
050600950725     C                   setoff                                       39
050700950725     C                   endif
050800950725     C                   movel     rivlibro      tlibro
050900090213C2106C                   eval      tdeslibro = dlidescr
051000130620>>>>>C                   movel     rivsoc        tsocieta
051100950725     C                   WRITE     TESTA2
051200950724     C*
051300950724     C* Posizionamento per aliquota > 0
051400950724     C                   z-add     0             rivaliq
051500020424C1550C                   move      TpLiq167      RIVTpLiq
051600020424C1550X**** k6riv3        setgt     ndriv03l
051700020424C1550C     k7riv3        setgt     ndriv03l
051800950724     C*
051900020424C1550X**** k5riv3        reade     ndriv03l                               21
052000020424C1550C     k6riv3        reade     ndriv03l                               21
052100950724     C*
052200950724     C                   z-add     rivaliq       savaliq
052300950725     C                   move      rivcaustes    savcaustes
052400950724     C*
052500950724     C     *in21         doweq     *off
052600950724     C*
052700950724     C     *in60         ifeq      *off
052800950724     C                   seton                                        60
052900950724     C                   WRITE     TEST1
053000950724     C                   endif
053100950725     C*
053200950725     C* Stampa totali causale
053300950725     C     cau167        ifeq      *on
053400950725     C     rivcaustes    ifne      savcaustes
053500960320     C     rivaliq       orne      savaliq
053600950725     C                   EXSR      STACAU
053700950725     C                   move      rivcaustes    savcaustes
053800950725     C                   endif
053900950725     C                   endif
054000950724     C*
054100950725     C* Stampo totali aliquota
054200950724     C     rivaliq       ifne      savaliq
054300950724     C                   EXSR      STAALQ
054400950724     C                   z-add     rivaliq       savaliq
054500950724     C                   endif
054600950724     C*
054700950725     C* Incremento totali causale
054800950725     C     cau167        ifeq      *on
054900950727     C                   add       rivimponib    p3imponnor
055000950727     C                   add       rivimporto    p3imposnor
055100950725     C     rivtpreg      ifeq      '1'
055200950727     C                   add       rivimpind     p3imponind
055300950727     C                   add       rivimpoind    p3imposind
055400950725     C                   endif
055500950725     C                   endif
055600950725     C*
055700950724     C* Incremento totali aliquota
055800950727     C                   add       rivimponib    p1imponnor
055900950727     C                   add       rivimporto    p1imposnor
056000950724     C     rivtpreg      ifeq      '1'
056100950727     C                   add       rivimpind     p1imponind
056200950727     C                   add       rivimpoind    p1imposind
056300950724     C                   endif
056400001212C1451C*
056500001212  "  C* skierizzo totali imposta per registro/libro
056600001212  "  C                   If        OpeImpon = '1'
056700001212  "  C                   Eval      i = 1
056800001212  "  C     $RivRegLib    LookUp    SkRegLib(i)                            24
056900001212  "  C                   If        *In24 = *On
057000001212  "  C                   Add       RIVImporto    SkImporto(i)
057100001212  "  C                   Else
057200001212  "  C     *Blank        LookUp    SkRegLib(i)                            24
057300001212  "  C                   If        *In24 = *On
057400001212  "  C                   Eval      SkRegLib(i) = $RivRegLib
057500001212  "  C                   Z-Add     RIVImporto    SkImporto(i)
057600001212  "  C                   EndIf
057700001212  "  C                   EndIf
057800001212C1451C                   EndIf
057900950724     C*
058000020424C1550X**** k5riv3        reade     ndriv03l                               21
058100020424C1550C     k6riv3        reade     ndriv03l                               21
058200950724     C*
058300950724     C                   enddo
058400950724     C*
058500950724     C     *in60         ifeq      *on
058600950725     C*
058700950725     C* Stampa totali ultima causale
058800950725     C     cau167        ifeq      *on
058900950725     C                   EXSR      STACAU
059000950725     C                   endif
059100950724     C*
059200950724     C* Stampo totali ultima aliquota
059300950724     C                   EXSR      STAALQ
059400950724     C*
059500950724     C* Stampa totali generali aliquote
059600950724     C     *in39         ifeq      *on
059700030623C1668C                   exsr      StampaTesta
059800030623c1668x** 96              WRITE     TESTA0
059900950724     C                   WRITE     TESTA
060000950725     C                   WRITE     TESTA1
060100950725     C                   WRITE     TESTA2
060200950724     C                   setoff                                       39
060300950724     C                   endif
060400950724     C                   WRITE     TOTT1
060500950726     C*
060600950724     C                   endif
060700950703     C*
060800950703     C                   ENDSR
060900950703     C/EJECT
061000950724     C************************************************************
061100950724     C* STAMPA TOTALI ALIQUOTA
061200950724     C************************************************************
061300950724     C     STAALQ        BEGSR
061400950724     C*
061500950727     C                   z-add     savaliq       p1aliq
061600950724     C* Reperisco descrizione aliquota
061700950724     C                   move      rivsoc        rifsoc
061800950725     C                   move      rivtpreg      riftpr
061900950725     C                   z-add     savaliq       rifalq
062000950725     C     k3rif1        chain     anriv01l                           23
062100950727     C  N23              move      rifdbr        p1desali
062200950727     C   23              move      *blank        p1desali
062300950724     C* Totali aliquota
062400950727     C                   eval      p1totimpon=p1imponnor+p1imponind
062500950727     C                   eval      p1totimpos=p1imposnor+p1imposind
062600950727     C                   eval      p1totgen=p1totimpon+p1totimpos
062700950724     C* Incremento totali generali
062800950727     C                   eval      t1totimpon=t1totimpon+p1totimpon
062900950727     C                   eval      t1totimpos=t1totimpos+p1totimpos
063000950727     C                   eval      t1totgen=t1totgen+p1totgen
063100951215     C                   eval      t1imponnor=t1imponnor+p1imponnor
063200951215     C                   eval      t1imposnor=t1imposnor+p1imposnor
063300951215     C                   eval      t1imponind=t1imponind+p1imponind
063400951215     C                   eval      t1imposind=t1imposind+p1imposind
063500950724     C* Stampa totali aliquota
063600950724     C     *in39         ifeq      *on
063700030623C1668C                   exsr      StampaTesta
063800030623c1668x** 96              WRITE     TESTA0
063900950724     C                   WRITE     TESTA
064000950725     C                   WRITE     TESTA1
064100950726     C  N86              WRITE     TESTA2
064200950724     C                   WRITE     TEST1
064300950724     C                   setoff                                       39
064400950724     C                   endif
064500950727     C                   WRITE     ALIQP1
064600950724     C* Azzero totali aliquota
064700950727     C                   z-add     0             p1imponnor
064800950727     C                   z-add     0             p1imposnor
064900950727     C                   z-add     0             p1imponind
065000950727     C                   z-add     0             p1imposind
065100950724     C*
065200950724     C                   ENDSR
065300950724     C/EJECT
065400950703     C************************************************************
065500950724     C* RIEPILOGO ALTRE OPERAZIONI LIBRO
065600950703     C************************************************************
065700950703     C     RIEAOP        BEGSR
065800950725     C*
065900950725     C* Testata libro
066000950725     C     *in39         ifeq      *on
066100030623C1668C                   exsr      StampaTesta
066200030623c1668x** 96              WRITE     TESTA0
066300950725     C                   WRITE     TESTA
066400950725     C                   WRITE     TESTA1
066500950725     C                   setoff                                       39
066600950725     C                   endif
066700950725     C                   movel     rivlibro      tlibro
066800090213C2106C                   eval      tdeslibro = dlidescr
066900130620>>>>>C                   movel     rivsoc        tsocieta
067000950725     C                   WRITE     TESTA2
067100950704     C*
067200950712     C* Posizionamento per aliquota = 0
067300950724     C                   z-add     0             rivaliq
067400020424C1550C                   move      TpLiq167      RIVTpLiq
067500020424C1550X**** k6riv3        setll     ndriv03l
067600020424C1550C     k7riv3        setll     ndriv03l
067700950704     C*
067800020424C1550X**** k6riv3        reade     ndriv03l                               21
067900020424C1550C     k7riv3        reade     ndriv03l                               21
068000950712     C*
068100950724     C                   move      rivrifiva     savrifiva
068200950725     C                   move      rivcaustes    savcaustes
068300950712     C*
068400950704     C     *in21         doweq     *off
068500950712     C*
068600950712     C     *in70         ifeq      *off
068700950712     C                   seton                                        70
068800950724     C                   WRITE     TEST2
068900950712     C                   endif
069000950725     C*
069100950725     C* Stampa totali causale
069200950725     C     cau167        ifeq      *on
069300950725     C     rivcaustes    ifne      savcaustes
069400960320     C     rivrifiva     orne      savrifiva
069500950725     C                   EXSR      STACAU
069600950725     C                   move      rivcaustes    savcaustes
069700950725     C                   endif
069800950725     C                   endif
069900950704     C*
070000950726     C* Stampo totale altre operazioni
070100950724     C     rivrifiva     ifne      savrifiva
070200950724     C                   EXSR      STAAOP
070300950724     C                   move      rivrifiva     savrifiva
070400950724     C                   endif
070500950725     C*
070600950725     C* Incremento totali causale
070700950725     C     cau167        ifeq      *on
070800950727     C                   add       rivimponib    p3totimpon
070900950725     C                   endif
071000950724     C*
071100950726     C* Incremento totale altre operazioni
071200950724     C                   add       rivimponib    p2imponib
071300950724     C*
071400020424C1550X**** k6riv3        reade     ndriv03l                               21
071500020424     C     k7riv3        reade     ndriv03l                               21
071600950704     C*
071700950704     C                   enddo
071800950724     C*
071900950724     C     *in70         ifeq      *on
072000950725     C*
072100950726     C* Stampa totale ultima causale
072200950725     C     cau167        ifeq      *on
072300950725     C                   EXSR      STACAU
072400950725     C                   endif
072500950724     C*
072600950726     C* Stampo ultimo totale altre operazioni
072700950724     C                   EXSR      STAAOP
072800950724     C*
072900950726     C* Stampa totale generale altre operazioni
073000950704     C     *in39         ifeq      *on
073100030623C1668C                   exsr      StampaTesta
073200030623c1668x** 96              WRITE     TESTA0
073300950724     C                   WRITE     TESTA
073400950725     C                   WRITE     TESTA1
073500950725     C                   WRITE     TESTA2
073600950724     C                   setoff                                       39
073700950704     C                   endif
073800950724     C                   WRITE     TOTT2
073900950726     C*
074000950710     C                   endif
074100950703     C*
074200950703     C                   ENDSR
074300950703     C/EJECT
074400950724     C************************************************************
074500950724     C* STAMPA TOTALI ALTRE OPERAZIONI
074600950724     C************************************************************
074700950724     C     STAAOP        BEGSR
074800950724     C*
074900950724     C                   move      savrifiva     p2rifiva
075000950724     C* Reperisco descrizione rifiva
075100950724     C                   move      rivsoc        rifsoc
075200950725     C                   move      rivtpreg      riftpr
075300950725     C                   z-add     0             rifalq
075400950725     C                   move      savrifiva     rifrif
075500950725     C     k4rif1        chain     anriv01l                           23
075600950725     C  N23              move      rifdbr        p2desrif
075700950724     C   23              move      *blank        p2desrif
075800950724     C* Incremento totale generale
075900950724     C                   eval      t2totimpon=t2totimpon+p2imponib
076000950726     C* Stampa totale altre operazioni
076100950724     C     *in39         ifeq      *on
076200030623C1668C                   exsr      StampaTesta
076300030623c1668x** 96              WRITE     TESTA0
076400950724     C                   WRITE     TESTA
076500950725     C                   WRITE     TESTA1
076600950726     C  N86              WRITE     TESTA2
076700950724     C                   WRITE     TEST2
076800950724     C                   setoff                                       39
076900950724     C                   endif
077000950727     C                   WRITE     ALOPP2
077100950726     C* Azzero totale altre operazioni
077200950724     C                   z-add     0             p2imponib
077300950724     C*
077400950724     C                   ENDSR
077500950724     C/EJECT
077600950725     C************************************************************
077700950725     C* STAMPA TOTALI CAUSALE
077800950725     C************************************************************
077900950725     C     STACAU        BEGSR
078000950725     C*
078100950727     C                   move      savcaustes    p3causale
078200950725     C* Reperisco descrizione causale
078300950725     C                   move      soc167        opesocieta
078400960320     C                   move      savcaustes    opecausale
078500950725     C     k2ope1        chain     anope01l                           23
078600950727     C  N23              movel     opedesbrev    p3descau
078700950727     C   23              move      *blank        p3descau
078800950725     C* Totali causale (per operazioni imponibili)
078900950725     C     opeimpon      ifeq      '1'
079000950727     C                   eval      p3totimpon=p3imponnor+p3imponind
079100950727     C                   eval      p3totimpos=p3imposnor+p3imposind
079200950727     C                   eval      p3totgen=p3totimpon+p3totimpos
079300950725     C                   endif
079400950725     C* Stampa totali causale
079500950725     C     *in39         ifeq      *on
079600030623C1668C                   exsr      StampaTesta
079700030623c1668x** 96              WRITE     TESTA0
079800950725     C                   WRITE     TESTA
079900950725     C                   WRITE     TESTA1
080000950725     C                   WRITE     TESTA2
080100950725     C                   WRITE     TEST1
080200950725     C                   setoff                                       39
080300950725     C                   endif
080400950727     C                   WRITE     CAUSP3
080500950725     C* Azzero totali causale
080600950727     C                   z-add     0             p3imponnor
080700950727     C                   z-add     0             p3imposnor
080800950727     C                   z-add     0             p3imponind
080900950727     C                   z-add     0             p3imposind
081000950727     C                   z-add     0             p3totimpon
081100950727     C                   z-add     0             p3totimpos
081200950727     C                   z-add     0             p3totgen
081300950725     C*
081400950725     C                   ENDSR
081500950725     C/EJECT
081600950725     C************************************************************
081700950726     C* RIEPILOGHI PER REGISTRO
081800950725     C************************************************************
081900950726     C     RIEREG        BEGSR
082000000719C1396X*
082100000719  "  X*****              Clear                   TotIvaDeb
082200000719C1396X*****              Clear                   TotIvaCre
082300000719     C*
082400950725     C* Testata
082500950727     C                   seton                                        8586
082600950726     C                   setoff                                       846070
082700030623C1668C                   exsr      StampaTesta
082800030623c1668x** 96              WRITE     TESTA0
082900950725     C                   WRITE     TESTA
083000991112     C                   setoff                                       39
083100950726     C*
083200950726     C                   move      soc167        rivsoc
083300950726     C                   move      ann167        rivanno
083400000720C1396X*****              If        XscIvaLiq = '1'
083500000720  "  X*****              move      mes167        rivperliq
083600000720C1396X*****              Else
083700000202     C                   move      per167        rivperliq
083800000720C1396X*****              EndIf
083900950725     C*
084000950725     C* Registro vendite
084100950725     C                   seton                                        81
084200950725     C                   setoff                                       83
084300950726     C                   move      '2'           rivtpreg
084400950726     C                   EXSR      ALQREG
084500950726     C                   EXSR      AOPREG
084600950726     C                   EXSR      TOTREG
084700000719C1396X*
084800000719  "  X* Incremento totale iva a debito (con totale reg. vendite)
084900000719C1396X*****              Add       T4ImposNor    TotIvaDeb
085000950725     C*
085100950725     C* Registro corrispettivi
085200950725     C                   seton                                        82
085300950725     C                   setoff                                       81
085400030721D1404C* Se gestiti
085500030721D1404c                   if        GestCorr=*on
085600950726     C                   move      '3'           rivtpreg
085700950726     C                   EXSR      ALQREG
085800950726     C                   EXSR      AOPREG
085900950726     C                   EXSR      TOTREG
086000030721D1404C                   endif
086100000719C1396X*
086200000719  "  X* Incremento totale iva a debito (con totale reg. corrispettivi)
086300000719C1396X*****              Add       T4ImposNor    TotIvaDeb
086400950725     C*
086500950725     C* Registro acquisti
086600950725     C                   seton                                        83
086700950725     C                   setoff                                       82
086800950726     C                   move      '1'           rivtpreg
086900950726     C                   EXSR      ALQREG
087000950726     C                   EXSR      AOPREG
087100950726     C                   EXSR      TOTREG
087200000719C1396X*
087300000719  "  X* Incremento totale iva a credito (con totale reg. acquisti)
087400000719C1396X*****              Add       T4ImposNor    TotIvaCre
087500950727     C*
087600950727     C                   setoff                                       8586
087700000719C1396X*
087800000719  "  X* Arrotondo totali iva a credito e iva a debito
087900000719  "  X*****              Z-Add     TotIvaDeb     WImpArr
088000000719  "  X*****              ExSr      ArroItlEur
088100000719  "  X*****              Z-Add     WImpArr       TotIvaDeb
088200000719  "  X*****              Z-Add     TotIvaCre     WImpArr
088300000719  "  X*****              ExSr      ArroItlEur
088400000719  "  X*****              Z-Add     WImpArr       TotIvaCre
088500000719  "  X*
088600000719  "  X* Stampo riepilogo iva a credito, iva a debito e saldo periodo
088700000719  "  X*****              Z-Add     TotIvaDeb     T5TotIvaDe
088800000719  "  X*****              Z-Add     TotIvaCre     T5TotIvaCr
088900000719  "  X*****              Eval      T5SaldoPer = TotIvaDeb - TotIvaCre
089000000719  "  X*****              If        *In39 = *On
089100000719  "  X***96              WRITE     TESTA0
089200000719  "  X*****              WRITE     TESTA
089300000719  "  X*****              WRITE     TESTA1
089400000719  "  X*****              SetOff                                       39
089500000719  "  X*****              EndIf
089600000719C1396X*****              WRITE     TOTT5
089700000202     C*
089800950725     C                   ENDSR
089900950725     C/EJECT
090000000719C1396X************************************************************
090100000719  "  X* Arrotondamento importo in ITL o EUR
090200000719  "  X************************************************************
090300000719  "  X*****ArroItlEur    BEGSR
090400000719  "  X*
090500000719  "  X* Se l'importo è negativo si rende positivo (per il calcolo) ...
090600000719  "  X*****              If        WImpArr < 0
090700000719  "  X*****              Eval      $ImpNegat = *On
090800000719  "  X*****              Eval      WImpArr = WImpArr * -1
090900000719  "  X*****              Else
091000000719  "  X*****              Eval      $ImpNegat = *Off
091100000719  "  X*****              EndIf
091200000719  "  X*
091300000719  "  X*****              Select
091400000719  "  X*
091500000719  "  X* 1) ITL: alle 1000 lire superiori se ultime tre cifre > 500 lire,
091600000719  "  X*         altrimenti alle 1000 lire inferiori
091700000719  "  X*****              When      CdInter = 'ITL'
091800000719  "  X*****              Add       499           WImpArr
091900000719  "  X*****WImpArr       Div       1000          WImpArr0D
092000000719  "  X*****WImpArr0D     Mult      1000          WImpArr
092100000719  "  X*
092200000719  "  X* 2) EUR: all'unità di euro superiore se i decimali sono >= 50,
092300000719  "  X*         altrimenti all'unità di euro inferiore
092400000719  "  X*****              When      CdInter = 'EUR'
092500000719  "  X*****              Z-Add     WImpArr       W3015
092600000719  "  X*****              Z-Add     0             NrDec
092700000719  "  X*****              CallP     X0201Arrot(W3015: 'H': NrDec)
092800000719  "  X*****              Z-Add     W3015         WImpArr
092900000719  "  X*
093000000719  "  X*****              EndSl
093100000719  "  X*
093200000719  "  X* Se l'importo era negativo ...
093300000719  "  X*****              If        $ImpNegat = *On
093400000719  "  X*****              Eval      WImpArr = WImpArr * -1
093500000719  "  X*****              EndIf
093600000719  "  X*
093700000719C1396X*****              ENDSR
093800950726     C************************************************************
093900950726     C* RIEPILOGHI ALIQUOTE PER REGISTRO
094000950726     C************************************************************
094100950726     C     ALQREG        BEGSR
094200001004C1425C*
094300001004C1425C                   Clear                   SAVLibro
094400950725     C*
094500950725     C* Testata registro
094600950725     C     *in39         ifeq      *on
094700030623C1668C                   exsr      StampaTesta
094800030623c1668x** 96              WRITE     TESTA0
094900950725     C                   WRITE     TESTA
095000950725     C                   setoff                                       39
095100950725     C                   endif
095200950725     C                   WRITE     TESTA1
095300950726     C*
095400950726     C* Se registro acquisti stampo anche indetraibili
095500950726     C     rivtpreg      ifeq      '1'
095600950726     C                   setoff                                       90
095700950726     C                   else
095800950726     C                   seton                                        90
095900950726     C                   endif
096000950726     C*
096100950726     C* Posizionamento per aliquota > 0
096200950726     C                   z-add     0             rivaliq
096300020430C1550X**** k5riv4        setgt     ndriv04l
096400020430C1550C                   move      TpLiq167      RIVTpLiq
096500020430C1550C     k6riv4        setgt     ndriv04l
096600950726     C*
096700020430C1550X**** k4riv4        reade     ndriv04l                               21
096800020430C1550C     k5riv4        reade     ndriv04l                               21
096900950726     C*
097000950726     C                   z-add     rivaliq       savaliq
097100950726     C*
097200950726     C     *in21         doweq     *off
097300001004C1425C*
097400001004  "  C* Scarto i libri sospesi/differiti
097500001004  "  C                   If        SAVLibro <> RIVLibro
097600001004  "  C                   Eval      DLISocieta = RIVSoc
097700001004  "  C                   Eval      DLITpReg = RIVTpReg
097800001004  "  C                   Eval      DLILibro = RIVLibro
097900001004  "  C     k3dli1        Chain     ANDLI01L                           22
098000090120C1425C   22              Clear                   DLISospens
098100090120C2106C                   If         GesPlur045 = *on
098200090120  "  C                   move      *blank        wokrec            1
098300090120  "  C     k4ail1        Chain     ANAIL01L                           34
098400090120  "  C                   If        (*IN34 = *OFF)
098500090120  "  C                   eval      wokrec = 'Y'
098600090120  "  C                   EndIf
098700090120C2106C                   EndIf
098800090120C1425C                   Eval      SAVLibro = RIVLibro
098900090120C1425C                   EndIf
099000090120C2106C                   If        (GesPlur045 = *on and wokrec = 'Y'
099100090120C2106C                              or GesPlur045 = *off)
099200001004C1425C                   If        DLISospens <> '1' and DLISospens <> '4'
099201180105R275 C                             and DLIsospens <> '9'
099300950726     C*
099400950726     C     *in60         ifeq      *off
099500950726     C                   seton                                        60
099600950726     C                   WRITE     TEST1
099700950726     C                   endif
099800950726     C*
099900950726     C* Stampo totali aliquota
100000950726     C     rivaliq       ifne      savaliq
100100950726     C                   EXSR      STAALQ
100200950726     C                   z-add     rivaliq       savaliq
100300950726     C                   endif
100400950726     C*
100500950726     C* Incremento totali aliquota
100600950727     C                   add       rivimponib    p1imponnor
100700950727     C                   add       rivimporto    p1imposnor
100800950726     C     rivtpreg      ifeq      '1'
100900950727     C                   add       rivimpind     p1imponind
101000950727     C                   add       rivimpoind    p1imposind
101100950726     C                   endif
101200001004C1425C*
101300001004C1425C                   EndIf
101400090120C2106C*
101500090120C2106C                   EndIf
101600950726     C*
101700020430C1550X**** k4riv4        reade     ndriv04l                               21
101800020430C1550C     k5riv4        reade     ndriv04l                               21
101900950726     C*
102000950726     C                   enddo
102100950726     C*
102200950726     C     *in60         ifeq      *on
102300950726     C*
102400950726     C* Stampo totali ultima aliquota
102500950726     C                   EXSR      STAALQ
102600950726     C*
102700950726     C* Stampa totali generali aliquote del registro
102800950726     C     *in39         ifeq      *on
102900030623C1668C                   exsr      StampaTesta
103000030623c1668x** 96              WRITE     TESTA0
103100950726     C                   WRITE     TESTA
103200950726     C                   WRITE     TESTA1
103300950726     C                   setoff                                       39
103400950726     C                   endif
103500950726     C                   WRITE     TOTT1
103600950726     C*
103700950726     C                   setoff                                       60
103800950726     C*
103900950726     C                   endif
104000950726     C*
104100950725     C                   ENDSR
104200950725     C/EJECT
104300950726     C************************************************************
104400950726     C* RIEPILOGO ALTRE OPERAZIONI PER REGISTRO
104500950726     C************************************************************
104600950726     C     AOPREG        BEGSR
104700001004C1425C*
104800001004C1425C                   Clear                   SAVLibro
104900950726     C*
105000950726     C* Posizionamento per aliquota = 0
105100950726     C                   z-add     0             rivaliq
105200020430C1550X**** k5riv4        setll     ndriv04l
105300020430C1550C                   move      TpLiq167      RIVTpLiq
105400020430C1550C     k6riv4        setll     ndriv04l
105500950726     C*
105600020430C1550X**** k5riv4        reade     ndriv04l                               21
105700020430C1550C     k6riv4        reade     ndriv04l                               21
105800950726     C*
105900950726     C                   move      rivrifiva     savrifiva
106000950726     C*
106100950726     C     *in21         doweq     *off
106200001004C1425C*
106300001004  "  C* Scarto i libri sospesi/differiti
106400001004  "  C                   If        SAVLibro <> RIVLibro
106500001004  "  C                   Eval      DLISocieta = RIVSoc
106600001004  "  C                   Eval      DLITpReg = RIVTpReg
106700001004  "  C                   Eval      DLILibro = RIVLibro
106800001004  "  C     k3dli1        Chain     ANDLI01L                           22
106900090120C1425C   22              Clear                   DLISospens
107000090120C2106C                   If         GesPlur045 = *on
107100090120  "  C                   move      *blank        wokrec            1
107200090120  "  C     k4ail1        Chain     ANAIL01L                           34
107300090120  "  C                   If        (*IN34 = *OFF)
107400090120  "  C                   eval      wokrec = 'Y'
107500090120  "  C                   EndIf
107600090120C2106C                   EndIf
107700090120C1425C                   Eval      SAVLibro = RIVLibro
107800090120C1425C                   EndIf
107900090120C2106C                   If        (GesPlur045 = *on and wokrec = 'Y'
108000090120C2106C                              or GesPlur045 = *off)
108100001004C1425C                   If        DLISospens <> '1' and DLISospens <> '4'
108101180105R275 C                             and DLIsospens <> '9'
108200950726     C*
108300950726     C     *in70         ifeq      *off
108400950726     C                   seton                                        70
108500950726     C                   WRITE     TEST2
108600950726     C                   endif
108700950726     C*
108800950726     C* Stampo totale altre operazioni
108900950726     C     rivrifiva     ifne      savrifiva
109000950726     C                   EXSR      STAAOP
109100950726     C                   move      rivrifiva     savrifiva
109200950726     C                   endif
109300950726     C*
109400950726     C* Incremento totale altre operazioni
109500950726     C                   add       rivimponib    p2imponib
109600001004C1425C*
109700001004C1425C                   EndIf
109800090120C2106C*
109900090120C2106C                   EndIf
110000950726     C*
110100020430C1550X**** k5riv4        reade     ndriv04l                               21
110200020430     C     k6riv4        reade     ndriv04l                               21
110300950726     C*
110400950726     C                   enddo
110500950726     C*
110600950726     C     *in70         ifeq      *on
110700950726     C*
110800950726     C* Stampo ultimo totale altre operazioni
110900950726     C                   EXSR      STAAOP
111000950726     C*
111100950726     C* Stampa totale generale altre operazioni
111200950726     C     *in39         ifeq      *on
111300030623C1668C                   exsr      StampaTesta
111400030623c1668x** 96              WRITE     TESTA0
111500950726     C                   WRITE     TESTA
111600950726     C                   WRITE     TESTA1
111700950726     C                   setoff                                       39
111800950726     C                   endif
111900950726     C                   WRITE     TOTT2
112000950726     C*
112100950726     C                   setoff                                       70
112200950726     C*
112300950726     C                   endif
112400950726     C*
112500950726     C                   ENDSR
112600950726     C/EJECT
112700950726     C************************************************************
112800950726     C* TOTALI REGISTRO
112900950726     C************************************************************
113000950726     C     TOTREG        BEGSR
113100950726     C*
113200950726     C                   eval      t4totimpon=t1totimpon+t2totimpon
113300950726     C                   eval      t4totimpos=t1totimpos
113400951215     C                   eval      t4totgen=t4totimpon+t4totimpos
113500951215     C                   eval      t4imponnor=t1imponnor
113600951215     C                   eval      t4imposnor=t1imposnor
113700951215     C                   eval      t4imponind=t1imponind
113800951215     C                   eval      t4imposind=t1imposind
113900950726     C*
114000950726     C     *in39         ifeq      *on
114100030623C1668C                   exsr      StampaTesta
114200030623c1668x** 96              WRITE     TESTA0
114300950726     C                   WRITE     TESTA
114400950726     C                   WRITE     TESTA1
114500950726     C                   setoff                                       39
114600950726     C                   endif
114700950726     C                   WRITE     TOTT4
114800950726     C*
114900950726     C* Inizializzazioni
115000950726     C                   z-add     0             t1totimpon
115100950726     C                   z-add     0             t1totimpos
115200950726     C                   z-add     0             t1totgen
115300970226     C                   z-add     0             t1imponnor
115400970226     C                   z-add     0             t1imposnor
115500970226     C                   z-add     0             t1imponind
115600970226     C                   z-add     0             t1imposind
115700950726     C                   z-add     0             t2totimpon
115800950726     C*
115900950726     C                   ENDSR
116000950726     C/EJECT
116100950726     C************************************************************
116200950727     C* LIQUIDAZIONE IVA
116300950726     C************************************************************
116400950727     C     LIQIVA        BEGSR
116500950726     C*
116600950726     C* Testata
116700950727     C                   seton                                        8587
116800030623C1668C                   exsr      StampaTesta
116900030623c1668x** 96              WRITE     TESTA0
117000950726     C                   WRITE     TESTA
117100991112     C                   setoff                                       39
117200950726     C*
117300950726     C                   z-add     spr167        p5salperpr
117400950726     C     spr167        ifgt      0
117500961009     C                   move      XscAvere      p5daperpr
117600950726     C                   else
117700961009     C                   move      XscDare       p5daperpr
117800950726     C                   endif
117900000721C1396C                   If        XScIvaRgm = '1'
118000000721  "  X*****              z-add     ipr167        p5salintpr
118100000721  "  C                   z-add     slp167        p5sliperpr
118200000721  "  X*****ipr167        ifgt      0
118300000721  "  C     slp167        ifgt      0
118400000721  "  X*****              move      XscAvere      p5daintpr
118500000721C1396C                   move      XscAvere      p5daslipr
118600950726     C                   else
118700000721C1396X*****              move      XscDare       p5daintpr
118800000721C1396C                   move      XscDare       p5daslipr
118900950726     C                   endif
119000000721C1396C                   EndIf
119100000721  "  X*****              z-add     spe167        p5salperio
119200000719  "  X*****spe167        ifgt      0
119300000719  "  X*****              move      XscAvere      p5daperio
119400000719  "  X*****              else
119500000719  "  X*****              move      XscDare       p5daperio
119600000721  "  X*****              endif
119700000721  "  C                   If        XScIvaRgm = '1'
119800000721  "  X*****              z-add     ipe167        p5salintra
119900000721  "  C                   z-add     sls167        p5slipersu
120000000721  "  X*****ipe167        ifgt      0
120100000721  "  C     sls167        ifgt      0
120200000721  "  X*****              move      XscAvere      p5daintra
120300000721C1396C                   move      XscAvere      p5daslisu
120400950726     C                   else
120500000721C1396X*****              move      XscDare       p5daintra
120600000721C1396C                   move      XscDare       p5daslisu
120700950726     C                   endif
120800000721C1396C                   EndIf
120900000721  "  X*****              z-add     snu167        p5salnuovo
121000000719  "  X*****snu167        ifgt      0
121100000719  "  X*****              move      XscAvere      p5danuovo
121200000719  "  X*****              else
121300000719  "  X*****              move      XscDare       p5danuovo
121400000719  "  X*****              endif
121500000719  "  X*****p5danuovo     ifeq      XscDare
121600000719  "  X*****              z-add     0             p5impdaver
121700000719  "  X*****              else
121800000719  "  X*****              z-add     p5salnuovo    p5impdaver
121900000719  "  X*****              endif
122000000719  "  C                   z-add     ivadeb167     p5ivadeb
122100000719  "  C     ivadeb167     ifgt      0
122200000719  "  C                   move      XscAvere      p5daivadeb
122300000719  "  C                   else
122400000719  "  C                   move      XscDare       p5daivadeb
122500000719  "  C                   endif
122600000719  "  C                   z-add     ivacre167     p5ivacred
122700000719  "  C     ivacre167     ifgt      0
122800000719  "  C                   move      XscAvere      p5daivacre
122900000719  "  C                   else
123000000719  "  C                   move      XscDare       p5daivacre
123100000719C1396C                   endif
123200950726     C*
123300020927C1583 *
123400020927  "   * Se non richiesto stampa prospetto liquidazione
123500020927  "   * non viene stampato da nessuna parte il nuovo saldo
123600020927  "   * e l'importo da versare quindi li aggiungo qui:
123700020927  "   *
123800020927  "   * Nuovo Saldo: saldo periodo prec. +/- saldo periodo attuale
123900020927  "   *                                 (iva a debito - iva a credito);
124000020927  "  C                   eval      SalNuovo = SPr167 +                          inizializzione S5
124100020927  "  C                                        (IvaDeb167 + IvaCre167)           inizializzione S5
124200020927  "  C                   z-add     SalNuovo      P5SalNuovo                     inizializzione S5
124300020927  "   * Segno Nuovo Saldo
124400020927  "  C                   if        SalNuovo > 0                                 inizializzione S5
124500020927  "  C                   move      XScAvere      P5DANuovo
124600020927  "  C                   else
124700020927  "  C                   move      XScDare       P5DANuovo
124800020927  "  C                   endif                                                  inizializzione S5
124900020927  "   * Imposta da versare
125000020927  "  C                   if        P5DANuovo = XscDare
125100020927  "  C                   z-add     0             P5ImpDaVer
125200020927  "  C                   else
125300020927  "  C                   z-add     P5SalNuovo    P5ImpDaVer
125400020927  "  C                   endif
125500020927  "   *
125600020927C1583 *
125700090626C2131C*
125800090626  "  C* se gestito il PRORATA, mostro i dati del prorata
125900090626  "  C                   if        $ProRata    = *On
126000090626  "  C                   Eval      *In18       = *On
126100090626  "  C                   EXSR      RepProRata
126200090626  "  C                   Eval      P5ProRata   = Perc046
126300090626  "  C* calcolo IVA detraibile per effetto del prorata
126400090626  "  C                   EXSR      CalcProRata
126500090626C2131C                   endif
126600031202C1703 * Dicitura iva di gruppo
126700031202  "  C                   exsr      DicituraIvaDiG
126800031202C1703 *
126900950727     C                   WRITE     LIQUP5
127000950726     C*
127100040727C1781 * Testo se richiesta la stampa
127200040727  "  c                   if        TpTesto167 <> *blank
127300040727  "  C                   exsr      StampaTesto
127400040727  "  C                   endif
127500040727C1781 *
127600950727     C                   setoff                                       8587
127700950727     C*
127800950726     C                   ENDSR
127900950726     C/EJECT
128000090626C2131C************************************************************
128100090626  "  C* Reperisco la percentuale di detraibilità del prorata
128200090626  "  C************************************************************
128300090626C2131C     RepProRata    BEGSR
128400090626     C*
128500090626     C                   Clear                   XAtbDS
128600090626     C                   Clear                   ANG046DS
128700090626     C*
128800090626     C                   Eval      XTbRic      = '1'
128900090626     C                   Eval      XTbAzi      = XScSoc
129000090626     C                   Eval      XTbKey      = *Zero
129100090626     C                   Eval      XTbCod      = '046'
129200090626     C                   Move      Ann167        WAnno
129300090626     C                   if        WAnno       > 0
129400090626     C                   Move      WAnno         XTbKey
129500090626     C                   endif
129600090626     C*
129700090626     C                   CALLB     'XATB'
129800090626     C                   PARM                    XAtbDS
129900090626     C*
130000090626     C                   if        XTbErr      = '0'
130100090626     C                   MoveL     XtbUni        ANG046DS
130200090626     C                   endif
130300090626     C*
130400090626C2131C                   ENDSR
130500090626C2131C/EJECT
130600090626C2131C************************************************************
130700090626  "  C* Calcolo IVA detraibile per effetto del prorata
130800090626  "  C************************************************************
130900090626C2131C     CalcProRata   BEGSR
131000090626     C*
131100090626     C                   Z-Add     IVACre167     WImpInp
131200090626     C*
131300090626     C                   CallP     X0201MultC(WImpInp: P5ProRata: 100:
131400090626     C                                        'H': '0': NrDecMC: WImpOut)
131500090626     C*
131600090626     C                   Z-Add     WImpOut       P5IVACred2
131700090626     C*
131800090626     C                   if        WImpOut     > 0
131900090626     C                   Move      XScAvere      P5DAIVACr2
132000090626     C                   else
132100090626     C                   Move      XScDare       P5DAIVACr2
132200090626     C                   endif
132300090626     C*
132400090626     C                   Eval      P5SalNuov2  = SalNuovo - IVACre167
132500090626     C                                           + WImpOut
132600090626     C* Segno Nuovo Saldo2
132700090626     C                   if        (SalNuovo - IVACre167 + WImpOut)             inizializzione S5
132800090626     C                               > 0                                        inizializzione S5
132900090626     C                   Move      XScAvere      P5DANuovo2
133000090626     C                   else
133100090626     C                   Move      XScDare       P5DANuovo2
133200090626     C                   endif                                                  inizializzione S5
133300090626     C*
133400090626     C* Imposta da versare
133500090626     C                   if        P5DANuovo2  = XscDare
133600090626     C                   z-add     0             P5ImpDaVe2
133700090626     C                   else
133800090626     C                   z-add     P5SalNuov2    P5ImpDaVe2
133900090626     C                   endif
134000090626     C*
134100090626C2131C                   ENDSR
134200090626C2131C/EJECT
134300031202C1703 ************************************************************
134400031202  "   * Dicitura iva di gruppo
134500031202  "   ************************************************************
134600031202  "  C     DicituraIvaDiGBegsr
134700031202  "   *
134800060801C1703C                   eval      *in15 = *off
134900060801C1955C                   eval      *in17 = *off
135000060801C1703C                   clear                   P5SocCtrD
135100031202  "  C                   clear                   P5SocCtrP
135200031202  "   *
135300031202  "   * Parametro modulo IVALIQ
135400031202  "  C                   clear                   NDC149DS
135500031202  "  C                   callb     'XPAR'
135600031202  "  C                   parm                    XScSoc
135700031202  "  C                   parm      'IVALIQ'      XPaPar            8
135800031202  "  C                   parm                    XPaOut
135900031202  "  C                   parm      *off          XPaErr            1
136000031202  "  C                   parm      ' '           XPaRic            1
136100031202  "   *
136200031202  "   *  C'è parametro modulo IVALIQ
136300031202  "  C                   if        XPaErr <> *on
136400031202  "  C                   movel     XPaOut        NDC149DS
136500060801  "   *
136600060801C1703 * se società aderente al gruppo, visualizzao la dicitura:
136700060801C1955 * se soc non è la controllante:
136800060801C1703 * Il saldo è trasferito alla società controllante'
136900060801C1955 * se soc è la controllante:
137000060801C1955 * 'Il saldo è trasferito al consolidato Iva di gruppo'
137100060801C1703C                   if        SocDelG149 = *on
137200060801  "  C                   eval      *in15 = *on
137300060801C1703C                   if        SocCtr149 = *on
137400060801C1955X*ex C1703 ******** eval      P5SocCtrD  = XScDSI
137500060801C1955X*ex C1703 ******** eval      P5SocCtrP  = XScIVA
137600060801C1955C                   eval      *in17 = *on
137700060801C1703C                   else
137800060801  "  C                   eval      P5SocCtrD  = SocCtrD149
137900031202  "  C                   eval      P5SocCtrP  = SocCtrP149
138000031202  "  C                   endif
138100031202  "  C                   endif
138200031202  "   *
138300031202  "  C                   endif
138400031202  "   *
138500031202C1703C     XDicituraIvaDiEndsr
138600040726C1781 ************************************************************
138700040727  "   * Stampa Testo
138800040726  "   ************************************************************
138900040727  "  C     StampaTesto   Begsr
139000040726  "   *
139100040726  "  C                   clear                   PTesto
139200040726  "   *
139300040726  "   * 3 righe vuote
139400040727  "  C                   write     RigaTesto
139500040727  "  C                   write     RigaTesto
139600040727  "  C                   write     RigaTesto
139700040726  "   *
139800040727  "   * righe testo
139900040727  "  C                   eval      XEsito = *Off
140000040727  "   *   setll + reade ..
140100040727  "  C                   eval      XOpe = '3'
140200040727  "   *   .. con kiave XScSoc/XScDUn/TpTesto167/XScLin
140300040727  "  C                   eval      XSocieta = XScSoc
140400040727  "  C                   eval      XTpTesto = TpTesto167
140500040726  "  C                   eval      XUnita = XScDUn
140600040727  "  C                   eval      XLingua  = XScLin
140700040727  "  C                   eval      XClaUte = *blanks
140800040727  "  C                   eval      XPosRig = *blank
140900040727  "  C                   eval      XNrRiga = *zeros
141000040727  "  C                   eval      XRibat = *blanks
141100040727  "  C                   eval      XTesRig = *blanks
141200040727  "  C                   eval      XNrChiavi = 4
141300040727  "  C                   eval      XUsoUnita = '1'
141400040727  "   *
141500040727  "  C                   call      'X72TXT'
141600040727  "  C                   parm                    XEsito
141700040727  "  C                   parm                    XOpe
141800040727  "  C                   parm                    XSocieta
141900040727  "  C                   parm                    XTpTesto
142000040727  "  C                   parm                    XUnita
142100040727  "  C                   parm                    XLingua
142200040727  "  C                   parm                    XClaUte
142300040727  "  C                   parm                    XPosRig
142400040727  "  C                   parm                    XNrRiga
142500040727  "  C                   parm                    XRibat
142600040727  "  C                   parm                    XTesRig
142700040727  "  C                   parm                    XNrChiavi
142800040727  "  C                   parm                    XUsoUnita
142900040727  "   *
143000040727  "  C                   dow       XEsito = *Off
143100040727  "   * Reade su ANTXT01L per trovare i record successivi
143200040727  "  C                   eval      XEsito = *Off
143300040727  "  C                   eval      XOpe = '4'
143400040727  "  C                   eval      XClaUte = *blanks
143500040727  "  C                   eval      XNrRiga = *zeros
143600040727  "  C                   eval      XRibat = *blanks
143700040727  "  C                   eval      XTesRig = *blanks
143800040727  "  C                   eval      XNrChiavi = 4
143900040727  "  C                   eval      XUsoUnita = '1'
144000040727  "   *
144100040727  "  C                   call      'X72TXT'
144200040727  "  C                   parm                    XEsito
144300040727  "  C                   parm                    XOpe
144400040727  "  C                   parm                    XSocieta
144500040727  "  C                   parm                    XTpTesto
144600040727  "  C                   parm                    XUnita
144700040727  "  C                   parm                    XLingua
144800040727  "  C                   parm                    XClaUte
144900040727  "  C                   parm                    XPosRig
145000040727  "  C                   parm                    XNrRiga
145100040727  "  C                   parm                    XRibat
145200040727  "  C                   parm                    XTesRig
145300040727  "  C                   parm                    XNrChiavi
145400040727  "  C                   parm                    XUsoUnita
145500040727  "   *
145600040727  "  C                   if        XEsito = *Off
145700040727  "  c                   movel     XTesRig       PTesto
145800040727  "  C                   eval      *in26 = *off
145900040727  "   * riga in alta intensità
146000040727  "  C                   if        XRibat = *on
146100040727  "  C                   eval      *in26 = *on
146200040727  "  C                   endif
146300040727  "   *
146400040727  "  C                   write     RigaTesto
146500040727  "   *
146600040727  "   * overflow
146700040727  "  C                   if        *in39 = *on
146800040727  "  C                   exsr      StampaTesta
146900040727  "  C                   WRITE     TESTA
147000040727  "  C                   setoff                                       39
147100040727  "  C                   endif
147200040727  "   *
147300040727  "  C                   endif
147400040727  "   *
147500040727  "  C                   enddo
147600040726  "   *
147700040727C1781C     XStampaTesto  Endsr
147800000719C1396C************************************************************
147900000719  "  C* Prospetto liquidazione IVA
148000000719  "  C************************************************************
148100000719C1396C     ProsLiqIVA    BEGSR
148200000719     C*
148300030626c1668c                   z-add     NumeroPag     NumPag167
148400000719     C                   MoveL     C167DS        KPJBU
148500000719     C*
148600000719     C                   CallB     'NDC167R1'
148700000719     C                   Parm                    KPJBA
148800000719     C                   Parm                    SOC001
148900000719     C*
149000000719     C                   MoveL     KPJBU         C167DS
149100030626c1668c                   z-add     NumPag167     NumeroPag
149200000719     C*
149300000719C1396C                   ENDSR
149400031128C1703 ************************************************************
149500031128  "   * Iva di gruppo
149600031128  "   ************************************************************
149700031128  "  C     IvaDiGruppo   BEGSR
149800031128  "   *
149900031128  "   *
150000031128  "  C                   movel     C167DS        KPJBU
150100031128  "   *
150200031128  "  C                   callb     'NDC167R2'
150300031128  "  C                   parm                    KPJBA
150400031128  "  C                   parm                    SOC001
150500031128  "   *
150600031128C1703C     XIvaDiGruppo  Endsr
150700951113     C************************************************************
150800951113     C* RIEPILOGO PLAFOND
150900951113     C************************************************************
151000951113     C     RIEPLF        BEGSR
151100970320     C* Testata
151200951114     C                   seton                                        85
151300030623C1668C                   exsr      StampaTesta
151400030623c1668x** 96              WRITE     TESTA0
151500970320     C                   WRITE     TESTA
151600991112     C                   setoff                                       39
151700970320     C*
151800970227     C                   if        XscPlafond = '1'
151900970227     C                   seton                                        95
152000970227     C                   endif
152100970320     C* Esportazioni (fisso e mobile)
152200951113     C                   movel     tmese         p7mese1
152300970320     C                   move      tanno         p7anno1
152400970320     C                   z-add     espeffp167    p7espeff1
152500970320     C                   WRITE     PLAF1P7
152600970320     C* Mobile ...
152700970320     C                   if        XscPlafond = '2'
152800970320     C* Acq. eff. 12 mesi precedenti
152900970320     C                   z-add     acqeffp167    p7acqeff2
153000970320     C                   WRITE     PLAF2P7
153100970320     C* Fisso ...
153200970320     C                   else
153300970320     C* Dett. acq. mesi prec. anno corrente (se mese corrente non è gennaio)
153400000720C1396X*****              if        mes167 <> '01'
153500000720  "  C                   if        per167 <> '01'
153600000720  "  X*****              move      mes167        amcomp
153700000720C1396C                   move      per167        amcomp
153800970320     C                   movel     ann167        amcomp
153900970320     C                   move      soc167        plfsocieta
154000970320     C                   move      '01'          plfamcomp
154100970320     C                   movel     ann167        plfamcomp
154200970320     C     k2plf1        setll     ndplf01l
154300970320     C     soc167        reade(N)  ndplf01l                               21
154400970320     C     *in21         doweq     *off
154500970320     C     plfamcomp     andlt     amcomp
154600970320     C                   move      plfamcomp     mese              2
154700970320     C                   RESET                   X07DS
154800970320     C                   MOVE      '1'           X07RIC
154900970320     C                   MOVEL     'ANDIZ000'    X07TRC
155000970320     C                   MOVEL     'DSPMESE'     X07CAM
155100970320     C                   MOVEL     mese          X07VAL
155200970320     C                   MOVE      *OFF          X07ALL
155300970320     C                   CALLb     'X07VALR'
155400970320     C                   PARM                    X07DS
155500970320   B3C     X07ERR        IFEQ      '0'
155600970320     C                   MOVEL     X07DES        P7mese3
155700970320   X3C                   ELSE
155800000720C1396X*****              MOVE      mes167        P7mese3
155900000720C1396C                   MOVE      per167        P7mese3
156000970320   E3C                   ENDIF
156100970320     C                   move      ann167        P7anno3
156200970320     C                   z-add     plfimpeff     p7acqeffm3
156300970320     C                   WRITE     PLAF3P7
156400970320     C     soc167        reade(N)  ndplf01l                               21
156500970320     C                   enddo
156600970320     C                   endif
156700970320     C* Plafond disponibile all'inizio del mese corrente
156800970320     C                   movel     tmese         p7mese4
156900970320     C                   move      tanno         p7anno4
157000970320     C                   eval      p7plafdis4 = espeffp167 - acqeffp167
157100970320     C                   WRITE     PLAF4P7
157200970320     C                   endif
157300970320     C* Acq. eff. e Plafond disponibile mese corrente (fisso e mobile)
157400970320     C                   movel     tmese         p7mese5
157500970320     C                   move      tanno         p7anno5
157600970320     C                   z-add     acqeff167     p7acqeff5
157700970320     C                   z-add     plafond167    p7plafond5
157800970320     C                   WRITE     PLAF5P7
157900951113     C*
158000951113     C* Agg. NDPLF e NDMIV con data odierna se richiesta stampa definitiva
158100951113     C     sdp167        ifeq      *on
158200951113     C                   move      soc167        plfsocieta
158300000720C1396X*****              move      mes167        plfamcomp
158400000720C1396C                   move      per167        plfamcomp
158500951113     C                   movel     ann167        plfamcomp
158600951113     C     k2plf1        chain     ndplf01l                           21
158700951113     C     *in21         ifeq      *off
158800951113     C     *eur          move      *date         plfdtstamp
158900951113     C                   UPDATE    ndplf000
159000951113     C                   endif
159100951113     C                   move      soc167        mivsocieta
159200000720C1396X*****              move      mes167        mivamcomp
159300000720C1396C                   move      per167        mivamcomp
159400951113     C                   movel     ann167        mivamcomp
159500951113     C     k2miv3        setll     ndmiv03l
159600951113     C     k2miv3        reade     ndmiv03l                               21
159700951113     C     *in21         doweq     *off
159800951113     C     *eur          move      *date         mivdtstamp
159900951113     C                   UPDATE    ndmiv000
160000951113     C     k2miv3        reade     ndmiv03l                               21
160100951113     C                   enddo
160200951113     C                   endif
160300951114     C*
160400951114     C                   setoff                                       85
160500951113     C*
160600951113     C                   ENDSR
160700951113     C/EJECT
160800950725     C************************************************************
160900950725     C* LEGENDA DEI LIBRI IVA
161000950725     C************************************************************
161100950725     C     LEGIVA        BEGSR
161200950725     C*
161300950725     C* Testata legenda
161400950725     C                   seton                                        85
161500030623C1668C                   exsr      StampaTesta
161600030623c1668x** 96              WRITE     TESTA0
161700950725     C                   WRITE     TESTA
161800991112     C                   setoff                                       39
161900950725     C                   WRITE     TESTAL
162000021014C1590 * solo se richiesto nel lancio stampa intestazione:
162100021014  "   *               -----------------Protocollo-----------------
162200021014  "   *                        Da             A          Nr totale
162300021014  "  C                   if        StpTotR167 = *on
162400021014  "  C                   write     TestaL0
162500021014C1590C                   endif
162600950725     C*
162700950725     C* Registro vendite
162800950725     C                   seton                                        81
162900950725     C                   setoff                                       83
163000950725     C                   WRITE     TESTL1
163100950725     C                   move      soc167        dlisocieta
163200950725     C                   move      '2'           dlitpreg
163300950725     C                   EXSR      STPLEG
163400950725     C*
163500950725     C* Registro corrispettivi
163600950725     C                   seton                                        82
163700950725     C                   setoff                                       81
163800030721D1404C* Se gestiti
163900030721D1404c                   if        GestCorr=*on
164000950725     C                   WRITE     TESTL1
164100950725     C                   move      soc167        dlisocieta
164200950725     C                   move      '3'           dlitpreg
164300950725     C                   EXSR      STPLEG
164400030721D1404C                   endif
164500950725     C*
164600950725     C* Registro acquisti
164700950725     C                   seton                                        83
164800950725     C                   setoff                                       82
164900950725     C                   WRITE     TESTL1
165000950725     C                   move      soc167        dlisocieta
165100950725     C                   move      '1'           dlitpreg
165200950725     C                   EXSR      STPLEG
165300950725     C*
165400950725     C                   ENDSR
165500950725     C/EJECT
165600950725     C************************************************************
165700950725     C* STAMPA LEGENDA DEI LIBRI IVA
165800950725     C************************************************************
165900950725     C     STPLEG        BEGSR
166000090120C2106C*
166100090120  "  C                   If         GesPlur045 = *on
166200090120  "  C* Lettura ANAIL01L per soc./cod.att./tipo reg.
166300090120  "  C     K3AIL1        SETLL     ANAIL01L                           34
166400090120  "  C     *IN34         DOWEQ     *OFF
166500090120  "  C     K3AIL1        READE     ANAIL01L                               34
166600090120  "  C                   IF        (*IN34 = *OFF)
166700090120  "  C                   move      aillibro      dlilibro
166800090120  "  C     K3DLI1        chain     andli01l                           21
166900090120  "  C                   if        (*in21 = *off)
167000090120  "  C                   EXSR      STPLEG2
167100090120  "  C                   EndIf
167200090120  "  C                   EndIf
167300090120  "  C                   Enddo
167400090120C2106C                   else
167500950725     C*
167600950725     C     k2dli1        setll     andli01l
167700950725     C     k2dli1        reade     andli01l                               21
167800950725     C*
167900950725   B1C     *in21         doweq     *off
168000950725     C*
168100090120C2106C                   EXSR      STPLEG2
168200090120C2106C* ---> Specifiche spostate nella routine STPLEG2
168300090120  "  X***                move      dlilibro      p6libro
168400090120C2106X***                movel     dlidescr      p6deslibro
168500001212C1451C* saldo imposta del libro
168600090120  "  X***                Clear                   P6TotImpD
168700090120  "  X***                Clear                   P6TotImpA
168800090120  "  X***                Eval      i = 1
168900090120  "  X***  $DliRegLib    LookUp    SkRegLib(i)                            24
169000090120  "  X***                If        *In24 = *On
169100001213  "  C* - iva acquisti normalmente in dare (credito)
169200001213  "  C* - iva vendite/corrispetivi normalmente in avere (debito)
169300090120  "  X***                If        (DLITpReg = '1' and
169400090120  "  X***                           SkImporto(i) > 0) or
169500090120  "  X***                          (DLITpReg <> '1' and
169600090120  "  X***                           SkImporto(i) < 0)
169700090120  "  X***                Z-Add     SkImporto(i)  P6TotImpD
169800090120  "  X***                Else
169900090120  "  X***                Z-Add     SkImporto(i)  P6TotImpA
170000090120  "  X***                EndIf
170100090120C1451X***                EndIf
170200021010C1590 *
170300021010  "   * stampa limite e tot nr registrazione se richiesto nel lancio
170400090120  "  X***                if        StpTotR167 = *on
170500090120  "  X***                exsr      StpTotReg
170600090120C1590X***                endif
170700950725     C*
170800090120     X***  *in39         ifeq      *on
170900090120C1668X***                exsr      StampaTesta
171000090120c1668x** 96              WRITE     TESTA0
171100090120     X***                WRITE     TESTA
171200090120     X***                WRITE     TESTAL
171300021014C1590 * solo se richiesto nel lancio stampa intestazione:
171400021014  "   *               -----------------Protocollo-----------------
171500021014  "   *                        Da             A          Nr totale
171600090120  "  X***                if        StpTotR167 = *on
171700090120  "  X***                write     TestaL0
171800090120C1590X***                endif
171900090120     X***                WRITE     TESTL1
172000090120     X***                setoff                                       39
172100090120     X***                endif
172200090120C2106X***                WRITE     LEGEP6
172300950725     C*
172400950725     C     k2dli1        reade     andli01l                               21
172500950725     C                   enddo
172600090120C2106C                   endif
172700950725     C*
172800950725     C                   ENDSR
172900090120C2106C************************************************************
173000090120  "  C* STAMPA LEGENDA DEI LIBRI IVA
173100090213C2106C************************************************************
173200090213     C     STPLEG2       BEGSR
173300090213     C*
173400090213     C                   move      dlilibro      p6libro
173500090213     C                   movel     dlidescr      p6deslibro
173600090213     C* saldo imposta del libro
173700090213     C                   Clear                   P6TotImpD
173800090213     C                   Clear                   P6TotImpA
173900090213     C                   Eval      i = 1
174000090213     C     $DliRegLib    LookUp    SkRegLib(i)                            24
174100090213     C                   If        *In24 = *On
174200090213     C* - iva acquisti normalmente in dare (credito)
174300090213     C* - iva vendite/corrispetivi normalmente in avere (debito)
174400090213     C                   If        (DLITpReg = '1' and
174500090213     C                              SkImporto(i) > 0) or
174600090213     C                             (DLITpReg <> '1' and
174700090213     C                              SkImporto(i) < 0)
174800090213     C                   Z-Add     SkImporto(i)  P6TotImpD
174900090213     C                   Else
175000090213     C                   Z-Add     SkImporto(i)  P6TotImpA
175100090213     C                   EndIf
175200090213     C                   EndIf
175300090213      *
175400090213      * stampa limite e tot nr registrazione se richiesto nel lancio
175500090213     C                   if        StpTotR167 = *on
175600090213     C                   exsr      StpTotReg
175700090213     C                   endif
175800090213     C*
175900090213     C     *in39         ifeq      *on
176000130906!!!! C                   exsr      StampaTesta
176100090213     C                   WRITE     TESTA
176200090213     C                   WRITE     TESTAL
176300090120C1590 * solo se richiesto nel lancio stampa intestazione:
176400090120  "   *               -----------------Protocollo-----------------
176500090120  "   *                        Da             A          Nr totale
176600090120  "  C                   if        StpTotR167 = *on
176700090120  "  C                   write     TestaL0
176800090120C1590C                   endif
176900090213     C                   WRITE     TESTL1
177000090213     C                   setoff                                       39
177100090213     C                   endif
177200090213     C                   WRITE     LEGEP6
177300090213     C*
177400090120C2106C                   ENDSR
177500021010C1590 ************************************************************
177600021010  "   * stampa limite e tot nr registrazione
177700021010  "   ************************************************************
177800021010  "  C     StpTotReg     Begsr
177900021010  "   *
178000021011  "  C                   z-add     0             P6NrReg1
178100021011  "  C                   z-add     0             P6NrReg2
178200021011  "  C                   z-add     0             P6TotReg
178300021011  "   *
178400021010  "  C                   move      Soc167        ERISoc
178500021010  "  C                   move      TpLiq167      ERITpLiq
178600021010  "  C                   move      Ann167        ERIAnno
178700021010  "  C                   move      Per167        ERIPerLiq
178800021010  "  C                   move      DLITpReg      ERITpReg
178900050601C1590C                   move      DLILibro      ERILibro
179000050601C1825X*ex C1590** K06ERI02      chain     NDERI02L                           23
179100050601  "  X*ex C1590**
179200050601C1825X*ex C1590**        if        *in23 = *off
179300050601C1825 *
179400050601  "  C     K06ERI02      setll     NDERI02L
179500050601  "  C     K06ERI02      reade     NDERI02L                               23
179600050601C1825C                   dow       *in23 = *off
179700050601C1590 *
179800050601C1590 *   Nr protocollo da:
179900050601C1825C                   if        P6NrReg1 = 0
180000050601C1590C                   z-add     ERINrReg1     P6NrReg1
180100050601C1825C                   endif
180200050601C1590 *   Nr protocollo a:
180300050601C1825C                   z-add     0             P6NrReg2
180400050601C1590C                   z-add     ERINrReg2     P6NrReg2
180500050601C1590 *   totali Nr protocollo:
180600050601C1825C                   eval      P6TotReg = P6TotReg + ERITotReg
180700050601  "   *
180800050601  "  C     K06ERI02      reade     NDERI02L                               23
180900050601  "  C                   enddo
181000050601C1825 *
181100050601C1825X*ex C1590**        z-add     ERITotReg     P6TotReg
181200050601C1825X*ex C1590**
181300050601C1825X*ex C1590**        endif
181400050601C1590C
181500021010C1590C     XStpTotReg    Endsr
181600950606     C************************************************************
181700950606     C* FINE PROGRAMMA
181800950606     C************************************************************
181900950606     C     ENDPGM        BEGSR
182000030623     c*
182100030623c1668c* aggiornamento numero di pagina
182200030623c1668c                   exsr      AggTab008
182300950606     C*
182400950724     C                   MOVEL     C167DS        KPJBU
182500950711     C                   SETON                                        LR
182600950606     C                   RETURN
182700950606     C*
182800950606     C                   ENDSR
182900030623     C*
183000030623c1668C************************************************************
183100030623     C* Aggiornamento ultimo numero di pagina
183200030623c1668C************************************************************
183300030623     C     AggTab008     BEGSR
183400030623     c*
183500030623     C* aggiorno con ultimo numero
183600030624     C* di pagina la tabella 008 dove è memorizzata  SOLO
183700030623     c* se chiesto il bollato
183800030623     c                   if        bol167=*on  and
183900030623     c* e se non ci sono stati errori
184000030623     c                             err167<>*on and
184100030623     c* e chiesta intestazione
184200030623     c                             intesta167=*on
184300030623     c* aggancio tabella 008
184400030623     c                   reset                   xatbds
184500030623     C                   MOVE      soc167        XTBAZI
184600030623     C                   MOVE      '008'         XTBcod
184700030623     c* Se numerazione autonoma
184800030623     c                   if        NumRiep045=' '
184900030623     c                   eval      xtbkey = '00000000000' + '4'+ '   '
185000030623     c                   else
185100030623     c* se numerazione in coda
185200030623     c* se numerazione libri iva per registro
185300030623     c                   if        Numeraz045='1'
185400030623     c                   eval      xtbkey = '00000000000' + '2'+ '   '
185500030623     c                   else
185600030623     c* se numerazione libri iva per libro
185700030623     c                   eval      xtbkey = '00000000000' + '2'+ libro167
185800030623     c                   endif
185900030623     c                   endif
186000030623     c*
186100030623     C                   MOVE      '1'           XTBRIC
186200030623     C                   MOVE      '1'           XTBALC
186300030623     C                   CALLB     'XATB'
186400030623     c                   parm                    xatbds
186500030623     C*
186600030623     c                   z-add     NumeroPag     Pagina008
186700030623     c*
186800030623     c                   eval      xtbuni = ang008ds
186900030623     C                   MOVE      '4'           XTBric
187000030623     C                   CALLB     'XATB'
187100030623     c                   parm                    xatbds
187200030623     c                   endif
187300030623     C*
187400030623c1668C                   ENDSR
187500950405     C************************************************************
187600900731     C* Definizioni e inizializzazioni
187700950405     C************************************************************
187800940831     C     *INZSR        BEGSR
187900000719C1396X*
188000000719C1396X******DMY          Move      UDate         Oggi
188100000719     C*
188200900731     C     *ENTRY        PLIST
188300900731     C                   PARM                    KPJBA
188400030623c1668c* chiamo programma generante tabella 008, il quale scrive in
188500030623  "  C* automatico i dati di tale tabella se vuota dal file ANDLI
188600030623  "  C* bisogna chiamarlo sempre, anche se non richiesta la stampa inte
188700030623  "  c* stazione, in quanto deve memorizzare nella tabella 008
188800030623  "  c* l'ultimo numero protocollo iva per libro
188900030623  "  C                   MOVEL     KPJBU         C167DS
189000030623  "  c                   call      'ANG008R1'
189100030623c1668c                   parm                    soc167
189200941104     C*
189300940831     C                   ENDSR
189400991230     C/EJECT
189500950606     C************************************************************
189600950606     C* INIZIALIZZAZIONE VARIABILI
189700950606     C************************************************************
189800950606     C     INZVAR        BEGSR
189900950606     C*
190000950724     C                   MOVEL     KPJBU         C167DS
190100060629D2015C                   if        intesta167=*blanks
190200060629  "  c                   eval      intesta167=*off
190300060629D2015C                   endif
190400961009     C*
190500961009     C* Reperimento dati società
190600961009     C*
190700961009    >C                   MOVEL     'SOC001'      TIPXSC
190800961009     C                   MOVEL     soc167        SOCXSC
190900961009     C                   EXSR      REPSOC
191000961009     C     RTNXSC        IFNE      '1'
191100961009    >C                   MOVEL     XSOCDS        SOC001
191200961009     C                   ELSE
191300961009     C                   EXSR      ENDPGM
191400961009     C                   ENDIF
191500090626C2131C*
191600090626  "  C* reperisco dai dati di società i numeri dei decimali della moneta di
191700090626  "  C* conto
191800090626  "  C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
191900090626C2131C                   Move      WNrDecMC      NrDecMC
192000030623C1668C*
192100030623  "  C* Reperimento parametro IVA (anagrafiche iva)
192200030623  "  C*
192300030623  "  C                   CALLB     'XPAR'
192400030623  "  C                   parm                    xscsoc
192500030623  "  C                   parm      'IVA     '    xpapar            8
192600030623  "  C                   parm                    xpaout
192700030623  "  C                   parm      *OFF          xpaerr            1
192800030623  "  C                   parm      ' '           xparic            1
192900030623  "  C     xpaerr        ifeq      *ON
193000030623  "  C                   MOVE      *ON           ERR167
193100030623  "  C                   EXSR      ENDPGM
193200030623  "  C                   else
193300030623  "  C                   movel     xpaout        ANP045DS
193400030623c1668C                   endif
193500090626C2131C*
193600090626C2131C                   Eval      $ProRata    = *Off
193700090126C2106C*
193800090126  "  C                   EVAL      PDESATT = *BLANK
193900090126  "  C                   If         GesPlur045 = *on
194000090126  "  C* Decodifica codice attività
194100090126  "  C     K2AIV1        CHAIN     ANAIV01L                           35
194200090126  "  C                   IF        (*IN35 = *OFF)
194300090626C2106C                   EVAL      PDESATT = AIVDESCR
194400090626C2131C*
194500090626  "  C* verifico se gestito il PRORATA per mostrare i dati del prorata
194600090626  "  C                   if        AIVProRata  = *On
194700090626  "  C                   Eval      $ProRata    = *On
194800090626  "  C                   endif
194900090626C2131C*
195000090626C2106C                   ENDIF
195100090126C2106C                   ENDIF
195200030623     c*
195300000719C1396X*
195400000719  "  X* Reperisco codice internazionale m.c.
195500000719  "  X*****              Clear                   CdInter
195600000719  "  X*****              If        X0201RpDVS(XScDiv: Oggi:
195700000719  "  X*****                                   ANDVS00F: %Size(ANDVS00F):
195800000719  "  X*****                                   ANDVX00F: %Size(ANDVX00F)) = 0
195900000719  "  X*****              Move      DVSCdInter    CdInter
196000000719C1396X*****              EndIf
196100030721     c*
196200030721D1404C* controllo sul datario se gestiti i corrispettivi
196300030721  "  c                   move      *on           GestCorr          1
196400030721  "  C                   MOVE      '1'           XDTRIC
196500030721  "  C                   MOVE      XSCSOC        XDTSOC
196600030721  "  C                   MOVE      *blanks       XDTUNI
196700030721  "  C                   MOVE      'CG'          XDTCTB
196800030721  "  C                   MOVE      *BLANKS       XDTKEY
196900030721  "  C                   MOVE      'IVACOR'      XDTTIP
197000030721  "  C                   MOVE      *OFF          XDTCMT
197100030721  "  C                   CALLB     'XDATR'       PDAT
197200030721  "  C     XDTERR        IFEQ      *on
197300030721  "  c                   move      *off          GestCorr
197400030721D1404c                   endif
197500991230     C*
197600991230     C* Valorizzazione campi univoci testate
197700991230     C                   MOVEL     KNSIF         NOMSIF
197800991230     C                   MOVEL     XSCDSI        NOMDIT
197900991230     C                   MOVEL     DSPGM         NOMPGM
198000991230     C* Reperisco nome sistema informatico
198100991230     C                   CALL      'XNETA'                              21
198200991230     C                   PARM                    NOMSYS
198300000202     C*
198400000202     C* Forzatura Regime se non impostato in anagrafica società
198500000202     C                   If        XscIvaRgm = ' '
198600000202     C                   Eval      XscIvaRgm = '0'
198700000202     C                   EndIf
198800000202     C*
198900000202     C* Forzatura Tipo Liquidazione se non impostato in anagrafica società
199000000202     C                   If        XscIvaLiq = ' '
199100000202     C                   Eval      XscIvaLiq = '1'
199200000202     C                   EndIf
199300990802     C*
199400000202     C* Regime IVA
199500020430C1550X****************** If        XscIvaRgm = '0'
199600020430C1550 * visualizzo l'iva slittata solo se regime iva trasp. normali
199700020430C1550C                   If        XscIvaRgm <>'1'
199800000202     C* (normale)
199900000202     C                   SetOff                                       14
200000000202     C                   Else
200100000202     C* (trasportatori)
200200000202     C                   SetOn                                        14
200300000202     C                   Endif
200400950724     C*
200500000202     C* Tipo liquidazione IVA
200600031219D1519X****************** If        XscIvaLiq <> '1'
200700031219D1519 *                                                                A)
200800031219  "  C                   select
200900031219D1519 *
201000000202     C* (non mensile)
201100031219D1519 * l'intestazione : "RIEPILOGO MOVIMENTI IVA DEL PERIODO" (TESTA)
201200031219  "   * visualizza nr e decodifica PERIODO (13='1') se:
201300031219  "   * regime iva NON tr. mista e tp liq in anagr società NON mensile
201400031219  "   * oppure Reg iva tr. mista e tp liq scelta nel lancio NON mensile
201500031219  "  C                   when      (XscIvaRgm<>'2' and  XscIvaLiq<>'1')
201600031219  "  C                             or
201700031219D1519C                             (XscIvaRgm ='2' and  TpLiq167<>'1')
201800000202     C                   SetOn                                        13
201900031219D1519X****************** Else
202000031219D1519 *
202100000202     C* (mensile)
202200031219D1519 * l'intestazione : "RIEPILOGO MOVIMENTI IVA DEL PERIODO" (TESTA)
202300031219  "   * visualizzo decodifica MESE (13='0') se:
202400031219  "   * regime iva non trasp. mista e tipo liq in anagr società mensile
202500031219  "   * oppure Regime iva tr. mista e tipo liq scelta nel lancio mensil
202600031219  "  C                   when      (XscIvaRgm<>'2' and  XscIvaLiq= '1')
202700031219  "  C                             or
202800031219D1519C                             (XscIvaRgm ='2' and  TpLiq167= '1')
202900000202     C                   SetOff                                       13
203000031219D1519 *
203100031219  "  X****************** Endif
203200031219D1519C                   endsl
203300020430C1550 *
203400020430  "   * descrizione tipo liquidazione se regime iva trasp. mista
203500020430C1550C                   exsr      RieTpLiqD
203600030623     c*
203700030624c1668c* Se chiesta intestazione
203800030624  "  c                   if        intesta167=*on
203900030623  "   * reperimento numero di pagina iniziale da tabella '008'
204000030624  "  C                   exsr      RepTab008
204100030624c1668c                   endif
204200000202     C*
204300950724     C* klist per ANDLI
204400950724     C     k2dli1        KLIST
204500950724     C                   KFLD                    dlisocieta
204600950724     C                   KFLD                    dlitpreg
204700001004C1425C     k3dli1        KLIST
204800001004  "  C                   KFLD                    dlisocieta
204900001004  "  C                   KFLD                    dlitpreg
205000001004C1425C                   KFLD                    dlilibro
205100950703     C*
205200950726     C* klist per NDRIV
205300020424C1550C     k7riv3        KLIST
205400020424  "  C                   KFLD                    rivsoc
205500020424  "  C                   KFLD                    RIVTpLiq
205600020424  "  C                   KFLD                    rivanno
205700020424  "  C                   KFLD                    rivperliq
205800020424  "  C                   KFLD                    rivtpreg
205900020424  "  C                   KFLD                    rivlibro
206000020424C1550C                   KFLD                    rivaliq
206100980722     C     k6riv3        KLIST
206200980722     C                   KFLD                    rivsoc
206300020424C1550C                   KFLD                    RIVTpLiq
206400980722     C                   KFLD                    rivanno
206500980722     C                   KFLD                    rivperliq
206600980722     C                   KFLD                    rivtpreg
206700980722     C                   KFLD                    rivlibro
206800020424C1550X****************** KFLD                    rivaliq
206900980722     C*
207000020430C1550X**** k5riv3        KLIST
207100020430  "  X****               KFLD                    rivsoc
207200020430  "  X****               KFLD                    rivanno
207300020430  "  X****               KFLD                    rivperliq
207400020430  "  X****               KFLD                    rivtpreg
207500020430C1550X****               KFLD                    rivlibro
207600980722     C*
207700020430C1550X**** k5riv4        KLIST
207800020430C1550C     k6riv4        KLIST
207900980722     C                   KFLD                    rivsoc
208000020430C1550C                   KFLD                    RIVTpLiq
208100020430     C                   KFLD                    rivanno
208200980722     C                   KFLD                    rivperliq
208300980722     C                   KFLD                    rivtpreg
208400020430     C                   KFLD                    rivaliq
208500980722     C*
208600020430C1550X**** k4riv4        KLIST
208700020430C1550C     k5riv4        KLIST
208800980722     C                   KFLD                    rivsoc
208900020430C1550C                   KFLD                    RIVTpLiq
209000980722     C                   KFLD                    rivanno
209100980722     C                   KFLD                    rivperliq
209200020429     C                   KFLD                    rivtpreg
209300021010C1590 * klist per NDERI
209400021010  "  C     K06ERI02      klist
209500021010  "  C                   kfld                    ERISoc
209600021010  "  C                   kfld                    ERITpLiq
209700021010  "  C                   kfld                    ERIAnno
209800021010  "  C                   kfld                    ERIPerLiq
209900021010  "  C                   kfld                    ERITpReg
210000021010C1590C                   kfld                    ERILibro
210100950724     C*
210200950725     C* klist per ANRIF
210300950724     C     k4rif1        KLIST
210400950724     C                   KFLD                    rifsoc
210500950725     C                   KFLD                    riftpr
210600950725     C                   KFLD                    rifalq
210700950725     C                   KFLD                    rifrif
210800950703     C*
210900950724     C     k3rif1        KLIST
211000950724     C                   KFLD                    rifsoc
211100950725     C                   KFLD                    riftpr
211200950725     C                   KFLD                    rifalq
211300950725     C*
211400950725     C* klist per ANOPE
211500950725     C     k2ope1        KLIST
211600950725     C                   KFLD                    opesocieta
211700950725     C                   KFLD                    opecausale
211800951113     C*
211900951113     C* klist per NDPLF
212000951113     C     k2plf1        KLIST
212100951113     C                   KFLD                    plfsocieta
212200951113     C                   KFLD                    plfamcomp
212300951113     C*
212400951113     C* klist per NDMIV
212500951113     C     k2miv3        KLIST
212600951113     C                   KFLD                    mivsocieta
212700951113     C                   KFLD                    mivamcomp
212800090213C2106C*
212900090213  "  C* klist per ANAIV
213000090213  "  C     k2aiv1        KLIST
213100090213  "  C                   KFLD                    SOC167
213200090213  "  C                   KFLD                    CDATT167
213300090213  "  C* klist per ANAIL
213400090213  "  C     k3ail1        KLIST
213500090120  "  C                   KFLD                    dlisocieta
213600090120  "  C                   KFLD                    CDATT167
213700090120  "  C                   KFLD                    dlitpreg
213800090120  "  C     k4ail1        KLIST
213900090120  "  C                   KFLD                    dlisocieta
214000090120  "  C                   KFLD                    CDATT167
214100090120  "  C                   KFLD                    dlitpreg
214200090120C2106C                   KFLD                    dlilibro
214300030721     C*
214400030721D1404C     PDAT          PLIST
214500030721  "  C                   PARM                    XDTRIC            1
214600030721  "  C                   PARM                    XDTSOC            3
214700030721  "  C                   PARM                    XDTUNI            8
214800030721  "  C                   PARM                    XDTCTB            2
214900030721  "  C                   PARM                    XDTKEY            8
215000030721  "  C                   PARM                    XDTALC            1
215100030721  "  C                   PARM                    XDTCMT            1
215200030721  "  C                   PARM                    XDTDAT
215300030721  "  C                   PARM                    XDTTIP            6
215400030721  "  C                   PARM                    XDTERR            1
215500030721D1404C                   PARM                    XDTTST            1
215600030721     C*
215700950724     C*
215800950724     C* Imposto campi testata
215900950724     C                   movel     ann167        tanno
216000950726     C*
216100031219D1519 * specifiche asteriscate e riunite nella routine Inz_IntestazP
216200031219  "  X**Liq. IVA mensile
216300031219  "  X****               If        XscIvaLiq = '1'
216400031219  "  X****               RESET                   X07DS
216500031219  "  X****               MOVE      '1'           X07RIC
216600031219  "  X****               MOVEL     'ANDIZ000'    X07TRC
216700031219  "  X****               MOVEL     'DSPMESE'     X07CAM
216800031219  "  X*ex C1396X****              MOVEL     mes167        X07VAL
216900031219  "  X*ex C1396C         MOVEL     per167        X07VAL
217000031219  "  X****               MOVE      *OFF          X07ALL
217100031219  "  X****               CALLb     'X07VALR'
217200031219  "  X****               PARM                    X07DS
217300031219  "  X**** X07ERR        IFEQ      '0'
217400031219  "  X****               MOVEL     X07DES        tmese
217500031219  "  X****               ELSE
217600031219  "  X*ex C1396X****              MOVE      mes167        tmese
217700031219  "  X*ex C1396C         MOVE      per167        tmese
217800031219  "  X****               ENDIF
217900031219  "  X****
218000031219  "  X**Liq. IVA non mensile
218100031219  "  X****               Else
218200031219  "  X****               If        XscIvaLiq = '0'
218300031219  "  X**(annuale)
218400031219  "  X****               MOVE      *blanks       tper
218500031219  "  X****               Else
218600031219  "  X**(non annuale)
218700031219  "  X****               MOVE      per167        tper
218800031219  "  X****               EndIf
218900031219  "  X****               RESET                   X62DS
219000031219  "  X****               MOVE      soc167        X62societa
219100031219  "  X****               MOVE      ann167        X62anno
219200031219  "  X****               MOVE      per167        X62perliq
219300031219  "  X****               z-add     0             x62mese
219400031219  "  X****               z-add     0             x62giorno
219500031219  "  X****               CALL      'X62IVALQ'
219600031219  "  X****               PARM                    X62ds
219700031219  "  X**** X62esito      IFEQ      '0'
219800031219  "  X****               MOVEL     X62PerLiqD    tperd
219900031219  "  X****               ELSE
220000031219  "  X****               MOVE      *blanks       tperd
220100031219  "  X****               ENDIF
220200031219D1519X****               endif
220300031219D1519 * decodifica periodo di liquidazione
220400031219D1519C                   exsr      Inz_IntestazP
220500950704     C*
220600950704     C* Campi di lavoro
220700950724     C     *like         define    rivaliq       savaliq
220800950724     C     *like         define    rivrifiva     savrifiva
220900950725     C     *like         define    rivcaustes    savcaustes
221000970320     C     *like         define    plfamcomp     amcomp
221100950703     C*
221200950712     C* Azzero totali aliquota
221300950727     C                   z-add     0             p1imponnor
221400950727     C                   z-add     0             p1imposnor
221500950727     C                   z-add     0             p1imponind
221600950727     C                   z-add     0             p1imposind
221700950727     C                   z-add     0             p1totimpon
221800950727     C                   z-add     0             p1totimpos
221900950727     C                   z-add     0             p1totgen
222000950712     C                   z-add     0             t1totimpon
222100950712     C                   z-add     0             t1totimpos
222200950712     C                   z-add     0             t1totgen
222300970226     C                   z-add     0             t1imponnor
222400970226     C                   z-add     0             t1imposnor
222500970226     C                   z-add     0             t1imponind
222600970226     C                   z-add     0             t1imposind
222700950712     C*
222800950712     C* Azzero totali rifiva
222900950712     C                   z-add     0             p2imponib
223000950712     C                   z-add     0             t2totimpon
223100950725     C*
223200950725     C* Azzero totali causale
223300950727     C                   z-add     0             p3imponnor
223400950727     C                   z-add     0             p3imposnor
223500950727     C                   z-add     0             p3imponind
223600950727     C                   z-add     0             p3imposind
223700950727     C                   z-add     0             p3totimpon
223800950727     C                   z-add     0             p3totimpos
223900950727     C                   z-add     0             p3totgen
224000001212C1451C*
224100001212  "  C* pulizia skiere totali registro/libro
224200001212  "  C                   Clear                   SkRegLib
224300001212C1451C                   Clear                   SkImporto
224400950712     C*
224500950725     C* Stampa riepilogo con causale
224600950725     C     cau167        ifeq      *on
224700950725     C                   seton                                        84
224800950725     C                   endif
224900991230     C*
225000991230     C* Stampa riepilogo definitivo/provvisiorio
225100991230     C                   If        Bol167 = *Off
225200991230     C                   SetOn                                        96
225300991230     C                   EndIf
225400950712     C*
225500950606     C                   ENDSR
225600031219D1519 ************************************************************
225700031219  "   * Decodifica periodo di liquidazione
225800031219  "  X* (specifiche asteriscate in InzVar e riunite qui)
225900031219  "   ************************************************************
226000031219  "  C     Inz_IntestazP Begsr
226100031219D1519 *
226200031219     C* Liq. IVA mensile
226300031219D1519X****************** If        XscIvaLiq = '1'
226400031219  "   *                                                                A)
226500031219  "  C                   select
226600031219  "   *
226700031219  "   * l'intestazione del periodo liquidato deve indicare il mese se:
226800031219  "   * Regime iva non trasp. mista e tipo liq in anagr società mensile
226900031219  "   * oppure Regime iva tr. mista e tipo liq scelta nel lancio mensil
227000031219  "  C                   when      (XscIvaRgm<>'2' and  XscIvaLiq= '1')
227100031219  "  C                             or
227200031219D1519C                             (XscIvaRgm ='2' and  TpLiq167= '1')
227300031219     C                   RESET                   X07DS
227400031219     C                   MOVE      '1'           X07RIC
227500031219     C                   MOVEL     'ANDIZ000'    X07TRC
227600031219     C                   MOVEL     'DSPMESE'     X07CAM
227700031219C1396X*****              MOVEL     mes167        X07VAL
227800031219C1396C                   MOVEL     per167        X07VAL
227900031219     C                   MOVE      *OFF          X07ALL
228000031219     C                   CALLb     'X07VALR'
228100031219     C                   PARM                    X07DS
228200031219   B3C     X07ERR        IFEQ      '0'
228300031219     C                   MOVEL     X07DES        tmese
228400031219   X3C                   ELSE
228500031219C1396X*****              MOVE      mes167        tmese
228600031219C1396C                   MOVE      per167        tmese
228700031219   E3C                   ENDIF
228800031219     C*
228900031219     C* Liq. IVA non mensile
229000031219D1519X****************** Else
229100031219  "   *
229200031219  "   * l'intestaz. del periodo liquidato deve indicare il periodo se:
229300031219  "   * Regime iva NON tr. mista e tp liq in anagr società NON mensile
229400031219  "   * oppure Reg iva tr. mista e tp liq scelta nel lancio NON mensile
229500031219  "  C                   when      (XscIvaRgm<>'2' and  XscIvaLiq<>'1')
229600031219  "  C                             or
229700031219  "  C                             (XscIvaRgm ='2' and  TpLiq167<>'1')
229800031219D1519 *
229900031219     C                   If        XscIvaLiq = '0'
230000031219     C* (annuale)
230100031219     C                   MOVE      *blanks       tper
230200031219     C                   Else
230300031219     C* (non annuale)
230400031219     C                   MOVE      per167        tper
230500031219     C                   EndIf
230600031219D1519 *
230700031219  "  C                   select
230800031219  "   *
230900031219  "   * Regime iva NON trasportatori mista
231000031219  "  C                   when      XscIvaRgm <> '2'
231100031219D1519 *
231200031219     C                   RESET                   X62DS
231300031219     C                   MOVE      soc167        X62societa
231400031219     C                   MOVE      ann167        X62anno
231500031219     C                   MOVE      per167        X62perliq
231600031219     C                   z-add     0             x62mese
231700031219     C                   z-add     0             x62giorno
231800031219     C                   CALL      'X62IVALQ'
231900031219     C                   PARM                    X62ds
232000031219   B3C     X62esito      IFEQ      '0'
232100031219     C                   MOVEL     X62PerLiqD    tperd
232200031219   X3C                   ELSE
232300031219     C                   MOVE      *blanks       tperd
232400031219   E3C                   ENDIF
232500031219D1519 *
232600031219  "   * Regime iva trasportatori mista
232700031219  "  C                   when      XscIvaRgm = '2'
232800031219  "   *   numero periodo-------                                        -----
232900031219  "  C                   clear                   $Periodo
233000031219  "  C                   move      TPer          $MeseNum
233100031219  "  C                   move      TpLiq167      $TpLiqNum
233200031219  "  C                   if        $MeseNum<>0  and  $TpLiqNum<>0
233300031219  "  C     $MeseNum      div       $TpLiqNum     $Periodo
233400031219  "  C                   mvr                     $Resto
233500031219  "  C                   if        $Resto = 0
233600031219  "  C                   movel     $Periodo      TPer
233700031219  "  C                   endif
233800031219  "  C                   endif
233900031219  "   *   decodifica periodo-------                                    -----
234000031219  "  C                   z-add     1             $xp
234100031219  "  C     TpLiq167      lookup    $LisP($xp)                             25
234200031219  "  C                   if        *in25 = *on
234300031219  "  C                   eval      $$xp  = $OrdP($xp)
234400031219  "  C                   eval      IDMsg = $DesP($$xp)
234500031219  "  C                   callb     'XRTVM'
234600031219  "  C                   parm                    IdMsg            27
234700031219  "  C                   parm                    TxtMsg          100
234800031219  "  C                   parm                    ErrMsg            1
234900031219  "  C                   parm      *off          CtrMsg            1
235000031219  "  C                   parm      20            LenMsg            3 0
235100031219  "  C                   if        ErrMsg <> *on
235200031219  "  C                   movel     TxtMsg        TPerD
235300031219  "  C                   else
235400031219  "  C                   movel     *blanks       TPerD
235500031219  "  C                   endif
235600031219  "  C                   endif
235700031219  "   *
235800031219  "  C                   endsl
235900031219  "   *
236000031219  "  X****************** endif
236100031219  "  C                   endsl
236200031219  "   *
236300031219D1519C     XInz_IntestazPEndsr
236400030623c1668C************************************************************
236500030623  "  C* Reperimento dati da tabella 008
236600030623c1668C************************************************************
236700030623     C     REPTab008     BEGSR
236800030623     c*
236900030623     C                   CALLB     'XPAR'
237000030623     C                   parm                    xscsoc
237100030623     C                   parm      'RIEPIVA'     xpapar            8
237200030623     C                   parm                    xpaout
237300030623     C                   parm      *OFF          xpaerr            1
237400030623     C                   parm                    xparic            1
237500030623     c                   movel     xpaout        ndc099ds
237600030623     C*
237700030623     c* aggancio tabella 008
237800030623     C                   if        xpaerr<>*on
237900030623     c                   reset                   xatbds
238000030623     C                   MOVE      soc167        XTBAZI
238100030623     C                   MOVE      '008'         XTBcod
238200030623     c* Se numerazione autonoma
238300030623     c                   if        NumRiep045=' '
238400030623     c                   eval      xtbkey = '00000000000' + '4'+ '   '
238500030623     c                   else
238600030623     c* se numerazione in coda
238700030623     c* se numerazione libri iva per registro
238800030623     c                   if        Numeraz045='1'
238900030623     c                   eval      xtbkey = '00000000000' + '2'+ '   '
239000030623     c                   else
239100030623     c* se numerazione libri iva per libro
239200030623     c                   eval      xtbkey = '00000000000' + '2'+ libro167
239300030623     c                   endif
239400030623     c                   endif
239500030623     c*
239600030623     C                   MOVE      '1'           XTBRIC
239700030623     C                   MOVE      '0'           XTBALC
239800030623     C                   CALLB     'XATB'
239900030623     c                   parm                    xatbds
240000030624     c*
240100030623     c                   eval      ang008ds = xtbuni
240200030623     C*
240300030623     c                   if        %subst(ang008ds:27:9) = *blanks
240400030623     c                   eval      %subst(ang008ds:27:9) = *zeros
240500030623     c                   endif
240600030623     c                   if        %subst(ang008ds:21:6) = *blanks
240700030623     c                   eval      %subst(ang008ds:21:6) = *zeros
240800030623     c                   endif
240900030623     c*
241000030623     c                   z-add     Pagina008     NumeroPag         6 0
241100030623     c*
241200030623     c                   else
241300030623     c* arriva messaggio di errore da xpar all'operatore
241400030623     c                   eval      err167 = *on
241500030623     c                   exsr      endpgm
241600030624     c                   endif
241700030623     C*
241800030623c1668C                   ENDSR
241900961009     C/EJECT
242000020430C1550 ************************************************************
242100020430  "   * Reperimento descrizione tipo liquidazione se iva mista
242200020430  "   ************************************************************
242300020430  "  C     RieTpLiqD     Begsr
242400020430  "   *
242500020430  "  C                   clear                   TTpLiqD
242600020430  "   *
242700020430  "  C                   if        XscIvaRgm = '2'
242800020430  "   *
242900020430  "  C                   reset                   X07DS
243000020430  "  C                   move      '1'           X07RIC
243100020430  "  C                   movel     'ANDIZ000'    X07TRC
243200020430  "  C                   movel     'DSPIVALIQ'   X07CAM
243300020430  "  C                   movel     TpLiq167      X07VAL
243400020430  "  C                   callb     'X07VALR'
243500020430  "  C                   parm                    X07DS
243600020430  "   *
243700020430  "  C                   if        X07Err = '0'
243800020430  "  C                   movel     X07DES        TTpLiqD
243900020430  "  C                   endif
244000020430  "   *
244100020430  "   * centro la descrizione dentro la variabile in stampa
244200020430  "  C                   eval      TTpLiqD = %trim(TTpLiqD)
244300020430  "  C     ' '           checkr    TTpLiqD       XLenStr
244400020430  "  C                   eval      XLenTot = %size(TTpLiqD)
244500020430  "  C                   call      'X71CENTRA'
244600020430  "  C                   parm                    TTpLiqD
244700020430  "  C                   parm                    XLenStr           3 0
244800020430  "  C                   parm                    XLenTot           3 0
244900020430  "   *
245000020430  "  C                   endif
245100020430  "   *
245200020430C1550C     XRieTpLiqD    Endsr
245300961009     C************************************************************
245400961009     C* REPERIMENTO DATI SOCIETA'
245500961009     C************************************************************
245600961009     C     REPSOC        BEGSR
245700961009     C*
245800961009     C                   CALLB     'XSOC'
245900961009     C                   PARM                    TIPXSC            6
246000961009     C                   PARM                    SOCXSC            3
246100961009     C                   PARM                    CDSXSC            9 0
246200961009     C                   PARM                    MODXSC            3
246300961009     C                   PARM      *BLANKS       RTNXSC            1
246400961009     C                   PARM                    XSOCDS
246500961009     C                   PARM                    KPJBA
246600961009     C*
246700961009     C                   ENDSR
246800130620>>>>>C************************************************************
246900130620>>>>>C* Annullamento riga
247000130620>>>>>C************************************************************
247100130620>>>>>C     YAnnRiga      BEGSR
247200130620>>>>> *
247300130620>>>>>C                   EVAL      YRigaAnn = *ALL'*'
247400130620>>>>>C                   EVAL      %SUBST(YRigaAnn
247500130620>>>>>C                             :89:19)
247600130620>>>>>C                             = Annullato
247700130620>>>>> *
247800130620>>>>>C                   DOW       YCurLine   < 66
247900130620>>>>>C                   WRITE     YAnnRie
248000130620>>>>>C                   ENDDO
248100130620>>>>> *
248200130620>>>>>C                   ENDSR
248300130620>>>>>
248400130620>>>>>***********************************************************************
248500130620>>>>>**
248600130620>>>>>** Copia dello spool per archiviazione ottica.
248700130620>>>>>**
248800130620>>>>>***********************************************************************
248900161129>>>>>C*    YCpySplF      BEGSR
249000161129>>>>> *
249100161129>>>>>C*                  CLOSE     NDC167P
249200161129>>>>> *
249300161129>>>>>C*                  CALL      'YCO1611R'
249400161129>>>>>C*                  PARM                    Soc167
249500161129>>>>>C*                  PARM                    Bol167
249600161129>>>>>C*                  PARM      '0001-01-01'  YDtLoval         10
249700161129>>>>>C*                  PARM      '0001-01-01'  YDtLoval
249800161129>>>>>C*                  PARM      '*'           YJob             26
249900161129>>>>>C*                  PARM      'R'           YTpStampa         1
250000161129>>>>>C*                  PARM      *BLANK        YEsito            1
250100161129>>>>>C*                  PARM                    YSpoolNbr
250200161129>>>>>C*                  PARM                    Ann167
250300161129>>>>>C*                  PARM                    Per167
250400161129>>>>>C*                  PARM      *BLANK        WrkRegistro       1
250500161129>>>>>C*                  PARM                    TpLiq167
250600161129>>>>> *
250700161129>>>>>C*                  ENDSR
250701161129>>>>>  // --------------------------------------------------
250702161129>>>>>  // Procedure name: SendToDocFlow
250703161129>>>>>  // Purpose:        Invia lo spool a DocFlow
250704161129>>>>>  // Returns:        Esito.
250705161129>>>>>  // --------------------------------------------------
250706161129>>>>>
250707161129>>>>>  DCL-PROC SendToDocFlow ;
250708161129>>>>>
250709161129>>>>>    DCL-PI *N INT(10);
250710161129>>>>>    END-PI ;
250711161129>>>>>
250712161129>>>>>    DCL-DS local QUALIFIED;
250713161129>>>>>      aplKey CHAR(15) INZ('PJIVARIEPILOGO');
250714161129>>>>>      filNam CHAR(10) INZ('NDC167P');
250715161129>>>>>      numFil CHAR(6);
250716170721>>>>>      fileOut CHAR(255) INZ('IVAR');
250717161129>>>>>      subFldr CHAR(50) INZ('IVAR');
250718161129>>>>>      esito CHAR(1);
250719161129>>>>>    END-DS;
250720161129>>>>>
250721161129>>>>>    // Non creo il file per DocFlow quando non è una stampa definitiva.
250724161129>>>>>
250725161129>>>>>    IF bol167 = *OFF;
250726161129>>>>>      RETURN *ZERO;
250727161129>>>>>    ENDIF;
250728161129>>>>>
250742161129>>>>>    local.numFil = %EDITC(%DEC(YSpoolNbr:6:0):'X');
250743171212>>>>>    local.fileOut = %TRIMR(local.fileOut) + soc167 + ann167;
250747161129>>>>>
250748171212>>>>>    SELECT;
250749171212>>>>>      WHEN tpLiq167 = '1'; // Liquidazione mensile.
250750171212>>>>>        local.fileOut = %TRIMR(local.fileOut) + per167; // Mese.
250751171212>>>>>      WHEN tpLiq167 = '3' AND per167 = '03'; // Liquidazione I trimestre.
250752171212>>>>>        local.fileOut = %TRIMR(local.fileOut) + 'T1';
250753171212>>>>>      WHEN tpLiq167 = '3' AND per167 = '06'; // Liquidazione II trimestre.
250754171212>>>>>        local.fileOut = %TRIMR(local.fileOut) + 'T2';
250755171212>>>>>      WHEN tpLiq167 = '3' AND per167 = '09'; // Liquidazione III trimestre.
250756171212>>>>>        local.fileOut = %TRIMR(local.fileOut) + 'T3';
250757171212>>>>>      WHEN tpLiq167 = '3' AND per167 = '12'; // Liquidazione IV trimestre.
250758171212>>>>>        local.fileOut = %TRIMR(local.fileOut) + 'T4';
250759171212>>>>>    ENDSL;
250760171212>>>>>
250761171212>>>>>    local.fileOut = %TRIMR(local.fileOut) + '.spl';
250762161129>>>>>
250763161129>>>>>    CLOSE ndc167p;
250764161129>>>>>
250765161129>>>>>    TRBM_SendSpoolFileToIfs( local.aplKey
250766161129>>>>>                           : *BLANK
250767161129>>>>>                           : local.filNam
250768161129>>>>>                           : psJobName : psJobUser : psJobNbr
250769161129>>>>>                           : local.numFil
250770161129>>>>>                           : *BLANK
250771161129>>>>>                           : local.fileOut
250772161129>>>>>                           : local.subFldr
250773161129>>>>>                           : 'S'
250774161129>>>>>                           : 'S'
250775161129>>>>>                           : 'S'
250776161129>>>>>                           : local.esito
250777161129>>>>>                           );
250778161129>>>>>
250779161129>>>>>    IF local.esito = '2';
250780161129>>>>>      DUMP(A);
250781161129>>>>>      RETURN -1;
250782161129>>>>>    ENDIF;
250783161129>>>>>
250784161129>>>>>    RETURN *ZERO;
250785161129>>>>>
250786161129>>>>>  END-PROC ;
250787161129>>>>>
250800950606     C/EJECT
250900031219** $LisP/$OrdP                                                                           ###D1519###
25100003121901                                                                                       ###D1519###
25110003121922                                                                                       ###D1519###
25120003121933                                                                                       ###D1519###
25130003121944                                                                                       ###D1519###
25140003121965                                                                                       ###D1519###
251500031219**  $DesP                                                                                ###D1519###
251600031219PROMSG    *LIBL     COS1217   Anno         (1)                                           ###D1519###
251700031219PROMSG    *LIBL     COS1218   Bimestre     (2)                                           ###D1519###
251800031219PROMSG    *LIBL     COS1214   Trimestre    (3)                                           ###D1519###
251900031219PROMSG    *LIBL     COS1215   Quadrimestre (4)                                           ###D1519###
252000031219PROMSG    *LIBL     COS1216   Semestre     (5)                                           ###D1519###
