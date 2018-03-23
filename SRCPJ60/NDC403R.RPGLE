000100000216     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981215     H*PARMS BNDDIR(PJXBND PJCBND)
000300951004     H*PARMS CVTOPT(*DATETIME)
000400950608     H DECEDIT('0,') DATEDIT(*DMY.)
000500900731     F*
000600951013     F*            STAMPA REGISTRO DICHIARAZIONI DI INTENTO
000700951013     F*           ------------------------------------------
000800950726     F*
000900040308     Fandic02l  IF   E           K DISK
000901160630R229 FSdgDif01l IF   E           K DISK
001000040308     Fanmel01l  UF A E           K DISK
001100040308C1353x***ndc403p   O    E             PRINTER OFLIND(*in39)
001200000316C1353Fndc403p   O    E             printer oflind(*in39) usropn
001300951004     F*
001400951004     D*-------------
001500900731     D KPJBA         E DS
001600950605     D*-------------
001700950130     D* Parametri del PGM
001800951011     D ndc403ds      E DS
001900950628     D*-------------
002000941103     D* Dati per pgm XSOC
002100941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
002200941103     D*-------------
002300950605     D* DS Interna per dati utente
002400941103     D XSOCDS          DS          1000
002500950704     D*-------------
002600950704     D* Reperimento nome PGM
002700950704     D STATUS         SDS           333
002800950704     D  DSPGM            *PROC
002900951005     D*-------------
003000951016     D* Sottodefinizione campo MELSTRINGA
003100951016     D PLAREG        E DS                  EXTNAME(PLAREGDS)
003200951016     D*-------------
003300951012     D* DS esterna per dati clienti
003400951012     D DVACLI        E DS
003500951012     D*-------------
003600951012     D* DS esterna per dati fornitori
003700951012     D DVAFOR        E DS
003800040223D1546D*-------------
003900040223  "  D* Definizioni parametri driver estremi fiscali
004000040223  "  D dvcRic          S              1
004100040223  "  D dvcStrutt       S             10
004200040223  "  D dvcDtRif        S               D
004300040223  "  D dvcLenOut       S              9B 0
004400040223  "  D dvcErrore       S              1
004500040223  "  D dvcOutput       S           4000
004600040223  "  D wrksogg         S                   like(dvfsogg)
004700040223  "  D*-------------
004800040223  "  D* Ds per output variazioni estremi fiscali
004900040223D1546D ANCPI         E DS                  EXTNAME(ANCPI00F)
005000951012     D*-------------
005100951013     D* DS esterna per dati indirizzo
005200951013    >D ANIND         E DS                  EXTNAME(ANIND00F)
005300951013     D*-------------
005400951012     D* Definizioni parametri driver soggetti
005500951012     DdvaRic           S              1
005600951012     DdvaSocieta       S              3
005700951012     DdvaUnita         S              8
005800951012     DdvaDtRif         S               D
005900951012     DdvaStrutt        S             10
006000951012     DdvaLenOut        S              9B 0
006100951012     DdvaSnatura       S              1
006200951012     DdvaCodice        S              8
006300951012     DdvaLineav        S              3
006400951012     DdvaFiliale       S              3  0
006500951012     DdvaSogg          S              8
006600951012     DdvaTpInd         S              2
006700951012     DdvaCdInd         S              3
006800951012     DdvaOutput        S           4000
006900951012     DdvaErrore        S              1
007000951012     D*-------------
007100951016     D SavAnnoPr       S                   like(annoprot)
007200951016     D SavNrPr         S                   like(unrprot)
007300951016     D*-------------
007400951006     D DataInizio      S             10A   inz('0001-01-01')
007500951006     D DataFine        S             10A   inz('9999-12-31')
007600951006     D*-------------
007700031118A1047D* Parametri memorizzazione intestazione
007800031118  "  D xpaout          DS          2000
007900031118  "  D* DS esterna per stampa intestazione bollato
008000031118A1047D ndc099ds      E DS
008100950405     C*----------------------------------------------------*
008200950405     C*                MAIN LINE PROGRAM
008300950405     C*----------------------------------------------------*
008400950606     C* Inizializzazione variabili
008500950606     C                   EXSR      INZVAR
008600950724     C*
008700951006     C     k4dic2        setll     andic02l
008800951006     C     k2dic2        reade     andic02l                               21
008900951006     C     *in21         doweq     *off
009000951006     C     dicdtinter    andle     datafine
009100951011     C                   if        dicnrinter <= nrf403
009200970410     C                             and dicnrinter >= nri403
009300951004     C                   EXSR      STAMPA
009400951004     C                   endif
009500951006     C     k2dic2        reade     andic02l                               21
009600951004     C                   enddo
009700951016     C*
009800951016     C* Aggiornamento progressivo registro (se stampa bollato)
009900951016     C                   if        bol403 = *on
010000951016     C                   EXSR      AGGMEL
010100951016     C                   endif
010200900731     C*
010300950606     C* Fine programma
010400950606     C                   EXSR      ENDPGM
010500950606     C*
010600950606     C/EJECT
010700950725     C************************************************************
010800951016     C* STAMPA REGISTRO
010900950725     C************************************************************
011000951004     C     STAMPA        BEGSR
011100951005     C*
011200951012     C                   CLEAR                   DETTA
011300951012     C*
011400951012     C* Dati cliente/fornitore
011500951012     C                   EXSR      CLIFOR
011600951005     C*
011700951012     C                   z-add     dicnrcli      nrprot
011800951026     C                   CALLb     'XDT4000'
011900951012     C                   PARM      dicdtcli      XDTAMG           10
012000951012     C                   PARM                    XDTGMA            6 0
012100951026     C                   PARM      1             XDTSTA            1 0
012200951012     C                   if        XdtSta = 0
012300951012     C                   move      XDTGMA        dtprot
012400951012     C                   endif
012500951012     C                   z-add     dicnrinter    nrprin
012600040219     c*
012700991217B0136 *se l'utente vuole stampare la data protocollo interno
012800991217B0136C                   If        StpDIT403   = *Off
012900991217     C                   CALLb     'XDT4000'
013000951012     C                   PARM      dicdtinter    XDTAMG           10
013100951005     C                   PARM                    XDTGMA            6 0
013200951026     C                   PARM      1             XDTSTA            1 0
013300040219     c*
013400160630B0136 *se l'utente vuole stampare la data invio o ricevimento
013500040219B0136C                   Else
013600040219     c*
013700040219D1540C* per i clienti
013800040219  "  C                   IF        COF403='C'
013900040219  "  c* se c'è la data ricevimento lettera stampa quella
014000040219D1540c                   if        Dicdit <> '0001-01-01'
014100040219B0136C                   CALLb     'XDT4000'
014200991217  "  C                   PARM      DICDIT        XDTAMG           10
014300991217  "  C                   PARM                    XDTGMA            6 0
014400040219B0136C                   PARM      1             XDTSTA            1 0
014500040219D1540C                   else
014600040219  "  c* atlrimenti stampa la data protocollo interna
014700040219  "  C                   CALLb     'XDT4000'
014800040219  "  C                   PARM      DICDtinter    XDTAMG           10
014900040219  "  C                   PARM                    XDTGMA            6 0
015000040219  "  C                   PARM      1             XDTSTA            1 0
015100040308  "  c                   endif
015200040219  "  C                   else
015300040219  "  c* per fornitori stampa sempre la data lettera che se non c'è
015400040219  "  c* significa che non l'ho stampata (è un controllo)
015500040219  "  C                   CALLb     'XDT4000'
015600040219  "  C                   PARM      DICDIT        XDTAMG           10
015700040219  "  C                   PARM                    XDTGMA            6 0
015800040219  "  C                   PARM      1             XDTSTA            1 0
015900040219D1540C                   endif
016000040308B0136C                   EndIf
016100951005     C                   if        XdtSta = 0
016200951012     C                   move      XDTGMA        dtprin
016300951005     C                   endif
016400951012     C                   if        dicrev = *on
016500951026     C                   CALLb     'XDT4000'
016600951013     C                   PARM      dicdtrevoc    XDTAMG           10
016700951013     C                   PARM                    XDTGMA            6 0
016800951026     C                   PARM      1             XDTSTA            1 0
016900951013     C                   if        XdtSta = 0
017000951013     C                   move      XDTGMA        dtrev
017100951013     C                   endif
017200951012     C                   endif
017201160701R229 C* Richiesta stampa protocollo dichiarazione inviata
017202160701  "  C                   If        StpProt403 = *on
017203160701  "  C*
017204160701  "  C                   Eval      DifSocieta = DicSocieta
017205160701  "  C                   Eval      DifCf      = DicCF
017206160701  "  C                   Eval      DifNrInter = DicNrInter
017207160701  "  C                   Eval      DifDtInter = DicDtInter
017208160701  "  C*
017209160701  "  C     K04Dif01      CHAIN     SdgDif01l
017210160701  "  C                   If        %found(SdgDif01l)
017211160701  "  C                   Eval      Prot = DifProt
017212160701  "  C                   If        Prot <> 0
017213160701  "  C                   Eval      IntProt = 'Prot. invio:'
017214160701  "  C                   Eval      Barra = '/'
017215160701  "  C                   EndIf
017216160701  "  C                   Eval      ProtD = DifProtD
017217160701  "  C                   Endif
017218160701R229 C                   Endif
017300950725     C*
017400951012     C                   if        *in39 = *on
017500031209A1047C* Se non chiesto bollato
017600031209  "  C                   if        bol403 = *off
017700031209  "  C* e se non chiesta stampa intestazione
017800031118A1047c                             and intesta403=*off
017900951012     C                   WRITE     TESTA
018000951012     C                   endif
018100031209A1047C* Se chiesto bollato
018200031209  "  C                   if        bol403 = *on
018300031209  "  C* e se non chiesta stampa intestazione
018400031209  "  c                             and intesta403=*off
018500031209  "  C                   WRITE     TESTA1
018600031209  "  C                   ENDIF
018700031209  "  C* se chiesta stampa intestazione (bollato o non bollato)
018800031118  "  c                   if        intesta403=*on
018900031118  "  C                   exsr      StampaTes
019000031209  "  C                   endif
019100031209A1047X***                WRITE     TESTA1
019200031209A1047c* in tutti i casi
019300031209A1047C                   WRITE     TESTA2
019400970226     c                   setoff                                       39
019500951012     C                   endif
019600951012     C                   WRITE     DETTA
019700951004     C*
019800951016     C* Salvo valori per aggiornamento progressivo registro
019900951016     C                   movel     dicdtinter    savannopr
020000951016     C                   z-add     dicnrinter    savnrpr
020100951016     C*
020200950725     C                   ENDSR
020300950725     C/EJECT
020400951016     C************************************************************
020500951016     C* AGGIORNAMENTO PROGRESSIVO REGISTRO
020600951016     C************************************************************
020700951016     C     AGGMEL        BEGSR
020800951016     C*
020900951016     C                   if        cof403 = 'C'
021000951016     C                   movel     'PLACLI'      meltipo
021100951016     C                   else
021200951016     C                   movel     'PLAFOR'      meltipo
021300951016     C                   endif
021400951016     C                   move      xscsoc        melsocieta
021500951016     C                   move      *blank        melunita
021600951016     C                   move      *blank        melctb
021700951016     C                   move      *blank        melkeygest
021800951016     C                   move      dtb403        meldata
021900951016     C*
022000951016     C     k6mel1        chain     anmel01l                           21
022100951016     C*
022200951016     C                   z-add     savannopr     annoprot
022300951016     C                   z-add     savnrpr       unrprot
022400951016     C                   movel     plareg        melstringa
022500951016     C*
022600951016     C                   if        *in21 = *off
022700951016     C                   UPDATE    anmel000
022800951016     C                   else
022900951016     C                   WRITE     anmel000
023000951016     C                   endif
023100951016     C*
023200951016     C                   ENDSR
023300951016     C/EJECT
023400950606     C************************************************************
023500950606     C* FINE PROGRAMMA
023600950606     C************************************************************
023700950606     C     ENDPGM        BEGSR
023800031118     c*
023900031118A1047C* se era stata chiesta l'intestazione, aggiorno con ultimo numero
024000031118  "  C* di pagina il parametro modulo dove è memorizzata
024100031118  "  c                   if        intesta403=*on and
024200031118  "  c* e se stampa bollato
024300031118  "  c                             Bol403    =*on
024400031118  "  c                   move      NumeroPag     Pgi099
024500031118  "  c                   movel     'DICHINT'     xpapar
024600031118  "  c                   move      cof403        xpapar
024700031118  "  C                   CALLB     'XPAR'
024800031118  "  C                   parm      soc403        xscsoc
024900031118  "  c                   parm                    xpapar            8
025000031118  "  c                   parm      ndc099ds      xpaout
025100031118  "  C                   parm      *OFF          xpaerr            1
025200031118  "  C                   parm      '1'           xparic            1
025300031118A1047c                   endif
025400950606     C*
025500951012     C                   MOVEL     ndc403ds      KPJBU
025600950711     C                   SETON                                        LR
025700950606     C                   RETURN
025800950606     C*
025900950606     C                   ENDSR
026000950606     C/EJECT
026100950405     C************************************************************
026200900731     C* Definizioni e inizializzazioni
026300950405     C************************************************************
026400940831     C     *INZSR        BEGSR
026500900731     C*
026600900731     C     *ENTRY        PLIST                                                  riceve
026700900731     C                   PARM                    KPJBA                          parametro
026800941104     C*
026900940831     C                   ENDSR
027000950606      /EJECT
027100950606     C************************************************************
027200950606     C* INIZIALIZZAZIONE VARIABILI
027300950606     C************************************************************
027400950606     C     INZVAR        BEGSR
027500950606     C*
027600951011     C                   MOVEL     KPJBU         ndc403ds
027700950724     C*
027800950606     C* Reperimento azienda
027900951012     C                   MOVE      soc403        SOCXSC
028000950606     C                   MOVEL     'SOC001'      TIPXSC
028100950606     C                   EXSR      REPSOC
028200950606     C     RTNXSC        IFNE      '1'
028300950606     C                   MOVEL     XSOCDS        SOC001
028400950606     C                   ELSE
028500950606     C                   EXSR      ENDPGM
028600950606     C                   ENDIF
028700951012     C*
028800951012     C* PLIST per NDVASOG
028900951012     C     PDVASOG       PLIST
029000951012     C                   PARM                    DVARIC
029100951012     C                   PARM                    DVASOCIETA
029200951012     C                   PARM                    DVAUNITA
029300951012     C                   PARM                    DVASTRUTT
029400951012     C                   PARM                    DVADTRIF
029500951012     C                   PARM                    DVALENOUT
029600951012     C                   PARM                    DVASNATURA
029700951012     C                   PARM                    DVACODICE
029800951012     C                   PARM                    DVALINEAV
029900951012     C                   PARM                    DVAFILIALE
030000951012     C                   PARM                    *omit
030100951012     C                   PARM                    DVATPIND
030200951012     C                   PARM                    DVACDIND
030300951012     C                   PARM                    DVAERRORE
030400951012     C                   PARM                    DVAOUTPUT
030500951016     C*
030600951016     C* klist per ANMEL01L
030700040308     C     k6mel1        KLIST
030800040308     C                   KFLD                    meltipo
030900040308     C                   KFLD                    melsocieta
031000040308     C                   KFLD                    melunita
031100040308     C                   KFLD                    melctb
031200040308     C                   KFLD                    melkeygest
031300040308     C                   KFLD                    meldata
031400040308     C*
031500040308     C* klist per ANDIC02L
031600040308     C     k4dic2        KLIST
031700040308     C                   KFLD                    dicsocieta
031800040308     C                   KFLD                    diccf
031900040308     C                   KFLD                    dicdtinter
032000040308     C                   KFLD                    dicnrinter
032100040308     C*
032200040308     C     k2dic2        KLIST
032300040308     C                   KFLD                    dicsocieta
032400040308     C                   KFLD                    diccf
032500040223     C*
032501160630R229 C* SdgDif01l
032502160630 "   C     K04Dif01      KLIST
032503160630 "   C                   KFLD                    DifSocieta
032504160630 "   C                   KFLD                    DifCf
032505160630 "   C                   KFLD                    DifDtInter
032506160630R229 C                   KFLD                    DifNrInter
032600040223D1546C* PLIST per NDDVACPI
032700040223  "  C     PDVACPI       PLIST
032800040223  "  C                   PARM                    DVCRIC
032900040223  "  C                   PARM                    DVCSOCIETA
033000040223  "  C                   PARM                    DVCUNITA
033100040223  "  C                   PARM                    DVCSTRUTT
033200040223  "  C                   PARM                    DVCDTRIF
033300040223  "  C                   PARM                    DVCLENOUT
033400040223  "  C                   PARM                    DVCSOGG
033500040223  "  C                   PARM                    DVCERRORE
033600040223D1546C                   PARM                    DVCOUTPUT
033700951005     C*
033800951016     C* Imposto klist per ANDIC01L
033900951011     C                   move      soc403        dicsocieta
034000951011     C                   move      cof403        diccf
034100951011     C                   movel     anno403       datainizio
034200951006     C                   move      datainizio    dicdtinter
034300951011     C                   z-add     nri403        dicnrinter
034400951006     C*
034500951011     C                   movel     anno403       datafine
034600951012     C*
034700951012     C* Valorizzazione campi univoci testate
034800951012     C                   MOVEL     KNSIF         NOMSIF
034900951012     C                   MOVEL     XSCDSI        NOMDIT
035000951012     C                   MOVEL     DSPGM         NOMPGM
035100951012     C* Reperisco nome sistema informatico
035200951012     C                   CALL      'XNETA'                              21
035300951012     C                   PARM                    NOMSYS
035400951012     C*
035500951012     C                   move      anno403       anno
035600951012     C                   if        cof403 = 'C'
035700951012     C                   seton                                        50
035800951012     C                   endif
035900000316C1353 *
036000000316  "   * Chiamata ad X32OvrPrtF per eseguire OVRPRTF in base a tab. G75
036100000316  "  C                   exsr      CallX32
036200000316  "  C                   open      Ndc403P
036300000316C1353 *
036400031118A1047C* se richiesto reperisco intestazione memorizzata sul
036500031118  "  C* parametro modulo 'DICHINT+COF'
036600031118  "  C                   if        intesta403=*on
036700031118  "  C                   movel     'DICHINT'     xpapar
036800031118  "  c                   move      Cof403        xpapar
036900031118  "  C                   CALLB     'XPAR'
037000031118  "  C                   parm      soc403        xscsoc
037100031118  "  C                   parm                    xpapar            8
037200031118  "  C                   parm                    xpaout
037300031118  "  C                   parm      *OFF          xpaerr            1
037400031118  "  C                   parm                    xparic            1
037500031118  "  c                   movel     xpaout        ndc099ds
037600031118  "  c                   if        xpaerr <> *on
037700031118  "  c* imposto pagina da cui partire
037800031118  "  c                   move      pgi099        NumeroPag         6 0
037900031118  "  c                   else
038000031118  "  c* arriva messaggio di errore da xpar all'operatore
038100031118  "  c                   exsr      endpgm
038200031118  "  c                   endif
038300031118A1047c                   endif
038400951012     C*
038500951012     C* Stampa testata
038600031209A1047C* se non richiesto bollato
038700031209  "  C                   if        bol403 = *off
038800031209  "  C* se non chiesta stampa intestazione
038900031118A1047c                             and intesta403=*off
039000951012     C                   WRITE     TESTA
039100951012     C                   endif
039200031209A1047C* se richiesto bollato
039300031209  "  C                   if        bol403 = *on
039400031209  "  C* e se non chiesta stampa intestazione
039500031209  "  c                             and intesta403=*off
039600031209  "  C                   WRITE     TESTA1
039700031209  "  c                   endif
039800031209  "  C* se chiesta stampa intestazione (bollato o non bollato)
039900031118  "  c                   if        intesta403=*on
040000031118  "  C                   exsr      StampaTes
040100031209  "  C                   endif
040200031209  "  x***                WRITE     TESTA1
040300031209  "  c* in tutti i casi
040400031209A1047C                   WRITE     TESTA2
040500951012     C*
040600951016     C                   z-add     0             savannopr
040700951016     C                   z-add     0             savnrpr
040800951016     C*
040900950606     C                   ENDSR
041000031118A1047 ************************************************************
041100031118      * STAMPA Intestazione da parametro modulo
041200031118A1047 ************************************************************
041300031118     C     StampaTes     BEGSR
041400031118     c*
041500031118     c                   eval      T0riga1 =rg1099
041600031118     c* anno/pagina
041700031118     c                   move      numeropag     numpag            6
041800031118     C                   eval      P1AnnoPag= Anno099+ '/' + numpag
041900031118     c                   eval      T0riga2 =rg2099
042000031118     c                   eval      T0riga3 =rg3099
042100031118     c                   eval      T0riga4 =rg4099
042200031118     c                   WRITE     Testata1
042300031118     C                   ADD       1             NumeroPag
042400031118     C*
042500031118A1047C                   ENDSR
042600950606     C/EJECT
042700000316C1353C***********************************************************
042800000316  "  C* Chiamata ad X32 per eseguire OvrPrtF
042900000316  "  C***********************************************************
043000000316  "  C     CallX32       BEGSR
043100000316  "   *
043200000316  "  C                   eval      X32Societa = XScSoc
043300000316  "  C                   eval      X32CdStamp = 'RDI'
043400000316  "  C                   eval      X32Unita   = Uni403
043500000316  "  C                   eval      X32Errore  = *Off
043600000316  "   *
043700000316  "   * stampa registro dich. d'intento per clienti/fornitori
043800000316  "  C                   if        Cof403 = 'C'
043900000316  "  C                   movel     'C'           X32KeyGest
044000000316  "  C                   else
044100000316  "  C                   movel     'F'           X32KeyGest
044200000316  "  C                   endif
044300000316  "   *
044400000316  "   * se stampa su bollato = 'S'
044500000316  "  C                   if        Bol403 = *On
044600000316  "  C                   cat       'D':0         X32KeyGest
044700000316  "  C                   else
044800000316  "  C                   cat       'P':0         X32KeyGest
044900000316  "  C                   endif
045000000316  "   *
045100000316  "   * Chiamo X32OvrPrtF senza passare il parametro SoloCtrl,in modo
045200000316  "   * che se il rcd non viene trovato nella tabella G75 venga emessa
045300000316  "   * una window di avvertimento
045400000316  "   *
045500000316  "  C                   call      'X32OVRPRTF'
045600000316  "  C                   parm                    X32Societa        3
045700000316  "  C                   parm                    X32CdStamp        3
045800000316  "  C                   parm                    X32Unita          8
045900000316  "  C                   parm                    X32KeyGest       12
046000000316  "  C                   parm                    X32Errore         1
046100000316  "  C                   parm                    Kpjba
046200000316  "  C*
046300000316C1353C                   ENDSR
046400951005     C************************************************************
046500951012     C* DATI CLIENTE/FORNITORE
046600951005     C************************************************************
046700951012     C     CLIFOR        BEGSR
046800951013     C*
046900951013     C                   setoff                                       80
047000951006     C*
047100951005     C                   move      '1'           DVARIC
047200951005     C                   move      xscsoc        DVASOCIETA
047300960313     C                   move      Uni403        DVAUnita
047400951012     C                   move      *blank        DVASTRUTT
047500951012     C                   if        cof403 = 'C'
047600951012     C                   movel     'DVACLI'      DVASTRUTT
047700951012     C                   eval      DVALENOUT = %Size(DVACLI)
047800951012     C                   else
047900951012     C                   movel     'DVAFOR'      DVASTRUTT
048000951012     C                   eval      DVALENOUT = %Size(DVAFOR)
048100951012     C                   endif
048200951005     C                   move      dicksc        DVACODICE
048300951012     C                   move      cof403        DVASNATURA
048400951012     C                   clear                   DVALINEAV
048500951012     C                   clear                   DVAFILIALE
048600951012     C                   clear                   DVATPIND
048700951012     C                   clear                   DVACDIND
048800951005     C*
048900951005     C                   CALLB     'NDDVASOG'    PDVASOG
049000951005     C*
049100951005     C     DVAERRORE     ifeq      *off
049200951012     C                   if        cof403 = 'C'
049300951012     C                   eval      %subst(DVACLI:1:DVALENOUT)
049400951012     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
049500951012     C                   movel     dvckcc        kcc
049600951012     C                   movel     dvcksc        ksc
049700951012     C                   movel     dvcdes        ragsoc
049800951012     C                   movel     dvcindriz     via
049900951012     C                   movel     dvccap        cap
050000951012     C                   movel     dvclocalit    local
050100951012     C                   movel     dvcprov       prov
050200040223D1546C     Dvcpartiva    ifne      *blank
050300951012     C                   movel     dvcpartiva    piva
050400040223D1546C                   else
050500040223  "  C                   movel     dvccdfisc     piva
050600040223  "  C                   endif
050700040224  "  C* vedo se ci sono variazioni di dati fiscali alla data protoc.int
050800040223  "  C                   eval      wrksogg = dvcsogg
050900040223D1546c                   exsr      CallDvaCpi
051000951012     C                   else
051100951012     C                   eval      %subst(DVAFOR:1:DVALENOUT)
051200951012     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
051300951013     C                   movel     dvfkcc        kcc
051400951013     C                   movel     dvfksc        ksc
051500040223D1546C     Dvfpartiva    ifne      *blank
051600951013     C                   movel     dvfpartiva    piva
051700040223D1546C                   else
051800040223  "  C                   movel     dvfcdfisc     piva
051900040224D1546C                   endif
052000951013     C                   if        dvfkcca <> *blank and dvfksca <> *blank
052100951013     C* Percipiente
052200951013     C                   if        dvfcognome <> *blank
052300951013     C                   seton                                        80
052400951013     C                   movel     dvfcognome    cognome
052500951013     C                   movel     dvfnome       nome
052600951013     C                   else
052700951013     C                   movel     dvfdes        ragsoc
052800951013     C                   endif
052900951013     C                   if        dvftpinda<>*blank and dvfcdinda<>*blank
053000951013     C* Indirizzo percipiente diverso
053100951013     C                   move      '1'           DVARIC
053200951013     C                   move      xscsoc        DVASOCIETA
053300960313     C                   move      Uni403        DVAUnita
053400951013     C                   move      *blank        DVASTRUTT
053500951013     C                   movel     'ANIND'       DVASTRUTT
053600951013     C                   eval      DVALENOUT = %Size(ANIND)
053700951013     C                   move      dicksc        DVACODICE
053800951013     C                   move      'F'           DVASNATURA
053900951013     C                   clear                   DVALINEAV
054000951013     C                   clear                   DVAFILIALE
054100951013     C                   move      dvftpinda     DVATPIND
054200951013     C                   move      dvfcdinda     DVACDIND
054300951013     C                   CALLB     'NDDVASOG'    PDVASOG
054400951013     C     DVAERRORE     ifeq      *off
054500951013     C                   eval      %subst(ANIND:1:DVALENOUT)
054600951013     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
054700951013     C                   movel     indindriz     via
054800951013     C                   movel     indcap        cap
054900951013     C                   movel     indlocalit    local
055000951013     C                   movel     indprov       prov
055100951013     C                   endif
055200000216D0452C* Indirizzo percipiente normale
055300000216  "  C                   else
055400000216  "  C                   movel     dvfindriz     via
055500000216  "  C                   movel     dvfcap        cap
055600000216  "  C                   movel     dvflocalit    local
055700000216D0452C                   movel     dvfprov       prov
055800951013     C                   endif
055900951013     C                   else
056000951013     C* Solo fornitore
056100951012     C                   movel     dvfdes        ragsoc
056200951012     C                   movel     dvfindriz     via
056300951012     C                   movel     dvfcap        cap
056400951012     C                   movel     dvflocalit    local
056500951012     C                   movel     dvfprov       prov
056600040224D1546c* ma in questo caso devo vedere se ci sono variazioni ai dati
056700040224  "  C* alla data prot.int.
056800040224  "  C                   eval      wrksogg = dvfsogg
056900040224D1546c                   exsr      CallDvaCpi
057000951012     C                   endif
057100951005     C                   endif
057200951013     C                   endif
057300951005     C*
057400951005     C                   ENDSR
057500951005     C/EJECT
057600040223D1546C************************************************************
057700040223     C* DATI CLIENTE/FORNITORE
057800040223D1546C************************************************************
057900040223     C     CallDvaCpi    BEGSR
058000040223     C*
058100040223     C                   move      '2'           DVCRIC
058200040223     C                   move      Soc403        DVCSOCIETA
058300040223     C                   move      *blank        DVCUNITA
058400040223     C                   movel     'ANCPI'       DVCSTRUTT
058500040223     C                   move      DicDtInter    DVCDTRIF
058600040223     C                   eval      DVCLENOUT = %Size(ANCPI)
058700040223     C                   move      wrksogg       DVCSOGG
058800040223     C*
058900040223     C                   CALLB     'NDDVACPI'    PDVACPI
059000040223     C*
059100040223     C     dvcerrore     ifeq      *off
059200040223     C                   movel     dvcoutput     ancpi
059300040223     C     cpipartiva    ifeq      *blank
059400040223     C                   movel     cpicdfisc     piva
059500040223     C                   else
059600040223     C                   movel     cpipartiva    piva
059700040223     C                   endif
059800040224     C                   movel     cpidessog1    ragsoc
059900040223     C                   movel     cpiindriz     via
060000040223     C                   movel     cpicap        Cap
060100040223     C                   movel     cpilocalit    Local
060200040223     C                   movel     cpiprov       Prov
060300040223     c                   endif
060400040223     C*
060500040223D1546C                   ENDSR
060600941103      ************************************************************
060700941103      * Reperimento dati società
060800941103      ************************************************************
060900941103     C     REPSOC        BEGSR
061000941103      *
061100950630     C                   CALLB     'XSOC'
061200941103     C                   PARM                    TIPXSC            6
061300941103     C                   PARM                    SOCXSC            3
061400941103     C                   PARM                    CDSXSC            9 0
061500941103     C                   PARM                    MODXSC            3
061600941103     C                   PARM      *BLANKS       RTNXSC            1
061700941103     C                   PARM                    XSOCDS
061800941103     C                   PARM                    KPJBA
061900941103      *
062000941103     C                   ENDSR
062100950606     C/EJECT
