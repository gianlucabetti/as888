000100000706     H*BIND
000200000706     H*PARMS CVTOPT(*NONE)
000300000706     H*PARMS OPTION(*NOXREF)
000400000706     H DECEDIT('0,') DATEDIT(*DMY.)
000500000706      *
000600000706      *           -------------------------------------
000700000719      *             Stampa prospetto liquidazione IVA
000800000706      *           -------------------------------------
000900000706      *
001000000719     F*------------
001100000719     FNDCEL02L  IF   E           K DISK
001200000719     F*------------
001300020430C1550 * Soc/TpLiq/ Anno/PerLiq/CodCel
001400000719     FNDRIP01L  IF   E           K DISK
001500000706     F*------------
001600000719     FNDC167P   O    E             PRINTER OFLIND(*IN39)
001700100621     F                                     USROPN
001800000719     F*------------
001900030626c1668D* Parametri memorizzazione intestazione
002000030626  "  D xpaout          DS          2000
002100030626  "  D*-------------
002200030626  "  D* DS esterna per stampa intestazione bollato
002300030626  "  D ndc099ds      E DS
002400030626c1668D*-------------
002500000706     D*-------------
002600000706     D* Passaggio Parametri
002700000706     D KPJBA         E DS
002800000714     D*-------------
002900000714     D* Dati di ambiente ottenuti da XSOC
003000000719     D SOC001        E DS                  EXTNAME(XSOC001DS)
003100000717     D    XScDare                     1    Overlay(XScFgO:7)
003200000717     D    XScAvere                    1    Overlay(XScFgO:8)
003300020430C1550D    XScIvaRgm                   1    Overlay(XScFgO:9)
003400000720     D    XScIvaLiq                   1    Overlay(XScFgO:10)
003500000706     D*-------------
003600000706     D* Parametri in ricezione
003700000719     D NDC167DS      E DS
003800000706     D*-------------
003900000720     D* Controllo values
004000000720     D X07DS         E DS                  EXTNAME(X07VALDS)
004100000720     D*-------------
004200000720     D* Periodi liquidazione iva
004300000720     D X62DS         E DS                  EXTNAME(X62IVALQDS)
004400000720     D*-------------
004500000706     D* Reperimento nome PGM
004600000706     D STATUS         SDS           333
004700000706     D  DSPGM            *PROC
004800000706     D  PARMS            *PARMS
004900000706     D*-------------
005000000721     D $CELEoF                        1A
005100000719     D*-------------
005200000706     C*----------------------------------------------------*
005300000706     C*                MAIN LINE PROGRAM
005400000706     C*----------------------------------------------------*
005500000706     C*
005600100621     C                   IF        NOT %OPEN(ndc167p)
005700100621     C                   OPEN      ndc167p
005800100621     C                   ENDIF
005900000706     C* inizializzazione variabili
006000000706     C                   EXSR      INZVAR
006100000706     C*
006200000719     C* Stampa prospetto di liquidazione
006300000719     C                   ExSr      StpProsLiq
006400000719     C*
006500000706     C* fine programma
006600000706     C                   EXSR      ENDPGM
006700000706     C*
006800000719     C************************************************************
006900000719     C* Stampa prospetto liquidazione
007000000719     C************************************************************
007100000719     C     StpProsLiq    BEGSR
007200000719     C*
007300000802     C     SocCap167     SetLL     NDCEL02L
007400000802     C     SocCap167     ReadE     NDCEL02L                               21
007500000721     C                   Eval      $CELEoF = *In21
007600000719     C*
007700000721     C                   DoW       $CELEoF = *Off
007800000719     C*
007900000719     C                   ExSr      Stampa
008000000719     C*
008100000802     C     SocCap167     ReadE     NDCEL02L                               21
008200000721     C                   Eval      $CELEoF = *In21
008300000719     C                   EndDo
008400000719     C*
008500000719     C                   ENDSR
008600000719     C************************************************************
008700000719     C* Stampa
008800000719     C************************************************************
008900000719     C     Stampa        BEGSR
009000000719     C*
009100000719     C* Aggancio memoria contenuto celle
009200000719     C                   Eval      RIPSocieta = Soc167
009300020430C1550C                   Eval      RIPTpLiq  = TpLiq167
009400000719     C                   Eval      RIPAnno = Ann167
009500000719     C                   Eval      RIPPerLiq = Per167
009600000719     C                   Eval      RIPCodCel = CELCodCel
009700020430C1550X**** K04rip01      Chain     NDRIP01L                           21
009800020430C1550C     K05rip01      Chain     NDRIP01L                           21
009900000720     C                   If        *In21 = *Off
010000000719     C*
010100000719     C* Testata
010200000719     C                   If        *In39 = *On
010300030626c1668c* se richieste intestazioni
010400030626c1668C                   if        Intesta167 =*off
010500030626     c* solo se non bollato (96 ON) stampa testata fissa a prtf
010600030626     C   96              WRITE     TESTA0
010700030626c1668c                   else
010800030626  "  c* sia bollato che provvisorio
010900030626  "  c* stampo intestazione memorizzata su param. modulo
011000030626  "  c                   eval      T0riga1 =rg1099
011100030626  "  c* anno/pagina
011200030626  "  c                   move      numeropag     numpag            6
011300030626  "  C                   eval      P1AnnoPag= Anno099+ '/' + numpag
011400030626  "  c                   eval      T0riga2 =rg2099
011500030626  "  c                   eval      T0riga3 =rg3099
011600030626  "  c                   eval      T0riga4 =rg4099
011700030626  "  C                   ADD       1             NumeroPag
011800030626  "  C                   WRITE     TESTA0A
011900030626c1668c                   endif
012000030626     C                   Write     TESTA
012100000731     C                   Write     TESTA8
012200000719     C                   SetOff                                       39
012300000719     C                   EndIf
012400000719     C*
012500000719     C                   MoveL     CELDescr      P8Descr
012600000721     C                   If        RIPDare = 1
012700001229C1457C                             and ColPrLq167 = '1' or
012800001229  "  C                             RIPAvere = 1
012900001229  "  C                             and ColPrLq167 = '2'
013000001229  "  X*****              Z-Add     RIPImporto    P8ImpDar
013100001229  "  X*****              Clear                   P8ImpAve
013200001229  "  C                   Z-Add     RIPImporto    P8Importo1
013300001229C1457C                   Clear                   P8Importo2
013400000719     C                   Else
013500001229C1457X*****              Z-Add     RIPImporto    P8ImpAve
013600001229  "  X*****              Clear                   P8ImpDar
013700001229  "  C                   Z-Add     RIPImporto    P8Importo2
013800001229C1457C                   Clear                   P8Importo1
013900000719     C                   EndIf
014000000719     C*
014100000719     C                   Write     ProLiqP8
014200000719     C*
014300000720     C                   EndIf
014400000719     C*
014500000719     C                   ENDSR
014600000706     C************************************************************
014700000706     C* OPERAZIONI INIZIALI
014800000706     C************************************************************
014900000706     C     *INZSR        BEGSR
015000000706     C*
015100000706     C* Reperimento parametri
015200000706     C*
015300000706     C     *ENTRY        PLIST
015400000706     C                   PARM                    KPJBA
015500000714     C                   PARM                    SOC001
015600000706     C*
015700000706     C                   ENDSR
015800000706     C************************************************************
015900000706     C* INIZIALIZZAZIONE VARIABILI
016000000706     C************************************************************
016100000706     C     INZVAR        BEGSR
016200000706     C*
016300000706     C* Dati da PGM chiamante
016400000719     C                   MOVEL     KPJBU         NDC167DS
016500000719     C*
016600000719     C* Valorizzazione campi univoci testate
016700000719     C                   MOVEL     KNSIF         NOMSIF
016800000719     C                   MOVEL     XSCDSI        NOMDIT
016900000719     C                   MOVEL     DSPGM         NOMPGM
017000000719     C* Reperisco nome sistema informatico
017100000719     C                   CALL      'XNETA'                              21
017200000719     C                   PARM                    NOMSYS
017300000719     C*
017400000719     C* Tipo liquidazione IVA
017500000719     C                   If        XscIvaLiq <> '1'
017600000719     C* (non mensile)
017700000719     C                   SetOn                                        13
017800000719     C                   Else
017900000719     C* (mensile)
018000000719     C                   SetOff                                       13
018100000719     C                   Endif
018200000719     C*
018300000719     C* Imposto campi testata
018400000720     C                   MoveL     Ann167        TAnno
018500000719     C* Liq. IVA mensile
018600000719     C                   If        XscIvaLiq = '1'
018700000719     C                   RESET                   X07DS
018800000719     C                   MOVE      '1'           X07RIC
018900000719     C                   MOVEL     'ANDIZ000'    X07TRC
019000000719     C                   MOVEL     'DSPMESE'     X07CAM
019100000720     C                   MOVEL     Per167        X07VAL
019200000719     C                   MOVE      *OFF          X07ALL
019300000719     C                   CALLb     'X07VALR'
019400000719     C                   PARM                    X07DS
019500000719     C     X07ERR        IFEQ      '0'
019600000720     C                   MOVEL     X07DES        TMese
019700000719     C                   ELSE
019800000720     C                   MOVE      Per167        TMese
019900000719     C                   ENDIF
020000000719     C* Liq. IVA non mensile
020100000719     C                   Else
020200000719     C                   If        XscIvaLiq = '0'
020300000719     C* (annuale)
020400000720     C                   MOVE      *Blank        TPer
020500000719     C                   Else
020600000719     C* (non annuale)
020700000720     C                   MOVE      Per167        TPer
020800000719     C                   EndIf
020900000719     C                   RESET                   X62DS
021000000720     C                   MOVE      Soc167        X62Societa
021100000720     C                   MOVE      Ann167        X62Anno
021200000720     C                   MOVE      Per167        X62PerLiq
021300000720     C                   Z-Add     0             X62Mese
021400000720     C                   Z-Add     0             X62Giorno
021500000719     C                   CALL      'X62IVALQ'
021600000720     C                   PARM                    X62DS
021700000720     C     X62Esito      IFEQ      '0'
021800000720     C                   MOVEL     X62PerLiqD    TPerD
021900000719     C                   ELSE
022000000720     C                   MOVE      *Blank        TPerD
022100000719     C                   ENDIF
022200000720     C                   EndIf
022300000719     C*
022400000719     C* Settaggio indicatori per la stampa
022500000719     C                   SetOn                                        39
022600000719     C                   SetOn                                        8587
022700000719     C                   If        Bol167 = *Off
022800000719     C                   SetOn                                        96
022900000719     C                   EndIf
023000001229C1457C* (dare a sinistra, avere a destra)
023100001229  "  C                   If        ColPrLq167 = '1'
023200001229  "  C                   SetOff                                       88
023300001229  "  C* (avere a sinistra, dare a destra)
023400001229  "  C                   Else
023500001229  "  C                   SetOn                                        88
023600001229C1457C                   EndIf
023700020430C1550 *
023800020430  "   * descrizione tipo liquidazione se regime iva trasp. mista
023900020430C1550C                   exsr      RieTpLiqD
024000030626     c*
024100030626c1668c* Se chiesta intestazione
024200030626  "  c                   if        intesta167=*on
024300030626  "   * reperimento parametro modulo intestazione
024400030626  "  C                   exsr      RepPar
024500030626c1668c                   endif
024600000706     C*
024700000706     C                   ENDSR
024800020430C1550 ************************************************************
024900020430  "   * Reperimento descrizione tipo liquidazione se iva mista
025000020430  "   ************************************************************
025100020430  "  C     RieTpLiqD     Begsr
025200020430  "   *
025300020430  "  C                   clear                   TTpLiqD
025400020430  "   *
025500020430  "  C                   if        XscIvaRgm = '2'
025600020430  "   *
025700020430  "  C                   reset                   X07DS
025800020430  "  C                   move      '1'           X07RIC
025900020430  "  C                   movel     'ANDIZ000'    X07TRC
026000020430  "  C                   movel     'DSPIVALIQ'   X07CAM
026100020430  "  C                   movel     TpLiq167      X07VAL
026200020430  "  C                   callb     'X07VALR'
026300020430  "  C                   parm                    X07DS
026400020430  "   *
026500020430  "  C                   if        X07Err = '0'
026600020430  "  C                   movel     X07DES        TTpLiqD
026700020430  "  C                   endif
026800020430  "   *
026900020430  "   * centro la descrizione dentro la variabile in stampa
027000020430  "  C                   eval      TTpLiqD = %trim(TTpLiqD)
027100020430  "  C     ' '           checkr    TTpLiqD       XLenStr
027200020430  "  C                   eval      XLenTot = %size(TTpLiqD)
027300020430  "  C                   call      'X71CENTRA'
027400020430  "  C                   parm                    TTpLiqD
027500020430  "  C                   parm                    XLenStr           3 0
027600020430  "  C                   parm                    XLenTot           3 0
027700020430  "   *
027800020430  "  C                   endif
027900020430  "   *
028000020430C1550C     XRieTpLiqD    Endsr
028100030626c1668C************************************************************
028200030626  "  C* Reperimento parametro modulo intestazione
028300030626c1668C************************************************************
028400030626     C     REPPar        BEGSR
028500030626     c*
028600030626     c* reperimento intestazione
028700030626     c*
028800030626     C                   CALLB     'XPAR'
028900030626     C                   parm                    xscsoc
029000030626     C                   parm      'RIEPIVA'     xpapar            8
029100030626     C                   parm                    xpaout
029200030626     C                   parm      *OFF          xpaerr            1
029300030626     C                   parm                    xparic            1
029400030626     c                   movel     xpaout        ndc099ds
029500030626     c* numero di pagina
029600030626     c                   z-add     NumPag167     NumeroPag         6 0
029700030626     C*
029800030626c1668C                   ENDSR
029900000719     C************************************************************
030000000719     C* FINE PROGRAMMA
030100000719     C************************************************************
030200000719     C     ENDPGM        BEGSR
030300000719     C*
030400030626c1668c                   z-add     NumeroPag     NumPag167
030500000719     C* passaggio dati al pgm chiamante
030600000719     C                   MOVEL     NDC167DS      KPJBU
030700000719     C*
030800100621     C                   IF        %OPEN(ndc167p)
030900100621     C                   CLOSE     ndc167p
031000100621     C                   ENDIF
031100100621     C*
031200000719     C                   SETON                                            RT
031300000719     C                   RETURN
031400000719     C*
031500000719     C                   ENDSR
031600000706     C************************************************************
031700000706     C* DEFINIZIONE KLIST
031800000706     C************************************************************
031900000706     C     DefKlist      BEGSR
032000000706     C*
032100000706     C* klist
032200000706     C*
032300020430C1550X**** k04rip01      KList
032400020430C1550C     k05rip01      KList
032500000719     C                   KFld                    RIPSocieta
032600020430C1550C                   KFld                    RIPTpLiq
032700000719     C                   KFld                    RIPAnno
032800000719     C                   KFld                    RIPPerLiq
032900000719     C                   KFld                    RIPCodCel
033000000706     C*
033100000706     C                   ENDSR
