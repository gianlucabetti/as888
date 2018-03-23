000100030721     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000200030721
000300080703      * FIMS75R *-------------------------------------------------------*
000400080703      *      Scrive da FNBLP a FITR600F   per Etichettatura DPD export  *
000500040520      *       Utilizzo FNBLP in modo da avere le spedizioni disponibili *
000600040520      *       indipendentemente da abbinamenti, trasmissioni ecc. ecc.  *
000700040617      *      Recupero tipo e numero FV da FLTR900F                      *
000800040517      *-----------------------------------------------------------------*
000900030721
001000040520     fFNBLP25L  if   e           k disk
001100030721      *
001200060217     fFiar401L  if   e           k disk
001300040520     fFNBLT01L  if   e           k disk
001400030721      *
001500030721     fTABEL00F  if   e           k disk
001600030721     fAZORG01L  if   e           k disk
001700040616     fFLTR901L  if   e           K disk
001800060616     Fdpcdp01l  IF   E           K DISK
001900060616     Fdpcsc01l  IF   E           K DISK
002000171002     Ffipnd01l  IF   E           K DISK
002100030721      *
002200080703     fFITR600F  o    e             disk
002300030721
002400030721      *---------------------------------------------------------------*
002500030721      *
002600030721      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002700030721      *
002800040616      * - P.O. da elaborare
002900040520     d POel            s                   like(BLPlna)  dim(30)  inz
003000041014      * - Tel e fax del p.o. da elaborare
003100041014     d DTel            s                   like(WDPDTEL)  dim(30)  inz
003200041014     d DFax            s                   like(WDPDFAX)  dim(30)  inz
003300040616      * - FV dei P.O. da elaborare
003400040616     d POnpg           s                   like(TR9npga)  dim(30)  inz
003500040616     d POnfv           s                   like(TR9nfva)  dim(30)  inz
003600140523     d PObau           s                   like(TR6bau)   dim(30)  inz
003700030721      * - di comodo
003800030721     d ws35a           s              1    dim(35) inz
003900030724      * - P.O. elaborati: elenco codici
004000040520     d $PO             s                   like(BLPlna)  dim(100) inz
004100030724      * - P.O. elaborati: dati corrispondenti
004200030724     d $POd            s                   like(AZORGds) dim(100) inz
004300030724      * - Tab.15 = Nazioni: elenco codici elaborati
004400040520     d $15             s                   like(BLPnzd)  dim(200)
004500030724     d                                     inz(*hival)
004600030724      * - Tab.15 = Nazioni: dati corrispondenti
004700030724     d $15ds           s                   like(ds15)    dim(200) inz
004800030724      * - Tab.3A = Codici bolla: elenco codici elaborati
004900040520     d $3A             s                   like(BLPcbo)  dim(50)
005000030724     d                                     inz(*hival)
005100030724      * - Tab.3A = Codici bolla: dati corrispondenti
005200030724     d $3Ads           s                   like(ds3A)    dim(50)  inz
005300030721      *
005400030721      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005500030721      *
005600030721     d Status         sds
005700030721     d   SDSpgm          *proc                                                  Procedure name
005800030721     d   SDSpar          *parms                                                 Num passed parms
005900030721     d   SDSjob              244    253                                         Job name
006000030721     d   SDSusr              254    263                                         User name
006100030721     d   SDSjnr              264    269s 0                                      Job number
006200030721      *
006300030721     d KPJBA         e ds                  inz
006400030721      * - Parametri per pgm. di caricamento schiera p.o.
006500170221     d*Trul06ds      e ds                  inz
006600170221     d** lia                   1     90  0 inz      dim(30)
006700170221      * - Parametri per pgm. di caricamento schiera p.o.
006800170221     d Trul09ds      e ds                  inz
006900170221     d   lia                  19    108  0 inz      dim(30)
007000030721      * - Parametri per pgm. di calcolo check-digit per bar-code
007100030721     d Trul28ds      e ds                  inz
007200030724     d   I28mod      e                     inz('BAR')
007300060616     d
007400060616     d Trul28ds4     e ds                  inz
007500060616     d
007600030721      * - Tab. "3I" = Variabili programmi partenze
007700030721     d ds3I          e ds                  inz
007800030721      * - Tab. "3A" = Codici bolla
007900030721     d ds3A          e ds                  inz
008000030721      * - Tab. "15" = Nazioni
008100030721     d ds15          e ds                  inz
008200040517      * - Descrizione 150 dell'organigramma
008300040517     d Og150         e ds                  inz
008400040517      * - Descrizione 143 dell'organigramma
008500040517     d Og143         e ds                  inz
008600030721     d Og143lnp      e ds                  inz extname(Og143)    prefix(LNP)
008700030721     d Og143lna      e ds                  inz extname(Og143)    prefix(LNA)
008800060616     d
008900060616     d tisie2ds      e ds
009000040520      * - ds per trk FNBLT01L
009100040520     d FNBLTds       e ds                  inz extname(FNBLT00F)
009200060530      * - ds per campo ar4not del file Fiar4 - tipo rec. "I"
009300170926     d*** dsBL4I        e ds                  inz
009400170926     d
009500170929     d dpnddp5       e ds
009600170929     d dpnddp4       e ds
009700170926     d
009800060530      * - ds per campo ar4not del file Fiar4 - tipo rec. "K"
009900060530     d dsBL4k        e ds                  inz
010000030724      * - DS codice a barre per calcolo check-digit
010100030724     d BARcodeDS       ds            18    inz
010200030724     d   BARcdeFLS                    3s 0 inz
010300030724     d   BARcdeLNA                    3s 0 inz
010400030724     d   BARcdeNRS                    2s 0 inz
010500030724     d   BARcdeNSC                    7s 0 inz
010600030724     d   BARcdeZNC                    2s 0 inz
010700030724      * - DS telefono e fax Bartolini da stampare per export DPD
010800030724     d wTelFax         ds                  inz
010900030724     d   wDPDtel                     15    inz
011000030724     d   wDPDfax                     15    inz
011100030724      * - DS per dati utilizzati del trk AZORG01L
011200030724     d AZORGds         ds                  inz
011300030724     d   ORGdes                            inz
011400030724     d   ORGde3                            inz
011500030724     d   ORGde5                            inz
011600030724     d AZORGfls        ds                  inz
011700030724     d   flsORGdes                         like(ORGdes) inz
011800030724     d   flsORGde3                         like(ORGde3) inz
011900030724     d   flsORGde5                         like(ORGde5) inz
012000030724     d AZORGlnp        ds                  inz
012100030724     d   lnpORGdes                         like(ORGdes) inz
012200030724     d   lnpORGde3                         like(ORGde3) inz
012300030724     d   lnpORGde5                         like(ORGde5) inz
012400030724     d AZORGlna        ds                  inz
012500030724     d   lnaORGdes                         like(ORGdes) inz
012600030724     d   lnaORGde3                         like(ORGde3) inz
012700030724     d   lnaORGde5                         like(ORGde5) inz
012800060616     d
012900060616     d wcalltisie2     s              1
013000060616     d ksrvc           s                   like(cscsrvc)
013100030721      *
013200030721      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
013300030721      *
013400030721      * - Parametri per pgm. TISID8R1
013500030721     d ID8rif          s              8  0 inz
013600030721     d OD8ver          s              5  0 inz
013700030721     d OD8des          s              5    inz
013800030721      * - campi di comodo
013900040616     d w0030           s              3  0 inz
014000040616     d w015a           s             15    inz
014100060620     d w016a           s             16    inz
014200060531     d w005a           s              5    inz
014300030721     d $ntw            s                   like(§OGntw) inz
014400040616     d Kfgs            s                   like(TR9fgs) inz
014500040616     d Knfva           s                   like(TR9nfva) inz
014600040616     d Knpga           s                   like(TR9npga) inz
014700030724      * - flag
014800030721     d $ok             s              1    inz(*off)
014900030721      * - indici di schiera / memo progressivi in ciclo DO
015000030721     d xx              s              3  0 inz
015100040616     d yy              s              3  0 inz(1)
015200030728     d xy              s              3  0 inz(1)
015300030721      * - date in formati diversi
015400030721     d Wdate8N         s              8  0 inz
015500030721     d WdateISO        s               d   datfmt(*iso) inz(*loval)
015600030721      * - campi per test "rottura"
015700040520     d wPO             s                   like(BLPlna) inz(*hival)
015800040520     d SAVEcbo         s                   like(BLPcbo) inz(*hival)
015900040520     d SAVEnzd         s                   like(BLPnzd) inz(*hival)
016000030721
016100030721      *===============================================================*
016200030721      *  RIEPILOGO INDICATORI                                         *
016300030721      *---------------------------------------------------------------*
016400030721      * 10    - Comodo                                                *
016500030721      *===============================================================*
016600030721
016700040517      * se non ho trovato p.o. da elaborare vado a fine
016800040517     c     YY            cabeq     *zeros        Fine
016900030721      * Operazioni Iniziali
017000030721     c                   exsr      OPE_INI
017100030721      *
017200040517      * Ciclo di gestione per p.o. da elaborarare
017300030721do  1c                   DO        30            xx
017400030721      *
017500060807if  2***c                   if         poel(xx) =  *zeros  or
017600060807if  2***c                             ponpg(xx) =  *zeros  or
017700060807if  2***c                             ponfv(xx) =  *zeros
017800060807      *  esco (dal ciclo) se manca il P.O.
017900060807if  2c                   if         poel(xx) =  *zeros
018000030721     c                   leave
018100030721e   2c                   endif
018200060807      *  non elaboro se manca NFV
018300060807if  2c                   if        ponpg(xx) =  *zeros  or
018400060807if  2c                             ponfv(xx) =  *zeros
018500060807     c                   iter
018600060807e   2c                   endif
018700060807      *  elaboro
018800041014     c                   reset                   wTelFax
018900041014     c                   movel     dtel(xx)      wdpdtel
019000041014     c                   movel     dfax(xx)      wdpdfax
019100030721      *
019200040520      * - Posizionamento iniziale sul file FNBLP25L
019300030721     c                   exsr      sr_SETLL
019400030721      *
019500030721      * - Ciclo di elaborazione
019600040520do  2c                   dow       NOT %eof(FNBLP25L)
019700030721     c                   exsr      sr_ELAB
019800040520     c     K02BLP25      reade     FNBLP000
019900030721e   2c                   enddo
020000030721      *
020100030721e   1c                   ENDDO
020200030721      *
020300030721      * Fine
020400030721     c     Fine          tag
020500030721     c                   clear                   TRUL28ds
020600030721     c                   eval      I28tla = 'C'
020700030721     c                   call      'TRUL28R1'
020800030721     c                   parm                    TRUL28DS
020900060616     c*
021000060616     c                   if        wcalltisie2='S'
021100060616     c                   clear                   tisie2ds
021200060616     c                   eval      sie2tla='C'
021300060616     c                   call      'TISIE2R'
021400060616     c                   parm                    tisie2ds
021500060616     c                   endif
021600060616     c
021700030721     c                   eval      *inLR  = *on
021800030721
021900030721      *---------------------------------------------------------------*
022000030721      * OPERAZIONI INIZIALI                                           *
022100030721      *---------------------------------------------------------------*
022200030721     c     OPE_INI       BegSr
022300040616     C
022400040616      *
022500040616      * Reperisco dal piano i FV DEFLUENZA per i p.o. da elaborare
022600040616      *
022700040616     C     kFLTR9        setll     FLTR9000
022800040616     C     kFLTR9        reade     FLTR9000
022900040616     c                   dow       NOT %EOF(fltr901L)
023000040616     C                   IF        TR9BAI >= '001'  and  TR9NPG > 0
023100040616     C                                              and  TR9NFV > 0
023200040616     c                   movel     TR9BAI        W0030
023300040616     c                   z-add     1             xx
023400040616     c     W0030         lookup    POel(xx)                               10
023500040616     c                   z-add     TR9npg        POnpg(xx)
023600040616     c                   z-add     TR9nfv        POnfv(xx)
023700140523     c                   movel     TR9pep        PObau(xx)
023800040616     c                   ENDIF
023900040616     C     kFLTR9        reade     FLTR9000
024000040616     c                   ENDDO
024100040616     c
024200040616      *
024300030721      * Reperisco tab. "3I" = variabili pgm partenze
024400030721     c                   clear                   ds3I
024500030721     c                   eval      TBLkut = 1
024600030721     c                   eval      TBLcod = '3I'
024700030721     c                   movel(p)  '1'           TBLkey
024800030721     c     K03tab00      chain     TABEL
024900030721if  1c                   if            %found(TABEL00F)
025000030721     c                             and TBLflg = *blanks
025100030721     c                   movel     TBLuni        ds3I
025200030721e   1c                   endif
025300040517      *
025400040517     c                   EndSr
025500030721      *---------------------------------------------------------------*
025600040520      * POSIZIONAMENTO INIZIALE SUL FILE FNBLP25L                     *
025700030721      *---------------------------------------------------------------*
025800030721     c     sr_SETLL      BegSr
025900030721      *
026000040520     c                   eval      BLPlna = poel(xx)
026100040520     c                   eval      BLPdcm = *zeros
026200040520     c                   movel     Wdate8N       BLPaas
026300040520     c                   move      Wdate8N       BLPmgs
026400030721      *
026500040520     c     K04BLP25      setll     FNBLP000
026600040520     c     K02BLP25      reade     FNBLP000
026700030721      *
026800030721     c                   EndSr
026900030721
027000030721      *---------------------------------------------------------------*
027100040520      * ELABORAZIONE SINGOLO RECORD DEL FILE FNBLP25L                 *
027200030721      *---------------------------------------------------------------*
027300030721     c     sr_ELAB       BegSr
027400030721      *
027500030721      * Pulisco dati reperiti solo in casi specifici
027600040520     c                   clear                   FNBLTds
027700170926     c***                clear                   dsBL4I
027800170926     c                   clear                   dpnddp5
027900170929     c                   clear                   dpnddp4
028000030721      *
028100030721      * Reperisco dati del p.o. segnacollo
028200040520     c                   eval      wPO    =  BLPfls
028300030724     c                   exsr      RepMemoPO
028400030724     c                   movel     $POd(yy)      AZORGfls
028500030721      *
028600030721      * Reperisco dati del p.o. di partenza
028700040520     c                   eval      wPO    =  BLPlnp
028800030724     c                   exsr      RepMemoPO
028900030724     c                   movel     $POd(yy)      AZORGlnp
029000030724     c                   movel     lnpORGde3     Og143lnp
029100030721      *
029200030721      * Reperisco dati del p.o. di arrivo
029300040520     c                   eval      wPO    =  BLPlna
029400030724     c                   exsr      RepMemoPO
029500030724     c                   movel     $POd(yy)      AZORGlna
029600030724     c                   movel     lnaORGde3     Og143lna
029700030721      *
029800030721      * Reperisco tabella Nazioni
029900040520if  1c                   if        BLPnzd    <> SAVEnzd
030000040520     c                   eval      SAVEnzd   =  BLPnzd
030100030724     c                   exsr      RepMemoT15
030200030721e   1c                   endif
030300030721      *
030400030721      * Reperisco tabella Codici bolla
030500040520if  1c                   if        BLPcbo    <> SAVEcbo
030600040520     c                   eval      SAVEcbo   =  BLPcbo
030700030724     c                   exsr      RepMemoT3A
030800030721e   1c                   endif
030900030721      *
031000080703      * Preparo e scrivo il record in FITR600F
031100030721     c                   exsr      WriteFLTR6
031200030721      *
031300030721     c                   EndSr
031400030724
031500030724      *---------------------------------------------------------------*
031600030724      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL P.O.                  *
031700030724      *---------------------------------------------------------------*
031800030724     c     RepMemoPO     BegSr
031900030724      *
032000030724      * Controllo se diverso con l'ultimo reperito
032100030724if  1c                   if        wPO    <> $PO(yy)
032200030724      *
032300030724      * Controllo se già elaborato
032400030724     c                   eval      *in10   = *off
032500030724     c                   eval      yy      = 1
032600030724     c     wPO           lookup    $PO(yy)                                10
032700030724      *
032800030724      * Reperimento dati se non ancora elaborato
032900030724if  2c                   if        not *in10
033000030724      *
033100030724     c     wPO           chain     AZORG
033200030724if  3c                   if        not %found(AZORG01L)
033300030724     c                             or  ORGfva <> *blanks
033400030724     c                   clear                   AZORGds
033500030724e   3c                   endif
033600030724      * - Memorizzazione dati:
033700030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
033800030724     c     *zeros        lookup    $PO(yy)                                10
033900030724if  3c                   if        not *in10
034000030724     c                   eval      yy      = %elem($PO)
034100030724e   3c                   endif
034200030724     c                   eval      $PO(yy) = wPO
034300030724     c                   movel     AZORGds       $POd(yy)
034400030724      *
034500030724e   2c                   endif
034600030724      *
034700030724e   1c                   endif
034800030724      *
034900030724     c                   EndSr
035000030724
035100030724      *---------------------------------------------------------------*
035200030724      * REPERIMENTO (E MEMORIZZAZIONE) TAB.15 = NAZIONI               *
035300030724      *---------------------------------------------------------------*
035400030724     c     RepMemoT15    BegSr
035500030724      *
035600030724      * Controllo se già elaborato
035700030724     c                   eval      *in10   =  *off
035800030728     c                   eval      xy      =  1
035900030728     c     SAVEnzd       lookup    $15(xy)                                10
036000030724      *
036100030724      * Reperimento dati se non ancora elaborato
036200030728if  1c                   if        not *in10
036300030724      *
036400030724     c                   clear                   ds15
036500030724     c                   eval      TBLcod = '15'
036600030724     c                   movel(p)  SAVEnzd       TBLkey
036700030724     c     K03tab00      chain     TABEL
036800030728if  2c                   if        %found(TABEL00F)
036900030724     c                             and TBLflg = *blanks
037000030724     c                   movel     TBLuni        ds15
037100030728e   2c                   endif
037200030724      * - Memorizzazione dati:
037300030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
037400030728     c     *hival        lookup    $15(xy)                                10
037500030728if  2c                   if        not *in10
037600030728     c                   eval      xy      =  %elem($15)
037700030728e   2c                   endif
037800030728     c                   eval      $15(xy) =  SAVEnzd
037900030728     c                   movel     ds15          $15ds(xy)
038000030728      *
038100030728      * RE-impostazione dati se già elaborato
038200030728x   1c                   else
038300030728      *
038400030728     c                   movel     $15ds(xy)     ds15
038500030724      *
038600030724e   1c                   endif
038700030724      *
038800030724     c                   EndSr
038900030724
039000030724      *---------------------------------------------------------------*
039100030724      * REPERIMENTO (E MEMORIZZAZIONE) TAB.3A = CODICI BOLLA          *
039200030724      *---------------------------------------------------------------*
039300030724     c     RepMemoT3A    BegSr
039400030724      *
039500030724      * Controllo se già elaborato
039600030724     c                   eval      *in10   =  *off
039700030728     c                   eval      xy      =  1
039800030728     c     SAVEcbo       lookup    $3A(xy)                                10
039900030724      *
040000030724      * Reperimento dati se non ancora elaborato
040100030728if  1c                   if        not *in10
040200030724      *
040300030724     c                   clear                   ds3A
040400030724     c                   eval      TBLcod = '3A'
040500030724     c                   movel(p)  SAVEcbo       TBLkey
040600030724     c     K03tab00      chain     TABEL
040700030728if  2c                   if        %found(TABEL00F)
040800030724     c                             and TBLflg = *blanks
040900030724     c                   movel     TBLuni        ds3A
041000030728e   2c                   endif
041100030724      * - Memorizzazione dati:
041200030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
041300030728     c     *hival        lookup    $3A(xy)                                10
041400030728if  2c                   if        not *in10
041500030728     c                   eval      xy      =  %elem($3A)
041600030728e   2c                   endif
041700030728     c                   eval      $3A(xy) =  SAVEcbo
041800030728     c                   movel     ds3A          $3Ads(xy)
041900030728      *
042000030728      * RE-impostazione dati se già elaborato
042100030728x   1c                   else
042200030728      *
042300030728     c                   movel     $3Ads(xy)     ds3A
042400030724      *
042500030724e   1c                   endif
042600030724      *
042700030724     c                   EndSr
042800030721
042900030721      *---------------------------------------------------------------*
043000080703      * PREPARAZIONE E SCRITTURA DEL RECORD IN FITR600F               *
043100030721      *---------------------------------------------------------------*
043200030721     c     WriteFLTR6    BegSr
043300030721      *
043400080703     c                   clear                   FITR6
043500080703     c
043600080703     c                   eval      tr6fil=filiale
043700080703     c                   eval      tr6cpc=codicepc
043800171016
043900171016     c                   eval      tr6tim=%timestamp
044000030721      *
044100030721      * segnacollo "chi sono"
044200030721      * (imposto i dati validi per tutti i segnacolli della spedizione;
044300030721      *  il pgm. TRUL28R verrà richiamato per ogni collo)
044400030721     c                   clear                   BarCodeDS
044500040520     c                   eval      BARcdeFLS = BLPfls
044600040520     c                   eval      BARcdeLNA = BLPlna
044700040520     c                   eval      BARcdeNRS = BLPnrs
044800040520     c                   eval      BARcdeZNC = BLPznc
044900030721      * secondi di attesa dopo stampa
045000030721     c                   eval      TR6sea    =  §3Isea
045100030721      * flag segnacollo da stampare ('S'/' ')
045200030721     c                   eval      TR6st1    =  'S'
045300030721      * flags
045400030721     c                   eval      TR6fl1    =  *off
045500030721      * codice cliente
045600040520if  1c                   if        BLPccm    >  *zeros
045700040520     c                   eval      TR6ksc    =  BLPccm
045800030721x   1c                   else
045900040520     c                   eval      TR6ksc    =  BLPksc
046000030721e   1c                   endif
046100030721      * p.o. partenza segnacollo
046200040520     c                   eval      TR6fls    =  BLPfls
046300030721      * descrizione del p.o. partenza segnacollo
046400030721     c                   eval      TR6dlp    =  flsORGdes
046500030721      * codice prodotto - default
046600040520     c                   eval      TR6cdp    =  BLPrma
046700030721      * numero serie segnacollo
046800040520     c                   eval      TR6nrs    =  BLPnrs
046900030721      * p.o. di arrivo
047000040520     c                   eval      TR6lna    =  BLPlna
047100030721      * descrizione p.o. arrivo spedizione
047200030721if  1c                   if        lnaORGde5 <> *blanks
047300030721     c                   eval      TR6dla    =  lnaORGde5
047400030721x   1c                   else
047500030721     c                   eval      TR6dla    =  lnaORGdes
047600030721e   1c                   endif
047700030721      * terminal di arrivo
047800040520     c                   eval      TR6tna    =  BLPtfa
047900030721      * tipo servizio bolle
048000040520     c                   eval      TR6tsp    =  BLPtsp
048100030721      * zona segnacollo
048200040520     c                   eval      TR6zsc    =  BLPznc
048300030721      * totale segnacolli della spedizione
048400040520     c                   eval      TR6nc2    =  BLPncl
048500030721      * ragione sociale mittente
048600040520     c                   eval      TR6rsm    =  BLPrsm
048700030721      * provincia mittente
048800040520     c                   eval      TR6prm    =  BLPprm
048900030721      * ragione sociale destinatario
049000040520     c                   eval      TR6rsd    =  BLPrsd
049100030721      * c.a.p. destinatario
049200040520     c                   eval      TR6cad    =  BLPcad
049300030721      * indirizzo destinatario
049400040520     c                   eval      TR6ind    =  BLPind
049500030721      * località destinatario
049600040520     c                   eval      TR6lod    =  BLPlod
049700030721      * provincia destinatario
049800040520     c                   eval      TR6prd    =  BLPprd
049900030721      * volume
050000040520     c                   eval      TR6vol    =  BLPvlf
050100030721      * peso in kg
050200040520     c                   eval      TR6pkg    =  BLPpkf
050300030721      * flag volume bollettato
050400040520     c                   eval      TR6fvr    =  BLPfvf
050500030721      * anno spedizione
050600040520     c                   eval      TR6aas    =  BLPaas
050700030721      * meso/giorno spedizione
050800040520     c                   eval      TR6mgs    =  BLPmgs
050900030730      *
051000040518      * tipo stampa:
051100030730     c                   eval      TR6tst    =  'EST'
051200030721      * codice iso nazione arrivo
051300060616     c*****              eval      TR6isa    =  §15cod
051400030721      * flag S=contrassegno; A=assegnato, X=c/assegno+assegnato
051500030730sel 2c                   select
051600030730w   2c                   when      %subst(§3Atb1:1:1) = 'A'
051700030721     c                             and    §3Afca      = *zeros
051800030721     c                   eval      TR6fca    =  'A'
051900030730w   2c                   when      %subst(§3Atb1:1:1) = 'A'
052000030721     c                             and    §3Afca     <> *zeros
052100030721     c                   eval      TR6fca    =  'X'
052200030730w   2c                   when      §3Afca    <> *zeros
052300030721     c                   eval      TR6fca    =  'S'
052400030730e   2c                   endsl
052500040617      * Tipo e numero FV
052600040617if  2c                   z-add     ponpg(xx)     TR6npg
052700040617if  2c                   z-add     ponfv(xx)     TR6nfv
052800140523      * Baia di smistamento
052900140523if  2c                   movel     pobau(xx)     TR6bau
053000030721      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
053100030721      *
053200030721      * cicli di WRITE dei record (1 x segnacollo)
053300030721sel 1c                   select
053400040520w   1c                   when      BLPncd >  *zeros
053500030721     c                   exsr      sr_SCS
053600040520w   1c                   when      BLPfns =  'S'
053700030721     c                   exsr      sr_SCNS
053800030721e   1c                   endsl
053900030721      *
054000030721     c                   EndSr
054100030721
054200030721      *---------------------------------------------------------------*
054300030721      * Registrazione elenco segnacolli sequenziali                   *
054400030721      *---------------------------------------------------------------*
054500030721     c     sr_SCS        BegSr
054600030721      *
054700030721     c                   clear                   TR6nc1
054800040520     c                   eval      TR6ncd    =  BLPncd - 1
054900040520if  1c                   if        BLPnca    =  *zeros
055000040520     c                   eval      BLPnca    =  BLPncd
055100030721e   1c                   endif
055200030721      *
055300030721      * Ciclo di calcolo segnacolli sequenziali
055400040520do  1c                   DOW       TR6ncd    <  BLPnca
055500030721      *
055600030721      * - numero segnacollo
055700030721     c                   eval      TR6ncd    =  TR6ncd + 1
055800030721      * - progressivo del segnacollo in esame
055900030721     c                   eval      TR6nc1    =  TR6nc1 + 1
056000030728      * - impostazione campi "specifici" per DPD
056100030728     c                   exsr      Dati_DPD
056200030721      * - calcolo il "chi sono" (comprensivo di check-digit)
056300030721     c                   exsr      sr_CHISONO
056400030721      *
056500080703     c                   WRITE(E)  FITR6
056600030721      *
056700030721e   1c                   ENDDO
056800030721      *
056900030721     c                   EndSr
057000030721
057100030721      *---------------------------------------------------------------*
057200030721      * Registrazione elenco segnacolli NON sequenziali               *
057300030721      *---------------------------------------------------------------*
057400030721     c     sr_SCNS       BegSr
057500030721      *
057600030721     c                   clear                   TR6nc1
057700030721      *
057800030721      * Ciclo di repermento segnacolli NON sequenziali
057900040520     c     k04BLT01      setll     FNBLT000
058000040520     c     k04BLT01      reade     FNBLT000
058100030721      *
058200040520do  1c                   DOW       NOT %eof(FNBLT01L)
058300030721      *
058400030721      * - numero segnacollo
058500040520     c                   eval      TR6ncd    =  BLTnsc
058600030721      * - progressivo del segnacollo in esame
058700030721     c                   eval      TR6nc1    =  TR6nc1 + 1
058800030728      * - impostazione campi "specifici" per DPD
058900030728     c                   exsr      Dati_DPD
059000030721      * - calcolo il "chi sono" (comprensivo di check-digit)
059100030721     c                   exsr      sr_CHISONO
059200030721      *
059300080703     c                   WRITE(E)  FITR6
059400030721      *
059500040520     c     k04BLT01      reade     FNBLT000
059600030721      *
059700030721e   1c                   ENDDO
059800030721      *
059900030721     c                   EndSr
060000030728
060100030728      *---------------------------------------------------------------*
060200030728      * Impostazione dati specifici per DPD                           *
060300030728      *---------------------------------------------------------------*
060400040518     C     Dati_DPD      BEGSR
060500030728      *
060600170926     c***                eval      ar4trc    =  'I'
060700170926     c***  K05ar401      chain     Fiar4000
060800170926
060900170926     c     K04blt01      chain     Fipnd01l
061000170926if  2c                   if        %found(Fipnd01L)
061100170926     c****               movel     ar4not        dsBL4I
061200170926
061300030728      *
061400170926if  2c****               IF        §B4ipn    <> *blanks
061500170926
061600171016     c*************
061700171016     c* D  P  4
061800171016     c*************
061900171002   2ac                   if        PNDDPE    =  'DP4'
062000170929     c                   movel     PNDETIU       dpnddp4
062100030728      *
062200060616      * - sigla servizio
062300060616if  3c                   if        BLPpkf    <= 3
062400060616     c                   eval      TR6sse    =  'X'
062500060616e   3c                   endif
062600060616     c
062700060616     c* Nuovo  cappario Bartolini
062800060616     c
062900060616     c                   eval      TR6tse    =  'DP4'
063000060616     c
063100060616     c                   movel(P)  wDPDfax       TR6CDP
063200060616     c* reperisco versione cappario DPD
063300060616     c                   clear                   tisie2ds
063400060616     c                   z-add     blpmgs        sie2dri
063500060616     c                   movel     blpaas        sie2dri
063600060616     c                   call      'TISIE2R'
063700060616     c                   parm                    TIsie2ds
063800060616     c                   eval      wcalltisie2='S'
063900060616     c
064000060616     c* chain su file dpcdp01l per prendere nazione depot
064100060616     c     kcdp          chain     dpcdp01l
064200060616     c                   if        %found(dpcdp01l)
064300060616     c                   movel     cdpiso2       tr6SIA
064400060616     c                   endif
064500061010     c                   eval      tr6awb= §b4iat + §b4ipy
064600060616     c* TR6ISA solo per DP4 asteriscato da altre parti
064700060616     c                   movel     §15cod        tr6ISA
064800060616     c
064900060616     c                   eval      %subst(tr6eur:1:1) = §b4ibi
065000060616     c* richiamo pgm per avere il cap da stampare come vuole dpd
065100060616     c                   clear                   outcapdpd
065200060616     c                   call      'XCAPDPD'
065300060616     c                   parm                    blpcad
065400060616     c                   parm                    outcapdpd         7
065500061016     c                   parm      blpnzd        inpnzd            3
065600060616     c
065700060616     c                   eval      %subst(tr6eur:2:7) = %subst(outcapdpd:1:7)
065800060616     c* Se il numero parcel è lungo 11, devo portarlo a 14
065900170926
066000170926     c* ES 10/17 --> non prevedo più il parcel lungo 11
066100170926     c**                 if        %subst(§b4ipn:12:1)=' '
066200170926     c**                 eval      %subst(tr6eur:9:1) = '0'
066300170926     c**                 eval      %subst(tr6eur:10:3)=%subst(§b4ipn:1:3)
066400170926     c**                 eval      %subst(tr6eur:13:2)='99'
066500170926     c**                 eval      %subst(tr6eur:15:8)=%subst(§b4ipn:4:8)
066600170926     c**                 else
066700170926     c                   eval      %subst(tr6eur:9:14)=pndipn
066800170926     c**                 endif
066900060616     c
067000170926     c                   eval      %subst(tr6eur:23:3)=pndisc
067100060616     c                   eval      %subst(tr6eur:26:3)=%editc(§15cie:'X')
067200060616     c* §DESIP solo per DP4
067300060616     c                   move      §b4icd        tr6SIP
067400060616     c                   clear                   trul28ds4
067500060616     c                   eval      i284cod=%subst(tr6EUR:2:27)
067600060616     c                   call      'TRUL28R5'
067700060616     c                   parm                    trul28ds4
067800060616     c                   movel     o284ckd       tr6SIP
067900060616     c
068000060616     c                   eval      %subst(tr6XX1:1:7)=sie2verd
068100060616     c                   eval      %subst(tr6XX1:8:4)=§b4ida
068200060616     c                   eval      %subst(tr6XX1:12:4)=§b4ids
068300060616     c
068400060616     c                   movel     wDPDtel       TR6xx2
068500170926     c                   movel     pndisc        ksrvc
068600060616     c     kcsc          chain     dpcsc01l
068700060616     c                   if        %found(dpcsc01l)
068800060620     c                   evalr     W016A = %TRIM(cscsrvd)
068900060620     c                   move      W016A         tr6XX2
069000060616     c                   endif
069100060616     c*
069200171002e  2ac                   ENDIF
069300170926     c
069400171016     c*************
069500171016     c* D  P  5
069600171016     c* Nuova etichetta
069700171016     c*************
069800171002   2ac                   if        PNDDPE    =  'DP5'
069900171016
070000171009     c                   eval      TR6tse    =  'DP5'
070100171013     c                   movel(P)  wDPDfax       TR6CDP
070200170929     c                   movel     PNDETIU       dpnddp5
070300170929     c
070400170926     c                   eval      %subst(tr6eur:1:1)=§PNDIBI
070500170926     c                   eval      %subst(tr6eur:2:7) = §PNDCADP
070600170926     c                   eval      %subst(tr6eur:9:14)=pndipn
070700170926     c                   eval      %subst(tr6eur:23:3)=pndisc
070800170926     c                   eval      %subst(tr6eur:26:3)=§pndNTWC
070900170926
071000170926     c                   eval      %subst(tr6XX1:1:8)=§PNDVERD
071100170927     c                   eval      %subst(tr6XX1:9:4)=§pndidast
071200170927
071300170927     c                   eval      tr6sse =§PNDDSRVM
071400170927
071500170927     c                   eval      %subst(tr6awb:1:5)=§PNDSSORT
071600170927     c                   eval      %subst(tr6awb:6:3)=§PNDBUCA
071700170927
071800170927     c                   clear                   TR6xx2
071900170927     c                   eval      %subst(tr6XX2:1:15)=wdpdtel
072000170927     c                   eval      %subst(tr6XX2:16:18)=§PNDDSRVX
072100170927
072200170927     c                   eval      tr6isa =§PNDISO2
072300170927
072400170927     c                   eval      tr6sia =§PNDDDPTC
072500170927
072600170929     c                   eval      %subst(tr6sip:1:1) =§PNDICBCD
072700170929     c                   eval      %subst(tr6sip:3:1) =§PNDICD
072800170929     c
072900170929     c
073000170929     c                   eval      %subst(tr6dta:1:5)  =§PNDDSORT
073100171016     c* Csorto Osort .............. previsti ma per ora vuoti
073200170929     c                   eval      %subst(tr6dta:6:5)  =§PNDoSORT
073300170929     c                   eval      %subst(tr6dta:11:5) =§PNDcSORT
073400171002     c* additional service code ... previsto ma per ora vuoto
073500171002     c                   eval      %subst(tr6dta:16:6) =§PNDasco
073600171002     c                   eval      %subst(tr6dta:22:35) =§PNDref
073700171002     c                   eval      %subst(tr6dta:57:16) =§PNDteld
073800171002     c                   eval      %subst(tr6dta:73:40) =§PNDdsi
073900170929
074000171002e  2ac                   ENDIF
074100171002e   2c                   ENDIF
074200030728      *
074300030728     c                   EndSr
074400030728
074500030721      *---------------------------------------------------------------*
074600030721      * Impostazione del segnacollo "CHI SONO" (con check digit)      *
074700030721      *---------------------------------------------------------------*
074800030721     c     sr_CHISONO    BegSr
074900030721      *
075000030721      * Costruisco il codice a barre: FLS+LNA+NRS+NSC+ZNC+CKD
075100030721      * Mancano solo NSC e CKD:
075200030721      * - NSC l'ho appena calcolato in TR6ncd
075300030721      * - chiamo TRUL28R per ricavarne il check-digit
075400030721     c                   eval      BARcdeNSC =  TR6ncd
075500030721     c                   reset                   TRUL28ds
075600030721     c                   movel     BarCodeDS     I28cod
075700030721     c                   call      'TRUL28R1'
075800030721     c                   parm                    TRUL28DS
075900030721if  1c                   if        O28err    =  *blanks
076000030721     c                   movel     O28cod        TR6who
076100030721e   1c                   endif
076200030721      *
076300030721     c                   EndSr
076400040517      *---------------------------------------------------------------*
076500040517      * SBR iniziale
076600040517      *---------------------------------------------------------------*
076700040517     c     *INZSR        BegSr
076800040517      *
076900040517     c     *Entry        plist
077000080703     c                   parm                    codicepc          1
077100080703     c                   parm                    filiale           3
077200080703     c                   parm                    catfoglio         1
077300080703     c                   parm                    numfoglio         7
077400080703     c                   parm                    sysnam            8
077500040616      *
077600040616      * Imposto campi KEY fltr9
077700080703     c                   move      filiale       Kfgs
077800080703     c                   move      catfoglio     Knpga
077900080703     c                   move      numfoglio     Knfva
078000040616      *
078100040616      * Imposto la data minima da elaborare
078200050118      *    in "buona" - 30 gg da oggi
078300050118      *    in "test" - 600 gg da oggi
078400040616     c                   move      *date         WdateISO
078500080703     c                   IF        sysnam='AS888   '
078600050118     c                   subdur    600:*d        WdateISO
078700050118     c                   ELSE
078800050118     c                   subdur    30:*d         WdateISO
078900050118     c                   ENDIF
079000040616     c                   movel     WdateISO      Wdate8N
079100040616      * Imposto la KPJBA
079200040616     c                   eval      KNMUS = SDSusr
079300040616     c                   eval      KNMTD = SDSjob
079400040616     c                   eval      KNRJO = SDSjnr
079500040616     c                   eval      KNMEB = SDSjob
079600040517      *
079700040517      * Carico tabella P.O. gestiti
079800170221     c***                movel     'PP'          D06cod
079900170221     c***                movel     'L'           D06tla
080000170221     c***                eval      D06key = filiale
080100170221     c***                movel(p)  Trul06ds      KPJBU
080200170221     c***                call      'TRUL06R'
080300170221     c***                parm                    KPJBA
080400170221     c***                movel     KPJBU         Trul06ds
080500170221     c                   clear                   Trul09ds
080600170221     c                   movel     'L'           i09tla
080700170221     c                   movel     'DEF'         i09mod
080800170221     c                   movel     filiale       i09tfa
080900170221     c                   call      'TRUL09R'
081000170221     c                   parm                    trul09ds
081100040517      *
081200040517      * Elaboro solo linee DPD con abilitatata stamba barcode in arrivo
081300040517     c                   clear                   yy
081400040517do  1c                   DO        30            xx
081500040517if  2c                   if        Lia(xx) = *zeros
081600040517     c                   leave
081700040517e   2c                   endif
081800040517     c     Lia(xx)       chain     AZORG
081900040517if  2c                   if        %found(AZORG01L) and ORGfva = *blanks
082000040517     c                   movel     ORGde3        OG143
082100040517     c                   movel     ORGdf0        OG150
082200040629if  2c                   if        §ogntw = 'DPD'
082300040517     c                   add       1             yy
082400040517     c                   movel     orgfil        POel(yy)
082500041014     c                   movel     orgtel        DTel(yy)
082600041014     c                   movel     orgfax        DFax(yy)
082700040517e   2c                   endif
082800040517e   2c                   endif
082900040517e   1c                   ENDDO
083000040517      *
083100040517      * Key-List
083200040616      *
083300040616      * - file FLTR901L
083400040616     c     KFLTR9        klist
083500040616     c                   kfld                    Kfgs
083600040616     c                   kfld                    Knpga
083700040616     c                   kfld                    Knfva
083800040517      *
083900040520      * - file FNBLP25L
084000040520     c     K04BLP25      klist
084100040520     c                   kfld                    BLPlna
084200040520     c                   kfld                    BLPdcm
084300040520     c                   kfld                    BLPaas
084400040520     c                   kfld                    BLPmgs
084500040520     c     K02BLP25      klist
084600040520     c                   kfld                    BLPlna
084700040520     c                   kfld                    BLPdcm
084800060217      * - file Fiar401L
084900060217     c     K05ar401      klist
085000040520     c                   kfld                    BLPaas
085100040520     c                   kfld                    BLPlnp
085200040520     c                   kfld                    BLPnrs
085300040520     c                   kfld                    BLPnsp
085400060217     c                   kfld                    ar4trc
085500040520      * - file FNBLT01L
085600040520     c     K04BLT01      klist
085700040520     c                   kfld                    BLPaas
085800040520     c                   kfld                    BLPlnp
085900040520     c                   kfld                    BLPnrs
086000040520     c                   kfld                    BLPnsp
086100040517      * - file TABEL00F
086200040517     c     K03tab00      klist
086300040517     c                   kfld                    TBLkut
086400040517     c                   kfld                    TBLcod
086500040517     c                   kfld                    TBLkey
086600060616     C     KCDP          KLIST
086700060616     C                   KFLD                    sie2ver
086800060616     C                   KFLD                    §b4ida
086900060616     C     KCSC          KLIST
087000060616     C                   KFLD                    sie2ver
087100060616     C                   KFLD                    ksrvc
087200040517      *
087300040517     c                   EndSr
