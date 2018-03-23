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
002000030721      *
002100080703     fFITR600F  o    e             disk
002200030721
002300030721      *---------------------------------------------------------------*
002400030721      *
002500030721      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002600030721      *
002700040616      * - P.O. da elaborare
002800040520     d POel            s                   like(BLPlna)  dim(30)  inz
002900041014      * - Tel e fax del p.o. da elaborare
003000041014     d DTel            s                   like(WDPDTEL)  dim(30)  inz
003100041014     d DFax            s                   like(WDPDFAX)  dim(30)  inz
003200040616      * - FV dei P.O. da elaborare
003300040616     d POnpg           s                   like(TR9npga)  dim(30)  inz
003400040616     d POnfv           s                   like(TR9nfva)  dim(30)  inz
003401140523     d PObau           s                   like(TR6bau)   dim(30)  inz
003500030721      * - di comodo
003600030721     d ws35a           s              1    dim(35) inz
003700030724      * - P.O. elaborati: elenco codici
003800040520     d $PO             s                   like(BLPlna)  dim(100) inz
003900030724      * - P.O. elaborati: dati corrispondenti
004000030724     d $POd            s                   like(AZORGds) dim(100) inz
004100030724      * - Tab.15 = Nazioni: elenco codici elaborati
004200040520     d $15             s                   like(BLPnzd)  dim(200)
004300030724     d                                     inz(*hival)
004400030724      * - Tab.15 = Nazioni: dati corrispondenti
004500030724     d $15ds           s                   like(ds15)    dim(200) inz
004600030724      * - Tab.3A = Codici bolla: elenco codici elaborati
004700040520     d $3A             s                   like(BLPcbo)  dim(50)
004800030724     d                                     inz(*hival)
004900030724      * - Tab.3A = Codici bolla: dati corrispondenti
005000030724     d $3Ads           s                   like(ds3A)    dim(50)  inz
005100030721      *
005200030721      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
005300030721      *
005400030721     d Status         sds
005500030721     d   SDSpgm          *proc                                                  Procedure name
005600030721     d   SDSpar          *parms                                                 Num passed parms
005700030721     d   SDSjob              244    253                                         Job name
005800030721     d   SDSusr              254    263                                         User name
005900030721     d   SDSjnr              264    269s 0                                      Job number
006000030721      *
006100030721     d KPJBA         e ds                  inz
006200030721      * - Parametri per pgm. di caricamento schiera p.o.
006300170221     d*Trul06ds      e ds                  inz
006400170221     d** lia                   1     90  0 inz      dim(30)
006401170221      * - Parametri per pgm. di caricamento schiera p.o.
006402170221     d Trul09ds      e ds                  inz
006403170221     d   lia                  19    108  0 inz      dim(30)
006500030721      * - Parametri per pgm. di calcolo check-digit per bar-code
006600030721     d Trul28ds      e ds                  inz
006700030724     d   I28mod      e                     inz('BAR')
006800060616     d
006900060616     d Trul28ds4     e ds                  inz
007000060616     d
007100030721      * - Tab. "3I" = Variabili programmi partenze
007200030721     d ds3I          e ds                  inz
007300030721      * - Tab. "3A" = Codici bolla
007400030721     d ds3A          e ds                  inz
007500030721      * - Tab. "15" = Nazioni
007600030721     d ds15          e ds                  inz
007700040517      * - Descrizione 150 dell'organigramma
007800040517     d Og150         e ds                  inz
007900040517      * - Descrizione 143 dell'organigramma
008000040517     d Og143         e ds                  inz
008100030721     d Og143lnp      e ds                  inz extname(Og143)    prefix(LNP)
008200030721     d Og143lna      e ds                  inz extname(Og143)    prefix(LNA)
008300060616     d
008400060616     d tisie2ds      e ds
008500040520      * - ds per trk FNBLT01L
008600040520     d FNBLTds       e ds                  inz extname(FNBLT00F)
008700060217      * - ds per campo ar4not del file Fiar4 - tipo rec. "F"
008800060530     d*dsBL4F        e ds                  inz
008900060530      * - ds per campo ar4not del file Fiar4 - tipo rec. "I"
009000060530     d dsBL4I        e ds                  inz
009100060530      * - ds per campo ar4not del file Fiar4 - tipo rec. "K"
009200060530     d dsBL4k        e ds                  inz
009300030724      * - DS codice a barre per calcolo check-digit
009400030724     d BARcodeDS       ds            18    inz
009500030724     d   BARcdeFLS                    3s 0 inz
009600030724     d   BARcdeLNA                    3s 0 inz
009700030724     d   BARcdeNRS                    2s 0 inz
009800030724     d   BARcdeNSC                    7s 0 inz
009900030724     d   BARcdeZNC                    2s 0 inz
010000030724      * - DS telefono e fax Bartolini da stampare per export DPD
010100030724     d wTelFax         ds                  inz
010200030724     d   wDPDtel                     15    inz
010300030724     d   wDPDfax                     15    inz
010400030724      * - DS telefono e fax Bartolini da stampare per export DPD
010500060531     d*wXX1dx          ds                  inz
010600060531     d*  wB4rtn                            like(§B4rtn) inz
010700060531     d*  wB4zon                            like(§B4zon) inz
010800030724      * - DS per dati utilizzati del trk AZORG01L
010900030724     d AZORGds         ds                  inz
011000030724     d   ORGdes                            inz
011100030724     d   ORGde3                            inz
011200030724     d   ORGde5                            inz
011300030724     d AZORGfls        ds                  inz
011400030724     d   flsORGdes                         like(ORGdes) inz
011500030724     d   flsORGde3                         like(ORGde3) inz
011600030724     d   flsORGde5                         like(ORGde5) inz
011700030724     d AZORGlnp        ds                  inz
011800030724     d   lnpORGdes                         like(ORGdes) inz
011900030724     d   lnpORGde3                         like(ORGde3) inz
012000030724     d   lnpORGde5                         like(ORGde5) inz
012100030724     d AZORGlna        ds                  inz
012200030724     d   lnaORGdes                         like(ORGdes) inz
012300030724     d   lnaORGde3                         like(ORGde3) inz
012400030724     d   lnaORGde5                         like(ORGde5) inz
012500060616     d
012600060616     d wcalltisie2     s              1
012700060616     d ksrvc           s                   like(cscsrvc)
012800030721      *
012900030721      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
013000030721      *
013100030721      * - Parametri per pgm. TISID8R1
013200030721     d ID8rif          s              8  0 inz
013300030721     d OD8ver          s              5  0 inz
013400030721     d OD8des          s              5    inz
013500030721      * - campi di comodo
013600040616     d w0030           s              3  0 inz
013700040616     d w015a           s             15    inz
013800060620     d w016a           s             16    inz
013900060531     d w005a           s              5    inz
014000030721     d $ntw            s                   like(§OGntw) inz
014100040616     d Kfgs            s                   like(TR9fgs) inz
014200040616     d Knfva           s                   like(TR9nfva) inz
014300040616     d Knpga           s                   like(TR9npga) inz
014400030724      * - flag
014500030721     d $ok             s              1    inz(*off)
014600030721      * - indici di schiera / memo progressivi in ciclo DO
014700030721     d xx              s              3  0 inz
014800040616     d yy              s              3  0 inz(1)
014900030728     d xy              s              3  0 inz(1)
015000030721      * - date in formati diversi
015100030721     d Wdate8N         s              8  0 inz
015200030721     d WdateISO        s               d   datfmt(*iso) inz(*loval)
015300030721      * - campi per test "rottura"
015400040520     d wPO             s                   like(BLPlna) inz(*hival)
015500040520     d SAVEcbo         s                   like(BLPcbo) inz(*hival)
015600040520     d SAVEnzd         s                   like(BLPnzd) inz(*hival)
015700030721
015800030721      *===============================================================*
015900030721      *  RIEPILOGO INDICATORI                                         *
016000030721      *---------------------------------------------------------------*
016100030721      * 10    - Comodo                                                *
016200030721      *===============================================================*
016300030721
016400040517      * se non ho trovato p.o. da elaborare vado a fine
016500040517     c     YY            cabeq     *zeros        Fine
016600030721      * Operazioni Iniziali
016700030721     c                   exsr      OPE_INI
016800030721      *
016900040517      * Ciclo di gestione per p.o. da elaborarare
017000030721do  1c                   DO        30            xx
017100030721      *
017200060807if  2***c                   if         poel(xx) =  *zeros  or
017300060807if  2***c                             ponpg(xx) =  *zeros  or
017400060807if  2***c                             ponfv(xx) =  *zeros
017500060807      *  esco (dal ciclo) se manca il P.O.
017600060807if  2c                   if         poel(xx) =  *zeros
017700030721     c                   leave
017800030721e   2c                   endif
017900060807      *  non elaboro se manca NFV
018000060807if  2c                   if        ponpg(xx) =  *zeros  or
018100060807if  2c                             ponfv(xx) =  *zeros
018200060807     c                   iter
018300060807e   2c                   endif
018400060807      *  elaboro
018500041014     c                   reset                   wTelFax
018600041014     c                   movel     dtel(xx)      wdpdtel
018700041014     c                   movel     dfax(xx)      wdpdfax
018800030721      *
018900040520      * - Posizionamento iniziale sul file FNBLP25L
019000030721     c                   exsr      sr_SETLL
019100030721      *
019200030721      * - Ciclo di elaborazione
019300040520do  2c                   dow       NOT %eof(FNBLP25L)
019400030721     c                   exsr      sr_ELAB
019500040520     c     K02BLP25      reade     FNBLP000
019600030721e   2c                   enddo
019700030721      *
019800030721e   1c                   ENDDO
019900030721      *
020000030721      * Fine
020100030721     c     Fine          tag
020200030721     c                   clear                   TRUL28ds
020300030721     c                   eval      I28tla = 'C'
020400030721     c                   call      'TRUL28R1'
020500030721     c                   parm                    TRUL28DS
020600060616     c*
020700060616     c                   if        wcalltisie2='S'
020800060616     c                   clear                   tisie2ds
020900060616     c                   eval      sie2tla='C'
021000060616     c                   call      'TISIE2R'
021100060616     c                   parm                    tisie2ds
021200060616     c                   endif
021300060616     c
021400030721     c                   eval      *inLR  = *on
021500030721
021600030721      *---------------------------------------------------------------*
021700030721      * OPERAZIONI INIZIALI                                           *
021800030721      *---------------------------------------------------------------*
021900030721     c     OPE_INI       BegSr
022000040616     C
022100040616      *
022200040616      * Reperisco dal piano i FV DEFLUENZA per i p.o. da elaborare
022300040616      *
022400040616     C     kFLTR9        setll     FLTR9000
022500040616     C     kFLTR9        reade     FLTR9000
022600040616     c                   dow       NOT %EOF(fltr901L)
022700040616     C                   IF        TR9BAI >= '001'  and  TR9NPG > 0
022800040616     C                                              and  TR9NFV > 0
022900040616     c                   movel     TR9BAI        W0030
023000040616     c                   z-add     1             xx
023100040616     c     W0030         lookup    POel(xx)                               10
023200040616     c                   z-add     TR9npg        POnpg(xx)
023300040616     c                   z-add     TR9nfv        POnfv(xx)
023301140523     c                   movel     TR9pep        PObau(xx)
023400040616     c                   ENDIF
023500040616     C     kFLTR9        reade     FLTR9000
023600040616     c                   ENDDO
023700040616     c
023800040616      *
023900030721      * Reperisco tab. "3I" = variabili pgm partenze
024000030721     c                   clear                   ds3I
024100030721     c                   eval      TBLkut = 1
024200030721     c                   eval      TBLcod = '3I'
024300030721     c                   movel(p)  '1'           TBLkey
024400030721     c     K03tab00      chain     TABEL
024500030721if  1c                   if            %found(TABEL00F)
024600030721     c                             and TBLflg = *blanks
024700030721     c                   movel     TBLuni        ds3I
024800030721e   1c                   endif
024900040517      *
025000040517     c                   EndSr
025100030721      *---------------------------------------------------------------*
025200040520      * POSIZIONAMENTO INIZIALE SUL FILE FNBLP25L                     *
025300030721      *---------------------------------------------------------------*
025400030721     c     sr_SETLL      BegSr
025500030721      *
025600040520     c                   eval      BLPlna = poel(xx)
025700040520     c                   eval      BLPdcm = *zeros
025800040520     c                   movel     Wdate8N       BLPaas
025900040520     c                   move      Wdate8N       BLPmgs
026000030721      *
026100040520     c     K04BLP25      setll     FNBLP000
026200040520     c     K02BLP25      reade     FNBLP000
026300030721      *
026400030721     c                   EndSr
026500030721
026600030721      *---------------------------------------------------------------*
026700040520      * ELABORAZIONE SINGOLO RECORD DEL FILE FNBLP25L                 *
026800030721      *---------------------------------------------------------------*
026900030721     c     sr_ELAB       BegSr
027000030721      *
027100030721      * Pulisco dati reperiti solo in casi specifici
027200040520     c                   clear                   FNBLTds
027300060530     c***                clear                   dsBL4F
027400060530     c                   clear                   dsBL4I
027500030721      *
027600030721      * Reperisco dati del p.o. segnacollo
027700040520     c                   eval      wPO    =  BLPfls
027800030724     c                   exsr      RepMemoPO
027900030724     c                   movel     $POd(yy)      AZORGfls
028000030721      *
028100030721      * Reperisco dati del p.o. di partenza
028200040520     c                   eval      wPO    =  BLPlnp
028300030724     c                   exsr      RepMemoPO
028400030724     c                   movel     $POd(yy)      AZORGlnp
028500030724     c                   movel     lnpORGde3     Og143lnp
028600030721      *
028700030721      * Reperisco dati del p.o. di arrivo
028800040520     c                   eval      wPO    =  BLPlna
028900030724     c                   exsr      RepMemoPO
029000030724     c                   movel     $POd(yy)      AZORGlna
029100030724     c                   movel     lnaORGde3     Og143lna
029200030721      *
029300030721      * Reperisco tabella Nazioni
029400040520if  1c                   if        BLPnzd    <> SAVEnzd
029500040520     c                   eval      SAVEnzd   =  BLPnzd
029600030724     c                   exsr      RepMemoT15
029700030721e   1c                   endif
029800030721      *
029900030721      * Reperisco tabella Codici bolla
030000040520if  1c                   if        BLPcbo    <> SAVEcbo
030100040520     c                   eval      SAVEcbo   =  BLPcbo
030200030724     c                   exsr      RepMemoT3A
030300030721e   1c                   endif
030400030721      *
030500080703      * Preparo e scrivo il record in FITR600F
030600030721     c                   exsr      WriteFLTR6
030700030721      *
030800030721     c                   EndSr
030900030724
031000030724      *---------------------------------------------------------------*
031100030724      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL P.O.                  *
031200030724      *---------------------------------------------------------------*
031300030724     c     RepMemoPO     BegSr
031400030724      *
031500030724      * Controllo se diverso con l'ultimo reperito
031600030724if  1c                   if        wPO    <> $PO(yy)
031700030724      *
031800030724      * Controllo se già elaborato
031900030724     c                   eval      *in10   = *off
032000030724     c                   eval      yy      = 1
032100030724     c     wPO           lookup    $PO(yy)                                10
032200030724      *
032300030724      * Reperimento dati se non ancora elaborato
032400030724if  2c                   if        not *in10
032500030724      *
032600030724     c     wPO           chain     AZORG
032700030724if  3c                   if        not %found(AZORG01L)
032800030724     c                             or  ORGfva <> *blanks
032900030724     c                   clear                   AZORGds
033000030724e   3c                   endif
033100030724      * - Memorizzazione dati:
033200030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
033300030724     c     *zeros        lookup    $PO(yy)                                10
033400030724if  3c                   if        not *in10
033500030724     c                   eval      yy      = %elem($PO)
033600030724e   3c                   endif
033700030724     c                   eval      $PO(yy) = wPO
033800030724     c                   movel     AZORGds       $POd(yy)
033900030724      *
034000030724e   2c                   endif
034100030724      *
034200030724e   1c                   endif
034300030724      *
034400030724     c                   EndSr
034500030724
034600030724      *---------------------------------------------------------------*
034700030724      * REPERIMENTO (E MEMORIZZAZIONE) TAB.15 = NAZIONI               *
034800030724      *---------------------------------------------------------------*
034900030724     c     RepMemoT15    BegSr
035000030724      *
035100030724      * Controllo se già elaborato
035200030724     c                   eval      *in10   =  *off
035300030728     c                   eval      xy      =  1
035400030728     c     SAVEnzd       lookup    $15(xy)                                10
035500030724      *
035600030724      * Reperimento dati se non ancora elaborato
035700030728if  1c                   if        not *in10
035800030724      *
035900030724     c                   clear                   ds15
036000030724     c                   eval      TBLcod = '15'
036100030724     c                   movel(p)  SAVEnzd       TBLkey
036200030724     c     K03tab00      chain     TABEL
036300030728if  2c                   if        %found(TABEL00F)
036400030724     c                             and TBLflg = *blanks
036500030724     c                   movel     TBLuni        ds15
036600030728e   2c                   endif
036700030724      * - Memorizzazione dati:
036800030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
036900030728     c     *hival        lookup    $15(xy)                                10
037000030728if  2c                   if        not *in10
037100030728     c                   eval      xy      =  %elem($15)
037200030728e   2c                   endif
037300030728     c                   eval      $15(xy) =  SAVEnzd
037400030728     c                   movel     ds15          $15ds(xy)
037500030728      *
037600030728      * RE-impostazione dati se già elaborato
037700030728x   1c                   else
037800030728      *
037900030728     c                   movel     $15ds(xy)     ds15
038000030724      *
038100030724e   1c                   endif
038200030724      *
038300030724     c                   EndSr
038400030724
038500030724      *---------------------------------------------------------------*
038600030724      * REPERIMENTO (E MEMORIZZAZIONE) TAB.3A = CODICI BOLLA          *
038700030724      *---------------------------------------------------------------*
038800030724     c     RepMemoT3A    BegSr
038900030724      *
039000030724      * Controllo se già elaborato
039100030724     c                   eval      *in10   =  *off
039200030728     c                   eval      xy      =  1
039300030728     c     SAVEcbo       lookup    $3A(xy)                                10
039400030724      *
039500030724      * Reperimento dati se non ancora elaborato
039600030728if  1c                   if        not *in10
039700030724      *
039800030724     c                   clear                   ds3A
039900030724     c                   eval      TBLcod = '3A'
040000030724     c                   movel(p)  SAVEcbo       TBLkey
040100030724     c     K03tab00      chain     TABEL
040200030728if  2c                   if        %found(TABEL00F)
040300030724     c                             and TBLflg = *blanks
040400030724     c                   movel     TBLuni        ds3A
040500030728e   2c                   endif
040600030724      * - Memorizzazione dati:
040700030724      *   forzatura in ultimo elemento se schiera senza elementi vuoti
040800030728     c     *hival        lookup    $3A(xy)                                10
040900030728if  2c                   if        not *in10
041000030728     c                   eval      xy      =  %elem($3A)
041100030728e   2c                   endif
041200030728     c                   eval      $3A(xy) =  SAVEcbo
041300030728     c                   movel     ds3A          $3Ads(xy)
041400030728      *
041500030728      * RE-impostazione dati se già elaborato
041600030728x   1c                   else
041700030728      *
041800030728     c                   movel     $3Ads(xy)     ds3A
041900030724      *
042000030724e   1c                   endif
042100030724      *
042200030724     c                   EndSr
042300030721
042400030721      *---------------------------------------------------------------*
042500080703      * PREPARAZIONE E SCRITTURA DEL RECORD IN FITR600F               *
042600030721      *---------------------------------------------------------------*
042700030721     c     WriteFLTR6    BegSr
042800030721      *
042900080703     c                   clear                   FITR6
043000080703     c
043100080703     c                   eval      tr6fil=filiale
043200080703     c                   eval      tr6cpc=codicepc
043300030721      *
043400030721      * segnacollo "chi sono"
043500030721      * (imposto i dati validi per tutti i segnacolli della spedizione;
043600030721      *  il pgm. TRUL28R verrà richiamato per ogni collo)
043700030721     c                   clear                   BarCodeDS
043800040520     c                   eval      BARcdeFLS = BLPfls
043900040520     c                   eval      BARcdeLNA = BLPlna
044000040520     c                   eval      BARcdeNRS = BLPnrs
044100040520     c                   eval      BARcdeZNC = BLPznc
044200030721      * secondi di attesa dopo stampa
044300030721     c                   eval      TR6sea    =  §3Isea
044400030721      * flag segnacollo da stampare ('S'/' ')
044500030721     c                   eval      TR6st1    =  'S'
044600030721      * flags
044700030721     c                   eval      TR6fl1    =  *off
044800030721      * codice cliente
044900040520if  1c                   if        BLPccm    >  *zeros
045000040520     c                   eval      TR6ksc    =  BLPccm
045100030721x   1c                   else
045200040520     c                   eval      TR6ksc    =  BLPksc
045300030721e   1c                   endif
045400030721      * p.o. partenza segnacollo
045500040520     c                   eval      TR6fls    =  BLPfls
045600030721      * descrizione del p.o. partenza segnacollo
045700030721     c                   eval      TR6dlp    =  flsORGdes
045800030721      * codice prodotto - default
045900040520     c                   eval      TR6cdp    =  BLPrma
046000030721      * numero serie segnacollo
046100040520     c                   eval      TR6nrs    =  BLPnrs
046200030721      * p.o. di arrivo
046300040520     c                   eval      TR6lna    =  BLPlna
046400030721      * descrizione p.o. arrivo spedizione
046500030721if  1c                   if        lnaORGde5 <> *blanks
046600030721     c                   eval      TR6dla    =  lnaORGde5
046700030721x   1c                   else
046800030721     c                   eval      TR6dla    =  lnaORGdes
046900030721e   1c                   endif
047000030721      * terminal di arrivo
047100040520     c                   eval      TR6tna    =  BLPtfa
047200030721      * tipo servizio bolle
047300040520     c                   eval      TR6tsp    =  BLPtsp
047400030721      * zona segnacollo
047500040520     c                   eval      TR6zsc    =  BLPznc
047600030721      * totale segnacolli della spedizione
047700040520     c                   eval      TR6nc2    =  BLPncl
047800030721      * ragione sociale mittente
047900040520     c                   eval      TR6rsm    =  BLPrsm
048000030721      * provincia mittente
048100040520     c                   eval      TR6prm    =  BLPprm
048200030721      * ragione sociale destinatario
048300040520     c                   eval      TR6rsd    =  BLPrsd
048400030721      * c.a.p. destinatario
048500040520     c                   eval      TR6cad    =  BLPcad
048600030721      * indirizzo destinatario
048700040520     c                   eval      TR6ind    =  BLPind
048800030721      * località destinatario
048900040520     c                   eval      TR6lod    =  BLPlod
049000030721      * provincia destinatario
049100040520     c                   eval      TR6prd    =  BLPprd
049200030721      * volume
049300040520     c                   eval      TR6vol    =  BLPvlf
049400030721      * peso in kg
049500040520     c                   eval      TR6pkg    =  BLPpkf
049600030721      * flag volume bollettato
049700040520     c                   eval      TR6fvr    =  BLPfvf
049800030721      * anno spedizione
049900040520     c                   eval      TR6aas    =  BLPaas
050000030721      * meso/giorno spedizione
050100040520     c                   eval      TR6mgs    =  BLPmgs
050200030730      *
050300040518      * tipo stampa:
050400030730     c                   eval      TR6tst    =  'EST'
050500030721      * codice iso nazione arrivo
050600060616     c*****              eval      TR6isa    =  §15cod
050700030721      * flag S=contrassegno; A=assegnato, X=c/assegno+assegnato
050800030730sel 2c                   select
050900030730w   2c                   when      %subst(§3Atb1:1:1) = 'A'
051000030721     c                             and    §3Afca      = *zeros
051100030721     c                   eval      TR6fca    =  'A'
051200030730w   2c                   when      %subst(§3Atb1:1:1) = 'A'
051300030721     c                             and    §3Afca     <> *zeros
051400030721     c                   eval      TR6fca    =  'X'
051500030730w   2c                   when      §3Afca    <> *zeros
051600030721     c                   eval      TR6fca    =  'S'
051700030730e   2c                   endsl
051800040617      * Tipo e numero FV
051900040617if  2c                   z-add     ponpg(xx)     TR6npg
052000040617if  2c                   z-add     ponfv(xx)     TR6nfv
052001140523      * Baia di smistamento
052002140523if  2c                   movel     pobau(xx)     TR6bau
052100030721      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
052200030721      *
052300030721      * cicli di WRITE dei record (1 x segnacollo)
052400030721sel 1c                   select
052500040520w   1c                   when      BLPncd >  *zeros
052600030721     c                   exsr      sr_SCS
052700040520w   1c                   when      BLPfns =  'S'
052800030721     c                   exsr      sr_SCNS
052900030721e   1c                   endsl
053000030721      *
053100030721     c                   EndSr
053200030721
053300030721      *---------------------------------------------------------------*
053400030721      * Registrazione elenco segnacolli sequenziali                   *
053500030721      *---------------------------------------------------------------*
053600030721     c     sr_SCS        BegSr
053700030721      *
053800030721     c                   clear                   TR6nc1
053900040520     c                   eval      TR6ncd    =  BLPncd - 1
054000040520if  1c                   if        BLPnca    =  *zeros
054100040520     c                   eval      BLPnca    =  BLPncd
054200030721e   1c                   endif
054300030721      *
054400030721      * Ciclo di calcolo segnacolli sequenziali
054500040520do  1c                   DOW       TR6ncd    <  BLPnca
054600030721      *
054700030721      * - numero segnacollo
054800030721     c                   eval      TR6ncd    =  TR6ncd + 1
054900030721      * - progressivo del segnacollo in esame
055000030721     c                   eval      TR6nc1    =  TR6nc1 + 1
055100030728      * - impostazione campi "specifici" per DPD
055200030728     c                   exsr      Dati_DPD
055300030721      * - calcolo il "chi sono" (comprensivo di check-digit)
055400030721     c                   exsr      sr_CHISONO
055500030721      *
055600080703     c                   WRITE(E)  FITR6
055700030721      *
055800030721e   1c                   ENDDO
055900030721      *
056000030721     c                   EndSr
056100030721
056200030721      *---------------------------------------------------------------*
056300030721      * Registrazione elenco segnacolli NON sequenziali               *
056400030721      *---------------------------------------------------------------*
056500030721     c     sr_SCNS       BegSr
056600030721      *
056700030721     c                   clear                   TR6nc1
056800030721      *
056900030721      * Ciclo di repermento segnacolli NON sequenziali
057000040520     c     k04BLT01      setll     FNBLT000
057100040520     c     k04BLT01      reade     FNBLT000
057200030721      *
057300040520do  1c                   DOW       NOT %eof(FNBLT01L)
057400030721      *
057500030721      * - numero segnacollo
057600040520     c                   eval      TR6ncd    =  BLTnsc
057700030721      * - progressivo del segnacollo in esame
057800030721     c                   eval      TR6nc1    =  TR6nc1 + 1
057900030728      * - impostazione campi "specifici" per DPD
058000030728     c                   exsr      Dati_DPD
058100030721      * - calcolo il "chi sono" (comprensivo di check-digit)
058200030721     c                   exsr      sr_CHISONO
058300030721      *
058400080703     c                   WRITE(E)  FITR6
058500030721      *
058600040520     c     k04BLT01      reade     FNBLT000
058700030721      *
058800030721e   1c                   ENDDO
058900030721      *
059000030721     c                   EndSr
059100030728
059200030728      *---------------------------------------------------------------*
059300030728      * Impostazione dati specifici per DPD                           *
059400030728      *---------------------------------------------------------------*
059500040518     C     Dati_DPD      BEGSR
059600030728      *
059700060217      * - chain Fiar4 per Export DPD
059800060530     c***                eval      ar4trc    =  'F'
059900060530     c                   eval      ar4trc    =  'I'
060000060217     c     K05ar401      chain     Fiar4000
060100060217if  2c                   if        %found(Fiar401L)
060200060530     c****               movel     ar4not        dsBL4F
060300060530     c                   movel     ar4not        dsBL4I
060400030728e   2c                   endif
060500030728      *
060600060530if  2c****               IF        §B4fnp    <> *blanks
060700060530if  2c                   IF        §B4ipn    <> *blanks
060800030728      *
060900060531      * - codice prodotto (trattandosi di pgm che stampa solo l'etichetta DP
061000060531      *   D non c'è bisogno di impostare questo campo con il numero parcel
061100060531      *   perchè ciò serve solo per la stampa dell'etichetta Bartolini
061200060530     c*****              eval      TR6cdp    =  %subst(§B4fnp : 1 : 3)
061300060530     c*****                                  +  ' '
061400060530     c*****                                  +  %subst(§B4fnp : 4 : 8)
061500060530     c*****                                  +  ' '
061600060530     c*****                                  +  %subst(§B4fnp :12 : 1)
061700060616      * - sigla servizio
061800060616     c* vale sia per dp3 che per dp4
061900060616if  3c                   if        BLPpkf    <= 3
062000060616     c                   eval      TR6sse    =  'X'
062100060616e   3c                   endif
066200060616     c
066300060616     c* Nuovo  cappario Bartolini
066400060616     c
066500060616     c                   eval      TR6tse    =  'DP4'
066600060616     c
066700060616     c                   movel(P)  wDPDfax       TR6CDP
066800060616     c* reperisco versione cappario DPD
066900060616     c                   clear                   tisie2ds
067000060616     c                   z-add     blpmgs        sie2dri
067100060616     c                   movel     blpaas        sie2dri
067200060616     c                   call      'TISIE2R'
067300060616     c                   parm                    TIsie2ds
067400060616     c                   eval      wcalltisie2='S'
067500060616     c
067600060616     c* chain su file dpcdp01l per prendere nazione depot
067700060616     c     kcdp          chain     dpcdp01l
067800060616     c                   if        %found(dpcdp01l)
067900060616     c                   movel     cdpiso2       tr6SIA
068000060616     c                   endif
068100061010     c                   eval      tr6awb= §b4iat + §b4ipy
068200060616     c* TR6ISA solo per DP4 asteriscato da altre parti
068300060616     c                   movel     §15cod        tr6ISA
068400060616     c
068500060616     c                   eval      %subst(tr6eur:1:1) = §b4ibi
068600060616     c* richiamo pgm per avere il cap da stampare come vuole dpd
068700060616     c                   clear                   outcapdpd
068800060616     c                   call      'XCAPDPD'
068900060616     c                   parm                    blpcad
069000060616     c                   parm                    outcapdpd         7
069100061016     c                   parm      blpnzd        inpnzd            3
069200060616     c
069300060616     c                   eval      %subst(tr6eur:2:7) = %subst(outcapdpd:1:7)
069400060616     c* Se il numero parcel è lungo 11, devo portarlo a 14
069500060616     c                   if        %subst(§b4ipn:12:1)=' '
069600060616     c                   eval      %subst(tr6eur:9:1) = '0'
069700060616     c                   eval      %subst(tr6eur:10:3)=%subst(§b4ipn:1:3)
069800060616     c                   eval      %subst(tr6eur:13:2)='99'
069900060616     c                   eval      %subst(tr6eur:15:8)=%subst(§b4ipn:4:8)
070000060616     c                   else
070100060616     c                   eval      %subst(tr6eur:9:14)=§b4ipn
070200060616     c                   endif
070300060616     c
070400060616     c                   eval      %subst(tr6eur:23:3)=§b4isc
070500060616     c                   eval      %subst(tr6eur:26:3)=%editc(§15cie:'X')
070600060616     c* §DESIP solo per DP4
070700060616     c                   move      §b4icd        tr6SIP
070800060616     c                   clear                   trul28ds4
070900060616     c                   eval      i284cod=%subst(tr6EUR:2:27)
071000060616     c                   call      'TRUL28R5'
071100060616     c                   parm                    trul28ds4
071200060616     c                   movel     o284ckd       tr6SIP
071300060616     c
071400060616     c                   eval      %subst(tr6XX1:1:7)=sie2verd
071500060616     c                   eval      %subst(tr6XX1:8:4)=§b4ida
071600060616     c                   eval      %subst(tr6XX1:12:4)=§b4ids
071700060616     c
071800060616     c                   movel     wDPDtel       TR6xx2
071900060616     c                   movel     §b4isc        ksrvc
072000060616     c     kcsc          chain     dpcsc01l
072100060616     c                   if        %found(dpcsc01l)
072200060620     c                   evalr     W016A = %TRIM(cscsrvd)
072300060620     c                   move      W016A         tr6XX2
072400060616     c                   endif
072700060616     c*
072800030728      *
072900030728e   2c                   ENDIF
073000030728      *
073100030728     c                   EndSr
073200030728
073300030721      *---------------------------------------------------------------*
073400030721      * Impostazione del segnacollo "CHI SONO" (con check digit)      *
073500030721      *---------------------------------------------------------------*
073600030721     c     sr_CHISONO    BegSr
073700030721      *
073800030721      * Costruisco il codice a barre: FLS+LNA+NRS+NSC+ZNC+CKD
073900030721      * Mancano solo NSC e CKD:
074000030721      * - NSC l'ho appena calcolato in TR6ncd
074100030721      * - chiamo TRUL28R per ricavarne il check-digit
074200030721     c                   eval      BARcdeNSC =  TR6ncd
074300030721     c                   reset                   TRUL28ds
074400030721     c                   movel     BarCodeDS     I28cod
074500030721     c                   call      'TRUL28R1'
074600030721     c                   parm                    TRUL28DS
074700030721if  1c                   if        O28err    =  *blanks
074800030721     c                   movel     O28cod        TR6who
074900030721e   1c                   endif
075000030721      *
075100030721     c                   EndSr
075200040517      *---------------------------------------------------------------*
075300040517      * SBR iniziale
075400040517      *---------------------------------------------------------------*
075500040517     c     *INZSR        BegSr
075600040517      *
075700040517     c     *Entry        plist
075800080703     c                   parm                    codicepc          1
075900080703     c                   parm                    filiale           3
076000080703     c                   parm                    catfoglio         1
076100080703     c                   parm                    numfoglio         7
076200080703     c                   parm                    sysnam            8
076300040616      *
076400040616      * Imposto campi KEY fltr9
076500080703     c                   move      filiale       Kfgs
076600080703     c                   move      catfoglio     Knpga
076700080703     c                   move      numfoglio     Knfva
076800040616      *
076900040616      * Imposto la data minima da elaborare
077000050118      *    in "buona" - 30 gg da oggi
077100050118      *    in "test" - 600 gg da oggi
077200040616     c                   move      *date         WdateISO
077300080703     c                   IF        sysnam='AS888   '
077400050118     c                   subdur    600:*d        WdateISO
077500050118     c                   ELSE
077600050118     c                   subdur    30:*d         WdateISO
077700050118     c                   ENDIF
077800040616     c                   movel     WdateISO      Wdate8N
077900040616      * Imposto la KPJBA
078000040616     c                   eval      KNMUS = SDSusr
078100040616     c                   eval      KNMTD = SDSjob
078200040616     c                   eval      KNRJO = SDSjnr
078300040616     c                   eval      KNMEB = SDSjob
078400040517      *
078500040517      * Carico tabella P.O. gestiti
078600170221     c***                movel     'PP'          D06cod
078700170221     c***                movel     'L'           D06tla
078800170221     c***                eval      D06key = filiale
078900170221     c***                movel(p)  Trul06ds      KPJBU
079000170221     c***                call      'TRUL06R'
079100170221     c***                parm                    KPJBA
079200170221     c***                movel     KPJBU         Trul06ds
079201170221     c                   clear                   Trul09ds
079202170221     c                   movel     'L'           i09tla
079203170221     c                   movel     'DEF'         i09mod
079204170221     c                   movel     filiale       i09tfa
079205170221     c                   call      'TRUL09R'
079206170221     c                   parm                    trul09ds
079300040517      *
079400040517      * Elaboro solo linee DPD con abilitatata stamba barcode in arrivo
079500040517     c                   clear                   yy
079600040517do  1c                   DO        30            xx
079700040517if  2c                   if        Lia(xx) = *zeros
079800040517     c                   leave
079900040517e   2c                   endif
080000040517     c     Lia(xx)       chain     AZORG
080100040517if  2c                   if        %found(AZORG01L) and ORGfva = *blanks
080200040517     c                   movel     ORGde3        OG143
080300040517     c                   movel     ORGdf0        OG150
080400040629if  2c                   if        §ogntw = 'DPD'
080500040517     c                   add       1             yy
080600040517     c                   movel     orgfil        POel(yy)
080700041014     c                   movel     orgtel        DTel(yy)
080800041014     c                   movel     orgfax        DFax(yy)
080900040517e   2c                   endif
081000040517e   2c                   endif
081100040517e   1c                   ENDDO
081200040517      *
081300040517      * Key-List
081400040616      *
081500040616      * - file FLTR901L
081600040616     c     KFLTR9        klist
081700040616     c                   kfld                    Kfgs
081800040616     c                   kfld                    Knpga
081900040616     c                   kfld                    Knfva
082000040517      *
082100040520      * - file FNBLP25L
082200040520     c     K04BLP25      klist
082300040520     c                   kfld                    BLPlna
082400040520     c                   kfld                    BLPdcm
082500040520     c                   kfld                    BLPaas
082600040520     c                   kfld                    BLPmgs
082700040520     c     K02BLP25      klist
082800040520     c                   kfld                    BLPlna
082900040520     c                   kfld                    BLPdcm
083000060217      * - file Fiar401L
083100060217     c     K05ar401      klist
083200040520     c                   kfld                    BLPaas
083300040520     c                   kfld                    BLPlnp
083400040520     c                   kfld                    BLPnrs
083500040520     c                   kfld                    BLPnsp
083600060217     c                   kfld                    ar4trc
083700040520      * - file FNBLT01L
083800040520     c     K04BLT01      klist
083900040520     c                   kfld                    BLPaas
084000040520     c                   kfld                    BLPlnp
084100040520     c                   kfld                    BLPnrs
084200040520     c                   kfld                    BLPnsp
084300040517      * - file TABEL00F
084400040517     c     K03tab00      klist
084500040517     c                   kfld                    TBLkut
084600040517     c                   kfld                    TBLcod
084700040517     c                   kfld                    TBLkey
084800060616     C     KCDP          KLIST
084900060616     C                   KFLD                    sie2ver
085000060616     C                   KFLD                    §b4ida
085100060616     C     KCSC          KLIST
085200060616     C                   KFLD                    sie2ver
085300060616     C                   KFLD                    ksrvc
085400040517      *
085500040517     c                   EndSr
