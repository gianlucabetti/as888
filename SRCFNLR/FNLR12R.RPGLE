000100030827      **-------------------------------------------------------------**
000200030922      ** FNLR12R ** Quadratura bolle da consegnare - Elaboraz. wrkf  **
000300030827      **-------------------------------------------------------------**
000400030827
000500030827     h decedit('0,') datedit(*dmy/)
000600030827
000700030827     fFNLR12D   cf   e             workstn
000800030827      *
000900030827     fWFCBB01L  uf a e           k disk
001000030827      *
001100030827     fFNARB01L  if   e           k disk
001200051114     fFiAR901L  if   e           k disk
001300030827     fFIAR601L  if   e           k disk
001400030827     fFNLBL01L  if   e           k disk
001500030827     fTABEL00F  if   e           k disk
001600030827     fAZORG01L  if   e           k disk
001700030827
001800030827      *---------------------------------------------------------------*
001900030827
002000030827      *
002100030827      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002200030827      *
002300030827      *
002400030827      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002500030827      *
002600030908      * Comandi da eseguire
002700030908     d $Cmd            s             80    dim(3) ctdata perrcd(1)
002800030827      * Messaggi di errore a video
002900031003     d $Msg            s             78    dim(16) ctdata perrcd(1)
003000030827      * Linee Arrivi
003100030827     d L6              S              3  0 DIM(29)
003200030827      *
003300030827      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
003400030827      *
003500030827      * Parametri ricevuti
003600030827     d KPJBA         e ds
003700030827     d FNLR12ds      e ds                  inz
003800030827      *
003900030827      * DS parametri utente
004000030827     d dDatiUte      e ds
004100030827     d AZUTEds       e ds                  extname(AZUTE00F)
004200030827     d TIBS34ds      e ds
004300030827      *
004400030827      * DS parametri per pgm TRUL06R  = Caricamento schiera p.o.
004500030827     d TRUL06ds      e ds
004600030827     d  Lia                    1     90  0 dim(30)
004700030827      *
004800030827      * DS parametri per pgm TRUL28R  = Calcolo check-digit da barcode
004900030827     d TRUL28ds      e ds                  inz
005000030827     d   I28mod      e                     inz('BAR')
005100030827      *
005200030827      * DS tab.3A = Codici bolla
005300030827     d ds3A          e ds                  inz
005400030908      *
005500040806      * ds per il controllo della presenza di CA per la spedizione richiesta
005600040806     d FIDN12DS      e ds
005700040806     d  skKey                 26    305    dim(20)
005800040806
005900040806     d dsKey           ds
006000040806     d  dctaac                        4  0
006100040806     d  dctfil                        3  0
006200040806     d  dctnca                        7  0
006300040806
006400030827      *
006500030827      * DS parametri per pgm FNLR36R  = Interrogazione bolle arrivi
006600030827     d FNLR36ds        ds                  inz
006700030827     d  P36aas                14     17  0 inz
006800030827     d  P36lnp                18     20  0 inz
006900030827     d  P36nrs                21     22  0 inz
007000030827     d  P36nsp                23     29  0 inz
007100030827     d  P36f03                30     30    inz
007200030827     d  P36flg                31     31    inz('2')
007300030827      *
007400030827      * ds per sottocampare il barcode della spedizione
007500030827     d BARcodeDS       ds            14    inz
007600030827     d  BARcdeAAS                     2s 0 inz
007700030827     d  BARcdeLNP                     3s 0 inz
007800030827     d  BARcdeNRS                     2s 0 inz
007900030827     d  BARcdeNSP                     7s 0 inz
008000030827     d**BARcdeCKD                     1s 0 inz
008100030827      *
008200030827     d WLBDA8          ds
008300030827     d  G02dat                 1      8  0
008400030827     d  G02inv                 9     16  0
008500030827     d  G02err                17     17
008600030827     d  G02tgi                18     22  0
008700030827      *
008800030827     d                sds
008900030827     d  VIDpgm           *proc
009000030827      *
009100030827      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009200030827      *
009300030827      * - Flags booleani
009400030827     d $Fine           s              1    inz(*off)
009500030827     d $InzD01         s              1    inz(*on)
009600030827     d $InzW01         s              1    inz(*off)
009700030827     d $SoloInca       s              1    inz(*off)
009800030827     d $Recupero       s              1    inz(*off)
009900030827     d $ExistCA        s              1    inz(*off)
010000030827      * - Altri flags
010100030827     d $TipVid         s              1    inz('1')
010200030827     d $Inserim        s              1    inz('P')
010300030827      *
010400030827      * - Memorizzazione temporanea dati x test rotture
010500030827     d SAVaas          s                   like(V1Caas)  inz
010600030827     d SAVlnp          s                   like(V1Clnp)  inz
010700030827     d SAVnrs          s                   like(V1Cnrs)  inz
010800030827     d SAVnsp          s                   like(V1Cnsp)  inz
010900030827      *
011000030827      * - Campi di comodo
011100030827     d w003a           s              3    inz
011200030827     d w0020           s              2  0 inz
011300030827     d W1Cpis          s                   like(O28cod) inz
011400030908      *
011500030908      * - Campi per esecuzione comandi
011600030908     d Qcmd            s            150    inz
011700030908     d Qlen            s             15  5 inz(150)
011800030908      *
011900030908     d Wfile           s             10    inz
012000030908     d Woutq           s             10    inz
012100030908     d Wform           s             10    inz
012200030908     d Wusrd           s             10    inz('EDPristLDV')
012300030827
012400030827      *===============================================================*
012500030827      *  RIEPILOGO INDICATORI                                         *
012600030827      *---------------------------------------------------------------*
012700030827      * 10    - Comodo                                                *
012800030908      * 13    - Consente la ristampa delle LDV (per utenti EDP*)      *
012900030827      * 28    - Emette il messaggio di errore a video                 *
013000030827      * 31    - Posiziona il cursore sul campo "Barcode spedizione"   *
013100030827      * 41    - Errore in anno spedizione                             *
013200030827      * 42    - Errore in p.o. di partenza                            *
013300030827      * 43    - Errore in numero  spedizione                          *
013400030827      * 44    - Errore in serie   spedizione                          *
013500030827      * 45    - Errore in barcode spedizione                          *
013600030827      * 90    - Generico di errore                                    *
013700030827      *===============================================================*
013800030827      *
013900030827     c     *Entry        plist
014000030827     c                   parm                    KPJBA
014100030827     c                   movel     KPJBU         FNLR12ds
014200030827      *
014300030827      * Recupero i P.O. associati al P.O. utente
014400030827     c                   exsr      Rep£6
014500030827      *
014600030827      * Gestione video
014700030827     c                   dow       $Fine = *off
014800030827     c     $TipVid       caseq     '1'           GesD01
014900030827     c     $TipVid       caseq     'W'           GesW01
015000030827     c                   endcs
015100030827     c                   enddo
015200030827      *
015300040806     C* chiusura
015400040806     C                   movel     'C'           I12tla
015500040806     C                   call      'FIDN12R'
015600040806     C                   parm                    FIDN12DS
015700040806      *
015800030827      * Fine
015900030827     c                   eval      *inLR = *on
016000030827      *
016100030827      *---------------------------------------------------------------*
016200030827      * *InzSr     > Operazioni Iniziali                              *
016300030827      *---------------------------------------------------------------*
016400030827     c     *InzSr        BegSr
016500030827      *
016600030827     c     *dtaara       define    §azute        azuteds
016700030827     c     *dtaara       define    §datiute      ddatiute
016800030827     c                   in(E)     *dtaara
016900030827if  1c                   IF        %Error  or  RSUT = *blanks
017000030827     c                   clear                   Tibs34Ds
017100030827     c                   call      'TIBS34R'
017200030827     c                   parm                    Tibs34Ds
017300030827     c                   in        *dtaara
017400030827e   1c                   ENDIF
017500030827      *
017600030924     c                   eval      *in13  = (%subst(DUTute:1:3)='EDP')
017700030827      *
017800030908     c                   eval      TBLkut = 1
017900030827      *
018000030827     c                   EndSr
018100030827      *
018200030827      *---------------------------------------------------------------*
018300030827      * Rep£6      > Aggancio della £6 e recupero dei P.O. associati  *
018400030827      *---------------------------------------------------------------*
018500030827     c     Rep£6         BegSr
018600030827      *
018700030827      * Imposto come default l'inserimento con postola se non ricevuto
018800030827     c                   if        D12pou <= *zeros
018900030827     c                   eval      D12pou =  DUTpou
019000030827     c                   endif
019100030827      *
019200030827     c                   clear                   TRUL06ds
019300030827     c                   eval      D06cod = '£6'
019400030827     c                   movel(p)  D12pou        D06key
019500030827     c                   movel(p)  TRUL06ds      KPJBU
019600030827     c                   call      'TRUL06R'
019700030827     c                   parm                    KPJBA
019800030827     c                   movel     KPJBU         TRUL06ds
019900030827     c                   movea     Lia           L6
020000030827      *
020100030827     c                   EndSr
020200030827      *
020300030827      *---------------------------------------------------------------*
020400030827      * GesD01      > Gestione formato video D01                      *
020500030827      * Compilazione distinta con inserimento singola spedizione      *
020600030827      *---------------------------------------------------------------*
020700030827     c     GesD01        BegSr
020800030827      *
020900030827      * Inizializzazione videata
021000030827if  1c                   if        $InzD01 = *on
021100030827     c                   exsr      InzD01
021200030827     c                   movel     *off          $InzD01
021300030827e   1c                   endif
021400030827      *
021500030827      * Scrivo la testata e la riga tasti funzionali abilitati
021600030827     c                   write     LR12T01
021700030827     c                   write     LR12F01
021800030827      * Emetto la videata
021900030827     c                   exfmt     LR12D01
022000030827     c                   setoff                                       28  90
022100030827     c                   clear                   V1Dmsg
022200030827      *
022300030827sel 1c                   select
022400030827      * F2=Manuale/Barcode
022500030827w   1c                   when      *inKB
022600030827     c                   exsr      F02D01
022700030827     c                   goto      EndGesD01
022800030827      * F3=Fine e Stampa
022900030827w   1c                   when      *inKC
023000030827     c                   exsr      F03D01
023100030827     c                   goto      EndGesD01
023200030827      * F7=Interrogazione bolle arrivi
023300030827w   1c                   when      *inKG
023400030827     c                   exsr      F07D01
023500030827     c                   goto      EndGesD01
023600130312     c                   endsl
023700030827      *
023800030827      * Controllo dati immessi a video
023900030827     c                   exsr      CtrD01
024000030827      *
024100030827      * Aggiornamento SE NON ci sono errori
024200030827if  1c                   if        NOT *in90
024300030827     c                   exsr      GesWFCBB
024400030827e   1c                   endif
024500030827      *
024600030827     c     EndGesD01     EndSr
024700030827
024800030827      *---------------------------------------------------------------*
024900030827      * InzD01      > Inizializzazione videata D01                    *
025000030827      *---------------------------------------------------------------*
025100030827     c     InzD01        BegSr
025200030827      *
025300030827      * Pulisco i campi a video
025400030827     c                   movel     *year         V1Caas
025500030827     c                   clear                   V1Clnp
025600030827     c                   clear                   V1Cnrs
025700030827     c                   clear                   V1Cnsp
025800030827     c                   clear                   V1Cpis
025900030827     c                   clear                   V1Dmsg
026000030827     c                   setoff                                       28  90
026100030827     c                   movea     *zeros        *in(41)
026200030827      *
026300030827      * Imposto *in31 per il posizionamento del cursore
026400030827      *   (sul P.O. di partenza o sul bar-code, a seconda sia stato
026500030827      *   chiesto l'inserimento Manuale o con Pistola)
026600030827if  1c                   if        $Inserim = 'P'
026700030827     c                   eval      V1Df02   = 'F2=Manuale'
026800030827     c                   eval      *in31 = *on
026900030827x   1c                   else
027000030827     c                   eval      V1Df02   = 'F2=Pistola'
027100030827     c                   eval      *in31 = *off
027200030827e   1c                   endif
027300030827      *
027400030827     c                   EndSr
027500030827
027600030827      *---------------------------------------------------------------*
027700030827      * F02D01      > Tasto funzionale F02 = Inserimento spedizione   *
027800030827      *               da manuale a pistola o viceversa                *
027900030827      * Posizionamento cursore sul campo "Spedizione" o "Barcode"     *
028000030827      *---------------------------------------------------------------*
028100030827     c     F02D01        BegSr
028200030827      *
028300030827if  1c                   if        $Inserim = 'P'
028400030827     c                   eval      $Inserim = 'M'
028500030827     c                   eval      V1Df02   = 'F2=Pistola'
028600030827     c                   eval      *in31 = *off
028700030827x   1c                   else
028800030827     c                   eval      $Inserim = 'P'
028900030827     c                   eval      V1Df02   = 'F2=Manuale'
029000030827     c                   eval      *in31 = *on
029100030827e   1c                   endif
029200030924      *
029300030924     c                   movea     *zeros        *in(41)
029400030924     c                   eval      *in90 = *on
029500030827      *
029600030827     c                   EndSr
029700030827
029800030827      *---------------------------------------------------------------*
029900030924      * F03D01      > Tasto funzionale F3 = Fine lavoro               *
030000030827      *---------------------------------------------------------------*
030100030827     c     F03D01        BegSr
030200030827      *
030300030924     c                   eval      $TipVid = 'W'
030400030924     c                   eval      $InzW01 = *on
030500030827      *
030600030827     c                   EndSr
030700030827
030800030827      *---------------------------------------------------------------*
030900030827      * F07D01      > Tasto funzionale F07 = Interrogaz. bolle arrivi *
031000030827      *---------------------------------------------------------------*
031100030827     c     F07D01        BegSr
031200031002      *
031300031002      * Se prevista l'immissione del barcode:
031400031002      * eseguo controllo dati immessi a video per re-impostare i dati
031500031002      *   della spedizione da barcode
031600031002     c                   if        *in31 = *on
031700031002     c                   exsr      CtrD01
031800031002     c                   setoff                                       28  90
031900031002     c                   endif
032000030827      *
032100030827     c                   reset                   FNLR36ds
032200030827     c                   z-add     V1Caas        P36aas
032300030827     c                   z-add     V1Clnp        P36lnp
032400030827     c                   z-add     V1Cnrs        P36nrs
032500030827     c                   z-add     V1Cnsp        P36nsp
032600030827if  1c                   if        P36nsp > *zeros
032700030827     c                   movel     '1'           P36flg
032800030827e   1c                   endif
032900030827      *
033000030827     c                   movel(p)  FNLR36ds      KPJBU
033100030827     c                   call      'FNLR36R'
033200030827     c                   parm                    KPJBA
033300030827     c                   movel     KPJBU         FNLR36ds
033400030827      *
033500031002sel 1c                   select
033600031002w   1c                   when          P36f03 <> *on
033700030827     c                             and V1Cnsp =  *zeros
033800030827     c                   z-add     P36aas        V1Caas
033900030827     c                   z-add     P36lnp        V1Clnp
034000030827     c                   z-add     P36nrs        V1Cnrs
034100030827     c                   z-add     P36nsp        V1Cnsp
034200031002     c                   clear                   V1Cpis
034300031002     c                   when      *in31 = *on
034400031002     c                   clear                   V1Clnp
034500031002     c                   clear                   V1Cnrs
034600031002     c                   clear                   V1Cnsp
034700031002e   1c                   endsl
034800030827      *
034900030827     c                   eval      *in90 = *on
035000030827      *
035100030827     c                   EndSr
035200030827
035300030827      *---------------------------------------------------------------*
035400030827      * GesW01      > Gestione formato video W01                      *
035500030827      * Richiesta conferma del tasto F12                              *
035600030827      *---------------------------------------------------------------*
035700030827     c     GesW01        BegSr
035800030827      *
035900030827      * Inizializzazione videata
036000030827if  1c                   if        $InzW01 = *on
036100030827     c                   exsr      InzW01
036200030827     c                   movel     *off          $InzW01
036300030827e   1c                   endif
036400030827      *
036500030827     c                   exfmt     LR12W01
036600030827     c                   setoff                                       28  90
036700030827      *
036800030827sel 1c                   select
036900030827      * F12=Ritorno
037000030827w   1c                   when      *inKL
037100030827     c                   exsr      F12W01
037200030827     c                   goto      EndGesW01
037300030827      * F6=Conferma uscita
037400030827w   1c                   when      *inKF
037500030827     c                   exsr      F06W01
037600030827     c                   goto      EndGesW01
037700030827e   1c                   endsl
037800030827      *
037900030827     c     EndGesW01     EndSr
038000030827
038100030827      *---------------------------------------------------------------*
038200030827      * InzW01      > Inizializzazione videata W01                    *
038300030827      *---------------------------------------------------------------*
038400030827     c     InzW01        BegSr
038500030827      *
038600030924     c***                eval      W1sns  = 1
038700030924     c                   eval      W1Csns = 'S'
038800030827      *
038900030827     c                   EndSr
039000030827
039100030827      *---------------------------------------------------------------*
039200030827      * F06W01      > Tasto funzionale F6 = Conferma uscita           *
039300030827      *---------------------------------------------------------------*
039400030827     c     F06W01        BegSr
039500030827      *
039600030924      * Richiamo pgm di stampa
039700030925if  1c***                if        W1sns  = 1
039800030925if  1c                   if        W1Csns = 'S'
039900030924     c                   movel(p)  FNLR12ds      KPJBU
040000030924     c                   call      'FNLR12R1'
040100030924     c                   parm                    KPJBA
040200030925e   1c                   endif
040300030924      *
040400030827     c                   movel     *on           $Fine                          fine pgm
040500030827      *
040600030827     c                   EndSr
040700030827
040800030827      *---------------------------------------------------------------*
040900030827      * F12W01      > Tasto funzionale F12 = Ritorno                  *
041000030827      *---------------------------------------------------------------*
041100030827     c     F12W01        BegSr
041200030827      *
041300030827     c                   eval      $TipVid = '1'
041400030827     c                   eval      $InzD01 = *on
041500030827      *
041600030827     c                   EndSr
041700030827
041800030827      *---------------------------------------------------------------*
041900030827      * CtrD01      > Controllo dati della videata D01                *
042000030827      *---------------------------------------------------------------*
042100030827     c     CtrD01        BegSr
042200030827      *
042300030827     c                   movea     *zeros        *in(41)
042400030827      *
042500030827      * Se il campo V1Cpis è impostato, valorizzo i campi della spedi-
042600030827      *   zione solo se non sono stati impostati a mano o al giro di
042700030827      *   barcode che poi ha dato l'errore
042800030827     c                   clear                   BARcodeDS
042900030827if  1c                   if        V1Cpis > *zeros
043000030827     c                   movel     V1Cpis        BARcodeDS
043100030827if  2c                   if            V1Clnp = *zeros
043200030827     c                             and V1Cnrs = *zeros
043300030827     c                             and V1Cnsp = *zeros
043400030827     c                   z-add     BARcdeAAS     V1Caas
043500030916if  3c                   if        BARcdeAAS < 100
043600030916if  4c                   if        BARcdeAAS > 60
043700030827     c                   add       1900          V1Caas
043800030916x   4c                   else
043900030827     c                   add       2000          V1Caas
044000030916e   4c                   endif
044100030916e   3c                   endif
044200030827     c                   z-add     BARcdeLNP     V1Clnp
044300030827     c                   z-add     BARcdeNRS     V1Cnrs
044400030827     c                   z-add     BARcdeNSP     V1Cnsp
044500030827      * - controllo il check digits
044600030827     c                   exsr      sr_ChkDgt
044700030827     c     *in90         cabeq     *on           EndCtrD01
044800030827x   2c                   else
044900030827     c                   z-add     V1Caas        w0020
045000030827      * - impostati sia barcode che dati della spedizione
045100030827if  3c                   if           w0020  <> BARcdeAAS
045200030827     c                             or V1Clnp <> BARcdeLNP
045300030827     c                             or V1Cnrs <> BARcdeNRS
045400030827     c                             or V1Cnsp <> BARcdeNSP
045500030827     c                   seton                                        284590
045600030827     c                   movel     $Msg(02)      V1Dmsg
045700030827e   3c                   endif
045800030827e   2c                   endif
045900030827     c     *in90         cabeq     *on           EndCtrD01
046000030827e   1c                   endif
046100030924      *
046200030924      * Imposto l'anno a 4 cifre (se digitate 2 cifre)
046300030924if  1c                   if            V1Caas > *zeros
046400030924     c                             and V1Caas < 100
046500030924if  2c                   if        V1Caas > 60
046600030924     c                   add       1900          V1Caas
046700030924x   2c                   else
046800030924     c                   add       2000          V1Caas
046900030924e   2c                   endif
047000030924e   1c                   endif
047100030827      *
047200030827      * Controllo l'immissione dei dati indispensabili
047300030827sel 1c                   select
047400030916w   1c                   when           V1Caas <= *zeros
047500030924     c                             or   V1Caas <  (*Year - 2)
047600030924     c                             or   V1Caas >  (*Year + 1)
047700030827     c                   seton                                        28  90
047800030827     c  n31              eval      *in41  = *on
047900030827     c   31              eval      *in45  = *on
048000030827     c                   movel     $Msg(03)      V1Dmsg
048100030827w   1c                   when      V1Clnp <= *zeros
048200030827     c                   seton                                        28  90
048300030827     c  n31              eval      *in42  = *on
048400030827     c   31              eval      *in45  = *on
048500030827     c                   movel     $Msg(04)      V1Dmsg
048600030827w   1c                   when      V1Cnsp <= *zeros
048700030827     c                   seton                                        28  90
048800030827     c  n31              eval      *in44  = *on
048900030827     c   31              eval      *in45  = *on
049000030827     c                   movel     $Msg(05)      V1Dmsg
049100030827e   1c                   endsl
049200030827     c     *in90         cabeq     *on           EndCtrD01
049300030827      *
049400030827     c     EndCtrD01     EndSr
049500030827
049600030827      *---------------------------------------------------------------*
049700030827      * GesWFCBB    > Verifica esistenza bolla nel file di lavoro     *
049800030827      *---------------------------------------------------------------*
049900030827     c     GesWFCBB      BegSr
050000030827      *
050100030827     c                   clear                   WFCBB000
050200030827      *
050300030827      * Controllo esistenza su WFCBB01L
050400030827     c                   z-add     V1Caas        CBBaas
050500030827     c                   z-add     V1Clnp        CBBlnp
050600030827     c                   z-add     V1Cnrs        CBBnrs
050700030827     c                   z-add     V1Cnsp        CBBnsp
050800030827     c     k04cbb01      chain     WFCBB000
050900030827      *
051000030827if  1c                   if        %found(WFCBB01L)
051100030827      *
051200030827     c                   exsr      sr_Y_Found
051300030827      *
051400030827x   1c                   else
051500030827      *
051600030827     c                   exsr      sr_N_Found
051700030827      *
051800030827e   1c                   endif
051900030827      *
052000030827      * Se non sono stati rilevati errori ripulisco la videata
052100030827      *   altrimenti ripulisco solo i campi impostati dal pgm.
052200030827      *   (se letto il barcode)
052300030827sel 1c                   select
052400030827w   1c                   when      *in90   = *off
052500030827     c                   eval      $InzD01 = *on
052600030827w   1c                   when      *in31   = *on
052700030827     c                   clear                   V1Clnp
052800030827     c                   clear                   V1Cnrs
052900030827     c                   clear                   V1Cnsp
053000030827e   1c                   endsl
053100030827      *
053200030827     c                   EndSr
053300030827
053400030827      *---------------------------------------------------------------*
053500030827      * sr_Y_Found  > Gestione bolla trovata nel file WFCBB01L        *
053600030827      *---------------------------------------------------------------*
053700030827     c     sr_Y_Found    BegSr
053800030827      *
053900030827sel 1c                   select
054000030827      *
054100030827      * Aggiorno record se NON già aggiornato
054200030827w   1c                   when      CBBerr = *blanks
054300030827     c                   eval      CBBerr = 'T'
054400030827     c                   UPDATE    WFCBB000
054500030827      *
054600030827      * Emetto errore se già impostato (e non aggiorno)
054700030827w   1c                   when      CBBerr = 'E'
054800030827     c                   movel     CBBmsg        V1Dmsg
054900030827     c                   seton                                        28  90
055000030827     c  n31              eval      *in41  = *on
055100030827     c   31              eval      *in45  = *on
055200030827      *
055300030827e   1c                   endsl
055400030827      *
055500030827     c                   EndSr
055600030827
055700030827      *---------------------------------------------------------------*
055800030827      * sr_N_Found  > Gestione bolla NON trovata nel file WFCBB01L    *
055900030827      *---------------------------------------------------------------*
056000030827     c     sr_N_Found    BegSr
056100030827      *
056200030827      * Reperisco dati da FNARB
056300030827     c     k04cbb01      chain     FNARB000
056400030827      *
056500030827      * Spedizione non trovata neppure in FNARB
056600030827if  1c                   if        NOT %found(FNARB01L)
056700030827     c                   seton                                        28  90
056800030827     c  n31              eval      *in42  =  *on
056900030827     c   31              eval      *in45  =  *on
057000030827     c                   movel     $Msg(06)      V1Dmsg
057100030827     c                   goto      End_N_Found
057200030827e   1c                   endif
057300030917      *
057400030917      * Imposto i dati per il nuovo record nel WF
057500030917     c                   eval      CBBmgs = ARBmgs
057600030917     c                   eval      CBBncl = ARBncl
057700030917     c                   eval      CBBpkf = ARBpkf
057800030917     c                   eval      CBBvlf = ARBvlf
057900030917     c                   eval      CBBrsd = ARBrsd
058000030917     c                   eval      CBBind = ARBind
058100030917     c                   eval      CBBcad = ARBcad
058200030917     c                   eval      CBBlod = ARBlod
058300030917     c                   eval      CBBprd = ARBprd
058400030917     c                   eval      CBBnzd = ARBnzd
058500031001     c                   eval      CBBtcr = ARBtcr
058600031001     c                   eval      CBBdcr = ARBdcr
058700030827      *
058800030827      * P.O. di arrivo non in £6
058900030827if  1c                   if        ARBlna <> DUTpou
059000030827     c                   eval      *in10  =  *off
059100030827     c     ARBlna        lookup    L6                                     10
059200030827if  2c                   if        NOT *in10
059300030827     c                   exsr      DecodLNA
059400030827     c                   seton                                        28  90
059500030827     c  n31              eval      *in42  =  *on
059600030827     c   31              eval      *in45  =  *on
059700030827     c                   movel     $Msg(07)      V1Dmsg
059800030827     c                   move      ARBlna        w003a
059900030827     c                   eval      V1Dmsg =  %replace(w003a:V1Dmsg:
060000030827     c                                       %scan('&PO':V1Dmsg))
060100030827     c                   eval      V1Dmsg =  %replace(ORGdes:V1Dmsg:
060200030827     c                                       %scan('&DES':V1Dmsg))
060300030827     c                   goto      End_N_Found
060400030827e   2c                   endif
060500030827e   1c                   endif
060600030916      *
060700030916      * Verifica se sono già state aperte C.A.
060800030916     c                   exsr      CtrExistCA
060900030916if  1c                   if        $ExistCA  = *on
061000030916     c                   seton                                        28  90
061100030916     c  n31              eval      *in42  =  *on
061200030916     c   31              eval      *in45  =  *on
061300031003     c                   movel     $Msg(15)      V1Dmsg
061400030916     c                   eval      V1Dmsg =  %replace(
061500030916     c                                       %editc(DCTaac:'Z'):
061600030916     c                                       V1Dmsg:
061700030916     c                                       %scan('&AAC':V1Dmsg))
061800030916     c                   move      DCTfil        w003a
061900030917     c                   eval      V1Dmsg =  %replace(%trim(
062000030917     c                                       %editw(DCTfil:'0   ')):
062100030916     c                                       V1Dmsg:
062200030916     c                                       %scan('&PO':V1Dmsg))
062300030916     c                   eval      V1Dmsg =  %replace(
062400030917     c                                       (%editc(DCTnca:'Z')):
062500030916     c                                       V1Dmsg:
062600030916     c                                       %scan('&NCA':V1Dmsg))
062700030916     c                   goto      End_N_Found
062800030916e   1c                   endif
062900030827      *
063000030827      * Verifica consegna
063100030827if  1c                   if        ARBdcm >  *zeros
063200030827     c                   seton                                        28  90
063300030827     c  n31              eval      *in42  =  *on
063400030827     c   31              eval      *in45  =  *on
063500030827     c                   exsr      CtrSoloInca
063600030827      * - Solo incasso
063700030827if  2c                   if        $SoloInca = *on
063800030827     c                   movel     $Msg(08)      V1Dmsg
063900030827x   2c                   else
064000030827      * - Già consegnata
064100030827     c                   clear                   WLBda8
064200031003     c                   move      ARBddc        G02inv
064300030827     c                   movel     '3'           G02err
064400030827     c                   call      'XSRDA8'
064500030827     c                   parm                    WLBda8
064600030827     c                   movel     $Msg(09)      V1Dmsg
064700030827     c                   eval      V1Dmsg =  %replace(
064800030827     c                                       %editc(ARBndc:'Z'):
064900030827     c                                       V1Dmsg:
065000030827     c                                       %scan('&ARBNDC':V1Dmsg))
065100030827     c                   eval      V1Dmsg =  %replace(
065200030827     c                                       %editc(G02dat:'Y'):
065300030827     c                                       V1Dmsg:
065400031003     c                                       %scan('&ARBDDC':V1Dmsg))
065500030827e   2c                   endif
065600030827     c                   goto      End_N_Found
065700031003      *
065800031003x   1c                   else
065900031003      * - In consegna
066000031003if  2c                   if        ARBndc >  *zeros
066100031003     c                   seton                                        28  90
066200031003     c  n31              eval      *in42  =  *on
066300031003     c   31              eval      *in45  =  *on
066400031003     c                   clear                   WLBda8
066500031003     c                   move      ARBddc        G02inv
066600031003     c                   movel     '3'           G02err
066700031003     c                   call      'XSRDA8'
066800031003     c                   parm                    WLBda8
066900031003     c                   movel     $Msg(10)      V1Dmsg
067000031003     c                   eval      V1Dmsg =  %replace(
067100031003     c                                       %editc(ARBndc:'Z'):
067200031003     c                                       V1Dmsg:
067300031003     c                                       %scan('&ARBNDC':V1Dmsg))
067400031003     c                   eval      V1Dmsg =  %replace(
067500031003     c                                       %editc(G02dat:'Y'):
067600031003     c                                       V1Dmsg:
067700031003     c                                       %scan('&ARBDDC':V1Dmsg))
067800031003     c                   goto      End_N_Found
067900031003e   2c                   endif
068000030827e   1c                   endif
068100030827      *
068200030827      * Verifica se spedizione bloccata
068300030827if  1c                   if        ARBfbc <> *blanks
068400030827     c                   seton                                        28  90
068500030827     c  n31              eval      *in42  =  *on
068600030827     c   31              eval      *in45  =  *on
068700030827sel 2c                   select
068800030827w   2c                   when      ARBfbc =  'A'
068900031003     c                   movel     $Msg(11)      V1Dmsg
069000030827w   2c                   when      ARBfbc =  'G'
069100031003     c                   movel     $Msg(12)      V1Dmsg
069200030827w   2c                   other
069300031003     c                   movel     $Msg(13)      V1Dmsg
069400030827e   2c                   endsl
069500030827     c                   goto      End_N_Found
069600030827e   1c                   endif
069700030827      *
069800030827      * Verifica se bolla di recupero
069900030827     c                   exsr      CtrRecupero
070000030827if  1c                   if        $Recupero = *on
070100030827     c                   seton                                        28  90
070200030827     c  n31              eval      *in42  =  *on
070300030827     c   31              eval      *in45  =  *on
070400031003     c                   movel     $Msg(14)      V1Dmsg
070500030827     c                   goto      End_N_Found
070600030827e   1c                   endif
070700030827      *
070800030827      * Superati tutti i controlli precedenti
070900030827      * (ma bolla non trovata nel file di lavoro)
071000030827     c                   seton                                        28  90
071100030827     c  n31              eval      *in42  =  *on
071200030827     c   31              eval      *in45  =  *on
071300031003     c                   movel     $Msg(16)      V1Dmsg
071400030827     c                   goto      End_N_Found
071500030827      *
071600030827     c     End_N_Found   tag
071700030827      *
071800030827      * Scrivo il record nel file di lavoro con messaggio di errore
071900030827     c                   eval      CBBerr =  'E'
072000030827     c                   eval      CBBmsg =  V1Dmsg
072100030827     c                   WRITE     WFCBB000
072200030827      *
072300030827     c                   EndSr
072400030827
072500030827      *---------------------------------------------------------------*
072600030827      * CtrSoloInca > Verifica se bolla di solo incasso               *
072700030827      *---------------------------------------------------------------*
072800030827     c     CtrSoloInca   BegSr
072900030827      *
073000030827     c                   reset                   $SoloInca
073100030827     c                   clear                   ds3A
073200030827      *
073300030827      *?SOLO INCASSO - C/ASSEGNO?
073400030827      *
073500030827      * Controllo se la bolla in esame ha legame con seconda bolla
073600030827     c     k04cbb01      chain     FNLBL000
073700030827      *
073800030827if  1c                   if        %found(FNLBL01L)
073900030827     c                             and LBLlan  = LBLlap
074000030827      *
074100030827      * > Se ho una bolla "mamma" ed è locale
074200030827     c     k04arb_1      chain     FNARB000
074300030827if  2c                   if        %found(FNARB01L)
074400030827      *
074500051114     c     K04arb_1      chain     FiAR9000
074600051114if  3c                   if             %found(FiAR901L)
074700030827     c                             and (ARBicc = *blanks
074800030827     c                              or  ARBicc = 'R')
074900030827     c                   eval      $SoloInca   = *on
075000030827e   3c                   endif
075100030827      *
075200030827e   2c                   endif
075300030827      *
075400030827      *   Mi sono "sporcato" FNARB, quindi riaggancio la figlia
075500030827     c     k04cbb01      chain     FNARB000
075600030827      *
075700030827x   1c                   else
075800030827      *
075900030827      * > Se non c'è legame controllo il c/a della bolla che sto
076000030827      *   elaborando
076100051114     c     k04cbb01      chain     FiAR9000
076200051114if  2c                   if             %found(FiAR901L)
076300030827     c                             and (ARBicc = *blanks
076400030827     c                              or  ARBicc = 'R')
076500030827     c                   eval      $SoloInca   = *on
076600030827e   2c                   endif
076700030827      *
076800030827e   1c                   endif
076900030827      *
077000030827     c     $SoloInca     cabeq     *on           EndSoloInca
077100030827      *
077200030827      *?SOLO INCASSO - FATTURA PORTO ASSEGNATO (PREPAGATO)?
077300030827      *
077400030827      * Decodifico il tipo bolla
077500030827     c                   movel     '3A'          TBLcod
077600030827     c                   movel(p)  ARBcbo        TBLkey
077700030827     c     kTABEL        chain     TABEL00F
077800030827if  1c                   if            %found(TABEL00F)
077900030827     c                             and TBLflg  = *blanks
078000030827     c                   movel     TBLuni        ds3A
078100030827e   1c                   endif
078200030827      *
078300030827      * Aggancio i dati di tassazione bolla
078400030827     c                   clear                   AR6ift
078500030827     c                   clear                   AR6div
078600030827if  1c                   if            §3Atb2  = *blanks
078700030827     c                             and %subst(§3Atb1:1:1) = 'A'
078800030827     c                   eval      AR6trc = '1'
078900030827     c     k05ar601      chain     FIAR6000
079000030827x   1c                   else
079100030827      * - 2ª bolla
079200030827if  2c                   if            §3Atb2 <> *blanks
079300030827     c                   eval      AR6trc = '2'
079400030827     c     k05ar601      chain     FIAR6000
079500030827e   2c                   endif
079600030827e   1c                   endif
079700030827      *
079800030827if  1c                   if             %found(FIAR601L)
079900030827     c                             and  AR6ift > *zeros
080000030827     c                             and (ARBfip = 'I'
080100030827     c                              or  ARBica = *blanks
080200030827     c                              or  ARBica = 'R')
080300030827     c                   eval      $SoloInca   = *on
080400030827e   1c                   endif
080500030827      *
080600030827     c     $SoloInca     cabeq     *on           EndSoloInca
080700030827      *
080800030827     c     EndSoloInca   EndSr
080900030827
081000030827      *---------------------------------------------------------------*
081100030827      * CtrRecupero > Verifica se bolla di recupero                   *
081200030827      *---------------------------------------------------------------*
081300030827     c     CtrRecupero   BegSr
081400030827      *
081500030827     c                   reset                   $Recupero
081600030827     c                   clear                   ds3A
081700030827      *
081800030827      *?BOLLA DI RECUPERO?
081900030827      *
082000030827if  1c                   IF            ARBdcm = *zeros
082100030827     c                             and ARBndc = *zeros
082200030827      *
082300030827      * Decodifico il tipo bolla
082400030827     c                   movel     '3A'          TBLcod
082500030827     c                   movel(p)  ARBcbo        TBLkey
082600030827     c     kTABEL        chain     TABEL
082700030827if  2c                   if            %found(TABEL00F)
082800030827     c                             and TBLflg = *blanks
082900030827     c                   movel     TBLuni        ds3A
083000030827e   2c                   endif
083100030827      *
083200030827if  2c                   if        §3Arbl  = 'R'
083300030827      *
083400030827     c     K04cbb01      setll     FNLBL000
083500030827      * Se non trovati legami memorizzo che è una bolla di recupero
083600030827if  3c                   if        NOT %equal(FNLBL01L)
083700030827     c                   eval      $Recupero  = *on
083800030827e   3c                   endif
083900030827      *
084000030827e   2c                   endif
084100030827      *
084200030827e   1c                   endif
084300030827      *
084400030827     c                   EndSr
084500030827
084600030827      *---------------------------------------------------------------*
084700030827      * CtrExistCA  > Verifica se ci sono C.A. aperte                 *
084800030827      *---------------------------------------------------------------*
084900030827     c     CtrExistCA    BegSr
085000030827      *
085100030827     c                   reset                   $ExistCA
085200030827      *
085300040806     c**""**     k04cbb01      setll     FNDCT000
085400040806     c**""**     k04cbb01      reade     FNDCT000
085500030827      *
085600040806do  1c**""**                   dow       NOT %eof(FNDCT02L)
085700030827      *
085800040806if  2c**""**                   if            DCTatb = *blanks
085900040806     c**""**                             and DCTdch = *zeros
086000040806     c**""**                   eval      $ExistCA   = *on
086100040806     c**""**                   leave
086200040806e   2c**""**                   endif
086300030827      *
086400040806     c**""**     k04cbb01      reade     FNDCT000
086500040806e   1c**""**                   enddo
086600040806      *
086700040806      * Sostituisco la lettura del file FNDTC02 con
086800040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e aperte
086900040806      * legate alla spedizione che passo
087000040806      *
087100040806     c                   clear                   fidn12ds
087200040806     c                   eval      i12aas = CBBaas
087300040806     c                   eval      i12lnp = CBBlnp
087400040806     c                   eval      i12nrs = CBBnrs
087500040806     c                   eval      i12nsp = CBBnsp
087600040806     c                   eval      i12fel = 'E'
087700040806     c                   eval      i12fan = 'E'
087800040806     c                   eval      i12fch = 'E'
087900040806      *
088000040806     c                   call      'FIDN12R'
088100040806     c                   parm                    fidn12ds
088200040806      *
088300040806      * se non ci sono errori
088400040806     c                   if        o12err = ' '
088500040806      * se numero di CA trovate maggiore di zero
088600040806     c                   if        o12nca > 0
088700040806     c                   eval      $ExistCA   = *on
088800040806     c                   movea     skkey(1)      dskey
088900040806     c                   endif
089000040806      *
089100040806     c                   endif
089200030827      *
089300030827     c                   EndSr
089400030827
089500030827      *---------------------------------------------------------------*
089600030827      * sr_ChkDgt  > Controllo il check digit del barcode             *
089700030827      *---------------------------------------------------------------*
089800030827     c     sr_ChkDgt     BegSr
089900030827      *
090000030827     c                   reset                   TRUL28ds
090100030827     c                   movel     BARcodeDS     I28cod
090200030827     c                   call      'TRUL28R1'
090300030827     c                   parm                    TRUL28ds
090400030827      *
090500030827      * se rilevato errore o se check digit errato lo segnalo
090600030827     c                   movel(p)  V1Cpis        W1Cpis
090700030827if  1c                   if           O28err <> *blanks
090800030827     c                             or O28cod <> W1Cpis
090900030827     c                   seton                                        284590
091000030827     c                   movel     $Msg(01)      V1Dmsg
091100030827e   1c                   endif
091200030827      *
091300030827     c                   EndSr
091400030827
091500030827      *---------------------------------------------------------------*
091600030827      * DecodLNA   > Decodificas P.O. di arrivo                       *
091700030827      *---------------------------------------------------------------*
091800030827     c     DecodLNA      BegSr
091900030827      *
092000030827     c     ARBlna        chain     AZORG
092100030827if  1c                   if        not %found(AZORG01L)
092200030827     c                             or  ORGfva = '*'
092300030827     c                   movel     *all'? '      ORGdes
092400030827e   1c                   endif
092500030827      *
092600030827     c                   EndSr
092700030908
092800030908      *---------------------------------------------------------------*
092900030908      * Eseguo le override                                            *
093000030908      *---------------------------------------------------------------*
093100030908
093200030908     c     SR_x_Cmd      BEGSR
093300030908      *
093400030908      * impostazione comando da eseguire
093500030908     c                   eval      Qcmd = %trim($Cmd(1)) + ' '
093600030908     c                                  + %trim($Cmd(2))
093700030908      * - nome del file
093800030908     c                   eval      Qcmd = %replace(wFILE:Qcmd:
093900030908     c                                    %scan('&FILE':Qcmd))
094000030908      * - nome della coda di stampa
094100030908     c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
094200030908     c                                    %scan('&OUTQ':Qcmd))
094300030908      * - tipo di modulo
094400030908     c                   eval      Qcmd = %replace(wFORM:Qcmd:
094500030908     c                                    %scan('&FORMTYPE':Qcmd))
094600030908      * - dati utente
094700030908     c                   eval      Qcmd = %replace(wUSRD:Qcmd:
094800030908     c                                    %scan('&USRDTA':Qcmd))
094900030908      *
095000030908     c                   call      'QCMDEXC'                            90
095100030908     c                   parm                    Qcmd
095200030908     c                   parm                    Qlen
095300030908      *
095400030908     c                   ENDSR
095500030827
095600030827      *---------------------------------------------------------------*
095700030827      * DefKList   > Definizione KEY-LIST                             *
095800030827      *---------------------------------------------------------------*
095900030827     c     DefKList      Begsr
096000030827      *
096100030827     c     k04cbb01      klist
096200030827     c                   kfld                    CBBaas
096300030827     c                   kfld                    CBBlnp
096400030827     c                   kfld                    CBBnrs
096500030827     c                   kfld                    CBBnsp
096600030827      *
096700030827     c     k04arb_1      klist
096800030827     c                   kfld                    LBLaap
096900030827     c                   kfld                    LBLlpp
097000030827     c                   kfld                    LBLnrp
097100030827     c                   kfld                    LBLnsp
097200030827      *
097300030827     c     k05ar601      klist
097400030827     c                   kfld                    CBBaas
097500030827     c                   kfld                    CBBlnp
097600030827     c                   kfld                    CBBnrs
097700030827     c                   kfld                    CBBnsp
097800030827     c                   kfld                    AR6trc
097900030827      *
098000030827     c     kTABEL        klist
098100030827     c                   kfld                    TBLkut
098200030827     c                   kfld                    TBLcod
098300030827     c                   kfld                    TBLkey
098400030827      *
098500030827     c                   EndSr
098600030908
098700030908**  $Cmd
098800030908OVRPRTF    FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
098900030908           USRDTA('&USRDTA   ') SHARE(*YES)
099000030925           COPIES(2)
099100030917**  $MSG                                                        *
099200030924Errore Barcode                                                                  1
099300030924Scegliere spedizione o barcode                                                  2
099400030924Anno errato                                                                     3
099500030924P.O. di partenza errato                                                         4
099600030924Immettere il numero di spedizione                                               5
099700030924MANCA RECORD BOLLA - VERIFICARE P.O. ARRIVO DELLA SPEDIZIONE                    6
099800030924DDT ECCEDENTE - P.O. D'ARRIVO: &PO  &DES                                        7
099900030924SPEDIZIONE SOLO INCASSO                                                         8
100000031003GIA' CONSEGNATA. DISTINTA N. &ARBNDC DEL &ARBDDC                                9
100100031003IN CONSEGNA. DISTINTA N. &ARBNDC DEL &ARBDDC                                   10
100200031003LASCIATO AVVISO                                                                11
100300031003SPEDIZIONE GIACENTE                                                            12
100400031003SPEDIZIONE BLOCCATA                                                            13
100500031003SPEDIZIONE DI RECUPERO                                                         14
100600031003PRATICA DI DANNO APERTA: C.A. NUMERO &AAC/&PO/&NCA                             15
100700031003SPEDIZIONE DA NON CONSEGNARE - CONTROLLARE                                     16
