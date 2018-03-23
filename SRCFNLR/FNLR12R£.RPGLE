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
001500040806     f**""**FNDCT02L  if   e           k disk
001600030827     fTABEL00F  if   e           k disk
001700030827     fAZORG01L  if   e           k disk
001800030827
001900030827      *---------------------------------------------------------------*
002000030827
002100030827      *
002200030827      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002300030827      *
002400030827      *
002500030827      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002600030827      *
002700030908      * Comandi da eseguire
002800030908     d $Cmd            s             80    dim(3) ctdata perrcd(1)
002900030827      * Messaggi di errore a video
003000031003     d $Msg            s             78    dim(16) ctdata perrcd(1)
003100030827      * Linee Arrivi
003200030827     d L6              S              3  0 DIM(29)
003300030827      *
003400030827      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
003500030827      *
003600030827      * Parametri ricevuti
003700030827     d KPJBA         e ds
003800030827     d FNLR12ds      e ds                  inz
003900030827      *
004000030827      * DS parametri utente
004100030827     d dDatiUte      e ds
004200030827     d AZUTEds       e ds                  extname(AZUTE00F)
004300030827     d TIBS34ds      e ds
004400030827      *
004500030827      * DS parametri per pgm TRUL06R  = Caricamento schiera p.o.
004600030827     d TRUL06ds      e ds
004700030827     d  Lia                    1     90  0 dim(30)
004800030827      *
004900030827      * DS parametri per pgm TRUL28R  = Calcolo check-digit da barcode
005000030827     d TRUL28ds      e ds                  inz
005100030827     d   I28mod      e                     inz('BAR')
005200030827      *
005300030827      * DS tab.3A = Codici bolla
005400030827     d ds3A          e ds                  inz
005500030908      *
005600030908      * Parametri per richiesta stampanti
005700030908     d TRUL90ds      e ds                  inz
005800030908     d   D90rsb      e                     inz('L')
005900030908      *
006000030908      * Parametri per stampa LDV
006100030908     d FNLSB5ds      e ds                  inz
006200040806      * ds per il controllo della presenza di CA per la spedizione richiesta
006300040806     d FIDN12DS      e ds
006400040806     d  skKey                 26    305    dim(20)
006500040806
006600040806     d dsKey           ds
006700040806     d  dctaac                        4  0
006800040806     d  dctfil                        3  0
006900040806     d  dctnca                        7  0
007000040806
007100030827      *
007200030827      * DS parametri per pgm FNLR36R  = Interrogazione bolle arrivi
007300030827     d FNLR36ds        ds                  inz
007400030827     d  P36aas                14     17  0 inz
007500030827     d  P36lnp                18     20  0 inz
007600030827     d  P36nrs                21     22  0 inz
007700030827     d  P36nsp                23     29  0 inz
007800030827     d  P36f03                30     30    inz
007900030827     d  P36flg                31     31    inz('2')
008000030827      *
008100030827      * ds per sottocampare il barcode della spedizione
008200030827     d BARcodeDS       ds            14    inz
008300030827     d  BARcdeAAS                     2s 0 inz
008400030827     d  BARcdeLNP                     3s 0 inz
008500030827     d  BARcdeNRS                     2s 0 inz
008600030827     d  BARcdeNSP                     7s 0 inz
008700030827     d**BARcdeCKD                     1s 0 inz
008800030827      *
008900030827     d WLBDA8          ds
009000030827     d  G02dat                 1      8  0
009100030827     d  G02inv                 9     16  0
009200030827     d  G02err                17     17
009300030827     d  G02tgi                18     22  0
009400030827      *
009500030827     d                sds
009600030827     d  VIDpgm           *proc
009700030827      *
009800030827      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009900030827      *
010000030827      * - Flags booleani
010100030827     d $Fine           s              1    inz(*off)
010200030827     d $InzD01         s              1    inz(*on)
010300030827     d $InzW01         s              1    inz(*off)
010400030827     d $SoloInca       s              1    inz(*off)
010500030827     d $Recupero       s              1    inz(*off)
010600030827     d $ExistCA        s              1    inz(*off)
010700030827      * - Altri flags
010800030827     d $TipVid         s              1    inz('1')
010900030827     d $Inserim        s              1    inz('P')
011000030827      *
011100030827      * - Memorizzazione temporanea dati x test rotture
011200030827     d SAVaas          s                   like(V1Caas)  inz
011300030827     d SAVlnp          s                   like(V1Clnp)  inz
011400030827     d SAVnrs          s                   like(V1Cnrs)  inz
011500030827     d SAVnsp          s                   like(V1Cnsp)  inz
011600030827      *
011700030827      * - Campi di comodo
011800030827     d w003a           s              3    inz
011900030827     d w0020           s              2  0 inz
012000030827     d W1Cpis          s                   like(O28cod) inz
012100030908      *
012200030908      * - Campi per esecuzione comandi
012300030908     d Qcmd            s            150    inz
012400030908     d Qlen            s             15  5 inz(150)
012500030908      *
012600030908     d Wfile           s             10    inz
012700030908     d Woutq           s             10    inz
012800030908     d Wform           s             10    inz
012900030908     d Wusrd           s             10    inz('EDPristLDV')
013000030827
013100030827      *===============================================================*
013200030827      *  RIEPILOGO INDICATORI                                         *
013300030827      *---------------------------------------------------------------*
013400030827      * 10    - Comodo                                                *
013500030908      * 13    - Consente la ristampa delle LDV (per utenti EDP*)      *
013600030827      * 28    - Emette il messaggio di errore a video                 *
013700030827      * 31    - Posiziona il cursore sul campo "Barcode spedizione"   *
013800030827      * 41    - Errore in anno spedizione                             *
013900030827      * 42    - Errore in p.o. di partenza                            *
014000030827      * 43    - Errore in numero  spedizione                          *
014100030827      * 44    - Errore in serie   spedizione                          *
014200030827      * 45    - Errore in barcode spedizione                          *
014300030827      * 90    - Generico di errore                                    *
014400030827      *===============================================================*
014500030827      *
014600030827     c     *Entry        plist
014700030827     c                   parm                    KPJBA
014800030827     c                   movel     KPJBU         FNLR12ds
014900030827      *
015000030827      * Recupero i P.O. associati al P.O. utente
015100030827     c                   exsr      Rep£6
015200030827      *
015300030827      * Gestione video
015400030827     c                   dow       $Fine = *off
015500030827     c     $TipVid       caseq     '1'           GesD01
015600030827     c     $TipVid       caseq     'W'           GesW01
015700030827     c                   endcs
015800030827     c                   enddo
015900030827      *
016000040806     C* chiusura
016100040806     C                   movel     'C'           I12tla
016200040806     C                   call      'FIDN12R'
016300040806     C                   parm                    FIDN12DS
016400040806      *
016500030827      * Fine
016600030827     c                   eval      *inLR = *on
016700030827      *
016800030827      *---------------------------------------------------------------*
016900030827      * *InzSr     > Operazioni Iniziali                              *
017000030827      *---------------------------------------------------------------*
017100030827     c     *InzSr        BegSr
017200030827      *
017300030827     c     *dtaara       define    §azute        azuteds
017400030827     c     *dtaara       define    §datiute      ddatiute
017500030827     c                   in(E)     *dtaara
017600030827if  1c                   IF        %Error  or  RSUT = *blanks
017700030827     c                   clear                   Tibs34Ds
017800030827     c                   call      'TIBS34R'
017900030827     c                   parm                    Tibs34Ds
018000030827     c                   in        *dtaara
018100030827e   1c                   ENDIF
018200030827      *
018300030908      * Consento la ristampa delle LDV per gli utenti EDP*
018400030908      *   (una sola volta)
018500030924      *   => NON più. Basta così !
018600030924     c                   eval      *in13  = (%subst(DUTute:1:3)='EDP')
018700030827      *
018800030908     c                   eval      TBLkut = 1
018900030827      *
019000030827     c                   EndSr
019100030827      *
019200030827      *---------------------------------------------------------------*
019300030827      * Rep£6      > Aggancio della £6 e recupero dei P.O. associati  *
019400030827      *---------------------------------------------------------------*
019500030827     c     Rep£6         BegSr
019600030827      *
019700030827      * Imposto come default l'inserimento con postola se non ricevuto
019800030827     c                   if        D12pou <= *zeros
019900030827     c                   eval      D12pou =  DUTpou
020000030827     c                   endif
020100030827      *
020200030827     c                   clear                   TRUL06ds
020300030827     c                   eval      D06cod = '£6'
020400030827     c                   movel(p)  D12pou        D06key
020500030827     c                   movel(p)  TRUL06ds      KPJBU
020600030827     c                   call      'TRUL06R'
020700030827     c                   parm                    KPJBA
020800030827     c                   movel     KPJBU         TRUL06ds
020900030827     c                   movea     Lia           L6
021000030827      *
021100030827     c                   EndSr
021200030827      *
021300030827      *---------------------------------------------------------------*
021400030827      * GesD01      > Gestione formato video D01                      *
021500030827      * Compilazione distinta con inserimento singola spedizione      *
021600030827      *---------------------------------------------------------------*
021700030827     c     GesD01        BegSr
021800030827      *
021900030827      * Inizializzazione videata
022000030827if  1c                   if        $InzD01 = *on
022100030827     c                   exsr      InzD01
022200030827     c                   movel     *off          $InzD01
022300030827e   1c                   endif
022400030827      *
022500030827      * Scrivo la testata e la riga tasti funzionali abilitati
022600030827     c                   write     LR12T01
022700030827     c                   write     LR12F01
022800030827      * Emetto la videata
022900030827     c                   exfmt     LR12D01
023000030827     c                   setoff                                       28  90
023100030827     c                   clear                   V1Dmsg
023200030827      *
023300030827sel 1c                   select
023400030827      * F2=Manuale/Barcode
023500030827w   1c                   when      *inKB
023600030827     c                   exsr      F02D01
023700030827     c                   goto      EndGesD01
023800030827      * F3=Fine e Stampa
023900030827w   1c                   when      *inKC
024000030827     c                   exsr      F03D01
024100030827     c                   goto      EndGesD01
024200030827      * F7=Interrogazione bolle arrivi
024300030827w   1c                   when      *inKG
024400030827     c                   exsr      F07D01
024500030827     c                   goto      EndGesD01
024600030908      * F13=Ristampa LDV
024700030924w   1c                   when      *inKM  and  *in13
024800030924     c                   exsr      F13D01
024900030924     c                   goto      EndGesD01
025000030924e   1c                   endsl
025100030827      *
025200030827      * Controllo dati immessi a video
025300030827     c                   exsr      CtrD01
025400030827      *
025500030827      * Aggiornamento SE NON ci sono errori
025600030827if  1c                   if        NOT *in90
025700030827     c                   exsr      GesWFCBB
025800030827e   1c                   endif
025900030827      *
026000030827     c     EndGesD01     EndSr
026100030827
026200030827      *---------------------------------------------------------------*
026300030827      * InzD01      > Inizializzazione videata D01                    *
026400030827      *---------------------------------------------------------------*
026500030827     c     InzD01        BegSr
026600030827      *
026700030827      * Pulisco i campi a video
026800030827     c                   movel     *year         V1Caas
026900030827     c                   clear                   V1Clnp
027000030827     c                   clear                   V1Cnrs
027100030827     c                   clear                   V1Cnsp
027200030827     c                   clear                   V1Cpis
027300030827     c                   clear                   V1Dmsg
027400030827     c                   setoff                                       28  90
027500030827     c                   movea     *zeros        *in(41)
027600030827      *
027700030827      * Imposto *in31 per il posizionamento del cursore
027800030827      *   (sul P.O. di partenza o sul bar-code, a seconda sia stato
027900030827      *   chiesto l'inserimento Manuale o con Pistola)
028000030827if  1c                   if        $Inserim = 'P'
028100030827     c                   eval      V1Df02   = 'F2=Manuale'
028200030827     c                   eval      *in31 = *on
028300030827x   1c                   else
028400030827     c                   eval      V1Df02   = 'F2=Pistola'
028500030827     c                   eval      *in31 = *off
028600030827e   1c                   endif
028700030827      *
028800030827     c                   EndSr
028900030827
029000030827      *---------------------------------------------------------------*
029100030827      * F02D01      > Tasto funzionale F02 = Inserimento spedizione   *
029200030827      *               da manuale a pistola o viceversa                *
029300030827      * Posizionamento cursore sul campo "Spedizione" o "Barcode"     *
029400030827      *---------------------------------------------------------------*
029500030827     c     F02D01        BegSr
029600030827      *
029700030827if  1c                   if        $Inserim = 'P'
029800030827     c                   eval      $Inserim = 'M'
029900030827     c                   eval      V1Df02   = 'F2=Pistola'
030000030827     c                   eval      *in31 = *off
030100030827x   1c                   else
030200030827     c                   eval      $Inserim = 'P'
030300030827     c                   eval      V1Df02   = 'F2=Manuale'
030400030827     c                   eval      *in31 = *on
030500030827e   1c                   endif
030600030924      *
030700030924     c                   movea     *zeros        *in(41)
030800030924     c                   eval      *in90 = *on
030900030827      *
031000030827     c                   EndSr
031100030827
031200030827      *---------------------------------------------------------------*
031300030924      * F03D01      > Tasto funzionale F3 = Fine lavoro               *
031400030827      *---------------------------------------------------------------*
031500030827     c     F03D01        BegSr
031600030827      *
031700030924     c                   eval      $TipVid = 'W'
031800030924     c                   eval      $InzW01 = *on
031900030827      *
032000030827     c                   EndSr
032100030827
032200030827      *---------------------------------------------------------------*
032300030827      * F07D01      > Tasto funzionale F07 = Interrogaz. bolle arrivi *
032400030827      *---------------------------------------------------------------*
032500030827     c     F07D01        BegSr
032600031002      *
032700031002      * Se prevista l'immissione del barcode:
032800031002      * eseguo controllo dati immessi a video per re-impostare i dati
032900031002      *   della spedizione da barcode
033000031002     c                   if        *in31 = *on
033100031002     c                   exsr      CtrD01
033200031002     c                   setoff                                       28  90
033300031002     c                   endif
033400030827      *
033500030827     c                   reset                   FNLR36ds
033600030827     c                   z-add     V1Caas        P36aas
033700030827     c                   z-add     V1Clnp        P36lnp
033800030827     c                   z-add     V1Cnrs        P36nrs
033900030827     c                   z-add     V1Cnsp        P36nsp
034000030827if  1c                   if        P36nsp > *zeros
034100030827     c                   movel     '1'           P36flg
034200030827e   1c                   endif
034300030827      *
034400030827     c                   movel(p)  FNLR36ds      KPJBU
034500030827     c                   call      'FNLR36R'
034600030827     c                   parm                    KPJBA
034700030827     c                   movel     KPJBU         FNLR36ds
034800030827      *
034900031002sel 1c                   select
035000031002w   1c                   when          P36f03 <> *on
035100030827     c                             and V1Cnsp =  *zeros
035200030827     c                   z-add     P36aas        V1Caas
035300030827     c                   z-add     P36lnp        V1Clnp
035400030827     c                   z-add     P36nrs        V1Cnrs
035500030827     c                   z-add     P36nsp        V1Cnsp
035600031002     c                   clear                   V1Cpis
035700031002     c                   when      *in31 = *on
035800031002     c                   clear                   V1Clnp
035900031002     c                   clear                   V1Cnrs
036000031002     c                   clear                   V1Cnsp
036100031002e   1c                   endsl
036200030827      *
036300030827     c                   eval      *in90 = *on
036400030827      *
036500030827     c                   EndSr
036600030908
036700030908      *---------------------------------------------------------------*
036800030908      * F13D01      > Tasto funzionale F13 = Ristampa LDV             *
036900030908      *---------------------------------------------------------------*
037000030908     c     F13D01        BegSr
037100030908      *
037200030908     c                   call      'TRUL90R'
037300030908     c                   parm                    KPJBA
037400030908     c                   parm                    TRUL90ds
037500030908if  1c                   if        D90f3   = *on
037600030908     c                   goto      EndF13D01
037700030908e   1c                   endif
037800030908      *
037900030908      * - FNLSB5PA4 > bolle laser in formato A4
038000030908     c                   eval      wFILE = 'FNLSB5PA4 '
038100030908     c                   eval      wOUTQ = D90prb4
038200030908     c                   eval      wFORM = D90mdb4
038300030908     c                   exsr      SR_x_Cmd
038400030908     c     *in90         cabeq     *on           EndF13D01
038500030908      *
038600030908      * - FNLSB5PA5 > bolle laser in formato A5
038700030908     c                   eval      wFILE = 'FNLSB5PA5 '
038800030908     c                   eval      wOUTQ = D90prb5
038900030908     c                   eval      wFORM = D90mdb5
039000030908     c                   exsr      SR_x_Cmd
039100030908     c     *in90         cabeq     *on           EndF13D01
039200030908      *
039300030908     c     *loval        setll     WFCBB000
039400030908     c                   read (n)  WFCBB000
039500030908      *
039600030908do  1c                   dow       not %eof(WFCBB01L)
039700030908      *
039800030908     c                   clear                   FNLSB5ds
039900030908     c                   eval      DB0tbo  = 'A'
040000030908     c                   eval      DB0ris  = 'R'
040100030908     c                   eval      DB0aas  = CBBaas
040200030908     c                   eval      DB0lnp  = CBBlnp
040300030908     c                   eval      DB0nrs  = CBBnrs
040400030908     c                   eval      DB0nsp  = CBBnsp
040500030908      *
040600030908     c                   call      D90psl
040700030908     c                   parm                    FNLSB5ds
040800030908      *
040900030908     c                   read (n)  WFCBB000
041000030908      *
041100030908e   1c                   enddo
041200030908      *
041300030908     c                   clear                   FNLSB5ds
041400030908     c                   eval      DB0tla = 'C'
041500090127     c                   call      D90psl
041600030908     c                   parm                    FNLSB5ds
041700030908      *
041800030908     c                   eval      *in13   = *off
041900030908      *
042000030908     c     EndF13D01     EndSr
042100030827
042200030827      *---------------------------------------------------------------*
042300030827      * GesW01      > Gestione formato video W01                      *
042400030827      * Richiesta conferma del tasto F12                              *
042500030827      *---------------------------------------------------------------*
042600030827     c     GesW01        BegSr
042700030827      *
042800030827      * Inizializzazione videata
042900030827if  1c                   if        $InzW01 = *on
043000030827     c                   exsr      InzW01
043100030827     c                   movel     *off          $InzW01
043200030827e   1c                   endif
043300030827      *
043400030827     c                   exfmt     LR12W01
043500030827     c                   setoff                                       28  90
043600030827      *
043700030827sel 1c                   select
043800030827      * F12=Ritorno
043900030827w   1c                   when      *inKL
044000030827     c                   exsr      F12W01
044100030827     c                   goto      EndGesW01
044200030827      * F6=Conferma uscita
044300030827w   1c                   when      *inKF
044400030827     c                   exsr      F06W01
044500030827     c                   goto      EndGesW01
044600030827e   1c                   endsl
044700030827      *
044800030827     c     EndGesW01     EndSr
044900030827
045000030827      *---------------------------------------------------------------*
045100030827      * InzW01      > Inizializzazione videata W01                    *
045200030827      *---------------------------------------------------------------*
045300030827     c     InzW01        BegSr
045400030827      *
045500030924     c***                eval      W1sns  = 1
045600030924     c                   eval      W1Csns = 'S'
045700030827      *
045800030827     c                   EndSr
045900030827
046000030827      *---------------------------------------------------------------*
046100030827      * F06W01      > Tasto funzionale F6 = Conferma uscita           *
046200030827      *---------------------------------------------------------------*
046300030827     c     F06W01        BegSr
046400030827      *
046500030924      * Richiamo pgm di stampa
046600030925if  1c***                if        W1sns  = 1
046700030925if  1c                   if        W1Csns = 'S'
046800030924     c                   movel(p)  FNLR12ds      KPJBU
046900030924     c                   call      'FNLR12R1'
047000030924     c                   parm                    KPJBA
047100030925e   1c                   endif
047200030924      *
047300030827     c                   movel     *on           $Fine                          fine pgm
047400030827      *
047500030827     c                   EndSr
047600030827
047700030827      *---------------------------------------------------------------*
047800030827      * F12W01      > Tasto funzionale F12 = Ritorno                  *
047900030827      *---------------------------------------------------------------*
048000030827     c     F12W01        BegSr
048100030827      *
048200030827     c                   eval      $TipVid = '1'
048300030827     c                   eval      $InzD01 = *on
048400030827      *
048500030827     c                   EndSr
048600030827
048700030827      *---------------------------------------------------------------*
048800030827      * CtrD01      > Controllo dati della videata D01                *
048900030827      *---------------------------------------------------------------*
049000030827     c     CtrD01        BegSr
049100030827      *
049200030827     c                   movea     *zeros        *in(41)
049300030827      *
049400030827      * Se il campo V1Cpis è impostato, valorizzo i campi della spedi-
049500030827      *   zione solo se non sono stati impostati a mano o al giro di
049600030827      *   barcode che poi ha dato l'errore
049700030827     c                   clear                   BARcodeDS
049800030827if  1c                   if        V1Cpis > *zeros
049900030827     c                   movel     V1Cpis        BARcodeDS
050000030827if  2c                   if            V1Clnp = *zeros
050100030827     c                             and V1Cnrs = *zeros
050200030827     c                             and V1Cnsp = *zeros
050300030827     c                   z-add     BARcdeAAS     V1Caas
050400030916if  3c                   if        BARcdeAAS < 100
050500030916if  4c                   if        BARcdeAAS > 60
050600030827     c                   add       1900          V1Caas
050700030916x   4c                   else
050800030827     c                   add       2000          V1Caas
050900030916e   4c                   endif
051000030916e   3c                   endif
051100030827     c                   z-add     BARcdeLNP     V1Clnp
051200030827     c                   z-add     BARcdeNRS     V1Cnrs
051300030827     c                   z-add     BARcdeNSP     V1Cnsp
051400030827      * - controllo il check digits
051500030827     c                   exsr      sr_ChkDgt
051600030827     c     *in90         cabeq     *on           EndCtrD01
051700030827x   2c                   else
051800030827     c                   z-add     V1Caas        w0020
051900030827      * - impostati sia barcode che dati della spedizione
052000030827if  3c                   if           w0020  <> BARcdeAAS
052100030827     c                             or V1Clnp <> BARcdeLNP
052200030827     c                             or V1Cnrs <> BARcdeNRS
052300030827     c                             or V1Cnsp <> BARcdeNSP
052400030827     c                   seton                                        284590
052500030827     c                   movel     $Msg(02)      V1Dmsg
052600030827e   3c                   endif
052700030827e   2c                   endif
052800030827     c     *in90         cabeq     *on           EndCtrD01
052900030827e   1c                   endif
053000030924      *
053100030924      * Imposto l'anno a 4 cifre (se digitate 2 cifre)
053200030924if  1c                   if            V1Caas > *zeros
053300030924     c                             and V1Caas < 100
053400030924if  2c                   if        V1Caas > 60
053500030924     c                   add       1900          V1Caas
053600030924x   2c                   else
053700030924     c                   add       2000          V1Caas
053800030924e   2c                   endif
053900030924e   1c                   endif
054000030827      *
054100030827      * Controllo l'immissione dei dati indispensabili
054200030827sel 1c                   select
054300030916w   1c                   when           V1Caas <= *zeros
054400030924     c                             or   V1Caas <  (*Year - 2)
054500030924     c                             or   V1Caas >  (*Year + 1)
054600030827     c                   seton                                        28  90
054700030827     c  n31              eval      *in41  = *on
054800030827     c   31              eval      *in45  = *on
054900030827     c                   movel     $Msg(03)      V1Dmsg
055000030827w   1c                   when      V1Clnp <= *zeros
055100030827     c                   seton                                        28  90
055200030827     c  n31              eval      *in42  = *on
055300030827     c   31              eval      *in45  = *on
055400030827     c                   movel     $Msg(04)      V1Dmsg
055500030827w   1c                   when      V1Cnsp <= *zeros
055600030827     c                   seton                                        28  90
055700030827     c  n31              eval      *in44  = *on
055800030827     c   31              eval      *in45  = *on
055900030827     c                   movel     $Msg(05)      V1Dmsg
056000030827e   1c                   endsl
056100030827     c     *in90         cabeq     *on           EndCtrD01
056200030827      *
056300030827     c     EndCtrD01     EndSr
056400030827
056500030827      *---------------------------------------------------------------*
056600030827      * GesWFCBB    > Verifica esistenza bolla nel file di lavoro     *
056700030827      *---------------------------------------------------------------*
056800030827     c     GesWFCBB      BegSr
056900030827      *
057000030827     c                   clear                   WFCBB000
057100030827      *
057200030827      * Controllo esistenza su WFCBB01L
057300030827     c                   z-add     V1Caas        CBBaas
057400030827     c                   z-add     V1Clnp        CBBlnp
057500030827     c                   z-add     V1Cnrs        CBBnrs
057600030827     c                   z-add     V1Cnsp        CBBnsp
057700030827     c     k04cbb01      chain     WFCBB000
057800030827      *
057900030827if  1c                   if        %found(WFCBB01L)
058000030827      *
058100030827     c                   exsr      sr_Y_Found
058200030827      *
058300030827x   1c                   else
058400030827      *
058500030827     c                   exsr      sr_N_Found
058600030827      *
058700030827e   1c                   endif
058800030827      *
058900030827      * Se non sono stati rilevati errori ripulisco la videata
059000030827      *   altrimenti ripulisco solo i campi impostati dal pgm.
059100030827      *   (se letto il barcode)
059200030827sel 1c                   select
059300030827w   1c                   when      *in90   = *off
059400030827     c                   eval      $InzD01 = *on
059500030827w   1c                   when      *in31   = *on
059600030827     c                   clear                   V1Clnp
059700030827     c                   clear                   V1Cnrs
059800030827     c                   clear                   V1Cnsp
059900030827e   1c                   endsl
060000030827      *
060100030827     c                   EndSr
060200030827
060300030827      *---------------------------------------------------------------*
060400030827      * sr_Y_Found  > Gestione bolla trovata nel file WFCBB01L        *
060500030827      *---------------------------------------------------------------*
060600030827     c     sr_Y_Found    BegSr
060700030827      *
060800030827sel 1c                   select
060900030827      *
061000030827      * Aggiorno record se NON già aggiornato
061100030827w   1c                   when      CBBerr = *blanks
061200030827     c                   eval      CBBerr = 'T'
061300030827     c                   UPDATE    WFCBB000
061400030827      *
061500030827      * Emetto errore se già impostato (e non aggiorno)
061600030827w   1c                   when      CBBerr = 'E'
061700030827     c                   movel     CBBmsg        V1Dmsg
061800030827     c                   seton                                        28  90
061900030827     c  n31              eval      *in41  = *on
062000030827     c   31              eval      *in45  = *on
062100030827      *
062200030827e   1c                   endsl
062300030827      *
062400030827     c                   EndSr
062500030827
062600030827      *---------------------------------------------------------------*
062700030827      * sr_N_Found  > Gestione bolla NON trovata nel file WFCBB01L    *
062800030827      *---------------------------------------------------------------*
062900030827     c     sr_N_Found    BegSr
063000030827      *
063100030827      * Reperisco dati da FNARB
063200030827     c     k04cbb01      chain     FNARB000
063300030827      *
063400030827      * Spedizione non trovata neppure in FNARB
063500030827if  1c                   if        NOT %found(FNARB01L)
063600030827     c                   seton                                        28  90
063700030827     c  n31              eval      *in42  =  *on
063800030827     c   31              eval      *in45  =  *on
063900030827     c                   movel     $Msg(06)      V1Dmsg
064000030827     c                   goto      End_N_Found
064100030827e   1c                   endif
064200030917      *
064300030917      * Imposto i dati per il nuovo record nel WF
064400030917     c                   eval      CBBmgs = ARBmgs
064500030917     c                   eval      CBBncl = ARBncl
064600030917     c                   eval      CBBpkf = ARBpkf
064700030917     c                   eval      CBBvlf = ARBvlf
064800030917     c                   eval      CBBrsd = ARBrsd
064900030917     c                   eval      CBBind = ARBind
065000030917     c                   eval      CBBcad = ARBcad
065100030917     c                   eval      CBBlod = ARBlod
065200030917     c                   eval      CBBprd = ARBprd
065300030917     c                   eval      CBBnzd = ARBnzd
065400031001     c                   eval      CBBtcr = ARBtcr
065500031001     c                   eval      CBBdcr = ARBdcr
065600030827      *
065700030827      * P.O. di arrivo non in £6
065800030827if  1c                   if        ARBlna <> DUTpou
065900030827     c                   eval      *in10  =  *off
066000030827     c     ARBlna        lookup    L6                                     10
066100030827if  2c                   if        NOT *in10
066200030827     c                   exsr      DecodLNA
066300030827     c                   seton                                        28  90
066400030827     c  n31              eval      *in42  =  *on
066500030827     c   31              eval      *in45  =  *on
066600030827     c                   movel     $Msg(07)      V1Dmsg
066700030827     c                   move      ARBlna        w003a
066800030827     c                   eval      V1Dmsg =  %replace(w003a:V1Dmsg:
066900030827     c                                       %scan('&PO':V1Dmsg))
067000030827     c                   eval      V1Dmsg =  %replace(ORGdes:V1Dmsg:
067100030827     c                                       %scan('&DES':V1Dmsg))
067200030827     c                   goto      End_N_Found
067300030827e   2c                   endif
067400030827e   1c                   endif
067500030916      *
067600030916      * Verifica se sono già state aperte C.A.
067700030916     c                   exsr      CtrExistCA
067800030916if  1c                   if        $ExistCA  = *on
067900030916     c                   seton                                        28  90
068000030916     c  n31              eval      *in42  =  *on
068100030916     c   31              eval      *in45  =  *on
068200031003     c                   movel     $Msg(15)      V1Dmsg
068300030916     c                   eval      V1Dmsg =  %replace(
068400030916     c                                       %editc(DCTaac:'Z'):
068500030916     c                                       V1Dmsg:
068600030916     c                                       %scan('&AAC':V1Dmsg))
068700030916     c                   move      DCTfil        w003a
068800030917     c                   eval      V1Dmsg =  %replace(%trim(
068900030917     c                                       %editw(DCTfil:'0   ')):
069000030916     c                                       V1Dmsg:
069100030916     c                                       %scan('&PO':V1Dmsg))
069200030916     c                   eval      V1Dmsg =  %replace(
069300030917     c                                       (%editc(DCTnca:'Z')):
069400030916     c                                       V1Dmsg:
069500030916     c                                       %scan('&NCA':V1Dmsg))
069600030916     c                   goto      End_N_Found
069700030916e   1c                   endif
069800030827      *
069900030827      * Verifica consegna
070000030827if  1c                   if        ARBdcm >  *zeros
070100030827     c                   seton                                        28  90
070200030827     c  n31              eval      *in42  =  *on
070300030827     c   31              eval      *in45  =  *on
070400030827     c                   exsr      CtrSoloInca
070500030827      * - Solo incasso
070600030827if  2c                   if        $SoloInca = *on
070700030827     c                   movel     $Msg(08)      V1Dmsg
070800030827x   2c                   else
070900030827      * - Già consegnata
071000030827     c                   clear                   WLBda8
071100031003     c                   move      ARBddc        G02inv
071200030827     c                   movel     '3'           G02err
071300030827     c                   call      'XSRDA8'
071400030827     c                   parm                    WLBda8
071500030827     c                   movel     $Msg(09)      V1Dmsg
071600030827     c                   eval      V1Dmsg =  %replace(
071700030827     c                                       %editc(ARBndc:'Z'):
071800030827     c                                       V1Dmsg:
071900030827     c                                       %scan('&ARBNDC':V1Dmsg))
072000030827     c                   eval      V1Dmsg =  %replace(
072100030827     c                                       %editc(G02dat:'Y'):
072200030827     c                                       V1Dmsg:
072300031003     c                                       %scan('&ARBDDC':V1Dmsg))
072400030827e   2c                   endif
072500030827     c                   goto      End_N_Found
072600031003      *
072700031003x   1c                   else
072800031003      * - In consegna
072900031003if  2c                   if        ARBndc >  *zeros
073000031003     c                   seton                                        28  90
073100031003     c  n31              eval      *in42  =  *on
073200031003     c   31              eval      *in45  =  *on
073300031003     c                   clear                   WLBda8
073400031003     c                   move      ARBddc        G02inv
073500031003     c                   movel     '3'           G02err
073600031003     c                   call      'XSRDA8'
073700031003     c                   parm                    WLBda8
073800031003     c                   movel     $Msg(10)      V1Dmsg
073900031003     c                   eval      V1Dmsg =  %replace(
074000031003     c                                       %editc(ARBndc:'Z'):
074100031003     c                                       V1Dmsg:
074200031003     c                                       %scan('&ARBNDC':V1Dmsg))
074300031003     c                   eval      V1Dmsg =  %replace(
074400031003     c                                       %editc(G02dat:'Y'):
074500031003     c                                       V1Dmsg:
074600031003     c                                       %scan('&ARBDDC':V1Dmsg))
074700031003     c                   goto      End_N_Found
074800031003e   2c                   endif
074900030827e   1c                   endif
075000030827      *
075100030827      * Verifica se spedizione bloccata
075200030827if  1c                   if        ARBfbc <> *blanks
075300030827     c                   seton                                        28  90
075400030827     c  n31              eval      *in42  =  *on
075500030827     c   31              eval      *in45  =  *on
075600030827sel 2c                   select
075700030827w   2c                   when      ARBfbc =  'A'
075800031003     c                   movel     $Msg(11)      V1Dmsg
075900030827w   2c                   when      ARBfbc =  'G'
076000031003     c                   movel     $Msg(12)      V1Dmsg
076100030827w   2c                   other
076200031003     c                   movel     $Msg(13)      V1Dmsg
076300030827e   2c                   endsl
076400030827     c                   goto      End_N_Found
076500030827e   1c                   endif
076600030827      *
076700030827      * Verifica se bolla di recupero
076800030827     c                   exsr      CtrRecupero
076900030827if  1c                   if        $Recupero = *on
077000030827     c                   seton                                        28  90
077100030827     c  n31              eval      *in42  =  *on
077200030827     c   31              eval      *in45  =  *on
077300031003     c                   movel     $Msg(14)      V1Dmsg
077400030827     c                   goto      End_N_Found
077500030827e   1c                   endif
077600030827      *
077700030827      * Superati tutti i controlli precedenti
077800030827      * (ma bolla non trovata nel file di lavoro)
077900030827     c                   seton                                        28  90
078000030827     c  n31              eval      *in42  =  *on
078100030827     c   31              eval      *in45  =  *on
078200031003     c                   movel     $Msg(16)      V1Dmsg
078300030827     c                   goto      End_N_Found
078400030827      *
078500030827     c     End_N_Found   tag
078600030827      *
078700030827      * Scrivo il record nel file di lavoro con messaggio di errore
078800030827     c                   eval      CBBerr =  'E'
078900030827     c                   eval      CBBmsg =  V1Dmsg
079000030827     c                   WRITE     WFCBB000
079100030827      *
079200030827     c                   EndSr
079300030827
079400030827      *---------------------------------------------------------------*
079500030827      * CtrSoloInca > Verifica se bolla di solo incasso               *
079600030827      *---------------------------------------------------------------*
079700030827     c     CtrSoloInca   BegSr
079800030827      *
079900030827     c                   reset                   $SoloInca
080000030827     c                   clear                   ds3A
080100030827      *
080200030827      *?SOLO INCASSO - C/ASSEGNO?
080300030827      *
080400030827      * Controllo se la bolla in esame ha legame con seconda bolla
080500030827     c     k04cbb01      chain     FNLBL000
080600030827      *
080700030827if  1c                   if        %found(FNLBL01L)
080800030827     c                             and LBLlan  = LBLlap
080900030827      *
081000030827      * > Se ho una bolla "mamma" ed è locale
081100030827     c     k04arb_1      chain     FNARB000
081200030827if  2c                   if        %found(FNARB01L)
081300030827      *
081400051114     c     K04arb_1      chain     FiAR9000
081500051114if  3c                   if             %found(FiAR901L)
081600030827     c                             and (ARBicc = *blanks
081700030827     c                              or  ARBicc = 'R')
081800030827     c                   eval      $SoloInca   = *on
081900030827e   3c                   endif
082000030827      *
082100030827e   2c                   endif
082200030827      *
082300030827      *   Mi sono "sporcato" FNARB, quindi riaggancio la figlia
082400030827     c     k04cbb01      chain     FNARB000
082500030827      *
082600030827x   1c                   else
082700030827      *
082800030827      * > Se non c'è legame controllo il c/a della bolla che sto
082900030827      *   elaborando
083000051114     c     k04cbb01      chain     FiAR9000
083100051114if  2c                   if             %found(FiAR901L)
083200030827     c                             and (ARBicc = *blanks
083300030827     c                              or  ARBicc = 'R')
083400030827     c                   eval      $SoloInca   = *on
083500030827e   2c                   endif
083600030827      *
083700030827e   1c                   endif
083800030827      *
083900030827     c     $SoloInca     cabeq     *on           EndSoloInca
084000030827      *
084100030827      *?SOLO INCASSO - FATTURA PORTO ASSEGNATO (PREPAGATO)?
084200030827      *
084300030827      * Decodifico il tipo bolla
084400030827     c                   movel     '3A'          TBLcod
084500030827     c                   movel(p)  ARBcbo        TBLkey
084600030827     c     kTABEL        chain     TABEL00F
084700030827if  1c                   if            %found(TABEL00F)
084800030827     c                             and TBLflg  = *blanks
084900030827     c                   movel     TBLuni        ds3A
085000030827e   1c                   endif
085100030827      *
085200030827      * Aggancio i dati di tassazione bolla
085300030827     c                   clear                   AR6ift
085400030827     c                   clear                   AR6div
085500030827if  1c                   if            §3Atb2  = *blanks
085600030827     c                             and %subst(§3Atb1:1:1) = 'A'
085700030827     c                   eval      AR6trc = '1'
085800030827     c     k05ar601      chain     FIAR6000
085900030827x   1c                   else
086000030827      * - 2ª bolla
086100030827if  2c                   if            §3Atb2 <> *blanks
086200030827     c                   eval      AR6trc = '2'
086300030827     c     k05ar601      chain     FIAR6000
086400030827e   2c                   endif
086500030827e   1c                   endif
086600030827      *
086700030827if  1c                   if             %found(FIAR601L)
086800030827     c                             and  AR6ift > *zeros
086900030827     c                             and (ARBfip = 'I'
087000030827     c                              or  ARBica = *blanks
087100030827     c                              or  ARBica = 'R')
087200030827     c                   eval      $SoloInca   = *on
087300030827e   1c                   endif
087400030827      *
087500030827     c     $SoloInca     cabeq     *on           EndSoloInca
087600030827      *
087700030827     c     EndSoloInca   EndSr
087800030827
087900030827      *---------------------------------------------------------------*
088000030827      * CtrRecupero > Verifica se bolla di recupero                   *
088100030827      *---------------------------------------------------------------*
088200030827     c     CtrRecupero   BegSr
088300030827      *
088400030827     c                   reset                   $Recupero
088500030827     c                   clear                   ds3A
088600030827      *
088700030827      *?BOLLA DI RECUPERO?
088800030827      *
088900030827if  1c                   IF            ARBdcm = *zeros
089000030827     c                             and ARBndc = *zeros
089100030827      *
089200030827      * Decodifico il tipo bolla
089300030827     c                   movel     '3A'          TBLcod
089400030827     c                   movel(p)  ARBcbo        TBLkey
089500030827     c     kTABEL        chain     TABEL
089600030827if  2c                   if            %found(TABEL00F)
089700030827     c                             and TBLflg = *blanks
089800030827     c                   movel     TBLuni        ds3A
089900030827e   2c                   endif
090000030827      *
090100030827if  2c                   if        §3Arbl  = 'R'
090200030827      *
090300030827     c     K04cbb01      setll     FNLBL000
090400030827      * Se non trovati legami memorizzo che è una bolla di recupero
090500030827if  3c                   if        NOT %equal(FNLBL01L)
090600030827     c                   eval      $Recupero  = *on
090700030827e   3c                   endif
090800030827      *
090900030827e   2c                   endif
091000030827      *
091100030827e   1c                   endif
091200030827      *
091300030827     c                   EndSr
091400030827
091500030827      *---------------------------------------------------------------*
091600030827      * CtrExistCA  > Verifica se ci sono C.A. aperte                 *
091700030827      *---------------------------------------------------------------*
091800030827     c     CtrExistCA    BegSr
091900030827      *
092000030827     c                   reset                   $ExistCA
092100030827      *
092200040806     c**""**     k04cbb01      setll     FNDCT000
092300040806     c**""**     k04cbb01      reade     FNDCT000
092400030827      *
092500040806do  1c**""**                   dow       NOT %eof(FNDCT02L)
092600030827      *
092700040806if  2c**""**                   if            DCTatb = *blanks
092800040806     c**""**                             and DCTdch = *zeros
092900040806     c**""**                   eval      $ExistCA   = *on
093000040806     c**""**                   leave
093100040806e   2c**""**                   endif
093200030827      *
093300040806     c**""**     k04cbb01      reade     FNDCT000
093400040806e   1c**""**                   enddo
093500040806      *
093600040806      * Sostituisco la lettura del file FNDTC02 con
093700040806      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e aperte
093800040806      * legate alla spedizione che passo
093900040806      *
094000040806     c                   clear                   fidn12ds
094100040806     c                   eval      i12aas = CBBaas
094200040806     c                   eval      i12lnp = CBBlnp
094300040806     c                   eval      i12nrs = CBBnrs
094400040806     c                   eval      i12nsp = CBBnsp
094500040806     c                   eval      i12fel = 'E'
094600040806     c                   eval      i12fan = 'E'
094700040806     c                   eval      i12fch = 'E'
094800040806      *
094900040806     c                   call      'FIDN12R'
095000040806     c                   parm                    fidn12ds
095100040806      *
095200040806      * se non ci sono errori
095300040806     c                   if        o12err = ' '
095400040806      * se numero di CA trovate maggiore di zero
095500040806     c                   if        o12nca > 0
095600040806     c                   eval      $ExistCA   = *on
095700040806     c                   movea     skkey(1)      dskey
095800040806     c                   endif
095900040806      *
096000040806     c                   endif
096100030827      *
096200030827     c                   EndSr
096300030827
096400030827      *---------------------------------------------------------------*
096500030827      * sr_ChkDgt  > Controllo il check digit del barcode             *
096600030827      *---------------------------------------------------------------*
096700030827     c     sr_ChkDgt     BegSr
096800030827      *
096900030827     c                   reset                   TRUL28ds
097000030827     c                   movel     BARcodeDS     I28cod
097100030827     c                   call      'TRUL28R1'
097200030827     c                   parm                    TRUL28ds
097300030827      *
097400030827      * se rilevato errore o se check digit errato lo segnalo
097500030827     c                   movel(p)  V1Cpis        W1Cpis
097600030827if  1c                   if           O28err <> *blanks
097700030827     c                             or O28cod <> W1Cpis
097800030827     c                   seton                                        284590
097900030827     c                   movel     $Msg(01)      V1Dmsg
098000030827e   1c                   endif
098100030827      *
098200030827     c                   EndSr
098300030827
098400030827      *---------------------------------------------------------------*
098500030827      * DecodLNA   > Decodificas P.O. di arrivo                       *
098600030827      *---------------------------------------------------------------*
098700030827     c     DecodLNA      BegSr
098800030827      *
098900030827     c     ARBlna        chain     AZORG
099000030827if  1c                   if        not %found(AZORG01L)
099100030827     c                             or  ORGfva = '*'
099200030827     c                   movel     *all'? '      ORGdes
099300030827e   1c                   endif
099400030827      *
099500030827     c                   EndSr
099600030908
099700030908      *---------------------------------------------------------------*
099800030908      * Eseguo le override                                            *
099900030908      *---------------------------------------------------------------*
100000030908
100100030908     c     SR_x_Cmd      BEGSR
100200030908      *
100300030908      * impostazione comando da eseguire
100400030908     c                   eval      Qcmd = %trim($Cmd(1)) + ' '
100500030908     c                                  + %trim($Cmd(2))
100600030908      * - nome del file
100700030908     c                   eval      Qcmd = %replace(wFILE:Qcmd:
100800030908     c                                    %scan('&FILE':Qcmd))
100900030908      * - nome della coda di stampa
101000030908     c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
101100030908     c                                    %scan('&OUTQ':Qcmd))
101200030908      * - tipo di modulo
101300030908     c                   eval      Qcmd = %replace(wFORM:Qcmd:
101400030908     c                                    %scan('&FORMTYPE':Qcmd))
101500030908      * - dati utente
101600030908     c                   eval      Qcmd = %replace(wUSRD:Qcmd:
101700030908     c                                    %scan('&USRDTA':Qcmd))
101800030908      *
101900030908     c                   call      'QCMDEXC'                            90
102000030908     c                   parm                    Qcmd
102100030908     c                   parm                    Qlen
102200030908      *
102300030908     c                   ENDSR
102400030827
102500030827      *---------------------------------------------------------------*
102600030827      * DefKList   > Definizione KEY-LIST                             *
102700030827      *---------------------------------------------------------------*
102800030827     c     DefKList      Begsr
102900030827      *
103000030827     c     k04cbb01      klist
103100030827     c                   kfld                    CBBaas
103200030827     c                   kfld                    CBBlnp
103300030827     c                   kfld                    CBBnrs
103400030827     c                   kfld                    CBBnsp
103500030827      *
103600030827     c     k04arb_1      klist
103700030827     c                   kfld                    LBLaap
103800030827     c                   kfld                    LBLlpp
103900030827     c                   kfld                    LBLnrp
104000030827     c                   kfld                    LBLnsp
104100030827      *
104200030827     c     k05ar601      klist
104300030827     c                   kfld                    CBBaas
104400030827     c                   kfld                    CBBlnp
104500030827     c                   kfld                    CBBnrs
104600030827     c                   kfld                    CBBnsp
104700030827     c                   kfld                    AR6trc
104800030827      *
104900030827     c     kTABEL        klist
105000030827     c                   kfld                    TBLkut
105100030827     c                   kfld                    TBLcod
105200030827     c                   kfld                    TBLkey
105300030827      *
105400030827     c                   EndSr
105500030908
105600030908**  $Cmd
105700030908OVRPRTF    FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
105800030908           USRDTA('&USRDTA   ') SHARE(*YES)
105900030925           COPIES(2)
106000030917**  $MSG                                                        *
106100030924Errore Barcode                                                                  1
106200030924Scegliere spedizione o barcode                                                  2
106300030924Anno errato                                                                     3
106400030924P.O. di partenza errato                                                         4
106500030924Immettere il numero di spedizione                                               5
106600030924MANCA RECORD BOLLA - VERIFICARE P.O. ARRIVO DELLA SPEDIZIONE                    6
106700030924DDT ECCEDENTE - P.O. D'ARRIVO: &PO  &DES                                        7
106800030924SPEDIZIONE SOLO INCASSO                                                         8
106900031003GIA' CONSEGNATA. DISTINTA N. &ARBNDC DEL &ARBDDC                                9
107000031003IN CONSEGNA. DISTINTA N. &ARBNDC DEL &ARBDDC                                   10
107100031003LASCIATO AVVISO                                                                11
107200031003SPEDIZIONE GIACENTE                                                            12
107300031003SPEDIZIONE BLOCCATA                                                            13
107400031003SPEDIZIONE DI RECUPERO                                                         14
107500031003PRATICA DI DANNO APERTA: C.A. NUMERO &AAC/&PO/&NCA                             15
107600031003SPEDIZIONE DA NON CONSEGNARE - CONTROLLARE                                     16
