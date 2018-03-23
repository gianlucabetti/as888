000100050713      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050713      *===============================================================*
000300051025      *?FIDN89R * Ricezione recupero da A.I.G.                       ?*
000400050713      *===============================================================*
000500050713
000600050713     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050713
000800050713      *---------------------------------------------------------------*
000900050713
001000050726     fFNDCT01L  if   e           k disk
001100050727     fFNDET01L  if   e           k disk
001200050802     fFNDCL01L  if   e           k disk
001300050726     fTITAS30C  if   e           k disk
001400050915      *
001500050915     fFNDKA01L  if   e           k disk
001600050915     fFNSPE01L  if   e           k disk
001700050915     fTNTMD01L  if   e           k disk
001800050714      *
001900050725     fFNDRA01L  Uf A e           k disk
002000050725      *
002100050726     fFIDN89D   cf   e             workstn
002200050713
002300050713      *---------------------------------------------------------------*
002400050713
002500050713      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002600050713      *
002700050713     d DigitN          c                   const('0123456789')
002800050721     d C_GMALimite     c                   const(18042005)
002900050713     d C_DataLimite    c                   const(20050418)
003000050715     d C_AnnoLimite    c                   const(2005)
003100050715     d C_MmGgLimite    c                   const(0418)
003200050714      * - Fasi C.A. fissate
003300050714     d C_F_ChiusCA     c                   const(900)
003400050713      *
003500050713      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003600050713      *
003700050714      * - Causali Chiusura C.A. - di Pagamento
003800050714     d $CCHpag         s                   like(DCTcch)
003900050714     d                                     dim(10) ctdata  perrcd(10)
004000050713      * - Messaggi di errore
004100051006     d $Msg            s             78    dim(15) ctdata  perrcd(1)
004200050713      *
004300050713      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004400050713      *
004500050713     d KPJBA         e ds
004600050725     d FIDN00ds      e ds                  inz
004700050725     d  I00mod       e                     inz('I')
004800050725     d  I00tpb       e                     inz('S')
004900050713      * - Parametri x Controllo profilo utenti
005000050713     d TIBS34DS      e ds                  inz
005100050713      * - Ds di riferimento al file esterno AZUTE00F
005200050713     d AZUTEds       e ds                  extname(AZUTE00F)
005300050713      * - Ds per dati organigramma
005400050713     d DDatiUte      e ds
005500050713      * - Parametri x Interrogazione tabelle
005600050713     d TIBS02DS      e ds                  inz
005700050713     d   T02mod      e                     inz('C')
005800050713     d   T02cod      e                     inz('TAD')
005900050714      * - Tab. "GED" (DN)
006000050725     d dGEDdn        e ds                  inz
006100050714      * - Tab. "STD"
006200050714     d dSTD          e ds                  inz
006300050713      * - Tab. "TAD"
006400050713     d dTAD          e ds                  inz
006500050713      *
006600050713      * - Ricerca Eventi
006700050726     d FIDN07ds      e ds                  inz
006800050713     d  I07mod       e                     inz('R')
006900050725      *
007000050725      * Gestione note C.A.
007100050725     d FIDN10ds      e ds                  inz
007200050725     d  I10flm       e                     inz('V')
007300050725     d  I10tpd       e                     inz('C')
007400050725     d  I10trc       e                     inz('N')
007500050715      *
007600050715      * - Trk (di comodo - per clear "unici")
007700050715     d FNDRAds       e ds                  extname(FNDRA00F) inz
007800050915      *
007900050915     d Ddct01        e ds                  inz
008000050915     d TIBS69ds      e ds                  inz
008100050915     d   I69tla      e                     inz('L')
008200050915     d ds_CNACO      e ds                  extname(CNACO00F) inz
008300050915     d ds_CNIND      e ds                  extname(CNIND00F) inz
008400050915     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
008500050915     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
008600050713      *
008700050713     d Status         sds           333
008800050713     d   SDSpgm          *proc
008900050713      *
009000050713      * - Controllo/Inversione date
009100050713     d WLBdat          ds                  inz
009200050713     d   G08dat                       8  0 inz
009300050713     d   G08inv                       8  0 inz
009400050713     d   G08err                       1    inz('3')
009500050713     d   G08tgi                       5  0 inz
009600050713      *
009700050713      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
009800050713      *
009900050715      * - Flags
010000050725     d $Called         s              1    inz(*off)
010100050713     d $Fine           s              1    inz(*off)
010200050726     d $InzD01         s              1    inz(*on)
010300050726     d $InzD02         s              1    inz(*on)
010400050915     d $Rivalsa        s              1    inz(*off)
010500050713      *
010600050726     d $Video          s              1    inz('2')
010700050725     d $UpdDRA         s              1    inz
010800050713      *
010900050713      * - Indici di schiera
011000050713      *
011100050713      * - Variabili riferite al data base o al display file
011200050915     d W2Cnev          s                   like(DETnev)   inz
011300050915     d W§DCTport       s                   like(§DCTport) inz
011400050915     d Wksc            s                   like(TASksc)   inz
011500050713      *
011600050713      * - Variabili definite a programma
011700050802     d W2Cdrr          s              8  0 inz
011800050726     d W2Cdvr          s              8  0 inz
011900050919     d Wdate           s              8  0 inz
012000050915     d W0040           s              4  0 inz
012100050915     d WPOintes        s              3  0 inz
012200050715     d Wcampo          s             10    inz
012300050713      *
012400050713      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
012500050713      *
012600050725      * - FNDRA01L / FNDCL01L
012700050725     c     K03DRA01      klist
012800050713     c                   kfld                    DCTaac
012900050713     c                   kfld                    DCTfil
013000050713     c                   kfld                    DCTnca
013100050727      * - FNDET01L
013200050727     c     K02DET01      klist
013300050727     c                   kfld                    DCTaae
013400050727     c                   kfld                    DCTnev
013500050726      * - TITAS30C
013600050726     c     K04TAS30      klist
013700050726     c                   kfld                    DCTaas
013800050726     c                   kfld                    DCTlnp
013900050726     c                   kfld                    DCTnrs
014000050726     c                   kfld                    DCTnsp
014100050915      * - FNDKA01L
014200050915     c     K04DKA01      klist
014300050915     c                   kfld                    DCTaac
014400050915     c                   kfld                    DCTfil
014500050915     c                   kfld                    DCTnca
014600050915     c                   kfld                    DKAtrc
014700050915      * - FNSPE01L
014800050915     c     K03SPE01      klist
014900050915     c                   kfld                    SPEfls
015000050915     c                   kfld                    V2Cksc
015100050915     c                   kfld                    SPEcod
015200050713
015300050713      *---------------------------------------------------------------*
015400050713      *  RIEPILOGO INDICATORI                                         *
015500050713      *---------------------------------------------------------------*
015600050713      * 10    - Comodo                                                *
015700050713      * 28    - Emette il messaggio di errore a video                 *
015800050914      * 40    - in D2: non consente F3=Fine                           *
015900050804      * 41    - in D2: visualizza estremi EVENTO                      *
016000050804      * 42    - in D2: visualizza estremi PRATICA                     *
016100051025      * 49on  - in D2: protezione Data Richiesta Recupero             *
016200050802      * 49off - in D2: richiede immissione data in D2CDRR             *
016300050728      * 51    - in D1: Numero Evento errato                           *
016400050728      * 52    - in D1: Anno   Evento errato                           *
016500051025      * 51    - in D2: Data ricezione recupero errata                 *
016600051025      * 52    - in D2: Importo recupero ricevuto errato               *
016700050713      * 90    - Generico di errore                                    *
016800050713      *---------------------------------------------------------------*
016900050713
017000050713     c     *Entry        plist
017100050713     c                   parm                    KPJBA
017200050713      *
017300050713      * Operazioni Iniziali
017400050713     c                   exsr      RoutInz
017500050713      *
017600050713      * Gestione Video
017700050715do  1c                   DOW       $Fine    = *off
017800050713      * - Filtro di lancio
017900050726cas 2c     $Video        caseq     '1'           GesD01
018000050725      * - Gestione singola C.A.
018100050726cas 2c     $Video        caseq     '2'           GesD02
018200050713e   2c                   endcs
018300050713e   1c                   ENDDO
018400050713      *
018500050713      * Fine
018600050713     c                   clear                   TIBS02ds
018700050715     c                   eval      T02tla   = 'C'
018800050713     c                   call      'TIBS02R'
018900050713     c                   parm                    KPJBA
019000050713     c                   parm                    TIBS02ds
019100050713     c                   movel     *on           *inLR
019200050713      *
019300050713      *---------------------------------------------------------------*
019400050713      * Operazioni Iniziali                                           *
019500050713      *---------------------------------------------------------------*
019600050713     c     RoutInz       BEGSR
019700050713      *
019800050713      * Reperisco dati job
019900050713     c                   exsr      DatiJob
020000050713      *
020100050713     c                   movel     SDSpgm        V1Tpgm
020200050919     c                   clear                   WLBdat
020300050919     c                   eval      G08dat   = *date
020400050919     c                   call      'XSRDA8'
020500050919     c                   parm                    WLBdat
020600050919     c                   eval      wDate    = G08inv
020700050714      *
020800050714      * Aggancio tabella GED (Gestionale: Divise)
020900050714     c                   clear                   dGEDdn
021000050714     c                   reset                   TIBS02ds
021100050715     c                   eval      T02sif   = KNSIF
021200050715     c                   eval      T02cod   = 'GED'
021300050714     c                   movel(p)  'DANNI'       T02ke1
021400050714     c                   call      'TIBS02R'
021500050714     c                   parm                    KPJBA
021600050714     c                   parm                    TIBS02ds
021700050715     c                   if        T02err   = *blanks
021800050714     c                   movel     T02uni        dGEDdn
021900050714     c                   endif
022000050714      * Aggancio tabella STD (Standard Danni)
022100050714     c                   clear                   dTAD
022200050714     c                   reset                   TIBS02ds
022300050715     c                   eval      T02sif   = KNSIF
022400050715     c                   eval      T02cod   = 'STD'
022500050714     c                   movel(p)  '1'           T02ke1
022600050714     c                   movel(p)  §GEDdba       T02ke2
022700050714     c                   call      'TIBS02R'
022800050714     c                   parm                    KPJBA
022900050714     c                   parm                    TIBS02ds
023000050715     c                   if        T02err   = *blanks
023100050714     c                   movel     T02uni        dSTD
023200050714     c                   endif
023300050725      *
023400050725      * Determinazione della videata iniziale da gestire
023500050725if  1c                   if        KPJBU    = *blanks
023600050725     c                   eval      $Called  = *off
023700050725     c                   eval      $Video   = '1'
023800050725x   1c                   else
023900050725     c                   eval      $Called  = *on
024000050725     c                   movel     KPJBU         FIDN00ds
024100050726     c                   eval      $Video   = '2'
024200050727     c***                move      I00nev        V1Cnev
024300050915     c***                z-add     I00aae        V1Caae
024400050726     c                   z-add     I00aac        V1Caac
024500050726     c                   z-add     I00fil        V1Cfil
024600050726     c                   z-add     I00nca        V1Cnca
024700050725     c                   exsr      CtrD01
024800051006if  2c                   if        *in28
024900051006     c                   eval      O00err   = 'E'
025000051006     c                   eval      O00msg   = V1Dmsg
025100051006     c                   movel(p)  FIDN00ds      KPJBU
025200051006     c                   eval      $Fine    = *on
025300051006e   2c                   endif
025400051006     c***                setoff                                       28  90
025500051006     c***                clear                   V1Dmsg
025600050725e   1c                   endif
025700050713      *
025800050713     c                   ENDSR
025900050713      *
026000050713      *---------------------------------------------------------------*
026100050713      * Reperimento Dati del job (Utente/Operativi)                   *
026200050713      *---------------------------------------------------------------*
026300050713     c     DatiJob       BEGSR
026400050713      *
026500050713     c     *dtaara       define    §azute        azuteds
026600050713     c     *dtaara       define    §datiute      ddatiute
026700050713      *
026800050713     c                   in(E)     *dtaara
026900050713     c                   IF        %ERROR or RSUT = *blanks
027000050713     c                   clear                   Tibs34Ds
027100050713     c                   call      'TIBS34R'
027200050713     c                   parm                    Tibs34Ds
027300050713     c                   in        *dtaara
027400050713     c                   ENDIF
027500050713      *
027600050713     c                   ENDSR
027700050713      *
027800050713      *---------------------------------------------------------------*
027900050726      * Gestione videata D01                                          *
028000050713      *---------------------------------------------------------------*
028100050726     c     GesD01        BEGSR
028200050713      *
028300050713      * Inizializzo la videata
028400050726if  1c                   if        $InzD01  = *on
028500050726     c                   exsr      InzD01
028600050726     c                   eval      $InzD01  = *off
028700050713e   1c                   endif
028800050713      *
028900050713      * Scrivo la testata e la riga tasti funzionali abilitati
029000050713if  1c                   if        NOT *in90
029100050726     c                   write     FI89T01
029200050726     c                   write     FI89Z01
029300050713e   1c                   endif
029400050713      * Emetto la videata
029500050726     c                   exfmt     FI89D01
029600050713     c                   setoff                                       28  90
029700050713     c                   clear                   V1Dmsg
029800050713      *
029900050915sel 1c                   select
030000050713      * F3=Fine
030100050915w   1c                   when      *inKC
030200050713     c                   exsr      F03D01
030300050713     c                   leavesr
030400050915      * F14=Interrogazione C.A.
030500050915w   1c                   when          *inKN
030600050915     c                             and V1Cfil <> *zeros
030700050915     c                             and V1Cnca <> *zeros
030800050915     c                             and V1Caac <> *zeros
030900050915     c                   exsr      F14D01
031000050915     c                   leavesr
031100050915e   1c                   endsl
031200050713      *
031300050713      * Controllo dati immessi a video
031400050713     c                   exsr      CtrD01
031500050713     c     *in90         cabeq     *on           EndGesD01
031600050713      *
031700050726     c                   eval      $InzD02  = *on
031800050726     c                   eval      $Video   = '2'
031900050713      *
032000050713     c     EndGesD01     ENDSR
032100050713      *
032200050713      *---------------------------------------------------------------*
032300050726      * Inizializzazione videata D01                                  *
032400050713      *---------------------------------------------------------------*
032500050726     c     InzD01        BEGSR
032600050713      *
032700050726     c                   clear                   FI89D01
032800050914      *
032900050914     c                   eval      V1Caac = *year
033000050713      *
033100050713     c                   ENDSR
033200050713      *
033300050713      *---------------------------------------------------------------*
033400050713      * Gestione tasto funzionale F03 da videata D01                  *
033500050713      *---------------------------------------------------------------*
033600050713     c     F03D01        BEGSR
033700050713      *
033800050713      * Chiudo il programma
033900050715     c                   eval      $Fine    = *on
034000050920if  1c                   if        $Called  = *on
034100050725     c                   eval      O00f03   = 'S'
034200050920e   1c                   endif
034300050713      *
034400050713     c                   ENDSR
034500050915      *
034600050915      *---------------------------------------------------------------*
034700050915      * Gestione tasto funzionale F14 da videata D01                  *
034800050915      *---------------------------------------------------------------*
034900050915     c     F14D01        BEGSR
035000050915      *
035100051006      * Prima reperisco i dati della spedizione
035200051006     c                   eval      DCTaac   = V1Caac
035300051006     c                   eval      DCTfil   = V1Cfil
035400051006     c                   eval      DCTnca   = V1Cnca
035500051006     c     K03DRA01      chain     FNDCT000
035600051006if  1c                   if        NOT %found(FNDCT01L)
035700051006     c                             or  DCTatb <> *blanks
035800051006     c                   clear                   DCTaas
035900051006     c                   clear                   DCTlnp
036000051006     c                   clear                   DCTnrs
036100051006     c                   clear                   DCTnsp
036200051006e   1c                   endif
036300051006      *
036400050915     c                   reset                   FIDN00ds
036500050915      *
036600050915     c                   eval      I00aac   = V1Caac
036700050915     c                   eval      I00fil   = V1Cfil
036800050915     c                   eval      I00nca   = V1Cnca
036900051006     c                   eval      I00aas   = DCTaas
037000051006     c                   eval      I00lnp   = DCTlnp
037100051006     c                   eval      I00nrs   = DCTnrs
037200051006     c                   eval      I00nsp   = DCTnsp
037300050915      *
037400050915     c                   movel(p)  FIDN00ds      KPJBU
037500050915     c                   call      'FIDN01R'
037600050915     c                   parm                    KPJBA
037700050915      *
037800050915if  1c                   if        O00err  <> *blanks
037900050915     c                   seton                                          2890
038000050915     c                   eval      V1Dmsg   =  O00msg
038100050915e   1c                   endif
038200050915      *
038300050915     c                   ENDSR
038400050713      *
038500050713      *---------------------------------------------------------------*
038600050713      * Controllo dati immessi in videata D01                         *
038700050713      *---------------------------------------------------------------*
038800050713     c     CtrD01        BEGSR
038900050713      *
039000050728     c                   movea     *zeros        *in(50)
039100050726     c                   clear                   W2Cnev
039200050727      *
039300050727      * Dati C.A.
039400050727sel 1c                   select
039500050727w   1c                   when      V1Cfil   = *zeros
039600050728     c                   seton                                        512890
039700050727     c                   eval      V1Dmsg   = $Msg(1)
039800050727     c                   leavesr
039900050727w   1c                   when      V1Cnca   = *zeros
040000050728     c                   seton                                        522890
040100050727     c                   eval      V1Dmsg   = $Msg(2)
040200050727     c                   leavesr
040300050727w   1c                   when      V1Caac   = *zeros
040400050727     c***                eval      V1Caac   = *year
040500050727     c                   seton                                        432890
040600050727     c                   eval      V1Dmsg   = $Msg(3)
040700050727     c                   leavesr
040800050727w   2c                   when      V1Caac   < 100  and  V1Caac >  60
040900050727     c                   add       1900          V1Caac
041000050727w   2c                   when      V1Caac   < 100  and  V1Caac <= 60
041100050727     c                   add       2000          V1Caac
041200050727e   2c                   endsl
041300050725      *
041400050725      * Numero C.A.
041500050725     c                   eval      DCTaac   = V1Caac
041600050725     c                   eval      DCTfil   = V1Cfil
041700050725     c                   eval      DCTnca   = V1Cnca
041800050726     c     K03DRA01      chain     FNDCT000
041900050920      * - C.A. chiusa
042000050725if  1c                   if        NOT %found(FNDCT01L)
042100050726     c                             or  DCTatb <> *blanks
042200050726     c                             or  DCTfca <> C_F_ChiusCA
042300050728     c                   seton                                        512890
042400050727     c                   eval      V1Dmsg   = $Msg(4)
042500050725     c                   leavesr
042600050725e   1c                   endif
042700050725      * - C.A. chiusa con causale di pagamento
042800050725     c                   eval      *in10    = *off
042900050726     c     DCTcch        lookup    $CCHpag                                10
043000050725if  1c                   if        NOT *in10
043100050728     c                   seton                                        512890
043200050727     c                   eval      V1Dmsg   = $Msg(5)
043300050725     c                   leavesr
043400050725e   1c                   endif
043500050920      * - C.A. contabilizzata
043600050920     c     K03DRA01      chain     FNDCL000
043700050920if  1c                   if        NOT %found(FNDCL01L)
043800050920     c                             or  DCLatb <> *blanks
043900050920     c                   clear                   DCLipl
044000050920     c                   clear                   DCLvpl
044100050920     c                   clear                   DCLfic
044200050920e   1c                   endif
044300050920if  1c                   if        DCLfic     <> 'C'
044400050920     c                   seton                                        512890
044500050920     c                   eval      V1Dmsg   = $Msg(6)
044600050920     c                   leavesr
044700050920e   1c                   endif
044800051005      *
044900051025      * - Recupero non ancora richiesto ad A.I.G.
045000051121      *   per tutte le CA con evento
045100050727     c     K03DRA01      chain     FNDRA000
045200050727if  1c                   if        NOT %found(FNDRA01L)
045300050805     c                             or  DRAatb <> *blanks
045400051121     c                             or (DRAdrr =  *zeros
045500051121     c                             and DRAdrd =  *zeros)
045600051125     c                             or (DCTnev > 0 and DRAdrr = *zeros)
045700050805     c                   clear                   FNDRAds
045800051005     c                   seton                                        512890
045900051005     c                   eval      V1Dmsg   = $Msg(7)
046000051005     c                   leavesr
046100050805e   1c                   endif
046200051025      * - C.A. esclusa da recupero
046300050805if  1c                   if        DRAesc   = 'E'
046400050805     c                   seton                                        512890
046500051005     c                   eval      V1Dmsg   = $Msg(8)
046600050805     c                   leavesr
046700050805e   1c                   endif
046800051025      * - Recupero non ancora richiesto ad A.I.G.
046900050805if  1c                   if        DRAdrr   = *zeros
047000051005     c                   clear                   DRAdvr
047100051005     c                   clear                   DRAdir
047200051005     c                   clear                   DRAiri
047300050805e   1c                   endif
047400051006      *
047500051006      * - Numero Evento
047600051007if  1c                   if        DCTnev   > *zeros
047700051006     c     K02DET01      chain     FNDET000
047800051025      * - - evento errato o già "fleggato" per recupero richiesto
047900051007if  2c                   if        NOT %found(FNDET01L)
048000051006     c                             or  DETatb <> *blanks
048100051021     c                             or  DETftr <> 'S'
048200051025      *** NON + ammessa l'immissione dei dati della richiesta recupero
048300051021      *** (ne mancherebbe la stampa)
048400051021     ***c                             or (DETftr <> *blanks
048500051021     ***c                             and DETftr <> 'S')
048600051006     c                   seton                                        512890
048700051006     c                   eval      V1Dmsg   = $Msg(9)
048800051006     c                   leavesr
048900051007e   2c                   endif
049000051007e   1c                   endif
049100050713      *
049200050713     c                   ENDSR
049300050714      *
049400050714      *---------------------------------------------------------------*
049500050715      * Decodifica / Controllo TIPO ANOMALIA                          *
049600050714      *---------------------------------------------------------------*
049700050714     c     DecodTAD      BEGSR
049800050714      *
049900050714     c                   clear                   dTAD
050000050714     c                   reset                   TIBS02ds
050100050714     c                   movel     KNSIF         T02sif
050200050714     c*** già impostato: movel     'TAD'         T02cod
050300050714     c                   movel(p)  DETtad        T02ke1
050400050714     c                   call      'TIBS02R'
050500050714     c                   parm                    KPJBA
050600050714     c                   parm                    TIBS02ds
050700050715     c                   if        T02err   = *blanks
050800050714     c                   movel     T02uni        dTAD
050900050714     c                   endif
051000050714      *
051100050714     c                   ENDSR
051200050714     *** *
051300050714     *** *---------------------------------------------------------------*
051400050714     *** * Decodifica P.O. che ha Comunicato l'Evento                    *
051500050714     *** *---------------------------------------------------------------*
051600050714     ***c     DecodFCE      BEGSR
051700050714     *** *
051800050714     ***c     DETfce        chain     AZORG
051900050714     ***c                   if        NOT %found(AZORG01L)
052000050714     ***c                             or  ORGfva <> *blanks
052100050714     ***c                   clear                   ORGdes
052200050714     ***c                   endif
052300050714     *** *
052400050714     ***c                   ENDSR
052500050725      *
052600050725      *---------------------------------------------------------------*
052700050725      * Aggiornamento FNDRA00F                                        *
052800050725      *---------------------------------------------------------------*
052900050725     c     UpdFNDRA      BEGSR
053000050725      *
053100050725     c                   clear                   DRAatb
053200050725if  1c                   if        NOT %found(FNDRA01L)
053300050802     c                   eval      DRAaac   = V1Caac
053400050802     c                   eval      DRAfil   = V1Cfil
053500050802     c                   eval      DRAnca   = V1Cnca
053600051007     c                   clear                   DRAdrd
053700050725e   1c                   endif
053800050725      *
053900051007     c                   eval      DRAdrr   = W2Cdrr
054000050726     c                   eval      DRAdvr   = W2Cdvr
054100050915     c                   eval      DRAiri   = V2Ciri
054200050915if  1c                   if        V2Ciri  <> *zeros
054300050915     c                   eval      DRAdir   = V2Cdir
054400050726e   1c                   endif
054500050725      *
054600050726if  1c                   if        %found(FNDRA01L)
054700050725     c                   UPDATE    FNDRA000
054800050725x   1c                   else
054900050725     c                   WRITE     FNDRA000
055000050725e   1c                   endif
055100050725      *
055200050725     c                   ENDSR
055300050725      *
055400050725      *---------------------------------------------------------------*
055500050726      * Gestione videata D02                                          *
055600050725      *---------------------------------------------------------------*
055700050726     c     GesD02        BEGSR
055800050725      *
055900050725      * Inizializzo la videata
056000050726if  1c                   if        $InzD02  = *on
056100050726     c                   exsr      InzD02
056200050726     c                   eval      $InzD02  = *off
056300050726e   1c                   endif
056400050725      *
056500050725      * Scrivo la testata e la riga tasti funzionali abilitati
056600050725if  1c                   if        NOT *in90
056700050726     c                   write     FI89T01
056800050726     c                   write     FI89Z02
056900050725e   1c                   endif
057000050725      * Emetto la videata
057100050726     c                   exfmt     FI89D02
057200050725     c                   setoff                                       28  90
057300050725     c                   clear                   V1Dmsg
057400050725      *
057500050920sel 1c                   SELECT
057600050725      * F3=Fine
057700050920w   1c                   WHEN      *inKC
057800050920if  2c                   if        $Called = *off
057900050726     c                   unlock    FNDRA01L
058000050726     c                   exsr      F03D01
058100050920e   2c                   endif
058200050725      * F12=Ritorno
058300050920w   1c                   WHEN      *inKL
058400050726     c                   unlock    FNDRA01L
058500050726     c                   exsr      F12D02
058600050802      * F14=Interrogazione C.A.
058700050920w   1c                   WHEN      *inKN
058800050802     c                   exsr      F14D02
058900050920      * Enter o F6
059000050920x   1c                   OTHER
059100050725      * Controllo dati immessi a video
059200050726     c                   exsr      CtrD02
059300050726     c     *in90         cabeq     *on           EndGesD02
059400050725      * F6=Conferma
059500050920if  2c                   if        *inKF
059600050802      * - Aggiorno FNDRA00F ed esco dal pgm
059700050726     c                   exsr      F06D02
059800050920e   2c                   endif
059900050920      *
060000050920e   1c                   ENDSL
060100050725      *
060200050726     c     EndGesD02     ENDSR
060300050725      *
060400050725      *---------------------------------------------------------------*
060500050726      * Inizializzazione videata D02                                  *
060600050725      *---------------------------------------------------------------*
060700050726     c     InzD02        BEGSR
060800050725      *
060900050726     c                   clear                   FI89D02
061000050726      *
061100050802      * Aggancio record in file FNDRA01L
061200050726     c*** già fatto:     eval      DCTaac   = I00aac
061300050726     c*** già fatto:     eval      DCTfil   = I00fil
061400050726     c*** già fatto:     eval      DCTnca   = I00nca
061500050726     c     K03DRA01      chain     FNDRA000
061600050726if  1c                   if        NOT %found(FNDRA01L)
061700050726     c                             or  DRAatb <> *blanks
061800050726     c                   clear                   FNDRAds
061900050726e   1c                   endif
062000051006     *** *
062100050920     *** * Aggancio record in file FNDCL01L
062200050920     *** * (già fatto in subr. CtrD01)
062300050920     ***c     K03DRA01      chain     FNDCL000
062400050920if  1***c                   if        NOT %found(FNDCL01L)
062500050920     ***c                             or  DCLatb <> *blanks
062600050920     ***c                   clear                   DCLipl
062700050920     ***c                   clear                   DCLvpl
062800050920     ***c                   clear                   DCLfic
062900050920e   1***c                   endif
063000051006      *
063100051006      * Aggancio record in file TITAS30C
063200051006     c     K04TAS30      chain     TITAS30C
063300051006if  1c                   if        NOT %found(TITAS30C)
063400051006     c                   clear                   TASmgs
063500051006     c                   clear                   TASccm
063600051006     c                   clear                   TASksc
063700051006e   1c                   endif
063800050725      *
063900050725      * Imposto i dati di output
064000050914     c                   exsr      DecodTAD
064100050915     c                   movel     §TADdesc      V2Dtae
064200050915     c                   eval      V2Cnev   = DCTnev
064300050915     c                   eval      V2Caae   = DCTaae
064400050914     c                   reset                   WLBdat
064500050914     c                   eval      G08inv   = DETaae * 10000 + DETmge
064600050914     c                   call      'XSRDA8'
064700050914     c                   parm                    WLBdat
064800050914if  1c                   if        G08err   = *off
064900050915     c                   eval      V2Cdev   = G08dat
065000050914e   1c                   endif
065100050915     c                   eval      V2Ctad   = DCTtad
065200050726     c                   eval      DETtad   = DCTtad
065300050725     c                   exsr      DecodTAD
065400050915     c                   movel     §TADdesc      V2Dtad
065500050915     c                   eval      V2Cfil   = DCTfil
065600050915     c                   eval      V2Cnca   = DCTnca
065700050725     c                   reset                   WLBdat
065800050726     c                   eval      G08inv   = DCTaac * 10000 + DCTmgc
065900050725     c                   call      'XSRDA8'
066000050725     c                   parm                    WLBdat
066100050725if  1c                   if        G08err   = *off
066200050915     c                   eval      V2Cdca   = G08dat
066300050725e   1c                   endif
066400050915     c                   eval      V2Cprn   = DCTprn
066500050915     c                   eval      V2Cpra   = DCTpra
066600050915     c                   eval      V2Clnp   = DCTlnp
066700050915     c                   eval      V2Cnrs   = DCTnrs
066800050915     c                   eval      V2Cnsp   = DCTnsp
066900050725     c                   reset                   WLBdat
067000050726     c                   eval      G08inv   = DCTaas * 10000 + TASmgs
067100050725     c                   call      'XSRDA8'
067200050725     c                   parm                    WLBdat
067300050725if  1c                   if        G08err   = *off
067400050915     c                   eval      V2Cdsp   = G08dat
067500050725e   1c                   endif
067600050915     c                   eval      V2Cdir   = §GEDdba
067700050726if  1c                   if        DRAdrr   > *zeros
067800050726     c                   reset                   WLBdat
067900050726     c                   eval      G08inv   = DRAdrr
068000050726     c                   call      'XSRDA8'
068100050726     c                   parm                    WLBdat
068200050726if  2c                   if        G08err   = *off
068300050915     c                   eval      V2Cdrr   = G08dat
068400050726e   2c                   endif
068500050726e   1c                   endif
068600050915     c                   eval      V2Cipl   = DCLipl
068700050915     c                   eval      V2Cvpl   = DCLvpl
068800050914      * ricerca benficiario
068900050914     c                   exsr      Sr_Ric_Ben
069000050725      *
069100050725      * Imposto i dati di I/O
069200050726if  1c                   if        DRAdvr   > *zeros
069300050726     c                   reset                   WLBdat
069400050726     c                   eval      G08inv   = DRAdvr
069500050726     c                   call      'XSRDA8'
069600050726     c                   parm                    WLBdat
069700050726if  2c                   if        G08err   = *off
069800050915     c                   eval      V2Cdvr   = G08dat
069900050726e   2c                   endif
070000050726e   1c                   endif
070100050915     c                   eval      V2Ciri   = DRAiri
070200050802      *
070300050802      * imposto gli indicatori
070400050914     c     $Called       comp      *on                                    40
070500050915     c     V2Cnev        comp      *zeros                             4141
070600050915     c     V2Cprn        comp      *zeros                             4242
070700050915     c     V2Cdrr        comp      *zeros                             4949
070800050725      *
070900050725     c                   ENDSR
071000050726      *
071100050726      *---------------------------------------------------------------*
071200050726      * Gestione tasto funzionale F06 da videata D02                  *
071300050726      *---------------------------------------------------------------*
071400050726     c     F06D02        BEGSR
071500050802      *
071600050802      * Aggiornamento FNDRA00F
071700050802     c                   exsr      UpdFNDRA
071800050726      *
071900050726      * Chiudo il programma
072000050726     c                   eval      $Fine    = *on
072100050726      *
072200050726     c                   ENDSR
072300050725      *
072400050725      *---------------------------------------------------------------*
072500050726      * Gestione tasto funzionale F12 da videata D02                  *
072600050725      *---------------------------------------------------------------*
072700050726     c     F12D02        BEGSR
072800050725      *
072900050726if  1c                   if        $Called  = *on
073000050726      * Chiudo il programma
073100050726     c                   eval      $Fine    = *on
073200050726     c                   eval      O00f12   = 'S'
073300050726     c                   else
073400050726      * Torno alla videata precedente
073500050726     c                   eval      $Video   = '1'
073600050726e   1c                   endif
073700050725      *
073800050725     c                   ENDSR
073900050802      *
074000050802      *---------------------------------------------------------------*
074100050802      * Gestione tasto funzionale F14 da videata D02                  *
074200050802      *---------------------------------------------------------------*
074300050802     c     F14D02        BEGSR
074400050802      *
074500050802     c                   reset                   FIDN00ds
074600050802      *
074700050802     c                   eval      I00aac   = DCTaac
074800050802     c                   eval      I00fil   = DCTfil
074900050802     c                   eval      I00nca   = DCTnca
075000050802     c                   eval      I00aas   = DCTaas
075100050802     c                   eval      I00lnp   = DCTlnp
075200050802     c                   eval      I00nrs   = DCTnrs
075300050802     c                   eval      I00nsp   = DCTnsp
075400050802     c                   eval      I00aae   = DETaae
075500050802     c                   eval      I00nev   = DCTnev
075600050802     c                   eval      I00dev   = DCTaae * 10000 + DETmge
075700050802     c                   eval      I00fca   = DCTfca
075800050802     c                   eval      I00tae   = DCTtad
075900050802      *
076000050802     c                   movel(p)  FIDN00ds      KPJBU
076100050802     c                   call      'FIDN01R'
076200050802     c                   parm                    KPJBA
076300050802      *
076400050802if  1c                   if        O00err  <> *blanks
076500050802     c                   seton                                          2890
076600050802     c                   eval      V1Dmsg   =  O00msg
076700050802e   1c                   endif
076800050802      *
076900050802     c                   ENDSR
077000050725      *
077100050725      *---------------------------------------------------------------*
077200050726      * Controllo dati immessi in videata D02                         *
077300050725      *---------------------------------------------------------------*
077400050726     c     CtrD02        BEGSR
077500050725      *
077600050728     c                   movea     *zeros        *in(50)
077700050802     c                   clear                   W2Cdrr
077800050726     c                   clear                   W2Cdvr
077900050802      *
078000051025      * Data Richiesta Recupero
078100050802if  0c                   if        *in49
078200050802     c                   eval      W2Cdrr   = DRAdrr
078300050802x   0c                   else
078400051007if  1c                   if        V2Cdrr   = *zeros
078500051007     c***                eval      V2Cdrr   = *date
078600051007     c                   seton                                        502890
078700051007     c                   eval      V1Dmsg   = $Msg(10)
078800051007     c                   leavesr
078900051007e   1c                   endif
079000050802     c                   clear                   WLBdat
079100050915     c                   eval      G08dat   = V2Cdrr
079200050802     c                   call      'XSRDA8'
079300050802     c                   parm                    WLBdat
079400050802if  1c                   if        G08err   = *on
079500050802     c                   seton                                        502890
079600051006     c                   eval      V1Dmsg   = $Msg(10)
079700050802     c                   leavesr
079800050802x   1c                   else
079900050915     c                   eval      V2Cdrr   = G08dat
080000050802     c                   eval      W2Cdrr   = G08inv
080100050802e   1c                   endif
080200050802e   0c                   endif
080300050802      *
080400050802if  1c                   if        W2Cdrr   < DCTaas * 10000 + TASmgs
080500050802     c                   seton                                        502890
080600051006     c                   eval      V1Dmsg   = $Msg(14)
080700050802     c                   leavesr
080800050802e   1c                   endif
080900050919if  1c                   if        W2Cdrr   > wDate
081000050919     c                   seton                                        502890
081100051006     c                   eval      V1Dmsg   = $Msg(15)
081200050919     c                   leavesr
081300050919e   1c                   endif
081400051007      *
081500051025      * Se immessa SOLO la data richiesta recupero: esco
081600051007if  1c                   if        NOT *in49
081700051007     c                             and V2Cdvr = *zeros
081800051007     c                             and V2Ciri = *zeros
081900051007     c                   leavesr
082000051007e   1c                   endif
082100050725      *
082200051025      * Data Ricezione Recupero
082300050915if  1c                   if        V2Cdvr   = *zeros
082400051007     c***                eval      V2Cdvr   = *date
082500051007     c                   seton                                        512890
082600051007     c                   eval      V1Dmsg   = $Msg(10)
082700051007     c                   leavesr
082800050728e   1c                   endif
082900050725     c                   clear                   WLBdat
083000050915     c                   eval      G08dat   = V2Cdvr
083100050725     c                   call      'XSRDA8'
083200050725     c                   parm                    WLBdat
083300050725if  1c                   if        G08err   = *on
083400050728     c                   seton                                        512890
083500051006     c                   eval      V1Dmsg   = $Msg(10)
083600050725     c                   leavesr
083700050725x   1c                   else
083800050915     c                   eval      V2Cdvr   = G08dat
083900050726     c                   eval      W2Cdvr   = G08inv
084000050725e   1c                   endif
084100050802      *
084200050802if  1c                   if        W2Cdvr   < W2Cdrr
084300050802     c                   seton                                        512890
084400051006     c                   eval      V1Dmsg   = $Msg(13)
084500050802     c                   leavesr
084600050802e   1c                   endif
084700050919if  1c                   if        W2Cdvr   > wDate
084800050919     c                   seton                                        512890
084900051006     c                   eval      V1Dmsg   = $Msg(15)
085000050919     c                   leavesr
085100050919e   1c                   endif
085200050802      *
085300051025      * Importo recuperato
085400050728sel 1c                   select
085500050915w   1c                   when      V2Ciri   < *zeros
085600050728     c                   seton                                        522890
085700051006     c                   eval      V1Dmsg   = $Msg(11)
085800050725     c                   leavesr
085900050915w   1c                   when      V2Ciri   = *zeros
086000050915     c***                eval      V2Ciri   = DCLipl
086100050728     c                   seton                                        522890
086200051006     c                   eval      V1Dmsg   = $Msg(12)
086300050728     c                   leavesr
086400050728e   1c                   endsl
086500050725      *
086600050725     c                   ENDSR
086700050914      *
086800050914      *---------------------------------------------------------------*
086900050914      * Ricerca beneficiario C.A.                                     *
087000050914      *---------------------------------------------------------------*
087100050914     c     Sr_Ric_Ben    BEGSR
087200050914      *
087300050915      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide
087400050915      *  con quello C.A. tranne se la sped. è "Mamma di cambio porto",
087500050915      *  in questo caso inverto il porto C.A.
087600050914      *
087700051007     c                   eval      Ddct01    = DCTflo
087800050915     c                   clear                   w§DCTport
087900050915     c                   clear                   wKSC
088000050915     c                   clear                   w0040
088100050915     c                   clear                   wPOintes
088200050915     c                   reset                   $Rivalsa
088300050915      *
088400050915sel 1c                   select
088500050915     c                   when      TAScca   <> '9'
088600050915     c                   movel     §DCTport      w§DCTport
088700050915     c                   when      §DCTport  = 'A'
088800050915     c                   movel     'F'           w§DCTport
088900050915     c                   when      §DCTport  = 'F'
089000050915     c                   movel     'A'           w§DCTport
089100050915e   1c                   endsl
089200050914      *
089300050914      * Dalla spedizione determino il codice beneficiario
089400050915sel 1c                   select
089500050915     c                   when           DCTptr    = 'D'                         DESTINATARIO  PF
089600050915     c                             and  w§DCTport = 'F'
089700050915     c                   when           DCTptr    = 'M'                         MITTENTE      PA
089800050915     c                             and  w§DCTport = 'A'
089900050915     c                   z-add     TASccm        wKSC
090000050915x   1c                   other
090100050915     c                   z-add     TASksc        wKSC
090200050915e   1c                   endsl
090300050914      *
090400050914      * Scorporo il  codice beneficiario
090500050914     c                   move      DCTksc        w0040
090600050914      *
090700050914      * Determino il P.O. del cliente beneficiario,  se non è codificato utilizzo lnp (M) o lna (D)
090800050915sel 1c                   select
090900050915     c                   when           DCTksc    > *zeros
091000050915     c                             and  w0040    <> 8888
091100050915     c                   movel     DCTksc        wPOintes
091200050915     c                   when      wKSC      > *zeros
091300050915     c                   movel     wKSC          wPOintes
091400050915     c                   when      DCTptr    = 'D'
091500050915     c                   z-add     DCTlna        wPOintes
091600050915x   1c                   other
091700050915     c                   movel     §DCTlnpc      wPOintes
091800050915e   1c                   endsl
091900050914      *
092000050914      * Controllo se ho rivalsa
092100050915     c                   eval      DKAtrc    = 'R'
092200050914     c     K04DKA01      chain     FNDKA000                           30
092300050915     c                   if             %found(FNDKA01L)
092400050915     c                             and  DKAatb    = *blanks
092500050915     c                   eval      $Rivalsa  = *on
092600050915     c                   endif
092700050914      *
092800050915sel 1c                   SELECT
092900050914      *
093000050914      * il beneficiario è alternativo e generico
093100050915w   1c                   WHEN      w0040     = 8888
093200050915     c                   eval      DKAtrc    = 'C'
093300050914     c     K04DKA01      chain     FNDKA000
093400050915     c                   if             %found(FNDKA01L)
093500050915     c                             and  DKAatb    = *blanks
093600050914     c                   exsr      Sr_Dec_Dka
093700050914     c                   endif
093800050914      *
093900050914      * il beneficiario è alternativo ed  è codificato
094000050915w   1c                   WHEN      DCTksc    > *zeros
094100050915     c                   eval      V2Cksc    = DCTksc
094200050915      * - controllo se esiste il luogo 8 o in subordine il luogo 6
094300050915     c                   eval      SPEfls    = 'L'
094400050915     c                   eval      SPEcod    = '008'
094500050915     c     K03SPE01      chain     FNSPE000
094600050915     c                   if        not %found(FNSPE01L)
094700050915     c                             or  SPEflg    <> *blanks
094800050915     c                   eval      SPEfls    = 'L'
094900050915     c                   eval      SPEcod    = '006'
095000050915     c     K03SPE01      chain     FNSPE000
095100050915     c                   endif
095200050915      * - indirizzo da luogo
095300050915if  2c                   IF        %found(FNSPE01L)
095400050915     c                             and SPEflg     = *blanks
095500050914     c                   exsr      Sr_Dec_Spe
095600050915x   2c                   ELSE
095700050915      * - - recupero beneficiario dall'anagrafica
095800050915     c                   reset                   TIBS69ds
095900050915     c                   eval      I69kac    = DCTksc
096000050915     c                   eval      I69kin    = DCTksc
096100050915     c                   call      'TIBS69R'
096200050915     c                   parm                    TIBS69ds
096300050915     c                   parm                    ds_CNACO
096400050915     c                   parm                    ds_CNIND
096500050915     c                   parm                    ds_CNCLP
096600050915     c                   parm                    ds_FNCLS
096700050915if  3c                   if        O69err    =  *blanks
096800050915     c                   movel     ACOrag        V2Crsc
096900050915     c***                movel     INDvia        V2Cind
097000050915     c                   movel     INDcae        V2Ccap
097100050915     c                   movel     INDcit        V2Cloc
097200050915     c                   if        INDprv   <> *blanks
097300050915     c                   movel     INDprv        V2Cprv
097400050915     c                   else
097500050915     c                   movel     INDsta        V2Cnaz
097600050915     c                   endif
097700050915e   3c                   endif
097800050915e   2c                   ENDIF
097900050914      *
098000050914      * L'intestatario è il cliente in bolla
098100050915x   1c                   OTHER
098200050915     c                   eval      V2Cksc    = wKSC
098300050915      * - controllo se esiste il luogo 8 o in subordine il luogo 6
098400050915     c                   move      wKSC          w0040
098500050915if  2c                   IF             w0040     > *zeros
098600050915     c                             and  w0040    <> 8888
098700050915     c                             and  w0040    <> 9999
098800050915     c                   eval      SPEfls    = 'L'
098900050915     c                   eval      SPEcod    = '008'
099000050915     c     K03SPE01      chain     FNSPE000
099100050915if  3c                   if        not %found(FNSPE01L)
099200050915     c                             or  SPEflg    <> *blanks
099300050915     c                   eval      SPEfls    = 'L'
099400050915     c                   eval      SPEcod    = '006'
099500050915     c     K03SPE01      chain     FNSPE000
099600050915e   3c                   endif
099700050915e   2c                   ENDIF
099800050915      * - indirizzo da luogo
099900050915if  2c                   IF            %found(FNSPE01L)
100000050915     c                             and SPEflg     = *blanks
100100050914     c                   exsr      Sr_Dec_Spe
100200050915x   2c                   ELSE
100300051006     c     K04TAS30      chain     TNTMD000
100400050915if  3c                   if        %found(TNTMD01L)
100500050915     c                   exsr      Sr_Dec_Tmd
100600050915e   3c                   endif
100700050915e   2c                   ENDIF
100800050914      *
100900050915e   1c                   ENDSL
101000050914      *
101100050914     c                   ENDSR
101200050914      *
101300050915      *---------------------------------------------------------------*
101400050915      * Recupero beneficiario da FNDKA                                *
101500050915      *---------------------------------------------------------------*
101600050914     c     Sr_Dec_Dka    BEGSR
101700050914      *
101800050915     c                   movel     DKArag        V2Crsc
101900050915     c***                movel     DKAvia        V2Cind
102000050915     c                   movel     DKAcap        V2Ccap
102100050915     c                   movel     DKAloc        V2Cloc
102200050915     c                   if        DKAprv   <> *blanks
102300050915     c                   movel     DKAprv        V2Cprv
102400050915     c                   else
102500050915     c                   movel     DKAnaz        V2Cprv
102600050915     c                   endif
102700050914      *
102800050914     C                   ENDSR
102900050915      *---------------------------------------------------------------*
103000050915      * Recupero il luogo del beneficiario                            *
103100050915      *---------------------------------------------------------------*
103200050914     c     Sr_Dec_Spe    BEGSR
103300050914      *
103400050915     c                   movel     SPErag        V2Crsc
103500050915     c***                movel     SPEind        V2Cind
103600050915     c                   movel     SPEcap        V2Ccap
103700050915     c                   movel     SPEloc        V2Cloc
103800050915     c                   if        SPEpro   <> *blanks
103900050915     c                   movel     SPEpro        V2Cprv
104000050915     c                   else
104100050915     c                   movel     SPEnaz        V2Cprv
104200050915     c                   endif
104300050914      *
104400050914     c                   ENDSR
104500050915      *---------------------------------------------------------------*
104600050915      * Recupero beneficiario da TNTMD                                *
104700050915      *---------------------------------------------------------------*
104800050915     c     Sr_Dec_Tmd    BEGSR
104900050915      *
105000050915if  1c                   IF        DCTptr    =  'D'
105100050915      * - indirizzo da destinatario spedizione
105200050915     c                   movel(p)  TMDrsd        V2Crsc
105300050915     c***                movel     TMDind        V2Cind
105400050915     c                   movel     TMDcad        V2Ccap
105500050915     c                   movel     TMDlod        V2Cloc
105600050915     c                   if        TMDprd   <> *blanks
105700050915     c                   movel     TMDprd        V2Cprv
105800050915     c                   else
105900050915     c                   movel     TMDnzd        V2Cnaz
106000050915     c                   endif
106100050915x   1c                   ELSE
106200050915      * - indirizzo da mittente spedizione
106300050915     c                   movel     TMDrsm        V2Crsc
106400050915     c***                movel     TMDinm        V2Cind
106500050915     c                   movel     TMDcam        V2Ccap
106600050915     c                   movel     TMDlom        V2Cloc
106700050915     c                   if        TMDprm   <> *blanks
106800050915     c                   movel     TMDprm        V2Cprv
106900050915     c                   else
107000050915     c                   movel     TMDnzm        V2Cnaz
107100050915     c                   endif
107200050915e   1c                   ENDIF
107300050915      *
107400050915     c                   ENDSR
107500050713
107600050713      *---------------------------------------------------------------*
107700050713
107800050714**   $CCHpag
107900051025051518
108000050727**   $MSG >------------------------------------------------------------------*
108100050727Immettere il P.O.                                                              1
108200050727Immettere il numero C.A.                                                       2
108300050727Immettere l'anno della C.A.                                                    3
108400050920C.A. inesistente o NON chiusa                                                  4
108500050920C.A. chiusa, ma con causale NON di pagamento                                   5
108600050920C.A. chiusa, ma NON contabilizzata                                             6
108700051025Recupero non ancora richiesto per questa C.A.                                  7
108800051025C.A. esclusa da recupero o facente parte di evento escluso dal recupero        8
108900051025Numero evento errato (escluso o già recuperato)                                9
109000051006Immettere data formalmente valida                                             10
109100051006Importo errato                                                                11
109200051006Immettere l'importo ricevuto                                                  12
109300051025Data Ricezione Recupero NON può essere precedente a Data Richiesta Recupero   13
109400051025Data Richiesta Recupero NON può essere precedente a Data C.A.                 14
109500051006Non ammessa data successiva a quella odierna                                  15
