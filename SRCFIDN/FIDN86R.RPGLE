000100050713      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050713      *===============================================================*
000300051017      *?FIDN86R * Gestione Recupero Eventi da A.I.G.                 ?*
000400050713      *===============================================================*
000500050726      *? N.B. ?:                                                      *
000600050727      * DSPF in uso anche dai pgm. FIDN87R e FIDN88R.                 *
000700050726      *---------------------------------------------------------------*
000800050713
000900050713     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000050713
001100050713      *---------------------------------------------------------------*
001200050713
001300050720     fFNDET01L  Uf   e           k disk
001400050726     fFNDRA01L  Uf A e           k disk
001500050720      *
001600050714     fFNDCT08L  if   e           k disk
001700050713     fFNDCL01L  if   e           k disk
001800050714     fFNDCS01L  if   e           k disk
001900050715      *
002000050714     fTITAS30C  if   e           k disk
002100050714      *
002200050713     fFIDN86D   cf   e             workstn
002300050713     f                                     sfile(FI86S01:S01nrr)
002400050726     f                                     sfile(FI86S02:S02nrr)
002500050714     f                                     infds(DSFMT)
002600050713
002700050713      *---------------------------------------------------------------*
002800050713
002900050713      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003000050713      *
003100050713     d DigitN          c                   const('0123456789')
003200050721     d C_GMALimite     c                   const(18042005)
003300050713     d C_DataLimite    c                   const(20050418)
003400050715     d C_AnnoLimite    c                   const(2005)
003500050713      * - Tasti di funzione
003600050713     d Enter           c                   const(x'F1')
003700050713     d RollDwn         c                   const(x'F4')
003800050713     d RollUp          c                   const(x'F5')
003900050714      * - Nr. di righe del sfl (SFLPAG)
004000050920     d C_SflPag1       c                   const(16)
004100050920     d C_SflPag2       c                   const(12)
004200050714      * - Fasi C.A. fissate
004300050714     d C_F_ChiusCA     c                   const(900)
004400050713      *
004500050713      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004600050713      *
004700050714      * - Causali Chiusura C.A. - di Pagamento
004800050714     d $CCHpag         s                   like(DCTcch)
004900050714     d                                     dim(10) ctdata  perrcd(10)
005000050713      * - Messaggi di errore
005100051013     d $Msg            s             78    dim(22) ctdata  perrcd(1)
005200050713      *
005300050713      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
005400050713      *
005500050713     d KPJBA         e ds
005600050713      * - Parametri x Controllo profilo utenti
005700050713     d TIBS34DS      e ds                  inz
005800050713      * - Ds di riferimento al file esterno AZUTE00F
005900050713     d AZUTEds       e ds                  extname(AZUTE00F)
006000050713      * - Ds per dati organigramma
006100050713     d DDatiUte      e ds
006200050713      * - Parametri x Interrogazione tabelle
006300050713     d TIBS02DS      e ds                  inz
006400050713     d   T02mod      e                     inz('C')
006500050713     d   T02cod      e                     inz('TAD')
006600050714      * - Tab. "GED" (DN)
006700050714     d dGEDDN        e ds                  inz
006800050714      * - Tab. "STD"
006900050714     d dSTD          e ds                  inz
007000050713      * - Tab. "TAD"
007100050713     d dTAD          e ds                  inz
007200050713      *
007300050713      * - Ricerca Eventi
007400050713     d FIDN07ds      e ds                  inz
007500050713     d  I07mod       e                     inz('R')
007600050715      *
007700050719      * - Gestione C.A. legate all'evento
007800050715     d FIDN00ds      e ds                  inz
007900050726     d  I00mod       e                     inz('I')
008000050726     d  I00tpb       e                     inz('S')
008100050726      *
008200050726      * Gestione note C.A.
008300050726     d FIDN10ds      e ds                  inz
008400050726     d  I10flm       e                     inz('V')
008500050726     d  I10tpd       e                     inz('C')
008600050726     d  I10trc       e                     inz('N')
008700050715      *
008800050715      * - Trk (di comodo - per clear "unici")
008900050715     d FNDRAds       e ds                  extname(FNDRA00F) inz
009000050713      *
009100050713     d Status         sds           333
009200050713     d   SDSpgm          *proc
009300050714      *
009400050714     d DSFMT           ds           512
009500050714     d  £Tasto               369    369
009600050714     d  £SFLnrr              378    379B 0
009700050713      *
009800050713      * - Controllo/Inversione date
009900050713     d WLBdat          ds                  inz
010000050713     d   G08dat                       8  0 inz
010100050713     d   G08inv                       8  0 inz
010200050713     d   G08err                       1    inz('3')
010300050713     d   G08tgi                       5  0 inz
010400050715      *
010500050726     d DS_KeyEve       ds                  inz
010600050726     d   DCTaae                            inz
010700050726     d   DCTnev                            inz
010800050726      *
010900050726     d DS_NumCA        ds                  inz
011000050726     d   H2Caac                            inz
011100050726     d   S2Cfil                            inz
011200050726     d   S2Cnca                            inz
011300050919      *
011400050919     d FIDN86ds        ds                  inz
011500050919     d   V1Cnev                            inz
011600050919     d   V1Caae                            inz
011700050919     d   V1Cdai                            inz
011800050919     d   V1Cdaf                            inz
011900050919     d   V1Cse1                            inz
012000050919     d   V1Cse2                            inz
012100051010     d   V1Cse3                            inz
012200051010     d   V1Cse4                            inz
012300051025     d   V1Cse5                            inz
012400050713      *
012500050713      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
012600050713      *
012700050715      * - Flags
012800050718     d $EoF            s              1    inz(*off)
012900050713     d $Fine           s              1    inz(*off)
013000050713     d $InzD01         s              1    inz(*on)
013100050713     d $InzS01         s              1    inz(*on)
013200050726     d $InzS02         s              1    inz(*on)
013300050713      *
013400050713     d $Video          s              1    inz('1')
013500050715     d $UpdDET         s              1    inz
013600050726     d $UpdDRA         s              1    inz
013700050713      *
013800050713      * - Indici di schiera
013900050713      *
014000050713      * - Variabili riferite al data base o al display file
014100050715     d S01nrr          s                   like(C01rcd) inz
014200050726     d S02nrr          s                   like(C02rcd) inz
014300050921     d SavS01csr       s                   like(C01rcd) inz
014400050921     d SavS02csr       s                   like(C02rcd) inz
014500050921     d SavS02csr_R     s                   like(C02rcd) inz
014600050713     d W1Cnev          s                   like(DETnev) inz
014700050715     d wDRAdrr         s                   like(DRAdrr) inz
014800050715     d wDRAdvr         s                   like(DRAdvr) inz
014900050714     d wTOTipl         s                   like(DCLipl) inz
015000050715     d wTOTir1         s                   like(DRAiri) inz
015100050715     d wTOTiri         s                   like(DRAiri) inz
015200050728     d SAViri          s                   like(DRAiri) inz
015300050714     d W1Cvpl          s                   like(DCLvpl) inz
015400050801     d W1Cdi1          s                   like(DRAdir) inz
015500050801     d W1Cdir          s                   like(DRAdir) inz
015600050713      *
015700050713      * - Variabili definite a programma
015800051017     d wData_eur       s               d   datfmt(*eur)
015900051017     d wData_gma       s               d   datfmt(*dmy)
016000050718     d wPag            s              4  0 inz
016100050718     d wRig            s              3  0 inz
016200050726     d W_SflPag1       s              3  0 inz
016300050726     d W_SflPag2       s              3  0 inz
016400051013     d W1Cdai          s              8  0 inz(*loval)
016500051013     d W1Cdaf          s              8  0 inz(*hival)
016600050726     d wTOTcale        s              3  0 inz
016700050916     d wTOTcaold       s              3  0 inz
016800050919     d wTOTnocch       s              3  0 inz
016900050726     d wTOTcanp        s              3  0 inz
017000050919     d wTOTcanrr       s              3  0 inz
017100050919     d wTOTcar1        s              3  0 inz
017200050726     d wTOTcara        s              3  0 inz
017300050802     d wCountOpzR2     s              5  0 inz
017400050801     d wSumImpRic      s             15  2 inz
017500050715     d Wcampo          s             10    inz
017600050713      *
017700050713      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
017800050713      *
017900050713      * - FNDET01L
018000050713     c     K02DET01      klist
018100050713     c                   kfld                    DETaae
018200050713     c                   kfld                    DETnev
018300050726      * - FNDRA01L / FNDCL01L
018400050726     c     K03DRA01      klist
018500050726     c                   kfld                    DCTaac
018600050726     c                   kfld                    DCTfil
018700050726     c                   kfld                    DCTnca
018800050714      * - FNDCT08L
018900050714     c     K03DCT08      klist
019000050714     c                   kfld                    DCTaae
019100050714     c                   kfld                    DCTnev
019200050714     c                   kfld                    DCTfca
019300050714     c     K02DCT08      klist
019400050714     c                   kfld                    DCTaae
019500050714     c                   kfld                    DCTnev
019600050715      * - TITAS30C
019700050715     c     K04TAS30      klist
019800050715     c                   kfld                    DCTaas
019900050715     c                   kfld                    DCTlnp
020000050715     c                   kfld                    DCTnrs
020100050715     c                   kfld                    DCTnsp
020200050714      * - FNDCS01L
020300050714     c     K07DCS01      klist
020400050714     c                   kfld                    DCStpd
020500050714     c                   kfld                    DCSnkt
020600050714     c                   kfld                    DCSstd
020700050714     c                   kfld                    DCSdim
020800050714     c                   kfld                    DCShim
020900050714     c                   kfld                    DCSnks
021000050714     c                   kfld                    DCStrc
021100050714     c*** NO progressivo kfld                    DCSpno
021200050713
021300050713      *---------------------------------------------------------------*
021400050713      *  RIEPILOGO INDICATORI                                         *
021500050713      *---------------------------------------------------------------*
021600050713      * 10    - Comodo                                                *
021700050713      * 28    - Emette il messaggio di errore a video                 *
021800050728      * 30    - Ripulisce il subfile S01                              *
021900050728      * 31    - NON emette il subfile S01                             *
022000050728      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
022100050728      * 33    - Fine dati nel subfile S01         (sflend)            *
022200050728      * 35    - Ripulisce il subfile S02                              *
022300050728      * 36    - NON emette il subfile S02                             *
022400050728      * 37    - Record modificato nel subfile S02 (sflnxtchg)         *
022500050728      * 38    - Fine dati nel subfile S02         (sflend)            *
022600051013      * 39    - subfile (S01 o S02) senza records                     *
022700051017      *N40    - Visualizza totale importo recuperato nel piede P02    *
022800050728      * 40    - Visualizza totale importo richiesto  nel piede P02    *
022900051017      * 41    - Flag Esclusione Evento da Richiesta Recupero in (RI)  *
023000051017      * 42    - Flag Evento Fortuito in (RI)                          *
023100050728      * 50    - Posizionamento cursore sul campo "Opzione"            *
023200050728      * 51    - Numero Evento errato                                  *
023300050728      * 52    - Anno   Evento errato                                  *
023400050728      * 53    - 1ª Data Apertura Evento (dal) errata                  *
023500050728      * 54    - 2ª Data Apertura Evento (al)  errata                  *
023600051017      * 55    - 1ª selezione errara (Eventi senza richiesta recupero) *
023700051017      * 56    - 2ª selezione errara (Eventi in attesa di recupero)    *
023800051017      * 57    - 3ª selezione errara (Eventi recuperati totalmente)    *
023900051011      * 58    - 4ª selezione errara (Eventi esclusi)                  *
024000050713      * 90    - Generico di errore                                    *
024100050713      *---------------------------------------------------------------*
024200050713
024300050713     c     *Entry        plist
024400050713     c                   parm                    KPJBA
024500050713      *
024600050713      * Operazioni Iniziali
024700050713     c                   exsr      RoutInz
024800050713      *
024900050713      * Gestione Video
025000050715do  1c                   DOW       $Fine    = *off
025100050713      * - Filtro di lancio
025200050713cas 2c     $Video        caseq     '1'           GesD01
025300050726      * - SubFile eventi
025400050713cas 2c     $Video        caseq     '2'           GesS01
025500050803      * - SubFile c.a.  (gestito all'interno delle sr. x opz. di S01)
025600050803cas 2c***  $Video        caseq     '3'           GesS02
025700050713e   2c                   endcs
025800050713e   1c                   ENDDO
025900050713      *
026000050713      * Fine
026100050713     c                   clear                   TIBS02ds
026200050715     c                   eval      T02tla   = 'C'
026300050713     c                   call      'TIBS02R'
026400050713     c                   parm                    KPJBA
026500050713     c                   parm                    TIBS02ds
026600050713     c                   movel     *on           *inLR
026700050713      *
026800050713      *---------------------------------------------------------------*
026900050713      * Operazioni Iniziali                                           *
027000050713      *---------------------------------------------------------------*
027100050713     c     RoutInz       BEGSR
027200050713      *
027300050713      * Reperisco dati job
027400050713     c                   exsr      DatiJob
027500050713      *
027600050713     c                   movel     SDSpgm        V1Tpgm
027700050714      *
027800050714      * Aggancio tabella GED (Gestionale: Divise)
027900050714     c                   clear                   dGEDdn
028000050714     c                   reset                   TIBS02ds
028100050715     c                   eval      T02sif   = KNSIF
028200050715     c                   eval      T02cod   = 'GED'
028300050714     c                   movel(p)  'DANNI'       T02ke1
028400050714     c                   call      'TIBS02R'
028500050714     c                   parm                    KPJBA
028600050714     c                   parm                    TIBS02ds
028700050715     c                   if        T02err   = *blanks
028800050714     c                   movel     T02uni        dGEDdn
028900050714     c                   endif
029000050714      * Aggancio tabella STD (Standard Danni)
029100051025     c*** già "inz":     clear                   dSTD
029200050714     c                   reset                   TIBS02ds
029300050715     c                   eval      T02sif   = KNSIF
029400050715     c                   eval      T02cod   = 'STD'
029500050714     c                   movel(p)  '1'           T02ke1
029600050714     c                   movel(p)  §GEDdba       T02ke2
029700050714     c                   call      'TIBS02R'
029800050714     c                   parm                    KPJBA
029900050714     c                   parm                    TIBS02ds
030000050715     c                   if        T02err   = *blanks
030100050714     c                   movel     T02uni        dSTD
030200050714     c                   endif
030300050713      *
030400050713     c                   ENDSR
030500050713      *
030600050713      *---------------------------------------------------------------*
030700050713      * Reperimento Dati del job (Utente/Operativi)                   *
030800050713      *---------------------------------------------------------------*
030900050713     c     DatiJob       BEGSR
031000050713      *
031100050713     c     *dtaara       define    §azute        azuteds
031200050713     c     *dtaara       define    §datiute      ddatiute
031300050713      *
031400050713     c                   in(E)     *dtaara
031500050713     c                   IF        %ERROR or RSUT = *blanks
031600050713     c                   clear                   Tibs34Ds
031700050713     c                   call      'TIBS34R'
031800050713     c                   parm                    Tibs34Ds
031900050713     c                   in        *dtaara
032000050713     c                   ENDIF
032100050713      *
032200050713     c                   ENDSR
032300050713      *
032400050713      *---------------------------------------------------------------*
032500050713      * Gestione videata D01                                          *
032600050713      *---------------------------------------------------------------*
032700050713     c     GesD01        BEGSR
032800050713      *
032900050713      * Inizializzo la videata
033000050803if  1c                   if        $InzD01     = *on
033100050713     c                   exsr      InzD01
033200050713     c                   movel     *off          $InzD01
033300050713e   1c                   endif
033400050713      *
033500050713      * Reimposto il numero F.V. a *blank se *zero (numerico editato)
033600050803if  2c                   if        V1Cnev      = *zeros
033700050803     c                   eval      V1Cnev      = *blanks
033800050713e   2c                   endif
033900050713      *
034000050713      * Scrivo la testata e la riga tasti funzionali abilitati
034100050713if  1c                   if        NOT *in90
034200050715     c                   write     FI86T01
034300050713     c                   write     FI86Z01
034400050713e   1c                   endif
034500050713      * Emetto la videata
034600050713     c                   exfmt     FI86D01
034700050713     c                   setoff                                       28  90
034800050713     c                   clear                   V1Dmsg
034900050713      *
035000050713sel 1c                   select
035100050713      * F3=Fine
035200050713w   1c                   when      *inKC
035300050713     c                   exsr      F03D01
035400050713     c                   leavesr
035500050713      *
035600050713e   1c                   endsl
035700050713      *
035800050713      * Controllo dati immessi a video
035900050713     c                   exsr      CtrD01
036000050921if  1c                   if        $UpdDET    <> *blanks
036100050914     c                   exsr      UpdFNDET
036200050921e   1c                   endif
036300050713     c     *in90         cabeq     *on           EndGesD01
036400050713      *
036500050713      * Passaggio al subfile
036600050803     c                   eval      $InzS01     = *on
036700050803     c                   eval      $Video      = '2'
036800050713      *
036900050713     c     EndGesD01     ENDSR
037000050713      *
037100050713      *---------------------------------------------------------------*
037200050713      * Inizializzazione videata D01                                  *
037300050713      *---------------------------------------------------------------*
037400050713     c     InzD01        BEGSR
037500050713      *
037600050713     c                   clear                   FI86D01
037700050713      *
037800050713      * Imposto i dati di default
037900050803     c                   eval      V1Caae      = *year
038000051010     c                   eval      V1Cse1      = 'S'
038100051010     c                   eval      V1Cse2      = 'S'
038200051010     c                   eval      V1Cse3      = 'N'
038300051010     c                   eval      V1Cse4      = 'N'
038400051017     c                   eval      V1Cse5      = 'S'
038500050713      *
038600050713     c                   ENDSR
038700050713      *
038800050713      *---------------------------------------------------------------*
038900050713      * Gestione tasto funzionale F03 da videata D01                  *
039000050713      *---------------------------------------------------------------*
039100050713     c     F03D01        BEGSR
039200050713      *
039300050713      * Chiudo il programma
039400050803     c                   eval      $Fine       = *on
039500050713      *
039600050713     c                   ENDSR
039700050713      *
039800050713      *---------------------------------------------------------------*
039900050713      * Controllo dati immessi in videata D01                         *
040000050713      *---------------------------------------------------------------*
040100050713     c     CtrD01        BEGSR
040200050713      *
040300050728     c                   movea     *zeros        *in(50)
040400050718     c                   reset                   $UpdDET
040500050721     c                   clear                   W1Cnev
040600051013     c                   reset                   W1Cdai
040700051013     c                   reset                   W1Cdaf
040800050713      *
040900050713      * Numero Evento
041000050713      * - se *blanks lo considero *zeros (sarebbe numerico)
041100050803if  1c                   if        V1Cnev      = *blanks
041200050803     c                   eval      V1Cnev      = *zeros
041300050713e   1c                   endif
041400050713      * - ricerca
041500050713     c     '?'           scan      V1Cnev                                 10
041600050803if  1c                   if        *in10       = *on
041700050803     c                   eval      V1Cnev      = *zeros
041800050803     c                   eval      Wcampo      = 'V1CNEV    '
041900050713     c                   exsr      srSEARCH
042000050713e   1c                   endif
042100050713      * - controlli:
042200050713      * - - numericità
042300050713     c     DigitN        check     V1Cnev                                 10
042400050803if  1c                   if        *in10       = *on
042500050728     c                   seton                                        512890
042600050803     c                   eval      V1Dmsg      = $Msg(1)
042700050713     c                   leavesr
042800050713e   1c                   endif
042900050713      * - - validità (se immesso)
043000050921if  1c                   IF        V1Cnev     <> *zeros
043100050713      * - - anno evento
043200050713sel 2c                   select
043300050803w   2c                   when      V1Caae      = *zeros
043400050728     c                   seton                                        522890
043500050803     c                   eval      V1Dmsg      = $Msg(2)
043600050713     c                   leavesr
043700050803w   2c                   when          V1Caae  < 100
043800050803     c                             and V1Caae  >  60
043900050713     c                   add       1900          V1Caae
044000050803w   2c                   when          V1Caae  < 100
044100050803     c                             and V1Caae <= 60
044200050713     c                   add       2000          V1Caae
044300050713e   2c                   endsl
044400050713      *
044500050713     c                   move      V1Cnev        W1Cnev
044600050803     c                   eval      DETaae      = V1Caae
044700050803     c                   eval      DETnev      = W1Cnev
044800050922     c     K02DET01      chain(n)  FNDET000
044900050713      * - - numero evento errato
045000050713if  2c                   if        NOT %found(FNDET01L)
045100050713     c                             or  DETatb <> *blanks
045200050728     c                   seton                                        512890
045300050803     c                   eval      V1Dmsg      = $Msg(3)
045400050713     c                   leavesr
045500050713e   2c                   endif
045600050718      * - - evento già "fleggato"
045700070326      *  >  gli eventi eventualmente già fleggati "N" non vanno
045800070326      *     scartati qui: verranno scartati (eventualmente) DOPO !!!
045900050718sel 2c                   select
046000050803w   2c                   when      DETftr      = *blanks
046100051017w   2***c                   when      DETftr      = 'F'
046200051017     ***c                   seton                                        512890
046300051017     ***c                   eval      V1Dmsg      = $Msg(4)
046400051017     ***c                   leavesr
046500050803w   2c                   when      DETftr      = 'X'
046600050728     c                   seton                                        512890
046700050803     c                   eval      V1Dmsg      = $Msg(6)
046800050718     c                   leavesr
046900070326w   2***c                   when      DETftr      = 'N'
047000070326     ***c                   seton                                        512890
047100070326     ***c                   eval      V1Dmsg   = %replace
047200070326     ***c                                        (%trim(%editc(§STDlae:'K'))
047300070326     ***c                                        :$Msg(7)
047400070326     ***c                                        :%scan('§STDLAE':$Msg(7)))
047500070326     ***c                   leavesr
047600051013      * - - avendo selezionato un singolo evento:
047700051013      *     NON controllo i flags di selezione
047800051013e   2c                   endsl
047900050713      * - - evento fortuito
048000050714     c                   exsr      DecodTAD
048100051017if  2***c                   if        $UpdDET     = 'F'
048200051017     ***c*> dulcis infundo: exsr      UpdFNDET
048300051017     ***c                   seton                                        512890
048400051017     ***c                   eval      V1Dmsg      = $Msg(4)
048500051017     ***c                   leavesr
048600051017e   2***c                   endif
048700050714      * - - data apertura evento
048800050714if  2c                   if        (DETaae*10000)+DETmge < C_DataLimite
048900050803     c                   eval      $UpdDET     = 'X'
049000050915     c*> dulcis infundo: exsr      UpdFNDET
049100050728     c                   seton                                        512890
049200050803     c                   eval      V1Dmsg      = $Msg(5)
049300050714     c                   leavesr
049400050714e   2c                   endif
049500050715      * - - date C.A. legate all'evento
049600050715     c                   exsr      ElabFNDCT
049700050715      *     X = C.A. relative a spedizioni precedenti il 18/04/2005
049800050715      *     N = Totale degli importi liquidati per l'evento < §STDLAE
049900050804      *     P = Totale degli importi rimporsatici >= totale importi
050000051013      *           liquidati ai clienti
050100051017      *       & Numero delle C.A. recuperate >= Numero C.A. legate
050200051013      *           all'evento
050300050921if  2c                   IF           $UpdDET <> *blanks
050400050718     c                             and DETftr <> $UpdDET
050500050915     c*> dulcis infundo: exsr      UpdFNDET
050600050715sel 3c                   select
050700050803w   3c                   when      $UpdDET     = 'X'
050800051013     c                   seton                                        512890
050900050803     c                   eval      V1Dmsg      = $Msg(6)
051000051013     c                   leavesr
051100050803w   3c                   when      $UpdDET     = 'N'
051200051013     c                   seton                                        512890
051300050718     c                   eval      V1Dmsg   = %replace
051400050718     c                                        (%trim(%editc(§STDlae:'K'))
051500050718     c                                        :$Msg(7)
051600050718     c                                        :%scan('§STDLAE':$Msg(7)))
051700051013     c                   leavesr
051800051021w   3***c                   when         $UpdDET  = 'P'
051900051021     ***c                             and V1Cse3 <> 'S'
052000051021     ***c                             and W1Cnev  = *zeros
052100051021     ***c                   seton                                        512890
052200051021     ***c                   eval      V1Dmsg      = $Msg(8)
052300051021     ***c                   leavesr
052400050715e   3c                   endsl
052500050921e   2c                   ENDIF
052600050713      *
052700050921e   1c                   ENDIF
052800050713      *
052900051013if  1c                   if        V1Cnev      = *zeros
053000050713      * Range di date apertura evento
053100051013if  2c                   if        V1Cdai      = *zeros
053200051007     c                   eval      V1Cdai      = C_GMAlimite
053300051013e   2c                   endif
053400051013if  3c                   if        V1Cdaf      = *zeros
053500051013     c                   eval      V1Cdaf      = *date
053600051013e   3c                   endif
053700051013      *
053800051013e   1c                   endif
053900050713      * - 1ª data
054000050725      *   (dft = 18/04/2005)
054100050802if  1c                   if        V1Cdai      > *zeros
054200050718     c                   clear                   WLBdat
054300050802     c                   eval      G08dat      = V1Cdai
054400050713     c                   call      'XSRDA8'
054500050713     c                   parm                    WLBdat
054600050802if  2c                   if        G08err      = *on
054700050728     c                   seton                                        532890
054800051013     c                   eval      V1Dmsg      = $Msg(9)
054900050713     c                   leavesr
055000050713e   2c                   endif
055100050802     c                   eval      V1Cdai      = G08dat
055200050802     c                   eval      W1Cdai      = G08inv
055300051013e   1c                   endif
055400051013if  1c                   if        W1Cdai      < C_DataLimite
055500051013     c                             and W1Cnev  = *zeros
055600051013     c                   seton                                        532890
055700051013     c                   eval      V1Dmsg      = $Msg(9)
055800051013     c                   leavesr
055900051013e   1c                   endif
056000050713      * - 2ª data
056100050725      *   (dft = *date)
056200050802if  1c                   if        V1Cdaf      > *zeros
056300050718     c                   clear                   WLBdat
056400050802     c                   eval      G08dat      = V1Cdaf
056500050713     c                   call      'XSRDA8'
056600050713     c                   parm                    WLBdat
056700050802if  2c                   if        G08err      = *on
056800050728     c                   seton                                        542890
056900051013     c                   eval      V1Dmsg      = $Msg(9)
057000050713     c                   leavesr
057100050713e   2c                   endif
057200050802     c                   eval      V1Cdaf      = G08dat
057300050802     c                   eval      W1Cdaf      = G08inv
057400051007x   1c                   else
057500051007     c                   eval      W1Cdaf      = W1Cdai
057600051013e   1c                   endif
057700051013if  1c                   if        W1Cdaf      < C_DataLimite
057800051013     c                             and W1Cnev  = *zeros
057900051013     c                   seton                                        542890
058000051013     c                   eval      V1Dmsg      = $Msg(9)
058100051013     c                   leavesr
058200051013e   1c                   endif
058300050715      * - range
058400050802if  1c                   if        W1Cdai      > W1Cdaf
058500050728     c                   seton                                        532890
058600051013     c                   eval      V1Dmsg      = $Msg(10)
058700050713     c                   leavesr
058800050713e   1c                   endif
058900050713      *
059000050713     c                   ENDSR
059100050713      *
059200050713      *---------------------------------------------------------------*
059300050713      * Ricerche                                                      *
059400050713      *---------------------------------------------------------------*
059500050713     c     srSEARCH      BEGSR
059600050713      *
059700050713sel 1c                   SELECT
059800050713      *
059900050713      * Evento
060000050802w   1c                   WHEN      Wcampo      = 'V1CNEV    '
060100050713     c                   reset                   FIDN07ds
060200050713     c                   call      'FIDN07R'
060300050713     c                   parm                    KPJBA
060400050713     c                   parm                    FIDN07ds
060500050802if  2c                   if        O07err      = 'E'
060600050713     c                   clear                   V1Cnev
060700050728     c                   seton                                        5128
060800050802     c                   eval      V1Dmsg      = O07msg
060900050713x   2c                   else
061000050713     c                   move      O07nev        V1Cnev
061100050802     c                   eval      V1Caae      = O07aae
061200050713e   2c                   endif
061300050713      *
061400050713e   1c                   ENDSL
061500050713      *
061600050802     c                   eval      *in90       = *on
061700050713      *
061800050713     c                   ENDSR
061900050713      *
062000050713      *---------------------------------------------------------------*
062100050713      * Gestione videata C01/S01                                      *
062200050713      *---------------------------------------------------------------*
062300050713     c     GesS01        BEGSR
062400050713      *
062500050713      * Inizializzo la videata
062600050802if  1c                   if        $InzS01     = *on
062700050713     c                   exsr      InzS01
062800050802     c                   eval      $InzS01     = *off
062900050713e   1c                   endif
063000050713      *
063100050713      * Posiziono il cursore
063200050802if  1c                   if        C01csr      > *zeros
063300050713     c                   z-add     C01csr        C01rcd
063400050713e   1c                   endif
063500050713      *
063600050713      * Scrivo la testata e la riga tasti funzionali abilitati
063700051013     c                   eval      *in39       = (C01rcd <= *zeros)
063800050713if  1c                   if        NOT *in90
063900050713     c                   write     FI86T01
064000050713     c                   write     FI86Z02
064100050713e   1c                   endif
064200050713      *
064300050713      * Visualizzo (eventualmente) il msg per la mancanza di dati
064400050802if  1c                   if        C01rcd     <= *zeros
064500050713     c                   write     FI86D02
064600050713e   1c                   endif
064700050713      *
064800050713      * Emetto la videata
064900050713     c                   exfmt     FI86C01
065000050718     c                   z-add     £SFLnrr       C01rcd
065100050713     c                   setoff                                       28  90
065200050713     c                   clear                   V1Dmsg
065300050713      *
065400050715sel 1c                   SELECT
065500050713      * F3=Fine
065600050715w   1c                   WHEN      *inKC
065700050713     c                   exsr      F03D01
065800050727      * F8=Stampa elenco Eventi
065900050727w   1c                   WHEN      *inKH
066000050727     c                   exsr      F08S01
066100050713      * F12=Ritorno
066200050715w   1c                   WHEN      *inKL
066300050713     c                   exsr      F12S01
066400050713      * Roll-UP
066500050802w   1c                   WHEN      £Tasto      = RollUp
066600050714     c                   exsr      RollUpS01
066700051014      * NO rec.
066800051014w   1c                   WHEN      *in39
066900050713      *
067000050713      * Gestione opzioni inserite
067100050715x   1c                   OTHER
067200050713     c                   exsr      OpzS01
067300050715e   1c                   ENDSL
067400050713      *
067500050713     c                   ENDSR
067600050713      *
067700050713      *---------------------------------------------------------------*
067800050713      * Inizializzazione videata S01                                  *
067900050713      *---------------------------------------------------------------*
068000050713     c     InzS01        BEGSR
068100050713      *
068200050713      * Pulizia subfile
068300050713     c                   seton                                        3031
068400050713     c                   write     FI86C01
068500050718     c                   setoff                                         3133
068600050713      *
068700050718     c                   reset                   $EoF
068800050726     c                   clear                   W_SflPag1
068900050713     c                   clear                   C01rcd
069000050713     c                   clear                   C01csr
069100050713     c                   clear                   S01nrr
069200050713     c                   clear                   V1Dmsg
069300050713     c                   setoff                                       28  90
069400050728     c                   movea     *zeros        *in(50)
069500050713      *
069600050713      * Caricamento dei dati da presentare nel sfl
069700050725      *   (NON PIU' a pagine)
069800050802if  1c                   if        W1Cnev      > *zeros
069900050802     c                   eval      DETaae      = V1Caae
070000050802     c                   eval      DETnev      = W1Cnev
070100051013     c     K02DET01      chain(n)  FNDET000
070200050714x   1c                   else
070300050714     c                   z-add     V1Cdai        DETaae
070400050714     c     DETaae        setll     FNDET000
070500050922     c     DETaae        reade(n)  FNDET000
070600050714e   1c                   endif
070700050713      *
070800050715     c                   exsr      RollUpS01
070900050725      *
071000050725      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
071100050725      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
071200050802if  1c                   if        C01csr      > *zeros
071300050802     c                   eval      C01csr      = 1
071400050802e   1c                   endif
071500050713      *
071600050713     c                   ENDSR
071700050714      *
071800050714      *---------------------------------------------------------------*
071900050714      * Caricamento pagina successiva di dati                         *
072000050714      *---------------------------------------------------------------*
072100050714     c     RollUpS01     BEGSR
072200050714      *
072300050922     c                   eval      S01nrr      = (W_SflPag1 * C_SflPag1)
072400050726     c                   add       1             W_SflPag1
072500050802     c                   eval      *in32       = *off
072600050714      *
072700050718      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
072800050922do  1c                   doW            $EoF   = *off
072900050922     c***                          and  S01nrr < (W_SflPag1 * C_SflPag1)
073000050725      * => carico?non?solo una pagina, bensì?tutti?i rec. !
073100050916      *    (per consentire al pgm. FIDN87R - F8=stampa elenco eventi -
073200050916      *     di semplificare la lista partendo con questo sfl in input
073300050916      *     - piuttosto che rifiltrare i records dei vari archivi, come
073400050916      *     fa già, appunto, questo pgm.).
073500050718      *
073600050714     c                   exsr      CarS01
073700050921      * - Aggiornamento record FNDET00F in esame
073800070326if  2***c                   if        $UpdDET    <> *blanks
073900070326if  2c                   if           $UpdDET <> *blanks
074000070326     c                             and DETftr <> $UpdDET
074100050921     c                   exsr      UpdFNDET
074200050921e   2c                   endif
074300050718      *
074400051013if  2c                   if        W1Cnev      > *zeros
074500051013     c                   eval      $EoF        = *on
074600051013x   2c                   else
074700070326     ***c     DETaae        reade(n)  FNDET000
074800070326     ***c                   eval      $EoF        = (%eof(FNDET01L))
074900070326     c                   read(n)   FNDET000
075000070326     c                   if             %eof(FNDET01L)
075100070326     c                             or  (DETaae*10000)+DETmge > W1Cdaf
075200070326     c                   eval      $EoF        = *on
075300070326     c                   endif
075400051013e   2c                   endif
075500050718      *
075600050714e   1c                   enddo
075700050718      *
075800051013      * Visualizzazione del SFL (se ci sono dati)
075900050802     c                   eval      *in30       = (S01nrr = *zeros)
076000050714      *
076100050714      * Attivazione (eventuale) del SFLEND
076200050802     c                   eval      *in33       = $EoF
076300050714      *
076400050718      * Posizionamento cursore al 1° rcd della pagina
076500050803if  1c                   if        S01nrr   > *zeros
076600050726     c     S01nrr        div       C_SflPag1     wPag
076700050718     c                   mvr                     wRig
076800050802     c                   eval      C01rcd      = wPag * C_SflPag1
076900050802     c                   if        wRig        > *zeros
077000050802     c                   eval      C01rcd      = C01rcd + 1
077100050718     c                   else
077200050802     c                   eval      C01rcd      = C01rcd - C_SflPag1 + 1
077300050718     c                   endif
077400050803x   1c                   else
077500050803     c                   clear                   C01rcd
077600050803e   1c                   endif
077700050802     c                   eval      C01csr      = C01rcd
077800050714      *
077900050714     c                   ENDSR
078000050715      *
078100050714      *---------------------------------------------------------------*
078200050714      * Caricamento dati in subfile (singolo record / singolo Evento) *
078300050714      *---------------------------------------------------------------*
078400050714     c     CarS01        BEGSR
078500050715      *
078600050715     c                   clear                   $UpdDET
078700050714      *
078800050715      * Selezione record - se non selezionato singolo Evento - - - - -*
078900050714sel 1c                   select
079000050725      * - evento annullato
079100050802w   1c                   when      DETatb     <> *blanks
079200050725     c                   leavesr
079300050714      * - selezionato singolo Evento (già controllato in subr. CTRD01)
079400050802w   1c                   when      V1Cnev      > *zeros
079500051017      * - flag Recupero A.I.G. non valido
079600070326     *** *   N = Evento NON fortuito con C.A. pagate, ma
079700070326     *** *       con totale importi liquidati inferiore al minimo
079800050718      *   X = Evento antecedente il 18/04/2005   o
079900050718      *       con C.A. su spedizioni antecedenti il 18/04/2005
080000051017     *** *   F = Evento fortuito
080100070326w   1***c                   when          DETftr  = 'N'
080200070326     ***c                             or  DETftr  = 'X'
080300051017     ***c                             or  DETftr  = 'F'
080400070326w   1c                   when      DETftr  = 'X'
080500050718     c                   leavesr
080600051025      * - selez. eventi recuperati totalmente
080700051025      *   P = Pagato (Recuperato totalmente)
080800051025w   1c                   when          V1Cse3 <> 'S'
080900051025     c                             and DETftr  = 'P'
081000050718     c                   leavesr
081100051025      * - selez. eventi esclusi dalle richieste di recupero
081200051025      *   E = Esclusi dalle richieste di recupero
081300051025w   1c                   when          V1Cse4 <> 'S'
081400051025     c                             and DETftr  = 'E'
081500051025     c                   leavesr
081600051025     *** * - selez. eventi fortuiti
081700051025     *** *   F = Evento Fortuito
081800051025w   1***c                   when          V1Cse5 <> 'S'
081900051025     ***c                             and DETftr  = 'F'
082000051025     ***c                   leavesr
082100051025      * - selez. eventi con richiesta di recupero da eseguire
082200051025      *   _ = Richiesta di recupero da eseguire
082300051025      *   S = Richiesta di recupero parzialmente eseguita
082400051025      *       (filtrata più in basso)
082500051025      *       o  In attesa di recupero
082600051025      *      ?MA?se selezionati i soli eventi fortuiti potrebbe essere
082700051025      *         selezionabile...
082800051025w   1c***                when          V1Cse1 <> 'S'
082900051025     c***                          and DETftr  = *blanks
083000051025     c***                leavesr
083100051017      * - selez. eventi in attesa di recupero
083200051017      *   S = In attesa di recupero
083300051017      *       (Richiesta recupero già eseguita - anche parziale)
083400051013      *      ?MA?per alcune C.A. potremmo non aver ancora chiesto il
083500051017      *         recupero...
083600051013w   1c***                when          V1Cse2 <> 'S'
083700051013     c***                          and DETftr  = 'S'
083800051013     c***                leavesr
083900050714      * - data evento antecedente il 18/04/2005
084000050714w   1c                   when      (DETaae*10000)+DETmge < C_DataLimite
084100050802     c                   eval      $UpdDET     = 'X'
084200050921     c                   leavesr
084300050720      * - data Evento non compresa nel range (se immesso)
084400050720w   1c                   when          W1Cdai                > *zeros
084500050720     c                             and (DETaae*10000)+DETmge < W1Cdai
084600050720     c                              or (DETaae*10000)+DETmge > W1Cdaf
084700050714     c                   leavesr
084800050715e   1c                   endsl
084900050714      *
085000050715      * Calcoli e Decodifiche  - - - - - - - - - - - - - - - - - - - -*
085100051017      * - Decodifica/Controllo Tipo Anomalia
085200051017     c                   exsr      DecodTAD
085300050715      * - Calcolo Totale degli Importi Liquidati ai clienti
085400050715      *     relativi all'Evento     &
085500051017      *   Reperimento dati Recupero A.I.G.
085600050714     c                   exsr      ElabFNDCT
085700051011      *
085800051011      * Selezione eventi in base allo status attuale - - - - - - - - -*
085900051011      * (dopo aver eseguito tutti i calcoli relativi alle C.A.)
086000051011     c                   select
086100051011      * - scarto eventi non fortuiti con importo < imp.min. da tab.STD
086200051011w   1c                   when      $UpdDET     = 'N'
086300051011     c                   leavesr
086400051011      * - scarto eventi antecedenti il 18/4/2005
086500051011w   1c                   when      $UpdDET     = 'X'
086600051011     c                   leavesr
086700051017     *** * - scarto eventi fortuiti
086800051017w   1***c                   when      $UpdDET     = 'F'
086900051017     ***c                   leavesr
087000051017      * - scarto eventi NON fortuiti con
087100051017      *   importo liquidato <= limite in tab. STD
087200051017w   1c                   when          §TADevfo = *blanks
087300051017     c                             and wTOTipl <= §STDlae
087400051014     c                   leavesr
087500051013      * - selezionato singolo Evento:
087600051013      *   le ulteriori selezioni NON vengono considerate
087700051013w   1c                   when      W1Cnev      > *zeros
087800051011      * - per gli eventi precedentemente esclusi non eseguo altri test
087900051011w   1c                   when           V1Cse4     = 'S'
088000051011     c                             and  DETftr     = 'E'
088100051025      * - per i soli eventi fortuiti non eseguo altri test
088200051025w   1c                   when           V1Cse1    <> 'S'
088300051025     c                             and  V1Cse2    <> 'S'
088400051025     c                             and  V1Cse5     = 'S'
088500051025     c                             and  §TADevfo   = 'S'
088600051017      * - scarto eventi fortuiti (SE V1Cse5 = 'N')
088700051017w   1c                   when           V1Cse5    <> 'S'
088800051021     c                             and  §TADevfo   = 'S'
088900051017     c                   leavesr
089000051017      * - scarto  eventi con richiesta di recupero da eseguire
089100051011      *   (anche se per una sola C.A. tra tutte quelle dell'evento)
089200051011      *   se NON già esclusi
089300051011      *   (SE V1Cse1 = 'N')
089400051011w   1c                   when           V1Cse1    <> 'S'
089500051011     c                             and ($UpdDET    = *blanks
089600051011     c                              or  $UpdDET    = 'S')
089700051011     c                             and (wTOTcanrr <> *zeros
089800051011     c                              or  wTOTnocch <> *zeros
089900051011     c                              or  wTOTcanp  <> *zeros)
090000051011     c                   leavesr
090100051017      * - scarto  eventi in attesa di recupero (SE V1Cse2 = 'N')
090200051012w   1c                   when           V1Cse2    <> 'S'
090300051012     c                             and  DETftr    <> 'E'
090400051012     c                             and  $UpdDET    = 'S'
090500051012     c                             and  wTOTir1    > wTOTiri
090600051011     c                   leavesr
090700051017      * - scarto  eventi recuperati totalmente (SE V1Cse3 = 'N')
090800051012w   1c                   when           V1Cse3    <> 'S'
090900051012     c                             and  $UpdDET    = 'P'
091000051011     c                   leavesr
091100051011e   1c                   endsl
091200050715      * Reperimento Descrizione Evento da FNDCS00F
091300050715     c                   exsr      RepFNDCS
091400050714      *
091500050715      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
091600050714     c                   exsr      RieS01
091700050714      *
091800050715      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
091900050714     c                   add       1             S01nrr
092000050714     c                   write     FI86S01
092100050714      *
092200050921     c     EndCarS01     ENDSR
092300050714      *
092400050714      *---------------------------------------------------------------*
092500050714      * Caricamento dati nel record del sfl                           *
092600050714      *---------------------------------------------------------------*
092700050714     c     RieS01        BEGSR
092800050714      *
092900050714     c                   clear                   FI86S01
093000050714      *
093100050714      * - 1ª riga campi output
093200050718      *   Numero e Data Evento
093300050802     c                   eval      S1Cnev      = DETnev
093400050718     c                   reset                   WLBdat
093500050802     c                   eval      G08inv      = DETaae*10000 + DETmge
093600050718     c                   call      'XSRDA8'
093700050718     c                   parm                    WLBdat
093800050802if  1c                   if        G08err      = *off
093900051017     c***                eval      S1Cdev      = G08dat
094000051017     c                   move      G08dat        wData_eur
094100051017     c                   move      wData_eur     wData_gma
094200051017     c                   move      wData_gma     S1Cdev
094300050718e   1c                   endif
094400051017      *   Flag Evento Fortuito
094500051017if  1c                   if        §TADevfo    = 'S'
094600051017     c                   eval      S1Cevf      = 'F'
094700051017e   1c                   endif
094800051017      *   Flag Esclusione dell'Evento dalla Richiesta Recupero
094900050802if  1c                   if        DETftr      = 'E'
095000050802     c                   eval      S1Cfle      = DETftr
095100050714e   1c                   endif
095200050919      *
095300050919      * - 1ª riga campi output   &
095400050919      *   Hidden fields
095500050919     c                   exsr      RieRecS01
095600050714      *
095700050714      * - 2ª riga campi output
095800050714      *   Tipo anomalia
095900050802     c                   eval      S1Ctad      = DETtad
096000050920     c                   movel     §TADdesc      S1Dtad
096100050714      *   Descrizione Evento (Note)
096200050914     c                   movel     DCSnot        S1Dnot
096300050714      *   P.O. che ha segnalato l'Evento
096400050802     c                   eval      S1Cfce      = DETfce
096500050714      *
096600050714      * - Indicatori
096700050728     c     S1Cfle        comp      *blanks                            4141
096800051017     c     S1Cevf        comp      *blanks                            4242
096900050714      *
097000050714     c                   ENDSR
097100050718      *
097200050718      *---------------------------------------------------------------*
097300050718      * Gestione tasto funzionale F08 da videata S01                  *
097400050719      * F8-Stampa elenco Eventi                                       *
097500050718      *---------------------------------------------------------------*
097600050718     c     F08S01        BEGSR
097700050718      *
097800050728      * Questo pgm. NON necessita di parametri perchè accede al sfl.
097900050919      * => FALSO !!!
098000050919      * Dovendo stampare i parametri di lancio (D01)  &
098100050919      * non potendo accedere al fmt D01 senza EXFMT (write e read)
098200050919      *   ho impostato i parametri di lancio in una ds interna.
098300050919     c                   movel(p)  FIDN86ds      KPJBU
098400050919      *
098500050719     c                   call      'FIDN87R'
098600050719     c                   parm                    KPJBA
098700050718      *
098800050718     c                   ENDSR
098900050718      *
099000050718      *---------------------------------------------------------------*
099100050718      * Gestione tasto funzionale F12 da videata S01                  *
099200050718      * F12-Ritorno                                                   *
099300050718      *---------------------------------------------------------------*
099400050718     c     F12S01        BEGSR
099500050718      *
099600050718      * Torno alla videata precedente
099700050802     c                   eval      $Video      = '1'
099800050718      *
099900050718     c                   ENDSR
100000050713      *
100100050713      *---------------------------------------------------------------*
100200050713      * Controllo opzioni subfile                                     *
100300050713      *---------------------------------------------------------------*
100400050713     c     OpzS01        BEGSR
100500050713      *
100600050921     c                   clear                   SavS01csr
100700050921      *
100800050713     c                   readc     FI86S01
100900050713      *
101000050713do  1c                   DOW       NOT %eof(FIDN86D)
101100050713      *
101200050802     c                   eval      *in32       = *off
101300050728     c                   movea     *zeros        *in(50)
101400050713     c                   z-add     S01nrr        C01rcd
101500050713      *
101600050715sel 2c                   SELECT
101700050713      * - Nessuna opzione
101800050802w   2c                   WHEN      S1Copz      = *blanks
101900050713      * - G = Gestione delle singole C.A. legate all'evento
102000050802w   2c                   WHEN      S1Copz      = 'G'
102100050726     c                   exsr      sr_OpzS1_G
102200051017      * - E = Esclusione dell'evento dalla richiesta di recupero
102300050720      * - I = Inclusione dell'evento precedentemente escluso
102400050802w   2c                   WHEN          S1Copz  = 'E'
102500050802     c                             or  S1Copz  = 'I'
102600050803     c                   exsr      sr_OpzS1_EI
102700051017      * - R = Richiesta di recupero dell'evento ad A.I.G.
102800050803w   2c                   WHEN      S1Copz      = 'R'
102900050726     c                   exsr      sr_OpzS1_R
103000050715      * - ? = Opzione NON valida
103100050715x   2c                   OTHER
103200050715     c                   seton                                        502890
103300051013     c                   eval      V1Dmsg      = $Msg(11)
103400050715e   2c                   ENDSL
103500050921      *
103600050921      * Memorizzazione nrr del sfl con la 1ª opzione
103700050921      * per riposizionarvici il cursore dopo il tasto "Invio"
103800050921if  1c                   if             S1Copz   <> *blanks
103900050921     c                             and (SavS01csr = *zeros
104000050921     c                              or  SavS01csr < C01rcd)
104100050921     c                   eval      SavS01csr   = C01rcd
104200050921e   1c                   endif
104300050715      *
104400050726      * Reimpostazione indicatori per DSPATR
104500050728     c     S1Cfle        comp      *blanks                            4141
104600051017     c     S1Cevf        comp      *blanks                            4242
104700050726      *
104800050726      * Aggiornamento sfl
104900050920sel 2c                   select
105000050920w   2c                   when      *in28
105100050802     c                   eval      *in32       = *on
105200050718     c                   z-add     C01rcd        C01csr
105300050920w   2c                   when      *in90       = *on
105400050920     c                   z-add     C01rcd        C01csr
105500050920     c                   clear                   S1Copz
105600050920x   2c                   other
105700050715     c                   clear                   S1Copz
105800050920e   2c                   endsl
105900050721     c                   UPDATE    FI86S01
106000050920if  2c                   if        *in28  OR  *in90
106100050721     c                   leave
106200050721e   2c                   endif
106300050715      *
106400050715     c                   readc     FI86S01
106500050715      *
106600050715e   1c                   ENDDO
106700050921      *
106800050921      * Riposiziono il cursore sul record contenente la prima opzione
106900050921      *  (se non sono stati rilevati errori)
107000050921     c                   if        NOT *in28  and  NOT *in90
107100050921     c                             and SavS01csr  > *zeros
107200050921     c                   z-add     SavS01csr     C01csr
107300050921     c                   endif
107400050920      *
107500050920      * Se *in90 = *on (no *in28) è stato premuto F3 da C02/S02:
107600050920      *   riemetto C01/S01, ma anche testata (T01) e piede (Z02)
107700050920      *   (visto che cambiano i tasti funzionali abilitati)
107800050920if  1c                   if        *in90  and  NOT *in28
107900050920     c                   eval      *in90       = *off
108000050920e   1c                   endif
108100050713      *
108200050713     c                   ENDSR
108300050801      *
108400050801      *---------------------------------------------------------------*
108500050801      * S1_Opz.G = Gestione delle singole C.A. legate all'evento      *
108600050801      *---------------------------------------------------------------*
108700050801     c     sr_OpzS1_G    BEGSR
108800050801      *
108900050801     c                   clear                   FI86P02
109000050802     c                   eval      *in40       = *off
109100050801      *
109200051017      * Passaggio alla videata C02/S02 per stampa richiesta recupero
109300050803      *  (non gestita a normale ciclo perchè in uscita da sr GesS02
109400050803      *   verrebbe riemesso S01 - senza terminare l'elaborazione di
109500050803      *   altre opzioni eventualmente in esso già immesse insieme a
109600050803      *   questa)
109700050802     c                   eval      $Video      = '3'
109800050802     c                   eval      $InzS02     = *on
109900050801      *
110000050803do  1c                   doW           $Video  = '3'
110100050801     c                             and $Fine   = *off
110200050801     c                   exsr      GesS02
110300050801e   1c                   enddo
110400050801      *
110500050803      * Aggiornamento dati nel rec. del sfl. S01
110600050920if  1c                   if        NOT *in28  and  NOT *in90
110700050920      * Va inizializzato il sfl se effettuata parzializzazione:
110800051017      * · NO eventi con richieste di recupero da eseguire
110900051017      * · NO eventi in attesa di recupero
111000051017      * · NO eventi recuperati totalmente
111100051010if  2c                   if            V1Cse1 <> 'S'
111200051010     c                             OR  V1Cse2 <> 'S'
111300051010     c                             OR  V1Cse3 <> 'S'
111400051010     c                             and (H1canc > *zeros
111500051010     c                              or  H1cara < (H1cale - H1caol)
111600051010     c                              or  S1Ciri < S1Cir1)
111700050803     c                   eval      $InzS01     = *on
111800050803x   2c                   else
111900050803     c                   exsr      UpdRecS01
112000050803e   2c                   endif
112100050803e   1c                   endif
112200050801      *
112300050801     c                   ENDSR
112400050801      *
112500050801      *---------------------------------------------------------------*
112600051017      * S1_Opz.E = Esclusione dell'evento dalla richiesta di recupero *
112700050803      * S1_Opz.I = Inclusione dell'evento precedentemente escluso     *
112800050801      *---------------------------------------------------------------*
112900050803     c     sr_OpzS1_EI   BEGSR
113000050801      *
113100050801      * controlli per opzione
113200050803if  1c                   if        S1Cdrr      > *zeros
113300050801     c                   seton                                        502890
113400051013     c                   eval      V1Dmsg      = $Msg(20)
113500050801     c                   leavesr
113600050803e   1c                   endif
113700050801      *
113800050802     c                   eval      $UpdDET     = S1Cfle
113900050801      *
114000050801      * Aggiornamento del flag DETFTR nel file FNDET00F
114100050920     c                   exsr      RepFNDET
114200050801if  1c                   IF        %found(FNDET01L)
114300050801      *
114400050801sel 2c                   select
114500050802w   2c                   when      S1Copz      = 'I'
114600050802     c                   eval      $UpdDET     = *blanks
114700050802w   2c                   when      S1Copz      = 'E'
114800050802     c                   eval      $UpdDET     = 'E'
114900050801e   2c                   endsl
115000050801      *
115100050801     c                   exsr      UpdFNDET
115200050801e   1c                   ENDIF
115300050801      *
115400050801      * Aggiornamento del flag S1CFLE nel subfile
115500050801      * (l'update del rec. verrà eseguita nella subr. chiamante OPZS01)
115600050802     c                   eval      S1Cfle      = $UpdDET
115700050803      *
115800050920      * Va inizializzato il sfl se effettuata parzializzazione:
115900051017      * · NO eventi con richieste di recupero da eseguire
116000051017      * · NO eventi in attesa di recupero
116100051017      * · NO eventi recuperati totalmente
116200051017      * · NO eventi esclusi dalle richieste di recupero
116300051010if  1c                   if            V1Cse1 <> 'S'
116400051010     c                             or  V1Cse2 <> 'S'
116500051010     c                             or  V1Cse3 <> 'S'
116600051010     c                             or  V1Cse4 <> 'S'
116700050803     c                   eval      $InzS01     = *on
116800050803e   1c                   endif
116900050801      *
117000050801     c                   ENDSR
117100050801      *
117200050801      *---------------------------------------------------------------*
117300051017      * S1_Opz.R = Richiesta di recupero dell'evento ad A.I.G.        *
117400050801      *---------------------------------------------------------------*
117500050801     c     sr_OpzS1_R    BEGSR
117600050801      *
117700050801      * Verifica se Evento già escluso
117800050802if  1c                   if        S1Cfle      = 'E'
117900050801     c                   seton                                        502890
118000051013     c                   eval      V1Dmsg      = $Msg(12)
118100050802     c                   leavesr
118200050801e   1c                   endif
118300050802      *
118400051017      * Passaggio alla videata C02/S02 per stampa richiesta recupero
118500050803      *  (non gestita a normale ciclo perchè in uscita da sr GesS02
118600050803      *   verrebbe riemesso S01 - senza terminare l'elaborazione di
118700050803      *   altre opzioni eventualmente in esso già immesse insieme a
118800050803      *   questa)
118900050803     c                   eval      $Video      = '3'
119000050803     c                   eval      $InzS02     = *on
119100050803      *
119200050803do  1c                   doW           $Video  = '3'
119300050803     c                             and $Fine   = *off
119400050803     c                   exsr      GesS02
119500050803e   1c                   enddo
119600050803      *
119700050803      * Aggiornamento dati nel rec. del sfl. S01
119800050920if  1c                   if        NOT *in28  and  NOT *in90
119900050920      * Va inizializzato il sfl se effettuata parzializzazione:
120000051017      * · solo eventi con richieste di recupero da eseguire
120100051017      * · solo eventi in attesa di recupero
120200051017      * o se recuperato tutto l'evento (non più visualizzabile)
120300051010if  2c                   if            V1Cse1 <> 'S'
120400051010     c                             OR  V1Cse2 <> 'S'
120500051010     c                             OR  V1Cse3 <> 'S'
120600051010     c                             and (H1canc > *zeros
120700051010     c                              or  H1cara < (H1cale - H1caol)
120800051010     c                              or  S1Ciri < S1Cir1)
120900050803     c                   eval      $InzS01     = *on
121000050803x   2c                   else
121100050803     c                   exsr      UpdRecS01
121200050803e   2c                   endif
121300050803e   1c                   endif
121400050801      *
121500050801     c                   ENDSR
121600050803      *
121700050803      *---------------------------------------------------------------*
121800050803      * Aggiornamento dati nel record del subfile S01                 *
121900050803      *---------------------------------------------------------------*
122000050803     c     UpdRecS01     BEGSR
122100050803      *
122200051017      * > Reperimento Recuperi A.I.G.: · Data Richiesta Recupero
122300051017      *                                · Data Valuta Recupero
122400051017      *                                · Importi Recuperati
122500050920     c                   exsr      RepFNDET
122600050919if  1c                   if        NOT %found(FNDET01L)
122700050919     c                             or  DETatb <> *blanks
122800050919     c                   leavesr
122900050919e   1c                   endif
123000050919      *
123100050803     c                   exsr      ElabFNDCT
123200050921if  1c                   if        $UpdDET    <> *blanks
123300050804     c                   exsr      UpdFNDET
123400050921e   1c                   endif
123500050919     c                   exsr      RieRecS01
123600050919      *
123700050919     c                   ENDSR
123800050919      *
123900050919      *---------------------------------------------------------------*
124000050919      * Aggiornamento dati nel record del subfile S01                 *
124100050919      *---------------------------------------------------------------*
124200050919     c     RieRecS01     BEGSR
124300050919      *
124400051017      *   Data Ultima Richiesta Recupero
124500050919if  1c                   if        wDRAdrr     > *zeros
124600050803     c                   reset                   WLBdat
124700050803     c                   eval      G08inv      = wDRAdrr
124800050803     c                   call      'XSRDA8'
124900050803     c                   parm                    WLBdat
125000050919if  2c                   if        G08err      = *off
125100050803     c                   eval      S1Cdrr      = G08dat
125200050919e   2c                   endif
125300050919e   1c                   endif
125400051017      *   Data Ultimo Recupero Ricevuto
125500050919if  1c                   if        wDRAdvr     > *zeros
125600050803     c                   reset                   WLBdat
125700050803     c                   eval      G08inv      = wDRAdvr
125800050803     c                   call      'XSRDA8'
125900050803     c                   parm                    WLBdat
126000050919if  2c                   if        G08err      = *off
126100050803     c                   eval      S1Cdvr      = G08dat
126200050919e   2c                   endif
126300050919e   1c                   endif
126400050803      *   Totale Importi Liquidati ai clienti relativi all'Evento
126500050803     c                   z-add     wTOTipl       S1Cipl
126600051017      *   Totale Importi Richiesti all'A.I.G. relativi all'Evento
126700050803     c                   z-add     wTOTir1       S1Cir1
126800051017      *   Totale Importi Recuperati dall'A.I.G. relativi all'Evento
126900050803     c                   z-add     wTOTiri       S1Ciri
127000050920      *
127100050803      * - Hidden fields
127200050920      *   Descrizione evento (completa)
127300050920     c                   eval      H1Dtad      = §TADdesc
127400051017      *   Anno evento (4 cifre)
127500051017     c                   eval      H1Caae      = DETaae
127600050803      *   Numero totale C.A. legate all'Evento
127700050915     c                   eval      H1cale      = wTOTcale
127800050919      *   Numero totale C.A. legate all'Evento relative a spedizioni
127900050919      *    precedenti il 18/04/2005
128000050919     c                   eval      H1caol      = wTOTcaold
128100050919      *   Numero totale C.A. ancora aperte legate all'Evento
128200050919     c                   eval      H1canc      = wTOTnocch
128300050803      *   Numero totale C.A. legate all'Evento non ancora pagate
128400050915     c                   eval      H1canp      = wTOTcanp
128500050919      *   Numero totale C.A. legate all'Evento pagate per le quali non
128600051017      *    è ancora stato richiesto il recupero ad A.I.G.
128700050919     c                   eval      H1canr      = wTOTcanrr
128800051017      *   Numero totale C.A. evento già recuperati da A.I.G.
128900050919     c                   eval      H1cara      = wTOTcara
129000051017      *   Numero totale C.A. evento non ancora recuperati da A.I.G.
129100050919     c                   eval      H1carr      = wTOTcar1
129200051017      *   Divisa Importi Liquidati  ai clienti  relativi all'Evento
129300050803     c                   eval      H1Cvpl      = W1Cvpl
129400051017      *   Divisa Importi Richiesti  all'A.I.G.  relativi all'Evento
129500050803     c                   eval      H1Cdi1      = W1Cdi1
129600051017      *   Divisa Importi Recuperati dall'A.I.G. relativi all'Evento
129700050803     c                   eval      H1Cdir      = W1Cdir
129800050803      *
129900050803     c                   ENDSR
130000050726      *
130100050726      *---------------------------------------------------------------*
130200050726      * Gestione videata C02/S02                                      *
130300050726      *---------------------------------------------------------------*
130400050726     c     GesS02        BEGSR
130500050726      *
130600050726      * Inizializzo la videata
130700050803if  1c                   if        $InzS02     = *on
130800050726     c                   exsr      InzS02
130900050803     c                   eval      $InzS02     = *off
131000050727     c                   exsr      RieP02
131100050726e   1c                   endif
131200050726      *
131300050726      * Posiziono il cursore
131400050803if  1c                   if        C02csr      > *zeros
131500050726     c                   z-add     C02csr        C02rcd
131600050726e   1c                   endif
131700050726      *
131800050726      * Scrivo testata, piede e riga dei tasti funzionali abilitati
131900051013     c                   eval      *in39       = (C02rcd <= *zeros)
132000050726if  1c                   if        NOT *in90
132100050726     c                   write     FI86T01
132200050726     c                   write     FI86P02
132300050726     c                   write     FI86Z03
132400050726e   1c                   endif
132500050726      *
132600050726      * Visualizzo (eventualmente) il msg per la mancanza di dati
132700050803if  1c                   if        C02rcd     <= *zeros
132800050726     c                   write     FI86D03
132900050726e   1c                   endif
133000050726      *
133100050726      * Emetto la videata
133200050726     c                   exfmt     FI86C02
133300050726     c                   z-add     £SFLnrr       C02rcd
133400050726     c                   setoff                                       28  90
133500050726     c                   clear                   V1Dmsg
133600050726      *
133700050726sel 1c                   SELECT
133800050726      * F3=Fine
133900050914w   1c                   WHEN      *inKC
134000050920     c                   exsr      F12S02
134100050920     c                   eval      *in90       = *on
134200051017      * F8=Visualizza totale richiesto/recuperato
134300050727w   1c                   WHEN      *inKH
134400050727     c                   exsr      F08S02
134500050727      * F12=Ritorno
134600050727w   1c                   WHEN      *inKL
134700050727     c                   exsr      F12S02
134800050726      * Roll-UP
134900050803w   1c                   WHEN      £Tasto      = RollUp
135000050726     c                   exsr      RollUpS02
135100051014      * NO rec.
135200051014w   1c                   WHEN      *in39
135300050726      *
135400050726      * Gestione opzioni inserite
135500050726x   1c                   OTHER
135600050726     c                   exsr      OpzS02
135700051017      * F6=Stampa richiesta recupero ad A.I.G.
135800050802if  2c                   if            *inKF
135900050803     c                             and *in90   = *off
136000050802     c                   exsr      F06S02
136100050802e   2c                   endif
136200050802e   1c                   ENDSL
136300050726      *
136400050726     c                   ENDSR
136500050726      *
136600050726      *---------------------------------------------------------------*
136700050726      * Inizializzazione videata S02                                  *
136800050726      *---------------------------------------------------------------*
136900050726     c     InzS02        BEGSR
137000050726      *
137100050726      * Pulizia subfile
137200050728     c                   seton                                        3536
137300050726     c                   write     FI86C02
137400050728     c                   setoff                                         3638
137500050726      *
137600050803     c                   clear                   wCountOpzR2
137700050726     c                   reset                   $EoF
137800050726     c                   clear                   W_SflPag2
137900050726     c                   clear                   C02rcd
138000050726     c                   clear                   C02csr
138100050726     c                   clear                   S02nrr
138200050726     c                   clear                   V1Dmsg
138300050726     c                   setoff                                       28  90
138400050728     c                   movea     *zeros        *in(50)
138500050726      *
138600050803     c                   eval      C2Cnev      = S1Cnev
138700051017     c***                z-add     S1Cdev        C2Caae
138800051017     c                   eval      C2Caae      = H1Caae
138900051025     c                   clear                   C2Devf
139000051025if  1c                   if        S1Cevf      = 'F'
139100051025     c                   eval      C2Devf      = 'FORTUITO  '
139200051025e   1c                   endif
139300050920     c                   eval      C2Ctad      = S1Ctad
139400050920     c                   eval      C2Dtad      = H1Dtad
139500050726      *   Numero totale C.A. legate all'Evento non ancora pagate
139600050915     c                   z-add     H1canp        C2Canp
139700050919      *   Numero totale C.A. legate all'Evento pagate per le quali non
139800051017      *    è ancora stato richiesto il recupero ad A.I.G.
139900050919     c                   z-add     H1canr        C2Canr
140000051017      *   Numero totale C.A. legate all'Evento non ancora recuperate
140100050919      *   da A.I.G.:
140200050919     c                   z-add     H1carr        C2Carr
140300051017      *   Numero totale C.A. legate all'Evento già recuperate da A.I.G.
140400050915     c                   z-add     H1cara        C2Cagr
140500050726      *
140600050726      * Caricamento dei dati da presentare nel sfl
140700050726      *   a pagine
140800050920     c                   eval      DCTaae      = C2Caae
140900050920     c                   eval      DCTnev      = C2Cnev
141000050919     c                   eval      DCTfca      = *loval
141100050726     c     K03DCT08      setll     FNDCT000
141200050919     c     K02DCT08      reade     FNDCT000
141300050726      *
141400050726     c                   exsr      RollUpS02
141500050803      *
141600051017      * Se chiesto il recupero per tutto l'evento dal sfl S01:
141700050803      *   emetto "direttamente" il messaggio per la richiesta di F6
141800050803      *   per conferma (senza *in90, che inibisce l'emissione di
141900050803      *   testata e piede del sfl S02)
142000050804if  1c                   if        S1Copz      = 'R'
142100050804if  2c                   if            S02nrr      <= *zeros
142200050804     c                             or  wCountOpzR2 <= *zeros
142300051013     c                   eval      V1Dmsg      = $Msg(18)
142400050803     c                   eval      *in28       = *on
142500050804x   2c                   else
142600050804     c                   exsr      OpzS02
142700050804     c                   eval      *in90       = *off
142800050804e   2c                   endif
142900050803e   1c                   endif
143000050726      *
143100050726     c                   ENDSR
143200050726      *
143300050726      *---------------------------------------------------------------*
143400050726      * Caricamento pagina successiva di dati                         *
143500050726      *---------------------------------------------------------------*
143600050726     c     RollUpS02     BEGSR
143700050726      *
143800050922     c                   eval      S02nrr      = (W_SflPag2 * C_SflPag2)
143900050726     c                   add       1             W_SflPag2
144000050803     c                   eval      *in37       = *off
144100050726      *
144200050726      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
144300050922      * ("per pagina" se richiesta opz. "G" da S01;
144400050922      *  "totale"     se richiesta opz. "R" da S01)
144500050804do  1c                   doW            $EoF   = *off
144600050804     c                             and (S1Copz = 'R'
144700050804     c                              or  S1Copz = 'G'
144800050922     c                              and S02nrr < (W_SflPag2 * C_SflPag2))
144900050726      *
145000050726     c                   exsr      CarS02
145100050726      *
145200050919     c     K02DCT08      reade     FNDCT000
145300050803     c                   eval      $EoF        = (%eof(FNDCT08L))
145400050726      *
145500050726e   1c                   enddo
145600050726      *
145700050726      * Viualizzazione del SFL (se ci sono dati)
145800050803     c                   eval      *in35       = (S02nrr = *zeros)
145900050726      *
146000050726      * Attivazione (eventuale) del SFLEND
146100050803     c                   eval      *in38       = $EoF
146200050726      *
146300050726      * Posizionamento cursore al 1° rcd della pagina
146400050803if  1c                   if        S02nrr      > *zeros
146500050803      *
146600050803      * - Se sono stati caricati TUTTI i dati (NON SOLO quelli della
146700050803      *   1ª pagina) occorre impostare il posizionamento cursore sul
146800050803      *   1° rec del sfl
146900050804if  2c                   if        S1Copz      = 'R'
147000050803     c                   eval      C02rcd      = 1
147100050803x   2c                   else
147200050803      * - Altrimenti il cursore va posizionato sul 1° record della a
147300050803      *   nuova pagina
147400050726     c     S02nrr        div       C_SflPag2     wPag
147500050726     c                   mvr                     wRig
147600050803     c                   eval      C02rcd      = wPag * C_SflPag2
147700050803     c                   if        wRig        > *zeros
147800050803     c                   eval      C02rcd      = C02rcd + 1
147900050726     c                   else
148000050803     c                   eval      C02rcd      = C02rcd - C_SflPag2 + 1
148100050726     c                   endif
148200050803x   2c                   endif
148300050803x   1c                   else
148400050803     c                   clear                   C02rcd
148500050803e   1c                   endif
148600050803     c                   eval      C02csr      = C02rcd
148700050726      *
148800050726     c                   ENDSR
148900050726      *
149000050726      *---------------------------------------------------------------*
149100050726      * Caricamento dati in subfile (singolo record / singola C.A.)   *
149200050726      *---------------------------------------------------------------*
149300050726     c     CarS02        BEGSR
149400050726      *
149500050726     c                   clear                   $UpdDET
149600050726      *
149700051017      *Come in subr. "ElabFNDCT":?
149800050726      *
149900050726      * C.A. annullata
150000050919if  1c                   if        DCTatb  <> *blanks
150100050726     c                   leavesr
150200050726e   1c                   endif
150300050919      * - Selezione per data spedizione (< 18/04/2005)
150400050919      *   (la singola c.a. viene conteggiata, ma non fa escludere
150500050919      *   tutto l'evento)
150600050919if  1c                   if        DCTaas   < C_AnnoLimite
150700050919     c                   leavesr
150800050919x   1c                   else
150900050919     c                   exsr      RepTITAS
151000050919if  2c                   if        (TASaas * 10000 + TASmgs)
151100050919     c                                      < C_DataLimite
151200050919     c                   leavesr
151300050919e   2c                   endif
151400050919e   1c                   endif
151500050726      *
151600050726      * Selezione delle C.A. chiuse con causale di pagamento
151700050920if  1c                   if        DCTfca   = C_F_ChiusCA
151800050919     c                   eval      *in10    = *off
151900050726     c     DCTcch        lookup    $CCHpag                                10
152000050919if  2c                   if        NOT *in10
152100050726     c                   leavesr
152200050919e   2c                   endif
152300050920e   1c                   endif
152400050726      *
152500050726      * Selezione delle C.A. contabilizzate
152600050920      * => NO: le si visualizzano tutte !
152700050920      *    Reperisco comunque l'importo liquidato (zero se C.A. aperta)
152800050920     c                   exsr      RepFNDCL
152900051013if  1c***                if        DCLfic     <> 'C'
153000051013     c***                leavesr
153100051013e   1c***                endif
153200050726      *
153300051025      * Reperimento Recuperi da A.I.G.
153400050726     c                   exsr      RepFNDRA
153500050726      *
153600050726      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
153700050726     c                   exsr      RieS02
153800050726      *
153900050726      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
154000050726     c                   add       1             S02nrr
154100050726     c                   write     FI86S02
154200050726      *
154300050726     c                   ENDSR
154400050726      *
154500050726      *---------------------------------------------------------------*
154600050726      * Caricamento dati nel record del sfl                           *
154700050726      *---------------------------------------------------------------*
154800050726     c     RieS02        BEGSR
154900050726      *
155000050726     c                   clear                   FI86S02
155100050728      *
155200050728      * Campo opzione
155300050802if  1c                   if            S1Copz  = 'R'
155400050802     c                             and DRAdrr  = *zeros
155500050802     c                             and DRAesc  = *blanks
155600050920     c                             and DCTfca  = C_F_ChiusCA
155700050920     c                             and DCLipl  > *zeros
155800050920     c                             and DCLfic  = 'C'
155900050802     c                   eval      *in37       = *on
156000050802     c                   eval      S2Copz      = S1Copz
156100050803     c                   add       1             wCountOpzR2
156200050728e   1c                   endif
156300050726      *
156400050726      * Campi visualizzati
156500050726      * - P.O. C.A.
156600050802     c                   eval      S2Cfil      = DCTfil
156700050726      * - Numero C.A.
156800050802     c                   eval      S2Cnca      = DCTnca
156900050726      * - Data C.A.
157000050726     c                   reset                   WLBdat
157100050802     c                   eval      G08inv      = DCTaac*10000 + DCTmgc
157200050726     c                   call      'XSRDA8'
157300050726     c                   parm                    WLBdat
157400050802if  1c                   if        G08err      = *off
157500050802     c                   eval      S2Cdca      = G08dat
157600050726e   1c                   endif
157700050726      * - Importo liquidato
157800050922      *   (se C.A. chiusa e contabilizzata)
157900050922if  1c                   if        DCLfic      = 'C'
158000050726     c                   z-add     DCLipl        S2Cipl
158100050922e   1c                   endif
158200050726      * - Data richiesta rimbrso
158300050802if  1c                   if        DRAdrr      > *zeros
158400050726     c                   reset                   WLBdat
158500050802     c                   eval      G08inv      = DRAdrr
158600050726     c                   call      'XSRDA8'
158700050726     c                   parm                    WLBdat
158800050802if  2c                   if        G08err      = *off
158900050802     c                   eval      S2Cdrr      = G08dat
159000050726e   2c                   endif
159100050726e   1c                   endif
159200051017      * - Importo recuperato
159300050802     c                   eval      S2Ciri      = DRAiri
159400051017      * - Data valuta recupero
159500050802if  1c                   if        DRAdvr      > *zeros
159600050726     c                   reset                   WLBdat
159700050802     c                   eval      G08inv      = DRAdvr
159800050726     c                   call      'XSRDA8'
159900050726     c                   parm                    WLBdat
160000050802if  2c                   if        G08err      = *off
160100050802     c                   eval      S2Cdvr      = G08dat
160200050726e   2c                   endif
160300050726e   1c                   endif
160400051017      * - Flag Esclusione della C.A. dalla Richiesta Recupero
160500050802if  1c                   if        DRAesc      = 'E'
160600050802     c                   eval      S2Cesc      = DRAesc
160700050726e   1c                   endif
160800050726      *
160900050726      * Campi hidden
161000050726      * - Evento
161100050802     c                   eval      H2Caae      = DCTaae
161200050914     c                   eval      H2Cmge      = DETmge
161300050802     c                   eval      H2Cnev      = DCTnev
161400050726      * - Data C.A.
161500050802     c                   eval      H2Caac      = DCTaac
161600050802     c                   eval      H2Cmgc      = DCTmgc
161700050726      * - Fase C.A.
161800050802     c                   eval      H2Cfca      = DCTfca
161900050921      * - Contabilizzazione
162000050921     c                   eval      H2Cfic      = DCLfic
162100050726      * - Tipo anomalia
162200050802     c                   eval      H2Ctad      = DCTtad
162300050726      * - Spedizione
162400050802     c                   eval      H2Caas      = DCTaas
162500050802     c                   eval      H2Clnp      = DCTlnp
162600050802     c                   eval      H2Cnrs      = DCTnrs
162700050802     c                   eval      H2Cnsp      = DCTnsp
162800050726      *
162900050726      * Indicatori
163000050728     c     S2Cesc        comp      *blanks                            4141
163100050726      *
163200050726     c                   ENDSR
163300050726      *
163400050726      *---------------------------------------------------------------*
163500050726      * Gestione tasto funzionale F06 da videata S02                  *
163600051017      * F6-Stampa richiesta recupero ad A.I.G. per singola C.A. ("R") *
163700050726      *---------------------------------------------------------------*
163800050726     c     F06S02        BEGSR
163900050726      *
164000050726     c                   reset                   FIDN00ds
164100050726      *
164200050802     c                   eval      I00aac      = H2Caac
164300050802     c                   eval      I00fil      = S2Cfil
164400050802     c                   eval      I00nca      = S2Cnca
164500050802     c                   eval      I00fca      = H2Cfca
164600050802     c                   eval      I00tae      = H2Ctad
164700050802     c                   eval      I00aae      = DETaae
164800050802     c                   eval      I00nev      = DETnev
164900050802     c                   eval      I00dev      = DETaae*10000 + DETmge
165000050726      *
165100050726     c                   movel(p)  FIDN00ds      KPJBU
165200050726     c                   call      'FIDN88R'
165300050726     c                   parm                    KPJBA
165400050915     c                   movel     KPJBU         FIDN00ds
165500050726      *
165600050728      * Emissione dell'errore rilevato (?se?rilevato )
165700050802if  1c                   if        O00err     <> *blanks
165800050728     c                   movel     O00msg        V1Dmsg
165900050728     c                   seton                                        28  90
166000050728     c                   leavesr
166100050801e   1c                   endif
166200050914      *
166300050914      * Uscita senza aggiornamento (F3 o F12)
166400050920      * (F3: si torna alla videata precedente)
166500050920if  1c                   if        O00f03      = 'S'
166600050920     c                   exsr      F12S02
166700050920     c                   eval      *in90       = *on
166800050920e   1c                   endif
166900050920if  1c                   if           O00f12   = 'S'
167000050920     c                             or O00f03   = 'S'
167100050914     c                   leavesr
167200050914e   1c                   endif
167300050728      *
167400050728      * Aggiornamento dati di totale nel piede (P02) - già calcolati
167500050920      * - Numero totale C.A. legate all'Evento pagate per le quali non
167600051017      *    è ancora stato richiesto il recupero ad A.I.G.
167700050923     c                   sub       wCountOpzR2   H1canr
167800050923     c                   eval      wTOTcanrr   = H1canr
167900050920     c                   z-add     H1canr        C2Canr
168000051017      * - Numero totale C.A. evento non ancora recuperati da A.I.G.
168100050923     c                   add       wCountOpzR2   H1carr
168200050923     c                   eval      wTOTcar1    = H1carr
168300050920     c                   z-add     H1carr        C2Carr
168400050915      * - Totale Importi Richiesti all'A.I.G. relativi all'Evento
168500050728     c                   add       wSumImpRic    S1Cir1
168600050923     c                   eval      wTOTir1     = S1Cir1
168700050801if  1c                   if            S1Cir1 <> *zeros
168800050801     c                             and H1Cdi1  = *blanks
168900050801     c                   movel     §GEDdba       H1Cdi1
169000050801e   1c                   endif
169100050802     c                   eval      *in40       = *on
169200050728     c                   exsr      RieP02
169300050726      *
169400050726      * Aggiornamento rec. nel sfl.
169500051017      * (se per l'evento ci fossero state recuperate tutte le C.A.,
169600050915      * non lo si dovrebbe più visualizzare anche nel sfl S01)
169700050801if  1c                   if        NOT *in28
169800050802     c                   eval      $InzS02     = *on
169900050801e   1c                   endif
170000050803      *
170100050803      * Aggiornamento campo opzione in sfl S01
170200050804      * (per non rieseguire i controlli relativi all'opz. "R" in S02)
170300050803if  1c                   if        NOT *in28
170400050804     c                             and S1Copz  = 'R'
170500050804     c                   eval      S1Copz      = 'G'
170600050803e   1c                   endif
170700050726      *
170800050726     c                   ENDSR
170900050727      *
171000050727      *---------------------------------------------------------------*
171100050727      * Gestione tasto funzionale F08 da videata S02                  *
171200051017      * F8-Visualizza totale richiesto / recuperato                   *
171300050727      *---------------------------------------------------------------*
171400050727     c     F08S02        BEGSR
171500050727      *
171600050802     c                   if        *in40       = *on
171700050802     c                   eval      *in40       = *off
171800050727     c                   else
171900050802     c                   eval      *in40       = *on
172000050727     c                   endif
172100050727      *
172200050727     c                   exsr      RieP02
172300050727      *
172400050727     c                   ENDSR
172500050726      *
172600050726      *---------------------------------------------------------------*
172700050726      * Gestione tasto funzionale F12 da videata S02                  *
172800050726      * F12-Ritorno                                                   *
172900050726      *---------------------------------------------------------------*
173000050726     c     F12S02        BEGSR
173100050726      *
173200050726      * Torno alla videata precedente
173300050802     c                   eval      $Video      = '2'
173400050726      *
173500050726     c                   ENDSR
173600050726      *
173700050726      *---------------------------------------------------------------*
173800050726      * Controllo opzioni subfile                                     *
173900050726      *---------------------------------------------------------------*
174000050726     c     OpzS02        BEGSR
174100050726      *
174200050921     c                   clear                   SavS02csr
174300050921     c                   clear                   SavS02csr_R
174400050802     c                   clear                   wCountOpzR2
174500050728     c                   clear                   wSumImpRic
174600050727      *
174700050726     c                   readc     FI86S02
174800050726      *
174900050726do  1c                   DOW       NOT %eof(FIDN86D)
175000050726      *
175100050802     c                   eval      *in37       = *off
175200050728     c                   movea     *zeros        *in(50)
175300050726     c                   z-add     S02nrr        C02rcd
175400050726     c                   clear                   $UpdDRA
175500050726      *
175600050726sel 2c                   SELECT
175700050726      * - Nessuna opzione
175800050802w   2c                   WHEN      S2Copz      = *blanks
175900050726      * - Opzione NON gestibile
176000050919w   1c                   when          S2Copz <> '5'
176100050919     c                             and S2Copz <> '8'
176200050921     c                             and(S2Cipl  = *zeros
176300050921     c                              or H2Cfca  < C_F_ChiusCA
176400050921     c                              or H2Cfic <> 'C')
176500050919     c                   seton                                        502890
176600051013     c                   eval      V1Dmsg      = $Msg(22)
176700050802w   1c                   when          (S2Copz = 'E'
176800050804     c                              or  S2Copz = 'I')
176900050802     c                             and (S2Cdrr > *zeros
177000050802     c                              or  S2Cdvr > *zeros)
177100050726     c                   seton                                        502890
177200051013     c                   eval      V1Dmsg      = $Msg(13)
177300051017      * - 2 = Gestione recupero della singola C.A. legata all'evento
177400051017      *       (impostazione importo recuperato da AIG e data recupero)
177500050914w   2c                   WHEN      S2Copz      = '2'
177600050914     c                   exsr      sr_OpzS2_2
177700050726      * - I = Inclusione della C.A. precedentemente esclusa
177800051017      * - E = Esclusione della C.A. dalla richiesta di recupero
177900050802w   2c                   WHEN          S2Copz  = 'I'
178000050802     c                             or  S2Copz  = 'E'
178100050803     c                   exsr      sr_OpzS2_EI
178200051017      * - R = Selezione C.A. x richiesta recupero ad A.I.G.
178300050802w   2c                   WHEN      S2Copz      = 'R'
178400050726     c                   exsr      sr_OpzS2_R
178500050726      * - 5 = Visualizzazione C.A.
178600050802w   2c                   WHEN      S2Copz      = '5'
178700050726     c                   exsr      sr_OpzS2_5
178800050726      * - 8 = Visualizzazione note C.A.
178900050802w   2c                   WHEN      S2Copz      = '8'
179000050726     c                   exsr      sr_OpzS2_8
179100050726      * - ? = Opzione NON valida
179200050726x   2c                   OTHER
179300050726     c                   seton                                        502890
179400051013     c                   eval      V1Dmsg      = $Msg(11)
179500050726e   2c                   ENDSL
179600050921      *
179700050921      * Memorizzazione nrr del sfl con la 1ª opzione
179800050921      * per riposizionarvici il cursore dopo il tasto "Invio"
179900050921if  1c                   if             S2Copz   <> *blanks
180000050921     c                             and (SavS02csr = *zeros
180100050921     c                              or  SavS02csr < C02rcd)
180200050921     c                   eval      SavS02csr   = C02rcd
180300050921e   1c                   endif
180400050726      *
180500050726      *   Reimpostazione indicatori
180600050728     c     S2Cesc        comp      *blanks                            4141
180700050726      *
180800050726      *   Aggiornamento sfl
180900050726sel 2c                   select
181000050726      *   - Errore
181100050726w   2c                   when      *in28
181200050802     c                   eval      *in37       = *on
181300050726     c                   z-add     C02rcd        C02csr
181400050920      *   - F3 (interruzione ciclo di esecuzione opzioni immesse)
181500050920w   2c                   when      *in90       = *on
181600050920     c                   z-add     C02rcd        C02csr
181700050920     c                   clear                   S2Copz
181800050726      *   - Opz.R = Selezione C.A.
181900050802w   2c                   when      S2Copz      = 'R'
182000050802     c                   eval      *in37       = *on
182100050726      *   - altrimenti...
182200050726x   2c                   other
182300050726     c                   clear                   S2Copz
182400050726e   2c                   endsl
182500050726     c                   UPDATE    FI86S02
182600050920if  2c                   if        *in28  OR  *in90
182700050726     c                   leave
182800050726e   2c                   endif
182900050726      *
183000050726     c                   readc     FI86S02
183100050726      *
183200050726e   1c                   ENDDO
183300050727      *
183400050727      * Controlli finali:
183500050727sel 1c                   select
183600050921      * - già rilevato errore...
183700050921w   1c                   when      *in28  OR  *in90
183800050727      * - immesse opzioni "R" e non premuto F6
183900050805w   1c                   when      wCountOpzR2 > *zeros
184000050805     c                             and *inKF   = *off
184100050921     c                   z-add     SavS02csr_R   C02csr
184200050727     c                   seton                                        502890
184300051013     c                   eval      V1Dmsg      = $Msg(17)
184400050727      * - premuto F6 ma non immesse opzioni "R"
184500050805w   1c                   when      wCountOpzR2 = *zeros
184600050805     c                             and *inKF   = *on
184700050805     c                             and S1Copz  = 'R'
184800050803     c                   seton                                        502890
184900051013     c                   eval      V1Dmsg      = $Msg(18)
185000050805w   1c                   when      wCountOpzR2 = *zeros
185100050805     c                             and *inKF   = *on
185200050921     c                             and S1Copz <> 'R'
185300050727     c                   seton                                        502890
185400051013     c                   eval      V1Dmsg      = $Msg(19)
185500050921      * - riposizionamento cursore sul record contenente la prima opz.
185600050921      *   (se non sono stati rilevati errori)
185700050921w   1c                   when      NOT *in28  and  NOT *in90
185800050921     c                             and SavS02csr  > *zeros
185900050921     c                   z-add     SavS02csr     C02csr
186000050727e   1c                   endsl
186100050726      *
186200050726     c                   ENDSR
186300050801      *
186400050801      *---------------------------------------------------------------*
186500051017      * S2_Opz.2 = Gestione recupero della singola C.A. legata        *
186600051017      *            all'evento (recupero ricevuto da A.I.G.)           *
186700050801      *---------------------------------------------------------------*
186800050914     c     sr_OpzS2_2    BEGSR
186900050801      *
187000050801      * Deve già essere stato richiesto un importo
187100050803if  1c                   if        S2Cdrr      = *zeros
187200050801     c                   seton                                        502890
187300051013     c                   eval      V1Dmsg      = $Msg(14)
187400050801     c                   leavesr
187500050801e   1c                   endif
187600050801      *
187700050801      * Richiamo pgm. FIDN89R
187800050801     c                   reset                   FIDN00ds
187900050801      *
188000050803     c                   eval      I00aac      = H2Caac
188100050803     c                   eval      I00fil      = S2Cfil
188200050803     c                   eval      I00nca      = S2Cnca
188300050803     c                   eval      I00aas      = H2Caas
188400050803     c                   eval      I00lnp      = H2Clnp
188500050803     c                   eval      I00nrs      = H2Cnrs
188600050803     c                   eval      I00nsp      = H2Cnsp
188700050803     c                   eval      I00aae      = DETaae
188800050803     c                   eval      I00nev      = DETnev
188900050803     c                   eval      I00dev      = DETaae*10000 + DETmge
189000050803     c                   eval      I00fca      = H2Cfca
189100050803     c                   eval      I00tae      = H2Ctad
189200050801      *
189300050801     c                   movel(p)  FIDN00ds      KPJBU
189400050801     c                   call      'FIDN89R'
189500050801     c                   parm                    KPJBA
189600050915     c                   movel     KPJBU         FIDN00ds
189700050920      *
189800050920      * F3: si torna alla videata precedente
189900050920if  1c                   if        O00f03      = 'S'
190000050920     c                   exsr      F12S02
190100050920     c                   eval      *in90       = *on
190200050920e   1c                   endif
190300050801      *
190400050801      * Emissione dell'errore rilevato (?se?rilevato )
190500050803     c                   if        O00err     <> *blanks
190600050801     c                   movel     O00msg        V1Dmsg
190700050801     c                   seton                                        28  90
190800050801     c                   leavesr
190900050801     c                   endif
191000050801      *
191100050801     c                   z-add     S2Ciri        SAViri
191200050801      * Aggiornamento dati nel record del sfl
191300050803     c                   eval      DCTaac      = I00aac
191400050803     c                   eval      DCTfil      = I00fil
191500050803     c                   eval      DCTnca      = I00nca
191600050803     c                   exsr      RepFNDRA
191700050801     c                   z-add     DRAiri        S2Ciri
191800050801     c                   reset                   WLBdat
191900050803     c                   eval      G08inv      = DRAdvr
192000050801     c                   call      'XSRDA8'
192100050801     c                   parm                    WLBdat
192200050803if  1c                   if        G08err      = *off
192300050803     c                   eval      S2Cdvr      = G08dat
192400050801x   1c                   else
192500050801     c                   clear                   S2Cdvr
192600050801e   1c                   endif
192700050915      *
192800050915      * Aggiornamento dati di totale in testata (C02) - già calcolati
192900050915if  1c                   if            SAViri  = *zeros
193000050915     c                             and DRAiri <> *zeros
193100051017      * - Numero totale C.A. legate all'Evento già recuperate da A.I.G.
193200050915     c                   add       1             H1cara
193300050915     c                   z-add     H1cara        C2Cagr
193400051017      * - Numero totale C.A. legate all'Evento non ancora recuperate
193500051017      *   da A.I.G. (ma con richiesta di recupero)
193600050919     c                   sub       1             H1carr
193700050919     c                   z-add     H1carr        C2Carr
193800050915e   1c                   endif
193900050801      *
194000050801      * Aggiornamento dati di totale nel piede (P02) - già calcolati
194100051017      * - Totale Importi Recuperati dall'A.I.G. relativi all'Evento
194200050804     c                   eval      wTOTiri     = S1Ciri-SAViri+DRAiri
194300050804     c                   z-add     wTOTiri       S1Ciri
194400050801     c                   if            S1Ciri <> *zeros
194500050801     c                             and H1Cdir  = *blanks
194600050801     c                   movel     P2Cdir        H1Cdir
194700050801     c                   endif
194800050803     c                   eval      *in40       = *off
194900050801     c                   exsr      RieP02
195000050801      *
195100050801     c                   ENDSR
195200050801      *
195300050801      *---------------------------------------------------------------*
195400051017      * S2_Opz.E = Esclusione dell'evento dalla richiesta di recupero *
195500050801      * S2_Opz.I = Inclusione dell'evento precedentemente escluso     *
195600050801      *---------------------------------------------------------------*
195700050803     c     sr_OpzS2_EI   BEGSR
195800050801      *
195900050801      * controlli per opzione
196000050801     c                   select
196100050803     c                   when      S2Cdrr      > *zeros
196200050801     c                   seton                                        502890
196300051013     c                   eval      V1Dmsg      = $Msg(21)
196400050801     c                   leavesr
196500050801e   1c                   endsl
196600050801      *
196700050803     c                   eval      $UpdDRA     = S2Cesc
196800050801      *
196900050801      * Aggiornamento del flag DRAESC nel file FNDRA00F
197000050803     c                   eval      DCTaac      = H2Caac
197100050803     c                   eval      DCTfil      = S2Cfil
197200050803     c                   eval      DCTnca      = S2Cnca
197300050801     c     K03DRA01      chain     FNDRA000
197400050801      *
197500050801if  1c                   if        NOT %found(FNDRA01L)
197600050801     c                             or  DRAatb <> *blanks
197700050801     c                   clear                   FNDRAds
197800050801e   1c                   endif
197900050801      *
198000050801sel 1c                   select
198100050803w   1c                   when      S2Copz      = 'I'
198200050803     c                   eval      $UpdDRA     = *blanks
198300050803w   1c                   when      S2Copz      = 'E'
198400050803     c                   eval      $UpdDRA     = 'E'
198500050801e   1c                   endsl
198600050801      *
198700050801     c                   exsr      UpdFNDRA
198800050801      *
198900050801      * Aggiornamento del flag S2CESC nel subfile
199000050801      * (l'update del rec. verrà eseguita nella subr. chiamante OPZS02)
199100050803     c                   eval      S2Cesc      = $UpdDRA
199200050801      *
199300050801     c                   ENDSR
199400050801      *
199500050801      *---------------------------------------------------------------*
199600051017      * S2_Opz.R = Richiesta recupero A.I.G. per C.A.                 *
199700050801      *---------------------------------------------------------------*
199800050801     c     sr_OpzS2_R    BEGSR
199900050801      *
200000050801      * Verifica se C.A. già esclusa
200100050803if  1c                   if        S2Cesc      = 'E'
200200050801     c                   seton                                        502890
200300051013     c                   eval      V1Dmsg      = $Msg(15)
200400050801e   1c                   endif
200500050802      *
200600050802      * Verifica se C.A. di evento già escluso
200700050803if  1c                   if        S1Cfle      = 'E'
200800050802     c                   seton                                        502890
200900051013     c                   eval      V1Dmsg      = $Msg(16)
201000050802e   1c                   endif
201100050804      *
201200051017      * Verifica se C.A. con recupero già richiesto
201300050805if  1c                   if        S2Cdrr     <> *zeros
201400050804     c                   seton                                        502890
201500051013     c                   eval      V1Dmsg      = $Msg(13)
201600050804e   1c                   endif
201700050801      *
201800050802     c                   add       1             wCountOpzR2
201900050801     c                   add       S2Cipl        wSumImpRic
202000050921      *
202100050921      * Memorizzazione nrr del sfl con la 1ª opzione
202200050921      * per riposizionarvici il cursore dopo il tasto "Invio"
202300050921if  1c                   if             S2Copz      = 'R'
202400050921     c                             and (SavS02csr_R = *zeros
202500050921     c                              or  SavS02csr_R > C02rcd)
202600050921     c                   eval      SavS02csr_R = C02rcd
202700050921e   1c                   endif
202800050801      *
202900050801     c                   ENDSR
203000050801      *
203100050801      *---------------------------------------------------------------*
203200050801      * S2_Opz.5 = Visualizzazione C.A.                               *
203300050801      *---------------------------------------------------------------*
203400050801     c     sr_OpzS2_5    BEGSR
203500050801      *
203600050801     c                   reset                   FIDN00ds
203700050801      *
203800050803     c                   eval      I00aac      = H2Caac
203900050803     c                   eval      I00fil      = S2Cfil
204000050803     c                   eval      I00nca      = S2Cnca
204100050803     c                   eval      I00aas      = H2Caas
204200050803     c                   eval      I00lnp      = H2Clnp
204300050803     c                   eval      I00nrs      = H2Cnrs
204400050803     c                   eval      I00nsp      = H2Cnsp
204500050803     c                   eval      I00aae      = DETaae
204600050803     c                   eval      I00nev      = DETnev
204700050803     c                   eval      I00dev      = DETaae*10000 + DETmge
204800050803     c                   eval      I00fca      = H2Cfca
204900050803     c                   eval      I00tae      = H2Ctad
205000050801      *
205100050801     c                   movel(p)  FIDN00ds      KPJBU
205200050801     c                   call      'FIDN01R'
205300050801     c                   parm                    KPJBA
205400050915     c                   movel     KPJBU         FIDN00ds
205500050920      *
205600050920      * F3: si torna alla videata precedente
205700050920if  1c                   if        O00f03      = 'S'
205800050920     c                   eval      *in90       = *on
205900050920e   1c                   endif
206000050801      *
206100050920      * Emissione dell'errore rilevato (?se?rilevato )
206200050803if  1c                   if        O00err     <> *blanks
206300050801     c                   seton                                        502890
206400050803     c                   eval      V1Dmsg      = O00msg
206500050801e   1c                   endif
206600050801      *
206700050801     c                   ENDSR
206800050801      *
206900050801      *---------------------------------------------------------------*
207000050801      * S2_Opz_8 = Gestione note C.A.                                 *
207100050801      *---------------------------------------------------------------*
207200050801     c     sr_OpzS2_8    BEGSR
207300050801      *
207400050801     c                   reset                   FIDN10ds
207500050801      *
207600050803     c                   eval      I10dta      = H2Caac*10000 + H2Cmgc
207700050801     c                   movel(p)  DS_numca      I10nkt
207800050801     c                   movel     H2Cfca        I10nks
207900050801      *
208000050801     c                   movel(p)  FIDN10ds      KPJBU
208100050801     c                   call      'FIDN10R'
208200050801     c                   parm                    KPJBA
208300050801     c                   movel     KPJBU         FIDN10ds
208400050801      *
208500050803if  1c                   if        O10err     <> *blanks
208600050801     c                   seton                                        502890
208700050803     c                   eval      V1Dmsg      = O10msg
208800050801e   1c                   endif
208900050801      *
209000050801     c                   ENDSR
209100050727      *
209200050727      *---------------------------------------------------------------*
209300050727      * Caricamento dati nel record di "piede" P02                    *
209400050727      *---------------------------------------------------------------*
209500050727     c     RieP02        BEGSR
209600050727      *
209700050727      * Impostazione dati di totale nel piede (P02) - già calcolati
209800050727      * - Totale Importi Liquidati ai clienti relativi all'Evento
209900050727     c                   z-add     S1Cipl        P2Cipl
210000050727     c                   movel     H1Cvpl        P2Cvpl
210100050728if  1c                   if        *in40
210200050915      * - Totale Importi Richiesti all'A.I.G. relativi all'Evento
210300050727     c                   z-add     S1Cir1        P2Ciri
210400050801     c                   movel     H1Cdi1        P2Cdir
210500050727x   1c                   else
210600051017      * - Totale Importi Recuperati dall'A.I.G. relativi all'Evento
210700050801     c                   z-add     S1Ciri        P2Ciri
210800050801     c                   movel     H1Cdir        P2Cdir
210900050727e   1c                   endif
211000050727      *
211100050727     c                   ENDSR
211200050715      *
211300050715      *---------------------------------------------------------------*
211400050715      * Pulizia dei campi di lavoro                                   *
211500050715      *---------------------------------------------------------------*
211600050715     c     ClearWfld     BEGSR
211700050715      *
211800050715     c                   clear                   wTOTipl
211900050715     c                   clear                   wTOTir1
212000050715     c                   clear                   wTOTiri
212100050726     c                   clear                   wTOTcale
212200050916     c                   clear                   wTOTcaold
212300050919     c                   clear                   wTOTnocch
212400050726     c                   clear                   wTOTcanp
212500050919     c                   clear                   wTOTcanrr
212600050919     c                   clear                   wTOTcar1
212700050726     c                   clear                   wTOTcara
212800050715     c                   clear                   W1Cvpl
212900050801     c                   clear                   W1Cdi1
213000050801     c                   clear                   W1Cdir
213100050726     c                   clear                   wDRAdrr
213200050726     c                   clear                   wDRAdvr
213300050715      *
213400050715     c                   ENDSR
213500050714      *
213600050714      *---------------------------------------------------------------*
213700050715      * Calcolo Totali relativi all'Evento                            *
213800050714      *---------------------------------------------------------------*
213900050714     c     ElabFNDCT     BEGSR
214000050714      *
214100051017     c                   exsr      ClearWfld
214200050721      *
214300050714      * Ciclo di lettura archivio "Testate C.A."
214400050804     c                   eval      DCTaae      = DETaae
214500050804     c                   eval      DCTnev      = DETnev
214600050804     c                   eval      DCTfca      = *loval
214700050714     c     K03DCT08      setll     FNDCT000
214800050714      *
214900050715do  1c                   DO        *hival
215000050715      *
215100050715     c     K02DCT08      reade     FNDCT000
215200050715if  2c                   if        %eof(FNDCT08L)
215300050715     c                   leave
215400050715e   2c                   endif
215500050804if  2c                   if        DCTatb     <> *blanks
215600050715     c                   iter
215700050715e   2c                   endif
215800050921      *
215900050922      * - Calcolo Numero Totale C.A. legate all'Evento
216000050921     c                   add       1             wTOTcale
216100050715      *
216200050922      * - Calcolo Numero Totale C.A. relative a spedizioni antecedenti
216300050921      *   il 18/04/2005
216400050916      *   (la singola c.a. viene conteggiata, ma non fa escludere
216500050916      *   tutto l'evento)
216600050804if  2c                   if        DCTaas      < C_AnnoLimite
216700050921     c                   add       1             wTOTcaold
216800050921     c                   iter
216900050715x   2c                   else
217000050715     c                   exsr      RepTITAS
217100050916if  3c                   if        (TASaas * 10000 + TASmgs)
217200050916     c                                         < C_DataLimite
217300050921     c                   add       1             wTOTcaold
217400050921     c                   iter
217500050715e   3c                   endif
217600050715e   2c                   endif
217700050916      *
217800050922      * - Calcolo Numero Totale C.A. Aperte legate all'Evento
217900050916if  2c                   if        DCTfca     <> C_F_ChiusCA
218000050916     c                   add       1             wTOTnocch
218100050922x   2c                   else
218200050922      * - Selezione delle C.A. chiuse con causale di pagamento
218300050922     c                   eval      *in10       = *off
218400050922     c     DCTcch        lookup    $CCHpag                                10
218500050922if  3c                   if        NOT *in10
218600050922     c                   iter
218700050922e   3c                   endif
218800050922e   2c                   endif
218900050922      *
219000050922      * > Reperimento Liquidazione C.A.: · flag imputazione contabile
219100050922      *                                  · importo liquidato
219200050922     c                   exsr      RepFNDCL
219300050922      *
219400050922      * - Selezione delle C.A. chiuse & contabilizzate
219500050922      * - Calcolo Numero C.A. NON Pagate legate all'Evento
219600050922if  2c                   if            DCTfca <> C_F_ChiusCA
219700050922     c                             or  DCLfic <> 'C'
219800050922     c                   add       1             wTOTcanp
219900050922     c                   iter
220000050922e   2c                   endif
220100050922      *
220200050922      * - Calcolo Totale Importi Liquidati ai clienti
220300050922      *   relativi all'Evento (per le sole C.A. contabilizzate)
220400050922     c                   add       DCLipl        wTOTipl
220500050922     c                   eval      W1Cvpl      = DCLvpl
220600050922      *
220700051017      * > Reperimento Recuperi A.I.G.: · Data Richiesta Recupero
220800051017      *                                · Data Valuta Recupero
220900051017      *                                · Importi Recuperati
221000050922     c                   exsr      RepFNDRA
221100050922      *
221200051017      * - Ultima Data Richiesta Recupero ad A.I.G.
221300050922if  2c                   if        wDRAdrr     < DRAdrr
221400050922     c                   eval      wDRAdrr     = DRAdrr
221500050922e   2c                   endif
221600051017      * - Ultima Data Valuta    Recupero ad A.I.G.
221700050922if  2c                   if        wDRAdvr     < DRAdvr
221800050922     c                   eval      wDRAdvr     = DRAdvr
221900050922e   2c                   endif
222000050922      *
222100050922sel 2c                   SELECT
222200051017      * - Calcolo Numero C.A. senza richiesta recupero ad A.I.G.
222300050922w   2c                   WHEN          DRAdrr  = *zeros
222400050922     c                             and DRAdvr  = *zeros
222500050922     c                   eval      wTOTcanrr   = wTOTcanrr + 1
222600051017      * - Calcolo Numero C.A. in attesa di recupero ad parte di A.I.G.
222700050922      * - Calcolo Totale Importi Richiesti all'AIG relativi all'Evento
222800050922w   2c                   WHEN          DRAdrr  > *zeros
222900050922     c                             and DRAdvr  = *zeros
223000050922     c                   eval      wTOTcar1    = wTOTcar1 + 1
223100050922if  3c                   if            DCLipl  > *zeros
223200050922     c                             and DRAesc <> 'E'
223300050922     c                   eval      W1Cdi1      = DCLvpl
223400050922     c                   eval      wTOTir1     = wTOTir1 + DCLipl
223500050922e   3c                   endif
223600051017      * - Calcolo Numero C.A. già recuperate dall'A.I.G. relativi
223700050922      *   all'Evento   &
223800051017      * - Calcolo Totale Importi Recuperati dall'A.I.G. relativi
223900050922      *   all'Evento
224000050922w   2c                   WHEN          DRAdvr  > *zeros
224100050916     c                             and DRAiri  > *zeros
224200050922     c                             and DRAesc <> 'E'
224300050804     c                   eval      W1Cdir      = DRAdir
224400050804     c                   eval      wTOTiri     = wTOTiri + DRAiri
224500050804     c                   eval      wTOTcara    = wTOTcara + 1
224600050922if  3c                   if        DCLipl      > *zeros
224700050922     c                   eval      W1Cdi1      = DCLvpl
224800050922     c                   eval      wTOTir1     = wTOTir1 + DCLipl
224900050922e   3c                   endif
225000050922e   2c                   ENDSL
225100050714      *
225200050714e   1c                   ENDDO
225300050916      *
225400050916      *
225500050916      * Verifiche per aggiornamento del campo DETFTR del file FNDET00F:
225600050916sel 1c                   SELECT
225700050916      *
225800070326      * - nessuna delle C.A. dell'evento risulta recuperabile
225900070326w   1c                   when      wTOTcale    = *zeros
226000070326     c                   eval      $UpdDET     = 'N'
226100050916      *
226200050916      * - tutte le C.A. dell'evento risultano abbinate a spedizioni
226300050916      *   precedenti il 18/04/2005
226400050916w   1c                   when      wTOTcale    = wTOTcaold
226500050916     c                   eval      $UpdDET     = 'X'
226600050916      *
226700050916      * - tutte le C.A. dell'evento risultano chiuse
226800050916      * - tutte le C.A. dell'evento (tranne quelle precedenti il
226900051017      *   18/04/2005) risultano recuperate
227000051017      * - il totale degli importi recuperati da A.I.G. raggiunge/supera
227100050915      *   il totale degli importi liquidati
227200050916w   1c                   when          wTOTnocch  = *zeros
227300050916     c                             and wTOTcara  >= (wTOTcale
227400050916     c                                            -  wTOTcaold)
227500050915     c                             and wTOTiri   >= wTOTipl
227600050914     c                   eval      $UpdDET     = 'P'
227700050916      *
227800050916      * - il totale degli importi liquidati ai clienti e relativi
227900050916      *   all'evento (per le sole C.A. contabilizzate)  E' INFERIORE
228000050916      *   all'importo minimo previsto in tab. "STD"
228100050718      *   (SE elaborate solo C.A. chiuse !!!)
228200060524      *    non è evento fortuito
228300050922w   1c                   when      wTOTnocch   = *zeros
228400050718     c                             and wTOTipl < §STDlae
228500060524     c                             and §tadevfo = *blanks
228600050804     c                   eval      $UpdDET     = 'N'
228700060524      *
228800060524      * - il totale degli importi liquidati ai clienti e relativi
228900060524      *   all'evento (per le sole C.A. contabilizzate)  E' 0
229000060524      *   (SE elaborate solo C.A. chiuse !!!)
229100060524      *   è evento fortuito
229200060524w   1c                   when      wTOTnocch   = *zeros
229300060524     c                             and wTOTipl = 0
229400060524     c                             and §tadevfo <> *blanks
229500060524     c                   eval      $UpdDET     = 'N'
229600050916      *
229700051017      * - il recupero di almeno qualche C.A. risulta già richiesto ad
229800050922      *   A.I.G.
229900050922w   1c                   when      wTOTir1     > *zeros
230000050922     c                   eval      $UpdDET     = 'S'
230100050916      *
230200050916e   1c                   ENDSL
230300050714      *
230400050714     c                   ENDSR
230500050920      *
230600050920      *---------------------------------------------------------------*
230700050920      * Reperimento dati evento (specifico)                           *
230800050920      *---------------------------------------------------------------*
230900050920     c     RepFNDET      BEGSR
231000050920      *
231100051017     c***                z-add     S1Cdev        DETaae
231200051017     c                   z-add     H1Caae        DETaae
231300050920     c                   eval      DETnev      = S1Cnev
231400050920      *
231500050922     c     K02DET01      chain(n)  FNDET000
231600050920      *
231700050920     c                   ENDSR
231800050715      *
231900050715      *---------------------------------------------------------------*
232000050715      * Reperimento Data Spedizione                                   *
232100050715      *---------------------------------------------------------------*
232200050715     c     RepTITAS      BEGSR
232300050715      *
232400050715     c     K04TAS30      chain     TITAS30C
232500050715      *
232600050715     c                   if        NOT %found(TITAS30C)
232700050919     c                   clear                   TASaas
232800050715     c                   clear                   TASmgs
232900050715     c                   endif
233000050715      *
233100050715     c                   ENDSR
233200050715      *
233300050715      *---------------------------------------------------------------*
233400051017      * Reperimento Recuperi A.I.G.                                   *
233500050715      *---------------------------------------------------------------*
233600050715     c     RepFNDRA      BEGSR
233700050715      *
233800050726     c     K03DRA01      chain(n)  FNDRA000
233900050715      *
234000050715if  1c                   if        NOT %found(FNDRA01L)
234100050715     c                             or  DRAatb <> *blanks
234200050715     c                   clear                   FNDRAds
234300050715e   1c                   endif
234400050715      *
234500050715     c                   ENDSR
234600050920      *
234700050920      *---------------------------------------------------------------*
234800050920      * Reperimento Importi Liquidati                                 *
234900050920      *---------------------------------------------------------------*
235000050920     c     RepFNDCL      BEGSR
235100050920      *
235200050920if  1c                   if        DCTfca   = C_F_ChiusCA
235300050920     c     K03DRA01      chain     FNDCL000
235400050920e   1c                   endif
235500050920      *
235600050920if  1c                   if            DCTfca <> C_F_ChiusCA
235700050920     c                             or  NOT %found(FNDCL01L)
235800050920     c                             or  DCLatb <> *blanks
235900050920     c                   clear                   DCLipl
236000050920     c                   clear                   DCLvpl
236100050920     c                   clear                   DCLfic
236200050920e   1c                   endif
236300050920      *
236400050920     c                   ENDSR
236500050714      *
236600050714      *---------------------------------------------------------------*
236700050714      * Reperimento Descrizione Evento                                *
236800050714      *---------------------------------------------------------------*
236900050714     c     RepFNDCS      BEGSR
237000050714      *
237100050805     c                   eval      DCStpd      = 'E'
237200050805     c                   eval      DCSnkt      = DS_KeyEve
237300050805     c                   eval      DCStrc      = 'D'
237400050714      *
237500050714     c     K07DCS01      setll     FNDCS000
237600050714     c     K07DCS01      reade     FNDCS000
237700050714      * esclusione record annullati
237800050714     c                   dow       NOT %eof(FNDCS01L)
237900050714     c                             and DCSatb <> *blanks
238000050714     c     K07DCS01      reade     FNDCS000
238100050714     c                   enddo
238200050720      *
238300050720     c                   if        %eof(FNDCS01L)
238400050720     c                   clear                   DCSnot
238500050720     c                   endif
238600050714      *
238700050714     c                   ENDSR
238800050714      *
238900050714      *---------------------------------------------------------------*
239000050715      * Decodifica / Controllo TIPO ANOMALIA                          *
239100050714      *---------------------------------------------------------------*
239200050714     c     DecodTAD      BEGSR
239300050714      *
239400050714     c                   clear                   dTAD
239500050714     c                   reset                   TIBS02ds
239600050714     c                   movel     KNSIF         T02sif
239700050714     c*** già impostato: movel     'TAD'         T02cod
239800050714     c                   movel(p)  DETtad        T02ke1
239900050714     c                   call      'TIBS02R'
240000050714     c                   parm                    KPJBA
240100050714     c                   parm                    TIBS02ds
240200050805     c                   if        T02err      = *blanks
240300050714     c                   movel     T02uni        dTAD
240400050714     c                   endif
240500050715      *
240600051017if  1***c                   if        §TADevfo    = 'S'
240700051017     ***c                   eval      $UpdDET     = 'F'
240800051017e   1***c                   endif
240900050714      *
241000050714     c                   ENDSR
241100050726      *
241200050726      *---------------------------------------------------------------*
241300050726      * Aggiornamento FNDET00F                                        *
241400050726      *---------------------------------------------------------------*
241500050726     c     UpdFNDET      BEGSR
241600050726      *
241700050921if  1c                   if        DETftr <> $UpdDET
241800050914      *
241900050922     c     K02DET01      chain     FNDET000
242000050922      *
242100050805     c                   eval      DETftr      = $UpdDET
242200050726      *
242300050726     c                   UPDATE    FNDET000
242400050914      *
242500050914e   1c                   endif
242600050726      *
242700050726     c                   ENDSR
242800050726      *
242900050726      *---------------------------------------------------------------*
243000050726      * Aggiornamento FNDRA00F                                        *
243100050726      *---------------------------------------------------------------*
243200050726     c     UpdFNDRA      BEGSR
243300050726      *
243400050914if  0c                   if        $UpdDRA    <> DRAesc
243500050914      *
243600050726     c                   clear                   DRAatb
243700050726if  1c                   if        NOT %found(FNDRA01L)
243800050805     c                   eval      DRAaac      = H2Caac
243900050805     c                   eval      DRAfil      = S2Cfil
244000050805     c                   eval      DRAnca      = S2Cnca
244100050726e   1c                   endif
244200050726      *
244300050805     c                   eval      DRAesc      = $UpdDRA
244400050726      *
244500050726if  1c                   if        %found(FNDRA01L)
244600050726     c                   UPDATE    FNDRA000
244700050726x   1c                   else
244800050726     c                   WRITE     FNDRA000
244900050726e   1c                   endif
245000050914      *
245100050914x   0c                   else
245200050914      *
245300050914     c                   UNLOCK    FNDRA01L
245400050914      *
245500050914e   0c                   endif
245600050726      *
245700050726     c                   ENDSR
245800050713
245900050713      *---------------------------------------------------------------*
246000050713
246100050714**   $CCHpag
246200051025051518
246300050727**   $MSG -------------------------------------------------------------------*
246400050727Numero Evento errato                                                           1
246500050727Immettere l'anno dell'evento                                                   2
246600050727Numero Evento inesistente                                                      3
246700051017?» Libero «?L'anomalia deve essere   EVENTO   NON FORTUITO                                 4
246800050727Evento aperto prima del 18/04/2005                                             5
246900050727Evento con C.A. relative a spedizioni precedenti il 18/04/2005                 6
247000060524Evento fortuito chiuso oppure importi liquidati per l'evento < §STDLAE         7
247100051025?» Libero «?Evento già recuperato                                                          8
247200051013Data apertura evento errata o precedente il 18/04/2005                         9
247300051013Range di date errato                                                          10
247400051013Opzione non valida                                                            11
247500051013Non è selezionabile un evento "escluso"                                       12
247600051017Recupero già richiesto per questa C.A.                                        13
247700051025Impossibile inserire i dati del recupero mancandone ancora la richiesta       14
247800051013Non è selezionabile una C.A. "esclusa"                                        15
247900051013Non è selezionabile una C.A. di un evento "escluso"                           16
248000051025Premere F6 per gestire le selezioni di "Richiesta Recupero"                   17
248100051017Per tutte le C.A. dell'evento è già stato chiesto (o escluso) il recupero     18
248200051017Selezionare le C.A. di cui richiedere recupero (opz. "R") PRIMA di premere F6 19
248300051017Opzione NON valida per un evento per il quale è già stato chiesto il recupero 20
248400051017Opzione NON valida per una C.A. per la quale è già stato chiesto il recupero  21
248500051013Opzione NON valida per una C.A.  aperta  o  chiusa ma non contabilizzata      22
