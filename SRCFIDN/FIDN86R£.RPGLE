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
045700050718sel 2c                   select
045800050803w   2c                   when      DETftr      = *blanks
045900051017w   2***c                   when      DETftr      = 'F'
046000051017     ***c                   seton                                        512890
046100051017     ***c                   eval      V1Dmsg      = $Msg(4)
046200051017     ***c                   leavesr
046300050803w   2c                   when      DETftr      = 'X'
046400050728     c                   seton                                        512890
046500050803     c                   eval      V1Dmsg      = $Msg(6)
046600050718     c                   leavesr
046700050803w   2c                   when      DETftr      = 'N'
046800050728     c                   seton                                        512890
046900050803     c                   eval      V1Dmsg   = %replace
047000050721     c                                        (%trim(%editc(§STDlae:'K'))
047100050721     c                                        :$Msg(7)
047200050721     c                                        :%scan('§STDLAE':$Msg(7)))
047300050718     c                   leavesr
047400051013      * - - avendo selezionato un singolo evento:
047500051013      *     NON controllo i flags di selezione
047600051013e   2c                   endsl
047700050713      * - - evento fortuito
047800050714     c                   exsr      DecodTAD
047900051017if  2***c                   if        $UpdDET     = 'F'
048000051017     ***c*> dulcis infundo: exsr      UpdFNDET
048100051017     ***c                   seton                                        512890
048200051017     ***c                   eval      V1Dmsg      = $Msg(4)
048300051017     ***c                   leavesr
048400051017e   2***c                   endif
048500050714      * - - data apertura evento
048600050714if  2c                   if        (DETaae*10000)+DETmge < C_DataLimite
048700050803     c                   eval      $UpdDET     = 'X'
048800050915     c*> dulcis infundo: exsr      UpdFNDET
048900050728     c                   seton                                        512890
049000050803     c                   eval      V1Dmsg      = $Msg(5)
049100050714     c                   leavesr
049200050714e   2c                   endif
049300050715      * - - date C.A. legate all'evento
049400050715     c                   exsr      ElabFNDCT
049500050715      *     X = C.A. relative a spedizioni precedenti il 18/04/2005
049600050715      *     N = Totale degli importi liquidati per l'evento < §STDLAE
049700050804      *     P = Totale degli importi rimporsatici >= totale importi
049800051013      *           liquidati ai clienti
049900051017      *       & Numero delle C.A. recuperate >= Numero C.A. legate
050000051013      *           all'evento
050100050921if  2c                   IF           $UpdDET <> *blanks
050200050718     c                             and DETftr <> $UpdDET
050300050915     c*> dulcis infundo: exsr      UpdFNDET
050400050715sel 3c                   select
050500050803w   3c                   when      $UpdDET     = 'X'
050600051013     c                   seton                                        512890
050700050803     c                   eval      V1Dmsg      = $Msg(6)
050800051013     c                   leavesr
050900050803w   3c                   when      $UpdDET     = 'N'
051000051013     c                   seton                                        512890
051100050718     c                   eval      V1Dmsg   = %replace
051200050718     c                                        (%trim(%editc(§STDlae:'K'))
051300050718     c                                        :$Msg(7)
051400050718     c                                        :%scan('§STDLAE':$Msg(7)))
051500051013     c                   leavesr
051600051021w   3***c                   when         $UpdDET  = 'P'
051700051021     ***c                             and V1Cse3 <> 'S'
051800051021     ***c                             and W1Cnev  = *zeros
051900051021     ***c                   seton                                        512890
052000051021     ***c                   eval      V1Dmsg      = $Msg(8)
052100051021     ***c                   leavesr
052200050715e   3c                   endsl
052300050921e   2c                   ENDIF
052400050713      *
052500050921e   1c                   ENDIF
052600050713      *
052700051013if  1c                   if        V1Cnev      = *zeros
052800050713      * Range di date apertura evento
052900051013if  2c                   if        V1Cdai      = *zeros
053000051007     c                   eval      V1Cdai      = C_GMAlimite
053100051013e   2c                   endif
053200051013if  3c                   if        V1Cdaf      = *zeros
053300051013     c                   eval      V1Cdaf      = *date
053400051013e   3c                   endif
053500051013      *
053600051013e   1c                   endif
053700050713      * - 1ª data
053800050725      *   (dft = 18/04/2005)
053900050802if  1c                   if        V1Cdai      > *zeros
054000050718     c                   clear                   WLBdat
054100050802     c                   eval      G08dat      = V1Cdai
054200050713     c                   call      'XSRDA8'
054300050713     c                   parm                    WLBdat
054400050802if  2c                   if        G08err      = *on
054500050728     c                   seton                                        532890
054600051013     c                   eval      V1Dmsg      = $Msg(9)
054700050713     c                   leavesr
054800050713e   2c                   endif
054900050802     c                   eval      V1Cdai      = G08dat
055000050802     c                   eval      W1Cdai      = G08inv
055100051013e   1c                   endif
055200051013if  1c                   if        W1Cdai      < C_DataLimite
055300051013     c                             and W1Cnev  = *zeros
055400051013     c                   seton                                        532890
055500051013     c                   eval      V1Dmsg      = $Msg(9)
055600051013     c                   leavesr
055700051013e   1c                   endif
055800050713      * - 2ª data
055900050725      *   (dft = *date)
056000050802if  1c                   if        V1Cdaf      > *zeros
056100050718     c                   clear                   WLBdat
056200050802     c                   eval      G08dat      = V1Cdaf
056300050713     c                   call      'XSRDA8'
056400050713     c                   parm                    WLBdat
056500050802if  2c                   if        G08err      = *on
056600050728     c                   seton                                        542890
056700051013     c                   eval      V1Dmsg      = $Msg(9)
056800050713     c                   leavesr
056900050713e   2c                   endif
057000050802     c                   eval      V1Cdaf      = G08dat
057100050802     c                   eval      W1Cdaf      = G08inv
057200051007x   1c                   else
057300051007     c                   eval      W1Cdaf      = W1Cdai
057400051013e   1c                   endif
057500051013if  1c                   if        W1Cdaf      < C_DataLimite
057600051013     c                             and W1Cnev  = *zeros
057700051013     c                   seton                                        542890
057800051013     c                   eval      V1Dmsg      = $Msg(9)
057900051013     c                   leavesr
058000051013e   1c                   endif
058100050715      * - range
058200050802if  1c                   if        W1Cdai      > W1Cdaf
058300050728     c                   seton                                        532890
058400051013     c                   eval      V1Dmsg      = $Msg(10)
058500050713     c                   leavesr
058600050713e   1c                   endif
058700050713      *
058800050713     c                   ENDSR
058900050713      *
059000050713      *---------------------------------------------------------------*
059100050713      * Ricerche                                                      *
059200050713      *---------------------------------------------------------------*
059300050713     c     srSEARCH      BEGSR
059400050713      *
059500050713sel 1c                   SELECT
059600050713      *
059700050713      * Evento
059800050802w   1c                   WHEN      Wcampo      = 'V1CNEV    '
059900050713     c                   reset                   FIDN07ds
060000050713     c                   call      'FIDN07R'
060100050713     c                   parm                    KPJBA
060200050713     c                   parm                    FIDN07ds
060300050802if  2c                   if        O07err      = 'E'
060400050713     c                   clear                   V1Cnev
060500050728     c                   seton                                        5128
060600050802     c                   eval      V1Dmsg      = O07msg
060700050713x   2c                   else
060800050713     c                   move      O07nev        V1Cnev
060900050802     c                   eval      V1Caae      = O07aae
061000050713e   2c                   endif
061100050713      *
061200050713e   1c                   ENDSL
061300050713      *
061400050802     c                   eval      *in90       = *on
061500050713      *
061600050713     c                   ENDSR
061700050713      *
061800050713      *---------------------------------------------------------------*
061900050713      * Gestione videata C01/S01                                      *
062000050713      *---------------------------------------------------------------*
062100050713     c     GesS01        BEGSR
062200050713      *
062300050713      * Inizializzo la videata
062400050802if  1c                   if        $InzS01     = *on
062500050713     c                   exsr      InzS01
062600050802     c                   eval      $InzS01     = *off
062700050713e   1c                   endif
062800050713      *
062900050713      * Posiziono il cursore
063000050802if  1c                   if        C01csr      > *zeros
063100050713     c                   z-add     C01csr        C01rcd
063200050713e   1c                   endif
063300050713      *
063400050713      * Scrivo la testata e la riga tasti funzionali abilitati
063500051013     c                   eval      *in39       = (C01rcd <= *zeros)
063600050713if  1c                   if        NOT *in90
063700050713     c                   write     FI86T01
063800050713     c                   write     FI86Z02
063900050713e   1c                   endif
064000050713      *
064100050713      * Visualizzo (eventualmente) il msg per la mancanza di dati
064200050802if  1c                   if        C01rcd     <= *zeros
064300050713     c                   write     FI86D02
064400050713e   1c                   endif
064500050713      *
064600050713      * Emetto la videata
064700050713     c                   exfmt     FI86C01
064800050718     c                   z-add     £SFLnrr       C01rcd
064900050713     c                   setoff                                       28  90
065000050713     c                   clear                   V1Dmsg
065100050713      *
065200050715sel 1c                   SELECT
065300050713      * F3=Fine
065400050715w   1c                   WHEN      *inKC
065500050713     c                   exsr      F03D01
065600050727      * F8=Stampa elenco Eventi
065700050727w   1c                   WHEN      *inKH
065800050727     c                   exsr      F08S01
065900050713      * F12=Ritorno
066000050715w   1c                   WHEN      *inKL
066100050713     c                   exsr      F12S01
066200050713      * Roll-UP
066300050802w   1c                   WHEN      £Tasto      = RollUp
066400050714     c                   exsr      RollUpS01
066500051014      * NO rec.
066600051014w   1c                   WHEN      *in39
066700050713      *
066800050713      * Gestione opzioni inserite
066900050715x   1c                   OTHER
067000050713     c                   exsr      OpzS01
067100050715e   1c                   ENDSL
067200050713      *
067300050713     c                   ENDSR
067400050713      *
067500050713      *---------------------------------------------------------------*
067600050713      * Inizializzazione videata S01                                  *
067700050713      *---------------------------------------------------------------*
067800050713     c     InzS01        BEGSR
067900050713      *
068000050713      * Pulizia subfile
068100050713     c                   seton                                        3031
068200050713     c                   write     FI86C01
068300050718     c                   setoff                                         3133
068400050713      *
068500050718     c                   reset                   $EoF
068600050726     c                   clear                   W_SflPag1
068700050713     c                   clear                   C01rcd
068800050713     c                   clear                   C01csr
068900050713     c                   clear                   S01nrr
069000050713     c                   clear                   V1Dmsg
069100050713     c                   setoff                                       28  90
069200050728     c                   movea     *zeros        *in(50)
069300050713      *
069400050713      * Caricamento dei dati da presentare nel sfl
069500050725      *   (NON PIU' a pagine)
069600050802if  1c                   if        W1Cnev      > *zeros
069700050802     c                   eval      DETaae      = V1Caae
069800050802     c                   eval      DETnev      = W1Cnev
069900051013     c     K02DET01      chain(n)  FNDET000
070000050714x   1c                   else
070100050714     c                   z-add     V1Cdai        DETaae
070200050714     c     DETaae        setll     FNDET000
070300050922     c     DETaae        reade(n)  FNDET000
070400050714e   1c                   endif
070500050713      *
070600050715     c                   exsr      RollUpS01
070700050725      *
070800050725      * Avendo caricato TUTTI i dati (NON SOLO quelli della 1ª pagina)
070900050725      *  occorre impostare il posizionamento cursore sul 1° rec del sfl
071000050802if  1c                   if        C01csr      > *zeros
071100050802     c                   eval      C01csr      = 1
071200050802e   1c                   endif
071300050713      *
071400050713     c                   ENDSR
071500050714      *
071600050714      *---------------------------------------------------------------*
071700050714      * Caricamento pagina successiva di dati                         *
071800050714      *---------------------------------------------------------------*
071900050714     c     RollUpS01     BEGSR
072000050714      *
072100050922     c                   eval      S01nrr      = (W_SflPag1 * C_SflPag1)
072200050726     c                   add       1             W_SflPag1
072300050802     c                   eval      *in32       = *off
072400050714      *
072500050718      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
072600050922do  1c                   doW            $EoF   = *off
072700050922     c***                          and  S01nrr < (W_SflPag1 * C_SflPag1)
072800050725      * => carico?non?solo una pagina, bensì?tutti?i rec. !
072900050916      *    (per consentire al pgm. FIDN87R - F8=stampa elenco eventi -
073000050916      *     di semplificare la lista partendo con questo sfl in input
073100050916      *     - piuttosto che rifiltrare i records dei vari archivi, come
073200050916      *     fa già, appunto, questo pgm.).
073300050718      *
073400050714     c                   exsr      CarS01
073500050921      * - Aggiornamento record FNDET00F in esame
073600050921if  2c                   if        $UpdDET    <> *blanks
073700050921     c                   exsr      UpdFNDET
073800050921e   2c                   endif
073900050718      *
074000051013if  2c                   if        W1Cnev      > *zeros
074100051013     c                   eval      $EoF        = *on
074200051013x   2c                   else
074300051013     c     DETaae        reade(n)  FNDET000
074400051013     c                   eval      $EoF        = (%eof(FNDET01L))
074500051013e   2c                   endif
074600050718      *
074700050714e   1c                   enddo
074800050718      *
074900051013      * Visualizzazione del SFL (se ci sono dati)
075000050802     c                   eval      *in30       = (S01nrr = *zeros)
075100050714      *
075200050714      * Attivazione (eventuale) del SFLEND
075300050802     c                   eval      *in33       = $EoF
075400050714      *
075500050718      * Posizionamento cursore al 1° rcd della pagina
075600050803if  1c                   if        S01nrr   > *zeros
075700050726     c     S01nrr        div       C_SflPag1     wPag
075800050718     c                   mvr                     wRig
075900050802     c                   eval      C01rcd      = wPag * C_SflPag1
076000050802     c                   if        wRig        > *zeros
076100050802     c                   eval      C01rcd      = C01rcd + 1
076200050718     c                   else
076300050802     c                   eval      C01rcd      = C01rcd - C_SflPag1 + 1
076400050718     c                   endif
076500050803x   1c                   else
076600050803     c                   clear                   C01rcd
076700050803e   1c                   endif
076800050802     c                   eval      C01csr      = C01rcd
076900050714      *
077000050714     c                   ENDSR
077100050715      *
077200050714      *---------------------------------------------------------------*
077300050714      * Caricamento dati in subfile (singolo record / singolo Evento) *
077400050714      *---------------------------------------------------------------*
077500050714     c     CarS01        BEGSR
077600050715      *
077700050715     c                   clear                   $UpdDET
077800050714      *
077900050715      * Selezione record - se non selezionato singolo Evento - - - - -*
078000050714sel 1c                   select
078100050725      * - evento annullato
078200050802w   1c                   when      DETatb     <> *blanks
078300050725     c                   leavesr
078400050714      * - selezionato singolo Evento (già controllato in subr. CTRD01)
078500050802w   1c                   when      V1Cnev      > *zeros
078600051017      * - flag Recupero A.I.G. non valido
078700050718      *   N = Evento NON fortuito con C.A. pagate, ma
078800050718      *       con totale importi liquidati inferiore al minimo
078900050718      *   X = Evento antecedente il 18/04/2005   o
079000050718      *       con C.A. su spedizioni antecedenti il 18/04/2005
079100051017     *** *   F = Evento fortuito
079200051010w   1c                   when          DETftr  = 'N'
079300050718     c                             or  DETftr  = 'X'
079400051017     ***c                             or  DETftr  = 'F'
079500050718     c                   leavesr
079600051025      * - selez. eventi recuperati totalmente
079700051025      *   P = Pagato (Recuperato totalmente)
079800051025w   1c                   when          V1Cse3 <> 'S'
079900051025     c                             and DETftr  = 'P'
080000050718     c                   leavesr
080100051025      * - selez. eventi esclusi dalle richieste di recupero
080200051025      *   E = Esclusi dalle richieste di recupero
080300051025w   1c                   when          V1Cse4 <> 'S'
080400051025     c                             and DETftr  = 'E'
080500051025     c                   leavesr
080600051025     *** * - selez. eventi fortuiti
080700051025     *** *   F = Evento Fortuito
080800051025w   1***c                   when          V1Cse5 <> 'S'
080900051025     ***c                             and DETftr  = 'F'
081000051025     ***c                   leavesr
081100051025      * - selez. eventi con richiesta di recupero da eseguire
081200051025      *   _ = Richiesta di recupero da eseguire
081300051025      *   S = Richiesta di recupero parzialmente eseguita
081400051025      *       (filtrata più in basso)
081500051025      *       o  In attesa di recupero
081600051025      *      ?MA?se selezionati i soli eventi fortuiti potrebbe essere
081700051025      *         selezionabile...
081800051025w   1c***                when          V1Cse1 <> 'S'
081900051025     c***                          and DETftr  = *blanks
082000051025     c***                leavesr
082100051017      * - selez. eventi in attesa di recupero
082200051017      *   S = In attesa di recupero
082300051017      *       (Richiesta recupero già eseguita - anche parziale)
082400051013      *      ?MA?per alcune C.A. potremmo non aver ancora chiesto il
082500051017      *         recupero...
082600051013w   1c***                when          V1Cse2 <> 'S'
082700051013     c***                          and DETftr  = 'S'
082800051013     c***                leavesr
082900050714      * - data evento antecedente il 18/04/2005
083000050714w   1c                   when      (DETaae*10000)+DETmge < C_DataLimite
083100050802     c                   eval      $UpdDET     = 'X'
083200050921     c                   leavesr
083300050720      * - data Evento non compresa nel range (se immesso)
083400050720w   1c                   when          W1Cdai                > *zeros
083500050720     c                             and (DETaae*10000)+DETmge < W1Cdai
083600050720     c                              or (DETaae*10000)+DETmge > W1Cdaf
083700050714     c                   leavesr
083800050715e   1c                   endsl
083900050714      *
084000050715      * Calcoli e Decodifiche  - - - - - - - - - - - - - - - - - - - -*
084100051017      * - Decodifica/Controllo Tipo Anomalia
084200051017     c                   exsr      DecodTAD
084300050715      * - Calcolo Totale degli Importi Liquidati ai clienti
084400050715      *     relativi all'Evento     &
084500051017      *   Reperimento dati Recupero A.I.G.
084600050714     c                   exsr      ElabFNDCT
084700051011      *
084800051011      * Selezione eventi in base allo status attuale - - - - - - - - -*
084900051011      * (dopo aver eseguito tutti i calcoli relativi alle C.A.)
085000051011     c                   select
085100051011      * - scarto eventi non fortuiti con importo < imp.min. da tab.STD
085200051011w   1c                   when      $UpdDET     = 'N'
085300051011     c                   leavesr
085400051011      * - scarto eventi antecedenti il 18/4/2005
085500051011w   1c                   when      $UpdDET     = 'X'
085600051011     c                   leavesr
085700051017     *** * - scarto eventi fortuiti
085800051017w   1***c                   when      $UpdDET     = 'F'
085900051017     ***c                   leavesr
086000051017      * - scarto eventi NON fortuiti con
086100051017      *   importo liquidato <= limite in tab. STD
086200051017w   1c                   when          §TADevfo = *blanks
086300051017     c                             and wTOTipl <= §STDlae
086400051014     c                   leavesr
086500051013      * - selezionato singolo Evento:
086600051013      *   le ulteriori selezioni NON vengono considerate
086700051013w   1c                   when      W1Cnev      > *zeros
086800051011      * - per gli eventi precedentemente esclusi non eseguo altri test
086900051011w   1c                   when           V1Cse4     = 'S'
087000051011     c                             and  DETftr     = 'E'
087100051025      * - per i soli eventi fortuiti non eseguo altri test
087200051025w   1c                   when           V1Cse1    <> 'S'
087300051025     c                             and  V1Cse2    <> 'S'
087400051025     c                             and  V1Cse5     = 'S'
087500051025     c                             and  §TADevfo   = 'S'
087600051017      * - scarto eventi fortuiti (SE V1Cse5 = 'N')
087700051017w   1c                   when           V1Cse5    <> 'S'
087800051021     c                             and  §TADevfo   = 'S'
087900051017     c                   leavesr
088000051017      * - scarto  eventi con richiesta di recupero da eseguire
088100051011      *   (anche se per una sola C.A. tra tutte quelle dell'evento)
088200051011      *   se NON già esclusi
088300051011      *   (SE V1Cse1 = 'N')
088400051011w   1c                   when           V1Cse1    <> 'S'
088500051011     c                             and ($UpdDET    = *blanks
088600051011     c                              or  $UpdDET    = 'S')
088700051011     c                             and (wTOTcanrr <> *zeros
088800051011     c                              or  wTOTnocch <> *zeros
088900051011     c                              or  wTOTcanp  <> *zeros)
089000051011     c                   leavesr
089100051017      * - scarto  eventi in attesa di recupero (SE V1Cse2 = 'N')
089200051012w   1c                   when           V1Cse2    <> 'S'
089300051012     c                             and  DETftr    <> 'E'
089400051012     c                             and  $UpdDET    = 'S'
089500051012     c                             and  wTOTir1    > wTOTiri
089600051011     c                   leavesr
089700051017      * - scarto  eventi recuperati totalmente (SE V1Cse3 = 'N')
089800051012w   1c                   when           V1Cse3    <> 'S'
089900051012     c                             and  $UpdDET    = 'P'
090000051011     c                   leavesr
090100051011e   1c                   endsl
090200050715      * Reperimento Descrizione Evento da FNDCS00F
090300050715     c                   exsr      RepFNDCS
090400050714      *
090500050715      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
090600050714     c                   exsr      RieS01
090700050714      *
090800050715      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
090900050714     c                   add       1             S01nrr
091000050714     c                   write     FI86S01
091100050714      *
091200050921     c     EndCarS01     ENDSR
091300050714      *
091400050714      *---------------------------------------------------------------*
091500050714      * Caricamento dati nel record del sfl                           *
091600050714      *---------------------------------------------------------------*
091700050714     c     RieS01        BEGSR
091800050714      *
091900050714     c                   clear                   FI86S01
092000050714      *
092100050714      * - 1ª riga campi output
092200050718      *   Numero e Data Evento
092300050802     c                   eval      S1Cnev      = DETnev
092400050718     c                   reset                   WLBdat
092500050802     c                   eval      G08inv      = DETaae*10000 + DETmge
092600050718     c                   call      'XSRDA8'
092700050718     c                   parm                    WLBdat
092800050802if  1c                   if        G08err      = *off
092900051017     c***                eval      S1Cdev      = G08dat
093000051017     c                   move      G08dat        wData_eur
093100051017     c                   move      wData_eur     wData_gma
093200051017     c                   move      wData_gma     S1Cdev
093300050718e   1c                   endif
093400051017      *   Flag Evento Fortuito
093500051017if  1c                   if        §TADevfo    = 'S'
093600051017     c                   eval      S1Cevf      = 'F'
093700051017e   1c                   endif
093800051017      *   Flag Esclusione dell'Evento dalla Richiesta Recupero
093900050802if  1c                   if        DETftr      = 'E'
094000050802     c                   eval      S1Cfle      = DETftr
094100050714e   1c                   endif
094200050919      *
094300050919      * - 1ª riga campi output   &
094400050919      *   Hidden fields
094500050919     c                   exsr      RieRecS01
094600050714      *
094700050714      * - 2ª riga campi output
094800050714      *   Tipo anomalia
094900050802     c                   eval      S1Ctad      = DETtad
095000050920     c                   movel     §TADdesc      S1Dtad
095100050714      *   Descrizione Evento (Note)
095200050914     c                   movel     DCSnot        S1Dnot
095300050714      *   P.O. che ha segnalato l'Evento
095400050802     c                   eval      S1Cfce      = DETfce
095500050714      *
095600050714      * - Indicatori
095700050728     c     S1Cfle        comp      *blanks                            4141
095800051017     c     S1Cevf        comp      *blanks                            4242
095900050714      *
096000050714     c                   ENDSR
096100050718      *
096200050718      *---------------------------------------------------------------*
096300050718      * Gestione tasto funzionale F08 da videata S01                  *
096400050719      * F8-Stampa elenco Eventi                                       *
096500050718      *---------------------------------------------------------------*
096600050718     c     F08S01        BEGSR
096700050718      *
096800050728      * Questo pgm. NON necessita di parametri perchè accede al sfl.
096900050919      * => FALSO !!!
097000050919      * Dovendo stampare i parametri di lancio (D01)  &
097100050919      * non potendo accedere al fmt D01 senza EXFMT (write e read)
097200050919      *   ho impostato i parametri di lancio in una ds interna.
097300050919     c                   movel(p)  FIDN86ds      KPJBU
097400050919      *
097500050719     c                   call      'FIDN87R'
097600050719     c                   parm                    KPJBA
097700050718      *
097800050718     c                   ENDSR
097900050718      *
098000050718      *---------------------------------------------------------------*
098100050718      * Gestione tasto funzionale F12 da videata S01                  *
098200050718      * F12-Ritorno                                                   *
098300050718      *---------------------------------------------------------------*
098400050718     c     F12S01        BEGSR
098500050718      *
098600050718      * Torno alla videata precedente
098700050802     c                   eval      $Video      = '1'
098800050718      *
098900050718     c                   ENDSR
099000050713      *
099100050713      *---------------------------------------------------------------*
099200050713      * Controllo opzioni subfile                                     *
099300050713      *---------------------------------------------------------------*
099400050713     c     OpzS01        BEGSR
099500050713      *
099600050921     c                   clear                   SavS01csr
099700050921      *
099800050713     c                   readc     FI86S01
099900050713      *
100000050713do  1c                   DOW       NOT %eof(FIDN86D)
100100050713      *
100200050802     c                   eval      *in32       = *off
100300050728     c                   movea     *zeros        *in(50)
100400050713     c                   z-add     S01nrr        C01rcd
100500050713      *
100600050715sel 2c                   SELECT
100700050713      * - Nessuna opzione
100800050802w   2c                   WHEN      S1Copz      = *blanks
100900050713      * - G = Gestione delle singole C.A. legate all'evento
101000050802w   2c                   WHEN      S1Copz      = 'G'
101100050726     c                   exsr      sr_OpzS1_G
101200051017      * - E = Esclusione dell'evento dalla richiesta di recupero
101300050720      * - I = Inclusione dell'evento precedentemente escluso
101400050802w   2c                   WHEN          S1Copz  = 'E'
101500050802     c                             or  S1Copz  = 'I'
101600050803     c                   exsr      sr_OpzS1_EI
101700051017      * - R = Richiesta di recupero dell'evento ad A.I.G.
101800050803w   2c                   WHEN      S1Copz      = 'R'
101900050726     c                   exsr      sr_OpzS1_R
102000050715      * - ? = Opzione NON valida
102100050715x   2c                   OTHER
102200050715     c                   seton                                        502890
102300051013     c                   eval      V1Dmsg      = $Msg(11)
102400050715e   2c                   ENDSL
102500050921      *
102600050921      * Memorizzazione nrr del sfl con la 1ª opzione
102700050921      * per riposizionarvici il cursore dopo il tasto "Invio"
102800050921if  1c                   if             S1Copz   <> *blanks
102900050921     c                             and (SavS01csr = *zeros
103000050921     c                              or  SavS01csr < C01rcd)
103100050921     c                   eval      SavS01csr   = C01rcd
103200050921e   1c                   endif
103300050715      *
103400050726      * Reimpostazione indicatori per DSPATR
103500050728     c     S1Cfle        comp      *blanks                            4141
103600051017     c     S1Cevf        comp      *blanks                            4242
103700050726      *
103800050726      * Aggiornamento sfl
103900050920sel 2c                   select
104000050920w   2c                   when      *in28
104100050802     c                   eval      *in32       = *on
104200050718     c                   z-add     C01rcd        C01csr
104300050920w   2c                   when      *in90       = *on
104400050920     c                   z-add     C01rcd        C01csr
104500050920     c                   clear                   S1Copz
104600050920x   2c                   other
104700050715     c                   clear                   S1Copz
104800050920e   2c                   endsl
104900050721     c                   UPDATE    FI86S01
105000050920if  2c                   if        *in28  OR  *in90
105100050721     c                   leave
105200050721e   2c                   endif
105300050715      *
105400050715     c                   readc     FI86S01
105500050715      *
105600050715e   1c                   ENDDO
105700050921      *
105800050921      * Riposiziono il cursore sul record contenente la prima opzione
105900050921      *  (se non sono stati rilevati errori)
106000050921     c                   if        NOT *in28  and  NOT *in90
106100050921     c                             and SavS01csr  > *zeros
106200050921     c                   z-add     SavS01csr     C01csr
106300050921     c                   endif
106400050920      *
106500050920      * Se *in90 = *on (no *in28) è stato premuto F3 da C02/S02:
106600050920      *   riemetto C01/S01, ma anche testata (T01) e piede (Z02)
106700050920      *   (visto che cambiano i tasti funzionali abilitati)
106800050920if  1c                   if        *in90  and  NOT *in28
106900050920     c                   eval      *in90       = *off
107000050920e   1c                   endif
107100050713      *
107200050713     c                   ENDSR
107300050801      *
107400050801      *---------------------------------------------------------------*
107500050801      * S1_Opz.G = Gestione delle singole C.A. legate all'evento      *
107600050801      *---------------------------------------------------------------*
107700050801     c     sr_OpzS1_G    BEGSR
107800050801      *
107900050801     c                   clear                   FI86P02
108000050802     c                   eval      *in40       = *off
108100050801      *
108200051017      * Passaggio alla videata C02/S02 per stampa richiesta recupero
108300050803      *  (non gestita a normale ciclo perchè in uscita da sr GesS02
108400050803      *   verrebbe riemesso S01 - senza terminare l'elaborazione di
108500050803      *   altre opzioni eventualmente in esso già immesse insieme a
108600050803      *   questa)
108700050802     c                   eval      $Video      = '3'
108800050802     c                   eval      $InzS02     = *on
108900050801      *
109000050803do  1c                   doW           $Video  = '3'
109100050801     c                             and $Fine   = *off
109200050801     c                   exsr      GesS02
109300050801e   1c                   enddo
109400050801      *
109500050803      * Aggiornamento dati nel rec. del sfl. S01
109600050920if  1c                   if        NOT *in28  and  NOT *in90
109700050920      * Va inizializzato il sfl se effettuata parzializzazione:
109800051017      * · NO eventi con richieste di recupero da eseguire
109900051017      * · NO eventi in attesa di recupero
110000051017      * · NO eventi recuperati totalmente
110100051010if  2c                   if            V1Cse1 <> 'S'
110200051010     c                             OR  V1Cse2 <> 'S'
110300051010     c                             OR  V1Cse3 <> 'S'
110400051010     c                             and (H1canc > *zeros
110500051010     c                              or  H1cara < (H1cale - H1caol)
110600051010     c                              or  S1Ciri < S1Cir1)
110700050803     c                   eval      $InzS01     = *on
110800050803x   2c                   else
110900050803     c                   exsr      UpdRecS01
111000050803e   2c                   endif
111100050803e   1c                   endif
111200050801      *
111300050801     c                   ENDSR
111400050801      *
111500050801      *---------------------------------------------------------------*
111600051017      * S1_Opz.E = Esclusione dell'evento dalla richiesta di recupero *
111700050803      * S1_Opz.I = Inclusione dell'evento precedentemente escluso     *
111800050801      *---------------------------------------------------------------*
111900050803     c     sr_OpzS1_EI   BEGSR
112000050801      *
112100050801      * controlli per opzione
112200050803if  1c                   if        S1Cdrr      > *zeros
112300050801     c                   seton                                        502890
112400051013     c                   eval      V1Dmsg      = $Msg(20)
112500050801     c                   leavesr
112600050803e   1c                   endif
112700050801      *
112800050802     c                   eval      $UpdDET     = S1Cfle
112900050801      *
113000050801      * Aggiornamento del flag DETFTR nel file FNDET00F
113100050920     c                   exsr      RepFNDET
113200050801if  1c                   IF        %found(FNDET01L)
113300050801      *
113400050801sel 2c                   select
113500050802w   2c                   when      S1Copz      = 'I'
113600050802     c                   eval      $UpdDET     = *blanks
113700050802w   2c                   when      S1Copz      = 'E'
113800050802     c                   eval      $UpdDET     = 'E'
113900050801e   2c                   endsl
114000050801      *
114100050801     c                   exsr      UpdFNDET
114200050801e   1c                   ENDIF
114300050801      *
114400050801      * Aggiornamento del flag S1CFLE nel subfile
114500050801      * (l'update del rec. verrà eseguita nella subr. chiamante OPZS01)
114600050802     c                   eval      S1Cfle      = $UpdDET
114700050803      *
114800050920      * Va inizializzato il sfl se effettuata parzializzazione:
114900051017      * · NO eventi con richieste di recupero da eseguire
115000051017      * · NO eventi in attesa di recupero
115100051017      * · NO eventi recuperati totalmente
115200051017      * · NO eventi esclusi dalle richieste di recupero
115300051010if  1c                   if            V1Cse1 <> 'S'
115400051010     c                             or  V1Cse2 <> 'S'
115500051010     c                             or  V1Cse3 <> 'S'
115600051010     c                             or  V1Cse4 <> 'S'
115700050803     c                   eval      $InzS01     = *on
115800050803e   1c                   endif
115900050801      *
116000050801     c                   ENDSR
116100050801      *
116200050801      *---------------------------------------------------------------*
116300051017      * S1_Opz.R = Richiesta di recupero dell'evento ad A.I.G.        *
116400050801      *---------------------------------------------------------------*
116500050801     c     sr_OpzS1_R    BEGSR
116600050801      *
116700050801      * Verifica se Evento già escluso
116800050802if  1c                   if        S1Cfle      = 'E'
116900050801     c                   seton                                        502890
117000051013     c                   eval      V1Dmsg      = $Msg(12)
117100050802     c                   leavesr
117200050801e   1c                   endif
117300050802      *
117400051017      * Passaggio alla videata C02/S02 per stampa richiesta recupero
117500050803      *  (non gestita a normale ciclo perchè in uscita da sr GesS02
117600050803      *   verrebbe riemesso S01 - senza terminare l'elaborazione di
117700050803      *   altre opzioni eventualmente in esso già immesse insieme a
117800050803      *   questa)
117900050803     c                   eval      $Video      = '3'
118000050803     c                   eval      $InzS02     = *on
118100050803      *
118200050803do  1c                   doW           $Video  = '3'
118300050803     c                             and $Fine   = *off
118400050803     c                   exsr      GesS02
118500050803e   1c                   enddo
118600050803      *
118700050803      * Aggiornamento dati nel rec. del sfl. S01
118800050920if  1c                   if        NOT *in28  and  NOT *in90
118900050920      * Va inizializzato il sfl se effettuata parzializzazione:
119000051017      * · solo eventi con richieste di recupero da eseguire
119100051017      * · solo eventi in attesa di recupero
119200051017      * o se recuperato tutto l'evento (non più visualizzabile)
119300051010if  2c                   if            V1Cse1 <> 'S'
119400051010     c                             OR  V1Cse2 <> 'S'
119500051010     c                             OR  V1Cse3 <> 'S'
119600051010     c                             and (H1canc > *zeros
119700051010     c                              or  H1cara < (H1cale - H1caol)
119800051010     c                              or  S1Ciri < S1Cir1)
119900050803     c                   eval      $InzS01     = *on
120000050803x   2c                   else
120100050803     c                   exsr      UpdRecS01
120200050803e   2c                   endif
120300050803e   1c                   endif
120400050801      *
120500050801     c                   ENDSR
120600050803      *
120700050803      *---------------------------------------------------------------*
120800050803      * Aggiornamento dati nel record del subfile S01                 *
120900050803      *---------------------------------------------------------------*
121000050803     c     UpdRecS01     BEGSR
121100050803      *
121200051017      * > Reperimento Recuperi A.I.G.: · Data Richiesta Recupero
121300051017      *                                · Data Valuta Recupero
121400051017      *                                · Importi Recuperati
121500050920     c                   exsr      RepFNDET
121600050919if  1c                   if        NOT %found(FNDET01L)
121700050919     c                             or  DETatb <> *blanks
121800050919     c                   leavesr
121900050919e   1c                   endif
122000050919      *
122100050803     c                   exsr      ElabFNDCT
122200050921if  1c                   if        $UpdDET    <> *blanks
122300050804     c                   exsr      UpdFNDET
122400050921e   1c                   endif
122500050919     c                   exsr      RieRecS01
122600050919      *
122700050919     c                   ENDSR
122800050919      *
122900050919      *---------------------------------------------------------------*
123000050919      * Aggiornamento dati nel record del subfile S01                 *
123100050919      *---------------------------------------------------------------*
123200050919     c     RieRecS01     BEGSR
123300050919      *
123400051017      *   Data Ultima Richiesta Recupero
123500050919if  1c                   if        wDRAdrr     > *zeros
123600050803     c                   reset                   WLBdat
123700050803     c                   eval      G08inv      = wDRAdrr
123800050803     c                   call      'XSRDA8'
123900050803     c                   parm                    WLBdat
124000050919if  2c                   if        G08err      = *off
124100050803     c                   eval      S1Cdrr      = G08dat
124200050919e   2c                   endif
124300050919e   1c                   endif
124400051017      *   Data Ultimo Recupero Ricevuto
124500050919if  1c                   if        wDRAdvr     > *zeros
124600050803     c                   reset                   WLBdat
124700050803     c                   eval      G08inv      = wDRAdvr
124800050803     c                   call      'XSRDA8'
124900050803     c                   parm                    WLBdat
125000050919if  2c                   if        G08err      = *off
125100050803     c                   eval      S1Cdvr      = G08dat
125200050919e   2c                   endif
125300050919e   1c                   endif
125400050803      *   Totale Importi Liquidati ai clienti relativi all'Evento
125500050803     c                   z-add     wTOTipl       S1Cipl
125600051017      *   Totale Importi Richiesti all'A.I.G. relativi all'Evento
125700050803     c                   z-add     wTOTir1       S1Cir1
125800051017      *   Totale Importi Recuperati dall'A.I.G. relativi all'Evento
125900050803     c                   z-add     wTOTiri       S1Ciri
126000050920      *
126100050803      * - Hidden fields
126200050920      *   Descrizione evento (completa)
126300050920     c                   eval      H1Dtad      = §TADdesc
126400051017      *   Anno evento (4 cifre)
126500051017     c                   eval      H1Caae      = DETaae
126600050803      *   Numero totale C.A. legate all'Evento
126700050915     c                   eval      H1cale      = wTOTcale
126800050919      *   Numero totale C.A. legate all'Evento relative a spedizioni
126900050919      *    precedenti il 18/04/2005
127000050919     c                   eval      H1caol      = wTOTcaold
127100050919      *   Numero totale C.A. ancora aperte legate all'Evento
127200050919     c                   eval      H1canc      = wTOTnocch
127300050803      *   Numero totale C.A. legate all'Evento non ancora pagate
127400050915     c                   eval      H1canp      = wTOTcanp
127500050919      *   Numero totale C.A. legate all'Evento pagate per le quali non
127600051017      *    è ancora stato richiesto il recupero ad A.I.G.
127700050919     c                   eval      H1canr      = wTOTcanrr
127800051017      *   Numero totale C.A. evento già recuperati da A.I.G.
127900050919     c                   eval      H1cara      = wTOTcara
128000051017      *   Numero totale C.A. evento non ancora recuperati da A.I.G.
128100050919     c                   eval      H1carr      = wTOTcar1
128200051017      *   Divisa Importi Liquidati  ai clienti  relativi all'Evento
128300050803     c                   eval      H1Cvpl      = W1Cvpl
128400051017      *   Divisa Importi Richiesti  all'A.I.G.  relativi all'Evento
128500050803     c                   eval      H1Cdi1      = W1Cdi1
128600051017      *   Divisa Importi Recuperati dall'A.I.G. relativi all'Evento
128700050803     c                   eval      H1Cdir      = W1Cdir
128800050803      *
128900050803     c                   ENDSR
129000050726      *
129100050726      *---------------------------------------------------------------*
129200050726      * Gestione videata C02/S02                                      *
129300050726      *---------------------------------------------------------------*
129400050726     c     GesS02        BEGSR
129500050726      *
129600050726      * Inizializzo la videata
129700050803if  1c                   if        $InzS02     = *on
129800050726     c                   exsr      InzS02
129900050803     c                   eval      $InzS02     = *off
130000050727     c                   exsr      RieP02
130100050726e   1c                   endif
130200050726      *
130300050726      * Posiziono il cursore
130400050803if  1c                   if        C02csr      > *zeros
130500050726     c                   z-add     C02csr        C02rcd
130600050726e   1c                   endif
130700050726      *
130800050726      * Scrivo testata, piede e riga dei tasti funzionali abilitati
130900051013     c                   eval      *in39       = (C02rcd <= *zeros)
131000050726if  1c                   if        NOT *in90
131100050726     c                   write     FI86T01
131200050726     c                   write     FI86P02
131300050726     c                   write     FI86Z03
131400050726e   1c                   endif
131500050726      *
131600050726      * Visualizzo (eventualmente) il msg per la mancanza di dati
131700050803if  1c                   if        C02rcd     <= *zeros
131800050726     c                   write     FI86D03
131900050726e   1c                   endif
132000050726      *
132100050726      * Emetto la videata
132200050726     c                   exfmt     FI86C02
132300050726     c                   z-add     £SFLnrr       C02rcd
132400050726     c                   setoff                                       28  90
132500050726     c                   clear                   V1Dmsg
132600050726      *
132700050726sel 1c                   SELECT
132800050726      * F3=Fine
132900050914w   1c                   WHEN      *inKC
133000050920     c                   exsr      F12S02
133100050920     c                   eval      *in90       = *on
133200051017      * F8=Visualizza totale richiesto/recuperato
133300050727w   1c                   WHEN      *inKH
133400050727     c                   exsr      F08S02
133500050727      * F12=Ritorno
133600050727w   1c                   WHEN      *inKL
133700050727     c                   exsr      F12S02
133800050726      * Roll-UP
133900050803w   1c                   WHEN      £Tasto      = RollUp
134000050726     c                   exsr      RollUpS02
134100051014      * NO rec.
134200051014w   1c                   WHEN      *in39
134300050726      *
134400050726      * Gestione opzioni inserite
134500050726x   1c                   OTHER
134600050726     c                   exsr      OpzS02
134700051017      * F6=Stampa richiesta recupero ad A.I.G.
134800050802if  2c                   if            *inKF
134900050803     c                             and *in90   = *off
135000050802     c                   exsr      F06S02
135100050802e   2c                   endif
135200050802e   1c                   ENDSL
135300050726      *
135400050726     c                   ENDSR
135500050726      *
135600050726      *---------------------------------------------------------------*
135700050726      * Inizializzazione videata S02                                  *
135800050726      *---------------------------------------------------------------*
135900050726     c     InzS02        BEGSR
136000050726      *
136100050726      * Pulizia subfile
136200050728     c                   seton                                        3536
136300050726     c                   write     FI86C02
136400050728     c                   setoff                                         3638
136500050726      *
136600050803     c                   clear                   wCountOpzR2
136700050726     c                   reset                   $EoF
136800050726     c                   clear                   W_SflPag2
136900050726     c                   clear                   C02rcd
137000050726     c                   clear                   C02csr
137100050726     c                   clear                   S02nrr
137200050726     c                   clear                   V1Dmsg
137300050726     c                   setoff                                       28  90
137400050728     c                   movea     *zeros        *in(50)
137500050726      *
137600050803     c                   eval      C2Cnev      = S1Cnev
137700051017     c***                z-add     S1Cdev        C2Caae
137800051017     c                   eval      C2Caae      = H1Caae
137900051025     c                   clear                   C2Devf
138000051025if  1c                   if        S1Cevf      = 'F'
138100051025     c                   eval      C2Devf      = 'FORTUITO  '
138200051025e   1c                   endif
138300050920     c                   eval      C2Ctad      = S1Ctad
138400050920     c                   eval      C2Dtad      = H1Dtad
138500050726      *   Numero totale C.A. legate all'Evento non ancora pagate
138600050915     c                   z-add     H1canp        C2Canp
138700050919      *   Numero totale C.A. legate all'Evento pagate per le quali non
138800051017      *    è ancora stato richiesto il recupero ad A.I.G.
138900050919     c                   z-add     H1canr        C2Canr
139000051017      *   Numero totale C.A. legate all'Evento non ancora recuperate
139100050919      *   da A.I.G.:
139200050919     c                   z-add     H1carr        C2Carr
139300051017      *   Numero totale C.A. legate all'Evento già recuperate da A.I.G.
139400050915     c                   z-add     H1cara        C2Cagr
139500050726      *
139600050726      * Caricamento dei dati da presentare nel sfl
139700050726      *   a pagine
139800050920     c                   eval      DCTaae      = C2Caae
139900050920     c                   eval      DCTnev      = C2Cnev
140000050919     c                   eval      DCTfca      = *loval
140100050726     c     K03DCT08      setll     FNDCT000
140200050919     c     K02DCT08      reade     FNDCT000
140300050726      *
140400050726     c                   exsr      RollUpS02
140500050803      *
140600051017      * Se chiesto il recupero per tutto l'evento dal sfl S01:
140700050803      *   emetto "direttamente" il messaggio per la richiesta di F6
140800050803      *   per conferma (senza *in90, che inibisce l'emissione di
140900050803      *   testata e piede del sfl S02)
141000050804if  1c                   if        S1Copz      = 'R'
141100050804if  2c                   if            S02nrr      <= *zeros
141200050804     c                             or  wCountOpzR2 <= *zeros
141300051013     c                   eval      V1Dmsg      = $Msg(18)
141400050803     c                   eval      *in28       = *on
141500050804x   2c                   else
141600050804     c                   exsr      OpzS02
141700050804     c                   eval      *in90       = *off
141800050804e   2c                   endif
141900050803e   1c                   endif
142000050726      *
142100050726     c                   ENDSR
142200050726      *
142300050726      *---------------------------------------------------------------*
142400050726      * Caricamento pagina successiva di dati                         *
142500050726      *---------------------------------------------------------------*
142600050726     c     RollUpS02     BEGSR
142700050726      *
142800050922     c                   eval      S02nrr      = (W_SflPag2 * C_SflPag2)
142900050726     c                   add       1             W_SflPag2
143000050803     c                   eval      *in37       = *off
143100050726      *
143200050726      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
143300050922      * ("per pagina" se richiesta opz. "G" da S01;
143400050922      *  "totale"     se richiesta opz. "R" da S01)
143500050804do  1c                   doW            $EoF   = *off
143600050804     c                             and (S1Copz = 'R'
143700050804     c                              or  S1Copz = 'G'
143800050922     c                              and S02nrr < (W_SflPag2 * C_SflPag2))
143900050726      *
144000050726     c                   exsr      CarS02
144100050726      *
144200050919     c     K02DCT08      reade     FNDCT000
144300050803     c                   eval      $EoF        = (%eof(FNDCT08L))
144400050726      *
144500050726e   1c                   enddo
144600050726      *
144700050726      * Viualizzazione del SFL (se ci sono dati)
144800050803     c                   eval      *in35       = (S02nrr = *zeros)
144900050726      *
145000050726      * Attivazione (eventuale) del SFLEND
145100050803     c                   eval      *in38       = $EoF
145200050726      *
145300050726      * Posizionamento cursore al 1° rcd della pagina
145400050803if  1c                   if        S02nrr      > *zeros
145500050803      *
145600050803      * - Se sono stati caricati TUTTI i dati (NON SOLO quelli della
145700050803      *   1ª pagina) occorre impostare il posizionamento cursore sul
145800050803      *   1° rec del sfl
145900050804if  2c                   if        S1Copz      = 'R'
146000050803     c                   eval      C02rcd      = 1
146100050803x   2c                   else
146200050803      * - Altrimenti il cursore va posizionato sul 1° record della a
146300050803      *   nuova pagina
146400050726     c     S02nrr        div       C_SflPag2     wPag
146500050726     c                   mvr                     wRig
146600050803     c                   eval      C02rcd      = wPag * C_SflPag2
146700050803     c                   if        wRig        > *zeros
146800050803     c                   eval      C02rcd      = C02rcd + 1
146900050726     c                   else
147000050803     c                   eval      C02rcd      = C02rcd - C_SflPag2 + 1
147100050726     c                   endif
147200050803x   2c                   endif
147300050803x   1c                   else
147400050803     c                   clear                   C02rcd
147500050803e   1c                   endif
147600050803     c                   eval      C02csr      = C02rcd
147700050726      *
147800050726     c                   ENDSR
147900050726      *
148000050726      *---------------------------------------------------------------*
148100050726      * Caricamento dati in subfile (singolo record / singola C.A.)   *
148200050726      *---------------------------------------------------------------*
148300050726     c     CarS02        BEGSR
148400050726      *
148500050726     c                   clear                   $UpdDET
148600050726      *
148700051017      *Come in subr. "ElabFNDCT":?
148800050726      *
148900050726      * C.A. annullata
149000050919if  1c                   if        DCTatb  <> *blanks
149100050726     c                   leavesr
149200050726e   1c                   endif
149300050919      * - Selezione per data spedizione (< 18/04/2005)
149400050919      *   (la singola c.a. viene conteggiata, ma non fa escludere
149500050919      *   tutto l'evento)
149600050919if  1c                   if        DCTaas   < C_AnnoLimite
149700050919     c                   leavesr
149800050919x   1c                   else
149900050919     c                   exsr      RepTITAS
150000050919if  2c                   if        (TASaas * 10000 + TASmgs)
150100050919     c                                      < C_DataLimite
150200050919     c                   leavesr
150300050919e   2c                   endif
150400050919e   1c                   endif
150500050726      *
150600050726      * Selezione delle C.A. chiuse con causale di pagamento
150700050920if  1c                   if        DCTfca   = C_F_ChiusCA
150800050919     c                   eval      *in10    = *off
150900050726     c     DCTcch        lookup    $CCHpag                                10
151000050919if  2c                   if        NOT *in10
151100050726     c                   leavesr
151200050919e   2c                   endif
151300050920e   1c                   endif
151400050726      *
151500050726      * Selezione delle C.A. contabilizzate
151600050920      * => NO: le si visualizzano tutte !
151700050920      *    Reperisco comunque l'importo liquidato (zero se C.A. aperta)
151800050920     c                   exsr      RepFNDCL
151900051013if  1c***                if        DCLfic     <> 'C'
152000051013     c***                leavesr
152100051013e   1c***                endif
152200050726      *
152300051025      * Reperimento Recuperi da A.I.G.
152400050726     c                   exsr      RepFNDRA
152500050726      *
152600050726      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
152700050726     c                   exsr      RieS02
152800050726      *
152900050726      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
153000050726     c                   add       1             S02nrr
153100050726     c                   write     FI86S02
153200050726      *
153300050726     c                   ENDSR
153400050726      *
153500050726      *---------------------------------------------------------------*
153600050726      * Caricamento dati nel record del sfl                           *
153700050726      *---------------------------------------------------------------*
153800050726     c     RieS02        BEGSR
153900050726      *
154000050726     c                   clear                   FI86S02
154100050728      *
154200050728      * Campo opzione
154300050802if  1c                   if            S1Copz  = 'R'
154400050802     c                             and DRAdrr  = *zeros
154500050802     c                             and DRAesc  = *blanks
154600050920     c                             and DCTfca  = C_F_ChiusCA
154700050920     c                             and DCLipl  > *zeros
154800050920     c                             and DCLfic  = 'C'
154900050802     c                   eval      *in37       = *on
155000050802     c                   eval      S2Copz      = S1Copz
155100050803     c                   add       1             wCountOpzR2
155200050728e   1c                   endif
155300050726      *
155400050726      * Campi visualizzati
155500050726      * - P.O. C.A.
155600050802     c                   eval      S2Cfil      = DCTfil
155700050726      * - Numero C.A.
155800050802     c                   eval      S2Cnca      = DCTnca
155900050726      * - Data C.A.
156000050726     c                   reset                   WLBdat
156100050802     c                   eval      G08inv      = DCTaac*10000 + DCTmgc
156200050726     c                   call      'XSRDA8'
156300050726     c                   parm                    WLBdat
156400050802if  1c                   if        G08err      = *off
156500050802     c                   eval      S2Cdca      = G08dat
156600050726e   1c                   endif
156700050726      * - Importo liquidato
156800050922      *   (se C.A. chiusa e contabilizzata)
156900050922if  1c                   if        DCLfic      = 'C'
157000050726     c                   z-add     DCLipl        S2Cipl
157100050922e   1c                   endif
157200050726      * - Data richiesta rimbrso
157300050802if  1c                   if        DRAdrr      > *zeros
157400050726     c                   reset                   WLBdat
157500050802     c                   eval      G08inv      = DRAdrr
157600050726     c                   call      'XSRDA8'
157700050726     c                   parm                    WLBdat
157800050802if  2c                   if        G08err      = *off
157900050802     c                   eval      S2Cdrr      = G08dat
158000050726e   2c                   endif
158100050726e   1c                   endif
158200051017      * - Importo recuperato
158300050802     c                   eval      S2Ciri      = DRAiri
158400051017      * - Data valuta recupero
158500050802if  1c                   if        DRAdvr      > *zeros
158600050726     c                   reset                   WLBdat
158700050802     c                   eval      G08inv      = DRAdvr
158800050726     c                   call      'XSRDA8'
158900050726     c                   parm                    WLBdat
159000050802if  2c                   if        G08err      = *off
159100050802     c                   eval      S2Cdvr      = G08dat
159200050726e   2c                   endif
159300050726e   1c                   endif
159400051017      * - Flag Esclusione della C.A. dalla Richiesta Recupero
159500050802if  1c                   if        DRAesc      = 'E'
159600050802     c                   eval      S2Cesc      = DRAesc
159700050726e   1c                   endif
159800050726      *
159900050726      * Campi hidden
160000050726      * - Evento
160100050802     c                   eval      H2Caae      = DCTaae
160200050914     c                   eval      H2Cmge      = DETmge
160300050802     c                   eval      H2Cnev      = DCTnev
160400050726      * - Data C.A.
160500050802     c                   eval      H2Caac      = DCTaac
160600050802     c                   eval      H2Cmgc      = DCTmgc
160700050726      * - Fase C.A.
160800050802     c                   eval      H2Cfca      = DCTfca
160900050921      * - Contabilizzazione
161000050921     c                   eval      H2Cfic      = DCLfic
161100050726      * - Tipo anomalia
161200050802     c                   eval      H2Ctad      = DCTtad
161300050726      * - Spedizione
161400050802     c                   eval      H2Caas      = DCTaas
161500050802     c                   eval      H2Clnp      = DCTlnp
161600050802     c                   eval      H2Cnrs      = DCTnrs
161700050802     c                   eval      H2Cnsp      = DCTnsp
161800050726      *
161900050726      * Indicatori
162000050728     c     S2Cesc        comp      *blanks                            4141
162100050726      *
162200050726     c                   ENDSR
162300050726      *
162400050726      *---------------------------------------------------------------*
162500050726      * Gestione tasto funzionale F06 da videata S02                  *
162600051017      * F6-Stampa richiesta recupero ad A.I.G. per singola C.A. ("R") *
162700050726      *---------------------------------------------------------------*
162800050726     c     F06S02        BEGSR
162900050726      *
163000050726     c                   reset                   FIDN00ds
163100050726      *
163200050802     c                   eval      I00aac      = H2Caac
163300050802     c                   eval      I00fil      = S2Cfil
163400050802     c                   eval      I00nca      = S2Cnca
163500050802     c                   eval      I00fca      = H2Cfca
163600050802     c                   eval      I00tae      = H2Ctad
163700050802     c                   eval      I00aae      = DETaae
163800050802     c                   eval      I00nev      = DETnev
163900050802     c                   eval      I00dev      = DETaae*10000 + DETmge
164000050726      *
164100050726     c                   movel(p)  FIDN00ds      KPJBU
164200050726     c                   call      'FIDN88R'
164300050726     c                   parm                    KPJBA
164400050915     c                   movel     KPJBU         FIDN00ds
164500050726      *
164600050728      * Emissione dell'errore rilevato (?se?rilevato )
164700050802if  1c                   if        O00err     <> *blanks
164800050728     c                   movel     O00msg        V1Dmsg
164900050728     c                   seton                                        28  90
165000050728     c                   leavesr
165100050801e   1c                   endif
165200050914      *
165300050914      * Uscita senza aggiornamento (F3 o F12)
165400050920      * (F3: si torna alla videata precedente)
165500050920if  1c                   if        O00f03      = 'S'
165600050920     c                   exsr      F12S02
165700050920     c                   eval      *in90       = *on
165800050920e   1c                   endif
165900050920if  1c                   if           O00f12   = 'S'
166000050920     c                             or O00f03   = 'S'
166100050914     c                   leavesr
166200050914e   1c                   endif
166300050728      *
166400050728      * Aggiornamento dati di totale nel piede (P02) - già calcolati
166500050920      * - Numero totale C.A. legate all'Evento pagate per le quali non
166600051017      *    è ancora stato richiesto il recupero ad A.I.G.
166700050923     c                   sub       wCountOpzR2   H1canr
166800050923     c                   eval      wTOTcanrr   = H1canr
166900050920     c                   z-add     H1canr        C2Canr
167000051017      * - Numero totale C.A. evento non ancora recuperati da A.I.G.
167100050923     c                   add       wCountOpzR2   H1carr
167200050923     c                   eval      wTOTcar1    = H1carr
167300050920     c                   z-add     H1carr        C2Carr
167400050915      * - Totale Importi Richiesti all'A.I.G. relativi all'Evento
167500050728     c                   add       wSumImpRic    S1Cir1
167600050923     c                   eval      wTOTir1     = S1Cir1
167700050801if  1c                   if            S1Cir1 <> *zeros
167800050801     c                             and H1Cdi1  = *blanks
167900050801     c                   movel     §GEDdba       H1Cdi1
168000050801e   1c                   endif
168100050802     c                   eval      *in40       = *on
168200050728     c                   exsr      RieP02
168300050726      *
168400050726      * Aggiornamento rec. nel sfl.
168500051017      * (se per l'evento ci fossero state recuperate tutte le C.A.,
168600050915      * non lo si dovrebbe più visualizzare anche nel sfl S01)
168700050801if  1c                   if        NOT *in28
168800050802     c                   eval      $InzS02     = *on
168900050801e   1c                   endif
169000050803      *
169100050803      * Aggiornamento campo opzione in sfl S01
169200050804      * (per non rieseguire i controlli relativi all'opz. "R" in S02)
169300050803if  1c                   if        NOT *in28
169400050804     c                             and S1Copz  = 'R'
169500050804     c                   eval      S1Copz      = 'G'
169600050803e   1c                   endif
169700050726      *
169800050726     c                   ENDSR
169900050727      *
170000050727      *---------------------------------------------------------------*
170100050727      * Gestione tasto funzionale F08 da videata S02                  *
170200051017      * F8-Visualizza totale richiesto / recuperato                   *
170300050727      *---------------------------------------------------------------*
170400050727     c     F08S02        BEGSR
170500050727      *
170600050802     c                   if        *in40       = *on
170700050802     c                   eval      *in40       = *off
170800050727     c                   else
170900050802     c                   eval      *in40       = *on
171000050727     c                   endif
171100050727      *
171200050727     c                   exsr      RieP02
171300050727      *
171400050727     c                   ENDSR
171500050726      *
171600050726      *---------------------------------------------------------------*
171700050726      * Gestione tasto funzionale F12 da videata S02                  *
171800050726      * F12-Ritorno                                                   *
171900050726      *---------------------------------------------------------------*
172000050726     c     F12S02        BEGSR
172100050726      *
172200050726      * Torno alla videata precedente
172300050802     c                   eval      $Video      = '2'
172400050726      *
172500050726     c                   ENDSR
172600050726      *
172700050726      *---------------------------------------------------------------*
172800050726      * Controllo opzioni subfile                                     *
172900050726      *---------------------------------------------------------------*
173000050726     c     OpzS02        BEGSR
173100050726      *
173200050921     c                   clear                   SavS02csr
173300050921     c                   clear                   SavS02csr_R
173400050802     c                   clear                   wCountOpzR2
173500050728     c                   clear                   wSumImpRic
173600050727      *
173700050726     c                   readc     FI86S02
173800050726      *
173900050726do  1c                   DOW       NOT %eof(FIDN86D)
174000050726      *
174100050802     c                   eval      *in37       = *off
174200050728     c                   movea     *zeros        *in(50)
174300050726     c                   z-add     S02nrr        C02rcd
174400050726     c                   clear                   $UpdDRA
174500050726      *
174600050726sel 2c                   SELECT
174700050726      * - Nessuna opzione
174800050802w   2c                   WHEN      S2Copz      = *blanks
174900050726      * - Opzione NON gestibile
175000050919w   1c                   when          S2Copz <> '5'
175100050919     c                             and S2Copz <> '8'
175200050921     c                             and(S2Cipl  = *zeros
175300050921     c                              or H2Cfca  < C_F_ChiusCA
175400050921     c                              or H2Cfic <> 'C')
175500050919     c                   seton                                        502890
175600051013     c                   eval      V1Dmsg      = $Msg(22)
175700050802w   1c                   when          (S2Copz = 'E'
175800050804     c                              or  S2Copz = 'I')
175900050802     c                             and (S2Cdrr > *zeros
176000050802     c                              or  S2Cdvr > *zeros)
176100050726     c                   seton                                        502890
176200051013     c                   eval      V1Dmsg      = $Msg(13)
176300051017      * - 2 = Gestione recupero della singola C.A. legata all'evento
176400051017      *       (impostazione importo recuperato da AIG e data recupero)
176500050914w   2c                   WHEN      S2Copz      = '2'
176600050914     c                   exsr      sr_OpzS2_2
176700050726      * - I = Inclusione della C.A. precedentemente esclusa
176800051017      * - E = Esclusione della C.A. dalla richiesta di recupero
176900050802w   2c                   WHEN          S2Copz  = 'I'
177000050802     c                             or  S2Copz  = 'E'
177100050803     c                   exsr      sr_OpzS2_EI
177200051017      * - R = Selezione C.A. x richiesta recupero ad A.I.G.
177300050802w   2c                   WHEN      S2Copz      = 'R'
177400050726     c                   exsr      sr_OpzS2_R
177500050726      * - 5 = Visualizzazione C.A.
177600050802w   2c                   WHEN      S2Copz      = '5'
177700050726     c                   exsr      sr_OpzS2_5
177800050726      * - 8 = Visualizzazione note C.A.
177900050802w   2c                   WHEN      S2Copz      = '8'
178000050726     c                   exsr      sr_OpzS2_8
178100050726      * - ? = Opzione NON valida
178200050726x   2c                   OTHER
178300050726     c                   seton                                        502890
178400051013     c                   eval      V1Dmsg      = $Msg(11)
178500050726e   2c                   ENDSL
178600050921      *
178700050921      * Memorizzazione nrr del sfl con la 1ª opzione
178800050921      * per riposizionarvici il cursore dopo il tasto "Invio"
178900050921if  1c                   if             S2Copz   <> *blanks
179000050921     c                             and (SavS02csr = *zeros
179100050921     c                              or  SavS02csr < C02rcd)
179200050921     c                   eval      SavS02csr   = C02rcd
179300050921e   1c                   endif
179400050726      *
179500050726      *   Reimpostazione indicatori
179600050728     c     S2Cesc        comp      *blanks                            4141
179700050726      *
179800050726      *   Aggiornamento sfl
179900050726sel 2c                   select
180000050726      *   - Errore
180100050726w   2c                   when      *in28
180200050802     c                   eval      *in37       = *on
180300050726     c                   z-add     C02rcd        C02csr
180400050920      *   - F3 (interruzione ciclo di esecuzione opzioni immesse)
180500050920w   2c                   when      *in90       = *on
180600050920     c                   z-add     C02rcd        C02csr
180700050920     c                   clear                   S2Copz
180800050726      *   - Opz.R = Selezione C.A.
180900050802w   2c                   when      S2Copz      = 'R'
181000050802     c                   eval      *in37       = *on
181100050726      *   - altrimenti...
181200050726x   2c                   other
181300050726     c                   clear                   S2Copz
181400050726e   2c                   endsl
181500050726     c                   UPDATE    FI86S02
181600050920if  2c                   if        *in28  OR  *in90
181700050726     c                   leave
181800050726e   2c                   endif
181900050726      *
182000050726     c                   readc     FI86S02
182100050726      *
182200050726e   1c                   ENDDO
182300050727      *
182400050727      * Controlli finali:
182500050727sel 1c                   select
182600050921      * - già rilevato errore...
182700050921w   1c                   when      *in28  OR  *in90
182800050727      * - immesse opzioni "R" e non premuto F6
182900050805w   1c                   when      wCountOpzR2 > *zeros
183000050805     c                             and *inKF   = *off
183100050921     c                   z-add     SavS02csr_R   C02csr
183200050727     c                   seton                                        502890
183300051013     c                   eval      V1Dmsg      = $Msg(17)
183400050727      * - premuto F6 ma non immesse opzioni "R"
183500050805w   1c                   when      wCountOpzR2 = *zeros
183600050805     c                             and *inKF   = *on
183700050805     c                             and S1Copz  = 'R'
183800050803     c                   seton                                        502890
183900051013     c                   eval      V1Dmsg      = $Msg(18)
184000050805w   1c                   when      wCountOpzR2 = *zeros
184100050805     c                             and *inKF   = *on
184200050921     c                             and S1Copz <> 'R'
184300050727     c                   seton                                        502890
184400051013     c                   eval      V1Dmsg      = $Msg(19)
184500050921      * - riposizionamento cursore sul record contenente la prima opz.
184600050921      *   (se non sono stati rilevati errori)
184700050921w   1c                   when      NOT *in28  and  NOT *in90
184800050921     c                             and SavS02csr  > *zeros
184900050921     c                   z-add     SavS02csr     C02csr
185000050727e   1c                   endsl
185100050726      *
185200050726     c                   ENDSR
185300050801      *
185400050801      *---------------------------------------------------------------*
185500051017      * S2_Opz.2 = Gestione recupero della singola C.A. legata        *
185600051017      *            all'evento (recupero ricevuto da A.I.G.)           *
185700050801      *---------------------------------------------------------------*
185800050914     c     sr_OpzS2_2    BEGSR
185900050801      *
186000050801      * Deve già essere stato richiesto un importo
186100050803if  1c                   if        S2Cdrr      = *zeros
186200050801     c                   seton                                        502890
186300051013     c                   eval      V1Dmsg      = $Msg(14)
186400050801     c                   leavesr
186500050801e   1c                   endif
186600050801      *
186700050801      * Richiamo pgm. FIDN89R
186800050801     c                   reset                   FIDN00ds
186900050801      *
187000050803     c                   eval      I00aac      = H2Caac
187100050803     c                   eval      I00fil      = S2Cfil
187200050803     c                   eval      I00nca      = S2Cnca
187300050803     c                   eval      I00aas      = H2Caas
187400050803     c                   eval      I00lnp      = H2Clnp
187500050803     c                   eval      I00nrs      = H2Cnrs
187600050803     c                   eval      I00nsp      = H2Cnsp
187700050803     c                   eval      I00aae      = DETaae
187800050803     c                   eval      I00nev      = DETnev
187900050803     c                   eval      I00dev      = DETaae*10000 + DETmge
188000050803     c                   eval      I00fca      = H2Cfca
188100050803     c                   eval      I00tae      = H2Ctad
188200050801      *
188300050801     c                   movel(p)  FIDN00ds      KPJBU
188400050801     c                   call      'FIDN89R'
188500050801     c                   parm                    KPJBA
188600050915     c                   movel     KPJBU         FIDN00ds
188700050920      *
188800050920      * F3: si torna alla videata precedente
188900050920if  1c                   if        O00f03      = 'S'
189000050920     c                   exsr      F12S02
189100050920     c                   eval      *in90       = *on
189200050920e   1c                   endif
189300050801      *
189400050801      * Emissione dell'errore rilevato (?se?rilevato )
189500050803     c                   if        O00err     <> *blanks
189600050801     c                   movel     O00msg        V1Dmsg
189700050801     c                   seton                                        28  90
189800050801     c                   leavesr
189900050801     c                   endif
190000050801      *
190100050801     c                   z-add     S2Ciri        SAViri
190200050801      * Aggiornamento dati nel record del sfl
190300050803     c                   eval      DCTaac      = I00aac
190400050803     c                   eval      DCTfil      = I00fil
190500050803     c                   eval      DCTnca      = I00nca
190600050803     c                   exsr      RepFNDRA
190700050801     c                   z-add     DRAiri        S2Ciri
190800050801     c                   reset                   WLBdat
190900050803     c                   eval      G08inv      = DRAdvr
191000050801     c                   call      'XSRDA8'
191100050801     c                   parm                    WLBdat
191200050803if  1c                   if        G08err      = *off
191300050803     c                   eval      S2Cdvr      = G08dat
191400050801x   1c                   else
191500050801     c                   clear                   S2Cdvr
191600050801e   1c                   endif
191700050915      *
191800050915      * Aggiornamento dati di totale in testata (C02) - già calcolati
191900050915if  1c                   if            SAViri  = *zeros
192000050915     c                             and DRAiri <> *zeros
192100051017      * - Numero totale C.A. legate all'Evento già recuperate da A.I.G.
192200050915     c                   add       1             H1cara
192300050915     c                   z-add     H1cara        C2Cagr
192400051017      * - Numero totale C.A. legate all'Evento non ancora recuperate
192500051017      *   da A.I.G. (ma con richiesta di recupero)
192600050919     c                   sub       1             H1carr
192700050919     c                   z-add     H1carr        C2Carr
192800050915e   1c                   endif
192900050801      *
193000050801      * Aggiornamento dati di totale nel piede (P02) - già calcolati
193100051017      * - Totale Importi Recuperati dall'A.I.G. relativi all'Evento
193200050804     c                   eval      wTOTiri     = S1Ciri-SAViri+DRAiri
193300050804     c                   z-add     wTOTiri       S1Ciri
193400050801     c                   if            S1Ciri <> *zeros
193500050801     c                             and H1Cdir  = *blanks
193600050801     c                   movel     P2Cdir        H1Cdir
193700050801     c                   endif
193800050803     c                   eval      *in40       = *off
193900050801     c                   exsr      RieP02
194000050801      *
194100050801     c                   ENDSR
194200050801      *
194300050801      *---------------------------------------------------------------*
194400051017      * S2_Opz.E = Esclusione dell'evento dalla richiesta di recupero *
194500050801      * S2_Opz.I = Inclusione dell'evento precedentemente escluso     *
194600050801      *---------------------------------------------------------------*
194700050803     c     sr_OpzS2_EI   BEGSR
194800050801      *
194900050801      * controlli per opzione
195000050801     c                   select
195100050803     c                   when      S2Cdrr      > *zeros
195200050801     c                   seton                                        502890
195300051013     c                   eval      V1Dmsg      = $Msg(21)
195400050801     c                   leavesr
195500050801e   1c                   endsl
195600050801      *
195700050803     c                   eval      $UpdDRA     = S2Cesc
195800050801      *
195900050801      * Aggiornamento del flag DRAESC nel file FNDRA00F
196000050803     c                   eval      DCTaac      = H2Caac
196100050803     c                   eval      DCTfil      = S2Cfil
196200050803     c                   eval      DCTnca      = S2Cnca
196300050801     c     K03DRA01      chain     FNDRA000
196400050801      *
196500050801if  1c                   if        NOT %found(FNDRA01L)
196600050801     c                             or  DRAatb <> *blanks
196700050801     c                   clear                   FNDRAds
196800050801e   1c                   endif
196900050801      *
197000050801sel 1c                   select
197100050803w   1c                   when      S2Copz      = 'I'
197200050803     c                   eval      $UpdDRA     = *blanks
197300050803w   1c                   when      S2Copz      = 'E'
197400050803     c                   eval      $UpdDRA     = 'E'
197500050801e   1c                   endsl
197600050801      *
197700050801     c                   exsr      UpdFNDRA
197800050801      *
197900050801      * Aggiornamento del flag S2CESC nel subfile
198000050801      * (l'update del rec. verrà eseguita nella subr. chiamante OPZS02)
198100050803     c                   eval      S2Cesc      = $UpdDRA
198200050801      *
198300050801     c                   ENDSR
198400050801      *
198500050801      *---------------------------------------------------------------*
198600051017      * S2_Opz.R = Richiesta recupero A.I.G. per C.A.                 *
198700050801      *---------------------------------------------------------------*
198800050801     c     sr_OpzS2_R    BEGSR
198900050801      *
199000050801      * Verifica se C.A. già esclusa
199100050803if  1c                   if        S2Cesc      = 'E'
199200050801     c                   seton                                        502890
199300051013     c                   eval      V1Dmsg      = $Msg(15)
199400050801e   1c                   endif
199500050802      *
199600050802      * Verifica se C.A. di evento già escluso
199700050803if  1c                   if        S1Cfle      = 'E'
199800050802     c                   seton                                        502890
199900051013     c                   eval      V1Dmsg      = $Msg(16)
200000050802e   1c                   endif
200100050804      *
200200051017      * Verifica se C.A. con recupero già richiesto
200300050805if  1c                   if        S2Cdrr     <> *zeros
200400050804     c                   seton                                        502890
200500051013     c                   eval      V1Dmsg      = $Msg(13)
200600050804e   1c                   endif
200700050801      *
200800050802     c                   add       1             wCountOpzR2
200900050801     c                   add       S2Cipl        wSumImpRic
201000050921      *
201100050921      * Memorizzazione nrr del sfl con la 1ª opzione
201200050921      * per riposizionarvici il cursore dopo il tasto "Invio"
201300050921if  1c                   if             S2Copz      = 'R'
201400050921     c                             and (SavS02csr_R = *zeros
201500050921     c                              or  SavS02csr_R > C02rcd)
201600050921     c                   eval      SavS02csr_R = C02rcd
201700050921e   1c                   endif
201800050801      *
201900050801     c                   ENDSR
202000050801      *
202100050801      *---------------------------------------------------------------*
202200050801      * S2_Opz.5 = Visualizzazione C.A.                               *
202300050801      *---------------------------------------------------------------*
202400050801     c     sr_OpzS2_5    BEGSR
202500050801      *
202600050801     c                   reset                   FIDN00ds
202700050801      *
202800050803     c                   eval      I00aac      = H2Caac
202900050803     c                   eval      I00fil      = S2Cfil
203000050803     c                   eval      I00nca      = S2Cnca
203100050803     c                   eval      I00aas      = H2Caas
203200050803     c                   eval      I00lnp      = H2Clnp
203300050803     c                   eval      I00nrs      = H2Cnrs
203400050803     c                   eval      I00nsp      = H2Cnsp
203500050803     c                   eval      I00aae      = DETaae
203600050803     c                   eval      I00nev      = DETnev
203700050803     c                   eval      I00dev      = DETaae*10000 + DETmge
203800050803     c                   eval      I00fca      = H2Cfca
203900050803     c                   eval      I00tae      = H2Ctad
204000050801      *
204100050801     c                   movel(p)  FIDN00ds      KPJBU
204200050801     c                   call      'FIDN01R'
204300050801     c                   parm                    KPJBA
204400050915     c                   movel     KPJBU         FIDN00ds
204500050920      *
204600050920      * F3: si torna alla videata precedente
204700050920if  1c                   if        O00f03      = 'S'
204800050920     c                   eval      *in90       = *on
204900050920e   1c                   endif
205000050801      *
205100050920      * Emissione dell'errore rilevato (?se?rilevato )
205200050803if  1c                   if        O00err     <> *blanks
205300050801     c                   seton                                        502890
205400050803     c                   eval      V1Dmsg      = O00msg
205500050801e   1c                   endif
205600050801      *
205700050801     c                   ENDSR
205800050801      *
205900050801      *---------------------------------------------------------------*
206000050801      * S2_Opz_8 = Gestione note C.A.                                 *
206100050801      *---------------------------------------------------------------*
206200050801     c     sr_OpzS2_8    BEGSR
206300050801      *
206400050801     c                   reset                   FIDN10ds
206500050801      *
206600050803     c                   eval      I10dta      = H2Caac*10000 + H2Cmgc
206700050801     c                   movel(p)  DS_numca      I10nkt
206800050801     c                   movel     H2Cfca        I10nks
206900050801      *
207000050801     c                   movel(p)  FIDN10ds      KPJBU
207100050801     c                   call      'FIDN10R'
207200050801     c                   parm                    KPJBA
207300050801     c                   movel     KPJBU         FIDN10ds
207400050801      *
207500050803if  1c                   if        O10err     <> *blanks
207600050801     c                   seton                                        502890
207700050803     c                   eval      V1Dmsg      = O10msg
207800050801e   1c                   endif
207900050801      *
208000050801     c                   ENDSR
208100050727      *
208200050727      *---------------------------------------------------------------*
208300050727      * Caricamento dati nel record di "piede" P02                    *
208400050727      *---------------------------------------------------------------*
208500050727     c     RieP02        BEGSR
208600050727      *
208700050727      * Impostazione dati di totale nel piede (P02) - già calcolati
208800050727      * - Totale Importi Liquidati ai clienti relativi all'Evento
208900050727     c                   z-add     S1Cipl        P2Cipl
209000050727     c                   movel     H1Cvpl        P2Cvpl
209100050728if  1c                   if        *in40
209200050915      * - Totale Importi Richiesti all'A.I.G. relativi all'Evento
209300050727     c                   z-add     S1Cir1        P2Ciri
209400050801     c                   movel     H1Cdi1        P2Cdir
209500050727x   1c                   else
209600051017      * - Totale Importi Recuperati dall'A.I.G. relativi all'Evento
209700050801     c                   z-add     S1Ciri        P2Ciri
209800050801     c                   movel     H1Cdir        P2Cdir
209900050727e   1c                   endif
210000050727      *
210100050727     c                   ENDSR
210200050715      *
210300050715      *---------------------------------------------------------------*
210400050715      * Pulizia dei campi di lavoro                                   *
210500050715      *---------------------------------------------------------------*
210600050715     c     ClearWfld     BEGSR
210700050715      *
210800050715     c                   clear                   wTOTipl
210900050715     c                   clear                   wTOTir1
211000050715     c                   clear                   wTOTiri
211100050726     c                   clear                   wTOTcale
211200050916     c                   clear                   wTOTcaold
211300050919     c                   clear                   wTOTnocch
211400050726     c                   clear                   wTOTcanp
211500050919     c                   clear                   wTOTcanrr
211600050919     c                   clear                   wTOTcar1
211700050726     c                   clear                   wTOTcara
211800050715     c                   clear                   W1Cvpl
211900050801     c                   clear                   W1Cdi1
212000050801     c                   clear                   W1Cdir
212100050726     c                   clear                   wDRAdrr
212200050726     c                   clear                   wDRAdvr
212300050715      *
212400050715     c                   ENDSR
212500050714      *
212600050714      *---------------------------------------------------------------*
212700050715      * Calcolo Totali relativi all'Evento                            *
212800050714      *---------------------------------------------------------------*
212900050714     c     ElabFNDCT     BEGSR
213000050714      *
213100051017     c                   exsr      ClearWfld
213200050721      *
213300050714      * Ciclo di lettura archivio "Testate C.A."
213400050804     c                   eval      DCTaae      = DETaae
213500050804     c                   eval      DCTnev      = DETnev
213600050804     c                   eval      DCTfca      = *loval
213700050714     c     K03DCT08      setll     FNDCT000
213800050714      *
213900050715do  1c                   DO        *hival
214000050715      *
214100050715     c     K02DCT08      reade     FNDCT000
214200050715if  2c                   if        %eof(FNDCT08L)
214300050715     c                   leave
214400050715e   2c                   endif
214500050804if  2c                   if        DCTatb     <> *blanks
214600050715     c                   iter
214700050715e   2c                   endif
214800050921      *
214900050922      * - Calcolo Numero Totale C.A. legate all'Evento
215000050921     c                   add       1             wTOTcale
215100050715      *
215200050922      * - Calcolo Numero Totale C.A. relative a spedizioni antecedenti
215300050921      *   il 18/04/2005
215400050916      *   (la singola c.a. viene conteggiata, ma non fa escludere
215500050916      *   tutto l'evento)
215600050804if  2c                   if        DCTaas      < C_AnnoLimite
215700050921     c                   add       1             wTOTcaold
215800050921     c                   iter
215900050715x   2c                   else
216000050715     c                   exsr      RepTITAS
216100050916if  3c                   if        (TASaas * 10000 + TASmgs)
216200050916     c                                         < C_DataLimite
216300050921     c                   add       1             wTOTcaold
216400050921     c                   iter
216500050715e   3c                   endif
216600050715e   2c                   endif
216700050916      *
216800050922      * - Calcolo Numero Totale C.A. Aperte legate all'Evento
216900050916if  2c                   if        DCTfca     <> C_F_ChiusCA
217000050916     c                   add       1             wTOTnocch
217100050922x   2c                   else
217200050922      * - Selezione delle C.A. chiuse con causale di pagamento
217300050922     c                   eval      *in10       = *off
217400050922     c     DCTcch        lookup    $CCHpag                                10
217500050922if  3c                   if        NOT *in10
217600050922     c                   iter
217700050922e   3c                   endif
217800050922e   2c                   endif
217900050922      *
218000050922      * > Reperimento Liquidazione C.A.: · flag imputazione contabile
218100050922      *                                  · importo liquidato
218200050922     c                   exsr      RepFNDCL
218300050922      *
218400050922      * - Selezione delle C.A. chiuse & contabilizzate
218500050922      * - Calcolo Numero C.A. NON Pagate legate all'Evento
218600050922if  2c                   if            DCTfca <> C_F_ChiusCA
218700050922     c                             or  DCLfic <> 'C'
218800050922     c                   add       1             wTOTcanp
218900050922     c                   iter
219000050922e   2c                   endif
219100050922      *
219200050922      * - Calcolo Totale Importi Liquidati ai clienti
219300050922      *   relativi all'Evento (per le sole C.A. contabilizzate)
219400050922     c                   add       DCLipl        wTOTipl
219500050922     c                   eval      W1Cvpl      = DCLvpl
219600050922      *
219700051017      * > Reperimento Recuperi A.I.G.: · Data Richiesta Recupero
219800051017      *                                · Data Valuta Recupero
219900051017      *                                · Importi Recuperati
220000050922     c                   exsr      RepFNDRA
220100050922      *
220200051017      * - Ultima Data Richiesta Recupero ad A.I.G.
220300050922if  2c                   if        wDRAdrr     < DRAdrr
220400050922     c                   eval      wDRAdrr     = DRAdrr
220500050922e   2c                   endif
220600051017      * - Ultima Data Valuta    Recupero ad A.I.G.
220700050922if  2c                   if        wDRAdvr     < DRAdvr
220800050922     c                   eval      wDRAdvr     = DRAdvr
220900050922e   2c                   endif
221000050922      *
221100050922sel 2c                   SELECT
221200051017      * - Calcolo Numero C.A. senza richiesta recupero ad A.I.G.
221300050922w   2c                   WHEN          DRAdrr  = *zeros
221400050922     c                             and DRAdvr  = *zeros
221500050922     c                   eval      wTOTcanrr   = wTOTcanrr + 1
221600051017      * - Calcolo Numero C.A. in attesa di recupero ad parte di A.I.G.
221700050922      * - Calcolo Totale Importi Richiesti all'AIG relativi all'Evento
221800050922w   2c                   WHEN          DRAdrr  > *zeros
221900050922     c                             and DRAdvr  = *zeros
222000050922     c                   eval      wTOTcar1    = wTOTcar1 + 1
222100050922if  3c                   if            DCLipl  > *zeros
222200050922     c                             and DRAesc <> 'E'
222300050922     c                   eval      W1Cdi1      = DCLvpl
222400050922     c                   eval      wTOTir1     = wTOTir1 + DCLipl
222500050922e   3c                   endif
222600051017      * - Calcolo Numero C.A. già recuperate dall'A.I.G. relativi
222700050922      *   all'Evento   &
222800051017      * - Calcolo Totale Importi Recuperati dall'A.I.G. relativi
222900050922      *   all'Evento
223000050922w   2c                   WHEN          DRAdvr  > *zeros
223100050916     c                             and DRAiri  > *zeros
223200050922     c                             and DRAesc <> 'E'
223300050804     c                   eval      W1Cdir      = DRAdir
223400050804     c                   eval      wTOTiri     = wTOTiri + DRAiri
223500050804     c                   eval      wTOTcara    = wTOTcara + 1
223600050922if  3c                   if        DCLipl      > *zeros
223700050922     c                   eval      W1Cdi1      = DCLvpl
223800050922     c                   eval      wTOTir1     = wTOTir1 + DCLipl
223900050922e   3c                   endif
224000050922e   2c                   ENDSL
224100050714      *
224200050714e   1c                   ENDDO
224300050916      *
224400050916      *
224500050916      * Verifiche per aggiornamento del campo DETFTR del file FNDET00F:
224600050916sel 1c                   SELECT
224700050916      *
224800051017      * - nessuna delle C.A. dell'evento risulta recuperabile
224900050916w   1c                   when      wTOTcale    = *zeros
225000050804     c                   eval      $UpdDET     = 'N'
225100050916      *
225200050916      * - tutte le C.A. dell'evento risultano abbinate a spedizioni
225300050916      *   precedenti il 18/04/2005
225400050916w   1c                   when      wTOTcale    = wTOTcaold
225500050916     c                   eval      $UpdDET     = 'X'
225600050916      *
225700050916      * - tutte le C.A. dell'evento risultano chiuse
225800050916      * - tutte le C.A. dell'evento (tranne quelle precedenti il
225900051017      *   18/04/2005) risultano recuperate
226000051017      * - il totale degli importi recuperati da A.I.G. raggiunge/supera
226100050915      *   il totale degli importi liquidati
226200050916w   1c                   when          wTOTnocch  = *zeros
226300050916     c                             and wTOTcara  >= (wTOTcale
226400050916     c                                            -  wTOTcaold)
226500050915     c                             and wTOTiri   >= wTOTipl
226600050914     c                   eval      $UpdDET     = 'P'
226700050916      *
226800050916      * - il totale degli importi liquidati ai clienti e relativi
226900050916      *   all'evento (per le sole C.A. contabilizzate)  E' INFERIORE
227000050916      *   all'importo minimo previsto in tab. "STD"
227100050718      *   (SE elaborate solo C.A. chiuse !!!)
227200060524      *    non è evento fortuito
227300050922w   1c                   when      wTOTnocch   = *zeros
227400050718     c                             and wTOTipl < §STDlae
227500060524     c                             and §tadevfo = *blanks
227600050804     c                   eval      $UpdDET     = 'N'
227700060524      *
227800060524      * - il totale degli importi liquidati ai clienti e relativi
227900060524      *   all'evento (per le sole C.A. contabilizzate)  E' 0
228000060524      *   (SE elaborate solo C.A. chiuse !!!)
228100060524      *   è evento fortuito
228200060524w   1c                   when      wTOTnocch   = *zeros
228300060524     c                             and wTOTipl = 0
228400060524     c                             and §tadevfo <> *blanks
228500060524     c                   eval      $UpdDET     = 'N'
228600050916      *
228700051017      * - il recupero di almeno qualche C.A. risulta già richiesto ad
228800050922      *   A.I.G.
228900050922w   1c                   when      wTOTir1     > *zeros
229000050922     c                   eval      $UpdDET     = 'S'
229100050916      *
229200050916e   1c                   ENDSL
229300050714      *
229400050714     c                   ENDSR
229500050920      *
229600050920      *---------------------------------------------------------------*
229700050920      * Reperimento dati evento (specifico)                           *
229800050920      *---------------------------------------------------------------*
229900050920     c     RepFNDET      BEGSR
230000050920      *
230100051017     c***                z-add     S1Cdev        DETaae
230200051017     c                   z-add     H1Caae        DETaae
230300050920     c                   eval      DETnev      = S1Cnev
230400050920      *
230500050922     c     K02DET01      chain(n)  FNDET000
230600050920      *
230700050920     c                   ENDSR
230800050715      *
230900050715      *---------------------------------------------------------------*
231000050715      * Reperimento Data Spedizione                                   *
231100050715      *---------------------------------------------------------------*
231200050715     c     RepTITAS      BEGSR
231300050715      *
231400050715     c     K04TAS30      chain     TITAS30C
231500050715      *
231600050715     c                   if        NOT %found(TITAS30C)
231700050919     c                   clear                   TASaas
231800050715     c                   clear                   TASmgs
231900050715     c                   endif
232000050715      *
232100050715     c                   ENDSR
232200050715      *
232300050715      *---------------------------------------------------------------*
232400051017      * Reperimento Recuperi A.I.G.                                   *
232500050715      *---------------------------------------------------------------*
232600050715     c     RepFNDRA      BEGSR
232700050715      *
232800050726     c     K03DRA01      chain(n)  FNDRA000
232900050715      *
233000050715if  1c                   if        NOT %found(FNDRA01L)
233100050715     c                             or  DRAatb <> *blanks
233200050715     c                   clear                   FNDRAds
233300050715e   1c                   endif
233400050715      *
233500050715     c                   ENDSR
233600050920      *
233700050920      *---------------------------------------------------------------*
233800050920      * Reperimento Importi Liquidati                                 *
233900050920      *---------------------------------------------------------------*
234000050920     c     RepFNDCL      BEGSR
234100050920      *
234200050920if  1c                   if        DCTfca   = C_F_ChiusCA
234300050920     c     K03DRA01      chain     FNDCL000
234400050920e   1c                   endif
234500050920      *
234600050920if  1c                   if            DCTfca <> C_F_ChiusCA
234700050920     c                             or  NOT %found(FNDCL01L)
234800050920     c                             or  DCLatb <> *blanks
234900050920     c                   clear                   DCLipl
235000050920     c                   clear                   DCLvpl
235100050920     c                   clear                   DCLfic
235200050920e   1c                   endif
235300050920      *
235400050920     c                   ENDSR
235500050714      *
235600050714      *---------------------------------------------------------------*
235700050714      * Reperimento Descrizione Evento                                *
235800050714      *---------------------------------------------------------------*
235900050714     c     RepFNDCS      BEGSR
236000050714      *
236100050805     c                   eval      DCStpd      = 'E'
236200050805     c                   eval      DCSnkt      = DS_KeyEve
236300050805     c                   eval      DCStrc      = 'D'
236400050714      *
236500050714     c     K07DCS01      setll     FNDCS000
236600050714     c     K07DCS01      reade     FNDCS000
236700050714      * esclusione record annullati
236800050714     c                   dow       NOT %eof(FNDCS01L)
236900050714     c                             and DCSatb <> *blanks
237000050714     c     K07DCS01      reade     FNDCS000
237100050714     c                   enddo
237200050720      *
237300050720     c                   if        %eof(FNDCS01L)
237400050720     c                   clear                   DCSnot
237500050720     c                   endif
237600050714      *
237700050714     c                   ENDSR
237800050714      *
237900050714      *---------------------------------------------------------------*
238000050715      * Decodifica / Controllo TIPO ANOMALIA                          *
238100050714      *---------------------------------------------------------------*
238200050714     c     DecodTAD      BEGSR
238300050714      *
238400050714     c                   clear                   dTAD
238500050714     c                   reset                   TIBS02ds
238600050714     c                   movel     KNSIF         T02sif
238700050714     c*** già impostato: movel     'TAD'         T02cod
238800050714     c                   movel(p)  DETtad        T02ke1
238900050714     c                   call      'TIBS02R'
239000050714     c                   parm                    KPJBA
239100050714     c                   parm                    TIBS02ds
239200050805     c                   if        T02err      = *blanks
239300050714     c                   movel     T02uni        dTAD
239400050714     c                   endif
239500050715      *
239600051017if  1***c                   if        §TADevfo    = 'S'
239700051017     ***c                   eval      $UpdDET     = 'F'
239800051017e   1***c                   endif
239900050714      *
240000050714     c                   ENDSR
240100050726      *
240200050726      *---------------------------------------------------------------*
240300050726      * Aggiornamento FNDET00F                                        *
240400050726      *---------------------------------------------------------------*
240500050726     c     UpdFNDET      BEGSR
240600050726      *
240700050921if  1c                   if        DETftr <> $UpdDET
240800050914      *
240900050922     c     K02DET01      chain     FNDET000
241000050922      *
241100050805     c                   eval      DETftr      = $UpdDET
241200050726      *
241300050726     c                   UPDATE    FNDET000
241400050914      *
241500050914e   1c                   endif
241600050726      *
241700050726     c                   ENDSR
241800050726      *
241900050726      *---------------------------------------------------------------*
242000050726      * Aggiornamento FNDRA00F                                        *
242100050726      *---------------------------------------------------------------*
242200050726     c     UpdFNDRA      BEGSR
242300050726      *
242400050914if  0c                   if        $UpdDRA    <> DRAesc
242500050914      *
242600050726     c                   clear                   DRAatb
242700050726if  1c                   if        NOT %found(FNDRA01L)
242800050805     c                   eval      DRAaac      = H2Caac
242900050805     c                   eval      DRAfil      = S2Cfil
243000050805     c                   eval      DRAnca      = S2Cnca
243100050726e   1c                   endif
243200050726      *
243300050805     c                   eval      DRAesc      = $UpdDRA
243400050726      *
243500050726if  1c                   if        %found(FNDRA01L)
243600050726     c                   UPDATE    FNDRA000
243700050726x   1c                   else
243800050726     c                   WRITE     FNDRA000
243900050726e   1c                   endif
244000050914      *
244100050914x   0c                   else
244200050914      *
244300050914     c                   UNLOCK    FNDRA01L
244400050914      *
244500050914e   0c                   endif
244600050726      *
244700050726     c                   ENDSR
244800050713
244900050713      *---------------------------------------------------------------*
245000050713
245100050714**   $CCHpag
245200051025051518
245300050727**   $MSG -------------------------------------------------------------------*
245400050727Numero Evento errato                                                           1
245500050727Immettere l'anno dell'evento                                                   2
245600050727Numero Evento inesistente                                                      3
245700051017?» Libero «?L'anomalia deve essere   EVENTO   NON FORTUITO                                 4
245800050727Evento aperto prima del 18/04/2005                                             5
245900050727Evento con C.A. relative a spedizioni precedenti il 18/04/2005                 6
246000060524Evento fortuito chiuso oppure importi liquidati per l'evento < §STDLAE         7
246100051025?» Libero «?Evento già recuperato                                                          8
246200051013Data apertura evento errata o precedente il 18/04/2005                         9
246300051013Range di date errato                                                          10
246400051013Opzione non valida                                                            11
246500051013Non è selezionabile un evento "escluso"                                       12
246600051017Recupero già richiesto per questa C.A.                                        13
246700051025Impossibile inserire i dati del recupero mancandone ancora la richiesta       14
246800051013Non è selezionabile una C.A. "esclusa"                                        15
246900051013Non è selezionabile una C.A. di un evento "escluso"                           16
247000051025Premere F6 per gestire le selezioni di "Richiesta Recupero"                   17
247100051017Per tutte le C.A. dell'evento è già stato chiesto (o escluso) il recupero     18
247200051017Selezionare le C.A. di cui richiedere recupero (opz. "R") PRIMA di premere F6 19
247300051017Opzione NON valida per un evento per il quale è già stato chiesto il recupero 20
247400051017Opzione NON valida per una C.A. per la quale è già stato chiesto il recupero  21
247500051013Opzione NON valida per una C.A.  aperta  o  chiusa ma non contabilizzata      22
