000100050302      *---------------------------------------------------------------*
000200050404      *? Gestione parametri per la cancellazione di spool            ?*
000300050302      *---------------------------------------------------------------*
000400050302      *
000500050302     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600050302      *
000700050404      *?A R C H I V I?------------------------------------------------*
000800050302      *
000900050404     fTNSY15D   cf   e             workstn sfile(TNSY15S1:S1NRR)
001000050719      *
001100050310     fAZCSP01L  if   e           k disk    rename(AZCSP000:AZCSP001)
001200050310     f                                     infds(INFOdsLF)
001300050310     fAZCSP00F  Uf A e             disk
001400050405     f                                     infds(INFOdsPF)
001500050302      *
001600050404      *?C O S T A N T I?----------------------------------------------*
001700050302      *
001800050303     ***d K_Enter         c                   const(X'F1')
001900050308     d k_ALL           c                   const('*ALL      ')
002000050308     d k_LIBL          c                   const('*LIBL     ')
002100050302      *
002200050404      *?S C H I E R E?------------------------------------------------*
002300050302      *
002400050719     d $Msg            s             78    dim(13) ctdata perrcd(1)
002500050308     d $Opz            s              1    dim(6)  ctdata perrcd(1)
002600050304     d $Tit            s             28    dim(6)  alt($Opz)
002700050302      *
002800050404      *?S T R U T T U R E   D A T I?----------------------------------*
002900050302      *
003000050303     d Tibs34ds      e ds
003100050303     d Azuteds       e ds                  extname(Azute00f)
003200050303     d dDatiute      e ds
003300050303      *
003400050303     d KPJBA         e ds
003500050310      *
003600050405      * *INFDS associata al file logico
003700050310     d INFOdsLF        ds
003800050405     d  lf_RRN               397    400i 0
003900050405      * *INFDS associata al file fisico
004000050405     d INFOdsPF        ds
004100050405     d  pf_RRN               397    400i 0
004200050302      *
004300050302     d WLBdat          ds                  inz
004400050302     d  G02dat                 1      8  0 inz
004500050302     d  G02inv                 9     16  0 inz
004600050302     d  G02err                17     17    inz('3')
004700050302     d  G02tgi                18     22  0 inz
004800050302      *
004900050302     d Status         sds
005000050303     d  SDSpgm           *proc
005100050303     D  SDSprm           *parms
005200050302      *
005300050404      *?V A R I A B I L I?--------------------------------------------*
005400050302      *
005500050303     d wKPJBA          s                   like(KPJBA)
005600050303      *
005700050303     d S1nrr           s              4  0 inz
005800051206     d X1nrr           s                   inz   like(S1nrr)
005900050303      *
006000050304      * indici di schiera
006100050303     d xx              s              3  0 inz
006200050302      *
006300050304      * flag booleani
006400050309     d winKJ           s              1    inz(*off)
006500050304     d $Found          s              1    inz(*off)
006600050302     d $Fine           s              1    inz(*off)
006700050310     d $First          s              1    inz(*on)
006800050302     d $InzS1          s              1    inz(*on)
006900050302     d $InzD1          s              1    inz(*on)
007000050308     d $InzW1          s              1    inz(*on)
007100050719     d $ASbackup       s              1
007200050303      *
007300050304      * Parametri x pgm. QCMDEXC
007400050303     d Qcmd            s            100    inz
007500050303     d Qlen            s             15  5 inz(100)
007600050304      *
007700050406      * Parametri x pgm. TRUL45R
007800050406     d PRPuser         s             10    inz
007900050406     d PRPoutq         s             10    inz
008000050406     d PRPoutqlib      s             10    inz
008100050406     d PRPformtype     s             10    inz
008200050406     d PRPusrdta       s             10    inz
008300051124     d PRPstatus       s              1    inz
008400050406     d PRPdatpul       s              7    inz
008500051123     d PRPrifdtp       s              1    inz
008600050406     d PRPlista        s              1    inz
008700050304      *
008800050304      * Altri campi
008900050308     d wDateISO        s               d   inz   datfmt(*iso)
009000050309     d wDateEUR        s               d   inz   datfmt(*eur)
009100050308     d wDate8          s              8  0 inz
009200050308     d wYear           s              4  0 inz
009300050308     d wSel            s              3  0 inz
009400050309     d $Gest           s              2    inz('S1')
009500051206     d $Seq24          s              1    inz('A')
009600051206     d WWWpty          s                   inz   like(CSPpty)
009700051206     d Wincrem         s                   inz   like(CSPpty)
009800050308     d SAVuser         s                   inz   like(CSPuser)
009900050308     d SAVoutq         s                   inz   like(CSPoutq)
010000050308     d SAVoutqlib      s                   inz   like(CSPoutqlib)
010100050302      *
010200050404      *?I N D I C A T O R I?------------------------------------------*
010300050302      *
010400050302      *  01 - Immissione
010500050303      *  02 - Modifica (record attivo - annullabile)
010600050303      *  04 - Ripristino record annullato
010700050302      *  05 - Visualizzazione
010800050302      *  20 - SflDsp/SflEnd
010900050302      *  21 - SflDspCtl
011000050302      * n21 - SflClr
011100050302      *  22 - SflNxtChg
011200050303      *  28 - Visualizzazione V1CMSG
011300050308      *  41 - condiziona dspatr(RI) sul campo VS1ATB (sfl)
011400050303      *       x evidenziare rec. annullato
011500050303      *  51 - Posiz. cursore su campo V1Cpty = Priorità
011600050303      *  53 - Posiz. cursore su campo V1Cdes = Descrizione
011700050303      *  54 - Posiz. cursore su campo V1Cuser = Profilo Utente
011800050303      *  55 - Posiz. cursore su campo V1Coutq = Coda di stampa
011900050303      *  56 - Posiz. cursore su campo V1Coutqlib = Libreria outq
012000050303      *  57 - Posiz. cursore su campo V1Cfrmtyp = Modulo di stampa
012100050303      *  58 - Posiz. cursore su campo V1Cusrdta = Dati Utente
012200050303      *  90 - Generico di errore
012300050302      *
012400050302      *---------------------------------------------------------------*
012500050404      *
012600050404      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
012700050302      *
012800050302do  1c                   doW       $Fine = *off
012900050302     c     $Gest         casEQ     'S1'          sr_GesS1
013000050304     c     $Gest         casEQ     'D1'          sr_GesD1
013100050308     c     $Gest         casEQ     'W1'          sr_GesW1
013200050310     c     $Gest         casEQ     'H1'          sr_GesH1
013300051206     c     $Gest         casEQ     'H2'          sr_GesH2
013400050302     c                   endcs
013500050302e   1c                   enddo
013600050302      *
013700050302     c                   eval      *inLR = *on
013800050302      *
013900050404      *?S U B R O U T I N E S?----------------------------------------*
014000050302      *
014100050302      *---------------------------------------------------------------*
014200050404      *?Routine Iniziale                                             ?*
014300050302      *---------------------------------------------------------------*
014400050302     c     *InzSR        BegSR
014500050303      *
014600050303     c     *Entry        plist
014700050303     c                   parm                    Wkpjba
014800050303     c                   if        SDSprm > *zeros
014900050303     c                   movel     Wkpjba        KPJBA
015000050303     c                   endif
015100050304      *
015200050304      * Reperimento data odierna
015300050304     c                   move      *date         WdateISO
015400050302      *
015500050302      * Reperisco dati job
015600050405     c                   if        SDSprm > *zeros
015700050302     c                   exsr      DatiJob
015800050405     c                   endif
015900050302     c                   movel     SDSpgm        VTCpgm
016000050719      *
016100050719      * Controllo se esiste area dati backup
016200050719     c                   call      'TNSY14C'
016300050719     c                   parm                    $ASbackup
016400050302      *
016500050302     c                   EndSr
016600050302      *
016700050302      *---------------------------------------------------------------*
016800050404      *?Reperimento Dati del job (Utente/Operativi)                  ?*
016900050302      *---------------------------------------------------------------*
017000050302     c     DatiJob       BegSr
017100050302      *
017200050302     c     *dtaara       define    §azute        azuteds
017300050302     c     *dtaara       define    §datiute      ddatiute
017400050302      *
017500050302     c                   in(E)     *dtaara
017600050302     c                   IF        %ERROR or RSUT = *blanks
017700050302     c                   clear                   Tibs34Ds
017800050302     c                   call      'TIBS34R'
017900050302     c                   parm                    Tibs34Ds
018000050302     c                   in        *dtaara
018100050302     c                   ENDIF
018200050302      *
018300050302     c                   EndSr
018400050302      *
018500050302      *---------------------------------------------------------------*
018600050404      *?sr_GesS1   * Gestione dati a video nel subfile               ?*
018700050302      *---------------------------------------------------------------*
018800050302     c     sr_GesS1      BEGSR
018900050302      *
019000050302      * Caricamento dati nel sfl
019100050302if  1c                   if        $InzS1 = *on
019200050302     c                   exsr      sr_InzS1
019300050302     c                   eval      $InzS1 = *off
019400050302e   1c                   endif
019500050302      *
019600050302      * Scrivo la testata e la riga tasti funzionali abilitati
019700050307     c                   clear                   VTCmod
019800050302if  1c                   if        NOT *in90
019900050302      * - emissione testata (x rstdsp *no)
020000050404     c                   write     tnsy15t1
020100050302      * - emissione piede
020200051206     c                   select
020300051206     c                   when      $Seq24 = 'A'
020400051206     c                   write     tnsy15z1a
020500051206     c                   when      $Seq24 = 'B'
020600051206     c                   write     tnsy15z1b
020700051206     c                   endsl
020800050302e   1c                   endif
020900050302      *
021000050302      * Posiziono il cursore nel sfl
021100050303if  1c                   if        S1nrr  > *zeros
021200050302     c                   z-add     V1Cpos        V1Cnrr
021300050303e   1c                   endif
021400050302      *
021500050302      * Emetto il subfile
021600050303if  1c                   if        S1nrr  > *zeros
021700050404     c                   exfmt     tnsy15c1
021800050303x   1c                   else
021900050404     c                   exfmt     tnsy15s0
022000050303e   1c                   endif
022100050302     c                   setoff                                       28  90
022200050302     c                   clear                   V1Cmsg
022300050302      *
022400050302sel 1c                   select
022500050302      * F3=Fine
022600050302w   1c                   when      *inKC
022700050302     c                   eval      $Fine  = *on
022800051206      ****** F5=Rivisualizza
022900051206w   1c*****              when      *inKE
023000050310     c*****              eval      $InzS1 = *on
023100051206      * F5=Decodifica status
023200051206w   1c                   when      *inKE
023300050310     c                   eval      $Gest  = 'H1'
023400051206      * F7=Decodifica data di riferimento per cancellazione spool
023500051206w   1c                   when      *inKG
023600051206     c                   eval      $Gest  = 'H2'
023700050302      * F10=Immissione
023800050302w   1c                   when      *inKJ
023900050304     c                   clear                   VS1opz
024000050302     c                   eval      $Gest  = 'D1'
024100050302     c                   eval      $InzD1 = *on
024200050302      * F12=Ritorno
024300050302w   1c                   when      *inKL
024400050302     c                   eval      $Fine  = *on
024500050302     c                   eval      $InzS1 = *on
024600051206      * F20=Sequenza priorità
024700051206w   1c                   when      *inKU
024800070718     c                             and  S1nrr  > *zeros
024900051206     c                   exsr      sr_PtySeq
025000051206     c                   eval      $InzS1 = *on
025100051206      * F24=Altri tasti
025200051206w   1c                   when      *inKY
025300051206     c                   select
025400051206     c                   when      $Seq24 = 'A'
025500051206     c                   eval      $Seq24 = 'B'
025600051206     c                   when      $Seq24 = 'B'
025700051206     c                   eval      $Seq24 = 'A'
025800051206     c                   endsl
025900050302      * Controllo opzione
026000051206x   1c                   other
026100051206if  2c                   if        S1nrr > *zeros
026200050302     c                   exsr      sr_CtrS1
026300051206e   2c                   endif
026400050302      * Gestione opzione
026500050303      * (opzione già gestita nella subr. sr_CtrS1)
026600051206if  2c***                if        S1nrr > *zeros
026700050303     c***                exsr      sr_OpzS1
026800051206e   2c***                endif
026900051206      *
027000051206e   1c                   endsl
027100050302      *
027200050310     c                   ENDSR
027300050302      *
027400050302      *---------------------------------------------------------------*
027500050404      *?Inizializzazione subfile S1                                  ?*
027600050302      *---------------------------------------------------------------*
027700050302     c     sr_InzS1      BEGSR
027800050302      *
027900050302      * Pulizia subfile
028000050302     c                   setoff                                       20  21
028100050404     c                   write     tnsy15c1
028200050302     c                   seton                                        20  21
028300050302      *
028400050302     c                   clear                   V1Cnrr
028500050302     c                   clear                   V1Cpos
028600050302     c                   clear                   S1nrr
028700050302     c                   clear                   V1Cmsg
028800050302     c                   setoff                                       28  90
028900050302     c                   movea     *zeros        *in(41)
029000051206      *
029100051206      * Caricamento dati da file AZCSP01L
029200051206      *
029300051206      * - Posizionamento iniziale
029400051206     c                   clear                   CSPtyp
029500051206     c     CSPtyp        setll     AZCSP001
029600051206     c     CSPtyp        reade     AZCSP001
029700051206      *
029800051206      * - Ciclo di lettura
029900051206do  1c                   dow       NOT %eof(AZCSP01L)
030000051206      *
030100051206      *   Visualizza SOLO i records NON annullati
030200051206      *   (ma ho lasciato la gestione del ripristino del singolo rec.)
030300051206if  2c                   if        CSPatb = *blanks
030400050302      *
030500051206      * - Caricamento dei dati da presentare nel rec del sfl
030600050302     c                   exsr      sr_RieS1
030700051206      *
030800051206      * - scrittura record nel sfl
030900051206     c                   add       1             S1nrr
031000051206     c                   WRITE     tnsy15s1
031100051206      *
031200051206e   2c                   endif
031300051206      *
031400051206     c     CSPtyp        reade     AZCSP001
031500051206      *
031600051206e   1c                   enddo
031700050303      *
031800050303      * Posizionamento cursore
031900050303if  1c                   if        S1nrr  > *zeros
032000050303     c                   eval      V1Cnrr = 1
032100050303     c                   eval      V1Cpos = 1
032200050303e   1c                   endif
032300051206      *
032400051206     c                   eval      X1nrr  = S1nrr
032500050302      *
032600050302     c                   ENDSR
032700050302      *
032800050302      *---------------------------------------------------------------*
032900051206      *?Impostazione dati nei campi del singolo record del sfl       ?*
033000050302      *---------------------------------------------------------------*
033100050302     c     sr_RieS1      BEGSR
033200050303      *
033300050404     c                   clear                   tnsy15s1
033400050303      *
033500050303      * - impostazione campi hidden
033600050405     c                   if        $InzS1     =  *on
033700050405     c                   eval      V1Hrrn     =  lf_RRN
033800050405     c                   else
033900050405     c                   eval      V1Hrrn     =  pf_RRN
034000050405     c                   endif
034100050303     c                   eval      V1Hatb     =  CSPatb
034200050304     c                   eval      V1Hdup     =  CSPdup
034300050720     c***                eval      V1Hdltbkup =  CSPdltbkup
034400050303      *
034500050303      * - impostazione campi visualizzati
034600051129     ***c                   eval      VS1atb     =  CSPatb
034700050303     c***                eval      VS1typ     =  CSPtyp
034800050303     c                   eval      VS1pty     =  CSPpty
034900050303     c                   eval      VS1des     =  CSPdes
035000050303     c                   eval      VS1user    =  CSPuser
035100050303     c                   eval      VS1outq    =  CSPoutq
035200050303     c                   eval      VS1outqlib =  CSPoutqlib
035300050303     c                   eval      VS1frmtyp  =  CSPfrmtyp
035400050303     c                   eval      VS1usrdta  =  CSPusrdta
035500050303     c                   eval      VS1status  =  CSPstatus
035600050303     c                   eval      VS1ggpul   =  CSPggpul
035700051129     c                   eval      VS1rifggp  =  CSPrifggp
035800050720     c                   eval      VS1dltbkup =  CSPdltbkup
035900050303if  1c                   if        CSPdup     >  *zeros
036000050303     c                   reset                   WLBdat
036100050303     c                   eval      G02inv     =  CSPdup
036200050303     c                   call      'XSRDA8'
036300050303     c                   parm                    WLBdat
036400050303     c                   eval      VS1dup     =  G02dat
036500050303e   1c                   endif
036600050303      *
036700050303      * - impostazione indicatori
036800050303      *   - evidenziazione record annullato
036900051129     c                   eval      *in41  = (V1Hatb <> *blanks)
037000050303     c                   eval      H1in41 =  *in41
037100050303      *
037200050303     c                   ENDSR
037300050302      *
037400050302      *---------------------------------------------------------------*
037500050404      *?Controllo subfile S1                                         ?*
037600050302      *---------------------------------------------------------------*
037700050302     c     sr_CtrS1      BEGSR
037800050302      *
037900050310     c                   reset                   $First
038000050302     c                   eval      *in28  =  *off
038100050302     c                   eval      *in90  =  *off
038200050302      *
038300050303      * Controllo opzioni sbf
038400050404     c                   readc     tnsy15s1
038500050302      *
038600050404do  1c                   dow       NOT %eof(tnsy15d)
038700050302     c                             and  not *in90
038800050302      *
038900050302     c                   z-add     S1nrr         V1Cnrr
039000050302if  2c                   if        VS1opz <> *blanks
039100050303     c                   eval      xx     =  1
039200050303     c                   eval      *in10  =  *off
039300050303     c     VS1opz        lookup    $Opz(xx)                               10
039400050302if  3c                   if        NOT *in10
039500050303     c                   movel     $Msg(1)       V1Cmsg
039600050302     c                   seton                                        284090
039700050303      * - attivazione sflnxtchg
039800050302     c                   eval      *in22  =  *on
039900050302x   3c                   else
040000050302     c                   exsr      sr_OpzS1
040100050302e   3c                   endif
040200050302e   2c                   endif
040300050302      *
040400050303     c                   eval      *in41  =  H1in41
040500050302     c                   clear                   VS1opz
040600050404     c                   update    tnsy15s1
040700050302     c                   eval      *in22  =  *off
040800050302      *
040900050302if  2c                   if        *in90
041000050719     c                             or *inKC
041100050303     c                   leavesr
041200050302e   2c                   endif
041300050404     c                   readc     tnsy15s1
041400050302      *
041500050302e   1c                   enddo
041600050302      *
041700050302     c                   ENDSR
041800050302      *
041900050302      *---------------------------------------------------------------*
042000050404      *?Gestione opzione subfile S1                                  ?*
042100050302      *---------------------------------------------------------------*
042200050302     c     sr_OpzS1      BEGSR
042300050302      *
042400050304      * Non è ammesso lanciare la cancellazione degli spool
042500050304      *   relativi ad un record annullato: viene forzata la
042600050304      *   visualizzazione di tale record.
042700050406if  1c                   if            VS1opz =  '1'
042800050406     c                             and V1Hatb <> *blanks
042900050304     c                   eval      VS1opz = '5'
043000050307     c                   eval      xx     =  1
043100050307     c                   eval      *in10  =  *off
043200050307     c     VS1opz        lookup    $Opz(xx)                               10
043300050307if  2c***                if        NOT *in10
043400050307     c***  impossibile !
043500050307e   2c***                endif
043600050304e   1c                   endif
043700050304      *
043800050302      * Conviene gestire la singola manutenzione "direttamente"
043900050302      *  in modo da poter gestire più opzioni nel subfile e
044000050302      *  riemettere il subfile stesso una volta sola
044100050304if  1c                   if        VS1opz = '1'
044200050308     c                   eval      $Gest  = 'W1'
044300050308     c                   eval      $InzW1 = *on
044400050308     c                   exsr      sr_GesW1
044500050304x   1c                   else
044600050302     c                   eval      $Gest  = 'D1'
044700050302     c                   eval      $InzD1 = *on
044800050302     c                   exsr      sr_GesD1
044900050308e   1c                   endif
045000050310     c*** in GesD1/W1:   eval      $Gest  = 'S1'
045100050310     c*** in GesD1/W1:   eval      $InzS1 = *on
045200050302      *
045300050302     c                   ENDSR
045400050310      *
045500050310      *---------------------------------------------------------------*
045600050404      *?sr_GesH1   * Visualizzazione windows con help su "status"    ?*
045700050310      *---------------------------------------------------------------*
045800050310     c     sr_GesH1      BEGSR
045900050310      *
046000050310do  1c                   DOU          *inKL = *on
046100050310     c                             or $Gest = 'S1'
046200050310      *
046300050310      * Emetto la videata
046400050404     c                   exfmt     tnsy15h1
046500050310      *
046600050310      * F12=Ritorno
046700050310if  2c                   if        *inKL
046800050310     c                   eval      $Gest  = 'S1'
046900050310e   2c                   endif
047000050310      *
047100050310e   1c                   enddo
047200050310      *
047300050310     c                   ENDSR
047400051206      *
047500051206      *---------------------------------------------------------------*
047600051206      *?sr_GesH2   * Visualizzazione windows con help su "data di    ?*
047700051206      *?             riferimento per cancellazione spool"            ?*
047800051206      *---------------------------------------------------------------*
047900051206     c     sr_GesH2      BEGSR
048000051206      *
048100051206do  1c                   DOU          *inKL = *on
048200051206     c                             or $Gest = 'S2'
048300051206      *
048400051206      * Emetto la videata
048500051206     c                   exfmt     tnsy15h2
048600051206      *
048700051206      * F12=Ritorno
048800051206if  2c                   if        *inKL
048900051206     c                   eval      $Gest  = 'S1'
049000051206e   2c                   endif
049100051206      *
049200051206e   1c                   enddo
049300051206      *
049400051206     c                   ENDSR
049500050302      *
049600050302      *---------------------------------------------------------------*
049700050404      *?sr_GesD1   * Gestione dati a video x singolo record          ?*
049800050302      *---------------------------------------------------------------*
049900050302     c     sr_GesD1      BEGSR
050000050302      *
050100050308      * Caricamento dati nella videata
050200050302if  1c                   if        $InzD1 = *on
050300050302     c                   exsr      sr_InzD1
050400050302     c                   eval      $InzD1 = *off
050500050302e   1c                   endif
050600050302      *
050700050302      * Scrivo la testata e la riga tasti funzionali abilitati
050800050302if  1c                   if        NOT *in90
050900050302      * - emissione testata (x rstdsp *no)
051000050404     c                   write     tnsy15t1
051100050302      * - emissione piede
051200050404     c                   write     tnsy15z2
051300050302e   1c                   endif
051400050302      *
051500050303do  0c                   DOU          $Fine = *on
051600050303     c                             or $Gest = 'S1'
051700050303      *
051800050302      * Emetto la videata
051900050406if  1c                   if            V1Catb <> *blanks
052000050406if  1c                             or  VS1opz =  '4'
052100050406if  1c                             or  VS1opz =  '5'
052200050404     c                   write     tnsy15d1
052300050302     c                   exfmt     Protect
052400050302x   1c                   else
052500050404     c                   exfmt     tnsy15d1
052600050302e   1c                   endif
052700050302     c                   setoff                                       28  90
052800050302     c                   clear                   V1Cmsg
052900050302      *
053000050302sel 1c                   select
053100050302      * F3=Fine
053200050302w   1c                   when      *inKC
053300050719     c                   eval      $Gest  = 'S1'
053400050719     c***                eval      $Fine  = *on
053500050303     c                   leavesr
053600050302      * F12=Ritorno
053700050302w   1c                   when      *inKL
053800050302     c                   eval      $Gest  = 'S1'
053900050303     c                   leavesr
054000050302e   1c                   endsl
054100050302      *
054200050302      * Controllo dati immessi a video
054300051212      * (non si fanno se richisto l'annullamento/cancellazione)
054400050303if  1c                   if        NOT *inKQ
054500050307     c                             and (VS1opz <> '4'
054600050307     c                              or (VS1opz =  '4'
054700050406     c                              and V1Hatb <> *blanks))
054800050302     c                   exsr      sr_CtrD1
054900050302e   1c                   endif
055000050302      *
055100051212      **F5=Ripristino***               ¯\
055200051212      * F6=Aggiornamento                 } se non ci sono errori
055300051212      * F16=Annullamento/Cancellazione _/
055400051212if  1c                   if        NOT *in90
055500051212     c                             and (*inKF or *inKQ)
055600051212     c***                          and (*inKE or *inKF or *inKQ)
055700050302     c                   exsr      sr_AggCSP
055800050303     c                   eval      $Gest  = 'S1'
055900050309if  2c                   if        WinKJ = *on or VS1opz = '3'
056000051206     c                                         or VS1opz = '4'
056100051209     c                                         or *inKQ  = *on
056200050310     c                                         or V1Cpty <> V1Hpty
056300050303     c                   reset                   $InzS1
056400050303x   2c                   else
056500051206     c                   exsr      sr_RieS1
056600050304     c*** in sr_CtrS1:   eval      *in41  =  H1in41
056700050304     c*** in sr_CtrS1:   clear                   VS1opz
056800050404     c*** in sr_CtrS1:   update    tnsy15s1
056900050303e   2c                   endif
057000050302e   1c                   endif
057100050303      *
057200050303e   0c                   enddo
057300050302      *
057400050303     c                   ENDSR
057500050302      *
057600050302      *---------------------------------------------------------------*
057700050404      *?Inizializzazione videata D1                                  ?*
057800050302      *---------------------------------------------------------------*
057900050302     c     sr_InzD1      BEGSR
058000050302      *
058100050302     c                   clear                   AZCSP000
058200050304     c                   reset                   $Found
058300050308     c                   reset                   SAVuser
058400050308     c                   reset                   SAVoutq
058500050308     c                   reset                   SAVoutqlib
058600050309     c                   eval      WinKJ  = *inKJ
058700050302      *
058800050302sel 1c                   select
058900050302      * F10=Immissione
059000050302w   1c                   when      *inKJ  = *on
059100050308     c                   eval      $Found = *off
059200050302      * Opz.2=Modifica / Opz.4=Annulla
059300050302w   1c                   when      VS1opz = '2' or VS1opz = '4'
059400050310     c     V1Hrrn        chain     AZCSP000
059500050310     c                   eval      $Found = (%found(AZCSP00F))
059600050302      * Opz.3=Copia / Opz.5=Visualizza
059700050302x   1c                   other
059800050310     c     V1Hrrn        chain(n)  AZCSP000
059900050310     c                   eval      $Found = (%found(AZCSP00F))
060000050302e   1c                   endsl
060100050302      *
060200050302     c                   exsr      sr_RieD1
060300050302      *
060400050302     c                   ENDSR
060500050302      *
060600050302      *---------------------------------------------------------------*
060700050404      *?Riempimento videata D1                                       ?*
060800050302      *---------------------------------------------------------------*
060900050302     c     sr_RieD1      BEGSR
061000050302      *
061100050404     c                   clear                   tnsy15d1
061200050302      *
061300050302      * Preparazione dati di "testata"
061400050303if  1c                   if        *inKJ
061500050303     c                   eval      xx     = 1
061600050303e   1c                   endif
061700051212sel 1c***                select
061800051212if  1c***                when          VS1opz =  '4'
061900051212     c***                          and V1Hatb =  *blanks
062000051212     c***                move      $Tit(4)       VTCmod
062100051212if  1c***                when          VS1opz =  '2'
062200051212     c***                          and V1Hatb <> *blanks
062300051212     c***                movel     $Tit(4)       VTCmod
062400051212x   1c***                other
062500050303     c                   movel     $Tit(xx)      VTCmod
062600051212e   1c***                endsl
062700050303      *
062800050303      * Attivazione indicatori di
062900050303      *   protezione campi / abilitazazione tasti funzionali
063000050303      *   a seconda dell'opzione
063100050303sel 1c                   select
063200050303w   1c                   when      *inKJ  = *on  or  VS1opz = '3'
063300050303     c                   eval      *in01  = *on
063400050406w   1c                   when         (VS1opz =  '2' or VS1opz = '4')
063500050406w   1c                             and V1Hatb <> *blanks
063600050303     c                   eval      *in04  = *on
063700050303w   1c                   when          VS1opz = '2'
063800050303w   1c                             and V1Hatb = *blanks
063900050303     c                   eval      *in02  = *on
064000050303w   1c                   when      VS1opz = '5'
064100050303     c                   eval      *in05  = *on
064200050303e   1c                   endsl
064300050302      *
064400050304if  1c                   if        $Found = *off
064500050302     c                   leavesr
064600050302e   1c                   endif
064700050303      *
064800050303      * impostazione campi hidden
064900050310      * (in caso di immissione: non copio numero relativo di record)
065000050310if  1c                   if            WinKJ  =  *off
065100050310     c                             and VS1opz <> '3'
065200050310     c                   eval      V1Crrn     =  V1Hrrn
065300050310e   1c                   endif
065400050304      * (in caso di copia: non copio flag di annullamento;
065500050304      *                    copio il record come "attivo")
065600050304if  1c                   if        VS1opz     <> '3'
065700050303     c                   eval      V1Catb     =  CSPatb
065800050304e   1c                   endif
065900050311      * (priorità - key modificabile)
066000050311     c                   eval      V1Hpty     =  CSPpty
066100050302      *
066200050302      * impostazione campi visualizzati
066300050302     c***                eval      V1Ctyp     =  CSPtyp
066400050302     c                   eval      V1Cpty     =  CSPpty
066500050302     c                   eval      V1Cdes     =  CSPdes
066600050302     c                   eval      V1Cuser    =  CSPuser
066700050302     c                   eval      V1Coutq    =  CSPoutq
066800050303     c                   eval      V1Coutqlib =  CSPoutqlib
066900050302     c                   eval      V1Cfrmtyp  =  CSPfrmtyp
067000050302     c                   eval      V1Cusrdta  =  CSPusrdta
067100050302     c                   eval      V1Cstatus  =  CSPstatus
067200050719     c                   eval      V1Cdltbkup =  CSPdltbkup
067300050302     c                   eval      V1Cggpul   =  CSPggpul
067400051123     c                   eval      V1Crifggp  =  CSPrifggp
067500050309      * (calcolo data fino a cui cancellare)
067600050309     c                   move      *date         WdateEUR
067700050309     c                   subdur    CSPggpul:*d   WdateEUR
067800050309     c*****              subdur    1:*d          WdateEUR
067900050309     c                   move      WdateEUR      V1Dpdup
068000050309      * (visualizzo data ultima pulizia)
068100050304if  1c                   if            VS1opz <> '1'
068200050304     c                             and VS1opz <> '3'
068300050303     c                             and CSPdup >  *zeros
068400050302     c                   reset                   WLBdat
068500050303     c                   eval      G02inv     =  CSPdup
068600050302     c                   call      'XSRDA8'
068700050302     c                   parm                    WLBdat
068800050302     c                   eval      V1Cdup     =  G02dat
068900050303e   1c                   endif
069000050302      *
069100050302      * - impostazione default
069200050310if  1c***                if        V1Cuser    =  *blanks
069300050310     c***                eval      V1Cuser    =  k_ALL
069400050310e   1c***                endif
069500050310if  1c***                if        V1Coutq    =  *blanks
069600050310     c***                eval      V1Coutq    =  k_ALL
069700050310e   1c***                endif
069800050310if  1c***                if        V1Coutqlib =  *blanks
069900050310     c***                eval      V1Coutqlib =  k_LIBL
070000050310e   1c***                endif
070100050310if  1c***                if        V1Cfrmtyp  =  *blanks
070200050310     c***                eval      V1Cfrmtyp  =  k_ALL
070300050310e   1c***                endif
070400050310if  1c***                if        V1Cusrdta  =  *blanks
070500050310     c***                eval      V1Cusrdta  =  k_ALL
070600050310e   1c***                endif
070700050303      *
070800050406if  1c                   if        V1Catb     <> *blanks
070900050310     c                   eval      V1Dann     =  'RECORD ANNULLATO'
071000050303e   1c                   endif
071100050302      *
071200050302     c                   ENDSR
071300050302      *
071400050302      *---------------------------------------------------------------*
071500050404      *?Controllo dati in videata D1                                 ?*
071600050302      *---------------------------------------------------------------*
071700050302     c     sr_CtrD1      BEGSR
071800050304      *
071900050304     c                   movea     '0000000000'  *in(50)
072000050303      *
072100050303      * - impostazione default
072200050310if  1c***                if        V1Cuser    =  *blanks
072300050310     c***                eval      V1Cuser    =  k_ALL
072400050310     c***                seton                                        54  90
072500050310e   1c***                endif
072600050310if  1c***                if        V1Coutq    =  *blanks
072700050310     c***                eval      V1Coutq    =  k_ALL
072800050310     c***                seton                                        55  90
072900050310e   1c***                endif
073000050310if  1c***                if        V1Coutqlib =  *blanks
073100050310     c***                eval      V1Coutqlib =  k_LIBL
073200050310     c***                seton                                        56  90
073300050310e   1c***                endif
073400050310if  1c***                if        V1Cfrmtyp  =  *blanks
073500050310     c***                eval      V1Cfrmtyp  =  k_ALL
073600050310     c***                seton                                        57  90
073700050310e   1c***                endif
073800050310if  1c***                if        V1Cusrdta  =  *blanks
073900050310     c***                eval      V1Cusrdta  =  k_ALL
074000050310     c***                seton                                        58  90
074100050310e   1c***                endif
074200050310      *
074300050310      * Descrizione obbligatoria
074400050310if  1c                   if        V1Cdes     =  *blanks
074500050310     c                   eval      V1Cmsg     =  $Msg(2)
074600050310     c                   seton                                        285390
074700050310     c                   leavesr
074800050310e   1c                   endif
074900050310      *
075000050310      * Profilo utente obbligatorio
075100050310if  1c                   if        V1Cuser    =  *blanks
075200050310     c                   eval      V1Cmsg     =  $Msg(3)
075300050310     c                   seton                                        285490
075400050310     c                   leavesr
075500050310e   1c                   endif
075600050310      * Coda di stampa obbligatoria
075700050310if  1c                   if        V1Coutq    =  *blanks
075800050310     c                   eval      V1Cmsg     =  $Msg(4)
075900050310     c                   seton                                        285590
076000050310     c                   leavesr
076100050310e   1c                   endif
076200050310      * Libreria della coda di stampa obbligatoria
076300050310if  1c                   if        V1Coutqlib =  *blanks
076400050310     c                   eval      V1Cmsg     =  $Msg(5)
076500050310     c                   seton                                        285690
076600050310     c                   leavesr
076700050310e   1c                   endif
076800050310      * Modulo di stampa obbligatorio
076900050310if  1c                   if        V1Cfrmtyp  =  *blanks
077000050310     c                   eval      V1Cmsg     =  $Msg(6)
077100050310     c                   seton                                        285790
077200050310     c                   leavesr
077300050310e   1c                   endif
077400050310      * Dati utente obbligatori
077500050713if  1c***                if        V1Cusrdta  =  *blanks
077600050713     c***                eval      V1Cmsg     =  $Msg(7)
077700050713     c***                seton                                        285890
077800050713     c***                leavesr
077900050713e   1c***                endif
078000050308      *
078100050308      * - controllo numero selezioni
078200050308     c                   clear                   wSel
078300050308     c                   if        V1Cuser    <> k_ALL
078400050308     c                   add       1             wSel
078500050308e   1c                   endif
078600050308     c                   if        V1Coutq    <> k_ALL
078700050308     c                   add       1             wSel
078800050308e   1c                   endif
078900050308if  1c                   if            V1Coutqlib <> k_ALL
079000050308     c                             and V1Coutqlib <> k_LIBL
079100050308     c                   add       1             wSel
079200050308e   1c                   endif
079300050308if  1c                   if        V1Cfrmtyp  <> k_ALL
079400050308     c                   add       1             wSel
079500050308e   1c                   endif
079600050308if  1c                   if        V1Cusrdta  <> k_ALL
079700050308     c                   add       1             wSel
079800050308e   1c                   endif
079900050308if  1c******             if        V1Cstatus  <> *blanks
080000050308     c******             add       1             wSel
080100050308e   1c******             endif
080200050308if  1c                   if        wSel       =  *zeros
080300050310     c                   eval      V1Cmsg     =  $Msg(8)
080400050308     c                   seton                                        28  90
080500050308     c                   movea     '11111'       *in(54)
080600050308     c                   endif
080700050303      *
080800050303if  1c                   if        *in90      =  *on
080900050303     c                   leavesr
081000050303e   1c                   endif
081100050303      *
081200050303      * - impostazione decodifiche
081300050303if  1c***                if        V1Cuser    =  *blanks
081400050303     c***                eval      V1Duser    =  '(dft = "*ALL      ")'
081500050303x   1c***                else
081600050303     c***                ... RtvUsrPrf ...
081700050303e   1c***                endif
081800050302      *
081900050303      * Se inserimento o copia: verifica esistenza record
082000050308if  1c                   if        *inKJ  or  VS1opz = '3'
082100050303     c                   clear                   CSPtyp
082200050310     c     CSPtyp        setll     AZCSP001
082300050310     c     CSPtyp        reade     AZCSP001
082400050303do  2c                   dow       NOT %eof(AZCSP01L)
082500050303if  3c                   if            CSPuser    = V1Cuser
082600050303     c                             and CSPoutq    = V1Coutq
082700050303     c                             and CSPoutqlib = V1Coutqlib
082800050303     c                             and CSPfrmtyp  = V1Cfrmtyp
082900050303     c                             and CSPusrdta  = V1Cusrdta
083000050303     c                             and CSPstatus  = V1Cstatus
083100050719     c                             and CSPdltbkup = V1Cdltbkup
083200050310     c                   eval      V1Cmsg  =  %trim($Msg(9)) + ' '
083300050308     c                                     +  %editc(CSPpty:'K')
083400050308if  4c                   if        CSPatb  <> *blanks
083500050308     c                   eval      V1Cmsg  =  %trim(V1Cmsg)
083600050308     c                                     +  ' (annullato)'
083700050303e   4c                   endif
083800050303     c                   seton                                        28  90
083900050303     c                   movea     '11111'       *in(54)
084000050303     c                   leavesr
084100050303e   3c                   endif
084200050310     c     CSPtyp        reade     AZCSP001
084300050303e   2c                   enddo
084400050303e   1c                   endif
084500050303      *
084600050303      * Verifica esistenza profilo utente
084700050308if  1c                   if            V1Cuser    <> k_ALL
084800050303     c                             and %subst(V1Cuser
084900050303     c                                       :%len(%trim(V1Cuser))
085000050304     c                                       :1)  <> '*'
085100050308     c                             and V1Cuser    <> SAVuser
085200050308     c                   eval      SAVuser    = V1Cuser
085300050308     c                   eval      Qcmd    =  'CHKOBJ OBJ('
085400050308     c                                     +  %trim(V1Cuser)
085500050308     c                                     +  ') OBJTYPE(*USRPRF)'
085600050303     c                   call      'QCMDEXC'                            90
085700050303     c                   parm                    Qcmd
085800050303     c                   parm                    Qlen
085900050303if  2c                   if        *in90
086000050310     c                   eval      V1Cmsg  =  $Msg(10)
086100050303     c                   seton                                        2854
086200050303     c                   leavesr
086300050303e   2c                   endif
086400050303e   1c                   endif
086500050304      *
086600050304      * Verifica esistenza libreria della coda di stampa
086700050308if  1c                   if            V1Coutqlib <> k_ALL
086800050308     c                             and V1Coutqlib <> k_LIBL
086900050308     c                             and V1Coutqlib <> SAVoutqlib
087000050304if  2c                   if        %subst(V1Coutqlib:1:1) = '*'
087100050308     c                   eval      *in90   =  *on
087200050304x   2c                   else
087300050308     c                   eval      SAVoutqlib = V1Coutqlib
087400050308     c                   eval      Qcmd    =  'CHKOBJ OBJ('
087500050308     c                                     +  %trim(V1Coutqlib)
087600050308     c                                     +  ') OBJTYPE(*LIB)'
087700050304     c                   call      'QCMDEXC'                            90
087800050304     c                   parm                    Qcmd
087900050304     c                   parm                    Qlen
088000050304e   2c                   endif
088100050304if  2c                   if        *in90
088200050308     c                   eval      SAVoutq    = V1Coutq
088300050310     c                   eval      V1Cmsg  =  $Msg(12)
088400050304     c                   seton                                        2856
088500050304     c                   leavesr
088600050304e   2c                   endif
088700050304e   1c                   endif
088800050304      *
088900050303      * Verifica esistenza coda di stampa
089000050308if  1c                   if            V1Coutq    <> k_ALL
089100050303     c                             and %subst(V1Coutq
089200050303     c                                       :%len(%trim(V1Coutq))
089300050304     c                                       :1)  <> '*'
089400050308     c                             and V1Coutqlib <> k_ALL
089500050308     c                             and V1Coutq    <> SAVoutq
089600050308     c                   eval      SAVoutq    = V1Coutq
089700050308     c                   eval      Qcmd    =  'CHKOBJ OBJ('
089800050308     c                                     +  %trim(V1Coutqlib)
089900050308     c                                     +  '/'
090000050308     c                                     +  %trim(V1Coutq)
090100050308     c                                     +  ') OBJTYPE(*OUTQ)'
090200050303     c                   call      'QCMDEXC'                            90
090300050303     c                   parm                    Qcmd
090400050303     c                   parm                    Qlen
090500050303if  2c                   if        *in90
090600050310     c                   eval      V1Cmsg  =  $Msg(11)
090700050303     c                   seton                                        2855
090800050303     c                   leavesr
090900050303e   2c                   endif
091000050303e   1c                   endif
091100050309      *
091200050309      * Calcolo data fino a cui cancellare
091300050309     c                   move      *date         WdateEUR
091400050309     c                   subdur    V1Cggpul:*d   WdateEUR
091500050309     c                   move      WdateEUR      V1Dpdup
091600050302      *
091700050302     c                   ENDSR
091800050302      *
091900050302      *---------------------------------------------------------------*
092000050404      *?Aggiornamento archivio AZCSP01L                              ?*
092100050302      *---------------------------------------------------------------*
092200050302     c     sr_AggCSP     BEGSR
092300051212      *
092400051212      * Cancellazione record
092500051212if  1c                   if        *in02 and *inKQ
092600051212     c                             or  VS1opz =  '4'
092700051212     c                   DELETE    AZCSP000
092800051212     c                   leavesr
092900051212e   1c                   endif
093000050302      *
093100050303      * (rec. già "ripulito" nella subr. sr_InzD1)
093200050308     c*** (già fatto)    clear                   AZCSP000
093300050303      * impostazione campi chiave
093400050303     c*** *blank         eval      CSPtyp     =  V1Ctyp
093500050303     c                   eval      CSPpty     =  V1Cpty
093600050303      *
093700050303      * impostazione campi hidden
093800051212sel 1c***                select
093900051212      * - ripristino record annullato
094000051212w   1c***                when      *in04 and *inKE
094100051212     c***                eval      CSPatb     =  *blanks
094200050303      * - annullamento record attivo
094300051212w   1c***                when      *in02 and *inKQ
094400051212     c***                          or (VS1opz =  '4'
094500051212     c***                          and V1Catb =  *blanks)
094600051212     c***                eval      CSPatb     =  'A'
094700050303      * - normale modifica
094800051212x   1c***                other
094900050303     c                   eval      CSPatb     =  V1Catb
095000051212e   1c***                endsl
095100050303      *
095200050303      * impostazione campi visualizzati
095300050303     c                   eval      CSPdes     =  V1Cdes
095400050303     c                   eval      CSPuser    =  V1Cuser
095500050303     c                   eval      CSPoutq    =  V1Coutq
095600050303     c                   eval      CSPoutqlib =  V1Coutqlib
095700050303     c                   eval      CSPfrmtyp  =  V1Cfrmtyp
095800050303     c                   eval      CSPusrdta  =  V1Cusrdta
095900050303     c                   eval      CSPstatus  =  V1Cstatus
096000050719     c                   eval      CSPdltbkup =  V1Cdltbkup
096100050303     c                   eval      CSPggpul   =  V1Cggpul
096200051123     c                   eval      CSPrifggp  =  V1Crifggp
096300050303if  1c                   if        V1Cdup     >  *zeros
096400050303     c                   clear                   WLBdat
096500050309     c                   eval      G02dat     =  V1Cdup
096600050303     c                   call      'XSRDA8'
096700050303     c                   parm                    WLBdat
096800050309     c                   eval      CSPdup     =  G02inv
096900050303e   1c                   endif
097000050303      *
097100050303      * Scrittura / Aggiornamento record
097200050309if  1c                   if            WinKJ  = *on
097300050304     c                             or  VS1opz = '3'
097400050303     c                   WRITE     AZCSP000
097500050303x   1c                   else
097600050303     c                   UPDATE    AZCSP000
097700050310     c                   UNLOCK    AZCSP00F
097800050303e   1c                   endif
097900050302      *
098000050302     c                   ENDSR
098100051206      *
098200051206      *---------------------------------------------------------------*
098300051206      *?Aggiornamento sequenza priorità in AZCSP01L                  ?*
098400051206      *---------------------------------------------------------------*
098500051206     c     sr_PtySeq     BEGSR
098600051206      *
098700051206     c                   clear                   WWWpty
098800051206     c                   if        X1nrr      <  99
098900051206     c                   eval      Wincrem    =  10
099000051206     c                   else
099100051206     c                   eval      Wincrem    =  5
099200051206     c                   endif
099300051206      *
099400051206      * Ciclo di lettura del subfile
099500051206     c                   eval      *in90      =  *off
099600051206     c                   eval      S1nrr      =  1
099700051206     c     S1nrr         chain     tnsy15s1
099800051206      *
099900051206do  1c                   dow       %found(tnsy15d)
100000051206     c                             and  not *in90
100100051206      *
100200051206     c                   add       Wincrem       WWWpty
100300051206      *
100400051206     c     V1Hrrn        chain     AZCSP000
100500051206      *
100600051206if  2c                   if        %found(AZCSP00F)
100700051206     c                   eval      CSPpty     =  WWWpty
100800051206     c                   UPDATE    AZCSP000
100900051206     c                   UNLOCK    AZCSP00F
101000051206e   2c                   endif
101100051206      *
101200051206     c                   add       1             S1nrr
101300051206     c     S1nrr         chain     tnsy15s1
101400051206      *
101500051206e   1c                   enddo
101600051206      *
101700051206     c                   ENDSR
101800050308      *
101900050308      *---------------------------------------------------------------*
102000050404      *?sr_GesW1   * Gestione dati a video x conferma lancio         ?*
102100050308      *---------------------------------------------------------------*
102200050308     c     sr_GesW1      BEGSR
102300050308      *
102400050308      * Caricamento dati nella videata
102500050310if  1c                   if            $InzW1 = *on
102600050310     c                             and $First = *on
102700050308     c                   exsr      sr_InzW1
102800050308     c                   eval      $InzW1 = *off
102900050308e   1c                   endif
103000050308      *
103100050308do  1c                   DOU          $Fine = *on
103200050308     c                             or $Gest = 'S1'
103300050308      *
103400050308      * Emetto la videata
103500050310if  2c                   if        $First = *on
103600050404     c                   exfmt     tnsy15w1
103700050719     c                   clear                   V1Cmsg
103800050719     c                   setoff                                       28  90
103900050719     c                   eval      *in40  = *off
104000050310e   2c                   endif
104100050308      *
104200050308      * F12=Ritorno
104300050308if  2c                   if        *inKL
104400050308     c                   eval      $Gest  = 'S1'
104500050308     c                   leavesr
104600050308e   2c                   endif
104700050719      *
104800050719      * Controllo se cancellazione non eseguibile su AS/400 di backup
104900050719if  2c                   if            $ASbackup  = *on
105000050720     c***                          and V1Hdltbkup = 'N'
105100050720     c                             and VS1dltbkup = 'N'
105200050719     c                             and W1Cdlt     = 'S'
105300050719     c                   eval      $First = *on
105400050719     c                   eval      V1Cmsg = $Msg(13)
105500050719     c                   seton                                        284090
105600050719     c                   iter
105700050719e   2c                   endif
105800050308      *
105900050308      * F6=Conferma
106000050308if  2c                   if        NOT *in90
106100050308     c                             and *inKF
106200050310     c                   eval      $First = *off
106300050308if  3c                   if            W1Cprt = 'S'
106400050308     c                             or  W1Cdlt = 'S'
106500050308     c                   exsr      Dlt_OldSpl
106600051206     c                   exsr      sr_RieS1
106700050308e   3c                   endif
106800050308     c                   eval      $Gest  = 'S1'
106900050308     c*** in sr_CtrS1:   eval      *in41  =  H1in41
107000050308     c*** in sr_CtrS1:   clear                   VS1opz
107100050404     c*** in sr_CtrS1:   update    tnsy15s1
107200050308e   2c                   endif
107300050308      *
107400050308e   1c                   enddo
107500050304      *
107600050308     c                   ENDSR
107700050308      *
107800050308      *---------------------------------------------------------------*
107900050404      *?Inizializzazione videata W1                                  ?*
108000050308      *---------------------------------------------------------------*
108100050308     c     sr_InzW1      BEGSR
108200050308      *
108300050404     c                   clear                   tnsy15w1
108400050308      *
108500050308      * Preparazione dati
108600050308     c***                eval      W1Cprt = 'S'
108700050308     c***                eval      W1Cdlt = 'S'
108800050308      *
108900050308     c                   ENDSR
109000050308      *
109100050304      *---------------------------------------------------------------*
109200050404      *?Dlt_OldSpl * Cancellazione spool                             ?*
109300050304      *---------------------------------------------------------------*
109400050304     c     Dlt_OldSpl    BEGSR
109500050304      *
109600050304      * aggancio record da aggiornare di AZCSP01L
109700050310     c     V1Hrrn        chain     AZCSP000
109800050310if  1c***                if        NOT %found(AZCSP00F)
109900050310     c***  impossibile !!!
110000050310e   1c***                endif
110100050304      *
110200050304      * calcolo data (secolo compreso) fino a cui cancellare...
110300050307     c                   move      *date         WdateISO
110400050307     c                   subdur    CSPggpul:*d   WdateISO
110500050304     c                   move      WdateISO      Wdate8
110600050406     c                   move      Wdate8        PRPdatpul
110700050304     c                   extrct    WdateISO:*Y   Wyear
110800050304     c                   if        Wyear >= 2000
110900050406     c                   movel     '1'           PRPdatpul
111000050304     c                   endif
111100050304      *
111200050304      * richiamo pgm x cancellazione vecchi spool da outq generiche
111300050308sel 1c                   select
111400050406w   1c                   when      W1Cprt   =  'S'     and W1Cdlt = *blanks
111500050406     c                   eval      PRPlista =  '1'
111600050406w   1c                   when      W1Cprt   =  *blanks and W1Cdlt = 'S'
111700050406     c                   eval      PRPlista =  '2'
111800050406w   1c                   when      W1Cprt   =  'S'     and W1Cdlt = 'S'
111900050406     c                   eval      PRPlista =  '3'
112000050308e   1c                   endsl
112100050406if  1c                   if        PRPlista <> *blanks
112200050406     c                   call      'TRUL45R'                            90
112300050406     c                   parm      CSPuser       PRPuser
112400050406     c                   parm      CSPoutq       PRPoutq
112500050406     c                   parm      CSPoutqlib    PRPoutqlib
112600050406     c                   parm      CSPfrmtyp     PRPformtype
112700050406     c                   parm      CSPusrdta     PRPusrdta
112800051124     c                   parm      CSPstatus     PRPstatus
112900050406     c                   parm                    PRPdatpul
113000051123     c                   parm      CSPrifggp     PRPrifdtp
113100050406     c                   parm                    PRPlista
113200050406      * PRPlista: 1 = solo lista
113300050406      *           2 = solo cancellazione
113400050406      *           3 = lista e cancellazione
113500050308e   1c                   endif
113600050304      *
113700050304      * aggiornamento archivio AZCSP00F (se non si sono stati errori)
113800050308if  1c                   if        W1Cdlt = 'S'
113900050308if  2c                   if        NOT *in90
114000050304     c                   move      WdateISO      CSPdup
114100050304     c                   UPDATE    AZCSP000
114200050309e   2c                   endif
114300050310     c                   UNLOCK    AZCSP00F
114400050308e   1c                   endif
114500050304      *
114600050304     c                   ENDSR
114700050302      *
114800050303**   $Msg
114900050303Opzione non valida                                                             1
115000050303Descrizione obbligatoria                                                       2
115100050310Profilo utente obbligatorio                                                    3
115200050310Coda di stampa obbligatoria                                                    4
115300050310Libreria della coda di stampa obbligatoria                                     5
115400050310Modulo di stampa obbligatorio                                                  6
115500050310Dati utente obbligatori                                                        7
115600050310Inserire ALMENO una selezione                                                  8
115700050310Selezione già inserita con priorità                                            9
115800050310AVVERTIMENTO: Profilo utente inesistente                                      10
115900050310AVVERTIMENTO: Coda di stampa inesistente                                      11
116000050310AVVERTIMENTO: Libreria inesistente                                            12
116100050719CANCELLAZIONE di questi spool NON ammessa su AS/400 di back-up                13
116200050303**   $Opz / $Tit
116300050303   IMMISSIONE                 > Immissione
1164000503032 MANUTENZIONE                > Modifica
1165000503033    COPIA                    > Copia
1166000512124CANCELLAZIONE                > Cancellazione - era:   RIPRISTINO   ANNULLAMENTO
1167000503035INTERROGAZIONE               > Visualizzazione
1168000503041                             > Lancio cancellazione spool
