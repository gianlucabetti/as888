000100090227      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200130606     /*CMD addlible lib(FILTRAPRD) position(*last)
000300100125     /*PRM closqlcsr(*endmod) dbgview(*source) dynusrprf(*owner)
000400130606     /*END rmvlible lib(FILTRAPRD)
000500100125     /*END
000600060412
000700060412      *===============================================================*
000800090225      *?TNSDA1R * Modif. cod. cliente in bolle (TAS/CSB/BLP/ARB/AR6) ?*
000900060412      *===============================================================*
001000060412
001100060412     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200060412     h alwnull(*inputonly)
001300060412      *
001400060412      *?A R C H I V I?------------------------------------------------*
001500060412      *
001600060419     fTABEL00F  if   e           k disk
001700060412     fAZORG01L  if   e           k disk
001800060413      *
001900100127     fTITAS00F  Uf   e             disk    usropn
002000090225     fTNCSB03L  Uf   e           k disk    usropn
002100100127     fFNBLP00F  Uf   e             disk    usropn
002200100127     fFNARB00F  Uf   e             disk    usropn
002300060418     fFIAR601L  Uf   e           k disk    usropn
002400060412      *
002500100315     fTNSDA1D   cf   e             workstn infds(DSfmt)
002600100126     f                                     sfile(SDA1S01 : S01nrr)
002700060412      *
002800060412      *?C O S T A N T I?----------------------------------------------*
002900060412      *
003000060418      * - Costanti per l'SQL
003100130129     d c_OrderBy       c                   const('ORDER BY AAS, LNP, +
003200130129     d                                                NRS, NSP, File')
003300100126     d c_ForFetch      c                   const('FOR FETCH ONLY')
003400060508      *
003500090225      * - Costante per controllo "caratteri solo numerici"
003600100126     d c_Digits        c                   const('1234567890')
003700100125      *
003800100126      * - Numero massimo di spedizioni elaborabili
003900100127     d c_MaxSped       c                   const(30000)
004000100126      *
004100100126      * - SubFile Page
004200100126     d c_SflPag        c                   const(16)
004300100126      *
004400100126      * - Tasti di funzione
004500100126     d c_Enter         c                   const(x'F1')
004600100127     d c_RollDown      c                   const(x'F4')
004700100126     d c_RollUp        c                   const(x'F5')
004800060412      *
004900060412      *?S C H I E R E?------------------------------------------------*
005000060412      *
005100060418      * - Messaggi di errore
005200060508     d $Msg            s             78    dim(10) ctdata perrcd(1)
005300130129      * - Archivi da gestire via SQL
005400130129     d $File           s             10    dim( 3) ctdata perrcd(3)
005500060418      * - Stringhe SQL
005600130129     d $Sql_FNBLP      s             55    dim(09) ctdata perrcd(1)
005700130129     d $Sql_FNARB      s             55    dim(09) ctdata perrcd(1)
005800130129     d $Sql_TITAS      s             55    dim(09) ctdata perrcd(1)
005900060419      *
006000100126      * - Spedizioni elaborabili
006100100127     d $Sped           s                   like(ds_Sped)
006200100126     d                                     dim(c_MaxSped)  inz
006300100126      * - Nrr spedizione rilevata nel file FNBLP00F
006400100127     d $nrrBLP         s                   like(SQLrrn)
006500100127     d                                     dim(c_MaxSped)  inz
006600100126      * - Nrr spedizione rilevata nel file FNARB00F
006700100127     d $nrrARB         s                   like(SQLrrn)
006800100127     d                                     dim(c_MaxSped)  inz
006900100126      * - Nrr spedizione rilevata nel file TITAS00F
007000100127     d $nrrTAS         s                   like(SQLrrn)
007100100127     d                                     dim(c_MaxSped)  inz
007200100126      * - Conteggio records x spedizioni rilevate in FIAR600F
007300100126     d $SpedAR6        s              3  0 dim(c_MaxSped)  inz
007400100126      * - Flag spedizione rilevata nel file TNCSB00F
007500100126     d $SpedCSB        s               n   dim(c_MaxSped)  inz
007600060412      *
007700060412      *?S T R U T T U R E   D A T I?----------------------------------*
007800060412      *
007900060412      * - Parametri
008000060412     d KPJBA         e ds
008100060418      *
008200060412      * - Parametri x Controllo profilo utenti
008300060412     d TIBS34ds      e ds                  inz
008400060412      * - Ds di riferimento al file esterno AZUTE00F
008500060412     d AZUTEds       e ds                  extname(AZUTE00F)
008600060412      * - Ds per dati organigramma
008700060412     d dDATIUTE      e ds
008800060412      *
008900060418      * - Parametri per decodifica cliente
009000060412     d TIBS69ds      e ds                  inz
009100060412     d DS_cnaco      e ds                  inz  extname(CNACO00F)
009200060412     d DS_cnind      e ds                  inz  extname(CNIND00F)
009300060412     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
009400060412     d DS_fncls      e ds                  inz  extname(FNCLS00F)
009500060420      *
009600090225      * - Parametri per Interrogazione singola spedizione Sede
009700060420     d TNSB50ds      e ds                  inz
009800060420     d  I50op0       e                     inz('D05')
009900060420      *
010000090225      * - Parametri per Interrogazione singola spedizione Partenza
010100060420     d FNLS05ds        ds                  inz
010200060420     d  LS05aas                1      4  0 inz
010300060420     d  LS05lnp                5      7  0 inz
010400060420     d  LS05nrs                8      9  0 inz
010500060420     d  LS05nsp               10     16  0 inz
010600060420     d  LS05f03               17     17    inz
010700060420     d  LS05flg               18     18    inz('C')
010800060420     d  LS05fa9               19     19    inz
010900060420     d  LS05fb5               20     20    inz
011000060420     d  LS05f75               21     21    inz
011100060420     d  LS05s98               22     22    inz
011200060420     d  LS05rsu               23     42    inz
011300060420     d  LS05cml               43     43    inz
011400060420     d  LS05fel               44     46  0 inz
011500060420      * LS05CLI indica se il pgm è richiamato da un cliente (='S')
011600060420     d  LS05cli               47     47    inz
011700060420      * LS05GIA indica se il pgm è richiamato dalle giacenze
011800060420     d  LS05gia               48     48    inz
011900060420      *
012000090225      * - Parametri per Interrogazione singola spedizione Arrivo
012100060420     d FNLR36ds        ds                  inz
012200060420     d  LR36aas               14     17  0 inz
012300060420     d  LR36lnp               18     20  0 inz
012400060420     d  LR36nrs               21     22  0 inz
012500060420     d  LR36nsp               23     29  0 inz
012600060420     d  LR36f03               30     30    inz
012700060421     d  LR36flg               31     31    inz('1')
012800060420     d  LR36bol               62     62    inz
012900060412      *
013000090225      * - Parametri per controllo data e transform in giorni - XSRDA8 -
013100060412     d WLBdat          ds                  inz
013200060419     d  G08dat                        8  0 inz
013300060419     d  G08inv                        8  0 inz
013400060419     d  G08err                        1    inz('3')
013500060419     d  G08tgi                        5  0 inz
013600090225      *
013700090225      * - Ridefinizione "Codici Bolla" a video in schiera
013800090225     d ds_V1Ccbx       ds                  inz
013900090225     d  V1Ccb1                             inz
014000090225     d  V1Ccb2                             inz
014100090225     d  V1Ccb3                             inz
014200090225     d  V1Ccb4                             inz
014300090225     d  V1Ccb5                             inz
014400090225     d  V1Ccb6                             inz
014500090225     d  V1Ccb7                             inz
014600090225     d  V1Ccb8                             inz
014700090225     d  V1Ccb9                             inz
014800090225     d  V1CcbA                             inz
014900100127     d   $V1Ccbx               1     20    inz  dim(10)
015000060418      *
015100060418      * - Dati estratti via SQL
015200060418     d wSQL_ds         ds                  inz
015300100127     d  SQLaas                        4s 0 inz
015400100127     d  SQLlnp                        3s 0 inz
015500100127     d  SQLnrs                        2s 0 inz
015600100127     d  SQLnsp                        7s 0 inz
015700130129     d  SQLfile                       6    inz
015800100127     d  SQLrrn                       11  0 inz
015900060419      *
016000100127     d ds_Sped         ds                  inz
016100100127     d  WWWaas                             inz  like(SQLaas)
016200100127     d  WWWlnp                             inz  like(SQLlnp)
016300100127     d  WWWnrs                             inz  like(SQLnrs)
016400100127     d  WWWnsp                             inz  like(SQLnsp)
016500060420      *
016600100126     d DSFMT           ds           512
016700100126     d  £Tasto               369    369
016800100126      *
016900060420     d Status         sds
017000060420     d  SDSpgm           *proc
017100060412      *
017200060412      *?V A R I A B I L I?--------------------------------------------*
017300060412      *
017400060412      * - Flags booleani
017500100125     d $EoF            s               n   inz(*off)
017600100125     d $Err            s               n   inz(*off)
017700100125     d $Fine           s               n   inz(*off)
017800100125     d $InzD01         s               n   inz(*on)
017900100126     d $InzD02         s               n   inz(*on)
018000100125     d $InzS01         s               n   inz(*on)
018100100127     d $AltreSped      s               n   inz
018200060418      * - Altri flag
018300100126     d $Video          s              1a   inz('1')
018400060412      * - Indici di schiera / Contatori
018500100126     d jj              s              5  0 inz
018600100125     d ww              s              5  0 inz
018700100125     d wx              s              5  0 inz
018800100125     d xx              s              5  0 inz
018900060419     d yy              s              3  0 inz
019000060412      * - Parametri per SQL
019100130129     d wSQL            s           5000    inz  varying
019200060418      * - Parametri per QCMDEXC
019300060418     d Qcmd            s             80    inz
019400060418     d Qlen            s             15  5 inz(80)
019500060419      * - Variabili con riferimento
019600060508     d W1Cdsi          s                   like(V1Cdsi) inz
019700060508     d W1Cdsf          s                   like(V1Cdsf) inz
019800100127     d wJJini          s                   like(JJ)     inz
019900100127     d wJJfin          s                   like(JJ)     inz
020000100127     d WjjMax          s                   like(JJ)     inz
020100100125      * - Variabili per la gestione del subfile
020200100125     d S01nrr          s              5i 0 inz
020300060412      * - Campi di comodo
020400060419     d wFile           s             10    inz
020500060419     d wLibr           s             10    inz
020600100127
020700100127       //--------------------------------------------------------------
020800100127       //?Definizione prototipi procedure.                             ?
020900100127       //--------------------------------------------------------------
021000100127
021100100127       // -?Reperimento dati utente?
021200100127      /copy gaitrasrc/srcProtoPR,XSRDA8
021300100127
021400060412      *
021500060412      *?K E Y - L I S T?----------------------------------------------*
021600060412      *
021700060419      * - file TABEL00F
021800060418     c     K03TABEL      klist
021900060418     c                   kfld                    TBLkut
022000060419     c                   kfld                    TBLcod
022100060419     c                   kfld                    TBLkey
022200060419      * - file FIAR601L
022300060419     c     K04AR601      klist
022400100127     c                   kfld                    WWWaas
022500100127     c                   kfld                    WWWlnp
022600100127     c                   kfld                    WWWnrs
022700100127     c                   kfld                    WWWnsp
022800060412      *
022900060412      *?I N D I C A T O R I?------------------------------------------*
023000060412      *                                                               *
023100060418      *  28 - Visualizza messaggio di errore a video                  *
023200060418      * N30 - SFLDSP                                                  *
023300060418      * N31 - SFLDSPCTL                                               *
023400060418      *  32 - SFLNXTCHG                                               *
023500060418      *  33 - SFLEND                                                  *
023600060418      *  51 - Data iniziale errata                                    *
023700060418      *  52 - Data  finale  errata                                    *
023800090225      *  53 - Fil. di Partenza errato                                 *
023900090225      *  54 - Fil. di Arrivo   errato                                 *
024000060508      *  55 - Codice Bolla 1   errato                                 *
024100060508      *  56 - Codice Bolla 2   errato                                 *
024200090225      *  66 - Codice Cliente   attuale   errato                       *
024300090225      *  67 - Codice Cliente sostitutivo errato                       *
024400090225      *  68 - Codice Tariffa   attuale   errato                       *
024500090225      *  69 - Codice Tariffa sostitutivo errato                       *
024600060508      *  71 - Flag stampa elenco spedizioni errato                    *
024700060412      *  90 - Generico di errore                                      *
024800060412      *===============================================================*
024900060412
025000060412     c     *Entry        plist
025100060412     c                   parm                    KPJBA
025200060412      *
025300060412      * Operazioni iniziali
025400100126     c                   exsr      sr_RoutInz
025500060412      *
025600060412      * Gestione videata
025700060419do  1c                   dow       $Fine   =  *off
025800100126     c     $Video        caseq     '1'           sr_GesD01
025900100126     c     $Video        caseq     '2'           sr_GesD02
026000100126     c     $Video        caseq     '3'           sr_GesS01
026100060418     c                   endcs
026200060418e   2c                   enddo
026300060412      *
026400060412      * Operazioni finali
026500100126     c                   exsr      sr_RoutEnd
026600060412      *
026700060412      * Fine programma
026800060419     c                   eval      *inLR   =  *on
026900060412      *
027000060412      *?S U B R O U T I N E S?----------------------------------------*
027100060412      *
027200060412      *---------------------------------------------------------------*
027300060412      *?Operazioni iniziali                                          ?*
027400060412      *---------------------------------------------------------------*
027500100126     c     sr_RoutInz    BEGSR
027600060509      *
027700060509      * Pgm. eseguibile?SOLO?da utenti EDP*
027800060509     c                   if        %subst(KNMUS:1:3) <> 'EDP'
027900060509     c                   eval      $Fine   =  *on
028000060509     c                   leavesr
028100060509     c                   endif
028200090227      /free
028300090227         // Impostazione opzioni per SQL
028400090227         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
028500090227         exec SQL   set option   CloSqlCsr = *endmod;
028600090227      /end-free
028700060412      *
028800060412      * Reperisce dati job
028900060412     c     *dtaara       define    §azute        AZUTEds
029000060412     c     *dtaara       define    §datiute      dDATIUTE
029100060412     c                   in(E)     *dtaara
029200060412     c                   if        %ERROR or RSUT = *blanks
029300060508     c                   clear                   TIBS34ds
029400060412     c                   call      'TIBS34R'
029500060508     c                   parm                    TIBS34ds
029600060412     c                   in        *dtaara
029700060412     c                   endif
029800060418      *
029900060418      * Impostazione dati "fissi"
030000060419     c                   eval      TBLkut  =  1
030100060419     c                   eval      TBLcod  =  '3A'
030200060418      *
030300060419     c                   eval      V1Tpgm  =  SDSpgm
030400060418      *
030500060418      * Esecuzione override
030600090225      * - da S.I. di filiale
030700060420if  1c                   IF        %subst(KNSIF : 1 : 3) = 'FIL'
030800060420if  2c                   if        %subst(KNSIF : 7 : 1) = 'P'
030900060419     c                   eval      wLIBR   =  'GAITRAGRPS'
031000060418x   2c                   else
031100060419     c                   eval      wLIBR   =  'GAITRAGRU '
031200060418e   2c                   endif
031300060421     c                   eval      wFILE   =  'TITAS00F  '
031400060418     c                   exsr      sr_Override
031500090225     c                   eval      wFILE   =  'TNCSB03L  '
031600090225     c                   exsr      sr_Override
031700060418e   1c                   ENDIF
031800060418      * - da S.I. di sede
031900060420if  1c                   IF        %subst(KNSIF : 1 : 3) = 'GAI'
032000060420if  2c                   if        %subst(KNSIF : 7 : 1) = 'P'
032100060419     c                   eval      wLIBR   =  'FILTRAPRD '
032200060418x   2c                   else
032300060419     c                   eval      wLIBR   =  'FILTRA201 '
032400060418e   2c                   endif
032500060421     c                   eval      wFILE   =  'FNBLP00F  '
032600060418     c                   exsr      sr_Override
032700060421     c                   eval      wFILE   =  'FNARB00F  '
032800060418     c                   exsr      sr_Override
032900060419     c                   eval      wFILE   =  'FIAR601L  '
033000060418     c                   exsr      sr_Override
033100060418e   1c                   ENDIF
033200060418      *
033300060418      * Apertura archivi
033400060418     c                   open      TITAS00F
033500090225     c                   open      TNCSB03L
033600060418     c                   open      FNBLP00F
033700060418     c                   open      FNARB00F
033800060418     c                   open      FIAR601L
033900060418      *
034000060418     c                   ENDSR
034100060418      *
034200060418      *---------------------------------------------------------------*
034300060418      *?Esecuzione singola Override                                  ?*
034400060418      *---------------------------------------------------------------*
034500060418     c     sr_Override   BEGSR
034600060418      *
034700060420     c                   eval      Qcmd = 'OVRDBF  file('
034800060420     c                                  + %trim(wFILE)
034900060420     c                                  + ') tofile('
035000060420     c                                  + %trim(wLIBR)
035100060420     c                                  + '/'
035200060420     c                                  + %trim(wFILE)
035300060420     c                                  + ')'
035400060418      *
035500060418     c                   call      'QCMDEXC'
035600060418     c                   parm                    Qcmd
035700060418     c                   parm                    Qlen
035800060418      *
035900060418     c                   ENDSR
036000060418      *
036100060418      *---------------------------------------------------------------*
036200060418      *?Gestione videata D01                                         ?*
036300060418      *---------------------------------------------------------------*
036400100126     c     sr_GesD01     BEGSR
036500060418      *
036600060418      * Inizializzazione videata
036700060419if  1c                   if        $InzD01 =  *on
036800100126     c                   exsr      sr_InzD01
036900060418     c                   movel     *off          $InzD01
037000060418e   1c                   endif
037100060418      *
037200060418      * Emissione testata
037300060509     c                   write     SDA1T01
037400060418      *
037500060418      * Emissione piede
037600060509     c                   write     SDA1Z01
037700060418      *
037800060418      * Emissione videata
037900060509     c                   exfmt     SDA1D01
038000060418     c                   setoff                                       28  90
038100060418     c                   movea     *zeros        *in(50)
038200060418     c                   clear                   V1Dmsg
038300060418      *
038400060418     c                   SELECT
038500060418      *
038600060418      * F3=Fine
038700060418w   1c                   WHEN      *inKC
038800100126     c                   exsr      sr_F03D01
038900060418     c                   leavesr
039000060418      *
039100060418      * Controllo dati immessi a video
039200060418x   1c                   OTHER
039300100126     c                   exsr      sr_CtrD01
039400060418if  2c                   if        *in90
039500060418     c                   leavesr
039600060418e   2c                   endif
039700060418      * F6=Esegue
039800060419if  2c                   if        *inKF
039900100126     c                   eval      $InzD02 =  *on
040000060419     c                   eval      $InzS01 =  *on
040100060419     c                   eval      $Video  =  '2'
040200060418e   2c                   endif
040300060418      *
040400060418e   1c                   ENDSL
040500060418      *
040600060418     c                   ENDSR
040700060418      *
040800060418      *---------------------------------------------------------------*
040900060418      *?Inizializzazione videata D01                                 ?*
041000060418      *---------------------------------------------------------------*
041100100126     c     sr_InzD01     BEGSR
041200060418      *
041300060509     c                   clear                   SDA1D01
041400060418      *
041500060418      * Imposto i dati di default
041600090225     c                   eval      V1Dlnp  =  '999 = Tutte le Fil.'
041700090225     c                   eval      V1Dlna  =  '999 = Tutte le Fil.'
041800060418      *
041900060418     c                   ENDSR
042000060418      *
042100060418      *---------------------------------------------------------------*
042200060418      *?Gestione tasto funzionale F03 da videata D01                 ?*
042300060418      *---------------------------------------------------------------*
042400100126     c     sr_F03D01     BEGSR
042500060418      *
042600060418      * Chiudo il programma
042700060419     c                   eval      $Fine   =  *on
042800060418      *
042900060418     c                   ENDSR
043000060418      *
043100060418      *---------------------------------------------------------------*
043200060418      *?Controllo dati immessi in videata D01                        ?*
043300060418      *---------------------------------------------------------------*
043400100126     c     sr_CtrD01     BEGSR
043500060418      *
043600060418     c                   movea     *zeros        *in(50)
043700060418      *
043800060418      * Controllo Data iniziale
043900060418     c                   clear                   WLBdat
044000060508     c                   eval      G08dat  =  V1Cdsi
044100060418     c                   call      'XSRDA8'
044200060419     c                   parm                    WLBdat
044300060419if  1c                   if        G08err  =  *on
044400060418     c                   seton                                        285190
044500060419     c                   eval      V1Dmsg  =  $Msg(1)
044600060418     c                   leavesr
044700060418e   1c                   endif
044800060508     c                   eval      V1Cdsi  =  G08dat
044900060508     c                   eval      W1Cdsi  =  G08inv
045000060418      *
045100060418      * Controllo Data finale
045200060508if  1c                   if        V1Cdsf  <> *zeros
045300060418     c                   clear                   WLBdat
045400060508     c                   eval      G08dat  =  V1Cdsf
045500060418     c                   call      'XSRDA8'
045600060419     c                   parm                    WLBdat
045700060421if  2c                   if        G08err  =  *on
045800060418     c                   seton                                        285290
045900060419     c                   eval      V1Dmsg  =  $Msg(1)
046000060418     c                   leavesr
046100060421e   2c                   endif
046200060508     c                   eval      V1Cdsf  =  G08dat
046300060508     c                   eval      W1Cdsf  =  G08inv
046400060421x   1c                   else
046500060508     c                   eval      W1Cdsf  =  W1Cdsi
046600060421e   1c                   endif
046700060418      *
046800060418      * Controllo range di Date
046900060508if  1c                   if        W1Cdsi  >  W1Cdsf
047000060418     c                   seton                                        285190
047100060419     c                   eval      V1Dmsg  =  $Msg(2)
047200060418     c                   leavesr
047300060418e   1c                   endif
047400060418      *
047500090225      * Controllo Fil. di Partenza
047600060508if  1c                   if        V1Clnp  <> *all'9'
047700060418     c     V1Clnp        chain     AZORG
047800060418if  2c                   if            %found(AZORG01L)
047900060418     c                             and ORGfva = *blanks
048000060419     c                   eval      V1Dlnp  =  ORGdes
048100060418x   2c                   else
048200060418     c                   seton                                        285390
048300060419     c                   eval      V1Dmsg  =  $Msg(3)
048400060418     c                   leavesr
048500060418e   2c                   endif
048600060508x   1c                   else
048700090225     c                   eval      V1Dlnp  =  '999 = Tutte le Fil.'
048800060418e   1c                   endif
048900060508      *
049000090225      * Controllo Fil. di Arrivo
049100060508if  1c                   if        V1Clna  <> *all'9'
049200060508     c     V1Clna        chain     AZORG
049300060508if  2c                   if            %found(AZORG01L)
049400060508     c                             and ORGfva = *blanks
049500060508     c                   eval      V1Dlna  =  ORGdes
049600060508x   2c                   else
049700060508     c                   seton                                        285490
049800060508     c                   eval      V1Dmsg  =  $Msg(4)
049900060508     c                   leavesr
050000060508e   2c                   endif
050100060508x   1c                   else
050200090225     c                   eval      V1Dlna  =  '999 = Tutte le Fil.'
050300060508e   1c                   endif
050400060418      *
050500060418      * Controllo codici Bolla
050600090225      /free
050700090225           FOR  xx = 1  TO  %elem($V1Ccbx);
050800090225
050900090225             If  $V1Ccbx(xx) <> *blank;
051000090225
051100090225               TBLkey = $V1Ccbx(xx);
051200090225               chain  k03tabel  TABEL;
051300090225
051400090302               if  Not %found(TABEL00F)   or   TBLflg <> *blank;
051500090225                 *in28 = *on;
051600090225                 *in90 = *on;
051700090225                 *in(54+xx) = *on;
051800090225                 V1Dmsg = $Msg(5);
051900090225                 leavesr;
052000090225               endif;
052100090225
052200090225             EndIf;
052300090225
052400090225           ENDFOR;
052500090225
052600090225       /end-free
052700060418      *
052800060418      * Controllo codice Cliente esistente
052900060508sel 1c                   SELECT
053000060508w   1c                   WHEN      V1Ckso  =  *zeros
053100090225     c                   seton                                        286690
053200060508     c                   eval      V1Dmsg  =  $Msg(6)
053300060418     c                   leavesr
053400060508w   1c                   WHEN           %subst(%editc(V1Ckso:'Z'):4:4)
053500060508     c                                     =  *all'8'
053600060508     c                             or   %subst(%editc(V1Ckso:'Z'):4:4)
053700060508     c                                     =  *all'9'
053800090225     c                   seton                                        286690
053900060508     c                   eval      V1Dmsg  =  $Msg(6)
054000060508     c                   leavesr
054100060508x   1c                   OTHER
054200060418     c                   clear                   TIBS69ds
054300060419     c                   eval      I69kac  =  V1Ckso
054400060418     c                   call      'TIBS69R'
054500060418     c                   parm                    TIBS69ds
054600060508     c                   parm                    ds_CNACO
054700060508     c                   parm                    ds_CNIND
054800060508     c                   parm                    ds_CNCLP
054900060508     c                   parm                    ds_FNCLS
055000060419if  2c                   if        O69err  =  *on
055100090225     c                   seton                                        286690
055200060508     c                   eval      V1Dmsg  =  $Msg(6)
055300060418     c                   leavesr
055400060418x   2c                   else
055500060418     c                   movel     ACOrag        V1Dkso
055600060418e   2c                   endif
055700060508e   1c                   ENDSL
055800060418      *
055900060418      * Controllo codice Cliente sostitutivo
056000060508sel 1c                   SELECT
056100060508w   1c                   WHEN      V1Cksn  =  *zeros
056200090225     c                   seton                                        286790
056300060508     c                   eval      V1Dmsg  =  $Msg(6)
056400060418     c                   leavesr
056500060508w   1c                   WHEN           %subst(%editc(V1Cksn:'Z'):4:4)
056600060508     c                                     =  *all'8'
056700060508     c                             or   %subst(%editc(V1Cksn:'Z'):4:4)
056800060508     c                                     =  *all'9'
056900090225     c                   seton                                        286790
057000060508     c                   eval      V1Dmsg  =  $Msg(6)
057100060508     c                   leavesr
057200060508x   1c                   OTHER
057300060418     c                   clear                   TIBS69ds
057400060419     c                   eval      I69kac  =  V1Cksn
057500060418     c                   call      'TIBS69R'
057600060418     c                   parm                    TIBS69ds
057700060508     c                   parm                    ds_CNACO
057800060508     c                   parm                    ds_CNIND
057900060508     c                   parm                    ds_CNCLP
058000060508     c                   parm                    ds_FNCLS
058100060419if  2c                   if        O69err  =  *on
058200090225     c                   seton                                        286790
058300060508     c                   eval      V1Dmsg  =  $Msg(6)
058400060418     c                   leavesr
058500060418x   2c                   else
058600060418     c                   movel     ACOrag        V1Dksn
058700060418e   2c                   endif
058800060508e   1c                   ENDSL
058900060419      *
059000060419      * Controllo codice tariffa esistente
059100060421      * (non obbligatorio)
059200060421if  1c                   if        V1Ccto  <> *blanks
059300100126     c     c_Digits      check     V1Ccto                                 10
059400060421if  2c                   if        *in10
059500090225     c                   seton                                        286890
059600060508     c                   eval      V1Dmsg  =  $Msg(7)
059700060421     c                   leavesr
059800060421e   2c                   endif
059900060421e   1c                   endif
060000060419      *
060100060419      * Controllo codice tariffa sostitutivo
060200060421      * (obbligatorio se immesso il codice tariffa esistente)
060300060419if  1c                   if             V1Ccto <> *blanks
060400060419     c                             and  V1Cctn  = *blanks
060500090225     c                   seton                                        286990
060600060508     c                   eval      V1Dmsg  =  $Msg(8)
060700060419     c                   leavesr
060800060419e   1c                   endif
060900120427if  1c**!!!              if             V1Cctn <> *blanks
061000120427     c**!!!                        and  V1Ccto  =  V1Cctn
061100120427     c**!!!              seton                                        286890
061200120427     c**!!!              eval      V1Dmsg  =  $Msg(9)
061300120427     c**!!!              leavesr
061400120427e   1c**!!!              endif
061500120427if  1c                   if             V1Cctn <> *blanks
061600120427     c                             and  V1Ccto  = *blanks
061700120427     c                   seton                                        286890
061800120427     c                   eval      V1Dmsg  =  $Msg(9)
061900120427     c                   leavesr
062000120427e   1c                   endif
062100060421if  1c                   if        V1Cctn  <> *blanks
062200100126     c     c_Digits      check     V1Cctn                                 10
062300060421if  2c                   if        *in10
062400090225     c                   seton                                        286990
062500060508     c                   eval      V1Dmsg  =  $Msg(7)
062600060421     c                   leavesr
062700060421e   2c                   endif
062800060421e   1c                   endif
062900060418      *
063000060418     c                   ENDSR
063100060419      *
063200060419      *---------------------------------------------------------------*
063300060419      *?Gestione videata D02                                         ?*
063400060419      *---------------------------------------------------------------*
063500100126     c     sr_GesD02     BEGSR
063600060419      *
063700060419      * Inizializzazione videata
063800100126if  1c                   if        $InzD02 =  *on
063900100126     c                   exsr      sr_InzD02
064000100126     c                   movel     *off          $InzD02
064100060419e   1c                   endif
064200060419      *
064300060421if  1c                   if        NOT *in90
064400060419      * Emissione testata
064500060509     c                   write     SDA1T01
064600060419      * Emissione piede
064700060509     c                   write     SDA1Z02
064800060421      * Emissione videata precedente (filtro di lancio)
064900060509     c                   write     SDA1D01
065000060421     c                   write     Protect
065100100125      /free
065200100126         else;
065300100126           *in28 = *off;
065400100126           *in90 = *off;
065500100126           write SDA1T01;
065600100126           write SDA1Z02;
065700100126           write SDA1D01;
065800100126           write Protect;
065900100126           *in28 = *on;
066000100126           *in90 = *on;
066100100126      /end-free
066200060421e   1c                   endif
066300060419      *
066400060419      * Emissione videata
066500060509     c                   exfmt     SDA1D02
066600060419     c                   setoff                                       28  90
066700060419     c                   movea     *zeros        *in(50)
066800060419     c                   clear                   V1Dmsg
066900060419      *
067000060419     c                   SELECT
067100060419      *
067200100126      * F3 = Fine
067300060419w   1c                   WHEN      *inKC
067400100126     c                   exsr      sr_F03D01
067500060419      *
067600100126      * F12 = Ritorno
067700060419w   1c                   WHEN      *inKL
067800100126     c                   exsr      sr_F12D02
067900060419      *
068000100126      * F21 = Elenco LdV
068100060419w   1c                   WHEN      *inKV
068200100126     c                   exsr      sr_F21D02
068300060419      *
068400100126      * F6 = Esegue
068500060419w   2c                   WHEN      *inKF
068600100126     c                   exsr      sr_F06D02
068700060419      *
068800060419e   1c                   ENDSL
068900060419      *
069000060419     c                   ENDSR
069100060419      *
069200060419      *---------------------------------------------------------------*
069300060419      *?Gestione tasto funzionale F12 da videata D02                 ?*
069400060419      *---------------------------------------------------------------*
069500100126     c     sr_F12D02     BEGSR
069600060419      *
069700060419      * Ritorno alla videata precedente
069800060419     c                   eval      $Video  =  '1'
069900060419     c                   reset                   $InzS01
070000060419      *
070100060419     c                   ENDSR
070200060419      *
070300060419      *---------------------------------------------------------------*
070400060419      *?Gestione tasto funzionale F21 da videata D02                 ?*
070500060419      *---------------------------------------------------------------*
070600100126     c     sr_F21D02     BEGSR
070700060419      *
070800060419     c                   eval      $Video  =  '3'
070900100126     c                   eval      $InzS01 = *on
071000060419      *
071100060419     c                   ENDSR
071200100126
071300100126      /free
071400100126
071500100126       //--------------------------------------------------------------
071600100126       //?Inizializzazione videata D02                                 ?
071700100126       //--------------------------------------------------------------
071800100126       BEGSR  sr_InzD02;
071900100126
072000100126         // - Pulizia D02 (totali)
072100100126         clear  SDA1D02;
072200100126
072300100126         clear  V1Dmsg;
072400100126         *in28 = *off;
072500100126         *in90 = *off;
072600100126
072700100126         // - Caricamento dati (in schiere)
072800100127         clear  jj;
072900100126         reset  $Sped;
073000100126         reset  $nrrBLP;
073100100126         reset  $nrrARB;
073200100126         reset  $nrrTAS;
073300100126         reset  $SpedAR6;
073400100126         reset  $SpedCSB;
073500100128         reset  $AltreSped;
073600100126
073700130129         clear  wSQL;
073800100126         clear  yy;
073900100126
074000100126         For  yy = 1  To  %elem($File);
074100100126           exsr  sr_SQL;
074200100126         EndFor;
074300130129
074400130129         wSQL  += ' ' + c_OrderBy + ' ' + c_ForFetch;
074500130129
074600130129         reset  $EoF;
074700130129         exsr  sr_ExecSQL;
074800100127
074900100127         WjjMax = jj;
075000100128
075100100128         // - (Dis)Abilitazione F6=Conferma
075200100128         *in30 = (WjjMax = *zeros);
075300100126
075400100126       ENDSR;
075500100126
075600100126       //--------------------------------------------------------------
075700100126       //?Gestione singolo record estratto (D02)                       ?
075800100126       //--------------------------------------------------------------
075900100126       BEGSR  sr_RieD02;
076000100127
076100100127         // - Preparazione struttura dati ds_Sped
076200100127         clear  ds_Sped;
076300100127         wwwAAS = SQLaas;
076400100127         wwwLNP = SQLlnp;
076500100127         wwwNRS = SQLnrs;
076600100127         wwwNSP = SQLnsp;
076700100126
076800100126         // - Verifica se spedizione già elaborata su un altro file
076900100127         //   Se NO: memorizzazione spedizione
077000100127         //jj = %lookup( ds_Sped : $Sped );
077100100127         //if  jj = *zero;
077200100127         //  jj = %lookup( *blank : $Sped );
077300100127         //  *inH1 = (jj = *zero);            //  <«=-*?"H1"?
077400100127         //  $Sped(jj) = ds_Sped;
077500100127         //endif;
077600100127
077700100127         // - Verifica se raggiunto il n° max di spedizioni elaborabili
077800100127         if  jj = %elem($Sped);
077900100127           *inH1 = *on;                       //  <«=-*?"H1"?
078000100127           $EoF  = *on;
078100100128           yy = *hival;
078200100127           $AltreSped = *on;
078300100128           // - impostazione messaggio da emettere a video
078400100128           *in28 = *on;
078500100128           *in90 = *on;
078600100128           V1Dmsg = $Msg(10);
078700100128           // - uscita
078800100127           leavesr;
078900100127         endif;
079000100127
079100100127         // - Memorizzazione spedizione
079200130129         //   (se nuova, non da archivio diverso)
079300130129         if  jj = *zero  or  ds_Sped <> $Sped(jj);
079400130129           jj += 1;
079500130129           $Sped(jj) = ds_Sped;
079600130129         endif;
079700100126
079800100126         // - Memorizzazione NRR del file da cui stato estratto il rec.
079900100126         //   & Conteggio dei record estratti
080000100126         select;
080100130129           when  SQLfile = '1FNBLP';
080200100126             $nrrBLP(jj) = SQLrrn;
080300100126             V2Cblp += 1;
080400100127             exsr  sr_CtrFIAR6;
080500130129           when  SQLfile = '2FNARB';
080600100126             $nrrARB(jj) = SQLrrn;
080700100126             V2Carb += 1;
080800100127             exsr  sr_CtrFIAR6;
080900130129           when  SQLfile = '3TITAS';
081000100126             $nrrTAS(jj) = SQLrrn;
081100100126             V2Ctas += 1;
081200100127             if  V1Ckso <> V1Cksn;
081300100127               exsr  sr_CtrTNCSB;
081400100127             endif;
081500100126         endsl;
081600100126
081700100126       ENDSR;
081800100126
081900100126      /end-free
082000060418      *
082100060418      *---------------------------------------------------------------*
082200060418      *?Gestione videata C01/S01                                     ?*
082300060418      *---------------------------------------------------------------*
082400100126     c     sr_GesS01     BEGSR
082500060418      *
082600060418      * Inizializzazione videata
082700060419if  1c                   if        $InzS01 =  *on
082800100127     c*//                exsr      sr_InzS01
082900100127     c                   exsr      sr_RollUpS01
083000100127     c                   movel     *off          $InzS01
083100060418e   1c                   endif
083200060418      *
083300060420if  1c                   if        NOT *in90
083400060418      * Emissione testata
083500060509     c                   write     SDA1T01
083600060418      * Emissione piede
083700060509     c                   write     SDA1Z03
083800060420e   1c                   endif
083900060508      *
084000060508      * Posizionamento cursore
084100100125     c                   if        C01csr  >  *zeros
084200100125     c                   eval      C01rcd  =  C01csr
084300100125     c                   endif
084400060418      *
084500060418      * Emissione videata
084600060509     c                   exfmt     SDA1C01
084700060418     c                   setoff                                       28  90
084800060418     c                   movea     *zeros        *in(50)
084900060418     c                   clear                   V1Dmsg
085000060418      *
085100060420sel 1c                   SELECT
085200060418      *
085300100126      * F3 = Fine
085400060418w   1c                   WHEN      *inKC
085500100126     c                   exsr      sr_F03D01
085600060418      *
085700100126      * F12 = Ritorno (a D02, non a D01)
085800060419w   1c                   WHEN      *inKL
085900100126     c                   exsr      sr_F12S01
086000060418      *
086100060418      * Controllo opzioni immesse a video
086200060418x   1c                   OTHER
086300100126     c                   exsr      sr_OpzS01
086400060420if  2c                   if        *in90
086500060420     c                   leavesr
086600060420e   2c                   endif
086700060428sel 2c                   select
086800100126      * RollUp = Pagina successiva
086900100127w   2c                   when      £Tasto = c_RollUp
087000100127     c                   exsr      sr_RollUpS01
087100100126      * RollDown = Pagina precedente
087200100127w   2c                   when      £Tasto = c_RollDown
087300100127     c                   exsr      sr_RollDownS01
087400100126      * F6 = Esegue
087500060428w   2c                   when      *inKF
087600100127     c                   exsr      sr_F06D02
087700060428e   2c                   endsl
087800060418      *
087900060418e   1c                   ENDSL
088000060418      *
088100060418     c                   ENDSR
088200060418      *
088300060418      *---------------------------------------------------------------*
088400100127      *?Inizializzazione videata C01/S01                             ?*
088500060418      *---------------------------------------------------------------*
088600100126     c     sr_InzS01     BEGSR
088700100126      *
088800100126      * Pulizia subfile-control
088900100126if  1c                   if        $InzS01 = *on
089000100126     c                   clear                   SDA1C01
089100100126     c                   eval      C1Cdsi  =  V1Cdsi
089200100126     c                   if        V1Cdsf  >  *zeros
089300100126     c                   eval      C1Cdsf  =  V1Cdsf
089400100126     c                   else
089500100126     c                   eval      C1Cdsf  =  C1Cdsi
089600100126     c                   endif
089700100127     c                   eval      C01pgt  =  %div( WjjMax : c_SflPag )
089800100127     c                   if        %rem( WjjMax : c_SflPag ) > *zero
089900100127     c                   eval      C01pgt += 1
090000100127     c                   endif
090100100126     c                   clear                   jj
090200100126e   1c                   endif
090300060418      *
090400060418      * Pulizia subfile
090500060418     c                   seton                                        3031
090600060509     c                   write     SDA1C01
090700060418     c                   setoff                                         3133
090800060418      *
090900060418     c                   eval      *in32   =  *off
091000100125     c                   clear                   C01rcd
091100100125     c                   clear                   C01csr
091200060418     c                   clear                   S01nrr
091300100126      *
091400100126     c                   ENDSR
091500100126
091600100126      /free
091700100126
091800100126       //--------------------------------------------------------------
091900100126       //?Paginazione avanti nel subfile (S01)                         ?
092000100126       //--------------------------------------------------------------
092100100127       BEGSR  sr_RollUpS01;
092200100126
092300100126         exsr  sr_InzS01;
092400100126
092500100127         C01pgv += 1;
092600100127         jj     =  ( ( C01pgv - 1 ) * c_SflPag );
092700100126         clear  wJJini;
092800100126         clear  wJJfin;
092900100126
093000100126         For  S01nrr = 1  To  c_SflPag;
093100100126
093200100126           jj += 1;
093300100126
093400100126           // - Fine delle spedizioni caricate
093500100127           if  $Sped ( jj ) <= *zero;
093600100126             S01nrr -= 1;
093700100126             jj     -= 1;
093800100126             leave;
093900100126           endif;
094000100126
094100100126           if  S01nrr = 1;
094200100126             wJJini = jj;
094300100126           endif;
094400100126           wJJfin = jj;
094500100126
094600100126           exsr  sr_RieS01;
094700100126
094800100126         EndFor;
094900100126
095000100126         // - Visualizzazione del SFL (se ci sono dati)
095100100126         *in30 = (S01nrr = *zeros);
095200100126
095300100126         // - Attivazione del SFLEND
095400100127         *in33 = ( $Sped ( wJJfin + 1 ) <= *zero );
095500100126
095600100126         // - (Dis)Attivazione RollDown
095700100127         *in34 = ( wJJini <= 1 );
095800100126
095900100126         // - Posizionamento cursore al 1° rcd della pagina
096000100126         if  S01nrr > *zero;
096100100126           C01rcd = 1;
096200100126         else;
096300100126           clear  C01rcd;
096400100126         endif;
096500100126         C01csr = C01rcd;
096600100126
096700100126       ENDSR;
096800100126
096900100126       //--------------------------------------------------------------
097000100126       //?Paginazione indietro nel subfile (S01)                       ?
097100100126       //--------------------------------------------------------------
097200100127       BEGSR  sr_RollDownS01;
097300100126
097400100126         exsr  sr_InzS01;
097500100126
097600100127         C01pgv -= 1;
097700100127         jj     =  ( ( C01pgv - 1 ) * c_SflPag );
097800100126         clear  wJJini;
097900100126         clear  wJJfin;
098000100126
098100100126         For  S01nrr = 1  To  c_SflPag;
098200100126
098300100126           jj += 1;
098400100126
098500100126           // - Fine delle spedizioni caricate
098600100127           if  $Sped ( jj ) <= *zero;
098700100126             S01nrr -= 1;
098800100126             jj     -= 1;
098900100126             leave;
099000100126           endif;
099100100126
099200100126           if  S01nrr = 1;
099300100126             wJJini = jj;
099400100126           endif;
099500100126           wJJfin = jj;
099600100126
099700100126           exsr  sr_RieS01;
099800100126
099900100126         EndFor;
100000100126
100100100126         // - Visualizzazione del SFL (se ci sono dati)
100200100126         *in30 = (S01nrr = *zeros);
100300100126
100400100126         // - Attivazione del SFLEND
100500100127         *in33 = ( $Sped ( wJJfin + 1 ) <= *zero );
100600100126
100700100126         // - (Dis)Attivazione RollDown
100800100127         *in34 = ( wJJini <= 1 );
100900100126
101000100126         // - Posizionamento cursore al 1° rcd della pagina
101100100126         if  S01nrr > *zero;
101200100126           C01rcd = 1;
101300100126         else;
101400100126           clear  C01rcd;
101500100126         endif;
101600100126         C01csr = C01rcd;
101700100126
101800100126       ENDSR;
101900100126
102000100126       //--------------------------------------------------------------
102100100126       //?Gestione singolo record estratto (C01/S01)                   ?
102200100126       //--------------------------------------------------------------
102300100126       BEGSR  sr_RieS01;
102400100126
102500100126         // - Riempimento campi nel record del subfile
102600100126
102700100126         clear  SDA1S01;
102800100126
102900100126         ds_Sped = $Sped(jj);
103000100126         VH1jj  = jj;
103100100127         VH1aas = WWWaas;
103200130129         VS1lnp = WWWlnp;
103300100127         VS1nrs = WWWnrs;
103400100127         VS1nsp = WWWnsp;
103500130129         if  $nrrBLP(jj) <> *zero;
103600130129           VH1rrnP = $nrrBLP(jj);
103700130129         endif;
103800130129         if  $nrrARB(jj) <> *zero;
103900130129           VH1rrnA = $nrrARB(jj);
104000130129         endif;
104100130129         if  $nrrTAS(jj) <> *zero;
104200130129           VH1rrnS = $nrrTAS(jj);
104300130129         endif;
104400100127
104500130129         if  VH1rrnP < *zero  or  VH1rrnA < *zero  or  VH1rrnS < *zero;
104600100127           VS1opz = 'E';
104700100127         endif;
104800100126
104900100127         reset  WLBdat;
105000100126
105100130129         // - dati da FNBLP00F
105200130129         if  $nrrBLP(jj) <> *zero;
105300130129           %subst( VS1PF : 1 : 1 ) = 'P';
105400130129           SQLrrn = %abs($nrrBLP(jj));
105500130129           chain(n)  SQLrrn  FNBLP000;
105600130129           if  %found(FNBLP00F);
105700130129             VS1lna = BLPlna;
105800130129             VS1ksc = BLPksc;
105900130129             G08inv = (BLPaas * 10000) + BLPmgs;
106000130129           endif;
106100130129         endif;
106200100126
106300130129         // - dati da FNARB00F
106400130129         if  $nrrARB(jj) <> *zero;
106500130129           %subst( VS1PF : 2 : 1 ) = 'A';
106600130129           if  %subst( VS1PF : 1 : 1 ) = *blank  or  VS1lna = *zero;
106700100127             SQLrrn = %abs($nrrARB(jj));
106800100127             chain(n)  SQLrrn  FNARB000;
106900100126             if  %found(FNARB00F);
107000100126               VS1lna = ARBlna;
107100100126               VS1ksc = ARBksc;
107200100126               G08inv = (ARBaas * 10000) + ARBmgs;
107300100126             endif;
107400130129           endif;
107500130129         endif;
107600100126
107700100126           // - dati da TITAS00F
107800130129         if  $nrrTAS(jj) <> *zero;
107900130129           %subst( VS1PF : 3 : 1 ) = 'S';
108000130129           if  %subst( VS1PF : 1 : 1 ) = *blank  or  VS1lna = *zero;
108100100127             SQLrrn = %abs($nrrTAS(jj));
108200100127             chain(n)  SQLrrn  TITAS00F;
108300100127             if  %found(TITAS00F);
108400100126               VS1lna = TASlna;
108500100126               VS1ksc = TASksc;
108600100126               G08inv = (TASaas * 10000) + TASmgs;
108700100126             endif;
108800130129           endif;
108900130129         endif;
109000100126
109100130129         // - Inversione data spedizione
109200100126         XSRDA8 ( WLBdat );
109300130131         VS1dts = G08dat;
109400130129
109500100126         // - Decodifica cliente
109600100126         exsr  sr_CallTIBS69;
109700100127         VS1rsc = %subst ( ACOrag : 1 : %len(VS1rsc) );
109800100126
109900100126         // - Scrittura record subfile
110000100126         write  SDA1S01;
110100100126
110200100126       ENDSR;
110300100126
110400100126      /end-free
110500100126      *
110600100126      *---------------------------------------------------------------*
110700101130      *?Reperimento dati anagrafici del cliente                      ?*
110800100126      *---------------------------------------------------------------*
110900100126     c     sr_CallTIBS69 BEGSR
111000100126      *
111100100126if  1c                   if        VS1ksc <> I69kac
111200100126     c                   clear                   TIBS69ds
111300100126     c                   z-add     VS1ksc        I69kac
111400100126     c                   call      'TIBS69R'
111500100126     c                   parm                    TIBS69ds
111600100126     c                   parm                    ds_CNACO
111700100126     c                   parm                    ds_CNIND
111800100126     c                   parm                    ds_CNCLP
111900100126     c                   parm                    ds_FNCLS
112000100126e   1c                   endif
112100100126      *
112200100126     c                   ENDSR
112300060421      *
112400060421      *---------------------------------------------------------------*
112500060421      *?Reperimento dati da FIAR601L (tassazione bolla)              ?*
112600060421      *---------------------------------------------------------------*
112700100126     c     sr_CtrFIAR6   BEGSR
112800060421      *
112900060421      * Verifica se spedizione già elaborata (da altro file)
113000101130     c                   eval      ww = %lookup( ds_Sped : $Sped )
113100101130     c                   if                 ww  > *zero  and
113200101130     c                             $SpedAR6(ww) > *zero
113300060421     c                   leavesr
113400060421     c                   endif
113500060421      *
113600060421      * Verifica se spedizione con dati relativi alla tassazione
113700060421     c     K04AR601      setll     FIAR6000
113800060421     c     K04AR601      reade     FIAR6000
113900060421      *
114000060421     c                   DOW       NOT %eof(FIAR601L)
114100060421      *
114200060421     c                   add       1             V2Car6
114300100126     c                   add       1             $SpedAR6(jj)
114400060421      *
114500060421     c     K04AR601      reade     FIAR6000
114600060421      *
114700060421     c                   ENDDO
114800060421      *
114900060421     c                   ENDSR
115000090225      /free
115100090225       //--------------------------------------------------------------
115200090225       //?Reperimento dati da TNCSB03L (contrassegni bolle)            ?
115300090225       //--------------------------------------------------------------
115400100126       BEGSR  sr_CtrTNCSB;
115500090225
115600130129         // - Verifica se spedizione già elaborata
115700130129         ww = %lookup( ds_Sped : $Sped );
115800130129         if  ww  > *zero  and  $SpedCSB(ww) = *on;
115900101130           leavesr;
116000101130         endif;
116100100126
116200130129         // - Verifica se spedizione con dati relativi alla tassazione
116300130129         //   (ed eventualmente la memorizza)
116400090227         chain(n)  K04AR601  TNCSB000;
116500090227         if  %found(TNCSB03L)   and   CSBbna = *zero   and
116600090227             (CSBksc = V1Ckso   or    CSBcdi = V1Ckso);
116700100126           V2Ccsb      += 1;
116800100126           $SpedCSB(jj) = *on;
116900090227         endif;
117000090225
117100090225       ENDSR;
117200090225
117300090225      /end-free
117400060418      *
117500060418      *---------------------------------------------------------------*
117600060418      *?Gestione opzioni videata C01/S01                             ?*
117700060418      *---------------------------------------------------------------*
117800100126     c     sr_OpzS01     BEGSR
117900060418      *
118000060509     c                   readc     SDA1S01
118100060420      *
118200100315do  1c                   DOW       NOT %eof(TNSDA1D)
118300060419      *
118400100125     c                   z-add     S01nrr        C01rcd
118500100127      /free
118600100127
118700100127           Select;
118800100127
118900100127             //- Opzione non variata
119000130129             When  VS1opz = *blank   and   VH1rrnP >= *zero
119100130129                                     and   VH1rrnA >= *zero
119200130129                                     and   VH1rrnS >= *zero;
119300100127               *in32  = *off;                              // NO sflnxtchg
119400130129             When  VS1opz = 'E'      and   VH1rrnP <= *zero
119500130129                                     and   VH1rrnA <= *zero
119600130129                                     and   VH1rrnS <= *zero;
119700100127               *in32  = *off;                              // NO sflnxtchg
119800100127
119900100127             // - Immessa "E"
120000130129             When  VS1opz = 'E'      and   (VH1rrnP > *zero  or
120100130129                                            VH1rrnA > *zero  or
120200130129                                            VH1rrnS > *zero);
120300100127               *in32  = *on;                               // SÌ sflnxtchg
120400130131               if  VH1rrnP > *zero;
120500130131                 VH1rrnP *= -1;
120600130131                 $nrrBLP(VH1jj) *= -1;
120700130131                 V2Cblp -= 1;
120800130129               endif;
120900130131               if  VH1rrnA > *zero;
121000130131                 VH1rrnA *= -1;
121100130131                 $nrrARB(VH1jj) = %abs( $nrrARB(VH1jj) ) * (-1);
121200130131                 V2Carb -= 1;
121300130129               endif;
121400130131               if  VH1rrnS > *zero;
121500130131                 VH1rrnS *= -1;
121600130131                 $nrrTAS(VH1jj) = %abs( $nrrTAS(VH1jj) ) * (-1);
121700130131                 V2Ctas -= 1;
121800130129               endif;
121900100127               if  $SpedAR6(VH1jj) > *zero;
122000130131                 V2Car6 -= $SpedAR6(VH1jj);
122100100127               endif;
122200100127               if  $SpedCSB (VH1jj) > *zero;
122300130131                 V2Ccsb -= 1;
122400100127               endif;
122500100127
122600100127             // - Tolta "E"
122700130129             When  VS1opz = *blank   and   (VH1rrnP < *zero  or
122800130129                                            VH1rrnA < *zero  or
122900130129                                            VH1rrnS < *zero);
123000100127               *in32  = *on;                               // SÌ sflnxtchg
123100130131               if  VH1rrnP < *zero;
123200130131                 VH1rrnP = %abs(VH1rrnP);
123300130129                 $nrrBLP(VH1jj) = %abs($nrrBLP(VH1jj));
123400130131                 V2Cblp += 1;
123500130129               endif;
123600130131               if  VH1rrnA < *zero;
123700130131                 VH1rrnA = %abs(VH1rrnA);
123800130129                 $nrrARB(VH1jj) = %abs($nrrARB(VH1jj));
123900130131                 V2Carb += 1;
124000130129               endif;
124100130131               if  VH1rrnS < *zero;
124200130131                 VH1rrnS = %abs(VH1rrnS);
124300130129                 $nrrTAS(VH1jj) = %abs($nrrTAS(VH1jj));
124400130131                 V2Ctas += 1;
124500130129               endif;
124600100127               if  $SpedAR6(VH1jj) > *zero;
124700130131                 V2Car6 += $SpedAR6(VH1jj);
124800100127               endif;
124900100127               if  $SpedCSB (VH1jj) > *zero;
125000130131                 V2Ccsb += 1;
125100100127               endif;
125200100127
125300100127e   2      EndSl;
125400100127
125500100127      /end-free
125600060420      *
125700060420      * - aggiorno il SFL
125800060509     c                   update    SDA1S01
125900060420      * - se rilevato errore esco dal ciclo di controllo
126000100125if  2c                   if        *in28   = *on
126100100125     c                   z-add     C01rcd        C01csr
126200100125     c                   leave
126300100125e   2c                   endif
126400060420      *
126500060509     c                   readc     SDA1S01
126600060419      *
126700060420e   1c                   ENDDO
126800060418      *
126900060420     c                   ENDSR
127000100127
127100100127      /free
127200100127
127300100127       //--------------------------------------------------------------
127400100127       //?Gestione tasto funzionale F6 da videata C01/S01              ?
127500100127       //--------------------------------------------------------------
127600100127       BEGSR  sr_F06D02;
127700100127
127800100127         // - Ciclo di elaborazione subfile per aggiornamento archivi
127900100127         For  jj = 1  To  %elem($Sped);
128000100127
128100100127           // - Fine spedizioni da elaborare
128200100127           if  $Sped(jj) <= *zero;
128300100127             leave;
128400100127           endif;
128500100127
128600100127           // - Spedizione esclusa ("E") dall'aggiornamento
128700100127           if  $nrrBLP(jj) < *zero   or
128800100127               $nrrARB(jj) < *zero   or
128900100127               $nrrTAS(jj) < *zero;
129000100127             iter;
129100100127           endif;
129200100127
129300100127           ds_Sped = $Sped(jj);
129400100127
129500100127           exsr  sr_UpdateSped;
129600100127
129700100127         EndFor;
129800100127
129900100127         if  Not $AltreSped;
130000100127           // - Imposta ritorno alla 1ª videata
130100100127           $Video  = '1';
130200100127           $InzD01 = *on;
130300100127         else;
130400100127           // - Imposta ritorno alla 2ª videata
130500100127           $Video  = '2';
130600100127         endif;
130700100127         $InzD02 = *on;
130800100127         $InzS01 = *on;
130900100127
131000100127       ENDSR;
131100100127
131200100127      /end-free
131300060419      *
131400060419      *---------------------------------------------------------------*
131500060508      *?Gestione tasto funzionale F12 da videata C01/S01             ?*
131600060419      *---------------------------------------------------------------*
131700100126     c     sr_F12S01     BEGSR
131800060419      *
131900060419     c                   eval      $Video  =  '2'
132000060419      *
132100060419     c                   ENDSR
132200060418      *
132300060418      *---------------------------------------------------------------*
132400060418      *?Gestione SQL per singolo archivio (TITAS/FNBLP/FNARB)        ?*
132500060418      *---------------------------------------------------------------*
132600060418     c     sr_SQL        BEGSR
132700060418      *
132800130129if  1c                   if        yy > 1
132900130129     c                   eval      wSQL += ' UNION '
133000130129e   1c                   endif
133100130129      *
133200060418sel 1c                   select
133300060418      *
133400060418      * Gestione file FNBLP00F
133500060419w   1c                   when      $File(yy) = 'FNBLP00F  '
133600100126     c                   exsr      sr_PrepSqlBLP
133700060418      *
133800060418      * Gestione file FNARB00F
133900060419w   1c                   when      $File(yy) = 'FNARB00F  '
134000100126     c                   exsr      sr_PrepSqlARB
134100060421      *
134200060421      * Gestione file TITAS00F
134300060421w   1c                   when      $File(yy) = 'TITAS00F  '
134400100126     c                   exsr      sr_PrepSqlTAS
134500060418      *
134600060418e   1c                   endsl
134700060418      *
134800060418     c                   ENDSR
134900060412      *
135000060412      *---------------------------------------------------------------*
135100060418      *?Preparazione stringa SQL per estrazione dati da TITAS00F     ?*
135200060412      *---------------------------------------------------------------*
135300100126     c     sr_PrepSqlTAS BEGSR
135400060412      *
135500060419     c                   clear                   xx
135600060412      *
135700060508      * Impostazione delle selezioni (se immesse)
135800130129for 1c                   FOR       xx = 1  TO  %elem($Sql_TITAS)
135900060419      *
136000060508if  2c                   if
136100060508      * - Data Spedizione (range)
136200130129     c*** obbligatorio:                (xx     =  4
136300060508     c***       "                  and  V1Cdsi =  *zeros
136400060508     c***       "                  and  V1Cdsf =  *zeros)
136500060508     c***                          OR
136600090225      * - Fil. di Partenza
136700130129     c                                 (xx     =  5
136800060508     c                             and  V1Clnp =  *all'9')
136900090225      * - Fil. di Arrivo
137000130129     c                             OR  (xx     =  6
137100060508     c                             and  V1Clna =  *all'9')
137200060508      * - Codice Bolla 1 e 2 (elenco)
137300130129     c                             OR  (xx     =  7
137400090227     c                             and  ds_V1Ccbx = *blanks)
137500090225      * - Codice Cliente attuale - TASksc / TASccm
137600130129     c*** obbligatorio:            OR  (xx     =  8
137700060508     c***       "                  and  V1Ckso =  *zeros)
137800060508      * - Codice Tariffa attuale
137900130129     c                             OR  (xx     =  9
138000060419     c                             and  V1Ccto =  *blanks)
138100060419     c                   iter
138200060508e   2c                   endif
138300060419      *
138400130129     c                   eval      wSQL   +=  ' '
138500060418     c                                     +  %trim($Sql_TITAS(xx))
138600130129if  2c                   if        xx      >= 4
138700100126     c                   exsr      sr_Add_Selez
138800060419e   2c                   endif
138900060419      *
139000130129e   1c                   ENDFOR
139100060412      *
139200060412     c                   ENDSR
139300060418      *
139400060418      *---------------------------------------------------------------*
139500060418      *?Preparazione stringa SQL per estrazione dati da FNBLP00F     ?*
139600060418      *---------------------------------------------------------------*
139700100126     c     sr_PrepSqlBLP BEGSR
139800060418      *
139900060419     c                   clear                   xx
140000060418      *
140100060508      * Impostazione delle selezioni (se immesse)
140200130129for 1c                   FOR       xx = 1  TO  %elem($Sql_FNBLP)
140300060419      *
140400060508if  2c                   if
140500060508      * - Data Spedizione (range)
140600130129     c*** obbligatorio:                (xx     =  4
140700060508     c***       "                  and  V1Cdsi =  *zeros
140800060508     c***       "                  and  V1Cdsf =  *zeros)
140900060508     c***                          OR
141000090225      * - Fil. di Partenza
141100130129     c                                 (xx     =  5
141200060508     c                             and  V1Clnp =  *all'9')
141300090225      * - Fil. di Arrivo
141400130129     c                             OR  (xx     =  6
141500060508     c                             and  V1Clna =  *all'9')
141600060508      * - Codice Bolla 1 e 2 (elenco)
141700130129     c                             OR  (xx     =  7
141800090227     c                             and  ds_V1Ccbx = *blanks)
141900090225      * - Codice Cliente attuale - BLPksc / BLPccm
142000130129     c*** obbligatorio:            OR  (xx     =  8
142100060508     c***       "                  and  V1Ckso =  *zeros)
142200060508      * - Codice Tariffa attuale
142300130129     c                             OR  (xx     =  9
142400060419     c                             and  V1Ccto =  *blanks)
142500060419     c                   iter
142600060508e   2c                   endif
142700060419      *
142800130129     c                   eval      wSQL   +=  ' '
142900060418     c                                     +  %trim($Sql_FNBLP(xx))
143000130129if  2c                   if        xx      >= 4
143100100126     c                   exsr      sr_Add_Selez
143200060419e   2c                   endif
143300060419      *
143400130129e   1c                   ENDFOR
143500060418      *
143600060418     c                   ENDSR
143700060418      *
143800060418      *---------------------------------------------------------------*
143900060418      *?Preparazione stringa SQL per estrazione dati da FNARB00F     ?*
144000060418      *---------------------------------------------------------------*
144100100126     c     sr_PrepSqlARB BEGSR
144200060418      *
144300060419     c                   clear                   xx
144400060418      *
144500060508      * Impostazione delle selezioni (se immesse)
144600130129for 1c                   FOR       xx = 1  TO  %elem($Sql_FNARB)
144700060419      *
144800060508if  2c                   if
144900060508      * - Data Spedizione (range)
145000130129     c*** obbligatorio:                (xx     =  4
145100060508     c***       "                  and  V1Cdsi =  *zeros
145200060508     c***       "                  and  V1Cdsf =  *zeros)
145300060508     c***                          OR
145400090225      * - Fil. di Partenza
145500130129     c                                 (xx     =  5
145600060508     c                             and  V1Clnp =  *all'9')
145700090225      * - Fil. di Arrivo
145800130129     c                             OR  (xx     =  6
145900060508     c                             and  V1Clna =  *all'9')
146000060508      * - Codice Bolla 1 e 2 (elenco)
146100130129     c                             OR  (xx     =  7
146200090227     c                             and  ds_V1Ccbx = *blanks)
146300090225      * - Codice Cliente attuale - ARBksc / ARBccm
146400130129     c*** obbligatorio:            OR  (xx     =  8
146500060508     c***       "                  and  V1Ckso =  *zeros)
146600060508      * - Codice Tariffa attuale
146700130129     c                             OR  (xx     =  9
146800060419     c                             and  V1Ccto =  *blanks)
146900060419     c                   iter
147000060508e   2c                   endif
147100060419      *
147200130129     c                   eval      wSQL   +=  ' '
147300060418     c                                     +  %trim($Sql_FNARB(xx))
147400130129if  2c                   if        xx      >= 4
147500100126     c                   exsr      sr_Add_Selez
147600060419e   2c                   endif
147700060419      *
147800130129e   1c                   ENDFOR
147900060418      *
148000060418     c                   ENDSR
148100060419      *
148200060419      *---------------------------------------------------------------*
148300060419      *?Aggiunta selezioni nella stringa SQL (di tutti gli archivi)  ?*
148400060419      *---------------------------------------------------------------*
148500100126     c     sr_Add_Selez  BEGSR
148600060419      *
148700060419sel 1c                   SELECT
148800060508      * - Range Date Spedizione
148900130129w   1c                   WHEN      xx      =  4
149000130129     c                   eval      wSQL   +=  ' '
149100060508     c                                     +  %trim(%char(W1Cdsi))
149200060419     c                                     +  ' and '
149300060508     c                                     +  %trim(%char(W1Cdsf))
149400090225      * - Fil. di Partenza
149500130129w   1c                   WHEN           xx     =  5
149600060419     c                             and  V1Clnp >  *zeros
149700130129     c                   eval      wSQL   +=  ' '
149800060419     c                                     +  %trim(%char(V1Clnp))
149900090225      * - Fil. di Arrivo
150000130129w   1c                   WHEN           xx     =  6
150100060508     c                             and  V1Clna >  *zeros
150200130129     c                   eval      wSQL   +=  ' '
150300060508     c                                     +  %trim(%char(V1Clna))
150400090225      * - Codici Bolla (1 <=> 11)
150500090225      /free
150600130129           WHEN  xx = 7   and   ds_V1Ccbx <> *blank;
150700090225             clear  ww;
150800100125             For  wx = 1  To  %elem($V1Ccbx);
150900100125               if  $V1Ccbx(wx) <> *blank;
151000090227                 if  ww > *zero;
151100130129                   wSQL += ',';
151200090227                 endif;
151300130129                 wSQL += '''' + $V1Ccbx(wx) + '''';
151400130129                 ww   += 1;
151500090225               endif;
151600090225             EndFor;
151700130129             wSQL += ')';
151800090225      /end-free
151900090225      * - Codice Cliente (xxxKSC / xxxCCM)
152000130129w   1c                   WHEN           xx     =  8
152100060419     c                             and  V1Ckso <> *zeros
152200130129     c                   eval      wSQL   +=  ' '
152300060419     c                                     +  %trim(%char(V1Ckso))
152400090225      /free
152500090225             if  V1Ckso = V1Cksn;
152600130129               wSQL += ')';
152700090225             else;
152800090225               select;
152900090227                 when  $File(yy) = 'FNBLP00F';
153000130129                   wSQL += ' or BLPccm =';
153100090227                 when  $File(yy) = 'FNARB00F';
153200130129                   wSQL += ' or ARBccm =';
153300090227                 when  $File(yy) = 'TITAS00F';
153400130129                   wSQL += ' or TASccm =';
153500090225               endsl;
153600130129               wSQL += ' ' +  %trim(%char(V1Ckso)) + ')';
153700090225             endif;
153800090225      /end-free
153900060508      * - Codice Tariffa
154000130129w   1c                   WHEN           xx     =  9
154100060419     c                             and  V1Ccto <> *blanks
154200130129     c                   eval      wSQL   +=  ' '
154300060419     c                                     +  %trim(V1Ccto)
154400060419e   1c                   ENDSL
154500060419      *
154600060419     c                   ENDSR
154700060418      *
154800060418      *---------------------------------------------------------------*
154900060418      *?Esecuzione stringa SQL per ingolo archivio                   ?*
155000060418      *---------------------------------------------------------------*
155100100126     c     sr_ExecSQL    BEGSR
155200060418      *
155300060418      * - Apertura cursore
155400100126     c                   exsr      sr_OpenCursor
155500060418      *
155600060418      * - Esecuzione SQL
155700100126     c                   dou       $Eof = *on
155800100126     c                   exsr      sr_ReadCursor
155900060418     c                   enddo
156000060418      *
156100060418      * - Chiusura cursore
156200100126     c                   exsr      sr_CloseCursor
156300060418      *
156400060418     c                   ENDSR
156500060418      *
156600060418      *---------------------------------------------------------------*
156700060418      *?Dichiarazione e Apertura cursore                             ?*
156800060418      *---------------------------------------------------------------*
156900100126     c     sr_OpenCursor BEGSR
157000060418      *
157100060418      * Dichiarazione cursore
157200060418      *
157300060418     c/exec sql
157400060418     c+     PREPARE S1 FROM :wSQL
157500060418     c/end-exec
157600060418     c*
157700060418     c/exec sql
157800060418     c+     DECLARE C1 CURSOR FOR S1
157900060418     c/end-exec
158000060418      *
158100060418      * Apertura del cursore
158200060418      *
158300060418     c/exec sql
158400060418     c+     OPEN C1
158500060418     c/end-exec
158600060418      *
158700060418     c                   ENDSR
158800060418      *
158900060418      *---------------------------------------------------------------*
159000060418      *?Lettura cursore                                              ?*
159100060418      *---------------------------------------------------------------*
159200100126     c     sr_ReadCursor BEGSR
159300060418      *
159400060418     c                   clear                   wSQL_ds
159500060418     c/exec sql
159600060418     c+     FETCH NEXT FROM C1 INTO :wSQL_ds
159700060418     c/end-exec
159800060418      *
159900060418sel 1c                   select
160000060418w   1c                   when      SQLcod  =  100
160100060418     c                   eval      $EoF    =  *on
160200060418w   1c                   when      SQLcod  <  *zeros
160300060418     c                   eval      $Err    =  *on
160400060418     c                   eval      $Eof    =  *on
160500060418x   1c                   other
160600100126     c                   exsr      sr_RieD02
160700060418e   1c                   endsl
160800060418      *
160900060418     c                   ENDSR
161000060418      *
161100060418      *---------------------------------------------------------------*
161200060418      *?Chiusura cursore                                             ?*
161300060418      *---------------------------------------------------------------*
161400100126     c     sr_CloseCursorBEGSR
161500060418      *
161600060418     c/exec sql
161700060418     c+     CLOSE C1
161800060418     c/end-exec
161900060418      *
162000060418     c                   ENDSR
162100060419      *
162200060419      *---------------------------------------------------------------*
162300060428      *?Modifica dati delle Spedizioni                               ?*
162400060419      *---------------------------------------------------------------*
162500100126     c     sr_UpdateSped BEGSR
162600060419      *
162700060419      * -FNBLP00F?
162800130129if  1c                   IF        $nrrBLP(jj) > *zero
162900130129      *
163000100127     c     $nrrBLP(jj)   chain     FNBLP000
163100060508if  2c                   if             %found(FNBLP00F)
163200090225     c                             and (BLPksc  =  V1Ckso
163300090225     c                              or (BLPccm  =  V1Ckso  and
163400090225     c                                  V1Ckso  <> V1Cksn))
163500060419      * - modifica codice cliente
163600090225      /free
163700090225           if  BLPksc = V1Ckso;
163800090225             BLPksc = V1Cksn;
163900090225           endif;
164000090225           if  BLPccm = V1Ckso   and   V1Ckso <> V1Cksn;
164100090225             BLPccm = V1Cksn;
164200090225           endif;
164300090225      /end-free
164400060419      * - modifica codice tariffa
164500060508     c                   if             V1Cctn  <> *blanks
164600060508     c                             and (V1Ccto  =  *blanks
164700090302     c                             or   BLPctr  =  %int(V1Ccto))
164800090302     c                   eval      BLPctr  =  %int(V1Cctn)
164900060419     c                   endif
165000060419      *                  __________________
165100060419     c                   UPDATE    FNBLP000
165200060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
165300060508e   2c                   endif
165400130129      *
165500130129e   1c                   ENDIF
165600060419      *
165700060419      * -FNARB00F?
165800130129if  1c                   IF        $nrrARB(jj) > *zero
165900130129      *
166000100127     c     $nrrARB(jj)   chain     FNARB000
166100060508if  2c                   if             %found(FNARB00F)
166200090225     c                             and (ARBksc  =  V1Ckso
166300090225     c                              or (ARBccm  =  V1Ckso  and
166400090225     c                                  V1Ckso  <> V1Cksn))
166500060419      * - modifica codice cliente
166600090225      /free
166700090225           if  ARBksc = V1Ckso;
166800090225             ARBksc = V1Cksn;
166900090225           endif;
167000090225           if  ARBccm = V1Ckso   and   V1Ckso <> V1Cksn;
167100090225             ARBccm = V1Cksn;
167200090225           endif;
167300090225      /end-free
167400060419      * - modifica codice tariffa
167500060508     c                   if             V1Cctn  <> *blanks
167600060508     c                             and (V1Ccto  =  *blanks
167700090302     c                             or   ARBctr  =  %int(V1Ccto))
167800090302     c                   eval      ARBctr  =  %int(V1Cctn)
167900060419     c                   endif
168000060419      *                  __________________
168100060419     c                   UPDATE    FNARB000
168200060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
168300060508e   2c                   endif
168400130129      *
168500130129e   1c                   ENDIF
168600060421      *
168700060421      * -TITAS00F?
168800130129if  1c                   IF        $nrrTAS(jj) > *zero
168900130129      *
169000100127     c     $nrrTAS(jj)   chain     TITAS000
169100060508if  2c                   if             %found(TITAS00F)
169200090225     c                             and (TASksc  =  V1Ckso
169300090225     c                              or (TASccm  =  V1Ckso  and
169400090225     c                                  V1Ckso  <> V1Cksn))
169500060421      * - modifica codice cliente
169600090225      /free
169700090225           if  TASksc = V1Ckso;
169800090225             TASksc = V1Cksn;
169900090225           endif;
170000090225           if  TASccm = V1Ckso   and   V1Ckso <> V1Cksn;
170100090225             TASccm = V1Cksn;
170200090225           endif;
170300090225      /end-free
170400060421      * - modifica codice tariffa
170500060508     c                   if             V1Cctn  <> *blanks
170600060508     c                             and (V1Ccto  =  *blanks
170700090302     c                             or   TASctr  =  %int(V1Ccto))
170800090302     c                   eval      TASctr  =  %int(V1Cctn)
170900060421     c                   endif
171000060421      *                  __________________
171100060421     c                   UPDATE    TITAS000
171200060421      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
171300060421e   2c                   endif
171400060419      *
171500130129e   1c                   ENDIF
171600060419      *
171700060419      * -FIAR601L?
171800060508      *   (SOLO per FNBLP00F/FNARB00F)
171900100127if  1c                   IF            ($nrrBLP(jj)  > *zero
172000100127     c                             or   $nrrARB(jj)  > *zero)
172100100127     c                             and  $SpedAR6(jj) > *zero
172200060508      *
172300060419     c     K04AR601      setll     FIAR6000
172400060508if  2c                   if        not %equal(FIAR601L)
172500060419     c                   leavesr
172600060508e   2c                   endif
172700060419     c     K04AR601      reade     FIAR6000
172800060508      *
172900060508do  2c                   DOW       NOT %eof(FIAR601L)
173000060508      *
173100060509if  3c                   if        AR6ksc  =  V1Ckso
173200060509      * - modifica codice cliente
173300060419     c                   eval      AR6ksc  =  V1Cksn
173400060419      * - modifica codice tariffa
173500060508     c                   if             V1Cctn  <> *blanks
173600060508     c                             and (V1Ccto  =  *blanks
173700090302     c                             or   AR6ctr  =  %int(V1Ccto))
173800090302     c                   eval      AR6ctr  =  %int(V1Cctn)
173900060419     c                   endif
174000060419      *                  __________________
174100060419     c                   UPDATE    FIAR6000
174200060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
174300060509e   3c                   endif
174400060509      *
174500060419     c     K04AR601      reade     FIAR6000
174600060508      *
174700060508e   2c                   ENDDO
174800090227      *
174900090225e   1c                   ENDIF
175000090227      /free
175100090227
175200090227         // -TNCSB03L?
175300090227         //   (SOLO per TITAS00F  e  SE richiesta mod. del cod.cliente)
175400100127         IF  $nrrTAS(jj)  > *zero   and
175500100127             $SpedCSB(jj) = *on     and   V1Ckso <> V1Cksn;
175600130129
175700090227           chain  K04AR601  TNCSB000;
175800090227           If  %found(TNCSB03L)  and
175900090227               CSBbna = *zero    and
176000090227              (CSBksc = V1Ckso   or   CSBcdi = V1Ckso);
176100130129
176200090227             // - modifica codice/i cliente
176300090227             if  CSBksc = V1Ckso;
176400090227               CSBksc = V1Cksn;
176500090227             endif;
176600090227             if  CSBcdi = V1Ckso;
176700090227               CSBcdi = V1Cksn;
176800090227             endif;
176900130129
177000090227             //_________________
177100090227             UPDATE    TNCSB000;
177200090227             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
177300130129
177400090227           EndIf;
177500130129
177600090227         ENDIF;
177700090227
177800090227      /end-free
177900090225      *
178000090225     c                   ENDSR
178100060418      *
178200060418      *---------------------------------------------------------------*
178300060418      *?Operazioni finali                                            ?*
178400060418      *---------------------------------------------------------------*
178500100126     c     sr_RoutEnd    BEGSR
178600060418      *
178700060418     c                   clear                   TIBS69ds
178800060419     c                   eval      I69tla  =  'C'
178900060418     c                   call      'TIBS69R'
179000060418     c                   parm                    TIBS69ds
179100060418      *
179200060418     c                   ENDSR
179300060428      *
179400060428      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
179500060428      *
179600060418** - $Msg -------------------------------------------------------------------*
179700060418Data formalmente errata                                                          1
179800060418Range di date errato                                                             2
179900090225Filiale di Partenza errata                                                       3
180000090225Filiale di Arrivo errata                                                         4
180100060508Codice Bolla errato                                                              5
180200060508Codice Cliente errato                                                            6
180300060508Codice Tariffa errato                                                            7
180400060508Codice "sostitutivo" obbligatorio se inserito quello "attuale"                   8
180500060508Codice "sostitutivo" uguale a quello "attuale"                                   9
180600100126ATTENZIONE: NON sono caricabili TUTTE le spedizioni => occorrerà RIESEGUIRE     10
180700130129** - $File
180800130129FNBLP00F  FNARB00F  TITAS00F
180900130129** - $Sql_FNBLP *..2....+....3....+....4....+....5....+
181000130129select BLPaas as AAS, BLPlnp as LNP, BLPnrs as NRS,      1
181100130129       BLPnsp as NSP, '1FNBLP' as File, rrn(FNBLP00F)    2
181200130129from   FNBLP00F                                          3
181300130129where  ((BLPaas * 10000) + BLPmgs) between               4
181400130129  and  BLPlnp =                                          5
181500130129  and  BLPlna =                                          6
181600130129  and  BLPcbo in (                                       7
181700130129  and (BLPksc =                                          8
181800130129  and  BLPctr =                                          9
181900130129** - $Sql_FNARB *..2....+....3....+....4....+....5....+
182000130129select ARBaas as AAS, ARBlnp as LNP, ARBnrs as NRS,      1
182100130129       ARBnsp as NSP, '2FNARB' as File, rrn(FNARB00F)    2
182200130129from   FNARB00F                                          3
182300130129where  ((ARBaas * 10000) + ARBmgs) between               4
182400130129  and  ARBlnp =                                          5
182500130129  and  ARBlna =                                          6
182600130129  and  ARBcbo in (                                       7
182700130129  and (ARBksc =                                          8
182800130129  and  ARBctr =                                          9
182900130129** - $Sql_TITAS *..2....+....3....+....4....+....5....+
183000130129select TASaas as AAS, TASlnp as LNP, TASnrs as NRS,      1
183100130129       TASnsp as NSP, '3TITAS' as File, rrn(TITAS00F)    2
183200130129from   TITAS00F                                          3
183300130129where  ((TASaas * 10000) + TASmgs) between               4
183400130129  and  TASlnp =                                          5
183500130129  and  TASlna =                                          6
183600130129  and  TAScbo in (                                       7
183700130129  and (TASksc =                                          8
183800130129  and  TASctr =                                          9
