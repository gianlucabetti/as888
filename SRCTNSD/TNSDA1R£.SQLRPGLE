000100090227      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200100125     /*CMD addlible lib(FILTRAPRD) position(*last)
000300100125     /*PRM closqlcsr(*endmod) dbgview(*source) dynusrprf(*owner)
000400100125     /*END rmvlible lib(FILTRAPRD)
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
003100100126     d c_ForFetch      c                   const('FOR FETCH ONLY')
003200060508      *
003300090225      * - Costante per controllo "caratteri solo numerici"
003400100126     d c_Digits        c                   const('1234567890')
003500100125      *
003600100126      * - Numero massimo di spedizioni elaborabili
003700100127     d c_MaxSped       c                   const(30000)
003800100126      *
003900100126      * - SubFile Page
004000100126     d c_SflPag        c                   const(16)
004100100126      *
004200100126      * - Tasti di funzione
004300100126     d c_Enter         c                   const(x'F1')
004400100127     d c_RollDown      c                   const(x'F4')
004500100126     d c_RollUp        c                   const(x'F5')
004600060412      *
004700060412      *?S C H I E R E?------------------------------------------------*
004800060412      *
004900060418      * - Messaggi di errore
005000060508     d $Msg            s             78    dim(10) ctdata perrcd(1)
005100060418      * - Archivi da gestire via SQL
005200060419     d $File           s             10    dim( 3) ctdata perrcd(3)
005300060418      * - Stringhe SQL
005400100127     d $Sql_FNBLP      s             65    dim(09) ctdata perrcd(1)
005500100127     d $Sql_FNARB      s             65    dim(09) ctdata perrcd(1)
005600100127     d $Sql_TITAS      s             65    dim(09) ctdata perrcd(1)
005700060419      *
005800100126      * - Spedizioni elaborabili
005900100127     d $Sped           s                   like(ds_Sped)
006000100126     d                                     dim(c_MaxSped)  inz
006100100126      * - Nrr spedizione rilevata nel file FNBLP00F
006200100127     d $nrrBLP         s                   like(SQLrrn)
006300100127     d                                     dim(c_MaxSped)  inz
006400100126      * - Nrr spedizione rilevata nel file FNARB00F
006500100127     d $nrrARB         s                   like(SQLrrn)
006600100127     d                                     dim(c_MaxSped)  inz
006700100126      * - Nrr spedizione rilevata nel file TITAS00F
006800100127     d $nrrTAS         s                   like(SQLrrn)
006900100127     d                                     dim(c_MaxSped)  inz
007000100126      * - Conteggio records x spedizioni rilevate in FIAR600F
007100100126     d $SpedAR6        s              3  0 dim(c_MaxSped)  inz
007200100126      * - Flag spedizione rilevata nel file TNCSB00F
007300100126     d $SpedCSB        s               n   dim(c_MaxSped)  inz
007400060412      *
007500060412      *?S T R U T T U R E   D A T I?----------------------------------*
007600060412      *
007700060412      * - Parametri
007800060412     d KPJBA         e ds
007900060418      *
008000060412      * - Parametri x Controllo profilo utenti
008100060412     d TIBS34ds      e ds                  inz
008200060412      * - Ds di riferimento al file esterno AZUTE00F
008300060412     d AZUTEds       e ds                  extname(AZUTE00F)
008400060412      * - Ds per dati organigramma
008500060412     d dDATIUTE      e ds
008600060412      *
008700060418      * - Parametri per decodifica cliente
008800060412     d TIBS69ds      e ds                  inz
008900060412     d DS_cnaco      e ds                  inz  extname(CNACO00F)
009000060412     d DS_cnind      e ds                  inz  extname(CNIND00F)
009100060412     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
009200060412     d DS_fncls      e ds                  inz  extname(FNCLS00F)
009300060420      *
009400090225      * - Parametri per Interrogazione singola spedizione Sede
009500060420     d TNSB50ds      e ds                  inz
009600060420     d  I50op0       e                     inz('D05')
009700060420      *
009800090225      * - Parametri per Interrogazione singola spedizione Partenza
009900060420     d FNLS05ds        ds                  inz
010000060420     d  LS05aas                1      4  0 inz
010100060420     d  LS05lnp                5      7  0 inz
010200060420     d  LS05nrs                8      9  0 inz
010300060420     d  LS05nsp               10     16  0 inz
010400060420     d  LS05f03               17     17    inz
010500060420     d  LS05flg               18     18    inz('C')
010600060420     d  LS05fa9               19     19    inz
010700060420     d  LS05fb5               20     20    inz
010800060420     d  LS05f75               21     21    inz
010900060420     d  LS05s98               22     22    inz
011000060420     d  LS05rsu               23     42    inz
011100060420     d  LS05cml               43     43    inz
011200060420     d  LS05fel               44     46  0 inz
011300060420      * LS05CLI indica se il pgm è richiamato da un cliente (='S')
011400060420     d  LS05cli               47     47    inz
011500060420      * LS05GIA indica se il pgm è richiamato dalle giacenze
011600060420     d  LS05gia               48     48    inz
011700060420      *
011800090225      * - Parametri per Interrogazione singola spedizione Arrivo
011900060420     d FNLR36ds        ds                  inz
012000060420     d  LR36aas               14     17  0 inz
012100060420     d  LR36lnp               18     20  0 inz
012200060420     d  LR36nrs               21     22  0 inz
012300060420     d  LR36nsp               23     29  0 inz
012400060420     d  LR36f03               30     30    inz
012500060421     d  LR36flg               31     31    inz('1')
012600060420     d  LR36bol               62     62    inz
012700060412      *
012800090225      * - Parametri per controllo data e transform in giorni - XSRDA8 -
012900060412     d WLBdat          ds                  inz
013000060419     d  G08dat                        8  0 inz
013100060419     d  G08inv                        8  0 inz
013200060419     d  G08err                        1    inz('3')
013300060419     d  G08tgi                        5  0 inz
013400090225      *
013500090225      * - Ridefinizione "Codici Bolla" a video in schiera
013600090225     d ds_V1Ccbx       ds                  inz
013700090225     d  V1Ccb1                             inz
013800090225     d  V1Ccb2                             inz
013900090225     d  V1Ccb3                             inz
014000090225     d  V1Ccb4                             inz
014100090225     d  V1Ccb5                             inz
014200090225     d  V1Ccb6                             inz
014300090225     d  V1Ccb7                             inz
014400090225     d  V1Ccb8                             inz
014500090225     d  V1Ccb9                             inz
014600090225     d  V1CcbA                             inz
014700100127     d   $V1Ccbx               1     20    inz  dim(10)
014800060418      *
014900060418      * - Dati estratti via SQL
015000060418     d wSQL_ds         ds                  inz
015100100127     d  SQLaas                        4s 0 inz
015200100127     d  SQLlnp                        3s 0 inz
015300100127     d  SQLnrs                        2s 0 inz
015400100127     d  SQLnsp                        7s 0 inz
015500100127     d  SQLrrn                       11  0 inz
015600060419      *
015700100127     d ds_Sped         ds                  inz
015800100127     d  WWWaas                             inz  like(SQLaas)
015900100127     d  WWWlnp                             inz  like(SQLlnp)
016000100127     d  WWWnrs                             inz  like(SQLnrs)
016100100127     d  WWWnsp                             inz  like(SQLnsp)
016200060420      *
016300100126     d DSFMT           ds           512
016400100126     d  £Tasto               369    369
016500100126      *
016600060420     d Status         sds
016700060420     d  SDSpgm           *proc
016800060412      *
016900060412      *?V A R I A B I L I?--------------------------------------------*
017000060412      *
017100060412      * - Flags booleani
017200100125     d $EoF            s               n   inz(*off)
017300100125     d $Err            s               n   inz(*off)
017400100125     d $Fine           s               n   inz(*off)
017500100125     d $InzD01         s               n   inz(*on)
017600100126     d $InzD02         s               n   inz(*on)
017700100125     d $InzS01         s               n   inz(*on)
017800100127     d $AltreSped      s               n   inz
017900060418      * - Altri flag
018000100126     d $Video          s              1a   inz('1')
018100060412      * - Indici di schiera / Contatori
018200100126     d jj              s              5  0 inz
018300100125     d ww              s              5  0 inz
018400100125     d wx              s              5  0 inz
018500100125     d xx              s              5  0 inz
018600060419     d yy              s              3  0 inz
018700060412      * - Parametri per SQL
018800060412     d wSQL            s           5000    inz
018900060418      * - Parametri per QCMDEXC
019000060418     d Qcmd            s             80    inz
019100060418     d Qlen            s             15  5 inz(80)
019200060419      * - Variabili con riferimento
019300060508     d W1Cdsi          s                   like(V1Cdsi) inz
019400060508     d W1Cdsf          s                   like(V1Cdsf) inz
019500100127     d wJJini          s                   like(JJ)     inz
019600100127     d wJJfin          s                   like(JJ)     inz
019700100127     d WjjMax          s                   like(JJ)     inz
019800100125      * - Variabili per la gestione del subfile
019900100125     d S01nrr          s              5i 0 inz
020000060412      * - Campi di comodo
020100060419     d wFile           s             10    inz
020200060419     d wLibr           s             10    inz
020300100127
020400100127       //--------------------------------------------------------------
020500100127       //?Definizione prototipi procedure.                             ?
020600100127       //--------------------------------------------------------------
020700100127
020800100127       // -?Reperimento dati utente?
020900100127      /copy gaitrasrc/srcProtoPR,XSRDA8
021000100127
021100060412      *
021200060412      *?K E Y - L I S T?----------------------------------------------*
021300060412      *
021400060419      * - file TABEL00F
021500060418     c     K03TABEL      klist
021600060418     c                   kfld                    TBLkut
021700060419     c                   kfld                    TBLcod
021800060419     c                   kfld                    TBLkey
021900060419      * - file FIAR601L
022000060419     c     K04AR601      klist
022100100127     c                   kfld                    WWWaas
022200100127     c                   kfld                    WWWlnp
022300100127     c                   kfld                    WWWnrs
022400100127     c                   kfld                    WWWnsp
022500060412      *
022600060412      *?I N D I C A T O R I?------------------------------------------*
022700060412      *                                                               *
022800060418      *  28 - Visualizza messaggio di errore a video                  *
022900060418      * N30 - SFLDSP                                                  *
023000060418      * N31 - SFLDSPCTL                                               *
023100060418      *  32 - SFLNXTCHG                                               *
023200060418      *  33 - SFLEND                                                  *
023300060418      *  51 - Data iniziale errata                                    *
023400060418      *  52 - Data  finale  errata                                    *
023500090225      *  53 - Fil. di Partenza errato                                 *
023600090225      *  54 - Fil. di Arrivo   errato                                 *
023700060508      *  55 - Codice Bolla 1   errato                                 *
023800060508      *  56 - Codice Bolla 2   errato                                 *
023900090225      *  66 - Codice Cliente   attuale   errato                       *
024000090225      *  67 - Codice Cliente sostitutivo errato                       *
024100090225      *  68 - Codice Tariffa   attuale   errato                       *
024200090225      *  69 - Codice Tariffa sostitutivo errato                       *
024300060508      *  71 - Flag stampa elenco spedizioni errato                    *
024400060412      *  90 - Generico di errore                                      *
024500060412      *===============================================================*
024600060412
024700060412     c     *Entry        plist
024800060412     c                   parm                    KPJBA
024900060412      *
025000060412      * Operazioni iniziali
025100100126     c                   exsr      sr_RoutInz
025200060412      *
025300060412      * Gestione videata
025400060419do  1c                   dow       $Fine   =  *off
025500100126     c     $Video        caseq     '1'           sr_GesD01
025600100126     c     $Video        caseq     '2'           sr_GesD02
025700100126     c     $Video        caseq     '3'           sr_GesS01
025800060418     c                   endcs
025900060418e   2c                   enddo
026000060412      *
026100060412      * Operazioni finali
026200100126     c                   exsr      sr_RoutEnd
026300060412      *
026400060412      * Fine programma
026500060419     c                   eval      *inLR   =  *on
026600060412      *
026700060412      *?S U B R O U T I N E S?----------------------------------------*
026800060412      *
026900060412      *---------------------------------------------------------------*
027000060412      *?Operazioni iniziali                                          ?*
027100060412      *---------------------------------------------------------------*
027200100126     c     sr_RoutInz    BEGSR
027300060509      *
027400060509      * Pgm. eseguibile?SOLO?da utenti EDP*
027500060509     c                   if        %subst(KNMUS:1:3) <> 'EDP'
027600060509     c                   eval      $Fine   =  *on
027700060509     c                   leavesr
027800060509     c                   endif
027900090227      /free
028000090227         // Impostazione opzioni per SQL
028100090227         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
028200090227         exec SQL   set option   CloSqlCsr = *endmod;
028300090227      /end-free
028400060412      *
028500060412      * Reperisce dati job
028600060412     c     *dtaara       define    §azute        AZUTEds
028700060412     c     *dtaara       define    §datiute      dDATIUTE
028800060412     c                   in(E)     *dtaara
028900060412     c                   if        %ERROR or RSUT = *blanks
029000060508     c                   clear                   TIBS34ds
029100060412     c                   call      'TIBS34R'
029200060508     c                   parm                    TIBS34ds
029300060412     c                   in        *dtaara
029400060412     c                   endif
029500060418      *
029600060418      * Impostazione dati "fissi"
029700060419     c                   eval      TBLkut  =  1
029800060419     c                   eval      TBLcod  =  '3A'
029900060418      *
030000060419     c                   eval      V1Tpgm  =  SDSpgm
030100060418      *
030200060418      * Esecuzione override
030300090225      * - da S.I. di filiale
030400060420if  1c                   IF        %subst(KNSIF : 1 : 3) = 'FIL'
030500060420if  2c                   if        %subst(KNSIF : 7 : 1) = 'P'
030600060419     c                   eval      wLIBR   =  'GAITRAGRPS'
030700060418x   2c                   else
030800060419     c                   eval      wLIBR   =  'GAITRAGRU '
030900060418e   2c                   endif
031000060421     c                   eval      wFILE   =  'TITAS00F  '
031100060418     c                   exsr      sr_Override
031200090225     c                   eval      wFILE   =  'TNCSB03L  '
031300090225     c                   exsr      sr_Override
031400060418e   1c                   ENDIF
031500060418      * - da S.I. di sede
031600060420if  1c                   IF        %subst(KNSIF : 1 : 3) = 'GAI'
031700060420if  2c                   if        %subst(KNSIF : 7 : 1) = 'P'
031800060419     c                   eval      wLIBR   =  'FILTRAPRD '
031900060418x   2c                   else
032000060419     c                   eval      wLIBR   =  'FILTRA201 '
032100060418e   2c                   endif
032200060421     c                   eval      wFILE   =  'FNBLP00F  '
032300060418     c                   exsr      sr_Override
032400060421     c                   eval      wFILE   =  'FNARB00F  '
032500060418     c                   exsr      sr_Override
032600060419     c                   eval      wFILE   =  'FIAR601L  '
032700060418     c                   exsr      sr_Override
032800060418e   1c                   ENDIF
032900060418      *
033000060418      * Apertura archivi
033100060418     c                   open      TITAS00F
033200090225     c                   open      TNCSB03L
033300060418     c                   open      FNBLP00F
033400060418     c                   open      FNARB00F
033500060418     c                   open      FIAR601L
033600060418      *
033700060418     c                   ENDSR
033800060418      *
033900060418      *---------------------------------------------------------------*
034000060418      *?Esecuzione singola Override                                  ?*
034100060418      *---------------------------------------------------------------*
034200060418     c     sr_Override   BEGSR
034300060418      *
034400060420     c                   eval      Qcmd = 'OVRDBF  file('
034500060420     c                                  + %trim(wFILE)
034600060420     c                                  + ') tofile('
034700060420     c                                  + %trim(wLIBR)
034800060420     c                                  + '/'
034900060420     c                                  + %trim(wFILE)
035000060420     c                                  + ')'
035100060418      *
035200060418     c                   call      'QCMDEXC'
035300060418     c                   parm                    Qcmd
035400060418     c                   parm                    Qlen
035500060418      *
035600060418     c                   ENDSR
035700060418      *
035800060418      *---------------------------------------------------------------*
035900060418      *?Gestione videata D01                                         ?*
036000060418      *---------------------------------------------------------------*
036100100126     c     sr_GesD01     BEGSR
036200060418      *
036300060418      * Inizializzazione videata
036400060419if  1c                   if        $InzD01 =  *on
036500100126     c                   exsr      sr_InzD01
036600060418     c                   movel     *off          $InzD01
036700060418e   1c                   endif
036800060418      *
036900060418      * Emissione testata
037000060509     c                   write     SDA1T01
037100060418      *
037200060418      * Emissione piede
037300060509     c                   write     SDA1Z01
037400060418      *
037500060418      * Emissione videata
037600060509     c                   exfmt     SDA1D01
037700060418     c                   setoff                                       28  90
037800060418     c                   movea     *zeros        *in(50)
037900060418     c                   clear                   V1Dmsg
038000060418      *
038100060418     c                   SELECT
038200060418      *
038300060418      * F3=Fine
038400060418w   1c                   WHEN      *inKC
038500100126     c                   exsr      sr_F03D01
038600060418     c                   leavesr
038700060418      *
038800060418      * Controllo dati immessi a video
038900060418x   1c                   OTHER
039000100126     c                   exsr      sr_CtrD01
039100060418if  2c                   if        *in90
039200060418     c                   leavesr
039300060418e   2c                   endif
039400060418      * F6=Esegue
039500060419if  2c                   if        *inKF
039600100126     c                   eval      $InzD02 =  *on
039700060419     c                   eval      $InzS01 =  *on
039800060419     c                   eval      $Video  =  '2'
039900060418e   2c                   endif
040000060418      *
040100060418e   1c                   ENDSL
040200060418      *
040300060418     c                   ENDSR
040400060418      *
040500060418      *---------------------------------------------------------------*
040600060418      *?Inizializzazione videata D01                                 ?*
040700060418      *---------------------------------------------------------------*
040800100126     c     sr_InzD01     BEGSR
040900060418      *
041000060509     c                   clear                   SDA1D01
041100060418      *
041200060418      * Imposto i dati di default
041300090225     c                   eval      V1Dlnp  =  '999 = Tutte le Fil.'
041400090225     c                   eval      V1Dlna  =  '999 = Tutte le Fil.'
041500060418      *
041600060418     c                   ENDSR
041700060418      *
041800060418      *---------------------------------------------------------------*
041900060418      *?Gestione tasto funzionale F03 da videata D01                 ?*
042000060418      *---------------------------------------------------------------*
042100100126     c     sr_F03D01     BEGSR
042200060418      *
042300060418      * Chiudo il programma
042400060419     c                   eval      $Fine   =  *on
042500060418      *
042600060418     c                   ENDSR
042700060418      *
042800060418      *---------------------------------------------------------------*
042900060418      *?Controllo dati immessi in videata D01                        ?*
043000060418      *---------------------------------------------------------------*
043100100126     c     sr_CtrD01     BEGSR
043200060418      *
043300060418     c                   movea     *zeros        *in(50)
043400060418      *
043500060418      * Controllo Data iniziale
043600060418     c                   clear                   WLBdat
043700060508     c                   eval      G08dat  =  V1Cdsi
043800060418     c                   call      'XSRDA8'
043900060419     c                   parm                    WLBdat
044000060419if  1c                   if        G08err  =  *on
044100060418     c                   seton                                        285190
044200060419     c                   eval      V1Dmsg  =  $Msg(1)
044300060418     c                   leavesr
044400060418e   1c                   endif
044500060508     c                   eval      V1Cdsi  =  G08dat
044600060508     c                   eval      W1Cdsi  =  G08inv
044700060418      *
044800060418      * Controllo Data finale
044900060508if  1c                   if        V1Cdsf  <> *zeros
045000060418     c                   clear                   WLBdat
045100060508     c                   eval      G08dat  =  V1Cdsf
045200060418     c                   call      'XSRDA8'
045300060419     c                   parm                    WLBdat
045400060421if  2c                   if        G08err  =  *on
045500060418     c                   seton                                        285290
045600060419     c                   eval      V1Dmsg  =  $Msg(1)
045700060418     c                   leavesr
045800060421e   2c                   endif
045900060508     c                   eval      V1Cdsf  =  G08dat
046000060508     c                   eval      W1Cdsf  =  G08inv
046100060421x   1c                   else
046200060508     c                   eval      W1Cdsf  =  W1Cdsi
046300060421e   1c                   endif
046400060418      *
046500060418      * Controllo range di Date
046600060508if  1c                   if        W1Cdsi  >  W1Cdsf
046700060418     c                   seton                                        285190
046800060419     c                   eval      V1Dmsg  =  $Msg(2)
046900060418     c                   leavesr
047000060418e   1c                   endif
047100060418      *
047200090225      * Controllo Fil. di Partenza
047300060508if  1c                   if        V1Clnp  <> *all'9'
047400060418     c     V1Clnp        chain     AZORG
047500060418if  2c                   if            %found(AZORG01L)
047600060418     c                             and ORGfva = *blanks
047700060419     c                   eval      V1Dlnp  =  ORGdes
047800060418x   2c                   else
047900060418     c                   seton                                        285390
048000060419     c                   eval      V1Dmsg  =  $Msg(3)
048100060418     c                   leavesr
048200060418e   2c                   endif
048300060508x   1c                   else
048400090225     c                   eval      V1Dlnp  =  '999 = Tutte le Fil.'
048500060418e   1c                   endif
048600060508      *
048700090225      * Controllo Fil. di Arrivo
048800060508if  1c                   if        V1Clna  <> *all'9'
048900060508     c     V1Clna        chain     AZORG
049000060508if  2c                   if            %found(AZORG01L)
049100060508     c                             and ORGfva = *blanks
049200060508     c                   eval      V1Dlna  =  ORGdes
049300060508x   2c                   else
049400060508     c                   seton                                        285490
049500060508     c                   eval      V1Dmsg  =  $Msg(4)
049600060508     c                   leavesr
049700060508e   2c                   endif
049800060508x   1c                   else
049900090225     c                   eval      V1Dlna  =  '999 = Tutte le Fil.'
050000060508e   1c                   endif
050100060418      *
050200060418      * Controllo codici Bolla
050300090225      /free
050400090225           FOR  xx = 1  TO  %elem($V1Ccbx);
050500090225
050600090225             If  $V1Ccbx(xx) <> *blank;
050700090225
050800090225               TBLkey = $V1Ccbx(xx);
050900090225               chain  k03tabel  TABEL;
051000090225
051100090302               if  Not %found(TABEL00F)   or   TBLflg <> *blank;
051200090225                 *in28 = *on;
051300090225                 *in90 = *on;
051400090225                 *in(54+xx) = *on;
051500090225                 V1Dmsg = $Msg(5);
051600090225                 leavesr;
051700090225               endif;
051800090225
051900090225             EndIf;
052000090225
052100090225           ENDFOR;
052200090225
052300090225       /end-free
052400060418      *
052500060418      * Controllo codice Cliente esistente
052600060508sel 1c                   SELECT
052700060508w   1c                   WHEN      V1Ckso  =  *zeros
052800090225     c                   seton                                        286690
052900060508     c                   eval      V1Dmsg  =  $Msg(6)
053000060418     c                   leavesr
053100060508w   1c                   WHEN           %subst(%editc(V1Ckso:'Z'):4:4)
053200060508     c                                     =  *all'8'
053300060508     c                             or   %subst(%editc(V1Ckso:'Z'):4:4)
053400060508     c                                     =  *all'9'
053500090225     c                   seton                                        286690
053600060508     c                   eval      V1Dmsg  =  $Msg(6)
053700060508     c                   leavesr
053800060508x   1c                   OTHER
053900060418     c                   clear                   TIBS69ds
054000060419     c                   eval      I69kac  =  V1Ckso
054100060418     c                   call      'TIBS69R'
054200060418     c                   parm                    TIBS69ds
054300060508     c                   parm                    ds_CNACO
054400060508     c                   parm                    ds_CNIND
054500060508     c                   parm                    ds_CNCLP
054600060508     c                   parm                    ds_FNCLS
054700060419if  2c                   if        O69err  =  *on
054800090225     c                   seton                                        286690
054900060508     c                   eval      V1Dmsg  =  $Msg(6)
055000060418     c                   leavesr
055100060418x   2c                   else
055200060418     c                   movel     ACOrag        V1Dkso
055300060418e   2c                   endif
055400060508e   1c                   ENDSL
055500060418      *
055600060418      * Controllo codice Cliente sostitutivo
055700060508sel 1c                   SELECT
055800060508w   1c                   WHEN      V1Cksn  =  *zeros
055900090225     c                   seton                                        286790
056000060508     c                   eval      V1Dmsg  =  $Msg(6)
056100060418     c                   leavesr
056200060508w   1c                   WHEN           %subst(%editc(V1Cksn:'Z'):4:4)
056300060508     c                                     =  *all'8'
056400060508     c                             or   %subst(%editc(V1Cksn:'Z'):4:4)
056500060508     c                                     =  *all'9'
056600090225     c                   seton                                        286790
056700060508     c                   eval      V1Dmsg  =  $Msg(6)
056800060508     c                   leavesr
056900060508x   1c                   OTHER
057000060418     c                   clear                   TIBS69ds
057100060419     c                   eval      I69kac  =  V1Cksn
057200060418     c                   call      'TIBS69R'
057300060418     c                   parm                    TIBS69ds
057400060508     c                   parm                    ds_CNACO
057500060508     c                   parm                    ds_CNIND
057600060508     c                   parm                    ds_CNCLP
057700060508     c                   parm                    ds_FNCLS
057800060419if  2c                   if        O69err  =  *on
057900090225     c                   seton                                        286790
058000060508     c                   eval      V1Dmsg  =  $Msg(6)
058100060418     c                   leavesr
058200060418x   2c                   else
058300060418     c                   movel     ACOrag        V1Dksn
058400060418e   2c                   endif
058500060508e   1c                   ENDSL
058600060419      *
058700060419      * Controllo codice tariffa esistente
058800060421      * (non obbligatorio)
058900060421if  1c                   if        V1Ccto  <> *blanks
059000100126     c     c_Digits      check     V1Ccto                                 10
059100060421if  2c                   if        *in10
059200090225     c                   seton                                        286890
059300060508     c                   eval      V1Dmsg  =  $Msg(7)
059400060421     c                   leavesr
059500060421e   2c                   endif
059600060421e   1c                   endif
059700060419      *
059800060419      * Controllo codice tariffa sostitutivo
059900060421      * (obbligatorio se immesso il codice tariffa esistente)
060000060419if  1c                   if             V1Ccto <> *blanks
060100060419     c                             and  V1Cctn  = *blanks
060200090225     c                   seton                                        286990
060300060508     c                   eval      V1Dmsg  =  $Msg(8)
060400060419     c                   leavesr
060500060419e   1c                   endif
060600120427if  1c**!!!              if             V1Cctn <> *blanks
060700120427     c**!!!                        and  V1Ccto  =  V1Cctn
060800120427     c**!!!              seton                                        286890
060900120427     c**!!!              eval      V1Dmsg  =  $Msg(9)
061000120427     c**!!!              leavesr
061100120427e   1c**!!!              endif
061200120427if  1c                   if             V1Cctn <> *blanks
061300120427     c                             and  V1Ccto  = *blanks
061400120427     c                   seton                                        286890
061500120427     c                   eval      V1Dmsg  =  $Msg(9)
061600120427     c                   leavesr
061700120427e   1c                   endif
061800060421if  1c                   if        V1Cctn  <> *blanks
061900100126     c     c_Digits      check     V1Cctn                                 10
062000060421if  2c                   if        *in10
062100090225     c                   seton                                        286990
062200060508     c                   eval      V1Dmsg  =  $Msg(7)
062300060421     c                   leavesr
062400060421e   2c                   endif
062500060421e   1c                   endif
062600060418      *
062700060418     c                   ENDSR
062800060419      *
062900060419      *---------------------------------------------------------------*
063000060419      *?Gestione videata D02                                         ?*
063100060419      *---------------------------------------------------------------*
063200100126     c     sr_GesD02     BEGSR
063300060419      *
063400060419      * Inizializzazione videata
063500100126if  1c                   if        $InzD02 =  *on
063600100126     c                   exsr      sr_InzD02
063700100126     c                   movel     *off          $InzD02
063800060419e   1c                   endif
063900060419      *
064000060421if  1c                   if        NOT *in90
064100060419      * Emissione testata
064200060509     c                   write     SDA1T01
064300060419      * Emissione piede
064400060509     c                   write     SDA1Z02
064500060421      * Emissione videata precedente (filtro di lancio)
064600060509     c                   write     SDA1D01
064700060421     c                   write     Protect
064800100125      /free
064900100126         else;
065000100126           *in28 = *off;
065100100126           *in90 = *off;
065200100126           write SDA1T01;
065300100126           write SDA1Z02;
065400100126           write SDA1D01;
065500100126           write Protect;
065600100126           *in28 = *on;
065700100126           *in90 = *on;
065800100126      /end-free
065900060421e   1c                   endif
066000060419      *
066100060419      * Emissione videata
066200060509     c                   exfmt     SDA1D02
066300060419     c                   setoff                                       28  90
066400060419     c                   movea     *zeros        *in(50)
066500060419     c                   clear                   V1Dmsg
066600060419      *
066700060419     c                   SELECT
066800060419      *
066900100126      * F3 = Fine
067000060419w   1c                   WHEN      *inKC
067100100126     c                   exsr      sr_F03D01
067200060419      *
067300100126      * F12 = Ritorno
067400060419w   1c                   WHEN      *inKL
067500100126     c                   exsr      sr_F12D02
067600060419      *
067700100126      * F21 = Elenco LdV
067800060419w   1c                   WHEN      *inKV
067900100126     c                   exsr      sr_F21D02
068000060419      *
068100100126      * F6 = Esegue
068200060419w   2c                   WHEN      *inKF
068300100126     c                   exsr      sr_F06D02
068400060419      *
068500060419e   1c                   ENDSL
068600060419      *
068700060419     c                   ENDSR
068800060419      *
068900060419      *---------------------------------------------------------------*
069000060419      *?Gestione tasto funzionale F12 da videata D02                 ?*
069100060419      *---------------------------------------------------------------*
069200100126     c     sr_F12D02     BEGSR
069300060419      *
069400060419      * Ritorno alla videata precedente
069500060419     c                   eval      $Video  =  '1'
069600060419     c                   reset                   $InzS01
069700060419      *
069800060419     c                   ENDSR
069900060419      *
070000060419      *---------------------------------------------------------------*
070100060419      *?Gestione tasto funzionale F21 da videata D02                 ?*
070200060419      *---------------------------------------------------------------*
070300100126     c     sr_F21D02     BEGSR
070400060419      *
070500060419     c                   eval      $Video  =  '3'
070600100126     c                   eval      $InzS01 = *on
070700060419      *
070800060419     c                   ENDSR
070900100126
071000100126      /free
071100100126
071200100126       //--------------------------------------------------------------
071300100126       //?Inizializzazione videata D02                                 ?
071400100126       //--------------------------------------------------------------
071500100126       BEGSR  sr_InzD02;
071600100126
071700100126         // - Pulizia D02 (totali)
071800100126         clear  SDA1D02;
071900100126
072000100126         clear  V1Dmsg;
072100100126         *in28 = *off;
072200100126         *in90 = *off;
072300100126
072400100126         // - Caricamento dati (in schiere)
072500100127         clear  jj;
072600100126         reset  $Sped;
072700100126         reset  $nrrBLP;
072800100126         reset  $nrrARB;
072900100126         reset  $nrrTAS;
073000100126         reset  $SpedAR6;
073100100126         reset  $SpedCSB;
073200100128         reset  $AltreSped;
073300100126
073400100126         clear  yy;
073500100126
073600100126         For  yy = 1  To  %elem($File);
073700100126           exsr  sr_SQL;
073800100126         EndFor;
073900100127
074000100127         WjjMax = jj;
074100100128
074200100128         // - (Dis)Abilitazione F6=Conferma
074300100128         *in30 = (WjjMax = *zeros);
074400100126
074500100126       ENDSR;
074600100126
074700100126       //--------------------------------------------------------------
074800100126       //?Gestione singolo record estratto (D02)                       ?
074900100126       //--------------------------------------------------------------
075000100126       BEGSR  sr_RieD02;
075100100127
075200100127         // - Preparazione struttura dati ds_Sped
075300100127         clear  ds_Sped;
075400100127         wwwAAS = SQLaas;
075500100127         wwwLNP = SQLlnp;
075600100127         wwwNRS = SQLnrs;
075700100127         wwwNSP = SQLnsp;
075800100126
075900100126         // - Verifica se spedizione già elaborata su un altro file
076000100127         //   Se NO: memorizzazione spedizione
076100100127         //jj = %lookup( ds_Sped : $Sped );
076200100127         //if  jj = *zero;
076300100127         //  jj = %lookup( *blank : $Sped );
076400100127         //  *inH1 = (jj = *zero);            //  <«=-*?"H1"?
076500100127         //  $Sped(jj) = ds_Sped;
076600100127         //endif;
076700100127
076800100127         // - Verifica se raggiunto il n° max di spedizioni elaborabili
076900100127         if  jj = %elem($Sped);
077000100127           *inH1 = *on;                       //  <«=-*?"H1"?
077100100127           $EoF  = *on;
077200100128           yy = *hival;
077300100127           $AltreSped = *on;
077400100128           // - impostazione messaggio da emettere a video
077500100128           *in28 = *on;
077600100128           *in90 = *on;
077700100128           V1Dmsg = $Msg(10);
077800100128           // - uscita
077900100127           leavesr;
078000100127         endif;
078100100127
078200100127         // - Memorizzazione spedizione
078300100127         jj += 1;
078400100127         $Sped(jj) = ds_Sped;
078500100126
078600100126         // - Memorizzazione NRR del file da cui stato estratto il rec.
078700100126         //   & Conteggio dei record estratti
078800100126         select;
078900100126           when  $File(yy) = 'FNBLP00F  ';
079000100126             $nrrBLP(jj) = SQLrrn;
079100100126             V2Cblp += 1;
079200100127             exsr  sr_CtrFIAR6;
079300100126           when  $File(yy) = 'FNARB00F  ';
079400100126             $nrrARB(jj) = SQLrrn;
079500100126             V2Carb += 1;
079600100127             exsr  sr_CtrFIAR6;
079700100126           when  $File(yy) = 'TITAS00F  ';
079800100126             $nrrTAS(jj) = SQLrrn;
079900100126             V2Ctas += 1;
080000100127             if  V1Ckso <> V1Cksn;
080100100127               exsr  sr_CtrTNCSB;
080200100127             endif;
080300100126         endsl;
080400100126
080500100126       ENDSR;
080600100126
080700100126      /end-free
080800060418      *
080900060418      *---------------------------------------------------------------*
081000060418      *?Gestione videata C01/S01                                     ?*
081100060418      *---------------------------------------------------------------*
081200100126     c     sr_GesS01     BEGSR
081300060418      *
081400060418      * Inizializzazione videata
081500060419if  1c                   if        $InzS01 =  *on
081600100127     c*//                exsr      sr_InzS01
081700100127     c                   exsr      sr_RollUpS01
081800100127     c                   movel     *off          $InzS01
081900060418e   1c                   endif
082000060418      *
082100060420if  1c                   if        NOT *in90
082200060418      * Emissione testata
082300060509     c                   write     SDA1T01
082400060418      * Emissione piede
082500060509     c                   write     SDA1Z03
082600060420e   1c                   endif
082700060508      *
082800060508      * Posizionamento cursore
082900100125     c                   if        C01csr  >  *zeros
083000100125     c                   eval      C01rcd  =  C01csr
083100100125     c                   endif
083200060418      *
083300060418      * Emissione videata
083400060509     c                   exfmt     SDA1C01
083500060418     c                   setoff                                       28  90
083600060418     c                   movea     *zeros        *in(50)
083700060418     c                   clear                   V1Dmsg
083800060418      *
083900060420sel 1c                   SELECT
084000060418      *
084100100126      * F3 = Fine
084200060418w   1c                   WHEN      *inKC
084300100126     c                   exsr      sr_F03D01
084400060418      *
084500100126      * F12 = Ritorno (a D02, non a D01)
084600060419w   1c                   WHEN      *inKL
084700100126     c                   exsr      sr_F12S01
084800060418      *
084900060418      * Controllo opzioni immesse a video
085000060418x   1c                   OTHER
085100100126     c                   exsr      sr_OpzS01
085200060420if  2c                   if        *in90
085300060420     c                   leavesr
085400060420e   2c                   endif
085500060428sel 2c                   select
085600100126      * RollUp = Pagina successiva
085700100127w   2c                   when      £Tasto = c_RollUp
085800100127     c                   exsr      sr_RollUpS01
085900100126      * RollDown = Pagina precedente
086000100127w   2c                   when      £Tasto = c_RollDown
086100100127     c                   exsr      sr_RollDownS01
086200100126      * F6 = Esegue
086300060428w   2c                   when      *inKF
086400100127     c                   exsr      sr_F06D02
086500060428e   2c                   endsl
086600060418      *
086700060418e   1c                   ENDSL
086800060418      *
086900060418     c                   ENDSR
087000060418      *
087100060418      *---------------------------------------------------------------*
087200100127      *?Inizializzazione videata C01/S01                             ?*
087300060418      *---------------------------------------------------------------*
087400100126     c     sr_InzS01     BEGSR
087500100126      *
087600100126      * Pulizia subfile-control
087700100126if  1c                   if        $InzS01 = *on
087800100126     c                   clear                   SDA1C01
087900100126     c                   eval      C1Cdsi  =  V1Cdsi
088000100126     c                   if        V1Cdsf  >  *zeros
088100100126     c                   eval      C1Cdsf  =  V1Cdsf
088200100126     c                   else
088300100126     c                   eval      C1Cdsf  =  C1Cdsi
088400100126     c                   endif
088500100127     c                   eval      C01pgt  =  %div( WjjMax : c_SflPag )
088600100127     c                   if        %rem( WjjMax : c_SflPag ) > *zero
088700100127     c                   eval      C01pgt += 1
088800100127     c                   endif
088900100126     c                   clear                   jj
089000100126e   1c                   endif
089100060418      *
089200060418      * Pulizia subfile
089300060418     c                   seton                                        3031
089400060509     c                   write     SDA1C01
089500060418     c                   setoff                                         3133
089600060418      *
089700060418     c                   eval      *in32   =  *off
089800100125     c                   clear                   C01rcd
089900100125     c                   clear                   C01csr
090000060418     c                   clear                   S01nrr
090100100126      *
090200100126     c                   ENDSR
090300100126
090400100126      /free
090500100126
090600100126       //--------------------------------------------------------------
090700100126       //?Paginazione avanti nel subfile (S01)                         ?
090800100126       //--------------------------------------------------------------
090900100127       BEGSR  sr_RollUpS01;
091000100126
091100100126         exsr  sr_InzS01;
091200100126
091300100127         C01pgv += 1;
091400100127         jj     =  ( ( C01pgv - 1 ) * c_SflPag );
091500100126         clear  wJJini;
091600100126         clear  wJJfin;
091700100126
091800100126         For  S01nrr = 1  To  c_SflPag;
091900100126
092000100126           jj += 1;
092100100126
092200100126           // - Fine delle spedizioni caricate
092300100127           if  $Sped ( jj ) <= *zero;
092400100126             S01nrr -= 1;
092500100126             jj     -= 1;
092600100126             leave;
092700100126           endif;
092800100126
092900100126           if  S01nrr = 1;
093000100126             wJJini = jj;
093100100126           endif;
093200100126           wJJfin = jj;
093300100126
093400100126           exsr  sr_RieS01;
093500100126
093600100126         EndFor;
093700100126
093800100126         // - Visualizzazione del SFL (se ci sono dati)
093900100126         *in30 = (S01nrr = *zeros);
094000100126
094100100126         // - Attivazione del SFLEND
094200100127         *in33 = ( $Sped ( wJJfin + 1 ) <= *zero );
094300100126
094400100126         // - (Dis)Attivazione RollDown
094500100127         *in34 = ( wJJini <= 1 );
094600100126
094700100126         // - Posizionamento cursore al 1° rcd della pagina
094800100126         if  S01nrr > *zero;
094900100126           C01rcd = 1;
095000100126         else;
095100100126           clear  C01rcd;
095200100126         endif;
095300100126         C01csr = C01rcd;
095400100126
095500100126       ENDSR;
095600100126
095700100126       //--------------------------------------------------------------
095800100126       //?Paginazione indietro nel subfile (S01)                       ?
095900100126       //--------------------------------------------------------------
096000100127       BEGSR  sr_RollDownS01;
096100100126
096200100126         exsr  sr_InzS01;
096300100126
096400100127         C01pgv -= 1;
096500100127         jj     =  ( ( C01pgv - 1 ) * c_SflPag );
096600100126         clear  wJJini;
096700100126         clear  wJJfin;
096800100126
096900100126         For  S01nrr = 1  To  c_SflPag;
097000100126
097100100126           jj += 1;
097200100126
097300100126           // - Fine delle spedizioni caricate
097400100127           if  $Sped ( jj ) <= *zero;
097500100126             S01nrr -= 1;
097600100126             jj     -= 1;
097700100126             leave;
097800100126           endif;
097900100126
098000100126           if  S01nrr = 1;
098100100126             wJJini = jj;
098200100126           endif;
098300100126           wJJfin = jj;
098400100126
098500100126           exsr  sr_RieS01;
098600100126
098700100126         EndFor;
098800100126
098900100126         // - Visualizzazione del SFL (se ci sono dati)
099000100126         *in30 = (S01nrr = *zeros);
099100100126
099200100126         // - Attivazione del SFLEND
099300100127         *in33 = ( $Sped ( wJJfin + 1 ) <= *zero );
099400100126
099500100126         // - (Dis)Attivazione RollDown
099600100127         *in34 = ( wJJini <= 1 );
099700100126
099800100126         // - Posizionamento cursore al 1° rcd della pagina
099900100126         if  S01nrr > *zero;
100000100126           C01rcd = 1;
100100100126         else;
100200100126           clear  C01rcd;
100300100126         endif;
100400100126         C01csr = C01rcd;
100500100126
100600100126       ENDSR;
100700100126
100800100126       //--------------------------------------------------------------
100900100126       //?Gestione singolo record estratto (C01/S01)                   ?
101000100126       //--------------------------------------------------------------
101100100126       BEGSR  sr_RieS01;
101200100126
101300100126         // - Riempimento campi nel record del subfile
101400100126
101500100126         clear  SDA1S01;
101600100126
101700100126         ds_Sped = $Sped(jj);
101800100126         VH1jj  = jj;
101900100127         VH1aas = WWWaas;
102000100127         VS1lnp = WWWlnp;
102100100127         VS1nrs = WWWnrs;
102200100127         VS1nsp = WWWnsp;
102300100127         select;
102400100127           when  $nrrBLP(jj) <> *zero;
102500100127             VH1rrn = $nrrBLP(jj);
102600100127           when  $nrrARB(jj) <> *zero;
102700100127             VH1rrn = $nrrARB(jj);
102800100127           when  $nrrTAS(jj) <> *zero;
102900100127             VH1rrn = $nrrTAS(jj);
103000100127         endsl;
103100100127
103200100127         if  VH1rrn < *zero;
103300100127           VS1opz = 'E';
103400100127         endif;
103500100126
103600100127         reset  WLBdat;
103700100126
103800100126         select;
103900100126
104000100126           // - dati da FNBLP00F
104100100127           when  $nrrBLP(jj) <> *zero;
104200100126             VH1pf  = $File(1);
104300100127             VS1PF  = 'P';
104400100127             SQLrrn = %abs($nrrBLP(jj));
104500100127             chain(n)  SQLrrn  FNBLP000;
104600100126             if  %found(FNBLP00F);
104700100126               VS1lna = BLPlna;
104800100126               VS1ksc = BLPksc;
104900100126               G08inv = (BLPaas * 10000) + BLPmgs;
105000100126             endif;
105100100126
105200100126           // - dati da FNARB00F
105300100127           when  $nrrARB(jj) <> *zero;
105400100126             VH1pf  = $File(2);
105500100127             VS1PF  = 'A';
105600100127             SQLrrn = %abs($nrrARB(jj));
105700100127             chain(n)  SQLrrn  FNARB000;
105800100126             if  %found(FNARB00F);
105900100126               VS1lna = ARBlna;
106000100126               VS1ksc = ARBksc;
106100100126               G08inv = (ARBaas * 10000) + ARBmgs;
106200100126             endif;
106300100126
106400100126           // - dati da TITAS00F
106500100127           when  $nrrTAS(jj) <> *zero;
106600100126             VH1pf  = $File(3);
106700100127             VS1PF  = 'S';
106800100127             SQLrrn = %abs($nrrTAS(jj));
106900100127             chain(n)  SQLrrn  TITAS00F;
107000100127             if  %found(TITAS00F);
107100100126               VS1lna = TASlna;
107200100126               VS1ksc = TASksc;
107300100126               G08inv = (TASaas * 10000) + TASmgs;
107400100126             endif;
107500100126
107600100126         endsl;
107700100126
107800100126         XSRDA8 ( WLBdat );
107900100126         VS1dts = G08dat;
108000100126         // - Decodifica cliente
108100100126         exsr  sr_CallTIBS69;
108200100127         VS1rsc = %subst ( ACOrag : 1 : %len(VS1rsc) );
108300100126
108400100126         // - Scrittura record subfile
108500100126         write  SDA1S01;
108600100126
108700100126       ENDSR;
108800100126
108900100126      /end-free
109000100126      *
109100100126      *---------------------------------------------------------------*
109200101130      *?Reperimento dati anagrafici del cliente                      ?*
109300100126      *---------------------------------------------------------------*
109400100126     c     sr_CallTIBS69 BEGSR
109500100126      *
109600100126if  1c                   if        VS1ksc <> I69kac
109700100126     c                   clear                   TIBS69ds
109800100126     c                   z-add     VS1ksc        I69kac
109900100126     c                   call      'TIBS69R'
110000100126     c                   parm                    TIBS69ds
110100100126     c                   parm                    ds_CNACO
110200100126     c                   parm                    ds_CNIND
110300100126     c                   parm                    ds_CNCLP
110400100126     c                   parm                    ds_FNCLS
110500100126e   1c                   endif
110600100126      *
110700100126     c                   ENDSR
110800060421      *
110900060421      *---------------------------------------------------------------*
111000060421      *?Reperimento dati da FIAR601L (tassazione bolla)              ?*
111100060421      *---------------------------------------------------------------*
111200100126     c     sr_CtrFIAR6   BEGSR
111300060421      *
111400060421      * Verifica se spedizione già elaborata (da altro file)
111500101130     c                   eval      ww = %lookup( ds_Sped : $Sped )
111600101130     c                   if                 ww  > *zero  and
111700101130     c                             $SpedAR6(ww) > *zero
111800060421     c                   leavesr
111900060421     c                   endif
112000060421      *
112100060421      * Verifica se spedizione con dati relativi alla tassazione
112200060421     c     K04AR601      setll     FIAR6000
112300060421     c     K04AR601      reade     FIAR6000
112400060421      *
112500060421     c                   DOW       NOT %eof(FIAR601L)
112600060421      *
112700060421     c                   add       1             V2Car6
112800100126     c                   add       1             $SpedAR6(jj)
112900060421      *
113000060421     c     K04AR601      reade     FIAR6000
113100060421      *
113200060421     c                   ENDDO
113300060421      *
113400060421     c                   ENDSR
113500090225      /free
113600090225       //--------------------------------------------------------------
113700090225       //?Reperimento dati da TNCSB03L (contrassegni bolle)            ?
113800090225       //--------------------------------------------------------------
113900100126       BEGSR  sr_CtrTNCSB;
114000090225
114100090225         // Verifica se spedizione già elaborata (da altro file)
114200101130         ww = %lookup( ds_Sped : $Sped );
114300101130         if  ww  > *zero  and  $SpedCSB(ww) = *on;
114400101130           leavesr;
114500101130         endif;
114600100126
114700090225         // Verifica se spedizione con dati relativi alla tassazione
114800090227         // (ed eventualmente la memorizza)
114900090227         chain(n)  K04AR601  TNCSB000;
115000090227         if  %found(TNCSB03L)   and   CSBbna = *zero   and
115100090227             (CSBksc = V1Ckso   or    CSBcdi = V1Ckso);
115200100126           V2Ccsb      += 1;
115300100126           $SpedCSB(jj) = *on;
115400090227         endif;
115500090225
115600090225       ENDSR;
115700090225
115800090225      /end-free
115900060418      *
116000060418      *---------------------------------------------------------------*
116100060418      *?Gestione opzioni videata C01/S01                             ?*
116200060418      *---------------------------------------------------------------*
116300100126     c     sr_OpzS01     BEGSR
116400060418      *
116500060509     c                   readc     SDA1S01
116600060420      *
116700100315do  1c                   DOW       NOT %eof(TNSDA1D)
116800060419      *
116900100125     c                   z-add     S01nrr        C01rcd
117000100127      /free
117100100127
117200100127           Select;
117300100127
117400100127             //- Opzione non variata
117500100127             When  VS1opz = *blank   and   VH1rrn > *zero;
117600100127               *in32  = *off;                              // NO sflnxtchg
117700100127             When  VS1opz = 'E'      and   VH1rrn < *zero;
117800100127               *in32  = *off;                              // NO sflnxtchg
117900100127
118000100127             // - Immessa "E"
118100100127             When  VS1opz = 'E'      and   VH1rrn > *zero;
118200100127               *in32  = *on;                               // SÌ sflnxtchg
118300100127               VH1rrn *= -1;
118400100127               select;
118500100127                 when  VH1pf = 'FNBLP00F  ';
118600100127                   $nrrBLP(VH1jj) *= -1;
118700100127                   V2Cblp -= 1;
118800100127                 when  VH1pf = 'FNARB00F  ';
118900100127                   $nrrARB(VH1jj) *= -1;
119000100127                   V2Carb -= 1;
119100100127                 when  VH1pf = 'TITAS00F  ';
119200100127                   $nrrTAS(VH1jj) *= -1;
119300100127                   V2Ctas -= 1;
119400100127               endsl;
119500100127               if  $SpedAR6(VH1jj) > *zero;
119600100127                 V2Car6 -= $SpedAR6(VH1jj);
119700100127               endif;
119800100127               if  $SpedCSB (VH1jj) > *zero;
119900100127                 V2Ccsb -= 1;
120000100127               endif;
120100100127
120200100127             // - Tolta "E"
120300100127             When  VS1opz = *blank   and   VH1rrn < *zero;
120400100127               *in32  = *on;                               // SÌ sflnxtchg
120500100127               VH1rrn = %abs(VH1rrn);
120600100127               select;
120700100127                 when  VH1pf = 'FNBLP00F  ';
120800100127                   $nrrBLP(VH1jj) = %abs($nrrBLP(VH1jj));
120900100127                   V2Cblp += 1;
121000100127                 when  VH1pf = 'FNARB00F  ';
121100100127                   $nrrARB(VH1jj) = %abs($nrrARB(VH1jj));
121200100127                   V2Carb += 1;
121300100127                 when  VH1pf = 'TITAS00F  ';
121400100127                   $nrrTAS(VH1jj) = %abs($nrrTAS(VH1jj));
121500100127                   V2Ctas += 1;
121600100127               endsl;
121700100127               if  $SpedAR6(VH1jj) > *zero;
121800100127                 V2Car6 += $SpedAR6(VH1jj);
121900100127               endif;
122000100127               if  $SpedCSB (VH1jj) > *zero;
122100100127                 V2Ccsb += 1;
122200100127               endif;
122300100127
122400100127e   2      EndSl;
122500100127
122600100127      /end-free
122700060420      *
122800060420      * - aggiorno il SFL
122900060509     c                   update    SDA1S01
123000060420      * - se rilevato errore esco dal ciclo di controllo
123100100125if  2c                   if        *in28   = *on
123200100125     c                   z-add     C01rcd        C01csr
123300100125     c                   leave
123400100125e   2c                   endif
123500060420      *
123600060509     c                   readc     SDA1S01
123700060419      *
123800060420e   1c                   ENDDO
123900060418      *
124000060420     c                   ENDSR
124100100127
124200100127      /free
124300100127
124400100127       //--------------------------------------------------------------
124500100127       //?Gestione tasto funzionale F6 da videata C01/S01              ?
124600100127       //--------------------------------------------------------------
124700100127       BEGSR  sr_F06D02;
124800100127
124900100127         // - Ciclo di elaborazione subfile per aggiornamento archivi
125000100127         For  jj = 1  To  %elem($Sped);
125100100127
125200100127           // - Fine spedizioni da elaborare
125300100127           if  $Sped(jj) <= *zero;
125400100127             leave;
125500100127           endif;
125600100127
125700100127           // - Spedizione esclusa ("E") dall'aggiornamento
125800100127           if  $nrrBLP(jj) < *zero   or
125900100127               $nrrARB(jj) < *zero   or
126000100127               $nrrTAS(jj) < *zero;
126100100127             iter;
126200100127           endif;
126300100127
126400100127           ds_Sped = $Sped(jj);
126500100127
126600100127           exsr  sr_UpdateSped;
126700100127
126800100127         EndFor;
126900100127
127000100127         if  Not $AltreSped;
127100100127           // - Imposta ritorno alla 1ª videata
127200100127           $Video  = '1';
127300100127           $InzD01 = *on;
127400100127         else;
127500100127           // - Imposta ritorno alla 2ª videata
127600100127           $Video  = '2';
127700100127         endif;
127800100127         $InzD02 = *on;
127900100127         $InzS01 = *on;
128000100127
128100100127       ENDSR;
128200100127
128300100127      /end-free
128400060419      *
128500060419      *---------------------------------------------------------------*
128600060508      *?Gestione tasto funzionale F12 da videata C01/S01             ?*
128700060419      *---------------------------------------------------------------*
128800100126     c     sr_F12S01     BEGSR
128900060419      *
129000060419     c                   eval      $Video  =  '2'
129100060419      *
129200060419     c                   ENDSR
129300060418      *
129400060418      *---------------------------------------------------------------*
129500060418      *?Gestione SQL per singolo archivio (TITAS/FNBLP/FNARB)        ?*
129600060418      *---------------------------------------------------------------*
129700060418     c     sr_SQL        BEGSR
129800060418      *
129900060418sel 1c                   select
130000060418      *
130100060418      * Gestione file FNBLP00F
130200060419w   1c                   when      $File(yy) = 'FNBLP00F  '
130300100126     c                   exsr      sr_PrepSqlBLP
130400060418      *
130500060418      * Gestione file FNARB00F
130600060419w   1c                   when      $File(yy) = 'FNARB00F  '
130700100126     c                   exsr      sr_PrepSqlARB
130800060421      *
130900060421      * Gestione file TITAS00F
131000060421w   1c                   when      $File(yy) = 'TITAS00F  '
131100100126     c                   exsr      sr_PrepSqlTAS
131200060418      *
131300060418e   1c                   endsl
131400060418      *
131500060418     c                   reset                   $EoF
131600100126     c                   exsr      sr_ExecSQL
131700060418      *
131800060418     c                   ENDSR
131900060412      *
132000060412      *---------------------------------------------------------------*
132100060418      *?Preparazione stringa SQL per estrazione dati da TITAS00F     ?*
132200060412      *---------------------------------------------------------------*
132300100126     c     sr_PrepSqlTAS BEGSR
132400060412      *
132500060412     c                   clear                   wSQL
132600060419     c                   clear                   xx
132700060412      *
132800060508      * Impostazione delle selezioni (se immesse)
132900060419do  1c                   DOW       xx      <  %elem($Sql_TITAS)
133000060419      *
133100060418     c                   eval      xx      =  xx + 1
133200060508if  2c                   if
133300060508      * - Data Spedizione (range)
133400100127     c*** obbligatorio:                (xx     =  3
133500060508     c***       "                  and  V1Cdsi =  *zeros
133600060508     c***       "                  and  V1Cdsf =  *zeros)
133700060508     c***                          OR
133800090225      * - Fil. di Partenza
133900100127     c                                 (xx     =  4
134000060508     c                             and  V1Clnp =  *all'9')
134100090225      * - Fil. di Arrivo
134200100127     c                             OR  (xx     =  5
134300060508     c                             and  V1Clna =  *all'9')
134400060508      * - Codice Bolla 1 e 2 (elenco)
134500100127     c                             OR  (xx     =  6
134600090227     c                             and  ds_V1Ccbx = *blanks)
134700090225      * - Codice Cliente attuale - TASksc / TASccm
134800100127     c*** obbligatorio:            OR  (xx     =  7
134900060508     c***       "                  and  V1Ckso =  *zeros)
135000060508      * - Codice Tariffa attuale
135100100127     c                             OR  (xx     =  8
135200060419     c                             and  V1Ccto =  *blanks)
135300060419     c                   iter
135400060508e   2c                   endif
135500060419      *
135600060418     c                   eval      wSQL    =  %trim(wSQL)
135700060418     c                                     +  ' '
135800060418     c                                     +  %trim($Sql_TITAS(xx))
135900100127if  2c                   if        xx      >= 3
136000100126     c                   exsr      sr_Add_Selez
136100060419e   2c                   endif
136200060419      *
136300060419e   1c                   ENDDO
136400060412      *
136500060412     c                   ENDSR
136600060418      *
136700060418      *---------------------------------------------------------------*
136800060418      *?Preparazione stringa SQL per estrazione dati da FNBLP00F     ?*
136900060418      *---------------------------------------------------------------*
137000100126     c     sr_PrepSqlBLP BEGSR
137100060418      *
137200060418     c                   clear                   wSQL
137300060419     c                   clear                   xx
137400060418      *
137500060508      * Impostazione delle selezioni (se immesse)
137600060419do  1c                   DOW       xx      <  %elem($Sql_FNBLP)
137700060419      *
137800060418     c                   eval      xx      =  xx + 1
137900060508if  2c                   if
138000060508      * - Data Spedizione (range)
138100100127     c*** obbligatorio:                (xx     =  3
138200060508     c***       "                  and  V1Cdsi =  *zeros
138300060508     c***       "                  and  V1Cdsf =  *zeros)
138400060508     c***                          OR
138500090225      * - Fil. di Partenza
138600100127     c                                 (xx     =  4
138700060508     c                             and  V1Clnp =  *all'9')
138800090225      * - Fil. di Arrivo
138900100127     c                             OR  (xx     =  5
139000060508     c                             and  V1Clna =  *all'9')
139100060508      * - Codice Bolla 1 e 2 (elenco)
139200100127     c                             OR  (xx     =  6
139300090227     c                             and  ds_V1Ccbx = *blanks)
139400090225      * - Codice Cliente attuale - BLPksc / BLPccm
139500100127     c*** obbligatorio:            OR  (xx     =  7
139600060508     c***       "                  and  V1Ckso =  *zeros)
139700060508      * - Codice Tariffa attuale
139800100127     c                             OR  (xx     =  8
139900060419     c                             and  V1Ccto =  *blanks)
140000060419     c                   iter
140100060508e   2c                   endif
140200060419      *
140300060418     c                   eval      wSQL    =  %trim(wSQL)
140400060418     c                                     +  ' '
140500060418     c                                     +  %trim($Sql_FNBLP(xx))
140600100127if  2c                   if        xx      >= 3
140700100126     c                   exsr      sr_Add_Selez
140800060419e   2c                   endif
140900060419      *
141000060419e   1c                   ENDDO
141100060418      *
141200060418     c                   ENDSR
141300060418      *
141400060418      *---------------------------------------------------------------*
141500060418      *?Preparazione stringa SQL per estrazione dati da FNARB00F     ?*
141600060418      *---------------------------------------------------------------*
141700100126     c     sr_PrepSqlARB BEGSR
141800060418      *
141900060418     c                   clear                   wSQL
142000060419     c                   clear                   xx
142100060418      *
142200060508      * Impostazione delle selezioni (se immesse)
142300060419do  1c                   DOW       xx      <  %elem($Sql_FNARB)
142400060419      *
142500060418     c                   eval      xx      =  xx + 1
142600060508if  2c                   if
142700060508      * - Data Spedizione (range)
142800100127     c*** obbligatorio:                (xx     =  3
142900060508     c***       "                  and  V1Cdsi =  *zeros
143000060508     c***       "                  and  V1Cdsf =  *zeros)
143100060508     c***                          OR
143200090225      * - Fil. di Partenza
143300100127     c                                 (xx     =  4
143400060508     c                             and  V1Clnp =  *all'9')
143500090225      * - Fil. di Arrivo
143600100127     c                             OR  (xx     =  5
143700060508     c                             and  V1Clna =  *all'9')
143800060508      * - Codice Bolla 1 e 2 (elenco)
143900100127     c                             OR  (xx     =  6
144000090227     c                             and  ds_V1Ccbx = *blanks)
144100090225      * - Codice Cliente attuale - ARBksc / ARBccm
144200100127     c*** obbligatorio:            OR  (xx     =  7
144300060508     c***       "                  and  V1Ckso =  *zeros)
144400060508      * - Codice Tariffa attuale
144500100127     c                             OR  (xx     =  8
144600060419     c                             and  V1Ccto =  *blanks)
144700060419     c                   iter
144800060508e   2c                   endif
144900060419      *
145000060418     c                   eval      wSQL    =  %trim(wSQL)
145100060418     c                                     +  ' '
145200060418     c                                     +  %trim($Sql_FNARB(xx))
145300100127if  2c                   if        xx      >= 3
145400100126     c                   exsr      sr_Add_Selez
145500060419e   2c                   endif
145600060419      *
145700060419e   1c                   ENDDO
145800060418      *
145900060418     c                   ENDSR
146000060419      *
146100060419      *---------------------------------------------------------------*
146200060419      *?Aggiunta selezioni nella stringa SQL (di tutti gli archivi)  ?*
146300060419      *---------------------------------------------------------------*
146400100126     c     sr_Add_Selez  BEGSR
146500060419      *
146600060419sel 1c                   SELECT
146700060508      * - Range Date Spedizione
146800100127w   1c                   WHEN      xx      =  3
146900060419     c                   eval      wSQL    =  %trim(wSQL)
147000060419     c                                     +  ' '
147100060508     c                                     +  %trim(%char(W1Cdsi))
147200060419     c                                     +  ' and '
147300060508     c                                     +  %trim(%char(W1Cdsf))
147400090225      * - Fil. di Partenza
147500100127w   1c                   WHEN           xx     =  4
147600060419     c                             and  V1Clnp >  *zeros
147700060419     c                   eval      wSQL    =  %trim(wSQL)
147800060419     c                                     +  ' '
147900060419     c                                     +  %trim(%char(V1Clnp))
148000090225      * - Fil. di Arrivo
148100100127w   1c                   WHEN           xx     =  5
148200060508     c                             and  V1Clna >  *zeros
148300060508     c                   eval      wSQL    =  %trim(wSQL)
148400060508     c                                     +  ' '
148500060508     c                                     +  %trim(%char(V1Clna))
148600090225      * - Codici Bolla (1 <=> 11)
148700090225      /free
148800100127           WHEN  xx = 6   and   ds_V1Ccbx <> *blank;
148900090225             clear  ww;
149000100125             For  wx = 1  To  %elem($V1Ccbx);
149100100125               if  $V1Ccbx(wx) <> *blank;
149200090227                 if  ww > *zero;
149300090227                   wSQL = %trimr(wSQL) + ',';
149400090227                 endif;
149500100125                 wSQL = %trimr(wSQL) + '''' + $V1Ccbx(wx) + '''';
149600090225                 ww += 1;
149700090225               endif;
149800090225             EndFor;
149900090225             wSQL = %trimr(wSQL) + ')';
150000090225      /end-free
150100090225      * - Codice Cliente (xxxKSC / xxxCCM)
150200100127w   1c                   WHEN           xx     =  7
150300060419     c                             and  V1Ckso <> *zeros
150400060419     c                   eval      wSQL    =  %trim(wSQL)
150500060419     c                                     +  ' '
150600060419     c                                     +  %trim(%char(V1Ckso))
150700090225      /free
150800090225             if  V1Ckso = V1Cksn;
150900090225               wSQL = %trimr(wSQL) + ')';
151000090225             else;
151100090225               select;
151200090227                 when  $File(yy) = 'FNBLP00F';
151300090225                   wSQL = %trimr(wSQL) + ' or BLPccm =';
151400090227                 when  $File(yy) = 'FNARB00F';
151500090225                   wSQL = %trimr(wSQL) + ' or ARBccm =';
151600090227                 when  $File(yy) = 'TITAS00F';
151700090225                   wSQL = %trimr(wSQL) + ' or TASccm =';
151800090225               endsl;
151900090225               wSQL = %trimr(wSQL) + ' ' +  %trim(%char(V1Ckso)) + ')';
152000090225             endif;
152100090225      /end-free
152200060508      * - Codice Tariffa
152300100127w   1c                   WHEN           xx     =  8
152400060419     c                             and  V1Ccto <> *blanks
152500060419     c                   eval      wSQL    =  %trim(wSQL)
152600060419     c                                     +  ' '
152700060419     c                                     +  %trim(V1Ccto)
152800060419e   1c                   ENDSL
152900060419      *
153000060419     c                   ENDSR
153100060418      *
153200060418      *---------------------------------------------------------------*
153300060418      *?Esecuzione stringa SQL per ingolo archivio                   ?*
153400060418      *---------------------------------------------------------------*
153500100126     c     sr_ExecSQL    BEGSR
153600060418      *
153700060418      * - Apertura cursore
153800100126     c                   exsr      sr_OpenCursor
153900060418      *
154000060418      * - Esecuzione SQL
154100100126     c                   dou       $Eof = *on
154200100126     c                   exsr      sr_ReadCursor
154300060418     c                   enddo
154400060418      *
154500060418      * - Chiusura cursore
154600100126     c                   exsr      sr_CloseCursor
154700060418      *
154800060418     c                   ENDSR
154900060418      *
155000060418      *---------------------------------------------------------------*
155100060418      *?Dichiarazione e Apertura cursore                             ?*
155200060418      *---------------------------------------------------------------*
155300100126     c     sr_OpenCursor BEGSR
155400060418      *
155500060418      * Dichiarazione cursore
155600060418      *
155700060418     c/exec sql
155800060418     c+     PREPARE S1 FROM :wSQL
155900060418     c/end-exec
156000060418     c*
156100060418     c/exec sql
156200060418     c+     DECLARE C1 CURSOR FOR S1
156300060418     c/end-exec
156400060418      *
156500060418      * Apertura del cursore
156600060418      *
156700060418     c/exec sql
156800060418     c+     OPEN C1
156900060418     c/end-exec
157000060418      *
157100060418     c                   ENDSR
157200060418      *
157300060418      *---------------------------------------------------------------*
157400060418      *?Lettura cursore                                              ?*
157500060418      *---------------------------------------------------------------*
157600100126     c     sr_ReadCursor BEGSR
157700060418      *
157800060418     c                   clear                   wSQL_ds
157900060418     c/exec sql
158000060418     c+     FETCH NEXT FROM C1 INTO :wSQL_ds
158100060418     c/end-exec
158200060418      *
158300060418sel 1c                   select
158400060418w   1c                   when      SQLcod  =  100
158500060418     c                   eval      $EoF    =  *on
158600060418w   1c                   when      SQLcod  <  *zeros
158700060418     c                   eval      $Err    =  *on
158800060418     c                   eval      $Eof    =  *on
158900060418x   1c                   other
159000100126     c                   exsr      sr_RieD02
159100060418e   1c                   endsl
159200060418      *
159300060418     c                   ENDSR
159400060418      *
159500060418      *---------------------------------------------------------------*
159600060418      *?Chiusura cursore                                             ?*
159700060418      *---------------------------------------------------------------*
159800100126     c     sr_CloseCursorBEGSR
159900060418      *
160000060418     c/exec sql
160100060418     c+     CLOSE C1
160200060418     c/end-exec
160300060418      *
160400060418     c                   ENDSR
160500060419      *
160600060419      *---------------------------------------------------------------*
160700060428      *?Modifica dati delle Spedizioni                               ?*
160800060419      *---------------------------------------------------------------*
160900100126     c     sr_UpdateSped BEGSR
161000060419      *
161100060419sel 1c                   SELECT
161200060419      *
161300060419      * -FNBLP00F?
161400100127w   1c                   WHEN      $nrrBLP(jj) > *zero
161500100127     c     $nrrBLP(jj)   chain     FNBLP000
161600060508if  2c                   if             %found(FNBLP00F)
161700090225     c                             and (BLPksc  =  V1Ckso
161800090225     c                              or (BLPccm  =  V1Ckso  and
161900090225     c                                  V1Ckso  <> V1Cksn))
162000060419      * - modifica codice cliente
162100090225      /free
162200090225           if  BLPksc = V1Ckso;
162300090225             BLPksc = V1Cksn;
162400090225           endif;
162500090225           if  BLPccm = V1Ckso   and   V1Ckso <> V1Cksn;
162600090225             BLPccm = V1Cksn;
162700090225           endif;
162800090225      /end-free
162900060419      * - modifica codice tariffa
163000060508     c                   if             V1Cctn  <> *blanks
163100060508     c                             and (V1Ccto  =  *blanks
163200090302     c                             or   BLPctr  =  %int(V1Ccto))
163300090302     c                   eval      BLPctr  =  %int(V1Cctn)
163400060419     c                   endif
163500060419      *                  __________________
163600060419     c                   UPDATE    FNBLP000
163700060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
163800060508e   2c                   endif
163900060419      *
164000060419      * -FNARB00F?
164100100127w   1c                   WHEN      $nrrARB(jj) > *zero
164200100127     c     $nrrARB(jj)   chain     FNARB000
164300060508if  2c                   if             %found(FNARB00F)
164400090225     c                             and (ARBksc  =  V1Ckso
164500090225     c                              or (ARBccm  =  V1Ckso  and
164600090225     c                                  V1Ckso  <> V1Cksn))
164700060419      * - modifica codice cliente
164800090225      /free
164900090225           if  ARBksc = V1Ckso;
165000090225             ARBksc = V1Cksn;
165100090225           endif;
165200090225           if  ARBccm = V1Ckso   and   V1Ckso <> V1Cksn;
165300090225             ARBccm = V1Cksn;
165400090225           endif;
165500090225      /end-free
165600060419      * - modifica codice tariffa
165700060508     c                   if             V1Cctn  <> *blanks
165800060508     c                             and (V1Ccto  =  *blanks
165900090302     c                             or   ARBctr  =  %int(V1Ccto))
166000090302     c                   eval      ARBctr  =  %int(V1Cctn)
166100060419     c                   endif
166200060419      *                  __________________
166300060419     c                   UPDATE    FNARB000
166400060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
166500060508e   2c                   endif
166600060421      *
166700060421      * -TITAS00F?
166800100127w   1c                   WHEN      $nrrTAS(jj) > *zero
166900100127     c     $nrrTAS(jj)   chain     TITAS000
167000060508if  2c                   if             %found(TITAS00F)
167100090225     c                             and (TASksc  =  V1Ckso
167200090225     c                              or (TASccm  =  V1Ckso  and
167300090225     c                                  V1Ckso  <> V1Cksn))
167400060421      * - modifica codice cliente
167500090225      /free
167600090225           if  TASksc = V1Ckso;
167700090225             TASksc = V1Cksn;
167800090225           endif;
167900090225           if  TASccm = V1Ckso   and   V1Ckso <> V1Cksn;
168000090225             TASccm = V1Cksn;
168100090225           endif;
168200090225      /end-free
168300060421      * - modifica codice tariffa
168400060508     c                   if             V1Cctn  <> *blanks
168500060508     c                             and (V1Ccto  =  *blanks
168600090302     c                             or   TASctr  =  %int(V1Ccto))
168700090302     c                   eval      TASctr  =  %int(V1Cctn)
168800060421     c                   endif
168900060421      *                  __________________
169000060421     c                   UPDATE    TITAS000
169100060421      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
169200060421e   2c                   endif
169300060419      *
169400060419e   1c                   ENDSL
169500060419      *
169600060419      * -FIAR601L?
169700060508      *   (SOLO per FNBLP00F/FNARB00F)
169800100127if  1c                   IF            ($nrrBLP(jj)  > *zero
169900100127     c                             or   $nrrARB(jj)  > *zero)
170000100127     c                             and  $SpedAR6(jj) > *zero
170100060508      *
170200060419     c     K04AR601      setll     FIAR6000
170300060508if  2c                   if        not %equal(FIAR601L)
170400060419     c                   leavesr
170500060508e   2c                   endif
170600060419     c     K04AR601      reade     FIAR6000
170700060508      *
170800060508do  2c                   DOW       NOT %eof(FIAR601L)
170900060508      *
171000060509if  3c                   if        AR6ksc  =  V1Ckso
171100060509      * - modifica codice cliente
171200060419     c                   eval      AR6ksc  =  V1Cksn
171300060419      * - modifica codice tariffa
171400060508     c                   if             V1Cctn  <> *blanks
171500060508     c                             and (V1Ccto  =  *blanks
171600090302     c                             or   AR6ctr  =  %int(V1Ccto))
171700090302     c                   eval      AR6ctr  =  %int(V1Cctn)
171800060419     c                   endif
171900060419      *                  __________________
172000060419     c                   UPDATE    FIAR6000
172100060419      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
172200060509e   3c                   endif
172300060509      *
172400060419     c     K04AR601      reade     FIAR6000
172500060508      *
172600060508e   2c                   ENDDO
172700090227      *
172800090225e   1c                   ENDIF
172900090227      /free
173000090227
173100090227         // -TNCSB03L?
173200090227         //   (SOLO per TITAS00F  e  SE richiesta mod. del cod.cliente)
173300100127         IF  $nrrTAS(jj)  > *zero   and
173400100127             $SpedCSB(jj) = *on     and   V1Ckso <> V1Cksn;
173500090227           chain  K04AR601  TNCSB000;
173600090227           If  %found(TNCSB03L)  and
173700090227               CSBbna = *zero    and
173800090227              (CSBksc = V1Ckso   or   CSBcdi = V1Ckso);
173900090227             // - modifica codice/i cliente
174000090227             if  CSBksc = V1Ckso;
174100090227               CSBksc = V1Cksn;
174200090227             endif;
174300090227             if  CSBcdi = V1Ckso;
174400090227               CSBcdi = V1Cksn;
174500090227             endif;
174600090227             //_________________
174700090227             UPDATE    TNCSB000;
174800090227             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
174900090227           EndIf;
175000090227         ENDIF;
175100090227
175200090227      /end-free
175300090225      *
175400090225     c                   ENDSR
175500060418      *
175600060418      *---------------------------------------------------------------*
175700060418      *?Operazioni finali                                            ?*
175800060418      *---------------------------------------------------------------*
175900100126     c     sr_RoutEnd    BEGSR
176000060418      *
176100060418     c                   clear                   TIBS69ds
176200060419     c                   eval      I69tla  =  'C'
176300060418     c                   call      'TIBS69R'
176400060418     c                   parm                    TIBS69ds
176500060418      *
176600060418     c                   ENDSR
176700060428      *
176800060428      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
176900060428      *
177000060418** - $Msg -------------------------------------------------------------------*
177100060418Data formalmente errata                                                          1
177200060418Range di date errato                                                             2
177300090225Filiale di Partenza errata                                                       3
177400090225Filiale di Arrivo errata                                                         4
177500060508Codice Bolla errato                                                              5
177600060508Codice Cliente errato                                                            6
177700060508Codice Tariffa errato                                                            7
177800060508Codice "sostitutivo" obbligatorio se inserito quello "attuale"                   8
177900060508Codice "sostitutivo" uguale a quello "attuale"                                   9
178000100126ATTENZIONE: NON sono caricabili TUTTE le spedizioni => occorrerà RIESEGUIRE     10
178100060418** - $File
178200060421FNBLP00F  FNARB00F  TITAS00F
178300060418** - $Sql_FNBLP ------------------------------------------------*
178400100127select BLPaas, BLPlnp, BLPnrs, BLPnsp, rrn(FNBLP00F)                 1
178500100127from   FNBLP00F                                                      2
178600100127where  ((BLPaas * 10000) + BLPmgs) between                           3
178700100127  and  BLPlnp =                                                      4
178800100127  and  BLPlna =                                                      5
178900100127  and  BLPcbo in (                                                   6
179000100127  and (BLPksc =                                                      7
179100100127  and  BLPctr =                                                      8
179200100127order  by  BLPaas, BLPlnp, BLPnrs, BLPnsp                            9
179300060418** - $Sql_FNARB ------------------------------------------------*
179400100127select ARBaas, ARBlnp, ARBnrs, ARBnsp, rrn(FNARB00F)                 1
179500100127from   FNARB00F                                                      2
179600100127where  ((ARBaas * 10000) + ARBmgs) between                           3
179700100127  and  ARBlnp =                                                      4
179800100127  and  ARBlna =                                                      5
179900100127  and  ARBcbo in (                                                   6
180000100127  and (ARBksc =                                                      7
180100100127  and  ARBctr =                                                      8
180200100127order  by  ARBaas, ARBlnp, ARBnrs, ARBnsp                            9
180300060508** - $Sql_TITAS ------------------------------------------------*
180400100127select TASaas, TASlnp, TASnrs, TASnsp, rrn(TITAS00F)                 1
180500100127from   TITAS00F                                                      2
180600100127where  ((TASaas * 10000) + TASmgs) between                           3
180700100127  and  TASlnp =                                                      4
180800100127  and  TASlna =                                                      5
180900100127  and  TAScbo in (                                                   6
181000100127  and (TASksc =                                                      7
181100100127  and  TASctr =                                                      8
181200100127order  by  TASaas, TASlnp, TASnrs, TASnsp                            9
