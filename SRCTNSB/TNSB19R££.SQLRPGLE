000100070704      *PARMS OPTION(*NOXREF) CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE)
000200120612     /*PRM dbgview(*source)
000300120615     /*CMD ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000400120615     /*CMD        ovrscope(*calllvl)
000500120615     /*CMD ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
000600120615     /*CMD        ovrscope(*calllvl)
000700120615     /*CMD ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
000800120615     /*CMD        ovrscope(*calllvl)
000900120615     /*CMD ovrdbf file(TNCSB03L) tofile(GAITRAGRPS/TNCSB03L) +
001000120615     /*CMD        ovrscope(*calllvl)
001100120615     /*CMD ovrdbf file(WFNIB00F) tofile(GAITRAAZP/WFNIB00F) +
001200120615     /*CMD        ovrscope(*calllvl)
001300120615     /*CMD ovrdbf file(TITAA00F) tofile(GAITRAGRPS/TITAA00F) +
001400120615     /*CMD        ovrscope(*calllvl)
001500120615     /*CMD ovrdbf file(TITAS00F) tofile(GAITRAGRPS/TITAS00F) +
001600120615     /*CMD        ovrscope(*calllvl)
001700120615     /*CMD ovrdbf file(TITASS0F) tofile(GAITRAGRPS/TITASS0F) +
001800120615     /*CMD        ovrscope(*calllvl)
001900120615     /*END dltovr file(*all) lvl(*)
002000120612     /*END
002100050809      *===============================================================*
002200050809      *?TNSB19R * Ricerca nominativi in bolle sede per G.d.F.        ?*
002300050809      *===============================================================*
002400050902      *
002500050809     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600050809     h alwnull(*inputonly)
002700050902      *
002800050902      *?A R C H I V I?------------------------------------------------*
002900050902      *
003000050810     fTITAS30C  if   e           k disk
003100050901     fTITAA30C  if   e           k disk
003200061220      *
003300061220     fTABEL00F  if   e           k disk
003400061219     fFIAPD01L  if   e           k disk
003500061220     fTNCSB03L  if   e           k disk
003600061220     fTITA430C  if   e           k disk
003700050809      *
003800050902     fWFNIB00F  o    e             disk    usropn
003900070703     fQSYSPRT   o    f  132        printer usropn
004000070703     f                                     oflind(*inOF)
004100050902      *
004200050902      *?C O S T A N T I?----------------------------------------------*
004300050902      *
004400061220     d C_Union         c                   const(' UNION ')
004500061220     d C_ForFetch      c                   const(' FOR FETCH ONLY')
004600070703     d C_OvrPrtF       c                   const('OVRPRTF +
004700070703     d                                            FILE(QSYSPRT) +
004800070703     d                                            USRDTA(''TNSB19+
004900070703     d                                                     *ERR'')')
005000070703     d C_DltOvr        c                   const('DLTOVR +
005100070703     d                                            FILE(QSYSPRT)')
005200120615     d c_Max_Rec       c                   const(63000)
005300050809      *
005400050902      *?S C H I E R E?------------------------------------------------*
005500050809      *
005600061220      * - Stringhe SQL da "comporre" per estrarre i dati dagli archivi
005700070703     d $Sql_TITASS     s             35    dim(13) ctdata perrcd(1)
005800070629     d $Sql_TITAS      s             40    dim(15) ctdata perrcd(1)
005900070629     d $Sql_TITAA      s             25    dim(12) ctdata perrcd(1)
006000070702      *
006100070702     d $Temp           s            100    inz  dim(100)
006200050809      *
006300050902      *?S T R U T T U R E   D A T I?----------------------------------*
006400050809      *
006500050809      * - Parametri
006600050809     d KPJBA         e ds
006700050809     d TNSB19ds      e ds                  inz
006800070704     d   SB19rs               19    168    inz  dim(5)
006900070704     d   SB19in              169    318    inz  dim(5)
007000070704     d   SB19lo              319    468    inz  dim(5)
007100070704     d   SB19pr              469    478    inz  dim(5)
007200070629      *
007300070629      * - Parametri per gestione parametri batch
007400070629     d TRUL01ds      e ds                  inz
007500120612     d*//UL01opz     e                     inz(*off)
007600070629     d   UL01err     e                     inz(*off)
007700050809      *
007800050809      * - Parametri x Controllo profilo utenti
007900050902     d TIBS34ds      e ds                  inz
008000050809      * - Ds di riferimento al file esterno AZUTE00F
008100050902     d AZUTEds       e ds                  extname(AZUTE00F)
008200050809      * - Ds per dati organigramma
008300070703     d dDatiUte      e ds
008400061220      *
008500061220      * - Dati reperiti via SQL
008600061220     d TITAAds       e ds                  inz  extname(TITAA00F)
008700061220     d TITASds       e ds                  inz  extname(TITAS00F)
008800061220     d TITASSds      e ds                  inz  extname(TITASS0F)
008900061220     d                                          prefix(S)
009000061219      *
009100061206      * - Ds tabella $3 (Date avanzamento Trasferimento bolle)
009200061219     d ds$3          e ds                  inz
009300061220      * - Ds Riferimenti (trk "A" file TITA400F)
009400061220     d dTA4a         e ds                  inz
009500061220      * - Ds Cod. Fiscale mittente/destinatario (trk "Q" file TITA400F)
009600061220     d dTA4q         e ds                  inz
009700050901      *
009800061220      * - Ds per reperimento dati anagrafici clienti
009900050902     d TIBS69ds      e ds                  inz
010000050901     d DS_cnaco      e ds                  inz  extname(CNACO00F)
010100050901     d DS_cnind      e ds                  inz  extname(CNIND00F)
010200050901     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
010300050901     d DS_fncls      e ds                  inz  extname(FNCLS00F)
010400050809      *
010500050809     d Status         sds
010600070704     d   SDSpgm          *proc
010700090915      *
010800090915     d                 ds                  inz
010900090915     d wDateTime                     14  0 inz
011000090915     d   wTime                 1      6s 0 inz
011100090915     d   wDate                 7     14s 0 inz
011200050809      *
011300050902      *?V A R I A B I L I?--------------------------------------------*
011400050809      *
011500050809      * - Flags booleani
011600050809     d $EoF            s              1    inz(*off)
011700050809     d $Err            s              1    inz(*off)
011800070702     d $Selez          s              1    inz(*off)
011900050809      * - Indici di schiera / Contatori
012000050810     d xx              s              3  0 inz
012100050810     d yy              s              3  0 inz
012200120613     d wCountRec       s              7  0 inz
012300050809      * - Parametri per SQL
012400070629     d wSQL            s           9999    inz  Varying
012500050901     d wAAI            s              4  0 inz
012600050901     d wAAF            s              4  0 inz
012700070703     d wFile           s              1  0 inz(1)
012800070703      * - Parametri per QCMDEXC
012900070703     d Qcmd            s             80    inz
013000070703     d Qlen            s             15  5 inz(80)
013100050809      * - Campi di comodo
013200050902     d Data_Iso        s               d   inz  datfmt(*iso)
013300050902     d wTimeHM         s              4  0 inz
013400050901     d w0040           s              4  0 inz
013500090915     d w0080           s              8  0 inz
013600090313
013700090313       //--------------------------------------------------------------
013800090313       //?Definizione procedure esterne.
013900090313       //--------------------------------------------------------------
014000090313
014100090313       // - Esecuzione comando di sistema
014200090313     d qCmdExc         pr                  extpgm('QCMDEXC')
014300090313     d   Qcmd                       512    const  options(*varsize)
014400090313     d   Qlen                        15  5 const
014500050809      *
014600050902      *?K E Y - L I S T?----------------------------------------------*
014700050809      *
014800050809     c     K04TAS30      klist
014900061220     c                   kfld                    TAAaas
015000061220     c                   kfld                    TAAlnp
015100061220     c                   kfld                    TAAnrs
015200061220     c                   kfld                    TAAnsp
015300050902      *
015400050811     c     K05TAA30      klist
015500061220     c                   kfld                    TASaas
015600061220     c                   kfld                    TASlnp
015700061220     c                   kfld                    TASnrs
015800061220     c                   kfld                    TASnsp
015900050811     c                   kfld                    TAAtrc
016000061220      *
016100061220     c     K05TA430      klist
016200061220     c                   kfld                    TASaas
016300061220     c                   kfld                    TASlnp
016400061220     c                   kfld                    TASnrs
016500061220     c                   kfld                    TASnsp
016600061220     c                   kfld                    TA4trc
016700061220      *
016800061220     c     K04CSB03      klist
016900061220     c                   kfld                    TASaas
017000061220     c                   kfld                    TASlnp
017100061220     c                   kfld                    TASnrs
017200061220     c                   kfld                    TASnsp
017300061220      *
017400061220     c     K02APD01      klist
017500061220     c                   kfld                    APDtip
017600061220     c                   kfld                    TASpdc
017700061206      *
017800061220     c     K03TABEL      klist
017900061220     c                   kfld                    TBLkut
018000061220     c                   kfld                    TBLcod
018100061220     c                   kfld                    TBLkey
018200050902      *
018300050902      *?I N D I C A T O R I?------------------------------------------*
018400050902      *
018500050902      *
018600070703      *===============================================================*
018700050902      *
018800050809     c     *Entry        plist
018900050809     c                   parm                    KPJBA
019000050809      *
019100070629     c***                movel     KPJBU         TNSB19ds
019200050809      *
019300050809      * Operazioni iniziali
019400070703     c                   exsr      InzSubr
019500061220      *
019600061220      * Estrazione dati dal file TITASS0F
019700061220if  1c                   if        SB19dal <  §$3dp0
019800061220     c                   eval      wFile   =  1
019900061220     c                   exsr      sr_TITASS
020000061220e   1c                   endif
020100061220      *
020200061220      * Estrazione dati dal file TITAS30C
020300061220if  1c                   if        SB19ric <> 'M'
020400061220     c                   eval      wFile   =  2
020500061220     c                   exsr      sr_TITAS
020600061220e   1c                   endif
020700061220      *
020800061220      * Estrazione dati dal file TITAA30C
020900061220if  1c                   if             SB19ric =  'M'
021000061220     c                             or   SB19ric =  'E'
021100061220     c                   eval      wFile   =  3
021200061220     c                   exsr      sr_TITAA
021300061220e   1c                   endif
021400061220      *
021500061220      * Operazioni finali
021600070703     c                   exsr      EndSubr
021700061220      *
021800061220      * Fine programma
021900120613     c*//                return         (già fatto nella subr. EndSubr)
022000050902      *
022100050902      *?S U B R O U T I N E S?----------------------------------------*
022200050902      *
022300050810      *---------------------------------------------------------------*
022400050810      *?Operazioni iniziali                                          ?*
022500050810      *---------------------------------------------------------------*
022600070703     c     InzSubr       BEGSR
022700070216      *
022800070216     c/exec sql
022900070216     c+     set  option  DYNUSRPRF = *OWNER,
023000070216     c+                  CLOSQLCSR = *ENDMOD
023100070216     c/end-exec
023200120613      *
023300120613     c                   eval      *inLR   =  *on
023400050809      *
023500050809      * Reperisce dati job
023600050809     c     *dtaara       define    §azute        azuteds
023700050809     c     *dtaara       define    §datiute      ddatiute
023800050809      *
023900050809     c                   in(E)     *dtaara
024000050809     c                   IF        %ERROR or RSUT = *blanks
024100050809     c                   clear                   Tibs34Ds
024200050809     c                   call      'TIBS34R'
024300050809     c                   parm                    Tibs34Ds
024400050809     c                   in        *dtaara
024500050809     c                   ENDIF
024600070629      *
024700070629      * Reperisce parametri da WFPRM00F
024800070629     c                   clear                   TNSB19ds
024900070629     c                   reset                   TRUL01ds
025000070629     c                   movel     KPJBU         UL01prg
025100070629     c                   call      'TRUL01R'
025200070629     c                   parm                    TRUL01ds
025300070703     c                   if        UL01err =  *on
025400120613     c                   exsr      PrtNoParms
025500120613     c                   exsr      EndSubr
025600120613     c*//                leavesr
025700070629     c                   endif
025800070629     c                   movel     UL01prm       TNSB19ds
025900061218      *
026000061206      * Recupero ultima data pulizia del titasp in cassetta
026100070703     c                   eval      TBLkut  =  1
026200070703     c                   eval      TBLcod  =  '$3'
026300070703     c                   eval      TBLkey  =  '1       '
026400061219     c     K03Tabel      chain     TABEL
026500061219     c                   if        %found(TABEL00F)
026600061219     c                   movel     TBLuni        ds$3
026700061206     c                   endif
026800050902      *
026900050902      * Impostazione data e ora per campi chiave del WrkF
027000090915     c                   time                    wDateTime
027100090915     c     *eur          movel     wDate         Data_Iso
027200050902     c                   movel     wTime         wTimeHM
027300061220      *
027400061220      * Impostazione anno/i di selezione
027500061220     c                   movel     SB19dal       wAAI
027600061220     c                   movel     SB19al        wAAF
027700061220      *
027800061220      * Impostazione chiavi di accesso "fisse"
027900070703     c                   eval      APDtip  =  'A'
028000050902      *
028100090916     *** * Richiesta pulizia del workfile WFNIB00F
028200090916if  1***c                   if        SB19cwf =  'S'
028300090916     ***c/exec sql
028400090916     ***c+ DELETE from WFNIB00F
028500090916     ***c+  where WNBkut = :KNMUS
028600090916     ***c/end-exec
028700090916e   1***c                   endif
028800090915      /free
028900090916
029000090915         w0080 = %dec( %date( wDate : *eur ) - %months(2) );
029100090916
029200090916         // - Richiesta pulizia del workfile WFNIB00F
029300090916         if  SB19cwf = 'S';
029400090916           exec sql     DELETE from WFNIB00F
029500090916                         where WNBkut  = :KNMUS
029600090916                            or WNBkdt <= :w0080;
029700090916         else;
029800090916           // - ...Comunque vengono cancellati i record relativi alle
029900090916           //   estrazioni di più di 2 (due) mesi fa - di tutti gli
030000090916           //   utenti!!!
030100090916           exec sql     DELETE from WFNIB00F
030200090916                         where WNBkdt <= :w0080;
030300090916         endif;
030400090916
030500090915      /end-free
030600050902      *
030700050902      * Apertura del workfile (dopo l'eventuale pulizia)
030800050902     c                   open      WFNIB00F
030900070703      *
031000070703      * Apertura del printer-file
031100070703     c                   call      'QCMDEXC'
031200070703     c                   parm      C_OvrPrtF     Qcmd
031300070703     c                   parm                    Qlen
031400070703     c                   open      QSYSPRT
031500050809      *
031600050809     c                   ENDSR
031700061220      *
031800061220      *---------------------------------------------------------------*
031900061220      *?Operazioni finali                                            ?*
032000061220      *---------------------------------------------------------------*
032100070703     c     EndSubr       BEGSR
032200061220      *
032300061220      * Chiusura del workfile
032400120612     c                   close(e)  WFNIB00F
032500070703      *
032600070703      * Chiusura del printer-file
032700120612     c                   close(e)  QSYSPRT
032800070703     c                   call      'QCMDEXC'
032900070703     c                   parm      C_DltOvr      Qcmd
033000070703     c                   parm                    Qlen
033100120613      *
033200120613     c                   return
033300061220      *
033400061220     c                   ENDSR
033500061220      *
033600061220      *---------------------------------------------------------------*
033700061220      *?Estrazione dati dal file TITASS0F                            ?*
033800061220      *---------------------------------------------------------------*
033900061220     c     sr_TITASS     BEGSR
034000061220      *
034100061220      * Preparazione stringa SQL
034200061220     c                   exsr      PrepSQL_1
034300090313     c                   reset                   $EoF
034400061220      *
034500061220      * Apertura cursore
034600061220     c                   exsr      OpenCursor
034700061220      *
034800061220      * Elaborazione
034900120613     c                   dou       $EoF    =  *on
035000061220     c                   exsr      ReadCursor
035100061220     c                   enddo
035200061220      *
035300061220      * Chiusura cursore
035400061220     c                   exsr      CloseCursor
035500061220      *
035600061220     c                   ENDSR
035700050809      *
035800050810      *---------------------------------------------------------------*
035900061220      *?Preparazione stringa SQL per file TITASS0F                   ?*
036000050810      *---------------------------------------------------------------*
036100061220     c     PrepSQL_1     BEGSR
036200050809      *
036300050809     c                   clear                   wSQL
036400061220      *
036500061220      *? E S E M P I O : ?
036600061220      * select *
036700061220      * from   TITASS0F
036800061220      * where  TASsss  <>  'S'
036900061220      *   and  TASaas  >=  2006
037000061220      *   and (TASrsm like '%TIZIO%'
037100061220      *    or  TASrsm like '%T I Z I O %'
037200061220      *    or  TASrsm like '%CAIO%'
037300061220      *    or  TASrsm like '%C A I O%'
037400061220      *    or  TASrsd like '%TIZIO%'
037500061220      *    or  TASrsd like '%T I Z I O %'
037600061220      *    or  TASrsd like '%CAIO%'
037700061220      *    or  TASrsd like '%C A I O%')
037800061220      * order  by  TASaas, TASlnp, TASnrs, TASnsp
037900061220      *
038000061220      *?Impostazione estrazione dati da TITASS0F?
038100061220      *
038200061220     c                   eval      wSQL    =  %trimr($Sql_TITASS(1))
038300061220     c                                     +  %trimr($Sql_TITASS(2))
038400061220     c                                     +  %trimr($Sql_TITASS(3))
038500061220     c                                     +  ' '
038600061220     c                                     +  %char(SB19dal)
038700061220     c                                     +  ' and '
038800061220     c                                     +  %char(SB19al)
038900070702sel 1c                   select
039000070703w   1c                   when      SB19ric =  'K'
039100070702     c                   exsr      Add_SelTASScd
039200070703w   1c                   when      SB19ric =  'M'
039300070702     c                   exsr      Add_SelTASSm
039400070703w   1c                   when      SB19ric =  'D'
039500070702     c                   exsr      Add_SelTASSd
039600070703w   1c                   when      SB19ric =  'E'
039700070702     c                   exsr      Add_SelTASSm
039800070702     c                   exsr      Add_SelTASSd
039900070702e   1c                   endsl
040000061220      *
040100061220      *?for fetch only?
040200061220     c                   eval      wSQL    =  wSQL + C_ForFetch
040300070702      *
040400070704     c                   clear                   $Temp
040500070702     c                   movea     wSQL          $Temp
040600061220      *
040700061220     c                   ENDSR
040800061227      *
040900061227      *---------------------------------------------------------------*
041000061227      *?Aggiunta selezione rec. TITASS per dati del mittente         ?*
041100061227      *---------------------------------------------------------------*
041200061227     c     Add_SelTASSm  BEGSR
041300061227      *
041400070702     c                   eval      $Selez  =  *off
041500061227      *
041600061227      * selezione per?RAGIONE SOCIALE?del mittente
041700061227      *
041800061227     c                   eval      xx      =  1
041900061227     c                   clear                   yy
042000061227      * ciclo di impostazione selezioni
042100061227do  1c                   DOW       xx      <= %elem(SB19rs)
042200061227      *
042300061227if  2c                   if        SB19rs(xx) <> *blanks
042400061227     c                   add       1             yy
042500061227if  3c                   if        yy      =  1
042600070703     c                   eval      wSQL    =  wSQL + ' AND ((('
042700070703x   3c                   else
042800070703     c                   eval      wSQL    =  wSQL + ' or '
042900070703e   3c                   endif
043000070703     c                   eval      wSQL    =  wSQL
043100070703     c                                     +  %trimr($Sql_TITASS(4))
043200070629     c                                     +  %trimr(SB19rs(xx))
043300070629     c                                     +  '%'''
043400061227e   2c                   endif
043500070703      *
043600061227     c                   add       1             xx
043700061227e   1c                   ENDDO
043800061227      * chiusura selezioni (se impostata almeno una)
043900061227if  1c                   if        yy      >  *zeros
044000061227     c                   eval      wSQL    =  wSQL + ')'
044100070702     c                   eval      $Selez  =  *on
044200061227e   1c                   endif
044300061227      *
044400061227      * selezione per?INDIRIZZO?del mittente
044500061227      *
044600061227     c                   eval      xx      =  1
044700061227     c                   clear                   yy
044800061227      * ciclo di impostazione selezioni
044900061227do  1c                   DOW       xx      <= %elem(SB19in)
045000061227      *
045100061227if  2c                   if        SB19in(xx) <> *blanks
045200061227     c                   add       1             yy
045300070703sel 3c                   select
045400061227w   3c                   when           yy     = 1
045500070702     c                             and  $Selez = *off
045600070703     c                   eval      wSQL    =  wSQL + ' AND ((('
045700061227w   3c                   when           yy     = 1
045800070702     c                             and  $Selez = *on
045900070703     c                   eval      wSQL    =  wSQL + ' and ('
046000061227x   3c                   other
046100070703     c                   eval      wSQL    =  wSQL + ' or '
046200061227e   3c                   endsl
046300070703     c                   eval      wSQL    =  wSQL
046400070703     c                                     +  %trimr($Sql_TITASS(5))
046500070703     c                                     +  %trimr(SB19in(xx))
046600070703     c                                     +  '%'''
046700061227e   2c                   endif
046800061227      *
046900061227     c                   add       1             xx
047000061227e   1c                   ENDDO
047100061227      * chiusura selezioni (se impostata almeno una)
047200061227if  1c                   if        yy      >  *zeros
047300061227     c                   eval      wSQL    =  wSQL + ')'
047400070702     c                   eval      $Selez  =  *on
047500061227e   1c                   endif
047600070629      *
047700070629      * selezione per?LOCALITÀ?del mittente
047800070629      *
047900070629     c                   eval      xx      =  1
048000070629     c                   clear                   yy
048100070629      * ciclo di impostazione selezioni
048200070629do  1c                   DOW       xx      <= %elem(SB19lo)
048300070629      *
048400070629if  2c                   if        SB19lo(xx) <> *blanks
048500070629     c                   add       1             yy
048600070703sel 3c                   select
048700070629w   3c                   when           yy     = 1
048800070702     c                             and  $Selez = *off
048900070703     c                   eval      wSQL    =  wSQL + ' AND ((('
049000070629w   3c                   when           yy     = 1
049100070702     c                             and  $Selez = *on
049200070703     c                   eval      wSQL    =  wSQL + ' and ('
049300070629x   3c                   other
049400070703     c                   eval      wSQL    =  wSQL + ' or '
049500070703e   3c                   endsl
049600070703     c                   eval      wSQL    =  wSQL
049700070703     c                                     +  %trimr($Sql_TITASS(6))
049800070629     c                                     +  %trimr(SB19lo(xx))
049900070629     c                                     +  '%'''
050000070629e   2c                   endif
050100070629      *
050200070629     c                   add       1             xx
050300070629e   1c                   ENDDO
050400070629      * chiusura selezioni (se impostata almeno una)
050500070629if  1c                   if        yy      >  *zeros
050600070629     c                   eval      wSQL    =  wSQL + ')'
050700070702     c                   eval      $Selez  =  *on
050800070629e   1c                   endif
050900061227      *
051000061227      * selezione per?PROVINCIA?del mittente
051100061227      *
051200061227     c                   eval      xx      =  1
051300061227     c                   clear                   yy
051400061227      * ciclo di impostazione selezioni
051500061227do  1c                   DOW       xx      <= %elem(SB19pr)
051600061227      *
051700061227if  2c                   if        SB19pr(xx) <> *blanks
051800061227     c                   add       1             yy
051900070702sel 3c                   select
052000070702w   3c                   when          yy      =  1
052100070702     c                             and SB19ric = 'E'
052200070702     c                             and $Selez  = *off
052300070703     c                   eval      wSQL    =  wSQL + ' and ('
052400070703     c                                     +  %trimr($Sql_TITASS(7))
052500070702w   3c                   when      yy      =  1
052600070703     c                   eval      wSQL    =  wSQL + ' and '
052700070703     c                                     +  %trimr($Sql_TITASS(7))
052800070702x   3c                   other
052900061227     c                   eval      wSQL    =  wSQL + ', '''
053000070702e   3c                   endsl
053100070703     c                   eval      wSQL    =  wSQL
053200070703     c                                     +  SB19pr(xx)
053300070703     c                                     +  ''''
053400061227e   2c                   endif
053500061227      *
053600061227     c                   add       1             xx
053700061227e   1c                   ENDDO
053800061227      * chiusura selezioni (se impostata almeno una)
053900061227if  1c                   if        yy      >  *zeros
054000061227     c                   eval      wSQL    =  wSQL + ')'
054100061227e   1c                   endif
054200061227      *
054300061227      * chiusura generale selezioni per mittente
054400070702sel 1c                   select
054500070702w   1c                   when      $Selez  =  *off
054600070702w   1c                   when      SB19ric <> 'M'
054700070702     c                   eval      wSQL    =  wSQL + ')'
054800070702x   1c                   other
054900070702     c                   eval      wSQL    =  wSQL + '))'
055000070702e   1c                   endsl
055100061227      *
055200061227     c                   ENDSR
055300061227      *
055400061227      *---------------------------------------------------------------*
055500061227      *?Aggiunta selezione rec. TITASS per dati del destinatario     ?*
055600061227      *---------------------------------------------------------------*
055700061227     c     Add_SelTASSd  BEGSR
055800061227      *
055900070702     c                   eval      $Selez  =  *off
056000061227      *
056100061227      * selezione per?RAGIONE SOCIALE?del destinatario
056200061227      *
056300061227     c                   eval      xx      =  1
056400061227     c                   clear                   yy
056500061227      * ciclo di impostazione selezioni
056600061227do  1c                   DOW       xx      <= %elem(SB19rs)
056700061227      *
056800061227if  2c                   if        SB19rs(xx) <> *blanks
056900061227     c                   add       1             yy
057000070702sel 3c                   select
057100070703w   3c                   when          yy      = 1
057200070703     c                             and SB19ric = 'E'
057300070703     c                   eval      wSQL    =  wSQL + ' OR (('
057400070703w   3c                   when          yy      = 1
057500070702     c                             and SB19ric = 'D'
057600070703     c                   eval      wSQL    =  wSQL + ' AND (('
057700070702x   3c                   other
057800070703     c                   eval      wSQL    =  wSQL + ' or '
057900070702e   3c                   endsl
058000070703     c                   eval      wSQL    =  wSQL
058100070703     c                                     +  %trimr($Sql_TITASS(8))
058200070703     c                                     +  %trimr(SB19rs(xx))
058300070703     c                                     +  '%'''
058400061227e   2c                   endif
058500061227      *
058600061227     c                   add       1             xx
058700061227e   1c                   ENDDO
058800061227      * chiusura selezioni (se impostata almeno una)
058900061227if  1c                   if        yy      >  *zeros
059000061227     c                   eval      wSQL    =  wSQL + ')'
059100070702     c                   eval      $Selez  =  *on
059200061227e   1c                   endif
059300061227      *
059400061227      * selezione per?INDIRIZZO?del destinatario
059500061227      *
059600061227     c                   eval      xx      =  1
059700061227     c                   clear                   yy
059800061227      * ciclo di impostazione selezioni
059900061227do  1c                   DOW       xx      <= %elem(SB19in)
060000061227      *
060100061227if  2c                   if        SB19in(xx) <> *blanks
060200061227     c                   add       1             yy
060300061227sel 3c                   select
060400070702w   3c                   when          yy      = 1
060500070702     c                             and $Selez  = *off
060600070703     c                             and SB19ric = 'E'
060700070703     c                   eval      wSQL    =  wSQL + ' OR (('
060800070702w   3c                   when          yy      = 1
060900070702     c                             and $Selez  = *off
061000070702     c                             and SB19ric = 'D'
061100070703     c                   eval      wSQL    =  wSQL + ' AND (('
061200070702w   3c                   when          yy      = 1
061300070702     c                             and $Selez  = *on
061400070703     c                   eval      wSQL    =  wSQL + ' and ('
061500061227x   3c                   other
061600070703     c                   eval      wSQL    =  wSQL + ' or '
061700061227e   3c                   endsl
061800070703     c                   eval      wSQL    =  wSQL
061900070703     c                                     +  %trimr($Sql_TITASS(9))
062000070703     c                                     +  %trimr(SB19in(xx))
062100070703     c                                     +  '%'''
062200061227e   2c                   endif
062300061227      *
062400061227     c                   add       1             xx
062500061227e   1c                   ENDDO
062600061227      * chiusura selezioni (se impostata almeno una)
062700061227if  1c                   if        yy      >  *zeros
062800061227     c                   eval      wSQL    =  wSQL + ')'
062900070702     c                   eval      $Selez  =  *on
063000061227e   1c                   endif
063100070629      *
063200070629      * selezione per?LOCALITÀ?del destinatario
063300070629      *
063400070629     c                   eval      xx      =  1
063500070629     c                   clear                   yy
063600070629      * ciclo di impostazione selezioni
063700070629do  1c                   DOW       xx      <= %elem(SB19lo)
063800070629      *
063900070629if  2c                   if        SB19lo(xx) <> *blanks
064000070629     c                   add       1             yy
064100070629sel 3c                   select
064200070702w   3c                   when          yy      = 1
064300070702     c                             and $Selez  = *off
064400070703     c                             and SB19ric = 'E'
064500070703     c                   eval      wSQL    =  wSQL + ' OR (('
064600070702w   3c                   when          yy      = 1
064700070702     c                             and $Selez  = *off
064800070702     c                             and SB19ric = 'D'
064900070703     c                   eval      wSQL    =  wSQL + ' AND (('
065000070629w   3c                   when           yy     = 1
065100070702     c                             and  $Selez = *on
065200070703     c                   eval      wSQL    =  wSQL + 'and ('
065300070629x   3c                   other
065400070703     c                   eval      wSQL    =  wSQL + ' or '
065500070629e   3c                   endsl
065600070703     c                   eval      wSQL    =  wSQL
065700070703     c                                     +  %trimr($Sql_TITASS(10))
065800070703     c                                     +  %trimr(SB19lo(xx))
065900070703     c                                     +  '%'''
066000070629e   2c                   endif
066100070629      *
066200070629     c                   add       1             xx
066300070629e   1c                   ENDDO
066400070629      * chiusura selezioni (se impostata almeno una)
066500070629if  1c                   if        yy      >  *zeros
066600070629     c                   eval      wSQL    =  wSQL + ')'
066700070702     c                   eval      $Selez  =  *on
066800070629e   1c                   endif
066900061227      *
067000061227      * selezione per?PROVINCIA?del destinatario
067100061227      *
067200061227     c                   eval      xx      =  1
067300061227     c                   clear                   yy
067400061227      * ciclo di impostazione selezioni
067500061227do  1c                   DOW       xx      <= %elem(SB19pr)
067600061227      *
067700061227if  2c                   if        SB19pr(xx) <> *blanks
067800061227     c                   add       1             yy
067900070702sel 3c                   select
068000070702w   3c                   when          yy      =  1
068100070702     c                             and SB19ric = 'E'
068200070702     c                             and $Selez  = *off
068300070703     c                   eval      wSQL    =  wSQL + ' or '
068400070703     c                                     +  %trimr($Sql_TITASS(11))
068500070702w   3c                   when      yy      =  1
068600070703     c                   eval      wSQL    =  wSQL + ' and '
068700070703     c                                     +  %trimr($Sql_TITASS(11))
068800070702x   3c                   other
068900061227     c                   eval      wSQL    =  wSQL + ', '''
069000070702e   3c                   endsl
069100070703     c                   eval      wSQL    =  wSQL
069200070703     c                                     +  %trimr(SB19pr(xx))
069300070703     c                                     +  ''''
069400061227e   2c                   endif
069500061227      *
069600061227     c                   add       1             xx
069700061227e   1c                   ENDDO
069800061227      * chiusura selezioni (se impostata almeno una)
069900061227if  1c                   if        yy      >  *zeros
070000061227     c                   eval      wSQL    =  wSQL + ')'
070100061227e   1c                   endif
070200061227      *
070300061227      * chiusura generale selezioni per mittente e destinatario
070400070702sel 1c                   select
070500070702w   1c                   when          $Selez  = *off
070600070703     c                             and SB19ric = 'D'
070700070702w   1c                   when          SB19ric = 'D'
070800070702     c                             or (SB19ric = 'E'
070900070702     c                             and $Selez  = *off)
071000070702     c                   eval      wSQL    =  wSQL + ')'
071100070702x   1c                   other
071200061227     c                   eval      wSQL    =  wSQL + '))'
071300070702e   1c                   endsl
071400061227      *
071500061227     c                   ENDSR
071600061220      *
071700061220      *---------------------------------------------------------------*
071800061220      *?Aggiunta selezione rec. TITASS per codice cliente            ?*
071900061220      *---------------------------------------------------------------*
072000061220     c     Add_SelTASScd BEGSR
072100061220      *
072200061220      *?Per TASKSC:?
072300061220     c                   eval      xx      = 1
072400061220     c                   clear                   yy
072500061220      * ciclo di impostazione selezioni
072600061220do  1c                   DOW       xx      <= %elem(SB19rs)
072700061220      *
072800061220if  2c                   if        SB19rs(xx) <> *blanks
072900061220     c                   add       1             yy
073000061220if  3c                   if        yy      =  1
073100061220     c                   eval      wSQL    =  wSQL
073200070703     c                                     +  %trimr($Sql_TITASS(12))
073300061220     c                                     +  %trimr(SB19rs(xx))
073400061220x   3c                   else
073500061220     c                   eval      wSQL    =  wSQL + ', '
073600061220     c                                     +  %trimr(SB19rs(xx))
073700061220e   3c                   endif
073800061220e   2c                   endif
073900061220      *
074000061220     c                   add       1             xx
074100061220e   1c                   ENDDO
074200061220      * chiusura selezioni (se impostata almeno una)
074300061220if  1c                   if        yy      >  *zeros
074400061220     c                   eval      wSQL    =  wSQL + ')'
074500061220e   1c                   endif
074600061220      *
074700061220      *?Per TASCCM:?
074800061220     c                   eval      xx      =  1
074900061220     c                   clear                   yy
075000061220      * ciclo di impostazione selezioni
075100061220do  1c                   DOW       xx      <= %elem(SB19rs)
075200061220      *
075300061220if  2c                   if        SB19rs(xx) <> *blanks
075400061220     c                   add       1             yy
075500061220if  3c                   if        yy      =  1
075600061220     c                   eval      wSQL    =  wSQL
075700070703     c                                     +  %trimr($Sql_TITASS(13))
075800061220     c                                     +  %trimr(SB19rs(xx))
075900061220x   3c                   else
076000061220     c                   eval      wSQL    =  wSQL + ', '
076100061220     c                                     +  %trim(SB19rs(xx))
076200061220e   3c                   endif
076300061220e   2c                   endif
076400061220      *
076500061220     c                   add       1             xx
076600061220e   1c                   ENDDO
076700061220      * chiusura selezioni (se impostata almeno una)
076800061220if  1c                   if        yy      >  *zeros
076900061220     c                   eval      wSQL    =  wSQL + '))'
077000061220e   1c                   endif
077100061220      *
077200061220     c                   ENDSR
077300061220      *
077400061220      *---------------------------------------------------------------*
077500061220      *?Estrazione dati dal file TITAS30C                            ?*
077600061220      *---------------------------------------------------------------*
077700061220     c     sr_TITAS      BEGSR
077800061220      *
077900061220      * Preparazione stringa SQL
078000061220     c                   exsr      PrepSQL_2
078100090313     c                   reset                   $EoF
078200061220      *
078300061220      * Apertura cursore
078400061220     c                   exsr      OpenCursor
078500061220      *
078600061220      * Elaborazione
078700120613     c                   dou       $EoF    =  *on
078800061220     c                   exsr      ReadCursor
078900061220     c                   enddo
079000061220      *
079100061220      * Chiusura cursore
079200061220     c                   exsr      CloseCursor
079300061220      *
079400061220     c                   ENDSR
079500061220      *
079600061220      *---------------------------------------------------------------*
079700061220      *?Preparazione stringa SQL per file TITAS30C                   ?*
079800061220      *---------------------------------------------------------------*
079900061220     c     PrepSQL_2     BEGSR
080000061220      *
080100061220     c                   clear                   wSQL
080200061220      *
080300061220      *? E S E M P I O : ?
080400061220      * select *
080500061220      * from   TITASP0F
080600061220      * where (TASaas*10000+TASmgs) >= 20050810
080700061220      *   and (TASrsd like '%TIZIO%'
080800061220      *    or  TASrsd like '%T I Z I O %'
080900061220      *    or  TASrsd like '%CAIO%'
081000061220      *    or  TASrsd like '%C A I O%')
081100061220      * UNION
081200061220      * select *
081300061220      * from   TITAS10F
081400061220      * where (TASaas*10000+TASmgs) >= 20050810
081500061220      *   and (TASrsd like '%TIZIO%'
081600061220      *    or  TASrsd like '%T I Z I O %'
081700061220      *    or  TASrsd like '%CAIO%'
081800061220      *    or  TASrsd like '%C A I O%')
081900061220      * UNION
082000061220      * select *
082100061220      * from   TITAS00F
082200061220      * where (TASaas*10000+TASmgs) >= 20050810
082300061220      *   and (TASrsd like '%TIZIO%'
082400061220      *    or  TASrsd like '%T I Z I O %'
082500061220      *    or  TASrsd like '%CAIO%'
082600061220      *    or  TASrsd like '%C A I O%')
082700061220      *
082800061220      *?Impostazione estrazione dati da TITAS30C?
082900061220      *
083000061220      *
083100061220      * - select TITASP0F
083200061220     c                   eval      wSQL    =  %trimr($Sql_TITAS(1))
083300061220     c                                     +  %trimr($Sql_TITAS(2))
083400061220     c                                     +  %trimr($Sql_TITAS(5))
083500061220     c                                     +  %trimr($Sql_TITAS(6))
083600061220     c                                     +  ' '
083700061220     c                                     +  %char(SB19dal)
083800061220     c                                     +  ' and '
083900061220     c                                     +  %char(SB19al)
084000061220if  1c                   if             SB19ric = 'D'
084100061220     c                             or   SB19ric = 'E'
084200061220     c                   exsr      Add_SelTASrsd
084300061220     c                   exsr      Add_SelTASind
084400070629     c                   exsr      Add_SelTASlod
084500061220     c                   exsr      Add_SelTASprd
084600061220x   1c                   else
084700061220     c                   exsr      Add_SelTAScod
084800061220e   1c                   endif
084900061220      * - union
085000061220     c                   eval      wSQL    =  wSQL + C_Union
085100061220      * - select TITAS10F
085200061220     c                   eval      wSQL    =  wSQL
085300061220     c                                     +  %trimr($Sql_TITAS(1))
085400061220     c                                     +  %trimr($Sql_TITAS(3))
085500061220     c                                     +  %trimr($Sql_TITAS(5))
085600061220     c                                     +  %trimr($Sql_TITAS(6))
085700061220     c                                     +  ' '
085800061220     c                                     +  %char(SB19dal)
085900061220     c                                     +  ' and '
086000061220     c                                     +  %char(SB19al)
086100061220if  1c                   if             SB19ric = 'D'
086200061220     c                             or   SB19ric = 'E'
086300061220     c                   exsr      Add_SelTASrsd
086400061220     c                   exsr      Add_SelTASind
086500070629     c                   exsr      Add_SelTASlod
086600061220     c                   exsr      Add_SelTASprd
086700061220x   1c                   else
086800061220     c                   exsr      Add_SelTAScod
086900061220e   1c                   endif
087000061220      * - union
087100061220     c                   eval      wSQL    =  wSQL + C_Union
087200061220      * - select TITAS00F
087300061220     c                   eval      wSQL    =  wSQL
087400061220     c                                     +  %trimr($Sql_TITAS(1))
087500061220     c                                     +  %trimr($Sql_TITAS(4))
087600061220     c                                     +  %trimr($Sql_TITAS(5))
087700061220     c                                     +  %trimr($Sql_TITAS(6))
087800061220     c                                     +  ' '
087900061220     c                                     +  %char(SB19dal)
088000061220     c                                     +  ' and '
088100061220     c                                     +  %char(SB19al)
088200061220if  1c                   if             SB19ric = 'D'
088300061220     c                             or   SB19ric = 'E'
088400061220     c                   exsr      Add_SelTASrsd
088500061220     c                   exsr      Add_SelTASind
088600070629     c                   exsr      Add_SelTASlod
088700061220     c                   exsr      Add_SelTASprd
088800061220x   1c                   else
088900061220     c                   exsr      Add_SelTAScod
089000061220e   1c                   endif
089100061220      *
089200061220      *?for fetch only?
089300061220     c                   eval      wSQL    =  wSQL + C_ForFetch
089400070702      *
089500070704     c                   clear                   $Temp
089600070702     c                   movea     wSQL          $Temp
089700061220      *
089800061220     c                   ENDSR
089900061220      *
090000061220      *---------------------------------------------------------------*
090100061220      *?Aggiunta selezione rec. TITAS per ragione sociale            ?*
090200061220      *---------------------------------------------------------------*
090300061220     c     Add_SelTASrsd BEGSR
090400061220      *
090500061220     c                   eval      xx      =  1
090600061220     c                   clear                   yy
090700061220      * ciclo di impostazione selezioni
090800061220do  1c                   DOW       xx      <= %elem(SB19rs)
090900061220      *
091000061220if  2c                   if        SB19rs(xx) <> *blanks
091100061220     c                   add       1             yy
091200061220if  3c                   if        yy      =  1
091300061220     c                   eval      wSQL    =  wSQL
091400061220     c                                     +  %trimr($Sql_TITAS(7))
091500070629     c                                     +  %trimr(SB19rs(xx))
091600070629     c                                     +  '%'''
091700061220x   3c                   else
091800061220     c                   eval      wSQL    =  wSQL
091900061220     c                                     +  %trimr($Sql_TITAS(8))
092000070629     c                                     +  %trimr(SB19rs(xx))
092100070629     c                                     +  '%'''
092200061220e   3c                   endif
092300061220e   2c                   endif
092400061220      *
092500061220     c                   add       1             xx
092600061220e   1c                   ENDDO
092700061220      * chiusura selezioni (se impostata almeno una)
092800061220if  1c                   if        yy      >  *zeros
092900061220     c                   eval      wSQL    =  wSQL + ')'
093000061220e   1c                   endif
093100061220      *
093200061220     c                   ENDSR
093300061220      *
093400061220      *---------------------------------------------------------------*
093500061220      *?Aggiunta selezione rec. TITAS per indirizzo                  ?*
093600061220      *---------------------------------------------------------------*
093700061220     c     Add_SelTASind BEGSR
093800061220      *
093900061220     c                   eval      xx      =  1
094000061220     c                   clear                   yy
094100061220      * ciclo di impostazione selezioni
094200061220do  1c                   DOW       xx      <= %elem(SB19in)
094300061220      *
094400061220if  2c                   if        SB19in(xx) <> *blanks
094500061220     c                   add       1             yy
094600061220if  3c                   if        yy      =  1
094700061220     c                   eval      wSQL    =  wSQL
094800061220     c                                     +  %trimr($Sql_TITAS(9))
094900070629     c                                     +  %trimr(SB19in(xx))
095000070629     c                                     +  '%'''
095100061220x   3c                   else
095200061222     c                   eval      wSQL    =  wSQL
095300061222     c                                     +  %trimr($Sql_TITAS(10))
095400070629     c                                     +  %trimr(SB19in(xx))
095500070629     c                                     +  '%'''
095600061220e   3c                   endif
095700061220e   2c                   endif
095800061220      *
095900061220     c                   add       1             xx
096000061220e   1c                   ENDDO
096100061220      * chiusura selezioni (se impostata almeno una)
096200061220if  1c                   if        yy      >  *zeros
096300061222     c                   eval      wSQL    =  wSQL + ')'
096400061220e   1c                   endif
096500061220      *
096600061220     c                   ENDSR
096700070629      *
096800070629      *---------------------------------------------------------------*
096900070629      *?Aggiunta selezione rec. TITAS per località                   ?*
097000070629      *---------------------------------------------------------------*
097100070629     c     Add_SelTASlod BEGSR
097200070629      *
097300070629     c                   eval      xx      =  1
097400070629     c                   clear                   yy
097500070629      * ciclo di impostazione selezioni
097600070629do  1c                   DOW       xx      <= %elem(SB19lo)
097700070629      *
097800070629if  2c                   if        SB19lo(xx) <> *blanks
097900070629     c                   add       1             yy
098000070629if  3c                   if        yy      =  1
098100070629     c                   eval      wSQL    =  wSQL
098200070629     c                                     +  %trimr($Sql_TITAS(11))
098300070629     c                                     +  %trimr(SB19lo(xx))
098400070629     c                                     +  '%'''
098500070629x   3c                   else
098600070629     c                   eval      wSQL    =  wSQL
098700070629     c                                     +  %trimr($Sql_TITAS(12))
098800070629     c                                     +  %trimr(SB19lo(xx))
098900070629     c                                     +  '%'''
099000070629e   3c                   endif
099100070629e   2c                   endif
099200070629      *
099300070629     c                   add       1             xx
099400070629e   1c                   ENDDO
099500070629      * chiusura selezioni (se impostata almeno una)
099600070629if  1c                   if        yy      >  *zeros
099700070629     c                   eval      wSQL    =  wSQL + ')'
099800070629e   1c                   endif
099900070629      *
100000070629     c                   ENDSR
100100061220      *
100200061220      *---------------------------------------------------------------*
100300061220      *?Aggiunta selezione rec. TITAS per provincia                  ?*
100400061220      *---------------------------------------------------------------*
100500061220     c     Add_SelTASprd BEGSR
100600061220      *
100700061220     c                   eval      xx      =  1
100800061220     c                   clear                   yy
100900061220      * ciclo di impostazione selezioni
101000061220do  1c                   DOW       xx      <= %elem(SB19pr)
101100061220      *
101200061220if  2c                   if        SB19pr(xx) <> *blanks
101300061220     c                   add       1             yy
101400061220if  3c                   if        yy      =  1
101500061220     c                   eval      wSQL    =  wSQL
101600070629     c                                     +  %trimr($Sql_TITAS(13))
101700061220     c                                     +  SB19pr(xx) + ''''
101800061220x   3c                   else
101900061222     c                   eval      wSQL    =  wSQL       + ', '''
102000061220     c                                     +  SB19pr(xx) + ''''
102100061220e   3c                   endif
102200061220e   2c                   endif
102300061220      *
102400061220     c                   add       1             xx
102500061220e   1c                   ENDDO
102600061220      * chiusura selezioni (se impostata almeno una)
102700061220if  1c                   if        yy      >  *zeros
102800061220     c                   eval      wSQL    =  wSQL + ')'
102900061220e   1c                   endif
103000061220      *
103100061220     c                   ENDSR
103200061220      *
103300061220      *---------------------------------------------------------------*
103400061220      *?Aggiunta selezione rec. TITAS per codice cliente             ?*
103500061220      *---------------------------------------------------------------*
103600061220     c     Add_SelTAScod BEGSR
103700061220      *
103800061220      *?Per TASKSC:?
103900061220     c                   eval      xx      =  1
104000061220     c                   clear                   yy
104100061220      * ciclo di impostazione selezioni
104200061220do  1c                   DOW       xx      <= %elem(SB19rs)
104300061220      *
104400061220if  2c                   if        SB19rs(xx) <> *blanks
104500061220     c                   add       1             yy
104600061220if  3c                   if        yy      =  1
104700061220     c                   eval      wSQL    =  wSQL
104800070629     c                                     +  %trimr($Sql_TITAS(14))
104900061220     c                                     +  %trimr(SB19rs(xx))
105000061220x   3c                   else
105100061220     c                   eval      wSQL    =  wSQL + ', '
105200061220     c                                     +  %trimr(SB19rs(xx))
105300061220e   3c                   endif
105400061220e   2c                   endif
105500061220      *
105600061220     c                   add       1             xx
105700061220e   1c                   ENDDO
105800061220      * chiusura selezioni (se impostata almeno una)
105900061220if  1c                   if        yy      >  *zeros
106000061220     c                   eval      wSQL    =  wSQL + ')'
106100061220e   1c                   endif
106200061220      *
106300061220      *?Per TASCCM:?
106400061220     c                   eval      xx      =  1
106500061220     c                   clear                   yy
106600061220      * ciclo di impostazione selezioni
106700061220do  1c                   DOW       xx      <= %elem(SB19rs)
106800061220      *
106900061220if  2c                   if        SB19rs(xx) <> *blanks
107000061220     c                   add       1             yy
107100061220if  3c                   if        yy      =  1
107200061220     c                   eval      wSQL    =  wSQL
107300070629     c                                     +  %trimr($Sql_TITAS(15))
107400061220     c                                     +  %trimr(SB19rs(xx))
107500061220x   3c                   else
107600061220     c                   eval      wSQL    =  wSQL + ', '
107700061220     c                                     +  %trimr(SB19rs(xx))
107800061220e   3c                   endif
107900061220e   2c                   endif
108000061220      *
108100061220     c                   add       1             xx
108200061220e   1c                   ENDDO
108300061220      * chiusura selezioni (se impostata almeno una)
108400061220if  1c                   if        yy      >  *zeros
108500061220     c                   eval      wSQL    =  wSQL + '))'
108600061220e   1c                   endif
108700061220      *
108800061220     c                   ENDSR
108900061220      *
109000061220      *---------------------------------------------------------------*
109100061220      *?Estrazione dati dal file TITAA30C                            ?*
109200061220      *---------------------------------------------------------------*
109300061220     c     sr_TITAA      BEGSR
109400061220      *
109500061220      * Preparazione stringa SQL
109600061220     c                   exsr      PrepSQL_3
109700090313     c                   reset                   $EoF
109800061220      *
109900061220      * Apertura cursore
110000061220     c                   exsr      OpenCursor
110100061220      *
110200061220      * Elaborazione
110300120613     c                   dou       $EoF    =  *on
110400061220     c                   exsr      ReadCursor
110500061220     c                   enddo
110600061220      *
110700061220      * Chiusura cursore
110800061220     c                   exsr      CloseCursor
110900061220      *
111000061220     c                   ENDSR
111100061220      *
111200061220      *---------------------------------------------------------------*
111300061220      *?Preparazione stringa SQL per file TITAA30C                   ?*
111400061220      *---------------------------------------------------------------*
111500061220     c     PrepSQL_3     BEGSR
111600061220      *
111700061220     c                   clear                   wSQL
111800061220      *
111900061220      *? E S E M P I O : ?
112000061220      * select *
112100061220      * from   TITAAP0F
112200061220      * where  TAAtrc  =   'M'   and    TAAaas >= 2005
112300061220      *   and (TAArsc like '%TIZIO%'
112400061220      *    or  TAArsc like '%T I Z I O %'
112500061220      *    or  TAArsc like '%CAIO%'
112600061220      *    or  TAArsc like '%C A I O%')
112700061220      * UNION
112800061220      * select *
112900061220      * from   TITAA00F
113000061220      * where  TAAtrc  =   'M'   and    TAAaas >= 2005
113100061220      *   and (TAArsc like '%TIZIO%'
113200061220      *    or  TAArsc like '%T I Z I O %'
113300061220      *    or  TAArsc like '%CAIO%'
113400061220      *    or  TAArsc like '%C A I O%')
113500061220      *
113600061220      *?Impostazione estrazione dati da TITAA30C?
113700061220      *
113800061220      * - select TITAAP0F
113900061220     c                   eval      wSQL    =  %trimr($Sql_TITAA(1))
114000061220     c                                     +  %trimr($Sql_TITAA(2))
114100061220     c                                     +  %trimr($Sql_TITAA(4))
114200061220     c                                     +  %trimr($Sql_TITAA(5))
114300061220     c                                     +  ' '
114400061220     c                                     +  %char(wAAI)
114500061220     c                                     +  ' and '
114600061220     c                                     +  %char(wAAF)
114700061220     c                   exsr      Add_SelTAArsc
114800061220     c                   exsr      Add_SelTAAind
114900070629     c                   exsr      Add_SelTAAloc
115000061220     c                   exsr      Add_SelTAAprv
115100061220      * - union
115200061220     c                   eval      wSQL    =  wSQL + C_Union
115300061220      * - select TITAA00F
115400061220     c                   eval      wSQL    =  wSQL
115500061220     c                                     +  %trimr($Sql_TITAA(1))
115600061220     c                                     +  %trimr($Sql_TITAA(3))
115700061220     c                                     +  %trimr($Sql_TITAA(4))
115800061220     c                                     +  %trimr($Sql_TITAA(5))
115900061220     c                                     +  ' '
116000061220     c                                     +  %char(wAAI)
116100061220     c                                     +  ' and '
116200061220     c                                     +  %char(wAAF)
116300061220     c                   exsr      Add_SelTAArsc
116400061220     c                   exsr      Add_SelTAAind
116500070629     c                   exsr      Add_SelTAAloc
116600061220     c                   exsr      Add_SelTAAprv
116700061220      *
116800061220      *?for fetch only?
116900061220     c                   eval      wSQL    =  wSQL + C_ForFetch
117000070702      *
117100070704     c                   clear                   $Temp
117200070702     c                   movea     wSQL          $Temp
117300061220      *
117400061220     c                   ENDSR
117500061220      *
117600061220      *---------------------------------------------------------------*
117700061220      *?Aggiunta selezione rec. TITAA per ragione sociale            ?*
117800061220      *---------------------------------------------------------------*
117900061220     c     Add_SelTAArsc BEGSR
118000061220      *
118100061220     c                   eval      xx      =  1
118200061220     c                   clear                   yy
118300061220      * ciclo di impostazione selezioni
118400061220do  1c                   DOW       xx      <= %elem(SB19rs)
118500061220      *
118600061220if  2c                   if        SB19rs(xx) <> *blanks
118700061220     c                   add       1             yy
118800061220if  3c                   if        yy      =  1
118900061220     c                   eval      wSQL    =  wSQL
119000061220     c                                     +  %trimr($Sql_TITAA(6))
119100070629     c                                     +  %trimr(SB19rs(xx))
119200070629     c                                     +  '%'''
119300061220x   3c                   else
119400061220     c                   eval      wSQL    =  wSQL
119500061220     c                                     +  %trimr($Sql_TITAA(7))
119600070629     c                                     +  %trimr(SB19rs(xx))
119700070629     c                                     +  '%'''
119800061220e   3c                   endif
119900061220e   2c                   endif
120000061220      *
120100061220     c                   add       1             xx
120200061220e   1c                   ENDDO
120300061220      * chiusura selezioni (se impostata almeno una)
120400061220if  1c                   if        yy      >  *zeros
120500061220     c                   eval      wSQL    =  wSQL + ')'
120600061220e   1c                   endif
120700061220      *
120800061220     c                   ENDSR
120900061220      *
121000061220      *---------------------------------------------------------------*
121100061220      *?Aggiunta selezione rec. TITAA per indirizzo                  ?*
121200061220      *---------------------------------------------------------------*
121300061220     c     Add_SelTAAind BEGSR
121400061220      *
121500061220     c                   eval      xx      =  1
121600061220     c                   clear                   yy
121700061220      * ciclo di impostazione selezioni
121800061220do  1c                   DOW       xx      <= %elem(SB19in)
121900061220      *
122000061220if  2c                   if        SB19in(xx) <> *blanks
122100061220     c                   add       1             yy
122200061220if  3c                   if        yy      =  1
122300061220     c                   eval      wSQL    =  wSQL
122400061220     c                                     +  %trimr($Sql_TITAA(8))
122500070629     c                                     +  %trimr(SB19in(xx))
122600070629     c                                     +  '%'''
122700061220x   3c                   else
122800061222     c                   eval      wSQL    =  wSQL
122900061220     c                                     +  %trimr($Sql_TITAA(9))
123000070629     c                                     +  %trimr(SB19in(xx))
123100070629     c                                     +  '%'''
123200061220e   3c                   endif
123300061220e   2c                   endif
123400061220      *
123500061220     c                   add       1             xx
123600061220e   1c                   ENDDO
123700061220      * chiusura selezioni (se impostata almeno una)
123800061220if  1c                   if        yy      >  *zeros
123900061220     c                   eval      wSQL    =  wSQL + ')'
124000061220e   1c                   endif
124100061220      *
124200061220     c                   ENDSR
124300070629      *
124400070629      *---------------------------------------------------------------*
124500070629      *?Aggiunta selezione rec. TITAA per località                   ?*
124600070629      *---------------------------------------------------------------*
124700070629     c     Add_SelTAAloc BEGSR
124800070629      *
124900070629     c                   eval      xx      =  1
125000070629     c                   clear                   yy
125100070629      * ciclo di impostazione selezioni
125200070629do  1c                   DOW       xx      <= %elem(SB19lo)
125300070629      *
125400070629if  2c                   if        SB19lo(xx) <> *blanks
125500070629     c                   add       1             yy
125600070629if  3c                   if        yy      =  1
125700070629     c                   eval      wSQL    =  wSQL
125800070629     c                                     +  %trimr($Sql_TITAA(10))
125900070629     c                                     +  %trimr(SB19lo(xx))
126000070629     c                                     +  '%'''
126100070629x   3c                   else
126200070629     c                   eval      wSQL    =  wSQL
126300070629     c                                     +  %trimr($Sql_TITAA(11))
126400070629     c                                     +  %trimr(SB19lo(xx))
126500070629     c                                     +  '%'''
126600070629e   3c                   endif
126700070629e   2c                   endif
126800070629      *
126900070629     c                   add       1             xx
127000070629e   1c                   ENDDO
127100070629      * chiusura selezioni (se impostata almeno una)
127200070629if  1c                   if        yy      >  *zeros
127300070629     c                   eval      wSQL    =  wSQL + ')'
127400070629e   1c                   endif
127500070629      *
127600070629     c                   ENDSR
127700061220      *
127800061220      *---------------------------------------------------------------*
127900061220      *?Aggiunta selezione rec. TITAA per provincia                  ?*
128000061220      *---------------------------------------------------------------*
128100061220     c     Add_SelTAAprv BEGSR
128200061220      *
128300061220     c                   eval      xx      =  1
128400061220     c                   clear                   yy
128500061220      * ciclo di impostazione selezioni
128600061220do  1c                   DOW       xx      <= %elem(SB19pr)
128700061220      *
128800061220if  2c                   if        SB19pr(xx) <> *blanks
128900061220     c                   add       1             yy
129000061220if  3c                   if        yy      =  1
129100061220     c                   eval      wSQL    =  wSQL
129200070629     c                                     +  %trimr($Sql_TITAA(12))
129300061220     c                                     +  SB19pr(xx) + ''''
129400061220x   3c                   else
129500061220     c                   eval      wSQL    =  wSQL + ', '''
129600061220     c                                     +  SB19pr(xx) + ''''
129700061220e   3c                   endif
129800061220e   2c                   endif
129900061220      *
130000061220     c                   add       1             xx
130100061220e   1c                   ENDDO
130200061220      * chiusura selezioni (se impostata almeno una)
130300061220if  1c                   if        yy      >  *zeros
130400061220     c                   eval      wSQL    =  wSQL + ')'
130500061220e   1c                   endif
130600061220      *
130700061220     c                   ENDSR
130800061220      *
130900061220      *---------------------------------------------------------------*
131000061220      *?Dichiarazione e Apertura cursore                             ?*
131100061220      *---------------------------------------------------------------*
131200061220     c     OpenCursor    BEGSR
131300061220      *
131400061220      * Dichiarazione cursore
131500061220      *
131600061220     c/exec sql
131700061220     c+     PREPARE S1 FROM :wSQL
131800061220     c/end-exec
131900061220     c*
132000061220     c/exec sql
132100061220     c+     DECLARE C1 CURSOR FOR S1
132200061220     c/end-exec
132300061220      *
132400061220      * Apertura del cursore
132500061220      *
132600061220     c/exec sql
132700061220     c+     OPEN C1
132800061220     c/end-exec
132900090313      *
133000090313w   1c                   if        SQLcode <  *zeros
133100090313     c                   eval      $Err    =  *on
133200120613     c                   eval      $EoF    =  *on
133300090313     c                   exsr      PrintErr
133400090313     c                   endif
133500061220      *
133600061220     c                   ENDSR
133700061220      *
133800061220      *---------------------------------------------------------------*
133900061220      *?Chiusura cursore                                             ?*
134000061220      *---------------------------------------------------------------*
134100061220     c     CloseCursor   BEGSR
134200061220      *
134300061220     c/exec sql
134400061220     c+     CLOSE C1
134500061220     c/end-exec
134600061220      *
134700061220     c                   ENDSR
134800050809      *
134900050810      *---------------------------------------------------------------*
135000050902      *?Lettura cursore                                              ?*
135100050810      *---------------------------------------------------------------*
135200061220     c     ReadCursor    BEGSR
135300050809      *
135400061220sel 1c                   select
135500061220      *
135600061220      * - Lettura file TITASS0F
135700061220w   1c                   when      wFile   =  1
135800061220     c                   clear                   TITASSds
135900050809     c/exec sql
136000061220     c+     FETCH NEXT FROM C1 INTO :TITASSds
136100050809     c/end-exec
136200061220      *
136300061220      * - Lettura file TITAS30C
136400061220w   1c                   when      wFile   =  2
136500061220     c                   clear                   TITASds
136600061220     c/exec sql
136700061220     c+     FETCH NEXT FROM C1 INTO :TITASds
136800061220     c/end-exec
136900061220      *
137000061220      * - Lettura file TITAA30C
137100061220w   1c                   when      wFile   =  3
137200061220     c                   clear                   TITAAds
137300061220     c/exec sql
137400061220     c+     FETCH NEXT FROM C1 INTO :TITAAds
137500061220     c/end-exec
137600061220e   1c                   endsl
137700050809      *
137800050809sel 1c                   select
137900090313w   1c                   when      SQLcode =  100
138000050809     c                   eval      $EoF    =  *on
138100090313w   1c                   when      SQLcode <  *zeros
138200050809     c                   eval      $Err    =  *on
138300120613     c                   eval      $EoF    =  *on
138400070703     c                   exsr      PrintErr
138500050809x   1c                   other
138600050809     c                   exsr      ElabRec
138700050809e   1c                   endsl
138800050809      *
138900050809     c                   ENDSR
139000070703      *
139100070703      *---------------------------------------------------------------*
139200070703      *?Stampa segnalazione dell'errore rilevato via SQL             ?*
139300070703      *---------------------------------------------------------------*
139400070703     c     PrintErr      BEGSR
139500090313      *
139600090313      /free
139700090313
139800090313         // Stampa Dump
139900090313         Dump(A);
140000090313
140100090313         // Stampa Job-Log
140200090313         Qcmd = 'DSPJOBLOG job(*) output(*print)';
140300090313         callp  qCmdExc(Qcmd : %size(Qcmd));
140400090313
140500090313      /end-free
140600070703      *
140700070704      * Stampa errore in QSYSPRT
140800070703sel 1c                   except    PRTtxt
140900070703     c                   eval      *inOF   =  *off
141000070703      *
141100070703     c                   except    PRTerr
141200070704      *
141300070704      * Registra segnalazione di errore in un record fittizio del WrkF
141400070704     c                   clear                   TITASSds
141500120615     c                   eval      sTASrsm =  'RILEVATO ERRORE SQL'
141600120615     c                   eval      sTASinm =  'Consegnare al CED il +
141700070704     c                                         relativo spool'
141800120615     c                   eval      sTAScam =  '"QSYSPRT"'
141900120615     c                   eval      sTASlom =  'con dati utente +
142000070704     c                                         "TNSB19*ERR".'
142100120615     c                   eval      sTASprm =  *all'*'
142200120615     c                   eval      sTASnzm =  *all'*'
142300120615      /free
142400120615         sTASrsd = *all'*';
142500120615         sTASind = *all'*';
142600120615         sTAScad = *all'*';
142700120615         sTASlod = *all'*';
142800120615         sTASprd = *all'*';
142900120615         sTASnzd = *all'*';
143000120615      /end-free
143100070704      *
143200070704     c                   exsr      WriteRec
143300070703      *
143400070703     c                   ENDSR
143500120613      /free
143600120613       //--------------------------------------------------------------
143700120613       //?Stampa segnalazione del mancato reperimento dei parametri   ?
143800120613       //--------------------------------------------------------------
143900120613       BEGSR  PrtNoParms;
144000120613
144100120613         // -?Stampa errore in QSYSPRT?
144200120613         except  PRTtxt;
144300120613         *inOF = *off;
144400120613
144500120613         except  PRTprm;
144600120613
144700120613         // -?Registra segnalazione di errore in un record fittizio del?
144800120613         //  ?WrkF?
144900120613         clear  TITASSds;
145000120615         sTASrsm = 'NON REPERITI I PARAMETRI DI LANCIO';
145100120615         sTASinm = 'RICERCA INTERROTTA.';
145200120615         sTAScam = *all'*';
145300120615         sTASlom = *all'*';
145400120615         sTASprm = *all'*';
145500120615         sTASnzm = *all'*';
145600120615         sTASrsd = *all'*';
145700120615         sTASind = *all'*';
145800120615         sTAScad = *all'*';
145900120615         sTASlod = *all'*';
146000120615         sTASprd = *all'*';
146100120615         sTASnzd = *all'*';
146200120613
146300120613         exsr  WriteRec;
146400120613
146500120613         // -?Imposta uscita dal *pgm?
146600120613         $Err = *on;
146700120613         $EoF = *on;
146800120613         exsr  EndSubr;
146900120613
147000120613       ENDSR;
147100120613
147200120613       //--------------------------------------------------------------
147300120613       //?Stampa segnalazione dei troppi record già registrati nel WF ?
147400120613       //--------------------------------------------------------------
147500120613       BEGSR  PrtTooMuchRec;
147600120613
147700120613         // -?Stampa errore in QSYSPRT?
147800120613         except  PRTtxt;
147900120613         *inOF = *off;
148000120613
148100120613         except  PRTmax;
148200120613
148300120613         // -?Registra segnalazione di errore in un record fittizio del?
148400120613         //  ?WrkF?
148500120613         clear  TITASSds;
148600120615         sTASrsm = 'GIÀ REGISTRATI TROPPI RECORD.';
148700120615         sTASinm = 'RICERCA INTERROTTA.';
148800120615         sTAScam = *all'*';
148900120615         sTASlom = *all'*';
149000120615         sTASprm = *all'*';
149100120615         sTASnzm = *all'*';
149200120615         sTASrsd = *all'*';
149300120615         sTASind = *all'*';
149400120615         sTAScad = *all'*';
149500120615         sTASlod = *all'*';
149600120615         sTASprd = *all'*';
149700120615         sTASnzd = *all'*';
149800120613
149900120613         exsr  WriteRec;
150000120613
150100120613         // -?Imposta uscita dal *pgm?
150200120613         $Err = *on;
150300120613         $EoF = *on;
150400120613         exsr  EndSubr;
150500120613
150600120613       ENDSR;
150700120613
150800120613      /end-free
150900050809      *
151000050810      *---------------------------------------------------------------*
151100061220      *?Elaborazione singolo record da TITASS0F/TITAS30C/TITAA30C    ?*
151200050810      *---------------------------------------------------------------*
151300050809     c     ElabRec       BEGSR
151400050809      *
151500061220      * Aggancio record x dati mancanti
151600061220sel 1c                   SELECT
151700120613      *
151800120613      * => Segnalazione immediata di errore: TROPPI REC.
151900120613     c                   WHEN      wCountRec >= c_Max_Rec
152000120613     c                   exsr      PrtTooMuchRec
152100061220      *
152200061220      * => -nulla-  (se reperito TITASS0F via sql)
152300061220w   1c                   WHEN      wFile   =  1
152400061220      *
152500061220      * => TITAA30C (se reperito TITAS30C via sql)
152600061220      *    per reperire i dati del mittente della spedizione in TITAS
152700061220      *    (per spedizioni da TITAA sono già stati reperiti, appunto,
152800061220      *     da TITAA)
152900061220w   1c                   WHEN      wFile   =  2
153000061220     c                   exsr      DecodMitt
153100061220     c                   exsr      Rie_TITASS
153200061220     c                   exsr      AltriDati
153300061220      *
153400061220      * => TITAS30C (se reperito TITAA30C via sql)
153500061220w   1c                   WHEN      wFile   =  3
153600061220     c     K04TAS30      chain     TITAS30C
153700061220if  2c                   if        NOT  %found(TITAS30C)
153800061220     c***                clear                   TITASds
153900061220     c                   leavesr
154000061220e   2c                   endif
154100061220      * -  Selezione delle spedizioni con segnacollo
154200061220if  2c                   if             TASfns <> 'S'
154300061220     c                             and  TASncd =  *zeros
154400061220     c                   leavesr
154500061220e   2c                   endif
154600061220      * -  Selezione per data spedizione (non reperibile da TITAA)
154700061220if  2c                   if             TASaas*10000+TASmgs < SB19dal
154800061220     c                             or   TASaas*10000+TASmgs > SB19al
154900061220     c                   leavesr
155000061220e   2c                   endif
155100061220     c                   exsr      Rie_TITASS
155200061220     c                   exsr      AltriDati
155300061220      *
155400061220e   1c                   ENDSL
155500050902      *
155600050902      * Scrittura record nel Work-File
155700050902     c                   exsr      WriteRec
155800050809      *
155900050809     c                   ENDSR
156000050901      *
156100050901      *---------------------------------------------------------------*
156200061219      *?Decodifica mittente (se dati da TITAS)                       ?*
156300050901      *---------------------------------------------------------------*
156400050901     c     DecodMitt     BEGSR
156500050901      *
156600050901     c                   move      TASccm        w0040
156700050901      *
156800061220if  1c                   if             w0040 = 9999
156900061220     c                             or   w0040 = 8888
157000061220     c                             or   w0040 = *zeros
157100050901      *
157200061220     c                   eval      TAAtrc  =  'M'
157300050901     c     K05TAA30      chain     TITAA30C
157400061220if  2c                   if        NOT  %found(TITAA30C)
157500050901     c                   clear                   TITAAds
157600050901e   2c                   endif
157700050901      *
157800050901x   1c                   else
157900050901      *
158000070216if  2c                   if             TASccm <> INDksc
158100070216     c                             or   TASccm <> ACOksc
158200070216     c                   clear                   TIBS69ds
158300070216     c                   clear                   ds_CNACO
158400070216     c                   clear                   ds_CNIND
158500050901     c                   z-add     TASccm        I69kac
158600050901     c                   z-add     TASccm        I69kin
158700050901     c                   call      'TIBS69R'
158800050901     c                   parm                    TIBS69ds
158900050901     c                   parm                    ds_CNACO
159000050901     c                   parm                    ds_CNIND
159100050901     c                   parm                    ds_CNCLP
159200050901     c                   parm                    ds_FNCLS
159300070216e   2c                   endif
159400070216if  2c***                if        O69err  <> *on
159500050901     c                   movel     ACOrag        TAArsc
159600050901     c                   movel     INDvia        TAAind
159700050902     c                   movel     INDcap        TAAcap
159800050901     c                   movel     INDcit        TAAloc
159900050902     c                   movel     INDprv        TAAprv
160000050902     c                   movel     INDsta        TAAnaz
160100070216e   2c***                endif
160200050901      *
160300050901e   1c                   endif
160400050901      *
160500050901     c                   ENDSR
160600061220      *
160700061220      *---------------------------------------------------------------*
160800061220      *?Impostazione campi del trk TITASS0F (con prefisso)           ?*
160900061220      *---------------------------------------------------------------*
161000061220     c     Rie_TITASS    BEGSR
161100061220      *
161200061220     c                   clear                   TITASSds
161300061220     c                   eval      sTASaas =  TASaas
161400061220     c                   eval      sTASlnp =  TASlnp
161500061220     c                   eval      sTASnrs =  TASnrs
161600061220     c                   eval      sTASnsp =  TASnsp
161700061220     c                   eval      sTAStbl =  TAStbl
161800061220     c                   eval      sTAScbo =  TAScbo
161900061220     c                   eval      sTASmgs =  TASmgs
162000061220     c                   eval      sTASksc =  TASksc
162100061220     c                   eval      sTASlna =  TASlna
162200061220     c                   eval      sTASfiv =  TASfiv
162300061220     c                   eval      sTASnft =  TASnft
162400061220     c                   eval      sTASdft =  TASdft
162500061220     c                   eval      sTASccm =  TASccm
162600061220     c                   eval      sTASrsm =  TAArsc
162700061220     c                   eval      sTASinm =  TAAind
162800061220     c                   eval      sTAScam =  TAAcap
162900061220     c                   eval      sTASlom =  TAAloc
163000061220     c                   eval      sTASprm =  TAAprv
163100061220     c                   eval      sTASnzm =  TAAnaz
163200061220     c                   eval      sTASrmn =  TASrmn
163300061220     c*** v.sr.AltriDati eval      sTASrma =
163400061220     c                   eval      sTASrsd =  TASrsd
163500061220     c                   eval      sTASind =  TASind
163600061220     c                   eval      sTAScad =  TAScad
163700061220     c                   eval      sTASlod =  TASlod
163800061220     c                   eval      sTASprd =  TASprd
163900061220     c                   eval      sTASnzd =  TASnzd
164000061220     c                   eval      sTASpdc =  TASpdc
164100061220     c*** v.sr.AltriDati eval      sTASpdd =
164200061220     c                   eval      sTASndc =  TASndc
164300061220     c                   eval      sTASddc =  TASddc
164400061220     c                   eval      sTASfdn =  TASfdn
164500061220     c                   eval      sTASffd =  TASffd
164600061220     c*** v.sr.AltriDati eval      sTAScas =
164700061220     c*** v.sr.AltriDati eval      sTASvca =
164800061220     c*** v.sr.AltriDati eval      sTASiva =
164900061220     c*** v.sr.AltriDati eval      sTAScdf =
165000061220     c***                eval      sTASsss =
165100061220     c                   eval      sTASncl =  TASncl
165200061220     c                   eval      sTASpkf =  TASpkf
165300061220     c                   eval      sTASvlf =  TASvlf
165400160122     c** v.sr.AltriDati  eval      sTASnas =  TASnas
165500061220      *
165600061220     c                   ENDSR
165700061220      *
165800061220      *---------------------------------------------------------------*
165900061220      *?Reperimento dati mancanti in TITAS30C e TITAA30C             ?*
166000061220      *---------------------------------------------------------------*
166100061220     c     AltriDati     BEGSR
166200061220      *
166300061220      * Decodifica padroncino
166400061220     c                   clear                   sTASpdd
166500061220if  1c                   if        TASpdc  <> *zeros
166600061220     c     K02APD01      chain     FIAPD000
166700061220if  2c                   if        %found(FIAPD01L)
166800061220     c                   movel     APDrsc        sTASpdd
166900061220e   2c                   endif
167000061220e   1c                   endif
167100061220      *
167200061220      * Recupero c/assegno
167300061220     c                   clear                   sTAScas
167400061220     c                   clear                   sTASvca
167500061220if  1c                   if        TASfca  =  'S'
167600061220     c     K04CSB03      chain     TNCSB000
167700061220if  2c                   if        %found(TNCSB03L)
167800061220     c                   z-add     CSBcas        sTAScas
167900061220     c                   movel     CSBvca        sTASvca
168000061220e   2c                   endif
168100061220e   1c                   endif
168200061220      *
168300160122      * Riferimento mittente alfanumerico e natura merce
168400061220     c                   clear                   sTASrma
168401160122     c                   clear                   sTASnas
168500061220     c                   eval      TA4trc  =  'A'
168600061220     c     K05TA430      chain     TITA430C
168700061220if  1c                   if        %found(TITA430C)
168800061220     c                   movel     TA4not        dTA4A
168900061220     c                   movel     §TA4Arma      sTASrma
168901160122     c                   movel     §TA4Anas      sTASnas
169000061220e   1c                   endif
169100061220      *
169200061220      * Codice Fiscale
169300061220     c                   clear                   sTAScdf
169400061220     c                   eval      TA4trc  =  'Q'
169500061220     c     K05TA430      chain     TITA430C
169600061220if  1c                   if        %found(TITA430C)
169700061220     c                   movel     TA4not        dTA4Q
169800061220      * - Verifico se Franco o Assegnato
169900061220sel 2c                   select
170000061220w   2c                   when      %subst(TAStbl:1:1) = 'F'
170100061220     c                   eval      sTAScdf =  §TA4Qcfm
170200061220w   2c                   when      %subst(TAStbl:1:1) = 'A'
170300061220     c                   eval      sTAScdf =  §TA4Qcfd
170400061220e   2c                   endsl
170500061220     c                   endif
170600061220      *
170700061220      * Partita IVA
170800061220     c                   clear                   sTASiva
170900061220sel 1c                   select
171000061220w   1c                   when      %subst(TAStbl:1:1) = 'A'
171100061220     c                             and  TAScpd <> *blanks
171200061220     c                   eval      sTASiva =  TAScpd
171300061220w   1c                   when      %subst(TAStbl:1:1) = 'F'
171400061220     c                   eval      sTASiva =  TAAcpi
171500061220     c                   endsl
171600061220      *
171700061220      * Partita IVA / Codice Fiscale
171800061220      *   SE partita iva non valorizzata
171900061220if  1c                   IF             sTASiva = *blanks
172000061220     c                             or   sTAScdf = *blanks
172100061220      * - - codificato franco o assegnato
172200061220     c                   move      TASksc        w0040
172300061220if  2c                   If             w0040  <> 8888
172400061220     c                             and  w0040  <> 9999
172500061220     c                   z-add     TASksc        I69kin
172600061220     c                   call      'TIBS69R'
172700061220     c                   parm                    TIBS69ds
172800061220     c                   parm                    ds_CNACO
172900061220     c                   parm                    ds_CNIND
173000061220     c                   parm                    ds_CNCLP
173100061220     c                   parm                    ds_FNCLS
173200061220if  3c                   if        O69err  <> *on
173300061220if  4c                   if        sTASiva =  *blanks
173400061220     c                   eval      sTASiva =  INDiva
173500061220e   4c                   endif
173600061220if  4c                   if        sTAScdf =  *blanks
173700061220     c                   eval      sTAScdf =  INDcdf
173800061220e   4c                   endif
173900061220e   3c                   endif
174000061220e   2c                   EndIf
174100061220e   1c                   ENDIF
174200061220      *
174300061220     c                   ENDSR
174400050902      *
174500050902      *---------------------------------------------------------------*
174600050902      *?Scrittura record nel Work-File WFNIB00F                      ?*
174700050902      *---------------------------------------------------------------*
174800050902     c     WriteRec      BEGSR
174900050902      *
175000050902     c                   clear                   WFNIB000
175100050902      * Campi chiave
175200061219      * - Utente esecuzione
175300061219     c                   eval      WNBkut  =  KNMUS
175400050902      * - Data esecuzione
175500050902     c     *iso          move      Data_Iso      WNBkdt
175600050902      * - Ora  esecuzione
175700050902     c                   movel     wTimeHM       WNBkhm
175800061220      * - 1ª ragione sociale o 1° codice
175900050902     c                   eval      xx      =  1
176000061220do  1c                   dow       xx      <= %elem(SB19rs)
176100050902if  2c                   if        SB19rs(xx) <> *blanks
176200050902     c                   movel     SB19rs(xx)    WNBkrs
176300050902     c                   leave
176400050902e   2c                   endif
176500061205     c                   add       1             xx
176600050902e   1c                   enddo
176700061220      * - 1° indirizzo (SE non immessa ragione sociale o codice)
176800061219if  1c                   IF        WNBkrs  =  *blanks
176900061205     c                   eval      xx      =  1
177000061220do  2c                   dow       xx      <= %elem(SB19in)
177100061219if  3c                   if        SB19in(xx) <> *blanks
177200061205     c                   movel     SB19in(xx)    WNBkrs
177300061205     c                   leave
177400061219e   3c                   endif
177500061205     c                   add       1             xx
177600061219e   2c                   enddo
177700061219e   1c                   endif
177800070703      * - 1ª località  (SE non immessi né ragione sociale o codice
177900070703      *                                né indirizzo)
178000070703if  1c                   IF        WNBkrs  =  *blanks
178100070703     c                   eval      xx      =  1
178200070703do  2c                   dow       xx      <= %elem(SB19lo)
178300070703if  3c                   if        SB19lo(xx) <> *blanks
178400070703     c                   movel     SB19lo(xx)    WNBkrs
178500070703     c                   leave
178600070703e   3c                   endif
178700070703     c                   add       1             xx
178800070703e   2c                   enddo
178900070703e   1c                   endif
179000070703      * - 1ª provincia (SE non immessi né ragione sociale o codice
179100070703      *                                né indirizzo  né località)
179200070703if  1c                   IF        WNBkrs  =  *blanks
179300070703     c                   eval      xx      =  1
179400070703do  2c                   dow       xx      <= %elem(SB19pr)
179500070703if  3c                   if        SB19pr(xx) <> *blanks
179600070703     c                   movel     SB19pr(xx)    WNBkrs
179700070703     c                   leave
179800070703e   3c                   endif
179900070703     c                   add       1             xx
180000070703e   2c                   enddo
180100070703e   1c                   endif
180200050902      * - Tipo ricerca (M=Mittente, D=Destinatario, E=Entrambi)
180300051024      *                 K=Codice Cliente [CCM o KSC]
180400061219     c                   eval      WNBkri  =  SB19ric
180500050902      * P.O. partenza
180600061220     c                   eval      WNBlnp  =  sTASlnp
180700050902      * Numero serie
180800061220     c                   eval      WNBnrs  =  sTASnrs
180900050902      * Numero spedizione
181000061220     c                   eval      WNBnsp  =  sTASnsp
181100050902      * Data spedizione
181200070629     c                   eval      WNBdsp  =  (sTASaas*10000+sTASmgs)
181300061219      * Tipo bolla
181400061220     c                   eval      WNBtbl  =  sTAStbl
181500061219      * Codice bolla
181600061220     c                   eval      WNBcbo  =  sTAScbo
181700050902      * P.O. arrivo
181800061220     c                   eval      WNBlna  =  sTASlna
181900050902      * P.O. i.v.a.
182000061220     c                   eval      WNBfiv  =  sTASfiv
182100050902      * Numero fattura
182200061220     c                   eval      WNBnft  =  sTASnft
182300050902      * Data fattura
182400061220     c                   eval      WNBdft  =  sTASdft
182500050902      * Mittente: ragione sociale
182600061220     c                   eval      WNBrsm  =  sTASrsm
182700050902      * Mittente: indirizzo
182800061220     c                   eval      WNBinm  =  sTASinm
182900050902      * Mittente: cap
183000061220     c                   eval      WNBcam  =  sTAScam
183100050902      * Mittente: località
183200061220     c                   eval      WNBlom  =  sTASlom
183300050902      * Mittente: provincia
183400061220     c                   eval      WNBprm  =  sTASprm
183500050902      * Mittente: nazione
183600061220     c                   eval      WNBnzm  =  sTASnzm
183700050902      * Destinatario: ragione sociale
183800061220     c                   eval      WNBrsd  =  sTASrsd
183900050902      * Destinatario: indirizzo
184000061220     c                   eval      WNBind  =  sTASind
184100050902      * Destinatario: cap
184200061220     c                   eval      WNBcad  =  sTAScad
184300050902      * Destinatario: località
184400061220     c                   eval      WNBlod  =  sTASlod
184500050902      * Destinatario: provincia
184600061220     c                   eval      WNBprd  =  sTASprd
184700050902      * Destinatario: nazione
184800061220     c                   eval      WNBnzd  =  sTASnzd
184900061219      * Riferimento mittente numerico
185000061220     c                   eval      WNBrmn  =  sTASrmn
185100061219      * Riferimento mittente alfanumerico
185200061220     c                   eval      WNBrma  =  sTASrma
185300061219      * Codice     padroncino consegna
185400061220     c                   eval      WNBpdc  =  sTASpdc
185500061219      * Decodifica padroncino consegna
185600061220     c                   eval      WNBpdd  =  sTASpdd
185700061219      * Numero distinta consegna
185800061220     c                   eval      WNBndc  =  sTASndc
185900061219      * Data   distinta consegna
186000061220     c                   eval      WNBddc  =  sTASddc
186100061219      * Flag "Merce portata a magazzino" ('S')
186200061220     c                   eval      WNBfdn  =  sTASfdn
186300061219      * Flag "Fermo deposito"            ('S')
186400061220     c                   eval      WNBffd  =  sTASffd
186500061219      * Importo contrassegno
186600061220     c                   eval      WNBcas  =  sTAScas
186700061219      * Divisa  contrassegno
186800061220     c                   eval      WNBvca  =  sTASvca
186900061219      * Codice partita iva europea
187000061220     c                   eval      WNBpiv  =  sTASiva
187100061219      * Codice fiscale
187200061220     c                   eval      WNBcdf  =  sTAScdf
187300050902      * Numero colli
187400061220     c                   eval      WNBncl  =  sTASncl
187500050902      * Peso in kg
187600061220     c                   eval      WNBpkf  =  sTASpkf
187700050902      * Volume
187800061220     c                   eval      WNBvlf  =  sTASvlf
187900050902      * Natura merce
188000061220     c                   eval      WNBnas  =  sTASnas
188100050902      *
188200050902     c                   WRITE     WFNIB000
188300120613      *
188400120613     c                   eval      wCountRec += 1
188500050902      *
188600050902     c                   ENDSR
188700070703      *
188800070703      *?O U T P U T?--------------------------------------------------*
188900070703      *
189000070704     oQSYSPRT   e            PRTtxt            1
189100070703     o                       RSUT
189200070703     o                                        +   6 'LISTA ERRORI RILE-
189300070703     o                                              VATI IN FASE DI RI-
189400070703     o                                              CERCA NOMINATIVI I-
189500070703     o                                              N BOLLE SEDE PER G-
189600070703     o                                              .D.F.'
189700070703     o                       SDSpgm           +   6
189800070703     o                       wDate         y  +   3
189900070703     o          e            PRTtxt      1
190000070703     o                       KNSIF
190100070703     o                       KNMUS            +   1
190200070703     o                                        +   5 '------------------
190300070703     o                                              -------------------
190400070703     o                                              -------------------
190500070703     o                                              -------------------
190600070703     o                                              -----'
190700070703     o                                        +   6 'Pag.'
190800070703     o                       Page          z  +   0
190900070703     o                       wTime            +   5 '  :  :  '
191000120613      *
191100120613     o          e            PRTprm      2
191200120613     o                                              'NON REPERITI I PARAMETRI-
191300120613     o                                               DI LANCIO.'
191400120613     o          e            PRTprm      1
191500120613     o                                              'RICERCA INTERROTTA.'
191600120613      *
191700120613     o          e            PRTmax      2
191800120613     o                                              'GIÀ REGISTRATI TROPPI RE-
191900120613     o                                              CORD NEL FILE WFNIB00F.'
192000120613     o          e            PRTmax      1
192100120613     o                                              'RICERCA INTERROTTA.'
192200120613      *
192300070704     o          e            PRTerr      2
192400090313     o                                              'RILEVATO ERRORE: SQLCODE'
192500090313     o                       SQLcode       k  +   1
192600090313     o                                        +   1 'SQLSTATE'
192700090313     o                       SQLstate         +   1
192800070703     o                                        +   1 'DURANTE L''ESECUZ-
192900070703     o                                              IONE DEL SEGUENTE -
193000070703     o                                              COMANDO SQL:'
193100070703     o          e            PRTerr      0
193200090313     o                                              'RILEVATO ERRORE: SQLCODE'
193300090313     o                       SQLcode       k  +   1
193400090313     o                                        +   1 'SQLSTATE'
193500090313     o                       SQLstate         +   1
193600070703     o          e            PRTerr      1
193700070704     o                       $Temp( 1)        +   3
193800070703     o          e            PRTerr      1
193900070704     o                       $Temp( 2)        +   3
194000070703     o          e            PRTerr      1
194100070704     o                       $Temp( 3)        +   3
194200070703     o          e            PRTerr      1
194300070704     o                       $Temp( 4)        +   3
194400070703     o          e            PRTerr      1
194500070704     o                       $Temp( 5)        +   3
194600070703     o          e            PRTerr      1
194700070704     o                       $Temp( 6)        +   3
194800070703     o          e            PRTerr      1
194900070704     o                       $Temp( 7)        +   3
195000070703     o          e            PRTerr      1
195100070704     o                       $Temp( 8)        +   3
195200070703     o          e            PRTerr      1
195300070704     o                       $Temp( 9)        +   3
195400070703     o          e            PRTerr      1
195500070703     o                       $Temp(10)        +   3
195600070704     o          e            PRTerr      1
195700070704     o                       $Temp(11)        +   3
195800070704     o          e            PRTerr      1
195900070704     o                       $Temp(12)        +   3
196000070704     o          e            PRTerr      1
196100070704     o                       $Temp(13)        +   3
196200070704     o          e            PRTerr      1
196300070704     o                       $Temp(14)        +   3
196400070704     o          e            PRTerr      1
196500070704     o                       $Temp(15)        +   3
196600070704     o          e            PRTerr      1
196700070704     o                       $Temp(16)        +   3
196800070704     o          e            PRTerr      1
196900070704     o                       $Temp(17)        +   3
197000070704     o          e            PRTerr      1
197100070704     o                       $Temp(18)        +   3
197200070704     o          e            PRTerr      1
197300070704     o                       $Temp(19)        +   3
197400070704     o          e            PRTerr      1
197500070704     o                       $Temp(20)        +   3
197600070704     o          e            PRTerr      1
197700070704     o                       $Temp(21)        +   3
197800070704     o          e            PRTerr      1
197900070704     o                       $Temp(22)        +   3
198000070704     o          e            PRTerr      1
198100070704     o                       $Temp(23)        +   3
198200070704     o          e            PRTerr      1
198300070704     o                       $Temp(24)        +   3
198400070704     o          e            PRTerr      1
198500070704     o                       $Temp(25)        +   3
198600070704     o          e            PRTerr      1
198700070704     o                       $Temp(26)        +   3
198800070704     o          e            PRTerr      1
198900070704     o                       $Temp(27)        +   3
199000070704     o          e            PRTerr      1
199100070704     o                       $Temp(28)        +   3
199200070704     o          e            PRTerr      1
199300070704     o                       $Temp(29)        +   3
199400070704     o          e            PRTerr      1
199500070704     o                       $Temp(30)        +   3
199600070704     o          e            PRTerr      1
199700070704     o                       $Temp(31)        +   3
199800070704     o          e            PRTerr      1
199900070704     o                       $Temp(32)        +   3
200000070704     o          e            PRTerr      1
200100070704     o                       $Temp(33)        +   3
200200070704     o          e            PRTerr      1
200300070704     o                       $Temp(34)        +   3
200400070704     o          e            PRTerr      1
200500070704     o                       $Temp(35)        +   3
200600070704     o          e            PRTerr      1
200700070704     o                       $Temp(36)        +   3
200800070704     o          e            PRTerr      1
200900070704     o                       $Temp(37)        +   3
201000070704     o          e            PRTerr      1
201100070704     o                       $Temp(38)        +   3
201200070704     o          e            PRTerr      1
201300070704     o                       $Temp(39)        +   3
201400070704     o          e            PRTerr      1
201500070704     o                       $Temp(40)        +   3
201600070704     o          e            PRTerr      1
201700070704     o                       $Temp(41)        +   3
201800070704     o          e            PRTerr      1
201900070704     o                       $Temp(42)        +   3
202000070704     o          e            PRTerr      1
202100070704     o                       $Temp(43)        +   3
202200070704     o          e            PRTerr      1
202300070704     o                       $Temp(44)        +   3
202400070704     o          e            PRTerr      1
202500070704     o                       $Temp(45)        +   3
202600070704     o          e            PRTerr      1
202700070704     o                       $Temp(46)        +   3
202800070704     o          e            PRTerr      1
202900070704     o                       $Temp(47)        +   3
203000070704     o          e            PRTerr      1
203100070704     o                       $Temp(48)        +   3
203200070704     o          e            PRTerr      1
203300070704     o                       $Temp(49)        +   3
203400070704     o          e            PRTerr      1
203500070704     o                       $Temp(50)        +   3
203600070704     o          e            PRTerr      1
203700070704     o                       $Temp(51)        +   3
203800070704     o          e            PRTerr      1
203900070704     o                       $Temp(52)        +   3
204000070704     o          e            PRTerr      1
204100070704     o                       $Temp(53)        +   3
204200070704     o          e            PRTerr      1
204300070704     o                       $Temp(54)        +   3
204400070704     o          e            PRTerr      1
204500070704     o                       $Temp(55)        +   3
204600070704     o          e            PRTerr      1
204700070703      *
204800070703      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
204900070703      *
205000061220**   $Sql_TITASS -----------------*
205100070703select * from TITASS0F                       1
205200070703 where TASsss = ' '                          2
205300070703  and (TASaas*10000+TASmgs) between          3
205400070703TASrsm like '%                               4
205500070703TASinm like '%                               5
205600070703TASlom like '%                               6
205700070703TASprm in ('                                 7
205800070703TASrsd like '%                               8
205900070703TASind like '%                               9
206000070703TASlod like '%                              10
206100070703TASprd in ('                                11
206200070703  and (TASksc in (                          12
206300070703   or  TASccm in (                          13
206400061220**   $Sql_TITAS -----------------------*
206500061220select *                                     1
206600061220 from  TITASP0F                              2
206700061220 from  TITAS10F                              3
206800061220 from  TITAS00F                              4
206900061220 where (TASfns  =  'S'  or  TASncd <> 0)     5
207000061220  and (TASaas*10000+TASmgs) between          6
207100061220  and (TASrsd like '%                        7
207200061220   or  TASrsd like '%                        8
207300061220  and (TASind like '%                        9
207400061220   or  TASind like '%                       10
207500070629  and (TASlod like '%                       11
207600070629   or  TASlod like '%                       12
207700070629  and  TASprd  in ('                        13
207800070629  and (TASksc  in (                         14
207900070629   or  TASccm  in (                         15
208000061220**   $Sql_TITAA --------*
208100061220select *                                     1
208200061220 from  TITAAP0F                              2
208300061220 from  TITAA00F                              3
208400061220 where TAAtrc  =   'M'                       4
208500061220  and  TAAaas between                        5
208600061220  and (TAArsc like '%                        6
208700061220   or  TAArsc like '%                        7
208800061220  and (TAAind like '%                        8
208900061220   or  TAAind like '%                        9
209000070629  and (TAAloc like '%                       10
209100070629   or  TAAloc like '%                       11
209200070629  and  TAAprv  in ('                        12
