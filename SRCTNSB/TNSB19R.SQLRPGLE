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
002100180307      *================================================================
002200180307      *? TNSB19R * Ricerca nominativi in bolle sede per G.d.F.          ?
002300180307      *================================================================
002400050902      *
002500050809     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600050809     h alwnull(*inputonly)
002700050902      *
002800180307      *? A R C H I V I ?------------------------------------------------*
002900050902      *
003000050810     fTITAS30C  if   e           k disk
003100180307     f                                     ignore( TITASP00 )
003200050901     fTITAA30C  if   e           k disk
003300180307     f                                     ignore( TITAAP00 )
003400061220      *
003500061220     fTABEL00F  if   e           k disk
003600061219     fFIAPD01L  if   e           k disk
003700061220     fTNCSB03L  if   e           k disk
003800061220     fTITA430C  if   e           k disk
003900050809      *
004000050902     fWFNIB00F  o    e             disk    usropn
004100070703     fQSYSPRT   o    f  132        printer usropn
004200070703     f                                     oflind(*inOF)
004300050902      *
004400180307      *? C O S T A N T I ?----------------------------------------------*
004500050902      *
004600061220     d C_Union         c                   const(' UNION ')
004700061220     d C_ForFetch      c                   const(' FOR FETCH ONLY')
004800070703     d C_OvrPrtF       c                   const('OVRPRTF +
004900070703     d                                            FILE(QSYSPRT) +
005000070703     d                                            USRDTA(''TNSB19+
005100070703     d                                                     *ERR'')')
005200070703     d C_DltOvr        c                   const('DLTOVR +
005300070703     d                                            FILE(QSYSPRT)')
005400120615     d c_Max_Rec       c                   const(63000)
005500050809      *
005600180307      *? S C H I E R E ?------------------------------------------------*
005700050809      *
005800061220      * - Stringhe SQL da "comporre" per estrarre i dati dagli archivi
005900070629     d $Sql_TITAS      s             40    dim(15) ctdata perrcd(1)
006000070629     d $Sql_TITAA      s             25    dim(12) ctdata perrcd(1)
006100070702      *
006200070702     d $Temp           s            100    inz  dim(100)
006300050809      *
006400180307      *? S T R U T T U R E   D A T I ?----------------------------------*
006500050809      *
006600050809      * - Parametri
006700050809     d KPJBA         e ds
006800050809     d TNSB19ds      e ds                  inz
006900070704     d   SB19rs               19    168    inz  dim(5)
007000070704     d   SB19in              169    318    inz  dim(5)
007100070704     d   SB19lo              319    468    inz  dim(5)
007200070704     d   SB19pr              469    478    inz  dim(5)
007300070629      *
007400070629      * - Parametri per gestione parametri batch
007500070629     d TRUL01ds      e ds                  inz
007600120612     d*//UL01opz     e                     inz(*off)
007700070629     d   UL01err     e                     inz(*off)
007800050809      *
007900050809      * - Parametri x Controllo profilo utenti
008000050902     d TIBS34ds      e ds                  inz
008100050809      * - Ds di riferimento al file esterno AZUTE00F
008200050902     d AZUTEds       e ds                  extname(AZUTE00F)
008300050809      * - Ds per dati organigramma
008400070703     d dDatiUte      e ds
008500061220      *
008600061220      * - Dati reperiti via SQL
008700061220     d TITAAds       e ds                  inz  extname(TITAA00F)
008800061220     d TITASds       e ds                  inz  extname(TITAS00F)
008900061219      *
009000180308      *//* - Ds tabella $3 (Date avanzamento Trasferimento bolle)
009100180308     d*// ds$3          e ds                  inz
009200061220      * - Ds Riferimenti (trk "A" file TITA400F)
009300061220     d dTA4a         e ds                  inz
009400061220      * - Ds Cod. Fiscale mittente/destinatario (trk "Q" file TITA400F)
009500061220     d dTA4q         e ds                  inz
009600050901      *
009700061220      * - Ds per reperimento dati anagrafici clienti
009800050902     d TIBS69ds      e ds                  inz
009900050901     d DS_cnaco      e ds                  inz  extname(CNACO00F)
010000050901     d DS_cnind      e ds                  inz  extname(CNIND00F)
010100050901     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
010200050901     d DS_fncls      e ds                  inz  extname(FNCLS00F)
010300050809      *
010400050809     d Status         sds
010500070704     d   SDSpgm          *proc
010600090915      *
010700090915     d                 ds                  inz
010800090915     d wDateTime                     14  0 inz
010900090915     d   wTime                 1      6s 0 inz
011000090915     d   wDate                 7     14s 0 inz
011100050809      *
011200180307      *? V A R I A B I L I ?--------------------------------------------*
011300050809      *
011400050809      * - Flags booleani
011500050809     d $EoF            s              1    inz(*off)
011600050809     d $Err            s              1    inz(*off)
011700070702     d $Selez          s              1    inz(*off)
011800050809      * - Indici di schiera / Contatori
011900050810     d xx              s              3  0 inz
012000050810     d yy              s              3  0 inz
012100160908     d wCountRec       s              9  0 inz
012200050809      * - Parametri per SQL
012300180309     d wSQL            s          32740    inz  Varying
012400050901     d wAAI            s              4  0 inz
012500050901     d wAAF            s              4  0 inz
012600070703     d wFile           s              1  0 inz(1)
012700070703      * - Parametri per QCMDEXC
012800070703     d Qcmd            s             80    inz
012900070703     d Qlen            s             15  5 inz(80)
013000050809      * - Campi di comodo
013100180307     d wrkWNBkrs       s                   inz  like(WNBkrs)
013200050902     d Data_Iso        s               d   inz  datfmt(*iso)
013300050902     d wTimeHM         s              4  0 inz
013400050901     d w0040           s              4  0 inz
013500090915     d w0080           s              8  0 inz
013600090313
013700090313       //--------------------------------------------------------------
013800180307       //? Definizione procedure esterne.
013900090313       //--------------------------------------------------------------
014000090313
014100090313       // - Esecuzione comando di sistema
014200090313     d qCmdExc         pr                  extpgm('QCMDEXC')
014300090313     d   Qcmd                       512    const  options(*varsize)
014400090313     d   Qlen                        15  5 const
014500050809      *
014600180307      *? K E Y - L I S T ?----------------------------------------------*
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
018300180307      *? I N D I C A T O R I ?------------------------------------------*
018400050902      *
018500050902      *
018600070703      *===============================================================*
018700050902      *
018800050809     c     *Entry        plist
018900050809     c                   parm                    KPJBA
019000050809      *
019100050809      * Operazioni iniziali
019200180307     c                   exsr      sr_InzSubr
019300061220      *
019400180307      * Estrazione dati dal file TITAS00F+10F
019500061220if  1c                   if        SB19ric <> 'M'
019600061220     c                   eval      wFile   =  2
019700061220     c                   exsr      sr_TITAS
019800061220e   1c                   endif
019900061220      *
020000180307      * Estrazione dati dal file TITAA00F
020100061220if  1c                   if             SB19ric =  'M'
020200061220     c                             or   SB19ric =  'E'
020300061220     c                   eval      wFile   =  3
020400061220     c                   exsr      sr_TITAA
020500061220e   1c                   endif
020600061220      *
020700061220      * Operazioni finali
020800180307     c                   exsr      sr_EndSubr
020900050902      *
021000180307      *? S U B R O U T I N E S ?----------------------------------------*
021100050902      *
021200180307      *----------------------------------------------------------------
021300180307      *? Operazioni iniziali                                            ?
021400180307      *----------------------------------------------------------------
021500180307     c     sr_InzSubr    BEGSR
021600070216      *
021700070216     c/exec sql
021800070216     c+     set  option  DYNUSRPRF = *OWNER,
021900070216     c+                  CLOSQLCSR = *ENDMOD
022000070216     c/end-exec
022100120613      *
022200120613     c                   eval      *inLR   =  *on
022300050809      *
022400050809      * Reperisce dati job
022500050809     c     *dtaara       define    §azute        azuteds
022600050809     c     *dtaara       define    §datiute      ddatiute
022700050809      *
022800050809     c                   in(E)     *dtaara
022900050809     c                   IF        %ERROR or RSUT = *blanks
023000050809     c                   clear                   Tibs34Ds
023100050809     c                   call      'TIBS34R'
023200050809     c                   parm                    Tibs34Ds
023300050809     c                   in        *dtaara
023400050809     c                   ENDIF
023500070629      *
023600070629      * Reperisce parametri da WFPRM00F
023700070629     c                   clear                   TNSB19ds
023800070629     c                   reset                   TRUL01ds
023900070629     c                   movel     KPJBU         UL01prg
024000070629     c                   call      'TRUL01R'
024100070629     c                   parm                    TRUL01ds
024200070703     c                   if        UL01err =  *on
024300180307     c                   exsr      sr_PrtNoParms
024400180307     c                   exsr      sr_EndSubr
024500070629     c                   endif
024600070629     c                   movel     UL01prm       TNSB19ds
024700180308      *//*
024800180308      *//* Recupero ultima data pulizia del titasp in cassetta
024900180308     c*//                eval      TBLkut  =  1
025000180308     c*//                eval      TBLcod  =  '$3'
025100180308     c*//                eval      TBLkey  =  '1       '
025200180308     c*//  K03Tabel      chain     TABEL
025300180308     c*//                if        %found(TABEL00F)
025400180308     c*//                movel     TBLuni        ds$3
025500180308     c*//                endif
025600050902      *
025700050902      * Impostazione data e ora per campi chiave del WrkF
025800090915     c                   time                    wDateTime
025900090915     c     *eur          movel     wDate         Data_Iso
026000050902     c                   movel     wTime         wTimeHM
026100061220      *
026200061220      * Impostazione anno/i di selezione
026300061220     c                   movel     SB19dal       wAAI
026400061220     c                   movel     SB19al        wAAF
026500180307      *
026600180307      * Impostazione dati "fissi" nel *work-file di output
026700180307      * - 1ª ragione sociale o 1° codice o 1ª spedizione o 1° padroncino
026800180307     c                   eval      xx      =  1
026900180307do  1c                   dow       xx      <= %elem(SB19rs)
027000180307if  2c                   if        SB19rs(xx) <> *blanks
027100180307     c                   movel     SB19rs(xx)    wrkWNBkrs
027200180307     c                   leave
027300180307e   2c                   endif
027400180307     c                   add       1             xx
027500180307e   1c                   enddo
027600180307      * - 1° indirizzo (SE non immessa ragione sociale o codice
027700180307      *                              o spedizione      o padroncino)
027800180307if  1c                   IF        wrkWNBkrs  =  *blanks
027900180307     c                   eval      xx      =  1
028000180307do  2c                   dow       xx      <= %elem(SB19in)
028100180307if  3c                   if        SB19in(xx) <> *blanks
028200180307     c                   movel     SB19in(xx)    wrkWNBkrs
028300180307     c                   leave
028400180307e   3c                   endif
028500180307     c                   add       1             xx
028600180307e   2c                   enddo
028700180307e   1c                   endif
028800180307      * - 1ª località  (SE non immessi né ragione sociale o codice
028900180307      *                                 o spedizione      o padroncino
029000180307      *                                né indirizzo)
029100180307if  1c                   IF        wrkWNBkrs  =  *blanks
029200180307     c                   eval      xx      =  1
029300180307do  2c                   dow       xx      <= %elem(SB19lo)
029400180307if  3c                   if        SB19lo(xx) <> *blanks
029500180307     c                   movel     SB19lo(xx)    wrkWNBkrs
029600180307     c                   leave
029700180307e   3c                   endif
029800180307     c                   add       1             xx
029900180307e   2c                   enddo
030000180307e   1c                   endif
030100180307      * - 1ª provincia (SE non immessi né ragione sociale o codice
030200180307      *                                 o spedizione      o padroncino
030300180307      *                                né indirizzo  né località)
030400180307if  1c                   IF        wrkWNBkrs  =  *blanks
030500180307     c                   eval      xx      =  1
030600180307do  2c                   dow       xx      <= %elem(SB19pr)
030700180307if  3c                   if        SB19pr(xx) <> *blanks
030800180307     c                   movel     SB19pr(xx)    wrkWNBkrs
030900180307     c                   leave
031000180307e   3c                   endif
031100180307     c                   add       1             xx
031200180307e   2c                   enddo
031300180307e   1c                   endif
031400061220      *
031500061220      * Impostazione chiavi di accesso "fisse"
031600070703     c                   eval      APDtip  =  'A'
031700050902      *
031800090915      /free
031900090916
032000090915         w0080 = %dec( %date( wDate : *eur ) - %months(2) );
032100090916
032200090916         // - Richiesta pulizia del workfile WFNIB00F
032300090916         if  SB19cwf = 'S';
032400090916           exec sql     DELETE from WFNIB00F
032500090916                         where WNBkut  = :KNMUS
032600090916                            or WNBkdt <= :w0080;
032700090916         else;
032800090916           // - ...Comunque vengono cancellati i record relativi alle
032900090916           //   estrazioni di più di 2 (due) mesi fa - di tutti gli
033000090916           //   utenti!!!
033100090916           exec sql     DELETE from WFNIB00F
033200090916                         where WNBkdt <= :w0080;
033300090916         endif;
033400090916
033500090915      /end-free
033600050902      *
033700050902      * Apertura del workfile (dopo l'eventuale pulizia)
033800050902     c                   open      WFNIB00F
033900070703      *
034000070703      * Apertura del printer-file
034100070703     c                   call      'QCMDEXC'
034200070703     c                   parm      C_OvrPrtF     Qcmd
034300070703     c                   parm                    Qlen
034400070703     c                   open      QSYSPRT
034500050809      *
034600050809     c                   ENDSR
034700061220      *
034800180307      *----------------------------------------------------------------
034900180307      *? Operazioni finali                                              ?
035000180307      *----------------------------------------------------------------
035100180307     c     sr_EndSubr    BEGSR
035200061220      *
035300061220      * Chiusura del workfile
035400120612     c                   close(e)  WFNIB00F
035500070703      *
035600070703      * Chiusura del printer-file
035700120612     c                   close(e)  QSYSPRT
035800070703     c                   call      'QCMDEXC'
035900070703     c                   parm      C_DltOvr      Qcmd
036000070703     c                   parm                    Qlen
036100120613      *
036200120613     c                   return
036300061220      *
036400061220     c                   ENDSR
036500061220      *
036600180307      *----------------------------------------------------------------
036700180307      *? Estrazione dati dal file TITAS00F+10F                          ?
036800180307      *----------------------------------------------------------------
036900061220     c     sr_TITAS      BEGSR
037000061220      *
037100061220      * Preparazione stringa SQL
037200180307     c                   exsr      sr_PrepSQL_1
037300090313     c                   reset                   $EoF
037400061220      *
037500061220      * Apertura cursore
037600180307     c                   exsr      sr_OpenCursor
037700061220      *
037800061220      * Elaborazione
037900120613     c                   dou       $EoF    =  *on
038000180307     c                   exsr      sr_ReadCursor
038100061220     c                   enddo
038200061220      *
038300061220      * Chiusura cursore
038400180307     c                   exsr      sr_CloseCursor
038500061220      *
038600061220     c                   ENDSR
038700061220      *
038800180307      *----------------------------------------------------------------
038900180307      *? Preparazione stringa SQL per file TITAS00F+10F                 ?
039000180307      *----------------------------------------------------------------
039100180307     c     sr_PrepSQL_1  BEGSR
039200061220      *
039300061220     c                   clear                   wSQL
039400061220      *
039500061220      *? E S E M P I O : ?
039600061220      * select *
039700061220      * from   TITAS10F
039800061220      * where (TASaas*10000+TASmgs) >= 20050810
039900061220      *   and (TASrsd like '%TIZIO%'
040000061220      *    or  TASrsd like '%T I Z I O %'
040100061220      *    or  TASrsd like '%CAIO%'
040200061220      *    or  TASrsd like '%C A I O%')
040300061220      * UNION
040400061220      * select *
040500061220      * from   TITAS00F
040600061220      * where (TASaas*10000+TASmgs) >= 20050810
040700061220      *   and (TASrsd like '%TIZIO%'
040800061220      *    or  TASrsd like '%T I Z I O %'
040900061220      *    or  TASrsd like '%CAIO%'
041000061220      *    or  TASrsd like '%C A I O%')
041100061220      *
041200180307      * ?Impostazione estrazione dati da TITAS30C?
041300061220      *
041400061220      *
041500061220      * - select TITAS10F
041600180307     c                   eval      wSQL    =  %trimr($Sql_TITAS(1))
041700061220     c                                     +  %trimr($Sql_TITAS(3))
041800061220     c                                     +  %trimr($Sql_TITAS(5))
041900180309if  1c                   if        SB19dal <> *zero  or
042000180309     c                             SB19al  <> *zero
042100180309     c                   eval      wSQL    =  wSQL
042200061220     c                                     +  %trimr($Sql_TITAS(6))
042300061220     c                                     +  ' '
042400061220     c                                     +  %char(SB19dal)
042500061220     c                                     +  ' and '
042600061220     c                                     +  %char(SB19al)
042700180309e   1c                   endif
042800180307sel 1c                   select
042900180307      * - - per ragione sociale / indirizzo / località
043000180307w   1c                   when           SB19ric = 'D'
043100061220     c                             or   SB19ric = 'E'
043200180307     c                   exsr      sr_SelTASrsd
043300180307     c                   exsr      sr_SelTASind
043400180307     c                   exsr      sr_SelTASlod
043500180307     c                   exsr      sr_SelTASprd
043600180307      * - - per codice
043700180307w   1c                   when           SB19ric = 'K'
043800180307     c                   exsr      sr_SelTAScod
043900180307      * - - per spedizione
044000180307w   1c                   when           SB19ric = 'S'
044100180307     c                   exsr      sr_SelTASnsp
044200180307      * - - per padroncino
044300180307w   1c                   when           SB19ric = 'P'
044400180307     c                   exsr      sr_SelTASpdr
044500180307e   1c                   endsl
044600061220      * - union
044700061220     c                   eval      wSQL    =  wSQL + C_Union
044800061220      * - select TITAS00F
044900061220     c                   eval      wSQL    =  wSQL
045000061220     c                                     +  %trimr($Sql_TITAS(1))
045100061220     c                                     +  %trimr($Sql_TITAS(4))
045200061220     c                                     +  %trimr($Sql_TITAS(5))
045300180309if  1c                   if        SB19dal <> *zero  or
045400180309     c                             SB19al  <> *zero
045500180309     c                   eval      wSQL    =  wSQL
045600061220     c                                     +  %trimr($Sql_TITAS(6))
045700061220     c                                     +  ' '
045800061220     c                                     +  %char(SB19dal)
045900061220     c                                     +  ' and '
046000061220     c                                     +  %char(SB19al)
046100180309e   1c                   endif
046200180307sel 1c                   select
046300180307      * - - per ragione sociale / indirizzo / località
046400180307w   1c                   when           SB19ric = 'D'
046500180307     c                             or   SB19ric = 'E'
046600180307     c                   exsr      sr_SelTASrsd
046700180307     c                   exsr      sr_SelTASind
046800180307     c                   exsr      sr_SelTASlod
046900180307     c                   exsr      sr_SelTASprd
047000180307      * - - per codice
047100180307w   1c                   when           SB19ric = 'K'
047200180307     c                   exsr      sr_SelTAScod
047300180307      * - - per spedizione
047400180307w   1c                   when           SB19ric = 'S'
047500180307     c                   exsr      sr_SelTASnsp
047600180307      * - - per padroncino
047700180307w   1c                   when           SB19ric = 'P'
047800180307     c                   exsr      sr_SelTASpdr
047900180307e   1c                   endsl
048000061220      *
048100180307      * ?for fetch only?
048200061220     c                   eval      wSQL    =  wSQL + C_ForFetch
048300070702      *
048400070704     c                   clear                   $Temp
048500070702     c                   movea     wSQL          $Temp
048600061220      *
048700061220     c                   ENDSR
048800061220      *
048900180307      *----------------------------------------------------------------
049000180307      *? Aggiunta selezione rec. TITAS per ragione sociale              ?
049100180307      *----------------------------------------------------------------
049200180307     c     sr_SelTASrsd  BEGSR
049300061220      *
049400061220     c                   eval      xx      =  1
049500061220     c                   clear                   yy
049600061220      * ciclo di impostazione selezioni
049700061220do  1c                   DOW       xx      <= %elem(SB19rs)
049800061220      *
049900061220if  2c                   if        SB19rs(xx) <> *blanks
050000061220     c                   add       1             yy
050100061220if  3c                   if        yy      =  1
050200061220     c                   eval      wSQL    =  wSQL
050300061220     c                                     +  %trimr($Sql_TITAS(7))
050400070629     c                                     +  %trimr(SB19rs(xx))
050500070629     c                                     +  '%'''
050600061220x   3c                   else
050700061220     c                   eval      wSQL    =  wSQL
050800061220     c                                     +  %trimr($Sql_TITAS(8))
050900070629     c                                     +  %trimr(SB19rs(xx))
051000070629     c                                     +  '%'''
051100061220e   3c                   endif
051200061220e   2c                   endif
051300061220      *
051400061220     c                   add       1             xx
051500061220e   1c                   ENDDO
051600061220      * chiusura selezioni (se impostata almeno una)
051700061220if  1c                   if        yy      >  *zeros
051800061220     c                   eval      wSQL    =  wSQL + ')'
051900061220e   1c                   endif
052000061220      *
052100061220     c                   ENDSR
052200061220      *
052300180307      *----------------------------------------------------------------
052400180307      *? Aggiunta selezione rec. TITAS per indirizzo                    ?
052500180307      *----------------------------------------------------------------
052600180307     c     sr_SelTASind  BEGSR
052700061220      *
052800061220     c                   eval      xx      =  1
052900061220     c                   clear                   yy
053000061220      * ciclo di impostazione selezioni
053100061220do  1c                   DOW       xx      <= %elem(SB19in)
053200061220      *
053300061220if  2c                   if        SB19in(xx) <> *blanks
053400061220     c                   add       1             yy
053500061220if  3c                   if        yy      =  1
053600061220     c                   eval      wSQL    =  wSQL
053700061220     c                                     +  %trimr($Sql_TITAS(9))
053800070629     c                                     +  %trimr(SB19in(xx))
053900070629     c                                     +  '%'''
054000061220x   3c                   else
054100061222     c                   eval      wSQL    =  wSQL
054200061222     c                                     +  %trimr($Sql_TITAS(10))
054300070629     c                                     +  %trimr(SB19in(xx))
054400070629     c                                     +  '%'''
054500061220e   3c                   endif
054600061220e   2c                   endif
054700061220      *
054800061220     c                   add       1             xx
054900061220e   1c                   ENDDO
055000061220      * chiusura selezioni (se impostata almeno una)
055100061220if  1c                   if        yy      >  *zeros
055200061222     c                   eval      wSQL    =  wSQL + ')'
055300061220e   1c                   endif
055400061220      *
055500061220     c                   ENDSR
055600070629      *
055700180307      *----------------------------------------------------------------
055800180307      *? Aggiunta selezione rec. TITAS per località                     ?
055900180307      *----------------------------------------------------------------
056000180307     c     sr_SelTASlod  BEGSR
056100070629      *
056200070629     c                   eval      xx      =  1
056300070629     c                   clear                   yy
056400070629      * ciclo di impostazione selezioni
056500070629do  1c                   DOW       xx      <= %elem(SB19lo)
056600070629      *
056700070629if  2c                   if        SB19lo(xx) <> *blanks
056800070629     c                   add       1             yy
056900070629if  3c                   if        yy      =  1
057000070629     c                   eval      wSQL    =  wSQL
057100070629     c                                     +  %trimr($Sql_TITAS(11))
057200070629     c                                     +  %trimr(SB19lo(xx))
057300070629     c                                     +  '%'''
057400070629x   3c                   else
057500070629     c                   eval      wSQL    =  wSQL
057600070629     c                                     +  %trimr($Sql_TITAS(12))
057700070629     c                                     +  %trimr(SB19lo(xx))
057800070629     c                                     +  '%'''
057900070629e   3c                   endif
058000070629e   2c                   endif
058100070629      *
058200070629     c                   add       1             xx
058300070629e   1c                   ENDDO
058400070629      * chiusura selezioni (se impostata almeno una)
058500070629if  1c                   if        yy      >  *zeros
058600070629     c                   eval      wSQL    =  wSQL + ')'
058700070629e   1c                   endif
058800070629      *
058900070629     c                   ENDSR
059000061220      *
059100180307      *----------------------------------------------------------------
059200180307      *? Aggiunta selezione rec. TITAS per provincia                    ?
059300180307      *----------------------------------------------------------------
059400180307     c     sr_SelTASprd  BEGSR
059500061220      *
059600061220     c                   eval      xx      =  1
059700061220     c                   clear                   yy
059800061220      * ciclo di impostazione selezioni
059900061220do  1c                   DOW       xx      <= %elem(SB19pr)
060000061220      *
060100061220if  2c                   if        SB19pr(xx) <> *blanks
060200061220     c                   add       1             yy
060300061220if  3c                   if        yy      =  1
060400061220     c                   eval      wSQL    =  wSQL
060500070629     c                                     +  %trimr($Sql_TITAS(13))
060600061220     c                                     +  SB19pr(xx) + ''''
060700061220x   3c                   else
060800061222     c                   eval      wSQL    =  wSQL       + ', '''
060900061220     c                                     +  SB19pr(xx) + ''''
061000061220e   3c                   endif
061100061220e   2c                   endif
061200061220      *
061300061220     c                   add       1             xx
061400061220e   1c                   ENDDO
061500061220      * chiusura selezioni (se impostata almeno una)
061600061220if  1c                   if        yy      >  *zeros
061700061220     c                   eval      wSQL    =  wSQL + ')'
061800061220e   1c                   endif
061900061220      *
062000061220     c                   ENDSR
062100061220      *
062200180307      *----------------------------------------------------------------
062300180307      *? Aggiunta selezione rec. TITAS per codice cliente               ?
062400180307      *----------------------------------------------------------------
062500180307     c     sr_SelTAScod  BEGSR
062600061220      *
062700180307      * - ?Per TASKSC:?
062800061220     c                   eval      xx      =  1
062900061220     c                   clear                   yy
063000061220      * ciclo di impostazione selezioni
063100061220do  1c                   DOW       xx      <= %elem(SB19rs)
063200061220      *
063300061220if  2c                   if        SB19rs(xx) <> *blanks
063400061220     c                   add       1             yy
063500061220if  3c                   if        yy      =  1
063600061220     c                   eval      wSQL    =  wSQL
063700070629     c                                     +  %trimr($Sql_TITAS(14))
063800061220     c                                     +  %trimr(SB19rs(xx))
063900061220x   3c                   else
064000061220     c                   eval      wSQL    =  wSQL + ', '
064100061220     c                                     +  %trimr(SB19rs(xx))
064200061220e   3c                   endif
064300061220e   2c                   endif
064400061220      *
064500061220     c                   add       1             xx
064600061220e   1c                   ENDDO
064700061220      * chiusura selezioni (se impostata almeno una)
064800061220if  1c                   if        yy      >  *zeros
064900061220     c                   eval      wSQL    =  wSQL + ')'
065000061220e   1c                   endif
065100061220      *
065200180307      * - ?Per TASCCM:?
065300061220     c                   eval      xx      =  1
065400061220     c                   clear                   yy
065500061220      * ciclo di impostazione selezioni
065600061220do  1c                   DOW       xx      <= %elem(SB19rs)
065700061220      *
065800061220if  2c                   if        SB19rs(xx) <> *blanks
065900061220     c                   add       1             yy
066000061220if  3c                   if        yy      =  1
066100061220     c                   eval      wSQL    =  wSQL
066200070629     c                                     +  %trimr($Sql_TITAS(15))
066300061220     c                                     +  %trimr(SB19rs(xx))
066400061220x   3c                   else
066500061220     c                   eval      wSQL    =  wSQL + ', '
066600061220     c                                     +  %trimr(SB19rs(xx))
066700061220e   3c                   endif
066800061220e   2c                   endif
066900061220      *
067000061220     c                   add       1             xx
067100061220e   1c                   ENDDO
067200061220      * chiusura selezioni (se impostata almeno una)
067300061220if  1c                   if        yy      >  *zeros
067400061220     c                   eval      wSQL    =  wSQL + '))'
067500061220e   1c                   endif
067600061220      *
067700061220     c                   ENDSR
067800180307
067900180307       //--------------------------------------------------------------
068000180307       //? Aggiunta selezione rec. TITAS per spedizione.                ?
068100180307       //--------------------------------------------------------------
068200180307       BEGSR  sr_SelTASnsp;
068300180307
068400180307         xx = 1;
068500180307         clear  yy;
068600180307
068700180307         // - ciclo di impostazione selezioni
068800180307         DOW  xx <= %elem( SB19rs );
068900180307
069000180307           if  SB19rs(xx) <> *blank;
069100180307             yy += 1;
069200180307             if  yy = 1;
069300180308               wSQL += ' and digits(TASaas) concat +
069400180308                             digits(TASlnp) concat +
069500180308                             digits(TASnrs) concat +
069600180308                             digits(TASnsp) in ('  +
069700180307                       %trimR( SB19rs(xx) );
069800180307             else;
069900180307               wSQL += ', '  + %trim( SB19rs(xx) );
070000180307             endif;
070100180307           endif;
070200180307
070300180307           xx += 1;
070400180307
070500180307         ENDDO;
070600180307
070700180307         // - chiusura selezioni (se impostata almeno una)
070800180307         if  yy > *zero;
070900180307           wSQL += ')';
071000180307         endif;
071100180307
071200180307       ENDSR;
071300180307
071400180307       //--------------------------------------------------------------
071500180307       //? Aggiunta selezione rec. TITAS per padroncino.                ?
071600180307       //--------------------------------------------------------------
071700180307       BEGSR  sr_SelTASpdr;
071800180307
071900180308         // - ?Non ha senso selezionare per TASPDR (ritiro)?
072000180308         //   ?in quanto sul TITASS0F  NON  c'è!?
072100180308         //*·// - ?Per TASPDR (ritiro)?
072200180308         //*·xx = 1;
072300180308         //*·clear  yy;
072400180308         //*·
072500180308         //*·// - ciclo di impostazione selezioni
072600180308         //*·DOW  xx <= %elem( SB19rs );
072700180308         //*·
072800180308         //*·  if  SB19rs(xx) <> *blank;
072900180308         //*·    yy += 1;
073000180308         //*·    if  yy = 1;
073100180308         //*·      wSQL += ' and ( TASpdr in (' +
073200180308         //*·              %trimR( SB19rs(xx) );
073300180308         //*·    else;
073400180308         //*·      wSQL += ', '  + %trim( SB19rs(xx) );
073500180308         //*·    endif;
073600180308         //*·  endif;
073700180308         //*·
073800180308         //*·  xx += 1;
073900180308         //*·
074000180308         //*·ENDDO;
074100180308         //*·
074200180308         //*·// - chiusura selezioni (se impostata almeno una)
074300180308         //*·if  yy > *zero;
074400180308         //*·  wSQL += ')';
074500180308         //*·endif;
074600180307
074700180307         // - ?Per TASPDC (consegna)?
074800180307         xx = 1;
074900180307         clear  yy;
075000180307
075100180307         // - ciclo di impostazione selezioni
075200180307         DOW  xx <= %elem( SB19rs );
075300180307
075400180307           if  SB19rs(xx) <> *blank;
075500180307             yy += 1;
075600180307             if  yy = 1;
075700180308               //*·wSQL += ' or TASpdc in (' +    ?(parzializzando?
075800180308               //*·        %trimR( SB19rs(xx) );  ?ANCHE per TASPDR)?
075900180308               wSQL += ' and TASpdc in (' +
076000180308                       %trimR( SB19rs(xx) );
076100180307             else;
076200180307               wSQL += ', '  + %trim( SB19rs(xx) );
076300180307             endif;
076400180307           endif;
076500180307
076600180307           xx += 1;
076700180307
076800180307         ENDDO;
076900180307
077000180307         // - chiusura selezioni (se impostata almeno una)
077100180307         if  yy > *zero;
077200180308           //*·wSQL += '))';  ?(parzializzando ANCHE per TASPDR)?
077300180308           wSQL += ')';
077400180307         endif;
077500180307
077600180307       ENDSR;
077700180307
077800180307      *----------------------------------------------------------------
077900180307      *? Estrazione dati dal file TITAA00F                              ?
078000180307      *----------------------------------------------------------------
078100061220     c     sr_TITAA      BEGSR
078200061220      *
078300061220      * Preparazione stringa SQL
078400180307     c                   exsr      sr_PrepSQL_2
078500090313     c                   reset                   $EoF
078600061220      *
078700061220      * Apertura cursore
078800180307     c                   exsr      sr_OpenCursor
078900061220      *
079000061220      * Elaborazione
079100120613     c                   dou       $EoF    =  *on
079200180307     c                   exsr      sr_ReadCursor
079300061220     c                   enddo
079400061220      *
079500061220      * Chiusura cursore
079600180307     c                   exsr      sr_CloseCursor
079700061220      *
079800061220     c                   ENDSR
079900061220      *
080000180307      *----------------------------------------------------------------
080100180307      *? Preparazione stringa SQL per file TITAA00F                     ?
080200180307      *----------------------------------------------------------------
080300180307     c     sr_PrepSQL_2  BEGSR
080400061220      *
080500061220     c                   clear                   wSQL
080600061220      *
080700061220      *? E S E M P I O : ?
080800061220      * select *
080900061220      * from   TITAA00F
081000061220      * where  TAAtrc  =   'M'   and    TAAaas >= 2005
081100061220      *   and (TAArsc like '%TIZIO%'
081200061220      *    or  TAArsc like '%T I Z I O %'
081300061220      *    or  TAArsc like '%CAIO%'
081400061220      *    or  TAArsc like '%C A I O%')
081500061220      *
081600180307      * ?Impostazione estrazione dati da TITAA00F?
081700061220      *
081800061220      * - select TITAA00F
081900180307     c                   eval      wSQL    =  %trimr($Sql_TITAA(1))
082000061220     c                                     +  %trimr($Sql_TITAA(3))
082100061220     c                                     +  %trimr($Sql_TITAA(4))
082200061220     c                                     +  %trimr($Sql_TITAA(5))
082300061220     c                                     +  ' '
082400061220     c                                     +  %char(wAAI)
082500061220     c                                     +  ' and '
082600061220     c                                     +  %char(wAAF)
082700180307     c                   exsr      sr_SelTAArsc
082800180307     c                   exsr      sr_SelTAAind
082900180307     c                   exsr      sr_SelTAAloc
083000180307     c                   exsr      sr_SelTAAprv
083100061220      *
083200180307      * ?for fetch only?
083300061220     c                   eval      wSQL    =  wSQL + C_ForFetch
083400070702      *
083500070704     c                   clear                   $Temp
083600070702     c                   movea     wSQL          $Temp
083700061220      *
083800061220     c                   ENDSR
083900061220      *
084000180307      *----------------------------------------------------------------
084100180307      *? Aggiunta selezione rec. TITAA per ragione sociale              ?
084200180307      *----------------------------------------------------------------
084300180307     c     sr_SelTAArsc  BEGSR
084400061220      *
084500061220     c                   eval      xx      =  1
084600061220     c                   clear                   yy
084700061220      * ciclo di impostazione selezioni
084800061220do  1c                   DOW       xx      <= %elem(SB19rs)
084900061220      *
085000061220if  2c                   if        SB19rs(xx) <> *blanks
085100061220     c                   add       1             yy
085200061220if  3c                   if        yy      =  1
085300061220     c                   eval      wSQL    =  wSQL
085400061220     c                                     +  %trimr($Sql_TITAA(6))
085500070629     c                                     +  %trimr(SB19rs(xx))
085600070629     c                                     +  '%'''
085700061220x   3c                   else
085800061220     c                   eval      wSQL    =  wSQL
085900061220     c                                     +  %trimr($Sql_TITAA(7))
086000070629     c                                     +  %trimr(SB19rs(xx))
086100070629     c                                     +  '%'''
086200061220e   3c                   endif
086300061220e   2c                   endif
086400061220      *
086500061220     c                   add       1             xx
086600061220e   1c                   ENDDO
086700061220      * chiusura selezioni (se impostata almeno una)
086800061220if  1c                   if        yy      >  *zeros
086900061220     c                   eval      wSQL    =  wSQL + ')'
087000061220e   1c                   endif
087100061220      *
087200061220     c                   ENDSR
087300061220      *
087400180307      *----------------------------------------------------------------
087500180307      *? Aggiunta selezione rec. TITAA per indirizzo                    ?
087600180307      *----------------------------------------------------------------
087700180307     c     sr_SelTAAind  BEGSR
087800061220      *
087900061220     c                   eval      xx      =  1
088000061220     c                   clear                   yy
088100061220      * ciclo di impostazione selezioni
088200061220do  1c                   DOW       xx      <= %elem(SB19in)
088300061220      *
088400061220if  2c                   if        SB19in(xx) <> *blanks
088500061220     c                   add       1             yy
088600061220if  3c                   if        yy      =  1
088700061220     c                   eval      wSQL    =  wSQL
088800061220     c                                     +  %trimr($Sql_TITAA(8))
088900070629     c                                     +  %trimr(SB19in(xx))
089000070629     c                                     +  '%'''
089100061220x   3c                   else
089200061222     c                   eval      wSQL    =  wSQL
089300061220     c                                     +  %trimr($Sql_TITAA(9))
089400070629     c                                     +  %trimr(SB19in(xx))
089500070629     c                                     +  '%'''
089600061220e   3c                   endif
089700061220e   2c                   endif
089800061220      *
089900061220     c                   add       1             xx
090000061220e   1c                   ENDDO
090100061220      * chiusura selezioni (se impostata almeno una)
090200061220if  1c                   if        yy      >  *zeros
090300061220     c                   eval      wSQL    =  wSQL + ')'
090400061220e   1c                   endif
090500061220      *
090600061220     c                   ENDSR
090700070629      *
090800180307      *----------------------------------------------------------------
090900180307      *? Aggiunta selezione rec. TITAA per località                     ?
091000180307      *----------------------------------------------------------------
091100180307     c     sr_SelTAAloc  BEGSR
091200070629      *
091300070629     c                   eval      xx      =  1
091400070629     c                   clear                   yy
091500070629      * ciclo di impostazione selezioni
091600070629do  1c                   DOW       xx      <= %elem(SB19lo)
091700070629      *
091800070629if  2c                   if        SB19lo(xx) <> *blanks
091900070629     c                   add       1             yy
092000070629if  3c                   if        yy      =  1
092100070629     c                   eval      wSQL    =  wSQL
092200070629     c                                     +  %trimr($Sql_TITAA(10))
092300070629     c                                     +  %trimr(SB19lo(xx))
092400070629     c                                     +  '%'''
092500070629x   3c                   else
092600070629     c                   eval      wSQL    =  wSQL
092700070629     c                                     +  %trimr($Sql_TITAA(11))
092800070629     c                                     +  %trimr(SB19lo(xx))
092900070629     c                                     +  '%'''
093000070629e   3c                   endif
093100070629e   2c                   endif
093200070629      *
093300070629     c                   add       1             xx
093400070629e   1c                   ENDDO
093500070629      * chiusura selezioni (se impostata almeno una)
093600070629if  1c                   if        yy      >  *zeros
093700070629     c                   eval      wSQL    =  wSQL + ')'
093800070629e   1c                   endif
093900070629      *
094000070629     c                   ENDSR
094100061220      *
094200180307      *----------------------------------------------------------------
094300180307      *? Aggiunta selezione rec. TITAA per provincia                    ?
094400180307      *----------------------------------------------------------------
094500180307     c     sr_SelTAAprv  BEGSR
094600061220      *
094700061220     c                   eval      xx      =  1
094800061220     c                   clear                   yy
094900061220      * ciclo di impostazione selezioni
095000061220do  1c                   DOW       xx      <= %elem(SB19pr)
095100061220      *
095200061220if  2c                   if        SB19pr(xx) <> *blanks
095300061220     c                   add       1             yy
095400061220if  3c                   if        yy      =  1
095500061220     c                   eval      wSQL    =  wSQL
095600070629     c                                     +  %trimr($Sql_TITAA(12))
095700061220     c                                     +  SB19pr(xx) + ''''
095800061220x   3c                   else
095900061220     c                   eval      wSQL    =  wSQL + ', '''
096000061220     c                                     +  SB19pr(xx) + ''''
096100061220e   3c                   endif
096200061220e   2c                   endif
096300061220      *
096400061220     c                   add       1             xx
096500061220e   1c                   ENDDO
096600061220      * chiusura selezioni (se impostata almeno una)
096700061220if  1c                   if        yy      >  *zeros
096800061220     c                   eval      wSQL    =  wSQL + ')'
096900061220e   1c                   endif
097000061220      *
097100061220     c                   ENDSR
097200061220      *
097300180307      *----------------------------------------------------------------
097400180307      *? Dichiarazione e Apertura cursore                               ?
097500180307      *----------------------------------------------------------------
097600180307     c     sr_OpenCursor BEGSR
097700061220      *
097800061220      * Dichiarazione cursore
097900061220      *
098000061220     c/exec sql
098100061220     c+     PREPARE S1 FROM :wSQL
098200061220     c/end-exec
098300061220     c*
098400061220     c/exec sql
098500061220     c+     DECLARE C1 CURSOR FOR S1
098600061220     c/end-exec
098700061220      *
098800061220      * Apertura del cursore
098900061220      *
099000061220     c/exec sql
099100061220     c+     OPEN C1
099200061220     c/end-exec
099300090313      *
099400090313w   1c                   if        SQLcode <  *zeros
099500090313     c                   eval      $Err    =  *on
099600120613     c                   eval      $EoF    =  *on
099700180307     c                   exsr      sr_PrintErr
099800090313     c                   endif
099900061220      *
100000061220     c                   ENDSR
100100061220      *
100200180307      *----------------------------------------------------------------
100300180307      *? Chiusura cursore                                               ?
100400180307      *----------------------------------------------------------------
100500180307     c     sr_CloseCursorBEGSR
100600061220      *
100700061220     c/exec sql
100800061220     c+     CLOSE C1
100900061220     c/end-exec
101000061220      *
101100061220     c                   ENDSR
101200050809      *
101300180307      *----------------------------------------------------------------
101400180307      *? Lettura cursore                                                ?
101500180307      *----------------------------------------------------------------
101600180307     c     sr_ReadCursor BEGSR
101700050809      *
101800061220sel 1c                   select
101900061220      *
102000180307      * - Lettura file TITAS00F+10F
102100061220w   1c                   when      wFile   =  2
102200061220     c                   clear                   TITASds
102300061220     c/exec sql
102400061220     c+     FETCH NEXT FROM C1 INTO :TITASds
102500061220     c/end-exec
102600061220      *
102700180307      * - Lettura file TITAA00F
102800061220w   1c                   when      wFile   =  3
102900061220     c                   clear                   TITAAds
103000061220     c/exec sql
103100061220     c+     FETCH NEXT FROM C1 INTO :TITAAds
103200061220     c/end-exec
103300061220e   1c                   endsl
103400050809      *
103500050809sel 1c                   select
103600090313w   1c                   when      SQLcode =  100
103700050809     c                   eval      $EoF    =  *on
103800090313w   1c                   when      SQLcode <  *zeros
103900050809     c                   eval      $Err    =  *on
104000120613     c                   eval      $EoF    =  *on
104100180307     c                   exsr      sr_PrintErr
104200050809x   1c                   other
104300180307     c                   exsr      sr_ElabRec
104400050809e   1c                   endsl
104500050809      *
104600050809     c                   ENDSR
104700070703      *
104800180307      *----------------------------------------------------------------
104900180307      *? Stampa segnalazione dell'errore rilevato via SQL               ?
105000180307      *----------------------------------------------------------------
105100180307     c     sr_PrintErr   BEGSR
105200090313      *
105300090313      /free
105400090313
105500090313         // Stampa Dump
105600090313         Dump(A);
105700090313
105800090313         // Stampa Job-Log
105900090313         Qcmd = 'DSPJOBLOG job(*) output(*print)';
106000090313         callp  qCmdExc(Qcmd : %size(Qcmd));
106100090313
106200090313      /end-free
106300070703      *
106400070704      * Stampa errore in QSYSPRT
106500070703sel 1c                   except    PRTtxt
106600070703     c                   eval      *inOF   =  *off
106700070703      *
106800070703     c                   except    PRTerr
106900070704      *
107000070704      * Registra segnalazione di errore in un record fittizio del WrkF
107100180307     c                   clear                   WFNIB000
107200180307     c                   eval      WNBrsm =  'RILEVATO ERRORE SQL'
107300180307     c                   eval      WNBinm =  'Consegnare al CED il +
107400180307     c                                        relativo spool'
107500180307     c                   eval      WNBcam =  '"QSYSPRT"'
107600180307     c                   eval      WNBlom =  'con dati utente +
107700180307     c                                        "TNSB19*ERR".'
107800180307     c                   eval      WNBprm =  *all'*'
107900180307     c                   eval      WNBnzm =  *all'*'
108000120615      /free
108100180307         WNBrsd = *all'*';
108200180307         WNBind = *all'*';
108300180307         WNBcad = *all'*';
108400180307         WNBlod = *all'*';
108500180307         WNBprd = *all'*';
108600180307         WNBnzd = *all'*';
108700120615      /end-free
108800070704      *
108900180307     c                   exsr      sr_WriteRec
109000180308      *
109100180308     c                   exsr      sr_EndSubr
109200070703      *
109300070703     c                   ENDSR
109400120613      /free
109500120613       //--------------------------------------------------------------
109600180307       //? Stampa segnalazione del mancato reperimento dei parametri   ?
109700120613       //--------------------------------------------------------------
109800180307       BEGSR  sr_PrtNoParms;
109900120613
110000180307         // - ?Stampa errore in QSYSPRT?
110100120613         except  PRTtxt;
110200120613         *inOF = *off;
110300120613
110400120613         except  PRTprm;
110500180307
110600180307         $Err = *on;
110700120613
110800180307         // - ?Registra segnalazione di errore in un record fittizio del?
110900180307         //   ?WrkF?
111000180307         clear  WFNIB000;
111100180307         WNBrsm = 'NON REPERITI I PARAMETRI DI LANCIO';
111200180307         WNBinm = 'RICERCA INTERROTTA.';
111300180307         WNBcam = *all'*';
111400180307         WNBlom = *all'*';
111500180307         WNBprm = *all'*';
111600180307         WNBnzm = *all'*';
111700180307         WNBrsd = *all'*';
111800180307         WNBind = *all'*';
111900180307         WNBcad = *all'*';
112000180307         WNBlod = *all'*';
112100180307         WNBprd = *all'*';
112200180307         WNBnzd = *all'*';
112300120613
112400180307         exsr  sr_WriteRec;
112500120613
112600180307         // - ?Imposta uscita dal *pgm?
112700120613         $EoF = *on;
112800180307         exsr  sr_EndSubr;
112900120613
113000120613       ENDSR;
113100120613
113200120613       //--------------------------------------------------------------
113300180307       //? Stampa segnalazione dei troppi record già registrati nel WF ?
113400120613       //--------------------------------------------------------------
113500180307       BEGSR  sr_PrtTooMuchRec;
113600120613
113700180307         // - ?Stampa errore in QSYSPRT?
113800120613         except  PRTtxt;
113900120613         *inOF = *off;
114000120613
114100120613         except  PRTmax;
114200180307
114300180307         $Err = *on;
114400120613
114500180307         // - ?Registra segnalazione di errore in un record fittizio del?
114600180307         //   ?WrkF?
114700180307         clear  WFNIB000;
114800180307         WNBrsm = 'GIÀ REGISTRATI TROPPI RECORD.';
114900180307         WNBinm = 'RICERCA INTERROTTA.';
115000180307         WNBcam = *all'*';
115100180307         WNBlom = *all'*';
115200180307         WNBprm = *all'*';
115300180307         WNBnzm = *all'*';
115400180307         WNBrsd = *all'*';
115500180307         WNBind = *all'*';
115600180307         WNBcad = *all'*';
115700180307         WNBlod = *all'*';
115800180307         WNBprd = *all'*';
115900180307         WNBnzd = *all'*';
116000120613
116100180307         exsr  sr_WriteRec;
116200120613
116300180307         // - ?Imposta uscita dal *pgm?
116400120613         $EoF = *on;
116500180307         exsr  sr_EndSubr;
116600120613
116700120613       ENDSR;
116800120613
116900120613      /end-free
117000050809      *
117100180307      *----------------------------------------------------------------
117200180307      *? Elaborazione singolo record da TITAS00F+10F/TITAA00F           ?
117300180307      *----------------------------------------------------------------
117400180307     c     sr_ElabRec    BEGSR
117500050809      *
117600061220      * Aggancio record x dati mancanti
117700061220sel 1c                   SELECT
117800120613      *
117900120613      * => Segnalazione immediata di errore: TROPPI REC.
118000160908      *    (SE utente NON EDP*)
118100160908w   1c*//*               WHEN      wCountRec >= c_Max_Rec
118200180307           WHEN  %subst( KNMUS : 1 : 3 ) <> 'EDP'  and
118300180307                 wCountRec               >= c_Max_Rec;
118400180307             exsr  sr_PrtTooMuchRec;
118500061220      *
118600180307      * => TITAA00F (se reperito TITAS00F+10F via sql)
118700061220      *    per reperire i dati del mittente della spedizione in TITAS
118800061220      *    (per spedizioni da TITAA sono già stati reperiti, appunto,
118900061220      *     da TITAA)
119000061220w   1c                   WHEN      wFile   =  2
119100180307     c                   exsr      sr_DecodMitt
119200180307     c                   exsr      sr_RieWFNIB
119300061220      *
119400180307      * => TITAS00F+10F (se reperito TITAA00F via sql)
119500061220w   1c                   WHEN      wFile   =  3
119600061220     c     K04TAS30      chain     TITAS30C
119700061220if  2c                   if        NOT  %found(TITAS30C)
119800061220     c***                clear                   TITASds
119900061220     c                   leavesr
120000061220e   2c                   endif
120100061220      * -  Selezione delle spedizioni con segnacollo
120200061220if  2c                   if             TASfns <> 'S'
120300061220     c                             and  TASncd =  *zeros
120400061220     c                   leavesr
120500061220e   2c                   endif
120600061220      * -  Selezione per data spedizione (non reperibile da TITAA)
120700061220if  2c                   if             TASaas*10000+TASmgs < SB19dal
120800061220     c                             or   TASaas*10000+TASmgs > SB19al
120900061220     c                   leavesr
121000061220e   2c                   endif
121100180307     c                   exsr      sr_RieWFNIB
121200061220      *
121300061220e   1c                   ENDSL
121400050902      *
121500050902      * Scrittura record nel Work-File
121600180307     c                   exsr      sr_WriteRec
121700050809      *
121800050809     c                   ENDSR
121900050901      *
122000180307      *----------------------------------------------------------------
122100180307      *? Decodifica mittente (se dati da TITAS)                         ?
122200180307      *----------------------------------------------------------------
122300180307     c     sr_DecodMitt  BEGSR
122400050901      *
122500050901     c                   move      TASccm        w0040
122600050901      *
122700061220if  1c                   if             w0040 = 9999
122800061220     c                             or   w0040 = 8888
122900061220     c                             or   w0040 = *zeros
123000050901      *
123100061220     c                   eval      TAAtrc  =  'M'
123200050901     c     K05TAA30      chain     TITAA30C
123300061220if  2c                   if        NOT  %found(TITAA30C)
123400050901     c                   clear                   TITAAds
123500050901e   2c                   endif
123600050901      *
123700050901x   1c                   else
123800050901      *
123900070216if  2c                   if             TASccm <> INDksc
124000070216     c                             or   TASccm <> ACOksc
124100070216     c                   clear                   TIBS69ds
124200070216     c                   clear                   ds_CNACO
124300070216     c                   clear                   ds_CNIND
124400050901     c                   z-add     TASccm        I69kac
124500050901     c                   z-add     TASccm        I69kin
124600050901     c                   call      'TIBS69R'
124700050901     c                   parm                    TIBS69ds
124800050901     c                   parm                    ds_CNACO
124900050901     c                   parm                    ds_CNIND
125000050901     c                   parm                    ds_CNCLP
125100050901     c                   parm                    ds_FNCLS
125200070216e   2c                   endif
125300070216if  2c***                if        O69err  <> *on
125400050901     c                   movel     ACOrag        TAArsc
125500050901     c                   movel     INDvia        TAAind
125600050902     c                   movel     INDcap        TAAcap
125700050901     c                   movel     INDcit        TAAloc
125800050902     c                   movel     INDprv        TAAprv
125900050902     c                   movel     INDsta        TAAnaz
126000070216e   2c***                endif
126100050901      *
126200050901e   1c                   endif
126300050901      *
126400050901     c                   ENDSR
126500061220      *
126600180307      *----------------------------------------------------------------
126700180307      *? Impostazione campi del wrkf WFNIB00F                           ?
126800180307      *----------------------------------------------------------------
126900180307     c     sr_RieWFNIB   BEGSR
127000061220      *
127100180307     c                   clear                   WFNIB000
127200180307      *
127300180307     c*//                eval      sTASksc =  TASksc
127400180307     c*//                eval      sTASccm =  TASccm
127500061220     c***                eval      sTASsss =
127600180307      *
127700180307      * Fil. partenza
127800180307     c                   eval      WNBlnp  =  TASlnp
127900180307      * Numero serie
128000180307     c                   eval      WNBnrs  =  TASnrs
128100180307      * Numero spedizione
128200180307     c                   eval      WNBnsp  =  TASnsp
128300180307      * Data spedizione
128400180307     c                   eval      WNBdsp  =  (TASaas*10000 + TASmgs)
128500180307      * Tipo bolla
128600180307     c                   eval      WNBtbl  =  TAStbl
128700180307      * Codice bolla
128800180307     c                   eval      WNBcbo  =  TAScbo
128900180307      * Fil. arrivo
129000180307     c                   eval      WNBlna  =  TASlna
129100180307      * Fil. i.v.a.
129200180307     c                   eval      WNBfiv  =  TASfiv
129300180307      * Numero fattura
129400180307     c                   eval      WNBnft  =  TASnft
129500180307      * Data fattura
129600180307     c                   eval      WNBdft  =  TASdft
129700180307      * Mittente: ragione sociale
129800180307     c                   eval      WNBrsm  =  TAArsc
129900180307      * Mittente: indirizzo
130000180307     c                   eval      WNBinm  =  TAAind
130100180307      * Mittente: cap
130200180307     c                   eval      WNBcam  =  TAAcap
130300180307      * Mittente: località
130400180307     c                   eval      WNBlom  =  TAAloc
130500180307      * Mittente: provincia
130600180307     c                   eval      WNBprm  =  TAAprv
130700180307      * Mittente: nazione
130800180307     c                   eval      WNBnzm  =  TAAnaz
130900180307      * Destinatario: ragione sociale
131000180307     c                   eval      WNBrsd  =  TASrsd
131100180307      * Destinatario: indirizzo
131200180307     c                   eval      WNBind  =  TASind
131300180307      * Destinatario: cap
131400180307     c                   eval      WNBcad  =  TAScad
131500180307      * Destinatario: località
131600180307     c                   eval      WNBlod  =  TASlod
131700180307      * Destinatario: provincia
131800180307     c                   eval      WNBprd  =  TASprd
131900180307      * Destinatario: nazione
132000180307     c                   eval      WNBnzd  =  TASnzd
132100180307      * Riferimento mittente numerico
132200180307     c                   eval      WNBrmn  =  TASrmn
132300180307      * Riferimento mittente alfanumerico
132400180307     c*** v.sr.AltriDati eval      WNBrma  =
132500180307      * Codice     padroncino consegna
132600180307     c                   eval      WNBpdc  =  TASpdc
132700180307      * Decodifica padroncino consegna
132800180307     c*** v.sr.AltriDati eval      WNBpdd  =
132900180307      * Numero distinta consegna
133000180307     c                   eval      WNBndc  =  TASndc
133100180307      * Data   distinta consegna
133200180307     c                   eval      WNBddc  =  TASddc
133300180307      * Flag "Merce portata a magazzino" ('S')
133400180307     c                   eval      WNBfdn  =  TASfdn
133500180307      * Flag "Fermo deposito"            ('S')
133600180307     c                   eval      WNBffd  =  TASffd
133700180307      * Importo contrassegno
133800180307     c*** v.sr.AltriDati eval      WNBcas  =
133900180307      * Divisa  contrassegno
134000180307     c*** v.sr.AltriDati eval      WNBvca  =
134100180307      * Codice partita iva europea
134200180307     c*** v.sr.AltriDati eval      WNBpiv  =
134300180307      * Codice fiscale
134400180307     c*** v.sr.AltriDati eval      WNBcdf  =
134500180307      * Numero colli
134600180307     c                   eval      WNBncl  =  TASncl
134700180307      * Peso in kg
134800180307     c                   eval      WNBpkf  =  TASpkf
134900180307      * Volume
135000180307     c                   eval      WNBvlf  =  TASvlf
135100180307      * Natura merce
135200180307     c*** v.sr.AltriDati eval      WNBnas  =  TASnas
135300180307      *
135400180307      * - Reperimento ed impostazione dei dati mancanti
135500180307     c                   exsr      sr_AltriDati
135600061220      *
135700061220     c                   ENDSR
135800061220      *
135900180307      *----------------------------------------------------------------
136000180307      *? Reperimento dati mancanti in TITAS00F+10F e TITAA00F           ?
136100180307      *----------------------------------------------------------------
136200180307     c     sr_AltriDati  BEGSR
136300061220      *
136400061220      * Decodifica padroncino
136500061220if  1c                   if        TASpdc  <> *zeros
136600061220     c     K02APD01      chain     FIAPD000
136700061220if  2c                   if        %found(FIAPD01L)
136800180307     c                   movel     APDrsc        WNBpdd
136900061220e   2c                   endif
137000061220e   1c                   endif
137100061220      *
137200061220      * Recupero c/assegno
137300061220if  1c                   if        TASfca  =  'S'
137400061220     c     K04CSB03      chain     TNCSB000
137500061220if  2c                   if        %found(TNCSB03L)
137600180307     c                   z-add     CSBcas        WNBcas
137700180307     c                   movel     CSBvca        WNBvca
137800061220e   2c                   endif
137900061220e   1c                   endif
138000061220      *
138100160122      * Riferimento mittente alfanumerico e natura merce
138200061220     c                   eval      TA4trc  =  'A'
138300061220     c     K05TA430      chain     TITA430C
138400061220if  1c                   if        %found(TITA430C)
138500061220     c                   movel     TA4not        dTA4A
138600180307     c                   movel     §TA4Arma      WNBrma
138700180307     c                   movel     §TA4Anas      WNBnas
138800061220e   1c                   endif
138900061220      *
139000061220      * Codice Fiscale
139100061220     c                   eval      TA4trc  =  'Q'
139200061220     c     K05TA430      chain     TITA430C
139300061220if  1c                   if        %found(TITA430C)
139400061220     c                   movel     TA4not        dTA4Q
139500061220      * - Verifico se Franco o Assegnato
139600061220sel 2c                   select
139700061220w   2c                   when      %subst(TAStbl:1:1) = 'F'
139800180307     c                   eval      WNBcdf  =  §TA4Qcfm
139900061220w   2c                   when      %subst(TAStbl:1:1) = 'A'
140000180307     c                   eval      WNBcdf  =  §TA4Qcfd
140100061220e   2c                   endsl
140200180307e   1c                   endif
140300061220      *
140400061220      * Partita IVA
140500061220sel 1c                   select
140600061220w   1c                   when      %subst(TAStbl:1:1) = 'A'
140700061220     c                             and  TAScpd <> *blanks
140800180307     c                   eval      WNBpiv  =  TAScpd
140900061220w   1c                   when      %subst(TAStbl:1:1) = 'F'
141000180307     c                   eval      WNBpiv  =  TAAcpi
141100180307e   1c                   endsl
141200061220      *
141300061220      * Partita IVA / Codice Fiscale
141400061220      *   SE partita iva non valorizzata
141500180307if  1c                   IF             WNBpiv  = *blanks
141600180307     c                             or   WNBcdf  = *blanks
141700061220      * - - codificato franco o assegnato
141800061220     c                   move      TASksc        w0040
141900061220if  2c                   If             w0040  <> 8888
142000061220     c                             and  w0040  <> 9999
142100061220     c                   z-add     TASksc        I69kin
142200061220     c                   call      'TIBS69R'
142300061220     c                   parm                    TIBS69ds
142400061220     c                   parm                    ds_CNACO
142500061220     c                   parm                    ds_CNIND
142600061220     c                   parm                    ds_CNCLP
142700061220     c                   parm                    ds_FNCLS
142800061220if  3c                   if        O69err  <> *on
142900180307if  4c                   if        WNBpiv  =  *blanks
143000180307     c                   eval      WNBpiv  =  INDiva
143100061220e   4c                   endif
143200180307if  4c                   if        WNBcdf  =  *blanks
143300180307     c                   eval      WNBcdf  =  INDcdf
143400061220e   4c                   endif
143500061220e   3c                   endif
143600061220e   2c                   EndIf
143700061220e   1c                   ENDIF
143800061220      *
143900061220     c                   ENDSR
144000050902      *
144100180307      *----------------------------------------------------------------
144200180307      *? Scrittura record nel Work-File WFNIB00F                        ?
144300180307      *----------------------------------------------------------------
144400180307     c     sr_WriteRec   BEGSR
144500050902      *
144600180307     c*//  (già fatto)   clear                   WFNIB000
144700180307      *
144800050902      * Campi chiave
144900061219      * - Utente esecuzione
145000061219     c                   eval      WNBkut  =  KNMUS
145100050902      * - Data esecuzione
145200050902     c     *iso          move      Data_Iso      WNBkdt
145300050902      * - Ora  esecuzione
145400050902     c                   movel     wTimeHM       WNBkhm
145500180307      * - 1ª ragione sociale / indirizzo / località / provincia
145600180307      *   o 1° codice o 1ª spedizione o 1° padroncino
145700180307     c                   movel     wrkWNBkrs     WNBkrs
145800180307      * - Tipo ricerca (M=Mittente, D=Destinatario, E=Entrambi,
145900180307      *                 K=Codice Cliente [CCM o KSC],
146000180307      *                 S=Spedizione [AAS + LNP + NRS + NSP],
146100180307      *                 P=Padrincino)
146200061219     c                   eval      WNBkri  =  SB19ric
146300180307      *
146400180307      * (gli altri campi cono già stati impostati nella subr. Rie_WFNIB)
146500050902      *
146600050902     c                   WRITE     WFNIB000
146700120613      *
146800160908     c                   if        %subst( KNMUS : 1 : 3 ) <> 'EDP'
146900120613     c                   eval      wCountRec += 1
147000160908     c                   endif
147100050902      *
147200050902     c                   ENDSR
147300070703      *
147400180307      *? O U T P U T ?--------------------------------------------------*
147500070703      *
147600070704     oQSYSPRT   e            PRTtxt            1
147700070703     o                       RSUT
147800070703     o                                        +   6 'LISTA ERRORI RILE-
147900070703     o                                              VATI IN FASE DI RI-
148000070703     o                                              CERCA NOMINATIVI I-
148100070703     o                                              N BOLLE SEDE PER G-
148200070703     o                                              .D.F.'
148300070703     o                       SDSpgm           +   6
148400070703     o                       wDate         y  +   3
148500070703     o          e            PRTtxt      1
148600070703     o                       KNSIF
148700070703     o                       KNMUS            +   1
148800070703     o                                        +   5 '------------------
148900070703     o                                              -------------------
149000070703     o                                              -------------------
149100070703     o                                              -------------------
149200070703     o                                              -----'
149300070703     o                                        +   6 'Pag.'
149400070703     o                       Page          z  +   0
149500070703     o                       wTime            +   5 '  :  :  '
149600120613      *
149700120613     o          e            PRTprm      2
149800120613     o                                              'NON REPERITI I PARAMETRI-
149900120613     o                                               DI LANCIO.'
150000120613     o          e            PRTprm      1
150100120613     o                                              'RICERCA INTERROTTA.'
150200120613      *
150300120613     o          e            PRTmax      2
150400120613     o                                              'GIÀ REGISTRATI TROPPI RE-
150500120613     o                                              CORD NEL FILE WFNIB00F.'
150600120613     o          e            PRTmax      1
150700120613     o                                              'RICERCA INTERROTTA.'
150800120613      *
150900070704     o          e            PRTerr      2
151000090313     o                                              'RILEVATO ERRORE: SQLCODE'
151100090313     o                       SQLcode       k  +   1
151200090313     o                                        +   1 'SQLSTATE'
151300090313     o                       SQLstate         +   1
151400070703     o                                        +   1 'DURANTE L''ESECUZ-
151500070703     o                                              IONE DEL SEGUENTE -
151600070703     o                                              COMANDO SQL:'
151700070703     o          e            PRTerr      0
151800090313     o                                              'RILEVATO ERRORE: SQLCODE'
151900090313     o                       SQLcode       k  +   1
152000090313     o                                        +   1 'SQLSTATE'
152100090313     o                       SQLstate         +   1
152200070703     o          e            PRTerr      1
152300070704     o                       $Temp( 1)        +   3
152400070703     o          e            PRTerr      1
152500070704     o                       $Temp( 2)        +   3
152600070703     o          e            PRTerr      1
152700070704     o                       $Temp( 3)        +   3
152800070703     o          e            PRTerr      1
152900070704     o                       $Temp( 4)        +   3
153000070703     o          e            PRTerr      1
153100070704     o                       $Temp( 5)        +   3
153200070703     o          e            PRTerr      1
153300070704     o                       $Temp( 6)        +   3
153400070703     o          e            PRTerr      1
153500070704     o                       $Temp( 7)        +   3
153600070703     o          e            PRTerr      1
153700070704     o                       $Temp( 8)        +   3
153800070703     o          e            PRTerr      1
153900070704     o                       $Temp( 9)        +   3
154000070703     o          e            PRTerr      1
154100070703     o                       $Temp(10)        +   3
154200070704     o          e            PRTerr      1
154300070704     o                       $Temp(11)        +   3
154400070704     o          e            PRTerr      1
154500070704     o                       $Temp(12)        +   3
154600070704     o          e            PRTerr      1
154700070704     o                       $Temp(13)        +   3
154800070704     o          e            PRTerr      1
154900070704     o                       $Temp(14)        +   3
155000070704     o          e            PRTerr      1
155100070704     o                       $Temp(15)        +   3
155200070704     o          e            PRTerr      1
155300070704     o                       $Temp(16)        +   3
155400070704     o          e            PRTerr      1
155500070704     o                       $Temp(17)        +   3
155600070704     o          e            PRTerr      1
155700070704     o                       $Temp(18)        +   3
155800070704     o          e            PRTerr      1
155900070704     o                       $Temp(19)        +   3
156000070704     o          e            PRTerr      1
156100070704     o                       $Temp(20)        +   3
156200070704     o          e            PRTerr      1
156300070704     o                       $Temp(21)        +   3
156400070704     o          e            PRTerr      1
156500070704     o                       $Temp(22)        +   3
156600070704     o          e            PRTerr      1
156700070704     o                       $Temp(23)        +   3
156800070704     o          e            PRTerr      1
156900070704     o                       $Temp(24)        +   3
157000070704     o          e            PRTerr      1
157100070704     o                       $Temp(25)        +   3
157200070704     o          e            PRTerr      1
157300070704     o                       $Temp(26)        +   3
157400070704     o          e            PRTerr      1
157500070704     o                       $Temp(27)        +   3
157600070704     o          e            PRTerr      1
157700070704     o                       $Temp(28)        +   3
157800070704     o          e            PRTerr      1
157900070704     o                       $Temp(29)        +   3
158000070704     o          e            PRTerr      1
158100070704     o                       $Temp(30)        +   3
158200070704     o          e            PRTerr      1
158300070704     o                       $Temp(31)        +   3
158400070704     o          e            PRTerr      1
158500070704     o                       $Temp(32)        +   3
158600070704     o          e            PRTerr      1
158700070704     o                       $Temp(33)        +   3
158800070704     o          e            PRTerr      1
158900070704     o                       $Temp(34)        +   3
159000070704     o          e            PRTerr      1
159100070704     o                       $Temp(35)        +   3
159200070704     o          e            PRTerr      1
159300070704     o                       $Temp(36)        +   3
159400070704     o          e            PRTerr      1
159500070704     o                       $Temp(37)        +   3
159600070704     o          e            PRTerr      1
159700070704     o                       $Temp(38)        +   3
159800070704     o          e            PRTerr      1
159900070704     o                       $Temp(39)        +   3
160000070704     o          e            PRTerr      1
160100070704     o                       $Temp(40)        +   3
160200070704     o          e            PRTerr      1
160300070704     o                       $Temp(41)        +   3
160400070704     o          e            PRTerr      1
160500070704     o                       $Temp(42)        +   3
160600070704     o          e            PRTerr      1
160700070704     o                       $Temp(43)        +   3
160800070704     o          e            PRTerr      1
160900070704     o                       $Temp(44)        +   3
161000070704     o          e            PRTerr      1
161100070704     o                       $Temp(45)        +   3
161200070704     o          e            PRTerr      1
161300070704     o                       $Temp(46)        +   3
161400070704     o          e            PRTerr      1
161500070704     o                       $Temp(47)        +   3
161600070704     o          e            PRTerr      1
161700070704     o                       $Temp(48)        +   3
161800070704     o          e            PRTerr      1
161900070704     o                       $Temp(49)        +   3
162000070704     o          e            PRTerr      1
162100070704     o                       $Temp(50)        +   3
162200070704     o          e            PRTerr      1
162300070704     o                       $Temp(51)        +   3
162400070704     o          e            PRTerr      1
162500070704     o                       $Temp(52)        +   3
162600070704     o          e            PRTerr      1
162700070704     o                       $Temp(53)        +   3
162800070704     o          e            PRTerr      1
162900070704     o                       $Temp(54)        +   3
163000070704     o          e            PRTerr      1
163100070704     o                       $Temp(55)        +   3
163200070704     o          e            PRTerr      1
163300070703      *
163400180307      *? S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E ?*
163500070703      *
163600061220**   $Sql_TITAS -----------------------*
163700061220select *                                     1
163800061220 from  TITASP0F                              2
163900061220 from  TITAS10F                              3
164000061220 from  TITAS00F                              4
164100061220 where (TASfns  =  'S'  or  TASncd <> 0)     5
164200061220  and (TASaas*10000+TASmgs) between          6
164300061220  and (TASrsd like '%                        7
164400061220   or  TASrsd like '%                        8
164500061220  and (TASind like '%                        9
164600061220   or  TASind like '%                       10
164700070629  and (TASlod like '%                       11
164800070629   or  TASlod like '%                       12
164900070629  and  TASprd  in ('                        13
165000070629  and (TASksc  in (                         14
165100070629   or  TASccm  in (                         15
165200061220**   $Sql_TITAA --------*
165300061220select *                                     1
165400061220 from  TITAAP0F                              2
165500061220 from  TITAA00F                              3
165600061220 where TAAtrc  =   'M'                       4
165700061220  and  TAAaas between                        5
165800061220  and (TAArsc like '%                        6
165900061220   or  TAArsc like '%                        7
166000061220  and (TAAind like '%                        8
166100061220   or  TAAind like '%                        9
166200070629  and (TAAloc like '%                       10
166300070629   or  TAAloc like '%                       11
166400070629  and  TAAprv  in ('                        12
