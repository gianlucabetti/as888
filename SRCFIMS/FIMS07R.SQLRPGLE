000100070918      *PARMS OPTION(*NOXREF)
000200070918      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000300130329     /*PRM dbgview(*source)
000400170201     /*CMD  ovrdbf file(FNMSS00F) tofile(FILTRAGRPF/FNMSS00F) +
000500130329     /*CMD         ovrscope(*calllvl)
000600130329     /*CMD  ovrdbf file(WFMSS00F) tofile(GAITRAAZP/WFMSS00F) +
000700130329     /*CMD         ovrscope(*calllvl)
000800130329     /*END  dltovr file(*all) lvl(*)
000900130329     /*END
001000070918      *===============================================================*
001100130329      *?FIMS07R    * Stampa riepilogo statistica letture VDL         ?*
001200070918      *===============================================================*
001300070918      *
001400061003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500061003     h alwnull(*inputonly)
001600070918      *
001700070918      *?A R C H I V I?------------------------------------------------*
001800070918      *
001900061003     fAZORG01L  if   e           k disk
002000061003      *
002100070918     fWFMSS00F  o    e             disk    usropn
002200061003     fPRTF198   o    f  198        printer usropn
002300061003     f                                     oflind(*inOF)
002400061003
002500061003      *---------------------------------------------------------------*
002600061003
002700061003      *
002800070918      *?C O S T A N T I?----------------------------------------------*
002900061003      *
003000061003      *
003100070918      *?S C H I E R E?------------------------------------------------*
003200061003      *
003300070919     d $Cmd            s             80    dim( 2) ctdata perrcd(1)
003400061003     d $Txt            s             66    dim(15) ctdata perrcd(1)
003500061003      *
003600070918      *?S T R U T T U R E   D A T I?----------------------------------*
003700061003      *
003800061003      * - Parametri
003900061003     d KPJBA         e ds
004000130329     d FIMS07ds      e ds                  inz
004100070918      *
004200061003      * - Parametri x Controllo profilo utenti
004300061003     d TIBS34DS      e ds                  inz
004400061003      * - Ds di riferimento al file esterno AZUTE00F
004500061003     d AZUTEDS       e ds                  extname(AZUTE00F)
004600061003      * - Ds per dati organigramma
004700070918     d dDatiUte      e ds
004800061003      *
004900061003      * Controllo tabelle
005000061003     d TIBS02ds      e ds                  inz
005100061003     d  T02mod       e                     inz('C')
005200061003     d  T02cod       e                     inz('KPI')
005300061003      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
005400061003     d dKPI          e ds                  inz
005500061003      *
005600061003     d Status         sds
005700061003     d  SDSpgm           *proc
005800061003     d  SDSusr               254    263
005900061003      *
006000170201      * - Dati di FNMSS00F estratti via SQL
006100170201     d FNMSS00F      e ds                  based(nullPtr)
006200061003     d ds_FNMSS        ds                  inz
006300061003     d  wMSSapl                            inz like(MSSapl)
006400061003     d  wMSSfil                            inz like(MSSfil)
006500061003     d  wMSSnpg                            inz like(MSSnpg)
006600061003     d  TOTtlm                     +  3    inz like(MSStlm)
006700061003     d  TOTcla                     +  2    inz like(MSScla)
006800061003     d  TOTsor                     +  2    inz like(MSSsor)
006900061003     d  TOTdna                     +  2    inz like(MSSdna)
007000061003     d  TOTerr                     +  2    inz like(MSSerr)
007100061003     d  TOTps2                     +  2    inz like(MSSps2)
007200061003     d  TOTcsd                     +  2    inz like(MSScsd)
007300061003     d  TOTccv                     +  2    inz like(MSSccv)
007400061003     d  TOTccp                     +  2    inz like(MSSccp)
007500061003     d  TOTnlp                     +  2    inz like(MSSnlp)
007600061003     d  TOTnls                     +  2    inz like(MSSnls)
007700061003     d  TOTnmi                     +  2    inz like(MSSnmi)
007800061003     d  TOTnpe                     +  2    inz like(MSSnpe)
007900061003     d  TOTmxx                     +  3    inz like(MSSm01)
008000061003      *
008100070918      *?V A R I A B I L I?--------------------------------------------*
008200061003      *
008300061003      * - Flags booleani
008400061003     d $EoF            s              1    inz(*off)
008500061003     d $Err            s              1    inz(*off)
008600061003      * - Parametri per QCMDEXC
008700061003     d Qcmd            s             80    inz
008800061003     d Qlen            s             15  5 inz(80)
008900061003      * - Campi di comodo
009000070918     d Data_Eur        s               d   inz  datfmt(*eur)
009100061003     d wDate           s              8  0
009200061003     d wTime           s              6  0
009300130329     d*** w0091           s              9  1 inz
009400061003      * - Campi in stampa
009500061003     d pDtaI           s              8  0 inz
009600061003     d pDtaF           s              8  0 inz
009700061003     d pNPG            s              1    inz
009800061003     d pKPIob          s              6  1 inz
009900061003     d pKPIef          s              6  1 inz
010000061003      *
010100070918      *?K E Y - L I S T?----------------------------------------------*
010200061003      *
010300061003
010400070918      *---------------------------------------------------------------*
010500070918      *?RIEPILOGO INDICATORI                                         ?*
010600070918      *---------------------------------------------------------------*
010700070918      * OF    - OverFlow                                              *
010800070918      *---------------------------------------------------------------*
010900061003
011000061003     c     *Entry        plist
011100061003     c                   parm                    KPJBA
011200061003      *
011300130329     c                   movel     KPJBU         FIMS07ds
011400061003      *
011500061003      * Operazioni Iniziali
011600061003     c                   exsr      OperazIniz
011700061003      *
011800130403      * Elaborazione
011900061003     c                   exsr      OpenCursor
012000061003      *
012100061003     c                   dou       $Eof    =  *on
012200061003     c                   exsr      ReadCursor
012300061003     c                   enddo
012400061003      *
012500061003     c                   exsr      CloseCursor
012600061003      *
012700061003      * Operazioni Finali
012800061003     c                   exsr      OperazFin
012900061003      *
013000061003      * Fine Programma
013100061003     c                   eval      *inLR   =  *on
013200061003      *
013300070918      *---------------------------------------------------------------*
013400070918      *?Operazioni iniziali                                          ?*
013500070918      *---------------------------------------------------------------*
013600061003     c     OperazIniz    BEGSR
013700130403      *
013800130403     c/exec sql
013900130403     c+ set option  dynusrprf = *owner,
014000130403     c+             closqlcsr = *endmod
014100130403     c/end-exec
014200061003      *
014300061003      * Reperisce dati job
014400061003     c     *dtaara       define    §azute        azuteds
014500061003     c     *dtaara       define    §datiute      ddatiute
014600061003      *
014700061003     c                   in(E)     *dtaara
014800061003     c                   IF        %ERROR or RSUT = *blanks
014900061003     c                   clear                   Tibs34Ds
015000061003     c                   call      'TIBS34R'
015100061003     c                   parm                    Tibs34Ds
015200061003     c                   in        *dtaara
015300061003     c                   ENDIF
015400061003      *
015500061003      * Override al prtf
015600061003     c                   call      'QCMDEXC'
015700061003     c                   parm      $Cmd(1)       Qcmd
015800061003     c                   parm                    Qlen
015900061003      *
016000061003      * Apertura prtf
016100061003     c                   open      PRTF198
016200061003      *
016300061003      * Stampa della testata iniziale
016400061003     c                   move      *date         Data_eur
016500061003     c     *eur          movel     Data_eur      wDate
016600061003     c                   time                    wTime
016700130329     c     *iso          movel     D07dti        Data_eur
016800061003     c     *eur          movel     Data_eur      pDtaI
016900130329     c     *iso          movel     D07dtf        Data_eur
017000061003     c     *eur          movel     Data_eur      pDtaF
017100061003      *
017200130329     c     D07fil        chain     AZORG
017300061003if  1c                   if        NOT %found(AZORG01L)
017400061003     c                   clear                   ORGdes
017500061003e   1c                   endif
017600061003      *
017700061003     c                   except    PRTtxt
017800130329     c                   if        D07wf   =  *on
017900070918     c                   except    PRTtxt2
018000070918     c                   endif
018100070918     c                   except    PRTtxt3
018200061003     c                   eval      *inOF   =  *off
018300070918      *
018400070918      * Apertura Work-File se richiesta
018500130329if  1c                   if        D07wf   =  *on
018600130329      * - ClrPfm: già eseguita dal cl chiamante (FIMS07C)
018700070918      * - Open
018800070918     c                   open      WFMSS00F
018900070918e   1c                   endif
019000061003      *
019100061003     c                   ENDSR
019200061003      *
019300070918      *---------------------------------------------------------------*
019400070918      *?Apertura cursore.                                            ?*
019500070918      *---------------------------------------------------------------*
019600061003     c     OpenCursor    BEGSR
019700061003      *
019800061003     c/exec sql
019900061003     c+ declare A1 cursor for
020000061003     c+ select  MSSapl, MSSfil, MSSnpg,
020100061003     c+         sum(MSStlm),
020200061003     c+         sum(MSScla),
020300061003     c+         sum(MSSsor),
020400061003     c+         sum(MSSdna),
020500061003     c+         sum(MSSerr),
020600061003     c+         sum(MSSps2),
020700061003     c+         sum(MSScsd),
020800061003     c+         sum(MSSccv),
020900061003     c+         sum(MSSccp),
021000061003     c+         sum(MSSnlp),
021100061003     c+         sum(MSSnls),
021200061003     c+         sum(MSSnmi),
021300061003     c+         sum(MSSnpe),
021400061003     c+         sum(MSSm01 + MSSm02 + MSSm03 + MSSm04 + MSSm05 +
021500061003     c+             MSSm06 + MSSm07 + MSSm08 + MSSm09 + MSSm10 +
021600061003     c+             MSSm11 + MSSm12 + MSSm13 + MSSm14 + MSSm15 +
021700070919     c+             MSSm16 + MSSm17 + MSSm18 + MSSm19)
021800170201     c+ from    FNMSS00F
021900061003     c+ where   MSSapl    =    'V'
022000130329     c+   and   MSSfil   <=    :D07fil
022100130329     c+   and   MSSdfv between :D07dti and :D07dtf
022200061003     c+ group   by     MSSapl, MSSfil, MSSnpg
022300061003     c+ order   by     MSSapl, MSSfil, MSSnpg
022400061003     c+ for     FETCH  only
022500061003     c/end-exec
022600061003      *
022700061003     c/exec sql
022800061003     c+ open A1
022900061003     c/end-exec
023000061003      *
023100061003     c                   ENDSR
023200061003      *
023300070918      *---------------------------------------------------------------*
023400070918      *?Lettura cursore.                                             ?*
023500070918      *---------------------------------------------------------------*
023600061003     c     ReadCursor    BEGSR
023700061003      *
023800061003     c                   clear                   ds_FNMSS
023900061003      *
024000061003     c/exec sql
024100061003     c+ fetch next from A1 into :ds_FNMSS
024200061003     c/end-exec
024300061003      *
024400061003sel 1c                   select
024500061003w   1c                   when      SQLcod  =  100
024600061003     c                   eval      $EoF    =  *on
024700061003w   1c                   when      SQLcod  <  0
024800061003     c                   eval      $Err    =  *on
024900061003     c                   eval      $Eof    =  *on
025000061003x   1c                   other
025100061003     c                   exsr      ElabRec
025200061003e   1c                   endsl
025300061003      *
025400061003     c                   ENDSR
025500061003      *
025600070918      *---------------------------------------------------------------*
025700170201      *?Elaborazione singolo record da FNMSS00F                      ?*
025800070918      *---------------------------------------------------------------*
025900061003     c     ElabRec       BEGSR
026000061003      *
026100070918      * Selezione filiale
026200130329if  1c                   if            D07fil <> 999
026300130329     c                             and D07fil <> wMSSfil
026400061003     c                   leavesr
026500061003e   1c                   endif
026600061003      *
026700061003      * Selezione turno (partenze/arrivi)
026800130329if  1c                   if            D07npg <> 9
026900130329     c                             and D07npg <> wMSSnpg
027000061003     c                   leavesr
027100061003e   1c                   endif
027200061003      *
027300061003      * Reperimento tab. "KPI":
027400061003      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
027500061003     c                   clear                   dKPI
027600061003     c                   reset                   TIBS02ds
027700061003     c*** già così:      eval      T02mod  =  'C'
027800061003     c                   eval      T02sif  =  KNSIF
027900061003     c*** già così:      eval      T02cod  =  'KPI'
028000061003     c                   movel(p)  wMSSnpg       T02ke1
028100061003     c                   movel(p)  wMSSapl       T02ke2
028200061003     c                   call      'TIBS02R'
028300061003     c                   parm                    KPJBA
028400061003     c                   parm                    TIBS02ds
028500061003     c                   if        T02err  =  *blanks
028600061003     c                   movel     T02uni        dKPI
028700061003     c                   endif
028800061003      *
028900070918      * Decodifica filiale
029000070918if  0c                   if        wMSSfil <> ORGfil
029100061003     c     wMSSfil       chain     AZORG
029200061003if  1c                   if        NOT %found(AZORG01L)
029300061003     c                             or  ORGfva <> *blanks
029400061003     c                   clear                   ORGdes
029500061003e   1c                   endif
029600070918e   0c                   endif
029700061003      *
029800061003      * Impostazione categoria F.V.
029900061003     c                   select
030000061003     c                   when      wMSSnpg =  2
030100061003     c                   eval      pNPG    =  'A'
030200061003     c                   when      wMSSnpg =  5
030300061003     c                   eval      pNPG    =  'P'
030400061003     c                   other
030500061003     c                   move      wMSSnpg       pNPG
030600061003     c                   endsl
030700061003      *
030800061003      * Calcolo KPI effettivo
030900070918     c                   eval(h)   pKPIef  =  TOTcla / TOTtlm
031000061003      *
031100061003      * Calcolo KPI obiettivo
031200070918     c                   eval(h)   pKPIob  =  (TOTmxx*§KPIpba)/TOTtlm
031300061003      *
031400061003      * Stampa
031500061003if  1c                   if        *inOF   =  *on
031600061003     c                   except    PRTtxt
031700130329     c                   if        D07wf   =  *on
031800070918     c                   except    PRTtxt2
031900070918     c                   endif
032000070918     c                   except    PRTtxt3
032100061003     c                   eval      *inOF   =  *off
032200061003e   1c                   endif
032300061003     c                   except    PRTdet
032400070918      *
032500070918      * Scrittura record nel Work-File WFMSS00F
032600130329if  1c                   if        D07wf   = *on
032700070918     c                   exsr      Wrt_WFMSS
032800070918e   1c                   endif
032900061003      *
033000061003     c                   ENDSR
033100070918      *
033200070918      *---------------------------------------------------------------*
033300070918      *?Scrittura record nel work-file WFMSS00F                      ?*
033400070918      *---------------------------------------------------------------*
033500070918     c     Wrt_WFMSS     BEGSR
033600070918      *
033700070918     c                   clear                   WFMSS000
033800070918      *
033900070918     c                   eval      WMSusr  = KNMUS
034000070918     c***                eval      WMSdta  = *date
034100070918     c                   move      *date         Data_eur
034200070918     c     *iso          movel     Data_eur      WMSdta
034300130329     c                   eval      WMSdti  = D07dti
034400130329     c                   eval      WMSdtf  = D07dtf
034500070918     c                   eval      WMSfil  = wMSSfil
034600070918     c                   eval      WMSdfil = ORGdes
034700070918     c                   eval      WMSnpg  = pNPG
034800070918     c                   eval      WMStlm  = TOTtlm
034900070918     c                   eval      WMScla  = TOTcla
035000070918     c                   eval      WMSsor  = TOTsor
035100070918     c                   eval      WMSdna  = TOTdna
035200070918     c                   eval      WMSerr  = TOTerr
035300070918     c                   eval      WMSps2  = TOTps2
035400070918     c                   eval      WMScsd  = TOTcsd
035500070918     c                   eval      WMSccv  = TOTccv
035600070918     c                   eval      WMSccp  = TOTccp
035700070918     c                   eval      WMSnlp  = TOTnlp
035800070918     c                   eval      WMSnls  = TOTnls
035900070918     c                   eval      WMSnmi  = TOTnmi
036000070918     c                   eval      WMSnpe  = TOTnpe
036100070918     c                   eval      WMSkpie = pKPIef
036200070918     c                   eval      WMSkpio = pKPIob
036300070920      **** % scostamento KPI
036400070920     c***                if        WMSkpio<> *zeros
036500070920     c***                eval(h)   w0091   = ((WMSkpiE*100)/WMSkpiO)
036600070920     c***                                  - 100
036700070920     c***                else
036800070920     c***                clear                   w0091
036900070920     c***                endif
037000070920if  1c***                if        w0091   > 999,9
037100070920     c***                eval      WMStps  = 999,9
037200070920x   1c***                else
037300070920     c***                eval      WMStps  = w0091
037400070920e   1c***                endif
037500070918      *
037600070918     c                   WRITE     WFMSS000
037700070918      *
037800070918     c                   ENDSR
037900061003      *
038000070918      *---------------------------------------------------------------*
038100070918      *?Chiusura cursore.                                            ?*
038200070918      *---------------------------------------------------------------*
038300061003     c     CloseCursor   BEGSR
038400061003      *
038500061003     c/exec sql
038600061003     c+ close A1
038700061003     c/end-exec
038800061003      *
038900061003     c                   ENDSR
039000061003      *
039100070918      *---------------------------------------------------------------*
039200070918      *?Operazioni finali                                            ?*
039300070918      *---------------------------------------------------------------*
039400061003     c     OperazFin     BEGSR
039500061003      *
039600061003      * Chiusura pgm. TIBS02R (controllo tabelle)
039700061003     c                   clear                   TIBS02ds
039800061003     c                   eval      T02tla  =  'C'
039900061003     c                   call      'TIBS02R'
040000061003     c                   parm                    KPJBA
040100061003     c                   parm                    TIBS02ds
040200070918      *
040300070918      * Chiusura Work-File
040400070918if  1c                   if        %open(WFMSS00F)
040500070918     c                   close     WFMSS00F
040600070918e   1c                   endif
040700061003      *
040800061003      * Stampa "Fine Lista"
040900061003     c                   except    PRTend
041000061003      *
041100061003      * Chiusura prtf
041200061003     c                   close     PRTF198
041300061003      *
041400061003      * Cancellazione override
041500061003     c                   call      'QCMDEXC'
041600061003     c                   parm      $Cmd(2)       Qcmd
041700061003     c                   parm                    Qlen
041800061003      *
041900061003     c                   ENDSR
042000070918      *
042100070918      *?O U T P U T?--------------------------------------------------*
042200070918      *
042300061003     oPRTF198   e            PRTtxt            2
042400061003     o                       RSUT
042500061003     o                                        +  30 'RIEPILOGO STATIST'
042600061003     o                                        +   0 'ICA LETTURE VDL'
042700061003     o                                        +   1 'DAL'
042800061003     o                       pDtaI         Y  +   1
042900061003     o                                        +   1 'AL'
043000061003     o                       pDtaF         Y  +   1
043100061003     o                       SDSpgm           +  30
043200061003     o                       Wdate         Y  +   5
043300061003     o                                        +   5 'Pag.'
043400061003     o                       Page          Z  +   0
043500061003     o          e            PRTtxt      0
043600061003     o                                        +  50 'RIEPILOGO STATIST'
043700061003     o                                        +   0 'ICA LETTURE VDL'
043800061003     o                                        +   1 'DAL'
043900061003     o                       pDtaI         Y  +   1
044000061003     o                                        +   1 'AL'
044100061003     o                       pDtaF         Y  +   1
044200061003     o          e            PRTtxt      1
044300061003     o                       KNSIF
044400061003     o                       SDSusr           +   1
044500061003     o                       Wtime              164 '  :  :  '
044600070918     o          e            PRTtxt2     0
044700070919     o                                        +  71 'CREATO FILE WFMSS-
044800070919     o                                              00F'
044900070918     o          e            PRTtxt3     3
045000061003     o                       $Txt(1)
045100061003     o                       $Txt(6)          +   0
045200061003     o                       $Txt(11)         +   0
045300070918     o          e            PRTtxt3     1
045400061003     o                       $Txt(2)
045500061003     o                       $Txt(7)          +   0
045600061003     o                       $Txt(12)         +   0
045700070918     o          e            PRTtxt3     1
045800061003     o                       $Txt(3)
045900061003     o                       $Txt(8)          +   0
046000061003     o                       $Txt(13)         +   0
046100070918     o          e            PRTtxt3     1
046200061003     o                       $Txt(4)
046300061003     o                       $Txt(9)          +   0
046400061003     o                       $Txt(14)         +   0
046500061003      *
046600061003     o          e            PRTdet      1
046700061003     o                       wMSSfil
046800061003     o                       ORGdes           +   1
046900061003     o                       pNPG             +   2
047000061003     o                       TOTtlm        k  +   1
047100061003     o                       TOTcla        j  +   0
047200061003     o                       TOTsor        j  +   0
047300061003     o                       TOTdna        j  +   0
047400061003     o                       TOTerr        j  +   0
047500061003     o                       TOTps2        k  +   0
047600061003     o                       TOTcsd        k  +   0
047700061003     o                       TOTccv        k  +   0
047800061003     o                       TOTccp        k  +   0
047900061003     o                       TOTnlp        k  +   0
048000061003     o                       TOTnls        k  +   0
048100061003     o                       TOTnmi        k  +   0
048200061003     o                       TOTnpe        k  +   0
048300061003     o                       pKPIef        l  +   0
048400061003     o                       pKPIob        l  +   0
048500061003      *
048600061003     o          e            PRTend      3
048700061003     o                                        +  65 '***  FINE  LISTA '
048800061003     o                                        +   0 ' ***'
048900061003     o          e            PRTend      0
049000061003     o                                        +  65 '***  FINE  LISTA '
049100061003     o                                        +   0 ' ***'
049200070918      *
049300070918      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
049400070918      *
049500070918** - $Cmd ---------------------------------------------------------------------*
049600061003OVRPRTF FILE(PRTF198) USRDTA('StatistVDL')
049700061003DLTOVR  FILE(PRTF198)
049800070918** - $Txt -------------------------------------------------------*
049900061003                          P                             Colli    O     1
050000061003                          /    Minuti       Colli    smistati    x     2
050100061003Punto Operativo           A  lavorati    caricati  correttam.          3
050200061003------------------------  - --------- ----------- ----------- ----     4
050300061003123 *...+....1....+....2  X 123.456.7-123.456.789-123.456.789-123.     5
050400061003verFlow    Overflow                                                    6
050500061003 uscita    x errori    Letti da    Letti da   Letti con   Letti co     7
050600061003  piena    macchina     pistola     scanner      volume        pes     8
050700061003------- ----------- ----------- ----------- ----------- ----------     9
050800061003456.789-123.456.789-123.456.789-123.456.789-123.456.789-123.456.78    10
050900061003     Letti da                                                         11
051000061003n     pistola   Non letti         Non         Non     KPI     KPI     12
051100061003o   e scanner  da scanner    misurati      pesati  effett  obiett     13
051200061003- ----------- ----------- ----------- ----------- ------- -------     14
0513000610039-123.456.789-123.456.789-123.456.789-123.456.789-12345,6-12345,6-    15
