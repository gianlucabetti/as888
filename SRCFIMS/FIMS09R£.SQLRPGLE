000100070918      *PARMS OPTION(*NOXREF)
000200070918      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000300070918      *===============================================================*
000400070918      *?FIMS09R    * Stampa riepilogo statistica letture VDL         ?*
000500070918      *===============================================================*
000600070918      *
000700061003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800061003     h alwnull(*inputonly)
000900070918      *
001000070918      *?A R C H I V I?------------------------------------------------*
001100070918      *
001200061003     fAZORG01L  if   e           k disk
001300061003      *
001400070918     fWFMSS00F  o    e             disk    usropn
001500061003     fPRTF198   o    f  198        printer usropn
001600061003     f                                     oflind(*inOF)
001700061003
001800061003      *---------------------------------------------------------------*
001900061003
002000061003      *
002100070918      *?C O S T A N T I?----------------------------------------------*
002200061003      *
002300061003      *
002400070918      *?S C H I E R E?------------------------------------------------*
002500061003      *
002600070919     d $Cmd            s             80    dim( 2) ctdata perrcd(1)
002700061003     d $Txt            s             66    dim(15) ctdata perrcd(1)
002800061003      *
002900070918      *?S T R U T T U R E   D A T I?----------------------------------*
003000061003      *
003100061003      * - Parametri
003200061003     d KPJBA         e ds
003300061003     d FIMS09ds      e ds                  inz
003400070918      *
003500061003      * - Parametri x Controllo profilo utenti
003600061003     d TIBS34DS      e ds                  inz
003700061003      * - Ds di riferimento al file esterno AZUTE00F
003800061003     d AZUTEDS       e ds                  extname(AZUTE00F)
003900061003      * - Ds per dati organigramma
004000070918     d dDatiUte      e ds
004100061003      *
004200061003      * Controllo tabelle
004300061003     d TIBS02ds      e ds                  inz
004400061003     d  T02mod       e                     inz('C')
004500061003     d  T02cod       e                     inz('KPI')
004600061003      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
004700061003     d dKPI          e ds                  inz
004800061003      *
004900061003     d Status         sds
005000061003     d  SDSpgm           *proc
005100061003     d  SDSusr               254    263
005200061003      *
005300061003      * - Dati di FNMSS10F estratti via SQL
005400061003     d FNMSSds       e ds                  inz extname(FNMSS10F)
005500061003     d ds_FNMSS        ds                  inz
005600061003     d  wMSSapl                            inz like(MSSapl)
005700061003     d  wMSSfil                            inz like(MSSfil)
005800061003     d  wMSSnpg                            inz like(MSSnpg)
005900061003     d  TOTtlm                     +  3    inz like(MSStlm)
006000061003     d  TOTcla                     +  2    inz like(MSScla)
006100061003     d  TOTsor                     +  2    inz like(MSSsor)
006200061003     d  TOTdna                     +  2    inz like(MSSdna)
006300061003     d  TOTerr                     +  2    inz like(MSSerr)
006400061003     d  TOTps2                     +  2    inz like(MSSps2)
006500061003     d  TOTcsd                     +  2    inz like(MSScsd)
006600061003     d  TOTccv                     +  2    inz like(MSSccv)
006700061003     d  TOTccp                     +  2    inz like(MSSccp)
006800061003     d  TOTnlp                     +  2    inz like(MSSnlp)
006900061003     d  TOTnls                     +  2    inz like(MSSnls)
007000061003     d  TOTnmi                     +  2    inz like(MSSnmi)
007100061003     d  TOTnpe                     +  2    inz like(MSSnpe)
007200061003     d  TOTmxx                     +  3    inz like(MSSm01)
007300061003      *
007400070918      *?V A R I A B I L I?--------------------------------------------*
007500061003      *
007600061003      * - Flags booleani
007700061003     d $EoF            s              1    inz(*off)
007800061003     d $Err            s              1    inz(*off)
007900061003      * - Indici di schiera / Contatori
008000061003     d xx              s              3  0 inz
008100061003      * - Parametri per QCMDEXC
008200061003     d Qcmd            s             80    inz
008300061003     d Qlen            s             15  5 inz(80)
008400061003      * - Campi di comodo
008500070918     d Data_Eur        s               d   inz  datfmt(*eur)
008600061003     d wDate           s              8  0
008700061003     d wTime           s              6  0
008800061003     d wCountPO        s              1  0 inz
008900070918     d w0091           s              9  1 inz
009000061003      * - Campi in stampa
009100061003     d pDtaI           s              8  0 inz
009200061003     d pDtaF           s              8  0 inz
009300061003     d pNPG            s              1    inz
009400061003     d pKPIob          s              6  1 inz
009500061003     d pKPIef          s              6  1 inz
009600061003      *
009700070918      *?K E Y - L I S T?----------------------------------------------*
009800061003      *
009900061003
010000070918      *---------------------------------------------------------------*
010100070918      *?RIEPILOGO INDICATORI                                         ?*
010200070918      *---------------------------------------------------------------*
010300070918      * OF    - OverFlow                                              *
010400070918      *---------------------------------------------------------------*
010500061003
010600061003     c     *Entry        plist
010700061003     c                   parm                    KPJBA
010800061003      *
010900061003     c                   movel     KPJBU         FIMS09ds
011000061003      *
011100061003      * Operazioni Iniziali
011200061003     c                   exsr      OperazIniz
011300061003      *
011400061003     c                   exsr      OpenCursor
011500061003      *
011600061003      * Elaborazione
011700061003     c                   dou       $Eof    =  *on
011800061003     c                   exsr      ReadCursor
011900061003     c                   enddo
012000061003      *
012100061003     c                   exsr      CloseCursor
012200061003      *
012300061003      * Operazioni Finali
012400061003     c                   exsr      OperazFin
012500061003      *
012600061003      * Fine Programma
012700061003     c                   eval      *inLR   =  *on
012800061003      *
012900070918      *---------------------------------------------------------------*
013000070918      *?Operazioni iniziali                                          ?*
013100070918      *---------------------------------------------------------------*
013200061003     c     OperazIniz    BEGSR
013300061003      *
013400061003      * Reperisce dati job
013500061003     c     *dtaara       define    §azute        azuteds
013600061003     c     *dtaara       define    §datiute      ddatiute
013700061003      *
013800061003     c                   in(E)     *dtaara
013900061003     c                   IF        %ERROR or RSUT = *blanks
014000061003     c                   clear                   Tibs34Ds
014100061003     c                   call      'TIBS34R'
014200061003     c                   parm                    Tibs34Ds
014300061003     c                   in        *dtaara
014400061003     c                   ENDIF
014500061003      *
014600061003      * Override al prtf
014700061003     c                   call      'QCMDEXC'
014800061003     c                   parm      $Cmd(1)       Qcmd
014900061003     c                   parm                    Qlen
015000061003      *
015100061003      * Apertura prtf
015200061003     c                   open      PRTF198
015300061003      *
015400061003      * Stampa della testata iniziale
015500061003     c                   move      *date         Data_eur
015600061003     c     *eur          movel     Data_eur      wDate
015700061003     c                   time                    wTime
015800061003     c     *iso          movel     D09dti        Data_eur
015900061003     c     *eur          movel     Data_eur      pDtaI
016000061003     c     *iso          movel     D09dtf        Data_eur
016100061003     c     *eur          movel     Data_eur      pDtaF
016200061003      *
016300061003     c     D09fil        chain     AZORG
016400061003if  1c                   if        NOT %found(AZORG01L)
016500061003     c                   clear                   ORGdes
016600061003e   1c                   endif
016700061003      *
016800061003     c                   except    PRTtxt
016900070918     c                   if        D09wf   =  *on
017000070918     c                   except    PRTtxt2
017100070918     c                   endif
017200070918     c                   except    PRTtxt3
017300061003     c                   eval      *inOF   =  *off
017400070918      *
017500070918      * Apertura Work-File se richiesta
017600070918if  1c                   if        D09wf   =  *on
017700070919      * - ClrPfm: già eseguita dal cl chiamante (FIMS09C)
017800070918      * - Open
017900070918     c                   open      WFMSS00F
018000070918e   1c                   endif
018100061003      *
018200061003     c                   ENDSR
018300061003      *
018400070918      *---------------------------------------------------------------*
018500070918      *?Apertura cursore.                                            ?*
018600070918      *---------------------------------------------------------------*
018700061003     c     OpenCursor    BEGSR
018800061003      *
018900061003     c/exec sql
019000061003     c+ declare A1 cursor for
019100061003     c+ select  MSSapl, MSSfil, MSSnpg,
019200061003     c+         sum(MSStlm),
019300061003     c+         sum(MSScla),
019400061003     c+         sum(MSSsor),
019500061003     c+         sum(MSSdna),
019600061003     c+         sum(MSSerr),
019700061003     c+         sum(MSSps2),
019800061003     c+         sum(MSScsd),
019900061003     c+         sum(MSSccv),
020000061003     c+         sum(MSSccp),
020100061003     c+         sum(MSSnlp),
020200061003     c+         sum(MSSnls),
020300061003     c+         sum(MSSnmi),
020400061003     c+         sum(MSSnpe),
020500061003     c+         sum(MSSm01 + MSSm02 + MSSm03 + MSSm04 + MSSm05 +
020600061003     c+             MSSm06 + MSSm07 + MSSm08 + MSSm09 + MSSm10 +
020700061003     c+             MSSm11 + MSSm12 + MSSm13 + MSSm14 + MSSm15 +
020800070919     c+             MSSm16 + MSSm17 + MSSm18 + MSSm19)
020900061003     c+ from    FNMSS10F
021000061003     c+ where   MSSapl    =    'V'
021100061003     c+   and   MSSfil   <=    :D09fil
021200061003     c+   and   MSSdfv between :D09dti and :D09dtf
021300061003     c+ group   by     MSSapl, MSSfil, MSSnpg
021400061003     c+ order   by     MSSapl, MSSfil, MSSnpg
021500061003     c+ for     FETCH  only
021600061003     c/end-exec
021700061003      *
021800061003     c/exec sql
021900061003     c+ open A1
022000061003     c/end-exec
022100061003      *
022200061003     c                   ENDSR
022300061003      *
022400070918      *---------------------------------------------------------------*
022500070918      *?Lettura cursore.                                             ?*
022600070918      *---------------------------------------------------------------*
022700061003     c     ReadCursor    BEGSR
022800061003      *
022900061003     c                   clear                   ds_FNMSS
023000061003      *
023100061003     c/exec sql
023200061003     c+ fetch next from A1 into :ds_FNMSS
023300061003     c/end-exec
023400061003      *
023500061003sel 1c                   select
023600061003w   1c                   when      SQLcod  =  100
023700061003     c                   eval      $EoF    =  *on
023800061003w   1c                   when      SQLcod  <  0
023900061003     c                   eval      $Err    =  *on
024000061003     c                   eval      $Eof    =  *on
024100061003x   1c                   other
024200061003     c                   exsr      ElabRec
024300061003e   1c                   endsl
024400061003      *
024500061003     c                   ENDSR
024600061003      *
024700070918      *---------------------------------------------------------------*
024800070918      *?Elaborazione singolo record da FNMSS10F                      ?*
024900070918      *---------------------------------------------------------------*
025000061003     c     ElabRec       BEGSR
025100061003      *
025200070918      * Selezione filiale
025300061003if  1c                   if            D09fil <> 999
025400061003     c                             and D09fil <> wMSSfil
025500061003     c                   leavesr
025600061003e   1c                   endif
025700061003      *
025800061003      * Selezione turno (partenze/arrivi)
025900061003if  1c                   if            D09npg <> 9
026000061003     c                             and D09npg <> wMSSnpg
026100061003     c                   leavesr
026200061003e   1c                   endif
026300061003      *
026400061003      * Reperimento tab. "KPI":
026500061003      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
026600061003     c                   clear                   dKPI
026700061003     c                   reset                   TIBS02ds
026800061003     c*** già così:      eval      T02mod  =  'C'
026900061003     c                   eval      T02sif  =  KNSIF
027000061003     c*** già così:      eval      T02cod  =  'KPI'
027100061003     c                   movel(p)  wMSSnpg       T02ke1
027200061003     c                   movel(p)  wMSSapl       T02ke2
027300061003     c                   call      'TIBS02R'
027400061003     c                   parm                    KPJBA
027500061003     c                   parm                    TIBS02ds
027600061003     c                   if        T02err  =  *blanks
027700061003     c                   movel     T02uni        dKPI
027800061003     c                   endif
027900061003      *
028000070918      * Decodifica filiale
028100070918if  0c                   if        wMSSfil <> ORGfil
028200061003     c     wMSSfil       chain     AZORG
028300061003if  1c                   if        NOT %found(AZORG01L)
028400061003     c                             or  ORGfva <> *blanks
028500061003     c                   clear                   ORGdes
028600061003e   1c                   endif
028700070918e   0c                   endif
028800061003      *
028900061003      * Impostazione categoria F.V.
029000061003     c                   select
029100061003     c                   when      wMSSnpg =  2
029200061003     c                   eval      pNPG    =  'A'
029300061003     c                   when      wMSSnpg =  5
029400061003     c                   eval      pNPG    =  'P'
029500061003     c                   other
029600061003     c                   move      wMSSnpg       pNPG
029700061003     c                   endsl
029800061003      *
029900061003      * Calcolo KPI effettivo
030000070918     c                   eval(h)   pKPIef  =  TOTcla / TOTtlm
030100061003      *
030200061003      * Calcolo KPI obiettivo
030300070918     c                   eval(h)   pKPIob  =  (TOTmxx*§KPIpba)/TOTtlm
030400061003      *
030500061003      * Stampa
030600061003if  1c                   if        *inOF   =  *on
030700061003     c                   except    PRTtxt
030800070918     c                   if        D09wf   =  *on
030900070918     c                   except    PRTtxt2
031000070918     c                   endif
031100070918     c                   except    PRTtxt3
031200061003     c                   eval      *inOF   =  *off
031300061003e   1c                   endif
031400061003     c                   except    PRTdet
031500070918      *
031600070918      * Scrittura record nel Work-File WFMSS00F
031700070918if  1c                   if        D09wf   = *on
031800070918     c                   exsr      Wrt_WFMSS
031900070918e   1c                   endif
032000061003      *
032100061003     c                   ENDSR
032200070918      *
032300070918      *---------------------------------------------------------------*
032400070918      *?Scrittura record nel work-file WFMSS00F                      ?*
032500070918      *---------------------------------------------------------------*
032600070918     c     Wrt_WFMSS     BEGSR
032700070918      *
032800070918     c                   clear                   WFMSS000
032900070918      *
033000070918     c                   eval      WMSusr  = KNMUS
033100070918     c***                eval      WMSdta  = *date
033200070918     c                   move      *date         Data_eur
033300070918     c     *iso          movel     Data_eur      WMSdta
033400070918     c                   eval      WMSdti  = D09dti
033500070918     c                   eval      WMSdtf  = D09dtf
033600070918     c                   eval      WMSfil  = wMSSfil
033700070918     c                   eval      WMSdfil = ORGdes
033800070918     c                   eval      WMSnpg  = pNPG
033900070918     c                   eval      WMStlm  = TOTtlm
034000070918     c                   eval      WMScla  = TOTcla
034100070918     c                   eval      WMSsor  = TOTsor
034200070918     c                   eval      WMSdna  = TOTdna
034300070918     c                   eval      WMSerr  = TOTerr
034400070918     c                   eval      WMSps2  = TOTps2
034500070918     c                   eval      WMScsd  = TOTcsd
034600070918     c                   eval      WMSccv  = TOTccv
034700070918     c                   eval      WMSccp  = TOTccp
034800070918     c                   eval      WMSnlp  = TOTnlp
034900070918     c                   eval      WMSnls  = TOTnls
035000070918     c                   eval      WMSnmi  = TOTnmi
035100070918     c                   eval      WMSnpe  = TOTnpe
035200070918     c                   eval      WMSkpie = pKPIef
035300070918     c                   eval      WMSkpio = pKPIob
035400070920      **** % scostamento KPI
035500070920     c***                if        WMSkpio<> *zeros
035600070920     c***                eval(h)   w0091   = ((WMSkpiE*100)/WMSkpiO)
035700070920     c***                                  - 100
035800070920     c***                else
035900070920     c***                clear                   w0091
036000070920     c***                endif
036100070920if  1c***                if        w0091   > 999,9
036200070920     c***                eval      WMStps  = 999,9
036300070920x   1c***                else
036400070920     c***                eval      WMStps  = w0091
036500070920e   1c***                endif
036600070918      *
036700070918     c                   WRITE     WFMSS000
036800070918      *
036900070918     c                   ENDSR
037000061003      *
037100070918      *---------------------------------------------------------------*
037200070918      *?Chiusura cursore.                                            ?*
037300070918      *---------------------------------------------------------------*
037400061003     c     CloseCursor   BEGSR
037500061003      *
037600061003     c/exec sql
037700061003     c+ close A1
037800061003     c/end-exec
037900061003      *
038000061003     c                   ENDSR
038100061003      *
038200070918      *---------------------------------------------------------------*
038300070918      *?Operazioni finali                                            ?*
038400070918      *---------------------------------------------------------------*
038500061003     c     OperazFin     BEGSR
038600061003      *
038700061003      * Chiusura pgm. TIBS02R (controllo tabelle)
038800061003     c                   clear                   TIBS02ds
038900061003     c                   eval      T02tla  =  'C'
039000061003     c                   call      'TIBS02R'
039100061003     c                   parm                    KPJBA
039200061003     c                   parm                    TIBS02ds
039300070918      *
039400070918      * Chiusura Work-File
039500070918if  1c                   if        %open(WFMSS00F)
039600070918     c                   close     WFMSS00F
039700070918e   1c                   endif
039800061003      *
039900061003      * Stampa "Fine Lista"
040000061003     c                   except    PRTend
040100061003      *
040200061003      * Chiusura prtf
040300061003     c                   close     PRTF198
040400061003      *
040500061003      * Cancellazione override
040600061003     c                   call      'QCMDEXC'
040700061003     c                   parm      $Cmd(2)       Qcmd
040800061003     c                   parm                    Qlen
040900061003      *
041000061003     c                   ENDSR
041100070918      *
041200070918      *?O U T P U T?--------------------------------------------------*
041300070918      *
041400061003     oPRTF198   e            PRTtxt            2
041500061003     o                       RSUT
041600061003     o                                        +  30 'RIEPILOGO STATIST'
041700061003     o                                        +   0 'ICA LETTURE VDL'
041800061003     o                                        +   1 'DAL'
041900061003     o                       pDtaI         Y  +   1
042000061003     o                                        +   1 'AL'
042100061003     o                       pDtaF         Y  +   1
042200061003     o                       SDSpgm           +  30
042300061003     o                       Wdate         Y  +   5
042400061003     o                                        +   5 'Pag.'
042500061003     o                       Page          Z  +   0
042600061003     o          e            PRTtxt      0
042700061003     o                                        +  50 'RIEPILOGO STATIST'
042800061003     o                                        +   0 'ICA LETTURE VDL'
042900061003     o                                        +   1 'DAL'
043000061003     o                       pDtaI         Y  +   1
043100061003     o                                        +   1 'AL'
043200061003     o                       pDtaF         Y  +   1
043300061003     o          e            PRTtxt      1
043400061003     o                       KNSIF
043500061003     o                       SDSusr           +   1
043600061003     o                       Wtime              164 '  :  :  '
043700070918     o          e            PRTtxt2     0
043800070919     o                                        +  71 'CREATO FILE WFMSS-
043900070919     o                                              00F'
044000070918     o          e            PRTtxt3     3
044100061003     o                       $Txt(1)
044200061003     o                       $Txt(6)          +   0
044300061003     o                       $Txt(11)         +   0
044400070918     o          e            PRTtxt3     1
044500061003     o                       $Txt(2)
044600061003     o                       $Txt(7)          +   0
044700061003     o                       $Txt(12)         +   0
044800070918     o          e            PRTtxt3     1
044900061003     o                       $Txt(3)
045000061003     o                       $Txt(8)          +   0
045100061003     o                       $Txt(13)         +   0
045200070918     o          e            PRTtxt3     1
045300061003     o                       $Txt(4)
045400061003     o                       $Txt(9)          +   0
045500061003     o                       $Txt(14)         +   0
045600061003      *
045700061003     o          e            PRTdet      1
045800061003     o                       wMSSfil
045900061003     o                       ORGdes           +   1
046000061003     o                       pNPG             +   2
046100061003     o                       TOTtlm        k  +   1
046200061003     o                       TOTcla        j  +   0
046300061003     o                       TOTsor        j  +   0
046400061003     o                       TOTdna        j  +   0
046500061003     o                       TOTerr        j  +   0
046600061003     o                       TOTps2        k  +   0
046700061003     o                       TOTcsd        k  +   0
046800061003     o                       TOTccv        k  +   0
046900061003     o                       TOTccp        k  +   0
047000061003     o                       TOTnlp        k  +   0
047100061003     o                       TOTnls        k  +   0
047200061003     o                       TOTnmi        k  +   0
047300061003     o                       TOTnpe        k  +   0
047400061003     o                       pKPIef        l  +   0
047500061003     o                       pKPIob        l  +   0
047600061003      *
047700061003     o          e            PRTend      3
047800061003     o                                        +  65 '***  FINE  LISTA '
047900061003     o                                        +   0 ' ***'
048000061003     o          e            PRTend      0
048100061003     o                                        +  65 '***  FINE  LISTA '
048200061003     o                                        +   0 ' ***'
048300070918      *
048400070918      *?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?*
048500070918      *
048600070918** - $Cmd ---------------------------------------------------------------------*
048700061003OVRPRTF FILE(PRTF198) USRDTA('StatistVDL')
048800061003DLTOVR  FILE(PRTF198)
048900070918** - $Txt -------------------------------------------------------*
049000061003                          P                             Colli    O     1
049100061003                          /    Minuti       Colli    smistati    x     2
049200061003Punto Operativo           A  lavorati    caricati  correttam.          3
049300061003------------------------  - --------- ----------- ----------- ----     4
049400061003123 *...+....1....+....2  X 123.456.7-123.456.789-123.456.789-123.     5
049500061003verFlow    Overflow                                                    6
049600061003 uscita    x errori    Letti da    Letti da   Letti con   Letti co     7
049700061003  piena    macchina     pistola     scanner      volume        pes     8
049800061003------- ----------- ----------- ----------- ----------- ----------     9
049900061003456.789-123.456.789-123.456.789-123.456.789-123.456.789-123.456.78    10
050000061003     Letti da                                                         11
050100061003n     pistola   Non letti         Non         Non     KPI     KPI     12
050200061003o   e scanner  da scanner    misurati      pesati  effett  obiett     13
050300061003- ----------- ----------- ----------- ----------- ------- -------     14
0504000610039-123.456.789-123.456.789-123.456.789-123.456.789-12345,6-12345,6-    15
