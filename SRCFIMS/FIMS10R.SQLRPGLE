000100070918      *PARMS OPTION(*NOXREF)
000200070918      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000300130402       //==============================================================
000400130402       //?FIMS10R - Riepilogo statistica letture Picking con Rulliera ?
000500130402       //==============================================================
000600130402
000700130402       //--------------------------------------------------------------
000800130402       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900130402       //--------------------------------------------------------------
001000130402
001100130402     /*PRM  dbgview(*source)
001200130402     /*PRM  commit(*NONE)
001300130402     /*CMD  ovrdbf file(WFMSS10F) tofile(GAITRAAZP/WFMSS10F) +
001400130402     /*CMD         ovrscope(*calllvl)
001500170201     /*CMD  ovrdbf file(FNMSS00F) tofile(FILTRAGRPF/FNMSS00F) +
001600130402     /*CMD         ovrscope(*calllvl)
001700130402     /*END  dltovr file(*all) lvl(*)
001800130402     /*END
001900130402
002000130402       //--------------------------------------------------------------
002100130402       //?Specifiche di controllo.                                     ?
002200130402       //--------------------------------------------------------------
002300130402
002400061003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500061003     h alwnull(*inputonly)
002600130402
002700130402       //--------------------------------------------------------------
002800130402       //?Dichiarazione file.                                          ?
002900130402       //--------------------------------------------------------------
003000130402
003100130402       // -?Organigramma di filiale?
003200061003     fAZORG01L  if   e           k disk
003300130402
003400130402       // -?Work-File?
003500130402     fWFMSS10F  o    e             disk    usropn
003600130402
003700130402       // -?Stampa?
003800061003     fPRTF198   o    f  198        printer usropn
003900061003     f                                     oflind(*inOF)
004000061003
004100130402       //--------------------------------------------------------------
004200130402       //?Definizione costanti.                                        ?
004300130402       //--------------------------------------------------------------
004400061003
004500130403     d c_APL           c                   const('K')
004600130403     d c_NPG           c                   const('2')
004700130402
004800130402       //--------------------------------------------------------------
004900130402       //?Definizione schiere.                                         ?
005000130402       //--------------------------------------------------------------
005100130402
005200130402       // -?Messaggi di errore?
005300130403     d $Cmd            s             80    dim( 6) ctdata perrcd(1)
005400130402
005500130402       // -?Testate in stampa?
005600130403     d $Txt            s             66    dim( 5) ctdata perrcd(1)
005700130402
005800130402       //--------------------------------------------------------------
005900130402       //?Definizione aree dati.                                       ?
006000130402       //--------------------------------------------------------------
006100130402
006200130402       // -?Dati utente?
006300130402     d §AzUte        e ds                  extname(AZUTE00F)
006400130402     d                                     dtaara
006500130402     d §DatiUte      e ds                  extname(dDatiUte)
006600130402     d                                     dtaara
006700130402
006800130402       //--------------------------------------------------------------
006900130402       //?Definizione strutture dati.                                  ?
007000130402       //--------------------------------------------------------------
007100130402
007200130402       // -?Parametri ricevuti?
007300061003     d KPJBA         e ds
007400130402     d FIMS10ds      e ds                  inz
007500130402
007600130402       // -?Tab. "KPI" = Obiettivi di Produttività di un Impianto?
007700130402       //               ?Automatico Smistacolli?
007800061003     d dKPI          e ds                  inz
007900130402
008000130402       // -?Status ds?
008100061003     d Status         sds
008200061003     d  SDSpgm           *proc
008300061003     d  SDSusr               254    263
008400130402
008500170201       // -?Dati di FNMSS00F estratti via SQL?
008600170201     d FNMSS00F      e ds                  based(nullPtr)
008700061003     d ds_FNMSS        ds                  inz
008800061003     d  wMSSapl                            inz like(MSSapl)
008900061003     d  wMSSfil                            inz like(MSSfil)
009000130403     d*//  wMSSnpg                            inz like(MSSnpg)
009100061003     d  TOTtlm                     +  3    inz like(MSStlm)
009200061003     d  TOTcla                     +  2    inz like(MSScla)
009300061003     d  TOTmxx                     +  3    inz like(MSSm01)
009400130402
009500130402       //--------------------------------------------------------------
009600130402       //?Definizione variabili globali.                               ?
009700130402       //--------------------------------------------------------------
009800130402
009900130402       // -?Flags booleani?
010000130402     d $EoF            s               n   inz(*off)
010100130402     d $Err            s               n   inz(*off)
010200130402
010300130402       // -?Campi di comodo?
010400070918     d Data_Eur        s               d   inz  datfmt(*eur)
010500061003     d wDate           s              8  0
010600061003     d wTime           s              6  0
010700130402
010800130402       // -?Campi di stampa?
010900061003     d pDtaI           s              8  0 inz
011000061003     d pDtaF           s              8  0 inz
011100130403     d*// pNPG            s              1    inz
011200061003     d pKPIob          s              6  1 inz
011300061003     d pKPIef          s              6  1 inz
011400130402
011500130402       //--------------------------------------------------------------
011600130402       //?Definizione prototipi procedure.                             ?
011700130402       //--------------------------------------------------------------
011800130402
011900130402       // -?Reperimento dati utente?
012000130402     d TIBS34ds      e ds
012100130402      /copy gaitrasrc/srcProtoPR,TIBS34R
012200130402
012300130402       // -?Execuzione comando di sistema?
012400130402     d Qcmd            s             80    inz
012500130402     d Qlen            s             15  5 inz(80)
012600130402      ***/copy gaitrasrc/srcProtoPR,QCMDEXC
012700130402
012800130402       // -?Valori in TIBS02DS per *pgm TIBS02R?
012900130402     d TNTBE_Modo_Controllo...
013000130402     d                 c                   const('C')
013100130402     d TNTBE_Errore_NonTrovato...
013200130402     d                 c                   const('E')
013300130402     d TNTBE_Errore_Annullato...
013400130402     d                 c                   const('A')
013500130402       // -?Ricerca/Controllo tabelle?
013600130402     d TIBS02ds      e ds                  inz
013700130402     d   T02mod      e                     inz('C')
013800130402     d   T02cod      e                     inz('KPI')
013900130402      ***/copy gaitrasrc/srcProtoPR,TIBS02R
014000130402
014100130402       //--------------------------------------------------------------
014200130402       //?Definizione key-list.                                        ?
014300130402       //--------------------------------------------------------------
014400130402
014500130402
014600130402       //--------------------------------------------------------------
014700130402       //?RIEPILOGO INDICATORI                                         ?
014800130402       //--------------------------------------------------------------
014900130402       // OF    - OverFlow
015000130402       //--------------------------------------------------------------
015100130402
015200130402       //--------------------------------------------------------------
015300130402       //?M A I N - L I N E                                            ?
015400130402       //--------------------------------------------------------------
015500061003
015600061003     c     *Entry        plist
015700061003     c                   parm                    KPJBA
015800061003      *
015900130402     c                   movel     KPJBU         FIMS10ds
016000061003      *
016100130402      * -?Operazioni iniziali?
016200061003     c                   exsr      OperazIniz
016300061003      *
016400130402      * -?Elaborazione?
016500130403     c                   exsr      OpenCursor
016600130403      *
016700061003     c                   dou       $Eof    =  *on
016800061003     c                   exsr      ReadCursor
016900061003     c                   enddo
017000061003      *
017100130403     c                   exsr      CloseCursor
017200130403      *
017300130402      * -?Operazioni Finali?
017400061003     c                   exsr      OperazFin
017500130402     c                   return
017600061003      *
017700070918      *---------------------------------------------------------------*
017800070918      *?Operazioni iniziali                                          ?*
017900070918      *---------------------------------------------------------------*
018000061003     c     OperazIniz    BEGSR
018100130402      *
018200130402
018300130402     c/exec sql
018400130402     c+ set option  dynusrprf = *owner,
018500130402     c+             closqlcsr = *endmod
018600130402     c/end-exec
018700130402      *
018800130402     c                   eval      *inLR   =  *on
018900061003      *
019000130402      * -?Reperimento dati job?
019100130402     c                   exsr      sr_DatiJob
019200130402      *
019300130402     c                   select
019400170201      * -?Override al file FNMSS00F dal s.i. di Sede su SETRAS?
019500130402     c                   when      KNSIF = 'GAITRA201 '
019600130402     c                   eval      Qcmd  = %trimr( $Cmd(3) )
019700170201     c                                   + 'FILTRAGRU/FNMSS00F)'
019800170201      * -?Override al file FNMSS00F dal s.i. di Sede su AS888?
019900130402     c                   when      %subst(KNSIF : 1 : 7) = 'GAITRAP'
020000130402     c                   eval      Qcmd  = %trimr( $Cmd(3) )
020100170201     c                                   + 'FILTRAGRPF/FNMSS00F)'
020200130402      * -?Override al file WFMSS10F dal s.i. di Filiale su SETRAS?
020300130402     c                   when      KNSIF = 'FILTRA201 '
020400130402     c                   eval      Qcmd  = %trimr( $Cmd(4) )
020500130402     c                                   + 'GAITRAAZM/WFMSS10F)'
020600130402      * -?Override al file WFMSS10F dal s.i. di Filiale su AS888?
020700130402     c                   when      %subst(KNSIF : 1 : 7) = 'FILTRAP'
020800130402     c                   eval      Qcmd  = %trimr( $Cmd(4) )
020900130402     c                                   + 'GAITRAAZP/WFMSS10F)'
021000130402     c                   endsl
021100130402      *
021200130402     c                   call      'QCMDEXC'
021300130402     c                   parm                    Qcmd
021400130402     c                   parm                    Qlen
021500061003      *
021600130402      * -?Override al prtf?
021700061003     c                   call      'QCMDEXC'
021800061003     c                   parm      $Cmd(1)       Qcmd
021900061003     c                   parm                    Qlen
022000061003      *
022100130402      * -?Apertura prtf?
022200061003     c                   open      PRTF198
022300061003      *
022400130402      * -?Stampa della testata iniziale?
022500061003     c                   move      *date         Data_eur
022600061003     c     *eur          movel     Data_eur      wDate
022700061003     c                   time                    wTime
022800130402     c     *iso          movel     D10dti        Data_eur
022900061003     c     *eur          movel     Data_eur      pDtaI
023000130402     c     *iso          movel     D10dtf        Data_eur
023100061003     c     *eur          movel     Data_eur      pDtaF
023200061003      *
023300130402     c     D10fil        chain     AZORG
023400061003if  1c                   if        NOT %found(AZORG01L)
023500061003     c                   clear                   ORGdes
023600061003e   1c                   endif
023700061003      *
023800061003     c                   except    PRTtxt
023900130402     c                   if        D10wf   =  *on
024000070918     c                   except    PRTtxt2
024100070918     c                   endif
024200070918     c                   except    PRTtxt3
024300061003     c                   eval      *inOF   =  *off
024400070918      *
024500130402      * -?Apertura Work-File se richiesta?
024600130402if  1c                   if        D10wf   =  *on
024700130402      * - -?ClrPfm?
024800130403     c                   select
024900130403     c                   when      KNSIF = 'FILTRA201 '
025000130403     c                   eval      Qcmd  = %replace( 'GAITRAAZM/WFMSS10F)' :
025100130403     c                                               $Cmd(6) :
025200130403     c                                     %scan('WFMSS10F)' : $Cmd(6) ) )
025300130403     c                   when      %subst(KNSIF : 1 : 7) = 'FILTRAP'
025400130403     c                   eval      Qcmd  = %replace( 'GAITRAAZP/WFMSS10F)' :
025500130403     c                                               $Cmd(6) :
025600130403     c                                     %scan('WFMSS10F)' : $Cmd(6) ) )
025700130403     c                   other
025800130403     c                   eval      Qcmd  = $Cmd(6)
025900130403     c                   endsl
026000130402     c                   call      'QCMDEXC'
026100130403     c                   parm                    Qcmd
026200130402     c                   parm                    Qlen
026300130402      * - -?Open?
026400130402     c                   open      WFMSS10F
026500070918e   1c                   endif
026600130403      *
026700130403      * -?Reperimento tab. "KPI":?
026800130403      *  ?Obiettivi di Produttività di un Impianto Automatico Smistacolli?
026900130403     c                   clear                   dKPI
027000130403     c                   reset                   TIBS02ds
027100130403     c*** già così:      eval      T02mod  =  'C'
027200130403     c                   eval      T02sif  =  KNSIF
027300130403     c*** già così:      eval      T02cod  =  'KPI'
027400130403     c*//                   movel(p)  wMSSnpg       T02ke1
027500130403     c*//                   movel(p)  wMSSapl       T02ke2
027600130403     c                   movel(p)  c_NPG         T02ke1
027700130403     c                   movel(p)  c_APL         T02ke2
027800130403     c                   call      'TIBS02R'
027900130403     c                   parm                    KPJBA
028000130403     c                   parm                    TIBS02ds
028100130403     c                   if        T02err  =  *blanks
028200130403     c                   movel     T02uni        dKPI
028300130403     c                   endif
028400061003      *
028500061003     c                   ENDSR
028600130402      *
028700130402      /free
028800130402
028900130402       //--------------------------------------------------------------
029000130402       //?Reperimento Dati del job (Utente/Operativi).                 ?
029100130402       //--------------------------------------------------------------
029200130402       BEGSR  sr_DatiJob;
029300130402
029400130402         in(E) §AzUte;
029500130402         if NOT %error;
029600130402           in(E) §DatiUte;
029700130402         endif;
029800130402         if %error or RSut = *blanks;
029900130402           clear TIBS34ds;
030000130402           tibs34r ( tibs34ds );
030100130402           in §AzUte;
030200130402           in §DatiUte;
030300130402         endif;
030400130402
030500130402       ENDSR;
030600130402
030700130402      /end-free
030800061003      *
030900070918      *---------------------------------------------------------------*
031000070918      *?Apertura cursore.                                            ?*
031100070918      *---------------------------------------------------------------*
031200061003     c     OpenCursor    BEGSR
031300061003      *
031400061003     c/exec sql
031500061003     c+ declare A1 cursor for
031600130403     c+ select  MSSapl, MSSfil,
031700061003     c+         sum(MSStlm),
031800061003     c+         sum(MSScla),
031900061003     c+         sum(MSSm01 + MSSm02 + MSSm03 + MSSm04 + MSSm05 +
032000061003     c+             MSSm06 + MSSm07 + MSSm08 + MSSm09 + MSSm10 +
032100061003     c+             MSSm11 + MSSm12 + MSSm13 + MSSm14 + MSSm15 +
032200070919     c+             MSSm16 + MSSm17 + MSSm18 + MSSm19)
032300170201     c+ from    FNMSS00F
032400130403     c+ where   MSSapl  = 'K'
032500130403     c+   and   MSSnpg  = '2'
032600130403     c+   and   MSSfil <= :D10fil
032700130402     c+   and   MSSdfv between :D10dti and :D10dtf
032800130403     c+ group   by  MSSapl, MSSfil
032900130403     c+ order   by  MSSapl, MSSfil
033000130403     c+ for     FETCH only
033100061003     c/end-exec
033200061003      *
033300061003     c/exec sql
033400061003     c+ open A1
033500061003     c/end-exec
033600061003      *
033700061003     c                   ENDSR
033800061003      *
033900070918      *---------------------------------------------------------------*
034000070918      *?Lettura cursore.                                             ?*
034100070918      *---------------------------------------------------------------*
034200061003     c     ReadCursor    BEGSR
034300061003      *
034400061003     c                   clear                   ds_FNMSS
034500061003      *
034600061003     c/exec sql
034700061003     c+ fetch next from A1 into :ds_FNMSS
034800061003     c/end-exec
034900061003      *
035000061003sel 1c                   select
035100061003w   1c                   when      SQLcod  =  100
035200061003     c                   eval      $EoF    =  *on
035300061003w   1c                   when      SQLcod  <  0
035400061003     c                   eval      $Err    =  *on
035500061003     c                   eval      $Eof    =  *on
035600061003x   1c                   other
035700061003     c                   exsr      ElabRec
035800061003e   1c                   endsl
035900061003      *
036000061003     c                   ENDSR
036100061003      *
036200070918      *---------------------------------------------------------------*
036300170201      *?Elaborazione singolo record da FNMSS00F                      ?*
036400070918      *---------------------------------------------------------------*
036500061003     c     ElabRec       BEGSR
036600061003      *
036700130402      * -?Selezione filiale?
036800130402if  1c                   if            D10fil <> 999
036900130402     c                             and D10fil <> wMSSfil
037000061003     c                   leavesr
037100061003e   1c                   endif
037200061003      *
037300130402      * -?Decodifica filiale?
037400070918if  0c                   if        wMSSfil <> ORGfil
037500061003     c     wMSSfil       chain     AZORG
037600061003if  1c                   if        NOT %found(AZORG01L)
037700061003     c                             or  ORGfva <> *blanks
037800061003     c                   clear                   ORGdes
037900061003e   1c                   endif
038000070918e   0c                   endif
038100061003      *
038200130402      * -?Calcolo KPI effettivo?
038300070918     c                   eval(h)   pKPIef  =  TOTcla / TOTtlm
038400061003      *
038500130402      * -?Calcolo KPI obiettivo?
038600130403     c                   eval(h)   pKPIob  =  (TOTmxx*§KPIpba)/TOTtlm
038700061003      *
038800130402      * -?Stampa?
038900061003if  1c                   if        *inOF   =  *on
039000061003     c                   except    PRTtxt
039100130402     c                   if        D10wf   =  *on
039200070918     c                   except    PRTtxt2
039300070918     c                   endif
039400070918     c                   except    PRTtxt3
039500061003     c                   eval      *inOF   =  *off
039600061003e   1c                   endif
039700061003     c                   except    PRTdet
039800070918      *
039900130402      * -?Scrittura record nel Work-File WFMSS10F?
040000130402if  1c                   if        D10wf   = *on
040100070918     c                   exsr      Wrt_WFMSS
040200070918e   1c                   endif
040300061003      *
040400061003     c                   ENDSR
040500070918      *
040600070918      *---------------------------------------------------------------*
040700130402      *?Scrittura record nel work-file WFMSS10F                      ?*
040800070918      *---------------------------------------------------------------*
040900070918     c     Wrt_WFMSS     BEGSR
041000070918      *
041100130402     c                   clear                   WFMSS100
041200070918      *
041300070918     c                   eval      WMSusr  = KNMUS
041400070918     c                   move      *date         Data_eur
041500070918     c     *iso          movel     Data_eur      WMSdta
041600130402     c                   eval      WMSdti  = D10dti
041700130402     c                   eval      WMSdtf  = D10dtf
041800070918     c                   eval      WMSfil  = wMSSfil
041900070918     c                   eval      WMSdfil = ORGdes
042000070918     c                   eval      WMStlm  = TOTtlm
042100070918     c                   eval      WMScla  = TOTcla
042200070918     c                   eval      WMSkpie = pKPIef
042300070918     c                   eval      WMSkpio = pKPIob
042400070918      *
042500130402     c                   WRITE     WFMSS100
042600070918      *
042700070918     c                   ENDSR
042800061003      *
042900070918      *---------------------------------------------------------------*
043000070918      *?Chiusura cursore.                                            ?*
043100070918      *---------------------------------------------------------------*
043200061003     c     CloseCursor   BEGSR
043300061003      *
043400061003     c/exec sql
043500061003     c+ close A1
043600061003     c/end-exec
043700061003      *
043800061003     c                   ENDSR
043900061003      *
044000070918      *---------------------------------------------------------------*
044100070918      *?Operazioni finali                                            ?*
044200070918      *---------------------------------------------------------------*
044300061003     c     OperazFin     BEGSR
044400061003      *
044500130402      * -?Chiusura pgm. TIBS02R (controllo tabelle)?
044600061003     c                   clear                   TIBS02ds
044700061003     c                   eval      T02tla  =  'C'
044800061003     c                   call      'TIBS02R'
044900061003     c                   parm                    KPJBA
045000061003     c                   parm                    TIBS02ds
045100061003      *
045200130402      * -?Stampa "Fine Lista"?
045300061003     c                   except    PRTend
045400061003      *
045500130402      * -?Chiusura prtf?
045600061003     c                   close     PRTF198
045700130403      *
045800130403      * -?Chiusura Work-File?
045900130403if  1c                   if        %open(WFMSS10F)
046000130403     c                   close     WFMSS10F
046100130403e   1c                   endif
046200061003      *
046300130402      * -?Cancellazione override?
046400061003     c                   call      'QCMDEXC'
046500061003     c                   parm      $Cmd(2)       Qcmd
046600061003     c                   parm                    Qlen
046700130402     c                   call      'QCMDEXC'
046800130402     c                   parm      $Cmd(5)       Qcmd
046900130402     c                   parm                    Qlen
047000061003      *
047100061003     c                   ENDSR
047200130402
047300130402       //--------------------------------------------------------------
047400130402       //?Specifiche di Output.                                        ?
047500130402       //--------------------------------------------------------------
047600130402
047700061003     oPRTF198   e            PRTtxt            2
047800061003     o                       RSUT
047900130402     o                                        +  25 'RIEPILOGO STATIST'
048000130402     o                                        +   0 'ICA LETTURE PICKI'
048100130402     o                                        +   0 'NG IN RULLIERA'
048200061003     o                                        +   1 'DAL'
048300061003     o                       pDtaI         Y  +   1
048400061003     o                                        +   1 'AL'
048500061003     o                       pDtaF         Y  +   1
048600130402     o                       SDSpgm           +  25
048700061003     o                       Wdate         Y  +   5
048800061003     o                                        +   5 'Pag.'
048900061003     o                       Page          Z  +   0
049000061003     o          e            PRTtxt      0
049100130402     o                                        +  45 'RIEPILOGO STATIST'
049200130402     o                                        +   0 'ICA LETTURE PICKI'
049300130402     o                                        +   0 'NG IN RULLIERA'
049400061003     o                                        +   1 'DAL'
049500061003     o                       pDtaI         Y  +   1
049600061003     o                                        +   1 'AL'
049700061003     o                       pDtaF         Y  +   1
049800061003     o          e            PRTtxt      1
049900061003     o                       KNSIF
050000061003     o                       SDSusr           +   1
050100130403     o                       Wtime              170 '  :  :  '
050200070918     o          e            PRTtxt2     0
050300070919     o                                        +  71 'CREATO FILE WFMSS-
050400130402     o                                              10F'
050500070918     o          e            PRTtxt3     3
050600061003     o                       $Txt(1)
050700070918     o          e            PRTtxt3     1
050800061003     o                       $Txt(2)
050900070918     o          e            PRTtxt3     1
051000061003     o                       $Txt(3)
051100070918     o          e            PRTtxt3     1
051200061003     o                       $Txt(4)
051300061003      *
051400061003     o          e            PRTdet      1
051500061003     o                       wMSSfil
051600061003     o                       ORGdes           +   1
051700130403     o*//                    pNPG             +   2
051800130403     o                       TOTtlm        k  +   2
051900130403     o                       TOTcla        j  +   1
052000130403     o                       pKPIef        l  +   1
052100130403     o                       pKPIob        l  +   1
052200061003      *
052300061003     o          e            PRTend      3
052400130403     o                                        +  25 '***  FINE  LISTA '
052500061003     o                                        +   0 ' ***'
052600061003     o          e            PRTend      0
052700130403     o                                        +  25 '***  FINE  LISTA '
052800061003     o                                        +   0 ' ***'
052900130402
053000130402       //--------------------------------------------------------------
053100130402       //?Definizione schiere a tempo di compilazione                  ?
053200130402       //--------------------------------------------------------------
053300130402
053400130402** -?$Cmd?---------------------------------------------------------------------*
053500130402OVRPRTF FILE(PRTF198) USRDTA('StatPkgRul')                                       1
053600130402DLTOVR  FILE(PRTF198)                                                            2
053700170201ovrdbf  file(FNMSS00F) tofile(                                                   3
053800130402ovrdbf  file(WFMSS10F) tofile(                                                   4
053900130402dltovr  file(*ALL)                                                               5
054000130403clrpfm  file(WFMSS10F)                                                           6
054100130402** -?$Txt?-------------------------------------------------------*
054200130403                                                                       1
054300130403                             Minuti        Colli      KPI      KPI     2
054400130403Punto Operativo            lavorati     caricati   effett   obiett     3
054500130403------------------------  ---------  -----------  -------  -------     4
054600130403123 *...+....1....+....2  123.456.7- 123.456.789- 12345,6- 12345,6-    5
