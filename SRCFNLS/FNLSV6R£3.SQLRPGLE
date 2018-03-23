000100120720     /*PRM  dbgview(*source)
000200120720     /*/*D  ovrdbf file(WFDDT01L) tofile(FILTRAAZP/WFDDT01L) +
000300120720     /*/*D         ovrscope(*calllvl)
000400120302     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000500120302     /*CMD         ovrscope(*calllvl)
000600120302     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
000700120302     /*CMD         ovrscope(*calllvl)
000800120302     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000900120302     /*CMD         ovrscope(*calllvl)
001000120720     /*/*D  dltovr file(WFDDT01L) lvl(*)
001100120302     /*END  dltovr file(TITAS30C TITA430C FIAR531C) lvl(*)
001200120302     /*END
001300120302      *
001400001027     H DECEDIT('0,') DATEDIT(*DMY.)
001500120720      * FNLSV6R *----------------------------------------------------------*
001600080909      *  controllo documenti da restituire al mittente
001700080319      *--------------------------------------------------------------------*
001800080327     Fazorg01l  IF   E           K DISK
001900080327     Ftabel00f  IF   E           K DISK
002000120402     Ftntbe01l  IF   E           K DISK
002100080902     Ffnlbl01l  IF   E           K DISK
002200120406     Ffnlbl02l  IF   E           K DISK    rename(fnlbl000:fnlbl002)
002300110616     Fcnaco00f  IF   E           K DISK
002400110617     FwfDDT01L  uF a E           K DISK    extfile(WWFDDT) usropn
002500080902     Ftitas30c  IF   E           K DISK    extfile(WTAs30) usropn
002600080829     Ftita430c  IF   E           K DISK    extfile(WTA430) usropn
002700080829     Ffiar531c  uF   E           K DISK    extfile(WTA531) usropn
002800120720     FFNLSV6P   O    E             PRINTER
002900050705      *
003000080317      * - Passaggio parametri
003100080325     d KPJBA         e ds
003200120720     d*
003300080328     d tibs34ds      e ds                  inz
003400080410     d tibs02ds      e ds                  inz
003500080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
003600080328     d DDatiUte      e ds                  dtaara(§datiute)
003700120720     d*
003800080829     d dar5gen       e ds
003900120824     d dAR5doc       e ds                  inz
004000080902     d DS7O          e ds
004100100603     d DS7R          e ds
004200120720     d*
004300080829     d tasSPE          ds
004400080829     d tasaas
004500080829     d tasmgs
004600080829     d TASDSP                  1      8  0
004700080829     d taslnp
004800080829     d tasnrs
004900080829     d tasnsp
005000120720     d*
005100120720      *
005200120822     D fnlsv5ds        DS
005300120822     d* data inventario DAL/AL
005400080828     D  stainvrd              17     24  0
005500080828     D  cominvrd              25     32  0
005600080828     D  stainvra              33     40  0
005700080828     D  cominvra              41     48  0
005800120822     d* Data CONSEGNA
005900080828     D  stadsd                49     56  0
006000080828     D  comdsd                57     64  0
006100080828     D  stadsa                65     72  0
006200080828     D  comdsa                73     80  0
006300120822     d
006400080828     D  staccm                81     87  0
006500080829     D  staRAG                88    112
006600080902     D  stalnp               113    115  0
006700080902     D  stadlnp              116    140
006800110617     D  staemail             142    171
006900120822     d*
007000120822     d* data spedizione
007100120823     D  comsped              173    180  0
007200120823     D  comspea              181    188  0
007300120823     D  v1cinve              189    189
007400120823     D  v1cerra              190    190
007500120720     d* Data scannerizzazione
007600120704     D  comscand             191    198  0
007700120704     D  comscana             199    206  0
007800120704     d*   Per il momento le date restituzione al mittente sono oscurate e non
007900120704     d*    usate
008000120704     D  cominvimD            207    214  0
008100120704     D  cominvimA            215    222  0
008200120704     D  v1cinvim             223    223
008300120720     d*
008400120704     D  v1cscan              224    224
008500120704     D  v1csta               225    225
008600120720     d***
008700110617     D  pInFILE        s             10
008800110617     D  pInIDJOB       s             26
008900110617     D  pInPWDXLS      s             15
009000110617     D  pInNOMXLS      s             78
009100110617     D  pInEMLDEST     s            121
009200111207     D  pInEMLVAR      s             80
009300120509     D  pInMRAKE2      s             15
009400120509     D  pOutESITO      s              1
009500120828     d origaas         s                   like(tasaas)
009600120828     d origlnp         s                   like(taslnp)
009700120828     d orignrs         s                   like(tasnrs)
009800120828     d orignsp         s                   like(tasnsp)
009900120828     d savspe          s                   like(tasspe)
010000120824     d SQL_ar5uni      s                   like(ar5uni)
010100120829     d SQL_ar5aas      s                   like(ar5aas)
010200120829     d SQL_ar5lnp      s                   like(ar5lnp)
010300120829     d SQL_ar5nrs      s                   like(ar5nrs)
010400120829     d SQL_ar5nsp      s                   like(ar5nsp)
010500120824     d wmtvdt          s                   like(ddtmtvdt)
010600080829     d righe           s              3  0
010700110613     d dateu           s              8  0
010800120824     d comjdrid        s              8  0
010900110616     d unmese          s              8  0
011000110613     d ora_u           s              6  0
011100080829     d Wsta            s              1
011200120824     d Wsqlscan        s              1
011300120828     d Wfiglia         s              1
011400120829     d docfiglia       s              8
011500110613     d wlib            s             10    inz('GAITRAGRPS')
011600120405     d stasped         s              8  0
011700120405     d staspea         s              8  0
011800120823     d stascand        s              8  0
011900120823     d stascana        s              8  0
012000120424     d contarec        s              8  0
012100080828     d wrkgetlista     s           4096    varying
012200110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
012300110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
012400110613     d Ora_iso         s               t   timfmt(*iso)
012500110613     d Dataiso         s               d   datfmt(*iso)
012600080327     d Dataeur         s               d   datfmt(*eur)
012700080327     d Datadmy         s               d   datfmt(*dmy)
012800120720     d*
012900110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
013000110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
013100110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
013200110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
013300120823     d decINV_s        c                   const('  Stampa Inventario  AL:')
013400120823     d decCON          c                   const('   Data   consegna  DAL:')
013500120823     d decSPED         c                   const('   Data spedizione  DAL:')
013600120823     d decINV          c                   const('   Data inventario  DAL:')
013700120823     d decSCAN         c                   const('   Data scannerizz. DAL:')
013800080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
013900080909     D                                     ESTITUIRE AL MITTENTE *')
014000080909     d Inv             c                   const('  Documenti inventariati  ')
014100080909     d RIinv           c                   const('Documenti già inventariati')
014200080909     d NOinv           c                   const(' Documenti non inventariat-
014300080909     D                                     i per bolle consegnate ')
014400120424     d xlsE            c                   const('Indirizzo e-mail per +
014500120424     d                                     invio file excel:')
014600120424     d xlsF            c                   const('Excel troppo grande:+
014700120424     d                                     posto nella cartella')
014800080317      *------------------------------------------------------------------------*
014900080317      *   P R O T O T I P I
015000080317      *------------------------------------------------------------------------*
015100080829      /copy gaitrasrc/srcprotopr,tibs34r
015200110617      /copy gaitrasrc/srcprotopr,trtcm8r1
015300080829      *------------------------------------------------------------------------*
015400080829     Ifiar5000      81
015500080829     Ifiar5p00      82
015600001027     C*---------------------------------------------------------------*
015700001027     C     *ENTRY        PLIST
015800080325     C                   PARM                    kpjba
015900080317      /free
016000080828
016100120405
016200120823          clear wsta          ;
016300120823          clear totale        ;
016400120824          clear wsqlscan      ;
016500120405       select    ;
016600120822
016700120822       // INVENTARIATE MAI STAMPATE             ;
016800120822       when  v1cinve= 'I'     ;
016900120822          EXSR SQLINV         ;
017000120822
017100120822       // scannerizzate
017200120829       when  (v1cscan ='S'  or comscand>0) and
017300120824              cominvrd=0 and cominvra=0 and v1cerra=' '  and v1cinve=' ';
017400120824          wsqlscan='S'  ;
017500120824
017600120822          EXSR SQLSCAN     ;
017700120822
017800120822       other     ;
017900120405
018000120405       exsr  SQLGEN  ;
018100120822       endsl  ;
018200080901
018300080901       //  Esegui SQL  e stampa
018400080901          EXSR SQLesegui   ;
018500080901
018600110617
018700120828  1     if v1csta<>'S'         ;
018800120424        select   ;
018900120424        when *in01 and contarec <=2000  ;
019000120424        stainvio=xlsE  ;
019100120406        staEM =%trim(staemail)+'@brt.it' ;
019200120424
019300120424        when *in01 and contarec > 2000  ;
019400120424        stainvio=xlsF  ;
019500120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
019600120504                               '\File Excel\WFDDT00F'  ;
019700120424
019800120424        other ;
019900120406        staEM='NESSUNA bolla elaborata:file excel non creato';
020000120424        endsl  ;
020100120406
020200120828 2       if v1csta='X'  ;
020300120824         write  LsV6PT2         ;
020400120824 2       endif  ;
020500120824 1      endif  ;
020600120824
020700120828 1     if   v1csta<>'X'        ;
020800120824 2        if wsta=' '  ;
020900120824           staEM='NESSUNA bolla elaborata' ;
021000120824           write  LsV6PT2         ;
021100120824 x2       else   ;
021200120824
021300120824 3     if  righe>57           ;
021400120824       write  LsV6PT1         ;
021500120824       write  LsV6PT3         ;
021600120824 3     endif                  ;
021700120824
021800120824       write  LsV6Pfine       ;
021900120824  2     endif  ;
022000120824  1    endif  ;
022100120406
022200120424        // Se scritto almeno un record,
022300120424
022400120828  1     if *in01 and v1csta<>'S'        ;
022500120424
022600110617          pInIDJOB=knmeb       ;
022700120503          clear acorag  ;
022800120503  2       if  staccm>0   ;
022900120503          chain (1:dutkci:staccm) cnaco00f  ;
023000120503  3       if not %found(cnaco00f)  ;
023100120503            acorag=*all'?'  ;
023200120503  3       endif  ;
023300120503  2       endif ;
023400120503
023500120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
023600120503                       %subst(acorag:1:15)  ;
023700120424
023800120424        //      meno di 2000 record --> invio all'indirizzo e-mail
023900120424  1a      if contarec<= 2000 ;
024000120509            pInFILE='WFDDT00F' ;
024100120509            pInMRAKE2='XLS_E'  ;
024200120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
024300111207
024400120424  x1a     else  ;
024500120509          pInFILE='WFDDT00F' ;
024600120509            pInMRAKE2='XLS_F'  ;
024700120424  1       endif ;
024800120424
024900110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
025000120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
025100110617        endif  ;
025200080901
025300080829
025400080317        *inlr ='1'   ;
025500080901       //-------------------------------------------------------------------
025600080901       BEGSR  SQLINV                 ;
025700080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
025800080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
025900080901
026000120822        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
026100120822                %char(cominvra) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
026200120822                        and substr(ar5uni, 115, 1)='' ''';
026300080902
026400080902        if stalnp>0    ;
026500080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
026600080902        endif          ;
026700080902
026800080901        ENDSR     ;
026900120823       //-------------------------------------------------------------------
027000120823       BEGSR  SQLSCAN                ;
027100120823        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
027200120823                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''DOC''     '      ;
027300120823
027400120823        // Solo     scannerizzate senza data
027500120823        //    seleziono tutti i record DOC
027600120823
027700120823        // Scannerizzate con data
027800120829        if    comscana>0  ;
027900120823        wrkgetlista=wrkgetlista + ' and substr(ar5uni, 2, 8) <=''' +
028000120823                    %char(comscana) +''' and substr(ar5uni, 2, 8) >= ''' +
028100120823                    %char(comscand) + '''' ;
028200120823        endif          ;
028300120829        // Scannerizzate senza data
028400120829        if   v1cscan='S'  and comscana=0  ;
028500120829        wrkgetlista=wrkgetlista + ' and substr(ar5uni, 2, 8) <   ''99999999'' +
028600120829                                   and substr(ar5uni, 2, 8) > ''00000000''';
028700120829        endif          ;
028800120823
028900120823
029000120823        if stalnp>0    ;
029100120823        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
029200120823        endif          ;
029300120823
029400120823        ENDSR     ;
029500120405       //-------------------------------------------------------------------
029600120405       BEGSR  SQLGEN                 ;
029700120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
029800120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
029900120405
030000120405        // Solo NON inventariate
030100120405        select   ;
030200120405        when  v1cinve='E'  ;
030300120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
030400120405
030500120411        // Solo     inventariate  senza data
030600120411        when  v1cinve='S'  and cominvra=0  ;
030700120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
030800120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
030900120411        // Solo     inventariate  con   data
031000120411        when   cominvra>0  ;
031100120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
031200120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
031300120411                    %char(cominvrd) + '''' ;
031400120405        other  ;
031500120405        // tutte
031600120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
031700120405        endsl  ;
031800120405
031900120405        // solo con   MOTIVO DDT ERRATO
032000120405        if  v1cerra='S'  ;
032100120405        wrkgetlista=wrkgetlista + ' and +
032200120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
032300120405        endif  ;
032400120405        // solo senza MOTIVO DDT ERRATO
032500120405        if  v1cerra='E'  ;
032600120405        wrkgetlista=wrkgetlista + ' and +
032700120405                                    substr(ar5uni, 167, 8) =   ''        ''';
032800120405        endif  ;
032900120405
033000120405        if stalnp>0    ;
033100120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
033200120405        endif          ;
033300120405
033400120405        ENDSR     ;
033500120405
033600080829       //-------------------------------------------------------------------
033700080829       BEGSR  SQLesegui              ;
033800080828       EXEC sql  prepare s1 from : wrkgetlista ;
033900080828       EXEC sql  declare a1 cursor for s1      ;
034000080828       EXEC sql  open    a1                    ;
034100080828
034200120829       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
034300120829                                        :sql_ar5nsp, :sql_ar5uni           ;
034400080828
034500120424       clear contarec ;
034600120424
034700080828       dow sqlcod=0    ;
034800080828
034900080829       exsr STAMPA     ;
035000080829
035100120829       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
035200120829                                        :sql_ar5nsp, :sql_ar5uni           ;
035300080828       enddo           ;
035400080828
035500080829       EXEC sql  CLOSE   a1                    ;
035600080829
035700080828       ENDSR     ;
035800080327       //-------------------------------------------------------------------
035900080325       BEGSR  Stampa          ;
036000120828       clear wfiglia   ;
036100120828       clear origaas      ;
036200120828       clear origlnp      ;
036300120828       clear orignrs      ;
036400120828       clear orignsp      ;
036500120829       clear docfiglia  ;
036600080325
036700080325       // Cerco la bolla
036800120829       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  titas30c  ;
036900080901
037000120823 1     if %found(titas30c) ;
037100120406
037200120406       // Controllo la data spedizione se richiesta
037300120824       // Escludo se non ha la particolarità RESTITUZ.DDT
037400100603       clear ds7R ;
037500120829 2     if tasgma<>*blanks  ;
037600100603
037700100603         clear tblkey    ;
037800100603         tblkey=tasgma    ;
037900100603         chain (1:'7R':tblkey) tabel00f   ;
038000100603          if %found(tabel00f)   ;
038100100603          ds7r=tbluni       ;
038200100603          endif             ;
038300120829 2     endif             ;
038400100603
038500120829 2     if v1cinve='I'    or §7R1BF='B'   ;
038600080901
038700100528       // Escludo il cliente mittente se selezionato
038800120829 3     if staccm=0 or (staccm>0 and tasccm=staccm)     ;
038900100528
039000080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
039100120828       //  se sto elaborando le scannerizzate, stampo la mamma al posto
039200120828       //  della figlia
039300120828
039400120829       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  fnlbl01l  ;
039500120829 4     if  %found(fnlbl01l) and wsqlscan='S';
039600120828           wfiglia='S'   ;
039700120828           origaas=lblaao  ;
039800120828           origlnp=lbllpo  ;
039900120828           orignrs=lblnro  ;
040000120828           orignsp=lblnso  ;
040100120828           chain (origaas:origlnp:orignrs:orignsp)  titas30c  ;
040200120829 4     endif   ;
040300120828
040400120829 4     if (wsqlscan=' ' and not %found(fnlbl01l)) or
040500120829          (wsqlscan='S' )  ;
040600080902
040700120829 5     if  comsped=0 or
040800120829             (tasdsp>=comsped and tasdsp<=comspea) ;
040900120829
041000120829       // Controllo la data consegna  se richiesta
041100120829 6     if  comdsd=0 or
041200120829             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
041300120829
041400080829       stplnp=taslnp          ;
041500080829       stpnrs=tasnrs          ;
041600080327         if stpnrs>0            ;
041700080327         tra01='-'              ;
041800080327         else                   ;
041900080327         tra01=' '              ;
042000080327         endif                  ;
042100080829       stpnsp=tasnsp          ;
042200080829       dataiso=%date(tasdsp)    ;
042300080327       datadmy=dataiso        ;
042400080327       stpDSP=%dec(datadmy)   ;
042500080829       stplna=taslna          ;
042600080829       stpRMN=tasRMN          ;
042700080902
042800080902       // Data consegna        ;
042900080902       clear stpdcm  ;
043000080902       clear stpcca    ;
043100120406       clear ds7o      ;
043200120423       CLEAR LBLLAN  ;
043300080902
043400120823 7     if tasdcm>0   ;
043500080902       DATAEUR=%DATE(tasdcm:*iso)       ;
043600080902       stpdcm=%dec(dataeur)   ;
043700120829 7     endif  ;
043800120829
043900120829       clear ds7O ;
044000120829 7     if tascca<>' '  ;
044100080902         clear tblkey    ;
044200080902         tblkey=tascca    ;
044300080902         stpcca=tascca    ;
044400080902
044500080902         chain (1:'7O':tblkey) tabel00f   ;
044600080902          if %found(tabel00f)   ;
044700080902          ds7o=tbluni       ;
044800080902           %subst(stpcca:2:10)='-'+tbluni   ;
044900080902          endif       ;
045000120829 7     endif       ;
045100120406
045200120406       // Se la consegna anomala non è reso, ma dirottamento,
045300120406       //  verifico se c'e' un'altra figlia resa
045400120829 7     if §7ofdi='D'  ;
045500120829       lblaan=sql_ar5aas ;
045600120829       lbllpn=sql_ar5lnp ;
045700120829       lblnrn=sql_ar5nrs ;
045800120829       lblnsn=sql_ar5nsp ;
045900120406
046000120829
046100120823 9     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
046200120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
046300120823 10    if  %found(fnlbl02l)  ;
046400120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
046500120829       chain (lblaan:lbllpn:lblnrn:lblnsn:'DOC')  fiar531c  ;
046600120829       if %found(fiar531c) and docfiglia=*blanks ;
046700120829         docfiglia=%subst(ar5uni: 2: 8)  ;
046800120829       endif  ;
046900120829
047000120406         clear ds7o      ;
047100120823 11    if tascca<>' '  ;
047200120406         clear tblkey    ;
047300120406         tblkey=tascca    ;
047400120406         chain (1:'7O':tblkey) tabel00f   ;
047500120406          if %found(tabel00f)   ;
047600120406           ds7o=tbluni       ;
047700120406          endif  ;
047800120823 11    endif  ;
047900120823 10    endif  ;
048000120406
048100120823 9     enddo ;
048200120406
048300120828 9      if  wfiglia='S'  ;
048400120828         chain (origaas:origlnp:orignrs:orignsp)  titas30c       ;
048500120828        else  ;
048600120829         chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  titas30c  ;
048700120828 9      endif       ;
048800120823 7     endif       ;
048900080829
049000120823       // Escludo  delle non inventariate le bolle di reso
049100120823 7     if v1cinve='I'     or tascca=' '  or §7ores<>'S' ;
049200120824
049300120824 8     if  wsqlscan=' '       ;
049400120824       dar5gen=sql_ar5uni     ;
049500120824            clear  dAR5doc;
049600120829            chain  (sql_AR5aas : sql_AR5lnp : sql_AR5nrs : sql_AR5nsp : 'DOC')
049700120824                      FIAR531C;
049800120824 9          if  %found(FIAR531C);
049900120824               dAR5doc = AR5uni;
050000120824 9          endif  ;
050100120824
050200120824 x8    else    ;
050300120824
050400120824       dar5doc=sql_ar5uni     ;
050500120824            clear  dAR5gen;
050600120829            chain  (tasaas : taslnp : tasnrs : tasnsp : 'GEN')
050700120824                      FIAR531C;
050800120824 9          if  %found(FIAR531C);
050900120824               dAR5gen = AR5uni;
051000120824 9          endif  ;
051100120824
051200120824 8     endif                  ;
051300120824
051400120824         // -?Data scanneriz.immagini documenti?
051500120824
051600120824           clear comjdrid  ;
051700120824           clear STPdtScan;
051800120824           monitor   ;
051900120824           if  §ar5jdrid>*zeros  ;
052000120824               comjdrid=%int(§ar5jdrid) ;
052100120824               DataEUR   = %date( comjdrid : *iso );
052200120824               STPdtScan = %dec(DataEUR);
052300120824           endif  ;
052400120824           on-error  ;
052500120829           // se vuota la data scannerizzazione della mamma controllo se
052600120829           //  piena la figlia
052700120824           endmon  ;
052800120829           monitor   ;
052900120829           if  §ar5jdrid<=*zeros  and docfiglia>*zeros  ;
053000120829               comjdrid=%int(docfiglia) ;
053100120829               DataEUR   = %date( comjdrid : *iso );
053200120829               STPdtScan = %dec(DataEUR);
053300120829           endif  ;
053400120829           on-error  ;
053500120829           endmon  ;
053600120824
053700120824       // controllo le scannerizzate se richiesto e non già fatto con sql
053800120824           select    ;
053900120824 8         when  wsqlscan=' ' and comscana>0 ;
054000120824           // la data deve essere compresa
054100120824 9           if comjdrid<comscand or comjdrid>comscana  ;
054200120824              leavesr  ;
054300120824 9           endif   ;
054400120824
054500120824 8         when  wsqlscan=' ' and v1cscan<>' ';
054600120824           if  v1cscan='S' and comjdrid=0  ;
054700120824              leavesr  ;
054800120824 9           endif   ;
054900120824           if v1cscan='E' and comjdrid>0  ;
055000120824              leavesr  ;
055100120824 9           endif   ;
055200120824 8         endsl   ;
055300120824
055400080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
055500120823 8     IF %FOUND(TITA430C)    ;
055600080829       STPRMA=ta4NOT          ;
055700080829       ELSE                   ;
055800080829       clear stprma           ;
055900120823 8     endif                  ;
056000080829
056100080901       stprsd=tasrsd          ;
056200080829       stplod=taslod          ;
056300080829       %subst(stplod:13:3)=' '+tasprd    ;
056400080901
056500080901       // Data inventario      ;
056600120823 8     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
056700080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
056800080901         stpinv=%dec(dataeur)   ;
056900080829       else   ;
057000080901        clear stpinv    ;
057100120823 8     endif           ;
057200120720
057300120720               // -?Motivo ricezione DDT errati?
057400120824                 clear  STPdiMtv;
057500120823 8             if  §AR5mtvDt <> *blank;
057600120720                 STPmtvDt = §AR5mtvDt;
057700120824                  exsr DecoMDE   ;
057800120824                  stdmtvdt=wmtvdt  ;
057900120824                  Tra02    = '-';
058000120720
058100120720               // -?Data inserimento motivo ricezione DDT errati?
058200120824 9             if  §AR5diMtv > *zero ;
058300120720                 DataEUR  = %date( %int(§AR5diMtv) : *iso );
058400120824                 stpdimtv=%dec(dataeur)  ;
058500120824 9             endif  ;
058600120824
058700120824 x8            else;
058800120824                 clear  STPmtvDt;
058900120824                 clear  STdmtvDt;
059000120824                 clear  Tra02;
059100120824 8             endif;
059200080327
059300080325
059400120823  8    // Se richiesto stampo
059500120828       if v1csta<>'X'     ;
059600120406
059700080902       // totale bolle stampate
059800080902       totale=totale+1        ;
059900080902
060000080327       // Testata bolla
060100080327
060200120823       // la prima volta stampo la testata T2 con tutti i parametri
060300120823       //  richiesti
060400120823       if  righe>63           ;
060500120823       if wsta=' '    ;
060600120823       write  LsV6PT2         ;
060700120823       write  LsV6PT3         ;
060800120823       righe=15               ;
060900120823       else    ;
061000120823       write  LsV6PT1         ;
061100120823       write  LsV6PT3         ;
061200080328       righe=8                ;
061300120823       endif                  ;
061400120823       endif                  ;
061500080331
061600120720       write   LsV6PD1        ;
061700080327
061800080327       righe=righe+1          ;
061900080829       wsta='S'               ;
062000120823 8     endif  ;
062100080829
062200110613       // Se richiesto --> creo file
062300120828 8     if   v1csta<>'S'         ;
062400110613       clear wfddt000         ;
062500110616       ddtidute=knmeb   ;
062600110616       ddteut=knmus   ;
062700110613       ddtepo=dutpou  ;
062800110613       ddtedt=dateu   ;
062900110613       ddteor=ora_u   ;
063000110613       ddtccm=tasccm  ;
063100110616       // ragione sociale mittente
063200110616       if staccm=0  ;
063300110616       chain  (1:dutkci:tasccm) cnaco00f  ;
063400110616        if %found(cnaco00f)  ;
063500110616         ddtrsm=acorag   ;
063600110616        else  ;
063700110616        ddtrsm=*all'?' ;
063800110616        endif ;
063900110616       else  ;
064000110616        ddtrsm=acorag  ;
064100110616       endif  ;
064200110616
064300110613       ddtaas=tasaas  ;
064400110613       ddtlnp=taslnp  ;
064500110613       ddtnrs=tasnrs  ;
064600110613       ddtnsp=tasnsp  ;
064700110613       ddtmgs=tasmgs  ;
064800120423       IF LBLLAN>0    ;
064900120423        ddtlna=lbllan  ;
065000120423        ELSE   ;
065100120423        ddtlna=taslna  ;
065200120423        ENDIF  ;
065300110613       ddtrmn=tasrmn  ;
065400110613       ddtrma=stprma  ;
065500110613       ddtrsd=tasrsd  ;
065600110613       ddtlod=taslod  ;
065700110613       ddtcad=tascad  ;
065800110613       ddtprd=tasprd  ;
065900110613       ddtnzd=tasnzd  ;
066000110613       ddtdcm=tasdcm  ;
066100110613       ddtcca=tascca  ;
066200120320       // Area da organigramma
066300120423       chain ddtlna    azorg01l  ;
066400120320       if %found(azorg01l)  ;
066500120320       ddtcar=orgcar  ;
066600120320       endif  ;
066700120320
066800120823       if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
066900110616         ddtinv =%int(§ar5drdt)             ;
067000110616       else   ;
067100110616        clear ddtinv    ;
067200120823       endif           ;
067300120402
067400120402       // Motivazione DDt errato
067500120402       if §ar5mtvdt<>*blanks   ;
067600120824        exsr DecoMDE   ;
067700120824         ddtmtvdt=wmtvdt  ;
067800120402
067900120402 4       if §ar5dimtv>*zeros     ;
068000120402           ddtdimtv =%int(§ar5dimtv)             ;
068100120402         else   ;
068200120402          clear ddtdimtv  ;
068300120823         endif           ;
068400120823       endif           ;
068500120827
068600120827       // data scannerizzazione
068700120827       ddtjdrid=comjdrid    ;
068800120402
068900110613       write wfddt000   ;
069000120424
069100120424       contarec=contarec +1 ;
069200110616
069300110616       *in01 =*on  ;
069400120823 8     endif  ;
069500110613
069600080829       // Aggiorno fiar5 se stampa
069700120406
069800120823 8       if v1cinve='I' ;
069900080829       *in81=*off     ;
070000080829       *in82=*off     ;
070100120829       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp:'GEN')  fiar531c  ;
070200120823         if %found(fiar531c)    ;
070300080829          dar5gen=ar5uni      ;
070400080829          §ar5fstd='S'        ;
070500080829          ar5uni=dar5gen       ;
070600120823            if *in81      ;
070700080829            update fiar5000    ;
070800080829            else    ;
070900080829            update fiar5p00    ;
071000120823            endif   ;
071100120823         endif    ;
071200120823 8     endif    ;
071300080901
071400120823 7     endif    ;
071500120823 6     endif    ;
071600120823 5     endif    ;
071700120823 4     endif    ;
071800120823 3     endif    ;
071900120823 2     endif    ;
072000080902 1     endif    ;
072100080327
072200080325       ENDSR                  ;
072300120824       //-------------------------------------------------------------------
072400120824       BEGSR   DecoMDE  ;
072500120824       clear  tbeke1  ;
072600120824       clear  tbeke2  ;
072700120824       tbeke1=%subst(§ar5mtvdt:1:3)  ;
072800120824       tbeke2=%subst(§ar5mtvdt:4:5)  ;
072900120824
073000120824       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
073100120824        if %found(tntbe01l)  ;
073200120824         wmtvdt=tbeuni  ;
073300120824        endif   ;
073400120824        ENDSR  ;
073500080327       //-------------------------------------------------------------------
073600080327       BEGSR  *INZSR                 ;
073700080328
073800080328       in(E) *dtaara   ;
073900080328       if    %error or rsut  =*blanks  ;
074000080328       callp TIBS34R (TIBS34DS)        ;
074100080328       in    *dtaara   ;
074200080328       endif                           ;
074300080829
074400080829       // Apertura file di sede
074500080829             open(e) titas30c                ;
074600080829           if      not %open(titas30c)  ;
074700110613             %subst(WTA430:7:4)='GRU '    ;
074800110613             %subst(WTAs30:7:4)='GRU '    ;
074900110613             %subst(WTA531:7:4)='GRU '    ;
075000110617             %subst(WWFDDT:7:4)='AZM '    ;
075100110613             %subst(Wlib:7:4)='GRU '    ;
075200080829             open titas30c                ;
075300110617           endif  ;
075400080829             open tita430c                ;
075500080829             open fiar531c                ;
075600110617             open wfddt01l                ;
075700080829
075800120822       fnlsv5ds=kpjbu     ;
075900120405       clear stasped  ;
076000120405       clear staspea  ;
076100120823       clear stascand ;
076200120823       clear stascana ;
076300080829
076400120823       stptes   =tes1     ;
076500120823       stpdecinv=decCON   ;
076600120823       stpdec3  =decSPED  ;
076700120823       stpdec4  =decINV   ;
076800120823       stpdec5  =decSCAN  ;
076900080829
077000120823 1     if  v1cinve='I'  ;
077100120823       stpdecinv=decINV_s ;
077200120823       stpdec4  =decCON   ;
077300120720       %subst(stptes2:11:26)  =Inv      ;
077400120720       StpData1 = StaInvRa;
077500120720       clear    stpdata2  ;
077600120720       clear    stpdecinv2;
077700120823 1     endif              ;
077800120411
077900120823 1     if cominvrd>0  or cominvra>0 ;
078000120411
078100120823 2      if cominvrd>0  and cominvra=0 ;
078200120823         cominvra=cominvrd  ;
078300120823 2      Endif            ;
078400120823 2      if cominvrd=0               ;
078500120823         cominvrd=19900101  ;
078600120823 2      Endif            ;
078700120411
078800120824 2     if  v1cinve<>'I'  ;
078900120411 3     select     ;
079000120411       when stainvrd=0        ;
079100120411         clear    stpdata6  ;
079200120411         clear    stpdec44  ;
079300120411         %subst(stpdec4:21:4)='AL :'     ;
079400120411         stpdata5=stainvra  ;
079500120411
079600120411       when stainvra=0        ;
079700120411         clear    stpdata6  ;
079800120411         clear    stpdec44  ;
079900120411         %subst(stpdec4:21:4)='IL :'     ;
080000120411         stpdata5=stainvrd  ;
080100120411
080200120411       other                ;
080300120411         stpdata5=stainvrd  ;
080400120411         stpdata6=stainvra  ;
080500120411         stpdec44  = 'AL'   ;
080600120411 3      endSL              ;
080700120411 2     endif              ;
080800120823 1     endif              ;
080900080829
081000120411       // data CONSEGNA
081100120405 1     if comdsd>0  or comdsa>0   ;
081200120411
081300080901        if comdsd>0  and comdsa=0 ;
081400080901        comdsa=comdsd    ;
081500080901        Endif            ;
081600080901        if comdsd=0               ;
081700080901        comdsd=19900101  ;
081800080901        Endif            ;
081900080901
082000080901       select     ;
082100080901       when stadsd=0        ;
082200080901         clear    stpdata2  ;
082300080901         clear    stpdecinv2;
082400080901         %subst(stpdecinv:21:4)='AL :'     ;
082500080901         stpdata1=stadsa    ;
082600080901
082700080901       when stadsa=0        ;
082800080901         clear    stpdata2  ;
082900080901         clear    stpdecinv2;
083000080901         %subst(stpdecinv:21:4)='IL :'     ;
083100080901         stpdata1=stadsd    ;
083200080901
083300080901       other                ;
083400080901         stpdata1=stadsd    ;
083500080901         stpdata2=stadsa    ;
083600080901         stpdecinv2= 'AL'   ;
083700080901        endSL              ;
083800080901       endif              ;
083900120405
084000120405       // Data spedizione DAL/AL
084100120823
084200120405 1     if comsped>*zeros  or comspea>0   ;
084300120823       if comsped>0  ;
084400120823         dataiso=%date(comsped)  ;
084500120823         dataeur=dataiso;
084600120823         stasped=%dec(dataeur)  ;
084700120823       endif  ;
084800120823       if comspea>0  ;
084900120823         dataiso=%date(comspea)  ;
085000120823         dataeur=dataiso;
085100120823         staspea=%dec(dataeur)  ;
085200120823       endif  ;
085300120823
085400120824        if comsped>0  and comspea=0 ;
085500120824        comspea=comsped   ;
085600120824        Endif            ;
085700120405        if comsped=0               ;
085800120406        comsped=19900101  ;
085900120405        Endif            ;
086000120405
086100120405       // Date in ggmmaaaa
086200120405       select     ;
086300120405       when stasped=0        ;
086400120405         clear    stpdata4  ;
086500120405         clear    stpdec33  ;
086600120405         %subst(stpdec3:21:4)='AL :'     ;
086700120405         stpdata3=staspea   ;
086800120405
086900120405       when staspea=0        ;
087000120405         clear    stpdata4  ;
087100120405         clear    stpdec33  ;
087200120405         %subst(stpdec3:21:4)='IL :'     ;
087300120405         stpdata3=stasped   ;
087400120405
087500120405       other                ;
087600120406         stpdata3=stasped   ;
087700120406         stpdata4=staspea   ;
087800120405         stpdec33  = 'AL'   ;
087900120405        endSL              ;
088000120405       endif              ;
088100120823
088200120823       // Data scannerizzazione
088300120823
088400120823 1     if comscand>*zeros  or comscana>0   ;
088500120823 2     if comscand>0  ;
088600120823         dataiso=%date(comscand)  ;
088700120823         dataeur=dataiso;
088800120823         stascand=%dec(dataeur)  ;
088900120823 2     endif  ;
089000120823 2     if comscana>0  ;
089100120823         dataiso=%date(comscana)  ;
089200120823         dataeur=dataiso;
089300120823         stascana=%dec(dataeur)  ;
089400120823 2     endif  ;
089500120824 2      if comscand>0  and comscana=0 ;
089600120824        comscana=comscand   ;
089700120824 2      Endif            ;
089800120823 2      if comscand=0               ;
089900120823        comscand=19900101  ;
090000120823 2      Endif            ;
090100120823
090200120823       // Date in ggmmaaaa
090300120823 2     select     ;
090400120823       when stascand=0        ;
090500120823         clear    stpdata8  ;
090600120823         clear    stpdec55  ;
090700120823         %subst(stpdec5:21:4)='AL :'     ;
090800120823         stpdata7=staspea   ;
090900120823
091000120823       when stascana=0        ;
091100120823         clear    stpdata8  ;
091200120823         clear    stpdec55  ;
091300120823         %subst(stpdec5:21:4)='IL :'     ;
091400120823         stpdata3=stascand   ;
091500120823
091600120823       other                ;
091700120823         stpdata7=stascand   ;
091800120823         stpdata8=stascana   ;
091900120823         stpdec55  = 'AL'   ;
092000120823 2      endSL              ;
092100120823 1     endif              ;
092200080902
092300080902       // Linea di partenza
092400080902       if stalnp=0  ;
092500080902       stpdlnp='TUTTE' ;
092600080902       else           ;
092700080903       stpdlnp='-'+ stadlnp ;
092800080902       endif          ;
092900080902
093000080829       righe=70   ;
093100110613       // imposto data e ora del giorno
093200110613       dataiso=datasys  ;
093300110613       dateu=%dec(dataiso)   ;
093400110616
093500110616       dataiso=dataiso-%days(30) ;
093600110616       unmese=%dec(dataiso)   ;
093700110616
093800110613       ora_iso=ora_sys  ;
093900110613       ora_u=%dec(ora_iso)   ;
094000080829
094100110616       // Se richiesto file pulisco il lancio precedente per
094200110616       //  fil e ccm
094300110616       setll (dutpou: staccm)  wfddt01l   ;
094400110616       reade (dutpou: staccm)  wfddt01l   ;
094500110616
094600110616       if not %eof(wfddt01l)  and
094700120828          (v1csta<>'S'     or ddtedt<unmese) ;
094800110616
094900110616       dow  not %eof(wfddt01l)   ;
095000110616       delete wfddt000  ;
095100110616
095200110616       reade (dutpou: staccm)  wfddt01l   ;
095300110616       enddo  ;
095400110616        else  ;
095500110616        unlock   wfddt01l  ;
095600110616        endif  ;
095700110616
095800120405       // Ragione sociale Mittente
095900120828        if v1csta<>'S'    and staccm>0 ;
096000110616        chain  (1:dutkci:staccm) cnaco00f  ;
096100110616         if not %found(cnaco00f)  ;
096200110616          acorag=*all'?'  ;
096300110616         endif ;
096400110616       endif ;
096500120405
096600120405       // Bolle inventariate
096700120405       if v1cinve='S'   ;
096800120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
096900120405       endif  ;
097000120405       if v1cinve='E'   ;
097100120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
097200120405       endif  ;
097300120405       if v1cerra='S'   ;
097400120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
097500120405       endif  ;
097600120405       if v1cerra='E'   ;
097700120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
097800120405       endif  ;
097900120823       if v1cscan='S'   ;
098000120823        stpscan='SOLO BOLLE CON DOCUMENTI SCANNERIZZATI';
098100120823       endif  ;
098200120823       if v1cSCAN='E'   ;
098300120824        stpscan='SOLO BOLLE SENZA DOCUMENTI SCANNERIZZATI';
098400120823       endif  ;
098500120405
098600120823       // richiesta la stampa del file
098700120828 1     if v1csta='S'or v1csta='E' ;
098800120823        stasta='SI' ;
098900120823       else  ;
099000120823        stasta='NO' ;
099100120823 1     endif  ;
099200120823
099300120823       // richiesta file
099400120828 1     if v1csta='X'or v1csta='E' ;
099500120823        stacreaf='WFDDT00F'  ;
099600120823        ELSE  ;
099700120823        stacreaf='NO      '  ;
099800120823 1     endif  ;
099900110616
100000080328       ENDSR                           ;
100100080317      /end-free
