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
003800150831     d*dar5gen       e ds
003900120824     d dAR5doc       e ds                  inz
003901150911     d dAR5doc_m       ds                  likeds(dar5doc)
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
011400121026     d figliacca       s              1
011500120829     d docfiglia       s              8
011501150911     d drdtfiglia      s                   like(§ar5drdt)
011502150911     d dimtvfiglia     s                   like(§ar5dimtv)
011503150911     d mtvdtfiglia     s                   like(§ar5mtvdt)
011504150911     d comdrdt         s                   like(§ar5drdt)
011505150911     d commtvdt        s                   like(§ar5mtvdt)
011506150911     d comdimtv        s                   like(§ar5dimtv)
011600110613     d wlib            s             10    inz('GAITRAGRPS')
011700120405     d stasped         s              8  0
011800120405     d staspea         s              8  0
011900120823     d stascand        s              8  0
012000120823     d stascana        s              8  0
012100120424     d contarec        s              8  0
012200080828     d wrkgetlista     s           4096    varying
012300110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
012400110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
012500110613     d Ora_iso         s               t   timfmt(*iso)
012600110613     d Dataiso         s               d   datfmt(*iso)
012700080327     d Dataeur         s               d   datfmt(*eur)
012800080327     d Datadmy         s               d   datfmt(*dmy)
012900120720     d*
013000110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
013100110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
013200110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
013300110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
013400120823     d decINV_s        c                   const('  Stampa Inventario  AL:')
013500120823     d decCON          c                   const('   Data   consegna  DAL:')
013600120823     d decSPED         c                   const('   Data spedizione  DAL:')
013700120823     d decINV          c                   const('   Data inventario  DAL:')
013800120823     d decSCAN         c                   const('   Data scannerizz. DAL:')
013900080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
014000080909     D                                     ESTITUIRE AL MITTENTE *')
014100080909     d Inv             c                   const('  Documenti inventariati  ')
014200080909     d RIinv           c                   const('Documenti già inventariati')
014300080909     d NOinv           c                   const(' Documenti non inventariat-
014400080909     D                                     i per bolle consegnate ')
014500120424     d xlsE            c                   const('Indirizzo e-mail per +
014600120424     d                                     invio file excel:')
014700120424     d xlsF            c                   const('Excel troppo grande:+
014800120424     d                                     posto nella cartella')
014900080317      *------------------------------------------------------------------------*
015000080317      *   P R O T O T I P I
015100080317      *------------------------------------------------------------------------*
015200080829      /copy gaitrasrc/srcprotopr,tibs34r
015300110617      /copy gaitrasrc/srcprotopr,trtcm8r1
015400080829      *------------------------------------------------------------------------*
015500080829     Ifiar5000      81
015600080829     Ifiar5p00      82
015700001027     C*---------------------------------------------------------------*
015800001027     C     *ENTRY        PLIST
015900080325     C                   PARM                    kpjba
016000080317      /free
016100080828
016200120405
016300120823          clear wsta          ;
016400120823          clear totale        ;
016500120824          clear wsqlscan      ;
016600120405       select    ;
016700120822
016800120822       // INVENTARIATE MAI STAMPATE             ;
016900120822       when  v1cinve= 'I'     ;
017000120822          EXSR SQLINV         ;
017100120822
017200120822       // scannerizzate
017300120829       when  (v1cscan ='S'  or comscand>0) and
017400120824              cominvrd=0 and cominvra=0 and v1cerra=' '  and v1cinve=' ';
017500120824          wsqlscan='S'  ;
017600120824
017700120822          EXSR SQLSCAN     ;
017800120822
017900120822       other     ;
018000120405
018100120405       exsr  SQLGEN  ;
018200120822       endsl  ;
018300080901
018400080901       //  Esegui SQL  e stampa
018500080901          EXSR SQLesegui   ;
018600080901
018700110617
018800120828  1     if v1csta<>'S'         ;
018900120424        select   ;
019000120424        when *in01 and contarec <=2000  ;
019100120424        stainvio=xlsE  ;
019200120406        staEM =%trim(staemail)+'@brt.it' ;
019300120424
019400120424        when *in01 and contarec > 2000  ;
019500120424        stainvio=xlsF  ;
019600120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
019700120504                               '\File Excel\WFDDT00F'  ;
019800120424
019900120424        other ;
020000120406        staEM='NESSUNA bolla elaborata:file excel non creato';
020100120424        endsl  ;
020200120406
020300120828 2       if v1csta='X'  ;
020400120824         write  LsV6PT2         ;
020500120824 2       endif  ;
020600120824 1      endif  ;
020700120824
020800120828 1     if   v1csta<>'X'        ;
020900120824 2        if wsta=' '  ;
021000120824           staEM='NESSUNA bolla elaborata' ;
021100120824           write  LsV6PT2         ;
021200120824 x2       else   ;
021300120824
021400120824 3     if  righe>57           ;
021500120824       write  LsV6PT1         ;
021600120824       write  LsV6PT3         ;
021700120824 3     endif                  ;
021800120824
021900120824       write  LsV6Pfine       ;
022000120824  2     endif  ;
022100120824  1    endif  ;
022200120406
022300120424        // Se scritto almeno un record,
022400120424
022500120828  1     if *in01 and v1csta<>'S'        ;
022600120424
022700110617          pInIDJOB=knmeb       ;
022800120503          clear acorag  ;
022900120503  2       if  staccm>0   ;
023000120503          chain (1:dutkci:staccm) cnaco00f  ;
023100120503  3       if not %found(cnaco00f)  ;
023200120503            acorag=*all'?'  ;
023300120503  3       endif  ;
023400120503  2       endif ;
023500120503
023600120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
023700120503                       %subst(acorag:1:15)  ;
023800120424
023900120424        //      meno di 2000 record --> invio all'indirizzo e-mail
024000120424  1a      if contarec<= 2000 ;
024100120509            pInFILE='WFDDT00F' ;
024200120509            pInMRAKE2='XLS_E'  ;
024300120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
024400111207
024500120424  x1a     else  ;
024600120509          pInFILE='WFDDT00F' ;
024700120509            pInMRAKE2='XLS_F'  ;
024800120424  1       endif ;
024900120424
025000110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
025100120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
025200110617        endif  ;
025300080901
025400080829
025500080317        *inlr ='1'   ;
025600080901       //-------------------------------------------------------------------
025700080901       BEGSR  SQLINV                 ;
025800080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
025900150831                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''DOC'' and '      ;
026000080901
026100150831        wrkgetlista=wrkgetlista + 'substr(ar5uni, 18, 8) <=  ''' +
026200150831                %char(cominvra) +''' and substr(ar5uni, 18, 8) > ''00000000'' +
026300150831                        and substr(ar5uni, 26, 1)='' ''';
026400080902
026500080902        if stalnp>0    ;
026600080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
026700080902        endif          ;
026800080902
026900080901        ENDSR     ;
027000120823       //-------------------------------------------------------------------
027100120823       BEGSR  SQLSCAN                ;
027200120823        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
027300120823                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''DOC''     '      ;
027400120823
027500120823        // Solo     scannerizzate senza data
027600120823        //    seleziono tutti i record DOC
027700120823
027800120823        // Scannerizzate con data
027900120829        if    comscana>0  ;
028000120823        wrkgetlista=wrkgetlista + ' and substr(ar5uni, 2, 8) <=''' +
028100120823                    %char(comscana) +''' and substr(ar5uni, 2, 8) >= ''' +
028200120823                    %char(comscand) + '''' ;
028300120823        endif          ;
028400120829        // Scannerizzate senza data
028500120829        if   v1cscan='S'  and comscana=0  ;
028600120829        wrkgetlista=wrkgetlista + ' and substr(ar5uni, 2, 8) <   ''99999999'' +
028700120829                                   and substr(ar5uni, 2, 8) > ''00000000''';
028800120829        endif          ;
028900120823
029000120823
029100120823        if stalnp>0    ;
029200120823        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
029300120823        endif          ;
029400120823
029500120823        ENDSR     ;
029600120405       //-------------------------------------------------------------------
029700120405       BEGSR  SQLGEN                 ;
029800120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
029900150831                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''DOC'' and '      ;
030000120405
030100120405        // Solo NON inventariate
030200120405        select   ;
030300120405        when  v1cinve='E'  ;
030400150831        wrkgetlista=wrkgetlista + 'substr(ar5uni, 18, 8) = ''99999999''' ;
030500120405
030600120411        // Solo     inventariate  senza data
030700120411        when  v1cinve='S'  and cominvra=0  ;
030800150831        wrkgetlista=wrkgetlista + ' substr(ar5uni, 18, 8) <   ''99999999'' +
030900150831                                   and substr(ar5uni, 18, 8) > ''00000000''';
031000120411        // Solo     inventariate  con   data
031100120411        when   cominvra>0  ;
031200150831        wrkgetlista=wrkgetlista + ' substr(ar5uni, 18, 8) <=''' +
031300150831                    %char(cominvra) +''' and substr(ar5uni, 18, 8) >= ''' +
031400120411                    %char(cominvrd) + '''' ;
031500120405        other  ;
031600120405        // tutte
031700150831        wrkgetlista=wrkgetlista + ' substr(ar5uni, 18, 8) <>  ''        ''' ;
031800120405        endsl  ;
031900120405
032000120405        // solo con   MOTIVO DDT ERRATO
032100120405        if  v1cerra='S'  ;
032200120405        wrkgetlista=wrkgetlista + ' and +
032300150831                                    substr(ar5uni, 27, 8) <>  ''        ''';
032400120405        endif  ;
032500120405        // solo senza MOTIVO DDT ERRATO
032600120405        if  v1cerra='E'  ;
032700120405        wrkgetlista=wrkgetlista + ' and +
032800150831                                    substr(ar5uni, 27, 8) =   ''        ''';
032900120405        endif  ;
033000120405
033100120405        if stalnp>0    ;
033200120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
033300120405        endif          ;
033400120405
033500120405        ENDSR     ;
033600120405
033700080829       //-------------------------------------------------------------------
033800080829       BEGSR  SQLesegui              ;
033900080828       EXEC sql  prepare s1 from : wrkgetlista ;
034000080828       EXEC sql  declare a1 cursor for s1      ;
034100080828       EXEC sql  open    a1                    ;
034200080828
034300120829       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
034400120829                                        :sql_ar5nsp, :sql_ar5uni           ;
034500080828
034600120424       clear contarec ;
034700120424
034800080828       dow sqlcod=0    ;
034900080828
035000080829       exsr STAMPA     ;
035100080829
035200120829       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
035300120829                                        :sql_ar5nsp, :sql_ar5uni           ;
035400080828       enddo           ;
035500080828
035600080829       EXEC sql  CLOSE   a1                    ;
035700080829
035800080828       ENDSR     ;
035900080327       //-------------------------------------------------------------------
036000080325       BEGSR  Stampa          ;
036100120828       clear wfiglia   ;
036200120828       clear origaas      ;
036300120828       clear origlnp      ;
036400120828       clear orignrs      ;
036500120828       clear orignsp      ;
036600120829       clear docfiglia  ;
036700121026       clear figliacca  ;
036701150910       clear drdtfiglia ;
036702150910       clear dimtvfiglia ;
036703150910       clear mtvdtfiglia ;
036800080325
036900080325       // Cerco la bolla
037000120829       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  titas30c  ;
037100080901
037200120823 1     if %found(titas30c) ;
037300120406
037400120406       // Controllo la data spedizione se richiesta
037500120824       // Escludo se non ha la particolarità RESTITUZ.DDT
037600100603       clear ds7R ;
037700120829 2     if tasgma<>*blanks  ;
037800100603
037900100603         clear tblkey    ;
038000100603         tblkey=tasgma    ;
038100100603         chain (1:'7R':tblkey) tabel00f   ;
038200100603          if %found(tabel00f)   ;
038300100603          ds7r=tbluni       ;
038400100603          endif             ;
038500120829 2     endif             ;
038600100603
038700120829 2     if v1cinve='I'    or §7R1BF='B'   ;
038800080901
038900100528       // Escludo il cliente mittente se selezionato
039000120829 3     if staccm=0 or (staccm>0 and tasccm=staccm)     ;
039100100528
039200080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
039300120828       //  se sto elaborando le scannerizzate, stampo la mamma al posto
039400120828       //  della figlia
039500120828
039600120829       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  fnlbl01l  ;
039700120829 4     if  %found(fnlbl01l) and wsqlscan='S';
039800120828           wfiglia='S'   ;
039900120828           origaas=lblaao  ;
040000120828           origlnp=lbllpo  ;
040100120828           orignrs=lblnro  ;
040200120828           orignsp=lblnso  ;
040300120828           chain (origaas:origlnp:orignrs:orignsp)  titas30c  ;
040400120829 4     endif   ;
040500120828
040600120829 4     if (wsqlscan=' ' and not %found(fnlbl01l)) or
040700120829          (wsqlscan='S' )  ;
040800080902
040900120829 5     if  comsped=0 or
041000120829             (tasdsp>=comsped and tasdsp<=comspea) ;
041100120829
041200120829       // Controllo la data consegna  se richiesta
041300120829 6     if  comdsd=0 or
041400120829             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
041500120829
041600080829       stplnp=taslnp          ;
041700080829       stpnrs=tasnrs          ;
041800080327         if stpnrs>0            ;
041900080327         tra01='-'              ;
042000080327         else                   ;
042100080327         tra01=' '              ;
042200080327         endif                  ;
042300080829       stpnsp=tasnsp          ;
042400080829       dataiso=%date(tasdsp)    ;
042500080327       datadmy=dataiso        ;
042600080327       stpDSP=%dec(datadmy)   ;
042700080829       stplna=taslna          ;
042800080829       stpRMN=tasRMN          ;
042900080902
043000080902       // Data consegna        ;
043100080902       clear stpdcm  ;
043200080902       clear stpcca    ;
043300120406       clear ds7o      ;
043400120423       CLEAR LBLLAN  ;
043500080902
043600120823 7     if tasdcm>0   ;
043700080902       DATAEUR=%DATE(tasdcm:*iso)       ;
043800080902       stpdcm=%dec(dataeur)   ;
043900120829 7     endif  ;
044000120829
044100120829       clear ds7O ;
044200120829 7     if tascca<>' '  ;
044300080902         clear tblkey    ;
044400080902         tblkey=tascca    ;
044500080902         stpcca=tascca    ;
044600080902
044700080902         chain (1:'7O':tblkey) tabel00f   ;
044800080902          if %found(tabel00f)   ;
044900080902          ds7o=tbluni       ;
045000080902           %subst(stpcca:2:10)='-'+tbluni   ;
045100080902          endif       ;
045200120829 7     endif       ;
045300120406
045400120406       // Se la consegna anomala non è reso, ma dirottamento,
045500121025       //  verifico se c'e' un'altra figlia resa/ o con 5-idd3
045600120829 7     if §7ofdi='D'  ;
045700120829       lblaan=sql_ar5aas ;
045800120829       lbllpn=sql_ar5lnp ;
045900120829       lblnrn=sql_ar5nrs ;
046000120829       lblnsn=sql_ar5nsp ;
046100120406
046200120829
046300121025 9     dou not %found(fnlbl02l) or  §7ores= 'S'  or tascca='5' ;
046400120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
046500120823 10    if  %found(fnlbl02l)  ;
046600120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
046700120829       chain (lblaan:lbllpn:lblnrn:lblnsn:'DOC')  fiar531c  ;
046800120829       if %found(fiar531c) and docfiglia=*blanks ;
046900120829         docfiglia=%subst(ar5uni: 2: 8)  ;
047000120829       endif  ;
047100120829
047200120406         clear ds7o      ;
047300121026         eval figliacca=tascca  ;
047400120823 11    if tascca<>' '  ;
047500120406         clear tblkey    ;
047600120406         tblkey=tascca    ;
047700120406         chain (1:'7O':tblkey) tabel00f   ;
047800120406          if %found(tabel00f)   ;
047900120406           ds7o=tbluni       ;
048000120406          endif  ;
048100120823 11    endif  ;
048200120823 10    endif  ;
048300120406
048400120823 9     enddo ;
048500120406
048600120828 9      if  wfiglia='S'  ;
048700120828         chain (origaas:origlnp:orignrs:orignsp)  titas30c       ;
048800120828        else  ;
048900120829         chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp)  titas30c  ;
049000120828 9      endif       ;
049100120823 7     endif       ;
049200080829
049300120823       // Escludo  delle non inventariate le bolle di reso
049400121026 7     if v1cinve='I'     or tascca=' '  or (§7ores<>'S' and figliacca<>'5'
049500121029                                            and tascca<>'5');
049600120824
049700120824 8     if  wsqlscan=' '       ;
049800150831       //dar5gen=sql_ar5uni     ;
049900150831       //   clear  dAR5doc;
050000150831       //   chain  (sql_AR5aas : sql_AR5lnp : sql_AR5nrs : sql_AR5nsp : 'DOC')
050100150831       //             FIAR531C;
050200150831 9     //   if  %found(FIAR531C);
050300150831       //      dAR5doc = AR5uni;
050400150831 9     //   endif  ;
050401150831         dar5doc=sql_ar5uni     ;
050500120824
050600120824 x8    else    ;
050700120824
050800120824       dar5doc=sql_ar5uni     ;
050802150831         if wfiglia='S';
050900150831            clear  dAR5doc_m;
051000150831            chain  (tasaas : taslnp : tasnrs : tasnsp : 'DOC')
051100120824                      FIAR531C;
051200120824 9          if  %found(FIAR531C);
051300150831               dAR5doc_m = AR5uni;
051301150831               drdtfiglia=dar5doc_m.§ar5drdt;
051302150910               dimtvfiglia=dar5doc_m.§ar5dimtv;
051303150911               mtvdtfiglia=dar5doc_m.§ar5mtvdt;
051400120824 9          endif  ;
051401150831         endif;
051500120824
051600120824 8     endif                  ;
051700120824
051800120824         // -?Data scanneriz.immagini documenti?
051900120824
052000120824           clear comjdrid  ;
052100120824           clear STPdtScan;
052200120824           monitor   ;
052300120824           if  §ar5jdrid>*zeros  ;
052400120824               comjdrid=%int(§ar5jdrid) ;
052500120824               DataEUR   = %date( comjdrid : *iso );
052600120824               STPdtScan = %dec(DataEUR);
052700120824           endif  ;
052800120824           on-error  ;
052900120829           // se vuota la data scannerizzazione della mamma controllo se
053000120829           //  piena la figlia
053100120824           endmon  ;
053200120829           monitor   ;
053300120829           if  §ar5jdrid<=*zeros  and docfiglia>*zeros  ;
053400120829               comjdrid=%int(docfiglia) ;
053500120829               DataEUR   = %date( comjdrid : *iso );
053600120829               STPdtScan = %dec(DataEUR);
053700120829           endif  ;
053800120829           on-error  ;
053900120829           endmon  ;
054000120824
054100120824       // controllo le scannerizzate se richiesto e non già fatto con sql
054200120824           select    ;
054300120824 8         when  wsqlscan=' ' and comscana>0 ;
054400120824           // la data deve essere compresa
054500120824 9           if comjdrid<comscand or comjdrid>comscana  ;
054600120824              leavesr  ;
054700120824 9           endif   ;
054800120824
054900120824 8         when  wsqlscan=' ' and v1cscan<>' ';
055000120824           if  v1cscan='S' and comjdrid=0  ;
055100120824              leavesr  ;
055200120824 9           endif   ;
055300120824           if v1cscan='E' and comjdrid>0  ;
055400120824              leavesr  ;
055500120824 9           endif   ;
055600120824 8         endsl   ;
055700120824
055800120913       chain (tasaas:taslnp:tasnrs:tasnsp:'A')  tita430c  ;
055900120823 8     IF %FOUND(TITA430C)    ;
056000080829       STPRMA=ta4NOT          ;
056100080829       ELSE                   ;
056200080829       clear stprma           ;
056300120823 8     endif                  ;
056400080829
056500080901       stprsd=tasrsd          ;
056600080829       stplod=taslod          ;
056700080829       %subst(stplod:13:3)=' '+tasprd    ;
056800080901
056900080901       // Data inventario      ;
056901150831       clear comdrdt;
056903150911       if wfiglia='S';
056904150911          comdrdt=drdtfiglia;
056905150911       else;
056906150911          comdrdt=§ar5drdt;
056907150911       endif;
057000150911 8     if comdrdt>*zeros and comdrdt<>'99999999'    ;
057100150911         DATAEUR=%DATE(%int(comdrdt):*iso)       ;
057200150911         stpinv=%dec(dataeur)   ;
057300150911       else   ;
057400150911        clear stpinv    ;
057500150911 8     endif           ;
057600120720
057700120720               // -?Motivo ricezione DDT errati?
057800120824                 clear  STPdiMtv;
057801150911                 clear commtvdt;
057802150911                 clear comdimtv;
057803150911               if wfiglia='S';
057804150911                  commtvdt=mtvdtfiglia;
057805150911                  comdimtv=dimtvfiglia;
057806150911               else;
057807150911                  commtvdt=§ar5mtvdt  ;
057808150911                  comdimtv=§ar5dimtv   ;
057809150911               endif;
057900150911 8             if  commtvdt <> *blank;
058004150911                  STPmtvDt= commtvdt;
058100120824                  exsr DecoMDE   ;
058200120824                  stdmtvdt=wmtvdt  ;
058300120824                  Tra02    = '-';
058400120720
058500120720               // -?Data inserimento motivo ricezione DDT errati?
058600150911 9             if  comdimtv  > *zero ;
058700150911                 DataEUR  = %date( %int(comdimtv) : *iso );
058800120824                 stpdimtv=%dec(dataeur)  ;
058900120824 9             endif  ;
059000120824
059100120824 x8            else;
059200120824                 clear  STPmtvDt;
059300120824                 clear  STdmtvDt;
059400120824                 clear  Tra02;
059500120824 8             endif;
059600080327
059700080325
059800120823  8    // Se richiesto stampo
059900120828       if v1csta<>'X'     ;
060000120406
060100080902       // totale bolle stampate
060200080902       totale=totale+1        ;
060300080902
060400080327       // Testata bolla
060500080327
060600120823       // la prima volta stampo la testata T2 con tutti i parametri
060700120823       //  richiesti
060800120823       if  righe>63           ;
060900120823       if wsta=' '    ;
061000120823       write  LsV6PT2         ;
061100120823       write  LsV6PT3         ;
061200120823       righe=15               ;
061300120823       else    ;
061400120823       write  LsV6PT1         ;
061500120823       write  LsV6PT3         ;
061600080328       righe=8                ;
061700120823       endif                  ;
061800120823       endif                  ;
061900080331
062000120720       write   LsV6PD1        ;
062100080327
062200080327       righe=righe+1          ;
062300080829       wsta='S'               ;
062400120823 8     endif  ;
062500080829
062600110613       // Se richiesto --> creo file
062700120828 8     if   v1csta<>'S'         ;
062800110613       clear wfddt000         ;
062900110616       ddtidute=knmeb   ;
063000110616       ddteut=knmus   ;
063100110613       ddtepo=dutpou  ;
063200110613       ddtedt=dateu   ;
063300110613       ddteor=ora_u   ;
063400110613       ddtccm=tasccm  ;
063500110616       // ragione sociale mittente
063600110616       if staccm=0  ;
063700110616       chain  (1:dutkci:tasccm) cnaco00f  ;
063800110616        if %found(cnaco00f)  ;
063900110616         ddtrsm=acorag   ;
064000110616        else  ;
064100110616        ddtrsm=*all'?' ;
064200110616        endif ;
064300110616       else  ;
064400110616        ddtrsm=acorag  ;
064500110616       endif  ;
064600110616
064700110613       ddtaas=tasaas  ;
064800110613       ddtlnp=taslnp  ;
064900110613       ddtnrs=tasnrs  ;
065000110613       ddtnsp=tasnsp  ;
065100110613       ddtmgs=tasmgs  ;
065200120423       IF LBLLAN>0    ;
065300120423        ddtlna=lbllan  ;
065400120423        ELSE   ;
065500120423        ddtlna=taslna  ;
065600120423        ENDIF  ;
065700110613       ddtrmn=tasrmn  ;
065800110613       ddtrma=stprma  ;
065900110613       ddtrsd=tasrsd  ;
066000110613       ddtlod=taslod  ;
066100110613       ddtcad=tascad  ;
066200110613       ddtprd=tasprd  ;
066300110613       ddtnzd=tasnzd  ;
066400110613       ddtdcm=tasdcm  ;
066500110613       ddtcca=tascca  ;
066600120320       // Area da organigramma
066700120423       chain ddtlna    azorg01l  ;
066800120320       if %found(azorg01l)  ;
066900120320       ddtcar=orgcar  ;
067000120320       endif  ;
067100120320
067200150831       //if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
067300150831       //  ddtinv =%int(§ar5drdt)             ;
067301150911       if comdrdt >*zeros and comdrdt<>'99999999'    ;
067302150911           ddtinv =%int(comdrdt)              ;
067400110616       else   ;
067500110616        clear ddtinv    ;
067600120823       endif           ;
067700120402
067800120402       // Motivazione DDt errato
067900150831       //if §ar5mtvdt<>*blanks   ;
067901150831       if STPmtvdt<>*blanks   ;
068000120824        exsr DecoMDE   ;
068100120824         ddtmtvdt=wmtvdt  ;
068200120402
068300150831 4     //  if §ar5dimtv>*zeros     ;
068400150831       //  ddtdimtv =%int(§ar5dimtv)             ;
068401150911 4       if comdimtv >*zeros     ;
068402150911           ddtdimtv =%int(comdimtv)             ;
068500120402         else   ;
068600120402          clear ddtdimtv  ;
068700120823         endif           ;
068800120823       endif           ;
068900120827
069000120827       // data scannerizzazione
069100150910       ddtjdrid=comjdrid    ;
069200120402
069300110613       write wfddt000   ;
069400120424
069500120424       contarec=contarec +1 ;
069600110616
069700110616       *in01 =*on  ;
069800120823 8     endif  ;
069900110613
070000080829       // Aggiorno fiar5 se stampa
070100120406
070200120823 8       if v1cinve='I' ;
070300080829       *in81=*off     ;
070400080829       *in82=*off     ;
070500150831       chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp:'DOC')  fiar531c  ;
070600120823         if %found(fiar531c)    ;
070700150831          dar5doc=ar5uni      ;
070800080829          §ar5fstd='S'        ;
070900150831          ar5uni=dar5doc       ;
071000120823            if *in81      ;
071100080829            update fiar5000    ;
071200080829            else    ;
071300080829            update fiar5p00    ;
071400120823            endif   ;
071500120823         endif    ;
071600120823 8     endif    ;
071700080901
071800120823 7     endif    ;
071900120823 6     endif    ;
072000120823 5     endif    ;
072100120823 4     endif    ;
072200120823 3     endif    ;
072300120823 2     endif    ;
072400080902 1     endif    ;
072500080327
072600080325       ENDSR                  ;
072700120824       //-------------------------------------------------------------------
072800120824       BEGSR   DecoMDE  ;
072900120824       clear  tbeke1  ;
073000120824       clear  tbeke2  ;
073100150831       //tbeke1=%subst(§ar5mtvdt:1:3)  ;
073200150831       //tbeke2=%subst(§ar5mtvdt:4:5)  ;
073201150831         tbeke1=%subst(STPmtvdt:1:3)  ;
073202150831         tbeke2=%subst(STPmtvdt:4:5)  ;
073300120824
073400120824       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
073500120824        if %found(tntbe01l)  ;
073600120824         wmtvdt=tbeuni  ;
073700120824        endif   ;
073800120824        ENDSR  ;
073900080327       //-------------------------------------------------------------------
074000080327       BEGSR  *INZSR                 ;
074100080328
074200080328       in(E) *dtaara   ;
074300080328       if    %error or rsut  =*blanks  ;
074400080328       callp TIBS34R (TIBS34DS)        ;
074500080328       in    *dtaara   ;
074600080328       endif                           ;
074700080829
074800080829       // Apertura file di sede
074900080829             open(e) titas30c                ;
075000080829           if      not %open(titas30c)  ;
075100110613             %subst(WTA430:7:4)='GRU '    ;
075200110613             %subst(WTAs30:7:4)='GRU '    ;
075300110613             %subst(WTA531:7:4)='GRU '    ;
075400110617             %subst(WWFDDT:7:4)='AZM '    ;
075500110613             %subst(Wlib:7:4)='GRU '    ;
075600080829             open titas30c                ;
075700110617           endif  ;
075800080829             open tita430c                ;
075900080829             open fiar531c                ;
076000110617             open wfddt01l                ;
076100080829
076200120822       fnlsv5ds=kpjbu     ;
076300120405       clear stasped  ;
076400120405       clear staspea  ;
076500120823       clear stascand ;
076600120823       clear stascana ;
076700080829
076800120823       stptes   =tes1     ;
076900120823       stpdecinv=decCON   ;
077000120823       stpdec3  =decSPED  ;
077100120823       stpdec4  =decINV   ;
077200120823       stpdec5  =decSCAN  ;
077300080829
077400120823 1     if  v1cinve='I'  ;
077500120823       stpdecinv=decINV_s ;
077600120823       stpdec4  =decCON   ;
077700120720       %subst(stptes2:11:26)  =Inv      ;
077800120720       StpData1 = StaInvRa;
077900120720       clear    stpdata2  ;
078000120720       clear    stpdecinv2;
078100120823 1     endif              ;
078200120411
078300120823 1     if cominvrd>0  or cominvra>0 ;
078400120411
078500120823 2      if cominvrd>0  and cominvra=0 ;
078600120823         cominvra=cominvrd  ;
078700120823 2      Endif            ;
078800120823 2      if cominvrd=0               ;
078900120823         cominvrd=19900101  ;
079000120823 2      Endif            ;
079100120411
079200120824 2     if  v1cinve<>'I'  ;
079300120411 3     select     ;
079400120411       when stainvrd=0        ;
079500120411         clear    stpdata6  ;
079600120411         clear    stpdec44  ;
079700120411         %subst(stpdec4:21:4)='AL :'     ;
079800120411         stpdata5=stainvra  ;
079900120411
080000120411       when stainvra=0        ;
080100120411         clear    stpdata6  ;
080200120411         clear    stpdec44  ;
080300120411         %subst(stpdec4:21:4)='IL :'     ;
080400120411         stpdata5=stainvrd  ;
080500120411
080600120411       other                ;
080700120411         stpdata5=stainvrd  ;
080800120411         stpdata6=stainvra  ;
080900120411         stpdec44  = 'AL'   ;
081000120411 3      endSL              ;
081100120411 2     endif              ;
081200120823 1     endif              ;
081300080829
081400120411       // data CONSEGNA
081500120405 1     if comdsd>0  or comdsa>0   ;
081600120411
081700080901        if comdsd>0  and comdsa=0 ;
081800080901        comdsa=comdsd    ;
081900080901        Endif            ;
082000080901        if comdsd=0               ;
082100080901        comdsd=19900101  ;
082200080901        Endif            ;
082300080901
082400080901       select     ;
082500080901       when stadsd=0        ;
082600080901         clear    stpdata2  ;
082700080901         clear    stpdecinv2;
082800080901         %subst(stpdecinv:21:4)='AL :'     ;
082900080901         stpdata1=stadsa    ;
083000080901
083100080901       when stadsa=0        ;
083200080901         clear    stpdata2  ;
083300080901         clear    stpdecinv2;
083400080901         %subst(stpdecinv:21:4)='IL :'     ;
083500080901         stpdata1=stadsd    ;
083600080901
083700080901       other                ;
083800080901         stpdata1=stadsd    ;
083900080901         stpdata2=stadsa    ;
084000080901         stpdecinv2= 'AL'   ;
084100080901        endSL              ;
084200080901       endif              ;
084300120405
084400120405       // Data spedizione DAL/AL
084500120823
084600120405 1     if comsped>*zeros  or comspea>0   ;
084700120823       if comsped>0  ;
084800120823         dataiso=%date(comsped)  ;
084900120823         dataeur=dataiso;
085000120823         stasped=%dec(dataeur)  ;
085100120823       endif  ;
085200120823       if comspea>0  ;
085300120823         dataiso=%date(comspea)  ;
085400120823         dataeur=dataiso;
085500120823         staspea=%dec(dataeur)  ;
085600120823       endif  ;
085700120823
085800120824        if comsped>0  and comspea=0 ;
085900120824        comspea=comsped   ;
086000120824        Endif            ;
086100120405        if comsped=0               ;
086200120406        comsped=19900101  ;
086300120405        Endif            ;
086400120405
086500120405       // Date in ggmmaaaa
086600120405       select     ;
086700120405       when stasped=0        ;
086800120405         clear    stpdata4  ;
086900120405         clear    stpdec33  ;
087000120405         %subst(stpdec3:21:4)='AL :'     ;
087100120405         stpdata3=staspea   ;
087200120405
087300120405       when staspea=0        ;
087400120405         clear    stpdata4  ;
087500120405         clear    stpdec33  ;
087600120405         %subst(stpdec3:21:4)='IL :'     ;
087700120405         stpdata3=stasped   ;
087800120405
087900120405       other                ;
088000120406         stpdata3=stasped   ;
088100120406         stpdata4=staspea   ;
088200120405         stpdec33  = 'AL'   ;
088300120405        endSL              ;
088400120405       endif              ;
088500120823
088600120823       // Data scannerizzazione
088700120823
088800120823 1     if comscand>*zeros  or comscana>0   ;
088900120823 2     if comscand>0  ;
089000120823         dataiso=%date(comscand)  ;
089100120823         dataeur=dataiso;
089200120823         stascand=%dec(dataeur)  ;
089300120823 2     endif  ;
089400120823 2     if comscana>0  ;
089500120823         dataiso=%date(comscana)  ;
089600120823         dataeur=dataiso;
089700120823         stascana=%dec(dataeur)  ;
089800120823 2     endif  ;
089900120824 2      if comscand>0  and comscana=0 ;
090000120824        comscana=comscand   ;
090100120824 2      Endif            ;
090200120823 2      if comscand=0               ;
090300120823        comscand=19900101  ;
090400120823 2      Endif            ;
090500120823
090600120823       // Date in ggmmaaaa
090700120823 2     select     ;
090800120823       when stascand=0        ;
090900120823         clear    stpdata8  ;
091000120823         clear    stpdec55  ;
091100120823         %subst(stpdec5:21:4)='AL :'     ;
091200120823         stpdata7=staspea   ;
091300120823
091400120823       when stascana=0        ;
091500120823         clear    stpdata8  ;
091600120823         clear    stpdec55  ;
091700120823         %subst(stpdec5:21:4)='IL :'     ;
091800120823         stpdata3=stascand   ;
091900120823
092000120823       other                ;
092100120823         stpdata7=stascand   ;
092200120823         stpdata8=stascana   ;
092300120823         stpdec55  = 'AL'   ;
092400120823 2      endSL              ;
092500120823 1     endif              ;
092600080902
092700080902       // Linea di partenza
092800080902       if stalnp=0  ;
092900080902       stpdlnp='TUTTE' ;
093000080902       else           ;
093100080903       stpdlnp='-'+ stadlnp ;
093200080902       endif          ;
093300080902
093400080829       righe=70   ;
093500110613       // imposto data e ora del giorno
093600110613       dataiso=datasys  ;
093700110613       dateu=%dec(dataiso)   ;
093800110616
093900110616       dataiso=dataiso-%days(30) ;
094000110616       unmese=%dec(dataiso)   ;
094100110616
094200110613       ora_iso=ora_sys  ;
094300110613       ora_u=%dec(ora_iso)   ;
094400080829
094500110616       // Se richiesto file pulisco il lancio precedente per
094600110616       //  fil e ccm
094700110616       setll (dutpou: staccm)  wfddt01l   ;
094800110616       reade (dutpou: staccm)  wfddt01l   ;
094900110616
095000110616       if not %eof(wfddt01l)  and
095100120828          (v1csta<>'S'     or ddtedt<unmese) ;
095200110616
095300110616       dow  not %eof(wfddt01l)   ;
095400110616       delete wfddt000  ;
095500110616
095600110616       reade (dutpou: staccm)  wfddt01l   ;
095700110616       enddo  ;
095800110616        else  ;
095900110616        unlock   wfddt01l  ;
096000110616        endif  ;
096100110616
096200120405       // Ragione sociale Mittente
096300120828        if v1csta<>'S'    and staccm>0 ;
096400110616        chain  (1:dutkci:staccm) cnaco00f  ;
096500110616         if not %found(cnaco00f)  ;
096600110616          acorag=*all'?'  ;
096700110616         endif ;
096800110616       endif ;
096900120405
097000120405       // Bolle inventariate
097100120405       if v1cinve='S'   ;
097200120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
097300120405       endif  ;
097400120405       if v1cinve='E'   ;
097500120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
097600120405       endif  ;
097700120405       if v1cerra='S'   ;
097800120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
097900120405       endif  ;
098000120405       if v1cerra='E'   ;
098100120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
098200120405       endif  ;
098300120823       if v1cscan='S'   ;
098400120823        stpscan='SOLO BOLLE CON DOCUMENTI SCANNERIZZATI';
098500120823       endif  ;
098600120823       if v1cSCAN='E'   ;
098700120824        stpscan='SOLO BOLLE SENZA DOCUMENTI SCANNERIZZATI';
098800120823       endif  ;
098900120405
099000120823       // richiesta la stampa del file
099100120828 1     if v1csta='S'or v1csta='E' ;
099200120823        stasta='SI' ;
099300120823       else  ;
099400120823        stasta='NO' ;
099500120823 1     endif  ;
099600120823
099700120823       // richiesta file
099800120828 1     if v1csta='X'or v1csta='E' ;
099900120823        stacreaf='WFDDT00F'  ;
100000120823        ELSE  ;
100100120823        stacreaf='NO      '  ;
100200120823 1     endif  ;
100300110616
100400080328       ENDSR                           ;
100500080317      /end-free
