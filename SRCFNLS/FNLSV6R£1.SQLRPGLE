000100120302     /*PRM dbgview(*source)
000200120302     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120302     /*CMD         ovrscope(*calllvl)
000400120302     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
000500120302     /*CMD         ovrscope(*calllvl)
000600120302     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000700120302     /*CMD         ovrscope(*calllvl)
000800120302     /*END  dltovr file(WFDDT01L) lvl(*)
000900120302     /*END  dltovr file(TITAS30C TITA430C FIAR531C) lvl(*)
001000120302     /*END
001100120302      *
001200001027     H DECEDIT('0,') DATEDIT(*DMY.)
001300080828      * FNLSV3R *----------------------------------------------------------*
001400080909      *  controllo documenti da restituire al mittente
001500080319      *--------------------------------------------------------------------*
001600080327     Fazorg01l  IF   E           K DISK
001700080327     Ftabel00f  IF   E           K DISK
001800120402     Ftntbe01l  IF   E           K DISK
001900080902     Ffnlbl01l  IF   E           K DISK
002000120406     Ffnlbl02l  IF   E           K DISK    rename(fnlbl000:fnlbl002)
002100110616     Fcnaco00f  IF   E           K DISK
002200110617     FwfDDT01L  uF a E           K DISK    extfile(WWFDDT) usropn
002300080902     Ftitas30c  IF   E           K DISK    extfile(WTAs30) usropn
002400080829     Ftita430c  IF   E           K DISK    extfile(WTA430) usropn
002500080829     Ffiar531c  uF   E           K DISK    extfile(WTA531) usropn
002600080828     FFNLSV3P   O    E             PRINTER
002700050705      *
002800080317      * - Passaggio parametri
002900080325     d KPJBA         e ds
003000080328     d
003100080328     d tibs34ds      e ds                  inz
003200080410     d tibs02ds      e ds                  inz
003300080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
003400080328     d DDatiUte      e ds                  dtaara(§datiute)
003500080829     d
003600080829     d dar5gen       e ds
003700080902     d DS7O          e ds
003800100603     d DS7R          e ds
003900080328     d
004000080829     d tasSPE          ds
004100080829     d tasaas
004200080829     d tasmgs
004300080829     d TASDSP                  1      8  0
004400080829     d taslnp
004500080829     d tasnrs
004600080829     d tasnsp
004700080829     d
004800080828
004900080828     D fnlsv2ds        DS
005000080828     D  stainv                 1      8  0
005100080828     D  cominv                 9     16  0
005200080828     D  stainvrd              17     24  0
005300080828     D  cominvrd              25     32  0
005400080828     D  stainvra              33     40  0
005500080828     D  cominvra              41     48  0
005600080828     D  stadsd                49     56  0
005700080828     D  comdsd                57     64  0
005800080828     D  stadsa                65     72  0
005900080828     D  comdsa                73     80  0
006000080828     D  staccm                81     87  0
006100080829     D  staRAG                88    112
006200080902     D  stalnp               113    115  0
006300080902     D  stadlnp              116    140
006400110613     D  stasfile             141    141
006500110617     D  staemail             142    171
006600120405     D
006700120405     D* Nuove parzializzazioni per lista generica
006800120405     d  listaGEN             172    172
006900120405     D  alfsped              173    180
007000120405     D  alfspea              181    188
007100120405     D  v1cinve              189    189
007200120405     D  v1cerra              190    190
007300120704     D  comscand             191    198  0
007400120704     D  comscana             199    206  0
007500120704     d*   Per il momento le date restituzione al mittente sono oscurate e non
007600120704     d*    usate
007700120704     D  cominvimD            207    214  0
007800120704     D  cominvimA            215    222  0
007900120704     D  v1cinvim             223    223
008000120704     d
008100120704     D  v1cscan              224    224
008200120704     D  v1csta               225    225
008300080828     d
008400110617     D  pInFILE        s             10
008500110617     D  pInIDJOB       s             26
008600110617     D  pInPWDXLS      s             15
008700110617     D  pInNOMXLS      s             78
008800110617     D  pInEMLDEST     s            121
008900111207     D  pInEMLVAR      s             80
009000120509     D  pInMRAKE2      s             15
009100120509     D  pOutESITO      s              1
009200080829     d savspe          s                   like(tasspe)
009300080829     d righe           s              3  0
009400110613     d dateu           s              8  0
009500110616     d unmese          s              8  0
009600110613     d ora_u           s              6  0
009700080829     d Wsta            s              1
009800080902     d STricezDDT      s              2
009900110613     d wlib            s             10    inz('GAITRAGRPS')
010000120405     d comsped         s              8  0
010100120405     d comspea         s              8  0
010200120405     d stasped         s              8  0
010300120405     d staspea         s              8  0
010400120424     d contarec        s              8  0
010500080828     d wrkgetlista     s           4096    varying
010600110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
010700110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
010800110613     d Ora_iso         s               t   timfmt(*iso)
010900110613     d Dataiso         s               d   datfmt(*iso)
011000080327     d Dataeur         s               d   datfmt(*eur)
011100080327     d Datadmy         s               d   datfmt(*dmy)
011200080829     d
011300110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
011400110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
011500110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
011600110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
011700080901     d dec1            c                   const('  Stampa Inventario  AL:')
011800080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
011900120406     d dec3            c                   const('   Data   consegna  DAL:')
012000120405     d dec4            c                   const('   Data spedizione  DAL:')
012100120411     d dec5            c                   const('   Data inventario  DAL:')
012200080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
012300080909     D                                     ESTITUIRE AL MITTENTE *')
012400120405     d tes2            c                   const('** EXCEL  DOCUMENTI  DA R-
012500120405     D                                     ESTITUIRE AL MITTENTE **')
012600080909     d Inv             c                   const('  Documenti inventariati  ')
012700080909     d RIinv           c                   const('Documenti già inventariati')
012800080909     d NOinv           c                   const(' Documenti non inventariat-
012900080909     D                                     i per bolle consegnate ')
013000120424     d xlsE            c                   const('Indirizzo e-mail per +
013100120424     d                                     invio file excel:')
013200120424     d xlsF            c                   const('Excel troppo grande:+
013300120424     d                                     posto nella cartella')
013400080317      *------------------------------------------------------------------------*
013500080317      *   P R O T O T I P I
013600080317      *------------------------------------------------------------------------*
013700080829      /copy gaitrasrc/srcprotopr,tibs34r
013800110617      /copy gaitrasrc/srcprotopr,trtcm8r1
013900080829      *------------------------------------------------------------------------*
014000080829     Ifiar5000      81
014100080829     Ifiar5p00      82
014200001027     C*---------------------------------------------------------------*
014300001027     C     *ENTRY        PLIST
014400080325     C                   PARM                    kpjba
014500080317      /free
014600080828
014700120405
014800120405       select    ;
014900120704       when  listaGEN='S'  and v1cinve<>'I'     ;
015000120405
015100120405       exsr  SQLGEN  ;
015200120405
015300080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
015400080829
015500120405 1     when STricezDDT='SI'        ;
015600080828
015700080828       // Prepara SQL
015800080901          clear wsta          ;
015900080902          EXSR SQLINV         ;
016000080902
016100120704       // elabora  le MAI inventariate
016200080902
016300120405 1     when STricezDDT='NO'        ;
016400080902          clear wsta          ;
016500080902          clear totale        ;
016600080902          EXSR SQLDSP      ;
016700120405 1     endsl               ;
016800080901
016900080901       //  Esegui SQL  e stampa
017000080901          EXSR SQLesegui   ;
017100080901
017200080901 2     if   wsta='S'          ;
017300120124 3     //**if  righe>59           ;
017400120124 3     if  righe>57           ;
017500080901       write  LsV3PT1         ;
017600080901 3     endif                  ;
017700080901
017800120406       if *in01   ;
017900120302       staEM =%trim(staemail)+'@brt.it' ;
018000120406       endif  ;
018100120406
018200080901       write  LsV3Pfine       ;
018300120406 2     endif               ;
018400110617
018500120406        if listaGEN='S'  ;
018600120424        select   ;
018700120424        when *in01 and contarec <=2000  ;
018800120424        stainvio=xlsE  ;
018900120406        staEM =%trim(staemail)+'@brt.it' ;
019000120424
019100120424        when *in01 and contarec > 2000  ;
019200120424        stainvio=xlsF  ;
019300120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
019400120504                               '\File Excel\WFDDT00F'  ;
019500120424
019600120424        other ;
019700120406        staEM='NESSUNA bolla elaborata:file excel non creato';
019800120424        endsl  ;
019900120406
020000120406        write  LsV3PT2         ;
020100120406        endif  ;
020200120406
020300120424        // Se scritto almeno un record,
020400120424
020500120424  1     if *in01  ;
020600120424
020700110617          pInIDJOB=knmeb       ;
020800120503          clear acorag  ;
020900120503  2       if  staccm>0   ;
021000120503          chain (1:dutkci:staccm) cnaco00f  ;
021100120503  3       if not %found(cnaco00f)  ;
021200120503            acorag=*all'?'  ;
021300120503  3       endif  ;
021400120503  2       endif ;
021500120503
021600120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
021700120503                       %subst(acorag:1:15)  ;
021800120424
021900120424        //      meno di 2000 record --> invio all'indirizzo e-mail
022000120424  1a      if contarec<= 2000 ;
022100120509            pInFILE='WFDDT00F' ;
022200120509            pInMRAKE2='XLS_E'  ;
022300120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
022400111207
022500120424  x1a     else  ;
022600120509          pInFILE='WFDDT00F' ;
022700120509            pInMRAKE2='XLS_F'  ;
022800120424  1       endif ;
022900120424
023000110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
023100120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
023200110617        endif  ;
023300080901
023400080829
023500080317        *inlr ='1'   ;
023600080327       //-------------------------------------------------------------------
023700080901       BEGSR  SQLDSP                 ;
023800080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
023900080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
024000080828
024100080901
024200080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
024300080829                        and substr(ar5uni, 115, 1)='' ''';
024400080902
024500080902        if stalnp>0    ;
024600080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
024700080902        endif          ;
024800080902
024900080829        ENDSR     ;
025000080828
025100080901       //-------------------------------------------------------------------
025200080901       BEGSR  SQLINV                 ;
025300080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
025400080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
025500080901
025600080901        // STampa
025700080901        if cominv>0     ;
025800080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
025900080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
026000080901                        and substr(ar5uni, 115, 1)='' ''';
026100080901        endif     ;
026200080901
026300080901        // RIstampa
026400080901
026500080901        if cominvra>0  ;
026600080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
026700080901                    substr(ar5uni, 107, 8) <= ''' +
026800080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
026900080901                    %char(cominvrd) + '''' ;
027000080901        endif     ;
027100080902
027200080902        if stalnp>0    ;
027300080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
027400080902        endif          ;
027500080902
027600080901        ENDSR     ;
027700120405       //-------------------------------------------------------------------
027800120405       BEGSR  SQLGEN                 ;
027900120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
028000120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
028100120405
028200120405        // Solo NON inventariate
028300120405        select   ;
028400120405        when  v1cinve='E'  ;
028500120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
028600120405
028700120411        // Solo     inventariate  senza data
028800120411        when  v1cinve='S'  and cominvra=0  ;
028900120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
029000120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
029100120411        // Solo     inventariate  con   data
029200120411        when   cominvra>0  ;
029300120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
029400120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
029500120411                    %char(cominvrd) + '''' ;
029600120405        other  ;
029700120405        // tutte
029800120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
029900120405        endsl  ;
030000120405
030100120405        // solo con   MOTIVO DDT ERRATO
030200120405        if  v1cerra='S'  ;
030300120405        wrkgetlista=wrkgetlista + ' and +
030400120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
030500120405        endif  ;
030600120405        // solo senza MOTIVO DDT ERRATO
030700120405        if  v1cerra='E'  ;
030800120405        wrkgetlista=wrkgetlista + ' and +
030900120405                                    substr(ar5uni, 167, 8) =   ''        ''';
031000120405        endif  ;
031100120405
031200120405        if stalnp>0    ;
031300120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
031400120405        endif          ;
031500120405
031600120405        ENDSR     ;
031700120405
031800080829       //-------------------------------------------------------------------
031900080829       BEGSR  SQLesegui              ;
032000080828       EXEC sql  prepare s1 from : wrkgetlista ;
032100080828       EXEC sql  declare a1 cursor for s1      ;
032200080828       EXEC sql  open    a1                    ;
032300080828
032400080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
032500080828                                        :ar5nsp, :ar5uni           ;
032600080828
032700120424       clear contarec ;
032800120424
032900080828       dow sqlcod=0    ;
033000080828
033100080829       exsr STAMPA     ;
033200080829
033300080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
033400080828                                        :ar5nsp, :ar5uni           ;
033500080828       enddo           ;
033600080828
033700080829       EXEC sql  CLOSE   a1                    ;
033800080829
033900080828       ENDSR     ;
034000080327       //-------------------------------------------------------------------
034100080325       BEGSR  Stampa          ;
034200080325
034300080325       // Cerco la bolla
034400080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
034500080901
034600120406 1     if %found(titas30c) and (STricezDDT='SI' or ListaGEN='S' or
034700110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
034800120406
034900120406       // Controllo la data spedizione se richiesta
035000120406 1c    if listaGEN <>'S' or comsped=0 or
035100120406             (tasdsp>=comsped and tasdsp<=comspea) ;
035200120406
035300120406       // Controllo la data consegna  se richiesta
035400120406 1d    if listaGEN <>'S' or comdsd=0 or
035500120406             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
035600120406
035700100603
035800100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
035900100603       clear ds7R ;
036000100603 2     if tasgma<>*blanks  ;
036100100603
036200100603         clear tblkey    ;
036300100603         tblkey=tasgma    ;
036400100603         chain (1:'7R':tblkey) tabel00f   ;
036500100603          if %found(tabel00f)   ;
036600100603          ds7r=tbluni       ;
036700100603          endif             ;
036800100603 2        endif             ;
036900100603
037000100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
037100080901
037200100528       // Escludo il cliente mittente se selezionato
037300100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
037400100528
037500080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
037600080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
037700080902 2     if not %found(fnlbl01l) ;
037800080902
037900080829       stplnp=taslnp          ;
038000080829       stpnrs=tasnrs          ;
038100080327         if stpnrs>0            ;
038200080327         tra01='-'              ;
038300080327         else                   ;
038400080327         tra01=' '              ;
038500080327         endif                  ;
038600080829       stpnsp=tasnsp          ;
038700080829       dataiso=%date(tasdsp)    ;
038800080327       datadmy=dataiso        ;
038900080327       stpDSP=%dec(datadmy)   ;
039000080829       stplna=taslna          ;
039100080829       stpRMN=tasRMN          ;
039200080902
039300080902       // Data consegna        ;
039400080902       clear stpdcm  ;
039500080902       clear stpcca    ;
039600120406       clear ds7o      ;
039700120423       CLEAR LBLLAN  ;
039800080902
039900080902 3     if tasdcm>0   ;
040000080902       DATAEUR=%DATE(tasdcm:*iso)       ;
040100080902       stpdcm=%dec(dataeur)   ;
040200080902 4     if tascca<>' '  ;
040300080902         clear tblkey    ;
040400080902         tblkey=tascca    ;
040500080902         stpcca=tascca    ;
040600080902
040700080902         chain (1:'7O':tblkey) tabel00f   ;
040800080902          if %found(tabel00f)   ;
040900080902          ds7o=tbluni       ;
041000080902           %subst(stpcca:2:10)='-'+tbluni   ;
041100080902          endif       ;
041200080902 4     endif       ;
041300120406
041400120406       // Se la consegna anomala non è reso, ma dirottamento,
041500120406       //  verifico se c'e' un'altra figlia resa
041600120406 4     if §7ofdi='D'  ;
041700120406       lblaan=ar5aas ;
041800120406       lbllpn=ar5lnp ;
041900120406       lblnrn=ar5nrs ;
042000120406       lblnsn=ar5nsp ;
042100120406
042200120406 5     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
042300120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
042400120406 6     if  %found(fnlbl02l)  ;
042500120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
042600120406         clear ds7o      ;
042700120406 7     if tascca<>' '  ;
042800120406         clear tblkey    ;
042900120406         tblkey=tascca    ;
043000120406         chain (1:'7O':tblkey) tabel00f   ;
043100120406          if %found(tabel00f)   ;
043200120406           ds7o=tbluni       ;
043300120406          endif  ;
043400120406 7     endif  ;
043500120406 6     endif  ;
043600120406
043700120406 5     enddo ;
043800120406
043900120406       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
044000120406 4     endif       ;
044100120406 3     endif       ;
044200080829
044300080902       // Escludo dalla stampa delle non inventariate le bolle di reso
044400080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
044500080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
044600080902 4     IF %FOUND(TITA430C)    ;
044700080829       STPRMA=ta4NOT          ;
044800080829       ELSE                   ;
044900080829       clear stprma           ;
045000080902 4     endif                  ;
045100080829
045200080901       stprsd=tasrsd          ;
045300080829       stplod=taslod          ;
045400080829       %subst(stplod:13:3)=' '+tasprd    ;
045500080901
045600080901       // Data inventario      ;
045700080829       dar5gen=ar5uni     ;
045800080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
045900080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
046000080901         stpinv=%dec(dataeur)   ;
046100080829       else   ;
046200080901        clear stpinv    ;
046300080902 4     endif           ;
046400080327
046500080325
046600120406       // Per lista generica non stampo il dettaglio
046700120406       if ListaGEN<>'S'  ;
046800120406
046900080902       // totale bolle stampate
047000080902       totale=totale+1        ;
047100080902
047200080327       // Testata bolla
047300080327
047400110617 4     if  righe>63           ;
047500080829       write  LsV3PT1         ;
047600080328       righe=8                ;
047700080902 4     endif                  ;
047800080331
047900080829       write   LsV3PD1        ;
048000080327
048100080327       righe=righe+1          ;
048200080829       wsta='S'               ;
048300120406       endif  ;
048400080829
048500110613       // Se richiesto --> creo file
048600110613       if   stasfile ='S'  ;
048700110613       clear wfddt000         ;
048800110616       ddtidute=knmeb   ;
048900110616       ddteut=knmus   ;
049000110613       ddtepo=dutpou  ;
049100110613       ddtedt=dateu   ;
049200110613       ddteor=ora_u   ;
049300110613       ddtccm=tasccm  ;
049400110616       // ragione sociale mittente
049500110616       if staccm=0  ;
049600110616       chain  (1:dutkci:tasccm) cnaco00f  ;
049700110616        if %found(cnaco00f)  ;
049800110616         ddtrsm=acorag   ;
049900110616        else  ;
050000110616        ddtrsm=*all'?' ;
050100110616        endif ;
050200110616       else  ;
050300110616        ddtrsm=acorag  ;
050400110616       endif  ;
050500110616
050600110613       ddtaas=tasaas  ;
050700110613       ddtlnp=taslnp  ;
050800110613       ddtnrs=tasnrs  ;
050900110613       ddtnsp=tasnsp  ;
051000110613       ddtmgs=tasmgs  ;
051100120423       IF LBLLAN>0    ;
051200120423        ddtlna=lbllan  ;
051300120423        ELSE   ;
051400120423        ddtlna=taslna  ;
051500120423        ENDIF  ;
051600110613       ddtrmn=tasrmn  ;
051700110613       ddtrma=stprma  ;
051800110613       ddtrsd=tasrsd  ;
051900110613       ddtlod=taslod  ;
052000110613       ddtcad=tascad  ;
052100110613       ddtprd=tasprd  ;
052200110613       ddtnzd=tasnzd  ;
052300110613       ddtdcm=tasdcm  ;
052400110613       ddtcca=tascca  ;
052500120320       // Area da organigramma
052600120423       chain ddtlna    azorg01l  ;
052700120320       if %found(azorg01l)  ;
052800120320       ddtcar=orgcar  ;
052900120320       endif  ;
053000120320
053100110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
053200110616         ddtinv =%int(§ar5drdt)             ;
053300110616       else   ;
053400110616        clear ddtinv    ;
053500110616 4     endif           ;
053600120402
053700120402       // Motivazione DDt errato
053800120402       if §ar5mtvdt<>*blanks   ;
053900120402       clear  tbeke1  ;
054000120402       clear  tbeke2  ;
054100120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
054200120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
054300120402
054400120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
054500120402        if %found(tntbe01l)  ;
054600120402         ddtmtvdt=tbeuni  ;
054700120402        endif   ;
054800120402
054900120402 4       if §ar5dimtv>*zeros     ;
055000120402           ddtdimtv =%int(§ar5dimtv)             ;
055100120402         else   ;
055200120402          clear ddtdimtv  ;
055300120402 4       endif           ;
055400120402 4     endif           ;
055500120402
055600110613       write wfddt000   ;
055700120424
055800120424       contarec=contarec +1 ;
055900110616
056000110616       *in01 =*on  ;
056100110613       endif  ;
056200110613
056300080829       // Aggiorno fiar5 se stampa
056400120406
056500120406 4     if cominv>0 and listagen<>'S'  ;
056600080829       *in81=*off     ;
056700080829       *in82=*off     ;
056800080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
056900080902 5       if %found(fiar531c)    ;
057000080829          dar5gen=ar5uni      ;
057100080829          §ar5fstd='S'        ;
057200080829          ar5uni=dar5gen       ;
057300080902 6          if *in81      ;
057400080829            update fiar5000    ;
057500080829            else    ;
057600080829            update fiar5p00    ;
057700080902 6          endif   ;
057800080902 5       endif    ;
057900080902 4     endif    ;
058000080901
058100080902 3     endif    ;
058200080902 2     endif    ;
058300100603 1b    endif    ;
058400100603 1a    endif    ;
058500120406 1c    endif    ;
058600120406 1d    endif    ;
058700080902 1     endif    ;
058800080327
058900080325       ENDSR                  ;
059000080327       //-------------------------------------------------------------------
059100080327       BEGSR  *INZSR                 ;
059200080328
059300080328       in(E) *dtaara   ;
059400080328       if    %error or rsut  =*blanks  ;
059500080328       callp TIBS34R (TIBS34DS)        ;
059600080328       in    *dtaara   ;
059700080328       endif                           ;
059800080829
059900080829       // Apertura file di sede
060000080829             open(e) titas30c                ;
060100080829           if      not %open(titas30c)  ;
060200110613             %subst(WTA430:7:4)='GRU '    ;
060300110613             %subst(WTAs30:7:4)='GRU '    ;
060400110613             %subst(WTA531:7:4)='GRU '    ;
060500110617             %subst(WWFDDT:7:4)='AZM '    ;
060600110613             %subst(Wlib:7:4)='GRU '    ;
060700080829             open titas30c                ;
060800110617           endif  ;
060900080829             open tita430c                ;
061000080829             open fiar531c                ;
061100110617             open wfddt01l                ;
061200080829
061300080829       fnlsv2ds=kpjbu     ;
061400120405       clear comsped  ;
061500120405       clear comspea  ;
061600120405       clear stasped  ;
061700120405       clear staspea  ;
061800120411
061900080829
062000120405 1     if ListaGEN='S'     ;
062100120405       stptes   =tes2     ;
062200120405       stpdecinv=dec3     ;
062300120405       stpdec3  =dec4     ;
062400120411       stpdec4  =dec5     ;
062500120405 x1    else  ;
062600120405
062700080909       stptes   =tes1     ;
062800120411
062900120411       if cominvra=0      ;
063000120411       cominvra=cominvrd  ;
063100120411       stainvra=stainvrd  ;
063200120411       endif              ;
063300080829
063400120411 2     if cominv>0        ;
063500080901       stpdecinv=dec1     ;
063600080909       %subst(stptes2:11:26)  =Inv      ;
063700080829       stpdata1=stainv    ;
063800080829       clear    stpdata2  ;
063900080829       clear    stpdecinv2;
064000080902       STricezDDT='SI'      ;
064100120411 2     endif              ;
064200080829
064300120411
064400120411 2     if cominvra>0      ;
064500080909       %subst(stptes2:11:26)  =RiInv      ;
064600080901       stpdecinv=dec2     ;
064700080901       stpdecinv2= 'AL'   ;
064800080829       stpdata1=stainvrd  ;
064900080829       stpdata2=stainvra  ;
065000120411       STricezDDT='SI'      ;
065100120411 2     endif              ;
065200120411 1     endif              ;
065300120411
065400120411 1     if ListaGEN='S'   ;
065500120411 2     if cominvrd>0  or cominvra>0 ;
065600120411
065700120411 3      if cominvrd>0  and cominvra=0 ;
065800120411        cominvra=cominvrd  ;
065900120411 3      Endif            ;
066000120411 3      if cominvrd=0               ;
066100120411        cominvrd=19900101  ;
066200120411 3      Endif            ;
066300120411
066400120411 3     select     ;
066500120411       when stainvrd=0        ;
066600120411         clear    stpdata6  ;
066700120411         clear    stpdec44  ;
066800120411         %subst(stpdec4:21:4)='AL :'     ;
066900120411         stpdata5=stainvra  ;
067000120411
067100120411       when stainvra=0        ;
067200120411         clear    stpdata6  ;
067300120411         clear    stpdec44  ;
067400120411         %subst(stpdec4:21:4)='IL :'     ;
067500120411         stpdata5=stainvrd  ;
067600120411
067700120411       other                ;
067800120411         stpdata5=stainvrd  ;
067900120411         stpdata6=stainvra  ;
068000120411         stpdec44  = 'AL'   ;
068100120411 3      endSL              ;
068200120411 2     endif              ;
068300120411 1     endif              ;
068400080829
068500120411       // data CONSEGNA
068600120405 1     if comdsd>0  or comdsa>0   ;
068700120411
068800120405 2     if ListaGEN<>'S'     ;
068900120405         STricezDDT='NO'      ;
069000120405         stptes2  =NOInv    ;
069100120405         stpdecinv=dec3     ;
069200120405       endif  ;
069300080901
069400080901        if comdsd>0  and comdsa=0 ;
069500080901        comdsa=comdsd    ;
069600080901        Endif            ;
069700080901        if comdsd=0               ;
069800080901        comdsd=19900101  ;
069900080901        Endif            ;
070000080901
070100080901       select     ;
070200080901       when stadsd=0        ;
070300080901         clear    stpdata2  ;
070400080901         clear    stpdecinv2;
070500080901         %subst(stpdecinv:21:4)='AL :'     ;
070600080901         stpdata1=stadsa    ;
070700080901
070800080901       when stadsa=0        ;
070900080901         clear    stpdata2  ;
071000080901         clear    stpdecinv2;
071100080901         %subst(stpdecinv:21:4)='IL :'     ;
071200080901         stpdata1=stadsd    ;
071300080901
071400080901       other                ;
071500080901         stpdata1=stadsd    ;
071600080901         stpdata2=stadsa    ;
071700080901         stpdecinv2= 'AL'   ;
071800080901        endSL              ;
071900080901       endif              ;
072000120405
072100120405       // Data spedizione DAL/AL
072200120405
072300120405       monitor   ;
072400120405       if alfsped>*zeros  ;
072500120405       comsped=%int(alfsped) ;
072600120405         dataiso=%date(comsped)  ;
072700120405         dataeur=dataiso;
072800120406         stasped=%dec(dataeur)  ;
072900120405       endif  ;
073000120405
073100120405       on-error  ;
073200120405       endmon  ;
073300120405       monitor   ;
073400120405       if alfspea>*zeros  ;
073500120405         comspea=%int(alfspea) ;
073600120405         dataiso=%date(comspea)  ;
073700120405         dataeur=dataiso;
073800120406         staspea=%dec(dataeur)  ;
073900120405       endif  ;
074000120405
074100120405       on-error  ;
074200120405       endmon  ;
074300120405
074400120405 1     if comsped>*zeros  or comspea>0   ;
074500120405        if comsped>0  and comspea=0 ;
074600120405        comspea=comsped   ;
074700120405        Endif            ;
074800120405        if comsped=0               ;
074900120406        comsped=19900101  ;
075000120405        Endif            ;
075100120405
075200120405       // Date in ggmmaaaa
075300120405       select     ;
075400120405       when stasped=0        ;
075500120405         clear    stpdata4  ;
075600120405         clear    stpdec33  ;
075700120405         %subst(stpdec3:21:4)='AL :'     ;
075800120405         stpdata3=staspea   ;
075900120405
076000120405       when staspea=0        ;
076100120405         clear    stpdata4  ;
076200120405         clear    stpdec33  ;
076300120405         %subst(stpdec3:21:4)='IL :'     ;
076400120405         stpdata3=stasped   ;
076500120405
076600120405       other                ;
076700120406         stpdata3=stasped   ;
076800120406         stpdata4=staspea   ;
076900120405         stpdec33  = 'AL'   ;
077000120405        endSL              ;
077100120405       endif              ;
077200080902
077300080902       // Linea di partenza
077400080902       if stalnp=0  ;
077500080902       stpdlnp='TUTTE' ;
077600080902       else           ;
077700080903       stpdlnp='-'+ stadlnp ;
077800080902       endif          ;
077900080902
078000080829       righe=70   ;
078100110613       // imposto data e ora del giorno
078200110613       dataiso=datasys  ;
078300110613       dateu=%dec(dataiso)   ;
078400110616
078500110616       dataiso=dataiso-%days(30) ;
078600110616       unmese=%dec(dataiso)   ;
078700110616
078800110613       ora_iso=ora_sys  ;
078900110613       ora_u=%dec(ora_iso)   ;
079000080829
079100110616       // Se richiesto file pulisco il lancio precedente per
079200110616       //  fil e ccm
079300110616       setll (dutpou: staccm)  wfddt01l   ;
079400110616       reade (dutpou: staccm)  wfddt01l   ;
079500110616
079600110616       if not %eof(wfddt01l)  and
079700110616          (stasfile='S'  or ddtedt<unmese) ;
079800110616
079900110616       dow  not %eof(wfddt01l)   ;
080000110616       delete wfddt000  ;
080100110616
080200110616       reade (dutpou: staccm)  wfddt01l   ;
080300110616       enddo  ;
080400110616        else  ;
080500110616        unlock   wfddt01l  ;
080600110616        endif  ;
080700110616
080800120405       // Ragione sociale Mittente
080900110617        if stasfile='S'   and staccm>0 ;
081000110616        chain  (1:dutkci:staccm) cnaco00f  ;
081100110616         if not %found(cnaco00f)  ;
081200110616          acorag=*all'?'  ;
081300110616         endif ;
081400110616       endif ;
081500120405
081600120405       // Solo file excel--> stmapo testata richieste
081700120405
081800120405       if listaGEN='S'    ;
081900120405       // Bolle inventariate
082000120405       if v1cinve='S'   ;
082100120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
082200120405       endif  ;
082300120405       if v1cinve='E'   ;
082400120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
082500120405       endif  ;
082600120405       if v1cerra='S'   ;
082700120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
082800120405       endif  ;
082900120405       if v1cerra='E'   ;
083000120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
083100120405       endif  ;
083200120405
083300120405       endif  ;
083400110616
083500080328       ENDSR                           ;
083600080317      /end-free
