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
007300080828     d
007400110617     D  pInFILE        s             10
007500110617     D  pInIDJOB       s             26
007600110617     D  pInPWDXLS      s             15
007700110617     D  pInNOMXLS      s             78
007800110617     D  pInEMLDEST     s            121
007900111207     D  pInEMLVAR      s             80
008000120509     D  pInMRAKE2      s             15
008100120509     D  pOutESITO      s              1
008200080829     d savspe          s                   like(tasspe)
008300080829     d righe           s              3  0
008400110613     d dateu           s              8  0
008500110616     d unmese          s              8  0
008600110613     d ora_u           s              6  0
008700080829     d Wsta            s              1
008800080902     d STricezDDT      s              2
008900110613     d wlib            s             10    inz('GAITRAGRPS')
009000120405     d comsped         s              8  0
009100120405     d comspea         s              8  0
009200120405     d stasped         s              8  0
009300120405     d staspea         s              8  0
009400120424     d contarec        s              8  0
009500080828     d wrkgetlista     s           4096    varying
009600110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
009700110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
009800110613     d Ora_iso         s               t   timfmt(*iso)
009900110613     d Dataiso         s               d   datfmt(*iso)
010000080327     d Dataeur         s               d   datfmt(*eur)
010100080327     d Datadmy         s               d   datfmt(*dmy)
010200080829     d
010300110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
010400110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
010500110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
010600110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
010700080901     d dec1            c                   const('  Stampa Inventario  AL:')
010800080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
010900120406     d dec3            c                   const('   Data   consegna  DAL:')
011000120405     d dec4            c                   const('   Data spedizione  DAL:')
011100120411     d dec5            c                   const('   Data inventario  DAL:')
011200080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
011300080909     D                                     ESTITUIRE AL MITTENTE *')
011400120405     d tes2            c                   const('** EXCEL  DOCUMENTI  DA R-
011500120405     D                                     ESTITUIRE AL MITTENTE **')
011600080909     d Inv             c                   const('  Documenti inventariati  ')
011700080909     d RIinv           c                   const('Documenti già inventariati')
011800080909     d NOinv           c                   const(' Documenti non inventariat-
011900080909     D                                     i per bolle consegnate ')
012000120424     d xlsE            c                   const('Indirizzo e-mail per +
012100120424     d                                     invio file excel:')
012200120424     d xlsF            c                   const('Excel troppo grande:+
012300120424     d                                     posto nella cartella')
012400080317      *------------------------------------------------------------------------*
012500080317      *   P R O T O T I P I
012600080317      *------------------------------------------------------------------------*
012700080829      /copy gaitrasrc/srcprotopr,tibs34r
012800110617      /copy gaitrasrc/srcprotopr,trtcm8r1
012900080829      *------------------------------------------------------------------------*
013000080829     Ifiar5000      81
013100080829     Ifiar5p00      82
013200001027     C*---------------------------------------------------------------*
013300001027     C     *ENTRY        PLIST
013400080325     C                   PARM                    kpjba
013500080317      /free
013600080828
013700120405       // File excel generico
013800120405
013900120405       select    ;
014000120405       when  listaGEN='S'  ;
014100120405
014200120405       exsr  SQLGEN  ;
014300120405
014400080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
014500080829
014600120405 1     when STricezDDT='SI'        ;
014700080828
014800080828       // Prepara SQL
014900080901          clear wsta          ;
015000080902          EXSR SQLINV         ;
015100080902
015200080902       // Stampa le non inventariate
015300080902
015400120405 1     when STricezDDT='NO'        ;
015500080902          clear wsta          ;
015600080902          clear totale        ;
015700080902          EXSR SQLDSP      ;
015800120405 1     endsl               ;
015900080901
016000080901       //  Esegui SQL  e stampa
016100080901          EXSR SQLesegui   ;
016200080901
016300080901 2     if   wsta='S'          ;
016400120124 3     //**if  righe>59           ;
016500120124 3     if  righe>57           ;
016600080901       write  LsV3PT1         ;
016700080901 3     endif                  ;
016800080901
016900120406       if *in01   ;
017000120302       staEM =%trim(staemail)+'@brt.it' ;
017100120406       endif  ;
017200120406
017300080901       write  LsV3Pfine       ;
017400120406 2     endif               ;
017500110617
017600120406        if listaGEN='S'  ;
017700120424        select   ;
017800120424        when *in01 and contarec <=2000  ;
017900120424        stainvio=xlsE  ;
018000120406        staEM =%trim(staemail)+'@brt.it' ;
018100120424
018200120424        when *in01 and contarec > 2000  ;
018300120424        stainvio=xlsF  ;
018400120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
018500120504                               '\File Excel\WFDDT00F'  ;
018600120424
018700120424        other ;
018800120406        staEM='NESSUNA bolla elaborata:file excel non creato';
018900120424        endsl  ;
019000120406
019100120406        write  LsV3PT2         ;
019200120406        endif  ;
019300120406
019400120424        // Se scritto almeno un record,
019500120424
019600120424  1     if *in01  ;
019700120424
019800110617          pInIDJOB=knmeb       ;
019900120503          clear acorag  ;
020000120503  2       if  staccm>0   ;
020100120503          chain (1:dutkci:staccm) cnaco00f  ;
020200120503  3       if not %found(cnaco00f)  ;
020300120503            acorag=*all'?'  ;
020400120503  3       endif  ;
020500120503  2       endif ;
020600120503
020700120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
020800120503                       %subst(acorag:1:15)  ;
020900120424
021000120424        //      meno di 2000 record --> invio all'indirizzo e-mail
021100120424  1a      if contarec<= 2000 ;
021200120509            pInFILE='WFDDT00F' ;
021300120509            pInMRAKE2='XLS_E'  ;
021400120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
021500111207
021600120424  x1a     else  ;
021700120509          pInFILE='WFDDT00F' ;
021800120509            pInMRAKE2='XLS_F'  ;
021900120424  1       endif ;
022000120424
022100110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
022200120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
022300110617        endif  ;
022400080901
022500080829
022600080317        *inlr ='1'   ;
022700080327       //-------------------------------------------------------------------
022800080901       BEGSR  SQLDSP                 ;
022900080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
023000080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
023100080828
023200080901
023300080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
023400080829                        and substr(ar5uni, 115, 1)='' ''';
023500080902
023600080902        if stalnp>0    ;
023700080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
023800080902        endif          ;
023900080902
024000080829        ENDSR     ;
024100080828
024200080901       //-------------------------------------------------------------------
024300080901       BEGSR  SQLINV                 ;
024400080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
024500080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
024600080901
024700080901        // STampa
024800080901        if cominv>0     ;
024900080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
025000080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
025100080901                        and substr(ar5uni, 115, 1)='' ''';
025200080901        endif     ;
025300080901
025400080901        // RIstampa
025500080901
025600080901        if cominvra>0  ;
025700080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
025800080901                    substr(ar5uni, 107, 8) <= ''' +
025900080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
026000080901                    %char(cominvrd) + '''' ;
026100080901        endif     ;
026200080902
026300080902        if stalnp>0    ;
026400080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
026500080902        endif          ;
026600080902
026700080901        ENDSR     ;
026800120405       //-------------------------------------------------------------------
026900120405       BEGSR  SQLGEN                 ;
027000120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
027100120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
027200120405
027300120405        // Solo NON inventariate
027400120405        select   ;
027500120405        when  v1cinve='E'  ;
027600120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
027700120405
027800120411        // Solo     inventariate  senza data
027900120411        when  v1cinve='S'  and cominvra=0  ;
028000120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
028100120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
028200120411        // Solo     inventariate  con   data
028300120411        when   cominvra>0  ;
028400120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
028500120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
028600120411                    %char(cominvrd) + '''' ;
028700120405        other  ;
028800120405        // tutte
028900120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
029000120405        endsl  ;
029100120405
029200120405        // solo con   MOTIVO DDT ERRATO
029300120405        if  v1cerra='S'  ;
029400120405        wrkgetlista=wrkgetlista + ' and +
029500120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
029600120405        endif  ;
029700120405        // solo senza MOTIVO DDT ERRATO
029800120405        if  v1cerra='E'  ;
029900120405        wrkgetlista=wrkgetlista + ' and +
030000120405                                    substr(ar5uni, 167, 8) =   ''        ''';
030100120405        endif  ;
030200120405
030300120405        if stalnp>0    ;
030400120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
030500120405        endif          ;
030600120405
030700120405        ENDSR     ;
030800120405
030900080829       //-------------------------------------------------------------------
031000080829       BEGSR  SQLesegui              ;
031100080828       EXEC sql  prepare s1 from : wrkgetlista ;
031200080828       EXEC sql  declare a1 cursor for s1      ;
031300080828       EXEC sql  open    a1                    ;
031400080828
031500080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
031600080828                                        :ar5nsp, :ar5uni           ;
031700080828
031800120424       clear contarec ;
031900120424
032000080828       dow sqlcod=0    ;
032100080828
032200080829       exsr STAMPA     ;
032300080829
032400080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
032500080828                                        :ar5nsp, :ar5uni           ;
032600080828       enddo           ;
032700080828
032800080829       EXEC sql  CLOSE   a1                    ;
032900080829
033000080828       ENDSR     ;
033100080327       //-------------------------------------------------------------------
033200080325       BEGSR  Stampa          ;
033300080325
033400080325       // Cerco la bolla
033500080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
033600080901
033700120406 1     if %found(titas30c) and (STricezDDT='SI' or ListaGEN='S' or
033800110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
033900120406
034000120406       // Controllo la data spedizione se richiesta
034100120406 1c    if listaGEN <>'S' or comsped=0 or
034200120406             (tasdsp>=comsped and tasdsp<=comspea) ;
034300120406
034400120406       // Controllo la data consegna  se richiesta
034500120406 1d    if listaGEN <>'S' or comdsd=0 or
034600120406             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
034700120406
034800100603
034900100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
035000100603       clear ds7R ;
035100100603 2     if tasgma<>*blanks  ;
035200100603
035300100603         clear tblkey    ;
035400100603         tblkey=tasgma    ;
035500100603         chain (1:'7R':tblkey) tabel00f   ;
035600100603          if %found(tabel00f)   ;
035700100603          ds7r=tbluni       ;
035800100603          endif             ;
035900100603 2        endif             ;
036000100603
036100100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
036200080901
036300100528       // Escludo il cliente mittente se selezionato
036400100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
036500100528
036600080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
036700080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
036800080902 2     if not %found(fnlbl01l) ;
036900080902
037000080829       stplnp=taslnp          ;
037100080829       stpnrs=tasnrs          ;
037200080327         if stpnrs>0            ;
037300080327         tra01='-'              ;
037400080327         else                   ;
037500080327         tra01=' '              ;
037600080327         endif                  ;
037700080829       stpnsp=tasnsp          ;
037800080829       dataiso=%date(tasdsp)    ;
037900080327       datadmy=dataiso        ;
038000080327       stpDSP=%dec(datadmy)   ;
038100080829       stplna=taslna          ;
038200080829       stpRMN=tasRMN          ;
038300080902
038400080902       // Data consegna        ;
038500080902       clear stpdcm  ;
038600080902       clear stpcca    ;
038700120406       clear ds7o      ;
038800120423       CLEAR LBLLAN  ;
038900080902
039000080902 3     if tasdcm>0   ;
039100080902       DATAEUR=%DATE(tasdcm:*iso)       ;
039200080902       stpdcm=%dec(dataeur)   ;
039300080902 4     if tascca<>' '  ;
039400080902         clear tblkey    ;
039500080902         tblkey=tascca    ;
039600080902         stpcca=tascca    ;
039700080902
039800080902         chain (1:'7O':tblkey) tabel00f   ;
039900080902          if %found(tabel00f)   ;
040000080902          ds7o=tbluni       ;
040100080902           %subst(stpcca:2:10)='-'+tbluni   ;
040200080902          endif       ;
040300080902 4     endif       ;
040400120406
040500120406       // Se la consegna anomala non è reso, ma dirottamento,
040600120406       //  verifico se c'e' un'altra figlia resa
040700120406 4     if §7ofdi='D'  ;
040800120406       lblaan=ar5aas ;
040900120406       lbllpn=ar5lnp ;
041000120406       lblnrn=ar5nrs ;
041100120406       lblnsn=ar5nsp ;
041200120406
041300120406 5     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
041400120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
041500120406 6     if  %found(fnlbl02l)  ;
041600120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
041700120406         clear ds7o      ;
041800120406 7     if tascca<>' '  ;
041900120406         clear tblkey    ;
042000120406         tblkey=tascca    ;
042100120406         chain (1:'7O':tblkey) tabel00f   ;
042200120406          if %found(tabel00f)   ;
042300120406           ds7o=tbluni       ;
042400120406          endif  ;
042500120406 7     endif  ;
042600120406 6     endif  ;
042700120406
042800120406 5     enddo ;
042900120406
043000120406       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
043100120406 4     endif       ;
043200120406 3     endif       ;
043300080829
043400080902       // Escludo dalla stampa delle non inventariate le bolle di reso
043500080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
043600080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
043700080902 4     IF %FOUND(TITA430C)    ;
043800080829       STPRMA=ta4NOT          ;
043900080829       ELSE                   ;
044000080829       clear stprma           ;
044100080902 4     endif                  ;
044200080829
044300080901       stprsd=tasrsd          ;
044400080829       stplod=taslod          ;
044500080829       %subst(stplod:13:3)=' '+tasprd    ;
044600080901
044700080901       // Data inventario      ;
044800080829       dar5gen=ar5uni     ;
044900080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
045000080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
045100080901         stpinv=%dec(dataeur)   ;
045200080829       else   ;
045300080901        clear stpinv    ;
045400080902 4     endif           ;
045500080327
045600080325
045700120406       // Per lista generica non stampo il dettaglio
045800120406       if ListaGEN<>'S'  ;
045900120406
046000080902       // totale bolle stampate
046100080902       totale=totale+1        ;
046200080902
046300080327       // Testata bolla
046400080327
046500110617 4     if  righe>63           ;
046600080829       write  LsV3PT1         ;
046700080328       righe=8                ;
046800080902 4     endif                  ;
046900080331
047000080829       write   LsV3PD1        ;
047100080327
047200080327       righe=righe+1          ;
047300080829       wsta='S'               ;
047400120406       endif  ;
047500080829
047600110613       // Se richiesto --> creo file
047700110613       if   stasfile ='S'  ;
047800110613       clear wfddt000         ;
047900110616       ddtidute=knmeb   ;
048000110616       ddteut=knmus   ;
048100110613       ddtepo=dutpou  ;
048200110613       ddtedt=dateu   ;
048300110613       ddteor=ora_u   ;
048400110613       ddtccm=tasccm  ;
048500110616       // ragione sociale mittente
048600110616       if staccm=0  ;
048700110616       chain  (1:dutkci:tasccm) cnaco00f  ;
048800110616        if %found(cnaco00f)  ;
048900110616         ddtrsm=acorag   ;
049000110616        else  ;
049100110616        ddtrsm=*all'?' ;
049200110616        endif ;
049300110616       else  ;
049400110616        ddtrsm=acorag  ;
049500110616       endif  ;
049600110616
049700110613       ddtaas=tasaas  ;
049800110613       ddtlnp=taslnp  ;
049900110613       ddtnrs=tasnrs  ;
050000110613       ddtnsp=tasnsp  ;
050100110613       ddtmgs=tasmgs  ;
050200120423       IF LBLLAN>0    ;
050300120423        ddtlna=lbllan  ;
050400120423        ELSE   ;
050500120423        ddtlna=taslna  ;
050600120423        ENDIF  ;
050700110613       ddtrmn=tasrmn  ;
050800110613       ddtrma=stprma  ;
050900110613       ddtrsd=tasrsd  ;
051000110613       ddtlod=taslod  ;
051100110613       ddtcad=tascad  ;
051200110613       ddtprd=tasprd  ;
051300110613       ddtnzd=tasnzd  ;
051400110613       ddtdcm=tasdcm  ;
051500110613       ddtcca=tascca  ;
051600120320       // Area da organigramma
051700120423       chain ddtlna    azorg01l  ;
051800120320       if %found(azorg01l)  ;
051900120320       ddtcar=orgcar  ;
052000120320       endif  ;
052100120320
052200110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
052300110616         ddtinv =%int(§ar5drdt)             ;
052400110616       else   ;
052500110616        clear ddtinv    ;
052600110616 4     endif           ;
052700120402
052800120402       // Motivazione DDt errato
052900120402       if §ar5mtvdt<>*blanks   ;
053000120402       clear  tbeke1  ;
053100120402       clear  tbeke2  ;
053200120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
053300120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
053400120402
053500120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
053600120402        if %found(tntbe01l)  ;
053700120402         ddtmtvdt=tbeuni  ;
053800120402        endif   ;
053900120402
054000120402 4       if §ar5dimtv>*zeros     ;
054100120402           ddtdimtv =%int(§ar5dimtv)             ;
054200120402         else   ;
054300120402          clear ddtdimtv  ;
054400120402 4       endif           ;
054500120402 4     endif           ;
054600120402
054700110613       write wfddt000   ;
054800120424
054900120424       contarec=contarec +1 ;
055000110616
055100110616       *in01 =*on  ;
055200110613       endif  ;
055300110613
055400080829       // Aggiorno fiar5 se stampa
055500120406
055600120406 4     if cominv>0 and listagen<>'S'  ;
055700080829       *in81=*off     ;
055800080829       *in82=*off     ;
055900080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
056000080902 5       if %found(fiar531c)    ;
056100080829          dar5gen=ar5uni      ;
056200080829          §ar5fstd='S'        ;
056300080829          ar5uni=dar5gen       ;
056400080902 6          if *in81      ;
056500080829            update fiar5000    ;
056600080829            else    ;
056700080829            update fiar5p00    ;
056800080902 6          endif   ;
056900080902 5       endif    ;
057000080902 4     endif    ;
057100080901
057200080902 3     endif    ;
057300080902 2     endif    ;
057400100603 1b    endif    ;
057500100603 1a    endif    ;
057600120406 1c    endif    ;
057700120406 1d    endif    ;
057800080902 1     endif    ;
057900080327
058000080325       ENDSR                  ;
058100080327       //-------------------------------------------------------------------
058200080327       BEGSR  *INZSR                 ;
058300080328
058400080328       in(E) *dtaara   ;
058500080328       if    %error or rsut  =*blanks  ;
058600080328       callp TIBS34R (TIBS34DS)        ;
058700080328       in    *dtaara   ;
058800080328       endif                           ;
058900080829
059000080829       // Apertura file di sede
059100080829             open(e) titas30c                ;
059200080829           if      not %open(titas30c)  ;
059300110613             %subst(WTA430:7:4)='GRU '    ;
059400110613             %subst(WTAs30:7:4)='GRU '    ;
059500110613             %subst(WTA531:7:4)='GRU '    ;
059600110617             %subst(WWFDDT:7:4)='AZM '    ;
059700110613             %subst(Wlib:7:4)='GRU '    ;
059800080829             open titas30c                ;
059900110617           endif  ;
060000080829             open tita430c                ;
060100080829             open fiar531c                ;
060200110617             open wfddt01l                ;
060300080829
060400080829       fnlsv2ds=kpjbu     ;
060500120405       clear comsped  ;
060600120405       clear comspea  ;
060700120405       clear stasped  ;
060800120405       clear staspea  ;
060900120411
061000080829
061100120405 1     if ListaGEN='S'     ;
061200120405       stptes   =tes2     ;
061300120405       stpdecinv=dec3     ;
061400120405       stpdec3  =dec4     ;
061500120411       stpdec4  =dec5     ;
061600120405 x1    else  ;
061700120405
061800080909       stptes   =tes1     ;
061900120411
062000120411       if cominvra=0      ;
062100120411       cominvra=cominvrd  ;
062200120411       stainvra=stainvrd  ;
062300120411       endif              ;
062400080829
062500120411 2     if cominv>0        ;
062600080901       stpdecinv=dec1     ;
062700080909       %subst(stptes2:11:26)  =Inv      ;
062800080829       stpdata1=stainv    ;
062900080829       clear    stpdata2  ;
063000080829       clear    stpdecinv2;
063100080902       STricezDDT='SI'      ;
063200120411 2     endif              ;
063300080829
063400120411
063500120411 2     if cominvra>0      ;
063600080909       %subst(stptes2:11:26)  =RiInv      ;
063700080901       stpdecinv=dec2     ;
063800080901       stpdecinv2= 'AL'   ;
063900080829       stpdata1=stainvrd  ;
064000080829       stpdata2=stainvra  ;
064100120411       STricezDDT='SI'      ;
064200120411 2     endif              ;
064300120411 1     endif              ;
064400120411
064500120411 1     if ListaGEN='S'   ;
064600120411 2     if cominvrd>0  or cominvra>0 ;
064700120411
064800120411 3      if cominvrd>0  and cominvra=0 ;
064900120411        cominvra=cominvrd  ;
065000120411 3      Endif            ;
065100120411 3      if cominvrd=0               ;
065200120411        cominvrd=19900101  ;
065300120411 3      Endif            ;
065400120411
065500120411 3     select     ;
065600120411       when stainvrd=0        ;
065700120411         clear    stpdata6  ;
065800120411         clear    stpdec44  ;
065900120411         %subst(stpdec4:21:4)='AL :'     ;
066000120411         stpdata5=stainvra  ;
066100120411
066200120411       when stainvra=0        ;
066300120411         clear    stpdata6  ;
066400120411         clear    stpdec44  ;
066500120411         %subst(stpdec4:21:4)='IL :'     ;
066600120411         stpdata5=stainvrd  ;
066700120411
066800120411       other                ;
066900120411         stpdata5=stainvrd  ;
067000120411         stpdata6=stainvra  ;
067100120411         stpdec44  = 'AL'   ;
067200120411 3      endSL              ;
067300120411 2     endif              ;
067400120411 1     endif              ;
067500080829
067600120411       // data CONSEGNA
067700120405 1     if comdsd>0  or comdsa>0   ;
067800120411
067900120405 2     if ListaGEN<>'S'     ;
068000120405         STricezDDT='NO'      ;
068100120405         stptes2  =NOInv    ;
068200120405         stpdecinv=dec3     ;
068300120405       endif  ;
068400080901
068500080901        if comdsd>0  and comdsa=0 ;
068600080901        comdsa=comdsd    ;
068700080901        Endif            ;
068800080901        if comdsd=0               ;
068900080901        comdsd=19900101  ;
069000080901        Endif            ;
069100080901
069200080901       select     ;
069300080901       when stadsd=0        ;
069400080901         clear    stpdata2  ;
069500080901         clear    stpdecinv2;
069600080901         %subst(stpdecinv:21:4)='AL :'     ;
069700080901         stpdata1=stadsa    ;
069800080901
069900080901       when stadsa=0        ;
070000080901         clear    stpdata2  ;
070100080901         clear    stpdecinv2;
070200080901         %subst(stpdecinv:21:4)='IL :'     ;
070300080901         stpdata1=stadsd    ;
070400080901
070500080901       other                ;
070600080901         stpdata1=stadsd    ;
070700080901         stpdata2=stadsa    ;
070800080901         stpdecinv2= 'AL'   ;
070900080901        endSL              ;
071000080901       endif              ;
071100120405
071200120405       // Data spedizione DAL/AL
071300120405
071400120405       monitor   ;
071500120405       if alfsped>*zeros  ;
071600120405       comsped=%int(alfsped) ;
071700120405         dataiso=%date(comsped)  ;
071800120405         dataeur=dataiso;
071900120406         stasped=%dec(dataeur)  ;
072000120405       endif  ;
072100120405
072200120405       on-error  ;
072300120405       endmon  ;
072400120405       monitor   ;
072500120405       if alfspea>*zeros  ;
072600120405         comspea=%int(alfspea) ;
072700120405         dataiso=%date(comspea)  ;
072800120405         dataeur=dataiso;
072900120406         staspea=%dec(dataeur)  ;
073000120405       endif  ;
073100120405
073200120405       on-error  ;
073300120405       endmon  ;
073400120405
073500120405 1     if comsped>*zeros  or comspea>0   ;
073600120405        if comsped>0  and comspea=0 ;
073700120405        comspea=comsped   ;
073800120405        Endif            ;
073900120405        if comsped=0               ;
074000120406        comsped=19900101  ;
074100120405        Endif            ;
074200120405
074300120405       // Date in ggmmaaaa
074400120405       select     ;
074500120405       when stasped=0        ;
074600120405         clear    stpdata4  ;
074700120405         clear    stpdec33  ;
074800120405         %subst(stpdec3:21:4)='AL :'     ;
074900120405         stpdata3=staspea   ;
075000120405
075100120405       when staspea=0        ;
075200120405         clear    stpdata4  ;
075300120405         clear    stpdec33  ;
075400120405         %subst(stpdec3:21:4)='IL :'     ;
075500120405         stpdata3=stasped   ;
075600120405
075700120405       other                ;
075800120406         stpdata3=stasped   ;
075900120406         stpdata4=staspea   ;
076000120405         stpdec33  = 'AL'   ;
076100120405        endSL              ;
076200120405       endif              ;
076300080902
076400080902       // Linea di partenza
076500080902       if stalnp=0  ;
076600080902       stpdlnp='TUTTE' ;
076700080902       else           ;
076800080903       stpdlnp='-'+ stadlnp ;
076900080902       endif          ;
077000080902
077100080829       righe=70   ;
077200110613       // imposto data e ora del giorno
077300110613       dataiso=datasys  ;
077400110613       dateu=%dec(dataiso)   ;
077500110616
077600110616       dataiso=dataiso-%days(30) ;
077700110616       unmese=%dec(dataiso)   ;
077800110616
077900110613       ora_iso=ora_sys  ;
078000110613       ora_u=%dec(ora_iso)   ;
078100080829
078200110616       // Se richiesto file pulisco il lancio precedente per
078300110616       //  fil e ccm
078400110616       setll (dutpou: staccm)  wfddt01l   ;
078500110616       reade (dutpou: staccm)  wfddt01l   ;
078600110616
078700110616       if not %eof(wfddt01l)  and
078800110616          (stasfile='S'  or ddtedt<unmese) ;
078900110616
079000110616       dow  not %eof(wfddt01l)   ;
079100110616       delete wfddt000  ;
079200110616
079300110616       reade (dutpou: staccm)  wfddt01l   ;
079400110616       enddo  ;
079500110616        else  ;
079600110616        unlock   wfddt01l  ;
079700110616        endif  ;
079800110616
079900120405       // Ragione sociale Mittente
080000110617        if stasfile='S'   and staccm>0 ;
080100110616        chain  (1:dutkci:staccm) cnaco00f  ;
080200110616         if not %found(cnaco00f)  ;
080300110616          acorag=*all'?'  ;
080400110616         endif ;
080500110616       endif ;
080600120405
080700120405       // Solo file excel--> stmapo testata richieste
080800120405
080900120405       if listaGEN='S'    ;
081000120405       // Bolle inventariate
081100120405       if v1cinve='S'   ;
081200120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
081300120405       endif  ;
081400120405       if v1cinve='E'   ;
081500120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
081600120405       endif  ;
081700120405       if v1cerra='S'   ;
081800120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
081900120405       endif  ;
082000120405       if v1cerra='E'   ;
082100120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
082200120405       endif  ;
082300120405
082400120405       endif  ;
082500110616
082600080328       ENDSR                           ;
082700080317      /end-free
