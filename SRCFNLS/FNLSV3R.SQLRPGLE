000100120302     /*PRM dbgview(*source)
000200120302     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120302     /*CMD         ovrscope(*calllvl)
000400120302     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
000500120302     /*CMD         ovrscope(*calllvl)
000600120302     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000700120302     /*CMD         ovrscope(*calllvl)
000800120302     /*END  dltovr file(TITAS30C TITA430C FIAR531C) lvl(*)
000900120302     /*END
001000120302      *
001100001027     H DECEDIT('0,') DATEDIT(*DMY.)
001200080828      * FNLSV3R *----------------------------------------------------------*
001300080909      *  controllo documenti da restituire al mittente
001400080319      *--------------------------------------------------------------------*
001500080327     Fazorg01l  IF   E           K DISK
001600080327     Ftabel00f  IF   E           K DISK
001700120402     Ftntbe01l  IF   E           K DISK
001800080902     Ffnlbl01l  IF   E           K DISK
001900120406     Ffnlbl02l  IF   E           K DISK    rename(fnlbl000:fnlbl002)
002000110616     Fcnaco00f  IF   E           K DISK
002100110617     FwfDDT01L  uF a E           K DISK    extfile(WWFDDT) usropn
002200080902     Ftitas30c  IF   E           K DISK    extfile(WTAs30) usropn
002300080829     Ftita430c  IF   E           K DISK    extfile(WTA430) usropn
002400080829     Ffiar531c  uF   E           K DISK    extfile(WTA531) usropn
002500080828     FFNLSV3P   O    E             PRINTER
002600050705      *
002700080317      * - Passaggio parametri
002800080325     d KPJBA         e ds
002900080328     d
003000080328     d tibs34ds      e ds                  inz
003100080410     d tibs02ds      e ds                  inz
003200080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
003300080328     d DDatiUte      e ds                  dtaara(§datiute)
003400080829     d
003500080829     d dar5gen       e ds
003600080902     d DS7O          e ds
003700100603     d DS7R          e ds
003800080328     d
003900080829     d tasSPE          ds
004000080829     d tasaas
004100080829     d tasmgs
004200080829     d TASDSP                  1      8  0
004300080829     d taslnp
004400080829     d tasnrs
004500080829     d tasnsp
004600080829     d
004700080828
004800080828     D fnlsv2ds        DS
004900080828     D  stainv                 1      8  0
005000080828     D  cominv                 9     16  0
005100080828     D  stainvrd              17     24  0
005200080828     D  cominvrd              25     32  0
005300080828     D  stainvra              33     40  0
005400080828     D  cominvra              41     48  0
005500080828     D  stadsd                49     56  0
005600080828     D  comdsd                57     64  0
005700080828     D  stadsa                65     72  0
005800080828     D  comdsa                73     80  0
005900080828     D  staccm                81     87  0
006000080829     D  staRAG                88    112
006100080902     D  stalnp               113    115  0
006200080902     D  stadlnp              116    140
006300110613     D  stasfile             141    141
006400110617     D  staemail             142    171
006500120405     D
006600120405     D* Nuove parzializzazioni per lista generica
006700120405     d  listaGEN             172    172
006800120405     D  alfsped              173    180
006900120405     D  alfspea              181    188
007000120405     D  v1cinve              189    189
007100120405     D  v1cerra              190    190
007200080828     d
007300110617     D  pInFILE        s             10
007400110617     D  pInIDJOB       s             26
007500110617     D  pInPWDXLS      s             15
007600110617     D  pInNOMXLS      s             78
007700110617     D  pInEMLDEST     s            121
007800111207     D  pInEMLVAR      s             80
007900120509     D  pInMRAKE2      s             15
008000120509     D  pOutESITO      s              1
008100080829     d savspe          s                   like(tasspe)
008200080829     d righe           s              3  0
008300110613     d dateu           s              8  0
008400110616     d unmese          s              8  0
008500110613     d ora_u           s              6  0
008600080829     d Wsta            s              1
008700080902     d STricezDDT      s              2
008800110613     d wlib            s             10    inz('GAITRAGRPS')
008900120405     d comsped         s              8  0
009000120405     d comspea         s              8  0
009100120405     d stasped         s              8  0
009200120405     d staspea         s              8  0
009300120424     d contarec        s              8  0
009400080828     d wrkgetlista     s           4096    varying
009500110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
009600110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
009700110613     d Ora_iso         s               t   timfmt(*iso)
009800110613     d Dataiso         s               d   datfmt(*iso)
009900080327     d Dataeur         s               d   datfmt(*eur)
010000080327     d Datadmy         s               d   datfmt(*dmy)
010100080829     d
010200110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
010300110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
010400110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
010500110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
010600080901     d dec1            c                   const('  Stampa Inventario  AL:')
010700080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
010800120406     d dec3            c                   const('   Data   consegna  DAL:')
010900120405     d dec4            c                   const('   Data spedizione  DAL:')
011000120411     d dec5            c                   const('   Data inventario  DAL:')
011100080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
011200080909     D                                     ESTITUIRE AL MITTENTE *')
011300120405     d tes2            c                   const('** EXCEL  DOCUMENTI  DA R-
011400120405     D                                     ESTITUIRE AL MITTENTE **')
011500080909     d Inv             c                   const('  Documenti inventariati  ')
011600080909     d RIinv           c                   const('Documenti già inventariati')
011700080909     d NOinv           c                   const(' Documenti non inventariat-
011800080909     D                                     i per bolle consegnate ')
011900120424     d xlsE            c                   const('Indirizzo e-mail per +
012000120424     d                                     invio file excel:')
012100120424     d xlsF            c                   const('Excel troppo grande:+
012200120424     d                                     posto nella cartella')
012300080317      *------------------------------------------------------------------------*
012400080317      *   P R O T O T I P I
012500080317      *------------------------------------------------------------------------*
012600080829      /copy gaitrasrc/srcprotopr,tibs34r
012700110617      /copy gaitrasrc/srcprotopr,trtcm8r1
012800080829      *------------------------------------------------------------------------*
012900080829     Ifiar5000      81
013000080829     Ifiar5p00      82
013100001027     C*---------------------------------------------------------------*
013200001027     C     *ENTRY        PLIST
013300080325     C                   PARM                    kpjba
013400080317      /free
013500080828
013600120405       // File excel generico
013700120405
013800120405       select    ;
013900120405       when  listaGEN='S'  ;
014000120405
014100120405       exsr  SQLGEN  ;
014200120405
014300080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
014400080829
014500120405 1     when STricezDDT='SI'        ;
014600080828
014700080828       // Prepara SQL
014800080901          clear wsta          ;
014900080902          EXSR SQLINV         ;
015000080902
015100080902       // Stampa le non inventariate
015200080902
015300120405 1     when STricezDDT='NO'        ;
015400080902          clear wsta          ;
015500080902          clear totale        ;
015600080902          EXSR SQLDSP      ;
015700120405 1     endsl               ;
015800080901
015900080901       //  Esegui SQL  e stampa
016000080901          EXSR SQLesegui   ;
016100080901
016200080901 2     if   wsta='S'          ;
016300120124 3     //**if  righe>59           ;
016400120124 3     if  righe>57           ;
016500080901       write  LsV3PT1         ;
016600080901 3     endif                  ;
016700080901
016800120406       if *in01   ;
016900120302       staEM =%trim(staemail)+'@brt.it' ;
017000120406       endif  ;
017100120406
017200080901       write  LsV3Pfine       ;
017300120406 2     endif               ;
017400110617
017500120406        if listaGEN='S'  ;
017600120424        select   ;
017700120424        when *in01 and contarec <=2000  ;
017800120424        stainvio=xlsE  ;
017900120406        staEM =%trim(staemail)+'@brt.it' ;
018000120424
018100120424        when *in01 and contarec > 2000  ;
018200120424        stainvio=xlsF  ;
018300120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
018400120504                               '\File Excel\WFDDT00F'  ;
018500120424
018600120424        other ;
018700120406        staEM='NESSUNA bolla elaborata:file excel non creato';
018800120424        endsl  ;
018900120406
019000120406        write  LsV3PT2         ;
019100120406        endif  ;
019200120406
019300120424        // Se scritto almeno un record,
019400120424
019500120424  1     if *in01  ;
019600120424
019700110617          pInIDJOB=knmeb       ;
019800120503          clear acorag  ;
019900120503  2       if  staccm>0   ;
020000120503          chain (1:dutkci:staccm) cnaco00f  ;
020100120503  3       if not %found(cnaco00f)  ;
020200120503            acorag=*all'?'  ;
020300120503  3       endif  ;
020400120503  2       endif ;
020500120503
020600120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
020700120503                       %subst(acorag:1:15)  ;
020800120424
020900120424        //      meno di 2000 record --> invio all'indirizzo e-mail
021000120424  1a      if contarec<= 2000 ;
021100120509            pInFILE='WFDDT00F' ;
021200120509            pInMRAKE2='XLS_E'  ;
021300120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
021400111207
021500120424  x1a     else  ;
021600120509          pInFILE='WFDDT00F' ;
021700120509            pInMRAKE2='XLS_F'  ;
021800120424  1       endif ;
021900120424
022000110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
022100120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
022200110617        endif  ;
022300080901
022400080829
022500080317        *inlr ='1'   ;
022600080327       //-------------------------------------------------------------------
022700080901       BEGSR  SQLDSP                 ;
022800080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
022900080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
023000080828
023100080901
023200080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
023300080829                        and substr(ar5uni, 115, 1)='' ''';
023400080902
023500080902        if stalnp>0    ;
023600080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
023700080902        endif          ;
023800080902
023900080829        ENDSR     ;
024000080828
024100080901       //-------------------------------------------------------------------
024200080901       BEGSR  SQLINV                 ;
024300080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
024400080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
024500080901
024600080901        // STampa
024700080901        if cominv>0     ;
024800080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
024900080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
025000080901                        and substr(ar5uni, 115, 1)='' ''';
025100080901        endif     ;
025200080901
025300080901        // RIstampa
025400080901
025500080901        if cominvra>0  ;
025600080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
025700080901                    substr(ar5uni, 107, 8) <= ''' +
025800080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
025900080901                    %char(cominvrd) + '''' ;
026000080901        endif     ;
026100080902
026200080902        if stalnp>0    ;
026300080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
026400080902        endif          ;
026500080902
026600080901        ENDSR     ;
026700120405       //-------------------------------------------------------------------
026800120405       BEGSR  SQLGEN                 ;
026900120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
027000120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
027100120405
027200120405        // Solo NON inventariate
027300120405        select   ;
027400120405        when  v1cinve='E'  ;
027500120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
027600120405
027700120411        // Solo     inventariate  senza data
027800120411        when  v1cinve='S'  and cominvra=0  ;
027900120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
028000120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
028100120411        // Solo     inventariate  con   data
028200120411        when   cominvra>0  ;
028300120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
028400120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
028500120411                    %char(cominvrd) + '''' ;
028600120405        other  ;
028700120405        // tutte
028800120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
028900120405        endsl  ;
029000120405
029100120405        // solo con   MOTIVO DDT ERRATO
029200120405        if  v1cerra='S'  ;
029300120405        wrkgetlista=wrkgetlista + ' and +
029400120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
029500120405        endif  ;
029600120405        // solo senza MOTIVO DDT ERRATO
029700120405        if  v1cerra='E'  ;
029800120405        wrkgetlista=wrkgetlista + ' and +
029900120405                                    substr(ar5uni, 167, 8) =   ''        ''';
030000120405        endif  ;
030100120405
030200120405        if stalnp>0    ;
030300120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
030400120405        endif          ;
030500120405
030600120405        ENDSR     ;
030700120405
030800080829       //-------------------------------------------------------------------
030900080829       BEGSR  SQLesegui              ;
031000080828       EXEC sql  prepare s1 from : wrkgetlista ;
031100080828       EXEC sql  declare a1 cursor for s1      ;
031200080828       EXEC sql  open    a1                    ;
031300080828
031400080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
031500080828                                        :ar5nsp, :ar5uni           ;
031600080828
031700120424       clear contarec ;
031800120424
031900080828       dow sqlcod=0    ;
032000080828
032100080829       exsr STAMPA     ;
032200080829
032300080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
032400080828                                        :ar5nsp, :ar5uni           ;
032500080828       enddo           ;
032600080828
032700080829       EXEC sql  CLOSE   a1                    ;
032800080829
032900080828       ENDSR     ;
033000080327       //-------------------------------------------------------------------
033100080325       BEGSR  Stampa          ;
033200080325
033300080325       // Cerco la bolla
033400080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
033500080901
033600120406 1     if %found(titas30c) and (STricezDDT='SI' or ListaGEN='S' or
033700110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
033800120406
033900120406       // Controllo la data spedizione se richiesta
034000120406 1c    if listaGEN <>'S' or comsped=0 or
034100120406             (tasdsp>=comsped and tasdsp<=comspea) ;
034200120406
034300120406       // Controllo la data consegna  se richiesta
034400120406 1d    if listaGEN <>'S' or comdsd=0 or
034500120406             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
034600120406
034700100603
034800100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
034900100603       clear ds7R ;
035000100603 2     if tasgma<>*blanks  ;
035100100603
035200100603         clear tblkey    ;
035300100603         tblkey=tasgma    ;
035400100603         chain (1:'7R':tblkey) tabel00f   ;
035500100603          if %found(tabel00f)   ;
035600100603          ds7r=tbluni       ;
035700100603          endif             ;
035800100603 2        endif             ;
035900100603
036000100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
036100080901
036200100528       // Escludo il cliente mittente se selezionato
036300100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
036400100528
036500080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
036600080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
036700080902 2     if not %found(fnlbl01l) ;
036800080902
036900080829       stplnp=taslnp          ;
037000080829       stpnrs=tasnrs          ;
037100080327         if stpnrs>0            ;
037200080327         tra01='-'              ;
037300080327         else                   ;
037400080327         tra01=' '              ;
037500080327         endif                  ;
037600080829       stpnsp=tasnsp          ;
037700080829       dataiso=%date(tasdsp)    ;
037800080327       datadmy=dataiso        ;
037900080327       stpDSP=%dec(datadmy)   ;
038000080829       stplna=taslna          ;
038100080829       stpRMN=tasRMN          ;
038200080902
038300080902       // Data consegna        ;
038400080902       clear stpdcm  ;
038500080902       clear stpcca    ;
038600120406       clear ds7o      ;
038700120423       CLEAR LBLLAN  ;
038800080902
038900080902 3     if tasdcm>0   ;
039000080902       DATAEUR=%DATE(tasdcm:*iso)       ;
039100080902       stpdcm=%dec(dataeur)   ;
039200080902 4     if tascca<>' '  ;
039300080902         clear tblkey    ;
039400080902         tblkey=tascca    ;
039500080902         stpcca=tascca    ;
039600080902
039700080902         chain (1:'7O':tblkey) tabel00f   ;
039800080902          if %found(tabel00f)   ;
039900080902          ds7o=tbluni       ;
040000080902           %subst(stpcca:2:10)='-'+tbluni   ;
040100080902          endif       ;
040200080902 4     endif       ;
040300120406
040400120406       // Se la consegna anomala non è reso, ma dirottamento,
040500120406       //  verifico se c'e' un'altra figlia resa
040600120406 4     if §7ofdi='D'  ;
040700120406       lblaan=ar5aas ;
040800120406       lbllpn=ar5lnp ;
040900120406       lblnrn=ar5nrs ;
041000120406       lblnsn=ar5nsp ;
041100120406
041200120406 5     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
041300120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
041400120406 6     if  %found(fnlbl02l)  ;
041500120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
041600120406         clear ds7o      ;
041700120406 7     if tascca<>' '  ;
041800120406         clear tblkey    ;
041900120406         tblkey=tascca    ;
042000120406         chain (1:'7O':tblkey) tabel00f   ;
042100120406          if %found(tabel00f)   ;
042200120406           ds7o=tbluni       ;
042300120406          endif  ;
042400120406 7     endif  ;
042500120406 6     endif  ;
042600120406
042700120406 5     enddo ;
042800120406
042900120406       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
043000120406 4     endif       ;
043100120406 3     endif       ;
043200080829
043300080902       // Escludo dalla stampa delle non inventariate le bolle di reso
043400080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
043500080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
043600080902 4     IF %FOUND(TITA430C)    ;
043700080829       STPRMA=ta4NOT          ;
043800080829       ELSE                   ;
043900080829       clear stprma           ;
044000080902 4     endif                  ;
044100080829
044200080901       stprsd=tasrsd          ;
044300080829       stplod=taslod          ;
044400080829       %subst(stplod:13:3)=' '+tasprd    ;
044500080901
044600080901       // Data inventario      ;
044700080829       dar5gen=ar5uni     ;
044800080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
044900080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
045000080901         stpinv=%dec(dataeur)   ;
045100080829       else   ;
045200080901        clear stpinv    ;
045300080902 4     endif           ;
045400080327
045500080325
045600120406       // Per lista generica non stampo il dettaglio
045700120406       if ListaGEN<>'S'  ;
045800120406
045900080902       // totale bolle stampate
046000080902       totale=totale+1        ;
046100080902
046200080327       // Testata bolla
046300080327
046400110617 4     if  righe>63           ;
046500080829       write  LsV3PT1         ;
046600080328       righe=8                ;
046700080902 4     endif                  ;
046800080331
046900080829       write   LsV3PD1        ;
047000080327
047100080327       righe=righe+1          ;
047200080829       wsta='S'               ;
047300120406       endif  ;
047400080829
047500110613       // Se richiesto --> creo file
047600110613       if   stasfile ='S'  ;
047700110613       clear wfddt000         ;
047800110616       ddtidute=knmeb   ;
047900110616       ddteut=knmus   ;
048000110613       ddtepo=dutpou  ;
048100110613       ddtedt=dateu   ;
048200110613       ddteor=ora_u   ;
048300110613       ddtccm=tasccm  ;
048400110616       // ragione sociale mittente
048500110616       if staccm=0  ;
048600110616       chain  (1:dutkci:tasccm) cnaco00f  ;
048700110616        if %found(cnaco00f)  ;
048800110616         ddtrsm=acorag   ;
048900110616        else  ;
049000110616        ddtrsm=*all'?' ;
049100110616        endif ;
049200110616       else  ;
049300110616        ddtrsm=acorag  ;
049400110616       endif  ;
049500110616
049600110613       ddtaas=tasaas  ;
049700110613       ddtlnp=taslnp  ;
049800110613       ddtnrs=tasnrs  ;
049900110613       ddtnsp=tasnsp  ;
050000110613       ddtmgs=tasmgs  ;
050100120423       IF LBLLAN>0    ;
050200120423        ddtlna=lbllan  ;
050300120423        ELSE   ;
050400120423        ddtlna=taslna  ;
050500120423        ENDIF  ;
050600110613       ddtrmn=tasrmn  ;
050700110613       ddtrma=stprma  ;
050800110613       ddtrsd=tasrsd  ;
050900110613       ddtlod=taslod  ;
051000110613       ddtcad=tascad  ;
051100110613       ddtprd=tasprd  ;
051200110613       ddtnzd=tasnzd  ;
051300110613       ddtdcm=tasdcm  ;
051400110613       ddtcca=tascca  ;
051500120320       // Area da organigramma
051600120423       chain ddtlna    azorg01l  ;
051700120320       if %found(azorg01l)  ;
051800120320       ddtcar=orgcar  ;
051900120320       endif  ;
052000120320
052100110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
052200110616         ddtinv =%int(§ar5drdt)             ;
052300110616       else   ;
052400110616        clear ddtinv    ;
052500110616 4     endif           ;
052600120402
052700120402       // Motivazione DDt errato
052800120402       if §ar5mtvdt<>*blanks   ;
052900120402       clear  tbeke1  ;
053000120402       clear  tbeke2  ;
053100120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
053200120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
053300120402
053400120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
053500120402        if %found(tntbe01l)  ;
053600120402         ddtmtvdt=tbeuni  ;
053700120402        endif   ;
053800120402
053900120402 4       if §ar5dimtv>*zeros     ;
054000120402           ddtdimtv =%int(§ar5dimtv)             ;
054100120402         else   ;
054200120402          clear ddtdimtv  ;
054300120402 4       endif           ;
054400120402 4     endif           ;
054500120402
054600110613       write wfddt000   ;
054700120424
054800120424       contarec=contarec +1 ;
054900110616
055000110616       *in01 =*on  ;
055100110613       endif  ;
055200110613
055300080829       // Aggiorno fiar5 se stampa
055400120406
055500120406 4     if cominv>0 and listagen<>'S'  ;
055600080829       *in81=*off     ;
055700080829       *in82=*off     ;
055800080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
055900080902 5       if %found(fiar531c)    ;
056000080829          dar5gen=ar5uni      ;
056100080829          §ar5fstd='S'        ;
056200080829          ar5uni=dar5gen       ;
056300080902 6          if *in81      ;
056400080829            update fiar5000    ;
056500080829            else    ;
056600080829            update fiar5p00    ;
056700080902 6          endif   ;
056800080902 5       endif    ;
056900080902 4     endif    ;
057000080901
057100080902 3     endif    ;
057200080902 2     endif    ;
057300100603 1b    endif    ;
057400100603 1a    endif    ;
057500120406 1c    endif    ;
057600120406 1d    endif    ;
057700080902 1     endif    ;
057800080327
057900080325       ENDSR                  ;
058000080327       //-------------------------------------------------------------------
058100080327       BEGSR  *INZSR                 ;
058200080328
058300080328       in(E) *dtaara   ;
058400080328       if    %error or rsut  =*blanks  ;
058500080328       callp TIBS34R (TIBS34DS)        ;
058600080328       in    *dtaara   ;
058700080328       endif                           ;
058800080829
058900080829       // Apertura file di sede
059000080829             open(e) titas30c                ;
059100080829           if      not %open(titas30c)  ;
059200110613             %subst(WTA430:7:4)='GRU '    ;
059300110613             %subst(WTAs30:7:4)='GRU '    ;
059400110613             %subst(WTA531:7:4)='GRU '    ;
059500110617             %subst(WWFDDT:7:4)='AZM '    ;
059600110613             %subst(Wlib:7:4)='GRU '    ;
059700080829             open titas30c                ;
059800110617           endif  ;
059900080829             open tita430c                ;
060000080829             open fiar531c                ;
060100110617             open wfddt01l                ;
060200080829
060300080829       fnlsv2ds=kpjbu     ;
060400120405       clear comsped  ;
060500120405       clear comspea  ;
060600120405       clear stasped  ;
060700120405       clear staspea  ;
060800120411
060900080829
061000120405 1     if ListaGEN='S'     ;
061100120405       stptes   =tes2     ;
061200120405       stpdecinv=dec3     ;
061300120405       stpdec3  =dec4     ;
061400120411       stpdec4  =dec5     ;
061500120405 x1    else  ;
061600120405
061700080909       stptes   =tes1     ;
061800120411
061900120411       if cominvra=0      ;
062000120411       cominvra=cominvrd  ;
062100120411       stainvra=stainvrd  ;
062200120411       endif              ;
062300080829
062400120411 2     if cominv>0        ;
062500080901       stpdecinv=dec1     ;
062600080909       %subst(stptes2:11:26)  =Inv      ;
062700080829       stpdata1=stainv    ;
062800080829       clear    stpdata2  ;
062900080829       clear    stpdecinv2;
063000080902       STricezDDT='SI'      ;
063100120411 2     endif              ;
063200080829
063300120411
063400120411 2     if cominvra>0      ;
063500080909       %subst(stptes2:11:26)  =RiInv      ;
063600080901       stpdecinv=dec2     ;
063700080901       stpdecinv2= 'AL'   ;
063800080829       stpdata1=stainvrd  ;
063900080829       stpdata2=stainvra  ;
064000120411       STricezDDT='SI'      ;
064100120411 2     endif              ;
064200120411 1     endif              ;
064300120411
064400120411 1     if ListaGEN='S'   ;
064500120411 2     if cominvrd>0  or cominvra>0 ;
064600120411
064700120411 3      if cominvrd>0  and cominvra=0 ;
064800120411        cominvra=cominvrd  ;
064900120411 3      Endif            ;
065000120411 3      if cominvrd=0               ;
065100120411        cominvrd=19900101  ;
065200120411 3      Endif            ;
065300120411
065400120411 3     select     ;
065500120411       when stainvrd=0        ;
065600120411         clear    stpdata6  ;
065700120411         clear    stpdec44  ;
065800120411         %subst(stpdec4:21:4)='AL :'     ;
065900120411         stpdata5=stainvra  ;
066000120411
066100120411       when stainvra=0        ;
066200120411         clear    stpdata6  ;
066300120411         clear    stpdec44  ;
066400120411         %subst(stpdec4:21:4)='IL :'     ;
066500120411         stpdata5=stainvrd  ;
066600120411
066700120411       other                ;
066800120411         stpdata5=stainvrd  ;
066900120411         stpdata6=stainvra  ;
067000120411         stpdec44  = 'AL'   ;
067100120411 3      endSL              ;
067200120411 2     endif              ;
067300120411 1     endif              ;
067400080829
067500120411       // data CONSEGNA
067600120405 1     if comdsd>0  or comdsa>0   ;
067700120411
067800120405 2     if ListaGEN<>'S'     ;
067900120405         STricezDDT='NO'      ;
068000120405         stptes2  =NOInv    ;
068100120405         stpdecinv=dec3     ;
068200120405       endif  ;
068300080901
068400080901        if comdsd>0  and comdsa=0 ;
068500080901        comdsa=comdsd    ;
068600080901        Endif            ;
068700080901        if comdsd=0               ;
068800080901        comdsd=19900101  ;
068900080901        Endif            ;
069000080901
069100080901       select     ;
069200080901       when stadsd=0        ;
069300080901         clear    stpdata2  ;
069400080901         clear    stpdecinv2;
069500080901         %subst(stpdecinv:21:4)='AL :'     ;
069600080901         stpdata1=stadsa    ;
069700080901
069800080901       when stadsa=0        ;
069900080901         clear    stpdata2  ;
070000080901         clear    stpdecinv2;
070100080901         %subst(stpdecinv:21:4)='IL :'     ;
070200080901         stpdata1=stadsd    ;
070300080901
070400080901       other                ;
070500080901         stpdata1=stadsd    ;
070600080901         stpdata2=stadsa    ;
070700080901         stpdecinv2= 'AL'   ;
070800080901        endSL              ;
070900080901       endif              ;
071000120405
071100120405       // Data spedizione DAL/AL
071200120405
071300120405       monitor   ;
071400120405       if alfsped>*zeros  ;
071500120405       comsped=%int(alfsped) ;
071600120405         dataiso=%date(comsped)  ;
071700120405         dataeur=dataiso;
071800120406         stasped=%dec(dataeur)  ;
071900120405       endif  ;
072000120405
072100120405       on-error  ;
072200120405       endmon  ;
072300120405       monitor   ;
072400120405       if alfspea>*zeros  ;
072500120405         comspea=%int(alfspea) ;
072600120405         dataiso=%date(comspea)  ;
072700120405         dataeur=dataiso;
072800120406         staspea=%dec(dataeur)  ;
072900120405       endif  ;
073000120405
073100120405       on-error  ;
073200120405       endmon  ;
073300120405
073400120405 1     if comsped>*zeros  or comspea>0   ;
073500120405        if comsped>0  and comspea=0 ;
073600120405        comspea=comsped   ;
073700120405        Endif            ;
073800120405        if comsped=0               ;
073900120406        comsped=19900101  ;
074000120405        Endif            ;
074100120405
074200120405       // Date in ggmmaaaa
074300120405       select     ;
074400120405       when stasped=0        ;
074500120405         clear    stpdata4  ;
074600120405         clear    stpdec33  ;
074700120405         %subst(stpdec3:21:4)='AL :'     ;
074800120405         stpdata3=staspea   ;
074900120405
075000120405       when staspea=0        ;
075100120405         clear    stpdata4  ;
075200120405         clear    stpdec33  ;
075300120405         %subst(stpdec3:21:4)='IL :'     ;
075400120405         stpdata3=stasped   ;
075500120405
075600120405       other                ;
075700120406         stpdata3=stasped   ;
075800120406         stpdata4=staspea   ;
075900120405         stpdec33  = 'AL'   ;
076000120405        endSL              ;
076100120405       endif              ;
076200080902
076300080902       // Linea di partenza
076400080902       if stalnp=0  ;
076500080902       stpdlnp='TUTTE' ;
076600080902       else           ;
076700080903       stpdlnp='-'+ stadlnp ;
076800080902       endif          ;
076900080902
077000080829       righe=70   ;
077100110613       // imposto data e ora del giorno
077200110613       dataiso=datasys  ;
077300110613       dateu=%dec(dataiso)   ;
077400110616
077500110616       dataiso=dataiso-%days(30) ;
077600110616       unmese=%dec(dataiso)   ;
077700110616
077800110613       ora_iso=ora_sys  ;
077900110613       ora_u=%dec(ora_iso)   ;
078000080829
078100110616       // Se richiesto file pulisco il lancio precedente per
078200110616       //  fil e ccm
078300110616       setll (dutpou: staccm)  wfddt01l   ;
078400110616       reade (dutpou: staccm)  wfddt01l   ;
078500110616
078600110616       if not %eof(wfddt01l)  and
078700110616          (stasfile='S'  or ddtedt<unmese) ;
078800110616
078900110616       dow  not %eof(wfddt01l)   ;
079000110616       delete wfddt000  ;
079100110616
079200110616       reade (dutpou: staccm)  wfddt01l   ;
079300110616       enddo  ;
079400110616        else  ;
079500110616        unlock   wfddt01l  ;
079600110616        endif  ;
079700110616
079800120405       // Ragione sociale Mittente
079900110617        if stasfile='S'   and staccm>0 ;
080000110616        chain  (1:dutkci:staccm) cnaco00f  ;
080100110616         if not %found(cnaco00f)  ;
080200110616          acorag=*all'?'  ;
080300110616         endif ;
080400110616       endif ;
080500120405
080600120405       // Solo file excel--> stmapo testata richieste
080700120405
080800120405       if listaGEN='S'    ;
080900120405       // Bolle inventariate
081000120405       if v1cinve='S'   ;
081100120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
081200120405       endif  ;
081300120405       if v1cinve='E'   ;
081400120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
081500120405       endif  ;
081600120405       if v1cerra='S'   ;
081700120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
081800120405       endif  ;
081900120405       if v1cerra='E'   ;
082000120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
082100120405       endif  ;
082200120405
082300120405       endif  ;
082400110616
082500080328       ENDSR                           ;
082600080317      /end-free
