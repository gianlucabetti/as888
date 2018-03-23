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
003900120720     d dAR5doc       e ds                  qualified  inz
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
005200080828     D fnlsv2ds        DS
005300120720     D*//stainv                 1      8  0
005400120720     D*//cominv                 9     16  0
005500080828     D  stainvrd              17     24  0
005600080828     D  cominvrd              25     32  0
005700080828     D  stainvra              33     40  0
005800080828     D  cominvra              41     48  0
005900080828     D  stadsd                49     56  0
006000080828     D  comdsd                57     64  0
006100080828     D  stadsa                65     72  0
006200080828     D  comdsa                73     80  0
006300080828     D  staccm                81     87  0
006400080829     D  staRAG                88    112
006500080902     D  stalnp               113    115  0
006600080902     D  stadlnp              116    140
006700110613     D  stasfile             141    141
006800110617     D  staemail             142    171
006900120720     D*
007000120405     D* Nuove parzializzazioni per lista generica
007100120405     d  listaGEN             172    172
007200120405     D  alfsped              173    180
007300120405     D  alfspea              181    188
007400120405     D  v1cinve              189    189
007500120405     D  v1cerra              190    190
007600120720     d* Data scannerizzazione
007700120704     D  comscand             191    198  0
007800120704     D  comscana             199    206  0
007900120704     d*   Per il momento le date restituzione al mittente sono oscurate e non
008000120704     d*    usate
008100120704     D  cominvimD            207    214  0
008200120704     D  cominvimA            215    222  0
008300120704     D  v1cinvim             223    223
008400120720     d*
008500120704     D  v1cscan              224    224
008600120704     D  v1csta               225    225
008700120720     d***
008800110617     D  pInFILE        s             10
008900110617     D  pInIDJOB       s             26
009000110617     D  pInPWDXLS      s             15
009100110617     D  pInNOMXLS      s             78
009200110617     D  pInEMLDEST     s            121
009300111207     D  pInEMLVAR      s             80
009400120509     D  pInMRAKE2      s             15
009500120509     D  pOutESITO      s              1
009600080829     d savspe          s                   like(tasspe)
009700080829     d righe           s              3  0
009800110613     d dateu           s              8  0
009900110616     d unmese          s              8  0
010000110613     d ora_u           s              6  0
010100080829     d Wsta            s              1
010200080902     d STricezDDT      s              2
010300110613     d wlib            s             10    inz('GAITRAGRPS')
010400120405     d comsped         s              8  0
010500120405     d comspea         s              8  0
010600120405     d stasped         s              8  0
010700120405     d staspea         s              8  0
010800120424     d contarec        s              8  0
010900080828     d wrkgetlista     s           4096    varying
011000110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
011100110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
011200110613     d Ora_iso         s               t   timfmt(*iso)
011300110613     d Dataiso         s               d   datfmt(*iso)
011400080327     d Dataeur         s               d   datfmt(*eur)
011500080327     d Datadmy         s               d   datfmt(*dmy)
011600120720     d*
011700110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
011800110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
011900110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
012000110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
012100080901     d dec1            c                   const('  Stampa Inventario  AL:')
012200080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
012300120406     d dec3            c                   const('   Data   consegna  DAL:')
012400120405     d dec4            c                   const('   Data spedizione  DAL:')
012500120411     d dec5            c                   const('   Data inventario  DAL:')
012600080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
012700080909     D                                     ESTITUIRE AL MITTENTE *')
012800120405     d tes2            c                   const('** EXCEL  DOCUMENTI  DA R-
012900120405     D                                     ESTITUIRE AL MITTENTE **')
013000080909     d Inv             c                   const('  Documenti inventariati  ')
013100080909     d RIinv           c                   const('Documenti già inventariati')
013200080909     d NOinv           c                   const(' Documenti non inventariat-
013300080909     D                                     i per bolle consegnate ')
013400120424     d xlsE            c                   const('Indirizzo e-mail per +
013500120424     d                                     invio file excel:')
013600120424     d xlsF            c                   const('Excel troppo grande:+
013700120424     d                                     posto nella cartella')
013800080317      *------------------------------------------------------------------------*
013900080317      *   P R O T O T I P I
014000080317      *------------------------------------------------------------------------*
014100080829      /copy gaitrasrc/srcprotopr,tibs34r
014200110617      /copy gaitrasrc/srcprotopr,trtcm8r1
014300080829      *------------------------------------------------------------------------*
014400080829     Ifiar5000      81
014500080829     Ifiar5p00      82
014600001027     C*---------------------------------------------------------------*
014700001027     C     *ENTRY        PLIST
014800080325     C                   PARM                    kpjba
014900080317      /free
015000080828
015100120405
015200120405       select    ;
015300120704       when  listaGEN='S'  and v1cinve<>'I'     ;
015400120405
015500120405       exsr  SQLGEN  ;
015600120405
015700080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
015800080829
015900120405 1     when STricezDDT='SI'        ;
016000080828
016100080828       // Prepara SQL
016200080901          clear wsta          ;
016300080902          EXSR SQLINV         ;
016400080902
016500120704       // elabora  le MAI inventariate
016600080902
016700120405 1     when STricezDDT='NO'        ;
016800080902          clear wsta          ;
016900080902          clear totale        ;
017000080902          EXSR SQLDSP      ;
017100120405 1     endsl               ;
017200080901
017300080901       //  Esegui SQL  e stampa
017400080901          EXSR SQLesegui   ;
017500080901
017600080901 2     if   wsta='S'          ;
017700120124 3     //**if  righe>59           ;
017800120124 3     if  righe>57           ;
017900120720       write  LsV6PT1         ;
018000080901 3     endif                  ;
018100080901
018200120406       if *in01   ;
018300120302       staEM =%trim(staemail)+'@brt.it' ;
018400120406       endif  ;
018500120406
018600120720       write  LsV6Pfine       ;
018700120406 2     endif               ;
018800110617
018900120406        if listaGEN='S'  ;
019000120424        select   ;
019100120424        when *in01 and contarec <=2000  ;
019200120424        stainvio=xlsE  ;
019300120406        staEM =%trim(staemail)+'@brt.it' ;
019400120424
019500120424        when *in01 and contarec > 2000  ;
019600120424        stainvio=xlsF  ;
019700120504        staEM ='N:\BARTMAIL\'+ %editc(dutpou:'X')+
019800120504                               '\File Excel\WFDDT00F'  ;
019900120424
020000120424        other ;
020100120406        staEM='NESSUNA bolla elaborata:file excel non creato';
020200120424        endsl  ;
020300120406
020400120720        write  LsV6PT2         ;
020500120406        endif  ;
020600120406
020700120424        // Se scritto almeno un record,
020800120424
020900120424  1     if *in01  ;
021000120424
021100110617          pInIDJOB=knmeb       ;
021200120503          clear acorag  ;
021300120503  2       if  staccm>0   ;
021400120503          chain (1:dutkci:staccm) cnaco00f  ;
021500120503  3       if not %found(cnaco00f)  ;
021600120503            acorag=*all'?'  ;
021700120503  3       endif  ;
021800120503  2       endif ;
021900120503
022000120503          pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
022100120503                       %subst(acorag:1:15)  ;
022200120424
022300120424        //      meno di 2000 record --> invio all'indirizzo e-mail
022400120424  1a      if contarec<= 2000 ;
022500120509            pInFILE='WFDDT00F' ;
022600120509            pInMRAKE2='XLS_E'  ;
022700120503            pInEMLDEST=%trim(staemail)+'@brt.it' ;
022800111207
022900120424  x1a     else  ;
023000120509          pInFILE='WFDDT00F' ;
023100120509            pInMRAKE2='XLS_F'  ;
023200120424  1       endif ;
023300120424
023400110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
023500120509                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
023600110617        endif  ;
023700080901
023800080829
023900080317        *inlr ='1'   ;
024000080327       //-------------------------------------------------------------------
024100080901       BEGSR  SQLDSP                 ;
024200080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
024300080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
024400080828
024500080901
024600080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
024700080829                        and substr(ar5uni, 115, 1)='' ''';
024800080902
024900080902        if stalnp>0    ;
025000080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
025100080902        endif          ;
025200080902
025300080829        ENDSR     ;
025400080828
025500080901       //-------------------------------------------------------------------
025600080901       BEGSR  SQLINV                 ;
025700080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
025800080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
025900080901
026000080901        // STampa
026100120720        //if cominv>0     ;
026200120720        //wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
026300120720        //        %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
026400120720        //              and substr(ar5uni, 115, 1)='' ''';
026500120720        //endif     ;
026600080901
026700080901        // RIstampa
026800080901
026900080901        if cominvra>0  ;
027000080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
027100080901                    substr(ar5uni, 107, 8) <= ''' +
027200080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
027300080901                    %char(cominvrd) + '''' ;
027400080901        endif     ;
027500080902
027600080902        if stalnp>0    ;
027700080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
027800080902        endif          ;
027900080902
028000080901        ENDSR     ;
028100120405       //-------------------------------------------------------------------
028200120405       BEGSR  SQLGEN                 ;
028300120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
028400120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
028500120405
028600120405        // Solo NON inventariate
028700120405        select   ;
028800120405        when  v1cinve='E'  ;
028900120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
029000120405
029100120411        // Solo     inventariate  senza data
029200120411        when  v1cinve='S'  and cominvra=0  ;
029300120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
029400120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
029500120411        // Solo     inventariate  con   data
029600120411        when   cominvra>0  ;
029700120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
029800120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
029900120411                    %char(cominvrd) + '''' ;
030000120405        other  ;
030100120405        // tutte
030200120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
030300120405        endsl  ;
030400120405
030500120405        // solo con   MOTIVO DDT ERRATO
030600120405        if  v1cerra='S'  ;
030700120405        wrkgetlista=wrkgetlista + ' and +
030800120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
030900120405        endif  ;
031000120405        // solo senza MOTIVO DDT ERRATO
031100120405        if  v1cerra='E'  ;
031200120405        wrkgetlista=wrkgetlista + ' and +
031300120405                                    substr(ar5uni, 167, 8) =   ''        ''';
031400120405        endif  ;
031500120405
031600120405        if stalnp>0    ;
031700120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
031800120405        endif          ;
031900120405
032000120405        ENDSR     ;
032100120405
032200080829       //-------------------------------------------------------------------
032300080829       BEGSR  SQLesegui              ;
032400080828       EXEC sql  prepare s1 from : wrkgetlista ;
032500080828       EXEC sql  declare a1 cursor for s1      ;
032600080828       EXEC sql  open    a1                    ;
032700080828
032800080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
032900080828                                        :ar5nsp, :ar5uni           ;
033000080828
033100120424       clear contarec ;
033200120424
033300080828       dow sqlcod=0    ;
033400080828
033500080829       exsr STAMPA     ;
033600080829
033700080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
033800080828                                        :ar5nsp, :ar5uni           ;
033900080828       enddo           ;
034000080828
034100080829       EXEC sql  CLOSE   a1                    ;
034200080829
034300080828       ENDSR     ;
034400080327       //-------------------------------------------------------------------
034500080325       BEGSR  Stampa          ;
034600080325
034700080325       // Cerco la bolla
034800080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
034900080901
035000120406 1     if %found(titas30c) and (STricezDDT='SI' or ListaGEN='S' or
035100110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
035200120406
035300120406       // Controllo la data spedizione se richiesta
035400120406 1c    if listaGEN <>'S' or comsped=0 or
035500120406             (tasdsp>=comsped and tasdsp<=comspea) ;
035600120406
035700120406       // Controllo la data consegna  se richiesta
035800120406 1d    if listaGEN <>'S' or comdsd=0 or
035900120406             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
036000120406
036100100603
036200100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
036300100603       clear ds7R ;
036400100603 2     if tasgma<>*blanks  ;
036500100603
036600100603         clear tblkey    ;
036700100603         tblkey=tasgma    ;
036800100603         chain (1:'7R':tblkey) tabel00f   ;
036900100603          if %found(tabel00f)   ;
037000100603          ds7r=tbluni       ;
037100100603          endif             ;
037200100603 2        endif             ;
037300100603
037400100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
037500080901
037600100528       // Escludo il cliente mittente se selezionato
037700100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
037800100528
037900080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
038000080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
038100080902 2     if not %found(fnlbl01l) ;
038200080902
038300080829       stplnp=taslnp          ;
038400080829       stpnrs=tasnrs          ;
038500080327         if stpnrs>0            ;
038600080327         tra01='-'              ;
038700080327         else                   ;
038800080327         tra01=' '              ;
038900080327         endif                  ;
039000080829       stpnsp=tasnsp          ;
039100080829       dataiso=%date(tasdsp)    ;
039200080327       datadmy=dataiso        ;
039300080327       stpDSP=%dec(datadmy)   ;
039400080829       stplna=taslna          ;
039500080829       stpRMN=tasRMN          ;
039600080902
039700080902       // Data consegna        ;
039800080902       clear stpdcm  ;
039900080902       clear stpcca    ;
040000120406       clear ds7o      ;
040100120423       CLEAR LBLLAN  ;
040200080902
040300080902 3     if tasdcm>0   ;
040400080902       DATAEUR=%DATE(tasdcm:*iso)       ;
040500080902       stpdcm=%dec(dataeur)   ;
040600080902 4     if tascca<>' '  ;
040700080902         clear tblkey    ;
040800080902         tblkey=tascca    ;
040900080902         stpcca=tascca    ;
041000080902
041100080902         chain (1:'7O':tblkey) tabel00f   ;
041200080902          if %found(tabel00f)   ;
041300080902          ds7o=tbluni       ;
041400080902           %subst(stpcca:2:10)='-'+tbluni   ;
041500080902          endif       ;
041600080902 4     endif       ;
041700120406
041800120406       // Se la consegna anomala non è reso, ma dirottamento,
041900120406       //  verifico se c'e' un'altra figlia resa
042000120406 4     if §7ofdi='D'  ;
042100120406       lblaan=ar5aas ;
042200120406       lbllpn=ar5lnp ;
042300120406       lblnrn=ar5nrs ;
042400120406       lblnsn=ar5nsp ;
042500120406
042600120406 5     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
042700120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
042800120406 6     if  %found(fnlbl02l)  ;
042900120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
043000120406         clear ds7o      ;
043100120406 7     if tascca<>' '  ;
043200120406         clear tblkey    ;
043300120406         tblkey=tascca    ;
043400120406         chain (1:'7O':tblkey) tabel00f   ;
043500120406          if %found(tabel00f)   ;
043600120406           ds7o=tbluni       ;
043700120406          endif  ;
043800120406 7     endif  ;
043900120406 6     endif  ;
044000120406
044100120406 5     enddo ;
044200120406
044300120406       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
044400120406 4     endif       ;
044500120406 3     endif       ;
044600080829
044700080902       // Escludo dalla stampa delle non inventariate le bolle di reso
044800080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
044900080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
045000080902 4     IF %FOUND(TITA430C)    ;
045100080829       STPRMA=ta4NOT          ;
045200080829       ELSE                   ;
045300080829       clear stprma           ;
045400080902 4     endif                  ;
045500080829
045600080901       stprsd=tasrsd          ;
045700080829       stplod=taslod          ;
045800080829       %subst(stplod:13:3)=' '+tasprd    ;
045900080901
046000080901       // Data inventario      ;
046100080829       dar5gen=ar5uni     ;
046200080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
046300080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
046400080901         stpinv=%dec(dataeur)   ;
046500080829       else   ;
046600080901        clear stpinv    ;
046700080902 4     endif           ;
046800120720
046900120720               // -?Motivo ricezione DDT errati?
047000120720               if  §AR5mtvDt <> *blank;
047100120720                 STPmtvDt = §AR5mtvDt;
047200120720               else;
047300120720                 clear  STPmtvDt;
047400120720               endif;
047500120720
047600120720               // -?Data inserimento motivo ricezione DDT errati?
047700120720               if  §AR5diMtv > *zero and  §AR5diMtv <> *all'9';
047800120720                 Tra02    = '-';
047900120720                 DataEUR  = %date( %int(§AR5diMtv) : *iso );
048000120720                 STPdiMtv = %dec(DataEUR);
048100120720               else;
048200120720                 clear  Tra02;
048300120720                 clear  STPdiMtv;
048400120720               endif;
048500120720
048600120720               // -?Data ricezione immagini documenti?
048700120720               clear  STPdtScan;
048800120720               clear  dAR5doc;
048900120720               chain  (AR5aas : AR5lnp : AR5nrs : AR5nsp : 'DOC')
049000120720                      FIAR531C;
049100120720               if  %found(FIAR531C);
049200120720                 dAR5doc = AR5uni;
049300120720                 if  dAR5doc.§AR5jdRid > *zero;
049400120720                   DataEUR   = %date( %int(dAR5doc.§AR5jdRid) : *iso );
049500120720                   STPdtScan = %dec(DataEUR);
049600120720                 endif;
049700120720               endif;
049800080327
049900080325
050000120406       // Per lista generica non stampo il dettaglio
050100120406       if ListaGEN<>'S'  ;
050200120406
050300080902       // totale bolle stampate
050400080902       totale=totale+1        ;
050500080902
050600080327       // Testata bolla
050700080327
050800110617 4     if  righe>63           ;
050900120720       write  LsV6PT1         ;
051000080328       righe=8                ;
051100080902 4     endif                  ;
051200080331
051300120720       write   LsV6PD1        ;
051400080327
051500080327       righe=righe+1          ;
051600080829       wsta='S'               ;
051700120406       endif  ;
051800080829
051900110613       // Se richiesto --> creo file
052000110613       if   stasfile ='S'  ;
052100110613       clear wfddt000         ;
052200110616       ddtidute=knmeb   ;
052300110616       ddteut=knmus   ;
052400110613       ddtepo=dutpou  ;
052500110613       ddtedt=dateu   ;
052600110613       ddteor=ora_u   ;
052700110613       ddtccm=tasccm  ;
052800110616       // ragione sociale mittente
052900110616       if staccm=0  ;
053000110616       chain  (1:dutkci:tasccm) cnaco00f  ;
053100110616        if %found(cnaco00f)  ;
053200110616         ddtrsm=acorag   ;
053300110616        else  ;
053400110616        ddtrsm=*all'?' ;
053500110616        endif ;
053600110616       else  ;
053700110616        ddtrsm=acorag  ;
053800110616       endif  ;
053900110616
054000110613       ddtaas=tasaas  ;
054100110613       ddtlnp=taslnp  ;
054200110613       ddtnrs=tasnrs  ;
054300110613       ddtnsp=tasnsp  ;
054400110613       ddtmgs=tasmgs  ;
054500120423       IF LBLLAN>0    ;
054600120423        ddtlna=lbllan  ;
054700120423        ELSE   ;
054800120423        ddtlna=taslna  ;
054900120423        ENDIF  ;
055000110613       ddtrmn=tasrmn  ;
055100110613       ddtrma=stprma  ;
055200110613       ddtrsd=tasrsd  ;
055300110613       ddtlod=taslod  ;
055400110613       ddtcad=tascad  ;
055500110613       ddtprd=tasprd  ;
055600110613       ddtnzd=tasnzd  ;
055700110613       ddtdcm=tasdcm  ;
055800110613       ddtcca=tascca  ;
055900120320       // Area da organigramma
056000120423       chain ddtlna    azorg01l  ;
056100120320       if %found(azorg01l)  ;
056200120320       ddtcar=orgcar  ;
056300120320       endif  ;
056400120320
056500110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
056600110616         ddtinv =%int(§ar5drdt)             ;
056700110616       else   ;
056800110616        clear ddtinv    ;
056900110616 4     endif           ;
057000120402
057100120402       // Motivazione DDt errato
057200120402       if §ar5mtvdt<>*blanks   ;
057300120402       clear  tbeke1  ;
057400120402       clear  tbeke2  ;
057500120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
057600120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
057700120402
057800120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
057900120402        if %found(tntbe01l)  ;
058000120402         ddtmtvdt=tbeuni  ;
058100120402        endif   ;
058200120402
058300120402 4       if §ar5dimtv>*zeros     ;
058400120402           ddtdimtv =%int(§ar5dimtv)             ;
058500120402         else   ;
058600120402          clear ddtdimtv  ;
058700120402 4       endif           ;
058800120402 4     endif           ;
058900120402
059000110613       write wfddt000   ;
059100120424
059200120424       contarec=contarec +1 ;
059300110616
059400110616       *in01 =*on  ;
059500110613       endif  ;
059600110613
059700080829       // Aggiorno fiar5 se stampa
059800120406
059900120720 4     //if cominv>0 and listagen<>'S'  ;
060000120720 4     if  ComInvRd = *zero  and  ComInvRa > *zero;
060100080829       *in81=*off     ;
060200080829       *in82=*off     ;
060300080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
060400080902 5       if %found(fiar531c)    ;
060500080829          dar5gen=ar5uni      ;
060600080829          §ar5fstd='S'        ;
060700080829          ar5uni=dar5gen       ;
060800080902 6          if *in81      ;
060900080829            update fiar5000    ;
061000080829            else    ;
061100080829            update fiar5p00    ;
061200080902 6          endif   ;
061300080902 5       endif    ;
061400080902 4     endif    ;
061500080901
061600080902 3     endif    ;
061700080902 2     endif    ;
061800100603 1b    endif    ;
061900100603 1a    endif    ;
062000120406 1c    endif    ;
062100120406 1d    endif    ;
062200080902 1     endif    ;
062300080327
062400080325       ENDSR                  ;
062500080327       //-------------------------------------------------------------------
062600080327       BEGSR  *INZSR                 ;
062700080328
062800080328       in(E) *dtaara   ;
062900080328       if    %error or rsut  =*blanks  ;
063000080328       callp TIBS34R (TIBS34DS)        ;
063100080328       in    *dtaara   ;
063200080328       endif                           ;
063300080829
063400080829       // Apertura file di sede
063500080829             open(e) titas30c                ;
063600080829           if      not %open(titas30c)  ;
063700110613             %subst(WTA430:7:4)='GRU '    ;
063800110613             %subst(WTAs30:7:4)='GRU '    ;
063900110613             %subst(WTA531:7:4)='GRU '    ;
064000110617             %subst(WWFDDT:7:4)='AZM '    ;
064100110613             %subst(Wlib:7:4)='GRU '    ;
064200080829             open titas30c                ;
064300110617           endif  ;
064400080829             open tita430c                ;
064500080829             open fiar531c                ;
064600110617             open wfddt01l                ;
064700080829
064800080829       fnlsv2ds=kpjbu     ;
064900120405       clear comsped  ;
065000120405       clear comspea  ;
065100120405       clear stasped  ;
065200120405       clear staspea  ;
065300120411
065400080829
065500120405 1     if ListaGEN='S'     ;
065600120405       stptes   =tes2     ;
065700120405       stpdecinv=dec3     ;
065800120405       stpdec3  =dec4     ;
065900120411       stpdec4  =dec5     ;
066000120405 x1    else  ;
066100120405
066200080909       stptes   =tes1     ;
066300120411
066400120720       if cominvra=0      ;
066500120720       cominvra=cominvrd  ;
066600120720       stainvra=stainvrd  ;
066700120720       endif              ;
066800080829
066900120720 2     //if cominv>0        ;
067000120720 2     if  ComInvRd = *zero  and  ComInvRa > *zero;
067100120720       stpdecinv=dec1     ;
067200120720       %subst(stptes2:11:26)  =Inv      ;
067300120720       //stpdata1=stainv    ;
067400120720       StpData1 = StaInvRa;
067500120720       clear    stpdata2  ;
067600120720       clear    stpdecinv2;
067700120720       STricezDDT='SI'      ;
067800120720 2     endif              ;
067900080829
068000120411
068100120411 2     if cominvra>0      ;
068200080909       %subst(stptes2:11:26)  =RiInv      ;
068300080901       stpdecinv=dec2     ;
068400080901       stpdecinv2= 'AL'   ;
068500080829       stpdata1=stainvrd  ;
068600080829       stpdata2=stainvra  ;
068700120411       STricezDDT='SI'      ;
068800120411 2     endif              ;
068900120411 1     endif              ;
069000120411
069100120411 1     if ListaGEN='S'   ;
069200120411 2     if cominvrd>0  or cominvra>0 ;
069300120411
069400120411 3      if cominvrd>0  and cominvra=0 ;
069500120411        cominvra=cominvrd  ;
069600120411 3      Endif            ;
069700120411 3      if cominvrd=0               ;
069800120411        cominvrd=19900101  ;
069900120411 3      Endif            ;
070000120411
070100120411 3     select     ;
070200120411       when stainvrd=0        ;
070300120411         clear    stpdata6  ;
070400120411         clear    stpdec44  ;
070500120411         %subst(stpdec4:21:4)='AL :'     ;
070600120411         stpdata5=stainvra  ;
070700120411
070800120411       when stainvra=0        ;
070900120411         clear    stpdata6  ;
071000120411         clear    stpdec44  ;
071100120411         %subst(stpdec4:21:4)='IL :'     ;
071200120411         stpdata5=stainvrd  ;
071300120411
071400120411       other                ;
071500120411         stpdata5=stainvrd  ;
071600120411         stpdata6=stainvra  ;
071700120411         stpdec44  = 'AL'   ;
071800120411 3      endSL              ;
071900120411 2     endif              ;
072000120411 1     endif              ;
072100080829
072200120411       // data CONSEGNA
072300120405 1     if comdsd>0  or comdsa>0   ;
072400120411
072500120405 2     if ListaGEN<>'S'     ;
072600120405         STricezDDT='NO'      ;
072700120405         stptes2  =NOInv    ;
072800120405         stpdecinv=dec3     ;
072900120405       endif  ;
073000080901
073100080901        if comdsd>0  and comdsa=0 ;
073200080901        comdsa=comdsd    ;
073300080901        Endif            ;
073400080901        if comdsd=0               ;
073500080901        comdsd=19900101  ;
073600080901        Endif            ;
073700080901
073800080901       select     ;
073900080901       when stadsd=0        ;
074000080901         clear    stpdata2  ;
074100080901         clear    stpdecinv2;
074200080901         %subst(stpdecinv:21:4)='AL :'     ;
074300080901         stpdata1=stadsa    ;
074400080901
074500080901       when stadsa=0        ;
074600080901         clear    stpdata2  ;
074700080901         clear    stpdecinv2;
074800080901         %subst(stpdecinv:21:4)='IL :'     ;
074900080901         stpdata1=stadsd    ;
075000080901
075100080901       other                ;
075200080901         stpdata1=stadsd    ;
075300080901         stpdata2=stadsa    ;
075400080901         stpdecinv2= 'AL'   ;
075500080901        endSL              ;
075600080901       endif              ;
075700120405
075800120405       // Data spedizione DAL/AL
075900120405
076000120405       monitor   ;
076100120405       if alfsped>*zeros  ;
076200120405       comsped=%int(alfsped) ;
076300120405         dataiso=%date(comsped)  ;
076400120405         dataeur=dataiso;
076500120406         stasped=%dec(dataeur)  ;
076600120405       endif  ;
076700120405
076800120405       on-error  ;
076900120405       endmon  ;
077000120405       monitor   ;
077100120405       if alfspea>*zeros  ;
077200120405         comspea=%int(alfspea) ;
077300120405         dataiso=%date(comspea)  ;
077400120405         dataeur=dataiso;
077500120406         staspea=%dec(dataeur)  ;
077600120405       endif  ;
077700120405
077800120405       on-error  ;
077900120405       endmon  ;
078000120405
078100120405 1     if comsped>*zeros  or comspea>0   ;
078200120405        if comsped>0  and comspea=0 ;
078300120405        comspea=comsped   ;
078400120405        Endif            ;
078500120405        if comsped=0               ;
078600120406        comsped=19900101  ;
078700120405        Endif            ;
078800120405
078900120405       // Date in ggmmaaaa
079000120405       select     ;
079100120405       when stasped=0        ;
079200120405         clear    stpdata4  ;
079300120405         clear    stpdec33  ;
079400120405         %subst(stpdec3:21:4)='AL :'     ;
079500120405         stpdata3=staspea   ;
079600120405
079700120405       when staspea=0        ;
079800120405         clear    stpdata4  ;
079900120405         clear    stpdec33  ;
080000120405         %subst(stpdec3:21:4)='IL :'     ;
080100120405         stpdata3=stasped   ;
080200120405
080300120405       other                ;
080400120406         stpdata3=stasped   ;
080500120406         stpdata4=staspea   ;
080600120405         stpdec33  = 'AL'   ;
080700120405        endSL              ;
080800120405       endif              ;
080900080902
081000080902       // Linea di partenza
081100080902       if stalnp=0  ;
081200080902       stpdlnp='TUTTE' ;
081300080902       else           ;
081400080903       stpdlnp='-'+ stadlnp ;
081500080902       endif          ;
081600080902
081700080829       righe=70   ;
081800110613       // imposto data e ora del giorno
081900110613       dataiso=datasys  ;
082000110613       dateu=%dec(dataiso)   ;
082100110616
082200110616       dataiso=dataiso-%days(30) ;
082300110616       unmese=%dec(dataiso)   ;
082400110616
082500110613       ora_iso=ora_sys  ;
082600110613       ora_u=%dec(ora_iso)   ;
082700080829
082800110616       // Se richiesto file pulisco il lancio precedente per
082900110616       //  fil e ccm
083000110616       setll (dutpou: staccm)  wfddt01l   ;
083100110616       reade (dutpou: staccm)  wfddt01l   ;
083200110616
083300110616       if not %eof(wfddt01l)  and
083400110616          (stasfile='S'  or ddtedt<unmese) ;
083500110616
083600110616       dow  not %eof(wfddt01l)   ;
083700110616       delete wfddt000  ;
083800110616
083900110616       reade (dutpou: staccm)  wfddt01l   ;
084000110616       enddo  ;
084100110616        else  ;
084200110616        unlock   wfddt01l  ;
084300110616        endif  ;
084400110616
084500120405       // Ragione sociale Mittente
084600110617        if stasfile='S'   and staccm>0 ;
084700110616        chain  (1:dutkci:staccm) cnaco00f  ;
084800110616         if not %found(cnaco00f)  ;
084900110616          acorag=*all'?'  ;
085000110616         endif ;
085100110616       endif ;
085200120405
085300120405       // Solo file excel--> stmapo testata richieste
085400120405
085500120405       if listaGEN='S'    ;
085600120405       // Bolle inventariate
085700120405       if v1cinve='S'   ;
085800120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
085900120405       endif  ;
086000120405       if v1cinve='E'   ;
086100120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
086200120405       endif  ;
086300120405       if v1cerra='S'   ;
086400120521        stperra='SOLO BOLLE CON MOTIVO DOC.ERRATO'  ;
086500120405       endif  ;
086600120405       if v1cerra='E'   ;
086700120521        stperra='SOLO BOLLE SENZA MOTIVO DOC.ERRATO'  ;
086800120405       endif  ;
086900120405
087000120405       endif  ;
087100110616
087200080328       ENDSR                           ;
087300080317      /end-free
