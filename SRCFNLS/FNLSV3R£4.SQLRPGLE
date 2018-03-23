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
008000110617     D  pOutESITO      s              1
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
009300080828     d wrkgetlista     s           4096    varying
009400110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
009500110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
009600110613     d Ora_iso         s               t   timfmt(*iso)
009700110613     d Dataiso         s               d   datfmt(*iso)
009800080327     d Dataeur         s               d   datfmt(*eur)
009900080327     d Datadmy         s               d   datfmt(*dmy)
010000080829     d
010100110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
010200110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
010300110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
010400110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
010500080901     d dec1            c                   const('  Stampa Inventario  AL:')
010600080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
010700120406     d dec3            c                   const('   Data   consegna  DAL:')
010800120405     d dec4            c                   const('   Data spedizione  DAL:')
010900120411     d dec5            c                   const('   Data inventario  DAL:')
011000080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
011100080909     D                                     ESTITUIRE AL MITTENTE *')
011200120405     d tes2            c                   const('** EXCEL  DOCUMENTI  DA R-
011300120405     D                                     ESTITUIRE AL MITTENTE **')
011400080909     d Inv             c                   const('  Documenti inventariati  ')
011500080909     d RIinv           c                   const('Documenti già inventariati')
011600080909     d NOinv           c                   const(' Documenti non inventariat-
011700080909     D                                     i per bolle consegnate ')
011800080317      *------------------------------------------------------------------------*
011900080317      *   P R O T O T I P I
012000080317      *------------------------------------------------------------------------*
012100080829      /copy gaitrasrc/srcprotopr,tibs34r
012200110617      /copy gaitrasrc/srcprotopr,trtcm8r1
012300080829      *------------------------------------------------------------------------*
012400080829     Ifiar5000      81
012500080829     Ifiar5p00      82
012600001027     C*---------------------------------------------------------------*
012700001027     C     *ENTRY        PLIST
012800080325     C                   PARM                    kpjba
012900080317      /free
013000080828
013100120405       // File excel generico
013200120405
013300120405       select    ;
013400120405       when  listaGEN='S'  ;
013500120405
013600120405       exsr  SQLGEN  ;
013700120405
013800080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
013900080829
014000120405 1     when STricezDDT='SI'        ;
014100080828
014200080828       // Prepara SQL
014300080901          clear wsta          ;
014400080902          EXSR SQLINV         ;
014500080902
014600080902       // Stampa le non inventariate
014700080902
014800120405 1     when STricezDDT='NO'        ;
014900080902          clear wsta          ;
015000080902          clear totale        ;
015100080902          EXSR SQLDSP      ;
015200120405 1     endsl               ;
015300080901
015400080901       //  Esegui SQL  e stampa
015500080901          EXSR SQLesegui   ;
015600080901
015700080901 2     if   wsta='S'          ;
015800120124 3     //**if  righe>59           ;
015900120124 3     if  righe>57           ;
016000080901       write  LsV3PT1         ;
016100080901 3     endif                  ;
016200080901
016300120406       if *in01   ;
016400120302       staEM =%trim(staemail)+'@brt.it' ;
016500120406       endif  ;
016600120406
016700080901       write  LsV3Pfine       ;
016800120406 2     endif               ;
016900110617
017000120406        if listaGEN='S'  ;
017100120406        if *in01  ;
017200120406        staEM =%trim(staemail)+'@brt.it' ;
017300120406        else  ;
017400120406        staEM='NESSUNA bolla elaborata:file excel non creato';
017500120406        endif  ;
017600120406
017700120406        write  LsV3PT2         ;
017800120406        endif  ;
017900120406
018000110617        // Se scritto almeno un record, invio all'indirizzo e-mail
018100110617        if *in01  ;
018200110617          pInFILE='WFDDT00F'   ;
018300110617          pInIDJOB=knmeb       ;
018400120302          pInEMLDEST=%trim(staemail)+'@brt.it' ;
018500111207  2       if  staccm>0   ;
018600111207          chain (1:dutkci:staccm) cnaco00f  ;
018700111207  3       if not %found(cnaco00f)  ;
018800111207            acorag=*all'?'  ;
018900111207  3       endif  ;
019000111207
019100111207            pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
019200111207                       %subst(acorag:1:15)  ;
019300111207          endif ;
019400111207
019500110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
019600111207                          pInEMLVAR:pOutESITO) ;
019700110617        endif  ;
019800080901
019900080829
020000080317        *inlr ='1'   ;
020100080327       //-------------------------------------------------------------------
020200080901       BEGSR  SQLDSP                 ;
020300080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
020400080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
020500080828
020600080901
020700080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
020800080829                        and substr(ar5uni, 115, 1)='' ''';
020900080902
021000080902        if stalnp>0    ;
021100080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
021200080902        endif          ;
021300080902
021400080829        ENDSR     ;
021500080828
021600080901       //-------------------------------------------------------------------
021700080901       BEGSR  SQLINV                 ;
021800080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
021900080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
022000080901
022100080901        // STampa
022200080901        if cominv>0     ;
022300080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
022400080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
022500080901                        and substr(ar5uni, 115, 1)='' ''';
022600080901        endif     ;
022700080901
022800080901        // RIstampa
022900080901
023000080901        if cominvra>0  ;
023100080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
023200080901                    substr(ar5uni, 107, 8) <= ''' +
023300080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
023400080901                    %char(cominvrd) + '''' ;
023500080901        endif     ;
023600080902
023700080902        if stalnp>0    ;
023800080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
023900080902        endif          ;
024000080902
024100080901        ENDSR     ;
024200120405       //-------------------------------------------------------------------
024300120405       BEGSR  SQLGEN                 ;
024400120405        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
024500120405                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
024600120405
024700120405        // Solo NON inventariate
024800120405        select   ;
024900120405        when  v1cinve='E'  ;
025000120405        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999''' ;
025100120405
025200120411        // Solo     inventariate  senza data
025300120411        when  v1cinve='S'  and cominvra=0  ;
025400120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <   ''99999999'' +
025500120405                                   and substr(ar5uni, 107, 8) > ''00000000''';
025600120411        // Solo     inventariate  con   data
025700120411        when   cominvra>0  ;
025800120411        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <=''' +
025900120411                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
026000120411                    %char(cominvrd) + '''' ;
026100120405        other  ;
026200120405        // tutte
026300120405        wrkgetlista=wrkgetlista + ' substr(ar5uni, 107, 8) <>  ''        ''' ;
026400120405        endsl  ;
026500120405
026600120405        // solo con   MOTIVO DDT ERRATO
026700120405        if  v1cerra='S'  ;
026800120405        wrkgetlista=wrkgetlista + ' and +
026900120405                                    substr(ar5uni, 167, 8) <>  ''        ''';
027000120405        endif  ;
027100120405        // solo senza MOTIVO DDT ERRATO
027200120405        if  v1cerra='E'  ;
027300120405        wrkgetlista=wrkgetlista + ' and +
027400120405                                    substr(ar5uni, 167, 8) =   ''        ''';
027500120405        endif  ;
027600120405
027700120405        if stalnp>0    ;
027800120405        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
027900120405        endif          ;
028000120405
028100120405        ENDSR     ;
028200120405
028300080829       //-------------------------------------------------------------------
028400080829       BEGSR  SQLesegui              ;
028500080828       EXEC sql  prepare s1 from : wrkgetlista ;
028600080828       EXEC sql  declare a1 cursor for s1      ;
028700080828       EXEC sql  open    a1                    ;
028800080828
028900080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
029000080828                                        :ar5nsp, :ar5uni           ;
029100080828
029200080828       dow sqlcod=0    ;
029300080828
029400080829       exsr STAMPA     ;
029500080829
029600080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
029700080828                                        :ar5nsp, :ar5uni           ;
029800080828       enddo           ;
029900080828
030000080829       EXEC sql  CLOSE   a1                    ;
030100080829
030200080828       ENDSR     ;
030300080327       //-------------------------------------------------------------------
030400080325       BEGSR  Stampa          ;
030500080325
030600080325       // Cerco la bolla
030700080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
030800080901
030900120406 1     if %found(titas30c) and (STricezDDT='SI' or ListaGEN='S' or
031000110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
031100120406
031200120406       // Controllo la data spedizione se richiesta
031300120406 1c    if listaGEN <>'S' or comsped=0 or
031400120406             (tasdsp>=comsped and tasdsp<=comspea) ;
031500120406
031600120406       // Controllo la data consegna  se richiesta
031700120406 1d    if listaGEN <>'S' or comdsd=0 or
031800120406             (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0);
031900120406
032000100603
032100100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
032200100603       clear ds7R ;
032300100603 2     if tasgma<>*blanks  ;
032400100603
032500100603         clear tblkey    ;
032600100603         tblkey=tasgma    ;
032700100603         chain (1:'7R':tblkey) tabel00f   ;
032800100603          if %found(tabel00f)   ;
032900100603          ds7r=tbluni       ;
033000100603          endif             ;
033100100603 2        endif             ;
033200100603
033300100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
033400080901
033500100528       // Escludo il cliente mittente se selezionato
033600100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
033700100528
033800080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
033900080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
034000080902 2     if not %found(fnlbl01l) ;
034100080902
034200080829       stplnp=taslnp          ;
034300080829       stpnrs=tasnrs          ;
034400080327         if stpnrs>0            ;
034500080327         tra01='-'              ;
034600080327         else                   ;
034700080327         tra01=' '              ;
034800080327         endif                  ;
034900080829       stpnsp=tasnsp          ;
035000080829       dataiso=%date(tasdsp)    ;
035100080327       datadmy=dataiso        ;
035200080327       stpDSP=%dec(datadmy)   ;
035300080829       stplna=taslna          ;
035400080829       stpRMN=tasRMN          ;
035500080902
035600080902       // Data consegna        ;
035700080902       clear stpdcm  ;
035800080902       clear stpcca    ;
035900120406       clear ds7o      ;
036000120423       CLEAR LBLLAN  ;
036100080902
036200080902 3     if tasdcm>0   ;
036300080902       DATAEUR=%DATE(tasdcm:*iso)       ;
036400080902       stpdcm=%dec(dataeur)   ;
036500080902 4     if tascca<>' '  ;
036600080902         clear tblkey    ;
036700080902         tblkey=tascca    ;
036800080902         stpcca=tascca    ;
036900080902
037000080902         chain (1:'7O':tblkey) tabel00f   ;
037100080902          if %found(tabel00f)   ;
037200080902          ds7o=tbluni       ;
037300080902           %subst(stpcca:2:10)='-'+tbluni   ;
037400080902          endif       ;
037500080902 4     endif       ;
037600120406
037700120406       // Se la consegna anomala non è reso, ma dirottamento,
037800120406       //  verifico se c'e' un'altra figlia resa
037900120406 4     if §7ofdi='D'  ;
038000120406       lblaan=ar5aas ;
038100120406       lbllpn=ar5lnp ;
038200120406       lblnrn=ar5nrs ;
038300120406       lblnsn=ar5nsp ;
038400120406
038500120406 5     dou not %found(fnlbl02l) or  §7ores= 'S'  ;
038600120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  fnlbl02l  ;
038700120406 6     if  %found(fnlbl02l)  ;
038800120406       chain (lblaan:lbllpn:lblnrn:lblnsn)  titas30c  ;
038900120406         clear ds7o      ;
039000120406 7     if tascca<>' '  ;
039100120406         clear tblkey    ;
039200120406         tblkey=tascca    ;
039300120406         chain (1:'7O':tblkey) tabel00f   ;
039400120406          if %found(tabel00f)   ;
039500120406           ds7o=tbluni       ;
039600120406          endif  ;
039700120406 7     endif  ;
039800120406 6     endif  ;
039900120406
040000120406 5     enddo ;
040100120406
040200120406       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
040300120406 4     endif       ;
040400120406 3     endif       ;
040500080829
040600080902       // Escludo dalla stampa delle non inventariate le bolle di reso
040700080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
040800080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
040900080902 4     IF %FOUND(TITA430C)    ;
041000080829       STPRMA=ta4NOT          ;
041100080829       ELSE                   ;
041200080829       clear stprma           ;
041300080902 4     endif                  ;
041400080829
041500080901       stprsd=tasrsd          ;
041600080829       stplod=taslod          ;
041700080829       %subst(stplod:13:3)=' '+tasprd    ;
041800080901
041900080901       // Data inventario      ;
042000080829       dar5gen=ar5uni     ;
042100080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
042200080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
042300080901         stpinv=%dec(dataeur)   ;
042400080829       else   ;
042500080901        clear stpinv    ;
042600080902 4     endif           ;
042700080327
042800080325
042900120406       // Per lista generica non stampo il dettaglio
043000120406       if ListaGEN<>'S'  ;
043100120406
043200080902       // totale bolle stampate
043300080902       totale=totale+1        ;
043400080902
043500080327       // Testata bolla
043600080327
043700110617 4     if  righe>63           ;
043800080829       write  LsV3PT1         ;
043900080328       righe=8                ;
044000080902 4     endif                  ;
044100080331
044200080829       write   LsV3PD1        ;
044300080327
044400080327       righe=righe+1          ;
044500080829       wsta='S'               ;
044600120406       endif  ;
044700080829
044800110613       // Se richiesto --> creo file
044900110613       if   stasfile ='S'  ;
045000110613       clear wfddt000         ;
045100110616       ddtidute=knmeb   ;
045200110616       ddteut=knmus   ;
045300110613       ddtepo=dutpou  ;
045400110613       ddtedt=dateu   ;
045500110613       ddteor=ora_u   ;
045600110613       ddtccm=tasccm  ;
045700110616       // ragione sociale mittente
045800110616       if staccm=0  ;
045900110616       chain  (1:dutkci:tasccm) cnaco00f  ;
046000110616        if %found(cnaco00f)  ;
046100110616         ddtrsm=acorag   ;
046200110616        else  ;
046300110616        ddtrsm=*all'?' ;
046400110616        endif ;
046500110616       else  ;
046600110616        ddtrsm=acorag  ;
046700110616       endif  ;
046800110616
046900110613       ddtaas=tasaas  ;
047000110613       ddtlnp=taslnp  ;
047100110613       ddtnrs=tasnrs  ;
047200110613       ddtnsp=tasnsp  ;
047300110613       ddtmgs=tasmgs  ;
047400120423       IF LBLLAN>0    ;
047500120423        ddtlna=lbllan  ;
047600120423        ELSE   ;
047700120423        ddtlna=taslna  ;
047800120423        ENDIF  ;
047900110613       ddtrmn=tasrmn  ;
048000110613       ddtrma=stprma  ;
048100110613       ddtrsd=tasrsd  ;
048200110613       ddtlod=taslod  ;
048300110613       ddtcad=tascad  ;
048400110613       ddtprd=tasprd  ;
048500110613       ddtnzd=tasnzd  ;
048600110613       ddtdcm=tasdcm  ;
048700110613       ddtcca=tascca  ;
048800120320       // Area da organigramma
048900120423       chain ddtlna    azorg01l  ;
049000120320       if %found(azorg01l)  ;
049100120320       ddtcar=orgcar  ;
049200120320       endif  ;
049300120320
049400110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
049500110616         ddtinv =%int(§ar5drdt)             ;
049600110616       else   ;
049700110616        clear ddtinv    ;
049800110616 4     endif           ;
049900120402
050000120402       // Motivazione DDt errato
050100120402       if §ar5mtvdt<>*blanks   ;
050200120402       clear  tbeke1  ;
050300120402       clear  tbeke2  ;
050400120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
050500120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
050600120402
050700120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
050800120402        if %found(tntbe01l)  ;
050900120402         ddtmtvdt=tbeuni  ;
051000120402        endif   ;
051100120402
051200120402 4       if §ar5dimtv>*zeros     ;
051300120402           ddtdimtv =%int(§ar5dimtv)             ;
051400120402         else   ;
051500120402          clear ddtdimtv  ;
051600120402 4       endif           ;
051700120402 4     endif           ;
051800120402
051900110613       write wfddt000   ;
052000110616
052100110616       *in01 =*on  ;
052200110613       endif  ;
052300110613
052400080829       // Aggiorno fiar5 se stampa
052500120406
052600120406 4     if cominv>0 and listagen<>'S'  ;
052700080829       *in81=*off     ;
052800080829       *in82=*off     ;
052900080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
053000080902 5       if %found(fiar531c)    ;
053100080829          dar5gen=ar5uni      ;
053200080829          §ar5fstd='S'        ;
053300080829          ar5uni=dar5gen       ;
053400080902 6          if *in81      ;
053500080829            update fiar5000    ;
053600080829            else    ;
053700080829            update fiar5p00    ;
053800080902 6          endif   ;
053900080902 5       endif    ;
054000080902 4     endif    ;
054100080901
054200080902 3     endif    ;
054300080902 2     endif    ;
054400100603 1b    endif    ;
054500100603 1a    endif    ;
054600120406 1c    endif    ;
054700120406 1d    endif    ;
054800080902 1     endif    ;
054900080327
055000080325       ENDSR                  ;
055100080327       //-------------------------------------------------------------------
055200080327       BEGSR  *INZSR                 ;
055300080328
055400080328       in(E) *dtaara   ;
055500080328       if    %error or rsut  =*blanks  ;
055600080328       callp TIBS34R (TIBS34DS)        ;
055700080328       in    *dtaara   ;
055800080328       endif                           ;
055900080829
056000080829       // Apertura file di sede
056100080829             open(e) titas30c                ;
056200080829           if      not %open(titas30c)  ;
056300110613             %subst(WTA430:7:4)='GRU '    ;
056400110613             %subst(WTAs30:7:4)='GRU '    ;
056500110613             %subst(WTA531:7:4)='GRU '    ;
056600110617             %subst(WWFDDT:7:4)='AZM '    ;
056700110613             %subst(Wlib:7:4)='GRU '    ;
056800080829             open titas30c                ;
056900110617           endif  ;
057000080829             open tita430c                ;
057100080829             open fiar531c                ;
057200110617             open wfddt01l                ;
057300080829
057400080829       fnlsv2ds=kpjbu     ;
057500120405       clear comsped  ;
057600120405       clear comspea  ;
057700120405       clear stasped  ;
057800120405       clear staspea  ;
057900120411
058000080829
058100120405 1     if ListaGEN='S'     ;
058200120405       stptes   =tes2     ;
058300120405       stpdecinv=dec3     ;
058400120405       stpdec3  =dec4     ;
058500120411       stpdec4  =dec5     ;
058600120405 x1    else  ;
058700120405
058800080909       stptes   =tes1     ;
058900120411
059000120411       if cominvra=0      ;
059100120411       cominvra=cominvrd  ;
059200120411       stainvra=stainvrd  ;
059300120411       endif              ;
059400080829
059500120411 2     if cominv>0        ;
059600080901       stpdecinv=dec1     ;
059700080909       %subst(stptes2:11:26)  =Inv      ;
059800080829       stpdata1=stainv    ;
059900080829       clear    stpdata2  ;
060000080829       clear    stpdecinv2;
060100080902       STricezDDT='SI'      ;
060200120411 2     endif              ;
060300080829
060400120411
060500120411 2     if cominvra>0      ;
060600080909       %subst(stptes2:11:26)  =RiInv      ;
060700080901       stpdecinv=dec2     ;
060800080901       stpdecinv2= 'AL'   ;
060900080829       stpdata1=stainvrd  ;
061000080829       stpdata2=stainvra  ;
061100120411       STricezDDT='SI'      ;
061200120411 2     endif              ;
061300120411 1     endif              ;
061400120411
061500120411 1     if ListaGEN='S'   ;
061600120411 2     if cominvrd>0  or cominvra>0 ;
061700120411
061800120411 3      if cominvrd>0  and cominvra=0 ;
061900120411        cominvra=cominvrd  ;
062000120411 3      Endif            ;
062100120411 3      if cominvrd=0               ;
062200120411        cominvrd=19900101  ;
062300120411 3      Endif            ;
062400120411
062500120411 3     select     ;
062600120411       when stainvrd=0        ;
062700120411         clear    stpdata6  ;
062800120411         clear    stpdec44  ;
062900120411         %subst(stpdec4:21:4)='AL :'     ;
063000120411         stpdata5=stainvra  ;
063100120411
063200120411       when stainvra=0        ;
063300120411         clear    stpdata6  ;
063400120411         clear    stpdec44  ;
063500120411         %subst(stpdec4:21:4)='IL :'     ;
063600120411         stpdata5=stainvrd  ;
063700120411
063800120411       other                ;
063900120411         stpdata5=stainvrd  ;
064000120411         stpdata6=stainvra  ;
064100120411         stpdec44  = 'AL'   ;
064200120411 3      endSL              ;
064300120411 2     endif              ;
064400120411 1     endif              ;
064500080829
064600120411       // data CONSEGNA
064700120405 1     if comdsd>0  or comdsa>0   ;
064800120411
064900120405 2     if ListaGEN<>'S'     ;
065000120405         STricezDDT='NO'      ;
065100120405         stptes2  =NOInv    ;
065200120405         stpdecinv=dec3     ;
065300120405       endif  ;
065400080901
065500080901        if comdsd>0  and comdsa=0 ;
065600080901        comdsa=comdsd    ;
065700080901        Endif            ;
065800080901        if comdsd=0               ;
065900080901        comdsd=19900101  ;
066000080901        Endif            ;
066100080901
066200080901       select     ;
066300080901       when stadsd=0        ;
066400080901         clear    stpdata2  ;
066500080901         clear    stpdecinv2;
066600080901         %subst(stpdecinv:21:4)='AL :'     ;
066700080901         stpdata1=stadsa    ;
066800080901
066900080901       when stadsa=0        ;
067000080901         clear    stpdata2  ;
067100080901         clear    stpdecinv2;
067200080901         %subst(stpdecinv:21:4)='IL :'     ;
067300080901         stpdata1=stadsd    ;
067400080901
067500080901       other                ;
067600080901         stpdata1=stadsd    ;
067700080901         stpdata2=stadsa    ;
067800080901         stpdecinv2= 'AL'   ;
067900080901        endSL              ;
068000080901       endif              ;
068100120405
068200120405       // Data spedizione DAL/AL
068300120405
068400120405       monitor   ;
068500120405       if alfsped>*zeros  ;
068600120405       comsped=%int(alfsped) ;
068700120405         dataiso=%date(comsped)  ;
068800120405         dataeur=dataiso;
068900120406         stasped=%dec(dataeur)  ;
069000120405       endif  ;
069100120405
069200120405       on-error  ;
069300120405       endmon  ;
069400120405       monitor   ;
069500120405       if alfspea>*zeros  ;
069600120405         comspea=%int(alfspea) ;
069700120405         dataiso=%date(comspea)  ;
069800120405         dataeur=dataiso;
069900120406         staspea=%dec(dataeur)  ;
070000120405       endif  ;
070100120405
070200120405       on-error  ;
070300120405       endmon  ;
070400120405
070500120405 1     if comsped>*zeros  or comspea>0   ;
070600120405        if comsped>0  and comspea=0 ;
070700120405        comspea=comsped   ;
070800120405        Endif            ;
070900120405        if comsped=0               ;
071000120406        comsped=19900101  ;
071100120405        Endif            ;
071200120405
071300120405       // Date in ggmmaaaa
071400120405       select     ;
071500120405       when stasped=0        ;
071600120405         clear    stpdata4  ;
071700120405         clear    stpdec33  ;
071800120405         %subst(stpdec3:21:4)='AL :'     ;
071900120405         stpdata3=staspea   ;
072000120405
072100120405       when staspea=0        ;
072200120405         clear    stpdata4  ;
072300120405         clear    stpdec33  ;
072400120405         %subst(stpdec3:21:4)='IL :'     ;
072500120405         stpdata3=stasped   ;
072600120405
072700120405       other                ;
072800120406         stpdata3=stasped   ;
072900120406         stpdata4=staspea   ;
073000120405         stpdec33  = 'AL'   ;
073100120405        endSL              ;
073200120405       endif              ;
073300080902
073400080902       // Linea di partenza
073500080902       if stalnp=0  ;
073600080902       stpdlnp='TUTTE' ;
073700080902       else           ;
073800080903       stpdlnp='-'+ stadlnp ;
073900080902       endif          ;
074000080902
074100080829       righe=70   ;
074200110613       // imposto data e ora del giorno
074300110613       dataiso=datasys  ;
074400110613       dateu=%dec(dataiso)   ;
074500110616
074600110616       dataiso=dataiso-%days(30) ;
074700110616       unmese=%dec(dataiso)   ;
074800110616
074900110613       ora_iso=ora_sys  ;
075000110613       ora_u=%dec(ora_iso)   ;
075100080829
075200110616       // Se richiesto file pulisco il lancio precedente per
075300110616       //  fil e ccm
075400110616       setll (dutpou: staccm)  wfddt01l   ;
075500110616       reade (dutpou: staccm)  wfddt01l   ;
075600110616
075700110616       if not %eof(wfddt01l)  and
075800110616          (stasfile='S'  or ddtedt<unmese) ;
075900110616
076000110616       dow  not %eof(wfddt01l)   ;
076100110616       delete wfddt000  ;
076200110616
076300110616       reade (dutpou: staccm)  wfddt01l   ;
076400110616       enddo  ;
076500110616        else  ;
076600110616        unlock   wfddt01l  ;
076700110616        endif  ;
076800110616
076900120405       // Ragione sociale Mittente
077000110617        if stasfile='S'   and staccm>0 ;
077100110616        chain  (1:dutkci:staccm) cnaco00f  ;
077200110616         if not %found(cnaco00f)  ;
077300110616          acorag=*all'?'  ;
077400110616         endif ;
077500110616       endif ;
077600120405
077700120405       // Solo file excel--> stmapo testata richieste
077800120405
077900120405       if listaGEN='S'    ;
078000120405       // Bolle inventariate
078100120405       if v1cinve='S'   ;
078200120406        stpinve='SOLO BOLLE INVENTARIATE'   ;
078300120405       endif  ;
078400120405       if v1cinve='E'   ;
078500120406        stpinve='SOLO BOLLE NON INVENTARIATE'   ;
078600120405       endif  ;
078700120405       if v1cerra='S'   ;
078800120406        stperra='SOLO BOLLE CON MOTIVO DDT ERRATO'  ;
078900120405       endif  ;
079000120405       if v1cerra='E'   ;
079100120406        stperra='SOLO BOLLE SENZA MOTIVO DDT ERRATO'  ;
079200120405       endif  ;
079300120405
079400120405       endif  ;
079500110616
079600080328       ENDSR                           ;
079700080317      /end-free
