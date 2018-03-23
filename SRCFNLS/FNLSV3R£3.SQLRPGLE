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
006500080828     d
006600110617     D  pInFILE        s             10
006700110617     D  pInIDJOB       s             26
006800110617     D  pInPWDXLS      s             15
006900110617     D  pInNOMXLS      s             78
007000110617     D  pInEMLDEST     s            121
007100111207     D  pInEMLVAR      s             80
007200110617     D  pOutESITO      s              1
007300080829     d savspe          s                   like(tasspe)
007400080829     d righe           s              3  0
007500110613     d dateu           s              8  0
007600110616     d unmese          s              8  0
007700110613     d ora_u           s              6  0
007800080829     d Wsta            s              1
007900080902     d STricezDDT      s              2
008000110613     d wlib            s             10    inz('GAITRAGRPS')
008100080828     d wrkgetlista     s           4096    varying
008200110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
008300110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
008400110613     d Ora_iso         s               t   timfmt(*iso)
008500110613     d Dataiso         s               d   datfmt(*iso)
008600080327     d Dataeur         s               d   datfmt(*eur)
008700080327     d Datadmy         s               d   datfmt(*dmy)
008800080829     d
008900110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
009000110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
009100110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
009200110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
009300080901     d dec1            c                   const('  Stampa Inventario  AL:')
009400080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
009500110616     d dec3            c                   const('    Data  consegna  DAL:')
009600080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
009700080909     D                                     ESTITUIRE AL MITTENTE *')
009800080909     d Inv             c                   const('  Documenti inventariati  ')
009900080909     d RIinv           c                   const('Documenti già inventariati')
010000080909     d NOinv           c                   const(' Documenti non inventariat-
010100080909     D                                     i per bolle consegnate ')
010200080317      *------------------------------------------------------------------------*
010300080317      *   P R O T O T I P I
010400080317      *------------------------------------------------------------------------*
010500080829      /copy gaitrasrc/srcprotopr,tibs34r
010600110617      /copy gaitrasrc/srcprotopr,trtcm8r1
010700080829      *------------------------------------------------------------------------*
010800080829     Ifiar5000      81
010900080829     Ifiar5p00      82
011000001027     C*---------------------------------------------------------------*
011100001027     C     *ENTRY        PLIST
011200080325     C                   PARM                    kpjba
011300080317      /free
011400080828
011500080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
011600080829
011700080902 1     if STricezDDT='SI'        ;
011800080828
011900080828       // Prepara SQL
012000080901          clear wsta          ;
012100080902          EXSR SQLINV         ;
012200080902       endif                  ;
012300080902
012400080902       // Stampa le non inventariate
012500080902
012600080902 1     if STricezDDT='NO'        ;
012700080902          clear wsta          ;
012800080902          clear totale        ;
012900080902          EXSR SQLDSP      ;
013000080902       endif               ;
013100080901
013200080901       //  Esegui SQL  e stampa
013300080901          EXSR SQLesegui   ;
013400080901
013500080901 2     if   wsta='S'          ;
013600120124 3     //**if  righe>59           ;
013700120124 3     if  righe>57           ;
013800080901       write  LsV3PT1         ;
013900080901 3     endif                  ;
014000080901
014100120302       staEM =%trim(staemail)+'@brt.it' ;
014200080901       write  LsV3Pfine       ;
014300080901 1     endif               ;
014400110617
014500110617        // Se scritto almeno un record, invio all'indirizzo e-mail
014600110617        if *in01  ;
014700110617          pInFILE='WFDDT00F'   ;
014800110617          pInIDJOB=knmeb       ;
014900120302          pInEMLDEST=%trim(staemail)+'@brt.it' ;
015000111207  2       if  staccm>0   ;
015100111207          chain (1:dutkci:staccm) cnaco00f  ;
015200111207  3       if not %found(cnaco00f)  ;
015300111207            acorag=*all'?'  ;
015400111207  3       endif  ;
015500111207
015600111207            pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
015700111207                       %subst(acorag:1:15)  ;
015800111207          endif ;
015900111207
016000110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
016100111207                          pInEMLVAR:pOutESITO) ;
016200110617        endif  ;
016300080901
016400080829
016500080317        *inlr ='1'   ;
016600080327       //-------------------------------------------------------------------
016700080901       BEGSR  SQLDSP                 ;
016800080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
016900080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
017000080828
017100080901
017200080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
017300080829                        and substr(ar5uni, 115, 1)='' ''';
017400080902
017500080902        if stalnp>0    ;
017600080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
017700080902        endif          ;
017800080902
017900080829        ENDSR     ;
018000080828
018100080901       //-------------------------------------------------------------------
018200080901       BEGSR  SQLINV                 ;
018300080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
018400080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
018500080901
018600080901        // STampa
018700080901        if cominv>0     ;
018800080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
018900080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
019000080901                        and substr(ar5uni, 115, 1)='' ''';
019100080901        endif     ;
019200080901
019300080901        // RIstampa
019400080901
019500080901        if cominvra>0  ;
019600080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
019700080901                    substr(ar5uni, 107, 8) <= ''' +
019800080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
019900080901                    %char(cominvrd) + '''' ;
020000080901        endif     ;
020100080902
020200080902        if stalnp>0    ;
020300080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
020400080902        endif          ;
020500080902
020600080901        ENDSR     ;
020700080829       //-------------------------------------------------------------------
020800080829       BEGSR  SQLesegui              ;
020900080828       EXEC sql  prepare s1 from : wrkgetlista ;
021000080828       EXEC sql  declare a1 cursor for s1      ;
021100080828       EXEC sql  open    a1                    ;
021200080828
021300080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
021400080828                                        :ar5nsp, :ar5uni           ;
021500080828
021600080828       dow sqlcod=0    ;
021700080828
021800080829       exsr STAMPA     ;
021900080829
022000080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
022100080828                                        :ar5nsp, :ar5uni           ;
022200080828       enddo           ;
022300080828
022400080829       EXEC sql  CLOSE   a1                    ;
022500080829
022600080828       ENDSR     ;
022700080327       //-------------------------------------------------------------------
022800080325       BEGSR  Stampa          ;
022900080325
023000080325       // Cerco la bolla
023100080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
023200080901
023300080902 1     if %found(titas30c) and (STricezDDT='SI' or
023400110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
023500110616       //                (tasdsp>=comdsd and tasdsp<=comdsa and tasdcm>0));
023600100603
023700100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
023800100603       clear ds7R ;
023900100603 2     if tasgma<>*blanks  ;
024000100603
024100100603         clear tblkey    ;
024200100603         tblkey=tasgma    ;
024300100603         chain (1:'7R':tblkey) tabel00f   ;
024400100603          if %found(tabel00f)   ;
024500100603          ds7r=tbluni       ;
024600100603          endif             ;
024700100603 2        endif             ;
024800100603
024900100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
025000080901
025100100528       // Escludo il cliente mittente se selezionato
025200100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
025300100528
025400080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
025500080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
025600080902 2     if not %found(fnlbl01l) ;
025700080902
025800080829       stplnp=taslnp          ;
025900080829       stpnrs=tasnrs          ;
026000080327         if stpnrs>0            ;
026100080327         tra01='-'              ;
026200080327         else                   ;
026300080327         tra01=' '              ;
026400080327         endif                  ;
026500080829       stpnsp=tasnsp          ;
026600080829       dataiso=%date(tasdsp)    ;
026700080327       datadmy=dataiso        ;
026800080327       stpDSP=%dec(datadmy)   ;
026900080829       stplna=taslna          ;
027000080829       stpRMN=tasRMN          ;
027100080902
027200080902       // Data consegna        ;
027300080902       clear stpdcm  ;
027400080902       clear stpcca    ;
027500080902
027600080902 3     if tasdcm>0   ;
027700080902       DATAEUR=%DATE(tasdcm:*iso)       ;
027800080902       stpdcm=%dec(dataeur)   ;
027900080902 4     if tascca<>' '  ;
028000080902         clear tblkey    ;
028100080902         tblkey=tascca    ;
028200080902         stpcca=tascca    ;
028300080902
028400080902         chain (1:'7O':tblkey) tabel00f   ;
028500080902          if %found(tabel00f)   ;
028600080902          ds7o=tbluni       ;
028700080902           %subst(stpcca:2:10)='-'+tbluni   ;
028800080902          endif       ;
028900080902 4     endif       ;
029000080902 3     endif       ;
029100080829
029200080902       // Escludo dalla stampa delle non inventariate le bolle di reso
029300080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
029400080902
029500080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
029600080902 4     IF %FOUND(TITA430C)    ;
029700080829       STPRMA=ta4NOT          ;
029800080829       ELSE                   ;
029900080829       clear stprma           ;
030000080902 4     endif                  ;
030100080829
030200080901       stprsd=tasrsd          ;
030300080829       stplod=taslod          ;
030400080829       %subst(stplod:13:3)=' '+tasprd    ;
030500080901
030600080901       // Data inventario      ;
030700080829       dar5gen=ar5uni     ;
030800080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
030900080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
031000080901         stpinv=%dec(dataeur)   ;
031100080829       else   ;
031200080901        clear stpinv    ;
031300080902 4     endif           ;
031400080327
031500080325
031600080902       // totale bolle stampate
031700080902       totale=totale+1        ;
031800080902
031900080327       // Testata bolla
032000080327
032100110617 4     if  righe>63           ;
032200080829       write  LsV3PT1         ;
032300080328       righe=8                ;
032400080902 4     endif                  ;
032500080331
032600080829       write   LsV3PD1        ;
032700080327
032800080327       righe=righe+1          ;
032900080829       wsta='S'               ;
033000080829
033100110613       // Se richiesto --> creo file
033200110613       if   stasfile ='S'  ;
033300110613       clear wfddt000         ;
033400110616       ddtidute=knmeb   ;
033500110616       ddteut=knmus   ;
033600110613       ddtepo=dutpou  ;
033700110613       ddtedt=dateu   ;
033800110613       ddteor=ora_u   ;
033900110613       ddtccm=tasccm  ;
034000110616       // ragione sociale mittente
034100110616       if staccm=0  ;
034200110616       chain  (1:dutkci:tasccm) cnaco00f  ;
034300110616        if %found(cnaco00f)  ;
034400110616         ddtrsm=acorag   ;
034500110616        else  ;
034600110616        ddtrsm=*all'?' ;
034700110616        endif ;
034800110616       else  ;
034900110616        ddtrsm=acorag  ;
035000110616       endif  ;
035100110616
035200110613       ddtaas=tasaas  ;
035300110613       ddtlnp=taslnp  ;
035400110613       ddtnrs=tasnrs  ;
035500110613       ddtnsp=tasnsp  ;
035600110613       ddtmgs=tasmgs  ;
035700110613       ddtlna=taslna  ;
035800110613       ddtrmn=tasrmn  ;
035900110613       ddtrma=stprma  ;
036000110613       ddtrsd=tasrsd  ;
036100110613       ddtlod=taslod  ;
036200110613       ddtcad=tascad  ;
036300110613       ddtprd=tasprd  ;
036400110613       ddtnzd=tasnzd  ;
036500110613       ddtdcm=tasdcm  ;
036600110613       ddtcca=tascca  ;
036700120320       // Area da organigramma
036800120320       chain taslna    azorg01l  ;
036900120320       if %found(azorg01l)  ;
037000120320       ddtcar=orgcar  ;
037100120320       endif  ;
037200120320
037300110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
037400110616         ddtinv =%int(§ar5drdt)             ;
037500110616       else   ;
037600110616        clear ddtinv    ;
037700110616 4     endif           ;
037800120402
037900120402       // Motivazione DDt errato
038000120402       if §ar5mtvdt<>*blanks   ;
038100120402       clear  tbeke1  ;
038200120402       clear  tbeke2  ;
038300120402       tbeke1=%subst(§ar5mtvdt:1:3)  ;
038400120402       tbeke2=%subst(§ar5mtvdt:4:5)  ;
038500120402
038600120402       chain ('MDE':tbeke1:tbeke2) tntbe01l   ;
038700120402        if %found(tntbe01l)  ;
038800120402         ddtmtvdt=tbeuni  ;
038900120402        endif   ;
039000120402
039100120402 4       if §ar5dimtv>*zeros     ;
039200120402           ddtdimtv =%int(§ar5dimtv)             ;
039300120402         else   ;
039400120402          clear ddtdimtv  ;
039500120402 4       endif           ;
039600120402 4     endif           ;
039700120402
039800110613       write wfddt000   ;
039900110616
040000110616       *in01 =*on  ;
040100110613       endif  ;
040200110613
040300080829       // Aggiorno fiar5 se stampa
040400080902 4     if cominv>0         ;
040500080829       *in81=*off     ;
040600080829       *in82=*off     ;
040700080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
040800080902 5       if %found(fiar531c)    ;
040900080829          dar5gen=ar5uni      ;
041000080829          §ar5fstd='S'        ;
041100080829          ar5uni=dar5gen       ;
041200080902 6          if *in81      ;
041300080829            update fiar5000    ;
041400080829            else    ;
041500080829            update fiar5p00    ;
041600080902 6          endif   ;
041700080902 5       endif    ;
041800080902 4     endif    ;
041900080901
042000080902 3     endif    ;
042100080902 2     endif    ;
042200100603 1b    endif    ;
042300100603 1a    endif    ;
042400080902 1     endif    ;
042500080327
042600080325       ENDSR                  ;
042700080327       //-------------------------------------------------------------------
042800080327       BEGSR  *INZSR                 ;
042900080328
043000080328       in(E) *dtaara   ;
043100080328       if    %error or rsut  =*blanks  ;
043200080328       callp TIBS34R (TIBS34DS)        ;
043300080328       in    *dtaara   ;
043400080328       endif                           ;
043500080829
043600080829       // Apertura file di sede
043700080829             open(e) titas30c                ;
043800080829           if      not %open(titas30c)  ;
043900110613             %subst(WTA430:7:4)='GRU '    ;
044000110613             %subst(WTAs30:7:4)='GRU '    ;
044100110613             %subst(WTA531:7:4)='GRU '    ;
044200110617             %subst(WWFDDT:7:4)='AZM '    ;
044300110613             %subst(Wlib:7:4)='GRU '    ;
044400080829             open titas30c                ;
044500110617           endif  ;
044600080829             open tita430c                ;
044700080829             open fiar531c                ;
044800110617             open wfddt01l                ;
044900080829
045000080829       fnlsv2ds=kpjbu     ;
045100080829
045200080909       stptes   =tes1     ;
045300080909
045400080829       if cominvra=0      ;
045500080829       cominvra=cominvrd  ;
045600080901       stainvra=stainvrd  ;
045700080829       endif              ;
045800080829
045900080829       if cominv>0        ;
046000080901       stpdecinv=dec1     ;
046100080909       %subst(stptes2:11:26)  =Inv      ;
046200080829       stpdata1=stainv    ;
046300080829       clear    stpdata2  ;
046400080829       clear    stpdecinv2;
046500080902       STricezDDT='SI'      ;
046600080829       endif              ;
046700080829
046800080829       if cominvra>0      ;
046900080909       %subst(stptes2:11:26)  =RiInv      ;
047000080901       stpdecinv=dec2     ;
047100080901       stpdecinv2= 'AL'   ;
047200080829       stpdata1=stainvrd  ;
047300080829       stpdata2=stainvra  ;
047400080902       STricezDDT='SI'      ;
047500080829       endif              ;
047600080829
047700080901       if comdsd>0  or comdsa>0   ;
047800080902       STricezDDT='NO'      ;
047900080901
048000080901        if comdsd>0  and comdsa=0 ;
048100080901        comdsa=comdsd    ;
048200080901        Endif            ;
048300080901        if comdsd=0               ;
048400080901        comdsd=19900101  ;
048500080901        Endif            ;
048600080901
048700080909       stptes2  =NOInv    ;
048800080901       stpdecinv=dec3     ;
048900080901
049000080901       select     ;
049100080901       when stadsd=0        ;
049200080901         clear    stpdata2  ;
049300080901         clear    stpdecinv2;
049400080901         %subst(stpdecinv:21:4)='AL :'     ;
049500080901         stpdata1=stadsa    ;
049600080901
049700080901       when stadsa=0        ;
049800080901         clear    stpdata2  ;
049900080901         clear    stpdecinv2;
050000080901         %subst(stpdecinv:21:4)='IL :'     ;
050100080901         stpdata1=stadsd    ;
050200080901
050300080901       other                ;
050400080901         stpdata1=stadsd    ;
050500080901         stpdata2=stadsa    ;
050600080901         stpdecinv2= 'AL'   ;
050700080901        endSL              ;
050800080901       endif              ;
050900080902
051000080902       // Linea di partenza
051100080902       if stalnp=0  ;
051200080902       stpdlnp='TUTTE' ;
051300080902       else           ;
051400080903       stpdlnp='-'+ stadlnp ;
051500080902       endif          ;
051600080902
051700080829       righe=70   ;
051800110613       // imposto data e ora del giorno
051900110613       dataiso=datasys  ;
052000110613       dateu=%dec(dataiso)   ;
052100110616
052200110616       dataiso=dataiso-%days(30) ;
052300110616       unmese=%dec(dataiso)   ;
052400110616
052500110613       ora_iso=ora_sys  ;
052600110613       ora_u=%dec(ora_iso)   ;
052700080829
052800110616       // Se richiesto file pulisco il lancio precedente per
052900110616       //  fil e ccm
053000110616       setll (dutpou: staccm)  wfddt01l   ;
053100110616       reade (dutpou: staccm)  wfddt01l   ;
053200110616
053300110616       if not %eof(wfddt01l)  and
053400110616          (stasfile='S'  or ddtedt<unmese) ;
053500110616
053600110616       dow  not %eof(wfddt01l)   ;
053700110616       delete wfddt000  ;
053800110616
053900110616       reade (dutpou: staccm)  wfddt01l   ;
054000110616       enddo  ;
054100110616        else  ;
054200110616        unlock   wfddt01l  ;
054300110616        endif  ;
054400110616
054500110617       // Ragione sociale mittente
054600110617        if stasfile='S'   and staccm>0 ;
054700110616        chain  (1:dutkci:staccm) cnaco00f  ;
054800110616         if not %found(cnaco00f)  ;
054900110616          acorag=*all'?'  ;
055000110616         endif ;
055100110616       endif ;
055200110616
055300080328       ENDSR                           ;
055400080317      /end-free
