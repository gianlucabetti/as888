000100120302     /*PRM dbgview(*source)
000200120302     /*CMD  ovrdbf file(WFDDT01L) tofile(FILTRAAZP/WFDDT01L) +
000300120302     /*CMD         ovrscope(*calllvl)
000400120302     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000500120302     /*CMD         ovrscope(*calllvl)
000600120302     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
000700120302     /*CMD         ovrscope(*calllvl)
000800120302     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000900120302     /*CMD         ovrscope(*calllvl)
001000120302     /*END  dltovr file(WFDDT01L) lvl(*)
001100120302     /*END  dltovr file(TITAS30C TITA430C FIAR531C) lvl(*)
001200120302     /*END
001300120302      *
001400001027     H DECEDIT('0,') DATEDIT(*DMY.)
001500080828      * FNLSV3R *----------------------------------------------------------*
001600080909      *  controllo documenti da restituire al mittente
001700080319      *--------------------------------------------------------------------*
001800080327     Fazorg01l  IF   E           K DISK
001900080327     Ftabel00f  IF   E           K DISK
002000080902     Ffnlbl01l  IF   E           K DISK
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
006600080828     d
006700110617     D  pInFILE        s             10
006800110617     D  pInIDJOB       s             26
006900110617     D  pInPWDXLS      s             15
007000110617     D  pInNOMXLS      s             78
007100110617     D  pInEMLDEST     s            121
007200111207     D  pInEMLVAR      s             80
007300110617     D  pOutESITO      s              1
007400080829     d savspe          s                   like(tasspe)
007500080829     d righe           s              3  0
007600110613     d dateu           s              8  0
007700110616     d unmese          s              8  0
007800110613     d ora_u           s              6  0
007900080829     d Wsta            s              1
008000080902     d STricezDDT      s              2
008100110613     d wlib            s             10    inz('GAITRAGRPS')
008200080828     d wrkgetlista     s           4096    varying
008300110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
008400110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
008500110613     d Ora_iso         s               t   timfmt(*iso)
008600110613     d Dataiso         s               d   datfmt(*iso)
008700080327     d Dataeur         s               d   datfmt(*eur)
008800080327     d Datadmy         s               d   datfmt(*dmy)
008900080829     d
009000110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
009100110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
009200110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
009300110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
009400080901     d dec1            c                   const('  Stampa Inventario  AL:')
009500080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
009600110616     d dec3            c                   const('    Data  consegna  DAL:')
009700080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
009800080909     D                                     ESTITUIRE AL MITTENTE *')
009900080909     d Inv             c                   const('  Documenti inventariati  ')
010000080909     d RIinv           c                   const('Documenti già inventariati')
010100080909     d NOinv           c                   const(' Documenti non inventariat-
010200080909     D                                     i per bolle consegnate ')
010300080317      *------------------------------------------------------------------------*
010400080317      *   P R O T O T I P I
010500080317      *------------------------------------------------------------------------*
010600080829      /copy gaitrasrc/srcprotopr,tibs34r
010700110617      /copy gaitrasrc/srcprotopr,trtcm8r1
010800080829      *------------------------------------------------------------------------*
010900080829     Ifiar5000      81
011000080829     Ifiar5p00      82
011100001027     C*---------------------------------------------------------------*
011200001027     C     *ENTRY        PLIST
011300080325     C                   PARM                    kpjba
011400080317      /free
011500080828
011600080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
011700080829
011800080902 1     if STricezDDT='SI'        ;
011900080828
012000080828       // Prepara SQL
012100080901          clear wsta          ;
012200080902          EXSR SQLINV         ;
012300080902       endif                  ;
012400080902
012500080902       // Stampa le non inventariate
012600080902
012700080902 1     if STricezDDT='NO'        ;
012800080902          clear wsta          ;
012900080902          clear totale        ;
013000080902          EXSR SQLDSP      ;
013100080902       endif               ;
013200080901
013300080901       //  Esegui SQL  e stampa
013400080901          EXSR SQLesegui   ;
013500080901
013600080901 2     if   wsta='S'          ;
013700120124 3     //**if  righe>59           ;
013800120124 3     if  righe>57           ;
013900080901       write  LsV3PT1         ;
014000080901 3     endif                  ;
014100080901
014200120302       staEM =%trim(staemail)+'@brt.it' ;
014300080901       write  LsV3Pfine       ;
014400080901 1     endif               ;
014500110617
014600110617        // Se scritto almeno un record, invio all'indirizzo e-mail
014700110617        if *in01  ;
014800110617          pInFILE='WFDDT00F'   ;
014900110617          pInIDJOB=knmeb       ;
015000120302          pInEMLDEST=%trim(staemail)+'@brt.it' ;
015100111207  2       if  staccm>0   ;
015200111207          chain (1:dutkci:staccm) cnaco00f  ;
015300111207  3       if not %found(cnaco00f)  ;
015400111207            acorag=*all'?'  ;
015500111207  3       endif  ;
015600111207
015700111207            pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
015800111207                       %subst(acorag:1:15)  ;
015900111207          endif ;
016000111207
016100110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
016200111207                          pInEMLVAR:pOutESITO) ;
016300110617        endif  ;
016400080901
016500080829
016600080317        *inlr ='1'   ;
016700080327       //-------------------------------------------------------------------
016800080901       BEGSR  SQLDSP                 ;
016900080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
017000080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
017100080828
017200080901
017300080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
017400080829                        and substr(ar5uni, 115, 1)='' ''';
017500080902
017600080902        if stalnp>0    ;
017700080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
017800080902        endif          ;
017900080902
018000080829        ENDSR     ;
018100080828
018200080901       //-------------------------------------------------------------------
018300080901       BEGSR  SQLINV                 ;
018400080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
018500080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
018600080901
018700080901        // STampa
018800080901        if cominv>0     ;
018900080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
019000080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
019100080901                        and substr(ar5uni, 115, 1)='' ''';
019200080901        endif     ;
019300080901
019400080901        // RIstampa
019500080901
019600080901        if cominvra>0  ;
019700080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
019800080901                    substr(ar5uni, 107, 8) <= ''' +
019900080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
020000080901                    %char(cominvrd) + '''' ;
020100080901        endif     ;
020200080902
020300080902        if stalnp>0    ;
020400080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
020500080902        endif          ;
020600080902
020700080901        ENDSR     ;
020800080829       //-------------------------------------------------------------------
020900080829       BEGSR  SQLesegui              ;
021000080828       EXEC sql  prepare s1 from : wrkgetlista ;
021100080828       EXEC sql  declare a1 cursor for s1      ;
021200080828       EXEC sql  open    a1                    ;
021300080828
021400080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
021500080828                                        :ar5nsp, :ar5uni           ;
021600080828
021700080828       dow sqlcod=0    ;
021800080828
021900080829       exsr STAMPA     ;
022000080829
022100080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
022200080828                                        :ar5nsp, :ar5uni           ;
022300080828       enddo           ;
022400080828
022500080829       EXEC sql  CLOSE   a1                    ;
022600080829
022700080828       ENDSR     ;
022800080327       //-------------------------------------------------------------------
022900080325       BEGSR  Stampa          ;
023000080325
023100080325       // Cerco la bolla
023200080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
023300080901
023400080902 1     if %found(titas30c) and (STricezDDT='SI' or
023500110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
023600110616       //                (tasdsp>=comdsd and tasdsp<=comdsa and tasdcm>0));
023700100603
023800100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
023900100603       clear ds7R ;
024000100603 2     if tasgma<>*blanks  ;
024100100603
024200100603         clear tblkey    ;
024300100603         tblkey=tasgma    ;
024400100603         chain (1:'7R':tblkey) tabel00f   ;
024500100603          if %found(tabel00f)   ;
024600100603          ds7r=tbluni       ;
024700100603          endif             ;
024800100603 2        endif             ;
024900100603
025000100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
025100080901
025200100528       // Escludo il cliente mittente se selezionato
025300100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
025400100528
025500080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
025600080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
025700080902 2     if not %found(fnlbl01l) ;
025800080902
025900080829       stplnp=taslnp          ;
026000080829       stpnrs=tasnrs          ;
026100080327         if stpnrs>0            ;
026200080327         tra01='-'              ;
026300080327         else                   ;
026400080327         tra01=' '              ;
026500080327         endif                  ;
026600080829       stpnsp=tasnsp          ;
026700080829       dataiso=%date(tasdsp)    ;
026800080327       datadmy=dataiso        ;
026900080327       stpDSP=%dec(datadmy)   ;
027000080829       stplna=taslna          ;
027100080829       stpRMN=tasRMN          ;
027200080902
027300080902       // Data consegna        ;
027400080902       clear stpdcm  ;
027500080902       clear stpcca    ;
027600080902
027700080902 3     if tasdcm>0   ;
027800080902       DATAEUR=%DATE(tasdcm:*iso)       ;
027900080902       stpdcm=%dec(dataeur)   ;
028000080902 4     if tascca<>' '  ;
028100080902         clear tblkey    ;
028200080902         tblkey=tascca    ;
028300080902         stpcca=tascca    ;
028400080902
028500080902         chain (1:'7O':tblkey) tabel00f   ;
028600080902          if %found(tabel00f)   ;
028700080902          ds7o=tbluni       ;
028800080902           %subst(stpcca:2:10)='-'+tbluni   ;
028900080902          endif       ;
029000080902 4     endif       ;
029100080902 3     endif       ;
029200080829
029300080902       // Escludo dalla stampa delle non inventariate le bolle di reso
029400080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
029500080902
029600080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
029700080902 4     IF %FOUND(TITA430C)    ;
029800080829       STPRMA=ta4NOT          ;
029900080829       ELSE                   ;
030000080829       clear stprma           ;
030100080902 4     endif                  ;
030200080829
030300080901       stprsd=tasrsd          ;
030400080829       stplod=taslod          ;
030500080829       %subst(stplod:13:3)=' '+tasprd    ;
030600080901
030700080901       // Data inventario      ;
030800080829       dar5gen=ar5uni     ;
030900080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
031000080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
031100080901         stpinv=%dec(dataeur)   ;
031200080829       else   ;
031300080901        clear stpinv    ;
031400080902 4     endif           ;
031500080327
031600080325
031700080902       // totale bolle stampate
031800080902       totale=totale+1        ;
031900080902
032000080327       // Testata bolla
032100080327
032200110617 4     if  righe>63           ;
032300080829       write  LsV3PT1         ;
032400080328       righe=8                ;
032500080902 4     endif                  ;
032600080331
032700080829       write   LsV3PD1        ;
032800080327
032900080327       righe=righe+1          ;
033000080829       wsta='S'               ;
033100080829
033200110613       // Se richiesto --> creo file
033300110613       if   stasfile ='S'  ;
033400110613       clear wfddt000         ;
033500110616       ddtidute=knmeb   ;
033600110616       ddteut=knmus   ;
033700110613       ddtepo=dutpou  ;
033800110613       ddtedt=dateu   ;
033900110613       ddteor=ora_u   ;
034000110613       ddtccm=tasccm  ;
034100110616       // ragione sociale mittente
034200110616       if staccm=0  ;
034300110616       chain  (1:dutkci:tasccm) cnaco00f  ;
034400110616        if %found(cnaco00f)  ;
034500110616         ddtrsm=acorag   ;
034600110616        else  ;
034700110616        ddtrsm=*all'?' ;
034800110616        endif ;
034900110616       else  ;
035000110616        ddtrsm=acorag  ;
035100110616       endif  ;
035200110616
035300110613       ddtaas=tasaas  ;
035400110613       ddtlnp=taslnp  ;
035500110613       ddtnrs=tasnrs  ;
035600110613       ddtnsp=tasnsp  ;
035700110613       ddtmgs=tasmgs  ;
035800110613       ddtlna=taslna  ;
035900110613       ddtrmn=tasrmn  ;
036000110613       ddtrma=stprma  ;
036100110613       ddtrsd=tasrsd  ;
036200110613       ddtlod=taslod  ;
036300110613       ddtcad=tascad  ;
036400110613       ddtprd=tasprd  ;
036500110613       ddtnzd=tasnzd  ;
036600110613       ddtdcm=tasdcm  ;
036700110613       ddtcca=tascca  ;
036800110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
036900110616         ddtinv =%int(§ar5drdt)             ;
037000110616       else   ;
037100110616        clear ddtinv    ;
037200110616 4     endif           ;
037300110613       write wfddt000   ;
037400110616
037500110616       *in01 =*on  ;
037600110613       endif  ;
037700110613
037800080829       // Aggiorno fiar5 se stampa
037900080902 4     if cominv>0         ;
038000080829       *in81=*off     ;
038100080829       *in82=*off     ;
038200080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
038300080902 5       if %found(fiar531c)    ;
038400080829          dar5gen=ar5uni      ;
038500080829          §ar5fstd='S'        ;
038600080829          ar5uni=dar5gen       ;
038700080902 6          if *in81      ;
038800080829            update fiar5000    ;
038900080829            else    ;
039000080829            update fiar5p00    ;
039100080902 6          endif   ;
039200080902 5       endif    ;
039300080902 4     endif    ;
039400080901
039500080902 3     endif    ;
039600080902 2     endif    ;
039700100603 1b    endif    ;
039800100603 1a    endif    ;
039900080902 1     endif    ;
040000080327
040100080325       ENDSR                  ;
040200080327       //-------------------------------------------------------------------
040300080327       BEGSR  *INZSR                 ;
040400080328
040500080328       in(E) *dtaara   ;
040600080328       if    %error or rsut  =*blanks  ;
040700080328       callp TIBS34R (TIBS34DS)        ;
040800080328       in    *dtaara   ;
040900080328       endif                           ;
041000080829
041100080829       // Apertura file di sede
041200080829             open(e) titas30c                ;
041300080829           if      not %open(titas30c)  ;
041400110613             %subst(WTA430:7:4)='GRU '    ;
041500110613             %subst(WTAs30:7:4)='GRU '    ;
041600110613             %subst(WTA531:7:4)='GRU '    ;
041700110617             %subst(WWFDDT:7:4)='AZM '    ;
041800110613             %subst(Wlib:7:4)='GRU '    ;
041900080829             open titas30c                ;
042000110617           endif  ;
042100080829             open tita430c                ;
042200080829             open fiar531c                ;
042300110617             open wfddt01l                ;
042400080829
042500080829       fnlsv2ds=kpjbu     ;
042600080829
042700080909       stptes   =tes1     ;
042800080909
042900080829       if cominvra=0      ;
043000080829       cominvra=cominvrd  ;
043100080901       stainvra=stainvrd  ;
043200080829       endif              ;
043300080829
043400080829       if cominv>0        ;
043500080901       stpdecinv=dec1     ;
043600080909       %subst(stptes2:11:26)  =Inv      ;
043700080829       stpdata1=stainv    ;
043800080829       clear    stpdata2  ;
043900080829       clear    stpdecinv2;
044000080902       STricezDDT='SI'      ;
044100080829       endif              ;
044200080829
044300080829       if cominvra>0      ;
044400080909       %subst(stptes2:11:26)  =RiInv      ;
044500080901       stpdecinv=dec2     ;
044600080901       stpdecinv2= 'AL'   ;
044700080829       stpdata1=stainvrd  ;
044800080829       stpdata2=stainvra  ;
044900080902       STricezDDT='SI'      ;
045000080829       endif              ;
045100080829
045200080901       if comdsd>0  or comdsa>0   ;
045300080902       STricezDDT='NO'      ;
045400080901
045500080901        if comdsd>0  and comdsa=0 ;
045600080901        comdsa=comdsd    ;
045700080901        Endif            ;
045800080901        if comdsd=0               ;
045900080901        comdsd=19900101  ;
046000080901        Endif            ;
046100080901
046200080909       stptes2  =NOInv    ;
046300080901       stpdecinv=dec3     ;
046400080901
046500080901       select     ;
046600080901       when stadsd=0        ;
046700080901         clear    stpdata2  ;
046800080901         clear    stpdecinv2;
046900080901         %subst(stpdecinv:21:4)='AL :'     ;
047000080901         stpdata1=stadsa    ;
047100080901
047200080901       when stadsa=0        ;
047300080901         clear    stpdata2  ;
047400080901         clear    stpdecinv2;
047500080901         %subst(stpdecinv:21:4)='IL :'     ;
047600080901         stpdata1=stadsd    ;
047700080901
047800080901       other                ;
047900080901         stpdata1=stadsd    ;
048000080901         stpdata2=stadsa    ;
048100080901         stpdecinv2= 'AL'   ;
048200080901        endSL              ;
048300080901       endif              ;
048400080902
048500080902       // Linea di partenza
048600080902       if stalnp=0  ;
048700080902       stpdlnp='TUTTE' ;
048800080902       else           ;
048900080903       stpdlnp='-'+ stadlnp ;
049000080902       endif          ;
049100080902
049200080829       righe=70   ;
049300110613       // imposto data e ora del giorno
049400110613       dataiso=datasys  ;
049500110613       dateu=%dec(dataiso)   ;
049600110616
049700110616       dataiso=dataiso-%days(30) ;
049800110616       unmese=%dec(dataiso)   ;
049900110616
050000110613       ora_iso=ora_sys  ;
050100110613       ora_u=%dec(ora_iso)   ;
050200080829
050300110616       // Se richiesto file pulisco il lancio precedente per
050400110616       //  fil e ccm
050500110616       setll (dutpou: staccm)  wfddt01l   ;
050600110616       reade (dutpou: staccm)  wfddt01l   ;
050700110616
050800110616       if not %eof(wfddt01l)  and
050900110616          (stasfile='S'  or ddtedt<unmese) ;
051000110616
051100110616       dow  not %eof(wfddt01l)   ;
051200110616       delete wfddt000  ;
051300110616
051400110616       reade (dutpou: staccm)  wfddt01l   ;
051500110616       enddo  ;
051600110616        else  ;
051700110616        unlock   wfddt01l  ;
051800110616        endif  ;
051900110616
052000110617       // Ragione sociale mittente
052100110617        if stasfile='S'   and staccm>0 ;
052200110616        chain  (1:dutkci:staccm) cnaco00f  ;
052300110616         if not %found(cnaco00f)  ;
052400110616          acorag=*all'?'  ;
052500110616         endif ;
052600110616       endif ;
052700110616
052800080328       ENDSR                           ;
052900080317      /end-free
