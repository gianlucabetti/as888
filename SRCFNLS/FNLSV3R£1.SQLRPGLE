000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200080828      * FNLSV3R *----------------------------------------------------------*
000300080909      *  controllo documenti da restituire al mittente
000400080319      *--------------------------------------------------------------------*
000500080327     Fazorg01l  IF   E           K DISK
000600080327     Ftabel00f  IF   E           K DISK
000700080902     Ffnlbl01l  IF   E           K DISK
000800110616     Fcnaco00f  IF   E           K DISK
000900110617     FwfDDT01L  uF a E           K DISK    extfile(WWFDDT) usropn
001000080902     Ftitas30c  IF   E           K DISK    extfile(WTAs30) usropn
001100080829     Ftita430c  IF   E           K DISK    extfile(WTA430) usropn
001200080829     Ffiar531c  uF   E           K DISK    extfile(WTA531) usropn
001300080828     FFNLSV3P   O    E             PRINTER
001400050705      *
001500080317      * - Passaggio parametri
001600080325     d KPJBA         e ds
001700080328     d
001800080328     d tibs34ds      e ds                  inz
001900080410     d tibs02ds      e ds                  inz
002000080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
002100080328     d DDatiUte      e ds                  dtaara(§datiute)
002200080829     d
002300080829     d dar5gen       e ds
002400080902     d DS7O          e ds
002500100603     d DS7R          e ds
002600080328     d
002700080829     d tasSPE          ds
002800080829     d tasaas
002900080829     d tasmgs
003000080829     d TASDSP                  1      8  0
003100080829     d taslnp
003200080829     d tasnrs
003300080829     d tasnsp
003400080829     d
003500080828
003600080828     D fnlsv2ds        DS
003700080828     D  stainv                 1      8  0
003800080828     D  cominv                 9     16  0
003900080828     D  stainvrd              17     24  0
004000080828     D  cominvrd              25     32  0
004100080828     D  stainvra              33     40  0
004200080828     D  cominvra              41     48  0
004300080828     D  stadsd                49     56  0
004400080828     D  comdsd                57     64  0
004500080828     D  stadsa                65     72  0
004600080828     D  comdsa                73     80  0
004700080828     D  staccm                81     87  0
004800080829     D  staRAG                88    112
004900080902     D  stalnp               113    115  0
005000080902     D  stadlnp              116    140
005100110613     D  stasfile             141    141
005200110617     D  staemail             142    171
005300080828     d
005400110617     D  pInFILE        s             10
005500110617     D  pInIDJOB       s             26
005600110617     D  pInPWDXLS      s             15
005700110617     D  pInNOMXLS      s             78
005800110617     D  pInEMLDEST     s            121
005900111207     D  pInEMLVAR      s             80
006000110617     D  pOutESITO      s              1
006100080829     d savspe          s                   like(tasspe)
006200080829     d righe           s              3  0
006300110613     d dateu           s              8  0
006400110616     d unmese          s              8  0
006500110613     d ora_u           s              6  0
006600080829     d Wsta            s              1
006700080902     d STricezDDT      s              2
006800110613     d wlib            s             10    inz('GAITRAGRPS')
006900080828     d wrkgetlista     s           4096    varying
007000110613     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007100110613     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
007200110613     d Ora_iso         s               t   timfmt(*iso)
007300110613     d Dataiso         s               d   datfmt(*iso)
007400080327     d Dataeur         s               d   datfmt(*eur)
007500080327     d Datadmy         s               d   datfmt(*dmy)
007600080829     d
007700110613     d WTA430          s             21    inz('GAITRAGRPS/TITA430C')
007800110613     d WTAs30          s             21    inz('GAITRAGRPS/TITAS30C')
007900110613     d WTA531          s             21    inz('GAITRAGRPS/FIAR531C')
008000110617     d WWFDDT          s             21    inz('FILTRAAZP /WFDDT01L')
008100080901     d dec1            c                   const('  Stampa Inventario  AL:')
008200080901     d dec2            c                   const('RISTAMPA Inventario DAL:')
008300110616     d dec3            c                   const('    Data  consegna  DAL:')
008400080909     d tes1            c                   const('* CONTROLLO DOCUMENTI DA R-
008500080909     D                                     ESTITUIRE AL MITTENTE *')
008600080909     d Inv             c                   const('  Documenti inventariati  ')
008700080909     d RIinv           c                   const('Documenti già inventariati')
008800080909     d NOinv           c                   const(' Documenti non inventariat-
008900080909     D                                     i per bolle consegnate ')
009000080317      *------------------------------------------------------------------------*
009100080317      *   P R O T O T I P I
009200080317      *------------------------------------------------------------------------*
009300080829      /copy gaitrasrc/srcprotopr,tibs34r
009400110617      /copy gaitrasrc/srcprotopr,trtcm8r1
009500080829      *------------------------------------------------------------------------*
009600080829     Ifiar5000      81
009700080829     Ifiar5p00      82
009800001027     C*---------------------------------------------------------------*
009900001027     C     *ENTRY        PLIST
010000080325     C                   PARM                    kpjba
010100080317      /free
010200080828
010300080828       // Stampa o ristampa INVENTARIO DDT RICEVUTI
010400080829
010500080902 1     if STricezDDT='SI'        ;
010600080828
010700080828       // Prepara SQL
010800080901          clear wsta          ;
010900080902          EXSR SQLINV         ;
011000080902       endif                  ;
011100080902
011200080902       // Stampa le non inventariate
011300080902
011400080902 1     if STricezDDT='NO'        ;
011500080902          clear wsta          ;
011600080902          clear totale        ;
011700080902          EXSR SQLDSP      ;
011800080902       endif               ;
011900080901
012000080901       //  Esegui SQL  e stampa
012100080901          EXSR SQLesegui   ;
012200080901
012300080901 2     if   wsta='S'          ;
012400120124 3     //**if  righe>59           ;
012500120124 3     if  righe>57           ;
012600080901       write  LsV3PT1         ;
012700080901 3     endif                  ;
012800080901
012900110617       staEM =%trim(staemail)+'@bartolini.it' ;
013000080901       write  LsV3Pfine       ;
013100080901 1     endif               ;
013200110617
013300110617        // Se scritto almeno un record, invio all'indirizzo e-mail
013400110617        if *in01  ;
013500110617          pInFILE='WFDDT00F'   ;
013600110617          pInIDJOB=knmeb       ;
013700110617          pInEMLDEST=%trim(staemail)+'@bartolini.it' ;
013800111207  2       if  staccm>0   ;
013900111207          chain (1:dutkci:staccm) cnaco00f  ;
014000111207  3       if not %found(cnaco00f)  ;
014100111207            acorag=*all'?'  ;
014200111207  3       endif  ;
014300111207
014400111207            pinEMLVAR='*OBJM* Manca DDT Cliente: '+%editc(staccm:'X')+ '-'+
014500111207                       %subst(acorag:1:15)  ;
014600111207          endif ;
014700111207
014800110617          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
014900111207                          pInEMLVAR:pOutESITO) ;
015000110617        endif  ;
015100080901
015200080829
015300080317        *inlr ='1'   ;
015400080327       //-------------------------------------------------------------------
015500080901       BEGSR  SQLDSP                 ;
015600080828        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
015700080829                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
015800080828
015900080901
016000080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) = ''99999999'' +
016100080829                        and substr(ar5uni, 115, 1)='' ''';
016200080902
016300080902        if stalnp>0    ;
016400080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
016500080902        endif          ;
016600080902
016700080829        ENDSR     ;
016800080828
016900080901       //-------------------------------------------------------------------
017000080901       BEGSR  SQLINV                 ;
017100080901        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
017200080901                     from '+ wlib+ '/FIAR500f  WHERE ar5trd=''GEN'' and '      ;
017300080901
017400080901        // STampa
017500080901        if cominv>0     ;
017600080901        wrkgetlista=wrkgetlista + 'substr(ar5uni, 107, 8) <=  ''' +
017700080901                  %char(cominv) +''' and substr(ar5uni, 107, 8) > ''00000000'' +
017800080901                        and substr(ar5uni, 115, 1)='' ''';
017900080901        endif     ;
018000080901
018100080901        // RIstampa
018200080901
018300080901        if cominvra>0  ;
018400080901        wrkgetlista=wrkgetlista + ' substr(ar5uni, 115, 1)=''S'' and +
018500080901                    substr(ar5uni, 107, 8) <= ''' +
018600080901                    %char(cominvra) +''' and substr(ar5uni, 107, 8) >= ''' +
018700080901                    %char(cominvrd) + '''' ;
018800080901        endif     ;
018900080902
019000080902        if stalnp>0    ;
019100080902        wrkgetlista=wrkgetlista + ' and ar5lnp=' +%editc(stalnp:'X')   ;
019200080902        endif          ;
019300080902
019400080901        ENDSR     ;
019500080829       //-------------------------------------------------------------------
019600080829       BEGSR  SQLesegui              ;
019700080828       EXEC sql  prepare s1 from : wrkgetlista ;
019800080828       EXEC sql  declare a1 cursor for s1      ;
019900080828       EXEC sql  open    a1                    ;
020000080828
020100080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
020200080828                                        :ar5nsp, :ar5uni           ;
020300080828
020400080828       dow sqlcod=0    ;
020500080828
020600080829       exsr STAMPA     ;
020700080829
020800080828       EXEC sql fetch next from a1 into :ar5aas, :ar5lnp, :ar5nrs,
020900080828                                        :ar5nsp, :ar5uni           ;
021000080828       enddo           ;
021100080828
021200080829       EXEC sql  CLOSE   a1                    ;
021300080829
021400080828       ENDSR     ;
021500080327       //-------------------------------------------------------------------
021600080325       BEGSR  Stampa          ;
021700080325
021800080325       // Cerco la bolla
021900080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  titas30c  ;
022000080901
022100080902 1     if %found(titas30c) and (STricezDDT='SI' or
022200110616                         (tasdcm>=comdsd and tasdcm<=comdsa and tasdcm>0));
022300110616       //                (tasdsp>=comdsd and tasdsp<=comdsa and tasdcm>0));
022400100603
022500100603       // Escludo se non ha la particolarità RESTUTUZ.DDT
022600100603       clear ds7R ;
022700100603 2     if tasgma<>*blanks  ;
022800100603
022900100603         clear tblkey    ;
023000100603         tblkey=tasgma    ;
023100100603         chain (1:'7R':tblkey) tabel00f   ;
023200100603          if %found(tabel00f)   ;
023300100603          ds7r=tbluni       ;
023400100603          endif             ;
023500100603 2        endif             ;
023600100603
023700100603 1a    if STricezDDT='SI' or §7R1BF='B'   ;
023800080901
023900100528       // Escludo il cliente mittente se selezionato
024000100603 1b    if staccm=0 or (staccm>0 and tasccm=staccm)     ;
024100100528
024200080902       // Escludo se si tratta di bolla FIGLIA (elboro solo la mamma)
024300080902       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp)  fnlbl01l  ;
024400080902 2     if not %found(fnlbl01l) ;
024500080902
024600080829       stplnp=taslnp          ;
024700080829       stpnrs=tasnrs          ;
024800080327         if stpnrs>0            ;
024900080327         tra01='-'              ;
025000080327         else                   ;
025100080327         tra01=' '              ;
025200080327         endif                  ;
025300080829       stpnsp=tasnsp          ;
025400080829       dataiso=%date(tasdsp)    ;
025500080327       datadmy=dataiso        ;
025600080327       stpDSP=%dec(datadmy)   ;
025700080829       stplna=taslna          ;
025800080829       stpRMN=tasRMN          ;
025900080902
026000080902       // Data consegna        ;
026100080902       clear stpdcm  ;
026200080902       clear stpcca    ;
026300080902
026400080902 3     if tasdcm>0   ;
026500080902       DATAEUR=%DATE(tasdcm:*iso)       ;
026600080902       stpdcm=%dec(dataeur)   ;
026700080902 4     if tascca<>' '  ;
026800080902         clear tblkey    ;
026900080902         tblkey=tascca    ;
027000080902         stpcca=tascca    ;
027100080902
027200080902         chain (1:'7O':tblkey) tabel00f   ;
027300080902          if %found(tabel00f)   ;
027400080902          ds7o=tbluni       ;
027500080902           %subst(stpcca:2:10)='-'+tbluni   ;
027600080902          endif       ;
027700080902 4     endif       ;
027800080902 3     endif       ;
027900080829
028000080902       // Escludo dalla stampa delle non inventariate le bolle di reso
028100080902 3     if STricezDDT='SI' or tascca=' '  or §7ores<>'S' ;
028200080902
028300080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'A')  tita430c  ;
028400080902 4     IF %FOUND(TITA430C)    ;
028500080829       STPRMA=ta4NOT          ;
028600080829       ELSE                   ;
028700080829       clear stprma           ;
028800080902 4     endif                  ;
028900080829
029000080901       stprsd=tasrsd          ;
029100080829       stplod=taslod          ;
029200080829       %subst(stplod:13:3)=' '+tasprd    ;
029300080901
029400080901       // Data inventario      ;
029500080829       dar5gen=ar5uni     ;
029600080902 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
029700080901         DATAEUR=%DATE(%int(§ar5drdt):*iso)       ;
029800080901         stpinv=%dec(dataeur)   ;
029900080829       else   ;
030000080901        clear stpinv    ;
030100080902 4     endif           ;
030200080327
030300080325
030400080902       // totale bolle stampate
030500080902       totale=totale+1        ;
030600080902
030700080327       // Testata bolla
030800080327
030900110617 4     if  righe>63           ;
031000080829       write  LsV3PT1         ;
031100080328       righe=8                ;
031200080902 4     endif                  ;
031300080331
031400080829       write   LsV3PD1        ;
031500080327
031600080327       righe=righe+1          ;
031700080829       wsta='S'               ;
031800080829
031900110613       // Se richiesto --> creo file
032000110613       if   stasfile ='S'  ;
032100110613       clear wfddt000         ;
032200110616       ddtidute=knmeb   ;
032300110616       ddteut=knmus   ;
032400110613       ddtepo=dutpou  ;
032500110613       ddtedt=dateu   ;
032600110613       ddteor=ora_u   ;
032700110613       ddtccm=tasccm  ;
032800110616       // ragione sociale mittente
032900110616       if staccm=0  ;
033000110616       chain  (1:dutkci:tasccm) cnaco00f  ;
033100110616        if %found(cnaco00f)  ;
033200110616         ddtrsm=acorag   ;
033300110616        else  ;
033400110616        ddtrsm=*all'?' ;
033500110616        endif ;
033600110616       else  ;
033700110616        ddtrsm=acorag  ;
033800110616       endif  ;
033900110616
034000110613       ddtaas=tasaas  ;
034100110613       ddtlnp=taslnp  ;
034200110613       ddtnrs=tasnrs  ;
034300110613       ddtnsp=tasnsp  ;
034400110613       ddtmgs=tasmgs  ;
034500110613       ddtlna=taslna  ;
034600110613       ddtrmn=tasrmn  ;
034700110613       ddtrma=stprma  ;
034800110613       ddtrsd=tasrsd  ;
034900110613       ddtlod=taslod  ;
035000110613       ddtcad=tascad  ;
035100110613       ddtprd=tasprd  ;
035200110613       ddtnzd=tasnzd  ;
035300110613       ddtdcm=tasdcm  ;
035400110613       ddtcca=tascca  ;
035500110616 4     if §ar5drdt>*zeros and §ar5drdt<>'99999999'    ;
035600110616         ddtinv =%int(§ar5drdt)             ;
035700110616       else   ;
035800110616        clear ddtinv    ;
035900110616 4     endif           ;
036000110613       write wfddt000   ;
036100110616
036200110616       *in01 =*on  ;
036300110613       endif  ;
036400110613
036500080829       // Aggiorno fiar5 se stampa
036600080902 4     if cominv>0         ;
036700080829       *in81=*off     ;
036800080829       *in82=*off     ;
036900080829       chain (ar5aas:ar5lnp:ar5nrs:ar5nsp:'GEN')  fiar531c  ;
037000080902 5       if %found(fiar531c)    ;
037100080829          dar5gen=ar5uni      ;
037200080829          §ar5fstd='S'        ;
037300080829          ar5uni=dar5gen       ;
037400080902 6          if *in81      ;
037500080829            update fiar5000    ;
037600080829            else    ;
037700080829            update fiar5p00    ;
037800080902 6          endif   ;
037900080902 5       endif    ;
038000080902 4     endif    ;
038100080901
038200080902 3     endif    ;
038300080902 2     endif    ;
038400100603 1b    endif    ;
038500100603 1a    endif    ;
038600080902 1     endif    ;
038700080327
038800080325       ENDSR                  ;
038900080327       //-------------------------------------------------------------------
039000080327       BEGSR  *INZSR                 ;
039100080328
039200080328       in(E) *dtaara   ;
039300080328       if    %error or rsut  =*blanks  ;
039400080328       callp TIBS34R (TIBS34DS)        ;
039500080328       in    *dtaara   ;
039600080328       endif                           ;
039700080829
039800080829       // Apertura file di sede
039900080829             open(e) titas30c                ;
040000080829           if      not %open(titas30c)  ;
040100110613             %subst(WTA430:7:4)='GRU '    ;
040200110613             %subst(WTAs30:7:4)='GRU '    ;
040300110613             %subst(WTA531:7:4)='GRU '    ;
040400110617             %subst(WWFDDT:7:4)='AZM '    ;
040500110613             %subst(Wlib:7:4)='GRU '    ;
040600080829             open titas30c                ;
040700110617           endif  ;
040800080829             open tita430c                ;
040900080829             open fiar531c                ;
041000110617             open wfddt01l                ;
041100080829
041200080829       fnlsv2ds=kpjbu     ;
041300080829
041400080909       stptes   =tes1     ;
041500080909
041600080829       if cominvra=0      ;
041700080829       cominvra=cominvrd  ;
041800080901       stainvra=stainvrd  ;
041900080829       endif              ;
042000080829
042100080829       if cominv>0        ;
042200080901       stpdecinv=dec1     ;
042300080909       %subst(stptes2:11:26)  =Inv      ;
042400080829       stpdata1=stainv    ;
042500080829       clear    stpdata2  ;
042600080829       clear    stpdecinv2;
042700080902       STricezDDT='SI'      ;
042800080829       endif              ;
042900080829
043000080829       if cominvra>0      ;
043100080909       %subst(stptes2:11:26)  =RiInv      ;
043200080901       stpdecinv=dec2     ;
043300080901       stpdecinv2= 'AL'   ;
043400080829       stpdata1=stainvrd  ;
043500080829       stpdata2=stainvra  ;
043600080902       STricezDDT='SI'      ;
043700080829       endif              ;
043800080829
043900080901       if comdsd>0  or comdsa>0   ;
044000080902       STricezDDT='NO'      ;
044100080901
044200080901        if comdsd>0  and comdsa=0 ;
044300080901        comdsa=comdsd    ;
044400080901        Endif            ;
044500080901        if comdsd=0               ;
044600080901        comdsd=19900101  ;
044700080901        Endif            ;
044800080901
044900080909       stptes2  =NOInv    ;
045000080901       stpdecinv=dec3     ;
045100080901
045200080901       select     ;
045300080901       when stadsd=0        ;
045400080901         clear    stpdata2  ;
045500080901         clear    stpdecinv2;
045600080901         %subst(stpdecinv:21:4)='AL :'     ;
045700080901         stpdata1=stadsa    ;
045800080901
045900080901       when stadsa=0        ;
046000080901         clear    stpdata2  ;
046100080901         clear    stpdecinv2;
046200080901         %subst(stpdecinv:21:4)='IL :'     ;
046300080901         stpdata1=stadsd    ;
046400080901
046500080901       other                ;
046600080901         stpdata1=stadsd    ;
046700080901         stpdata2=stadsa    ;
046800080901         stpdecinv2= 'AL'   ;
046900080901        endSL              ;
047000080901       endif              ;
047100080902
047200080902       // Linea di partenza
047300080902       if stalnp=0  ;
047400080902       stpdlnp='TUTTE' ;
047500080902       else           ;
047600080903       stpdlnp='-'+ stadlnp ;
047700080902       endif          ;
047800080902
047900080829       righe=70   ;
048000110613       // imposto data e ora del giorno
048100110613       dataiso=datasys  ;
048200110613       dateu=%dec(dataiso)   ;
048300110616
048400110616       dataiso=dataiso-%days(30) ;
048500110616       unmese=%dec(dataiso)   ;
048600110616
048700110613       ora_iso=ora_sys  ;
048800110613       ora_u=%dec(ora_iso)   ;
048900080829
049000110616       // Se richiesto file pulisco il lancio precedente per
049100110616       //  fil e ccm
049200110616       setll (dutpou: staccm)  wfddt01l   ;
049300110616       reade (dutpou: staccm)  wfddt01l   ;
049400110616
049500110616       if not %eof(wfddt01l)  and
049600110616          (stasfile='S'  or ddtedt<unmese) ;
049700110616
049800110616       dow  not %eof(wfddt01l)   ;
049900110616       delete wfddt000  ;
050000110616
050100110616       reade (dutpou: staccm)  wfddt01l   ;
050200110616       enddo  ;
050300110616        else  ;
050400110616        unlock   wfddt01l  ;
050500110616        endif  ;
050600110616
050700110617       // Ragione sociale mittente
050800110617        if stasfile='S'   and staccm>0 ;
050900110616        chain  (1:dutkci:staccm) cnaco00f  ;
051000110616         if not %found(cnaco00f)  ;
051100110616          acorag=*all'?'  ;
051200110616         endif ;
051300110616       endif ;
051400110616
051500080328       ENDSR                           ;
051600080317      /end-free
