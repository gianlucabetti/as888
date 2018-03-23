000100031205      *------------------------------------------------------------------------*
000200081119      * Controlli WADEL, TNCPO, TNCPI   per Mappatura
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600081215     fazorg01l  if   e           k Disk
000700081215     fTntbe01l  if   e           k Disk
000800080102     ftabel00f  if   e           k Disk
000900081215     fTNcpo01L  if   e           k Disk
001000081119     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
001100081119     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
001200081119     fCNACO00F  if   e           k Disk
001300081119     fCNIND00F  if   e           k Disk
001400081119     fCNCLP00F  if   e           k Disk
001500090210     FWADELpCT  iF   E             DISK    usropn extfile(CWADEL)
001600071129     F
001700090114     fwfdei01l  if   e           k Disk    rename(wfdei000:wfdei001)
001800090123     f                                     usropn extfile(CWFDEI1)
001900090123     FWFDEi05l  uF a E           k DISK    usropn extfile(CWFDEI)
002000090123     FWFDEid0F  uF a E             DISK    usropn
002100031205
002200031205      *------------------------------------------------------------------------*
002300031205      *   C A M P I   D I   L A V O R O
002400031205      *------------------------------------------------------------------------*
002500090114     d CMDLY           s             50    dim(1) ctdata perrcd(1)
002600090114
002700110414     d potsede         s             11  0 dim(101)
002800110414     d potinfo         s              1    dim(101)
002900110414     d potinfop        s              1    dim(101)
003000081119     D
003100090114     d comman          s            100
003200090114     d lung            s             15  5
003300090114     d kcod            s              7
003400081215     d Wfil            s              3  0
003500081215     d Wksc            s              7  0
003600081119     d xx              s              4  0
003700081119     d Indx            s              4  0
003800081119     d yy              s              4  0
003900081119     d kut             s              1  0 inz(1)
004000080102     d kkey            s                   like(tblkey)
004100071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
004200090114     D*
004300090114     D PARAM           DS
004400090114     D* CODICE FILIALE
004500090114     D  prmFIL                 1      3  0
004600090114     D* CODICE COMMERCIALE unificante (0 = TUTTI)
004700090114     D  prmagu                 4     10  0
004800090114     D  prmARE                11     13  0
004900090114     d*
005000090114     d* prmela='T'  --> rielabora da WADEL tutto
005100090114     D  prmela                14     14
005200090114     d*
005300090114     d* prmerr='E'  --> elaborazione non effettuabile
005400090114     d*                 per mancanza di dati
005500090114     D  prmerr                15     15
005600090123
005700090123     d CWFDEI          s             21    inz('GAITRAAZM /WFDEI05L')
005800090123     d CWFDEI1         s             21    inz('GAITRAAZM /WFDEI01L')
005900090210     d CWaDEL          s             21    inz('EDPMAPPA  /WADELPCT')
006000031205      *------------------------------------------------------------------------*
006100031205      *   D S   I N T E R N E / E S T E R N E
006200031205      *------------------------------------------------------------------------*
006300081215     d TRMK50ds      e ds                  inz
006400081119     d Dcpo01        e ds
006500090213     d DS01          e ds
006600081119     d Kpjba         e ds
006700081119      // - Ricerca unificante padre
006800081119     d TIBS10ds      e ds                  inz
006900081119     d  skc                   21   5520    dim(500)
007000081119      // - Reperimento dati utente
007100081119     d TIBS34ds      e ds
007200081119      // - Dati utente
007300081119     d §AzUte        e ds                  extname(AZUTE00F)
007400081119     d                                     dtaara
007500081119     d §DatiUte      e ds                  extname(dDatiUte)
007600081119     d                                     dtaara
007700081119
007800071122     d
007900071122      *------------------------------------------------------------------------*
008000071122      *   P R O T O T I P I
008100071122      *------------------------------------------------------------------------*
008200081119      /copy gaitrasrc/srcprotopr,tibs10r
008300081119      /copy gaitrasrc/srcprotopr,tibs34r
008400081215      /copy gaitrasrc/srcprotopr,trmk50r
008500090114      /copy gaitrasrc/srcprotopr,qcmdexc
008600081119      //---------------------------------------------------------------
008700081119      //?M A I N - L I N E
008800081119      //---------------------------------------------------------------
008900081119     c     *Entry        plist
009000081119     c                   parm                    KPJBA
009100090114     c                   movel     kpjbu         param
009200081119
009300081119      /free
009400081119
009500081119         // Reperimento dati job
009600081119         exsr DatiJob;
009700080108
009800090114         // pulizie file di work
009900090114         exsr Pulizwork   ;
010000080108
010100090120         if prmerr=' '    ;
010200090120
010300090114         // Elabora da WADEL
010400090114         if prmela='T'  ;
010500090114           exsr ElaWADEL  ;
010600090114         else  ;
010700090114           exsr ElaWFDEI  ;
010800090114         endif        ;
010900090120         endif        ;
011000071218
011100090120       kpjbu =param   ;
011200071121       *inlr=*on         ;
011300090114       //-------------------------------------------------------------
011400090114       //?Pulizia file di work
011500090114       //--------------------------------------------------------------
011600090114         BEGSR PulizWork    ;
011700090123
011800090123       // apertura file in GAITRAAZM
011900090123       OPEN(e)   WFDEI05L              ;
012000090123
012100090123       if        not %open(wfdei05l)    ;
012200090123         %subst(CWFDEI:7:4)='AZP '      ;
012300090123         %subst(CWFDEI1:7:4)='AZP '      ;
012400090123         OPEN      WFDEI05L             ;
012500090123       endif                            ;
012600090123
012700090123         OPEN      WFDEI01L             ;
012800090114
012900090123       // se devo rielaborare anche WADEL cancello
013000090114 0     if prmela='T'   ;
013100090114
013200090126       open  wadelpct     ;
013300090114
013400090114       setll  *loval  wfdei05l  ;
013500090114       read    wfdei000   ;
013600090114 1     dow not %eof(wfdei05l) ;
013700090114       delete  wfdei000  ;
013800090114       read    wfdei05l   ;
013900090114 1     enddo ;
014000090114
014100090123       open WFDEID0F   ;
014200090114       setll  *start  wfdeid0F  ;
014300090114       read    wfdeid00   ;
014400090114 1     dow not %eof(wfdeid0F) ;
014500090114       delete  wfdeid00  ;
014600090114       read    wfdeid0F   ;
014700090114 1     enddo ;
014800090114
014900090114 x0    else   ;
015000090114
015100090114       // Vedo se presente record di fine altrimenti attendo con uh dlyjob
015200090114       prmerr='E'    ;
015300090114       xx=1   ;
015400090114
015500090114 1     dow  xx<=3   ;
015600090114       chain  ('FINE            ')   WFdei01l  ;
015700090114 2     if    %found(wfdei01l)   ;
015800090114         clear prmerr       ;
015900090114         leave   ;
016000090114       else  ;
016100090114         comman=cmdly(1)  ;
016200090114         lung=50   ;
016300090114         callp QCMDEXC (comman:lung)   ;
016400090114 2     endif   ;
016500090114
016600090114       xx=xx+1   ;
016700090114 1     enddo     ;
016800090114 0     endif   ;
016900090114
017000090114       ENDSR   ;
017100090114       //-------------------------------------------------------------
017200090126       //?elabora da WADELpcT
017300090114       //--------------------------------------------------------------
017400090114       BEGSR ElaWADEL    ;
017500090126       setll  *start  wadelpct  ;
017600090126       read    wadelpct ;
017700090114
017800090126 1     dow  not %eof(wadelpct) ;
017900090114
018000090213       // Prendo l'agente da CNCLP00f attuale
018100090213             chain (kut:dutkci:wapcli)  cnclp00f ;
018200090213 2           if %found(cnclp00f) ;
018300090213              clear kkey    ;
018400090213              kkey=%editc(clpage:'X') ;
018500090213              chain (1: '01': kkey) tabel00f ;
018600090213 3            if %found(tabel00f) ;
018700090213                ds01=tbluni    ;
018800090213                wapauc=§01rgf    ;
018900090213                clear kkey    ;
019000090213                kkey=%editc(§01rgf:'X') ;
019100090213                chain (1: '01': kkey) tabel00f ;
019200090213 4               if %found(tabel00f) ;
019300090213                  wapaud  =tbluni  ;
019400090213 4               endif  ;
019500090213 3            endif  ;
019600090213 2          endif    ;
019700090213
019800090213         // Escludo distretto estero   agente unificante attuale
019900090114         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
020000090114         chain   wfil   azorg01l   ;
020100090213
020200090213             waparau=orgcar   ;
020300090213             clear   kkey  ;
020400090213             kkey=%editc(orgcar: 'X')    ;
020500090213             chain (1: '05': kkey) tabel00f ;
0206000902132            if %found(tabel00f) ;
020700090213               waparaud=tbluni  ;
0208000902132            endif  ;
020900090114
021000090114 2a      if orgfl3<>'7'    ;
021100090114
021200090114         kcod = %editc(wapcli: 'X' ) ;
021300090114         // Escludo codici indirizzo italia ma esteri
021400090114            if kcod<>'0894106' and kcod<>'0894274' ;
021500090114
021600090114         // Escludo 8888  e 9999
021700090114 2         if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
021800090114
021900090114         // Escludo senza fatturato  o senza spedizioni  anno corrente
022000090114 3          if   wapfac>0 and wapsac>0;
022100090114
022200090122 4          if wapblo=' ' ;
022300090114
022400090114             clear  wfdei000   ;
022500090114             clear  wfdeid00   ;
022600090114
022700090114             deikscu=wapcli ;
022800090114             deirag =wapcld ;
022900090114             deicmmu=wapauc ;
023000090114             deidcmm=wapaud ;
023100090114             deifil =wfil  ;
023200090114             deidfil=orgdes;
023300090114             deiare =waparau ;
023400090114             deidare=waparaud;
023500090114             deiblo =wapblo;
023600090114             deiclv =wapclv;
023700090126             select  ;
023800090126             when deiclv='T'   ;
023900090126             deiord='1'  ;
024000090126             when deiclv='A'   ;
024100090126             deiord='2'  ;
024200090126             when deiclv='B'   ;
024300090126             deiord='3'  ;
024400090126             when deiclv='C'   ;
024500090126             deiord='4'  ;
024600090126             when deiclv='D'   ;
024700090126             deiord='5'  ;
024800090126             other   ;
024900090126             deiord='6'  ;
025000090126             endsl    ;
025100090126
025200090114             deicap =wapcap;
025300090114             deiprv =wapprv;
025400090114             deinaz =wapnaz;
025500090114             deipiv =wapiva;
025600090114             deicdf =wapcdf;
025700090114             deidmerc=wapitc;
025800090114
025900090114
026000090114               chain (kut:dutkci:wapcli)  cnaco00f ;
026100090114 5              if %found(cnaco00f) ;
026200090114             deirag  =%triml(acorag)   ;
026300090121               deicmerc=%subst(%editc(acoitc:'Z'):3:5)          ;
026400090114
026500090213             clear kkey ;
026600090114             kkey=deicmerc               ;
026700090114             chain (1: '1L': kkey) tabel00f ;
026800090114 6           if %found(tabel00f) ;
026900090114              deidmerc=tbluni  ;
027000090114 6           endif  ;
027100090114 5           endif  ;
027200090114
027300090114             chain (kut:dutkci:wapcli)  cnind00f ;
027400090114 5           if %found(cnind00f) ;
027500090114             deicap =indcae;
027600090114             deiprv =indprv;
027700090114             deinaz =indsta;
027800090114             deipiv =indiva;
027900090114             deicdf =indcdf;
028000090114 5         endif    ;
028100090114
028200090114           EXSR ElaboraDati   ;
028300090114
028400090114
028500090114           write(e) wfdei000   ;
028600090114
028700090114 4         endif  ;
028800090114 4         endif  ;
028900090114 3         endif  ;
029000090114 2         endif  ;
029100090114 2a        endif  ;
029200090114
029300090126       read    wadelpct ;
029400090114 1     enddo ;
029500090114
029600090114       // Scrivo nel wfdei00f record di FINE elaborazione per
029700090114       //  non far creare la stampa se sta ricreando i dati
029800090114       clear wfdei000   ;
029900090114       deipiv='FINE'    ;
030000090114       write wfdei000   ;
030100090114
030200090114       ENDSR   ;
030300090114       //-------------------------------------------------------------
030400090114       //?Elabora da WFDEI00F
030500090114       //--------------------------------------------------------------
030600090114       BEGSR ElaWFDEI    ;
030700090114
030800090114       setll   (prmare:prmfil:prmagu)     wfdei05l  ;
030900090121       if prmagu=0   ;
031000090114       reade   (prmare:prmfil)            wfdei05l  ;
031100090114       else   ;
031200090114       reade   (prmare:prmfil:prmagu)     wfdei05l  ;
031300090114       endif   ;
031400090114
031500090114       dow   not %eof(wfdei05l)    ;
031600090114       clear deicpoal  ;
031700090114       clear deicpo1   ;
031800090114       clear deicpi1   ;
031900090114       clear deicpi1p  ;
032000090114       clear deicpo2   ;
032100090114       clear deicpi2   ;
032200090114       clear deicpi2p  ;
032300090114       clear deicpo3   ;
032400090114       clear deicpi3   ;
032500090114       clear deicpi3p  ;
032600090114       clear deicpo4   ;
032700090114       clear deicpi4   ;
032800090114       clear deicpi4p  ;
032900090114       clear deicpo5   ;
033000090114       clear deicpi5   ;
033100090114       clear deicpi5p  ;
033200090114       clear deicpo6   ;
033300090114       clear deicpi6   ;
033400090114       clear deicpi6p  ;
033500090114       clear deicpo7   ;
033600090114       clear deicpi7   ;
033700090114       clear deicpi7p  ;
033800090114       clear deicpo8   ;
033900090114       clear deicpi8   ;
034000090114       clear deicpi8p  ;
034100090114       clear deicpo9   ;
034200090114       clear deicpi9   ;
034300090114       clear deicpi9p  ;
034400090114
034500090114       exsr Elaboradati   ;
034600090114
034700090114       update wfdei000  ;
034800090114
034900090114
035000090121       if prmagu=0   ;
035100090114       reade   (prmare:prmfil)             wfdei05l  ;
035200090114       else   ;
035300090114       reade   (prmare:prmfil:prmagu)     wfdei05l  ;
035400090114       endif   ;
035500090114
035600090114       enddo   ;
035700090114
035800090114       ENDSR             ;
035900090114       //-------------------------------------------------------------
036000090114       //?Elaborazione dati potenziali per riempire WFDEI
036100090114       //--------------------------------------------------------------
036200090114           BEGSR ElaboraDati   ;
036300090114
036400090114           EXSR CercaPotSede   ;
036500090114           xx=1   ;
036600090114           dow potsede(xx)>0  ;
036700090114
036800090114           select  ;
036900090114           when deicpo1=0       ;
037000090114             deicpo1=potsede(xx) ;
037100090114             deicpi1=potinfo(xx);
037200090114             deicpi1p=potinfop(xx);
037300090114           when deicpo2=0       ;
037400090114             deicpo2=potsede(xx) ;
037500090114             deicpi2=potinfo(xx);
037600090114             deicpi2p=potinfop(xx);
037700090114           when deicpo3=0       ;
037800090114             deicpo3=potsede(xx) ;
037900090114             deicpi3=potinfo(xx);
038000090114             deicpi3p=potinfop(xx);
038100090114           when deicpo4=0       ;
038200090114             deicpo4=potsede(xx) ;
038300090114             deicpi4=potinfo(xx);
038400090114             deicpi4p=potinfop(xx);
038500090114           when deicpo5=0       ;
038600090114             deicpo5=potsede(xx) ;
038700090114             deicpi5=potinfo(xx);
038800090114             deicpi5p=potinfop(xx);
038900090114           when deicpo6=0       ;
039000090114             deicpo6=potsede(xx) ;
039100090114             deicpi6=potinfo(xx);
039200090114             deicpi6p=potinfop(xx);
039300090114           when deicpo7=0       ;
039400090114             deicpo7=potsede(xx) ;
039500090114             deicpi7=potinfo(xx);
039600090114             deicpi7p=potinfop(xx);
039700090114           when deicpo8=0       ;
039800090114             deicpo8=potsede(xx) ;
039900090114             deicpi8=potinfo(xx);
040000090114             deicpi8p=potinfop(xx);
040100090114           when deicpo9=0       ;
040200090114             deicpo9=potsede(xx) ;
040300090114             deicpi9=potinfo(xx);
040400090114             deicpi9p=potinfop(xx);
040500090114           other   ;
040600090114             deicpoal='S'         ;
040700090114           endsl   ;
040800090114
040900090114           if prmela='T'      ;
041000090114           //Scrivo file DEID per vedere potenziali doppi
041100090114           exsr ScriviDEID     ;
041200090114           endif               ;
041300090114
041400090114           xx=xx+1   ;
041500090114           enddo              ;
041600090114           ENDSR   ;
041700071116       //-------------------------------------------------------------
041800081121       //?Cerca i potenziali sede associati  all'unificante
041900081121       //--------------------------------------------------------------
042000081119       BEGSR  CercaPotSede ;
042100081119
042200090114       // Prendo codice unificante e carico cod collegati
042300081119           clear tibs10ds ;
042400081119           d10tle='ST'   ;
042500081119           d10paf='F'    ;
042600081215           d10cod=deikscu;
042700081119           callp TIBS10R (tibs10ds)   ;
042800081119
042900081119  1        if d10err<>*blanks   ;
043000081119           // Se errore verifico se è figlio
043100081119           clear tibs10ds ;
043200081119           d10tle='ST'   ;
043300081119           d10paf='P'    ;
043400081215           d10cod=deikscu;
043500081119           callp TIBS10R (tibs10ds)   ;
043600081119
043700081119  2        if d10err<>*blanks   ;
043800081119           // Imposto nella skiera e come padre solo se stesso
043900081215           skc(1)='0000'+%editc(deikscu:'X');
044000081215           d10cop=deikscu                  ;
044100081119  2        endif                ;
044200081119  1        endif ;
044300081119
044400081119       clear potsede  ;
044500081119       clear potinfo  ;
044600081215       clear potinfop ;
044700081119       clear xx       ;
044800081119       yy=1    ;
044900081119
045000081119 1     dow skc(yy)>*Zeros  ;
045100081119
045200081119       // Verifico se cnaco ha un codice SEDE attaccato ;
045300081119       wksc=%int(%subst(skc(yy):5:7)) ;
045400081119       chain   (1:dutkci:wksc)  cnaco00f  ;
045500081119 2     if %found(cnaco00f)   ;
045600081119
045700081119         chain  acolib   tncpo01l   ;
045800081119
045900081119 3       if %found(tncpo01l) and cpofsf='S'  ;
046000081119         exsr MemPotSede     ;
046100081119 3       endif     ;
046200081119 2     endif     ;
046300081119
046400081119       yy=yy+1   ;
046500081119 1     enddo     ;
046600081119
046700081119       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
046800081119       //  codice unificante, altri potenziali  SEDE
046900081119       wksc=d10cop;
047000081119       chain   (1:dutkci:wksc)  cnclp00f  ;
047100081119 1     if %found(cnclp00f)   ;
047200081119
047300081119       // col sottoconti int fattura
047400081119       chain   (1:dutkci:clpscf)  cnind00f  ;
047500090113 2     if %found(cnind00f) and indsta=*blanks  ;
047600081119         // con la partita iva
047700090113 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
047800090113                             AND INDIVA<>'04507990150'        ;
047900081119         setll indiva   tncpo06l  ;
048000081119         reade indiva   tncpo06l  ;
048100081119
048200081119 3         dow not %eof(tncpo06l)   ;
048300081119 4         if cpofsf='S'          ;
048400081119              exsr MempotSede       ;
048500081119 4         endif                  ;
048600081119
048700081119           reade indiva   tncpo06l  ;
048800081119 3       enddo                    ;
048900081119 2a      endif    ;
049000081119
049100081119         // con il codice fiscale
049200081119 2a      if indcdf<>*blanks       ;
049300081119         setll indcdf   tncpo05l  ;
049400081119         reade indcdf   tncpo05l  ;
049500081119
049600081119 3         dow not %eof(tncpo05l)   ;
049700081119 4         if cpofsf='S'          ;
049800081119              exsr MempotSede      ;
049900081119 4         endif                  ;
050000081119
050100081119           reade indcdf   tncpo05l  ;
050200081119 3       enddo                    ;
050300081119 2a    endif                  ;
050400081119 2     endif                  ;
050500081119 1     endif                  ;
050600081119
050700080110       ENDSR  ;
050800081119       //--------------------------------------------------------------
050900081121       //?Memorizza potenziali sede per ogni unificante
051000081119       //--------------------------------------------------------------
051100081119       BEGSR MemPotSede;
051200081119         // Memorizzo i codici visualizzati per evitare ripetizioni
051300081119         Indx=%lookup(cpocpo:potsede)   ;
051400110414           if Indx=0 and xx<100 ;
051500081119           xx=xx+1  ;
051600081119           potsede(xx)=cpocpo   ;
051700081119           dcpo01=cporst  ;
051800081119           potinfo(xx)=§cpoifotot ;
051900081215
052000081215           // Se le info non sono complete, verifico se mancano solo mo
052100081215             if §cpoifotot <>'S'    ;
052200081215                    clear trmk50ds    ;
052300081215                    i50cpo=cpocpo     ;
052400081215                    i50mod='C'        ;
052500081215                    i50obl='S'        ;
052600081215                    callp TRMK50R (kpjba:trmk50ds)  ;
052700081215                     potinfo(xx)=o50ifotot  ;
052800081215                     potinfop(xx)=o50ifomin  ;
052900081215             endif;
053000081215           endif;
053100081119           ENDSR  ;
053200081121
053300081121       //--------------------------------------------------------------
053400081121       //?Memorizza potenziali sede per ogni unificante
053500081121       //--------------------------------------------------------------
053600081121       BEGSR ScriviDEID;
053700081121
053800081121       deicpo=potsede(xx)  ;
053900081121       write wfdeid00      ;
054000081121
054100081121       ENDSR  ;
054200081119
054300081119       //--------------------------------------------------------------
054400081119       //?Reperimento Dati del job (Utente/Operativi).
054500081119       //--------------------------------------------------------------
054600081119       BEGSR DatiJob;
054700081119
054800081119         in(E) §AzUte;
054900081119         if NOT %error;
055000081119           in(E) §DatiUte;
055100081119         endif;
055200081119         if %error or RSut = *blanks;
055300081119           clear TIBS34ds;
055400081119           tibs34r(tibs34ds);
055500081119           in §AzUte;
055600081119           in §DatiUte;
055700081119         endif;
055800081119
055900081119       ENDSR;
056000081119
056100071130
056200071116      /end-free
056300090114**
056400090121 DLYJOB DLY(010)
