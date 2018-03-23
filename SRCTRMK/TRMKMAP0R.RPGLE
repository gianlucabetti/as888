000100031205      *------------------------------------------------------------------------*
000200081119      * Controlli WADEL, TNCPO, TNCPI   per Mappatura
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600081215     fazorg01l  if   e           k Disk
000700130919     fAZCMM01L  if   e           k disk
000800081215     fTntbe01l  if   e           k Disk
000900080102     ftabel00f  if   e           k Disk
001000081215     fTNcpo01L  if   e           k Disk
001100081119     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
001200081119     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
001300081119     fCNACO00F  if   e           k Disk
001400081119     fCNIND00F  if   e           k Disk
001500081119     fCNCLP00F  if   e           k Disk
001600090210     FWADELpCT  iF   E             DISK    usropn extfile(CWADEL)
001700071129     F
001800090114     fwfdei01l  if   e           k Disk    rename(wfdei000:wfdei001)
001900090123     f                                     usropn extfile(CWFDEI1)
002000090123     FWFDEi05l  uF a E           k DISK    usropn extfile(CWFDEI)
002100090123     FWFDEid0F  uF a E             DISK    usropn
002200031205
002300031205      *------------------------------------------------------------------------*
002400031205      *   C A M P I   D I   L A V O R O
002500031205      *------------------------------------------------------------------------*
002600090114     d CMDLY           s             50    dim(1) ctdata perrcd(1)
002700090114
002800110414     d potsede         s             11  0 dim(101)
002900110414     d potinfo         s              1    dim(101)
003000110414     d potinfop        s              1    dim(101)
003100081119     D
003200090114     d comman          s            100
003300090114     d lung            s             15  5
003400090114     d kcod            s              7
003500081215     d Wfil            s              3  0
003600081215     d Wksc            s              7  0
003700081119     d xx              s              4  0
003800081119     d Indx            s              4  0
003900081119     d yy              s              4  0
004000081119     d kut             s              1  0 inz(1)
004100080102     d kkey            s                   like(tblkey)
004200071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
004300090114     D*
004400090114     D PARAM           DS
004500090114     D* CODICE FILIALE
004600090114     D  prmFIL                 1      3  0
004700090114     D* CODICE COMMERCIALE unificante (0 = TUTTI)
004800090114     D  prmagu                 4     10  0
004900090114     D  prmARE                11     13  0
005000090114     d*
005100090114     d* prmela='T'  --> rielabora da WADEL tutto
005200090114     D  prmela                14     14
005300090114     d*
005400090114     d* prmerr='E'  --> elaborazione non effettuabile
005500090114     d*                 per mancanza di dati
005600090114     D  prmerr                15     15
005700090123
005800090123     d CWFDEI          s             21    inz('GAITRAAZM /WFDEI05L')
005900090123     d CWFDEI1         s             21    inz('GAITRAAZM /WFDEI01L')
006000090210     d CWaDEL          s             21    inz('EDPMAPPA  /WADELPCT')
006100031205      *------------------------------------------------------------------------*
006200031205      *   D S   I N T E R N E / E S T E R N E
006300031205      *------------------------------------------------------------------------*
006400081215     d TRMK50ds      e ds                  inz
006500081119     d Dcpo01        e ds
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
018300130919              chain (CLPage) AZCMM000;
018400130919 3            if %found(AZCMM01L);
018500130919                wapauc=CMMuni;
018600130919                chain (WAPauc) AZCMM000;
018700130919 4              if %found(AZCMM01L);
018800130919                  wapaud = CMMdes;
018900130919 4              endif  ;
019000090213 3            endif  ;
019100090213 2          endif    ;
019200090213
019300090213         // Escludo distretto estero   agente unificante attuale
019400090114         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
019500090114         chain   wfil   azorg01l   ;
019600090213
019700090213             waparau=orgcar   ;
019800090213             clear   kkey  ;
019900090213             kkey=%editc(orgcar: 'X')    ;
020000090213             chain (1: '05': kkey) tabel00f ;
0201000902132            if %found(tabel00f) ;
020200090213               waparaud=tbluni  ;
0203000902132            endif  ;
020400090114
020500090114 2a      if orgfl3<>'7'    ;
020600090114
020700090114         kcod = %editc(wapcli: 'X' ) ;
020800090114         // Escludo codici indirizzo italia ma esteri
020900090114            if kcod<>'0894106' and kcod<>'0894274' ;
021000090114
021100090114         // Escludo 8888  e 9999
021200090114 2         if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
021300090114
021400090114         // Escludo senza fatturato  o senza spedizioni  anno corrente
021500090114 3          if   wapfac>0 and wapsac>0;
021600090114
021700090122 4          if wapblo=' ' ;
021800090114
021900090114             clear  wfdei000   ;
022000090114             clear  wfdeid00   ;
022100090114
022200090114             deikscu=wapcli ;
022300090114             deirag =wapcld ;
022400090114             deicmmu=wapauc ;
022500090114             deidcmm=wapaud ;
022600090114             deifil =wfil  ;
022700090114             deidfil=orgdes;
022800090114             deiare =waparau ;
022900090114             deidare=waparaud;
023000090114             deiblo =wapblo;
023100090114             deiclv =wapclv;
023200090126             select  ;
023300090126             when deiclv='T'   ;
023400090126             deiord='1'  ;
023500090126             when deiclv='A'   ;
023600090126             deiord='2'  ;
023700090126             when deiclv='B'   ;
023800090126             deiord='3'  ;
023900090126             when deiclv='C'   ;
024000090126             deiord='4'  ;
024100090126             when deiclv='D'   ;
024200090126             deiord='5'  ;
024300090126             other   ;
024400090126             deiord='6'  ;
024500090126             endsl    ;
024600090126
024700090114             deicap =wapcap;
024800090114             deiprv =wapprv;
024900090114             deinaz =wapnaz;
025000090114             deipiv =wapiva;
025100090114             deicdf =wapcdf;
025200090114             deidmerc=wapitc;
025300090114
025400090114
025500090114               chain (kut:dutkci:wapcli)  cnaco00f ;
025600090114 5              if %found(cnaco00f) ;
025700090114             deirag  =%triml(acorag)   ;
025800090121               deicmerc=%subst(%editc(acoitc:'Z'):3:5)          ;
025900090114
026000090213             clear kkey ;
026100090114             kkey=deicmerc               ;
026200090114             chain (1: '1L': kkey) tabel00f ;
026300090114 6           if %found(tabel00f) ;
026400090114              deidmerc=tbluni  ;
026500090114 6           endif  ;
026600090114 5           endif  ;
026700090114
026800090114             chain (kut:dutkci:wapcli)  cnind00f ;
026900090114 5           if %found(cnind00f) ;
027000090114             deicap =indcae;
027100090114             deiprv =indprv;
027200090114             deinaz =indsta;
027300090114             deipiv =indiva;
027400090114             deicdf =indcdf;
027500090114 5         endif    ;
027600090114
027700090114           EXSR ElaboraDati   ;
027800090114
027900090114
028000090114           write(e) wfdei000   ;
028100090114
028200090114 4         endif  ;
028300090114 4         endif  ;
028400090114 3         endif  ;
028500090114 2         endif  ;
028600090114 2a        endif  ;
028700090114
028800090126       read    wadelpct ;
028900090114 1     enddo ;
029000090114
029100090114       // Scrivo nel wfdei00f record di FINE elaborazione per
029200090114       //  non far creare la stampa se sta ricreando i dati
029300090114       clear wfdei000   ;
029400090114       deipiv='FINE'    ;
029500090114       write wfdei000   ;
029600090114
029700090114       ENDSR   ;
029800090114       //-------------------------------------------------------------
029900090114       //?Elabora da WFDEI00F
030000090114       //--------------------------------------------------------------
030100090114       BEGSR ElaWFDEI    ;
030200090114
030300090114       setll   (prmare:prmfil:prmagu)     wfdei05l  ;
030400090121       if prmagu=0   ;
030500090114       reade   (prmare:prmfil)            wfdei05l  ;
030600090114       else   ;
030700090114       reade   (prmare:prmfil:prmagu)     wfdei05l  ;
030800090114       endif   ;
030900090114
031000090114       dow   not %eof(wfdei05l)    ;
031100090114       clear deicpoal  ;
031200090114       clear deicpo1   ;
031300090114       clear deicpi1   ;
031400090114       clear deicpi1p  ;
031500090114       clear deicpo2   ;
031600090114       clear deicpi2   ;
031700090114       clear deicpi2p  ;
031800090114       clear deicpo3   ;
031900090114       clear deicpi3   ;
032000090114       clear deicpi3p  ;
032100090114       clear deicpo4   ;
032200090114       clear deicpi4   ;
032300090114       clear deicpi4p  ;
032400090114       clear deicpo5   ;
032500090114       clear deicpi5   ;
032600090114       clear deicpi5p  ;
032700090114       clear deicpo6   ;
032800090114       clear deicpi6   ;
032900090114       clear deicpi6p  ;
033000090114       clear deicpo7   ;
033100090114       clear deicpi7   ;
033200090114       clear deicpi7p  ;
033300090114       clear deicpo8   ;
033400090114       clear deicpi8   ;
033500090114       clear deicpi8p  ;
033600090114       clear deicpo9   ;
033700090114       clear deicpi9   ;
033800090114       clear deicpi9p  ;
033900090114
034000090114       exsr Elaboradati   ;
034100090114
034200090114       update wfdei000  ;
034300090114
034400090114
034500090121       if prmagu=0   ;
034600090114       reade   (prmare:prmfil)             wfdei05l  ;
034700090114       else   ;
034800090114       reade   (prmare:prmfil:prmagu)     wfdei05l  ;
034900090114       endif   ;
035000090114
035100090114       enddo   ;
035200090114
035300090114       ENDSR             ;
035400090114       //-------------------------------------------------------------
035500090114       //?Elaborazione dati potenziali per riempire WFDEI
035600090114       //--------------------------------------------------------------
035700090114           BEGSR ElaboraDati   ;
035800090114
035900090114           EXSR CercaPotSede   ;
036000090114           xx=1   ;
036100090114           dow potsede(xx)>0  ;
036200090114
036300090114           select  ;
036400090114           when deicpo1=0       ;
036500090114             deicpo1=potsede(xx) ;
036600090114             deicpi1=potinfo(xx);
036700090114             deicpi1p=potinfop(xx);
036800090114           when deicpo2=0       ;
036900090114             deicpo2=potsede(xx) ;
037000090114             deicpi2=potinfo(xx);
037100090114             deicpi2p=potinfop(xx);
037200090114           when deicpo3=0       ;
037300090114             deicpo3=potsede(xx) ;
037400090114             deicpi3=potinfo(xx);
037500090114             deicpi3p=potinfop(xx);
037600090114           when deicpo4=0       ;
037700090114             deicpo4=potsede(xx) ;
037800090114             deicpi4=potinfo(xx);
037900090114             deicpi4p=potinfop(xx);
038000090114           when deicpo5=0       ;
038100090114             deicpo5=potsede(xx) ;
038200090114             deicpi5=potinfo(xx);
038300090114             deicpi5p=potinfop(xx);
038400090114           when deicpo6=0       ;
038500090114             deicpo6=potsede(xx) ;
038600090114             deicpi6=potinfo(xx);
038700090114             deicpi6p=potinfop(xx);
038800090114           when deicpo7=0       ;
038900090114             deicpo7=potsede(xx) ;
039000090114             deicpi7=potinfo(xx);
039100090114             deicpi7p=potinfop(xx);
039200090114           when deicpo8=0       ;
039300090114             deicpo8=potsede(xx) ;
039400090114             deicpi8=potinfo(xx);
039500090114             deicpi8p=potinfop(xx);
039600090114           when deicpo9=0       ;
039700090114             deicpo9=potsede(xx) ;
039800090114             deicpi9=potinfo(xx);
039900090114             deicpi9p=potinfop(xx);
040000090114           other   ;
040100090114             deicpoal='S'         ;
040200090114           endsl   ;
040300090114
040400090114           if prmela='T'      ;
040500090114           //Scrivo file DEID per vedere potenziali doppi
040600090114           exsr ScriviDEID     ;
040700090114           endif               ;
040800090114
040900090114           xx=xx+1   ;
041000090114           enddo              ;
041100090114           ENDSR   ;
041200071116       //-------------------------------------------------------------
041300081121       //?Cerca i potenziali sede associati  all'unificante
041400081121       //--------------------------------------------------------------
041500081119       BEGSR  CercaPotSede ;
041600081119
041700090114       // Prendo codice unificante e carico cod collegati
041800081119           clear tibs10ds ;
041900081119           d10tle='ST'   ;
042000081119           d10paf='F'    ;
042100081215           d10cod=deikscu;
042200081119           callp TIBS10R (tibs10ds)   ;
042300081119
042400081119  1        if d10err<>*blanks   ;
042500081119           // Se errore verifico se è figlio
042600081119           clear tibs10ds ;
042700081119           d10tle='ST'   ;
042800081119           d10paf='P'    ;
042900081215           d10cod=deikscu;
043000081119           callp TIBS10R (tibs10ds)   ;
043100081119
043200081119  2        if d10err<>*blanks   ;
043300081119           // Imposto nella skiera e come padre solo se stesso
043400081215           skc(1)='0000'+%editc(deikscu:'X');
043500081215           d10cop=deikscu                  ;
043600081119  2        endif                ;
043700081119  1        endif ;
043800081119
043900081119       clear potsede  ;
044000081119       clear potinfo  ;
044100081215       clear potinfop ;
044200081119       clear xx       ;
044300081119       yy=1    ;
044400081119
044500081119 1     dow skc(yy)>*Zeros  ;
044600081119
044700081119       // Verifico se cnaco ha un codice SEDE attaccato ;
044800081119       wksc=%int(%subst(skc(yy):5:7)) ;
044900081119       chain   (1:dutkci:wksc)  cnaco00f  ;
045000081119 2     if %found(cnaco00f)   ;
045100081119
045200081119         chain  acolib   tncpo01l   ;
045300081119
045400081119 3       if %found(tncpo01l) and cpofsf='S'  ;
045500081119         exsr MemPotSede     ;
045600081119 3       endif     ;
045700081119 2     endif     ;
045800081119
045900081119       yy=yy+1   ;
046000081119 1     enddo     ;
046100081119
046200081119       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
046300081119       //  codice unificante, altri potenziali  SEDE
046400081119       wksc=d10cop;
046500081119       chain   (1:dutkci:wksc)  cnclp00f  ;
046600081119 1     if %found(cnclp00f)   ;
046700081119
046800081119       // col sottoconti int fattura
046900081119       chain   (1:dutkci:clpscf)  cnind00f  ;
047000090113 2     if %found(cnind00f) and indsta=*blanks  ;
047100081119         // con la partita iva
047200090113 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
047300090113                             AND INDIVA<>'04507990150'        ;
047400081119         setll indiva   tncpo06l  ;
047500081119         reade indiva   tncpo06l  ;
047600081119
047700081119 3         dow not %eof(tncpo06l)   ;
047800081119 4         if cpofsf='S'          ;
047900081119              exsr MempotSede       ;
048000081119 4         endif                  ;
048100081119
048200081119           reade indiva   tncpo06l  ;
048300081119 3       enddo                    ;
048400081119 2a      endif    ;
048500081119
048600081119         // con il codice fiscale
048700081119 2a      if indcdf<>*blanks       ;
048800081119         setll indcdf   tncpo05l  ;
048900081119         reade indcdf   tncpo05l  ;
049000081119
049100081119 3         dow not %eof(tncpo05l)   ;
049200081119 4         if cpofsf='S'          ;
049300081119              exsr MempotSede      ;
049400081119 4         endif                  ;
049500081119
049600081119           reade indcdf   tncpo05l  ;
049700081119 3       enddo                    ;
049800081119 2a    endif                  ;
049900081119 2     endif                  ;
050000081119 1     endif                  ;
050100081119
050200080110       ENDSR  ;
050300081119       //--------------------------------------------------------------
050400081121       //?Memorizza potenziali sede per ogni unificante
050500081119       //--------------------------------------------------------------
050600081119       BEGSR MemPotSede;
050700081119         // Memorizzo i codici visualizzati per evitare ripetizioni
050800081119         Indx=%lookup(cpocpo:potsede)   ;
050900110414           if Indx=0 and xx<100 ;
051000081119           xx=xx+1  ;
051100081119           potsede(xx)=cpocpo   ;
051200081119           dcpo01=cporst  ;
051300081119           potinfo(xx)=§cpoifotot ;
051400081215
051500081215           // Se le info non sono complete, verifico se mancano solo mo
051600081215             if §cpoifotot <>'S'    ;
051700081215                    clear trmk50ds    ;
051800081215                    i50cpo=cpocpo     ;
051900081215                    i50mod='C'        ;
052000081215                    i50obl='S'        ;
052100081215                    callp TRMK50R (kpjba:trmk50ds)  ;
052200081215                     potinfo(xx)=o50ifotot  ;
052300081215                     potinfop(xx)=o50ifomin  ;
052400081215             endif;
052500081215           endif;
052600081119           ENDSR  ;
052700081121
052800081121       //--------------------------------------------------------------
052900081121       //?Memorizza potenziali sede per ogni unificante
053000081121       //--------------------------------------------------------------
053100081121       BEGSR ScriviDEID;
053200081121
053300081121       deicpo=potsede(xx)  ;
053400081121       write wfdeid00      ;
053500081121
053600081121       ENDSR  ;
053700081119
053800081119       //--------------------------------------------------------------
053900081119       //?Reperimento Dati del job (Utente/Operativi).
054000081119       //--------------------------------------------------------------
054100081119       BEGSR DatiJob;
054200081119
054300081119         in(E) §AzUte;
054400081119         if NOT %error;
054500081119           in(E) §DatiUte;
054600081119         endif;
054700081119         if %error or RSut = *blanks;
054800081119           clear TIBS34ds;
054900081119           tibs34r(tibs34ds);
055000081119           in §AzUte;
055100081119           in §DatiUte;
055200081119         endif;
055300081119
055400081119       ENDSR;
055500081119
055600071130
055700071116      /end-free
055800090114**
055900090121 DLYJOB DLY(010)
