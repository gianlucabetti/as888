000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200080319      * FNLRV7R *----------------------------------------------------------*
000300080319      *  Pgm di controllo spunte doppie cioè di colli sparati più volte
000400080327      *  nello stesso foglio (potenziale stessa etichetta su 2 colli diversi)
000500080319      *--------------------------------------------------------------------*
000600080317     Ffnbrv07l  IF   E           K DISK
000700140430     Ffnbrv01l  IF   E           K DISK    rename(fnbrv000:fnbrv001) usropn
000800140430     Ffnfvv01l  IF   E           K DISK    prefix(C_) usropn
000900140430     Ffnfvv02l  IF   E           K DISK    rename(fnfvv000:fnfvv002) usropn
001000140430     Ffnfgv02l  IF   E           K DISK    usropn
001100140430     Ffnblp01l  IF   E           K DISK    prefix(arb:3) usropn
001200140430     Ffnarb01l  IF   E           K DISK    usropn
001300140430     Ffiars01l  IF   E           K DISK    usropn
001400140430     Ffnblt27l  IF   E           K DISK    prefix(art:3) usropn
001500140430     Ffnart27l  IF   E           K DISK    usropn
001600140429     Fazorg01l  IF   E           K DISK    usropn
001700140429     Ftabel00f  IF   E           K DISK    usropn
001800081010     FFNLRV7P   O    E             PRINTER usropn
001900050705      *
002000080317      * - Passaggio parametri
002100080325     d KPJBA         e ds
002200080325     d FNLRV7DS      e ds
002300080317     d FNLV53DS      e ds                  inz
002400080328     d
002500080328     d tibs34ds      e ds                  inz
002600080410     d tibs02ds      e ds                  inz
002700080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
002800080328     d DDatiUte      e ds                  dtaara(§datiute)
002900080317     d
003000080328     d ds7N          e ds
003100080328     d
003200080328     d NPGPAR          s              2    dim(10)
003300080328     d
003400080317     d BRVIMM          ds
003500080317     d brvdfs
003600080317     d brvhms
003700080327     d
003800080327
003900080327     d arbSPE          ds
004000080327     d arbaas
004100080327     d arbmgs
004200080327     d ARBDSP                  1      8  0
004300080327     d arblnp
004400080327     d arbnrs
004500080327     d arbnsp
004600080317     d
004700080317     d wimm            s                   like(brvimm)
004800080320     d kfgs            s                   like(fvvfgs)
004900080320     d knfv            s                   like(brvnfv)
005000080327     d knpg            s                   like(fvvnpg)
005100080327     d kfnpg           s                   like(fvvnpg)
005200080320     d kfls            s                   like(brvlnp)
005300080320     d klna            s                   like(brvlna)
005400080320     d knrs            s                   like(brvnrs)
005500080320     d knsc            s                   like(brvnsc)
005600080327     d savnsc          s                   like(brvnsc)
005700080331     d savlnp          s                   like(brvlnp)
005800090603     d savlna          s                   like(brvlna)
005900090603     d savnrs          s                   like(brvnrs)
006000080327     d savspe          s                   like(arbspe)
006100080328     d Wsta            s              1
006200081103     d SpuIMA          s              1
006300080327     d EseguiSTA       s              1
006400080331     d NewCollo        s              1
006500090603     d Aggtotale       s              1
006600080327     d DatiBolla       s              1
006700080327     d MancaBolla      s              1
006800080328     d Wnpgpar         s              2
006900080327     d DesCAT          s              3
007000080328     d XX              s              3  0
007100080331     d Totale          s              9  0
007200080328     d Trov            s              2  0
007300080328     d righe           s              3  0
007400080327     d Wdfv            s              8  0
007500080327     d conta           s              2  0
007600080327     d Wggmm           s              5
007700080327     d Dataiso         s               d   datfmt(*iso)
007800080327     d Dataeur         s               d   datfmt(*eur)
007900080327     d Datadmy         s               d   datfmt(*dmy)
008000080403     d Csip            c                   const('con almeno una spunta doppia -
008100080403     d                                     in partenza')
008200080317      *------------------------------------------------------------------------*
008300080317      *   P R O T O T I P I
008400080317      *------------------------------------------------------------------------*
008500081010      /copy gaitrasrc/srcprotopr,fnlv53r
008600081010      /copy gaitrasrc/srcprotopr,tibs34r
008700080317     D
008800001027     C*---------------------------------------------------------------*
008900001027     C     *ENTRY        PLIST
009000080325     C                   PARM                    kpjba
009100080317      /free
009200080325       fnlrv7ds=kpjbu     ;
009300080327       righe=90           ;
009400080317
009500080328       // Verifico se
009600080319
0097001404291      select                 ;
009800140429       // solo chiusura file  ;
009900140430       when  ILRV7TLA='C'     ;
010000140429       wsta=' '               ;
010100140429
010200140429       //  Controllo di una spunta  --> NON C'E' STAMPA!!
0103000803271      when  ilrv7fls>0 and ilrv7nsc>0   ;
010400080320       kfls=ilrv7fls          ;
010500080320       klna=ilrv7lna          ;
010600080320       knrs=ilrv7nrs          ;
010700080320       knsc=ilrv7nsc          ;
010800080325       wsta=' '               ;
010900080319       EXSR  contrSpunta      ;
011000080320
011100080328       //  Lista per FOGLIO
0112000803271      when  ilrv7nfv>0       ;
011300140430       EXSR OpenF             ;
011400080320       kfgs=ilrv7fil          ;
011500080320       knpg=ilrv7npg          ;
011600080320       knfv=ilrv7nfv          ;
011700080328       chain  (ilrv7npg:ilrv7nfv:ilrv7fil) fnfvv01l  ;
011800080328       if %found(fnfvv01l)    ;
011900080328       ilrv7dela=C_fvvdfv     ;
012000080328       endif                  ;
012100080328       exsr Preparasel        ;
012200080328
012300080325       wsta='S'               ;
012400080319       EXSR  contrFoglio      ;
012500080320
012600080328       //  Lista per AMBITO
0127000803271      other                  ;
012800140430       EXSR OpenF             ;
012900080328       exsr Preparasel        ;
013000080325       wsta='S'               ;
013100080320       EXSR  contrAmbito      ;
013200080327       endsl                  ;
013300080319
013400080328       if   wsta='S'          ;
013500080410 1     if  righe>60           ;
013600080328       write  LRV7PT1         ;
013700080328       endif                  ;
013800080328
013900080328       write  LRV7Pfine       ;
014000080328       endif                  ;
014100080328
014200140429        // quando elaboro per segnacollo posso tenere i file aperti
0143001404301      if    ilrv7fls>0 and ilrv7nsc>0 and ILRV7TLA= ' '  ;
014400140429        *inRT ='1'   ;
014500140429       else  ;
014600080317        *inlr ='1'   ;
014700140429        endif  ;
014800080327       //-------------------------------------------------------------------
014900080319       BEGSR  ContrSpunta            ;
015000080325
015100080325       if  wsta=' '         ;
015200080325       exsr   LeggiSpunte   ;
015300081014       kpjbu=fnlrv7ds       ;
015400080325       else                 ;
015500080325
015600080325       EseguiSTA='N'                 ;
015700080325       exsr   LeggiSpunte   ;
015800080325
015900080403       if (olrv7nric>=ilrv7minr and ilrv7sip=' ') or
016000080403          (olrv7nric>=ilrv7minr and ilrv7sip='S' and olrv7part='S' ) ;
016100080325         EseguiSTA='S'                 ;
016200080325         exsr   LeggiSpunte   ;
016300080325       endif                ;
016400080325       endif                ;
016500080325
016600080325       ENDSR                ;
016700080327       //-------------------------------------------------------------------
016800080325       BEGSR  LeggiSpunte            ;
016900080325       clear  olrv7nric   ;
017000080403       clear  olrv7part   ;
017100080325       clear  olrv7dfv    ;
017200080325       clear  olrv7nfv    ;
017300080325       clear  olrv7npg    ;
017400080325       clear  olrv7fgs    ;
017500081103       clear  spuima      ;
017600080320       setll (kfls:klna:knrs:knsc) fnbrv07l  ;
017700080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
017800080319
0179000803251      dow   not %eof(fnbrv07l)  ;
018000080319
0181000803252      if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
018200080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
018300080319
018400080319       // Prendo la data del foglio e la memorizzo se la più alta   ;
018500081014       clear  fnlv53DS ;
018600080319       d53nfv=brvnfv   ;
018700080319       d53npg=brvnpg   ;
018800080319       d53fgs=brvfgs   ;
018900080319       callp FNLV53R  (FNLV53DS)  ;
019000080319
0191000803253      if    d53err=' '           ;
019200090909
019300080325       // Se non c'e' stampa  memorizzo dati di output della DS
019400090909
0195000803254      if wsta=' '     ;
019600080325       exsr   DatiOutput   ;
019700080325x4     else                ;
019800080403
019900080325       // Doppio giro: prima conto le ricorrenze: Se  > del minimo
020000080325       //   richiesto, secondo giro per la stampa    ;
020100080403
020200090909       exsr   ContaRIC   ;
020300080403
0204000803255      if Eseguista='N'        ;
020500080403         if ilrv7sip=' '       ;
0206000804036          if olrv7nric>=ilrv7minr ;
020700080403           leave     ;
0208000804036          endif  ;
020900080403         else   ;
021000080403
0211000804036          if olrv7nric>=ilrv7minr and olrv7part='S';
021200080403           leave     ;
0213000804036          endif  ;
021400080403         endif  ;
021500080403
021600080325x5     else     ;
021700080325       EXSR   Stampa ;
0218000803255      endif    ;
021900080325
0220000803254      endif  ;
0221000803253      endif  ;
0222000803252      endif  ;
022300080319
022400080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
0225000803251      enddo   ;
022600080319
022700080319       ENDSR   ;
022800080327       //-------------------------------------------------------------------
022900140430       BEGSR  OpenF           ;
023000080328
023100140429       if  not %open(tabel00f)  ;
023200140429       open tabel00f ;
023300140429       endif  ;
023400140429       open azorg01l ;
023500140430       open fnfvv01l ;
023600140430       open fnfvv02l ;
023700140430       open fnfgv02l ;
023800140430       open fiars01l ;
023900140430       open fnart27l ;
024000140430       open fnblt27l ;
024100140430       open fnarb01l ;
024200140430       open fnblp01l ;
024300140430       open fnbrv01l ;
024400140430       ENDSR         ;
024500080328
024600140430       //-------------------------------------------------------------------
024700140430       BEGSR  Stampa          ;
024800080327       // a cambio segnacollo ricarico i dati
0249000906030      if        savnsc<>brvnsc or savlnp<>brvlnp  or
025000090603                 savlna<>brvlna or savnrs<>brvnrs   ;
025100090603       Aggtotale='S'   ;
025200090603
025300080331       totale=totale+1        ;
025400080325       clear   MancaBolla     ;
025500080331       Newcollo='S';
025600080325
025700080325       // Cerco la bolla
025800080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNBLT27L  ;
0259000803251      if   not %found(fnblt27l)    ;
026000080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNart27L  ;
0261000803252      if   not %found(fnart27l)    ;
026200080325       MancaBolla='S'               ;
0263000803252      endif                  ;
0264000803251      endif                  ;
026500080325
0266000803251      if  MancaBolla=' '     ;
026700080327       chain (artaas:artlnp:artnrs:artnsp)  FNblp01L  ;
0268000803252      if   not %found(fnblp01l)    ;
026900080327       chain (artaas:artlnp:artnrs:artnsp)  FNarb01L  ;
0270000803253      if   not %found(fnblp01l)    ;
027100080325       MancaBolla='S'               ;
0272000803253      endif  ;
0273000803252      endif  ;
0274000803251      endif  ;
027500080325
027600080325       // Manca bolla
0277000803251      if Mancabolla='S'      ;
027800080325       stplnp=brvlnp          ;
027900080325       stplna=brvlna          ;
028000080325       stpnrs=brvnrs          ;
028100080327       stpncd=brvnsc    ;
028200080325       clear tra01            ;
028300080325       clear stpnsp           ;
028400080327       clear stpDSP           ;
028500080325       clear stprsm           ;
028600080325       clear stplom           ;
028700080325       clear stprsd           ;
028800080325       clear stplod           ;
028900080325       clear stpnca           ;
029000080325       clear stpchs           ;
029100080327
029200080327       DatiBolla='S'          ;
029300080325 x1    else                   ;
029400080325       // rimemorizzo a cambio spedizione
029500080325
029600080325 2     if   savspe<>arbspe    ;
029700080325       stplnp=arblnp          ;
029800080325       stpnrs=arbnrs          ;
029900080327         if stpnrs>0            ;
030000080327         tra01='-'              ;
030100080327         else                   ;
030200080327         tra01=' '              ;
030300080327         endif                  ;
030400080325       stpnsp=arbnsp          ;
030500080327       dataiso=%date(arbdsp)    ;
030600080327       datadmy=dataiso        ;
030700080327       stpDSP=%dec(datadmy)   ;
030800080325       stplna=arblna          ;
030900080325       stprsm=arbrsm          ;
031000080325       stplom=arblom          ;
031100080325       %subst(stplom:10:3)=' '+arbprm    ;
031200080325
031300080325       stprsd=arbrsd          ;
031400080325       stplod=arblod          ;
031500080325       %subst(stplod:13:3)=' '+arbprd    ;
031600080325       stpncd=arbncd          ;
031700080325       stpnca=arbnca          ;
031800080325
031900080325       savspe=arbspe          ;
032000080327       DatiBolla='S'          ;
032100080325 2     endif                  ;
032200080327
032300080325 1     endif                  ;
032400080325
032500080325       stpnsc=brvnsc          ;
032600080327        // chi sono
032700080327       clear   stpchs ;
032800080327       chain  (brvlnp:brvlna:brvnrs:brvnsc:'C') fiars01l ;
032900080327 1     If %found(fiars01l)     ;
033000080327       stpchs=arsnot          ;
033100080327 1     endif                  ;
033200080327
033300080327       savnsc=brvnsc          ;
033400080331       savlnp=brvlnp          ;
033500090603       savlna=brvlna          ;
033600090603       savnrs=brvnrs          ;
033700080327 0     endif                  ;
033800080327
033900090603       // Se si tratta di un secondo livello
034000090603       //  stampo solo le mancanze dei suoi colli
034100090603       // per primo livello stampo tutto il terminal
034200090603 0     if dutlpo<>'2' or
034300090603          (dutlpo='2' and stplnp=dutpou )  or
034400090603          (dutlpo='2' and stplna=dutpou ) ;
034500090603
034600080327       // Dati spunta doppia
034700080327       clear   stpdop         ;
034800080327
034900080327       chain    brvfgs    azorg01l   ;
035000080327 1     if not %found(azorg01l)  ;
035100080327       orgde5='????????'        ;
035200080327 x1    else                     ;
035300080327 2       if orgde5=*blanks        ;
035400080327         orgde5=orgdes            ;
035500080327 2       endif                    ;
035600080327 1     endif                    ;
035700080327
035800080327       stpdop=%editc(brvfgs: 'X') +'-'+%subst(orgde5:1:8)+ ' in' ;
035900080327       // categoria spunta
036000080327       clear   descat         ;
036100080327 1     if    brvnpg=3         ;
036200080327       descat='IMx'           ;
036300080327       %subst(descat:3:1)=d53spg ;
036400080327 x1    else                   ;
036500080327       clear  tblkey          ;
036600080327       tblkey=%editc(brvnpg: 'X') ;
036700080327       chain   (1:'7N':tblkey) tabel00f  ;
036800080327 2     if %found(tabel00f)   ;
036900080327       descat=tbluni          ;
037000080327 2     endif                  ;
037100080327
037200080327 1     endif                  ;
037300080327
037400080327       stpdop=%trim(stpdop)+' ' +descat + ' ' +%editc(brvnpg: 'X')+'-'
037500080327              +%editc(brvnfv:'4')+' il' ;
037600080327
037700080327       dataiso=%date(d53dfv)         ;
037800080327       dataeur=dataiso        ;
037900080327       wdfv=%dec(dataeur)            ;
038000080327       wggmm=%subst(%editc(wdfv:'X'):1:2)+'/'+%subst(%editc(wdfv:'X'):3:2);
038100080327       stpdop=%trim(stpdop)+' ' +wggmm+' '+%editw(brvhms: '  :  :  ');
038200080327
038300080327       if brvnpg=4 or brvnpg=8   ;
038400080327         chain  (brvnpg:brvnfv:brvfgs) fnfvv01l;
038500080327         if %found(fnfvv01l)    ;
038600080327           stpdop=%trim(stpdop)+' ' +%subst(C_fvvdsf:1:7) ;
038700080327         endif  ;
038800080327       endif  ;
038900080325
039000080327       // Testata bolla
039100080327
039200080331 1     if  righe>60           ;
039300080327       write  LRV7PT1         ;
039400080327       DatiBolla='S'          ;
039500080331       clear NewCollo         ;
039600080328       righe=8                ;
039700080327 1     endif                  ;
039800080325
039900080331       // riga vuota tra un collo e l'altro
040000080331       if Newcollo='S'        ;
040100080331       write   LRV7PD3        ;
040200080331       righe=righe+1          ;
040300080331       clear  NewCollo        ;
040400080331       endif                  ;
040500080331
040600080327       // Dati bolla
040700080331 1     if DatiBolla='S'       ;
040800080327       write   LRV7PD1        ;
040900080327       clear  Datibolla       ;
041000080327 x1    else                   ;
041100080327       // solo dati collo doppio
041200080327       write   LRV7PD2        ;
041300080327 1     endif                  ;
041400080327
041500080327       righe=righe+1          ;
041600090603       else  ;
041700090603
041800090603       // Se non è collo di compentenza lo tolto dalla somma
041900090603 1     if Aggtotale='S'       ;
042000090603        totale=totale-1        ;
042100090603 1     endif                  ;
042200090603 0     endif                  ;
042300080327
042400090603       clear   Aggtotale      ;
042500080325       ENDSR                  ;
042600080327       //-------------------------------------------------------------------
042700080325       BEGSR  DatiOutput      ;
042800080325
042900090909       // conto numero ricorrenze doppi
043000090909       exsr   ContaRic    ;
043100080325
043200080325        // tengo spunta se data maggiore
043300080325        // Se uguale tengo la data ora immissione + alta
043400080325        // Se stessa data e ora immissione, tengo cat non arrivi
0435000803251      if    d53dfv>olrv7dfv  or
043600080325
043700080325            (d53dfv=olrv7dfv  and brvimm>wimm) or
043800080325
043900080325            (d53dfv=olrv7dfv and brvimm=wimm and brvnpg<>2)  ;
044000080325         olrv7dfv=d53dfv   ;
044100080325         olrv7npg=d53npg   ;
044200080325         olrv7nfv=d53nfv   ;
044300080325         olrv7fgs=d53fgs   ;
044400080325         wimm=brvimm   ;
0445000803251      endif  ;
044600080325
044700080325       ENDSR  ;
044800090909       //-------------------------------------------------------------------
044900090909       BEGSR ContaRIC   ;
045000090909
045100090909       // Numero ricorrenze doppie :
045200090909       //    di  IMA e PARTENZA ne conto una sola;
045300090909
045400090909       IF D53def='S'   ;
045500090909         wnpgpar=%editc(d53npg:'X')+'D'       ;
045600090909         else;
045700090909         wnpgpar=%editc(d53npg:'X')+d53spg    ;
045800090909       endif   ;
045900090909
046000090909       TROV=%lookup   (wnpgpar: NPGPAR)  ;
046100090909
046200090909       select   ;
046300090909       when trov>0  and olrv7part=' '    ;
046400090909         olrv7nric=olrv7nric+1  ;
046500090909         olrv7part='S'   ;
046600090909
046700090909       when trov=0 and  spuima=' ' ;
046800090909         olrv7nric=olrv7nric+1  ;
046900090909       when trov=0 and   d53npg<>3                  ;
047000090909         olrv7nric=olrv7nric+1  ;
047100090909       when trov=0 and   d53spg<>'A'   ;
047200090909       olrv7nric=olrv7nric+1  ;
047300090909       endsl   ;
047400090909
047500090909       // Memorizzo se ce ne è già una IMA
047600090909       if d53npg=3    and d53spg='A'  ;
047700090909       eval SPUIMA='S'   ;
047800090909       endif   ;
047900090909
048000090909       ENDSR   ;
048100080327       //-------------------------------------------------------------------
048200080320       BEGSR  ContrAmbito            ;
048300080327
048400080327       // Ambito partenza --> spunta cat 1 e 5
048500080327       if   ilrv7tbo='P' or ilrv7tbo=' '     ;
048600080327
048700080320       setll  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
048800080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
048900080320
0490000803271      dow   not %eof(fnfvv02l)   ;
049100080320       // Imposto la key per la chian per foglio
049200080320       kfgs=fvvfgs                ;
049300080320       knfv=fvvnfv                ;
049400080320       knpg=fvvnpg                ;
049500080320       exsr    contrFoglio        ;
049600080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0497000803271      enddo                      ;
049800080327
049900080327       // Leggo fogli partenza
050000080327       setll  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
050100080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
050200080327
0503000803271      dow   not %eof(fnfvv02l)   ;
050400080327       // Imposto la key per la chian per foglio
050500080327       kfgs=fgvlnp                ;
050600080327       knfv=fgvnfv                ;
050700080327       knpg=1                     ;
050800080327       exsr    contrFoglio        ;
050900080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
0510000803271      enddo                      ;
051100080327
051200080327       endif                      ;
051300080327
051400080327
051500080327       // Ambito ARRIVO   --> spunta cat 3 e 4
0516000803271      if   ilrv7tbo='A' or ilrv7tbo=' '     ;
051700080327
051800080327       conta   =1     ;
051900080327       kfnpg=3         ;
052000080327
0521000803272      dow conta<=2;
052200080327       setll  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
052300080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
052400080327
0525000803273      dow   not %eof(fnfvv02l)   ;
052600080327       // Imposto la key per la chian per foglio
052700080327       kfgs=fvvfgs                ;
052800080327       knfv=fvvnfv                ;
052900080327       knpg=fvvnpg                ;
053000080327       exsr    contrFoglio        ;
053100080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0532000803273      enddo                      ;
053300080327
053400080327       conta=conta+1   ;
053500080327       kfnpg=4          ;
0536000803272      enddo                      ;
053700080327
0538000803271      endif                      ;
053900080327       ENDSR                      ;
054000080327       //-------------------------------------------------------------------
054100080320       BEGSR  Contrfoglio            ;
054200080320       setll (knpg:knfv:kfgs) fnbrv01l  ;
054300080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
054400080320
054500080320       dow   not %eof(fnbrv01l)           ;
054600080320
054700080320       // Attivo il controllo solo se spunta doppia
054800080320       //  se spunta arrivi con anomalia il controllo si attiva
054900080320       //  per spunta tripla
055000080320
055100080320       if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
055200080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
055300080328
055400080328       //  Escludo pistole autogenerate cioè non scaricate
055500080328       if brvdcs>0          ;
055600090603
055700080328       kfls=brvlnp           ;
055800080320       klna=brvlna          ;
055900080320       knrs=brvnrs          ;
056000080320       knsc=brvnsc          ;
056100080325       exsr    ContrSpunta  ;
056200080320       endif   ;
056300080328       endif   ;
056400080320
056500080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
056600080320       enddo                            ;
056700080327       ENDSR                            ;
056800080328       //-------------------------------------------------------------------
056900080328       BEGSR  Preparasel      ;
057000081010
057100081010       // Apertura file di stampa
057200081010       open fnlrv7p               ;
057300081010
057400080328       // Imposto i parametri di stampa
057500080328
057600080328       if ilrv7tbo='P'               ;
057700080328       stptbo='PARTENZA'          ;
057800080328       ENDIF                      ;
057900080328       if   ilrv7tbo='A'               ;
058000080328       stptbo='ARRIVO  '          ;
058100080328       ENDIF                      ;
058200080328
058300080328       STPMINR=ILRV7MINR          ;
058400080403       if ilrv7sip='S'            ;
058500080403       stpdsip=csip                ;
058600080403       ENDIF                      ;
058700080403
058800080328       IF   ILRV7FIL>0            ;
058900080328       STPFIL =%char(ILRV7FIL)+'-'           ;
059000080328       CHAIN   ILRV7FIL   AZORG01L;
059100080328       if  %found(azorg01l)       ;
059200080328       stpdfil=orgdes             ;
059300080328       endif                      ;
059400080328       endif                      ;
059500080328
059600080328       if ilrv7nfv>0               ;
059700080328       stpnpg=ilrv7npg            ;
059800080328       stpnfv=ilrv7nfv            ;
059900080328       endif                      ;
060000080328
060100080328       if ilrv7dela>0               ;
060200080328       dataiso=%date(ilrv7dela)   ;
060300080328       dataeur=dataiso            ;
060400080328       stpdela=%dec(dataeur)      ;
060500080328       endif                      ;
060600080328
060700080328       ENDSR   ;
060800080327
060900080327       //-------------------------------------------------------------------
061000080327       BEGSR  *INZSR                 ;
061100080328
061200080328       in(E) *dtaara   ;
061300080328       if    %error or rsut  =*blanks  ;
061400080328       callp TIBS34R (TIBS34DS)        ;
061500080328       in    *dtaara   ;
061600080328       endif                           ;
061700080328
061800080328       // Carico le categorie con ambito partenza
061900140429       open tabel00f ;
062000080328       xx=1   ;
062100080328       setll   (1:'7N') tabel00f   ;
062200080328       reade   (1:'7N') tabel00f   ;
062300080328
062400080328       dow not %eof(tabel00f)      ;
062500080328       if tblflg=' '               ;
062600080328       ds7n=tbluni                 ;
062700080328
062800080328       if §7ntbo<>' '              ;
062900080328
063000080328          if §7ntbo='P'               ;
063100080328          npgpar(xx)=tblkey;
063200080328          xx=xx+1  ;
063300080328          endif  ;
063400080328       else   ;
063500080328
063600080328         if  §7ntb1='P'                         ;
063700080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc1   ;
063800080328         xx=xx+1  ;
063900080328         endif                                  ;
064000080328         if  §7ntb2='P'                         ;
064100080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc2   ;
064200080328         xx=xx+1  ;
064300080328         endif                                  ;
064400080328         if  §7ntb3='P'                         ;
064500080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc3   ;
064600080328         xx=xx+1  ;
064700080328         endif                                  ;
064800080328         if  §7ntb4='P'                         ;
064900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc4   ;
065000080328         xx=xx+1  ;
065100080328         endif                                  ;
065200080328         if  §7ntb5='P'                         ;
065300080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc5   ;
065400080328         xx=xx+1  ;
065500080328         endif                                  ;
065600080328         if  §7ntb6='P'                         ;
065700080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc6   ;
065800080328         xx=xx+1  ;
065900080328         endif                                  ;
066000080328
066100080328       endif                       ;
066200080328       endif                       ;
066300080328
066400080328       reade   (1:'7N') tabel00f   ;
066500080328       enddo                       ;
066600140429       close tabel00f ;
066700080328
066800080328       ENDSR                           ;
066900080317      /end-free
