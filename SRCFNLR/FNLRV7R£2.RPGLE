000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200080319      * FNLRV7R *----------------------------------------------------------*
000300080319      *  Pgm di controllo spunte doppie cioè di colli sparati più volte
000400080327      *  nello stesso foglio (potenziale stessa etichetta su 2 colli diversi)
000500080319      *--------------------------------------------------------------------*
000600080317     Ffnbrv07l  IF   E           K DISK
000700080320     Ffnbrv01l  IF   E           K DISK    rename(fnbrv000:fnbrv001)
000800080327     Ffnfvv01l  IF   E           K DISK    prefix(C_)
000900080320     Ffnfvv02l  IF   E           K DISK    rename(fnfvv000:fnfvv002)
001000080327     Ffnfgv02l  IF   E           K DISK
001100080325     Ffnblp01l  IF   E           K DISK    prefix(arb:3)
001200080325     Ffnarb01l  IF   E           K DISK
001300080327     Ffiars01l  IF   E           K DISK
001400080325     Ffnblt27l  IF   E           K DISK    prefix(art:3)
001500080325     Ffnart27l  IF   E           K DISK
001600080327     Fazorg01l  IF   E           K DISK
001700080327     Ftabel00f  IF   E           K DISK
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
0097000803271      select                            ;
009800080328       //  Controllo di una spunta
0099000803271      when  ilrv7fls>0 and ilrv7nsc>0   ;
010000080320       kfls=ilrv7fls          ;
010100080320       klna=ilrv7lna          ;
010200080320       knrs=ilrv7nrs          ;
010300080320       knsc=ilrv7nsc          ;
010400080325       wsta=' '               ;
010500080319       EXSR  contrSpunta      ;
010600080320
010700080328       //  Lista per FOGLIO
0108000803271      when  ilrv7nfv>0       ;
010900080320       kfgs=ilrv7fil          ;
011000080320       knpg=ilrv7npg          ;
011100080320       knfv=ilrv7nfv          ;
011200080328       chain  (ilrv7npg:ilrv7nfv:ilrv7fil) fnfvv01l  ;
011300080328       if %found(fnfvv01l)    ;
011400080328       ilrv7dela=C_fvvdfv     ;
011500080328       endif                  ;
011600080328       exsr Preparasel        ;
011700080328
011800080325       wsta='S'               ;
011900080319       EXSR  contrFoglio      ;
012000080320
012100080328       //  Lista per AMBITO
0122000803271      other                  ;
012300080328       exsr Preparasel        ;
012400080325       wsta='S'               ;
012500080320       EXSR  contrAmbito      ;
012600080327       endsl                  ;
012700080319
012800080328       if   wsta='S'          ;
012900080410 1     if  righe>60           ;
013000080328       write  LRV7PT1         ;
013100080328       endif                  ;
013200080328
013300080328       write  LRV7Pfine       ;
013400080328       endif                  ;
013500080328
013600080317        *inlr ='1'   ;
013700080327       //-------------------------------------------------------------------
013800080319       BEGSR  ContrSpunta            ;
013900080325
014000080325       if  wsta=' '         ;
014100080325       exsr   LeggiSpunte   ;
014200081014       kpjbu=fnlrv7ds       ;
014300080325       else                 ;
014400080325
014500080325       EseguiSTA='N'                 ;
014600080325       exsr   LeggiSpunte   ;
014700080325
014800080403       if (olrv7nric>=ilrv7minr and ilrv7sip=' ') or
014900080403          (olrv7nric>=ilrv7minr and ilrv7sip='S' and olrv7part='S' ) ;
015000080325         EseguiSTA='S'                 ;
015100080325         exsr   LeggiSpunte   ;
015200080325       endif                ;
015300080325       endif                ;
015400080325
015500080325       ENDSR                ;
015600080327       //-------------------------------------------------------------------
015700080325       BEGSR  LeggiSpunte            ;
015800080325       clear  olrv7nric   ;
015900080403       clear  olrv7part   ;
016000080325       clear  olrv7dfv    ;
016100080325       clear  olrv7nfv    ;
016200080325       clear  olrv7npg    ;
016300080325       clear  olrv7fgs    ;
016400081103       clear  spuima      ;
016500080320       setll (kfls:klna:knrs:knsc) fnbrv07l  ;
016600080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
016700080319
0168000803251      dow   not %eof(fnbrv07l)  ;
016900080319
0170000803252      if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
017100080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
017200080319
017300080319       // Prendo la data del foglio e la memorizzo se la più alta   ;
017400081014       clear  fnlv53DS ;
017500080319       d53nfv=brvnfv   ;
017600080319       d53npg=brvnpg   ;
017700080319       d53fgs=brvfgs   ;
017800080319       callp FNLV53R  (FNLV53DS)  ;
017900080319
0180000803253      if    d53err=' '           ;
018100080325       // Se non c'e' stampa  memorizzo dati di output della DS
0182000803254      if wsta=' '     ;
018300080325       exsr   DatiOutput   ;
018400080325x4     else                ;
018500080403
018600080325       // Doppio giro: prima conto le ricorrenze: Se  > del minimo
018700080325       //   richiesto, secondo giro per la stampa    ;
018800080403
018900081103       if  spuima=' ' or  d53npg<>3 or d53spg<>'A'   ;
019000080327       olrv7nric=olrv7nric+1  ;
019100081103       endif   ;
019200081103
019300081103       // Memorizzo se ce ne è già una IMA
019400081103       if d53npg=3    and d53spg='A'  ;
019500081103       eval SPUIMA='S'   ;
019600081103       endif   ;
019700080403
0198000803255      if Eseguista='N'        ;
019900080403         if ilrv7sip=' '       ;
0200000804036          if olrv7nric>=ilrv7minr ;
020100080403           leave     ;
0202000804036          endif  ;
020300080403         else   ;
020400080403
020500080403           wnpgpar=%editc(d53npg:'X')+d53spg    ;
020600080403           TROV=%lookup   (wnpgpar: NPGPAR)  ;
020700080403           if trov>0       ;
020800080403           olrv7part='S'   ;
0209000804031          endif   ;
0210000804036          if olrv7nric>=ilrv7minr and olrv7part='S';
021100080403           leave     ;
0212000804036          endif  ;
021300080403         endif  ;
021400080403
021500080325x5     else     ;
021600080325       EXSR   Stampa ;
0217000803255      endif    ;
021800080325
0219000803254      endif  ;
0220000803253      endif  ;
0221000803252      endif  ;
022200080319
022300080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
0224000803251      enddo   ;
022500080319
022600080319       ENDSR   ;
022700080327       //-------------------------------------------------------------------
022800080325       BEGSR  Stampa          ;
022900080328
023000080328
023100080327       // a cambio segnacollo ricarico i dati
0232000906030      if        savnsc<>brvnsc or savlnp<>brvlnp  or
023300090603                 savlna<>brvlna or savnrs<>brvnrs   ;
023400090603       Aggtotale='S'   ;
023500090603
023600080331       totale=totale+1        ;
023700080325       clear   MancaBolla     ;
023800080331       Newcollo='S';
023900080325
024000080325       // Cerco la bolla
024100080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNBLT27L  ;
0242000803251      if   not %found(fnblt27l)    ;
024300080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNart27L  ;
0244000803252      if   not %found(fnart27l)    ;
024500080325       MancaBolla='S'               ;
0246000803252      endif                  ;
0247000803251      endif                  ;
024800080325
0249000803251      if  MancaBolla=' '     ;
025000080327       chain (artaas:artlnp:artnrs:artnsp)  FNblp01L  ;
0251000803252      if   not %found(fnblp01l)    ;
025200080327       chain (artaas:artlnp:artnrs:artnsp)  FNarb01L  ;
0253000803253      if   not %found(fnblp01l)    ;
025400080325       MancaBolla='S'               ;
0255000803253      endif  ;
0256000803252      endif  ;
0257000803251      endif  ;
025800080325
025900080325       // Manca bolla
0260000803251      if Mancabolla='S'      ;
026100080325       stplnp=brvlnp          ;
026200080325       stplna=brvlna          ;
026300080325       stpnrs=brvnrs          ;
026400080327       stpncd=brvnsc    ;
026500080325       clear tra01            ;
026600080325       clear stpnsp           ;
026700080327       clear stpDSP           ;
026800080325       clear stprsm           ;
026900080325       clear stplom           ;
027000080325       clear stprsd           ;
027100080325       clear stplod           ;
027200080325       clear stpnca           ;
027300080325       clear stpchs           ;
027400080327
027500080327       DatiBolla='S'          ;
027600080325 x1    else                   ;
027700080325       // rimemorizzo a cambio spedizione
027800080325
027900080325 2     if   savspe<>arbspe    ;
028000080325       stplnp=arblnp          ;
028100080325       stpnrs=arbnrs          ;
028200080327         if stpnrs>0            ;
028300080327         tra01='-'              ;
028400080327         else                   ;
028500080327         tra01=' '              ;
028600080327         endif                  ;
028700080325       stpnsp=arbnsp          ;
028800080327       dataiso=%date(arbdsp)    ;
028900080327       datadmy=dataiso        ;
029000080327       stpDSP=%dec(datadmy)   ;
029100080325       stplna=arblna          ;
029200080325       stprsm=arbrsm          ;
029300080325       stplom=arblom          ;
029400080325       %subst(stplom:10:3)=' '+arbprm    ;
029500080325
029600080325       stprsd=arbrsd          ;
029700080325       stplod=arblod          ;
029800080325       %subst(stplod:13:3)=' '+arbprd    ;
029900080325       stpncd=arbncd          ;
030000080325       stpnca=arbnca          ;
030100080325
030200080325       savspe=arbspe          ;
030300080327       DatiBolla='S'          ;
030400080325 2     endif                  ;
030500080327
030600080325 1     endif                  ;
030700080325
030800080325       stpnsc=brvnsc          ;
030900080327        // chi sono
031000080327       clear   stpchs ;
031100080327       chain  (brvlnp:brvlna:brvnrs:brvnsc:'C') fiars01l ;
031200080327 1     If %found(fiars01l)     ;
031300080327       stpchs=arsnot          ;
031400080327 1     endif                  ;
031500080327
031600080327       savnsc=brvnsc          ;
031700080331       savlnp=brvlnp          ;
031800090603       savlna=brvlna          ;
031900090603       savnrs=brvnrs          ;
032000080327 0     endif                  ;
032100080327
032200090603       // Se si tratta di un secondo livello
032300090603       //  stampo solo le mancanze dei suoi colli
032400090603       // per primo livello stampo tutto il terminal
032500090603 0     if dutlpo<>'2' or
032600090603          (dutlpo='2' and stplnp=dutpou )  or
032700090603          (dutlpo='2' and stplna=dutpou ) ;
032800090603
032900080327       // Dati spunta doppia
033000080327       clear   stpdop         ;
033100080327
033200080327       chain    brvfgs    azorg01l   ;
033300080327 1     if not %found(azorg01l)  ;
033400080327       orgde5='????????'        ;
033500080327 x1    else                     ;
033600080327 2       if orgde5=*blanks        ;
033700080327         orgde5=orgdes            ;
033800080327 2       endif                    ;
033900080327 1     endif                    ;
034000080327
034100080327       stpdop=%editc(brvfgs: 'X') +'-'+%subst(orgde5:1:8)+ ' in' ;
034200080327       // categoria spunta
034300080327       clear   descat         ;
034400080327 1     if    brvnpg=3         ;
034500080327       descat='IMx'           ;
034600080327       %subst(descat:3:1)=d53spg ;
034700080327 x1    else                   ;
034800080327       clear  tblkey          ;
034900080327       tblkey=%editc(brvnpg: 'X') ;
035000080327       chain   (1:'7N':tblkey) tabel00f  ;
035100080327 2     if %found(tabel00f)   ;
035200080327       descat=tbluni          ;
035300080327 2     endif                  ;
035400080327
035500080327 1     endif                  ;
035600080327
035700080327       stpdop=%trim(stpdop)+' ' +descat + ' ' +%editc(brvnpg: 'X')+'-'
035800080327              +%editc(brvnfv:'4')+' il' ;
035900080327
036000080327       dataiso=%date(d53dfv)         ;
036100080327       dataeur=dataiso        ;
036200080327       wdfv=%dec(dataeur)            ;
036300080327       wggmm=%subst(%editc(wdfv:'X'):1:2)+'/'+%subst(%editc(wdfv:'X'):3:2);
036400080327       stpdop=%trim(stpdop)+' ' +wggmm+' '+%editw(brvhms: '  :  :  ');
036500080327
036600080327       if brvnpg=4 or brvnpg=8   ;
036700080327         chain  (brvnpg:brvnfv:brvfgs) fnfvv01l;
036800080327         if %found(fnfvv01l)    ;
036900080327           stpdop=%trim(stpdop)+' ' +%subst(C_fvvdsf:1:7) ;
037000080327         endif  ;
037100080327       endif  ;
037200080325
037300080327       // Testata bolla
037400080327
037500080331 1     if  righe>60           ;
037600080327       write  LRV7PT1         ;
037700080327       DatiBolla='S'          ;
037800080331       clear NewCollo         ;
037900080328       righe=8                ;
038000080327 1     endif                  ;
038100080325
038200080331       // riga vuota tra un collo e l'altro
038300080331       if Newcollo='S'        ;
038400080331       write   LRV7PD3        ;
038500080331       righe=righe+1          ;
038600080331       clear  NewCollo        ;
038700080331       endif                  ;
038800080331
038900080327       // Dati bolla
039000080331 1     if DatiBolla='S'       ;
039100080327       write   LRV7PD1        ;
039200080327       clear  Datibolla       ;
039300080327 x1    else                   ;
039400080327       // solo dati collo doppio
039500080327       write   LRV7PD2        ;
039600080327 1     endif                  ;
039700080327
039800080327       righe=righe+1          ;
039900090603       else  ;
040000090603
040100090603       // Se non è collo di compentenza lo tolto dalla somma
040200090603 1     if Aggtotale='S'       ;
040300090603        totale=totale-1        ;
040400090603 1     endif                  ;
040500090603 0     endif                  ;
040600080327
040700090603       clear   Aggtotale      ;
040800080325       ENDSR                  ;
040900080327       //-------------------------------------------------------------------
041000080325       BEGSR  DatiOutput      ;
041100080325
041200081103       // Numero ricorrenze doppie : di ima ne conto una sola;
041300081103       if  spuima=' ' or  d53npg<>3 or d53spg<>'A'   ;
041400080325       olrv7nric=olrv7nric+1  ;
041500081103       endif   ;
041600081103
041700081103       // Memorizzo se ce ne è già una IMA
041800081103       if d53npg=3    and d53spg='A'  ;
041900081103       eval SPUIMA='S'   ;
042000081103       endif   ;
042100080325
042200080328       wnpgpar=%editc(d53npg:'X')+d53spg    ;
042300080328       TROV=%lookup   (wnpgpar: NPGPAR)  ;
042400080328       if trov>0       ;
042500080325       olrv7part='S'   ;
0426000803251      endif   ;
042700080325
042800080325        // tengo spunta se data maggiore
042900080325        // Se uguale tengo la data ora immissione + alta
043000080325        // Se stessa data e ora immissione, tengo cat non arrivi
0431000803251      if    d53dfv>olrv7dfv  or
043200080325
043300080325            (d53dfv=olrv7dfv  and brvimm>wimm) or
043400080325
043500080325            (d53dfv=olrv7dfv and brvimm=wimm and brvnpg<>2)  ;
043600080325         olrv7dfv=d53dfv   ;
043700080325         olrv7npg=d53npg   ;
043800080325         olrv7nfv=d53nfv   ;
043900080325         olrv7fgs=d53fgs   ;
044000080325         wimm=brvimm   ;
0441000803251      endif  ;
044200080325
044300080325       ENDSR  ;
044400080327       //-------------------------------------------------------------------
044500080320       BEGSR  ContrAmbito            ;
044600080327
044700080327       // Ambito partenza --> spunta cat 1 e 5
044800080327       if   ilrv7tbo='P' or ilrv7tbo=' '     ;
044900080327
045000080320       setll  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
045100080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
045200080320
0453000803271      dow   not %eof(fnfvv02l)   ;
045400080320       // Imposto la key per la chian per foglio
045500080320       kfgs=fvvfgs                ;
045600080320       knfv=fvvnfv                ;
045700080320       knpg=fvvnpg                ;
045800080320       exsr    contrFoglio        ;
045900080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0460000803271      enddo                      ;
046100080327
046200080327       // Leggo fogli partenza
046300080327       setll  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
046400080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
046500080327
0466000803271      dow   not %eof(fnfvv02l)   ;
046700080327       // Imposto la key per la chian per foglio
046800080327       kfgs=fgvlnp                ;
046900080327       knfv=fgvnfv                ;
047000080327       knpg=1                     ;
047100080327       exsr    contrFoglio        ;
047200080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
0473000803271      enddo                      ;
047400080327
047500080327       endif                      ;
047600080327
047700080327
047800080327       // Ambito ARRIVO   --> spunta cat 3 e 4
0479000803271      if   ilrv7tbo='A' or ilrv7tbo=' '     ;
048000080327
048100080327       conta   =1     ;
048200080327       kfnpg=3         ;
048300080327
0484000803272      dow conta<=2;
048500080327       setll  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
048600080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
048700080327
0488000803273      dow   not %eof(fnfvv02l)   ;
048900080327       // Imposto la key per la chian per foglio
049000080327       kfgs=fvvfgs                ;
049100080327       knfv=fvvnfv                ;
049200080327       knpg=fvvnpg                ;
049300080327       exsr    contrFoglio        ;
049400080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0495000803273      enddo                      ;
049600080327
049700080327       conta=conta+1   ;
049800080327       kfnpg=4          ;
0499000803272      enddo                      ;
050000080327
0501000803271      endif                      ;
050200080327       ENDSR                      ;
050300080327       //-------------------------------------------------------------------
050400080320       BEGSR  Contrfoglio            ;
050500080320       setll (knpg:knfv:kfgs) fnbrv01l  ;
050600080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
050700080320
050800080320       dow   not %eof(fnbrv01l)           ;
050900080320
051000080320       // Attivo il controllo solo se spunta doppia
051100080320       //  se spunta arrivi con anomalia il controllo si attiva
051200080320       //  per spunta tripla
051300080320
051400080320       if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
051500080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
051600080328
051700080328       //  Escludo pistole autogenerate cioè non scaricate
051800080328       if brvdcs>0          ;
051900090603
052000080328       kfls=brvlnp           ;
052100080320       klna=brvlna          ;
052200080320       knrs=brvnrs          ;
052300080320       knsc=brvnsc          ;
052400080325       exsr    ContrSpunta  ;
052500080320       endif   ;
052600080328       endif   ;
052700080320
052800080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
052900080320       enddo                            ;
053000080327       ENDSR                            ;
053100080328       //-------------------------------------------------------------------
053200080328       BEGSR  Preparasel      ;
053300081010
053400081010       // Apertura file di stampa
053500081010       open fnlrv7p               ;
053600081010
053700080328       // Imposto i parametri di stampa
053800080328
053900080328       if ilrv7tbo='P'               ;
054000080328       stptbo='PARTENZA'          ;
054100080328       ENDIF                      ;
054200080328       if   ilrv7tbo='A'               ;
054300080328       stptbo='ARRIVO  '          ;
054400080328       ENDIF                      ;
054500080328
054600080328       STPMINR=ILRV7MINR          ;
054700080403       if ilrv7sip='S'            ;
054800080403       stpdsip=csip                ;
054900080403       ENDIF                      ;
055000080403
055100080328       IF   ILRV7FIL>0            ;
055200080328       STPFIL =%char(ILRV7FIL)+'-'           ;
055300080328       CHAIN   ILRV7FIL   AZORG01L;
055400080328       if  %found(azorg01l)       ;
055500080328       stpdfil=orgdes             ;
055600080328       endif                      ;
055700080328       endif                      ;
055800080328
055900080328       if ilrv7nfv>0               ;
056000080328       stpnpg=ilrv7npg            ;
056100080328       stpnfv=ilrv7nfv            ;
056200080328       endif                      ;
056300080328
056400080328       if ilrv7dela>0               ;
056500080328       dataiso=%date(ilrv7dela)   ;
056600080328       dataeur=dataiso            ;
056700080328       stpdela=%dec(dataeur)      ;
056800080328       endif                      ;
056900080328
057000080328       ENDSR   ;
057100080327
057200080327       //-------------------------------------------------------------------
057300080327       BEGSR  *INZSR                 ;
057400080328
057500080328       in(E) *dtaara   ;
057600080328       if    %error or rsut  =*blanks  ;
057700080328       callp TIBS34R (TIBS34DS)        ;
057800080328       in    *dtaara   ;
057900080328       endif                           ;
058000080328
058100080328       // Carico le categorie con ambito partenza
058200080328       xx=1   ;
058300080328       setll   (1:'7N') tabel00f   ;
058400080328       reade   (1:'7N') tabel00f   ;
058500080328
058600080328       dow not %eof(tabel00f)      ;
058700080328       if tblflg=' '               ;
058800080328       ds7n=tbluni                 ;
058900080328
059000080328       if §7ntbo<>' '              ;
059100080328
059200080328          if §7ntbo='P'               ;
059300080328          npgpar(xx)=tblkey;
059400080328          xx=xx+1  ;
059500080328          endif  ;
059600080328       else   ;
059700080328
059800080328         if  §7ntb1='P'                         ;
059900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc1   ;
060000080328         xx=xx+1  ;
060100080328         endif                                  ;
060200080328         if  §7ntb2='P'                         ;
060300080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc2   ;
060400080328         xx=xx+1  ;
060500080328         endif                                  ;
060600080328         if  §7ntb3='P'                         ;
060700080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc3   ;
060800080328         xx=xx+1  ;
060900080328         endif                                  ;
061000080328         if  §7ntb4='P'                         ;
061100080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc4   ;
061200080328         xx=xx+1  ;
061300080328         endif                                  ;
061400080328         if  §7ntb5='P'                         ;
061500080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc5   ;
061600080328         xx=xx+1  ;
061700080328         endif                                  ;
061800080328         if  §7ntb6='P'                         ;
061900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc6   ;
062000080328         xx=xx+1  ;
062100080328         endif                                  ;
062200080328
062300080328       endif                       ;
062400080328       endif                       ;
062500080328
062600080328       reade   (1:'7N') tabel00f   ;
062700080328       enddo                       ;
062800080328
062900080328       ENDSR                           ;
063000080317      /end-free
