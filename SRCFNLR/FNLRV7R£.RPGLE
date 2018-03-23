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
018100090909
018200080325       // Se non c'e' stampa  memorizzo dati di output della DS
018300090909
0184000803254      if wsta=' '     ;
018500080325       exsr   DatiOutput   ;
018600080325x4     else                ;
018700080403
018800080325       // Doppio giro: prima conto le ricorrenze: Se  > del minimo
018900080325       //   richiesto, secondo giro per la stampa    ;
019000080403
019100090909       exsr   ContaRIC   ;
019200080403
0193000803255      if Eseguista='N'        ;
019400080403         if ilrv7sip=' '       ;
0195000804036          if olrv7nric>=ilrv7minr ;
019600080403           leave     ;
0197000804036          endif  ;
019800080403         else   ;
019900080403
0200000804036          if olrv7nric>=ilrv7minr and olrv7part='S';
020100080403           leave     ;
0202000804036          endif  ;
020300080403         endif  ;
020400080403
020500080325x5     else     ;
020600080325       EXSR   Stampa ;
0207000803255      endif    ;
020800080325
0209000803254      endif  ;
0210000803253      endif  ;
0211000803252      endif  ;
021200080319
021300080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
0214000803251      enddo   ;
021500080319
021600080319       ENDSR   ;
021700080327       //-------------------------------------------------------------------
021800080325       BEGSR  Stampa          ;
021900080328
022000080328
022100080327       // a cambio segnacollo ricarico i dati
0222000906030      if        savnsc<>brvnsc or savlnp<>brvlnp  or
022300090603                 savlna<>brvlna or savnrs<>brvnrs   ;
022400090603       Aggtotale='S'   ;
022500090603
022600080331       totale=totale+1        ;
022700080325       clear   MancaBolla     ;
022800080331       Newcollo='S';
022900080325
023000080325       // Cerco la bolla
023100080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNBLT27L  ;
0232000803251      if   not %found(fnblt27l)    ;
023300080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNart27L  ;
0234000803252      if   not %found(fnart27l)    ;
023500080325       MancaBolla='S'               ;
0236000803252      endif                  ;
0237000803251      endif                  ;
023800080325
0239000803251      if  MancaBolla=' '     ;
024000080327       chain (artaas:artlnp:artnrs:artnsp)  FNblp01L  ;
0241000803252      if   not %found(fnblp01l)    ;
024200080327       chain (artaas:artlnp:artnrs:artnsp)  FNarb01L  ;
0243000803253      if   not %found(fnblp01l)    ;
024400080325       MancaBolla='S'               ;
0245000803253      endif  ;
0246000803252      endif  ;
0247000803251      endif  ;
024800080325
024900080325       // Manca bolla
0250000803251      if Mancabolla='S'      ;
025100080325       stplnp=brvlnp          ;
025200080325       stplna=brvlna          ;
025300080325       stpnrs=brvnrs          ;
025400080327       stpncd=brvnsc    ;
025500080325       clear tra01            ;
025600080325       clear stpnsp           ;
025700080327       clear stpDSP           ;
025800080325       clear stprsm           ;
025900080325       clear stplom           ;
026000080325       clear stprsd           ;
026100080325       clear stplod           ;
026200080325       clear stpnca           ;
026300080325       clear stpchs           ;
026400080327
026500080327       DatiBolla='S'          ;
026600080325 x1    else                   ;
026700080325       // rimemorizzo a cambio spedizione
026800080325
026900080325 2     if   savspe<>arbspe    ;
027000080325       stplnp=arblnp          ;
027100080325       stpnrs=arbnrs          ;
027200080327         if stpnrs>0            ;
027300080327         tra01='-'              ;
027400080327         else                   ;
027500080327         tra01=' '              ;
027600080327         endif                  ;
027700080325       stpnsp=arbnsp          ;
027800080327       dataiso=%date(arbdsp)    ;
027900080327       datadmy=dataiso        ;
028000080327       stpDSP=%dec(datadmy)   ;
028100080325       stplna=arblna          ;
028200080325       stprsm=arbrsm          ;
028300080325       stplom=arblom          ;
028400080325       %subst(stplom:10:3)=' '+arbprm    ;
028500080325
028600080325       stprsd=arbrsd          ;
028700080325       stplod=arblod          ;
028800080325       %subst(stplod:13:3)=' '+arbprd    ;
028900080325       stpncd=arbncd          ;
029000080325       stpnca=arbnca          ;
029100080325
029200080325       savspe=arbspe          ;
029300080327       DatiBolla='S'          ;
029400080325 2     endif                  ;
029500080327
029600080325 1     endif                  ;
029700080325
029800080325       stpnsc=brvnsc          ;
029900080327        // chi sono
030000080327       clear   stpchs ;
030100080327       chain  (brvlnp:brvlna:brvnrs:brvnsc:'C') fiars01l ;
030200080327 1     If %found(fiars01l)     ;
030300080327       stpchs=arsnot          ;
030400080327 1     endif                  ;
030500080327
030600080327       savnsc=brvnsc          ;
030700080331       savlnp=brvlnp          ;
030800090603       savlna=brvlna          ;
030900090603       savnrs=brvnrs          ;
031000080327 0     endif                  ;
031100080327
031200090603       // Se si tratta di un secondo livello
031300090603       //  stampo solo le mancanze dei suoi colli
031400090603       // per primo livello stampo tutto il terminal
031500090603 0     if dutlpo<>'2' or
031600090603          (dutlpo='2' and stplnp=dutpou )  or
031700090603          (dutlpo='2' and stplna=dutpou ) ;
031800090603
031900080327       // Dati spunta doppia
032000080327       clear   stpdop         ;
032100080327
032200080327       chain    brvfgs    azorg01l   ;
032300080327 1     if not %found(azorg01l)  ;
032400080327       orgde5='????????'        ;
032500080327 x1    else                     ;
032600080327 2       if orgde5=*blanks        ;
032700080327         orgde5=orgdes            ;
032800080327 2       endif                    ;
032900080327 1     endif                    ;
033000080327
033100080327       stpdop=%editc(brvfgs: 'X') +'-'+%subst(orgde5:1:8)+ ' in' ;
033200080327       // categoria spunta
033300080327       clear   descat         ;
033400080327 1     if    brvnpg=3         ;
033500080327       descat='IMx'           ;
033600080327       %subst(descat:3:1)=d53spg ;
033700080327 x1    else                   ;
033800080327       clear  tblkey          ;
033900080327       tblkey=%editc(brvnpg: 'X') ;
034000080327       chain   (1:'7N':tblkey) tabel00f  ;
034100080327 2     if %found(tabel00f)   ;
034200080327       descat=tbluni          ;
034300080327 2     endif                  ;
034400080327
034500080327 1     endif                  ;
034600080327
034700080327       stpdop=%trim(stpdop)+' ' +descat + ' ' +%editc(brvnpg: 'X')+'-'
034800080327              +%editc(brvnfv:'4')+' il' ;
034900080327
035000080327       dataiso=%date(d53dfv)         ;
035100080327       dataeur=dataiso        ;
035200080327       wdfv=%dec(dataeur)            ;
035300080327       wggmm=%subst(%editc(wdfv:'X'):1:2)+'/'+%subst(%editc(wdfv:'X'):3:2);
035400080327       stpdop=%trim(stpdop)+' ' +wggmm+' '+%editw(brvhms: '  :  :  ');
035500080327
035600080327       if brvnpg=4 or brvnpg=8   ;
035700080327         chain  (brvnpg:brvnfv:brvfgs) fnfvv01l;
035800080327         if %found(fnfvv01l)    ;
035900080327           stpdop=%trim(stpdop)+' ' +%subst(C_fvvdsf:1:7) ;
036000080327         endif  ;
036100080327       endif  ;
036200080325
036300080327       // Testata bolla
036400080327
036500080331 1     if  righe>60           ;
036600080327       write  LRV7PT1         ;
036700080327       DatiBolla='S'          ;
036800080331       clear NewCollo         ;
036900080328       righe=8                ;
037000080327 1     endif                  ;
037100080325
037200080331       // riga vuota tra un collo e l'altro
037300080331       if Newcollo='S'        ;
037400080331       write   LRV7PD3        ;
037500080331       righe=righe+1          ;
037600080331       clear  NewCollo        ;
037700080331       endif                  ;
037800080331
037900080327       // Dati bolla
038000080331 1     if DatiBolla='S'       ;
038100080327       write   LRV7PD1        ;
038200080327       clear  Datibolla       ;
038300080327 x1    else                   ;
038400080327       // solo dati collo doppio
038500080327       write   LRV7PD2        ;
038600080327 1     endif                  ;
038700080327
038800080327       righe=righe+1          ;
038900090603       else  ;
039000090603
039100090603       // Se non è collo di compentenza lo tolto dalla somma
039200090603 1     if Aggtotale='S'       ;
039300090603        totale=totale-1        ;
039400090603 1     endif                  ;
039500090603 0     endif                  ;
039600080327
039700090603       clear   Aggtotale      ;
039800080325       ENDSR                  ;
039900080327       //-------------------------------------------------------------------
040000080325       BEGSR  DatiOutput      ;
040100080325
040200090909       // conto numero ricorrenze doppi
040300090909       exsr   ContaRic    ;
040400080325
040500080325        // tengo spunta se data maggiore
040600080325        // Se uguale tengo la data ora immissione + alta
040700080325        // Se stessa data e ora immissione, tengo cat non arrivi
0408000803251      if    d53dfv>olrv7dfv  or
040900080325
041000080325            (d53dfv=olrv7dfv  and brvimm>wimm) or
041100080325
041200080325            (d53dfv=olrv7dfv and brvimm=wimm and brvnpg<>2)  ;
041300080325         olrv7dfv=d53dfv   ;
041400080325         olrv7npg=d53npg   ;
041500080325         olrv7nfv=d53nfv   ;
041600080325         olrv7fgs=d53fgs   ;
041700080325         wimm=brvimm   ;
0418000803251      endif  ;
041900080325
042000080325       ENDSR  ;
042100090909       //-------------------------------------------------------------------
042200090909       BEGSR ContaRIC   ;
042300090909
042400090909       // Numero ricorrenze doppie :
042500090909       //    di  IMA e PARTENZA ne conto una sola;
042600090909
042700090909       IF D53def='S'   ;
042800090909         wnpgpar=%editc(d53npg:'X')+'D'       ;
042900090909         else;
043000090909         wnpgpar=%editc(d53npg:'X')+d53spg    ;
043100090909       endif   ;
043200090909
043300090909       TROV=%lookup   (wnpgpar: NPGPAR)  ;
043400090909
043500090909       select   ;
043600090909       when trov>0  and olrv7part=' '    ;
043700090909         olrv7nric=olrv7nric+1  ;
043800090909         olrv7part='S'   ;
043900090909
044000090909       when trov=0 and  spuima=' ' ;
044100090909         olrv7nric=olrv7nric+1  ;
044200090909       when trov=0 and   d53npg<>3                  ;
044300090909         olrv7nric=olrv7nric+1  ;
044400090909       when trov=0 and   d53spg<>'A'   ;
044500090909       olrv7nric=olrv7nric+1  ;
044600090909       endsl   ;
044700090909
044800090909       // Memorizzo se ce ne è già una IMA
044900090909       if d53npg=3    and d53spg='A'  ;
045000090909       eval SPUIMA='S'   ;
045100090909       endif   ;
045200090909
045300090909       ENDSR   ;
045400080327       //-------------------------------------------------------------------
045500080320       BEGSR  ContrAmbito            ;
045600080327
045700080327       // Ambito partenza --> spunta cat 1 e 5
045800080327       if   ilrv7tbo='P' or ilrv7tbo=' '     ;
045900080327
046000080320       setll  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
046100080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
046200080320
0463000803271      dow   not %eof(fnfvv02l)   ;
046400080320       // Imposto la key per la chian per foglio
046500080320       kfgs=fvvfgs                ;
046600080320       knfv=fvvnfv                ;
046700080320       knpg=fvvnpg                ;
046800080320       exsr    contrFoglio        ;
046900080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0470000803271      enddo                      ;
047100080327
047200080327       // Leggo fogli partenza
047300080327       setll  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
047400080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
047500080327
0476000803271      dow   not %eof(fnfvv02l)   ;
047700080327       // Imposto la key per la chian per foglio
047800080327       kfgs=fgvlnp                ;
047900080327       knfv=fgvnfv                ;
048000080327       knpg=1                     ;
048100080327       exsr    contrFoglio        ;
048200080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
0483000803271      enddo                      ;
048400080327
048500080327       endif                      ;
048600080327
048700080327
048800080327       // Ambito ARRIVO   --> spunta cat 3 e 4
0489000803271      if   ilrv7tbo='A' or ilrv7tbo=' '     ;
049000080327
049100080327       conta   =1     ;
049200080327       kfnpg=3         ;
049300080327
0494000803272      dow conta<=2;
049500080327       setll  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
049600080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
049700080327
0498000803273      dow   not %eof(fnfvv02l)   ;
049900080327       // Imposto la key per la chian per foglio
050000080327       kfgs=fvvfgs                ;
050100080327       knfv=fvvnfv                ;
050200080327       knpg=fvvnpg                ;
050300080327       exsr    contrFoglio        ;
050400080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0505000803273      enddo                      ;
050600080327
050700080327       conta=conta+1   ;
050800080327       kfnpg=4          ;
0509000803272      enddo                      ;
051000080327
0511000803271      endif                      ;
051200080327       ENDSR                      ;
051300080327       //-------------------------------------------------------------------
051400080320       BEGSR  Contrfoglio            ;
051500080320       setll (knpg:knfv:kfgs) fnbrv01l  ;
051600080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
051700080320
051800080320       dow   not %eof(fnbrv01l)           ;
051900080320
052000080320       // Attivo il controllo solo se spunta doppia
052100080320       //  se spunta arrivi con anomalia il controllo si attiva
052200080320       //  per spunta tripla
052300080320
052400080320       if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
052500080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
052600080328
052700080328       //  Escludo pistole autogenerate cioè non scaricate
052800080328       if brvdcs>0          ;
052900090603
053000080328       kfls=brvlnp           ;
053100080320       klna=brvlna          ;
053200080320       knrs=brvnrs          ;
053300080320       knsc=brvnsc          ;
053400080325       exsr    ContrSpunta  ;
053500080320       endif   ;
053600080328       endif   ;
053700080320
053800080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
053900080320       enddo                            ;
054000080327       ENDSR                            ;
054100080328       //-------------------------------------------------------------------
054200080328       BEGSR  Preparasel      ;
054300081010
054400081010       // Apertura file di stampa
054500081010       open fnlrv7p               ;
054600081010
054700080328       // Imposto i parametri di stampa
054800080328
054900080328       if ilrv7tbo='P'               ;
055000080328       stptbo='PARTENZA'          ;
055100080328       ENDIF                      ;
055200080328       if   ilrv7tbo='A'               ;
055300080328       stptbo='ARRIVO  '          ;
055400080328       ENDIF                      ;
055500080328
055600080328       STPMINR=ILRV7MINR          ;
055700080403       if ilrv7sip='S'            ;
055800080403       stpdsip=csip                ;
055900080403       ENDIF                      ;
056000080403
056100080328       IF   ILRV7FIL>0            ;
056200080328       STPFIL =%char(ILRV7FIL)+'-'           ;
056300080328       CHAIN   ILRV7FIL   AZORG01L;
056400080328       if  %found(azorg01l)       ;
056500080328       stpdfil=orgdes             ;
056600080328       endif                      ;
056700080328       endif                      ;
056800080328
056900080328       if ilrv7nfv>0               ;
057000080328       stpnpg=ilrv7npg            ;
057100080328       stpnfv=ilrv7nfv            ;
057200080328       endif                      ;
057300080328
057400080328       if ilrv7dela>0               ;
057500080328       dataiso=%date(ilrv7dela)   ;
057600080328       dataeur=dataiso            ;
057700080328       stpdela=%dec(dataeur)      ;
057800080328       endif                      ;
057900080328
058000080328       ENDSR   ;
058100080327
058200080327       //-------------------------------------------------------------------
058300080327       BEGSR  *INZSR                 ;
058400080328
058500080328       in(E) *dtaara   ;
058600080328       if    %error or rsut  =*blanks  ;
058700080328       callp TIBS34R (TIBS34DS)        ;
058800080328       in    *dtaara   ;
058900080328       endif                           ;
059000080328
059100080328       // Carico le categorie con ambito partenza
059200080328       xx=1   ;
059300080328       setll   (1:'7N') tabel00f   ;
059400080328       reade   (1:'7N') tabel00f   ;
059500080328
059600080328       dow not %eof(tabel00f)      ;
059700080328       if tblflg=' '               ;
059800080328       ds7n=tbluni                 ;
059900080328
060000080328       if §7ntbo<>' '              ;
060100080328
060200080328          if §7ntbo='P'               ;
060300080328          npgpar(xx)=tblkey;
060400080328          xx=xx+1  ;
060500080328          endif  ;
060600080328       else   ;
060700080328
060800080328         if  §7ntb1='P'                         ;
060900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc1   ;
061000080328         xx=xx+1  ;
061100080328         endif                                  ;
061200080328         if  §7ntb2='P'                         ;
061300080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc2   ;
061400080328         xx=xx+1  ;
061500080328         endif                                  ;
061600080328         if  §7ntb3='P'                         ;
061700080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc3   ;
061800080328         xx=xx+1  ;
061900080328         endif                                  ;
062000080328         if  §7ntb4='P'                         ;
062100080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc4   ;
062200080328         xx=xx+1  ;
062300080328         endif                                  ;
062400080328         if  §7ntb5='P'                         ;
062500080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc5   ;
062600080328         xx=xx+1  ;
062700080328         endif                                  ;
062800080328         if  §7ntb6='P'                         ;
062900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc6   ;
063000080328         xx=xx+1  ;
063100080328         endif                                  ;
063200080328
063300080328       endif                       ;
063400080328       endif                       ;
063500080328
063600080328       reade   (1:'7N') tabel00f   ;
063700080328       enddo                       ;
063800080328
063900080328       ENDSR                           ;
064000080317      /end-free
