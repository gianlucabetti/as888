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
005800080327     d savspe          s                   like(arbspe)
005900080328     d Wsta            s              1
006000081103     d SpuIMA          s              1
006100080327     d EseguiSTA       s              1
006200080331     d NewCollo        s              1
006300080327     d DatiBolla       s              1
006400080327     d MancaBolla      s              1
006500080328     d Wnpgpar         s              2
006600080327     d DesCAT          s              3
006700080328     d XX              s              3  0
006800080331     d Totale          s              9  0
006900080328     d Trov            s              2  0
007000080328     d righe           s              3  0
007100080327     d Wdfv            s              8  0
007200080327     d conta           s              2  0
007300080327     d Wggmm           s              5
007400080327     d Dataiso         s               d   datfmt(*iso)
007500080327     d Dataeur         s               d   datfmt(*eur)
007600080327     d Datadmy         s               d   datfmt(*dmy)
007700080403     d Csip            c                   const('con almeno una spunta doppia -
007800080403     d                                     in partenza')
007900080317      *------------------------------------------------------------------------*
008000080317      *   P R O T O T I P I
008100080317      *------------------------------------------------------------------------*
008200081010      /copy gaitrasrc/srcprotopr,fnlv53r
008300081010      /copy gaitrasrc/srcprotopr,tibs34r
008400080317     D
008500001027     C*---------------------------------------------------------------*
008600001027     C     *ENTRY        PLIST
008700080325     C                   PARM                    kpjba
008800080317      /free
008900080325       fnlrv7ds=kpjbu     ;
009000080327       righe=90           ;
009100080317
009200080328       // Verifico se
009300080319
0094000803271      select                            ;
009500080328       //  Controllo di una spunta
0096000803271      when  ilrv7fls>0 and ilrv7nsc>0   ;
009700080320       kfls=ilrv7fls          ;
009800080320       klna=ilrv7lna          ;
009900080320       knrs=ilrv7nrs          ;
010000080320       knsc=ilrv7nsc          ;
010100080325       wsta=' '               ;
010200080319       EXSR  contrSpunta      ;
010300080320
010400080328       //  Lista per FOGLIO
0105000803271      when  ilrv7nfv>0       ;
010600080320       kfgs=ilrv7fil          ;
010700080320       knpg=ilrv7npg          ;
010800080320       knfv=ilrv7nfv          ;
010900080328       chain  (ilrv7npg:ilrv7nfv:ilrv7fil) fnfvv01l  ;
011000080328       if %found(fnfvv01l)    ;
011100080328       ilrv7dela=C_fvvdfv     ;
011200080328       endif                  ;
011300080328       exsr Preparasel        ;
011400080328
011500080325       wsta='S'               ;
011600080319       EXSR  contrFoglio      ;
011700080320
011800080328       //  Lista per AMBITO
0119000803271      other                  ;
012000080328       exsr Preparasel        ;
012100080325       wsta='S'               ;
012200080320       EXSR  contrAmbito      ;
012300080327       endsl                  ;
012400080319
012500080328       if   wsta='S'          ;
012600080410 1     if  righe>60           ;
012700080328       write  LRV7PT1         ;
012800080328       endif                  ;
012900080328
013000080328       write  LRV7Pfine       ;
013100080328       endif                  ;
013200080328
013300080317        *inlr ='1'   ;
013400080327       //-------------------------------------------------------------------
013500080319       BEGSR  ContrSpunta            ;
013600080325
013700080325       if  wsta=' '         ;
013800080325       exsr   LeggiSpunte   ;
013900081014       kpjbu=fnlrv7ds       ;
014000080325       else                 ;
014100080325
014200080325       EseguiSTA='N'                 ;
014300080325       exsr   LeggiSpunte   ;
014400080325
014500080403       if (olrv7nric>=ilrv7minr and ilrv7sip=' ') or
014600080403          (olrv7nric>=ilrv7minr and ilrv7sip='S' and olrv7part='S' ) ;
014700080325         EseguiSTA='S'                 ;
014800080325         exsr   LeggiSpunte   ;
014900080325       endif                ;
015000080325       endif                ;
015100080325
015200080325       ENDSR                ;
015300080327       //-------------------------------------------------------------------
015400080325       BEGSR  LeggiSpunte            ;
015500080325       clear  olrv7nric   ;
015600080403       clear  olrv7part   ;
015700080325       clear  olrv7dfv    ;
015800080325       clear  olrv7nfv    ;
015900080325       clear  olrv7npg    ;
016000080325       clear  olrv7fgs    ;
016100081103       clear  spuima      ;
016200080320       setll (kfls:klna:knrs:knsc) fnbrv07l  ;
016300080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
016400080319
0165000803251      dow   not %eof(fnbrv07l)  ;
016600080319
0167000803252      if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
016800080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
016900080319
017000080319       // Prendo la data del foglio e la memorizzo se la più alta   ;
017100081014       clear  fnlv53DS ;
017200080319       d53nfv=brvnfv   ;
017300080319       d53npg=brvnpg   ;
017400080319       d53fgs=brvfgs   ;
017500080319       callp FNLV53R  (FNLV53DS)  ;
017600080319
0177000803253      if    d53err=' '           ;
017800080325       // Se non c'e' stampa  memorizzo dati di output della DS
0179000803254      if wsta=' '     ;
018000080325       exsr   DatiOutput   ;
018100080325x4     else                ;
018200080403
018300080325       // Doppio giro: prima conto le ricorrenze: Se  > del minimo
018400080325       //   richiesto, secondo giro per la stampa    ;
018500080403
018600081103       if  spuima=' ' or  d53npg<>3 or d53spg<>'A'   ;
018700080327       olrv7nric=olrv7nric+1  ;
018800081103       endif   ;
018900081103
019000081103       // Memorizzo se ce ne è già una IMA
019100081103       if d53npg=3    and d53spg='A'  ;
019200081103       eval SPUIMA='S'   ;
019300081103       endif   ;
019400080403
0195000803255      if Eseguista='N'        ;
019600080403         if ilrv7sip=' '       ;
0197000804036          if olrv7nric>=ilrv7minr ;
019800080403           leave     ;
0199000804036          endif  ;
020000080403         else   ;
020100080403
020200080403           wnpgpar=%editc(d53npg:'X')+d53spg    ;
020300080403           TROV=%lookup   (wnpgpar: NPGPAR)  ;
020400080403           if trov>0       ;
020500080403           olrv7part='S'   ;
0206000804031          endif   ;
0207000804036          if olrv7nric>=ilrv7minr and olrv7part='S';
020800080403           leave     ;
0209000804036          endif  ;
021000080403         endif  ;
021100080403
021200080325x5     else     ;
021300080325       EXSR   Stampa ;
0214000803255      endif    ;
021500080325
0216000803254      endif  ;
0217000803253      endif  ;
0218000803252      endif  ;
021900080319
022000080320       reade (kfls:klna:knrs:knsc) fnbrv07l  ;
0221000803251      enddo   ;
022200080319
022300080319       ENDSR   ;
022400080327       //-------------------------------------------------------------------
022500080325       BEGSR  Stampa          ;
022600080328
022700080328
022800080327       // a cambio segnacollo ricarico i dati
0229000803310      if        savnsc<>brvnsc or savlnp<>brvlnp  ;
023000080331       totale=totale+1        ;
023100080325       clear   MancaBolla     ;
023200080331       Newcollo='S';
023300080325
023400080325       // Cerco la bolla
023500080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNBLT27L  ;
0236000803251      if   not %found(fnblt27l)    ;
023700080325       chain (brvlnp:brvlna:brvnrs:brvnsc)  FNart27L  ;
0238000803252      if   not %found(fnart27l)    ;
023900080325       MancaBolla='S'               ;
0240000803252      endif                  ;
0241000803251      endif                  ;
024200080325
0243000803251      if  MancaBolla=' '     ;
024400080327       chain (artaas:artlnp:artnrs:artnsp)  FNblp01L  ;
0245000803252      if   not %found(fnblp01l)    ;
024600080327       chain (artaas:artlnp:artnrs:artnsp)  FNarb01L  ;
0247000803253      if   not %found(fnblp01l)    ;
024800080325       MancaBolla='S'               ;
0249000803253      endif  ;
0250000803252      endif  ;
0251000803251      endif  ;
025200080325
025300080325       // Manca bolla
0254000803251      if Mancabolla='S'      ;
025500080325       stplnp=brvlnp          ;
025600080325       stplna=brvlna          ;
025700080325       stpnrs=brvnrs          ;
025800080327       stpncd=brvnsc    ;
025900080325       clear tra01            ;
026000080325       clear stpnsp           ;
026100080327       clear stpDSP           ;
026200080325       clear stprsm           ;
026300080325       clear stplom           ;
026400080325       clear stprsd           ;
026500080325       clear stplod           ;
026600080325       clear stpnca           ;
026700080325       clear stpchs           ;
026800080327
026900080327       DatiBolla='S'          ;
027000080325 x1    else                   ;
027100080325       // rimemorizzo a cambio spedizione
027200080325
027300080325 2     if   savspe<>arbspe    ;
027400080325       stplnp=arblnp          ;
027500080325       stpnrs=arbnrs          ;
027600080327         if stpnrs>0            ;
027700080327         tra01='-'              ;
027800080327         else                   ;
027900080327         tra01=' '              ;
028000080327         endif                  ;
028100080325       stpnsp=arbnsp          ;
028200080327       dataiso=%date(arbdsp)    ;
028300080327       datadmy=dataiso        ;
028400080327       stpDSP=%dec(datadmy)   ;
028500080325       stplna=arblna          ;
028600080325       stprsm=arbrsm          ;
028700080325       stplom=arblom          ;
028800080325       %subst(stplom:10:3)=' '+arbprm    ;
028900080325
029000080325       stprsd=arbrsd          ;
029100080325       stplod=arblod          ;
029200080325       %subst(stplod:13:3)=' '+arbprd    ;
029300080325       stpncd=arbncd          ;
029400080325       stpnca=arbnca          ;
029500080325
029600080325       savspe=arbspe          ;
029700080327       DatiBolla='S'          ;
029800080325 2     endif                  ;
029900080327
030000080325 1     endif                  ;
030100080325
030200080325       stpnsc=brvnsc          ;
030300080327        // chi sono
030400080327       clear   stpchs ;
030500080327       chain  (brvlnp:brvlna:brvnrs:brvnsc:'C') fiars01l ;
030600080327 1     If %found(fiars01l)     ;
030700080327       stpchs=arsnot          ;
030800080327 1     endif                  ;
030900080327
031000080327       savnsc=brvnsc          ;
031100080331       savlnp=brvlnp          ;
031200080327 0     endif                  ;
031300080327
031400080327       // Dati spunta doppia
031500080327       clear   stpdop         ;
031600080327
031700080327       chain    brvfgs    azorg01l   ;
031800080327 1     if not %found(azorg01l)  ;
031900080327       orgde5='????????'        ;
032000080327 x1    else                     ;
032100080327 2       if orgde5=*blanks        ;
032200080327         orgde5=orgdes            ;
032300080327 2       endif                    ;
032400080327 1     endif                    ;
032500080327
032600080327       stpdop=%editc(brvfgs: 'X') +'-'+%subst(orgde5:1:8)+ ' in' ;
032700080327       // categoria spunta
032800080327       clear   descat         ;
032900080327 1     if    brvnpg=3         ;
033000080327       descat='IMx'           ;
033100080327       %subst(descat:3:1)=d53spg ;
033200080327 x1    else                   ;
033300080327       clear  tblkey          ;
033400080327       tblkey=%editc(brvnpg: 'X') ;
033500080327       chain   (1:'7N':tblkey) tabel00f  ;
033600080327 2     if %found(tabel00f)   ;
033700080327       descat=tbluni          ;
033800080327 2     endif                  ;
033900080327
034000080327 1     endif                  ;
034100080327
034200080327       stpdop=%trim(stpdop)+' ' +descat + ' ' +%editc(brvnpg: 'X')+'-'
034300080327              +%editc(brvnfv:'4')+' il' ;
034400080327
034500080327       dataiso=%date(d53dfv)         ;
034600080327       dataeur=dataiso        ;
034700080327       wdfv=%dec(dataeur)            ;
034800080327       wggmm=%subst(%editc(wdfv:'X'):1:2)+'/'+%subst(%editc(wdfv:'X'):3:2);
034900080327       stpdop=%trim(stpdop)+' ' +wggmm+' '+%editw(brvhms: '  :  :  ');
035000080327
035100080327       if brvnpg=4 or brvnpg=8   ;
035200080327         chain  (brvnpg:brvnfv:brvfgs) fnfvv01l;
035300080327         if %found(fnfvv01l)    ;
035400080327           stpdop=%trim(stpdop)+' ' +%subst(C_fvvdsf:1:7) ;
035500080327         endif  ;
035600080327       endif  ;
035700080325
035800080327       // Testata bolla
035900080327
036000080331 1     if  righe>60           ;
036100080327       write  LRV7PT1         ;
036200080327       DatiBolla='S'          ;
036300080331       clear NewCollo         ;
036400080328       righe=8                ;
036500080327 1     endif                  ;
036600080325
036700080331       // riga vuota tra un collo e l'altro
036800080331       if Newcollo='S'        ;
036900080331       write   LRV7PD3        ;
037000080331       righe=righe+1          ;
037100080331       clear  NewCollo        ;
037200080331       endif                  ;
037300080331
037400080327       // Dati bolla
037500080331 1     if DatiBolla='S'       ;
037600080327       write   LRV7PD1        ;
037700080327       clear  Datibolla       ;
037800080327 x1    else                   ;
037900080327       // solo dati collo doppio
038000080327       write   LRV7PD2        ;
038100080327 1     endif                  ;
038200080327
038300080327       righe=righe+1          ;
038400080327
038500080325       ENDSR                  ;
038600080327       //-------------------------------------------------------------------
038700080325       BEGSR  DatiOutput      ;
038800080325
038900081103       // Numero ricorrenze doppie : di ima ne conto una sola;
039000081103       if  spuima=' ' or  d53npg<>3 or d53spg<>'A'   ;
039100080325       olrv7nric=olrv7nric+1  ;
039200081103       endif   ;
039300081103
039400081103       // Memorizzo se ce ne è già una IMA
039500081103       if d53npg=3    and d53spg='A'  ;
039600081103       eval SPUIMA='S'   ;
039700081103       endif   ;
039800080325
039900080328       wnpgpar=%editc(d53npg:'X')+d53spg    ;
040000080328       TROV=%lookup   (wnpgpar: NPGPAR)  ;
040100080328       if trov>0       ;
040200080325       olrv7part='S'   ;
0403000803251      endif   ;
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
042100080327       //-------------------------------------------------------------------
042200080320       BEGSR  ContrAmbito            ;
042300080327
042400080327       // Ambito partenza --> spunta cat 1 e 5
042500080327       if   ilrv7tbo='P' or ilrv7tbo=' '     ;
042600080327
042700080320       setll  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
042800080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
042900080320
0430000803271      dow   not %eof(fnfvv02l)   ;
043100080320       // Imposto la key per la chian per foglio
043200080320       kfgs=fvvfgs                ;
043300080320       knfv=fvvnfv                ;
043400080320       knpg=fvvnpg                ;
043500080320       exsr    contrFoglio        ;
043600080320       reade  (5:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0437000803271      enddo                      ;
043800080327
043900080327       // Leggo fogli partenza
044000080327       setll  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
044100080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
044200080327
0443000803271      dow   not %eof(fnfvv02l)   ;
044400080327       // Imposto la key per la chian per foglio
044500080327       kfgs=fgvlnp                ;
044600080327       knfv=fgvnfv                ;
044700080327       knpg=1                     ;
044800080327       exsr    contrFoglio        ;
044900080327       reade  (ilrv7fil:ilrv7dela)   fnfgv02l   ;
0450000803271      enddo                      ;
045100080327
045200080327       endif                      ;
045300080327
045400080327
045500080327       // Ambito ARRIVO   --> spunta cat 3 e 4
0456000803271      if   ilrv7tbo='A' or ilrv7tbo=' '     ;
045700080327
045800080327       conta   =1     ;
045900080327       kfnpg=3         ;
046000080327
0461000803272      dow conta<=2;
046200080327       setll  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
046300080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
046400080327
0465000803273      dow   not %eof(fnfvv02l)   ;
046600080327       // Imposto la key per la chian per foglio
046700080327       kfgs=fvvfgs                ;
046800080327       knfv=fvvnfv                ;
046900080327       knpg=fvvnpg                ;
047000080327       exsr    contrFoglio        ;
047100080327       reade  (kfnpg:ilrv7dela:ilrv7fil)   fnfvv02l   ;
0472000803273      enddo                      ;
047300080327
047400080327       conta=conta+1   ;
047500080327       kfnpg=4          ;
0476000803272      enddo                      ;
047700080327
0478000803271      endif                      ;
047900080327       ENDSR                      ;
048000080327       //-------------------------------------------------------------------
048100080320       BEGSR  Contrfoglio            ;
048200080320       setll (knpg:knfv:kfgs) fnbrv01l  ;
048300080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
048400080320
048500080320       dow   not %eof(fnbrv01l)           ;
048600080320
048700080320       // Attivo il controllo solo se spunta doppia
048800080320       //  se spunta arrivi con anomalia il controllo si attiva
048900080320       //  per spunta tripla
049000080320
049100080320       if    (brvtsu<>' ' and (brvnpg<>2 or brvcan=' ')) or
049200080331             (brvtsu>'1'  and brvnpg=2  and brvcan<>' ')     ;
049300080328
049400080328       //  Escludo pistole autogenerate cioè non scaricate
049500080328       if brvdcs>0          ;
049600080328       kfls=brvlnp           ;
049700080320       klna=brvlna          ;
049800080320       knrs=brvnrs          ;
049900080320       knsc=brvnsc          ;
050000080325       exsr    ContrSpunta  ;
050100080320       endif   ;
050200080328       endif   ;
050300080320
050400080320       reade (knpg:knfv:kfgs) fnbrv01l  ;
050500080320       enddo                            ;
050600080327       ENDSR                            ;
050700080328       //-------------------------------------------------------------------
050800080328       BEGSR  Preparasel      ;
050900081010
051000081010       // Apertura file di stampa
051100081010       open fnlrv7p               ;
051200081010
051300080328       // Imposto i parametri di stampa
051400080328
051500080328       if ilrv7tbo='P'               ;
051600080328       stptbo='PARTENZA'          ;
051700080328       ENDIF                      ;
051800080328       if   ilrv7tbo='A'               ;
051900080328       stptbo='ARRIVO  '          ;
052000080328       ENDIF                      ;
052100080328
052200080328       STPMINR=ILRV7MINR          ;
052300080403       if ilrv7sip='S'            ;
052400080403       stpdsip=csip                ;
052500080403       ENDIF                      ;
052600080403
052700080328       IF   ILRV7FIL>0            ;
052800080328       STPFIL =%char(ILRV7FIL)+'-'           ;
052900080328       CHAIN   ILRV7FIL   AZORG01L;
053000080328       if  %found(azorg01l)       ;
053100080328       stpdfil=orgdes             ;
053200080328       endif                      ;
053300080328       endif                      ;
053400080328
053500080328       if ilrv7nfv>0               ;
053600080328       stpnpg=ilrv7npg            ;
053700080328       stpnfv=ilrv7nfv            ;
053800080328       endif                      ;
053900080328
054000080328       if ilrv7dela>0               ;
054100080328       dataiso=%date(ilrv7dela)   ;
054200080328       dataeur=dataiso            ;
054300080328       stpdela=%dec(dataeur)      ;
054400080328       endif                      ;
054500080328
054600080328       ENDSR   ;
054700080327
054800080327       //-------------------------------------------------------------------
054900080327       BEGSR  *INZSR                 ;
055000080328
055100080328       in(E) *dtaara   ;
055200080328       if    %error or rsut  =*blanks  ;
055300080328       callp TIBS34R (TIBS34DS)        ;
055400080328       in    *dtaara   ;
055500080328       endif                           ;
055600080328
055700080328       // Carico le categorie con ambito partenza
055800080328       xx=1   ;
055900080328       setll   (1:'7N') tabel00f   ;
056000080328       reade   (1:'7N') tabel00f   ;
056100080328
056200080328       dow not %eof(tabel00f)      ;
056300080328       if tblflg=' '               ;
056400080328       ds7n=tbluni                 ;
056500080328
056600080328       if §7ntbo<>' '              ;
056700080328
056800080328          if §7ntbo='P'               ;
056900080328          npgpar(xx)=tblkey;
057000080328          xx=xx+1  ;
057100080328          endif  ;
057200080328       else   ;
057300080328
057400080328         if  §7ntb1='P'                         ;
057500080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc1   ;
057600080328         xx=xx+1  ;
057700080328         endif                                  ;
057800080328         if  §7ntb2='P'                         ;
057900080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc2   ;
058000080328         xx=xx+1  ;
058100080328         endif                                  ;
058200080328         if  §7ntb3='P'                         ;
058300080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc3   ;
058400080328         xx=xx+1  ;
058500080328         endif                                  ;
058600080328         if  §7ntb4='P'                         ;
058700080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc4   ;
058800080328         xx=xx+1  ;
058900080328         endif                                  ;
059000080328         if  §7ntb5='P'                         ;
059100080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc5   ;
059200080328         xx=xx+1  ;
059300080328         endif                                  ;
059400080328         if  §7ntb6='P'                         ;
059500080328         npgpar(xx)=%subst(tblkey:1:1)+§7nsc6   ;
059600080328         xx=xx+1  ;
059700080328         endif                                  ;
059800080328
059900080328       endif                       ;
060000080328       endif                       ;
060100080328
060200080328       reade   (1:'7N') tabel00f   ;
060300080328       enddo                       ;
060400080328
060500080328       ENDSR                           ;
060600080317      /end-free
