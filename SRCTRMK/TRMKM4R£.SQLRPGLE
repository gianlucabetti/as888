000100031205      *------------------------------------------------------------------------*
000200090202      * Creazione file per mappatura commerciale
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090205     FWfDEMd0f  uF   E             DISK    rename(wadelpc0:wfdemd00)
000700090205     FWfDEMd1l  iF   E           k DISK    rename(wadelpc0:wfdemd01)
000800090205     f                                     prefix(d_)
000900090205     FWfDEMd2l  iF   E           k DISK    rename(wadelpc0:wfdemd02)
001000090205     f                                     prefix(d_)
001100090205     FWfDEMd3l  uF a E           k DISK    rename(wadelpc0:wfdemd03)
001200090204     f                                     prefix(DEM:3)
001300090205     FWfDEMd4L  uF   E           k DISK    rename(wadelpc0:wfdemd04)
001400090205     f                                     prefix(DEM:3)
001500090205     FWfDEMe0f  uF   E             DISK    rename(wadelpc0:wfdeme00)
001600090205     FWfDEMe3l  uF a E           k DISK    rename(wadelpc0:wfdeme03)
001700090204     f                                     prefix(DEM:3)
001800090205     FWfDEME4l  uF   E           k DISK    rename(wadelpc0:wfdeme04)
001900090205     f                                     prefix(DEM:3)
002000090205     FWfDEMi0f  uF   E             DISK    rename(wadelpc0:wfdemi00)
002100090205     FWfDEMi3l  uF a E           k DISK    rename(wadelpc0:wfdemi03)
002200090204     f                                     prefix(DEM:3)
002300090205     FWfDEMI4L  uF   E           k DISK    rename(wadelpc0:wfdemI04)
002400090205     f                                     prefix(DEM:3)
002500090204     FWADELpCT  iF   E             DISK
002600090204     FWADELpCE  iF   E             DISK    rename(wadelpc0:wadelpe0)
002700090204     FWADELpCF  iF   E             DISK    rename(wadelpc0:wfdelpf0)
002800090205     fazorg01l  if   e           k Disk
002900090204     f
003000031205      *------------------------------------------------------------------------*
003100031205      *   C A M P I   D I   L A V O R O
003200031205      *------------------------------------------------------------------------*
003300090205     d Kcod            s              7
003400090205     d Wfil            s              3  0
003500090204     d kpiv            s             16
003600090204     d kcdf            s             16
003700090204     d kcli            s                   like(wapcli)
003800090205     d kioe            s                   like(wapioe)
003900090204     d wrrw            s             17  5
004000090204     d wrpw            s             17  5
004100090204     d wdpw            s              4  1
004200090205     d newcli          s              7  0
004300090205     d oldcli          s              7  0
004400090205     d wtrov           s              1
004500031205
004600031205      *------------------------------------------------------------------------*
004700031205      *   D S   I N T E R N E / E S T E R N E
004800031205      *------------------------------------------------------------------------*
004900031205     d Kpjba         e ds
005000071122     d
005100090203      // - Reperimento dati utente
005200090203     d TIBS34ds      e ds
005300090203      // - Dati utente
005400090203     d §AzUte        e ds                  extname(AZUTE00F)
005500090203     d                                     dtaara
005600090203     d §DatiUte      e ds                  extname(dDatiUte)
005700090203     d                                     dtaara
005800090204
005900090204      * -
006000090204     d WADELPCds     e ds                  inz  extname(WADELPCF)
006100090204     d Wfdemd0ds     e ds                  inz  extname(Wfdemd0F)
006200090204     d                                          prefix(DEM:3)
006300090204     d
006400090203     D*
006500071122      *------------------------------------------------------------------------*
006600071122      *   P R O T O T I P I
006700071122      *------------------------------------------------------------------------*
006800090203      /copy gaitrasrc/srcprotopr,tibs34r
006900031205      *------------------------------------------------------------------------*
007000071217     c     *entry        plist
007100090203     c                   parm                    kpjba
007200090203     c
007300071116      /free
007400090203         // Reperimento dati job
007500090203         exsr DatiJob;
007600090202
007700090204       exec sql   delete  from  wfdemd0f   ;
007800090204       exec sql   delete  from  wfdeme0f   ;
007900090204       exec sql   delete  from  wfdemi0f   ;
008000071217
008100080110
008200090204       setll *start wadelpct   ;
008300090204       read         wadelpct   ;
008400090204 1     dow not %eof(wadelpct)   ;
008500090205
008600090205         // Escludo distretto estero   agente unificante
008700090205         wfil=%int(%subst(%editc(wapauc:'X'):1:3))     ;
008800090205         chain   wfil   azorg01l   ;
008900090205
009000090205 1a      if orgfl3<>'7'    ;
009100090205
009200090205         kcod = %editc(wapcli: 'X' ) ;
009300090205         // Escludo codici indirizzo italia ma esteri
009400090205 1b         if kcod<>'0894106' and kcod<>'0894274' ;
009500090205
009600090205         // Escludo 8888  e 9999
009700090205 1c        if (%subst(kcod:4:4)<>'9999' and %subst(kcod:4:4)<>'8888') ;
009800090205
009900090203
010000090204       kpiv=wapiva       ;
010100090204       kcdf=wapcdf        ;
010200090204       kcli=wapcli        ;
010300090205       kioe=wapioe        ;
010400080110       clear   wtrov ;
010500090204 2     if kpiv<>*blanks  and %subst(kpiv:1:2)<>'$$'   ;
010600090203
010700090205       chain   (kpiv:' ')      WFDEmd1l  ;
010800090204 3     if %found(wfdemd1l) ;
010900080110       Wtrov='S' ;
011000090204 3     else  ;
011100080110
011200090205       chain   (kpiv:' ')      WFDEmd2l  ;
011300090204 4     if %found(wfdemd2l) ;
011400080110       Wtrov='S' ;
011500090204 4     endif ;
011600090204 3     endif ;
011700090204 2     endif ;
011800080110
011900090204 2     if  kcdf<>*blanks  and wtrov<>'S'   ;
012000080110
012100090205       chain   (kcdf:' ')    WFdemd1l  ;
012200090204 3     if %found(wfdemd1l ) ;
012300080110       Wtrov='S' ;
012400090204 3     else  ;
012500080110
012600090205       chain   (kcdf:' ')    WFDEmd2l  ;
012700090204 4     if %found(wfdemd2l) ;
012800080110       Wtrov='S' ;
012900090203 4     endif ;
013000090203 3     endif ;
013100080110
013200090203 2     endif ;
013300080110
013400090203 2     if  wtrov='S'  ;
013500090205       exsr CtrFATT    ;
013600090204       else    ;
013700090204       exsr Scrividati  ;
013800090205 2     endif    ;
013900090205 1c    endif    ;
014000090205 1b    endif    ;
014100090205 1a    endif    ;
014200090205
014300090205       read         wadelpct   ;
014400090205 1     enddo    ;
014500090204
014600090205       // Sommo ora i dati del delta  per gli unificati   DEMD
014700090205       setll *start wfdemd0f   ;
014800090205       read         wfdemd0f   ;
014900090205 1     dow not %eof(wfdemd0f)   ;
015000090205
015100090205       if wapunio='U'   ;
015200090205       chain    wapcliuni    wfdemd3l   ;
015300090205       exsr ADDdatiD  ;
015400090205       exsr Cleardati  ;
015500090205       update wfdemd00 ;
015600090205       endif  ;
015700090205
015800090205       read         wfdemd0f   ;
015900090205       enddo   ;
016000090205
016100090205       // Sommo ora i dati del delta  per gli unificati DEME
016200090205       setll *start wfdeme0f   ;
016300090205       read         wfdeme0f   ;
016400090205 1     dow not %eof(wfdeme0f)   ;
016500090205
016600090205       if wapunio='U'   ;
016700090205       chain    wapcliuni    wfdeme3l   ;
016800090206       if %found(wfdeme3l)  ;
016900090206         exsr DatiRicavi;
017000090206         update wfdeme03 ;
017100090206       else  ;
017200090206       chain    wapcliuni    wfdemd3l   ;
017300090206          clear demunio   ;
017400090206          clear demcliuni ;
017500090206          demcli=wapcliuni ;
017600090206          demioe='E' ;
017700090206          %subst(wfdemd0ds:186:32)=%subst(wadelpcds:178:32);
017800090206          %subst(wfdemd0ds:221:521)=%subst(wadelpcds:213:521);
017900090206          %subst(wfdemd0ds:1051:201)=%subst(wadelpcds:1043:201);
018000090206          write wfdeme03  ;
018100090206       endif    ;
018200090205
018300090205       exsr Cleardati  ;
018400090205       update wfdeme00 ;
018500090205       endif  ;
018600090205
018700090205       read         wfdeme0f   ;
018800090205       enddo   ;
018900090205
019000090205       // Sommo ora i dati del delta  per gli unificati  DEMI
019100090205       setll *start wfdemi0f   ;
019200090205       read         wfdemi0f   ;
019300090205 1     dow not %eof(wfdemi0f)   ;
019400090205
019500090205       if wapunio='U'   ;
019600090205       chain    wapcliuni    wfdemi3l   ;
019700090206       if %found(wfdemi3l)  ;
019800090206         exsr DatiRicavi;
019900090206         update wfdemi03 ;
020000090206       else  ;
020100090206       chain    wapcliuni    wfdemd3l   ;
020200090206          clear demunio   ;
020300090206          clear demcliuni ;
020400090206          demcli=wapcliuni ;
020500090206          demioe='E' ;
020600090206          %subst(wfdemd0ds:186:32)=%subst(wadelpcds:178:32);
020700090206          %subst(wfdemd0ds:221:521)=%subst(wadelpcds:213:521);
020800090206          %subst(wfdemd0ds:1051:201)=%subst(wadelpcds:1043:201);
020900090206          write wfdemi03  ;
021000090206       endif ;
021100090205
021200090205       exsr Cleardati  ;
021300090205       update wfdemi00 ;
021400090205       endif  ;
021500090205
021600090205       read         wfdemi0f   ;
021700090205       enddo   ;
021800090205
021900071121       *inlr=*on         ;
022000071116       //-------------------------------------------------------------
022100090204       //  Scrittura file WFDEMD0F/E0F / I0F
022200090202       //-------------------------------------------------------------
022300090204       BEGSR  ScriviDati    ;
022400090204
022500090205       %subst(wfdemd0ds:9:1250)=wadelpcds ;
022600090205       clear demunio   ;
022700090205       clear demcliuni ;
022800090204       write wfdemd03  ;
022900090204
023000090205       if kioe  <>'I'   ;
023100090205       exec sql   select * into :wadelpcds from WADELPCE
023200090205                                                         where wapcli=:kcli   ;
023300090204         if sqlcod=*zeros  ;
023400090205         %subst(wfdemd0ds:9:1250)=wadelpcds ;
023500090205          clear demunio   ;
023600090205          clear demcliuni ;
023700090204          write wfdeme03  ;
023800090204         endif ;
023900090204       endif ;
024000090204
024100090205       if kioe  <>'E'   ;
024200090205       exec sql   select * into :wadelpcds from WADELPCF where wapcli=:kcli   ;
024300090204         if sqlcod=*zeros  ;
024400090205         %subst(wfdemd0ds:9:1250)=wadelpcds ;
024500090205           clear demunio   ;
024600090205           clear demcliuni ;
024700090204           write wfdemi03  ;
024800090204         endif ;
024900090204       endif ;
025000090204
025100071116       ENDSR                ;
025200090205       //-------------------------------------------------------------
025300090205       //  controllo dati fatturato per vedere quale unificare
025400090205       //-------------------------------------------------------------
025500090205       BEGSR  ctrFATT       ;
025600090205       clear  oldcli   ;
025700090205       newcli=d_wapcli ;
025800090205
025900090205       // Scelgo i dati del cliente  con maggior fatturato anno corr
026000090205 1     if wapfac>d_wapfac   ;
026100090205       oldcli=d_wapcli  ;
026200090205       newcli=wapcli  ;
026300090205
026400090205       // chaino codice OLD e aggiorno falg unificato
026500090205       chain  oldcli   wfdemd3l ;
026600090205 2     if %found(wfdemd3l)  ;
026700090205       DEMunio='U'   ;
026800090205       DEMcliuni=newcli ;
026900090205       update wfdemd03 ;
027000090205
027100090205       chain  oldcli   wfdemi3l ;
027200090205 3     if %found(wfdemi3l)  ;
027300090205       DEMunio='U'   ;
027400090205       DEMcliuni=newcli ;
027500090205       update wfdemi03                         ;
027600090205 3     endif   ;
027700090205
027800090205       chain  oldcli   wfdeme3l ;
027900090205 3     if %found(wfdeme3l)  ;
028000090205       DEMunio='U'   ;
028100090205       DEMcliuni=newcli ;
028200090205       update wfdeme03                              ;
028300090205 3     endif   ;
028400090205 2     endif   ;
028500090205
028600090205       // ciclo altro codici unificati in precedenza
028700090205       exsr cicloAltri  ;
028800090205
028900090205       // scrittura nuovo cliente così come è
029000090205       exsr Scrividati  ;
029100090205
029200090205 x1    else   ;
029300090205       %subst(wfdemd0ds:9:1250)=wadelpcds ;
029400090205       DEMunio='U'   ;
029500090205       DEMcliuni=newcli ;
029600090205       write wfdemd03  ;
029700090205
029800090205       if wapioe<>'I'   ;
029900090205       exec sql   select * into :wadelpcds from WADELPCE where wapcli=:kcli   ;
030000090205         if sqlcod=*zeros  ;
030100090205         %subst(wfdemd0ds:9:1250)=wadelpcds ;
030200090205          DEMunio='U'   ;
030300090205          DEMcliuni=newcli ;
030400090205          write wfdeme03  ;
030500090205         endif ;
030600090205       endif ;
030700090205
030800090205       if wapioe<>'E'   ;
030900090205       exec sql   select * into :wadelpcds from WADELPCF where wapcli=:kcli   ;
031000090205         if sqlcod=*zeros  ;
031100090205           %subst(wfdemd0ds:9:1250)=wadelpcds ;
031200090205           DEMunio='U'   ;
031300090205           DEMcliuni=newcli ;
031400090205           write wfdemi03  ;
031500090205         endif ;
031600090205       endif ;
031700090205
031800090205       endif    ;
031900090205       ENDSR                ;
032000090204       //-------------------------------------------------------------
032100090205       //  ciclo altri codici unificati in precedenza
032200090204       //-------------------------------------------------------------
032300090205       BEGSR  cicloaltri     ;
032400090205        setll oldcli wfdemd4l  ;
032500090205        reade oldcli wfdemd4l  ;
032600090205        dow not%eof(wfdemd4l)  ;
032700090205        DEMcliuni=newcli ;
032800090205        update wfdemd04   ;
032900090205        reade oldcli wfdemd4l  ;
033000090205        enddo   ;
033100090205
033200090205        setll oldcli wfdemi4l  ;
033300090205        reade oldcli wfdemi4l  ;
033400090205        dow not%eof(wfdemi4l)  ;
033500090205        DEMcliuni=newcli ;
033600090205        update wfdemi04   ;
033700090205        reade oldcli wfdemi4l  ;
033800090205        enddo   ;
033900090205
034000090205        setll oldcli wfdeme4l  ;
034100090205        reade oldcli wfdeme4l  ;
034200090205        dow not%eof(wfdeme4l)  ;
034300090205        DEMcliuni=newcli ;
034400090205        update wfdeme04   ;
034500090205        reade oldcli wfdeme4l  ;
034600090205        enddo   ;
034700090205        ENDSR  ;
034800090205       //-------------------------------------------------------------
034900090205       //  Aggiunta dati delta  WFDEMD0F
035000090205       //-------------------------------------------------------------
035100090205       BEGSR  ADDdatiD       ;
035200090204
035300090204       // flag italia estero I/E/ _ per entrambi
035400090204       select ;
035500090204       // se uguali --> già ok
035600090205       when wapioe=DEMIOE ;
035700090204
035800090204       // se il nuovo a blanks --> già ok
035900090204       when DEMioe=' '  ;
036000090204
036100090204       // se il vecchio a blanks --> aggiorno
036200090204       when wapioe=' '  ;
036300090204       DEMioe=' '     ;
036400090204
036500090204       // se diversi (I/E)  --> aggiorno  con blanks
036600090204       when wapioe<>DEMIOE  ;
036700090204       DEMioe=' '     ;
036800090204       ENDSL  ;
036900090204
037000090204       exsr    DatiRicavi  ;
037100090204
037200090204       update wfdemd03  ;
037300090204
037400090204       ENDSR                ;
037500090204       //-------------------------------------------------------------
037600090204       //  Aggiornamento dai ricavi
037700090204       //-------------------------------------------------------------
037800090204       BEGSR   datiRicavi  ;
037900090204         demSAP     = demSAP      + WAPSAP       ;
038000090204         demkgP     = demkgP      + WAPkgP       ;
038100090204         demFAP     = demFAP      + WAPFAP       ;
038200090204         demSAC     = demSAC      + WAPSAC       ;
038300090204         demkgC     = demkgC      + WAPkgC       ;
038400090204         demFAC     = demFAC      + WAPFAC       ;
038500090204         demNS_01AP = demNS_01AP  + WAPNS_01Ap   ;
038600090204         demkg_01AP = demkg_01AP  + WAPkg_01Ap   ;
038700090204         demRR_01AP = demRR_01AP  + WAPRR_01Ap   ;
038800090204         demRP_01AP = demRP_01AP  + WAPRP_01Ap   ;
038900090204         demNS_02AP = demNS_02AP  + WAPNS_02Ap   ;
039000090204         demkg_02AP = demkg_02AP  + WAPkg_02Ap   ;
039100090204         demRR_02AP = demRR_02AP  + WAPRR_02Ap   ;
039200090204         demRP_02AP = demRP_02AP  + WAPRP_02Ap   ;
039300090204
039400090204         demNS_03AP = demNS_03AP  + WAPNS_03AP   ;
039500090204         demkg_03AP = demkg_03AP  + WAPkg_03AP   ;
039600090204         demRR_03AP = demRR_03AP  + WAPRR_03AP   ;
039700090204         demRP_03AP = demRP_03AP  + WAPRP_03AP   ;
039800090204         demNS_04AP = demNS_04AP  + WAPNS_04AP   ;
039900090204         demkg_04AP = demkg_04AP  + WAPkg_04AP   ;
040000090204         demRR_04AP = demRR_04AP  + WAPRR_04AP   ;
040100090204         demRP_04AP = demRP_04AP  + WAPRP_04AP   ;
040200090204         demNS_05AP = demNS_05AP  + WAPNS_05AP   ;
040300090204         demkg_05AP = demkg_05AP  + WAPkg_05AP   ;
040400090204         demRR_05AP = demRR_05AP  + WAPRR_05AP   ;
040500090204         demRP_05AP = demRP_05AP  + WAPRP_05AP   ;
040600090204         demNS_06AP = demNS_06AP  + WAPNS_06AP   ;
040700090204         demkg_06AP = demkg_06AP  + WAPkg_06AP   ;
040800090204         demRR_06AP = demRR_06AP  + WAPRR_06AP   ;
040900090204         demRP_06AP = demRP_06AP  + WAPRP_06AP   ;
041000090204         demNS_07AP = demNS_07AP  + WAPNS_07AP   ;
041100090204         demkg_07AP = demkg_07AP  + WAPkg_07AP   ;
041200090204         demRR_07AP = demRR_07AP  + WAPRR_07AP   ;
041300090204         demRP_07AP = demRP_07AP  + WAPRP_07AP   ;
041400090204         demNS_08AP = demNS_08AP  + WAPNS_08AP   ;
041500090204         demkg_08AP = demkg_08AP  + WAPkg_08AP   ;
041600090204         demRR_08AP = demRR_08AP  + WAPRR_08AP   ;
041700090204         demRP_08AP = demRP_08AP  + WAPRP_08AP   ;
041800090204         demNS_09AP = demNS_09AP  + WAPNS_09AP   ;
041900090204         demkg_09AP = demkg_09AP  + WAPkg_09AP   ;
042000090204         demRR_09AP = demRR_09AP  + WAPRR_09AP   ;
042100090204         demRP_09AP = demRP_09AP  + WAPRP_09AP   ;
042200090204         demNS_10AP = demNS_10AP  + WAPNS_10AP   ;
042300090204         demkg_10AP = demkg_10AP  + WAPkg_10AP   ;
042400090204         demRR_10AP = demRR_10AP  + WAPRR_10AP   ;
042500090204         demRP_10AP = demRP_10AP  + WAPRP_10AP   ;
042600090204         demNS_11AP = demNS_11AP  + WAPNS_11AP    ;
042700090204         demkg_11AP = demkg_11AP  + WAPkg_11AP   ;
042800090204         demRR_11AP = demRR_11AP  + WAPRR_11AP   ;
042900090204         demRP_11AP = demRP_11AP  + WAPRP_11AP   ;
043000090204         demNS_12AP = demNS_12AP  + WAPNS_12AP   ;
043100090204         demkg_12AP = demkg_12AP  + WAPkg_12AP  ;
043200090204         demRR_12AP = demRR_12AP  + WAPRR_12AP  ;
043300090204         demRP_12AP = demRP_12AP  + WAPRP_12AP  ;
043400090204         demNS_01AC = demNS_01AC  + WAPNS_01AC  ;
043500090204         demkg_01AC = demkg_01AC  + WAPkg_01AC  ;
043600090204         demRR_01AC = demRR_01AC  + WAPRR_01AC  ;
043700090204         demRP_01AC = demRP_01AC  + WAPRP_01AC  ;
043800090204         demNS_02AC = demNS_02AC  + WAPNS_02AC    ;
043900090204         demkg_02AC = demkg_02AC  + WAPkg_02AC    ;
044000090204         demRR_02AC = demRR_02AC  + WAPRR_02AC    ;
044100090204         demRP_02AC = demRP_02AC  + WAPRP_02AC    ;
044200090204         demNS_03AC = demNS_03AC  + WAPNS_03AC    ;
044300090204         demkg_03AC = demkg_03AC  + WAPkg_03AC    ;
044400090204         demRR_03AC = demRR_03AC  + WAPRR_03AC    ;
044500090204         demRP_03AC = demRP_03AC  + WAPRP_03AC  ;
044600090204         demNS_04AC = demNS_04AC  + WAPNS_04AC  ;
044700090204         demkg_04AC = demkg_04AC  + WAPkg_04AC  ;
044800090204         demRR_04AC = demRR_04AC  + WAPRR_04AC  ;
044900090204         demRP_04AC = demRP_04AC  + WAPRP_04AC  ;
045000090204         demNS_05AC = demNS_05AC  + WAPNS_05AC  ;
045100090204         demkg_05AC = demkg_05AC  + WAPkg_05AC  ;
045200090204         demRR_05AC = demRR_05AC  + WAPRR_05AC    ;
045300090204         demRP_05AC = demRP_05AC  + WAPRP_05AC    ;
045400090204         demNS_06AC = demNS_06AC  + WAPNS_06AC    ;
045500090204         demkg_06AC = demkg_06AC  + WAPkg_06AC    ;
045600090204         demRR_06AC = demRR_06AC  + WAPRR_06AC    ;
045700090204         demRP_06AC = demRP_06AC  + WAPRP_06AC    ;
045800090204         demNS_07AC = demNS_07AC  + WAPNS_07AC    ;
045900090204         demkg_07AC = demkg_07AC  + WAPkg_07AC    ;
046000090204         demRR_07AC = demRR_07AC  + WAPRR_07AC    ;
046100090204         demRP_07AC = demRP_07AC  + WAPRP_07AC    ;
046200090204         demNS_08AC = demNS_08AC  + WAPNS_08AC    ;
046300090204         demkg_08AC = demkg_08AC  + WAPkg_08AC    ;
046400090204         demRR_08AC = demRR_08AC  + WAPRR_08AC    ;
046500090204         demRP_08AC = demRP_08AC  + WAPRP_08AC    ;
046600090204         demNS_09AC = demNS_09AC  + WAPNS_09AC   ;
046700090204         demkg_09AC = demkg_09AC  + WAPkg_09AC   ;
046800090204         demRR_09AC = demRR_09AC  + WAPRR_09AC   ;
046900090204         demRP_09AC = demRP_09AC  + WAPRP_09AC   ;
047000090204         demNS_10AC = demNS_10AC  + WAPNS_10AC   ;
047100090204         demkg_10AC = demkg_10AC  + WAPkg_10AC   ;
047200090204         demRR_10AC = demRR_10AC  + WAPRR_10AC   ;
047300090204         demRP_10AC = demRP_10AC  + WAPRP_10AC     ;
047400090204         demNS_11AC = demNS_11AC  + WAPNS_11AC     ;
047500090204         demkg_11AC = demkg_11AC  + WAPkg_11AC     ;
047600090204         demRR_11AC = demRR_11AC  + WAPRR_11AC     ;
047700090204         demRP_11AC = demRP_11AC  + WAPRP_11AC     ;
047800090204         demNS_12AC = demNS_12AC  + WAPNS_12AC     ;
047900090204         demkg_12AC = demkg_12AC  + WAPkg_12AC     ;
048000090204         demRR_12AC = demRR_12AC  + WAPRR_12AC  ;
048100090204         demRP_12AC = demRP_12AC  + WAPRP_12AC  ;
048200090204
048300090204       // Ricalcolo del delta
048400090204         exsr      New_Delta            ;
048500090204
048600090204       //  Deve controllare qual'è il mese in cui è stato considerato Nuovo
048700090204       //   fra Italia / Estero .
048800090204          if        (WAPNEW_MES > demNEW_MES and demNEW_MES = 0)
048900090204                     or
049000090204                    (WAPNEW_MES < demNEW_MES and wapNEW_MES > 0)   ;
049100090204               demNEW_MES = WAPNEW_MES                             ;
049200090205          endif                                                    ;
049300090204
049400090204       ENDSR                  ;
049500090205       //-------------------------------------------------------------
049600090205       //  Pulisco campi ricavi del recod unificato
049700090205       //-------------------------------------------------------------
049800090205       BEGSR  clearDati      ;
049900090205       clear wapsap   ;
050000090205       clear wapfap   ;
050100090205       clear wapdap   ;
050200090205       clear wapsac   ;
050300090205       clear wapfac   ;
050400090205       clear wapdac   ;
050500090205       clear wapns_01AP   ;
050600090205       clear waprr_01AP   ;
050700090205       clear waprp_01AP   ;
050800090205       clear wapns_02AP   ;
050900090205       clear waprr_02AP   ;
051000090205       clear waprp_02AP   ;
051100090205       clear wapns_03AP   ;
051200090205       clear waprr_03AP   ;
051300090205       clear waprp_03AP   ;
051400090205       clear wapns_04AP   ;
051500090205       clear waprr_04AP   ;
051600090205       clear waprp_04AP   ;
051700090205       clear wapns_05AP   ;
051800090205       clear waprr_05AP   ;
051900090205       clear waprp_05AP   ;
052000090205       clear wapns_06AP   ;
052100090205       clear waprr_06AP   ;
052200090205       clear waprp_06AP   ;
052300090205       clear wapns_07AP   ;
052400090205       clear waprr_07AP   ;
052500090205       clear waprp_07AP   ;
052600090205       clear wapns_08AP   ;
052700090205       clear waprr_08AP   ;
052800090205       clear waprp_08AP   ;
052900090205       clear wapns_09AP   ;
053000090205       clear waprr_09AP   ;
053100090205       clear waprp_09AP   ;
053200090205       clear wapns_10AP   ;
053300090205       clear waprr_10AP   ;
053400090205       clear waprp_10AP   ;
053500090205       clear wapns_11AP   ;
053600090205       clear waprr_11AP   ;
053700090205       clear waprp_11AP   ;
053800090205       clear wapns_12AP   ;
053900090205       clear waprr_12AP   ;
054000090205       clear waprp_12AP   ;
054100090205       clear wapns_01Ac   ;
054200090205       clear waprr_01Ac   ;
054300090205       clear waprp_01Ac   ;
054400090205       clear wapns_02Ac   ;
054500090205       clear waprr_02Ac   ;
054600090205       clear waprp_02Ac   ;
054700090205       clear wapns_03Ac   ;
054800090205       clear waprr_03Ac   ;
054900090205       clear waprp_03Ac   ;
055000090205       clear wapns_04Ac   ;
055100090205       clear waprr_04Ac   ;
055200090205       clear waprp_04Ac   ;
055300090205       clear wapns_05Ac   ;
055400090205       clear waprr_05Ac   ;
055500090205       clear waprp_05Ac   ;
055600090205       clear wapns_06Ac   ;
055700090205       clear waprr_06Ac   ;
055800090205       clear waprp_06Ac   ;
055900090205       clear wapns_07Ac   ;
056000090205       clear waprr_07Ac   ;
056100090205       clear waprp_07Ac   ;
056200090205       clear wapns_08Ac   ;
056300090205       clear waprr_08Ac   ;
056400090205       clear waprp_08Ac   ;
056500090205       clear wapns_09Ac   ;
056600090205       clear waprr_09Ac   ;
056700090205       clear waprp_09Ac   ;
056800090205       clear wapns_10Ac   ;
056900090205       clear waprr_10Ac   ;
057000090205       clear waprp_10Ac   ;
057100090205       clear wapns_11Ac   ;
057200090205       clear waprr_11Ac   ;
057300090205       clear waprp_11Ac   ;
057400090205       clear wapns_12Ac   ;
057500090205       clear waprr_12Ac   ;
057600090205       clear waprp_12Ac   ;
057700090205
057800090205       clear wapkg_01AP   ;
057900090205       clear wapkg_02AP   ;
058000090205       clear wapkg_03AP   ;
058100090205       clear wapkg_04AP   ;
058200090205       clear wapkg_05AP   ;
058300090205       clear wapkg_06AP   ;
058400090205       clear wapkg_07AP   ;
058500090205       clear wapkg_08AP   ;
058600090205       clear wapkg_09AP   ;
058700090205       clear wapkg_10AP   ;
058800090205       clear wapkg_11AP   ;
058900090205       clear wapkg_12AP   ;
059000090205       clear wapkgp       ;
059100090205       clear wapkg_01Ac   ;
059200090205       clear wapkg_02Ac   ;
059300090205       clear wapkg_03Ac   ;
059400090205       clear wapkg_04Ac   ;
059500090205       clear wapkg_05Ac   ;
059600090205       clear wapkg_06Ac   ;
059700090205       clear wapkg_07Ac   ;
059800090205       clear wapkg_08Ac   ;
059900090205       clear wapkg_09Ac   ;
060000090205       clear wapkg_10Ac   ;
060100090205       clear wapkg_11Ac   ;
060200090205       clear wapkg_12Ac   ;
060300090205       clear wapkgc       ;
060400090205       ENDSR                  ;
060500090203       //--------------------------------------------------------------
060600090203       //?Reperimento Dati del job (Utente/Operativi).
060700090203       //--------------------------------------------------------------
060800090203       BEGSR DatiJob;
060900090203
061000090203         in(E) §AzUte;
061100090203         if NOT %error;
061200090203           in(E) §DatiUte;
061300090203         endif;
061400090203         if %error or RSut = *blanks;
061500090203           clear TIBS34ds;
061600090203           tibs34r(tibs34ds);
061700090203           in §AzUte;
061800090203           in §DatiUte;
061900090203         endif;
062000090203
062100090203       ENDSR;
062200090203
062300090203
062400071116      /end-free
062500090204     c
062600090204     C*------------------------------------------------------------------------*
062700090204      *   deve ricalcolare il Delta per la somma su Anno precedente e in Corso
062800090204     C*------------------------------------------------------------------------*
062900090204     C     New_Delta     BEgSR
063000090204     C*
063100090204     C* DELTA 12 MESI
063200090204     C                   clear                   WRRW                           *RICAVO REALE
063300090204     C                   clear                   WRPW                           *RICAVO PRESUNTO
063400090204     C                   clear                   WDPW                           *DELTA PERCENTUALE
063500090206     c                   eval      WRRW = + DEMRR_01AP
063600090206     c                                    + DEMRR_02AP
063700090206     c                                    + DEMRR_03AP
063800090206     c                                    + DEMRR_04AP
063900090206     c                                    + DEMRR_05AP
064000090206     c                                    + DEMRR_06AP
064100090206     c                                    + DEMRR_07AP
064200090206     c                                    + DEMRR_08AP
064300090206     c                                    + DEMRR_09AP
064400090206     c                                    + DEMRR_10AP
064500090206     c                                    + DEMRR_11AP
064600090206     c                                    + DEMRR_12AP
064700090204      *
064800090206     c                   eval      WRPW = + DEMRP_01AP
064900090206     c                                    + DEMRP_02AP
065000090206     c                                    + DEMRP_03AP
065100090206     c                                    + DEMRP_04AP
065200090206     c                                    + DEMRP_05AP
065300090206     c                                    + DEMRP_06AP
065400090206     c                                    + DEMRP_07AP
065500090206     c                                    + DEMRP_08AP
065600090206     c                                    + DEMRP_09AP
065700090206     c                                    + DEMRP_10AP
065800090206     c                                    + DEMRP_11AP
065900090206     c                                    + DEMRP_12AP
066000090204     C                   EXSR      CALDEL
066100090206     C                   Z-ADD     WDPW          DEMDAP                         *SCHIERA DELTA
066200090204      *
066300090204     C                   clear                   WRRW             17 5          *RICAVO REALE
066400090204     C                   clear                   WRPW             17 5          *RICAVO PRESUNTO
066500090204     C                   clear                   WDPW              4 1          *DELTA PERCENTUALE
066600090206     c                   eval      WRRW = + DEMRR_01Ac
066700090206     c                                    + DEMRR_02Ac
066800090206     c                                    + DEMRR_03Ac
066900090206     c                                    + DEMRR_04Ac
067000090206     c                                    + DEMRR_05Ac
067100090206     c                                    + DEMRR_06Ac
067200090206     c                                    + DEMRR_07Ac
067300090206     c                                    + DEMRR_08Ac
067400090206     c                                    + DEMRR_09Ac
067500090206     c                                    + DEMRR_10Ac
067600090206     c                                    + DEMRR_11Ac
067700090206     c                                    + DEMRR_12Ac
067800090204      *
067900090206     c                   eval      WRPW = + DEMRP_01Ac
068000090206     c                                    + DEMRP_02Ac
068100090206     c                                    + DEMRP_03Ac
068200090206     c                                    + DEMRP_04Ac
068300090206     c                                    + DEMRP_05Ac
068400090206     c                                    + DEMRP_06Ac
068500090206     c                                    + DEMRP_07Ac
068600090206     c                                    + DEMRP_08Ac
068700090206     c                                    + DEMRP_09Ac
068800090206     c                                    + DEMRP_10Ac
068900090206     c                                    + DEMRP_11Ac
069000090206     c                                    + DEMRP_12Ac
069100090204     C                   EXSR      CALDEL
069200090206     C                   Z-ADD     WDPW          DEMDAC                         *SCHIERA DELTA
069300090204     C*
069400090204     C                   ENDSR
069500090204     C*------------------------------------------------------------------------*
069600090204     C* CALDEL - CALCOLA IL DELTA
069700090204     C*          INPUT  --> WRRW: RICAVO REALE
069800090204     C*                     WRPW: RICAVO PRESUNTO
069900090204     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
070000090204     C*------------------------------------------------------------------------*
070100090204     C     CALDEL        BEGSR
070200090204     C*--------------------
070300090204     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
070400090204     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
070500090204     C*--------------------
070600090204     C*
070700090204     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
070800090204     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
070900090204     C     GAP           MULT      100           GAP                            (GAP x 100)
071000090204     C*
071100090204     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
071200090204IF  1C     WRRW          IFEQ      *ZEROS
071300090204     C                   Z-ADD     1             WRRW
071400090204E   1C                   ENDIF
071500090204     C*
071600090204     C     GAP           DIV       WRRW          GAP
071700090204     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
071800090204     C*
071900090204     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
072000090204     C     WDPW          COMP      0                                    28
072100090204     C* POSITIVO
072200090204IF  1C     *IN28         IFEQ      *OFF
072300090204IF  2C     GAP           IFGE      1000
072400090204     C                   Z-ADD     +999,9        WDPW
072500090204E   2C                   ENDIF
072600090204     C* NEGATIVO
072700090204X   1C                   ELSE
072800090204IF  2C     GAP           IFLE      -1000
072900090204     C                   Z-ADD     -999,9        WDPW
073000090204E   2C                   ENDIF
073100090204E   1C                   ENDIF
073200090204     C*
073300090204     C                   ENDSR
