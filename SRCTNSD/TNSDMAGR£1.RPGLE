000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300100929      //?TNSDMAGR - controllo magazzino correlato / destinazione
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100929     fazorg01l  if   e           k disk
001200100929     fazorg21l  if   e           k disk
001300100929      //---------------------------------------------------------------
001400100929      // - Dati utente
001500100929     d §AzUte        e ds                  extname(AZUTE00F)
001600100929     d                                     dtaara
001700100929     d §DatiUte      e ds                  extname(dDatiUte)
001800100929     d                                     dtaara
001900100929
002000100929      //---------------------------------------------------------------
002100100929     d MSG             s             78    dim(3) ctdata perrcd(1)
002200110225     d magf            s              3    dim(10)
002300100929      //---------------------------------------------------------------
002400080206      // - Reperimento dati utente
002500080206     d TIBS34ds      e ds
002600080206     d dUte01        e ds
002700080702
002800100929     d tnsdmagds     e ds
002900100902     d KPJBA         e ds
003000100929     d og148         e ds
003100080206
003200080206      // - Ricerca/Controllo tabelle
003300080206     d TIBS02ds      e ds                  inz
003400080414
003500100929     d Indx            s              3  0 inz
003600100929     d desmag          s             12
003700100929     d wazorg          s              3    Inz('   ')
003800100929
003900100929      // ricerca filiale su organigramma
004000100929     d   pInFIL        S              3
004100100929     d   pInFAG        S              1
004200100929     d   pInDES        S             25
004300100929     d   pInDIT        S              3
004400100929
004500100929     d Digits          c                   const('0123456789')
004600100929
004700080206      //---------------------------------------------------------------
004800080206      //?Definizione procedure usate.
004900080206      //---------------------------------------------------------------
005000080414      /copy gaitrasrc/srcprotopr,tibs34r
005100100909      /copy gaitrasrc/srcprotopr,tnsd20r
005200080206
005300080206      //---------------------------------------------------------------
005400080206      //?M A I N - L I N E
005500080206      //---------------------------------------------------------------
005600080206
005700080627     c     *Entry        plist
005800080206     c                   parm                    KPJBA
005900100929     c                   parm                    tnsdmagds
006000080702
006100080206      /free
006200080206
006300080206       // Operazioni iniziali
006400080206       exsr RoutInz;
006500081215
006600100929       clear §omagmsg  ;
006700100929       clear §omagerr  ;
006800100929       clear §omagdes   ;
006900100929       clear §omagfag   ;
007000100929       clear §omaglue   ;
007100100929
007200100929       exsr ChkMAG  ;
007300080530
007400080206       // Operazioni finali
007500080206       exsr RoutEnd;
007600080206
007700080206       //--------------------------------------------------------------
007800080206       //?Operazioni iniziali.
007900080206       //--------------------------------------------------------------
008000080206       BEGSR RoutInz;
008100080703
008200080206         // Reperimento dati job
008300080206         exsr DatiJob;
008400080703
008500080627         ENDSR;
008600080206
008700080206       //--------------------------------------------------------------
008800080206       //?Reperimento Dati del job (Utente/Operativi).
008900080206       //--------------------------------------------------------------
009000080206       BEGSR DatiJob;
009100080206
009200080206         in(E) §AzUte;
009300080206         if NOT %error;
009400080206           in(E) §DatiUte;
009500080206         endif;
009600080206         if %error or RSut = *blanks;
009700080206           clear TIBS34ds;
009800080206           tibs34r(tibs34ds);
009900080206           in §AzUte;
010000080206           in §DatiUte;
010100080206         endif;
010200080206
010300080206       ENDSR;
010400100929       //---------------------------------------------------------------
010500100929       //?Controllo magazzino
010600100929       //---------------------------------------------------------------
010700100929       BEGSR CHKMAG  ;
010800101013
010900101013       clear magf          ;
011000100929
011100100929        Indx=%scan('?':§magmag )   ;
011200100929
011300100929       // Mag.Destinazione
011400100929       // per MAG --> Filiale e logistica
011500100929       // per FOR --> sia controllo azorg20f  tipi "W" "Y" che azorg00F
011600100929       // Mag.correlato
011700101013       // per MAG --> non controllo, fisso 999 e 935
011800100929       // per FOR --> filiale e logistica
011900100929
012000100929       if   §imagtorg='   ';
012100100929       select   ;
012200100929                when      §imagtmag='D' and §imaginvr='I'     ;
012300100929                eval      wazorg='F  '                  ;
012400100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr=' ' ;
012500100929                eval      wazorg='E  '                  ;
012600100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='F' ;
012700100929                eval      wazorg='S  '                  ;
012800100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='R'  ;
012900100929                eval      wazorg='SW '                  ;
013000101013                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='S'  ;
013100101013                eval      wazorg='S  '                  ;
013200100929
013300100929                when      §imagtmag='C' and §imaginvr='I'   ;
013400100929                eval      wazorg='999'                  ;
013500101013                eval      magf(1)='999'                 ;
013600101013                eval      magf(2)='935'                 ;
013700101203                eval      magf(3)='946'                 ;
013800110225                eval      magf(4)='940'                 ;
013900110324                eval      magf(5)='903'                 ;
014000100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='F' ;
014100100929                eval      wazorg='F  '                  ;
014200100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='R' ;
014300100929                eval      wazorg='SY '                  ;
014400101013                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='S' ;
014500101013                eval      wazorg='SW '                  ;
014600100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr=' '  ;
014700100929                eval      wazorg='E  '                  ;
014800100929       ENDSL  ;
014900100929       else  ;
015000101013
015100100929       wazorg=§imagtorg  ;
015200101013       if wazorg='999'   ;
015300101013            magf(1)='999'                 ;
015400101013            magf(2)='935'                 ;
015500110225            magf(3)='946'                 ;
015600110225            magf(4)='940'                 ;
015700110324            magf(5)='903'                 ;
015800101013            else  ;
015900101013            magf(1)=wazorg                ;
016000101013       endif  ;
016100101013
016200100929       endif   ;
016300100929
016400100929       // ricerca alfabetica
016500100929       if Indx>0 ;
016600100929       clear pinfil   ;
016700100929       clear pinfag   ;
016800100929       clear pindes   ;
016900100929       callp TNSD20R (pinfil:pinfag:pindes)   ;
017000100929       if pinfil>*zeros  ;
017100100930       orgfil =%int(pinfil)   ;
017200100930       §magmag=pinfil   ;
017300100929       §omagdes=pindes   ;
017400100929       else    ;
017500100929       clear §magmag ;
017600100929       endif  ;
017700100929
017800100929        §omagerr='R'      ;
017900100929       endif  ;
018000100929
018100100929       clear og148  ;
018200100929
018300100929 0     if §omagerr<>'R'  ;
018400100929       // Tipo magazzino da impostare nei messaggi
018500100930         IF §imagTMAG='D'  ;
018600100929         desmag= 'DESTINAZIONE'    ;
018700100929         ELSE  ;
018800100929         desmag= '"CORRELATO" ' ;
018900100929         endif    ;
019000100929
019100100929        if   %checkr(digits:§magmag)>0;
019200100929             §omagerr='E'     ;
019300100929 4           if §imagtmag='D'  ;
019400100929              §omagmsg=msg(3)  ;
019500100929             else  ;
019600100929              §omagmsg=msg(2)  ;
019700100929 4           endif ;
019800100929             leavesr  ;
019900100929        endif  ;
020000100929
020100100929       eval   orgfil=%int(§magmag) ;
020200100929
020300100929 1     if %subst(wazorg:3:1)= ' ' ;
020400100929       chain   orgfil   azorg21l   ;
020500100929 2     if  not %found(azorg21l) ;
020600100929        clear orgfag      ;
020700100929        clear og148       ;
020800100929       else   ;
020900100929       og148=orgde8  ;
021000100929       endif  ;
021100100929
021200100929 2     select ;
021300100929 2     when  orgfag=' ' ;
021400100929          §omagerr='E'     ;
021500100929          §omagmsg=msg(01) ;
021600100929          %suBST(§omagmsg:14:12) =desmag         ;
021700100929          %suBST(§omagmsg:39:13) ='un codice 9xx'  ;
021800100929
021900100929 2     when  %subst(wazorg:1:1)= 'F' and orgfag<>'F' and orgfag<>'A'  ;
022000100929          §omagerr='E'      ;
022100100929          §omagmsg=msg(01) ;
022200100929          %suBST(§omagmsg:14:12) =desmag         ;
022300100929          %suBST(§omagmsg:39:11) ='una filiale o Logistica o"951"'  ;
022400100929
022500100929 2     when  %subst(wazorg:2:1)<>' ' and orgfag<> %subst(wazorg:2:1);
022600100929         §omagerr='E'      ;
022700100929         §omagmsg=msg(01) ;
022800100929         %suBST(§omagmsg:14:12) =desmag         ;
022900100929 3       if %subst(wazorg:2:1)='Y'  ;
023000100929         %suBST(§omagmsg:39:12) ='un fornitore' ;
023100100929         else  ;
023200100929         %suBST(§omagmsg:39:17) ='un codice di sede'  ;
023300100929 3       endif  ;
023400100929 x2    other  ;
023500100929       §omagdes=orgdes  ;
023600100929       §omagfag=orgfag  ;
023700100929       §omaglue=§oglue  ;
023800100929 2     endsl    ;
023900100929 1     endif  ;
024000100929
024100100929 1     if §omagerr='E'  and (%subst(wazorg:1:1)= 'F' or
024200100929                             %subst(wazorg:1:1)= 'E ')    ;
024300100929         chain   orgfil  azorg01l   ;
024400100929         if %found(azorg01l) ;
024500100929         og148=orgde8  ;
024600100929         else ;
024700100929         clear  og148  ;
024800100929         clear orgfag  ;
024900100929         endif ;
025000100929
025100100930 2      if §oglue= 'S' or orgfag='A' or orgfag='F'   ;
025200100929         §omagdes=orgdes  ;
025300100929         §omagfag=orgfag  ;
025400100929         §omaglue=§oglue  ;
025500100929         §omagerr=' '     ;
025600100929         clear §omagmsg   ;
025700100929 x2      else     ;
025800100929         §omagerr='E'      ;
025900100929 3         if %subst(wazorg:2:1)='E'  ;
026000100929
026100100929 4           if §imagtmag='D'  ;
026200100929              §omagmsg=msg(3)  ;
026300100929             else  ;
026400100929              §omagmsg=msg(2)  ;
026500100929 4           endif ;
026600100929 x3        else  ;
026700100929            §omagmsg=msg(01) ;
026800100929            %suBST(§omagmsg:14:12) =desmag         ;
026900100929            %suBST(§omagmsg:39:11) ='una filiale o Logistica o "951"'  ;
027000100929 3         endif    ;
027100100929
027200100929 2     endif   ;
027300100929 1     endif   ;
027400100929
027500100929       // Magazzino specifico
027600100929 1     if %subst(wazorg:3:1)<> ' '  ;
027700101013         Indx=%lookup(§magmag:magf)   ;
027800101013 2       if  Indx=0  ;
027900100929
028000100929         §omagerr='E'      ;
028100100929         §omagmsg=msg(01)   ;
028200100929         %suBST(§omagmsg:14:12) =desmag         ;
028300101013 3       if magf(1)<>*blanks  ;
028400101013         %suBST(§omagmsg:39:3) =magf(1)        ;
028500101013 3       endif  ;
028600101013 3       if magf(2)<>*blanks  ;
028700101013         %suBST(§omagmsg:42:4) =',' + magf(2)        ;
028800101013 3       endif  ;
028900101013 3       if magf(3)<>*blanks  ;
029000101013         %suBST(§omagmsg:46:4) =',' + magf(3)        ;
029100101013 3       endif  ;
029200101013 3       if magf(4)<>*blanks  ;
029300101013         %suBST(§omagmsg:50:4) =',' + magf(4)        ;
029400101013 3       endif                ;
029500101013 3       if magf(5)<>*blanks  ;
029600101013         %suBST(§omagmsg:54:5) =', ...'               ;
029700101013 3       endif  ;
029800100929
029900100929 2       endif ;
030000100929 1       endif ;
030100100929 0       endif ;
030200100929
030300100929        // Per ricerca o magazzino specifico chaino azorg pre prendere i dati di output
030400100929 1     if %subst(wazorg:3:1)<> ' '  or §omagerr='R'  ;
030500100929         chain   orgfil   azorg21l   ;
030600100929 2       if  not %found(azorg21l) ;
030700100929         chain   orgfil   azorg01l   ;
030800100929 3       if  not %found(azorg01l) ;
030900100929         clear orgde8      ;
031000100929         §omagerr='E'      ;
031100100929 4       if §imagtmag='D'  ;
031200100929          §omagmsg=msg(3)  ;
031300100929         else  ;
031400100929          §omagmsg=msg(2)  ;
031500100929 4       endif ;
031600100929 3       endif    ;
031700100929 2       endif    ;
031800100929
031900100929         og148=orgde8  ;
032000100929         §omagdes=orgdes   ;
032100100929         §omagfag=orgfag  ;
032200100929         §omaglue=§oglue  ;
032300100929 1       endif ;
032400100929
032500100929         if §OmagERR='E'     ;
032600100929         §omagdes=*all'?'   ;
032700100929         clear §omaglue  ;
032800100929         clear §omagfag  ;
032900100929         endif  ;
033000100929
033100100929         ENDSR  ;
033200080206       //--------------------------------------------------------------
033300080206       //?Operazioni finali.
033400080206       //--------------------------------------------------------------
033500080206       BEGSR RoutEnd;
033600080704
033700080206         *inLR = *on;
033800080704
033900080206         return;
034000080206
034100080206       ENDSR;
034200080206
034300080206      /end-free
034400080206
034500080206       //--------------------------------------------------------------
034600080206       //?Schiere a tempo di compilazione.
034700080206       //--------------------------------------------------------------
034800080206
034900080410** - MSG ------------------------------------------------------------------ -*
035000100929Il magazzino DESTINAZIONE deve essere XXX                                       01
035100100929Magazzino Correlato Errato                                                      02
035200100929Magazzino Destinazione Errato                                                   03
