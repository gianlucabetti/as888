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
002100130410     d MSG             s             78    dim(4) ctdata perrcd(1)
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
003800130410     d Worg21          s              1
003900100929
004000100929      // ricerca filiale su organigramma
004100100929     d   pInFIL        S              3
004200100929     d   pInFAG        S              1
004300100929     d   pInDES        S             25
004400100929     d   pInDIT        S              3
004500100929
004600100929     d Digits          c                   const('0123456789')
004700100929
004800080206      //---------------------------------------------------------------
004900080206      //?Definizione procedure usate.
005000080206      //---------------------------------------------------------------
005100080414      /copy gaitrasrc/srcprotopr,tibs34r
005200100909      /copy gaitrasrc/srcprotopr,tnsd20r
005300080206
005400080206      //---------------------------------------------------------------
005500080206      //?M A I N - L I N E
005600080206      //---------------------------------------------------------------
005700080206
005800080627     c     *Entry        plist
005900080206     c                   parm                    KPJBA
006000100929     c                   parm                    tnsdmagds
006100080702
006200080206      /free
006300080206
006400080206       // Operazioni iniziali
006500080206       exsr RoutInz;
006600081215
006700100929       clear §omagmsg  ;
006800100929       clear §omagerr  ;
006900100929       clear §omagdes   ;
007000100929       clear §omagfag   ;
007100100929       clear §omaglue   ;
007200100929
007300100929       exsr ChkMAG  ;
007400080530
007500080206       // Operazioni finali
007600080206       exsr RoutEnd;
007700080206
007800080206       //--------------------------------------------------------------
007900080206       //?Operazioni iniziali.
008000080206       //--------------------------------------------------------------
008100080206       BEGSR RoutInz;
008200080703
008300080206         // Reperimento dati job
008400080206         exsr DatiJob;
008500080703
008600080627         ENDSR;
008700080206
008800080206       //--------------------------------------------------------------
008900080206       //?Reperimento Dati del job (Utente/Operativi).
009000080206       //--------------------------------------------------------------
009100080206       BEGSR DatiJob;
009200080206
009300080206         in(E) §AzUte;
009400080206         if NOT %error;
009500080206           in(E) §DatiUte;
009600080206         endif;
009700080206         if %error or RSut = *blanks;
009800080206           clear TIBS34ds;
009900080206           tibs34r(tibs34ds);
010000080206           in §AzUte;
010100080206           in §DatiUte;
010200080206         endif;
010300080206
010400080206       ENDSR;
010500100929       //---------------------------------------------------------------
010600100929       //?Controllo magazzino
010700100929       //---------------------------------------------------------------
010800100929       BEGSR CHKMAG  ;
010900101013
011000130410       clear magf          ;
011100130410       clear Worg21        ;
011200100929
011300100929        Indx=%scan('?':§magmag )   ;
011400100929
011500100929       // Mag.Destinazione
011600100929       // per MAG --> Filiale e logistica
011700100929       // per FOR --> sia controllo azorg20f  tipi "W" "Y" che azorg00F
011800100929       // Mag.correlato
011900101013       // per MAG --> non controllo, fisso 999 e 935
012000100929       // per FOR --> filiale e logistica
012100100929
012200100929       if   §imagtorg='   ';
012300100929       select   ;
012400100929                when      §imagtmag='D' and §imaginvr='I'     ;
012500100929                eval      wazorg='F  '                  ;
012600100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr=' ' ;
012700100929                eval      wazorg='E  '                  ;
012800100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='F' ;
012900100929                eval      wazorg='S  '                  ;
013000100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='R'  ;
013100100929                eval      wazorg='SW '                  ;
013200101013                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='S'  ;
013300101013                eval      wazorg='S  '                  ;
013400120523                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='O'  ;
013500120523                eval      wazorg='F  '                  ;
013600100929
013700100929                when      §imagtmag='C' and §imaginvr='I'   ;
013800100929                eval      wazorg='999'                  ;
013900101013                eval      magf(1)='999'                 ;
014000101013                eval      magf(2)='935'                 ;
014100101203                eval      magf(3)='946'                 ;
014200110225                eval      magf(4)='940'                 ;
014300110324                eval      magf(5)='903'                 ;
014400100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='F' ;
014500100929                eval      wazorg='F  '                  ;
014600100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='R' ;
014700100929                eval      wazorg='SY '                  ;
014800120523                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='O' ;
014900120523                eval      wazorg='SY '                  ;
015000101013                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='S' ;
015100101013                eval      wazorg='SW '                  ;
015200100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr=' '  ;
015300100929                eval      wazorg='E  '                  ;
015400100929       ENDSL  ;
015500100929       else  ;
015600101013
015700100929       wazorg=§imagtorg  ;
015800130410       // Esiste anche il codice richiamato 'SD ' --> qualsiasi codice dell'organigramma di
015900130409       //  appoggio
016000101013       if wazorg='999'   ;
016100101013            magf(1)='999'                 ;
016200101013            magf(2)='935'                 ;
016300110225            magf(3)='946'                 ;
016400110225            magf(4)='940'                 ;
016500110324            magf(5)='903'                 ;
016600101013            else  ;
016700101013            magf(1)=wazorg                ;
016800101013       endif  ;
016900101013
017000100929       endif   ;
017100100929
017200100929       // ricerca alfabetica
017300100929       if Indx>0 ;
017400100929       clear pinfil   ;
017500100929       clear pinfag   ;
017600100929       clear pindes   ;
017700100929       callp TNSD20R (pinfil:pinfag:pindes)   ;
017800100929       if pinfil>*zeros  ;
017900100930       orgfil =%int(pinfil)   ;
018000100930       §magmag=pinfil   ;
018100100929       §omagdes=pindes   ;
018200100929       else    ;
018300100929       clear §magmag ;
018400100929       endif  ;
018500100929
018600100929        §omagerr='R'      ;
018700100929       endif  ;
018800100929
018900100929       clear og148  ;
019000100929
019100100929 0     if §omagerr<>'R'  ;
019200100929       // Tipo magazzino da impostare nei messaggi
019300100930         IF §imagTMAG='D'  ;
019400100929         desmag= 'DESTINAZIONE'    ;
019500100929         ELSE  ;
019600100929         desmag= '"CORRELATO" ' ;
019700100929         endif    ;
019800100929
019900100929        if   %checkr(digits:§magmag)>0;
020000100929             §omagerr='E'     ;
020100100929 4           if §imagtmag='D'  ;
020200100929              §omagmsg=msg(3)  ;
020300100929             else  ;
020400100929              §omagmsg=msg(2)  ;
020500100929 4           endif ;
020600100929             leavesr  ;
020700100929        endif  ;
020800100929
020900100929       eval   orgfil=%int(§magmag) ;
021000100929
021100100929 1     if %subst(wazorg:3:1)= ' ' ;
021200100929       chain   orgfil   azorg21l   ;
021300100929 2     if  not %found(azorg21l) ;
021400100929        clear orgfag      ;
021500100929        clear og148       ;
021600130410        Worg21='N'  ;
021700100929       else   ;
021800100929       og148=orgde8  ;
021900100929       endif  ;
022000100929
022100100929 2     select ;
022200130410 2     when  orgfag=' ' and %subst(wazorg:2:1)<>'D' ;
022300100929          §omagerr='E'     ;
022400100929          §omagmsg=msg(01) ;
022500100929          %suBST(§omagmsg:14:12) =desmag         ;
022600100929          %suBST(§omagmsg:39:13) ='un codice 9xx'  ;
022700100929
022800100929 2     when  %subst(wazorg:1:1)= 'F' and orgfag<>'F' and orgfag<>'A'  ;
022900100929          §omagerr='E'      ;
023000100929          §omagmsg=msg(01) ;
023100100929          %suBST(§omagmsg:14:12) =desmag         ;
023200100929          %suBST(§omagmsg:39:11) ='una filiale o Logistica o"951"'  ;
023300100929
023400130410 2     when  %subst(wazorg:2:1)<>' ' and %subst(wazorg:2:1)<>'D'
023500130409                                     and orgfag<> %subst(wazorg:2:1);
023600100929         §omagerr='E'      ;
023700100929         §omagmsg=msg(01) ;
023800100929         %suBST(§omagmsg:14:12) =desmag         ;
023900100929 3       if %subst(wazorg:2:1)='Y'  ;
024000100929         %suBST(§omagmsg:39:12) ='un fornitore' ;
024100100929         else  ;
024200100929         %suBST(§omagmsg:39:17) ='un codice di sede'  ;
024300100929 3       endif  ;
024400100929 x2    other  ;
024500100929       §omagdes=orgdes  ;
024600100929       §omagfag=orgfag  ;
024700100929       §omaglue=§oglue  ;
024800100929 2     endsl    ;
024900100929 1     endif  ;
025000130410
025100130410       // se controllo solo nell'organigramma di appoggio e non esiste erroe
025200130410       if  (Worg21='N' and %subst(wazorg:1:2)= 'SD');
025300130410          §omagerr='E'     ;
025400130410          §omagmsg=msg(04) ;
025500130410       endif  ;
025600100929
025700130410 1     if  §omagerr='E'  and (%subst(wazorg:1:1)= 'F' or
025800130410                             %subst(wazorg:1:1)= 'E ') ;
025900130410
026000100929         chain   orgfil  azorg01l   ;
026100100929         if %found(azorg01l) ;
026200100929         og148=orgde8  ;
026300100929         else ;
026400100929         clear  og148  ;
026500100929         clear orgfag  ;
026600100929         endif ;
026700100929
026800100930 2      if §oglue= 'S' or orgfag='A' or orgfag='F'   ;
026900100929         §omagdes=orgdes  ;
027000100929         §omagfag=orgfag  ;
027100100929         §omaglue=§oglue  ;
027200100929         §omagerr=' '     ;
027300100929         clear §omagmsg   ;
027400100929 x2      else     ;
027500100929         §omagerr='E'      ;
027600100929 3         if %subst(wazorg:2:1)='E'  ;
027700100929
027800100929 4           if §imagtmag='D'  ;
027900100929              §omagmsg=msg(3)  ;
028000100929             else  ;
028100100929              §omagmsg=msg(2)  ;
028200100929 4           endif ;
028300100929 x3        else  ;
028400100929            §omagmsg=msg(01) ;
028500100929            %suBST(§omagmsg:14:12) =desmag         ;
028600100929            %suBST(§omagmsg:39:11) ='una filiale o Logistica o "951"'  ;
028700100929 3         endif    ;
028800100929
028900100929 2     endif   ;
029000100929 1     endif   ;
029100100929
029200100929       // Magazzino specifico
029300100929 1     if %subst(wazorg:3:1)<> ' '  ;
029400101013         Indx=%lookup(§magmag:magf)   ;
029500101013 2       if  Indx=0  ;
029600100929
029700100929         §omagerr='E'      ;
029800100929         §omagmsg=msg(01)   ;
029900100929         %suBST(§omagmsg:14:12) =desmag         ;
030000101013 3       if magf(1)<>*blanks  ;
030100101013         %suBST(§omagmsg:39:3) =magf(1)        ;
030200101013 3       endif  ;
030300101013 3       if magf(2)<>*blanks  ;
030400101013         %suBST(§omagmsg:42:4) =',' + magf(2)        ;
030500101013 3       endif  ;
030600101013 3       if magf(3)<>*blanks  ;
030700101013         %suBST(§omagmsg:46:4) =',' + magf(3)        ;
030800101013 3       endif  ;
030900101013 3       if magf(4)<>*blanks  ;
031000101013         %suBST(§omagmsg:50:4) =',' + magf(4)        ;
031100101013 3       endif                ;
031200101013 3       if magf(5)<>*blanks  ;
031300101013         %suBST(§omagmsg:54:5) =', ...'               ;
031400101013 3       endif  ;
031500100929
031600100929 2       endif ;
031700100929 1       endif ;
031800100929 0       endif ;
031900100929
032000100929        // Per ricerca o magazzino specifico chaino azorg pre prendere i dati di output
032100100929 1     if %subst(wazorg:3:1)<> ' '  or §omagerr='R'  ;
032200100929         chain   orgfil   azorg21l   ;
032300100929 2       if  not %found(azorg21l) ;
032400100929         chain   orgfil   azorg01l   ;
032500100929 3       if  not %found(azorg01l) ;
032600100929         clear orgde8      ;
032700100929         §omagerr='E'      ;
032800100929 4       if §imagtmag='D'  ;
032900100929          §omagmsg=msg(3)  ;
033000100929         else  ;
033100100929          §omagmsg=msg(2)  ;
033200100929 4       endif ;
033300100929 3       endif    ;
033400100929 2       endif    ;
033500100929
033600100929         og148=orgde8  ;
033700100929         §omagdes=orgdes   ;
033800100929         §omagfag=orgfag  ;
033900100929         §omaglue=§oglue  ;
034000100929 1       endif ;
034100100929
034200100929         if §OmagERR='E'     ;
034300100929         §omagdes=*all'?'   ;
034400100929         clear §omaglue  ;
034500100929         clear §omagfag  ;
034600100929         endif  ;
034700100929
034800100929         ENDSR  ;
034900080206       //--------------------------------------------------------------
035000080206       //?Operazioni finali.
035100080206       //--------------------------------------------------------------
035200080206       BEGSR RoutEnd;
035300080704
035400080206         *inLR = *on;
035500080704
035600080206         return;
035700080206
035800080206       ENDSR;
035900080206
036000080206      /end-free
036100080206
036200080206       //--------------------------------------------------------------
036300080206       //?Schiere a tempo di compilazione.
036400080206       //--------------------------------------------------------------
036500080206
036600080410** - MSG ------------------------------------------------------------------ -*
036700100929Il magazzino DESTINAZIONE deve essere XXX                                       01
036800100929Magazzino Correlato Errato                                                      02
036900100929Magazzino Destinazione Errato                                                   03
037000130410Immettere una ditta/magazzino solo di SEDE                                      04
