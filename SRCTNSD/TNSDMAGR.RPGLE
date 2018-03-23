000100080206      //--------------------------------------------------------------
000200100929      //?TNSDMAGR - controllo magazzino correlato / destinazione
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100929     fazorg01l  if   e           k disk
001100100929     fazorg21l  if   e           k disk
001200100929      //---------------------------------------------------------------
001300100929      // - Dati utente
001400100929     d §AzUte        e ds                  extname(AZUTE00F)
001500100929     d                                     dtaara
001600100929     d §DatiUte      e ds                  extname(dDatiUte)
001700100929     d                                     dtaara
001800100929
001900100929      //---------------------------------------------------------------
002000130410     d MSG             s             78    dim(4) ctdata perrcd(1)
002100110225     d magf            s              3    dim(10)
002200100929      //---------------------------------------------------------------
002300080206      // - Reperimento dati utente
002400080206     d TIBS34ds      e ds
002500080206     d dUte01        e ds
002600080702
002700100929     d tnsdmagds     e ds
002800100902     d KPJBA         e ds
002900100929     d og148         e ds
003000080206
003100080206      // - Ricerca/Controllo tabelle
003200080206     d TIBS02ds      e ds                  inz
003300080414
003400100929     d Indx            s              3  0 inz
003500100929     d desmag          s             12
003600100929     d wazorg          s              3    Inz('   ')
003700130410     d Worg21          s              1
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
010900130410       clear magf          ;
011000130410       clear Worg21        ;
011100100929
011200100929        Indx=%scan('?':§magmag )   ;
011300100929
011400100929       // Mag.Destinazione
011500100929       // per MAG --> Filiale e logistica
011600100929       // per FOR --> sia controllo azorg20f  tipi "W" "Y" che azorg00F
011700100929       // Mag.correlato
011800101013       // per MAG --> non controllo, fisso 999 e 935
011900100929       // per FOR --> filiale e logistica
012000100929
012100100929       if   §imagtorg='   ';
012200100929       select   ;
012300100929                when      §imagtmag='D' and §imaginvr='I'     ;
012400100929                eval      wazorg='F  '                  ;
012500100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr=' ' ;
012600100929                eval      wazorg='E  '                  ;
012700100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='F' ;
012800100929                eval      wazorg='S  '                  ;
012900100929                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='R'  ;
013000100929                eval      wazorg='SW '                  ;
013100101013                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='S'  ;
013200101013                eval      wazorg='S  '                  ;
013300120523                when      §imagtmag='D' and §imagcuf ='FOR' and §imaginvr='O'  ;
013400120523                eval      wazorg='F  '                  ;
013500100929
013600100929                when      §imagtmag='C' and §imaginvr='I'   ;
013700100929                eval      wazorg='999'                  ;
013800101013                eval      magf(1)='999'                 ;
013900101013                eval      magf(2)='935'                 ;
014000101203                eval      magf(3)='946'                 ;
014100110225                eval      magf(4)='940'                 ;
014200110324                eval      magf(5)='903'                 ;
014300100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='F' ;
014400100929                eval      wazorg='F  '                  ;
014500100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='R' ;
014600100929                eval      wazorg='SY '                  ;
014700120523                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='O' ;
014800120523                eval      wazorg='SY '                  ;
014900101013                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr='S' ;
015000101013                eval      wazorg='SW '                  ;
015100100929                when      §imagtmag='C' and §imagcuf ='FOR' and §imaginvr=' '  ;
015200100929                eval      wazorg='E  '                  ;
015300100929       ENDSL  ;
015400100929       else  ;
015500101013
015600100929       wazorg=§imagtorg  ;
015700130410       // Esiste anche il codice richiamato 'SD ' --> qualsiasi codice dell'organigramma di
015800130409       //  appoggio
015900101013       if wazorg='999'   ;
016000101013            magf(1)='999'                 ;
016100101013            magf(2)='935'                 ;
016200110225            magf(3)='946'                 ;
016300110225            magf(4)='940'                 ;
016400110324            magf(5)='903'                 ;
016500101013            else  ;
016600101013            magf(1)=wazorg                ;
016700101013       endif  ;
016800101013
016900100929       endif   ;
017000100929
017100100929       // ricerca alfabetica
017200100929       if Indx>0 ;
017300100929       clear pinfil   ;
017400100929       clear pinfag   ;
017500100929       clear pindes   ;
017600100929       callp TNSD20R (pinfil:pinfag:pindes)   ;
017700100929       if pinfil>*zeros  ;
017800100930       orgfil =%int(pinfil)   ;
017900100930       §magmag=pinfil   ;
018000100929       §omagdes=pindes   ;
018100100929       else    ;
018200100929       clear §magmag ;
018300100929       endif  ;
018400100929
018500100929        §omagerr='R'      ;
018600100929       endif  ;
018700100929
018800100929       clear og148  ;
018900100929
019000100929 0     if §omagerr<>'R'  ;
019100100929       // Tipo magazzino da impostare nei messaggi
019200100930         IF §imagTMAG='D'  ;
019300100929         desmag= 'DESTINAZIONE'    ;
019400100929         ELSE  ;
019500100929         desmag= '"CORRELATO" ' ;
019600100929         endif    ;
019700100929
019800100929        if   %checkr(digits:§magmag)>0;
019900100929             §omagerr='E'     ;
020000100929 4           if §imagtmag='D'  ;
020100100929              §omagmsg=msg(3)  ;
020200100929             else  ;
020300100929              §omagmsg=msg(2)  ;
020400100929 4           endif ;
020500100929             leavesr  ;
020600100929        endif  ;
020700100929
020800100929       eval   orgfil=%int(§magmag) ;
020900100929
021000100929 1     if %subst(wazorg:3:1)= ' ' ;
021100100929       chain   orgfil   azorg21l   ;
021200100929 2     if  not %found(azorg21l) ;
021300100929        clear orgfag      ;
021400100929        clear og148       ;
021500130410        Worg21='N'  ;
021600100929       else   ;
021700100929       og148=orgde8  ;
021800100929       endif  ;
021900100929
022000100929 2     select ;
022100130410 2     when  orgfag=' ' and %subst(wazorg:2:1)<>'D' ;
022200100929          §omagerr='E'     ;
022300100929          §omagmsg=msg(01) ;
022400100929          %suBST(§omagmsg:14:12) =desmag         ;
022500100929          %suBST(§omagmsg:39:13) ='un codice 9xx'  ;
022600100929
022700100929 2     when  %subst(wazorg:1:1)= 'F' and orgfag<>'F' and orgfag<>'A'  ;
022800100929          §omagerr='E'      ;
022900100929          §omagmsg=msg(01) ;
023000100929          %suBST(§omagmsg:14:12) =desmag         ;
023100100929          %suBST(§omagmsg:39:11) ='una filiale o Logistica o"951"'  ;
023200100929
023300130410 2     when  %subst(wazorg:2:1)<>' ' and %subst(wazorg:2:1)<>'D'
023400130409                                     and orgfag<> %subst(wazorg:2:1);
023500100929         §omagerr='E'      ;
023600100929         §omagmsg=msg(01) ;
023700100929         %suBST(§omagmsg:14:12) =desmag         ;
023800100929 3       if %subst(wazorg:2:1)='Y'  ;
023900100929         %suBST(§omagmsg:39:12) ='un fornitore' ;
024000100929         else  ;
024100100929         %suBST(§omagmsg:39:17) ='un codice di sede'  ;
024200100929 3       endif  ;
024300100929 x2    other  ;
024400100929       §omagdes=orgdes  ;
024500100929       §omagfag=orgfag  ;
024600100929       §omaglue=§oglue  ;
024700100929 2     endsl    ;
024800100929 1     endif  ;
024900130410
025000130410       // se controllo solo nell'organigramma di appoggio e non esiste erroe
025100130410       if  (Worg21='N' and %subst(wazorg:1:2)= 'SD');
025200130410          §omagerr='E'     ;
025300130410          §omagmsg=msg(04) ;
025400130410       endif  ;
025500100929
025600130410 1     if  §omagerr='E'  and (%subst(wazorg:1:1)= 'F' or
025700130410                             %subst(wazorg:1:1)= 'E ') ;
025800130410
025900100929         chain   orgfil  azorg01l   ;
026000100929         if %found(azorg01l) ;
026100100929         og148=orgde8  ;
026200100929         else ;
026300100929         clear  og148  ;
026400100929         clear orgfag  ;
026500100929         endif ;
026600100929
026700100930 2      if §oglue= 'S' or orgfag='A' or orgfag='F'   ;
026800100929         §omagdes=orgdes  ;
026900100929         §omagfag=orgfag  ;
027000100929         §omaglue=§oglue  ;
027100100929         §omagerr=' '     ;
027200100929         clear §omagmsg   ;
027300100929 x2      else     ;
027400100929         §omagerr='E'      ;
027500100929 3         if %subst(wazorg:2:1)='E'  ;
027600100929
027700100929 4           if §imagtmag='D'  ;
027800100929              §omagmsg=msg(3)  ;
027900100929             else  ;
028000100929              §omagmsg=msg(2)  ;
028100100929 4           endif ;
028200100929 x3        else  ;
028300100929            §omagmsg=msg(01) ;
028400100929            %suBST(§omagmsg:14:12) =desmag         ;
028500100929            %suBST(§omagmsg:39:11) ='una filiale o Logistica o "951"'  ;
028600100929 3         endif    ;
028700100929
028800100929 2     endif   ;
028900100929 1     endif   ;
029000100929
029100100929       // Magazzino specifico
029200100929 1     if %subst(wazorg:3:1)<> ' '  ;
029300101013         Indx=%lookup(§magmag:magf)   ;
029400101013 2       if  Indx=0  ;
029500100929
029600100929         §omagerr='E'      ;
029700100929         §omagmsg=msg(01)   ;
029800100929         %suBST(§omagmsg:14:12) =desmag         ;
029900101013 3       if magf(1)<>*blanks  ;
030000101013         %suBST(§omagmsg:39:3) =magf(1)        ;
030100101013 3       endif  ;
030200101013 3       if magf(2)<>*blanks  ;
030300101013         %suBST(§omagmsg:42:4) =',' + magf(2)        ;
030400101013 3       endif  ;
030500101013 3       if magf(3)<>*blanks  ;
030600101013         %suBST(§omagmsg:46:4) =',' + magf(3)        ;
030700101013 3       endif  ;
030800101013 3       if magf(4)<>*blanks  ;
030900101013         %suBST(§omagmsg:50:4) =',' + magf(4)        ;
031000101013 3       endif                ;
031100101013 3       if magf(5)<>*blanks  ;
031200101013         %suBST(§omagmsg:54:5) =', ...'               ;
031300101013 3       endif  ;
031400100929
031500100929 2       endif ;
031600100929 1       endif ;
031700100929 0       endif ;
031800100929
031900100929        // Per ricerca o magazzino specifico chaino azorg pre prendere i dati di output
032000100929 1     if %subst(wazorg:3:1)<> ' '  or §omagerr='R'  ;
032100100929         chain   orgfil   azorg21l   ;
032200100929 2       if  not %found(azorg21l) ;
032300100929         chain   orgfil   azorg01l   ;
032400100929 3       if  not %found(azorg01l) ;
032500100929         clear orgde8      ;
032600100929         §omagerr='E'      ;
032700100929 4       if §imagtmag='D'  ;
032800100929          §omagmsg=msg(3)  ;
032900100929         else  ;
033000100929          §omagmsg=msg(2)  ;
033100100929 4       endif ;
033200100929 3       endif    ;
033300100929 2       endif    ;
033400100929
033500100929         og148=orgde8  ;
033600100929         §omagdes=orgdes   ;
033700100929         §omagfag=orgfag  ;
033800100929         §omaglue=§oglue  ;
033900100929 1       endif ;
034000100929
034100100929         if §OmagERR='E'     ;
034200100929         §omagdes=*all'?'   ;
034300100929         clear §omaglue  ;
034400100929         clear §omagfag  ;
034500100929         endif  ;
034600100929
034700100929         ENDSR  ;
034800080206       //--------------------------------------------------------------
034900080206       //?Operazioni finali.
035000080206       //--------------------------------------------------------------
035100080206       BEGSR RoutEnd;
035200080704
035300080206         *inLR = *on;
035400080704
035500080206         return;
035600080206
035700080206       ENDSR;
035800080206
035900080206      /end-free
036000080206
036100080206       //--------------------------------------------------------------
036200080206       //?Schiere a tempo di compilazione.
036300080206       //--------------------------------------------------------------
036400080206
036500080410** - MSG ------------------------------------------------------------------ -*
036600100929Il magazzino DESTINAZIONE deve essere XXX                                       01
036700100929Magazzino Correlato Errato                                                      02
036800100929Magazzino Destinazione Errato                                                   03
036900130410Immettere una ditta/magazzino solo di SEDE                                      04
