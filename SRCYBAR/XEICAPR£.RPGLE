000100061024     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200061024
000300061024      *------------------------------------------------------------------------*
000400061024      *  Controllo Codice Fiscale e/o Partita IVA europea
000500061024      *------------------------------------------------------------------------*
000600100122     FTABEL00F  IF   E           K DISK
000700061024
000800061025      *------------------------------------------------------------------------*
000900061025      * campi di work
001000061025      *------------------------------------------------------------------------*
001100061025     d xcodfis         s                   like(xcfivapar)
001200061025     d xivaprv         s                   like(xcfivaprv)
001300080311     d xForzaita       s              1
001400061025     d xstato          s              1  0 inz
001500061025     d wstato          s              1  0 inz
001600080421     d  Wpos           s              2  0
001700100122     d tbwkut          s              1  0 inz(1)
001800100122     d tbwkey          s                   like(tblkey)
001900100122     d tbwcod          s                   like(tblcod)
002000061025
002100061024      *------------------------------------------------------------------------*
002200061025      * ds interne/esterne
002300061024      *------------------------------------------------------------------------*
002400151013     d xcfiva1ds     e ds
002500061026     d  ivaeu                  3      4
002600061026     d  ivait                  5     18
002700151111     d xivaeu        e ds                  inz
002800100122     d DS4I          e ds
002900161020     d
003000161020     d  lung           s              3  0
003100061025
003200061025      *------------------------------------------------------------------------*
003300061025      * pgm richiamati
003400061025      *------------------------------------------------------------------------*
003500061025     d xcdfis          pr                  extpgm('XCDFIS')
003600061025     d  xcodfis                            like(xcfivapar)
003700061025     d  xstato                             like(wstato)
003800061025
003900061025     d xivaeur         pr                  extpgm('XIVAEUR')
004000061025     d  xivaeu                             likeds(xivaeu)
004100061025     d  xivaprv                            like(xcfivaprv)
004200080311     d  xforzaita                     1
004300080421      *------------------------------------------------------------------------*
004400061025      /free
004500080421
004600080421       exsr  INIZIO;
004700061025
004800061025       // controllo validità tipo lancio
004900061025       if xcfivatla <> 'C' and xcfivatla <> 'L' and xcfivatla <> *blanks;
005000061025       clear xcfivatla;
005100061025       endif;
005200061024
005300061025       // se tipo lancio 'C' --> solo chiusura file non elaboro
005400061025       if xcfivatla <> 'C';
005500061025       exsr sr_elabora;
005600061025       endif;
005700061024
005800061025       if xcfivatla <> *blanks;
005900061025       *inlr = *on;
006000061025       else;
006100061025       *inrt = *on;
006200061025       endif;
006300061025
006400061025       // ------------------------------------------------------------------------
006500061025       // routine elabora
006600061025       // ------------------------------------------------------------------------
006700061025       begsr sr_elabora;
006800161020       clear xivaeu  ;
006900061025
0070000804211      if xcfivamod = 'C' or xcfivamod = *blanks;
007100080421         exsr sr_ctrcf;
0072000903032       if xstato < 0;
0073000804213        if xcfivamod = 'C';
007400080421           xcfivaflg = xstato;
007500090303           clear xcfivamsg   ;
007600080421           xcfivamsg = 'Codice fiscale formalmente errato';
007700080421           leavesr;
0078000804213        endif;
007900080421         exsr sr_ctriva;
008000090303           if ivaflg < 0;
008100090303           clear xcfivamsg   ;
008200090303           xcfivamsg = 'Codice fiscale formalmente errato';
0083000903032          endif;
0084000903032       endif;
0085000804211      endif;
008600061025
008700061025       if xcfivamod = 'P';
008800061025       exsr sr_ctriva;
008900061025       endif;
009000061025
009100061025       endsr;
009200061025
009300061025       // ------------------------------------------------------------------------
009400061025       // routine controllo codice fiscale
009500061025       // ------------------------------------------------------------------------
009600061025       begsr sr_ctrcf;
009700080421
009800080421       clear xstato;
009900080421
010000080421        // Per Nazione estera --> non ci vuole codice fiscale
010100080421
010200080421 1       if xcfivanar<>*blanks ;
010300080421
010400080421 2        if xcfivapar<>*blanks ;
010500080421          xcfivamsg = 'Codice fiscale non ammesso per nazione estera' ;
010600080421          xcfivaflg = -2    ;
010700080421 x2       else ;
010800080421          // Imposto xcfivaflg=9 per indicare --> non ci vuole
010900080421          xcfivaflg =  9    ;
011000080421 2        endif  ;
011100080421
011200080421 1       endif  ;
011300100122
011400100122
011500100122        // Per codice iso estero di par.IVA --> non ci vuole codice fiscale
011600100122
011700151013 1       if %subst(xcfivapiva:1:2)<>*blanks  and
011800151013            %subst(xcfivapiva:1:2)<*zeros   ;
011900100122
012000100122           clear tbwkey  ;
012100100122           tbwcod = '4I'                     ;
012200151013           tbwkey = %subst(xcfivapiva:1:2)  ;
012300100122          Chain  (tbwkut:tbwcod:tbwkey)   Tabel00f ;
012400100122          If        %Found(Tabel00f)     ;
012500100122           ds4i = tbluni   ;
012600100122            if        §4iita<>'I'        ;
012700100122             // Imposto xcfivaflg=9 per indicare --> non ci vuole
012800100122 2        if xcfivapar<>*blanks ;
012900100122          xcfivamsg = 'Codice fiscale non ammesso per partita IVA estera' ;
013000100122          xcfivaflg = -2    ;
013100100122 x2       else ;
013200100122          // Imposto xcfivaflg=9 per indicare --> non ci vuole
013300100122          xcfivaflg =  9    ;
013400100122 2        endif  ;
013500100122 2          endif  ;
013600100122 2        endif  ;
013700100122 2       endif  ;
013800080421
013900080421        // Per San Marino --> ovvero partita iva che ha SM -->
014000080421        //                        non ci vuole codice fiscale
014100080421
014200080421 1       if xcfivanar=*blanks and (xcfivaprv='RN' or xcfivaprv='FO' or
014300151103                                   xcfivaprv='FC')  ;
014400151104 2         if %subst(xcfivacap:1:4)='4789' ;
014500151104
014600151104           //clear wpos    ;
014700151104           //wpos = %scan('SM':xcfivapiva)  ;
014800080421
014900151104 3         if xcfivapar<>*blanks;
015000080421            xcfivamsg='La Repubblica di San Marino NON PREVEDE codice fiscale' ;
015100080421            xcfivaflg = -2    ;
015200151104 3         endif ;
015300080421
015400080421          // Imposto xcfivaflg=9 per indicare --> non ci vuole
015500151104 3        if xcfivapar= *blanks;
015600080421            xcfivaflg =  9    ;
015700151104 3        endif  ;
015800151104 2        endif  ;
015900080421 1        endif  ;
016000080421
016100151013        // Per Città del Vaticano --> non ci vuole nè cod fiscale nè partita iva
016200151013
016300151103 1       if xcfivanar=*blanks and (xcfivaprv='RM' and xcfivacap='00120    ')   ;
016400151014            xcfivaese = 9     ;
016500151014 2         if xcfivapar<>*blanks;
016600151014            xcfivamsg='Lo stato del Vaticano NON PREVEDE codice fiscale' ;
016700151014            xcfivaflg = -2    ;
016800151014 2         endif ;
016900151014
017000151014          // Imposto xcfivaflg=9 per indicare --> non ci vuole
017100151014 2        if xcfivapar= *blanks;
017200151014            xcfivaflg =  9    ;
017300151014 2        endif  ;
017400151014 1        endif  ;
017500061025
017600080421 1     if  xcfivapar<>*blanks  and xcfivaflg=0;
017700080421         xcodfis = xcfivapar;
017800080421         xcdfis(xcodfis:xstato);
017900080421 1     endif;
018000061025
018100061025       endsr;
018200061025
018300061025       // ------------------------------------------------------------------------
018400061025       // routine controllo partita iva
018500061025       // ------------------------------------------------------------------------
018600061025       begsr sr_ctriva;
018700061025
018800151104        // Per Città del Vaticano --> non ci vuole nè cod fiscale nè p rtita iva
018900151103 1       if xcfivanar=*blanks and (xcfivaprv='RM' and xcfivacap='00120    ') ;
019000151014            xcfivaese = 9     ;
019100151014
019200151014 2         if  xcfivapar<>*blanks;
019300151014            xcfivamsg='Lo stato del Vaticano NON PREVEDE partita IVA' ;
019400151014            xcfivaflg = -2    ;
019500151014 2         endif ;
019600151014
019700151014          // Imposto xcfivaflg=9 per indicare --> non ci vuole
019800151014 2        if xcfivapar= *blanks;
019900151014            xcfivaflg =  9    ;
020000151014 2        endif  ;
020100151014 1        endif  ;
020200151014
020300161020         // per fare i controlli di san Marino, verifico che la lunghezza
020400161020         //  corrisponda effettivamente alla P.iva di S.M.
020500151104 1       if     %scan('SM':xcfivapar)>0 ;
020600161020
020700161020           clear tbwkey  ;
020800161020           clear ds4I    ;
020900161020           §4ichr=5   ;
021000161020           tbwcod = '4I'                     ;
021100161020           tbwkey = 'SM      '              ;
021200161020          Chain  (tbwkut:tbwcod:tbwkey)   Tabel00f ;
021300161020 2        If        %Found(Tabel00f)     ;
021400161020           ds4i = tbluni   ;
021500161020 2        endif    ;
021600161020
021700161020          lung=%checkr(' ':xcfivapar)  ;
021800161020 2        if lung=0;
021900161020          lung=14  ;
022000161020          else  ;
022100161020          lung=lung-2  ;
022200161020 2        endif  ;
022300151104
022400151104         // Per san Marino o vuota o SM
022500151104 2        if xcfivanar<>*blanks or
022600151104               %subst(xcfivacap:1:4)<>'4789'  ;
022700161020   3        if lung=§4ichr or lung=§4ich2 or lung=§4ich3  ;
022800151104            xcfivamsg='La partita iva "SM" è valida solo per San Marino' ;
022900151104            xcfivaflg = -2    ;
023000161020   3        endif ;
023100161020 2        endif ;
023200151104
023300151104           // non accetto SM0000
023400151104           if xcfivapar='SM00000      ' ;
023500151104            xcfivamsg='Partita iva non valida  per San Marino' ;
023600151104            xcfivaflg = -2    ;
023700151104 2        endif ;
023800151104
023900161020 x1      else  ;
024000151104 2        if xcfivanar= *blanks and xcfivapar<>*blanks   and
024100151104              %subst(xcfivacap:1:4)= '4789'  ;
024200151104            xcfivamsg='Per San Marino inserire partita iva "SM" ' ;
024300151104            xcfivaflg = -2    ;
024400151104 2        endif ;
024500151104 1       endif ;
024600151014
024700151104         // solo pe san Marino e cliente occasionale, posso accettare
024800151104         //  la partita ia vuota
024900151104           if xcfivapar =*blanks and xcfivanar=*blanks and
025000151104              xcfivap88='8' and
025100151104             %subst(xcfivacap:1:4)='4789'  ;
025200151104            xcfivaflg =  9    ;
025300151104           endif  ;
025400151104
025500151014 1     if  xcfivapar<>*blanks  and xcfivaflg=0;
025600151014
025700061025       clear xivaeu;
025800061027       if xcfivamod = *blanks;
025900061027       ivapar = xcfivapar;
026000061027       else;
026100061026       ivapar = ivait;
026200061026       ivasta = ivaeu;
026300061027       endif;
026400080311
026500061025       ivanar = xcfivanar;
026600061025       xivaprv = xcfivaprv;
026700151014       xforzaita= xcfivafori;
026800080311       xivaeur(xivaeu:xivaprv:xforzaita);
026900061025       if ivaflg < 0;
027000061025       xcfivaflg = ivaflg;
027100080311       clear xcfivamsg   ;
027200061025       xcfivamsg = ivamsg;
027300061025       leavesr;
027400061025       endif;
027500061106       ivaeu = ivasta;
027600061106       ivait = ivapar;
027700061025
027800151014       endif;
027900061025       endsr;
028000061024
028100061025       // ------------------------------------------------------------------------
028200061025       // routine iniziale
028300061025       // ------------------------------------------------------------------------
028400080421         begsr inizio;
028500061024
028600061025      /end-free
028700061025
028800061024     c     *entry        plist
028900151013     c                   parm                    xcfiva1ds
029000080421
029100080421     c                   clear                   xcfivaflg
029200151014     c                   clear                   xcfivaese
029300080421     c                   clear                   xcfivamsg
029400080421     c
029500061024     c                   endsr
