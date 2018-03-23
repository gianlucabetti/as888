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
002700061025     d xivaeu        e ds
002800100122     d DS4I          e ds
002900061025
003000061025      *------------------------------------------------------------------------*
003100061025      * pgm richiamati
003200061025      *------------------------------------------------------------------------*
003300061025     d xcdfis          pr                  extpgm('XCDFIS')
003400061025     d  xcodfis                            like(xcfivapar)
003500061025     d  xstato                             like(wstato)
003600061025
003700061025     d xivaeur         pr                  extpgm('XIVAEUR')
003800061025     d  xivaeu                             likeds(xivaeu)
003900061025     d  xivaprv                            like(xcfivaprv)
004000080311     d  xforzaita                     1
004100080421      *------------------------------------------------------------------------*
004200061025      /free
004300080421
004400080421       exsr  INIZIO;
004500061025
004600061025       // controllo validità tipo lancio
004700061025       if xcfivatla <> 'C' and xcfivatla <> 'L' and xcfivatla <> *blanks;
004800061025       clear xcfivatla;
004900061025       endif;
005000061024
005100061025       // se tipo lancio 'C' --> solo chiusura file non elaboro
005200061025       if xcfivatla <> 'C';
005300061025       exsr sr_elabora;
005400061025       endif;
005500061024
005600061025       if xcfivatla <> *blanks;
005700061025       *inlr = *on;
005800061025       else;
005900061025       *inrt = *on;
006000061025       endif;
006100061025
006200061025       // ------------------------------------------------------------------------
006300061025       // routine elabora
006400061025       // ------------------------------------------------------------------------
006500061025       begsr sr_elabora;
006600061025
0067000804211      if xcfivamod = 'C' or xcfivamod = *blanks;
006800080421         exsr sr_ctrcf;
0069000903032       if xstato < 0;
0070000804213        if xcfivamod = 'C';
007100080421           xcfivaflg = xstato;
007200090303           clear xcfivamsg   ;
007300080421           xcfivamsg = 'Codice fiscale formalmente errato';
007400080421           leavesr;
0075000804213        endif;
007600080421         exsr sr_ctriva;
007700090303           if ivaflg < 0;
007800090303           clear xcfivamsg   ;
007900090303           xcfivamsg = 'Codice fiscale formalmente errato';
0080000903032          endif;
0081000903032       endif;
0082000804211      endif;
008300061025
008400061025       if xcfivamod = 'P';
008500061025       exsr sr_ctriva;
008600061025       endif;
008700061025
008800061025       endsr;
008900061025
009000061025       // ------------------------------------------------------------------------
009100061025       // routine controllo codice fiscale
009200061025       // ------------------------------------------------------------------------
009300061025       begsr sr_ctrcf;
009400080421
009500080421       clear xstato;
009600080421
009700080421        // Per Nazione estera --> non ci vuole codice fiscale
009800080421
009900080421 1       if xcfivanar<>*blanks ;
010000080421
010100080421 2        if xcfivapar<>*blanks ;
010200080421          xcfivamsg = 'Codice fiscale non ammesso per nazione estera' ;
010300080421          xcfivaflg = -2    ;
010400080421 x2       else ;
010500080421          // Imposto xcfivaflg=9 per indicare --> non ci vuole
010600080421          xcfivaflg =  9    ;
010700080421 2        endif  ;
010800080421
010900080421 1       endif  ;
011000100122
011100100122
011200100122        // Per codice iso estero di par.IVA --> non ci vuole codice fiscale
011300100122
011400151013 1       if %subst(xcfivapiva:1:2)<>*blanks  and
011500151013            %subst(xcfivapiva:1:2)<*zeros   ;
011600100122
011700100122           clear tbwkey  ;
011800100122           tbwcod = '4I'                     ;
011900151013           tbwkey = %subst(xcfivapiva:1:2)  ;
012000100122          Chain  (tbwkut:tbwcod:tbwkey)   Tabel00f ;
012100100122          If        %Found(Tabel00f)     ;
012200100122           ds4i = tbluni   ;
012300100122            if        §4iita<>'I'        ;
012400100122             // Imposto xcfivaflg=9 per indicare --> non ci vuole
012500100122 2        if xcfivapar<>*blanks ;
012600100122          xcfivamsg = 'Codice fiscale non ammesso per partita IVA estera' ;
012700100122          xcfivaflg = -2    ;
012800100122 x2       else ;
012900100122          // Imposto xcfivaflg=9 per indicare --> non ci vuole
013000100122          xcfivaflg =  9    ;
013100100122 2        endif  ;
013200100122 2          endif  ;
013300100122 2        endif  ;
013400100122 2       endif  ;
013500080421
013600080421        // Per San Marino --> ovvero partita iva che ha SM -->
013700080421        //                        non ci vuole codice fiscale
013800080421
013900080421 1       if xcfivanar=*blanks and (xcfivaprv='RN' or xcfivaprv='FO' or
014000151103                                   xcfivaprv='FC')  ;
014100151104 2         if %subst(xcfivacap:1:4)='4789' ;
014200151104
014300151104           //clear wpos    ;
014400151104           //wpos = %scan('SM':xcfivapiva)  ;
014500080421
014600151104 3         if xcfivapar<>*blanks;
014700080421            xcfivamsg='La Repubblica di San Marino NON PREVEDE codice fiscale' ;
014800080421            xcfivaflg = -2    ;
014900151104 3         endif ;
015000080421
015100080421          // Imposto xcfivaflg=9 per indicare --> non ci vuole
015200151104 3        if xcfivapar= *blanks;
015300080421            xcfivaflg =  9    ;
015400151104 3        endif  ;
015500151104 2        endif  ;
015600080421 1        endif  ;
015700080421
015800151013        // Per Città del Vaticano --> non ci vuole nè cod fiscale nè partita iva
015900151013
016000151103 1       if xcfivanar=*blanks and (xcfivaprv='RM' and xcfivacap='00120    ')   ;
016100151014            xcfivaese = 9     ;
016200151014 2         if xcfivapar<>*blanks;
016300151014            xcfivamsg='Lo stato del Vaticano NON PREVEDE codice fiscale' ;
016400151014            xcfivaflg = -2    ;
016500151014 2         endif ;
016600151014
016700151014          // Imposto xcfivaflg=9 per indicare --> non ci vuole
016800151014 2        if xcfivapar= *blanks;
016900151014            xcfivaflg =  9    ;
017000151014 2        endif  ;
017100151014 1        endif  ;
017200061025
017300080421 1     if  xcfivapar<>*blanks  and xcfivaflg=0;
017400080421         xcodfis = xcfivapar;
017500080421         xcdfis(xcodfis:xstato);
017600080421 1     endif;
017700061025
017800061025       endsr;
017900061025
018000061025       // ------------------------------------------------------------------------
018100061025       // routine controllo partita iva
018200061025       // ------------------------------------------------------------------------
018300061025       begsr sr_ctriva;
018400061025
018500151104        // Per Città del Vaticano --> non ci vuole nè cod fiscale nè p rtita iva
018600151103 1       if xcfivanar=*blanks and (xcfivaprv='RM' and xcfivacap='00120    ') ;
018700151014            xcfivaese = 9     ;
018800151014
018900151014 2         if  xcfivapar<>*blanks;
019000151014            xcfivamsg='Lo stato del Vaticano NON PREVEDE partita IVA' ;
019100151014            xcfivaflg = -2    ;
019200151014 2         endif ;
019300151014
019400151014          // Imposto xcfivaflg=9 per indicare --> non ci vuole
019500151014 2        if xcfivapar= *blanks;
019600151014            xcfivaflg =  9    ;
019700151014 2        endif  ;
019800151014 1        endif  ;
019900151014
020000151104 1       if     %scan('SM':xcfivapar)>0 ;
020100151104
020200151104         // Per san Marino o vuota o SM
020300151104 2        if xcfivanar<>*blanks or
020400151104               %subst(xcfivacap:1:4)<>'4789'  ;
020500151104            xcfivamsg='La partita iva "SM" è valida solo per San Marino' ;
020600151104            xcfivaflg = -2    ;
020700151104 2        endif ;
020800151104
020900151104           // non accetto SM0000
021000151104           if xcfivapar='SM00000      ' ;
021100151104            xcfivamsg='Partita iva non valida  per San Marino' ;
021200151104            xcfivaflg = -2    ;
021300151104 2        endif ;
021400151104
021500151104         else  ;
021600151104 2        if xcfivanar= *blanks and xcfivapar<>*blanks   and
021700151104              %subst(xcfivacap:1:4)= '4789'  ;
021800151104            xcfivamsg='Per San Marino inserire partita iva "SM" ' ;
021900151104            xcfivaflg = -2    ;
022000151104 2        endif ;
022100151104 1       endif ;
022200151014
022300151104         // solo pe san Marino e cliente occasionale, posso accettare
022400151104         //  la partita ia vuota
022500151104           if xcfivapar =*blanks and xcfivanar=*blanks and
022600151104              xcfivap88='8' and
022700151104             %subst(xcfivacap:1:4)='4789'  ;
022800151104            xcfivaflg =  9    ;
022900151104           endif  ;
023000151104
023100151014 1     if  xcfivapar<>*blanks  and xcfivaflg=0;
023200151014
023300061025       clear xivaeu;
023400061027       if xcfivamod = *blanks;
023500061027       ivapar = xcfivapar;
023600061027       else;
023700061026       ivapar = ivait;
023800061026       ivasta = ivaeu;
023900061027       endif;
024000080311
024100061025       ivanar = xcfivanar;
024200061025       xivaprv = xcfivaprv;
024300151014       xforzaita= xcfivafori;
024400080311       xivaeur(xivaeu:xivaprv:xforzaita);
024500061025       if ivaflg < 0;
024600061025       xcfivaflg = ivaflg;
024700080311       clear xcfivamsg   ;
024800061025       xcfivamsg = ivamsg;
024900061025       leavesr;
025000061025       endif;
025100061106       ivaeu = ivasta;
025200061106       ivait = ivapar;
025300061025
025400151014       endif;
025500061025       endsr;
025600061024
025700061025       // ------------------------------------------------------------------------
025800061025       // routine iniziale
025900061025       // ------------------------------------------------------------------------
026000080421         begsr inizio;
026100061024
026200061025      /end-free
026300061025
026400061024     c     *entry        plist
026500151013     c                   parm                    xcfiva1ds
026600080421
026700080421     c                   clear                   xcfivaflg
026800151014     c                   clear                   xcfivaese
026900080421     c                   clear                   xcfivamsg
027000080421     c
027100061024     c                   endsr
