000100031205      *------------------------------------------------------------------------*
000200081022      * Stampa informazioni commerciali
000300031205      *------------------------------------------------------------------------*
000400141001      *?  ATTENZIONE!!!!                                                      ?
000500141001      *?  Nella tabella IFO è previsto l'inserimento di un valore decimale    ?
000600141001      *?  sulle INFO, per ora questo flag viene usato SOLO per il ricavo      ?
000700141001      *?  medio spedizione calcolato in AUTOMATICO ed è una INFO non          ?
000800141001      *?  memorizzata sul file.                                               ?
000900141001      *?  In caso di necessità di INFO a valore con i decimali si dovrà       ?
001000141001      *?  variare il file TICPI00F per aggiungere un campo con i decimali.    ?
001100071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
001200031205      *------------------------------------------------------------------------*
001300081022     ftntbe01l  if   e           k Disk
001400081022     fticpi01l  if   e           k Disk
001500081022     ftncpo01l  if   e           k Disk
001600141001     FTRMK51P   O    E             PRINTER oflind(*in99)
001700080930      *
001800081022     d ifo             s              3    dim(50)
001900081022     d ifods           s             70    dim(50)
002000081022     d ifocg           s              2    dim(50)
002100081022     d ifoor           s              8    dim(50)
002200081022     d cmp             s             26    dim(5)
002300081022     d sot             s              1    dim(5)
002400081022     d
002500081022     d Ifosott         s              1    inz('0')
002600081022     d WIFS            s              4    inz
002700081022     d Wifo            s              3    inz
002800081022     d XX              s              3  0 inz
002900081022     d CC              s              3  0 inz
003000081022     d savcat          s              2    inz
003100081022     d Wcat            s              2    inz
003200081022     d contaR          s              3  0 inz
003300081022     d Indx            s              3  0 inz
003400081022     d Primavolta      s              1    inz
003500081022     d Esicpo          s              1    inz
003600081022     d WSTariga        s              1    inz
003700081022      //---------------------------------------------------------------
003800081022      //?Definizione aree dati.
003900081022      //---------------------------------------------------------------
004000081022
004100081022      // - Dati utente
004200081022     d §AzUte        e ds                  extname(AZUTE00F)
004300081022     d                                     dtaara
004400081022     d §DatiUte      e ds                  extname(dDatiUte)
004500081022     d                                     dtaara
004600081022     d KPJBA         e ds
004700081022     d TRMK50DS      e ds
004800081022      // - Reperimento dati utente
004900081022     d TIBS34ds      e ds
005000081022     d dUte01        e ds
005100081022     d DLAT          e ds
005200081022     d Difs          e ds
005300081022     d dIFO          e ds                  inz
005400081022     d dIFG          e ds                  inz
005500081022
005600081022      // - Ricerca/Controllo tabelle
005700081022     d TIBS02ds      e ds                  inz
005800081022      //---------------------------------------------------------------
005900081022      //?Definizione procedure usate.
006000081022      //---------------------------------------------------------------
006100081022      /copy gaitrasrc/srcprotopr,tibs02r
006200081022      /copy gaitrasrc/srcprotopr,tibs34r
006300071122      *------------------------------------------------------------------------*
006400081001     c     *entry        plist
006500081022     c                   parm                    kpjba
006600081022     c                   movel     kpjbu         trmk50ds
006700071116      /free
006800071122       *inlr = *on   ;
006900081022
007000081022       // Operazioni iniziali
007100081022       exsr RoutInz;
007200081022
007300081022       // Chaino il potenziale per i dati anagrafici e stampo
007400081022       chain    i50cpo    tncpo01l    ;
007500081023       s1ccpo=i50cpo   ;
007600081023       if %found(tncpo01l)  ;
007700081023       s1crag=cporag   ;
007800081022       s1cvia=cpovia   ;
007900081022       s1ccap=cpocap   ;
008000081022       s1cloc=cpocit   ;
008100081023       s1cprv=cpoprv   ;
008200081022       s1cnaz=cponaz   ;
008300081022       if cpofsf='S'  ;
008400081022       s1dfsf=' SEDE : ' ;
008500081022       else  ;
008600081022       s1dfsf='FILIALE:' ;
008700081022       endif   ;
008800081023       endif   ;
008900081022
009000081022       write mk50t1   ;
009100081022
009200081022       // LETTURA DELLE INFO ORDINATE COME A VIDEO
009300141001       //?  NON prevedo la stampa del ricavo medio spedizione RMS    ?
009400081022       xx=1    ;
009500081022       Indx=1    ;
009600081022       clear cmp   ;
009700081022 1     dow   xx<=50    ;
009800141001 2     if    ifoor(xx)<>*blanks   and %subst(IFOor(xx):6:3) <> 'RMS';
009900081022
010000081022       Wcat= %subst(ifoor(xx):1:2)   ;
010100081022       Wifo= %subst(ifoor(xx):6:3)   ;
010200081022
010300081022       // cambio di categoria
010400081022 3      if savcat=*blanks or savcat<>wcat               ;
010500081022        savcat=wcat   ;
010600081022        Ifosott=*on   ;
010700081022
010800081022       // Verifico se devo visualizzare la titolo della categoria
010900081022         clear  DIFG  ;
011000081022         reset TIBS02ds;
011100081022         T02sif = knsif;
011200081022         T02cod = 'IFG';
011300081022         T02mod = 'C'  ;
011400081022         T02ke1 = wcat;
011500081022         TNTBE_RicercaControllo  (kpjba : tibs02ds);
011600081022 4       if t02err=*blanks ;
011700081022            difg=t02uni    ;
011800081022 4       endif            ;
011900081022
012000081022       // Se prevista la visualizzazione della categoria
012100081022       //  emetto la riga in videata,
012200141001       // NON lo faccio per spesa totale SPT
012300141001 4     if §ifgvis='S'    and wifo <> 'SPT';
012400081022         EXSR Stampa     ;
012500081022
012600081022         cmp(1)=§ifgdes         ;
012700081022         cmp(1)=%trim(cmp(1))+':'    ;
012800081022         sot(1)=*on        ;
012900081022         if §ifgstr='S'    ;
013000081022         wstariga='S'      ;
013100081022         endif             ;
013200081022         cc=1              ;
013300081022
013400081022         // Se non prevede solo importi a valore la devo
013500081022         //  stampare in riga a parte
013600081022         if §ifgval= ' '   ;
013700081022         exsr STAMPA       ;
013800081022 5       endif               ;
013900081022         Ifosott=*off      ;
014000081022 4     endif               ;
014100081022 3     endif               ;
014200081022
014300081022       Indx=%lookup(wifo:ifo)   ;
014400081022 3     if Indx>0                ;
014500081022         difo=ifods(Indx)         ;
014600081022 4     if §IFOANN=' '           ;
014700081022
014800081022         // conta i campi previsti
014900081022         clear contaR  ;
015000081022         // VALORE
015100081022 5        if     §ifosnv<>' '  ;
015200081022          contaR=contar+1  ;
015300081022 5        endif    ;
015400141001
015500141001         // VALORE DECIMALE
015600141001 5        if     §ifosnvd<>' '  ;
015700141001          contaR=contar+1  ;
015800141001 5        endif    ;
015900081022
016000081022         // SPEDIZIONI
016100081022          if     §ifosns<>' '  ;
016200081022          contaR=contar+1  ;
016300081022          endif    ;
016400081022
016500081022         // FATTURATO
016600081022          if     §ifosnf<>' '  ;
016700081022          contaR=contar+1  ;
016800081022          endif    ;
016900081022
017000081022         // SI/NO
017100081022 5        if     §ifosnE<>' '  ;
017200081022          contaR=contar+1  ;
017300081022 5        endif    ;
017400081022
017500081022         // Se più di uno lo stampo in riga a parte
017600081022 5       if contaR>1   ;
017700081022           EXSR Stampa ;
017800081022 5       endif    ;
017900081022
018000081022         if CC=0 and §ifgval<>' '  ;
018100081022         cc=1    ;
018200081022         endif   ;
018300081022
018400081022         // Record senza sottotipo
018500081022 5       select    ;
018600081022         when §ifoifs=' '    ;
018700081022            cc=cc+1     ;
018800081022            cmp(cc)=§ifodes         ;
018900081022            if Ifosott=*on    ;
019000081022               cmp(CC)=%trim(cmp(CC))+':'    ;
019100081022               sot(CC)='1'   ;
019200081022               // Riga di separazione delle categorie
019300081022            if §ifgstr='S'    ;
019400081022              wstariga='S'      ;
019500081022            endif    ;
019600081022            endif    ;
019700081022
019800081022            chain    (i50cpo:wifo) ticpi01l  ;
019900081022            EXSR CaricaInfo        ;
020000081022
020100081022         // Record con   sottotipo da caricare tutti
020200081022         when   §ifoifs='S' ;
020300081022            EXSR CARCon    ;
020400081022
020500081022 5       endsl ;
020600081022
020700081022 4       endif ;
020800081022 3       endif ;
020900081022 2       endif ;
021000081022
021100081022         xx=xx+1   ;
021200081022 1       enddo     ;
021300081022
021400081022         // Stampo ultimo
021500081022           EXSR Stampa     ;
021600141001           IF  *in99;
021700141001             write mk50t1   ;
021800141001             *in99 = *off;
021900141001           ENDIF;
022000081022           write mk50r1      ;
022100081022
022200081022       //--------------------------------------------------------------
022300081022       //?Operazioni iniziali.
022400081022       //--------------------------------------------------------------
022500081022       BEGSR RoutInz;
022600081022
022700081022       // Solo la prima volta
022800081022 1     if primavolta=' '   ;
022900081022
023000081022         // Reperimento dati job
023100081022         exsr DatiJob;
023200081022
023300081022         // Carico tabella info commerciali
023400081022         xx=0                     ;
023500081022         setll ('IFO') tntbe01l   ;
023600081022         READE ('IFO') tntbe01l   ;
023700081022
023800081022 2       dow not %eof(tntbe01l)   ;
023900081022 3       if tbeatb=' '    ;
024000081022           difo=tbeuni    ;
024100081022           xx=xx+1        ;
024200081022           ifo(xx)=tbeke1 ;
024300081022           ifods(xx)=difo ;
024400081022           ifocg(xx)=§ifoifg ;
024500081022           // ordino in base alla categoria e  num.ordinamento
024600081022           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
024700081022 3       endif               ;
024800081022
024900081022         READE ('IFO') tntbe01l   ;
025000081022 2       enddo    ;
025100081022
025200081022         sorta  IFOOR  ;
025300081022
025400081022         Primavolta='N'   ;
025500081022 1       endif      ;
025600081022
025700081022         ENDSR;
025800081022
025900081022       //--------------------------------------------------------------
026000081022       //?Reperimento Dati del job (Utente/Operativi).
026100081022       //--------------------------------------------------------------
026200081022       BEGSR DatiJob;
026300081022
026400081022         in(E) §AzUte;
026500081022         if NOT %error;
026600081022           in(E) §DatiUte;
026700081022         endif;
026800081022         if %error or RSut = *blanks;
026900081022           clear TIBS34ds;
027000081022           tibs34r(tibs34ds);
027100081022           in §AzUte;
027200081022           in §DatiUte;
027300081022         endif;
027400081022
027500081022       ENDSR;
027600081022       //--------------------------------------------------------------
027700081022       //?Carica ogni riga di info commerciale
027800081022       //--------------------------------------------------------------
027900081022         BEGSR CaricaINFO             ;
028000081022
028100081022         clear  Esicpo  ;
028200081022 1       if %found(ticpi01l)    ;
028300081022         Esicpo='S'     ;
028400081022         endif          ;
028500081022
028600081022         // FATTURATO
028700141001 3       if §ifosnf<>' '  and CPItpf = 'SPT';
028800081022         cc=cc+1    ;
028900081022         cmp(cc)='Fatturato Annuo presunto' ;
029000081022         cc=cc+1    ;
029100081022         sot(CC)='1'   ;
029200081022 4       IF Esicpo='S' and CPIPFT>0      ;
029300081023         cmp(CC)='    ' + %editc(cpipft:'2') +' EUR' ;
029400081022 4       endif ;
029500081022 3       endif ;
029600081022
029700081022         // SPEDIZIONI
029800081022 3       if §ifosnS<>' '  ;
029900081022         cc=cc+1    ;
030000081022         cmp(cc)='Spedizioni Annue Presunte' ;
030100081022         cc=cc+1    ;
030200081022         sot(CC)='1'   ;
030300081022 4       IF Esicpo='S' and CPIsna>0      ;
030400081023         cmp(CC)='    '+%editc(cpisna:'2') +'  ' ;
030500081022 4       endif ;
030600081022 3       endif ;
030700141001
030800141001         // FATTURATO del NON affidato BRT
030900141001 3       if §ifosnf<>' '  and CPItpf <> 'SPT';
031000141001         cc=cc+1    ;
031100141001         sot(CC)='1'   ;
031200141001 4       IF Esicpo='S' and CPIPFT>0      ;
031300141001         cmp(CC)='    ' + %editc(cpipft:'2') +' EUR' ;
031400141001 4       endif ;
031500141001 3       endif ;
031600081022
031700081022         // S/N    --> non imposto se ha solo il sottotipo perS/N
031800081022 3       if §ifosnE<>' ' ;
031900081022 3a        if  contaR>1 or §ifoifs=' '   ;
032000081022         cc=cc+1    ;
032100081022         cmp(cc)='(Si/No)                    ' ;
032200081022         sot(CC)='1'   ;
032300081022 4         IF esicpo='S' and CPIFSN='S'     ;
032400081022           cmp(CC)=%trim(cmp(CC)) + '    SI'     ;
032500081022 4         endif ;
032600081022 4         IF esicpo='S' and CPIFSN='N'     ;
032700081022           cmp(CC)=%trim(cmp(CC)) + '    NO'     ;
032800081022 4         endif ;
032900081022
033000081022 3a         else ;
033100081022
033200081022 4          if esicpo='S' and cpifsn='S'  ;
033300081022            %subst(cmp(CC):2:1)='S'   ;
033400081022            sot(CC)='H' ;
033500081022 4          endif;
033600081022 3a        endif ;
033700081022 3       endif ;
033800081022
033900081022         // VALORE
034000081022 3       if §ifosnV<>' '  ;
034100081022         cc=cc+1    ;
034200081022         sot(CC)='1'   ;
034300081022 4       IF esicpo='S' and CPIval>0      ;
034400081022         %subst(cmp(CC):12:6)=%editc(cpival:'2')  ;
034500081022         %subst(cmp(CC):20:4)=cpitva       ;
034600081022         else  ;
034700081022         %subst(cmp(CC):20:4)=§ifotva      ;
034800081022 4       endif ;
034900081022 3       endif ;
035000141001
035100141001         // VALORE decimale
035200141001 3       if §ifosnVd <>' '  and §IFOsnvd <> 'A';
035300141001         cc=cc+1    ;
035400141001         sot(CC)='1'   ;
035500141001 4       IF esicpo='S' and CPIval>0      ;
035600141001         %subst(cmp(CC):12:6)=%editc(cpival:'2')  ;
035700141001         %subst(cmp(CC):20:4)=cpitva       ;
035800141001         else  ;
035900141001         %subst(cmp(CC):20:4)=§ifotva      ;
036000141001 4       endif ;
036100141001 3       endif ;
036200081022
036300081022         // Se pieni 4 o 5 campi --> stampo
036400081022         if cc>4    ;
036500081022         EXSR STAMPA             ;
036600081022         endif     ;
036700081022
036800081022       ENDSR;
036900081022       //--------------------------------------------------------------
037000081022       //?Stampa info
037100081022       //--------------------------------------------------------------
037200081022       BEGSR STAMPA ;
037300081022       if cmp(1)<>*blanks  or cmp(2)<>*blanks  ;
037400081022
037500081022           campo1=cmp(1)   ;
037600081022           if sot(1)='1'   ;
037700081022           *in21=*on       ;
037800081022           endif           ;
037900081022           campo2=cmp(2)   ;
038000081022           if sot(2)='1'   ;
038100081022           *in22=*on       ;
038200081022           endif           ;
038300081022           if sot(2)='H'   ;
038400081022           *in22=*on       ;
038500081022           *in32=*on       ;
038600081022           endif           ;
038700081022           campo3=cmp(3)   ;
038800081023           if %subst(cmp(3) :26:1)='|'      ;
038900081023              %subst(campo3 :25:1)='|'      ;
039000081023           endif           ;
039100081022           if sot(3)='1'   ;
039200081022           *in23=*on       ;
039300081022           endif           ;
039400081022           if sot(3)='H'   ;
039500081022           *in23=*on       ;
039600081022           *in33=*on       ;
039700081022           endif           ;
039800081022           campo4=cmp(4)   ;
039900081022           if sot(4)='1'   ;
040000081022           *in24=*on       ;
040100081022           endif           ;
040200081022           if sot(4)='H'   ;
040300081022           *in24=*on       ;
040400081022           *in34=*on       ;
040500081022           endif           ;
040600081022
040700081022           campo5=cmp(5)   ;
040800081023           if %subst(cmp(5) :26:1)='|'      ;
040900081023              %subst(campo5 :24:1)='|'      ;
041000081023           endif           ;
041100081022           if sot(5)='1'   ;
041200081022           *in25=*on       ;
041300081022           endif           ;
041400081022           if sot(5)='H'   ;
041500081022           *in25=*on       ;
041600081022           *in35=*on       ;
041700081022           endif           ;
041800141001
041900141001           IF  *in99;
042000141001             write mk50t1   ;
042100141001             *in99 = *off;
042200141001           ENDIF;
042300081022
042400081022           if wstariga='S'    ;
042500081022             write mk50r1      ;
042600081022             clear wstariga  ;
042700081022           endif             ;
042800081022             write mk50d2   ;
042900081022           endif           ;
043000081022           clear   cmp  ;
043100081022           clear   sot  ;
043200081022           clear   CC   ;
043300081022           *in21=*off   ;
043400081022           *in22=*off   ;
043500081022           *in23=*off   ;
043600081022           *in24=*off   ;
043700081022           *in25=*off   ;
043800081022           *in32=*off   ;
043900081022           *in33=*off   ;
044000081022           *in34=*off   ;
044100081022           *in35=*off   ;
044200081022           clear campo1 ;
044300081022           clear campo2 ;
044400081022           clear campo3 ;
044500081022           clear campo4 ;
044600081022           clear campo5 ;
044700081022       ENDSR;
044800081022       //--------------------------------------------------------------
044900081022       //?Carico con   sottotipo  carica riga per riga
045000081022       //--------------------------------------------------------------
045100081022       BEGSR  CARcon            ;
045200081022
045300081022         tbeke1=wifo    ;
045400081022
045500081022         setll ('IFS':tbeke1) tntbe01l   ;
045600081022         READE ('IFS':tbeke1) tntbe01l   ;
045700081022
045800081022         dow not %eof(tntbe01l)   ;
045900081022         if tbeatb=' '    ;
046000081022         difs=tbeuni      ;
046100081022         wifs=tbeke2      ;
046200081022
046300081022         if CC=0 and §ifgval='S'  ;
046400081023         cmp(1)='         S=si     '     ;
046500081022         cc=1    ;
046600081022         endif   ;
046700081022
046800081022         if CC=0 and §ifgval='V'  ;
046900081022         cc=1    ;
047000081022         endif   ;
047100081022
047200081022         cc=cc+1     ;
047300081022         if contar=1 and §ifosne<>' '  ;
047400081022         cmp(CC)='| |'+§ifsdes ;
047500081022         %subst(cmp(CC):26:1)='|';
047600081022         sot(CC)='1'   ;
047700081022
047800081022         else   ;
047900081022         cmp(cc)=§ifsdes         ;
048000081022         endif   ;
048100081022
048200081022         chain    (i50cpo:wifo:wifs) ticpi01l  ;
048300081022         EXSR CaricaInfo    ;
048400081022         endif   ;
048500081022
048600081022         READE ('IFS':tbeke1) tntbe01l   ;
048700081022         enddo   ;
048800081022
048900081022       ENDSR;
049000081022
049100081001       //-------------------------------------------------------------
049200071116      /end-free
