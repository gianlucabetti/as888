000100031205      *------------------------------------------------------------------------*
000200081022      * Stampa informazioni commerciali
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600081022     ftntbe01l  if   e           k Disk
000700081022     fticpi01l  if   e           k Disk
000800081022     ftncpo01l  if   e           k Disk
000900081022     FTRMK51P   O    E             PRINTER
001000080930      *
001100081022     d ifo             s              3    dim(50)
001200081022     d ifods           s             70    dim(50)
001300081022     d ifocg           s              2    dim(50)
001400081022     d ifoor           s              8    dim(50)
001500081022     d cmp             s             26    dim(5)
001600081022     d sot             s              1    dim(5)
001700081022     d
001800081022     d Ifosott         s              1    inz('0')
001900081022     d WIFS            s              4    inz
002000081022     d Wifo            s              3    inz
002100081022     d XX              s              3  0 inz
002200081022     d CC              s              3  0 inz
002300081022     d savcat          s              2    inz
002400081022     d Wcat            s              2    inz
002500081022     d contaR          s              3  0 inz
002600081022     d Indx            s              3  0 inz
002700081022     d Primavolta      s              1    inz
002800081022     d Esicpo          s              1    inz
002900081022     d WSTariga        s              1    inz
003000081022      //---------------------------------------------------------------
003100081022      //?Definizione aree dati.
003200081022      //---------------------------------------------------------------
003300081022
003400081022      // - Dati utente
003500081022     d §AzUte        e ds                  extname(AZUTE00F)
003600081022     d                                     dtaara
003700081022     d §DatiUte      e ds                  extname(dDatiUte)
003800081022     d                                     dtaara
003900081022     d KPJBA         e ds
004000081022     d TRMK50DS      e ds
004100081022      // - Reperimento dati utente
004200081022     d TIBS34ds      e ds
004300081022     d dUte01        e ds
004400081022     d DLAT          e ds
004500081022     d Difs          e ds
004600081022     d dIFO          e ds                  inz
004700081022     d dIFG          e ds                  inz
004800081022
004900081022      // - Ricerca/Controllo tabelle
005000081022     d TIBS02ds      e ds                  inz
005100081022      //---------------------------------------------------------------
005200081022      //?Definizione procedure usate.
005300081022      //---------------------------------------------------------------
005400081022      /copy gaitrasrc/srcprotopr,tibs02r
005500081022      /copy gaitrasrc/srcprotopr,tibs34r
005600071122      *------------------------------------------------------------------------*
005700081001     c     *entry        plist
005800081022     c                   parm                    kpjba
005900081022     c                   movel     kpjbu         trmk50ds
006000071116      /free
006100071122       *inlr = *on   ;
006200081022
006300081022       // Operazioni iniziali
006400081022       exsr RoutInz;
006500081022
006600081022       // Chaino il potenziale per i dati anagrafici e stampo
006700081022       chain    i50cpo    tncpo01l    ;
006800081023       s1ccpo=i50cpo   ;
006900081023       if %found(tncpo01l)  ;
007000081023       s1crag=cporag   ;
007100081022       s1cvia=cpovia   ;
007200081022       s1ccap=cpocap   ;
007300081022       s1cloc=cpocit   ;
007400081023       s1cprv=cpoprv   ;
007500081022       s1cnaz=cponaz   ;
007600081022       if cpofsf='S'  ;
007700081022       s1dfsf=' SEDE : ' ;
007800081022       else  ;
007900081022       s1dfsf='FILIALE:' ;
008000081022       endif   ;
008100081023       endif   ;
008200081022
008300081022       write mk50t1   ;
008400081022
008500081022       // LETTURA DELLE INFO ORDINATE COME A VIDEO
008600081022       xx=1    ;
008700081022       Indx=1    ;
008800081022       clear cmp   ;
008900081022 1     dow   xx<=50    ;
009000081022 2     if    ifoor(xx)<>*blanks  ;
009100081022
009200081022       Wcat= %subst(ifoor(xx):1:2)   ;
009300081022       Wifo= %subst(ifoor(xx):6:3)   ;
009400081022
009500081022       // cambio di categoria
009600081022 3      if savcat=*blanks or savcat<>wcat               ;
009700081022        savcat=wcat   ;
009800081022        Ifosott=*on   ;
009900081022
010000081022       // Verifico se devo visualizzare la titolo della categoria
010100081022         clear  DIFG  ;
010200081022         reset TIBS02ds;
010300081022         T02sif = knsif;
010400081022         T02cod = 'IFG';
010500081022         T02mod = 'C'  ;
010600081022         T02ke1 = wcat;
010700081022         TNTBE_RicercaControllo  (kpjba : tibs02ds);
010800081022 4       if t02err=*blanks ;
010900081022            difg=t02uni    ;
011000081022 4       endif            ;
011100081022
011200081022       // Se prevista la visualizzazione della categoria
011300081022       //  emetto la riga in videata,
011400081022 4     if §ifgvis='S'    ;
011500081022         EXSR Stampa     ;
011600081022
011700081022         cmp(1)=§ifgdes         ;
011800081022         cmp(1)=%trim(cmp(1))+':'    ;
011900081022         sot(1)=*on        ;
012000081022         if §ifgstr='S'    ;
012100081022         wstariga='S'      ;
012200081022         endif             ;
012300081022         cc=1              ;
012400081022
012500081022         // Se non prevede solo importi a valore la devo
012600081022         //  stampare in riga a parte
012700081022         if §ifgval= ' '   ;
012800081022         exsr STAMPA       ;
012900081022 5       endif               ;
013000081022         Ifosott=*off      ;
013100081022 4     endif               ;
013200081022 3     endif               ;
013300081022
013400081022       Indx=%lookup(wifo:ifo)   ;
013500081022 3     if Indx>0                ;
013600081022         difo=ifods(Indx)         ;
013700081022 4     if §IFOANN=' '           ;
013800081022
013900081022         // conta i campi previsti
014000081022         clear contaR  ;
014100081022         // VALORE
014200081022 5        if     §ifosnv<>' '  ;
014300081022          contaR=contar+1  ;
014400081022 5        endif    ;
014500081022
014600081022         // SPEDIZIONI
014700081022          if     §ifosns<>' '  ;
014800081022          contaR=contar+1  ;
014900081022          endif    ;
015000081022
015100081022         // FATTURATO
015200081022          if     §ifosnf<>' '  ;
015300081022          contaR=contar+1  ;
015400081022          endif    ;
015500081022
015600081022         // SI/NO
015700081022 5        if     §ifosnE<>' '  ;
015800081022          contaR=contar+1  ;
015900081022 5        endif    ;
016000081022
016100081022         // Se più di uno lo stampo in riga a parte
016200081022 5       if contaR>1   ;
016300081022           EXSR Stampa ;
016400081022 5       endif    ;
016500081022
016600081022         if CC=0 and §ifgval<>' '  ;
016700081022         cc=1    ;
016800081022         endif   ;
016900081022
017000081022         // Record senza sottotipo
017100081022 5       select    ;
017200081022         when §ifoifs=' '    ;
017300081022            cc=cc+1     ;
017400081022            cmp(cc)=§ifodes         ;
017500081022            if Ifosott=*on    ;
017600081022               cmp(CC)=%trim(cmp(CC))+':'    ;
017700081022               sot(CC)='1'   ;
017800081022               // Riga di separazione delle categorie
017900081022            if §ifgstr='S'    ;
018000081022              wstariga='S'      ;
018100081022            endif    ;
018200081022            endif    ;
018300081022
018400081022            chain    (i50cpo:wifo) ticpi01l  ;
018500081022            EXSR CaricaInfo        ;
018600081022
018700081022         // Record con   sottotipo da caricare tutti
018800081022         when   §ifoifs='S' ;
018900081022            EXSR CARCon    ;
019000081022
019100081022 5       endsl ;
019200081022
019300081022 4       endif ;
019400081022 3       endif ;
019500081022 2       endif ;
019600081022
019700081022         xx=xx+1   ;
019800081022 1       enddo     ;
019900081022
020000081022         // Stampo ultimo
020100081022           EXSR Stampa     ;
020200081022           write mk50r1      ;
020300081022
020400081022       //--------------------------------------------------------------
020500081022       //?Operazioni iniziali.
020600081022       //--------------------------------------------------------------
020700081022       BEGSR RoutInz;
020800081022
020900081022       // Solo la prima volta
021000081022 1     if primavolta=' '   ;
021100081022
021200081022         // Reperimento dati job
021300081022         exsr DatiJob;
021400081022
021500081022         // Carico tabella info commerciali
021600081022         xx=0                     ;
021700081022         setll ('IFO') tntbe01l   ;
021800081022         READE ('IFO') tntbe01l   ;
021900081022
022000081022 2       dow not %eof(tntbe01l)   ;
022100081022 3       if tbeatb=' '    ;
022200081022           difo=tbeuni    ;
022300081022           xx=xx+1        ;
022400081022           ifo(xx)=tbeke1 ;
022500081022           ifods(xx)=difo ;
022600081022           ifocg(xx)=§ifoifg ;
022700081022           // ordino in base alla categoria e  num.ordinamento
022800081022           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
022900081022 3       endif               ;
023000081022
023100081022         READE ('IFO') tntbe01l   ;
023200081022 2       enddo    ;
023300081022
023400081022         sorta  IFOOR  ;
023500081022
023600081022         Primavolta='N'   ;
023700081022 1       endif      ;
023800081022
023900081022         ENDSR;
024000081022
024100081022       //--------------------------------------------------------------
024200081022       //?Reperimento Dati del job (Utente/Operativi).
024300081022       //--------------------------------------------------------------
024400081022       BEGSR DatiJob;
024500081022
024600081022         in(E) §AzUte;
024700081022         if NOT %error;
024800081022           in(E) §DatiUte;
024900081022         endif;
025000081022         if %error or RSut = *blanks;
025100081022           clear TIBS34ds;
025200081022           tibs34r(tibs34ds);
025300081022           in §AzUte;
025400081022           in §DatiUte;
025500081022         endif;
025600081022
025700081022       ENDSR;
025800081022       //--------------------------------------------------------------
025900081022       //?Carica ogni riga di info commerciale
026000081022       //--------------------------------------------------------------
026100081022         BEGSR CaricaINFO             ;
026200081022
026300081022         clear  Esicpo  ;
026400081022 1       if %found(ticpi01l)    ;
026500081022         Esicpo='S'     ;
026600081022         endif          ;
026700081022
026800081022         // FATTURATO
026900081022 3       if §ifosnf<>' '  ;
027000081022         cc=cc+1    ;
027100081022         cmp(cc)='Fatturato Annuo presunto' ;
027200081022         cc=cc+1    ;
027300081022         sot(CC)='1'   ;
027400081022 4       IF Esicpo='S' and CPIPFT>0      ;
027500081023         cmp(CC)='    ' + %editc(cpipft:'2') +' EUR' ;
027600081022 4       endif ;
027700081022 3       endif ;
027800081022
027900081022         // SPEDIZIONI
028000081022 3       if §ifosnS<>' '  ;
028100081022         cc=cc+1    ;
028200081022         cmp(cc)='Spedizioni Annue Presunte' ;
028300081022         cc=cc+1    ;
028400081022         sot(CC)='1'   ;
028500081022 4       IF Esicpo='S' and CPIsna>0      ;
028600081023         cmp(CC)='    '+%editc(cpisna:'2') +'  ' ;
028700081022 4       endif ;
028800081022 3       endif ;
028900081022
029000081022         // S/N    --> non imposto se ha solo il sottotipo perS/N
029100081022 3       if §ifosnE<>' ' ;
029200081022 3a        if  contaR>1 or §ifoifs=' '   ;
029300081022         cc=cc+1    ;
029400081022         cmp(cc)='(Si/No)                    ' ;
029500081022         sot(CC)='1'   ;
029600081022 4         IF esicpo='S' and CPIFSN='S'     ;
029700081022           cmp(CC)=%trim(cmp(CC)) + '    SI'     ;
029800081022 4         endif ;
029900081022 4         IF esicpo='S' and CPIFSN='N'     ;
030000081022           cmp(CC)=%trim(cmp(CC)) + '    NO'     ;
030100081022 4         endif ;
030200081022
030300081022 3a         else ;
030400081022
030500081022 4          if esicpo='S' and cpifsn='S'  ;
030600081022            %subst(cmp(CC):2:1)='S'   ;
030700081022            sot(CC)='H' ;
030800081022 4          endif;
030900081022 3a        endif ;
031000081022 3       endif ;
031100081022
031200081022         // VALORE
031300081022 3       if §ifosnV<>' '  ;
031400081022         cc=cc+1    ;
031500081022         sot(CC)='1'   ;
031600081022 4       IF esicpo='S' and CPIval>0      ;
031700081022         %subst(cmp(CC):12:6)=%editc(cpival:'2')  ;
031800081022         %subst(cmp(CC):20:4)=cpitva       ;
031900081022         else  ;
032000081022         %subst(cmp(CC):20:4)=§ifotva      ;
032100081022 4       endif ;
032200081022 3       endif ;
032300081022
032400081022         // Se pieni 4 o 5 campi --> stampo
032500081022         if cc>4    ;
032600081022         EXSR STAMPA             ;
032700081022         endif     ;
032800081022
032900081022       ENDSR;
033000081022       //--------------------------------------------------------------
033100081022       //?Stampa info
033200081022       //--------------------------------------------------------------
033300081022       BEGSR STAMPA ;
033400081022       if cmp(1)<>*blanks  or cmp(2)<>*blanks  ;
033500081022
033600081022           campo1=cmp(1)   ;
033700081022           if sot(1)='1'   ;
033800081022           *in21=*on       ;
033900081022           endif           ;
034000081022           campo2=cmp(2)   ;
034100081022           if sot(2)='1'   ;
034200081022           *in22=*on       ;
034300081022           endif           ;
034400081022           if sot(2)='H'   ;
034500081022           *in22=*on       ;
034600081022           *in32=*on       ;
034700081022           endif           ;
034800081022           campo3=cmp(3)   ;
034900081023           if %subst(cmp(3) :26:1)='|'      ;
035000081023              %subst(campo3 :25:1)='|'      ;
035100081023           endif           ;
035200081022           if sot(3)='1'   ;
035300081022           *in23=*on       ;
035400081022           endif           ;
035500081022           if sot(3)='H'   ;
035600081022           *in23=*on       ;
035700081022           *in33=*on       ;
035800081022           endif           ;
035900081022           campo4=cmp(4)   ;
036000081022           if sot(4)='1'   ;
036100081022           *in24=*on       ;
036200081022           endif           ;
036300081022           if sot(4)='H'   ;
036400081022           *in24=*on       ;
036500081022           *in34=*on       ;
036600081022           endif           ;
036700081022
036800081022           campo5=cmp(5)   ;
036900081023           if %subst(cmp(5) :26:1)='|'      ;
037000081023              %subst(campo5 :24:1)='|'      ;
037100081023           endif           ;
037200081022           if sot(5)='1'   ;
037300081022           *in25=*on       ;
037400081022           endif           ;
037500081022           if sot(5)='H'   ;
037600081022           *in25=*on       ;
037700081022           *in35=*on       ;
037800081022           endif           ;
037900081022
038000081022           if wstariga='S'    ;
038100081022             write mk50r1      ;
038200081022             clear wstariga  ;
038300081022           endif             ;
038400081022             write mk50d2   ;
038500081022           endif           ;
038600081022           clear   cmp  ;
038700081022           clear   sot  ;
038800081022           clear   CC   ;
038900081022           *in21=*off   ;
039000081022           *in22=*off   ;
039100081022           *in23=*off   ;
039200081022           *in24=*off   ;
039300081022           *in25=*off   ;
039400081022           *in32=*off   ;
039500081022           *in33=*off   ;
039600081022           *in34=*off   ;
039700081022           *in35=*off   ;
039800081022           clear campo1 ;
039900081022           clear campo2 ;
040000081022           clear campo3 ;
040100081022           clear campo4 ;
040200081022           clear campo5 ;
040300081022       ENDSR;
040400081022       //--------------------------------------------------------------
040500081022       //?Carico con   sottotipo  carica riga per riga
040600081022       //--------------------------------------------------------------
040700081022       BEGSR  CARcon            ;
040800081022
040900081022         tbeke1=wifo    ;
041000081022
041100081022         setll ('IFS':tbeke1) tntbe01l   ;
041200081022         READE ('IFS':tbeke1) tntbe01l   ;
041300081022
041400081022         dow not %eof(tntbe01l)   ;
041500081022         if tbeatb=' '    ;
041600081022         difs=tbeuni      ;
041700081022         wifs=tbeke2      ;
041800081022
041900081022         if CC=0 and §ifgval='S'  ;
042000081023         cmp(1)='         S=si     '     ;
042100081022         cc=1    ;
042200081022         endif   ;
042300081022
042400081022         if CC=0 and §ifgval='V'  ;
042500081022         cc=1    ;
042600081022         endif   ;
042700081022
042800081022         cc=cc+1     ;
042900081022         if contar=1 and §ifosne<>' '  ;
043000081022         cmp(CC)='| |'+§ifsdes ;
043100081022         %subst(cmp(CC):26:1)='|';
043200081022         sot(CC)='1'   ;
043300081022
043400081022         else   ;
043500081022         cmp(cc)=§ifsdes         ;
043600081022         endif   ;
043700081022
043800081022         chain    (i50cpo:wifo:wifs) ticpi01l  ;
043900081022         EXSR CaricaInfo    ;
044000081022         endif   ;
044100081022
044200081022         READE ('IFS':tbeke1) tntbe01l   ;
044300081022         enddo   ;
044400081022
044500081022       ENDSR;
044600081022
044700081001       //-------------------------------------------------------------
044800071116      /end-free
