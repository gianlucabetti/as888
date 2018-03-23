000100031205      *------------------------------------------------------------------------*
000200090107      * Trasforma un potenziale in sede per i clienti che non hanno sedi
000300090107      *   collegate
000400031205      *------------------------------------------------------------------------*
000500071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000600031205      *------------------------------------------------------------------------*
000700081215     fazorg01l  if   e           k Disk
000800081215     fTntbe01l  if   e           k Disk
000900080102     ftabel00f  if   e           k Disk
001000090109     fTNcpo01L  if   e           k Disk    infds(cpo01)
001100081119     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
001200081119     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
001300081119     fCNACO00F  if   e           k Disk
001400081119     fCNIND00F  if   e           k Disk
001500081119     fCNCLP00F  if   e           k Disk
001600090109     fTNcpo00F  uf   e             Disk    rename(tncpo000:tncpofis)
001700071129     F
001800090107     FWFDEi00F  iF   E             DISK
001900090109     f
002000090109     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002100031205
002200031205      *------------------------------------------------------------------------*
002300031205      *   C A M P I   D I   L A V O R O
002400031205      *------------------------------------------------------------------------*
002500090109     D CPO01           DS
002600090109     D  cp1NRR               397    400B 0
002700090109
002800090109     d pottutt         s             11  0 dim(150)
002900090109     d potinfo         s              1    dim(150)
003000090109     d potinfop        s              1    dim(150)
003100090109     d potazione       s              8  0 dim(150)
003200081119     D
003300081119     d kcod            s              7
003400081215     d Wfil            s              3  0
003500081215     d Wksc            s              7  0
003600081119     d xx              s              4  0
003700081119     d Indx            s              4  0
003800081119     d yy              s              4  0
003900090109     d zz              s              4  0
004000090109     d contatot        s              5  0
004100090109     d contapar        s              5  0
004200090109     d contaaz         s              5  0
004300090109     d CaggioIFO       s              7  0
004400090109     d CaggioPAR       s              7  0
004500090109     d CaggioPRI       s              7  0
004600090109     d CaggioAZI       s              7  0
004700090109     d CaggioUNI       s              7  0
004800090109     d CaggioPAD       s              7  0
004900090109     d CaggioTOT       s              7  0
005000090109     d Cnontrov        s              7  0
005100090109     d dataazione      s              8  0
005200090109     d saltapot        s              1
005300090109     d wusa            s              1
005400081119     d kut             s              1  0 inz(1)
005500080102     d kkey            s                   like(tblkey)
005600071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
005700031205
005800031205      *------------------------------------------------------------------------*
005900031205      *   D S   I N T E R N E / E S T E R N E
006000031205      *------------------------------------------------------------------------*
006100081215     d TRMK50ds      e ds                  inz
006200081119     d Dcpo01        e ds
006300081119     d Kpjba         e ds
006400081119      // - Ricerca unificante padre
006500081119     d TIBS10ds      e ds                  inz
006600081119     d  skc                   21   5520    dim(500)
006700081119      // - Reperimento dati utente
006800081119     d TIBS34ds      e ds
006900081119      // - Dati utente
007000081119     d §AzUte        e ds                  extname(AZUTE00F)
007100081119     d                                     dtaara
007200081119     d §DatiUte      e ds                  extname(dDatiUte)
007300081119     d                                     dtaara
007400081119
007500090109      // - Ricerca ultimo stato cliente potenziale
007600090109     d TRMK25ds      e ds                  inz
007700090109     d TNCPSds       e ds                  inz  extname(TNCPS00F)
007800071122      *------------------------------------------------------------------------*
007900071122      *   P R O T O T I P I
008000071122      *------------------------------------------------------------------------*
008100081119      /copy gaitrasrc/srcprotopr,tibs10r
008200081119      /copy gaitrasrc/srcprotopr,tibs34r
008300090109      /copy gaitrasrc/srcprotopr,trmk25r
008400090109      /copy gaitrasrc/srcprotopr,trmk50r
008500081119      //---------------------------------------------------------------
008600081119      //?M A I N - L I N E
008700081119      //---------------------------------------------------------------
008800081119     c     *Entry        plist
008900081119     c                   parm                    KPJBA
009000081119
009100081119      /free
009200081119
009300081119         // Reperimento dati job
009400081119         exsr DatiJob;
009500080108
009600081119       setll  *start  wfdei00F  ;
009700081119       read    wfdei000   ;
009800081119 1     dow not %eof(wfdei00F) ;
009900090107
010000090107       // elaboro solo quelli senza pot sede associati
010100090109 2     if deicpo1=0   ;
010200071217
010300090107           EXSR CercaPot       ;
010400090109
010500090109        // Solo se non ho trovato nel frattempo una sede
010600090109 3         if saltapot=' '     ;
010700090109           // se non ho trovato nulla --> errore
010800090109           if pottutt(1)=0   ;
010900090109           Cnontrov=cnontrov+1   ;
011000090109           else    ;
011100090107
011200090107        // Verifico quanto hanno le info complete e quanti le info parziali
011300081119           xx=1   ;
011400090107           clear contatot ;
011500090107           clear contapar ;
011600090109           clear contaaz  ;
011700090109           clear dataazione;
011800090107
011900090109 4         dow pottutt(xx)>0  ;
012000090109 5         if potinfo(xx)='S'    ;
012100090107           contatot=contatot+1   ;
012200090107           yy=xx      ;
012300090109 x5        else  ;
012400090109 6          if potinfop(xx)='S'   ;
012500090107           contapar=contapar+1   ;
012600090107            zz=xx      ;
012700090109 6          endif   ;
012800090109 5         endif   ;
012900081121
013000081119           xx=xx+1   ;
013100090109 4         enddo              ;
013200081121
013300090109 4         select ;
013400090107           // se solo uno con le info complete --> diventa SEDE
013500090109 4         when  contatot=1   ;
013600090109           chain   pottutt(yy) tncpo01l   ;
013700090109           exsr AggioSEDE    ;
013800090109           CaggioIfo=CaggioIfo+1   ;
013900090109
014000090109           // se solo uno con le info parziali --> diventa SEDE
014100090109 4         when  contapar=1   ;
014200090109           chain   pottutt(zz) tncpo01l   ;
014300090109           exsr AggioSEDE    ;
014400090109           Caggiopar=Caggiopar+1   ;
014500090109
014600090109           // se solo un potenziale trovato    --> diventa SEDE
014700090109  4        when  pottutt(1)>0 and pottutt(2)=0  ;
014800090109           chain   pottutt(1) tncpo01l   ;
014900090109           exsr AggioSEDE    ;
015000090109           Caggiouni=Caggiouni+1   ;
015100090109  x4       other  ;
015200090109
015300090109  5          if contatot>1   ;
015400090107              wusa='I'  ;
015500090109  x5         else   ;
015600090109  6            if contapar>1      ;
015700090107                wusa='P'  ;
015800090109  x6           else   ;
015900090107                  wusa='T'    ;
016000090109  6            endif   ;
016100090109  5          endif   ;
016200090107
016300090109           EXSR CercaAzione ;
016400090109
016500090109           // trovata almeno una azione
016600090109 5         if contaaz>0       ;
016700090109
016800090109           xx= 1    ;
016900090109 6         dow pottutt(xx)>0  ;
017000090109 7           if potazione(xx)>dataazione     ;
017100090109               yy=xx      ;
017200090109               dataazione=potazione(xx)   ;
017300090109 7           endif      ;
017400090109           xx=xx+1   ;
017500090109 6         enddo      ;
017600090109           // Aggiorno
017700090109           chain   pottutt(yy) tncpo01l   ;
017800090109           exsr AggioSEDE    ;
017900090109           Caggioazi=Caggioazi+1   ;
018000090109
018100090109 x5        else    ;
018200090109
018300090109           // aggiorno il potenzale legato al cliente unificante
018400090109           //  sse non ha il duns
018500090109          wksc=d10cop ;
018600090109          chain   (1:dutkci:wksc)  cnaco00f  ;
018700090109 6        if %found(cnaco00f)   ;
018800090109
018900090109            chain  acolib   tncpo01l   ;
019000090109 7           if %found(tncpo01l)  and cpoduns=*blanks and cpofsf='F' ;
019100090109                exsr AggioSEDE    ;
019200090109           Caggiopad=Caggiopad+1   ;
019300090109 x7          else   ;
019400090109             //   altrimenti il primo della lista
019500090109                 chain   pottutt(1) tncpo01l   ;
019600090109                 exsr AggioSEDE    ;
019700090109           Caggiopri=Caggiopri+1   ;
019800090109 7           endif   ;
019900090109 6         endif   ;
020000090109 5         endif   ;
020100090107
020200090109 4         endsl   ;
020300090107
020400090109 3         endif  ;
020500090109 3         endif  ;
020600090109 2         endif  ;
020700071217
020800090107       read    wfdei00F   ;
020900090107 1     enddo ;
021000071218
021100090109       caggiotot=caggioifo+caggiopar+caggiouni+caggioazi
021200090109                 +caggiopri+caggiopad   ;
021300090109
021400090109       except totale      ;
021500090109
021600071121       *inlr=*on         ;
021700090109       //-------------------------------------------------------------
021800090109       //?Aggiorno potenziale in sede
021900090109       //--------------------------------------------------------------
022000090109       BEGSR  AggioSEDE    ;
022100090109           chain    cp1nrr   tncpo00f    ;
022200090109  1        if %found(tncpo00f)   ;
022300090109
022400090109           cpofsf='S'    ;
022500090109             §cpooldfil='F'     ;
022600090109             cporst=dcpo01      ;
022700090116                update tncpofis    ;
022800090109           endif   ;
022900090109       ENDSR    ;
023000071116       //-------------------------------------------------------------
023100081121       //?Cerca i potenziali sede associati  all'unificante
023200081121       //--------------------------------------------------------------
023300090107       BEGSR  CercaPot     ;
023400090109       clear saltapot   ;
023500081119
023600090107       // Prendo codice unificante e carico cod collegati
023700081119           clear tibs10ds ;
023800081119           d10tle='ST'   ;
023900081119           d10paf='F'    ;
024000081215           d10cod=deikscu;
024100081119           callp TIBS10R (tibs10ds)   ;
024200081119
024300081119  1        if d10err<>*blanks   ;
024400081119           // Se errore verifico se è figlio
024500081119           clear tibs10ds ;
024600081119           d10tle='ST'   ;
024700081119           d10paf='P'    ;
024800081215           d10cod=deikscu;
024900081119           callp TIBS10R (tibs10ds)   ;
025000081119
025100081119  2        if d10err<>*blanks   ;
025200081119           // Imposto nella skiera e come padre solo se stesso
025300081215           skc(1)='0000'+%editc(deikscu:'X');
025400081215           d10cop=deikscu                  ;
025500081119  2        endif                ;
025600081119  1        endif ;
025700081119
025800090107       clear pottutt  ;
025900081119       clear potinfo  ;
026000081215       clear potinfop ;
026100090109       clear potazione;
026200081119       clear xx       ;
026300081119       yy=1    ;
026400081119
026500081119 1     dow skc(yy)>*Zeros  ;
026600081119
026700090107       // carico da cnaco i potenziali associati        ;
026800081119       wksc=%int(%subst(skc(yy):5:7)) ;
026900081119       chain   (1:dutkci:wksc)  cnaco00f  ;
027000081119 2     if %found(cnaco00f)   ;
027100081119
027200081119         chain  acolib   tncpo01l   ;
027300081119
027400090109 3       if %found(tncpo01l)                 ;
027500090109
027600090109            exsr MemPot         ;
027700090109 3       endif     ;
027800081119 2     endif     ;
027900081119
028000081119       yy=yy+1   ;
028100081119 1     enddo     ;
028200081119
028300081119       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
028400090109       //  codice unificante, altri potenziali  se non è estero
028500090109
028600081119       wksc=d10cop;
028700081119       chain   (1:dutkci:wksc)  cnclp00f  ;
028800081119 1     if %found(cnclp00f)   ;
028900081119
029000081119       // col sottoconti int fattura
029100081119       chain   (1:dutkci:clpscf)  cnind00f  ;
029200090109 2     if %found(cnind00f)  and indsta=*blanks  ;
029300081119         // con la partita iva
029400090113 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
029500090113                             AND   INDIVA<>'04507990150'  ;
029600081119         setll indiva   tncpo06l  ;
029700081119         reade indiva   tncpo06l  ;
029800081119
029900081119 3         dow not %eof(tncpo06l)   ;
030000090107              exsr Mempot           ;
030100081119
030200081119           reade indiva   tncpo06l  ;
030300081119 3       enddo                    ;
030400081119 2a      endif    ;
030500081119
030600090109         // con il codice fiscale
030700081119 2a      if indcdf<>*blanks       ;
030800081119         setll indcdf   tncpo05l  ;
030900081119         reade indcdf   tncpo05l  ;
031000081119
031100081119 3         dow not %eof(tncpo05l)   ;
031200090107              exsr Mempot          ;
031300081119
031400081119           reade indcdf   tncpo05l  ;
031500081119 3       enddo                    ;
031600081119 2a    endif                  ;
031700090109 2a    endif                  ;
031800081119 2     endif                  ;
031900081119
032000080110       ENDSR  ;
032100081119       //--------------------------------------------------------------
032200081121       //?Memorizza potenziali sede per ogni unificante
032300081119       //--------------------------------------------------------------
032400090107       BEGSR MemPot    ;
032500090109
032600090109         // se nel frattempo ne trovo uno di sede --> non aggiorno
032700090109           if cpofsf='S'      ;
032800090109            saltapot='S'   ;
032900090109           else    ;
033000090109
033100090109         // Elaboro solo i codici senza il duns
033200090109          if cpoduns=*blanks    ;
033300090109
033400081119         // Memorizzo i codici visualizzati per evitare ripetizioni
033500090107         Indx=%lookup(cpocpo:pottutt)   ;
033600081119           if Indx=0    ;
033700081119           xx=xx+1  ;
033800090107           pottutt(xx)=cpocpo   ;
033900081119           dcpo01=cporst  ;
034000081119           potinfo(xx)=§cpoifotot ;
034100081215
034200081215           // Se le info non sono complete, verifico se mancano solo mo
034300081215             if §cpoifotot <>'S'    ;
034400081215                    clear trmk50ds    ;
034500081215                    i50cpo=cpocpo     ;
034600081215                    i50mod='C'        ;
034700081215                    i50obl='S'        ;
034800081215                    callp TRMK50R (kpjba:trmk50ds)  ;
034900081215                     potinfo(xx)=o50ifotot  ;
035000081215                     potinfop(xx)=o50ifomin  ;
035100081215             endif;
035200090109            endif;
035300081215           endif;
035400090109           endif;
035500081119           ENDSR  ;
035600081121
035700090107       //-------------------------------------------------------------
035800090109       //?Cerco per ogni codice potenzialmente valido l'ultima azione comm
035900090107       //--------------------------------------------------------------
036000090109       BEGSR  CercaAzione ;
036100090109       clear contaaz ;
036200090109       xx= 1    ;
036300090109       dow pottutt(XX) >  0  ;
036400090109
036500090109       // solo con le info   ;
036600090109       if (wusa='I'  and potinfo(xx) ='S')   or
036700090109       // solo con le parziali
036800090109          (wusa='P'  and potinfop(xx)='S')   or
036900090109       // tutti
037000090109          (wusa='T' )    ;
037100090109          clear  trmk25ds    ;
037200090109          clear TNCPSds;
037300090109          I25cpo   = pottutt(xx)   ;
037400090109          kpjbu    = trmk25ds;
037500090109          trmk25r ( kpjba : TNCPSds );
037600090109          trmk25ds = kpjbu;
037700090109           if  O25err = *blank;
037800090109             potazione(XX)=cpsdst   ;
037900090109             contaaz =contaaz+1     ;
038000090109           endif     ;
038100090109       endif     ;
038200090109
038300090109       xx=xx+1    ;
038400090109       enddo    ;
038500090109
038600090107       ENDSR   ;
038700081119       //--------------------------------------------------------------
038800081119       //?Reperimento Dati del job (Utente/Operativi).
038900081119       //--------------------------------------------------------------
039000081119       BEGSR DatiJob;
039100081119
039200081119         in(E) §AzUte;
039300081119         if NOT %error;
039400081119           in(E) §DatiUte;
039500081119         endif;
039600081119         if %error or RSut = *blanks;
039700081119           clear TIBS34ds;
039800081119           tibs34r(tibs34ds);
039900081119           in §AzUte;
040000081119           in §DatiUte;
040100081119         endif;
040200081119
040300081119       ENDSR;
040400081119
040500071130
040600071116      /end-free
040700090109     OQSYSPRT   E            TOTALE           02
040800090109     O                                              'TOTale pot aggiornati in '
040900090109     O                                              'SEDE:'
041000090109     OQSYSPRT   E            TOTALE      2
041100090109     O                                              'con una INFO TOT presente'
041200090109     O                       CaggioIFO     2  +   2
041300090109     O                                        +   2 'con una INFO PAR presente'
041400090109     O                       CaggioPAR     2  +   2
041500090109     O                                        +   2 'unico presente'
041600090109     O                       CaggioUNI     2  +   2
041700090109     OQSYSPRT   E            TOTALE      2
041800090109     O                                              'con azione più recente'
041900090109     O                       CaggioAZI     2  +   2
042000090109     O                                        +   2 'primo trovato'
042100090109     O                       Caggiopri     2  +   2
042200090109     O                                        +   2 'del codice unificante'
042300090109     O                       Caggiopad     2  +   2
042400090109     OQSYSPRT   E            TOTALE      2
042500090109     O                                              'non trovato alcun pot'
042600090109     O                       Cnontrov      2  +   2
042700090109     O                                        +   4 'T O T A L E'
042800090109     O                       Caggiotot     2  +   2
