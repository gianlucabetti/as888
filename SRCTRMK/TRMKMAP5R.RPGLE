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
010100090227 2     if (deicpo1=0  and deikscu=0910874) or
010200090227 2        (deicpo1=0  and deikscu=0910874) ;
010300071217
010400090107           EXSR CercaPot       ;
010500090109
010600090109        // Solo se non ho trovato nel frattempo una sede
010700090109 3         if saltapot=' '     ;
010800090109           // se non ho trovato nulla --> errore
010900090109           if pottutt(1)=0   ;
011000090109           Cnontrov=cnontrov+1   ;
011100090109           else    ;
011200090107
011300090107        // Verifico quanto hanno le info complete e quanti le info parziali
011400081119           xx=1   ;
011500090107           clear contatot ;
011600090107           clear contapar ;
011700090109           clear contaaz  ;
011800090109           clear dataazione;
011900090107
012000090109 4         dow pottutt(xx)>0  ;
012100090109 5         if potinfo(xx)='S'    ;
012200090107           contatot=contatot+1   ;
012300090107           yy=xx      ;
012400090109 x5        else  ;
012500090109 6          if potinfop(xx)='S'   ;
012600090107           contapar=contapar+1   ;
012700090107            zz=xx      ;
012800090109 6          endif   ;
012900090109 5         endif   ;
013000081121
013100081119           xx=xx+1   ;
013200090109 4         enddo              ;
013300081121
013400090109 4         select ;
013500090107           // se solo uno con le info complete --> diventa SEDE
013600090109 4         when  contatot=1   ;
013700090109           chain   pottutt(yy) tncpo01l   ;
013800090109           exsr AggioSEDE    ;
013900090109           CaggioIfo=CaggioIfo+1   ;
014000090109
014100090109           // se solo uno con le info parziali --> diventa SEDE
014200090109 4         when  contapar=1   ;
014300090109           chain   pottutt(zz) tncpo01l   ;
014400090109           exsr AggioSEDE    ;
014500090109           Caggiopar=Caggiopar+1   ;
014600090109
014700090109           // se solo un potenziale trovato    --> diventa SEDE
014800090109  4        when  pottutt(1)>0 and pottutt(2)=0  ;
014900090109           chain   pottutt(1) tncpo01l   ;
015000090109           exsr AggioSEDE    ;
015100090109           Caggiouni=Caggiouni+1   ;
015200090109  x4       other  ;
015300090109
015400090109  5          if contatot>1   ;
015500090107              wusa='I'  ;
015600090109  x5         else   ;
015700090109  6            if contapar>1      ;
015800090107                wusa='P'  ;
015900090109  x6           else   ;
016000090107                  wusa='T'    ;
016100090109  6            endif   ;
016200090109  5          endif   ;
016300090107
016400090109           EXSR CercaAzione ;
016500090109
016600090109           // trovata almeno una azione
016700090109 5         if contaaz>0       ;
016800090109
016900090109           xx= 1    ;
017000090109 6         dow pottutt(xx)>0  ;
017100090109 7           if potazione(xx)>dataazione     ;
017200090109               yy=xx      ;
017300090109               dataazione=potazione(xx)   ;
017400090109 7           endif      ;
017500090109           xx=xx+1   ;
017600090109 6         enddo      ;
017700090109           // Aggiorno
017800090109           chain   pottutt(yy) tncpo01l   ;
017900090109           exsr AggioSEDE    ;
018000090109           Caggioazi=Caggioazi+1   ;
018100090109
018200090109 x5        else    ;
018300090109
018400090109           // aggiorno il potenzale legato al cliente unificante
018500090109           //  sse non ha il duns
018600090109          wksc=d10cop ;
018700090109          chain   (1:dutkci:wksc)  cnaco00f  ;
018800090109 6        if %found(cnaco00f)   ;
018900090109
019000090109            chain  acolib   tncpo01l   ;
019100090109 7           if %found(tncpo01l)  and cpoduns=*blanks and cpofsf='F' ;
019200090109                exsr AggioSEDE    ;
019300090109           Caggiopad=Caggiopad+1   ;
019400090109 x7          else   ;
019500090109             //   altrimenti il primo della lista
019600090109                 chain   pottutt(1) tncpo01l   ;
019700090109                 exsr AggioSEDE    ;
019800090109           Caggiopri=Caggiopri+1   ;
019900090109 7           endif   ;
020000090109 6         endif   ;
020100090109 5         endif   ;
020200090107
020300090109 4         endsl   ;
020400090107
020500090109 3         endif  ;
020600090109 3         endif  ;
020700090109 2         endif  ;
020800071217
020900090107       read    wfdei00F   ;
021000090107 1     enddo ;
021100071218
021200090109       caggiotot=caggioifo+caggiopar+caggiouni+caggioazi
021300090109                 +caggiopri+caggiopad   ;
021400090109
021500090109       except totale      ;
021600090109
021700071121       *inlr=*on         ;
021800090109       //-------------------------------------------------------------
021900090109       //?Aggiorno potenziale in sede
022000090109       //--------------------------------------------------------------
022100090109       BEGSR  AggioSEDE    ;
022200090109           chain    cp1nrr   tncpo00f    ;
022300090109  1        if %found(tncpo00f)   ;
022400090109
022500090109           cpofsf='S'    ;
022600090109             §cpooldfil='F'     ;
022700090109             cporst=dcpo01      ;
022800090116                update tncpofis    ;
022900090109           endif   ;
023000090109       ENDSR    ;
023100071116       //-------------------------------------------------------------
023200081121       //?Cerca i potenziali sede associati  all'unificante
023300081121       //--------------------------------------------------------------
023400090107       BEGSR  CercaPot     ;
023500090109       clear saltapot   ;
023600081119
023700090107       // Prendo codice unificante e carico cod collegati
023800081119           clear tibs10ds ;
023900081119           d10tle='ST'   ;
024000081119           d10paf='F'    ;
024100081215           d10cod=deikscu;
024200081119           callp TIBS10R (tibs10ds)   ;
024300081119
024400081119  1        if d10err<>*blanks   ;
024500081119           // Se errore verifico se è figlio
024600081119           clear tibs10ds ;
024700081119           d10tle='ST'   ;
024800081119           d10paf='P'    ;
024900081215           d10cod=deikscu;
025000081119           callp TIBS10R (tibs10ds)   ;
025100081119
025200081119  2        if d10err<>*blanks   ;
025300081119           // Imposto nella skiera e come padre solo se stesso
025400081215           skc(1)='0000'+%editc(deikscu:'X');
025500081215           d10cop=deikscu                  ;
025600081119  2        endif                ;
025700081119  1        endif ;
025800081119
025900090107       clear pottutt  ;
026000081119       clear potinfo  ;
026100081215       clear potinfop ;
026200090109       clear potazione;
026300081119       clear xx       ;
026400081119       yy=1    ;
026500081119
026600081119 1     dow skc(yy)>*Zeros  ;
026700081119
026800090107       // carico da cnaco i potenziali associati        ;
026900081119       wksc=%int(%subst(skc(yy):5:7)) ;
027000081119       chain   (1:dutkci:wksc)  cnaco00f  ;
027100081119 2     if %found(cnaco00f)   ;
027200081119
027300081119         chain  acolib   tncpo01l   ;
027400081119
027500090109 3       if %found(tncpo01l)                 ;
027600090109
027700090109            exsr MemPot         ;
027800090109 3       endif     ;
027900081119 2     endif     ;
028000081119
028100081119       yy=yy+1   ;
028200081119 1     enddo     ;
028300081119
028400081119       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
028500090109       //  codice unificante, altri potenziali  se non è estero
028600090109
028700081119       wksc=d10cop;
028800081119       chain   (1:dutkci:wksc)  cnclp00f  ;
028900081119 1     if %found(cnclp00f)   ;
029000081119
029100081119       // col sottoconti int fattura
029200081119       chain   (1:dutkci:clpscf)  cnind00f  ;
029300090109 2     if %found(cnind00f)  and indsta=*blanks  ;
029400081119         // con la partita iva
029500090113 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
029600090113                             AND   INDIVA<>'04507990150'  ;
029700081119         setll indiva   tncpo06l  ;
029800081119         reade indiva   tncpo06l  ;
029900081119
030000081119 3         dow not %eof(tncpo06l)   ;
030100090107              exsr Mempot           ;
030200081119
030300081119           reade indiva   tncpo06l  ;
030400081119 3       enddo                    ;
030500081119 2a      endif    ;
030600081119
030700090109         // con il codice fiscale
030800081119 2a      if indcdf<>*blanks       ;
030900081119         setll indcdf   tncpo05l  ;
031000081119         reade indcdf   tncpo05l  ;
031100081119
031200081119 3         dow not %eof(tncpo05l)   ;
031300090107              exsr Mempot          ;
031400081119
031500081119           reade indcdf   tncpo05l  ;
031600081119 3       enddo                    ;
031700081119 2a    endif                  ;
031800090109 2a    endif                  ;
031900081119 2     endif                  ;
032000081119
032100080110       ENDSR  ;
032200081119       //--------------------------------------------------------------
032300081121       //?Memorizza potenziali sede per ogni unificante
032400081119       //--------------------------------------------------------------
032500090107       BEGSR MemPot    ;
032600090109
032700090109         // se nel frattempo ne trovo uno di sede --> non aggiorno
032800090109           if cpofsf='S'      ;
032900090109            saltapot='S'   ;
033000090109           else    ;
033100090109
033200090109         // Elaboro solo i codici senza il duns
033300090109          if cpoduns=*blanks    ;
033400090109
033500081119         // Memorizzo i codici visualizzati per evitare ripetizioni
033600090107         Indx=%lookup(cpocpo:pottutt)   ;
033700081119           if Indx=0    ;
033800081119           xx=xx+1  ;
033900090107           pottutt(xx)=cpocpo   ;
034000081119           dcpo01=cporst  ;
034100081119           potinfo(xx)=§cpoifotot ;
034200081215
034300081215           // Se le info non sono complete, verifico se mancano solo mo
034400081215             if §cpoifotot <>'S'    ;
034500081215                    clear trmk50ds    ;
034600081215                    i50cpo=cpocpo     ;
034700081215                    i50mod='C'        ;
034800081215                    i50obl='S'        ;
034900081215                    callp TRMK50R (kpjba:trmk50ds)  ;
035000081215                     potinfo(xx)=o50ifotot  ;
035100081215                     potinfop(xx)=o50ifomin  ;
035200081215             endif;
035300090109            endif;
035400081215           endif;
035500090109           endif;
035600081119           ENDSR  ;
035700081121
035800090107       //-------------------------------------------------------------
035900090109       //?Cerco per ogni codice potenzialmente valido l'ultima azione comm
036000090107       //--------------------------------------------------------------
036100090109       BEGSR  CercaAzione ;
036200090109       clear contaaz ;
036300090109       xx= 1    ;
036400090109       dow pottutt(XX) >  0  ;
036500090109
036600090109       // solo con le info   ;
036700090109       if (wusa='I'  and potinfo(xx) ='S')   or
036800090109       // solo con le parziali
036900090109          (wusa='P'  and potinfop(xx)='S')   or
037000090109       // tutti
037100090109          (wusa='T' )    ;
037200090109          clear  trmk25ds    ;
037300090109          clear TNCPSds;
037400090109          I25cpo   = pottutt(xx)   ;
037500090109          kpjbu    = trmk25ds;
037600090109          trmk25r ( kpjba : TNCPSds );
037700090109          trmk25ds = kpjbu;
037800090109           if  O25err = *blank;
037900090109             potazione(XX)=cpsdst   ;
038000090109             contaaz =contaaz+1     ;
038100090109           endif     ;
038200090109       endif     ;
038300090109
038400090109       xx=xx+1    ;
038500090109       enddo    ;
038600090109
038700090107       ENDSR   ;
038800081119       //--------------------------------------------------------------
038900081119       //?Reperimento Dati del job (Utente/Operativi).
039000081119       //--------------------------------------------------------------
039100081119       BEGSR DatiJob;
039200081119
039300081119         in(E) §AzUte;
039400081119         if NOT %error;
039500081119           in(E) §DatiUte;
039600081119         endif;
039700081119         if %error or RSut = *blanks;
039800081119           clear TIBS34ds;
039900081119           tibs34r(tibs34ds);
040000081119           in §AzUte;
040100081119           in §DatiUte;
040200081119         endif;
040300081119
040400081119       ENDSR;
040500081119
040600071130
040700071116      /end-free
040800090109     OQSYSPRT   E            TOTALE           02
040900090109     O                                              'TOTale pot aggiornati in '
041000090109     O                                              'SEDE:'
041100090109     OQSYSPRT   E            TOTALE      2
041200090109     O                                              'con una INFO TOT presente'
041300090109     O                       CaggioIFO     2  +   2
041400090109     O                                        +   2 'con una INFO PAR presente'
041500090109     O                       CaggioPAR     2  +   2
041600090109     O                                        +   2 'unico presente'
041700090109     O                       CaggioUNI     2  +   2
041800090109     OQSYSPRT   E            TOTALE      2
041900090109     O                                              'con azione più recente'
042000090109     O                       CaggioAZI     2  +   2
042100090109     O                                        +   2 'primo trovato'
042200090109     O                       Caggiopri     2  +   2
042300090109     O                                        +   2 'del codice unificante'
042400090109     O                       Caggiopad     2  +   2
042500090109     OQSYSPRT   E            TOTALE      2
042600090109     O                                              'non trovato alcun pot'
042700090109     O                       Cnontrov      2  +   2
042800090109     O                                        +   4 'T O T A L E'
042900090109     O                       Caggiotot     2  +   2
