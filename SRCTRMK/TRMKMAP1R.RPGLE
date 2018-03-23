000100031205      *------------------------------------------------------------------------*
000200081120      * totali controlli per mappatura
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090113     fTNcpo01L  if   e           k Disk
000700090113     fTNcpo05L  if   e           k Disk    rename(tncpo000:tncpo005)
000800090113     fTNcpo06L  if   e           k Disk    rename(tncpo000:tncpo006)
000900090113     fCNACO00F  if   e           k Disk
001000090113     fCNIND00F  if   e           k Disk
001100090113     fCNCLP00F  if   e           k Disk
001200071123     fTntbe01l  if   e           k Disk
001300080102     ftabel00f  if   e           k Disk
001400081120     FWFDEi05l  iF   E           k DISK
001500081120     FWFDEiT0F  uF a E           k DISK    prefix(T)
001600031205
001700031205      *------------------------------------------------------------------------*
001800031205      *   C A M P I   D I   L A V O R O
001900031205      *------------------------------------------------------------------------*
002000081120     d cpi             s              1    dim(10)
002100081216     d cpip            s              1    dim(10)
002200090113     D
002300090113     d potFIL          s             11  0 dim(100)
002400090113     d potinfo         s              1    dim(100)
002500090113     d potinfop        s              1    dim(100)
002600090113     D
002700081119     D
002800081119     d xx              s              4  0
002900081119     d yy              s              4  0
003000081120     d conta           s              2  0
003100081216     d contap          s              2  0
003200090113     d Wksc            s              7  0
003300090113     d Indx            s              4  0
003400090113     d AA              s              4  0
003500090113     d BB              s              4  0
003600081120     d
003700081120     d tcli            s                   like(tdeitcli)
003800081120     d noif            s                   like(tdeinoif)
003900081216     d nomoi           s                   like(tdeinomoi)
004000090113     d IFOFI           s                   like(tdeiIFOFI)
004100081120     d nosd            s                   like(tdeinosd)
004200081120     d piusd           s                   like(tdeipiusd)
004300081120     d psdui           s                   like(tdeipsdui)
004400081120     d psdpi           s                   like(tdeipsdpi)
004500081120     d savfil          s                   like(deifil)
004600081120     d savdfil         s                   like(deidfil)
004700081120     d savare          s                   like(deiare)
004800081120     d savdare         s                   like(deidare)
004900081120     d
005000031205
005100031205      *------------------------------------------------------------------------*
005200031205      *   D S   I N T E R N E / E S T E R N E
005300031205      *------------------------------------------------------------------------*
005400090113     d TRMK50ds      e ds                  inz
005500090113     d Dcpo01        e ds
005600081119     d Kpjba         e ds
005700090113      // - Ricerca unificante padre
005800090113     d TIBS10ds      e ds                  inz
005900090113     d  skc                   21   5520    dim(500)
006000090113      // - Reperimento dati utente
006100081119     d TIBS34ds      e ds
006200081119      // - Dati utente
006300081119     d §AzUte        e ds                  extname(AZUTE00F)
006400081119     d                                     dtaara
006500081119     d §DatiUte      e ds                  extname(dDatiUte)
006600081119     d                                     dtaara
006700071122      *------------------------------------------------------------------------*
006800071122      *   P R O T O T I P I
006900071122      *------------------------------------------------------------------------*
007000081119      /copy gaitrasrc/srcprotopr,tibs34r
007100090113      /copy gaitrasrc/srcprotopr,tibs10r
007200090113      /copy gaitrasrc/srcprotopr,trmk50r
007300081119      //---------------------------------------------------------------
007400081119      //?M A I N - L I N E
007500081119      //---------------------------------------------------------------
007600081119     c     *Entry        plist
007700081119     c                   parm                    KPJBA
007800081119
007900081119      /free
008000081119
008100081119         // Reperimento dati job
008200081119         exsr DatiJob;
008300080108
008400081120       setll  *start  wfdeiT0F  ;
008500081120       read    wfdeiT00   ;
008600081120 1     dow not %eof(wfdeiT0F) ;
008700081120       delete  wfdeiT00  ;
008800081120       read    wfdeiT0F   ;
008900080108 1     enddo ;
009000080108
009100081120       setll  *loval  wfdei05l  ;
009200081120       read    wfdei05l ;
009300071217
009400081120 1     dow  not %eof(wfdei05l) ;
009500081120
009600090121       if deifil>0   ;
009700090121
009800090121       // a cambio filiale scrivo record
009900081120 2     if savfil<>deifil   ;
010000081120 3     if savfil> 0   ;
010100081120       exsr Scrivitot ;
010200081120 3     endif          ;
010300081120
010400081120       savfil=deifil  ;
010500081120       savdfil=deidfil  ;
010600081120       savdare=deidare  ;
010700081120       savare=deiare  ;
010800081120 2     endif          ;
010900081120
011000081120       // totali di filiale
011100081120       tcli=tcli+1   ;
011200081120
011300081120       // totali senza sedi
011400081120 2     if deicpo1=0  ;
011500081120        nosd=nosd+1  ;
011600081120        noif=noif+1   ;
011700081120 x2    else           ;
011800081120       cpi(1)=deicpi1  ;
011900081120       cpi(2)=deicpi2  ;
012000081120       cpi(3)=deicpi3  ;
012100081120       cpi(4)=deicpi4  ;
012200081120       cpi(5)=deicpi5  ;
012300081120       cpi(6)=deicpi6  ;
012400081120       cpi(7)=deicpi7  ;
012500081120       cpi(8)=deicpi8  ;
012600081120       cpi(9)=deicpi9  ;
012700081120
012800081216       cpip(1)=deicpi1p  ;
012900081216       cpip(2)=deicpi2p  ;
013000081216       cpip(3)=deicpi3p  ;
013100081216       cpip(4)=deicpi4p  ;
013200081216       cpip(5)=deicpi5p  ;
013300081216       cpip(6)=deicpi6p  ;
013400081216       cpip(7)=deicpi7p  ;
013500081216       cpip(8)=deicpi8p  ;
013600081216       cpip(9)=deicpi9p  ;
013700081216
013800081120       clear conta     ;
013900081216       clear contap    ;
014000081120       xx=1            ;
014100081120       dow xx<=9       ;
014200081120       if cpi(xx)='S'  ;
014300081120       conta=conta+1   ;
014400081120       endif           ;
014500081216       if cpip(xx)='S'  ;
014600081216       contap=contap+1   ;
014700081216       endif           ;
014800081120       xx=xx+1         ;
014900081120       enddo           ;
015000090113
015100090113        // totale clienti senza info complete
015200081120 3      if conta=0     ;
015300081120           noif=noif+1     ;
015400090113
015500090113           // DI CUI con info parziali
015600081216 3      if contap>0     ;
015700081216           nomoi=nomoi+1     ;
015800090113           else            ;
015900090113
016000090113           exsr Cercapotfi ;
016100090113
016200090113           AA=1            ;
016300090113           dow POTFIL(AA)>0 AND AA<=100     ;
016400090113            if potinfo(AA)='S' OR potinfop(AA)='S'       ;
016500090113             ifofi=ifofi+1   ;
016600090113             LEAVE          ;
016700090113            endif           ;
016800090113
016900090113           Aa=AA+1         ;
017000090113           enddo           ;
017100090113
017200081216 3      endif              ;
017300090113 3      endif              ;
017400081120
017500081120 3      if deicpo2>0      ;
017600081120           piusd=piusd+1   ;
017700071217
017800081120           if conta=1      ;
017900081120           psdui=psdui+1  ;
018000081120           endif           ;
018100081120           if conta>1      ;
018200081120           psdpi=psdpi+1  ;
018300081120           endif           ;
018400081120 3       endif           ;
018500081120 2       endif           ;
018600090121 2       endif           ;
018700071217
018800081120       read    wfdei05l;
018900071218 1     enddo ;
019000071218
019100071121       *inlr=*on         ;
019200071116       //-------------------------------------------------------------
019300081120       BEGSR  Scrivitot    ;
019400081120
019500081120       clear wfdeit00      ;
019600081120
019700081120       tdeifil=savfil       ;
019800081120       tdeidfil=savdfil       ;
019900081120       tdeiare=savare       ;
020000081120       tdeidare=savdare       ;
020100081120       tdeitcli=tcli       ;
020200081120       tdeinoif=noif        ;
020300081216       tdeinomoi=nomoi       ;
020400090113       tdeiifofi=ifofi       ;
020500081120       tdeinosd=nosd        ;
020600081120       tdeipiusd=piusd        ;
020700081120       tdeipsdui=psdui        ;
020800081120       tdeipsdpi=psdpi        ;
020900081120       write wfdeit00         ;
021000081120
021100081120       clear tcli            ;
021200081120       clear noif            ;
021300081216       clear nomoi           ;
021400090113       clear ifofi           ;
021500081120       clear nosd            ;
021600081120       clear piusd           ;
021700081120       clear psdui           ;
021800081120       clear psdpi           ;
021900081119
022000080110       ENDSR  ;
022100090113       //-------------------------------------------------------------
022200090113       BEGSR  CercaPotFi   ;
022300090113
022400090113       // Prendo codice unificante e carico cod collegati figli
022500090113           clear tibs10ds ;
022600090113           d10tle='ST'   ;
022700090113           d10paf='F'    ;
022800090113           d10cod=deikscu;
022900090113           callp TIBS10R (tibs10ds)   ;
023000090113
023100090113  1        if d10err<>*blanks   ;
023200090113           // Se errore verifico se è figlio
023300090113           clear tibs10ds ;
023400090113           d10tle='ST'   ;
023500090113           d10paf='P'    ;
023600090113           d10cod=deikscu;
023700090113           callp TIBS10R (tibs10ds)   ;
023800090113
023900090113  2        if d10err<>*blanks   ;
024000090113           // Imposto nella skiera e come padre solo se stesso
024100090113           skc(1)='0000'+%editc(deikscu:'X');
024200090113           d10cop=deikscu                  ;
024300090113  2        endif                ;
024400090113  1        endif ;
024500090113
024600090113       clear potFIL   ;
024700090113       clear potinfo  ;
024800090113       clear potinfop ;
024900090113       clear AA       ;
025000090113       BB=1    ;
025100090113
025200090113 1     dow skc(BB)>*Zeros  ;
025300090113
025400090113       // Verifico se cnaco ha un codice FIg  attaccato ;
025500090113       wksc=%int(%subst(skc(BB):5:7)) ;
025600090113       chain   (1:dutkci:wksc)  cnaco00f  ;
025700090113 2     if %found(cnaco00f)   ;
025800090113
025900090113         chain  acolib   tncpo01l   ;
026000090113
026100090113 3       if %found(tncpo01l) and cpofsf='F'  ;
026200090113         exsr MemPotFIGLIO   ;
026300090113 3       endif     ;
026400090113 2     endif     ;
026500090113
026600090113       BB=BB+1   ;
026700090113 1     enddo     ;
026800090113
026900090113       // Cerca poi con la partita iva e cod fiscale sottoconto int fattura
027000090113       //  codice unificante, altri potenziali  SEDE
027100090113       wksc=d10cop;
027200090113       chain   (1:dutkci:wksc)  cnclp00f  ;
027300090113 1     if %found(cnclp00f)   ;
027400090113
027500090113       // col sottoconti int fattura
027600090113       chain   (1:dutkci:clpscf)  cnind00f  ;
027700090113 2     if %found(cnind00f) and indsta=*blanks  ;
027800090113         // con la partita iva
027900090113 2a      if indiva<>*blanks  and %subst(indiva:1:2)<>'$$'
028000090113                             AND INDIVA<>'04507990150'   ;
028100090113         setll indiva   tncpo06l  ;
028200090113         reade indiva   tncpo06l  ;
028300090113
028400090113 3         dow not %eof(tncpo06l)   ;
028500090113 4         if cpofsf='F'          ;
028600090113              exsr MempotFIGLIO     ;
028700090113 4         endif                  ;
028800090113
028900090113           reade indiva   tncpo06l  ;
029000090113 3       enddo                    ;
029100090113 2a      endif    ;
029200090113
029300090113         // con il codice fiscale
029400090113 2a      if indcdf<>*blanks       ;
029500090113         setll indcdf   tncpo05l  ;
029600090113         reade indcdf   tncpo05l  ;
029700090113
029800090113 3         dow not %eof(tncpo05l)   ;
029900090113 4         if cpofsf='F'          ;
030000090113              exsr MempotFIGLIO    ;
030100090113 4         endif                  ;
030200090113
030300090113           reade indcdf   tncpo05l  ;
030400090113 3       enddo                    ;
030500090113 2a    endif                  ;
030600090113 2     endif                  ;
030700090113 1     endif                  ;
030800090113
030900090113       ENDSR  ;
031000090113       //--------------------------------------------------------------
031100090113       //?Memorizza potenziali sede per ogni unificante
031200090113       //--------------------------------------------------------------
031300090113       BEGSR MemPotFIGLIO ;
031400090113         // Memorizzo i codici visualizzati per evitare ripetizioni
031500090113         Indx=%lookup(cpocpo:potFIL )   ;
031600090113           if Indx=0    ;
031700090113           AA=AA+1  ;
031800090113           potFIL (AA)=cpocpo   ;
031900090113           dcpo01=cporst  ;
032000090113           potinfo(AA)=§cpoifotot ;
032100090113
032200090113           // Se le info non sono complete, verifico se mancano solo mo
032300090113             if §cpoifotot <>'S'    ;
032400090113                    clear trmk50ds    ;
032500090113                    i50cpo=cpocpo     ;
032600090113                    i50mod='C'        ;
032700090113                    i50obl='S'        ;
032800090113                    callp TRMK50R (kpjba:trmk50ds)  ;
032900090113                     potinfo(AA)=o50ifotot  ;
033000090113                     potinfop(AA)=o50ifomin  ;
033100090113             endif;
033200090113           endif;
033300090113           ENDSR  ;
033400081119       //--------------------------------------------------------------
033500081119       //?Reperimento Dati del job (Utente/Operativi).
033600081119       //--------------------------------------------------------------
033700081119       BEGSR DatiJob;
033800081119
033900081119         in(E) §AzUte;
034000081119         if NOT %error;
034100081119           in(E) §DatiUte;
034200081119         endif;
034300081119         if %error or RSut = *blanks;
034400081119           clear TIBS34ds;
034500081119           tibs34r(tibs34ds);
034600081119           in §AzUte;
034700081119           in §DatiUte;
034800081119         endif;
034900081119
035000081119       ENDSR;
035100071130
035200071116      /end-free
