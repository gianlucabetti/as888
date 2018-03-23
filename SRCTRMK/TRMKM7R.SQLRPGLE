000100031205      *------------------------------------------------------------------------*
000200090210      * elaborazione spesa trasporti presunta
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090211     FWFDEmp4L  uF   E           K DISK    rename(wfdemp00:wfdemp04)
000700090211     FWFDEmr1l  iF a E           K DISK    prefix(r_)
000800031205      *------------------------------------------------------------------------*
000900031205      *   C A M P I   D I   L A V O R O
001000031205      *------------------------------------------------------------------------*
001100071213     d kut             s              1  0 inz(1)
001200071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
001300090211     d Perc            s              5  2
001400090211     d picPerc         s              5  2
001500090210     d MedPerc         s              5  2
001600090210     d GraPerc         s              5  2
001700090209     d Indx            s              4  0
001800090209     d yy              s              4  0
001900090212     d PiccoleAL       s             11  0  inz(10000000)
002000090212     d MedieAL         s             11  0  inz(50000000)
002100090210     d savare          s                    like(demare)
002200090210     d savcmerc        s                    like(demcmerc)
002300090212     d totfatpic       s             15  0  inz
002400090212     d totfatmed       s             15  0  inz
002500090212     d totfatgra       s             15  0  inz
002600090212     d totsptpic       s             15  0  inz
002700090212     d totsptmed       s             15  0  inz
002800090212     d totsptgra       s             15  0  inz
002900090212     d totpic          s              7  0  inz
003000090212     d totmed          s              7  0  inz
003100090212     d totgra          s              7  0  inz
003200090212     d merfatpic       s             15  0  inz
003300090212     d merfatmed       s             15  0  inz
003400090212     d merfatgra       s             15  0  inz
003500090212     d mersptpic       s             15  0  inz
003600090212     d mersptmed       s             15  0  inz
003700090212     d mersptgra       s             15  0  inz
003800090212     d merpic          s              7  0  inz
003900090212     d mermed          s              7  0  inz
004000090212     d mergra          s              7  0  inz
004100090223     d w0080           s              8  0  inz
004200031205
004300031205      *------------------------------------------------------------------------*
004400031205      *   D S   I N T E R N E / E S T E R N E
004500031205      *------------------------------------------------------------------------*
004600031205     d Kpjba         e ds
004700071122     d
004800090203      // - Reperimento dati utente
004900090203     d TIBS34ds      e ds
005000090203      // - Dati utente
005100090203     d §AzUte        e ds                  extname(AZUTE00F)
005200090203     d                                     dtaara
005300090203     d §DatiUte      e ds                  extname(dDatiUte)
005400090203     d                                     dtaara
005500090203     D*
005600071122      *------------------------------------------------------------------------*
005700071122      *   P R O T O T I P I
005800071122      *------------------------------------------------------------------------*
005900090203      /copy gaitrasrc/srcprotopr,tibs34r
006000031205      *------------------------------------------------------------------------*
006100071217     c     *entry        plist
006200090203     c                   parm                    kpjba
006300090203     c
006400071116      /free
006500090203         // Reperimento dati job
006600090203         exsr DatiJob;
006700090203
006800090210
006900090210       // pulizia file delle %
007000090210       exec sql   delete  from  wfdemr0f   ;
007100090210
007200090211       clear savare  ;
007300090211       clear savcmerc;
007400090210       // lettura dda file base potenziali
007500090210       setll  *loval wfdemp4l  ;
007600090211       read(N)       wfdemp4l  ;
007700090210       dow   not %eof(wfdemp4l) ;
007800090210
007900090212       if demsptrel>0  and demtotfat>demsptrel ;
008000090210       // Cambio area o settore merceologico
008100090210
008200090210       if savare<>demare or savcmerc<>demcmerc ;
008300090212         if savare>0      ;
008400090212            exsr ScriviPercA;
008500090210         endif   ;
008600090210
008700090212       savare=demare   ;
008800090212       clear  totfatpic   ;
008900090212       clear  totsptpic   ;
009000090212       clear  totfatmed   ;
009100090212       clear  totsptmed   ;
009200090212       clear  totfatgra   ;
009300090212       clear  totsptgra   ;
009400090212       clear  totpic      ;
009500090212       clear  totmed      ;
009600090212       clear  totgra      ;
009700090212
009800090212         if savcmerc<>demcmerc and savcmerc<>*blanks ;
009900090212            exsr ScriviPercM ;
010000090212
010100090212         clear  merfatpic   ;
010200090212         clear  mersptpic   ;
010300090212         clear  merfatmed   ;
010400090212         clear  mersptmed   ;
010500090212         clear  merfatgra   ;
010600090212         clear  mersptgra   ;
010700090212         clear  merpic      ;
010800090212         clear  mermed      ;
010900090212         clear  mergra      ;
011000090212         endif   ;
011100090212
011200090212         savcmerc=demcmerc;
011300090212
011400090212       endif   ;
011500090210
011600090210       // Aziende piccole
011700090210       select   ;
011800090210       when demtotfat<=piccoleAL  ;
011900090210       totfatpic=totfatpic+demtotfat  ;
012000090210       totsptpic=totsptpic+demsptrel  ;
012100090212       totpic=totpic+1  ;
012200090210       when demtotfat<=MedieAL  ;
012300090210       totfatmed=totfatmed+demtotfat  ;
012400090210       totsptmed=totsptmed+demsptrel  ;
012500090212       totmed=totmed+1  ;
012600090210       other  ;
012700090210       totfatgra=totfatgra+demtotfat  ;
012800090210       totsptgra=totsptgra+demsptrel  ;
012900090212       totgra=totgra+1  ;
013000090210       endsl   ;
013100090210
013200090210       endif   ;
013300090210
013400090211       read(N)       wfdemp4l  ;
013500090210       enddo ;
013600090210
013700090210       // Ultima scrittura
013800090210       if savare>0   ;
013900090212         exsr ScriviPercA;
014000090212         exsr ScriviPercM;
014100090210       endif   ;
014200071122
014300090211       // ora calcolo il fatturato presunto trasporti a tutti i pot sede
014400090211       // con fatturato azienda
014500090212
014600090211       setll  *loval wfdemp4l  ;
014700090211       read          wfdemp4l  ;
014800090211       dow   not %eof(wfdemp4l) ;
014900090211
015000090211       if  demtotfat>0 ;
015100090211
015200090211       if savare<>demare or savcmerc<>demcmerc   ;
015300090212       chain (demcmerc:demare)   wfdemr1l   ;
015400090211       savare=demare  ;
015500090211       savcmerc=demcmerc   ;
015600090211       endif   ;
015700090211
015800090212       exsr ScegliP    ;
015900090212
016000090212       if perc=0   ;
016100090212       chain (demcmerc: 000)   wfdemr1l   ;
016200090212       clear savare   ;
016300090212       savcmerc=demcmerc   ;
016400090212       endif   ;
016500090212
016600090212       exsr ScegliP    ;
016700090211
016800090212       if perc>0   ;
016900090212       eval(H) demsptprs=(demtotfat*perc)/100   ;
017000090223
017100090223       // Arrotondo alle 1000 superiori
017200090223       w0080=demsptprs/1000 ;
017300090223       w0080=w0080+1  ;
017400090223       demsptprs=w0080*1000  ;
017500090211
017600090211       update wfdemp04   ;
017700090211       endif    ;
017800090212       endif    ;
017900090211
018000090211       read          wfdemp4l  ;
018100090211       enddo   ;
018200090209
018300071121       *inlr=*on         ;
018400090210       //--------------------------------------------------------------
018500090210       //?Reperimento Dati del job (Utente/Operativi).
018600090210       //--------------------------------------------------------------
018700090210       BEGSR DatiJob;
018800090210
018900090210         in(E) §AzUte;
019000090210         if NOT %error;
019100090210           in(E) §DatiUte;
019200090210         endif;
019300090210         if %error or RSut = *blanks;
019400090210           clear TIBS34ds;
019500090210           tibs34r(tibs34ds);
019600090210           in §AzUte;
019700090210           in §DatiUte;
019800090210         endif;
019900090210
020000090210       ENDSR;
020100090210       //--------------------------------------------------------------
020200090210       //?calcolo e scrivo le % incidenza spesa trasporti sul fatturato
020300090212       //    per area
020400090210       //--------------------------------------------------------------
020500090212       BEGSR ScriviPercA;
020600090210       clear picperc  ;
020700090210       if totfatpic>0 and totsptpic>0   ;
020800090212       eval(h) picperc=(totsptpic*100)/totfatpic  ;
020900090210       endif ;
021000090210       clear medperc  ;
021100090210       if totfatmed>0 and totsptmed>0   ;
021200090212       eval(H) medperc=(totsptmed*100)/totfatmed  ;
021300090210       endif ;
021400090210       clear graperc  ;
021500090210       if totfatgra>0 and totsptgra>0   ;
021600090212       eval(H) graperc=(totsptgra*100)/totfatgra  ;
021700090210       endif ;
021800090210
021900090210       clear wfdemr00 ;
022000090210       r_demare=savare ;
022100090210       r_demcmerc=savcmerc ;
022200090210       r_dempperc=picperc   ;
022300090212       r_dempcli=totpic  ;
022400090210       r_demmperc=medperc   ;
022500090212       r_demmcli=totmed  ;
022600090210       r_demgperc=graperc   ;
022700090212       r_demgcli=totgra  ;
022800090210
022900090210       write wfdemr00   ;
023000090210
023100090212       // salvo i dati per il calcolo di tutto il settore merceologico
023200090212       merfatpic=merfatpic+totfatpic  ;
023300090212       mersptpic=mersptpic+totsptpic  ;
023400090212       merfatmed=merfatmed+totfatmed  ;
023500090212       mersptmed=mersptmed+totsptmed  ;
023600090212       merfatgra=merfatgra+totfatgra  ;
023700090212       mersptgra=mersptgra+totsptgra  ;
023800090212       merpic=merpic+totpic  ;
023900090212       mermed=mermed+totmed  ;
024000090212       mergra=mergra+totgra  ;
024100090212
024200090210       ENDSR;
024300090212       //--------------------------------------------------------------
024400090212       //?Calcolo e scrivo le % incidenza spesa trasporti sul fatturato
024500090212       //    per settore merceologico
024600090212       //--------------------------------------------------------------
024700090212       BEGSR ScriviPercM;
024800090212       clear picperc  ;
024900090212       if merfatpic>0 and mersptpic>0   ;
025000090212       eval(H) picperc=(mersptpic*100)/merfatpic  ;
025100090212       endif ;
025200090212       clear medperc  ;
025300090212       if merfatmed>0 and mersptmed>0   ;
025400090212       eval(H) medperc=(mersptmed*100)/merfatmed  ;
025500090212       endif ;
025600090212       clear graperc  ;
025700090212       if merfatgra>0 and mersptgra>0   ;
025800090212       eval(H) graperc=(mersptgra*100)/merfatgra  ;
025900090212       endif ;
026000090212
026100090212       clear wfdemr00 ;
026200090212       r_demcmerc=savcmerc ;
026300090212       r_dempperc=picperc   ;
026400090212       r_dempcli=merpic  ;
026500090212       r_demmperc=medperc   ;
026600090212       r_demmcli=mermed  ;
026700090212       r_demgperc=graperc   ;
026800090212       r_demgcli=mergra  ;
026900090212
027000090212       write wfdemr00   ;
027100090212
027200090212       ENDSR;
027300090212       //--------------------------------------------------------------
027400090212       //?Scegli la percentuale in base alla grandezza azienda
027500090212       //--------------------------------------------------------------
027600090212       BEGSR ScegliP   ;
027700090212       select   ;
027800090212       when demtotfat<=piccoleAL  ;
027900090212       perc=r_dempperc  ;
028000090212       when demtotfat<=MedieAL  ;
028100090212       perc=r_demmperc  ;
028200090212       other  ;
028300090212       perc=r_demgperc  ;
028400090212       endsl   ;
028500090212       ENDSR   ;
028600071116      /end-free
