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
002300090210     d totfatpic       s             15  0
002400090210     d totfatmed       s             15  0
002500090210     d totfatgra       s             15  0
002600090210     d totsptpic       s             15  0
002700090210     d totsptmed       s             15  0
002800090210     d totsptgra       s             15  0
002900090212     d totpic          s              7  0
003000090212     d totmed          s              7  0
003100090212     d totgra          s              7  0
003200031205
003300031205      *------------------------------------------------------------------------*
003400031205      *   D S   I N T E R N E / E S T E R N E
003500031205      *------------------------------------------------------------------------*
003600031205     d Kpjba         e ds
003700071122     d
003800090203      // - Reperimento dati utente
003900090203     d TIBS34ds      e ds
004000090203      // - Dati utente
004100090203     d §AzUte        e ds                  extname(AZUTE00F)
004200090203     d                                     dtaara
004300090203     d §DatiUte      e ds                  extname(dDatiUte)
004400090203     d                                     dtaara
004500090203     D*
004600071122      *------------------------------------------------------------------------*
004700071122      *   P R O T O T I P I
004800071122      *------------------------------------------------------------------------*
004900090203      /copy gaitrasrc/srcprotopr,tibs34r
005000031205      *------------------------------------------------------------------------*
005100071217     c     *entry        plist
005200090203     c                   parm                    kpjba
005300090203     c
005400071116      /free
005500090203         // Reperimento dati job
005600090203         exsr DatiJob;
005700090203
005800090210
005900090210       // pulizia file delle %
006000090210       exec sql   delete  from  wfdemr0f   ;
006100090210
006200090211       clear savare  ;
006300090211       clear savcmerc;
006400090210       // lettura dda file base potenziali
006500090210       setll  *loval wfdemp4l  ;
006600090211       read(N)       wfdemp4l  ;
006700090210       dow   not %eof(wfdemp4l) ;
006800090210
006900090210       if demsptrel>0  and demtotfat>0 ;
007000090210       // Cambio area o settore merceologico
007100090210
007200090210       if savare<>demare or savcmerc<>demcmerc ;
007300090210         if savare>0   ;
007400090210            exsr ScriviPerc ;
007500090210         endif   ;
007600090210
007700090210       savare=demare   ;
007800090210       savcmerc=demcmerc;
007900090210       clear  totfatpic   ;
008000090210       clear  totsptpic   ;
008100090210       clear  totfatmed   ;
008200090210       clear  totsptmed   ;
008300090210       clear  totfatgra   ;
008400090210       clear  totsptgra   ;
008500090212       clear  totpic      ;
008600090212       clear  totmed      ;
008700090212       clear  totgra      ;
008800090210       endif   ;
008900090210
009000090210       // Aziende piccole
009100090210       select   ;
009200090210       when demtotfat<=piccoleAL  ;
009300090210       totfatpic=totfatpic+demtotfat  ;
009400090210       totsptpic=totsptpic+demsptrel  ;
009500090212       totpic=totpic+1  ;
009600090210       when demtotfat<=MedieAL  ;
009700090210       totfatmed=totfatmed+demtotfat  ;
009800090210       totsptmed=totsptmed+demsptrel  ;
009900090212       totmed=totmed+1  ;
010000090210       other  ;
010100090210       totfatgra=totfatgra+demtotfat  ;
010200090210       totsptgra=totsptgra+demsptrel  ;
010300090212       totgra=totgra+1  ;
010400090210       endsl   ;
010500090210
010600090210       endif   ;
010700090210
010800090211       read(N)       wfdemp4l  ;
010900090210       enddo ;
011000090210
011100090210       // Ultima scrittura
011200090210       if savare>0   ;
011300090210         exsr ScriviPerc ;
011400090210       endif   ;
011500071122
011600090211       // ora calcolo il fatturato presunto trasporti a tutti i pot sede
011700090211       // con fatturato azienda
011800090211       setll  *loval wfdemp4l  ;
011900090211       read          wfdemp4l  ;
012000090211       dow   not %eof(wfdemp4l) ;
012100090211
012200090211       if  demtotfat>0 ;
012300090211
012400090211       if savare<>demare or savcmerc<>demcmerc   ;
012500090211       chain (demare:demcmerc)   wfdemr1l   ;
012600090211       savare=demare  ;
012700090211       savcmerc=demcmerc   ;
012800090211       endif   ;
012900090211
013000090211       select   ;
013100090211       when demtotfat<=piccoleAL  ;
013200090211       perc=r_dempperc  ;
013300090211       when demtotfat<=MedieAL  ;
013400090211       perc=r_demmperc  ;
013500090211       other  ;
013600090211       perc=r_demmperc  ;
013700090211       endsl   ;
013800090211
013900090211       eval(R) demsptprs=(demtotfat*perc)/100   ;
014000090211
014100090211       update wfdemp04   ;
014200090211       endif    ;
014300090211
014400090211       read          wfdemp4l  ;
014500090211       enddo   ;
014600090209
014700071121       *inlr=*on         ;
014800090210       //--------------------------------------------------------------
014900090210       //?Reperimento Dati del job (Utente/Operativi).
015000090210       //--------------------------------------------------------------
015100090210       BEGSR DatiJob;
015200090210
015300090210         in(E) §AzUte;
015400090210         if NOT %error;
015500090210           in(E) §DatiUte;
015600090210         endif;
015700090210         if %error or RSut = *blanks;
015800090210           clear TIBS34ds;
015900090210           tibs34r(tibs34ds);
016000090210           in §AzUte;
016100090210           in §DatiUte;
016200090210         endif;
016300090210
016400090210       ENDSR;
016500090210       //--------------------------------------------------------------
016600090210       //?calcolo e scrivo le % incidenza spesa trasporti sul fatturato
016700090210       //--------------------------------------------------------------
016800090210       BEGSR ScriviPerc;
016900090210       clear picperc  ;
017000090210       if totfatpic>0 and totsptpic>0   ;
017100090210       eval(R) picperc=(totsptpic*100)/totfatpic  ;
017200090210       endif ;
017300090210       clear medperc  ;
017400090210       if totfatmed>0 and totsptmed>0   ;
017500090210       eval(R) medperc=(totsptmed*100)/totfatmed  ;
017600090210       endif ;
017700090210       clear graperc  ;
017800090210       if totfatgra>0 and totsptgra>0   ;
017900090210       eval(R) graperc=(totsptgra*100)/totfatgra  ;
018000090210       endif ;
018100090210
018200090210       clear wfdemr00 ;
018300090210       r_demare=savare ;
018400090210       r_demcmerc=savcmerc ;
018500090210       r_dempperc=picperc   ;
018600090212       r_dempcli=totpic  ;
018700090210       r_demmperc=medperc   ;
018800090212       r_demmcli=totmed  ;
018900090210       r_demgperc=graperc   ;
019000090212       r_demgcli=totgra  ;
019100090210
019200090210       write wfdemr00   ;
019300090210
019400090210       ENDSR;
019500071116      /end-free
