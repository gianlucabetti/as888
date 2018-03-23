000100131227     H*------------------------------------------------------------------------*
000200141125     H* Creazione File volumi filiale filiale WFTWV
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500131227     h dftactgrp(*no) actgrp(*caller)
000600970910     F*------------------------------------------------------------------------*
000700970910     F* DATA BASE
000800970910     F*------------------------------------------------------------------------*
000900141125     fsisdr01l  if   e           K disk
001000141125     FAZCLN01L  IF   E           K DISK
001100141128     fwaoma01l  if   e           K disk
001200141125     fwftwm00f  o    e             disk
001300130906
001400130906     d*
001500141128     d fnlv55ds      e ds
001600141121     d kpjba         e ds
001700141125     d Tntls6ds      e ds
001800140103     d Tibs34ds      e ds                  inz
001900140103     d §azute        e ds                  Extname(Azute00f)
002000140103     d                                     dtaara
002100140103     d §Datiute      e ds                  EXTname(ddatiute)
002200140103     d                                     dtaara
002300141125     D WDAT8           DS
002400141125     D  DATADA                 1      8  0
002500141125     D  DATAA                  9     16  0
002600141125     D  GGL                   17     21  0
002700141125
002800141125     D GFEDS           DS
002900141125     D  GFE                    1     31
003000141125     D                                     DIM(31)
003100131227
003200141125     d xx              s              2  0
003300141125     d kann            s              4  0
003400141125     d kmes            s              4  0
003500141125     d wvol            s             13  3
003600150513     d w_wmmcm         s              8  3
003700150513     d w_twmahmx       s                   like(omahmx)
003800150513     d w_twmphmx       s                   like(omahmp)
003900141125     d wrkgetlista     s           4096    varying
004000140103
004100130906     D
004200140103      // ? PROTOTIPI ?
004300141125      /copy gaitrasrc/srcprotopr,tibs34r
004400141125      /copy gaitrasrc/srcprotopr,xsrlav8
004500141128      /copy gaitrasrc/srcprotopr,fnlv55r
004600130905      //---------------------------------------------------------------
004700130905      //?Definizione procedure usate.
004800130905      //---------------------------------------------------------------
004900970910     C*------------------------------------------------------------------------*
005000970910     C* MAIN LINES
005100970910     C*------------------------------------------------------------------------*
005200141121     C     *entry        plist
005300141121     C                   parm                    kpjba
005400141125     c                   eval      tntls6ds=kpjbu
005500130905      /free
005600141121         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005700141125       EXEC sql  delete from WFTWM00F          ;
005800141125
005900141125       // Operazioni iniziali
006000141125       exsr RoutInz;
006100141125
006200141125         exsr prepaSQL   ;
006300141125       EXEC sql  prepare s1 from : wrkgetlista ;
006400141125       EXEC sql  declare a1 cursor for s1      ;
006500141125       EXEC sql  open    a1                    ;
006600141125
006700141125       EXEC sql fetch next from a1 into :sdrflp, :sdrfla, :wvol    ;
006800141125
006900141125       dow sqlcod=0    ;
007000141125       exsr Scrivi   ;
007100141125
007200141125       EXEC sql fetch next from a1 into :sdrflp, :sdrfla, :wvol    ;
007300141125       enddo  ;
007400141125
007500141125       EXEC sql  close   a1                    ;
007600130905
007700130905       // Operazioni finali
007800130905       exsr RoutEnd;
007900130905       //--------------------------------------------------------------
008000130905       //?Operazioni iniziali.
008100130905       //--------------------------------------------------------------
008200130905       BEGSR RoutInz;
008300130905
008400130905         // Reperimento dati job
008500130905         exsr DatiJob;
008600130905
008700141125       // Recupero giorni lavorativi dei mesi richiesti
008800141125       if    TLS6AAMMA=0 ;
008900141125        TLS6AAMMA = TLS6AAMMd  ;
009000141125       endif  ;
009100141125
009200141125       clear wdat8  ;
009300141125       datada=(TLS6AAMMd*100)+01  ;
009400141125
009500141125       kann=%int(%subst(%editc(tls6aamma:'X'):1:4))  ;
009600141125       kmes=%int(%subst(%editc(tls6aamma:'X'):5:2))  ;
009700141125       CHAIN (000:000:kann:kmes)    AZCLN01L     ;
009800141125
009900141125       if %found(azcln01l)  ;
010000141125       gfeds=clnpom  ;
010100141125       xx=31  ;
010200141125       dow gfe(xx)='*'  ;
010300141125       xx=xx-1  ;
010400141125       enddo ;
010500141125       dataa =(TLS6AAMMA*100)+xx  ;
010600141125
010700141125       else  ;
010800141125       dataa =(TLS6AAMMA*100)+31  ;
010900141125       endif  ;
011000141125
011100141125       callp xsrlav8 (wdat8)  ;
011200141125
011300130905       endsr;
011400130905       //--------------------------------------------------------------
011500130905       //?Reperimento Dati del job (Utente/Operativi).
011600130905       //--------------------------------------------------------------
011700130905       BEGSR DatiJob;
011800130905
011900130905         in(E) §AzUte;
012000130905         if NOT %error;
012100130905           in(E) §DatiUte;
012200130905         endif;
012300130905         if %error or RSut = *blanks;
012400130905           clear TIBS34ds;
012500130905           tibs34r(tibs34ds);
012600130905           in §AzUte;
012700130905           in §DatiUte;
012800130905         endif;
012900130905
013000130905       ENDSR;
013100130905       //--------------------------------------------------------------
013200130905       //?Operazioni finali.
013300130905       //--------------------------------------------------------------
013400130905       BEGSR RoutEnd;
013500130905
013600130905         // Chiusura pgm   ;
013700141128             clear fnlv55ds;
013800141128             d55tla='C'     ;
013900141128             fnlv55r(fnlv55ds);
014000130905
014100130905         *inLR = *on;
014200130905
014300130905         return;
014400130905
014500130905       ENDSR;
014600141125       //--------------------------------------------------------------
014700141125       //?Prepara SQL
014800141125       //--------------------------------------------------------------
014900141125       BEGSR  PrepaSQL               ;
015000141126        wrkgetlista ='select sdrflp, sdrfla, sum(sdrvol) from sisdr00f +
015100141125                     where (sdrann*100)+sdrmes >='+%editc(TLS6AAMMD:'X')  +
015200141126                     ' and (sdrann*100)+sdrmes <='+%editc(TLS6AAMMa:'X')   +
015300141126                     ' and sdrrbl<>''R'' GROUP BY sdrflp, sdrfla ORDER BY +
015400141125                     sdrflp, sdrfla'  ;
015500141125       ENDSR  ;
015600141125       //--------------------------------------------------------------
015700141125       //?Scrittura file
015800141125       //--------------------------------------------------------------
015900141125       BEGSR  scrivi                 ;
016000141125       clear wftwm000 ;
016100150513       clear w_TWmAHMX;
016200150513       clear w_TWmPHMX;
016300141125       twmalnp=%editc(sdrflp:'X') ;
016400141125       twmalna=%editc(sdrfla:'X') ;
016500150513       // eval(H) twmmcm = wvol/ggl  ;
016600150513          eval(H) w_wmmcm = wvol/ggl  ;
016700141125
016800150513       //if  twmmcm>0    ;
016900150513       if  w_wmmcm>0    ;
017000150514          twmmcm=%trim(%editw(w_wmmcm:'0     .   '));
017100141128          //   Determino il terminal di partenza
017200141128             clear fnlv55ds;
017300141128             d55tpt='P'     ;
017400141128             d55lin = sdrflp;
017500141128             d55drf = tls6drf;
017600141128             fnlv55r(fnlv55ds);
017700141128                chain (sdrfla:d55tfp) waoma01l;
017800150513          // Trovato il record specifico --> memorizzo gli orari presenti
017900150513                if %found(waoma01l);
018000150513                  w_TWmAHMX=OMAHMX;
018100150513                  w_TWmPHMX=OMAHMP;
018200150513                endif ;
018300150513          // Se gli orari sono vuoti cerco il record generico
018400150513          // tfp=000 per l'orario arrivo
018500150513                if w_twmahmx=0;
018600150513                  chain (sdrfla:000)    waoma01l;
018700150513                  if %found(waoma01l);
018800150513                     w_TWmAHMX=OMAHMX;
018900150513                  endif;
019000150513                endif;
019100150513          // lna=000 per l'orario partenza
019200150513                if W_twmphmx=0      ;
019300150513                  chain (000:d55tfp)    waoma01l;
019400150513                  if %found(waoma01l);
019500150514                     W_twmPHMX=OMAHMP;
019600150513                  endif;
019700150513                endif;
019800150513          //    if not %found(waoma01l);
019900150513          //    chain (sdrfla:000)    waoma01l;
020000150513          //    endif  ;
020100150513          //    if %found(waoma01l);
020200150513          //      TWmAHMX=%editw(OMAHMX:'  :  ');
020300150513          //    endif;
020400150513          if w_twmahmx>0;
020500150609          //   twmahmx=%trim(%editw(w_twmAHMX:'0  :  '));
020600150609               twmahhx=%subst(%editc(w_twmahmx:'X'):1:2) ;
020700150609               twmammx=%subst(%editc(w_twmahmx:'X'):3:2) ;
020800150513          endif;
020900150513          if w_twmphmx>0;
021000150609          //   twmphmx=%trim(%editw(w_twmpHMX:'0  :  '));
021100150609               twmphhx=%subst(%editc(w_twmphmx:'X'):1:2) ;
021200150609               twmpmmx=%subst(%editc(w_twmphmx:'X'):3:2) ;
021300150513          endif;
021400141128
021500141125       write wftwm000  ;
021600141128       endif           ;
021700141128
021800141125       ENDSR    ;
