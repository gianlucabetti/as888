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
001001141128     fwaoma01l  if   e           K disk
001100141125     fwftwm00f  o    e             disk
001200130906
001300130906     d*
001301141128     d fnlv55ds      e ds
001400141121     d kpjba         e ds
001500141125     d Tntls6ds      e ds
001700140103     d Tibs34ds      e ds                  inz
001800140103     d §azute        e ds                  Extname(Azute00f)
001900140103     d                                     dtaara
002000140103     d §Datiute      e ds                  EXTname(ddatiute)
002100140103     d                                     dtaara
002200141125     D WDAT8           DS
002300141125     D  DATADA                 1      8  0
002400141125     D  DATAA                  9     16  0
002500141125     D  GGL                   17     21  0
002600141125
002700141125     D GFEDS           DS
002800141125     D  GFE                    1     31
002900141125     D                                     DIM(31)
003000131227
003100141125     d xx              s              2  0
003200141125     d kann            s              4  0
003300141125     d kmes            s              4  0
003400141125     d wvol            s             13  3
003401150513     d w_wmmcm         s              8  3
003402150513     d w_twmahmx       s                   like(omahmx)
003403150513     d w_twmphmx       s                   like(omahmp)
003500141125     d wrkgetlista     s           4096    varying
003600140103
003700130906     D
003800140103      // ? PROTOTIPI ?
003900141125      /copy gaitrasrc/srcprotopr,tibs34r
004000141125      /copy gaitrasrc/srcprotopr,xsrlav8
004001141128      /copy gaitrasrc/srcprotopr,fnlv55r
004100130905      //---------------------------------------------------------------
004200130905      //?Definizione procedure usate.
004300130905      //---------------------------------------------------------------
004400970910     C*------------------------------------------------------------------------*
004500970910     C* MAIN LINES
004600970910     C*------------------------------------------------------------------------*
004700141121     C     *entry        plist
004800141121     C                   parm                    kpjba
004900141125     c                   eval      tntls6ds=kpjbu
005000130905      /free
005100141121         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005200141125       EXEC sql  delete from WFTWM00F          ;
005300141125
005400141125       // Operazioni iniziali
005500141125       exsr RoutInz;
005600141125
005700141125         exsr prepaSQL   ;
005800141125       EXEC sql  prepare s1 from : wrkgetlista ;
005900141125       EXEC sql  declare a1 cursor for s1      ;
006000141125       EXEC sql  open    a1                    ;
006100141125
006200141125       EXEC sql fetch next from a1 into :sdrflp, :sdrfla, :wvol    ;
006300141125
006400141125       dow sqlcod=0    ;
006500141125       exsr Scrivi   ;
006600141125
006700141125       EXEC sql fetch next from a1 into :sdrflp, :sdrfla, :wvol    ;
006800141125       enddo  ;
006900141125
007000141125       EXEC sql  close   a1                    ;
007100130905
007200130905       // Operazioni finali
007300130905       exsr RoutEnd;
007400130905       //--------------------------------------------------------------
007500130905       //?Operazioni iniziali.
007600130905       //--------------------------------------------------------------
007700130905       BEGSR RoutInz;
007800130905
007900130905         // Reperimento dati job
008000130905         exsr DatiJob;
008100130905
008200141125       // Recupero giorni lavorativi dei mesi richiesti
008300141125       if    TLS6AAMMA=0 ;
008400141125        TLS6AAMMA = TLS6AAMMd  ;
008500141125       endif  ;
008600141125
008700141125       clear wdat8  ;
008800141125       datada=(TLS6AAMMd*100)+01  ;
008900141125
009000141125       kann=%int(%subst(%editc(tls6aamma:'X'):1:4))  ;
009100141125       kmes=%int(%subst(%editc(tls6aamma:'X'):5:2))  ;
009200141125       CHAIN (000:000:kann:kmes)    AZCLN01L     ;
009300141125
009400141125       if %found(azcln01l)  ;
009500141125       gfeds=clnpom  ;
009600141125       xx=31  ;
009700141125       dow gfe(xx)='*'  ;
009800141125       xx=xx-1  ;
009900141125       enddo ;
010000141125       dataa =(TLS6AAMMA*100)+xx  ;
010100141125
010200141125       else  ;
010300141125       dataa =(TLS6AAMMA*100)+31  ;
010400141125       endif  ;
010500141125
010600141125       callp xsrlav8 (wdat8)  ;
010700141125
010800130905       endsr;
010900130905       //--------------------------------------------------------------
011000130905       //?Reperimento Dati del job (Utente/Operativi).
011100130905       //--------------------------------------------------------------
011200130905       BEGSR DatiJob;
011300130905
011400130905         in(E) §AzUte;
011500130905         if NOT %error;
011600130905           in(E) §DatiUte;
011700130905         endif;
011800130905         if %error or RSut = *blanks;
011900130905           clear TIBS34ds;
012000130905           tibs34r(tibs34ds);
012100130905           in §AzUte;
012200130905           in §DatiUte;
012300130905         endif;
012400130905
012500130905       ENDSR;
012600130905       //--------------------------------------------------------------
012700130905       //?Operazioni finali.
012800130905       //--------------------------------------------------------------
012900130905       BEGSR RoutEnd;
013000130905
013100130905         // Chiusura pgm   ;
013101141128             clear fnlv55ds;
013102141128             d55tla='C'     ;
013104141128             fnlv55r(fnlv55ds);
013200130905
013300130905         *inLR = *on;
013400130905
013500130905         return;
013600130905
013700130905       ENDSR;
013800141125       //--------------------------------------------------------------
013900141125       //?Prepara SQL
014000141125       //--------------------------------------------------------------
014100141125       BEGSR  PrepaSQL               ;
014200141126        wrkgetlista ='select sdrflp, sdrfla, sum(sdrvol) from sisdr00f +
014300141125                     where (sdrann*100)+sdrmes >='+%editc(TLS6AAMMD:'X')  +
014400141126                     ' and (sdrann*100)+sdrmes <='+%editc(TLS6AAMMa:'X')   +
014500141126                     ' and sdrrbl<>''R'' GROUP BY sdrflp, sdrfla ORDER BY +
014600141125                     sdrflp, sdrfla'  ;
014700141125       ENDSR  ;
014800141125       //--------------------------------------------------------------
014900141125       //?Scrittura file
015000141125       //--------------------------------------------------------------
015100141125       BEGSR  scrivi                 ;
015200141125       clear wftwm000 ;
015201150513       clear w_TWmAHMX;
015202150513       clear w_TWmPHMX;
015300141125       twmalnp=%editc(sdrflp:'X') ;
015400141125       twmalna=%editc(sdrfla:'X') ;
015500150513       // eval(H) twmmcm = wvol/ggl  ;
015501150513          eval(H) w_wmmcm = wvol/ggl  ;
015600141125
015601150513       //if  twmmcm>0    ;
015602150513       if  w_wmmcm>0    ;
015603150514          twmmcm=%trim(%editw(w_wmmcm:'0     .   '));
015604141128          //   Determino il terminal di partenza
015605141128             clear fnlv55ds;
015606141128             d55tpt='P'     ;
015607141128             d55lin = sdrflp;
015608141128             d55drf = tls6drf;
015609141128             fnlv55r(fnlv55ds);
015610141128                chain (sdrfla:d55tfp) waoma01l;
015611150513          // Trovato il record specifico --> memorizzo gli orari presenti
015612150513                if %found(waoma01l);
015613150513                  w_TWmAHMX=OMAHMX;
015614150513                  w_TWmPHMX=OMAHMP;
015615150513                endif ;
015616150513          // Se gli orari sono vuoti cerco il record generico
015617150513          // tfp=000 per l'orario arrivo
015618150513                if w_twmahmx=0;
015619150513                  chain (sdrfla:000)    waoma01l;
015620150513                  if %found(waoma01l);
015621150513                     w_TWmAHMX=OMAHMX;
015622150513                  endif;
015623150513                endif;
015624150513          // lna=000 per l'orario partenza
015625150513                if W_twmphmx=0      ;
015626150513                  chain (000:d55tfp)    waoma01l;
015627150513                  if %found(waoma01l);
015628150514                     W_twmPHMX=OMAHMP;
015629150513                  endif;
015630150513                endif;
015631150513          //    if not %found(waoma01l);
015632150513          //    chain (sdrfla:000)    waoma01l;
015633150513          //    endif  ;
015634150513          //    if %found(waoma01l);
015635150513          //      TWmAHMX=%editw(OMAHMX:'  :  ');
015636150513          //    endif;
015637150513          if w_twmahmx>0;
015638150513             twmahmx=%trim(%editw(w_twmAHMX:'0  :  '));
015639150513          endif;
015640150513          if w_twmphmx>0;
015641150513             twmphmx=%trim(%editw(w_twmpHMX:'0  :  '));
015642150513          endif;
015643141128
015700141125       write wftwm000  ;
015701141128       endif           ;
015702141128
015800141125       ENDSR    ;
