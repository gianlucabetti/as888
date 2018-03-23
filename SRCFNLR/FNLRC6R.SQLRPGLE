000100131227     H*------------------------------------------------------------------------*
000200150922     H* Verifica Controalert da elaborare e richiamo pgm scrittura file spia
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800141121     fazorg01l  if   e           K disk
000900150923     fazcln01l  if   e           K disk
001000130906
001100130906     d*
001200150925     d tibs02ds      e ds
001300141121     d kpjba         e ds
001400150922     d fnlrc6ds      e ds
001500150922     d fnlrc4ds      e ds
001600150922     d fnlv55ds      e ds
001700140103     d Tibs34ds      e ds                  inz
001800140103     d §azute        e ds                  Extname(Azute00f)
001900140103     d                                     dtaara
002000140103     d §Datiute      e ds                  EXTname(ddatiute)
002100140103     d                                     dtaara
002200150923     d DSDF          e ds
002300150925     d Dvpo          e ds
002400150930     d og143         e ds
002500131227
002600150923     D clnmat          DS
002700150923     D  mat                    1     31    dim(31)
002800150923     D clnpom          DS
002900150923     D  pom                    1     31    dim(31)
003000130905     d datacur         s               d   datfmt(*iso)
003100140103     d dateu           s              8  0
003200150923     d wlna            s                   like(ilrc6lna)
003300150923     d wlnae           s                   like(ilrc6lna)
003400150923     d wdat            s              8  0
003500150923     d wdatiso         s               d   datfmt(*iso)
003600150925     d orasys          s              6  0
003700150928     d orasys4         s              4  0
003800150928     d whlcoa4         s                   like(orasys4)
003900150928     d wolpora         s                   like(orasys4)
004000150928     d wfes            s              1
004100150928     d wsdfca3         s                   like(dateu)
004200140103
004300150922     d bch10           pr                  extpgm('BCH10')
004400150922     d  kpjba                              likeds(kpjba)
004500150925     d tibs02r         pr                  extpgm('TIBS02R')
004600150925     d  kpjba                              likeds(kpjba)
004700150925     d  tibs02ds                           likeds(tibs02ds)
004800130906     D
004900150925     D digits          c                   '0123456789'
005000140103      // ? PROTOTIPI ?
005100140103      /copy gaitrasrc/srcprotopr,tibs34r
005200150922      /copy gaitrasrc/srcprotopr,fnlv55r
005300130905      //---------------------------------------------------------------
005400130905      //?Definizione procedure usate.
005500130905      //---------------------------------------------------------------
005600970910     C*------------------------------------------------------------------------*
005700970910     C* MAIN LINES
005800970910     C*------------------------------------------------------------------------*
005900141121     C     *entry        plist
006000141121     C                   parm                    kpjba
006100150923     C                   PARM                    chiudi            1
006200150922     c                   eval      fnlrc6ds=kpjbu
006300130905      /free
006400141126
006500130905
006600130905       // Operazioni iniziali
006700130905       exsr RoutInz;
006800150923       // Verifica parametri ricevuti
006900150923       // Linea di arrivo
007000150930            clear wlna ;
007100150923            monitor;
007200150923            if ilrc6lna>0;
007300150923               wlna=ilrc6lna;
007400150923            endif;
007500150923            on-error  ;
007600150930       //      clear wlna ;
007700150923            endmon  ;
007800150923       // Data Elaborazione
007900150930            wdat=dateu ;
008000150923            monitor;
008100150923            if ilrc6dat>0;
008200150923               wdat=ilrc6dat;
008300150923            endif;
008400150923            on-error  ;
008500150930       //      wdat=dateu ;
008600150923            endmon  ;
008700151009       // Non elaboro nessun controalert se data <> udate
008800151009       if wdat=dateu;
008900150923       if wlna>0;
009000150923          wlnae=wlna;
009100150922          exsr sr_Calert;
009200150922       else;
009300141121       // lettura delle filiali su azorg
009400141121       setll *loval azorg01l;
009500141121       read  azorg01l;
009600141121       dow not %eof(azorg01l);
009700150930          og143=orgde3;
009800141121          // elaboro solo se record non annullato e filiale o agenzia
009900150930          // ed escludo i network esteri
010000150930          if orgfva=*blanks and (orgfag='F' or orgfag='A') and
010100150930             §ogntw<>'DPD' and §ogntw<>'EEX' and §ogntw<>'FED';
010200150922          //   elaboro solo se non è in £6
010300141121             clear fnlv55ds;
010400150922             d55tpt='6'  ;
010500141121             d55lin = orgfil;
010600150923             if wdat>0;
010700150923             d55drf=wdat;
010800150922             else;
010900150922             d55drf=dateu  ;
011000150922             endif;
011100141121             fnlv55r(fnlv55ds);
011200150922             if d55err<>*blanks;
011300150923                wlnae=orgfil  ;
011400150922                exsr sr_Calert      ;
011500150922             endif;
011600131220          endif                                   ;
011700141121          read azorg01l;
011800131220       enddo;
011900150922       endif;
012000150923       endif;
012100130905
012200130905       // Operazioni finali
012300130905       exsr RoutEnd;
012400150922       //--------------------------------------------------------------
012500150922       //?Sottomissione pgm ControAlert
012600150922       //--------------------------------------------------------------
012700150922       BEGSR Sr_Calert ;
012800150922
012900150923       clear wfes;
013000150923       exsr sr_DataFes;
013100150923       // Non elaboro nessun controalert sa data cade in giorno festivo
013200150923       if wfes='F';
013300150923          leavesr;
013400150923       endif;
013500150923
013600150923       clear fnlrc4ds;
013700150923
013800150925       // Determino i Tipi di ControAlert da elaborare
013900150923       // Verifico se è già stato elaborato il 3° Controalert
014000150925       clear tibs02ds;
014100150925       clear dsdf      ;
014200150925       t02mod='C';
014300150925       t02cod='SDF';
014400150928       t02ke1=%editc(wlnae:'X');
014500150925       t02sif=knsif;
014600150925       tibs02r(kpjba:tibs02ds);
014700150925       if t02err=*blanks;
014800150925          dsdf=t02uni;
014900150925       endif;
015000150930            clear wsdfca3;
015100150928            monitor;
015200150928            if §sdfca3 >0;
015300150928               wsdfca3=§sdfca3;
015400150928            endif;
015500150928            on-error  ;
015600150928            endmon  ;
015700150928
015800150928       // Reperisco ora di sistema per i controlli seguenti
015900150928          orasys=%dec( %time() );
016000150928          orasys4=%int(%subst(%editc(orasys:'X'):1:4));
016100150928
016200150925       // 3° CONTROALERT GIà ELABORATO  -->
016300151005       //              elaboro il controalert di tipo 1 solo se pgm richiamato
016400150928       if wsdfca3>=dateu;
016500150928          if wlna>0 and (orasys4<=whlcoa4 or whlcoa4=0);
016600151005             ILRC4TCAL0='1';
016700150923          else;
016800150923       // in questo caso non c'è niente da elaborare --> vado a fine routine
016900150923             leavesr;
017000150923          endif;
017100150923       else;
017200150928       // 3° CONTROALERT NON ANCORA ELABORATO
017300150925       // reperisco orario limite picking di filiale
017400150925          clear wolpora;
017500150925          exsr sr_repOLP;
017600150925       // Orario limite non ancora superato
017700150928       if  orasys4<=wolpora;
017800151005       // Solo se pgm richiamato (NO GufO)     --> elaboro tipi 1
017900150928          if wlna>0;
018000150928             ILRC4TCAL0='1';
018100151005        //     ILRC4TCAL1='2';
018200150928          else;
018300150928             leavesr;
018400150928          endif;
018500150925       else;
018600151005       // Orario limite            superato    --> elaboro tipi 1 e 3
018700150925          ILRC4TCAL0='1';
018800151005          ILRC4TCAL1='3';
018900151005         //  ILRC4TCAL1='2';
019000151005         //  ILRC4TCAL2='3';
019100150925       endif;
019200150923       endif;
019300150923                      ILRC4LNA=wlnae;
019400150923                      ILRC4DAT=wdat ;
019500150923                      kpjbu=fnlrc4ds;
019600150922                      kcoaz='LRC4';
019700150923       // Se pgm richiamato da gufo forzo coda batch affinchè il lavori
019800150923       // vengano sottomessi nella coda batch del terminal di partenza
019900150923                      if wlna=0  and
020000150923                            %subst(knsif:7:1)<>'P' ;
020100150923                            clear fnlv55ds;
020200150923                            d55tpt='P'  ;
020300150923                            d55lin = wlnae ;
020400150923                            d55drf=wdat    ;
020500150923                            fnlv55r(fnlv55ds);
020600150928                            kqieb='K' + %editc(d55tfp:'X') + 'SIMMED' ;
020700150923                      endif ;
020800150922                      bch10(kpjba);
020900150922       endsr;
021000130905       //--------------------------------------------------------------
021100130905       //?Operazioni iniziali.
021200130905       //--------------------------------------------------------------
021300130905       BEGSR RoutInz;
021400130905
021500130905         // Reperimento dati job
021600130905         exsr DatiJob;
021700130905       // Reperisco la data corrente
021800150925          datacur=( %date() );
021900131220          dateu=%dec(datacur);
022000150925       // Recupero ORA LIMITE INVIO CONTROALERT tab VPO
022100150925       clear tibs02ds;
022200150925       clear dvpo      ;
022300150925       t02mod='C';
022400150925       t02cod='VPO';
022500150925       t02ke1='VPO';
022600150925       t02sif=knsif;
022700150925       tibs02r(kpjba:tibs02ds);
022800150925       if t02err=*blanks;
022900150925          dvpo=t02uni;
023000150925       endif;
023100150925       // verifico contenuto campo §vpohlcoa e lo sposto in un campo
023200150928       // numerico lungo 4 per poterlo poi confrontare con l'ora di sistema
023300150928       clear whlcoa4 ;
023400150925       if %check(digits:§vpohlcoa)=0;
023500150925          whlcoa4=%dec(§vpohlcoa:4:0);
023600150925       endif;
023700130905
023800130905       endsr;
023900150923       //--------------------------------------------------------------
024000150923       //?Controllo data elaborazione se festiva
024100150923       //--------------------------------------------------------------
024200150923       BEGSR  sr_DataFes;
024300150923       wdatiso=%date(wdat);
024400150928       chain (0:0:%subdt(wdatiso:*years):
024500150923                        %subdt(wdatiso:*MONTHS)) azcln01l;
024600150923          if %found(azcln01l) and mat(%subdt(wdatiso:*days)) = 'F'
024700150923                               or pom(%subdt(wdatiso:*days)) = 'F';
024800150923       // data festiva
024900150923            wfes='F';
025000150923          endif;
025100150923       endsr;
025200150925       //--------------------------------------------------------------
025300150925       //?Reperimento Orario limite picking
025400150925       //--------------------------------------------------------------
025500150925       BEGSR  sr_repOLP ;
025600150925       wolpora=0930;
025700150925       // Filiale/data
025800150925       clear tibs02ds;
025900150925       t02cod='OLP';
026000150925       t02ke1=%editc(wlnae:'X') ;
026100150925       t02ke2=%editc(%dec(dateu):'X') ;
026200150925       t02mod='C'                     ;
026300150925       tibs02r(kpjba:tibs02ds);
026400150925       // Filiale
026500150925       if t02err='E' ;
026600150925          clear tibs02ds;
026700150925          t02cod='OLP';
026800150925          t02ke1=%editc(wlnae:'X') ;
026900150925          t02mod='C'                     ;
027000150925          tibs02r(kpjba:tibs02ds);
027100150925       endif;
027200150925       // Generico 000
027300150925       if t02err = 'E' ;
027400150925          clear tibs02ds;
027500150925          t02cod='OLP';
027600150925          t02ke1='000';
027700150925          t02mod='C'                     ;
027800150925          tibs02r(kpjba:tibs02ds);
027900150925       endif;
028000150925       if t02err<>'E' and %subst(t02uni:1:4)>*zeros  ;
028100150925          wolpora=%int(%subst(t02uni:1:4));
028200150925       endif;
028300150925       endsr;
028400130905       //--------------------------------------------------------------
028500130905       //?Reperimento Dati del job (Utente/Operativi).
028600130905       //--------------------------------------------------------------
028700130905       BEGSR DatiJob;
028800130905
028900130905         in(E) §AzUte;
029000130905         if NOT %error;
029100130905           in(E) §DatiUte;
029200130905         endif;
029300130905         if %error or RSut = *blanks;
029400130905           clear TIBS34ds;
029500130905           tibs34r(tibs34ds);
029600130905           in §AzUte;
029700130905           in §DatiUte;
029800130905         endif;
029900130905
030000130905       ENDSR;
030100130905       //--------------------------------------------------------------
030200130905       //?Operazioni finali.
030300130905       //--------------------------------------------------------------
030400130905       BEGSR RoutEnd;
030500130905
030600130905         // Chiusura pgm   ;
030700141128
030800141128             clear fnlv55ds;
030900141128             d55tla='C'     ;
031000141128             fnlv55r(fnlv55ds);
031100150925
031200150925             clear   tibs02ds;
031300150925             t02tla='C';
031400150925             tibs02r(kpjba:tibs02ds);
031500150923
031600150923         // Verifico se richiesta la chiusura del sottosistema
031700150923         if %shtdn;
031800150923            chiudi='S';
031900150923         endif;
032000150930
032100150930         // Pulisco la kpjbu  per il gufo
032200150930         clear kpjbu;
032300130905
032400130905         *inLR = *on;
032500130905
032600130905         return;
032700130905
032800130905       ENDSR;
032900130905
