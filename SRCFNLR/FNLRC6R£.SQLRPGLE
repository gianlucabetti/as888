000100131227     H*------------------------------------------------------------------------*
000200150922     H* Verifica Controalert da elaborare e richiamo pgm scrittura file spia
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000705141121     fazorg01l  if   e           K disk
000707150923     fazcln01l  if   e           K disk
001901130906
001902130906     d*
001903150925     d tibs02ds      e ds
001904141121     d kpjba         e ds
001905150922     d fnlrc6ds      e ds
001906150922     d fnlrc4ds      e ds
001907150922     d fnlv55ds      e ds
001908140103     d Tibs34ds      e ds                  inz
001909140103     d §azute        e ds                  Extname(Azute00f)
001910140103     d                                     dtaara
001911140103     d §Datiute      e ds                  EXTname(ddatiute)
001912140103     d                                     dtaara
001913150923     d DSDF          e ds
001914150925     d Dvpo          e ds
001915150930     d og143         e ds
001916131227
001917150923     D clnmat          DS
001918150923     D  mat                    1     31    dim(31)
001919150923     D clnpom          DS
001920150923     D  pom                    1     31    dim(31)
001921130905     d datacur         s               d   datfmt(*iso)
001922140103     d dateu           s              8  0
001923150923     d wlna            s                   like(ilrc6lna)
001924150923     d wlnae           s                   like(ilrc6lna)
001925150923     d wdat            s              8  0
001926150923     d wdatiso         s               d   datfmt(*iso)
001927150925     d orasys          s              6  0
001928150928     d orasys4         s              4  0
001929150928     d whlcoa4         s                   like(orasys4)
001930150928     d wolpora         s                   like(orasys4)
001931150928     d wfes            s              1
001932150928     d wsdfca3         s                   like(dateu)
001933140103
001934150922     d bch10           pr                  extpgm('BCH10')
001935150922     d  kpjba                              likeds(kpjba)
001936150925     d tibs02r         pr                  extpgm('TIBS02R')
001937150925     d  kpjba                              likeds(kpjba)
001938150925     d  tibs02ds                           likeds(tibs02ds)
010500130906     D
010501150925     D digits          c                   '0123456789'
010502140103      // ? PROTOTIPI ?
010503140103      /copy gaitrasrc/srcprotopr,tibs34r
010504150922      /copy gaitrasrc/srcprotopr,fnlv55r
011219130905      //---------------------------------------------------------------
011220130905      //?Definizione procedure usate.
011221130905      //---------------------------------------------------------------
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011501141121     C     *entry        plist
011502141121     C                   parm                    kpjba
011503150923     C                   PARM                    chiudi            1
011504150922     c                   eval      fnlrc6ds=kpjbu
011505130905      /free
011507141126
011508130905
011509130905       // Operazioni iniziali
011510130905       exsr RoutInz;
011511150923       // Verifica parametri ricevuti
011512150923       // Linea di arrivo
011513150930            clear wlna ;
011514150923            monitor;
011515150923            if ilrc6lna>0;
011516150923               wlna=ilrc6lna;
011517150923            endif;
011518150923            on-error  ;
011519150930       //      clear wlna ;
011520150923            endmon  ;
011521150923       // Data Elaborazione
011522150930            wdat=dateu ;
011523150923            monitor;
011524150923            if ilrc6dat>0;
011525150923               wdat=ilrc6dat;
011526150923            endif;
011527150923            on-error  ;
011528150930       //      wdat=dateu ;
011529150923            endmon  ;
011533150923       // Non elaboro nessun controalert se data > udate
011534150923       if wdat<=dateu;
011535150923       if wlna>0;
011536150923          wlnae=wlna;
011537150922          exsr sr_Calert;
011538150922       else;
011539141121       // lettura delle filiali su azorg
011540141121       setll *loval azorg01l;
011541141121       read  azorg01l;
011542141121       dow not %eof(azorg01l);
011543150930          og143=orgde3;
011544141121          // elaboro solo se record non annullato e filiale o agenzia
011545150930          // ed escludo i network esteri
011546150930          if orgfva=*blanks and (orgfag='F' or orgfag='A') and
011547150930             §ogntw<>'DPD' and §ogntw<>'EEX' and §ogntw<>'FED';
011548150922          //   elaboro solo se non è in £6
011549141121             clear fnlv55ds;
011550150922             d55tpt='6'  ;
011551141121             d55lin = orgfil;
011552150923             if wdat>0;
011553150923             d55drf=wdat;
011554150922             else;
011555150922             d55drf=dateu  ;
011556150922             endif;
011557141121             fnlv55r(fnlv55ds);
011558150922             if d55err<>*blanks;
011559150923                wlnae=orgfil  ;
011560150922                exsr sr_Calert      ;
011561150922             endif;
011562131220          endif                                   ;
011563141121          read azorg01l;
011564131220       enddo;
011565150922       endif;
011566150923       endif;
011567130905
011568130905       // Operazioni finali
011569130905       exsr RoutEnd;
011570150922       //--------------------------------------------------------------
011571150922       //?Sottomissione pgm ControAlert
011572150922       //--------------------------------------------------------------
011573150922       BEGSR Sr_Calert ;
011575150922
011576150923       clear wfes;
011577150923       exsr sr_DataFes;
011579150923       // Non elaboro nessun controalert sa data cade in giorno festivo
011580150923       if wfes='F';
011581150923          leavesr;
011582150923       endif;
011583150923
011584150923       clear fnlrc4ds;
011585150923
011586150925       // Determino i Tipi di ControAlert da elaborare
011587150923       // Verifico se è già stato elaborato il 3° Controalert
011593150925       clear tibs02ds;
011594150925       clear dsdf      ;
011595150925       t02mod='C';
011596150925       t02cod='SDF';
011597150928       t02ke1=%editc(wlnae:'X');
011599150925       t02sif=knsif;
011600150925       tibs02r(kpjba:tibs02ds);
011601150925       if t02err=*blanks;
011602150925          dsdf=t02uni;
011603150925       endif;
011604150930            clear wsdfca3;
011605150928            monitor;
011606150928            if §sdfca3 >0;
011607150928               wsdfca3=§sdfca3;
011608150928            endif;
011609150928            on-error  ;
011611150928            endmon  ;
011612150928
011613150928       // Reperisco ora di sistema per i controlli seguenti
011614150928          orasys=%dec( %time() );
011615150928          orasys4=%int(%subst(%editc(orasys:'X'):1:4));
011616150928
011617150925       // 3° CONTROALERT GIà ELABORATO  -->
011618150923       //              elaboro il controalert di tipo 2 solo se pgm richiamato
011619150928       if wsdfca3>=dateu;
011620150928          if wlna>0 and (orasys4<=whlcoa4 or whlcoa4=0);
011621150923             ILRC4TCAL0='2';
011622150923          else;
011623150923       // in questo caso non c'è niente da elaborare --> vado a fine routine
011624150923             leavesr;
011625150923          endif;
011626150923       else;
011627150928       // 3° CONTROALERT NON ANCORA ELABORATO
011628150925       // reperisco orario limite picking di filiale
011629150925          clear wolpora;
011630150925          exsr sr_repOLP;
011631150925       // Orario limite non ancora superato
011632150928       if  orasys4<=wolpora;
011633150928       // Solo se pgm richiamato (NO GufO)     --> elaboro tipi 1 e 2
011634150928          if wlna>0;
011635150928             ILRC4TCAL0='1';
011636150928             ILRC4TCAL1='2';
011637150928          else;
011638150928             leavesr;
011639150928          endif;
011640150925       else;
011641150925       // Orario limite            superato    --> elaboro tipi 1, 2 e 3
011642150925          ILRC4TCAL0='1';
011643150925          ILRC4TCAL1='2';
011644150925          ILRC4TCAL2='3';
011645150925       endif;
011646150923       endif;
011647150923                      ILRC4LNA=wlnae;
011648150923                      ILRC4DAT=wdat ;
011649150923                      kpjbu=fnlrc4ds;
011650150922                      kcoaz='LRC4';
011651150923       // Se pgm richiamato da gufo forzo coda batch affinchè il lavori
011652150923       // vengano sottomessi nella coda batch del terminal di partenza
011653150923                      if wlna=0  and
011654150923                            %subst(knsif:7:1)<>'P' ;
011655150923                            clear fnlv55ds;
011656150923                            d55tpt='P'  ;
011657150923                            d55lin = wlnae ;
011658150923                            d55drf=wdat    ;
011659150923                            fnlv55r(fnlv55ds);
011660150928                            kqieb='K' + %editc(d55tfp:'X') + 'SIMMED' ;
011663150923                      endif ;
011664150922                      bch10(kpjba);
011665150922       endsr;
011884130905       //--------------------------------------------------------------
011885130905       //?Operazioni iniziali.
011886130905       //--------------------------------------------------------------
011887130905       BEGSR RoutInz;
011888130905
011890130905         // Reperimento dati job
011891130905         exsr DatiJob;
011892130905       // Reperisco la data corrente
011894150925          datacur=( %date() );
011895131220          dateu=%dec(datacur);
011896150925       // Recupero ORA LIMITE INVIO CONTROALERT tab VPO
011897150925       clear tibs02ds;
011898150925       clear dvpo      ;
011899150925       t02mod='C';
011900150925       t02cod='VPO';
011901150925       t02ke1='VPO';
011903150925       t02sif=knsif;
011904150925       tibs02r(kpjba:tibs02ds);
011905150925       if t02err=*blanks;
011906150925          dvpo=t02uni;
011914150925       endif;
011915150925       // verifico contenuto campo §vpohlcoa e lo sposto in un campo
011916150928       // numerico lungo 4 per poterlo poi confrontare con l'ora di sistema
011917150928       clear whlcoa4 ;
011918150925       if %check(digits:§vpohlcoa)=0;
011919150925          whlcoa4=%dec(§vpohlcoa:4:0);
011921150925       endif;
011922130905
011923130905       endsr;
011924150923       //--------------------------------------------------------------
011925150923       //?Controllo data elaborazione se festiva
011926150923       //--------------------------------------------------------------
011927150923       BEGSR  sr_DataFes;
011928150923       wdatiso=%date(wdat);
011929150928       chain (0:0:%subdt(wdatiso:*years):
011930150923                        %subdt(wdatiso:*MONTHS)) azcln01l;
011931150923          if %found(azcln01l) and mat(%subdt(wdatiso:*days)) = 'F'
011932150923                               or pom(%subdt(wdatiso:*days)) = 'F';
011933150923       // data festiva
011934150923            wfes='F';
011935150923          endif;
011936150923       endsr;
011937150925       //--------------------------------------------------------------
011938150925       //?Reperimento Orario limite picking
011939150925       //--------------------------------------------------------------
011940150925       BEGSR  sr_repOLP ;
011941150925       wolpora=0930;
011942150925       // Filiale/data
011943150925       clear tibs02ds;
011944150925       t02cod='OLP';
011945150925       t02ke1=%editc(wlnae:'X') ;
011946150925       t02ke2=%editc(%dec(dateu):'X') ;
011947150925       t02mod='C'                     ;
011948150925       tibs02r(kpjba:tibs02ds);
011950150925       // Filiale
011951150925       if t02err='E' ;
011952150925          clear tibs02ds;
011953150925          t02cod='OLP';
011954150925          t02ke1=%editc(wlnae:'X') ;
011955150925          t02mod='C'                     ;
011956150925          tibs02r(kpjba:tibs02ds);
011957150925       endif;
011958150925       // Generico 000
011959150925       if t02err = 'E' ;
011960150925          clear tibs02ds;
011961150925          t02cod='OLP';
011962150925          t02ke1='000';
011963150925          t02mod='C'                     ;
011964150925          tibs02r(kpjba:tibs02ds);
011966150925       endif;
011967150925       if t02err<>'E' and %subst(t02uni:1:4)>*zeros  ;
011968150925          wolpora=%int(%subst(t02uni:1:4));
011969150925       endif;
011970150925       endsr;
011982130905       //--------------------------------------------------------------
011983130905       //?Reperimento Dati del job (Utente/Operativi).
011984130905       //--------------------------------------------------------------
011985130905       BEGSR DatiJob;
011986130905
011987130905         in(E) §AzUte;
011988130905         if NOT %error;
011989130905           in(E) §DatiUte;
011990130905         endif;
011991130905         if %error or RSut = *blanks;
011992130905           clear TIBS34ds;
011993130905           tibs34r(tibs34ds);
011994130905           in §AzUte;
011995130905           in §DatiUte;
011996130905         endif;
011997130905
011998130905       ENDSR;
011999130905       //--------------------------------------------------------------
012000130905       //?Operazioni finali.
012001130905       //--------------------------------------------------------------
012002130905       BEGSR RoutEnd;
012003130905
012004130905         // Chiusura pgm   ;
012005141128
012006141128             clear fnlv55ds;
012007141128             d55tla='C'     ;
012008141128             fnlv55r(fnlv55ds);
012009150925
012010150925             clear   tibs02ds;
012011150925             t02tla='C';
012012150925             tibs02r(kpjba:tibs02ds);
012013150923
012014150923         // Verifico se richiesta la chiusura del sottosistema
012015150923         if %shtdn;
012016150923            chiudi='S';
012017150923         endif;
012018150930
012019150930         // Pulisco la kpjbu  per il gufo
012021150930         clear kpjbu;
012022130905
012023130905         *inLR = *on;
012024130905
012025130905         return;
012026130905
012027130905       ENDSR;
012028130905
