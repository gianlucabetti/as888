000100131227     H*------------------------------------------------------------------------*
000200141126     H* Creazione/Aggiornamento Orario Massimo al TFA per linea di arrivo
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000401131227     h dftactgrp(*no) actgrp(*caller)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000701131220     fazcae03l  if   e           k disk
000702131220     fazcae05l  if   e           k disk    rename(azcae000:azcae5)
000703131227     f                                     prefix(c5_)
000704131227     ftntlo01l  if   e           k disk
000705141126     fwaoma01l  uf a e           k disk
000706141126     fwaoma00f  o    e             disk    rename(waoma000:waomafis)
000707141126     f                                     prefix(f_)
001901130906
001902130906     d*
001903140103     d Tibs34ds      e ds                  inz
001904140103     d §azute        e ds                  Extname(Azute00f)
001905140103     d                                     dtaara
001906140103     d §Datiute      e ds                  EXTname(ddatiute)
001907140103     d                                     dtaara
001908131227     d tntlmds       e ds                  extname(tntlm00f) inz
001909131227     d tntlrds       e ds                  extname(tntlr00f) inz
001910141126      * Parametri per richiamo a TRUL09R
001911141126     d Trul09ds      e ds
001912141126     d  mec                   19    108  0 dim(30)
001913131227
001914130905     d datacur         s               d   datfmt(*iso)
001915130905     d dataiso         s               d   datfmt(*iso)
001916130905     d dataeur         s               d   datfmt(*eur)
001917141126     d w_omahmx        s                   like(omahmx)
001918141126     d w_hmx           s                   like(omahmx)
001919140108     d w_ogga          s                   like(tlogga)
001920140108     d w_gga           s                   like(tlogga)
001921140103     d wtfp            s                   like(tlmtfp)
001922140103     d pre_tfe         s                   like(caetfe)
001923140103     d dateu           s              8  0
001924141126     d ii              s              2  0
001925141126     d ix              s              3  0
001926141126     d sga             S              1    DIM(999) inz
001927140103
001928140103     d $Finerec        s              1    inz(*off)
001929131227
001930131227     D WrkStringaSql   S           4500
001931131227     D                                     VARYING
010500130906     D
010501140103      // ? PROTOTIPI ?
010502141126     D TRUL09R         pr                  extpgm('TRUL09R')
010504141126     D  TRUL09DS                           likeds(TRUL09DS)
010505140103      /copy gaitrasrc/srcprotopr,tibs34r
011219130905      //---------------------------------------------------------------
011220130905      //?Definizione procedure usate.
011221130905      //---------------------------------------------------------------
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011504130905      /free
011505131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011506130905
011507141126       EXEC sql  delete from WAOMA00F          ;
011508141126
011510141126
011511130905       // Operazioni iniziali
011512130905       exsr RoutInz;
011513131220       // lettura di tutti i terminal di partenza
011514131220       setll 'P' azcae03l;
011515131220       reade 'P' azcae03l;
011516131220       dow not %eof(azcae03l);
011517131220       // Elaboro solo se decorrente
011518140109          if dateu >= caedde and  dateu <= caedsc and caeatb=' '
011519140109          and caetfe=caetfp;
011520131220          // Accoppio il terminal di partenza con ogni terminal di arrivo
011521131220          // decorrente
011522140103             clear pre_tfe;
011523131220             setll 'A' azcae05l;
011524131220             reade 'A' azcae05l;
011525131227             dow not %eof(azcae05l);
011526131227          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
011527131227                if dateu >= c5_caedde and dateu<=c5_caedsc and c5_caeatb=' '
011528141126                and c5_caetfp=0
011529141126          //    and (c5_caetfp=0 or c5_caetfp=caetfe)
011530140103                // e solo se non ancora elaborato
011531140103                and c5_caetfe<>pre_tfe;
011532140103                   pre_tfe=c5_caetfe;
011533140103                   wtfp = caetfe;
011534141126                   clear w_omahmx;
011535140108                   clear w_ogga  ;
011536131227                   // orario arrivo previsto
011537131227                   exsr sr_orario;
011538140103                      // Se il traino trovato prevede un transito wtfp contiene il cod. della
011539140103                      // filiale di transito e rieseguo ricerca traino con quest'ultimo per
011540140103                      // recuperare l'orario previsto della trazione
011541140108                      // Faccio al max 5 tentativi prima di uscire dal ciclo
011542140108                      clear ii;
011543141126                      dow w_omahmx=0    and ii<=5;
011544131227                      // orario arrivo previsto
011545140108                         exsr sr_orario;
011546140108                         ii+=1;
011547140108                      enddo;
011548141126                   // Scrivo / aggiorno File WAOMA00F
011549141126                      chain (c5_caetfe:000) waoma01l;
011550141126                      if %found (waoma01l);
011556141126       // devo tenere l'orario massimo
011557141126                   if w_hmx>0 and ( (w_omahmx<omahmx and w_ogga> sga(c5_caetfe))
011558141126                           or (w_omahmx>omahmx and w_ogga>= sga(c5_caetfe)));
011559141126                         omahmx=w_omahmx ;
011561141126                         sga(c5_caetfe)=w_ogga;
011565141126                         omaduv=dateu    ;
011566141126                         update waoma000;
011567141126                       endif;
011568140103                      else  ;
011569141126                         clear omatfp ;
011570141126                         omalna=c5_caetfe;
011571141126                         omahmx=w_omahmx ;
011572141126
011574141126                         eval sga(c5_caetfe)=w_ogga;
011575141126
011576141126                         omaduv=dateu    ;
011577141126                         write  waoma000;
011578140103                      endif;
011579131227                endif;
011580131220                reade 'A' azcae05l;
011581131220             enddo;
011582131220          endif                                   ;
011583131220          reade 'P' azcae03l;
011584131220       enddo;
011585141126
011586141126       feod waoma01l;
011588141126       // Lettura dei record scritti per espolsione per filiale di arrivo
011589141126       setll *loval waoma01l;
011590141126       read waoma01l;
011591141126       dow not %eof(waoma01l);
011592141126       // reperisco tutti i secondi livello del terminal di arrivo e duplico il
011593141126       // record per ognuno di essi
011594141126          clear trul09ds;
011595141126          i09mod='TFU';
011596141126          i09tfa=omalna;
011597141126          I09dta = dateu;
011598141126          trul09r(trul09ds);
011599141126          for ix=1 to %elem(mec);
011600141126             if mec(ix)=0;
011601141126                leave;
011602141126             endif;
011603141126             if mec(ix)<> omalna;
011604141126             clear f_omatfp;
011605141126             f_omalna=mec(ix);
011606141126             f_omahmx=omahmx;
011607141126             f_omaduv=omaduv;
011608141126             write waomafis;
011609141126             endif;
011610141126          endfor;
011611141126          read waoma01l;
011612141126       enddo;
011613130905
011614130905       // Operazioni finali
011615130905       exsr RoutEnd;
011616131227       //--------------------------------------------------------------
011617131227       //?Determina orario previsto di arrivo
011618131227       //--------------------------------------------------------------
011619131227       BEGSR sr_orario;
011620131227       // preparo stringa sql
011621131227        exsr sr_prepSql;
011622131227       // elaboro i record estratti
011623131227        $finerec=*off;
011624131227        exec sql prepare STRINGASQL from :WrkStringaSql;
011625131227        exec sql declare TLMCsr cursor for StringaSql;
011626131227        exec sql open TLMcsr;
011627131227        dow $finerec=*off;
011628131227           exec sql Fetch TLMcsr into :tntlmds, :tntlrds;
011629131227           if sqlcod=100 or sqlcod<0;
011630131227              $finerec = *on;
011631131227              leave;
011632131227           endif;
011633131227           // se tlmtfa=tlmtff --> Ok vado su TNTLO per recuperare l'orario
011634141125           if tlmtfa=tlmtff and tlrfil=tlmtfa;
011635131227              exsr sr_tlo;
011636140103              //Se trovato orario lo memorizzo ma se presente più di un traino
011637140109              // tengo l'orario più alto
011638140108              // if w_hha>0 and (w_hha<w_opahha or w_opahha=0);
011639141126              if w_hmx>0 and ( (w_hmx<w_omahmx and w_gga> w_ogga) or
011640141126                               (w_hmx>W_omahmx and w_gga>= w_ogga) or
011641141126                               (w_omahmx=0) );
011642141126                 w_omahmx=w_hmx;
011643140108                 w_ogga  =w_gga;
011644140103              endif;
011645131227           else;
011646140103           // altrimenti devo cercare un traino avente tfp = filiale di scarico
011647141126           // (se ho già trovato un diretto (w_omahmx>0) non faccio niente)
011648141126              if w_omahmx=0;
011649140103                 wtfp=tlmtfa;
011650140103              endif;
011651131227           endif;
011652131227        enddo;
011653131227        exec sql close TLMcsr;
011654131227       endsr;
011655131227       //-------------------------------------------------------------*
011656131227       //Preparazione stringa sql per Estrazione clienti da elaborare *
011657131227       //-------------------------------------------------------------*
011658131227       Begsr SR_prepSql;
011659140103         WrkStringaSql='select tntlm00f.*, tntlr00f.* +
011660140103                        from tntlm00f, tntlt00f, tntlr00f +
011661140103                        where tlmtrn=tlttrn and tlmdde=tltdde  +
011662140103                          and tlttrn=tlrtrn and tltdde=tlrdde  +
011663140107                                              and tlrsca=''S'' +
011664140107                          and tlmtfp=' + %editc(wtfp:'X') +
011665140103                          ' and tlmtff=' + %editc(c5_caetfe:'X') +
011666140103                          ' and ' + %editc(dateu:'X') + ' between +
011667131227                          tltdde and tltdsc';
011668131227       endsr;
011669131227       //--------------------------------------------------------------
011670140103       //?Recupero orario previsto trazione
011671131227       //--------------------------------------------------------------
011672131227       BEGSR sr_tlo;
011673140103
011674141126       clear w_hmx;
011675140108       clear w_gga;
011676140103       setll (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011677140103       reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011678140103       dow not %eof(tntlo01l);
011679140103          // memorizzo orario arrivo: sempre se giorno settimana blanks,
011680140103          // solo se 0 se giorno settimana specifico
011681141126          if tlogse=' ' or w_hmx = 0;
011682141126             w_hmx=tlohha;
011683140108             w_gga=tlogga;
011684140103          endif;
011685140103          reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011686140103       enddo ;
011687140103
011688131227       ENDSR;
011884130905       //--------------------------------------------------------------
011885130905       //?Operazioni iniziali.
011886130905       //--------------------------------------------------------------
011887130905       BEGSR RoutInz;
011888130905
011890130905         // Reperimento dati job
011891130905         exsr DatiJob;
011892130905       // Reperisco la data corrente
011893130905          datacur=(%date());
011894131220          dateu=%dec(datacur);
011897130905
011898130905       endsr;
011899130905       //--------------------------------------------------------------
011900130905       //?Reperimento Dati del job (Utente/Operativi).
011901130905       //--------------------------------------------------------------
011902130905       BEGSR DatiJob;
011903130905
011904130905         in(E) §AzUte;
011905130905         if NOT %error;
011906130905           in(E) §DatiUte;
011907130905         endif;
011908130905         if %error or RSut = *blanks;
011909130905           clear TIBS34ds;
011910130905           tibs34r(tibs34ds);
011911130905           in §AzUte;
011912130905           in §DatiUte;
011913130905         endif;
011914130905
011915130905       ENDSR;
011916130905       //--------------------------------------------------------------
011917130905       //?Operazioni finali.
011918130905       //--------------------------------------------------------------
011919130905       BEGSR RoutEnd;
011920130905
011921130905         // Chiusura pgm   ;
011922130905
011923130905         *inLR = *on;
011924130905
011925130905         return;
011926130905
011927130905       ENDSR;
011928130905
