000100131227     H*------------------------------------------------------------------------*
000200131227     H* Creazione/Aggiornamento Orario Previsto Arrivo
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
000705140110     fwaopa01l  uf a e           k disk
001901130906
001902130906     d*
001903140103     d Tibs34ds      e ds                  inz
001904140103     d §azute        e ds                  Extname(Azute00f)
001905140103     d                                     dtaara
001906140103     d §Datiute      e ds                  EXTname(ddatiute)
001907140103     d                                     dtaara
001908131227     d tntlmds       e ds                  extname(tntlm00f) inz
001909131227     d tntlrds       e ds                  extname(tntlr00f) inz
001910131227
001911130905     d datacur         s               d   datfmt(*iso)
001912130905     d dataiso         s               d   datfmt(*iso)
001913130905     d dataeur         s               d   datfmt(*eur)
001914140103     d w_opahha        s                   like(opahha)
001917140103     d w_hha           s                   like(opahha)
001918140108     d w_ogga          s                   like(tlogga)
001919140108     d w_gga           s                   like(tlogga)
001920140103     d wtfp            s                   like(tlmtfp)
001921140103     d pre_tfe         s                   like(caetfe)
001922140103     d dateu           s              8  0
001923140108     d ii              s              2  0
001924140103
001925140103     d $Finerec        s              1    inz(*off)
001926131227
001927131227     D WrkStringaSql   S           4500
001928131227     D                                     VARYING
010500130906     D
010501140103      // ? PROTOTIPI ?
010502140103      /copy gaitrasrc/srcprotopr,tibs34r
011219130905      //---------------------------------------------------------------
011220130905      //?Definizione procedure usate.
011221130905      //---------------------------------------------------------------
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011504130905      /free
011505131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011506130905
011507130905       // Operazioni iniziali
011508130905       exsr RoutInz;
011509131220       // lettura di tutti i terminal di partenza
011510131220       setll 'P' azcae03l;
011511131220       reade 'P' azcae03l;
011512131220       dow not %eof(azcae03l);
011513131220       // Elaboro solo se decorrente
011514140109          if dateu >= caedde and  dateu <= caedsc and caeatb=' '
011515140109          and caetfe=caetfp;
011516131220          // Accoppio il terminal di partenza con ogni terminal di arrivo
011517131220          // decorrente
011518140103             clear pre_tfe;
011519131220             setll 'A' azcae05l;
011520131220             reade 'A' azcae05l;
011521131227             dow not %eof(azcae05l);
011522131227          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
011523131227                if dateu >= c5_caedde and dateu<=c5_caedsc and c5_caeatb=' '
011524140103                and (c5_caetfp=0 or c5_caetfp=caetfe)
011525140103                // e solo se non ancora elaborato
011526140103                and c5_caetfe<>pre_tfe;
011527140103                   pre_tfe=c5_caetfe;
011528140103                   wtfp = caetfe;
011529131227                   clear w_opahha;
011530140108                   clear w_ogga  ;
011531131227                   // orario arrivo previsto
011532131227                   exsr sr_orario;
011533140103                      // Se il traino trovato prevede un transito wtfp contiene il cod. della
011534140103                      // filiale di transito e rieseguo ricerca traino con quest'ultimo per
011535140103                      // recuperare l'orario previsto della trazione
011536140108                      // Faccio al max 5 tentativi prima di uscire dal ciclo
011537140108                      clear ii;
011538140108                      dow w_opahha=0    and ii<=5;
011539131227                      // orario arrivo previsto
011540140108                         exsr sr_orario;
011541140108                         ii+=1;
011542140108                      enddo;
011544140110                   // Scrivo / aggiorno File WAOPA00F
011545140110                      chain (caetfe: c5_caetfe) waopa01l;
011546140103                      opahha=w_opahha ;
011547140103                      opaduv=dateu    ;
011548140110                      if %found (waopa01l);
011549140110                      update waopa000;
011550140103                      else  ;
011551140103                      opatfp=caetfe;
011552140103                      opatfa=c5_caetfe;
011553140110                      write  waopa000;
011554140103                      endif;
011555131227                endif;
011556131220                reade 'A' azcae05l;
011557131220             enddo;
011558131220          endif                                   ;
011559131220          reade 'P' azcae03l;
011560131220       enddo;
011561130905
011562130905       // Operazioni finali
011563130905       exsr RoutEnd;
011564131227       //--------------------------------------------------------------
011565131227       //?Determina orario previsto di arrivo
011566131227       //--------------------------------------------------------------
011567131227       BEGSR sr_orario;
011568131227       // preparo stringa sql
011569131227        exsr sr_prepSql;
011570131227       // elaboro i record estratti
011571131227        $finerec=*off;
011572131227        exec sql prepare STRINGASQL from :WrkStringaSql;
011573131227        exec sql declare TLMCsr cursor for StringaSql;
011574131227        exec sql open TLMcsr;
011575131227        dow $finerec=*off;
011576131227           exec sql Fetch TLMcsr into :tntlmds, :tntlrds;
011577131227           if sqlcod=100 or sqlcod<0;
011578131227              $finerec = *on;
011579131227              leave;
011580131227           endif;
011581131227           // se tlmtfa=tlmtff --> Ok vado su TNTLO per recuperare l'orario
011582140103           if tlmtfa=tlmtff;
011583131227              exsr sr_tlo;
011584140103              //Se trovato orario lo memorizzo ma se presente più di un traino
011585140109              // tengo l'orario più alto
011586140108              // if w_hha>0 and (w_hha<w_opahha or w_opahha=0);
011587140109              if w_hha>0 and ( (w_hha<w_opahha and w_gga> w_ogga) or
011588140109                               (w_hha>W_opahha and w_gga<= w_ogga) or
011589140108                               (w_opahha=0) );
011590140103                 w_opahha=w_hha;
011591140108                 w_ogga  =w_gga;
011592140103              endif;
011593131227           else;
011594140103           // altrimenti devo cercare un traino avente tfp = filiale di scarico
011595140103           // (se ho già trovato un diretto (w_opahha>0) non faccio niente)
011596140103              if w_opahha=0;
011597140103                 wtfp=tlmtfa;
011599140103              endif;
011600131227           endif;
011601131227        enddo;
011602131227        exec sql close TLMcsr;
011603131227       endsr;
011604131227       //-------------------------------------------------------------*
011605131227       //Preparazione stringa sql per Estrazione clienti da elaborare *
011606131227       //-------------------------------------------------------------*
011607131227       Begsr SR_prepSql;
011608140103         WrkStringaSql='select tntlm00f.*, tntlr00f.* +
011609140103                        from tntlm00f, tntlt00f, tntlr00f +
011610140103                        where tlmtrn=tlttrn and tlmdde=tltdde  +
011611140103                          and tlttrn=tlrtrn and tltdde=tlrdde  +
011612140107                                              and tlrsca=''S'' +
011613140107                          and tlmtfp=' + %editc(wtfp:'X') +
011614140103                          ' and tlmtff=' + %editc(c5_caetfe:'X') +
011615140103                          ' and ' + %editc(dateu:'X') + ' between +
011616131227                          tltdde and tltdsc';
011617131227       endsr;
011618131227       //--------------------------------------------------------------
011619140103       //?Recupero orario previsto trazione
011620131227       //--------------------------------------------------------------
011621131227       BEGSR sr_tlo;
011622140103
011623140103       clear w_hha;
011624140108       clear w_gga;
011625140103       setll (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011626140103       reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011627140103       dow not %eof(tntlo01l);
011628140103          // memorizzo orario arrivo: sempre se giorno settimana blanks,
011629140103          // solo se 0 se giorno settimana specifico
011630140103          if tlogse=' ' or w_hha = 0;
011631140103             w_hha=tlohha;
011632140108             w_gga=tlogga;
011633140103          endif;
011634140103          reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
011635140103       enddo ;
011636140103
011637131227       ENDSR;
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
