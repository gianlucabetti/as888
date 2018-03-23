000100131227     H*------------------------------------------------------------------------*
000200141126     H* Creazione/Aggiornamento Orario Massimo al TFA per linea di arrivo
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500131227     h dftactgrp(*no) actgrp(*caller)
000600970910     F*------------------------------------------------------------------------*
000700970910     F* DATA BASE
000800970910     F*------------------------------------------------------------------------*
000900131220     fazcae03l  if   e           k disk
001000131220     fazcae05l  if   e           k disk    rename(azcae000:azcae5)
001100131227     f                                     prefix(c5_)
001200131227     ftntlo01l  if   e           k disk
001300151028     fazcas01l  if   e           k disk
001400151026     fwaoma11l  uf a e           k disk
001500130906
001600130906     d*
001700151028     d fnlv55ds      e ds
001800151027     d kpjba         e ds
001900151027     d Tibs34ds      e ds                  inz
002000140103     d §azute        e ds                  Extname(Azute00f)
002100140103     d                                     dtaara
002200140103     d §Datiute      e ds                  EXTname(ddatiute)
002300140103     d                                     dtaara
002400131227     d tntlmds       e ds                  extname(tntlm00f) inz
002500131227     d tntlrds       e ds                  extname(tntlr00f) inz
002600131227
002700130905     d datacur         s               d   datfmt(*iso)
002800130905     d dataiso         s               d   datfmt(*iso)
002900130905     d dataeur         s               d   datfmt(*eur)
003000141126     d w_omahmx        s                   like(omahmx)
003100141126     d w_hmx           s                   like(omahmx)
003200140108     d w_ogga          s                   like(tlogga)
003300140108     d w_gga           s                   like(tlogga)
003400140103     d wtfp            s                   like(tlmtfp)
003500140103     d pre_tfe         s                   like(caetfe)
003600140103     d dateu           s              8  0
003700141126     d ii              s              2  0
003800141126     d ix              s              3  0
003900151028     d addtrans        s              4  0
004000141126     d sga             S              1    DIM(999) inz
004100140103
004200140103     d $Finerec        s              1    inz(*off)
004300131227
004400131227     D WrkStringaSql   S           4500
004500131227     D                                     VARYING
004600130906     D
004700140103      /copy gaitrasrc/srcprotopr,tibs34r
004800151028      /copy gaitrasrc/srcprotopr,fnlv55r
004900970910     C*------------------------------------------------------------------------*
005000970910     C* MAIN LINES
005100970910     C*------------------------------------------------------------------------*
005200151027     c     *entry        plist
005300151027     c                   parm                    kpjba
005400151027     c                   movel     kpjbu         parelab           1
005500151027
005600130905      /free
005700131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005800151028
005900151028       // Operazioni iniziali
006000151028       exsr RoutInz;
006100130905
006200151027      // pulisco gli orari sul file senza cancellare
006300151027       setll *loval   waoma11l ;
006400151027       read waoma11l   ;
006500151027       dow not %eof(waoma11l)  ;
006600151028
006700151027       if  omatfp=0 and omatfae=0 ;
006800151028       // pulisco orari solo dei terminal
006900151028             clear fnlv55ds;
007000151028             d55tpt='A'   ;
007100151028             d55lin = %int(omalna) ;
007200151028             d55drf = dateu                  ;
007300151028             fnlv55r(fnlv55ds);
007400151028
007500151028          if omalna = d55tfa  ;
007600151028            clear omahmx  ;
007700151028            update waoma010  ;
007800151028          endif  ;
007900151028       endif  ;
008000151027
008100151027       read waoma11l   ;
008200151027       enddo ;
008300151027
008400151027       unlock waoma11l  ;
008500141126
008600141126
008700131220       // lettura di tutti i terminal di partenza
008800131220       setll 'P' azcae03l;
008900131220       reade 'P' azcae03l;
0090001510261      dow not %eof(azcae03l);
009100151026
009200131220       // Elaboro solo se decorrente
0093001510262         if dateu >= caedde and  dateu <= caedsc and caeatb=' '
009400140109          and caetfe=caetfp;
009500151026
009600131220          // Accoppio il terminal di partenza con ogni terminal di arrivo
009700131220          // decorrente
009800140103             clear pre_tfe;
009900131220             setll 'A' azcae05l;
010000131220             reade 'A' azcae05l;
010100151027
0102001510263            dow not %eof(azcae05l);
010300131227          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
0104001510264               if dateu >= c5_caedde and dateu<=c5_caedsc and c5_caeatb=' '
010500141126                and c5_caetfp=0
010600141126          //    and (c5_caetfp=0 or c5_caetfp=caetfe)
010700140103                // e solo se non ancora elaborato
010800140103                and c5_caetfe<>pre_tfe;
010900140103                   pre_tfe=c5_caetfe;
011000140103                   wtfp = caetfe;
011100141126                   clear w_omahmx;
011200140108                   clear w_ogga  ;
011300151027
011400151027       // Se è presente una eccezione terminal non lo devo elaborare
011500151027       //  in quanto si tratta di un secondo livello ai fini della traini
011600151027                      chain (c5_caetfe:000) waoma11l;
011700151027 4a                   if (%found (waoma11l)and omatfae=0) or
011800151027                         not %found (waoma11l) ;
011900151027
012000131227                   // orario arrivo previsto
012100131227                   exsr sr_orario;
012200151027
012300140103                      // Se il traino trovato prevede un transito wtfp contiene il cod. della
012400140103                      // filiale di transito e rieseguo ricerca traino con quest'ultimo per
012500140103                      // recuperare l'orario previsto della trazione
012600140108                      // Faccio al max 5 tentativi prima di uscire dal ciclo
012700140108                      clear ii;
012800151028 5                    dow wtfp>0        and ii<=5;
012900131227                      // orario arrivo previsto
013000140108                         exsr sr_orario;
013100140108                         ii+=1;
013200151026 5                    enddo;
013300151027
013400141126                   // Scrivo / aggiorno File WAOMA00F
013500151026 5                    if %found (waoma11l);
013600151027
013700141126       // devo tenere l'orario massimo
013800151027 6                 if w_omahmx>omahmx  ;
013900141126                         omahmx=w_omahmx ;
014000141126                         sga(c5_caetfe)=w_ogga;
014100141126                         omaduv=dateu    ;
014200151026                         update waoma010;
014300151027 6                 endif;
014400151027
014500151026 x5                   else  ;
014600151027                         clear waoma010  ;
014700141126                         omalna=c5_caetfe;
014800141126                         omahmx=w_omahmx ;
014900141126
015000141126                         eval sga(c5_caetfe)=w_ogga;
015100141126
015200141126                         omaduv=dateu    ;
015300151026                         write  waoma010;
015400151026 5                    endif;
015500151026 4              endif;
015600151027 4a             endif;
015700131220                reade 'A' azcae05l;
015800151026 3           enddo;
015900151026 2        endif                                   ;
016000131220          reade 'P' azcae03l;
016100151026 1     enddo;
016200141126
016300151026       feod waoma11l;
016400130905
016500130905       // Operazioni finali
016600130905       exsr RoutEnd;
016700151027
016800131227       //--------------------------------------------------------------
016900131227       //?Determina orario previsto di arrivo
017000131227       //--------------------------------------------------------------
017100131227       BEGSR sr_orario;
017200131227       // preparo stringa sql
017300131227        exsr sr_prepSql;
017400131227       // elaboro i record estratti
017500131227        $finerec=*off;
017600131227        exec sql prepare STRINGASQL from :WrkStringaSql;
017700131227        exec sql declare TLMCsr cursor for StringaSql;
017800131227        exec sql open TLMcsr;
017900151028        clear wtfp       ;
018000151028
0181001510261       dow $finerec=*off;
018200131227           exec sql Fetch TLMcsr into :tntlmds, :tntlrds;
0183001510262          if sqlcod=100 or sqlcod<0;
018400131227              $finerec = *on;
018500131227              leave;
0186001510262          endif;
018700131227           // se tlmtfa=tlmtff --> Ok vado su TNTLO per recuperare l'orario
0188001510271a         if     tlmtrn<9000000  ;
0189001510272          if tlmtfa=tlmtff and tlrfil=tlmtfa  ;
019000151027
019100131227              exsr sr_tlo;
019200140103              //Se trovato orario lo memorizzo ma se presente più di un traino
019300140109              // tengo l'orario più alto
019400151026              // if w_hha>0 and (w_hha<w_opahha or w_opahha=0);
019500151026              // if w_hmx>0 and ( (w_hmx<w_omahmx and w_gga> w_ogga) or
019600151026              //                 (w_hmx>W_omahmx and w_gga>= w_ogga) or
019700151026              //                 (w_omahmx=0) );
019800151026              //   w_omahmx=w_hmx;
019900151026              //   w_ogga  =w_gga;
020000151026              // endif;
020100151026
0202001510263              if w_hmx>0 and w_hmx>w_omahmx  ;
020300151026                   w_omahmx=w_hmx;
020400151026                   w_ogga  =w_gga;
0205001510263             endif;
020600151026
020700151026x2         else;
020800140103           // altrimenti devo cercare un traino avente tfp = filiale di scarico
020900141126           // (se ho già trovato un diretto (w_omahmx>0) non faccio niente)
021000151028 3         //   if w_omahmx=0;
021100140103                 wtfp=tlmtfa;
021200151028 3         //   endif;
021300151026 2         endif;
021400151027 1a        endif;
021500151026 1      enddo;
021600131227        exec sql close TLMcsr;
021700131227       endsr;
021800131227       //-------------------------------------------------------------*
021900131227       //Preparazione stringa sql per Estrazione clienti da elaborare *
022000131227       //-------------------------------------------------------------*
022100131227       Begsr SR_prepSql;
022200140103         WrkStringaSql='select tntlm00f.*, tntlr00f.* +
022300140103                        from tntlm00f, tntlt00f, tntlr00f +
022400140103                        where tlmtrn=tlttrn and tlmdde=tltdde  +
022500140103                          and tlttrn=tlrtrn and tltdde=tlrdde  +
022600140107                                              and tlrsca=''S'' +
022700140107                          and tlmtfp=' + %editc(wtfp:'X') +
022800140103                          ' and tlmtff=' + %editc(c5_caetfe:'X') +
022900140103                          ' and ' + %editc(dateu:'X') + ' between +
023000131227                          tltdde and tltdsc';
023100131227       endsr;
023200131227       //--------------------------------------------------------------
023300140103       //?Recupero orario previsto trazione
023400131227       //--------------------------------------------------------------
023500131227       BEGSR sr_tlo;
023600140103
023700141126       clear w_hmx;
023800140108       clear w_gga;
023900140103       setll (tlrtrn:tlrdde:tlrnfm) tntlo01l;
024000140103       reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
024100140103       dow not %eof(tntlo01l);
024200140103          // memorizzo orario arrivo: sempre se giorno settimana blanks,
024300140103          // solo se 0 se giorno settimana specifico
024400141126          if tlogse=' ' or w_hmx = 0;
024500141126             w_hmx=tlohha;
024600140108             w_gga=tlogga;
024700151028
024800151028          // se si tratta di transito dai tempi di trazione verifico se devo aggiungere
024900151028          //  24 ore per il transito
025000151028          clear azcas000  ;
025100151028          clear addtrans  ;
025200151028
025300151028          if tlmtfp<>caetfe   ;
025400151028          setgt  ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
025500151028          readpe ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
025600151028          dow not  %eof(azcas01l) and casdde>dateu ;
025700151028          readpe ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
025800151028          enddo  ;
025900151028
026000151028          if  not  %eof(azcas01l)  and castin>=48  ;
026100151028          addtrans=2400 ;
026200151028          endif       ;
026300151028          if  not  %eof(azcas01l)  and castin>=72  ;
026400151028          addtrans=4800 ;
026500151028          endif       ;
026600151028
026700151028          endif  ;
026800151028
026900151026          // trovo l'orario con la somma dei giorni di trazione
027000151026          if w_gga='B'  ;
027100151026          w_hmx=w_hmx+2400;
027200151028          endif  ;
027300151028
027400151027          if w_gga='C'  ;
027500151026          w_hmx=w_hmx+4800;
027600151026          endif  ;
027700151028
027800151027          if w_gga='D'  ;
027900151026          w_hmx=w_hmx+7200;
028000151026          endif  ;
028100151028
028200151028          if w_hmx+addtrans<92  ;
028300151028          w_hmx=w_hmx+addtrans ;
028400151028          endif  ;
028500151026
028600140103          endif;
028700140103          reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
028800140103       enddo ;
028900140103
029000131227       ENDSR;
029100130905       //--------------------------------------------------------------
029200130905       //?Operazioni iniziali.
029300130905       //--------------------------------------------------------------
029400130905       BEGSR RoutInz;
029500130905
029600130905         // Reperimento dati job
029700130905         exsr DatiJob;
029800130905       // Reperisco la data corrente
029900130905          datacur=(%date());
030000131220          dateu=%dec(datacur);
030100130905
030200130905       endsr;
030300130905       //--------------------------------------------------------------
030400130905       //?Reperimento Dati del job (Utente/Operativi).
030500130905       //--------------------------------------------------------------
030600130905       BEGSR DatiJob;
030700130905
030800130905         in(E) §AzUte;
030900130905         if NOT %error;
031000130905           in(E) §DatiUte;
031100130905         endif;
031200130905         if %error or RSut = *blanks;
031300130905           clear TIBS34ds;
031400130905           tibs34r(tibs34ds);
031500130905           in §AzUte;
031600130905           in §DatiUte;
031700130905         endif;
031800130905
031900130905       ENDSR;
032000130905       //--------------------------------------------------------------
032100130905       //?Operazioni finali.
032200130905       //--------------------------------------------------------------
032300130905       BEGSR RoutEnd;
032400130905
032500130905         // Chiusura pgm   ;
032600151028             clear fnlv55ds;
032700151028             d55tla='C'   ;
032800151028             fnlv55r(fnlv55ds);
032900130905
033000130905         *inLR = *on;
033100130905
033200130905         return;
033300130905
033400130905       ENDSR;
033500130905
