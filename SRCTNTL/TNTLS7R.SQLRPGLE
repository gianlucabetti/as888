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
001700151103     d tntls7ds      e ds
001800151103     d fnlv55ds      e ds
001900151027     d kpjba         e ds
002000151027     d Tibs34ds      e ds                  inz
002100140103     d §azute        e ds                  Extname(Azute00f)
002200140103     d                                     dtaara
002300140103     d §Datiute      e ds                  EXTname(ddatiute)
002400140103     d                                     dtaara
002500131227     d tntlmds       e ds                  extname(tntlm00f) inz
002600131227     d tntlrds       e ds                  extname(tntlr00f) inz
002700131227
002800130905     d datacur         s               d   datfmt(*iso)
002900130905     d dataiso         s               d   datfmt(*iso)
003000130905     d dataeur         s               d   datfmt(*eur)
003100141126     d w_omahmx        s                   like(omahmx)
003200151030     d w_omahmxB       s                   like(omahmx)
003300151030     d w_omahmxC       s                   like(omahmx)
003400151030     d w_omahmx0       s                   like(omahmx)
003500141126     d w_hmx           s                   like(omahmx)
003600140108     d w_ogga          s                   like(tlogga)
003700140108     d w_gga           s                   like(tlogga)
003800140103     d wtfp            s                   like(tlmtfp)
003900140103     d pre_tfe         s                   like(caetfe)
004000151030     d castin_t        s                   like(castin)
004100140103     d dateu           s              8  0
004200141126     d ii              s              2  0
004300141126     d ix              s              3  0
004400151030     d indxb           s              4  0
004500151030     d indxc           s              4  0
004600151030     d indx            s              4  0
004700151028     d addtrans        s              4  0
004800151030     d diff            s              4  0
004900151030     d tfpb            S              3  0 DIM(999) inz
005000151030     d tfpc            S              3  0 DIM(999) inz
005100140103
005200140103     d $Finerec        s              1    inz(*off)
005300131227
005400131227     D WrkStringaSql   S           4500
005500131227     D                                     VARYING
005600130906     D
005700140103      /copy gaitrasrc/srcprotopr,tibs34r
005800151028      /copy gaitrasrc/srcprotopr,fnlv55r
005900970910     C*------------------------------------------------------------------------*
006000970910     C* MAIN LINES
006100970910     C*------------------------------------------------------------------------*
006200151027     c     *entry        plist
006300151027     c                   parm                    kpjba
006400151103     c                   movel     kpjbu         tntls7ds
006500151027
006600130905      /free
006700131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006800151028
006900151028       // Operazioni iniziali
007000151028       exsr RoutInz;
007100130905
007200151027      // pulisco gli orari sul file senza cancellare
007300151103      //  solo se ho elaborato tutto
007400151103       if tls7tela<>'A'  ;
007500151103
007600151027       setll *loval   waoma11l ;
007700151027       read waoma11l   ;
007800151027       dow not %eof(waoma11l)  ;
007900151028
008000151027       if  omatfp=0 and omatfae=0 ;
008100151028       // pulisco orari solo dei terminal
008200151028             clear fnlv55ds;
008300151028             d55tpt='A'   ;
008400151028             d55lin = %int(omalna) ;
008500151103             d55drf = TLS7DRF                ;
008600151028             fnlv55r(fnlv55ds);
008700151028
008800151028          if omalna = d55tfa  ;
008900151028            clear omahmx  ;
009000151028            update waoma010  ;
009100151028          endif  ;
009200151028       endif  ;
009300151027
009400151027       read waoma11l   ;
009500151027       enddo ;
009600151027
009700151027       unlock waoma11l  ;
009800151103       endif  ;
009900141126
010000141126
010100151030             clear pre_tfe;
010200151030             setll 'A' azcae05l;
010300151030             reade 'A' azcae05l;
010400151030
0105001510301            dow not %eof(azcae05l);
010600151030          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
010700151030                // e solo se non ancora elaborato
0108001511032               if TLS7DRF >= c5_caedde and TLS7DRF<=c5_caedsc and c5_caeatb=' '
010900151030                and c5_caetfp=0
011000151030                and c5_caetfe<>pre_tfe;
011100151030
011200151030                pre_tfe=c5_caetfe;
011300151030
011400151030                   clear w_omahmxC;
011500151030                   clear w_omahmxB;
011600151030                   clear indxB    ;
011700151030                   clear indxC    ;
011800151030                   clear tfpb     ;
011900151030                   clear tfpc     ;
012000151030                   clear w_ogga   ;
012100151030
012200151030       // Se è presente una eccezione terminal non lo devo elaborare
012300151030       //  in quanto si tratta di un secondo livello ai fini della traini
012400151103
012500151030                      chain(n) (c5_caetfe:000) waoma11l;
0126001511032a                    if (%found(waoma11l) and omatfae=0) or
012700151103                         not %found(waoma11l) ;
012800151030
012900151103       // Se solo in aggiunta non elaboro se già presente con orario impostato
0130001511032b     if tls7tela<>'A'  or not %found(waoma11l) or omahmx=0 ;
013100151103
013200131220       // lettura di tutti i terminal di partenza
013300131220       setll 'P' azcae03l;
013400131220       reade 'P' azcae03l;
0135001510303      dow not %eof(azcae03l);
013600151026
013700131220       // Elaboro solo se decorrente
0138001511034         if TLS7DRF>= caedde and  TLS7DRF<= caedsc and caeatb=' '
013900140109          and caetfe=caetfp;
014000151026
014100151030       // escludo se ha eccezione di terminal di arrivo ( do per scontato che sia eccezione
014200151030       //  anche per terminal di partenza)
014300151030                      chain(n) (caetfe:000) waoma11l;
0144001510304a                    if (%found (waoma11l)and omatfae=0) or
014500151030                         not %found (waoma11l) ;
014600140103                   wtfp = caetfe;
014700151027
014800131227                   // orario arrivo previsto
014900131227                   exsr sr_orario;
015000151027
015100140103                      // Se il traino trovato prevede un transito wtfp contiene il cod. della
015200140103                      // filiale di transito e rieseguo ricerca traino con quest'ultimo per
015300140103                      // recuperare l'orario previsto della trazione
015400140108                      // Faccio al max 5 tentativi prima di uscire dal ciclo
015500140108                      clear ii;
015600151028 5                    dow wtfp>0        and ii<=5;
015700131227                      // orario arrivo previsto
015800140108                         exsr sr_orario;
015900140108                         ii+=1;
016000151026 5                    enddo;
016100151026 4              endif;
016200151030 4a             endif;
016300151030
016400151030               reade 'P' azcae03l;
016500151026 3           enddo;
016600151030
016700151030       // devo tenere l'orario massimo ma il più numeroso dei due
016800151030 3     if indxb>0 or indxc>0 ;
016900151030
017000151030 4     if indxb>=indxc   ;
017100151030       eval   w_omahmx0=w_omahmxb ;
017200151030       clear indxb      ;
017300151030       clear w_omahmxb  ;
017400151030       else  ;
017500151030       eval   w_omahmx0=w_omahmxC ;
017600151030       clear indxC      ;
017700151030       clear w_omahmxC  ;
017800151030 4     endif ;
017900151030                   // Scrivo / aggiorno File WAOMA10F
018000151030                      chain (c5_caetfe:000) waoma11l;
018100151030 4                    if %found (waoma11l);
018200151030                         omahmx=w_omahmx0;
018300151030                         omaduv=dateu    ;
018400151030                         update waoma010;
018500151030 x4                   else  ;
018600151030                         clear waoma010  ;
018700151030                         omalna=c5_caetfe;
018800151030                         omahmx=w_omahmx0;
018900151030
019000151030                         omaduv=dateu    ;
019100151030                         write  waoma010;
019200151030 4                    endif;
019300151030
019400151030        // se presenti eccezioni le scrivo
019500151030 4      if indxb>0   ;
019600151030        indxb=1   ;
019700151030 5      dow tfpb(indxb)>0   ;
019800151030            chain (c5_caetfe:tfpb(indxb)) waoma11l;
019900151030 6                    if %found (waoma11l);
020000151030                         omahmx=w_omahmxb;
020100151030                         omaduv=dateu    ;
020200151030                         update waoma010;
020300151030 x6                   else  ;
020400151030                         clear waoma010  ;
020500151030                         omalna=c5_caetfe;
020600151030                         omatfp=tfpb(indxb) ;
020700151030                         omahmx=w_omahmxB;
020800151030
020900151030                         omaduv=dateu    ;
021000151030                         write  waoma010;
021100151030 6                    endif;
021200151030        indxb=indxb+1  ;
021300151030 5                    enddo;
021400151030 4                    endif;
021500151030 4      if indxc>0   ;
021600151030        indxc=1   ;
021700151030 5      dow tfpc(indxc)>0   ;
021800151030            chain (c5_caetfe:tfpc(indxc)) waoma11l;
021900151030 6                    if %found (waoma11l);
022000151030                         omahmx=w_omahmxc;
022100151030                         omaduv=dateu    ;
022200151030                         update waoma010;
022300151030 x6                   else  ;
022400151030                         clear waoma010  ;
022500151030                         omalna=c5_caetfe;
022600151030                         omatfp=tfpc(indxc) ;
022700151030                         omahmx=w_omahmxc;
022800151030
022900151030                         omaduv=dateu    ;
023000151030                         write  waoma010;
023100151030 6                    endif;
023200151030        indxc=indxc+1  ;
023300151030 5                    enddo;
023400151030 4                    endif;
023500151030 3                    endif;
023600151030
023700151103 2b       endif                                   ;
023800151103 2a       endif                                   ;
023900151030 2        endif                                   ;
024000151030
024100151030        reade 'A' azcae05l;
024200151026 1     enddo;
024300141126
024400151026       feod waoma11l;
024500130905
024600130905       // Operazioni finali
024700130905       exsr RoutEnd;
024800151027
024900131227       //--------------------------------------------------------------
025000131227       //?Determina orario previsto di arrivo
025100131227       //--------------------------------------------------------------
025200131227       BEGSR sr_orario;
025300131227       // preparo stringa sql
025400131227        exsr sr_prepSql;
025500131227       // elaboro i record estratti
025600131227        $finerec=*off;
025700131227        exec sql prepare STRINGASQL from :WrkStringaSql;
025800131227        exec sql declare TLMCsr cursor for StringaSql;
025900131227        exec sql open TLMcsr;
026000151028        clear wtfp       ;
026100151028
0262001510261       dow $finerec=*off;
026300131227           exec sql Fetch TLMcsr into :tntlmds, :tntlrds;
0264001510262          if sqlcod=100 or sqlcod<0;
026500131227              $finerec = *on;
026600131227              leave;
0267001510262          endif;
026800131227           // se tlmtfa=tlmtff --> Ok vado su TNTLO per recuperare l'orario
0269001510271a         if     tlmtrn<9000000  ;
027000151030
0271001510302          if tlmtfa=tlmtff ;
0272001510302a         if tlrfil=tlmtfa  ;
027300151027
027400131227              exsr sr_tlo;
027500151030
027600151030         // devo fae due memorizzazioni : entro giorno B alle 14.00
027700151111         // oltre giorno B alle 11.30
027800151030
027900151111         if   w_hmx<=3530  ;
028000151030
0281001510303              if w_hmx>0 and w_hmx>w_omahmxB ;
028200151030                   w_omahmxB=w_hmx;
0283001510263             endif;
028400151030          // memorizzo i temrinal interessati
028500151030          if %lookup(caetfe:tfpB)=0  ;
028600151030          indxb=indxb+1           ;
028700151030          TFPB(indxb)=caetfe  ;
028800151030          endif ;
028900151030
029000151030          else ;
029100151030
0292001510303              if w_hmx>0 and w_hmx>w_omahmxC ;
029300151030                   w_omahmxC=w_hmx;
0294001510303             endif;
029500151030
029600151030          // memorizzo i terminal interessati
029700151030          if %lookup(caetfe:tfpC)=0  ;
029800151030          indxc=indxc+1           ;
029900151030          TFPC(indxc)=caetfe  ;
030000151030          endif ;
030100151030          endif  ;
030200151030 2a        endif;
030300151026
030400151026x2         else;
030500140103           // altrimenti devo cercare un traino avente tfp = filiale di scarico
030600141126           // (se ho già trovato un diretto (w_omahmx>0) non faccio niente)
030700140103                 wtfp=tlmtfa;
030800151026 2         endif;
030900151027 1a        endif;
031000151026 1      enddo;
031100131227        exec sql close TLMcsr;
031200131227       endsr;
031300131227       //-------------------------------------------------------------*
031400131227       //Preparazione stringa sql per Estrazione clienti da elaborare *
031500131227       //-------------------------------------------------------------*
031600131227       Begsr SR_prepSql;
031700140103         WrkStringaSql='select tntlm00f.*, tntlr00f.* +
031800140103                        from tntlm00f, tntlt00f, tntlr00f +
031900140103                        where tlmtrn=tlttrn and tlmdde=tltdde  +
032000140103                          and tlttrn=tlrtrn and tltdde=tlrdde  +
032100140107                                              and tlrsca=''S'' +
032200140107                          and tlmtfp=' + %editc(wtfp:'X') +
032300140103                          ' and tlmtff=' + %editc(c5_caetfe:'X') +
032400151103                          ' and ' + %editc(TLS7DRF :'X') + ' between +
032500131227                          tltdde and tltdsc';
032600131227       endsr;
032700131227       //--------------------------------------------------------------
032800140103       //?Recupero orario previsto trazione
032900131227       //--------------------------------------------------------------
033000131227       BEGSR sr_tlo;
033100140103
033200141126       clear w_hmx;
033300140108       clear w_gga;
033400140103       setll (tlrtrn:tlrdde:tlrnfm) tntlo01l;
033500140103       reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
033600151030
0337001510301      dow not %eof(tntlo01l);
033800140103          // memorizzo orario arrivo: sempre se giorno settimana blanks,
033900140103          // solo se 0 se giorno settimana specifico
0340001510302         if tlogse=' ' or w_hmx = 0;
034100141126             w_hmx=tlohha;
034200140108             w_gga=tlogga;
034300151028
034400151028          // se si tratta di transito dai tempi di trazione verifico se devo aggiungere
034500151028          //  24 ore per il transito
034600151028          clear azcas000  ;
034700151028          clear addtrans  ;
034800151028
0349001510303         if tlmtfp<>caetfe   ;
035000151030          clear castin_T ;
035100151030          clear castin   ;
035200151030          // Memorizzo tempo dal transito
035300151028          setgt  ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
035400151028          readpe ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
0355001511034         dow not  %eof(azcas01l) and casdde>TLS7DRF  ;
035600151028          readpe ('T':tlmtfp:c5_caetfe:'C') azcas01l  ;
0357001510304         enddo  ;
035800151030          //
0359001510304         if  not  %eof(azcas01l)                  ;
036000151030          castin_T=castin  ;
0361001510304         endif  ;
036200151030          // Memoprizzo tempo dalla filiale di partenza
036300151030          setgt  ('T':caetfe:c5_caetfe:'C') azcas01l  ;
036400151030          readpe ('T':caetfe:c5_caetfe:'C') azcas01l  ;
0365001511034         dow not  %eof(azcas01l) and casdde>TLS7DRF  ;
036600151030          readpe ('T':caetfe:c5_caetfe:'C') azcas01l  ;
0367001510304         enddo  ;
0368001510304         if       %eof(azcas01l)                  ;
036900151030          clear castin  ;
0370001510304         endif  ;
0371001510304         if  castin>castin_t and castin_t >0   ;
037200151030          diff=castin-castin_t  ;
037300151028
0374001510305         if  diff>12 and  diff<=24                ;
037500151028          addtrans=2400 ;
0376001510305         endif       ;
0377001510305         if  diff>24                              ;
037800151028          addtrans=4800 ;
0379001510305         endif       ;
0380001510304         endif  ;
038100151028
0382001510303         endif  ;
038300151028
038400151026          // trovo l'orario con la somma dei giorni di trazione
0385001510303         if w_gga='B'  ;
038600151026          w_hmx=w_hmx+2400;
0387001510303         endif  ;
038800151028
0389001510303         if w_gga='C'  ;
039000151026          w_hmx=w_hmx+4800;
0391001510303         endif  ;
039200151028
0393001510303         if w_gga='D'  ;
039400151026          w_hmx=w_hmx+7200;
0395001510303         endif  ;
039600151028
0397001510303         if w_hmx+addtrans<9200;
039800151028          w_hmx=w_hmx+addtrans ;
0399001510303         endif  ;
040000151026
0401001510302         endif;
040200140103          reade (tlrtrn:tlrdde:tlrnfm) tntlo01l;
0403001510301      enddo ;
040400140103
040500131227       ENDSR;
040600130905       //--------------------------------------------------------------
040700130905       //?Operazioni iniziali.
040800130905       //--------------------------------------------------------------
040900130905       BEGSR RoutInz;
041000130905
041100130905         // Reperimento dati job
041200130905         exsr DatiJob;
041300130905       // Reperisco la data corrente
041400130905          datacur=(%date());
041500131220          dateu=%dec(datacur);
041600130905
041700130905       endsr;
041800130905       //--------------------------------------------------------------
041900130905       //?Reperimento Dati del job (Utente/Operativi).
042000130905       //--------------------------------------------------------------
042100130905       BEGSR DatiJob;
042200130905
042300130905         in(E) §AzUte;
042400130905         if NOT %error;
042500130905           in(E) §DatiUte;
042600130905         endif;
042700130905         if %error or RSut = *blanks;
042800130905           clear TIBS34ds;
042900130905           tibs34r(tibs34ds);
043000130905           in §AzUte;
043100130905           in §DatiUte;
043200130905         endif;
043300130905
043400130905       ENDSR;
043500130905       //--------------------------------------------------------------
043600130905       //?Operazioni finali.
043700130905       //--------------------------------------------------------------
043800130905       BEGSR RoutEnd;
043900130905
044000130905         // Chiusura pgm   ;
044100151028             clear fnlv55ds;
044200151028             d55tla='C'   ;
044300151028             fnlv55r(fnlv55ds);
044400130905
044500130905         *inLR = *on;
044600130905
044700130905         return;
044800130905
044900130905       ENDSR;
045000130905
