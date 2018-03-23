000100970613     H*------------------------------------------------------------------------*
000200130906     H* CAPPARIO - STAMPA Orari Servizi
000300970613     H*------------------------------------------------------------------------*
000400970613     H DECEDIT('0,') DATEDIT(*DMY.)
000500970613     F*------------------------------------------------------------------------*
000600970613     F* DATA BASE
000700970613     F*------------------------------------------------------------------------*
000800130906     FAZorg01l  IF   E           K DISK
000900130909     FAZcos01l  IF   E           K DISK
001000130909     FAZcos02l  IF   E           K DISK    rename(azcos000:azcos02)
001100130906     FTISIO6P   O    E             PRINTER
001200970613     D*------------------------------------------------------------------------*
001300130906      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
001400130906      *
001500130906     d Tibs34ds      e ds                  inz
001600130906     d §azute        e ds                  Extname(Azute00f)
001700130906     d                                     dtaara
001800130906     d §Datiute      e ds                  EXTname(ddatiute)
001900130906     d                                     dtaara
002000151209      // - Ricerca/Controllo tabelle
002100151209     d TIBS02ds      e ds                  inz
002200130906     d KPJBA         e ds
002300130906      * - Parametri
002400130906     d tisio6ds      e ds
002500130906
002600130906      * - Campi di comodo
002700130906     d $righe          s              2  0
002800131218     d r               s              1  0
002900151209     d wdoppiag        s              1
003000130906     d wdeslna         s                   like(orgdes)
003100130910     d savloc          s                   like(cosloc)
003200130906     d pos             s              3  0
003300130906     d posI            s              3  0
003400130906     d wtitolo2        s                   like(ptitolo2)
003500130906     d dataiso_cor     s               d   datfmt(*iso)
003600130906     d dataeur_cor     s               d   datfmt(*eur)
003700130909     d datadmy         s               d   datfmt(*dmy)
003800131018     d wvuo35          s             35    inz
003900131218     d postisc_s       s                   like(postisc)
004000131218     d postfsc_s       s                   like(postfsc)
004100131218     d pomiisc_S       s                   like(pomiisc)
004200131218     d pomxfsc_S       s                   like(pomxfsc)
004300131218     d ptrzsc_s        s                   like(ptrzsc)
004400130906      // ? PROTOTIPI ?
004500130906      /copy gaitrasrc/srcprotopr,tibs34r
004600151209      /copy gaitrasrc/srcprotopr,tibs02r
004700130906      *===============================================================*
004800130906      *  RIEPILOGO INDICATORI                                         *
004900130906      *---------------------------------------------------------------*
005000130906      *  XX    -
005100130906      *===============================================================*
005200130906     c     *Entry        plist
005300130906     c                   parm                    KPJBA
005400130906     c                   movel     kpjbu         tisio6ds
005500130906      /free
005600130909       // Stampo intestazione
005700130909       exsr sr_impt;
005800130909       write sio6t;
005900131218       $righe=8;
006000130909       //
006100130909       if do6lna>0;
006200130909          exsr elalna;
006300130909       else;
006400130909          exsr elacap;
006500130909       endif;
006600130909       write fines;
006700151209         // Chiusura pgm   ;
006800151209          clear tibs02ds  ;
006900151209          t02tla='C'      ;
007000151209          TNTBE_RicercaControllo  (kpjba : tibs02ds);
007100151209
007200130906       eval *inlr=*on;
007300130909       //-------------------------------------------------------------*
007400130909       //elaborazione stampa per lna                                  *
007500130909       //-------------------------------------------------------------*
007600130909       begsr elalna;
007700130909       clear plna;
007800130909       clear plnad;
007900130909       pcap =*hival;
008000130910       savloc =*hival;
008100130909       setll (do6ver:do6lna:do6nar) azcos02l;
008200130909       reade (do6ver:do6lna:do6nar) azcos02l;
008300130909       dow not %eof (azcos02l) ;
008400130909       if do6ca2<>*blanks and coscap>do6ca2;
008500130909          leave;
008600130909       endif;
008700130909       if do6ca1<>*blanks and coscap<>*blanks and
008800130909       coscap<do6ca1;
008900130909          setll (do6ver:do6lna:do6nar:do6cA1) azcos02l;
009000130909          reade (do6ver:do6lna:do6nar) azcos02l;
009100130909          iter;
009200130909       endif;
009300130909       // OPERAZIONI A ROTTURA di cap/località
009400130910       if  coscap<>pcap or cosloc<>savloc;
009500130909          // Stampa riga
009600131219          exsr sr_riga;
009700130909          // Pulizia campi di stampa
009800130909          exsr sr_pulcampi;
009900130909          // memorizzo i nuovi cap e località per la stampa
010000130909          pcap=coscap;
010100130909          ploc=cosloc;
010200130910          savloc=cosloc;
010300130909       endif;
010400130909       // memorizzo dati del record letto
010500130909       exsr sr_riecampi;
010600130909       reade (do6ver:do6lna:do6nar) azcos02l;
010700130909       enddo;
010800130909          // Stampa riga a fine ciclo
010900131219          exsr sr_riga;
011000130909       endsr;
011100130909       //-------------------------------------------------------------*
011200130909       //elaborazione stampa per cap                                  *
011300130909       //-------------------------------------------------------------*
011400130909       begsr elacap;
011500130909       clear plna;
011600130909       clear plnad;
011700130909       pcap =*hival;
011800130910       savloc =*hival;
011900130909       setll (do6ver:do6nar:do6ca1) azcos01l;
012000130909       reade (do6ver:do6nar) azcos01l;
012100130909       dow not %eof (azcos01l) ;
012200130909       if do6ca2<>*blanks and coscap>do6ca2;
012300130909          leave;
012400130909       endif;
012500130909       // OPERAZIONI A ROTTURA di cap/località/lna
012600130910       if  coscap<>pcap or cosloc<>savloc or coslna<>plna;
012700130909          // Stampa riga
012800131219          exsr sr_riga;
012900130909          // Pulizia campi di stampa
013000130909          exsr sr_pulcampi;
013100130909          // memorizzo i nuovi cap /località/lna per la stampa
013200130909          pcap=coscap;
013300130909          ploc=cosloc;
013400130910          savloc=cosloc;
013500130909          plna=coslna;
013600130909       endif;
013700130909       // memorizzo dati del record letto
013800130909       exsr sr_riecampi;
013900130909       reade (do6ver:do6nar) azcos01l;
014000130909       enddo;
014100130909          // Stampa riga a fine ciclo
014200131219          exsr sr_riga;
014300130909       endsr;
014400130909       //-------------------------------------------------------------*
014500130909       //Riempimento campi di stampa                                  *
014600130909       //-------------------------------------------------------------*
014700130909       begsr sr_riecampi;
014800130909       // Ritiri
014900130909       if costser='R';
015000130909       postis=COSOSTIS;
015100130909       postfs=COSOSTFS;
015200131018       plrsc =COSOlrsc;
015300131018       plrnc =COSOlrnc;
015400131018       plrnk =COSOlrnk;
015500131003       //ptrzs=COSTRZS;
015600130909       else;
015700130909       // Consegne
015800131218          if cosserv='P';
015900131218             postisc_s=COSOSTIS;
016000131218             postfsc_s=COSOSTFS;
016100131218             pomiisc_S=COSOMIIS;
016200131218             pomxfsc_S=COSOMXFS;
016300131218             ptrzsc_s=COSTRZS;
016400131218          else;
016500131218             postisc=COSOSTIS;
016600131218             postfsc=COSOSTFS;
016700131218             pomiisc=COSOMIIS;
016800131218             pomxfsc=COSOMXFS;
016900131218             ptrzsc=COSTRZS;
017000131218          endif;
017100131218       endif;
017200130909       // Utente e data inserimento/ultima variazione: non distinguo tra ritiri e
017300130909       // consegne perchè il conenuto è lo stesso
017400130909       pute=cosute;
017500130909       datadmy=%date(cosduv:*iso);
017600130909       pduv  =%dec(datadmy);
017700130909
017800130909       endsr;
017900131218       //-------------------------------------------------------------*
018000131218       //Riempimento campi di stampa riga soglia pomeriggio           *
018100131218       //-------------------------------------------------------------*
018200131218       begsr riecampi_pom;
018300131219       // Se ho stampato anche la riga "normale" pulisco campi di stampa
018400131219       // che non variano
018500131219       if postis<>0 or postisc<>0;
018600131219          clear pcap;
018700131219          clear ploc;
018800131219          clear plna;
018900131219          clear plnad;
019000131219          clear pstd;
019100131219       endif;
019200131218       clear postis;
019300131218       clear postfs;
019400131218       clear plrsc ;
019500131218       clear plrnc ;
019600131218       clear plrnk ;
019700131218       postisc=postisc_s;
019800131218       postfsc=postfsc_s;
019900131218       pomiisc=pomiisc_S;
020000131218       pomxfsc=pomxfsc_S;
020100131218       ptrzsc=ptrzsc_s;
020200151209       if wdoppiag=' ' ;
020300131218       psoglia='Pom.';
020400151209       else  ;
020500151211       psoglia='2Fa.';
020600151209       endif  ;
020700131218       endsr;
020800130910       //-------------------------------------------------------------*
020900130910       //Decodifica lna                                               *
021000130910       //-------------------------------------------------------------*
021100130910       begsr sr_dlna    ;
021200130910       clear plnad;
021300130910       chain plna azorg01l;
021400130910       if %found(azorg01l);
021500130910          plnad=orgdes;
021600130910       endif;
021700151209       // Verifico se invece che avere soglia POM ha soglia giorno "B/C"
021800151209          clear tibs02ds  ;
021900151209          clear wdoppiag  ;
022000151209          t02tla=' '      ;
022100151209          t02mod='C'      ;
022200151209          t02cod='FLO'    ;
022300151209          t02ke1=%editc(plna:'X') ;
022400151209          TNTBE_RicercaControllo  (kpjba : tibs02ds);
022500151209          if t02err=' '   and t02atb=' '  ;
022600151209          wdoppiag='S'  ;
022700151209          endif  ;
022800151209
022900130910       endsr;
023000130909       //-------------------------------------------------------------*
023100130909       //Pulizia  campi di stampa                                     *
023200130909       //-------------------------------------------------------------*
023300130909       begsr sr_pulcampi;
023400130909       // Ritiri
023500130909       clear postis ;
023600130909       clear postfs;
023700131018       clear plrnc ;
023800131018       clear plrsc ;
023900131018       clear plrnk ;
024000131003       //clear ptrzs;
024100130909       // Consegne
024200130909       clear postisc;
024300130909       clear postfsc;
024400130909       clear pomiisc;
024500130909       clear pomxfsc;
024600131218       clear ptrzsc;
024700131218       // Consegne orari Pom
024800131218       clear postisc_s;
024900131218       clear postfsc_s;
025000131218       clear pomiisc_s;
025100131218       clear pomxfsc_s;
025200131218       clear ptrzsc_s;
025300131218       clear psoglia;
025400130909
025500130909       clear pute;
025600130909       clear pduv;
025700130909       clear pcap;
025800130909       clear ploc;
025900130910       clear savloc;
026000130909       endsr;
026100131219       //-------------------------------------------------------------*
026200131219       //Stamp riga                                                   *
026300131219       //-------------------------------------------------------------*
026400131219       begsr sr_riga;
026500131219          if pute <>*blanks;
026600131219             if do6lna=0;
026700131219                exsr sr_dlna;
026800131219             endif;
026900131219             if postisc_s<>0 and (postis<>0 or postisc<>0);
027000131219                r=3;
027100131219             else;
027200131219                r=2;
027300131219             endif;
027400131219             if $righe+r>60;
027500131219                write sio6t;
027600131219                $righe=8;
027700131219             endif;
027800131219          // Se cap vuoto sto stampando lo standard di fliale
027900131219             clear pstd;
028000131219             if pcap=*blanks;
028100131219             pstd='Standard Filiale';
028200131219             endif;
028300131219             if postis<>0 or postisc<>0;
028400131219                write sio6d ;
028500131219                $righe+=1;
028600131219             endif;
028700151209          // Se presente la soglia stampo riga "Pom" o "2gg"
028800131219             if postisc_s<>0;
028900131219                // Imposta campi di stampa per riga Pom
029000131219                exsr riecampi_pom;
029100131219                write sio6d ;
029200131219                $righe+=1;
029300131219             endif;
029400131219          // Stampo riga di separazione
029500131219                write sio6sep;
029600131219                $righe+=1;
029700131219          endif;
029800131219       endsr;
029900130906       //-------------------------------------------------------------*
030000130906       begsr *inzsr;
030100130906
030200130906       //Reperisco dati job
030300130906         exsr datijob;
030400130906
030500130906       // Reperisco la data corrente
030600130906       dataiso_cor=(%date());
030700130906       dataeur_cor=dataiso_cor;
030800130906       wdate=%dec(dataeur_cor);
030900130906       utime= %dec(%time());
031000130906
031100130906
031200130906       EndSr;
031300130906       //-------------------------------------------------------------*
031400130906       //Routine di impostazione campi di stampa record di testata    *
031500130906       //-------------------------------------------------------------*
031600130906       Begsr SR_impt  ;
031700130906
031800130906       // Titolo
031900130906
032000130906       ptitolo='STAMPA CAPPARIO: ORARI SERVIZI AL' ;
032100130906       ptitolo=%trim(ptitolo)+ ' '+ %editw(do6dri:'  /  /    ');
032200130906
032300130906       // Parametri di lancio: CAP dal/al
032400130906
032500130906       if do6ca1<>*blanks;
032600130906          ptitolo2='CAP: Dal ' + %trim(do6ca1) + ' al ' + %trim(do6ca2);
032700130906       endif;
032800130906
032900130906       // Parametri di lancio: linea di arrivo
033000130906
033100130906       if do6lna>0;
033200130906          clear wdeslna;
033300130906          chain do6lna azorg01l;
033400130906          if %found(azorg01l);
033500130906             wdeslna=orgdes;
033600130906          endif;
033700130906
033800130906          if ptitolo2=*blanks;
033900130906             ptitolo2='Linea Arrivo';
034000130906          else;
034100130906             ptitolo2=%trim(ptitolo2) + ' Linea Arrivo';
034200130906          endif;
034300130906          ptitolo2=%trim(ptitolo2) + ' ' + %editc(do6lna:'X') + ' ' + wdeslna;
034400130906       endif;
034500130906       // Se il campo Ptitolo2 non è tutto pieno lo centro
034600130906       // determino la lunghezza effettiva della stringa contenuta in ptitolo2
034700130906       pos=%checkr(' ':PTITOlo2);
034800130906       if pos<%len(ptitolo2);
034900130906          wtitolo2=ptitolo2;
035000130906          clear ptitolo2;
035100130906          posI=((%len(ptitolo2)-pos)/2)+1;
035200130906          %subst(ptitolo2:posi:pos)=%subst(wtitolo2:1:pos);
035300130906       endif;
035400130906
035500130909       clear pintlna;
035600130909       *in21=*off;
035700130909       if do6lna=0;
035800131219          pintlna='LINEA RITIRO/CONSEGNA';
035900130909          *in21=*on;
036000130909       endif;
036100131018
036200131018       // Tempo di organizzazione record 999
036300131218       //clear ptrzsc;
036400131218       //chain (do6ver: '   ': '         ': Wvuo35: 999: 'C') azcos01l;
036500131218       //if %found(azcos01l);
036600131218       //   ptrzsc=costrzs;
036700131218       //endif;
036800130906
036900130906       endsr;
037000130906       //-------------------------------------------------------------*
037100130906       //Reperimento Dati del job (Utente/Operativi)                  *
037200130906       //-------------------------------------------------------------*
037300130906       Begsr DatiJob;
037400130906
037500130906          in(e) §azute;
037600130906          if not %error;
037700130906           in(e) §datiute;
037800130906          endif;
037900130906          if %error or rsut = *blanks;
038000130906           tibs34r(tibs34ds);
038100130906           in §azute;
038200130906           in §datiute;
038300130906          endif;
038400130906         endsr;
