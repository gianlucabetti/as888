000100970613     H*------------------------------------------------------------------------*
000200130906     H* CAPPARIO - STAMPA Orari Servizi
000300970613     H*------------------------------------------------------------------------*
000400970613     H DECEDIT('0,') DATEDIT(*DMY.)
000500970613     F*------------------------------------------------------------------------*
000600970613     F* DATA BASE
000700970613     F*------------------------------------------------------------------------*
000800130906     FAZorg01l  IF   E           K DISK
000801130909     FAZcos01l  IF   E           K DISK
000802130909     FAZcos02l  IF   E           K DISK    rename(azcos000:azcos02)
001800130906     FTISIO6P   O    E             PRINTER
001900970613     D*------------------------------------------------------------------------*
002000130906      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002100130906      *
002200130906     d Tibs34ds      e ds                  inz
002300130906     d §azute        e ds                  Extname(Azute00f)
002400130906     d                                     dtaara
002500130906     d §Datiute      e ds                  EXTname(ddatiute)
002600130906     d                                     dtaara
002700130906     d KPJBA         e ds
002800130906      * - Parametri
002900130906     d tisio6ds      e ds
002901130906
003000130906      * - Campi di comodo
003200130906     d $righe          s              2  0
003201131218     d r               s              1  0
003202130906     d wdeslna         s                   like(orgdes)
003203130910     d savloc          s                   like(cosloc)
003204130906     d pos             s              3  0
003205130906     d posI            s              3  0
003206130906     d wtitolo2        s                   like(ptitolo2)
003207130906     d dataiso_cor     s               d   datfmt(*iso)
003208130906     d dataeur_cor     s               d   datfmt(*eur)
003209130909     d datadmy         s               d   datfmt(*dmy)
003210131018     d wvuo35          s             35    inz
003211131218     d postisc_s       s                   like(postisc)
003212131218     d postfsc_s       s                   like(postfsc)
003213131218     d pomiisc_S       s                   like(pomiisc)
003214131218     d pomxfsc_S       s                   like(pomxfsc)
003215131218     d ptrzsc_s        s                   like(ptrzsc)
003300130906      // ? PROTOTIPI ?
003400130906      /copy gaitrasrc/srcprotopr,tibs34r
003500130906      *===============================================================*
003600130906      *  RIEPILOGO INDICATORI                                         *
003700130906      *---------------------------------------------------------------*
003800130906      *  XX    -
003900130906      *===============================================================*
004000130906     c     *Entry        plist
004100130906     c                   parm                    KPJBA
004101130906     c                   movel     kpjbu         tisio6ds
004102130906      /free
004103130909       // Stampo intestazione
004104130909       exsr sr_impt;
004105130909       write sio6t;
004106131218       $righe=8;
004107130909       //
004108130909       if do6lna>0;
004109130909          exsr elalna;
004110130909       else;
004111130909          exsr elacap;
004112130909       endif;
004113130909       write fines;
004114130906       eval *inlr=*on;
004116130909       //-------------------------------------------------------------*
004117130909       //elaborazione stampa per lna                                  *
004118130909       //-------------------------------------------------------------*
004119130909       begsr elalna;
004120130909       clear plna;
004121130909       clear plnad;
004122130909       pcap =*hival;
004123130910       savloc =*hival;
004125130909       setll (do6ver:do6lna:do6nar) azcos02l;
004126130909       reade (do6ver:do6lna:do6nar) azcos02l;
004127130909       dow not %eof (azcos02l) ;
004128130909       if do6ca2<>*blanks and coscap>do6ca2;
004129130909          leave;
004130130909       endif;
004131130909       if do6ca1<>*blanks and coscap<>*blanks and
004132130909       coscap<do6ca1;
004133130909          setll (do6ver:do6lna:do6nar:do6cA1) azcos02l;
004134130909          reade (do6ver:do6lna:do6nar) azcos02l;
004135130909          iter;
004136130909       endif;
004137130909       // OPERAZIONI A ROTTURA di cap/località
004138130910       if  coscap<>pcap or cosloc<>savloc;
004139130909          // Stampa riga
004140131219          exsr sr_riga;
004171130909          // Pulizia campi di stampa
004172130909          exsr sr_pulcampi;
004173130909          // memorizzo i nuovi cap e località per la stampa
004174130909          pcap=coscap;
004175130909          ploc=cosloc;
004176130910          savloc=cosloc;
004177130909       endif;
004178130909       // memorizzo dati del record letto
004179130909       exsr sr_riecampi;
004180130909       reade (do6ver:do6lna:do6nar) azcos02l;
004181130909       enddo;
004182130909          // Stampa riga a fine ciclo
004183131219          exsr sr_riga;
004205130909       endsr;
004206130909       //-------------------------------------------------------------*
004207130909       //elaborazione stampa per cap                                  *
004208130909       //-------------------------------------------------------------*
004209130909       begsr elacap;
004210130909       clear plna;
004211130909       clear plnad;
004212130909       pcap =*hival;
004213130910       savloc =*hival;
004214130909       setll (do6ver:do6nar:do6ca1) azcos01l;
004215130909       reade (do6ver:do6nar) azcos01l;
004216130909       dow not %eof (azcos01l) ;
004217130909       if do6ca2<>*blanks and coscap>do6ca2;
004218130909          leave;
004219130909       endif;
004220130909       // OPERAZIONI A ROTTURA di cap/località/lna
004221130910       if  coscap<>pcap or cosloc<>savloc or coslna<>plna;
004222130909          // Stampa riga
004223131219          exsr sr_riga;
004250130909          // Pulizia campi di stampa
004251130909          exsr sr_pulcampi;
004252130909          // memorizzo i nuovi cap /località/lna per la stampa
004253130909          pcap=coscap;
004254130909          ploc=cosloc;
004255130910          savloc=cosloc;
004256130909          plna=coslna;
004257130909       endif;
004258130909       // memorizzo dati del record letto
004259130909       exsr sr_riecampi;
004260130909       reade (do6ver:do6nar) azcos01l;
004261130909       enddo;
004262130909          // Stampa riga a fine ciclo
004263131219          exsr sr_riga;
004284130909       endsr;
004285130909       //-------------------------------------------------------------*
004286130909       //Riempimento campi di stampa                                  *
004287130909       //-------------------------------------------------------------*
004288130909       begsr sr_riecampi;
004289130909       // Ritiri
004290130909       if costser='R';
004291130909       postis=COSOSTIS;
004292130909       postfs=COSOSTFS;
004293131018       plrsc =COSOlrsc;
004294131018       plrnc =COSOlrnc;
004295131018       plrnk =COSOlrnk;
004296131003       //ptrzs=COSTRZS;
004297130909       else;
004298130909       // Consegne
004299131218          if cosserv='P';
004300131218             postisc_s=COSOSTIS;
004301131218             postfsc_s=COSOSTFS;
004302131218             pomiisc_S=COSOMIIS;
004303131218             pomxfsc_S=COSOMXFS;
004304131218             ptrzsc_s=COSTRZS;
004305131218          else;
004306131218             postisc=COSOSTIS;
004307131218             postfsc=COSOSTFS;
004308131218             pomiisc=COSOMIIS;
004309131218             pomxfsc=COSOMXFS;
004310131218             ptrzsc=COSTRZS;
004311131218          endif;
004312131218       endif;
004313130909       // Utente e data inserimento/ultima variazione: non distinguo tra ritiri e
004314130909       // consegne perchè il conenuto è lo stesso
004315130909       pute=cosute;
004316130909       datadmy=%date(cosduv:*iso);
004317130909       pduv  =%dec(datadmy);
004318130909
004319130909       endsr;
004320131218       //-------------------------------------------------------------*
004321131218       //Riempimento campi di stampa riga soglia pomeriggio           *
004322131218       //-------------------------------------------------------------*
004323131218       begsr riecampi_pom;
004324131219       // Se ho stampato anche la riga "normale" pulisco campi di stampa
004325131219       // che non variano
004326131219       if postis<>0 or postisc<>0;
004327131219          clear pcap;
004328131219          clear ploc;
004329131219          clear plna;
004330131219          clear plnad;
004331131219          clear pstd;
004332131219       endif;
004333131218       clear postis;
004334131218       clear postfs;
004335131218       clear plrsc ;
004336131218       clear plrnc ;
004337131218       clear plrnk ;
004338131218       postisc=postisc_s;
004339131218       postfsc=postfsc_s;
004340131218       pomiisc=pomiisc_S;
004341131218       pomxfsc=pomxfsc_S;
004342131218       ptrzsc=ptrzsc_s;
004343131218       psoglia='Pom.';
004344131218       endsr;
004345130910       //-------------------------------------------------------------*
004346130910       //Decodifica lna                                               *
004347130910       //-------------------------------------------------------------*
004348130910       begsr sr_dlna    ;
004349130910       clear plnad;
004350130910       chain plna azorg01l;
004351130910       if %found(azorg01l);
004352130910          plnad=orgdes;
004353130910       endif;
004354130910       endsr;
004355130909       //-------------------------------------------------------------*
004356130909       //Pulizia  campi di stampa                                     *
004357130909       //-------------------------------------------------------------*
004358130909       begsr sr_pulcampi;
004359130909       // Ritiri
004360130909       clear postis ;
004361130909       clear postfs;
004362131018       clear plrnc ;
004363131018       clear plrsc ;
004364131018       clear plrnk ;
004365131003       //clear ptrzs;
004366130909       // Consegne
004367130909       clear postisc;
004368130909       clear postfsc;
004369130909       clear pomiisc;
004370130909       clear pomxfsc;
004371131218       clear ptrzsc;
004372131218       // Consegne orari Pom
004373131218       clear postisc_s;
004374131218       clear postfsc_s;
004375131218       clear pomiisc_s;
004376131218       clear pomxfsc_s;
004377131218       clear ptrzsc_s;
004378131218       clear psoglia;
004379130909
004380130909       clear pute;
004381130909       clear pduv;
004382130909       clear pcap;
004383130909       clear ploc;
004384130910       clear savloc;
004385130909       endsr;
004386131219       //-------------------------------------------------------------*
004387131219       //Stamp riga                                                   *
004388131219       //-------------------------------------------------------------*
004389131219       begsr sr_riga;
004390131219          if pute <>*blanks;
004391131219             if do6lna=0;
004392131219                exsr sr_dlna;
004393131219             endif;
004394131219             if postisc_s<>0 and (postis<>0 or postisc<>0);
004395131219                r=3;
004396131219             else;
004397131219                r=2;
004398131219             endif;
004399131219             if $righe+r>60;
004400131219                write sio6t;
004401131219                $righe=8;
004402131219             endif;
004403131219          // Se cap vuoto sto stampando lo standard di fliale
004404131219             clear pstd;
004405131219             if pcap=*blanks;
004406131219             pstd='Standard Filiale';
004407131219             endif;
004408131219             if postis<>0 or postisc<>0;
004409131219                write sio6d ;
004410131219                $righe+=1;
004411131219             endif;
004412131219          // Se presente la soglia stampo riga "Pom"
004413131219             if postisc_s<>0;
004414131219                // Imposta campi di stampa per riga Pom
004415131219                exsr riecampi_pom;
004416131219                write sio6d ;
004417131219                $righe+=1;
004418131219             endif;
004419131219          // Stampo riga di separazione
004420131219                write sio6sep;
004421131219                $righe+=1;
004422131219          endif;
004423131219       endsr;
004424130906       //-------------------------------------------------------------*
004425130906       begsr *inzsr;
004426130906
004500130906       //Reperisco dati job
004600130906         exsr datijob;
007400130906
007500130906       // Reperisco la data corrente
007600130906       dataiso_cor=(%date());
007700130906       dataeur_cor=dataiso_cor;
007800130906       wdate=%dec(dataeur_cor);
007900130906       utime= %dec(%time());
007901130906
008000130906
008100130906       EndSr;
008101130906       //-------------------------------------------------------------*
008102130906       //Routine di impostazione campi di stampa record di testata    *
008103130906       //-------------------------------------------------------------*
008104130906       Begsr SR_impt  ;
008105130906
008106130906       // Titolo
008107130906
008110130906       ptitolo='STAMPA CAPPARIO: ORARI SERVIZI AL' ;
008111130906       ptitolo=%trim(ptitolo)+ ' '+ %editw(do6dri:'  /  /    ');
008115130906
008116130906       // Parametri di lancio: CAP dal/al
008117130906
008118130906       if do6ca1<>*blanks;
008119130906          ptitolo2='CAP: Dal ' + %trim(do6ca1) + ' al ' + %trim(do6ca2);
008120130906       endif;
008121130906
008122130906       // Parametri di lancio: linea di arrivo
008123130906
008124130906       if do6lna>0;
008125130906          clear wdeslna;
008126130906          chain do6lna azorg01l;
008127130906          if %found(azorg01l);
008128130906             wdeslna=orgdes;
008129130906          endif;
008130130906
008131130906          if ptitolo2=*blanks;
008132130906             ptitolo2='Linea Arrivo';
008133130906          else;
008134130906             ptitolo2=%trim(ptitolo2) + ' Linea Arrivo';
008135130906          endif;
008136130906          ptitolo2=%trim(ptitolo2) + ' ' + %editc(do6lna:'X') + ' ' + wdeslna;
008137130906       endif;
008138130906       // Se il campo Ptitolo2 non è tutto pieno lo centro
008139130906       // determino la lunghezza effettiva della stringa contenuta in ptitolo2
008140130906       pos=%checkr(' ':PTITOlo2);
008141130906       if pos<%len(ptitolo2);
008142130906          wtitolo2=ptitolo2;
008143130906          clear ptitolo2;
008144130906          posI=((%len(ptitolo2)-pos)/2)+1;
008145130906          %subst(ptitolo2:posi:pos)=%subst(wtitolo2:1:pos);
008146130906       endif;
008147130906
008148130909       clear pintlna;
008149130909       *in21=*off;
008150130909       if do6lna=0;
008151131219          pintlna='LINEA RITIRO/CONSEGNA';
008152130909          *in21=*on;
008153130909       endif;
008154131018
008155131018       // Tempo di organizzazione record 999
008156131218       //clear ptrzsc;
008157131218       //chain (do6ver: '   ': '         ': Wvuo35: 999: 'C') azcos01l;
008158131218       //if %found(azcos01l);
008159131218       //   ptrzsc=costrzs;
008160131218       //endif;
008172130906
008173130906       endsr;
008200130906       //-------------------------------------------------------------*
008300130906       //Reperimento Dati del job (Utente/Operativi)                  *
008400130906       //-------------------------------------------------------------*
008500130906       Begsr DatiJob;
008600130906
008700130906          in(e) §azute;
008800130906          if not %error;
008900130906           in(e) §datiute;
009000130906          endif;
009100130906          if %error or rsut = *blanks;
009200130906           tibs34r(tibs34ds);
009300130906           in §azute;
009400130906           in §datiute;
009500130906          endif;
009800130906         endsr;
