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
003201130906     d wdeslna         s                   like(orgdes)
003202130910     d savloc          s                   like(cosloc)
003203130906     d pos             s              3  0
003204130906     d posI            s              3  0
003205130906     d wtitolo2        s                   like(ptitolo2)
003206130906     d dataiso_cor     s               d   datfmt(*iso)
003207130906     d dataeur_cor     s               d   datfmt(*eur)
003208130909     d datadmy         s               d   datfmt(*dmy)
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
004106130909       $righe=9;
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
004140130909          if pute <>*blanks;
004141130909             if $righe+1>60;
004142130909                write sio6t;
004143130909                $righe=9;
004144130909             endif;
004145130909          // Se cap vuoto sto stampando lo standard di fliale
004146130909             clear pstd;
004147130909             if pcap=*blanks;
004148130909             pstd='Standard Filiale';
004149130909             endif;
004150130909             write sio6d ;
004151130909             $righe+=1;
004152130909          endif;
004153130909          // Pulizia campi di stampa
004154130909          exsr sr_pulcampi;
004155130909          // memorizzo i nuovi cap e località per la stampa
004156130909          pcap=coscap;
004157130909          ploc=cosloc;
004158130910          savloc=cosloc;
004159130909       endif;
004160130909       // memorizzo dati del record letto
004161130909       exsr sr_riecampi;
004162130909       reade (do6ver:do6lna:do6nar) azcos02l;
004163130909       enddo;
004164130909          // Stampa riga a fine ciclo
004165130909          if pute <>*blanks;
004166130909             if $righe+1>60;
004167130909                write sio6t;
004168130909                $righe=9;
004169130909             endif;
004170130909             write sio6d ;
004171130909             $righe+=1;
004172130909          endif;
004175130909       endsr;
004176130909       //-------------------------------------------------------------*
004177130909       //elaborazione stampa per cap                                  *
004178130909       //-------------------------------------------------------------*
004179130909       begsr elacap;
004180130909       clear plna;
004181130909       clear plnad;
004182130909       pcap =*hival;
004183130910       savloc =*hival;
004184130909       setll (do6ver:do6nar:do6ca1) azcos01l;
004185130909       reade (do6ver:do6nar) azcos01l;
004186130909       dow not %eof (azcos01l) ;
004187130909       if do6ca2<>*blanks and coscap>do6ca2;
004188130909          leave;
004189130909       endif;
004196130909       // OPERAZIONI A ROTTURA di cap/località/lna
004197130910       if  coscap<>pcap or cosloc<>savloc or coslna<>plna;
004198130909          // Stampa riga
004199130909          if pute <>*blanks;
004200130909             exsr sr_dlna;
004201130909             if $righe+1>60;
004202130909                write sio6t;
004203130909                $righe=9;
004204130909             endif;
004205130909          //
004206130909             clear pstd;
004209130909             write sio6d ;
004210130909             $righe+=1;
004211130909          endif;
004212130909          // Pulizia campi di stampa
004213130909          exsr sr_pulcampi;
004214130909          // memorizzo i nuovi cap /località/lna per la stampa
004215130909          pcap=coscap;
004216130909          ploc=cosloc;
004217130910          savloc=cosloc;
004218130909          plna=coslna;
004219130909       endif;
004220130909       // memorizzo dati del record letto
004221130909       exsr sr_riecampi;
004222130909       reade (do6ver:do6nar) azcos01l;
004223130909       enddo;
004224130909          // Stampa riga a fine ciclo
004225130909          if pute <>*blanks;
004226130909             exsr sr_dlna;
004227130909             if $righe+1>60;
004228130909                write sio6t;
004229130909                $righe=9;
004230130909             endif;
004231130909             write sio6d ;
004232130909             $righe+=1;
004233130909          endif;
004234130909       endsr;
004235130909       //-------------------------------------------------------------*
004236130909       //Riempimento campi di stampa                                  *
004237130909       //-------------------------------------------------------------*
004238130909       begsr sr_riecampi;
004239130909       // Ritiri
004240130909       if costser='R';
004241130909       postis=COSOSTIS;
004242130909       postfs=COSOSTFS;
004243130909       pomiis=COSOMIIS;
004244130909       pomxfs=COSOMXFS;
004245130909       ptrzs=COSTRZS;
004246130909       else;
004247130909       // Consegne
004248130909       postisc=COSOSTIS;
004249130909       postfsc=COSOSTFS;
004250130909       pomiisc=COSOMIIS;
004251130909       pomxfsc=COSOMXFS;
004252130909       ptrzsc=COSTRZS;
004253130909       endif;
004254130909       // Utente e data inserimento/ultima variazione: non distinguo tra ritiri e
004255130909       // consegne perchè il conenuto è lo stesso
004256130909       pute=cosute;
004257130909       datadmy=%date(cosduv:*iso);
004258130909       pduv  =%dec(datadmy);
004259130909
004260130909       endsr;
004261130910       //-------------------------------------------------------------*
004262130910       //Decodifica lna                                               *
004263130910       //-------------------------------------------------------------*
004264130910       begsr sr_dlna    ;
004265130910       clear plnad;
004266130910       chain plna azorg01l;
004267130910       if %found(azorg01l);
004268130910          plnad=orgdes;
004269130910       endif;
004270130910       endsr;
004271130909       //-------------------------------------------------------------*
004272130909       //Pulizia  campi di stampa                                     *
004273130909       //-------------------------------------------------------------*
004274130909       begsr sr_pulcampi;
004275130909       // Ritiri
004276130909       clear postis ;
004277130909       clear postfs;
004278130909       clear pomiis;
004279130909       clear pOMXFS;
004280130909       clear ptrzs;
004281130909       // Consegne
004282130909       clear postisc;
004283130909       clear postfsc;
004284130909       clear pomiisc;
004285130909       clear pomxfsc;
004286130909       clear ptrzsc;
004287130909
004288130909       clear pute;
004289130909       clear pduv;
004290130909       clear pcap;
004291130909       clear ploc;
004292130910       clear savloc;
004293130909       endsr;
004294130906       //-------------------------------------------------------------*
004300130906       begsr *inzsr;
004400130906
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
008151130909          pintlna='LINEA DI ARRIVO';
008152130909          *in21=*on;
008153130909       endif;
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
