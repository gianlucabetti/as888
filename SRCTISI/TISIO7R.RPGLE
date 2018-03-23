000100970613     H*------------------------------------------------------------------------*
000200130911     H* CAPPARIO - Orari Servizi --> Controlli/cancellazioni
000201130911
000202130911      * Il programma deve verificare la presenza di orari servizi per la lna
000203130913      * ricevuta da eliminare in base al livello di richiamo (CAP/LOCALITA'/ECCEZIONI)
000204130911      * Deve poi, tenendo conto del livello di richiamo:
000205130913      * A) IN MODALITA' CONTROLLO --> Ritornare un messaggio al chiamante,
000206130913      *    qualora riscontri la presenza di orari per la lna che verranno cancellati
000211130913      * B) IN MODALITA' DELETE --> Cancellare dagli orari servizi tutti i record
000212130913      *    relativi alla linea ricevuta tenendo conto del livello;
000214130911
000300970613     H*------------------------------------------------------------------------*
000400970613     H DECEDIT('0,') DATEDIT(*DMY.)
000500970613     F*------------------------------------------------------------------------*
000600970613     F* DATA BASE
000700970613     F*------------------------------------------------------------------------*
000701130911     FAZcos01L  IF   E           K DISK
000702130913     FAZcos02L  uF   E           K DISK    rename(azcos000:azcos002)
000703130912     FAZcpe01l  IF   E           K DISK
000704130912     FAZcpl01l  IF   E           K DISK
000705130913     FAZcpc01l  IF   E           K DISK
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
002900130912     d tisio7ds      e ds
002901130912
002902130912     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
002903130906
003000130906      * - Campi di comodo
003001130912     d wok_chk         s              1
003002130912     d wok_del         s              1
003300130906      // ? PROTOTIPI ?
003400130906      /copy gaitrasrc/srcprotopr,tibs34r
003500130906      *===============================================================*
003600130906      *  RIEPILOGO INDICATORI                                         *
003700130906      *---------------------------------------------------------------*
003800130906      *  XX    -
003900130906      *===============================================================*
004000130906     c     *Entry        plist
004100130906     c                   parm                    KPJBA
004101131016     c                   parm                    tisio7ds
004102130906      /free
004103130913       // pulizia dei campi di output
004104130913       clear oo7err;
004105130913       clear oo7msg;
004106130912
004107130912       // Controllo validità tipo lancio
004108130912       if io7tla<>'C' and io7tla<>'L' and io7tla<>*blanks;
004109130912          clear io7tla;
004110130912       endif;
004111130912
004112130912       if io7tla <>'C';
004113130912
004114130912       // lna = 0 non elaboro
004115130912        if io7lna>0;
004116130912
004117130912       // Verifico se lna ricevuta è realmente da elaborare: se presente nelle
004118130912       // linee ricevute non devo fare nulla sugli orari
004119130912          if io7lna<>io7lns and io7lna<>io7los and io7lna<>io7lol;
004120130912
004121130912       // Modalità C --> controllo
004122130912             if io7mod='C';
004123130912                exsr elabora_C ;
004124130912             else;
004125130912        // Modalità D --> Delete
004126130913       //       prima controllo se da cancellare
004127130913                exsr elabora_C ;
004128130913       //       se da cancellare procedo
004129130913                if wok_chk='S' and wok_del<>'N';
004130130913                   exsr elabora_D ;
004131130913                endif;
004132130912             endif;
004133130912          endif;
004134130912        endif;
004135131016       //kpjbu=tisio7ds;
004136130912       endif;
004137130912       //
004138130912       if io7tla <>*blanks;
004139130912           *inlr=*on;
004140130912       else;
004141130912           *inrt=*on;
004142130912       endif;
004143130912       //-------------------------------------------------------------*
004144130913       //Elaborazione in modalità controllo                           *
004145130912       //-------------------------------------------------------------*
004146130912       Begsr Elabora_C;
004147130912
004148130912       clear wok_chk;
004149130912       clear wok_del;
004150130912       // Verifico se per la lna eliminata sono presenti orari per
004151130912       // determinare se procedere con i controlli completi
004152130912       chain (io7ver:io7nar:io7cap:io7loc:io7lna) azcos01l;
004153130912       if %found (azcos01l);
004154130912          wok_chk='S';
004155130912       else                    ;
004156130912       // Se il controllo  è a livello di CAP o ECCEZIONI verifico anche che non siano
004157130912       // presenti orari a livello di CAP/LOCALITA per località che si appoggiano
004158130912       // al CAP
004159130912          if (io7liv='C'and io7loc=*blanks) or io7liv='E';
004160130912             setgt (io7ver:io7lna:io7nar:io7cap:*blanks)  azcos02l;
004161130913             reade(n) (io7ver:io7lna:io7nar:io7cap)          azcos02l;
004162130912             dow not %eof(azcos02l);
004163130912                chain (io7ver:io7nar:io7cap:cosloc) azcpl01l;
004164130912                if %found(azcpl01l) and cpllna<>io7lna and cpllol<>io7lna
004165130912                                    and cpllos<>io7lna;
004166130912                   wok_chk='S';
004167130912                   leave;
004168130912                endif;
004169130913                reade(N) (io7ver:io7lna:io7nar:io7cap)       azcos02l;
004170130912             enddo;
004171130912          endif;
004172130912       endif;
004173130912
004174130912       if wok_chk='S'    ;
004175130912          select;
004176130912          // LIVELLO CAP
004177130912          when IO7liV='C';
004178130912          // Verifico ECCEZIONI
004179130912             setll (io7ver:io7nar:io7cap) azcpe01l;
004180130912             reade (io7ver:io7nar:io7cap) azcpe01l;
004181130912             dow not %eof(azcpe01l) and wok_del=*blanks;
004182130912                if io7lna=cpelna or
004183130912                   io7lna=cpelol or
004184130917                   io7lna=cpelos;
004185130912                   wok_del='N'  ;
004186130912                endif;
004187130912                reade (io7ver:io7nar:io7cap) azcpe01l;
004188130912             enddo;
004189130918          // Quello che segue è inutile perchè questo controllo è già stato fatto
004190130918          // all'inizio quando la linea da eliminare viene confrontata con le linee
004191130918          // ricevute dal chiamante ( sono le linee del cap!! --> non ho quindi bisogno
004192130918          // di richainare il CAP per vederle)
004193130912          // Se ho la località verifico anche CAP
004194130918          // if wok_del=*blanks and io7loc<>*blanks;
004195130918          // chain (io7ver:io7nar:io7cap) azcpc01l;
004196130918          //    if %found (azcpc01l) and io7lna=cpclna or
004197130918          //                             io7lna=cpclol or
004198130918          //                             io7lna=cpclos;
004199130918          //       wok_del='N'  ;
004200130918          //    endif;
004201130918          // endif;
004202130912          // LIVELLO ECCEZIONI
004203130912          when IO7liV='E';
004204130912          // Verifico il CAP
004205130912             chain (io7ver:io7nar:io7cap) azcpc01l;
004206130912             if %found (azcpc01l) and io7lna=cpclna or
004207130912                                      io7lna=cpclol or
004208130912                                      io7lna=cpclos;
004209130912                   wok_del='N'  ;
004210130912             endif;
004211130913          // devo verificare anche la presenza di eventuali altre eccezioni
004212130913             if wok_del=*blanks;
004213130913                setll (io7ver:io7nar:io7cap) azcpe01l;
004214130913                reade (io7ver:io7nar:io7cap) azcpe01l;
004215130913                dow not %eof(azcpe01l) and wok_del=*blanks;
004216130913          // non elaboro l'eccezione se è quella che è modifica
004217130913                  if io7tfp<>cpetfp;
004218130913                   if io7lna=cpelna or
004219130913                      io7lna=cpelol or
004220130917                      io7lna=cpelos;
004221130913                      wok_del='N'  ;
004222130913                   endif;
004223130913                  endif;
004224130913                   reade (io7ver:io7nar:io7cap) azcpe01l;
004225130913                enddo;
004226130913             endif;
004227130912          // LIVELLO LOCALITA'
004228130912          when IO7liV='L';
004229130912          // a livello di Località non devo fare uteriori controlli
004230130912          endsl;
004231130912       endif;
004232130917       if wok_chk='S' and wok_del<>'N';
004233130913          oo7err='1';
004234130913          oo7msg=msg(1);
004235130912       endif;
004236130912       endsr;
004237130912       //-------------------------------------------------------------*
004238130913       //Elaborazione per cancellazione                               *
004239130912       //-------------------------------------------------------------*
004240130912       Begsr Elabora_D;
004241130913       // clearo nuovamente campi di output perchè in fase di cancellazione
004242130913       // non devo restituire i messaggi relativi ai controlli
004243130913       clear oo7err;
004244130913       clear oo7msg;
004245130913
004246130912       select;
004247130913       // LIVELLO DI CAP  (non da gestione località) o livello ECCEZIONI
004248130913       when (io7liv='C'and io7loc=*blanks) or io7liv='E';
004249130912             setll (io7ver:io7lna:io7nar:io7cap) azcos02l;
004250130912             reade (io7ver:io7lna:io7nar:io7cap) azcos02l;
004251130912             dow not %eof(azcos02l);
004252130913                if cosloc=*blanks;
004253130912                   delete azcos002;
004254130912                else;
004255130912                   chain (cosver:cosnar:coscap:cosloc) azcpl01l;
004256130913                   if %found(azcpl01l) and cplliv='C';
004257130913                   delete azcos002;
004258130913                   else;
004259130913                   unlock azcos02l;
004260130913                   endif;
004261130912                endif;
004262130912                reade (io7ver:io7lna:io7nar:io7cap) azcos02l;
004263130912             enddo;
004264130913       // LIVELLO DI CAP (da località che si appoggia al CAP)
004265130913       // o LIVELLO LOCALITA'
004266130913       when io7liv='L' or (io7liv='C' and io7loc<>*blanks);
004267130913             setll (io7ver:io7lna:io7nar:io7cap:io7loc) azcos02l;
004268130913             reade (io7ver:io7lna:io7nar:io7cap:io7loc) azcos02l;
004269130913             dow not %eof(azcos02l);
004274130913                   delete azcos002;
004279130913                reade (io7ver:io7lna:io7nar:io7cap) azcos02l;
004280130913             enddo;
004281130912       endsl;
004282130912
004283130912       endsr;
004294130906       //-------------------------------------------------------------*
004300130906       begsr *inzsr;
004400130906
004500130906       //Reperisco dati job
004600130906         exsr datijob;
008000130906
008100130906       EndSr;
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
009801130912**
009900130912Esistono OrariServizi personalizz.x la linea variata. Saranno eliminati       01
