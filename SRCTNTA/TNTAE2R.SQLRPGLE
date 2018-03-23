000100111014      *---------------------------------------------------------------*
000200111014      * Analisi clienti con utilizzo servizio di avviso al destinatario
000300111014      *---------------------------------------------------------------*
000400111014     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000500111017     ftitas33c  if   e           k Disk    extfile(wlibsede)usropn
000600111017     ftabel00f  if   e           k disk    prefix(t_)
000700111013     fazorg01l  if   e           k disk
000800111017     ftntam04l  if   e           k Disk
000900111017     fTITPD01L  if   e           k Disk
001000111017     fWFEmd01l  uf a E           k disk
001100111020     ftntae2p   o    e             printer usropn
001200111012
001300090715     d
001400111020      *
001500111020      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
001600111020      *
001700111014     d KPJBA         e ds
001800111014     d tntae2ds      e ds
001900111012     d dtasflo       e ds
002000111017     d ds05          e ds
002100111017     d ds17          e ds
002200111017     d ds3c          e ds
002300111014     d cnclpds       e ds                  extname(CNCLP00F) inz
002400111014     d cnacods       e ds                  extname(cnaco00f) inz
002500130912     d AZCMMds       e ds                  extname(AZCMM00F) inz
002600111014     d Tibs34ds      e ds                  inz
002700111014     d §azute        e ds                  Extname(Azute00f)
002800111014     d                                     dtaara
002900111014     d §Datiute      e ds                  EXTname(ddatiute)
003000111014     d                                     dtaara
003100111014     d dute01        e ds
003200111014     d trul31ds      e ds
003300111014     d POG                            3  0 DIM(250) overlay(o31pog)
003400111014
003500111014      * - Indici di schiera / Contatori
003600111014     d i               s              3  0 inz
003700111014      * - Campi di comodo
003800111017     D Wlibsede        S             21
003900111014     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
004000111014     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
004100111014     d w_aasdal        s                   like(tasaas)
004200111014     d w_aasal         s                   like(tasaas)
004300111014     d w_mgsdal        s                   like(tasmgs)
004400111014     d w_mgsal         s                   like(tasmgs)
004500111014     d wabi            s                   like(UteAut)
004600111013     d kfil            s                   like(orgfil)
004700111013     d WtotE           s              5  0
004800111014     d prectr          s                   like(tasctr)
004900111017     d kftc            s                   like(tpdftc)
005000111017     d wfd1            s                   like(tpditr)
005100111017     d wfd2            s                   like(tpditr)
005200111017     d wfd3            s                   like(tpditr)
005300111017     d wsg1            s                   like(tpdsgl)
005400111017     d wsg2            s                   like(tpdsgl)
005500111017     d wsg3            s                   like(tpdsgl)
005600111017     d wcts            s                   like(tpdcts)
005700111017     d contatore       s              2  0
005800111017     d overcts         s              1
005900111014     d Primo           s              4  0 inz
006000111013     d dataiso_cor     s               d   datfmt(*iso)
006100111014     d dataeur_cor     s               d   datfmt(*eur)
006200111021     d dataeur         s               d   datfmt(*eur)
006300111021     d $righe          s              2  0
006400111021     d savage          s                   like(emdage)
006500111024     d savccm          s                   like(emdccm)
006600111021     d w_ttsp          s                   like(emdtsp)
006700111021     d w_ttspgen       s                   like(emdtsp)
006800111021     d wDataGMA        s              8  0
006900111012
007000111014     D WrkStringaSqlC  S           4500
007100111014     D                                     VARYING
007200111014     D WrkFilIn        S           4000
007300111014     D                                     VARYING
007400111013     d $Finerec        s              1    inz(*off)
007500111013
007600111014      // ? PROTOTIPI ?
007700111014      /copy gaitrasrc/srcprotopr,tibs34r
007800111014      /copy gaitrasrc/srcprotopr,trul31r
007900111014
008000111013     c     *Entry        plist
008100111013     c                   parm                    KPJBA
008200111013     c
008300111014     c                   movel     kpjbu         tntae2ds
008400111013      /free
008500111013         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008600111013
008700111021       // ELABORAZIONE
008800111014       // preparo stringa sql per estrazione codici clienti da elaborare
008900111014       exsr sr_prepSql;
009000111014       // elaboro dati estratti
009100111014       exsr sr_Elabora;
009200111021
009300111020       // OPERAZIONI PER LA STAMPA
009400111020       Select;
009500111020
009600111020         // - Lancio da Filiale
009700111020         When  tae2crt <> 'S';
009800111020           open tntae2p;
009900111020           // Stampo intestazione
010000111020           exsr sr_impt00;
010100111020           write tae2t00;
010200111020           write tae2t01;
010300111021           $righe=6;
010400111020              exsr sr_elaemd;
010500111020           // Stampo *** Fine Stampa ***
010600111020           if $righe+2>60;
010700111020              exsr sr_overflow;
010800111020           endif;
010900111020           write fines;
011000111020       EndSl;
011100111013       eval *inlr=*on;
011200111014       //-------------------------------------------------------------*
011300111014       //Routine Iniziale                                             *
011400111014       //-------------------------------------------------------------*
011500111014       begsr *inzsr;
011600111014
011700111014       //Reperisco dati job
011800111014         exsr datijob;
011900111014
012000111014       if %subst(knsif:7:1) = 'P';
012100111014         Wlibsede = Wlibsedep;
012200111014         Else ;
012300111014         wlibsede = Wlibsedeb;
012400111014       endif;
012500111014       Open   Titas33c;
012600111014
012700111014       // Reperisco la data corrente
012800111014       dataiso_cor=(%date());
012900111014       dataeur_cor=dataiso_cor;
013000111021       wdate=%dec(dataeur_cor);
013100111021       utime= %dec(%time());
013200111014
013300111014       EndSr;
013400111014       //-------------------------------------------------------------*
013500111014       //Reperimento Dati del job (Utente/Operativi)                  *
013600111014       //-------------------------------------------------------------*
013700111014       Begsr DatiJob;
013800111014
013900111014          in(e) §azute;
014000111014          if not %error;
014100111014           in(e) §datiute;
014200111014          endif;
014300111014          if %error or rsut = *blanks;
014400111014           tibs34r(tibs34ds);
014500111014           in §azute;
014600111014           in §datiute;
014700111014          endif;
014800111014          dute01=utefaf;
014900111014          wabi=§utegtc;
015000111014         endsr;
015100111014       //-------------------------------------------------------------*
015200111014       //Preparazione stringa sql per Estrazione clienti da elaborare *
015300111014       //-------------------------------------------------------------*
015400111014       Begsr SR_prepSql;
015500111014
015600130912       WrkStringaSqlC='select * from cnaco00f, cnclp00f, AZCMM00F' +
015700111014                      ' where acokut=clpkut and acokcc=clpkcc and' +
015800130912                      ' acoksc=clpksc and clpage=CMMcod';
015900111014       // richiesto un comm.le unificante specifico
016000111014       if tae2age>0;
016100111014          WrkStringaSqlC=WrkStringaSqlC+
016200130912          ' and CMMuni=' + %editc(tae2age:'X');
016300111014       else;
016400111014       // richiesta per area comm.le: preparo schiera delle filiali di appartene
016500111014          exsr sr_whrfil;
016600111014          WrkStringaSqlC=WrkStringaSqlC+WrkFilIn;
016700111014       endif;
016800111014
016900111014       endsr;
017000111014       //-------------------------------------------------------------*
017100111014       //Schiera filiali di appartenenza all'area del comm.le unific. *
017200111014       //-------------------------------------------------------------*
017300111014       Begsr SR_whrfil;
017400111014          Primo= 1 ;
017500111014
017600111014          reset WrkFilIn;
017700111014          clear trul31ds  ;
017800111014          I31abi = wabi    ;
017900111014          I31cpo = DUTpou  ;
018000111014          trul31r(kpjba:trul31ds);
018100130912          WrkFilIn= ' And substr(digits(CMMuni), 1, 3) in (';
018200111014          for I=1 to %elem(pog);
018300111014             if pog(i)=0;
018400111014                leave;
018500111014             endif;
018600111014             if tae2car>0   ;
018700111014              chain   pog(i) azorg01l  ;
018800111014             endif  ;
018900111014
019000111014             if tae2car=0 or
019100111014               (%found(azorg01l) and   orgcar=tae2car) ;
019200111014               if primo>1;
019300130912                 WrkFilIn=WrkFilIn + ', ';
019400111014               endif;
019500111014             // se richiesta AREA, carico solo i p.o. dell'area richeista
019600111014             WrkFilIn=WrkFilIn + %char(pog(i));
019700111014             Primo=primo+1   ;
019800111014             endif;
019900111014          endfor;
020000111014          WrkFilIn=WrkFilIn + ')';
020100111014       endsr;
020200111014       //-------------------------------------------------------------*
020300111014       //Lettura ed elaborazione dati estratti con sql                *
020400111014       //-------------------------------------------------------------*
020500111014       Begsr SR_Elabora;
020600111014       // imposto key per titas
020700111014       w_aasdal=%dec(%subst(%editc(tae2dal:'X'):1:4):4:0);
020800111014       w_mgsdal=%dec(%subst(%editc(tae2dal:'X'):5:4):4:0);
020900111014       w_aasal=%dec(%subst(%editc(tae2al:'X'):1:4):4:0);
021000111014       w_mgsal=%dec(%subst(%editc(tae2al:'X'):5:4):4:0);
021100111014       $finerec=*off;
021200111014       exec sql prepare STRINGASQLC from :WrkStringaSqlC;
021300111014       exec sql declare ACOCsr cursor for StringaSqlC;
021400111014       exec sql open ACOcsr;
021500111014       dow $finerec=*off;
021600130912          exec sql Fetch ACOcsr into :cnacods, :cnclpds, :AZCMMds;
021700111014          if sqlcod=100 or sqlcod<0;
021800111014             $finerec = *on;
021900111014             leave;
022000111014          endif;
022100111014
022200111014       // Elaboro titas per ogni cliente caricato in schiera
022300111014          exsr sr_Etitas;
022400111014       enddo;
022500111014       endsr;
022600111014
022700111014       //-------------------------------------------------------------*
022800111014       //Elaborazione Titas per cliente mittente                      *
022900111014       //-------------------------------------------------------------*
023000111014       Begsr Sr_Etitas;
023100111014
023200111020       prectr=*hival;
023300111014
023400111014       setll (acoksc: w_aasdal: w_mgsdal) titas33c;
023500111014       reade (acoksc) titas33c  ;
023600111014       dow not %eof(titas33c);
023700111014           if tasaas>W_aasal  or (tasaas=W_aasal and tasmgs>W_mgsal);
023800111014              leave;
023900111014           endif;
024000111014         // elaboro solo se bolla con avviso al destinatario
024100111014           dtasflo=tasflo;
024200111014           if §floemd='S';
024300111014         // a cambio di CTR azzero il contatore ed eseguo tutte
024400111014         // le operazioni di ricerca dati accessori e scrivo record nel
024500111014         // file di work
024600111014             if tasctr<>prectr              ;
024700111020                if prectr<>*hival           ;
024800111017                   exsr sr_aggwf            ;
024900111014                endif                       ;
025000111014                WTotE=0                     ;
025100111014                prectr=tasctr               ;
025200111014             endif                          ;
025300111014        // incremento contatore numero e-mail per ccm/ctr
025400111020             wtotE+=1  ;
025500111014           endif;
025600111014           reade (acoksc) titas33c  ;
025700111017       enddo;
025800111020       if prectr<>*hival           ;
025900111014           exsr sr_aggWf            ;
026000111017       endif                       ;
026100111017       endsr;
026200111013       //-------------------------------------------------------------*
026300111021       //Scrittura del file di work                                   *
026400111021       //-------------------------------------------------------------*
026500111013       Begsr sr_aggWf  ;
026600130912        chain (CMMuni: acorag: acoksc: prectr) wfemd01l;
026700111017        if not %found(wfemd01l);
026800111017           clear wfemd000;
026900111017        endif;
027000111017        emdtsp+=wtotE;
027100111017        if %found(wfemd01l);
027200111017           update wfemd000;
027300111017        else;
027400111017           exsr sr_riefissi;
027500111017           write wfemd000;
027600111017        endif;
027700111017       endsr;
027800111017       //-------------------------------------------------------------*
027900111017       //Valorizzazione campi "fissi" WFEMD00F                         *
028000111017       //-------------------------------------------------------------*
028100111017       Begsr SR_Riefissi;
028200111017        EMDDEL=%dec(dataiso_cor);
028300111017        EMDUSR=knmus;
028400111017        EMDDAL=tae2dal;
028500111017        EMDAL=tae2al;
028600111020        EMDCCM=acoksc;
028700111017        EMDRAG=acorag;
028800130912        EMDAGE=CMMuni;
028900130912        EMDDAG=CMMdes;
029000130912        kfil=%dec(%subst(%editc(CMMuni:'X'):1:3):3:0);
029100111017        chain (kfil) azorg01l;
029200111017        if %found(azorg01l);
029300111017           EMDDIV=orgfl3 ;
029400111017           EMDCAR=orgcar ;
029500111017           EMDFIL=orgfil ;
029600111017           EMDDIL=orgdes;
029700111017        endif;
029800111017        t_tblkey=orgfl3;
029900111017        chain (1:'17':t_tblkey) tabel00f  ;
030000111017        if %found(tabel00f);
030100111017           ds17=t_tbluni;
030200111017           EMDddi=§17des;
030300111017        endif;
030400111017        t_tblkey=%editc(orgcar:'X');
030500111017        chain (1:'05':t_tblkey) tabel00f  ;
030600111017        if %found(tabel00f);
030700111017           ds05=t_tbluni;
030800111017           EMDDAR=§05des;
030900111017        endif;
031000111017        EMDCTR=prectr;
031100111017
031200111017        // Dati riguardanti la tariffa
031300111017
031400111020        chain (acoksc:prectr) tntam04l  ;
031500111017        if %found(tntam04l);
031600111017           exsr sr_tarparm;
031700111017           EMDPRG=tamprg;
031800111017           EMDDDT=tamddt;
031900111017           EMDDST=tamdst;
032000111017           if contatore>0;
032100111017              EMDVRM='S';
032200111017           endif;
032300111017           EMDFD1=wfd1;
032400111017           EMDSG1=wsg1;
032500111017           EMDFD2=wfd2;
032600111017           EMDSG2=wsg2;
032700111017           EMDFD3=wfd3;
032800111017           EMDSG3=wsg3;
032900111017           if overcts='S';
033000111017              EMDFCT='S';
033100111017           endif;
033200111017           if contatore>3;
033300111017              EMDFSG='S';
033400111017           endif;
033500111017        endif;
033600111020        t_tblkey=%editc(acoksc:'X');
033700111017        chain (1:'3C':t_tblkey) tabel00f  ;
033800111017        if %found(tabel00f);
033900111017           ds3c=t_tbluni;
034000111017           EMDCBA=§3ccba;
034100111017        endif;
034200111013       endsr;
034300111017       //-------------------------------------------------------------*
034400111017       //Ricerca presenza tariffa particolare "m"                      *
034500111017       //-------------------------------------------------------------*
034600111017       Begsr SR_TarparM;
034700111017       kftc='m ';
034800111017       clear wfd1;
034900111017       clear wfd2;
035000111017       clear wfd3;
035100111017       clear wsg1;
035200111017       clear wsg2;
035300111017       clear wsg3;
035400111017       clear wcts;
035500111017       clear overcts;
035600111017       contatore=0;
035700111017       setll (tamksc:tamctr:tamprg:kftc) titpd01l;
035800111017       reade (tamksc:tamctr:tamprg:kftc) titpd01l;
035900111017       dow not %eof(titpd01l);
036000111017       // elaboro solo quelle non annuallate
036100111017          if tpdatb=*blanks;
036200111017             if wcts=*blanks;
036300111017                wcts=tpdcts;
036400111017             endif;
036500111017             if wcts<>tpdcts;
036600111017                overcts='S';
036700111017                leave;
036800111017             endif;
036900111017             contatore+=1;
037000111021       // uniformo in scaglione 99999 tutti quelli con scaglione>99
037100111021             if tpdsgl>=99;
037200111021                tpdsgl= 99999,000;
037300111021             endif;
037400111017             select;
037500111017             when contatore=1;
037600111017                wfd1=tpditr;
037700111017                wsg1=tpdsgl;
037800111017             when contatore=2;
037900111017                wfd2=tpditr;
038000111017                wsg2=tpdsgl;
038100111017             when contatore=3;
038200111017                wfd3=tpditr;
038300111017                wsg3=tpdsgl;
038400111017             when contatore>3;
038500111017                leave;
038600111017             endsl;
038700111017          endif;
038800111019          reade (tamksc:tamctr:tamprg:kftc) titpd01l;
038900111017       enddo;
039000111017       endsr;
039100111020       //-------------------------------------------------------------*
039200111020       //Routine di impostazione campi di stampa record di testata    *
039300111020       //-------------------------------------------------------------*
039400111020       Begsr SR_impt00;
039500111020
039600111020       // Titolo
039700111020
039800111020       dataeur=%date(tae2dal:*iso);
039900111020       wdatagma=%dec(dataeur);
040000111020       ptitolo='Analisi clienti con servizio di avviso al destinatario dal';
040100111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
040200111020       dataeur=%date(tae2al:*iso);
040300111020       wdatagma=%dec(dataeur);
040400111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
040500111020
040600111020       // area se richiesta
040700111020       if tae2car>0   ;
040800130912          clear t_tblkey  ;
040900130912          eval t_tblkey=%editc(tae2car:'X') ;
041000130912          chain (1:'05':t_tblkey) tabel00f  ;
041100111020          if not %found(tabel00f);
041200111020             clear t_tbluni   ;
041300111020          endif   ;
041400111020          eval priare='Area  ' + %editc(tae2car:'X') +' ' + t_tbluni   ;
041500111020       endif  ;
041600111020
041700111020       endsr;
041800111021       //-------------------------------------------------------------*
041900111021       //Routine di elaborazione dati del Workfile per la stampa      *
042000111021       //-------------------------------------------------------------*
042100111021       Begsr SR_ElaEmd;
042200111021       clear savage;
042300111021       setll (*loval) wfemd01l;
042400111021       read  wfemd01l;
042500111021       dow not %eof(wfemd01l);
042600111021          *in40=*off;
042700111024          *in41=*off;
042800111021          // ROTTURA COMMERCIALE UNIFICANTE
042900111021          if emdage<>savage;
043000111021             exsr sr_rotage;
043100111021             savage=emdage;
043200111021          endif;
043300111024          // ROTTURA CLIENTE
043400111024          if emdccm<>savccm;
043500111024             // indicatore per stampare codice e rag.sociale
043600111024             *in41=*on;
043700111024             savccm=emdccm;
043800111024          endif;
043900111021          // PER OGNI RECORD LETTO TOTALIZZO LE SPEDIZIONI E STAMPO LA RIGA
044000111021          w_ttsp+=emdtsp;
044100111021          if $righe+2>57;
044200111021             exsr sr_overflow;
044300111021             write tae2rv1;
044400111021             $righe+=1;
044500111021          endif;
044600111021          exsr sr_rier01;
044700111021          write tae2r01;
044800111021          $righe+=2;
044900111021
045000111021          read  wfemd01l;
045100111021       enddo;
045200111021
045300111021
045400111021       // Stampo totale del commerciale
045500111021       if savage>0;
045600111021          exsr StaTotAge  ;
045700111021       endif;
045800111021       // totale generale solo se non richiesto 1 unificante
045900111021        if tae2age=0 and w_ttspgen>0 ;
046000111021           exsr staTOTGEN  ;
046100111021        endif  ;
046200111021       endsr;
046300111021       //-------------------------------------------------------------*
046400111021       //Valorizzazione campi riga di stampa                          *
046500111021       //-------------------------------------------------------------*
046600111021       Begsr SR_RieR01  ;
046700111021       PKSC=emdccm;
046800111021       PKSCDES=emdrag;
046900111021       PCBA=emdcba;
047000111021       PTSP=emdtsp;
047100111021       PCTR=emdctr;
047200111021       PPRG=emdprg;
047300111021       // conversione data decorrenza/scadenza
047400111021       dataeur=%date(emdddt:*iso);
047500111021       PDDT=%dec(dataeur);
047600111021       dataeur=%date(emddst:*iso);
047700111021       PDST=%dec(dataeur);
047800111021       PSG1=emdsg1;
047900111021       PFD1=emdfd1;
048000111021       endsr;
048100111021       //-------------------------------------------------------------*
048200111021       //Operazioni a rottura di commerciale unificante               *
048300111021       //-------------------------------------------------------------*
048400111021       Begsr SR_rotage;
048500111021          if savage>0;
048600111021             exsr statotage;
048700111021             // Eseguo il salto pagina per il prossimo commerciale
048800111021             exsr sr_overflow;
048900111021             write tae2rv1;
049000111021             $righe+=1;
049100111021          endif;
049200111021          *in40=*on;
049300111021          pageuni=emdage;
049400111021          pagedes=emddag;
049500111021       endsr;
049600111021       //-------------------------------------------------------------*
049700111021       //Stampa totale commerciale                                    *
049800111021       //-------------------------------------------------------------*
049900111021       Begsr StaTotAge;
050000111021             // Stampo totale del precedente commerciale
050100111021             *in40=*on;
050200111021             pgenerale='TOTALE COMMERCIALE';
050300111021             ttsp=w_ttsp;
050400111021             if $righe+2>61;
050500111021                exsr sr_overflow;
050600111021                write tae2rv1;
050700111021                $righe+=1;
050800111021             endif;
050900111021             write tae2tage1;
051000111021             write tae2tage2;
051100111021             $righe+=2;
051200111024             // incremento il totale generale e pulisco il totale commerciale
051300111024             w_ttspgen+=w_ttsp;
051400111024             clear w_ttsp;
051500111021       endsr;
051600111021       //-------------------------------------------------------------*
051700111021       //Stampa totale GENERALE
051800111021       //-------------------------------------------------------------*
051900111021       Begsr StaTOTGEN;
052000111021
052100111021       // Eseguo salto pagina
052200111021       exsr sr_overflow;
052300111021       write tae2rv1;
052400111021       $righe+=1;
052500111021       *in40=*off ;
052600111021       pgenerale='TOTALE GENERALE   '   ;
052700111021       ttsp=w_ttspgen;
052800111021       write tae2tage1;
052900111021       write tae2tage2 ;
053000111021       write tae2tage1;
053100111021       $righe+=3;
053200111021       endsr;
053300111021       //-------------------------------------------------------------*
053400111021       //Stampa intestazione per salto pagina                         *
053500111021       //-------------------------------------------------------------*
053600111021       Begsr SR_overflow;
053700111021          write tae2t00;
053800111021          write tae2t01;
053900111021          $righe=6;
054000111021          *in40=*on;
054100111024          *in41=*on;
054200111021       endsr;
