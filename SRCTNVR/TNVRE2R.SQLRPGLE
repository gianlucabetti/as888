000100111014      *---------------------------------------------------------------*
000200111014      * Analisi clienti con utilizzo servizio di avviso al destinatario
000300111014      *---------------------------------------------------------------*
000400111014     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000500111017     ftitas33c  if   e           k Disk    extfile(wlibsede)usropn
000600111017     ftabel00f  if   e           k disk    prefix(t_)
000700111013     fazorg01l  if   e           k disk
000800111017     ftntam04l  if   e           k Disk
000900111108     ffiar501l  if   e           k Disk    extfile(wlibAR5)usropn
001000111017     fTITPD01L  if   e           k Disk
001100111017     fWFEmd01l  uf a E           k disk
001200111020     ftntae2p   o    e             printer usropn
001300111012
001400090715     d
001500111020      *
001600111020      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
001700111020      *
001800111014     d KPJBA         e ds
001900111014     d tntae2ds      e ds
002000111012     d dtasflo       e ds
002100111017     d ds01          e ds
002200111017     d ds05          e ds
002300111017     d ds17          e ds
002400111017     d ds3c          e ds
002500111014     d cnclpds       e ds                  extname(CNCLP00F) inz
002600111014     d cnacods       e ds                  extname(cnaco00f) inz
002700111014     d tabelds       e ds                  extname(tabel00f) inz
002800111014     d Tibs34ds      e ds                  inz
002900111014     d §azute        e ds                  Extname(Azute00f)
003000111014     d                                     dtaara
003100111014     d §Datiute      e ds                  EXTname(ddatiute)
003200111014     d                                     dtaara
003300111014     d dute01        e ds
003400111014     d trul31ds      e ds
003500111014     d POG                            3  0 DIM(250) overlay(o31pog)
003600111014
003700111014      * - Indici di schiera / Contatori
003800111014     d i               s              3  0 inz
003900111014      * - Campi di comodo
004000111014     d Wtas33          s             21    inz('GAITRAGRU /TITAS33C')
004100111017     D Wlibsede        S             21
004200111014     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
004300111014     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
004400111108     D WlibAR5         S             21    INZ('FILTRA201 /FIAR501L')
004500111014     d w_aasdal        s                   like(tasaas)
004600111014     d w_aasal         s                   like(tasaas)
004700111014     d w_mgsdal        s                   like(tasmgs)
004800111014     d w_mgsal         s                   like(tasmgs)
004900111014     d wabi            s                   like(UteAut)
005000111013     d kfil            s                   like(orgfil)
005100111013     d WtotE           s              5  0
005200111014     d prectr          s                   like(tasctr)
005300111017     d kftc            s                   like(tpdftc)
005400111017     d wfd1            s                   like(tpditr)
005500111017     d wfd2            s                   like(tpditr)
005600111017     d wfd3            s                   like(tpditr)
005700111017     d wsg1            s                   like(tpdsgl)
005800111017     d wsg2            s                   like(tpdsgl)
005900111017     d wsg3            s                   like(tpdsgl)
006000111017     d wcts            s                   like(tpdcts)
006100111017     d contatore       s              2  0
006200111017     d overcts         s              1
006300111014     d Primo           s              4  0 inz
006400111013     d dataiso_cor     s               d   datfmt(*iso)
006500111014     d dataeur_cor     s               d   datfmt(*eur)
006600111021     d dataeur         s               d   datfmt(*eur)
006700111021     d $righe          s              2  0
006800111021     d savage          s                   like(emdage)
006900111024     d savccm          s                   like(emdccm)
007000111021     d w_ttsp          s                   like(emdtsp)
007100111021     d w_ttspgen       s                   like(emdtsp)
007200111108     d KTRD            s                   like(AR5TRD) INZ('EMD')
007300111021     d wDataGMA        s              8  0
007400111012
007500111014     D WrkStringaSqlC  S           4500
007600111014     D                                     VARYING
007700111014     D WrkFilIn        S           4000
007800111014     D                                     VARYING
007900111013     d $Finerec        s              1    inz(*off)
008000111013
008100111014      // ? PROTOTIPI ?
008200111014      /copy gaitrasrc/srcprotopr,tibs34r
008300111014      /copy gaitrasrc/srcprotopr,trul31r
008400111014
008500111013     c     *Entry        plist
008600111013     c                   parm                    KPJBA
008700111013     c
008800111014     c                   movel     kpjbu         tntae2ds
008900111013      /free
009000111013         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009100111013
009200111021       // ELABORAZIONE
009300111014       // preparo stringa sql per estrazione codici clienti da elaborare
009400111014       exsr sr_prepSql;
009500111014       // elaboro dati estratti
009600111014       exsr sr_Elabora;
009700111021
009800111020       // OPERAZIONI PER LA STAMPA
009900111020       Select;
010000111020
010100111020         // - Lancio da Filiale
010200111020         When  tae2crt <> 'S';
010300111020           open tntae2p;
010400111020           // Stampo intestazione
010500111020           exsr sr_impt00;
010600111020           write tae2t00;
010700111020           write tae2t01;
010800111021           $righe=6;
010900111020              exsr sr_elaemd;
011000111020           // Stampo *** Fine Stampa ***
011100111020           if $righe+2>60;
011200111020              exsr sr_overflow;
011300111020           endif;
011400111020           write fines;
011500111020       EndSl;
011600111013       eval *inlr=*on;
011700111014       //-------------------------------------------------------------*
011800111014       //Routine Iniziale                                             *
011900111014       //-------------------------------------------------------------*
012000111014       begsr *inzsr;
012100111014
012200111014       //Reperisco dati job
012300111014         exsr datijob;
012400111014
012500111014       if %subst(knsif:7:1) = 'P';
012600111014         Wlibsede = Wlibsedep;
012700111014         Else ;
012800111014         wlibsede = Wlibsedeb;
012900111014       endif;
013000111014       Open   Titas33c;
013100111108       Open   FIAR501L;
013200111014
013300111014       // Reperisco la data corrente
013400111014       dataiso_cor=(%date());
013500111014       dataeur_cor=dataiso_cor;
013600111021       wdate=%dec(dataeur_cor);
013700111021       utime= %dec(%time());
013800111014
013900111014       EndSr;
014000111014       //-------------------------------------------------------------*
014100111014       //Reperimento Dati del job (Utente/Operativi)                  *
014200111014       //-------------------------------------------------------------*
014300111014       Begsr DatiJob;
014400111014
014500111014          in(e) §azute;
014600111014          if not %error;
014700111014           in(e) §datiute;
014800111014          endif;
014900111014          if %error or rsut = *blanks;
015000111014           tibs34r(tibs34ds);
015100111014           in §azute;
015200111014           in §datiute;
015300111014          endif;
015400111014          dute01=utefaf;
015500111014          wabi=§utegtc;
015600111014         endsr;
015700111014       //-------------------------------------------------------------*
015800111014       //Preparazione stringa sql per Estrazione clienti da elaborare *
015900111014       //-------------------------------------------------------------*
016000111014       Begsr SR_prepSql;
016100111014
016200111014       WrkStringaSqlC='select * from cnaco00f, cnclp00f, tabel00f' +
016300111014                      ' where acokut=clpkut and acokcc=clpkcc and' +
016400111014                      ' acoksc=clpksc and clpage=tblkey and tblkut=1 and' +
016500111014                      ' tblcod=''01''';
016600111014       // richiesto un comm.le unificante specifico
016700111014       if tae2age>0;
016800111014          WrkStringaSqlC=WrkStringaSqlC+
016900111014          ' and substr(tbluni, 26, 7)=' + '''' + %editc(tae2age:'X')+ '''';
017000111014       else;
017100111014       // richiesta per area comm.le: preparo schiera delle filiali di appartene
017200111014          exsr sr_whrfil;
017300111014          WrkStringaSqlC=WrkStringaSqlC+WrkFilIn;
017400111014       endif;
017500111014
017600111014       endsr;
017700111014       //-------------------------------------------------------------*
017800111014       //Schiera filiali di appartenenza all'area del comm.le unific. *
017900111014       //-------------------------------------------------------------*
018000111014       Begsr SR_whrfil;
018100111014          Primo= 1 ;
018200111014
018300111014          reset WrkFilIn;
018400111014          clear trul31ds  ;
018500111014          I31abi = wabi    ;
018600111014          I31cpo = DUTpou  ;
018700111014          trul31r(kpjba:trul31ds);
018800111014          WrkFilIn= ' And substr(tbluni, 26, 3) in (';
018900111014          for I=1 to %elem(pog);
019000111014             if pog(i)=0;
019100111014                leave;
019200111014             endif;
019300111014             if tae2car>0   ;
019400111014              chain   pog(i) azorg01l  ;
019500111014             endif  ;
019600111014
019700111014             if tae2car=0 or
019800111014               (%found(azorg01l) and   orgcar=tae2car) ;
019900111014               if primo>1;
020000111014                 WrkFilIn=WrkFilIn + ' , ';
020100111014               endif;
020200111014             // se richiesta AREA, carico solo i p.o. dell'area richeista
020300111014             WrkFilIn=WrkFilIn + %char(pog(i));
020400111014             Primo=primo+1   ;
020500111014             endif;
020600111014          endfor;
020700111014          WrkFilIn=WrkFilIn + ')';
020800111014       endsr;
020900111014       //-------------------------------------------------------------*
021000111014       //Lettura ed elaborazione dati estratti con sql                *
021100111014       //-------------------------------------------------------------*
021200111014       Begsr SR_Elabora;
021300111014       // imposto key per titas
021400111014       w_aasdal=%dec(%subst(%editc(tae2dal:'X'):1:4):4:0);
021500111014       w_mgsdal=%dec(%subst(%editc(tae2dal:'X'):5:4):4:0);
021600111014       w_aasal=%dec(%subst(%editc(tae2al:'X'):1:4):4:0);
021700111014       w_mgsal=%dec(%subst(%editc(tae2al:'X'):5:4):4:0);
021800111014       $finerec=*off;
021900111014       exec sql prepare STRINGASQLC from :WrkStringaSqlC;
022000111014       exec sql declare ACOCsr cursor for StringaSqlC;
022100111014       exec sql open ACOcsr;
022200111014       dow $finerec=*off;
022300111014          exec sql Fetch ACOcsr into :cnacods, :cnclpds, :tabelds;
022400111014          if sqlcod=100 or sqlcod<0;
022500111014             $finerec = *on;
022600111014             leave;
022700111014          endif;
022800111014
022900111014       // Elaboro titas per ogni cliente caricato in schiera
023000111014          exsr sr_Etitas;
023100111014       enddo;
023200111014       endsr;
023300111014
023400111014       //-------------------------------------------------------------*
023500111014       //Elaborazione Titas per cliente mittente                      *
023600111014       //-------------------------------------------------------------*
023700111014       Begsr Sr_Etitas;
023800111014
023900111020       prectr=*hival;
024000111014
024100111014       setll (acoksc: w_aasdal: w_mgsdal) titas33c;
024200111014       reade (acoksc) titas33c  ;
024300111014       dow not %eof(titas33c);
024400111014           if tasaas>W_aasal  or (tasaas=W_aasal and tasmgs>W_mgsal);
024500111014              leave;
024600111014           endif;
024700111014         // elaboro solo se bolla con avviso al destinatario
024800111108
024900111108
025000111108        CHAIN (TASAAS:TASLNP:TASNRS:TASNSP:KTRD)  FIAR501L   ;
025100111108        IF %FOUND(FIAR501L)  ;
025200111108
025300111014         // a cambio di CTR azzero il contatore ed eseguo tutte
025400111014         // le operazioni di ricerca dati accessori e scrivo record nel
025500111014         // file di work
025600111014             if tasctr<>prectr              ;
025700111020                if prectr<>*hival           ;
025800111017                   exsr sr_aggwf            ;
025900111014                endif                       ;
026000111014                WTotE=0                     ;
026100111014                prectr=tasctr               ;
026200111014             endif                          ;
026300111014        // incremento contatore numero e-mail per ccm/ctr
026400111020             wtotE+=1  ;
026500111014           endif;
026600111014           reade (acoksc) titas33c  ;
026700111017       enddo;
026800111020       if prectr<>*hival           ;
026900111014           exsr sr_aggWf            ;
027000111017       endif                       ;
027100111017       endsr;
027200111013       //-------------------------------------------------------------*
027300111021       //Scrittura del file di work                                   *
027400111021       //-------------------------------------------------------------*
027500111013       Begsr sr_aggWf  ;
027600111020        ds01=tbluni;
027700111020        chain (§01rgf: acorag: acoksc: prectr) wfemd01l;
027800111017        if not %found(wfemd01l);
027900111017           clear wfemd000;
028000111017        endif;
028100111017        emdtsp+=wtotE;
028200111017        if %found(wfemd01l);
028300111017           update wfemd000;
028400111017        else;
028500111017           exsr sr_riefissi;
028600111017           write wfemd000;
028700111017        endif;
028800111017       endsr;
028900111017       //-------------------------------------------------------------*
029000111017       //Valorizzazione campi "fissi" WFEMD00F                         *
029100111017       //-------------------------------------------------------------*
029200111017       Begsr SR_Riefissi;
029300111017        EMDDEL=%dec(dataiso_cor);
029400111017        EMDUSR=knmus;
029500111017        EMDDAL=tae2dal;
029600111017        EMDAL=tae2al;
029700111020        EMDCCM=acoksc;
029800111017        EMDRAG=acorag;
029900111017        EMDAGE=§01rgf;
030000111017        EMDDAG=§01age;
030100111017        kfil=%dec(%subst(%editc(§01rgf:'X'):1:3):3:0);
030200111017        chain (kfil) azorg01l;
030300111017        if %found(azorg01l);
030400111017           EMDDIV=orgfl3 ;
030500111017           EMDCAR=orgcar ;
030600111017           EMDFIL=orgfil ;
030700111017           EMDDIL=orgdes;
030800111017        endif;
030900111017        t_tblkey=orgfl3;
031000111017        chain (1:'17':t_tblkey) tabel00f  ;
031100111017        if %found(tabel00f);
031200111017           ds17=t_tbluni;
031300111017           EMDddi=§17des;
031400111017        endif;
031500111017        t_tblkey=%editc(orgcar:'X');
031600111017        chain (1:'05':t_tblkey) tabel00f  ;
031700111017        if %found(tabel00f);
031800111017           ds05=t_tbluni;
031900111017           EMDDAR=§05des;
032000111017        endif;
032100111017        EMDCTR=prectr;
032200111017
032300111017        // Dati riguardanti la tariffa
032400111017
032500111020        chain (acoksc:prectr) tntam04l  ;
032600111017        if %found(tntam04l);
032700111017           exsr sr_tarparm;
032800111017           EMDPRG=tamprg;
032900111017           EMDDDT=tamddt;
033000111017           EMDDST=tamdst;
033100111017           if contatore>0;
033200111017              EMDVRM='S';
033300111017           endif;
033400111017           EMDFD1=wfd1;
033500111017           EMDSG1=wsg1;
033600111017           EMDFD2=wfd2;
033700111017           EMDSG2=wsg2;
033800111017           EMDFD3=wfd3;
033900111017           EMDSG3=wsg3;
034000111017           if overcts='S';
034100111017              EMDFCT='S';
034200111017           endif;
034300111017           if contatore>3;
034400111017              EMDFSG='S';
034500111017           endif;
034600111017        endif;
034700111020        t_tblkey=%editc(acoksc:'X');
034800111017        chain (1:'3C':t_tblkey) tabel00f  ;
034900111017        if %found(tabel00f);
035000111017           ds3c=t_tbluni;
035100111017           EMDCBA=§3ccba;
035200111017        endif;
035300111013       endsr;
035400111017       //-------------------------------------------------------------*
035500111017       //Ricerca presenza tariffa particolare "m"                      *
035600111017       //-------------------------------------------------------------*
035700111017       Begsr SR_TarparM;
035800111017       kftc='m ';
035900111017       clear wfd1;
036000111017       clear wfd2;
036100111017       clear wfd3;
036200111017       clear wsg1;
036300111017       clear wsg2;
036400111017       clear wsg3;
036500111017       clear wcts;
036600111017       clear overcts;
036700111017       contatore=0;
036800111017       setll (tamksc:tamctr:tamprg:kftc) titpd01l;
036900111017       reade (tamksc:tamctr:tamprg:kftc) titpd01l;
037000111017       dow not %eof(titpd01l);
037100111017       // elaboro solo quelle non annuallate
037200111017          if tpdatb=*blanks;
037300111017             if wcts=*blanks;
037400111017                wcts=tpdcts;
037500111017             endif;
037600111017             if wcts<>tpdcts;
037700111017                overcts='S';
037800111017                leave;
037900111017             endif;
038000111017             contatore+=1;
038100111021       // uniformo in scaglione 99999 tutti quelli con scaglione>99
038200111021             if tpdsgl>=99;
038300111021                tpdsgl= 99999,000;
038400111021             endif;
038500111017             select;
038600111017             when contatore=1;
038700111017                wfd1=tpditr;
038800111017                wsg1=tpdsgl;
038900111017             when contatore=2;
039000111017                wfd2=tpditr;
039100111017                wsg2=tpdsgl;
039200111017             when contatore=3;
039300111017                wfd3=tpditr;
039400111017                wsg3=tpdsgl;
039500111017             when contatore>3;
039600111017                leave;
039700111017             endsl;
039800111017          endif;
039900111019          reade (tamksc:tamctr:tamprg:kftc) titpd01l;
040000111017       enddo;
040100111017       endsr;
040200111020       //-------------------------------------------------------------*
040300111020       //Routine di impostazione campi di stampa record di testata    *
040400111020       //-------------------------------------------------------------*
040500111020       Begsr SR_impt00;
040600111020
040700111020       // Titolo
040800111020
040900111020       dataeur=%date(tae2dal:*iso);
041000111020       wdatagma=%dec(dataeur);
041100111020       ptitolo='Analisi clienti con servizio di avviso al destinatario dal';
041200111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
041300111020       dataeur=%date(tae2al:*iso);
041400111020       wdatagma=%dec(dataeur);
041500111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
041600111020
041700111020       // area se richiesta
041800111020       if tae2car>0   ;
041900111020          clear tblkey  ;
042000111020          eval tblkey=%editc(tae2car:'X') ;
042100111020          chain (1:'05':tblkey) tabel00f  ;
042200111020          if not %found(tabel00f);
042300111020             clear t_tbluni   ;
042400111020          endif   ;
042500111020          eval priare='Area  ' + %editc(tae2car:'X') +' ' + t_tbluni   ;
042600111020       endif  ;
042700111020
042800111020       endsr;
042900111021       //-------------------------------------------------------------*
043000111021       //Routine di elaborazione dati del Workfile per la stampa      *
043100111021       //-------------------------------------------------------------*
043200111021       Begsr SR_ElaEmd;
043300111021       clear savage;
043400111021       setll (*loval) wfemd01l;
043500111021       read  wfemd01l;
043600111021       dow not %eof(wfemd01l);
043700111021          *in40=*off;
043800111024          *in41=*off;
043900111021          // ROTTURA COMMERCIALE UNIFICANTE
044000111021          if emdage<>savage;
044100111021             exsr sr_rotage;
044200111021             savage=emdage;
044300111021          endif;
044400111024          // ROTTURA CLIENTE
044500111024          if emdccm<>savccm;
044600111024             // indicatore per stampare codice e rag.sociale
044700111024             *in41=*on;
044800111024             savccm=emdccm;
044900111024          endif;
045000111021          // PER OGNI RECORD LETTO TOTALIZZO LE SPEDIZIONI E STAMPO LA RIGA
045100111021          w_ttsp+=emdtsp;
045200111021          if $righe+2>57;
045300111021             exsr sr_overflow;
045400111021             write tae2rv1;
045500111021             $righe+=1;
045600111021          endif;
045700111021          exsr sr_rier01;
045800111021          write tae2r01;
045900111021          $righe+=2;
046000111021
046100111021          read  wfemd01l;
046200111021       enddo;
046300111021
046400111021
046500111021       // Stampo totale del commerciale
046600111021       if savage>0;
046700111021          exsr StaTotAge  ;
046800111021       endif;
046900111021       // totale generale solo se non richiesto 1 unificante
047000111021        if tae2age=0 and w_ttspgen>0 ;
047100111021           exsr staTOTGEN  ;
047200111021        endif  ;
047300111021       endsr;
047400111021       //-------------------------------------------------------------*
047500111021       //Valorizzazione campi riga di stampa                          *
047600111021       //-------------------------------------------------------------*
047700111021       Begsr SR_RieR01  ;
047800111021       PKSC=emdccm;
047900111021       PKSCDES=emdrag;
048000111021       PCBA=emdcba;
048100111021       PTSP=emdtsp;
048200111021       PCTR=emdctr;
048300111021       PPRG=emdprg;
048400111021       // conversione data decorrenza/scadenza
048500111021       dataeur=%date(emdddt:*iso);
048600111021       PDDT=%dec(dataeur);
048700111021       dataeur=%date(emddst:*iso);
048800111021       PDST=%dec(dataeur);
048900111021       PSG1=emdsg1;
049000111021       PFD1=emdfd1;
049100111021       endsr;
049200111021       //-------------------------------------------------------------*
049300111021       //Operazioni a rottura di commerciale unificante               *
049400111021       //-------------------------------------------------------------*
049500111021       Begsr SR_rotage;
049600111021          if savage>0;
049700111021             exsr statotage;
049800111021             // Eseguo il salto pagina per il prossimo commerciale
049900111021             exsr sr_overflow;
050000111021             write tae2rv1;
050100111021             $righe+=1;
050200111021          endif;
050300111021          *in40=*on;
050400111021          pageuni=emdage;
050500111021          pagedes=emddag;
050600111021       endsr;
050700111021       //-------------------------------------------------------------*
050800111021       //Stampa totale commerciale                                    *
050900111021       //-------------------------------------------------------------*
051000111021       Begsr StaTotAge;
051100111021             // Stampo totale del precedente commerciale
051200111021             *in40=*on;
051300111021             pgenerale='TOTALE COMMERCIALE';
051400111021             ttsp=w_ttsp;
051500111021             if $righe+2>61;
051600111021                exsr sr_overflow;
051700111021                write tae2rv1;
051800111021                $righe+=1;
051900111021             endif;
052000111021             write tae2tage1;
052100111021             write tae2tage2;
052200111021             $righe+=2;
052300111024             // incremento il totale generale e pulisco il totale commerciale
052400111024             w_ttspgen+=w_ttsp;
052500111024             clear w_ttsp;
052600111021       endsr;
052700111021       //-------------------------------------------------------------*
052800111021       //Stampa totale GENERALE
052900111021       //-------------------------------------------------------------*
053000111021       Begsr StaTOTGEN;
053100111021
053200111021       // Eseguo salto pagina
053300111021       exsr sr_overflow;
053400111021       write tae2rv1;
053500111021       $righe+=1;
053600111021       *in40=*off ;
053700111021       pgenerale='TOTALE GENERALE   '   ;
053800111021       ttsp=w_ttspgen;
053900111021       write tae2tage1;
054000111021       write tae2tage2 ;
054100111021       write tae2tage1;
054200111021       $righe+=3;
054300111021       endsr;
054400111021       //-------------------------------------------------------------*
054500111021       //Stampa intestazione per salto pagina                         *
054600111021       //-------------------------------------------------------------*
054700111021       Begsr SR_overflow;
054800111021          write tae2t00;
054900111021          write tae2t01;
055000111021          $righe=6;
055100111021          *in40=*on;
055200111024          *in41=*on;
055300111021       endsr;
