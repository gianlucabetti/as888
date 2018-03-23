000003111014      *---------------------------------------------------------------*
000004111014      * Analisi clienti con utilizzo servizio di avviso al destinatario
000005111014      *---------------------------------------------------------------*
000100111014     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000101111017     ftitas33c  if   e           k Disk    extfile(wlibsede)usropn
000102111017     ftabel00f  if   e           k disk    prefix(t_)
000407111013     fazorg01l  if   e           k disk
000410111017     ftntam04l  if   e           k Disk
000411111017     fTITPD01L  if   e           k Disk
000412111017     fWFEmd01l  uf a E           k disk
000413111020     ftntae2p   o    e             printer usropn
000414111012
000700090715     d
000701111020      *
000702111020      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
000703111020      *
000704111014     d KPJBA         e ds
000705111014     d tntae2ds      e ds
000801111012     d dtasflo       e ds
000802111017     d ds01          e ds
000803111017     d ds05          e ds
000804111017     d ds17          e ds
000805111017     d ds3c          e ds
000806111014     d cnclpds       e ds                  extname(CNCLP00F) inz
000807111014     d cnacods       e ds                  extname(cnaco00f) inz
000808111014     d tabelds       e ds                  extname(tabel00f) inz
000813111014     d Tibs34ds      e ds                  inz
000814111014     d §azute        e ds                  Extname(Azute00f)
000815111014     d                                     dtaara
000816111014     d §Datiute      e ds                  EXTname(ddatiute)
000817111014     d                                     dtaara
000818111014     d dute01        e ds
000819111014     d trul31ds      e ds
000820111014     d POG                            3  0 DIM(250) overlay(o31pog)
000821111014
000822111014      * - Indici di schiera / Contatori
000823111014     d i               s              3  0 inz
000824111014      * - Campi di comodo
000825111014     d Wtas33          s             21    inz('GAITRAGRU /TITAS33C')
000826111017     D Wlibsede        S             21
000827111014     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
000828111014     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
000829111014     d w_aasdal        s                   like(tasaas)
000830111014     d w_aasal         s                   like(tasaas)
000831111014     d w_mgsdal        s                   like(tasmgs)
000832111014     d w_mgsal         s                   like(tasmgs)
000833111014     d wabi            s                   like(UteAut)
000834111013     d kfil            s                   like(orgfil)
000835111013     d WtotE           s              5  0
000836111014     d prectr          s                   like(tasctr)
000837111017     d kftc            s                   like(tpdftc)
000838111017     d wfd1            s                   like(tpditr)
000839111017     d wfd2            s                   like(tpditr)
000840111017     d wfd3            s                   like(tpditr)
000841111017     d wsg1            s                   like(tpdsgl)
000842111017     d wsg2            s                   like(tpdsgl)
000843111017     d wsg3            s                   like(tpdsgl)
000844111017     d wcts            s                   like(tpdcts)
000845111017     d contatore       s              2  0
000846111017     d overcts         s              1
000847111014     d Primo           s              4  0 inz
000848111013     d dataiso_cor     s               d   datfmt(*iso)
000849111014     d dataeur_cor     s               d   datfmt(*eur)
000850111021     d dataeur         s               d   datfmt(*eur)
000851111021     d $righe          s              2  0
000852111021     d savage          s                   like(emdage)
000853111024     d savccm          s                   like(emdccm)
000854111021     d w_ttsp          s                   like(emdtsp)
000855111021     d w_ttspgen       s                   like(emdtsp)
000856111021     d wDataGMA        s              8  0
000912111012
000915111014     D WrkStringaSqlC  S           4500
000916111014     D                                     VARYING
000917111014     D WrkFilIn        S           4000
000918111014     D                                     VARYING
001201111013     d $Finerec        s              1    inz(*off)
001203111013
001204111014      // ? PROTOTIPI ?
001205111014      /copy gaitrasrc/srcprotopr,tibs34r
001206111014      /copy gaitrasrc/srcprotopr,trul31r
001208111014
001209111013     c     *Entry        plist
001210111013     c                   parm                    KPJBA
001211111013     c
001212111014     c                   movel     kpjbu         tntae2ds
001216111013      /free
001217111013         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
001218111013
001219111021       // ELABORAZIONE
001220111014       // preparo stringa sql per estrazione codici clienti da elaborare
001221111014       exsr sr_prepSql;
001222111014       // elaboro dati estratti
001223111014       exsr sr_Elabora;
001224111021
001225111020       // OPERAZIONI PER LA STAMPA
001226111020       Select;
001227111020
001228111020         // - Lancio da Filiale
001229111020         When  tae2crt <> 'S';
001230111020           open tntae2p;
001231111020           // Stampo intestazione
001232111020           exsr sr_impt00;
001233111020           write tae2t00;
001234111020           write tae2t01;
001235111021           $righe=6;
001241111020              exsr sr_elaemd;
001243111020           // Stampo *** Fine Stampa ***
001244111020           if $righe+2>60;
001245111020              exsr sr_overflow;
001246111020           endif;
001247111020           write fines;
001258111020       EndSl;
001266111013       eval *inlr=*on;
001267111014       //-------------------------------------------------------------*
001268111014       //Routine Iniziale                                             *
001269111014       //-------------------------------------------------------------*
001270111014       begsr *inzsr;
001271111014
001272111014       //Reperisco dati job
001273111014         exsr datijob;
001274111014
001275111014       if %subst(knsif:7:1) = 'P';
001276111014         Wlibsede = Wlibsedep;
001277111014         Else ;
001278111014         wlibsede = Wlibsedeb;
001279111014       endif;
001280111014       Open   Titas33c;
001287111014
001288111014       // Reperisco la data corrente
001289111014       dataiso_cor=(%date());
001290111014       dataeur_cor=dataiso_cor;
001291111021       wdate=%dec(dataeur_cor);
001292111021       utime= %dec(%time());
001298111014
001299111014       EndSr;
001300111014       //-------------------------------------------------------------*
001301111014       //Reperimento Dati del job (Utente/Operativi)                  *
001302111014       //-------------------------------------------------------------*
001303111014       Begsr DatiJob;
001304111014
001305111014          in(e) §azute;
001306111014          if not %error;
001307111014           in(e) §datiute;
001308111014          endif;
001309111014          if %error or rsut = *blanks;
001310111014           tibs34r(tibs34ds);
001311111014           in §azute;
001312111014           in §datiute;
001313111014          endif;
001314111014          dute01=utefaf;
001315111014          wabi=§utegtc;
001316111014         endsr;
001317111014       //-------------------------------------------------------------*
001318111014       //Preparazione stringa sql per Estrazione clienti da elaborare *
001319111014       //-------------------------------------------------------------*
001320111014       Begsr SR_prepSql;
001321111014
001322111014       WrkStringaSqlC='select * from cnaco00f, cnclp00f, tabel00f' +
001323111014                      ' where acokut=clpkut and acokcc=clpkcc and' +
001324111014                      ' acoksc=clpksc and clpage=tblkey and tblkut=1 and' +
001325111014                      ' tblcod=''01''';
001326111014       // richiesto un comm.le unificante specifico
001327111014       if tae2age>0;
001328111014          WrkStringaSqlC=WrkStringaSqlC+
001329111014          ' and substr(tbluni, 26, 7)=' + '''' + %editc(tae2age:'X')+ '''';
001330111014       else;
001331111014       // richiesta per area comm.le: preparo schiera delle filiali di appartene
001332111014          exsr sr_whrfil;
001333111014          WrkStringaSqlC=WrkStringaSqlC+WrkFilIn;
001334111014       endif;
001335111014
001336111014       endsr;
001337111014       //-------------------------------------------------------------*
001338111014       //Schiera filiali di appartenenza all'area del comm.le unific. *
001339111014       //-------------------------------------------------------------*
001340111014       Begsr SR_whrfil;
001341111014          Primo= 1 ;
001342111014
001343111014          reset WrkFilIn;
001344111014          clear trul31ds  ;
001345111014          I31abi = wabi    ;
001346111014          I31cpo = DUTpou  ;
001347111014          trul31r(kpjba:trul31ds);
001348111014          WrkFilIn= ' And substr(tbluni, 26, 3) in (';
001349111014          for I=1 to %elem(pog);
001350111014             if pog(i)=0;
001351111014                leave;
001352111014             endif;
001353111014             if tae2car>0   ;
001354111014              chain   pog(i) azorg01l  ;
001355111014             endif  ;
001356111014
001357111014             if tae2car=0 or
001358111014               (%found(azorg01l) and   orgcar=tae2car) ;
001359111014               if primo>1;
001360111014                 WrkFilIn=WrkFilIn + ' , ';
001361111014               endif;
001362111014             // se richiesta AREA, carico solo i p.o. dell'area richeista
001363111014             WrkFilIn=WrkFilIn + %char(pog(i));
001364111014             Primo=primo+1   ;
001365111014             endif;
001366111014          endfor;
001367111014          WrkFilIn=WrkFilIn + ')';
001368111014       endsr;
001369111014       //-------------------------------------------------------------*
001370111014       //Lettura ed elaborazione dati estratti con sql                *
001371111014       //-------------------------------------------------------------*
001372111014       Begsr SR_Elabora;
001373111014       // imposto key per titas
001374111014       w_aasdal=%dec(%subst(%editc(tae2dal:'X'):1:4):4:0);
001375111014       w_mgsdal=%dec(%subst(%editc(tae2dal:'X'):5:4):4:0);
001376111014       w_aasal=%dec(%subst(%editc(tae2al:'X'):1:4):4:0);
001377111014       w_mgsal=%dec(%subst(%editc(tae2al:'X'):5:4):4:0);
001378111014       $finerec=*off;
001379111014       exec sql prepare STRINGASQLC from :WrkStringaSqlC;
001380111014       exec sql declare ACOCsr cursor for StringaSqlC;
001381111014       exec sql open ACOcsr;
001382111014       dow $finerec=*off;
001383111014          exec sql Fetch ACOcsr into :cnacods, :cnclpds, :tabelds;
001384111014          if sqlcod=100 or sqlcod<0;
001385111014             $finerec = *on;
001386111014             leave;
001387111014          endif;
001403111014
001404111014       // Elaboro titas per ogni cliente caricato in schiera
001405111014          exsr sr_Etitas;
001406111014       enddo;
001407111014       endsr;
001408111014
001409111014       //-------------------------------------------------------------*
001410111014       //Elaborazione Titas per cliente mittente                      *
001411111014       //-------------------------------------------------------------*
001412111014       Begsr Sr_Etitas;
001413111014
001414111020       prectr=*hival;
001421111014
001422111014       setll (acoksc: w_aasdal: w_mgsdal) titas33c;
001423111014       reade (acoksc) titas33c  ;
001432111014       dow not %eof(titas33c);
001433111014           if tasaas>W_aasal  or (tasaas=W_aasal and tasmgs>W_mgsal);
001434111014              leave;
001435111014           endif;
001436111014         // elaboro solo se bolla con avviso al destinatario
001437111014           dtasflo=tasflo;
001438111014           if §floemd='S';
001439111014         // a cambio di CTR azzero il contatore ed eseguo tutte
001440111014         // le operazioni di ricerca dati accessori e scrivo record nel
001441111014         // file di work
001442111014             if tasctr<>prectr              ;
001443111020                if prectr<>*hival           ;
001444111017                   exsr sr_aggwf            ;
001446111014                endif                       ;
001447111014                WTotE=0                     ;
001448111014                prectr=tasctr               ;
001449111014             endif                          ;
001450111014        // incremento contatore numero e-mail per ccm/ctr
001451111020             wtotE+=1  ;
001452111014           endif;
001453111014           reade (acoksc) titas33c  ;
001454111017       enddo;
001455111020       if prectr<>*hival           ;
001456111014           exsr sr_aggWf            ;
001457111017       endif                       ;
001459111017       endsr;
001460111013       //-------------------------------------------------------------*
001461111021       //Scrittura del file di work                                   *
001462111021       //-------------------------------------------------------------*
001463111013       Begsr sr_aggWf  ;
001464111020        ds01=tbluni;
001465111020        chain (§01rgf: acorag: acoksc: prectr) wfemd01l;
001466111017        if not %found(wfemd01l);
001467111017           clear wfemd000;
001468111017        endif;
001469111017        emdtsp+=wtotE;
001470111017        if %found(wfemd01l);
001471111017           update wfemd000;
001472111017        else;
001473111017           exsr sr_riefissi;
001474111017           write wfemd000;
001475111017        endif;
001476111017       endsr;
001477111017       //-------------------------------------------------------------*
001478111017       //Valorizzazione campi "fissi" WFEMD00F                         *
001479111017       //-------------------------------------------------------------*
001480111017       Begsr SR_Riefissi;
001481111017        EMDDEL=%dec(dataiso_cor);
001482111017        EMDUSR=knmus;
001483111017        EMDDAL=tae2dal;
001484111017        EMDAL=tae2al;
001485111020        EMDCCM=acoksc;
001486111017        EMDRAG=acorag;
001487111017        EMDAGE=§01rgf;
001488111017        EMDDAG=§01age;
001489111017        kfil=%dec(%subst(%editc(§01rgf:'X'):1:3):3:0);
001490111017        chain (kfil) azorg01l;
001491111017        if %found(azorg01l);
001492111017           EMDDIV=orgfl3 ;
001493111017           EMDCAR=orgcar ;
001494111017           EMDFIL=orgfil ;
001495111017           EMDDIL=orgdes;
001496111017        endif;
001497111017        t_tblkey=orgfl3;
001498111017        chain (1:'17':t_tblkey) tabel00f  ;
001499111017        if %found(tabel00f);
001500111017           ds17=t_tbluni;
001501111017           EMDddi=§17des;
001502111017        endif;
001503111017        t_tblkey=%editc(orgcar:'X');
001504111017        chain (1:'05':t_tblkey) tabel00f  ;
001505111017        if %found(tabel00f);
001506111017           ds05=t_tbluni;
001507111017           EMDDAR=§05des;
001508111017        endif;
001509111017        EMDCTR=prectr;
001510111017
001511111017        // Dati riguardanti la tariffa
001512111017
001513111020        chain (acoksc:prectr) tntam04l  ;
001514111017        if %found(tntam04l);
001515111017           exsr sr_tarparm;
001516111017           EMDPRG=tamprg;
001517111017           EMDDDT=tamddt;
001518111017           EMDDST=tamdst;
001519111017           if contatore>0;
001520111017              EMDVRM='S';
001521111017           endif;
001522111017           EMDFD1=wfd1;
001523111017           EMDSG1=wsg1;
001524111017           EMDFD2=wfd2;
001525111017           EMDSG2=wsg2;
001526111017           EMDFD3=wfd3;
001527111017           EMDSG3=wsg3;
001528111017           if overcts='S';
001529111017              EMDFCT='S';
001530111017           endif;
001531111017           if contatore>3;
001532111017              EMDFSG='S';
001533111017           endif;
001534111017        endif;
001535111020        t_tblkey=%editc(acoksc:'X');
001536111017        chain (1:'3C':t_tblkey) tabel00f  ;
001537111017        if %found(tabel00f);
001538111017           ds3c=t_tbluni;
001539111017           EMDCBA=§3ccba;
001540111017        endif;
001541111013       endsr;
001600111017       //-------------------------------------------------------------*
001700111017       //Ricerca presenza tariffa particolare "m"                      *
001800111017       //-------------------------------------------------------------*
001900111017       Begsr SR_TarparM;
001901111017       kftc='m ';
001902111017       clear wfd1;
001903111017       clear wfd2;
001904111017       clear wfd3;
001905111017       clear wsg1;
001906111017       clear wsg2;
001907111017       clear wsg3;
001908111017       clear wcts;
001909111017       clear overcts;
001910111017       contatore=0;
001911111017       setll (tamksc:tamctr:tamprg:kftc) titpd01l;
001912111017       reade (tamksc:tamctr:tamprg:kftc) titpd01l;
001913111017       dow not %eof(titpd01l);
001915111017       // elaboro solo quelle non annuallate
001916111017          if tpdatb=*blanks;
001917111017             if wcts=*blanks;
001918111017                wcts=tpdcts;
001919111017             endif;
001920111017             if wcts<>tpdcts;
001921111017                overcts='S';
001922111017                leave;
001923111017             endif;
001924111017             contatore+=1;
001925111021       // uniformo in scaglione 99999 tutti quelli con scaglione>99
001926111021             if tpdsgl>=99;
001927111021                tpdsgl= 99999,000;
001928111021             endif;
001929111017             select;
001930111017             when contatore=1;
001931111017                wfd1=tpditr;
001932111017                wsg1=tpdsgl;
001933111017             when contatore=2;
001934111017                wfd2=tpditr;
001935111017                wsg2=tpdsgl;
001936111017             when contatore=3;
001937111017                wfd3=tpditr;
001938111017                wsg3=tpdsgl;
001939111017             when contatore>3;
001940111017                leave;
001941111017             endsl;
001942111017          endif;
001943111019          reade (tamksc:tamctr:tamprg:kftc) titpd01l;
001944111017       enddo;
002000111017       endsr;
002100111020       //-------------------------------------------------------------*
002200111020       //Routine di impostazione campi di stampa record di testata    *
002300111020       //-------------------------------------------------------------*
002400111020       Begsr SR_impt00;
002500111020
002600111020       // Titolo
002700111020
002800111020       dataeur=%date(tae2dal:*iso);
002900111020       wdatagma=%dec(dataeur);
003000111020       ptitolo='Analisi clienti con servizio di avviso al destinatario dal';
003100111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
003200111020       dataeur=%date(tae2al:*iso);
003300111020       wdatagma=%dec(dataeur);
003400111020       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
008100111020
008200111020       // area se richiesta
008300111020       if tae2car>0   ;
008400111020          clear tblkey  ;
008500111020          eval tblkey=%editc(tae2car:'X') ;
008600111020          chain (1:'05':tblkey) tabel00f  ;
008700111020          if not %found(tabel00f);
008800111020             clear t_tbluni   ;
008900111020          endif   ;
009000111020          eval priare='Area  ' + %editc(tae2car:'X') +' ' + t_tbluni   ;
009100111020       endif  ;
009200111020
009300111020       endsr;
009400111021       //-------------------------------------------------------------*
009500111021       //Routine di elaborazione dati del Workfile per la stampa      *
009600111021       //-------------------------------------------------------------*
009700111021       Begsr SR_ElaEmd;
009701111021       clear savage;
009702111021       setll (*loval) wfemd01l;
009703111021       read  wfemd01l;
009704111021       dow not %eof(wfemd01l);
009705111021          *in40=*off;
009706111024          *in41=*off;
009707111021          // ROTTURA COMMERCIALE UNIFICANTE
009708111021          if emdage<>savage;
009709111021             exsr sr_rotage;
009710111021             savage=emdage;
009730111021          endif;
009731111024          // ROTTURA CLIENTE
009732111024          if emdccm<>savccm;
009733111024             // indicatore per stampare codice e rag.sociale
009734111024             *in41=*on;
009735111024             savccm=emdccm;
009736111024          endif;
009737111021          // PER OGNI RECORD LETTO TOTALIZZO LE SPEDIZIONI E STAMPO LA RIGA
009738111021          w_ttsp+=emdtsp;
009739111021          if $righe+2>57;
009740111021             exsr sr_overflow;
009741111021             write tae2rv1;
009742111021             $righe+=1;
009743111021          endif;
009744111021          exsr sr_rier01;
009745111021          write tae2r01;
009746111021          $righe+=2;
009747111021
009748111021          read  wfemd01l;
009749111021       enddo;
009750111021
009751111021
009752111021       // Stampo totale del commerciale
009753111021       if savage>0;
009754111021          exsr StaTotAge  ;
009755111021       endif;
009756111021       // totale generale solo se non richiesto 1 unificante
009757111021        if tae2age=0 and w_ttspgen>0 ;
009758111021           exsr staTOTGEN  ;
009759111021        endif  ;
009800111021       endsr;
009801111021       //-------------------------------------------------------------*
009802111021       //Valorizzazione campi riga di stampa                          *
009803111021       //-------------------------------------------------------------*
009804111021       Begsr SR_RieR01  ;
009805111021       PKSC=emdccm;
009806111021       PKSCDES=emdrag;
009807111021       PCBA=emdcba;
009808111021       PTSP=emdtsp;
009810111021       PCTR=emdctr;
009811111021       PPRG=emdprg;
009812111021       // conversione data decorrenza/scadenza
009813111021       dataeur=%date(emdddt:*iso);
009814111021       PDDT=%dec(dataeur);
009815111021       dataeur=%date(emddst:*iso);
009816111021       PDST=%dec(dataeur);
009818111021       PSG1=emdsg1;
009820111021       PFD1=emdfd1;
009823111021       endsr;
009824111021       //-------------------------------------------------------------*
009825111021       //Operazioni a rottura di commerciale unificante               *
009826111021       //-------------------------------------------------------------*
009827111021       Begsr SR_rotage;
009829111021          if savage>0;
009830111021             exsr statotage;
009843111021             // Eseguo il salto pagina per il prossimo commerciale
009844111021             exsr sr_overflow;
009845111021             write tae2rv1;
009846111021             $righe+=1;
009847111021          endif;
009851111021          *in40=*on;
009852111021          pageuni=emdage;
009853111021          pagedes=emddag;
009854111021       endsr;
009855111021       //-------------------------------------------------------------*
009856111021       //Stampa totale commerciale                                    *
009857111021       //-------------------------------------------------------------*
009858111021       Begsr StaTotAge;
009859111021             // Stampo totale del precedente commerciale
009860111021             *in40=*on;
009861111021             pgenerale='TOTALE COMMERCIALE';
009862111021             ttsp=w_ttsp;
009863111021             if $righe+2>61;
009864111021                exsr sr_overflow;
009865111021                write tae2rv1;
009866111021                $righe+=1;
009867111021             endif;
009868111021             write tae2tage1;
009869111021             write tae2tage2;
009870111021             $righe+=2;
009871111024             // incremento il totale generale e pulisco il totale commerciale
009872111024             w_ttspgen+=w_ttsp;
009873111024             clear w_ttsp;
009874111021       endsr;
009875111021       //-------------------------------------------------------------*
009876111021       //Stampa totale GENERALE
009877111021       //-------------------------------------------------------------*
009878111021       Begsr StaTOTGEN;
009879111021
009880111021       // Eseguo salto pagina
009881111021       exsr sr_overflow;
009882111021       write tae2rv1;
009883111021       $righe+=1;
009884111021       *in40=*off ;
009925111021       pgenerale='TOTALE GENERALE   '   ;
009926111021       ttsp=w_ttspgen;
009927111021       write tae2tage1;
009928111021       write tae2tage2 ;
009929111021       write tae2tage1;
009930111021       $righe+=3;
009933111021       endsr;
009934111021       //-------------------------------------------------------------*
010000111021       //Stampa intestazione per salto pagina                         *
010100111021       //-------------------------------------------------------------*
010200111021       Begsr SR_overflow;
010300111021          write tae2t00;
010400111021          write tae2t01;
010500111021          $righe=6;
010600111021          *in40=*on;
010601111024          *in41=*on;
010800111021       endsr;
