000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200130509      * FNlST8R *----------------------------------------------------*
000300130509      *      - Stampa fogli viaggio partenza per terminal di arrivo  *
000400940915      *--------------------------------------------------------------*
000500900517      * INDICATORI USATI
000600951025      *--------------------------------------------------------------*
006800900517      *--------------------------------------------------------------*
012400020805      *
012500020805      * DEFINIZIONE SCHIERE
012600020805      *
016600020213
016601130510     ftabel00f  if   e           k disk
016602130513     fazorg01l  if   e           k disk
016603130513     ftntlt01l  if   e           k disk
016604130513     ftntlr02l  if   e           k disk
016605130513     ffnfv401l  if   e           k disk
016606130513     fFNLST8P   o    e             printer oflind(*in98)
016700020805      *
016800020805      * DEFINIZIONE CAMPI
016900020805      *
016901130509     D WrkStringaSql   S           4500
016902130509     D                                     VARYING
017300130509     D w0060           S              6  0
017301130510     D w003a           S              3
017302130509     D wkey            S                   like(tblkey)
017303130509     d time            s               t
017304130509     d dataiso_cor     s               d   datfmt(*iso)
017305130509     d dataeur_cor     s               d   datfmt(*eur)
017306130513     d dataiso         s               d   datfmt(*iso)
017307130513     d dataeur         s               d   datfmt(*eur)
017308130513     d wdtn            s                   like(tltdde)
017309130513     d gg              s              2  0
017310130510     d wintal          ds
017311130510     d wintdtal                            like(lst8al)
017312130510     d winthtal                            like(§1xhho)
017313130510     d wintdal         ds
017314130510     d wintdtdal                           like(lst8al)
017315130510     d winthtdal                           like(§1xhho)
022200110513     d
022300110513     d wrkgetlista     s           4096    varying
022301130509     d $Finerec        s              1    inz(*off)
022302130510
022303130510     d schfv           ds           897
022304130510     d fflp                         447
022306130510     d fflpW                        450
022307130510     d flp                            3    dim(299) overlay(schfv)
022308130513     D GFV             S              1    DIM(20) CTDATA PERRCD(20)
022400020805     D*
029400020805      *
029500020805      * STRUTTURE DATI DEFINITE ESTERNAMENTE
031900130509      *
031901130510     d Tibs34ds      e ds                  inz
031902130510     d §azute        e ds                  Extname(Azute00f)
031903130510     d                                     dtaara
031904130510     d §Datiute      e ds                  EXTname(ddatiute)
031905130510     d                                     dtaara
031906130509     d fnfgvds       e ds                  extname(fnfgv00f) inz
031909130509     d fnfgWds       e ds                  extname(fnfgw00f) inz
031912130509     d fnfv2ds       e ds                  extname(fnfv200f) inz
031913130510     d ds1x          e ds
031914130513     d dstu          e ds
031915130510     d kpjba         e ds
031916130509      // - Aggiunge/Toglie gg/mm dalla data
031917130509     d XGIOLAVds     e ds                  inz
032000130509     D fnlst8ds      E DS
033600010927      *
036901130509
036903130509      // - Aggiunge/Toglie gg/mm dalla data
036904130509     d Xgiolav         pr                  extpgm('XGIOLAV')
036905130509     d  xgiolavds                          likeds(xgiolavds)
036906130510      // ? PROTOTIPI ?
036907130510      /copy gaitrasrc/srcprotopr,tibs34r
037000020805      *
053700951013     C*---------------------------------------------------------------*
053701130509     c     *Entry        plist
053702130509     c                   parm                    KPJBA
053703130509     c
053704130509     c                   movel     kpjbu         fnlst8ds
053705130509      *
053706130509      /free
053900130509         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
053903130509
053904130510       exsr sr_opeiniz;
053935130509       // elaborazione e stampa dei fogli viaggio abbinati a traino
053936130509       exsr sr_elaboraStd ;
053937130509
053938130509       // elaborazione e stampa dei fogli non     abbinati a traino
053939130509       exsr sr_elaboraBis ;
053940130513
053941130513       if *in98;
053942130513          write testata;
053943130513       endif;
053944130513       write fines;
053945130509
054000130509       eval *inlr=*on;
054100130509       //-------------------------------------------------------------*
054200130509       //Routine Iniziale                                             *
054300130509       //-------------------------------------------------------------*
054400130509       begsr *inzsr;
057500130509
057501130510       exsr datijob;
057502130510
057600130509       // Reperisco la data corrente
057700130509       dataiso_cor=(%date());
057800130509       dataeur_cor=dataiso_cor;
057900130513       wdate=%dec(dataeur_cor);
058000130513       utime= %dec(%time());
058001130509
058002130509       // Leggo tabella "1X" per recuperare l'ora di soglia trazioni
058003130509       clear ds1x;
058004130510       wkey=%char(dutpou)+'OST '     ;
058005130509       chain (1:'1X': wkey) tabel00f  ;
058008130509  1    if not %found(tabel00f)  ;
058009130509          chain (1:'1X': '   OST ') tabel00f  ;
058016130509  1    endif     ;
058017130509       ds1x=tbluni             ;
058100130509
058200130509       EndSr;
058201130510       //-------------------------------------------------------------*
058202130510       //Reperimento Dati del job (Utente/Operativi)                  *
058203130510       //-------------------------------------------------------------*
058204130510       Begsr DatiJob;
058205130510
058206130510          in(e) §azute;
058207130510          if not %error;
058208130510           in(e) §datiute;
058209130510          endif;
058210130510          if %error or rsut = *blanks;
058211130510           tibs34r(tibs34ds);
058212130510           in §azute;
058213130510           in §datiute;
058214130510          endif;
058217130510         endsr;
058218130510       //-------------------------------------------------------------*
058219130510       //operazioni iniziali                                          *
058220130510       //-------------------------------------------------------------*
058221130510       Begsr SR_opeiniz;
058222130510       // INTERVALLO DI ARRIVO PER STABILIRE IL GIORNO DI ARRIVO DI COMPE-
058223130510       // TENZA DEI FOGLI VIAGGIO IN BASE ALLA DATA/ORA TEORICA DI ARRIVO
058224130510       // Imposto l'intervallo di arrivo come segue:
058225130510       // data al dell'intervallo = data arrivo ricevuta
058226130510       // ora  al dell'intervallo = ora  di soglia presa da tabella
058227130510       clear wintdtal;
058228130510       clear winthtal;
058229130510       wintdtal=lst8al;
058230130510       winthtal=§1xhho;
058231130510
058232130510       // data dal dell'intervallo = data arrivo ricevuta - 1 giorno lavorativo
058233130510       // ora  dal dell'intervallo = ora  di soglia + 1 minuto
058234130510       clear wintdtdal;
058235130510       clear winthtdal;
058236130510         clear XGIOLAVDS;
058237130510         IXGLdata = lst8al;
058238130510         IXGLsub  = 'S';
058239130510         IXGLgg   = 1;
058240130510         IXGLlav  = 'S';
058241130510         IXGLpa   = 'A';
058242130510         IXGLfil  = dutpou;
058243130510         XGIOLAV (xgiolavds);
058244130510         IF  OXGLerr = *blanks;
058245130510           wintdtdal= OXGLdata;
058246130510         ELSE;
058247130510           wintdtdal= wintdtdal    ;
058248130510         ENDIF;
058249130510         time=%time((winthtal*100):*hms)+%minutes(1) ;
058250130510         w0060=%dec(time)      ;
058251130510         winthtdal=%dec(%subst(%editc(w0060:'X'):1:4):4:0);
058252130513
058253130513       // Stampo intestazione
058254130513       pter=dutpou;
058255130513       clear pterd;
058256130513       chain (dutpou) azorg01l;
058257130513       if %found(azorg01l);
058258130513          pterd=orgdes ;
058259130513       endif;
058260130513       dataeur=%date(lst8al:*iso);
058261130513       pdatarr=%dec(dataeur);
058262130513       write testata;
058263130513       *in98=*off;
058264130510         endsr;
058265130510
058300130509       //-------------------------------------------------------------*
058400130509       //elaborazione fogli viaggio abbinati a traino                 *
058500130509       //-------------------------------------------------------------*
058600130509       begsr sr_elaboraStd;
058601130509       exsr sr_PrepSqlStd;
058602130509       // elaboro dati estratti
058603130509       exsr sr_SqlStd;
058700130509       endsr;
058800130509       //-------------------------------------------------------------*
058900130509       //elaborazione fogli viaggio non abbinati a  traino            *
059000130509       //-------------------------------------------------------------*
059100130509       begsr sr_elaboraBis;
059200130509       endsr;
059300130509       //-------------------------------------------------------------*
059400130509       //Prepara stringa sql per fogli standard                       *
059500130509       //-------------------------------------------------------------*
059600130509       begsr sr_PrepSqlStd;
059603130510       WrkStringaSql='with sel001 as ( select fnfgv00f.*, ifnull(fgwfl3, +
059604130510        '' ''), ifnull(fgwfl4,'' '') from fnfgv00f left +
059605130509        outer join fnfgw00f on fgvlnp=fgwlnp and  fgvnfv=fgwnfv +
059606130510        where (fgvlna=' + %editc(dutpou:'X') + ' or fgvflp like' + ''''  + '%' +
059607130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgvfl2 like' + '''' + '%' +
059608130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl3 like' + '''' + '%' +
059609130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl4 like' + '''' + '%' +
059610130510        %editc(dutpou:'X')  + '%' + '''' + ') and fgvatb=' + '''' + ' ' + '''' +
059611130513        ' and fgvttr<>' + '''' + 'L' + '''' + ') +
059612130509         select * from sel001, fnfv200f where fgvlnp=fv2lnp and +
059613130510        fgvnfv=fv2nfv and fv2epa=' + '''' + 'A' + '''' + ' and fv2tdh=' +
059614130510        '''' + 'T' + '''' + ' and (fv2dpa*10000)+fv2hpa between ' +
059615130510        wintdal + ' and ' + wintal +
059616130510        ' order by fv2dpa, fv2hpa';
059626130509
059700130509       endsr;
059800130509       //-------------------------------------------------------------*
059900130509       //Legge e stampa fogli standard                                *
060000130509       //-------------------------------------------------------------*
060100130509       begsr sr_SqlStd;
060101130509        $finerec=*off;
060102130509        exec sql prepare STRINGASQL from :WrkStringaSql;
060103130509        exec sql declare FGVCsr cursor for StringaSql;
060104130509        exec sql open FGVcsr;
060105130509        dow $finerec=*off;
060106130510           exec sql Fetch FGVcsr into :fnfgvds, :fgwfl3, :fgwfl4, :fnfv2ds;
060107130509           if sqlcod=100 or sqlcod<0;
060108130509              $finerec = *on;
060109130509              leave;
060110130509           endif;
060111130509        // Scarto il foglio viaggio se il terminal non è presente
060112130510        eval w003a= %editc(dutpou:'X');
060113130510        eval fflp=fgvflp+fgvfl2;
060114130510        eval fflpw=fgWfl3+fgWfl4;
060115130510        if %lookup(w003a:flp)=0 and dutpou<>fgvlna;
060116130510           iter;
060117130510        endif;
060120130509        // Stampo il foglio viaggio
060121130513        if *in98;
060122130513           write testata;
060123130513           *in98=*off;
060124130513        endif;
060125130513        dataeur=%date(fgvdfv:*iso);
060126130513        pdfv=%dec(dataeur);
060127130513        ptrn=fgvtrn;
060128130513        // cerco i dati del traino
060129130513        exsr caldtn;
060130130513        exsr redtlt;
060131130513
060132130513        // Automezzo
060133130513        clear pautom;
060134130513        tblkey=fgvtmz;
060135130513        chain (1: 'TU': tblkey) tabel00f;
060136130513        if %found(tabel00f);
060137130513           dstu=tbluni;
060138130513           pautom=§tudes;
060139130513        endif;
060140130513        // Trazionista
060141130513           ppdr=%editc(fgvpdr:'X')+' '+ fgvdpd;
060142130513        // Conducente
060143130513           pcond=fgvpmb;
060144130513        // Piombi
060145130513        clear ppmb;
060146130513           setll (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
060147130513           reade (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
060148130513              if not %eof(fnfv401l) and fv4atb=*blanks;
060149130513                 ppmb=fv4not ;
060150130513                 reade (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
060151130513                 if not %eof(fnfv401l) and fv4atb=*blanks;
060152130513                     %subst(ppmb:36)=fv4not;
060153130513                 endif;
060154130513              endif;
060155130513        // Data/ora teorica di arrivo
060156130513        clear pdtarr;
060157130513        clear phtarr;
060158130513        dataiso=%date(fv2dpa);
060160130513        pdtarr=%dec(dataiso:*eur);
060161130513        phtarr=fv2hpa;
060162130513        write riga;
060163130509        enddo;
060200130509       endsr;
060300130513       //-------------------------------------------------------------*
060400130513       //Calcolo data traino per lettura testata traino               *
060500130513       //-------------------------------------------------------------*
060600130513       begsr caldtn;
060601130513       clear wdtn;
060602130513       SETLL (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
060603130513       reade (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
060604130513       dow not %eof(tntlr02l);
060605130513          if tlratb=' ' and tlrcar='S';
060606130513             if tlrgfv='A';
060607130513                wdtn=fgvdfv;
060608130513             else ;
060610130513                gg= %lookup(tlrgfv:gfv);
060611130513                dataiso=%date(fgvdfv)+ %days(gg);
060612130513                wdtn=%dec(dataiso);
060624130513             endif;
060625130513             leavesr;
060626130513          endif;
060627130513          reade (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
060628130513       enddo;
060700130513       endsr;
060701130513       //-------------------------------------------------------------*
060702130513       //Lettura testata traino per recupero dati del traino          *
060703130513       //-------------------------------------------------------------*
060704130513       begsr redtlt;
060705130513       clear plnp;
060706130513       clear plna;
060707130513       clear psep;
060708130513       SETLL (TLRTRN) tntlt01l;
060709130513       reade (tlrtrn) tntlt01l;
060710130513       dow not %eof(tntlt01l);
060711130513          if tltatb=*blanks and wdtn>=tltdde and wdtn<=tltdsc;
060712130513             plnp=tlttfp;
060713130513             psep='/';
060714130513             plna=tlttfa;
060715130513             leavesr;
060716130513          endif;
060717130513          reade (tlrtrn) tntlt01l;
060718130513       enddo;
060719130513       endsr;
060800130513** SCHIERA GFV
060900130513BCDEFGHILMNOPQRSTUVZ
