000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200130509      * FNlST8R *----------------------------------------------------*
000300130517      *      - Elenco trazioni in arrivo                             *
000400940915      *--------------------------------------------------------------*
000500900517      * INDICATORI USATI
000600951025      *--------------------------------------------------------------*
000700900517      *--------------------------------------------------------------*
000800020805      *
000900020805      * DEFINIZIONE SCHIERE
001000020805      *
001100020213
001200130510     ftabel00f  if   e           k disk
001300130513     fazorg01l  if   e           k disk
001400130516     ffnfgs01l  if   e           k disk
001500130515     f*tntlt01l  if   e           k disk
001600130515     f*tntlr02l  if   e           k disk
001700130513     ffnfv401l  if   e           k disk
001800130513     fazcpc03l  if   e           k disk
001900130515     fFNLST8P   o    e             printer
002000020805      *
002100020805      * DEFINIZIONE CAMPI
002200020805      *
002300130509     D WrkStringaSql   S           4500
002400130509     D                                     VARYING
002500130513     D WrkStringaSql1  S           4500
002600130513     D                                     VARYING
002700130509     D w0060           S              6  0
002800130510     D w003a           S              3
002900130516     D w0080           S              8  0
003000130509     D wkey            S                   like(tblkey)
003100130509     d time            s               t
003200130509     d dataiso_cor     s               d   datfmt(*iso)
003300130509     d dataeur_cor     s               d   datfmt(*eur)
003400130513     d dataiso         s               d   datfmt(*iso)
003500130513     d dataeur         s               d   datfmt(*eur)
003600130515     d*wdtn            s                   like(tltdde)
003700130513     d gg              s              2  0
003800130514     d p96dri          s              8  0
003900130514     d p96ver          s              5  0
004000130515     d $righe          s              2  0
004100130516     d*wintal          ds
004200130516     d*wintdtal                            like(lst8al)
004300130516     d*winthtal                            like(�1xhho)
004400130516     d*wintdal         ds
004500130516     d*wintdtdal                           like(lst8al)
004600130516     d*winthtdal                           like(�1xhho)
004700110513     d
004800110513     d wrkgetlista     s           4096    varying
004900130509     d $Finerec        s              1    inz(*off)
005000130510
005100130510     d schfv           ds           897
005200130510     d fflp                         447
005300130510     d fflpW                        450
005400130510     d flp                            3    dim(299) overlay(schfv)
005500130513     D GFV             S              1    DIM(20) CTDATA PERRCD(20)
005600020805     D*
005700020805      *
005800020805      * STRUTTURE DATI DEFINITE ESTERNAMENTE
005900130509      *
006000130510     d Tibs34ds      e ds                  inz
006100130510     d �azute        e ds                  Extname(Azute00f)
006200130510     d                                     dtaara
006300130510     d �Datiute      e ds                  EXTname(ddatiute)
006400130510     d                                     dtaara
006500130509     d fnfgvds       e ds                  extname(fnfgv00f) inz
006600130509     d fnfgWds       e ds                  extname(fnfgw00f) inz
006700130509     d fnfv2ds       e ds                  extname(fnfv200f) inz
006800130516     d*ds1x          e ds
006900130513     d dstu          e ds
007000130510     d kpjba         e ds
007100130509      // - Aggiunge/Toglie gg/mm dalla data
007200130516     d*XGIOLAVds     e ds                  inz
007300130509     D fnlst8ds      E DS
007301130516     d wintdal                 1     12
007302130516     d wintal                 13     24
007400130513     D tnsd99ds      E DS
007500010927      *
007600130509
007700130509      // - Aggiunge/Toglie gg/mm dalla data
007800130516     d*Xgiolav         pr                  extpgm('XGIOLAV')
007900130516     d* xgiolavds                          likeds(xgiolavds)
008000130510      // ? PROTOTIPI ?
008100130510      /copy gaitrasrc/srcprotopr,tibs34r
008200130513      /copy gaitrasrc/srcProtoPr,TISI96R
008300130513      /copy gaitrasrc/srcProtoPr,TNSD99R
008400020805      *
008500951013     C*---------------------------------------------------------------*
008600130509     c     *Entry        plist
008700130509     c                   parm                    KPJBA
008800130509     c
008900130509     c                   movel     kpjbu         fnlst8ds
009000130509      *
009100130509      /free
009200130509         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009300130509
009400130510       exsr sr_opeiniz;
009500130509       // elaborazione e stampa dei fogli viaggio abbinati a traino
009600130509       exsr sr_elaboraStd ;
009700130509
009800130509       // elaborazione e stampa dei fogli non     abbinati a traino
009900130509       exsr sr_elaboraBis ;
010000130513
010100130513       write fines;
010200130509
010300130509       eval *inlr=*on;
010400130509       //-------------------------------------------------------------*
010500130509       //Routine Iniziale                                             *
010600130509       //-------------------------------------------------------------*
010700130509       begsr *inzsr;
010800130509
010900130510       exsr datijob;
011000130510
011100130509       // Reperisco la data corrente
011200130509       dataiso_cor=(%date());
011300130509       dataeur_cor=dataiso_cor;
011400130517       wdate=%dec(%subst(%editc(%dec(dataeur_cor):'X'):1:4):4:0);
011500130517       utime=%dec(%subst(%editc(%dec(%time()):'X'):1:4):4:0);
011600130509
011700130509       // Leggo tabella "1X" per recuperare l'ora di soglia trazioni
011800130516       //clear ds1x;
011900130517       //wkey=%editc(dutpou:'X')+'OST '     ;
012000130516       //chain (1:'1X': wkey) tabel00f  ;
012100130517  1    //if not %found(tabel00f) or tblflg<>' ' ;
012200130516       //   chain (1:'1X': '   OST ') tabel00f  ;
012300130516  1    //endif     ;
012400130516       //ds1x=tbluni             ;
012500130509
012600130509       EndSr;
012700130510       //-------------------------------------------------------------*
012800130510       //Reperimento Dati del job (Utente/Operativi)                  *
012900130510       //-------------------------------------------------------------*
013000130510       Begsr DatiJob;
013100130510
013200130510          in(e) �azute;
013300130510          if not %error;
013400130510           in(e) �datiute;
013500130510          endif;
013600130510          if %error or rsut = *blanks;
013700130510           tibs34r(tibs34ds);
013800130510           in �azute;
013900130510           in �datiute;
014000130510          endif;
014100130510         endsr;
014200130510       //-------------------------------------------------------------*
014300130510       //operazioni iniziali                                          *
014400130510       //-------------------------------------------------------------*
014500130510       Begsr SR_opeiniz;
014600130510       // INTERVALLO DI ARRIVO PER STABILIRE IL GIORNO DI ARRIVO DI COMPE-
014700130510       // TENZA DEI FOGLI VIAGGIO IN BASE ALLA DATA/ORA TEORICA DI ARRIVO
014800130510       // Imposto l'intervallo di arrivo come segue:
014900130510       // data al dell'intervallo = data arrivo ricevuta
015000130510       // ora  al dell'intervallo = ora  di soglia presa da tabella
015100130516       //clear wintdtal;
015200130516       //clear winthtal;
015300130516       //wintdtal=lst8al;
015400130516       //winthtal=�1xhho;
015500130510
015600130510       // data dal dell'intervallo = data arrivo ricevuta - 1 giorno lavorativo
015700130510       // ora  dal dell'intervallo = ora  di soglia + 1 minuto
015800130516       //clear wintdtdal;
015900130516       //clear winthtdal;
016000130516       //  clear XGIOLAVDS;
016100130516       //  IXGLdata = lst8al;
016200130516       //  IXGLsub  = 'S';
016300130516       //  IXGLgg   = 1;
016400130516       //  IXGLlav  = 'S';
016500130516       //  IXGLpa   = 'A';
016600130516       //  IXGLfil  = dutpou;
016700130516       //  XGIOLAV (xgiolavds);
016800130516       //  IF  OXGLerr = *blanks;
016900130516       //    wintdtdal= OXGLdata;
017000130516       //  ELSE;
017100130516       //    wintdtdal= wintdtdal    ;
017200130516       //  ENDIF;
017300130516       //  time=%time((winthtal*100):*hms)+%minutes(1) ;
017400130516       //  w0060=%dec(time)      ;
017500130516       //  winthtdal=%dec(%subst(%editc(w0060:'X'):1:4):4:0);
017600130513
017700130513       // Stampo intestazione
017800130513       pter=dutpou;
017900130513       clear pterd;
018000130513       chain (dutpou) azorg01l;
018100130513       if %found(azorg01l);
018200130513          pterd=orgdes ;
018300130513       endif;
018400130517       dataeur=%date(lst8dal:*iso);
018500130513       pdatarr=%dec(dataeur);
018501130517       dataeur=%date(lst8al:*iso);
018502130517       pdatara=%dec(dataeur);
018600130513       write testata;
018700130517       $righe=5;
018800130510         endsr;
018900130510
019000130509       //-------------------------------------------------------------*
019100130509       //elaborazione fogli viaggio abbinati a traino                 *
019200130509       //-------------------------------------------------------------*
019300130509       begsr sr_elaboraStd;
019400130509       exsr sr_PrepSqlStd;
019500130509       // elaboro dati estratti
019600130509       exsr sr_SqlStd;
019700130509       endsr;
019800130509       //-------------------------------------------------------------*
019900130509       //elaborazione fogli viaggio non abbinati a  traino            *
020000130509       //-------------------------------------------------------------*
020100130509       begsr sr_elaboraBis;
020200130513       exsr sr_PrepSqlBis;
020300130513       // elaboro dati estratti
020400130513       exsr sr_SqlBis;
020500130509       endsr;
020600130509       //-------------------------------------------------------------*
020700130509       //Prepara stringa sql per fogli standard                       *
020800130509       //-------------------------------------------------------------*
020900130509       begsr sr_PrepSqlStd;
021000130510       WrkStringaSql='with sel001 as ( select fnfgv00f.*, ifnull(fgwfl3, +
021100130510        '' ''), ifnull(fgwfl4,'' '') from fnfgv00f left +
021200130509        outer join fnfgw00f on fgvlnp=fgwlnp and  fgvnfv=fgwnfv +
021300130510        where (fgvlna=' + %editc(dutpou:'X') + ' or fgvflp like' + ''''  + '%' +
021400130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgvfl2 like' + '''' + '%' +
021500130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl3 like' + '''' + '%' +
021600130510        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl4 like' + '''' + '%' +
021700130510        %editc(dutpou:'X')  + '%' + '''' + ') and fgvatb=' + '''' + ' ' + '''' +
021800130514        ' and fgvttr not in(' + '''' + 'L' + '''' + ' ,' + '''' + 'D' + '''' +
021900130514        ' ,' + '''' + 'H' + '''' + ')) +
022000130509         select * from sel001, fnfv200f where fgvlnp=fv2lnp and +
022100130514        fgvnfv=fv2nfv and fv2lai=' + %editc(dutpou:'X') +
022200130514        ' and fv2epa=' + '''' + 'A' + '''' + ' and fv2tdh=' +
022300130510        '''' + 'T' + '''' + ' and (fv2dpa*10000)+fv2hpa between ' +
022400130510        wintdal + ' and ' + wintal +
022500130515        ' order by fv2dpa, fv2hpa, fgvtrn';
022600130509
022700130509       endsr;
022800130509       //-------------------------------------------------------------*
022900130509       //Legge e stampa fogli standard                                *
023000130509       //-------------------------------------------------------------*
023100130509       begsr sr_SqlStd;
023200130509        $finerec=*off;
023300130509        exec sql prepare STRINGASQL from :WrkStringaSql;
023400130509        exec sql declare FGVCsr cursor for StringaSql;
023500130509        exec sql open FGVcsr;
023600130509        dow $finerec=*off;
023700130510           exec sql Fetch FGVcsr into :fnfgvds, :fgwfl3, :fgwfl4, :fnfv2ds;
023800130509           if sqlcod=100 or sqlcod<0;
023900130509              $finerec = *on;
024000130509              leave;
024100130509           endif;
024200130509        // Scarto il foglio viaggio se il terminal non � presente
024300130510        eval w003a= %editc(dutpou:'X');
024400130510        eval fflp=fgvflp+fgvfl2;
024500130510        eval fflpw=fgWfl3+fgWfl4;
024600130510        if %lookup(w003a:flp)=0 and dutpou<>fgvlna;
024700130510           iter;
024800130510        endif;
024900130515        *in40 = *on;
025000130513        exsr stampa;
025100130509        enddo;
025200130509       endsr;
025300130513       //-------------------------------------------------------------*
025400130513       //Prepara stringa sql per fogli Bis                            *
025500130513       //-------------------------------------------------------------*
025600130513       begsr sr_PrepSqlBis;
025700130513       WrkStringaSql1='select fnfgv00f.*, ifnull(fgwfl3, +
025800130513        '' ''), ifnull(fgwfl4,'' '') from fnfgv00f left +
025900130513        outer join fnfgw00f on fgvlnp=fgwlnp and  fgvnfv=fgwnfv +
026000130513        where (fgvlna=' + %editc(dutpou:'X') + ' or fgvflp like' + ''''  + '%' +
026100130513        %editc(dutpou:'X')  + '%' + '''' +  ' or fgvfl2 like' + '''' + '%' +
026200130513        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl3 like' + '''' + '%' +
026300130513        %editc(dutpou:'X')  + '%' + '''' +  ' or fgWfl4 like' + '''' + '%' +
026400130513        %editc(dutpou:'X')  + '%' + '''' + ') and fgvatb=' + '''' + ' ' + '''' +
026500130514        ' and fgvttr not in (' +
026600130514        '''' + 'L' + '''' + ' ,' +
026700130514        '''' + 'D' + '''' + ' ,' +
026800130514        '''' + 'H' + '''' + ') and fgvtrn=0 order by fgvpdr, +
026900130513         fgvpmb';
027000130513
027100130513       endsr;
027200130513       //-------------------------------------------------------------*
027300130513       //Legge e stampa fogli Bis                                     *
027400130513       //-------------------------------------------------------------*
027500130513       begsr sr_SqlBis;
027600130513        $finerec=*off;
027700130514        exec sql prepare STRINGASQL1 from :WrkStringaSql1;
027800130513        exec sql declare FGVCsr1 cursor for StringaSql1;
027900130513        exec sql open FGVcsr1;
028000130513        dow $finerec=*off;
028100130513           exec sql Fetch FGVcsr1 into :fnfgvds, :fgwfl3, :fgwfl4;
028200130513           if sqlcod=100 or sqlcod<0;
028300130513              $finerec = *on;
028400130513              leave;
028500130513           endif;
028600130513        // Scarto il foglio viaggio se il terminal non � presente
028700130513           eval w003a= %editc(dutpou:'X');
028800130513           eval fflp=fgvflp+fgvfl2;
028900130513           eval fflpw=fgWfl3+fgWfl4;
029000130513           if %lookup(w003a:flp)=0 and dutpou<>fgvlna;
029100130513              iter;
029200130513           endif;
029300130513        // Determino data/ora prevista trazione
029400130513        // e scarto il foglio se la data/ora calcolata non rientra nell'inter-
029500130513        // vallo di arrivo calcolato
029600130514           p96dri=fgvdfv;
029700130514           clear p96ver;
029800130514           GetVersioneCappario(p96dri:p96ver);
029900130513           clear tnsd99ds;
030000130513           d98tla='T';
030100130513           d98lnp=fgvlnp;
030200130513           d98tfp=fgvlnp;
030300130513           d98lna=dutpou;
030400130513           d98dsp=fgvdfv;
030500130513           d98tsp='E';
030600130514           setll (p96ver: '   ': dutpou) azcpc03l;
030700130514           reade (p96ver: '   ': dutpou) azcpc03l;
030800130513           dow not %eof(azcpc03l);
030900130513              if cpcatb <> ' ' or cpcfit='S';
031000130513                 clear cpccap;
031100130514                 reade (p96ver: '   ': dutpou) azcpc03l;
031200130513              else;
031300130513                leave;
031400130513              endif;
031500130513           enddo;
031600130513           if cpccap = *blanks;
031700130513              chain (dutpou) azorg01l;
031800130514              d98cad=%editc(orgcpf:'X');
031900130513           else;
032000130514              d98cad=cpccap;
032100130513           endif;
032200130513           d98tfa=dutpou;
032300130513           tnsd99r(tnsd99ds);
032400130514           if ((d98dti*10000)+d98hti)<%dec(wintdal:12:0) or
032500130514              ((d98dti*10000)+d98hti)>%dec(wintal:12:0);
032600130513               iter;
032700130513           endif;
032800130515           *in40=*off;
032900130513           exsr stampa;
033000130513        enddo;
033100130513       endsr;
033200130513       //-------------------------------------------------------------*
033300130513       //Calcolo data traino per lettura testata traino               *
033400130513       //-------------------------------------------------------------*
033500130515       //begsr caldtn;
033600130515       //clear wdtn;
033700130515       //SETLL (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
033800130515       //reade (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
033900130515       //dow not %eof(tntlr02l);
034000130515       //   if tlratb=' ' and tlrcar='S';
034100130515       //      if tlrgfv='A';
034200130515       //         wdtn=fgvdfv;
034300130515       //      else ;
034400130515       //         gg= %lookup(tlrgfv:gfv);
034500130515       //         dataiso=%date(fgvdfv)+ %days(gg);
034600130515       //         wdtn=%dec(dataiso);
034700130515       //      endif;
034800130515       //      leavesr;
034900130515       //   endif;
035000130515       //   reade (fgvtrn: fgvfrq: fgvlnp) tntlr02l;
035100130515       //enddo;
035200130515       //endsr;
035300130513       //-------------------------------------------------------------*
035400130513       //Lettura testata traino per recupero dati del traino          *
035500130513       //-------------------------------------------------------------*
035600130515       //begsr redtlt;
035700130515       //SETLL (TLRTRN) tntlt01l;
035800130515       //reade (tlrtrn) tntlt01l;
035900130515       //dow not %eof(tntlt01l);
036000130515       //   if tltatb=*blanks and wdtn>=tltdde and wdtn<=tltdsc;
036100130515       //      plnp=%editc(fgvlnp:'X');
036200130515       //      psep='/';
036300130515       //      plna=%editc(fgvlna:'X');
036400130515       //      leavesr;
036500130515       //   endif;
036600130515       //   reade (tlrtrn) tntlt01l;
036700130515       //enddo;
036800130515       //endsr;
036900130513       //-------------------------------------------------------------*
037000130513       //Stampa riga foglio viaggio                                   *
037100130513       //-------------------------------------------------------------*
037200130513       begsr stampa;
037300130513        // Stampo il foglio viaggio
037400130513        dataeur=%date(fgvdfv:*iso);
037500130516        w0080=%dec(dataeur);
037600130516        pdfv=%int(%subst(%editc(w0080:'X'):1:4));
037700130516
037800130517        //clear plnplna;
037900130517        //plnplna=%editc(fgvlna:'X');
038000130517        //chain (fgvlna) azorg01l;
038100130517        //if %found(azorg01l);
038200130517        //   plnplna=%trim(plnplna) + '-' + %subst(orgdes:1:16) ;
038300130517        //endif;
038400130516
038500130516        clear pdeslnp;
038600130516        chain (fgvlnp) azorg01l;
038700130516        if %found(azorg01l);
038800130516           pdeslnp=orgdes ;
038900130516        endif;
039000130516
039100130517        clear ptrn;
039101130517        *in41=*off;
039200130515        if fgvsnp='B';
039300130517           ptrn='    Bis';
039301130517           *in41=*on;
039302130517
039303130517        else;
039500130516
039600130516        clear ptrn;
039700130516        // numero traino: se foglio sbinato cerco il numero traino di sede
039800130516        // nelle note
039900130516          if fgvtrn > 0 ;
040000130517             ptrn=%editc(fgvtrn:'Z');
040100130516          else;
040200130516             chain (fgvlnp: fgvnfv: 'P': 000: 'D') fnfv401l;
040300130516             if %found(fnfv401l);
040400130517                ptrn=%editc(%dec(%subst(fv4not:1:7):7:0):'Z');
040500130516             endif;
040600130516          endif;
040601130517        endif;
040700130516
040800130513        // cerco i dati del traino
040900130515       //clear plnp;
041000130515       //clear plna;
041100130515       //clear psep;
041200130515       // if fgvtrn>0;
041300130515       //    exsr caldtn;
041400130515       //    exsr redtlt;
041500130515       // endif;
041600130513
041700130513        // Automezzo
041800130513        clear pautom;
041900130513        tblkey=fgvtmz;
042000130513        chain (1: 'TU': tblkey) tabel00f;
042100130513        if %found(tabel00f);
042200130513           dstu=tbluni;
042300130513           pautom=�tudes;
042400130513        endif;
042500130516
042600130513        // Trazionista
042700130516        // ppdr=%editc(fgvpdr:'X')+' '+ fgvdpd;
042800130516           ppdr=fgvdpd;
042900130516
043000130516        // Verifico se c'e' lo scambio. Se presente al posto
043100130516        //  del conducente scrivo SCAMBIO con TRAINO
043200130516
043300130516           chain (fgvnfv: fgvlnp)  fnfgs01l;
043400130516           if %found(fnfgs01l)  ;
043500130516           IF FGSTRNs>0  ;
043600130516           pcond='SCAMBIO con TRAINO :'+%EDITC(FGSTRNS:'Z') ;
043700130516           ELSE  ;
043800130516           chain fgslnar azorg01l  ;
043900130516           if not %found(azorg01l) ;
044000130516            clear orgdes  ;
044100130516           endif  ;
044200130516
044300130516           pcond='SCAMBIO con RITORNO:'+%EDITC(FGSlnar:'Z')+'-'+orgdes ;
044400130516           ENDIF ;
044500130516
044600130516           else  ;
044700130516
044800130516        // Conducente + cellulare
044900130516
045000130516           chain (fgvlnp: fgvnfv: 'P': 000: 'L') fnfv401l;
045100130516           if not %found(fnfv401l)  ;
045200130516            clear fv4not  ;
045300130516            endif  ;
045400130516
045500130516           pcond=%trim(fgvpmb) + '  ' +fv4not  ;
045600130516           endif  ;
045700130516
045800130513        // Piombi
045900130513        clear ppmb;
046000130513           setll (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
046100130513           reade (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
046200130513              if not %eof(fnfv401l) and fv4atb=*blanks;
046300130513                 ppmb=fv4not ;
046400130513                 reade (fgvlnp: fgvnfv: 'P': 000: 'P') fnfv401l;
046500130513                 if not %eof(fnfv401l) and fv4atb=*blanks;
046600130513                     %subst(ppmb:36)=fv4not;
046700130513                 endif;
046800130513              endif;
046900130513        // Data/ora teorica di arrivo
047000130513        clear pdtarr;
047100130513        clear phtarr;
047200130513        if fgvtrn>0;
047300130513           dataiso=%date(fv2dpa);
047400130517           pdtarr=%dec(%subst(%editc(%dec(dataiso:*eur):'X'):1:4):4:0);
047500130513           phtarr=fv2hpa;
047600130513        endif;
047700130517        if $righe+3>86;
047800130515           write testata;
047900130517           $righe=5;
048000130515        endif;
048100130513        write riga;
048200130515        $righe+=3;
048300130513        endsr;
048400130513** SCHIERA GFV
048500130513BCDEFGHILMNOPQRSTUVZ
