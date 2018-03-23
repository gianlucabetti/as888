000100110620     /*PRM dbgview(*source)
000200110620     /*CMD ovrdbf file(TITAS00F) tofile(GAITRAGRPS/TITAS00F) +
000300110620     /*CMD        ovrscope(*calllvl)
000400110620     /*CMD ovrdbf file(TITAS33C) tofile(GAITRAGRPS/TITAS33C) +
000500110620     /*CMD        ovrscope(*calllvl)
000600110620     /*CMD ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000700110620     /*CMD        ovrscope(*calllvl)
000800110620     /*END dltovr file(TITAS00F TITAS33C FIAR531C) lvl(*)
000900110620     /*END
001000040107      *---------------------------------------------------------------*
001100091007      * Analisi spedizioni clienti D/T/A/B                            *
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500090528     h dftactgrp(*no) actgrp(*caller)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001900090918     ftabel00f  if   e           k disk    prefix(t_)
002000090922     fazorg01l  if   e           k disk
002100090828     ffnlbl01l  if   e           k disk
002200090922     fwfmsp01l  uf A e           k disk
002300091005     fwfmsp02l  if   e           k disk    rename(wfmsp000:wfmsp2)
002400110228     ftitas33c  if   e           k Disk    extfile(wlibsede)usropn
002500110228     fFiar531c  if   e           k Disk    extfile(wlibsede) usropn
002600090922     ftist68p   o    e             printer usropn
002700040112      *
002800040107      *---------------------------------------------------------------*
002900040107
003000040107      *
003100040107      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
003200040107      *
003300040108     d K_tit1          c                   const('  IMMISSIONE  ')
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
003700090922     d rec             S              2    DIM(100) inz
003800100614     d Upr             S              8  0 DIM(4) inz
003900100614     d Upp             S              8  0 DIM(4) inz
004000100614     d Ukr             S             15  1 DIM(4) inz
004100100614     d Ukp             S             15  1 DIM(4) inz
004200100615     d
004300100615     d gUpr            S              8  0 DIM(4) inz
004400100615     d gUpp            S              8  0 DIM(4) inz
004500100615     d gUkr            S             15  1 DIM(4) inz
004600100615     d gUkp            S             15  1 DIM(4) inz
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
005000090922     d Tibs34ds      e ds                  inz
005100090922     d §azute        e ds                  Extname(Azute00f)
005200090922     d                                     dtaara
005300090922     d §Datiute      e ds                  EXTname(ddatiute)
005400090922     d                                     dtaara
005500090918     d dute01        e ds
005600090828     d dstb          e ds
005700090828     d ds01          e ds
005800090918     d ds17          e ds
005900090918     d ds05          e ds
006000090828     d Dar5Bnb       e Ds
006100090828
006200040107     d KPJBA         e ds
006300040108      * - Parametri
006400090827     d tist68ds      e ds
006500090827     d CLV                            1    DIM(4) overlay(st68clv)
006600090827
006700090827     d trul31ds      e ds
006800090827     d POG                            3  0 DIM(250) overlay(o31pog)
006900090828
007000090918     D WDAT8           DS
007100090918     D  DATADA                 1      8  0
007200090918     D  DATAA                  9     16  0
007300090918     D  GGL                   17     21  0
007400090918
007500090828     d tibs10ds      e ds
007600090916     d skc                           11    DIM(500) overlay(d10skc)
007700090828     d TIBS69DS      E DS                  INZ
007800090918     d DS_cnaco      E DS                  extname(CNACO00F) prefix(ds_)
007900090918     d DS_cnind      E DS                  extname(CNIND00F) prefix(ds_)
008000090918     d DS_cnclp      E DS                  extname(CNCLP00F) prefix(ds_)
008100090918     d DS_fncls      E DS                  extname(FNCLS00F) prefix(ds_)
008200040123      *
008300090827     d titasds       e ds                  extname(TITAS00F) inz
008400090828     d cnclpds       e ds                  extname(CNCLP00F) inz
008500090916     d cnacods       e ds                  extname(cnaco00f) inz
008600090916     d tabelds       e ds                  extname(tabel00f) inz
008700090929     d wfmspds       e ds                  extname(wfmsp00f) inz
008800090611     d                 ds
008900040109      *
009000040109     d Status         sds
009100040109     d  SDSpgm           *proc
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009500040107      *
009600090409     D WrkStringaSql   S           4500
009700090409     D                                     VARYING
009800090827     D WrkFilIn        S           4000
009900090827     D                                     VARYING
010000040213      *
010100090827     D WrkclvIn        S             50
010200090827     D                                     VARYING
010300090827     D WrkTblIn        S            200
010400090827     D                                     VARYING
010500090922     D WrkStringaSqlM  S           4500
010600090922     D                                     VARYING
010700040213      * - Indici di schiera / Contatori
010800090827     d i               s              3  0 inz
010900090922     d c_lnp           s              3  0 inz
011000040109      *
011100040109      * - Campi di comodo
011200090929     d wprimo          s              1
011300090923     d $righe          s              2  0
011400090924     d w0122           s             12  2
011500090924     d w0142           s             14  2
011600091006     d w0030           s              3  0
011700090828     d w_unif          s                   like(tasccm)
011800090921     d w_taslnp        s                   like(taslnp)
011900090828     d w_rscuni        s                   like(acorag)
012000090922     d w_cli           s                   like(acoksc)
012100090828     d w_rscage        s                   like(§01age)
012200090828     d sav_ccm         s                   like(tasccm) inz
012300090828     d w_collio        s                   like(§ar5bcor)
012400090917     d w_nspr          s                   like(mspspr)
012500090917     d w_nspe          s                   like(mspspr)
012600090917     d w_pesor         s                   like(msppkr)
012700090917     d w_pesoe         s                   like(msppkr)
012800090917     d w_volur         s                   like(mspvlr)
012900090917     d w_volue         s                   like(mspvlr)
013000090917     d w_collir        s                   like(mspncr)
013100090917     d w_collie        s                   like(mspncr)
013200090922     d savage          s                   like(mspage)
013300090929     d savage1         s                   like(mspage)
013400091005     d savrag1         s                   like(msprag)
013500090925     d pre_age         s                   like(mspage) inz
013600090925     d savdes          s                   like(mspdag)
013700090922     d savcli          s                   like(mspcli)
013800090929     d savcli1         s                   like(mspcli)
013900090925     d savrag          s                   like(msprag)
014000090928     d savclv          s                   like(mspclv)
014100090828     d w_tk            s                   like(taspkf)
014200090828     d w_tm            s                   like(tasvlf)
014300090922     d tpkr            s                   like(msppkr)
014400090922     d tpkp            s                   like(msppkp)
014500090928     d t_mspspr        s                   like(mspspr)
014600090928     d t_mspspp        s                   like(mspspr)
014700090928     d t_msppkp        s                   like(msppkr)
014800090928     d t_msppkr        s                   like(msppkr)
014900090928     d t_mspsmr        s                   like(mspsmr)
015000090928     d t_mspsmp        s                   like(mspsmp)
015100090928     d t_mspspc        s                   like(mspspc)
015200090928     d t_msppmr        s                   like(msppmr)
015300090928     d t_msppmp        s                   like(msppmp)
015400090928     d t_mspppc        s                   like(mspppc)
015500090918     d ggl_e           s                   like(ggl)
015600090918     d ggl_r           s                   like(ggl)
015700090917     d w_aasmin        s                   like(tasaas)
015800090917     d w_aasmax        s                   like(tasaas)
015900090917     d w_mgsmin        s                   like(tasmgs)
016000090917     d w_mgsmax        s                   like(tasmgs)
016100090917     d kccm            s                   like(tasccm)
016200090929     d sav_wfmspds     s                   like(wfmspds)
016300040108     d w0140           s             14  0
016400040123     d wDataAMG        s              8  0
016500040123     d wDataGMA        s              8  0
016600040120     d wOra            s              6  0
016700100614     d xx              s              4  0
016800101110     d Primo           s              4  0 inz
016900090414     d dataiso_cor     s               d   datfmt(*iso)
017000090414     d dataeur_cor     s               d   datfmt(*eur)
017100090416     d dataeur         s               d   datfmt(*eur)
017200090409     d datadmy         s               d   datfmt(*dmy)
017300090408     d kut             s                   like(acokut) inz(1)
017400090917     d wabi            s                   like(UteAut)
017500090918     d kfil            s                   like(orgfil)
017600090922     d Wtas33          s             21    inz('GAITRAGRU /TITAS33C')
017700090922     d War531          s             21    inz('GAITRAGRU /FIAR531C')
017800110228     D Wlibsede        S             21
017900110228     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
018000110228     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
018100110228     D WlibsedeP5      S             21    inz('GAITRAGRPS/FIAR531C')
018200110228     D WlibsedeB5      S             21    INZ('GAITRAGRU /FIAR531C')
018300090828      *
018400090828      * - Flags booleani
018500090828     d $Finerec        s              1    inz(*off)
018600081023
018700090402      // ? PROTOTIPI ?
018800090402      /copy gaitrasrc/srcprotopr,tibs34r
018900090827      /copy gaitrasrc/srcprotopr,trul31r
019000090828      /copy gaitrasrc/srcprotopr,tibs10r
019100090828      /copy gaitrasrc/srcprotopr,tibs69r
019200090918      /copy gaitrasrc/srcprotopr,xsrlav8
019300040107
019400040107      *===============================================================*
019500040107      *  RIEPILOGO INDICATORI                                         *
019600040107      *---------------------------------------------------------------*
019700090827      *  XX    -
019800040107      *===============================================================*
019900040107
020000040107     c     *Entry        plist
020100040107     c                   parm                    KPJBA
020200090827     c
020300090827     c                   movel     kpjbu         tist68ds
020400040123      *
020500090331      /free
020600090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
020700090610
020800090925       // Se richiesto dettaglio per lnp attivo indicatore per stampa
020900090925       if st68det='S';
021000090925          *in42=*on;
021100090925       endif;
021200090827       // determino numero giorni lavorativi fra i due periodi dal/al
021300090917       // ed imposto aas/mgs minimo e max da elaborare
021400090827       exsr sr_gglav;
021500090916       // preparo stringa sql per estrazione codici clienti da elaborare
021600090827       exsr sr_prepSql;
021700090827       // elaboro dati estratti
021800090827       exsr sr_Elabora;
021900090925       // Calcolo le medie e le %
022000090921       exsr sr_output;
022100090922
022200090922       // OPERAZIONI PER LA STAMPA
022300110620       Select;
022400110620
022500110620         // - Lancio da Filiale
022600110620         When  st68crt <> 'S';
022700110620           open tist68p;
022800110620           // Stampo intestazione
022900110620           exsr sr_impt00;
023000110620           write st68t00;
023100110620           write st68t01;
023200110620           $righe=10;
023300110620           clear c_lnp;
023400110620           // preparo stringa sql per estrazione clienti da stampare in base al limite
023500110620           // percentuale
023600110620           if st68sco<>0;
023700110620              exsr sr_prepSqlMsp;
023800110620              exsr sr_elamsp1;
023900110620           else;
024000110620              exsr sr_elamsp2;
024100110620           endif;
024200110620           // Stampo *** Fine Stampa ***
024300110620           if $righe+2>60;
024400110620              exsr sr_overflow;
024500110620           endif;
024600110620           write fines;
024700110620
024800110620         // - Lancio da Sede per le Poste
024900110620         When  ST68crt = 'S'  and  KCDAZ = 'ST6H';
025000110620           open  TIST68P;
025100110620           // - Stampa intestazione
025200110620           exsr  sr_ImpT00;
025300110620           write ST68T00;
025400110620           // - Stampa "Elaborazione Terminata"
025500110620           write ST68END;
025600110620
025700110620       EndSl;
025800090922
025900090827       eval *inlr=*on;
026000040108
026100090402       //-------------------------------------------------------------*
026200090402       //Routine Iniziale                                             *
026300090402       //-------------------------------------------------------------*
026400090414       begsr *inzsr;
026500090402
026600090402       //Reperisco dati job
026700090917         exsr datijob;
026800110228       if %subst(knsif:7:1) = 'P';
026900110228         Wlibsede = Wlibsedep;
027000110228         Else ;
027100110228         wlibsede = Wlibsedeb;
027200110228       endif;
027300110228       Open   Titas33c;
027400110228       if %subst(knsif:7:1) = 'P';
027500110228         Wlibsede = Wlibsedep5;
027600110228         Else ;
027700110228         wlibsede = Wlibsedeb5;
027800110228       endif;
027900110228       Open   fiar531c;
028000090827       //vtcpgm=SDSpgm;
028100090917       // Leggo tabella "TB" per caricare schiera dei codici bolla di recupero
028200090928       // conto servizio e assegnati
028300090917       i=0;
028400090917       setll (1:'TB') tabel00f  ;
028500090917       reade (1:'TB') tabel00f  ;
028600090917
028700090917  1    dow    not %eof(tabel00f)  ;
028800090918          dstb=t_tbluni           ;
028900090928          if §tbrbl='R' or §tbrbl='C' or §tbtpo='A';
029000090917             i=i+1;
029100090923             rec(i)=t_tblkey;
029200090917          endif;
029300090917          reade (1:'TB') tabel00f  ;
029400090917  1    enddo     ;
029500090917
029600090917       // Reperisco la data corrente
029700090917       dataiso_cor=(%date());
029800091005       dataeur_cor=dataiso_cor;
029900091005       wdate=%dec(dataeur_cor);
030000091005       utime= %dec(%time());
030100090402
030200090402       EndSr;
030300040108
030400090918       //-------------------------------------------------------------*
030500090918       //                                                             *
030600090918       //-------------------------------------------------------------*
030700090918       Begsr sr_gglav;
030800090918       // giorni lavorativi periodo da elaborare
030900090918       datada=st68dal;
031000090918       dataa=st68al;
031100090918       xsrlav8(wdat8);
031200090918       ggl_e=ggl;
031300091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
031400091201       if ggl_e=0;
031500091201          ggl_e=1;
031600091201       endif;
031700090918       // giorni lavorativi periodo di riferimento
031800090918       datada=st68rda;
031900090918       dataa=st68ral;
032000090918       xsrlav8(wdat8);
032100090918       ggl_r=ggl;
032200091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
032300091201       if ggl_r=0;
032400091201          ggl_r=1;
032500091201       endif;
032600090918       // imposto periodo minimo e max per lettura titas
032700090918       if st68dal<st68rda;
032800090918          w_aasmin=%dec(%subst(%editc(st68dal:'X'):1:4):4:0);
032900090922          w_mgsmin=%dec(%subst(%editc(ST68dal:'X'):5:4):4:0);
033000090918       else;
033100090918          w_aasmin=%dec(%subst(%editc(st68rda:'X'):1:4):4:0);
033200090922          w_mgsmin=%dec(%subst(%editc(st68rda:'X'):5:4):4:0);
033300090918       endif;
033400090922       if st68al>st68ral;
033500090918          w_aasmax=%dec(%subst(%editc(st68al:'X'):1:4):4:0);
033600090922          w_mgsmax=%dec(%subst(%editc(st68al:'X'):5:4):4:0);
033700090918       else;
033800090918          w_aasmax=%dec(%subst(%editc(st68ral:'X'):1:4):4:0);
033900090922          w_mgsmax=%dec(%subst(%editc(st68ral:'X'):5:4):4:0);
034000090918       endif;
034100090918       endsr;
034200090402       //-------------------------------------------------------------*
034300090402       //Reperimento Dati del job (Utente/Operativi)                  *
034400090402       //-------------------------------------------------------------*
034500090918       Begsr DatiJob;
034600090403
034700090917          in(e) §azute;
034800090917          if not %error;
034900090917           in(e) §datiute;
035000090917          endif;
035100090917          if %error or rsut = *blanks;
035200090917           tibs34r(tibs34ds);
035300090917           in §azute;
035400090917           in §datiute;
035500090917          endif;
035600090918          dute01=utefaf;
035700090918          wabi=§utegtc;
035800090917         endsr;
035900040108
036000090403       //-------------------------------------------------------------*
036100090916       //Preparazione stringa sql per Estrazione clienti da elaborare *
036200090403       //-------------------------------------------------------------*
036300090827       Begsr SR_prepSql;
036400090403
036500090916       WrkStringaSql='select * from cnaco00f, cnclp00f, tabel00f' +
036600090916                     ' where acokut=clpkut and acokcc=clpkcc and' +
036700090923                     ' acoksc=clpksc and clpage=tblkey and tblkut=1 and' +
036800090923                     ' tblcod=''01''';
036900090916       // richiesto un comm.le unificante specifico
037000090916       if st68age>0;
037100090916          WrkStringaSql=WrkStringaSql+
037200090929          ' and substr(tbluni, 26, 7)=' + '''' + %editc(st68age:'X')+ '''';
037300090916       else;
037400090916       // richiesta per area comm.le: preparo schiera delle filiali di appartenenza
037500090916          exsr sr_whrfil;
037600090916          WrkStringaSql=WrkStringaSql+WrkFilIn;
037700090916       endif;
037800090827
037900090827       // preparo schiera dei codici importanza clienti richiesti
038000090827       exsr sr_whrclv;
038100090827       WrkStringaSql=WrkStringaSql+WrkClvIn;
038200090403
038300090403       endsr;
038400090827       //-------------------------------------------------------------*
038500090916       //Schiera filiali di appartenenza all'area del comm.le unific. *
038600090827       //-------------------------------------------------------------*
038700090916       Begsr SR_whrfil;
038800101110          Primo= 1 ;
038900101110
039000090916          reset WrkFilIn;
039100090827          clear trul31ds  ;
039200090917          I31abi = wabi    ;
039300090917          I31cpo = DUTpou  ;
039400090827          trul31r(kpjba:trul31ds);
039500090922          WrkFilIn= ' And substr(tbluni, 26, 3) in (';
039600090922          for I=1 to %elem(pog);
039700090827             if pog(i)=0;
039800090827                leave;
039900090827             endif;
040000101110             if st68car>0   ;
040100101110              chain   pog(i) azorg01l  ;
040200101110             endif  ;
040300101110
040400101110             if st68car=0 or
040500101110               (%found(azorg01l) and   orgcar=st68car) ;
040600101110               if primo>1;
040700101110                 WrkFilIn=WrkFilIn + ' , ';
040800101110               endif;
040900101110             // se riicheista AREA, carico solo i p.o. dell'area richeista
041000090827             WrkFilIn=WrkFilIn + %char(pog(i));
041100101110             Primo=primo+1   ;
041200101110             endif;
041300090827          endfor;
041400090827          WrkFilIn=WrkFilIn + ')';
041500090827       endsr;
041600090827       //-------------------------------------------------------------*
041700090918       //Schiera codici importanza clienti da elaborare               *
041800090827       //-------------------------------------------------------------*
041900090827       Begsr SR_whrclv;
042000090929       wprimo='S';
042100090827       reset WrkClvIn;
042200090827       WrkclvIn= ' And clpclv in (';
042300090922          for I=1 to %elem(clv);
042400090929             if clv(i)<>*blanks;
042500090929                if wprimo<>'S';
042600090929                   WrkclvIn=WrkclvIn + ', ';
042700090929                endif;
042800090929                WrkCLVIn=WrkClvIn + '''' + clv(i) + '''';
042900090929                wprimo=' ';
043000090929             endif;
043100090827          endfor;
043200090929       WrkClvIn=WrkClvIn + ')';
043300090827       endsr;
043400090828       //-------------------------------------------------------------*
043500090828       //Lettura ed elaborazione dati estratti con sql                *
043600090828       //-------------------------------------------------------------*
043700090828       Begsr SR_Elabora;
043800090828        $finerec=*off;
043900090916        exec sql prepare STRINGASQL from :WrkStringaSql;
044000090916        exec sql declare ACOCsr cursor for StringaSql;
044100090916        exec sql open ACOcsr;
044200090828        dow $finerec=*off;
044300090916           exec sql Fetch ACOcsr into :cnacods, :cnclpds, :tabelds;
044400090828           if sqlcod=100 or sqlcod<0;
044500090828              $finerec = *on;
044600090828              leave;
044700090828           endif;
044800090916        // Scarto il cliente se non è padre o cliente singolo
044900090916           clear tibs10ds;
045000090916           d10tle='ST';
045100090916           d10paf='P';
045200090916           d10cod=acoksc;
045300090916           tibs10r(tibs10ds);
045400090916           if d10cop>0;
045500090916              iter;
045600090916           endif;
045700090916        // Recupero tutti i figli del cliente letto
045800090916           clear tibs10ds;
045900090916           d10tle='ST';
046000090916           d10paf='F';
046100090916           d10cod=acoksc;
046200090916           tibs10r(tibs10ds);
046300090916           if d10cop=0;
046400090930              skc(1)=%editc(d10cod:'X');
046500090916           endif;
046600090921        // Elaboro titas per ogni cliente caricato in schiera
046700090916           exsr sr_Etitas;
046800090916        enddo;
046900090916        endsr;
047000090916       //-------------------------------------------------------------*
047100090916       //Elaborazione Titas per cliente mittente                      *
047200090916       //-------------------------------------------------------------*
047300090916       Begsr Sr_Etitas;
047400090924          exsr sr_inz;
047500090924          clear w_taslnp;
047600090922       for I=1 to %elem(skc);
047700090923          if skc(i)=*blanks or skc(i)=*zeros;
047800090916             leave;
047900090916          endif;
048000090917            // routine di azzeramento campi di work
048100090916          kccm=%dec(%subst(skc(I):5:7):7:0);
048200090917          setll (kccm: w_aasmin: w_mgsmin) titas33c;
048300090916          reade (kccm) titas33c  ;
048400090916
048500091201  1       dow not %eof(titas33c)  ;
048600091201             if tasaas>W_aasmax or (tasaas=W_aasmax and tasmgs>W_mgsMax);
048700090916                leave;
048800090916             endif;
048900091201           // includo bolla solo se rientra in uno dei due periodi
049000091201             if ((tasaas*10000+tasmgs)>=st68dal and
049100091201                (tasaas*10000+tasmgs)<=st68al) or
049200091201                ((tasaas*10000+tasmgs)>=st68rda and
049300091201                (tasaas*10000+tasmgs)<=st68ral);
049400090916           // Scarto bolla se figlia
049500090916             setll (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l;
049600090923             if not %equal(fnlbl01l);
049700090923                   // scarto reso, recupero, e conto servizio
049800090923                if %lookup(tastbl:rec)=0 and tasfbr=' ';
049900090923                      // OPERAZIONI A ROTTURA LINEA DI PARTENZA BOLLA
050000090923                   if taslnp<>w_taslnp;
050100090921                // Scrittura dati memorizzati nel file
050200090923                      if w_taslnp>0;
050300090923                         exsr sr_wrtmsp;
050400090923                      endif;
050500090923                      exsr sr_inz;
050600090923                      w_taslnp=taslnp;
050700090923                   endif;
050800090923                // MEMORIZZAZIONE DATI IN CAMPI DI COMODO
050900090923                   exsr sr_memdati;
051000090923                endif;
051100090923             endif;
051200091201             endif;
051300090916          reade (kccm) titas33c  ;
051400090916          enddo;
051500090916       endfor;
051600090917           // Scrittura dati memorizzati nel file
051700090923          if w_taslnp>0;
051800090917             exsr sr_wrtmsp;
051900090923          endif;
052000090916       ENDSR;
052100090828       //-------------------------------------------------------------*
052200090828       //Memorizzazione dati letti                                    *
052300090828       //-------------------------------------------------------------*
052400090828       Begsr SR_rscage ;
052500090828       clear w_rscage;
052600090828       tblkey=%editc(clpage:'X');
052700090828       chain (1:'01':tblkey) tabel00f  ;
052800090828       if %found(tabel00f);
052900090828          ds01=tbluni;
053000090828          w_rscage=§01age;
053100090828       endif;
053200090828       endsr;
053300090828       //-------------------------------------------------------------*
053400090828       //Memorizzazione dati letti                                    *
053500090828       //-------------------------------------------------------------*
053600090828       Begsr SR_memdati;
053700090828
053800090828       exsr sr_pesvol;
053900090917         // --> Se bancali devo sommare i colli originali
054000090828       clear w_collio;
054100090828       If %subst(tasGva:1:1) = 'O';
054200090828          exsr sr_bancali;
054300090828       endif;
054400090921
054500090917         // Dati del periodo di riferimento
054600090921
054700090917       if (tasaas*10000+tasmgs) >= st68rda and
054800090917          (tasaas*10000+tasmgs) <= st68ral;
054900090917          // peso
055000090917          w_pesor=w_pesor+w_tk;
055100090917          // volume
055200090917          w_volur=w_volur+w_tm;
055300090917          // spedizioni
055400090917          w_nspr+=1;
055500090917          // colli
055600090922          if w_collio>0;
055700090917             w_collir=w_collir+w_collio;
055800090917          else;
055900090917             w_collir=w_collir+tasncl;
056000090917          endif;
056100090917       endif;
056200090921
056300090917         // Dati del periodo da elaborare
056400090921
056500090917       if (tasaas*10000+tasmgs) >= st68dal and
056600090917          (tasaas*10000+tasmgs) <= st68al;
056700090917          // peso
056800090917          w_pesoe=w_pesoe+w_tk;
056900090917          // volume
057000090917          w_volue=w_volue+w_tm;
057100090917          // spedizioni
057200090917          w_nspe+=1;
057300090917          // colli
057400090917          if w_collio>0;
057500090917             w_collie=w_collie+w_collio;
057600090917          else;
057700090917             w_collie=w_collie+tasncl;
057800090917          endif;
057900090917       endif;
058000090917       endsr;
058100090828
058200090917       //-------------------------------------------------------------*
058300090917       //Scarico su file dei dati memorizzati                         *
058400090917       //-------------------------------------------------------------*
058500090917       Begsr SR_Wrtmsp;
058600090921        ds01=tbluni;
058700090921        if d10cop>0;
058800090921           w_clI=d10cop;
058900090921        else;
059000090921           w_Cli=acoksc;
059100090921        endif;
059200090922        chain (§01rgf: w_cli : w_taslnp) wfmsp01l;
059300090921        if not %found(wfmsp01l);
059400090922           clear wfmsp000;
059500090921        endif;
059600090918        // SPEDIZIONI
059700090921         MSPSPR+=w_nspr ;
059800090921         MSPSPP+=w_nspe ;
059900090918         // PESO
060000090921         MSPPKR+=w_pesor;
060100090921         MSPPKP+=w_pesoe;
060200090918         // COLLI
060300090921         MSPNCR+=w_collir;
060400090921         MSPNCP+=w_collie;
060500090918         // VOLUME
060600090921         MSPVLR+=w_volur;
060700090921         MSPVLP+=w_volue;
060800090921
060900090921       if %found(wfmsp01l);
061000090922          update wfmsp000;
061100090921       else;
061200090921          exsr sr_riefissi;
061300090922          write wfmsp000;
061400090921       endif;
061500090828
061600090828       endsr;
061700090921       //-------------------------------------------------------------*
061800090921       //Valorizzazione campi "fissi" WFMSP00F                         *
061900090921       //-------------------------------------------------------------*
062000090921       Begsr SR_Riefissi;
062100090921        MSPDEL=%dec(dataiso_cor);
062200090921        MSPUSR=knmus;
062300090921        MSPDAL=st68dal;
062400090921        MSPALL=st68al;
062500090921        MSPRDL=st68rda;
062600090921        MSPRAL=st68ral;
062700090921        MSPAGE=§01rgf ;
062800090921        MSPDAG=§01age ;
062900090922        kfil=%dec(%subst(%editc(§01rgf:'X'):1:3):3:0);
063000090921        chain (kfil) azorg01l;
063100090921        if %found(azorg01l);
063200090921           MSPDIV=orgfl3 ;
063300090921           MSPCAR=orgcar ;
063400090921        endif;
063500090921        t_tblkey=orgfl3;
063600090921        chain (1:'17':t_tblkey) tabel00f  ;
063700090921        if %found(tabel00f);
063800090921           ds17=t_tbluni;
063900090921           mspddi=§17des;
064000090921        endif;
064100090922        t_tblkey=%editc(orgcar:'X');
064200090921        chain (1:'05':t_tblkey) tabel00f  ;
064300090921        if %found(tabel00f);
064400090921           ds05=t_tbluni;
064500090921           mspdar=§05des;
064600090921        endif;
064700090921        if d10cop>0;
064800090921           MSPCLI=d10cop;
064900090921           clear DS_cnaco;
065000090921           clear DS_cnind;
065100090921           clear DS_cnclp;
065200090921           clear DS_fncls;
065300090921           I69kac=mspcli ;
065400090921           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
065500090921           msprag=ds_acorag;
065600090921        else;
065700090921           mspcli=acoksc;
065800090921           msprag=acorag;
065900090921        endif;
066000090921         MSPLNP=w_taslnp ;
066100090921         chain (taslnp) azorg01l;
066200090921         if %found(azorg01l);
066300090921            mspdla=orgdes ;
066400090921         endif;
066500090921         MSPCLV=clpclv ;
066600090921       endsr;
066700090828       //-------------------------------------------------------------*
066800090828       //Reperimento colli originali                                  *
066900090828       //-------------------------------------------------------------*
067000090828       Begsr SR_bancali;
067100090828       chain (tasaas:taslnp:tasnrs:tasnsp:'BNB') fiar531c;
067200090828       if %found(fiar531c) and ar5atb=*blanks;
067300090828          dar5bnb=ar5uni;
067400090828          w_collio=§ar5bcor;
067500090828       endif;
067600090922       endsr;
067700090828       //-------------------------------------------------------------*
067800090828       //Determino peso/volume da memorizzare                         *
067900090828       //-------------------------------------------------------------*
068000090828       Begsr SR_pesvol;
068100090828       clear w_tk;
068200090828       clear w_tm;
068300090828       // peso
068400090828       if tasncp=tasncl or taspkc>taspkf;
068500090828          w_tk=taspkc;
068600090828       else;
068700090828          w_tk=taspkf;
068800090828       endif;
068900090828       // volume
069000090828       if tasfvf='T';
069100090828          w_tm=tasvlf;
069200090828       else;
069300090828          if tasncl=tasncr;
069400090828                w_tm=tasvlc;
069500090828          else;
069600091008             if tasvlc>tasvlf;
069700090828                w_tm=tasvlc;
069800090828             else;
069900090828                w_tm=tasvlf;
070000090828             endif;
070100090828          endif;
070200090828       endif;
070300090922       endsr;
070400090917       //-------------------------------------------------------------*
070500090917       //Azzeramento campi di lavoro colli/peso/volume/spedizione     *
070600090917       //-------------------------------------------------------------*
070700090917       Begsr SR_inz;
070800090917       clear w_nspr;
070900090917       clear w_nspe;
071000090917       clear w_pesor;
071100090917       clear w_pesoe;
071200090917       clear w_volur;
071300090917       clear w_volue;
071400090917       clear w_collir;
071500090917       clear w_collie;
071600090917       endsr;
071700090922       //-------------------------------------------------------------*
071800090922       //Routine di aggiornamento file di work con medie e % di scost.*
071900090922       //-------------------------------------------------------------*
072000090922       Begsr SR_output;
072100090922
072200090929       clear savage1;
072300090929       clear savcli1;
072400091005       clear savrag1;
072500090929
072600090922       setll (*loval) wfmsp01l;
072700090922       read  wfmsp01l;
072800090922       dow not %eof(wfmsp01l);
072900090922       exsr sr_calcola;
073000091006       // a cambio di age o cli registro record di totale
073100091006       if st68crt=*blanks;
073200090929          if  (mspage<>savage1 or mspcli<>savcli1);
073300090929             if savage1>0;
073400090929                exsr sr_calcolat;
073500090929             endif;
073600091006             clear c_lnp;
073700090929             savage1=mspage;
073800090929             savcli1=mspcli;
073900091005             savrag1=msprag;
074000090929             clear t_mspspr;
074100090929             clear t_mspspp;
074200090929             clear t_msppkr;
074300090929             clear t_msppkp;
074400090929          endif;
074500090929          t_mspspr+=mspspr;
074600090929          t_mspspp+=mspspp;
074700090929          t_msppkr+=msppkr;
074800090929          t_msppkp+=msppkp;
074900091006          c_lnp+=1;
075000091006       endif;
075100090922       read  wfmsp01l;
075200090922       enddo;
075300091006       if st68crt=*blanks and (T_mspspr>0 or t_mspspp>0);
075400090929          exsr sr_calcolat;
075500090929       endif;
075600090922
075700090922       endsr;
075800090922       //-------------------------------------------------------------*
075900090922       //Preparo stringa sql per estrazione dati da WFMSP00F          *
076000090922       //-------------------------------------------------------------*
076100090922       Begsr sr_prepSqlMsp;
076200091005       WrkStringaSqlM='select distinct mspage, mspcli, msprag from wfmsp00f'+
076300091005                      ' where msplnp=0 and';
076400090929       // limite % riferito a SPEDIZIONI MEDIE
076500090929       if st68rif='S';
076600090929         // limite percentuale negativo
076700090929         if st68sco<0;
076800090929            WrkStringaSqlM=WrkStringaSqlM+
076900090929            ' mspspc<' + %char(st68sco);
077000090929         else;
077100090929         // limite percentuale positivo
077200090929            WrkStringaSqlM=WrkStringaSqlM+
077300090929            ' mspspc>'+ %char(st68sco);
077400090929         endif;
077500090929       else;
077600090929       // limite % riferito a PESO MEDIO
077700090929         // limite percentuale negativo
077800090929         if st68sco<0;
077900090929            WrkStringaSqlM=WrkStringaSqlM+
078000090929            ' mspppc<' + %char(st68sco);
078100090929         else;
078200090929         // limite percentuale positivo
078300090929            WrkStringaSqlM=WrkStringaSqlM+
078400090929            ' mspppc>' + %char(st68sco);
078500090929         endif;
078600090929       endif;
078700090929
078800090929       // ordino per comm.le/cliente/lnp
078900090929            WrkStringaSqlM=WrkStringaSqlM+
079000091005            ' order by mspage, msprag, mspcli';
079100090929
079200090922       endsr;
079300090922       //-------------------------------------------------------------*
079400090922       //leggo dati file di work estratti con sql                     *
079500090922       //-------------------------------------------------------------*
079600090922       Begsr sr_elamsp1;
079700090922        $finerec=*off;
079800090922        exec sql prepare STRINGASQLM from :WrkStringaSqlM;
079900090922        exec sql declare MSPCsr cursor for StringaSqlM;
080000090922        exec sql open MSPcsr;
080100090922        dow $finerec=*off;
080200091005           exec sql Fetch MSPcsr into :mspage, :mspcli, :msprag;
080300090922           if sqlcod=100 or sqlcod<0;
080400090922              $finerec = *on;
080500090922              leave;
080600090922           endif;
080700090922           exsr sr_leggimsp;
080800090922        enddo;
080900100615
081000090929        if savcli>0;
081100090929           if st68det='N';
081200090929              exsr sr_stampatot;
081300090929           else;
081400090929              exsr sr_rotcli;
081500100615           endif;
081600100615
081700100615             // Stampo i totali  del commerciale
081800100615                 exsr staTOTage  ;
081900090929        endif;
082000100615
082100100615        // totale generale solo se non richeisto 1 unificante
082200100615        if st68age=0  ;
082300100615                 exsr staTOTGEN  ;
082400100615         endif   ;
082500090922       endsr;
082600090922       //-------------------------------------------------------------*
082700090922       //                                                             *
082800090922       //-------------------------------------------------------------*
082900090922       Begsr sr_leggimsp;
083000090922        setll (mspage: mspcli) wfmsp01l;
083100090922        reade (mspage: mspcli) wfmsp01l;
083200090922  1     dow   not %eof(wfmsp01l)  ;
083300091006        // del record di totale prendo il numero di lnp per il cliente
083400091006          if msplnp=0;
083500091006             eval *in44=*off;
083600091006             w0030=%dec(%subst(mspusr:1:3):3:0);
083700091006             if w0030=1;
083800091006                eval *in44=*on;
083900091006             endif;
084000091006          else;
084100090929             if st68det='S';
084200090929                exsr sr_stampa;
084300090929             else;
084400090929                exsr sr_stampa1;
084500090929             endif;
084600090929          endif;
084700090922          reade (mspage: mspcli) wfmsp01l;
084800090922        enddo;
084900090922       endsr;
085000090922       //-------------------------------------------------------------*
085100090928       //leggo dati file di work - Richiesta senza limite % scostam.  *
085200090922       //-------------------------------------------------------------*
085300090922       Begsr sr_elamsp2;
085400091005       setll (*loval) wfmsp02l;
085500100111       read  wfmsp02l;
085600091005       dow not %eof(wfmsp02l);
085700090922
085800091006       if msplnp=0;
085900091006          eval *in44=*off;
086000091006          w0030=%dec(%subst(mspusr:1:3):3:0);
086100091006          if w0030=1;
086200091006             eval *in44=*on;
086300091006          endif;
086400091006       else;
086500091006          if st68det='S';
086600091006             exsr sr_stampa;
086700091006          else;
086800091006             exsr sr_stampa1;
086900091006          endif;
087000091006       endif;
087100090922
087200091005       read  wfmsp02l;
087300090922       enddo;
087400100615
087500090929       if savcli>0;
087600090929          if st68det='N';
087700090929             exsr sr_stampatot;
087800090929          else;
087900090929             exsr sr_rotcli;
088000090929           endif;
088100100615
088200100615             // Stampo i totali  del commerciale
088300100615                 exsr staTOTage  ;
088400090929       endif;
088500100615        // totale generale solo se non richeisto 1 unificante
088600100615        if st68age=0  ;
088700100615                 exsr staTOTGEN  ;
088800100615        endif  ;
088900090922       endsr;
089000090921       //-------------------------------------------------------------*
089100090921       //                                                             *
089200090921       //-------------------------------------------------------------*
089300090921       Begsr SR_calcola;
089400090921         // Calcolo spedizioni medie periodo di riferimento
089500090924         eval(H) MSPSMR=mspspr/ggl_r;
089600090924         eval(H) MSPSMP=mspspp/ggl_e ;
089700090921         // Calcolo la % di scostamento della media sped. del periodo rispetto
089800090921         // alla media spedizioni del periodo di riferimento
089900090923         if mspsmp>0 and mspsmp<>mspsmr and mspsmr>0;
090000090924            eval(H) MSPSPC=((mspsmp-mspsmr)*100)/mspsmr ;
090100090923         endif;
090200090923         if mspsmp<>mspsmr and mspsmp=0;
090300090923            mspspc=-100;
090400090923         endif;
090500090921         // calcolo il peso medio a spedizione
090600090923         if mspspr>0;
090700090924            eval(H) MSPPMR=msppkr/mspspr;
090800090923         endif;
090900090923         if mspspp>0;
091000090924            eval(H) MSPPMP=msppkp/mspspp ;
091100090923         endif;
091200090921         // Calcolo la % di scostamento del peso medio del periodo rispetto
091300090921         // al peso medio del periodo di riferimento
091400090923         if msppmp>0 and msppmp<>msppmr and msppmr>0;
091500090924            eval(H) mspppc=((msppmp-msppmr)*100)/msppmr;
091600090923         endif;
091700090923         if msppmp<>msppmr and msppmp=0;
091800090923            mspppc=-100;
091900090923         endif;
092000090921
092100090921         update wfmsp000;
092200090921
092300090921       endsr;
092400090928       //-------------------------------------------------------------*
092500090928       //                                                             *
092600090928       //-------------------------------------------------------------*
092700090928       Begsr SR_calcolat;
092800090929        clear t_mspsmr;
092900090929        clear t_mspsmp;
093000090929        clear t_mspspc;
093100090929        clear t_msppmr;
093200090929        clear t_msppmp;
093300090929        clear t_mspppc;
093400090928        exsr sr_calcolat_s;
093500090928        exsr sr_calcolat_p;
093600090929        // scrivo nel file di work record di totale
093700090929
093800090929        sav_wfmspds=wfmspds;
093900090929
094000090929        clear wfmsp000;
094100091006        // memorizzo se  per il cliente unificante sono presenti più lnp
094200091006        // servirà al momento della stampa per sapere se la riga deve essere
094300091006        // in grassetto oppure no
094400091006        eval mspusr=%editc(c_lnp:'X');
094500090929        mspage=savage1;
094600090929        mspcli=savcli1;
094700091005        msprag=savrag1;
094800090929        mspspr=t_mspspr;
094900090929        mspspp=t_mspspp;
095000090929        msppkr=t_msppkr;
095100090929        msppkp=t_msppkp;
095200090929        mspsmr=t_mspsmr;
095300090929        mspsmp=t_mspsmp;
095400090929        mspspc=t_mspspc;
095500090929        msppmr=t_msppmr;
095600090929        msppmp=t_msppmp;
095700090929        mspppc=t_mspppc;
095800090929        write wfmsp000;
095900090929
096000090929        wfmspds=sav_wfmspds;
096100090929
096200090928       endsr;
096300090928       //-------------------------------------------------------------*
096400090928       //                                                             *
096500090928       //-------------------------------------------------------------*
096600090928       Begsr SR_calcolat_s;
096700090928         // Calcolo spedizioni medie periodo di riferimento
096800091130         eval(H) t_msPSMR=t_mspspr/ggl_r;
096900091130         eval(H) t_MSPSMP=t_mspspp/ggl_e ;
097000090928         // Calcolo la % di scostamento della media sped. del periodo rispetto
097100090928         // alla media spedizioni del periodo di riferimento
097200090928         if t_mspsmp>0 and t_mspsmp<>t_mspsmr and t_mspsmr>0;
097300090928            eval(H) t_MSPSPC=((t_mspsmp-t_mspsmr)*100)/t_mspsmr ;
097400090928         endif;
097500090928         if t_mspsmp<>t_mspsmr and t_mspsmp=0;
097600090928            t_mspspc=-100;
097700090928         endif;
097800090928         endsr;
097900090928       //-------------------------------------------------------------*
098000090928       //                                                             *
098100090928       //-------------------------------------------------------------*
098200090928       Begsr SR_calcolat_p;
098300090928         // calcolo il peso medio a spedizione
098400090928         if t_mspspr>0;
098500090928            eval(H) t_MSPPMR=t_msppkr/t_mspspr;
098600090928         endif;
098700090928         if t_mspspp>0;
098800090928            eval(H) t_MSPPMP=t_msppkp/t_mspspp ;
098900090928         endif;
099000090928         // Calcolo la % di scostamento del peso medio del periodo rispetto
099100090928         // al peso medio del periodo di riferimento
099200090928         if t_msppmp>0 and t_msppmp<>t_msppmr and t_msppmr>0;
099300090928            eval(H) t_mspppc=((t_msppmp-t_msppmr)*100)/t_msppmr;
099400090928         endif;
099500090928         if t_msppmp<>t_msppmr and t_msppmp=0;
099600090928            t_mspppc=-100;
099700090928         endif;
099800090928       endsr;
099900090921       //-------------------------------------------------------------*
100000090925       // Stampa riga con dettaglio per lnp                           *
100100090921       //-------------------------------------------------------------*
100200090921       Begsr SR_stampa ;
100300090922       *in40=*off;
100400090922       *in41=*off;
100500090925       //Gestione rottura cliente unificante
100600090925       if mspcli<>savcli;
100700090925          exsr sr_rotcli;
100800090925       endif;
100900100615
101000090925       //Gestione rottura commerciale unificante
101100090922       if mspage<>savage;
101200090925          exsr sr_rotage;
101300090922       endif;
101400090922       plnp=msplnp;
101500090922       pspr=mspspr;
101600090922       pspp=mspspp;
101700090922       psmr=mspsmr;
101800090922       psmp=mspsmp;
101900100615       pcat=mspclv;
102000100615
102100090924       if %abs(mspspc)>999,99;
102200090924          if mspspc>0;
102300090924             pspc=999,99;
102400090924          else;
102500090924             pspc=-999,99;
102600090924          endif;
102700090924       else;
102800090924          pspc=mspspc;
102900090924       endif;
103000090922       ppmr=msppmr;
103100090922       ppmp=msppmp;
103200090924       if %abs(mspppc)>999,99;
103300090924          if mspppc>0;
103400090924             pppc=999,99;
103500090924          else;
103600090924             pppc=-999,99;
103700090924          endif;
103800090924       else;
103900090924          pppc=mspppc;
104000090924       endif;
104100090922       c_lnp+=1;
104200090925       if $righe+1>57;
104300090925          exsr sr_overflow;
104400090925          write st68rv1;
104500090925          $righe+=1;
104600090923       endif;
104700090922       write st68r01;
104800090925       $righe+=1;
104900100615       savclv=mspclv   ;
105000090922       exsr sr_imptot;
105100090921       endsr;
105200090925       //-------------------------------------------------------------*
105300090925       //Operazioni a rottura di codice cliente unificante            *
105400090925       //-------------------------------------------------------------*
105500090925       Begsr SR_rotcli;
105600090925          // stampa totale cliente precedente se aveva più lnp
105700090925          if c_lnp>1;
105800090925             exsr sr_caltot;
105900090925             write st68tot;
106000090925             $righe+=2;
106100090925          endif;
106200100615
106300090929          exsr sr_clrtot;
106400090925          *in41=*on;
106500090925          pkscuni=mspcli;
106600090925          pkscdes=msprag;
106700091006          tkscdes=msprag;
106800090928          pcat=mspclv;
106900090925          savcli=mspcli;
107000090925          clear c_lnp;
107100100615          if $righe+1<=60;
107200090925             write st68rv1;
107300090925             $righe+=1;
107400090925          endif;
107500090925       endsr;
107600090925       //-------------------------------------------------------------*
107700090925       //Operazioni a rottura di commerciale unificante               *
107800090925       //-------------------------------------------------------------*
107900090925       Begsr SR_rotage;
108000100615       // Eseguo stampa totale commerciale precedente
108100100615       if savage>0        ;
108200100615          exsr staTOTage  ;
108300100615       endif    ;
108400100615
108500090925          *in40=*on;
108600090925          pageuni=mspage;
108700090925          pagedes=mspdag;
108800090925       // Eseguo salto pagina se non è la prima pagina di stampa
108900090925          if savage>0;
109000090925             exsr sr_overflow;
109100090925             write st68rv1;
109200090925             $righe+=1;
109300090925          endif;
109400100615
109500090925          savage=mspage;
109600100615          savdes=mspdag;
109700090925       endsr;
109800090925       //-------------------------------------------------------------*
109900090925       // Stampa riga - no dettaglio per lnp                          *
110000090925       //-------------------------------------------------------------*
110100090925       Begsr SR_stampa1;
110200090925       *in40=*off;
110300090925       //Gestione rottura cliente unificante
110400090925       if mspcli<>savcli;
110500090925          if savcli>0;
110600090929             exsr sr_stampatot;
110700090925          endif;
110800090925          exsr sr_clrtot;
110900100614
111000100614             // Se cambiato cod commerciale, stampo i totali
111100100615              if mspage<>savage  and savage>0;
111200100614                 exsr staTOTage  ;
111300100614                 exsr sr_clrtot;
111400100614              endif         ;
111500100614
111600090925          savcli=mspcli;
111700090925          savrag=msprag;
111800090925          savage=mspage;
111900090925          savdes=mspdag;
112000090928          savclv=mspclv;
112100090925       endif;
112200100614
112300090925       exsr sr_imptot;
112400090925       endsr;
112500090925       //-------------------------------------------------------------*
112600090925       //Stampa intestazione per salto pagina                         *
112700090925       //-------------------------------------------------------------*
112800090925       Begsr SR_overflow;
112900090925          write st68t00;
113000110620          write st68t01;
113100090925          $righe=10;
113200090925          *in40=*on;
113300090925          *in41=*on;
113400090925       endsr;
113500090922       //-------------------------------------------------------------*
113600090922       //Routine di pulizia campi per stampa totale                   *
113700090922       //-------------------------------------------------------------*
113800090922       Begsr SR_clrtot;
113900090922             clear tspr;
114000090922             clear tspp;
114100090922             clear tsmr;
114200090922             clear tsmp;
114300090922             clear tspc;
114400090922             clear tpmr;
114500090922             clear tpmp;
114600090922             clear tppc;
114700090922             clear tpkr;
114800090922             clear tpkp;
114900090922       endsr;
115000090922       //-------------------------------------------------------------*
115100090922       //Routine di impostazione campi di stampa record di totale     *
115200090922       //-------------------------------------------------------------*
115300090922       Begsr SR_imptot;
115400090922
115500090925       tspr+=mspspr;
115600090925       tspp+=mspspp;
115700090922       tpkr+=msppkr;
115800090922       tpkp+=msppkp;
115900100614
116000100614        // sommo anche nelle skiere per tipo cliente
116100100614        select   ;
116200100614        when savclv='D' ;
116300100614        xx=1  ;
116400100614        when savclv='T' ;
116500100614        xx=2  ;
116600100614        when savclv='A' ;
116700100614        xx=3  ;
116800100614        other         ;
116900100614        xx=4  ;
117000100614        endsl   ;
117100090922
117200100614       Upr(xx)+=mspspr;
117300100614       Upp(xx)+=mspspp;
117400100614       Ukr(xx)+=msppkr;
117500100614       Ukp(xx)+=msppkp;
117600100614
117700090922       endsr;
117800090922       //-------------------------------------------------------------*
117900090922       //Routine di calcolo medie e % per riga totale                 *
118000090922       //-------------------------------------------------------------*
118100090922       Begsr SR_caltot;
118200090922         // Calcolo spedizioni medie periodo di riferimento
118300091130         eval(h) tsmr=tspr/ggl_r;
118400091130         eval(h) tSMP=tspp/ggl_e ;
118500100615
118600090922         // Calcolo la % di scostamento della media sped. del periodo rispetto
118700090922         // alla media spedizioni del periodo di riferimento
118800100615
118900090923         if tsmp>0 and tsmr>0 and tsmp<>tsmr;
119000090924            eval(h) w0122=((tsmp-tsmr)*100)/tsmr ;
119100090924            if %abs(w0122)>999,99;
119200090924               if w0122>0;
119300090924                  tspc=999,99;
119400090924               else;
119500090924                  tspc=-999,99;
119600090924               endif;
119700090924            else;
119800090924               tspc=w0122;
119900090924            endif;
120000090923         endif;
120100090923         if tsmp<>tsmr and tsmp=0;
120200090923            tSPC=-100;
120300090923         endif;
120400090922         // calcolo il peso medio a spedizione
120500090923         if tspr>0;
120600090924            eval(h) tPMR=tpkr/tspr;
120700090923         endif;
120800090923         if tspp>0;
120900090924            eval(h) tPMP=tpkp/tspp ;
121000090923         endif;
121100100615
121200090922         // Calcolo la % di scostamento del peso medio del periodo rispetto
121300090922         // al peso medio del periodo di riferimento
121400100615
121500090923         if tpmp>0 and tpmr>0 and tpmp<>tpmr;
121600090924            eval(h) w0142=((tpmp-tpmr)*100)/tpmr;
121700090924            if %abs(w0142)>999,99;
121800090924               if w0142>0;
121900090924                  tppc=999,99;
122000090924               else;
122100090924                  tppc=-999,99;
122200090924               endif;
122300090924            else;
122400090924               tppc=w0142;
122500090924            endif;
122600090923         endif;
122700090923         if tpmp<>tpmr and tpmp=0;
122800090923            tppc=-100;
122900090923         endif;
123000090922       endsr;
123100090929       //-------------------------------------------------------------*
123200090929       //Stampa totale cliente - stampa sintetica -                   *
123300090929       //-------------------------------------------------------------*
123400090929       Begsr SR_stampatot;
123500090929             // calcolo medie e %scostamento
123600090929             exsr sr_caltot;
123700090929             // verifico rottura commerciale
123800090929                if savage<>pre_age;
123900100614
124000100614
124100090929                   *in40=*on;
124200090929                   pageuni=savage;
124300090929                   pagedes=savdes;
124400090929                   // Eseguo salto pagina se non è la prima pagina di stampa
124500090929                   if $righe>10;
124600090929                      exsr sr_overflow;
124700090929                      write st68rv1;
124800090929                      $righe+=1;
124900090929                   else;
125000090929                      write st68rv1;
125100090929                      $righe+=1;
125200090929                   endif;
125300090929                   pre_age=savage;
125400090929                endif;
125500100614
125600090929                pkscuni=savcli;
125700090929                pkscdes=savrag;
125800090929                pcat   =savclv;
125900100615                if $righe+2>60;
126000090929                   exsr sr_overflow;
126100090929                   write st68rv1;
126200090929                   $righe+=1;
126300090929                endif;
126400090929                write st68rt1;
126500090929                write st68rv1;
126600090929                $righe+=2;
126700090929       endsr;
126800100614       //-------------------------------------------------------------*
126900100615       //Stampa totale Agente
127000100614       //-------------------------------------------------------------*
127100100614       Begsr StaTOTage;
127200100614
127300100614       // La prima riga stampa anche il commerciale
127400100614       *in40=*on;
127500100614       pageuni=savage;
127600100614       pagedes=savdes;
127700100614
127800100615       // Faccio un ciclo di 4 per ogni tipo cliente
127900100614       xx=1   ;
1280001006141      dow xx<=4  ;
1281001006142      if Upr(xx)>0 or Upp(xx)>0 or Ukr(xx)>0 or Ukp(XX)>0 ;
128200100614       TSPR=Upr(xx)  ;
128300100614       TSPp=Upp(xx)  ;
128400100614       Tpkr=Ukr(xx)  ;
128500100614       Tpkp=Ukp(xx)  ;
128600100614
128700100615             // sommo ai totali generali
128800100615       Gupr(xx)+=Upr(xx)  ;
128900100615       Gupp(xx)+=Upp(xx)  ;
129000100615       Gukr(xx)+=Ukr(xx)  ;
129100100615       Gukp(xx)+=Ukp(xx)  ;
129200100615
129300100614             // calcolo medie e %scostamento
129400100614             exsr sr_caltot;
129500100614
129600100614        select   ;
129700100614        when xx=1       ;
129800100614        pcat='D';
129900100614        when xx=2       ;
130000100614        pcat='T';
130100100614        when xx=3       ;
130200100614        pcat='A';
130300100614        other         ;
130400100614        pcat='B';
130500100614        endsl   ;
130600100614
130700100614 3              if $righe+1>61;
130800100614                   exsr sr_overflow;
130900100614                   write st68rv1;
131000100614                   $righe+=1;
131100100614 3              endif;
131200100614
131300100614 3              if *in40 ;
131400100614                write st68tage1;
131500100614 3              endif   ;
131600100614                write st68tage2 ;
131700100614
131800100614 3              if *in40 ;
131900100614                $righe+=2;
132000100614                else  ;
132100100614                $righe+=1;
132200100614 3              endif  ;
132300100614
132400100614       *in40 =*off   ;
132500100614 2     endif   ;
132600100614
132700100614       xx=xx+1 ;
132800100614 1     enddo  ;
132900100615
133000100615       // Totale generale
133100100615       TSPR=%xfoot(Upr)   ;
133200100615       TSPp=%xfoot(Upp)  ;
133300100615       Tpkr=%Xfoot(Ukr)  ;
133400100615       Tpkp=%xfoot(Ukp)  ;
133500100615
133600100615             // calcolo medie e %scostamento
133700100615             exsr sr_caltot;
133800100615 3              if $righe+3>62;
133900100615                   exsr sr_overflow;
134000100615                   write st68rv1;
134100100615                   $righe+=1;
134200100615 3              endif;
134300100615
134400100615                *in40=*on   ;
134500100615                *in50=*on   ;
134600100615                clear pcat  ;
134700100615                pgenerale='TOTALE COMMERCIALE'   ;
134800100615                write st68tage1;
134900100615                write st68tage2 ;
135000100615                write st68tage1;
135100101221                $righe+=3;
135200100615                *in40=*oFF  ;
135300100615                *in50=*oFF  ;
135400100615
135500100615       clear upr       ;
135600100615       clear upp       ;
135700100615       clear ukr       ;
135800100615       clear ukp       ;
135900100614
136000100614       endsr;
136100100615       //-------------------------------------------------------------*
136200100615       //Stampa totale GENERALE
136300100615       //-------------------------------------------------------------*
136400100615       Begsr StaTOTGEN;
136500100615
136600100615       // Eseguo salto pagina
136700100615             exsr sr_overflow;
136800100615             write st68rv1;
136900100615             $righe+=1;
137000100615
137100100615       *in40=*off ;
137200100615       write st68tage1;
137300101221       $righe+=1;
137400100615
137500100615       // Faccio un ciclo di 4 per ogni tipo cliente
137600100615       xx=1   ;
1377001006151      dow xx<=4  ;
1378001006152      if GUpr(xx)>0 or GUpp(xx)>0 or GUkr(xx)>0 or GUkp(XX)>0 ;
137900100615       TSPR=gUpr(xx)  ;
138000100615       TSPp=gUpp(xx)  ;
138100100615       Tpkr=gUkr(xx)  ;
138200100615       Tpkp=gUkp(xx)  ;
138300100615
138400100615             // calcolo medie e %scostamento
138500100615             exsr sr_caltot;
138600100615
138700100615        select   ;
138800100615        when xx=1       ;
138900100615        pcat='D';
139000100615        when xx=2       ;
139100100615        pcat='T';
139200100615        when xx=3       ;
139300100615        pcat='A';
139400100615        other         ;
139500100615        pcat='B';
139600100615        endsl   ;
139700100615
139800100615                write st68tage2 ;
139900100615                $righe+=1;
140000100615
140100100615 2     endif   ;
140200100615
140300100615       xx=xx+1 ;
140400100615 1     enddo  ;
140500100615
140600100615       // Totale generale
140700100615       TSPR=%xfoot(gUpr)   ;
140800100615       TSPp=%xfoot(gUpp)  ;
140900100615       Tpkr=%Xfoot(gUkr)  ;
141000100615       Tpkp=%xfoot(gUkp)  ;
141100100615
141200100615             // calcolo medie e %scostamento
141300100615             exsr sr_caltot;
141400100615
141500100615                *in50=*on   ;
141600100615                clear pcat  ;
141700100615                pgenerale='TOTALE GENERALE   '   ;
141800100615                write st68tage1;
141900100615                write st68tage2 ;
142000100615                write st68tage1;
142100101221                $righe+=3;
142200100615                *in40=*oFF  ;
142300100615                *in50=*oFF  ;
142400100615
142500100615       endsr;
142600090921       //-------------------------------------------------------------*
142700090921       //Routine di impostazione campi di stampa record di testata    *
142800090921       //-------------------------------------------------------------*
142900090921       Begsr SR_impt00;
143000090921
143100090921       // Titolo
143200090921
143300090921       dataeur=%date(st68dal:*iso);
143400090921       wdatagma=%dec(dataeur);
143500091007       ptitolo='Analisi Spedizioni dal';
143600090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
143700090921       dataeur=%date(st68al:*iso);
143800090921       wdatagma=%dec(dataeur);
143900090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
144000090921
144100090925       // Categoria cliente
144200090921
144300090929       wprimo='S';
144400090925       pcatcli ='Categoria clienti';
144500090921       if st68clv=*blanks;
144600090925          pcatcli=%trim(pcatcli)+ ' D/T/A/B';
144700090921       else;
144800090922          for I=1 to %elem(clv);
144900090929             if clv(i)<>*blanks;
145000090929                if wprimo<>'S';
145100090929                   pcatcli=%trim(pcatcli)+ '/';
145200090929                endif;
145300090929                if wprimo='S';
145400090929                   pcatcli=%trim(pcatcli)+ ' ' +clv(i);
145500090929                else;
145600090929                   pcatcli=%trim(pcatcli)+ clv(i);
145700090929                endif;
145800090929                wprimo=' ';
145900090929             endif;
146000090921          endfor;
146100090921       endif;
146200090921
146300090921       // Periodo di riferimento dal/al
146400090921
146500090921       dataeur=%date(st68rda:*iso);
146600090922       prifdal=%dec(dataeur);
146700090921       dataeur=%date(st68ral:*iso);
146800090922       prifal=%dec(dataeur);
146900090921
147000090921       // limite % di scostamento
147100090930       *in43=*off;
147200090930       if st68sco<>0;
147300090930          *in43=*on;
147400090921
147500090930          pscost=st68sco;
147600090921
147700090921       // Valore di riferimento %
147800090921
147900090930          select;
148000090930          when st68rif='S';
148100090930            prifper='Spedizioni';
148200090930          when st68rif='P';
148300090930            prifper='Peso medio';
148400090930          endsl;
148500090930       endif;
148600101110
148700101110       // area se richiesta
148800101110       if st68car>0   ;
148900101110       clear tblkey  ;
149000101110       eval tblkey=%editc(st68car:'X') ;
149100101110       chain (1:'05':tblkey) tabel00f  ;
149200101110         if not %found(tabel00f);
149300101110          clear t_tbluni   ;
149400101110         endif   ;
149500101110       eval priare='Area  ' + %editc(st68car:'X') +' ' + t_tbluni   ;
149600101110       endif  ;
149700101110
149800090922       endsr;
