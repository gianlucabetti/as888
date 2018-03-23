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
011700110805     d wuniabl         s              1
011800090828     d w_unif          s                   like(tasccm)
011900090921     d w_taslnp        s                   like(taslnp)
012000090828     d w_rscuni        s                   like(acorag)
012100090922     d w_cli           s                   like(acoksc)
012200090828     d w_rscage        s                   like(§01age)
012300090828     d sav_ccm         s                   like(tasccm) inz
012400090828     d w_collio        s                   like(§ar5bcor)
012500090917     d w_nspr          s                   like(mspspr)
012600090917     d w_nspe          s                   like(mspspr)
012700090917     d w_pesor         s                   like(msppkr)
012800090917     d w_pesoe         s                   like(msppkr)
012900090917     d w_volur         s                   like(mspvlr)
013000090917     d w_volue         s                   like(mspvlr)
013100090917     d w_collir        s                   like(mspncr)
013200090917     d w_collie        s                   like(mspncr)
013300090922     d savage          s                   like(mspage)
013400090929     d savage1         s                   like(mspage)
013500091005     d savrag1         s                   like(msprag)
013600090925     d pre_age         s                   like(mspage) inz
013700090925     d savdes          s                   like(mspdag)
013800090922     d savcli          s                   like(mspcli)
013900090929     d savcli1         s                   like(mspcli)
014000090925     d savrag          s                   like(msprag)
014100090928     d savclv          s                   like(mspclv)
014200110808     d savblo          s                   like(mspblo)
014300090828     d w_tk            s                   like(taspkf)
014400090828     d w_tm            s                   like(tasvlf)
014500090922     d tpkr            s                   like(msppkr)
014600090922     d tpkp            s                   like(msppkp)
014700090928     d t_mspspr        s                   like(mspspr)
014800090928     d t_mspspp        s                   like(mspspr)
014900090928     d t_msppkp        s                   like(msppkr)
015000090928     d t_msppkr        s                   like(msppkr)
015100090928     d t_mspsmr        s                   like(mspsmr)
015200090928     d t_mspsmp        s                   like(mspsmp)
015300090928     d t_mspspc        s                   like(mspspc)
015400090928     d t_msppmr        s                   like(msppmr)
015500090928     d t_msppmp        s                   like(msppmp)
015600090928     d t_mspppc        s                   like(mspppc)
015700090918     d ggl_e           s                   like(ggl)
015800090918     d ggl_r           s                   like(ggl)
015900090917     d w_aasmin        s                   like(tasaas)
016000090917     d w_aasmax        s                   like(tasaas)
016100090917     d w_mgsmin        s                   like(tasmgs)
016200090917     d w_mgsmax        s                   like(tasmgs)
016201110819     d w_aasdax        s                   like(tasaas)
016202110819     d w_mgsdax        s                   like(tasmgs)
016300090917     d kccm            s                   like(tasccm)
016400090929     d sav_wfmspds     s                   like(wfmspds)
016500040108     d w0140           s             14  0
016600040123     d wDataAMG        s              8  0
016700040123     d wDataGMA        s              8  0
016800040120     d wOra            s              6  0
016900100614     d xx              s              4  0
017000101110     d Primo           s              4  0 inz
017100090414     d dataiso_cor     s               d   datfmt(*iso)
017200090414     d dataeur_cor     s               d   datfmt(*eur)
017300090416     d dataeur         s               d   datfmt(*eur)
017400090409     d datadmy         s               d   datfmt(*dmy)
017500090408     d kut             s                   like(acokut) inz(1)
017600090917     d wabi            s                   like(UteAut)
017700090918     d kfil            s                   like(orgfil)
017800090922     d Wtas33          s             21    inz('GAITRAGRU /TITAS33C')
017900090922     d War531          s             21    inz('GAITRAGRU /FIAR531C')
018000110228     D Wlibsede        S             21
018100110228     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
018200110228     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
018300110228     D WlibsedeP5      S             21    inz('GAITRAGRPS/FIAR531C')
018400110228     D WlibsedeB5      S             21    INZ('GAITRAGRU /FIAR531C')
018500090828      *
018600090828      * - Flags booleani
018700090828     d $Finerec        s              1    inz(*off)
018800081023
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
019100090827      /copy gaitrasrc/srcprotopr,trul31r
019200090828      /copy gaitrasrc/srcprotopr,tibs10r
019300090828      /copy gaitrasrc/srcprotopr,tibs69r
019400090918      /copy gaitrasrc/srcprotopr,xsrlav8
019500040107
019600040107      *===============================================================*
019700040107      *  RIEPILOGO INDICATORI                                         *
019800040107      *---------------------------------------------------------------*
019900090827      *  XX    -
020000040107      *===============================================================*
020100040107
020200040107     c     *Entry        plist
020300040107     c                   parm                    KPJBA
020400090827     c
020500090827     c                   movel     kpjbu         tist68ds
020600040123      *
020700090331      /free
020800090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
020900090610
021000090925       // Se richiesto dettaglio per lnp attivo indicatore per stampa
021100090925       if st68det='S';
021200090925          *in42=*on;
021300090925       endif;
021400090827       // determino numero giorni lavorativi fra i due periodi dal/al
021500090917       // ed imposto aas/mgs minimo e max da elaborare
021600090827       exsr sr_gglav;
021700090916       // preparo stringa sql per estrazione codici clienti da elaborare
021800090827       exsr sr_prepSql;
021900090827       // elaboro dati estratti
022000090827       exsr sr_Elabora;
022100090925       // Calcolo le medie e le %
022200090921       exsr sr_output;
022300090922
022400090922       // OPERAZIONI PER LA STAMPA
022500110620       Select;
022600110620
022700110620         // - Lancio da Filiale
022800110620         When  st68crt <> 'S';
022900110620           open tist68p;
023000110620           // Stampo intestazione
023100110620           exsr sr_impt00;
023200110620           write st68t00;
023300110620           write st68t01;
023400110620           $righe=10;
023500110620           clear c_lnp;
023600110620           // preparo stringa sql per estrazione clienti da stampare in base al limite
023700110620           // percentuale
023800110620           if st68sco<>0;
023900110620              exsr sr_prepSqlMsp;
024000110620              exsr sr_elamsp1;
024100110620           else;
024200110620              exsr sr_elamsp2;
024300110620           endif;
024400110620           // Stampo *** Fine Stampa ***
024500110620           if $righe+2>60;
024600110620              exsr sr_overflow;
024700110620           endif;
024800110620           write fines;
024900110620
025000110620         // - Lancio da Sede per le Poste
025100110620         When  ST68crt = 'S'  and  KCDAZ = 'ST6H';
025200110620           open  TIST68P;
025300110620           // - Stampa intestazione
025400110620           exsr  sr_ImpT00;
025500110620           write ST68T00;
025600110620           // - Stampa "Elaborazione Terminata"
025700110620           write ST68END;
025800110620
025900110620       EndSl;
026000090922
026100090827       eval *inlr=*on;
026200040108
026300090402       //-------------------------------------------------------------*
026400090402       //Routine Iniziale                                             *
026500090402       //-------------------------------------------------------------*
026600090414       begsr *inzsr;
026700090402
026800090402       //Reperisco dati job
026900090917         exsr datijob;
027000110228       if %subst(knsif:7:1) = 'P';
027100110228         Wlibsede = Wlibsedep;
027200110228         Else ;
027300110228         wlibsede = Wlibsedeb;
027400110228       endif;
027500110228       Open   Titas33c;
027600110228       if %subst(knsif:7:1) = 'P';
027700110228         Wlibsede = Wlibsedep5;
027800110228         Else ;
027900110228         wlibsede = Wlibsedeb5;
028000110228       endif;
028100110228       Open   fiar531c;
028200090827       //vtcpgm=SDSpgm;
028300090917       // Leggo tabella "TB" per caricare schiera dei codici bolla di recupero
028400090928       // conto servizio e assegnati
028500090917       i=0;
028600090917       setll (1:'TB') tabel00f  ;
028700090917       reade (1:'TB') tabel00f  ;
028800090917
028900090917  1    dow    not %eof(tabel00f)  ;
029000090918          dstb=t_tbluni           ;
029100090928          if §tbrbl='R' or §tbrbl='C' or §tbtpo='A';
029200090917             i=i+1;
029300090923             rec(i)=t_tblkey;
029400090917          endif;
029500090917          reade (1:'TB') tabel00f  ;
029600090917  1    enddo     ;
029700090917
029800090917       // Reperisco la data corrente
029900090917       dataiso_cor=(%date());
030000091005       dataeur_cor=dataiso_cor;
030100091005       wdate=%dec(dataeur_cor);
030200091005       utime= %dec(%time());
030300090402
030400090402       EndSr;
030500040108
030600090918       //-------------------------------------------------------------*
030700090918       //                                                             *
030800090918       //-------------------------------------------------------------*
030900090918       Begsr sr_gglav;
031000090918       // giorni lavorativi periodo da elaborare
031100090918       datada=st68dal;
031200090918       dataa=st68al;
031300090918       xsrlav8(wdat8);
031400090918       ggl_e=ggl;
031500091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
031600091201       if ggl_e=0;
031700091201          ggl_e=1;
031800091201       endif;
031900090918       // giorni lavorativi periodo di riferimento
032000090918       datada=st68rda;
032100090918       dataa=st68ral;
032200090918       xsrlav8(wdat8);
032300090918       ggl_r=ggl;
032400091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
032500091201       if ggl_r=0;
032600091201          ggl_r=1;
032700091201       endif;
032800090918       // imposto periodo minimo e max per lettura titas
032900090918       if st68dal<st68rda;
033000090918          w_aasmin=%dec(%subst(%editc(st68dal:'X'):1:4):4:0);
033100090922          w_mgsmin=%dec(%subst(%editc(ST68dal:'X'):5:4):4:0);
033101110819          w_aasdax=%dec(%subst(%editc(st68rda:'X'):1:4):4:0);
033102110819          w_mgsdax=%dec(%subst(%editc(ST68rda:'X'):5:4):4:0);
033200090918       else;
033300090918          w_aasmin=%dec(%subst(%editc(st68rda:'X'):1:4):4:0);
033400090922          w_mgsmin=%dec(%subst(%editc(st68rda:'X'):5:4):4:0);
033401110819          w_aasdax=%dec(%subst(%editc(st68dal:'X'):1:4):4:0);
033402110819          w_mgsdax=%dec(%subst(%editc(ST68dal:'X'):5:4):4:0);
033500090918       endif;
033600090922       if st68al>st68ral;
033700090918          w_aasmax=%dec(%subst(%editc(st68al:'X'):1:4):4:0);
033800090922          w_mgsmax=%dec(%subst(%editc(st68al:'X'):5:4):4:0);
033900090918       else;
034000090918          w_aasmax=%dec(%subst(%editc(st68ral:'X'):1:4):4:0);
034100090922          w_mgsmax=%dec(%subst(%editc(st68ral:'X'):5:4):4:0);
034200090918       endif;
034300090918       endsr;
034400090402       //-------------------------------------------------------------*
034500090402       //Reperimento Dati del job (Utente/Operativi)                  *
034600090402       //-------------------------------------------------------------*
034700090918       Begsr DatiJob;
034800090403
034900090917          in(e) §azute;
035000090917          if not %error;
035100090917           in(e) §datiute;
035200090917          endif;
035300090917          if %error or rsut = *blanks;
035400090917           tibs34r(tibs34ds);
035500090917           in §azute;
035600090917           in §datiute;
035700090917          endif;
035800090918          dute01=utefaf;
035900090918          wabi=§utegtc;
036000090917         endsr;
036100040108
036200090403       //-------------------------------------------------------------*
036300090916       //Preparazione stringa sql per Estrazione clienti da elaborare *
036400090403       //-------------------------------------------------------------*
036500090827       Begsr SR_prepSql;
036600090403
036700090916       WrkStringaSql='select * from cnaco00f, cnclp00f, tabel00f' +
036800090916                     ' where acokut=clpkut and acokcc=clpkcc and' +
036900090923                     ' acoksc=clpksc and clpage=tblkey and tblkut=1 and' +
037000090923                     ' tblcod=''01''';
037100090916       // richiesto un comm.le unificante specifico
037200090916       if st68age>0;
037300090916          WrkStringaSql=WrkStringaSql+
037400090929          ' and substr(tbluni, 26, 7)=' + '''' + %editc(st68age:'X')+ '''';
037500090916       else;
037600090916       // richiesta per area comm.le: preparo schiera delle filiali di appartenenza
037700090916          exsr sr_whrfil;
037800090916          WrkStringaSql=WrkStringaSql+WrkFilIn;
037900090916       endif;
038000090827
038100090827       // preparo schiera dei codici importanza clienti richiesti
038200090827       exsr sr_whrclv;
038300090827       WrkStringaSql=WrkStringaSql+WrkClvIn;
038400090403
038500090403       endsr;
038600090827       //-------------------------------------------------------------*
038700090916       //Schiera filiali di appartenenza all'area del comm.le unific. *
038800090827       //-------------------------------------------------------------*
038900090916       Begsr SR_whrfil;
039000101110          Primo= 1 ;
039100101110
039200090916          reset WrkFilIn;
039300090827          clear trul31ds  ;
039400090917          I31abi = wabi    ;
039500090917          I31cpo = DUTpou  ;
039600090827          trul31r(kpjba:trul31ds);
039700090922          WrkFilIn= ' And substr(tbluni, 26, 3) in (';
039800090922          for I=1 to %elem(pog);
039900090827             if pog(i)=0;
040000090827                leave;
040100090827             endif;
040200101110             if st68car>0   ;
040300101110              chain   pog(i) azorg01l  ;
040400101110             endif  ;
040500101110
040600101110             if st68car=0 or
040700101110               (%found(azorg01l) and   orgcar=st68car) ;
040800101110               if primo>1;
040900101110                 WrkFilIn=WrkFilIn + ' , ';
041000101110               endif;
041100101110             // se riicheista AREA, carico solo i p.o. dell'area richeista
041200090827             WrkFilIn=WrkFilIn + %char(pog(i));
041300101110             Primo=primo+1   ;
041400101110             endif;
041500090827          endfor;
041600090827          WrkFilIn=WrkFilIn + ')';
041700090827       endsr;
041800090827       //-------------------------------------------------------------*
041900090918       //Schiera codici importanza clienti da elaborare               *
042000090827       //-------------------------------------------------------------*
042100090827       Begsr SR_whrclv;
042200090929       wprimo='S';
042300090827       reset WrkClvIn;
042400090827       WrkclvIn= ' And clpclv in (';
042500090922          for I=1 to %elem(clv);
042600090929             if clv(i)<>*blanks;
042700090929                if wprimo<>'S';
042800090929                   WrkclvIn=WrkclvIn + ', ';
042900090929                endif;
043000090929                WrkCLVIn=WrkClvIn + '''' + clv(i) + '''';
043100090929                wprimo=' ';
043200090929             endif;
043300090827          endfor;
043400090929       WrkClvIn=WrkClvIn + ')';
043500090827       endsr;
043600090828       //-------------------------------------------------------------*
043700090828       //Lettura ed elaborazione dati estratti con sql                *
043800090828       //-------------------------------------------------------------*
043900090828       Begsr SR_Elabora;
044000090828        $finerec=*off;
044100090916        exec sql prepare STRINGASQL from :WrkStringaSql;
044200090916        exec sql declare ACOCsr cursor for StringaSql;
044300090916        exec sql open ACOcsr;
044400090828        dow $finerec=*off;
044500090916           exec sql Fetch ACOcsr into :cnacods, :cnclpds, :tabelds;
044600090828           if sqlcod=100 or sqlcod<0;
044700090828              $finerec = *on;
044800090828              leave;
044900090828           endif;
045000090916        // Scarto il cliente se non è padre o cliente singolo
045100090916           clear tibs10ds;
045200090916           d10tle='ST';
045300090916           d10paf='P';
045400090916           d10cod=acoksc;
045500090916           tibs10r(tibs10ds);
045600090916           if d10cop>0;
045700090916              iter;
045800090916           endif;
045900090916        // Recupero tutti i figli del cliente letto
046000090916           clear tibs10ds;
046100090916           d10tle='ST';
046200090916           d10paf='F';
046300090916           d10cod=acoksc;
046400090916           tibs10r(tibs10ds);
046500090916           if d10cop=0;
046600090930              skc(1)=%editc(d10cod:'X');
046700090916           endif;
046800110805        // Verifico se tutto il cliente bloccato: lo faccio prima
046900110805        //  perchè a rottura di linea di partenza scrivo file
047000110805           exsr sr_ctrBLO      ;
047100110805
047200110805        // Elaboro titas per ogni cliente caricato in schiera
047300090916           exsr sr_Etitas;
047400090916        enddo;
047500090916        endsr;
047600110805       //-------------------------------------------------------------*
047700110805       //Verifico se tutti i ksc del cliente unificannte soono bloccati
047800110805       //-------------------------------------------------------------*
047900110805       Begsr Sr_ctrBlo ;
048000110805
048100110805          wuniabl='S'  ;
048200110805       for I=1 to %elem(skc);
048300110805          if skc(i)=*blanks or skc(i)=*zeros;
048400110805             leave;
048500110805          endif;
048600110805          kccm=%dec(%subst(skc(I):5:7):7:0);
048700110805
048800110805          // Verifico se bloccato. se tutti bloccati imposto flag su file
048900110805           clear DS_cnaco;
049000110805           clear DS_cnind;
049100110805           clear DS_cnclp;
049200110805           clear DS_fncls;
049300110805           I69kac=kccm   ;
049400110805           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
049500110805
049600110805             if ds_acoabl<>'8';
049700110805              clear wuniabl ;
049800110805             endif  ;
049900110805       endfor;
050000110805
050100110805        ENDSR   ;
050200110805
050300090916       //-------------------------------------------------------------*
050400090916       //Elaborazione Titas per cliente mittente                      *
050500090916       //-------------------------------------------------------------*
050600090916       Begsr Sr_Etitas;
050700110805
050800090924          exsr sr_inz;
050900090924          clear w_taslnp;
051000090922       for I=1 to %elem(skc);
051100090923          if skc(i)=*blanks or skc(i)=*zeros;
051200090916             leave;
051300090916          endif;
051400090917            // routine di azzeramento campi di work
051500090916          kccm=%dec(%subst(skc(I):5:7):7:0);
051600110805
051700090917          setll (kccm: w_aasmin: w_mgsmin) titas33c;
051800090916          reade (kccm) titas33c  ;
051900090916
052000091201  1       dow not %eof(titas33c)  ;
052100091201             if tasaas>W_aasmax or (tasaas=W_aasmax and tasmgs>W_mgsMax);
052200090916                leave;
052300090916             endif;
052400091201           // includo bolla solo se rientra in uno dei due periodi
052500091201             if ((tasaas*10000+tasmgs)>=st68dal and
052600091201                (tasaas*10000+tasmgs)<=st68al) or
052700091201                ((tasaas*10000+tasmgs)>=st68rda and
052800091201                (tasaas*10000+tasmgs)<=st68ral);
052900090916           // Scarto bolla se figlia
053000090916             setll (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l;
053100090923             if not %equal(fnlbl01l);
053200090923                   // scarto reso, recupero, e conto servizio
053300090923                if %lookup(tastbl:rec)=0 and tasfbr=' ';
053400090923                      // OPERAZIONI A ROTTURA LINEA DI PARTENZA BOLLA
053500090923                   if taslnp<>w_taslnp;
053600090921                // Scrittura dati memorizzati nel file
053700090923                      if w_taslnp>0;
053800090923                         exsr sr_wrtmsp;
053900090923                      endif;
054000090923                      exsr sr_inz;
054100090923                      w_taslnp=taslnp;
054200090923                   endif;
054300090923                // MEMORIZZAZIONE DATI IN CAMPI DI COMODO
054400090923                   exsr sr_memdati;
054500090923                endif;
054600090923             endif;
054601110819             else;
054602110819           // se la bolla non rientra in nessuno dei due periodi
054603110819           // mi riposiziono a partire dalla data dal maggiore
054604110819             setll (kccm: w_aasdax: w_mgsdax) titas33c;
054605110819             reade (kccm) titas33c  ;
054606110819             iter;
054700091201             endif;
054800090916          reade (kccm) titas33c  ;
054900090916          enddo;
055000090916       endfor;
055100090917           // Scrittura dati memorizzati nel file
055200090923          if w_taslnp>0;
055300090917             exsr sr_wrtmsp;
055400090923          endif;
055500090916       ENDSR;
055600090828       //-------------------------------------------------------------*
055700090828       //Memorizzazione dati letti                                    *
055800090828       //-------------------------------------------------------------*
055900090828       Begsr SR_rscage ;
056000090828       clear w_rscage;
056100090828       tblkey=%editc(clpage:'X');
056200090828       chain (1:'01':tblkey) tabel00f  ;
056300090828       if %found(tabel00f);
056400090828          ds01=tbluni;
056500090828          w_rscage=§01age;
056600090828       endif;
056700090828       endsr;
056800090828       //-------------------------------------------------------------*
056900090828       //Memorizzazione dati letti                                    *
057000090828       //-------------------------------------------------------------*
057100090828       Begsr SR_memdati;
057200090828
057300090828       exsr sr_pesvol;
057400090917         // --> Se bancali devo sommare i colli originali
057500090828       clear w_collio;
057600090828       If %subst(tasGva:1:1) = 'O';
057700090828          exsr sr_bancali;
057800090828       endif;
057900090921
058000090917         // Dati del periodo di riferimento
058100090921
058200090917       if (tasaas*10000+tasmgs) >= st68rda and
058300090917          (tasaas*10000+tasmgs) <= st68ral;
058400090917          // peso
058500090917          w_pesor=w_pesor+w_tk;
058600090917          // volume
058700090917          w_volur=w_volur+w_tm;
058800090917          // spedizioni
058900090917          w_nspr+=1;
059000090917          // colli
059100090922          if w_collio>0;
059200090917             w_collir=w_collir+w_collio;
059300090917          else;
059400090917             w_collir=w_collir+tasncl;
059500090917          endif;
059600090917       endif;
059700090921
059800090917         // Dati del periodo da elaborare
059900090921
060000090917       if (tasaas*10000+tasmgs) >= st68dal and
060100090917          (tasaas*10000+tasmgs) <= st68al;
060200090917          // peso
060300090917          w_pesoe=w_pesoe+w_tk;
060400090917          // volume
060500090917          w_volue=w_volue+w_tm;
060600090917          // spedizioni
060700090917          w_nspe+=1;
060800090917          // colli
060900090917          if w_collio>0;
061000090917             w_collie=w_collie+w_collio;
061100090917          else;
061200090917             w_collie=w_collie+tasncl;
061300090917          endif;
061400090917       endif;
061500090917       endsr;
061600090828
061700090917       //-------------------------------------------------------------*
061800090917       //Scarico su file dei dati memorizzati                         *
061900090917       //-------------------------------------------------------------*
062000090917       Begsr SR_Wrtmsp;
062100090921        ds01=tbluni;
062200090921        if d10cop>0;
062300090921           w_clI=d10cop;
062400090921        else;
062500090921           w_Cli=acoksc;
062600090921        endif;
062700090922        chain (§01rgf: w_cli : w_taslnp) wfmsp01l;
062800090921        if not %found(wfmsp01l);
062900090922           clear wfmsp000;
063000090921        endif;
063100090918        // SPEDIZIONI
063200090921         MSPSPR+=w_nspr ;
063300090921         MSPSPP+=w_nspe ;
063400090918         // PESO
063500090921         MSPPKR+=w_pesor;
063600090921         MSPPKP+=w_pesoe;
063700090918         // COLLI
063800090921         MSPNCR+=w_collir;
063900090921         MSPNCP+=w_collie;
064000090918         // VOLUME
064100090921         MSPVLR+=w_volur;
064200090921         MSPVLP+=w_volue;
064300090921
064400090921       if %found(wfmsp01l);
064500090922          update wfmsp000;
064600090921       else;
064700090921          exsr sr_riefissi;
064800090922          write wfmsp000;
064900090921       endif;
065000090828
065100090828       endsr;
065200090921       //-------------------------------------------------------------*
065300090921       //Valorizzazione campi "fissi" WFMSP00F                         *
065400090921       //-------------------------------------------------------------*
065500090921       Begsr SR_Riefissi;
065600090921        MSPDEL=%dec(dataiso_cor);
065700090921        MSPUSR=knmus;
065800090921        MSPDAL=st68dal;
065900090921        MSPALL=st68al;
066000090921        MSPRDL=st68rda;
066100090921        MSPRAL=st68ral;
066200090921        MSPAGE=§01rgf ;
066300090921        MSPDAG=§01age ;
066400090922        kfil=%dec(%subst(%editc(§01rgf:'X'):1:3):3:0);
066500090921        chain (kfil) azorg01l;
066600090921        if %found(azorg01l);
066700090921           MSPDIV=orgfl3 ;
066800090921           MSPCAR=orgcar ;
066900090921        endif;
067000090921        t_tblkey=orgfl3;
067100090921        chain (1:'17':t_tblkey) tabel00f  ;
067200090921        if %found(tabel00f);
067300090921           ds17=t_tbluni;
067400090921           mspddi=§17des;
067500090921        endif;
067600090922        t_tblkey=%editc(orgcar:'X');
067700090921        chain (1:'05':t_tblkey) tabel00f  ;
067800090921        if %found(tabel00f);
067900090921           ds05=t_tbluni;
068000090921           mspdar=§05des;
068100090921        endif;
068200090921        if d10cop>0;
068300090921           MSPCLI=d10cop;
068400090921           clear DS_cnaco;
068500090921           clear DS_cnind;
068600090921           clear DS_cnclp;
068700090921           clear DS_fncls;
068800090921           I69kac=mspcli ;
068900090921           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
069000090921           msprag=ds_acorag;
069100090921        else;
069200090921           mspcli=acoksc;
069300090921           msprag=acorag;
069400090921        endif;
069500110805         MSPblo=wuniabl  ;
069600090921         MSPLNP=w_taslnp ;
069700090921         chain (taslnp) azorg01l;
069800090921         if %found(azorg01l);
069900090921            mspdla=orgdes ;
070000090921         endif;
070100090921         MSPCLV=clpclv ;
070200090921       endsr;
070300090828       //-------------------------------------------------------------*
070400090828       //Reperimento colli originali                                  *
070500090828       //-------------------------------------------------------------*
070600090828       Begsr SR_bancali;
070700090828       chain (tasaas:taslnp:tasnrs:tasnsp:'BNB') fiar531c;
070800090828       if %found(fiar531c) and ar5atb=*blanks;
070900090828          dar5bnb=ar5uni;
071000090828          w_collio=§ar5bcor;
071100090828       endif;
071200090922       endsr;
071300090828       //-------------------------------------------------------------*
071400090828       //Determino peso/volume da memorizzare                         *
071500090828       //-------------------------------------------------------------*
071600090828       Begsr SR_pesvol;
071700090828       clear w_tk;
071800090828       clear w_tm;
071900090828       // peso
072000090828       if tasncp=tasncl or taspkc>taspkf;
072100090828          w_tk=taspkc;
072200090828       else;
072300090828          w_tk=taspkf;
072400090828       endif;
072500090828       // volume
072600090828       if tasfvf='T';
072700090828          w_tm=tasvlf;
072800090828       else;
072900090828          if tasncl=tasncr;
073000090828                w_tm=tasvlc;
073100090828          else;
073200091008             if tasvlc>tasvlf;
073300090828                w_tm=tasvlc;
073400090828             else;
073500090828                w_tm=tasvlf;
073600090828             endif;
073700090828          endif;
073800090828       endif;
073900090922       endsr;
074000090917       //-------------------------------------------------------------*
074100090917       //Azzeramento campi di lavoro colli/peso/volume/spedizione     *
074200090917       //-------------------------------------------------------------*
074300090917       Begsr SR_inz;
074400090917       clear w_nspr;
074500090917       clear w_nspe;
074600090917       clear w_pesor;
074700090917       clear w_pesoe;
074800090917       clear w_volur;
074900090917       clear w_volue;
075000090917       clear w_collir;
075100090917       clear w_collie;
075200090917       endsr;
075300090922       //-------------------------------------------------------------*
075400090922       //Routine di aggiornamento file di work con medie e % di scost.*
075500090922       //-------------------------------------------------------------*
075600090922       Begsr SR_output;
075700090922
075800090929       clear savage1;
075900090929       clear savcli1;
076000091005       clear savrag1;
076100090929
076200090922       setll (*loval) wfmsp01l;
076300090922       read  wfmsp01l;
076400090922       dow not %eof(wfmsp01l);
076500090922       exsr sr_calcola;
076600091006       // a cambio di age o cli registro record di totale
076700091006       if st68crt=*blanks;
076800090929          if  (mspage<>savage1 or mspcli<>savcli1);
076900090929             if savage1>0;
077000090929                exsr sr_calcolat;
077100090929             endif;
077200091006             clear c_lnp;
077300090929             savage1=mspage;
077400090929             savcli1=mspcli;
077500091005             savrag1=msprag;
077600090929             clear t_mspspr;
077700090929             clear t_mspspp;
077800090929             clear t_msppkr;
077900090929             clear t_msppkp;
078000090929          endif;
078100090929          t_mspspr+=mspspr;
078200090929          t_mspspp+=mspspp;
078300090929          t_msppkr+=msppkr;
078400090929          t_msppkp+=msppkp;
078500091006          c_lnp+=1;
078600091006       endif;
078700090922       read  wfmsp01l;
078800090922       enddo;
078900091006       if st68crt=*blanks and (T_mspspr>0 or t_mspspp>0);
079000090929          exsr sr_calcolat;
079100090929       endif;
079200090922
079300090922       endsr;
079400090922       //-------------------------------------------------------------*
079500090922       //Preparo stringa sql per estrazione dati da WFMSP00F          *
079600090922       //-------------------------------------------------------------*
079700090922       Begsr sr_prepSqlMsp;
079800110805       WrkStringaSqlM='select distinct mspage, mspcli, msprag, mspblo from '+
079900110805                      ' wfmsp00f where msplnp=0 and';
080000090929       // limite % riferito a SPEDIZIONI MEDIE
080100090929       if st68rif='S';
080200090929         // limite percentuale negativo
080300090929         if st68sco<0;
080400090929            WrkStringaSqlM=WrkStringaSqlM+
080500090929            ' mspspc<' + %char(st68sco);
080600090929         else;
080700090929         // limite percentuale positivo
080800090929            WrkStringaSqlM=WrkStringaSqlM+
080900090929            ' mspspc>'+ %char(st68sco);
081000090929         endif;
081100090929       else;
081200090929       // limite % riferito a PESO MEDIO
081300090929         // limite percentuale negativo
081400090929         if st68sco<0;
081500090929            WrkStringaSqlM=WrkStringaSqlM+
081600090929            ' mspppc<' + %char(st68sco);
081700090929         else;
081800090929         // limite percentuale positivo
081900090929            WrkStringaSqlM=WrkStringaSqlM+
082000090929            ' mspppc>' + %char(st68sco);
082100090929         endif;
082200090929       endif;
082300090929
082400090929       // ordino per comm.le/cliente/lnp
082500090929            WrkStringaSqlM=WrkStringaSqlM+
082600091005            ' order by mspage, msprag, mspcli';
082700090929
082800090922       endsr;
082900090922       //-------------------------------------------------------------*
083000090922       //leggo dati file di work estratti con sql                     *
083100090922       //-------------------------------------------------------------*
083200090922       Begsr sr_elamsp1;
083300090922        $finerec=*off;
083400090922        exec sql prepare STRINGASQLM from :WrkStringaSqlM;
083500090922        exec sql declare MSPCsr cursor for StringaSqlM;
083600090922        exec sql open MSPcsr;
083700090922        dow $finerec=*off;
083800110805           exec sql Fetch MSPcsr into :mspage, :mspcli, :msprag, :mspblo;
083900090922           if sqlcod=100 or sqlcod<0;
084000090922              $finerec = *on;
084100090922              leave;
084200090922           endif;
084300110805
084400110805          // Se richiesto escludo i codici clienti tutti bloccati
084500110805          if st68abl=' ' or mspblo=' ' ;
084600110805             exsr sr_leggimsp;
084700110805          endif;
084800090922        enddo;
084900100615
085000090929        if savcli>0;
085100090929           if st68det='N';
085200090929              exsr sr_stampatot;
085300090929           else;
085400090929              exsr sr_rotcli;
085500100615           endif;
085600100615
085700100615             // Stampo i totali  del commerciale
085800100615                 exsr staTOTage  ;
085900090929        endif;
086000100615
086100100615        // totale generale solo se non richeisto 1 unificante
086200100615        if st68age=0  ;
086300100615                 exsr staTOTGEN  ;
086400100615         endif   ;
086500090922       endsr;
086600090922       //-------------------------------------------------------------*
086700090922       //                                                             *
086800090922       //-------------------------------------------------------------*
086900090922       Begsr sr_leggimsp;
087000090922        setll (mspage: mspcli) wfmsp01l;
087100090922        reade (mspage: mspcli) wfmsp01l;
087200090922  1     dow   not %eof(wfmsp01l)  ;
087300091006        // del record di totale prendo il numero di lnp per il cliente
087400091006          if msplnp=0;
087500091006             eval *in44=*off;
087600091006             w0030=%dec(%subst(mspusr:1:3):3:0);
087700091006             if w0030=1;
087800091006                eval *in44=*on;
087900091006             endif;
088000091006          else;
088100090929             if st68det='S';
088200090929                exsr sr_stampa;
088300090929             else;
088400090929                exsr sr_stampa1;
088500090929             endif;
088600090929          endif;
088700090922          reade (mspage: mspcli) wfmsp01l;
088800090922        enddo;
088900090922       endsr;
089000090922       //-------------------------------------------------------------*
089100090928       //leggo dati file di work - Richiesta senza limite % scostam.  *
089200090922       //-------------------------------------------------------------*
089300090922       Begsr sr_elamsp2;
089400091005       setll (*loval) wfmsp02l;
089500100111       read  wfmsp02l;
089600091005       dow not %eof(wfmsp02l);
089700090922
089800091006       if msplnp=0;
089900091006          eval *in44=*off;
090000091006          w0030=%dec(%subst(mspusr:1:3):3:0);
090100091006          if w0030=1;
090200091006             eval *in44=*on;
090300091006          endif;
090400091006       else;
090500091006          if st68det='S';
090600091006             exsr sr_stampa;
090700091006          else;
090800091006             exsr sr_stampa1;
090900091006          endif;
091000091006       endif;
091100090922
091200091005       read  wfmsp02l;
091300090922       enddo;
091400100615
091500090929       if savcli>0;
091600090929          if st68det='N';
091700090929             exsr sr_stampatot;
091800090929          else;
091900090929             exsr sr_rotcli;
092000090929           endif;
092100100615
092200100615             // Stampo i totali  del commerciale
092300100615                 exsr staTOTage  ;
092400090929       endif;
092500100615        // totale generale solo se non richeisto 1 unificante
092600100615        if st68age=0  ;
092700100615                 exsr staTOTGEN  ;
092800100615        endif  ;
092900090922       endsr;
093000090921       //-------------------------------------------------------------*
093100090921       //                                                             *
093200090921       //-------------------------------------------------------------*
093300090921       Begsr SR_calcola;
093400090921         // Calcolo spedizioni medie periodo di riferimento
093500090924         eval(H) MSPSMR=mspspr/ggl_r;
093600090924         eval(H) MSPSMP=mspspp/ggl_e ;
093700090921         // Calcolo la % di scostamento della media sped. del periodo rispetto
093800090921         // alla media spedizioni del periodo di riferimento
093900090923         if mspsmp>0 and mspsmp<>mspsmr and mspsmr>0;
094000090924            eval(H) MSPSPC=((mspsmp-mspsmr)*100)/mspsmr ;
094100090923         endif;
094200090923         if mspsmp<>mspsmr and mspsmp=0;
094300090923            mspspc=-100;
094400090923         endif;
094500090921         // calcolo il peso medio a spedizione
094600090923         if mspspr>0;
094700090924            eval(H) MSPPMR=msppkr/mspspr;
094800090923         endif;
094900090923         if mspspp>0;
095000090924            eval(H) MSPPMP=msppkp/mspspp ;
095100090923         endif;
095200090921         // Calcolo la % di scostamento del peso medio del periodo rispetto
095300090921         // al peso medio del periodo di riferimento
095400090923         if msppmp>0 and msppmp<>msppmr and msppmr>0;
095500090924            eval(H) mspppc=((msppmp-msppmr)*100)/msppmr;
095600090923         endif;
095700090923         if msppmp<>msppmr and msppmp=0;
095800090923            mspppc=-100;
095900090923         endif;
096000090921
096100090921         update wfmsp000;
096200090921
096300090921       endsr;
096400090928       //-------------------------------------------------------------*
096500090928       //                                                             *
096600090928       //-------------------------------------------------------------*
096700090928       Begsr SR_calcolat;
096800090929        clear t_mspsmr;
096900090929        clear t_mspsmp;
097000090929        clear t_mspspc;
097100090929        clear t_msppmr;
097200090929        clear t_msppmp;
097300090929        clear t_mspppc;
097400090928        exsr sr_calcolat_s;
097500090928        exsr sr_calcolat_p;
097600090929        // scrivo nel file di work record di totale
097700090929
097800090929        sav_wfmspds=wfmspds;
097900090929
098000090929        clear wfmsp000;
098100091006        // memorizzo se  per il cliente unificante sono presenti più lnp
098200091006        // servirà al momento della stampa per sapere se la riga deve essere
098300091006        // in grassetto oppure no
098400091006        eval mspusr=%editc(c_lnp:'X');
098500090929        mspage=savage1;
098600090929        mspcli=savcli1;
098700091005        msprag=savrag1;
098800090929        mspspr=t_mspspr;
098900090929        mspspp=t_mspspp;
099000090929        msppkr=t_msppkr;
099100090929        msppkp=t_msppkp;
099200090929        mspsmr=t_mspsmr;
099300090929        mspsmp=t_mspsmp;
099400090929        mspspc=t_mspspc;
099500090929        msppmr=t_msppmr;
099600090929        msppmp=t_msppmp;
099700090929        mspppc=t_mspppc;
099800090929        write wfmsp000;
099900090929
100000090929        wfmspds=sav_wfmspds;
100100090929
100200090928       endsr;
100300090928       //-------------------------------------------------------------*
100400090928       //                                                             *
100500090928       //-------------------------------------------------------------*
100600090928       Begsr SR_calcolat_s;
100700090928         // Calcolo spedizioni medie periodo di riferimento
100800091130         eval(H) t_msPSMR=t_mspspr/ggl_r;
100900091130         eval(H) t_MSPSMP=t_mspspp/ggl_e ;
101000090928         // Calcolo la % di scostamento della media sped. del periodo rispetto
101100090928         // alla media spedizioni del periodo di riferimento
101200090928         if t_mspsmp>0 and t_mspsmp<>t_mspsmr and t_mspsmr>0;
101300090928            eval(H) t_MSPSPC=((t_mspsmp-t_mspsmr)*100)/t_mspsmr ;
101400090928         endif;
101500090928         if t_mspsmp<>t_mspsmr and t_mspsmp=0;
101600090928            t_mspspc=-100;
101700090928         endif;
101800090928         endsr;
101900090928       //-------------------------------------------------------------*
102000090928       //                                                             *
102100090928       //-------------------------------------------------------------*
102200090928       Begsr SR_calcolat_p;
102300090928         // calcolo il peso medio a spedizione
102400090928         if t_mspspr>0;
102500090928            eval(H) t_MSPPMR=t_msppkr/t_mspspr;
102600090928         endif;
102700090928         if t_mspspp>0;
102800090928            eval(H) t_MSPPMP=t_msppkp/t_mspspp ;
102900090928         endif;
103000090928         // Calcolo la % di scostamento del peso medio del periodo rispetto
103100090928         // al peso medio del periodo di riferimento
103200090928         if t_msppmp>0 and t_msppmp<>t_msppmr and t_msppmr>0;
103300090928            eval(H) t_mspppc=((t_msppmp-t_msppmr)*100)/t_msppmr;
103400090928         endif;
103500090928         if t_msppmp<>t_msppmr and t_msppmp=0;
103600090928            t_mspppc=-100;
103700090928         endif;
103800090928       endsr;
103900090921       //-------------------------------------------------------------*
104000090925       // Stampa riga con dettaglio per lnp                           *
104100090921       //-------------------------------------------------------------*
104200090921       Begsr SR_stampa ;
104300090922       *in40=*off;
104400090922       *in41=*off;
104500090925       //Gestione rottura cliente unificante
104600090925       if mspcli<>savcli;
104700090925          exsr sr_rotcli;
104800090925       endif;
104900100615
105000090925       //Gestione rottura commerciale unificante
105100090922       if mspage<>savage;
105200090925          exsr sr_rotage;
105300090922       endif;
105400090922       plnp=msplnp;
105500090922       pspr=mspspr;
105600090922       pspp=mspspp;
105700090922       psmr=mspsmr;
105800090922       psmp=mspsmp;
105900100615       pcat=mspclv;
106000110808       clear pblo   ;
106100110808       if mspblo='S';
106200110808       pblo='BL'    ;
106300110808       endif        ;
106400100615
106500090924       if %abs(mspspc)>999,99;
106600090924          if mspspc>0;
106700090924             pspc=999,99;
106800090924          else;
106900090924             pspc=-999,99;
107000090924          endif;
107100090924       else;
107200090924          pspc=mspspc;
107300090924       endif;
107400090922       ppmr=msppmr;
107500090922       ppmp=msppmp;
107600090924       if %abs(mspppc)>999,99;
107700090924          if mspppc>0;
107800090924             pppc=999,99;
107900090924          else;
108000090924             pppc=-999,99;
108100090924          endif;
108200090924       else;
108300090924          pppc=mspppc;
108400090924       endif;
108500090922       c_lnp+=1;
108600090925       if $righe+1>57;
108700090925          exsr sr_overflow;
108800090925          write st68rv1;
108900090925          $righe+=1;
109000090923       endif;
109100090922       write st68r01;
109200090925       $righe+=1;
109300100615       savclv=mspclv   ;
109400110808       savblo=mspblo   ;
109500090922       exsr sr_imptot;
109600090921       endsr;
109700090925       //-------------------------------------------------------------*
109800090925       //Operazioni a rottura di codice cliente unificante            *
109900090925       //-------------------------------------------------------------*
110000090925       Begsr SR_rotcli;
110100090925          // stampa totale cliente precedente se aveva più lnp
110200090925          if c_lnp>1;
110300090925             exsr sr_caltot;
110400090925             write st68tot;
110500090925             $righe+=2;
110600090925          endif;
110700100615
110800090929          exsr sr_clrtot;
110900090925          *in41=*on;
111000090925          pkscuni=mspcli;
111100090925          pkscdes=msprag;
111200091006          tkscdes=msprag;
111300090928          pcat=mspclv;
111400110808          clear pblo  ;
111500110808          if mspblo='S'  ;
111600110808           pblo='BL'  ;
111700110808          endif  ;
111800090925          savcli=mspcli;
111900090925          clear c_lnp;
112000100615          if $righe+1<=60;
112100090925             write st68rv1;
112200090925             $righe+=1;
112300090925          endif;
112400090925       endsr;
112500090925       //-------------------------------------------------------------*
112600090925       //Operazioni a rottura di commerciale unificante               *
112700090925       //-------------------------------------------------------------*
112800090925       Begsr SR_rotage;
112900100615       // Eseguo stampa totale commerciale precedente
113000100615       if savage>0        ;
113100100615          exsr staTOTage  ;
113200100615       endif    ;
113300100615
113400090925          *in40=*on;
113500090925          pageuni=mspage;
113600090925          pagedes=mspdag;
113700090925       // Eseguo salto pagina se non è la prima pagina di stampa
113800090925          if savage>0;
113900090925             exsr sr_overflow;
114000090925             write st68rv1;
114100090925             $righe+=1;
114200090925          endif;
114300100615
114400090925          savage=mspage;
114500100615          savdes=mspdag;
114600090925       endsr;
114700090925       //-------------------------------------------------------------*
114800090925       // Stampa riga - no dettaglio per lnp                          *
114900090925       //-------------------------------------------------------------*
115000090925       Begsr SR_stampa1;
115100090925       *in40=*off;
115200090925       //Gestione rottura cliente unificante
115300090925       if mspcli<>savcli;
115400090925          if savcli>0;
115500090929             exsr sr_stampatot;
115600090925          endif;
115700090925          exsr sr_clrtot;
115800100614
115900100614             // Se cambiato cod commerciale, stampo i totali
116000100615              if mspage<>savage  and savage>0;
116100100614                 exsr staTOTage  ;
116200110825       //        exsr sr_clrtot;
116300100614              endif         ;
116400100614
116500090925          savcli=mspcli;
116600090925          savrag=msprag;
116700090925          savage=mspage;
116800090925          savdes=mspdag;
116900090928          savclv=mspclv;
117000110808          savblo=mspblo;
117100090925       endif;
117200100614
117300090925       exsr sr_imptot;
117400090925       endsr;
117500090925       //-------------------------------------------------------------*
117600090925       //Stampa intestazione per salto pagina                         *
117700090925       //-------------------------------------------------------------*
117800090925       Begsr SR_overflow;
117900090925          write st68t00;
118000110620          write st68t01;
118100090925          $righe=10;
118200090925          *in40=*on;
118300090925          *in41=*on;
118400090925       endsr;
118500090922       //-------------------------------------------------------------*
118600090922       //Routine di pulizia campi per stampa totale                   *
118700090922       //-------------------------------------------------------------*
118800090922       Begsr SR_clrtot;
118900090922             clear tspr;
119000090922             clear tspp;
119100090922             clear tsmr;
119200090922             clear tsmp;
119300090922             clear tspc;
119400090922             clear tpmr;
119500090922             clear tpmp;
119600090922             clear tppc;
119700090922             clear tpkr;
119800090922             clear tpkp;
119900090922       endsr;
120000090922       //-------------------------------------------------------------*
120100090922       //Routine di impostazione campi di stampa record di totale     *
120200090922       //-------------------------------------------------------------*
120300090922       Begsr SR_imptot;
120400090922
120500090925       tspr+=mspspr;
120600090925       tspp+=mspspp;
120700090922       tpkr+=msppkr;
120800090922       tpkp+=msppkp;
120900100614
121000100614        // sommo anche nelle skiere per tipo cliente
121100100614        select   ;
121200100614        when savclv='D' ;
121300100614        xx=1  ;
121400100614        when savclv='T' ;
121500100614        xx=2  ;
121600100614        when savclv='A' ;
121700100614        xx=3  ;
121800100614        other         ;
121900100614        xx=4  ;
122000100614        endsl   ;
122100090922
122200100614       Upr(xx)+=mspspr;
122300100614       Upp(xx)+=mspspp;
122400100614       Ukr(xx)+=msppkr;
122500100614       Ukp(xx)+=msppkp;
122600100614
122700090922       endsr;
122800090922       //-------------------------------------------------------------*
122900090922       //Routine di calcolo medie e % per riga totale                 *
123000090922       //-------------------------------------------------------------*
123100090922       Begsr SR_caltot;
123200090922         // Calcolo spedizioni medie periodo di riferimento
123300091130         eval(h) tsmr=tspr/ggl_r;
123400091130         eval(h) tSMP=tspp/ggl_e ;
123500100615
123600090922         // Calcolo la % di scostamento della media sped. del periodo rispetto
123700090922         // alla media spedizioni del periodo di riferimento
123800100615
123900090923         if tsmp>0 and tsmr>0 and tsmp<>tsmr;
124000090924            eval(h) w0122=((tsmp-tsmr)*100)/tsmr ;
124100090924            if %abs(w0122)>999,99;
124200090924               if w0122>0;
124300090924                  tspc=999,99;
124400090924               else;
124500090924                  tspc=-999,99;
124600090924               endif;
124700090924            else;
124800090924               tspc=w0122;
124900090924            endif;
125000090923         endif;
125100090923         if tsmp<>tsmr and tsmp=0;
125200090923            tSPC=-100;
125300090923         endif;
125400090922         // calcolo il peso medio a spedizione
125500090923         if tspr>0;
125600090924            eval(h) tPMR=tpkr/tspr;
125700090923         endif;
125800090923         if tspp>0;
125900090924            eval(h) tPMP=tpkp/tspp ;
126000090923         endif;
126100100615
126200090922         // Calcolo la % di scostamento del peso medio del periodo rispetto
126300090922         // al peso medio del periodo di riferimento
126400100615
126500090923         if tpmp>0 and tpmr>0 and tpmp<>tpmr;
126600090924            eval(h) w0142=((tpmp-tpmr)*100)/tpmr;
126700090924            if %abs(w0142)>999,99;
126800090924               if w0142>0;
126900090924                  tppc=999,99;
127000090924               else;
127100090924                  tppc=-999,99;
127200090924               endif;
127300090924            else;
127400090924               tppc=w0142;
127500090924            endif;
127600090923         endif;
127700090923         if tpmp<>tpmr and tpmp=0;
127800090923            tppc=-100;
127900090923         endif;
128000090922       endsr;
128100090929       //-------------------------------------------------------------*
128200090929       //Stampa totale cliente - stampa sintetica -                   *
128300090929       //-------------------------------------------------------------*
128400090929       Begsr SR_stampatot;
128500090929             // calcolo medie e %scostamento
128600090929             exsr sr_caltot;
128700090929             // verifico rottura commerciale
128800090929                if savage<>pre_age;
128900100614
129000100614
129100090929                   *in40=*on;
129200090929                   pageuni=savage;
129300090929                   pagedes=savdes;
129400090929                   // Eseguo salto pagina se non è la prima pagina di stampa
129500090929                   if $righe>10;
129600090929                      exsr sr_overflow;
129700090929                      write st68rv1;
129800090929                      $righe+=1;
129900090929                   else;
130000090929                      write st68rv1;
130100090929                      $righe+=1;
130200090929                   endif;
130300090929                   pre_age=savage;
130400090929                endif;
130500100614
130600090929                pkscuni=savcli;
130700090929                pkscdes=savrag;
130800090929                pcat   =savclv;
130900110808                clear pblo  ;
131000110808                if savblo='S'  ;
131100110808                 pblo='BL' ;
131200110808                endif  ;
131300100615                if $righe+2>60;
131400090929                   exsr sr_overflow;
131500090929                   write st68rv1;
131600090929                   $righe+=1;
131700090929                endif;
131800090929                write st68rt1;
131900090929                write st68rv1;
132000090929                $righe+=2;
132100090929       endsr;
132200100614       //-------------------------------------------------------------*
132300100615       //Stampa totale Agente
132400100614       //-------------------------------------------------------------*
132500100614       Begsr StaTOTage;
132600100614
132700100614       // La prima riga stampa anche il commerciale
132800100614       *in40=*on;
132900100614       pageuni=savage;
133000100614       pagedes=savdes;
133100100614
133200100615       // Faccio un ciclo di 4 per ogni tipo cliente
133300100614       xx=1   ;
1334001006141      dow xx<=4  ;
1335001006142      if Upr(xx)>0 or Upp(xx)>0 or Ukr(xx)>0 or Ukp(XX)>0 ;
133600100614       TSPR=Upr(xx)  ;
133700100614       TSPp=Upp(xx)  ;
133800100614       Tpkr=Ukr(xx)  ;
133900100614       Tpkp=Ukp(xx)  ;
134000100614
134100100615             // sommo ai totali generali
134200100615       Gupr(xx)+=Upr(xx)  ;
134300100615       Gupp(xx)+=Upp(xx)  ;
134400100615       Gukr(xx)+=Ukr(xx)  ;
134500100615       Gukp(xx)+=Ukp(xx)  ;
134600100615
134700100614             // calcolo medie e %scostamento
134800100614             exsr sr_caltot;
134900100614
135000100614        select   ;
135100100614        when xx=1       ;
135200100614        pcat='D';
135300100614        when xx=2       ;
135400100614        pcat='T';
135500100614        when xx=3       ;
135600100614        pcat='A';
135700100614        other         ;
135800100614        pcat='B';
135900100614        endsl   ;
136000100614
136100100614 3              if $righe+1>61;
136200100614                   exsr sr_overflow;
136300100614                   write st68rv1;
136400100614                   $righe+=1;
136500100614 3              endif;
136600100614
136700100614 3              if *in40 ;
136800100614                write st68tage1;
136900100614 3              endif   ;
137000100614                write st68tage2 ;
137100100614
137200100614 3              if *in40 ;
137300100614                $righe+=2;
137400100614                else  ;
137500100614                $righe+=1;
137600100614 3              endif  ;
137700100614
137800100614       *in40 =*off   ;
137900100614 2     endif   ;
138000100614
138100100614       xx=xx+1 ;
138200100614 1     enddo  ;
138300100615
138400100615       // Totale generale
138500100615       TSPR=%xfoot(Upr)   ;
138600100615       TSPp=%xfoot(Upp)  ;
138700100615       Tpkr=%Xfoot(Ukr)  ;
138800100615       Tpkp=%xfoot(Ukp)  ;
138900100615
139000100615             // calcolo medie e %scostamento
139100100615             exsr sr_caltot;
139200100615 3              if $righe+3>62;
139300100615                   exsr sr_overflow;
139400100615                   write st68rv1;
139500100615                   $righe+=1;
139600100615 3              endif;
139700100615
139800100615                *in40=*on   ;
139900100615                *in50=*on   ;
140000110808                clear pcat  ;
140100100615                pgenerale='TOTALE COMMERCIALE'   ;
140200100615                write st68tage1;
140300100615                write st68tage2 ;
140400100615                write st68tage1;
140500101221                $righe+=3;
140600100615                *in40=*oFF  ;
140700100615                *in50=*oFF  ;
140800100615
140900100615       clear upr       ;
141000100615       clear upp       ;
141100100615       clear ukr       ;
141200100615       clear ukp       ;
141201110825          exsr sr_clrtot;
141300100614
141400100614       endsr;
141500100615       //-------------------------------------------------------------*
141600100615       //Stampa totale GENERALE
141700100615       //-------------------------------------------------------------*
141800100615       Begsr StaTOTGEN;
141900100615
142000100615       // Eseguo salto pagina
142100100615             exsr sr_overflow;
142200100615             write st68rv1;
142300100615             $righe+=1;
142400100615
142500100615       *in40=*off ;
142600100615       write st68tage1;
142700101221       $righe+=1;
142800100615
142900100615       // Faccio un ciclo di 4 per ogni tipo cliente
143000100615       xx=1   ;
1431001006151      dow xx<=4  ;
1432001006152      if GUpr(xx)>0 or GUpp(xx)>0 or GUkr(xx)>0 or GUkp(XX)>0 ;
143300100615       TSPR=gUpr(xx)  ;
143400100615       TSPp=gUpp(xx)  ;
143500100615       Tpkr=gUkr(xx)  ;
143600100615       Tpkp=gUkp(xx)  ;
143700100615
143800100615             // calcolo medie e %scostamento
143900100615             exsr sr_caltot;
144000100615
144100100615        select   ;
144200100615        when xx=1       ;
144300100615        pcat='D';
144400100615        when xx=2       ;
144500100615        pcat='T';
144600100615        when xx=3       ;
144700100615        pcat='A';
144800100615        other         ;
144900100615        pcat='B';
145000100615        endsl   ;
145100100615
145200100615                write st68tage2 ;
145300100615                $righe+=1;
145400100615
145500100615 2     endif   ;
145600100615
145700100615       xx=xx+1 ;
145800100615 1     enddo  ;
145900100615
146000100615       // Totale generale
146100100615       TSPR=%xfoot(gUpr)   ;
146200100615       TSPp=%xfoot(gUpp)  ;
146300100615       Tpkr=%Xfoot(gUkr)  ;
146400100615       Tpkp=%xfoot(gUkp)  ;
146500100615
146600100615             // calcolo medie e %scostamento
146700100615             exsr sr_caltot;
146800100615
146900100615                *in50=*on   ;
147000100615                clear pcat  ;
147100100615                pgenerale='TOTALE GENERALE   '   ;
147200100615                write st68tage1;
147300100615                write st68tage2 ;
147400100615                write st68tage1;
147500101221                $righe+=3;
147600100615                *in40=*oFF  ;
147700100615                *in50=*oFF  ;
147800100615
147900100615       endsr;
148000090921       //-------------------------------------------------------------*
148100090921       //Routine di impostazione campi di stampa record di testata    *
148200090921       //-------------------------------------------------------------*
148300090921       Begsr SR_impt00;
148400090921
148500090921       // Titolo
148600090921
148700090921       dataeur=%date(st68dal:*iso);
148800090921       wdatagma=%dec(dataeur);
148900091007       ptitolo='Analisi Spedizioni dal';
149000090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
149100090921       dataeur=%date(st68al:*iso);
149200090921       wdatagma=%dec(dataeur);
149300090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
149400090921
149500090925       // Categoria cliente
149600090921
149700090929       wprimo='S';
149800090925       pcatcli ='Categoria clienti';
149900090921       if st68clv=*blanks;
150000090925          pcatcli=%trim(pcatcli)+ ' D/T/A/B';
150100090921       else;
150200090922          for I=1 to %elem(clv);
150300090929             if clv(i)<>*blanks;
150400090929                if wprimo<>'S';
150500090929                   pcatcli=%trim(pcatcli)+ '/';
150600090929                endif;
150700090929                if wprimo='S';
150800090929                   pcatcli=%trim(pcatcli)+ ' ' +clv(i);
150900090929                else;
151000090929                   pcatcli=%trim(pcatcli)+ clv(i);
151100090929                endif;
151200090929                wprimo=' ';
151300090929             endif;
151400090921          endfor;
151500090921       endif;
151600090921
151700090921       // Periodo di riferimento dal/al
151800090921
151900090921       dataeur=%date(st68rda:*iso);
152000090922       prifdal=%dec(dataeur);
152100090921       dataeur=%date(st68ral:*iso);
152200090922       prifal=%dec(dataeur);
152300090921
152400090921       // limite % di scostamento
152500090930       *in43=*off;
152600090930       if st68sco<>0;
152700090930          *in43=*on;
152800090921
152900090930          pscost=st68sco;
153000090921
153100090921       // Valore di riferimento %
153200090921
153300090930          select;
153400090930          when st68rif='S';
153500090930            prifper='Spedizioni';
153600090930          when st68rif='P';
153700090930            prifper='Peso medio';
153800090930          endsl;
153900090930       endif;
154000101110
154100101110       // area se richiesta
154200101110       if st68car>0   ;
154300101110       clear tblkey  ;
154400101110       eval tblkey=%editc(st68car:'X') ;
154500101110       chain (1:'05':tblkey) tabel00f  ;
154600101110         if not %found(tabel00f);
154700101110          clear t_tbluni   ;
154800101110         endif   ;
154900101110       eval priare='Area  ' + %editc(st68car:'X') +' ' + t_tbluni   ;
155000101110       endif  ;
155100101110
155200090922       endsr;
