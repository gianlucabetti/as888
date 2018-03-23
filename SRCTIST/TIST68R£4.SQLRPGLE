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
016300110819     d w_aasdax        s                   like(tasaas)
016400110819     d w_mgsdax        s                   like(tasmgs)
016500090917     d kccm            s                   like(tasccm)
016600090929     d sav_wfmspds     s                   like(wfmspds)
016700040108     d w0140           s             14  0
016800040123     d wDataAMG        s              8  0
016900040123     d wDataGMA        s              8  0
017000040120     d wOra            s              6  0
017100100614     d xx              s              4  0
017200101110     d Primo           s              4  0 inz
017300090414     d dataiso_cor     s               d   datfmt(*iso)
017400090414     d dataeur_cor     s               d   datfmt(*eur)
017500090416     d dataeur         s               d   datfmt(*eur)
017600090409     d datadmy         s               d   datfmt(*dmy)
017700090408     d kut             s                   like(acokut) inz(1)
017800090917     d wabi            s                   like(UteAut)
017900090918     d kfil            s                   like(orgfil)
018000090922     d Wtas33          s             21    inz('GAITRAGRU /TITAS33C')
018100090922     d War531          s             21    inz('GAITRAGRU /FIAR531C')
018200110228     D Wlibsede        S             21
018300110228     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
018400110228     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
018500110228     D WlibsedeP5      S             21    inz('GAITRAGRPS/FIAR531C')
018600110228     D WlibsedeB5      S             21    INZ('GAITRAGRU /FIAR531C')
018700090828      *
018800090828      * - Flags booleani
018900090828     d $Finerec        s              1    inz(*off)
019000081023
019100090402      // ? PROTOTIPI ?
019200090402      /copy gaitrasrc/srcprotopr,tibs34r
019300090827      /copy gaitrasrc/srcprotopr,trul31r
019400090828      /copy gaitrasrc/srcprotopr,tibs10r
019500090828      /copy gaitrasrc/srcprotopr,tibs69r
019600090918      /copy gaitrasrc/srcprotopr,xsrlav8
019700040107
019800040107      *===============================================================*
019900040107      *  RIEPILOGO INDICATORI                                         *
020000040107      *---------------------------------------------------------------*
020100090827      *  XX    -
020200040107      *===============================================================*
020300040107
020400040107     c     *Entry        plist
020500040107     c                   parm                    KPJBA
020600090827     c
020700090827     c                   movel     kpjbu         tist68ds
020800040123      *
020900090331      /free
021000090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021100090610
021200090925       // Se richiesto dettaglio per lnp attivo indicatore per stampa
021300090925       if st68det='S';
021400090925          *in42=*on;
021500090925       endif;
021600090827       // determino numero giorni lavorativi fra i due periodi dal/al
021700090917       // ed imposto aas/mgs minimo e max da elaborare
021800090827       exsr sr_gglav;
021900090916       // preparo stringa sql per estrazione codici clienti da elaborare
022000090827       exsr sr_prepSql;
022100090827       // elaboro dati estratti
022200090827       exsr sr_Elabora;
022300090925       // Calcolo le medie e le %
022400090921       exsr sr_output;
022500090922
022600090922       // OPERAZIONI PER LA STAMPA
022700110620       Select;
022800110620
022900110620         // - Lancio da Filiale
023000110620         When  st68crt <> 'S';
023100110620           open tist68p;
023200110620           // Stampo intestazione
023300110620           exsr sr_impt00;
023400110620           write st68t00;
023500110620           write st68t01;
023600110620           $righe=10;
023700110620           clear c_lnp;
023800110620           // preparo stringa sql per estrazione clienti da stampare in base al limite
023900110620           // percentuale
024000110620           if st68sco<>0;
024100110620              exsr sr_prepSqlMsp;
024200110620              exsr sr_elamsp1;
024300110620           else;
024400110620              exsr sr_elamsp2;
024500110620           endif;
024600110620           // Stampo *** Fine Stampa ***
024700110620           if $righe+2>60;
024800110620              exsr sr_overflow;
024900110620           endif;
025000110620           write fines;
025100110620
025200110620         // - Lancio da Sede per le Poste
025300110620         When  ST68crt = 'S'  and  KCDAZ = 'ST6H';
025400110620           open  TIST68P;
025500110620           // - Stampa intestazione
025600110620           exsr  sr_ImpT00;
025700110620           write ST68T00;
025800110620           // - Stampa "Elaborazione Terminata"
025900110620           write ST68END;
026000110620
026100110620       EndSl;
026200090922
026300090827       eval *inlr=*on;
026400040108
026500090402       //-------------------------------------------------------------*
026600090402       //Routine Iniziale                                             *
026700090402       //-------------------------------------------------------------*
026800090414       begsr *inzsr;
026900090402
027000090402       //Reperisco dati job
027100090917         exsr datijob;
027200110228       if %subst(knsif:7:1) = 'P';
027300110228         Wlibsede = Wlibsedep;
027400110228         Else ;
027500110228         wlibsede = Wlibsedeb;
027600110228       endif;
027700110228       Open   Titas33c;
027800110228       if %subst(knsif:7:1) = 'P';
027900110228         Wlibsede = Wlibsedep5;
028000110228         Else ;
028100110228         wlibsede = Wlibsedeb5;
028200110228       endif;
028300110228       Open   fiar531c;
028400090827       //vtcpgm=SDSpgm;
028500090917       // Leggo tabella "TB" per caricare schiera dei codici bolla di recupero
028600090928       // conto servizio e assegnati
028700090917       i=0;
028800090917       setll (1:'TB') tabel00f  ;
028900090917       reade (1:'TB') tabel00f  ;
029000090917
029100090917  1    dow    not %eof(tabel00f)  ;
029200090918          dstb=t_tbluni           ;
029300090928          if §tbrbl='R' or §tbrbl='C' or §tbtpo='A';
029400090917             i=i+1;
029500090923             rec(i)=t_tblkey;
029600090917          endif;
029700090917          reade (1:'TB') tabel00f  ;
029800090917  1    enddo     ;
029900090917
030000090917       // Reperisco la data corrente
030100090917       dataiso_cor=(%date());
030200091005       dataeur_cor=dataiso_cor;
030300091005       wdate=%dec(dataeur_cor);
030400091005       utime= %dec(%time());
030500090402
030600090402       EndSr;
030700040108
030800090918       //-------------------------------------------------------------*
030900090918       //                                                             *
031000090918       //-------------------------------------------------------------*
031100090918       Begsr sr_gglav;
031200090918       // giorni lavorativi periodo da elaborare
031300090918       datada=st68dal;
031400090918       dataa=st68al;
031500090918       xsrlav8(wdat8);
031600090918       ggl_e=ggl;
031700091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
031800091201       if ggl_e=0;
031900091201          ggl_e=1;
032000091201       endif;
032100090918       // giorni lavorativi periodo di riferimento
032200090918       datada=st68rda;
032300090918       dataa=st68ral;
032400090918       xsrlav8(wdat8);
032500090918       ggl_r=ggl;
032600091201       // Per sicurezza per evitare che si spacchi ma il filtro evita tale situazione
032700091201       if ggl_r=0;
032800091201          ggl_r=1;
032900091201       endif;
033000090918       // imposto periodo minimo e max per lettura titas
033100090918       if st68dal<st68rda;
033200090918          w_aasmin=%dec(%subst(%editc(st68dal:'X'):1:4):4:0);
033300090922          w_mgsmin=%dec(%subst(%editc(ST68dal:'X'):5:4):4:0);
033400110819          w_aasdax=%dec(%subst(%editc(st68rda:'X'):1:4):4:0);
033500110819          w_mgsdax=%dec(%subst(%editc(ST68rda:'X'):5:4):4:0);
033600090918       else;
033700090918          w_aasmin=%dec(%subst(%editc(st68rda:'X'):1:4):4:0);
033800090922          w_mgsmin=%dec(%subst(%editc(st68rda:'X'):5:4):4:0);
033900110819          w_aasdax=%dec(%subst(%editc(st68dal:'X'):1:4):4:0);
034000110819          w_mgsdax=%dec(%subst(%editc(ST68dal:'X'):5:4):4:0);
034100090918       endif;
034200090922       if st68al>st68ral;
034300090918          w_aasmax=%dec(%subst(%editc(st68al:'X'):1:4):4:0);
034400090922          w_mgsmax=%dec(%subst(%editc(st68al:'X'):5:4):4:0);
034500090918       else;
034600090918          w_aasmax=%dec(%subst(%editc(st68ral:'X'):1:4):4:0);
034700090922          w_mgsmax=%dec(%subst(%editc(st68ral:'X'):5:4):4:0);
034800090918       endif;
034900090918       endsr;
035000090402       //-------------------------------------------------------------*
035100090402       //Reperimento Dati del job (Utente/Operativi)                  *
035200090402       //-------------------------------------------------------------*
035300090918       Begsr DatiJob;
035400090403
035500090917          in(e) §azute;
035600090917          if not %error;
035700090917           in(e) §datiute;
035800090917          endif;
035900090917          if %error or rsut = *blanks;
036000090917           tibs34r(tibs34ds);
036100090917           in §azute;
036200090917           in §datiute;
036300090917          endif;
036400090918          dute01=utefaf;
036500090918          wabi=§utegtc;
036600090917         endsr;
036700040108
036800090403       //-------------------------------------------------------------*
036900090916       //Preparazione stringa sql per Estrazione clienti da elaborare *
037000090403       //-------------------------------------------------------------*
037100090827       Begsr SR_prepSql;
037200090403
037300090916       WrkStringaSql='select * from cnaco00f, cnclp00f, tabel00f' +
037400090916                     ' where acokut=clpkut and acokcc=clpkcc and' +
037500090923                     ' acoksc=clpksc and clpage=tblkey and tblkut=1 and' +
037600090923                     ' tblcod=''01''';
037700090916       // richiesto un comm.le unificante specifico
037800090916       if st68age>0;
037900090916          WrkStringaSql=WrkStringaSql+
038000090929          ' and substr(tbluni, 26, 7)=' + '''' + %editc(st68age:'X')+ '''';
038100090916       else;
038200090916       // richiesta per area comm.le: preparo schiera delle filiali di appartenenza
038300090916          exsr sr_whrfil;
038400090916          WrkStringaSql=WrkStringaSql+WrkFilIn;
038500090916       endif;
038600090827
038700090827       // preparo schiera dei codici importanza clienti richiesti
038800090827       exsr sr_whrclv;
038900090827       WrkStringaSql=WrkStringaSql+WrkClvIn;
039000090403
039100090403       endsr;
039200090827       //-------------------------------------------------------------*
039300090916       //Schiera filiali di appartenenza all'area del comm.le unific. *
039400090827       //-------------------------------------------------------------*
039500090916       Begsr SR_whrfil;
039600101110          Primo= 1 ;
039700101110
039800090916          reset WrkFilIn;
039900090827          clear trul31ds  ;
040000090917          I31abi = wabi    ;
040100090917          I31cpo = DUTpou  ;
040200090827          trul31r(kpjba:trul31ds);
040300090922          WrkFilIn= ' And substr(tbluni, 26, 3) in (';
040400090922          for I=1 to %elem(pog);
040500090827             if pog(i)=0;
040600090827                leave;
040700090827             endif;
040800101110             if st68car>0   ;
040900101110              chain   pog(i) azorg01l  ;
041000101110             endif  ;
041100101110
041200101110             if st68car=0 or
041300101110               (%found(azorg01l) and   orgcar=st68car) ;
041400101110               if primo>1;
041500101110                 WrkFilIn=WrkFilIn + ' , ';
041600101110               endif;
041700101110             // se riicheista AREA, carico solo i p.o. dell'area richeista
041800090827             WrkFilIn=WrkFilIn + %char(pog(i));
041900101110             Primo=primo+1   ;
042000101110             endif;
042100090827          endfor;
042200090827          WrkFilIn=WrkFilIn + ')';
042300090827       endsr;
042400090827       //-------------------------------------------------------------*
042500090918       //Schiera codici importanza clienti da elaborare               *
042600090827       //-------------------------------------------------------------*
042700090827       Begsr SR_whrclv;
042800090929       wprimo='S';
042900090827       reset WrkClvIn;
043000090827       WrkclvIn= ' And clpclv in (';
043100090922          for I=1 to %elem(clv);
043200090929             if clv(i)<>*blanks;
043300090929                if wprimo<>'S';
043400090929                   WrkclvIn=WrkclvIn + ', ';
043500090929                endif;
043600090929                WrkCLVIn=WrkClvIn + '''' + clv(i) + '''';
043700090929                wprimo=' ';
043800090929             endif;
043900090827          endfor;
044000090929       WrkClvIn=WrkClvIn + ')';
044100090827       endsr;
044200090828       //-------------------------------------------------------------*
044300090828       //Lettura ed elaborazione dati estratti con sql                *
044400090828       //-------------------------------------------------------------*
044500090828       Begsr SR_Elabora;
044600090828        $finerec=*off;
044700090916        exec sql prepare STRINGASQL from :WrkStringaSql;
044800090916        exec sql declare ACOCsr cursor for StringaSql;
044900090916        exec sql open ACOcsr;
045000090828        dow $finerec=*off;
045100090916           exec sql Fetch ACOcsr into :cnacods, :cnclpds, :tabelds;
045200090828           if sqlcod=100 or sqlcod<0;
045300090828              $finerec = *on;
045400090828              leave;
045500090828           endif;
045600090916        // Scarto il cliente se non è padre o cliente singolo
045700090916           clear tibs10ds;
045800090916           d10tle='ST';
045900090916           d10paf='P';
046000090916           d10cod=acoksc;
046100090916           tibs10r(tibs10ds);
046200090916           if d10cop>0;
046300090916              iter;
046400090916           endif;
046500090916        // Recupero tutti i figli del cliente letto
046600090916           clear tibs10ds;
046700090916           d10tle='ST';
046800090916           d10paf='F';
046900090916           d10cod=acoksc;
047000090916           tibs10r(tibs10ds);
047100090916           if d10cop=0;
047200090930              skc(1)=%editc(d10cod:'X');
047300090916           endif;
047400110805        // Verifico se tutto il cliente bloccato: lo faccio prima
047500110805        //  perchè a rottura di linea di partenza scrivo file
047600110805           exsr sr_ctrBLO      ;
047700110805
047800110805        // Elaboro titas per ogni cliente caricato in schiera
047900090916           exsr sr_Etitas;
048000090916        enddo;
048100090916        endsr;
048200110805       //-------------------------------------------------------------*
048300110805       //Verifico se tutti i ksc del cliente unificannte soono bloccati
048400110805       //-------------------------------------------------------------*
048500110805       Begsr Sr_ctrBlo ;
048600110805
048700110805          wuniabl='S'  ;
048800110805       for I=1 to %elem(skc);
048900110805          if skc(i)=*blanks or skc(i)=*zeros;
049000110805             leave;
049100110805          endif;
049200110805          kccm=%dec(%subst(skc(I):5:7):7:0);
049300110805
049400110805          // Verifico se bloccato. se tutti bloccati imposto flag su file
049500110805           clear DS_cnaco;
049600110805           clear DS_cnind;
049700110805           clear DS_cnclp;
049800110805           clear DS_fncls;
049900110805           I69kac=kccm   ;
050000110805           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
050100110805
050200110805             if ds_acoabl<>'8';
050300110805              clear wuniabl ;
050400110805             endif  ;
050500110805       endfor;
050600110805
050700110805        ENDSR   ;
050800110805
050900090916       //-------------------------------------------------------------*
051000090916       //Elaborazione Titas per cliente mittente                      *
051100090916       //-------------------------------------------------------------*
051200090916       Begsr Sr_Etitas;
051300110805
051400090924          exsr sr_inz;
051500090924          clear w_taslnp;
051600090922       for I=1 to %elem(skc);
051700090923          if skc(i)=*blanks or skc(i)=*zeros;
051800090916             leave;
051900090916          endif;
052000090917            // routine di azzeramento campi di work
052100090916          kccm=%dec(%subst(skc(I):5:7):7:0);
052200110805
052300090917          setll (kccm: w_aasmin: w_mgsmin) titas33c;
052400090916          reade (kccm) titas33c  ;
052500090916
052600091201  1       dow not %eof(titas33c)  ;
052700091201             if tasaas>W_aasmax or (tasaas=W_aasmax and tasmgs>W_mgsMax);
052800090916                leave;
052900090916             endif;
053000091201           // includo bolla solo se rientra in uno dei due periodi
053100091201             if ((tasaas*10000+tasmgs)>=st68dal and
053200091201                (tasaas*10000+tasmgs)<=st68al) or
053300091201                ((tasaas*10000+tasmgs)>=st68rda and
053400091201                (tasaas*10000+tasmgs)<=st68ral);
053500090916           // Scarto bolla se figlia
053600090916             setll (tasaas:taslnp:tasnrs:tasnsp) fnlbl01l;
053700090923             if not %equal(fnlbl01l);
053800090923                   // scarto reso, recupero, e conto servizio
053900090923                if %lookup(tastbl:rec)=0 and tasfbr=' ';
054000090923                      // OPERAZIONI A ROTTURA LINEA DI PARTENZA BOLLA
054100090923                   if taslnp<>w_taslnp;
054200090921                // Scrittura dati memorizzati nel file
054300090923                      if w_taslnp>0;
054400090923                         exsr sr_wrtmsp;
054500090923                      endif;
054600090923                      exsr sr_inz;
054700090923                      w_taslnp=taslnp;
054800090923                   endif;
054900090923                // MEMORIZZAZIONE DATI IN CAMPI DI COMODO
055000090923                   exsr sr_memdati;
055100090923                endif;
055200090923             endif;
055300110819             else;
055400110819           // se la bolla non rientra in nessuno dei due periodi
055500110819           // mi riposiziono a partire dalla data dal maggiore
055600110819             setll (kccm: w_aasdax: w_mgsdax) titas33c;
055700110819             reade (kccm) titas33c  ;
055800110819             iter;
055900091201             endif;
056000090916          reade (kccm) titas33c  ;
056100090916          enddo;
056200090916       endfor;
056300090917           // Scrittura dati memorizzati nel file
056400090923          if w_taslnp>0;
056500090917             exsr sr_wrtmsp;
056600090923          endif;
056700090916       ENDSR;
056800090828       //-------------------------------------------------------------*
056900090828       //Memorizzazione dati letti                                    *
057000090828       //-------------------------------------------------------------*
057100090828       Begsr SR_rscage ;
057200090828       clear w_rscage;
057300090828       tblkey=%editc(clpage:'X');
057400090828       chain (1:'01':tblkey) tabel00f  ;
057500090828       if %found(tabel00f);
057600090828          ds01=tbluni;
057700090828          w_rscage=§01age;
057800090828       endif;
057900090828       endsr;
058000090828       //-------------------------------------------------------------*
058100090828       //Memorizzazione dati letti                                    *
058200090828       //-------------------------------------------------------------*
058300090828       Begsr SR_memdati;
058400090828
058500090828       exsr sr_pesvol;
058600090917         // --> Se bancali devo sommare i colli originali
058700090828       clear w_collio;
058800090828       If %subst(tasGva:1:1) = 'O';
058900090828          exsr sr_bancali;
059000090828       endif;
059100090921
059200090917         // Dati del periodo di riferimento
059300090921
059400090917       if (tasaas*10000+tasmgs) >= st68rda and
059500090917          (tasaas*10000+tasmgs) <= st68ral;
059600090917          // peso
059700090917          w_pesor=w_pesor+w_tk;
059800090917          // volume
059900090917          w_volur=w_volur+w_tm;
060000090917          // spedizioni
060100090917          w_nspr+=1;
060200090917          // colli
060300090922          if w_collio>0;
060400090917             w_collir=w_collir+w_collio;
060500090917          else;
060600090917             w_collir=w_collir+tasncl;
060700090917          endif;
060800090917       endif;
060900090921
061000090917         // Dati del periodo da elaborare
061100090921
061200090917       if (tasaas*10000+tasmgs) >= st68dal and
061300090917          (tasaas*10000+tasmgs) <= st68al;
061400090917          // peso
061500090917          w_pesoe=w_pesoe+w_tk;
061600090917          // volume
061700090917          w_volue=w_volue+w_tm;
061800090917          // spedizioni
061900090917          w_nspe+=1;
062000090917          // colli
062100090917          if w_collio>0;
062200090917             w_collie=w_collie+w_collio;
062300090917          else;
062400090917             w_collie=w_collie+tasncl;
062500090917          endif;
062600090917       endif;
062700090917       endsr;
062800090828
062900090917       //-------------------------------------------------------------*
063000090917       //Scarico su file dei dati memorizzati                         *
063100090917       //-------------------------------------------------------------*
063200090917       Begsr SR_Wrtmsp;
063300090921        ds01=tbluni;
063400090921        if d10cop>0;
063500090921           w_clI=d10cop;
063600090921        else;
063700090921           w_Cli=acoksc;
063800090921        endif;
063900090922        chain (§01rgf: w_cli : w_taslnp) wfmsp01l;
064000090921        if not %found(wfmsp01l);
064100090922           clear wfmsp000;
064200090921        endif;
064300090918        // SPEDIZIONI
064400090921         MSPSPR+=w_nspr ;
064500090921         MSPSPP+=w_nspe ;
064600090918         // PESO
064700090921         MSPPKR+=w_pesor;
064800090921         MSPPKP+=w_pesoe;
064900090918         // COLLI
065000090921         MSPNCR+=w_collir;
065100090921         MSPNCP+=w_collie;
065200090918         // VOLUME
065300090921         MSPVLR+=w_volur;
065400090921         MSPVLP+=w_volue;
065500090921
065600090921       if %found(wfmsp01l);
065700090922          update wfmsp000;
065800090921       else;
065900090921          exsr sr_riefissi;
066000090922          write wfmsp000;
066100090921       endif;
066200090828
066300090828       endsr;
066400090921       //-------------------------------------------------------------*
066500090921       //Valorizzazione campi "fissi" WFMSP00F                         *
066600090921       //-------------------------------------------------------------*
066700090921       Begsr SR_Riefissi;
066800090921        MSPDEL=%dec(dataiso_cor);
066900090921        MSPUSR=knmus;
067000090921        MSPDAL=st68dal;
067100090921        MSPALL=st68al;
067200090921        MSPRDL=st68rda;
067300090921        MSPRAL=st68ral;
067400090921        MSPAGE=§01rgf ;
067500090921        MSPDAG=§01age ;
067600090922        kfil=%dec(%subst(%editc(§01rgf:'X'):1:3):3:0);
067700090921        chain (kfil) azorg01l;
067800090921        if %found(azorg01l);
067900090921           MSPDIV=orgfl3 ;
068000090921           MSPCAR=orgcar ;
068100090921        endif;
068200090921        t_tblkey=orgfl3;
068300090921        chain (1:'17':t_tblkey) tabel00f  ;
068400090921        if %found(tabel00f);
068500090921           ds17=t_tbluni;
068600090921           mspddi=§17des;
068700090921        endif;
068800090922        t_tblkey=%editc(orgcar:'X');
068900090921        chain (1:'05':t_tblkey) tabel00f  ;
069000090921        if %found(tabel00f);
069100090921           ds05=t_tbluni;
069200090921           mspdar=§05des;
069300090921        endif;
069400090921        if d10cop>0;
069500090921           MSPCLI=d10cop;
069600090921           clear DS_cnaco;
069700090921           clear DS_cnind;
069800090921           clear DS_cnclp;
069900090921           clear DS_fncls;
070000090921           I69kac=mspcli ;
070100090921           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
070200090921           msprag=ds_acorag;
070300090921        else;
070400090921           mspcli=acoksc;
070500090921           msprag=acorag;
070600090921        endif;
070700110805         MSPblo=wuniabl  ;
070800090921         MSPLNP=w_taslnp ;
070900120222         chain (w_taslnp) azorg01l;
071000090921         if %found(azorg01l);
071100090921            mspdla=orgdes ;
071200090921         endif;
071300090921         MSPCLV=clpclv ;
071400090921       endsr;
071500090828       //-------------------------------------------------------------*
071600090828       //Reperimento colli originali                                  *
071700090828       //-------------------------------------------------------------*
071800090828       Begsr SR_bancali;
071900090828       chain (tasaas:taslnp:tasnrs:tasnsp:'BNB') fiar531c;
072000090828       if %found(fiar531c) and ar5atb=*blanks;
072100090828          dar5bnb=ar5uni;
072200090828          w_collio=§ar5bcor;
072300090828       endif;
072400090922       endsr;
072500090828       //-------------------------------------------------------------*
072600090828       //Determino peso/volume da memorizzare                         *
072700090828       //-------------------------------------------------------------*
072800090828       Begsr SR_pesvol;
072900090828       clear w_tk;
073000090828       clear w_tm;
073100090828       // peso
073200090828       if tasncp=tasncl or taspkc>taspkf;
073300090828          w_tk=taspkc;
073400090828       else;
073500090828          w_tk=taspkf;
073600090828       endif;
073700090828       // volume
073800090828       if tasfvf='T';
073900090828          w_tm=tasvlf;
074000090828       else;
074100090828          if tasncl=tasncr;
074200090828                w_tm=tasvlc;
074300090828          else;
074400091008             if tasvlc>tasvlf;
074500090828                w_tm=tasvlc;
074600090828             else;
074700090828                w_tm=tasvlf;
074800090828             endif;
074900090828          endif;
075000090828       endif;
075100090922       endsr;
075200090917       //-------------------------------------------------------------*
075300090917       //Azzeramento campi di lavoro colli/peso/volume/spedizione     *
075400090917       //-------------------------------------------------------------*
075500090917       Begsr SR_inz;
075600090917       clear w_nspr;
075700090917       clear w_nspe;
075800090917       clear w_pesor;
075900090917       clear w_pesoe;
076000090917       clear w_volur;
076100090917       clear w_volue;
076200090917       clear w_collir;
076300090917       clear w_collie;
076400090917       endsr;
076500090922       //-------------------------------------------------------------*
076600090922       //Routine di aggiornamento file di work con medie e % di scost.*
076700090922       //-------------------------------------------------------------*
076800090922       Begsr SR_output;
076900090922
077000090929       clear savage1;
077100090929       clear savcli1;
077200091005       clear savrag1;
077300090929
077400090922       setll (*loval) wfmsp01l;
077500090922       read  wfmsp01l;
077600090922       dow not %eof(wfmsp01l);
077700090922       exsr sr_calcola;
077800091006       // a cambio di age o cli registro record di totale
077900091006       if st68crt=*blanks;
078000090929          if  (mspage<>savage1 or mspcli<>savcli1);
078100090929             if savage1>0;
078200090929                exsr sr_calcolat;
078300090929             endif;
078400091006             clear c_lnp;
078500090929             savage1=mspage;
078600090929             savcli1=mspcli;
078700091005             savrag1=msprag;
078800090929             clear t_mspspr;
078900090929             clear t_mspspp;
079000090929             clear t_msppkr;
079100090929             clear t_msppkp;
079200090929          endif;
079300090929          t_mspspr+=mspspr;
079400090929          t_mspspp+=mspspp;
079500090929          t_msppkr+=msppkr;
079600090929          t_msppkp+=msppkp;
079700091006          c_lnp+=1;
079800091006       endif;
079900090922       read  wfmsp01l;
080000090922       enddo;
080100091006       if st68crt=*blanks and (T_mspspr>0 or t_mspspp>0);
080200090929          exsr sr_calcolat;
080300090929       endif;
080400090922
080500090922       endsr;
080600090922       //-------------------------------------------------------------*
080700090922       //Preparo stringa sql per estrazione dati da WFMSP00F          *
080800090922       //-------------------------------------------------------------*
080900090922       Begsr sr_prepSqlMsp;
081000110805       WrkStringaSqlM='select distinct mspage, mspcli, msprag, mspblo from '+
081100110805                      ' wfmsp00f where msplnp=0 and';
081200090929       // limite % riferito a SPEDIZIONI MEDIE
081300090929       if st68rif='S';
081400090929         // limite percentuale negativo
081500090929         if st68sco<0;
081600090929            WrkStringaSqlM=WrkStringaSqlM+
081700090929            ' mspspc<' + %char(st68sco);
081800090929         else;
081900090929         // limite percentuale positivo
082000090929            WrkStringaSqlM=WrkStringaSqlM+
082100090929            ' mspspc>'+ %char(st68sco);
082200090929         endif;
082300090929       else;
082400090929       // limite % riferito a PESO MEDIO
082500090929         // limite percentuale negativo
082600090929         if st68sco<0;
082700090929            WrkStringaSqlM=WrkStringaSqlM+
082800090929            ' mspppc<' + %char(st68sco);
082900090929         else;
083000090929         // limite percentuale positivo
083100090929            WrkStringaSqlM=WrkStringaSqlM+
083200090929            ' mspppc>' + %char(st68sco);
083300090929         endif;
083400090929       endif;
083500090929
083600090929       // ordino per comm.le/cliente/lnp
083700090929            WrkStringaSqlM=WrkStringaSqlM+
083800091005            ' order by mspage, msprag, mspcli';
083900090929
084000090922       endsr;
084100090922       //-------------------------------------------------------------*
084200090922       //leggo dati file di work estratti con sql                     *
084300090922       //-------------------------------------------------------------*
084400090922       Begsr sr_elamsp1;
084500090922        $finerec=*off;
084600090922        exec sql prepare STRINGASQLM from :WrkStringaSqlM;
084700090922        exec sql declare MSPCsr cursor for StringaSqlM;
084800090922        exec sql open MSPcsr;
084900090922        dow $finerec=*off;
085000110805           exec sql Fetch MSPcsr into :mspage, :mspcli, :msprag, :mspblo;
085100090922           if sqlcod=100 or sqlcod<0;
085200090922              $finerec = *on;
085300090922              leave;
085400090922           endif;
085500110805
085600110805          // Se richiesto escludo i codici clienti tutti bloccati
085700110805          if st68abl=' ' or mspblo=' ' ;
085800110805             exsr sr_leggimsp;
085900110805          endif;
086000090922        enddo;
086100100615
086200090929        if savcli>0;
086300090929           if st68det='N';
086400090929              exsr sr_stampatot;
086500090929           else;
086600090929              exsr sr_rotcli;
086700100615           endif;
086800100615
086900100615             // Stampo i totali  del commerciale
087000100615                 exsr staTOTage  ;
087100090929        endif;
087200100615
087300100615        // totale generale solo se non richeisto 1 unificante
087400100615        if st68age=0  ;
087500100615                 exsr staTOTGEN  ;
087600100615         endif   ;
087700090922       endsr;
087800090922       //-------------------------------------------------------------*
087900090922       //                                                             *
088000090922       //-------------------------------------------------------------*
088100090922       Begsr sr_leggimsp;
088200090922        setll (mspage: mspcli) wfmsp01l;
088300090922        reade (mspage: mspcli) wfmsp01l;
088400090922  1     dow   not %eof(wfmsp01l)  ;
088500091006        // del record di totale prendo il numero di lnp per il cliente
088600091006          if msplnp=0;
088700091006             eval *in44=*off;
088800091006             w0030=%dec(%subst(mspusr:1:3):3:0);
088900091006             if w0030=1;
089000091006                eval *in44=*on;
089100091006             endif;
089200091006          else;
089300090929             if st68det='S';
089400090929                exsr sr_stampa;
089500090929             else;
089600090929                exsr sr_stampa1;
089700090929             endif;
089800090929          endif;
089900090922          reade (mspage: mspcli) wfmsp01l;
090000090922        enddo;
090100090922       endsr;
090200090922       //-------------------------------------------------------------*
090300090928       //leggo dati file di work - Richiesta senza limite % scostam.  *
090400090922       //-------------------------------------------------------------*
090500090922       Begsr sr_elamsp2;
090600091005       setll (*loval) wfmsp02l;
090700100111       read  wfmsp02l;
090800091005       dow not %eof(wfmsp02l);
090900090922
091000091006       if msplnp=0;
091100091006          eval *in44=*off;
091200091006          w0030=%dec(%subst(mspusr:1:3):3:0);
091300091006          if w0030=1;
091400091006             eval *in44=*on;
091500091006          endif;
091600091006       else;
091700091006          if st68det='S';
091800091006             exsr sr_stampa;
091900091006          else;
092000091006             exsr sr_stampa1;
092100091006          endif;
092200091006       endif;
092300090922
092400091005       read  wfmsp02l;
092500090922       enddo;
092600100615
092700090929       if savcli>0;
092800090929          if st68det='N';
092900090929             exsr sr_stampatot;
093000090929          else;
093100090929             exsr sr_rotcli;
093200090929           endif;
093300100615
093400100615             // Stampo i totali  del commerciale
093500100615                 exsr staTOTage  ;
093600090929       endif;
093700100615        // totale generale solo se non richeisto 1 unificante
093800100615        if st68age=0  ;
093900100615                 exsr staTOTGEN  ;
094000100615        endif  ;
094100090922       endsr;
094200090921       //-------------------------------------------------------------*
094300090921       //                                                             *
094400090921       //-------------------------------------------------------------*
094500090921       Begsr SR_calcola;
094600090921         // Calcolo spedizioni medie periodo di riferimento
094700090924         eval(H) MSPSMR=mspspr/ggl_r;
094800090924         eval(H) MSPSMP=mspspp/ggl_e ;
094900090921         // Calcolo la % di scostamento della media sped. del periodo rispetto
095000090921         // alla media spedizioni del periodo di riferimento
095100090923         if mspsmp>0 and mspsmp<>mspsmr and mspsmr>0;
095200090924            eval(H) MSPSPC=((mspsmp-mspsmr)*100)/mspsmr ;
095300090923         endif;
095400090923         if mspsmp<>mspsmr and mspsmp=0;
095500090923            mspspc=-100;
095600090923         endif;
095700090921         // calcolo il peso medio a spedizione
095800090923         if mspspr>0;
095900090924            eval(H) MSPPMR=msppkr/mspspr;
096000090923         endif;
096100090923         if mspspp>0;
096200090924            eval(H) MSPPMP=msppkp/mspspp ;
096300090923         endif;
096400090921         // Calcolo la % di scostamento del peso medio del periodo rispetto
096500090921         // al peso medio del periodo di riferimento
096600090923         if msppmp>0 and msppmp<>msppmr and msppmr>0;
096700090924            eval(H) mspppc=((msppmp-msppmr)*100)/msppmr;
096800090923         endif;
096900090923         if msppmp<>msppmr and msppmp=0;
097000090923            mspppc=-100;
097100090923         endif;
097200090921
097300090921         update wfmsp000;
097400090921
097500090921       endsr;
097600090928       //-------------------------------------------------------------*
097700090928       //                                                             *
097800090928       //-------------------------------------------------------------*
097900090928       Begsr SR_calcolat;
098000090929        clear t_mspsmr;
098100090929        clear t_mspsmp;
098200090929        clear t_mspspc;
098300090929        clear t_msppmr;
098400090929        clear t_msppmp;
098500090929        clear t_mspppc;
098600090928        exsr sr_calcolat_s;
098700090928        exsr sr_calcolat_p;
098800090929        // scrivo nel file di work record di totale
098900090929
099000090929        sav_wfmspds=wfmspds;
099100090929
099200090929        clear wfmsp000;
099300091006        // memorizzo se  per il cliente unificante sono presenti più lnp
099400091006        // servirà al momento della stampa per sapere se la riga deve essere
099500091006        // in grassetto oppure no
099600091006        eval mspusr=%editc(c_lnp:'X');
099700090929        mspage=savage1;
099800090929        mspcli=savcli1;
099900091005        msprag=savrag1;
100000090929        mspspr=t_mspspr;
100100090929        mspspp=t_mspspp;
100200090929        msppkr=t_msppkr;
100300090929        msppkp=t_msppkp;
100400090929        mspsmr=t_mspsmr;
100500090929        mspsmp=t_mspsmp;
100600090929        mspspc=t_mspspc;
100700090929        msppmr=t_msppmr;
100800090929        msppmp=t_msppmp;
100900090929        mspppc=t_mspppc;
101000090929        write wfmsp000;
101100090929
101200090929        wfmspds=sav_wfmspds;
101300090929
101400090928       endsr;
101500090928       //-------------------------------------------------------------*
101600090928       //                                                             *
101700090928       //-------------------------------------------------------------*
101800090928       Begsr SR_calcolat_s;
101900090928         // Calcolo spedizioni medie periodo di riferimento
102000091130         eval(H) t_msPSMR=t_mspspr/ggl_r;
102100091130         eval(H) t_MSPSMP=t_mspspp/ggl_e ;
102200090928         // Calcolo la % di scostamento della media sped. del periodo rispetto
102300090928         // alla media spedizioni del periodo di riferimento
102400090928         if t_mspsmp>0 and t_mspsmp<>t_mspsmr and t_mspsmr>0;
102500090928            eval(H) t_MSPSPC=((t_mspsmp-t_mspsmr)*100)/t_mspsmr ;
102600090928         endif;
102700090928         if t_mspsmp<>t_mspsmr and t_mspsmp=0;
102800090928            t_mspspc=-100;
102900090928         endif;
103000090928         endsr;
103100090928       //-------------------------------------------------------------*
103200090928       //                                                             *
103300090928       //-------------------------------------------------------------*
103400090928       Begsr SR_calcolat_p;
103500090928         // calcolo il peso medio a spedizione
103600090928         if t_mspspr>0;
103700090928            eval(H) t_MSPPMR=t_msppkr/t_mspspr;
103800090928         endif;
103900090928         if t_mspspp>0;
104000090928            eval(H) t_MSPPMP=t_msppkp/t_mspspp ;
104100090928         endif;
104200090928         // Calcolo la % di scostamento del peso medio del periodo rispetto
104300090928         // al peso medio del periodo di riferimento
104400090928         if t_msppmp>0 and t_msppmp<>t_msppmr and t_msppmr>0;
104500090928            eval(H) t_mspppc=((t_msppmp-t_msppmr)*100)/t_msppmr;
104600090928         endif;
104700090928         if t_msppmp<>t_msppmr and t_msppmp=0;
104800090928            t_mspppc=-100;
104900090928         endif;
105000090928       endsr;
105100090921       //-------------------------------------------------------------*
105200090925       // Stampa riga con dettaglio per lnp                           *
105300090921       //-------------------------------------------------------------*
105400090921       Begsr SR_stampa ;
105500090922       *in40=*off;
105600090922       *in41=*off;
105700090925       //Gestione rottura cliente unificante
105800090925       if mspcli<>savcli;
105900090925          exsr sr_rotcli;
106000090925       endif;
106100100615
106200090925       //Gestione rottura commerciale unificante
106300090922       if mspage<>savage;
106400090925          exsr sr_rotage;
106500090922       endif;
106600090922       plnp=msplnp;
106700090922       pspr=mspspr;
106800090922       pspp=mspspp;
106900090922       psmr=mspsmr;
107000090922       psmp=mspsmp;
107100100615       pcat=mspclv;
107200110808       clear pblo   ;
107300110808       if mspblo='S';
107400110808       pblo='BL'    ;
107500110808       endif        ;
107600100615
107700090924       if %abs(mspspc)>999,99;
107800090924          if mspspc>0;
107900090924             pspc=999,99;
108000090924          else;
108100090924             pspc=-999,99;
108200090924          endif;
108300090924       else;
108400090924          pspc=mspspc;
108500090924       endif;
108600090922       ppmr=msppmr;
108700090922       ppmp=msppmp;
108800090924       if %abs(mspppc)>999,99;
108900090924          if mspppc>0;
109000090924             pppc=999,99;
109100090924          else;
109200090924             pppc=-999,99;
109300090924          endif;
109400090924       else;
109500090924          pppc=mspppc;
109600090924       endif;
109700090922       c_lnp+=1;
109800090925       if $righe+1>57;
109900090925          exsr sr_overflow;
110000090925          write st68rv1;
110100090925          $righe+=1;
110200090923       endif;
110300090922       write st68r01;
110400090925       $righe+=1;
110500100615       savclv=mspclv   ;
110600110808       savblo=mspblo   ;
110700090922       exsr sr_imptot;
110800090921       endsr;
110900090925       //-------------------------------------------------------------*
111000090925       //Operazioni a rottura di codice cliente unificante            *
111100090925       //-------------------------------------------------------------*
111200090925       Begsr SR_rotcli;
111300090925          // stampa totale cliente precedente se aveva più lnp
111400090925          if c_lnp>1;
111500090925             exsr sr_caltot;
111600090925             write st68tot;
111700090925             $righe+=2;
111800090925          endif;
111900100615
112000090929          exsr sr_clrtot;
112100090925          *in41=*on;
112200090925          pkscuni=mspcli;
112300090925          pkscdes=msprag;
112400091006          tkscdes=msprag;
112500090928          pcat=mspclv;
112600110808          clear pblo  ;
112700110808          if mspblo='S'  ;
112800110808           pblo='BL'  ;
112900110808          endif  ;
113000090925          savcli=mspcli;
113100090925          clear c_lnp;
113200100615          if $righe+1<=60;
113300090925             write st68rv1;
113400090925             $righe+=1;
113500090925          endif;
113600090925       endsr;
113700090925       //-------------------------------------------------------------*
113800090925       //Operazioni a rottura di commerciale unificante               *
113900090925       //-------------------------------------------------------------*
114000090925       Begsr SR_rotage;
114100100615       // Eseguo stampa totale commerciale precedente
114200100615       if savage>0        ;
114300100615          exsr staTOTage  ;
114400100615       endif    ;
114500100615
114600090925          *in40=*on;
114700090925          pageuni=mspage;
114800090925          pagedes=mspdag;
114900090925       // Eseguo salto pagina se non è la prima pagina di stampa
115000090925          if savage>0;
115100090925             exsr sr_overflow;
115200090925             write st68rv1;
115300090925             $righe+=1;
115400090925          endif;
115500100615
115600090925          savage=mspage;
115700100615          savdes=mspdag;
115800090925       endsr;
115900090925       //-------------------------------------------------------------*
116000090925       // Stampa riga - no dettaglio per lnp                          *
116100090925       //-------------------------------------------------------------*
116200090925       Begsr SR_stampa1;
116300090925       *in40=*off;
116400090925       //Gestione rottura cliente unificante
116500090925       if mspcli<>savcli;
116600090925          if savcli>0;
116700090929             exsr sr_stampatot;
116800090925          endif;
116900090925          exsr sr_clrtot;
117000100614
117100100614             // Se cambiato cod commerciale, stampo i totali
117200100615              if mspage<>savage  and savage>0;
117300100614                 exsr staTOTage  ;
117400110825       //        exsr sr_clrtot;
117500100614              endif         ;
117600100614
117700090925          savcli=mspcli;
117800090925          savrag=msprag;
117900090925          savage=mspage;
118000090925          savdes=mspdag;
118100090928          savclv=mspclv;
118200110808          savblo=mspblo;
118300090925       endif;
118400100614
118500090925       exsr sr_imptot;
118600090925       endsr;
118700090925       //-------------------------------------------------------------*
118800090925       //Stampa intestazione per salto pagina                         *
118900090925       //-------------------------------------------------------------*
119000090925       Begsr SR_overflow;
119100090925          write st68t00;
119200110620          write st68t01;
119300090925          $righe=10;
119400090925          *in40=*on;
119500090925          *in41=*on;
119600090925       endsr;
119700090922       //-------------------------------------------------------------*
119800090922       //Routine di pulizia campi per stampa totale                   *
119900090922       //-------------------------------------------------------------*
120000090922       Begsr SR_clrtot;
120100090922             clear tspr;
120200090922             clear tspp;
120300090922             clear tsmr;
120400090922             clear tsmp;
120500090922             clear tspc;
120600090922             clear tpmr;
120700090922             clear tpmp;
120800090922             clear tppc;
120900090922             clear tpkr;
121000090922             clear tpkp;
121100090922       endsr;
121200090922       //-------------------------------------------------------------*
121300090922       //Routine di impostazione campi di stampa record di totale     *
121400090922       //-------------------------------------------------------------*
121500090922       Begsr SR_imptot;
121600090922
121700090925       tspr+=mspspr;
121800090925       tspp+=mspspp;
121900090922       tpkr+=msppkr;
122000090922       tpkp+=msppkp;
122100100614
122200100614        // sommo anche nelle skiere per tipo cliente
122300100614        select   ;
122400100614        when savclv='D' ;
122500100614        xx=1  ;
122600100614        when savclv='T' ;
122700100614        xx=2  ;
122800100614        when savclv='A' ;
122900100614        xx=3  ;
123000100614        other         ;
123100100614        xx=4  ;
123200100614        endsl   ;
123300090922
123400100614       Upr(xx)+=mspspr;
123500100614       Upp(xx)+=mspspp;
123600100614       Ukr(xx)+=msppkr;
123700100614       Ukp(xx)+=msppkp;
123800100614
123900090922       endsr;
124000090922       //-------------------------------------------------------------*
124100090922       //Routine di calcolo medie e % per riga totale                 *
124200090922       //-------------------------------------------------------------*
124300090922       Begsr SR_caltot;
124400090922         // Calcolo spedizioni medie periodo di riferimento
124500091130         eval(h) tsmr=tspr/ggl_r;
124600091130         eval(h) tSMP=tspp/ggl_e ;
124700100615
124800090922         // Calcolo la % di scostamento della media sped. del periodo rispetto
124900090922         // alla media spedizioni del periodo di riferimento
125000100615
125100090923         if tsmp>0 and tsmr>0 and tsmp<>tsmr;
125200090924            eval(h) w0122=((tsmp-tsmr)*100)/tsmr ;
125300090924            if %abs(w0122)>999,99;
125400090924               if w0122>0;
125500090924                  tspc=999,99;
125600090924               else;
125700090924                  tspc=-999,99;
125800090924               endif;
125900090924            else;
126000090924               tspc=w0122;
126100090924            endif;
126200090923         endif;
126300090923         if tsmp<>tsmr and tsmp=0;
126400090923            tSPC=-100;
126500090923         endif;
126600090922         // calcolo il peso medio a spedizione
126700090923         if tspr>0;
126800090924            eval(h) tPMR=tpkr/tspr;
126900090923         endif;
127000090923         if tspp>0;
127100090924            eval(h) tPMP=tpkp/tspp ;
127200090923         endif;
127300100615
127400090922         // Calcolo la % di scostamento del peso medio del periodo rispetto
127500090922         // al peso medio del periodo di riferimento
127600100615
127700090923         if tpmp>0 and tpmr>0 and tpmp<>tpmr;
127800090924            eval(h) w0142=((tpmp-tpmr)*100)/tpmr;
127900090924            if %abs(w0142)>999,99;
128000090924               if w0142>0;
128100090924                  tppc=999,99;
128200090924               else;
128300090924                  tppc=-999,99;
128400090924               endif;
128500090924            else;
128600090924               tppc=w0142;
128700090924            endif;
128800090923         endif;
128900090923         if tpmp<>tpmr and tpmp=0;
129000090923            tppc=-100;
129100090923         endif;
129200090922       endsr;
129300090929       //-------------------------------------------------------------*
129400090929       //Stampa totale cliente - stampa sintetica -                   *
129500090929       //-------------------------------------------------------------*
129600090929       Begsr SR_stampatot;
129700090929             // calcolo medie e %scostamento
129800090929             exsr sr_caltot;
129900090929             // verifico rottura commerciale
130000090929                if savage<>pre_age;
130100100614
130200100614
130300090929                   *in40=*on;
130400090929                   pageuni=savage;
130500090929                   pagedes=savdes;
130600090929                   // Eseguo salto pagina se non è la prima pagina di stampa
130700090929                   if $righe>10;
130800090929                      exsr sr_overflow;
130900090929                      write st68rv1;
131000090929                      $righe+=1;
131100090929                   else;
131200090929                      write st68rv1;
131300090929                      $righe+=1;
131400090929                   endif;
131500090929                   pre_age=savage;
131600090929                endif;
131700100614
131800090929                pkscuni=savcli;
131900090929                pkscdes=savrag;
132000090929                pcat   =savclv;
132100110808                clear pblo  ;
132200110808                if savblo='S'  ;
132300110808                 pblo='BL' ;
132400110808                endif  ;
132500100615                if $righe+2>60;
132600090929                   exsr sr_overflow;
132700090929                   write st68rv1;
132800090929                   $righe+=1;
132900090929                endif;
133000090929                write st68rt1;
133100090929                write st68rv1;
133200090929                $righe+=2;
133300090929       endsr;
133400100614       //-------------------------------------------------------------*
133500100615       //Stampa totale Agente
133600100614       //-------------------------------------------------------------*
133700100614       Begsr StaTOTage;
133800100614
133900100614       // La prima riga stampa anche il commerciale
134000100614       *in40=*on;
134100100614       pageuni=savage;
134200100614       pagedes=savdes;
134300100614
134400100615       // Faccio un ciclo di 4 per ogni tipo cliente
134500100614       xx=1   ;
1346001006141      dow xx<=4  ;
1347001006142      if Upr(xx)>0 or Upp(xx)>0 or Ukr(xx)>0 or Ukp(XX)>0 ;
134800100614       TSPR=Upr(xx)  ;
134900100614       TSPp=Upp(xx)  ;
135000100614       Tpkr=Ukr(xx)  ;
135100100614       Tpkp=Ukp(xx)  ;
135200100614
135300100615             // sommo ai totali generali
135400100615       Gupr(xx)+=Upr(xx)  ;
135500100615       Gupp(xx)+=Upp(xx)  ;
135600100615       Gukr(xx)+=Ukr(xx)  ;
135700100615       Gukp(xx)+=Ukp(xx)  ;
135800100615
135900100614             // calcolo medie e %scostamento
136000100614             exsr sr_caltot;
136100100614
136200100614        select   ;
136300100614        when xx=1       ;
136400100614        pcat='D';
136500100614        when xx=2       ;
136600100614        pcat='T';
136700100614        when xx=3       ;
136800100614        pcat='A';
136900100614        other         ;
137000100614        pcat='B';
137100100614        endsl   ;
137200100614
137300100614 3              if $righe+1>61;
137400100614                   exsr sr_overflow;
137500100614                   write st68rv1;
137600100614                   $righe+=1;
137700100614 3              endif;
137800100614
137900100614 3              if *in40 ;
138000100614                write st68tage1;
138100100614 3              endif   ;
138200100614                write st68tage2 ;
138300100614
138400100614 3              if *in40 ;
138500100614                $righe+=2;
138600100614                else  ;
138700100614                $righe+=1;
138800100614 3              endif  ;
138900100614
139000100614       *in40 =*off   ;
139100100614 2     endif   ;
139200100614
139300100614       xx=xx+1 ;
139400100614 1     enddo  ;
139500100615
139600100615       // Totale generale
139700100615       TSPR=%xfoot(Upr)   ;
139800100615       TSPp=%xfoot(Upp)  ;
139900100615       Tpkr=%Xfoot(Ukr)  ;
140000100615       Tpkp=%xfoot(Ukp)  ;
140100100615
140200100615             // calcolo medie e %scostamento
140300100615             exsr sr_caltot;
140400100615 3              if $righe+3>62;
140500100615                   exsr sr_overflow;
140600100615                   write st68rv1;
140700100615                   $righe+=1;
140800100615 3              endif;
140900100615
141000100615                *in40=*on   ;
141100100615                *in50=*on   ;
141200110808                clear pcat  ;
141300100615                pgenerale='TOTALE COMMERCIALE'   ;
141400100615                write st68tage1;
141500100615                write st68tage2 ;
141600100615                write st68tage1;
141700101221                $righe+=3;
141800100615                *in40=*oFF  ;
141900100615                *in50=*oFF  ;
142000100615
142100100615       clear upr       ;
142200100615       clear upp       ;
142300100615       clear ukr       ;
142400100615       clear ukp       ;
142500110825          exsr sr_clrtot;
142600100614
142700100614       endsr;
142800100615       //-------------------------------------------------------------*
142900100615       //Stampa totale GENERALE
143000100615       //-------------------------------------------------------------*
143100100615       Begsr StaTOTGEN;
143200100615
143300100615       // Eseguo salto pagina
143400100615             exsr sr_overflow;
143500100615             write st68rv1;
143600100615             $righe+=1;
143700100615
143800100615       *in40=*off ;
143900100615       write st68tage1;
144000101221       $righe+=1;
144100100615
144200100615       // Faccio un ciclo di 4 per ogni tipo cliente
144300100615       xx=1   ;
1444001006151      dow xx<=4  ;
1445001006152      if GUpr(xx)>0 or GUpp(xx)>0 or GUkr(xx)>0 or GUkp(XX)>0 ;
144600100615       TSPR=gUpr(xx)  ;
144700100615       TSPp=gUpp(xx)  ;
144800100615       Tpkr=gUkr(xx)  ;
144900100615       Tpkp=gUkp(xx)  ;
145000100615
145100100615             // calcolo medie e %scostamento
145200100615             exsr sr_caltot;
145300100615
145400100615        select   ;
145500100615        when xx=1       ;
145600100615        pcat='D';
145700100615        when xx=2       ;
145800100615        pcat='T';
145900100615        when xx=3       ;
146000100615        pcat='A';
146100100615        other         ;
146200100615        pcat='B';
146300100615        endsl   ;
146400100615
146500100615                write st68tage2 ;
146600100615                $righe+=1;
146700100615
146800100615 2     endif   ;
146900100615
147000100615       xx=xx+1 ;
147100100615 1     enddo  ;
147200100615
147300100615       // Totale generale
147400100615       TSPR=%xfoot(gUpr)   ;
147500100615       TSPp=%xfoot(gUpp)  ;
147600100615       Tpkr=%Xfoot(gUkr)  ;
147700100615       Tpkp=%xfoot(gUkp)  ;
147800100615
147900100615             // calcolo medie e %scostamento
148000100615             exsr sr_caltot;
148100100615
148200100615                *in50=*on   ;
148300100615                clear pcat  ;
148400100615                pgenerale='TOTALE GENERALE   '   ;
148500100615                write st68tage1;
148600100615                write st68tage2 ;
148700100615                write st68tage1;
148800101221                $righe+=3;
148900100615                *in40=*oFF  ;
149000100615                *in50=*oFF  ;
149100100615
149200100615       endsr;
149300090921       //-------------------------------------------------------------*
149400090921       //Routine di impostazione campi di stampa record di testata    *
149500090921       //-------------------------------------------------------------*
149600090921       Begsr SR_impt00;
149700090921
149800090921       // Titolo
149900090921
150000090921       dataeur=%date(st68dal:*iso);
150100090921       wdatagma=%dec(dataeur);
150200091007       ptitolo='Analisi Spedizioni dal';
150300090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ') + ' al';
150400090921       dataeur=%date(st68al:*iso);
150500090921       wdatagma=%dec(dataeur);
150600090921       ptitolo=%trim(ptitolo)+ ' '+ %editw(wdatagma:'  /  /    ');
150700090921
150800090925       // Categoria cliente
150900090921
151000090929       wprimo='S';
151100090925       pcatcli ='Categoria clienti';
151200090921       if st68clv=*blanks;
151300090925          pcatcli=%trim(pcatcli)+ ' D/T/A/B';
151400090921       else;
151500090922          for I=1 to %elem(clv);
151600090929             if clv(i)<>*blanks;
151700090929                if wprimo<>'S';
151800090929                   pcatcli=%trim(pcatcli)+ '/';
151900090929                endif;
152000090929                if wprimo='S';
152100090929                   pcatcli=%trim(pcatcli)+ ' ' +clv(i);
152200090929                else;
152300090929                   pcatcli=%trim(pcatcli)+ clv(i);
152400090929                endif;
152500090929                wprimo=' ';
152600090929             endif;
152700090921          endfor;
152800090921       endif;
152900090921
153000090921       // Periodo di riferimento dal/al
153100090921
153200090921       dataeur=%date(st68rda:*iso);
153300090922       prifdal=%dec(dataeur);
153400090921       dataeur=%date(st68ral:*iso);
153500090922       prifal=%dec(dataeur);
153600090921
153700090921       // limite % di scostamento
153800090930       *in43=*off;
153900090930       if st68sco<>0;
154000090930          *in43=*on;
154100090921
154200090930          pscost=st68sco;
154300090921
154400090921       // Valore di riferimento %
154500090921
154600090930          select;
154700090930          when st68rif='S';
154800090930            prifper='Spedizioni';
154900090930          when st68rif='P';
155000090930            prifper='Peso medio';
155100090930          endsl;
155200090930       endif;
155300101110
155400101110       // area se richiesta
155500101110       if st68car>0   ;
155600101110       clear tblkey  ;
155700101110       eval tblkey=%editc(st68car:'X') ;
155800101110       chain (1:'05':tblkey) tabel00f  ;
155900101110         if not %found(tabel00f);
156000101110          clear t_tbluni   ;
156100101110         endif   ;
156200101110       eval priare='Area  ' + %editc(st68car:'X') +' ' + t_tbluni   ;
156300101110       endif  ;
156400101110
156500090922       endsr;
