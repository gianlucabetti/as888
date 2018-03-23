000100120330      *               OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
000200030326     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*----------------------------------------------------------------
000400120330     FTitas30c  iF   E           K DISK    ignore(titasp00) usropn
000500120402     f                                     extfile(filetas)
000600120330     Ffidsf01l  iF   E           K DISK    usropn
000700120402     f                                     extfile(filedsf)
000800130128     FfidsT01l  iF   E           K DISK    usropn
000900130128     f                                     extfile(filedsT)
001000120330     Ffiapd01l  iF   E           K DISK
001100120329     Ffnorm01l  iF   E           K DISK
001200120329     Ftabel00f  iF   E           K DISK
001300120329     Ftntbe01l  iF   E           K DISK
001400120618     Fwfnlvp11f o  a E           K DISK    usropn
001500120618     Ffnlvp11p  O    e             PRINTER oflind(*in10) usropn
001600030314     d*
001700000000     D*----------------------------------------------------------------
001800030314     D KPJBA         E DS
001900120329     D ds2a          E DS
002000120329     D dcmr          E DS
002100120402     D tibs55ds      E DS
002200120402     D fnlvp10ds     E DS
002300120329     D fiprdcetds    E DS
002400120329     D fiprdcrids    E DS
002500120329     D fiprcresds    E DS
002600120329     D fiprcrchds    E DS
002700120330     D wfiplgds      E DS                  extname(wfiplg0f)
002800120329     D titasds       E DS                  extname(titas00f)
002900120329     D fiapdds       E DS                  extname(fiapd00f)
003000120329     D fnormds       E DS                  extname(fnorm00f)
003100120620     d dafasi          s              1
003200120329     d ora             s              6
003300121031     d WRKDATHHMM      s             12A
003400131128     d V1CCMC          s              3A
003500120329     d V1SORA          s              6  0
003600121023     d V1esito         s              6  0
003700120329     d DTAISO          S               D   DATFMT(*ISO)
003800120329     d DTAEUR          S               D   DATFMT(*EUR)
003900120329     d savdata         S                   like(wrkdata)
004000120329     d savndc          S                   like(wrkndc)
004100120329     d savpdr          S                   like(wrkcodaut)
004200131128     d v1caut          S                   like(apdpdr)
004300120330     d norecchi        s              1    inz('1')
004400120402     d lib046alg       s                   like(o50alg)
004500120402     d lib001ala       s                   like(o50ala)
004600120402     d filetas         s             21
004700120402     d filedsf         s             21
004800130128     d filedsT         s             21
004900130128     d Record_OK       s              1
005000131203      *
005100131203     D*-----------
005200131203     D WLBDAT          DS
005300131203     D  G02DAT                 1      8  0
005400131203     D  G02INV                 9     16  0
005500131203     D  G02ERR                17     17
005600131203     D  G02TGI                18     22  0
005700131203     D*-----------
005800120618      *
005900120618     D  MSGDS          S            100
006000120618     D  LENGH          S             15  5
006100120618     D su_file         C                   CONST('F')
006200120618     D su_Stampa       C                   CONST('S')
006300121031     D Error_DATA      C                   CONST('DATA < o > di 10 GG')
006400030314     c*****************************************************************
006500120329     c                   movel     *ALL'-'       TRA
006600120329     c                   movel     *ALL'-'       TRA1
006700120330     c                   movel     f10dti        da                8
006800120330     c                   movel     f10dtf        a                 8
006900120605     c                   clear                   almeno_uno        1
007000030310     C/EXEC SQL
007100120330     C+ DELETE FROM wfiplg0f
007200120329     C/END-EXEC
007300121127     c*
007400121127      * PRIMA prende i dati senza errori di trasmissione
007500120329     C/EXEC SQL
007600120330     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
007700120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
007800120329     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 29, 8),
007900121205     C+ SUBSTR(PLGDATI, 37, 6), 'D'
008000121205     C+  FROM FIPLG00F WHERE plGfgs = :f10po and
008100121115     C+ substr(plgdatora, 1, 8) between :da and :a and
008200121115     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd ='CET' and
008300121115     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
008400121115     C+ substr(plgdati, 218, 1) = ' '
008500121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
008600120329     C/END-EXEC
008700120329
008800121127      * PRIMA prende i dati senza errori di trasmissione
008900120329     C/EXEC SQL
009000120330     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
009100120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
009200120329     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 24, 8),
009300121205     C+ SUBSTR(PLGDATI, 32, 6), 'D'
009400121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po and
009500121115     C+ substr(plgdatora, 1, 8) between :da and :a and
009600121115     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd='RES'
009700121115     C+ and PLGTIPAPPL = 'R' and plgerr = ' ' and
009800121115     C+ substr(plgdati, 185, 1) = ' '
009900121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
010000120329     C/END-EXEC
010100120329
010200121127      * PRIMA prende i dati senza errori di trasmissione
010300120329     C/EXEC SQL
010400120330     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
010500120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
010600120329     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 15, 8),
010700121205     C+ SUBSTR(PLGDATI, 23, 6), 'T'
010800121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po and
010900121115     C+ substr(plgdatora, 1, 8) between :da and :a and
011000121115     C+ plgcodaut between :f10cda and :f10ca and
011100121115     C+ plgtiporcd='CRI' and PLGTIPAPPL = 'C' and plgerr = ' '
011200121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
011300120329     C/END-EXEC
011400120329
011500121127      * PRIMA prende i dati senza errori di trasmissione
011600120329     C/EXEC SQL
011700120330     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
011800120329     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
011900120329     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, SUBSTR(PLGDATI, 20, 8),
012000121205     C+ SUBSTR(PLGDATI, 28, 6), 'T'
012100121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po
012200121115     C+ and substr(plgdatora, 1, 8) between :da and :a and
012300121115     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd='RCH'
012400121115     C+ and PLGTIPAPPL = 'R' and plgerr = ' '
012500121127     C+ and plgMSG not like '%DATA < o > di 10 GG%'
012600120329     C/END-EXEC
012700121127
012800121128      *---- ---------------------------------------- ----
012900121127      * POI prende i dati con errori di trasmissione
013000121128      *---- ---------------------------------------- ----
013100121127     c*
013200121127      * PRIMA prende i dati senza errori di trasmissione
013300121127     C/EXEC SQL
013400121127     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
013500121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
013600121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
013700121205     C+ SUBSTR(PLGDATI, 37, 6), 'D'
013800121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po and
013900121127     C+ substr(plgdatora, 1, 8) between :da and :a and
014000121127     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd ='CET' and
014100121127     C+ PLGTIPAPPL = 'C' and plgerr = ' ' and
014200121127     C+ substr(plgdati, 218, 1) = ' '
014300121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
014400121127     C/END-EXEC
014500121128     c                   if        SQLCOD = 0
014600121128     c                   eval      *in33 = *on
014700121128     c                   end
014800121127
014900121127      * PRIMA prende i dati senza errori di trasmissione
015000121127     C/EXEC SQL
015100121127     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
015200121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
015300121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
015400121205     C+ SUBSTR(PLGDATI, 32, 6), 'D'
015500121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po and
015600121127     C+ substr(plgdatora, 1, 8) between :da and :a and
015700121127     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd='RES'
015800121127     C+ and PLGTIPAPPL = 'R' and plgerr = ' ' and
015900121127     C+ substr(plgdati, 185, 1) = ' '
016000121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
016100121127     C/END-EXEC
016200121128     c                   if        SQLCOD = 0
016300121128     c                   eval      *in33 = *on
016400121128     c                   end
016500121127
016600121127      * PRIMA prende i dati senza errori di trasmissione
016700121127     C/EXEC SQL
016800121127     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
016900121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
017000121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
017100121205     C+ SUBSTR(PLGDATI, 23, 6), 'T'
017200121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po and
017300121127     C+ substr(plgdatora, 1, 8) between :da and :a and
017400121127     C+ plgcodaut between :f10cda and :f10ca and
017500121127     C+ plgtiporcd='CRI' and PLGTIPAPPL = 'C' and plgerr = ' '
017600121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
017700121127     C/END-EXEC
017800121128     c                   if        SQLCOD = 0
017900121128     c                   eval      *in33 = *on
018000121128     c                   end
018100121127
018200121127      * PRIMA prende i dati senza errori di trasmissione
018300121127     C/EXEC SQL
018400121127     C+ INSERT INTO wfiplg0f SELECT PLGTIPORCD, PLGTIPAPPL,
018500121127     C+ PLGPRFC, PLGIDDISP, PLGDATORA, PLGCODAUT, PLGFGS, PLGNDC,
018600121127     C+ PLGIDDOC, PLGDATI, PLGERR, PLGMSG, substr(plgdatora, 1, 8),
018700121205     C+ SUBSTR(PLGDATI, 28, 6), 'T'
018800121205     C+   FROM FIPLG00F WHERE plGfgs = :f10po
018900121127     C+ and substr(plgdatora, 1, 8) between :da and :a and
019000121127     C+ plgcodaut between :f10cda and :f10ca and plgtiporcd='RCH'
019100121127     C+ and PLGTIPAPPL = 'R' and plgerr = ' '
019200121127     C+ and plgMSG like '%DATA < o > di 10 GG%'
019300121127     C/END-EXEC
019400121128     c                   if        SQLCOD = 0
019500121128     c                   eval      *in33 = *on
019600121128     c                   end
019700120329     C/EXEC SQL
019800120330     C+ DECLARE FIPLG CURSOR FOR SELECT DISTINCT * FROM wfiplg0f ORDER BY
019900121205     C+ wrkCODAUT, wrkFGS, wrkNDC, wrkrtotal, WRKDATA, wrkora
020000120329     C/END-EXEC
020100030314
020200030314     C/EXEC SQL
020300120329     C+ OPEN fiplg
020400030314     C/END-EXEC
020500030325     C                   DOU       sqlcod <> 0
020600030314     C/EXEC SQL
020700120330     C+ FETCH NEXT FROM fiplg  INTO :wfiplgds
020800030314     C/END-EXEC
020900030314
021000030314     C                   SELECT
021100030314     C                   WHEN      SQLCod = 100
021200120605     c                   leave
021300030314     C                   WHEN      SQLCod < 0
021400030314     C                   EVAL      *INH1 = *ON
021500030314     C                   OTHER
021600130128      *** il Default è "OK"
021700130128     c                   eval      record_OK = 'S'
021800130128      ***
021900130128      *   Solo se richiesta una particolare tipologia può non essere "OK"
022000130128      *      e se possibile chiederla è stato già testato nel pgm di lancio
022100140304     c                   if        F10PRCMES <>'T'  or f10test = 'N'
022200130128      *
022300130128     C     Kdst          chain     fiDST01l
022400130128     c                   if        %Found(fiDST01l)
022500130128      *  se nella Distinta non corrisponde la tipologia NEGA l'elaborazione
022600130128      *   del Record ..... e SE NON TROVASSE la DISTINTA ?!?!?! --> elabora
022700130128      *
022800130128     c                   If        f10prcMES='P' and dstTPM <> *blank
022900130128     c                              or
023000130128     c                             f10prcMES='M' and dstTPM <> 'M'
023001140304     c                              or
023002140304     c                             f10test='N' and %subst(dstflr:1:1) <> ' '
023100130128      *
023200130128     c                   eval      record_OK = 'N'
023300130128      *
023400130128     c                   End
023500130128     c                   endif
023600130128      *
023700130128     c                   endIF
023800130128      ***
023900130128      *    se richiesto un particolare tipo di Distinta PRC/MES
024000130128      *    Parcel o Messaggerie oppure TUTTO  altrimenti salta
024100130128     c                   if        record_OK = 'S'
024200120329     c* elabora il record letto
024300030314     c                   exsr      leggi
024400120605     c                   move      'S'           almeno_uno        1
024500130128     c                   endIF
024600130128      ***
024700030314     C                   ENDSL
024800030314
024900030314     C                   ENDDO
025000030314     C/EXEC SQL
025100120329     C+ CLOSE fiplg
025200030314     C/END-EXEC
025300120330     c* verifico se ho stampato la chiusura distinta da PDA altrimenti la
025400120330     c* cerco sul file delle fasi
025500120605     c                   if         almeno_uno = 'S'
025600120330     c                   if        wrkndc = savndc and savndc <> 0
025700120330     c                             and norecchi = *on
025800120330     c                   exsr      srfasi
025900120330     c                   end
026000120605     c                   endif
026100030310
026200120618     c                   close     wfnlvp11f                            99
026300120618     c                   close     fnlvp11p                             99
026400030314     C                   SETON                                        LR
026500030314     C*--------------------------------------------------------
026600030314     C* leggi           elabora record letto                  *
026700030314     C*--------------------------------------------------------
026800120329      /free
026900120329       begsr leggi;
027000121031
027100120329       *in01=*off;
027200120330       *in02=*off;
027300120330       *in03=*off;
027400121031
027500121031       //  Imposta la data di ricezione senza i secondi
027600121031       wrkDAThhmm = %subst(wrkDATora: 1: 12);
027700121031
027800120329       // consegne
027900120329       if wrktipappl= 'C';
028000121031
028100120329         if wrktiporcd = 'CET';
028200121031
028300120329          v1stipo = 'C';
028400120329          *in01=*on;
028500120329           if wrkdati <> ' ' ;
028600121031
028700120329            eval fiprdcetds = wrkdati;
028800121114
028900121114      /end-free
029000121114      *
029100121114      ***  se presente una data/ora non corretta
029200121114      ***  prende la data ora (senza i secondi) della ricezione
029300121114      ***  e la sostituisce.
029400121120     C                   clear                   v1erro
029500121114     c                   if          wrkMSG = Error_DATA
029600121120     c*********          movel     wrkDAThhmm    §CETDTORIN
029700121120     C*********          move      §CETDTORIN    §cetora
029800121120     C*********          movel     §CETDTORIN    v1sdta
029900121120     c*********          move      v1sdta        dtaiso
030000121120     c*********          move      dtaiso        dtaeur
030100121120     c*********          move      dtaeur        v1sdta
030200121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
030300121120     C                   movel     '*'           v1erro
030400121114     c                   end
030500121114      *
030600121114     C                   clear                   v1esito
030700121114     C                   move      §CETDTORIN    v1esito
030800131120      *
030900131120      *  deve sempre sostituire l'orario rilevato dal PDA come orario reale
031000131120     c                   if        v1esito >0
031100131204     C****  ??????  da attivare appena Michele allineerà  ?????????
031200131204     C*************      move      v1esito       §cetora
031300131120     c                   end
031400121114      /free
031500121114
031600120329            v1sora = §cetora ;
031700131128            v1ccmc = §cetCMC;
031800120329             if §cetcmc = ' ';
031900120329              v1dcmc = 'CONSEGNATA';
032000120329             else;
032100120329              tblKEy = §cetcmc;
032200120329              tblcod = '2A';
032300120329              tblkut = 1;
032400120329              chain (tblkut:tblcod:tblkey) tabel00f;
032500120329              clear v1dcmc;
032600120329               if  %found(tabel00f);
032700120329               ds2a = tblUNI;
032800120329               v1dcmc= §2ades;
032900120329               endif;
033000120329             endif;
033100120329           endif;
033200120329          tasaas = §CETAAS;
033300120329          taslnp = §CETLNP;
033400120329          tasnrs = §CETNRS;
033500120329          tasnsp = §CETNSP;
033600120329          chain (tasaas:taslnp:tasnrs:tasnsp) titas30c;
033700120329           if  not %found(titas30c);
033800120329           clear titasds;
033900120329           endif;
034000120330          v1srag = tasrsd;
034100120330          v1sind = tasind;
034200120330          v1sloc = taslod;
034300120330          v1spkg = taspkb;
034400120330          v1svlm = tasvlb;
034500120528          v1stsp = ' ';
034600120528          if tastsp ='E' or tastsp ='H';
034700120528             v1stsp = tastsp;
034800120528          endif;
034900120329          v1saas = §CETAAS;
035000120329          v1slnp = §CETLNP;
035100120329          v1snrs = §CETNRS;
035200120329          v1snsp = §CETNSP;
035300120329          v1spoe = 0;
035400120329          v1snsr = 0;
035500120329          v1snor = 0;
035600120329          v1snrv = 0;
035700120329         endif;
035800121031
035900120329       // chiusura distinta consegne
036000120329          if wrktiporcd= 'CRI' and wrkdati <> ' ' ;
036100121031
036200120330          eval fiprdcrids = wrkdati;
036300121031
036400120330      /end-free
036500121031      *
036600121031      ***  se presente una data/ora non corretta
036700121031      ***  prende la data ora (senza i secondi) della ricezione
036800121031      ***  e la sostituisce.
036900121120     C                   clear                   v1erro
037000121031     c                   if          wrkMSG = Error_DATA
037100121120     c************       movel     wrkDAThhmm    §CRIDTORIN
037200121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
037300121120     C                   movel     '*'           v1erro
037400121031     c                   end
037500121031      *
037600131128     c                   clear                   v1ccmc
037700121023     c                   clear                   v1esito
037800120330     c                   move      §cridtorin    v1sora
037900120402     c                   movel     §cridtorin    v1sdta
038000120402     c                   move      v1sdta        dtaiso
038100120402     c                   move      dtaiso        dtaeur
038200120402     c                   move      dtaeur        v1sdta
038300120330      /free
038400120329          *in02=*on;
038500120330          v1stipchi='PDA';
038600120330          norecchi = *off;
038700120329          endif;
038800121031
038900120329        else;
039000121031
039100120329       // ritiri
039200120329         if wrktiporcd= 'RES';
039300121031
039400120329          if wrkdati <> ' ';
039500120329           *in01=*on;
039600120329           v1saas = 0;
039700120329           v1slnp = 0;
039800120329           v1snrs = 0;
039900120329           v1snsp = 0;
040000120528           v1stsp = ' ';
040100120329           v1stipo = 'R';
040200131128           clear v1ccmc;
040300121031
040400120329           eval FIPRCRESDS = wrkdati;
040500121031
040600121031      /end-free
040700121031      *
040800121031      ***  se presente una data/ora non corretta
040900121031      ***  prende la data ora (senza i secondi) della ricezione
041000121031      ***  e la sostituisce.
041100121120     C                   clear                   v1erro
041200121031     c                   if          wrkMSG = Error_DATA
041300121120     c***********        movel     wrkDAThhmm    §RESDATORA
041400121120     c***********        movel     wrkDAThhmm    §RESDTORIN
041500121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
041600121120     C                   movel     '*'           v1erro
041700121031     c                   end
041800121031      *
041900121031      /free
042000121031
042100120330           v1spoe = %dec(§RESPOE:3:0);
042200120329           v1snsr = %dec(§RESNSR:6:0);
042300120329           v1snor = %dec(§RESNOR:7:0);
042400120329           v1snrv = %dec(§RESNRV:2:0);
042500131128              clear v1ccmc;
042600121031
042700120329       // ritiri manuali
042800120329            if %subst(wrkiddoc: 1: 8) = '00000000';
042900120330              v1dcmc = 'RITIRATO';
043000120329             v1srag = §RESRSR;
043100120329             v1sind = §RESINR;
043200120329             v1sloc = ' ';
043300120329             v1spkg = 0;
043400120329             v1svlm = 0;
043500120329            else;
043600120330             ormpoe = %dec(§RESpoe:3:0);
043700120329             ormnsr = %dec(§RESNSR:6:0);
043800120329             ormnor = %dec(§RESNOR:7:0);
043900120329             ormnrv = %dec(§RESNRV:2:0);
044000120329             chain (ormpoe:ormnsr:ormnor:ormnrv) fnorm01l;
044100120329              if  not %found(fnorm01l);
044200120329              clear fnormds;
044300120329              endif;
044400120329             v1srag = ORMRSR;
044500120329             v1sind = ORMINR;
044600120329             v1sloc = ormlor;
044700120329             v1spkg = ormpkg;
044800120329             v1svlm = ormvlm;
044900120329            endif;
045000120329           endif;
045100121031
045200120329       // ritiri normali
045300120329           if wrkdati <> ' ' and %subst(wrkiddoc: 1: 8)<> '00000000';
045400120329            ora = %subst(§RESDATORA: 9: 6);
045500120329            v1sora = %dec(ora:6:0);
045600121023      /end-free
045700121023     C                   clear                   v1esito
045800121023     c                   if        %subst(§RESDTORIN:9:6) <> *blank
045900121023     C                   move      §RESDTORIN    v1esito
046000121023     c                   end
046100121023      /free
046200131128              clear v1ccmc;
046300120329             if §rescmr = ' ';
046400120329              v1dcmc = 'RITIRATO';
046500120329             else;
046600120329              tbeke1 = §rescmr;
046700120329              tbecod = 'CMR';
046800120329              chain (tbecod:tbeke1) tntbe01l;
046900120329              clear v1dcmc;
047000120329               if  %found(tntbe01l);
047100120329                dcmr = tbeUNI;
047200120329                v1dcmc= D§CMRDES;
047300120329               endif;
047400120329             endif;
047500120329           endif;
047600120330         endif;
047700121031
047800120329       // chiusura distinta consegne
047900121031
048000120329          if wrktiporcd= 'RCH' and wrkdati <> ' ' ;
048100121031
048200120330           eval fiprcrchds = wrkdati;
048300121031
048400121031      /end-free
048500121031      *
048600121031      ***  se presente una data/ora non corretta
048700121031      ***  prende la data ora (senza i secondi) della ricezione
048800121031      ***  e la sostituisce.
048900121120     C                   clear                   v1erro
049000121031     c                   if          wrkMSG = Error_DATA
049100121120     c*************      movel     wrkDAThhmm    §RCHDATORA
049200121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
049300121120     C                   movel     '*'           v1erro
049400121031     c                   end
049500121031      *
049600121031      /free
049700121031
049800131128            clear v1ccmc;
049900120330            ora = %subst(§RCHDATORA: 9: 6);
050000120330            v1sora = %dec(ora:6:0);
050100120402      /end-free
050200121023     c                   clear                   v1esito
050300120402     c                   movel     §rchdatora    v1sdta
050400120402     c                   move      v1sdta        dtaiso
050500120402     c                   move      dtaiso        dtaeur
050600120402     c                   move      dtaeur        v1sdta
050700120402      /free
050800120330          v1stipchi='PDA';
050900120330           *in03=*on;
051000120330          norecchi = *off;
051100120329          endif;
051200120329        endif;
051300121031
051400120329       exsr stampa;
051500121031
051600120329       endsr;
051700121031
051800120329      /end-free
051900030314     C*--------------------------------------------------------
052000120330     C* srfasi          elabora le fasi                       *
052100030314     C*--------------------------------------------------------
052200120330     C     srfasi        BEGSR
052300130128      ***
052400120330     c     kdsf          setll     fidsf01l
052500120330     c                   do        *hival
052600120330     c     kdsf          reade     fidsf01l
052700120330     c                   if        %eof(fidsf01l)
052800120330     c                   leave
052900120330     c                   end
053000120402     c                   if        dsftrd <>'CRI' and
053100120402     c                             dsftrd <>'RQE' and
053200120402     c                             DSFPDAAS4 ='A'
053300120402     c                   iter
053400120402     c                   end
053500120402     c                   move      DSFDTORIN     v1sora
053600120402     c                   movel     DSFDTORIN     v1sdta
053700120402     c                   move      v1sdta        dtaiso
053800120402     c                   move      dtaiso        dtaeur
053900120402     c                   move      dtaeur        v1sdta
054000120402     c                   eval      v1stipchi='AS400'
054100120402     c*
054200120402     c                   if        dsftrd = 'CRI'
054300120618     c                   If           F10STAFIL = su_stampa
054400120330     c                   write     TOTcon
054500120618     c                   elseIf       F10STAFIL = su_file
054600120620     c                   eval      dafasi = 'S'
054700120618     c                   exsr      write_TOT
054800120618     c                   end
054900120402     c                   else
055000120618     c                   If           F10STAFIL = su_stampa
055100120330     c                   write     TOTrit
055200120618     c                   elseIf       F10STAFIL =  su_file
055300120620     c                   eval      dafasi = 'S'
055400120618     c                   exsr      write_TOT
055500120618     c                   end
055600120330     c                   end
055700120618      **
055800120330     c                   enddo
055900120618      **
056000120330     c                   endsr
056100120330     C*--------------------------------------------------------
056200120330     C* stampa          elabora il dettaglio                  *
056300120330     C*--------------------------------------------------------
056400120330     C     stampa        BEGSR
056500130128      ***
056600120329     c                   if        wrkcodaut <> savpdr
056700120329     c     kapd          chain     fiapd01l
056800120329     c                   if        not %found(fiapd01l)
056900120329     c                   clear                   fiapdds
057000120329     c                   end
057100120329     c                   seton                                        10
057200120329     c                   eval      savpdr = wrkcodaut
057300120329     c                   end
057400120329     c*
057500120329     c                   if        wrkndc <> savndc
057600120330     c* verifico se ho stampato la chiusura distinta da PDA altrimenti la
057700120330     c* cerco sul file delle fasi
057800120330     c                   if        norecchi = *on and savndc <> 0
057900120330     c                   exsr      srfasi
058000120330     c                   end
058100120329     c                   seton                                        10
058200120330     c                   eval      norecchi = *on
058300120329     c                   eval      savndc = wrkndc
058400120329     c                   end
058500120329     c*
058600120329     c                   if        wrkdata<> savdata
058700120329     c                   seton                                        10
058800120329     c                   eval      savdata= wrkdata
058900120329     c                   move      wrkdata       com08             8 0
059000120329     c                   move      com08         dtaiso
059100120329     c                   move      dtaiso        dtaeur
059200120329     c                   move      dtaeur        wddc
059300120329     c                   end
059400120618      **
059500120618     c                   if        F10STAFIL = su_stampa
059600120329     c* testa
059700120329     c                   if        *in10
059800120329     c                   write     testa
059900120329     c                   write     tesDIS
060000120329     c                   setoff                                       10
060100120329     c                   end
060200120329     c* consegna
060300120329     c   01              write     DET
060400120330     c   02              write     TOTcon
060500120330     c   03              write     TOTrit
060600120618      **
060700120618     c                   elseIf    F10STAFIL = su_file
060800120618      **
060900120618     c   01              exsr      write_DET
061000120620     c                   eval      dafasi = ' '
061100120618     c   02
061200120618     cor 03              exsr      write_TOT
061300120618      **
061400120618     c                   end
061500120618      **
061600030314     C                   ENDSR
061700120329     C*---------------------------------------------------------------*
061800120329     C     *inzsr        BEGSR
061900120329     C*---------------------------------------------------------------*
062000120329     C     *ENTRY        PLIST
062100120329     C                   PARM                    KPJBA
062200131203     C                   PARM                    XStorico          1
062300131128      *
062400120329     c                   movel     kpjbu         fnlvp10ds
062500131128      *
062600131203     c                   move      'N'           storicizza        1
062700131128      **  Se richiamato senza la seconda PARM
062800131128     c                   if        %parms > 1
062900131203     C                   eval         storicizza  ='S'
063000131128     c                   end
063100130128      *
063200120329     C     Kapd          KLIST
063300120329     C                   KFLD                    apdtip
063400120329     C                   KFLD                    wrkcodaut
063500130128      *
063600120330     C     Kdsf          KLIST
063700120330     C                   KFLD                    DSFNPG
063800130128     C                   KFLD                    savndc
063900130128     C                   KFLD                    f10po
064000130128      *
064100130128     C     Kdst          KLIST
064200130128     C                   KFLD                    dsTnpg
064300130128     C                   KFLD                    wrkNDC
064400130128     C                   KFLD                    wrkFGS
064500130128      *
064600120330     c                   eval      dsfnpg = 4
064700130128     c                   eval      dsTnpg = 4
064800120330     c                   eval      apdtip = 'A'
064900120329     c                   move      udate         dtaeur
065000120329     c                   move      dtaeur        dtaiso
065100120329     C                   move      dtaiso        AMGUDT            8 0
065200120329     C                   MOVE      dtaeur        WOGGI
065300120402     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
065400120402     C* da/a cui spedire
065500120402     C                   CLEAR                   TIBS55ds
065600120402     C                   MOVEL     'L'           I50TLA
065700120402     C                   MOVEL     '046'         I50PPO
065800120402     C                   MOVEL     '046'         I50APO
065900120402     C                   CALL      'TIBS55R'
066000120402     C                   PARM                    TIBS55ds
066100120402     c                   eval      lib046alg = o50alg
066200120402     C                   CLEAR                   TIBS55ds
066300120402     C                   MOVEL     'L'           I50TLA
066400120402     C                   MOVEL     '001'         I50PPO
066500120402     C                   MOVEL     '001'         I50APO
066600120402     C                   CALL      'TIBS55R'
066700120402     C                   PARM                    TIBS55ds
066800120402     c                   eval      lib001ala = o50ala
066900120330     C* TITAS00F
067000120402     c                   EVAL      filetas=%trim(lib046alg)+'/'+'TITAS30C'
067100120330     C* FIDSF00F
067200120402     c                   EVAL      filedsf=%trim(lib001ala)+'/'+'FIDSF01L'
067300120618      **
067400130128     C* FIDST00F
067500130128     c                   EVAL      filedsT=%trim(lib001ala)+'/'+'FIDST01L'
067600130128      **
067700130128     c                   open      fidsT01l
067800120330     c                   open      fidsf01l
067900120330     c                   open      titas30c
068000120618      ** output su File
068100120618     c                   if        F10STAFIL = su_File
068200120618      **   wfile
068300131203     c                   if        storicizza = 'N'
068400120618     c                   exsr      CLEAR_WFile
068500131128     c                   end
068600131128      **
068700120618     c                   open      wfnlvp11f
068800120618      ** output su Stampa
068900120618     c                   elseIF    F10STAFIL = su_Stampa
069000120618      **   printer
069100120618     c                   open      fnlvp11p
069200120618      **
069300120618     c                   end
069400120618      **
069500120329     C                   ENDSR
069600120618     C*---------------------------------------------------------------*
069700120618     C     WRIte_DET     BEGSR
069800120618     C*---------------------------------------------------------------*
069900120618     c                   clear                   wfnlvp11
070000120618      **
070100120618     c                   eval       P11WRKNDC = WRKNDC
070200120618     c                   eval       P11WDDC   = WDDC
070300120618     c                   eval       P11APDRSC = APDRSC
070400131128     c                   eval       P11cAUT   = APDpdr
070500120618     c                   eval       P11STIPO  = v1STIPO
070600120618     c                   eval       P11SRAG   = v1SRAG
070700120618     c                   eval       P11SIND   = v1SIND
070800120618     c                   eval       P11SLOC   = v1SLOC
070900120618     c                   eval       P11SPKG   = v1SPKG
071000120618     c                   eval       P11SVLM   = v1SVLM
071100120618     c                   eval       P11DCMC   = v1DCMC
071200131128     c                   eval       P11cCMC   = v1cCMC
071300120618     c                   eval       P11SORA   = v1SORA
071400120618     c                   eval       P11SAAS   = v1SAAS
071500120618     c                   eval       P11SLNP   = v1SLNP
071600120618     c                   eval       P11SNRS   = v1SNRS
071700120618     c                   eval       P11SNSP   = v1SNSP
071800120618     c                   eval       P11STSP   = v1STSP
071900120618     c                   eval       P11SPOE   = v1SPOE
072000120618     c                   eval       P11SNSR   = v1SNSR
072100120618     c                   eval       P11SNOR   = v1SNOR
072200120618     c                   eval       P11SNRV   = v1SNRV
072300121023     c                   eval       P11ESIORA = v1esito
072400121205      **  sul FILE non si è voluto mettere l'asterisco perchè se dovesse
072500121205      **   essere importato su un EXCEL è meglio vedere una "S"
072600121205     c                   if        v1erro ='*'
072700121205     c                   eval       P11Errori = 'S'
072800121205     c                   end
072900131203      **
073000131203      **  Se esegue la storicizzazione
073100131203     c                   if        storicizza  = 'S'
073200131203     C                   Z-ADD     P11WDDC       G02DAT
073300131203     C                   MOVEL     *BLANK        G02ERR
073400131203     C                   CALL      'XSRDA8'
073500131203     C                   PARM                    WLBDAT
073600131203     C                   Z-ADD     G02INV        P11WDDC
073700131203     c                   end
073800120618      **
073900120618     c                   write     wfnlvp11
074000120618      **
074100120618     C                   ENDSR
074200120618     C*---------------------------------------------------------------*
074300120618     C     WRIte_TOT     BEGSR
074400120618     C*---------------------------------------------------------------*
074500120618     c                   clear                   wfnlvp11
074600120618      **
074700120618     c                   eval       P11WRKNDC = WRKNDC
074800120618     c                   eval       P11WDDC   = WDDC
074900120618     c                   eval       P11APDRSC = APDRSC
075000131203     c                   eval       P11cAUT   = APDpdr
075100120618      **
075200120620     c                   if        *in02  and dafasi= ' '
075300120620     c                               or
075400120620     c                             dsftrd <>'CRI' and dafasi= 'S'
075500120620     c                   eval       P11STIPO  = 'CT'
075600120620     c                   eval       P11SRAG   = 'CHIUSURA DISTINTA CONSEGNE'
075700120620     c                   end
075800120618      **
075900120620     c                   if        *in03  and dafasi= ' '
076000120620     c                               or
076100120620     c                             dsftrd = 'CRI' and dafasi= 'S'
076200120620     c                   eval       P11STIPO  = 'RT'
076300120620     c                   eval       P11SRAG   = 'CHIUSURA DISTINTA RITIRI'
076400120620     c                   end
076500120618      **
076600120618     c                   eval       P11SIND   = %editW(V1sora:'  :  :  ')
076700120618     c                               +  '  '
076800120618     c                               + %editW(V1sdta:'  /  /    ')
076900120618      **
077000120618     c                   eval       P11SLOC   = V1STIPCHI
077100120618      **
077200131203      **  Se esegue la storicizzazione
077300131203     c                   if        storicizza = 'S'
077400131203     C                   Z-ADD     P11WDDC       G02DAT
077500131203     C                   MOVEL     *BLANK        G02ERR
077600131203     C                   CALL      'XSRDA8'
077700131203     C                   PARM                    WLBDAT
077800131203     C                   Z-ADD     G02INV        P11WDDC
077900131203     c                   end
078000131203      **
078100120618     c                   write     wfnlvp11
078200120618      **
078300120618     C                   ENDSR
078400120618     C*---------------------------------------------------------------*
078500120618     C     CLEAR_WFile   BEGSR
078600120618     C*---------------------------------------------------------------*
078700120618      **
078800120618      * Pulisce il file
078900120618     C                   clear                   MSGDS
079000120618     C                   eval      MSGDS = 'CLRPFM FILE(WFNLVP11F)'
079100120618     C                   Z-ADD     100           LENGH
079200120618     C                   CALL      'QCMDEXC'
079300120618     C                   PARM                    MSGDS
079400120618     C                   PARM                    LENGH
079500120618      **
079600120618     C                   ENDSR
079700120618     C*---------------------------------------------------------------*
