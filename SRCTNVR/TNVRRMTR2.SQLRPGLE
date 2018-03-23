000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990317      ****************************************************************
000500980521     H DECEDIT('0,') DATEDIT(*DMY.)
000600980623
000700990602      *------------------------------------------------------------------------*
000800121029     ffnlbl01l  IF   E           K DISK
000900121030     ffnblp01l  IF   E           K DISK
001000121031     ffiar401l  IF   E           K DISK
001100131014     ffiarbf2c  IF   E           K DISK
001200121030     f                                     ignore(fnarbk00)
001300121030     f                                     ignore(fiarbt00)
001400121030     f                                     ignore(fnarbg00)
001500121030     f                                     ignore(fnarbm00)
001600121030     f                                     ignore(fnarbv00)
001700121030     f                                     ignore(fnarbp00)
001800131014     f                                     ignore(fnarbn00)
001900121030     fwftyd00f  o  a E             DISK    usropn
002000980521      *------------------------------------------------------------------------*
002100980702
002200990602      * Definizione campi di work ------------------------------------------
002300121029     d i               s              3  0 inz
002400121029     d Primo           s              4  0 inz
002500121030     d Qlen            s             15  5 inz(%len(Qcmd))
002600121030     d Qcmd            s             80    inz
002700121030     d $Finerec        s              1    inz(*off)
002800990601
002900990602      * DS esterne ---------------------------------------------------------
003000990602
003100121029     D TNVRRMTRDS    E DS
003200121029     d ksc                            7  0 dim(10) overlay(TNVRRMTRDS:34)
003300990602
003400990602     D KPJBA         E DS
003500980525      *
003600020911     D*-------------
003700020911     D* DS DI WRK
003800020911     D*-------------
003900121029     d fnblpds       e ds                  extname(fnblp00f) inz
004000121029
004100121029     D WrkStringaSql   S           4500
004200121029     D                                     VARYING
004300121029     D WrkKscIn        S           4000
004400121029     D                                     VARYING
004500121030      * - Comando per pulizia Work-File
004600121030     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
004700121030     d                                                   WFTYD00F)')
004800990603
004900980521      *
005000121029     C     *entry        plist
005100121029     C                   parm                    kpjba
005200121029     C                   movel     kpjbu         TNVRRMTRDS
005300121030
005400121030     c* Pulizia work file prima di scriverlo
005500121030     c                   call      'QCMDEXC'
005600121030     c                   parm      C_Cmd_ClrWF   Qcmd
005700121030     c                   parm                    Qlen
005800121030     c
005900121030     c                   open      WFtyd00f
006000121029      /free
006100121029         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006200121030
006300121030       // PRIMO GIRO: elaboro le bolle figlie di un dirottamento
006400121029       exsr sr_prepsql;
006500121029       // elaboro dati estratti
006600121029       exsr sr_elabora;
006700121030
006800121030       // SECONDO GIRO: elaboro le bolle sulle con variazioni "I0"=Destinatario
006900121030       exsr sr_prepsqlI0;
007000121030       // elaboro dati estratti
007100121030       exsr sr_elaboraI0;
007200980623
007300121029       *inlr=*on;
007400121029       //-------------------------------------------------------------*
007500121029       //Preparazione stringa sql per elaborazione figlie di dirottam *
007600121029       //-------------------------------------------------------------*
007700121029       Begsr SR_prepSql;
007800121029
007900121029       WrkStringaSql='select * from fnblp00f' +
008000121029                     ' where' +
008100121029                     ' blpcbo=''$ ''' +
008200121030                     ' and blpcca not in (' +
008300121106                     '''1'', ''5'',''7'',''2'',''6'',''S'')'+
008400121029                     ' and blpksc in (' ;
008500121029                     exsr sr_whrksc;
008600121029          WrkStringaSql=WrkStringaSql+WrkKscIn+
008700121029          ' and blpdcm between ' +
008800121029          %char(dl5dti) + ' and ' +
008900121029          %char(dl5dtf) ;
009000121029
009100121029       endsr;
009200121029       //-------------------------------------------------------------*
009300121029       Begsr SR_whrksc;
009400121029          Primo= 1 ;
009500121029
009600121029          reset WrkKscIn;
009700121029          for I=1 to 10;
009800121029             if ksc(i)=0;
009900121029                leave;
010000121029             endif;
010100121029             if primo>1;
010200121029                WrkKscIn=WrkKscIn + ' , ';
010300121029             endif;
010400121029             WrkKscIn=WrkKscIn + %char(Ksc(i));
010500121029             Primo=primo+1   ;
010600121029          endfor;
010700121029          WrkKscIn=WrkKscIn + ')';
010800121029       endsr;
010900121029       //-------------------------------------------------------------*
011000121029       Begsr SR_Elabora;
011100121029        $finerec=*off;
011200121029        exec sql prepare STRINGASQL from :WrkStringaSql;
011300121029        exec sql declare BLPCsr cursor for StringaSql;
011400121029        exec sql open BLPcsr;
011500121029        dow $finerec=*off;
011600121029           exec sql Fetch BLPcsr into :fnblpds ;
011700121029           if sqlcod=100 or sqlcod<0;
011800121029              $finerec = *on;
011900121029              leave;
012000121029           endif;
012100121031        // Aggancio fiar4 per seconda ragione sociale
012200121031           clear ar4not;
012300121031           chain (blpaas:blplnp:blpnrs:blpnsp:'D') fiar401l ;
012400121029        // Cerco la bolla mamma
012500121029           chain (blpaas:blplnp:blpnrs:blpnsp) fnlbl01l ;
012600121030           if %found(fnlbl01l);
012700121030              exsr wtr_wf;
012800121030           endif;
012900121029        enddo;
013000121030        exec sql close BLPcsr;
013100121030       endsr;
013200121030       //-------------------------------------------------------------*
013300121030       //Preparazione stringa sql per elaborazione varizioni I0       *
013400121030       //-------------------------------------------------------------*
013500121030       Begsr SR_prepSqlI0;
013600121030
013700121030       clear WrkStringaSql;
013800121030       WrkStringaSql='select distinct blpaas, blplnp, blpnrs, blpnsp ' +
013900121030                     'from fnblp00f, fnarbd0f ' +
014000121030                     ' where' +
014100121030                     ' blpaas=arbaas and blplnp=arblnp and blpnrs=arbnrs ' +
014200121030                     ' and blpnsp=arbnsp and ''I0''=arbcvb ' +
014300121030                     ' and blpcbo<>''$ ''' +
014400121030                     ' and blpcca not in (' +
014500121106                     '''1'', ''5'',''7'',''2'',''6'',''S'')'+
014600121030                     ' and blpksc in (' ;
014700121030                     exsr sr_whrksc;
014800121030          WrkStringaSql=WrkStringaSql+WrkKscIn+
014900121030          ' and blpdcm between ' +
015000121030          %char(dl5dti) + ' and ' +
015100121030          %char(dl5dtf) ;
015200121030
015300121030       endsr;
015400121030       //-------------------------------------------------------------*
015500121030       Begsr SR_ElaboraI0;
015600121030        $finerec=*off;
015700121030        exec sql prepare STRINGASQLI from :WrkStringaSql;
015800121030        exec sql declare I0Csr cursor for StringaSqlI;
015900121030        exec sql open I0csr;
016000121030        dow $finerec=*off;
016100121030           exec sql Fetch I0csr into :blpaas, :blplnp, :blpnrs, :blpnsp;
016200121030           if sqlcod=100 or sqlcod<0;
016300121030              $finerec = *on;
016400121030              leave;
016500121030           endif;
016600121030        //
016700121030           chain (blpaas:blplnp:blpnrs:blpnsp) fnblp01l ;
016800121030              if %found(fnblp01l);
016900121031                 clear ar4not;
017000121031                 chain (blpaas:blplnp:blpnrs:blpnsp:'D') fiar401l ;
017100131014                 setll (blpaas:blplnp:blpnrs:blpnsp) fiarbf2c;
017200131014                 reade (blpaas:blplnp:blpnrs:blpnsp) fiarbf2c;
017300131014                    if %equal(fiarbf2c) and (blprsd<>arbrsd or blpind<>arbind
017400121030                                         or  blpcad<>arbcad or blpprd<>arbprd
017500121031                                         or  blplod<>arblod or blpnzd<>arbnzd
017600121031                                         or  ar4not<>arbrd2);
017700121030                       exsr wtr_wfi0;
017800121030                    endif;
017900121030              endif;
018000121030        enddo;
018100121030        exec sql close I0csr;
018200121030       endsr;
018300990608
018400121030       //-------------------------------------------------------------*
018500121030       Begsr wtr_wf    ;
018600121030       clear wftyd000;
018700121030       exsr comune;
018800121030          chain (lblaao:lbllpo:lblnro:lblnso) fnblp01l;
018900121031          clear ar4not;
019000121031          chain (lblaao:lbllpo:lblnro:lblnso:'D') fiar401l;
019100121030             if %found(fnblp01l)  and (blprsd<>tydrsdn or blpind<>tydindn
019200121030                                   or  blpcad<>tydcadn or blpprd<>tydprdn
019300121031                                   or  blplod<>tydlodn or blpnzd<>tydnzdn
019400121031                                   or  ar4not<>tydrd2n);
019500121030                TYDRSD=blprsd;
019600121030                TYDIND=blpind;
019700121030                TYDCAD=blpcad;
019800121030                TYDLOD=blplod;
019900121030                TYDPRD=blpprd;
020000121030                tydnzd=blpnzd;
020100121031                tydnzd=blpnzd;
020200121031                tydrd2=ar4not;
020300121030                write wftyd000;
020400121030             endif;
020500121030       endsr;
020600121030       //-------------------------------------------------------------*
020700121030       Begsr wtr_wfi0  ;
020800121030       clear wftyd000;
020900121030       exsr comune;
021000121030       TYDRSD=arbrsd;
021100121030       TYDIND=arbind;
021200121030       TYDCAD=arbcad;
021300121030       TYDLOD=arblod;
021400121030       TYDPRD=arbprd;
021500121030       tydnzd=arbnzd;
021600121031       tydrd2=arbrd2;
021700121030       write wftyd000;
021800121030       endsr;
021900121030       //-------------------------------------------------------------*
022000121030       Begsr comune    ;
022100121030       TYDDCD=DL5DTI;
022200121030       TYDDCA=DL5DTF;
022300121030       TYDAAS=blpaas;
022400121030       TYDLNP=blplnp;
022500121030       TYDNRS=blpnrs;
022600121030       TYDNSP=blpnsp;
022700121030       TYDDSP=(blpaas*10000)+blpmgs;
022800121030       TYDRMA=blprma;
022900121030       TYDRMN=blprmn;
023000121030       TYDKSC=blpksc;
023100121030       TYDRSdn=blprsd;
023200121030       TYDINdn=blpind;
023300121030       TYDCAdn=blpcad;
023400121030       TYDLOdn=blplod;
023500121030       TYDPRdn=blpprd;
023600121030       TYDNZdn=blpnzd;
023700121031       tydrd2n=ar4not;
023800121030       endsr;
