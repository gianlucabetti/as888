000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000700990317      ****************************************************************
000800980521     H DECEDIT('0,') DATEDIT(*DMY.)
000900980623
001000990602      *------------------------------------------------------------------------*
001400121029     ffnlbl01l  IF   E           K DISK
001401121030     ffnblp01l  IF   E           K DISK
001402121031     ffiar401l  IF   E           K DISK
001403121030     ffiarbf1c  IF   E           K DISK
001404121030     f                                     ignore(fnarbk00)
001405121030     f                                     ignore(fiarbt00)
001406121030     f                                     ignore(fnarbg00)
001407121030     f                                     ignore(fnarbm00)
001408121030     f                                     ignore(fnarbv00)
001409121030     f                                     ignore(fnarbp00)
001500121030     fwftyd00f  o  a E             DISK    usropn
001800980521      *------------------------------------------------------------------------*
001900980702
002000990602      * Definizione campi di work ------------------------------------------
002001121029     d i               s              3  0 inz
002002121029     d Primo           s              4  0 inz
002003121030     d Qlen            s             15  5 inz(%len(Qcmd))
002004121030     d Qcmd            s             80    inz
002005121030     d $Finerec        s              1    inz(*off)
004800990601
004900990602      * DS esterne ---------------------------------------------------------
006000990602
006100121029     D TNVRRMTRDS    E DS
006200121029     d ksc                            7  0 dim(10) overlay(TNVRRMTRDS:34)
006300990602
006500990602     D KPJBA         E DS
007600980525      *
015300020911     D*-------------
015400020911     D* DS DI WRK
015500020911     D*-------------
015801121029     d fnblpds       e ds                  extname(fnblp00f) inz
015803121029
015804121029     D WrkStringaSql   S           4500
015805121029     D                                     VARYING
015808121029     D WrkKscIn        S           4000
015809121029     D                                     VARYING
015810121030      * - Comando per pulizia Work-File
015811121030     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
015812121030     d                                                   WFTYD00F)')
016800990603
018400980521      *
018401121029     C     *entry        plist
018402121029     C                   parm                    kpjba
018403121029     C                   movel     kpjbu         TNVRRMTRDS
018404121030
018405121030     c* Pulizia work file prima di scriverlo
018406121030     c                   call      'QCMDEXC'
018407121030     c                   parm      C_Cmd_ClrWF   Qcmd
018408121030     c                   parm                    Qlen
018409121030     c
018410121030     c                   open      WFtyd00f
018411121029      /free
018412121029         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018413121030
018414121030       // PRIMO GIRO: elaboro le bolle figlie di un dirottamento
018415121029       exsr sr_prepsql;
018416121029       // elaboro dati estratti
018417121029       exsr sr_elabora;
018418121030
018419121030       // SECONDO GIRO: elaboro le bolle sulle con variazioni "I0"=Destinatario
018420121030       exsr sr_prepsqlI0;
018421121030       // elaboro dati estratti
018422121030       exsr sr_elaboraI0;
021400980623
050800121029       *inlr=*on;
050801121029       //-------------------------------------------------------------*
050802121029       //Preparazione stringa sql per elaborazione figlie di dirottam *
050803121029       //-------------------------------------------------------------*
050804121029       Begsr SR_prepSql;
050805121029
050806121029       WrkStringaSql='select * from fnblp00f' +
050807121029                     ' where' +
050808121029                     ' blpcbo=''$ ''' +
050809121030                     ' and blpcca not in (' +
050810121106                     '''1'', ''5'',''7'',''2'',''6'',''S'')'+
050811121029                     ' and blpksc in (' ;
050812121029                     exsr sr_whrksc;
050813121029          WrkStringaSql=WrkStringaSql+WrkKscIn+
050814121029          ' and blpdcm between ' +
050815121029          %char(dl5dti) + ' and ' +
050816121029          %char(dl5dtf) ;
050826121029
050827121029       endsr;
050828121029       //-------------------------------------------------------------*
050829121029       Begsr SR_whrksc;
050830121029          Primo= 1 ;
050831121029
050832121029          reset WrkKscIn;
050838121029          for I=1 to 10;
050839121029             if ksc(i)=0;
050840121029                leave;
050841121029             endif;
050848121029             if primo>1;
050849121029                WrkKscIn=WrkKscIn + ' , ';
050850121029             endif;
050852121029             WrkKscIn=WrkKscIn + %char(Ksc(i));
050853121029             Primo=primo+1   ;
050855121029          endfor;
050856121029          WrkKscIn=WrkKscIn + ')';
050857121029       endsr;
050858121029       //-------------------------------------------------------------*
050859121029       Begsr SR_Elabora;
050860121029        $finerec=*off;
050861121029        exec sql prepare STRINGASQL from :WrkStringaSql;
050862121029        exec sql declare BLPCsr cursor for StringaSql;
050863121029        exec sql open BLPcsr;
050864121029        dow $finerec=*off;
050865121029           exec sql Fetch BLPcsr into :fnblpds ;
050866121029           if sqlcod=100 or sqlcod<0;
050867121029              $finerec = *on;
050868121029              leave;
050870121029           endif;
050871121031        // Aggancio fiar4 per seconda ragione sociale
050872121031           clear ar4not;
050873121031           chain (blpaas:blplnp:blpnrs:blpnsp:'D') fiar401l ;
050874121029        // Cerco la bolla mamma
050875121029           chain (blpaas:blplnp:blpnrs:blpnsp) fnlbl01l ;
050876121030           if %found(fnlbl01l);
050879121030              exsr wtr_wf;
050880121030           endif;
050881121029        enddo;
050882121030        exec sql close BLPcsr;
050883121030       endsr;
050884121030       //-------------------------------------------------------------*
050885121030       //Preparazione stringa sql per elaborazione varizioni I0       *
050886121030       //-------------------------------------------------------------*
050887121030       Begsr SR_prepSqlI0;
050888121030
050889121030       clear WrkStringaSql;
050890121030       WrkStringaSql='select distinct blpaas, blplnp, blpnrs, blpnsp ' +
050891121030                     'from fnblp00f, fnarbd0f ' +
050892121030                     ' where' +
050893121030                     ' blpaas=arbaas and blplnp=arblnp and blpnrs=arbnrs ' +
050894121030                     ' and blpnsp=arbnsp and ''I0''=arbcvb ' +
050895121030                     ' and blpcbo<>''$ ''' +
050896121030                     ' and blpcca not in (' +
050897121106                     '''1'', ''5'',''7'',''2'',''6'',''S'')'+
050898121030                     ' and blpksc in (' ;
050899121030                     exsr sr_whrksc;
050900121030          WrkStringaSql=WrkStringaSql+WrkKscIn+
050901121030          ' and blpdcm between ' +
050902121030          %char(dl5dti) + ' and ' +
050903121030          %char(dl5dtf) ;
050904121030
050905121030       endsr;
050906121030       //-------------------------------------------------------------*
050907121030       Begsr SR_ElaboraI0;
050908121030        $finerec=*off;
050909121030        exec sql prepare STRINGASQLI from :WrkStringaSql;
050910121030        exec sql declare I0Csr cursor for StringaSqlI;
050911121030        exec sql open I0csr;
050912121030        dow $finerec=*off;
050913121030           exec sql Fetch I0csr into :blpaas, :blplnp, :blpnrs, :blpnsp;
050914121030           if sqlcod=100 or sqlcod<0;
050915121030              $finerec = *on;
050916121030              leave;
050917121030           endif;
050918121030        //
050919121030           chain (blpaas:blplnp:blpnrs:blpnsp) fnblp01l ;
050920121030              if %found(fnblp01l);
050921121031                 clear ar4not;
050922121031                 chain (blpaas:blplnp:blpnrs:blpnsp:'D') fiar401l ;
050923121030                 setll (blpaas:blplnp:blpnrs:blpnsp) fiarbf1c;
050924121030                 reade (blpaas:blplnp:blpnrs:blpnsp) fiarbf1c;
050925121030                    if %equal(fiarbf1c) and (blprsd<>arbrsd or blpind<>arbind
050926121030                                         or  blpcad<>arbcad or blpprd<>arbprd
050927121031                                         or  blplod<>arblod or blpnzd<>arbnzd
050928121031                                         or  ar4not<>arbrd2);
050929121030                       exsr wtr_wfi0;
050930121030                    endif;
050931121030              endif;
050932121030        enddo;
050933121030        exec sql close I0csr;
050934121030       endsr;
061400990608
061401121030       //-------------------------------------------------------------*
061402121030       Begsr wtr_wf    ;
061404121030       clear wftyd000;
061405121030       exsr comune;
062104121030          chain (lblaao:lbllpo:lblnro:lblnso) fnblp01l;
062105121031          clear ar4not;
062106121031          chain (lblaao:lbllpo:lblnro:lblnso:'D') fiar401l;
062107121030             if %found(fnblp01l)  and (blprsd<>tydrsdn or blpind<>tydindn
062108121030                                   or  blpcad<>tydcadn or blpprd<>tydprdn
062109121031                                   or  blplod<>tydlodn or blpnzd<>tydnzdn
062110121031                                   or  ar4not<>tydrd2n);
062113121030                TYDRSD=blprsd;
062114121030                TYDIND=blpind;
062115121030                TYDCAD=blpcad;
062116121030                TYDLOD=blplod;
062117121030                TYDPRD=blpprd;
062118121030                tydnzd=blpnzd;
062119121031                tydnzd=blpnzd;
062120121031                tydrd2=ar4not;
062121121030                write wftyd000;
062122121030             endif;
062200121030       endsr;
062300121030       //-------------------------------------------------------------*
062400121030       Begsr wtr_wfi0  ;
062500121030       clear wftyd000;
062501121030       exsr comune;
064600121030       TYDRSD=arbrsd;
064700121030       TYDIND=arbind;
064800121030       TYDCAD=arbcad;
064900121030       TYDLOD=arblod;
065000121030       TYDPRD=arbprd;
065100121030       tydnzd=arbnzd;
065101121031       tydrd2=arbrd2;
065400121030       write wftyd000;
065500121030       endsr;
065501121030       //-------------------------------------------------------------*
065502121030       Begsr comune    ;
065600121030       TYDDCD=DL5DTI;
065700121030       TYDDCA=DL5DTF;
065800121030       TYDAAS=blpaas;
065900121030       TYDLNP=blplnp;
066000121030       TYDNRS=blpnrs;
066100121030       TYDNSP=blpnsp;
066200121030       TYDDSP=(blpaas*10000)+blpmgs;
066300121030       TYDRMA=blprma;
066400121030       TYDRMN=blprmn;
066500121030       TYDKSC=blpksc;
066600121030       TYDRSdn=blprsd;
066700121030       TYDINdn=blpind;
066800121030       TYDCAdn=blpcad;
066900121030       TYDLOdn=blplod;
067000121030       TYDPRdn=blpprd;
067100121030       TYDNZdn=blpnzd;
067102121031       tydrd2n=ar4not;
067300121030       endsr;
