000100140113     H DFTACTGRP(*NO) ACTGRP('QILE') DATFMT(*ISO) BNDDIR('PJXBND')
000401151123
000402150320      *
000403150320      * Scittura file YHSCT00F e creazione file xml Distinta SEPA
000404150320      *
000405150320
000406140113     FNDDPR01L  UF   E           K DISK    commit usropn
000407140110
001200140107     D KPJBA         E DS
001201140304     D sql             s           7000
001324140117
001325150323     D EXCEPTIONMESSAGEID...
001326150323     D                 S              7A   IMPORT('_EXCP_MSGID')
001328150323
001331140227     D MY_LOG          S                   SQLTYPE(CLOB_FILE) CCSID(1252)
001332140227     D MY_XML          S                   SQLTYPE(XML_CLOB_FILE)
001333140409     D ifsdir          S            500    varying
001334140424     D ifslog          S            500    varying
001335140227     D CRLF            S              2A   Inz(x'0D25')
001336140227
001337140109     D SocK58          S              3
001338140124     D NDtK58          S              9  0
001339140109     D DDtK58          S               D
001340140109     D KccK58          S              6
001341140109     D ElaK58          S              1
001342140109     D ErrK58          S              1
001343140117     D OpzK58          S              2
001344140227     D iidd            S             35
001345140226     D alfa35          S             35
001346140303     D alfa20          S             20
001347140306     D filename        S            200    varying
001348140110     D conteggio       S              3  0
001349140303     D limite          S             11  2
001350140305     D numero          S              5  0
001351140305     D somma           S             11  2
001352140307     D textstring      S           1000
001353140307     D testo           S          32740    varying
001354140306     D cmd             S            300
001355140120
001356140113     D LckLvlChg       C                   '1'
001357140113     D LckLvlAll       C                   '2'
001358140113     D CmtScopeActGrp  C                   '1'
001359140113     D
001360140113     D xStrCmt         PR                  EXTPROC('XSTRCMT')
001361140113     D  lckLvl                        1    CONST                                Livello vincolo
001362140113     D  cmtScope                      1    CONST                                Ambito definizione s
001363140307
001364140124     D* Reperimento nome PGM
001365140124     D                SDS           333
001366140124     D  DSPGM            *PROC
001367140124     D  PARMS            *PARMS
001368140227     D  utente               254    263
001369140307
001370140307     D/COPY QSYSINC/QRPGLESRC,QUSEC
001371140307     D/COPY GAITRASRC/SRCPROTOPR,QUILNGTX
001372140306     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
001373140113
001600140109     C     *ENTRY        PLIST
001601140109     C                   PARM                    SocK58
001602140109     C                   PARM                    NDtK58
001603140109     C                   PARM                    DDtK58
001605140109     C                   PARM                    KccK58
001606140124     C                   PARM                    ElaK58
001608140117     C                   PARM                    OpzK58
001609140124     C                   PARM                    ErrK58
001701140107
001702140117      /free
001800140117                EXSR InzVar;
001801140306                EXSR Check;
001804140228                 exsr allocanddpr;
001805140228                 exsr insert;
001806140228                 EXSR validate;
001808140228                 exsr writeifs;
001809140424                 exsr chkTotali;
001810140306              exsr Uscita;
001811140306
001812140306        begsr *inzsr;
002003140224
002004140306        exec sql set option decmpt = *period , commit = *chg;
002010140113        xStrCmt(LckLvlAll:CmtScopeActGrp);
002011140113        open nddpr01l;
002012140124        endsr;
002014140113
002015140306        begsr inzvar;
002018140306
002019140306        errK58 = *off;
002020140306         //clear testo;
002021140306        //clear flgerr;
002022140117        if opzk58 <> '01'
002023140306          and opzk58 <> '02';
002024140117        return;
002025140117        ENDIF;
002026140306
002027140306           Exsr SetIfsDir;
002028140424           Exsr ChkIfsDir;
002029140306           EXSR SetId;
002030140306           Exsr SetLimite;
002031140306
003901140113        endsr;
003902140306
003903140306        begsr uscita;
003904140306
003906140306              if opzk58 = '01' and errk58 = *off;
003907140306              exsr MovIfs;
003908140306              exsr UPDATENDDPR;
003909140306              exec sql commit;
003911140306              else;
003912140306               exsr DelIfs;
003914140306               exec sql rollback;
003918140306              endif;
003920140306              return;
003921140306
003922140306        endsr;
003923140110
003924140110        begsr setid;
003925140110
003926150320        // PRIMA VERSIONE SET IIDD
003927150320        // exec sql select distinct societa concat '_' concat
003928150320        // char(datadistinta) concat '_' concat digits(nrdistinta)
003929150320        // concat '_' concat seriedistinta into :iidd
003930150320        // from YHBOS99v
003931150320        // where societa = :socK58 and tipela = :elaK58
003932150320        // and kcc = :kccK58 and datadistinta = :ddtK58 and nrdistinta = :ndtK58;
003933150320
003934150320        exec sql select distinct
003935150323          societa concat '_' concat digits(nrdistinta)
003936150320          into :iidd
003937150320          from YHBOS99v
003938150320          where societa = :socK58 and tipela = :elaK58
003939150320          and kcc = :kccK58 and datadistinta = :ddtK58 and nrdistinta = :ndtK58;
003940150320
003941140307         // textString = 'Test';
003942140307         // EXSR displayText;
003943140307
003944140306         If sqlcod < 0;
003945140307          errK58 = '1';
003946140307          textString = 'Errore reperimento id distinta. (YHBOS99V)';
003947140307          EXSR displayText;
003948140307         exsr endback;
003949140307
003950140306         endif;
003951140306
003952140306        endsr;
003953140306
003954140306        begsr setIfsDir;
003955140306
003956140424           exec sql SELECT  trim(tbeuni) CONCAT '/SCT/'
003957140424             into :IfsDir
003958140306           FROM tntbe00f WHERE tbecod='CBI'
003959140306           and tbeke1 ='*TX_IFSDIR' and trim(tbeke2) =
003960140306           (SELECT  trim(TBEUNI)
003961140306           FROM tntbe00f WHERE tbecod='CBI'
003962140424           AND TBEKE1 ='*TX_GETPROVIDER' AND TBEKE2 ='*ALL' )
003963140424           ;
003964140424
003965150310           IFSLOG = ifsdir + 'log/' ;
003966140424           IFSDIR = IFSDIR + %CHAR(%date(udate):*iso);
003967140424
003968140306        endsr;
003969140306
003970140303        begsr setLimite;
003971140303
003972140303          clear limite;
003973140306        exec sql SELECT DEC(TBEUNI , 11 , 2 , ',' )  into :limite
003974140306        from tntbe00f
003975140306        where tbecod ='CVS' and tbeke1 = 'IMPORTO_LIMITE' AND TBEKE2='EUR';
003976140306
003977140303        if limite = 0 or sqlcod <0 or sqlcod=100;
003978140307          errK58 = '1';
003979140307          textString = 'Errore reperimento importo limite. (YHBOS99V)';
003980140307          EXSR displayText;
003981140307        exsr endback;
003982140303        endif;
003983140303
003984140303        endsr;
003985140110
003986140110        begsr check;
003987140110
003988140110        exec sql select count(*) into :conteggio
003989140221        from YHSCT00F where id_Sepa = :iidd
003990140221        and flgann <> '1';
003991140221
003992140306        select;
003993140306        when sqlcod < 0;
003994140307         errK58 = '1';
003995140307          textString = 'Errore esecuzione sql verifica distinta +
003996140307          già elaborata (YHSCT00F)';
003997140307          EXSR displayText;
003998140307         exsr endback;
003999140306        when conteggio > 0;
004000140307         errK58 = '2';
004001140307          textString = 'Distinta già elaborata (YHSCT00F)';
004002140307          EXSR displayText;
004003140306         exsr endback;
004004140306        endsl;
004005140110
004006140110        endsr;
004007140117
004008140227        begsr inzsql;
004869140120
004870140227        sql = 'insert into YHSCT00F +
004871140221        ( id_sepa , id_societa , dtdis , nrdis , serdis, +
004872140221        stato , flgann , ute_imm , tm_imm , ute_mod , tm_mod, +
004873140224        distinta) +
004874140224            // values( +
004875140225        select ''' + %trimr(iidd) + ''' ,  societa , datadistinta , +
004876140227        nrdistinta , seriedistinta , +
004877140227          ''0'' , ''0'' , user , current timestamp , +
004878140227          user , current timestamp , +
004879140227
004885140227            xmldocument( +
004888140227         xmlelement(name "CBIBdyPaymentRequest" , +
004889140227         xmlnamespaces(default ''urn:CBI:xsd:CBIBdyPaymentRequest.00.04.00'', +
004890140227         ''http://www.w3.org/2001/XMLSchema-instance'' AS "xsi") , +
004891140227         xmlelement(name "CBIEnvelPaymentRequest" , +
004892140227
004894140226        xmlelement(name "CBIPaymentRequest" +
004895140227        //, xmlnamespaces(default ''urn:CBI:xsd:CBIPaymentRequest.00.04.00'') +
004896140227        // ,''http://www.w3.org/2001/XMLSchema-instance'' AS "xsi"), +
004897140226        , xmlelement(name "GrpHdr" +
004898140226        , xmlnamespaces(default ''urn:CBI:xsd:CBIPaymentRequest.00.04.00'') +
004899140226        , xmlforest( ''' + %trimr(iidd) + ''' as "MsgId", +
004900140224          //  char(curdate()) concat ''T'' concat +
004901140224          // char(curtime()) as "CreDtTm", +
004902140224        current timestamp as "CreDtTm", +
004903140120        count(*) as "NbOfTxs"  , +
004904140120        sum(dec(importo , 15 , 2)) as "CtrlSum" ,    +
004905140120        xmlforest(trim(max(sifrgs)) as "Nm",     +
004906140226        xmlelement(name "OrgId" , +
004907140225
004908140225        xmlelement(name "Othr" , +
004909140226        xmlforest( MAX(cuc) as "Id" , ''CBI'' as "Issr")) +
004910140226         // ADE commentato perchè telemaco ce l'ha respinto
004911140226         // , xmlelement(name "Othr" , +
004912140226         // xmlforest( rtrim(MAX(sifcdfisc)) as "Id" , ''ADE'' as "Issr")) +
004913140226
004914140225        )  as "Id" +
004915140303
004916140226        ) as "InitgPty" , +
004917140120        xmlforest(	xmlelement(name "ClrSysMmbId" , +
004918140120        xmlelement(name "MmbId" , max(CBAABI)  ) +
004919140120        ) as"FinInstnId"   +
004920140224        ) as "FwdgAgt") +
004921140224        ) ,  +
004922140226        xmlelement(name "PmtInf" +
004923140226        , xmlnamespaces(default ''urn:CBI:xsd:CBIPaymentRequest.00.04.00'') +
004924140226        , xmlforest( ''' + %trimr(iidd) + ''' as "PmtInfId" +
004925151123        , ''TRA'' as "PmtMtd"   +
004926140226        , xmlelement(name "SvcLvl" +
004927140226        , xmlelement(name "Cd" , ''SEPA'')) as "PmtTpInf" +
004928140226        , case when datadistinta > current date then datadistinta else +
004929140226         current date end as "ReqdExctnDt", +
004930140120        xmlforest( +
004931140120        trim(max(sifrgs)) as "Nm" +
004932140120        ) as "Dbtr", +
004933140120         xmlelement(name "Id" , xmlelement(name "IBAN" , +
004934140120         trim(max(CBAiban)))) as "DbtrAcct", +
004935140120         xmlelement(name "FinInstnId" , +
004936140306         xmlelement(name "BIC" , TRIM(max(CBABIC))) ,  +
004937140120         xmlelement(name "ClrSysMmbId" ,    +
004938140120         xmlelement(name "MmbId" , max(cbaabi) ) +
004939140120         ) +
004940140120         ) as "DbtrAgt", +
004941140120         ''SLEV'' as "ChrgBr") , +
004942140120         xmlagg(xmlelement(name "CdtTrfTxInf" , +
004943140120         xmlelement(name "PmtId" ,    +
004944140120         xmlelement(name "InstrId" , idbonifico ) , +
004945140120         xmlelement(name "EndToEndId" , idbonifico ) +
004946140120         ) , +
004947140226
004948140120         xmlelement(name "PmtTpInf" , +
004949140226          xmlelement(name "CtgyPurp" , +
004950140226           xmlelement(name "Prtry" , +
004951140226            case when substr(iban , 1 , 2) =''IT'' +
004952140226            then substr(UNIYP100 , 66 , 5) else null end +
004953140226           OPTION null on null) +
004954140226          OPTION null on null) +
004955140226         OPTION null on null) , +
004956140226
004957140120         xmlelement(name "Amt" , +
004958140225         xmlelement(name "InstdAmt" , xmlattributes(trim(y.divisa) AS "Ccy") +
004959140120         , dec(importo , 15 , 2)) +
004960140120         ) , +
004961140120         xmlelement(name "CdtrAgt" ,  +
004962140120         xmlelement(name "FinInstnId" , +
004963140120         xmlelement(name "BIC" , case when length(trim(BIC)) = 8 or +
004964140120         length(trim(BIC)) = 11  then BIC else null end OPTION null on null) +
004965140120         OPTION null on null) +
004966140120         OPTION null on null) , +
004967140120         xmlelement(name "Cdtr" ,  +
004968140120         xmlelement(name "Nm" , trim(ragionesociale)) , +
004969140226         xmlelement(name "PstlAdr" +
004970140303
004971140303        , xmlelement(name "StrtNm" , +
004972140303        case when via<>'' '' then trim(via) else null end +
004973140303        OPTION null on null) +
004974140303
004975140303        , xmlelement(name "PstCd" , +
004976140303        case when cap<>'' '' then trim(cap) else null end +
004977140303        OPTION null on null) +
004978140303
004979140303        , xmlelement(name "TwnNm" , +
004980140303        case when citta<>'' '' then trim(citta) else null end +
004981140303        OPTION null on null) +
004982140303
004985140226        , xmlelement(name "CtrySubDvsn" , +
004986140226        case when provincia<>'' '' then provincia else null end +
004987140226         OPTION null on null) +
004988140226        , xmlelement(name "Ctry" +
004989140226        , trim( value(substr(tbluni , 46 , 2 ) , nazione))) +
004990140226         // , xmlelement(name "AdrLine" , trim(VIA)) +
004991140226         // , xmlelement(name "AdrLine" , trim(CITTA)) +
004992140226         ) +
004993140120         ) , +
004994140120         xmlelement(name "CdtrAcct" , +
004995140120         xmlelement(name "Id" , +
004996140120         xmlelement(name "IBAN" , trim(IBAN)) +
004997140120         ) +
004998140120         ) , +
004999140226
005000140325
005001140325        //  "RgltryRptg" Da Marzo 2014 non è più obbligatorio
005002140325        // xmlelement(name "RgltryRptg" , +
005003140325        //  xmlelement(name "DbtCdtRptgInd" , +
005004140325        //    case when substr(iban , 1 , 2)<>''IT'' then ''DEBT'' +
005005140325        //   else null end OPTION null on null) , +
005006140325
005007140325        //  xmlelement(name "Dtls" , +
005008140325        //    xmlelement(name "Cd" , +
005009140325        //    case when substr(iban , 1 , 2)<>''IT'' then ''INF'' +
005010140325        //    else null end OPTION null on null) , +
005011140325        //    xmlelement(name "Inf" , +
005012140325        //    case +
005013140325        //    when substr(iban , 1 , 2)<>''IT'' and importo<' + %char(limite) +
005014140325        //     ' then ''' + Alfa35 + ''' +
005015140325        //    when substr(iban , 1 , 2)<>''IT'' and importo>=' + %char(limite) +
005016140325        //     ' then substr(UNIYP100 , 72 , 5) concat ''' + Alfa20 + ''' +
005017140325        //    else null end OPTION null on null) +
005018140325        //   OPTION null on null) +
005019140325        //  OPTION null on null) +
005020140226
005021140409          xmlelement(name "RmtInf" , +
005022140228         xmlelement(name "Ustrd" , +
005023140120         case when length(trim( +
005024140120         substr(getdispdes(societa , datadistinta ,nrdistinta  +
005025140120         , IDBONIFICO) , 1 ,140))) = 0 then null else +
005026140120         trim(substr(getdispdes(societa , datadistinta ,nrdistinta  +
005027140120         , IDBONIFICO) , 1 ,140)) end OPTION null on null) , +
005028140228         xmlelement(name "Ustrd" ,  +
005029140120         case when length(trim( +
005030140120         substr(getdispdes(societa , datadistinta ,nrdistinta  +
005031140120         , IDBONIFICO) , 141 ,140))) = 0 then null else +
005032140120         trim(substr(getdispdes(societa , datadistinta ,nrdistinta  +
005033140120         , IDBONIFICO) , 141 ,140)) end OPTION null on null) , +
005034140228         xmlelement(name "Ustrd" ,  +
005035140120         case when length(trim( +
005036140120         substr(getdispdes(societa , datadistinta ,nrdistinta  +
005037140120         , IDBONIFICO) , 281 ,140))) = 0 then null else +
005038140120         trim(substr(getdispdes(societa , datadistinta ,nrdistinta  +
005039140120         , IDBONIFICO) , 281 ,140)) end OPTION null on null) , +
005040140228         xmlelement(name "Ustrd" ,  +
005041140120         case when length(trim( +
005042140120         substr(getdispdes(societa , datadistinta ,nrdistinta  +
005043140120         , IDBONIFICO) , 421 ,30))) = 0 then null else +
005044140120         trim(substr(getdispdes(societa , datadistinta ,nrdistinta  +
005045140120         , IDBONIFICO) , 421 ,30)) end OPTION null on null) +
005046140120         OPTION null on null) +
005047140120         ) +
005048140120         ) +
005049140120         ) +
005050140120         ) +
005051140227         ))) +
005052140227
005053140227          from YHBOS99v as y +
005054140120         join ansif00f on societa=sifsocieta  +
005055140120         join ancba00f on societa = cbasocieta +
005056140120         and CapocontoBanca = cbakcc and SottocontoBanca = cbaksc +
005057140120         left join tabel00f on tblkut = 1 +
005058140120         and TBLCOD =''15''  and tblkey = nazione +
005059140120         left join atbyp100v on +
005060140120         societa = SOCYP100 +
005061140120         and kcc = CODYP100 +
005062140225         inner join azsoc00f on sifsocieta = idsocieta +
005063140225         and current date between valdatiniz and valdatfine +
005064140120         where societa = ''' + socK58 +
005065140120         ''' and tipela = ''' + elaK58 +
005066140120         ''' and kcc = ''' + kccK58 +
005067140120         ''' and datadistinta = ''' + %char(ddtK58) +
005068140227         ''' and nrdistinta = ' + %char(ndtK58) +
005069140228          ' group by societa,  datadistinta , nrdistinta , seriedistinta +
005070140224         ';
005071140120
005072140120         endsr;
005073140120
005074140117         begsr allocaNddpr;
005075140110
005076140110         Chain (SocK58:'CG':'3':NDtK58:DDtK58) NdDpr01l;
005077140110         if not %found(NdDpr01l);
005078140307          errK58 = '1';
005079140307          textString = 'Non reperita testata distinta !!!';
005080140307          EXSR displayText;
005081140307          exsr endback;
005082140110         ENDIF;
005083140110
005084140110         endsr;
005085140110
005086140110         begsr updateNddpr;
005087140110
005088140110         dprhomeb = *on;
005089140110         Update(e) NdDpr000 %fields(dprhomeb);
005090140110         if %error;
005091140306          // flgerr = 'E';
005092140306          // testo = 'errore in update NDDPR';
005093140227           errK58 = '1';
005094140307           textString = 'Errore in aggiornamento testata Distinta';
005095140307           EXSR displayText;
005096140307           exsr endback;
005097140227         else;
005098140227           errK58 = '0';
005099140227         endif;
005100140306          // exsr updateCBI;
005101140306         endsr;
005102140117
005103140306         //begsr updateCBI;
005104140306         //exec sql update  YHSCT00F set stato = :flgerr , log = :testo
005105140306         //where id_Sepa = :iidd and flgann <> '1';
005106140306         //endsr;
005107140227
005108140227         begsr insert;
005109140227
005110140227           // compongo stringa senza validazione schema
005111140227         exsr inzsql;
005112140227         exec sql execute immediate :sql;
005113140227
005114140306         If sqlcod < 0;    // errore in insert
005115140307          exec sql get diagnostics :testo = all;
005116140307          exsr logsql;
005117140307          errK58 = '1';
005118140307          textString = 'Errore insert YHSCT00F --- '+ testo;
005119140307          EXSR displayText;
005120140307          exsr endback;
005122140306         endif;
005123140306
005124140227         If utente = 'EDPPF';         // LOG FELLETTI
005125140227         exsr logsql;
005126140227         endif;
005127140227
005128140227         endsr;
005129140227
005130140227         begsr validate;
005131140227
005132140306         exec sql update  YHSCT00F set distinta =
005133140306          xmlvalidate(document xmldocument(DISTINTA)
005136140227          according to xmlschema id GAITRAOBJ.YHSCT0XSR)
005137140306          where id_Sepa = :iidd and flgann <> '1';
005138140227
005139140227          select;
005140140306          when sqlcod < 0;    // "errore" in fase di validazione schema
005141140307           exec sql get diagnostics :testo = all;
005142140306           exsr logsql;
005143140306           exsr logdistinta;
005144140307           errK58 = '1';
005145140307           textString = 'Errore in fase di validazione schema - ' + testo;
005146140307           EXSR displayText;
005147140307         //flgerr = 'E';
005148140306         //exsr updateCBI;
005149140306         // when opzK58 = '02';    // solo controllo "ok"
005150140306         // errK58 = '0';
005151140306         // exec sql rollback;
005152140306         // exsr msgok;
005153140306         endsl;
005154140306
005159140227         endsr;
005160140227
005161140424         begsr chkTotali;
005162140227
005163140306               filename = ifslog + %trim(iidd) + '.xml';
005164140305
005170140305               exec sql  select NbOfTxs
005171140305                      , CtrlSum
005172140305                    into :numero , :somma
005173140305          FROM XMLTABLE (xmlnamespaces(
005174140305
005175140305            default 'urn:CBI:xsd:CBIBdyPaymentRequest.00.04.00',
005176140305            'urn:CBI:xsd:CBIPaymentRequest.00.04.00' as "n2") ,
005177140305           '$r/CBIBdyPaymentRequest/CBIEnvelPaymentRequest/+
005178140305           CBIPaymentRequest/n2:GrpHdr'
005179140306           passing xmlparse(document get_xml_file(:filename)) as "r"
005182140306           COLUMNS
005187140306               NbOfTxs  dec(7 , 0) PATH 'n2:NbOfTxs'
005188140306               , CtrlSum  dec(13 , 2) PATH 'n2:CtrlSum'
005189140306              ) as x;
005191140305
005192140305         select;
005193140307         when sqlcod < 0;
005194140306           eval errk58 = '1';
005195140307           textString = 'Errore in lettura XML generato (log folder)';
005196140307           EXSR displayText;
005197140306           exsr endback;
005198140306         //flgerr = 'E';
005199140306         //testo = 'errore in controllo IFS: ' + %trim(iidd);
005200140306         //exsr updateCBI;
005201150710         when somma <> DPRIMPORTD or numero <> DPRNRELEM;
005202140307           eval errk58 = '1';
005203140307           textString = 'Testata Distinta non corrisponde con +
005204140307           XML generato (importo totale o numero bonifici)';
005205140307           EXSR displayText;
005206140306           exsr endback;
005207140306         //flgerr = 'E';
005208140306         //testo = 'errore in controllo IFS: ' + %trim(iidd);
005209140306         //exsr updateCBI;
005210140306         endsl;
005211140227
005212140224         endsr;
005213140306
005214140306         begsr movifs;
005215140306
005216150326            exsr dlyjob;
005217150323
005218140306            cmd = 'MOV OBJ(''' + IFSlog + %TRIM(IIDD) +
005219140306             '.xml'') TODIR(''' + ifsdir +''')';
005220140306
005221140306            if executeCommand(%trim(cmd))
005222140306                <> 0;
005223140306            errk58 = '1';
005224150323            textString = 'Errore Move XML (folder trasmissione): ' +
005225150323                 EXCEPTIONMESSAGEID;
005226140307            EXSR displayText;
005227140306            exsr endback;
005228140306            //   rpyEsito = -1;
005229140306            //   rpyOpCode = exceptionMessageID;
005230140306            endif;
005231140306
005232140306         endsr;
005233140224
005234140306         begsr delifs;
005235140306
005236140306            cmd = 'del ''' + ifslog + %trim(iidd) + '.xml''';
005237140307            if executeCommand(%trim(cmd)) <> 0;
005238140307             errk58 = '1';
005239140307            textString = 'Errore cancellazione XML (log folder)';
005240140307            EXSR displayText;
005241140307             exsr endback;
005242140306            endif;
005243140306
005244140306         endsr;
005245140306
005246140305         begsr writeIfs;
005247140305
005248140306               my_xml_name = ifslog + %trim(iidd) + '.xml';
005249140306               my_xml_fo = SQFOVR;
005250140305               my_xml_nl = %len(%trim(my_xml_name));
005251140305
005252140306               exec sql select distinta
005253140305                        into :my_xml
005254140305                        from YHSCT00F
005255140305                        where id_sepa=:iidd and flgann<>'1';
005256140305
005257140305         if sqlcod < 0; // errore in scrittura IFS
005258140306           errK58 = '1';
005259140307            textString = 'Errore write XML (log folder)';
005260140307            EXSR displayText;
005261140307           exsr endback;
005262140306         // flgerr = 'E';
005263140306         // testo = 'errore in scrittura IFS: ' + %trim(iidd);
005264140306         // exsr updateCBI;
005265140306         endif;
005266140305
005267140305         endsr;
005268140307
005269140424         begsr chkIfsDir;
005270140424
005271140424            cmd = 'MD DIR(''' + IFSDIR + ''')';
005272140424
005273140424            if executeCommand(%trim(cmd))
005274140424                <> 0;
005275140424            //errk58 = '1';
005276140424            //textString = 'Errore Create ifsdir (folder trasmissione)';
005277140424            // EXSR displayText;
005278140424            // exsr endback;
005279140424                  //   rpyEsito = -1;
005280140424                  //   rpyOpCode = exceptionMessageID;
005281140424            endif;
005282140424
005283140424         endsr;
005284140424
005285140307       //********************************************************************************
005286140307       // Richiamo a Display Long Text (QUILNGTX) API.
005287140307       //********************************************************************************
005288140307            BEGSR displayText;
005289140307
005290140307                  RESET qusec;
005291140307                  qusbprv = %SIZE(qusec);
005292140307                  DisplayLongText( textString
005293140307                                 : %SIZE(textString)
005294140307                                 : 'BAR0019'
005295140307                                 : 'YBARMSG   *LIBL'
005296140307                                 : qusec
005297140307                                 );
005298140307
005299140307            ENDSR;
005300140307
005301140305
005302140227         begsr logsql;
005303140227
005304140227           my_log_name = ifslog + 'sql.log';
005305140227           my_log_fo = SQFAPP;
005306140227           my_log_nl = %len(%trim(my_log_name));
005307140224
005308140227           exec sql set :my_log = :crlf concat
005309140227            'LOG_SQL___' concat char(current timestamp)
005310140227            concat '_' concat trim(user) concat '_' concat :CRLF
005311140227             concat trim(:sql)  concat :CRLF;
005312140227
005313140224         ENDSR;
005314140227
005315140227         begsr logdistinta;
005316140227
005317140227           my_log_name = ifslog + 'sql.log';
005318140227           my_log_fo = SQFAPP;
005319140227           my_log_nl = %len(%trim(my_log_name));
005320140227
005321140227            exec sql select :crlf concat 'LOG_DISTINTA___' concat
005322140227              char(current timestamp)
005323140227               concat '_' concat trim(user) concat '_' concat :CRLF
005324140306                concat xmlserialize(distinta as clob) concat :CRLF
005325140227              into :my_log
005326140227               from YHSCT00F
005327140227                where id_sepa=:iidd and flgann<>'1';
005328140227
005329140306         ENDSR;
005330140306
005331140306         begsr endback;
005332140306
005333140306           exec sql rollback;
005334140306          return;
005335140306
005336140306         ENDSR;
005337150323
005338150323         begsr dlyjob;
005339150323
005340150323            cmd = 'DLYJOB dly(2)';
005341150323            if executeCommand(%trim(cmd))
005342150323                <> 0;
005343150323            errk58 = '1';
005344150323            textString = 'Errore Dly job';
005345150323            EXSR displayText;
005346150323            exsr endback;
005347150323            endif;
005350150323
005353150323         endsr;
005354150323
005356140113      /end-free
