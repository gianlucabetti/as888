000100110617      *---------------------------------------------------------------*
000200110627      * Avviso al destinatario di affidamento spedizione              *
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600130503     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000170124     ffiar503l  if   e           k disk    infds(infAR503)
001100170124     f                                     rename(fiar5000:fiar5030)
001200170124     ffiar500f  uf   e             disk
001300130408     ffiar501l  if   e           k disk    prefix(gen_)
001400170124     f                                     rename(fiar5000:fiar5010)
001500130408     ffiar804l  uf   e           k disk
001600110617     ffiar901l  if   e           k disk
001700110617     fazorg01l  if   e           k disk
001800110617     ftabel00f  if   e           k disk
001900110617     ftntbe01l  if   e           k disk
002000160506     ftidp300f  uf a e             disk
002100110617
002200120619     D trul21r         PR
002300120619     D                                     EXTPGM('TRUL21R')
002400130406     D  trul21ds                           likeds(trul21ds)
002500140324
002600140324
002700140324     D TRULORSR        PR
002800140324     D                                     extpgm('TRULORSR')
002900140324     D kpjba                               likeds(kpjba)
003000140324     D trulORsds                           likeds(trulORsds)
003100140324     D trulOR2ds                           likeds(trulOR2ds)
003200140414
003300140414
003400140414     D FIDNA6R         PR
003500140414     D                                     extpgm('FIDNA6R')
003600140414     D fidna6ds                            likeds(fidna6ds)
003700141013
003800141013
003900141013     D TRUL28R0        PR
004000141013     D                                     extpgm('TRUL28R0')
004100141013     D trul28ds0                           likeds(trul28ds0)
004200130406
004300140324
004400110617     D trul33r         PR
004500110617     D                                     EXTPGM('TRUL33R')
004600130406     D  kpjba                              likeds(kpjba)
004700140725
004800110617
004900130406     d ds3a          E DS
005000141128     d ds7r          e ds
005100130406     d dcli          E DS
005200140401     d og129         e ds                  inz
005300141016     d og130         e ds                  inz
005400150929     D og143         e ds                  inz
005500150929     D dntw          e ds                  inz
005600110617     d dar5emd       e ds
005700130406     d dar5gen       e ds
005800130406     d dar8§pdf      e ds
005900130406     d pdftabel      e ds
006000130406     d kpjba         E DS
006100110617     d trul33ds      e ds
006200120619     d trul21ds      e ds
006300120619     d tnsd99ds      e ds
006400141013     D trul28ds0     e ds                  qualified inz
006500140414     d fidna6ds      e ds                  inz
006600140324     d trulORsds     e ds                  inz
006700140324     d trulOR2ds     e ds                  inz
006800120202     d fnlv55ds      e ds
006900130625     d fnblpds       e ds                  extname('FNBLP00F')
007000130625     d fnlvemds      e ds
007100150730     d diore01       e ds
007200170221     D UBRSMALDS     E DS                  QUALIFIED
007300110627     d cbc             S              2    DIM(100) inz
007400110628     d i               s              3  0 inz
007500130829     d y               s              3  0 inz
007600130829     d cbf             S              2    DIM(100) inz
007700110617      * - Campi di comodo
007800110617     d bolok           s              1
007900131003     d bologgi         s              1
008000110617     d kdto            s                   like(ar5dto) inz
008100110617     d Dataeur         s               d   datfmt(*eur)
008200110617     d Dataiso         s               d   datfmt(*iso)
008300110617     d wdataspe        s              8  0
008400130408     d wPOspe          s              3A   inz
008500130827     d wPOcli          s              3A   inz
008600170221     d wRSM_Eml        s             60A   inz
008700110621     d war9cas         s             12  2
008800110617     d string          s            200a   varying
008900110617     d strTRBM         s          65535a   varying
009000161110     d strTRBMATCH     s          65535a   varying
009100120202     d wlna            s                   like(blplna)
009200120619     d wke1            s                   like(tbeke1)
009300120619     d wcliper         s                   like(§cliemd)
009400141128     d wPINcode        s              1    inz
009500120619     d wdcr            s                   like(blpdcr)
009600140327     d wdcrRev         s                   like(blpdcr)
009700130406     d pesito          S             10I 0
009800161111     d wEmlSnt         S              4S 0 inz
009900141128     d sk7R_PINcode    s              2    dim(100) inz
010000141128     d ix7R            s              3  0 inz
010100150929     D skFilExt        s                   like(orgFIL) dim(500) inz
010200160506     D skFilExtDPD     s                   like(orgFIL) dim(500) inz
010300150929     D ixFilExt        s              3  0 inz
010400161118     D wOK_Atch        s               n   inz
010500170116     D wOBJM           s             20a   inz
010600170124     D wDivid          s                   like(divid)
010700141013
010800170124
010900170124     D infAR503        DS
011000170124     D   ar503_rrn           397    400I 0
011100170124
011200141013
011300141013     D BRTcode         ds                  qualified
011400141013     D   aas4                         4  0
011500141013     D   lnp                          3  0
011600141013     D   nrs                          2  0
011700141013     D   nsp                          7  0
011800141013     D   chkdgt1                      1  0
011900141013     D   code                         3  0
012000141013     D   chkdgt2                      1  0
012100141013     D   BRTcode               3     21
012200141013
012300140725
012400130406
012500130406     D* DICHIARAZIONE VARIABILI X INVIO PDF IN ALLEGATO
012600130406
012700130406     D FNLSPDFC2       PR
012800130406     D                                     extpgm('FNLSPDFC2')
012900130406     D iOpz                           1A
013000130406     D iPathOrig                    120A
013100130406     D iPathCli                            like(§AR8PPDFP)
013200130406     D iFileName                    256A
013300130406     D iPathZip                     120A
013400130503     D iShare                       120A
013500130406     D iProgress                     16A
013600130408     D iEsito                         1A
013700130406
013800130406     D PDF100R         PR
013900130406     D                                     extpgm('PDF100R')
014000130406     D iTBKEY                              like(TBKEY)
014100130406     D iTBELE                              like(TBELE)
014200130416     D iPGM                          10A
014300130406     D iTBDAT                              like(TBDAT)
014400130408     D iEsito                         1A
014500130406
014600161118     D FNLSPDFR1       PR
014700130406     D                                     extpgm('FNLSPDFR1')
014800130406     D iPathOrig                    120A
014900130406     D iPathCli                            like(§AR8PPDFP)
015000130430     D iNomePDF                      61A
015100130416     D iCheckOnly                     1A
015200130406     D iFileName                    256A
015300130408     D iEsito                         1A
015400161118
015500161118     D FNLSEMAR        PR
015600161118     D                                     extpgm('FNLSEMAR')
015700161118     D iPATH                        120A
015800161118     D iAAS                           4  0
015900161118     D iLNP                           3  0
016000161118     D iNRS                           2  0
016100161118     D iNSP                           7  0
016200161118     D oOK                            1
016300130625
016400130625     D FNLVEMR         PR
016500130625     D                                     extpgm('FNLVEMR')
016600130625     D iKPJBA                              likeds(kpjba)
016700130625     D iFNBLP                              likeds(fnblpds)
016800170201
016900170201     D TRULVPOR        PR
017000170201     D                                     extpgm('TRULVPOR')
017100170201     D trulvpods                           likeds(TRULVPODS)
017200130406
017300131210     D TIS701C1        PR
017400131210     D                                     extpgm('TIS701C1')
017500140423     D wEml                         253A
017600140423     D wCcEml                       253A
017700140423     D wOgg                          44A
017800140423     D wMsg                        5000A
017900140423     D wEsito                         1A
018000170201
018100170201
018200131210
018300140806     d wTipoEml        s              5A   inz
018400130406     d wAllega         s               N   inz(*off)
018500130406     d w§CM1VAR        s                   like(§CM1VAR)  inz
018600130406     d w§CM1PROG       s                   like(§CM1PROG) inz
018700130406     d pTBKEY          s                   like(TBKEY)
018800130406     d pTBELE          s                   like(TBELE)
018900130416     d pPGM            s             10A
019000130406     d pTBDAT          s                   like(TBDAT)
019100130406     d pPathCli        s                   like(§AR8PPDFP)
019200130430     d pNomePDF        s             61A
019300130416     d pCheckOnly      s              1A
019400130406     d pOpz            s              1A
019500130406     d pPathOrig       s            120A
019600161110     d pPathOrigWin    s            120A
019700130406     d pFileName       s            256A
019800130406     d pPathZipAS      s            120A
019900130503     d pShare          s            120A
020000130406     d pPathZipSRV     s            120A
020100130406     d pProgress       s             16A
020200130408     d mEsito          s              1A
020300140808     d wCnt            s              3  0
020400140808     d wOpz            s              1A
020500140808     d pTyp            s              1A
020600140808     d pTry            s              3  0
020700140423     d wEml            s            253A
020800140423     d wCcEml          s            253A
020900140423     d wOgg            s             44A
021000140423     d wMsg            s           5000A
021100140423     d wEsito          s              1A
021200141016     D wBRTcode        s             19                   inz
021300141016     D wBRTcodeLong    s             25                   inz
021400140623     d wSpedConsOrari  s             64A                  inz
021500140324     d wSpedConsOraDa  s              5A                  inz
021600140324     d wSpedConsOraA   s              5A                  inz
021700141016     d wLnaOraMMD_A    s              5A                  inz
021800141016     d wLnaOraMMA_A    s              5A                  inz
021900141016     d wLnaOraMPD_A    s              5A                  inz
022000141016     d wLnaOraMPA_A    s              5A                  inz
022100141016     d wLnaOraTMD_A    s              5A                  inz
022200141016     d wLnaOraTMA_A    s              5A                  inz
022300141016     d wLnaOraTPD_A    s              5A                  inz
022400141016     d wLnaOraTPA_A    s              5A                  inz
022500140423     d wConsegna       s           1000A   varying        inz
022600170206     d wConsegnaN      s           1000A   varying        inz
022700170202     d wVPOdeploy      s              6A   inz
022800170202     d wTypAlert       s              1A   inz
022900170202     d wRiga01         s           2000A   varying        inz
023000170202     d wRiga02         s           2000A   varying        inz
023100170202     d wRiga03         s           2000A   varying        inz
023200170202     d wRiga04         s           2000A   varying        inz
023300170202     d wRiga05         s           2000A   varying        inz
023400170202     d wRiga06         s           2000A   varying        inz
023500170202     d wRiga07         s           2000A   varying        inz
023600170202     d wRiga08         s           2000A   varying        inz
023700170711     d wTagRSM         s             35A   varying        inz
023800170202     d wTagV01         s             35A   varying        inz
023900170202     d wTagV02_1       s             35A   varying        inz
024000170202     d wTagV02_2       s             35A   varying        inz
024100170202     d wTagV02_3       s             35A   varying        inz
024200170202     d wTagV02_4       s             35A   varying        inz
024300170202     d wTagV03_1       s             35A   varying        inz
024400170202     d wTagV03_2       s             35A   varying        inz
024500170202     d wTagV04         s             35A   varying        inz
024600170202     d wTagV05         s             35A   varying        inz
024700170202     d wTagV06         s             35A   varying        inz
024800170202     d wTagV07_1       s             35A   varying        inz
024900170202     d wTagV07_2       s             35A   varying        inz
025000170202     d wTagV07_3       s             35A   varying        inz
025100170202     d wTagV07_4       s             35A   varying        inz
025200170202     d wTagV07_5       s             35A   varying        inz
025300170202
025400120619
025500170201     D TRULVPODS     e ds
025600170201     D  skFilvpo              16    765    dim(250)
025700170201     D skFil_New       s                   like(skFilvpo) dim(250) inz
025800120619
025900120619     d                sds
026000120619     d  nompgm                 1     10
026100120619     d  SDSusr               254    263
026200110617
026300110617      // ? PROTOTIPI ?
026400110617      /copy gaitrasrc/srcprotopr,trbmbypsr
026500120202      /copy gaitrasrc/srcprotopr,fnlv55r
026600120619      /copy gaitrasrc/srcprotopr,tnsd99r
026700170221      /copy gaitrasrc/srcProtoPr,UBRSMALR
026800110617
026900130406
027000110627     C     *ENTRY        PLIST
027100110627     C                   PARM                    chiudi            1
027200170124     C                   PARM                    divis             2 0
027300170124     C                   PARM                    divid             2 0
027400130406
027500110617      /free
027600140806
027700140806       // Inizializzo flag di errore globale procedura
027800140806       mEsito = '0';
027900140806
028000130604       // Carico le tabelle occorrenti
028100110627       exsr sr_cartab;
028200170201
028300170201       // carico le filiali per rilasci pilotati
028400170201       exsr sr_VPOR;
028500130625
028600130604       // Controllo se è raggiungibile la cartella remota su server
028700140808       exsr sr_OpzChkNet;
028800130604       if mEsito = '2';
028900130604       else;
029000130604          // Leggo fiar5 rk "EMD" da inviare
029100130604          exsr sr_Elabora;
029200130604       endif;
029300130406
029400110627       if chiudi='S';
029500140623          eval *inlr=*on;
029600130625          clear fnlvemds;
029700130625          iemTLA = 'C';
029800130625          kpjbu = fnlvemds;
029900130625          callP(e) FNLVEMR (kpjba : fnblpds);
030000140623       else;
030100140725          eval *inrt=*on;
030200140725       endif;
030300140725
030400170124       unlock(e) fiar500F;
030500140725       unlock(e) fiar804L;
030600160506
030700160506
030800160506       //-------------------------------------------------------------*
030900160506       //inserimento Predict DPD                                      *
031000160506       //-------------------------------------------------------------*
031100160506        Begsr insPredict;
031200160506
031300160506           clear tidp3000;
031400160506           dp3AAS   = blpAAS;
031500160506           dp3LNP   = blpLNP;
031600160506           dp3NRS   = blpNRS;
031700160506           dp3NSP   = blpNSP;
031800160506           dp3EVD   = '18';
031900160506           dp3CEV   = 'E25';
032000160506           dp3FLE   = blpLNA;
032100160518           dp3DEV   = 99991231;
032200160512           dp3HEV   = %dec(%time() : *HMS)/100;
032300160512           dp3DTSPE = blpAAS*10000+blpMGS;
032400160506           write tidp3000;
032500160506
032600160506        endsr;
032700140725
032800140725
032900140725       //-------------------------------------------------------------*
033000110627       //caricamento codici bolla con c/a                              *
033100110627       //-------------------------------------------------------------*
033200110627       Begsr SR_cartab;
033300130406
033400110627       // Leggo tabella "3A" per caricare schiera dei codici bolla con c/a
033500130829       // e schiera codici bolla franco
033600140725       i=0;
033700130829       y=0;
033800110628       clear cbc;
033900130829       clear cbf;
034000140806       setll (1:'3A') tabel00f;
034100140806       reade (1:'3A') tabel00f;
034200110627
034300140806       dow    not %eof(tabel00f);
034400140806          ds3a=tbluni;
034500110627          if §3afca>0;
034600110627             i=i+1;
034700110627             cbc(i)=tblkey;
034800140806          endif;
034900130829          if %subst(§3atb1:1:1)='F';
035000130829             y=y+1;
035100130829             cbf(y)=tblkey;
035200130829          endif;
035300140806          reade (1:'3A') tabel00f;
035400140806       enddo;
035500141128
035600141128
035700141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
035800141128       clear sk7R_PINcode;
035900141128       clear ix7R;
036000141128       setll (1:'7R') tabel00f;
036100141128       reade (1:'7R') tabel00f;
036200141128
036300141128       dow    not %eof(tabel00f);
036400141128          ds7r=tbluni;
036500141128          if §7RPINCODE = 'S';
036600141128             ix7R = ix7R + 1;
036700141128             sk7R_PINcode(ix7r)=tblkey;
036800141128          endif;
036900141128          reade (1:'7R') tabel00f;
037000141128       enddo;
037100150929
037200150929
037300150929       // Carico la lista di filiali "estere" (secondo il network)
037400150929       clear skFilExt;
037500150929       clear ixFilExt;
037600150929       setll (*zeros) azorg01l;
037700150929       read azorg01l;
037800150929
037900150929       dow not %eof(azorg01l);
038000150929           if ORGFVA = *blanks;
038100150929              og143 = orgDE3;
038200150929              chain ('NTW':§OGNTW) tntbe01l;
038300150929              if %found(tntbe01l);
038400150929                 dntw = tbeuni;
038500150929                 if §NTWFIE = 'E';
038600150929                    ixFilExt = ixFilExt + 1;
038700150929                    skFilExt(ixFilExt) = orgFIL;
038800160506                    if §OGNTW = 'DPD';
038900160506                       skFilExtDPD(ixFilExt) = orgFIL;
039000160506                    endif;
039100150929                 endif;
039200150929              endif;
039300150929           endif;
039400150929           read azorg01l;
039500150929       enddo;
039600130406
039700130406
039800140806       // Reperisco i parametri di procedura da PDFTABEL
039900140806
040000130406       pTBKEY = 'BRT';
040100130406       pTBELE = 'PATHORIGINE';
040200130416       pPGM   = 'FNLSEMR';
040300130406       pTBDAT = *blanks;
040400130406       callP(e) PDF100R (pTBKEY :
040500130406                         pTBELE :
040600130416                         pPGM   :
040700130406                         pTBDAT :
040800140806                         wEsito);
040900140806       if %error or wEsito <> '0';
041000140806          mEsito = '2';
041100130406       else;
041200130406          pPathOrig = %trim(pTBDAT);
041300130406       endif;
041400161110
041500161110
041600161110       pTBKEY = 'BRT';
041700161110       pTBELE = 'PATHORIGINWIN';
041800161110       pPGM   = 'FNLSEMR';
041900161110       pTBDAT = *blanks;
042000161110       callP(e) PDF100R (pTBKEY :
042100161110                         pTBELE :
042200161110                         pPGM   :
042300161110                         pTBDAT :
042400161110                         wEsito);
042500161110       if %error or wEsito <> '0';
042600161110          mEsito = '2';
042700161110       else;
042800161110          pPathOrigWin = %trim(pTBDAT);
042900161110       endif;
043000130406
043100140806
043200130406       pTBKEY = 'BRT';
043300130406       pTBELE = 'PATHZIPAS400';
043400130416       pPGM   = 'FNLSEMR';
043500130406       pTBDAT = *blanks;
043600130406       callP(e) PDF100R (pTBKEY :
043700130406                         pTBELE :
043800130416                         pPGM   :
043900130406                         pTBDAT :
044000140806                         wEsito);
044100140806       if %error or wEsito <> '0';
044200140806          mEsito = '2';
044300130406       else;
044400130406          pPathZipAS = %trim(pTBDAT);
044500130406       endif;
044600130406
044700140806
044800130406       pTBKEY = 'BRT';
044900130406       pTBELE = 'PATHZIPSERVER';
045000130416       pPGM   = 'FNLSEMR';
045100130406       pTBDAT = *blanks;
045200130406       callP(e) PDF100R (pTBKEY :
045300130406                         pTBELE :
045400130416                         pPGM   :
045500130406                         pTBDAT :
045600140806                         wEsito);
045700140806       if %error or wEsito <> '0';
045800140806          mEsito = '2';
045900130406       else;
046000130406          pPathZipSRV = %trim(pTBDAT);
046100130406       endif;
046200130503
046300140806
046400130503       pTBKEY = 'BRT';
046500130503       pTBELE = 'SHARE';
046600130503       pPGM   = 'FNLSEMR';
046700130503       pTBDAT = *blanks;
046800130503       callP(e) PDF100R (pTBKEY :
046900130503                         pTBELE :
047000130503                         pPGM   :
047100130503                         pTBDAT :
047200140806                         wEsito);
047300140806       if %error or wEsito <> '0';
047400140806          mEsito = '2';
047500130503       else;
047600130503          pShare = %trim(pTBDAT);
047700130503       endif;
047800130406
047900110627       endsr;
048000170124
048100170124
048200170124       //-------------------------------------------------------------*
048300170124       //Gestione elaborazione parallela                              *
048400170124       //-------------------------------------------------------------*
048500170124       Begsr SR_elapar;
048600170124
048700170124           *in40 = *off;                  // record KO
048800170124           if divis > *zeros AND divid > *zeros;
048900170124              wDivid = divid - 1;
049000170124              if %rem(ar5NSP:divis) = wDivid;
049100170124                 *in40 = *on;             // record OK
049200170124                 chain ar503_rrn fiar500f;
049300170124              endif;
049400170124           else;
049500170124              *in40 = *on;                // record OK
049600170124           endif;
049700170124
049800170124       endsr;
049900130406
050000130406
050100110617       //-------------------------------------------------------------*
050200110617       //Lettura fiar5 record "EMD" da inviare                         *
050300110617       //-------------------------------------------------------------*
050400110617       Begsr SR_Elabora;
050500110617        setll (kdto) fiar503l;
050600110617        reade (kdto) fiar503l;
050700130605
050800130605        // inizializzo il contatore di email con allegato
050900140806        wEmlSnt = *zeros;
051000110617
051100140806        dow not %eof(fiar503l) and wEmlSnt < 50;
051200130406
051300130502        // controllo se è stata richiesta la chiusura del sottosistema
051400110627           if %shtdn;
051500110627               chiudi='S';
051600110627               leavesr;
051700110627           endif;
051800170124
051900170124        // Se richiesta elaborazione parallela gestisco
052000170124           exsr sr_elapar;
052100170124
052200170124        // Se OK record da elaborare => procedo
052300170124           if *in40;
052400130406
052500110617        // controllo lo stato della bolla: per poter inviare bisogna
052600110617        // che la bolla risulti partita
052700110617           exsr sr_chkstblp;
052800110617           if bolok='S';
052900131008              dar5emd=ar5uni;
053000131008              exsr sr_inviamail;
053100110617           endif;
053200150930           if bolok='E';
053300150930              clear wdcrRev;
053400150930              clear trulORsds;
053500150930              clear trulOR2ds;
053600150930              dar5emd=ar5uni;
053700150930              exsr sr_UpdFIAR5;
053800150929           endif;
053900130406
054000170124           endif;
054100170124
054200130605           reade (kdto) fiar503l;
054300110617        enddo;
054400130406
054500110617        endsr;
054600130406
054700130406
054800110617       //-------------------------------------------------------------*
054900110617       //Verifica stato della bolla                                   *
055000110617       //-------------------------------------------------------------*
055100130625        Begsr sr_chkstblp;
055200130406
055300140806        // Inizializzazioni di verifica
055400110617        bolok=' ';
055500131003        bologgi=' ';
055600140806
055700140806        // Inizializzo email come NO allegati
055800140806        wTipoEml = '*STD';
055900140806        wAllega = *off;
056000130625
056100140806
056200130625        // Effettuo controlli tramite driver che detrmina "inviabilità" delle
056300130625        // comunicazioni ai destinatari
056400130625        clear fnlvemds;
056500130625        clear fnblpds;
056600130625        iemAAS = ar5aas;
056700130625        iemLNP = ar5lnp;
056800130625        iemNRS = ar5nrs;
056900130625        iemNSP = ar5nsp;
057000130625        kpjbu = fnlvemds;
057100130625        callP(e) FNLVEMR (kpjba : fnblpds);
057200130625        fnlvemds = kpjbu;
057300140725
057400130625
057500131008        // Verifico se esito favorevole al invio
057600140725 ===>   if oemESITO = '1';
057700150929
057800150929           // Verifico che NON trattasi di spedizione "export"
057900150929           if  %lookup(blpLNA:skFilExt) > 0;
058000150929               bolok = 'E';
058100150929           endif;
058200140725
058300150929           // Se tutto OK => procedo
058400150929           if bolok <> 'E';
058500170221
058600170221           // Verifico se per cliente corrente presente forzatura
058700170221           // per Rag.Soc. invio alerts
058800170221           UBRSMALDS.iTpRic   = 'E';
058900170221           UBRSMALDS.iOrigine = 'P';
059000170221           UBRSMALDS.iAAS     = blpAAS;
059100170221           UBRSMALDS.iLNP     = blpLNP;
059200170221           UBRSMALDS.iNRS     = blpNRS;
059300170221           UBRSMALDS.iNSP     = blpNSP;
059400170221           UBRSMALR_S ( UBRSMALDS );
059500170221           if UBRSMALDS.oEsito = *zeros;
059600170221              wRSM_Eml = %trim(UBRSMALDS.oRagSoc);
059700170221           else;
059800170221              wRSM_Eml = %trim(blpRSM);
059900170221           endif;
060000141013
060100141013           // Calcolo il BRTcode (tramite apposito driver)
060200141013           clear BRTcode;
060300141013           BRTcode.aas4    = blpAAS;
060400141013           BRTcode.lnp     = blpLNP;
060500141013           BRTcode.nrs     = blpNRS;
060600141013           BRTcode.nsp     = blpNSP;
060700141013
060800141013           clear trul28ds0;
060900141013           trul28ds0.i280COD = BRTcode.BRTcode;
061000141013           callP(e) TRUL28R0 (trul28ds0);
061100141013
061200141013           // Se BRTcode errato => esito KO
061300141016           clear wBRTcode;
061400141016           clear wBRTcodeLong;
061500141013           if %error OR trul28ds0.o280err = '1';
061600141013           else;
061700141016              wBRTcode     = %subst(trul28ds0.o280cod:1:5)      +
061800141016                             %subst(trul28ds0.o280cod:6:5)      +
061900141016                             %subst(trul28ds0.o280cod:11:5)     +
062000141016                             %subst(trul28ds0.o280cod:16:4);
062100170201              wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +'  '+
062200141016                             %subst(trul28ds0.o280cod:6:5) +'  '+
062300141016                             %subst(trul28ds0.o280cod:11:5)+'  '+
062400141016                             %subst(trul28ds0.o280cod:16:4);
062500141013           endif;
062600141013
062700131008
062800131008           // Verifico se per la spedizione corrente è previsto invio allegati
062900140806           clear dar5gen;
063000131008           chain (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'GEN') fiar501l;
063100131008           if  %found(fiar501l);
063200131008               dar5gen = gen_ar5UNI;
063300131008           endif;
063400131008
063500131008           // Verifico se previsto invio allegati oppure no
063600131008           // Con invio allegati aspetto chiusura fogli viaggio partenza
063700161121           // if (§AR5ALM='S' AND blpft1<>*blanks) OR §AR5ALM<>'S';
063800140806
063900140806              // Se previsto invio allegati => imposto email di tipo "allegati"
064000140806              if §AR5ALM = 'S';
064100140806                 wTipoEml = '*ATCH';
064200140806              endif;
064300140806
064400131008              // Effettuo verifica anche su data/ora invio rispetto alla bolla
064500131008              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
064600131008                 bolok='S';
064700131008              endif;
064800131008              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
064900131008                 %dec(%time():*ISO)  >= 150000;
065000131008                 bolok='S';
065100131008                 bologgi='S';
065200131008              endif;
065300131008
065400161121           // endif;
065500131008
065600131002
065700131002           if bolok='S';
065800131002              // data spedizione in formato gg/mm/aaaa
065900131002              wdataspe=blpaas*10000+blpmgs;
066000131002              dataeur=%date(wdataspe:*iso);
066100131002              wdataspe=%dec(dataeur);
066200131002              wPOspe=%editc(blpLNP:'X');
066300131002              if blpccm > *zeros;
066400131002                 wPOcli=%editc(blpCCM:'X');
066500131002              else;
066600131002                 wPOcli=%editc(blpKSC:'X');
066700131002              endif;
066800131002           endif;
066900150929
067000150929           endif;
067100150929
067200110617        endif;
067300130406
067400110617        endsr;
067500130406
067600150929
067700130406
067800110617       //-------------------------------------------------------------*
067900110617       //Richiamo driver per invio mail e aggiorno dto su fiar5       *
068000110617       //-------------------------------------------------------------*
068100110617        Begsr sr_inviamail;
068200130406
068300161110        // Inizializzo variabili di work
068400161110        clear strTRBMATCH;
068500140806
068600130406        // chaino file accessori che mi servono per il passaggio info
068700130406        // chaino contrassegno se presente
068800110621        ar9cas = *zeros;
068900110627        if %lookup(blpcbo:cbc)>0;
069000110617           chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
069100110617        endif;
069200130406
069300140806
069400130406        // chaino azorg della linea di arrivo
069500140401        clear og129;
069600141016        reset wLnaOraMMD_A;
069700141016        reset wLnaOraMMA_A;
069800141016        reset wLnaOraMPD_A;
069900141016        reset wLnaOraMPA_A;
070000141016        clear og130;
070100141016        reset wLnaOraTMD_A;
070200141016        reset wLnaOraTMA_A;
070300141016        reset wLnaOraTPD_A;
070400141016        reset wLnaOraTPA_A;
070500130406        chain (blplna) azorg01l;
070600141016
070700141016        og129 = orgDC9;
070800140401        if §OGORFMD <> *zeros AND §OGORFMD <> *blanks;
070900141016           wLnaOraMMD_A = %subst(§OGORFMD:1:2)+':'+%subst(§OGORFMD:3:2);
071000140401        endif;
071100140401        if §OGORFMA <> *zeros AND §OGORFMA <> *blanks;
071200141016           wLnaOraMMA_A = %subst(§OGORFMA:1:2)+':'+%subst(§OGORFMA:3:2);
071300140401        endif;
071400140401        if §OGORFPD <> *zeros AND §OGORFPD <> *blanks;
071500141016           wLnaOraMPD_A = %subst(§OGORFPD:1:2)+':'+%subst(§OGORFPD:3:2);
071600140401        endif;
071700140401        if §OGORFPA <> *zeros AND §OGORFPA <> *blanks;
071800141016           wLnaOraMPA_A = %subst(§OGORFPA:1:2)+':'+%subst(§OGORFPA:3:2);
071900140401        endif;
072000141016
072100141016        og130 = orgDD0;
072200141016        if §OGORAMD <> *zeros AND §OGORAMD <> *blanks;
072300141016           wLnaOraTMD_A = %subst(§OGORAMD:1:2)+':'+%subst(§OGORAMD:3:2);
072400141016        endif;
072500141016        if §OGORAMA <> *zeros AND §OGORAMA <> *blanks;
072600141016           wLnaOraTMA_A = %subst(§OGORAMA:1:2)+':'+%subst(§OGORAMA:3:2);
072700141016        endif;
072800141016        if §OGORAPD <> *zeros AND §OGORAPD <> *blanks;
072900141016           wLnaOraTPD_A = %subst(§OGORAPD:1:2)+':'+%subst(§OGORAPD:3:2);
073000141016        endif;
073100141016        if §OGORAPA <> *zeros AND §OGORAPA <> *blanks;
073200141016           wLnaOraTPA_A = %subst(§OGORAPA:1:2)+':'+%subst(§OGORAPA:3:2);
073300141016        endif;
073400130406
073500140324
073600130406        // Se la linea di arrivo è in £6 cerco la sua capofila
073700120202        clear wlna;
073800120202        clear fnlv55ds;
073900120202        d55lin = blplna;
074000120202        d55tpt='6';
074100120202        d55drf=blpaas*10000+blpmgs;
074200120202        fnlv55r(fnlv55ds);
074300120202        wlna=d55tfa;
074400130406
074500140806
074600130406        // Stacco subito un progressivo per la email corrente
074700130408        exsr sr_rtvPrg;
074800130408
074900140806
075000140806        // Se email di tipo "allegati" => effettuo iter preposto
075100140806        if wTipoEml = '*ATCH';
075200161118
075300161118           // Inizializzo gli allegati a NO
075400161118           wAllega = *off;
075500140806
075600161118           // Verifico se per la bolla corrente tutti gli allegati sono OK
075700161118           callP(e) FNLSEMAR (pPathOrig
075800161118                             :blpAAS
075900161118                             :blpLNP
076000161118                             :blpNRS
076100161118                             :blpNSP
076200161118                             :wOK_Atch);
076300161118
076400161118           // Invio email con allegati solo se:
076500161118           // - o tutti allegati esistenti e OK
076600161222           // - o oppure chisura fogli viaggio partenza effettuata AND
076700161223           //     data/ora corrente oltre le 23:00 della data bolla
076800161222           if wOK_Atch OR (blpft1<>*blanks AND
076900161223                       %char(%dec(%timestamp:*ISO)) >=
077000161223                       %char(blpAAS*10000+blpMGS)+'230000');
077100161118
077200161118              // Verifico eventuali personalizzazioni relative al PDF
077300161118              exsr sr_PDFper;
077400161118
077500161118              // Scorro tutto il FIAR8/§PDF della bolla corrente
077600161118              exsr sr_elaAR8;
077700161118
077800161118              // Verifico l'esito finale di uscita
077900161118              if wAllega;
078000161118                 wEmlSnt = wEmlSnt + 1;
078100161118              endif;
078200161118
078300161118           endif;
078400140806
078500130406        endif;
078600170123
078700130406
078800170123        // Se tutto OK procedo
078900170123        if wTipoEml <> '*ATCH' OR (wTipoEml = '*ATCH' AND wAllega);
079000130406
079100130406        // In relazione al tipo di email da inviare (standard o con allegato)
079200130406        // reperisco la tabella MRA key "TRBMBYPSR" corrispondente
079300130406        // e imposto le relative "particolarità"
079400170116        clear indmrabyps;
079500170116
079600170116        // Se spedizione con appuntamento
079700170116        wOBJM = *blanks;
079800170116        if blpTC1='A' or blpTC2='A';
079900170116           wOBJM = '*OBJM*BRT  -';
080000170116        else;
080100170116           wOBJM = '*OBJM*BRT -';
080200170116        endif;
080300170116
080400130406        select;
080500141014          when chiudi = 'T';
080600141014             chain ('MRA':'TRBMBYPSR':'TEST') tntbe01l  ;
080700141014             if blpFFD <> 'S';
080800170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ' +
080900141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
081000141014             else;
081100170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione '+
081200141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
081300141014             endif;
081400141014             exsr sr_invEml;
081500161116          when wTipoEml = '*ATCH';
081600130406             chain ('MRA':'TRBMBYPSR':'*ATCH') tntbe01l  ;
081700141014             if blpFFD <> 'S';
081800170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ';
081900141014             else;
082000170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione ';
082100141014             endif;
082200170915             w§CM1VAR = %trim(w§CM1VAR) + '  ' +
082300161116                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
082400130723             exsr sr_invEml;
082500140806          other;
082600130408             chain ('MRA':'TRBMBYPSR':*blanks) tntbe01l  ;
082700141014             if blpFFD <> 'S';
082800170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ' +
082900130406                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
083000141014             else;
083100170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione '+
083200141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
083300141014             endif;
083400130723             exsr sr_invEml;
083500130406        endsl;
083600170123
083700170123        endif;
083800130406
083900110617        endsr;
084000130406
084100140806
084200130406
084300130406       //-------------------------------------------------------------*
084400130406       //elaborazione PDF da allegare alla email (FIAR804L)           *
084500161110       //-------------------------------------------------------------*
084600161110        Begsr sr_elaAR8;
084700130406
084800140806           // Ad ogni passaggio testo se tutto ok
084900140806           if mEsito <> '2';
085000140806
085100140806             // Quindi procedo con l'elaborazione degli allegati
085200140806             setll (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
085300140806             reade (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
085400130406
085500140806             // leggo TUTTO il fiar804l
085600140806             dow not %eof(fiar804l);
085700140806
085800140806               // Ad ogni check di PDF da allegare => inizializzo flag errore
085900140806               mEsito = '0';
086000140806
086100140806               // Ridefinisco i dati relativi al PDF
086200140806               dar8§pdf = ar8UNI;
086300140806               pPathCli = §AR8PPDFP;
086400140806               pNomePDF = §AR8PPDFN;
086500140806
086600140806               // Se serve verifico presenza e univocità del file da trattare
086700140806               if %scan('*':pNomePDF) > *zeros;
086800140808                  exsr sr_OpzRtvPDF;
086900140806               else;
087000140806                  pFileName = pNomePDF;
087100140806               endif;
087200140806
087300140806               // Se tutto OK procedo con l'elaborazione allegati/invio email
087400140806               if mEsito <> '0';
087500140806                  §AR8PEML = 'E';      // PDF non presente (o non univoco)
087600140806               else;
087700140806
087800170915                  strTRBMATCH=strTRBMATCH+'§§Atch§§=='+%trim(pPathOrigWin)+
087900161115                        '\'+%trim(pPathCli)+'\'+%trim(pFileName)+'||';
088000161115
088100140806                  // Se OK => segno almeno un allegato da inviare
088200140806                  if mEsito = '0';
088300140806                     wAllega = *on;
088400140806                     §AR8PEML = 'S';  // PDF ok
088500140806                  else;
088600140806                     §AR8PEML = 'E';  // PDF non presente o errore in attach PDF
088700140806                  endif;
088800140806               endif;
088900140806
089000140806               // Aggiorno lo stato del FLAG INVIO EMAIL PDF
089100140806               ar8UNI = dar8§pdf;
089200141014               if chiudi = *blanks;
089300141014                  update fiar8000;
089400141014               endif;
089500130406
089600140806               // quindi prosegui con la lettura
089700140806               reade (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
089800130406
089900140806             enddo;
090000140806
090100140806           endif;
090200130406
090300130406        endsr;
090400130723
090500140806
090600130723
090700130723       //-------------------------------------------------------------*
090800130723       //Invio effettivo della email                                  *
090900130723       //-------------------------------------------------------------*
091000130723        Begsr sr_invEml;
091100130723
091200130723          if %found(tntbe01l);
091300170203
091400170203             // Verifoco se per linea arrivo corrente previsto nuovo deploy
091500170203             exsr sr_ChkDeploy;
091600170203
091700170203             // Impostazioni ambiente e variabili specifiche
091800130723             indmrabyps=tbeuni;
091900130723             exsr sr_trtcm1ds;
092000161115             exsr sr_cliper;
092100170201
092200170201             // Compongo il testo del SMS e gestisco modo vecchio/nuovo
092300170201             if not *in60;
092400170201                exsr sr_impVarBm;
092500170201             else;
092600170201                exsr sr_impVarBmNew;
092700170201             endif;
092800170201
092900161110             exsr sr_impAtchBm;
093000130723             pesito=0;
093100130723             if Bartmail_insert(strTRBM:indmrabyps:
093200130723                    intrtcm1ds:0:pesito)=0 and pesito=0;
093300150930                exsr sr_UpdFIAR5;
093400131210
093500131210                // Verifico se condizione di anomalia
093600131210                if §AR5ALM='S' and §AR8PEML = *blanks;
093700131210                   exsr snd_Alert;
093800140414                else;
093900140414                   // Se tutto ok => inserisco R.A.
094000140414                   exsr sr_InsRA;
094100131210                endif;
094200131210
094300130723             endif;
094400130723          endif;
094500130723
094600130723        endsr;
094700130406
094800140806
094900130406
095000130406       //-------------------------------------------------------------*
095100140806       // Gestione tentativi esecuzioni comandi                       *
095200130406       //-------------------------------------------------------------*
095300140806        Begsr sr_ExeOpz;
095400140806
095500140808          wCnt = 0;
095600140808          wEsito = '0';
095700140808          dou wCnt >= pTry or wEsito = '0';
095800140808              select;
095900140808                when pTyp = '1';
096000140808                     wOpz = pOpz;
096100140808                     exsr sr_RunOpz;
096200140808                when pTyp = '2';
096300140808                     exsr sr_findPDF;
096400140808              endsl;
096500140808
096600140808              // Se problema
096700140806              if %error or wEsito <> '0';
096800140806                 wEsito = 'E';
096900140806
097000140808                 // Eseguo attesa
097100140808                 wOpz = 'S';
097200140808                 exsr sr_RunOpz;
097300140806
097400140806                 // Incremento tentativi
097500140808                 wCnt = wCnt + 1;
097600140806
097700140806              endif;
097800140806
097900140806          enddo;
098000140806
098100140806          if wEsito <> '0';
098200140806             mEsito = '2';
098300140806          endif;
098400130406
098500130406        endsr;
098600150930
098700150930
098800150930
098900150930       //-------------------------------------------------------------*
099000150930       // Gestione tentativi esecuzioni comandi                       *
099100150930       //-------------------------------------------------------------*
099200150930        Begsr sr_UpdFIAR5;
099300150930
099400150930          eval §ar5dto=%char(%dec(%timestamp:*ISO));
099500150930          §ar5DPC  = %editc(wdcrRev:'X');
099600150930          §ar5OPCD = %editc(OOR2STIS:'X');
099700150930          §ar5OPCA = %editc(OOR2STFS:'X');
099800150930          // Verifico che i valori sopra attribuiti siano diversi da zero
099900150930          if §AR5DPC  = *all'0';
100000150930             §AR5DPC  = *blanks;
100100160330             §AR5OPCD = *zeros;
100200160330             §AR5OPCA = *zeros;
100300150930          endif;
100400150930          if §AR5OPCD = *all'0';
100500150930             §AR5OPCD = *blanks;
100600150930          endif;
100700150930          if §AR5OPCA = *all'0';
100800150930             §AR5OPCA = *blanks;
100900150930          endif;
101000150930          ar5uni=dar5emd;
101100150930          if chiudi = *blanks;
101200150930             update fiar5000;
101300150930          endif;
101400150930
101500150930        endsr;
101600140808
101700140808
101800140808
101900140808       //-------------------------------------------------------------*
102000140808       // Richiamo driver "trova PDF"                                 *
102100140808       //-------------------------------------------------------------*
102200140808        Begsr sr_findPDF;
102300140808
102400170125            pFileName  = *blanks;
102500170125            pCheckOnly = *blanks;
102600140808            callP(e) FNLSPDFR1 (pPathOrig  :
102700140808                                pPathCli   :
102800140808                                pNomePDF   :
102900140808                                pCheckOnly :
103000140808                                pFileName  :
103100140808                                wEsito);
103200140808
103300140808        endsr;
103400130406
103500130502
103600130502
103700130502       //-------------------------------------------------------------*
103800140808       // Eseguo Opzione richiesta                                    *
103900130502       //-------------------------------------------------------------*
104000140808        Begsr sr_RunOpz;
104100130502
104200140808          pProgress = w§CM1PROG;
104300140808          callP(e) FNLSPDFC2 (wOpz       :
104400130502                              pPathOrig  :
104500130502                              pPathCli   :
104600130502                              pFileName  :
104700130502                              pPathZipAS :
104800130503                              pShare     :
104900130502                              pProgress  :
105000140806                              wEsito);
105100130502
105200130502        endsr;
105300140808
105400140808
105500140808
105600140808       //-------------------------------------------------------------*
105700140808       // Opzione - Reperisci PDF                                     *
105800140808       //-------------------------------------------------------------*
105900140808        Begsr sr_OpzRtvPDF;
106000140808
106100140808           pTyp = '2';
106200140808           pTry = 120;
106300140808           exsr sr_exeOpz;
106400140808
106500140808        endsr;
106600140808
106700140808
106800140808
106900140808       //-------------------------------------------------------------*
107000140808       // Opzione "H" - Check Network                                 *
107100140808       //-------------------------------------------------------------*
107200140808        Begsr sr_OpzChkNet;
107300140808
107400140808           pTyp = '1';
107500140808           pTry = 120;
107600140808           pOpz = 'H';
107700140808           exsr sr_exeOpz;
107800140808
107900140808        endsr;
108000130406
108100130406
108200130406
108300110617       //-------------------------------------------------------------*
108400110617       //Valorizzazione trtcm1ds                                      *
108500110617       //-------------------------------------------------------------*
108600110617        Begsr sr_trtcm1ds;
108700130406
108800110617           clear intrtcm1ds;
108900130408           if %scan('*PO':§mraemlmit) > *zeros;
109000130827              string=%replace(wPOspe:§mraemlmit:
109100130408                              %scan('*PO':§mraemlmit:1):3);
109200130827           else;
109300130827              if %scan('*PC':§mraemlmit) > *zeros;
109400130827                 string=%replace(wPOcli:§mraemlmit:
109500130827                                 %scan('*PC':§mraemlmit:1):3);
109600130827              else;
109700130827                 string=§mraemlmit;
109800130827              endif;
109900130827           endif;
110000110617           exsr sr_chkstr;
110100110617           §CM1MITT=string;
110200110617           string=§ar5eml;
110300110617           exsr sr_chkstr;
110400141014           select;
110500141014              when chiudi = 'T';
110600171006                   §CM1DST='email_di_test@brt.it';
110700141014              when chiudi = *blanks;
110800141014                   §CM1DST=string;
110900141014           endsl;
111000170202           if not *in60;
111100170202              §CM1TIPS=§mrareg;
111200170202           else;
111300170202              §CM1TIPS=§mraregnew;
111400170202           endif;
111500110617           if §mrapo<> '*PO';
111600130406              §CM1PO=§mrapo;
111700110617           else;
111800130406              §CM1PO=%editc(blplnp:'X');
111900110617           endif;
112000130406           §CM1VAR=w§CM1VAR;
112100130406           §CM1STS='0';
112200130406           §CM1IDP=§mraidpro;
112300130406           §CM1PROG=w§CM1PROG;
112400130406           §CM1ATT='01';
112500110617           §CM1TOTATT='01';
112600130406
112700110617        endsr;
112800130406
112900140806
113000130406
113100110617       //-------------------------------------------------------------*
113200110617       //controllo stringa                                            *
113300110617       //-------------------------------------------------------------*
113400110617        Begsr sr_chkstr;
113500110617           string=%xlate('||':'  ':string);
113600110617           string=%xlate('==':'  ':string);
113700110617        endsr;
113800130406
113900140806
114000130406
114100130406       //-------------------------------------------------------------*
114200130406       //reperimento progressivo                                      *
114300130406       //-------------------------------------------------------------*
114400130406        Begsr sr_rtvPrg;
114500130406
114600130406           clear trul33ds;
114700130406           w§CM1PROG=*all'0';
114800130406           i33Cnu = 24;
114900130406           i33num = 1;
115000130406           kpjbu = trul33ds;
115100130406           trul33r(kpjba);
115200130406           trul33ds = kpjbu;
115300130406           w§CM1PROG=%subst(%editc(o33nri:'X'):9:7);
115400130406
115500130406        endsr;
115600130406
115700140806
115800130406
115900110617       //-------------------------------------------------------------*
116000110617       //Imposto campo contenente la stringa per driver TRBMBYPSR     *
116100170201       //-------------------------------------------------------------*
116200170201        Begsr sr_impvarbm;
116300130406
116400140423           clear wConsegna;
116500140423
116600110617           strTRBM='§§Tag01§§=='+%trim(blprsd)+'||';
116700140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)+'||';
116800110617           strTRBM=strTRBM+'§§Tag03§§=='+%trim(blpinm)+'||';
116900110617           strTRBM=strTRBM+'§§Tag04§§=='+%trim(blpcam)+'||';
117000110617           strTRBM=strTRBM+'§§Tag05§§=='+%trim(blplom)+'||';
117100110617           strTRBM=strTRBM+'§§Tag06§§=='+%trim(blpprm)+'||';
117200140704           strTRBM=strTRBM+'§§Tag07§§=='+%editw(wdataspe:'0  /  /    ')+'||';
117300161003        // strTRBM=strTRBM+'§§Tag08§§=='+'http://vas.brt.it/vas/sped';
117400141013        // strTRBM=strTRBM+'_det_show.hsm?referer=sped_numspe_par.htm&Nspediz=';
117500141013        // strTRBM=strTRBM+%editc(blplnp:'X');
117600141013        // strTRBM=strTRBM+%editc(blpnrs:'X');
117700141013        // strTRBM=strTRBM+%editc(blpnsp:'X');
117800141013        // strTRBM=strTRBM+'&RicercaNumeroSpedizione=Ricerca';
117900141013        // strTRBM=strTRBM+'&AnnoSpedizione='+%editc(blpaas:'X');
118000141013        // strTRBM=strTRBM+'&urltype=a'+'||';
118100141013           strTRBM=strTRBM+'§§Tag08§§=='+
118200161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
118300141013           strTRBM=strTRBM+wBRTcode;
118400141013           strTRBM=strTRBM+'&urltype=a'+'||';
118500151111
118600151111           // Se spedizione con appuntamento
118700151111           if blpTC1='A' or blpTC2='A';
118800151112             strTRBM=strTRBM+'§§TagT01§§=='+'destinata che prevede la ' +
118900151112                     'CONSEGNA SU APPUNTAMENTO.<BR><BR>' +
119000151112                     'Sarà contattato da un nostro operatore per concordare ' +
119100151112                     'l''orario di consegna a Lei più gradito.'+'||';
119200151112             strTRBM=strTRBM+'§§TagT02§§=='+'precise,'+'||';
119300151111           else;
119400151112             strTRBM=strTRBM+'§§TagT01§§=='+'destinata.'+'||';
119500151112             strTRBM=strTRBM+'§§TagT02§§=='+
119600151112                     'precise sul orario di consegna,'+'||';
119700151111           endif;
119800151111
119900151111           // Se spedizione in fermo deposito
120000110617           if blpffd='S';
120100140423              wConsegna = 'La spedizione dovrà essere' +
120200140324              ' ritirata, esibendo documento di identità, presso la filiale' +
120300140324              ' di '+%trim(orgDES)+' '+
120400140324                     %trim(orgIND)+' '+
120500140324                     %trim(%editc(orgCPF:'X'))+' '+
120600140324                     %trim(orgLOC)+' '+
120700140324                 '('+%trim(orgPRO)+')'+
120800140327              ' nei seguenti orari  ' +
120900141016                %trim(wLnaOraMMD_A)+' - '+
121000141016                %trim(wLnaOraMMA_A);
121100141016              if wLnaOraMPD_A <> *blanks;
121200141016                 wConsegna=wConsegna+'  e  ' + %trim(wLnaOraMPD_A)+
121300141016                                       ' - ' + %trim(wLnaOraMPA_A);
121400140324              endif;
121500151112              strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
121600140324
121700140324              strTRBM=strTRBM+'§§Tag28§§=='+'Servizio Clienti   ' +
121800140324                              %trim(orgDD2)+'@brt.it' + '  -  '+
121900140324                              %trim(orgTEL)+'||';
122000140324
122100140327             strTRBM=strTRBM+'§§Tag29§§=='+'<BR><BR>Nel caso voglia delegare' +
122200140324               ' il ritiro il delegato dovrà esibire: il proprio documento' +
122300140324               ' di identità, il documento di identità del delegante (anche' +
122400140324               ' in fotocopia) e il modulo di delega presente nella sezione' +
122500140324               ' <a href="http://www.brt.it/it/area_download.do">download</a>'+
122600140327               ' del sito BRT.'+'||';
122700140324
122800140327             strTRBM=strTRBM+'§§Tag23§§=='+'<BR><BR>Sarà nostra cura inviarle' +
122900140327              ' un ulteriore e-mail non appena la merce sarà disponibile' +
123000140327              ' per il ritiro.'+'||';
123100110617           else;
123200120619              if wcliper='S'and wdcr >0 ;
123300140423               select;
123400120620               when blptcr=*blanks;
123500141016                 wConsegna = '<b>La consegna ' +
123600140704                          'é prevista il' ;
123700120620               when blptcr='D';
123800141016                 wConsegna = '<b>La consegna ' +
123900140704                          'é prevista dopo il' ;
124000120620               when blptcr='P';
124100141016                 wConsegna = '<b>La consegna ' +
124200140704                          'é prevista prima del' ;
124300120620               endsl;
124400140423
124500141016                 wConsegna=wConsegna+' '+%trim(%editw(wdcr:'0  /  /    '));
124600141016                 wConsegna=wConsegna+'</b> '+%trim(wSpedConsOrari);
124700151112                 strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
124800140423
124900120619                 strTRBM=strTRBM+'§§Tag23§§=='+''+'||';
125000140324                 strTRBM=strTRBM+'§§Tag28§§=='+''+'||';
125100140324                 strTRBM=strTRBM+'§§Tag29§§=='+''+'||';
125200120619              else;
125300140423                 wConsegna = '';
125400151112                 strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
125500120619                 strTRBM=strTRBM+'§§Tag23§§=='+''+'||';
125600140324                 strTRBM=strTRBM+'§§Tag28§§=='+''+'||';
125700140324                 strTRBM=strTRBM+'§§Tag29§§=='+''+'||';
125800120619              endif;
125900110617           endif;
126000110617           strTRBM=strTRBM+'§§Tag10§§=='+%editc(blplnp:'X')+'  ';
126100110617           strTRBM=strTRBM+%editc(blpnrs:'X')+'  ';
126200110617           strTRBM=strTRBM+%editc(blpnsp:'X')+'||';
126300110617           strTRBM=strTRBM+'§§Tag11§§=='+%trim(%editc(blprmn:'Z'))+'||';
126400110617           strTRBM=strTRBM+'§§Tag12§§=='+%trim(blprma)+'||';
126500110617           strTRBM=strTRBM+'§§Tag13§§=='+%trim(%editc(blpncl:'Z'))+'||';
126600110617           strTRBM=strTRBM+'§§Tag14§§=='+%trim(%editc(blppkb:'2'))+'||';
126700110617           if ar9cas>0;
126800110621              war9cas = ar9cas;
126900110621              strTRBM=strTRBM+'§§Tag15§§=='+%trim(%editc(war9cas:'2'))+
127000110617                                           '  '+ar9vca+'||';
127100110617           else;
127200141016              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
127300110617           endif;
127400110617           strTRBM=strTRBM+'§§Tag16§§=='+%trim(orgdes)+'||';
127500110617           strTRBM=strTRBM+'§§Tag17§§=='+%trim(orgind)+'||';
127600110617           strTRBM=strTRBM+'§§Tag18§§=='+%editc(orgcpf:'X')+'||';
127700110617           strTRBM=strTRBM+'§§Tag19§§=='+%trim(orgloc)+'||';
127800140324           strTRBM=strTRBM+'§§Tag20§§=='+%trim(orgpro)+'||';
127900110617           strTRBM=strTRBM+'§§Tag21§§=='+%trim(orgtel)+'||';
128000140327           strTRBM=strTRBM+'§§Tag22§§=='+%trim(orgDD2)+'@brt.it'+'||';
128100120618           strTRBM=strTRBM+'§§Tag24§§=='+%trim(blpind)+'||';
128200120622           strTRBM=strTRBM+'§§Tag25§§=='+%trim(blplod)+'  '+%trim(blpprd)+'||';
128300161115           if wAllega;
128400151112              strTRBM=strTRBM+'§§Tag26§§=='+'<BR>'+'In allegato può '+
128500151112                      'trovare i documenti predisposti dal mittente.'+'||';
128600140805           else;
128700140805              strTRBM=strTRBM+'§§Tag26§§=='+''+'||';
128800130408           endif;
128900131003           if bologgi='S';
129000131003              strTRBM=strTRBM+'§§Tag27§§=='+'Da domani potrà '+'||';
129100131003           else;
129200131003              strTRBM=strTRBM+'§§Tag27§§=='+'Potrà '+'||';
129300131003           endif;
129400141016           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
129500141128
129600141128           if wPINcode='S';
129700151112              strTRBM=strTRBM+'§§Tag42§§=='+'<BR>'+
129800141128               'ATTENZIONE: Come richiesto dal mittente la consegna potrà '   +
129900141128               'avvenire solo a seguito di inserimento del codice PIN. La '   +
130000141128               'invitiamo pertanto a predisporsi in quanto il ns. Incaricato '+
130100141128               'lo esigerà per poter effettuare la consegna.'
130200141128               +'||';
130300141128           else;
130400141128              strTRBM=strTRBM+'§§Tag42§§=='+''+'||';
130500141128           endif;
130600131003
130700110617        endsr;
130800170201
130900170201
131000170201
131100170201       //---------------------------------------------------------------*
131200170202       //Imposto campo contenente la stringa per driver TRBMBYPSR (NEW) *
131300170202       //---------------------------------------------------------------*
131400170201        Begsr sr_impvarbmNew;
131500170202
131600170202
131700170202          // Inizializzazione variabili di wrk
131800170206          clear wConsegnaN;
131900170206
132000170202          clear wRiga01;
132100170202          clear wRiga02;
132200170202          clear wRiga03;
132300170202          clear wRiga04;
132400170202          clear wRiga05;
132500170202          clear wRiga06;
132600170202          clear wRiga07;
132700170202          clear wRiga08;
132800170202
132900170711          clear wTagRSM;
133000170202          clear wTagV01;
133100170202          clear wTagV02_1;
133200170202          clear wTagV02_2;
133300170202          clear wTagV02_3;
133400170202          clear wTagV02_4;
133500170202          clear wTagV03_1;
133600170202          clear wTagV03_2;
133700170202          clear wTagV04;
133800170202          clear wTagV05;
133900170202          clear wTagV06;
134000170202          clear wTagV07_1;
134100170202          clear wTagV07_2;
134200170202          clear wTagV07_3;
134300170202          clear wTagV07_4;
134400170202          clear wTagV07_5;
134500170202
134600170201
134700170202          // Innanzitutto determino il tipo di alert
134800170202          clear wTypAlert;
134900170202          select;
135000170202            when blpTC1='A' or blpTC2='A';
135100170202               wTypAlert = 'A';            // APPUNTAMENTO
135200170202            when blpffd='S';
135300170202               wTypAlert = 'F';            // FERMO DEPOSITO
135400170202            when wcliper='S'and wdcr >0 ;
135500170202               wTypAlert = 'S';            // STANDARD
135600170202            other;
135700170202               wTypAlert = 'O';            // ALTRO
135800170202          endsl;
135900170202
136000170202
136100170202          // In base alla tipologia di alert personalizzo le righe di testo
136200170202          clear wConsegna;
136300170202          select;
136400170202            when blpTCR = *blanks;
136500170202              wConsegna = 'il';
136600170202            when blpTCR = 'D';
136700170202              wConsegna = 'dopo il';
136800170202            when blpTCR = 'P';
136900170202              wConsegna = 'prima del';
137000170202          endsl;
137100170202
137200170202
137300170202          select;
137400170202            // APPUNTAMENTO
137500170202            when wTypAlert = 'A';
137600170202
137700170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
137800170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' in data ' +
137900170711                %editw(wdataspe:'0  /  /    ') + ' ci ha affidato una tua ' +
138000170711                'spedizione che prevede la CONSEGNA SU APPUNTAMENTO.';
138100170711              wRiga06 = 'Programma subito la consegna cliccando ' +
138200170202                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
138300170711                'brtCode='+wBRTcode+'&urltype=a">qui</a></b>' +
138400170711                ', in caso contrario verrai contattato ' +
138500170203                'per concordare la modalità più gradita.';
138600170206
138700170206              wConsegnaN = %trim(wRiga03);
138800170206
138900170202
139000170202            // FERMO DEPOSITO
139100170202            when wTypAlert = 'F';
139200170202
139300170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
139400170202              wRiga03 = %trim(wRSM_Eml) + ' ci ha affidato in data ' +
139500170202                %editw(wdataspe:'0  /  /    ') + ' una tua spedizione ' +
139600170202                'che dovrà essere ritirata, esibendo documento di identità, ' +
139700170202                'presso la filiale di ' +
139800170202                       %trim(orgDES)+' '+
139900170202                       %trim(orgIND)+' '+
140000170202                       %trim(%editc(orgCPF:'X'))+' '+
140100170202                       %trim(orgLOC)+' '+
140200170202                   '('+%trim(orgPRO)+')'+
140300170202                   ' nei seguenti orari  ' +
140400170202                   %trim(wLnaOraMMD_A)+' - '+
140500170202                   %trim(wLnaOraMMA_A);
140600170202                if wLnaOraMPD_A <> *blanks;
140700170202                   wRiga03 = %trim(wRiga03)+'  e  ' + %trim(wLnaOraMPD_A)+
140800170202                                              ' - ' + %trim(wLnaOraMPA_A);
140900170202                endif;
141000170202              wRiga04 = 'Servizio Clienti   ' +
141100170202                %trim(orgDD2)+'@brt.it' + '  -  ' + %trim(orgTEL);
141200170202              wRiga05 = '<b>Sarà nostra cura inviarti un ulteriore email ' +
141300170202                'non appena la merce sarà disponibile per il ritiro.</b>' +
141400170202                '<BR><BR>';
141500170202              wRiga06 = 'Se vuoi delegare il ritiro, il delegato dovrà ' +
141600170202                'esibire: il proprio documento di identità, il documento ' +
141700170202                'di identità del delegante (anche in fotocopia) e il modulo ' +
141800170203                'di delega presente nella sezione ' +
141900170202                '<a href="http://www.brt.it/it/area_download.do">download</a>'+
142000170203                ' del sito BRT.<BR><BR><BR>' +
142100170202                '<a href="http://vas.brt.it/vas/sped_det_show.htm?' +
142200170203                'brtCode='+wBRTcode+'&urltype=a">La tua spedizione</a>';
142300170206
142400170206              wConsegnaN = %trim(wRiga03) + ' ' + %trim(wRiga05);
142500170206
142600170202
142700170202            // STANDARD
142800170202            when wTypAlert = 'S';
142900170202
143000170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
143100170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' ci ha affidato una tua ' +
143200170206                'spedizione che <b>prevediamo di consegnare ' + wConsegna +
143300170202                ' ' + %trim(%editw(wdcr:'0  /  /    ')) + '</b>' +
143400170202                '<BR><BR>';
143500170711            //  wRiga04 = 'Generalmente nella tua zona i servizi vengono ' +
143600170711            //    'effettuati <u>' + %trim(wSpedConsOrari) + '</u>';
143700170711            //  wRiga05 = '<b>Informazioni più precise sull''orario di ' +
143800170711            //    'consegna saranno pubblicate il giorno della prevista ' +
143900170711            //    'consegna.</b>' +
144000170711            //    '<BR><BR><BR>';
144100170711            //  wRiga06 =
144200170711            //    'Se vuoi riprogrammare la tua consegna clicca ' +
144300170711            //    '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
144400170711            //    'brtCode='+wBRTcode+'&urltype=a">qui</a></b>.';
144500170711              wRiga04 = 'Generalmente nella tua zona i servizi vengono ' +
144600170711                'effettuati ' + %trim(wSpedConsOrari) + ', informazioni ' +
144700170711                'più precise saranno pubblicate il giorno della prevista ' +
144800170711                'consegna.';
144900170711              wRiga06 =
145000170711                'Per visualizzare o riprogrammare la tua consegna clicca ' +
145100170711                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
145200170711                'brtCode='+wBRTcode+'&urltype=a">qui</a></b>.';
145300170215
145400170206
145500170206              wConsegnaN = %trim(wRiga03) + ' ' + %trim(wRiga04);
145600170203
145700170202
145800170202            // ALTRO
145900170202            when wTypAlert = 'O';
146000170202
146100170202              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR><BR>';
146200170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' in data ' +
146300170202                %editw(wdataspe:'0  /  /    ') + ' ci ha affidato una tua ' +
146400170203                'spedizione.';
146500170203              wRiga06 = '<b>Per informazioni su precise verifica la ' +
146600170202                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
146700170203                'brtCode='+wBRTcode+'&urltype=a">tua consegna</a></b>.';
146800170206
146900170206              wConsegnaN = %trim(wRiga03);
147000170206
147100170202
147200170202          endsl;
147300170202
147400170202
147500170202
147600170202          // Gestione particolarità specifiche
147700170202          if wPINcode='S';
147800170203             wRiga07 = '<BR><b><u>' +
147900170202              'ATTENZIONE: Come richiesto dal mittente la consegna potrà '   +
148000170202              'avvenire solo a seguito di inserimento del codice PIN. Ti '   +
148100170202              'invitiamo pertanto a predisporti in quanto il ns. Incaricato '+
148200170203              'lo esigerà per poter effettuare la consegna.</u></b><BR><BR>';
148300170206
148400170206              wConsegnaN = %trim(wConsegnaN) + ' ' + %trim(wRiga07);
148500170202          endif;
148600170203
148700170203          if wAllega;
148800170203             wRiga08 = '<BR><b><u>In allegato puoi trovare i documenti ' +
148900170203               'predisposti dal mittente.</u></b><BR><BR></b>';
149000170203          endif;
149100170202
149200170206
149300170206
149400170206          // Valorizzo indicazioni per Inserimento RA
149500170206          wConsegna = %trim(wConsegnaN);
149600170206
149700170202
149800170202
149900170202          // Valorizzo le variabili "standard"
150000170711          wTagRSM   = %trim(wRSM_Eml);
150100170202          wTagV01   = %trim(wBRTcodeLong);
150200170202          wTagV02_1 = %trim(blpRSD);
150300170202          wTagV02_2 = %trim(blpIND);
150400170202          wTagV02_3 = %trim(blpLOD);
150500170202          wTagV02_4 = %trim(blpPRD);
150600170202          wTagV03_1 = %trim(%editc(blpRMN:'Z'));
150700170202          wTagV03_2 = %trim(blpRMA);
150800170202          wTagV04   = %trim(%editc(blpNCL:'Z'));
150900170202          wTagV05   = %trim(%editc(blpPKB:'2'));
151000170202          if ar9cas > *zeros;
151100170202             war9cas = ar9cas;
151200170202             wTagV06 = %trim(%editc(war9cas:'2')) + '  ' + ar9vca;
151300170202          else;
151400170202             wTagV06 = '0,00';
151500170202          endif;
151600170202          wTagV07_1 = %trim(orgDES);
151700170202          wTagV07_2 = %trim(orgIND);
151800170202          wTagV07_3 = %trim(%editc(orgCPF:'X'));
151900170202          wTagV07_4 = %trim(orgLOC);
152000170202          wTagV07_5 = %trim(orgPRO);
152100170202
152200170202
152300170202
152400170202          // Valorizzozione stringa per driver di "by-pass spool"
152500170202          strTRBM=        '§§TagR01§§=='+%trim(wRiga01)+'||';
152600170202          strTRBM=strTRBM+'§§TagR02§§=='+%trim(wRiga02)+'||';
152700170202          strTRBM=strTRBM+'§§TagR03§§=='+%trim(wRiga03)+'||';
152800170202          strTRBM=strTRBM+'§§TagR04§§=='+%trim(wRiga04)+'||';
152900170202          strTRBM=strTRBM+'§§TagR05§§=='+%trim(wRiga05)+'||';
153000170202          strTRBM=strTRBM+'§§TagR06§§=='+%trim(wRiga06)+'||';
153100170202          strTRBM=strTRBM+'§§TagR07§§=='+%trim(wRiga07)+'||';
153200170202          strTRBM=strTRBM+'§§TagR08§§=='+%trim(wRiga08)+'||';
153300170202
153400170711          strTRBM=strTRBM+'§§TagRSM§§=='+%trim(wTagRSM)+'||';
153500170202          strTRBM=strTRBM+'§§TagV01§§=='  +%trim(wTagV01)  +'||';
153600170202          strTRBM=strTRBM+'§§TagV02_1§§=='+%trim(wTagV02_1)+'||';
153700170202          strTRBM=strTRBM+'§§TagV02_2§§=='+%trim(wTagV02_2)+'||';
153800170202          strTRBM=strTRBM+'§§TagV02_3§§=='+%trim(wTagV02_3)+'||';
153900170202          strTRBM=strTRBM+'§§TagV02_4§§=='+%trim(wTagV02_4)+'||';
154000170202          strTRBM=strTRBM+'§§TagV03_1§§=='+%trim(wTagV03_1)+'||';
154100170202          strTRBM=strTRBM+'§§TagV03_2§§=='+%trim(wTagV03_2)+'||';
154200170202          strTRBM=strTRBM+'§§TagV04§§=='  +%trim(wTagV04)  +'||';
154300170202          strTRBM=strTRBM+'§§TagV05§§=='  +%trim(wTagV05)  +'||';
154400170202          strTRBM=strTRBM+'§§TagV06§§=='  +%trim(wTagV06)  +'||';
154500170202          strTRBM=strTRBM+'§§TagV07_1§§=='+%trim(wTagV07_1)+'||';
154600170202          strTRBM=strTRBM+'§§TagV07_2§§=='+%trim(wTagV07_2)+'||';
154700170202          strTRBM=strTRBM+'§§TagV07_3§§=='+%trim(wTagV07_3)+'||';
154800170202          strTRBM=strTRBM+'§§TagV07_4§§=='+%trim(wTagV07_4)+'||';
154900170202          strTRBM=strTRBM+'§§TagV07_5§§=='+%trim(wTagV07_5)+'||';
155000170201
155100170201        endsr;
155200161110
155300161110
155400161110
155500161110       //-------------------------------------------------------------*
155600161110       //Aggiunta allegati                                            *
155700161110       //-------------------------------------------------------------*
155800161110        Begsr sr_impAtchBm;
155900161110
156000161110             strTRBM=strTRBM+strTRBMATCH;
156100161110
156200161110        endsr;
156300130406
156400140806
156500130406
156600120619       //-------------------------------------------------------------*
156700120619       //Verifica eventuali personalizzazioni per il cliente          *
156800120619       //-------------------------------------------------------------*
156900120619        Begsr sr_cliper;
157000120619
157100120619        clear wcliper;
157200120619        clear wdcr;
157300140327        clear wdcrRev;
157400141128        clear wPINcode;
157500120619        clear dcli;
157600140327        clear tnsd99ds;
157700141128
157800141128        // Verifica se spedizione con particolarità consegna "PINcode"
157900141128        if blpGMA <> *blanks;
158000141128           if %lookup(blpGMA:sk7R_PINcode)>0;
158100141128              wPINcode = 'S';
158200141128            endif;
158300141128        endif;
158400140327
158500120619        if blpccm>0  ;
158600120619           wke1=%editc(blpccm:'X');
158700120619        else          ;
158800120619           wke1=%editc(blpksc:'X');
158900120619        endif;
159000130406        //  if §cliemd='S';
159100120619              // Se presente il contrassegno verifica se richiesto il contatto
159200120619              if ar9cas>0;
159300120619                 clear trul21ds;
159400120619                 i21tla='L';
159500120619                 i21cbo = blpcbo;
159600120619                 i21tsp = blptsp;
159700120619                 i21lnp = blplnp;
159800120619                 i21nzm = blpnzm;
159900120619                 i21lna = blplna;
160000120619                 i21nzd = blpnzd;
160100120619                 i21tic = ar9tic;
160200120619                 i21imp = ar9cas;
160300120619                 i21div = ar9vca;
160400120619                 i21ute = sdsusr;
160500120619                 i21pgm = nompgm;
160600130829              //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
160700130829                 if %lookup(blpcbo:cbf)>0;
160800130829                    i21ksc = blpksc;
160900130829                 else;
161000130829                    i21ksc = blpccm;
161100130829                    if i21ksc=0;
161200130829                       i21ksc=(blplnp*10000) + 8888;
161300130829                    endif;
161400130829                 endif;
161500120619                 trul21r(trul21ds);
161600120619              endif;
161700120619              // La personalizzazione della mail con l'indicazione della
161800120619              // consegna prevista viene fatta solo se la spedizione
161900120619              // non prevede contatti col destinatario:
162000120619                 // No Fermo deposito
162100120619              if blpffd=*blanks and (
162200120619                 // No appuntamento o supermercato OPPURE indicata DCR tassativa
162300120619              (blptc1 <>'A' and blptc1<>'S' and blptc2 <>'A' and blptc2<>'S') or
162400120619              (blpdcr>0 and blptcr=*blanks))
162500120619                 // No contrassegno oppure importo che non richiede
162600120619                 // l'abilitazione "manuale"
162700120619              and (ar9cas=0 or o21fca=*blanks);
162800120619                 wcliper='S';
162900120619              endif;
163000130406        //   endif;
163100120619        // Se personalizzazione da fare e non è indicata la data consegna
163200120619        // richiesta essa viene calcolata mediante pgm di calcolo delivery
163300120621        if wcliper='S' ;
163400160502          //        if blpdcr=0;
163500120621              d98tbo='P';
163600120621              d98aas=blpaas;
163700120621              d98lnp=blplnp;
163800120621              d98nrs=blpnrs;
163900120621              d98nsp=blpnsp;
164000130529              if blptsp='D';
164100130529                 I98TSP_FOR='C';
164200130529              endif;
164300120621              tnsd99r(tnsd99ds);
164400120621              if d98err='0';
164500151111                 wdcr=d98dee;
164600160502              else  ;
164700160502                if blpdcr>0  ;
164800160502                   wdcr=blpdcr;
164900160502                endif;
165000120621              endif;
165100160502             //        else;
165200160502             //           wdcr=blpdcr;
165300160502             //        endif;
165400160502
165500140327           wdcrRev = wdcr;
165600120621           if wdcr>0;
165700120621              dataeur=%date(wdcr:*iso);
165800120621              wdcr=%dec(dataeur);
165900120621           endif;
166000160502        endif;
166100150930
166200140324        clear trulORsds;
166300140324        clear trulOR2ds;
166400150730        clear diore01;
166500140324
166600140324        wSpedConsOrari = '.';
166700140324        reset wSpedConsOraDa;
166800140324        reset wSpedConsOraA;
166900140324
167000140327        IOREDTA  = wdcrRev;
167100140324        IOREFIL  = blpLNA;
167200140324        IORECAP  = blpCAD;
167300140324        IORELOC  = blpLOD;
167400140324        IORENAR  = blpNZD;
167500140324        IORETSER = 'C';
167600140324        IOREDDC  = blpDDC;
167700140324        IORENDC  = blpNDC;
167800140324        IORETFP  = blpTFP;
167900140324        IORETFA  = blpTFA;
168000140324        IOREDTI  = blpDTI;
168100140324        IOREHTI  = blpHTI;
168200140324        IOREDCR  = blpDCR;
168300140324        IOREHCR  = blpHCR;
168400140324        IORETCR  = blpTCR;
168500140324        IOREDEI  = d98DEI;
168600140324        IOREOEI  = d98OEI;
168700140324        IORETSP  = blpTSP;
168800150730        §IORETRAZC = %editc(D98TTC:'X');
168900150730        §IORECONSC = %editc(D98TCC:'X');
169000150730        IOREFLO  = diore01;
169100140324
169200140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
169300140324        if not %error AND OOREERR = *blanks;
169400140324           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
169500140324           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
169600141013         //  wSpedConsOrari = '<b>indicativamente</b>' +
169700141013           wSpedConsOrari = 'indicativamente' +
169800141013                            ' dalle ' + wSpedConsOraDa +
169900141013                            ' alle '   + wSpedConsOraA;
170000140324        endif;
170100120619
170200120619        endsr;
170300161115
170400161115
170500161115
170600161115       //-------------------------------------------------------------*
170700161115       //Verifica eventuali personalizzazioni per l'attach            *
170800161115       //-------------------------------------------------------------*
170900161115        Begsr sr_PDFper;
171000161115
171100161115        if blpccm>0;
171200161115           wke1=%editc(blpccm:'X');
171300161115        else;
171400161115           wke1=%editc(blpksc:'X');
171500161115        endif;
171600161115
171700161115        chain ('CLI':wke1) tntbe01l;
171800161115        if %found(tntbe01l);
171900161115           dcli=tbeuni;
172000161115        endif;
172100161115
172200161115        endsr;
172300140414
172400140806
172500140414
172600140414       //-------------------------------------------------------------*
172700140414       //inserimento Richiesta Assistenza                             *
172800140414       //-------------------------------------------------------------*
172900141016        Begsr sr_InsRA;
173000140414
173100141016         if chiudi = *blanks;
173200141016
173300140414           clear fidna6ds;
173400140414
173500140423           iDA06MAD = ' 82';
173600140609
173700170206           wConsegna = %scanrpl('<BR>' : '' : wConsegna);
173800170206           wConsegna = %scanrpl('<b>'  : '' : wConsegna);
173900170206           wConsegna = %scanrpl('</b>' : '' : wConsegna);
174000170206           wConsegna = %scanrpl('<i>'  : '' : wConsegna);
174100170206           wConsegna = %scanrpl('</i>' : '' : wConsegna);
174200170206           wConsegna = %scanrpl('<u>'  : '' : wConsegna);
174300170206           wConsegna = %scanrpl('</u>' : '' : wConsegna);
174400140609
174500140908           iDA06RSC = 'EMAIL';
174600160114
174700160114           // Verifico il tipo di email
174800160114           if wTipoEml = '*ATCH';
174900160114              iDA06NO1 = 'Email Affidamento + PDF a: '+%trim(§ar5eml)+
175000160114                         '  -  ' + wConsegna;
175100160114           else;
175200160114              iDA06NO1 = 'Email Affidamento a: '+%trim(§ar5eml)+
175300160114                         '  -  ' + wConsegna;
175400160114           endif;
175500160114
175600140414           iDA06TOR = 'S';
175700140520           iDA06OGG = %trim(%editc(blpLNP:'X')) +
175800140520                      %trim(%editc(blpNRS:'X')) +
175900140520                      %trim(%editc(blpNSP:'X')) +
176000140520                      %trim(%editc(blpAAS:'X'));
176100140414           iDA06UTE = 'QPGMR';
176200140520           iDA06FIL = blpLNP;
176300140414
176400140414           callP(e) FIDNA6R (fidna6ds);
176500160506
176600160506           // Inserimento spia per Predict DPD
176700160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
176800160506               exsr insPredict;
176900160506           endif;
177000141016
177100141016         endif;
177200140414
177300140414        endsr;
177400170201
177500170201
177600170201
177700170201       //-------------------------------------------------------------*
177800170201       //caricamento filiali abilitate al deploy                      *
177900170201       //-------------------------------------------------------------*
178000170201       Begsr sr_VPOR;
178100170201
178200170201          clear wVPOdeploy;
178300170201
178400170201          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
178500170201          clear TRULVPODS;
178600170201          IVPOke1 = 'DECOFIIDC';
178700170201          TRULVPOR (trulvpods);
178800170201          skFil_New = skFilvpo;
178900170201
179000170201          select;
179100170201            // se al primo elemento c'è 999 => tutto a modo nuovo
179200170201            when skFil_New(1) = '999';
179300170201                 wVPOdeploy = 'NUOVO';
179400170201
179500170201            // se al primo elemento c'è *blanks => tutto a modo vecchio
179600170201            when skFil_New(1) = *zeros;
179700170201                 wVPOdeploy = 'VECCHIO';
179800170201
179900170201            other;
180000170201                 wVPOdeploy = 'MISTO';
180100170201          endsl;
180200170201
180300170201       endsr;
180400170201
180500170201
180600170201
180700170201       //-------------------------------------------------------------*
180800170201       //Effettuo controlli pre invio 2                               *
180900170201       //-------------------------------------------------------------*
181000170201       Begsr sr_ChkDeploy;
181100170201
181200170201          *in60 = *off;
181300170201
181400170201          select;
181500170201            when wVPOdeploy = 'VECCHIO';
181600170201              *in60  = *off;
181700170201
181800170201            when wVPOdeploy = 'NUOVO';
181900170201              *in60  = *on;
182000170201
182100170201            when wVPOdeploy = 'MISTO';
182200170201              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
182300170201                *in60 = *on;
182400170201              endif;
182500170201          endsl;
182600170201
182700170201       endsr;
182800170201
182900140806
183000131210
183100131210       //-------------------------------------------------------------*
183200131210       //Invio email di alert                                         *
183300131210       //-------------------------------------------------------------*
183400131210       Begsr snd_Alert;
183500131210
183600171006          wEml   = 'cedalert@brt.it';
183700131210          wCcEml = *blanks;
183800131210          wOgg   = 'Anomalia invio avviso spedizione a destinatario ' +
183900131210                   w§CM1PROG;
184000131210          wMsg   = ' w§CM1PROG     '     + w§CM1PROG           + ':/N' +
184100131210                   ' ar5aas        '     + %editc(ar5aas:'X')  + ':/N' +
184200131210                   ' ar5lnp        '     + %editc(ar5lnp:'X')  + ':/N' +
184300131210                   ' ar5nrs        '     + %editc(ar5nrs:'X')  + ':/N' +
184400131210                   ' ar5nsp        '     + %editc(ar5nsp:'X')  + ':/N' +
184500131210                   ' §AR5ALM       '     + §AR5ALM             + ':/N' +
184600131210                   ' blpft1        '     + blpft1              + ':/N' +
184700131210                   ' §AR8PEML      '     + §AR8PEML            + ':/N' +
184800131210                   ' mEsito        '     + mEsito              + ':/N' +
184900131210                   ' wTipoEml      '     + wTipoEml            + ':/N' +
185000131210                   ' wAllega       '     + wAllega             + ':/N' +
185100131210                   ' wEmlSnt       '     + %editc(wEmlSnt:'X') + ':/N' +
185200131210                   ' w§CM1VAR      '     + w§CM1VAR            + ':/N' +
185300131210                   ' pPathOrig     '     + %trim(pPathOrig)    + ':/N' +
185400131210                   ' pPathZipAS    '     + %trim(pPathZipAS)   + ':/N' +
185500131210                   ' pPathZipSRV   '     + %trim(pPathZipSRV)  + ':/N' +
185600131210                   ' pShare        '     + %trim(pShare)       + ':/N' +
185700131210                   ' (EMD) ar5UNI  '     + %trimr(ar5UNI)      + ':/N' +
185800131210                   ' GEN_ar5UNI    '     + %trimr(GEN_ar5UNI)  + ':/N' +
185900131210                   ' (§PDF) ar8UNI '     + %trimr(ar8UNI)      + ':/N' +
186000131210                   ' pPathCli      '     + %trim(pPathCli)     + ':/N' +
186100131210                   ' pNomePDF      '     + %trim(pNomePDF)     + ':/N' +
186200161118                   ' pFileName     '     + %trim(pFileName)    + ':/N';
186300131210
186400131210          callP(e) TIS701C1 (wEml : wCcEml : wOgg : wMsg : wEsito);
186500131210
186600131210        endsr;
186700131210
