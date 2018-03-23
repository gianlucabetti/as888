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
010400161111     D wPDF_NoZip      s               n   inz
010500161118     D wOK_Atch        s               n   inz
010600170116     D wOBJM           s             20a   inz
010700170124     D wDivid          s                   like(divid)
010800141013
010900170124
011000170124     D infAR503        DS
011100170124     D   ar503_rrn           397    400I 0
011200170124
011300141013
011400141013     D BRTcode         ds                  qualified
011500141013     D   aas4                         4  0
011600141013     D   lnp                          3  0
011700141013     D   nrs                          2  0
011800141013     D   nsp                          7  0
011900141013     D   chkdgt1                      1  0
012000141013     D   code                         3  0
012100141013     D   chkdgt2                      1  0
012200141013     D   BRTcode               3     21
012300141013
012400140725
012500130406
012600130406     D* DICHIARAZIONE VARIABILI X INVIO PDF IN ALLEGATO
012700130406
012800130406     D FNLSPDFC2       PR
012900130406     D                                     extpgm('FNLSPDFC2')
013000130406     D iOpz                           1A
013100130406     D iPathOrig                    120A
013200130406     D iPathCli                            like(§AR8PPDFP)
013300130406     D iFileName                    256A
013400130406     D iPathZip                     120A
013500130503     D iShare                       120A
013600130406     D iProgress                     16A
013700130408     D iEsito                         1A
013800130406
013900130406     D PDF100R         PR
014000130406     D                                     extpgm('PDF100R')
014100130406     D iTBKEY                              like(TBKEY)
014200130406     D iTBELE                              like(TBELE)
014300130416     D iPGM                          10A
014400130406     D iTBDAT                              like(TBDAT)
014500130408     D iEsito                         1A
014600130406
014700161118     D FNLSPDFR1       PR
014800130406     D                                     extpgm('FNLSPDFR1')
014900130406     D iPathOrig                    120A
015000130406     D iPathCli                            like(§AR8PPDFP)
015100130430     D iNomePDF                      61A
015200130416     D iCheckOnly                     1A
015300130406     D iFileName                    256A
015400130408     D iEsito                         1A
015500161118
015600161118     D FNLSEMAR        PR
015700161118     D                                     extpgm('FNLSEMAR')
015800161118     D iPATH                        120A
015900161118     D iAAS                           4  0
016000161118     D iLNP                           3  0
016100161118     D iNRS                           2  0
016200161118     D iNSP                           7  0
016300161118     D oOK                            1
016400130625
016500130625     D FNLVEMR         PR
016600130625     D                                     extpgm('FNLVEMR')
016700130625     D iKPJBA                              likeds(kpjba)
016800130625     D iFNBLP                              likeds(fnblpds)
016900170201
017000170201     D TRULVPOR        PR
017100170201     D                                     extpgm('TRULVPOR')
017200170201     D trulvpods                           likeds(TRULVPODS)
017300130406
017400131210     D TIS701C1        PR
017500131210     D                                     extpgm('TIS701C1')
017600140423     D wEml                         253A
017700140423     D wCcEml                       253A
017800140423     D wOgg                          44A
017900140423     D wMsg                        5000A
018000140423     D wEsito                         1A
018100170201
018200170201
018300131210
018400140806     d wTipoEml        s              5A   inz
018500130406     d wAllega         s               N   inz(*off)
018600130406     d w§CM1VAR        s                   like(§CM1VAR)  inz
018700130406     d w§CM1PROG       s                   like(§CM1PROG) inz
018800130406     d pTBKEY          s                   like(TBKEY)
018900130406     d pTBELE          s                   like(TBELE)
019000130416     d pPGM            s             10A
019100130406     d pTBDAT          s                   like(TBDAT)
019200130406     d pPathCli        s                   like(§AR8PPDFP)
019300130430     d pNomePDF        s             61A
019400130416     d pCheckOnly      s              1A
019500130406     d pOpz            s              1A
019600130406     d pPathOrig       s            120A
019700161110     d pPathOrigWin    s            120A
019800130406     d pFileName       s            256A
019900130406     d pPathZipAS      s            120A
020000130503     d pShare          s            120A
020100130406     d pPathZipSRV     s            120A
020200130406     d pProgress       s             16A
020300130408     d mEsito          s              1A
020400140808     d wCnt            s              3  0
020500140808     d wOpz            s              1A
020600140808     d pTyp            s              1A
020700140808     d pTry            s              3  0
020800140423     d wEml            s            253A
020900140423     d wCcEml          s            253A
021000140423     d wOgg            s             44A
021100140423     d wMsg            s           5000A
021200140423     d wEsito          s              1A
021300141016     D wBRTcode        s             19                   inz
021400141016     D wBRTcodeLong    s             25                   inz
021500140623     d wSpedConsOrari  s             64A                  inz
021600140324     d wSpedConsOraDa  s              5A                  inz
021700140324     d wSpedConsOraA   s              5A                  inz
021800141016     d wLnaOraMMD_A    s              5A                  inz
021900141016     d wLnaOraMMA_A    s              5A                  inz
022000141016     d wLnaOraMPD_A    s              5A                  inz
022100141016     d wLnaOraMPA_A    s              5A                  inz
022200141016     d wLnaOraTMD_A    s              5A                  inz
022300141016     d wLnaOraTMA_A    s              5A                  inz
022400141016     d wLnaOraTPD_A    s              5A                  inz
022500141016     d wLnaOraTPA_A    s              5A                  inz
022600140423     d wConsegna       s           1000A   varying        inz
022700170206     d wConsegnaN      s           1000A   varying        inz
022800170202     d wVPOdeploy      s              6A   inz
022900170202     d wTypAlert       s              1A   inz
023000170202     d wRiga01         s           2000A   varying        inz
023100170202     d wRiga02         s           2000A   varying        inz
023200170202     d wRiga03         s           2000A   varying        inz
023300170202     d wRiga04         s           2000A   varying        inz
023400170202     d wRiga05         s           2000A   varying        inz
023500170202     d wRiga06         s           2000A   varying        inz
023600170202     d wRiga07         s           2000A   varying        inz
023700170202     d wRiga08         s           2000A   varying        inz
023800170711     d wTagRSM         s             35A   varying        inz
023900170202     d wTagV01         s             35A   varying        inz
024000170202     d wTagV02_1       s             35A   varying        inz
024100170202     d wTagV02_2       s             35A   varying        inz
024200170202     d wTagV02_3       s             35A   varying        inz
024300170202     d wTagV02_4       s             35A   varying        inz
024400170202     d wTagV03_1       s             35A   varying        inz
024500170202     d wTagV03_2       s             35A   varying        inz
024600170202     d wTagV04         s             35A   varying        inz
024700170202     d wTagV05         s             35A   varying        inz
024800170202     d wTagV06         s             35A   varying        inz
024900170202     d wTagV07_1       s             35A   varying        inz
025000170202     d wTagV07_2       s             35A   varying        inz
025100170202     d wTagV07_3       s             35A   varying        inz
025200170202     d wTagV07_4       s             35A   varying        inz
025300170202     d wTagV07_5       s             35A   varying        inz
025400170202
025500120619
025600170201     D TRULVPODS     e ds
025700170201     D  skFilvpo              16    765    dim(250)
025800170201     D skFil_New       s                   like(skFilvpo) dim(250) inz
025900120619
026000120619     d                sds
026100120619     d  nompgm                 1     10
026200120619     d  SDSusr               254    263
026300110617
026400110617      // ? PROTOTIPI ?
026500110617      /copy gaitrasrc/srcprotopr,trbmbypsr
026600120202      /copy gaitrasrc/srcprotopr,fnlv55r
026700120619      /copy gaitrasrc/srcprotopr,tnsd99r
026800170221      /copy gaitrasrc/srcProtoPr,UBRSMALR
026900110617
027000130406
027100110627     C     *ENTRY        PLIST
027200110627     C                   PARM                    chiudi            1
027300170124     C                   PARM                    divis             2 0
027400170124     C                   PARM                    divid             2 0
027500130406
027600110617      /free
027700140806
027800140806       // Inizializzo flag di errore globale procedura
027900140806       mEsito = '0';
028000140806
028100130604       // Carico le tabelle occorrenti
028200110627       exsr sr_cartab;
028300170201
028400170201       // carico le filiali per rilasci pilotati
028500170201       exsr sr_VPOR;
028600130625
028700130604       // Controllo se è raggiungibile la cartella remota su server
028800140808       exsr sr_OpzChkNet;
028900130604       if mEsito = '2';
029000130604       else;
029100130604          // Leggo fiar5 rk "EMD" da inviare
029200130604          exsr sr_Elabora;
029300130604       endif;
029400130406
029500110627       if chiudi='S';
029600140623          eval *inlr=*on;
029700130625          clear fnlvemds;
029800130625          iemTLA = 'C';
029900130625          kpjbu = fnlvemds;
030000130625          callP(e) FNLVEMR (kpjba : fnblpds);
030100140623       else;
030200140725          eval *inrt=*on;
030300140725       endif;
030400140725
030500170124       unlock(e) fiar500F;
030600140725       unlock(e) fiar804L;
030700160506
030800160506
030900160506       //-------------------------------------------------------------*
031000160506       //inserimento Predict DPD                                      *
031100160506       //-------------------------------------------------------------*
031200160506        Begsr insPredict;
031300160506
031400160506           clear tidp3000;
031500160506           dp3AAS   = blpAAS;
031600160506           dp3LNP   = blpLNP;
031700160506           dp3NRS   = blpNRS;
031800160506           dp3NSP   = blpNSP;
031900160506           dp3EVD   = '18';
032000160506           dp3CEV   = 'E25';
032100160506           dp3FLE   = blpLNA;
032200160518           dp3DEV   = 99991231;
032300160512           dp3HEV   = %dec(%time() : *HMS)/100;
032400160512           dp3DTSPE = blpAAS*10000+blpMGS;
032500160506           write tidp3000;
032600160506
032700160506        endsr;
032800140725
032900140725
033000140725       //-------------------------------------------------------------*
033100110627       //caricamento codici bolla con c/a                              *
033200110627       //-------------------------------------------------------------*
033300110627       Begsr SR_cartab;
033400130406
033500110627       // Leggo tabella "3A" per caricare schiera dei codici bolla con c/a
033600130829       // e schiera codici bolla franco
033700140725       i=0;
033800130829       y=0;
033900110628       clear cbc;
034000130829       clear cbf;
034100140806       setll (1:'3A') tabel00f;
034200140806       reade (1:'3A') tabel00f;
034300110627
034400140806       dow    not %eof(tabel00f);
034500140806          ds3a=tbluni;
034600110627          if §3afca>0;
034700110627             i=i+1;
034800110627             cbc(i)=tblkey;
034900140806          endif;
035000130829          if %subst(§3atb1:1:1)='F';
035100130829             y=y+1;
035200130829             cbf(y)=tblkey;
035300130829          endif;
035400140806          reade (1:'3A') tabel00f;
035500140806       enddo;
035600141128
035700141128
035800141128       // Leggo tabella "7R" per caricare particolarità consegna "PINcode"
035900141128       clear sk7R_PINcode;
036000141128       clear ix7R;
036100141128       setll (1:'7R') tabel00f;
036200141128       reade (1:'7R') tabel00f;
036300141128
036400141128       dow    not %eof(tabel00f);
036500141128          ds7r=tbluni;
036600141128          if §7RPINCODE = 'S';
036700141128             ix7R = ix7R + 1;
036800141128             sk7R_PINcode(ix7r)=tblkey;
036900141128          endif;
037000141128          reade (1:'7R') tabel00f;
037100141128       enddo;
037200150929
037300150929
037400150929       // Carico la lista di filiali "estere" (secondo il network)
037500150929       clear skFilExt;
037600150929       clear ixFilExt;
037700150929       setll (*zeros) azorg01l;
037800150929       read azorg01l;
037900150929
038000150929       dow not %eof(azorg01l);
038100150929           if ORGFVA = *blanks;
038200150929              og143 = orgDE3;
038300150929              chain ('NTW':§OGNTW) tntbe01l;
038400150929              if %found(tntbe01l);
038500150929                 dntw = tbeuni;
038600150929                 if §NTWFIE = 'E';
038700150929                    ixFilExt = ixFilExt + 1;
038800150929                    skFilExt(ixFilExt) = orgFIL;
038900160506                    if §OGNTW = 'DPD';
039000160506                       skFilExtDPD(ixFilExt) = orgFIL;
039100160506                    endif;
039200150929                 endif;
039300150929              endif;
039400150929           endif;
039500150929           read azorg01l;
039600150929       enddo;
039700130406
039800130406
039900140806       // Reperisco i parametri di procedura da PDFTABEL
040000140806
040100130406       pTBKEY = 'BRT';
040200130406       pTBELE = 'PATHORIGINE';
040300130416       pPGM   = 'FNLSEMR';
040400130406       pTBDAT = *blanks;
040500130406       callP(e) PDF100R (pTBKEY :
040600130406                         pTBELE :
040700130416                         pPGM   :
040800130406                         pTBDAT :
040900140806                         wEsito);
041000140806       if %error or wEsito <> '0';
041100140806          mEsito = '2';
041200130406       else;
041300130406          pPathOrig = %trim(pTBDAT);
041400130406       endif;
041500161110
041600161110
041700161110       pTBKEY = 'BRT';
041800161110       pTBELE = 'PATHORIGINWIN';
041900161110       pPGM   = 'FNLSEMR';
042000161110       pTBDAT = *blanks;
042100161110       callP(e) PDF100R (pTBKEY :
042200161110                         pTBELE :
042300161110                         pPGM   :
042400161110                         pTBDAT :
042500161110                         wEsito);
042600161110       if %error or wEsito <> '0';
042700161110          mEsito = '2';
042800161110       else;
042900161110          pPathOrigWin = %trim(pTBDAT);
043000161110       endif;
043100130406
043200140806
043300130406       pTBKEY = 'BRT';
043400130406       pTBELE = 'PATHZIPAS400';
043500130416       pPGM   = 'FNLSEMR';
043600130406       pTBDAT = *blanks;
043700130406       callP(e) PDF100R (pTBKEY :
043800130406                         pTBELE :
043900130416                         pPGM   :
044000130406                         pTBDAT :
044100140806                         wEsito);
044200140806       if %error or wEsito <> '0';
044300140806          mEsito = '2';
044400130406       else;
044500130406          pPathZipAS = %trim(pTBDAT);
044600130406       endif;
044700130406
044800140806
044900130406       pTBKEY = 'BRT';
045000130406       pTBELE = 'PATHZIPSERVER';
045100130416       pPGM   = 'FNLSEMR';
045200130406       pTBDAT = *blanks;
045300130406       callP(e) PDF100R (pTBKEY :
045400130406                         pTBELE :
045500130416                         pPGM   :
045600130406                         pTBDAT :
045700140806                         wEsito);
045800140806       if %error or wEsito <> '0';
045900140806          mEsito = '2';
046000130406       else;
046100130406          pPathZipSRV = %trim(pTBDAT);
046200130406       endif;
046300130503
046400140806
046500130503       pTBKEY = 'BRT';
046600130503       pTBELE = 'SHARE';
046700130503       pPGM   = 'FNLSEMR';
046800130503       pTBDAT = *blanks;
046900130503       callP(e) PDF100R (pTBKEY :
047000130503                         pTBELE :
047100130503                         pPGM   :
047200130503                         pTBDAT :
047300140806                         wEsito);
047400140806       if %error or wEsito <> '0';
047500140806          mEsito = '2';
047600130503       else;
047700130503          pShare = %trim(pTBDAT);
047800130503       endif;
047900130406
048000110627       endsr;
048100170124
048200170124
048300170124       //-------------------------------------------------------------*
048400170124       //Gestione elaborazione parallela                              *
048500170124       //-------------------------------------------------------------*
048600170124       Begsr SR_elapar;
048700170124
048800170124           *in40 = *off;                  // record KO
048900170124           if divis > *zeros AND divid > *zeros;
049000170124              wDivid = divid - 1;
049100170124              if %rem(ar5NSP:divis) = wDivid;
049200170124                 *in40 = *on;             // record OK
049300170124                 chain ar503_rrn fiar500f;
049400170124              endif;
049500170124           else;
049600170124              *in40 = *on;                // record OK
049700170124           endif;
049800170124
049900170124       endsr;
050000130406
050100130406
050200110617       //-------------------------------------------------------------*
050300110617       //Lettura fiar5 record "EMD" da inviare                         *
050400110617       //-------------------------------------------------------------*
050500110617       Begsr SR_Elabora;
050600110617        setll (kdto) fiar503l;
050700110617        reade (kdto) fiar503l;
050800130605
050900130605        // inizializzo il contatore di email con allegato
051000140806        wEmlSnt = *zeros;
051100110617
051200140806        dow not %eof(fiar503l) and wEmlSnt < 50;
051300130406
051400130502        // controllo se è stata richiesta la chiusura del sottosistema
051500110627           if %shtdn;
051600110627               chiudi='S';
051700110627               leavesr;
051800110627           endif;
051900170124
052000170124        // Se richiesta elaborazione parallela gestisco
052100170124           exsr sr_elapar;
052200170124
052300170124        // Se OK record da elaborare => procedo
052400170124           if *in40;
052500130406
052600110617        // controllo lo stato della bolla: per poter inviare bisogna
052700110617        // che la bolla risulti partita
052800110617           exsr sr_chkstblp;
052900110617           if bolok='S';
053000131008              dar5emd=ar5uni;
053100131008              exsr sr_inviamail;
053200110617           endif;
053300150930           if bolok='E';
053400150930              clear wdcrRev;
053500150930              clear trulORsds;
053600150930              clear trulOR2ds;
053700150930              dar5emd=ar5uni;
053800150930              exsr sr_UpdFIAR5;
053900150929           endif;
054000130406
054100170124           endif;
054200170124
054300130605           reade (kdto) fiar503l;
054400110617        enddo;
054500130406
054600110617        endsr;
054700130406
054800130406
054900110617       //-------------------------------------------------------------*
055000110617       //Verifica stato della bolla                                   *
055100110617       //-------------------------------------------------------------*
055200130625        Begsr sr_chkstblp;
055300130406
055400140806        // Inizializzazioni di verifica
055500110617        bolok=' ';
055600131003        bologgi=' ';
055700140806
055800140806        // Inizializzo email come NO allegati
055900140806        wTipoEml = '*STD';
056000140806        wAllega = *off;
056100130625
056200140806
056300130625        // Effettuo controlli tramite driver che detrmina "inviabilità" delle
056400130625        // comunicazioni ai destinatari
056500130625        clear fnlvemds;
056600130625        clear fnblpds;
056700130625        iemAAS = ar5aas;
056800130625        iemLNP = ar5lnp;
056900130625        iemNRS = ar5nrs;
057000130625        iemNSP = ar5nsp;
057100130625        kpjbu = fnlvemds;
057200130625        callP(e) FNLVEMR (kpjba : fnblpds);
057300130625        fnlvemds = kpjbu;
057400140725
057500130625
057600131008        // Verifico se esito favorevole al invio
057700140725 ===>   if oemESITO = '1';
057800150929
057900150929           // Verifico che NON trattasi di spedizione "export"
058000150929           if  %lookup(blpLNA:skFilExt) > 0;
058100150929               bolok = 'E';
058200150929           endif;
058300140725
058400150929           // Se tutto OK => procedo
058500150929           if bolok <> 'E';
058600170221
058700170221           // Verifico se per cliente corrente presente forzatura
058800170221           // per Rag.Soc. invio alerts
058900170221           UBRSMALDS.iTpRic   = 'E';
059000170221           UBRSMALDS.iOrigine = 'P';
059100170221           UBRSMALDS.iAAS     = blpAAS;
059200170221           UBRSMALDS.iLNP     = blpLNP;
059300170221           UBRSMALDS.iNRS     = blpNRS;
059400170221           UBRSMALDS.iNSP     = blpNSP;
059500170221           UBRSMALR_S ( UBRSMALDS );
059600170221           if UBRSMALDS.oEsito = *zeros;
059700170221              wRSM_Eml = %trim(UBRSMALDS.oRagSoc);
059800170221           else;
059900170221              wRSM_Eml = %trim(blpRSM);
060000170221           endif;
060100141013
060200141013           // Calcolo il BRTcode (tramite apposito driver)
060300141013           clear BRTcode;
060400141013           BRTcode.aas4    = blpAAS;
060500141013           BRTcode.lnp     = blpLNP;
060600141013           BRTcode.nrs     = blpNRS;
060700141013           BRTcode.nsp     = blpNSP;
060800141013
060900141013           clear trul28ds0;
061000141013           trul28ds0.i280COD = BRTcode.BRTcode;
061100141013           callP(e) TRUL28R0 (trul28ds0);
061200141013
061300141013           // Se BRTcode errato => esito KO
061400141016           clear wBRTcode;
061500141016           clear wBRTcodeLong;
061600141013           if %error OR trul28ds0.o280err = '1';
061700141013           else;
061800141016              wBRTcode     = %subst(trul28ds0.o280cod:1:5)      +
061900141016                             %subst(trul28ds0.o280cod:6:5)      +
062000141016                             %subst(trul28ds0.o280cod:11:5)     +
062100141016                             %subst(trul28ds0.o280cod:16:4);
062200170201              wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +'  '+
062300141016                             %subst(trul28ds0.o280cod:6:5) +'  '+
062400141016                             %subst(trul28ds0.o280cod:11:5)+'  '+
062500141016                             %subst(trul28ds0.o280cod:16:4);
062600141013           endif;
062700141013
062800131008
062900131008           // Verifico se per la spedizione corrente è previsto invio allegati
063000140806           clear dar5gen;
063100131008           chain (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'GEN') fiar501l;
063200131008           if  %found(fiar501l);
063300131008               dar5gen = gen_ar5UNI;
063400131008           endif;
063500131008
063600131008           // Verifico se previsto invio allegati oppure no
063700131008           // Con invio allegati aspetto chiusura fogli viaggio partenza
063800161121           // if (§AR5ALM='S' AND blpft1<>*blanks) OR §AR5ALM<>'S';
063900140806
064000140806              // Se previsto invio allegati => imposto email di tipo "allegati"
064100140806              if §AR5ALM = 'S';
064200140806                 wTipoEml = '*ATCH';
064300140806              endif;
064400140806
064500131008              // Effettuo verifica anche su data/ora invio rispetto alla bolla
064600131008              if blpaas*10000+blpmgs  < %dec(%date():*ISO);
064700131008                 bolok='S';
064800131008              endif;
064900131008              if blpaas*10000+blpmgs  = %dec(%date():*ISO) AND
065000131008                 %dec(%time():*ISO)  >= 150000;
065100131008                 bolok='S';
065200131008                 bologgi='S';
065300131008              endif;
065400131008
065500161121           // endif;
065600131008
065700131002
065800131002           if bolok='S';
065900131002              // data spedizione in formato gg/mm/aaaa
066000131002              wdataspe=blpaas*10000+blpmgs;
066100131002              dataeur=%date(wdataspe:*iso);
066200131002              wdataspe=%dec(dataeur);
066300131002              wPOspe=%editc(blpLNP:'X');
066400131002              if blpccm > *zeros;
066500131002                 wPOcli=%editc(blpCCM:'X');
066600131002              else;
066700131002                 wPOcli=%editc(blpKSC:'X');
066800131002              endif;
066900131002           endif;
067000150929
067100150929           endif;
067200150929
067300110617        endif;
067400130406
067500110617        endsr;
067600130406
067700150929
067800130406
067900110617       //-------------------------------------------------------------*
068000110617       //Richiamo driver per invio mail e aggiorno dto su fiar5       *
068100110617       //-------------------------------------------------------------*
068200110617        Begsr sr_inviamail;
068300130406
068400161110        // Inizializzo variabili di work
068500161110        clear strTRBMATCH;
068600140806
068700130406        // chaino file accessori che mi servono per il passaggio info
068800130406        // chaino contrassegno se presente
068900110621        ar9cas = *zeros;
069000110627        if %lookup(blpcbo:cbc)>0;
069100110617           chain (ar5aas:ar5lnp:ar5nrs:ar5nsp) fiar901l;
069200110617        endif;
069300130406
069400140806
069500130406        // chaino azorg della linea di arrivo
069600140401        clear og129;
069700141016        reset wLnaOraMMD_A;
069800141016        reset wLnaOraMMA_A;
069900141016        reset wLnaOraMPD_A;
070000141016        reset wLnaOraMPA_A;
070100141016        clear og130;
070200141016        reset wLnaOraTMD_A;
070300141016        reset wLnaOraTMA_A;
070400141016        reset wLnaOraTPD_A;
070500141016        reset wLnaOraTPA_A;
070600130406        chain (blplna) azorg01l;
070700141016
070800141016        og129 = orgDC9;
070900140401        if §OGORFMD <> *zeros AND §OGORFMD <> *blanks;
071000141016           wLnaOraMMD_A = %subst(§OGORFMD:1:2)+':'+%subst(§OGORFMD:3:2);
071100140401        endif;
071200140401        if §OGORFMA <> *zeros AND §OGORFMA <> *blanks;
071300141016           wLnaOraMMA_A = %subst(§OGORFMA:1:2)+':'+%subst(§OGORFMA:3:2);
071400140401        endif;
071500140401        if §OGORFPD <> *zeros AND §OGORFPD <> *blanks;
071600141016           wLnaOraMPD_A = %subst(§OGORFPD:1:2)+':'+%subst(§OGORFPD:3:2);
071700140401        endif;
071800140401        if §OGORFPA <> *zeros AND §OGORFPA <> *blanks;
071900141016           wLnaOraMPA_A = %subst(§OGORFPA:1:2)+':'+%subst(§OGORFPA:3:2);
072000140401        endif;
072100141016
072200141016        og130 = orgDD0;
072300141016        if §OGORAMD <> *zeros AND §OGORAMD <> *blanks;
072400141016           wLnaOraTMD_A = %subst(§OGORAMD:1:2)+':'+%subst(§OGORAMD:3:2);
072500141016        endif;
072600141016        if §OGORAMA <> *zeros AND §OGORAMA <> *blanks;
072700141016           wLnaOraTMA_A = %subst(§OGORAMA:1:2)+':'+%subst(§OGORAMA:3:2);
072800141016        endif;
072900141016        if §OGORAPD <> *zeros AND §OGORAPD <> *blanks;
073000141016           wLnaOraTPD_A = %subst(§OGORAPD:1:2)+':'+%subst(§OGORAPD:3:2);
073100141016        endif;
073200141016        if §OGORAPA <> *zeros AND §OGORAPA <> *blanks;
073300141016           wLnaOraTPA_A = %subst(§OGORAPA:1:2)+':'+%subst(§OGORAPA:3:2);
073400141016        endif;
073500130406
073600140324
073700130406        // Se la linea di arrivo è in £6 cerco la sua capofila
073800120202        clear wlna;
073900120202        clear fnlv55ds;
074000120202        d55lin = blplna;
074100120202        d55tpt='6';
074200120202        d55drf=blpaas*10000+blpmgs;
074300120202        fnlv55r(fnlv55ds);
074400120202        wlna=d55tfa;
074500130406
074600140806
074700130406        // Stacco subito un progressivo per la email corrente
074800130408        exsr sr_rtvPrg;
074900130408
075000140806
075100140806        // Se email di tipo "allegati" => effettuo iter preposto
075200140806        if wTipoEml = '*ATCH';
075300161118
075400161118           // Inizializzo gli allegati a NO
075500161118           wAllega = *off;
075600140806
075700161118           // Verifico se per la bolla corrente tutti gli allegati sono OK
075800161118           callP(e) FNLSEMAR (pPathOrig
075900161118                             :blpAAS
076000161118                             :blpLNP
076100161118                             :blpNRS
076200161118                             :blpNSP
076300161118                             :wOK_Atch);
076400161118
076500161118           // Invio email con allegati solo se:
076600161118           // - o tutti allegati esistenti e OK
076700161222           // - o oppure chisura fogli viaggio partenza effettuata AND
076800161223           //     data/ora corrente oltre le 23:00 della data bolla
076900161222           if wOK_Atch OR (blpft1<>*blanks AND
077000161223                       %char(%dec(%timestamp:*ISO)) >=
077100161223                       %char(blpAAS*10000+blpMGS)+'230000');
077200161118
077300161118              // Verifico eventuali personalizzazioni relative al PDF
077400161118              exsr sr_PDFper;
077500161118
077600161118              // Scorro tutto il FIAR8/§PDF della bolla corrente
077700161118              exsr sr_elaAR8;
077800161118
077900161118              // Verifico l'esito finale di uscita
078000161118              if wAllega;
078100161118                 wEmlSnt = wEmlSnt + 1;
078200161118              endif;
078300161118
078400161118           endif;
078500140806
078600130406        endif;
078700170123
078800130406
078900170123        // Se tutto OK procedo
079000170123        if wTipoEml <> '*ATCH' OR (wTipoEml = '*ATCH' AND wAllega);
079100130406
079200130406        // In relazione al tipo di email da inviare (standard o con allegato)
079300130406        // reperisco la tabella MRA key "TRBMBYPSR" corrispondente
079400130406        // e imposto le relative "particolarità"
079500170116        clear indmrabyps;
079600170116
079700170116        // Se spedizione con appuntamento
079800170116        wOBJM = *blanks;
079900170116        if blpTC1='A' or blpTC2='A';
080000170116           wOBJM = '*OBJM*BRT  -';
080100170116        else;
080200170116           wOBJM = '*OBJM*BRT -';
080300170116        endif;
080400170116
080500130406        select;
080600141014          when chiudi = 'T';
080700141014             chain ('MRA':'TRBMBYPSR':'TEST') tntbe01l  ;
080800141014             if blpFFD <> 'S';
080900170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ' +
081000141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
081100141014             else;
081200170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione '+
081300141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
081400141014             endif;
081500141014             exsr sr_invEml;
081600161116          when wTipoEml = '*ATCH';
081700130406             chain ('MRA':'TRBMBYPSR':'*ATCH') tntbe01l  ;
081800141014             if blpFFD <> 'S';
081900170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ';
082000141014             else;
082100170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione ';
082200141014             endif;
082300161116             if wPDF_NoZip;
082400161117                w§CM1VAR = %trim(w§CM1VAR) + '  ' +
082500161116                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
082600161116             else;
082700161116                // se presente almeno un allegato da inviare
082800161116                if wAllega;
082900161116                   w§CM1VAR = %trim(w§CM1VAR)+'§*ATCH*'+%trim(pPathZipSRV)+
083000161116                              %trim(w§CM1PROG)+'.zip';
083100161116                endif;
083200161116             endif;
083300130723             exsr sr_invEml;
083400140806          other;
083500130408             chain ('MRA':'TRBMBYPSR':*blanks) tntbe01l  ;
083600141014             if blpFFD <> 'S';
083700170116                w§CM1VAR = %trim(wOBJM)+' Avviso affidamento spedizione ' +
083800130406                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
083900141014             else;
084000170116                w§CM1VAR = %trim(wOBJM)+'  Avviso affidamento spedizione '+
084100141014                   %editc(blplnp:'X')+%editc(blpnrs:'X')+%editc(blpnsp:'X');
084200141014             endif;
084300130723             exsr sr_invEml;
084400130406        endsl;
084500170123
084600170123        endif;
084700130406
084800110617        endsr;
084900130406
085000140806
085100130406
085200130406       //-------------------------------------------------------------*
085300130406       //elaborazione PDF da allegare alla email (FIAR804L)           *
085400161110       //-------------------------------------------------------------*
085500161110        Begsr sr_elaAR8;
085600130406
085700161115           // Se non espressamente richiesto NO ZIP
085800130406           // Creo la cartella di lavoro x email corrente
085900161115           if not wPDF_NoZip;
086000161115              exsr sr_OpzCrtDir;
086100161115           endif;
086200130406
086300140806           // Ad ogni passaggio testo se tutto ok
086400140806           if mEsito <> '2';
086500140806
086600140806             // Quindi procedo con l'elaborazione degli allegati
086700140806             setll (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
086800140806             reade (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
086900130406
087000140806             // leggo TUTTO il fiar804l
087100140806             dow not %eof(fiar804l);
087200140806
087300140806               // Ad ogni check di PDF da allegare => inizializzo flag errore
087400140806               mEsito = '0';
087500140806
087600140806               // Ridefinisco i dati relativi al PDF
087700140806               dar8§pdf = ar8UNI;
087800140806               pPathCli = §AR8PPDFP;
087900140806               pNomePDF = §AR8PPDFN;
088000140806
088100140806               // Se serve verifico presenza e univocità del file da trattare
088200140806               if %scan('*':pNomePDF) > *zeros;
088300140808                  exsr sr_OpzRtvPDF;
088400140806               else;
088500140806                  pFileName = pNomePDF;
088600140806               endif;
088700140806
088800140806               // Se tutto OK procedo con l'elaborazione allegati/invio email
088900140806               if mEsito <> '0';
089000140806                  §AR8PEML = 'E';      // PDF non presente (o non univoco)
089100140806               else;
089200140806
089300161115                  // Se non espressamente richiesto NO ZIP
089400161115                  // Eseguo copia da cartella remota su IFS
089500161115                  if not wPDF_NoZip;
089600161115                     exsr sr_OpzCpyAtch;
089700161115                  else;
089800161115                     strTRBMATCH=strTRBMATCH+'§§Atch§§=='+%trim(pPathOrigWin)+
089900161115                        '\'+%trim(pPathCli)+'\'+%trim(pFileName)+'||';
090000161115                  endif;
090100161115
090200140806                  // Se OK => segno almeno un allegato da inviare
090300140806                  if mEsito = '0';
090400140806                     wAllega = *on;
090500140806                     §AR8PEML = 'S';  // PDF ok
090600140806                  else;
090700140806                     §AR8PEML = 'E';  // PDF non presente o errore in attach PDF
090800140806                  endif;
090900140806               endif;
091000140806
091100140806               // Aggiorno lo stato del FLAG INVIO EMAIL PDF
091200140806               ar8UNI = dar8§pdf;
091300141014               if chiudi = *blanks;
091400141014                  update fiar8000;
091500141014               endif;
091600130406
091700140806               // quindi prosegui con la lettura
091800140806               reade (ar5AAS:ar5LNP:ar5NRS:ar5NSP:'§PDF') fiar804l;
091900130406
092000140806             enddo;
092100140806
092200140806           endif;
092300130406
092400130408           // Al termine se ci sono allegati da allegare => creo ZIP
092500161115           if wAllega AND not wPDF_NoZip;
092600140808              exsr sr_OpzZipAtch;
092700140806              // Se errore in "zippamento" allegato => KO tutta email "ATCH"
092800140806              if mEsito <> '0';
092900140806                 wAllega = *off;
093000140806              endif;
093100130406           endif;
093200161115
093300161115           // Se non espressamente richiesto NO ZIP
093400161115           // Elimino la cartella di lavoro x email corrente
093500161115           if not wPDF_NoZip;
093600161115              exsr sr_OpzDelDir;
093700161115           endif;
093800130406
093900130406        endsr;
094000130723
094100140806
094200130723
094300130723       //-------------------------------------------------------------*
094400130723       //Invio effettivo della email                                  *
094500130723       //-------------------------------------------------------------*
094600130723        Begsr sr_invEml;
094700130723
094800130723          if %found(tntbe01l);
094900170203
095000170203             // Verifoco se per linea arrivo corrente previsto nuovo deploy
095100170203             exsr sr_ChkDeploy;
095200170203
095300170203             // Impostazioni ambiente e variabili specifiche
095400130723             indmrabyps=tbeuni;
095500130723             exsr sr_trtcm1ds;
095600161115             exsr sr_cliper;
095700170201
095800170201             // Compongo il testo del SMS e gestisco modo vecchio/nuovo
095900170201             if not *in60;
096000170201                exsr sr_impVarBm;
096100170201             else;
096200170201                exsr sr_impVarBmNew;
096300170201             endif;
096400170201
096500161110             exsr sr_impAtchBm;
096600130723             pesito=0;
096700130723             if Bartmail_insert(strTRBM:indmrabyps:
096800130723                    intrtcm1ds:0:pesito)=0 and pesito=0;
096900150930                exsr sr_UpdFIAR5;
097000131210
097100131210                // Verifico se condizione di anomalia
097200131210                if §AR5ALM='S' and §AR8PEML = *blanks;
097300131210                   exsr snd_Alert;
097400140414                else;
097500140414                   // Se tutto ok => inserisco R.A.
097600140414                   exsr sr_InsRA;
097700131210                endif;
097800131210
097900130723             endif;
098000130723          endif;
098100130723
098200130723        endsr;
098300130406
098400140806
098500130406
098600130406       //-------------------------------------------------------------*
098700140806       // Gestione tentativi esecuzioni comandi                       *
098800130406       //-------------------------------------------------------------*
098900140806        Begsr sr_ExeOpz;
099000140806
099100140808          wCnt = 0;
099200140808          wEsito = '0';
099300140808          dou wCnt >= pTry or wEsito = '0';
099400140808              select;
099500140808                when pTyp = '1';
099600140808                     wOpz = pOpz;
099700140808                     exsr sr_RunOpz;
099800140808                when pTyp = '2';
099900140808                     exsr sr_findPDF;
100000140808              endsl;
100100140808
100200140808              // Se problema
100300140806              if %error or wEsito <> '0';
100400140806                 wEsito = 'E';
100500140806
100600140808                 // Eseguo attesa
100700140808                 wOpz = 'S';
100800140808                 exsr sr_RunOpz;
100900140806
101000140806                 // Incremento tentativi
101100140808                 wCnt = wCnt + 1;
101200140806
101300140806              endif;
101400140806
101500140806          enddo;
101600140806
101700140806          if wEsito <> '0';
101800140806             mEsito = '2';
101900140806          endif;
102000130406
102100130406        endsr;
102200150930
102300150930
102400150930
102500150930       //-------------------------------------------------------------*
102600150930       // Gestione tentativi esecuzioni comandi                       *
102700150930       //-------------------------------------------------------------*
102800150930        Begsr sr_UpdFIAR5;
102900150930
103000150930          eval §ar5dto=%char(%dec(%timestamp:*ISO));
103100150930          §ar5DPC  = %editc(wdcrRev:'X');
103200150930          §ar5OPCD = %editc(OOR2STIS:'X');
103300150930          §ar5OPCA = %editc(OOR2STFS:'X');
103400150930          // Verifico che i valori sopra attribuiti siano diversi da zero
103500150930          if §AR5DPC  = *all'0';
103600150930             §AR5DPC  = *blanks;
103700160330             §AR5OPCD = *zeros;
103800160330             §AR5OPCA = *zeros;
103900150930          endif;
104000150930          if §AR5OPCD = *all'0';
104100150930             §AR5OPCD = *blanks;
104200150930          endif;
104300150930          if §AR5OPCA = *all'0';
104400150930             §AR5OPCA = *blanks;
104500150930          endif;
104600150930          ar5uni=dar5emd;
104700150930          if chiudi = *blanks;
104800150930             update fiar5000;
104900150930          endif;
105000150930
105100150930        endsr;
105200140808
105300140808
105400140808
105500140808       //-------------------------------------------------------------*
105600140808       // Richiamo driver "trova PDF"                                 *
105700140808       //-------------------------------------------------------------*
105800140808        Begsr sr_findPDF;
105900140808
106000170125            pFileName  = *blanks;
106100170125            pCheckOnly = *blanks;
106200140808            callP(e) FNLSPDFR1 (pPathOrig  :
106300140808                                pPathCli   :
106400140808                                pNomePDF   :
106500140808                                pCheckOnly :
106600140808                                pFileName  :
106700140808                                wEsito);
106800140808
106900140808        endsr;
107000130406
107100130502
107200130502
107300130502       //-------------------------------------------------------------*
107400140808       // Eseguo Opzione richiesta                                    *
107500130502       //-------------------------------------------------------------*
107600140808        Begsr sr_RunOpz;
107700130502
107800140808          pProgress = w§CM1PROG;
107900140808          callP(e) FNLSPDFC2 (wOpz       :
108000130502                              pPathOrig  :
108100130502                              pPathCli   :
108200130502                              pFileName  :
108300130502                              pPathZipAS :
108400130503                              pShare     :
108500130502                              pProgress  :
108600140806                              wEsito);
108700130502
108800130502        endsr;
108900140808
109000140808
109100140808
109200140808       //-------------------------------------------------------------*
109300140808       // Opzione - Reperisci PDF                                     *
109400140808       //-------------------------------------------------------------*
109500140808        Begsr sr_OpzRtvPDF;
109600140808
109700140808           pTyp = '2';
109800140808           pTry = 120;
109900140808           exsr sr_exeOpz;
110000140808
110100140808        endsr;
110200140808
110300140808
110400140808
110500140808       //-------------------------------------------------------------*
110600140808       // Opzione "H" - Check Network                                 *
110700140808       //-------------------------------------------------------------*
110800140808        Begsr sr_OpzChkNet;
110900140808
111000140808           pTyp = '1';
111100140808           pTry = 120;
111200140808           pOpz = 'H';
111300140808           exsr sr_exeOpz;
111400140808
111500140808        endsr;
111600140808
111700140808
111800140808
111900140808       //-------------------------------------------------------------*
112000140808       // Opzione "D" - Crea Directory IFS                            *
112100140808       //-------------------------------------------------------------*
112200140808        Begsr sr_OpzCrtDir;
112300140808
112400140808           pTyp = '1';
112500140808           pTry = 120;
112600140808           pOpz = 'D';
112700140808           exsr sr_exeOpz;
112800140808
112900140808        endsr;
113000140808
113100140808
113200140808
113300140808       //-------------------------------------------------------------*
113400140808       // Opzione "C" - Copia Attach su IFS                           *
113500140808       //-------------------------------------------------------------*
113600140808        Begsr sr_OpzCpyAtch;
113700140808
113800140808           pTyp = '1';
113900140808           pTry = 120;
114000140808           pOpz = 'C';
114100140808           exsr sr_exeOpz;
114200140808
114300140808        endsr;
114400140808
114500140808
114600140808
114700140808       //-------------------------------------------------------------*
114800140808       // Opzione "Z" - Zippa Attach                                  *
114900140808       //-------------------------------------------------------------*
115000140808        Begsr sr_OpzZipAtch;
115100140808
115200140808           pTyp = '1';
115300140808           pTry = 120;
115400140808           pOpz = 'Z';
115500140808           exsr sr_exeOpz;
115600140808
115700140808        endsr;
115800140808
115900140808
116000140808
116100140808       //-------------------------------------------------------------*
116200140808       // Opzione "E" - Elimina Directory IFS                         *
116300140808       //-------------------------------------------------------------*
116400140808        Begsr sr_OpzDelDir;
116500140808
116600140808           pTyp = '1';
116700140808           pTry = 60;
116800140808           pOpz = 'E';
116900140808           exsr sr_exeOpz;
117000140808
117100140808        endsr;
117200130406
117300130406
117400130406
117500110617       //-------------------------------------------------------------*
117600110617       //Valorizzazione trtcm1ds                                      *
117700110617       //-------------------------------------------------------------*
117800110617        Begsr sr_trtcm1ds;
117900130406
118000110617           clear intrtcm1ds;
118100130408           if %scan('*PO':§mraemlmit) > *zeros;
118200130827              string=%replace(wPOspe:§mraemlmit:
118300130408                              %scan('*PO':§mraemlmit:1):3);
118400130827           else;
118500130827              if %scan('*PC':§mraemlmit) > *zeros;
118600130827                 string=%replace(wPOcli:§mraemlmit:
118700130827                                 %scan('*PC':§mraemlmit:1):3);
118800130827              else;
118900130827                 string=§mraemlmit;
119000130827              endif;
119100130827           endif;
119200110617           exsr sr_chkstr;
119300110617           §CM1MITT=string;
119400110617           string=§ar5eml;
119500110617           exsr sr_chkstr;
119600141014           select;
119700141014              when chiudi = 'T';
119800141014                   §CM1DST='fabrizio.gurrieri@brt.it';
119900141014              when chiudi = *blanks;
120000141014                   §CM1DST=string;
120100141014           endsl;
120200170202           if not *in60;
120300170202              §CM1TIPS=§mrareg;
120400170202           else;
120500170202              §CM1TIPS=§mraregnew;
120600170202           endif;
120700110617           if §mrapo<> '*PO';
120800130406              §CM1PO=§mrapo;
120900110617           else;
121000130406              §CM1PO=%editc(blplnp:'X');
121100110617           endif;
121200130406           §CM1VAR=w§CM1VAR;
121300130406           §CM1STS='0';
121400130406           §CM1IDP=§mraidpro;
121500130406           §CM1PROG=w§CM1PROG;
121600130406           §CM1ATT='01';
121700110617           §CM1TOTATT='01';
121800130406
121900110617        endsr;
122000130406
122100140806
122200130406
122300110617       //-------------------------------------------------------------*
122400110617       //controllo stringa                                            *
122500110617       //-------------------------------------------------------------*
122600110617        Begsr sr_chkstr;
122700110617           string=%xlate('||':'  ':string);
122800110617           string=%xlate('==':'  ':string);
122900110617        endsr;
123000130406
123100140806
123200130406
123300130406       //-------------------------------------------------------------*
123400130406       //reperimento progressivo                                      *
123500130406       //-------------------------------------------------------------*
123600130406        Begsr sr_rtvPrg;
123700130406
123800130406           clear trul33ds;
123900130406           w§CM1PROG=*all'0';
124000130406           i33Cnu = 24;
124100130406           i33num = 1;
124200130406           kpjbu = trul33ds;
124300130406           trul33r(kpjba);
124400130406           trul33ds = kpjbu;
124500130406           w§CM1PROG=%subst(%editc(o33nri:'X'):9:7);
124600130406
124700130406        endsr;
124800130406
124900140806
125000130406
125100110617       //-------------------------------------------------------------*
125200110617       //Imposto campo contenente la stringa per driver TRBMBYPSR     *
125300170201       //-------------------------------------------------------------*
125400170201        Begsr sr_impvarbm;
125500130406
125600140423           clear wConsegna;
125700140423
125800110617           strTRBM='§§Tag01§§=='+%trim(blprsd)+'||';
125900140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)+'||';
126000110617           strTRBM=strTRBM+'§§Tag03§§=='+%trim(blpinm)+'||';
126100110617           strTRBM=strTRBM+'§§Tag04§§=='+%trim(blpcam)+'||';
126200110617           strTRBM=strTRBM+'§§Tag05§§=='+%trim(blplom)+'||';
126300110617           strTRBM=strTRBM+'§§Tag06§§=='+%trim(blpprm)+'||';
126400140704           strTRBM=strTRBM+'§§Tag07§§=='+%editw(wdataspe:'0  /  /    ')+'||';
126500161003        // strTRBM=strTRBM+'§§Tag08§§=='+'http://vas.brt.it/vas/sped';
126600141013        // strTRBM=strTRBM+'_det_show.hsm?referer=sped_numspe_par.htm&Nspediz=';
126700141013        // strTRBM=strTRBM+%editc(blplnp:'X');
126800141013        // strTRBM=strTRBM+%editc(blpnrs:'X');
126900141013        // strTRBM=strTRBM+%editc(blpnsp:'X');
127000141013        // strTRBM=strTRBM+'&RicercaNumeroSpedizione=Ricerca';
127100141013        // strTRBM=strTRBM+'&AnnoSpedizione='+%editc(blpaas:'X');
127200141013        // strTRBM=strTRBM+'&urltype=a'+'||';
127300141013           strTRBM=strTRBM+'§§Tag08§§=='+
127400161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
127500141013           strTRBM=strTRBM+wBRTcode;
127600141013           strTRBM=strTRBM+'&urltype=a'+'||';
127700151111
127800151111           // Se spedizione con appuntamento
127900151111           if blpTC1='A' or blpTC2='A';
128000151112             strTRBM=strTRBM+'§§TagT01§§=='+'destinata che prevede la ' +
128100151112                     'CONSEGNA SU APPUNTAMENTO.<BR><BR>' +
128200151112                     'Sarà contattato da un nostro operatore per concordare ' +
128300151112                     'l''orario di consegna a Lei più gradito.'+'||';
128400151112             strTRBM=strTRBM+'§§TagT02§§=='+'precise,'+'||';
128500151111           else;
128600151112             strTRBM=strTRBM+'§§TagT01§§=='+'destinata.'+'||';
128700151112             strTRBM=strTRBM+'§§TagT02§§=='+
128800151112                     'precise sul orario di consegna,'+'||';
128900151111           endif;
129000151111
129100151111           // Se spedizione in fermo deposito
129200110617           if blpffd='S';
129300140423              wConsegna = 'La spedizione dovrà essere' +
129400140324              ' ritirata, esibendo documento di identità, presso la filiale' +
129500140324              ' di '+%trim(orgDES)+' '+
129600140324                     %trim(orgIND)+' '+
129700140324                     %trim(%editc(orgCPF:'X'))+' '+
129800140324                     %trim(orgLOC)+' '+
129900140324                 '('+%trim(orgPRO)+')'+
130000140327              ' nei seguenti orari  ' +
130100141016                %trim(wLnaOraMMD_A)+' - '+
130200141016                %trim(wLnaOraMMA_A);
130300141016              if wLnaOraMPD_A <> *blanks;
130400141016                 wConsegna=wConsegna+'  e  ' + %trim(wLnaOraMPD_A)+
130500141016                                       ' - ' + %trim(wLnaOraMPA_A);
130600140324              endif;
130700151112              strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
130800140324
130900140324              strTRBM=strTRBM+'§§Tag28§§=='+'Servizio Clienti   ' +
131000140324                              %trim(orgDD2)+'@brt.it' + '  -  '+
131100140324                              %trim(orgTEL)+'||';
131200140324
131300140327             strTRBM=strTRBM+'§§Tag29§§=='+'<BR><BR>Nel caso voglia delegare' +
131400140324               ' il ritiro il delegato dovrà esibire: il proprio documento' +
131500140324               ' di identità, il documento di identità del delegante (anche' +
131600140324               ' in fotocopia) e il modulo di delega presente nella sezione' +
131700140324               ' <a href="http://www.brt.it/it/area_download.do">download</a>'+
131800140327               ' del sito BRT.'+'||';
131900140324
132000140327             strTRBM=strTRBM+'§§Tag23§§=='+'<BR><BR>Sarà nostra cura inviarle' +
132100140327              ' un ulteriore e-mail non appena la merce sarà disponibile' +
132200140327              ' per il ritiro.'+'||';
132300110617           else;
132400120619              if wcliper='S'and wdcr >0 ;
132500140423               select;
132600120620               when blptcr=*blanks;
132700141016                 wConsegna = '<b>La consegna ' +
132800140704                          'é prevista il' ;
132900120620               when blptcr='D';
133000141016                 wConsegna = '<b>La consegna ' +
133100140704                          'é prevista dopo il' ;
133200120620               when blptcr='P';
133300141016                 wConsegna = '<b>La consegna ' +
133400140704                          'é prevista prima del' ;
133500120620               endsl;
133600140423
133700141016                 wConsegna=wConsegna+' '+%trim(%editw(wdcr:'0  /  /    '));
133800141016                 wConsegna=wConsegna+'</b> '+%trim(wSpedConsOrari);
133900151112                 strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
134000140423
134100120619                 strTRBM=strTRBM+'§§Tag23§§=='+''+'||';
134200140324                 strTRBM=strTRBM+'§§Tag28§§=='+''+'||';
134300140324                 strTRBM=strTRBM+'§§Tag29§§=='+''+'||';
134400120619              else;
134500140423                 wConsegna = '';
134600151112                 strTRBM=strTRBM+'§§Tag09§§=='+'<BR>'+wConsegna+'||';
134700120619                 strTRBM=strTRBM+'§§Tag23§§=='+''+'||';
134800140324                 strTRBM=strTRBM+'§§Tag28§§=='+''+'||';
134900140324                 strTRBM=strTRBM+'§§Tag29§§=='+''+'||';
135000120619              endif;
135100110617           endif;
135200110617           strTRBM=strTRBM+'§§Tag10§§=='+%editc(blplnp:'X')+'  ';
135300110617           strTRBM=strTRBM+%editc(blpnrs:'X')+'  ';
135400110617           strTRBM=strTRBM+%editc(blpnsp:'X')+'||';
135500110617           strTRBM=strTRBM+'§§Tag11§§=='+%trim(%editc(blprmn:'Z'))+'||';
135600110617           strTRBM=strTRBM+'§§Tag12§§=='+%trim(blprma)+'||';
135700110617           strTRBM=strTRBM+'§§Tag13§§=='+%trim(%editc(blpncl:'Z'))+'||';
135800110617           strTRBM=strTRBM+'§§Tag14§§=='+%trim(%editc(blppkb:'2'))+'||';
135900110617           if ar9cas>0;
136000110621              war9cas = ar9cas;
136100110621              strTRBM=strTRBM+'§§Tag15§§=='+%trim(%editc(war9cas:'2'))+
136200110617                                           '  '+ar9vca+'||';
136300110617           else;
136400141016              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
136500110617           endif;
136600110617           strTRBM=strTRBM+'§§Tag16§§=='+%trim(orgdes)+'||';
136700110617           strTRBM=strTRBM+'§§Tag17§§=='+%trim(orgind)+'||';
136800110617           strTRBM=strTRBM+'§§Tag18§§=='+%editc(orgcpf:'X')+'||';
136900110617           strTRBM=strTRBM+'§§Tag19§§=='+%trim(orgloc)+'||';
137000140324           strTRBM=strTRBM+'§§Tag20§§=='+%trim(orgpro)+'||';
137100110617           strTRBM=strTRBM+'§§Tag21§§=='+%trim(orgtel)+'||';
137200140327           strTRBM=strTRBM+'§§Tag22§§=='+%trim(orgDD2)+'@brt.it'+'||';
137300120618           strTRBM=strTRBM+'§§Tag24§§=='+%trim(blpind)+'||';
137400120622           strTRBM=strTRBM+'§§Tag25§§=='+%trim(blplod)+'  '+%trim(blpprd)+'||';
137500161115           if wAllega;
137600151112              strTRBM=strTRBM+'§§Tag26§§=='+'<BR>'+'In allegato può '+
137700151112                      'trovare i documenti predisposti dal mittente.'+'||';
137800140805           else;
137900140805              strTRBM=strTRBM+'§§Tag26§§=='+''+'||';
138000130408           endif;
138100131003           if bologgi='S';
138200131003              strTRBM=strTRBM+'§§Tag27§§=='+'Da domani potrà '+'||';
138300131003           else;
138400131003              strTRBM=strTRBM+'§§Tag27§§=='+'Potrà '+'||';
138500131003           endif;
138600141016           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
138700141128
138800141128           if wPINcode='S';
138900151112              strTRBM=strTRBM+'§§Tag42§§=='+'<BR>'+
139000141128               'ATTENZIONE: Come richiesto dal mittente la consegna potrà '   +
139100141128               'avvenire solo a seguito di inserimento del codice PIN. La '   +
139200141128               'invitiamo pertanto a predisporsi in quanto il ns. Incaricato '+
139300141128               'lo esigerà per poter effettuare la consegna.'
139400141128               +'||';
139500141128           else;
139600141128              strTRBM=strTRBM+'§§Tag42§§=='+''+'||';
139700141128           endif;
139800131003
139900110617        endsr;
140000170201
140100170201
140200170201
140300170201       //---------------------------------------------------------------*
140400170202       //Imposto campo contenente la stringa per driver TRBMBYPSR (NEW) *
140500170202       //---------------------------------------------------------------*
140600170201        Begsr sr_impvarbmNew;
140700170202
140800170202
140900170202          // Inizializzazione variabili di wrk
141000170206          clear wConsegnaN;
141100170206
141200170202          clear wRiga01;
141300170202          clear wRiga02;
141400170202          clear wRiga03;
141500170202          clear wRiga04;
141600170202          clear wRiga05;
141700170202          clear wRiga06;
141800170202          clear wRiga07;
141900170202          clear wRiga08;
142000170202
142100170711          clear wTagRSM;
142200170202          clear wTagV01;
142300170202          clear wTagV02_1;
142400170202          clear wTagV02_2;
142500170202          clear wTagV02_3;
142600170202          clear wTagV02_4;
142700170202          clear wTagV03_1;
142800170202          clear wTagV03_2;
142900170202          clear wTagV04;
143000170202          clear wTagV05;
143100170202          clear wTagV06;
143200170202          clear wTagV07_1;
143300170202          clear wTagV07_2;
143400170202          clear wTagV07_3;
143500170202          clear wTagV07_4;
143600170202          clear wTagV07_5;
143700170202
143800170201
143900170202          // Innanzitutto determino il tipo di alert
144000170202          clear wTypAlert;
144100170202          select;
144200170202            when blpTC1='A' or blpTC2='A';
144300170202               wTypAlert = 'A';            // APPUNTAMENTO
144400170202            when blpffd='S';
144500170202               wTypAlert = 'F';            // FERMO DEPOSITO
144600170202            when wcliper='S'and wdcr >0 ;
144700170202               wTypAlert = 'S';            // STANDARD
144800170202            other;
144900170202               wTypAlert = 'O';            // ALTRO
145000170202          endsl;
145100170202
145200170202
145300170202          // In base alla tipologia di alert personalizzo le righe di testo
145400170202          clear wConsegna;
145500170202          select;
145600170202            when blpTCR = *blanks;
145700170202              wConsegna = 'il';
145800170202            when blpTCR = 'D';
145900170202              wConsegna = 'dopo il';
146000170202            when blpTCR = 'P';
146100170202              wConsegna = 'prima del';
146200170202          endsl;
146300170202
146400170202
146500170202          select;
146600170202            // APPUNTAMENTO
146700170202            when wTypAlert = 'A';
146800170202
146900170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
147000170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' in data ' +
147100170711                %editw(wdataspe:'0  /  /    ') + ' ci ha affidato una tua ' +
147200170711                'spedizione che prevede la CONSEGNA SU APPUNTAMENTO.';
147300170711              wRiga06 = 'Programma subito la consegna cliccando ' +
147400170202                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
147500170711                'brtCode='+wBRTcode+'&urltype=a">qui</a></b>' +
147600170711                ', in caso contrario verrai contattato ' +
147700170203                'per concordare la modalità più gradita.';
147800170206
147900170206              wConsegnaN = %trim(wRiga03);
148000170206
148100170202
148200170202            // FERMO DEPOSITO
148300170202            when wTypAlert = 'F';
148400170202
148500170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
148600170202              wRiga03 = %trim(wRSM_Eml) + ' ci ha affidato in data ' +
148700170202                %editw(wdataspe:'0  /  /    ') + ' una tua spedizione ' +
148800170202                'che dovrà essere ritirata, esibendo documento di identità, ' +
148900170202                'presso la filiale di ' +
149000170202                       %trim(orgDES)+' '+
149100170202                       %trim(orgIND)+' '+
149200170202                       %trim(%editc(orgCPF:'X'))+' '+
149300170202                       %trim(orgLOC)+' '+
149400170202                   '('+%trim(orgPRO)+')'+
149500170202                   ' nei seguenti orari  ' +
149600170202                   %trim(wLnaOraMMD_A)+' - '+
149700170202                   %trim(wLnaOraMMA_A);
149800170202                if wLnaOraMPD_A <> *blanks;
149900170202                   wRiga03 = %trim(wRiga03)+'  e  ' + %trim(wLnaOraMPD_A)+
150000170202                                              ' - ' + %trim(wLnaOraMPA_A);
150100170202                endif;
150200170202              wRiga04 = 'Servizio Clienti   ' +
150300170202                %trim(orgDD2)+'@brt.it' + '  -  ' + %trim(orgTEL);
150400170202              wRiga05 = '<b>Sarà nostra cura inviarti un ulteriore email ' +
150500170202                'non appena la merce sarà disponibile per il ritiro.</b>' +
150600170202                '<BR><BR>';
150700170202              wRiga06 = 'Se vuoi delegare il ritiro, il delegato dovrà ' +
150800170202                'esibire: il proprio documento di identità, il documento ' +
150900170202                'di identità del delegante (anche in fotocopia) e il modulo ' +
151000170203                'di delega presente nella sezione ' +
151100170202                '<a href="http://www.brt.it/it/area_download.do">download</a>'+
151200170203                ' del sito BRT.<BR><BR><BR>' +
151300170202                '<a href="http://vas.brt.it/vas/sped_det_show.htm?' +
151400170203                'brtCode='+wBRTcode+'&urltype=a">La tua spedizione</a>';
151500170206
151600170206              wConsegnaN = %trim(wRiga03) + ' ' + %trim(wRiga05);
151700170206
151800170202
151900170202            // STANDARD
152000170202            when wTypAlert = 'S';
152100170202
152200170203              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR>';
152300170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' ci ha affidato una tua ' +
152400170206                'spedizione che <b>prevediamo di consegnare ' + wConsegna +
152500170202                ' ' + %trim(%editw(wdcr:'0  /  /    ')) + '</b>' +
152600170202                '<BR><BR>';
152700170711            //  wRiga04 = 'Generalmente nella tua zona i servizi vengono ' +
152800170711            //    'effettuati <u>' + %trim(wSpedConsOrari) + '</u>';
152900170711            //  wRiga05 = '<b>Informazioni più precise sull''orario di ' +
153000170711            //    'consegna saranno pubblicate il giorno della prevista ' +
153100170711            //    'consegna.</b>' +
153200170711            //    '<BR><BR><BR>';
153300170711            //  wRiga06 =
153400170711            //    'Se vuoi riprogrammare la tua consegna clicca ' +
153500170711            //    '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
153600170711            //    'brtCode='+wBRTcode+'&urltype=a">qui</a></b>.';
153700170711              wRiga04 = 'Generalmente nella tua zona i servizi vengono ' +
153800170711                'effettuati ' + %trim(wSpedConsOrari) + ', informazioni ' +
153900170711                'più precise saranno pubblicate il giorno della prevista ' +
154000170711                'consegna.';
154100170711              wRiga06 =
154200170711                'Per visualizzare o riprogrammare la tua consegna clicca ' +
154300170711                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
154400170711                'brtCode='+wBRTcode+'&urltype=a">qui</a></b>.';
154500170215
154600170206
154700170206              wConsegnaN = %trim(wRiga03) + ' ' + %trim(wRiga04);
154800170203
154900170202
155000170202            // ALTRO
155100170202            when wTypAlert = 'O';
155200170202
155300170202              wRiga01 = 'Gentile <i>' + %trim(blpRSD) + '</i>,<BR><BR>';
155400170719              wRiga03 = '   ' + %trim(wRSM_Eml) + ' in data ' +
155500170202                %editw(wdataspe:'0  /  /    ') + ' ci ha affidato una tua ' +
155600170203                'spedizione.';
155700170203              wRiga06 = '<b>Per informazioni su precise verifica la ' +
155800170202                '<b><a href="http://vas.brt.it/vas/sped_det_show.htm?' +
155900170203                'brtCode='+wBRTcode+'&urltype=a">tua consegna</a></b>.';
156000170206
156100170206              wConsegnaN = %trim(wRiga03);
156200170206
156300170202
156400170202          endsl;
156500170202
156600170202
156700170202
156800170202          // Gestione particolarità specifiche
156900170202          if wPINcode='S';
157000170203             wRiga07 = '<BR><b><u>' +
157100170202              'ATTENZIONE: Come richiesto dal mittente la consegna potrà '   +
157200170202              'avvenire solo a seguito di inserimento del codice PIN. Ti '   +
157300170202              'invitiamo pertanto a predisporti in quanto il ns. Incaricato '+
157400170203              'lo esigerà per poter effettuare la consegna.</u></b><BR><BR>';
157500170206
157600170206              wConsegnaN = %trim(wConsegnaN) + ' ' + %trim(wRiga07);
157700170202          endif;
157800170203
157900170203          if wAllega;
158000170203             wRiga08 = '<BR><b><u>In allegato puoi trovare i documenti ' +
158100170203               'predisposti dal mittente.</u></b><BR><BR></b>';
158200170203          endif;
158300170202
158400170206
158500170206
158600170206          // Valorizzo indicazioni per Inserimento RA
158700170206          wConsegna = %trim(wConsegnaN);
158800170206
158900170202
159000170202
159100170202          // Valorizzo le variabili "standard"
159200170711          wTagRSM   = %trim(wRSM_Eml);
159300170202          wTagV01   = %trim(wBRTcodeLong);
159400170202          wTagV02_1 = %trim(blpRSD);
159500170202          wTagV02_2 = %trim(blpIND);
159600170202          wTagV02_3 = %trim(blpLOD);
159700170202          wTagV02_4 = %trim(blpPRD);
159800170202          wTagV03_1 = %trim(%editc(blpRMN:'Z'));
159900170202          wTagV03_2 = %trim(blpRMA);
160000170202          wTagV04   = %trim(%editc(blpNCL:'Z'));
160100170202          wTagV05   = %trim(%editc(blpPKB:'2'));
160200170202          if ar9cas > *zeros;
160300170202             war9cas = ar9cas;
160400170202             wTagV06 = %trim(%editc(war9cas:'2')) + '  ' + ar9vca;
160500170202          else;
160600170202             wTagV06 = '0,00';
160700170202          endif;
160800170202          wTagV07_1 = %trim(orgDES);
160900170202          wTagV07_2 = %trim(orgIND);
161000170202          wTagV07_3 = %trim(%editc(orgCPF:'X'));
161100170202          wTagV07_4 = %trim(orgLOC);
161200170202          wTagV07_5 = %trim(orgPRO);
161300170202
161400170202
161500170202
161600170202          // Valorizzozione stringa per driver di "by-pass spool"
161700170202          strTRBM=        '§§TagR01§§=='+%trim(wRiga01)+'||';
161800170202          strTRBM=strTRBM+'§§TagR02§§=='+%trim(wRiga02)+'||';
161900170202          strTRBM=strTRBM+'§§TagR03§§=='+%trim(wRiga03)+'||';
162000170202          strTRBM=strTRBM+'§§TagR04§§=='+%trim(wRiga04)+'||';
162100170202          strTRBM=strTRBM+'§§TagR05§§=='+%trim(wRiga05)+'||';
162200170202          strTRBM=strTRBM+'§§TagR06§§=='+%trim(wRiga06)+'||';
162300170202          strTRBM=strTRBM+'§§TagR07§§=='+%trim(wRiga07)+'||';
162400170202          strTRBM=strTRBM+'§§TagR08§§=='+%trim(wRiga08)+'||';
162500170202
162600170711          strTRBM=strTRBM+'§§TagRSM§§=='+%trim(wTagRSM)+'||';
162700170202          strTRBM=strTRBM+'§§TagV01§§=='  +%trim(wTagV01)  +'||';
162800170202          strTRBM=strTRBM+'§§TagV02_1§§=='+%trim(wTagV02_1)+'||';
162900170202          strTRBM=strTRBM+'§§TagV02_2§§=='+%trim(wTagV02_2)+'||';
163000170202          strTRBM=strTRBM+'§§TagV02_3§§=='+%trim(wTagV02_3)+'||';
163100170202          strTRBM=strTRBM+'§§TagV02_4§§=='+%trim(wTagV02_4)+'||';
163200170202          strTRBM=strTRBM+'§§TagV03_1§§=='+%trim(wTagV03_1)+'||';
163300170202          strTRBM=strTRBM+'§§TagV03_2§§=='+%trim(wTagV03_2)+'||';
163400170202          strTRBM=strTRBM+'§§TagV04§§=='  +%trim(wTagV04)  +'||';
163500170202          strTRBM=strTRBM+'§§TagV05§§=='  +%trim(wTagV05)  +'||';
163600170202          strTRBM=strTRBM+'§§TagV06§§=='  +%trim(wTagV06)  +'||';
163700170202          strTRBM=strTRBM+'§§TagV07_1§§=='+%trim(wTagV07_1)+'||';
163800170202          strTRBM=strTRBM+'§§TagV07_2§§=='+%trim(wTagV07_2)+'||';
163900170202          strTRBM=strTRBM+'§§TagV07_3§§=='+%trim(wTagV07_3)+'||';
164000170202          strTRBM=strTRBM+'§§TagV07_4§§=='+%trim(wTagV07_4)+'||';
164100170202          strTRBM=strTRBM+'§§TagV07_5§§=='+%trim(wTagV07_5)+'||';
164200170201
164300170201        endsr;
164400161110
164500161110
164600161110
164700161110       //-------------------------------------------------------------*
164800161110       //Aggiunta allegati                                            *
164900161110       //-------------------------------------------------------------*
165000161110        Begsr sr_impAtchBm;
165100161110
165200161115          if wPDF_NoZip;
165300161110             strTRBM=strTRBM+strTRBMATCH;
165400161111          endif;
165500161110
165600161110        endsr;
165700130406
165800140806
165900130406
166000120619       //-------------------------------------------------------------*
166100120619       //Verifica eventuali personalizzazioni per il cliente          *
166200120619       //-------------------------------------------------------------*
166300120619        Begsr sr_cliper;
166400120619
166500120619        clear wcliper;
166600120619        clear wdcr;
166700140327        clear wdcrRev;
166800141128        clear wPINcode;
166900120619        clear dcli;
167000140327        clear tnsd99ds;
167100141128
167200141128        // Verifica se spedizione con particolarità consegna "PINcode"
167300141128        if blpGMA <> *blanks;
167400141128           if %lookup(blpGMA:sk7R_PINcode)>0;
167500141128              wPINcode = 'S';
167600141128            endif;
167700141128        endif;
167800140327
167900120619        if blpccm>0  ;
168000120619           wke1=%editc(blpccm:'X');
168100120619        else          ;
168200120619           wke1=%editc(blpksc:'X');
168300120619        endif;
168400130406        //  if §cliemd='S';
168500120619              // Se presente il contrassegno verifica se richiesto il contatto
168600120619              if ar9cas>0;
168700120619                 clear trul21ds;
168800120619                 i21tla='L';
168900120619                 i21cbo = blpcbo;
169000120619                 i21tsp = blptsp;
169100120619                 i21lnp = blplnp;
169200120619                 i21nzm = blpnzm;
169300120619                 i21lna = blplna;
169400120619                 i21nzd = blpnzd;
169500120619                 i21tic = ar9tic;
169600120619                 i21imp = ar9cas;
169700120619                 i21div = ar9vca;
169800120619                 i21ute = sdsusr;
169900120619                 i21pgm = nompgm;
170000130829              //in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
170100130829                 if %lookup(blpcbo:cbf)>0;
170200130829                    i21ksc = blpksc;
170300130829                 else;
170400130829                    i21ksc = blpccm;
170500130829                    if i21ksc=0;
170600130829                       i21ksc=(blplnp*10000) + 8888;
170700130829                    endif;
170800130829                 endif;
170900120619                 trul21r(trul21ds);
171000120619              endif;
171100120619              // La personalizzazione della mail con l'indicazione della
171200120619              // consegna prevista viene fatta solo se la spedizione
171300120619              // non prevede contatti col destinatario:
171400120619                 // No Fermo deposito
171500120619              if blpffd=*blanks and (
171600120619                 // No appuntamento o supermercato OPPURE indicata DCR tassativa
171700120619              (blptc1 <>'A' and blptc1<>'S' and blptc2 <>'A' and blptc2<>'S') or
171800120619              (blpdcr>0 and blptcr=*blanks))
171900120619                 // No contrassegno oppure importo che non richiede
172000120619                 // l'abilitazione "manuale"
172100120619              and (ar9cas=0 or o21fca=*blanks);
172200120619                 wcliper='S';
172300120619              endif;
172400130406        //   endif;
172500120619        // Se personalizzazione da fare e non è indicata la data consegna
172600120619        // richiesta essa viene calcolata mediante pgm di calcolo delivery
172700120621        if wcliper='S' ;
172800160502          //        if blpdcr=0;
172900120621              d98tbo='P';
173000120621              d98aas=blpaas;
173100120621              d98lnp=blplnp;
173200120621              d98nrs=blpnrs;
173300120621              d98nsp=blpnsp;
173400130529              if blptsp='D';
173500130529                 I98TSP_FOR='C';
173600130529              endif;
173700120621              tnsd99r(tnsd99ds);
173800120621              if d98err='0';
173900151111                 wdcr=d98dee;
174000160502              else  ;
174100160502                if blpdcr>0  ;
174200160502                   wdcr=blpdcr;
174300160502                endif;
174400120621              endif;
174500160502             //        else;
174600160502             //           wdcr=blpdcr;
174700160502             //        endif;
174800160502
174900140327           wdcrRev = wdcr;
175000120621           if wdcr>0;
175100120621              dataeur=%date(wdcr:*iso);
175200120621              wdcr=%dec(dataeur);
175300120621           endif;
175400160502        endif;
175500150930
175600140324        clear trulORsds;
175700140324        clear trulOR2ds;
175800150730        clear diore01;
175900140324
176000140324        wSpedConsOrari = '.';
176100140324        reset wSpedConsOraDa;
176200140324        reset wSpedConsOraA;
176300140324
176400140327        IOREDTA  = wdcrRev;
176500140324        IOREFIL  = blpLNA;
176600140324        IORECAP  = blpCAD;
176700140324        IORELOC  = blpLOD;
176800140324        IORENAR  = blpNZD;
176900140324        IORETSER = 'C';
177000140324        IOREDDC  = blpDDC;
177100140324        IORENDC  = blpNDC;
177200140324        IORETFP  = blpTFP;
177300140324        IORETFA  = blpTFA;
177400140324        IOREDTI  = blpDTI;
177500140324        IOREHTI  = blpHTI;
177600140324        IOREDCR  = blpDCR;
177700140324        IOREHCR  = blpHCR;
177800140324        IORETCR  = blpTCR;
177900140324        IOREDEI  = d98DEI;
178000140324        IOREOEI  = d98OEI;
178100140324        IORETSP  = blpTSP;
178200150730        §IORETRAZC = %editc(D98TTC:'X');
178300150730        §IORECONSC = %editc(D98TCC:'X');
178400150730        IOREFLO  = diore01;
178500140324
178600140324        callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
178700140324        if not %error AND OOREERR = *blanks;
178800140324           wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
178900140324           wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
179000141013         //  wSpedConsOrari = '<b>indicativamente</b>' +
179100141013           wSpedConsOrari = 'indicativamente' +
179200141013                            ' dalle ' + wSpedConsOraDa +
179300141013                            ' alle '   + wSpedConsOraA;
179400140324        endif;
179500120619
179600120619        endsr;
179700161115
179800161115
179900161115
180000161115       //-------------------------------------------------------------*
180100161115       //Verifica eventuali personalizzazioni per l'attach            *
180200161115       //-------------------------------------------------------------*
180300161115        Begsr sr_PDFper;
180400161115
180500161115        clear wPDF_NoZip;
180600161115
180700161115        if blpccm>0;
180800161115           wke1=%editc(blpccm:'X');
180900161115        else;
181000161115           wke1=%editc(blpksc:'X');
181100161115        endif;
181200161115
181300161115        chain ('CLI':wke1) tntbe01l;
181400161115        if %found(tntbe01l);
181500161115           dcli=tbeuni;
181600161115           if §CLINOZIP = 'N';
181700161115              wPDF_NoZip = *on;
181800161115           endif;
181900161115        endif;
182000161115
182100161115        endsr;
182200140414
182300140806
182400140414
182500140414       //-------------------------------------------------------------*
182600140414       //inserimento Richiesta Assistenza                             *
182700140414       //-------------------------------------------------------------*
182800141016        Begsr sr_InsRA;
182900140414
183000141016         if chiudi = *blanks;
183100141016
183200140414           clear fidna6ds;
183300140414
183400140423           iDA06MAD = ' 82';
183500140609
183600170206           wConsegna = %scanrpl('<BR>' : '' : wConsegna);
183700170206           wConsegna = %scanrpl('<b>'  : '' : wConsegna);
183800170206           wConsegna = %scanrpl('</b>' : '' : wConsegna);
183900170206           wConsegna = %scanrpl('<i>'  : '' : wConsegna);
184000170206           wConsegna = %scanrpl('</i>' : '' : wConsegna);
184100170206           wConsegna = %scanrpl('<u>'  : '' : wConsegna);
184200170206           wConsegna = %scanrpl('</u>' : '' : wConsegna);
184300140609
184400140908           iDA06RSC = 'EMAIL';
184500160114
184600160114           // Verifico il tipo di email
184700160114           if wTipoEml = '*ATCH';
184800160114              iDA06NO1 = 'Email Affidamento + PDF a: '+%trim(§ar5eml)+
184900160114                         '  -  ' + wConsegna;
185000160114           else;
185100160114              iDA06NO1 = 'Email Affidamento a: '+%trim(§ar5eml)+
185200160114                         '  -  ' + wConsegna;
185300160114           endif;
185400160114
185500140414           iDA06TOR = 'S';
185600140520           iDA06OGG = %trim(%editc(blpLNP:'X')) +
185700140520                      %trim(%editc(blpNRS:'X')) +
185800140520                      %trim(%editc(blpNSP:'X')) +
185900140520                      %trim(%editc(blpAAS:'X'));
186000140414           iDA06UTE = 'QPGMR';
186100140520           iDA06FIL = blpLNP;
186200140414
186300140414           callP(e) FIDNA6R (fidna6ds);
186400160506
186500160506           // Inserimento spia per Predict DPD
186600160506           if  %lookup(blpLNP:skFilExtDPD) > 0;
186700160506               exsr insPredict;
186800160506           endif;
186900141016
187000141016         endif;
187100140414
187200140414        endsr;
187300170201
187400170201
187500170201
187600170201       //-------------------------------------------------------------*
187700170201       //caricamento filiali abilitate al deploy                      *
187800170201       //-------------------------------------------------------------*
187900170201       Begsr sr_VPOR;
188000170201
188100170201          clear wVPOdeploy;
188200170201
188300170201          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
188400170201          clear TRULVPODS;
188500170201          IVPOke1 = 'DECOFIIDC';
188600170201          TRULVPOR (trulvpods);
188700170201          skFil_New = skFilvpo;
188800170201
188900170201          select;
189000170201            // se al primo elemento c'è 999 => tutto a modo nuovo
189100170201            when skFil_New(1) = '999';
189200170201                 wVPOdeploy = 'NUOVO';
189300170201
189400170201            // se al primo elemento c'è *blanks => tutto a modo vecchio
189500170201            when skFil_New(1) = *zeros;
189600170201                 wVPOdeploy = 'VECCHIO';
189700170201
189800170201            other;
189900170201                 wVPOdeploy = 'MISTO';
190000170201          endsl;
190100170201
190200170201       endsr;
190300170201
190400170201
190500170201
190600170201       //-------------------------------------------------------------*
190700170201       //Effettuo controlli pre invio 2                               *
190800170201       //-------------------------------------------------------------*
190900170201       Begsr sr_ChkDeploy;
191000170201
191100170201          *in60 = *off;
191200170201
191300170201          select;
191400170201            when wVPOdeploy = 'VECCHIO';
191500170201              *in60  = *off;
191600170201
191700170201            when wVPOdeploy = 'NUOVO';
191800170201              *in60  = *on;
191900170201
192000170201            when wVPOdeploy = 'MISTO';
192100170201              if %lookup(%editc(blpLNA:'X'):skFil_New) > 0;
192200170201                *in60 = *on;
192300170201              endif;
192400170201          endsl;
192500170201
192600170201       endsr;
192700170201
192800140806
192900131210
193000131210       //-------------------------------------------------------------*
193100131210       //Invio email di alert                                         *
193200131210       //-------------------------------------------------------------*
193300131210       Begsr snd_Alert;
193400131210
193500131210          wEml   = 'fabrizio.gurrieri@brt.it';
193600131210          wCcEml = *blanks;
193700131210          wOgg   = 'Anomalia invio avviso spedizione a destinatario ' +
193800131210                   w§CM1PROG;
193900131210          wMsg   = ' w§CM1PROG     '     + w§CM1PROG           + ':/N' +
194000131210                   ' ar5aas        '     + %editc(ar5aas:'X')  + ':/N' +
194100131210                   ' ar5lnp        '     + %editc(ar5lnp:'X')  + ':/N' +
194200131210                   ' ar5nrs        '     + %editc(ar5nrs:'X')  + ':/N' +
194300131210                   ' ar5nsp        '     + %editc(ar5nsp:'X')  + ':/N' +
194400131210                   ' §AR5ALM       '     + §AR5ALM             + ':/N' +
194500131210                   ' blpft1        '     + blpft1              + ':/N' +
194600131210                   ' §AR8PEML      '     + §AR8PEML            + ':/N' +
194700131210                   ' mEsito        '     + mEsito              + ':/N' +
194800131210                   ' wTipoEml      '     + wTipoEml            + ':/N' +
194900131210                   ' wAllega       '     + wAllega             + ':/N' +
195000131210                   ' wEmlSnt       '     + %editc(wEmlSnt:'X') + ':/N' +
195100131210                   ' w§CM1VAR      '     + w§CM1VAR            + ':/N' +
195200131210                   ' pPathOrig     '     + %trim(pPathOrig)    + ':/N' +
195300131210                   ' pPathZipAS    '     + %trim(pPathZipAS)   + ':/N' +
195400131210                   ' pPathZipSRV   '     + %trim(pPathZipSRV)  + ':/N' +
195500131210                   ' pShare        '     + %trim(pShare)       + ':/N' +
195600131210                   ' (EMD) ar5UNI  '     + %trimr(ar5UNI)      + ':/N' +
195700131210                   ' GEN_ar5UNI    '     + %trimr(GEN_ar5UNI)  + ':/N' +
195800131210                   ' (§PDF) ar8UNI '     + %trimr(ar8UNI)      + ':/N' +
195900131210                   ' pPathCli      '     + %trim(pPathCli)     + ':/N' +
196000131210                   ' pNomePDF      '     + %trim(pNomePDF)     + ':/N' +
196100161118                   ' pFileName     '     + %trim(pFileName)    + ':/N';
196200131210
196300131210          callP(e) TIS701C1 (wEml : wCcEml : wOgg : wMsg : wEsito);
196400131210
196500131210        endsr;
196600131210
