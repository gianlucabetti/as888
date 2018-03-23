000100091228       //==============================================================
000200100302       //?STORE PROCEDURE - Trasmissione anagrafica cliente            ?
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600091228       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091228     /*PRM commit(*none)
001100091223     /*END
001200091228
001300091228       //--------------------------------------------------------------
001400091228       //?Specifiche di controllo.                                     ?
001500091228       //--------------------------------------------------------------
001600091223
001700091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800100325     h dftactgrp(*no)
001900091223     h alwnull(*inputonly)
002000100325     h bnddir('UBBNDDIR')
002100091223
002200091223       //--------------------------------------------------------------
002300091223       //?Dichiarazione file.                                          ?
002400091223       //--------------------------------------------------------------
002500100112
002600100302       // -?Dati anagrafici clienti?
002700100302     fCNACO00F  if   e           k disk
002800100302     fCNIND00F  if   e           k disk
002900110516
003000110516       // -?File di stampa per eventuale invio e-mail?
003100110516     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003200091223
003300091223       //--------------------------------------------------------------
003400091223       //?Definizione costanti.                                        ?
003500091223       //--------------------------------------------------------------
003600100302
003700100302       // -?Codice utente conti?
003800100302     d c_Kut           c                   const(1)
003900100303
004000100303       // -?Default per codice capoconto in anagrafica clienti?
004100100303     d c_Kci           c                   const(0151)
004200110516
004300110516       // -?Limite record estraibili per l'invio di un *msg di?
004400110516       //  ?avvertimento al CED?
004500170925     d c_MaxRec        c                   const(27000)
004600110516
004700110516       // -?Comandi di override al PrtF?
004800110516     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004900110516     d                                           file(PRTEMAIL) +
005000110516     d                                           pagesize(66 132) +
005100110516     d                                           lpi(6) cpi(10) +
005200110516     d                                           ovrscope(*actgrpdfn) +
005300110516     d                                           ')
005400110516     d c_CmdDltOvr     c                   const('DLTOVR +
005500110516     d                                            file(PRTEMAIL) +
005600110516     d                                            lvl(*actgrpdfn)')
005700091223
005800091223       //--------------------------------------------------------------
005900091223       //?Definizione schiere.                                         ?
006000091223       //--------------------------------------------------------------
006100091223
006200091228       // -?Dati da restituire (per ogni cliente)?
006300100121     d sch_pOutDati    ds                  likeds(pOutDati_ds)
006400150609     d                                     inz  dim(27000)
006500091223
006600091223       //--------------------------------------------------------------
006700091223       //?Definizione aree dati.                                       ?
006800091223       //--------------------------------------------------------------
006900091223
007000091223
007100091223       //--------------------------------------------------------------
007200091223       //?Definizione strutture dati.                                  ?
007300091223       //--------------------------------------------------------------
007400110523
007500110523       // -?Comodo per chiamati?
007600110523     d kpjba         e ds                  inz
007700100107
007800100305       // -?Comodo per la definizione di campi?
007900121116     d TNTBEds       e ds                  extname(TNTBE00F)
008000121116     d                                     based(nullPtr)
008100121116     d TABELds       e ds                  extname(TABEL00F)
008200121116     d                                     based(nullPtr)
008300121116     d                                     qualified
008400100305
008500091223       // -?Tabelle usate:?
008600100302       // ·?Tab. "3EW" = Dati assegnati alla postazione EasyWEB?
008700100323     d d3EW          e ds                  inz  qualified
008800121116       // ·?Tab. "3C"  = Clienti che inviano bolle partenza?
008900121116     d ds3C          e ds                  inz  qualified
009000110523
009100110523       // -?Tab. "MRA" = Bart-Mailing x FISPE04R?
009200110523     d dMRAdan       e ds                  inz
009300110523     d   §MRAddes    e                     inz('ERRORE aggiornam. +
009400110523     d                                          tab.3EW')
009500110523     d   §MRAdreg    e                     inz('COR')
009600110523     d   §MRAdmitt   e                     inz('ced')
009700171006     d   §MRAddest   e                     inz('cedalert@brt.it')
009800110523     d   §MRAdidpro  e                     inz('2')
009900110523     d   §MRAdoutqi  e                     inz('EMAILIN')
010000110516
010100110516       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
010200110516     d TRTCM1ds      e ds                  inz
010300110516       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010400110516     d   §CM1mitt    e                     inz('ced')
010500110516       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010600120302     d   §CM1dst     e                     inz('ced@brt.it')
010700110516       //  ?·§CM1tips = Tipo lettera via e-mail:?
010800110516       //   ?"LET" = testo allegato in corpo con logo?
010900110516       //           ?(richiede righe libere iniziali per il logo)?
011000110516       //   ?"COR" = testo integrato senza logo?
011100110516       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011200110516     d   §CM1tips    e                     inz('COR')
011300110516       //  ?·§CM1po   = Filiale?
011400110516     d   §CM1po      e                     inz('046')
011500110516       //  ?·§CM1var  = Oggetto e-mail?
011600110516     d   §CM1var     e                     inz('*OBJM*+
011700110516     d                                     Nr MAX clienti estraibili -
011800110516     d                                     dalla s.p. di trasm. anagr-
011900110516     d                                     af. mittenti')
012000110516       //  ?·§CM1sts  = Stato?
012100110516     d   §CM1sts     e                     inz(*off)
012200110516       //  ?·§CM1sts  = Id processo?
012300110516     d   §CM1idp     e                     inz('2')
012400091223
012500091223       // -?Dati estratti via SQL?
012600100325     d wSQL_dati       ds                  inz  qualified
012700110524     d   wSQLke2                           inz  like(TBEke2)
012800110524     d   wSQLke1                           inz  like(TBEke1)
012900110524     d   wSQLkey                           inz  like(TABELds.TBLkey)
013000110524     d   wSQLuni                           inz  like(TBEuni)
013100121116     d   wSQLuni_3C                        inz  like(TABELds.TBLuni)
013200100402     d   wSQLaziatb                   1a   inz
013300110524     d   wSQLksuatb                        inz  like(TBEatb)
013400110524     d   wSQLccmatb                        inz  like(TABELds.TBLflg)
013500100121
013600100121       // -?Dati da restituire (per ogni cliente)?
013700100121     d pOutDati_ds...
013800100121     d                 ds                  inz
013900110524     d   §3EWazi                           inz  like(TBEke1)
014000110524     d   §3EWksu                           inz  like(TBEke2)
014100110524     d   §3EWccm                           inz  like(TABELds.TBLkey)
014200100402     d   §3EWlnp                           inz  like(d3EW.§3EWlnp)
014300100402     d   §3EWfls                           inz  like(d3EW.§3EWfls)
014400100402     d   §3EWnrs                           inz  like(d3EW.§3EWnrs)
014500100402     d   §3EWnsi                           inz  like(d3EW.§3EWnsi)
014600100402     d   §3EWnsf                           inz  like(d3EW.§3EWnsf)
014700100402     d   §3EWctm                           inz  like(d3EW.§3EWctm)
014800100402     d   §3EWmad                           inz  like(d3EW.§3EWmad)
014900100402     d   §3EWaew                           inz  like(d3EW.§3EWaew)
015000100402     d   §3EWaziatb                   1a   inz
015100110524     d   §3EWksuatb                        inz  like(TBEatb)
015200110524     d   §3EWccmatb                        inz  like(TABELds.TBLflg)
015300100402     d   §3EWrag                           inz  like(ACOrag)
015400100402     d   §3EWprv                           inz  like(INDprv)
015500100402     d   §3EWsta                           inz  like(INDsta)
015600100402     d   §3EWcae                           inz  like(INDcae)
015700100402     d   §3EWcit                           inz  like(INDcit)
015800100402     d   §3EWvia                           inz  like(INDvia)
015900110516
016000110516       // -?Status?
016100110516     d Psds           sds
016200110516     d   SDSpgm          *proc
016300110516     d***JobName             244    253                                         Job name
016400110523     d***JobUser             254    263                                         User name
016500110516     d***JobNumber           264    269s 0                                      Job number
016600091223
016700091223       //--------------------------------------------------------------
016800091223       //?Definizione variabili.                                       ?
016900091223       //--------------------------------------------------------------
017000091223
017100100323       // -?Parametri ricevuti:?
017200100323       //   ·?Tipo Ambiente EasyWeb: "EW" = Ambiente Production,?
017300100323       //                           ?"ET" = Ambiente Test.?
017400100325     d pIn_TypENV      s              2a
017500100323       //   ·?Tipo Lancio: *on  = Solo modificati,?
017600100323       //                 ?*off = Tutti.?
017700100325     d pIn_TypEXE      s              1a
017800100303
017900100303       // -?Parametri ricevuti e da restituire al chiamante?
018000100303     d RtnEsito        s             10i 0
018100100303     d RtnOpCode       s             10
018200100303     d RtnStatus       s             10i 0
018300100303
018400091223       // -?Flags booleani?
018500091223     d $EoF            s               n   inz
018600091228
018700091228       // -?Indici di schiera?
018800091228     d xx              s              5  0 inz
018900091223
019000100112       // -?Campi di comodo?
019100100112     d wDate           s              8  0 inz
019200110516
019300110516       // -?Campi di stampa?
019400110516     d O_testo         s            132    inz
019500091223
019600091223       // -?Stringa SQL da eseguire?
019700100325     d wSQL            s           2048    inz  varying
019800091223
019900091223       //--------------------------------------------------------------
020000091223       //?Definizione prototipi procedure.                             ?
020100091223       //--------------------------------------------------------------
020200100325
020300100325       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
020400110523     d wRtnEsito       s             10i 0 inz
020500100325      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
020600110516
020700110516       // -?Parametri API QCAPCMD (Process Commands)?
020800110516     d Qcmd            s           2048    inz  varying
020900110516      /copy qSysInc/qRpgleSrc,QCAPCMD
021000110516       // -?API QCAPCMD (Process Commands)?
021100110516      /copy gaitrasrc/srcProtoPR,QCAPCMD
021200110516
021300110516       // -?Parametri gestione errori API.?
021400110516      /copy qsysinc/qrpglesrc,QUSEC
021500091223
021600091223       //--------------------------------------------------------------
021700091223       //?Definizione key-list.                                        ?
021800091223       //--------------------------------------------------------------
021900091223
022000100302       // -?File CNACO00F e CNIND00F?
022100100302     d k03cnaco00    e ds                  extname(CNACO00F : *key)
022200100302     d                                     prefix(k_)   inz
022300091223
022400091223       //--------------------------------------------------------------
022500091223       //?Definizione parametri procedura.                             ?
022600091223       //--------------------------------------------------------------
022700091223
022800091223     c     *Entry        plist
022900100325     c                   parm                    pIn_TypENV
023000100325     c                   parm                    pIn_TypEXE
023100091223     c                   parm                    RtnEsito
023200091223     c                   parm                    RtnOpCode
023300091223     c                   parm                    RtnStatus
023400091223
023500091223      /free
023600091223
023700091223       //--------------------------------------------------------------
023800091223       //?M A I N - L I N E                                            ?
023900091223       //--------------------------------------------------------------
024000091223
024100091223       // -?Operazioni iniziali?
024200091223       exsr sr_RoutInz;
024300091223
024400091223       // -?Ciclo di reperimento clienti in tab. "3C"?
024500100112       DoW  Not $EoF;
024600091223         exsr  sr_ReadCursor;
024700091223       EndDo;
024800091223
024900091223       // -?Operazioni finali?
025000091223       exsr sr_RoutEnd;
025100091223
025200091223       //--------------------------------------------------------------
025300091223       //?Operazioni iniziali.                                         ?
025400091223       //--------------------------------------------------------------
025500091223       BEGSR  sr_RoutInz;
025600091223
025700091223         exec sql  set option  dynusrprf = *owner,
025800100325                               closqlcsr = *endmod;
025900091223
026000091223         *inLR = *on;
026100100112
026200100303         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
026300100303         wDate = %dec( %date() );
026400091223
026500091228         // -?Pulizia dati di Output?
026600091223         clear  RtnEsito;
026700091223         clear  RtnOpCode;
026800091223         clear  RtnStatus;
026900110516         //clear  sch_pOutDati;       [già pulita]?
027000110523
027100110524         // -?Reperimento dati tab. "MRA" (dati per invio e-mail)?
027200110523         exsr  sr_TabMRA;
027300100402
027400100402         // -?"Inizializzazione" dati con modifiche in sospeso?
027500100402         wSQL = 'update TNTBE00F +
027600100402                    set TBEuni = substr(TBEuni, 1, 31) concat ''0'' +
027700100514                                 concat substr(TBEuni, 33) +
027800100402                  where TBEcod = ''3EW'' +
027900100402                    and substr(TBEuni, 32, 1) = ''M''';
028000100402         exec sql   execute immediate  :wSQL;
028100091223
028200091223         // -?Preparazione stringa SQL?
028300091223         exsr  sr_PrepSQL;
028400091223
028500091223         // -?Apertura cursore SQL?
028600091223         exsr  sr_OpenCursor;
028700091223
028800091223       ENDSR;
028900110523
029000110523       //--------------------------------------------------------------
029100110523       //?Reperimento dati da tab. "MRA" (regole per invio e-mail).    ?
029200110523       //--------------------------------------------------------------
029300110523       BEGSR  sr_TabMRA;
029400110523
029500110523         reset  dMRAdan;
029600110524
029700110524         // -?Preparazione stringa SQL?
029800110524         wSQL = 'select TBEuni from TNTBE00F +
029900110524                  where TBEcod = ''MRA'' +
030000110524                    and TBEke1 = ''' + SDSpgm + ''' +
030100110524                    and TBEke2 = '' '' +
030200110524                    and TBEatb = '' ''';
030300110524
030400110524         // -?Dichiarazione cursore?
030500110524         exec sql   prepare S0   from :wSQL;
030600110524         exec sql   declare C0   cursor   for S0;
030700110524
030800110524         // -?Apertura del cursore?
030900110524         exec sql   open C0;
031000110524
031100110524         // -?Lettura cursore?
031200110524         if  SQLcode >= *zero;
031300121116           exec sql   fetch next   from C0   into :wSQL_dati.wSQLuni;
031400110524           select;
031500110524             when  SQLcode < *zero;
031600110524             when  SQLcode = 100;
031700110524             other;
031800121116               dMRAdan = wSQL_dati.wSQLuni;
031900110524           endsl;
032000110524         endif;
032100121116
032200121116         clear  wSQL_dati.wSQLuni;
032300110523
032400110523       ENDSR;
032500091223
032600091223       //--------------------------------------------------------------
032700091223       //?Preparazione stringa SQL                                     ?
032800091223       //--------------------------------------------------------------
032900091223       BEGSR  sr_PrepSQL;
033000091223
033100100325         // -?Parte iniziale (comune)?
033200121116         wSQL = 'select TBEke2, TBEke1, TBLkey, TBEuni, TBLuni, +
033300120814                        '' '', case +
033400120814                        when TBEatb <> '' '' then TBEatb +
033500120814                        when substr(TBEuni, 33, 1) <> '' '' then +
033600120814                             substr(TBEuni, 33, 1) +
033700120814                        else '' '' end, +
033800120814                        TBLflg +
033900100325                   from TNTBE00F inner join TABEL00F +
034000100325                     on substr(TBEke1, 2, 7) = substr(TBLuni, 79, 7) +
034100100325                    and TBEcod = ''3EW'' and TBLcod = ''3C'' +
034200100325                  where';
034300100325
034400100325         // -?Selezione per estrazione dati - incrementale?
034500100325         if  pIn_TypEXE = '1';
034600100325           wSQL +=    ' substr(TBEuni, 32, 1) NOT in (''1'', ''M'') +
034700100325                    and' ;
034800100325         endif;
034900100325
035000100325         // -?Parte comune?
035100100325         wSQL +=      ' TBEke2 in ( select VSSsun from TIVSS00F +
035200100325                                     where VSSisv = ''' + pIn_TypENV +
035300100324                                           ''' and VSSdsc >= ' +
035400100324                                           %editc( wDate : 'X' ) +
035500100325                                           ' ) +
035600100325                  UNION +
035700121119                 select TBEke2, TBEke1, TBLkey, TBEuni, TBLuni, +
035800100402                        ''A'', ''A'', ''A'' +
035900121119                   from TNTBE00F inner join TABEL00F +
036000121119                     on substr(TBEke1, 2, 7) = substr(TBLuni, 79, 7) +
036100121119                    and TBEcod = ''3EW'' and TBLcod = ''3C'' +
036200100325                  where TBEcod = ''3EW'' and';
036300100325
036400100325         // -?Selezione per estrazione dati - incrementale?
036500100325         if  pIn_TypEXE = '1';
036600100325           wSQL +=    ' substr(TBEuni, 32, 1) NOT in (''1'', ''M'') +
036700100325                    and';
036800100325         endif;
036900100325
037000100325         // -?Parte finale (comune)?
037100100325         wSQL +=      ' TBEke2 in ( select VSSsun from TIVSS00F +
037200100325                                     where VSSisv = ''' + pIn_TypENV +
037300100325                                           ''' and VSSdsc < ' +
037400100325                                           %editc( wDate : 'X' ) +
037500100325                                           ' ) +
037600100325                  order by TBEke2, TBEke1, TBLkey +
037700100325                    for fetch only';
037800091223
037900091223       ENDSR;
038000091223
038100091223       //--------------------------------------------------------------
038200091223       //?Apertura cursore.                                            ?
038300091223       //--------------------------------------------------------------
038400091223       BEGSR  sr_OpenCursor;
038500091223
038600091223         // -?Dichiarazione cursore?
038700091223         exec sql   prepare S1   from :wSQL;
038800100402         exec sql   declare C1   insensitive   cursor for S1;
038900091223
039000091223         // -?Apertura del cursore?
039100091223         exec sql   open C1;
039200091223
039300091223         if  SQLcode < *zero;
039400091223           $EoF = *on;
039500100305           RtnEsito  = -1;
039600100112           RtnOpCode = 'SQLCODE   ';
039700100112           RtnStatus = sqlCode;
039800100305           //exsr  sr_RoutEnd;        [qui NON serve!]?
039900091223         endif;
040000091223
040100091223       ENDSR;
040200091223
040300091223       //--------------------------------------------------------------
040400091223       //?Lettura cursore.                                             ?
040500091223       //--------------------------------------------------------------
040600091223       BEGSR  sr_ReadCursor;
040700091223
040800100304         clear  wSQL_dati;
040900091223
041000100304         exec sql   fetch next   from C1   into :wSQL_dati;
041100091223
041200091223         select;
041300100112
041400091223           when  SQLcode < *zero;
041500100112             $EoF = *on;
041600100305             RtnEsito  = -2;
041700100112             RtnOpCode = 'SQLCODE   ';
041800100112             RtnStatus = sqlCode;
041900100112
042000100112           when  SQLcode = 100;
042100100112             $EoF = *on;
042200100112             if  xx = *zero;
042300100305               RtnEsito  = -3;
042400100112               RtnOpCode = 'NO_DATI   ';
042500100112               RtnStatus = sqlCode;
042600100112             endif;
042700100112
042800091223           other;
042900091223             exsr  sr_ElabCliente;
043000100112
043100091223         endsl;
043200091223
043300091223       ENDSR;
043400091223
043500091223       //--------------------------------------------------------------
043600091223       //?Chiusura cursore.                                            ?
043700091223       //--------------------------------------------------------------
043800091223       BEGSR  sr_CloseCursor;
043900091223
044000091223         // -?Chiusura del cursore?
044100091223         exec sql   close C1;
044200091223
044300091223       ENDSR;
044400091223
044500091223       //--------------------------------------------------------------
044600091223       //?Elaborazione singolo cliente dalla tab. "3C                  ?
044700091223       //--------------------------------------------------------------
044800091223       BEGSR  sr_ElabCliente;
044900091223
045000091223         // -?Decodifica cliente?
045100100302         k_ACOkut = c_Kut;
045200100305         k_ACOkcc = c_Kci;
045300100406         if  wSQL_dati.wSQLkey > *zero;
045400100406           k_ACOksc = %int( %trim( wSQL_dati.wSQLkey ) );
045500100406           chain  %kds(k03cnaco00)  CNACO000;
045600100406           chain  %kds(k03cnaco00)  CNIND000;
045700100406         endif;
045800121119         //if  wSQL_dati.wSQLkey <= *zero  or
045900121119         //    Not %found(CNACO00F)   or   ACOflg <> *blank;
046000121119         //  clear  ACOrag;
046100121119         //endif;
046200121119         //if  wSQL_dati.wSQLkey <= *zero  or
046300121119         //    Not %found(CNIND00F)   or   INDflg <> *blank;
046400121119         //  clear  INDvia;
046500121119         //  clear  INDcit;
046600121119         //  clear  INDprv;
046700121119         //  clear  INDsta;
046800121119         //  clear  INDcae;
046900121119         //endif;
047000100302
047100100302         // -?Posizionamento nella schiera multipla?
047200100302         xx += 1;
047300100302         clear  sch_pOutDati(xx);
047400100302         clear  pOutDati_ds;
047500091223
047600091228         // -?Riempimento (altri) campi di output nella struttura dati?
047700121116         clear  ds3C;
047800121116         if  wSQL_dati.wSQLccmatb = *blank;
047900121116           ds3C    = wSQL_dati.wSQLuni_3C;
048000121116         endif;
048100100325         d3EW    = wSQL_dati.wSQLuni;
048200100325         §3EWazi = wSQL_dati.wSQLke2;
048300100325         §3EWksu = wSQL_dati.wSQLke1;
048400100325         §3EWccm = wSQL_dati.wSQLkey;
048500170511         if d3EW.§3EWlnp = 999;
048600170511            §3EWlnp = %dec(%subst(§3EWccm:1:3):3:0);
048700170511         else;
048800170511            §3EWlnp = d3EW.§3EWlnp;
048900170511         endif;
049000100325         §3EWfls = d3EW.§3EWfls;
049100100325         §3EWnrs = d3EW.§3EWnrs;
049200100325         §3EWnsi = d3EW.§3EWnsi;
049300100325         §3EWnsf = d3EW.§3EWnsf;
049400100325         §3EWctm = d3EW.§3EWctm;
049500100325         §3EWmad = d3EW.§3EWmad;
049600100325         §3EWaew = d3EW.§3EWaew;
049700100402         §3EWaziatb = wSQL_dati.wSQLaziatb;
049800100402         §3EWksuatb = wSQL_dati.wSQLksuatb;
049900121116         select;
050000121116           when  wSQL_dati.wSQLccmatb <> *blank;
050100121116             §3EWccmatb = 'A';
050200121116           when  ds3C.§3Cokd <> '*';
050300121116             §3EWccmatb = 'A';
050400121119             //clear  §3EWccm;
050500130314           //when  ACOabl = '8'  or  ACOabl = '*';
050600130314           when  ACOabl <> *blank;
050700121116             §3EWccmatb = 'A';
050800121119             //clear  §3EWccm;
050900121116           other;
051000121116             §3EWccmatb = wSQL_dati.wSQLccmatb;
051100121116         endsl;
051200121119         //if  §3EWccmatb = *blank;
051300121116           §3EWrag = ACOrag;
051400121116           §3EWprv = INDprv;
051500121116           §3EWsta = INDsta;
051600121116           §3EWcae = INDcae;
051700121116           §3EWcit = INDcit;
051800121116           §3EWvia = INDvia;
051900121119         //endif;
052000100121
052100100121         sch_pOutDati(xx) = pOutDati_ds;
052200100325
052300100325         // -?Aggiornamento flag di dato variato (§3EWUPD) in tab."3EW"?
052400110523         wRtnEsito = Ub§3EWupd_Upd ( wSQL_dati.wSQLke1 :
052500110523                                     wSQL_dati.wSQLke2 :
052600110523                                     'I' :
052700110523                                     '1' );
052800110524         if wRtnEsito < *zero  and  wRtnEsito <> -4;
052900110523           exsr  sr_SendEmail_1;
053000110524           //$EoF = *on;
053100110523           RtnEsito  = -4;
053200110523           RtnOpCode = 'UB§3EWUPD ';
053300110523           RtnStatus = wRtnEsito;
053400110524           //exsr  sr_RoutEnd;
053500110523         endif;
053600110516
053700110516         // -?Invio *msg di avviso al superamento del limite estraibile?
053800110516         if  xx > c_MaxRec  and  not %open(PrtEMAIL);
053900110523           exsr  sr_SendEmail_9;
054000110516         endif;
054100091223
054200091223       ENDSR;
054300110523
054400110523       //--------------------------------------------------------------
054500110523       //?Invio e-mail di avviso errore in aggiornamento flag §3EWUPD. ?
054600110523       //--------------------------------------------------------------
054700110523       BEGSR  sr_SendEmail_1;
054800110523
054900110523         // -?Override al file di stampa ed apertura dello stesso?
055000110523         if  NOT  %open(PrtEMAIL);
055100110523           §CM1mitt = %trim(§MRAdmitt);
055200110523           §CM1dst  = %trim(§MRAddest);
055300110523           §CM1tips = §MRAdreg;
055400110523           §CM1var  = '*OBJM*' + §MRAddes;
055500110523           §CM1idp  = §MRAdidpro;
055600110523           Qcmd = c_CmdOvrPrtF
055700110523                + ' outq(' + %trim(§MRAdoutqi) + ')'
055800110523                + ' usrdfndta(''' + TRTCM1ds + ''')';
055900110523           exsr  sr_ExecCmd;
056000110523           //*inH1 = (Qusei <> *blank);
056100110523           open  PrtEMAIL;
056200110523         endif;
056300110523
056400110523         // -?Stampa del messaggio di errore?
056500110523         select;
056600110523           when  wRtnEsito = -1;
056700110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
056800110523                        rilevato parametri errati per il cliente ' +
056900110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
057000110523                        %trimr(wSQL_dati.wSQLke2);
057100110523           when  wRtnEsito = -2;
057200110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
057300110523                        rilevato errori nel reperimento del nome +
057400110524                        del sistema per il cliente ' +
057500110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
057600110523                        %trimr(wSQL_dati.wSQLke2);
057700110523           when  wRtnEsito = -3;
057800110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
057900110523                        rilevato errori nell''apertura del file +
058000110523                        tabelle (tab. "3EW") per il cliente ' +
058100110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
058200110523                        %trimr(wSQL_dati.wSQLke2);
058300110523           when  wRtnEsito = -4;
058400130605             O_testo = 'Il programma di servizio UB§3EWUPD non ha +
058500130605                        trovato il cliente ' +
058600130605                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
058700130605                        %trimr(wSQL_dati.wSQLke2) +
058800130605                       ' in tab. "3EW".';
058900130605           when  wRtnEsito = -5;
059000110523             O_testo = 'Fallito aggiornamento del flag "Modificato" +
059100110523                        (§3EWUPD) in tab. "3EW" per il cliente ' +
059200110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
059300110523                        %trimr(wSQL_dati.wSQLke2);
059400110523           when  wRtnEsito = -6;
059500110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
059600110523                        rilevato errori nella chiusura del file +
059700110524                        tabelle (tab. "3EW"), key ' +
059800110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
059900110523                        %trimr(wSQL_dati.wSQLke2);
060000130605           when  wRtnEsito = -7;
060100130807             O_testo = 'Fallito aggiornam. del flag "Modificato" +
060200130807                        (§3EWUPD) in tab. "3EW" nel SIST.+
060300130807                        INFORM. di FIL. per il cliente ' +
060400130605                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
060500130605                        %trimr(wSQL_dati.wSQLke2);
060600110523         endsl;
060700110524         if  O_Testo <> *blank;
060800110524           except  PrtDet;
060900110524         endif;
061000110523
061100110523         // -?Chiusura del file di stampa e cancellazione override?
061200110523         close  PrtEMAIL;
061300110523         Qcmd = c_CmdDltOvr;
061400110523         exsr  sr_ExecCmd;
061500110523
061600110523       ENDSR;
061700110516
061800110516       //--------------------------------------------------------------
061900110516       //?Invio e-mail di avviso superamento limite rec. estraibili.   ?
062000110516       //--------------------------------------------------------------
062100110523       BEGSR  sr_SendEmail_9;
062200110516
062300110516         // -?Override al file di stampa ed apertura dello stesso?
062400110516         if  NOT  %open(PrtEMAIL);
062500110523           §CM1mitt = %trim(§MRAdmitt);
062600110523           §CM1dst  = %trim(§MRAddest);
062700110523           §CM1tips = §MRAdreg;
062800110523           §CM1idp  = §MRAdidpro;
062900110516           Qcmd = c_CmdOvrPrtF
063000110523                + ' outq(' + %trim(§MRAdoutqi) + ')'
063100110516                + ' usrdfndta(''' + TRTCM1ds + ''')';
063200110516           exsr  sr_ExecCmd;
063300110523           //*inH1 = (Qusei <> *blank);
063400110516           open  PrtEMAIL;
063500110516         endif;
063600110516
063700110516         // -?Stampa della 1ª riga del messaggio di errore?
063800110516         O_testo = 'Superato il numero limite di record estraibili dal+
063900110516                    la stored procedure di trasmissione anagrafiche +
064000110516                    mittenti ' + %trimr( SDSpgm ) +
064100110516                   ': ' + %editc( c_MaxRec : 'J' );
064200110516         except  PrtDet;
064300110516
064400110516       ENDSR;
064500110516
064600110516       //--------------------------------------------------------------
064700110516       //?Esecuzione del comando (già impostato).                      ?
064800110516       //--------------------------------------------------------------
064900110516       BEGSR  sr_ExecCmd;
065000110516
065100110516         clear Qcap0100;
065200110516         Qcabcsdh = *off;
065300110516         Qcapa    = *off;
065400110516         Qcacmdss = *off;
065500110516         Qcaerved = *allX'00';
065600110516
065700110516         clear Qusec;
065800110516         Qusbprv  = %size(Qusec);
065900110516
066000110516         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
066100110516                           %size(Qcap0100) : 'CPOP0100' : *omit :
066200110516                           0 : 0 : Qusec);
066300110516
066400110516         //if  Qusei <> *blank;
066500110516         //  ...;
066600110516         //endif;
066700110516
066800110516       ENDSR;
066900091223
067000091223       //--------------------------------------------------------------
067100091223       //?Operazioni finali.                                           ?
067200091223       //--------------------------------------------------------------
067300091223       BEGSR  sr_RoutEnd;
067400091223
067500091223         // -?Chiusura cursore SQL?
067600091223         exsr  sr_CloseCursor;
067700110516
067800110516         // -?Chiusura (eventuale) file di stampa?
067900110523         if  xx > c_MaxRec  and  %open(PrtEMAIL);
068000110516           O_testo = 'Aumentare tale limite: già estratti n° ' +
068100110516                      %editc( xx : 'J' ) + ' clienti.';
068200110516           except  PrtDet;
068300110516           close  PrtEMAIL;
068400110516           Qcmd = c_CmdDltOvr;
068500110516           exsr  sr_ExecCmd;
068600110516         endif;
068700091228
068800091228         // -?Restituzione parametri?
068900091228         exec sql   set result sets
069000091228                    for return to client
069100100121                    array :sch_pOutDati
069200091228                    for :xx rows;
069300091223
069400091223         // -?Chiusura pgm?
069500091223         return;
069600091223
069700091223       ENDSR;
069800091223
069900091223      /end-free
070000110516
070100110523       //--------------------------------------------------------------
070200110523       //?Spool di stampa (per e-mail).                                ?
070300110523       //--------------------------------------------------------------
070400110516
070500110516     oPrtEMAIL  e            PRTdet      1
070600110516     o                       O_testo
