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
004500150608     d c_MaxRec        c                   const(25000)
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
009700110523     d   §MRAddest   e                     inz('fabrizio.gurrieri+
009800120302     d                                          @brt.it; +
009900110523     d                                          antonio.sgrignoli+
010000120302     d                                          @brt.it')
010100110523     d   §MRAdidpro  e                     inz('2')
010200110523     d   §MRAdoutqi  e                     inz('EMAILIN')
010300110516
010400110516       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
010500110516     d TRTCM1ds      e ds                  inz
010600110516       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010700110516     d   §CM1mitt    e                     inz('ced')
010800110516       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010900120302     d   §CM1dst     e                     inz('ced@brt.it')
011000110516       //  ?·§CM1tips = Tipo lettera via e-mail:?
011100110516       //   ?"LET" = testo allegato in corpo con logo?
011200110516       //           ?(richiede righe libere iniziali per il logo)?
011300110516       //   ?"COR" = testo integrato senza logo?
011400110516       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011500110516     d   §CM1tips    e                     inz('COR')
011600110516       //  ?·§CM1po   = Filiale?
011700110516     d   §CM1po      e                     inz('046')
011800110516       //  ?·§CM1var  = Oggetto e-mail?
011900110516     d   §CM1var     e                     inz('*OBJM*+
012000110516     d                                     Nr MAX clienti estraibili -
012100110516     d                                     dalla s.p. di trasm. anagr-
012200110516     d                                     af. mittenti')
012300110516       //  ?·§CM1sts  = Stato?
012400110516     d   §CM1sts     e                     inz(*off)
012500110516       //  ?·§CM1sts  = Id processo?
012600110516     d   §CM1idp     e                     inz('2')
012700091223
012800091223       // -?Dati estratti via SQL?
012900100325     d wSQL_dati       ds                  inz  qualified
013000110524     d   wSQLke2                           inz  like(TBEke2)
013100110524     d   wSQLke1                           inz  like(TBEke1)
013200110524     d   wSQLkey                           inz  like(TABELds.TBLkey)
013300110524     d   wSQLuni                           inz  like(TBEuni)
013400121116     d   wSQLuni_3C                        inz  like(TABELds.TBLuni)
013500100402     d   wSQLaziatb                   1a   inz
013600110524     d   wSQLksuatb                        inz  like(TBEatb)
013700110524     d   wSQLccmatb                        inz  like(TABELds.TBLflg)
013800100121
013900100121       // -?Dati da restituire (per ogni cliente)?
014000100121     d pOutDati_ds...
014100100121     d                 ds                  inz
014200110524     d   §3EWazi                           inz  like(TBEke1)
014300110524     d   §3EWksu                           inz  like(TBEke2)
014400110524     d   §3EWccm                           inz  like(TABELds.TBLkey)
014500100402     d   §3EWlnp                           inz  like(d3EW.§3EWlnp)
014600100402     d   §3EWfls                           inz  like(d3EW.§3EWfls)
014700100402     d   §3EWnrs                           inz  like(d3EW.§3EWnrs)
014800100402     d   §3EWnsi                           inz  like(d3EW.§3EWnsi)
014900100402     d   §3EWnsf                           inz  like(d3EW.§3EWnsf)
015000100402     d   §3EWctm                           inz  like(d3EW.§3EWctm)
015100100402     d   §3EWmad                           inz  like(d3EW.§3EWmad)
015200100402     d   §3EWaew                           inz  like(d3EW.§3EWaew)
015300100402     d   §3EWaziatb                   1a   inz
015400110524     d   §3EWksuatb                        inz  like(TBEatb)
015500110524     d   §3EWccmatb                        inz  like(TABELds.TBLflg)
015600100402     d   §3EWrag                           inz  like(ACOrag)
015700100402     d   §3EWprv                           inz  like(INDprv)
015800100402     d   §3EWsta                           inz  like(INDsta)
015900100402     d   §3EWcae                           inz  like(INDcae)
016000100402     d   §3EWcit                           inz  like(INDcit)
016100100402     d   §3EWvia                           inz  like(INDvia)
016200110516
016300110516       // -?Status?
016400110516     d Psds           sds
016500110516     d   SDSpgm          *proc
016600110516     d***JobName             244    253                                         Job name
016700110523     d***JobUser             254    263                                         User name
016800110516     d***JobNumber           264    269s 0                                      Job number
016900091223
017000091223       //--------------------------------------------------------------
017100091223       //?Definizione variabili.                                       ?
017200091223       //--------------------------------------------------------------
017300091223
017400100323       // -?Parametri ricevuti:?
017500100323       //   ·?Tipo Ambiente EasyWeb: "EW" = Ambiente Production,?
017600100323       //                           ?"ET" = Ambiente Test.?
017700100325     d pIn_TypENV      s              2a
017800100323       //   ·?Tipo Lancio: *on  = Solo modificati,?
017900100323       //                 ?*off = Tutti.?
018000100325     d pIn_TypEXE      s              1a
018100100303
018200100303       // -?Parametri ricevuti e da restituire al chiamante?
018300100303     d RtnEsito        s             10i 0
018400100303     d RtnOpCode       s             10
018500100303     d RtnStatus       s             10i 0
018600100303
018700091223       // -?Flags booleani?
018800091223     d $EoF            s               n   inz
018900091228
019000091228       // -?Indici di schiera?
019100091228     d xx              s              5  0 inz
019200091223
019300100112       // -?Campi di comodo?
019400100112     d wDate           s              8  0 inz
019500110516
019600110516       // -?Campi di stampa?
019700110516     d O_testo         s            132    inz
019800091223
019900091223       // -?Stringa SQL da eseguire?
020000100325     d wSQL            s           2048    inz  varying
020100091223
020200091223       //--------------------------------------------------------------
020300091223       //?Definizione prototipi procedure.                             ?
020400091223       //--------------------------------------------------------------
020500100325
020600100325       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
020700110523     d wRtnEsito       s             10i 0 inz
020800100325      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
020900110516
021000110516       // -?Parametri API QCAPCMD (Process Commands)?
021100110516     d Qcmd            s           2048    inz  varying
021200110516      /copy qSysInc/qRpgleSrc,QCAPCMD
021300110516       // -?API QCAPCMD (Process Commands)?
021400110516      /copy gaitrasrc/srcProtoPR,QCAPCMD
021500110516
021600110516       // -?Parametri gestione errori API.?
021700110516      /copy qsysinc/qrpglesrc,QUSEC
021800091223
021900091223       //--------------------------------------------------------------
022000091223       //?Definizione key-list.                                        ?
022100091223       //--------------------------------------------------------------
022200091223
022300100302       // -?File CNACO00F e CNIND00F?
022400100302     d k03cnaco00    e ds                  extname(CNACO00F : *key)
022500100302     d                                     prefix(k_)   inz
022600091223
022700091223       //--------------------------------------------------------------
022800091223       //?Definizione parametri procedura.                             ?
022900091223       //--------------------------------------------------------------
023000091223
023100091223     c     *Entry        plist
023200100325     c                   parm                    pIn_TypENV
023300100325     c                   parm                    pIn_TypEXE
023400091223     c                   parm                    RtnEsito
023500091223     c                   parm                    RtnOpCode
023600091223     c                   parm                    RtnStatus
023700091223
023800091223      /free
023900091223
024000091223       //--------------------------------------------------------------
024100091223       //?M A I N - L I N E                                            ?
024200091223       //--------------------------------------------------------------
024300091223
024400091223       // -?Operazioni iniziali?
024500091223       exsr sr_RoutInz;
024600091223
024700091223       // -?Ciclo di reperimento clienti in tab. "3C"?
024800100112       DoW  Not $EoF;
024900091223         exsr  sr_ReadCursor;
025000091223       EndDo;
025100091223
025200091223       // -?Operazioni finali?
025300091223       exsr sr_RoutEnd;
025400091223
025500091223       //--------------------------------------------------------------
025600091223       //?Operazioni iniziali.                                         ?
025700091223       //--------------------------------------------------------------
025800091223       BEGSR  sr_RoutInz;
025900091223
026000091223         exec sql  set option  dynusrprf = *owner,
026100100325                               closqlcsr = *endmod;
026200091223
026300091223         *inLR = *on;
026400100112
026500100303         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
026600100303         wDate = %dec( %date() );
026700091223
026800091228         // -?Pulizia dati di Output?
026900091223         clear  RtnEsito;
027000091223         clear  RtnOpCode;
027100091223         clear  RtnStatus;
027200110516         //clear  sch_pOutDati;       [già pulita]?
027300110523
027400110524         // -?Reperimento dati tab. "MRA" (dati per invio e-mail)?
027500110523         exsr  sr_TabMRA;
027600100402
027700100402         // -?"Inizializzazione" dati con modifiche in sospeso?
027800100402         wSQL = 'update TNTBE00F +
027900100402                    set TBEuni = substr(TBEuni, 1, 31) concat ''0'' +
028000100514                                 concat substr(TBEuni, 33) +
028100100402                  where TBEcod = ''3EW'' +
028200100402                    and substr(TBEuni, 32, 1) = ''M''';
028300100402         exec sql   execute immediate  :wSQL;
028400091223
028500091223         // -?Preparazione stringa SQL?
028600091223         exsr  sr_PrepSQL;
028700091223
028800091223         // -?Apertura cursore SQL?
028900091223         exsr  sr_OpenCursor;
029000091223
029100091223       ENDSR;
029200110523
029300110523       //--------------------------------------------------------------
029400110523       //?Reperimento dati da tab. "MRA" (regole per invio e-mail).    ?
029500110523       //--------------------------------------------------------------
029600110523       BEGSR  sr_TabMRA;
029700110523
029800110523         reset  dMRAdan;
029900110524
030000110524         // -?Preparazione stringa SQL?
030100110524         wSQL = 'select TBEuni from TNTBE00F +
030200110524                  where TBEcod = ''MRA'' +
030300110524                    and TBEke1 = ''' + SDSpgm + ''' +
030400110524                    and TBEke2 = '' '' +
030500110524                    and TBEatb = '' ''';
030600110524
030700110524         // -?Dichiarazione cursore?
030800110524         exec sql   prepare S0   from :wSQL;
030900110524         exec sql   declare C0   cursor   for S0;
031000110524
031100110524         // -?Apertura del cursore?
031200110524         exec sql   open C0;
031300110524
031400110524         // -?Lettura cursore?
031500110524         if  SQLcode >= *zero;
031600121116           exec sql   fetch next   from C0   into :wSQL_dati.wSQLuni;
031700110524           select;
031800110524             when  SQLcode < *zero;
031900110524             when  SQLcode = 100;
032000110524             other;
032100121116               dMRAdan = wSQL_dati.wSQLuni;
032200110524           endsl;
032300110524         endif;
032400121116
032500121116         clear  wSQL_dati.wSQLuni;
032600110523
032700110523       ENDSR;
032800091223
032900091223       //--------------------------------------------------------------
033000091223       //?Preparazione stringa SQL                                     ?
033100091223       //--------------------------------------------------------------
033200091223       BEGSR  sr_PrepSQL;
033300091223
033400100325         // -?Parte iniziale (comune)?
033500121116         wSQL = 'select TBEke2, TBEke1, TBLkey, TBEuni, TBLuni, +
033600120814                        '' '', case +
033700120814                        when TBEatb <> '' '' then TBEatb +
033800120814                        when substr(TBEuni, 33, 1) <> '' '' then +
033900120814                             substr(TBEuni, 33, 1) +
034000120814                        else '' '' end, +
034100120814                        TBLflg +
034200100325                   from TNTBE00F inner join TABEL00F +
034300100325                     on substr(TBEke1, 2, 7) = substr(TBLuni, 79, 7) +
034400100325                    and TBEcod = ''3EW'' and TBLcod = ''3C'' +
034500100325                  where';
034600100325
034700100325         // -?Selezione per estrazione dati - incrementale?
034800100325         if  pIn_TypEXE = '1';
034900100325           wSQL +=    ' substr(TBEuni, 32, 1) NOT in (''1'', ''M'') +
035000100325                    and' ;
035100100325         endif;
035200100325
035300100325         // -?Parte comune?
035400100325         wSQL +=      ' TBEke2 in ( select VSSsun from TIVSS00F +
035500100325                                     where VSSisv = ''' + pIn_TypENV +
035600100324                                           ''' and VSSdsc >= ' +
035700100324                                           %editc( wDate : 'X' ) +
035800100325                                           ' ) +
035900100325                  UNION +
036000121119                 select TBEke2, TBEke1, TBLkey, TBEuni, TBLuni, +
036100100402                        ''A'', ''A'', ''A'' +
036200121119                   from TNTBE00F inner join TABEL00F +
036300121119                     on substr(TBEke1, 2, 7) = substr(TBLuni, 79, 7) +
036400121119                    and TBEcod = ''3EW'' and TBLcod = ''3C'' +
036500100325                  where TBEcod = ''3EW'' and';
036600100325
036700100325         // -?Selezione per estrazione dati - incrementale?
036800100325         if  pIn_TypEXE = '1';
036900100325           wSQL +=    ' substr(TBEuni, 32, 1) NOT in (''1'', ''M'') +
037000100325                    and';
037100100325         endif;
037200100325
037300100325         // -?Parte finale (comune)?
037400100325         wSQL +=      ' TBEke2 in ( select VSSsun from TIVSS00F +
037500100325                                     where VSSisv = ''' + pIn_TypENV +
037600100325                                           ''' and VSSdsc < ' +
037700100325                                           %editc( wDate : 'X' ) +
037800100325                                           ' ) +
037900100325                  order by TBEke2, TBEke1, TBLkey +
038000100325                    for fetch only';
038100091223
038200091223       ENDSR;
038300091223
038400091223       //--------------------------------------------------------------
038500091223       //?Apertura cursore.                                            ?
038600091223       //--------------------------------------------------------------
038700091223       BEGSR  sr_OpenCursor;
038800091223
038900091223         // -?Dichiarazione cursore?
039000091223         exec sql   prepare S1   from :wSQL;
039100100402         exec sql   declare C1   insensitive   cursor for S1;
039200091223
039300091223         // -?Apertura del cursore?
039400091223         exec sql   open C1;
039500091223
039600091223         if  SQLcode < *zero;
039700091223           $EoF = *on;
039800100305           RtnEsito  = -1;
039900100112           RtnOpCode = 'SQLCODE   ';
040000100112           RtnStatus = sqlCode;
040100100305           //exsr  sr_RoutEnd;        [qui NON serve!]?
040200091223         endif;
040300091223
040400091223       ENDSR;
040500091223
040600091223       //--------------------------------------------------------------
040700091223       //?Lettura cursore.                                             ?
040800091223       //--------------------------------------------------------------
040900091223       BEGSR  sr_ReadCursor;
041000091223
041100100304         clear  wSQL_dati;
041200091223
041300100304         exec sql   fetch next   from C1   into :wSQL_dati;
041400091223
041500091223         select;
041600100112
041700091223           when  SQLcode < *zero;
041800100112             $EoF = *on;
041900100305             RtnEsito  = -2;
042000100112             RtnOpCode = 'SQLCODE   ';
042100100112             RtnStatus = sqlCode;
042200100112
042300100112           when  SQLcode = 100;
042400100112             $EoF = *on;
042500100112             if  xx = *zero;
042600100305               RtnEsito  = -3;
042700100112               RtnOpCode = 'NO_DATI   ';
042800100112               RtnStatus = sqlCode;
042900100112             endif;
043000100112
043100091223           other;
043200091223             exsr  sr_ElabCliente;
043300100112
043400091223         endsl;
043500091223
043600091223       ENDSR;
043700091223
043800091223       //--------------------------------------------------------------
043900091223       //?Chiusura cursore.                                            ?
044000091223       //--------------------------------------------------------------
044100091223       BEGSR  sr_CloseCursor;
044200091223
044300091223         // -?Chiusura del cursore?
044400091223         exec sql   close C1;
044500091223
044600091223       ENDSR;
044700091223
044800091223       //--------------------------------------------------------------
044900091223       //?Elaborazione singolo cliente dalla tab. "3C                  ?
045000091223       //--------------------------------------------------------------
045100091223       BEGSR  sr_ElabCliente;
045200091223
045300091223         // -?Decodifica cliente?
045400100302         k_ACOkut = c_Kut;
045500100305         k_ACOkcc = c_Kci;
045600100406         if  wSQL_dati.wSQLkey > *zero;
045700100406           k_ACOksc = %int( %trim( wSQL_dati.wSQLkey ) );
045800100406           chain  %kds(k03cnaco00)  CNACO000;
045900100406           chain  %kds(k03cnaco00)  CNIND000;
046000100406         endif;
046100121119         //if  wSQL_dati.wSQLkey <= *zero  or
046200121119         //    Not %found(CNACO00F)   or   ACOflg <> *blank;
046300121119         //  clear  ACOrag;
046400121119         //endif;
046500121119         //if  wSQL_dati.wSQLkey <= *zero  or
046600121119         //    Not %found(CNIND00F)   or   INDflg <> *blank;
046700121119         //  clear  INDvia;
046800121119         //  clear  INDcit;
046900121119         //  clear  INDprv;
047000121119         //  clear  INDsta;
047100121119         //  clear  INDcae;
047200121119         //endif;
047300100302
047400100302         // -?Posizionamento nella schiera multipla?
047500100302         xx += 1;
047600100302         clear  sch_pOutDati(xx);
047700100302         clear  pOutDati_ds;
047800091223
047900091228         // -?Riempimento (altri) campi di output nella struttura dati?
048000121116         clear  ds3C;
048100121116         if  wSQL_dati.wSQLccmatb = *blank;
048200121116           ds3C    = wSQL_dati.wSQLuni_3C;
048300121116         endif;
048400100325         d3EW    = wSQL_dati.wSQLuni;
048500100325         §3EWazi = wSQL_dati.wSQLke2;
048600100325         §3EWksu = wSQL_dati.wSQLke1;
048700100325         §3EWccm = wSQL_dati.wSQLkey;
048800170511         if d3EW.§3EWlnp = 999;
048900170511            §3EWlnp = %dec(%subst(§3EWccm:1:3):3:0);
049000170511         else;
049100170511            §3EWlnp = d3EW.§3EWlnp;
049200170511         endif;
049300100325         §3EWfls = d3EW.§3EWfls;
049400100325         §3EWnrs = d3EW.§3EWnrs;
049500100325         §3EWnsi = d3EW.§3EWnsi;
049600100325         §3EWnsf = d3EW.§3EWnsf;
049700100325         §3EWctm = d3EW.§3EWctm;
049800100325         §3EWmad = d3EW.§3EWmad;
049900100325         §3EWaew = d3EW.§3EWaew;
050000100402         §3EWaziatb = wSQL_dati.wSQLaziatb;
050100100402         §3EWksuatb = wSQL_dati.wSQLksuatb;
050200121116         select;
050300121116           when  wSQL_dati.wSQLccmatb <> *blank;
050400121116             §3EWccmatb = 'A';
050500121116           when  ds3C.§3Cokd <> '*';
050600121116             §3EWccmatb = 'A';
050700121119             //clear  §3EWccm;
050800130314           //when  ACOabl = '8'  or  ACOabl = '*';
050900130314           when  ACOabl <> *blank;
051000121116             §3EWccmatb = 'A';
051100121119             //clear  §3EWccm;
051200121116           other;
051300121116             §3EWccmatb = wSQL_dati.wSQLccmatb;
051400121116         endsl;
051500121119         //if  §3EWccmatb = *blank;
051600121116           §3EWrag = ACOrag;
051700121116           §3EWprv = INDprv;
051800121116           §3EWsta = INDsta;
051900121116           §3EWcae = INDcae;
052000121116           §3EWcit = INDcit;
052100121116           §3EWvia = INDvia;
052200121119         //endif;
052300100121
052400100121         sch_pOutDati(xx) = pOutDati_ds;
052500100325
052600100325         // -?Aggiornamento flag di dato variato (§3EWUPD) in tab."3EW"?
052700110523         wRtnEsito = Ub§3EWupd_Upd ( wSQL_dati.wSQLke1 :
052800110523                                     wSQL_dati.wSQLke2 :
052900110523                                     'I' :
053000110523                                     '1' );
053100110524         if wRtnEsito < *zero  and  wRtnEsito <> -4;
053200110523           exsr  sr_SendEmail_1;
053300110524           //$EoF = *on;
053400110523           RtnEsito  = -4;
053500110523           RtnOpCode = 'UB§3EWUPD ';
053600110523           RtnStatus = wRtnEsito;
053700110524           //exsr  sr_RoutEnd;
053800110523         endif;
053900110516
054000110516         // -?Invio *msg di avviso al superamento del limite estraibile?
054100110516         if  xx > c_MaxRec  and  not %open(PrtEMAIL);
054200110523           exsr  sr_SendEmail_9;
054300110516         endif;
054400091223
054500091223       ENDSR;
054600110523
054700110523       //--------------------------------------------------------------
054800110523       //?Invio e-mail di avviso errore in aggiornamento flag §3EWUPD. ?
054900110523       //--------------------------------------------------------------
055000110523       BEGSR  sr_SendEmail_1;
055100110523
055200110523         // -?Override al file di stampa ed apertura dello stesso?
055300110523         if  NOT  %open(PrtEMAIL);
055400110523           §CM1mitt = %trim(§MRAdmitt);
055500110523           §CM1dst  = %trim(§MRAddest);
055600110523           §CM1tips = §MRAdreg;
055700110523           §CM1var  = '*OBJM*' + §MRAddes;
055800110523           §CM1idp  = §MRAdidpro;
055900110523           Qcmd = c_CmdOvrPrtF
056000110523                + ' outq(' + %trim(§MRAdoutqi) + ')'
056100110523                + ' usrdfndta(''' + TRTCM1ds + ''')';
056200110523           exsr  sr_ExecCmd;
056300110523           //*inH1 = (Qusei <> *blank);
056400110523           open  PrtEMAIL;
056500110523         endif;
056600110523
056700110523         // -?Stampa del messaggio di errore?
056800110523         select;
056900110523           when  wRtnEsito = -1;
057000110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
057100110523                        rilevato parametri errati per il cliente ' +
057200110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
057300110523                        %trimr(wSQL_dati.wSQLke2);
057400110523           when  wRtnEsito = -2;
057500110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
057600110523                        rilevato errori nel reperimento del nome +
057700110524                        del sistema per il cliente ' +
057800110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
057900110523                        %trimr(wSQL_dati.wSQLke2);
058000110523           when  wRtnEsito = -3;
058100110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
058200110523                        rilevato errori nell''apertura del file +
058300110523                        tabelle (tab. "3EW") per il cliente ' +
058400110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
058500110523                        %trimr(wSQL_dati.wSQLke2);
058600110523           when  wRtnEsito = -4;
058700130605             O_testo = 'Il programma di servizio UB§3EWUPD non ha +
058800130605                        trovato il cliente ' +
058900130605                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
059000130605                        %trimr(wSQL_dati.wSQLke2) +
059100130605                       ' in tab. "3EW".';
059200130605           when  wRtnEsito = -5;
059300110523             O_testo = 'Fallito aggiornamento del flag "Modificato" +
059400110523                        (§3EWUPD) in tab. "3EW" per il cliente ' +
059500110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
059600110523                        %trimr(wSQL_dati.wSQLke2);
059700110523           when  wRtnEsito = -6;
059800110523             O_testo = 'Il programma di servizio UB§3EWUPD ha +
059900110523                        rilevato errori nella chiusura del file +
060000110524                        tabelle (tab. "3EW"), key ' +
060100110523                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
060200110523                        %trimr(wSQL_dati.wSQLke2);
060300130605           when  wRtnEsito = -7;
060400130807             O_testo = 'Fallito aggiornam. del flag "Modificato" +
060500130807                        (§3EWUPD) in tab. "3EW" nel SIST.+
060600130807                        INFORM. di FIL. per il cliente ' +
060700130605                        %trimr(wSQL_dati.wSQLke1) + ' - ' +
060800130605                        %trimr(wSQL_dati.wSQLke2);
060900110523         endsl;
061000110524         if  O_Testo <> *blank;
061100110524           except  PrtDet;
061200110524         endif;
061300110523
061400110523         // -?Chiusura del file di stampa e cancellazione override?
061500110523         close  PrtEMAIL;
061600110523         Qcmd = c_CmdDltOvr;
061700110523         exsr  sr_ExecCmd;
061800110523
061900110523       ENDSR;
062000110516
062100110516       //--------------------------------------------------------------
062200110516       //?Invio e-mail di avviso superamento limite rec. estraibili.   ?
062300110516       //--------------------------------------------------------------
062400110523       BEGSR  sr_SendEmail_9;
062500110516
062600110516         // -?Override al file di stampa ed apertura dello stesso?
062700110516         if  NOT  %open(PrtEMAIL);
062800110523           §CM1mitt = %trim(§MRAdmitt);
062900110523           §CM1dst  = %trim(§MRAddest);
063000110523           §CM1tips = §MRAdreg;
063100110523           §CM1idp  = §MRAdidpro;
063200110516           Qcmd = c_CmdOvrPrtF
063300110523                + ' outq(' + %trim(§MRAdoutqi) + ')'
063400110516                + ' usrdfndta(''' + TRTCM1ds + ''')';
063500110516           exsr  sr_ExecCmd;
063600110523           //*inH1 = (Qusei <> *blank);
063700110516           open  PrtEMAIL;
063800110516         endif;
063900110516
064000110516         // -?Stampa della 1ª riga del messaggio di errore?
064100110516         O_testo = 'Superato il numero limite di record estraibili dal+
064200110516                    la stored procedure di trasmissione anagrafiche +
064300110516                    mittenti ' + %trimr( SDSpgm ) +
064400110516                   ': ' + %editc( c_MaxRec : 'J' );
064500110516         except  PrtDet;
064600110516
064700110516       ENDSR;
064800110516
064900110516       //--------------------------------------------------------------
065000110516       //?Esecuzione del comando (già impostato).                      ?
065100110516       //--------------------------------------------------------------
065200110516       BEGSR  sr_ExecCmd;
065300110516
065400110516         clear Qcap0100;
065500110516         Qcabcsdh = *off;
065600110516         Qcapa    = *off;
065700110516         Qcacmdss = *off;
065800110516         Qcaerved = *allX'00';
065900110516
066000110516         clear Qusec;
066100110516         Qusbprv  = %size(Qusec);
066200110516
066300110516         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
066400110516                           %size(Qcap0100) : 'CPOP0100' : *omit :
066500110516                           0 : 0 : Qusec);
066600110516
066700110516         //if  Qusei <> *blank;
066800110516         //  ...;
066900110516         //endif;
067000110516
067100110516       ENDSR;
067200091223
067300091223       //--------------------------------------------------------------
067400091223       //?Operazioni finali.                                           ?
067500091223       //--------------------------------------------------------------
067600091223       BEGSR  sr_RoutEnd;
067700091223
067800091223         // -?Chiusura cursore SQL?
067900091223         exsr  sr_CloseCursor;
068000110516
068100110516         // -?Chiusura (eventuale) file di stampa?
068200110523         if  xx > c_MaxRec  and  %open(PrtEMAIL);
068300110516           O_testo = 'Aumentare tale limite: già estratti n° ' +
068400110516                      %editc( xx : 'J' ) + ' clienti.';
068500110516           except  PrtDet;
068600110516           close  PrtEMAIL;
068700110516           Qcmd = c_CmdDltOvr;
068800110516           exsr  sr_ExecCmd;
068900110516         endif;
069000091228
069100091228         // -?Restituzione parametri?
069200091228         exec sql   set result sets
069300091228                    for return to client
069400100121                    array :sch_pOutDati
069500091228                    for :xx rows;
069600091223
069700091223         // -?Chiusura pgm?
069800091223         return;
069900091223
070000091223       ENDSR;
070100091223
070200091223      /end-free
070300110516
070400110523       //--------------------------------------------------------------
070500110523       //?Spool di stampa (per e-mail).                                ?
070600110523       //--------------------------------------------------------------
070700110516
070800110516     oPrtEMAIL  e            PRTdet      1
070900110516     o                       O_testo
