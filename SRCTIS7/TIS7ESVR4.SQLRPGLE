000100111027       //==============================================================
000200111110       //?Invio sollecito via e-mail al POC per ERRORI rilevati nel    ?
000300111110       //?file "Storico Easy-Sped - Versioni".                         ?
000400111027       //==============================================================
000500111027
000600111027       //--------------------------------------------------------------
000700111027       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800111027       //--------------------------------------------------------------
000900111027
001000111027     /*PRM dbgview(*source)
001100111027     /*END
001200111027
001300111027       //--------------------------------------------------------------
001400111027       //?Specifiche di controllo.                                     ?
001500111027       //--------------------------------------------------------------
001600111027
001700111027     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800111027     h dftactgrp(*no) bnddir('TRUL')
001900111027     h alwnull(*inputonly)
002000111027
002100111027       //--------------------------------------------------------------
002200111027       //?Dichiarazione file.                                          ?
002300111027       //--------------------------------------------------------------
002400111027
002500111027       // -?Immagini Lettere di Vettura?
002600111111     fWFESV00F  Uf   e             disk
002700111027
002800111027       // -?Stampa errori (e-mail)?
002900111027     fPrtEMAIL  o    f  158        printer  oflind(*inOF)  usropn
003000111027
003100111027       //--------------------------------------------------------------
003200111027       //?Definizione costanti.                                        ?
003300111027       //--------------------------------------------------------------
003400111027
003500111027       // -?Comando di override al PrtF?
003600111027     d c_CmdOvrPrtF    c                   const('OVRPRTF +
003700111027     d                                           file(PRTEMAIL) +
003800111027     d                                           pagesize(66 158) +
003900111027     d                                           lpi(6) cpi(12) +
004000111027     d                                           ovrscope(*actgrpdfn) +
004100111027     d                                           ')
004200111027     d c_CmdDltOvr     c                   const('DLTOVR +
004300111027     d                                            file(PRTEMAIL) +
004400111027     d                                            lvl(*actgrpdfn)')
004500111107
004600111107       // -?Costante con la versione di Easy-Sped che prevede controlli?
004700111107       //  ?sul Tipo Aggiornamento Web?
004800111107     d c_VersEsySp     c                   const('4.0.4 ')
004900111115       // -?Costante con la versione di Easy-Sped che NON prevede più?
005000111115       //  ?controlli sul Corpo eMail?
005100111107     d c_VersEsySp2    c                   const('4.0.7 ')
005200111027
005300111027       // -?Intestazione e-mail?
005400111110     d c_Titolo        c                   const('Segnalazione errori -
005500111110     d                                     rilevati nei dati EasySped -
005600111111     d                                     ricevuti dai clienti della -
005700111111     d                                     tua area.')
005800111111
005900111114       // -?Lineee separatrice in stampa?         *...+....1....+....2
006000111114     d c_SepLine1      c                   const('____________________+
006100111114     d                                            ____________________+
006200111114     d                                            ____________________+
006300111114     d                                            ____________________+
006400111114     d                                            ____________________+
006500111114     d                                            _______')
006600111114     d c_SepLine2      c                   const('¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯+
006700111111     d                                            ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯+
006800111111     d                                            ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯+
006900111114     d                                            ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯+
007000111114     d                                            ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯+
007100111114     d                                            ¯¯¯¯¯¯¯')
007200111121     d c_SepPoint      c                   const('                    -
007300111121     d                                                                -
007400111121     d                                                                -
007500111121     d                                                                -
007600111121     d                                          ·')
007700111027
007800111027       // -?Dati di default?
007900111027     d c_Sede          c                   const('046')
008000120302     d c_AtBrt         c                   const('@brt.it')
008100111114
008200111114       // -?Messaggio di avviso allocazione rec. di WFESV00F?
008300111114       //  ?(max 80 char)?
008400111115     d c_Msg_01        c                   CONST('Si sta bloccando l''i+
008500111114     d                                            nvio solleciti ai Po+
008600111114     d                                            c. Uscire SUBITO da +
008700111114     d                                            questo lavoro!      +
008800111114     d                                           ')
008900111027
009000111027       //--------------------------------------------------------------
009100111027       //?Definizione schiere.                                         ?
009200111027       //--------------------------------------------------------------
009300111107
009400111107       // -?Tipi Aggiornamento Web?
009500111107     d sch_TypWeb      s              2    dim(08)  ctdata  perrcd(1)
009600111107     d sch_TypWebD     s             30    dim(08)  alt(sch_TypWeb)
009700111027
009800111027       // -?Codici Errore / Giorni Limite per invio e-mail?
009900111117     d c_MaxErr        c                   const(05)
010000111117     d sch_Err         s              2    dim(c_MaxErr) ctdata perrcd(1)
010100111031     d sch_DatiErr     s                   like(ds_DatiErr)
010200111117     d                                     dim(c_MaxErr) alt(sch_Err)
010300111117       // -?Descrizioni aggiuntive delle azioni da intraprendere?
010400111117       //  ?(per errore): 4 righe per ogni errore (4 * 5 = 20)?
010500111117     d c_ErrLines      c                   const(04)
010600111117     d sch_Msg2        s             80    dim(20)  ctdata  perrcd(1)
010700111027
010800111027       //--------------------------------------------------------------
010900111027       //?Definizione aree dati.                                       ?
011000111027       //--------------------------------------------------------------
011100111027
011200111027       // -?Dati utente?
011300111027     d §AzUte        e ds                  extname(AZUTE00F)
011400111027     d                                     dtaara
011500111027     d §DatiUte      e ds                  extname(dDatiUte)
011600111027     d                                     dtaara
011700111027
011800111027       //---------------------------------------------------------------
011900111027       //?Definizione strutture dati.                                  ?
012000111027       //---------------------------------------------------------------
012100111027
012200111027       // -?Status?
012300111027     d Psds           sds
012400111027     d   SDSpgm          *proc
012500111027     d*//JobName             244    253                                         Job name
012600111027     d   JobUser             254    263                                         User name
012700111027     d*//JobNumber           264    269s 0                                      Job number
012800111111
012900111111       // -?Dati del singolo errore in schiera sch_DatiErr?
013000111111     d ds_DatiErr      ds            98    inz   qualified
013100111111     d   DE___filler1                 1    inz
013200111111     d   DE_Gravita                   2s 0 inz
013300111111     d   DE___filler2                 1    inz
013400111111     d   DE_SNmail                    1    inz
013500111111     d   DE___filler3                 1    inz
013600111111     d   DE_Tempist                   3s 0 inz
013700111111     d   DE___filler4                 1    inz
013800111118     d   DE_TettoCED                  3s 0 inz
013900111118     d   DE___filler5                 1    inz
014000111118     d   DE_TxtAggior                84    inz
014100111107
014200111107       // -?Definizione campi?
014300111107     d cnDizion      e ds                  qualified  based(nullPtr)
014400111111     d AzOrg00F      e ds                  qualified  based(nullPtr)
014500111027
014600111027       // -?Parametri ricevuti?
014700111027     d KPJBA         e ds
014800111027
014900111027       // -?Tabella "MRA" = Bart-Mailing - Danni?
015000111027     d dMRAdan       e ds                  inz
015100111027
015200111027       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
015300111027     d TRTCM1ds      e ds                  inz
015400111027       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
015500111027     d   §CM1mitt    e                     inz('ced')
015600111027       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
015700120302     d   §CM1dst     e                     inz('ced@brt.it')
015800111027       //  ?·§CM1tips = Tipo lettera via e-mail:?
015900111027       //   ?"LET" = testo allegato in corpo con logo?
016000111027       //           ?(richiede righe libere iniziali per il logo)?
016100111027       //   ?"COR" = testo integrato senza logo?
016200111027       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
016300111027     d   §CM1tips    e                     inz('COR')
016400111027       //  ?·§CM1po   = Filiale?
016500111031     d   §CM1po      e                     inz('046')
016600111027       //  ?·§CM1var  = Oggetto e-mail?
016700111027     d   §CM1var     e                     inz('*OBJM*+
016800111027     d                                     Errori rilevati in fase di +
016900111027     d                                     memorizzaz. versioni EasySp+
017000111027     d                                     ed in tab.')
017100111027       //  ?·§CM1sts  = Stato?
017200111027     d   §CM1sts     e                     inz(*off)
017300111027       //  ?·§CM1sts  = Id processo?
017400111027     d   §CM1idp     e                     inz('2')
017500111111
017600111111       // -?Dati estratti via SQL?
017700111111     d wWESrrn         s             11  0 inz
017800111111     d WFESVds       e ds                  inz  extname(WFESV00F)
017900111111     d                                          qualified
018000111111     d wORGde0         s                   inz  like(AzOrg00F.ORGde0)
018100111111
018200111111       // -?140ª descrizione dell'organigramma?
018300111111     d Og140         e ds                  inz  qualified
018400111111     d   §ogAPO      e                     inz(c_Sede)
018500111027
018600111027       //--------------------------------------------------------------
018700111027       //?Definizione variabili globali.                               ?
018800111027       //--------------------------------------------------------------
018900111027
019000111027       // -?Flags booleani?
019100111027     d $Invio          s               n   inz
019200111111     d $EoF            s               n   inz
019300111107
019400111107       // -?Indici di schiera?
019500111107     d wMsgErr         s              3  0 inz
019600111107     d xx              s              3  0 inz
019700111027
019800111027       // -?Campi di stampa?
019900111118     d wDest_eMail     s             30    inz
020000111108     d O_Testo         s            158    inz
020100111111
020200111111       // -?Stringa SQL da eseguire?
020300111111     d wSQL            s           2048    inz  Varying
020400111107
020500111111       // -?Campi per controllo "rotture"?
020600111111     d SaveAPO         s                   inz  like(Og140.§ogAPO)
020700111114     d SaveKSC         s                   inz  like(WFESVds.WESksc)
020800111114     d SaveEDAT        s                   inz  like(WFESVds.WESdt9)
020900111111
021000111107       // -?Campi di comodo?
021100111107     d wData_Eur       s               d   inz  datfmt(*eur)
021200111027
021300111027       //--------------------------------------------------------------
021400111108       //?Definizione prototipi procedure usate.                       ?
021500111027       //--------------------------------------------------------------
021600111027
021700111027       // -?Reperimento dati utente?
021800111027     d TIBS34ds      e ds                  inz
021900111027      /copy gaitrasrc/srcProtoPr,TIBS34R
022000111107
022100111111
022200111111       // ·?Versione Cappario in elaborazione?
022300111111     d VersioneCappario...
022400111111     d                 s                   like(cndizion.$Ver)
022500111111       // ·?Versione Cappario ad oggi?
022600111111     d VersioneCappario0...
022700111111     d                 s                   like(cndizion.$Ver)
022800111111       // ·?Versione Cappario a ieri?
022900111111     d VersioneCappario1...
023000111111     d                 s                   like(cndizion.$Ver)
023100111111       // ·?Versione Cappario ad un altro giorno?
023200111111     d VersioneCappario2...
023300111111     d                 s                   like(cndizion.$Ver)
023400111107       // -?Reperimento versione cappario in vigore?
023500111107      /copy gaitrasrc/srcProtoPr,TISI96R
023600111114
023700111114       // -?Invio *msg per gestione allocazione record?
023800111114     d TRUL82ds      e ds                  inz
023900111114     d  UL82§fil     e                     inz('WFESV00F  ')
024000111114     d  UL82§win     e                     inz('N')
024100111114     d  UL82§f5      e                     inz('N')
024200111114     d  UL82§f7      e                     inz('S')
024300111114     d  UL82§num     e                     inz(05)
024400111114     d  UL82§att     e                     inz(60)
024500111114     d trul82r         pr                  extpgm('TRUL82R')
024600111114     d   trul82ds                          likeds(TRUL82ds)
024700111027
024800111107       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
024900111107     d c_TABEL         c                   const('1')
025000111107       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
025100111107     d c_TNTBE         c                   const('2')
025200111107       // -?Reperimento dati tabelle?
025300111107      /copy gaitrasrc/srcProtoPR,TRULTAB
025400111027
025500111027       // -?Parametri API QCAPCMD (Process Commands)?
025600111027     d Qcmd            s           4096    inz  varying
025700111027      /copy qSysInc/qRpgleSrc,QCAPCMD
025800111027       // -?API QCAPCMD (Process Commands)?
025900111027      /copy gaitrasrc/srcProtoPR,QCAPCMD
026000111027
026100111027       // -?Parametri gestione errori API.?
026200111027      /copy qsysinc/qRpgleSrc,QUSEC
026300111027
026400111027       //--------------------------------------------------------------
026500111027       //?Definizione key-list.                                        ?
026600111027       //--------------------------------------------------------------
026700111027
026800111027
026900111027       //--------------------------------------------------------------
027000111027       //?Riepilogo indicatori.                                        ?
027100111027       //--------------------------------------------------------------
027200111027       //--------------------------------------------------------------
027300111027
027400111027       //--------------------------------------------------------------
027500111027       //?M A I N - L I N E                                            ?
027600111027       //--------------------------------------------------------------
027700111027
027800111027     c     *Entry        plist
027900111027     c                   parm                    KPJBA
028000111027
028100111027      /free
028200111027
028300111027       // -?Operazioni iniziali?
028400111027       exsr  sr_RoutInz;
028500111027
028600111027       // -?Ciclo di lettura work-file WFESV00F?
028700111111       dow  NOT  $EoF;
028800111027         exsr  sr_Elab_WFESV;
028900111111         exsr  sr_ReadCursor;
029000111027       enddo;
029100111027
029200111027       // -?Operazioni finali?
029300111027       exsr  sr_RoutEnd;
029400111027
029500111027       //--------------------------------------------------------------
029600111027       //?Operazioni iniziali.                                         ?
029700111027       //--------------------------------------------------------------
029800111027       BEGSR  sr_RoutInz;
029900111027
030000111027         *inLR = *on;
030100111111
030200111111         // -?Impostazione opzioni per SQL?
030300111111         exec sql   set  option  DynUsrPrf = *Owner,
030400111111                                 CloSqlCsr = *EndMod;
030500111027
030600111027         // -?Reperimento dati job?
030700111108         exsr  sr_DatiJob;
030800111107
030900111107         // -?Reperimento versione Cappario in vigore ad oggi e a ieri?
031000111107         GetVersioneCappario ( %dec( %date() : *ISO ) :
031100111107                               VersioneCappario0 );
031200111107         GetVersioneCappario ( %dec( %date() - %days(1) : *ISO ) :
031300111107                               VersioneCappario1 );
031400111027
031500111027         // -?Reperimento tab. "MRA"?
031600111107         ds_TNTBE.TBEke1 = SDSpgm;
031700111107         if  getTabella ( c_Tntbe : 'MRA' : ds_TNTBE.TBEke1 :
031800111107                          'DAN' : *omit : *omit :
031900111107                          *omit : *omit :
032000111107                          *omit : *omit : *omit : *omit :
032100111107                          *omit : *omit : *omit : *omit :
032200111107                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
032300111107                        = *zero      AND
032400111107             ds_TNTBE.TBEatb = *blank;
032500111107           dMRAdan = ds_TNTBE.TBEuni;
032600111107         endif;
032700111111
032800111111         // -?Apertura cursore SQL?
032900111111         exsr  sr_OpenCursor;
033000111111
033100111111         // -?1ª lettura WFESV00F via SQL?
033200111111         exsr  sr_ReadCursor;
033300111027
033400111027       ENDSR;
033500111027
033600111027       //--------------------------------------------------------------
033700111027       //?Reperimento Dati del job (Utente/Operativi).                 ?
033800111027       //--------------------------------------------------------------
033900111108       BEGSR  sr_DatiJob;
034000111027
034100111027         in(E) §AzUte;
034200111027         if NOT %error;
034300111027           in(E) §DatiUte;
034400111027         endif;
034500111027         if %error or RSut = *blanks;
034600111027           clear TIBS34ds;
034700111027           tibs34r(tibs34ds);
034800111027           in §AzUte;
034900111027           in §DatiUte;
035000111027         endif;
035100111027
035200111027       ENDSR;
035300111111
035400111111       //--------------------------------------------------------------
035500111111       //?Apertura cursore C1.                                         ?
035600111111       //--------------------------------------------------------------
035700111111       BEGSR  sr_OpenCursor;
035800111111
035900111111         // -?Preparazione stringa SQL per lettura file WFESV00F?
036000111111         wSQL  = 'select rrn(WFESV00F), WFESV00F.*, ORGde0 +
036100111111                    from WFESV00F left outer join AZORG00F +
036200111111                      on int(substr(digits(WESksc), 1, 3)) = ORGfil';
036300111111         //?· selezione dei soli errori NON bloccati?
036400111111         wSQL += ' where WESbloc = '' ''';
036500111111         //?· selezione dei soli errori per i quali è previsto l'invio?
036600111111         //  ?e-mail?
036700111114         clear  xx;
036800111114         for  wMsgErr = 1  To  %elem(sch_DatiErr);
036900111114           ds_DatiErr = sch_DatiErr(wMsgErr);
037000111114           if  ds_DatiErr.DE_SNmail <> 'S';
037100111114             xx = wMsgErr;
037200111114             leave;
037300111114           endif;
037400111114         endfor;
037500111114         If  xx > *zero;
037600111114           clear  xx;
037700111114           wSQL +=   ' and WESerr NOT in (';
037800111114           For  wMsgErr = 1  To  %elem(sch_DatiErr);
037900111114             ds_DatiErr = sch_DatiErr(wMsgErr);
038000111114             if  ds_DatiErr.DE_SNmail <> 'S';
038100111114               if  xx > *zero;
038200111114                 wSQL += ', ';
038300111114               endif;
038400111114               wSQL += '''' + sch_Err(wMsgErr) + '''';
038500111114               xx   += 1;
038600111114             endif;
038700111114           EndFor;
038800111114           wSQL +=   ')';
038900111114         EndIf;
039000111111         //?· selezione dei soli errori per i quali risulta inviabile?
039100111111         //  ?la e-mail - anche se NON è detto lo sarà - con?
039200111111         //  ?tot. giorni di occorrenza > num. solleciti già inviati?
039300111122         wSQL += ' and WEStgo > WESnsol';
039400111111         // ·?Ordinamento per Area-Poc / Cod.Cliente?
039500111111         wSQL += ' order by substr(ORGde0, 7, 3), WESksc';
039600111111
039700111111         // -?Dichiarazione cursore?
039800111111         exec sql   prepare S1   from :wSQL;
039900111111         exec sql   declare C1   cursor for S1;
040000111111
040100111111         // -?Apertura del cursore?
040200111111         exec sql   open C1;
040300111111
040400111111         if  SQLcode < *zero;
040500111111           $EoF = *on;
040600111114           exsr  sr_PrintErr;
040700111111         endif;
040800111111
040900111111       ENDSR;
041000111111
041100111111       //--------------------------------------------------------------
041200111111       //?Lettura cursore C1.                                          ?
041300111111       //--------------------------------------------------------------
041400111111       BEGSR  sr_ReadCursor;
041500111111
041600111111         clear  WFESVds;
041700111111
041800111111         exec sql   fetch next   from C1
041900111111                    into :wWESrrn, :WFESVds, :wORGde0;
042000111111
042100111111         select;
042200111111           when  SQLcode = 100;
042300111111             $EoF = *on;
042400111111           when  SQLcode < *zero;
042500111111             $EoF = *on;
042600111114             exsr  sr_PrintErr;
042700111111         endsl;
042800111111
042900111111       ENDSR;
043000111111
043100111111       //--------------------------------------------------------------
043200111111       //?Chiusura cursore C1.                                         ?
043300111111       //--------------------------------------------------------------
043400111111       BEGSR  sr_CloseCursor;
043500111111
043600111111         // -?Chiusura del cursore?
043700111111         exec sql   close C1;
043800111111
043900111111       ENDSR;
044000111027
044100111027       //--------------------------------------------------------------
044200111027       //?Elaborazione singolo record dal file WFESV00F                ?
044300111027       //--------------------------------------------------------------
044400111027       BEGSR  sr_Elab_WFESV;
044500111027
044600111027         // -?Selezione record?
044700111114         wMsgErr = %lookup( WFESVds.WESerr : sch_Err );
044800111031
044900111107         if  wMsgErr = *zero;
045000111111           // -?Codice errore non previsto?
045100111115           //  ?(impossibile, vista la selezione operata via SQL)?
045200111111           clear  ds_DatiErr;
045300111111           leavesr;
045400111111         else;
045500111111           // -?Codice errore previsto?
045600111111           ds_DatiErr = sch_DatiErr(wMsgErr);
045700111031         endif;
045800111031
045900111031         Select;
046000111031           // -?Gravità (?)?
046100111107           //When  ds_DatiErr.DE_Gravita < 10 ???
046200111031           //  leavesr;
046300111031           // -?Non prevista e-mail?
046400111111           //  ?(impossibile, vista la selezione operata via SQL)?
046500111111           //When  ds_DatiErr.DE_SNmail = 'N';
046600111111           //  leavesr;
046700111031           // -?Numero giorni limite non ancora raggiunto?
046800111121           //When  ds_DatiErr.DE_Tempist > WFESVds.WEStgo /
046900111121           //                              ( WFESVds.WESnsol + 1 );
047000111121           //  leavesr;
047100111125           //When  ( WFESVds.WESdusol > *zero  and
047200111125           //        WFESVds.WESdt9   <
047300111125           //          %dec( %date( WFESVds.WESdusol : *iso ) +
047400111125           //                %days( ds_DatiErr.DE_Tempist ) ) )  OR
047500111125           //      ( WFESVds.WESdusol     <= *zero  and
047600111125           //        ds_DatiErr.DE_Tempist > WFESVds.WEStgo /
047700111125           //                              ( WFESVds.WESnsol + 1 ) );
047800111125           //  leavesr;
047900111125           When  %dec( %date( WFESVds.WESdt9 : *iso ) -
048000111125                       %days( ds_DatiErr.DE_Tempist ) )  <
048100111125                 WFESVds.WESdusol;
048200111121             leavesr;
048300111031         EndSl;
048400111027
048500111111         // -?"Rottura" Area Poc:?
048600111111         Og140 = wORGde0;
048700111111         IF  Og140.§ogAPO <> SaveAPO;
048800111107
048900111111           // ·?invio e-mail al Poc (area precedente)?
049000111109           if  %open(PrtEMAIL);
049100111109             exsr  sr_Send_eMail;
049200111109           endif;
049300111107
049400111111           // ·?impostazione nuova Area Poc?
049500111111           SaveAPO = Og140.§ogAPO;
049600111111           if  Og140.§ogAPO > *zero;
049700170810             //*·eval   wDest_eMail = 'Poc' + Og140.§ogAPO + c_AtBrt;
049800170810             eval   wDest_eMail = 'Ced' + Og140.§ogAPO + c_AtBrt;
049900111111           else;
050000111115             eval   wDest_eMail = 'ced' + c_AtBrt;
050100111111           endif;
050200111107
050300111107         ENDIF;
050400111114
050500111114         // -?Allocazione rec. WFESV00F?
050600111114         clear  xx;
050700111114         DoU  %error;
050800111114
050900111114           chain(E)  wWESrrn  WFESV000;
051000111114
051100111114           select;
051200111114             // -?Record NON trovato?
051300111114             //  ?(appena cancellato da un utente?)?
051400111114             when  Not  %found(WFESV00F);
051500111114               leavesr;
051600111114             // -?Record allocato da altro lavoro?
051700111114             when  %error;
051800111114               if  xx = *zero;
051900111114                 // ·?Invio di un avviso a video?
052000111114                 xx += 1;
052100111114                 reset  TRUL82ds;
052200111114                 UL82§rrn = wWESrrn;
052300111114                 UL82§mss = c_Msg_01;
052400111114                 TRUL82R ( TRUL82ds );
052500111114               else;
052600111114                 // ·?Uscita (ci si riproverà domani...)?
052700111114                 leavesr;
052800111114               endif;
052900111114             // -?Ok?
053000111114             other;
053100111114               leave;
053200111114           endsl;
053300111114
053400111114         EndDo;
053500111027
053600111103         // -?Preparazione singola e-mail?
053700111103         exsr  sr_Prep_eMail;
053800111031
053900111031         // -?Aggiornamento file WFESV00F?
054000111031         exsr  sr_Upd_WFESV;
054100111027
054200111027       ENDSR;
054300111027
054400111027       //--------------------------------------------------------------
054500111103       //?Preparazione singola e-mail.                                 ?
054600111027       //--------------------------------------------------------------
054700111103       BEGSR  sr_Prep_eMail;
054800111027
054900111027         // -?Override al file di stampa ed apertura dello stesso?
055000111027         if  NOT  %open(PrtEMAIL);
055100111027           exsr  sr_Open_PrtF;
055200111027         endif;
055300111027
055400111108         // -?Stampa riga vuota (a separazione tra i clienti)?
055500111108         //  ?(in realtà conviene stampare un carattere qualsiasi,?
055600111108         //   ?perchè se fosse la 66ª ed ultima riga della pagina?
055700111108         //   ?NON verrebbe riportata nello spool inviato via?
055800111108         //   ?e-mail...)?
055900111114         if  WFESVds.WESksc <> SaveKSC;
056000111114           SaveKSC = WFESVds.WESksc;
056100111114           O_Testo = c_SepLine1;
056200111114           except  PrtDet;
056300111114           O_Testo = c_SepLine2;
056400111109         else;
056500111109           O_Testo = ' + ';
056600111108         endif;
056700111109         except  PrtDet;
056800111031
056900111107         // -?Stampa segnalazione?
057000111114         O_Testo = WFESVds.WESmsg;
057100111027         except  PrtDet;
057200111111         if  %subst( ds_DatiErr.DE_TxtAggior : 1: 1 ) <> *blank;
057300111111           O_Testo = ds_DatiErr.DE_TxtAggior;
057400111111           except  PrtDet;
057500111111         endif;
057600111107
057700111107         // -?Stampa (dopo il messaggio di errore) i dati del record?
057800111107         //  ?in elaborazione del file WFESV00F?
057900111107         exsr  sr_Print_Dati;
058000111108
058100111108         // -?Stampa (dopo il messaggio di errore) la descrizione?
058200111108         //  ?aggiuntiva per le azioni da intraprendere?
058300111108         exsr  sr_Print_Azione;
058400111027
058500111027       ENDSR;
058600111107
058700111107       //--------------------------------------------------------------
058800111107       //?Stampa ultimi dati errati nel record del file WFESV          ?
058900111107       //--------------------------------------------------------------
059000111107       BEGSR  sr_Print_Dati;
059100111107
059200111122         O_Testo = '·';
059300111122         //O_Testo = c_SepPoint;
059400111122         except PrtDet;
059500111111
059600111117         //wData_EUR = %date( WFESVds.WESdt1 : *iso );
059700111117         //O_Testo = '- Data 1ª occorrenza: '
059800111117         //        + %editc( %dec( wData_EUR ) : 'Y' );
059900111117         //wData_EUR = %date( WFESVds.WESdt9 : *iso );
060000111117         //O_Testo = %trimr( O_Testo )
060100111117         //        + '   Data ultima occorrenza: '
060200111117         //        + %editc( %dec( wData_EUR ) : 'Y' );
060300111117         //O_Testo = %trimr( O_Testo )
060400111117         //        + '   Tot. occorrenze: '
060500111117         //        + %char( WFESVds.WEStgo );
060600111117         //except  PrtDet;
060700111122
060800111122         wData_EUR = %date( WFESVds.WESdt9 : *iso );
060900111122         O_Testo = 'Dati ricevuti dal cliente in data '
061000111122                 + %editc( %dec( wData_EUR ) : 'Y' )
061100111122                 + ':';
061200111122         except PrtDet;
061300111107
061400111107         O_Testo = 'Versione EasySped: '
061500111114                 + WFESVds.WESever
061600111117                 + '   Versione cappario in uso: '
061700111114                 + %editc( WFESVds.WEScver : '3' )
061800111117                 + '   Scadenza: '
061900111114                 + %editw( WFESVds.WEScdsc : '    /  /0 ' );
062000111109         except PrtDet;
062100111109
062200111109         O_Testo = 'Range segnacolli: '
062300111114                 + %editc( WFESVds.WESsncd : '3' )
062400111107                 + '-'
062500111114                 + %editc( WFESVds.WESsnca : '3' )
062600111122                 + '    Segnacollo corrente: '
062700111114                 + %editc( WFESVds.WESsncc : '3' )
062800111122                 + '      Serie: '
062900111114                 + WFESVds.WESnrs;
063000111107         except PrtDet;
063100111107
063200111114         if  WFESVds.WEScvermax <> *zero   or
063300111114             WFESVds.WEScddemax <> *zero   or
063400111114             WFESVds.WEScdscmax <> *zero;
063500111108           O_Testo = 'Versione massima cappario: '
063600111117                   + %editc( WFESVds.WEScvermax : '3' )
063700111122                   + '     Decorrenza: '
063800111117                   + %editw( WFESVds.WEScddemax : '    /  /0 ' )
063900111122                   + '            Scadenza: '
064000111114                   + %editw( WFESVds.WEScdscmax : '    /  /0 ' );
064100111107           except PrtDet;
064200111107         endif;
064300111107
064400111107       ENDSR;
064500111108
064600111108       //--------------------------------------------------------------
064700111108       //?Stampa descrizione aggiuntiva delle azioni da intraprendere  ?
064800111108       //--------------------------------------------------------------
064900111108       BEGSR  sr_Print_Azione;
065000111110
065100111117         xx = ( ( wMsgErr - 1 ) * c_ErrLines ) + 1;
065200111117
065300111110         // -?Nessuna azione prevista...?
065400111117         if  sch_Msg2( xx ) = *blank;
065500111110           leavesr;
065600111110         endif;
065700111108
065800111121         // -?Stampa separazione?
065900111122         O_Testo = '·';
066000111122         //O_Testo = c_SepPoint;
066100111108         except PrtDet;
066200111108
066300111117         // -?Stampa 1ª descrizione?
066400111117         O_Testo = sch_Msg2( xx ) + sch_Msg2( xx + 1 );
066500111109         except PrtDet;
066600111117
066700111122         // -?Stampa 2ª descrizione (se c'è)?
066800111122         if  sch_Msg2( xx + 2 ) <> *blank;
066900111122           O_Testo = sch_Msg2( xx + 2 ) + sch_Msg2( xx + 3 );
067000111122           except PrtDet;
067100111122         endif;
067200111122
067300111122         // -?Stampa n° sollecito?
067400111122         O_Testo = '- ' + %char( WFESVds.WESnsol + 1 ) + '° sollecito.';
067500111122         except  PrtDet;
067600111108
067700111108       ENDSR;
067800111107
067900111107       //--------------------------------------------------------------
068000111107       //?Reperimento Versione Cappario in vigore il giorno della sped.?
068100111107       //--------------------------------------------------------------
068200111107       BEGSR  sr_RtvVersioneCappario;
068300111107
068400111107         select;
068500111107
068600111107           // -?Spedizione di oggi?
068700111114           when  WFESVds.WESdt9 = %dec( %date() : *ISO );
068800111107             VersioneCappario = VersioneCappario0;
068900111107
069000111107           // -?Spedizione di ieri?
069100111114           when  WFESVds.WESdt9 = %dec( %date() - %days(1) : *ISO );
069200111107             VersioneCappario = VersioneCappario1;
069300111107
069400111107           // -?Spedizione dell'ultimo giorno memorizzato?
069500111114           when  WFESVds.WESdt9 = SaveEDAT;
069600111107             VersioneCappario = VersioneCappario2;
069700111107
069800111107           // -?Spedizione di un altro giorno?
069900111107           other;
070000111114             SaveEDAT = WFESVds.WESdt9;
070100111114             GetVersioneCappario ( WFESVds.WESdt9 : VersioneCappario2 );
070200111107             VersioneCappario = VersioneCappario2;
070300111107
070400111107         endsl;
070500111107
070600111107       ENDSR;
070700111027
070800111027       //--------------------------------------------------------------
070900111027       //?Apertura file di stampa per invio e-mail                     ?
071000111027       //--------------------------------------------------------------
071100111027       BEGSR  sr_Open_PrtF;
071200111027
071300111027         // -?Override al file di stampa per impostarvi i dati per?
071400111027         //  ?l'invio via e-mail?
071500111107         exsr  sr_Override;
071600111027
071700111107         open  PrtEMAIL;
071800111027
071900111027         // -?Stampa una testata se NON è richiesta la e-mail?
072000111027         IF  §MRAdreg =  *blank;
072100111108           O_Testo = JobUser + ' - ' + SDSpgm
072200111027                   + ' - ' + %editc( *date : 'Y' )
072300111107                   + ' - *REM* ' + %trimr( %subst(§CM1var : 7 : 70) );
072400111107           except  PrtDet;
072500111108           clear  O_Testo;
072600111107           except  PrtDet;
072700111111           O_Testo = ' - Area POC ' + Og140.§ogAPO;
072800111107           except  PrtDet;
072900111108           clear  O_Testo;
073000111107           except  PrtDet;
073100111027         ENDIF;
073200111027
073300111027         // -?Stampa testo iniziale?
073400111111         O_Testo = c_Titolo;
073500111107         except  PrtDet;
073600111027
073700111027       ENDSR;
073800111027
073900111027       //--------------------------------------------------------------
074000111027       //?Override al file di stampa per impostarvi i dati per         ?
074100111027       //?  l'invio via e-mail.                                        ?
074200111027       //--------------------------------------------------------------
074300111027       BEGSR  sr_Override;
074400111027
074500111107         reset  $Invio;
074600111107         reset  TRTCM1ds;
074700111027
074800111027         IF  §MRAdreg <> *blank;
074900111027           §CM1mitt = %trim(§MRAdmitt);
075000111110           if  §MRAddest = *blank;
075100111110             §CM1dst  = wDest_eMail;
075200111110           else;
075300111110             §CM1dst  = §MRAddest;
075400111110           endif;
075500111027           §CM1tips = §MRAdreg;
075600111115           //§CM1po   = Og140.§OGapo;
075700111115           §CM1po   = c_Sede;
075800111027           §CM1var  = '*OBJM*' + §MRAddes;
075900111027           §CM1idp  = §MRAdidpro;
076000111111           Qcmd = c_CmdOvrPrtF + ' usrdta(''ErESYSP' + SaveAPO + ''')'
076100111027                + ' outq(' + %trim(§MRAdoutqi) + ')'
076200111027                + ' usrdfndta(''' + TRTCM1ds + ''')';
076300111027         ELSE;
076400111111           Qcmd = c_CmdOvrPrtF + ' usrdta(''ErrESYSP' + SaveAPO + ''')';
076500111027         ENDIF;
076600111027
076700111027         exsr  sr_ExecCmd;
076800111107         if  §MRAdreg <> *blank  and  Qusei = *blank;
076900111027           $Invio = *on;
077000111107         endif;
077100111027
077200111027       ENDSR;
077300111107
077400111107       //--------------------------------------------------------------
077500111107       //?Invio singola e-mail.                                        ?
077600111107       //--------------------------------------------------------------
077700111107       BEGSR  sr_Send_eMail;
077800111110
077900111110         // -?Stampa "Fine Lista"?
078000111110         clear O_testo;
078100111110         except PrtDet;
078200111110         O_testo = '***   Fine Lista   ***';
078300111110         except PrtDet;
078400111107
078500111107         // -?Chiusura dello spool ed eliminazione override?
078600111107         //  ?(a rottura di Filiale-Poc)?
078700111107         close  PrtEMAIL;
078800111107         Qcmd = c_CmdDltOvr;
078900111107         exsr  sr_ExecCmd;
079000111107
079100111107       ENDSR;
079200111031
079300111031       //--------------------------------------------------------------
079400111031       //?Aggiornamento work-file WFESV00F.                            ?
079500111031       //--------------------------------------------------------------
079600111031       BEGSR  sr_Upd_WFESV;
079700111031
079800120705         if  WESnsol < *hival;
079900120705           WESnsol += 1;
080000120705         endif;
080100111125         //WESdusol = %dec( %date() : *ISO );
080200111125         WESdusol = WESdt9;
080300111114         WESapo   = %int( Og140.§ogAPO );
080400111031
080500111031         //_______________
080600111031         update  WFESV000;
080700111031         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080800111031
080900111031       ENDSR;
081000111111
081100111111       //--------------------------------------------------------------
081200111111       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
081300111111       //--------------------------------------------------------------
081400111114       BEGSR  sr_PrintErr;
081500111111
081600111111         // -?Stampa del Dump?
081700111111         Dump(A);
081800111111
081900111111         // -?Stampa del Job-Log?
082000111111         Qcmd = 'DSPJOBLOG job(*) output(*print)';
082100111111         exsr  sr_ExecCmd;
082200111111
082300111111         // -?Chiusura del programma?
082400111111         exsr  sr_RoutEnd;
082500111111
082600111111       ENDSR;
082700111027
082800111027       //--------------------------------------------------------------
082900111027       //?Esecuzione del comando (già impostato).                      ?
083000111027       //--------------------------------------------------------------
083100111027       BEGSR  sr_ExecCmd;
083200111027
083300111107         clear  Qcap0100;
083400111027         Qcabcsdh = *off;
083500111027         Qcapa    = *off;
083600111027         Qcacmdss = *off;
083700111027         Qcaerved = *allX'00';
083800111027
083900111107         clear  Qusec;
084000111027         Qusbprv  = %size(Qusec);
084100111027
084200111027         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084300111027                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
084400111027                           0 : 0 : Qusec);
084500111027
084600111027         // -?Invio *msg per segnalazione errore?
084700111027         //if  Qusei <> *blank;
084800111027         //  ...;
084900111027         //endif;
085000111027
085100111027       ENDSR;
085200111027
085300111027       //--------------------------------------------------------------
085400111027       //?Operazioni finali.                                           ?
085500111027       //--------------------------------------------------------------
085600111027       BEGSR  sr_RoutEnd;
085700111111
085800111111         // -?Chiusura cursore SQL?
085900111111         exsr  sr_CloseCursor;
086000111109
086100111109         // -?Invio e-mail all'ultimo Poc?
086200111109         if  %open(PrtEMAIL);
086300111109           exsr  sr_Send_eMail;
086400111109         endif;
086500111027
086600111027         // -?Chiusura applicazioni?
086700111107         cloTabella ( c_Tntbe );
086800111027
086900111027         // -?Uscita dal *pgm?
087000111027         return;
087100111027
087200111027       ENDSR;
087300111027
087400111027      /end-free
087500111107
087600111107       //--------------------------------------------------------------
087700111107       //?Spool di stampa (per e-mail).                                ?
087800111107       //--------------------------------------------------------------
087900111107
088000111107     oPrtEMAIL  e            PRTdet      1
088100111108     o                       O_Testo
088200111027
088300111027       //--------------------------------------------------------------
088400111027       //?Schiere a tempo di compilazione.                             ?
088500111027       //--------------------------------------------------------------
088600111027
088700111117** -?sch_TypWeb / sch_TypWebD?-*
0888001111070  - Funzionalità NON attiva
0889001111071  - Funzionalità attiva
0890001111072  - Framework 1.0
0891001111074  - Framework 1.1
0892001111076  - Framework 1.1 & 1.0
0893001111078  - Framework 2.0
08940011110710 - Framework 2.0 & 1.0
08950011110712 - Framework 2.0 & 1.1
089600111121** -?sch_Err / sch_DatiErr?-?Cod. ERRORE  &  dati: Gravità - S/N eMail - Nr gg limite - Nr gg x CED?
08970011112101 99-S-001-001»  I dati in esame, intanto, sono stati cancellati.
08980011111702 99-S-001-001·  I dati in esame sono comunque stati memorizzati.
08990011112103 99-S-001-001»  I dati in esame verranno rielaborati domani.
09000011112104 99-S-001-001»  I dati in esame verranno rielaborati domani.
09010011111805 99-S-001-001·  I dati in esame sono comunque stati memorizzati.
090200111117** -?sch_Msg2?-?Descrizione aggiuntiva delle azioni da intraprendere?7....+..|.8
090300111121- AZIONE DA INTRAPRENDERE: Se cliente da abilitare avvisare CED fornendo le oppo   1  1.a
090400111121rtune informazioni.                                                                2  1.b
090500111117Altimenti si tratta di un parametro errato in EasySped, modificare le impostazio   3  1.c
090600111117ni del programma sul pc cliente.                                                   4  1.d
090700111121- AZIONE DA INTRAPRENDERE: Disinstallare EasySped DE oppure avvisare CED per far   5  2.a
090800111121 disabilitare Easysped WEB.                                                        6  2.b
090900111121                                                                                   7  2.c
091000111121                                                                                   8  2.d
091100111121- AZIONE DA INTRAPRENDERE: Avvisare CED per far impostare l'oppurtuno supporto d   9  3.a
091200111121i scambio dati.                                                                   10  3.b
091300111117                                                                                  11  3.c
091400111117                                                                                  12  3.d
091500111121- AZIONE DA INTRAPRENDERE: Occorre modificare la serie inserita sul PC del clien  13  4.a
091600111121te.                                                                               14  4.b
091700111118                                                                                  15  4.c
091800111121                                                                                  16  4.d
091900111121- AZIONE DA INTRAPRENDERE: AGGIORNARE IL CAPPARIO.                                17  5.a
092000111118                                                                                  18  5.b
092100111118                                                                                  19  5.c
092200111118                                                                                  20  5.d
