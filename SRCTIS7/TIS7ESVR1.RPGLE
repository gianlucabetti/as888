000100111012       //==============================================================
000200090604       //?Memorizzazione Versioni Easy-Sped in tab. "3CE"              ?
000300111012       //==============================================================
000400111012       //?N.B.?- Schedulato!
000500111012       //       ?Compilare da s.i. di sede                             ?
000600090604       //--------------------------------------------------------------
000700111012
000800111012       //--------------------------------------------------------------
000900111012       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001000111012       //--------------------------------------------------------------
001100111012
001200111012     /*PRM dbgview(*source)
001300111012     /*END
001400111012
001500111012       //--------------------------------------------------------------
001600111012       //?Specifiche di controllo.                                     ?
001700111012       //--------------------------------------------------------------
001800080520
001900080520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000090604     h dftactgrp(*no)
002100080520
002200090604       //--------------------------------------------------------------
002300090604       //?Dichiarazione file.                                          ?
002400090604       //--------------------------------------------------------------
002500080520
002600111012       // -?Tabella "3C"?
002700080520     fTABEL00F  if   e           k disk
002800111206
002900111206       // -?VAS - Easy Sped - Versioni?
003000111206     fTIESV01L  Uf   e           k disk
003100111104
003200111104       // -?Tabella "3CE"?
003300111104     fTNTBE01L  Uf A e           k disk
003400111012
003500111012       // -?Work-File errori?
003600111012     fWFESV01L  Uf A e           k disk
003700080520
003800111012       // -?Stampa errori (e-mail)?
003900111020     fPrtEMAIL  o    f  158        printer  oflind(*inOF)  usropn
004000080520
004100090604       //--------------------------------------------------------------
004200090604       //?Definizione costanti.                                        ?
004300090604       //--------------------------------------------------------------
004400090604
004500111012       // -?Tabella in gestione?
004600080523     d c_Tab           c                   const('3CE')
004700080520
004800111012       // -?Dati di default?
004900090604     d c_Sede          c                   const('046')
005000080520
005100111012       // -?Comando di override al PrtF?
005200090604     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005300080520     d                                           file(PRTEMAIL) +
005400111020     d                                           pagesize(66 158) +
005500111020     d                                           lpi(6) cpi(12) +
005600080520     d                                           ovrscope(*actgrpdfn) +
005700080520     d                                           ')
005800090604     d c_CmdDltOvr     c                   const('DLTOVR +
005900080520     d                                            file(PRTEMAIL) +
006000080520     d                                            lvl(*actgrpdfn)')
006100090604
006200111012       // -?Costante per controllo "caratteri solo numerici"?
006300090604     d c_Digits        c                   const('0123456789')
006400090604
006500111012       // -?Costante con la versione di Easy-Sped che prevede controlli?
006600111012       //  ?sul Tipo Aggiornamento Web?
006700090622     d c_VersEsySp     c                   const('4.0.4 ')
006800111115       // -?Costante con la versione di Easy-Sped che NON prevede più?
006900111115       //  ?controlli sul Corpo eMail?
007000100129     d c_VersEsySp2    c                   const('4.0.7 ')
007100111020
007200111020       // -?Intestazione lista di stampa?
007300111020     d c_Titolo        c                   const('Elenco anomalie rile-
007400111020     d                                     vate nel file "Versioni Eas-
007500111020     d                                     ySped" (TIESV00F) per l''ag-
007600111020     d                                     giornamento della tab. "3CE-
007700111020     d                                     " (Dati EasySped per client-
007800111020     d                                     e)')
007900111020
008000111020       // -?Effetto sui dati?
008100111020     d c_UpdTab3CE     c                   const(' Dati aggiornati in -
008200111020     d                                     tab. "3CE".')
008300111020     d c_DltTIESV      c                   const(' Dati cancellati da -
008400111020     d                                     TIESV00F.')
008500111020     d c_NoUpdNoDlt    c                   const(' Dati né aggiornati -
008600111020     d                                     in "3CE" né cancellati da T-
008700111020     d                                     IESV00F.')
008800080520
008900090604       //--------------------------------------------------------------
009000090604       //?Definizione schiere.                                         ?
009100090604       //--------------------------------------------------------------
009200090604
009300111012       // -?Tipi Aggiornamento Web?
009400111213     d sk_TypWeb       s              2    dim(08)  ctdata  perrcd(1)
009500111213     d sk_TypWebD      s             30    dim(08)  alt(sk_TypWeb)
009600111020
009700111020       // -?Messaggi di Errore?
009800111117     d c_MaxErr        c                   const(05)
009900111213     d sk_Err          s              2    dim(c_MaxErr) ctdata perrcd(1)
010000111213     d sk_DatiErr      s                   like(ds_DatiErr)
010100111213     d                                     dim(c_MaxErr) alt(sk_Err)
010200111213     d sk_Msg1         s            100    dim(c_MaxErr) ctdata perrcd(1)
010300111213
010400111213       // -?Errori registrati in WFESV00F per Cliente / Data?
010500111213     d sk_Segnalaz     s               n   dim(c_MaxErr) inz
010600111213       // -?Errori segnalati al Ced per Cliente?
010700111213     d sk_eMailCed     s               n   dim(c_MaxErr) inz
010800120613       // -?Numero solleciti registrati in WFESV00F per Cliente / Data?
010900120613     d sk_nSol         s                   like(WESnsol)
011000120613     d                                     dim(c_MaxErr) inz
011100090604
011200090604       //--------------------------------------------------------------
011300090604       //?Definizione aree dati.                                       ?
011400090604       //--------------------------------------------------------------
011500080520
011600111012       // -?Dati utente?
011700080520     d §AzUte        e ds                  extname(AZUTE00F)
011800080520     d                                     dtaara
011900080520     d §DatiUte      e ds                  extname(dDatiUte)
012000080520     d                                     dtaara
012100080520
012200120316       //--------------------------------------------------------------
012300090604       //?Definizione strutture dati.                                  ?
012400120316       //--------------------------------------------------------------
012500080520
012600111012       // -?Status?
012700080520     d Psds           sds
012800080520     d   SDSpgm          *proc
012900080520     d***JobName             244    253                                         Job name
013000080520     d   JobUser             254    263                                         User name
013100080520     d***JobNumber           264    269s 0                                      Job number
013200111104
013300111104       // -?Definizione campi?
013400111104     d cnDizion      e ds                  qualified  based(nullPtr)
013500080520
013600111012       // -?Parametri?
013700080520     d KPJBA         e ds
013800080520
013900111012       // -?Reperimento dati utente?
014000080520     d TIBS34ds      e ds                  inz
014100080520
014200111012       // -?Tracciato record file TNTBE00F per dati di base?
014300080520     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014400080520     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014500080520     d                                          prefix(X)
014600080520
014700111012       // -?Tab. 3C = Clienti con dischetto?
014800080520     d ds3C          e ds                  inz
014900090615
015000111012       // -?Tab. 3CE = Dati Easy-Sped per cliente?
015100090615     d d3CE          e ds                  inz
015200120316
015300120316       // -?Tab. ESV = VAS - Easy Sped - Versioni (vedi TIESV00F.ESVFLO)?
015400120316     d dESV          e ds                  inz
015500080520
015600111012       // -?Gestione tabelle: controllo e ricerca?
015700080520     d TIBS02ds      e ds                  inz
015800080520     d   T02tla      e                     inz('L')
015900080520     d   T02mod      e                     inz('C')
016000080520     d   T02cod      e                     inz('MRA')
016100080521     d   T02ke2      e                     inz('DAN')
016200080520
016300111012       // -?Tabella "MRA" = Bart-Mailing - Danni?
016400080520     d dMRAdan       e ds                  inz
016500080520
016600111012       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
016700080520     d TRTCM1ds      e ds                  inz
016800111012       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
016900080520     d   §CM1mitt    e                     inz('ced')
017000111012       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
017100120302     d   §CM1dst     e                     inz('ced@brt.it')
017200111012       //  ?·§CM1tips = Tipo lettera via e-mail:?
017300111012       //   ?"LET" = testo allegato in corpo con logo?
017400111012       //           ?(richiede righe libere iniziali per il logo)?
017500111012       //   ?"COR" = testo integrato senza logo?
017600111012       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
017700080520     d   §CM1tips    e                     inz('COR')
017800111012       //  ?·§CM1po   = Filiale?
017900080520     d   §CM1po      e                     inz('046')
018000111012       //  ?·§CM1var  = Oggetto e-mail?
018100080520     d   §CM1var     e                     inz('*OBJM*+
018200080520     d                                     Errori rilevati in fase di +
018300080520     d                                     memorizzaz. versioni EasySp+
018400080523     d                                     ed in tab.')
018500111012       //  ?·§CM1sts  = Stato?
018600080520     d   §CM1sts     e                     inz(*off)
018700111012       //  ?·§CM1sts  = Id processo?
018800080520     d   §CM1idp     e                     inz('2')
018900111107
019000111213       // -?Dati del singolo errore in schiera sk_DatiErr?
019100111117     d ds_DatiErr      ds            98    inz   qualified
019200111107     d   DE___filler1                 1    inz
019300111107     d   DE_Gravita                   2s 0 inz
019400111107     d   DE___filler2                 1    inz
019500111107     d   DE_SNmail                    1    inz
019600111107     d   DE___filler3                 1    inz
019700111107     d   DE_Tempist                   3s 0 inz
019800111107     d   DE___filler4                 1    inz
019900111117     d   DE_TettoCED                  3s 0 inz
020000111117     d   DE___filler5                 1    inz
020100111117     d   DE_TxtAttenz                84    inz
020200080520
020300090604       //--------------------------------------------------------------
020400090604       //?Definizione variabili globali.                               ?
020500090604       //--------------------------------------------------------------
020600080520
020700111012       // -?Flags booleani?
020800111012     d $Invio          s               n   inz
020900111121     d $MsgErr         s               n   inz
021000111020     d $DltTIESV       s               n   inz
021100111012     d $UpdTab3CE      s               n   inz
021200100107
021300111012       // -?Indici di schiera?
021400111107     d wMsgErr         s              3  0 inz
021500100107     d xx              s              3  0 inz
021600111104
021700111104       // -?Versione Cappario in elaborazione?
021800111104     d VersioneCappario...
021900111104     d                 s                   like(cndizion.$Ver)
022000111104       // -?Versione Cappario ad oggi?
022100111104     d VersioneCappario0...
022200111104     d                 s                   like(cndizion.$Ver)
022300111104       // -?Versione Cappario a ieri?
022400111104     d VersioneCappario1...
022500111104     d                 s                   like(cndizion.$Ver)
022600111104       // -?Versione Cappario ad un altro giorno?
022700111104     d VersioneCappario2...
022800111104     d                 s                   like(cndizion.$Ver)
022900080520
023000111012       // -?Campi di stampa?
023100111107     d O_Testo         s            158    inz
023200111108
023300111108       // -?Campi di comodo?
023400111108     d SaveEKSC        s                   inz  like(ESVeksc)
023500111108     d SaveEDAT        s                   inz  like(ESVedat)
023600111108     d wMemoEDAT       s                   inz  like(ESVedat)
023700111121     d wData_Eur       s               d   inz  datfmt(*eur)
023800080520
023900090604       //--------------------------------------------------------------
024000111104       //?Definizione prototipi procedure usate.                       ?
024100090604       //--------------------------------------------------------------
024200080520
024300111012       // -?Reperimento dati utente?
024400090615      /copy gaitrasrc/srcProtoPr,TIBS34R
024500080520
024600111012       // -?Gestione tabelle?
024700111104      /copy gaitrasrc/srcProtoPr,TIBS02R
024800111104
024900111104       // -?Reperimento versione cappario in vigore?
025000111104      /copy gaitrasrc/srcProtoPr,TISI96R
025100111110
025200111110       // -?Cancellazione vecchi record da WFESV00F?
025300111110     d DeleteRecWFESV...
025400111110     d                 pr                  extpgm('TIS7ESVR3')
025500111122
025600111122       // -?Invio solleciti ai Poc?
025700111123     d SendSollecitiPoc...
025800111123     d                 pr                  extpgm('TIS7ESVR4')
025900111123     d   kpjba                             likeds(kpjba)
026000080520
026100111117       // -?Parametri API QCAPCMD (Process Commands)?
026200111117     d Qcmd            s            512    inz  varying
026300111117      /copy qSysInc/qRpgleSrc,QCAPCMD
026400111117       // -?API QCAPCMD (Process Commands)?
026500111117      /copy gaitrasrc/srcProtoPR,QCAPCMD
026600111117
026700111117       // -?Parametri gestione errori API.?
026800111117      /copy qsysinc/qRpgleSrc,QUSEC
026900080520
027000090604       //--------------------------------------------------------------
027100090604       //?Definizione key-list.                                        ?
027200090604       //--------------------------------------------------------------
027300111012
027400111012       // -?File TABEL00F?
027500111012     d k03tabel00    e ds                  extname(TABEL00F:*key) inz
027600080520
027700111012       // -?File TNTBE01L?
027800080520     d k05tntbe01    e ds                  extname(TNTBE01L:*key) inz
027900080520     d                                     prefix(K_)
028000111012
028100111012       // -?File TIESV01L?
028200111012     d k02tiesv01    e ds                  extname(TIESV01L:*key) inz
028300111012     d                                     prefix(K_)
028400111012
028500111012       // -?File WFESV01L?
028600111012     d k02wfesv01    e ds                  extname(WFESV01L:*key) inz
028700111012     d                                     prefix(K_)
028800080520
028900090604       //--------------------------------------------------------------
029000090604       //?Riepilogo indicatori.                                        ?
029100090604       //--------------------------------------------------------------
029200090604       //--------------------------------------------------------------
029300090604
029400090604       //--------------------------------------------------------------
029500090604       //?M A I N - L I N E                                            ?
029600090604       //--------------------------------------------------------------
029700080520
029800080520     c     *Entry        plist
029900080520     c                   parm                    KPJBA
030000080520
030100080520      /free
030200080520
030300111012       //  ?___________________?
030400111012       // -?Operazioni iniziali?
030500111012       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
030600090604       exsr  sr_RoutInz;
030700080520
030800111012       //  ?______________________________?
030900111012       // -?Ciclo di lettura file TIESV00F?
031000111012       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
031100080520       dow  NOT  %eof(TIESV01L);
031200080520         exsr  sr_Elab_TIESV;
031300080520         read  TIESV000;
031400080520       enddo;
031500080520
031600111012       //  ?_________________?
031700111012       // -?Operazioni finali?
031800111012       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
031900090604       exsr  sr_RoutEnd;
032000080520
032100080520       //--------------------------------------------------------------
032200090604       //?Operazioni iniziali.                                         ?
032300080520       //--------------------------------------------------------------
032400090604       BEGSR  sr_RoutInz;
032500080520
032600080520         *inLR = *on;
032700080520
032800111117         // -?Reperimento dati job?
032900111104         exsr  sr_DatiJob;
033000080520
033100111117         // -?Reperimento versione Cappario in vigore ad oggi e a ieri?
033200111104         GetVersioneCappario ( %dec( %date() : *ISO ) :
033300111104                               VersioneCappario0 );
033400111104         GetVersioneCappario ( %dec( %date() - %days(1) : *ISO ) :
033500111104                               VersioneCappario1 );
033600111104
033700111117         // -?Aggancio dati generali della tabella in esame?
033800080520         clear  k_TBEcod;
033900080520         k_TBEke1 = *zero;
034000080520         %subst( k_TBEke1 : %len(TBEke1) - %len(c_Tab) + 1 : %len(c_Tab) )
034100080520                = c_Tab;
034200080520         clear  k_TBEke2;
034300080520         clear  k_TBElin;
034400080520         clear  k_TBEsif;
034500080520         chain(n)  %kds(k05tntbe01)  TNTBE000;
034600080520         if  %found(TNTBE01L);
034700080520           xTNTBEds = TNTBEds;
034800080520         else;
034900080520           clear  xTNTBEds;
035000080520         endif;
035100080520
035200111117         // -?Reperimento tab. "MRA"?
035300080520         clear dMRAdan;
035400080520         clear kpjbu;
035500080520         reset tibs02ds;
035600080520         T02ke1 = SDSpgm;
035700090615         TNTBE_RicercaControllo ( kpjba : tibs02ds );
035800080520         if T02err = *blank;
035900080520           dMRAdan = T02uni;
036000080520         endif;
036100080523
036200111117         // -?Impostazione dei campi chiave?
036300111012         clear  k05tntbe01;
036400080523         k_TBEcod = c_Tab;
036500111012         clear  k03tabel00;
036600080523         TBLkut   = 1;
036700111012         TBLcod   = '3C';
036800080520
036900111117         // -?Posizionamento inziale e prima lettura del file TIESV00F?
037000080520         setll  *loval  TIESV000;
037100080520         read   TIESV000;
037200080520
037300080520       ENDSR;
037400080520
037500080520       //--------------------------------------------------------------
037600090604       //?Reperimento Dati del job (Utente/Operativi).                 ?
037700080520       //--------------------------------------------------------------
037800111104       BEGSR  sr_DatiJob;
037900080520
038000080520         in(E) §AzUte;
038100080520         if NOT %error;
038200080520           in(E) §DatiUte;
038300080520         endif;
038400080520         if %error or RSut = *blanks;
038500080520           clear TIBS34ds;
038600080520           tibs34r(tibs34ds);
038700080520           in §AzUte;
038800080520           in §DatiUte;
038900080520         endif;
039000080520
039100080520       ENDSR;
039200080520
039300080520       //--------------------------------------------------------------
039400090604       //?Elaborazione singolo record dal file TIESV00F                ?
039500080520       //--------------------------------------------------------------
039600090604       BEGSR  sr_Elab_TIESV;
039700080520
039800111020         $DltTIESV  = *off;
039900090623         $UpdTab3CE = *off;
040000080604
040100111117         // -?"Rottura" CLIENTE?
040200111118         If  k_TBEke1 <> %triml( %editc( ESVeksc : 'X' ) );
040300111117
040400111213           clear  sk_eMailCed;
040500111117           clear  wMemoEDAT;
040600111117
040700111117           // -?Reperimento tabella "3CE" per il cliente in elaborazione?
040800111117           clear  d3CE;
040900111117           k_TBEke1 = %triml( %editc( ESVeksc : 'X' ) );
041000111121           chain(n)  %kds(k05tntbe01 : 3)  TNTBE000;
041100111117           if  %found(TNTBE01L);
041200111117             d3CE = TBEuni;
041300111117           endif;
041400111117
041500111117           // -?Aggancio tabella "3C" se non reperita tab. "3CE"?
041600111117           //  ?e comunque per reperire la serie?
041700111117           clear  ds3C;
041800111117           TBLkey = k_TBEke1;
041900111117           chain  %kds(k03tabel00 : 3)  TABEL;
042000111117           if  %found(TABEL00F);
042100111117             ds3C = TBLuni;
042200111117           endif;
042300111117
042400111117         EndIf;
042500111117
042600111117         // -?"Rottura" DATA?
042700111117         If  ESVedat <> wMemoEDAT;
042800111117
042900111213           clear  sk_Segnalaz;
043000120613           clear  sk_nSol;
043100111117           wMemoEDAT  = ESVedat;
043200111117
043300111117         EndIf;
043400120316
043500120316         // -?Valorizzazione struttura dati di "Flag Operativi"?
043600120316         dESV = ESVflo;
043700080520
043800111117         // -?Controllo dei dati ricevuti?
043900090623         exsr  sr_Ctrl_Dati;
044000090623
044100111117         // -?Aggiornamento tabella "3CE" per il cliente in elaborazione?
044200111121         if  $UpdTab3CE = *on;
044300111121           k_TBEke1 = %triml( %editc( ESVeksc : 'X' ) );
044400111121           chain  %kds(k05tntbe01 : 3)  TNTBE000;
044500111121         endif;
044600111121
044700090610         Select;
044800090610
044900111012           // =>?NON?aggiorna?3CE?
045000090623           When  $UpdTab3CE = *off;
045100111118
045200111118           // =>?NON?aggiorna?3CE?
045300111118           When  %found(TNTBE01L)  and  ESVedat <= §3CEedat;
045400090604
045500111012           // =>?Aggiorna?3CE?
045600090623           When  $UpdTab3CE = *on   and   %found(TNTBE01L);
045700080523             exsr  sr_Upd_Tab3CE;
045800111020             $DltTIESV = *on;
045900090604
046000111012           // =>?Aggiunge?3CE?
046100090623           When  $UpdTab3CE = *on   and   NOT  %found(TNTBE01L);
046200080523             exsr  sr_Wrt_Tab3CE;
046300111020             $DltTIESV = *on;
046400090604
046500090610         EndSl;
046600111117
046700111117         // -?Cancellazione rec. TIESV00F in elaborazione?
046800111117         If  $DltTIESV = *on;
046900111117           //_______________
047000111117           DELETE  TIESV000;
047100111117           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047200111117         EndIf;
047300080520
047400080520       ENDSR;
047500090623
047600090623       //--------------------------------------------------------------
047700090623       //?Controlli da eseguire PRIMA dell'eventuale aggiornamento     ?
047800090623       //--------------------------------------------------------------
047900090623       BEGSR  sr_Ctrl_Dati;
048000111104
048100111104         // -?Reperimento Vers.Cappario in vigore il giorno della?
048200111104         //  ?spedizione?
048300111104         select;
048400111104           when  ESVedat = %dec( %date() : *ISO );
048500111104             VersioneCappario = VersioneCappario0;
048600111104           when  ESVedat = %dec( %date() - %days(1) : *ISO );
048700111104             VersioneCappario = VersioneCappario1;
048800111108           when  ESVedat = SaveEDAT;
048900111104             VersioneCappario = VersioneCappario2;
049000111104           other;
049100111108             SaveEDAT = ESVedat;
049200120817             GetVersioneCappario ( SaveEDAT : VersioneCappario2 );
049300120817             // -?da REimpostare perchè può risultare modificato dal?
049400120817             //  ?*pgm TISI96R:?
049500120817             SaveEDAT = ESVedat;
049600111104             VersioneCappario = VersioneCappario2;
049700111104         endsl;
049800111108
049900111117
050000111117         // -?CONTROLLI:?
050100111117         $DltTIESV  = *on;
050200111118         $UpdTab3CE = *on;
050300111117
050400111121         reset  $MsgErr;
050500111117         clear  wMsgErr;
050600090623
050700111118         IF  NOT  %found(TABEL00F);
050800111118
050900111118           // -?Non trovato in tab.3C?
051000111118           //   =>?NON?aggiorna?3CE   e  ?Cancella?da TIESV?
051100111118           //$DltTIESV  = *on;          ?(già così)?
051200111118           $UpdTab3CE = *off;
051300111118           wMsgErr = 1;
051400111213           if  sk_Segnalaz( wMsgErr ) = *off;
051500111118             exsr  sr_Write_WF;
051600111118           else;
051700111118             clear  wMsgErr;
051800111118           endif;
051900111118
052000111118         ELSE;
052100090623
052200111118           // -?Cliente in tab.3C NON come EasySped?
052300111118           //   »>?SE già EasyWeb:?
052400111118           //   =>?Aggiorna?3CE   e  ?Cancella?da TIESV?
052500111118           //   »>?Altrimenti:?
052600111118           //   =>?NON?aggiorna?3CE   e  ?NON?cancella?da TIESV?
052700111118           If  §3Ccba <> 'ESYSP'   and   §3Ccba <> 'ESVAL';
052800111118             if  §3Ccba =  'ESWEB';
052900111118               //$DltTIESV  = *on;      ?(già così)?
053000111118               //$UpdTab3CE = *on;      ?(già così)?
053100111118               wMsgErr = 2;
053200111118             else;
053300111118               $DltTIESV  = *off;
053400111118               $UpdTab3CE = *off;
053500111118               wMsgErr = 3;
053600111118             endif;
053700111213             if  sk_Segnalaz( wMsgErr ) = *off;
053800111118               exsr  sr_Write_WF;
053900111118             else;
054000111118               clear  wMsgErr;
054100111118             endif;
054200111118           EndIf;
054300111118
054400111118             // -?Ricevuta una serie diversa da quella prevista in tab.3C?
054500111118             //   =>?NON?aggiorna?3CE   e  ?NON?cancella?da TIESV?
054600111118           If  ESVever > '3.0.1'   and   ESVnrs <> %editc(§3Cnrs:'X');
054700111118             $DltTIESV  = *off;
054800111118             $UpdTab3CE = *off;
054900111118             wMsgErr = 4;
055000111213             if  sk_Segnalaz( wMsgErr ) = *off;
055100111118               exsr  sr_Write_WF;
055200111118             else;
055300111118               clear  wMsgErr;
055400111118             endif;
055500111118           EndIf;
055600111118
055700111118         ENDIF;
055800111117
055900111117         // -?Ricevuta una Vers.Cappario diversa da quella in vigore?
056000130730         //  ?&  NON previsto disk-B che diventa disk-C (in tab. "3C")?
056100111117         //   =>?Aggiorna?3CE   e  ?Cancella?da TIESV?
056200130730         If  ESVcver <> VersioneCappario   and   §3Cdkc <> 'S';
056300111118           //$DltTIESV  = *on;          ?(già così)?
056400111118           //$UpdTab3CE = *on;          ?(già così)?
056500111118           wMsgErr = 5;
056600111213           if  sk_Segnalaz( wMsgErr ) = *off;
056700111117             exsr  sr_Write_WF;
056800111118           else;
056900111118             clear  wMsgErr;
057000111117           endif;
057100111117         EndIf;
057200111118
057300111118         // -?Stampa (dopo il messaggio di errore) i dati del record?
057400111118         //  ?in elaborazione del file TIESV00F?
057500111121         if  $MsgErr;
057600111118           exsr  sr_Dati_eMail;
057700111118         endif;
057800090623
057900090623       ENDSR;
058000111118
058100111118       //--------------------------------------------------------------
058200111118       //?Registrazione errori nel work-file WFESV00F                  ?
058300111118       //--------------------------------------------------------------
058400111118       BEGSR  sr_Write_WF;
058500111118
058600111118         // -?Aggancio record nel work-file?
058700111118         clear  k02wfesv01;
058800111118         k_WESksc = ESVeksc;
058900111118         k_WESerr = %subst( %editc( wMsgErr : 'X' ) : 2 );
059000111118
059100111118         chain  %kds(k02wfesv01)  WFESV000;
059200111118
059300111118         Select;
059400111118
059500111118           // -?Errore già registrato per il cliente in data uguale o?
059600111118           //  ?più recente?
059700111118           //  ?Occorre pulire il codice errore (wMsgErr) per scartare?
059800111118           //  ?l'aggiornamento della tab. "3CE".?
059900111118           When  %found(WFESV01L)  and  WESdt9 >= ESVedat;
060000111118             unlock WFESV01L;
060100111118             clear  wMsgErr;
060200111118             leavesr;
060300111118
060400111118           // -?Record nuovo?
060500111118           //  ?(impostazione campi chiave o della "prima volta")?
060600111118           When  NOT %found(WFESV01L);
060700111118             clear  WFESV000;
060800111118             WESksc = k_WESksc;
060900111118             WESerr = k_WESerr;
061000111118             //WESmsg = %subst( O_Testo : 3 );    ?Dopo e comunque!?
061100111118             WESdt1 = ESVedat;
061200111118
061300111118         EndSl;
061400111118
061500111123
061600111213         ds_DatiErr = sk_DatiErr(wMsgErr);
061700120705         if  sk_nSol(wMsgErr) < *hival;
061800120705           sk_nSol(wMsgErr) = WESnsol + 1;
061900120705         endif;
062000111118
062100111206         // -?Segnalazione in stampa dell'errore rilevato?
062200111213         ////  ?(ogni 5 giorni - vedi schiera sk_DatiErr)?
062300111206         //if  %rem( WEStgo + 1 : ds_DatiErr.DE_TettoCED ) > *zero  and
062400111213         //  ?(dopo 5 giorni - vedi schiera sk_DatiErr)?
062500111213         if  ( WEStgo + 1 )      >= ds_DatiErr.DE_TettoCED  and
062600111213             WESbloc              = *blank                  and
062700111213             sk_eMailCed(wMsgErr) = *off;
062800111129
062900111213           // -?Memorizzazione errore rilevato per cliente?
063000111213           sk_eMailCed(wMsgErr) = *on;
063100111213           // -?Stampa errore?
063200111123           $MsgErr = *on;
063300111123           exsr  sr_Prep_eMail;
063400111129
063500111129         else;
063600111129
063700111129           // -?Prepararazione del singolo messaggio di errore?
063800111129           //  ?da registrare nel WrkF e da inviare via email al Poc?
063900111129           exsr  sr_Prep_MsgErr;
064000111129
064100111123         endif;
064200111118
064300111125
064400111118         // -?Aggiornamento campi nel record del work-file?
064500111125         WESmsg  = %subst( O_Testo : 3 );
064600111124
064700111118         if  WESdt9 <> ESVedat;
064800111118           if  WESdt1 > ESVedat;
064900111118             WESdt1 = ESVedat;
065000111118           endif;
065100120705           if  WEStgo < *hival;
065200120705             WEStgo += 1;
065300120705           endif;
065400111118         endif;
065500111118         if  WESdt9 < ESVedat;
065600111118           WESdt9  = ESVedat;
065700111118           WESever = ESVever;
065800111118           WEScver = ESVcver;
065900111118           WEScdsc = ESVcdsc;
066000111118           WESsncd = ESVsncd;
066100111118           WESsnca = ESVsnca;
066200111118           WESsncc = ESVsncc;
066300111118           WESnrs  = ESVnrs;
066400111118           WESobj  = ESVobj;
066500111118           WEScorpo   = ESVcorpo;
066600111118           WEStypweb  = ESVtypweb;
066700111118           WEScverMax = ESVcverMax;
066800111118           WEScddeMax = ESVcddeMax;
066900111118           WEScdscMax = ESVcdscMax;
067000111118         endif;
067100111118
067200111118         // -?Scrittura/Aggiornamento record?
067300111118         if  NOT %found(WFESV01L);
067400111118           //______________
067500111118           WRITE  WFESV000;
067600111118           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
067700111118         else;
067800111118           //_______________
067900111118           UPDATE  WFESV000;
068000111118           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068100111118         endif;
068200111118
068300111213         // -?Memorizzazione errori registrati per cliente / data?
068400111213         sk_Segnalaz( wMsgErr ) = *on;
068500111118
068600111118       ENDSR;
068700111118
068800111118       //--------------------------------------------------------------
068900111118       //?Aggiornamento tabella "3CE" per il cliente in elaborazione   ?
069000111118       //--------------------------------------------------------------
069100111118       BEGSR  sr_Upd_Tab3CE;
069200111118
069300111118         // -?Impostazione dati in tabella "3CE"?
069400111118         exsr  sr_Rie_Tab3CE;
069500111118
069600111118         // -?Pulizia flag di trasmissione?
069700111118         clear  TBEftr;
069800111118
069900111118         // -?Aggiornamento record?
070000111118         //_______________
070100111118         UPDATE  TNTBE000;
070200111118         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
070300111118
070400111118       ENDSR;
070500111118
070600111118       //--------------------------------------------------------------
070700111118       //?Inserimento tabella "3CE" per il cliente in elaborazione     ?
070800111118       //--------------------------------------------------------------
070900111118       BEGSR  sr_Wrt_Tab3CE;
071000111118
071100111118         clear  TNTBE000;
071200111118
071300111118         // -?Impostazione dati nel nuovo record?
071400111118         TBEsif = xTBEsif;
071500111118         TBElin = xTBElin;
071600111118         TBEapl = xTBEapl;
071700111118         TBEcod = c_tab;
071800111118         TBEke1 = %triml( %editc( ESVeksc : 'X' ) );
071900111118         TBEftt = xTBEftt;
072000111118         //TBEflt = DUTpou;
072100111118
072200111118         // -?Impostazione dati in tabella "3CE"?
072300111118         exsr  sr_Rie_Tab3CE;
072400111118
072500111118         // -?Scrittura nuovo record?
072600111121         //_________________
072700111121         WRITE(e)  TNTBE000;
072800111121         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
072900111118
073000111118       ENDSR;
073100111118
073200111118       //--------------------------------------------------------------
073300111118       //?Impostazione dati in tabella "3CE"                           ?
073400111118       //--------------------------------------------------------------
073500111118       BEGSR  sr_Rie_Tab3CE;
073600111118
073700111118         §3CEedat = ESVedat;
073800111118         §3CEever = ESVever;
073900111118         §3CEcver = ESVcver;
074000111118         §3CEcdsc = ESVcdsc;
074100111118         §3CEsncd = ESVsncd;
074200111118         §3CEsnca = ESVsnca;
074300111118         §3CEsncc = ESVsncc;
074400111118         §3CEobj    = ESVobj;
074500111118         §3CEcorpo  = ESVcorpo;
074600111118         §3CEtypweb = ESVtypweb;
074700111118         §3CEnrs    = ESVnrs;
074800111118         §3CEcvermx = ESVcvermax;
074900111118         §3CEcddemx = ESVcddemax;
075000111118         §3CEcdscmx = ESVcdscmax;
075100120316
075200120316         §3CEesvsrv = d§ESVsrv;
075300111118
075400111118         TBEuni = d3CE;
075500111118
075600111118       ENDSR;
075700111108
075800111108       //--------------------------------------------------------------
075900111108       //?Segnalazione errore rilevato                                 ?
076000111108       //--------------------------------------------------------------
076100111108       BEGSR  sr_Prep_eMail;
076200111108
076300111117         // -?Override al file di stampa ed apertura dello stesso?
076400111108         if  NOT  %open(PrtEMAIL);
076500111108           exsr  sr_Open_PrtF;
076600111108         endif;
076700111108
076800111108
076900111117         // -?Stampa una riga vuota (a rottura di cliente)?
077000111117         If  ESVeksc <> SaveEKSC;
077100111108           SaveEKSC = ESVeksc;
077200111129           //clear O_Testo;
077300111129           O_Testo = '.';
077400111108           except PrtDet;
077500111108         EndIf;
077600111108
077700111117         // -?Prepararazione del singolo messaggio di errore?
077800111108         exsr  sr_Prep_MsgErr;
077900111108
078000111117         // -?Stampa della singola segnalazione?
078100111117         except  PrtDet;
078200111108
078300111108       ENDSR;
078400111118
078500111118       //--------------------------------------------------------------
078600111118       //?Apertura file di stampa per invio e-mail                     ?
078700111118       //--------------------------------------------------------------
078800111118       BEGSR  sr_Open_PrtF;
078900111118
079000111118         // -?Override al file di stampa per impostarvi i dati per?
079100111118         //  ?l'invio via e-mail?
079200111118         exsr sr_Override;
079300111118
079400111118         open PrtEMAIL;
079500111118
079600111118         // -?Stampa una testata se NON è richiesta la e-mail?
079700111118         IF  §MRAdreg =  *blank;
079800111118           O_Testo = JobUser + ' - ' + SDSpgm
079900111118                   + ' - ' + %editc( *date : 'Y' )
080000111118                   + ' - *REM* ' + %trimr( %subst(§CM1var : 7 : 70) )
080100111118                   + ' ' + c_Tab;
080200111118           except PrtDet;
080300111118           clear O_Testo;
080400111118           except PrtDet;
080500111118           except PrtDet;
080600111118         ENDIF;
080700111118
080800111118         // -?Stampa testo iniziale?
080900111118         O_Testo = c_Titolo;
081000111118         except PrtDet;
081100111118         clear  O_Testo;
081200111118         except PrtDet;
081300111118
081400111118       ENDSR;
081500111118
081600111118       //--------------------------------------------------------------
081700111118       //?Override al file di stampa per impostarvi i dati per         ?
081800111118       //?  l'invio via e-mail   +   stampa inizio e-mail              ?
081900111118       //--------------------------------------------------------------
082000111118       BEGSR  sr_Override;
082100111118
082200111118         reset $Invio;
082300111118         reset TRTCM1ds;
082400111118
082500111118         IF  §MRAdreg <> *blank;
082600111118           §CM1mitt = %trim(§MRAdmitt);
082700111118           §CM1dst  = %trim(§MRAddest);
082800111118           §CM1tips = §MRAdreg;
082900111118           §CM1po   = c_Sede;
083000111118           §CM1var  = '*OBJM*' + §MRAddes;
083100111118           §CM1idp  = §MRAdidpro;
083200111118           Qcmd = c_CmdOvrPrtF
083300111118                + ' outq(' + %trim(§MRAdoutqi) + ')'
083400111118                + ' usrdfndta(''' + TRTCM1ds + ''')';
083500111118         ELSE;
083600111118           Qcmd = c_CmdOvrPrtF;
083700111118         ENDIF;
083800111118
083900111118         exsr  sr_ExecCmd;
084000111118         IF  §MRAdreg <> *blanks  and  Qusei = *blank;
084100111118           $Invio = *on;
084200111118         ENDIF;
084300111118
084400111118       ENDSR;
084500111108
084600111108       //--------------------------------------------------------------
084700111108       //?Preparazione del singolo messaggio di errore                 ?
084800111108       //--------------------------------------------------------------
084900111108       BEGSR  sr_Prep_MsgErr;
085000111108
085100111117         // -?Preparazione singola segnalazione:?
085200111108         if  ds_DatiErr.DE_TxtAttenz <> *blank;
085300111108           O_Testo = '- ' + %trimr( ds_DatiErr.DE_TxtAttenz ) + ' '
085400111213                          + %trimr( sk_Msg1(wMsgErr) );
085500111108         else;
085600111213           O_Testo = '- ' + sk_Msg1(wMsgErr);
085700111108         endif;
085800111108
085900111108         Select;
086000111108
086100111117           // -?Cliente non trovato in tab.3C?
086200111108           When  wMsgErr = 1;
086300111108             O_Testo = %replace ( %editc ( ESVeksc : 'X' ) :
086400111108                                  O_Testo :
086500111114                                  %scan ( '&WESKSC' : O_Testo ) );
086600111117
086700111117           // -?Cliente in tab.3C come EasySped-WEB?
086800111117           When  wMsgErr = 2;
086900111117             O_Testo = %replace ( %editc( ESVeksc : 'X' ) :
087000111117                                  O_Testo :
087100111117                                  %scan ( '&WESKSC' : O_Testo ) );
087200111108
087300111108           // -?Cliente in tab.3C non come EasySped?
087400111117           When  wMsgErr = 3;
087500111108             O_Testo = %replace ( %editc( ESVeksc : 'X' ) :
087600111108                                  O_Testo :
087700111114                                  %scan ( '&WESKSC' : O_Testo ) );
087800111108             O_Testo = %replace ( §3Ccba :
087900111108                                  O_Testo :
088000111114                                  %scan ( '&3CCB' : O_Testo ) );
088100111108
088200111108           // -?Ricevuta serie diversa da quella prevista in tab.3C?
088300111118           When  wMsgErr = 4;
088400111108             O_Testo = %replace ( %editc ( ESVeksc : 'X' ) :
088500111108                                  O_Testo :
088600111114                                  %scan ( '&WESKSC' : O_Testo ) );
088700111108             O_Testo = %replace ( ESVnrs :
088800111108                                  O_Testo :
088900111118                                  %scan ( '&1' : O_Testo ) );
089000111118             O_Testo = %replace ( %editc ( §3Cnrs :  'X' ) :
089100111118                                  O_Testo :
089200111118                                  %scan ( '&2' : O_Testo ) );
089300111118
089400111118           // -?Ricevuta versione cappario diversa da quella in vigore?
089500111118           When  wMsgErr = 5;
089600111118             O_Testo = %replace ( %editc( ESVeksc : 'X' ) :
089700111118                                  O_Testo :
089800111118                                  %scan ( '&WESKSC' : O_Testo ) );
089900111118             O_Testo = %trimr ( O_Testo ) + ' ' +
090000111118                       %trim ( %editc( ESVcver : '3' ) ) +
090100111118                       '. In vigore: ' +
090200111118                       %trim ( %editc( VersioneCappario : '3' ) ) +
090300111118                       '.';
090400111108
090500111108         EndSl;
090600111108
090700111118         // -?Segnalazione che i dati sono stati aggiornati in tab. 3CE?
090800111117         //                                 ?e/o cancellati da TIESV00F?
090900111108         //select;
091000111117         //  when  wMsgErr = 2;
091100111108         //  when  $UpdTab3CE;
091200111114         //    O_Testo = %trimr(O_Testo) + c_UpdTab3CE;
091300111108         //  when  $DltTIESV;
091400111114         //    O_Testo = %trimr(O_Testo) + c_DltTIESV;
091500111108         //  other;
091600111114         //    O_Testo = %trimr(O_Testo) + c_NoUpdNoDlt;
091700111108         //endsl;
091800111108
091900111108       ENDSR;
092000090623
092100090623       //--------------------------------------------------------------
092200090623       //?Stampa dati errati nel record del file TIESV                 ?
092300090623       //--------------------------------------------------------------
092400090623       BEGSR  sr_Dati_eMail;
092500080520
092600111121         //O_Testo = %editc( ESVeksc : 'X' )
092700111121         //        + '|'
092800111121         //        + %editc( ESVedat : 'X' )
092900111121         //        + '|'
093000111121         //        + ESVever
093100111121         //        + '|'
093200111121         //        + %editc( ESVcver : 'X' )
093300111121         //        + '|'
093400111121         //        + %editc( ESVcdsc : 'X' )
093500111121         //        + '|'
093600111121         //        + %editc( ESVsncd : 'X' )
093700111121         //        + '|'
093800111121         //        + %editc( ESVsnca : 'X' )
093900111121         //        + '|'
094000111121         //        + %editc( ESVsncc : 'X' )
094100111121         //        + '|'
094200111121         //        + ESVnrs;
094300111121         //except PrtDet;
094400111121         //
094500111121         //if  ESVobj <> *blank   or   ESVever >= c_VersEsySp;
094600111121         //  O_Testo = 'Obj email: ' + ESVobj;
094700111121         //  except PrtDet;
094800111121         //endif;
094900111121         //
095000111121         //if  ESVcorpo <> *blank   or   ESVever >= c_VersEsySp;
095100111121         //  O_Testo = 'Corpo email: ' + ESVcorpo;
095200111121         //  except PrtDet;
095300111121         //endif;
095400111121         //
095500111121         //if  ESVtypweb <> *blank   or   ESVever >= c_VersEsySp;
095600111121         //  O_Testo = 'Tipo aggior. web: ' + ESVtypweb;
095700111213         //  xx = %lookup ( ESVtypweb : sk_TypWeb );
095800111121         //  if  xx > *zero;
095900111213         //    O_Testo = %trimr(O_Testo) + %trimr(sk_TypWebD(xx));
096000111121         //  endif;
096100111121         //  except PrtDet;
096200111121         //endif;
096300111121         //
096400111121         //if  ESVcvermax <> *zero   or   ESVcddemax <> *zero
096500111121         //                          or   ESVcdscmax <> *zero;
096600111121         //  O_Testo = 'Versione massima cappario: '
096700111121         //          + %editc(ESVcvermax : 'Z');
096800111121         //  except PrtDet;
096900111121         //  O_Testo = 'Decorrenza Vers.Max.Capp.: '
097000111121         //          + %editw(ESVcddemax : '    /  /  ');
097100111121         //  except PrtDet;
097200111121         //  O_Testo = 'Scadenza   Vers.Max.Capp.: '
097300111121         //          + %editw(ESVcdscmax : '    /  /  ');
097400111121         //  except PrtDet;
097500111121         //endif;
097600111121         //
097700111121         //select;
097800111121         //  when  ESVflo =  *blank;
097900111121         //  when  ESVflo <> *blank   and  %len(ESVflo) <= %len(O_Testo);
098000111121         //    O_Testo = ESVflo;
098100111121         //    except PrtDet;
098200111121         //  when  ESVflo <> *blank   and  %len(ESVflo) >  %len(O_Testo);
098300111121         //    O_Testo = %subst( ESVflo : 1 : %len(O_Testo) );
098400111121         //    except PrtDet;
098500111121         //    O_Testo = %subst( ESVflo : %len(O_Testo) );
098600111121         //    except PrtDet;
098700111121         //endSL;
098800111121
098900111121         //wData_EUR = %date( WESdt1 : *iso );
099000111121         //O_Testo = '- Data 1ª occorrenza: '
099100111121         //        + %editc( %dec( wData_EUR ) : 'Y' );
099200111121         //wData_EUR = %date( WESdt9 : *iso );
099300111121         //O_Testo = %trimr( O_Testo )
099400111121         //        + '   Data ultima occorrenza: '
099500111121         //        + %editc( %dec( wData_EUR ) : 'Y' );
099600111121         //O_Testo = %trimr( O_Testo )
099700111121         //        + '   Tot. occorrenze: '
099800111121         //        + %char( WEStgo );
099900111121         //except  PrtDet;
100000111121
100100111121         wData_EUR = %date( ESVedat : *iso );
100200111122         O_Testo = 'Dati ricevuti dal cliente in data '
100300111122                 + %editc( %dec( wData_EUR ) : 'Y' )
100400111122                 + ':';
100500111122         except PrtDet;
100600111121
100700111121         O_Testo = 'Versione EasySped: '
100800111121                 + ESVever
100900111121                 + '   Versione cappario in uso: '
101000111121                 + %editc( ESVcver : '3' )
101100111121                 + '   Scadenza: '
101200111121                 + %editw( ESVcdsc : '    /  /0 ' );
101300111121         except PrtDet;
101400111121
101500111121         O_Testo = 'Range segnacolli: ' + %editc( ESVsncd : '3' )
101600111121                                  + '-' + %editc( ESVsnca : '3' )
101700111122                 + '    Segnacollo corrente: ' + %editc( ESVsncc : '3' )
101800111122                 + '      Serie: ' + ESVnrs;
101900111121         except PrtDet;
102000111121
102100111121         if  ESVcvermax <> *zero   or
102200111121             ESVcddemax <> *zero   or
102300111121             ESVcdscmax <> *zero;
102400111121           O_Testo = 'Versione massima cappario: '
102500111121                   + %editc( ESVcvermax : '3' )
102600111122                   + '     Decorrenza: '
102700111121                   + %editw( ESVcddemax : '    /  /0 ' )
102800111122                   + '            Scadenza: '
102900111121                   + %editw( ESVcdscmax : '    /  /0 ' );
103000111121           except PrtDet;
103100111121         endif;
103200111122
103300120613         //O_Testo = '- ' + %char( WESnsol + 1 ) + '° sollecito.';
103400120613         For  xx = 1  To  %elem(sk_nSol);
103500120613           ds_DatiErr = sk_DatiErr(xx);
103600120613           if  sk_nSol(xx) >= ds_DatiErr.DE_TettoCED;
103700120613             O_Testo = '- ' + %char( sk_nSol(xx) ) + '° sollecito.';
103800120613             except  PrtDet;
103900120613           endif;
104000120613         EndFor;
104100120613         ds_DatiErr = sk_DatiErr(wMsgErr);
104200111121
104300080520       ENDSR;
104400080520
104500080520       //--------------------------------------------------------------
104600090604       //?Operazioni finali.                                           ?
104700080520       //--------------------------------------------------------------
104800090604       BEGSR  sr_RoutEnd;
104900080520
105000111110         If %open(PrtEMAIL);
105100080520
105200111117           // -?Chiusura dello spool?
105300111114           clear O_Testo;
105400080520           except PrtDet;
105500111114           O_Testo = '***   Fine Lista   ***';
105600080520           except PrtDet;
105700080520
105800080520           close PrtEMAIL;
105900080520
106000111117           // -?Eliminazione override?
106100090604           Qcmd = c_CmdDltOvr;
106200111117           exsr  sr_ExecCmd;
106300080520
106400111110         EndIf;
106500111110
106600111117         // -?Cancellazione vecchi record?
106700111110         DeleteRecWFESV();
106800111122
106900111122         // -?Invio solleciti via e-mail ai Poc?
107000111123         SendSollecitiPoc( kpjba );
107100080520
107200111122         // -?Uscita?
107300080520         return;
107400080520
107500080520       ENDSR;
107600111117
107700111117       //--------------------------------------------------------------
107800111117       //?Esecuzione del comando (già impostato).                      ?
107900111117       //--------------------------------------------------------------
108000111117       BEGSR  sr_ExecCmd;
108100111117
108200111117         clear Qcap0100;
108300111117         Qcabcsdh = *off;
108400111117         Qcapa    = *off;
108500111117         Qcacmdss = *off;
108600111117         Qcaerved = *allX'00';
108700111117
108800111117         clear Qusec;
108900111117         Qusbprv  = %size(Qusec);
109000111117
109100111117         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
109200111117                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
109300111117                           0 : 0 : Qusec);
109400111117
109500111117         // -?Invio *msg per segnalazione errore?
109600111117         //if  Qusei <> *blank;
109700111117         //  ...;
109800111117         //endif;
109900111117
110000111117       ENDSR;
110100080520
110200080520      /end-free
110300080520
110400080520      //---------------------------------------------------------------
110500090604      //?Spool di stampa (per e-mail).                                 ?
110600080520      //---------------------------------------------------------------
110700090604
110800080520     oPrtEMAIL  e            PRTdet      1
110900111114     o                       O_Testo
111000100107
111100100107      //---------------------------------------------------------------
111200100107      //?Schiere a tempo di compilazione.                              ?
111300100107      //---------------------------------------------------------------
111400100107
111500111213** -?sk_TypWeb / sk_TypWebD?---*
1116001001070  - Funzionalità NON attiva
1117001001071  - Funzionalità attiva
1118001001072  - Framework 1.0
1119001001074  - Framework 1.1
1120001001076  - Framework 1.1 & 1.0
1121001001078  - Framework 2.0
11220010010710 - Framework 2.0 & 1.0
11230010010712 - Framework 2.0 & 1.1
112400111213** -?sk_Err / sk_DatiErr?-?Cod. ERRORE  &  dati: Gravità - S/N eMail - Nr gg limite - Nr gg x CED?
11250011112301 99-S-001-005-ATTENZIONE:
11260011112302 99-S-001-005-ATTENZIONE:
11270011112303 99-S-001-005-ATTENZIONE:
11280011112304 99-S-001-005-ATTENZIONE:
11290011112305 99-S-001-005-ATTENZIONE:
113000111213** -?sk_Msg1?-?Messaggi di Errore?+....4....+....5....+....6....+....7....+....8....+....9....+...10
113100111107il cliente &WESKSC NON è iscritto per invio dati.
113200111117ricevuti dati dal cliente &WESKSC che risulta abilitato ad EasySped WEB.
113300111107cliente &WESKSC abilitato al supporto "&3CCB", non EasySped.
113400111117ricevuti dati dal cliente &WESKSC con serie diversa da quella abilitata. Ricevuta &1. Abilitata &2.
113500111118ricevuti dati dal cliente &WESKSC con versione cappario NON in vigore. Ricevuta
