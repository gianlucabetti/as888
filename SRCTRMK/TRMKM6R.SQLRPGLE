000100031205      *------------------------------------------------------------------------*
000200090209      * Elaborazione totali mappatura commerciale - base unificanti
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090211     FWFDEmTuf  uF a E           k DISK
000700031205      *------------------------------------------------------------------------*
000800031205      *   C A M P I   D I   L A V O R O
000900031205      *------------------------------------------------------------------------*
001000090206     d wrkgetlista     s           4096    varying
001100090210     d sommaspt        s             15  0
001200090210     d sommafac        s             15  0
001300090210     d sommaprf        s             15  0
001400090210     d sommapro        s             15  0
001500090210     d sommames        s             15  0
001600090210     d sommaaer        s             15  0
001700090210     d sommacae        s             15  0
001800090210     d sommaatr        s             15  0
001900090210     d sommabar        s             15  0
002000090210     d sommamez        s             15  0
002100090210     d sommaarco       s             15  0
002200090210     d sommaarto       s             15  0
002300090210     d sommaaws        s             15  0
002400090210     d sommabars       s             15  0
002500090210     d sommadhl        s             15  0
002600090210     d sommaexec       s             15  0
002700090210     d sommaferc       s             15  0
002800090210     d sommamail       s             15  0
002900090210     d sommamtn        s             15  0
003000090210     d sommasda        s             15  0
003100090210     d sommasusa       s             15  0
003200090210     d sommatnt        s             15  0
003300090210     d sommaups        s             15  0
003400090210     d sommacatr       s             15  0
003500090206     d contacli        s             11  0
003600090211     d ddiv            s                     like(demdiv)
003700090211     d dddiv           s                     like(demddiv)
003800090211     d dare            s                     like(demare)
003900090211     d ddare           s                     like(demdare)
004000090211     d dcmerc          s                     like(demcmerc)
004100090211     d ddmerc          s                     like(demdmerc)
004200031205
004300031205      *------------------------------------------------------------------------*
004400031205      *   D S   I N T E R N E / E S T E R N E
004500031205      *------------------------------------------------------------------------*
004600031205     d Kpjba         e ds
004700071122     d
004800090203      // - Reperimento dati utente
004900090203     d TIBS34ds      e ds
005000090203      // - Dati utente
005100090203     d §AzUte        e ds                  extname(AZUTE00F)
005200090203     d                                     dtaara
005300090203     d §DatiUte      e ds                  extname(dDatiUte)
005400090203     d                                     dtaara
005500090203     D*
005600071122      *------------------------------------------------------------------------*
005700071122      *   P R O T O T I P I
005800071122      *------------------------------------------------------------------------*
005900090203      /copy gaitrasrc/srcprotopr,tibs34r
006000031205      *------------------------------------------------------------------------*
006100071217     c     *entry        plist
006200090203     c                   parm                    kpjba
006300090203     c
006400071116      /free
006500090211         // Reperimento dati job
006600090203         exsr DatiJob;
006700090203
006800090206         // pulizia file dei totali
006900090211         exec sql   delete  from  wfdemtuf   ;
007000090206
007100090206
007200090206         // SQL dinamico per riempire i totali
007300090209         wrkgetlista='SELECT demdiv, demddiv, demare, demdare, demcmerc, '  +
007400090209                     'demdmerc, count(*), ' +
007500090209                     'sum(demspt), sum(demfac), sum(demprf), sum(dempro), '+
007600090209                     'sum(demmes), sum(demaer), sum(demcae), sum(dematr), '+
007700090209                     'sum(dembar), sum(demmez), sum(demarco), sum(demarto), '+
007800090209                     'sum(demaws), sum(dembars), sum(demdhl), sum(demexec),  '+
007900090209                     'sum(demferc), sum(demmail), sum(demmtn), sum(demsda),  '+
008000090210                     'sum(demsusa), sum(demtnt), sum(demups), sum(demcatr)   '+
008100090209                     ' from WFDEMU0F ' +
008200090211                     ' WHERE DEMERR=''     '' and demcpoifo>0 '+
008300090209                     ' group BY DEMdiv, DEMddiv, demare, demdare,' +
008400090210                     '  demCMERC, demdmerc   ' +
008500090210                     ' ORDER BY DEMdiv, DEMare, demCMERC  ' ;
008600071122
008700090206         exec sql   PREPARE s1 FROM :wrkgetlista   ;
008800090206
008900090206         exec sql   DECLARE a1 cursor FOR  s1   ;
009000090206
009100090206         exec sql OPEN a1   ;
009200090206
009300090206         clear sqlcod   ;
009400090206
009500090206         dow sqlcod=0   ;
009600090206
009700090206         exec sql FETCH NEXT  FROM a1 INTO: demdiv, :demddiv, :demare,
009800090209                  :demdare, :demcmerc, :demdmerc, :contacli, :sommaspt,
009900090209                  :sommafac,  :sommaprf,  :sommapro,  :sommames,
010000090209                  :sommaaer,  :sommacae,  :sommaatr,  :sommabar,
010100090209                  :sommamez,  :sommaarco, :sommaarto, :sommaaws,
010200090209                  :sommabars, :sommadhl,  :sommaexec, :sommaferc,
010300090209                  :sommamail, :sommamtn,  :sommasda,  :sommasusa,
010400090209                  :sommatnt,  :sommaups,  :sommacatr ;
010500090206
010600090206         select ;
010700090206         when sqlcod=100 ;
010800090206
010900090206         when sqlcod<0   ;
011000090211         *inh1=*on   ;
011100090206
011200090206         other   ;
011300090206         exsr scrivirec ;
011400090206         endsl  ;
011500090206
011600090206         enddo   ;
011700090204
011800090206         exec sql CLOSE a1   ;
011900090206
012000090211         // SQL dinamico per riempire i totali
012100090211         clear wrkgetlista  ;
012200090211         wrkgetlista='SELECT demdiv, demddiv, demare, demdare, demcmerc, '  +
012300090211                     'demdmerc, count(*)  ' +
012400090211                     ' from WFDEMU0F ' +
012500090211                     ' WHERE DEMERR=''     '' and demcpoifo=0 '+
012600090211                     ' group BY DEMdiv, DEMddiv, demare, demdare,' +
012700090211                     '  demCMERC, demdmerc   ' +
012800090211                     ' ORDER BY DEMdiv, DEMare, demCMERC  ' ;
012900090211
013000090211         exec sql   PREPARE s2 FROM :wrkgetlista   ;
013100090211
013200090211         exec sql   DECLARE a2 cursor FOR  s2   ;
013300090211
013400090211         exec sql OPEN a2   ;
013500090211
013600090211         clear sqlcod   ;
013700090211
013800090211         dow sqlcod=0   ;
013900090211
014000090211         exec sql FETCH NEXT  FROM a2 INTO: ddiv, :dddiv, :dare,
014100090211                  :ddare, :dcmerc, :ddmerc, :contacli  ;
014200090211
014300090211         select ;
014400090211         when sqlcod=100 ;
014500090211
014600090211         when sqlcod<0   ;
014700090211         *inh2=*on   ;
014800090211
014900090211         other   ;
015000090211         exsr Aggiorec ;
015100090211         endsl  ;
015200090211
015300090211         enddo   ;
015400090211
015500090211         exec sql CLOSE a2   ;
015600071121       *inlr=*on         ;
015700071116       //-------------------------------------------------------------
015800090211       //  Scrittura record di totale
015900090202       //-------------------------------------------------------------
016000090206       BEGSR  ScriviREC     ;
016100090206       clear  demfil  ;
016200090206       clear  demdfil  ;
016300090206
016400090206       demtotcli=contacli   ;
016500090211       demtotinfo=contacli   ;
016600090210       demtotspt=sommaspt   ;
016700090210       demtotfac=sommafac   ;
016800090210       demtotprf=sommaprf   ;
016900090210       demtotpro=sommapro   ;
017000090210       demtotmes=sommames   ;
017100090210       demtotaer=sommaaer   ;
017200090210       demtotcae=sommacae   ;
017300090210       demtotatr=sommaatr   ;
017400090210       demtotbar=sommabar   ;
017500090210       demtotmez=sommamez   ;
017600090210       demtotarco=sommaarco  ;
017700090210       demtotarto=sommaarto  ;
017800090210       demtotaws=sommaaws   ;
017900090210       demtotbars=sommabars  ;
018000090210       demtotdhl=sommadhl   ;
018100090210       demtotexec=sommaexec  ;
018200090210       demtotferc=sommaferc  ;
018300090210       demtotmail=sommamail  ;
018400090210       demtotmtn=sommamtn   ;
018500090210       demtotsda =sommasda   ;
018600090210       demtotsusa=sommasusa   ;
018700090210       demtottnt =sommatnt   ;
018800090210       demtotups =sommaups   ;
018900090210       demtotcatr=sommacatr   ;
019000090206
019100090210       write wfdemtu0  ;
019200090206       ENDSR   ;
019300090211       //-------------------------------------------------------------
019400090211       //  Aggiornamento con clienti senza info
019500090211       //-------------------------------------------------------------
019600090211       BEGSR  AggioREC     ;
019700090211       chain   (ddiv:dare:dcmerc)  wfdemtuf  ;
019800090211       if %found(wfdemtuf)    ;
019900090211       demtotcli=demtotcli+contacli   ;
020000090211       update wfdemtu0    ;
020100090211       else   ;
020200090211       clear  wfdemtu0   ;
020300090211       demdiv=ddiv  ;
020400090211       demddiv=dddiv  ;
020500090211       demare=dare ;
020600090211       demdare=ddare  ;
020700090211       demcmerc=dcmerc  ;
020800090211       demdmerc=ddmerc  ;
020900090211       demtotcli=contacli   ;
021000090211
021100090211       write wfdemtu0    ;
021200090211       endif   ;
021300090211       ENDSR   ;
021400090206       //--------------------------------------------------------------
021500090206       //?Reperimento Dati del job (Utente/Operativi).
021600090206       //--------------------------------------------------------------
021700090206       BEGSR DatiJob;
021800090206
021900090206         in(E) §AzUte;
022000090206         if NOT %error;
022100090206           in(E) §DatiUte;
022200090206         endif;
022300090206         if %error or RSut = *blanks;
022400090206           clear TIBS34ds;
022500090206           tibs34r(tibs34ds);
022600090206           in §AzUte;
022700090206           in §DatiUte;
022800090206         endif;
022900090206
023000090206       ENDSR;
023100090206
023200090206
023300071116      /end-free
