000100031205      *------------------------------------------------------------------------*
000200090212      * Elaborazione totali mappatura commerciale - base potenziali  per
000300090212      *                     spesa trasporti presunta
000400031205      *------------------------------------------------------------------------*
000500071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000600031205      *------------------------------------------------------------------------*
000700090212     FWFDEmTrf  iF a E             DISK
000800031205      *------------------------------------------------------------------------*
000900031205      *   C A M P I   D I   L A V O R O
001000031205      *------------------------------------------------------------------------*
001100090206     d wrkgetlista     s           4096    varying
001200090212     d sommasptp       s             15  0
001300090206     d contacli        s             11  0
001400031205
001500031205      *------------------------------------------------------------------------*
001600031205      *   D S   I N T E R N E / E S T E R N E
001700031205      *------------------------------------------------------------------------*
001800031205     d Kpjba         e ds
001900071122     d
002000090203      // - Reperimento dati utente
002100090203     d TIBS34ds      e ds
002200090203      // - Dati utente
002300090203     d §AzUte        e ds                  extname(AZUTE00F)
002400090203     d                                     dtaara
002500090203     d §DatiUte      e ds                  extname(dDatiUte)
002600090203     d                                     dtaara
002700090203     D*
002800071122      *------------------------------------------------------------------------*
002900071122      *   P R O T O T I P I
003000071122      *------------------------------------------------------------------------*
003100090203      /copy gaitrasrc/srcprotopr,tibs34r
003200031205      *------------------------------------------------------------------------*
003300071217     c     *entry        plist
003400090203     c                   parm                    kpjba
003500090203     c
003600071116      /free
003700090203         // Reperimento dati job
003800090203         exsr DatiJob;
003900090203
004000090206         // pulizia file dei totali
004100090212         exec sql   delete  from  wfdemtrf   ;
004200090206
004300090206
004400090206         // SQL dinamico per riempire i totali
004500090209         wrkgetlista='SELECT demdiv, demddiv, demare, demdare, demcmerc, '  +
004600090212                     'demdmerc, sum(demsptprs), count(*) ' +
004700090206                     ' from WFDEMP0F ' +
004800090212                     ' WHERE DEMERR=''     '' and DEMUNIF1=0 ' +
004900090212                     ' and DEMSPTPRS>0 ' +
005000090209                     ' group BY DEMdiv, DEMddiv, demare, demdare,' +
005100090212                     '  demCMERC, demdmerc  ' +
005200090212                     ' order BY DEMdiv, DEMare, demCMERC ' ;
005300071122
005400090206         exec sql   PREPARE s1 FROM :wrkgetlista   ;
005500090206
005600090206         exec sql   DECLARE a1 cursor FOR  s1   ;
005700090206
005800090206         exec sql OPEN a1   ;
005900090206
006000090206         clear sqlcod   ;
006100090206
006200090206         dow sqlcod=0   ;
006300090206
006400090206         exec sql FETCH NEXT  FROM a1 INTO: demdiv, :demddiv, :demare,
006500090212                  :demdare, :demcmerc, :demdmerc, :sommasptp, :contacli;
006600090206
006700090206         select ;
006800090206         when sqlcod=100 ;
006900090206
007000090206         when sqlcod<0   ;
007100090206         *inh1=*on   ;
007200090206
007300090206         other   ;
007400090206         exsr scrivirec ;
007500090206         endsl  ;
007600090206
007700090206         enddo   ;
007800090204
007900090206         exec sql CLOSE a1   ;
008000090206
008100071121       *inlr=*on         ;
008200071116       //-------------------------------------------------------------
008300090206       //  Scrittura recorrd di totale
008400090202       //-------------------------------------------------------------
008500090206       BEGSR  ScriviREC     ;
008600090206       clear  demfil  ;
008700090206       clear  demdfil  ;
008800090206
008900090212       demtotsptp=sommasptp  ;
009000090206       demtotcli=contacli   ;
009100090206
009200090212       write wfdemtr0  ;
009300090206       ENDSR   ;
009400090206       //--------------------------------------------------------------
009500090206       //?Reperimento Dati del job (Utente/Operativi).
009600090206       //--------------------------------------------------------------
009700090206       BEGSR DatiJob;
009800090206
009900090206         in(E) §AzUte;
010000090206         if NOT %error;
010100090206           in(E) §DatiUte;
010200090206         endif;
010300090206         if %error or RSut = *blanks;
010400090206           clear TIBS34ds;
010500090206           tibs34r(tibs34ds);
010600090206           in §AzUte;
010700090206           in §DatiUte;
010800090206         endif;
010900090206
011000090206       ENDSR;
011100090206
011200090206
011300071116      /end-free
