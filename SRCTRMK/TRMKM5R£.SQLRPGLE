000100031205      *------------------------------------------------------------------------*
000200090206      * elaborazione totali mappatura commerciale - base potenziali
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600090206     FWFDEmT0f  iF a E             DISK
000700031205      *------------------------------------------------------------------------*
000800031205      *   C A M P I   D I   L A V O R O
000900031205      *------------------------------------------------------------------------*
001000090206     d wrkgetlista     s           4096    varying
001100090206     d sommafat        s             15  0
001200090206     d contacli        s             11  0
001300031205
001400031205      *------------------------------------------------------------------------*
001500031205      *   D S   I N T E R N E / E S T E R N E
001600031205      *------------------------------------------------------------------------*
001700031205     d Kpjba         e ds
001800071122     d
001900090203      // - Reperimento dati utente
002000090203     d TIBS34ds      e ds
002100090203      // - Dati utente
002200090203     d §AzUte        e ds                  extname(AZUTE00F)
002300090203     d                                     dtaara
002400090203     d §DatiUte      e ds                  extname(dDatiUte)
002500090203     d                                     dtaara
002600090203     D*
002700071122      *------------------------------------------------------------------------*
002800071122      *   P R O T O T I P I
002900071122      *------------------------------------------------------------------------*
003000090203      /copy gaitrasrc/srcprotopr,tibs34r
003100031205      *------------------------------------------------------------------------*
003200071217     c     *entry        plist
003300090203     c                   parm                    kpjba
003400090203     c
003500071116      /free
003600090203         // Reperimento dati job
003700090203         exsr DatiJob;
003800090203
003900090206         // pulizia file dei totali
004000090206         exec sql   delete  from  wfdemt0f   ;
004100090206
004200090206
004300090206         // SQL dinamico per riempire i totali
004400090206         wrkgetlista='SELECT demdiv, demddiv, demare, demdare, demmerc, '  +
004500090206                     'demdmerc, demcli, sum(demtotfat), count(*) ' +
004600090206                     ' from WFDEMP0F ' +
004700090206                     ' WHERE DEMERR=''     '' OR DEMUNIF1>0 ' +
004800090206                     ' group BY DEMdiv, DEMare, demCMERC, demcli ' +
004900090206                     ' order BY DEMdiv, DEMare, demCMERC, demcli ' ;
005000071122
005100090206         exec sql   PREPARE s1 FROM :wrkgetlista   ;
005200090206
005300090206         exec sql   DECLARE a1 cursor FOR  s1   ;
005400090206
005500090206         exec sql OPEN a1   ;
005600090206
005700090206         clear sqlcod   ;
005800090206
005900090206         dow sqlcod=0   ;
006000090206
006100090206         exec sql FETCH NEXT  FROM a1 INTO: demdiv, :demddiv, :demare,
006200090206                  :demdare, :demcmerc, :demdmerc, :demcli, :sommafat, :contacli;
006300090206
006400090206         select ;
006500090206         when sqlcod=100 ;
006600090206
006700090206         when sqlcod<0   ;
006800090206         *inh1=*on   ;
006900090206
007000090206         other   ;
007100090206         exsr scrivirec ;
007200090206         endsl  ;
007300090206
007400090206         enddo   ;
007500090204
007600090206         exec sql CLOSE a1   ;
007700090206
007800071121       *inlr=*on         ;
007900071116       //-------------------------------------------------------------
008000090206       //  Scrittura recorrd di totale
008100090202       //-------------------------------------------------------------
008200090206       BEGSR  ScriviREC     ;
008300090206       clear  demfil  ;
008400090206       clear  demdfil  ;
008500090206
008600090206       if demcli=' '   ;
008700090206         demcli='N' ;
008800090206       endif   ;
008900090206
009000090206       demtotfat=sommafat   ;
009100090206       demtotcli=contacli   ;
009200090206
009300090206       write wfdemt00  ;
009400090206       ENDSR   ;
009500090206       //--------------------------------------------------------------
009600090206       //?Reperimento Dati del job (Utente/Operativi).
009700090206       //--------------------------------------------------------------
009800090206       BEGSR DatiJob;
009900090206
010000090206         in(E) §AzUte;
010100090206         if NOT %error;
010200090206           in(E) §DatiUte;
010300090206         endif;
010400090206         if %error or RSut = *blanks;
010500090206           clear TIBS34ds;
010600090206           tibs34r(tibs34ds);
010700090206           in §AzUte;
010800090206           in §DatiUte;
010900090206         endif;
011000090206
011100090206       ENDSR;
011200090206
011300090206
011400071116      /end-free
