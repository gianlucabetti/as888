000100121218      //---------------------------------------------------------------
000200121218      //
000300150414      //?   Modifica Addizionale Gestione per Lettere errate
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100150414     fTARIFFE_AUif   e           k disk    rename(TARIFFE_AU:TARIFFE)
001200140331     fPRTF198   o    f  198        printer oflind(*inof)
001300150414     fTNTAM01L  uf   e           k disk
001400040119
001500040119      *---------------------------------------------------------------*
001600121218
001700121218      // Parametri
001800121218     d kpjba         e ds
001900121218
002000121218       // - Campi di comodo
002100121218
002200121218       // - flag booleani
002300121218     d wEoF            s               n
002400090212
002500090212       //--------------------------------------------------------------
002600090212       //?Definizione procedure usate.                                 ?
002700090212       //--------------------------------------------------------------
002800040119
002900040119      *---------------------------------------------------------------*
003000121218
003100121218     c     *entry        plist
003200121218     c                   parm                    kpjba
003300040119
003400121218      /free
003500121218
003600121218        wEoF = *off;
003700121218
003800150414       // Leggo file TARIFFE
003900121218        DOW not wEoF;
004000150414          read  TARIFFE_AU;
004100121218
004200121218       // fine file
004300150414          IF  %eof(TARIFFE_AU);
004400121218            leave;
004500121218          ENDIF;
004600150413
004700150414       // Cerco la tariffa
004800150414          IF  %subst(kpjbu:1:3) = 'STP';
004900150414            chain(n) (WFAksc:WFActrn:WFAprgn) TNTAM01L;
005000150414            IF  %found(TNTAM01L);
005100150414              exsr stampa;
005200150414            ENDIF;
005300150413          ELSE;
005400150414            chain (WFAksc:WFActrn:WFAprgn) TNTAM01L;
005500150414            IF  %found(TNTAM01L);
005600150414         // Aggiorno la % addizionale gestione anno corrente
005700150414              TAMppa = CMFpea;
005800150414              UPDATE TNTAM000;
005900150414            ENDIF;
006000150413          ENDIF;
006100121218        ENDDO;
006200121218
006300121218        *inLR = *ON;
006400140331
006500140331       //-------------------------------------------------------------**
006600150414       // Stampa tariffa
006700140331       //-------------------------------------------------------------**
006800140331        BEGSR stampa;
006900140331
007000140331          IF  not *in99 or *inof;
007100140331            except testa;
007200140331            *in99 = *on;
007300140331          ENDIF;
007400140331
007500140331          except riga;
007600140331
007700140331        ENDSR;
007800050330
007900121218      /end-free
008000140331
008100140331     oPRTF198   e            testa          2 02
008200141216     o                                           20 'BRT S.p.A.'
008300150414     o                                           60 '**  TARIFFE MOD **'
008400140331     o                       udate              110 '  /  /  '
008500150414     o                                          120 'TNVRMB25R'
008600140331     o                                          128 'Pag.'
008700140331     o                       page          Z    132
008800140331     o          e            testa          2
008900150414     o                                           10 'Tariffa'
009000140331     o          e            riga        1
009100150414     o                       TAMksc               7
009200150414     o                       TAMctr            +  1
009300150414     o                       TAMprg            +  1
009400150414     o                       CMFpea            +  1
009500150414     o                                         +  1 'Vecchia'
009600150414     o                       WFAppan           +  1
