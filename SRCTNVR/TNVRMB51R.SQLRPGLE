000100120423      //---------------------------------------------------------------
000200120423      //
000300160919      //?      Cancella Tariffe DPD
000400120423      //
000500120423      //---------------------------------------------------------------
000600120423     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000700120423
000800120423      //---------------------------------------------------------------
000900120423      //?Dichiarazione file.
001000120423      //---------------------------------------------------------------
001100160919      // - File Tariffe
001200160919     fTNTAM00L  uf   e           k disk
001300160919     fTITAD04L  uf   e           k disk
001400160919     fTITPT01L  uf   e           k disk
001500160919     fTITPD01L  uf   e           k disk
001600160919     fTITGC01L  uf   e           k disk
001700120423
001800120423      //---------------------------------------------------------------
001900120423      //?Definizione costanti.
002000120423      //---------------------------------------------------------------
002100120423
002200120423      //---------------------------------------------------------------
002300120423      //?Definizione schiere.
002400120423      //---------------------------------------------------------------
002500120423
002600120423      //---------------------------------------------------------------
002700120423      //?Definizione aree dati.
002800120423      //---------------------------------------------------------------
002900120423
003000120423      //---------------------------------------------------------------
003100120423      //?Definizione strutture dati.
003200120423      //---------------------------------------------------------------
003300160919      // - Testata Tariffe
003400160919     d tntamds       e ds                  extname(TNTAM00F) prefix(w)
003500120423
003600120423      //---------------------------------------------------------------
003700120423      //?Definizione variabili globali.
003800120423      //---------------------------------------------------------------
003900120423      // - Flags booleani
004000120423     d wEoF            s               n   inz(*off)
004100120423
004200120423      //---------------------------------------------------------------
004300120423      //?Definizione procedure esterne.
004400120423      //---------------------------------------------------------------
004500120423
004600120423      //---------------------------------------------------------------
004700120423      //?Definizione prototipi.
004800120423      //---------------------------------------------------------------
004900120423
005000120423      //---------------------------------------------------------------
005100120423      //?Definizione key-list.
005200120423      //---------------------------------------------------------------
005300120423
005400120423      //---------------------------------------------------------------
005500120423      //?Riepilogo indicatori.
005600120423      //---------------------------------------------------------------
005700120423
005800120423      //---------------------------------------------------------------
005900120423      //?M A I N - L I N E
006000120423      //---------------------------------------------------------------
006100120423
006200120423      /free
006300120423
006400120423       //?Operazioni iniziali
006500120423       exsr RoutInz;
006600120423
006700120430       //?Elabora file
006800120430       exsr Elabora;
006900120423
007000120423       //?Operazioni finali
007100120423       exsr RoutEnd;
007200120423
007300120423       //--------------------------------------------------------------
007400120423       //?Operazioni iniziali.
007500120423       //--------------------------------------------------------------
007600120423       BEGSR RoutInz;
007700120706
007800120706       //?Impostazione opzioni per SQL?
007900160919         exec sql set option DynUsrPrf = *Owner, CloSqlCsr = *EndMod;
008000120423
008100120423       ENDSR;
008200120423
008300120423       //--------------------------------------------------------------
008400120430       //?Elabora file
008500120423       //--------------------------------------------------------------
008600120430       BEGSR Elabora;
008700120423
008800120423         wEoF = *off;
008900120423
009000160919       //?Leggo file Testata Tariffe solo CTR 199 DDT 20170101
009100120706         exec sql
009200160919           DECLARE  TAM cursor FOR
009300160919           SELECT   TAMksc, TAMctr, TAMprg from TNTAM00F
009400160919           WHERE TAMctr = 199 and TAMddt = 20170101
009500160919           ORDER BY TAMksc, TAMctr, TAMprg;
009600120706
009700120706         exec sql
009800160919           open TAM;
009900120706           IF sqlcode < 0;
010000120706             wEoF = *on;
010100120706           ENDIF;
010200120706
010300120706         DOW not wEoF;
010400120706           exec sql
010500160919             FETCH NEXT from TAM into :wTAMksc, :wTAMctr, :wTAMprg;
010600120706           IF sqlcod = 100 or sqlcod < 0;
010700120706             wEOF = *on;
010800120706             leave;
010900120706           ENDIF;
011000120706
011100130125         //?Cancello le anagrafiche provvisorie
011200160919           DELETE (wTAMksc: wTAMctr: wTAMprg) TNTAM00L;
011300160919
011400160919           setll (wTAMksc: wTAMctr: wTAMprg) TITAD04L;
011500160919           reade (wTAMksc: wTAMctr: wTAMprg) TITAD04L;
011600160919           DOW  not %eof(TITAD04L);
011700160919             delete TITAD000;
011800160919             reade (wTAMksc: wTAMctr: wTAMprg) TITAD04L;
011900160919           ENDDO;
012000160919
012100160919           setll (wTAMksc: wTAMctr: wTAMprg) TITPT01L;
012200160919           reade (wTAMksc: wTAMctr: wTAMprg) TITPT01L;
012300160919           DOW  not %eof(TITPT01L);
012400160919             delete TITPT000;
012500160919             reade (wTAMksc: wTAMctr: wTAMprg) TITPT01L;
012600160919           ENDDO;
012700160919
012800160919           setll (wTAMksc: wTAMctr: wTAMprg) TITPD01L;
012900160919           reade (wTAMksc: wTAMctr: wTAMprg) TITPD01L;
013000160919           DOW  not %eof(TITPD01L);
013100160919             delete TITPD000;
013200160919             reade (wTAMksc: wTAMctr: wTAMprg) TITPD01L;
013300160919           ENDDO;
013400160919
013500160919           DELETE (wTAMksc: wTAMctr: wTAMprg) TITGC01L;
013600120706
013700120706         ENDDO;
013800120706
013900120706       //?Chiudo il cursore
014000120706         exec sql
014100160919           close TAM;
014200120706
014300120706       ENDSR;
014400130125
014500120423       //--------------------------------------------------------------
014600120423       //?Operazioni finali.
014700120423       //--------------------------------------------------------------
014800120423       BEGSR RoutEnd;
014900120423
015000120423         *inLR = *on;
015100120423         return;
015200120423
015300120423       ENDSR;
015400051221
015500051221      /end-free
015600051221
