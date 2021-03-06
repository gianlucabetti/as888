000100100413      //---------------------------------------------------------------
000200101203      //?TNVRMB69R - Elimina TFNTC su clienti no in CNACO
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000101203     fTFNTC01L  uf   e           k disk    rename(TFNTC:TFNTC000)
001100101203     fCNACO00F  if   e           k disk
001200101203     fNTC       o    e             disk    rename(NTC:NTCMB)
001300101203     fTNVRMBP   o    e             printer oflind(*in99)
001400100413
001500100413      //---------------------------------------------------------------
001600100413      //?Definizione costanti.
001700100413      //---------------------------------------------------------------
001800100413
001900100413      //---------------------------------------------------------------
002000100413      //?Definizione schiere.
002100100413      //---------------------------------------------------------------
002200100513
002300100726     d aggiorna        s              2a
002400090715
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione aree dati.
002700100413      //---------------------------------------------------------------
002800100413
002900100413      //---------------------------------------------------------------
003000100413      //?Definizione strutture dati.
003100100413      //---------------------------------------------------------------
003200100415
003300100415     d KPJBA         e ds
003400100415
003500100413      //---------------------------------------------------------------
003600100413      //?Definizione variabili globali.
003700100413      //---------------------------------------------------------------
003800100413
003900100413      // - Flags booleani
004000100413     d $End            s               n   inz(*off)
004100101203     d $NoACO          s               n   inz(*off)
004200100414     d $Stampa         s               n   inz(*off)
004300100413
004400100413      // - Campi di comodo
004500101203     d TotNtc          s              7s 0
004600100415     d wDataOggi       s              8  0
004700100414     d wData_ISO       s               d   datfmt(*iso)
004800100415     d wRiga           s              2  0
004900101203     d w0070           s              7  0
005000100413
005100100413      //---------------------------------------------------------------
005200100413      //?Definizione procedure esterne.
005300100413      //---------------------------------------------------------------
005400100413
005500100413      //---------------------------------------------------------------
005600100413      //?prototipi
005700100413      //---------------------------------------------------------------
005800100414
005900100413
006000100413      //---------------------------------------------------------------
006100100413      //?Definizione key-list.
006200100413      //---------------------------------------------------------------
006300100415
006400100413
006500100413      //---------------------------------------------------------------
006600100413      //?Riepilogo indicatori.
006700100413      //---------------------------------------------------------------
006800100414
006900100414      // 99    : Salto pagina
007000100413
007100100413      //---------------------------------------------------------------
007200100413      //?M A I N - L I N E
007300100413      //---------------------------------------------------------------
007400100413
007500100413     c     *Entry        plist
007600100726     c                   parm                    aggiorna
007700100413
007800100413      /free
007900100413
008000100413       //?Operazioni iniziali
008100100513       exsr RoutInz;
008200100413
008300100726       //?Elaboro
008400100726       exsr Elabora;
008500100413
008600100413       //?Operazioni finali
008700100513       exsr RoutEnd;
008800100413
008900100413       //--------------------------------------------------------------
009000100413       //?Operazioni iniziali.
009100100413       //--------------------------------------------------------------
009200100513       BEGSR RoutInz;
009300100726
009400100726         IF  aggiorna <> 'SI' and aggiorna <> 'NO';
009500100726           aggiorna = 'NO';
009600100726         ENDIF;
009700100414
009800100414         wData_ISO = %date();
009900100415         wDataOggi = %dec(wData_ISO);
010000100726
010100100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010200100514
010300101203         clear TotNtc;
010400100413
010500100413       ENDSR;
010600100413
010700100413       //--------------------------------------------------------------
010800100422       //?Elaboro.
010900100413       //--------------------------------------------------------------
011000100513       BEGSR Elabora;
011100100413
011200100413         $End = *off;
011300100413
011400101203       //?Estraggo solo le note Clienti
011500100413         exec sql
011600101203         DECLARE A cursor for
011700101203         SELECT  NTCapl, NTCnk1, NTCnk2, NTCtnt
011800101203         FROM TFNTC00F
011900101203         WHERE NTCapl = 'C'
012000101203         ORDER by NTCnk1, NTCnk2, NTCtnt;
012100100413
012200101203         exec sql OPEN A;
012300100413
012400100413         DOW  not $End;
012500101203           exec sql FETCH next from A into :NTCapl, :NTCnk1,
012600101203                                           :NTCnk2, :NTCtnt;
012700100413           IF  sqlcod = 100 or sqlcod < 0;
012800100413             $End = *on;
012900100413             leave;
013000100413           ENDIF;
013100100422
013200101203         //?Controllo se esiste il cliente su CNACO
013300101203           exsr CtrAco;
013400100726
013500101203         //?Se NON trovo CNACO
013600101203           IF  $NoAco;
013700100726
013800101203           //?Cancello la nota
013900101203             TotNtc += 1;
014000101203             IF  aggiorna = 'NO';
014100101203               write NTCMB;
014200101203             ENDIF;
014300101203             IF  aggiorna = 'SI';
014400101203               exsr DelNtc;
014500101203             ENDIF;
014600100726
014700100726           ENDIF;
014800100413         ENDDO;
014900100413
015000101203         exec sql CLOSE A;
015100100514
015200100514       //?Stampa totali
015300100514         wRiga = 00;
015400100514         exsr Stampa;
015500100413
015600100413       ENDSR;
015700100726
015800100726       //--------------------------------------------------------------
015900101203       //?Controllo se esiste il cliente su CNACO.
016000100726       //--------------------------------------------------------------
016100101203       BEGSR CtrAco;
016200100726
016300101203         $NoAco = *off;
016400101203
016500101203       //?Controllo se il cliente esiste su CNACO
016600101203         w0070 = %dec(%subst(NTCnk1:5:7):7:0);
016700101203         ACOkut = 1;
016800101203         ACOkcc = 151;
016900101203         chain (ACOkut:ACOkcc:w0070) CNACO00F;
017000101203         IF  not %found(CNACO00F);
017100101203           $NoAco = *on;
017200101203         ENDIF;
017300100726
017400100726       ENDSR;
017500100413
017600100413       //--------------------------------------------------------------
017700101203       //?Cancello la nota.
017800100413       //--------------------------------------------------------------
017900101203       BEGSR DelNtc;
018000100726
018100101203       //?Aggancio il file TFNTC
018200101203         chain (NTCapl:NTCnk1:NTCnk2:NTCtnt) TFNTC01L;
018300101203       //?Se non trovo vado via
018400101203         IF  not %Found(TFNTC01L);
018500100726           leavesr;
018600100726         ENDIF;
018700100422
018800101203       //?Cancello la nota
018900101203         DELETE TFNTC000;
019000100413
019100100413       ENDSR;
019200100414
019300100414       //--------------------------------------------------------------
019400101203       //?Stampa tot note cancellate
019500100414       //--------------------------------------------------------------
019600100513       BEGSR Stampa;
019700100414
019800100519         IF  not $Stampa or *in99;
019900101203           write TESTATA;
020000100414           $Stampa = *on;
020100100519           *in99 = *off;
020200100414         ENDIF;
020300100811
020400101203         write DETTAGLIO;
020500100414
020600100414       ENDSR;
020700100413
020800100413       //--------------------------------------------------------------
020900100413       //?Operazioni finali.
021000100413       //--------------------------------------------------------------
021100100513       BEGSR RoutEnd;
021200100413
021300100413         *inLR = *on;
021400100413         return;
021500100413
021600100413       ENDSR;
021700100413
021800100413      /end-free
