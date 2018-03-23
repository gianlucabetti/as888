000100121218      //---------------------------------------------------------------
000200121218      //
000300140613      //?        Controllo cappario GB tra BRT e DPD
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100140613     fDPCRO01L  if   e           k disk
001200140616     fCAPDPD01  if a e           k disk
001300040119
001400040119      *---------------------------------------------------------------*
001500121218
001600121218       // - Campi di comodo
001700140714     d k_verdpd        s                   like(CROver)  inz(52)
001800140613     d k_nazdpd        s                   like(CROiso2) inz('GB')
001900140613
002000140613     d lenpredpd       s              2s 0 inz
002100140613     d predpd          s              4a   inz
002200140613     d sav_predpd      s              4a   inz
002300121218
002400121218       // - flag booleani
002500121218     d wEoF            s               n
002600140613
002700140613       //--------------------------------------------------------------
002800140613       //?Definizione strutture dati.                                  ?
002900140613       //--------------------------------------------------------------
003000140613     d KPJBA         e ds
003100140616
003200140616     d capdpdds      e ds                  extname(capdpd)
003300040119
003400140613       //--------------------------------------------------------------
003500140613       //?M A I N - L I N E
003600140613       //---------------------------------------------------------------
003700140613
003800140613     c     *entry        plist
003900140613     c                   parm                    KPJBA
004000040119
004100121218      /free
004200140616
004300140616       // Scrivo file di work con cap DPD solo prefissi
004400140616         exsr newcapdpd;
004500140616         clear predpd;
004600121218
004700140613         *inLR = *ON;
004800140616
004900140616       //--------------------------------------------------------------
005000140616       // Creo new cap DPD solo prefisso.                              ?
005100140616       //--------------------------------------------------------------
005200140616       BEGSR  newcapDPD;
005300140616
005400140616         wEoF = *off;
005500140616         clear sav_predpd;
005600140616
005700140616       // Leggo cappario DPD e scrivo file work
005800140616         setll (k_verdpd:k_nazdpd) DPCRO01L;
005900140616         reade (k_verdpd:k_nazdpd) DPCRO01L;
006000140616         DOW  not wEoF;
006100140616           IF  %eof(DPCRO01L);
006200140616             sav_predpd = predpd;
006300140616             leave;
006400140616           ENDIF;
006500140616
006600140616           reset lenpredpd;
006700140616           reset predpd;
006800140616
006900140616         // calcolo quanto è pieno il campo
007000140616           lenpredpd = %checkr(' ' : CROptcb);
007100140616         // calcolo il prefisso DPD
007200140616           SELECT;
007300140616         // prefisso CAP lungo 4
007400140616           WHEN  lenpredpd = 7;
007500140616             predpd = %subst(CROptcb:1:4);
007600140616             lenpredpd = 4;
007700140616         // prefisso CAP lungo 3
007800140616           WHEN  lenpredpd = 6;
007900140616             predpd = %subst(CROptcb:1:3);
008000140616             lenpredpd = 3;
008100140616         // prefisso CAP lungo 2
008200140616           WHEN  lenpredpd = 5;
008300140616             predpd = %subst(CROptcb:1:2);
008400140616             lenpredpd = 2;
008500140616           ENDSL;
008600140616
008700140616           IF  sav_predpd <> *blanks and
008800140616               sav_predpd <> predpd;
008900140616             chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
009000140616             IF  not %found(CAPDPD01);
009100140616               clear capdpd00;
009200140616               cap = sav_predpd;
009300140616               ver = k_verdpd;
009400140616               naz = k_nazdpd;
009500140616               write CAPDPD00;
009600140616             ENDIF;
009700140616             sav_predpd = predpd;
009800140616           ENDIF;
009900140616
010000140616           IF  sav_predpd = *blanks;
010100140616             sav_predpd = predpd;
010200140616           ENDIF;
010300140616
010400140616           reade (k_verdpd:k_nazdpd) DPCRO01L;
010500140616         ENDDO;
010600140616
010700140616         chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
010800140616         IF  not %found(CAPDPD01);
010900140616           clear capdpd00;
011000140616           cap = sav_predpd;
011100140616           ver = k_verdpd;
011200140616           naz = k_nazdpd;
011300140616           write CAPDPD00;
011400140616         ENDIF;
011500140616
011600140616       ENDSR;
011700050330
011800121218      /end-free
011900140331
