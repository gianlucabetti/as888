000100070226     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200070226
000300070226      *------------------------------------------------------------------------*
000400070226      *
000500070226      *           PASSA DISTINTA PER PDA DA UN AUT ALL'ALTRO  ?
000600070226      *
000700070226      *------------------------------------------------------------------------*
000800130719     fFIPDO03T  uf a e           k DISK
000900080731     ffnorg03l  uf   e           k disk
001000070226
001100130719      * DS INTERNE/ESTERNE ?
001200070226     d fnlrd1ds      e ds
001300140129     d TIBS02DS      e ds
001400140129     d FNLRX3DS      e ds
001500130719
001600130719      * DS file FIPDO00T
001700130719     d FIPDOds       e ds                  extname(FIPDO00T)
001800140129
001900140129       //?kpjba?
002000140129     d KPJBA         e ds
002100130719
002200130719      * Flag Booleani
002300130719     d wEnd            s               n   inz(*off)
002400130719
002500130719      * Campi di comodo
002600130719     d wDatOra         s                   like(PDOdatora)
002700140129
002800140129      // Sk per filiali abilitate a Telefonate AUT
002900140129     d FilOkTelAut     ds           256
003000140129     d  skFilOkTelAut          1    255    dim(85)
003100130719
003200130719       // - File FIPDO03T key-list
003300130719     d kFIPDO03      e ds                  extname(FIPDO03T:*key)
003400130719     d                                     prefix(k_)
003500140129
003600140129       // -?Status ds?
003700140129     d Status         sds
003800140129     d   SDSpgm          *proc
003900140129     d   SDSusr              254    263
004000140129
004100140129      //---------------------------------------------------------------
004200140129      //?Definizione prototipi e programmi esterni utilizzati.
004300140129      //---------------------------------------------------------------
004400140129      /copy gaitrasrc/srcprotopr,FNLRX3R
004500140129      /copy gaitrasrc/srcprotopr,TIBS02R
004600070226
004700070226      *------------------------------------------------------------------------*
004800130719
004900130719      /free
005000130719         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005100130719      /end-free
005200080731
005300080731      * aggiorno FNORG
005400080731     c                   eval      orgfgs =lrd1fgs
005500080731     c                   eval      orgndc =lrd1ndc
005600080731     c     kfnorg        setll     fnorg03l
005700080731     c                   do        *hival
005800080731     c     kfnorg        reade     fnorg03l
005900080731     c                   if        %eof(fnorg03l)
006000080731     c                   leave
006100080731     c                   endif
006200080731     c                   eval      orgpdc = lrd1pdrn
006300080731     c                   update    fnorg000
006400140129      /free
006500140129       //?Richiamo routine per aggiornare anche le telefonate AUT
006600140129         exsr telAUT;
006700140129      /end-free
006800080731     c                   enddo
006900070226
007000080731      * aggiorno FIPDO
007100130719
007200130719       //?Nuove specifiche x aggiornate FIPDO
007300130719      /free
007400130719         wDatOra = %char(%timestamp:*iso0);
007500130719
007600130719       //?Leggo FIPDO x Distinta e AUT old per aggiornare i dati con AUT old
007700130719       //?e scrivere/aggiornare quello con AUT new
007800130719         exec sql
007900130719         DECLARE PDO cursor for
008000130719         SELECT * from FIPDO00T
008100130719         WHERE PDOfgs = :LRD1fgs and PDOndc = :LRD1ndc and
008200130719               PDOcodaut = :LRD1pdro and PDOatb = ' ';
008300130719
008400130719         exec sql
008500130719         OPEN PDO;
008600130719
008700130719       //?Leggo il file
008800130719         DOW  not wEnd;
008900130719           exec sql
009000130719           FETCH NEXT from PDO into :FIPDOds;
009100130719           IF  sqlcod = 100 or sqlcod < 0;
009200130719             wEnd = *on;
009300130719             leave;
009400130719           ENDIF;
009500130719
009600130719         //?Preparo la chiave
009700130719           k_PDOfgs = PDOfgs;
009800130719           k_PDOndc = PDOndc;
009900130719           k_PDOpoe = PDOpoe;
010000130719           k_PDOnsr = PDOnsr;
010100130719           k_PDOnor = PDOnor;
010200130719           k_PDOnrv = PDOnrv;
010300130719
010400130719         //?Aggiorno il rcd appena letto (AUT old)
010500130719           k_PDOcodaut = LRD1pdro;
010600130719           chain %kds(kFIPDO03) FIPDO03T;
010700130719           IF  %found(FIPDO03T);
010800130719             PDOatb = 'A';
010900130719             PDOidrow += 1;
011000130719             PDOdatora = wDatOra;
011100130719             UPDATE FIPDO000;
011200130719           ENDIF;
011300130719
011400130719         //?Aggiorno/Scrivo rcd con AUT new
011500130719           k_PDOcodaut = LRD1pdrn;
011600130719           chain %kds(kFIPDO03) FIPDO03T;
011700130719           IF  %found(FIPDO03T);
011800130719             clear PDOatb;
011900130719             PDOidrow += 1;
012000130719             PDOdatora = wDatOra;
012100130719             UPDATE FIPDO000;
012200130719           ENDIF;
012300130719           IF  not %found(FIPDO03T);
012400130719             clear PDOatb;
012500130719             PDOcodaut = LRD1pdrn;
012600130719             PDOidrow += 1;
012700130719             PDOdatora = wDatOra;
012800130719             WRITE  FIPDO000;
012900130719           ENDIF;
013000130719
013100130719         ENDDO;
013200130719
013300130719         exec sql
013400130719         CLOSE PDO;
013500130719
013600130719      /end-free
013700070226
013800070226     c                   eval      *inlr = *on
013900070226
014000070226      *------------------------------------------------------------------------*
014100070226      *? ROUTINE INIZIALE
014200070226      *------------------------------------------------------------------------*
014300070226     c     *inzsr        begsr
014400070226
014500070226     c     *entry        plist
014600070226     c                   parm                    fnlrd1ds
014700140129
014800140129      /free
014900140129       //?Imposto utente in kpjba che serve a FNLRX3R
015000140129         KNMUS = SDSusr;
015100140129
015200140129       //?Carico VPO x schierare le filiali abilitate a prg.816 Telefonate AUT
015300140129         clear TIBS02DS;
015400140129         T02mod = 'C';
015500140129         T02cod = 'VPO';
015600140129         T02ke1 ='DECOFI816TEL';
015700140129         T02sif = knsif;
015800140129         TNTBE_RicercaControllo (kpjba:tibs02ds);
015900140129         IF  T02err = *blanks;
016000140129           FilOkTelAut = T02uni;
016100140129         ENDIF;
016200140129
016300140129      /end-free
016400080731
016500080731     c     kfnorg        klist
016600080731     c                   kfld                    orgfgs
016700080731     c                   kfld                    orgndc
016800070226
016900070226     c                   endsr
017000140129      /free
017100140129       //--------------------------------------------------------------
017200140129       //?Richiamo pgm x Aggiornare FICAU.                             ?
017300140129       //--------------------------------------------------------------
017400140129       BEGSR  telAUT;
017500140129
017600140129       //?Se la filiale distinta è attiva alle telefonate AUT
017700140129         IF  skFilOkTelAut(1) = '999' or
017800140129             %lookup(%editc(LRD1fgs:'X'):skFilOkTelAut) > 0;
017900140129
018000140129         //?Richiamo FNLRX3R prima con 'A' di annullato e vecchio codice AUT
018100140129           clear FNLRX3DS;
018200140129           IX3tpdoc = 'R';
018300140129           IX3iddoc = %editc(ORGpoe : 'X' ) +
018400140129                             %editc(ORGnsr : 'X' ) +
018500140129                             %editc(ORGnor : 'X' ) +
018600140129                             %editc(ORGnrv : 'X' );
018700140129           IX3fgs = LRD1fgs;
018800140129           IX3nfv = LRD1ndc;
018900140129           IX3dfv = ORGddc;
019000140129           IX3pdr = LRD1pdro;
019100140129           IX3cmd = 'A';
019200140129           fnlrx3r (kpjba:FNLRX3DS);
019300140129
019400140129         //?Richiamo FNLRX3R poi con 'I' di inserito e nuovo codice AUT
019500140129           IX3pdr = LRD1pdrn;
019600140129           IX3cmd = 'I';
019700140129           fnlrx3r (kpjba:FNLRX3DS);
019800140129
019900140129         ENDIF;
020000140129
020100140129       ENDSR;
020200140129      /end-free
