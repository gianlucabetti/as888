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
001300130719
001400130719      * DS file FIPDO00T
001500130719     d FIPDOds       e ds                  extname(FIPDO00T)
001600130719
001700130719      * Flag Booleani
001800130719     d wEnd            s               n   inz(*off)
001900130719
002000130719      * Campi di comodo
002100130719     d wDatOra         s                   like(PDOdatora)
002200130719
002300130719       // - File FIPDO03T key-list
002400130719     d kFIPDO03      e ds                  extname(FIPDO03T:*key)
002500130719     d                                     prefix(k_)
002600070226
002700070226      *------------------------------------------------------------------------*
002800130719
002900130719      /free
003000130719         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003100130719      /end-free
003200080731
003300080731      * aggiorno FNORG
003400080731     c                   eval      orgfgs =lrd1fgs
003500080731     c                   eval      orgndc =lrd1ndc
003600080731     c     kfnorg        setll     fnorg03l
003700080731     c                   do        *hival
003800080731     c     kfnorg        reade     fnorg03l
003900080731     c                   if        %eof(fnorg03l)
004000080731     c                   leave
004100080731     c                   endif
004200080731     c                   eval      orgpdc = lrd1pdrn
004300080731     c                   update    fnorg000
004400080731     c                   enddo
004500070226
004600080731      * aggiorno FIPDO
004700130719
004800130719       //?Nuove specifiche x aggiornate FIPDO
004900130719      /free
005000130719         wDatOra = %char(%timestamp:*iso0);
005100130719
005200130719       //?Leggo FIPDO x Distinta e AUT old per aggiornare i dati con AUT old
005300130719       //?e scrivere/aggiornare quello con AUT new
005400130719         exec sql
005500130719         DECLARE PDO cursor for
005600130719         SELECT * from FIPDO00T
005700130719         WHERE PDOfgs = :LRD1fgs and PDOndc = :LRD1ndc and
005800130719               PDOcodaut = :LRD1pdro and PDOatb = ' ';
005900130719
006000130719         exec sql
006100130719         OPEN PDO;
006200130719
006300130719       //?Leggo il file
006400130719         DOW  not wEnd;
006500130719           exec sql
006600130719           FETCH NEXT from PDO into :FIPDOds;
006700130719           IF  sqlcod = 100 or sqlcod < 0;
006800130719             wEnd = *on;
006900130719             leave;
007000130719           ENDIF;
007100130719
007200130719         //?Preparo la chiave
007300130719           k_PDOfgs = PDOfgs;
007400130719           k_PDOndc = PDOndc;
007500130719           k_PDOpoe = PDOpoe;
007600130719           k_PDOnsr = PDOnsr;
007700130719           k_PDOnor = PDOnor;
007800130719           k_PDOnrv = PDOnrv;
007900130719
008000130719         //?Aggiorno il rcd appena letto (AUT old)
008100130719           k_PDOcodaut = LRD1pdro;
008200130719           chain %kds(kFIPDO03) FIPDO03T;
008300130719           IF  %found(FIPDO03T);
008400130719             PDOatb = 'A';
008500130719             PDOidrow += 1;
008600130719             PDOdatora = wDatOra;
008700130719             UPDATE FIPDO000;
008800130719           ENDIF;
008900130719
009000130719         //?Aggiorno/Scrivo rcd con AUT new
009100130719           k_PDOcodaut = LRD1pdrn;
009200130719           chain %kds(kFIPDO03) FIPDO03T;
009300130719           IF  %found(FIPDO03T);
009400130719             clear PDOatb;
009500130719             PDOidrow += 1;
009600130719             PDOdatora = wDatOra;
009700130719             UPDATE FIPDO000;
009800130719           ENDIF;
009900130719           IF  not %found(FIPDO03T);
010000130719             clear PDOatb;
010100130719             PDOcodaut = LRD1pdrn;
010200130719             PDOidrow += 1;
010300130719             PDOdatora = wDatOra;
010400130719             WRITE  FIPDO000;
010500130719           ENDIF;
010600130719
010700130719         ENDDO;
010800130719
010900130719         exec sql
011000130719         CLOSE PDO;
011100130719
011200130719      /end-free
011300070226
011400070226     c                   eval      *inlr = *on
011500070226
011600070226      *------------------------------------------------------------------------*
011700070226      *? ROUTINE INIZIALE
011800070226      *------------------------------------------------------------------------*
011900070226     c     *inzsr        begsr
012000070226
012100070226     c     *entry        plist
012200070226     c                   parm                    fnlrd1ds
012300080731
012400080731     c     kfnorg        klist
012500080731     c                   kfld                    orgfgs
012600080731     c                   kfld                    orgndc
012700070226
012800070226     c                   endsr
