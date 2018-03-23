000100070226     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200070226
000300070226      *------------------------------------------------------------------------*
000400070226      *
000500070226      *           PASSA DISTINTA PER PDA DA UN AUT ALL'ALTRO  ?
000600070226      *
000700070226      *------------------------------------------------------------------------*
000800130719     fFIPDO03T  uf a e           k DISK
000900080731     ffnorg03l  uf   e           k disk
001000140212       // -?Distinte uscite AUT (Consegna/Ritiro)?
001100140212     fFIDST01L  if   e           k disk
001200140212
001300070226
001400130719      * DS INTERNE/ESTERNE ?
001500070226     d fnlrd1ds      e ds
001600140129     d TIBS02DS      e ds
001700140129     d FNLRX3DS      e ds
001800130719
001900130719      * DS file FIPDO00T
002000130719     d FIPDOds       e ds                  extname(FIPDO00T)
002100140212
002200140212       // -?DS lettura campo DSTFLR di FIDST00F?
002300140212     d dDSTflr       e ds                  inz   qualified
002400140212
002500140129
002600140129       //?kpjba?
002700140129     d KPJBA         e ds
002800130719
002900130719      * Flag Booleani
003000130719     d wEnd            s               n   inz(*off)
003100140212     d Oktel           s               n   inz(*off)
003200130719
003300130719      * Campi di comodo
003400130719     d wDatOra         s                   like(PDOdatora)
003500130719
003600130719       // - File FIPDO03T key-list
003700130719     d kFIPDO03      e ds                  extname(FIPDO03T:*key)
003800130719     d                                     prefix(k_)
003900140212
004000140212     d keyFIDST01    e ds                  extname( FIDST01L : *key )
004100140212     d                                     prefix(k_)   inz
004200140129
004300140129       // -?Status ds?
004400140129     d Status         sds
004500140129     d   SDSpgm          *proc
004600140129     d   SDSusr              254    263
004700140129
004800140129      //---------------------------------------------------------------
004900140129      //?Definizione prototipi e programmi esterni utilizzati.
005000140129      //---------------------------------------------------------------
005100140129      /copy gaitrasrc/srcprotopr,FNLRX3R
005200140129      /copy gaitrasrc/srcprotopr,TIBS02R
005300070226
005400070226      *------------------------------------------------------------------------*
005500130719
005600130719      /free
005700130719         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005800140212         oKtel  = *off;
005900140212         // -?Impostazione campi chiave?
006000140212         clear  keyFIDST01;
006100140212         k_DSTfgs = lrd1fgs ;
006200140212         k_DSTnfv = lrd1ndc ;
006300140212         k_DSTnpg = 4;
006400140212         // -?Verifica distinta?
006500140212         chain  %kds( keyFIDST01 )  FIDST000;
006600140212         if   %found(FIDST01L);
006700140212
006800140212         // -?Flag "Dati a PDA" & "Test su PDA"?
006900140212         dDSTflr = DSTflr;
007000140212         select;
007100140212           // ·?Distinta a PDA per ORM & in test x ORM => OK?
007200140212           when  (DSTpda = 'E'  or  DSTpda = 'O')  and
007300140212                 (dDSTflr.§DSTtstPda = 'O'  or  dDSTflr.§DSTtstPda = 'E');
007400140212                 oKtel  = *on ;
007500140212           // ·?Distinta NO a PDA per ORM => OK?
007600140212           when  (DSTpda <> 'E'  and  DSTpda <> 'O');
007700140212                 oKtel  = *on ;
007800140212           // ·?Altrimanti: da scartare?
007900140212           other;
008000140212                 oKtel  = *off;
008100140212         endsl;
008200140212         endif;
008300080731
008400140212      /end-free
008500080731      * aggiorno FNORG
008600080731     c                   eval      orgfgs =lrd1fgs
008700080731     c                   eval      orgndc =lrd1ndc
008800080731     c     kfnorg        setll     fnorg03l
008900080731     c                   do        *hival
009000080731     c     kfnorg        reade     fnorg03l
009100080731     c                   if        %eof(fnorg03l)
009200080731     c                   leave
009300080731     c                   endif
009400080731     c                   eval      orgpdc = lrd1pdrn
009500080731     c                   update    fnorg000
009600140129      /free
009700140212       //?Richiamo routine per aggiornare anche le telefonate AUT
009800140212
009900140212            exsr telAUT;
010000140212
010100140129      /end-free
010200080731     c                   enddo
010300070226
010400080731      * aggiorno FIPDO
010500130719
010600130719       //?Nuove specifiche x aggiornate FIPDO
010700130719      /free
010800130719         wDatOra = %char(%timestamp:*iso0);
010900130719
011000130719       //?Leggo FIPDO x Distinta e AUT old per aggiornare i dati con AUT old
011100130719       //?e scrivere/aggiornare quello con AUT new
011200130719         exec sql
011300130719         DECLARE PDO cursor for
011400130719         SELECT * from FIPDO00T
011500130719         WHERE PDOfgs = :LRD1fgs and PDOndc = :LRD1ndc and
011600130719               PDOcodaut = :LRD1pdro and PDOatb = ' ';
011700130719
011800130719         exec sql
011900130719         OPEN PDO;
012000130719
012100130719       //?Leggo il file
012200130719         DOW  not wEnd;
012300130719           exec sql
012400130719           FETCH NEXT from PDO into :FIPDOds;
012500130719           IF  sqlcod = 100 or sqlcod < 0;
012600130719             wEnd = *on;
012700130719             leave;
012800130719           ENDIF;
012900130719
013000130719         //?Preparo la chiave
013100130719           k_PDOfgs = PDOfgs;
013200130719           k_PDOndc = PDOndc;
013300130719           k_PDOpoe = PDOpoe;
013400130719           k_PDOnsr = PDOnsr;
013500130719           k_PDOnor = PDOnor;
013600130719           k_PDOnrv = PDOnrv;
013700130719
013800130719         //?Aggiorno il rcd appena letto (AUT old)
013900130719           k_PDOcodaut = LRD1pdro;
014000130719           chain %kds(kFIPDO03) FIPDO03T;
014100130719           IF  %found(FIPDO03T);
014200130719             PDOatb = 'A';
014300130719             PDOidrow += 1;
014400130719             PDOdatora = wDatOra;
014500130719             UPDATE FIPDO000;
014600130719           ENDIF;
014700130719
014800130719         //?Aggiorno/Scrivo rcd con AUT new
014900130719           k_PDOcodaut = LRD1pdrn;
015000130719           chain %kds(kFIPDO03) FIPDO03T;
015100130719           IF  %found(FIPDO03T);
015200130719             clear PDOatb;
015300130719             PDOidrow += 1;
015400130719             PDOdatora = wDatOra;
015500130719             UPDATE FIPDO000;
015600130719           ENDIF;
015700130719           IF  not %found(FIPDO03T);
015800130719             clear PDOatb;
015900130719             PDOcodaut = LRD1pdrn;
016000130719             PDOidrow += 1;
016100130719             PDOdatora = wDatOra;
016200130719             WRITE  FIPDO000;
016300130719           ENDIF;
016400130719
016500130719         ENDDO;
016600130719
016700130719         exec sql
016800130719         CLOSE PDO;
016900130719
017000130719      /end-free
017100070226
017200070226     c                   eval      *inlr = *on
017300070226
017400070226      *------------------------------------------------------------------------*
017500070226      *? ROUTINE INIZIALE
017600070226      *------------------------------------------------------------------------*
017700070226     c     *inzsr        begsr
017800070226
017900070226     c     *entry        plist
018000070226     c                   parm                    fnlrd1ds
018100140129
018200140129      /free
018300140129       //?Imposto utente in kpjba che serve a FNLRX3R
018400140129         KNMUS = SDSusr;
018500140129
018600140129      /end-free
018700080731
018800080731     c     kfnorg        klist
018900080731     c                   kfld                    orgfgs
019000080731     c                   kfld                    orgndc
019100070226
019200070226     c                   endsr
019300140129      /free
019400140129       //--------------------------------------------------------------
019500140129       //?Richiamo pgm x Aggiornare FICAU.                             ?
019600140129       //--------------------------------------------------------------
019700140129       BEGSR  telAUT;
019800140129
019900140129
020000140129         //?Richiamo FNLRX3R prima con 'A' di annullato e vecchio codice AUT
020100140129           clear FNLRX3DS;
020200140129           IX3tpdoc = 'R';
020300140129           IX3iddoc = %editc(ORGpoe : 'X' ) +
020400140129                             %editc(ORGnsr : 'X' ) +
020500140129                             %editc(ORGnor : 'X' ) +
020600140129                             %editc(ORGnrv : 'X' );
020700140129           IX3fgs = LRD1fgs;
020800140129           IX3nfv = LRD1ndc;
020900140129           IX3dfv = ORGddc;
021000140129           IX3pdr = LRD1pdro;
021100140129           IX3cmd = 'A';
021200140129           fnlrx3r (kpjba:FNLRX3DS);
021300140129
021400140212         //?Richiamo FNLRX3R poi con 'I' di inserito e nuovo codice AUT  solo se distinta no PDA o
021500140212         //?PDA in test
021600140212           If oktel = *on ;
021700140212              IX3pdr = LRD1pdrn;
021800140212              IX3cmd = 'I';
021900140212              fnlrx3r (kpjba:FNLRX3DS);
022000140212           Endif ;
022100140129
022200140129       ENDSR;
022300140129      /end-free
