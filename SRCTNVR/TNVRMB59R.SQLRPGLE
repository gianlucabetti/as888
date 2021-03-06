000100170314      //---------------------------------------------------------------
000200170407      //?TNVRMB59R - Pulisco rcd 'AA' di FNACRE doppi
000300170314      //---------------------------------------------------------------
000400170314     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170314
000600170314      //---------------------------------------------------------------
000700170314      //?Dichiarazione file.
000800170314      //---------------------------------------------------------------
000900170407     fFNACRE0F  uf   e             disk
001000170314
001100170314      //---------------------------------------------------------------
001200170314      //?Definizione costanti.
001300170314      //---------------------------------------------------------------
001400170314
001500170314      //---------------------------------------------------------------
001600170314      //?Definizione schiere.
001700170314      //---------------------------------------------------------------
001800170314
001900170314      //---------------------------------------------------------------
002000170314      //?Definizione aree dati.
002100170314      //---------------------------------------------------------------
002200170314      // - Dati utente
002300170314     d �AzUte        e ds                  extname(AZUTE00F)
002400170314     d                                     dtaara
002500170314     d �DatiUte      e ds                  extname(dDatiUte)
002600170314     d                                     dtaara
002700170314
002800170314      //---------------------------------------------------------------
002900170314      //?Definizione strutture dati.
003000170314      //---------------------------------------------------------------
003100170314      // - Parametri ricevuti
003200170314     d KPJBA         e ds
003300170407
003400170407      // - Ds File FNACRE0F
003500170407     d FNACREDS      e ds                  extname(FNACRE0F)
003600170407     d                                     prefix(w)
003700170314
003800170314      //---------------------------------------------------------------
003900170314      //?Definizione variabili globali.
004000170314      //---------------------------------------------------------------
004100170314      // - Flags booleani
004200170314     d Fine            s               n   inz(*off)
004300170314     d wEoF            s               n   inz(*off)
004400170314
004500170314      // - Campi di comodo
004600170407     d conta           s              2s 0 inz
004700170410     d rrnAcre         s              9s 0 inz
004800170407     d sav_CRO         s                   like(ACREcro)
004900170314
005000170314      //---------------------------------------------------------------
005100170314      //?Definizione procedure esterne.
005200170314      //---------------------------------------------------------------
005300170314
005400170314      //---------------------------------------------------------------
005500170314      //?Definizione prototipi.
005600170314      //---------------------------------------------------------------
005700170314
005800170314      //---------------------------------------------------------------
005900170314      //?Definizione key-list.
006000170314      //---------------------------------------------------------------
006100170314
006200170314      //---------------------------------------------------------------
006300170314      //?Riepilogo indicatori.
006400170314      //---------------------------------------------------------------
006500170314
006600170314      //---------------------------------------------------------------
006700170314      //?M A I N - L I N E
006800170314      //---------------------------------------------------------------
006900170314     c     *Entry        plist
007000170314     c                   parm                    kpjba
007100170314
007200170314      /free
007300170314
007400170314       //?Operazioni iniziali
007500170314       exsr RoutInz;
007600170314
007700170407       //?Elaborazione principale
007800170407       exsr Elabora;
007900170314
008000170314       //?Operazioni finali
008100170314       exsr RoutEnd;
008200170314
008300170314       //--------------------------------------------------------------
008400170314       //?Operazioni iniziali.
008500170314       //--------------------------------------------------------------
008600170314       BEGSR RoutInz;
008700170314
008800170314         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008900170314
009000170314       ENDSR;
009100170314
009200170314       //--------------------------------------------------------------
009300170407       //?Elaborazione principale.
009400170314       //--------------------------------------------------------------
009500170407       BEGSR Elabora;
009600170407
009700170407       //?Leggo i rcd 'AA' in ordine decrescente di data/ora immissione
009800170407         exec sql
009900170407         DECLARE fnacre cursor for
010000170410         SELECT rrn(FNACRE0F), ACREcro from FNACRE0F
010100170407         WHERE  ACREtrc = 'AA'
010200170407         ORDER BY ACREcro, substr(ACREdati, 27, 14) desc;
010300170407
010400170407         exec sql open fnacre;
010500170407
010600170407         IF  sqlcode < 0;
010700170407           wEoF = *on;
010800170407           exec sql close fnacre;
010900170407           leavesr;
011000170407         ENDIF;
011100170407
011200170407         DOW  not wEoF;
011300170407           exec sql
011400170410           fetch next from fnacre into :rrnACRE, :wACRecro;
011500170407           IF  sqlcod = 100 or sqlcod < 0;
011600170407             wEoF = *on;
011700170407             leave;
011800170407           ENDIF;
011900170407
012000170407           IF  sav_CRO <> wACREcro;
012100170407             clear conta;
012200170407             sav_cro = wACREcro;
012300170407           ENDIF;
012400170407
012500170407         //?Il primo che leggo lo tengo, gli altri li cancello
012600170407           IF  conta = 0;
012700170407             conta += 1;
012800170407           ELSE;
012900170407             chain rrnACRE FNACRE0F;
013000170407             IF  %found(FNACRE0F);
013100170407               delete FNACRE00;
013200170407             ENDIF;
013300170407           ENDIF;
013400170407
013500170407         ENDDO;
013600170407
013700170407         exec sql close fnacre;
013800170314
013900170314       ENDSR;
014000170314
014100170314       //--------------------------------------------------------------
014200170314       //?Operazioni finali.
014300170314       //--------------------------------------------------------------
014400170314       BEGSR RoutEnd;
014500170314
014600170314         *inLR = *on;
014700170314         return;
014800170314
014900170314       ENDSR;
015000170314
015100170314      /end-free
