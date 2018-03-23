000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400071128     ** ISTRUZIONI PER LA COMPILAZIONE
000500071128     **
000600120110     ** 1. Creare il modulo UBTISTB01 (Opz 15 PDM)
000700120110     ** 2. Creare/Aggiornare il programma di servizio UBTISTB01 (CRTSRVPGM / UPDSRVPGM)
000800120110     ** 3. Cancellare il modulo.
000900071128     **
001000071128     ***********************************************************************************************
001100080612
001200080612
001300080612      //********************************************************************************************
001400080612      //
001500080612      // Definizione file (globali)
001600080612      //
001700080612      //********************************************************************************************
001800120110     F*----------------------------------------------------*
001900120110     FTISTB05I  UF   E           K DISK
002000120110     F*----------------------------------------------------*
002100071128
002200080612
002300071128      //********************************************************************************************
002400071128      //
002500080612      // Definizione prototipi procedure (globali)
002600071128      //
002700090710      //********************************************************************************************
002800120110     D/COPY GAITRASRC/SRCPROTOPR,UBTISTB
002900080612
003000080612
003100071128      //********************************************************************************************
003200071128      //
003300080612      // Definizione variabili work (globali)
003400071128      //
003500071128      //********************************************************************************************
003600090710
003700120110      //********************************************************************************************
003800120110      //
003900120110      // Definizione key-list locali.                                 ?
004000120110      //
004100120110      //********************************************************************************************
004200120110
004300120110       // - TNTBE01L?
004400120110     d k05TISTB05    e ds                  extname(TISTB05I : *key)
004500120110     d                                     prefix(k_)   inz
004600090710
004700090710     P*
004800120110     P UBTISTB_Ctrl_Shipment_Sent...
004900090710     P                 B                   EXPORT
005000090710     D*
005100120110     D UBTISTB_Ctrl_Shipment_Sent...
005200120110     D                 PI             1
005300120110     D pInAnno                        4s 0 CONST
005400120110     D pInLineaPar                    3s 0 CONST
005500120110     D pInSerie                       2s 0 CONST
005600120110     D pInNumSped                     7s 0 CONST
005700120110     D pInFlagTrasm                   1A   CONST OPTIONS(*OMIT:*NOPASS)
005800120102     D*
005900111221     **
006000111221     ** ESEMPI DI CHIAMATA
006100111221     **
006200120110     ** IF UBTISTB_Ctrl_Shipment_sent(STBAAS : STBLNP : STBNRS : STBNSP) = '1'
006300090710     D*
006400090710      //********************************************************************************************
006500090710      //
006600090710      // Definizione variabili work (locali)
006700111221      //
006800120103
006900111221      /FREE
007000111222
007100120110        //imposto le chiavi con i dati in ingresso
007200120110        k_STBAAS = pInAnno;
007300120110        k_STBLNP = pInLineaPar;
007400120110        k_STBNRS = pInSerie;
007500120110        k_STBNSP = pInNumSped;
007600120110        //se il flag è stato passato, cerco ci sia almeno un record con quello
007700120110        //altrimenti cerco con blank
007800120110        if %addr(pInFlagTrasm) <> *null;
007900120110          k_STBFTR = pInFlagTrasm;
008000120110        else;
008100120110          clear  k_STBFTR;
008200120110        endif;
008300120110
008400120110        //viene testata l'esistenza di almeno un rcd della spedizione in ingresso con flag di
008500120110        //trasmissione = blank
008600120110        chain(e)  %kds(k05STB05)  TISTB000;
008700120110        select;
008800120110        when %error;
008900120110          return 'E';
009000120110        when %found;
009100120110          return '1';
009200120110        when not %found;
009300120110          return '1';
009400120110        other;
009500120110          return 'X';
009600120110        endsl;
009700090710
009800090710      /END-FREE
009900090710
010000120110     P UBTISTB_Ctrl_Shipment_Sent...
010100090710     P                 E
010200120110
010300120110     P*
010400120110     P UBTISTB_Upd_Shipment_Sent...
010500120110     P                 B                   EXPORT
010600120110     D*
010700120110     D UBTISTB_Upd_Shipment_Sent...
010800120110     D                 PI             1
010900120110     D pInAnno                        4s 0 CONST
011000120110     D pInLineaPar                    3s 0 CONST
011100120110     D pInSerie                       2s 0 CONST
011200120110     D pInNumSped                     7s 0 CONST
011300120110     D pInFlagTrasm                   1A   CONST OPTIONS(*OMIT:*NOPASS)
011400120110     D*
011500120110     **
011600120110     ** ESEMPI DI CHIAMATA
011700120110     **
011800120110     ** IF UBTISTB_Upd_Shipment_sent(STBAAS : STBLNP : STBNRS : STBNSP) = '1'
011900120110     D*
012000120110      //********************************************************************************************
012100120110      //
012200120110      // Definizione variabili work (locali)
012300120110      //
012400120110     D risultato       s              1
012500120110
012600120110      /FREE
012700120110
012800120110        risultato ='0';
012900120110
013000120110        //imposto le chiavi con i dati in ingresso della spedizione
013100120110        k_STBAAS = pInAnno;
013200120110        k_STBLNP = pInLineaPar;
013300120110        k_STBNRS = pInSerie;
013400120110        k_STBNSP = pInNumSped;
013500120110
013600120110        //mi posizione per la spedizione in ingresso (prime 4 chiavi)
013700120110        setll  %kds(k05TISTB05 : 4)  TISTB000;
013800120110        //leggo
013900120110        reade(e)  %kds(k05TISTB05 : 4)  TISTB000;
014000120110        //se c'è stato un errore esco segnalandolo
014100120110        if %error;
014200120110         return 'E';
014300120110        endif;
014400120110        dow not %eof;
014500120110          clear  k_STBFTR;
014600120110          update  TISTB000;
014700120110          //se c'è stato un errore esco segnalandolo
014800120110          if %error;
014900120110            return 'E';
015000120110          else;
015100120110            risultato = '1';
015200120110          endif;
015300120110          //rileggo
015400120110          reade(e)  %kds(k05TISTB05 : 4)  TISTB000;
015500120110          //se c'è stato un errore esco segnalandolo
015600120110          if %error;
015700120110            return 'E';
015800120110          endif;
015900120110        enddo;
016000120110
016100120110        return risultato;
016200120110
016300120110      /END-FREE
016400120110
016500120110     P UBTISTB_Upd_Shipment_Sent...
016600120110     P                 E
