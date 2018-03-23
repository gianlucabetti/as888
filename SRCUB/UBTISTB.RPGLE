000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300120110     **
000400071128     ** ISTRUZIONI PER LA COMPILAZIONE
000500071128     **
000600120118     ** 1. Creare il modulo UBTISTB (Opz 15 PDM)
000700120118     ** 2. Creare/Aggiornare il programma di servizio UBTISTB (CRTSRVPGM / UPDSRVPGM)
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
001900120110     FTISTB05I  UF   E           K DISK    usropn
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
007100120110        //apro il file se chiuso
007200120110        if not %open(TISTB05I);
007300120110          open(e) TISTB05I;
007400120110          if %error;
007500120110            return 'E';
007600120110          endif;
007700120110        endif;
007800120110
007900120110        //imposto le chiavi con i dati in ingresso
008000120110        k_STBAAS = pInAnno;
008100120110        k_STBLNP = pInLineaPar;
008200120110        k_STBNRS = pInSerie;
008300120110        k_STBNSP = pInNumSped;
008400120110        //se il flag è stato passato, cerco ci sia almeno un record con quello
008500120110        //altrimenti cerco con blank
008600120110        if %addr(pInFlagTrasm) <> *null;
008700120110          k_STBFTR = pInFlagTrasm;
008800120110        else;
008900120110          clear  k_STBFTR;
009000120110        endif;
009100120110
009200120110        //viene testata l'esistenza di almeno un rcd della spedizione in ingresso con flag di
009300120118        //trasmissione = dato ricevuto
009400120110        chain(e)  %kds(k05TISTB05)  TISTB000;
009500120110        select;
009600120110        when %error;
009700120110          return 'E';
009800120110        when %found;
009900120110          return '1';
010000120110        when not %found;
010100120110          return '0';
010200120110        other;
010300120110          return 'X';
010400120110        endsl;
010500090710
010600090710      /END-FREE
010700090710
010800120110     P UBTISTB_Ctrl_Shipment_Sent...
010900090710     P                 E
011000120110
011100120110     P*
011200120110     P UBTISTB_Upd_Shipment_Sent...
011300120110     P                 B                   EXPORT
011400120110     D*
011500120110     D UBTISTB_Upd_Shipment_Sent...
011600120110     D                 PI             1
011700120110     D pInAnno                        4s 0 CONST
011800120110     D pInLineaPar                    3s 0 CONST
011900120110     D pInSerie                       2s 0 CONST
012000120110     D pInNumSped                     7s 0 CONST
012100120110     D pInFlagTrasm                   1A   CONST OPTIONS(*OMIT:*NOPASS)
012200120110     D*
012300120110     **
012400120110     ** ESEMPI DI CHIAMATA
012500120110     **
012600120110     ** IF UBTISTB_Upd_Shipment_sent(STBAAS : STBLNP : STBNRS : STBNSP) = '1'
012700120110     D*
012800120110      //********************************************************************************************
012900120110      //
013000120110      // Definizione variabili work (locali)
013100120110      //
013200120110     D risultato       s              1
013300120110
013400120110      /FREE
013500120110
013600120110        //apro il file se chiuso
013700120110        if not %open(TISTB05I);
013800120110          open(e) TISTB05I;
013900120110          if %error;
014000120110            return 'E';
014100120110          endif;
014200120110        endif;
014300120110
014400120110        risultato ='0';
014500120110
014600120110        //imposto le chiavi con i dati in ingresso della spedizione
014700120110        k_STBAAS = pInAnno;
014800120110        k_STBLNP = pInLineaPar;
014900120110        k_STBNRS = pInSerie;
015000120110        k_STBNSP = pInNumSped;
015100120110        clear  k_STBFTR;
015200120110
015300120110        //mi posizione per la spedizione in ingresso (prime 4 chiavi e la 5a a blank)
015400120110        setll  %kds(k05TISTB05)  TISTB000;
015500120110        //leggo solo per spedizione
015600120110        reade(e)  %kds(k05TISTB05 : 4)  TISTB000;
015700120110        //se c'è stato un errore esco segnalandolo
015800120110        if %error;
015900120110         return 'E';
016000120110        endif;
016100120110        dow not %eof;
016200120110          if %addr(pInFlagTrasm) <> *null;
016300120110            STBFTR = pInFlagTrasm;
016400120110          else;
016500120110            clear  STBFTR;
016600120110          endif;
016700120110          update  TISTB000;
016800120110          //se c'è stato un errore esco segnalandolo
016900120110          if %error;
017000120110            return 'E';
017100120110          else;
017200120110            risultato = '1';
017300120110          endif;
017400120110          //rileggo
017500120110          reade(e)  %kds(k05TISTB05 : 4)  TISTB000;
017600120110          //se c'è stato un errore esco segnalandolo
017700120110          if %error;
017800120110            return 'E';
017900120110          endif;
018000120110        enddo;
018100120110
018200120110        return risultato;
018300120110
018400120110      /END-FREE
018500120110
018600120110     P UBTISTB_Upd_Shipment_Sent...
018700120110     P                 E
