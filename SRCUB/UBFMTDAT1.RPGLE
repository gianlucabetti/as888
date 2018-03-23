000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400071128     ** ISTRUZIONI PER LA COMPILAZIONE
000500071128     **
000600111221     ** 1. Creare il modulo UBFMTDATE (Opz 15 PDM)
000700111221     ** 2. Creare/Aggiornare il programma di servizio UBFMTDATE (CRTSRVPGM / UPDSRVPGM)
000800080610     ** 4. Cancellare il modulo.
000900071128     **
001000071128     ***********************************************************************************************
001100080612
001200080612
001300080612      //********************************************************************************************
001400080612      //
001500080612      // Definizione file (globali)
001600080612      //
001700080612      //********************************************************************************************
001800071128
001900080612
002000071128      //********************************************************************************************
002100071128      //
002200080612      // Definizione prototipi procedure (globali)
002300071128      //
002400090710      //********************************************************************************************
002500111221     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002600080612
002700080612
002800071128      //********************************************************************************************
002900071128      //
003000080612      // Definizione variabili work (globali)
003100071128      //
003200071128      //********************************************************************************************
003300090710
003400090710
003500090710
003600090710     P*--------------------------------------------------
003700111221     P* Procedure name: UBFMTDATE_Convert
003800111221     P* Purpose:        Converte data da un formato a un altro
003900111221     P* Returns:        10A => data nel formato richiesto
004000111221     P* Parameter:      pInDateIn       => Data numerica di partenza
004100111221     P*                 pInMaskDateIn   => Formato della data di partenza
004200111221     P*                 pInMaskDateOut  => Formato della data di arrivo
004300090710     P*--------------------------------------------------
004400090710     P*
004500111221     P UBFMTDATE_Convert...
004600090710     P                 B                   EXPORT
004700090710     D*
004800111221     D UBFMTDATE_Convert...
004900111221     D                 PI            10A
005000111221     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
005100111221     **
005200111221     **
005300111221     ** ESEMPI DI CHIAMATA
005400111221     **
005500111221     ** dataISO = UBFMTDATE_Convert('190211:'ddmmyy':'YYYY-MM-DD')
005600090710     D*
005700090710      //********************************************************************************************
005800090710      //
005900090710      // Definizione variabili work (locali)
006000111221      //
006100090710      //********************************************************************************************
006200111221     D dateOut         s                   like(pInDateIn)
006300111221     D up              C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
006400111221     D lo              C                   const('abcdefghijklmnopqrstuvwxyz')
006500111221     D String          S             10A
006600111221     D date8A_DMY      S              8A
006700111221     D dateISO         S               d   datfmt(*ISO)
006800111221     D posizioneInYYYY...
006900111221     D                 S
007000111221     D posizioneInYY
007100111221     D posizioneInMM
007200111221     D posizioneInDD
007300111221     D posizioneOutYYYY...
007400111221     D posizioneOutYY
007500111221     D posizioneOutMM
007600111221     D posizioneOutDD
007700111221
007800111221      /FREE
007900111221
008000111221
008100111221        //per sicurezza impongo i caratteri maiuscoli alla maschera di ingresso e a quella di uscita
008200111221        string = %XLATE(lo:up:pInMaskDateIn);
008300111221        pInMaskDateIn=string;
008400111221        string = %XLATE(lo:up:pInMaskDateOut);
008500111221        pInMaskDateOut=string;
008600111221
008700111221
008800111221        //inizializzo
008900111221        posizioneInYYYY = 0;
009000111221        posizioneInYY = 0;
009100111221        posizioneInMM = 0;
009200111221        posizioneInDD = 0;
009300111221        posizioneOutYYYY = 0;
009400111221        posizioneOutYY = 0;
009500111221        posizioneOutMM = 0;
009600111221        posizioneOutDD = 0;
009700111221
009800111221
009900111221        //reperisco le posizioni di partenza del formato di ingresso
010000111221
010100111221        //cerco la posizione di partenza dell'anno di 4 (non è detto ci sia, alternativa anno di 2)
010200111221        posizioneInYYYY = %scan('YYYY':pInMaskDateIn);
010300111221        //cerco la posizione di partenza dell'anno di 2 (non è detto ci sia, alternativa anno di 4)
010400111221        if posizioneInYYYY <> 0;
010500111221          posizioneInYY = %scan('YY':pInMaskDateIn);
010600111221        endif;
010700111221        //cerco la posizione di partenza del mese
010800111221        posizioneInMM = %scan('MM':pInMaskDateIn);
010900111221        //cerco la posizione di partenza del giorno
011000111221        posizioneInDD = %scan('DD':pInMaskDateIn);
011100090710
011200111221
011300111221        //se manca la posizione di partenza di uno dei blocchi, lo considero un errore
011400111221        //e quindi esco passando in output la stessa stringa ricevuta
011500111221        if posizioneInYYYY=0 and posizioneInYY=0
011600111221           or posizioneInMM=0
011700111221           or posizioneInDD=0;
011800111221          return pInDateIn;
011900111221        endif;
012000111221
012100111221
012200111221        //reperisco le posizioni di partenza del formato di uscita  (nessuna è obbligatoria)
012300111221
012400111221        //cerco la posizione di partenza dell'anno di 4 (alternativa anno di 2)
012500111221        posizioneOutYYYY = %scan('YYYY':pInMaskDateOut);
012600111221        //cerco la posizione di partenza dell'anno di 2 (alternativa anno di 4)
012700111221        if posizioneOutYYYY <> 0;
012800111221          posizioneOutYY = %scan('YY':pInMaskDateOut);
012900111221        endif;
013000111221        //cerco la posizione di partenza del mese
013100111221        posizioneOutMM = %scan('MM':pInMaskDateOut);
013200111221        //cerco la posizione di partenza del giorno
013300111221        posizioneOutDD = %scan('DD':pInMaskDateOut);
013400111221
013500111221
013600111221        //compongo la data in uscita nel formato voluto
013700111221        dateOut=pInMaskDateOut;
013800111221        if posizioneInYYYY > 0 and posizioneOutYYYY>0;
013900111221          %subst(dateOut:posizioneOutYYYY:4)
014000111221          = %subst(pInDateIn:posizioneInYYYY:4);
014100111221        endif;
014200111221        if posizioneInYY > 0 and posizioneOutYY>0;
014300111221          %subst(dateOut:posizioneOutYY:2)
014400111221          = %subst(pInDateIn:posizioneInYY:2);
014500111221        endif;
014600111221        if posizioneInYYYY > 0 and posizioneOutYYYY=0
014700111221        and posizioneOutYY>0;
014800111221        //eseguo trasformazione dell'anno se passato di 4 ma voluto di 2
014900111221          %subst(dateOut:posizioneOutYY:2)
015000111221          = %subst(pInDateIn:posizioneInYYYY+2:2);
015100111221        endif;
015200111221        if posizioneInYY > 0 and posizioneOutYY=0
015300111221        and posizioneOutYYYY>0;
015400111221        //eseguo trasformazione dell'anno se passato di 2 ma voluto di 4
015500111221          date8A_DMY='01/01/' + %subst(pInDateIn:posizioneInYY:2);
015600111221          dateISO=%date(date8A_DMT:*DMY);
015700111221          %subst(dateOut:posizioneOutYY:2)
015800111221          = %subst(dateISO:1:4);
015900111221        endif;
016000111221        if posizioneInMM > 0 and posizioneOutMM>0;
016100111221          %subst(dateOut:posizioneOutMM:2)
016200111221          = %subst(pInDateIn:posizioneInMM:2);
016300111221        endif;
016400111221        if posizioneInDD > 0 and posizioneOutDD>0;
016500111221          %subst(dateOut:posizioneOutDD:2)
016600111221          = %subst(pInDateIn:posizioneInDD:2);
016700111221        endif;
016800111221
016900111221
017000111221        return dateOut;
017100090710
017200090710      /END-FREE
017300090710
017400111221     P UBFMTDATE_Convert...
017500090710     P                 E
