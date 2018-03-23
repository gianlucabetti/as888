000100090710     H NOMAIN
000200071128     ***********************************************************************************************
000300120110     **
000400071128     ** ISTRUZIONI PER LA COMPILAZIONE
000500071128     **
000600120120     ** 1. Creare il modulo TIS723R2U (Opz 15 PDM)
000700120120     ** 2. Creare/Aggiornare il programma di servizio TIS723R2U (CRTSRVPGM / UPDSRVPGM)
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
001900120120     FTIVIR06L  IF   E           K DISK
002000120110     F*----------------------------------------------------*
002100071128
002200080612
002300071128      //********************************************************************************************
002400071128      //
002500080612      // Definizione prototipi procedure (globali)
002600071128      //
002700090710      //********************************************************************************************
002800120120     D/COPY GAITRASRC/SRCPROTOPR,TIS723R2U
002900080612
003000080612
003100071128      //********************************************************************************************
003200071128      //
003300080612      // Definizione variabili work (globali)
003400071128      //
003500071128      //********************************************************************************************
003600120120
003700090710
003800120120
003900120110      //********************************************************************************************
004000120110      //
004100120120      // Definizione key-list globali                                 ?
004200120110      //
004300120110      //********************************************************************************************
004400120110
004500120110       // - TNTBE01L?
004600120120     d k03TIVIR06    e ds                  extname(TIVIR06L : *key)
004700090710
004800090710     P*
004900120120     P TIS723R2U_Ctrl_Chain_Dup...
005000090710     P                 B                   EXPORT
005100090710     D*
005200120120     D TIS723R2U_Ctrl_Chain_Dup...
005300120110     D                 PI             1
005400120120     D pInTpFileSrc                   2    CONST
005500120120     D pInKscSrc                      8    CONST
005600120120     D pInTpFileCtrl                  2    VALUE
005700120120     D pInKscCtrl                     8    VALUE
005800120102     D*
005900111221     **
006000111221     ** ESEMPI DI CHIAMATA
006100111221     **
006200120120     **       if TIS723R2U_Ctrl_Chain_Dup(
006300120120     **        pInTpFileSrc :
006400120120     **        pInKscSrc    :
006500120120     **        §VIR01Tip2   :
006600120120     **        §VIR01Ksc2   ) = '1';
006700090710     D*
006800090710      //********************************************************************************************
006900090710      //
007000090710      // Definizione variabili work (locali)
007100111221      //
007200120120
007300120120     D DVIR01        E DS
007400111221      /FREE
007500111222
007600120110        //apro il file se chiuso
007700120120        if not %open(TIVIR06L);
007800120120          open(e) TIVIR06L;
007900120110          if %error;
008000120120            return '1';
008100120110          endif;
008200120110        endif;
008300120110
008400120110        //imposto le chiavi con i dati in ingresso
008500120120        VIRKsc = pInKscCtrl;
008600120120        VIRTip = pInTpFileCtrl;
008700120120        //punto alla regola abilitata a oggi
008800120120        VIRDtF = %dec(%date() : *ISO);
008900120120        setll(e)  %kds(k03TIVIR06)  TIVIR000;
009000120120        reade(e)  %kds(k03TIVIR06 : 2)  TIVIR000;
009100120120        //se non trovo la regola torno indietro dando apposita segnalazione
009200120120        //dovrebbe essere un caso raro perché l'esistenza e abilitazione della regola
009300120120        //è già stato controllato nell'immissione della duplica: significa che la regola
009400120120        //si è invalidata prima della scadenza inizialmente messa
009500120120        select;
009600120120        when %error;
009700120120          return 'E';
009800120120        when not %found;
009900120120          return 'D';
010000120120        when %found;
010100120120          //se il tipo file / cliente per duplica esiste,
010200120120          //questo non deve contenere nelle sue coppie per duplica
010300120120          //il tipo file / cliente iniziale
010400120120          DVIR01 = VIRPTH;
010500120120          //le dupliche possibili sono 3 e per ognuna devo:
010600120120          // - controllare che la duplica sia valorizzata...
010700120120          // - controllare che la regola da duplicare non sia quella iniziale
010800120120          // - controllare la catena che parte da questa richiamando ricorsivamente questa procedura
010900120120          if §VIR01Tip2 <> *blank and
011000120120             §VIR01Ksc2 <> *blank;
011100120120            if §VIR01Tip2 = pInTpFileSrc and
011200120120               §VIR01Ksc2 = pInKscSrc;
011300120120              return '1';
011400120120            else;
011500120120              if TIS723R2U_Ctrl_Chain_Dup(
011600120120               pInTpFileSrc :
011700120120               pInKscSrc    :
011800120120               §VIR01Tip2   :
011900120120               §VIR01Ksc2   ) = '1';
012000120120                return '1';
012100120120              endif;
012200120120            endif;
012300120120          endif;
012400120120          if §VIR01Tip3 <> *blank and
012500120120             §VIR01Ksc3 <> *blank;
012600120120            if §VIR01Tip3 = pInTpFileSrc and
012700120120               §VIR01Ksc3 = pInKscSrc;
012800120120              return '1';
012900120120            else;
013000120120              if TIS723R2U_Ctrl_Chain_Dup(
013100120120               pInTpFileSrc :
013200120120               pInKscSrc    :
013300120120               §VIR01Tip3   :
013400120120               §VIR01Ksc3   ) = '1';
013500120120                return '1';
013600120120              endif;
013700120120            endif;
013800120120          endif;
013900120120          if §VIR01Tip4 <> *blank and
014000120120             §VIR01Ksc4 <> *blank;
014100120120            if §VIR01Tip4 = pInTpFileSrc and
014200120120               §VIR01Ksc4 = pInKscSrc;
014300120120              return '1';
014400120120            else;
014500120120              if TIS723R2U_Ctrl_Chain_Dup(
014600120120               pInTpFileSrc :
014700120120               pInKscSrc    :
014800120120               §VIR01Tip4   :
014900120120               §VIR01Ksc4   ) = '1';
015000120120                return '1';
015100120120              endif;
015200120120            endif;
015300120120          endif;
015400120120          //se arrivo qua vuol dire che non ho errori nella catena
015500120120          return '0';
015600120120        other;
015700120120          return '?';
015800120120        endsl;
015900090710
016000090710      /END-FREE
016100090710
016200120120     P TIS723R2U_Ctrl_Chain_Dup...
016300090710     P                 E
