000100070330     H DFTACTGRP(*NO) ACTGRP('YCOBOLLO') BNDDIR('YCO')
000200070330     H DECEDIT(*JOBRUN)
000300070330
000400070330      //********************************************************************************************
000500070330      //
000600070330      // Definizione costanti.
000700070330      //
000800070330      //********************************************************************************************
000900070330     D BOLLO_FATTURA...
001000070330     D                 C                   'F'
001100070330
001200070330      //********************************************************************************************
001300070330      //
001400070330      // Definizione prototipi.
001500070330      //
001600070330      //********************************************************************************************
001700070330      /COPY GAITRASRC/SRCPROTOPR,YCOBOLLOI
001800070330
001900070330      //********************************************************************************************
002000070330      //
002100070330      // Definizione variabili globali.
002200070330      //
002300070330      //********************************************************************************************
002400070330     D dataValidita...
002500070330     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
002600070330     D societa...
002700070330     D                 S              3A
002800070330     D imponibileIVA...
002900070330     D                 S              7P 2
003000070330     D importoBollo...
003100070330     D                 S              7P 2
003200070330     D riferimentoIVA...
003300070330     D                 S              3A
003400070330     D bolloVirtuale...
003500070330     D                 S               N
003600070330     D dichiarazioneBolloVirtuale...
003700070330     D                 S            100A
003800070330     D capoConto...
003900070330     D                 S              6A
004000070330     D sottoConto...
004100070330     D                 S              8A
004200070330     D causale...
004300070330     D                 S              4A
004400070330     D voce...
004500070330     D                 S              8A
004600070330
004700070330      //********************************************************************************************
004800070330      //
004900070330      // Main..
005000070330      //
005100070330      //********************************************************************************************
005200070330      /FREE
005300070330
005400070330       *INLR = *ON;
005500070330
005600070330       IF Bollo_i_init() >= *ZERO;
005700070330
005800070330         // Società con bollo virtuale.
005900070330         societa = '201';
006000070330         IF Bollo_i_newDocumento( BOLLO_FATTURA : dataValidita : societa )
006100070330         >= *ZERO;
006200070330           imponibileIVA = 40;
006300070330           riferimentoIVA = '101';
006400070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
006500070330           imponibileIVA = 50;
006600070330           riferimentoIVA = 'XXX'; // Questo non esiste, quindi non deve considerarlo.
006700070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
006800070330           imponibileIVA = 60;
006900070330           riferimentoIVA = '104';
007000070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
007100070330           Bollo_i_endDocumento();
007200070330           importoBollo = Bollo_i_getImportoBollo();
007300070330           Bollo_i_getDatiRegistrazioneCosto( capoConto
007400070330           : sottoConto : causale : voce );
007500070330           Bollo_i_getDatiRegistrazioneRicavo( capoConto
007600070330           : sottoConto : causale : voce : riferimentoIVA );
007700070330           IF Bollo_i_isVirtuale();
007800070330             dichiarazioneBolloVirtuale =
007900070330             Bollo_i_getDichiarazioneBolloVirtuale();
008000070330             Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale( capoConto
008100070330             : sottoConto : causale );
008200070330           ENDIF;
008300070330         ENDIF;
008400070330
008500070330         // Società senza bollo virtuale.
008600070330         societa = '248';
008700070330         IF Bollo_i_newDocumento( BOLLO_FATTURA : dataValidita : societa )
008800070330         >= *ZERO;
008900070330           imponibileIVA = 40;
009000070330           riferimentoIVA = '101';
009100070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
009200070330           imponibileIVA = 50;
009300070330           riferimentoIVA = 'XXX'; // Questo non esiste, quindi non deve considerarlo.
009400070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
009500070330           Bollo_i_endDocumento();
009600070330           importoBollo = Bollo_i_getImportoBollo();
009700070330           bolloVirtuale = Bollo_i_isVirtuale();
009800070330         ENDIF;
009900070330
010000070330         Bollo_i_finalize();
010100070330
010200070330       ENDIF;
010300070330
010400070330       RETURN;
010500070330
010600070330      /END-FREE
