000100070330     H DFTACTGRP(*NO) ACTGRP('YCOBOLLO') BNDDIR('YCO')
000200070330
000300070330      //********************************************************************************************
000400070330      //
000500070330      // Definizione costanti.
000600070330      //
000700070330      //********************************************************************************************
000800070330     D ESITO_OK...
000900070330     D                 C                   0
001000070330     D ESITO_WARNING...
001100070330     D                 C                   1
001200070330     D ESITO_ERROR...
001300070330     D                 C                   -1
001400070330     D BOLLO_FATTURA...
001500070330     D                 C                   'F'
001600070330
001700070330      //********************************************************************************************
001800070330      //
001900070330      // Definizione prototipi.
002000070330      //
002100070330      //********************************************************************************************
002200070330      /COPY GAITRASRC/SRCPROTOPR,YCOBOLLOI
002300070330
002400070330      //********************************************************************************************
002500070330      //
002600070330      // Definizione variabili globali.
002700070330      //
002800070330      //********************************************************************************************
002900070330     D dataValidita...
003000070330     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
003100070330     D societa...
003200070330     D                 S              3A
003300070330     D imponibileIVA...
003400070330     D                 S              7P 2
003500070330     D importoBollo...
003600070330     D                 S              7P 2
003700070330     D riferimentoIVA...
003800070330     D                 S              3A
003900070330     D bolloVirtuale...
004000070330     D                 S               N
004100070330
004200070330      //********************************************************************************************
004300070330      //
004400070330      // Main..
004500070330      //
004600070330      //********************************************************************************************
004700070330      /FREE
004800070330
004900070330       *INLR = *ON;
005000070330
005100070330       IF Bollo_i_init() >= *ZERO;
005200070330
005300070330         // Società con bollo virtuale.
005400070330         societa = '201';
005500070330         IF Bollo_i_newDocumento( BOLLO_FATTURA : dataValidita : societa )
005600070330         >= *ZERO;
005700070330           imponibileIVA = 40;
005800070330           riferimentoIVA = '101';
005900070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
006000070330           imponibileIVA = 50;
006100070330           riferimentoIVA = 'XXX'; // Questo non esiste, quindi non deve considerarlo.
006200070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
006300070330           imponibileIVA = 60;
006400070330           riferimentoIVA = '104';
006500070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
006600070330           Bollo_i_endDocumento();
006700070330           importoBollo = Bollo_i_getImportoBollo();
006800070330           bolloVirtuale = Bollo_i_isVirtuale();
006900070330         ENDIF;
007000070330
007100070330         // Società senza bollo virtuale.
007200070330         societa = '248';
007300070330         IF Bollo_i_newDocumento( BOLLO_FATTURA : dataValidita : societa )
007400070330         >= *ZERO;
007500070330           imponibileIVA = 40;
007600070330           riferimentoIVA = '101';
007700070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
007800070330           imponibileIVA = 50;
007900070330           riferimentoIVA = 'XXX'; // Questo non esiste, quindi non deve considerarlo.
008000070330           Bollo_i_addImponibileEsenteIVA( riferimentoIVA : imponibileIVA );
008100070330           Bollo_i_endDocumento();
008200070330           importoBollo = Bollo_i_getImportoBollo();
008300070330           bolloVirtuale = Bollo_i_isVirtuale();
008400070330         ENDIF;
008500070330
008600070330         Bollo_i_finalize();
008700070330
008800070330       ENDIF;
008900070330
009000070330       RETURN;
009100070330
009200070330      /END-FREE
