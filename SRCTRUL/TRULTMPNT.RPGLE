000100130123     H DFTACTGRP(*YES)
000200130122
000300130123     ***************************************************************************
000400130123     **
000500130123     ** Costanti.
000600130123     **
000700130123     ***************************************************************************
000800130122
000900130123     ***************************************************************************
001000130123     **
001100130123     ** Strutture dati.
001200130123     **
001300130123     ***************************************************************************
001400130122
001500130123     ***************************************************************************
001600130123     **
001700130123     ** Campi.
001800130123     **
001900130123     ***************************************************************************
002000130123     D nomeOggetto     S             10A
002100130123     D libreria        S             10A
002200130122
002300130123     ***************************************************************************
002400130123     **
002500130123     ** Prototipi.
002600130123     **
002700130123     ***************************************************************************
002800130123     D/COPY GAITRASRC/SRCPROTOPR,TRULTMPN
002900130122
003000130122      /FREE
003100130122
003200130123       *INLR = *ON;
003300130123
003400130123       RESET libreria;
003500130123       TrulTmpN('*USRSPC' : *BLANK : libreria : nomeOggetto);
003600130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *USRSPC');
003700130123
003800130123       RESET libreria;
003900130123       TrulTmpN('*FILE' : *BLANK : libreria : nomeOggetto);
004000130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *FILE');
004100130123
004200130123       RESET libreria;
004300130123       TrulTmpN('*DTAARA' : *BLANK : libreria : nomeOggetto);
004400130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *DTAARA');
004500130123
004600130123       RESET libreria;
004700130123       TrulTmpN('*OUTQ' : 'O' : libreria : nomeOggetto);
004800130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *OUTQ');
004900130123
005000130123       RESET libreria;
005100130123       TrulTmpN('*OUTQ' : *BLANK : libreria : nomeOggetto);
005200130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *OUTQ');
005300130123
005400130123       RESET libreria;
005500130123       TrulTmpN('*LIB' : *BLANK : libreria : nomeOggetto);
005600130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *LIB');
005700130123
005800130123       libreria = 'YYYYYYYYYY';
005900130123       TrulTmpN('*DTAARA' : *BLANK : libreria : nomeOggetto);
006000130123       DSPLY (%TRIMR(libreria) + '/' + nomeOggetto + ' *LIB');
006100130123
006200130122       RETURN;
006300130122
006400130122      /END-FREE
006500130122
