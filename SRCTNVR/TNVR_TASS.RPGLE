000100080321      *PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200080321      //---------------------------------------------------------------
000300080321      //?TNSB04R - Impostazione data fine pulizia x DIR in TITASS0F
000400080321      //---------------------------------------------------------------
000500080321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080321     h dftactgrp(*no)
000700080321
000800080321      //---------------------------------------------------------------
000900080321      //?Dichiarazione file.
001000080321      //---------------------------------------------------------------
001100080321
001200080321     fTILAD01L  if   e           k disk
001300080321
001400080321     fTITASS0F  Uf   e           k disk
001500080321
001600080321      //---------------------------------------------------------------
001700080321      //?Dichiarazione variabili.
001800080321      //---------------------------------------------------------------
001900080321
002000080321     d wAAS            s              2  0 inz
002100080321
002200080321      //---------------------------------------------------------------
002300080321      //?Main.
002400080321      //---------------------------------------------------------------
002500080321
002600080321      /free
002700080321
002800080321       *inLR = *on;
002900080321
003000080321       // Ciclo di elaborazione file TITASS0F
003100080321       read TITASS00;
003200080321
003300080321       DOW  not  %eof(TITASS0F);
003400080321
003500080321         // elaborazione dei soli record senza data fine pulizia
003600080321         if  TASdtfp = *blanks;
003700080321
003800080321           // reperimento data da TILAD01L
003900080321           if  TASaas >= 2000;
004000080321             wAAS = TASaas - 2000;
004100080321           else;
004200080321             wAAS = TASaas - 1900;
004300080321           endif;
004400080321           LADaas = %trim( %editc( wAAS   : 'X' ) );
004500080321           LADlnp = %trim( %editc( TASlnp : 'X' ) );
004600080321           LADnrs = %trim( %editc( TASnrs : 'X' ) );
004700080321           LADnsp = %trim( %editc( TASnsp : 'X' ) );
004800080321           chain (LADaas : LADlnp : LADnrs : LADnsp) TILAD000;
004900080321
005000080321           // aggiornamento TITASS solo se reperita data da TILAD
005100080321           if  %found(TILAD01L)     and
005200080321               %subst(LADdir : 1 : 8) > *zeros;
005300080321             eval TASdtfp = %subst(LADdir : 1 : 8);
005400080321           //_______________
005500080321             UPDATE TITASS00;
005600080321           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
005700080321           else;
005800080321             UNLOCK TITASS0F;
005900080321           endif;
006000080321
006100080321         else;
006200080321
006300080321           UNLOCK TITASS0F;
006400080321
006500080321         endif;
006600080321
006700080321         read TITASS00;
006800080321
006900080321       ENDDO;
007000080321
007100080321      /end-free
