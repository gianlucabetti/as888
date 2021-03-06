000100080206      //--------------------------------------------------------------
000200100429      //?TRMK86R2 - Pgm di comodo per passare i dati nella DS esterna
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Definizione strutture dati.
001000080206      //---------------------------------------------------------------
001100080206
001200080206      // - Parametri ricevuti
001300080206     d KPJBA         e ds
001400100429
001500100329      // - Passaggio dati a pgm stampa
001600100429     d TRMK86DS      e ds
001700080208
001800090508      //---------------------------------------------------------------
001900090508      //?Definizione procedure usate.
002000090508      //---------------------------------------------------------------
002100100329
002200100428      // - Stampa agenda
002300100429     d trmk86r         pr                  extpgm('TRMK86R')
002400100428     d  kpjba                              likeds(KPJBA)
002500100429     d  trmk86ds                           likeds(TRMK86DS)
002600100429
002700080206      //---------------------------------------------------------------
002800080206
002900080206      //---------------------------------------------------------------
003000080206      //?M A I N - L I N E
003100080206      //---------------------------------------------------------------
003200080206
003300080206     c     *Entry        plist
003400080206     c                   parm                    KPJBA
003500080206
003600080206      /free
003700090601
003800100429       //?Imposto la ds esterna e stampo
003900100429       TRMK86DS = kpjbu;
004000100429       trmk86r (kpjba:trmk86ds);
004100080206
004200100429       *inLR = *on;
004300100429       return;
004400080206
004500080206      /end-free
