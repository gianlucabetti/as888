000100110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200120521     h dftactgrp(*no) actgrp('QILE')
000300110221     h bnddir('UBBNDDIR')
000400120511
000500170306
000600170306      /copy gaitrasrc/srcProtoPR,UBLGOBR3
000700170306      /copy gaitrasrc/srcProtoPI,UBLGOBR3
000800170306
000900170306     d jObj            s              3  0 inz
001000170306     d wFil            s              3A   inz
001100170306     d wFAXIN          s             10A   inz
001200110221
001300110221       //==============================================================
001400120511       // M A I N - L I N E                                            ?
001500170125       //==============================================================
001600110221
001700170306         // Imposto i parametri relativi al filtro richiesto
001800170306         LGOB3DS.iGestione  = *blank;
001900170306         LGOB3DS.iLib       = 'GAITRAOBJ';
002000170306         LGOB3DS.iFiltroObj = 'FAXIN*';
002100170306         LGOB3DS.iObjT      = '*OUTQ';
002200170306
002300170306         // Ciclo per tutti gli oggetti che saranno reperiti
002400170306         dou LGOB3DS.oObj(%elem(LGOB3DS.oObj)) = *blank;
002500170306
002600170306           // Chiamata drover per reperimentio lista oggetti corrispondenti al filtro richiesto
002700170306           UBLGOBR3(LGOB3DS);
002800170306
002900170306           // Per ciascun elemento di schiera eseguo elaborazione
003000170306           jObj = 1;
003100170306           dow jObj <= %elem(LGOB3DS.oObj) AND LGOB3DS.oObj(jObj) <> *blanks;
003200170306
003300170306               // Reperisco codice Filiale
003400170306               wFil = %subst(LGOB3DS.oObj(jObj):6:3);
003500170306               if wFil <> *blanks;
003600170306
003700170306                  // Processo *OUTQ FAXIN corrente
003800170306                  wFAXIN = LGOB3DS.oObj(jObj);
003900170306
004000170306                  // Forzatura momentanea su filiale 043 (in test)
004100170308                  //if wFil = '043';
004200170306
004300170306     c                   call      'TRTCO1R1'
004400170306     c                   parm                    wFAXIN
004500170306     c                   parm                    wFil
004600170306
004700170308                  //endif;
004800170306               endif;
004900170306
005000170306               // Scorro elemento successivo
005100170306               jObj = jObj + 1;
005200170306           enddo;
005300170306
005400170306           // Se ultimo elemento valorizzato, possibile elenco oggetti successivi (paginazione)
005500170306           if LGOB3DS.oObj(%elem(LGOB3DS.oObj)) <> *blank;
005600170306             LGOB3DS.iGestione = 'R';
005700170303           endif;
005800170306
005900170303         enddo;
006000170303
006100110221         *inLR = *on;
