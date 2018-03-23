000100110617      *---------------------------------------------------------------*
000200161220      * Crea spool da PDF allegati tariffari fissi                    *
000300110617      *---------------------------------------------------------------*
000400110617
000500161220     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600161221     H dftactgrp(*no) actgrp(*caller)
000700110617
000800110617      *---------------------------------------------------------------*
000900161220
001000161220     D*********
001100161220     D* DS DI PROCEDURA
001200161220     D*********
001300161221     D TRBMATRR1DS     ds                  qualified inz
001400161221     D   piCodRegola                  3A
001500161221     D   piCodOperat                 15A
001600161221     D   piUsrData                   10A
001700161221     D   piTipoModulo                10A
001800161221     D   piPrinter                   10A
001900161221     D   poEsitoOK                    1A
002000161220
002100161220
002200161220     D*********
002300161220     D* PROTOTIPI CHIAMATE ESTERNE
002400161220     D*********
002500161220     D*
002600161221     D TRBMATRR1       PR
002700161221     D                                     extpgm('TRBMATRR1')
002800161221     D piCodRegola                         like(TRBMATRR1DS.piCodRegola)
002900161221     D piCodOperat                         like(TRBMATRR1DS.piCodOperat)
003000161221     D piUsrData                           like(TRBMATRR1DS.piUsrData)
003100161221     D piTipoModulo                        like(TRBMATRR1DS.piTipoModulo)
003200161221     D piPrinter                           like(TRBMATRR1DS.piPrinter)
003300161221     D poEsitoOK                           like(TRBMATRR1DS.poEsitoOK)
003400161220     D*
003500110617
003600161221
003700161221      /free
003800161221
003900161221        TRBMATRR1DS.piCodRegola  = 'TM4';
004000161221        TRBMATRR1DS.piCodOperat  = '*ATTACH*';
004100161221        TRBMATRR1DS.piUsrData    = 'ALLEGATI';
004200161221        TRBMATRR1DS.piTipoModulo = '*CARTAINT';
004300161221        TRBMATRR1DS.piPrinter    = 'QPRINT';
004400161221        TRBMATRR1DS.poEsitoOK    = *blanks;
004500161221
004600161221        callP(e) TRBMATRR1 (TRBMATRR1DS.piCodRegola
004700161221                           :TRBMATRR1DS.piCodOperat
004800161221                           :TRBMATRR1DS.piUsrData
004900161221                           :TRBMATRR1DS.piTipoModulo
005000161221                           :TRBMATRR1DS.piPrinter
005100161221                           :TRBMATRR1DS.poEsitoOK
005200161221                           );
005300161220
005400161220       // Uscita
005500161220       exsr uscita;
005600161220
005700161220
005800161220
005900161220       //-------------------------------------------------------------*
006000161220       // Uscita incondizionata                                       *
006100161220       //-------------------------------------------------------------*
006200161220        BEGSR uscita;
006300161220
006400161220           *inlr = *on;
006500161220           return;
006600161220
006700161220        ENDSR;
006800131210
