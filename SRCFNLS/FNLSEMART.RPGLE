000100161118      *------------------------------------------------------------------*
000200161118      * Avviso al destinatario di affidamento spedizione - Check Allegati
000300161118      *------------------------------------------------------------------*
000400110617
000500161118     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600161118     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000130406
001100161118     C                   EVAL      iPATH =
001200161118     C                             '/QNTC/CEDFSBO1/VASPDF'
001300161118     C                   EVAL      iAAS  = 2016
001400161118     C                   EVAL      iLNP  = 220
001500161118     C                   EVAL      iNRS  = 27
001600161118     C                   EVAL      iNSP  = 206142
001700161118     C                   EVAL      oOK   = *blanks
001800161118     C*
001900161118     C                   CALL      'FNLSEMAR'
002000161118     C                   PARM                    iPATH           120
002100161118     C                   PARM                    iAAS              4 0
002200161118     C                   PARM                    iLNP              3 0
002300161118     C                   PARM                    iNRS              2 0
002400161118     C                   PARM                    iNSP              7 0
002500161118     C                   PARM                    oOK               1
002600161118     C*
002700161118     C                   SETON                                        LR
002800161118     C*
002900161118
