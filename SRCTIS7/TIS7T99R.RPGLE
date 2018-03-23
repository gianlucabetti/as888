000100161212     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200030325     H DECEDIT('0.') DATEDIT(*DMY.)
000300991027
000400161118     Ftabel00f  IF   E           K DISK
000500991027
000600161118     D TIS799DSI     E DS
000700161118     D DS3Q          E DS
000800120406
000900991027
001000120406     C*
001100161118     C* Reperisco se clienet abilitato in tabella '3Q' a ricevere i dati
001200161118     C* accettazione bolle su solo "FLAG ANTICIPO DATI SU BOLLETTAZIONE"
001300161118     C                   eval      tblKUT = 1
001400161118     C                   eval      tblCOD = '3Q'
001500161118     C                   eval      tblKEY = %editc(I99CCM:'X')
001600120406     C*
001700161118     c     KEYtabel_C    chain(e)  tabel00f
001800161118     C                   if        %found(tabel00f)
001900161118     C                   clear                   DS3Q
002000161118     C                   eval      DS3Q = tblUNI
002100161118     C                   if        §3QADB = 'S'
002200161118     C                   clear                   TIS799DSI
002300161122     C                   eval      CLII99 = '0'+%editc(I99CCM:'X')
002400161118     C                   eval      AASI99 = I99AAS
002500161118     C                   eval      LNPI99 = I99LNP
002600161118     C                   eval      NRSI99 = I99NRS
002700161118     C                   eval      NSPI99 = I99NSP
002800161118     C                   eval      TBLI99 = *blanks
002900161122     C                   CALL      'TIS7I2C1'                                   * Batch
003000161122     C***                CALL      'TIS799R1'                                   * Interattivo
003100161118     C                   PARM                    TIS799DSI
003200161118     C                   endif
003300120406     C                   endif
003400120406     C*
003500921023     C                   SETON                                        LR
003600080527
003700080527
003800991027
003900991027      /TITLE Operazioni iniziali.
004000991027     C     *inzsr        BEGSR
004100991027     C*
004200991027     C     *ENTRY        PLIST
004300161118     C                   PARM                    I99CCM            7 0
004400161118     C                   PARM                    I99AAS            4 0
004500161118     C                   PARM                    I99LNP            3 0
004600161118     C                   PARM                    I99NRS            2 0
004700161118     C                   PARM                    I99NSP            7 0
004800120406     C*
004900161118     C     KEYtabel_C    KLIST
005000161118     C                   KFLD                    tblKUT
005100161118     C                   KFLD                    tblCOD
005200161118     C                   KFLD                    tblKEY
005300991027     C*
005400991027     C                   ENDSR
