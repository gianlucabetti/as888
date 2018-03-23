000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170418
000300170418
000400170418      //********************************************************************************************
000500170418      //
000600170418      // Definizione file
000700170418      //
000800170418      //********************************************************************************************
000900170412
001000170412
001100170412      //********************************************************************************************
001200170412      //
001300170418      // Definizione interfacce/prototipi procedure.
001400170412      //
001500170412      //********************************************************************************************
001600170505     D/COPY GAITRASRC/SRCPROTOPI,UBSI95R
001700170522
001800170418
001900010525     C*-----------------------------------------------------------------------------
002000170505     C*
002100170505     C* Imposto i dati bolla
002200170522     C                   clear                   EDIVABDS
002300170505     C                   eval      vabATB = *blanks                             E / D / S / *blnk
002400170505     C                   eval      vabLNP = 001
002500170505     C                   eval      vabCCM = 1662139
002600170505     C                   eval      vabCBO = '1'                                 1 / 2 / 4 / 6
002700170508     C                   eval      vabRSD = 'ADAMANTE'
002800170508     C                   eval      vabIND = 'VIA PASSOVECCHIO KM 14'
002900170508     C                   eval      vabCAD = '17740'
003000170508     C                   eval      vabLOD = 'PARIS'
003100170508     C                   eval      vabPRD = *blanks
003200170508     C                   eval      vabNZD = 'F'
003300170505     C                   eval      vabCTR = *zeros
003400170505     C                   eval      vabTSP = 'E'                                 C / E / D / H
003500170505     C                   eval      vabNCL = 1
003600170505     C                   eval      vabPKB = 17,8
003700170505     C                   eval      vabVLB = *zeros
003800170505     C                   eval      vabFFD = 'N'                                 N / S
003900170508     C                   eval      vabGVA = 'U'                                 U / B / *blnk
004000170508     C                   eval      vabTC1 = 'A'                                 S / A / P
004100170508     C                   eval      vabTC2 = *blanks                             S / A / P
004200170505     C                   eval      vabANT = *zeros
004300170505     C*
004400170505     C* Imposto i dati bolla
004500170505     C                   clear                   UBSI95DS
004600170522     C                   eval      ISI95LIN = 'it'                              it / EN
004700170522     C                   eval      ISI95TYP = 'F'                               C / V / F / *blnk
004800170522     C                   eval      ISI95CFD = ' '                               S / N / *blnk
004900170522     C                   eval      ISI95TLL = 'S'                               S / N / *blnk
005000170522     C                   eval      ISI95IED = 'S'                               S / N / *blnk
005100170522     C                   eval      ISI95DAT = *zeros
005200170508     C*
005300170508     C                   clear                   RTNesito
005400170508     C                   clear                   RTNopcode
005500170508     C                   clear                   RTNstatus
005600170505     C*
005700170505     C* Chiamata al driver di Calcolo Instradamento
005800170505     C                   call      'UBSI95R'
005900170505     C                   parm                    UBSI95DS
006000170522     C                   parm                    EDIVABDS
006100170508     C                   parm                    RTNesito
006200170508     C                   parm                    RTNopcode
006300170508     C                   parm                    RTNstatus
006400170505     C*
006500170505     C* Verifico esito chiamata
006600170505     C                   if        OSI95ERR <> *blanks
006700170505     C     OSI95ERR      dsply
006800170505     C                   else
006900170505     C     OSI95LNA      dsply
007000170505     C                   endif
007100100624     C*
007200170418     C                   seton                                        lr
007300010525     C*
007400170413
007500050920
007600050920
007700000714      /TITLE Routine di *init PGM
007800000714     C*
007900000714     C     *inzsr        begsr
008000060103     C*
008100170505     C***  *entry        plist
008200170505     C***                parm                    pInCCM            7
008300170505     C***                parm                    pInDSTO           5
008400000714     C*
008500000714     C                   endsr
008600000714
