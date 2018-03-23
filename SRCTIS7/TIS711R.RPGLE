000100990920      /TITLE Upload via Internet: invio a PO.
000200170523     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990920
000400990920     D dscmz         e ds
000500020805     D tibs55ds      e ds                  inz
000600990920     D esito           s              1
000700991228
000800991228     D sndpm01         s              7                                         Message id
000900991228     D sndpm02         s             20                                         Message file name
001000991228     D sndpm03         s            128                                         Immediate text
001100991228     D sndpm04         s              8b 0 inz(%size(sndpm03))                  Length of text
001200991228     D sndpm05         s             10    inz('*INFO')                         Message type
001300991228     D sndpm06         s             16    inz('*')                             Call stack entry
001400991228     D sndpm07         s              8b 0                                      Call stack counter
001500991228     D sndpm08         s              4                                         Call stack counter
001600991228     D/COPY QSYSINC/QRPGLESRC,QUSEC
001700990916
001800990920     C* Reperisco librerie e sistema informativo.
001900020805     C                   reset                   tibs55ds
002000990920     C                   move      cmzdst        i50apo
002100020805     C                   call(e)   'TIBS55R'
002200020805     C                   parm                    tibs55ds
002300990916     C* Se non ho reperito l'AS400 segnalo l'errore.
002400990916     C                   if        %error
002500990916     C                             or o50err <> *blanks
002600991228     C*
002700990916     C                   eval      esito = '1'
002800991228     C* Registro l'errore nel JOBLOG.
002900991228     C                   eval      sndpm03 = o50msg
003000991228     C                   eval      qusbprv = %size(qusec)
003100991228     C                   call      'QMHSNDPM'
003200991228     C                   parm                    sndpm01
003300991228     C                   parm                    sndpm02
003400991228     C                   parm                    sndpm03
003500991228     C                   parm                    sndpm04
003600991228     C                   parm                    sndpm05
003700991228     C                   parm                    sndpm06
003800991228     C                   parm                    sndpm07
003900991228     C                   parm                    sndpm08
004000991228     C                   parm                    qusec
004100991228     C*
004200990916     C                   else
004300010123     C* Eseguo considerazioni sulla libreria di arrivo
004400010123     C                   if        cmzlba = 'G'
004500010123     C                   eval      cmzlba = o50alg
004600010123     C                   else
004700010123     C                   eval      cmzlba = o50ala
004800010123     C                   endif
004900990916     C* Chiamo la procedura di invio.
005000020805     C                   call(e)   'TIBS53C'
005100020805     C                   parm                    O50LCL
005200990916     C                   parm                    CMZDST
005300990920     C                   parm                    CMZFLD
005400990920     C                   parm                    CMZMBD
005500990916     C                   parm      o50plg        CMZLBD
005600990920     C                   parm                    CMZFLA
005700990920     C                   parm                    CMZMBA
005800010123     C                   parm                    CMZLBA
005900990916     C                   parm      *zeros        CMZORI
006000990916     C                   parm      *zeros        CMZDTI
006100990916     C                   parm      'N'           CMZURG
006200990916     C                   parm      'A'           CMZADD
006300001215     C                   parm      'N'           CMZCLR
006400990916     C                   parm      'S'           CMZFTR
006500990916     C                   parm                    CMZERR
006600990916     C                   parm                    CMZAZN
006700990916     C                   parm      o50asi        CMZSYF
006800990916     C                   parm                    CMZNRR
006900990916     C                   parm                    CMZPRR
007000020805     C                   parm      *blanks       PAR236          236
007100990916     C                   if        %error
007200990916     C                             or cmzerr = '1'
007300990916     C                   eval      esito = '1'
007400990916     C                   else
007500990916     C                   eval      esito = '0'
007600990916     C                   endif
007700990916     C                   endif
007800990916     C*
007900990916     C                   return
008000990916
008100990916      /TITLE Operazioni iniziali.
008200990916     C     *inzsr        begsr
008300990916     C*
008400990916     C     *entry        plist
008500990920     C                   parm                    dscmz
008600990916     C                   parm                    esito
008700990916     C*
008800990916     C                   endsr
