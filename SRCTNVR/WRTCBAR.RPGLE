000100990119     FANCBA01L  IP   E           K DISK
000200990119     F*---------
000300990119     FCNCLP00F  IF   E           K DISK    usropn
000400990119     F*---------
000500990119     FQsysprt   O    F  132        printer oflind(*inof)
000600990119      *--------------------------------------------------------------
000700990119     Dlungh            s             15  5 inz(80,00000)
000800990119     DOVRclp1          s             29    inz('OVRDBF FILE(CNCLP00F) TOFILE(')
000900990119     DOVRclp2          s             19    inz('/CNCLP00F)')
001000990119     DOVRclp3          s             29    inz('DLTOVR FILE(CNCLP00F)')
001100990119      *--------------------------------------------------------------
001200990119     IANCBA000      01
001300990119     I                                          CBASOCIETA    L1
001400990119      *--------------------------------------------------------------
001500990119     C   L1              exsr      ovrclp
001600990119     C*
001700990119     C                   if        cbaclifor='C' and cbatpappo='CA'
001800990119     C                   Z-add     1             clpkut
001900990119     C                   Z-add     151           clpkcc
002000990119     C                   move      cbaksc        clpksc
002100990119     C     kclp          chain     cnclp00f                           21
002200990119     C                   if        not *in21 and clpfpc='2'
002300990119     C                   move      cbaabi        wabi              5 0
002400990119     C                   move      cbacab        wcab              5 0
002500990119     C                   movel     cbaccb        wccb             16
002600990119     C                   if        wabi<>clpabi or wcab<>clpcab or clpccb<>wccb
002700990119     C   of              except    testa
002800990119     C                   except    erro
002900990119     C                   end
003000990119     c                   end
003100990119     c                   end
003200990119     C*
003300990119     CL1                 EXSR      DLTOVR
003400990119      *--------------------------------------------------------------
003500990119     C     ovrclp        begsr
003600990119     C*--------------------------------------------------------------
003700990119     C     cbasocieta    ifge      '200'
003800990119     C     'GAITRA'      cat       cbasocieta    libreria         10
003900990119     C                   else
004000990119     C     'SDITRA'      cat       cbasocieta    libreria
004100990119     C                   endif
004200990119      * Devo eseguire OVRDBF ai file nelle librerie di CNclp00F
004300990119     C     ovrclp1       cat       libreria:0    commwrk          80
004400990119     C     commwrk       cat(P)    ovrclp2:0     command          80
004500990119     C                   call      'QCMDEXC'
004600990119     C                   parm                    command
004700990119     C                   parm                    lungh
004800990119      * se non riesce l'OVRDB
004900990119     c                   open      CNclp00f
005000990119      *
005100990119     C                   endsr
005200990119      *--------------------------------------------------------------
005300990119     C     dltovr        begsr
005400990119     C*--------------------------------------------------------------
005500990119     c                   close     CNclp00f
005600990119      *
005700990119     C                   movel     *blanks       command          80
005800990119     C                   movel     ovrclp3       command          80
005900990119     C                   call      'QCMDEXC'
006000990119     C                   parm                    command
006100990119     C                   parm                    lungh
006200990119      *
006300990119     C                   endsr
006400990119      *--------------------------------------------------------------
006500990119     C     *inzsr        begsr
006600990119     C*--------------------------------------------------------------
006700990119     C     kclp          klist
006800990119     C                   kfld                    clpkut
006900990119     C                   kfld                    clpkcc
007000990119     C                   kfld                    clpksc
007100990119     C*
007200990119     C                   seton                                        of
007300990119     C*
007400990119     C                   endsr
007500990119     O*-----------------------------------------------------------------------*
007600990119     OQSYSPRT   E            TESta         01 01
007700990119     O                                           73 'Stampa conti clienti da ag'
007800990119     O                                              'giornare'
007900990119     O          E            TESta         01 02
008000990119     O                                            3 'SOC'
008100990119     O                                           19 'CODICE CONTO'
008200990119     O                                           31 'ABI CAB NEW'
008300990119     O                                           50 'NUMERO C/C NEW'
008400990119     O                                           62 'ABI CAB OLD'
008500990119     O                                           80 'NUMERO C/C NEW'
008600990119     O          E            erro          01
008700990119     O                       cbasocieta           3
008800990119     O                       clpKCC        Z      8
008900990119     O                       cbaksc              17
009000990119     O                       cbaclifor           19
009100990119     O                       cbaabi              25
009200990119     O                       cbacab              31
009300990119     O                       cbaccb              50
009400990119     O                       clpabi              56
009500990119     O                       clpcab              62
009600990119     O                       clpccb              80
009700990119     O                       clpfpc              82
