000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400100512     D*--------------------
000500100705     D*
000600100512     D*--------------------
000700100512     D*
000800100512
000900100512     D*------------
001000100512     D* ARCHITETTURA
001100100512     D*------------
001200100512     D KPJBA         E DS
001300100705     D*
001400100705
001500100705     D*------------
001600100705     D* VARIABILI D WRK
001700100705     D*------------
001800100705     D savKPJBU        S                   like(KPJBU) inz
001900100705     D*
002000100705
002100010402
002200100512     C*
002300100705     C* Salvo il passaggio parametri "originale" e compongo quello da "passare"
002400100705     C                   eval      savKPJBU = KPJBU
002500100705     C                   eval      KPJBU = %subst(KPJBU:3)
002600100705     C*
002700100705     C* Verifico il tipo file da rigenerare
002800100705     C                   select
002900100705     C* - VC
003000100705     C                   when      %subst(savKPJBU:1:2) = 'VC'
003100100705     C                   call      'TIS7VC1R'
003200100705     C                   parm                    KPJBA
003300100705     C* - VG
003400100705     C                   when      %subst(savKPJBU:1:2) = 'VG'
003500100705     C                   call      'TIS7VG1R'
003600100705     C                   parm                    KPJBA
003700100719     C* - BT
003800100719     C                   when      %subst(savKPJBU:1:2) = 'BT'
003900100719     C                   call      'TIS7VB1R'
004000100719     C                   parm                    KPJBA
004100120323     C* - BS
004200120323     C                   when      %subst(savKPJBU:1:2) = 'BS'
004300120323     C                   call      'TIS7BS1R'
004400120323     C                   parm                    KPJBA
004500110307     C* - FF
004600110307     C                   when      %subst(savKPJBU:1:2) = 'FF'
004700110307     C                   call      'TIS7FF1R'
004800110307     C                   parm                    KPJBA
004900130801     C* - X1
005000130801     C                   when      %subst(savKPJBU:1:2) = 'X1'
005100130801     C                   call      'TIS7X11R'
005200130801     C                   parm                    KPJBA
005300141009     C* - VS
005400141009     C                   when      %subst(savKPJBU:1:2) = 'VS'
005500141009     C                   call      'TIS7VS1R'
005600141009     C                   parm                    KPJBA
005700160321     C* - CS
005800160321     C                   when      %subst(savKPJBU:1:2) = 'CS'
005900160321     C                   call      'TIS7CSR'
006000160321     C                   parm                    KPJBA
006100100705     C*
006200100705     C                   other
006300100705     C                   endsl
006400100705     C*
006500100705     C* Ripristino il passaggio paremtri "originale"
006600100705     C                   eval      KPJBU = savKPJBU
006700100705     C*
006800100512     C                   seton                                        LR
006900100512
007000100512
007100100512
007200090424     C*----------------------------------------------------*
007300090424     C     *INZSR        BEGSR
007400090424     C*
007500090424     C     *ENTRY        PLIST
007600090424     C                   PARM                    KPJBA
007700100512     C*
007800100512     C                   ENDSR
