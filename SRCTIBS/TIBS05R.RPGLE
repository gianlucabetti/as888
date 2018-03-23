000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200990512     F* TIBS05R * -----------------------------------------*
000300990512     F*    Reperimento sistema informativo di una tabella
000400990512     f*       dato un s.i. di inout e un codice lingua
000500000000     F*----------------------------------------------------*
000600990512     Ftntbe01l  IF   E           K DISK
000700981012     D*----------------
000800990512     D TIBS05DS      E DS                                                       *reper.librerie job
000900990512     D
001000990512     D KKE1            s                   LIKE(tbeke1)
001100990512     D KKE2            s                   LIKE(tbeke2)
001200990512     D Klin            s                   LIKE(tbelin)
001300990512     D Ksif            s                   LIKE(tbesif)
001400990512     D Kcod            s                   LIKE(tbecod)
001500981012     D*
001600981012     C*--------------------------------------------------------------------------------------------*
001700981012     C* Main lines
001800981012     C*--------------------------------------------------------------------------------------------*
001900990512     c                   if        i05tla<>'C'
002000990512     c                   clear                   O05ERR
002100990512     c                   clear                   O05SIF
002200990512     C
002300990512     c* Imposto la key per trovare il record di testata della tabella
002400990512     c                   eval      kke1=*all'0'
002500990512     c                   move      i05cod        kke1
002600990512     c                   eval      klin=i05lin
002700990512     c                   eval      ksif=i05sif
002800990512     c
002900990512     c     ktbe          chain     tntbe01l                           30
003000990512     c* Trovata
003100990512     c                   if        not *in30 AND tbeatb=' '
003200990512     c                   eval      o05sif=i05sif
003300990512     c                   else
003400990512     c                   clear                   ksif
003500990512     c     ktbe          chain     tntbe01l                           30
003600990512     c                   if        *in30 or  tbeatb<>' '
003700990512     c                   eval      o05err='N'
003800990512     c                   endif
003900990512     c                   endif
004000990512     c
004100990512     c                   endif
004200981012     C*
004300990512     c                   if        i05tla=' '
004400990512     C                   SETON                                        rt
004500990512     c                   else
004600981012     C                   SETON                                        lr
004700990512     c                   endif
004800981012     c*--------------------------------------------------------------------
004900981012     c*  *inzsr Routine iniziale
005000981012     c*--------------------------------------------------------------------
005100981012     C     *inzsr        BEGSR                                                  *
005200981012     c*.
005300981012     C     *ENTRY        PLIST                                                  *
005400990512     C                   PARM                    TIBS05DS                       *
005500990512     c
005600990512     c     ktbe          klist
005700990512     c                   kfld                    kcod
005800990512     c                   kfld                    kke1
005900990512     c                   kfld                    kke2
006000990512     c                   kfld                    klin
006100990512     c                   kfld                    ksif
006200990512     c                   clear                   kke2
006300990512     c                   clear                   kcod
006400981012     c*
006500981012     c                   endsr
