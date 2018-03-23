000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020114      *--------------------------------------------------------------*
000300091210     fFIFGT00Fexif   e             disk    rename(fifgt000:fifgtEX)
000400091210     f                                     prefix(Ex_)
000500091207      **
000600091214     fFIFGT01L  uf   e           k disk
000700091210      **
000800091210      **  Poi esegue la testatona
000900091210     c                   read      fiFGT00fEX
001000091210     c                   dow       not %Eof(fiFGT00fEX)
001100091210
001200091214     c     kfgt          chain     fifgt01l
001300091214     c                   z-add     EX_FGTDTS     FGTDTS
001400091214     c                   z-add     EX_FGTDCN     FGTDCN
001500091214     c                   update    fiFGT000
001600091210
001700091210     c                   read      fiFGT00fEX
001800091210     c                   enddo
001900091210
002000020704     c                   Eval      *InLr = *on
002100020704
002200091214     c     kfgt          klist
002300091214     c                   kfld                    EX_FGTPDR
002400091214     c                   kfld                    EX_FGTSML
002500091214     c                   kfld                    EX_FGTTSR
002600091214     c                   kfld                    EX_FGTCTR
002700091214     c                   kfld                    EX_FGTPRG
002800020704      *---------------------------------------------------------------*
