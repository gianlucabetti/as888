000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020115     Ffifgt01l  uf   e           k disk
000500020211     Ffifpt01l  uf   e           k disk
000600020211     Ffifpd01l  uf   e           k disk
000601020528     Ffifgts0f  o    e           k disk
000700011026
000800000000     C*---------------------------------------------------------------*
000900020109      *conversioni
001000011026     c                   exsr      fifgt
001100990113     c                   seton                                        lr
001200990113     c*-------------------------------------------------
001300011026     c     fifgt         begsr
001400990113     c*-------------------------------------------------
001500020211     c     kfgt          chain     fifgt01l                           99
001600020114      * non verticalizzo le tariffe che risultano annullate
001700020211     c                   if        fgtatb = *blank and not *in99
001800020211     c                   exsr      figli
001900020211     c                   move      'A'           fgtatb
002000020211     c                   move      'T'           fgtftr
002100020211     c                   move      *all'9'       fgtdtr
002200020211
002201020528     c                   write     fifgts00
002300020211     c                   update    fifgt000
002400020114     c                   end
002500020110
002600990113     c                   endsr
002700020116     c*-------------------------------------------------
002800020211     c     figli         begsr
002900020116     c*-------------------------------------------------
003000020211     c     kfgt          setll     fifpt01l
003100020211     c                   do        *hival
003200020211     c     kfgt          reade     fifpt01l                               98
003300020211     c   98              leave
003400020211     c                   exsr      dettaglio
003500020211     c                   move      'A'           fptatb
003600020211     c                   move      'T'           fptftr
003700020211     c                   move      *all'9'       fptdtr
003800020211     c                   update    fifpt000
003900020211     c                   enddo
004000020211
004100020116     c                   endsr
004200020211     c*-------------------------------------------------
004300020211     c     dettaglio     begsr
004400020211     c*-------------------------------------------------
004500020211     c     kfpd          setll     fifpd01l
004600020211     c                   do        *hival
004700020211     c     kfpd          reade     fifpd01l                               97
004800020211     c   97              leave
004900020211
005000020211     c                   move      'A'           fpdatb
005100020211     c                   move      'T'           fpdftr
005200020211     c                   move      *all'9'       fpddtr
005300020211     c                   update    fifpd000
005400020211     c                   enddo
005500020211
005600020211     c                   endsr
005700011026     c*-------------------------------------------------
005800011026     c     *inzsr        begsr
005900011026     c*-------------------------------------------------
006000020211     c     *entry        plist
006100020211     c                   parm                    pdr               7 0
006200020211     c                   parm                    sml               1
006300020211     c                   parm                    tsr               1
006400020211     c                   parm                    ctr               3 0
006500020211     c                   parm                    prg               3 0
006600020211     c     kfgt          klist
006700020211     c                   kfld                    pdr
006800020211     c                   kfld                    sml
006900020211     c                   kfld                    tsr
007000020211     c                   kfld                    ctr
007100020211     c                   kfld                    prg
007200011026
007300020211     c     kfpd          klist
007400020211     c                   kfld                    fptpdr
007500020211     c                   kfld                    fptsml
007600020211     c                   kfld                    fpttsr
007700020211     c                   kfld                    fptctr
007800020211     c                   kfld                    fptprg
007900020211     c                   kfld                    fptctd
008000011026
008100011026     c                   endsr
008200011129     C*--------------------------------------------------------------------
