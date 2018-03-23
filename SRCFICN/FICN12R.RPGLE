000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020115     Ffifgt01l  uf   e           k disk
000500020211     Ffifpt01l  uf   e           k disk
000600020211     Ffifpd01l  uf   e           k disk
000700020528     Ffifgts0f  o    e           k disk
000800011026
000900000000     C*---------------------------------------------------------------*
001000020109      *conversioni
001100011026     c                   exsr      fifgt
001200990113     c                   seton                                        lr
001300990113     c*-------------------------------------------------
001400011026     c     fifgt         begsr
001500990113     c*-------------------------------------------------
001600020211     c     kfgt          chain     fifgt01l                           99
001700020114      * non verticalizzo le tariffe che risultano annullate
001800020211     c                   if        fgtatb = *blank and not *in99
001900020211     c                   exsr      figli
002000020211     c                   move      'A'           fgtatb
002100091210     c******             move      'T'           fgtftr
002200091210     c******             move      *all'9'       fgtdtr
002300020211
002400140917     c                   if        fgtsml <> 'S'
002500020528     c                   write     fifgts00
002600140917     c                   endif
002700140917      *
002800020211     c                   update    fifgt000
002900020114     c                   end
003000020110
003100990113     c                   endsr
003200020116     c*-------------------------------------------------
003300020211     c     figli         begsr
003400020116     c*-------------------------------------------------
003500020211     c     kfgt          setll     fifpt01l
003600020211     c                   do        *hival
003700020211     c     kfgt          reade     fifpt01l                               98
003800020211     c   98              leave
003900020211     c                   exsr      dettaglio
004000020211     c                   move      'A'           fptatb
004100091210     c******             move      'T'           fptftr
004200091210     c******             move      *all'9'       fptdtr
004300020211     c                   update    fifpt000
004400020211     c                   enddo
004500020211
004600020116     c                   endsr
004700020211     c*-------------------------------------------------
004800020211     c     dettaglio     begsr
004900020211     c*-------------------------------------------------
005000020211     c     kfpd          setll     fifpd01l
005100020211     c                   do        *hival
005200020211     c     kfpd          reade     fifpd01l                               97
005300020211     c   97              leave
005400020211
005500020211     c                   move      'A'           fpdatb
005600091210     c******             move      'T'           fpdftr
005700091210     c******             move      *all'9'       fpddtr
005800020211     c                   update    fifpd000
005900020211     c                   enddo
006000020211
006100020211     c                   endsr
006200011026     c*-------------------------------------------------
006300011026     c     *inzsr        begsr
006400011026     c*-------------------------------------------------
006500020211     c     *entry        plist
006600020211     c                   parm                    pdr               7 0
006700020211     c                   parm                    sml               1
006800020211     c                   parm                    tsr               1
006900020211     c                   parm                    ctr               3 0
007000020211     c                   parm                    prg               3 0
007100020211     c     kfgt          klist
007200020211     c                   kfld                    pdr
007300020211     c                   kfld                    sml
007400020211     c                   kfld                    tsr
007500020211     c                   kfld                    ctr
007600020211     c                   kfld                    prg
007700011026
007800020211     c     kfpd          klist
007900020211     c                   kfld                    fptpdr
008000020211     c                   kfld                    fptsml
008100020211     c                   kfld                    fpttsr
008200020211     c                   kfld                    fptctr
008300020211     c                   kfld                    fptprg
008400020211     c                   kfld                    fptctd
008500011026
008600011026     c                   endsr
008700011129     C*--------------------------------------------------------------------
