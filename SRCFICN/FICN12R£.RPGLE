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
002400020528     c                   write     fifgts00
002500020211     c                   update    fifgt000
002600020114     c                   end
002700020110
002800990113     c                   endsr
002900020116     c*-------------------------------------------------
003000020211     c     figli         begsr
003100020116     c*-------------------------------------------------
003200020211     c     kfgt          setll     fifpt01l
003300020211     c                   do        *hival
003400020211     c     kfgt          reade     fifpt01l                               98
003500020211     c   98              leave
003600020211     c                   exsr      dettaglio
003700020211     c                   move      'A'           fptatb
003800091210     c******             move      'T'           fptftr
003900091210     c******             move      *all'9'       fptdtr
004000020211     c                   update    fifpt000
004100020211     c                   enddo
004200020211
004300020116     c                   endsr
004400020211     c*-------------------------------------------------
004500020211     c     dettaglio     begsr
004600020211     c*-------------------------------------------------
004700020211     c     kfpd          setll     fifpd01l
004800020211     c                   do        *hival
004900020211     c     kfpd          reade     fifpd01l                               97
005000020211     c   97              leave
005100020211
005200020211     c                   move      'A'           fpdatb
005300091210     c******             move      'T'           fpdftr
005400091210     c******             move      *all'9'       fpddtr
005500020211     c                   update    fifpd000
005600020211     c                   enddo
005700020211
005800020211     c                   endsr
005900011026     c*-------------------------------------------------
006000011026     c     *inzsr        begsr
006100011026     c*-------------------------------------------------
006200020211     c     *entry        plist
006300020211     c                   parm                    pdr               7 0
006400020211     c                   parm                    sml               1
006500020211     c                   parm                    tsr               1
006600020211     c                   parm                    ctr               3 0
006700020211     c                   parm                    prg               3 0
006800020211     c     kfgt          klist
006900020211     c                   kfld                    pdr
007000020211     c                   kfld                    sml
007100020211     c                   kfld                    tsr
007200020211     c                   kfld                    ctr
007300020211     c                   kfld                    prg
007400011026
007500020211     c     kfpd          klist
007600020211     c                   kfld                    fptpdr
007700020211     c                   kfld                    fptsml
007800020211     c                   kfld                    fpttsr
007900020211     c                   kfld                    fptctr
008000020211     c                   kfld                    fptprg
008100020211     c                   kfld                    fptctd
008200011026
008300011026     c                   endsr
008400011129     C*--------------------------------------------------------------------
