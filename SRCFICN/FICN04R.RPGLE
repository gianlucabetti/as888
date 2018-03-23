000100081229     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300081229     Ffitgt01l  if   e           k disk
000400081229     Ffifgt01l  uf   e           k disk    commit
000500081229     Ffifpt01l  uf   e           k disk    commit
000600081229     Ffifpd01l  uf   e           k disk    commit
000700011026
000800000000     C*---------------------------------------------------------------*
000900020109      *conversioni
001000011026     c                   exsr      fifgt
001100990113     c                   seton                                        lr
001200990113     c*-------------------------------------------------
001300011026     c     fifgt         begsr
001400990113     c*-------------------------------------------------
001500081229     c     ktgt          chain     fitgt01l                           99
001600081229     c     kfgt          setll     fifgt01l
001700081229     c                   do        *hival
001800081229     c     kfgt          reade     fifgt01l
001900081229     c                   if        %eof(fifgt01l)
002000081229     c                   leave
002100081229     c                   end
002200090113     c                   if        tsr <> ' '  and fgttsr <> tsr
002300090113     c                   iter
002400090113     c                   end
002500090507     c* testo il tipo servizio perchè possono esistere cod.tariffe = 0
002600090507     c* quindi se il tipo servizio è = ' ' vuol dire che voglio cancellare
002700090507     c* tutto altrimenti solo quel codice tariffa
002800090507     c                   if        tsr <> ' '  and fgtctr <> ctr
002900090113     c                   iter
003000090113     c                   end
003100081229     c                   if        fgtatb = *blank and fgtprg = tgtprg
003200020211     c                   exsr      figli
003700100526     c                   delete    fifgt000
003800020114     c                   end
003900081229     c                   enddo
004000020110
004100990113     c                   endsr
004200020116     c*-------------------------------------------------
004300020211     c     figli         begsr
004400020116     c*-------------------------------------------------
004500020211     c     kfgt          setll     fifpt01l
004600020211     c                   do        *hival
004700020211     c     kfgt          reade     fifpt01l                               98
004800020211     c   98              leave
004900081229     c                   if        fptprg = tgtprg
005000090505     c                   if        tsr <> ' '  and fpttsr <> tsr
005100090505     c                   iter
005200090505     c                   end
005300090507     c                   if        tsr <> ' ' and fptctr <> ctr
005400090505     c                   iter
005500090505     c                   end
005600020211     c                   exsr      dettaglio
006000100526     c                   delete    fifpt000
006100081229     c                   end
006200020211     c                   enddo
006300020211
006400020116     c                   endsr
006500020211     c*-------------------------------------------------
006600020211     c     dettaglio     begsr
006700020211     c*-------------------------------------------------
006800020211     c     kfpd          setll     fifpd01l
006900020211     c                   do        *hival
007000020211     c     kfpd          reade     fifpd01l                               97
007100020211     c   97              leave
007200020211
007300081229     c                   if        fptprg = tgtprg
007700100526     c                   delete    fifpd000
007800081229     c                   end
007900020211     c                   enddo
008000020211
008100020211     c                   endsr
008200011026     c*-------------------------------------------------
008300011026     c     *inzsr        begsr
008400011026     c*-------------------------------------------------
008500020211     c     *entry        plist
008600020211     c                   parm                    pdr               7 0
008700020211     c                   parm                    sml               1
008800020211     c                   parm                    prg               3 0
008900090113     c                   parm                    tsr               1
009000090113     c                   parm                    ctr               3 0
009100020211     c     kfgt          klist
009200020211     c                   kfld                    pdr
009300020211     c                   kfld                    sml
009400011026
009500020211     c     kfpd          klist
009600020211     c                   kfld                    fptpdr
009700020211     c                   kfld                    fptsml
009800020211     c                   kfld                    fpttsr
009900020211     c                   kfld                    fptctr
010000020211     c                   kfld                    fptprg
010100020211     c                   kfld                    fptctd
010200081229
010300081229     c     ktgt          klist
010400081229     c                   kfld                    pdr
010500081229     c                   kfld                    sml
010600081229     c                   kfld                    prg
010700011026
010800011026     c                   endsr
010900011129     C*--------------------------------------------------------------------
