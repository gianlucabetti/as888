000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400160412      * elimina campo natura merce
000500040225      *------------------------------------------------------------------------*
000600160413     fTitas38c  uf   e           k Disk
000700160413     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800160413     d
000900160413     D kpjba         E DS
001000160413     d
001100160413     D param           DS                  INZ
001200160413     D  dal_aas                1      4  0
001300160413     D  dal_mgs                5      8  0
001400160414     D  dal                    1      8  0
001500160413     D  al                     9     16  0
001600160413     D                 DS                  INZ
001700160413     D  tasaas                 1      4  0
001800160413     D  tasmgs                 5      8  0
001900160413     D  sped                   1      8  0
002000160413     d
002100160413     ITITAS000      02
002200160413     ITITAS010      03
002300160413     ITITASP00      04
002400160413     I*
002500001006      *------------------------------------------------------------------------*
002600160413     c     *entry        plist
002700160413     c                   parm                    kpjba
002800160413     c                   movel     kpjbu         param
002900111107     c
003000160413     c     ktas          klist
003100160413     c                   kfld                    dal_aas
003200160413     c                   kfld                    dal_mgs
003300160413     c
003400160413     c                   setoff                                       020304
003500160413     c     ktas          setll     titas38c
003600160413     c                   Read      titas38c
003700040226
003800160413    1c                   dow       not %eof(titas38c)
003900160413     c
004000160413     c                   if        sped>al
004100160413     c                   leave
004200160413     c                   endif
004300160413     c
004400160412     c                   clear                   tasnas
004500160413
004600160413     c   02              except    tas
004700160413     c   03              except    tas10
004800160413     c   04              except    tasp
004900160413     c   02              add       1             conta             7 0
005000160413     c   03              add       1             conta10           7 0
005100160413     c   04              add       1             contaP            7 0
005200160413
005300160413     c                   setoff                                       020304
005400160413     c                   Read      titas38c
005500040415    1c                   EndDo
005600010202
005700160413     c
005800160413     c                   except    totali
005900040225     c                   Eval      *Inlr = *On
006000160413
006100160412     Otitas000  E            tas
006200160412     O                       TASNAS
006300160413     Otitas010  E            tas10
006400160413     O                       TASNAS
006500160413     Otitasp00  E            tasp
006600160413     O                       TASNAS
006700160413     OQSYSPRT   E            totali      1
006800160414     O                                         +  1 'bolle pulite dal'
006900160414     O                       DAL               +  1 '    /  /  '
007000160414     O                                         +  1 ' al '
007100160414     O                       AL                +  1 '    /  /  '
007200160414     O                                         +  2 'TITAS00F'
007300160414     O                       conta         2   +  1
007400160414     O                                         +  2 'TITAS10F'
007500160413     O                       conta10       2   +  1
007600160414     O                                         +  2 'TITASP0F'
007700160413     O                       contaP        2   +  1
