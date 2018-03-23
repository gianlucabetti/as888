000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081121     H* TNVR11AR *---------------------------------------------------*
000300081229     H*       Scrive la testata nuova per un nuovo progressivo       *
000400000000     H*--------------------------------------------------------------*
000500081229     Ffifgt00f  iF   E             DISK
000600081222     F                                     INFDS(FGTDS)
000700081229     Fwxfgt00f  o  a E             DISK    rename(FIFGT000:wxFGT000)
000800081229     D*--------------------------------------------------------------*
000900081222     D FGTDS           DS
001000081222     D  NrecFGTFis           397    400B 0
001100081110      *
001200081209     D Data_Oggi       s              8s 0
001300081209      *
001400081211     D prmNREC         s              9s 0
001500081229     D newPRGtes       s                   like(FgtPRG)
001600081229     D dataDDT         s                   like(FgtDDT)
001700081229     D dataDST         s                   like(FgtDST)
001800900521     C*****************************************************************
001900081209      *   Crea le tariffe direttamente sull'autista fotocopiando
002000081209      *    la tariffa di riferimento.
002100081209     C*****************************************************************
002200000000     C     *ENTRY        PLIST
002300081229     c                   parm                    prmNREC
002400081229     c                   parm                    newPRGtes
002500081229     c                   parm                    dataDDT
002600081229     c                   parm                    dataDST
002700081209      *
002800081110     C                   SETON                                        LR
002900081209     c                   move      *date         Data_Oggi
003000081229      *
003100081229     c                   z-add     prmNREC       NrecFGTFis
003200081229     c     NrecFGTFis    chain     fiFGT00f
003300081229     c                   if        %Found(fiFGT00f)
003400081229      *
003500081229     c                   z-add     newPRGtes     fgtPRG
003600081229     c                   z-add     dataDDT       fgtDDT
003700081229     c                   z-add     dataDST       fgtDST
003800081229     c                   z-add     *date         fgtDUV
003900090108      *
004000090108     c                   if        fgtDCN > 0
004100090108     c                   z-add     99991231      fgtDCN
004200090108     c                   end
004300090108      *
004400090108     c                   if        fgtDTS > 0
004500090108     c                   z-add     99991231      fgtDTS
004600090108     c                   end
004700090108      *
004800081230     c                   eval      fgtDCV = 'Da ' + %char(fgtRAP) + '/' +
004900081230     c                             %char(fgtRCT)
005000081230     c                   clear                   fgtRAP
005100081230     c                   clear                   fgtRCT
005200081229      *
005300081229     c                   write     wxFGT000
005400081229     c                   end
005500081121      *
005600081110     C     FINE          TAG
005700081110     c                   return
005800081110     C*---------------------------------------------------------------*
