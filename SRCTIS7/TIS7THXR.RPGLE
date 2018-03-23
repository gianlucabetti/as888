000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500170411     FWAS7IFS1I uF   E           K DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000081210
001100090302     D wIFSKEY         s                   like(ifsKEY)
001200090302
001300090302     D*------------------
001400090302     D* LINKING A DEFINIZIONI ESTERNE
001500090302     D*------------------
001600090302     D/COPY GAITRASRC/SRCPROTOPR,UBIFSCHKLK
001700090302     D/COPY GAITRASRC/SRCPROTOPI,UBIFSCHKLK
001800090302
001900021007
002000090302
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300991027
002400081007
002500081007
002600991027     C     traduci       BEGSR
002700991027     C*
002800080924     C                   READ      TIVGDTMP
002900080924     C                   DOW       not %eof(TIVGDTMP)
003000080924     C*
003100090302     C                   if        %subst(vgddta:1:2) = 'K>'
003200090302     C                   eval      wIFSKEY = %trim(%subst(vgddta:3))
003300081210     C                   endif
003400090302     C*
003500090302     C                   if        %subst(vgddta:1:2) = 'F>'
003600090302     C                   exsr      existsLINK
003700090302     C                   endif
003800080613     C*
003900081007     C                   DELETE    TIVGD000
004000991027     C*
004100080924     C                   READ      TIVGDTMP
004200081007     C                   ENDDO
004300991027     C*
004400991027     C                   eval      wrkesito = '0'
004500991027     C*
004600910830     C                   ENDSR
004700090302
004800090302
004900090302
005000100316     C     chkWAS7IFS    BEGSR
005100090302     C*
005200170411     C     wIFSKEY       chain     was7ifs1i
005300170411     C                   if        %found(was7ifs1i)
005400090302     C
005500090302     C                   select
005600090302     C                   when      ifsSTS = '2'
005700090302     C                   eval      ifsSTS = *blanks
005800100316     C                   update    was7ifs0
005900090302     C                   when      ifsSTS = '3'
006000090302     C                   eval      ifsSTS = *blanks
006100090302     C                   eval      ifsOPZ = 'RENA'
006200100316     C                   update    was7ifs0
006300090302     C                   other
006400090302     C                   eval      wrkesito = '2'
006500090302     C                   endsl
006600090302     C*
006700090302     C                   else
006800090302     C                   eval      wrkesito = '0'
006900090302     C                   endif
007000090302     C*
007100090302     C                   ENDSR
007200081210
007300090302
007400090302
007500090302
007600090302     C     existsLINK    BEGSR
007700090302     C*
007800090302     C* valorizzazione proprietà
007900090302     C                   eval      pInFullIFS = %trim(%subst(vgddta:3))
008000090302     C*
008100090302     C* Chiamata metodo Found
008200090302     C                   if        UBIFSCHKLK_Found(pInPathIFS
008300090302     C                                             :pInFileIFS
008400090302     C                                             :pInFullIFS
008500090302     C                                             :pOutResult) = 0
008600090302     C                   if        pOutResult = '*FOUND'
008700090302     C                   eval      wrkesito = '0'
008800090302     C                   else
008900100316     C                   exsr      chkWAS7IFS
009000090302     C                   endif
009100090302     C                   endif
009200090302     C*
009300090302     C                   ENDSR
009400090302
009500081210
009600081210
009700081007
009800991027      /TITLE Operazioni iniziali.
009900991027     C     *inzsr        BEGSR
010000991027     C*
010100991027     C     *ENTRY        PLIST
010200991027     C                   parm                    prmppt
010300991027     C     wrkesito      parm      wrkesito      prmesito
010400050622     C*
010500991027     C                   ENDSR
