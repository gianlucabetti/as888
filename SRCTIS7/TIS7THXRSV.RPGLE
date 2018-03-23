000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200081210     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500991027
000600991027     D prmppt          s             50
000700991027     D prmesito        s              1
000800991027     D wrkesito        s                   like(prmesito)
000900021007
001000081210
001100081210     D*------------------
001200081210     D* LINKING A DEFINIZIONI ESTERNE
001300081210     D*------------------
001400081210     D/COPY GAITRASRC/SRCPROTOPR,UBIFSCHKLK
001500081210     D/COPY GAITRASRC/SRCPROTOPI,UBIFSCHKLK
001600081210
001700081210
001800021007
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
002100991027
002200081007
002300081007
002400991027     C     traduci       BEGSR
002500991027     C*
002600080924     C                   READ      TIVGDTMP
002700080924     C                   DOW       not %eof(TIVGDTMP)
002800080924     C*
002900090123     C                   if        %subst(vgddta:1:1) = 'I'
003000081210     C                   exsr      existsLINK
003100081210     C                   endif
003200080613     C*
003300081007     C                   DELETE    TIVGD000
003400991027     C*
003500080924     C                   READ      TIVGDTMP
003600081007     C                   ENDDO
003700991027     C*
003800991027     C                   eval      wrkesito = '0'
003900991027     C*
004000910830     C                   ENDSR
004100081210
004200081210
004300081210
004400081210     C     existsLINK    BEGSR
004500090123     C*
004600090123     C* valorizzazione proprietà
004700090123     C                   eval      pInFullIFS = %trim(%subst(vgddta:2))
004800081210     C*
004900081210     C* Chiamata metodo Found
005000081210     C                   if        UBIFSCHKLK_Found(pInPathIFS
005100081210     C                                             :pInFileIFS
005200081210     C                                             :pInFullIFS
005300081210     C                                             :pOutResult) = 0
005400081210     C                   if        pOutResult = '*FOUND'
005500081210     C                   else
005600081210     C                   eval      wrkesito = '2'
005700081210     C                   endif
005800081210     C                   endif
005900081210     C*
006000081210     C                   ENDSR
006100081210
006200081210
006300081210
006400081007
006500991027      /TITLE Operazioni iniziali.
006600991027     C     *inzsr        BEGSR
006700991027     C*
006800991027     C     *ENTRY        PLIST
006900991027     C                   parm                    prmppt
007000991027     C     wrkesito      parm      wrkesito      prmesito
007100050622     C*
007200991027     C                   ENDSR
