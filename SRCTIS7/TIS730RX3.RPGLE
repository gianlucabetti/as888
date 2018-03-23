000100970526     H*------------------------------------------------------------------------*
000200000321     H* Reperimento archivi effettivamenti downloadati dai Clienti
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     F*------------
000900001220     Fsgius     UF   E             DISK
001000001220     Ftiabl01l  UF A E           K DISK
001100970526     C*------------------------------------------------------------------------*
001200970526     C* MAIN LINES
001300970526     C*------------------------------------------------------------------------*
001400970526     C*
001500001220     C* Chiave su TIABL01L
001600001220     C     keyabl        klist
001700001220     C                   kfld                    kabsun            9
001800001220     C                   kfld                    kabisv            2
001900001220     C                   kfld                    kabtip            3
002000001220     C*
002100001220     C                   eval      kabisv = 'TT'
002200001220     C                   eval      kabtip = 'SG1'
002300001220     C*
002400001220     C                   read      sgius                                  40
002500000321     C                   dow       *in40=*off
002600001220     C                   movel     '0'           chkrec            1
002700001220     C*
002800001220     C                   eval      kabsun = ususer
002900001220     C     keyabl        chain     tiabl01l
003000001220     C                   if        %found(tiabl01l)
003100001220     C*                  if        ussnam = *blanks
003200001220     C*                  eval      ussnam = ablrsp
003300001220     C*                  movel     '1'           chkrec
003400001220     C*                  endif
003500001220     C*                  if        usmail = *blanks
003600001220     C*                  eval      usmail = ableml
003700001220     C*                  movel     '1'           chkrec
003800001220     C*                  endif
003900001220     C*                  if        chkrec = '1'
004000001220     C*                  update    sgiusr
004100001220     C*                  endif
004200001220     C                   else
004300001220     C                   clear                   tiabl000
004400001220     C                   eval      ablsun = ususer
004500001220     C                   eval      ablisv = 'TT'
004600001220     C                   eval      ablute = usanam
004700001228     C                   eval      ablpwd = *all'@'
004800001220     C                   eval      ablrsp = ussnam
004900001220     C                   eval      ableml = usmail
005000001220     C                   eval      abltip = 'SG1'
005100001220     C                   eval      ablinv = '1'
005200001220     C                   write     tiabl000
005300001220     C                   endif
005400001220     C                   read      sgius                                  40
005500000321     C                   enddo
005600000321     C*
005700970526     C* FINE PGM
005800970526     C                   SETON                                        LR
