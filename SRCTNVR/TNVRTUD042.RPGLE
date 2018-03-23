000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200000000     H*--------------------------------------------------------------*
000300060323     ftntud01l  if   e           k disk
000400060323     ftntud03l  uf a e           k disk    prefix(NEW:3)
000500060323     F                                     RENAME(tntud000:tntud003)
000600060323     D tfp             S              9  0 DIM(500)
000700051215     d
000800051215     d
000900060323     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
001000060323     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001100051215     d Dataiso         s               d   datfmt(*iso)
001200060323     d Kmult           s                   Like(tudmult) inz('042')
001300060323     d Ktfp            s                   Like(tudfil)
001400060323     d Kcli            s                   Like(tudcli) inz(0)
001500030722     F**
001600900518     C***************************************************************
001700060323     c                   clear                   tfp
001800060323     c                   clear                   dataiso
001900060323     c     '040'         setll     tntud01l
002000060323     c     '040'         reade     tntud01l
002100060323     c                   dow       not %eof(tntud01l)
002200060323     c
002300060323     c                   if        tuddta<>dataiso
002400060323     c
002500060323     c                   if        dataiso<>*loval
002600060323     c                   exsr      scrivi
002700060323     c                   endif
002800060323     c                   clear                   tfp
002900060323     c                   eval      dataiso=tuddta
003000060323     c                   endif
003100051215     c
003200060323     C                   CLEAR                   DSLV55
003300060323     C                   MOVEL     'P'           D55TPT
003400060323     C                   MOVEL     tudfil        D55LIN
003500060323     C     *iso          MOVEL     tuddta        D55DRF
003600060323     C                   CALL      'FNLV55R'
003700060323     C                   PARM                    DSLV55
003800060323     c                   add       tudqta        tfp(d55tfp)
003900060323     c
004000060323     c     '040'         reade     tntud01l
004100060323     c                   enddo
004200060323     c
004300060323     c                   if        dataiso<>*loval
004400060323     c                   exsr      scrivi
004500060323     c                   endif
004600941216     C*
004700051123     c
004800051123     C                   SETON                                        LR
004900051215     C**************************************************************************
005000051215     c* Scrivo il file TNTUD00F
005100051215     C**************************************************************************
005200060323     C     SCrivi        BEGSR
005300060323     c     ktud          Klist
005400060323     c                   Kfld                    kmult
005500060323     c                   Kfld                    ktfp
005600060323     c                   Kfld                    dataiso
005700051215     c
005800060323     c                   z-add     1             xx                3 0
005900060323     c                   do        500           xx
006000060323     c                   if        tfp(xx)>0
006100060323     c                   eval      ktfp=xx
006200051215     c
006300060323     c     ktud          chain     tntud03l
006400051215     c
006500060323     c                   if        not %found(tntud03l)
006600060323     c                   eval      newdta=dataiso
006700060323     c                   eval      newmult=kmult
006800060323     c                   eval      newfil=ktfp
006900060323     c                   eval      newqta=tfp(xx)
007000060323     c                   clear                   newcli
007100060323     c                   write     tntud003
007200051215     c                   else
007300060323     c                   eval      newqta=tfp(xx)
007400060323     c                   update    tntud003
007500051215     c                   endif
007600051215     c                   endif
007700060323     c                   enddo
007800051215     c
007900051215     c                   ENDSR
