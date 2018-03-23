000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200111012     Ffiar503l  iF   E           K DISK
000400111013     FTITAS30C  uF   E           K DISK    usropn extfile(wtas30)
000401111013     f                                     ignore(titas010)
000402111012     f                                     ignore(titasp00)
000407111012     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000408111012
000700090715     d
000801111012     d dtasflo       e ds
000901111012     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
000912111012
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
002600100317     c
002700090715     c     ktas          klist
002800111012     c                   kfld                    ar5aas
002900111012     c                   kfld                    ar5lnp
003000111012     c                   kfld                    ar5nrs
003001111012     c                   kfld                    ar5nsp
003100100317     c
004401111012     c*
004408111012     c*
004409111012     c                   Open(e)   titas30c
004410111012     c                   if        not %open(titas30c)
004411111012     c                   eval      %subst(Wtas30:7:4)='GRPS'
004412111012     c                   Open      titas30c
004413111012     c                   endif
004500011003     C*
004600111012     C     *loval        SETLL     fiar503l
004700111012     C                   read      fiar503l
004800090715     c
004900111012    1c                   dow       not %eof(fiar503l)
004909111013     c* chaino titas e aggiorno flag che pilota la tassazione della varia "m"
004913111012     c     ktas          chain     titas30c
004914111012    3c                   if        %found(titas30c)
004915111012     c                   movel     tasflo        dtasflo
004916111013     c                   if        §floemd=' '
004917111013     c                   eval      §floemd='S'
004918111013     c                   movel     dtasflo       tasflo
004919111013     c                   update    titas000
004920111012     c                   except    bolla
004921111012     c                   endif
004954111012    3c                   endif
004956111012     c*
004957111012     C                   read      fiar503l
004958111012    1c                   enddo
004959111012     c*
004960111012     c                   close     titas30c
005000090715     c
013800011003     C
013900011003     C                   SETON                                        LR
018600111012     OPRTF198   E            bolla       1
018700100317     O                       tasaas           +   1
018800100317     O                       taslnp           +   1
018900100317     O                       tasnrs           +   1
019000100317     O                       tasnsp        2  +   1
019100111012     o                       tasccm           +   1
019101111012     o                       tasaas           +   1
019102111012     o                       tasmgs           +   1
