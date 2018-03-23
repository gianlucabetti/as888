000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200111012     Ffiar503l  iF   E           K DISK    infds(fiar5inf)
000201111012     Ffiar500f  UF   E             DISK    rename(fiar5000:fiar5f00)
000400111012     FTITAS30C  UF   E           K DISK    usropn extfile(wtas30)
000401111012     f****                                 ignore(titas010)
000402111012     f                                     ignore(titasp00)
000403111012     FTITA730C  IF   E           K DISK    usropn extfile(wta730)
000405111012     f                                     ignore(tita7p00)
000406111012     fwferremd  o  a E             disk
000407111012     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000408111012
000409111012     D fiar5inf        ds
000410111012     D  fiar5nrr             397    400i 0
000411111012     d dsemail       e ds
000700090715     d
000800111012     d dAr5emd       e ds                  Inz
000801111012     d dtasflo       e ds
000901111012     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
000902111012     d Wta730          s             21    inz('GAITRAGRU /TITA730C')
000903111012     d num             s              3  0 dim(100)
000904111012     d numf            s              3  0 dim(100)
000905111012     d ccm             s              7  0 dim(100)
000906111012     d variam          s             11  3 dim(100)
000907111012     D waggio          s              1
000908111012     d wccm            s                   like(tasccm)
000909111012     d wnum            s              3  0
000910111012     d wimpvrm         s                   like(ta7van)
000911111012     d oldemd          s                   like(§floemd)
000912111012
001200100317     d
001201111012     ITITAS000      05
001202111012     ITITAS010      06
001500100317
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
002600100317     c
002700090715     c     ktas          klist
002800111012     c                   kfld                    ar5aas
002900111012     c                   kfld                    ar5lnp
003000111012     c                   kfld                    ar5nrs
003001111012     c                   kfld                    ar5nsp
003002111012     c     kta7          klist
003003111012     c                   kfld                    tasaas
003004111012     c                   kfld                    taslnp
003005111012     c                   kfld                    tasnrs
003006111012     c                   kfld                    tasnsp
003007111012     c                   kfld                    tastbl
003100100317     c
004401111012     c*
004402111012     c                   clear                   num
004403111012     c                   clear                   numf
004404111012     c                   clear                   ccm
004406111012     c                   clear                   variam
004407111012     c                   eval      waggio='S'
004408111012     c*
004409111012     c                   Open(e)   titas30c
004410111012     c                   if        not %open(titas30c)
004411111012     c                   eval      %subst(Wtas30:7:4)='GRPS'
004412111012     c                   Open      titas30c
004413111012     c                   endif
004414111012     c                   Open(e)   tita730c
004415111012     c                   if        not %open(tita730c)
004416111012     c                   eval      %subst(Wta730:7:4)='GRPS'
004417111012     c                   Open      tita730c
004418111012     c                   endif
004500011003     C*
004600111012     C     *loval        SETLL     fiar503l
004700111012     C                   read      fiar503l
004800090715     c
004900111012    1c                   dow       not %eof(fiar503l)
004901111012     c* controllo se va bene indirizzo email
004902111012     c                   movel     ar5uni        dar5emd
004903111012     c                   Clear                   dsemail
004904111012     c                   Eval      §emlindi = §ar5eml
004905111012     c                   Call      'XEMAIL'
004906111012     c                   Parm                    dsemail
004907111012      * Errore
004908111012if  2c                   If        §emlerro <> '0'
004909111012     c* chaino titas ed elimino flag che pilota la tassazione della varia "m"
004910111012     c                   setoff                                       0506
004913111012     c     ktas          chain     titas30c
004914111012    3c                   if        %found(titas30c)
004915111012    4c                   if        *in05 and tasnft=0
004916111012     c                   movel     tasflo        dtasflo
004917111012     c                   eval      oldemd=§floemd
004918111012     c                   clear                   §floemd
004919111012     c                   movel     dtasflo       tasflo
004920111012     c* verifico se tassata la varia 'm' e in questo caso la stampo
004921111012     c                   exsr      sr_variam
004922111012     c                   if        waggio='S'
004923111012     C   05              UPDATE    TITAS000
004924111012     c                   endif
004925111012     c                   z-add     1             i                 4 0
004926111012     c     tasccm        lookup    ccm(i)                                 30
004927111012     c                   if        *in30='0'
004928111012     c                   z-add     1             i
004929111012     c     0000000       lookup    ccm(i)                                 30
004930111012     c                   z-add     tasccm        ccm(I)
004931111012     c                   endif
004932111012     c                   add       1             num(I)
004933111012     c                   except    bolla
004934111012     c* cancello record da fiar5
004935111012     c                   if        waggio='S'
004936111012     c     fiar5nrr      delete    fiar500f
004937111012     c                   endif
004938111012     c                   else
004939111012     c* se fatturato memorizzo nel totale delle fatturate e conteggio la variam
004940111012     c                   z-add     1             i                 4 0
004941111012     c     tasccm        lookup    ccm(i)                                 30
004942111012     c                   if        *in30='0'
004943111012     c                   z-add     1             i
004944111012     c     0000000       lookup    ccm(i)                                 30
004945111012     c                   z-add     tasccm        ccm(I)
004946111012     c                   endif
004947111012     c                   add       1             numf(I)
004948111012     c                   exsr      sr_variam
004949111012     c                   add       wimpvrm       variam(I)
004950111012    4c                   endif
004951111012     c                   else
004952111012     c* se non trovato titas30  stampo la spedizione
004953111012     c                   except    notitas
004954111012    3c                   endif
004955111012    2c                   endif
004956111012     c*
004957111012     C                   read      fiar503l
004958111012    1c                   enddo
004959111012     c*
004960111012     c                   close     titas30c
004961111012     c                   close     tita730c
004962111012     c                   z-add     1             i
004963111012     c                   do        100           i
004964111012     c                   if        ccm(I)=0
004965111012     c                   leave
004966111012     c                   endif
004967111012     c                   z-add     ccm(I)        wccm
004968111012     c                   z-add     num(I)        wnum
004969111012     c                   z-add     numf(i)       wnumf
004970111012     c                   z-add     variam(i)     wvariam
004971111012     c                   write     wferrrec
004972111012     c                   enddo
005000090715     c
013800011003     C
013900011003     C                   SETON                                        LR
013901111012     c******************************************************************************
013902111012     c*Recupero importo della varia 'm'
013903111012     c******************************************************************************
013904111012     C     sr_variam     begsr
013905111012     c                   clear                   wimpvrm
013906111012     c                   select
013907111012     c                   when      tassv1='m'
013908111012     c                   z-add     tasva1        wimpvrm
013909111012     c                   when      tassv2='m'
013910111012     c                   z-add     tasva2        wimpvrm
013911111012     c                   when      tassv3='m'
013912111012     c                   z-add     tasva3        wimpvrm
013913111012     c                   when      tassv3<>*blanks
013914111012     c     kta7          setll     tita730c
013915111012     c     kta7          reade     tita730c
013916111012     c                   dow       not  %eof(tita730c)
013917111012     c                   if        ta7svn='m'
013918111012     c                   z-add     ta7van        wimpvrm
013919111012     c                   leave
013920111012     c                   endif
013921111012     c     kta7          reade     tita730c
013922111012     c                   enddo
013923111012     c                   endsl
013924111012     c                   endsr
018600111012     OPRTF198   E            bolla       1
018700100317     O                       tasaas           +   1
018800100317     O                       taslnp           +   1
018900100317     O                       tasnrs           +   1
019000100317     O                       tasnsp        2  +   1
019100111012     o                       tasccm           +   1
019101111012     o                       wimpvrm       2  +   2
019102111012     o                       oldemd           +   1
019103111012     O          E            notitas     2
019104111012     O                       ar5aas           +   1
019105111012     O                       ar5lnp           +   1
019106111012     O                       ar5nrs           +   1
019107111012     O                       ar5nsp        2  +   1
019108111012     O                                        +   1 'Non trovato titas'
