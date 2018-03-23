000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100112     Ftabel00f  if   e           K DISK
000400100121     FWFMON00F  iF   E           K DISK
000500100121     FFNblp31L  uF   E           K DISK
000600091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700091125
000800100112     D kpjba         E DS
000900100112     d totpes          s             12  0
001000100112     d totvol          s             13  0
001100100112     d w0140           s             14  0
001200100112     d alfarmn         s             14
001300100121     d alfacolli       s             10
001400100121     d wcolli          s             10  0
001500100112     d kcod            s                   like(tblcod)
001600100112     d kkey            s                   like(tblkey)
001700021214      *--------------------------------------------------
001800100112     c     *entry        plist
001900100112     c                   parm                    kpjba
002000100112
002100100112     C                   Z-ADD     1             CODUT             1 0
002200100112
002300100113     C     Kmondo        KLIST
002400100113     C                   KFLD                    kfile             1
002500100113     C                   KFLD                    alfarmn
002600100112     C     KTAB1         KLIST
002700100112     C                   KFLD                    CODUT
002800100112     C                   KFLD                    KCOD
002900100112     C                   KFLD                    KKEY
003000070913     c     kblp          klist
003100091125     c                   kfld                    lnp               3 0
003200091125     c                   kfld                    aas               4 0
003300091125     c                   kfld                    mgs               4 0
003400100112     c                   z-add     049           lnp
003500100111     c                   z-add     2010          aas
003600100113     c                   z-add     0113          mgs
003700091125
003800070913     c
003900091125     c     kblp          setll     fnblp31l
004000091125     c     kblp          reade     fnblp31l
004100051110
004200091125    1c                   DOW       not %EOF(FNblp31l)
004300100111     c*
004400100112     c                   if        blpksc=0493666
004500100121     c
004600100121     c                   exsr      fileout
004700100121     c                   endif
004800100112     C*
004900091125     c     kblp          reade     fnblp31l
005000091125     c                   enddo
005100021216      *
005200021214     C                   eval       *inlr = *on
005300100112     c*.-------------------------------------------------------------
005400100112     c     FileOUT       BEGSR
005500100112     c                   eval      w0140=blprmn
005600100112     c                   movel     w0140         alfarmn
005700100113     c* Vedo di quale file si tratta
005800100113     c                   select
005900100113     c                   when      blprma='SEP2009XXX '
006000100113     c                   eval      kfile='A'
006100100113     c                   when      blprma='SEP20091ST '
006200100113     c                   eval      kfile='B'
006300100113     c                   when      blprma='SEP20092ND '
006400100113     c                   eval      kfile='C'
006500100113     c                   when      blprma='SEP20093RD '
006600100113     c                   eval      kfile='D'
006700100113     c                   ENDSL
006800100112     c
006900100113     c     kmondo        chain     wfmon00f
007000100112    1c                   if        not %found(wfmon00f)
007100100112     c                   except    errore
007200100112   x1c                   else
007300100121     c* Reperisco il numero reale dei colli e aggiorno fnblp
007400100121     c                   eval      alfacolli=mon_colli
007500100121     c                   eval      alfacolli=%xlate(' ':'0':alfacolli)
007600100121     c                   eval      wcolli=%int(alfacolli)
007700100121     c                   if        wcolli<>blpncl
007800100121     c                   except    divcolli
007900100121
008000100121     c                   eval      blpncl=wcolli
008100100121     c                   except    Aggio
008200100121     c                   endif
008300100121     c                   endif
008400100121     c
008500100121     c                   ENDSR
008600100112
008700091125     OQSYSPRT   E            errore      1
008800100112     O                                              'Non trovato RMN'
008900100112     O                       blprmn           +   1
009000100112     O                                        +   2 'Cad'
009100100112     O                       blpCAD           +   1
009200100112     O                                        +   2 'Lod'
009300100112     O                       blplod           +   1
009400100112     O                                        +   2 'Prd'
009500100112     O                       blpprd           +   1
009600100121     OQSYSPRT   E            divcolli    1
009700100121     O                                              'Sistemo numero colli.BOLLA'
009800100121     O                       blprmn           +   1
009900100121     o                                        +   2 'Num colli errato:'
010000100121     O                       blpncl        2  +   1
010100100121     o                                        +   2 'Num colli corretto:'
010200100121     O                       wcolli        2  +   1
010300100121     OFNBLP000  E            Aggio
010400100121     O                       blpncl
