000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300161130     FFNarb01L  iF   E           K DISK
000400051110     FFNblt01L  iF   E           K DISK
000500161130     FFiart01L  uF   E           K DISK    prefix(new:3)
000600161130     FFNart01L  iF   E           K DISK
000700161130     F                                     INFDS(blt1)
000800161130     FFNart00f  uF   E             DISK    rename(fnart000:fnartfis)
000900161130     FFNAGB01L  iF A E           K DISK
001000161130     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100161130     D blt1            DS
001200161130     D  artNRR               397    400B 0
001300021214      *--------------------------------------------------
001400161130     c     2016          setll     fnblt01l
001500051110     c                   read      fnblt01l
001600051110
001700051110    1c                   DOW       not %EOF(FNblt01l)
001800161130     c     kblt          chain     fnarb01l                           90
001900161130     c                   if        %found(fnarb01l) and arbduc=0
002000161130     c     kbltS         chain     fnart01l                           90
002100161130     c                   if        bltdfv>0 and artdfv=0
002200161130     c                   except    err
002300161130
002400161130     c     artnrr        chain     fnartfis
002500161130     c                   movel     bltdfv        artdfv
002600161130     c                   movel     bltnpp        artnpp
002700161130     c                   update    fnartfis
002800161130     c
002900161130     C                   CLEAR                   FNAGB000
003000161130     C                   MOVEL     'A'           AGBTBO
003100161130     C                   MOVEL     'DT'          AGBAGB
003200161130     C                   MOVEL     artAAS        AGBAAS
003300161130     C                   MOVEL     artLNP        AGBLNP
003400161130     C                   MOVEL     artNRS        AGBNRS
003500161130     C                   MOVEL     artNSP        AGBNSP
003600161130     C*
003700161130     C                   WRITE     FNAGB000                             99
003800161130
003900161130     c                   if        artdcm>0
004000161130     c     kbltS         chain     fiart01l
004100161130     c                   if        %found(fiart01l)
004200161130     c                   movel     bltdfv        newdfv
004300161130     c                   movel     bltnpp        newnpp
004400161130     c                   update    fiart000
004500161130     c                   endif
004600161130     c                   endif
004700161130
004800161130     c                   endif
004900161130     c                   endif
005000161130     c
005100051110     c                   read      fnblt01l
005200051110     c                   enddo
005300021216      *
005400021214     C                   eval       *inlr = *on
005500021214      *---------------------------------------------------------
005600021214     c     *inzsr        BEGSR
005700021214      *
005800051110     C     Kblt          KLIST
005900051110     C                   KFLD                    bltaas
006000051110     C                   KFLD                    bltlnp
006100051110     C                   KFLD                    bltnrs
006200051110     C                   KFLD                    bltnsp
006300161130     C     KbltS         KLIST
006400161130     C                   KFLD                    bltaas
006500161130     C                   KFLD                    bltlnp
006600161130     C                   KFLD                    bltnrs
006700161130     C                   KFLD                    bltnsp
006800161130     C                   KFLD                    bltnsc
006900021216     C*
007000021214     C                   ENDSR
007100161130     OQSYSPRT   E            err         1
007200161130     O                                              'BLTcon DFV e non ART'
007300161130     o                       bltaas           +   1
007400161130     o                       bltlnp           +   1
007500161130     o                       bltnrs           +   1
007600161130     o                       bltnsp           +   1
007700161130     o                       bltlna           +   1
007800161130     o                       bltnsc           +   2
007900161130     o                       bltdfv           +   2
