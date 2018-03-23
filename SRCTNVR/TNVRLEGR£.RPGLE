000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300170915     FDPcdp12i  iF   E           K DISK
000400170929     FDPLEG10F  o  A E             DISK
000500170929     FDPPDC00F  uf   E             DISK
000600170929     C*------------------------------------------------------------------------*
000700170929     c     depot         klist
000800170929     c                   kfld                    CDPATB
000900170929     c                   kfld                    CDPVER
001000170929     c                   kfld                    CDPDSTR
001100170929     C*------------------------------------------------------------------------*
001200170929     c                   seton                                        LR
001300170929      *
001400170929      **  1° cappario nuovo DPD valido
001500170929     C                   z-add     20170903      datsca            8 0
001600170929     C*------------------------------------------------------------------------*
001700170929      ** Depot attivi
001800170929     c                   read      DPpdc00f
001900170929      **
002000170929     c                   dow       not %EoF(DPpdc00f)
002100170929      **
002200170929      **   far SCADERE al giorno della trascodifica
002300170929     c                   if        pdcDSC > datsca
002400170929      **               *-------------------*
002500170929     c                   exsr      wri_LEG
002600170929      **               *-------------------*
002700170929     c                   eval      pdcDSC = datsca
002800170929     c                   end
002900170929      **
003000170929     c                   update    DPpdc000
003100170929     c                   read      DPpdc00f
003200170929      **
003300170929     c                   enddo
003400170915      **
003500170929     C                   SETON                                        LR
003600170929     c                   return
003700170929     C*------------------------------------------------------------------------*
003800170929     c     wri_LEG       begsr
003900170929      **
004000170929     c                   clear                   DPLEG000
004100170929      **
004200170929     c                   movel     PDCDPC        legDPC
004300170929     c                   movel     PDCNAR        legNAR
004400170929     c                   z-add     PDCKSC        legKSC
004500170929     c                   z-add     PDCDDE        legDDE
004600170929     c                   z-add     PDCDSC        legDSC
004700170929     c                   z-add     PDCDTV        legDTV
004800170929     c                   z-add     PDCORV        legORV
004900170929     c                   movel     PDCPRU        legPRU
005000170929     c                   movel     PDCORMA       legORMA
005100170929     c                   movel     PDCATB        legATB
005200170929     c                   movel     PDCFLO        legFLO
005300170929      **
005400170929     c                   move      *blank        CDPATB
005500170929     c                   z-add     3             CDPVER
005600170929     c                   movel     legdpc        CDPDSTR
005700170929     c     depot         chain     dpcdp12i
005800170929     c                   if        %Found(dpcdp12i)
005900170929     c                   movel     cdpDPTN       legdep7
006000170929     c                   z-add     cdpBUCN       legbun
006100170929     c                   else
006200170929     c                   move      *zeros        legdep7
006300170929     c                   move      cdpDstr       legdep7
006400170929     c                   end
006500170929      **
006600170929     c                   write     dpleg000
006700170929      **
006800170929     c                   endsr
006900090424     C*---------------------------------------------------------------*
