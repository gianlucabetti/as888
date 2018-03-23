000100920317     H DATEDIT(*YMD)
000200021017     F* TNVR40R *----------------------------------------------------*
000300021017     F*           TRASCODIFICA FILE TESTI                            *
000400021017      * -------------------------------------------------------------*
000500021030     fFnTxt02L  UF   e           K disk
000600021018     fTabel00f  if   e           k disk
000700021018      *
000800021018     d Ds1H          e ds
000900021022     d DfntxtH       e ds
001000021017      *----------------------------------------------------------------*
001100021030     C     *entry        plist
001200021030     c                   parm                    pog               3
001300021030      *
001400021030     C     kTab          klist
001500021030     C                   kfld                    kut
001600021030     C                   kfld                    Cod
001700021030      *
001800021030     C     kTxt          klist
001900021030     C                   kfld                    klet              2
002000021030     C                   Kfld                    KPO               3 0
002100021030     C                   Kfld                    Klin              1
002200021030     C                   Kfld                    Krig              1
002300021030      *
002400021030     c                   move      pog           kpo
002500021030     c                   move      'I'           klin
002600021030     c                   move      'H'           krig
002700021030     c                   move      '1H'          Cod               2
002800021022     c                   z-add     1             Kut               1 0
002900021017      *----------------------------------------------------------------*
003000021030     c     KTAB          setll     TABEL00f
003100021030     c     KTAB          reade     tabel00f
003200021017      *
003300021030    1c                   Dow       not %EOF(TABEL00F)
003400021030     c                   movel     tbluni        ds1H
003500021030     c                   movel     tblkey        klet
003600021021      *
003700021030     c     ktxt          chain     Fntxt000
003800021030     c                   If        %found(FNTXT02L)
003900021030     c                   movel     txtrig        DfntxtH
004000021030     c                   movel     §1HDes        §TxDes
004100021030     c                   movel(P)  DfntxtH       TxtRig
004200021030     c                   update    FnTxt000
004300021030    4c                   EndIf
004400021017      *
004500021030     c     KTAB          reade     tabel00f
004600021021    1c                   enddo
004700020705      *
004800021017     c                   SETON                                        LR
004900021021      *----------------------------------------------------------
