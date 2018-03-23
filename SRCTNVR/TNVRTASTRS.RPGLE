000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200090715     Ffidta00r  o    E             DISK
000300090715     Ffiar531C  iF   E           K DISK
000400090715     FTITAS38C  iF   E           K DISK
000500011003     F                                     IGNORE(TITASP00)
000600011003     F                                     IGNORE(TITAS010)
000700090715     d Skpo            s              3  0 Dim(5)
000800090715     d
000900090715     d dAr5Trs       e ds                  Inz
001000090715     d KAr5Trd         s                   Like(Ar5Trd) Inz('TRS')
001100011003     C*------------------------------------------------------------------------*
001200011003     C* MAIN LINE
001300011003     C*------------------------------------------------------------------------*
001400090715     c     ktas          klist
001500090715     c                   kfld                    anno              4 0
001600090715     c                   kfld                    mmgg              4 0
001700090715     c
001800090715     c                   z-add     2009          anno
001900090715     c                   z-add     0710          mmgg
002000090715     c     KeyFiar5      Klist
002100090715     c                   Kfld                    tasAas
002200090715     c                   Kfld                    tasLnp
002300090715     c                   Kfld                    tasNrs
002400090715     c                   Kfld                    tasNsp
002500090715     c                   Kfld                    KAr5Trd
002600011003     C*
002700090715     C     ktas          SETLL     TITAS000
002800090715     C                   read      TITAS000
002900090715     c
003000090715    1c                   dow       not %eof(titas38c)
003100090715    2c                   if        tasmgs>0713
003200090715     c                   leave
003300090715     c                   endif
003400090715     c
003500090715    2c                   if        tasdbr=20090713
003600090715    2c                   if        tastfp=116 or tastfp=275 or tastfp=05
003700090715     c
003800090715    3c                   if        taslna=081 or taslna=082 or taslna=104
003900090715     c                             or taslna=138  or taslna=113
004000090715    4c                   if        tasctm<>'4 ' and tasctm<>'4F'
004100090715     c* Controllo che non esiste già
004200090715     c     keyFiar5      Setgt     Fiar531c
004300090715    5c                   Do        *Hival
004400090715     c     keyFiar5      Readpe(n) Fiar531c
004500090715     c                   If        %Eof(Fiar531c)
004600090715     c                   Leave
004700090715     c                   EndIf
004800090715     c                   Movel     Ar5Uni        dAr5Trs
004900090715     c                   Eval      Skpo(1) = §Ar5Flp
005000090715     c                   Eval      Skpo(2) = §Ar5Fl2
005100090715     c                   Eval      Skpo(3) = §Ar5Fl3
005200090715     c                   Eval      Skpo(4) = §Ar5Fl4
005300090715     c                   Eval      Skpo(5) = §Ar5Fl5
005400090715     c                   Leave
005500090715    5c                   EndDo
005600090715     c
005700090715     c     099           LOOKUP    SKPO                                   90
005800090715    5c
005900090715     C                   IF        NOT *IN90
006000090715     c                   clear                   fidta000
006100090715     c                   Eval      DtaAas = tasAas
006200090715     c                   Eval      DtaLnp = tasLnp
006300090715     c                   Eval      DtaNrs = tasNrs
006400090715     c                   Eval      DtaNsp = tasNsp
006500090715     c                   Eval      DtaPoc = 099
006600090715     c                   Eval      DtaTrd = 'DET'
006700090715     c                   Eval      DtaDta = 20090714
006800090715     c                   Write     Fidta000
006900090715     c
007000090715     c                   Eval      DtaTrd = 'DUT'
007100090715     c                   Eval      DtaDta = 20090713
007200090715     c                   Write     Fidta000
007300090715    5c                   endif
007400090715    4c                   endif
007500090715    3c                   endif
007600090715    2c                   endif
007700090715    2c                   endif
007800090715      *
007900090715     C*
008000090715     C                   read      TITAS000
008100090715    1C                   ENDDO
008200011003     C
008300011003     C                   SETON                                        LR
