000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200090715     Ffiar531C  uF   E           K DISK
000300090715     FTITAS38C  iF   E           K DISK
000400011003     F                                     IGNORE(TITASP00)
000500011003     F                                     IGNORE(TITAS010)
000600090715     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
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
003900090715     c                             or taslna=138
004000090715    4c                   if        tasctm<>'4 ' and tasctm<>'4F'
004100090715     c* Controllo che non esiste già
004200090715     c     keyFiar5      Setgt     Fiar531c
004300090715    5c                   Do        *Hival
004400090715     c     keyFiar5      Readpe    Fiar531c
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
005600090715     c*
005700090715     c                   if        skpo(1)=113 and skpo(2)=99
005800090715     c                   eval      §ar5flp=99
005900090715     c                   eval      §ar5det=§ar5de2
006000090715     c                   eval      §ar5dut=§ar5du2
006100090715     c                   clear                   §ar5fl2
006200090715     c                   clear                   §ar5de2
006300090715     c                   clear                   §ar5du2
006400090715     c                   movel     dar5trs       ar5uni
006500090715     c                   update    fiar5000
006600090715     c
006700090715     C                   MOVEL     AR5UNI        W100            100
006800090715     C                   EXCEPT    BOLLA
006900090715     c                   leave
007000090715     c                   endif
007100090715     c
007200090715    4c                   endif
007300090715    3c                   endif
007400090715    2c                   endif
007500090715    2c                   endif
007600090715      *
007700090715     C*
007800090715     C                   read      TITAS000
007900090715    1C                   ENDDO
008000011003     C
008100011003     C                   SETON                                        LR
008200090715     OPRTF198   E            bolla            02
008300090715     O                       tasaas           +   1
008400090715     O                       taslnp           +   1
008500090715     O                       tasnrs           +   1
008600090715     O                       tasnsp        2  +   1
008700090715     O                       W100             +   1
