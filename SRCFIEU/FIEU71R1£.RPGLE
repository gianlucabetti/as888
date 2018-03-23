000100161298      ***********************************************************************
000200981216     H DECEDIT('0,') DATEDIT(*DMY.)
000300161298      *---------------------------------------------------------------------*
000400040115     FTNcfp01L  iF   E           k DISK
000500030723     F******Tita433C  iF   E           k DISK
000600030624     FTNEFE00F  UF   E             DISK
000700030623     FTNEFR00F  O  a E             DISK
000800040901     fFIEU71P   o    E             PRINTER OFLIND(*in66)
000900040901      *
001000040901     d stamp           ds
001100040901     d  lin                  367    368b 0
001200030624      *
001300030624     D WLBDAT          DS
001400030624     D  G02DAT                 1      8  0
001500030624     D  G02INV                 9     16  0
001600030624     D  G02ERR                17     17
001700030624     D  G02TGI                18     22  0
001800030624     d Digits          c                   Const('0123456789')
001900030623      *
002000161298      *---------------------------------------------------------------------*
002100040115     c     kcfp          klist
002200040115     c                   kfld                    cfpNTW
002300040115     c                   kfld                    cfpCAU
002400040115      *
002500040901     c                   eval      partner = 'FED'
002600040923      *
002700040923      *  imposta UDATE x confronto
002800040923     C                   move      *date         G02DAT
002900040923     C                   MOVE      *ZEROS        G02INV
003000040923     C                   MOVE      *BLANKS       G02ERR
003100040923     C                   CALL      'XSRDA8'
003200040923     C                   PARM                    WLBDAT
003300040923     C                   Z-ADD     G02INV        data_oggi         8 0
003400040923     C                   MOVE      data_oggi     day
003500040923      *
003600040901     C                   TIME                    W0140            14 0
003700040901     C                   MOVEl     W0140         ore
003800040901     c                   seton                                        66
003900030623      *---------------------------------------------------------------------*
004000030624     c     *start        setll     tnefe00F
004100030620     c                   do        *hival
004200030624     c                   read      tnefe00F
004300030624     c                   if        %eof(tnefe00F)
004400030620     C                   leave
004500030620     C                   Endif
004600030623      *
004700030623     c                   clear                   tnEFR000
004800030623      *
004900030624     c                   eval      EFRNTW = 'FED'
005000030624     c                   movel     efeAWB        EFRRIF
005100030623     c                   move      *all'0'       EFRNFT
005200030624     c                   if        efenrf <> 0
005300030624     c                   move      efenrf        EFRNFT
005400030624     c                   end
005500030624      *
005600030624     c                   movel     efedtf        EFRDFT
005700030623      *
005800040115      *  Prende il gruppo di apprtenenza x causale originale
005900040115     c                   exsr      TBE_CFP
006000040115     c                   movel     efeCAU        efrCAU
006100040115      *
006200030721      *  SOLO FATTURE
006300030721     c                   movel     'F'           efrTPD
006400030721      *
006500030624     c                   z-add     efeVAD        EFRIMP
006600030623      *  imposta UDATE
006700030624     C                   move      *date         G02DAT
006800030624     C                   MOVE      *ZEROS        G02INV
006900030624     C                   MOVE      *BLANKS       G02ERR
007000030624     C                   CALL      'XSRDA8'
007100030624     C                   PARM                    WLBDAT
007200030624     C                   Z-ADD     G02INV        efrDTR
007300030723      **********
007400030723      ***reperisce la spedizione bartolini da TITA433C
007500030723     c**********         movel     efeAWB        ta4RPT
007600030723     c*****ta4RPT        chain     tita433C
007700030723     c**********         if        %Found(tita433C)
007800030723     c**********         eval      EFRAAS = TA4AAS
007900030723     c**********         eval      EFRLNP = TA4LNP
008000030723     c**********         eval      EFRNRS = TA4NRS
008100030723     c**********         eval      EFRNSP = TA4NSP
008200030723     c**********         end
008300040901      *
008400040901      *   Se ci sono delle voci non classificate in tabella segnala in stampa
008500040901     c                   if        efrgrp = '999'
008600040901     c                   if        *in66
008700040901      *                 * ------------------ *
008800040901     c                   write     testa
008900040901      *                 * ------------------ *
009000040901     c                   setoff                                       66
009100040901     c                   end
009200040901     c                   write     dettagli
009300040901     c                   End
009400040901      *
009500030623      *
009600030623     c                   write     tnEFR000
009700030624     c                   delete    tnefe00F
009800030620      *
009900030620     C                   ENDdo
010000030620     c
010100030620     c                   seton                                        LR
010200030723      *----------------------------------------------------------------
010300040115      *  Decodifica il raggruppamento per la Causale Originale
010400040115      *----------------------------------------------------------------
010500040115     c     TBE_CFP       Begsr
010600040115      *
010700040115     c                   move      '999'         Gruppo            3
010800040115      *
010900040115     c                   movel     'FED'         cfpNTW
011000040115     c                   movel(P)  efeCAU        cfpCAU
011100040115     c     Kcfp          setll     tnCFP01l
011200040115      *
011300040115     C                   IF        %EQUAL
011400040115      *
011500040115     C                   DOU       %EOF
011600040115     C     Kcfp          Reade     tnCFP01l
011700040115     C                   IF        NOT %EOF and
011800040115     C                             cfpDDA <= Data_oggi and
011900040115     C                             cfpADA >= Data_oggi
012000040115     c                   move      cfpGRP        Gruppo
012100040115     c                   leave
012200040115     C                   ENDIF
012300040115     C                   ENDDO
012400040115      *
012500040115     c                   end
012600040115      *
012700040115     c                   movel     Gruppo        efrGRP
012800040115      *
012900040115     c                   Endsr
013000040115      *----------------------------------------------------------------
