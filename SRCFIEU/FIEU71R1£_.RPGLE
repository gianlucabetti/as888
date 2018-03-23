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
002600040901     C                   TIME                    W0140            14 0
002700040901     C                   MOVEl     W0140         ore
002800040901     C                   MOVE      W0140         day
002900040901     c                   seton                                        66
003000030623      *---------------------------------------------------------------------*
003100030624     c     *start        setll     tnefe00F
003200030620     c                   do        *hival
003300030624     c                   read      tnefe00F
003400030624     c                   if        %eof(tnefe00F)
003500030620     C                   leave
003600030620     C                   Endif
003700030623      *
003800030623     c                   clear                   tnEFR000
003900030623      *
004000030624     c                   eval      EFRNTW = 'FED'
004100030624     c                   movel     efeAWB        EFRRIF
004200030623     c                   move      *all'0'       EFRNFT
004300030624     c                   if        efenrf <> 0
004400030624     c                   move      efenrf        EFRNFT
004500030624     c                   end
004600030624      *
004700030624     c                   movel     efedtf        EFRDFT
004800030623      *
004900040115      *  Prende il gruppo di apprtenenza x causale originale
005000040115     c                   exsr      TBE_CFP
005100040115     c                   movel     efeCAU        efrCAU
005200040115      *
005300030721      *  SOLO FATTURE
005400030721     c                   movel     'F'           efrTPD
005500030721      *
005600030624     c                   z-add     efeVAD        EFRIMP
005700030623      *  imposta UDATE
005800030624     C                   move      *date         G02DAT
005900030624     C                   MOVE      *ZEROS        G02INV
006000030624     C                   MOVE      *BLANKS       G02ERR
006100030624     C                   CALL      'XSRDA8'
006200030624     C                   PARM                    WLBDAT
006300030624     C                   Z-ADD     G02INV        efrDTR
006400030723      **********
006500030723      ***reperisce la spedizione bartolini da TITA433C
006600030723     c**********         movel     efeAWB        ta4RPT
006700030723     c*****ta4RPT        chain     tita433C
006800030723     c**********         if        %Found(tita433C)
006900030723     c**********         eval      EFRAAS = TA4AAS
007000030723     c**********         eval      EFRLNP = TA4LNP
007100030723     c**********         eval      EFRNRS = TA4NRS
007200030723     c**********         eval      EFRNSP = TA4NSP
007300030723     c**********         end
007400040901      *
007500040901      *   Se ci sono delle voci non classificate in tabella segnala in stampa
007600040901     c                   if        efrgrp = '999'
007700040901     c                   if        *in66
007800040901      *                 * ------------------ *
007900040901     c                   write     testa
008000040901      *                 * ------------------ *
008100040901     c                   setoff                                       66
008200040901     c                   end
008300040901     c                   write     dettagli
008400040901     c                   End
008500040901      *
008600030623      *
008700030623     c                   write     tnEFR000
008800030624     c                   delete    tnefe00F
008900030620      *
009000030620     C                   ENDdo
009100030620     c
009200030620     c                   seton                                        LR
009300030723      *----------------------------------------------------------------
009400040115      *  Decodifica il raggruppamento per la Causale Originale
009500040115      *----------------------------------------------------------------
009600040115     c     TBE_CFP       Begsr
009700040115      *
009800040115     c                   move      '999'         Gruppo            3
009900040115      *  imposta UDATE x confronto
010000040115     C                   move      *date         G02DAT
010100040115     C                   MOVE      *ZEROS        G02INV
010200040115     C                   MOVE      *BLANKS       G02ERR
010300040115     C                   CALL      'XSRDA8'
010400040115     C                   PARM                    WLBDAT
010500040115     C                   Z-ADD     G02INV        data_oggi         8 0
010600040115      *
010700040115     c                   movel     'FED'         cfpNTW
010800040115     c                   movel(P)  efeCAU        cfpCAU
010900040115     c     Kcfp          setll     tnCFP01l
011000040115      *
011100040115     C                   IF        %EQUAL
011200040115      *
011300040115     C                   DOU       %EOF
011400040115     C     Kcfp          Reade     tnCFP01l
011500040115     C                   IF        NOT %EOF and
011600040115     C                             cfpDDA <= Data_oggi and
011700040115     C                             cfpADA >= Data_oggi
011800040115     c                   move      cfpGRP        Gruppo
011900040115     c                   leave
012000040115     C                   ENDIF
012100040115     C                   ENDDO
012200040115      *
012300040115     c                   end
012400040115      *
012500040115     c                   movel     Gruppo        efrGRP
012600040115      *
012700040115     c                   Endsr
012800040115      *----------------------------------------------------------------
