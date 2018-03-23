000100950130     H* FNLSB2R *-----------------------------------------------------*
000200930113     H*      PROGRAMMA STAMPA QUADRATURA SPUNTE I.M.P. (BATCH)        *
000300930113     H*---------------------------------------------------------------*
000400930113     H DECEDIT('0,') DATEDIT(*DMY.)
000500051021     FFNFVV02L  IF   E           K DISK
000600000204     FFIBRV01L  IF   E           K DISK
000700950131     FFNBLP01L  IF   E           K DISK
000800950131     FFNBLT27L  IF   E           K DISK
000900051021     FFNart01L  IF   E           K DISK
001000051021     FFNblpwrk  IF a E           K DISK    rename(fnblp000:fnblp00w)
001100930119     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200930114     D*
001300051021     D posic           S              3  0 DIM(7)
001400930113     D*
001500930113     D*
001600020813     D KPJBA         E DS
001700021203
001800930113     C*---------------------------------------------------------------*
001900051024     c     *entry        plist
002000051024     c                   parm                    dataAAMMGG        8
002100930113     C*
002200051021     c                   eval      posic(1)=30
002300051021     c                   eval      posic(2)=31
002400051021     c                   eval      posic(3)=34
002500051021     c                   eval      posic(4)=35
002600051021     c                   eval      posic(5)=38
002700051021     c                   eval      posic(6)=32
002800051021     c                   eval      posic(7)=37
002900051021     c
003000051021     c     kfvv          Klist
003100051021     c                   kfld                    knpg              1 0
003200051021     c                   kfld                    kdfv              8 0
003300051021     C* LETTURA DI FIBRV01L
003400051021     C     KBRV          KLIST
003500051021     C                   KFLD                    fvvnpg
003600051021     C                   KFLD                    knfv              6 0
003700051021     C                   KFLD                    fvvfgs
003800051021     C* LETTURA DI FNBLP01L
003900051021     C     KBLP          KLIST
004000051021     C                   KFLD                    BLTAAS
004100051021     C                   KFLD                    BLTLNP
004200051021     C                   KFLD                    BLTNRS
004300051021     C                   KFLD                    BLTNSP
004400051021     C     Kart          KLIST
004500051021     C                   KFLD                    BLpAAS
004600051021     C                   KFLD                    BLpLNP
004700051021     C                   KFLD                    BLpNRS
004800051021     C                   KFLD                    BLpNSP
004900051021     C     KBLT          KLIST
005000051021     C                   KFLD                    brvLNP
005100051021     C                   KFLD                    brvLNA
005200051021     C                   KFLD                    brvNRS
005300051021     C                   KFLD                    brvNSC
005400051021     c
005500051021     c                   z-add     3             knpg
005600051024     c                   movel     dataAAMMGG    kdfv
005700051021     C     Kfvv          SETLL     Fnfvv02l
005800051021     C     Knpg          reade     Fnfvv02l
005900051021     c                   dow       not %eof(fnfvv02l)
006000051021     c
006100051021     c                   if        fvvspg='P'
006200051021     c     fvvfgs        lookup    posic                                  32
006300051021     c                   if        not *in32
006400051021     c
006500051021     c
006600051021     c                   z-add     fvvnfv        knfv
006700051021     C     KBRV          SETLL     FIBRV01L
006800051021     C     KBRV          READE     FIBRV01L                               31
006900051021     c                   dow       not %eof(fibrv01l)
007000051021     c     brvlna        lookup    posic                                  32
007100051021     c                   if        *in32
007200051021     c
007300051021     c     kblt          chain     fnblt27l                           30
007400051021     c  n30kblp          chain     fnblp01l                           30
007500051021     c* Scrivo spedizione a terra
007600051021     c                   write     fnblp00W                             30
007700051021     c                   endif
007800051021     c
007900051021     C     KBRV          READE     FIBRV01L                               31
008000051021     c                   enddo
008100051021     c
008200051021     c                   endif
008300051021     c                   endif
008400051021     c
008500051021     C     Knpg          reade     Fnfvv02l
008600051021     c                   enddo
008700051021     c
008800051021     c* leggo le bolle in sospensione o con duc=0 senza data di arrivo
008900051021     c     *loval        setll     fnblp01l
009000051021     c                   read      fnblp01l
009100051021    1c                   dow       not %eof(fnblp01l)
009200051021     C
009300051021    2c                   if        blpduc=0
009400051021    2c                   if        blpCTM<>'4 ' AND BLPCTM<>'4F'
009500051021     c     blplna        lookup    posic                                  32
009600051021    3c                   if        *in32
009700051021     c* Verifico che non ci sia la data di arrivo
009800051021     c     kart          setll     fnart01l
009900051021     c     kart          reade     fnart01l
010000051021    4c                   if        %eof(fnart01l)
010100051021     c                   write     fnblp00W                             30
010200051021   x4c                   else
010300051021     c
010400051021    5c                   dow       not %eof(fnart01l)  and artdam>0
010500051021     c     kart          reade     fnart01l
010600051021    5c                   enddo
010700051021     c
010800051021    5c                   if        not %eof(fnart01l)
010900051021     c                   write     fnblp00W                             30
011000051021    5c                   endif
011100051021    4c                   endif
011200051021    4c                   endif
011300051021    3c                   endif
011400051021    2c                   endif
011500051021     c
011600051021     c                   read      fnblp01l
011700051021    1c                   enddo
011800051021     c
011900930113     C*
012000930113     C                   SETON                                            LR
