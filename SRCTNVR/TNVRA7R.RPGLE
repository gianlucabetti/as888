000100001005     H DECEDIT('0,') DATEDIT(*DMY.)
000200990901     F*------------------------------------------------------------------------*
000300990901     F* DATA BASE
000400990901     F*------------------------------------------------------------------------*
000500011003     FTITAS30C  IF   E           K DISK
000600011003     FTNCSB03L  IF   E           K DISK    usropn
000700011003     F                                     rename(TNCSB000:TNCSB003)
000800011003     FTNCSB07L  IF   E           K DISK    usropn
000900011003     FAZORG01L  IF   E           K DISK
001000011003     FWKTCS00F  O  A E           K DISK    usropn
001100001005     F*------------------------------------------------------------------------*
001200001005      * Comandi
001300001005     F*------------------------------------------------------------------------*
001400011204     D KPJBA         E DS
001500001005     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
001600001005     D  MSGDS          S             80
001700001005     D  LENGH          S             15  5
001800011003     D WLBDA8          DS
001900011003     D  G02DAT                 1      8  0
002000011003     D  G02INV                 9     16  0
002100011003     D  G02ERR                17     17
002200011003     D  G02TGI                18     22  0
002300011003     D Wdata8          DS
002400011003     D  dadata                 1      8  0
002500011003     D  adata                  9     16  0
002600011003     D  GioLav                17     21  0
002700011204     D param           DS
002800011204     D  anno4                  1      4
002900011204     D  gg4                    5      8
003000011204     D  cli7                   9     15
003100001005      *------------------------------------------------------------------------*
003200001005      * per riconoscere da quale dei tre del combinato aggiornare
003300001005      *------------------------------------------------------------------------*
003400001005     ITITAS000      01
003500001005     ITITAS010      02
003600001005     ITITASP00      03
003700001005      *------------------------------------------------------------------------*
003800011204     C     *ENTRY        PLIST
003900011204     C                   PARM                    KPJBA
004000011204     C                   MOVEL     KPJBU         PARAM
004100011003     C*
004200011003     C                   if        Anno4 = *blank
004300011003     C                   move      *year         anno40            4 0
004400011003     C                   else
004500011003     C                   move      Anno4         anno40
004600011003     C                   end
004700011003     C*
004800011003     C                   if        Gg4 = *blank
004900011003     C                   z-add     1             gg40              4 0
005000011003     C                   else
005100011003     C                   move      gg4           gg40
005200011003     C                   end
005300011003     C*
005400011003     C                   if        cli7 = *blank
005500011003     C                   z-add     0             cli70             7 0
005600011003     C                   else
005700011003     C                   move      cli7          cli70
005800011003     C                   end
005900011003     C*
006000011003     C     Kspediz       klist
006100011003     C                   kfld                    CSBAAS
006200011003     C                   kfld                    CSBLNP
006300011003     C                   kfld                    CSBNRS
006400011003     C                   kfld                    CSBNSP
006500011003     C*
006600011003     C                   MOVEL     CMD(1)        MSGDS
006700011003     C                   Z-ADD     80            LENGH
006800011003     C                   CALL      'QCMDEXC'
006900011003     C                   PARM                    MSGDS
007000011003     C                   PARM                    LENGH
007100011003     C*
007200011003     C                   OPEN      WKTCS00F
007300011003     C*
007400011129     C                   setoff                                       66
007500011003     C                   if        cli70 >0
007600011129     C                   seton                                        66
007700011003     C                   end
007800011003     C*
007900011129     C   66              OPEN      TNCSB07L
008000011129     C  N66              OPEN      TNCSB03L
008100011003     C* ---------------------------------------------------------
008200011003     C*  ciclo principale di creazione file
008300011003     C* ---------------------------------------------------------
008400011129     C   66Cli70         SETLL     TNCSB07L
008500011129     C  N66Anno40        SETLL     TNCSB03L
008600011008      *
008700011129     C   66Cli70         READE     TNCSB07L
008800011129     C  N66Anno40        READE     TNCSB03L
008900001005      *
009000011008     C                   DOW       not %eof
009100011003      *
009200011008     C                   IF        CSBsta = 0 and CSBaas = Anno40
009300011008      *
009400011008     C                   clear                   WKTCS000
009500011003      *
009600011003     C                   eval      TCSAAS = CSBAAS
009700011003     C                   eval      TCSLNP = CSBLNP
009800011003     C                   eval      TCSNRS = CSBNRS
009900011003     C                   eval      TCSNSP = CSBNSP
010000011003     C                   eval      TCSMGS = CSBMGS
010100011003     C                   eval      TCSLNA = CSBLNA
010200011003     C                   eval      TCSFLE = CSBFLE
010300011003     C                   eval      TCSCDI = CSBCDI
010400011003     C                   eval      TCSNDP = CSBNDP
010500011003     C                   eval      TCSDDP = CSBDDP
010600011003     C                   eval      TCSTPA = CSBTPA
010700011003     C                   eval      TCSTPI = CSBTPI
010800011003     C                   eval      TCSCAS = CSBCAS
010900011003     C                   eval      TCSVCA = CSBVCA
011000011003     C                   eval      TCSDDC = CSBDDC
011100011003     C*
011200011003     C     Kspediz       Chain     TITAS30C
011300011003     C                   IF        %Found(TITAS30C)
011400011003     C                   eval      TCSDCM = TASDCM
011500011003     C                   End
011600011003      *
011700011129     C     TCSLNA        Chain     AZORG01L
011800011003     C                   IF        %Found(AZORG01L)
011900011003     C                   eval      TCSCAR = ORGCAR
012000011003     C                   move      ORGCRE        TCSCRE
012100011003     C                   eval      TCSDIS = ORGFL3
012200011003     C                   End
012300001005      *
012400011003     C                   eval      Giorni1 = 0
012500011003     C                   eval      Giorni2 = 0
012600011003      *
012700011003     C                   IF        TCSDDC > 0
012800011003     C                   eval      G02INV = TCSDDC
012900011003     C                   eval      G02DAT = 0
013000011003     C                   eval      G02ERR = '3'
013100011003     C                   CALL      'XSRDA8'
013200011003     C                   PARM                    WLBDA8
013300011003     C                   Z-add     G02TGI        Giorni1           7 0
013400011003     C                   End
013500011003      *
013600011003     C                   IF        TCSDCM > 0
013700011003     C                   eval      G02INV = TCSDCM
013800011003     C                   eval      G02DAT = 0
013900011003     C                   eval      G02ERR = '3'
014000011003     C                   CALL      'XSRDA8'
014100011003     C                   PARM                    WLBDA8
014200011003     C                   Z-add     G02TGI        Giorni2           7 0
014300011003     C                   End
014400011003      *
014500011003     C                   IF        TCSDDP > 0
014600011003     C                   eval      G02INV = TCSDDP
014700011003     C                   eval      G02DAT = 0
014800011003     C                   eval      G02ERR = '3'
014900011003     C                   CALL      'XSRDA8'
015000011003     C                   PARM                    WLBDA8
015100011003     C                   Z-add     G02TGI        Giorni3           7 0
015200011003     C                   End
015300011003      *
015400011003      *
015500011003     C                   IF        TCSDDC > 0 and TCSDCM > 0
015600011003      *
015700011003     C                   IF        Giorni1 < Giorni2
015800011003     C                   eval      dadata = TCSDDC
015900011003     C                   eval      adata  = TCSDCM
016000011003     C                   Else
016100011003     C                   eval      dadata = TCSDCM
016200011003     C                   eval      adata  = TCSDDC
016300011003     C                   End
016400011003     C                   CALL      'XSRLAV8'
016500011003     C                   PARM                    Wdata8
016600011003     C                   Z-add     GioLav        TCSDIF
016700011003     C                   sub       1             TCSDIF
016800011003      *
016900011003     C                   End
017000011003      *
017100011003     C                   IF        TCSDDC > 0 and TCSDDP > 0
017200011003      *
017300011003     C                   IF        Giorni1 < Giorni3
017400011003     C                   eval      dadata = TCSDDC
017500011003     C                   eval      adata  = TCSDDP
017600011003     C                   Else
017700011003     C                   eval      dadata = TCSDDP
017800011003     C                   eval      adata  = TCSDDC
017900011003     C                   End
018000011003     C                   CALL      'XSRLAV8'
018100011003     C                   PARM                    Wdata8
018200011003     C                   Z-add     GioLav        TCSDIP
018300011003     C                   sub       1             TCSDIP
018400011003      *
018500011003     C                   End
018600011003      *
018700011003      *  differenza in giorni fra consegna e data incasso
018800011003     C                   If        Tcsdif > gg40
018900011129     C                   if        *in66 = *on and tcsaas = anno40 or
019000011129     C                             *in66 = *off
019100011003     C                   WRITE     WKTCS000
019200011003     C                   End
019300011003     C                   END
019400011003     C                   END
019500011003      *
019600011129     C   66Cli70         READE     TNCSB07L
019700011129     C  N66Anno40        READE     TNCSB03L
019800001005     C                   ENDDO
019900000204     C*
020000000204     C                   SETON                                        LR
020100000203     C*------------------------------------------------------------------------*
020200001005** (CMD)
020300011003CLRPFM FILE(WKTCS00F)
