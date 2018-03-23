000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300170915     FDPcdp12i  iF   E           K DISK
000400171020     FTIDP200F  uf   E             DISK
000500171020      ** ---------------------------------------------------------*
000600171020     D DATEU           S              8  0
000700171020      ** ---------------------------------------------------------*
000800171020     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
000900171020      ** -------------------------------------------------------- *
001000171020     c     Kcdp4         klist
001100171020     c                   kfld                    CDPATB
001200171020     c                   kfld                    CDPVER
001300171020     c                   kfld                    CDPDSTR
001400170929     C*------------------------------------------------------------------------*
001500170929     c                   seton                                        LR
001600171020     c                   eval      dateu = *date
001700170929     C*------------------------------------------------------------------------*
001800171020      ** Depot da scrivere
001900171020     c                   read      tiDP200f
002000171020     c                   dow       not %EoF(tiDP200f)
002100170929      **
002200171020      **
002300171020     c                   if        %subst(i2depNAME:6:1) = ' '
002400171020      * trascodifico la B.U.
002500171020      * recupero  versione di cappario
002600171020     C                   clear                   ISID1DRI
002700171020     c                   z-add     dateu         ISID1DRI
002800171020     C                   clear                   OSID1VER
002900171020     C                   clear                   OSID1VERD
003000171020     C                   clear                   OSID1DDE
003100171020     C                   clear                   OSID1DSC
003200171020     C                   clear                   OSID1BCID
003300171020     c                   call      'TISID1R'
003400171020     C                   PARM                    ISID1DRI
003500171020     C                   PARM                    OSID1VER
003600171020     C                   PARM                    OSID1VERD
003700171020     C                   PARM                    OSID1DDE
003800171020     C                   PARM                    OSID1DSC
003900171020     C                   PARM                    OSID1BCID
004000171020      * se TROVATA VERSIONE CAPPARIO
004100171020      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
004200171020     c                   if        osid1ver > 0
004300171020     c                   move      *blank        CDPATB
004400171020     c                   z-add     OSID1VER      CDPVER
004500171020     c                   movel     i2depcode     CDPDSTR
004600171020     c     Kcdp4         chain     dpcdp12i
004700171020      *   il Depot con la B.U.
004800171020     c                   if        %Found(dpcdp12i)
004900171020     c                   eval      i2depNAME = CDPDPTN
005000171020     c                   end
005100171020     c                   end
005200171020      ** ------
005300171020     c                   endif
005400171020      ** ------
005500171020     c                   update    tiDP2000
005600171020     c                   read      tiDP200f
005700170929      **
005800170929     c                   enddo
005900170915      **
006000170929     C                   SETON                                        LR
006100170929     c                   return
006200170929     C*------------------------------------------------------------------------*
