000100000103     H DECEDIT('0,') DATEDIT(*DMY/)
000200000103      *                                                                     *****
000300000103     FFIDP300F  IP   E             DISK    rename(fidp3000:fidp3old)  prefix(O_)
000400000103     FFIDP301L  O  A E             DISK
000500000103     FFIDP202L  IF A E           K DISK
000600000103      *
000700000103     FFNBLP01L  IF   E           K DISK
000800000103     FAZORG05L  IF   E           K DISK
000900000103      *----------------------------------------------------*
001000000103     D Worgfil         S                   LIKE(ORGFIL)
001100000103      *-------------------
001200000103     D CNCR80        E DS
001300000103     D UT§DSE0F      E DS
001400000103     D OG143         E DS
001500000103      *----------------------------------------------------*
001600000103
001700000103     c                   clear                   FIDP3000
001800000103     c                   eval      DP3atb = O_DP3atb
001900000103     c                   eval      DP3aas = O_DP3aas
002000000103     c                   eval      DP3lnp = O_DP3lnp
002100000103     c                   eval      DP3nrs = O_DP3nrs
002200000103     c                   eval      DP3nsp = O_DP3nsp
002300000103     c                   eval      DP3fle = O_DP3fle
002400000103     c                   eval      DP3dev = O_DP3dev
002500000103     c                   eval      DP3hev = O_DP3hev
002600000103     c                   eval      DP3ftr = O_DP3ftr
002700000103     c                   eval      DP3dtr = O_DP3dtr
002800000103      *
002900000103     c                   SELECT
003000000103     c                   WHEN      O_DP3evd = '03'  and  O_DP3cev = *blanks
003100000103     c                   eval      DP3evd = '03F'
003200000103     c                   clear                   DP3cev
003300000103     c                   WHEN      O_DP3evd = '03'  and  O_DP3cev = 'CON'
003400000103     c                   eval      DP3evd = '03 '
003500000103     c                   clear                   DP3cev
003600000103     c                   OTHER
003700000103     c                   eval      DP3evd = O_DP3evd
003800000103     c                   eval      DP3cev = O_DP3cev
003900000103     c                   ENDSL
004000000103      *
004100000103     c                   IF        Worgfil = dp3lnp
004200000103     c     Kspe          chain     FNBLP000
004300000103     c                   if        %found
004400000103     c                   move      BLPrmn        DP3prn
004500000103     c     DP3prn        chain     FIDP2000
004600000103     c                   if        %found
004700000103     c                   movel     DP2rtr        DP3rtr
004800000103     c                   endif
004900000103     c                   endif
005000000103     c                   ENDIF
005100000103
005200000103     c                   WRITE     FIDP3000
005300000103      *----------------------------------------------------*
005400000103      *   DEFINIZIONE CHIAVI                               *
005500000103      *----------------------------------------------------*
005600000103     C     *INZSR        BEGSR
005700000103      *
005800000103     C     KSPE          KLIST
005900000103     C                   KFLD                    dp3AAS
006000000103     C                   KFLD                    dp3LNP
006100000103     C                   KFLD                    dp3NRS
006200000103     C                   KFLD                    dp3NSP
006300000103      *
006400000103      * Dati società
006500000103     C                   Z-ADD     1             CODUT
006600000103     C                   CALL      'X§PARUT'
006700000103     C                   PARM                    UT§DSE0F
006800000103      *
006900000103     C                   MOVEL     REC80         CNCR80
007000000103      *
007100000103      * Determino il P.O. DPD del mio AS400
007200000103     c     *loval        setll     azorg
007300000103     c                   read      azorg
007400000103      *
007500000103     C                   DOW       NOT %eof
007600000103     C                   movel     ORGDE3        OG143
007700000103     c                   If        §OGdpd = 'S'  and  SIMFEL = ORGfel
007800000103     C                   movel     ORGFIL        WORGFIL
007900000103     c                   Endif
008000000103     c                   read      azorg
008100000103     c                   ENDDO
008200000103      *
008300000103     C                   ENDSR
008400000103      *-------------------------------------------------------------------------
