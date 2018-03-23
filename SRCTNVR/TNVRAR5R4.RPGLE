000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000200950130     F* FNLV52R *-----------------------------------------------------*
000300950130     F*       AGGIORNAMENTI VARI SULLE BOLLE
000400950130     F*---------------------------------------------------------------*
000500040416     FFNBTP01L  iF   E           K DISK
000600040416     Ftabel00f  iF   E           K DISK
000700030108     fFidta01l  uf a e           K Disk
000800030108
000900030108     d SavPoc          s                   Like(DtaPoc)
001000030108     d SavTrd          s                   Like(DtaTrd)
001100030109     d wFidta          s              1    Inz('0')
001200030320     d Cod             s                   Like(TblCod)
001300030320     d Key             s                   Like(TblKey)
001400030320     d ds3a          e ds
001500950130     I*----------------------------------------------------------------
001600040416     c                   exsr      aggbtp
001700950130     C                   SETON                                        LR
001800040416     C*
001900950130     C     AGGBTP        BEGSR
002000950130     C*
002100040416     C     *loval        SETLL     FNBTP01L
002200040416     C                   READ      FNBTP01L                               34
002300040416    1c                   dow       not *in34
002400040401     c
002500030320      * Se è un tipo bolla da trasmettere in sede
002600030320     c                   Clear                   ds3a
002700030320     c                   Z-Add     1             Codut
002800030320     c                   Eval      Cod = '3A'
002900030320     c                   Clear                   Key
003000030320     c                   Movel     BtpCbo        Key
003100030320     c     kTab          Chain     Tabel00f
003200040416    2c                   If        %Found(Tabel00f)
003300030320     c                   Movel     TblUni        ds3a
003400040416    2c                   EndIf
003500030320If  2c                   If        §3aBsd = *Blanks
003600040401     c
003700040415      * Scrivo/Aggiorno Fidta
003800040415     c                   Eval      SavPoc = BtpFlp
003900040415     c
004000040401      * DUT
004100040401     c
004200040422    3c                   if        btpdut>=20040415
004300030109     c                   Eval      wFidta = '0'
004400030108     c                   Eval      SavTrd = 'DUT'
004500030109     c     kFidta        Setll     Fidta01l
004600040416    4c                   Do        *Hival
004700030109     c     kFidta        Reade     Fidta01l
004800030109      * Fine file
004900040416    5c                   If        %Eof(Fidta01l)
005000030109     c                   Leave
005100040416    5c                   EndIf
005200030109      * Già trasmesso
005300040416    5c                   If        DtaFtr <> *Blanks
005400030109     c                   Iter
005500040416    5c                   EndIf
005600030109      * Aggiorno
005700030109     c                   Eval      DtaDta = BtpDut
005800030108     c                   Update    Fidta000
005900030109     c                   Eval      wFidta = '1'
006000040416    4c                   EndDo
006100030109      * Scrivo
006200040416    4c                   If        wFidta = '0'
006300030108     c                   Clear                   Fidta000
006400030108     c                   Eval      DtaAas = BtpAas
006500030108     c                   Eval      DtaLnp = BtpLnp
006600030108     c                   Eval      DtaNrs = BtpNrs
006700030108     c                   Eval      DtaNsp = BtpNsp
006800030108     c                   Eval      DtaTrd = SavTrd
006900030108     c                   Eval      DtaPoc = SavPoc
007000030109     c                   Eval      DtaDta = BtpDut
007100030108     c                   Write     Fidta000
007200040416    4c                   EndIf
007300040416    3c                   EndIf
007400030320    2c                   EndIf
007500031127     c
007600040416     C                   READ      FNBTP01L                               34
007700040416    1c                   enddo
007800950130     C*
007900950130     C                   ENDSR
008000000000     C**************************************************************************
008100000000     C* R O U T I N E    I N I Z I A L E
008200000000     C**************************************************************************
008300000000     C     *INZSR        BEGSR
008400030108     c     kFidta        Klist
008500040416     c                   Kfld                    btpAas
008600040416     c                   Kfld                    btpLnp
008700040416     c                   Kfld                    btpNrs
008800040416     c                   Kfld                    btpnsp
008900030108     c                   Kfld                    SavTrd
009000030108     c                   Kfld                    SavPoc
009100030320
009200030320     c     kTab          Klist
009300040416     c                   Kfld                    Codut             1 0
009400030320     c                   Kfld                    Cod
009500030320     c                   Kfld                    Key
009600000000     C*
009700000000     C                   ENDSR
