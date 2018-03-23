000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  UF   E             DISK
000500070523     Ftivawwwt  O    E             DISK
000600030113     FTITAS30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200130212     D wrkCASnum       s             12  2
001300130212     D wrkCASalf       s             13
001400070523
001500070523     D
001600070523     D DS_Out          ds
001700070523     D  cnt01                        10    inz(*all'x')
001800070523     D  fil01                         3    inz(*blanks)
001900070523     D  sh_data                       8    inz(*zeros)
002000070523     D  nome                         40    inz(*blanks)
002100070523     D  nrdoc                        10    inz(*blanks)
002200070523     D  fil02                         6    inz(*blanks)
002300090318     D  cod                           3    inz(*all'x')
002400070523     D  amount                       12    inz(*blanks)
002500070523     D  fil03                         2    inz(*blanks)
002600070523     D  cur_data                      8    inz(*all'x')
002700070523     D  waehr                         3    inz(*all'x')
002800070523     D  fil04                         5    inz(*blanks)
002900130212     D  amount_eur                   13    inz(*blanks)
003000070523
003100070523     D*------------------
003200070523     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300070523     D*------------------
003400070523     D WLBDA8          DS                  INZ
003500070523     D  G08DAT                 1      8  0
003600070523     D  G08INV                 9     16  0
003700070523     D  G08ERR                17     17
003800070523     D  G08TGI                18     22  0
003900070523
004000991027
004100991027     C                   EXSR      traduci
004200921023     C                   SETON                                        LR
004300991027
004400991027     C     traduci       BEGSR
004500991027     C*
004600090122     C                   READ      tivgdtmp
004700090122     C                   DOW       not %eof(tivgdtmp)
004800090122     C                   movel     vgddta        tnvasds
004900020508     C*
005000020508     C* Innanzitutto inizializzo il formato record del file tradotto
005100070523     C                   CLEAR                   TIVAW000
005200030113     C*
005300030113     C* Poi "chaino" la bolla x reperire alcune informazioni mancanti nel file dei contrassegni
005400030113     C     KEYVAS        CHAIN     TITAS30C
005500030113     C                   IF        %found(TITAS30C)
005600070523     C*
005700070523     C                   MOVEL     tasAAS        sh_data
005800070523     C                   MOVE      tasMGS        sh_data
005900070523     C                   EVAL      nome  = tasRSD
006000070523     C                   EVAL      nrdoc = vasRMA
006100070523     C*
006200070523     C                   EVAL      wrkCASnum=vasCAS
006300090318     C                   EVAL      wrkCASalf=
006400130212     C                             %trim(%editw(wrkCASnum:'0          ,  '))
006500070523     C                   MOVE      wrkCASalf     amount
006600070523     C                   MOVE      wrkCASalf     amount_eur
006700070523     C*
006800070523     C                   MOVE      datcor        cur_data
006900090212     C                   EVAL      vawDTA = DS_Out
007000020508     C*
007100070523     C                   WRITE     TIVAW000
007200090122     C                   DELETE    tivgd000
007300030113     C                   ENDIF
007400020508     C*
007500090122     C                   READ      tivgdtmp
007600030113     C                   ENDDO
007700991027     C*
007800030113     C                   EVAL      wrkesito = '0'
007900991027     C*
008000910830     C                   ENDSR
008100020508
008200020508
008300020508
008400991027
008500991027      /TITLE Operazioni iniziali.
008600991027     C     *inzsr        BEGSR
008700991027     C*
008800991027     C     *ENTRY        PLIST
008900030113     C                   PARM                    prmppt
009000030113     C     wrkesito      PARM      wrkesito      prmesito
009100070523     C*
009200070523     C* CALCOLA LA DATA CORRENTE
009300070523     C                   time                    wn14             14 0
009400070523     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
009500070523     C                   z-add     wn8           g08dat
009600070523     C                   z-add     *zeros        g08inv
009700070523     C                   movel     '0'           g08err
009800070523     C                   call      'XSRDA8'
009900070523     C                   parm                    wlbda8
010000070523     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
010100030113     C*
010200030113     C* Definizione chiavi di lettura
010300030113     C*
010400030113     C* - TITAS30C: PARZIALE
010500030113     C     KEYVAS        KLIST
010600030113     C                   KFLD                    vasAAS
010700030113     C                   KFLD                    vasLNP
010800030113     C                   KFLD                    vasNRS
010900030113     C                   KFLD                    vasNSP
011000991027     C*
011100991027     C                   ENDSR
