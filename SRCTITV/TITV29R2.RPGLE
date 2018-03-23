000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  UF   E             DISK
000500130226     Ftivawwwt  UF A E             DISK
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
004500130226     C*
004600130226     C                   READ      TIVAWWWT                               55
004700130226     C*
004800130226     C* Inserisco header
004900130226     C   55              EVAL      vawDTA = '"data inizio","'+
005000130226     C                                    %trim(%char(%dec(%date() : *ISO)))+'"'
005100130226     C   55              WRITE     TIVAW000
005200991027     C*
005300090122     C                   READ      tivgdtmp
005400090122     C                   DOW       not %eof(tivgdtmp)
005500090122     C                   movel     vgddta        tnvasds
005600020508     C*
005700020508     C* Innanzitutto inizializzo il formato record del file tradotto
005800070523     C                   CLEAR                   TIVAW000
005900030113     C*
006000030113     C* Poi "chaino" la bolla x reperire alcune informazioni mancanti nel file dei contrassegni
006100030113     C     KEYVAS        CHAIN     TITAS30C
006200030113     C                   IF        %found(TITAS30C)
006300070523     C*
006400070523     C                   MOVEL     tasAAS        sh_data
006500070523     C                   MOVE      tasMGS        sh_data
006600070523     C                   EVAL      nome  = tasRSD
006700070523     C                   EVAL      nrdoc = vasRMA
006800070523     C*
006900070523     C                   EVAL      wrkCASnum=vasCAS
007000090318     C                   EVAL      wrkCASalf=
007100130212     C                             %trim(%editw(wrkCASnum:'0          ,  '))
007200070523     C                   MOVE      wrkCASalf     amount
007300070523     C                   MOVE      wrkCASalf     amount_eur
007400070523     C*
007500070523     C                   MOVE      datcor        cur_data
007600090212     C                   EVAL      vawDTA = DS_Out
007700020508     C*
007800070523     C                   WRITE     TIVAW000
007900090122     C                   DELETE    tivgd000
008000030113     C                   ENDIF
008100020508     C*
008200090122     C                   READ      tivgdtmp
008300030113     C                   ENDDO
008400130226     C*
008500130226     C* Inserisco footer
008600130226     C   55              EVAL      vawDTA = '"data fine","'+
008700130226     C                                    %trim(%char(%dec(%date() : *ISO)))+'"'
008800130226     C   55              WRITE     TIVAW000
008900991027     C*
009000030113     C                   EVAL      wrkesito = '0'
009100991027     C*
009200910830     C                   ENDSR
009300020508
009400020508
009500020508
009600991027
009700991027      /TITLE Operazioni iniziali.
009800991027     C     *inzsr        BEGSR
009900991027     C*
010000991027     C     *ENTRY        PLIST
010100030113     C                   PARM                    prmppt
010200030113     C     wrkesito      PARM      wrkesito      prmesito
010300070523     C*
010400070523     C* CALCOLA LA DATA CORRENTE
010500070523     C                   time                    wn14             14 0
010600070523     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
010700070523     C                   z-add     wn8           g08dat
010800070523     C                   z-add     *zeros        g08inv
010900070523     C                   movel     '0'           g08err
011000070523     C                   call      'XSRDA8'
011100070523     C                   parm                    wlbda8
011200070523     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
011300030113     C*
011400030113     C* Definizione chiavi di lettura
011500030113     C*
011600030113     C* - TITAS30C: PARZIALE
011700030113     C     KEYVAS        KLIST
011800030113     C                   KFLD                    vasAAS
011900030113     C                   KFLD                    vasLNP
012000030113     C                   KFLD                    vasNRS
012100030113     C                   KFLD                    vasNSP
012200991027     C*
012300991027     C                   ENDSR
