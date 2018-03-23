000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000125     H*
000400100908     FTITAS30C  IF   E           K DISK
000500100908     FTITAA30C  IF   E           K DISK
000600100908     Ftivgdtmp  uF   E             DISK
000700100908     FTIVAWWWT  O    E             DISK    commit
000800000125     F*
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090325     D tivafds       e ds                  extname(tivaf00t)
001300100908     D TIVAFWWT1     e ds
001400100908     D*------------------
001500100908     D* DS REPERIMENTO DATI CLIENTE
001600100908     D*-------------------
001700100908     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001800100908     D ACODS         E DS                  EXTNAME(CNACO00F)
001900100908     D INDDS         E DS                  EXTNAME(CNIND00F)
002000100908     D CLPDS         E DS                  EXTNAME(CNCLP00F)
002100100908     D CLSDS         E DS                  EXTNAME(FNCLS00F)
002200000904
002300100908     D*-------------------
002400100908     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
002500100908     D*-------------------
002600100908     D TxtInOut        S           2048
002700100908     D ElencoChar      S            256
002800100908     D TipoElenco      S              1
002900100908     D CharSost        S              1
003000100908     D UpperCase       S              1
003100100908     D ChkNull         S              1
003200100908     D CharNull        S              1
003300100908     D Esito           S              1
003400100908
003500100908
003600000904     C*
003700991027     C                   EXSR      traduci
003800921023     C                   SETON                                        LR
003900000125     C*
004000020611
004100991027     C     traduci       BEGSR
004200991027     C*
004300090325     C                   READ      tivgdtmp
004400020611     C                   DOW       not %eof
004500090325     c                   movel     vgddta        tivafds
004600000904     C*
004700000125     C                   MOVE      VAFAAS        VHFAAS
004800000125     C                   MOVE      VAFLNP        VHFLNP
004900000125     C                   MOVE      VAFNRS        VHFNRS
005000000125     C                   MOVE      VAFNSP        VHFNSP
005100000125     C                   MOVE      VAFTBL        VHFTBL
005200000125     C                   MOVE      VAFMGS        VHFMGS
005300000125     C                   MOVE      VAFKSC        VHFKSC
005400000125     C                   MOVE      VAFLNA        VHFLNA
005500000125     C                   MOVE      VAFNCL        VHFNCL
005600020611     C                   MOVE      VAFPKB        VHFPKB
005700020611     C                   MOVE      VAFVLF        VHFVLF
005800020611     C                   MOVE      VAFQFT        VHFQFT
005900020611     C                   MOVE      VAFPOR        VHFPOR
006000000125     C                   MOVE      VAFSV1        VHFSV1
006100020611     C                   MOVE      VAFVA1        VHFVA1
006200000125     C                   MOVE      VAFSV2        VHFSV2
006300020611     C                   MOVE      VAFVA2        VHFVA2
006400000125     C                   MOVE      VAFSV3        VHFSV3
006500020611     C                   MOVE      VAFVA3        VHFVA3
006600000125     C                   MOVE      VAFSV4        VHFSV4
006700020611     C                   MOVE      VAFVA4        VHFVA4
006800000125     C                   MOVE      VAFSV5        VHFSV5
006900020611     C                   MOVE      VAFVA5        VHFVA5
007000000125     C                   MOVE      VAFSV6        VHFSV6
007100020611     C                   MOVE      VAFVA6        VHFVA6
007200000125     C                   MOVE      VAFSV7        VHFSV7
007300020611     C                   MOVE      VAFVA7        VHFVA7
007400000125     C                   MOVE      VAFSV8        VHFSV8
007500020611     C                   MOVE      VAFVA8        VHFVA8
007600020611     C                   MOVE      VAFVAX        VHFVAX
007700020611     C                   MOVE      VAFIMV        VHFIMV
007800000125     C                   MOVE      VAFDFT        VHFDFT
007900000125     C                   MOVE      VAFNFT        VHFNFT
008000000125     C                   MOVE      VAFFIV        VHFFIV
008100000125     C                   MOVE      VAFDIV        VHFDIV
008200000125     C                   MOVE      VAFDRT        VHFDRT
008300000125     C                   MOVE      VAFRMN        VHFRMN
008400000125     C                   MOVE      VAFRMA        VHFRMA
008500000125     C                   MOVE      VAFRMX        VHFRMX
008600000125     C                   MOVE      VAFFTC        VHFFTC
008700000125     C                   MOVE      VAFTC2        VHFTC2
008800000125     C                   MOVE      VAFTSP        VHFTSP
008900000125     C                   MOVE      VAFFAP        VHFFAP
009000000125     C                   MOVE      VAFFIN        VHFFIN
009100000125     C                   MOVE      VAFCTR        VHFCTR
009200000125     C                   MOVE      VAFNAS        VHFNAS
009300000125     C                   MOVE      VAFCTS        VHFCTS
009400000125     C                   MOVE      VAFCAD        VHFCAD
009500000125     C                   MOVE      VAFLOD        VHFLOD
009600000125     C                   MOVE      VAFPRD        VHFPRD
009700000125     C                   MOVE      VAFNZD        VHFNZD
009800000125     C                   MOVE      VAFVAS        VHFVAS
009900020611     C                   MOVE      VAFIAS        VHFIAS
010000000125     C                   MOVE      VAFFPC        VHFFPC
010100020611     C                   MOVE      VAFPKC        VHFPKC
010200000125     C                   MOVE      VAFFVC        VHFFVC
010300020611     C                   MOVE      VAFVLC        VHFVLC
010400930409     C*
010500100908     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
010600100908     C     KEYtas30C     CHAIN     titas30c
010700100908     C                   IF        %found(titas30c)
010800100908     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
010900100908     C                   EVAL      VHFRSD = TASRSD
011000100908     C                   EVAL      VHFIND = TASIND
011100100908     C* Peso tassabile
011200100908     C                   MOVE      TASPVL        VHFPVL
011300100908     C* X reperire i dati del mittente verifico il codice del cliente mittente:
011400100908     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
011500100908     C                   MOVE(P)   tasCCM        wCLI              4 0
011600100908     C                   IF        wCLI =    0 OR
011700100908     C                             wCLI = 8888 OR
011800100908     C                             wCLI = 9999
011900100908     C                   EVAL      taaTRC = 'M'
012000100908     C     KEYtaa30C     CHAIN     titaa30c
012100100908     C                   IF        %found(titaa30c)
012200100908     C                   EVAL      VHFRSM = TAARSC
012300100908     C                   EVAL      VHFINM = TAAIND
012400100908     C                   EVAL      VHFCAM = TAACAP
012500100908     C                   EVAL      VHFLOM = TAALOC
012600100908     C                   EVAL      VHFPRM = TAAPRV
012700100908     C                   EVAL      VHFNZM = TAANAZ
012800100908     C                   ENDIF
012900100908     C*
013000100908     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
013100100908     C                   ELSE
013200100908     C                   CLEAR                   BS69DS
013300100908     C                   CLEAR                   ACODS
013400100908     C                   CLEAR                   INDDS
013500100908     C                   CLEAR                   CLPDS
013600100908     C                   CLEAR                   CLSDS
013700100908     C                   Z-ADD     tasCCM        I69KAC
013800100908     C                   Z-ADD     tasCCM        I69KIN
013900100908     C                   CALL      'TIBS69R'
014000100908     C                   PARM                    BS69DS
014100100908     C                   PARM                    ACODS
014200100908     C                   PARM                    INDDS
014300100908     C                   PARM                    CLPDS
014400100908     C                   PARM                    CLSDS
014500100908     C     O69ERR        IFNE      '1'
014600100908     C                   EVAL      VHFRSM = ACORAG
014700100908     C                   EVAL      VHFINM = INDVIA
014800100908     C                   MOVEL(P)  INDCAP        VHFCAM
014900100908     C                   EVAL      VHFLOM = INDCIT
015000100908     C                   EVAL      VHFPRM = INDPRV
015100100908     C                   EVAL      VHFNZM = INDSTA
015200100908     C                   ENDIF
015300100908     C*
015400100908     C                   ENDIF
015500100908     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
015600100908     C                   EVAL      taaTRC = 'O'
015700100908     C     KEYtaa30C     CHAIN     titaa30c
015800100908     C                   IF        %found(titaa30c)
015900100908     C                   EVAL      VHFRMO = TAARSC
016000100908     C                   EVAL      VHFCMO = TAACAP
016100100908     C                   EVAL      VHFNMO = TAANAZ
016200100908     C                   ENDIF
016300100908     C                   ENDIF
016400100908     C*
016500100908     C                   EVAL      VHFNUL = '.'
016600100908     C*
016700170505     C                   EVAL      vawDTA = TIVAFWWT1
016800100908     C*
016900100908     C                   WRITE     TIVAW000
017000090325     C                   delete    tivgd000
017100000904     C*
017200090325     C                   READ      tivgdtmp
017300020611     C                   ENDDO
017400991027     C*
017500020611     C                   EVAL      wrkesito = '0'
017600991027     C*
017700910830     C                   ENDSR
017800000125     C*
017900170505
018000170505
018100170505
018200991027      /TITLE Operazioni iniziali.
018300991027     C     *inzsr        BEGSR
018400991027     C*
018500991027     C     *ENTRY        PLIST
018600991027     C                   parm                    prmppt
018700991027     C     wrkesito      parm      wrkesito      prmesito
018800100908     C*
018900100908     C* Definizione chiavi
019000100908     C*
019100100908     C* Chiave su TITAS30C - Completa
019200100908     C     KEYtas30C     KLIST
019300100908     C                   KFLD                    vafAAS
019400100908     C                   KFLD                    vafLNP
019500100908     C                   KFLD                    vafNRS
019600100908     C                   KFLD                    vafNSP
019700100908     C                   KFLD                    vafTBL
019800100908     C*
019900100908     C* Chiave su TITAA30C - Completa
020000100908     C     KEYtaa30C     KLIST
020100100908     C                   KFLD                    vafAAS
020200100908     C                   KFLD                    vafLNP
020300100908     C                   KFLD                    vafNRS
020400100908     C                   KFLD                    vafNSP
020500100908     C                   KFLD                    taaTRC
020600991027     C*
020700991027     C                   ENDSR
