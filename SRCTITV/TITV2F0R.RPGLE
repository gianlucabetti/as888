000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400100225     FTITAS30C  IF   E           K DISK
000500060410     FTIVGDTMP  UF   E             DISK
000600090323     Ftivawwwt  uF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200060504     D wfdscds       e ds                  extname(wfdsc00f)
001300110503     D TRUL28DS      E DS
001400110503     D wCAS            s             11
001500110502
001600110502
001700110502     D*****
001800110502     D*  Formato buffer OUTPUT
001900110502     D*****
002000110502     D  O_DS_CLIENTE   DS
002100110503     D  O_DataSped                    8    inz
002200110503     D  O_LetteraVett                10    inz
002300110503     D  O_RifMittente                10    inz
002400110503     D  O_RagSocMitt                 40    inz
002500110503     D  O_IndMitt                    35    inz
002600110503     D  O_LocMitt                    40    inz
002700110503     D  O_CapMitt                     5    inz
002800110503     D  O_ProvMitt                    2    inz
002900110503     D  O_RagSocDest                 40    inz
003000110503     D  O_IndDest                    35    inz
003100110503     D  O_LocDest                    40    inz
003200110503     D  O_CapDest                     5    inz
003300110503     D  O_ProvDest                    2    inz
003400110503     D  O_ContrDiv                    3    inz
003500110503     D  O_ContrImp                   11    inz
003600110503     D  O_TipoMerce                   1    inz('C')
003700110503     D  O_Flag_001                    1    inz
003800110502     D  O_TipoReso                    1    inz('R')
003900110502     D  O_DDT                         1    inz('D')
004000110503     D  O_TipoServ                    1    inz
004100110503     D  O_Porto                       1    inz('A')
004200110503     D  O_CodCliTNT                  11    inz
004300110503     D  O_CodGiro                     5    inz
004400110503     D  O_NumGiorn                    5    inz
004500110503     D  O_SegnacTNT                   4    inz
004600110503     D  O_NumCollo                   15    inz
004700110503     D  O_BarcodeBrt                 30    inz
004800110503     D  O_Flag_002                    1    inz
004900110502
005000110502
005100110502
005200991027
005300030908     C                   EXSR      chkRECFILMBR
005400991027     C                   EXSR      traduci
005500921023     C                   SETON                                        LR
005600110502
005700110502
005800030908
005900030908     C     chkRECFILMBR  BEGSR
006000030908     C*
006100060410     C                   READ      TIVAWWWT                               55
006200030908     C*
006300030908     C                   ENDSR
006400991027
006500110502
006600110502
006700991027     C     traduci       BEGSR
006800991027     C*
006900060410     C                   READ      TIVGDTMP
007000060410     C                   DOW       not %eof(TIVGDTMP)
007100060411     C*
007200060411     C* Valorizzo il buffer dal campo flat di inout
007300060411     C                   EVAL      wfdscds = vgdDTA
007400100225     C*
007500100225     C* Innanzitutto chaino la bolla direttamente su TITAS
007600110503     C                   z-add     *zeros        wDSP              8 0
007700110503     C                   movel     *blanks       wDSPa             8
007800100225     C     KEYtas30P     chain     titas30c
007900100225     C                   if        %found(titas30c)
008000110503     C                   eval      wDSP = tasAAS*10000+tasMGS
008100110503     C                   movel(p)  wDSP          wDSPa
008200100225     C                   endif
008300051007     C*
008400110503     C* Si assume la ',' come separatore decimale
008500110503     C                   setoff                                       60
008600110503     C*
008700051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
008800110603     C**N60              eval      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
008900110603     C** 60              eval      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
009000110603     C                   move(p)   DSCCAS        wCAS
009100110503     C*
009200110503     C* Valorizzo la DS cliente
009300110503     C                   reset                   O_DS_CLIENTE
009400110503     C                   eval      O_DataSped    = %subst(wDSPa:7:2) +
009500110503     C                                             %subst(wDSPa:5:2) +
009600110503     C                                             %subst(wDSPa:1:4)
009700110504     C                   evalr     O_LetteraVett =
009800110504     C                                %editc(dscLNP*10000000+dscNSP:'X')
009900110503     C                   eval      O_RifMittente = dscRMA
010000110503     C                   eval      O_RagSocMitt  = dscRSM
010100110503     C                   eval      O_IndMitt     = dscINM
010200110503     C                   eval      O_LocMitt     = dscLOM
010300110503     C                   eval      O_CapMitt     = dscCAM
010400110503     C                   eval      O_ProvMitt    = dscPRM
010500110503     C                   eval      O_RagSocDest  = dscRSD
010600110503     C                   eval      O_IndDest     = dscIND
010700110503     C                   eval      O_LocDest     = dscLOD
010800110503     C                   eval      O_CapDest     = dscCAD
010900110503     C                   eval      O_ProvDest    = dscPRD
011000110503     C                   eval      O_ContrDiv    = dscVCA
011100110503     C                   evalr     O_ContrImp    = wCAS
011200110503     C*
011300110503     C                   select
011400110503     C                   when      dscTSP = 'C'
011500110503     C                   eval      O_TipoServ    = 'A'
011600110503     C                   when      dscTSP = 'E'
011700110503     C                   eval      O_TipoServ    = 'T'
011800110503     C                   when      dscTSP = 'H'
011900110503     C                   eval      O_TipoServ    = 'D'
012000110503     C                   other
012100110503     C                   eval      O_TipoServ    = 'A'
012200110503     C                   endsl
012300110503     C*
012400110603     C**                 evalr     O_NumCollo    = %editc(dscNCD:'X')
012500110603     C                   eval      O_NumCollo    = %editc(dscNCD:'X')
012600110503     C*
012700110503     C* Compongo il barcode completo Bartolini
012800110503     C                   move(p)   dscFLS        wFLS              3
012900110503     C                   move(p)   dscLNA        wLNA              3
013000110503     C                   move(p)   dscNRS        wNRS              2
013100110503     C                   move(p)   dscNCD        wNCD              7
013200110503     C                   move(p)   dscZNC        wZNC              2
013300110503     C*
013400110503     C                   clear                   TRUL28DS
013500110503     C*
013600110503     C* Valorizzo il buffer d input
013700110503     C                   eval      I28TLA = 'E'
013800110503     C                   eval      I28MOD = 'BAR'
013900110503     C                   eval      I28COD = wFLS+wLNA+wNRS+wNCD+wZNC
014000110503     C                   call      'TRUL28R1'
014100110503     C                   parm                    TRUL28DS
014200110503     C                   if        O28ERR = *blanks
014300110503     C                   eval      O_BarcodeBrt  = O28COD
014400110503     C                   else
014500110503     C                   eval      O_BarcodeBrt  = *all'*'
014600110503     C                   endif
014700051007     C*
014800110503     C* Valorizzo il buffer d output
014900110503     C                   EVAL      VAWDTA = O_DS_CLIENTE
015000930409     C*
015100060410     C                   WRITE     TIVAW000
015200060410     C                   DELETE    TIVGD000
015300991027     C*
015400060410     C                   READ      TIVGDTMP
015500030325     C                   ENDDO
015600991027     C*
015700030325     C                   EVAL      wrkesito = '0'
015800991027     C*
015900910830     C                   ENDSR
016000991027
016100110502
016200110502
016300991027      /TITLE Operazioni iniziali.
016400991027     C     *inzsr        BEGSR
016500991027     C*
016600991027     C     *ENTRY        PLIST
016700991027     C                   parm                    prmppt
016800991027     C     wrkesito      parm      wrkesito      prmesito
016900100225     C*
017000100225     C* Definizione chiavi
017100100225     C*
017200100225     C* Chiave su TITAS30C - Parziale
017300100225     C     KEYtas30P     KLIST
017400100225     C                   KFLD                    dscAAS
017500100225     C                   KFLD                    dscLNP
017600100225     C                   KFLD                    dscNRS
017700100225     C                   KFLD                    dscNSP
017800991027     C*
017900991027     C                   ENDSR
