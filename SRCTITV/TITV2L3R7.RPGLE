000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400090323     FTIVAWWWT  UF A E             DISK    commit
000500120808     FTIVGDTMP  UF   E             DISK
000600120808     FTIVGD00F  O    E             DISK    prefix(o_) rename(TIVGD000:TIVGDOUT)
000700120427
000800030924
000900041015     D*-----------
001000041015     D* VARIABILI D WRK
001100041015     D*-----------
001200031103     D prmppi          S             50
001300030924     D prmesito        S              1
001400030924     D wrkesito        S                   like(prmesito)
001500120808     D skREF           S             20    DIM(30000)
001600120809     D skOUT           S           1254    varying DIM(10000) inz
001700120809     D iOUT            S              5  0 inz
001800120808
001900041015
002000041015     D*-----------
002100041015     D* DEFINIZIONE DS ESTERNE
002200041015     D*-----------
002300120712     D TITV2L3DF     E DS
002400120712     D TITV2L3DH     E DS
002500120712     D TITV2L3DL     E DS
002600120808
002700120808
002800120808     D*-----------
002900120808     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
003000120808     D*-----------
003100120808     D DSPPI           DS
003200120808     D  w§PPITIP                      2
003300120808     D  w§PPIKSU                      7  0
003400031103
003500030924
003600030325
003700030924     C* MAIN/
003800041015     C*
003900041015     C* Esegue traduzione
004000991027     C                   EXSR      traduci
004100120809     C*
004200120809     C* Scarico i dati d output
004300120809     C                   EXSR      wrirec
004400110929     C*
004500921023     C                   SETON                                        LR
004600111121
004700120712
004800030924
004900991027     C     traduci       BEGSR
005000120712     C*
005100120809     C                   setoff                                       40
005200120808     C                   setoff                                       51
005300120808     C*
005400120808     C**************************************
005500120712     C*
005600120712     C* 1° => Record di file-header
005700120712     C                   clear                   TITV2L3DF
005800120712     C                   eval      F§REGTYPE  = 'F'
005900120808     C                   eval      F§FILETYPE = 'CRTCF'
006000120712     C                   eval      F§CARRCD   = 'BARTOLINI_TR'
006100120712     C                   eval      F§CRTDT    = %char(%dec(%date() : *ISO))
006200120712     C                   eval      F§CRTTM    = %editc(%dec(%time() : *ISO):'X')
006300120808     C                   eval      F§COLLECT  = 'RETU'
006400120809     C*
006500120809     C* Popolo la schiera degli output
006600120809     C                   eval      vawDTA = TITV2L3DF
006700120809     C                   z-add     1             iOUT
006800120809     C                   eval      skOUT(iOUT) = vawDTA
006900120808     C*
007000120808     C**************************************
007100120712     C*
007200120712     C* 2° => Record di shipment-header
007300120719     C*
007400120719     C* Inizializzo il contatore di spedizioni elaborate
007500120809     C                   z-add     0             skREFidx          5 0
007600120808     C*
007700120808     C                   READ      TIVGDTMP
007800120808     C                   DOW       not %eof(TIVGDTMP)
007900120808     C*
008000120808     C* Ignoro record "*VOID"
008100120808     C                   if        %trim(vgdDTA) = '*VOID'
008200120808     C                   else
008300120808     C*
008400120808     C                   eval      TITV2L3DH = vgdDTA
008500120719     C*
008600120808     C* Verifico che nel medesimo "giro" (ovvero file) non sia presente più volte lo stesso REF
008700120808     C     H§REF_1       lookup    skREF                                  55
008800120808     C                   if        %found
008900120727     C                   seton                                        51
009000120808     C                   else
009100120808     C* Salvo il REF 1 corrente
009200120808     C                   add       1             skREFidx
009300120808     C                   eval      skREF(skREFidx) = H§REF_1
009400120808     C                   endif
009500120808     C*
009600120808     C     H§REF_2       lookup    skREF                                  55
009700120808     C                   if        %found
009800120808     C                   seton                                        51
009900120808     C                   else
010000120808     C* Salvo il REF 2 corrente
010100120808     C                   add       1             skREFidx
010200120808     C                   eval      skREF(skREFidx) = H§REF_2
010300120808     C                   endif
010400120808     C*
010500120808     C* Se tutto ok => procedo
010600120808     C                   if        not *in51
010700120808     C*
010800120808     C* Scarico il buffer d output
010900120808     C                   eval      vawDTA = TITV2L3DH
011000120809     C                   exsr      INSREC
011100120808     C*
011200120808     C**************************************
011300120719     C*
011400120712     C* 3° => Record di shipment-pack (solo se record di testata ok)
011500120712     C                   clear                   TITV2L3DL
011600120712     C                   eval      L§REGTYPE  = 'L'
011700120712     C                   eval      L§ACTDT    = H§ACTDT
011800120712     C                   eval      L§ACTTM    = H§ACTTM
011900120712     C                   eval      L§REF_1    = H§REF_1
012000120712     C                   eval      L§STS      = H§STS
012100120712     C                   eval      L§PACKID   = *all'0'
012200120712     C                   eval      L§PCKWT    = '000000.000'
012300120712     C                   eval      L§PCKVOL   = '0000000000.000'
012400120712     C                   eval      L§PCKNR    = *all'0'
012500120712     C*
012600120712     C* Scarico il buffer d output
012700120712     C                   eval      vawDTA = TITV2L3DL
012800120809     C                   exsr      INSREC
012900120808     C*
013000120712     C                   endif
013100120808     C                   endif
013200120808     C*
013300120808     C                   DELETE    TIVGD000
013400120808     C*
013500120808     C                   READ      TIVGDTMP
013600120808     C                   ENDDO
013700120727     C*
013800120727     C* Se "spia" accesa => inserisco un record "*VOID"
013900120727     C                   if        *in51
014000120808     C                   clear                   tivgdOUT
014100120808     C                   eval      o_vgdTIP = w§PPITIP
014200120808     C                   eval      o_vgdKSU = '0'+%editc(w§PPIKSU:'X')
014300120808     C                   eval      o_vgdTSC = 'WW'
014400120808     C                   eval      o_vgdDAT = %dec(%date() : *ISO)
014500120808     C                   eval      o_vgdPGM = 'TITV2L3R7'
014600120808     C                   eval      o_vgdDTA = '*VOID'
014700120808     C                   write     tivgdOUT
014800120727     C                   endif
014900120808     C*
015000120808     C                   EVAL      wrkesito = '0'
015100991027     C*
015200910830     C                   ENDSR
015300031103
015400031103
015500120808
015600120809     C     insrec        BEGSR
015700120809     C*
015800120809     C* Popolo la schiera degli output
015900120809     C                   seton                                        40
016000120809     C                   add       1             iOUT
016100120809     C                   eval      skOUT(iOUT) = vawDTA
016200120808     C*
016300031103     C                   ENDSR
016400120809
016500120809
016600120809
016700120809     C     wrirec        BEGSR
016800120809     C*
016900120809     C* Solo se presenti anche record d spedizione => scarico lo stack di buffer d output
017000120809     C                   if        *in40
017100120809     C                   eval      iOUT = 1
017200120809     C                   dow       iOUT <= %elem(skOUT) AND
017300120809     C                             skOUT(iOUT) <> *blanks
017400120809     C                   eval      vawDTA = skOUT(iOUT)
017500120809     C                   write     tivaw000
017600120809     C                   add       1             iOUT
017700120809     C                   enddo
017800120809     C                   endif
017900120809     C*
018000120809     C                   ENDSR
018100031103
018200031103
018300991027
018400991027      /TITLE Operazioni iniziali.
018500991027     C     *inzsr        BEGSR
018600991027     C*
018700991027     C     *ENTRY        PLIST
018800031103     C                   parm                    prmppi
018900991027     C     wrkesito      parm      wrkesito      prmesito
019000031103     C*
019100031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
019200031103     C                   EVAL      DSPPI = prmppi
019300991027     C*
019400991027     C                   ENDSR
