000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400070329     FTITAS30C  IF   E           K DISK
000500080924     FTIVGDTMP  UF   E             DISK
000600040719     FTIVAWWWT  UF A E             DISK
000700030924
000800080924     D fnvac00t      E DS
000900030924     D prmppt          S             50
001000030924     D prmesito        S              1
001100030924     D wrkesito        S                   like(prmesito)
001200030924
001300030924     D*-----------
001400040719     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO RITORNO ESITI DEL CLIENTE
001500030924     D*-----------
001600040719     D E_DS_Cli        DS
001700070329     D  E_RMA                        10    INZ
001800070329     D  E_DRT                         8    INZ('00000000')
001900070329     D  E_FILL1                       4    INZ('0000')
002000070329     D  E_DCM                         8    INZ('00000000')
002100070329     D  E_HMC                         4    INZ('0000')
002200070329     D  E_ESITO                       2    INZ
002300070329     D  E_NOMINATIVO                 15    INZ
002400070329     D  E_RMN                        10    INZ('0000000000')
002500040719
002600030924
002700030325
002800030924     C* MAIN/
002900991027     C                   EXSR      traduci
003000921023     C                   SETON                                        LR
003100991027
003200030924
003300030924
003400991027     C     traduci       BEGSR
003500991027     C*
003600080924     C                   READ      TIVGDTMP
003700060119     C                   MOVEL     '0'           wScrivi           1
003800060119     C*
003900080924     C                   DOW       not %eof(TIVGDTMP)
004000080924     C*
004100080924     C                   eval      fnvac00t = vgdDTA
004200070329     C*
004300070329     C* Aggancio i file bolle
004400070329     C     KEYTAS        chain     titas30c
004500070329     C                   if        %found(titas30c)
004600070329     C*
004700070329     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
004800070329     C                   reset                   E_DS_Cli
004900070329     C*
005000070329     C* Imposto il buffer d output
005100070329     C                   movel     VACRMA        E_RMA
005200070329     C                   move(p)   TASDRT        E_DRT
005300070329     C                   move(p)   VACDCM        E_DCM
005400070329     C                   move(p)   VACHMC        E_HMC
005500070329     C                   move(p)   VACRMN        E_RMN
005600040719     C*
005700070329     C* Restituisco al cliente le bolle consegnate ok
005800070329     C                   if        VACDCM > *zeros AND
005900070329     C                             VACCCA = *blanks
006000070329     C                   eval      E_ESITO = 'OK'
006100070329     C                   eval      VAWDTA = E_DS_Cli
006200070329     C                   write     TIVAW000
006300070329     C                   endif
006400070329     C*
006500070329     C* Restituisco al cliente le bolle rese
006600070329     C                   if        VACDCM > *zeros AND
006700070329     C                             (VACCCA = '2' or VACCCA = '6')
006800070329     C                   eval      E_ESITO = 'RT'
006900070329     C                   eval      VAWDTA = E_DS_Cli
007000070329     C                   write     TIVAW000
007100070329     C                   endif
007200070329     C*
007300070329     C* Restituisco al cliente le bolle chiuse con anomalia
007400070329     C                   if        VACDCM > *zeros AND
007500070329     C                             VACCCA = '5'
007600070329     C                   eval      E_ESITO = 'KO'
007700070329     C                   eval      VAWDTA = E_DS_Cli
007800070329     C                   write     TIVAW000
007900070329     C                   endif
008000070329     C*
008100070329     C                   endif
008200040719     C*
008300040719     C* In ogni caso storicizzo e cancello l'input sempre
008400080924     C                   DELETE    TIVGD000
008500040719     C*
008600040719     C* Proseguo con la lettura dell'input
008700080924     C                   READ      TIVGDTMP
008800060119     C*
008900030325     C                   ENDDO
009000991027     C*
009100030325     C                   EVAL      wrkesito = '0'
009200991027     C*
009300910830     C                   ENDSR
009400991027
009500070329
009600070329
009700991027      /TITLE Operazioni iniziali.
009800991027     C     *inzsr        BEGSR
009900991027     C*
010000991027     C     *ENTRY        PLIST
010100991027     C                   parm                    prmppt
010200991027     C     wrkesito      parm      wrkesito      prmesito
010300070329     C*
010400070329     C* Definizione chivi sui file
010500070329     C* - TITAS30C -
010600070329     C     KEYTAS        KLIST
010700070329     C                   KFLD                    VACAAS
010800070329     C                   KFLD                    VACLNP
010900070329     C                   KFLD                    VACNRS
011000070329     C                   KFLD                    VACNSP
011100991027     C*
011200991027     C                   ENDSR
