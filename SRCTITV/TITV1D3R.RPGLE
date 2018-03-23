000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700080924     D fnvac00t      e DS
000800030924     D prmppt          S             50
000900030924     D prmesito        S              1
001000030924     D wrkesito        S                   like(prmesito)
001100080116     D wRMNalfa        S             15    inz
001200080116     D wTotRec         S             10  0 inz(2)
001300080116     D*------------------
001400080116     D* DS "XSRDA8" - CONTROLLA DATA (8)
001500080116     D*------------------
001600080116     D WLBDA8          DS                  INZ
001700080116     D  G08DAT                 1      8  0
001800080116     D  G08INV                 9     16  0
001900080116     D  G08ERR                17     17
002000080116     D  G08TGI                18     22  0
002100030924
002200030924     D*-----------
002300040719     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO RITORNO ESITI DEL CLIENTE
002400030924     D*-----------
002500080116     D E_DS_Cli_STR    DS
002600080116     D  E_STR_TIPREC                  1    INZ('0')
002700080116     D  E_STR_IDFILE                  6    INZ('RIEVIT')
002800080116     D  E_STR_DATFILE                 8    INZ('AAAAMMGG')
002900080116     D  E_STR_ORAFILE                 6    INZ('HHMMSS')
003000080116     D  E_STR_FILLER                  4    INZ
003100080116     D*
003200080116     D E_DS_Cli_DAT    DS
003300080116     D  E_DAT_TIPREC                  1    INZ('1')
003400080116     D  E_DAT_DATPREL                 8    INZ('AAAAMMGG')
003500080116     D  E_DAT_PRGPREL                 4    INZ(*all'0')
003600080116     D  E_DAT_CODMITT                 5    INZ
003700080116     D  E_DAT_CODDEST                 6    INZ
003800080116     D  E_DAT_ESITO                   1    INZ
003900080116     D*
004000080116     D E_DS_Cli_END    DS
004100080116     D  E_END_TIPREC                  1    INZ('9')
004200080116     D  E_END_TOTREC                 10    INZ(*all'0')
004300080116     D  E_END_FILLER                 14    INZ
004400080116     D*
004500040719
004600030924
004700030325
004800030924     C* MAIN/
004900991027     C                   EXSR      traduci
005000080116     C*
005100921023     C                   SETON                                        LR
005200991027
005300030924
005400030924
005500991027     C     traduci       BEGSR
005600080116     C*
005700080116     C* Inizializzo la DS d output => TESTATA
005800080116     C                   RESET                   E_DS_Cli_STR
005900080116     C                   MOVE(P)   datcor        E_STR_DATFILE
006000080116     C                   MOVE(P)   oracor        E_STR_ORAFILE
006100080116     C*
006200080116     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
006300080116     C                   EVAL      VAWDTA = E_DS_Cli_STR
006400080116     C                   WRITE     TIVAW000
006500080116     C*
006600080116     C* Restituisco al cliente solo le bolle consegnate OK o chiuse con reso
006700991027     C*
006800080924     C                   READ      TIVGDTMP
006900060119     C*
007000080924     C                   DOW       not %eof(TIVGDTMP)
007100080924     C*
007200080924     C                   EVAL      fnvac00t = vgdDTA
007300080116     C*
007400080116     C* Restituisco al cliente solo le bolle consegnate OK o chiuse con reso
007500080116     C                   IF        (VACDCM > *zeros AND VACCCA = *blanks) OR
007600080116     C                             (VACDCM > *zeros AND VACCCA = '2')
007700080116     C*
007800080116     C* Incremento il contatore record scritti nel file
007900080116     C                   EVAL      wTotRec = wTotRec + 1
008000060118     C*
008100080116     C* Inizializzo la DS d output => DETTAGLIO
008200080116     C                   RESET                   E_DS_Cli_DAT
008300060118     C*
008400080116     C* Valorizzazione buffer out
008500080116     C                   MOVE(P)   vacRMN        wRMNalfa
008600080116     C                   EVAL      E_DAT_DATPREL = %subst(wRMNalfa:4:8)
008700080116     C                   EVAL      E_DAT_PRGPREL = %subst(wRMNalfa:12:4)
008800080116     C                   EVAL      E_DAT_CODMITT = %subst(vacRMA:7:5)
008900080116     C                   EVAL      E_DAT_CODDEST = %subst(vacRMO:1:6)
009000080116     C                   IF        vacCCA = *blanks
009100080116     C                   EVAL      E_DAT_ESITO   = 'C'
009200080116     C                   ELSE
009300080116     C                   EVAL      E_DAT_ESITO   = 'R'
009400080116     C                   ENDIF
009500080116     C*
009600080116     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
009700080116     C                   EVAL      VAWDTA = E_DS_Cli_DAT
009800080116     C                   WRITE     TIVAW000
009900080116     C*
010000040719     C                   ENDIF
010100040719     C*
010200040719     C* In ogni caso storicizzo e cancello l'input sempre
010300080924     C                   DELETE    TIVGD000
010400040719     C*
010500040719     C* Proseguo con la lettura dell'input
010600080924     C                   READ      TIVGDTMP
010700060119     C*
010800030325     C                   ENDDO
010900080116     C*
011000080116     C* Inizializzo la DS d output => PIEDE
011100080116     C                   RESET                   E_DS_Cli_END
011200080116     C                   MOVE(P)   wTotRec       E_END_TOTREC
011300080116     C*
011400080116     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
011500080116     C                   EVAL      VAWDTA = E_DS_Cli_END
011600080116     C                   WRITE     TIVAW000
011700991027     C*
011800030325     C                   EVAL      wrkesito = '0'
011900991027     C*
012000910830     C                   ENDSR
012100080116
012200080116
012300080116
012400991027
012500991027      /TITLE Operazioni iniziali.
012600991027     C     *inzsr        BEGSR
012700991027     C*
012800991027     C     *ENTRY        PLIST
012900991027     C                   parm                    prmppt
013000991027     C     wrkesito      parm      wrkesito      prmesito
013100080116     C*
013200080116     C* CALCOLA LA DATA CORRENTE
013300080116     C                   time                    wn14             14 0
013400080116     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
013500080116     C                   movel     wn14          oracor            6 0          *ORA
013600080116     C                   z-add     wn8           g08dat
013700080116     C                   z-add     *zeros        g08inv
013800080116     C                   movel     '0'           g08err
013900080116     C                   call      'XSRDA8'
014000080116     C                   parm                    wlbda8
014100080116     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
014200991027     C*
014300991027     C                   ENDSR
