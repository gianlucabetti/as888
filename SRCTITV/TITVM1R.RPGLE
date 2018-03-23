000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050413     FFNBLP01L  IF   E           K DISK
000500060215     FFIAR404L  IF   E           K DISK
000600050909     FTIVGDTMP  UF   E             DISK
000700050412     FTIVAWWWT  UF A E             DISK
000800031103
000900031103     D****
001000031103     D* DEFINIZIONE DS ESTERNE
001100031103     D****
001200050413     D DSBL4M        E DS
001300050412     D DSBLD7        E DS
001400050412     D TRUL28DS      E DS
001500031103
001600050412     D prmppt          S             50
001700050412     D prmesito        S              1
001800050412     D wrkesito        S                   like(prmesito)
001900050909     D fnvapds       e ds                  extname(fnvap00t)
002000050412
002100031103     D****
002200050413     D* VARIABILI DI WRK
002300031103     D****
002400050412     D wBARCODE        s             18    inz(*blanks)
002500050413     D i               s              7  0 inz(*zeros)
002600050412     D parDelimFld     s              1
002700050412     D parDelimTXT     s              1
002800031031
002900031031
003000031031     C***********
003100030924     C* MAIN/
003200031031     C***********
003300050412     C                   EXSR      traduci
003400921023     C                   SETON                                        LR
003500991027
003600030924
003700030924
003800050412     C     traduci       BEGSR
003900050413     C*
004000050413     C* Inizializzo indicatore d wrk
004100050413     C                   setoff                                       55
004200991027     C*
004300050909     C                   READ      TIVGDTMP
004400050909     C                   dow       not %eof(TIVGDTMP)
004500050909     C                   eval      fnvapds = vgdDTA
004600050413     C*
004700050413     C* Elaboro solo x fasi 900 (ovvero ORM bollettato)
004800050413     C                   if        vapFAR = 900
004900050412     C*
005000050412     C* Inizializzo il buffer del file d out TIVAWWWT
005100050412     C                   clear                   TIVAW000
005200050413     C*
005300050413     C* Se 1° record => scrivo preventivamente anche riga d intestazione colonne
005400050413     C                   if        *IN55 = *off
005500050413     C                   eval      VAWDTA = parDelimTXT +
005600050413     C                                      'ORDINE ID' +
005700050413     C                                      parDelimTXT +
005800050413     C                                      parDelimFld +
005900050413     C                                      parDelimTXT +
006000050413     C                              'COLLI DA RITIRARE' +
006100050413     C                                      parDelimTXT +
006200050413     C                                      parDelimFld +
006300050413     C                                      parDelimTXT +
006400050413     C                                    'DATA RITIRO' +
006500050413     C                                      parDelimTXT +
006600050413     C                                      parDelimFld +
006700050413     C                                      parDelimTXT +
006800050413     C                                  'LDV_Bartolini' +
006900050413     C                                      parDelimTXT
007000050413     C                   write     TIVAW000
007100050413     C                   seton                                        55
007200050413     C                   endif
007300050413     C*
007400050413     C* Dall'ORM aggancio l'estensione bolle partenza x reperire la bolla legata all'ORM corrente
007500050413     C                   clear                   DSBL4M
007600050413     C                   eval      §B4POE = VAPPOE
007700050413     C                   eval      §B4NSR = VAPNSR
007800050413     C                   eval      §B4NOR = VAPNOR
007900050413     C                   eval      §B4NRV = VAPNRV
008000050413     C*
008100060215     C                   eval      AR4TRC = 'M'
008200060215     C                   eval      AR4N14 = DSBL4M
008300060215     C     KEYar404M     chain     FIAR404L
008400060215     C                   if        %found(FIAR404L)
008500050413     C*
008600050413     C* Se ok => aggancio la bolla in partenza
008700050413     C     KEYblp01      chain     FNBLP01L
008800050413     C                   if        %found(FNBLP01L)
008900050413     C* ...ciclo sulle bolle in partenza x costruire il barcode d ogni segnacollo
009000050413     C                   for       i=BLPNCD TO (BLPNCD + BLPNCL - 1)
009100050413     C                   clear                   DSBLD7
009200050413     C                   eval      §LCLNP = BLPLNP
009300050413     C                   eval      §LCLNA = BLPLNA
009400050413     C                   eval      §LCNRS = BLPNRS
009500050413     C                   eval      §LCNCD = i
009600050413     C                   eval      §LCZSC = BLPZNC
009700110524     C* X ogni segnacollo costruisco il barcode standard BRT
009800050413     C                   exsr      calbarcode
009900050413     C                   eval      VAWDTA =      parDelimTXT +
010000050413     C                                         %trim(VAPRFA) +
010100050413     C                                           parDelimTXT +
010200050413     C                                           parDelimFld +
010300050413     C                             %trim(%editc(BLPNCL:'Z')) +
010400050413     C                                           parDelimFld +
010500050413     C                             %trim(%editc(BLPDRT:'Z')) +
010600050413     C                                           parDelimFld +
010700050413     C                                           parDelimTXT +
010800050413     C                                       %trim(wBARCODE) +
010900050413     C                                           parDelimTXT
011000050413     C* Quindi scarico il buffer del collo corrente
011100050413     C                   write     TIVAW000
011200050413     C                   endfor
011300050413     C*
011400050413     C                   endif
011500050413     C                   endif
011600050413     C                   endif
011700031031     C*
011800050413     C* X ogni fase ORM eseguo storicizzazione e delete del file d input
011900050909     C                   DELETE    TIVGD000
012000031031     C*
012100050413     C* E proseguo con la lettura delle fasi da VAP
012200050909     C                   READ      TIVGDTMP
012300050412     C                   enddo
012400031031     C*
012500050412     C                   eval      wrkesito = '0'
012600031031     C*
012700031031     C                   ENDSR
012800050412
012900050412
013000050412
013100050412     C     calbarcode    BEGSR
013200050412     C*
013300110524     C* Inizializzo la ds d procedura x calcolo barcode standard BRT e variabili d wrk
013400050412     C                   clear                   TRUL28DS
013500050412     C                   clear                   wBARCODE
013600050412     C*
013700050412     C* Valorizzo il buffer d input
013800050412     C                   eval      I28TLA = 'E'
013900050412     C                   eval      I28MOD = 'BAR'
014000050412     C                   eval      I28COD = DSBLD7
014100050413     C                   call      'TRUL28R1'
014200050413     C                   parm                    TRUL28DS
014300050412     C                   if        O28ERR = *blanks
014400050412     C                   eval      wBARCODE = O28COD
014500050412     C                   endif
014600050412     C*
014700050412     C                   ENDSR
014800031031
014900031031
015000031031
015100031031
015200991027
015300991027      /TITLE Operazioni iniziali.
015400991027     C     *inzsr        BEGSR
015500991027     C*
015600991027     C     *ENTRY        PLIST
015700991027     C                   parm                    prmppt
015800991027     C     wrkesito      parm      wrkesito      prmesito
015900050412     C*
016000050412     C* Imposto i flag d procedura dai parametri d traduzione ricevuti in input
016100050413     C                   eval      parDelimTXT = %subst(prmppt:1:1)
016200050413     C                   eval      parDelimFld = %subst(prmppt:2:1)
016300030709     C*
016400030709     C* Definizione chiavi
016500030924     C*
016600060215     C* Chiave su FIAR404L - Completa
016700060215     C     KEYar404M     KLIST
016800060215     C                   KFLD                    AR4TRC
016900060215     C                   KFLD                    AR4N14
017000050413     C* Chiave su FNBLP01L - Completa
017100050413     C     KEYblp01      KLIST
017200060215     C                   KFLD                    AR4AAS
017300060215     C                   KFLD                    AR4LNP
017400060215     C                   KFLD                    AR4NRS
017500060215     C                   KFLD                    AR4NSP
017600991027     C*
017700991027     C                   ENDSR
