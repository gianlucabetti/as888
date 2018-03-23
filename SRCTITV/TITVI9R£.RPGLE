000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050316     FTIORI02L  IF   E           K DISK    rename(tiori000:tiori002)
000500050316     F                                     prefix(O_)
000600050316     FTIORI05L  IF   E           K DISK    rename(tiori000:tiori005)
000700050316     F                                     prefix(O_)
000800050909     FTIVGDTMP  UF   E             DISK
000900011123     Ffnvapwwt  O    E             DISK    rename(fnvap000:fnvapaaa)
001000011123     F                                     prefix(A_)
001100050316     Ftivawwwt  O    E             DISK
001200050316     FTIVLT01L  UF A E           K DISK
001300050316     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001400050316     F                                     PREFIX(f_)
001500050316     F                                     USROPN
001600991027
001700991027     D prmppt          s             50
001800991027     D prmesito        s              1
001900050316     D prmksc          s              7
002000050316     D prmisv          s              2
002100050929     D prmtip          s              2
002200991027     D wrkesito        s                   like(prmesito)
002300050316     D dsVAP         e ds                  extname(FNVAPWWT) prefix(A_)
002400050316     D wrkoggiiso      s               d
002500050316     D wrkmese         s              2  0
002600050316     D dwlprg          s             10    INZ(*all'0')
002700050316     D wrkprg          s              8  0 INZ(*zeros)
002800050316     D                 ds                  INZ
002900050316     D wrkday                  1      2s 0
003000050316     D wrkdaychr                      2    overlay(wrkday)
003100050316     D mc              s              1    dim(12) ctdata perrcd(12)
003200050316     D flgWP           s              1    inz('0')
003300050909     D fnvapds       e ds                  extname(fnvap00t)
003400050316
003500050316     D*---------------------
003600050316     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700050316     D*---------------------
003800050316     D WLBDA8          DS
003900050316     D  G08DAT                 1      8  0
004000050316     D  G08INV                 9     16  0
004100050316     D  G08ERR                17     17
004200050316     D  G08TGI                18     22  0
004300991027
004400050316
004500050316
004600050316     C* \MAIN
004700991027     C                   EXSR      traduci
004800921023     C                   SETON                                        LR
004900050316
005000050316
005100991027
005200991027     C     traduci       BEGSR
005300991027     C*
005400050909     C                   READ      TIVGDTMP
005500991027     C                   dow       not %eof
005600050909     C                   eval      fnvapds = vgdDTA
005700991027     C*
005800011123     C                   MOVE      vapIDC        A_vapIDC
005900011123     C                   MOVE      vapPOE        A_vapPOE
006000011123     C                   MOVE      vapDPE        A_vapDPE
006100011123     C                   MOVE      vapNSR        A_vapNSR
006200011123     C                   MOVE      vapNOR        A_vapNOR
006300011123     C                   MOVE      vapNRV        A_vapNRV
006400011123     C                   MOVE      vapRFA        A_vapRFA
006500011123     C                   MOVE      vapPOG        A_vapPOG
006600011123     C                   MOVE      vapDPG        A_vapDPG
006700011123     C                   MOVE      vapDAE        A_vapDAE
006800011123     C                   MOVE      vapORE        A_vapORE
006900011123     C                   MOVE      vapFAR        A_vapFAR
007000011123     C                   MOVE      vapDFA        A_vapDFA
007100011123     C                   MOVE      vapCAR        A_vapCAR
007200011123     C                   MOVE      vapDCA        A_vapDCA
007300011123     C                   MOVE      vapNOT        A_vapNOT
007400030120     C*
007500030120     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
007600030120     C                   MOVE      '.'           A_vapNOT
007700050316     C*
007800050316     C* Se ricevuti parametri in ingresso gestisco lo "sdoppiamento" della traduzione,
007900050316     C* altrimenti nulla cambia
008000050316     C*
008100050316     C                   IF        prmppt <> *blanks
008200930409     C*
008300050316     C* Verifico se trattasi d fase relativa a ORM proveniente da Internet (data entry cliente)
008400050316     C* oppure se trattasi d ORM proveniente da file cliente.
008500050316     C*
008600050316     C* X le fasi 000 sono costretto ad agganciare il TIORI02L (in quanto nn è ancora stato
008700050316     C* generato l'ORM effettivo e quindi sulle fasi nn è ancora presente la "chiave" ORM)
008800050316     C                   IF        vapFAR = *zeros
008900050316     C     KeyORM02_C    CHAIN     TIORI02L
009000050316     C                   IF        %found(TIORI02L)
009100050316     C                   IF        O_vaoTCO = 'I'
009200071121     C   41              WRITE     FNVAPaaa
009300050316     C                   ENDIF
009400050316     C                   IF        O_vaoTCO = 'F'
009500050316     C                   EVAL      vawDTA = dsVAP
009600071121     C   40              WRITE     TIVAW000
009700050316     C                   EVAL      flgWP = '1'
009800050316     C                   ENDIF
009900050316     C                   ENDIF
010000050316     C* se fase <> 000 invece aggancio il TIORI05L con "chiave" ORM
010100050316     C                   ELSE
010200050316     C     KeyORM05_C    CHAIN     TIORI05L
010300050316     C                   IF        %found(TIORI05L)
010400050316     C                   IF        O_vaoTCO = 'I'
010500071121     C   41              WRITE     FNVAPaaa
010600050316     C                   ENDIF
010700050316     C                   IF        O_vaoTCO = 'F'
010800050316     C                   EVAL      vawDTA = dsVAP
010900071121     C   40              WRITE     TIVAW000
011000050316     C                   EVAL      flgWP = '1'
011100050316     C                   ENDIF
011200050316     C                   ENDIF
011300050316     C                   ENDIF
011400050316     C*
011500050316     C                   ELSE
011600071121     C   41              WRITE     FNVAPaaa
011700050316     C                   ENDIF
011800050316     C*
011900050316     C* In ogni caso storicizzo e deleto dal file d input
012000050909     C                   DELETE    TIVGD000
012100991027     C*
012200050909     C                   READ      TIVGDTMP
012300991027     C                   ENDdo
012400991027     C*
012500050316     C* Al termine della traduzione creo il record d log relativo al 2° file creato in fase
012600050316     C* d traduzione (1° file soli ORM da Internet - 2° file soli ORM da file)
012700050316     C* ma solo se effettivamente tradotto qualcosa
012800050316     C                   IF        flgWP = '1'
012900060904SOSP C***                EXSR      crealogs
013000050316     C                   ENDIF
013100050316     C*
013200991027     C                   eval      wrkesito = '0'
013300991027     C*
013400910830     C                   ENDSR
013500050316
013600050316
013700050316     C     CREALOGS      BEGSR
013800050316     C*
013900050316     C* Valorizzo i campi comuni a tutti i reord di log da "clonare"
014000050316     C                   CLEAR                   TIVLT000
014100050316     C                   EVAL      vltISV = prmisv
014200050316     C                   EVAL      vltKSC = '0' + prmksc
014300050316     C                   EVAL      vltKSU = '0' + prmksc
014400050316     C                   EVAL      vltMBD = 'M0'+ prmksc
014500050316     C                   EVAL      vltSND = 'D'
014600050909     C                   EVAL      vltFLS = 'FNVAP00S'
014700050316     C*
014800050316     C                   time                    wrkoggiiso
014900050316     C                   extrct    wrkoggiiso:*m wrkmese
015000050316     C                   extrct    wrkoggiiso:*d wrkday
015100050316     C                   eval      vltMBS =
015200050316     C                             mc(wrkmese)
015300050316     C                             + wrkdaychr
015400050316     C                             + %subst(vltKSU:2:7)
015500050316     C*
015600050316     C                   EVAL      vltDAT = DATCOR
015700050316     C                   EVAL      vltORA = ORACOR
015800050316     C                   EVAL      vltTIA = 'F'
015900050316     C                   EVAL      vltFL2 = '01'
016000050316     C                   EVAL      vltSTS = '0'
016100050316     C*
016200050316     C* => scrivo record di log x tipo file WP (FASI ORM DA FILE WURTH)
016300050316     C*
016400050316     C                   exsr      calprog
016500050316     C                   EVAL      vltPRG = dwlprg
016600050316     C*
016700050929     C***                EVAL      vltTIP = 'WP'
016800050929     C                   EVAL      vltTIP = prmtip
016900050929     C***                EVAL      vltFLD = 'TIVAWWWT'
017000050929     C                   EVAL      vltFLD = 'FNVAPWWT'
017100050316     C*
017200050316     C* Quindi scarico il buffer e creo il record d log x il download del tipo file WP
017300050316     C* autogenerato dalla traduzione del tipo file VP
017400050316     C                   WRITE     TIVLT000
017500050316     C*
017600050316     C                   ENDSR
017700050316
017800050316
017900050316
018000050316      /TITLE Valorizzazione Progressivo Applicazione
018100050316     C     calprog       begsr
018200050316     C*
018300050316     C                   open      tis7prgf
018400050316     C*
018500050316     C                   read(e)   tis7prgf
018600050316     C                   if        not %error
018700050316     C                   eval      dwlprg = f_tis7prgf
018800050316     C*
018900050316     C                   move(p)   dwlprg        wrkprg
019000050316     C                   add       1             wrkprg
019100050316     C                   move(p)   wrkprg        dwlprg
019200050316     C                   movel     prmisv        dwlprg
019300050316     C*
019400050316     C                   eval      f_tis7prgf = dwlprg
019500050316     C                   update    tis7prg0
019600050316     C                   endif
019700050316     C*
019800050316     C                   close     tis7prgf
019900050316     C*
020000050316     C                   endsr
020100050316
020200050316
020300050316
020400050316
020500991027
020600991027      /TITLE Operazioni iniziali.
020700991027     C     *inzsr        BEGSR
020800991027     C*
020900991027     C     *ENTRY        PLIST
021000991027     C                   parm                    prmppt
021100991027     C     wrkesito      parm      wrkesito      prmesito
021200050316     C                   PARM                    prmisv
021300050316     C                   PARM                    prmksc
021400050929     C                   PARM                    prmtip
021500071121     C*
021600071121     C* Verifico se richiesta scrittura:
021700071121     C* - E = entrambi i file TIVAW e FNVAP
021800071121     C* - W = solo       file TIVAW
021900071121     C* - P = solo       file FNVAP
022000071121     C                   setoff                                       4041
022100071121     C                   if        %subst(prmppt:12:1) = 'E'
022200071121     C                   seton                                        4041
022300071121     C                   endif
022400071121     C                   if        %subst(prmppt:12:1) = 'W'
022500071121     C                   seton                                        40
022600071121     C                   endif
022700071121     C                   if        %subst(prmppt:12:1) = 'P'
022800071121     C                   seton                                        41
022900071121     C                   endif
023000050316     C*
023100050316     C* IMPOSTA LA DATA CORRENTE
023200050316     C                   TIME                    N14              14 0
023300050316     C                   MOVE      N14           N8                8 0
023400050316     C                   MOVEL     N14           ORACOR            6 0
023500050316     C                   Z-ADD     N8            G08DAT
023600050316     C                   Z-ADD     *ZEROS        G08INV
023700050316     C                   MOVEL     '0'           G08ERR
023800050316     C                   CALL      'XSRDA8'
023900050316     C                   PARM                    WLBDA8
024000050316     C                   Z-ADD     G08INV        DATCOR            8 0
024100050316     C*
024200050316     C* Definizione chiavi file DB
024300050316     C     KeyORM02_C    KLIST
024400050316     C                   KFLD                    vapIDC
024500050316     C                   KFLD                    vapDAE
024600050316     C                   KFLD                    vapORE
024700050316     C*
024800050316     C     KeyORM05_C    KLIST
024900050316     C                   KFLD                    vapPOE
025000050316     C                   KFLD                    vapNSR
025100050316     C                   KFLD                    vapNOR
025200050316     C                   KFLD                    vapNRV
025300991027     C*
025400991027     C                   ENDSR
025500050316** ctdata mc
025600050316ABCDEFGHILMN
