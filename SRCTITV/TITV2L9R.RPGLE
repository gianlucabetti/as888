000100120508     H DECEDIT('0.') DATEDIT(*DMY.)
000200120424     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120807     FTABEL00F  IF   E           K DISK
000500090325     Ftivgdtmp  uF   E             DISK
000600120725     FTITAH30C  IF   E           K DISK
000700120807     FTIGCP51L  IF   E           K DISK
000800100209     FTIVAWWWT  UF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300120508     D fnvacds       e ds                  extname(fnvac00t)
001400120807     D sepFld          s              1
001500120807
001600120807
001700120807     D*-----------
001800120807     D* DEFINIZIONE DS / SCHIERE CODICI EVENTO
001900120807     D*-----------
002000120807     D ds2A          e ds
002100120807     D sk2A            s             10    dim(500)
002200120807     D sk2Ades         s             75    dim(500)
002300120807     D i               s              3  0 inz
002400120807     D w2A             s             10
002500120807     D w2Ades          s             75
002600071205
002700120508
002800120508     D*------------------
002900120508     D* LINKING A DEFINIZIONI ESTERNE
003000120508     D*------------------
003100120508     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003200120508     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
003300120508
003400100224
003500100224
003600120807     C*
003700120807     C                   EXSR      cartab
003800030908     C                   EXSR      chkRECFILMBR
003900991027     C                   EXSR      traduci
004000120807     C*
004100921023     C                   SETON                                        LR
004200120807
004300120807
004400120807
004500120807     C     cartab        BEGSR
004600120807     C*
004700120807     C* Carico causali evento
004800120807     C                   clear                   sk2A
004900120807     C                   clear                   sk2Ades
005000120807     C                   z-add     *zeros        i
005100120807     C                   eval      tblKUT = 1
005200120807     C                   eval      tblCOD = '2A'
005300120807     C     KEYtabP       setll     tabel00f
005400120807     C     KEYtabP       reade     tabel00f
005500120807     C                   dow       not %eof(tabel00f)
005600120807     C                   movel(p)  tblUNI        ds2A
005700120807     C                   add       1             i
005800120807     C                   eval      sk2A(i)    = tblKEY
005900120807     C                   eval      sk2Ades(i) = §2ADES
006000120807     C     KEYtabP       reade     tabel00f
006100120807     C                   enddo
006200120807     C*
006300120807     C                   ENDSR
006400030908
006500100224
006600030908     C     chkRECFILMBR  BEGSR
006700030908     C*
006800100209     C                   READ      TIVAWWWT                               55
006900030908     C*
007000030908     C                   ENDSR
007100100224
007200991027
007300991027     C     traduci       BEGSR
007400030325     C*
007500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007600090427     C* (nomi campi)
007700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007800100209     C                   EVAL      VAWDTA =
007900120725     C                             '"VACRSD"'                           +sepFld+
008000120725     C                             '"VACRMA"'                           +sepFld+
008100120725     C                             '"VACRMA_2"'                         +sepFld+
008200120725     C                             '"VATNOT_E"'                         +sepFld+
008300120725     C                             '"VACDCM"'                           +sepFld+
008400120725     C                             '"VACCCA"'                           +sepFld+
008500120725     C                             '"VACDLA"'                           +sepFld+
008600120807     C                             '"VACDAG"'                           +sepFld+
008700120807     C                             '"VACCMC"'                           +sepFld+
008800120807     C                             '"VACCMCDES"'                        +sepFld
008900030325     C*
009000100209     C                   WRITE     TIVAW000
009100030325     C                   ENDIF
009200090427     C*
009300090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009400090427     C* (descrizione campi)
009500090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
009600100209     C                   EVAL      VAWDTA =
009700120725     C                             '"RAG. SOC. DESTINATARIO"'           +sepFld+
009800120725     C                             '"RIFERIMENTO MITTENTE"'             +sepFld+
009900120725     C                             '"RIFERIMENTO MITTENTE 2"'           +sepFld+
010000120725     C                             '"BARCODE"'                          +sepFld+
010100120725     C                             '"DATA CONSEGNA MERCE"'              +sepFld+
010200120725     C                             '"COD. CONSEGNA"'                    +sepFld+
010300120725     C                             '"DATA LASCIATO AVVISO"'             +sepFld+
010400120807     C                             '"DATA APERTURA GIACENZA"'           +sepFld+
010500120807     C                             '"CODICE GIACENZA"'                  +sepFld+
010600120807     C                             '"CAUSALE GIACENZA"'                 +sepFld
010700090427     C*
010800100209     C                   WRITE     TIVAW000
010900090427     C                   ENDIF
011000991027     C*
011100090325     C                   READ      tivgdtmp
011200090325     C                   DOW       not %eof
011300120508     C                   movel     vgddta        fnvacds
011400120807     C*
011500120807     C* Quindi aggancio l'ultima giacenza
011600120807     C                   clear                   w2A
011700120807     C                   clear                   w2Ades
011800120807     C                   eval      gcpFRG = *zeros
011900120807     C     KEYgcp51      chain     tigcp51l
012000120807     C                   if        %found(tigcp51l)
012100120807     C*
012200120807     C* Se giacenza NN annullata la considero
012300120807     C                   if        gcpATB = *blanks
012400120807     C                   z-add     1             i
012500120807     C                   movel     gcpCMC        w2A
012600120807     C     w2A           lookup    sk2A(i)                                50
012700120807     C                   if        %found
012800120807     C                   eval      w2Ades = sk2Ades(i)
012900120807     C                   endif
013000120807     C                   endif
013100120807     C                   endif
013200120725     C*
013300120725     C* Aggancio il dettaglio colli
013400120725     C                   EVAL      tahTRC = 'C'
013500120725     C     KEYtah30_C    SETLL     titah30c
013600120725     C                   IF        %equal(titah30c)
013700120725     C     KEYtah30_C    READE     titah30c
013800120725     C                   DOW       not %eof(titah30c)
013900120725     C*
014000120725     C                   EVAL      VAWDTA  =
014100120725     C                                              %trim(vacRSD)     +sepFld+
014200120725     C                                              %trim(vacRMA)     +sepFld+
014300120725     C                                         'W.'+%trim(vacRMA)     +sepFld+
014400120725     C                                              %trim(tahNOT)     +sepFld+
014500120725     C                                       %trim(%editc(vacDCM:'X'))+sepFld+
014600120725     C                                              %trim(vacCCA)     +sepFld+
014700120725     C                                       %trim(%editc(vacDLA:'X'))+sepFld+
014800120807     C                                       %trim(%editc(vacDAG:'X'))+sepFld+
014900120807     C                                              %trim(w2A)        +sepFld+
015000120807     C                                              %trim(w2Ades)     +sepFld
015100120725     C*
015200120725     C                   WRITE     TIVAW000
015300120725     C*
015400120725     C     KEYtah30_C    READE     titah30c
015500120725     C                   ENDDO
015600120725     C                   ENDIF
015700120424     C*
015800120725     C                   delete    tivgd000
015900991027     C*
016000090325     C                   READ      tivgdtmp
016100030325     C                   ENDDO
016200991027     C*
016300030325     C                   EVAL      wrkesito = '0'
016400991027     C*
016500910830     C                   ENDSR
016600991027
016700120424
016800120424
016900991027      /TITLE Operazioni iniziali.
017000991027     C     *inzsr        BEGSR
017100991027     C*
017200991027     C     *ENTRY        PLIST
017300991027     C                   parm                    prmppt
017400991027     C     wrkesito      parm      wrkesito      prmesito
017500070316     C*
017600070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
017700070316     C                   eval      sepFld = %subst(prmppt:2:1)
017800070316     C                   if        sepFld = *blanks
017900120508     C                   eval      sepFld = ';'
018000070316     C                   endif
018100120508     C*
018200120508     C* Verifico se richiesto un particolare carattere d separatore decimale
018300120508     C                   SETON                                        60
018400120508     C                   IF        %subst(prmppt:3:1) = ','
018500120508     C                   SETOFF                                       60
018600120508     C                   ENDIF
018700120725     C*
018800120725     C* Definizione chiavi
018900120807     C*
019000120807     C* Chiave su TABEL00F - parziale
019100120807     C     KEYtabP       KLIST
019200120807     C                   KFLD                    tblKUT
019300120807     C                   KFLD                    tblCOD
019400120807     C*
019500120807     C* Chiave su TIGCP51L - Completa
019600120807     C     KEYgcp51      KLIST
019700120807     C                   KFLD                    vacAAS
019800120807     C                   KFLD                    vacLNP
019900120807     C                   KFLD                    vacNRS
020000120807     C                   KFLD                    vacNSP
020100120807     C                   KFLD                    gcpFRG
020200120725     C*
020300120725     C* Chiave su TITAH30C - Completa
020400120725     C     KEYtah30_C    KLIST
020500120725     C                   KFLD                    vacAAS
020600120725     C                   KFLD                    vacLNP
020700120725     C                   KFLD                    vacNRS
020800120725     C                   KFLD                    vacNSP
020900120725     C                   KFLD                    tahTRC
021000120424     C*
021100991027     C                   ENDSR
