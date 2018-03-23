000100120508     H DECEDIT('0.') DATEDIT(*DMY.)
000200120424     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000120508     D fnvacds       e ds                  extname(fnvac00t)
001100070316     D sepFld          s              1
001200071205
001300120509     D wDataSped       s             10    inz
001400120601     D wDataRich       s             10    inz
001500120508
001600120508
001700120508     D*------------------
001800120508     D* LINKING A DEFINIZIONI ESTERNE
001900120508     D*------------------
002000120508     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002100120508     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
002200120508
002300100224
002400100224
002500030908     C                   EXSR      chkRECFILMBR
002600991027     C                   EXSR      traduci
002700921023     C                   SETON                                        LR
002800030908
002900100224
003000030908     C     chkRECFILMBR  BEGSR
003100030908     C*
003200100209     C                   READ      TIVAWWWT                               55
003300030908     C*
003400030908     C                   ENDSR
003500100224
003600991027
003700991027     C     traduci       BEGSR
003800030325     C*
003900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004000090427     C* (nomi campi)
004100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004200100209     C                   EVAL      VAWDTA =
004300120601     C                             '"VACCCM"'                           +sepFld+
004400120601     C                             '"VACLNP"'                           +sepFld+
004500120601     C                             '"VACNRS"'                           +sepFld+
004600120601     C                             '"VACNSP"'                           +sepFld+
004700120601     C                             '"VACAAS"'                           +sepFld+
004800120508     C                             '"VACMGS"'                           +sepFld+
004900120601     C                             '"VACRMN"'                           +sepFld+
005000120601     C                             '"VACRMA"'                           +sepFld+
005100120601     C                             '"VACDCR"'                           +sepFld
005200030325     C*
005300100209     C                   WRITE     TIVAW000
005400030325     C                   ENDIF
005500090427     C*
005600090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005700090427     C* (descrizione campi)
005800090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
005900100209     C                   EVAL      VAWDTA =
006000120601     C                             '"COD CLIENTE"'                      +sepFld+
006100120601     C                             '"LINEA PARTENZA"'                   +sepFld+
006200120601     C                             '"SERIE"'                            +sepFld+
006300120601     C                             '"NUM.SPEDIZIONE"'                   +sepFld+
006400120601     C                             '"ANNO SPEDIZIONE"'                  +sepFld+
006500120508     C                             '"MESE/GIORNO SPED."'                +sepFld+
006600120601     C                             '"RIF.NUMERICO"'                     +sepFld+
006700120601     C                             '"RIF.ALFABETICO"'                   +sepFld+
006800120601     C                             '"DATA CONS. RICHIESTA"'             +sepFld
006900090427     C*
007000100209     C                   WRITE     TIVAW000
007100090427     C                   ENDIF
007200991027     C*
007300090325     C                   READ      tivgdtmp
007400090325     C                   DOW       not %eof
007500120508     C                   movel     vgddta        fnvacds
007600120508     C*
007700120601     C* Ritorno solamente le spedizioni non consegnate
007800120601     C                   IF        vacDCM > *zeros AND vacCCA = *blanks
007900120601     C                   ELSE
008000120601     C*
008100120601     C* Ritorno solamente le spedizioni con appuntamento
008200120601     C                   IF        vacTC1 = 'A' OR vacTC2 = 'A'
008300120601     C*
008400120601     C* Ritorno solamente le spedizioni con data consegna richiesta
008500120601     C                   IF        vacDCR > *zeros
008600120509     C*
008700120601     C                   EVAL      wDataRich =  %trim(UBFMTDATE_Convert(
008800120601     C                              %editc(vacDCR:'4'):'YYYYMMDD':'DD/MM/YYYY'))
008900120424     C*
009000100224     C                   EVAL      VAWDTA  =
009100120601     C                                       %trim(%editc(vacCCM:'X'))+sepFld+
009200120601     C                                       %trim(%editc(vacLNP:'X'))+sepFld+
009300120601     C                                       %trim(%editc(vacNRS:'X'))+sepFld+
009400120601     C                                       %trim(%editc(vacNSP:'X'))+sepFld+
009500120601     C                                       %trim(%editc(vacAAS:'X'))+sepFld+
009600120601     C                                       %trim(%editc(vacMGS:'X'))+sepFld+
009700120601     C                                       %trim(%editc(vacRMN:'X'))+sepFld+
009800120601     C                                       %trim(vacRMA)            +sepFld+
009900120601     C                                                     wDataRich  +sepFld
010000930409     C*
010100100209     C                   WRITE     TIVAW000
010200090325     C                   delete    tivgd000
010300120424     C*
010400120601     C                   ENDIF
010500120424     C                   ENDIF
010600120424     C                   ENDIF
010700991027     C*
010800090325     C                   READ      tivgdtmp
010900030325     C                   ENDDO
011000991027     C*
011100030325     C                   EVAL      wrkesito = '0'
011200991027     C*
011300910830     C                   ENDSR
011400991027
011500120424
011600120424
011700991027      /TITLE Operazioni iniziali.
011800991027     C     *inzsr        BEGSR
011900991027     C*
012000991027     C     *ENTRY        PLIST
012100991027     C                   parm                    prmppt
012200991027     C     wrkesito      parm      wrkesito      prmesito
012300070316     C*
012400070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
012500070316     C                   eval      sepFld = %subst(prmppt:2:1)
012600070316     C                   if        sepFld = *blanks
012700120508     C                   eval      sepFld = ';'
012800070316     C                   endif
012900120508     C*
013000120508     C* Verifico se richiesto un particolare carattere d separatore decimale
013100120508     C                   SETON                                        60
013200120508     C                   IF        %subst(prmppt:3:1) = ','
013300120508     C                   SETOFF                                       60
013400120508     C                   ENDIF
013500120424     C*
013600991027     C                   ENDSR
