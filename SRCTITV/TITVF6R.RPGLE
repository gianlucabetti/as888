000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400041025     FTITAS30C  IF   E           K DISK
000500041025     FTITAA30C  IF   E           K DISK
000600080924     FTIVGDTMP  uF   E             DISK
000700041025     FTIVAWWWT  uF A E             DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200041025
001300080924     D fnvac00t      e ds
001400991027
001500030908     C                   EXSR      chkRECFILMBR
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800030908
001900030908     C     chkRECFILMBR  BEGSR
002000030908     C*
002100041025     C                   READ      TIVAWWWT                               55
002200030908     C*
002300030908     C                   ENDSR
002400991027
002500991027     C     traduci       BEGSR
002600030325     C*
002700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
002800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
002900041025     C                   EVAL      VAWDTA  = '"Lettera di vettura"'    +';'+
003000041025     C                                       '"Data accettazione"'     +';'+
003100041025     C                                       '"Data Consegna"'         +';'+
003200041025     C                                       '"Ora consegna"'          +';'+
003300041025     C                                       '"Firma"'                 +';'+
003400041025     C                                       '"Mittente"'              +';'+
003500041025     C                                       '"Prov"'                  +';'+
003600041025     C                                       '"Destinatario"'          +';'+
003700041025     C                                       '"Via destinatario"'      +';'+
003800041025     C                                       '"CAP Destinatario"'      +';'+
003900041025     C                                       '"Città destinatario"'    +';'+
004000041025     C                                       '"Provincia destinatario"'+';'+
004100041025     C                                       '"Riferimento cliente"'   +';'+
004200041025     C                                       '"Codice consegna"'       +';'+
004300041025     C                                       '"Descrizione causale"'
004400030325     C*
004500041025     C                   WRITE     TIVAW000
004600030325     C                   ENDIF
004700991027     C*
004800080924     C                   READ      TIVGDTMP
004900080924     C                   DOW       not %eof(TIVGDTMP)
005000080924     C*
005100080924     C                   eval      fnvac00t = vgdDTA
005200041025     C*
005300041025     C* Aggancio i file bolle e relativa estensione 'A'
005400041025     C     KEYTAS        chain     titas30c
005500041025     C                   if        %found(titas30c)
005600041025     C                   movel     tasIND        wIndDest         35
005700041025     C                   movel     tasCAD        wCadDest          9
005800041025     C                   movel     tasLOD        wLodDest         35
005900041025     C                   else
006000041025     C                   movel     *blanks       wIndDest
006100041025     C                   movel     *blanks       wCadDest
006200041025     C                   movel     *blanks       wLodDest
006300041025     C                   endif
006400041025     C*
006500041025     C                   eval      taaTRC = 'M'
006600041025     C     KEYTAA        chain     titaa30c
006700041025     C                   if        %found(titaa30c)
006800041025     C                   movel     taaRSC        wRagSocMitt      35
006900041025     C                   movel     taaPRV        wPrvMitt          2
007000041025     C                   else
007100041025     C                   movel     *blanks       wRagSocMitt      35
007200041025     C                   movel     *blanks       wPrvMitt          2
007300041025     C                   endif
007400030325     C*
007500041025     C* Inizializzo le variabili d wrk x la valorizzazione della causale/descrizione del cliente
007600041025     C                   movel     *blanks       wCodCausale      10
007700041025     C                   movel     *blanks       wDesCausale     200
007800041025     C*
007900041025     C* Effettuo considerazioni
008000041025     C* ...consegna OK
008100041025     C                   if        VACCCA = *blanks AND VACDCM > *zeros
008200041025     C                   eval      wCodCausale = *blanks
008300041025     C                   eval      wDesCausale = 'CONSEGNATO'
008400041025     C                   endif
008500041025     C* ...messa in consegna
008600041025     C                   if        VACCCA = 'C'
008700041025     C                   eval      wCodCausale = *blanks
008800041025     C                   eval      wDesCausale = 'IN CONSEGNA'
008900041025     C                   endif
009000041025     C* ...lasciato avviso
009100041025     C                   if        VACDLA > *zeros AND VACCCA = *blanks
009200041025     C                   eval      wCodCausale = *blanks
009300041025     C                   eval      wDesCausale = 'TENTATA RICONSEGNA'
009400041025     C                   endif
009500041025     C*
009600041025     C* Quindi compongo il buffer d out e scarico
009700041025     C                   EVAL      VAWDTA = %trim(%editc(VACNSP:'Q'))+';'+
009800041025     C                             %trim(%editc(VACAAS*10000+VACMGS:'Q'))+';'+
009900041025     C                             %trim(%editc(VACDCM:'Q'))+';'+
010000041025     C                             %trim(%editc(VACHMC:'Q'))+';'+
010100041025     C                             '" "'+';'+
010200041025     C                             '"'+%trim(wRagSocMitt)+'"'+';'+
010300041025     C                             '"'+%trim(wPrvMitt)+'"'+';'+
010400041025     C                             '"'+%trim(VACRSD)+'"'+';'+
010500041025     C                             '"'+%trim(wIndDest)+'"'+';'+
010600041025     C                             '"'+%trim(wCadDest)+'"'+';'+
010700041025     C                             '"'+%trim(wLodDest)+'"'+';'+
010800041025     C                             '"'+%trim(VACPRD)+'"'+';'+
010900041025     C                             '"'+%trim(VACRMA)+'"'+';'+
011000041025     C                             '"'+%trim(wCodCausale)+'"'+';'+
011100041025     C                             '"'+%trim(wDesCausale)+'"'
011200930409     C*
011300041025     C                   WRITE     TIVAW000
011400080924     C                   DELETE    TIVGD000
011500991027     C*
011600080924     C                   READ      TIVGDTMP
011700030325     C                   ENDDO
011800991027     C*
011900030325     C                   EVAL      wrkesito = '0'
012000991027     C*
012100910830     C                   ENDSR
012200041025
012300041025
012400991027
012500991027      /TITLE Operazioni iniziali.
012600991027     C     *inzsr        BEGSR
012700991027     C*
012800991027     C     *ENTRY        PLIST
012900991027     C                   parm                    prmppt
013000991027     C     wrkesito      parm      wrkesito      prmesito
013100041025     C*
013200041025     C* Definizione chivi sui file
013300041025     C* - TITAS30C -
013400041025     C     KEYTAS        KLIST
013500041025     C                   KFLD                    VACAAS
013600041025     C                   KFLD                    VACLNP
013700041025     C                   KFLD                    VACNRS
013800041025     C                   KFLD                    VACNSP
013900041025     C* - TITAA30C -
014000041025     C     KEYTAA        KLIST
014100041025     C                   KFLD                    VACAAS
014200041025     C                   KFLD                    VACLNP
014300041025     C                   KFLD                    VACNRS
014400041025     C                   KFLD                    VACNSP
014500041025     C                   KFLD                    TAATRC
014600991027     C*
014700991027     C                   ENDSR
