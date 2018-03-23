000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200091104     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
000500090320     FTIVAWWWT  uF A E             DISK    commit
000600091104     FFNBLT01L  IF   E           K DISK    usropn
000700091104     F                                     extfile(LibFileBLT01)
000800091104     FFIARS01L  IF   E           K DISK    usropn
000900091104     F                                     extfile(LibFileARS01)
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400090320     D fnvabds       e ds                  extname(fnvab00t)
001500030325
001600070227     D iFLD            s              1    inz
001700070906     D iTXT            s             10    inz varying
001800091104     D wNNC            s             35
001900091104     D VABDSP          s              8  0 inz
002000091104
002100091104     D LibFileBLT01    s             21A   inz
002200091104     D LibFileARS01    s             21A   inz
002300091104     D currSysNeta     s              8A   inz('*NULL')
002400091104
002500091104
002600091104     D*------------------
002700091104     D* LINKING A DEFINIZIONI ESTERNE
002800091104     D*------------------
002900091104     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003000091104     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003100091104
003200091104
003300091104     C*
003400091104     C* Reperisco il sistema AS/400 corrente
003500091104     C                   callp     UBRTVNETA_Rtv(currSysNeta)
003600091104     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
003700091104     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
003800091104     C                   eval      LibFileARS01 = 'FILTRA201/FIARS01L'
003900091104     C                   else
004000091104     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
004100091104     C                   eval      LibFileARS01 = 'FILTRAPRD/FIARS01L'
004200091104     C                   endif
004300091104     C*
004400091104     C* Apertura file "overraidati"
004500091104     C                   if        not %open(FNBLT01L)
004600091104     C                   open      FNBLT01L
004700091104     C                   endif
004800091104     C                   if        not %open(FIARS01L)
004900091104     C                   open      FIARS01L
005000091104     C                   endif
005100091104     C*
005200030908     C                   EXSR      chkRECFILMBR
005300991027     C                   EXSR      traduci
005400091104     C*
005500091104     C* Chiusura file "overraidati"
005600091104     C                   if        %open(FNBLT01L)
005700091104     C                   close     FNBLT01L
005800091104     C                   endif
005900091104     C                   if        %open(FIARS01L)
006000091104     C                   close     FIARS01L
006100091104     C                   endif
006200091104     C*
006300921023     C                   SETON                                        LR
006400030908
006500091104
006600030908     C     chkRECFILMBR  BEGSR
006700030908     C*
006800070227     C                   READ      TIVAWWWT                               55
006900030908     C*
007000030908     C                   ENDSR
007100070312
007200070312
007300991027
007400991027     C     traduci       BEGSR
007500030325     C*
007600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007800091104     C                   EVAL      VAWDTA  =
007900091104     C                             'DATA SPEDIZIONE'        +iFLD+
008000091104     C                             'RIF. NUMERICO'          +iFLD+
008100091104     C                             'RIF. ALFABETICO'        +iFLD+
008200091104     C                             'RAG. SOC. DESTINATARIO' +iFLD+
008300091104     C                             'SEGNACOLLO'
008400030325     C*
008500070227     C                   WRITE     TIVAW000
008600030325     C                   ENDIF
008700991027     C*
008800090320     C                   READ      tivgdtmp
008900090320     C                   DOW       not %eof(tivgdtmp)
009000090320     c                   movel     vgddta        fnvabds
009100091104     C*
009200091104     C                   CLEAR                   wNNC
009300091104     C*
009400091104     C* Aggancio il dettaglio colli in partenza
009500091104     C     KEYblt01_P    SETLL     fnblt01l
009600091104     C                   IF        %equal(fnblt01l)
009700091104     C     KEYblt01_P    READE     fnblt01l
009800091104     C                   DOW       not %eof(fnblt01l)
009900091104     C*
010000091104     C                   EVAL      arsTRC = 'C'
010100091104     C     KEYars01_C    CHAIN     fiars01l
010200091104     C                   IF        %found(fiars01l)
010300091105     C                   EVAL      wNNC = %subst(arsNOT:9:11)
010400091104     C*
010500091104     C* Valorizzo il buffer d output
010600091104     C                   EXSR      wriOUT
010700091104     C                   ENDIF
010800091104     C*
010900091104     C     KEYblt01_P    READE     fnblt01l
011000091104     C                   ENDDO
011100091104     C                   ELSE
011200091104     C*
011300091104     C* Valorizzo il buffer d output
011400091104     C                   EXSR      wriOUT
011500091104     C*
011600091104     C                   ENDIF
011700070227     C*
011800090320     C                   delete    tivgd000
011900090320     C                   READ      tivgdtmp
012000091104     C*
012100030325     C                   ENDDO
012200991027     C*
012300030325     C                   EVAL      wrkesito = '0'
012400991027     C*
012500910830     C                   ENDSR
012600070228
012700070228
012800070228
012900070228     C     wriOUT        BEGSR
013000070228     C*
013100091104     C                   EVAL      VABDSP = VABAAS*10000+VABMGS
013200091104     C                   EVAL      VAWDTA  =
013300091104     C                             %trim(%editc(VABDSP:'Q'))             +iFLD+
013400091104     C                             %trim(%editc(VABRMN:'Q'))             +iFLD+
013500091104     C                             iTXT+%trim(VABRMA)               +iTXT+iFLD+
013600091104     C                             iTXT+%trim(VABRSD)               +iTXT+iFLD+
013700091104     C                             iTXT+%trim(wNNC)                 +iTXT
013800070228     C*
013900070228     C                   WRITE     TIVAW000
014000070228     C*
014100070228     C                   ENDSR
014200070228
014300070228
014400070228
014500991027
014600991027      /TITLE Operazioni iniziali.
014700991027     C     *inzsr        BEGSR
014800991027     C*
014900991027     C     *ENTRY        PLIST
015000991027     C                   parm                    prmppt
015100991027     C     wrkesito      parm      wrkesito      prmesito
015200070227     C*
015300070227     C* Definizione chiavi
015400091104     C*
015500091104     C* Chiave su FNBLT01L - Parziale
015600091104     C     KEYblt01_P    KLIST
015700091104     C                   KFLD                    vabAAS
015800091104     C                   KFLD                    vabLNP
015900091104     C                   KFLD                    vabNRS
016000091104     C                   KFLD                    vabNSP
016100091104     C*
016200091104     C* Chiave su FIARS01L - Completa
016300091104     C     KEYars01_C    KLIST
016400091104     C                   KFLD                    bltFLS
016500091104     C                   KFLD                    bltLNA
016600091104     C                   KFLD                    bltNRS
016700091104     C                   KFLD                    bltNSC
016800091104     C                   KFLD                    arsTRC
016900070227     C*
017000070227     C* Verifico il separatore campo richiesto
017100070227     C                   EVAL      iFLD = %subst(prmppt:2:1)
017200070906     C                   EVAL      iTXT = %trim(%subst(prmppt:3:1))
017300070227     C*
017400070227     C* Verifico correttezza parametri indicati
017500070227     C                   IF        %subst(prmppt:2:1) = *blanks
017600070227     C                   EVAL      iFLD=','
017700070227     C                   ENDIF
017800991027     C*
017900991027     C                   ENDSR
