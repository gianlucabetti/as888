000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090303     Ftitas30c  iF   E           K DISK
000600090303     Fwfdsc00f  uF A E             DISK    usropn
000700090303     F                                     extfile(LibFile)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300090303     D wfdscds       e ds                  extname(wfdsc00f)
001400090303     D LibFile         s             21    inz
001500090303     D dataIso         S               D   datfmt(*iso)
001600090303     D dataEur         S               D   datfmt(*eur)
001700090303
001800090303
001900090303
002000991027
002100090303     C*
002200090303     C* Gestione parametri d esecuzione
002300090303     C*
002400090303     C* Primi 10 byte => libreria destinazione
002500090303     C                   eval      LibFile = %trim(%subst(prmppt:1:10))+'/'+
002600090303     C                                       'WFDSC00F'
002700090303     C*
002800090303     C* Apertura file
002900090303     C                   if        not %open(wfdsc00f)
003000090303     C                   open      wfdsc00f
003100090303     C                   endif
003200090303     C*
003300090303     C* Elaborazione
003400090303     C                   exsr      traduci
003500090303     C*
003600090303     C* Chiusura file
003700090303     C                   if        %open(wfdsc00f)
003800090303     C                   close     wfdsc00f
003900090303     C                   endif
004000090303     C*
004100921023     C                   SETON                                        LR
004200060505
004300090303
004400991027
004500991027     C     traduci       BEGSR
004600030325     C*
004700060410     C                   READ      TIVGDTMP
004800060410     C                   DOW       not %eof(TIVGDTMP)
004900060411     C*
005000060411     C* Valorizzo il buffer dal campo flat di inout
005100090303     C                   EVAL      wfdscds = vgdDTA
005200090303     C*
005300090303     C* Verifico se bolla corrente è della "famiglia" (rispetto al cliente pagante)
005400090511     C     KEYtas30_P    chain     titas30c
005500090511     C                   if        %found(titas30c)
005600090303     C                   WRITE     wfdsc000
005700090303     C                   endif
005800930409     C*
005900060410     C                   DELETE    TIVGD000
006000991027     C*
006100060410     C                   READ      TIVGDTMP
006200030325     C                   ENDDO
006300991027     C*
006400030325     C                   EVAL      wrkesito = '0'
006500991027     C*
006600910830     C                   ENDSR
006700060505
006800090303
006900090303
007000991027
007100991027      /TITLE Operazioni iniziali.
007200991027     C     *inzsr        BEGSR
007300991027     C*
007400991027     C     *ENTRY        PLIST
007500991027     C                   parm                    prmppt
007600991027     C     wrkesito      parm      wrkesito      prmesito
007700090303     C*
007800090303     C* Definizione chiavi
007900090303     C*
008000090303     C* TITAS30C - Parziale
008100090303     C     KEYtas30_P    KLIST
008200090303     C                   KFLD                    DSCAAS
008300090303     C                   KFLD                    DSCLNP
008400090303     C                   KFLD                    DSCNRS
008500090303     C                   KFLD                    DSCNSP
008600090303     C*
008700090303     C* Data di elaborazione
008800090303     C                   time                    w0140            14 0
008900090303     C                   move      w0140         w0080             8 0          *DATA (8) IN G/M/AA
009000090303     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
009100090303     C     *eur          movel     w0080         dataeur
009200090303     C                   movel     dataeur       dataiso
009300090303     C     *iso          movel     dataiso       datcor            8 0
009400991027     C*
009500991027     C                   ENDSR
