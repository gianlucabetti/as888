000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090123     Ftivgdtmp  uF   E             DISK
000500091222     Ftivawwwt  uf a E             DISK
000600100420     Ftncsb03l  iF   E           K DISK
000700991027
000800090203     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090123
001300100420     D  wCharCSV       s              1a   inz
001400100420     D  wCAS           s              9s 2 inz
001500100420     D  wTIC           s              3a   inz
001600090203
001700991027
001800100420     C*
001900100420     C* Verifico parametri in ingresso
002000100420     C                   EVAL      wCharCSV = %subst(prmppt:1:1)
002100100420     C                   IF        wCharCSV = *blanks
002200100420     C                   EVAL      wCharCSV = ','
002300100420     C                   ENDIF
002400100420     C*
002500991027     C                   EXSR      traduci
002600921023     C                   SETON                                        LR
002700030908
002800090203
002900090123
003000991027     C     traduci       BEGSR
003100991027     C*
003200090203     C* Se tutto ok => procedo
003300090203     C                   IF        wrkesito = *blanks
003400100420     C*
003500100420     C* Prima riga d intestazione
003600100420     C                   exsr      wriHead
003700100420     C*
003800090123     C                   READ      tivgdtmp
003900090203     C                   movel     vgddta        tnvasds
004000090203     C*
004100090123     C                   DOW       not %eof(tivgdtmp)
004200100420     C*
004300100420     C* Aggancio quindi il contrassegno relativo alla bolla originale
004400100420     C     KEYcsb03_C    chain     tncsb03l
004500100420     C                   if        %found(tncsb03l)
004600090203     C*
004700090203     C* Valorizzo il record d dettaglio corrente
004800090203     C                   exsr      wriBody
004900091222     C*
005000091222     C                   endif
005100930409     C*
005200090123     C                   DELETE    tivgd000
005300090123     C                   READ      tivgdtmp
005400090203     C                   movel     vgddta        tnvasds
005500090203     C*
005600030325     C                   ENDDO
005700100420     C*
005800100420     C* Ultima riga d piede
005900100420     C                   exsr      wriFoot
006000090203     C*
006100090203     C                   EVAL      wrkesito = '0'
006200090203     C*
006300090203     C                   ENDIF
006400991027     C*
006500910830     C                   ENDSR
006600100420
006700100420
006800100420
006900100420     C     wriHead       BEGSR
007000100420     C*
007100100420     C                   eval      vawdta =
007200100420     C                                      '"'+'data inizio'+'"'              +
007300100420     C                                      wCharCSV                           +
007400100420     C                                      '"'+%char(%dec(%date() : *ISO))+'"'+
007500100420     C                                      wCharCSV                           +
007600100420     C                                      '"'+'contrassegno'+'"'
007700100420     C*
007800100420     C                   write     tivaw000
007900100420     C*
008000100420     C                   ENDSR
008100100420
008200100420
008300100420
008400100420     C     wriFoot       BEGSR
008500100420     C*
008600100420     C                   eval      vawdta =
008700100420     C                                      '"'+'data fine'+'"'                +
008800100420     C                                      wCharCSV                           +
008900100420     C                                      '"'+%char(%dec(%date() : *ISO))+'"'
009000100420     C*
009100100420     C                   write     tivaw000
009200100420     C*
009300100420     C                   ENDSR
009400090203
009500091222
009600091222
009700090203     C     wriBody       BEGSR
009800100420     C*
009900100420     C* Considerazioni personalizzate
010000100420     C                   z-add     vasCAS        wCAS
010100100420     C                   select
010200100420     C                   when      csbTPA + csbTPI = 'B'
010300100420     C                   eval      wTIC = 'ABS'
010400100420     C                   when      csbTPA + csbTPI = 'BM'
010500100420     C                   eval      wTIC = 'ABM'
010600100420     C                   when      csbTPA + csbTPI = 'CM'
010700100420     C                   eval      wTIC = 'ACM'
010800100420     C                   when      csbTPA + csbTPI = *blanks
010900100420     C                   eval      wTIC = 'CON'
011000100420     C                   other
011100100420     C                   eval      wTIC = '***'
011200100420     C                   endsl
011300100420     C*
011400100420     C                   eval      vawdta =
011500100420     C                               '"'+%trim(vasRMA)+'"'                     +
011600100420     C                               wCharCSV                                  +
011700100420     C                               '"'+%char(vasDDP)+'"'                     +
011800100420     C                               wCharCSV                                  +
011900100420     C                               '"'+%trim(%editw(wCAS:'0       .  '))+'"' +
012000100420     C                               wCharCSV                                  +
012100100420     C                               '"'+wTIC+'"'
012200090203     C*
012300090205     C                   write     tivaw000
012400090203     C*
012500090203     C                   ENDSR
012600090203
012700090203
012800090203
012900090123
013000991027      /TITLE Operazioni iniziali.
013100991027     C     *inzsr        BEGSR
013200991027     C*
013300991027     C     *ENTRY        PLIST
013400991027     C                   parm                    prmppt
013500991027     C     wrkesito      parm      wrkesito      prmesito
013600091222     C*
013700091222     C****
013800091222     C* Definizione chiavi
013900091222     C****
014000100420     C*
014100100420     C* Chiave su TNCSB03L - Completa
014200100420     C     KEYcsb03_C    KLIST
014300100420     C                   KFLD                    vasAAS
014400100420     C                   KFLD                    vasLNP
014500100420     C                   KFLD                    vasNRS
014600100420     C                   KFLD                    vasNSP
014700991027     C*
014800991027     C                   ENDSR
