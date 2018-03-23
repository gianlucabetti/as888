000100161205     H DECEDIT('0,') DATEDIT(*DMY.)
000200161205     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300170327
000400000000
000500160906     D*****
000600160906     D* Variabili di work
000700160906     D*****
000800170327     D  TIVGDDS      e ds                  extname(tivgd00f) inz
000900170323     D  TIS7VASDS    e ds
001000000000
001100160906
001200160906     D* DS di work per lettura recordset SQL (C1)
001300000000     D  dsSQLVGD       ds
001400170327     D   SQL_STO                           like(vgdsto)
001500000000     D   SQL_KSU                           like(vgdksu)
001600170327     D   SQL_TIP                           like(vgdtip)
001700170327     D   SQL_PRG                           like(vgdprg)
001800170405     D   SQL_TSC                           like(vgdtsc)
001900170314
002000170314
002100170314     D* Schiera di work per momorizzazione dati recordset SQL (C1)
002200170327     D  skVGD          ds                  likeds(dsSQLVGD) dim(500)
002300170327     D  jVGD           s              3S 0 inz
002400170327     D  sVGD           s              3S 0 inz
002500000000
002600000000
002700000000     C*-----------------------------------------------------------------------------
002800000000     C*
002900000000     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003000000000     C
003100000000     C/EXEC SQL
003200000000     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
003300000000     C/END-EXEC
003400000000     C*
003500160906     C* Monitoro l'intero flusso del *pgm
003600160906     C                   monitor
003700000000     C*
003800000000     C/EXEC SQL
003900170315     C+ DECLARE C1 CURSOR FOR
004000170405     C+ SELECT DISTINCT VGDSTO, VGDKSU, VGDTIP, VGDPRG, VGDTSC FROM TIVGD00F
004100170327     C+ WHERE VGDSTO IN ('G', 'L', 'R') AND
004200170327     C+       (VGDPRG = ' ' OR VGDPRG LIKE 'XX%') AND
004300170405     C+       VGDTSC <> '  ' AND
004400170222     C+       VGDKSU NOT IN
004500170222     C+          (SELECT VGSKSU FROM TIVGS10F WHERE VGSTIP=' ') AND
004600170227     C+       VGDTIP NOT IN
004700170227     C+          (SELECT VGSTIP FROM TIVGS10F WHERE VGSKSU=' ') AND
004800170222     C+       (VGDKSU, VGDTIP) NOT IN
004900170222     C+          (SELECT VGSKSU, VGSTIP FROM TIVGS10F)
005000000000     C+ FOR READ ONLY
005100000000     C/END-EXEC
005200000000     C
005300000000     C/EXEC SQL
005400000000     C+ OPEN C1
005500000000     C/END-EXEC
005600000000     C
005700000000     C/EXEC SQL
005800000000     C+ FETCH C1 INTO :dsSQLVGD
005900000000     C/END-EXEC
006000170314     C
006100000000     C*
006200170314     C* Inizializzo schiera di work per momorizzazione dati recordset SQL (C1)
006300170314     C                   clear                   skVGD
006400170314     C                   clear                   jVGD
006500170314     C*
006600170314     C* Schiera di work per momorizzazione dati recordset SQL (C1)
006700170315     C                   dow       sqlcod = *zeros AND
006800170329     C                             jVGD <  %elem(skVGD)
006900170314     C                   add       1             jVGD
007000170314     C                   eval      skVGD(jVGD) = dsSQLVGD
007100170314     C*
007200170314     C/EXEC SQL
007300170314     C+ FETCH C1 INTO :dsSQLVGD
007400170314     C/END-EXEC
007500170314     C                   enddo
007600170314     C*
007700170314     C/EXEC SQL
007800170314     C+ CLOSE C1
007900170314     C/END-EXEC
008000170314     C
008100170323     C*
008200170323     C* Ricalcolo Progressivo univoco download per valori particolari
008300170327     C                   exsr      ReCalcPrg
008400160906     C*
008500160906     C* Gestisco eventuale errore
008600160906     C                   on-error
008700160906     C                   eval      prmesito = '2'
008800171003     C                   dump(A)
008900160906     C*
009000160906     C* Interrompo monitoraggio errori
009100160906     C                   endmon
009200000000     C*
009300000000     C                   seton                                        lr
009400000000     C*-----------------------------------------------------------------------------
009500170327
009600170323
009700170323
009800170327      /TITLE Ricalcola Progressivo univoco transazione download
009900170327     C     ReCalcPrg     begsr
010000170323     C*
010100170323     C                   z-add     1             sVGD
010200170323     C                   dow       sVGD <= jVGD
010300170323     C                   eval      dsSQLVGD = skVGD(sVGD)
010400170327     C*
010500170327     C* Gestione ENDJOB.
010600170327     C                   shtdn                                        60
010700170327     C                   if        not *in60
010800170327     C*
010900170327     C* Stacco progressivo univoco download
011000170327     C                   CLEAR                   TIS7VASDS
011100170327     C                   EVAL      i§7VASOPZ = 'PRG'
011200170327     C                   CALL(e)   'TIS7VASR1'
011300170327     C                   PARM                    TIS7VASDS
011400170327     C*
011500170327     C                   if        not %error AND
011600170327     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
011700170327     C
011800170327     C/EXEC SQL
011900170327     C+ UPDATE TIVGD00F SET VGDPRG = :o§7VASPRG
012000170327     C+ WHERE VGDSTO = :SQL_STO AND VGDPRG = :SQL_PRG AND
012100170327     C+       VGDTIP = :SQL_TIP AND VGDKSU = :SQL_KSU AND
012200170405     C+       VGDTSC = :SQL_TSC
012300170327     C+ WITH CS
012400170327     C/END-EXEC
012500170327     C
012600170327     C/EXEC SQL
012700170327     C+ COMMIT
012800170327     C/END-EXEC
012900170327     C
013000170327     C                   endif
013100170327     C*
013200170327     C* Gestisco lo shutdown
013300170327     C                   else
013400170327     C                   eval      prmesito = '2'
013500170327     C                   leave
013600170327     C                   endif
013700170323     C*
013800170323     C                   add       1             sVGD
013900170323     C                   enddo
014000170323     C*
014100170323     C                   endsr
014200000000
014300000000
014400000000
014500000000      /TITLE Routine di *init PGM
014600000000     C*
014700000000     C     *inzsr        begsr
014800000000     C*
014900000000     C* Passaggio parametri
015000000000     C     *entry        plist
015100000000     C                   parm                    prmesito          1
015200160906     C*
015300000000     C                   endsr
015400160906     C*
