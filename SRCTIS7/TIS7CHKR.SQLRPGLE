000100100204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300171127     Ftis7chkd  CF   E             WORKSTN
000400020304     dkpjba          e ds
000500080502      *
000600171127     D* DS d wrk
000700171127     D  dsSQLVGD       ds
000800171127     D   SQL_TIP                           like(vgdtip)
000900171127     D   SQL_KSU                           like(vgdksu)
001000171127     D   SQL_TSC                           like(vgdtsc)
001100171127 xxx D   SQL_PRG                           like(vgdprg)
001200000000     C*---------------------------------------------------------------*
001300020304      *
001400031105     c                   do        *hival
001500171127     c                   exfmt     s7chkd1
001600171127
001700031105     c                   clear                   $msg
001800031105     c   kc              leave
001900171127
002000031105     c                   exsr      controv1
002100031105     c   96              iter
002200050623     c                   if        *inkf
002300171127     c                   EXSR      Elabora
002400170619     c                   leave
002500050623     c                   endif
002600170616     c                   enddo
002700050802
002800020308     c                   seton                                        lr
002900031105     c*-------------------------------------------------
003000171127     c     CONTRV1       begsr
003100171127
003200171127     C                   MOVE      VIDDAT        G02DAT
003300171127     C                   MOVEL     *BLANK        G02ERR
003400171127     C                   CALL      'XSRDA8'
003500171127     C                   PARM                    WLBDA8
003600171127     C     G02ERR        IFEQ      '1'
003700171127     C                   SETON                                        7296
003800171127     c                   goto      endcontr
003900171127     C                   endif
004000171127     c                   move      g02dat        VIDDAT
004100171127     c                   move      g02inv        AMGDAT            8
004200171127     C                   endif
004300171127     C     ENDCONTR      ENDSR
004400171127     C
004500171127     c     elabora       begsr
004600171127     C*-----------------------------------------------------------------------------
004700171127     C*
004800171127     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004900171127     C
005000171127     C/EXEC SQL
005100171127     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
005200171127     C/END-EXEC
005300171127     C*
005400171127     C*
005500171127     C* In ingresso elimino subito il contenuto del file d wrk in QTEMP
005600171127     C                   z-add     50            LENGH            15 5
005700171127     C                   movel     *blanks       CMD              50
005800171127     C                   eval      CMD = 'CLRPFM QTEMP/TIVGDTMP'
005900171127     C                   call      'QCMDEXC'                                    *LANCIA CMD
006000171127     C                   parm                    CMD
006100171127     C                   parm                    LENGH
006200171127     C*
006300171127     c                   move      vidcli        cli               8
006400171127     c                   movel     '0'           cli               8
006500171127     C*
006600171127     C/EXEC SQL
006700171127     C+ DECLARE C1 CURSOR FOR
006800171127 xxx C+ SELECT DISTINCT VGDTIP, VGDKSU, VGDTSC, VGDPRG FROM TIVGD00F
006900171127     C+ AND VGDPGM  = :VIDPGMDA      AND VGDDAT=   :AMGDAT
007000171127     C+ AND VGDKSU  = :CLI
007100171127     C+ FOR READ ONLY
007200171127     C/END-EXEC
007300171127     C
007400171127     C/EXEC SQL
007500171127     C+ OPEN C1
007600171127     C/END-EXEC
007700171127     C
007800171127     C/EXEC SQL
007900171127     C+ FETCH C1 INTO :dsSQLVGD
008000171127     C/END-EXEC
008100171127     C*
008200171127     C                   dow       sqlcod = *zeros
008300171127     C*
008400171127     C*
008500171127     C* Apro il file d wrk
008600171127     C                   if        not %open(tivgdtmp)
008700171127     C                   open      tivgdtmp
008800171127     C                   endif
008900171127
009000171127     C     KEYvgd01_c    klist
009100171127     C                   kfld                    SQL_TIP
009200171127     C                   kfld                    SQL_KSU
009300171127     C                   kfld                    SQL_TSC
009400171127 xxx C                   kfld                    SQL_PRG
009500171127     C*
009600171127     C* Inizializzo flag x verifica lettura record
009700171127     C                   movel     'N'           wRecordLetti      1
009800171127     C*
009900171127     C* Leggo la vista logica del file generico download x ogni chiave d rottura traduzione
010000171127     C* ovvero TIP/KSU/TSC/PRG
010100171127     C     KEYvgd01_c    setll     tivgd01l
010200171127     C                   if        %equal(tivgd01l)
010300171127     C*
010400171127     C* 1° lettura file fuori ciclo
010500171127     C     KEYvgd01_c    reade     tivgd01l
010600171127     C*
010700171127     C* Leggo tutta la vista logica
010800171127     C                   dow       not %eof(tivgd01l)
010900171127     C*
011000171127     C* Verifico innanzitutto se richieste parzializzazionei in ingresso
011100171127     C                   movel     'S'           wRecOk
011200171127     C                   exsr      chkrec
011300171127     C*
011400171127     C*
011500171127     C* Gestisco attributi specifici a livello d record corrente
011600171127     C                   exsr      chkattrib
011700171127     C*
011800171127     C* Valorizzo flag x verifica lettura record
011900171127     C                   movel     'S'           wRecordLetti
012000171127     C*
012100171127     C* Passo i dati dal file "buono" al file d wrk in QTEMP
012200171127     C                   eval      dsVGDtmp = dsVGD
012300171127     C                   write     tivgdT
012400171127     C                   eval      vgdsto = '*'
012500171127     C                   update    tivgd000
012600171127     C                   endif
012700171127     C*
012800171127     C* Gestisco lo shutdown
012900171127     C                   else
013000171127     C                   eval      prmesito = '2'
013100171127     C                   leave
013200171127     C                   endif
013300171127     C*
013400171127     C* Proseguo con la lettura del file Vas Generico Download (TIVGD)
013500171127     C     KEYvgd01_c    reade     tivgd01l
013600171127     C                   enddo
013700171127     C*
013800171127     C* Effettuo test particolare x tipo file 'VP' x verifica abilitazione cliente
013900171127     C  N60              if        vgdtip = 'VP'
014000171127     C                   exsr      CtrlAbil
014100171127     C                   else
014200171127     C                   eval      Cli_Abil = *on
014300171127     C                   endif
014400171127     C*
014500171127     C* Effettuo la traduzione
014600171127     C  N60              if        Cli_Abil = *on
014700171127     C                   exsr      traduzione
014800171127     C                   else
014900171127     C                   exsr      elimina
015000171127     C***                exsr      storicizzaNA
015100171127     C                   endif
015200171127     C*
015300171127     C                   endif
015400171127     C*
015500171127  E 1C                   endif
015600171127     C*
015700171127     C* Verifico eventuale shutdown
015800171127     C   60              leave
015900171127     C*
016000171127     C/EXEC SQL
016100171127     C+ FETCH C1 INTO :dsSQLVGD
016200171127     C/END-EXEC
016300171127     C                   enddo
016400171127     C*
016500171127     C/EXEC SQL
016600171127     C+ CLOSE C1
016700171127     C/END-EXEC
016800171127     C
016900171127     C*
017000031105     c*-------------------------------------------------
017100171127     c                   endsr
017200011026     c*-------------------------------------------------
017300011026     c     *inzsr        begsr
017400011026     c*-------------------------------------------------
017500020304     c     *entry        plist
017600020304     c                   parm                    kpjba
017700050801
017800170802      *
017900011026     c                   endsr
