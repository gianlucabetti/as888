000100170201     H DECEDIT('0,') DATEDIT(*DMY.)
000200161202     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300890921     H*----------------------------------------------------*
000400170731     Ftivlr00f  if   e             disk
000500170731     Ftivav01i  uf a e           k disk
000600161207
000700170731
000800170731       // - Status
000900170731     D Psds           sds
001000170731     D   SDSpgm          *proc
001100170731     D   JobName             244    253                                         Job name
001200170731     D   JobUser             254    263                                         User name
001300170731     D   JobNumber           264    269s 0                                      Job number
001400170731
001500170731
001600170731     D TIVIRDS       E DS                  extname(tivir00f) inz
001700170731     D DVIR01        E DS                                    inz
001800170731     D tivlrnrrp       s             10s 0                   inz
001900170731     D sVIRKSC         s                   like(virKSC)      inz
002000170731     D sVIRTIP         s                   like(virTIP)      inz
002100170731     D sVIRPTH         s                   like(virPTH)      inz
002200170731     D pOutKSC         s              7a
002300170731     D pOutOPZ         s              3a
002400161207
002500010402
002600161202     C*
002700170126     C* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
002800161202     C
002900161202     C/EXEC SQL
003000170731     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003100161202     C/END-EXEC
003200170731     C*
003300170731     C* Monitoro l'intero flusso del *pgm
003400170731     C                   monitor
003500170731     C*
003600170731     C* Inizializzazioni parametri
003700170731     C                   clear                   pOutKSC
003800170731     C                   clear                   pOutOPZ
003900170731     C*
004000170731     C* Reperisco il RRN del TIVLR della transazione corrente
004100170731     C                   movel     *blanks       wCurrJob         30
004200170731     C                   eval      wCurrJob = %editc(JobNumber:'X') + '/' +
004300170731     C                                        %trim(JobUser)        + '/' +
004400170731     C                                        %trim(JobName)
004500170731     C*
004600170731     C* Aggancio il record di work relativo al RRN del TIVLR
004700170731     C                   eval      vavKEY1 = 'VASUPL'
004800170731     C                   eval      vavKEY2 = wCurrJob
004900170731     C                   eval      vavKEY3 = 'TIVLR00F_RRN'
005000170731     C     KEYvav01_C    chain     tivav01i
005100170731     C                   if        %found(tivav01i)
005200170731     C                   eval      tivlrnrrp = %dec(%trim(vavDTA):10:0)
005300170731     C*
005400170731     C* Reperisco la KEY Cli/TipoFile della transazione corrente
005500170731     C     tivlrnrrp     chain     tivlr00f
005600170731     C                   if        %found(tivlr00f)
005700170731     C*
005800170731     C* A questo punto aggancio l'anagrafica Cli/TipF correntemente valida
005900170731     C
006000170731     C*
006100170731     C/EXEC SQL
006200170731     C+ declare C_TIVIR cursor for
006300170731     C+ select virKSC, virPTH from tivir00f
006400170731     C+ where virKSC = :vlrKSC and virTIP = :vlrTIP and
006500170731     C+ decimal(CURRENT_DATE, 8, 0) between virDTI and virDTF
006600170731     C+ for read only
006700170731     C/END-EXEC
006800170731     C
006900170731     C/EXEC SQL
007000170731     C+ open C_TIVIR
007100170731     C/END-EXEC
007200170731     C
007300170731     C/EXEC SQL
007400170731     C+ Fetch C_TIVIR into :sVIRKSC, :sVIRPTH
007500170731     C/END-EXEC
007600170731     C*
007700170731     C                   if        sqlcod = *zeros
007800170731     C                   eval      DVIR01 = sVIRPTH
007900170731     C                   eval      pOutKSC = %subst(sVIRKSC:2:7)
008000170731     C                   eval      pOutOPZ = §vir01vpco
008100170731     C                   endif
008200170731     C                   endif
008300170731     C                   endif
008400161202     C*
008500161202     C* Gestisco eventuale errore
008600161202     C                   on-error
008700161205     C                   dump(A)
008800161202     C*
008900161202     C* Interrompo monitoraggio errori
009000161202     C                   endmon
009100161202     C*
009200161207     C                   seton                                        lr
009300161202     C*-----------------------------------------------------------------------------
009400100512
009500100512
009600100512
009700090424     C*----------------------------------------------------*
009800090424     C     *INZSR        BEGSR
009900090424     C*
010000161202     C     *ENTRY        PLIST
010100170731     C                   PARM                    pOutKSC
010200170731 xxx C                   PARM                    pOutOPZ
010300170731     C*
010400170731     C     KEYvav01_C    klist
010500170731     C                   kfld                    vavKEY1
010600170731     C                   kfld                    vavKEY2
010700170731     C                   kfld                    vavKEY3
010800100512     C*
010900100512     C                   ENDSR
011000161202
