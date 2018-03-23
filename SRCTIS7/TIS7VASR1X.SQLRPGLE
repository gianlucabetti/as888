000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200161202     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300890921     H*----------------------------------------------------*
000400170126     Ftivgd00f  O    E             DISK    commit(i§7VASCMTO)
000500161207     F                                     usropn
000600170126     Ftivgs10f  O    E             DISK    commit(i§7VASCMTO)
000700161207     F                                     usropn
000800161202     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000900161202     F                                     prefix(f_)
001000161202     F                                     usropn
001100161207
001200161207     D*--------------------
001300161207     D* DS DI PROCEDURA
001400161207     D*--------------------
001500170126     D tis7vasds     E DS
001600161202
001700161207     D*--------------------
001800161207     D* PROGRAM STATUS DS
001900161207     D*--------------------
002000161206     D psds           sds
002100161207     D  §procname        *PROC
002200161207     D  §JOB_NAME            244    253
002300161207     D  §USER                254    263
002400161207     D  §JOB_NUM             264    269
002500100705
002600161207     D*--------------------
002700161207     D* SCHIERA STACK CHIAMATA
002800161207     D*--------------------
002900161207     D DSStack         ds
003000161207     D  £Stack                       10    dim(100)
003100161207
003200010402
003300161202     C*
003400170126     C* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
003500161202     C
003600161202     C/EXEC SQL
003700161202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
003800161202     C/END-EXEC
003900161202     C*
004000161202     C* Inizializzo parametri di output
004100170126     C                   movel     '0'           o§7VASOK
004200161202     C*
004300161202     C* Monitoro l'intero flusso del *pgm
004400161202     C                   monitor
004500161206     C*
004600161206     C* Verifica parametri di input
004700161206     C                   exsr      chkpar
004800161202     C*
004900161202     C* In base al opzione richiesta eseguo specifiche operazioni
005000161202     C                   select
005100161206     C*
005200161206     C* - Calcolo progressivo univoco intera transazione VGD - inizio
005300170127     C                   when      i§7VASOPZ = 'PRG'
005400161207     C                   exsr      PRGVGD
005500161206     C*
005600170126     C* - Rilascio transazione VGD - fine
005700170127     C                   when      i§7VASOPZ = 'RLS'
005800161207     C                   exsr      RLSVGD
005900161206     C*
006000161206     C* - Inserimento recod "trigger" VGD
006100170127     C                   when      i§7VASOPZ = 'TRG'
006200170126     C                   exsr      TRGVGD
006300161207     C*
006400161207     C* - Inserimento "semaforo" VGD
006500170127     C                   when      i§7VASOPZ = 'SEM'
006600170127     C                   exsr      SEMVGDINS
006700170127     C*
006800170127     C* - Eliminazione "semaforo" VGD
006900170127     C                   when      i§7VASOPZ = 'DEL'
007000170127     C                   exsr      SEMVGDDEL
007100161207     C*
007200161202     C                   endsl
007300161202     C*
007400161202     C* Gestisco eventuale errore
007500161202     C                   on-error
007600161207     C   55              rolbk(e)
007700161205     C                   dump(A)
007800170126     C                   movel     '0'           o§7VASOK
007900161202     C*
008000161202     C* Interrompo monitoraggio errori
008100161202     C                   endmon
008200161202     C*
008300161207     C                   seton                                        lr
008400161202     C*-----------------------------------------------------------------------------
008500161206
008600161206
008700161206
008800161206     C*-----------------------------------------------------------------------------
008900161206     C* Verifica parametri di input
009000161206     C*-----------------------------------------------------------------------------
009100161206     C     CHKPAR        BEGSR
009200161207     C*
009300161207     C* Reperisco la data corrente
009400161207     C                   z-add     *zeros        datcor            8 0
009500161207     C                   eval      datcor = %dec(%date() : *iso)
009600161207     C                   z-add     *zeros        oracor            6 0
009700161207     C                   eval      oracor = %dec(%time() : *iso)
009800170126     C*
009900170126     C* Se richiesta gestione COMMIT "OPEN"
010000170126     C                   if        i§7VASCMTO = *on
010100170126     C                   else
010200170126     C                   eval      i§7VASCMTO = *off
010300170126     C                   endif
010400161207     C*
010500170126     C* Se richiesta gestione COMMIT "EXECUTE"
010600170126     C                   if        i§7VASCMTE = *on
010700170126     C                   seton                                        55
010800170126     C                   else
010900170126     C                   setoff                                       55
011000170126     C                   endif
011100161206     C*
011200161206     C* Se data non specificata in input => assumo data corrente
011300170126     C                   if        i§7VASDAT = *zeros
011400170126     C                   eval      i§7VASDAT = datcor
011500161206     C                   endif
011600161206     C*
011700161206     C* Se tipo scarico non specificato in input => assumo 'WW'
011800170126     C                   if        i§7VASTSC = *blanks
011900170126     C                   eval      i§7VASTSC = 'WW'
012000161206     C                   endif
012100161206     C*
012200161206     C* Se procedura inserimento dati non specificata in input => assumo *pgm corrente
012300170126     C                   if        i§7VASPGM = *blanks
012400170126     C                   eval      i§7VASPGM = §procname
012500161206     C                   endif
012600170127     C*
012700170127     C* Per Opz RLS se Flag Storicizzazione non specificata in input => assumo '?'
012800170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTO = *blanks
012900170127     C                   eval      i§7VASSTO = '?'
013000170127     C                   endif
013100170127     C*
013200170127     C* Per Opz RLS se Flag Storicizzazione "TO" non specificata in input => assumo 'G'
013300170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTTO = *blanks
013400170127     C                   eval      i§7VASSTTO = 'G'
013500170127     C                   endif
013600161206     C*
013700161206     C                   ENDSR
013800161206     C*------------------------------------------------------------------------*
013900161202
014000161202
014100161202
014200161202     C*-----------------------------------------------------------------------------
014300161202     C* Valorizzazione Progressivo Applicazione
014400161202     C*-----------------------------------------------------------------------------
014500161207     C     PRGVGD        BEGSR
014600161207     C*
014700161207     C* Inizio transazione
014800161207     C   55              commit(e)
014900161202     C*
015000161205     C* Inizializzazione proogressivo di output
015100170126     C                   clear                   o§7VASPRG
015200161202     C*
015300161205     C* Calcolo progressivo
015400161202     C                   movel     'RR'          dwlisv            2
015500161202     C                   movel     *all'0'       dwlprg           10
015600161202     C                   z-add     *zeros        wrkprg            8 0
015700161202     C*
015800161207     C                   open(e)   tis7prgf
015900161202     C*
016000161205     C                   read      tis7prgf
016100161202     C                   eval      dwlprg = f_tis7prgf
016200161202     C*
016300161202     C                   move(p)   dwlprg        wrkprg
016400161202     C                   add       1             wrkprg
016500161202     C                   move(p)   wrkprg        dwlprg
016600161202     C                   movel     dwlisv        dwlprg
016700161202     C*
016800161202     C                   eval      f_tis7prgf = dwlprg
016900161205     C                   update    tis7prg0
017000170126     C                   movel(p)  dwlprg        o§7VASPRG
017100170126     C                   movel     '1'           o§7VASOK
017200161202     C*
017300161207     C                   close(e)  tis7prgf
017400161202     C*
017500161202     C                   ENDSR
017600161202     C*-----------------------------------------------------------------------------
017700161202
017800161202
017900161202
018000161202     C*--------------------------------------------------------
018100161207     C* RLSVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
018200161202     C*--------------------------------------------------------
018300161207     C     RLSVGD        BEGSR
018400161202     C*
018500161202     C/EXEC SQL
018600170126 xxx C***+ UPDATE TIVGD00F SET VGDSTO = 'G'
018700170126 xxx C***+ WHERE VGDSTO = '?' AND
018800170127     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
018900170126     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
019000170126     C+       VGDTIP = :i§7VASTIP AND VGDKSU = :i§7VASKSU AND
019100170126     C+       VGDTSC = :i§7VASTSC
019200161202     C/END-EXEC
019300161202     C*
019400161205     C* Fine transazione OK
019500170126     C                   if        sqlcod >= *zeros
019600161207     C   55              commit(e)
019700170126     C                   movel     '1'           o§7VASOK
019800170126     C                   else
019900161207     C* Fine transazione KO
020000161207     C   55              rolbk(e)
020100161205     C                   dump(A)
020200170126     C                   movel     '0'           o§7VASOK
020300161205     C                   seton                                        lr
020400161202     C                   endif
020500161202     C*
020600161202     C                   ENDSR
020700161202     C***
020800161206
020900161206
021000161206
021100161206     C*-----------------------------------------------------------------------------
021200161206     C* Inserimento recod "trigger" VGD
021300161206     C*-----------------------------------------------------------------------------
021400161207     C     TRGVGD        BEGSR
021500161207     C*
021600161207     C                   open(e)   tivgd00f
021700161206     C*
021800161206     C                   clear                   tivgd000
021900170126     C                   eval      vgdDTA = '*VOID'
022000170126     C                   eval      vgdTIP = i§7VASTIP
022100170126     C                   eval      vgdKSU = i§7VASKSU
022200170126     C                   eval      vgdTSC = i§7VASTSC
022300170126     C                   eval      vgdDAT = i§7VASDAT
022400170126     C                   eval      vgdPGM = i§7VASPGM
022500170126     C                   eval      vgdPRG = i§7VASPRG
022600170126     C                   eval      vgdSTO = i§7VASSTO
022700161206     C                   write     tivgd000
022800161207     C   55              commit(e)
022900170126     C                   movel     '1'           o§7VASOK
023000161207     C*
023100161207     C                   close(e)  tivgd00f
023200161206     C*
023300161206     C                   ENDSR
023400161206     C*------------------------------------------------------------------------*
023500161207
023600161207
023700161207
023800161207     C*-----------------------------------------------------------------------------
023900161207     C* Inserimento "semaforo" VGD
024000161207     C*-----------------------------------------------------------------------------
024100170127     C     SEMVGDINS     BEGSR
024200161207     C*
024300161207     C                   clear                   tivgs010
024400161207     C*
024500161207     C                   open(e)   tivgs10f
024600161207     C*
024700161207     C* Valorizzo il record "semaforo" corrente
024800170126     C                   eval      vgsKSU  = i§7VASKSU
024900170126     C                   eval      vgsTIP  = i§7VASTIP
025000161207     C                   eval      vgsDAT  = datcor
025100161207     C                   eval      vgsORA  = oracor
025200161207     C                   eval      vgsJNAM = §JOB_NAME
025300161207     C                   eval      vgsUNAM = §USER
025400161207     C                   eval      vgsJNUM = §JOB_NUM
025500161207     C                   eval      vgsPGM  = §procname
025600161207     C*
025700161207     C* Reperisco stack di chiamata corrente
025800161207     C                   call      'XRTVSTKR'
025900161207     C                   parm                    PStack         1000
026000161207     C                   parm                    PStartPoint       3 0
026100161207     C                   parm                    PEntries          3 0
026200161207     C                   movel     Pstack        DSstack
026300161207     C*
026400161207     C* Reperisco il *pgm chiamante
026500161207     C                   eval      PStartPoint = 1
026600161207     C                   dow       PStartPoint <= PEntries
026700161207     C                   if        £Stack(PStartPoint) <> §procname
026800161207     C                   eval      vgsPGM  = £Stack(PStartPoint)
026900161207     C                   leave
027000161207     C                   endif
027100161207     C                   eval      PStartPoint = PStartPoint + 1
027200161207     C                   enddo
027300161207     C*
027400161207     C* Inserisco il "semaforo"
027500161207     C                   write     tivgs010
027600161207     C   55              commit(e)
027700170126     C                   movel     '1'           o§7VASOK
027800161207     C*
027900161207     C                   close(e)  tivgs10f
028000161207     C*
028100161207     C                   ENDSR
028200161207     C*------------------------------------------------------------------------*
028300170127
028400170127
028500170127
028600170127     C*-----------------------------------------------------------------------------
028700170127     C* Eliminazione "semaforo" VGD
028800170127     C*-----------------------------------------------------------------------------
028900170127     C     SEMVGDDEL     BEGSR
029000170127     C*
029100170127     C/EXEC SQL
029200170127     C+ DELETE FROM TIVGS10F
029300170127     C+ WHERE VGSJNAM = :§JOB_NAME AND VGSUNAM = :§USER     AND
029400170127     C+       VGSJNUM = :§JOB_NUM  AND VGSKSU  = :i§7VASKSU AND
029500170127     C+       VGSTIP = :i§7VASTIP
029600170127     C/END-EXEC
029700170127     C*
029800170127     C* Fine transazione OK
029900170127     C                   if        sqlcod >= *zeros
030000170127     C   55              commit(e)
030100170127     C                   movel     '1'           o§7VASOK
030200170127     C                   else
030300170127     C* Fine transazione KO
030400170127     C   55              rolbk(e)
030500170127     C                   dump(A)
030600170127     C                   movel     '0'           o§7VASOK
030700170127     C                   seton                                        lr
030800170127     C                   endif
030900170127     C*
031000170127     C                   ENDSR
031100170127     C*------------------------------------------------------------------------*
031200100512
031300100512
031400100512
031500090424     C*----------------------------------------------------*
031600090424     C     *INZSR        BEGSR
031700090424     C*
031800161202     C     *ENTRY        PLIST
031900170126     C                   PARM                    tis7vasds
032000100512     C*
032100100512     C                   ENDSR
032200161202
