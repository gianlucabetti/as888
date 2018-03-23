000100170201     H DECEDIT('0,') DATEDIT(*DMY.)
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
005100170201     C*
005200170201     C* - Avvio controllo sincronia
005300170201     C                   when      i§7VASOPZ = 'SCM'
005400170201     C                   exsr      STRCMT
005500170201     C*
005600170201     C* - Fine controllo sincronia
005700170201     C                   when      i§7VASOPZ = 'ECM'
005800170201     C                   exsr      ENDCMT
005900161206     C*
006000161206     C* - Calcolo progressivo univoco intera transazione VGD - inizio
006100170127     C                   when      i§7VASOPZ = 'PRG'
006200161207     C                   exsr      PRGVGD
006300161206     C*
006400170126     C* - Rilascio transazione VGD - fine
006500170127     C                   when      i§7VASOPZ = 'RLS'
006600161207     C                   exsr      RLSVGD
006700161206     C*
006800161206     C* - Inserimento recod "trigger" VGD
006900170127     C                   when      i§7VASOPZ = 'TRG'
007000170126     C                   exsr      TRGVGD
007100161207     C*
007200161207     C* - Inserimento "semaforo" VGD
007300170127     C                   when      i§7VASOPZ = 'SEM'
007400170127     C                   exsr      SEMVGDINS
007500170127     C*
007600170127     C* - Eliminazione "semaforo" VGD
007700170127     C                   when      i§7VASOPZ = 'DEL'
007800170127     C                   exsr      SEMVGDDEL
007900161207     C*
008000161202     C                   endsl
008100161202     C*
008200161202     C* Gestisco eventuale errore
008300161202     C                   on-error
008400161207     C   55              rolbk(e)
008500161205     C                   dump(A)
008600170126     C                   movel     '0'           o§7VASOK
008700161202     C*
008800161202     C* Interrompo monitoraggio errori
008900161202     C                   endmon
009000161202     C*
009100161207     C                   seton                                        lr
009200161202     C*-----------------------------------------------------------------------------
009300161206
009400161206
009500161206
009600161206     C*-----------------------------------------------------------------------------
009700161206     C* Verifica parametri di input
009800161206     C*-----------------------------------------------------------------------------
009900161206     C     CHKPAR        BEGSR
010000161207     C*
010100161207     C* Reperisco la data corrente
010200161207     C                   z-add     *zeros        datcor            8 0
010300161207     C                   eval      datcor = %dec(%date() : *iso)
010400161207     C                   z-add     *zeros        oracor            6 0
010500161207     C                   eval      oracor = %dec(%time() : *iso)
010600170126     C*
010700170126     C* Se richiesta gestione COMMIT "OPEN"
010800170126     C                   if        i§7VASCMTO = *on
010900170126     C                   else
011000170126     C                   eval      i§7VASCMTO = *off
011100170126     C                   endif
011200161207     C*
011300170126     C* Se richiesta gestione COMMIT "EXECUTE"
011400170126     C                   if        i§7VASCMTE = *on
011500170126     C                   seton                                        55
011600170126     C                   else
011700170126     C                   setoff                                       55
011800170126     C                   endif
011900161206     C*
012000161206     C* Se data non specificata in input => assumo data corrente
012100170126     C                   if        i§7VASDAT = *zeros
012200170126     C                   eval      i§7VASDAT = datcor
012300161206     C                   endif
012400161206     C*
012500161206     C* Se tipo scarico non specificato in input => assumo 'WW'
012600170126     C                   if        i§7VASTSC = *blanks
012700170126     C                   eval      i§7VASTSC = 'WW'
012800161206     C                   endif
012900161206     C*
013000161206     C* Se procedura inserimento dati non specificata in input => assumo *pgm corrente
013100170126     C                   if        i§7VASPGM = *blanks
013200170126     C                   eval      i§7VASPGM = §procname
013300161206     C                   endif
013400170127     C*
013500170127     C* Per Opz RLS se Flag Storicizzazione non specificata in input => assumo '?'
013600170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTO = *blanks
013700170127     C                   eval      i§7VASSTO = '?'
013800170127     C                   endif
013900170127     C*
014000170127     C* Per Opz RLS se Flag Storicizzazione "TO" non specificata in input => assumo 'G'
014100170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTTO = *blanks
014200170127     C                   eval      i§7VASSTTO = 'G'
014300170127     C                   endif
014400161206     C*
014500161206     C                   ENDSR
014600161206     C*------------------------------------------------------------------------*
014700161202
014800161202
014900161202
015000161202     C*-----------------------------------------------------------------------------
015100161202     C* Valorizzazione Progressivo Applicazione
015200161202     C*-----------------------------------------------------------------------------
015300161207     C     PRGVGD        BEGSR
015400161207     C*
015500161207     C* Inizio transazione
015600161207     C   55              commit(e)
015700161202     C*
015800161205     C* Inizializzazione proogressivo di output
015900170126     C                   clear                   o§7VASPRG
016000161202     C*
016100161205     C* Calcolo progressivo
016200161202     C                   movel     'RR'          dwlisv            2
016300161202     C                   movel     *all'0'       dwlprg           10
016400161202     C                   z-add     *zeros        wrkprg            8 0
016500161202     C*
016600161207     C                   open(e)   tis7prgf
016700161202     C*
016800161205     C                   read      tis7prgf
016900161202     C                   eval      dwlprg = f_tis7prgf
017000161202     C*
017100161202     C                   move(p)   dwlprg        wrkprg
017200161202     C                   add       1             wrkprg
017300161202     C                   move(p)   wrkprg        dwlprg
017400161202     C                   movel     dwlisv        dwlprg
017500161202     C*
017600161202     C                   eval      f_tis7prgf = dwlprg
017700161205     C                   update    tis7prg0
017800170126     C                   movel(p)  dwlprg        o§7VASPRG
017900170126     C                   movel     '1'           o§7VASOK
018000161202     C*
018100161207     C                   close(e)  tis7prgf
018200161202     C*
018300161202     C                   ENDSR
018400161202     C*-----------------------------------------------------------------------------
018500161202
018600161202
018700161202
018800161202     C*--------------------------------------------------------
018900161207     C* RLSVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
019000161202     C*--------------------------------------------------------
019100161207     C     RLSVGD        BEGSR
019200161202     C*
019300161202     C/EXEC SQL
019400170126 xxx C***+ UPDATE TIVGD00F SET VGDSTO = 'G'
019500170126 xxx C***+ WHERE VGDSTO = '?' AND
019600170127     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
019700170126     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
019800170126     C+       VGDTIP = :i§7VASTIP AND VGDKSU = :i§7VASKSU AND
019900170126     C+       VGDTSC = :i§7VASTSC
020000161202     C/END-EXEC
020100161202     C*
020200161205     C* Fine transazione OK
020300170126     C                   if        sqlcod >= *zeros
020400161207     C   55              commit(e)
020500170126     C                   movel     '1'           o§7VASOK
020600170126     C                   else
020700161207     C* Fine transazione KO
020800161207     C   55              rolbk(e)
020900161205     C                   dump(A)
021000170126     C                   movel     '0'           o§7VASOK
021100161205     C                   seton                                        lr
021200161202     C                   endif
021300161202     C*
021400161202     C                   ENDSR
021500161202     C***
021600161206
021700161206
021800161206
021900161206     C*-----------------------------------------------------------------------------
022000161206     C* Inserimento recod "trigger" VGD
022100161206     C*-----------------------------------------------------------------------------
022200161207     C     TRGVGD        BEGSR
022300161207     C*
022400161207     C                   open(e)   tivgd00f
022500161206     C*
022600161206     C                   clear                   tivgd000
022700170126     C                   eval      vgdDTA = '*VOID'
022800170126     C                   eval      vgdTIP = i§7VASTIP
022900170126     C                   eval      vgdKSU = i§7VASKSU
023000170126     C                   eval      vgdTSC = i§7VASTSC
023100170126     C                   eval      vgdDAT = i§7VASDAT
023200170126     C                   eval      vgdPGM = i§7VASPGM
023300170126     C                   eval      vgdPRG = i§7VASPRG
023400170126     C                   eval      vgdSTO = i§7VASSTO
023500161206     C                   write     tivgd000
023600161207     C   55              commit(e)
023700170126     C                   movel     '1'           o§7VASOK
023800161207     C*
023900161207     C                   close(e)  tivgd00f
024000161206     C*
024100161206     C                   ENDSR
024200161206     C*------------------------------------------------------------------------*
024300161207
024400161207
024500161207
024600161207     C*-----------------------------------------------------------------------------
024700161207     C* Inserimento "semaforo" VGD
024800161207     C*-----------------------------------------------------------------------------
024900170127     C     SEMVGDINS     BEGSR
025000161207     C*
025100161207     C                   clear                   tivgs010
025200161207     C*
025300161207     C                   open(e)   tivgs10f
025400161207     C*
025500161207     C* Valorizzo il record "semaforo" corrente
025600170126     C                   eval      vgsKSU  = i§7VASKSU
025700170126     C                   eval      vgsTIP  = i§7VASTIP
025800161207     C                   eval      vgsDAT  = datcor
025900161207     C                   eval      vgsORA  = oracor
026000161207     C                   eval      vgsJNAM = §JOB_NAME
026100161207     C                   eval      vgsUNAM = §USER
026200161207     C                   eval      vgsJNUM = §JOB_NUM
026300161207     C                   eval      vgsPGM  = §procname
026400161207     C*
026500161207     C* Reperisco stack di chiamata corrente
026600161207     C                   call      'XRTVSTKR'
026700161207     C                   parm                    PStack         1000
026800161207     C                   parm                    PStartPoint       3 0
026900161207     C                   parm                    PEntries          3 0
027000161207     C                   movel     Pstack        DSstack
027100161207     C*
027200161207     C* Reperisco il *pgm chiamante
027300161207     C                   eval      PStartPoint = 1
027400161207     C                   dow       PStartPoint <= PEntries
027500161207     C                   if        £Stack(PStartPoint) <> §procname
027600161207     C                   eval      vgsPGM  = £Stack(PStartPoint)
027700161207     C                   leave
027800161207     C                   endif
027900161207     C                   eval      PStartPoint = PStartPoint + 1
028000161207     C                   enddo
028100161207     C*
028200161207     C* Inserisco il "semaforo"
028300161207     C                   write     tivgs010
028400161207     C   55              commit(e)
028500170126     C                   movel     '1'           o§7VASOK
028600161207     C*
028700161207     C                   close(e)  tivgs10f
028800161207     C*
028900161207     C                   ENDSR
029000161207     C*------------------------------------------------------------------------*
029100170127
029200170127
029300170127
029400170127     C*-----------------------------------------------------------------------------
029500170127     C* Eliminazione "semaforo" VGD
029600170127     C*-----------------------------------------------------------------------------
029700170127     C     SEMVGDDEL     BEGSR
029800170127     C*
029900170127     C/EXEC SQL
030000170127     C+ DELETE FROM TIVGS10F
030100170127     C+ WHERE VGSJNAM = :§JOB_NAME AND VGSUNAM = :§USER     AND
030200170127     C+       VGSJNUM = :§JOB_NUM  AND VGSKSU  = :i§7VASKSU AND
030300170127     C+       VGSTIP = :i§7VASTIP
030400170127     C/END-EXEC
030500170127     C*
030600170127     C* Fine transazione OK
030700170127     C                   if        sqlcod >= *zeros
030800170127     C   55              commit(e)
030900170127     C                   movel     '1'           o§7VASOK
031000170127     C                   else
031100170127     C* Fine transazione KO
031200170127     C   55              rolbk(e)
031300170127     C                   dump(A)
031400170127     C                   movel     '0'           o§7VASOK
031500170127     C                   seton                                        lr
031600170127     C                   endif
031700170127     C*
031800170127     C                   ENDSR
031900170127     C*------------------------------------------------------------------------*
032000170201
032100170201
032200170201
032300170201     C*-----------------------------------------------------------------------------
032400170201     C* Avvio controllo sincronia
032500170201     C*-----------------------------------------------------------------------------
032600170201     C     STRCMT        BEGSR
032700170201     C*
032800170201     C* Reperisco stack di chiamata corrente
032900170201     C                   call(e)   'TIS7VASC1'
033000170201     C                   parm      'SCM'         pOPZ              3
033100170201     C                   parm      '*ACTGRP'     pPARAM           10
033200170201     C*
033300170201     C                   if        not %error
033400170201     C                   movel     '1'           o§7VASOK
033500170201     C                   else
033600170201     C                   movel     '0'           o§7VASOK
033700170201     C                   endif
033800170201     C*
033900170201     C                   ENDSR
034000170201     C*------------------------------------------------------------------------*
034100170201
034200170201
034300170201
034400170201     C*-----------------------------------------------------------------------------
034500170201     C* Fine controllo sincronia
034600170201     C*-----------------------------------------------------------------------------
034700170201     C     ENDCMT        BEGSR
034800170201     C*
034900170201     C* Reperisco stack di chiamata corrente
035000170201     C                   call(e)   'TIS7VASC1'
035100170201     C                   parm      'ECM'         pOPZ              3
035200170201     C                   parm      *blanks       pPARAM           10
035300170201     C*
035400170201     C                   if        not %error
035500170201     C                   movel     '1'           o§7VASOK
035600170201     C                   else
035700170201     C                   movel     '0'           o§7VASOK
035800170201     C                   endif
035900170201     C*
036000170201     C                   ENDSR
036100170201     C*------------------------------------------------------------------------*
036200100512
036300100512
036400100512
036500090424     C*----------------------------------------------------*
036600090424     C     *INZSR        BEGSR
036700090424     C*
036800161202     C     *ENTRY        PLIST
036900170126     C                   PARM                    tis7vasds
037000100512     C*
037100100512     C                   ENDSR
037200161202
