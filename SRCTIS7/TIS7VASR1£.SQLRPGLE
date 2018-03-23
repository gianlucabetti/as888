000100170201     H DECEDIT('0,') DATEDIT(*DMY.)
000200161202     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300890921     H*----------------------------------------------------*
000400170126     Ftivgd00f  O    E             DISK    commit(i§7VASCMTO)
000500161207     F                                     usropn
000600170126     Ftivgs10f  O    E             DISK    commit(i§7VASCMTO)
000700161207     F                                     usropn
000800161202     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000900161202     F                                     prefix(f_)
001000161207
001100161207     D*--------------------
001200161207     D* DS DI PROCEDURA
001300161207     D*--------------------
001400170126     D tis7vasds     E DS
001500161202
001600161207     D*--------------------
001700161207     D* PROGRAM STATUS DS
001800161207     D*--------------------
001900161206     D psds           sds
002000161207     D  §procname        *PROC
002100161207     D  §JOB_NAME            244    253
002200161207     D  §USER                254    263
002300161207     D  §JOB_NUM             264    269
002400100705
002500161207     D*--------------------
002600161207     D* SCHIERA STACK CHIAMATA
002700161207     D*--------------------
002800161207     D DSStack         ds
002900161207     D  £Stack                       10    dim(100)
003000161207
003100010402
003200161202     C*
003300170126     C* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
003400161202     C
003500161202     C/EXEC SQL
003600161202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
003700161202     C/END-EXEC
003800161202     C*
003900161202     C* Inizializzo parametri di output
004000170126     C                   movel     '0'           o§7VASOK
004100161202     C*
004200161202     C* Monitoro l'intero flusso del *pgm
004300161202     C                   monitor
004400161206     C*
004500161206     C* Verifica parametri di input
004600161206     C                   exsr      chkpar
004700161202     C*
004800161202     C* In base al opzione richiesta eseguo specifiche operazioni
004900161202     C                   select
005000170201     C*
005100170201     C* - Avvio controllo sincronia
005200170201     C                   when      i§7VASOPZ = 'SCM'
005300170201     C                   exsr      STRCMT
005400170201     C*
005500170201     C* - Fine controllo sincronia
005600170201     C                   when      i§7VASOPZ = 'ECM'
005700170201     C                   exsr      ENDCMT
005800161206     C*
005900161206     C* - Calcolo progressivo univoco intera transazione VGD - inizio
006000170127     C                   when      i§7VASOPZ = 'PRG'
006100161207     C                   exsr      PRGVGD
006200161206     C*
006300170126     C* - Rilascio transazione VGD - fine
006400170127     C                   when      i§7VASOPZ = 'RLS'
006500161207     C                   exsr      RLSVGD
006600170321     C*
006700170321     C* - Rilascio transazione VGD (solo TIP) - fine
006800170321     C                   when      i§7VASOPZ = 'RLT'
006900170321     C                   exsr      RLTVGD
007000161206     C*
007100161206     C* - Inserimento recod "trigger" VGD
007200170127     C                   when      i§7VASOPZ = 'TRG'
007300170126     C                   exsr      TRGVGD
007400161207     C*
007500161207     C* - Inserimento "semaforo" VGD
007600170127     C                   when      i§7VASOPZ = 'SEM'
007700170127     C                   exsr      SEMVGDINS
007800170127     C*
007900170127     C* - Eliminazione "semaforo" VGD
008000170127     C                   when      i§7VASOPZ = 'DEL'
008100170127     C                   exsr      SEMVGDDEL
008200161207     C*
008300161202     C                   endsl
008400161202     C*
008500161202     C* Gestisco eventuale errore
008600161202     C                   on-error
008700161207     C   55              rolbk(e)
008800161205     C                   dump(A)
008900170126     C                   movel     '0'           o§7VASOK
009000161202     C*
009100161202     C* Interrompo monitoraggio errori
009200161202     C                   endmon
009300161202     C*
009400161207     C                   seton                                        lr
009500161202     C*-----------------------------------------------------------------------------
009600161206
009700161206
009800161206
009900161206     C*-----------------------------------------------------------------------------
010000161206     C* Verifica parametri di input
010100161206     C*-----------------------------------------------------------------------------
010200161206     C     CHKPAR        BEGSR
010300161207     C*
010400161207     C* Reperisco la data corrente
010500161207     C                   z-add     *zeros        datcor            8 0
010600161207     C                   eval      datcor = %dec(%date() : *iso)
010700161207     C                   z-add     *zeros        oracor            6 0
010800161207     C                   eval      oracor = %dec(%time() : *iso)
010900170126     C*
011000170126     C* Se richiesta gestione COMMIT "OPEN"
011100170126     C                   if        i§7VASCMTO = *on
011200170126     C                   else
011300170126     C                   eval      i§7VASCMTO = *off
011400170126     C                   endif
011500170228     C*
011600170228     C* Se non espressamente indicato NO COMMIT => assumo SI COMMIT
011700170228     C                   if        i§7VASCMTE <> '0'
011800170228     C                   eval      i§7VASCMTE  = '1'
011900170228     C                   endif
012000161207     C*
012100170126     C* Se richiesta gestione COMMIT "EXECUTE"
012200170126     C                   if        i§7VASCMTE = *on
012300170126     C                   seton                                        55
012400170126     C                   else
012500170126     C                   setoff                                       55
012600170126     C                   endif
012700161206     C*
012800161206     C* Se data non specificata in input => assumo data corrente
012900170126     C                   if        i§7VASDAT = *zeros
013000170126     C                   eval      i§7VASDAT = datcor
013100161206     C                   endif
013200161206     C*
013300161206     C* Se tipo scarico non specificato in input => assumo 'WW'
013400170126     C                   if        i§7VASTSC = *blanks
013500170126     C                   eval      i§7VASTSC = 'WW'
013600161206     C                   endif
013700161206     C*
013800161206     C* Se procedura inserimento dati non specificata in input => assumo *pgm corrente
013900170126     C                   if        i§7VASPGM = *blanks
014000170126     C                   eval      i§7VASPGM = §procname
014100161206     C                   endif
014200170127     C*
014300170127     C* Per Opz RLS se Flag Storicizzazione non specificata in input => assumo '?'
014400170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTO = *blanks
014500170127     C                   eval      i§7VASSTO = '?'
014600170127     C                   endif
014700170127     C*
014800170127     C* Per Opz RLS se Flag Storicizzazione "TO" non specificata in input => assumo 'G'
014900170127     C                   if        i§7VASOPZ = 'RLS' AND i§7VASSTTO = *blanks
015000170127     C                   eval      i§7VASSTTO = 'G'
015100170127     C                   endif
015200161206     C*
015300161206     C                   ENDSR
015400161206     C*------------------------------------------------------------------------*
015500161202
015600161202
015700161202
015800161202     C*-----------------------------------------------------------------------------
015900161202     C* Valorizzazione Progressivo Applicazione
016000161202     C*-----------------------------------------------------------------------------
016100161207     C     PRGVGD        BEGSR
016200161207     C*
016300161207     C* Inizio transazione
016400161207     C   55              commit(e)
016500161202     C*
016600161205     C* Inizializzazione proogressivo di output
016700170126     C                   clear                   o§7VASPRG
016800161202     C*
016900161205     C* Calcolo progressivo
017000170324     C                   movel     'ID'          dwlisv            2
017100161202     C                   movel     *all'0'       dwlprg           10
017200161202     C                   z-add     *zeros        wrkprg            8 0
017300161202     C*
017400170324     C     *start        setll     tis7prgf
017500170324     C                   read(e)   tis7prgf
017600170324     C                   if        not %error
017700161202     C                   eval      dwlprg = f_tis7prgf
017800161202     C*
017900161202     C                   move(p)   dwlprg        wrkprg
018000161202     C                   add       1             wrkprg
018100161202     C                   move(p)   wrkprg        dwlprg
018200161202     C                   movel     dwlisv        dwlprg
018300161202     C*
018400161202     C                   eval      f_tis7prgf = dwlprg
018500161205     C                   update    tis7prg0
018600170126     C                   movel(p)  dwlprg        o§7VASPRG
018700170126     C                   movel     '1'           o§7VASOK
018800170324     C                   endif
018900161202     C*
019000161202     C                   ENDSR
019100161202     C*-----------------------------------------------------------------------------
019200161202
019300161202
019400161202
019500161202     C*--------------------------------------------------------
019600161207     C* RLSVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
019700161202     C*--------------------------------------------------------
019800161207     C     RLSVGD        BEGSR
019900161202     C*
020000161202     C/EXEC SQL
020100170127     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
020200170126     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
020300170126     C+       VGDTIP = :i§7VASTIP AND VGDKSU = :i§7VASKSU AND
020400170126     C+       VGDTSC = :i§7VASTSC
020500161202     C/END-EXEC
020600161202     C*
020700161205     C* Fine transazione OK
020800170126     C                   if        sqlcod >= *zeros
020900161207     C   55              commit(e)
021000170126     C                   movel     '1'           o§7VASOK
021100170126     C                   else
021200161207     C* Fine transazione KO
021300161207     C   55              rolbk(e)
021400161205     C                   dump(A)
021500170126     C                   movel     '0'           o§7VASOK
021600161205     C                   seton                                        lr
021700161202     C                   endif
021800161202     C*
021900161202     C                   ENDSR
022000161202     C***
022100170321
022200170321
022300170321
022400170321     C*--------------------------------------------------------
022500170321     C* RLTVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
022600170321     C*--------------------------------------------------------
022700170321     C     RLTVGD        BEGSR
022800170321     C*
022900170321     C/EXEC SQL
023000170321     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
023100170321     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
023200170321     C+       VGDTIP = :i§7VASTIP
023300170321     C/END-EXEC
023400170321     C*
023500170321     C* Fine transazione OK
023600170321     C                   if        sqlcod >= *zeros
023700170321     C   55              commit(e)
023800170321     C                   movel     '1'           o§7VASOK
023900170321     C                   else
024000170321     C* Fine transazione KO
024100170321     C   55              rolbk(e)
024200170321     C                   dump(A)
024300170321     C                   movel     '0'           o§7VASOK
024400170321     C                   seton                                        lr
024500170321     C                   endif
024600170321     C*
024700170321     C                   ENDSR
024800170321     C***
024900161206
025000161206
025100161206
025200161206     C*-----------------------------------------------------------------------------
025300161206     C* Inserimento recod "trigger" VGD
025400161206     C*-----------------------------------------------------------------------------
025500161207     C     TRGVGD        BEGSR
025600161207     C*
025700161207     C                   open(e)   tivgd00f
025800161206     C*
025900161206     C                   clear                   tivgd000
026000170126     C                   eval      vgdDTA = '*VOID'
026100170126     C                   eval      vgdTIP = i§7VASTIP
026200170126     C                   eval      vgdKSU = i§7VASKSU
026300170126     C                   eval      vgdTSC = i§7VASTSC
026400170126     C                   eval      vgdDAT = i§7VASDAT
026500170126     C                   eval      vgdPGM = i§7VASPGM
026600170126     C                   eval      vgdPRG = i§7VASPRG
026700170126     C                   eval      vgdSTO = i§7VASSTO
026800161206     C                   write     tivgd000
026900161207     C   55              commit(e)
027000170126     C                   movel     '1'           o§7VASOK
027100161207     C*
027200161207     C                   close(e)  tivgd00f
027300161206     C*
027400161206     C                   ENDSR
027500161206     C*------------------------------------------------------------------------*
027600161207
027700161207
027800161207
027900161207     C*-----------------------------------------------------------------------------
028000161207     C* Inserimento "semaforo" VGD
028100161207     C*-----------------------------------------------------------------------------
028200170127     C     SEMVGDINS     BEGSR
028300161207     C*
028400161207     C                   clear                   tivgs010
028500161207     C*
028600161207     C                   open(e)   tivgs10f
028700161207     C*
028800161207     C* Valorizzo il record "semaforo" corrente
028900170126     C                   eval      vgsKSU  = i§7VASKSU
029000170126     C                   eval      vgsTIP  = i§7VASTIP
029100161207     C                   eval      vgsDAT  = datcor
029200161207     C                   eval      vgsORA  = oracor
029300161207     C                   eval      vgsJNAM = §JOB_NAME
029400161207     C                   eval      vgsUNAM = §USER
029500161207     C                   eval      vgsJNUM = §JOB_NUM
029600161207     C                   eval      vgsPGM  = §procname
029700161207     C*
029800161207     C* Reperisco stack di chiamata corrente
029900161207     C                   call      'XRTVSTKR'
030000161207     C                   parm                    PStack         1000
030100161207     C                   parm                    PStartPoint       3 0
030200161207     C                   parm                    PEntries          3 0
030300161207     C                   movel     Pstack        DSstack
030400161207     C*
030500161207     C* Reperisco il *pgm chiamante
030600161207     C                   eval      PStartPoint = 1
030700161207     C                   dow       PStartPoint <= PEntries
030800161207     C                   if        £Stack(PStartPoint) <> §procname
030900161207     C                   eval      vgsPGM  = £Stack(PStartPoint)
031000161207     C                   leave
031100161207     C                   endif
031200161207     C                   eval      PStartPoint = PStartPoint + 1
031300161207     C                   enddo
031400161207     C*
031500161207     C* Inserisco il "semaforo"
031600161207     C                   write     tivgs010
031700161207     C   55              commit(e)
031800170126     C                   movel     '1'           o§7VASOK
031900161207     C*
032000161207     C                   close(e)  tivgs10f
032100161207     C*
032200161207     C                   ENDSR
032300161207     C*------------------------------------------------------------------------*
032400170127
032500170127
032600170127
032700170127     C*-----------------------------------------------------------------------------
032800170127     C* Eliminazione "semaforo" VGD
032900170127     C*-----------------------------------------------------------------------------
033000170127     C     SEMVGDDEL     BEGSR
033100170127     C*
033200170127     C/EXEC SQL
033300170127     C+ DELETE FROM TIVGS10F
033400170127     C+ WHERE VGSJNAM = :§JOB_NAME AND VGSUNAM = :§USER     AND
033500170127     C+       VGSJNUM = :§JOB_NUM  AND VGSKSU  = :i§7VASKSU AND
033600170127     C+       VGSTIP = :i§7VASTIP
033700170127     C/END-EXEC
033800170127     C*
033900170127     C* Fine transazione OK
034000170127     C                   if        sqlcod >= *zeros
034100170127     C   55              commit(e)
034200170127     C                   movel     '1'           o§7VASOK
034300170127     C                   else
034400170127     C* Fine transazione KO
034500170127     C   55              rolbk(e)
034600170127     C                   dump(A)
034700170127     C                   movel     '0'           o§7VASOK
034800170127     C                   seton                                        lr
034900170127     C                   endif
035000170127     C*
035100170127     C                   ENDSR
035200170127     C*------------------------------------------------------------------------*
035300170201
035400170201
035500170201
035600170201     C*-----------------------------------------------------------------------------
035700170201     C* Avvio controllo sincronia
035800170201     C*-----------------------------------------------------------------------------
035900170201     C     STRCMT        BEGSR
036000170201     C*
036100170201     C* Reperisco stack di chiamata corrente
036200170201     C                   call(e)   'TIS7VASC1'
036300170201     C                   parm      'SCM'         pOPZ              3
036400170201     C                   parm      '*ACTGRP'     pPARAM           10
036500170201     C*
036600170201     C                   if        not %error
036700170201     C                   movel     '1'           o§7VASOK
036800170201     C                   else
036900170201     C                   movel     '0'           o§7VASOK
037000170201     C                   endif
037100170201     C*
037200170201     C                   ENDSR
037300170201     C*------------------------------------------------------------------------*
037400170201
037500170201
037600170201
037700170201     C*-----------------------------------------------------------------------------
037800170201     C* Fine controllo sincronia
037900170201     C*-----------------------------------------------------------------------------
038000170201     C     ENDCMT        BEGSR
038100170201     C*
038200170201     C* Reperisco stack di chiamata corrente
038300170201     C                   call(e)   'TIS7VASC1'
038400170201     C                   parm      'ECM'         pOPZ              3
038500170201     C                   parm      *blanks       pPARAM           10
038600170201     C*
038700170201     C                   if        not %error
038800170201     C                   movel     '1'           o§7VASOK
038900170201     C                   else
039000170201     C                   movel     '0'           o§7VASOK
039100170201     C                   endif
039200170201     C*
039300170201     C                   ENDSR
039400170201     C*------------------------------------------------------------------------*
039500100512
039600100512
039700100512
039800090424     C*----------------------------------------------------*
039900090424     C     *INZSR        BEGSR
040000090424     C*
040100161202     C     *ENTRY        PLIST
040200170126     C                   PARM                    tis7vasds
040300100512     C*
040400100512     C                   ENDSR
040500161202
