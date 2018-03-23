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
008700170327     C                   movel     '0'           o§7VASOK
008800161207     C   55              rolbk(e)
008900161205     C                   dump(A)
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
018800170327     C                   else
018900170327     C                   movel(p)  'XX00000000'  o§7VASPRG
019000170327     C                   movel     '1'           o§7VASOK
019100170324     C                   endif
019200161202     C*
019300161202     C                   ENDSR
019400161202     C*-----------------------------------------------------------------------------
019500161202
019600161202
019700161202
019800161202     C*--------------------------------------------------------
019900161207     C* RLSVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
020000161202     C*--------------------------------------------------------
020100161207     C     RLSVGD        BEGSR
020200161202     C*
020300161202     C/EXEC SQL
020400170127     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
020500170126     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
020600170126     C+       VGDTIP = :i§7VASTIP AND VGDKSU = :i§7VASKSU AND
020700170126     C+       VGDTSC = :i§7VASTSC
020800161202     C/END-EXEC
020900161202     C*
021000161205     C* Fine transazione OK
021100170126     C                   if        sqlcod >= *zeros
021200161207     C   55              commit(e)
021300170126     C                   movel     '1'           o§7VASOK
021400170126     C                   else
021500161207     C* Fine transazione KO
021600161207     C   55              rolbk(e)
021700161205     C                   dump(A)
021800170126     C                   movel     '0'           o§7VASOK
021900161205     C                   seton                                        lr
022000161202     C                   endif
022100161202     C*
022200161202     C                   ENDSR
022300161202     C***
022400170321
022500170321
022600170321
022700170321     C*--------------------------------------------------------
022800170321     C* RLTVGD - RILASCIO TRANSAZIONE (GRUPPO) CORRENTE
022900170321     C*--------------------------------------------------------
023000170321     C     RLTVGD        BEGSR
023100170321     C*
023200170321     C/EXEC SQL
023300170321     C+ UPDATE TIVGD00F SET VGDSTO = :i§7VASSTTO
023400170321     C+ WHERE VGDSTO = :i§7VASSTO AND VGDPRG = :i§7VASPRG AND
023500170321     C+       VGDTIP = :i§7VASTIP
023600170321     C/END-EXEC
023700170321     C*
023800170321     C* Fine transazione OK
023900170321     C                   if        sqlcod >= *zeros
024000170321     C   55              commit(e)
024100170321     C                   movel     '1'           o§7VASOK
024200170321     C                   else
024300170321     C* Fine transazione KO
024400170321     C   55              rolbk(e)
024500170321     C                   dump(A)
024600170321     C                   movel     '0'           o§7VASOK
024700170321     C                   seton                                        lr
024800170321     C                   endif
024900170321     C*
025000170321     C                   ENDSR
025100170321     C***
025200161206
025300161206
025400161206
025500161206     C*-----------------------------------------------------------------------------
025600161206     C* Inserimento recod "trigger" VGD
025700161206     C*-----------------------------------------------------------------------------
025800161207     C     TRGVGD        BEGSR
025900161207     C*
026000161207     C                   open(e)   tivgd00f
026100161206     C*
026200161206     C                   clear                   tivgd000
026300170126     C                   eval      vgdDTA = '*VOID'
026400170126     C                   eval      vgdTIP = i§7VASTIP
026500170126     C                   eval      vgdKSU = i§7VASKSU
026600170126     C                   eval      vgdTSC = i§7VASTSC
026700170126     C                   eval      vgdDAT = i§7VASDAT
026800170126     C                   eval      vgdPGM = i§7VASPGM
026900170126     C                   eval      vgdPRG = i§7VASPRG
027000170126     C                   eval      vgdSTO = i§7VASSTO
027100161206     C                   write     tivgd000
027200161207     C   55              commit(e)
027300170126     C                   movel     '1'           o§7VASOK
027400161207     C*
027500161207     C                   close(e)  tivgd00f
027600161206     C*
027700161206     C                   ENDSR
027800161206     C*------------------------------------------------------------------------*
027900161207
028000161207
028100161207
028200161207     C*-----------------------------------------------------------------------------
028300161207     C* Inserimento "semaforo" VGD
028400161207     C*-----------------------------------------------------------------------------
028500170127     C     SEMVGDINS     BEGSR
028600161207     C*
028700161207     C                   clear                   tivgs010
028800161207     C*
028900161207     C                   open(e)   tivgs10f
029000161207     C*
029100161207     C* Valorizzo il record "semaforo" corrente
029200170126     C                   eval      vgsKSU  = i§7VASKSU
029300170126     C                   eval      vgsTIP  = i§7VASTIP
029400161207     C                   eval      vgsDAT  = datcor
029500161207     C                   eval      vgsORA  = oracor
029600161207     C                   eval      vgsJNAM = §JOB_NAME
029700161207     C                   eval      vgsUNAM = §USER
029800161207     C                   eval      vgsJNUM = §JOB_NUM
029900161207     C                   eval      vgsPGM  = §procname
030000161207     C*
030100161207     C* Reperisco stack di chiamata corrente
030200161207     C                   call      'XRTVSTKR'
030300161207     C                   parm                    PStack         1000
030400161207     C                   parm                    PStartPoint       3 0
030500161207     C                   parm                    PEntries          3 0
030600161207     C                   movel     Pstack        DSstack
030700161207     C*
030800161207     C* Reperisco il *pgm chiamante
030900161207     C                   eval      PStartPoint = 1
031000161207     C                   dow       PStartPoint <= PEntries
031100161207     C                   if        £Stack(PStartPoint) <> §procname
031200161207     C                   eval      vgsPGM  = £Stack(PStartPoint)
031300161207     C                   leave
031400161207     C                   endif
031500161207     C                   eval      PStartPoint = PStartPoint + 1
031600161207     C                   enddo
031700161207     C*
031800161207     C* Inserisco il "semaforo"
031900161207     C                   write     tivgs010
032000161207     C   55              commit(e)
032100170126     C                   movel     '1'           o§7VASOK
032200161207     C*
032300161207     C                   close(e)  tivgs10f
032400161207     C*
032500161207     C                   ENDSR
032600161207     C*------------------------------------------------------------------------*
032700170127
032800170127
032900170127
033000170127     C*-----------------------------------------------------------------------------
033100170127     C* Eliminazione "semaforo" VGD
033200170127     C*-----------------------------------------------------------------------------
033300170127     C     SEMVGDDEL     BEGSR
033400170127     C*
033500170127     C/EXEC SQL
033600170127     C+ DELETE FROM TIVGS10F
033700170127     C+ WHERE VGSJNAM = :§JOB_NAME AND VGSUNAM = :§USER     AND
033800170127     C+       VGSJNUM = :§JOB_NUM  AND VGSKSU  = :i§7VASKSU AND
033900170127     C+       VGSTIP = :i§7VASTIP
034000170127     C/END-EXEC
034100170127     C*
034200170127     C* Fine transazione OK
034300170127     C                   if        sqlcod >= *zeros
034400170127     C   55              commit(e)
034500170127     C                   movel     '1'           o§7VASOK
034600170127     C                   else
034700170127     C* Fine transazione KO
034800170127     C   55              rolbk(e)
034900170127     C                   dump(A)
035000170127     C                   movel     '0'           o§7VASOK
035100170127     C                   seton                                        lr
035200170127     C                   endif
035300170127     C*
035400170127     C                   ENDSR
035500170127     C*------------------------------------------------------------------------*
035600170201
035700170201
035800170201
035900170201     C*-----------------------------------------------------------------------------
036000170201     C* Avvio controllo sincronia
036100170201     C*-----------------------------------------------------------------------------
036200170201     C     STRCMT        BEGSR
036300170201     C*
036400170201     C* Reperisco stack di chiamata corrente
036500170201     C                   call(e)   'TIS7VASC1'
036600170201     C                   parm      'SCM'         pOPZ              3
036700170201     C                   parm      '*ACTGRP'     pPARAM           10
036800170201     C*
036900170201     C                   if        not %error
037000170201     C                   movel     '1'           o§7VASOK
037100170201     C                   else
037200170201     C                   movel     '0'           o§7VASOK
037300170201     C                   endif
037400170201     C*
037500170201     C                   ENDSR
037600170201     C*------------------------------------------------------------------------*
037700170201
037800170201
037900170201
038000170201     C*-----------------------------------------------------------------------------
038100170201     C* Fine controllo sincronia
038200170201     C*-----------------------------------------------------------------------------
038300170201     C     ENDCMT        BEGSR
038400170201     C*
038500170201     C* Reperisco stack di chiamata corrente
038600170201     C                   call(e)   'TIS7VASC1'
038700170201     C                   parm      'ECM'         pOPZ              3
038800170201     C                   parm      *blanks       pPARAM           10
038900170201     C*
039000170201     C                   if        not %error
039100170201     C                   movel     '1'           o§7VASOK
039200170201     C                   else
039300170201     C                   movel     '0'           o§7VASOK
039400170201     C                   endif
039500170201     C*
039600170201     C                   ENDSR
039700170201     C*------------------------------------------------------------------------*
039800100512
039900100512
040000100512
040100090424     C*----------------------------------------------------*
040200090424     C     *INZSR        BEGSR
040300090424     C*
040400161202     C     *ENTRY        PLIST
040500170126     C                   PARM                    tis7vasds
040600100512     C*
040700100512     C                   ENDSR
040800161202
