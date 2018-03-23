000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200161202     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300890921     H*----------------------------------------------------*
000400161202     Ftivgd00f  O    E             DISK    commit
000500161202     Ftis7prgf  uF   E             DISK    rename(tis7prgf:tis7prg0)
000600161202     F                                     prefix(f_)
000700161202     F                                     usropn
000800161202
000900100512     D*--------------------
001000161202     D* PARAMETRI I/O
001100100512     D*--------------------
001200161202     D iOPZ            S              1A
001300161202     D iDSVGD        E DS                  extname(tivgd00f) prefix(I_)
001400161202     D oPRG            S             10A
001500161202     D oOK             S              1A
001600161202
001700161206     D psds           sds
001800161206     D  procname         *PROC
001900100705
002000010402
002100161202     C*
002200161202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002300161202     C
002400161202     C/EXEC SQL
002500161202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD, COMMIT = *CHG
002600161202     C/END-EXEC
002700161202     C*
002800161202     C* Inizializzo parametri di output
002900161202     C                   movel     '0'           oOK
003000161202     C*
003100161202     C* Monitoro l'intero flusso del *pgm
003200161202     C                   monitor
003300161206     C*
003400161206     C* Verifica parametri di input
003500161206     C                   exsr      chkpar
003600161202     C*
003700161202     C* In base al opzione richiesta eseguo specifiche operazioni
003800161202     C                   select
003900161206     C*
004000161206     C* - Calcolo progressivo univoco intera transazione VGD - inizio
004100161202     C                   when      iOPZ = '1'
004200161202     C                   exsr      CALPROG
004300161205     C                   movel     '1'           oOK
004400161206     C*
004500161202     C* - Popolamento dati TIVGD - popolamento
004600161202     C                   when      iOPZ = '2'
004700161202     C                   exsr      WRIVGD
004800161205     C                   movel     '1'           oOK
004900161206     C*
005000161206     C* - "Liberazione" transazione VGD - fine
005100161202     C                   when      iOPZ = '3'
005200161202     C                   exsr      STRVGD
005300161205     C                   movel     '1'           oOK
005400161205     C                   seton                                        lr
005500161206     C*
005600161206     C* - Inserimento recod "trigger" VGD
005700161206     C                   when      iOPZ = '4'
005800161206     C                   exsr      VOIDVGD
005900161206     C                   movel     '1'           oOK
006000161206     C                   seton                                        lr
006100161202     C                   endsl
006200161202     C*
006300161202     C* Gestisco eventuale errore
006400161202     C                   on-error
006500161202     C                   rolbk(e)
006600161205     C                   dump(A)
006700161202     C                   movel     '0'           oOK
006800161205     C                   seton                                        lr
006900161202     C*
007000161202     C* Interrompo monitoraggio errori
007100161202     C                   endmon
007200161202     C*
007300161202     C                   return
007400161202     C*-----------------------------------------------------------------------------
007500161206
007600161206
007700161206
007800161206     C*-----------------------------------------------------------------------------
007900161206     C* Verifica parametri di input
008000161206     C*-----------------------------------------------------------------------------
008100161206     C     CHKPAR        BEGSR
008200161206     C*
008300161206     C* Reperisco la data corrente
008400161206     C                   z-add     *zeros        datcor            8 0
008500161206     C                   eval      datcor = %dec(%date() : *iso)
008600161206     C*
008700161206     C* Se data non specificata in input => assumo data corrente
008800161206     C                   if        I_vgdDAT = *zeros
008900161206     C                   eval      I_vgdDAT = datcor
009000161206     C                   endif
009100161206     C*
009200161206     C* Se tipo scarico non specificato in input => assumo 'WW'
009300161206     C                   if        I_vgdTSC = *blanks
009400161206     C                   eval      I_vgdTSC = 'WW'
009500161206     C                   endif
009600161206     C*
009700161206     C* Se procedura inserimento dati non specificata in input => assumo *pgm corrente
009800161206     C                   if        I_vgdPGM = *blanks
009900161206     C                   eval      I_vgdPGM = procname
010000161206     C                   endif
010100161206     C*
010200161206     C                   ENDSR
010300161206     C*------------------------------------------------------------------------*
010400161202
010500161202
010600161202
010700161202     C*-----------------------------------------------------------------------------
010800161202     C* Valorizzazione Progressivo Applicazione
010900161202     C*-----------------------------------------------------------------------------
011000161202     C     CALPROG       BEGSR
011100161205     C*
011200161205     C* Inizio transazione
011300161205     C                   commit(e)
011400161202     C*
011500161205     C* Inizializzazione proogressivo di output
011600161202     C                   clear                   oPRG
011700161202     C*
011800161205     C* Calcolo progressivo
011900161202     C                   movel     'RR'          dwlisv            2
012000161202     C                   movel     *all'0'       dwlprg           10
012100161202     C                   z-add     *zeros        wrkprg            8 0
012200161202     C*
012300161202     C                   open      tis7prgf
012400161202     C*
012500161205     C                   read      tis7prgf
012600161202     C                   eval      dwlprg = f_tis7prgf
012700161202     C*
012800161202     C                   move(p)   dwlprg        wrkprg
012900161202     C                   add       1             wrkprg
013000161202     C                   move(p)   wrkprg        dwlprg
013100161202     C                   movel     dwlisv        dwlprg
013200161202     C*
013300161202     C                   eval      f_tis7prgf = dwlprg
013400161205     C                   update    tis7prg0
013500161202     C                   movel(p)  dwlprg        oPRG
013600161202     C*
013700161202     C                   close     tis7prgf
013800161202     C*
013900161202     C                   ENDSR
014000161202     C*-----------------------------------------------------------------------------
014100161202
014200161202
014300161202
014400161202     C*-----------------------------------------------------------------------------
014500161202     C* Scrittura buffer TIVGD00F (file VAS generico download)
014600161202     C*-----------------------------------------------------------------------------
014700161205     C     WRIVGD        BEGSR
014800161202     C*
014900161202     C                   clear                   tivgd000
015000161202     C                   eval      vgdDTA = I_vgdDTA
015100161202     C                   eval      vgdTIP = I_vgdTIP
015200161202     C                   eval      vgdKSU = I_vgdKSU
015300161202     C                   eval      vgdTSC = I_vgdTSC
015400161202     C                   eval      vgdDAT = I_vgdDAT
015500161202     C                   eval      vgdPRG = I_vgdPRG
015600161202     C                   eval      vgdPGM = I_vgdPGM
015700161202     C                   eval      vgdSTO = '?'
015800161205     C                   write     tivgd000
015900161202     C*
016000161202     C                   ENDSR
016100161202     C*------------------------------------------------------------------------*
016200161202
016300161202
016400161202
016500161202     C*--------------------------------------------------------
016600161202     C* STRVGD - "LIBERO" LA TRANSAZIONE (GRUPPO) CORRENTE    *
016700161202     C*--------------------------------------------------------
016800161202     C     STRVGD        BEGSR
016900161205     C*
017000161202     C*
017100161202     C/EXEC SQL
017200161206     C***+ UPDATE TIVGD00F SET VGDSTO = 'G'
017300161206     C+ UPDATE TIVGD00F SET VGDSTO = ' '
017400161202     C+ WHERE VGDSTO = '?' AND
017500161202     C+ VGDTIP = :I_VGDTIP AND VGDKSU = :I_VGDKSU AND
017600161202     C+ VGDTSC = :I_VGDTSC AND VGDPRG = :I_VGDPRG
017700161202     C/END-EXEC
017800161202     C*
017900161205     C* Fine transazione OK
018000161205     C                   if        sqlcod = *zeros
018100161205     C                   commit(e)
018200161205     C                   else
018300161205     C* Fine transazione KO
018400161205     C                   rolbk(e)
018500161205     C                   dump(A)
018600161205     C                   movel     '0'           oOK
018700161205     C                   seton                                        lr
018800161202     C                   endif
018900161202     C*
019000161202     C                   ENDSR
019100161202     C***
019200161206
019300161206
019400161206
019500161206     C*-----------------------------------------------------------------------------
019600161206     C* Inserimento recod "trigger" VGD
019700161206     C*-----------------------------------------------------------------------------
019800161206     C     VOIDVGD       BEGSR
019900161206     C*
020000161206     C                   clear                   tivgd000
020100161206     C                   eval      vgdDTA = '*VOID'
020200161206     C                   eval      vgdTIP = I_vgdTIP
020300161206     C                   eval      vgdKSU = I_vgdKSU
020400161206     C                   eval      vgdTSC = I_vgdTSC
020500161206     C                   eval      vgdDAT = I_vgdDAT
020600161206     C                   eval      vgdPRG = I_vgdPRG
020700161206     C                   eval      vgdPGM = I_vgdPGM
020800161206     C                   eval      vgdSTO = *blanks
020900161206     C                   write     tivgd000
021000161206     C                   commit(e)
021100161206     C*
021200161206     C                   ENDSR
021300161206     C*------------------------------------------------------------------------*
021400100512
021500100512
021600100512
021700090424     C*----------------------------------------------------*
021800090424     C     *INZSR        BEGSR
021900090424     C*
022000161202     C     *ENTRY        PLIST
022100161202     C                   PARM                    iOPZ
022200161202     C                   PARM                    iDSVGD
022300161202     C                   PARM                    oPRG
022400161202     C                   PARM                    oOK
022500100512     C*
022600100512     C                   ENDSR
022700161202
