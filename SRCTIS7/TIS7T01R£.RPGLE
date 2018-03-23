000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200170228     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300991027
000400120105     FTIVGDTMP  IF   E             DISK    rename(tivgd000:tivgd) prefix(I_)
000500170228     FTIVGD00F  UF A E             DISK    COMMIT
000600111230     FTIVIR02L  IF   E           K DISK
000700030924
000800170228     D psds           sds
000900170228     D  procname         *PROC
001000111230     D dvir01        E DS
001100170228     D TIS7VASDS     E DS
001200071121
001300071121
001400071121     D*------------
001500071121     D* VARIABILI D WRK
001600071121     D*------------
001700111230     D prmKSC          S                   like(virKSC)
001800111230     D prmTIP          S                   like(virTIP)
001900111230     D prmDTI          S              8S 0
002000030924     D prmesito        S              1
002100111230     D
002200111230     D wCurrTIP        S                   like(virTIP) inz
002300120119     D wCurrKSC        S                   like(virKSC) inz
002400030924     D wrkesito        S                   like(prmesito)
002500071211     D*
002600030924
002700030325
002800030924     C* MAIN/
002900111230     C*
003000111230     C* Aggancio l'anagrafica del cliente/tipo-file ricevuto in input
003100111230     C                   eval      virKSC = prmKSC
003200111230     C                   eval      virTIP = prmTIP
003300111230     C                   eval      virDTI = prmDTI
003400111230     C*
003500111230     C     KEYvir02_C    chain     tivir02l
003600111230     C                   if        %found(tivir02l)
003700111230     C                   eval      dvir01 = virPTH
003800111230     C*
003900120119     C                   if        §VIR01TIP2 <> *blanks AND
004000120119     C                             §VIR01KSC2 <> *blanks
004100120119     C                   eval      wCurrTIP = §VIR01TIP2
004200120119     C                   eval      wCurrKSC = §VIR01KSC2
004300111230     C                   exsr      DUPLICA
004400120110     C                   endif
004500120110     C*
004600120119     C                   if        §VIR01TIP3 <> *blanks AND
004700120119     C                             §VIR01KSC3 <> *blanks
004800120119     C                   eval      wCurrTIP = §VIR01TIP3
004900120119     C                   eval      wCurrKSC = §VIR01KSC3
005000111230     C                   exsr      DUPLICA
005100120110     C                   endif
005200120110     C*
005300120119     C                   if        §VIR01TIP4 <> *blanks AND
005400120119     C                             §VIR01KSC4 <> *blanks
005500120119     C                   eval      wCurrTIP = §VIR01TIP4
005600120119     C                   eval      wCurrKSC = §VIR01KSC4
005700111230     C                   exsr      DUPLICA
005800120110     C                   endif
005900111230     C*
006000111230     C                   endif
006100071121     C*
006200921023     C                   SETON                                        LR
006300991027
006400071121
006500071121
006600111230     C     DUPLICA       BEGSR
006700170228     C*
006800170228     C* Inizializzo variabili d wrk
006900170228     C                   movel     'N'           wProcedi          1
007000170228     C*
007100170228     C* Inizializzo la transazione
007200170228     C                   CLEAR                   TIS7VASDS
007300170228     C                   EVAL      i§7VASOPZ = 'PRG'
007400170228     C                   CALL(e)   'TIS7VASR1'
007500170228     C                   PARM                    TIS7VASDS
007600170228     C*
007700170228     C* Verifico esito chiamata
007800170228     C                   if        not %error AND
007900170228     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
008000170228     C                   movel     'S'           wProcedi
008100170228     C                   endif
008200111230     C*
008300111230     C* Se elaborazione consentita => proseguo
008400170228     C                   if        wProcedi = 'S'
008500170228     C                   movel     '?'           wVGDSTO           1
008600170228     C                   else
008700120105     C                   movel     '§'           wVGDSTO
008800120105     C                   endif
008900111230     C*
009000111230     C* Duplico i dati
009100111230     C                   exsr      WRIVGD
009200170228     C*
009300170228     C* Finalizzo la transazione
009400170228     C                   EVAL      i§7VASOPZ  = 'RLS'
009500170228     C                   EVAL      i§7VASTIP  = wCurrTIP
009600170228     C                   EVAL      i§7VASKSU  = wCurrKSC
009700170228     C                   EVAL      i§7VASTSC  = 'WW'
009800170228     C                   EVAL      i§7VASSTO  = '?'
009900170228     C                   EVAL      i§7VASSTTO = 'G'
010000170228     C                   EVAL      i§7VASPRG  = o§7VASPRG
010100170228     C                   CALL(e)   'TIS7VASR1'
010200170228     C                   PARM                    TIS7VASDS
010300170228     C*
010400170228     C* Verifico esito chiamata
010500170228     C                   if        %error OR o§7VASOK = *off
010600170228     C                   exsr      EXEERR
010700170228     C                   endif
010800071121     C*
010900071121     C                   ENDSR
011000071121
011100071121
011200030924
011300111230     C     WRIVGD        BEGSR
011400991027     C*
011500111230     C                   read      TIVGDTMP
011600111230     C                   dow       not %eof(TIVGDTMP)
011700071121     C*
011800111230     C* Duplico i dati dal TIVGDTMP al TIVGD00F sul nuovo tipo file corrente
011900111230     C                   clear                   tivgd000
012000111230     C                   eval      vgdTIP = wCurrTIP
012100120119     C                   eval      vgdKSU = wCurrKSC
012200111230     C                   eval      vgdTSC = 'WW'
012300111230     C                   eval      vgdDAT = datcor
012400111230     C                   eval      vgdDTA = I_vgdDTA
012500170228     C                   eval      vgdPRG = o§7VASPRG
012600170228     C                   eval      vgdPGM = procname
012700170228     C                   eval      vgdSTO = wVGDSTO
012800111230     C                   write     tivgd000
012900040719     C*
013000040719     C* Proseguo con la lettura dell'input
013100111230     C                   read      TIVGDTMP
013200060119     C*
013300111230     C                   enddo
013400991027     C*
013500910830     C                   ENDSR
013600170228
013700170228
013800170228
013900170228     C*------------------------------------------------------------------------*
014000170228     C* EXEERR - Routine di esecuzione azioni su Errore
014100170228     C*------------------------------------------------------------------------*
014200170228     C     EXEERR        BEGSR
014300170228     C*
014400170228     C                   dump(A)
014500170228     C                   rolbk
014600170228     C                   seton                                        lr
014700170228     C                   return
014800170228     C*
014900170228     C                   ENDSR
015000170228     C*------------------------------------------------------------------------*
015100071121
015200071121
015300991027
015400991027      /TITLE Operazioni iniziali.
015500991027     C     *inzsr        BEGSR
015600991027     C*
015700991027     C     *ENTRY        PLIST
015800111230     C                   parm                    prmKSC
015900111230     C                   parm                    prmTIP
016000111230     C                   parm                    prmDTI
016100991027     C     wrkesito      parm      wrkesito      prmesito
016200100910     C*
016300100910     C* CALCOLA LA DATA CORRENTE
016400100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
016500100910     C                   eval      datcor = %dec(%date() : *ISO)
016600071210     C*
016700111230     C* TIVIR02L - Completa
016800111230     C     KEYvir02_C    klist
016900111230     C                   kfld                    virKSC
017000111230     C                   kfld                    virTIP
017100111230     C                   kfld                    virDTI
017200991027     C*
017300991027     C                   ENDSR
