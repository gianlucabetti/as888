000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200170418
000300170418
000400170418      //********************************************************************************************
000500170418      //
000600170418      // Definizione file
000700170418      //
000800170418      //********************************************************************************************
000900170418     FEDIVAB2L  IF   E           K DISK    usropn
001000170418     F                                     extfile(LibFileEDIVAB)
001100170418     F                                     extdesc('FILTRAGRPF/EDIVAB2L')
001200170412
001300170412
001400170412      //********************************************************************************************
001500170412      //
001600170418      // Definizione interfacce/prototipi procedure.
001700170412      //
001800170412      //********************************************************************************************
001900170418     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002000170418     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002100170418     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
002200170413     D/COPY GAITRASRC/SRCPROTOPI,UBSGU1R
002300170413
002400010524
002500170412     D* Variabili di work
002600170413     D  UBSGU1DSA    e ds                  inz qualified
002700170413     D  UBBRTETIDS   e ds                  inz qualified
002800170413     D  EDIVABDS     e ds                  inz extname(EDIVABWR)
002900170418     D  DVPC         e ds                  inz
003000170418     D LibFileEDIVAB   s             21A   inz
003100170418     D currSysNeta     s              8A   inz('*NULL')
003200170418     D pInSPED         ds            16
003300170418     D  wAAS                          4  0
003400170418     D  wLNP                          3  0
003500170418     D  wNRS                          2  0
003600170418     D  wNSP                          7  0
003700010525
003800170418
003900010525     C*-----------------------------------------------------------------------------
004000170418     C*
004100170418     C* Reperisco il sistema AS/400 corrente
004200170418     C                   callp     UBRTVNETA_Rtv(currSysNeta)
004300170418     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
004400170418     C                   eval      LibFileEDIVAB = 'FILTRAGRU/EDIVAB2L'
004500170418     C                   else
004600170418     C                   eval      LibFileEDIVAB = 'FILTRAGRPF/EDIVAB2L'
004700170418     C                   endif
004800170418     C*
004900170418     C* Apertura file "overraidati"
005000170418     C                   if        not %open(EDIVAB2L)
005100170418     C                   open      EDIVAB2L
005200170418     C                   endif
005300170412     C*
005400170412     C* Avvio il monitoring del intero flusso
005500170412     C                   monitor
005600170418     C*
005700170418     C* Reperisco dati tabellati cliente
005800170418     C                   exsr      rtvTAB
005900170413     C*
006000170418     C* Valorizzazione dati per generazione/reperimento etichette segnacollo
006100170418     C                   exsr      exeETI
006200170412     C*
006300170412     C* Gestisco eventuale errore
006400170412     C                   on-error
006500170412     C                   dump(A)
006600170412     C*
006700170412     C* Arresto il monitoring
006800170412     C                   endmon
006900170418     C*
007000170418     C* Chiusura file "overraidati"
007100170418     C                   if        %open(EDIVAB2L)
007200170418     C                   close     EDIVAB2L
007300170418     C                   endif
007400100624     C*
007500170418     C                   seton                                        lr
007600010525     C*
007700010525     C*-----------------------------------------------------------------------------
007800170418
007900170418
008000170418
008100170418     C*
008200170418     C     rtvTAB        begsr
008300170418     C*
008400170418     C* Richiami per reperimento vari parametri di procedura
008500170418     C                   eval      IS713TBECOD    = 'VPC'
008600170418     C                   eval      IS713TBEKE1    = pInCCM
008700170418     C                   eval      IS713TBEKE2    = *blanks
008800170418     C                   eval      IS713TBELIN    = *blanks
008900170418     C                   eval      IS713TBESIF    = *blanks
009000170418     C*
009100170418     C                   call      'TIS713R'
009200170418     C                   parm                    IS713TBECOD
009300170418     C                   parm                    IS713TBEKE1
009400170418     C                   parm                    IS713TBEKE2
009500170418     C                   parm                    IS713TBELIN
009600170418     C                   parm                    IS713TBESIF
009700170418     C                   parm                    OS713TBEUNI
009800170418     C                   parm                    OS713ESITO
009900170418     C*
010000170418     C                   if        OS713ESITO = '1'
010100170418     C                   eval      DVPC = OS713TBEUNI
010200170418     C                   else
010300170418     C     '*ERROR'      dsply
010400170418     C                   seton                                        rt
010500170418     C                   endif
010600170418     C*
010700170418     C                   endsr
010800170413
010900170413
011000170413
011100170413     C*
011200170418     C     exeETI        begsr
011300170418     C*
011400170418     C* Valorizzo parametri per stampa segnacollo
011500170418     C                   clear                   UBBRTETIDS
011600170516     C                   select
011700170516     C                   when      §VPCSGF = 'P' or §VPCSGF = 'T'
011800170516     C                   eval      UBBRTETIDS.PIN_TPSTP  = 'A'
011900170516     C                   other
012000170516     C                   eval      UBBRTETIDS.PIN_TPSTP  = 'Z'
012100170516     C                   endsl
012200170420     C                   eval      UBBRTETIDS.PIN_TLA    = *blanks
012300170420     C                   eval      UBBRTETIDS.PIN_OFSX   = *zeros
012400170420     C                   eval      UBBRTETIDS.PIN_OFSY   = *zeros
012500170420     C                   eval      UBBRTETIDS.PIN_BORDO  = 'N'
012600170420     C                   eval      UBBRTETIDS.PIN_LOGO   = 'S'
012700170420     C                   eval      UBBRTETIDS.PIN_ROW    = 'S'
012800170420     C                   eval      UBBRTETIDS.PIN_TIE    = *blanks
012900170420     C                   eval      UBBRTETIDS.PIN_RST    = *blanks
013000170516     C                   select
013100170516     C                   when      §VPCSGR = 'P'
013200170516     C                   eval      UBBRTETIDS.PIN_1SPLXC = 'S'
013300170516     C                   other
013400170516     C                   eval      UBBRTETIDS.PIN_1SPLXC = *blanks
013500170516     C                   endsl
013600170418     C*
013700170418     C* Valorizzo dati bolla
013800170418     C                   clear                   EDIVABDS
013900170418     C                   eval      vabAAS = wAAS
014000170418     C                   eval      vabLNP = wLNP
014100170418     C                   eval      vabNRS = wNRS
014200170418     C                   eval      vabNSP = wNSP
014300170418     C     KEYvab2_C     chain     EDIVAB2L
014400170418     C                   if        %found(EDIVAB2L)
014500170418     C*
014600170418     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
014700170418     C                   clear                   UBSGU1DSA
014800170418     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
014900170420     C                   eval      UBSGU1DSA.ISGU1AKSU  = pInCCM
015000170420     C                   eval      UBSGU1DSA.ISGU1AUNI  = OS713TBEUNI
015100170418     C                   eval      UBSGU1DSA.ISGU1ADSTO = pInDSTO
015200170418     C                   eval      UBSGU1DSA.ISGU1ATIPO = pInTIPO
015300170418     C                   eval      UBSGU1DSA.ISGU1ATIPF = pInTIPF
015400170418     C                   eval      UBSGU1DSA.ISGU1ATIPT = pInTIPT
015500170418     C                   eval      UBSGU1DSA.ISGU1AMSKF = *blanks
015600170418     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
015700170418     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
015800170418     C*
015900170418     C* Valorizzo parametri per driver utilità segnacolli 1
016000170418     C                   clear                   UBSGU1IDS
016100170418     C                   eval      UBSGU1IDS.ISGU1ILIN  = *blanks
016200170418     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
016300170418     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
016400170418     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
016500170418     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
016600170418     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
016700170418     C*
016800170418     C* Chiamata al driver stampa e reperimento etichetta segnacolli
016900170418     C                   call      'UBSGU1R'
017000170418     C                   parm                    UBSGU1IDS
017100170418     C                   parm                    UBSGU1OUT
017200170418     C*
017300170418     C                   endif
017400170413     C*
017500170413     C                   endsr
017600170413
017700050920
017800050920
017900000714      /TITLE Routine di *init PGM
018000000714     C*
018100000714     C     *inzsr        begsr
018200060103     C*
018300060103     C     *entry        plist
018400170418     C                   parm                    pInCCM            7
018500170418     C                   parm                    pInDSTO           5
018600170418     C                   parm                    pInTIPO           3
018700170418     C                   parm                    pInTIPF           2
018800170418     C                   parm                    pInTIPT           2
018900170418     C                   parm                    pInSPED
019000170418     C*
019100170418     C* Definizione chiavi
019200170418     C*
019300170418     C* Chiave su EDIVAB2L - Completa
019400170418     C     KEYvab2_C     KLIST
019500170418     C                   KFLD                    vabAAS
019600170418     C                   KFLD                    vabLNP
019700170418     C                   KFLD                    vabNRS
019800170418     C                   KFLD                    vabNSP
019900000714     C*
020000000714     C                   endsr
020100000714
